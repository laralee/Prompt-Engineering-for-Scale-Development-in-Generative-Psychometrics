cleaning_function <- function(raw_text, item_type, verbose = FALSE) {

  log_msg <- function(...) {
    if (isTRUE(verbose)) {
      message("[cleaning_function][", item_type, "] ", sprintf(...))
    }
  }

  # Helper: empty result
  empty_df <- data.frame(
    type      = character(),
    attribute = character(),
    statement = character(),
    stringsAsFactors = FALSE
  )

  # Helper: attempt to salvage truncated JSON array by:
  # - taking from first '[' to last '}'
  # - appending ']' if needed
  repair_truncated_json_array <- function(json_str) {
    start <- regexpr("\\[", json_str)
    if (start == -1L) return(json_str)

    closes <- gregexpr("\\}", json_str, perl = TRUE)[[1]]
    if (length(closes) == 0L || closes[1] == -1L) return(json_str)

    last_close <- closes[length(closes)]
    repaired   <- substr(json_str, start, last_close)

    if (!grepl("^\\s*\\[", repaired)) {
      repaired <- paste0("[", repaired)
    }
    if (!grepl("\\]\\s*$", repaired)) {
      repaired <- paste0(repaired, "]")
    }
    repaired
  }

  # Helper: salvage individual {"attribute":..., "statement":...} objects
  salvage_objects <- function(json_str) {
    # minimal object pattern that includes both keys
    obj_pattern <- '(?s)\\{[^\\{\\}]*"attribute"[^\\{\\}]*"statement"[^\\{\\}]*\\}'
    objs <- regmatches(json_str, gregexpr(obj_pattern, json_str, perl = TRUE))[[1]]
    if (length(objs) == 0) {
      log_msg("object-salvage: no individual objects with 'attribute' and 'statement' found.")
      return(NULL)
    }

    rows <- list()
    for (j in seq_along(objs)) {
      obj <- objs[[j]]

      # clean obvious issues inside object as well
      obj_clean <- obj
      obj_clean <- gsub(",\\s*\\}", "}", obj_clean, perl = TRUE)
      obj_clean <- gsub("[^[:print:]]", "", obj_clean)
      obj_clean <- gsub("`", "", obj_clean, fixed = TRUE)

      this <- try(jsonlite::fromJSON(obj_clean, simplifyDataFrame = TRUE), silent = TRUE)
      if (inherits(this, "try-error") || is.null(this)) next

      df <- as.data.frame(this, stringsAsFactors = FALSE)

      if (!all(c("attribute", "statement") %in% names(df))) next

      rows[[length(rows) + 1L]] <- df[, c("attribute", "statement")]
    }

    if (length(rows) == 0) {
      log_msg("object-salvage: all individual object parses failed or lacked required keys.")
      return(NULL)
    }

    do.call(rbind.data.frame, c(rows, list(stringsAsFactors = FALSE)))
  }

  # ---- Early exit: empty text ----
  if (is.null(raw_text) || nchar(trimws(raw_text)) == 0) {
    log_msg("raw_text is NULL or empty; returning 0 items.")
    return(empty_df)
  }

  raw_text <- as.character(raw_text)

  # Quick check for obviously truncated JSON in full text
  n_open  <- length(gregexpr("\\[", raw_text, perl = TRUE)[[1]])
  n_close <- length(gregexpr("\\]", raw_text, perl = TRUE)[[1]])
  if (n_open > 0 && n_close > 0 && n_open != n_close) {
    log_msg("detected unmatched '[' and ']' in full text (open=%d, close=%d); JSON may be truncated.", n_open, n_close)
  }

  # ---- 1. Find candidate JSON array ----
  strict_pattern <- "(?s)\\[\\s*\\{.*?\\}\\s*(,\\s*\\{.*?\\}\\s*)*\\]"
  strict_match   <- regmatches(raw_text, gregexpr(strict_pattern, raw_text, perl = TRUE))[[1]]

  json_candidates <- strict_match

  if (length(json_candidates) == 0) {
    lenient_pattern <- "(?s)\\[.*\\]"
    lenient_match   <- regmatches(raw_text, gregexpr(lenient_pattern, raw_text, perl = TRUE))[[1]]
    json_candidates <- lenient_match
    if (length(json_candidates) == 0) {
      log_msg("no '[' ... ']' JSON-like block found; returning 0 items.")
      return(empty_df)
    } else {
      log_msg("no strict JSON array found; using lenient '[ ... ]' match.")
    }
  } else {
    log_msg("found %d strict JSON array candidate(s).", length(json_candidates))
  }

  result_df <- empty_df

  # ---- 2. Try candidates in order, stop at first good one ----
  for (idx in seq_along(json_candidates)) {
    json_str <- json_candidates[[idx]]
    log_msg("attempting to parse candidate %d (first 80 chars): %s",
            idx, substr(json_str, 1, 80))

    cleaned_json <- json_str

    # Remove trailing commas before closing brackets/braces
    cleaned_json <- gsub(",\\s*\\}", "}", cleaned_json, perl = TRUE)
    cleaned_json <- gsub(",\\s*\\]", "]", cleaned_json, perl = TRUE)

    # Remove non-printable characters and obvious junk
    cleaned_json <- gsub("[^[:print:]]", "", cleaned_json)
    cleaned_json <- gsub("`", "", cleaned_json, fixed = TRUE)

    # Fix simple cases of single quotes around keys/values
    cleaned_json <- gsub(
      "(\\{|,|:)\\s*'([^']*?)'\\s*(:|,|\\})",
      '\\1"\\2"\\3',
      cleaned_json,
      perl = TRUE
    )

    parsed_json <- NULL

    # First parse attempt
    try({
      parsed_json <- jsonlite::fromJSON(cleaned_json, simplifyDataFrame = TRUE)
    }, silent = TRUE)

    # Second attempt: trim to first '[' and last ']'
    if (is.null(parsed_json)) {
      try({
        trimmed <- cleaned_json
        trimmed <- sub("^[^\\[]*", "", trimmed)
        trimmed <- sub("[^\\]]*$", "", trimmed)
        parsed_json <- jsonlite::fromJSON(trimmed, simplifyDataFrame = TRUE)
      }, silent = TRUE)
    }

    # Third attempt: repair truncated array if bracket counts mismatch
    if (is.null(parsed_json)) {
      n_open_cand  <- length(gregexpr("\\[", cleaned_json, perl = TRUE)[[1]])
      n_close_cand <- length(gregexpr("\\]", cleaned_json, perl = TRUE)[[1]])
      if (n_open_cand != n_close_cand) {
        log_msg("candidate %d appears truncated (open=%d, close=%d); attempting repair.",
                idx, n_open_cand, n_close_cand)
        repaired <- repair_truncated_json_array(cleaned_json)
        if (!identical(repaired, cleaned_json)) {
          try({
            parsed_json <- jsonlite::fromJSON(repaired, simplifyDataFrame = TRUE)
          }, silent = TRUE)
          if (!is.null(parsed_json)) {
            log_msg("repair succeeded for candidate %d.", idx)
          } else {
            log_msg("repair failed for candidate %d.", idx)
          }
        } else {
          log_msg("repair did not change candidate %d; skipping repair.", idx)
        }
      } else {
        log_msg("candidate %d has balanced brackets but still failed to parse JSON.", idx)
      }
    }

    # Fourth attempt: salvage individual objects
    if (is.null(parsed_json)) {
      log_msg("attempting object-level salvage for candidate %d.", idx)
      salvaged <- salvage_objects(cleaned_json)
      if (!is.null(salvaged)) {
        parsed_json <- salvaged
        log_msg("object-level salvage succeeded for candidate %d with %d row(s).",
                idx, nrow(salvaged))
      } else {
        log_msg("object-level salvage failed for candidate %d.", idx)
      }
    }

    if (is.null(parsed_json)) {
      log_msg("JSON parse failed for candidate %d; moving to next candidate.", idx)
      next
    }

    # ---- 3. Normalize to a data frame ----
    temp_df <- NULL
    if (is.data.frame(parsed_json)) {
      temp_df <- parsed_json
    } else if (is.list(parsed_json)) {
      try({
        temp_df <- do.call(
          rbind.data.frame,
          c(lapply(parsed_json, as.data.frame, stringsAsFactors = FALSE),
            list(stringsAsFactors = FALSE))
        )
      }, silent = TRUE)
    }

    if (is.null(temp_df)) {
      log_msg("parsed candidate %d is neither data.frame nor list; skipping.", idx)
      next
    }

    # ---- 4. Schema checks ----
    if (!all(c("attribute", "statement") %in% names(temp_df))) {
      log_msg("candidate %d missing required keys 'attribute' and/or 'statement'; names: %s",
              idx, paste(names(temp_df), collapse = ", "))
      next
    }

    temp_df <- temp_df[, c("attribute", "statement")]
    temp_df$attribute <- as.character(temp_df$attribute)
    temp_df$statement <- as.character(temp_df$statement)

    # Drop NA/empty statements
    before_n <- nrow(temp_df)
    temp_df <- temp_df[
      !is.na(temp_df$statement) &
        nchar(trimws(temp_df$statement)) > 0,
      ,
      drop = FALSE
    ]
    after_n <- nrow(temp_df)
    if (after_n == 0) {
      log_msg("candidate %d: all %d rows dropped due to empty/NA statements.", idx, before_n)
      next
    } else if (after_n < before_n) {
      log_msg("candidate %d: dropped %d rows with empty/NA statements.", idx, before_n - after_n)
    }

    # ---- 5. Trait-level attribute validation ----
    valid_attrs <- tolower(cfg("item_attributes")[[item_type]])
    temp_df$attribute <- tolower(trimws(temp_df$attribute))

    before_attr_n <- nrow(temp_df)
    temp_df <- temp_df[
      temp_df$attribute %in% valid_attrs,
      ,
      drop = FALSE
    ]
    after_attr_n <- nrow(temp_df)

    if (after_attr_n == 0) {
      log_msg("candidate %d: all %d rows dropped after filtering to valid attributes for '%s': %s",
              idx, before_attr_n, item_type, paste(valid_attrs, collapse = ", "))
      next
    } else if (after_attr_n < before_attr_n) {
      log_msg("candidate %d: dropped %d rows with invalid attributes. Remaining: %d.",
              idx, before_attr_n - after_attr_n, after_attr_n)
    }

    # ---- 6. Add type, drop duplicates ----
    temp_df$type <- item_type
    temp_df <- temp_df[, c("type", "attribute", "statement")]
    temp_df$type      <- as.character(temp_df$type)
    temp_df$attribute <- as.character(temp_df$attribute)
    temp_df$statement <- as.character(temp_df$statement)

    before_dup_n <- nrow(temp_df)
    temp_df <- temp_df[!duplicated(temp_df$statement), , drop = FALSE]
    after_dup_n <- nrow(temp_df)

    if (after_dup_n == 0) {
      log_msg("candidate %d: all rows removed as duplicates.", idx)
      next
    } else if (after_dup_n < before_dup_n) {
      log_msg("candidate %d: dropped %d duplicate statement(s).",
              idx, before_dup_n - after_dup_n)
    }

    log_msg("candidate %d accepted with %d item(s).", idx, nrow(temp_df))
    result_df <- temp_df
    break
  }

  if (nrow(result_df) == 0) {
    log_msg("no candidate JSON block passed all checks; returning 0 items.")
    return(empty_df)
  }

  return(result_df)
}

generate_items_via_llm <- function(main.prompts,
                                   system.role = NULL,
                                   LLM.model,
                                   top.p = cfg("top.p"),
                                   temperature = 1,
                                   adaptive = FALSE,
                                   silently = cfg("silently"),
                                   groq.API =  cfg("GROQ_KEY"),
                                   openai.API = cfg("OPENAI_KEY"),
                                   target.N = cfg("target.N")) {

  # Ensure Python environment is ready
  ensure_aigenie_python()

  # Extract model name and provider
  provider <- sub("/.*$", "", LLM.model)
  normalized_model <- sub("^[^/]*/", "", LLM.model)

  # Initialize results dataframe
  all_items_df <- data.frame(
    type = character(),
    attribute = character(),
    statement = character(),
    stringsAsFactors = FALSE
  )

  # Set up API client and generate function based on provider
  tryCatch({
    if (provider == "groq") {
      if (is.null(groq.API)) {
        stop(paste("Groq API key required for model:", normalized_model))
      }
      groq <- reticulate::import("groq")
      groq_client <- groq$Groq(api_key = groq.API)
      generate_FUN <- groq_client$chat$completions$create
      use_groq <- TRUE
    } else if (provider == "openai") {
      if (is.null(openai.API)) {
        stop(paste("OpenAI API key required for model:", normalized_model))
      }
      openai <- reticulate::import("openai")
      openai$api_key <- openai.API
      generate_FUN <- openai$ChatCompletion$create
      use_groq <- FALSE
    } else {
      stop(paste("Unsupported provider:", provider))
    }
  }, error = function(e) {
    stop(paste("Failed to initialize API client:", conditionMessage(e)))
  })

  # Helper function to make API call
  call_generate_FUN <- function(messages_list) {
    if(grepl("5.1", normalized_model)){

      call_params <- list(
        model = normalized_model,  # Use the model name without prefix
        messages = messages_list,
        temperature = temperature,
        top_p = top.p,
        max_completion_tokens = 2000L
      )
      do.call(generate_FUN, call_params)

    } else{

    call_params <- list(
      model = normalized_model,  # Use the model name without prefix
      messages = messages_list,
      temperature = temperature,
      top_p = top.p,
      max_tokens = 2000L
    )
    do.call(generate_FUN, call_params)
    }
  }

  # Iterate through each item type
  for (item_type in names(main.prompts)) {
    if (!silently) {
      cat(paste("Generating items for", item_type, "...\n"))
    }

    # Track items for this type
    type_items_df <- data.frame(
      type = character(),
      attribute = character(),
      statement = character(),
      stringsAsFactors = FALSE
    )

    iterations_without_new <- 0
    total_iterations <- 0
    rate_limit_truncate <- FALSE
    max_previous_items <- Inf

    # pull the base prompt and JSON instruction for this type
    base_prompt  <- main.prompts[[item_type]]
    json_suffix  <- cfg("json_format_str")[[item_type]]

    while (nrow(type_items_df) < target.N[[item_type]]) {

      total_iterations <- total_iterations + 1

      # Build current_prompt
      if (adaptive && nrow(type_items_df) > 0) {
        # Use ONLY previous items from THIS type
        previous_items_to_use <- type_items_df

        if (rate_limit_truncate && nrow(previous_items_to_use) > max_previous_items) {
          previous_items_to_use <- tail(previous_items_to_use, max_previous_items)
        }

        examples_string <- construct_item.examples_string(previous_items_to_use, item_type)

        # base instructions + "don't repeat" list + JSON instruction LAST
        current_prompt <- paste0(
          base_prompt,
          "\n\nDo NOT repeat, rephrase, or reuse the content of ANY items from this list of ",
          "items you've already generated for ", item_type, ":\n",
          examples_string,
          json_suffix
        )
      } else {
        # non-adaptive: base instructions + JSON instruction
        current_prompt <- paste0(base_prompt, json_suffix)
      }

      # Prepare messages list
      if (is.null(system.role) || length(system.role) == 0) {
        messages_list <- list(
          list(role = "user", content = current_prompt)
        )
      } else {
        current_role <- system.role[[item_type]]

        messages_list <- list(
          list(role = "system", content = current_role),
          list(role = "user",   content = current_prompt)
        )
      }

      api_success <- FALSE
      api_attempts <- 0

      while (!api_success && api_attempts < 5) {
        api_attempts <- api_attempts + 1

        try_resp <- tryCatch({
          response <- call_generate_FUN(messages_list)
          api_success <- TRUE
          response

        }, error = function(e) {

          msg <- conditionMessage(e)

          # ---- Special handling: embedded NUL ----
          if (grepl("embedded NUL", msg, ignore.case = TRUE)) {
            if (!silently) {
              cat("\n[Warning] Embedded NUL detected. Retrying this request...\n")
            }
            Sys.sleep(1)
            return(NULL)   # Do NOT count this as a real failure — try again
          }

          # ---- Normal error handling ----
          if (grepl("token limit|context length|context window|token window",
                    msg, ignore.case = TRUE)) {
            if (adaptive && !rate_limit_truncate) {
              if (!silently) {
                cat("\nWarning: Rate limiting detected. Reducing number of previous items in prompt.\n")
              }
              rate_limit_truncate <- TRUE
              max_previous_items <- floor(nrow(all_items_df) * 0.5)
            }
          }

          # Count as a real failure
          e
        })

        if (!api_success) {
          Sys.sleep(2)
        }
      }


      # Check if API calls failed
      if (!api_success) {
        error_msg <- paste("API call failed after 5 attempts for", item_type,
                           "Error:", conditionMessage(try_resp))
        cat(paste0("\n", error_msg, "\n"))

        if (nrow(all_items_df) > 0) {
          warning("Returning partial results generated before error.")
          return(list(items = all_items_df, successful = FALSE))
        } else {
          stop(error_msg)
        }
      }

      # Extract raw text from response with protection against embedded NUL errors
      nul_error <- FALSE

      raw_text <- tryCatch({
        if (use_groq) {
          try_resp$choices[[1]]$message$content
        } else {
          try_resp$choices[[1]]$message$content
        }
      }, error = function(e) {
        msg <- conditionMessage(e)
        if (grepl("embedded NUL", msg, ignore.case = TRUE)) {
          if (!silently) {
            cat("\n[Warning] Embedded NUL detected when converting response to R string. Retrying this request...\n")
          }
          nul_error <<- TRUE
          return(NA_character_)  # placeholder
        } else {
          stop(e)  # re-throw non-NUL errors
        }
      })

      # If NUL error occurred, retry this iteration without counting as a stall
      if (nul_error) {
        # don't increment iterations_without_new here; just go back to while-loop
        next
      }


      # Clean the output
      cleaned_df <- cleaning_function(raw_text, item_type)

      if (nrow(cleaned_df) > 0) {
        # Keep only unique statements
        new_items <- cleaned_df[!cleaned_df$statement %in% c(type_items_df$statement, all_items_df$statement), ]

        if (nrow(new_items) > 0) {
          type_items_df <- rbind(type_items_df, new_items)
          all_items_df <- rbind(all_items_df, new_items)
          iterations_without_new <- 0

          if (!silently) {
            cat(sprintf("\rItems generated for %s: %d", item_type, nrow(type_items_df)))
            flush.console()
          }
        } else {
          iterations_without_new <- iterations_without_new + 1
        }
      } else {
        iterations_without_new <- iterations_without_new + 1
      }

      # Check for stalling
      if (iterations_without_new >= 40) {
        if (!silently) {
          warning(paste0("\nWarning: Unable to generate new unique items for ", item_type,
                         " after 20 iterations. Moving to next item type.\n",
                         "Generated ", nrow(type_items_df), " out of ",
                         target.N[[item_type]], " requested items."))
        }
        break
      }
    }

    if (!silently) {
      cat("\n")  # New line after progress updates
    }
  }

  # Final message
  if (!silently) {
    cat(paste0("All items generated. Final sample size: ", nrow(all_items_df), "\n"))
  }

  # Add an ID column now for easy labeling
  all_items_df$ID <- as.factor(1:nrow(all_items_df))

  return(list(items = all_items_df, successful = TRUE))
}





embed_items <- function(embedding.model, openai.API, items, silently) {

  # Ensure Python environment is ready
  ensure_aigenie_python()

  if(!silently){
    cat("\n")
    cat("Generating embeddings...")

  }

  # Initialize return structure
  result <- list(
    embeddings = NULL,
    success = FALSE
  )

  # Initialize OpenAI API
  tryCatch({
    openai <- reticulate::import("openai")
    openai$api_key <- openai.API

    # Extract statements to embed
    statements <- items$statement
    item_ids <- items$ID

    # Initialize list to store embeddings
    all_embeddings <- list()

    # Process each item statement
    for (i in seq_along(statements)) {

      # Create embedding for current statement
      response <- openai$Embedding$create(
        input = statements[i],
        model = embedding.model
      )

      # Extract embedding vector
      embedding_vector <- unlist(response$data[[1]]$embedding)
      all_embeddings[[i]] <- embedding_vector
    }

    # Combine all embeddings into a matrix
    # Each column is an item, each row is an embedding dimension
    embedding_matrix <- do.call(cbind, all_embeddings)

    # Set column names to item IDs
    colnames(embedding_matrix) <- item_ids

    # Update result
    result$embeddings <- embedding_matrix
    result$success <- TRUE

    if(!silently){
      cat(" Done.\n\n")

    }

  }, error = function(e) {
    # Handle API errors gracefully
    cat("Error occurred during embedding process:\n")
    cat("Error message:", conditionMessage(e), "\n")
    cat("Returning partial results with success = FALSE\n")

    result$success <- FALSE
    # result$embeddings remains NULL
  })

  return(result)
}

log <- function(fmt, ..., newline = TRUE, overwrite = FALSE){
  if (cfg("silently")) return(invisible())
  txt <- sprintf(fmt, ...)
  if (overwrite) {
    cat("\r", txt)
  } else {
    cat(txt)
    if (newline) cat("\n")
  }
  flush.console()
  invisible()
}

sparsify_embeddings <- function(embedding_matrix,
                                lower_quantile = 0.025,
                                upper_quantile = 0.975,
                                fallback_lower = 0.10,
                                fallback_upper = 0.90) {

  # Store original for fallback
  original_embedding <- embedding_matrix

  # Helper function for applying sparsification
  apply_sparsification <- function(mat, lower, upper) {
    q <- quantile(mat, probs = c(lower, upper), na.rm = TRUE)
    mat[mat > q[1] & mat < q[2]] <- 0
    return(mat)
  }

  # First attempt with primary quantiles
  embedding_sparse <- apply_sparsification(embedding_matrix, lower_quantile, upper_quantile)

  # Check if all values are zero
  if (all(embedding_sparse == 0, na.rm = TRUE)) {

    # Try with fallback quantiles
    embedding_sparse <- apply_sparsification(embedding_matrix, fallback_lower, fallback_upper)

    # If still all zeros, return original
    if (all(embedding_sparse == 0, na.rm = TRUE)) {

      if (!silently) {
        cat("Fallback sparsification also resulted in all zeros. Using original embeddings.\n")
      }

      embedding_sparse <- original_embedding
      attr(embedding_sparse, "sparsification_applied") <- FALSE

    } else {

      attr(embedding_sparse, "sparsification_applied") <- TRUE
      attr(embedding_sparse, "quantiles_used") <- c(lower = fallback_lower, upper = fallback_upper)
    }

  } else {

    attr(embedding_sparse, "sparsification_applied") <- TRUE
    attr(embedding_sparse, "quantiles_used") <- c(lower = lower_quantile, upper = upper_quantile)
  }

  return(embedding_sparse)
}




embed_with_retry <- function(embedding.model,
                             items, openai.API,
                             silently, max_retries = 5,
                             wait_base = 1) {
  attempts <- 0
  repeat {
    attempts <- attempts + 1
    res <- try(embed_items(embedding.model = embedding.model,
                           items = items, openai.API = openai.API,
                           silently = silently), silent = TRUE)

    # if it worked and res$success is TRUE:
    if (!inherits(res, "try-error") && isTRUE(res$success)) {
      return(res)
    }

    if (attempts >= max_retries) {
      message("Embedding failed after ", attempts, " attempts.")
      return(res)  # or return a structured failure object
    }

    # backoff: 1s, 2s, 4s, ...
    Sys.sleep(wait_base * 2^(attempts - 1))
  }
}


construct_item.examples_string <- function(item.examples, current_type) {
  # Filter by exact match
  filtered <- item.examples[item.examples$type == current_type, , drop = FALSE]

  if (nrow(filtered) == 0) return(NULL)

  # Prepare clean subset
  df <- data.frame(
    attribute = as.character(filtered$attribute),
    statement = as.character(filtered$statement),
    stringsAsFactors = FALSE
  )

  # Convert to JSON string
  json_string <- jsonlite::toJSON(df, auto_unbox = TRUE)

  return(as.character(json_string))
}

