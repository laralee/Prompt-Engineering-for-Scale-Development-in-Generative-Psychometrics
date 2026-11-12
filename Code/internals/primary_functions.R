GENIE <- function(
    df,
    emb_full,
    emb_sparse,
    trait,
    silently = cfg("silently"),
    ...
){
  # df: data.frame of items for ONE trait
  #     must contain columns: ID, attribute, statement
  # emb_full, emb_sparse: matrices with columns named by df$ID
  # trait: character, for logging only

  if (!all(c("ID", "attribute", "statement") %in% names(df))) {
    stop("GENIE: df must contain columns ID, attribute, and statement.")
  }

  # Align df and embeddings by ID
  ids_common <- intersect(df$ID, colnames(emb_full))
  ids_common <- intersect(ids_common, colnames(emb_sparse))

  if (length(ids_common) < 3L) {
    stop("GENIE: fewer than 3 items with embeddings for trait ", trait, ".")
  }

  items_not_in_community <- character(0) # internal only; not returned

  df <- df[match(ids_common, df$ID), , drop = FALSE]
  emb_full   <- emb_full[,   ids_common, drop = FALSE]
  emb_sparse <- emb_sparse[, ids_common, drop = FALSE]

  start_N <- nrow(df)

  # Truth = attributes, keyed by STATEMENT
  truth_attr <- tolower(df$attribute)
  names(truth_attr) <- df$statement

  log("[%-12s] Step 2: initial EGA (full vs sparse, truth = attributes)", trait)

  # --- Initial EGA on FULL ---
  ega_full   <- ega_run(emb_full)
  memb_full  <- ega_full$membership

  # --- Initial EGA on SPARSE ---
  ega_sparse  <- ega_run(emb_sparse)
  memb_sparse <- ega_sparse$membership

  # If membership is NULL at this stage, treat as poor-quality but continue
  if (is.null(memb_full) || is.null(memb_sparse)) {
    if (!silently) {
      message("GENIE [", trait, "]: initial EGA returned NULL membership (full=",
              is.null(memb_full), ", sparse=", is.null(memb_sparse),
              "). Setting initial NMI(s) to NA and continuing.")
    }
    nmi_full   <- NA_real_
    nmi_sparse <- NA_real_
  } else {

    # Handle any NA memberships by dropping those items from BOTH embeddings + df
    na_idx_full   <- which(is.na(memb_full))
    na_idx_sparse <- which(is.na(memb_sparse))
    na_idx_all    <- sort(unique(c(na_idx_full, na_idx_sparse)))

    if (length(na_idx_all) > 0L) {
      if (!silently) {
        message("GENIE [", trait, "]: dropping ", length(na_idx_all),
                " item(s) with NA community membership at initial EGA.")
      }

      keep_idx <- setdiff(seq_along(ids_common), na_idx_all)
      ids_keep <- ids_common[keep_idx]

      if (length(ids_keep) < 3L) {
        # Not a "failure" in your sense, but pipeline can't proceed
        # (bootEGA/EGA needs >=3 items)
        stop("GENIE: fewer than 3 items remain after dropping NA memberships (trait=", trait, ").")
      }

      df <- df[match(ids_keep, df$ID), , drop = FALSE]

      truth_attr <- tolower(df$attribute)
      names(truth_attr) <- df$statement

      emb_full   <- emb_full[,   ids_keep, drop = FALSE]
      emb_sparse <- emb_sparse[, ids_keep, drop = FALSE]

      # re-run EGA on reduced set
      ega_full    <- ega_run(emb_full)
      memb_full   <- ega_full$membership
      ega_sparse  <- ega_run(emb_sparse)
      memb_sparse <- ega_sparse$membership
    }

    # If re-run produced NULL membership, still not fatal: just NA NMIs
    if (is.null(memb_full) || is.null(memb_sparse)) {
      if (!silently) {
        message("GENIE [", trait, "]: initial EGA re-run returned NULL membership (full=",
                is.null(memb_full), ", sparse=", is.null(memb_sparse),
                "). Setting initial NMI(s) to NA and continuing.")
      }
      nmi_full   <- NA_real_
      nmi_sparse <- NA_real_
    } else {
      # Compute initial NMI for FULL
      nmi_full <- igraph::compare(
        as.vector(memb_full),
        as.factor(as.vector(truth_attr)),
        method = "nmi"
      )

      # And for SPARSE
      nmi_sparse <- igraph::compare(
        as.vector(memb_sparse),
        as.factor(as.vector(truth_attr)),
        method = "nmi"
      )
    }
  }

  initial_NMI <- nmi_full
  log("    initial NMI (full) = %.3f, (sparse) = %.3f", nmi_full, nmi_sparse)

  # ------------------------------------------------------------------
  # Step 3: UVA on SPARSE embeddings
  # ------------------------------------------------------------------
  log("[%-12s] Step 3: UVA on sparse embeddings (wTO = .20)", trait)

  uva <- uva_iter(emb_sparse)
  kept_stmts    <- uva$keep
  removed_stmts <- uva$removed
  n_uva_removed <- length(removed_stmts)
  n_uva_sweeps  <- uva$n_sweeps

  log("    UVA sweeps = %d, removed = %d, kept = %d",
      n_uva_sweeps, n_uva_removed, length(kept_stmts))

  if (length(kept_stmts) < 3L) {
    # Can't proceed with EGA/bootEGA meaningfully
    if (!silently) {
      message("GENIE [", trait, "]: UVA reduced to < 3 items; treating as poor result.")
    }
    return(list(
      successful  = TRUE,
      initial_NMI = as.numeric(initial_NMI),
      final_NMI   = NA_real_,
      start_N     = as.integer(start_N),
      final_N     = as.integer(length(kept_stmts)),
      UVA = list(
        n_removed   = as.integer(n_uva_removed),
        n_sweeps    = as.integer(n_uva_sweeps),
        IDs_removed = removed_stmts
      ),
      boot = list(
        n_removed   = 0L,
        n_sweeps    = 0L,
        IDs_removed = character(0)
      ),
      embeddings = list(
        selected = NA_character_
      )
    ))
  }

  # Subset by kept IDs
  df_red <- df[match(kept_stmts, df$ID), , drop = FALSE]
  truth_attr_red <- tolower(df_red$attribute)
  names(truth_attr_red) <- df_red$statement

  emb_full_red   <- emb_full[,   kept_stmts, drop = FALSE]
  emb_sparse_red <- emb_sparse[, kept_stmts, drop = FALSE]

  # ------------------------------------------------------------------
  # Step 4: post-UVA EGA (full vs sparse)
  # ------------------------------------------------------------------
  log("[%-12s] Step 4: post-UVA EGA (full vs sparse)", trait)

  ega_full2   <- ega_run(emb_full_red)
  ega_sparse2 <- ega_run(emb_sparse_red)

  memb_full2   <- ega_full2$membership
  memb_sparse2 <- ega_sparse2$membership

  # --- Option A: if membership NULL, do NOT fail; set NMI to NA and continue
  nmi_full2   <- NA_real_
  nmi_sparse2 <- NA_real_

  # Align FULL only if membership exists
  if (!is.null(memb_full2)) {
    align_full2 <- align_ega_membership(
      df       = df_red,
      X        = emb_full_red,
      ega_res  = ega_full2,
      trait    = trait,
      stage    = "post_UVA_full",
      items_not_in_community = items_not_in_community,
      silently = silently
    )

    df_red         <- align_full2$df
    emb_full_red   <- align_full2$X
    memb_full2     <- align_full2$membership
    truth_attr_red <- align_full2$truth
    items_not_in_community <- align_full2$dropped

    nmi_full2 <- igraph::compare(
      as.vector(memb_full2),
      as.factor(as.vector(truth_attr_red)),
      method = "nmi"
    )
  } else if (!silently) {
    message("GENIE [", trait, "]: post-UVA FULL EGA returned NULL membership; setting nmi_full2=NA.")
  }

  # Align SPARSE only if membership exists
  if (!is.null(memb_sparse2)) {
    align_sparse2 <- align_ega_membership(
      df       = df_red,
      X        = emb_sparse_red,
      ega_res  = ega_sparse2,
      trait    = trait,
      stage    = "post_UVA_sparse",
      items_not_in_community = items_not_in_community,
      silently = silently
    )

    df_red         <- align_sparse2$df
    emb_sparse_red <- align_sparse2$X
    memb_sparse2   <- align_sparse2$membership
    truth_attr_red <- align_sparse2$truth
    items_not_in_community <- align_sparse2$dropped

    nmi_sparse2 <- igraph::compare(
      as.vector(memb_sparse2),
      as.factor(as.vector(truth_attr_red)),
      method = "nmi"
    )
  } else if (!silently) {
    message("GENIE [", trait, "]: post-UVA SPARSE EGA returned NULL membership; setting nmi_sparse2=NA.")
  }

  # Ensure FULL embeddings are aligned to df_red if FULL alignment happened
  if (!is.null(memb_full2)) {
    valid_cols <- intersect(df_red$ID, colnames(emb_full_red))
    emb_full_red <- emb_full_red[, valid_cols, drop = FALSE]
    memb_full2   <- memb_full2[match(valid_cols, names(memb_full2))]

    df_red <- df_red[match(valid_cols, df_red$ID), , drop = FALSE]
    truth_attr_red <- tolower(df_red$attribute)
    names(truth_attr_red) <- df_red$statement
  }

  # Choose embedding type; handle NA cases
  chosen_type <- ifelse(
    is.na(nmi_full2) && !is.na(nmi_sparse2), "sparse",
    ifelse(
      is.na(nmi_sparse2) && !is.na(nmi_full2), "full",
      ifelse(!is.na(nmi_full2) && !is.na(nmi_sparse2) && nmi_full2 >= nmi_sparse2,
             "full", "sparse")
    )
  )

  # If both are NA, default to sparse (still proceed)
  if (is.na(nmi_full2) && is.na(nmi_sparse2)) {
    chosen_type <- "sparse"
  }

  log("    post-UVA NMI: full = %.3f, sparse = %.3f, chosen = %s",
      nmi_full2, nmi_sparse2, chosen_type)

  X <- if (chosen_type == "full") emb_full_red else emb_sparse_red

  # ------------------------------------------------------------------
  # Step 5: bootEGA per trait
  # ------------------------------------------------------------------
  log("[%-12s] Step 5: bootEGA (seed = 123, cutoff = .75)", trait)

  current_stmts <- colnames(X)
  boot_sweeps <- 0L

  repeat {
    boot_sweeps <- boot_sweeps + 1L

    be <- try(suppressMessages(
      EGAnet::bootEGA(
        data               = X[, current_stmts, drop = FALSE],
        type               = "parametric",
        seed               = 123,
        EGA.type           = "EGA.fit",
        plot.itemStability = FALSE,
        suppress           = TRUE,
        clear              = TRUE,
        model              = cfg("model"),
        algorithm          = cfg("algorithm")
      )
    ), silent = TRUE)

    if (inherits(be, "try-error")) break

    ss <- be$stability$item.stability$item.stability$empirical.dimensions
    names(ss) <- rownames(be$stability$item.stability$item.stability)

    if (length(ss) == length(current_stmts) && is.null(names(ss))) {
      names(ss) <- current_stmts
    }
    ss <- ss[names(ss) %in% current_stmts]

    low <- names(which(ss < 0.75))

    log("    bootEGA sweep %d: items = %3d, dropping = %3d",
        boot_sweeps, length(current_stmts), length(low),
        newline = FALSE, overwrite = TRUE)

    if (length(low) == 0L) break
    current_stmts <- setdiff(current_stmts, low)
    if (length(current_stmts) < 3L) break
  }

  log("", newline = TRUE, overwrite = TRUE)
  log("    bootEGA finished: sweeps = %d, final items = %d",
      boot_sweeps, length(current_stmts))

  final_stmts <- current_stmts
  final_df    <- df_red[match(final_stmts, df_red$ID), , drop = FALSE]
  final_X     <- X[, final_stmts, drop = FALSE]

  # ------------------------------------------------------------------
  # Step 6: final EGA & NMI vs attributes
  # ------------------------------------------------------------------
  log("[%-12s] Step 6: final EGA & NMI (truth = attributes)", trait)

  ega_final <- ega_run(final_X)
  final_NMI <- NA_real_

  if (is.null(ega_final$membership)) {
    if (!silently) {
      message("GENIE [", trait, "]: final EGA returned NULL membership; setting final_NMI=NA and continuing.")
    }
  } else {
    align_final <- align_ega_membership(
      df       = final_df,
      X        = final_X,
      ega_res  = ega_final,
      trait    = trait,
      stage    = "final",
      items_not_in_community = items_not_in_community,
      silently = silently
    )

    final_df  <- align_final$df
    final_X   <- align_final$X
    memb_final <- align_final$membership
    truth_final <- align_final$truth
    items_not_in_community <- align_final$dropped

    final_NMI <- igraph::compare(
      as.vector(memb_final),
      as.factor(as.vector(truth_final)),
      method = "nmi"
    )
  }

  log("    final NMI = %.3f (start_N = %d, final_N = %d)",
      final_NMI, start_N, nrow(final_df))

  # IDs removed by bootEGA: present after UVA, gone in final selection
  boot_removed_IDs <- setdiff(df_red$ID, final_df$ID)

  # ---------------------------
  # FIX 1: always return a well-formed object
  # ---------------------------
  return(list(
    successful  = TRUE,
    initial_NMI = as.numeric(initial_NMI),
    final_NMI   = as.numeric(final_NMI),
    start_N     = as.integer(start_N),
    final_N     = as.integer(nrow(final_df)),
    UVA = list(
      n_removed   = as.integer(n_uva_removed),
      n_sweeps    = as.integer(n_uva_sweeps),
      IDs_removed = removed_stmts
    ),
    boot = list(
      n_removed   = as.integer(length(boot_removed_IDs)),
      n_sweeps    = as.integer(boot_sweeps),
      IDs_removed = boot_removed_IDs
    ),
    embeddings = list(
      selected = chosen_type  # "full" or "sparse" (or "sparse" default if both NA)
    )
  ))
}






reduce_item_pool <- function(study.grid,
                             embedding_dir = "embeddings",
                             reduction_dir = "reduction_results") {


  for (EGA.model in cfg("model_set")){
    silently <- cfg("silently")

    # Make sure output dir exists
    dir.create(reduction_dir, showWarnings = FALSE, recursive = TRUE)

    # Paths for logs
    summary_path <- file.path(reduction_dir, "reduction_summary.csv")
    fail_path    <- file.path(reduction_dir, "failed_reduction_runs.csv")

    # Load or init failure log
    if (file.exists(fail_path)) {
      fail_reduction_runs <- read.csv(fail_path, stringsAsFactors = FALSE)
    } else {
      fail_reduction_runs <- data.frame(
        design      = character(),
        LLM.model   = character(),
        temperature = numeric(),
        rep         = integer(),
        trait       = character(),
        EGA.model   = character(),
        error_msg   = character(),
        stringsAsFactors = FALSE
      )
    }

    consecutive_fails <- 0L

    # Main loop over all study conditions (now includes EGA model)
    for (row in seq_len(nrow(study.grid))) {

      design      <- study.grid$design[[row]]
      LLM.model   <- study.grid$llm_model[[row]]
      temperature <- study.grid$temperature[[row]]
      rep         <- study.grid$rep[[row]]

      model_short <- sub(".*/", "", LLM.model)
      temp_folder <- as.character(temperature)

      if (!silently) {
        message("\n=== Reduction: design=", design,
                " / LLM=", LLM.model,
                " / temp=", temperature,
                " / rep=", rep,
                " / EGA.model=", EGA.model, " ===")
      }

      # For each trait, load embeddings and run GENIE
      for (trait in cfg("traits")) {

        # Path to embeddings created by embed_item_pools()
        emb_file <- file.path(
          embedding_dir,
          model_short,
          temp_folder,
          design,
          paste0("rep-", rep),
          paste0(trait, ".rds")
        )

        if (!file.exists(emb_file)) {
          msg <- paste0("Embeddings file not found for trait ", trait,
                        ": ", emb_file)
          warning(msg)

          new_fail <- data.frame(
            design      = design,
            LLM.model   = LLM.model,
            temperature = temperature,
            rep         = rep,
            trait       = trait,
            EGA.model   = EGA.model,
            error_msg   = msg,
            stringsAsFactors = FALSE
          )
          fail_reduction_runs <- rbind(fail_reduction_runs, new_fail)
          write.csv(fail_reduction_runs, fail_path, row.names = FALSE)

          consecutive_fails <- consecutive_fails + 1L
          if (consecutive_fails > 5L) {
            stop("ERROR: Six item reduction failures in a row (missing embeddings). Stopping run.")
          }
          next
        }

        # Reset failure streak since this embedding exists
        consecutive_fails <- 0L

        emb_obj <- readRDS(emb_file)
        df_trait    <- emb_obj$items
        emb_full    <- emb_obj$full
        emb_sparse  <- emb_obj$sparse

        # Result file path for this condition & trait & EGA model
        out_dir <- file.path(
          reduction_dir,
          model_short,
          temp_folder,
          design,
          paste0("rep-", rep),
          EGA.model
        )
        dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

        out_file <- file.path(
          out_dir,
          paste0("trait-", trait, ".rds")
        )

        # RESUMABLE: if this result already exists, skip
        if (file.exists(out_file)) {
          if (!silently) {
            message("  [", trait, "] Reduction result already exists; skipping.")
          }
          next
        }

        # Set EGA model in cfg; GENIE uses cfg("model")
        sim_cfg$model <- EGA.model

        if (!silently) {
          message("  [", trait, "] Running reduction (GENIE) with model=", EGA.model, "...")
        }

        run <- tryCatch(
          GENIE(
            df         = df_trait,
            emb_full   = emb_full,
            emb_sparse = emb_sparse,
            trait      = trait
          ),
          error = function(e) {
            msg <- paste0("Error in condition ",
                          design, " / ", LLM.model,
                          " / temp=", temperature,
                          " / rep=", rep,
                          " / trait=", trait,
                          " / model=", EGA.model,
                          ": ", e$message)
            message(msg)
            list(successful = FALSE, error = e, error_msg = msg)
          }
        )

        success <- isTRUE(run$successful)

        if (success) {
          consecutive_fails <- 0L

          # Save full run object
          saveRDS(run, out_file)

          # Build summary row
          important_stats <- data.frame(
            rep          = rep,
            design       = design,
            temperature  = temperature,
            LLM.model    = LLM.model,
            trait        = trait,
            EGA.model    = EGA.model,
            initial_NMI  = run$initial_NMI,
            final_NMI    = run$final_NMI,
            start_N      = run$start_N,
            final_N      = run$final_N,
            UVA_removed  = run$UVA$n_removed,
            UVA_sweeps   = run$UVA$n_sweeps,
            boot_removed = run$boot$n_removed,
            boot_sweeps  = run$boot$n_sweeps,
            embed_used   = run$embeddings$selected,
            stringsAsFactors = FALSE
          )

          # Append to summary CSV (create if it doesn't exist)
          if (file.exists(summary_path)) {
            existing <- read.csv(summary_path, stringsAsFactors = FALSE)
            existing <- rbind(existing, important_stats)
            write.csv(existing, summary_path, row.names = FALSE)
          } else {
            write.csv(important_stats, summary_path, row.names = FALSE)
          }

          if (!silently) {
            message("  [", trait, "] Reduction successful. NMI(final) = ",
                    sprintf("%.3f", run$final_NMI),
                    " (start_N=", run$start_N,
                    ", final_N=", run$final_N, ")")
          }

        } else {
          # reduction failed for this trait/model
          consecutive_fails <- consecutive_fails + 1L

          err_msg <- if (!is.null(run$error_msg)) run$error_msg else {
            if (!is.null(run$error)) conditionMessage(run$error) else "Unknown error"
          }

          new_fail <- data.frame(
            design      = design,
            LLM.model   = LLM.model,
            temperature = temperature,
            rep         = rep,
            trait       = trait,
            EGA.model   = EGA.model,
            error_msg   = err_msg,
            stringsAsFactors = FALSE
          )

          fail_reduction_runs <- rbind(fail_reduction_runs, new_fail)
          write.csv(fail_reduction_runs, fail_path, row.names = FALSE)

          if (consecutive_fails > 5L) {
            stop("ERROR: Six item reduction failures in a row. Stopping run.")
          }
        }
      } # trait loop
    }   # study.grid loop
  } # EGA model loop
  invisible(list(
    reduction_dir        = reduction_dir,
    summary_path         = summary_path,
    failed_reduction_log = fail_path
  ))
}




generate_items <- function(study.grid,
                           prompt_rolodex,
                           results_dir = "item_generation_results") {

  # Ensure results directory exists
  dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)

  # Path for persistent failure log
  fail_log_path <- file.path(results_dir, "failed_item_generation_runs.csv")

  # Load previous failure log if it exists, else create empty one
  if (file.exists(fail_log_path)) {
    fail_item_generation_runs <- read.csv(fail_log_path, stringsAsFactors = FALSE)
  } else {
    fail_item_generation_runs <- data.frame(
      design      = character(),
      LLM.model   = character(),
      temperature = numeric(),
      rep         = integer(),
      stringsAsFactors = FALSE
    )
  }

  # initialize the failure count
  consecutive_fails <- 0

  for (row in 1:nrow(study.grid)) {

    # get all of the params for this run
    design <- study.grid$design[[row]]
    LLM.model <- study.grid$llm_model[[row]]
    temperature <- study.grid$temperature[[row]]
    rep <- study.grid$rep[[row]]

    # nested directory structure
    model_short <- sub(".*/", "", LLM.model)

    # temperature as character for folder name ("0.5", "1", "1.5")
    temp_folder <- as.character(temperature)

    # Build nested directories
    model_dir <- file.path(results_dir, model_short)
    temp_dir  <- file.path(model_dir, temp_folder)

    # Ensure directories exist
    dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)

    # File name just uses design + rep
    out_file <- file.path(
      temp_dir,
      sprintf("items_design-%s_rep-%s.rds", design, rep)
    )

    # RESUME LOGIC
    if (file.exists(out_file)) {
      next
    }

    # add a system role if specified by design
    add_persona <- grepl("persona", design)
    system.role <- if (add_persona) cfg("persona.role") else NULL

    # adaptive flag
    set_adaptive <- grepl("adaptive", design)

    # select prompt set
    if (grepl("basic", design)) {
      main.prompts <- prompt_rolodex$basic
    } else if (grepl("few_shot", design)) {
      main.prompts <- prompt_rolodex$few_shot
    } else {
      main.prompts <- prompt_rolodex$expanded
    }

    #  Call item generation function
    pool <- generate_items_via_llm(
      main.prompts = main.prompts,
      system.role = system.role,
      LLM.model = LLM.model,
      temperature = temperature,
      adaptive = set_adaptive,
      silently = cfg("silently")
    )

    success <- isTRUE(pool$successful) # did it work?

    if (success) {
      # SUCCESS: save items & reset failure counter
      consecutive_fails <- 0
      items <- pool$items

      saveRDS(items, out_file) # saving the run

      message("Saved items to: ", out_file)

    } else {
      # FAILURE: log and persist failed condition
      consecutive_fails <- consecutive_fails + 1

      new_fail <- data.frame(
        design      = design,
        LLM.model   = LLM.model,
        temperature = temperature,
        rep         = rep,
        stringsAsFactors = FALSE
      )

      fail_item_generation_runs <- rbind(fail_item_generation_runs, new_fail)

      write.csv(fail_item_generation_runs, fail_log_path, row.names = FALSE)

      if (consecutive_fails > 5) {
        stop("ERROR: Six item generation failures in a row. Stopping generation.")
      }
    }
  }

  invisible(list(
    fail_item_generation_runs = fail_item_generation_runs,
    results_dir = results_dir
  ))
}





embed_item_pools <- function(study.grid,
                             items_dir = "item_generation_results",
                             embedding_dir = "embeddings") {

  silently <- cfg("silently")

  # Make sure output dir exists
  dir.create(embedding_dir, showWarnings = FALSE, recursive = TRUE)

  # Failure log path
  fail_log_path <- file.path(embedding_dir, "failed_embedding_runs.csv")

  # Load or init failure log
  if (file.exists(fail_log_path)) {
    fail_embedding_runs <- read.csv(fail_log_path, stringsAsFactors = FALSE)
  } else {
    fail_embedding_runs <- data.frame(
      design      = character(),
      LLM.model   = character(),
      temperature = numeric(),
      rep         = integer(),
      trait       = character(),
      stringsAsFactors = FALSE
    )
  }

  # We only need unique combos of design/llm_model/temperature/rep for embeddings
  base_grid <- unique(
    study.grid[, c("design", "llm_model", "temperature", "rep")]
  )

  consecutive_fails <- 0L

  for (row in seq_len(nrow(base_grid))) {

    design      <- base_grid$design[[row]]
    LLM.model   <- base_grid$llm_model[[row]]
    temperature <- base_grid$temperature[[row]]
    rep         <- base_grid$rep[[row]]

    # Short model name, e.g. "openai/gpt-4o" -> "gpt-4o"
    model_short <- sub(".*/", "", LLM.model)
    temp_folder <- as.character(temperature)

    # Where item pools are already stored from step 1
    items_file <- file.path(
      items_dir,
      model_short,
      temp_folder,
      sprintf("items_design-%s_rep-%s.rds", design, rep)
    )

    if (!file.exists(items_file)) {
      warning("Items file not found for embeddings: ", items_file)

      # Log a failure for each trait once, so you can re-run later
      for (trait in cfg("traits")) {
        new_fail <- data.frame(
          design      = design,
          LLM.model   = LLM.model,
          temperature = temperature,
          rep         = rep,
          trait       = trait,
          stringsAsFactors = FALSE
        )
        fail_embedding_runs <- rbind(fail_embedding_runs, new_fail)
      }
      write.csv(fail_embedding_runs, fail_log_path, row.names = FALSE)

      consecutive_fails <- consecutive_fails + 1L
      if (consecutive_fails > 5L) {
        stop("ERROR: Six embedding failures (missing item files) in a row. Stopping.")
      }
      next
    }

    # Reset fail counter since we found the items file
    consecutive_fails <- 0L

    # Load all generated items for this condition
    items <- readRDS(items_file)

    if (!silently) {
      message("\nEmbedding items for design=", design,
              " / LLM=", LLM.model,
              " / temp=", temperature,
              " / rep=", rep)
    }

    # Loop over traits and compute embeddings ONCE per trait
    for (trait in cfg("traits")) {

      # Subset items for this trait
      df_trait <- items[items$type == trait, , drop = FALSE]
      if (nrow(df_trait) == 0L) {
        if (!silently) {
          message("  [", trait, "] No items found for this trait; skipping.")
        }
        next
      }

      # Output location: embeddings/<model_short>/<temp>/<design>/rep-<rep>/<trait>.rds
      out_dir <- file.path(
        embedding_dir,
        model_short,
        temp_folder,
        design,
        paste0("rep-", rep)
      )
      dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

      out_file <- file.path(out_dir, paste0(trait, ".rds"))

      # RESUMABLE: if embeddings already exist, skip
      if (file.exists(out_file)) {
        if (!silently) {
          message("  [", trait, "] Embeddings already exist; skipping.")
        }
        next
      }

      if (!silently) {
        message("  [", trait, "] Computing embeddings...")
      }

      # Call your existing embedding helper
      emb_res <- tryCatch(
        embed_with_retry(
          embedding.model = cfg("embedding.model"),
          items           = df_trait,
          openai.API      = cfg("OPENAI_KEY"),
          silently        = silently
        ),
        error = function(e) {
          message("  [", trait, "] ERROR during embedding: ", e$message)
          NULL
        }
      )

      # Handle failures
      if (is.null(emb_res) || inherits(emb_res, "try-error") || !isTRUE(emb_res$success)) {
        consecutive_fails <- consecutive_fails + 1L

        new_fail <- data.frame(
          design      = design,
          LLM.model   = LLM.model,
          temperature = temperature,
          rep         = rep,
          trait       = trait,
          stringsAsFactors = FALSE
        )
        fail_embedding_runs <- rbind(fail_embedding_runs, new_fail)
        write.csv(fail_embedding_runs, fail_log_path, row.names = FALSE)

        if (consecutive_fails > 5L) {
          stop("ERROR: Six embedding failures in a row. Stopping.")
        }

        next
      }

      # Successful embedding: reset failure counter
      consecutive_fails <- 0L

      emb_full <- emb_res$embeddings
      # Ensure columns are ID-aligned
      colnames(emb_full) <- df_trait$ID

      # Sparse version
      emb_sparse <- sparsify_embeddings(emb_full)
      colnames(emb_sparse) <- df_trait$ID

      # Something compact but useful to save
      emb_obj <- list(
        design      = design,
        LLM.model   = LLM.model,
        temperature = temperature,
        rep         = rep,
        trait       = trait,
        full        = emb_full,
        sparse      = emb_sparse,
        item_ids    = df_trait$ID,
        items       = df_trait
      )

      saveRDS(emb_obj, out_file)

      if (!silently) {
        message("  [", trait, "] Embeddings saved to: ", out_file)
      }
    } # trait loop
  } # condition loop

  invisible(list(
    embedding_dir       = embedding_dir,
    fail_embedding_runs = fail_embedding_runs
  ))
}


