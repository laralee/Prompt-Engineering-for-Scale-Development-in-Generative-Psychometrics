ega_run <- function(X, model = cfg("model"), algorithm = cfg("algorithm")){

  res <- try(
    suppressMessages(
      EGAnet::EGA.fit(
        data = X,
        plot.EGA = FALSE,
        verbose = FALSE,
        model = model,
        algorithm = algorithm
      )
    ),
    silent = TRUE
  )

  # If EGA.fit failed or structure isn't as expected, return NULLs
  if (inherits(res, "try-error") ||
      is.null(res$EGA) ||
      is.null(res$EGA$wc)) {

    return(list(membership = NULL, obj = NULL))
  }

  # Walktrap memberships
  memb <- as.vector(res$EGA$wc)
  names(memb) <- names(res$EGA$wc)

  list(
    membership = memb,
    obj = res$EGA
  )
}

uva_iter <- function(X_sparse){

  current <- X_sparse
  prev_current <- current

  sweeps         <- 0L
  removed        <- character(0)
  any_reduction  <- FALSE

  repeat {
    sweeps <- sweeps + 1L

    u <- try(
      suppressMessages(
        EGAnet::UVA(
          data = current,
          cut.off = 0.2,
          reduce = TRUE,
          reduce.method = "remove",
          auto = TRUE,
          verbose = FALSE
        )
      ),
      silent = TRUE
    )

    if (inherits(u, "try-error")) {
      # If UVA blew up on the first call, report zero sweeps
      if (!any_reduction) sweeps <- 0L
      break
    }

    # Get reduced data, handling both modern and older UVA structures
    new_data <- NULL
    if (!is.null(u$reduced_data)) {
      new_data <- u$reduced_data
    } else if (!is.null(u$reduced$data)) {
      # older EGAnet: reduced is a list, e.g., u$reduced$data
      new_data <- u$reduced$data
    }

    # If UVA didn't give reduced data or didn't actually reduce the number of variables:
    if (is.null(new_data) || ncol(new_data) >= ncol(current)) {
      if (!any_reduction) {
        # No reductions in *any* sweep: treat as zero sweeps
        sweeps <- 0L
      } else {
        # We did reduce at least once; revert to last reduced set
        current <- prev_current
      }
      break
    }

    any_reduction <- TRUE

    # Track removed variables using UVA's own bookkeeping if available
    if (!is.null(u$keep_remove) && !is.null(u$keep_remove$remove)) {
      removed <- c(removed, u$keep_remove$remove)
    } else {
      removed <- c(
        removed,
        setdiff(colnames(current), colnames(new_data))
      )
    }

    prev_current <- new_data
    current      <- new_data

    # Optional: hard stop if we get too few items
    if (ncol(current) <= 2L) break
  }

  list(
    keep     = colnames(current),
    removed  = unique(removed),
    n_sweeps = sweeps
  )
}

ensure_aigenie_python <- function() {
  # Check if we've already initialized in this session
  if (isTRUE(getOption("aigenie.python_initialized", FALSE))) {
    return(invisible(TRUE))
  }

  message("Setting up Python environment ...")

  # Configure Python version
  reticulate::py_config()

  # Check and install packages
  if (!reticulate::py_module_available("openai")) {
    message("Installing openai==0.28...")
    reticulate::py_require(c("openai==0.28"))
  }

  if (!reticulate::py_module_available("groq")) {
    message("Installing groq...")
    reticulate::py_require(c("groq"))
  }

  if (!reticulate::py_module_available("requests")) {
    message("Installing requests...")
    reticulate::py_install("requests", pip = TRUE)
  }

  # Initialize Python
  reticulate::py_available(initialize = TRUE)

  # Verify installations
  tryCatch({
    openai <- reticulate::import("openai")
    groq <- reticulate::import("groq")
    requests <- reticulate::import("requests")
    message("AI-GENIE: Python environment ready!")
  }, error = function(e) {
    stop("Failed to set up Python packages. Please try restarting R and reinstalling.")
  })

  # Mark as initialized for this session
  options(aigenie.python_initialized = TRUE)

  invisible(TRUE)
}

get_membership <- function(ega_obj){
  if (!is.null(ega_obj$membership)) return(ega_obj$membership)
  if (!is.null(ega_obj$wc)) return(ega_obj$wc)
  stop("EGA object does not contain a membership/wc vector")
}

drop_unassigned_items <- function(membership, ids, items_not_in_community){
  na_idx <- which(is.na(membership))
  if (length(na_idx) == 0L) {
    return(list(
      membership = membership,
      ids        = ids,
      dropped    = items_not_in_community
    ))
  }

  dropped_ids <- ids[na_idx]
  items_not_in_community <- c(items_not_in_community, dropped_ids)

  keep_idx <- setdiff(seq_along(membership), na_idx)

  list(
    membership = membership[keep_idx],
    ids        = ids[keep_idx],
    dropped    = items_not_in_community
  )
}



check_for_over_reduction <- function(items, trait){

  attributes <- cfg("item_attributes")$trait
  counts <- table(items$attribute)
  over_reduced <- !all(counts[attributes] >= cfg("min_per_attr"))

  return(over_reduced)


}


align_ega_membership <- function(df,
                                 X,
                                 ega_res,
                                 trait,
                                 stage,
                                 items_not_in_community,
                                 silently = cfg("silently")) {

  memb <- ega_res$membership
  if (is.null(memb)) {
    stop("EGA membership is NULL for trait ", trait, " at stage ", stage, ".")
  }

  # Only consider IDs that are in both df and X
  all_ids <- intersect(df$ID, colnames(X))

  # Names of membership vector returned by EGA
  ega_ids <- names(memb)

  # If membership vector is unnamed, assume order corresponds to all_ids
  if (is.null(ega_ids) || !length(ega_ids)) {
    ega_ids <- all_ids[seq_along(memb)]
    names(memb) <- ega_ids
  }

  drop_ids <- character(0)

  # 1) Drop NA memberships
  na_idx <- which(is.na(memb))
  if (length(na_idx)) {
    drop_ids <- c(drop_ids, ega_ids[na_idx])
    memb     <- memb[-na_idx]
    ega_ids  <- ega_ids[-na_idx]
  }

  # 2) Any item that exists in df/X but NOT in EGA output → unassigned
  missing_ids <- setdiff(all_ids, ega_ids)
  drop_ids    <- c(drop_ids, missing_ids)

  # Candidate keep set: IDs in all_ids that are not dropped
  keep_ids <- setdiff(all_ids, drop_ids)

  # 3) Extra paranoia: drop keep_ids that somehow aren't in df
  keep_ids <- intersect(keep_ids, df$ID)

  if (length(keep_ids) < 3L) {
    stop("GENIE: fewer than 3 items remain for trait ", trait,
         " at stage ", stage,
         " after dropping items with no community.")
  }

  if (!silently && length(drop_ids)) {
    message("GENIE [", trait, "]: dropping ", length(drop_ids),
            " item(s) with no valid EGA community at stage ", stage, ".")
  }

  items_not_in_community <- c(items_not_in_community, drop_ids)

  # Match keep_ids to df rows, drop any NA matches just in case
  idx <- match(keep_ids, df$ID)
  valid <- !is.na(idx)

  if (any(!valid)) {
    # these IDs were not found in df for some reason
    extra_drops <- keep_ids[!valid]
    items_not_in_community <- c(items_not_in_community, extra_drops)
    keep_ids <- keep_ids[valid]
    idx      <- idx[valid]
  }

  # Final check after dropping weird cases
  if (length(keep_ids) < 3L) {
    stop("GENIE: fewer than 3 items remain for trait ", trait,
         " at stage ", stage,
         " after alignment.")
  }

  df_new <- df[idx, , drop = FALSE]
  X_new  <- X[, keep_ids, drop = FALSE]

  truth <- tolower(df_new$attribute)
  names(truth) <- df_new$statement

  memb_new <- memb[match(keep_ids, names(memb))]

  list(
    df        = df_new,
    X         = X_new,
    membership= memb_new,
    truth     = truth,
    dropped   = items_not_in_community
  )
}



