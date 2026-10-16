# Sampling support describes observed, positive-likelihood-weight fitted rows,
# not prediction-only rows retained in a backend's original data. Native design
# row names provide the alignment; neither calls nor row positions are guessed.
.domain_sampling_data <- function(model, backend, info) {
  data <- info$data
  fail <- function(detail) stop("Cannot align fitted observations for domain sampling: ",
    detail, ".", call. = FALSE)
  valid_ids <- function(x) is.character(x) && !anyNA(x) &&
    !anyDuplicated(x) && all(nzchar(x))
  if (!is.data.frame(data) || !valid_ids(rownames(data))) {
    fail("stored observation data need unique row identifiers")
  }
  if (backend == "gam") {
    frame <- stats::model.frame(model)
    if (!identical(rownames(frame), rownames(data))) {
      fail("the retained GAM frame and sampling data have different rows or ordering")
    }
    response <- stats::model.response(frame)
    weight <- stats::model.weights(frame) %||% rep(1, nrow(frame))
    rows <- seq_len(nrow(data))
    trials <- if (is.matrix(response) && ncol(response) == 2L) {
      rowSums(response)
    } else rep(1, length(rows))
  } else if (backend %in% c("sdmTMB", "tinyVAST")) {
    td <- if (backend == "sdmTMB") model$tmb_data else model$tmb_inputs$tmb_data
    designs <- if (backend == "sdmTMB") td$X_ij else list(td$X_ij)
    if (!is.list(designs) || !length(designs) ||
        any(!vapply(designs, function(x) is.matrix(x) || inherits(x, "Matrix"), logical(1)))) {
      fail("native fitted design matrices are unavailable")
    }
    # Weighted tinyVAST GAM matrices need not retain dimnames. Its retained
    # mgcv frame and named native response provide exact fitted row IDs.
    native_frame <- if (backend == "tinyVAST") model$internal$gam_setup$mf else NULL
    ids <- if (!is.null(native_frame)) rownames(native_frame) else rownames(designs[[1L]])
    if (!valid_ids(ids) || !length(ids) ||
        !all(vapply(designs, function(x) nrow(x) == length(ids) &&
          (identical(rownames(x), ids) ||
            (backend == "tinyVAST" && is.null(rownames(x)) &&
              identical(names(td$y_i), ids))), logical(1))) ||
        !all(ids %in% rownames(data))) {
      fail("native fitted design row identifiers do not match the stored data")
    }
    rows <- match(ids, rownames(data))
    formula <- if (backend == "sdmTMB") model$formula[[1L]] else model$formula
    if (!inherits(formula, "formula") || !is.name(formula[[2L]]) ||
        !as.character(formula[[2L]]) %in% names(data)) {
      fail("a retained, named natural-response column is required")
    }
    response <- data[[as.character(formula[[2L]])]][rows]
    align <- function(x, label) {
      if (!is.numeric(x) || !is.null(dim(x))) fail(paste("native", label, "are unavailable"))
      if (length(x) == length(ids)) return(as.numeric(x))
      # sdmTMB's default delta weights can be repeated once per component.
      # Accept only identical blocks: component-specific support would require
      # a separate declared coverage target rather than an implicit choice.
      if (backend == "sdmTMB" && label == "likelihood weights" &&
          length(designs) > 1L && length(x) == length(ids) * length(designs)) {
        block <- matrix(x, nrow = length(ids))
        if (any(!is.finite(block)) ||
            any(vapply(seq_len(ncol(block)), function(j)
              !identical(block[, j], block[, 1L]), logical(1)))) {
          fail("native component likelihood weights disagree")
        }
        return(as.numeric(block[, 1L]))
      }
      # tinyVAST retains full-data weight/trial vectors even when its design
      # drops missing responses; their source is the retained model$data.
      if (backend == "tinyVAST" && is.data.frame(model$data) &&
          valid_ids(rownames(model$data)) && length(x) == nrow(model$data) &&
          all(ids %in% rownames(model$data))) {
        return(as.numeric(x[match(ids, rownames(model$data))]))
      }
      fail(paste("native", label, "do not align with fitted design rows"))
    }
    weight <- align(td$weights_i, "likelihood weights")
    trials <- align(if (backend == "sdmTMB") td[["size"]] else td[["size_i"]],
      "trial counts")
  } else fail("unsupported model backend")
  if (!(is.numeric(response) || is.logical(response) || is.factor(response)) ||
      NROW(response) != length(rows) ||
      !is.numeric(weight) || length(weight) != length(rows) ||
      any(!is.finite(weight)) || any(weight < 0) ||
      any(!is.finite(trials)) || any(trials < 0)) {
    fail("responses, non-negative weights, or trial counts are malformed")
  }
  observed <- if (is.matrix(response)) {
    rowSums(is.finite(response)) == ncol(response)
  } else if (is.factor(response)) !is.na(response) else is.finite(response)
  eligible <- observed & weight > 0 & trials > 0
  out <- data[rows[eligible], , drop = FALSE]
  attr(out, "domain_sampling_support") <- list(
    policy = "observed response with positive fitted likelihood weight and positive trials",
    stored_rows = nrow(data), fitted_rows = length(rows),
    supported_rows = sum(eligible),
    excluded_missing_or_unfitted = nrow(data) - sum(observed),
    excluded_zero_weight_or_trials = sum(observed & (weight == 0 | trials == 0)))
  out
}
