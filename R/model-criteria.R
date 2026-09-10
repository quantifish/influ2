#' Summarise criteria across CPUE model types
#'
#' A model-specific summary table for GLMs, mgcv GAMs, glmmTMB, brms,
#' sdmTMB, and tinyVAST. Different criteria occupy different columns; missing
#' or inappropriate criteria are never replaced by another statistic.
#'
#' @param fits One supported fitted model, or a non-empty list of models.
#'   A list may mix backends. Complete brms fits are required.
#' @param criterion Character vector, case-insensitive: `"auto"`, `"AIC"`,
#'   `"BIC"`, `"cAIC"`, `"logLik"`, `"deviance"`, `"loo"`, `"loo_R2"`,
#'   `"bayes_R2"`, or `"log_lik"`. `"auto"` requests native likelihood-based
#'   summaries for frequentist models and LOO/Bayesian R-squared for brms.
#'   Add `"cAIC"` explicitly: native spatial Hessian calculations can be costly.
#'   `"log_lik"` is the posterior mean total log likelihood, not ML `"logLik"`.
#' @param sort Preserve input order by default. `TRUE` sorts only when every
#'   row belongs to one verified comparison group, using LOOIC, AIC, or cAIC
#'   in that order. Alternatively specify one of those column names. No
#'   sorting combines different criteria, and R-squared is not a ranking rule.
#' @param labels Optional unique, non-empty model labels. Otherwise list names
#'   are used, or labels are generated from the list position.
#' @param ... Arguments passed only to requested brms criterion functions.
#'   These arguments cannot select a response or prediction subset: comparison
#'   metadata describe the complete fitted response. Refitting options are
#'   rejected; this function never launches MCMC or cross-validation refits.
#'
#' @details
#' `df` is the native log-likelihood parameter count or effective penalty
#' degrees of freedom, not simply the number of regression coefficients.
#' `df_residual` is reported separately where provided by the backend.
#' `cAIC_df` is the effective penalty used by the conditional criterion, when
#' available; `EDF_random` is sdmTMB's summed random-effect EDF.
#'
#' mgcv's ordinary `AIC()` already uses a conditional formulation, with its
#' smoothing-parameter uncertainty correction when available. Its `cAIC`
#' column therefore equals native AIC; it is not an independent criterion.
#' sdmTMB and tinyVAST use their own approximate `cAIC()` methods. glmmTMB
#' has no supported native conditional-AIC method here. No generic mixed-model
#' correction, cAIC4 conversion, or refitting workaround is substituted.
#' For an ordinary model without random effects, cAIC reduces to native AIC.
#' Profiled spatial fits are not yet validated for conditional penalty
#' counting; their cAIC is unavailable, while native marginal criteria remain.
#' Restricted-likelihood and penalised/prior fits are flagged and excluded
#' from automatic ranking. In particular, REML objective values must not
#' rank different fixed-effect structures. mgcv's conditional AIC is not its
#' REML smoothing-selection objective.
#'
#' AIC/BIC and cAIC comparison groups distinguish conditional observation
#' likelihoods from marginal likelihoods. Ordinary models with no random
#' effects can join either target, but cannot bridge incompatible targets.
#' Groups are formed in input order; compare a subset separately to examine
#' an ordinary model against another target. They require aligned fitted row
#' names, responses, weights, compatible response support, and matching values
#' of shared model-frame columns. Row names must genuinely identify the same
#' observations: these checks cannot establish provenance for independently
#' renumbered datasets. Unsupported or unverifiable cases get separate groups
#' and no differences. Native deviances are descriptive, backend-specific
#' quantities, not a universal scale for ranking families or model classes.
#'
#' brms LOO comparisons use paired pointwise ELPD differences and their
#' standard errors through [loo::loo_compare()]. Pareto-k diagnostics are
#' reported; unreliable LOO estimates are not ranked. Posterior R-squared
#' standard deviations retain the earlier `se_bayes_R2`/`se_loo_R2` names,
#' but are posterior uncertainty, not Monte Carlo standard errors.
#'
#' Conditional criteria target new observations sharing fitted latent
#' effects; marginal criteria target new random effects. Neither establishes
#' that an abundance index is unbiased or robust to extrapolation. LOO-PIT
#' and refitted cross-validation are separate workflows, not implemented here.
#'
#' @return A data frame with model labels, backend/family/likelihood metadata,
#'   sample size, degrees of freedom, convergence flags, requested statistics,
#'   criterion-specific comparison groups and differences, and readable `notes`.
#'   Inapplicable or unavailable values are `NA`, with reasons in `notes` and
#'   the `criteria_notes` attribute (model, criterion, status, and detail).
#'   No full fits, posterior draws, or pointwise likelihood arrays are retained.
#' @seealso [get_bayes_R2()], [plot_compare()]
#' @references
#' Wood, Pya, and Saefken (2016). Smoothing parameter and model selection
#' for general smooth models. \doi{10.1080/01621459.2016.1180986}.
#'
#' Zheng, Cadigan, and Thorson (2024). A note on numerical evaluation of
#' conditional Akaike information for nonlinear mixed-effects models.
#' \doi{10.48550/arXiv.2411.14185}.
#' @examples
#' a <- glm(mpg ~ wt, data = mtcars, family = gaussian())
#' b <- glm(mpg ~ wt + hp, data = mtcars, family = gaussian())
#' result <- table_criterion(list(Weight = a, Weight_and_power = b))
#' result[c("Model", "df", "AIC", "delta_AIC")]
#' @md
#' @export
table_criterion <- function(fits, criterion = "auto", sort = FALSE,
                            labels = NULL, ...) {
  allowed <- c("auto", "aic", "bic", "caic", "loglik", "deviance",
    "loo", "loo_r2", "bayes_r2", "log_lik")
  if (!is.character(criterion) || !length(criterion) || anyNA(criterion) ||
      any(!tolower(criterion) %in% allowed)) {
    stop("Unknown `criterion`; see `?table_criterion`.", call. = FALSE)
  }
  requested <- unique(tolower(criterion))
  if (!(is.logical(sort) && length(sort) == 1L && !is.na(sort)) &&
      !(is.character(sort) && length(sort) == 1L &&
        !is.na(sort) && tolower(sort) %in% c("aic", "caic", "looic"))) {
    stop("`sort` must be TRUE, FALSE, or one of AIC, cAIC, and looic.", call. = FALSE)
  }
  classes <- c("glm", "gam", "glmmTMB", "brmsfit", "sdmTMB", "tinyVAST")
  if (inherits(fits, classes)) fits <- list(fits)
  if (!is.list(fits) || !length(fits) ||
      !all(vapply(fits, inherits, logical(1), classes))) {
    stop("`fits` must contain supported GLM, GAM, glmmTMB, brmsfit, sdmTMB, or tinyVAST models.", call. = FALSE)
  }
  bayesian <- vapply(fits, inherits, logical(1), "brmsfit")
  .check_brms_criterion_fits(fits[bayesian])
  dots <- list(...)
  # Do not let native options change the fitted rows/response described below,
  # or start expensive sampling/refitting behind a reporting function.
  blocked <- c("resp", "newdata", "subset", "re_formula", "re.form",
    "pointwise", "moment_match", "reloo", "k_threshold", "summary", "compare")
  unsafe <- vapply(names(dots), function(nm)
    any(startsWith(blocked, nm)), logical(1))
  if (length(dots) && (is.null(names(dots)) || any(names(dots) == "") ||
      any(unsafe))) {
    stop("Use named brms options that do not change the response/rows or request refitting; subset and refitting options are not supported.", call. = FALSE)
  }
  if (length(dots) && !any(bayesian)) {
    stop("`...` supplies brms criterion options, but no brms models were supplied.", call. = FALSE)
  }
  if (is.null(labels)) {
    labels <- names(fits)
    if (is.null(labels)) labels <- rep("", length(fits))
    blank <- is.na(labels) | !nzchar(trimws(labels))
    labels[blank] <- paste0("Model ", which(blank))
  }
  if (!is.character(labels) || length(labels) != length(fits) || anyNA(labels) ||
      any(!nzchar(trimws(labels))) || anyDuplicated(trimws(labels))) {
    stop("Supply one unique, non-empty label per model.", call. = FALSE)
  }
  rows <- lapply(seq_along(fits), function(i) {
    context <- new.env(parent = emptyenv())
    context$notes <- list()
    result <- .criterion_model(fits[[i]], requested, context, dots)
    result$row$id <- i
    result$row$Model <- labels[i]
    result$notes <- context$notes
    result
  })
  numeric_columns <- c("nobs", "df", "df_residual", "logLik", "AIC", "BIC",
    "cAIC", "cAIC_df", "EDF_random", "deviance", "elpd_loo", "se_elpd_loo",
    "p_loo", "looic", "se_looic", "pareto_k_max", "pareto_k_bad",
    "loo_R2", "se_loo_R2", "bayes_R2", "se_bayes_R2", "log_lik", "se_log_lik")
  columns <- unique(c("id", "Model", "Backend", "Formula", "Distribution", "Link",
    "nobs", "df", "df_residual", "df_type", "likelihood", "AIC_type",
    "cAIC_method", "deviance_type", "converged", "pdHess",
    unlist(lapply(rows, function(x) names(x$row)))))
  out <- do.call(rbind, lapply(rows, function(x) {
    row <- x$row
    for (nm in setdiff(columns, names(row))) row[[nm]] <-
      if (nm %in% numeric_columns) NA_real_ else NA_character_
    as.data.frame(row[columns], stringsAsFactors = FALSE)
  }))
  for (nm in intersect(numeric_columns, names(out))) out[[nm]] <- as.numeric(out[[nm]])
  for (nm in c("converged", "pdHess")) out[[nm]] <- as.logical(out[[nm]])
  out <- .criterion_comparisons(out, rows, sort)
  issue_rows <- lapply(seq_along(rows), function(i) {
    if (!length(rows[[i]]$notes)) return(NULL)
    do.call(rbind, lapply(rows[[i]]$notes, function(note) {
      data.frame(Model = labels[i], criterion = note$criterion,
        status = note$status, detail = note$detail, stringsAsFactors = FALSE)
    }))
  })
  notes <- do.call(rbind, issue_rows)
  out$notes <- vapply(out$id, function(i) {
    paste(unique(vapply(rows[[i]]$notes, function(x)
      paste0(x$criterion, ": ", x$detail), character(1))), collapse = "; ")
  }, character(1))
  rownames(out) <- NULL
  attr(out, "criteria_notes") <- if (is.null(notes)) data.frame(
    Model = character(), criterion = character(), status = character(), detail = character()) else notes
  attr(out, "comparison_note") <- paste(
    "Groups check fitted row names, responses, weights, shared covariates, and likelihood targets.",
    "Row names must identify the same observations. Missing differences do not mean equal performance.",
    "Information criteria do not establish index robustness; LOO-PIT and refitted CV are separate.")
  out
}

.criterion_model <- function(model, requested, context, dots) UseMethod(".criterion_model")

.criterion_note <- function(context, criterion, detail, status = "unavailable") {
  context$notes[[length(context$notes) + 1L]] <- list(
    criterion = criterion, detail = detail, status = status)
  invisible(NULL)
}

.criterion_eval <- function(context, criterion, expr) {
  tryCatch(withCallingHandlers(expr, warning = function(w) {
    .criterion_note(context, criterion, conditionMessage(w), "warning")
    invokeRestart("muffleWarning")
  }), error = function(e) {
    .criterion_note(context, criterion, conditionMessage(e), "error")
    NULL
  })
}

.criterion_scalar <- function(value) {
  if (is.numeric(value) && length(value) == 1L && is.finite(value)) as.numeric(value) else NA_real_
}

.criterion_request <- function(requested, bayesian = FALSE) {
  defaults <- if (bayesian) c("loo", "bayes_r2") else c("aic", "bic", "loglik", "deviance")
  unique(c(setdiff(requested, "auto"), if ("auto" %in% requested) defaults))
}

.criterion_frame <- function(model, formula = NULL, data = NULL, weights = NULL) {
  frame <- tryCatch(if (is.null(data)) stats::model.frame(model) else
    stats::model.frame(formula, data = data, na.action = stats::na.pass), error = function(e) NULL)
  if (is.null(frame) && !is.null(data) && inherits(formula, "formula")) {
    # Spatial formulas can contain smooths or random terms not evaluated by
    # model.frame(). Retain the actual response and raw formula covariates.
    frame <- tryCatch({
      response <- eval(formula[[2L]], data, environment(formula))
      value <- data[setdiff(intersect(all.vars(formula), names(data)), all.vars(formula[[2L]]))]
      value <- cbind(data.frame(response = I(response), row.names = rownames(data)), value)
      names(value)[1L] <- paste(deparse(formula[[2L]]), collapse = "")
      value
    }, error = function(e) NULL)
  }
  if (is.null(frame) || !nrow(frame)) return(NULL)
  y <- if (is.null(attr(frame, "terms"))) frame[[1L]] else stats::model.response(frame)
  if (is.null(y) || anyNA(y)) return(NULL)
  if (is.null(weights)) weights <- stats::model.weights(frame)
  if (is.null(weights)) weights <- rep(1, nrow(frame))
  if (length(weights) != nrow(frame) || anyNA(weights)) return(NULL)
  list(frame = frame, y = y, weights = as.numeric(weights), rows = rownames(frame),
    response = names(frame)[1L])
}

.criterion_base <- function(model, backend, formula, family, context,
                            frame = NULL, converged = NA, pdHess = NA) {
  if (is.null(frame)) frame <- .criterion_frame(model)
  response <- if (is.null(frame)) NA_character_ else frame$response
  family_name <- paste(family$family, collapse = "+")
  family_key <- tolower(family_name)
  support <- if (grepl("delta|hurdle|zero_inflated|tweedie", family_key) &&
      !grepl("poisson|nbinom|negbinomial", family_key)) "mixed-continuous" else {
    if (grepl("binomial|bernoulli", family_key) &&
        !grepl("negative binomial|negbinomial", family_key)) "binomial" else {
      if (grepl("poisson|nbinom|negbinomial|negative binomial", family_key)) "count" else "continuous"
    }
  }
  list(row = list(Backend = backend, Formula = paste(deparse(formula), collapse = " "),
      Distribution = family_name, Link = paste(family$link, collapse = "+"),
      nobs = if (is.null(frame)) NA_real_ else nrow(frame$frame), df = NA_real_,
      df_residual = NA_real_, df_type = "not available", likelihood = "not available",
      AIC_type = NA_character_, cAIC_method = NA_character_,
      deviance_type = NA_character_, converged = converged, pdHess = pdHess),
    frame = frame, support = support, response = response,
    eligible = !identical(converged, FALSE) && !identical(pdHess, FALSE), loo = NULL)
}

.criterion_same_data <- function(a, b) {
  x <- a$frame; y <- b$frame
  if (is.null(x) || is.null(y) || !identical(a$support, b$support) ||
      !identical(x$response, y$response) || anyDuplicated(x$rows) ||
      anyDuplicated(y$rows) || !identical(x$rows, y$rows) ||
      !isTRUE(all.equal(x$y, y$y, check.attributes = FALSE)) ||
      !identical(x$weights, y$weights)) return(FALSE)
  common <- intersect(names(x$frame), names(y$frame))
  # Offsets can legitimately differ between candidate mean structures.
  common <- common[!grepl("^offset\\(|^\\(offset\\)$", common)]
  all(vapply(common, function(nm) {
    u <- x$frame[[nm]]; v <- y$frame[[nm]]
    if (is.factor(u)) u <- as.character(u)
    if (is.factor(v)) v <- as.character(v)
    isTRUE(all.equal(u, v, check.attributes = FALSE))
  }, logical(1)))
}

.criterion_comparisons <- function(out, rows, sort) {
  for (metric in intersect(c("AIC", "BIC", "cAIC", "looic"), names(out))) {
    group <- rep(NA_integer_, nrow(out))
    for (i in seq_len(nrow(out))) {
      if (!is.finite(out[[metric]][i]) || !isTRUE(rows[[i]]$eligible) ||
          is.null(rows[[i]]$frame)) next
      target <- function(j) if (metric == "looic") "loo" else {
        if (metric == "cAIC") "conditional" else out$AIC_type[j]
      }
      if (is.na(target(i)) || target(i) == "restricted") next
      earlier <- which(!is.na(group) & seq_along(group) < i)
      match <- earlier[vapply(earlier, function(j) {
        members <- which(group == group[j])
        all(vapply(members, function(k) (identical(target(i), target(k)) ||
          "ordinary" %in% c(target(i), target(k))) &&
          .criterion_same_data(rows[[i]], rows[[k]]), logical(1)))
      }, logical(1))]
      group[i] <- if (length(match)) group[match[1]] else max(c(0L, group), na.rm = TRUE) + 1L
    }
    out[[paste0(metric, "_group")]] <- ifelse(is.na(group), NA_character_, paste0(metric, "-", group))
    delta <- rep(NA_real_, nrow(out))
    for (g in unique(group[!is.na(group)])) {
      keep <- which(group == g)
      if (length(keep) > 1L) delta[keep] <- out[[metric]][keep] - min(out[[metric]][keep])
    }
    out[[paste0("delta_", metric)]] <- delta
  }
  if ("looic" %in% names(out)) {
    out$elpd_diff <- out$se_diff <- NA_real_
    for (g in unique(stats::na.omit(out$looic_group))) {
      keep <- which(out$looic_group == g)
      if (length(keep) < 2L || !all(vapply(rows[keep], function(x)
          inherits(x$loo, "loo"), logical(1)))) next
      objects <- lapply(rows[keep], `[[`, "loo")
      names(objects) <- as.character(keep)
      comparison <- loo::loo_compare(objects)
      # loo >= 2.10 returns a data frame with a model column; earlier
      # versions return a matrix whose row names carry model identities.
      ids <- if ("model" %in% colnames(comparison)) comparison[["model"]] else rownames(comparison)
      index <- as.integer(ids)
      if (anyNA(index) || !setequal(index, keep) || anyDuplicated(index)) {
        stop("Native LOO comparison model identities could not be aligned.", call. = FALSE)
      }
      out$elpd_diff[index] <- comparison[, "elpd_diff"]
      out$se_diff[index] <- comparison[, "se_diff"]
    }
  }
  metric <- if (isTRUE(sort)) {
    available <- intersect(c("looic", "AIC", "cAIC"), names(out))
    if (length(available)) available[1] else NULL
  } else if (is.character(sort)) c(aic = "AIC", caic = "cAIC", looic = "looic")[[tolower(sort)]] else NULL
  if (!is.null(metric)) {
    groups <- out[[paste0(metric, "_group")]]
    if (!is.null(groups) && !anyNA(groups) && length(unique(groups)) == 1L) {
      out <- out[order(out[[metric]]), , drop = FALSE]
    } else {
      warning("Rows were not sorted: no single verified comparison group for ", metric,
        ". Input order is retained.", call. = FALSE)
    }
  }
  out
}
