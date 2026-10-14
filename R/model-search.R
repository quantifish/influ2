#' Search a constrained set of CPUE models
#'
#' A bounded, buildmer-style search using each backend's native fitter and
#' information criteria. The default is a preflight report: fitting requires
#' the explicit argument `refit = TRUE`.
#'
#' @param model One fitted GLM, mgcv GAM, glmmTMB, lme4, brms, sdmTMB, or
#'   tinyVAST model. Each search uses one backend and one family/link.
#' @param year Optional name of the annual variable. Otherwise the annual
#'   variable is detected using the same conventions as other CPUE interfaces.
#' @param scope Optional upper formula, or a list with `lower` and `upper`
#'   formulas. The fitted formula is the default upper scope. Required terms
#'   are retained even when omitted from a supplied scope.
#' @param include Additional terms to retain, as a one-sided formula or
#'   character vector. For annual interaction coding, explicitly include the
#'   base annual block, for example `~ fyear:season`.
#' @param optional Optional formula or character vector naming the only terms
#'   to search. Other terms remain protected. This can explicitly nominate
#'   non-essential random effects; annual base terms and offsets cannot be
#'   released. Without this argument, ordinary covariates are optional, while
#'   random effects and multidimensional smooths are protected.
#' @param direction `"forward"`, `"backward"`, `"both"`, or
#'   `c("order", "backward")`. The latter builds an admissible maximal model
#'   by adding converged blocks to the protected core, then prunes it. Ordering
#'   can add terms even when the criterion increases. Other moves must improve
#'   the criterion by more than a numerical tolerance of `1e-6`.
#' @param criterion `"auto"`, `"AIC"`, `"BIC"`, `"cAIC"`, or `"LOOIC"`
#'   (also `"loo"`). Automatic selection uses AIC for frequentist backends and
#'   LOOIC for brms. Native cAIC is only available for supported backends; no
#'   replacement mixed-model penalty is invented.
#' @param max_models Positive integer limit on distinct evaluated candidate
#'   specifications, including a reused original fit. Failed candidates count.
#' @param refit Logical; `FALSE` reports the constraints and fitting plan
#'   without optimisation, MCMC, or criterion calculation.
#' @param data Optional original raw data, required when the saved call cannot
#'   recover them. Fitted rows, responses, weights, and offsets are checked.
#' @param component For a joint sdmTMB delta model, explicitly select
#'   `"occurrence"` or `"positive"`; the other component stays fixed.
#' @param keep_fits `"best"` (default), `"all"`, or `"none"`. Native fit
#'   objects can be large; candidate tables and paths are always compact.
#' @param refit_args Named list of native execution controls. Structural
#'   changes to families, data, offsets, priors, meshes, or process definitions
#'   are rejected. For lme4 negative-binomial models, explicitly state
#'   `reestimate_nb = TRUE` or `FALSE`.
#' @param seed Optional non-negative integer. Evaluation seeds are recorded;
#'   a reused fit retains its original draws. The caller's random-number state
#'   is restored on success or failure.
#' @param blocks Optional named list of disjoint term groups, moved together.
#'   Members use term labels, not coefficient names.
#'
#' @details
#' Year, response definition, offsets, weights, and essential random/spatial
#' structure are fixed specifications, not fixed numerical estimates. Estimated
#' coefficients, variance/dispersion parameters, and latent effects are
#' re-estimated in each candidate, except parameters deliberately fixed in the
#' original native specification. Formula terms are atomic, and interaction
#' dependencies present in the upper scope are respected. New main effects
#' are not invented for intentional interaction-only annual coding.
#'
#' Gaussian mixed models fitted by REML are refitted using ML for searching
#' fixed effects; this conversion is recorded. mgcv's native AIC is not its
#' REML smoothing objective. Priors/penalties that invalidate ordinary
#' maximum-likelihood ranking, missing convergence evidence, unreliable
#' brms LOO estimates, and incompatible observations/likelihood targets make
#' a candidate ineligible, rather than silently switching criteria.
#'
#' Every distinct specification is evaluated at most once. The recommendation
#' is the best eligible evaluated model, not a guaranteed global optimum.
#' Search stopping, warnings, singular fits, and failed fits remain visible.
#' There is no production refitting, search-driven residual simulation, model
#' averaging, or automatic spatial-process selection. Advanced nonlinear or
#' distributional formulas that cannot safely retain auxiliary specifications
#' fail explicitly. Positional parameter maps are not automatically remapped
#' as terms change: explicitly mapped TMB models require separately specified
#' candidates instead. Stored sdmTMB latent-process formulas and native trial
#' counts are retained, rather than re-evaluated from changed call variables.
#' Use [check_cpue()] on the shortlist separately: residual
#' p-values are not term-removal rules. Post-selection intervals remain
#' conditional on the chosen model and do not include selection uncertainty.
#' Compact retention limits stored results, not native fitting, LOO, or
#' Hessian workspace. In particular, brms criterion calculations can still
#' require substantial temporary memory.
#'
#' @return An `influ_search` object with `terms`, `candidates`, `path`,
#'   `metadata`, and the retained `best_model` (unless `keep_fits = "none"`).
#'   [selected_model()] retrieves the recommended native fit. Printing,
#'   summaries, and plotting use stored results without new fits or simulations.
#' @seealso [table_criterion()], [check_cpue()], [influ_steps()], [cpue_index()]
#' @references
#' Voeten (2020). buildmer: Stepwise elimination and term ordering for mixed
#' models. <https://cran.r-project.org/package=buildmer>.
#' @examples
#' set.seed(12)
#' d <- data.frame(year = factor(rep(2020:2023, each = 15)), x = rnorm(60))
#' d$response <- rpois(60, exp(1 + 0.5 * d$x))
#' fit <- glm(response ~ year + x, data = d, family = poisson())
#' search_cpue(fit, year = "year") # preflight only
#' result <- search_cpue(fit, year = "year", direction = "backward", refit = TRUE)
#' result$candidates[c("candidate", "formula", "score", "status")]
#' selected_model(result)
#' @md
#' @export
search_cpue <- function(model, year = NULL, scope = NULL, include = NULL,
    optional = NULL, direction = c("order", "backward"), criterion = "auto",
    max_models = 50, refit = FALSE, data = NULL, component = NULL,
    keep_fits = "best", refit_args = list(), seed = NULL, blocks = NULL) {
  if (!is.character(direction) || anyNA(direction) ||
      !(identical(direction, c("order", "backward")) ||
        (length(direction) == 1L && direction %in% c("forward", "backward", "both"))))
    stop("`direction` must be forward, backward, both, or c('order', 'backward').", call. = FALSE)
  if (!is.logical(refit) || length(refit) != 1L || is.na(refit))
    stop("`refit` must be TRUE or FALSE.", call. = FALSE)
  if (!is.numeric(max_models) || length(max_models) != 1L || !is.finite(max_models) ||
      max_models < 1 || max_models != floor(max_models) || max_models > .Machine$integer.max)
    stop("`max_models` must be a positive integer.", call. = FALSE)
  if (!is.character(keep_fits) || length(keep_fits) != 1L || is.na(keep_fits) ||
      !keep_fits %in% c("best", "all", "none"))
    stop("`keep_fits` must be best, all, or none.", call. = FALSE)
  if (!is.null(seed) && (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed) ||
      seed < 0 || seed != floor(seed) || seed > .Machine$integer.max))
    stop("`seed` must be a non-negative integer or NULL.", call. = FALSE)
  old_seed <- if (exists(".Random.seed", .GlobalEnv, inherits = FALSE))
    get(".Random.seed", .GlobalEnv) else NULL
  on.exit({
    if (is.null(old_seed)) {
      if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
    } else assign(".Random.seed", old_seed, .GlobalEnv)
  }, add = TRUE)
  backend <- .step_backend(model)
  if (backend == "lm") stop("Use a Gaussian GLM or a supported mixed-model backend, not an lm object.", call. = FALSE)
  package <- switch(backend, glm = "stats", gam = "mgcv", merMod = "lme4", brmsfit = "brms", backend)
  if (!.search_package_available(package))
    stop("Package '", package, "' is required for model search.", call. = FALSE)
  spec <- .search_prepare_spec(model, year, scope, include, optional, component, blocks)
  if ("seed" %in% names(refit_args)) stop("Supply the search seed via `seed`, not `refit_args`.", call. = FALSE)
  metric <- .search_metric(criterion, spec$backend)
  args <- .search_refit_args(model, spec$backend, refit_args)
  locked <- .search_locked_data(model, spec$upper_formula, spec$backend, data)
  metadata <- list(backend = spec$backend, year = spec$year, component = spec$component,
    annual_protection = spec$annual_protection,
    lower_formula = .search_formula_text(spec$lower_formula),
    upper_formula = .search_formula_text(spec$upper_formula),
    blocks = spec$blocks, dependencies = spec$dependencies,
    refit_controls = if (length(args$args)) paste(deparse(args$args, width.cutoff = 500L),
      collapse = " ") else "Original native execution settings retained.",
    criterion = metric, direction = direction, refit = refit, max_models = as.integer(max_models),
    keep_fits = keep_fits, seed = seed, nobs = nrow(locked$data),
    notes = args$notes, fitting_specification = paste(
      "Response, fitted rows, weights, offsets, family/link, and protected processes retained;",
      "estimated parameters re-estimated natively."),
    selection_uncertainty = "Intervals conditional on the selected model; selection uncertainty not included.",
    stop_reason = if (refit) "not_started" else "preflight_only", best_candidate = NA_integer_)
  out <- list(terms = spec$terms, candidates = .search_empty_candidates(),
    path = .search_empty_path(), metadata = metadata, best_model = NULL,
    fits = if (keep_fits == "all") list() else NULL)
  class(out) <- "influ_search"
  if (!refit) return(out)
  .search_run(model, spec, metric, args, locked, direction, max_models, seed, out)
}

.search_metric <- function(criterion, backend) {
  if (!is.character(criterion) || length(criterion) != 1L || is.na(criterion))
    stop("`criterion` must be auto, AIC, BIC, cAIC, or LOOIC.", call. = FALSE)
  value <- tolower(criterion)
  if (value == "auto") value <- if (backend == "brmsfit") "looic" else "aic"
  if (value == "loo") value <- "looic"
  if (!value %in% c("aic", "bic", "caic", "looic"))
    stop("`criterion` must be auto, AIC, BIC, cAIC, or LOOIC.", call. = FALSE)
  if ((backend == "brmsfit") != (value == "looic"))
    stop("brms searches use LOOIC; likelihood-based searches use native AIC, BIC, or cAIC.", call. = FALSE)
  if (value == "caic" && backend %in% c("glmmTMB", "merMod"))
    stop("No supported native cAIC method for this backend; choose AIC or BIC.", call. = FALSE)
  c(aic = "AIC", bic = "BIC", caic = "cAIC", looic = "looic")[[value]]
}

.search_package_available <- function(package) requireNamespace(package, quietly = TRUE)

.search_empty_candidates <- function() data.frame(candidate = integer(), key = character(),
  formula = character(), n_terms = integer(), score = numeric(), eligible = logical(),
  status = character(), detail = character(), warnings = character(), flags = character(),
  refitted = logical(), seed = numeric(), elapsed = numeric(), stringsAsFactors = FALSE)

.search_empty_path <- function() data.frame(step = integer(), phase = character(),
  action = character(), terms = character(), candidate = integer(), score = numeric(),
  improvement = numeric(), stringsAsFactors = FALSE)

.search_key <- function(ids) if (length(ids)) paste(sort(ids), collapse = "+") else "<intercept>"

.search_formula_text <- function(formula) paste(vapply(.step_formulas(formula),
  function(f) paste(deparse(f, width.cutoff = 500L), collapse = " "), character(1)), collapse = " ; ")

.search_bind_rows <- function(rows) {
  if (!length(rows)) return(.search_empty_candidates())
  columns <- unique(unlist(lapply(rows, names)))
  proto <- lapply(columns, function(nm) {
    value <- rows[[which(vapply(rows, function(row) nm %in% names(row), logical(1)))[1L]]][[nm]]
    if (is.logical(value)) NA else if (is.numeric(value)) NA_real_ else NA_character_
  })
  names(proto) <- columns
  do.call(rbind, lapply(rows, function(row) {
    for (nm in setdiff(columns, names(row))) row[[nm]] <- proto[[nm]]
    as.data.frame(row[columns], stringsAsFactors = FALSE)
  }))
}

.search_score <- function(fit, metric, reference = NULL) {
  context <- new.env(parent = emptyenv()); context$notes <- list()
  native <- .criterion_model(fit, tolower(if (metric == "looic") "loo" else metric), context, list())
  value <- native$row[[metric]]
  eligible <- isTRUE(native$eligible) && is.numeric(value) && length(value) == 1L && is.finite(value)
  target <- if (metric == "looic") "loo" else if (metric == "cAIC") "conditional" else native$row$AIC_type
  comparable <- !is.null(native$frame) && length(target) == 1L && !is.na(target) && target != "restricted"
  if (!is.null(reference)) comparable <- comparable && .criterion_same_data(native, reference) &&
    (identical(target, reference$target) || "ordinary" %in% c(target, reference$target))
  notes <- vapply(context$notes, function(x) paste(x$criterion, x$detail, sep = ": "), character(1))
  if (!comparable) notes <- c(notes, "Fitted observations or likelihood targets cannot be verified as comparable.")
  if (!eligible && !length(notes)) notes <- "Native criterion or ranking eligibility unavailable."
  native$loo <- NULL
  native$target <- target
  list(score = if (is.numeric(value) && length(value) == 1L) as.numeric(value) else NA_real_,
    eligible = eligible && comparable, detail = paste(unique(notes), collapse = " "),
    statistics = native$row, reference = native)
}

.search_move_units <- function(spec) {
  groups <- Filter(function(ids) all(ids %in% spec$optional_ids), spec$blocks)
  grouped <- unlist(groups, use.names = FALSE)
  singles <- setdiff(spec$optional_ids, grouped)
  c(groups, stats::setNames(lapply(singles, function(x) x), singles))
}

.search_run <- function(model, spec, metric, args, locked, direction, max_models, seed, out) {
  cache <- new.env(parent = emptyenv())
  rows <- list(); path <- list(); reference <- NULL; best_fit <- NULL
  best_id <- NA_integer_; best_score <- Inf; best_terms <- Inf; best_key <- NULL
  retained <- list(); exhausted <- FALSE; units <- .search_move_units(spec)
  all_ids <- spec$terms$id
  label <- function(ids) paste(spec$terms$term[match(ids, spec$terms$id)], collapse = " + ")
  evaluate <- function(ids) {
    ids <- all_ids[all_ids %in% ids]
    key <- .search_key(ids)
    if (exists(key, cache, inherits = FALSE)) return(get(key, cache, inherits = FALSE))
    if (length(rows) >= max_models) { exhausted <<- TRUE; return(NULL) }
    id <- length(rows) + 1L
    f <- .search_formula(spec, ids)
    candidate_seed <- if (is.null(seed)) NA_real_ else (as.double(seed) + id - 1) %% .Machine$integer.max
    if (is.finite(candidate_seed)) set.seed(as.integer(candidate_seed))
    warnings <- character(); tick <- proc.time()[[3L]]
    reused <- !args$needs_original_refit && !length(args$args) && setequal(ids, spec$original_ids) &&
      length(ids) == length(.search_formula_terms(spec$original_component_formula))
    fit <- NULL
    value <- tryCatch(withCallingHandlers({
      native_args <- args$args
      if (spec$backend == "brmsfit" && is.finite(candidate_seed)) native_args$seed <- as.integer(candidate_seed)
      fit <- if (reused) model else .search_refit_one(model, f, native_args, locked, spec$backend)
      status <- .search_fit_status(fit, spec$backend)
      if (!isTRUE(status$eligible)) list(score = NA_real_, eligible = FALSE,
        detail = status$detail, statistics = list(), reference = NULL, flags = status$flags) else {
        score <- .search_score(fit, metric, reference)
        score$detail <- paste(status$detail, score$detail)
        score$flags <- status$flags
        score
      }
    }, warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning")
    }), error = function(e) list(score = NA_real_, eligible = FALSE,
      detail = conditionMessage(e), statistics = list(), reference = NULL, flags = character()))
    row <- c(list(candidate = id, key = key, formula = .search_formula_text(f),
      n_terms = length(ids), score = value$score, eligible = value$eligible,
      status = if (value$eligible) "eligible" else "ineligible", detail = value$detail,
      warnings = paste(unique(warnings), collapse = " | "), flags = paste(value$flags, collapse = " | "),
      refitted = !reused, seed = candidate_seed, elapsed = unname(proc.time()[[3L]] - tick)), value$statistics)
    rows[[id]] <<- row
    result <- list(candidate = id, ids = ids, score = value$score, eligible = value$eligible)
    assign(key, result, cache)
    if (isTRUE(value$eligible)) {
      if (is.null(reference)) reference <<- value$reference
      better <- value$score < best_score - 1e-6 ||
        (abs(value$score - best_score) <= 1e-6 &&
          (length(ids) < best_terms || (length(ids) == best_terms && (is.null(best_key) || key < best_key))))
      if (better) {
        best_id <<- id; best_score <<- value$score; best_terms <<- length(ids); best_key <<- key
        if (out$metadata$keep_fits != "none") best_fit <<- fit
      }
    }
    if (out$metadata$keep_fits == "all" && !is.null(fit)) retained[[paste0("candidate_", id)]] <<- fit
    result
  }
  record <- function(current, phase, action, moved = character(), previous = NA_real_) {
    path[[length(path) + 1L]] <<- data.frame(step = length(path), phase = phase,
      action = action, terms = label(moved), candidate = current$candidate,
      score = current$score, improvement = previous - current$score, stringsAsFactors = FALSE)
  }
  neighbours <- function(current, add = FALSE, drop = FALSE) {
    options <- list()
    for (unit in units) {
      present <- all(unit %in% current$ids)
      if ((present && drop) || (!present && add)) {
        ids <- if (present) setdiff(current$ids, unit) else union(current$ids, unit)
        if (.search_legal(spec, ids)) options[[length(options) + 1L]] <-
          list(ids = ids, action = if (present) "drop" else "add", moved = unit)
      }
    }
    candidates <- lapply(options, function(x) { x$value <- evaluate(x$ids); x })
    candidates <- Filter(function(x) !is.null(x$value) && isTRUE(x$value$eligible), candidates)
    if (!length(candidates)) return(NULL)
    scores <- vapply(candidates, function(x) x$value$score, numeric(1))
    sizes <- vapply(candidates, function(x) length(x$ids), integer(1))
    keys <- vapply(candidates, function(x) .search_key(x$ids), character(1))
    candidates[[order(scores, sizes, keys)[1L]]]
  }
  ordering <- identical(direction, c("order", "backward"))
  start <- if (ordering || identical(direction, "forward")) spec$protected_ids else all_ids
  current <- evaluate(start)
  record(current, if (ordering) "order" else direction, "start")
  reason <- "no_improving_legal_move"
  if (!isTRUE(current$eligible) && setequal(start, spec$protected_ids)) {
    reason <- "protected_core_ineligible"
  } else {
    if (ordering) {
      repeat {
        move <- neighbours(current, add = TRUE)
        if (is.null(move)) break
        previous <- current$score; current <- move$value
        record(current, "order", "add", move$moved, previous)
      }
    }
    phase <- if (ordering) "backward" else direction
    repeat {
      move <- neighbours(current, add = phase %in% c("forward", "both"),
        drop = phase %in% c("backward", "both"))
      if (is.null(move)) break
      if (isTRUE(current$eligible) && move$value$score >= current$score - 1e-6) break
      previous <- current$score; current <- move$value
      record(current, phase, move$action, move$moved, previous)
    }
  }
  out$candidates <- .search_bind_rows(rows)
  out$path <- if (length(path)) do.call(rbind, path) else .search_empty_path()
  out$metadata$stop_reason <- if (exhausted) "max_models_reached" else if (is.na(best_id) &&
    reason != "protected_core_ineligible") "no_eligible_model" else reason
  out$metadata$best_candidate <- best_id
  out$metadata$final_path_candidate <- current$candidate
  out$metadata$distinct_models <- length(rows)
  out$metadata$refitted_models <- sum(out$candidates$refitted)
  out$metadata$recommendation <- "Best eligible evaluated model; not a guarantee of a global optimum."
  out$best_model <- best_fit
  if (out$metadata$keep_fits == "all") out$fits <- retained
  out
}

#' Retrieve the recommended model from a CPUE search
#'
#' @param object An `influ_search` result from [search_cpue()].
#' @return The retained native fitted model, without refitting.
#' @md
#' @export
selected_model <- function(object) {
  if (!inherits(object, "influ_search")) stop("`object` must be an influ_search result.", call. = FALSE)
  if (!isTRUE(object$metadata$refit)) stop("This is a preflight report; use refit = TRUE to run a search.", call. = FALSE)
  if (is.na(object$metadata$best_candidate)) stop("No eligible model was found; inspect candidate details.", call. = FALSE)
  if (is.null(object$best_model)) stop("The selected fit was not retained; use keep_fits = 'best' or 'all'.", call. = FALSE)
  object$best_model
}

#' @rdname search_cpue
#' @param x,object An `influ_search` object.
#' @param ... Reserved for additional method arguments.
#' @export
print.influ_search <- function(x, ...) {
  cat("<influ_search> ", x$metadata$backend, " / ", x$metadata$criterion,
    "\n  Mode: ", if (x$metadata$refit) "native refitting" else "preflight only",
    "\n  Protected terms: ", sum(x$terms$protected),
    "\n  Evaluated candidates: ", nrow(x$candidates),
    "\n  Stop: ", x$metadata$stop_reason, "\n", sep = "")
  if (isTRUE(x$metadata$refit) && !is.na(x$metadata$best_candidate)) {
    row <- x$candidates[x$candidates$candidate == x$metadata$best_candidate, , drop = FALSE]
    cat("  Best evaluated: ", row$formula, " (", format(row$score), ")\n", sep = "")
  }
  if (length(x$metadata$notes)) cat("  ", paste(x$metadata$notes, collapse = " "), "\n", sep = "")
  invisible(x)
}

#' @rdname search_cpue
#' @export
summary.influ_search <- function(object, ...) {
  list(metadata = object$metadata, terms = object$terms,
    candidates = object$candidates, path = object$path)
}

#' @rdname search_cpue
#' @param row.names Optional row names for the data-frame method. Its generic
#'   `optional` argument is reserved and does not change the retained table.
#' @export
as.data.frame.influ_search <- function(x, row.names = NULL, optional = FALSE, ...) {
  value <- x$candidates
  if (!is.null(row.names)) rownames(value) <- row.names
  value
}

#' Plot a stored CPUE model search
#'
#' @param x,object An `influ_search` object from [search_cpue()].
#' @param ... Reserved for additional plotting arguments.
#' @return A ggplot showing evaluated native criterion values. No fits,
#'   simulations, or criteria are recalculated.
#' @md
#' @name plot.influ_search
NULL

#' @rdname plot.influ_search
#' @export
plot.influ_search <- function(x, ...) {
  d <- x$candidates
  if (!nrow(d)) stop("No evaluated candidates to plot; this may be a preflight report.", call. = FALSE)
  d$recommended <- d$candidate == x$metadata$best_candidate
  ggplot2::ggplot(d[d$eligible & is.finite(d$score), , drop = FALSE],
      ggplot2::aes(x = .data$candidate, y = .data$score, colour = .data$recommended)) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::scale_colour_manual(values = c(`FALSE` = "grey50", `TRUE` = "#6A3D9A"), guide = "none") +
    ggplot2::labs(x = "Evaluated candidate", y = x$metadata$criterion,
      subtitle = paste("Best evaluated model, not necessarily a global optimum;",
        gsub("_", " ", x$metadata$stop_reason, fixed = TRUE)))
}

#' @rdname plot.influ_search
#' @export
autoplot.influ_search <- function(object, ...) plot.influ_search(object, ...)
