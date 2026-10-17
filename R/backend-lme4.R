# Native lme4 extraction. No conversion to another fitting package is used.
.lme4_formula_tool <- function(name, formula) {
  # lme4 2.x moved these formula utilities to reformulas; old lme4 versions
  # still provide them directly. Avoid deprecated wrappers on modern versions.
  package <- if (requireNamespace("reformulas", quietly = TRUE) &&
      name %in% getNamespaceExports("reformulas")) "reformulas" else "lme4"
  getExportedValue(package, name)(formula)
}

.lme4_check <- function(model, response_transform = "identity") {
  .require_model_backend(model)
  if (utils::packageVersion("lme4") < "1.1.36") stop("Update optional package 'lme4' to >= 1.1-36 for native joint covariance support.", call. = FALSE)
  if (!inherits(model, c("lmerMod", "glmerMod")) || inherits(model, "nlmerMod")) {
    stop("lme4 support requires an lmer() or glmer() fit, not nlmer().", call. = FALSE)
  }
  response_transform <- match.arg(response_transform, c("identity", "log"))
  fam <- stats::family(model)
  canonical <- .normalise_family_name(fam$family)$family
  allowed <- (canonical == "gaussian" && fam$link == "identity") ||
    (canonical == "binomial" && fam$link %in% c("logit", "probit", "cloglog")) ||
    (canonical %in% c("poisson", "negative_binomial", "gamma") && fam$link == "log")
  if (!allowed) stop("Supported lme4 families are Gaussian(identity), binomial(logit/probit/cloglog), and Poisson/NB2/Gamma(log). Custom families need a validated adapter.", call. = FALSE)
  lhs <- stats::formula(model)[[2L]]
  logged <- is.call(lhs) && length(lhs) == 2L && identical(lhs[[1L]], as.name("log")) && is.name(lhs[[2L]])
  trials <- is.call(lhs) && identical(lhs[[1L]], as.name("cbind")) && canonical == "binomial"
  if (!is.name(lhs) && !logged && !trials) stop("lme4 requires a named response, cbind binomial trials, or explicitly declared log(response).", call. = FALSE)
  if (logged && response_transform != "log") stop("Declare `response_transform = 'log'` for an lme4 model of log(response).", call. = FALSE)
  if (response_transform == "log" && !inherits(model, "lmerMod")) stop("`response_transform = 'log'` requires a Gaussian lmer() fit.", call. = FALSE)
  if (length(attr(lme4::getME(model, "X"), "col.dropped"))) stop("Rank-deficient lme4 designs are not supported; remove aliased columns before fitting.", call. = FALSE)
  opt <- model@optinfo
  messages <- opt$conv$lme4$messages %||% character()
  messages <- messages[!grepl("boundary.*singular", messages, ignore.case = TRUE)]
  if (any(opt$conv$opt != 0) || length(messages)) stop("The lme4 fit must have converged without optimiser/gradient warnings.", call. = FALSE)
  list(family = fam, canonical = canonical, transform = response_transform,
    response = paste(deparse(lhs), collapse = " "), logged = logged,
    natural_response = if (response_transform == "log") {
      if (logged) as.character(lhs[[2L]]) else paste0("exp(", as.character(lhs), ")")
    } else paste(deparse(lhs), collapse = " "),
    singular = lme4::isSingular(model), reml = lme4::isREML(model))
}

.lme4_X <- function(model, data) {
  term <- stats::delete.response(stats::terms(model, fixed.only = TRUE))
  frame <- stats::model.frame(model)
  mf <- stats::model.frame(term, data, na.action = stats::na.fail,
    xlev = stats::.getXlevels(term, frame))
  X <- stats::model.matrix(term, mf,
    contrasts.arg = attr(lme4::getME(model, "X"), "contrasts"))
  names <- names(lme4::fixef(model))
  if (!all(names %in% colnames(X))) stop("The lme4 prediction design does not match the fitted coefficients.", call. = FALSE)
  X[, names, drop = FALSE]
}

.lme4_data <- function(model, data = NULL) {
  frame <- stats::model.frame(model)
  if (is.null(data) && all(all.vars(stats::formula(model)) %in% names(frame))) data <- frame
  d <- .residual_observations(model, data, seq_len(stats::nobs(model)))$data
  f <- stats::formula(model)
  y <- tryCatch(eval(f[[2L]], envir = d, enclos = environment(f)), error = function(e) NULL)
  if (is.null(y) || !.same_observation_column(y, stats::model.response(stats::model.frame(model)))) stop("The supplied lme4 data do not reproduce the fitted response.", call. = FALSE)
  fitted <- lme4::getME(model, "X")
  if (!isTRUE(all.equal(unname(.lme4_X(model, d)), unname(as.matrix(fitted)),
      check.attributes = FALSE, tolerance = 1e-10))) {
    stop("The supplied lme4 data do not reproduce the fitted design.", call. = FALSE)
  }
  # Random predictors and grouping must reproduce the native design too.
  # Keep this validation sparse, even for large sets of grouping levels.
  random <- Matrix::t(lme4::mkNewReTrms(model, d, re.form = NULL,
    allow.new.levels = FALSE)$Zt)
  original <- lme4::getME(model, "Z")
  if (!identical(dim(random), dim(original)) || any(abs(random - original) > 1e-10)) {
    stop("The supplied lme4 data do not reproduce the fitted random-effect design.", call. = FALSE)
  }
  d
}

.lme4_Z <- function(model, data, reference_data = NULL) {
  Z <- lme4::getME(model, "Z")
  if (is.null(reference_data)) return(Z)
  # Include the fitted rows to retain EVERY fitted grouping level and preserve
  # native coefficient order, even when a reference grid uses only a subset.
  required <- all.vars(.lme4_formula_tool("reOnly", stats::formula(model)))
  if (!all(required %in% names(reference_data))) stop("The lme4 influence reference grid must contain random-effect predictors and grouping columns.", call. = FALSE)
  stats::predict(model, newdata = reference_data, re.form = NULL, type = "link")
  common <- union(all.vars(stats::delete.response(stats::terms(model, fixed.only = TRUE))), required)
  joined <- rbind(data[common], as.data.frame(reference_data)[common])
  native <- Matrix::t(lme4::mkNewReTrms(model, joined, re.form = NULL,
    allow.new.levels = FALSE)$Zt)
  if (ncol(native) != ncol(Z) || !isTRUE(all.equal(
      as.matrix(native[seq_len(nrow(data)), , drop = FALSE]), as.matrix(Z),
      check.attributes = FALSE, tolerance = 1e-10))) {
    stop("The native lme4 random-effect reference design changed coefficient order.", call. = FALSE)
  }
  native[nrow(data) + seq_len(nrow(reference_data)), , drop = FALSE]
}

.lme4_joint_covariance <- function(model) {
  q <- length(lme4::getME(model, "b"))
  p <- length(lme4::fixef(model))
  if (8 * (as.double(q) + p)^2 / 1024^2 > 256) stop("The dense lme4 joint coefficient covariance would exceed 256 MiB; use `uncertainty = 'none'` for influence previews.", call. = FALSE)
  V <- as.matrix(stats::vcov(model, full = TRUE))
  .index_check_covariance(V, q + p)
}

#' Influence diagnostics for lme4 mixed models
#'
#' Fixed terms and random effects use the same joint native conditional
#' covariance. Variance/dispersion parameters are held fixed;
#' GLMM covariance is a local Gaussian approximation, not posterior draws.
#' Singular fits remain flagged in metadata, rather than acquiring invented
#' variance. Dense joint covariance calculations are limited to 256 MiB.
#' Non-log natural differences are evaluated at the full fitted reference
#' predictor, including random effects. Analytic difference intervals condition
#' on that reference; joint simulations also propagate its coefficient dependence.
#' Influence reference grids require focus values and existing grouping levels;
#' response-index reference grids instead omit year and set group effects to zero.
#' Random effects form one term per grouping factor, named after the factor as
#' for brms (for example `vessel` or `area:vessel`), in the `random_effects`
#' component. Terms sharing a factor, such as a random intercept and slope, are
#' combined, and each term's CDI categories are the factor's fitted levels.
#'
#' @inheritParams influ.glm
#' @param model A fit from [lme4::lmer()], [lme4::glmer()], or [lme4::glmer.nb()].
#'   Nonlinear `nlmer()` and arbitrary custom families are not supported.
#' @param response_transform For lme4 only, `"identity"` (default) or `"log"`.
#'   Declare `"log"` for a Gaussian lmer model of `log(response)`, or a named
#'   precomputed log-response column. Influence ratios are on the natural scale.
#'   Response indices use `exp(eta + sigma^2/2)`; sigma and random-effect variance
#'   parameters are treated as known. Other transformations are not supported.
#' @return An [influ_diag] with fixed and random-effect components.
#' @export
influ.merMod <- function(model, focus, data = NULL, weights = NULL,
    reference_data = NULL, reference_weights = NULL, uncertainty = "auto",
    retain = "summary", probs = c(.025, .975), ndraws = 1000L, seed = NULL,
    draws_path = NULL, keep_model = FALSE, response_transform = "identity", ...) {
  info <- .lme4_check(model, response_transform)
  if (length(list(...))) stop("Unsupported lme4 influence arguments.", call. = FALSE)
  data <- .lme4_data(model, data)
  X <- lme4::getME(model, "X")
  Z <- lme4::getME(model, "Z")
  b <- as.numeric(lme4::getME(model, "b"))
  beta <- lme4::fixef(model)
  V <- if (uncertainty == "none") NULL else .lme4_joint_covariance(model)
  q <- length(b)
  fixed <- q + seq_along(beta)
  joint_draws <- if (uncertainty == "simulation") {
    saved <- if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) get(".Random.seed", .GlobalEnv) else NULL
    on.exit({
      if (is.null(saved)) {
        if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
      } else assign(".Random.seed", saved, .GlobalEnv)
    }, add = TRUE)
    .draw_mvn(ndraws, c(b, beta), V, seed)
  } else NULL
  labels <- attr(stats::terms(model, fixed.only = TRUE), "term.labels")
  assignment <- attr(X, "assign")
  terms <- stats::setNames(lapply(seq_along(labels), function(i) which(assignment == i)), labels)
  terms <- terms[lengths(terms) > 0]
  spec <- if (info$transform == "log") .new_family_spec("lognormal", "identity", "lme4") else {
    .new_family_spec(info$family$family, info$family$link, "lme4")
  }
  call <- stats::getCall(model)
  offset <- .influ_offset_sources(stats::formula(model), call, stats::model.offset(stats::model.frame(model)))
  .check_influ_offset_scope(spec, offset)
  response <- info$natural_response
  # The nominal series is on the observed natural-response scale as declared.
  observed <- stats::model.response(stats::model.frame(model))
  data[[response]] <- if (info$transform == "log") exp(observed) else {
    if (is.matrix(observed)) observed[, 1L] / rowSums(observed) else {
      if (is.factor(observed)) as.integer(observed) - 1L else observed
    }
  }
  reference_X <- if (is.null(reference_data)) X else .lme4_X(model, reference_data)
  reference_Z <- .lme4_Z(model, data, reference_data)
  reference_w <- if (is.null(reference_data)) .resolve_influ_weights(data, weights) else
    .resolve_influ_weights(as.data.frame(reference_data), reference_weights)
  fixed_reference <- .weighted_col_mean(reference_X, reference_w)
  random_reference <- .weighted_col_mean(reference_Z, reference_w)
  eta <- sum(fixed_reference * beta) + sum(random_reference * b)
  eta_draws <- if (is.null(joint_draws)) NULL else as.numeric(joint_draws %*%
    c(random_reference, fixed_reference))
  run <- function(design, coefficients, covariance, terms, reference, component,
                  response, columns, term_data = data, standardise = TRUE) {
    .influ_linear_engine(backend = "lme4", model = model, data = term_data,
      response = response, focus = focus, family_spec = spec, X = design,
      beta = coefficients, vcov = covariance, term_columns = terms,
      beta_draws = if (is.null(joint_draws)) NULL else joint_draws[, columns, drop = FALSE],
      uncertainty = uncertainty, retain = if (retain == "disk") "derived_draws" else retain,
      probs = probs, weights = weights, reference_data = reference_data,
      reference_X = reference, reference_weights = reference_weights,
      component = component, ndraws = ndraws, seed = seed,
      reference_eta = eta, reference_eta_draws = eta_draws,
      notes = "Native joint conditional coefficient covariance; variance parameters held fixed.",
      standardise = standardise)
  }
  fixed_diag <- run(X, beta, if (is.null(V)) NULL else V[fixed, fixed, drop = FALSE],
    terms, if (is.null(reference_data)) NULL else reference_X, "conditional", response, fixed)
  colnames(Z) <- paste0("random_", seq_len(q))
  if (!is.null(reference_Z)) colnames(reference_Z) <- colnames(Z)
  # One term per grouping factor; lme4 orders the Z blocks as cnms and Gp.
  grouped <- .random_group_terms(names(lme4::getME(model, "cnms")),
    diff(lme4::getME(model, "Gp")), lme4::getME(model, "flist"), data, focus,
    "lme4")
  random_diag <- run(Z, b, if (is.null(V)) NULL else V[seq_len(q), seq_len(q), drop = FALSE],
    grouped$term_columns, if (is.null(reference_data)) NULL else reference_Z,
    "random_effects", NULL, seq_len(q), grouped$data, standardise = FALSE)
  out <- .combine_influ_diags(list(fixed = fixed_diag, random = random_diag),
    "lme4", spec, focus, model, keep_model,
    notes = "Joint fixed/random conditional covariance; Gaussian approximation; variance parameters not propagated.")
  out$metadata$lme4 <- list(response_transform = info$transform, REML = info$reml,
    native_response = info$response,
    singular = info$singular, covariance = "native joint conditional covariance, variance parameters fixed")
  out$metadata$lme4$reference_predictor <- eta
  out$metadata$random_effect_terms <- .random_terms_table(grouped$identified,
    "random_effects")
  if (retain == "disk") {
    if (is.null(draws_path)) stop("`draws_path` is required for disk retention.", call. = FALSE)
    saveRDS(out$draws, draws_path, compress = FALSE)
    out$draws <- NULL
    out$retained$mode <- "disk"
    out$retained$path <- normalizePath(draws_path, mustWork = TRUE)
  }
  .record_influ_offset_scope(out, offset)
}

#' @rdname influ.merMod
#' @export
influ.lmerMod <- influ.merMod

#' @rdname influ.merMod
#' @export
influ.glmerMod <- influ.merMod
