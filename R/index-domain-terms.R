# All matrices use native predictor-component order, including delta models.
# Only the prediction objective is evaluated; its owner must free `obj`.
.domain_projection <- function(model, backend, data, offset = NULL, year = NULL) {
  if (!backend %in% c("sdmTMB", "tinyVAST")) {
    stop("Native domain projections require sdmTMB or tinyVAST.", call. = FALSE)
  }
  if (!is.data.frame(data) || nrow(data) < 1L) {
    stop("Native domain projections require nonempty prediction data.", call. = FALSE)
  }
  if (!is.null(year) && (!is.character(year) || length(year) != 1L ||
      is.na(year) || !year %in% names(data))) {
    stop("`year` must name the prediction year column.", call. = FALSE)
  }
  if (backend == "tinyVAST" && !is.null(offset)) {
    stop("tinyVAST prediction offsets must be supplied through its formula.", call. = FALSE)
  }
  if (backend == "tinyVAST" &&
      (length(model$internal$family) != 1L || length(model$internal$variables) > 1L)) {
    stop("Domain projections require a univariate, single-family tinyVAST fit.", call. = FALSE)
  }
  if (backend == "sdmTMB" && !is.null(model$nonlocal_parsed)) {
    stop("Nonlocal sdmTMB operators are not supported for domain decomposition.", call. = FALSE)
  }
  if (backend == "sdmTMB" && is.null(model$family$family)) {
    stop("Domain projections require a single native sdmTMB family.", call. = FALSE)
  }
  block <- .index_spatial_projection(model, backend, data, "all", offset)
  obj <- block$obj
  ready <- FALSE
  on.exit(if (!ready) .index_free_tmb(obj), add = TRUE)
  td <- obj$env$data
  if (is.null(td)) stop("Native prediction data are unavailable.", call. = FALSE)
  n <- nrow(data)
  nc <- if (backend == "sdmTMB") {
    length(td$proj_X_ij)
  } else as.integer(td$components_e[1L])
  if (!nc %in% 1:2) stop("Only one- and two-component predictors are supported.", call. = FALSE)
  zero <- matrix(0, n, nc)
  cached_par <- cached <- NULL
  native <- function(par) {
    if (!is.numeric(par) || length(par) != length(obj$env$last.par) ||
        any(!is.finite(par))) {
      stop("Supply a finite native joint parameter vector.", call. = FALSE)
    }
    if (is.null(cached) || !identical(par, cached_par)) {
      fixed <- setdiff(seq_along(par), obj$env$random)
      cached <<- list(report = obj$report(par),
        parameters = obj$env$parList(x = par[fixed], par = par))
      cached_par <<- par
    }
    cached
  }
  eta <- function(par) {
    r <- native(par)$report
    value <- if (backend == "sdmTMB") r$proj_eta else {
      if (nc == 2L) cbind(r$p1_g, r$p2_g) else matrix(r$p1_g, ncol = 1L)
    }
    .domain_term_matrix(value, n, nc)
  }
  terms <- function(par) {
    state <- native(par)
    out <- if (backend == "sdmTMB") {
      .domain_sdmtmb_terms(model, td, state$parameters, state$report, zero)
    } else {
      .domain_tinyvast_terms(model, td, state$parameters, state$report, zero)
    }
    out <- .domain_normalize_terms(out, year)
    .domain_check_terms(out, eta(par))
    out
  }
  response <- function(par, subtract = NULL) {
    value <- eta(par)
    if (!is.null(subtract)) {
      value <- value - .domain_term_matrix(subtract, n, nc)
    }
    r <- native(par)$report
    p <- native(par)$parameters
    mu <- if (backend == "sdmTMB") {
      .domain_sdmtmb_response(model, td, p, value)
    } else {
      mu <- .domain_native_linkinv(value[, 1L], td$link_ez[1L, 1L])
      if (nc == 2L) mu <- mu *
        .domain_native_linkinv(value[, 2L], td$link_ez[1L, 2L])
      mu
    }
    # Detect upstream family changes rather than silently returning a different
    # estimand. These reports contain the native expected response, not data.
    expected <- if (backend == "tinyVAST") r$mu_g else r$proj_response_combined
    if (is.null(subtract) && !is.null(expected) &&
        (length(expected) != length(mu) ||
         any(!is.finite(expected)) || max(abs(mu - expected)) > 1e-8)) {
      stop("Native family response does not match the domain adapter.", call. = FALSE)
    }
    .index_check_prediction(mu, n)
  }
  ready <- TRUE
  list(obj = obj, terms = terms, eta = eta, response = response)
}

.domain_normalize_terms <- function(terms, year = NULL) {
  variables <- attr(terms, "variables")
  reserved <- c("spatial", "spatiotemporal", "svc", "temporal",
    "offset", "family_adjustment", "iid")
  out <- list()
  for (name in names(terms)) {
    label <- name
    vars <- variables[[name]]
    if (startsWith(name, "svc:")) label <- "svc"
    if (grepl("^(fixed|smooth):", name)) {
      label <- sub("^(fixed|smooth):", "", name)
      if (length(vars) == 1L && !grepl(":", label, fixed = TRUE)) {
        label <- if (identical(vars, year)) "year" else vars
      }
      if (label %in% reserved) label <- name
    }
    if (label != "svc" && !is.null(out[[label]]) &&
        !setequal(attr(out, "variables")[[label]], vars)) {
      stop("Distinct native predictor terms have the same normalized label: ",
        label, ".", call. = FALSE)
    }
    for (m in seq_len(ncol(terms[[name]]))) {
      out <- .domain_term_add(out, label, terms[[name]][, m], m,
        matrix(0, nrow(terms[[name]]), ncol(terms[[name]])), vars)
    }
  }
  out
}

.domain_term_matrix <- function(x, n, nc) {
  if (!is.numeric(x) || length(dim(x)) != 2L ||
      !identical(as.integer(dim(x)), as.integer(c(n, nc))) ||
      any(!is.finite(x))) {
    stop("Native domain terms must be finite row-by-component matrices.", call. = FALSE)
  }
  matrix(as.numeric(x), n, nc, dimnames = dimnames(x))
}

.domain_check_terms <- function(terms, eta) {
  if (!length(terms) || is.null(names(terms)) ||
      anyNA(names(terms)) || any(!nzchar(names(terms))) || anyDuplicated(names(terms))) {
    stop("Native domain terms must have unique, nonempty names.", call. = FALSE)
  }
  pieces <- lapply(terms, .domain_term_matrix, n = nrow(eta), nc = ncol(eta))
  if (max(abs(Reduce(`+`, pieces) - eta)) > 1e-8) {
    stop("Native domain terms do not sum to the full predictor within 1e-8; this model structure is unsupported.", call. = FALSE)
  }
  invisible(NULL)
}

.domain_term_add <- function(out, name, value, component, zero, variables = character()) {
  if (!is.numeric(value) || length(value) != nrow(zero) || any(!is.finite(value))) {
    stop("Native domain term values do not align with prediction rows.", call. = FALSE)
  }
  if (is.null(out[[name]])) out[[name]] <- zero
  out[[name]][, component] <- out[[name]][, component] + as.numeric(value)
  vars <- attr(out, "variables")
  vars[[name]] <- union(vars[[name]], variables)
  attr(out, "variables") <- vars
  out
}

.domain_fixed_terms <- function(out, X, beta, component, zero, term = NULL) {
  if (ncol(X) != length(beta)) {
    stop("Native fixed coefficients and prediction design do not align.", call. = FALSE)
  }
  assignment <- attr(X, "assign")
  labels <- if (!is.null(term)) attr(stats::terms(term), "term.labels") else NULL
  for (j in seq_len(ncol(X))) {
    label <- colnames(X)[j]
    if (is.null(label) || is.na(label) || !nzchar(label)) label <- paste0("column", j)
    if (length(assignment) == ncol(X) && assignment[j] == 0L) {
      label <- "(Intercept)"
    } else if (length(assignment) == ncol(X) && assignment[j] <= length(labels)) {
      label <- labels[assignment[j]]
    }
    variables <- if (label == "(Intercept)") character() else {
      tryCatch(all.vars(str2lang(label)), error = function(e) label)
    }
    out <- .domain_term_add(out, paste0("fixed:", label), X[, j] * beta[j],
      component, zero, variables)
  }
  out
}

.domain_sdmtmb_terms <- function(model, td, p, r, zero) {
  out <- list()
  nc <- ncol(zero)
  n <- nrow(zero)
  for (m in seq_len(nc)) {
    out <- .domain_fixed_terms(out, td$proj_X_ij[[m]],
      p[[if (m == 1L) "b_j" else "b_j2"]], m, zero, model$terms[[m]])
  }
  out$offset <- zero
  out$offset[, nc] <- td$proj_offset_i
  out$spatial <- .domain_term_matrix(r$proj_omega_s_A, n, nc)
  out$spatiotemporal <- .domain_term_matrix(r$proj_epsilon_st_A_vec, n, nc)
  if (ncol(td$proj_z_i)) {
    for (j in seq_len(ncol(td$proj_z_i))) {
      label <- colnames(td$proj_z_i)[j]
      if (is.null(label)) label <- model$spatial_varying[j]
      if (is.null(label) || is.na(label)) label <- paste0("column", j)
      for (m in seq_len(nc)) {
        out <- .domain_term_add(out, paste0("svc:", label),
          r$proj_zeta_s_A[, j, m] * td$proj_z_i[, j], m, zero,
          tryCatch(all.vars(str2lang(label)), error = function(e) label))
      }
    }
  }
  if (isTRUE(as.logical(td$has_smooths))) {
    # Each smooth may have several random blocks (tensor products/by levels)
    # and an unpenalized block. Keep these together as one scientific term.
    random <- linear <- 0L
    for (i in seq_along(model$smoothers$basis_out)) {
      basis <- model$smoothers$basis_out[[i]]
      label <- model$smoothers$labels[[i]]
      if (is.null(label)) label <- basis[[1L]]$label
      variables <- unique(unlist(lapply(basis, function(b) {
        c(b$term, if (!identical(b$by, "NA")) b$by)
      })))
      value <- zero
      for (b in basis) {
        design <- mgcv::smooth2random(b, names(model$data), type = 2)
        for (unused in seq_along(design$rand)) {
          random <- random + 1L
          Z <- td$proj_Zs[[random]]
          ids <- td$b_smooth_start[random] + seq_len(ncol(Z))
          for (m in seq_len(nc)) value[, m] <- value[, m] +
            as.numeric(Z %*% p$b_smooth[ids, m])
        }
        k <- ncol(design$Xf)
        if (!is.null(k) && k > 0L) {
          ids <- linear + seq_len(k)
          for (m in seq_len(nc)) value[, m] <- value[, m] +
            as.numeric(td$proj_Xs[, ids, drop = FALSE] %*% p$bs[ids, m])
          linear <- linear + k
        }
      }
      for (m in seq_len(nc)) out <- .domain_term_add(out, paste0("smooth:", label),
        value[, m], m, zero, variables)
    }
    if (random != length(td$proj_Zs) || linear != ncol(td$proj_Xs)) {
      stop("Native smoother basis blocks could not be aligned.", call. = FALSE)
    }
  }
  if (isTRUE(as.logical(td$ar1_time)) ||
      isTRUE(td$random_walk %in% c(1L, 2L))) {
    for (j in seq_len(ncol(td$proj_X_rw_ik))) {
      label <- colnames(td$proj_X_rw_ik)[j]
      if (is.null(label)) label <- paste0("column", j)
      for (m in seq_len(nc)) out <- .domain_term_add(out, paste0("temporal:", label),
        td$proj_X_rw_ik[, j] * p$b_rw_t[td$proj_year + 1L, j, m], m, zero,
        tryCatch(all.vars(str2lang(label)), error = function(e) label))
    }
  }
  if (!is.null(r$proj_iid_re_i) && any(r$proj_iid_re_i != 0)) {
    out$iid <- .domain_term_matrix(r$proj_iid_re_i, n, nc)
  }
  if (td$threshold_func > 0L) {
    for (m in seq_len(nc)) {
      b <- p$b_threshold[, m]
      value <- if (td$threshold_func == 1L) {
        b[1L] * pmin(td$proj_X_threshold, b[2L])
      } else b[3L] * stats::plogis(log(19) *
        (td$proj_X_threshold - b[1L]) / exp(b[2L]))
      out <- .domain_term_add(out, paste0("threshold:", model$threshold_parameter),
        value, m, zero, model$threshold_parameter)
    }
  }
  families <- if (nc == 2L) model$family[1:2] else list(model$family)
  for (m in seq_len(nc)) {
    if (grepl("_mix$", families[[m]]$family)) {
      proportion <- if (!is.null(p$logit_p_extreme)) {
        stats::plogis(p$logit_p_extreme)
      } else stats::plogis(p$logit_p_mix)
      ratio <- 1 + exp(p$log_ratio_mix)
      scale <- 1 + proportion * (ratio - 1)
      raw <- Reduce(`+`, out)[, m]
      link <- stats::make.link(families[[m]]$link)
      adjustment <- link$linkfun(link$linkinv(raw) * scale) - raw
      out <- .domain_term_add(out, "family_adjustment", adjustment, m, zero)
    }
  }
  out
}

.domain_tinyvast_terms <- function(model, td, p, r, zero) {
  out <- list()
  nc <- ncol(zero)
  for (m in seq_len(nc)) {
    second <- m == 2L
    X <- td[[if (second) "X2_gj" else "X_gj"]]
    gam <- model$internal[[if (second) "delta_gam_setup" else "gam_setup"]]
    out <- .domain_fixed_terms(out, X, p[[if (second) "alpha2_j" else "alpha_j"]],
      m, zero, gam$pterms)
    Z <- td[[if (second) "Z2_gk" else "Z_gk"]]
    gamma <- p[[if (second) "gamma2_k" else "gamma_k"]]
    start <- 0L
    for (sm in gam$smooth) {
      k <- ncol(mgcv::PredictMat(sm, data = model$data[1L, , drop = FALSE]))
      ids <- start + seq_len(k)
      out <- .domain_term_add(out, paste0("smooth:", sm$label),
        as.numeric(Z[, ids, drop = FALSE] %*% gamma[ids]), m, zero,
        c(sm$term, if (!identical(sm$by, "NA")) sm$by))
      start <- start + k
    }
    if (start != ncol(Z) || start != length(gamma)) {
      stop("Native tinyVAST smooth coefficients could not be aligned.", call. = FALSE)
    }
    reports <- c(spatial = "pomega", spatiotemporal = "pepsilon", temporal = "pdelta")
    for (label in names(reports)) {
      out <- .domain_term_add(out, label, r[[paste0(reports[[label]], m, "_g")]],
        m, zero)
    }
    W <- td[[if (second) "W2_gl" else "W_gl"]]
    xi <- p[[if (second) "xi2_sl" else "xi_sl"]]
    if (ncol(W) && length(xi)) {
      projected <- as.matrix(td$A_gs %*% xi) / exp(r$log_tau)
      for (j in seq_len(ncol(W))) {
        label <- colnames(W)[j]
        if (is.null(label)) label <- paste0("column", j)
        out <- .domain_term_add(out, paste0("svc:", label),
          projected[, j] * W[, j], m, zero,
          tryCatch(all.vars(str2lang(label)), error = function(e) label))
      }
    }
  }
  out$offset <- zero
  out$offset[, 1L] <- td$offset_g
  out
}

.domain_native_linkinv <- function(eta, code) {
  switch(as.character(code), `0` = eta, `1` = exp(eta),
    `2` = stats::plogis(eta), `3` = -expm1(-exp(eta)),
    stop("Unsupported native tinyVAST link.", call. = FALSE))
}

.domain_sdmtmb_response <- function(model, td, p, eta) {
  nc <- ncol(eta)
  if (nc == 2L && (identical(model$family$type, "poisson_link_delta") ||
      identical(model$family$type, "poisson-link"))) {
    return(exp(eta[, 1L] + eta[, 2L]))
  }
  families <- if (nc == 2L) model$family[1:2] else list(model$family)
  mu <- rep(1, nrow(eta))
  for (m in seq_len(nc)) {
    family <- families[[m]]
    value <- stats::make.link(family$link)$linkinv(eta[, m])
    if (family$family %in% c("truncated_nbinom1", "truncated_nbinom2")) {
      slot <- if (!is.null(td$ln_phi_slot)) td$ln_phi_slot[1L] + 1L else m
      phi <- exp(p$ln_phi[slot])
      log_prob0 <- if (family$family == "truncated_nbinom1") {
        -value / phi * log1p(phi)
      } else -phi * log1p(value / phi)
      value <- value / (-expm1(log_prob0))
    }
    if (identical(family$family, "ordbeta")) {
      lower <- p$psi[1L]
      upper <- lower + exp(p$psi[2L])
      p0 <- stats::plogis(lower - eta[, m])
      p1 <- stats::plogis(eta[, m] - upper)
      value <- p1 + (1 - p0 - p1) * value
    }
    mu <- mu * value
  }
  mu
}
