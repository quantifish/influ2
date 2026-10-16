test_that("native projection terms are recomputed for joint draws", {
  X <- stats::model.matrix(~ year + depth,
    data.frame(year = factor(c("a", "b")), depth = c(2, 4)))
  z <- matrix(0, 2, 1)
  td <- list(proj_X_ij = list(X), proj_offset_i = c(0.2, 0.4),
    proj_z_i = matrix(0, 2, 0), has_smooths = 0L,
    ar1_time = 0L, random_walk = 0L, threshold_func = 0L)
  model <- list(family = stats::poisson(), terms = list(stats::terms(~ year + depth)))
  env <- new.env(parent = emptyenv())
  env$data <- td
  env$last.par <- c(1, 2, 3)
  env$random <- integer()
  env$parList <- function(x, par) list(b_j = par)
  reports <- 0L
  obj <- list(env = env, report = function(par) {
    reports <<- reports + 1L
    list(proj_eta = matrix(X %*% par + td$proj_offset_i, 2, 1),
      proj_omega_s_A = z, proj_epsilon_st_A_vec = z)
  })
  adapter <- .domain_projection
  scope <- new.env(parent = environment(adapter))
  scope$.index_spatial_projection <- function(...) list(obj = obj,
    response = function(par) stop("Baseline response must reuse the cached report"))
  scope$.index_free_tmb <- function(...) stop("Unexpected objective disposal")
  environment(adapter) <- scope
  block <- adapter(model, "sdmTMB", data.frame(row = 1:2))
  one <- block$terms(c(1, 2, 3))
  two <- block$terms(c(4, 5, 6))
  expect_equal(one[["depth"]][, 1], c(6, 12))
  expect_equal(two[["depth"]][, 1], c(12, 24))
  expect_equal(attr(one, "variables")[["depth"]], "depth")
  expect_equal(Reduce(`+`, two), block$eta(c(4, 5, 6)))
  expect_equal(block$response(c(4, 5, 6), subtract = two[["depth"]]),
    exp(4 + c(0, 5) + td$proj_offset_i))
  expect_equal(reports, 2L)
  expect_equal(block$response(c(4, 5, 6)),
    model$family$linkinv(as.numeric(X %*% c(4, 5, 6) + td$proj_offset_i)))
  expect_equal(block$response(c(4, 5, 6), subtract = z), block$response(c(4, 5, 6)))
  expect_equal(reports, 2L)
  block$response(c(4, 5, 7))
  block$terms(c(4, 5, 7))
  expect_equal(reports, 3L)
  expect_error(block$response(c(4, 5, 6), matrix(0, 1, 2)),
    "row-by-component")
})

test_that("domain terms normalize year and covariates without hiding interactions", {
  zero <- matrix(0, 2, 2)
  raw <- .domain_term_add(list(), "fixed:factor(survey_year)", c(1, 2),
    1, zero, "survey_year")
  raw <- .domain_term_add(raw, "fixed:survey_year", c(3, 4),
    2, zero, "survey_year")
  raw <- .domain_term_add(raw, "fixed:survey_year:depth", c(5, 6),
    1, zero, c("survey_year", "depth"))
  raw <- .domain_term_add(raw, "smooth:s(depth)", c(7, 8), 2, zero, "depth")
  normalized <- .domain_normalize_terms(raw, "survey_year")
  expect_named(normalized, c("year", "survey_year:depth", "s(depth)"))
  expect_equal(normalized$year, cbind(c(1, 2), c(3, 4)))
  expect_equal(attr(normalized, "variables")$year, "survey_year")
  expect_equal(normalized[["survey_year:depth"]], raw[["fixed:survey_year:depth"]])
  expect_equal(Reduce(`+`, normalized), Reduce(`+`, raw))
  raw <- .domain_term_add(raw, "svc:depth", c(1, 2), 1, zero, "depth")
  raw <- .domain_term_add(raw, "svc:temperature", c(3, 4), 1, zero, "temperature")
  normalized <- .domain_normalize_terms(raw, "survey_year")
  expect_equal(normalized[["svc:depth"]][, 1], c(1, 2))
  expect_equal(normalized[["svc:temperature"]][, 1], c(3, 4))
  expect_equal(attr(normalized, "variables")[["svc:depth"]], "depth")
  expect_equal(Reduce(`+`, normalized), Reduce(`+`, raw))
})

test_that("native transformed covariates remain separate scientific terms", {
  zero <- matrix(0, 2, 1)
  raw <- .domain_term_add(list(), "fixed:x", c(1, 2), 1, zero, "x")
  raw <- .domain_term_add(raw, "fixed:I(x^2)", c(3, 4), 1, zero, "x")
  raw <- .domain_term_add(raw, "smooth:s(x)", c(5, 6), 1, zero, "x")
  raw <- .domain_term_add(raw, "fixed:factor(region)", c(7, 8), 1, zero, "region")
  normalized <- .domain_normalize_terms(raw, "year")
  expect_named(normalized, c("x", "I(x^2)", "s(x)", "factor(region)"))
  expect_equal(normalized$x, raw[["fixed:x"]])
  expect_equal(normalized[["I(x^2)"]], raw[["fixed:I(x^2)"]])
  expect_equal(normalized[["s(x)"]], raw[["smooth:s(x)"]])
  expect_equal(Reduce(`+`, normalized), Reduce(`+`, raw))
})

test_that("decomposition rejects hidden residuals and misaligned components", {
  expect_error(.domain_check_terms(list(fixed = matrix(1, 2, 1)),
    matrix(1.01, 2, 1)), "do not sum")
  expect_error(.domain_check_terms(list(fixed = matrix(1, 1, 2)),
    matrix(1, 2, 1)), "row-by-component")
  expect_error(.domain_check_terms(list(matrix(1, 2, 1)),
    matrix(1, 2, 1)), "unique")
  expect_silent(.domain_check_terms(list(a = matrix(1, 2, 2),
    b = matrix(2, 2, 2)), matrix(3, 2, 2)))
})

test_that("tinyVAST baseline and masked responses share one native report", {
  model <- list(internal = list(family = list("poisson"), variables = "response"))
  env <- new.env(parent = emptyenv())
  env$data <- list(components_e = 1L, link_ez = matrix(1L, 1, 1))
  env$last.par <- c(1, 2)
  env$random <- integer()
  env$parList <- function(x, par) list(alpha_j = par)
  reports <- 0L
  obj <- list(env = env, report = function(par) {
    reports <<- reports + 1L
    list(p1_g = par, mu_g = exp(par))
  })
  adapter <- .domain_projection
  scope <- new.env(parent = environment(adapter))
  scope$.index_spatial_projection <- function(...) list(obj = obj,
    response = function(par) stop("Baseline response must reuse the cached report"))
  scope$.index_free_tmb <- function(...) stop("Unexpected objective disposal")
  environment(adapter) <- scope
  block <- adapter(model, "tinyVAST", data.frame(row = 1:2))
  expect_equal(block$eta(c(1, 2)), matrix(c(1, 2), 2, 1))
  expect_equal(block$response(c(1, 2)), exp(c(1, 2)))
  expect_equal(block$response(c(1, 2), subtract = matrix(0, 2, 1)), exp(c(1, 2)))
  expect_equal(reports, 1L)
})

test_that("delta response adapters retain native component semantics", {
  eta <- cbind(c(-2, 1), c(1, 2))
  standard <- list(family = list(stats::binomial(), stats::Gamma(link = "log"),
    delta = TRUE, type = "standard"))
  expect_equal(.domain_sdmtmb_response(standard, list(), list(), eta),
    stats::plogis(eta[, 1]) * exp(eta[, 2]))
  standard$family$type <- "poisson_link_delta"
  standard$family[[1L]] <- stats::binomial(link = "log")
  expect_equal(.domain_sdmtmb_response(standard, list(), list(), eta),
    exp(rowSums(eta)))
  truncated <- list(family = list(family = "truncated_nbinom2", link = "log",
    linkinv = function(eta) {
      mu <- exp(eta)
      mu / (1 - (2 / (2 + mu))^2)
    }))
  mu <- exp(eta[, 1])
  expect_equal(.domain_sdmtmb_response(truncated, list(), list(ln_phi = log(2)),
    eta[, 1, drop = FALSE]), mu / (1 - (2 / (2 + mu))^2))
  expect_equal(.domain_native_linkinv(eta[, 1], 3L), -expm1(-exp(eta[, 1])))
  custom <- list(family = list(family = "custom",
    linkinv = function(eta) exp(eta) + 2))
  expect_equal(.domain_sdmtmb_response(custom, list(), list(),
    eta[, 1, drop = FALSE]), exp(eta[, 1]) + 2)
})

test_that("tinyVAST components include offsets, SVCs and native temporal fields", {
  X <- stats::model.matrix(~ x, data.frame(x = c(2, 3)))
  assign <- attr(X, "assign")
  attr(X, "assign") <- NULL
  td <- list(X_gj = X, X2_gj = X, Z_gk = matrix(0, 2, 0),
    Z2_gk = matrix(0, 2, 0), W_gl = matrix(c(2, 3), 2, 1,
      dimnames = list(NULL, "depth")), W2_gl = matrix(0, 2, 0),
    A_gs = diag(2), offset_g = c(0.1, 0.2))
  model <- list(internal = list(gam_setup = list(pterms = stats::terms(~ x), assign = assign),
    delta_gam_setup = list(pterms = stats::terms(~ x), assign = assign)))
  p <- list(alpha_j = c(1, 2), alpha2_j = c(3, 4), gamma_k = numeric(),
    gamma2_k = numeric(), xi_sl = matrix(c(5, 6), 2, 1), xi2_sl = matrix(0, 2, 0))
  r <- list(log_tau = log(2))
  for (m in 1:2) {
    for (field in c("pomega", "pepsilon", "pdelta")) {
      r[[paste0(field, m, "_g")]] <- c(m, m + 1)
    }
  }
  out <- .domain_tinyvast_terms(model, td, p, r, matrix(0, 2, 2))
  expect_equal(out[["svc:depth"]], cbind(c(5, 9), c(0, 0)))
  expect_equal(out$offset, cbind(c(0.1, 0.2), c(0, 0)))
  expect_equal(out$temporal, cbind(c(1, 2), c(2, 3)))
  expect_equal(out[["fixed:x"]], cbind(c(4, 6), c(8, 12)))
})

test_that("sdmTMB smooth blocks and temporal coefficients stay draw dependent", {
  skip_if_not_installed("mgcv")
  d <- data.frame(depth = seq(0, 1, length.out = 12))
  b <- mgcv::smoothCon(mgcv::s(depth, k = 5), data = d,
    absorb.cons = TRUE, modCon = 3)[[1L]]
  design <- mgcv::smooth2random(b, names(d), type = 2)
  n <- nrow(d)
  td <- list(proj_X_ij = list(matrix(1, n, 1), matrix(1, n, 1)),
    proj_offset_i = rep(0.2, n), proj_z_i = matrix(0, n, 0),
    has_smooths = 1L, proj_Zs = design$rand, proj_Xs = design$Xf,
    b_smooth_start = c(0, head(cumsum(vapply(design$rand, ncol, 1L)), -1)),
    ar1_time = 1L, random_walk = 0L,
    proj_X_rw_ik = matrix(d$depth, n, 1, dimnames = list(NULL, "depth")),
    proj_year = rep(0:1, n / 2), threshold_func = 0L)
  model <- list(data = d, terms = list(stats::terms(~ 1), stats::terms(~ 1)),
    family = list(stats::binomial(), stats::Gamma(link = "log"), delta = TRUE),
    smoothers = list(basis_out = list(list(b)), labels = list(b$label)))
  p <- list(b_j = 1, b_j2 = 2,
    b_smooth = matrix(0.1, sum(vapply(design$rand, ncol, 1L)), 2),
    bs = matrix(0.3, ncol(design$Xf), 2),
    b_rw_t = array(1:4, c(2, 1, 2)))
  r <- list(proj_omega_s_A = matrix(0, n, 2),
    proj_epsilon_st_A_vec = matrix(0, n, 2))
  out <- .domain_sdmtmb_terms(model, td, p, r, matrix(0, n, 2))
  expected <- as.numeric(design$Xf %*% p$bs[, 1])
  for (Z in design$rand) expected <- expected + as.numeric(Z %*% rep(0.1, ncol(Z)))
  expect_equal(out[[paste0("smooth:", b$label)]], unname(cbind(expected, expected)))
  expect_equal(out[["temporal:depth"]][, 2], d$depth * rep(3:4, n / 2))
  component_model <- model
  component_model$smoothers <- list(model$smoothers, model$smoothers)
  component_out <- .domain_sdmtmb_terms(component_model, td, p, r, matrix(0, n, 2))
  expect_equal(component_out, out)
  component_p <- p
  component_p$b_smooth[, 2] <- component_p$b_smooth[, 2] * 3
  component_changed <- .domain_sdmtmb_terms(component_model, td,
    component_p, r, matrix(0, n, 2))
  expect_equal(component_changed[[paste0("smooth:", b$label)]][, 1],
    out[[paste0("smooth:", b$label)]][, 1])
  expect_false(isTRUE(all.equal(component_changed[[paste0("smooth:", b$label)]][, 2],
    out[[paste0("smooth:", b$label)]][, 2])))
  single_model <- model
  single_model$family <- stats::poisson()
  single_model$smoothers <- list(model$smoothers)
  single_td <- td
  single_td$proj_X_ij <- td$proj_X_ij[1]
  single_r <- lapply(r, function(x) x[, 1, drop = FALSE])
  single_out <- .domain_sdmtmb_terms(single_model, single_td, p, single_r,
    matrix(0, n, 1))
  expect_equal(single_out[[paste0("smooth:", b$label)]][, 1], expected)
  single_model$smoothers <- list(list())
  expect_error(.domain_sdmtmb_terms(single_model, single_td, p, single_r,
    matrix(0, n, 1)), "smoother metadata")
  p$b_smooth <- p$b_smooth * 2
  changed <- .domain_sdmtmb_terms(model, td, p, r, matrix(0, n, 2))
  expect_false(isTRUE(all.equal(changed[[paste0("smooth:", b$label)]],
    out[[paste0("smooth:", b$label)]])))
})
