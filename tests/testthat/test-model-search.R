search_test_data <- function() {
  set.seed(421)
  d <- data.frame(year = factor(rep(2018:2021, each = 45)),
    x = rnorm(180), z = rnorm(180), exposure = runif(180, 1, 3),
    vessel = factor(rep(1:12, 15)))
  d$y <- rpois(180, d$exposure * exp(0.7 + 0.8 * d$x))
  d
}

test_that("search preflight does not fit, score, simulate, or retain the source", {
  d <- search_test_data()
  m <- glm(y ~ year + x + z + offset(log(exposure)), d, family = poisson())
  local_mocked_bindings(.search_refit_one = function(...) stop("No fitting"),
    .search_score = function(...) stop("No scoring"))
  out <- search_cpue(m)
  expect_s3_class(out, "influ_search")
  expect_identical(out$metadata$stop_reason, "preflight_only")
  expect_equal(nrow(out$candidates), 0)
  expect_equal(nrow(out$path), 0)
  expect_null(out$best_model)
  expect_true(all(out$terms$protected[out$terms$term %in% c("year", "offset(log(exposure))")]))
  expect_error(selected_model(out), "preflight")
  expect_error(plot(out), "No evaluated")
  expect_output(print(out), "preflight only")
  expect_named(summary(out), c("metadata", "terms", "candidates", "path"))
  expect_identical(as.data.frame(out), out$candidates)
})

test_that("backward GLM scores are native and comparable on exactly fitted rows", {
  d <- search_test_data()
  d$x[3] <- NA
  m <- glm(y ~ year + x + z + offset(log(exposure)), d, family = poisson(), na.action = na.omit)
  out <- search_cpue(m, direction = "backward", refit = TRUE, keep_fits = "all")
  expect_true(all(out$candidates$eligible))
  expect_equal(anyDuplicated(out$candidates$key), 0)
  expect_equal(out$candidates$AIC, out$candidates$score)
  expect_equal(out$candidates$nobs, rep(nobs(m), nrow(out$candidates)))
  expect_identical(out$candidates$refitted[1], FALSE)
  expect_true(all(out$path$improvement[-1] > 1e-6))
  best <- selected_model(out)
  expect_s3_class(best, "glm")
  expect_equal(AIC(best), min(out$candidates$score))
  expect_true("year" %in% attr(terms(best), "term.labels"))
  expect_equal(model.offset(model.frame(best)), model.offset(model.frame(m)))
  expect_identical(row.names(model.frame(best)), row.names(model.frame(m)))
  expect_equal(length(out$fits), nrow(out$candidates))
  expect_s3_class(plot(out), "ggplot")
  expect_s3_class(ggplot2::autoplot(out), "ggplot")
  expect_output(print(out), "Best evaluated")
})

test_that("ordering, forward, and bidirectional paths obey hierarchy and cache candidates", {
  d <- search_test_data()
  m <- glm(y ~ year + x * z + offset(log(exposure)), d, family = poisson())
  for (direction in list("forward", "both", c("order", "backward"))) {
    out <- search_cpue(m, direction = direction, criterion = "BIC", refit = TRUE)
    spec <- .search_prepare_spec(m)
    expect_true(all(vapply(strsplit(out$candidates$key, "+", fixed = TRUE),
      function(ids) .search_legal(spec, ids), logical(1))))
    expect_equal(anyDuplicated(out$candidates$key), 0)
    expect_equal(out$candidates$BIC, out$candidates$score)
    expect_s3_class(selected_model(out), "glm")
    expect_equal(BIC(selected_model(out)), min(out$candidates$score))
    if (length(direction) == 2) expect_true(any(out$path$phase == "order"))
  }
})

test_that("grouped moves and protected groups are honoured", {
  d <- search_test_data()
  m <- glm(y ~ year + x + z, d, family = poisson())
  out <- search_cpue(m, direction = "forward", blocks = list(covariates = c("x", "z")), refit = TRUE)
  expect_equal(nrow(out$candidates), 2)
  expect_equal(out$candidates$n_terms, c(1, 3))
  expect_true(all(out$path$terms[-1] == "x + z"))
  all_protected <- search_cpue(m, blocks = list(core = c("year", "x", "z")), refit = TRUE)
  expect_equal(nrow(all_protected$candidates), 1)
  expect_true(all(all_protected$terms$protected))
})

test_that("budgets, retention, and method errors are explicit", {
  d <- search_test_data()
  m <- glm(y ~ year + x + z, d, family = poisson())
  out <- search_cpue(m, max_models = 1, refit = TRUE, keep_fits = "none")
  expect_equal(nrow(out$candidates), 1)
  expect_identical(out$metadata$stop_reason, "max_models_reached")
  expect_null(out$best_model)
  expect_error(selected_model(out), "not retained")
  expect_error(selected_model(m), "influ_search")
  for (value in list(0, -1, NA_real_, 1.5, Inf)) expect_error(search_cpue(m, max_models = value), "positive integer")
  expect_error(search_cpue(m, refit = NA), "refit")
  expect_error(search_cpue(m, direction = "step"), "direction")
  expect_error(search_cpue(m, keep_fits = "some"), "keep_fits")
  expect_error(search_cpue(m, seed = -1), "seed")
  expect_error(search_cpue(m, refit_args = list(seed = 1)), "via.*seed")
  expect_error(search_cpue(m, criterion = "deviance"), "criterion")
  expect_error(search_cpue(m, criterion = "loo"), "brms searches")
  expect_identical(.search_metric("auto", "brmsfit"), "looic")
  expect_identical(.search_metric("LOOIC", "brmsfit"), "looic")
  expect_error(.search_metric("AIC", "brmsfit"), "brms searches")
  expect_error(.search_metric("cAIC", "merMod"), "No supported native")
  expect_identical(.search_metric("auto", "glm"), "AIC")
  expect_error(search_cpue(lm(y ~ year, d)), "not an lm")
  local_mocked_bindings(.search_package_available = function(...) FALSE)
  expect_error(search_cpue(m), "Package 'stats' is required")
})

test_that("search preserves caller RNG, including failures and absent state", {
  d <- search_test_data()
  m <- glm(y ~ year + x + z, d, family = poisson())
  set.seed(782)
  saved <- .Random.seed
  a <- search_cpue(m, refit = TRUE, seed = 4)
  expect_identical(.Random.seed, saved)
  b <- search_cpue(m, refit = TRUE, seed = 4)
  expect_equal(a$candidates$score, b$candidates$score)
  expect_equal(a$candidates$seed, seq(4, length.out = nrow(a$candidates)))
  expect_identical(.Random.seed, saved)
  expect_error(search_cpue(m, scope = ~ non_existent, refit = TRUE, seed = 4))
  expect_identical(.Random.seed, saved)
  withr::local_preserve_seed()
  rm(".Random.seed", envir = .GlobalEnv)
  search_cpue(m, refit = TRUE, seed = 12)
  expect_false(exists(".Random.seed", .GlobalEnv, inherits = FALSE))
})

test_that("native failures remain cached and protected-core failure stops safely", {
  d <- search_test_data()
  m <- glm(y ~ year + x + z, d, family = poisson())
  original <- .search_refit_one
  count <- 0L
  local_mocked_bindings(.search_refit_one = function(model, formula, args, locked, backend) {
    count <<- count + 1L
    labels <- attr(terms(formula), "term.labels")
    if ("z" %in% labels && !"x" %in% labels) stop("deliberate failed candidate")
    original(model, formula, args, locked, backend)
  })
  out <- search_cpue(m, refit = TRUE)
  expect_true(any(grepl("deliberate failed", out$candidates$detail)))
  expect_equal(count, sum(out$candidates$refitted))
  expect_equal(anyDuplicated(out$candidates$key), 0)
  expect_true(all(is.na(out$candidates$score[!out$candidates$eligible])))
  local_mocked_bindings(.search_fit_status = function(...) list(eligible = FALSE,
    detail = "No numerical evidence", flags = "missing"))
  bad <- search_cpue(m, refit = TRUE)
  expect_identical(bad$metadata$stop_reason, "protected_core_ineligible")
  expect_equal(nrow(bad$candidates), 1)
  expect_error(selected_model(bad), "No eligible")
})

test_that("scope reductions never falsely reuse an original larger model", {
  d <- search_test_data()
  m <- glm(y ~ year + x + z, d, family = poisson())
  out <- search_cpue(m, scope = ~ year + x, direction = "backward", refit = TRUE)
  expect_true(all(out$candidates$refitted))
  expect_false(any(grepl("z", out$candidates$formula)))
  expect_equal(out$candidates$score[1], AIC(glm(y ~ year + x, d, family = poisson())))
})

test_that("scope can add unused raw covariates and controls refit original candidates", {
  d <- search_test_data()
  m <- glm(y ~ year + x, d, family = poisson())
  added <- search_cpue(m, scope = ~ . + z, direction = "backward", refit = TRUE)
  expect_true(grepl("z", added$candidates$formula[1]))
  expect_true(added$candidates$refitted[1])
  expect_equal(added$candidates$score[1], AIC(glm(y ~ year + x + z, d, family = poisson())))
  controlled <- search_cpue(m, direction = "backward", refit = TRUE,
    refit_args = list(control = glm.control(maxit = 60)))
  expect_true(all(controlled$candidates$refitted))
  expect_equal(controlled$candidates$score[1], AIC(m))
})

test_that("ordering may worsen criterion and recommendation can differ from path endpoint", {
  d <- search_test_data()
  m <- glm(y ~ year + x + z, d, family = poisson())
  original <- .search_score
  local_mocked_bindings(.search_score = function(fit, metric, reference = NULL) {
    value <- original(fit, metric, reference)
    labels <- attr(terms(fit), "term.labels")
    value$score <- if (all(c("x", "z") %in% labels)) 5 else if ("x" %in% labels) 10 else if ("z" %in% labels) 11 else 0
    value$statistics$AIC <- value$score
    value
  })
  out <- search_cpue(m, refit = TRUE)
  expect_true(any(out$path$improvement < 0, na.rm = TRUE))
  expect_false(out$metadata$best_candidate == out$metadata$final_path_candidate)
  expect_equal(attr(terms(selected_model(out)), "term.labels"), "year")
  expect_equal(nrow(out$candidates), 4)
  expect_equal(anyDuplicated(out$candidates$key), 0)
})

test_that("incompatible likelihood data cannot be recommended", {
  d <- search_test_data()
  m <- glm(y ~ year + x + z, d, family = poisson())
  score <- .search_score(m, "AIC")
  changed <- d; changed$y[1] <- changed$y[1] + 10
  b <- glm(y ~ year + x, changed, family = poisson())
  invalid <- .search_score(b, "AIC", score$reference)
  expect_false(invalid$eligible)
  expect_match(invalid$detail, "cannot be verified")
  q <- glm(y ~ year + x, d, family = quasipoisson())
  expect_false(.search_score(q, "AIC")$eligible)
})

test_that("GAM search keeps native smooth blocks and random structure", {
  skip_if_not_installed("mgcv")
  d <- search_test_data()
  m <- mgcv::gam(y ~ year + s(x, k = 5) + z + s(vessel, bs = "re"),
    data = d, family = poisson(), method = "REML")
  out <- search_cpue(m, refit = TRUE, direction = "backward", criterion = "cAIC")
  expect_true(any(grepl("s\\(vessel", out$terms$term[out$terms$protected])))
  expect_true(all(out$candidates$eligible))
  expect_equal(out$candidates$cAIC, out$candidates$score)
  expect_equal(AIC(selected_model(out)), min(out$candidates$score))
  expect_match(paste(out$metadata$notes, collapse = " "), "conditional AIC")
})

test_that("glmmTMB searches protect essentials and permit nominated random terms", {
  skip_if_not_installed("glmmTMB")
  d <- search_test_data()
  set.seed(126)
  d$y <- rpois(nrow(d), d$exposure * exp(0.7 + 0.8 * d$x + rep(rnorm(12, 0, 0.6), 15)))
  m <- glmmTMB::glmmTMB(y ~ year + x + z + (1 | vessel) + offset(log(exposure)),
    data = d, family = poisson())
  out <- search_cpue(m, direction = "backward", refit = TRUE, criterion = "BIC")
  expect_true(all(out$candidates$eligible))
  expect_true(all(grepl("vessel", out$candidates$formula)))
  expect_equal(BIC(selected_model(out)), min(out$candidates$score))
  expect_error(search_cpue(m, criterion = "cAIC"), "No supported native")
  optional_random <- search_cpue(m, optional = "1 | vessel", direction = "backward", refit = TRUE)
  expect_equal(nrow(optional_random$candidates), 2)
  expect_true(all(grepl("x", optional_random$candidates$formula)))
  expect_true(all(grepl("z", optional_random$candidates$formula)))
  expect_false(any(optional_random$terms$protected[optional_random$terms$type == "random"]))
})

test_that("lme4 search converts REML baselines and preserves original", {
  skip_if_not_installed("lme4")
  d <- search_test_data()
  set.seed(45)
  d$y <- 2 + d$x + rep(rnorm(12, 0, 0.5), 15) + rnorm(nrow(d))
  m <- lme4::lmer(y ~ year + x + z + (1 | vessel), data = d, REML = TRUE)
  out <- search_cpue(m, direction = "backward", refit = TRUE)
  expect_true(all(out$candidates$refitted))
  expect_true(all(out$candidates$eligible))
  expect_match(paste(out$metadata$notes, collapse = " "), "refitted with ML")
  expect_false(lme4::isREML(selected_model(out)))
  expect_true(lme4::isREML(m))
  expect_equal(AIC(selected_model(out)), min(out$candidates$score))
})

test_that("sdmTMB search is component-aware and scores native fits", {
  skip_if_not_installed("sdmTMB")
  d <- search_test_data()
  set.seed(634)
  d$X <- runif(nrow(d)); d$Y <- runif(nrow(d))
  mesh <- sdmTMB::make_mesh(d, c("X", "Y"), n_knots = 8)
  m <- sdmTMB::sdmTMB(y ~ year + x + z, data = d, mesh = mesh,
    family = poisson(), spatial = "off", spatiotemporal = "off")
  out <- search_cpue(m, direction = "backward", refit = TRUE)
  expect_true(all(out$candidates$eligible))
  expect_equal(AIC(selected_model(out)), min(out$candidates$score))
  expect_identical(selected_model(out)$spde, m$spde)
  d$delta <- exp(1 + d$x + rnorm(nrow(d), sd = .4)) * rbinom(nrow(d), 1, .75)
  joint <- sdmTMB::sdmTMB(list(delta ~ year + x, delta ~ year + x + z),
    data = d, mesh = mesh, family = sdmTMB::delta_lognormal(),
    spatial = "off", spatiotemporal = "off")
  expect_error(search_cpue(joint), "component")
  out <- search_cpue(joint, component = "positive", optional = "z",
    direction = "backward", refit = TRUE, keep_fits = "all")
  expect_equal(nrow(out$candidates), 2)
  expect_true(all(out$candidates$eligible))
  for (fit in out$fits) expect_identical(fit$formula[[1]], joint$formula[[1]])
  expect_equal(AIC(selected_model(out)), min(out$candidates$score))
})

test_that("tinyVAST search uses native weighted likelihood and keeps process configuration", {
  skip_if_not_installed("tinyVAST")
  d <- search_test_data(); d$w <- rep(c(1, 2), length.out = nrow(d))
  m <- tinyVAST::tinyVAST(y ~ year + x + z, data = d, spatial_domain = NULL,
    family = poisson(), weights = d$w,
    control = tinyVAST::tinyVASTcontrol(calculate_deviance_explained = FALSE))
  out <- search_cpue(m, direction = "backward", refit = TRUE)
  expect_true(all(out$candidates$eligible))
  expect_equal(AIC(selected_model(out)), min(out$candidates$score))
  expect_identical(selected_model(out)$spatial_domain, m$spatial_domain)
  expect_equal(selected_model(out)$tmb_inputs$tmb_data$weights_i, m$tmb_inputs$tmb_data$weights_i)
  expect_false(selected_model(out)$internal$control$calculate_deviance_explained)
})
