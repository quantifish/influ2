test_that("new brms joint families reproduce native log likelihoods and means", {
  skip_if_not_installed("brms")
  skip_if_not_installed("rstan")
  for (name in c("hurdle_gamma", "hurdle_poisson", "hurdle_negbinomial",
      "zero_inflated_poisson", "zero_inflated_negbinomial")) {
    m <- readRDS(test_path("fixtures", "brms-implied", paste0(name, ".rds")))
    for (id in list(NULL, 17L)) {
      a <- .implied_adapter(m, NULL, "year", "area", "year_group", "combined", draw_id = id)
      prep <- brms::prepare_predictions(m, point_estimate = if (is.null(id)) "mean" else NULL, draw_ids = id)
      gate <- if (a$joint_kind == "hurdle") "hu" else "zi"
      for (theta in list(c(0, 0), c(.2, -.15), c(-.3, .25))) {
        native <- prep
        p <- brms::get_dpar(prep, gate)
        native$dpars[[gate]] <- plogis(qlogis(p) - theta[1L])
        native$dpars$mu <- brms::get_dpar(prep, "mu") * exp(theta[2L])
        expect_equal(.implied_joint_loglik(theta, a$observed, a$eta, a$dispersion,
          a$positive_family, a$joint_kind, a$extra), sum(brms::log_lik(native, cores = 1)), tolerance = 1e-9)
      }
      z <- implied_effects(m, year = "year", groups = "area", component = "combined", draw_id = id)
      means <- as.numeric(brms::posterior_epred(m, point_estimate = if (is.null(id)) "mean" else NULL, draw_ids = id))
      expect_equal(z$table$baseline, as.numeric(tapply(means, interaction(m$data$year, m$data$area), mean)), tolerance = 1e-8)
      expect_equal(z$metadata$reference, if (is.null(id)) "posterior_mean_parameters" else "joint_posterior_draw")
      expect_true(any(z$table$status == "ok"))
      expect_lt(as.numeric(object.size(z)), 30000)
    }
    parts <- if (startsWith(name, "hurdle")) c("encounter", "positive") else c("conditional", "zero_inflation")
    for (part in parts) {
      z <- implied_effects(m, year = "year", groups = "area", component = part)
      expect_s3_class(ggplot2::ggplot_build(plot(z)), "ggplot_built")
      if (part == "zero_inflation") expect_match(plot(z)$labels$caption, "not observed encounter")
    }
    if (startsWith(name, "zero_inflated")) {
      z <- implied_effects(m, year = "year", groups = "area", component = "positive")
      expect_match(plot(z)$labels$caption, "including zeros")
      expect_false(grepl("Positive observations only", plot(z)$labels$caption, fixed = TRUE))
    }
  }
})

test_that("a verified native custom brms Tweedie uses fixed phi and power", {
  skip_if_not_installed("brms")
  skip_if_not_installed("rstan")
  skip_if_not_installed("mgcv")
  m <- readRDS(test_path("fixtures", "brms-implied", "tweedie.rds"))
  for (id in list(NULL, 17L)) {
    a <- .implied_adapter(m, NULL, "year", "area", "year_group", draw_id = id)
    z <- implied_effects(m, year = "year", groups = "area", draw_id = id)
    p <- brms::prepare_predictions(m, point_estimate = if (is.null(id)) "mean" else NULL, draw_ids = id)
    expect_equal(.implied_loglik(0, a$observed, a$eta, a$dispersion, "tweedie", a$extra), sum(brms::log_lik(p, cores = 1)), tolerance = 1e-8)
    expect_equal(z$metadata$power, 1.5)
    expect_true(all(z$table$status == "ok"))
  }
  bad <- m
  bad$family$log_lik <- function(i, prep) rep(-1, prep$ndraws)
  bad$formula$family <- bad$family
  expect_error(implied_effects(bad, year = "year", groups = "area"), "does not agree")
  bad <- m; bad$family$name <- "unrelated"
  expect_error(implied_effects(bad, year = "year", groups = "area"), "Supported native")
})
