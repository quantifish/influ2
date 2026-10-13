check_term_data <- function(n = 60L) {
  x <- seq(-2, 2, length.out = n)
  data.frame(y = rep(c(0, 1, 2, 3, 4), length.out = n), x = x,
    year = factor(rep(2018:2020, length.out = n)),
    group = factor(rep(LETTERS[1:3], each = n / 3)),
    effort = rep(c(1, 2), length.out = n),
    row.names = paste0("operation-", seq_len(n)))
}

check_term_residuals <- function(data, pit = seq(0.01, 0.99, length.out = nrow(data)),
    component = "single") {
  structure(list(observations = data.frame(row = rownames(data),
    observed = data$y, pit = pit, residual = qnorm(pit)),
    metadata = list(component = component, conditioning = "fitted",
      residual_method = "simulation_pit")), class = "influ_residuals")
}

test_that("term inventory retains transformations, interactions, offsets, and raw variables", {
  d <- check_term_data()
  f <- glm(y ~ year + poly(x, 2) + group + x:group + offset(log(effort)),
    data = d, family = poisson())
  x <- .check_term_inventory(f)
  expect_identical(x$backend, "glm")
  expect_setequal(x$terms$term, c("year", "poly(x, 2)", "group", "group:x", "offset(log(effort))"))
  expect_identical(x$terms$variables[[match("poly(x, 2)", x$terms$term)]], "x")
  expect_setequal(x$terms$variables[[match("group:x", x$terms$term)]], c("x", "group"))
  expect_identical(x$terms$term_type[match("group:x", x$terms$term)], "interaction")
  expect_identical(x$terms$term_type[match("offset(log(effort))", x$terms$term)], "offset")
  expect_identical(x$data$x, d$x)
  expect_identical(rownames(x$data), rownames(d))
  expect_identical(x$response_variables, "y")
})

test_that("term screens keep rank effect sizes and Holm-adjust nominal p-values", {
  d <- check_term_data()
  f <- glm(y ~ year + poly(x, 2) + group + x:group, data = d, family = poisson())
  r <- check_term_residuals(d)
  old <- list(coefficients = f$coefficients, model = f$model,
    fitted = f$fitted.values, terms = attr(f$terms, "term.labels"))
  set.seed(713)
  before <- .Random.seed
  x <- .check_terms(f, r)
  expect_identical(.Random.seed, before)
  expect_identical(list(coefficients = f$coefficients, model = f$model,
    fitted = f$fitted.values, terms = attr(f$terms, "term.labels")), old)
  numeric <- x[x$check == "rank_association", ]
  expect_equal(numeric$statistic, 1)
  expect_match(numeric$method, "statistic is rho")
  expect_match(numeric$message, "not a test of the term")
  cat <- x[x$check == "group_location", ]
  expect_equal(cat$df, rep(2, 2))
  expect_true(all(grepl("not DHARMa", cat$method)))
  expect_true(all(grepl("Kruskal-Wallis", cat$method)))
  computed <- is.finite(x$p_value)
  expect_equal(x$p_adjusted[computed], p.adjust(x$p_value[computed], "holm"))
  expect_true(all(x$status[computed] %in% c("review", "no_flag")))
  expect_identical(x$status[x$term == "group:x"], "not_assessed")
  expect_match(x$message[x$term == "group:x"], "interaction")
  expect_true(all(x$component == "conditional"))
  expect_true(all(grepl("Residual target: single response", x$message)))
  expect_true(all(x$conditioning == "fitted"))
})

test_that("residual IDs align NA omissions, reordered rows, and positive-only components", {
  d <- check_term_data()
  d$year[4] <- NA
  f <- glm(y ~ year + x, data = d, family = poisson(), na.action = na.exclude)
  fitted <- d[-4, ]
  selected <- fitted[rev(which(fitted$y > 0)), ]
  pit <- seq(0.05, 0.95, length.out = nrow(selected))
  r <- check_term_residuals(selected, pit, component = "positive")
  x <- .check_terms(f, r, data = d)
  rho <- cor(selected$x, pit, method = "spearman")
  expect_equal(x$statistic[x$check == "rank_association"], rho)
  expect_true(all(x$n == nrow(selected)))
  r$observations$row[1] <- "not-a-fitted-row"
  expect_error(.check_terms(f, r, data = d), "cannot be aligned safely")
  r <- check_term_residuals(selected, pit, component = "positive")
  r$observations$row[2] <- r$observations$row[1]
  expect_error(.check_terms(f, r, data = d), "cannot be aligned safely")
  r <- check_term_residuals(selected, pit, component = "positive")
  r$observations$pit[1] <- 1.2
  expect_error(.check_terms(f, r, data = d), "uniform-scale")
  changed <- d
  changed$x[5] <- 999
  expect_error(.check_terms(f, check_term_residuals(selected, pit), data = changed), "does not match|does not reproduce")
})

test_that("unsafe, sparse, unavailable, repeated, and response-defined term screens are explicit", {
  d <- check_term_data()
  d$singleton <- factor(seq_len(nrow(d)))
  f <- glm(y ~ x + I(x^2) + singleton + I(y > 0), data = d, family = poisson())
  x <- .check_terms(f, check_term_residuals(d))
  expect_identical(x$status[x$term == "I(x^2)"], "not_assessed")
  expect_match(x$message[x$term == "I(x^2)"], "already been screened")
  expect_identical(x$status[x$term == "singleton"], "not_assessed")
  expect_identical(x$status[x$term == "I(y > 0)"], "not_assessed")
  expect_match(x$message[x$term == "I(y > 0)"], "response variable")
  expect_error(.check_terms(f, NULL, terms = "not_in_model"), "Unknown model terms")
  expect_error(.check_terms(f, NULL, terms = c("x", "x")), "distinct")
  expect_error(.check_terms(f, NULL, alpha = 0), "alpha")
  expect_true(all(.check_terms(f, NULL)$status == "not_assessed"))
  isolated <- local({
    raw <- d
    z <- glm(y ~ poly(x, 2) + year, data = raw, family = poisson())
    rm(raw)
    z
  })
  no_raw <- .check_terms(isolated, check_term_residuals(d))
  expect_identical(no_raw$status[no_raw$term == "poly(x, 2)"], "not_assessed")
  expect_match(no_raw$message[no_raw$term == "poly(x, 2)"], "raw predictor x is unavailable")
})

test_that("native GAM smooth summaries are distinct from residual adequacy tests", {
  skip_if_not_installed("mgcv")
  d <- check_term_data()
  f <- mgcv::gam(y ~ year + s(x, k = 5), data = d)
  native <- summary(f)$s.table
  inventory <- .check_term_inventory(f)
  expect_identical(inventory$terms$term_type[inventory$terms$term == "s(x, k = 5)"], "smooth")
  x <- .check_terms(f, check_term_residuals(d))
  smooth <- x[x$check == "smooth_summary", ]
  expect_equal(smooth$df, as.numeric(native[, "edf"]))
  expect_equal(smooth$p_value, as.numeric(native[, ncol(native)]))
  expect_true(is.na(smooth$p_adjusted))
  expect_identical(smooth$status, "not_applicable")
  expect_match(smooth$message, "not residual adequacy")
  no_residual <- .check_terms(f, NULL)
  expect_true(any(no_residual$check == "smooth_summary"))
  expect_true(all(no_residual$status[no_residual$check == "residual_structure"] == "not_assessed"))
})

test_that("random intercept and slope inventories do not flatten the slope basis", {
  skip_if_not_installed("lme4")
  d <- check_term_data()
  f <- suppressWarnings(lme4::lmer(y ~ x + (1 | group), data = d))
  inventory <- .check_term_inventory(f)
  expect_identical(inventory$backend, "lme4")
  expect_true(any(inventory$terms$term == "(1 | group)"))
  x <- .check_terms(f, check_term_residuals(d))
  group <- x[x$term == "(1 | group)", ]
  expect_identical(group$check, "group_location")
  expect_match(group$message, "not.*random-effect variance")
  f2 <- suppressWarnings(lme4::lmer(y ~ x + (x | group), data = d))
  x2 <- .check_terms(f2, check_term_residuals(d))
  expect_identical(x2$status[x2$term == "(x | group)"], "not_assessed")
  expect_match(x2$message[x2$term == "(x | group)"], "multiple predictors")
})

test_that("glmmTMB auxiliary component terms are not relabelled combined diagnostics", {
  skip_if_not_installed("glmmTMB")
  d <- check_term_data()
  f <- glmmTMB::glmmTMB(y ~ year + x, ziformula = ~group,
    data = d, family = poisson())
  inventory <- .check_term_inventory(f)
  expect_true(any(inventory$terms$component == "zero_inflation"))
  x <- .check_terms(f, check_term_residuals(d, component = "combined"))
  group <- x[x$term == "group", ]
  expect_identical(group$status, "not_assessed")
  expect_identical(group$component, "zero_inflation")
  expect_match(group$message, "not this zero_inflation component")
})

test_that("spatial and brms process inventories retain explicit unsupported checks", {
  d <- check_term_data()
  spatial <- structure(list(formula = list(y ~ year + x), data = d,
    family = list(delta = FALSE), spatial = "on", spatiotemporal = "AR1",
    time = "year", time_varying = ~x), class = "sdmTMB")
  inventory <- .check_term_inventory(spatial)
  expect_setequal(inventory$terms$term, c("year", "x", "spatial_field",
    "spatiotemporal_field", "time_varying_effects"))
  x <- .check_terms(spatial, check_term_residuals(d))
  process <- grepl("field|effects", x$term)
  expect_true(all(x$status[process] == "not_assessed"))
  expect_true(all(grepl("component-aware", x$message[process])))
  tiny <- structure(list(formula = y ~ year + s(x, k = 5), data = d,
    internal = list(family = list(poisson = list(delta = FALSE)),
      space_term = "density <-> density, sd", spacetime_term = "density <-> density, 0, sd",
      time_term = "density -> density, 1, rho", time_column = "year", space_columns = c("x", "z"))),
    class = "tinyVAST")
  inventory <- .check_term_inventory(tiny)
  expect_setequal(inventory$terms$term, c("year", "s(x, k = 5)", "spatial_field",
    "spatiotemporal_field", "temporal_process"))
  tiny$internal$family[[1L]]$delta <- TRUE
  tiny$internal$delta_gam_setup <- list(pterms = terms(~x))
  inventory <- .check_term_inventory(tiny)
  expect_true(all(c("encounter", "positive") %in% inventory$terms$component))
  bayes <- structure(list(formula = list(formula = y ~ x + (1 | group),
    pforms = list(hu = ~year)), data = d), class = "brmsfit")
  inventory <- .check_term_inventory(bayes)
  expect_true(all(c("mu", "hu") %in% inventory$terms$component))
  expect_true(any(inventory$terms$term == "(1 | group)"))
  x <- .check_terms(bayes, check_term_residuals(d, component = "combined"))
  expect_identical(x$status[x$component == "hu"], "not_assessed")
})

test_that("numeric codes explicitly fitted as factor or random groups use categorical screens", {
  d <- check_term_data()
  d$code <- rep(1:3, each = 20)
  f <- glm(y ~ factor(code) + x, data = d, family = poisson())
  x <- .check_terms(f, check_term_residuals(d))
  expect_identical(x$check[x$term == "factor(code)"], "group_location")
  expect_equal(x$df[x$term == "factor(code)"], 2)
})

test_that("native sdmTMB and tinyVAST term screens require no simulation or refitting", {
  skip_if_not_installed("sdmTMB")
  skip_if_not_installed("tinyVAST")
  d <- check_term_data()
  d$time <- as.integer(d$year)
  d$var <- "density"
  d$dist <- "poisson"
  models <- list(
    sdmTMB = sdmTMB::sdmTMB(y ~ year + x, data = d, family = poisson(),
      spatial = "off", spatiotemporal = "off", silent = TRUE),
    tinyVAST = tinyVAST::tinyVAST(y ~ year + x, data = d,
      family = list(poisson = poisson()), spatial_domain = NULL))
  for (f in models) {
    native_obj <- if (inherits(f, "sdmTMB")) f$tmb_obj else f$obj
    before <- native_obj$env$last.par.best
    rng <- .Random.seed
    x <- .check_terms(f, check_term_residuals(d))
    expect_setequal(x$term, c("year", "x"))
    expect_setequal(x$check, c("rank_association", "group_location"))
    expect_identical(native_obj$env$last.par.best, before)
    expect_identical(.Random.seed, rng)
    expect_equal(nrow(x), 2L)
  }
})

test_that("correlation wrappers and intercept-only fits do not invent predictor screens", {
  expression <- quote(year + ar1(time + 0 | group) + us(1 | vessel))
  bars <- .check_term_random_calls(expression)
  expect_identical(bars, list(quote(ar1(time + 0 | group)), quote(us(1 | vessel))))
  expect_identical(.check_term_without_random(expression), quote(year + 1 + 1))
  d <- check_term_data()
  f <- glm(y ~ 1, data = d, family = poisson())
  x <- .check_terms(f, check_term_residuals(d))
  expect_identical(x$term, "(Intercept)")
  expect_identical(x$status, "not_applicable")
  expect_true(is.na(x$p_value))
  empty <- .check_terms(f, NULL, terms = character())
  expect_s3_class(empty, "data.frame")
  expect_identical(nrow(empty), 0L)
  expect_identical(attr(empty, "term_inventory")$term, character())
})

test_that("sdmTMB and tinyVAST process inventories follow their actual delta components", {
  d <- check_term_data()
  spatial <- structure(list(formula = list(y ~ year, y ~ x), data = d,
    family = list(delta = TRUE), spatial = c("on", "off"),
    spatiotemporal = c("off", "iid"), time = "year"), class = "sdmTMB")
  inventory <- .check_term_inventory(spatial)$terms
  expect_identical(inventory$component[inventory$term == "spatial_field"], "encounter")
  expect_identical(inventory$component[inventory$term == "spatiotemporal_field"], "positive")
  tiny <- structure(list(formula = y ~ year, data = d,
    internal = list(family = list(delta = list(delta = TRUE)), delta_formula = ~x,
      space_term = "density <-> density, sd", delta_spacetime_term = "density <-> density, 0, sd",
      delta_time_term = "density -> density, 1, rho", delta_spatial_varying = ~x,
      time_column = "year", space_columns = c("x", "z"))), class = "tinyVAST")
  inventory <- .check_term_inventory(tiny)$terms
  expect_identical(inventory$component[inventory$term == "spatial_field"], "encounter")
  expect_identical(inventory$component[inventory$term == "spatiotemporal_field"], "positive")
  expect_identical(inventory$component[inventory$term == "temporal_process"], "positive")
  expect_identical(inventory$component[inventory$term == "spatially_varying_effects"], "positive")
  expect_identical(inventory$variables[[which(inventory$term == "spatially_varying_effects")]], "x")
  tiny$internal$delta_spatial_varying <- ~0
  tiny$internal$space_term_ram <- list(output = list(ram = matrix(numeric(), 0L, 4L)))
  inventory <- .check_term_inventory(tiny)$terms
  expect_false(any(inventory$term == "spatial_field"))
  expect_false(any(inventory$term == "spatially_varying_effects"))
})

test_that("GAM by-level summaries retain the parent formula term and native statistic identity", {
  skip_if_not_installed("mgcv")
  d <- check_term_data()
  f <- mgcv::gam(y ~ group + s(x, by = group, k = 4), data = d)
  x <- .check_terms(f, check_term_residuals(d), terms = "s(x, by = group, k = 4)")
  native <- x[x$check == "smooth_summary", ]
  expect_identical(native$term, rep("s(x, by = group, k = 4)", 3L))
  expect_true(all(grepl("statistic is F", native$method)))
  expect_match(native$method, "group[ABC]")
  expect_equal(native$df, as.numeric(summary(f)$s.table[, 1L]))
  expect_equal(native$p_value, as.numeric(summary(f)$s.table[, 4L]))
})

test_that("group screens record supported category sample counts", {
  d <- check_term_data()
  d$code <- rep(1:3, each = 20)
  f <- glm(y ~ base::factor(code), data = d, family = poisson())
  x <- .check_terms(f, check_term_residuals(d))
  expect_identical(x$check, "group_location")
  expect_match(x$message, "3 groups; group counts range from 20 to 20 records")
})
