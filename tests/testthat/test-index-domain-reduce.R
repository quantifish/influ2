domain_reduce_fixture <- function() {
  env <- new.env(parent = environment(influ2:::.domain_reduce))
  env$freed <- 0L
  env$created <- 0L
  env$.index_free_tmb <- function(obj) env$freed <- env$freed + 1L
  env$.index_spatial_sampler <- function(info, obj) {
    function(n) matrix(stats::rnorm(n), nrow = 1L)
  }
  env$.domain_projection <- function(model, backend, data, offset, year = NULL) {
    env$created <- env$created + 1L
    terms <- function(par) {
      m <- function(x) matrix(x, ncol = 1L,
        dimnames = list(NULL, "conditional"))
      list(intercept = m(rep(1 + par[1], nrow(data))),
        year = m(data$year / 10), x = m(data$x * (1 + par[1] / 10)),
        spatial = m(data$X / 10), svc = m(data$x * data$X / 100))
    }
    eta <- function(par) Reduce(`+`, terms(par))
    response <- function(par, subtract = NULL) {
      z <- eta(par)
      if (!is.null(subtract)) z <- z - subtract
      as.numeric(exp(z))
    }
    list(obj = list(), terms = terms, eta = eta, response = response)
  }
  reduce <- influ2:::.domain_reduce
  environment(reduce) <- env
  years <- c("1", "2")
  reference <- data.frame(x = c(0, 1, 2), X = c(1, 2, 3), Y = c(4, 5, 6))
  sampled <- matrix(c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE), 3, 2)
  classes <- matrix(c("sampled", "previously_sampled", "never_sampled",
    "previously_sampled", "sampled", "never_sampled"), 3, 2)
  masks <- list(cell_time = sampled,
    cell_ever = matrix(c(TRUE, TRUE, FALSE), 3, 2),
    cell_always = matrix(FALSE, 3, 2))
  settings <- list(backend = "mock", method = "standardised",
    probs = c(0.025, 0.975), reference = "fixed common population",
    min_years = 1, rescale = "raw")
  args <- list(model = list(), info = list(mode = 0, year = "year"),
    years = years, newdata = function(y, rows) {
      d <- reference[rows, , drop = FALSE]
      d$year <- as.numeric(years[y])
      d
    }, weights = c(0.2, 0.3, 0.5), classes = classes, masks = masks,
    cell_time = sampled, omit = character(), where = c("unsampled", "all"),
    covariate_reference = "sampled_mean", decompose = TRUE,
    retain_cells = "2", positive = c(2L, 4L, 6L), reference_data = reference,
    settings = settings, uncertainty = TRUE, ndraws = 17L,
    batch_size = 2L, draw_batch_size = 3L, seed = 19L, offset = NULL,
    sampled_only = TRUE)
  run <- function(...) {
    changes <- list(...)
    a <- args
    a[names(changes)] <- changes
    suppressWarnings(do.call(reduce, a))
  }
  list(env = env, reduce = reduce, args = args, run = run)
}

test_that("domain reduction uses shared joint draws independently of block sizes", {
  f <- domain_reduce_fixture()
  set.seed(198)
  before <- .Random.seed
  a <- f$run(omit = c("spatial", "x", "year"))
  expect_identical(.Random.seed, before)
  b <- f$run(omit = c("spatial", "x", "year"),
    batch_size = 6L, draw_batch_size = 11L)
  expect_identical(.Random.seed, before)
  expect_equal(a$baseline, b$baseline)
  expect_equal(a$sampled_only, b$sampled_only)
  expect_equal(a$omission, b$omission)
  expect_equal(a$partition, b$partition)
  expect_equal(a$decomposition, b$decomposition, tolerance = 1e-12)
  expect_equal(Reduce(`+`, a$partition_draws), a$baseline$draws)
  expect_equal(f$env$freed, f$env$created)
  expect_equal(a$sampled_only$cell_ever$metadata$retained_weight,
    c(`1` = 0.5, `2` = 0.5))
  expect_true(all(is.na(a$sampled_only$cell_always$table$Mean)))
  expect_setequal(names(a$omission), c(
    "spatial:unsampled:cell_time", "spatial:unsampled:cell_ever",
    "spatial:unsampled:cell_always", "spatial:all",
    "x:unsampled:cell_time", "x:unsampled:cell_ever",
    "x:unsampled:cell_always", "x:all",
    "year:unsampled:cell_time", "year:unsampled:cell_ever",
    "year:unsampled:cell_always", "year:all"))
})

test_that("domain restrictions renormalise means but not totals", {
  f <- domain_reduce_fixture()
  a <- f$run(uncertainty = FALSE)
  mu <- function(y) exp(1 + y / 10 + c(0, 1, 2) +
    c(1, 2, 3) / 10 + c(0, 2, 6) / 100)
  expect_equal(a$baseline$table$Mean,
    vapply(1:2, function(y) sum(mu(y) * f$args$weights), numeric(1)))
  expect_equal(a$sampled_only$cell_time$table$Mean, c(mu(1)[1], mu(2)[2]))
  expect_equal(a$unsampled_only$table$Mean,
    c(sum(mu(1)[2:3] * c(0.3, 0.5)) / 0.8,
      sum(mu(2)[c(1, 3)] * c(0.2, 0.5)) / 0.7))
  settings <- f$args$settings
  settings$method <- "integrated"
  b <- f$run(settings = settings, weights = f$args$weights * 10,
    uncertainty = FALSE)
  expect_equal(b$baseline$table$Mean, a$baseline$table$Mean * 10)
  expect_equal(b$sampled_only$cell_time$table$Mean,
    a$sampled_only$cell_time$table$Mean * c(2, 3))
  expect_equal(b$sampled_only$cell_always$table$Mean, c(0, 0))
  expect_null(a$baseline$draws)
  expect_true(all(is.na(a$partition$share_lower)))
})

test_that("omissions use year-row references and draw-specific sampled contributions", {
  f <- domain_reduce_fixture()
  a <- f$run(omit = c("year", "x", "spatial", "svc"), where = "all")
  expect_equal(a$omission[["year:all"]]$table$Mean,
    rep(a$baseline$table$Mean[1], 2))
  set.seed(19)
  draws <- rnorm(17)
  expected <- sapply(1:2, function(y) {
    sampled_row <- y
    vapply(draws, function(par) {
      d <- f$args$newdata(rep(y, 3), 1:3)
      p <- f$env$.domain_projection(NULL, NULL, d, NULL)
      terms <- p$terms(par)
      subtract <- terms$x - terms$x[sampled_row, 1]
      sum(p$response(par, subtract) * f$args$weights)
    }, numeric(1))
  })
  expect_equal(a$omission[["x:all"]]$draws, expected, ignore_attr = TRUE)
  expect_true(all(a$omission[["spatial:all"]]$table$Mean <
    a$omission[["svc:all"]]$table$Mean))
  fixed <- f$run(omit = "x", where = "all",
    covariate_reference = list(x = list(rule = "fixed", value = list(x = 3))))
  zero <- f$run(omit = "x", where = "all", covariate_reference = "zero")
  expect_true(all(fixed$omission[["x:all"]]$table$Mean >
    zero$omission[["x:all"]]$table$Mean))
  row <- a$omission_summary[a$omission_summary$series == "year:all", ]
  expect_equal(row$ratio, a$omission[["year:all"]]$table$Mean / a$baseline$table$Mean)
  expect_equal(row$log_difference, log(row$ratio))
})

test_that("decomposition uses weighted population moments and bounded histograms", {
  f <- domain_reduce_fixture()
  a <- f$run(uncertainty = FALSE)
  d <- subset(a$decomposition, class == "all" & term == "x")
  expect_equal(d$mean, c(1.3, 1.3))
  expect_equal(d$SD, rep(sqrt(sum(c(0.2, 0.3, 0.5) * (c(0, 1, 2) - 1.3)^2)), 2))
  expect_true(all(abs(d$Q10 - 0) <= d$bin_width))
  expect_true(all(abs(d$Q50 - 1) <= d$bin_width))
  expect_true(all(abs(d$Q90 - 2) <= d$bin_width))
  expect_equal(nrow(a$retained_cells), 15L)
  expect_setequal(a$retained_cells$cell, c(2L, 4L, 6L))
  expect_true(all(a$retained_cells$Year == "2"))
  expect_equal(a$retained_cells$X, match(a$retained_cells$cell, c(2L, 4L, 6L)))
})

test_that("native failures free all objectives and restore caller RNG", {
  f <- domain_reduce_fixture()
  set.seed(412)
  before <- .Random.seed
  expect_error(f$run(omit = "not_a_term"), "Unknown omission term")
  expect_identical(.Random.seed, before)
  expect_equal(f$env$freed, f$env$created)
  native <- f$env$.domain_projection
  f$env$.domain_projection <- function(...) {
    p <- native(...)
    p$response <- function(...) rep(Inf, 2L)
    p
  }
  expect_error(f$run(), "finite")
  expect_identical(.Random.seed, before)
  expect_equal(f$env$freed, f$env$created)
})

test_that("omission masks use requested definitions but reporting classes do not", {
  f <- domain_reduce_fixture()
  a <- f$run(omit = "spatial", where = c("unsampled", "never_sampled", "all"),
    uncertainty = FALSE)
  expect_setequal(names(a$omission), c("spatial:unsampled:cell_time",
    "spatial:unsampled:cell_ever", "spatial:unsampled:cell_always",
    "spatial:never_sampled", "spatial:all"))
  expect_equal(a$omission[["spatial:never_sampled"]]$table$Mean,
    a$omission[["spatial:unsampled:cell_ever"]]$table$Mean)
  expect_equal(a$omission[["spatial:all"]]$table$Mean,
    a$omission[["spatial:unsampled:cell_always"]]$table$Mean)
  b <- f$run(masks = list(cell_ever = f$args$masks$cell_ever),
    sampled_only = FALSE, uncertainty = FALSE)
  expect_equal(a$partition, b$partition)
  expect_length(b$sampled_only, 0)
})

test_that("delta component contributions are omitted together before response conversion", {
  f <- domain_reduce_fixture()
  native <- f$env$.domain_projection
  f$env$.domain_projection <- function(...) {
    p <- native(...)
    one <- p$terms
    p$terms <- function(par) lapply(one(par), function(x) {
      cbind(encounter = x[, 1] / 2, positive = x[, 1])
    })
    p$eta <- function(par) Reduce(`+`, p$terms(par))
    p$response <- function(par, subtract = NULL) {
      eta <- p$eta(par)
      if (!is.null(subtract)) eta <- eta - subtract
      stats::plogis(eta[, 1]) * exp(eta[, 2])
    }
    p
  }
  a <- f$run(omit = c("x", "year"), where = "all")
  b <- f$run(omit = c("x", "year"), where = "all", batch_size = 6L,
    draw_batch_size = 17L)
  expect_equal(a$omission, b$omission)
  expect_setequal(a$decomposition$component, c("encounter", "positive"))
  expect_equal(a$omission[["year:all"]]$draws[, 1],
    a$omission[["year:all"]]$draws[, 2])
  expect_equal(f$env$created, f$env$freed)
})

test_that("zero restricted totals have valid ratios but undefined log differences", {
  f <- domain_reduce_fixture()
  settings <- f$args$settings
  settings$method <- "integrated"
  a <- f$run(settings = settings)
  rows <- a$omission_summary$series == "cell_always"
  expect_equal(a$omission_summary$ratio[rows], c(0, 0))
  expect_equal(a$omission_summary$ratio_lower[rows], c(0, 0))
  expect_true(all(is.na(a$omission_summary$log_difference[rows])))
  expect_true(all(is.na(a$omission_summary$log_difference_lower[rows])))
})

test_that("RNG absence and temporary objectives are preserved on alternate errors", {
  f <- domain_reduce_fixture()
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit(if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv), add = TRUE)
  if (had_seed) rm(".Random.seed", envir = .GlobalEnv)
  f$run()
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  expect_error(f$run(omit = "x", covariate_reference =
    list(x = list(rule = "fixed", value = list(missing = 3)))), "unknown predictor")
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  expect_equal(f$env$created, f$env$freed)
})

test_that("SVC field aliases omit native pieces without double-counting decomposition", {
  f <- domain_reduce_fixture()
  expected <- f$run(omit = "svc", where = "all", uncertainty = FALSE)
  native <- f$env$.domain_projection
  f$env$.domain_projection <- function(...) {
    p <- native(...)
    terms <- p$terms
    p$terms <- function(par) {
      value <- terms(par)
      value[["svc:x"]] <- value$svc
      value$svc <- NULL
      value
    }
    p
  }
  a <- f$run(omit = c("svc", "svc:x"), where = "all", uncertainty = FALSE)
  expect_equal(a$omission[["svc:all"]]$table, expected$omission[["svc:all"]]$table)
  expect_equal(a$omission[["svc:x:all"]]$table, expected$omission[["svc:all"]]$table)
  expect_false("svc" %in% a$decomposition$term)
  expect_true("svc:x" %in% a$decomposition$term)
})
