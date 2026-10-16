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
  expect_equal(a$retained_cells, b$retained_cells)
  for (batch in c(1L, 3L, 4L, 5L)) {
    alternative <- f$run(omit = c("spatial", "x", "year"),
      batch_size = batch, draw_batch_size = 1L)
    expect_equal(a$decomposition, alternative$decomposition, tolerance = 1e-12)
    expect_equal(a$baseline$draws, alternative$baseline$draws)
    expect_equal(a$omission, alternative$omission)
  }
  expect_equal(Reduce(`+`, a$partition_draws), a$baseline$draws)
  expect_equal(f$env$freed, f$env$created)
  expect_equal(a$sampled_only$cell_ever$metadata$retained_weight,
    c(`1` = 0.5, `2` = 0.5))
  expect_true(all(is.na(a$sampled_only$cell_always$table$Mean)))
  expect_setequal(names(a$omission), c(
    "spatial:unsampled:cell_time", "spatial:all",
    "x:unsampled:cell_time", "x:all",
    "year:unsampled:cell_time", "year:all"))
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
  expect_equal(a$retained_cells$x, a$retained_cells$X - 1)
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
    "spatial:never_sampled", "spatial:all"))
  ever <- f$run(omit = "spatial", where = "unsampled", uncertainty = FALSE,
    settings = utils::modifyList(f$args$settings, list(omission_definition = "cell_ever")))
  explicit <- f$run(omit = "spatial", where = "unsampled", uncertainty = FALSE,
    omission_definition = "cell_ever")
  expect_equal(explicit$omission[[1L]]$table, ever$omission[[1L]]$table)
  expect_equal(explicit$omission[[1L]]$metadata$omission_definition, "cell_ever")
  always <- f$run(omit = "spatial", where = "unsampled", uncertainty = FALSE,
    settings = utils::modifyList(f$args$settings, list(omission_definition = "cell_always")))
  expect_equal(a$omission[["spatial:never_sampled"]]$table$Mean,
    ever$omission[["spatial:unsampled:cell_ever"]]$table$Mean)
  expect_equal(a$omission[["spatial:all"]]$table$Mean,
    always$omission[["spatial:unsampled:cell_always"]]$table$Mean)
  b <- f$run(masks = list(cell_ever = f$args$masks$cell_ever),
    sampled_only = FALSE, uncertainty = FALSE, omit = "spatial")
  expect_equal(a$partition, b$partition)
  expect_length(b$sampled_only, 0)
  expect_setequal(names(b$omission), c("spatial:unsampled:cell_time", "spatial:all"))
  expect_equal(b$omission[["spatial:unsampled:cell_time"]]$table,
    a$omission[["spatial:unsampled:cell_time"]]$table)
  selected <- f$run(omit = "spatial", uncertainty = FALSE,
    settings = utils::modifyList(f$args$settings, list(sampled_definition = "cell_ever",
      omission_definition = "cell_time")))
  expect_named(selected$sampled_only, "cell_ever")
  expect_setequal(names(selected$omission),
    c("spatial:unsampled:cell_time", "spatial:all"))
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
  expected <- f$run(omit = c("svc", "spatial"), where = "all", uncertainty = FALSE)
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
  a <- f$run(omit = c("svc", "svc:x", "spatial"), where = "all", uncertainty = FALSE)
  expect_equal(a$omission[["svc:all"]]$table, expected$omission[["svc:all"]]$table)
  expect_equal(a$omission[["svc:x:all"]]$table, expected$omission[["svc:all"]]$table)
  expect_equal(a$omission[["spatial:all"]]$table, expected$omission[["spatial:all"]]$table)
  expect_false("svc" %in% a$decomposition$term)
  expect_true("svc:x" %in% a$decomposition$term)
})

test_that("spatial omission bundles persistent and SVC fields like spatial_fields", {
  f <- domain_reduce_fixture()
  a <- f$run(omit = c("spatial", "svc"), where = "all")
  expected <- sapply(1:2, function(y) {
    p <- f$env$.domain_projection(NULL, NULL, f$args$newdata(rep(y, 3), 1:3), NULL)
    terms <- p$terms(0)
    sum(p$response(0, terms$spatial + terms$svc) * f$args$weights)
  })
  persistent_only <- sapply(1:2, function(y) {
    p <- f$env$.domain_projection(NULL, NULL, f$args$newdata(rep(y, 3), 1:3), NULL)
    sum(p$response(0, p$terms(0)$spatial) * f$args$weights)
  })
  expect_equal(a$omission[["spatial:all"]]$table$Mean, expected)
  expect_true(all(a$omission[["spatial:all"]]$table$Mean < persistent_only))
  expect_true(all(a$omission[["spatial:all"]]$table$Mean <
    a$omission[["svc:all"]]$table$Mean))
  expect_equal(a$omission[["spatial:all"]]$metadata$omission_bundle, c("spatial", "svc"))
  set.seed(19)
  parameters <- rnorm(17)
  draw_expected <- sapply(1:2, function(y) {
    p <- f$env$.domain_projection(NULL, NULL, f$args$newdata(rep(y, 3), 1:3), NULL)
    vapply(parameters, function(par) {
      terms <- p$terms(par)
      sum(p$response(par, terms$spatial + terms$svc) * f$args$weights)
    }, numeric(1))
  })
  expect_equal(a$omission[["spatial:all"]]$draws, draw_expected, ignore_attr = TRUE)
})

test_that("restricted annual comparisons and combined unsampled shares use paired draws", {
  f <- domain_reduce_fixture()
  a <- f$run()
  expect_setequal(unique(a$sampled_only_summary$series), names(a$sampled_only))
  expect_identical(a$comparison_summary, a$omission_summary)
  rows <- a$sampled_only_summary$series == "cell_time"
  tab <- a$sampled_only_summary[rows, ]
  ratios <- a$sampled_only$cell_time$draws / a$baseline$draws
  differences <- log(a$sampled_only$cell_time$draws) - log(a$baseline$draws)
  expect_equal(tab$ratio_lower, apply(ratios, 2, quantile, probs = 0.025),
    ignore_attr = TRUE)
  expect_equal(tab$log_difference_upper,
    apply(differences, 2, quantile, probs = 0.975), ignore_attr = TRUE)
  shares <- (a$partition_draws$previously_sampled +
    a$partition_draws$never_sampled) / a$baseline$draws
  expect_equal(a$unsampled_share$lower,
    apply(shares, 2, quantile, probs = 0.025), ignore_attr = TRUE)
  expect_equal(a$unsampled_share$upper,
    apply(shares, 2, quantile, probs = 0.975), ignore_attr = TRUE)
  expect_equal(a$unsampled_share$share,
    1 - a$partition$share[a$partition$class == "sampled"])
  expect_true(all(a$unsampled_share$variable_domain))
  expect_equal(a$sampled_only$cell_time$metadata$definition, "cell_time")
  expect_true(a$sampled_only$cell_time$metadata$variable_domain)
  expect_false(a$sampled_only$cell_ever$metadata$variable_domain)
})

test_that("the public domain entry point passes its independent omission definition", {
  f <- domain_reduce_fixture()
  public <- influ2:::index_domain
  environment(public) <- f$env
  f$env$.domain_reduce <- f$reduce
  f$env$.index_spatial_info <- function(...) {
    list(data = data.frame(year = rep(1:2, each = 3)), year = "year",
      time = "year", mode = 0, family = "poisson", link = "log")
  }
  x <- public(structure(list(), class = "sdmTMB"), f$args$reference_data,
    sampled = f$args$cell_time, sampled_definition = "cell_ever",
    omit = "spatial", where = "unsampled", uncertainty = "none",
    decompose = FALSE)
  expect_named(x$sampled_only, "cell_ever")
  expect_named(x$omission, "spatial:unsampled:cell_time")
  expect_equal(x$metadata$omission_definition, "cell_time")
  expect_true(all(c("Year", "share", "lower", "upper") %in% names(x$unsampled_share)))
  expect_equal(f$env$created, f$env$freed)
})

test_that("temporal coefficient pieces and their aggregate alias are zeroed fields", {
  f <- domain_reduce_fixture()
  baseline <- f$run()
  native <- f$env$.domain_projection
  f$env$.domain_projection <- function(model, backend, data, offset, year = NULL) {
    p <- native(model, backend, data, offset, year)
    terms <- p$terms
    p$terms <- function(par) {
      value <- terms(par)
      value[["temporal:x"]] <- matrix(data$x * (par[1] + 0.3), ncol = 1,
        dimnames = list(NULL, "conditional"))
      value[["temporal:(Intercept)"]] <- matrix(0.2, nrow(data), 1,
        dimnames = list(NULL, "conditional"))
      value
    }
    p$eta <- function(par) Reduce(`+`, p$terms(par))
    p$response <- function(par, subtract = NULL) {
      eta <- p$eta(par)
      if (!is.null(subtract)) eta <- eta - subtract
      as.numeric(exp(eta))
    }
    p
  }
  a <- f$run(omit = c("temporal", "temporal:x"), where = c("all", "unsampled"))
  expect_equal(a$omission[["temporal:all"]]$table$Mean, baseline$baseline$table$Mean)
  expect_equal(a$omission[["temporal:all"]]$draws, baseline$baseline$draws)
  expect_equal(a$omission[["temporal:x:all"]]$table$Mean,
    baseline$baseline$table$Mean * exp(0.2))
  expect_equal(a$omission[["temporal:x:all"]]$draws, baseline$baseline$draws * exp(0.2))
  expect_false("temporal" %in% a$decomposition$term)
  expect_true(all(c("temporal:x", "temporal:(Intercept)") %in% a$decomposition$term))
  expect_equal(f$env$created, f$env$freed)
})

test_that("retained cells preserve custom native and geographic coordinate names", {
  f <- domain_reduce_fixture()
  reference <- f$args$reference_data
  reference$ycoord <- c(10, 20, 30)
  reference$lon <- c(-170, -160, -150)
  reference$lat <- c(45, 50, 55)
  a <- f$run(reference_data = reference, uncertainty = FALSE)
  expect_true(all(c("X", "Y", "x", "ycoord", "lon", "lat") %in%
    names(a$retained_cells)))
  for (name in c("x", "ycoord", "lon", "lat")) {
    expect_equal(a$retained_cells[[name]],
      reference[[name]][match(a$retained_cells$cell, f$args$positive)])
  }
})

test_that("empty automatic unsampled series are silent but requested empty domains warn", {
  f <- domain_reduce_fixture()
  args <- f$args
  args$cell_time[] <- TRUE
  args$classes[] <- "sampled"
  args$masks <- lapply(args$masks, function(x) {
    x[] <- TRUE
    x
  })
  expect_no_warning(a <- do.call(f$reduce, args))
  expect_true(all(is.na(a$unsampled_only$table$Mean)))
  expect_true(all(is.na(a$unsampled_only$draws)))
  expect_equal(a$unsampled_share$share, c(0, 0))
  args$settings$method <- "integrated"
  expect_no_warning(b <- do.call(f$reduce, args))
  expect_equal(b$unsampled_only$table$Mean, c(0, 0))
  expect_equal(unname(b$unsampled_only$draws), matrix(0, 17, 2))
  args$masks$cell_always[] <- FALSE
  expect_warning(do.call(f$reduce, args), "Empty cell_always")
})
