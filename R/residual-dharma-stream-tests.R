# Bounded-memory equivalents of the non-refitted DHARMa 0.4.7/0.5.0 response
# dispersion and outlier tests. Genuine DHARMa objects are never fabricated.
# Reference algorithms: DHARMa/R/tests.R (testDispersion, testOutliers) and
# DHARMa/R/helper.R (getP, getQuantile), florianhartig/DHARMa on GitHub.
.resid_dharma_stream_check <- function(cache, observed) {
  if (!is.numeric(cache$n) || length(cache$n) != 1L ||
      !is.finite(cache$n) || cache$n < 1L || cache$n > .Machine$integer.max ||
      cache$n != as.integer(cache$n) ||
      !is.numeric(cache$nsim) || length(cache$nsim) != 1L ||
      !is.finite(cache$nsim) || cache$nsim < 2L || cache$nsim > .Machine$integer.max ||
      cache$nsim != as.integer(cache$nsim)) {
    stop("The DHARMa simulation cache has invalid dimensions.", call. = FALSE)
  }
  if (!is.numeric(observed) || length(observed) != cache$n ||
      any(!is.finite(observed))) {
    stop("Observed responses do not match the DHARMa simulation cache.", call. = FALSE)
  }
  block <- cache$row_block_size
  if (!is.numeric(block) || length(block) != 1L || !is.finite(block) ||
      block < 1L || block > .Machine$integer.max || block != as.integer(block)) {
    stop("The DHARMa cache row-block size must be a positive integer.", call. = FALSE)
  }
  if (!is.null(cache$method) && !identical(cache$method, "PIT")) {
    stop("Bounded-memory DHARMa tests currently require method = 'PIT'.", call. = FALSE)
  }
  seq.int(1L, cache$n, by = block)
}

.resid_dharma_stream_version <- function() as.character(utils::packageVersion("DHARMa"))

.resid_dharma_stream_require_reference <- function() {
  version <- .resid_dharma_stream_version()
  if (!version %in% c("0.4.7", "0.5.0")) {
    stop("Bounded-memory equivalent tests have not been validated against DHARMa ",
      version, "; use `dharma_storage = 'memory'` for its native tests or await version validation.", call. = FALSE)
  }
  invisible(NULL)
}

.resid_dharma_stream_block <- function(cache, first, last) {
  x <- .resid_dharma_cache_read(cache, first, last)
  if (!is.matrix(x) || !is.numeric(x) ||
      !identical(dim(x), c(as.integer(last - first + 1L), as.integer(cache$nsim))) ||
      any(!is.finite(x))) {
    stop("The DHARMa cache returned an invalid simulation row block.", call. = FALSE)
  }
  x
}

.resid_dharma_stream_p <- function(simulated, observed) {
  # Native DHARMa getP's two-sided finite Monte Carlo reference. Do not add
  # pseudocounts or silently replace a finite Monte Carlo zero by a bound.
  min(2 * min(mean(simulated <= observed), mean(simulated >= observed)), 1)
}

.resid_dharma_stream_source <- function() {
  list(implementation = "influ2 bounded-memory equivalent",
    reference = "DHARMa 0.4.7/0.5.0 non-refitted PIT tests",
    dharma_version = .resid_dharma_stream_version(),
    source = "https://github.com/florianhartig/DHARMa/blob/master/DHARMa/R/tests.R")
}

.resid_dharma_stream_column <- function(cache, column, starts) {
  if (is.environment(cache)) {
    # A column-major cache can seek directly to one full response vector.
    # O(n) temporary storage is independent of the simulation count.
    return(.resid_dharma_cache_read_column(cache, column))
  }
  x <- numeric(cache$n)
  for (first in starts) {
    last <- min(cache$n, first + cache$row_block_size - 1L)
    x[first:last] <- .resid_dharma_stream_block(cache, first, last)[, column]
  }
  x
}

.resid_dharma_stream_dispersion <- function(cache, observed, predicted) {
  .resid_dharma_stream_require_reference()
  starts <- .resid_dharma_stream_check(cache, observed)
  if (!is.numeric(predicted) || length(predicted) != cache$n ||
      any(!is.finite(predicted))) {
    stop("Predictive means do not match the DHARMa simulation cache.", call. = FALSE)
  }
  n <- cache$n
  nsim <- cache$nsim
  # Two passes avoid subtracting squared large, almost equal raw moments.
  # Keep only one bounded row slab and O(nsim) centred-moment accumulators.
  means <- numeric(nsim)
  total_mean <- 0
  for (first in starts) {
    last <- min(n, first + cache$row_block_size - 1L)
    x <- .resid_dharma_stream_block(cache, first, last)
    means <- means + colSums(x - predicted[first:last]) / n
    total_mean <- total_mean + sum(x) / (as.double(n) * nsim)
  }
  squared <- centred <- numeric(nsim)
  total_squared <- total_centred <- 0
  for (first in starts) {
    last <- min(n, first + cache$row_block_size - 1L)
    x <- .resid_dharma_stream_block(cache, first, last)
    z <- sweep(x - predicted[first:last], 2L, means, "-")
    squared <- squared + colSums(z * z)
    centred <- centred + colSums(z)
    z <- x - total_mean
    total_squared <- total_squared + sum(z * z)
    total_centred <- total_centred + sum(z)
  }
  # The correction is the same centred-sum correction used by R's variance
  # calculation; negative round-off cannot be a meaningful variance.
  simulated_variance <- pmax(squared - centred^2 / n, 0) / (n - 1)
  observed_variance <- stats::var(observed - predicted)
  rm(x, z)
  # A block-reduced variance can differ from R's vector variance by a few
  # ulps. Such a difference must not remove a true reference tie and change
  # a Monte Carlo p-value. Recalculate only near-boundary columns with native
  # var(), rather than imposing a tolerance on the hypothesis-test comparison.
  near <- which(is.finite(simulated_variance) & is.finite(observed_variance) &
    abs(simulated_variance - observed_variance) <=
      64 * .Machine$double.eps * pmax(abs(simulated_variance), abs(observed_variance)))
  for (column in near) {
    simulated_variance[column] <- stats::var(
      .resid_dharma_stream_column(cache, column, starts) - predicted)
  }
  count <- as.double(n) * nsim
  expected_variance <- max(total_squared - total_centred^2 / count, 0) / (count - 1)
  simulated <- simulated_variance / expected_variance
  observed_spread <- observed_variance / expected_variance
  structure(list(statistic = c(dispersion = observed_spread / mean(simulated)),
    p.value = .resid_dharma_stream_p(simulated, observed_spread),
    alternative = "two.sided",
    method = "influ2 bounded-memory equivalent of DHARMa nonparametric dispersion test",
    data.name = "cached response simulations",
    source = .resid_dharma_stream_source()), class = "htest")
}

.resid_dharma_stream_outliers <- function(cache, observed, pit,
    integer_response, nboot = min(100L, cache$nsim)) {
  .resid_dharma_stream_require_reference()
  starts <- .resid_dharma_stream_check(cache, observed)
  if (!is.numeric(pit) || length(pit) != cache$n ||
      any(!is.finite(pit)) || any(pit < 0 | pit > 1)) {
    stop("PIT values do not match the DHARMa simulation cache.", call. = FALSE)
  }
  if (!is.logical(integer_response) || length(integer_response) != 1L ||
      is.na(integer_response)) {
    stop("`integer_response` must be TRUE or FALSE.", call. = FALSE)
  }
  n <- cache$n
  nsim <- cache$nsim
  if (!integer_response) {
    # Native continuous-response outlier definition uses finite rank margins,
    # not only exact 0/1 PIT endpoints.
    outliers <- sum(pit < 1 / (nsim + 1)) + sum(pit > 1 - 1 / (nsim + 1))
    out <- stats::binom.test(outliers, n, p = 2 / (nsim + 1),
      alternative = "two.sided")
    out$method <- "influ2 bounded-memory equivalent of DHARMa binomial outlier test"
    out$data.name <- "cached response simulations"
    out$margin <- "both"
    names(out$statistic) <- "outliers at both margin(s)"
    names(out$parameter) <- "observations"
    names(out$estimate) <- paste("frequency of outliers (expected:", out$null.value, ")")
    out$source <- .resid_dharma_stream_source()
    return(out)
  }
  if (!is.numeric(nboot) || length(nboot) != 1L || !is.finite(nboot) ||
      nboot < 1L || nboot > .Machine$integer.max || nboot != as.integer(nboot)) {
    stop("`nboot` must be a positive integer.", call. = FALSE)
  }
  if (nboot > nsim) {
    message("DHARMa bootstrap outlier replicates reduced to the number of response simulations.")
    nboot <- nsim
  }
  outliers <- mean(pit == 0) + mean(pit == 1)
  frequencies <- numeric(nboot)
  indices <- seq_len(nsim)
  # Preserve native bootstrap-major RNG ordering: sample columns once for
  # each whole joint simulated dataset, then randomise rows in their original
  # order. Drawing every selection first would change subsequent selections.
  for (i in seq_len(nboot)) {
    selected <- sample(indices[-i], size = nsim, replace = TRUE)
    upper <- lower <- 0L
    for (first in starts) {
      last <- min(n, first + cache$row_block_size - 1L)
      x <- .resid_dharma_stream_block(cache, first, last)
      residuals <- DHARMa::getQuantile(simulations = x[, selected, drop = FALSE],
        observed = x[, i], integerResponse = TRUE, method = "PIT")
      .resid_dharma_validate_pit(residuals, nrow(x))
      upper <- upper + sum(residuals == 1)
      lower <- lower + sum(residuals == 0)
    }
    frequencies[i] <- upper / n + lower / n
  }
  structure(list(statistic = stats::setNames(outliers * n, "outliers at both margin(s)"),
    parameter = c(observations = n),
    p.value = .resid_dharma_stream_p(frequencies, outliers),
    conf.int = stats::quantile(frequencies, c(0.025, 0.975)),
    estimate = stats::setNames(outliers, paste("outlier frequency (expected:", mean(frequencies), ")")),
    alternative = "two.sided", margin = "both",
    method = "influ2 bounded-memory equivalent of DHARMa bootstrapped outlier test",
    data.name = "cached response simulations", source = .resid_dharma_stream_source()),
    class = "htest")
}
