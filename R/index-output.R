#' Format a calculated index for assessment reporting
#'
#' Return a reporting table without refitting or repeating predictions. The
#' full, stable table is still available through [as.data.frame.influ_index()].
#' @md
#' @param x An `influ_index` from [cpue_index()] or [integrate_index()].
#' @param format `"summary"` for the existing index summaries, or `"lognormal"`
#'   to append a moment-matched lognormal approximation to marginal index
#'   uncertainty. This is not the fitted response distribution.
#' @param include_median `"auto"` omits `Median` only when every value is
#'   missing; `"always"` retains it, and `"never"` omits it. An available brms
#'   posterior median is never replaced by a lognormal approximation.
#' @details `Mean`, `SD`, and `CV` refer to the expected-response index and its
#'   uncertainty, not variability among individual observations. With
#'   `format = "lognormal"`, `SDlog = sqrt(log(1 + CV^2))`,
#'   `Meanlog = log(Mean) - SDlog^2 / 2`, and
#'   `LognormalMedian = exp(Meanlog)`. These additional columns describe a
#'   univariate moment-matched approximation, even when the CPUE model itself
#'   is not lognormal. They require positive means and finite non-negative
#'   index SDs. Preview and year-effect results cannot provide this output.
#'
#'   `Meanlog` is not `log(Mean)`, and `SDlog` is not automatically the square
#'   root of the diagonal of [index_vcov()] on the log scale. The latter comes
#'   from the actual joint delta method or log-index draws, not marginal
#'   lognormal moment matching. Use the joint matrix for a correlated-index
#'   assessment; do not combine its correlations with these approximate SDs
#'   without explicitly choosing a different uncertainty model.
#' @return A data frame in the stored year order. The input is unchanged.
#' @seealso [index_vcov()], [plot_index()], [cpue_index()]
#' @examples
#' data(lobsters_per_pot)
#' fit <- glm(lobsters ~ year + depth, poisson(), data = lobsters_per_pot)
#' index <- cpue_index(fit, reference_data = data.frame(depth = 40))
#' head(index_table(index))
#' head(index_table(index, format = "lognormal"))
#' @export
index_table <- function(x, format = c("summary", "lognormal"),
    include_median = c("auto", "always", "never")) {
  if (!inherits(x, "influ_index") || !is.data.frame(x$table)) {
    stop("Supply an `influ_index` calculated by `cpue_index()` or `integrate_index()`.", call. = FALSE)
  }
  format <- match.arg(format)
  include_median <- match.arg(include_median)
  tab <- x$table
  if (include_median == "never" ||
      (include_median == "auto" && all(is.na(tab$Median)))) tab$Median <- NULL
  if (format == "lognormal") {
    if (!isTRUE(x$metadata$method %in% c("standardised", "integrated")) ||
        identical(x$metadata$uncertainty, "none")) {
      stop("Lognormal assessment summaries require an expected-response index with uncertainty, not preview or year-effect output.", call. = FALSE)
    }
    if (!is.numeric(tab$Mean) || !is.numeric(tab$SD) || !nrow(tab) ||
        any(!is.finite(tab$Mean)) || any(tab$Mean <= 0) ||
        any(!is.finite(tab$SD)) || any(tab$SD < 0)) {
      stop("Lognormal assessment summaries require positive finite means and finite non-negative index SDs.", call. = FALSE)
    }
    # Stable log(1 + (SD / Mean)^2), including zero SD and very large ratios.
    z <- 2 * (log(tab$SD) - log(tab$Mean))
    log_variance <- pmax(z, 0) + log1p(exp(-abs(z)))
    tab$Meanlog <- log(tab$Mean) - log_variance / 2
    tab$SDlog <- sqrt(log_variance)
    tab$LognormalMedian <- exp(tab$Meanlog)
  }
  tab
}

#' Extract covariance among calculated annual indices
#'
#' Retrieve the joint uncertainty of the annual expected-response index,
#' not the model-coefficient covariance or observation-error covariance.
#' @md
#' @param x,object An `influ_index` from [cpue_index()] or [integrate_index()].
#' @param scale `"log"` (default) for covariance of log annual indices, or
#'   `"response"` for covariance in the reported response units.
#' @param years Optional unique year labels selecting and ordering both matrix
#'   dimensions. They must match `Year` in the index table exactly.
#' @param require_pd If `TRUE`, fail unless the selected matrix admits a
#'   Cholesky factorisation. No diagonal jitter or eigenvalue adjustment is
#'   applied. The default permits positive-semidefinite matrices.
#' @param ... Reserved for future use; unused arguments are rejected.
#' @details All six standardisation backends and area integration retain these
#'   compact matrices when uncertainty is calculated, including when only
#'   summaries are retained. GLM/GAM/glmmTMB covariance is obtained by propagating
#'   the joint fitted-parameter covariance through the weighted annual means.
#'   Log-scale covariance uses their log-index gradients. brms uses sample
#'   covariance of the same annual posterior draws, taking logs before
#'   calculating log covariance. sdmTMB/tinyVAST use their shared joint Gaussian
#'   parameter/field draws in the same way. Positive estimates and, where used,
#'   positive draws are required for log covariance; values are never clipped.
#'
#'   The matrix follows the index's reference population, random-effect target,
#'   units, and normalisation. Multiplying by a known positive constant changes
#'   response covariance by its square and leaves log covariance unchanged.
#'   However, normalising every draw to geometric mean one, or applying the
#'   corresponding delta method, makes log covariance singular: one common
#'   log-level has been removed. Prefer `rescale = "raw"` for an assessment
#'   estimating catchability. Other model structures or too few draws may also
#'   yield singular matrices. Do not repair these silently to fit a likelihood.
#'
#'   Missing covariance cannot be reconstructed from marginal SDs. Old saved
#'   summaries, point-estimate previews, and year-effect diagnostics therefore
#'   fail explicitly. Recalculate the expected-response index with uncertainty.
#'   The matrix includes no additional assessment observation/process error,
#'   uncertainty in reference weights or catchability conversions, or
#'   cross-series covariance from separately calculated index objects.
#' @return A numeric square matrix with matching year row and column names.
#' @seealso [index_table()], [plot_index()], [cpue_index()], [integrate_index()]
#' @examples
#' data(lobsters_per_pot)
#' fit <- glm(lobsters ~ year + depth, poisson(), data = lobsters_per_pot)
#' index <- cpue_index(fit, reference_data = data.frame(depth = 40))
#' Sigma <- index_vcov(index)
#' sqrt(diag(Sigma)) # Joint-calculation log-index SEs, not observation SDs.
#' plot(index, type = "correlation")
#' @export
index_vcov <- function(x, scale = c("log", "response"), years = NULL,
    require_pd = FALSE) {
  scale <- match.arg(scale)
  if (!inherits(x, "influ_index") || !is.data.frame(x$table) ||
      !isTRUE(x$metadata$method %in% c("standardised", "integrated"))) {
    stop("Supply a standardised or integrated expected-response `influ_index`, not model coefficients or year-effect contrasts.", call. = FALSE)
  }
  if (!is.logical(require_pd) || length(require_pd) != 1L || is.na(require_pd)) {
    stop("`require_pd` must be TRUE or FALSE.", call. = FALSE)
  }
  labels <- as.character(x$table$Year)
  if (!length(labels) || anyNA(labels) || any(!nzchar(labels)) || anyDuplicated(labels)) {
    stop("The index table must have unique, non-missing year labels.", call. = FALSE)
  }
  if (is.null(x$covariance)) {
    stop("No joint covariance is stored. Recalculate the index with uncertainty; marginal SDs cannot recover cross-year covariance.", call. = FALSE)
  }
  covariance <- x$covariance[[scale]]
  if (is.null(covariance)) {
    if (scale == "response") {
      stop("No response-scale joint covariance is stored; recalculate the index with uncertainty.", call. = FALSE)
    }
    stop("Log-index covariance requires positive index estimates and positive joint draws; use response scale where appropriate. Values are not clipped.", call. = FALSE)
  }
  if (!is.matrix(covariance) || !is.numeric(covariance) ||
      !identical(dim(covariance), rep(length(labels), 2L)) ||
      !identical(dimnames(covariance), list(labels, labels)) ||
      any(!is.finite(covariance))) {
    stop("Stored covariance must be finite and aligned exactly with the index table's years; do not subset or reorder the table alone.", call. = FALSE)
  }
  tolerance <- sqrt(.Machine$double.eps) * max(abs(covariance))
  if (max(abs(covariance - t(covariance))) > tolerance) {
    stop("Stored index covariance is not symmetric.", call. = FALSE)
  }
  if (min(eigen(covariance, symmetric = TRUE, only.values = TRUE)$values) < -tolerance) {
    stop("Stored index covariance is not positive semidefinite.", call. = FALSE)
  }
  if (!is.null(years)) {
    if (!(is.character(years) || is.numeric(years) || is.factor(years)) ||
        !is.null(dim(years)) || !length(years) || anyNA(years)) {
      stop("`years` must contain unique, non-missing year labels.", call. = FALSE)
    }
    years <- as.character(years)
    if (anyDuplicated(years) || any(!years %in% labels)) {
      stop("`years` must be unique labels present in the index table.", call. = FALSE)
    }
    covariance <- covariance[years, years, drop = FALSE]
  }
  if (require_pd && (inherits(try(chol(covariance), silent = TRUE), "try-error") ||
      min(eigen(covariance, symmetric = TRUE, only.values = TRUE)$values) <=
        .Machine$double.eps * nrow(covariance) * max(abs(covariance)))) {
    stop("Index covariance is singular or numerically non-positive-definite. Use an unnormalised index or an explicitly justified likelihood; no jitter is added.", call. = FALSE)
  }
  covariance
}

#' @rdname index_vcov
#' @export
vcov.influ_index <- function(object, scale = c("log", "response"), years = NULL,
    require_pd = FALSE, ...) {
  if (length(list(...))) stop("Unused covariance arguments.", call. = FALSE)
  index_vcov(object, scale = scale, years = years, require_pd = require_pd)
}
