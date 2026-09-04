#' Families supported by the influence engine
#'
#' `influ_families()` describes the first set of response distributions that
#' can be represented by an [influ_diag] object. Model backends do not
#' necessarily implement every family and component immediately; unsupported
#' combinations fail before any calculations are attempted.
#'
#' @return A data frame containing canonical family names, aliases, and the
#'   default influence scale.
#' @examples
#' influ_families()
#' @export
influ_families <- function() {
  data.frame(
    family = c(
      "gaussian", "binomial", "poisson", "negative_binomial",
      "lognormal", "gamma", "tweedie"
    ),
    aliases = c(
      "normal",
      "bernoulli",
      "poisson",
      "negbinomial, nbinom1, nbinom2",
      "lognormal",
      "Gamma",
      "Tweedie"
    ),
    default_scale = c(
      "difference", "difference", "ratio", "ratio", "ratio", "ratio", "ratio"
    ),
    stringsAsFactors = FALSE
  )
}

.normalise_family_name <- function(family) {
  if (length(family) != 1L || is.na(family) || !nzchar(family)) {
    stop("A single, non-missing family name is required.", call. = FALSE)
  }

  original <- family
  family <- tolower(gsub("[^a-zA-Z0-9]+", "_", family))
  family <- gsub("^_|_$", "", family)

  structure <- "single"
  base <- family

  if (grepl("^(hurdle|delta)_", family)) {
    structure <- "hurdle"
    base <- sub("^(hurdle|delta)_", "", family)
  } else if (grepl("^(zero_inflated|zero_inflation|zi)_", family)) {
    structure <- "zero_inflated"
    base <- sub("^(zero_inflated|zero_inflation|zi)_", "", family)
  }

  base <- sub("^truncated_", "", base)
  if (grepl("tweedie", base)) base <- "tweedie"
  if (grepl("negative.*binomial|negbin|nbinom1|nbinom2", base)) {
    base <- "negative_binomial"
  }

  aliases <- c(
    normal = "gaussian",
    gaussian = "gaussian",
    bernoulli = "binomial",
    binomial = "binomial",
    poisson = "poisson",
    negbinomial = "negative_binomial",
    negativebinomial = "negative_binomial",
    negative_binomial = "negative_binomial",
    nbinom = "negative_binomial",
    nbinom1 = "negative_binomial",
    nbinom2 = "negative_binomial",
    lognormal = "lognormal",
    log_normal = "lognormal",
    gamma = "gamma",
    tweedie = "tweedie"
  )

  canonical <- unname(aliases[base])
  if (length(canonical) == 0L || is.na(canonical)) {
    stop(
      "Unsupported family '", original, "'. Supported base families are: ",
      paste(influ_families()$family, collapse = ", "), ".",
      call. = FALSE
    )
  }

  list(
    original = original,
    family = canonical,
    structure = structure
  )
}

.new_family_spec <- function(family, link, backend = NA_character_,
                             response_structure = NULL,
                             complement = FALSE) {
  parsed <- .normalise_family_name(family)
  if (!is.null(response_structure)) parsed$structure <- response_structure

  link <- tolower(link %||% .default_link(parsed$family))
  supported_links <- c("identity", "log", "logit", "probit", "cloglog")
  if (!link %in% supported_links) {
    stop(
      "Unsupported link '", link, "'. Supported links are: ",
      paste(supported_links, collapse = ", "), ".",
      call. = FALSE
    )
  }

  if (parsed$structure == "hurdle" &&
      !parsed$family %in% c("binomial", "gamma", "lognormal", "poisson", "negative_binomial")) {
    stop(
      "Hurdle/delta support is currently limited to gamma, lognormal, ",
      "Poisson, negative-binomial, and binomial components.",
      call. = FALSE
    )
  }

  if (parsed$structure == "zero_inflated" &&
      !parsed$family %in% c("binomial", "poisson", "negative_binomial")) {
    stop(
      "Zero-inflated support is currently limited to Poisson and ",
      "negative-binomial count components.",
      call. = FALSE
    )
  }

  natural_scale <- if (identical(link, "log") ||
    identical(parsed$family, "lognormal")) "ratio" else "difference"

  structure(
    list(
      family = parsed$family,
      original_family = parsed$original,
      link = link,
      structure = parsed$structure,
      backend = backend,
      natural_scale = natural_scale,
      complement = isTRUE(complement)
    ),
    class = "influ_family_spec"
  )
}

.default_link <- function(family) {
  switch(
    family,
    gaussian = "identity",
    binomial = "logit",
    poisson = "log",
    negative_binomial = "log",
    lognormal = "identity",
    gamma = "log",
    tweedie = "log",
    "identity"
  )
}

.link_inverse <- function(eta, link) {
  switch(
    link,
    identity = eta,
    log = exp(eta),
    logit = stats::plogis(eta),
    probit = stats::pnorm(eta),
    cloglog = 1 - exp(-exp(eta)),
    stop("No inverse-link implementation for '", link, "'.", call. = FALSE)
  )
}

.effect_transform <- function(delta, family_spec, eta_reference = 0) {
  if (identical(family_spec$natural_scale, "ratio")) {
    return(exp(delta))
  }

  out <- .link_inverse(eta_reference + delta, family_spec$link) -
    .link_inverse(eta_reference, family_spec$link)
  if (isTRUE(family_spec$complement)) -out else out
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}
