# Fixed offsets are not estimated terms. The compact engine omits them from
# its reference linear predictor, which is harmless only when that reference
# cancels algebraically (ordinary log ratios or identity differences).
.influ_has_offset_formula <- function(formulas) {
  if (is.null(formulas)) return(FALSE)
  if (inherits(formulas, "formula")) {
    has_call <- function(x) {
      if (!is.call(x)) return(FALSE)
      head <- x[[1L]]
      name <- if (is.symbol(head)) as.character(head) else {
        if (is.call(head) && as.character(head[[1L]]) %in% c("::", ":::")) {
          as.character(head[[3L]])
        } else ""
      }
      name %in% c("offset", "rate", "resp_rate") ||
        any(vapply(as.list(x)[-1L], has_call, logical(1)))
    }
    return(has_call(formulas))
  }
  if (is.list(formulas)) {
    return(any(vapply(formulas, .influ_has_offset_formula, logical(1))))
  }
  FALSE
}

.influ_has_offset_value <- function(offset) {
  !is.null(offset) && length(offset) > 0L &&
    (!is.numeric(offset) || anyNA(offset) || any(offset != 0))
}

.influ_offset_sources <- function(formulas = NULL, call = NULL,
                                   stored = NULL) {
  sources <- character()
  if (.influ_has_offset_formula(formulas)) sources <- c(sources, "formula")
  if (!is.null(call) && !is.null(call$offset)) sources <- c(sources, "call")
  if (is.list(stored)) {
    has_stored <- any(vapply(stored, .influ_has_offset_value, logical(1)))
  } else {
    has_stored <- .influ_has_offset_value(stored)
  }
  if (has_stored) sources <- c(sources, "stored offset")
  sources
}

.check_influ_offset_scope <- function(family_spec, sources) {
  if (!length(sources)) return(invisible(sources))
  invariant <- family_spec$structure == "single" &&
    (family_spec$link == "identity" ||
       (family_spec$link == "log" && family_spec$natural_scale == "ratio"))
  if (!invariant) {
    stop(
      "Offsets/exposure are currently supported only for single-component ",
      "log-link ratios or identity-link contrasts. Nonlinear probability ",
      "and hurdle/zero-inflated diagnostics require an offset-aware ",
      "reference calculation and are not yet supported.",
      call. = FALSE
    )
  }
  invisible(sources)
}

.check_influ_lognormal_scale <- function(family_spec, scale_formula) {
  if (family_spec$family != "lognormal" || is.null(scale_formula)) {
    return(invisible(NULL))
  }
  constant <- inherits(scale_formula, "formula") &&
    !length(all.vars(scale_formula[[length(scale_formula)]]))
  if (!constant) {
    stop(
      "Lognormal models with varying sigma/dispersion are not yet supported. ",
      "Use constant sigma: its arithmetic-mean correction cancels in ratios, ",
      "whereas varying scale needs a joint location-and-scale calculation.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

.check_influ_lognormal_mean_link <- function(family_spec) {
  if (family_spec$family == "lognormal" && family_spec$link != "log") {
    stop(
      "Lognormal diagnostics for ", family_spec$backend,
      " currently require a log link for the arithmetic mean. ",
      "Other links do not produce exp(contrast) mean ratios.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

.record_influ_offset_scope <- function(diagnostic, sources) {
  if (!length(sources)) return(diagnostic)
  diagnostic$metadata$offset <- list(
    detected = TRUE,
    sources = sources,
    scope = "Estimated-term contrasts with fixed offset held unchanged; no offset contribution is estimated."
  )
  diagnostic$metadata$nominal_scope <- paste(
    "Nominal summaries describe the observed response, not exposure-adjusted CPUE.",
    "No response/exposure division is performed."
  )
  diagnostic$metadata$notes <- c(
    diagnostic$metadata$notes,
    diagnostic$metadata$offset$scope,
    diagnostic$metadata$nominal_scope
  )
  diagnostic
}
