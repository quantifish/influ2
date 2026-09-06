.cdi_level_groups <- function(data, term, contribution, weights, bins = 20L) {
  value <- .term_level_values(data, term, contribution, bins)
  keep <- weights > 0 & !is.na(value) & is.finite(contribution)
  split(which(keep), factor(value[keep], levels = unique(value[keep])))
}

# Keep uncertainty on compact term-level contrasts, including the uncertainty
# in their common centre. Posterior transformations precede summarisation.
.cdi_summary_row <- function(term, level, estimate, centred_estimate,
                             std_error = NA_real_, centred_std_error = NA_real_,
                             draws = NULL, centred_draws = NULL,
                             family_spec, method, probs) {
  summarise <- function(estimate, se, draws) {
    if (!is.null(draws) && method != "none") {
      return(.summarise_vector(draws, probs))
    }
    c(estimate = estimate, std_error = se,
      lower = estimate + stats::qnorm(probs[1]) * se,
      upper = estimate + stats::qnorm(probs[2]) * se)
  }
  raw <- summarise(estimate, std_error, draws)
  centred <- summarise(centred_estimate, centred_std_error, centred_draws)
  ratio <- identical(family_spec$natural_scale, "ratio")
  relative <- if (ratio) {
    if (!is.null(centred_draws) && method != "none") {
      .summarise_vector(exp(centred_draws), probs)
    } else {
      c(estimate = exp(centred["estimate"]),
        std_error = exp(centred["estimate"]) * centred["std_error"],
        lower = exp(centred["lower"]), upper = exp(centred["upper"]))
    }
  } else {
    centred
  }
  # Strip names before prefixing: named inputs would otherwise create names
  # such as estimate.estimate in the analytic transformation above.
  names(relative) <- names(centred)
  data.frame(
    term = term, level = as.character(level),
    as.list(raw),
    as.list(stats::setNames(centred, paste0("centred_", names(centred)))),
    as.list(stats::setNames(relative, paste0("relative_", names(relative)))),
    method = method,
    link = family_spec$link %||% NA_character_,
    cdi_scale = if (ratio) "ratio" else "link",
    complement = isTRUE(family_spec$complement),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}
