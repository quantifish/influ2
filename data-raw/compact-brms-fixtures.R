# Compact the development BRMS fixtures used by tests and vignettes.
#
# A brmsfit serialises its Stan dynamic-library state, which is unnecessary for
# influence diagnostics and makes otherwise small fixtures tens of megabytes.
# The package backend recognises `influ2_draws`; `brms::standata()` continues to
# reconstruct design matrices from the retained formula, data, and basis.

compact_brms_fixture <- function(path, ndraws = 200L) {
  model <- readRDS(path)
  draws <- posterior::as_draws_matrix(model)
  keep <- unique(round(seq(1, nrow(draws), length.out = min(ndraws, nrow(draws)))))
  model$influ2_draws <- draws[keep, , drop = FALSE]
  model$fit <- NULL
  model$criteria <- list()
  saveRDS(model, path, compress = "xz")
  invisible(path)
}

compact_brms_fixture("inst/extdata/brms-fixtures/fit2.rds")
compact_brms_fixture("inst/extdata/brms-fixtures/m1.rds")
