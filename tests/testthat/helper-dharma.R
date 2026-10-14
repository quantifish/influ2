skip_if_dharma_equivalent_unvalidated <- function() {
  testthat::skip_if_not_installed("DHARMa", "0.4.7")
  testthat::skip_if(!.resid_dharma_stream_version() %in% c("0.4.7", "0.5.0"),
    "Bounded-memory DHARMa-equivalent parity is not validated for this version")
}
