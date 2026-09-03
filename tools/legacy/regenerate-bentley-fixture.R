# Regenerate the frozen Bentley parity fixture.
# Run from the influ2 package root; requires the suggested package `proto`.

if (!requireNamespace("proto", quietly = TRUE)) {
  stop("Package 'proto' is required to regenerate the legacy fixture.")
}

source("tools/legacy/influ-proto.R", local = TRUE)

data <- utils::read.csv(
  "inst/extdata/bentley-poisson-data.csv",
  colClasses = c(year = "factor", area = "factor", vessel = "factor")
)
fit <- stats::glm(
  catch ~ year + area + vessel,
  family = stats::poisson(link = "log"),
  data = data
)

legacy <- Influence$new(
  fit,
  data = data,
  response = "catch",
  focus = "year"
)
legacy$calc()

reference <- reshape(
  legacy$influences,
  varying = names(legacy$influences)[-1],
  v.names = "link_influence",
  timevar = "term",
  times = names(legacy$influences)[-1],
  direction = "long"
)
reference <- reference[c("level", "term", "link_influence")]
reference$focus <- "year"
reference$natural_influence <- exp(reference$link_influence)
reference <- reference[c(
  "focus", "level", "term", "link_influence", "natural_influence"
)]
rownames(reference) <- NULL

utils::write.csv(
  reference,
  "inst/extdata/bentley-poisson-reference.csv",
  row.names = FALSE
)
