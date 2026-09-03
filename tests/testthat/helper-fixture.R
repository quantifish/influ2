bentley_fixture <- function() {
  data_file <- system.file(
    "extdata", "bentley-poisson-data.csv", package = "influ2"
  )
  reference_file <- system.file(
    "extdata", "bentley-poisson-reference.csv", package = "influ2"
  )
  data <- utils::read.csv(
    data_file,
    colClasses = c(year = "factor", area = "factor", vessel = "factor")
  )
  list(
    data = data,
    reference = utils::read.csv(reference_file),
    model = stats::glm(
      catch ~ year + area + vessel,
      family = stats::poisson(link = "log"),
      data = data
    )
  )
}
