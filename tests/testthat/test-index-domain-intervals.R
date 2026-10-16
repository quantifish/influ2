test_that("domain plots preserve pointwise intervals at isolated annual positions", {
  d <- data.frame(Year = c("2001", "2002", "2004", "2006"),
    Series = "Full index", Mean = c(2, 3, 4, 5),
    Qlower = c(1, 2, 3, 4), Qupper = c(3, 4, 5, 6))
  p <- .domain_series_plot(d, "Mean", "Series", "Qlower", "Qupper")
  expect_identical(p$data$.isolated_interval, c(FALSE, FALSE, TRUE, TRUE))
  bars <- Filter(function(x) inherits(x$geom, "GeomLinerange"), p$layers)
  expect_length(bars, 1L)
  expect_identical(as.character(bars[[1L]]$data$Year), c("2004", "2006"))
  built <- ggplot2::ggplot_build(p)$data
  expect_equal(utils::tail(built, 1L)[[1L]]$ymin, c(3, 4))
  expect_equal(utils::tail(built, 1L)[[1L]]$ymax, c(5, 6))
  without <- .domain_series_plot(d, "Mean", "Series", "Qlower", "Qupper", FALSE)
  expect_false(any(vapply(without$layers,
    function(x) inherits(x$geom, "GeomLinerange"), logical(1))))
  d$Qlower[3] <- NA_real_
  changed <- .domain_series_plot(d, "Mean", "Series", "Qlower", "Qupper")
  expect_identical(changed$data$.isolated_interval, c(FALSE, FALSE, FALSE, TRUE))
  d$Year <- c("early", "middle", "late", "final")
  expect_s3_class(.domain_series_plot(d, "Mean", "Series"), "ggplot")
})

test_that("omission scenarios retain colours between index and paired-ratio panels", {
  index <- structure(list(table = data.frame(Year = 2001:2003, Mean = 1:3,
    Qlower = (1:3) / 2, Qupper = (1:3) * 2),
    metadata = list(method = "standardised")), class = "influ_index")
  x <- structure(list(baseline = index, omission = list(spatial = index),
    omission_summary = data.frame(Year = 2001:2003, series = "spatial",
      ratio = 1, ratio_lower = 0.8, ratio_upper = 1.2)), class = "influ_domain")
  p <- plot(x, type = "omission")
  upper <- ggplot2::ggplot_build(p[[1L]])$plot$scales
  lower <- ggplot2::ggplot_build(p[[2L]])$plot$scales
  for (aesthetic in c("colour", "fill")) {
    expect_identical(upper$get_scales(aesthetic)$map("spatial"),
      lower$get_scales(aesthetic)$map("spatial"))
  }
})
