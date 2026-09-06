test_that("core influence figures remain visually stable", {
  skip_if_not_installed("vdiffr")
  diagnostic <- influ(bentley_fixture()$model, focus = "year")

  vdiffr::expect_doppelganger(
    "model-neutral influence plot",
    plot(diagnostic, type = "influence")
  )
  vdiffr::expect_doppelganger(
    "nominal and standardised index plot",
    plot(diagnostic, type = "index")
  )
  vdiffr::expect_doppelganger(
    "coefficient distribution influence plot",
    plot(diagnostic, type = "cdi", term = "area")
  )
  vdiffr::expect_doppelganger(
    "model coded CDI plot",
    plot(diagnostic, type = "cdi", term = "area",
      coefficient_reference = "model")
  )
  vdiffr::expect_doppelganger(
    "purple bubble plot",
    plot_bubble(
      bentley_fixture()$data,
      group = c("year", "area"),
      fill = "purple4"
    )
  )
  vdiffr::expect_doppelganger(
    "model comparison plot",
    plot_compare(
      list(
        diagnostic,
        influ(stats::glm(
          catch ~ year + area,
          family = stats::poisson(link = "log"),
          data = bentley_fixture()$data
        ), focus = "year")
      ),
      labels = c("Full", "Reduced")
    )
  )
  vdiffr::expect_doppelganger(
    "implied residual coefficient plot",
    plot_implied_residuals(
      bentley_fixture()$model,
      year = "year",
      groups = "area",
      min_n = 1
    )
  )
})
