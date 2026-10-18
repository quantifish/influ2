# The article cache is documentation infrastructure, not a runtime export.
source_article_models <- function(envir) {
  candidates <- c(test_path("..", "..", "vignettes", "example-models.R"),
    system.file("doc", "example-models.R", package = "influ2"),
    file.path(Sys.getenv("GITHUB_WORKSPACE", unset = ""),
      "vignettes", "example-models.R"))
  path <- candidates[nzchar(candidates) & file.exists(candidates)][1L]
  if (is.na(path)) skip("The worked-article model helper is unavailable")
  sys.source(path, envir = envir)
  invisible(path)
}
