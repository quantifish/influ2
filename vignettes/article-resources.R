# Keep installed HTML vignettes portable without embedding every PNG in base64.
# The executable Rmd sources remain alongside their figures; the notangle
# engine omits only the redundant, extracted R scripts.
influ2_article_resources <- function() {
  # pkgdown supplies its own stylesheet, figure handling, and lightbox assets.
  if (identical(Sys.getenv("IN_PKGDOWN"), "true") || !knitr::is_html_output()) {
    return(invisible(NULL))
  }
  # External images become archive members. Shorten the generated directory,
  # not the chunk labels or figures, to keep their paths portable in tar files.
  path <- knitr::opts_chunk$get("fig.path")
  if (is.character(path) && length(path) == 1L &&
      grepl("figure-html/$", path)) {
    # Preserve rmarkdown's prefix when output_dir differs from the input dir.
    knitr::opts_chunk$set(fig.path = sub("figure-html/$", "f/", path))
  }
  css <- system.file("rmarkdown/templates/html_vignette/resources/vignette.css",
    package = "rmarkdown", mustWork = TRUE)
  knitr::knit_meta_add(list(htmltools::htmlDependency(
    name = "influ2-vignette-css",
    version = as.character(utils::packageVersion("rmarkdown")),
    src = dirname(css), stylesheet = basename(css), all_files = FALSE
  )))
  invisible(NULL)
}
influ2_article_resources()
