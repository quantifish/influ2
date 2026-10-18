documentation_resource_source <- function() {
  candidates <- c(test_path("..", "..", "vignettes", "article-resources.R"),
    system.file("doc", "article-resources.R", package = "influ2"),
    file.path(Sys.getenv("GITHUB_WORKSPACE", unset = ""),
      "vignettes", "article-resources.R"))
  path <- candidates[nzchar(candidates) & file.exists(candidates)][1L]
  if (is.na(path)) skip("The article resource helper is unavailable")
  path
}

test_that("every article keeps executable source and portable external figures", {
  directory <- dirname(documentation_resource_source())
  articles <- list.files(directory, pattern = "[.]Rmd$", full.names = TRUE)
  expect_length(articles, 12L)
  for (article in articles) {
    lines <- readLines(article, warn = FALSE)
    expect_true(any(grepl("%\\VignetteEngine{knitr::rmarkdown_notangle}",
      lines, fixed = TRUE)), info = basename(article))
    expect_true(any(trimws(lines) == "self_contained: false"),
      info = basename(article))
    expect_true(any(trimws(lines) == "highlight: default"),
      info = basename(article))
    expect_true(any(trimws(lines) == "css: []"), info = basename(article))
    expect_true(any(trimws(lines) == "lib_dir: libs"), info = basename(article))
    expect_true(any(lines == 'source("article-resources.R", local = TRUE)'),
      info = basename(article))
    expect_true(any(startsWith(lines, "```{r ")), info = basename(article))
  }
})

test_that("the build installs supporting directories without flattening them", {
  source <- test_path("..", "..", "vignettes")
  extras <- file.path(source, ".install_extras")
  if (!file.exists(extras)) skip("The source-only vignette build rules are unavailable")
  rules <- readLines(extras, warn = FALSE)
  included <- function(path) any(vapply(rules, function(rule) grepl(rule, path),
    logical(1)))
  expect_true(included("vignettes/influ2_files"))
  expect_true(included("vignettes/residual-diagnostics_files"))
  expect_true(included("vignettes/libs"))
  expect_true(included("vignettes/article-resources.R"))
  expect_true(included("vignettes/example-models.R"))
  expect_false(included("vignettes/influ2_files/figure-html/example-1.png"))
  expect_false(included("vignettes/libs/header-attrs-2.32/header-attrs.js"))
  expect_true(included("vignettes/references.bib"))
  makefile <- readLines(file.path(source, "Makefile"), warn = FALSE)
  expect_true(any(makefile == "all:"))
})

test_that("the standard vignette stylesheet never changes pkgdown styling", {
  skip_if_not_installed("knitr")
  skip_if_not_installed("htmltools")
  skip_if_not_installed("rmarkdown")
  previous <- knitr::knit_meta(clean = TRUE)
  old_options <- knitr::opts_knit$get()
  old_chunks <- knitr::opts_chunk$get()
  withr::defer({
    knitr::knit_meta(clean = TRUE)
    knitr::knit_meta_add(previous)
    knitr::opts_knit$restore(old_options)
    knitr::opts_chunk$restore(old_chunks)
  })
  knitr::opts_knit$set(rmarkdown.pandoc.to = "html")
  path <- documentation_resource_source()
  withr::with_envvar(c(IN_PKGDOWN = "true"),
    sys.source(path, envir = new.env(parent = baseenv())))
  expect_length(knitr::knit_meta(clean = TRUE), 0L)
  withr::with_envvar(c(IN_PKGDOWN = "false"),
    sys.source(path, envir = new.env(parent = baseenv())))
  dependencies <- knitr::knit_meta(clean = TRUE)
  expect_length(dependencies, 1L)
  expect_s3_class(dependencies[[1L]], "html_dependency")
  expect_identical(dependencies[[1L]]$name, "influ2-vignette-css")
  expect_identical(dependencies[[1L]]$stylesheet, "vignette.css")
  expect_false(dependencies[[1L]]$all_files)
  expect_true(file.exists(file.path(dependencies[[1L]]$src$file, "vignette.css")))
})

test_that("the source archive excludes only redundant generated article assets", {
  ignore <- test_path("..", "..", ".Rbuildignore")
  if (!file.exists(ignore)) skip("The source-only archive rules are unavailable")
  rules <- readLines(ignore, warn = FALSE)
  excluded <- function(path) any(vapply(rules, function(rule) grepl(rule, path),
    logical(1)))
  expect_true(excluded("vignettes/influ2_files"))
  expect_true(excluded("vignettes/influ2_files/figure-html/cdi-1.png"))
  expect_true(excluded("vignettes/libs/header-attrs-2.32/header-attrs.js"))
  expect_false(excluded("inst/doc/influ2_files/figure-html/cdi-1.png"))
  expect_false(excluded("inst/doc/libs/header-attrs-2.32/header-attrs.js"))
  expect_false(excluded("vignettes/influ2.Rmd"))
  expect_false(excluded("vignettes/article-resources.R"))
  expect_false(excluded("vignettes/example-models.R"))
  expect_false(excluded("vignettes/references.bib"))
})

test_that("rendered external assets work without the author's R library path", {
  skip_if_not_installed("knitr")
  skip_if_not_installed("htmltools")
  skip_if_not_installed("rmarkdown")
  if (!rmarkdown::pandoc_available()) skip("Pandoc is unavailable")
  directory <- tempfile("influ2-article-assets-")
  dir.create(directory)
  withr::defer(unlink(directory, recursive = TRUE))
  withr::local_envvar(IN_PKGDOWN = "false")
  expect_true(file.copy(documentation_resource_source(), directory))
  article <- file.path(directory, "portable.Rmd")
  writeLines(c(
    "---", 'title: "Portable article"',
    "output:", "  rmarkdown::html_vignette:", "    self_contained: false",
    "    highlight: default",
    "    css: []", "    lib_dir: libs", "vignette: >",
    "  %\\VignetteIndexEntry{Portable article}",
    "  %\\VignetteEncoding{UTF-8}",
    "  %\\VignetteEngine{knitr::rmarkdown_notangle}", "---", "",
    "```{r setup, include=FALSE}",
    'source("article-resources.R", local = TRUE)', "```", "",
    '```{r portable-plot, fig.cap="The complete figure caption."}',
    "plot(1:5)", "```"
  ), article)
  output <- rmarkdown::render(article, envir = new.env(parent = globalenv()),
    quiet = TRUE)
  html <- paste(readLines(output, warn = FALSE), collapse = "\n")
  expect_match(html, "The complete figure caption.", fixed = TRUE)
  expect_match(html, "portable-plot-1.png", fixed = TRUE)
  expect_match(html, "portable_files/f/portable-plot-1.png", fixed = TRUE)
  expect_false(grepl("data:image/png;base64,", html, fixed = TRUE))
  expect_false(grepl(system.file(package = "rmarkdown"), html, fixed = TRUE))
  references <- regmatches(html, gregexpr('(src|href)="[^"#]+"', html))[[1L]]
  references <- sub('^(src|href)="', "", sub('"$', "", references))
  local <- references[!grepl("^([a-zA-Z]+:|//)", references)]
  expect_true(length(local) >= 3L)
  expect_false(any(startsWith(local, "/")))
  expect_true(all(file.exists(file.path(directory, local))))
  expect_true(any(grepl("libs/highlightjs-", local, fixed = TRUE)))
  expect_length(list.files(file.path(directory, "libs"),
    pattern = "vignette[.]css$", recursive = TRUE), 1L)
  # The timing tool and users may put rendered HTML outside the source tree.
  elsewhere <- file.path(directory, "elsewhere")
  dir.create(elsewhere)
  moved <- rmarkdown::render(article, output_dir = elsewhere,
    envir = new.env(parent = globalenv()), quiet = TRUE)
  moved_html <- paste(readLines(moved, warn = FALSE), collapse = "\n")
  moved_sources <- regmatches(moved_html, gregexpr('src="[^"]+"', moved_html))[[1L]]
  moved_sources <- sub('^src="', "", sub('"$', "", moved_sources))
  moved_sources <- moved_sources[!grepl("^([a-zA-Z]+:|//)", moved_sources)]
  expect_true(all(file.exists(file.path(elsewhere, moved_sources))))
  expect_true(any(grepl("portable_files/f/portable-plot-1.png", moved_sources,
    fixed = TRUE)))
})
