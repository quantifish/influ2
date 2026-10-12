#!/usr/bin/env Rscript
# Developer-only timing of fully executed articles; no cached model results.
args <- commandArgs(trailingOnly = TRUE)
root <- normalizePath(if (length(args)) args[1L] else ".", mustWork = TRUE)
destination <- if (length(args) > 1L) args[2L] else tempfile("influ2-vignette-profile-")
dir.create(destination, recursive = TRUE, showWarnings = FALSE)
destination <- normalizePath(destination, mustWork = TRUE)
articles <- if (length(args) > 2L) args[-c(1L, 2L)] else
  sub("\\.Rmd$", "", basename(list.files(file.path(root, "vignettes"),
    pattern = "\\.Rmd$", full.names = TRUE)))
pkgload::load_all(root, quiet = TRUE, helpers = FALSE)
timings <- list()
starts <- new.env(parent = emptyenv())
knitr::opts_chunk$set(profile = TRUE)
knitr::knit_hooks$set(profile = function(before, options, envir) {
  if (before) {
    starts[[options$label]] <- proc.time()
  } else {
    elapsed <- proc.time() - starts[[options$label]]
    timings[[length(timings) + 1L]] <<- data.frame(
      article = current_article, chunk = options$label,
      user = unname(elapsed[1L]), system = unname(elapsed[2L]),
      elapsed = unname(elapsed[3L]))
    cat(sprintf("%s / %s: %.3fs elapsed\n", current_article,
      options$label, unname(elapsed[3L])))
    flush.console()
  }
  invisible(NULL)
})
totals <- lapply(articles, function(article) {
  current_article <<- article
  out <- file.path(destination, article)
  dir.create(out, showWarnings = FALSE)
  before <- proc.time()
  cat("Rendering", article, "with all its existing executed examples\n")
  flush.console()
  rmarkdown::render(file.path(root, "vignettes", paste0(article, ".Rmd")),
    output_dir = out, intermediates_dir = out,
    envir = new.env(parent = globalenv()), quiet = TRUE)
  elapsed <- proc.time() - before
  data.frame(article = article, user = unname(elapsed[1L]),
    system = unname(elapsed[2L]), elapsed = unname(elapsed[3L]))
})
chunks <- do.call(rbind, timings)
totals <- do.call(rbind, totals)
write.csv(chunks, file.path(destination, "chunks.csv"), row.names = FALSE)
write.csv(totals, file.path(destination, "articles.csv"), row.names = FALSE)
print(totals, row.names = FALSE)
cat("Total elapsed article seconds:", sum(totals$elapsed), "\n")
cat("Largest chunks:\n")
print(head(chunks[order(chunks$elapsed, decreasing = TRUE), ], 20L), row.names = FALSE)
cat("Outputs:", destination, "\n")
