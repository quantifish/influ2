# Run after pkgdown builds: Rscript tools/check-figure-captions.R
# Developer-only dependencies are already installed with pkgdown/rmarkdown.
stopifnot(requireNamespace("xml2", quietly = TRUE), requireNamespace("jsonlite", quietly = TRUE))
node <- Sys.which("node")
if (!nzchar(node)) stop("Node.js is required for the site caption check.")
files <- list.files("docs", pattern = "\\.html$", recursive = TRUE, full.names = TRUE)
if (!length(files)) stop("Build the local pkgdown site before checking captions.")
figure_class <- "contains(concat(' ', normalize-space(@class), ' '), ' figure ')"
plot_class <- "contains(concat(' ', normalize-space(@class), ' '), ' r-plt ')"
caption_class <- "contains(concat(' ', normalize-space(@class), ' '), ' caption ')"
image_record <- function(node) list(
  alt = if (is.na(xml2::xml_attr(node, "alt"))) "" else xml2::xml_attr(node, "alt"),
  src = xml2::xml_attr(node, "src"))
pages <- lapply(files, function(file) {
  doc <- xml2::read_html(file)
  images <- xml2::xml_find_all(doc, paste0("//main//img[", plot_class, "]"))
  if (!length(images)) return(NULL)
  for (image in images) {
    caption <- xml2::xml_find_first(image, paste0("ancestor::*[", figure_class,
      "]//*[", caption_class, " or self::figcaption]"))
    if (inherits(caption, "xml_missing") || !nzchar(trimws(xml2::xml_text(caption)))) {
      stop("Missing visible figure caption in ", file, ": ", xml2::xml_attr(image, "src"))
    }
  }
  figures <- xml2::xml_find_all(doc, paste0("//main//*[", figure_class, "]"))
  records <- lapply(figures, function(figure) {
    caption <- xml2::xml_find_first(figure, paste0(".//*[", caption_class, "] | .//figcaption"))
    list(caption = if (inherits(caption, "xml_missing")) NULL else trimws(xml2::xml_text(caption)),
      images = lapply(xml2::xml_find_all(figure, paste0(".//img[", plot_class, "]")), image_record))
  })
  loose <- xml2::xml_find_all(doc, paste0("//main//img[", plot_class,
    " and not(ancestor::*[", figure_class, "])]"))
  if (!length(xml2::xml_find_all(doc, "//script[contains(@src, 'extra.js')]"))) {
    stop("No shared lightbox script on ", file)
  }
  list(file = file, figures = records, loose_images = lapply(loose, image_record))
})
pages <- Filter(Negate(is.null), pages)
fixture <- tempfile(fileext = ".json")
jsonlite::write_json(pages, fixture, auto_unbox = TRUE, null = "null")
status <- system2(node, c("tools/tests/lightbox.test.cjs", shQuote(fixture)))
unlink(fixture)
if (status != 0L) stop("Lightbox caption checks failed.")
