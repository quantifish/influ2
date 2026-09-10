# Run Rscript tools/branding/logo.R from the repository root.
# Development-only dependencies: hexSticker, ggplot2, ragg, and png.
# This is never run during installation, checking, or website deployment.
# Deliberately overwrites the package logo and its website icon derivatives.
# Restore the original hexSticker recipe, hiding only the axis numbers.
stopifnot(file.exists("DESCRIPTION"), dir.exists("pkgdown/favicon"))
stopifnot(requireNamespace("ragg", quietly = TRUE),
          requireNamespace("png", quietly = TRUE),
          requireNamespace("hexSticker", quietly = TRUE))
library(ggplot2)

make_logo <- function(filename = "man/figures/logo.png", axis_numbers = FALSE) {
  bubbles <- stats::aggregate(
    list(n = rep.int(1L, nrow(datasets::iris))),
    list(width = round(datasets::iris$Sepal.Width),
         length = round(datasets::iris$Sepal.Length)), sum
  )
  # Freeze the original plot_bubble styling, independently of the public API.
  p <- ggplot(bubbles, aes(factor(width), factor(length), size = n)) +
    geom_point(alpha = 0.5, shape = 16, colour = "orange") +
    geom_point(shape = 1, colour = "orange") +
    labs(x = NULL, y = NULL, size = NULL) +
    theme_bw() + scale_size(range = c(0, 10)) +
    theme(legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1),
      # Match the original fine dotted grid under the current line renderer.
      panel.grid = element_line(colour = "black", linewidth = 0.2, linetype = "17"),
      panel.border = element_rect(linewidth = 0.6),
      panel.background = element_rect(fill = "transparent", colour = "transparent"),
      plot.background = element_rect(fill = "transparent", colour = "transparent"))
  # Invisible text retains its layout space, keeping ticks and plot geometry.
  if (!axis_numbers) p <- p + theme(axis.text = element_text(colour = "transparent"))
  sticker <- hexSticker::sticker(p, package = "influ2",
    h_color = "orange", h_fill = "#1881C2",
    p_y = 1.6, p_size = 20, p_color = "orange", p_family = "Aller_Rg",
    s_x = 0.925, s_y = 0.81, s_width = 1.35, s_height = 1.38,
    filename = filename
  )
  # Current ggplot2/hexSticker margins differ from the historical renderer.
  # Keep the original 518 x 600 artwork bounds and panel placement explicit.
  sticker <- sticker + theme(plot.margin = margin(0, 0, 0, 0))
  xscale <- sticker$scales$get_scales("x")
  yscale <- sticker$scales$get_scales("y")
  xscale$limits <- 1 + c(-1, 1) * sqrt(3) / 2 * 1.02
  yscale$limits <- c(-0.02, 2.02)
  hexSticker::save_sticker(filename, sticker, dpi = 300)
  invisible(sticker)
}

make_icons <- function() {
  logo <- png::readPNG("man/figures/logo.png")
  draw_icon <- function(path, size) {
    ragg::agg_png(path, width = size, height = size, units = "px",
                  background = "transparent")
    on.exit(grDevices::dev.off(), add = TRUE)
    grid::grid.newpage()
    grid::grid.raster(logo, width = dim(logo)[2] / dim(logo)[1], height = 1, interpolate = TRUE)
  }
  for (size in c(60, 76, 120, 152, 180)) {
    draw_icon(sprintf("pkgdown/favicon/apple-touch-icon-%sx%s.png", size, size), size)
  }
  draw_icon("pkgdown/favicon/apple-touch-icon.png", 180)
  for (size in c(16, 32)) {
    draw_icon(sprintf("pkgdown/favicon/favicon-%sx%s.png", size, size), size)
  }

  # ICO directory with two PNG-compressed entries; no external image service.
  sizes <- c(16L, 32L)
  images <- lapply(sizes, function(size) {
    path <- sprintf("pkgdown/favicon/favicon-%sx%s.png", size, size)
    readBin(path, "raw", n = file.info(path)$size)
  })
  le16 <- function(x) writeBin(as.integer(x), raw(), size = 2L, endian = "little")
  le32 <- function(x) writeBin(as.integer(x), raw(), size = 4L, endian = "little")
  directory <- c(le16(0), le16(1), le16(length(images)))
  offset <- 6L + 16L * length(images)
  for (i in seq_along(images)) {
    directory <- c(directory, as.raw(c(sizes[i], sizes[i], 0, 0)),
                   le16(1), le16(32), le32(length(images[[i]])), le32(offset))
    offset <- offset + length(images[[i]])
  }
  writeBin(c(directory, do.call(c, images)), "pkgdown/favicon/favicon.ico")
}

make_logo()
make_icons()
