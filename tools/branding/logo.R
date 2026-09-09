# Run Rscript tools/branding/logo.R from the repository root.
# Development-only dependencies: ragg and png (plus R's grid package).
# This is never run during installation, checking, or website deployment.
# Deliberately overwrites the package logo and its website icon derivatives.
# The original iris bubble motif, blue/orange palette, and hexagon are kept;
# the miniature axes, labels, plot frame, and dotted grid are removed.
stopifnot(file.exists("DESCRIPTION"), dir.exists("pkgdown/favicon"))
stopifnot(requireNamespace("ragg", quietly = TRUE),
          requireNamespace("png", quietly = TRUE))

make_logo <- function() {
  orange <- "#FFA500"
  blue <- "#1881C2"
  bubbles <- stats::aggregate(
    list(n = rep.int(1L, nrow(datasets::iris))),
    list(width = round(datasets::iris$Sepal.Width),
         length = round(datasets::iris$Sepal.Length)), sum
  )
  # Preserve the original plot's square-root area scale and bubble positions.
  radius <- 47 * sqrt((bubbles$n - min(bubbles$n)) /
                        (max(bubbles$n) - min(bubbles$n)))
  ragg::agg_png("man/figures/logo.png", width = 520, height = 600,
                units = "px", res = 72, background = "transparent")
  on.exit(grDevices::dev.off(), add = TRUE)
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(xscale = c(0, 520), yscale = c(0, 600)))
  grid::grid.polygon(
    x = c(260, 514, 514, 260, 6, 6),
    y = 600 - c(6, 153, 447, 594, 447, 153), default.units = "native",
    gp = grid::gpar(fill = blue, col = orange, lwd = 10, linejoin = "mitre")
  )
  grid::grid.text("influ2", x = 260, y = 474, default.units = "native",
                  gp = grid::gpar(col = orange, fontsize = 72, fontfamily = "sans"))
  visible <- which(radius > 0)
  grid::grid.circle(
    x = 160 + 98 * (bubbles$width[visible] - 2),
    y = 160 + 57.25 * (bubbles$length[visible] - 4),
    r = radius[visible], default.units = "native",
    gp = grid::gpar(col = orange, fill = grDevices::adjustcolor(orange, alpha.f = 0.5),
                    lwd = 2.5)
  )
}

make_icons <- function() {
  logo <- png::readPNG("man/figures/logo.png")
  draw_icon <- function(path, size) {
    ragg::agg_png(path, width = size, height = size, units = "px",
                  background = "transparent")
    on.exit(grDevices::dev.off(), add = TRUE)
    grid::grid.newpage()
    grid::grid.raster(logo, width = 520 / 600, height = 1, interpolate = TRUE)
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
