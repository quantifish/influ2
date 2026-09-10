# Logo and website icons

Run `Rscript tools/branding/logo.R` from the repository root to regenerate:

- `man/figures/logo.png`, used by GitHub README, the website, and package help;
- the PNG browser/touch icons and multi-resolution ICO in `pkgdown/favicon/`.

Development-only dependencies are `hexSticker`, `ggplot2`, `ragg`, and `png`;
no package runtime dependencies were added. `hexSticker` supplies the original
Aller font. Its `magick` dependency requires ImageMagick when installed from
source. Website builds copy the checked-in assets; they do not run this recipe
or depend on a font download, an image-generation service, or online favicon
conversion.

Following the final 10 September 2026 visual review, the original hexSticker
design is restored: blue (`#1881C2`), orange (`#FFA500`), original typography,
hexagon, iris-based bubbles, rectangular frame, ticks, and fine dotted grid.
Only the tiny numeric axis labels are hidden. Transparent text retains their
layout space, and explicit margins and panel dimensions match the historical
placement with the current graphics renderer. `make_logo(..., axis_numbers =
TRUE)` can render the numbered version for a layout comparison. This logo is
an identifying motif, not an inferential figure.

The previous logo and generation recipe remain recoverable from Git commit
`d215b3f`. The frozen legacy Get Started page is a review artefact and must
not be rewritten as part of a branding change.
