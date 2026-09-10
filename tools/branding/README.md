# Logo and website icons

Run `Rscript tools/branding/logo.R` from the repository root to regenerate:

- `man/figures/logo.png`, used by GitHub README, the website, and package help;
- the PNG browser/touch icons and multi-resolution ICO in `pkgdown/favicon/`.

Only `ragg` and `png` are development dependencies; neither is a new runtime
dependency. The source uses R's grid graphics, so no image-generation service,
external font download, or online favicon conversion is needed. Website builds
copy the checked-in icons rather than regenerating them with a different font.

The 10 September 2026 refinement retains the original blue (`#1881C2`), orange
(`#FFA500`), hexagon, and iris-based bubble composition. It removes miniature
axis text, ticks, and the dotted grid to improve small-size legibility.
Following the maintainer's visual review, a plain rectangular plot frame is
retained around the bubbles. The logo is an identifying motif, not an
inferential figure.

The previous logo and generation recipe remain recoverable from Git commit
`d215b3f`. The frozen legacy Get Started page is a review artefact and must
not be rewritten as part of a branding change.
