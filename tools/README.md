# Repository layout

| Location | Purpose | In the CRAN source archive? |
| --- | --- | --- |
| `R/`, `man/` | Active implementation and generated R help | Yes |
| `tests/testthat/` | Numerical, guard, backend, and visual regression tests | Yes |
| `vignettes/` | Current articles, references, and shared rendering assets | Yes |
| `data/` | Simulated lobster example | Yes |
| `data-raw/` | Recipes and local material for preparing data/fixtures | No |
| `inst/extdata/` | Compact numerical and fitted-model fixtures | Yes |
| `inst/legacy/` | Separately licensed, frozen Bentley validation code | Yes |
| `inst/COPYRIGHTS` | Scope of package and validation-artefact copyrights | Yes |
| `man/figures/logo.png` | Existing logo used by README, pkgdown, and package help | Yes |
| `tools/branding/` | Logo and website-icon recipe; not automatically executed | No |
| `tools/references/` | Historical reference PDFs, moved without modification | No |
| `tools/legacy/` | Fixture recipe and remaining retired-helper review copies | No |
| `tools/release-review.md` | Pre-release decisions, issue queue, and checks | No |
| `tools/development-backlog.md` | Nicholas's proposals, Ginflu-inspired candidates, and parked decisions | No |
| `tools/check-figure-captions.R`, `tools/tests/` | Site-wide lightbox caption and keyboard regression checks | No |
| `pkgdown/`, `_pkgdown.yml` | Website configuration, assets, and frozen review article | No |
| `docs/` | Generated local website; ignored by Git | No |
| `.github/`, `codecov.yml` | CI, website deployment, and coverage configuration | No |

Keep DESCRIPTION, NAMESPACE, LICENSE, NEWS.md, and README.md at the package
root. The installed package does not need the development scripts, PDFs, or
retired helpers. Do not move reference PDFs into `data/` or `inst/doc/`:
neither is an archive for unrelated development material.

Ignored local outputs such as `Rplots.pdf`, `doc/`, `Meta/`, and RStudio state
are also excluded from the source archive. They have not been deleted.
The remaining legacy source and frozen Get Started page must stay in Git
until the maintainer completes their review.

After building the website, run `Rscript tools/check-figure-captions.R`.
This checks every plotted image in the rendered HTML, including the frozen
review article, against the shared lightbox script. It requires the website's
existing xml2/jsonlite dependencies and Node.js; no extra Node packages are
needed. The pkgdown workflow runs the same check before deployment.
