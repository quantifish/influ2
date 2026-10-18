# Repository layout

| Location | Purpose | In the CRAN source archive? |
| --- | --- | --- |
| `R/`, `man/` | Active implementation and generated R help | Yes |
| `tests/testthat/` | Numerical, guard, backend, and visual regression tests | Yes |
| `vignettes/` | Complete executable articles, references, and rendering/model-reuse helpers | Yes |
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
| `tools/scientific-release-review.md` | Current scientific interpretation audit and decisions for Darcy and Nicholas | No |
| `tools/development-backlog.md` | Nicholas's proposals, Ginflu-inspired candidates, and parked decisions | No |
| `tools/n09/` | Fixed residual-validation protocol, simulation driver, report builder, and developer tests | No |
| `tools/implied-validation/` | IV01 NB2 implied-effect protocol, independent target checks, simulation driver, and compact report builder | No |
| `tools/check-figure-captions.R`, `tools/tests/` | Site-wide lightbox caption and keyboard regression checks | No |
| `tools/check-minimal-install.R`, `tools/tests/minimal-install-worker.R` | Isolated installation, core, saved-result, and missing-dependency smoke checks | No |
| `tools/profile-vignettes.R` | Per-chunk timing of all fully executed worked articles | No |
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

For per-chunk article timings, run
`Rscript --vanilla tools/profile-vignettes.R . /absolute/temporary/output`.
Optional trailing article names restrict the run. Every existing evaluated
example is executed, without reducing data or simulations or skipping slow
chunks. Identical ordinary native fits can be reused through the
documentation-only `example_model()` helper; changed data, formulas, families,
controls, or software builds produce different cache keys. The output contains
rendered HTML, `articles.csv`, and
`chunks.csv`. For a single-thread comparison, start the R process with
`OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 MKL_NUM_THREADS=1
BLIS_NUM_THREADS=1 VECLIB_MAXIMUM_THREADS=1`. These process-level controls
are also used in CI to avoid excess numerical-library threading; the installed
package does not change a user's thread settings.

### Documentation size and exact-fit reuse

All twelve articles remain complete executable R Markdown vignettes, including
every worked model, calculation, table, figure, and caption. They use knitr's
standard `rmarkdown_notangle` engine to avoid shipping a second, extracted R
script containing the same code. Installed HTML refers to portable external
PNG figures and shared CSS/JavaScript in `inst/doc/`, rather than base64 copies
embedded in every HTML file. Standard bundled highlight.js provides coloured
code without repeating Pandoc's token markup throughout every article; its
JavaScript and stylesheet are installed once and work without the Internet.
Code remains readable if JavaScript is disabled. The bibliography and helpers
accompany the source.
`.install_extras` preserves directory structure; narrow `.Rbuildignore` rules
remove only duplicate generated assets from the source `vignettes/` directory,
after R has installed their copies under `inst/doc/`. Website styling and
lightbox handling remain pkgdown's responsibility.

`example_model(data, fit)` wraps a displayed native fitting call, not a new
public influ2 model interface. It evaluates the call in a small, private
data-only environment and keys its temporary cache by the actual call,
complete data and factor levels, relevant options, R, and backend builds.
Only exactly matching GLM, MASS negative-binomial, and glmmTMB fits are reused;
different year ranges, polynomial orders, families, and spatial specifications
remain separate. Cached glmmTMB objectives are reconstructed by the native
`up2date()` method without optimisation, with caller RNG and thread settings
preserved. Cache integrity is checked. No new native fits are shipped, and no
residual simulation, index calculation, model search, or plotted example is
silently cached. The native fitting call inside the wrapper can be run directly
in a user's own analysis.

The default cache is scoped to R's temporary directory. The existing CI jobs
set `INFLU2_EXAMPLE_CACHE` to a runner-temporary directory so separate article
and check processes can reuse the same verified fits. Use a new empty temporary
directory for cold timing, retain it for warm timing, or call the helper with
`cache_dir = NULL` for an uncached numerical comparison. Never commit this cache.

Only three full-size article integration tests use `skip_on_cran()`: the
5,049-record area-CPUE, unequal-period CDI, and worked model-search reruns.
Their articles still execute during build and check, and all independent
compact engine, backend, family, covariance, and saved-result regressions
remain active on CRAN. Both platform CI jobs and coverage explicitly set
`NOT_CRAN=true`, retaining the exhaustive integration tests too. No test
tolerances or frozen validation values are changed.

The two release-platform checks request PDF-manual and HTML-help validation.
They explicitly use `--as-cran --timings`, without `--no-manual`; TinyTeX is
provided by the existing R actions, with the Courier font and `makeindex`
explicitly installed for R's default Windows manual format. A separate
`R CMD Rd2pdf` preflight
catches compiler/font failures before the longer tests and worked examples;
its PDF is written outside the checkout and retained as an Actions artefact,
and the full check still recompiles the manual. Local full checks likewise
need a usable TeX installation and a recent HTML Tidy. CI may report an
environment NOTE if Tidy or the optional
V8 math checker is absent; record that limitation rather than claiming those
checks passed. A successful no-manual check is not a substitute for this
final check.

After building the website, run `Rscript tools/check-figure-captions.R`.
This checks every plotted image in the rendered HTML, including the frozen
review article, against the shared lightbox script. It requires the website's
existing xml2/jsonlite dependencies and Node.js; no extra Node packages are
needed. The pkgdown workflow runs the same check before deployment.

The saved-result regression tests in `tests/testthat/test-saved-results.R`
write compact objects to temporary RDS files, then reopen and render them in
a separate R session using the test-only `callr` dependency. Only file paths
are passed to the child, not fitted models or the parent workspace. During
source-tree tests, the test-only `pkgload` dependency loads that exact source
without test helpers; installed-package checks load the exact tested library.
The tests compare complete objects, public summaries, plot-layer coordinates,
axis labels/ranges, and panel layouts, and draw the composite figures. They
also reject hidden environments, functions, and external pointers, and check
that plotting neither changes the results nor advances the random-number state.
This is a same-version restart check, not a promise of indefinite format
compatibility or portable fitted-model/external-draw-file storage.

Run `Rscript --vanilla tools/check-minimal-install.R` from the repository root
to repeat the minimal-installation check. The existing Ubuntu-release check
job runs it automatically; no additional job or platform is added. It copies
only the installed mandatory runtime dependency closure into a temporary library,
builds the current source without rebuilding vignettes, and installs that
archive with user and site libraries excluded. No existing library is changed,
no packages are downloaded, and no MCMC is run. R's default base/recommended
library remains available; unexpected extra packages there cause an explicit
failure rather than a silently weakened test. The driver follows R's documented
[library-path isolation](https://stat.ethz.ch/R-manual/R-devel/library/base/html/libPaths.html).
Build-time header packages of already compiled dependencies are not runtime
requirements; any build-time requirements declared by influ2 itself are staged.

A separate producer with glmmTMB, brms, and posterior available generates
compact numerical/plot baselines using that exact installed influ2 version.
The minimal worker confirms that the optional backends and development tools
cannot be found or loaded, then recalculates core GLM diagnostics, uncertainty,
indices, area integration, and steps. It also reopens all four mixed-model
result types and stored Bayesian influence/residual summaries, reproduces
tables and plots, and checks explicit missing-package errors for influence,
residual, and index calculations, and optional PIT ECDF panels. These are developer smoke checks,
not a full no-Suggests vignette build, statistical calibration study, or final
CRAN archive check. The general restart regression tests above remain part of
the package's ordinary test suite.

The frozen residual-engine regression results live in
`tests/testthat/fixtures/residual-engine-baseline.rds`. Their developer recipe,
`data-raw/residual-engine-baseline.R`, must run against the pre-refactor source
`6c3d1c9`, not the current implementation. Do not regenerate golden results to
make a changed calculation pass. The fixed inputs isolate the shared engine;
native backend integration tests are maintained separately.
