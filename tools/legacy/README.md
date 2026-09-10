# Frozen Bentley implementation

`inst/legacy/influ-proto.R` is the original `proto`-based implementation used
by `influ2` before the model-neutral S3 overhaul. It is excluded from the
runtime namespace and retained only to document and regenerate the Bentley
parity fixture. Keeping it under `inst/` also allows the comparison vignette
to run the original code through `system.file()` after installation.

The routine fixture tests compare the new engine with the compact reference
values in `inst/extdata/bentley-poisson-reference.csv` without requiring
`proto`. A separate lobster parity test sources the original implementation
when `proto` is installed. To deliberately rebuild the fixture, run
`Rscript tools/legacy/regenerate-bentley-fixture.R` from the package root.

The source file retains its original copyright and licence notice.

## Earlier influ2 helpers

`tools/legacy/R/` and `tools/legacy/man/` contain the remaining review copies
of the earlier brms-specific helper interface. These files are deliberately
outside the runtime package namespace while each helper is triaged. Selected
functions have already been rebuilt in the active package as model-neutral
diagnostics:

- `plot_compare()` and `plot_step()`;
- `plot_data_extent()`;
- `plot_implied_residuals()` and `plot_predicted_residuals()`;
- `get_bayes_R2()` and `table_criterion()`; and
- `plot_bubble()`.

The remaining files are comparison material, not maintained implementations.
They should only be deleted after the remaining functions have been reviewed
individually. Retaining their source does not re-export their old names.

On 9 September 2026, `geo_mean()` was explicitly restored as a stable public
utility. Assessment-table functionality is now provided by `cpue_index()` and
a calculated-object `plot_index()`, with explicit reference populations and
uncertainty definitions, not wrappers around the old brms implementation.
The old source remains here for the final frozen-page review only. The retired
influence/CDI helpers and Shiny launcher are not reinstated.

### Accepted removals: 7 September 2026

The maintainer approved retirement of `plot_hurdle()`, `get_coefs()`,
`get_coefs_raw()`, `get_marginal()`, `get_influ()`, `get_influ2()`,
`plot_influ()`, `plot_bayesian_cdi()`, `plot_bayesian_cdi2()`, and `influ_app()`.
Their source and help files have been removed. The coefficient helpers had
no callers outside the retired influence/CDI helpers; no retained function
requires the old Shiny app. Its use of `get_bayes_R2()` and `plot_compare()`
does not make those independently useful functions removal candidates.

On 10 September 2026, the maintainer also approved retiring `plot_qq()` and
its native-residual Q-Q workflow. Use `influ_residuals(fit)` followed by
`plot(checks, type = "qq")`; the simulation-based diagnostic supersedes the
old approach rather than reproducing its residuals. The old Q-Q source and
help are removed, without a compatibility wrapper. Per-point posterior
intervals are not required for the first release. The frozen article is
unchanged; the retired source remains recoverable from Git history.

The current `influ()` generic, the Bentley implementation above, the frozen
Get Started page at `pkgdown/assets/articles/legacy-get-started.html`, and all
its figures remain intact. The full pre-triage helper source is recoverable
from Git commit `cf12bb6`.

Still retained for review: `get_index()`, `plot_index()`,
`get_unstandarsied()` (the original spelling), `rescale_index()`, earlier
`table_criterion()` and `get_bayes_R2()` reporting, `glm_term_table()`, and
internal utilities and examples not yet individually triaged. See
`tools/release-review.md` for the decisions still needed. A new model-neutral
viewer remains an optional future idea, not an implementation commitment.
