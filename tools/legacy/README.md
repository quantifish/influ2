# Frozen Bentley implementation

`inst/legacy/influ-proto.R` is the original `proto`-based implementation used
by `influ2` before the model-neutral S3 overhaul. It is excluded from the
runtime namespace and retained only to document and regenerate the Bentley
parity fixture. Keeping it under `inst/` also allows the comparison vignette
to run the original code through `system.file()` after installation.

The routine package tests compare the new engine with the compact reference
values in `inst/extdata/bentley-poisson-reference.csv`; they do not source the
legacy implementation or require `proto`. To deliberately rebuild the fixture, run
`Rscript tools/legacy/regenerate-bentley-fixture.R` from the package root.

The source file retains its original copyright and licence notice.

## Earlier influ2 helpers

`tools/legacy/R/` and `tools/legacy/man/` contain a frozen copy of the earlier
BRMS-specific helper interface. These files are deliberately outside the
runtime package namespace while each helper is triaged. Selected functions
have already been rebuilt in the active package as model-neutral diagnostics:

- `plot_compare()` and `plot_step()`;
- `plot_data_extent()`;
- `plot_implied_residuals()`, `plot_predicted_residuals()`, and `plot_qq()`;
- `get_bayes_R2()` and `table_criterion()`; and
- `plot_bubble()`.

The frozen files are comparison material, not maintained implementations.
They should only be deleted after the remaining functions have been reviewed
individually.
