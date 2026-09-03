# Frozen Bentley implementation

`influ-proto.R` is the original `proto`-based implementation used by
`influ2` before the model-neutral S3 overhaul. It is excluded from the runtime
namespace and retained only to document and regenerate the Bentley parity
fixture.

The routine package tests compare the new engine with the compact reference
values in `inst/extdata/bentley-poisson-reference.csv`; they do not source this
file or require `proto`. To deliberately rebuild the fixture, run
`Rscript tools/legacy/regenerate-bentley-fixture.R` from the package root.

The source file retains its original copyright and licence notice.
