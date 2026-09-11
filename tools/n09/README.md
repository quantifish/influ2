# N09: first residual-validation increment

Read [protocol.md](protocol.md) before interpreting or repeating the study.
This is a bounded NB2 glmmTMB/sdmTMB audit, not a new residual algorithm or
approval to change conditioning defaults. The article is
`vignettes/residual-validation.Rmd`; it reads compact, frozen results and does
not repeat model fits during package checks or website builds.

From the repository root, with the optional native packages installed:

```sh
Rscript --vanilla tools/n09/test-study.R
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 Rscript --vanilla tools/n09/run.R pilot /absolute/pilot-directory
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 Rscript --vanilla tools/n09/run.R production /absolute/production-directory
Rscript --vanilla tools/n09/report.R /absolute/production-directory
```

- `study.R`: fixed generator, designs, seeds, fits, metrics, and controls.
- `run.R`: versioned configuration, per-dataset checkpoints, fit/error records,
  paired sensitivity reruns, and first-eligible-example selection.
- `report.R`: complete-run checks, eligibility denominators, Wilson intervals,
  paired sensitivities, independent reference-band audit, CSV export, and
  `inst/extdata/n09-validation.rds`.
- `test-study.R`: deterministic developer checks of metrics, seeds, graph
  alignment, errors, and native plotted curves.
- `tests/testthat/test-n09-validation.R`: portable audits of the frozen
  results, denominators, summaries, pairing, and compact examples. No native
  model fitting or optional bayesplot requirement for these artefact tests.

Use separate pilot and production directories. Do not combine them. The driver
refuses to resume a configuration with changed source hashes or dependency
versions. Do not regenerate the committed artefact merely to make a later
change pass tests: a changed design or implementation needs a new labelled
study and interpretation. The recorded `source_commit` is the package-runtime
baseline, not a claim that the new study files existed in that commit; the
study and runtime hashes identify the exact inputs separately.

## Reference-band finding

The preliminary known-uniform audit is reproduced by `n09_band_audit()` from
the reference limits extracted through `n09_bands()`. In bayesplot 1.16.0,
`ppc_pit_ecdf(..., K = 100, method = "independent", interpolate_adj = FALSE)`
places its last K binomial limits on `seq(0, 1, length.out = K)`, whereas
the interval calculation uses `(0:K) / K`. Applying the displayed limits at
their intended `(1:K) / K` positions changes the independent-uniform crossing
rate from 12.46% to 4.77% (n = 480; 10,000 replicates; seed 90912).
The independent DKW reference gives 4.71%. This isolates a display-grid issue
from model estimation and latent dependence. No patch or upstream message was
made in this increment. The full audit counts and Monte Carlo intervals are
in the installed artefact and article.

## Interpretation boundary

The DKW crossing summary is an iid-uniform reference comparison, not a
calibrated fitted-model test. Treat near-threshold flags and ten-dataset
seed/count sensitivities cautiously. The neighbour score is a descriptive
within-year spatial measure, not a new public mapping API or hypothesis test.
Review these findings with Nicholas before expanding the design or choosing
statistical defaults. The study contains only simulated data; private review
PDFs and collaborators' correspondence are not part of its public artefacts.
