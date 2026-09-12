# IV01: residual-implied-effect validation

Read [protocol.md](protocol.md) before running or interpreting this bounded
NB2 glmmTMB study. It is separate from the original frozen N09 PIT study.
No package algorithms, default panels, or conditioning settings are changed.

From the repository root:

```sh
Rscript --vanilla tools/implied-validation/test-study.R
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 Rscript --vanilla tools/implied-validation/run.R pilot /absolute/pilot-directory
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 Rscript --vanilla tools/implied-validation/run.R production /absolute/production-directory
Rscript --vanilla tools/implied-validation/report.R /absolute/production-directory /absolute/new-result.rds
```

Use distinct pilot and production directories. Resuming a run requires the
same settings, source hashes, package versions, and baseline revision. The
driver keeps one checkpoint per replicate, counts every attempted fit, and
stores neither fitted models nor observation arrays in the final artefact.
The report refuses to overwrite its target. Inspect its complete counts and
developer tests before publishing `inst/extdata/implied-validation.rds`.
Once published, do not regenerate that frozen artefact to make later edits pass.

The known-parameter control exercises the existing local shift/profile
routines at the true additive predictors, realised vessel effects, and size.
The public fitted-model calculation is evaluated against an independently
computed expected-likelihood target conditional on that fit. These are
different targets: the latter is data-dependent and is not an interaction
parameter with ordinary 95% confidence coverage. Summaries and Monte Carlo
errors use datasets as independent units, not correlated season-year cells.

`test-study.R` covers the independent likelihood, expected-score target,
generator, sampling counts, exact states, plotting gaps, and summary denominators.
Package tests audit the frozen artefact without fitting any study models.
The worked results belong in `vignettes/implied-effects.Rmd`.
