# ldaR6 - Predictive Linear Discriminant Analysis (R6) + Shiny

`ldaR6` implements **predictive linear discriminant analysis** from scratch as an `R6` class with a
scikit-learn like interface, plus a **Shiny application** for non-programmers.
Project of the M2 SISE (Universite Lumiere Lyon 2), course *Programmation R*, 2026-2027.

* Standard Gaussian LDA classification functions; no SAS execution verified (pooled covariance, `ln(prior)` in the constant)
* Wilks' lambda with Rao's F, Pillai / Hotelling-Lawley / Roy, Bartlett; **partial** tests per variable
* Stepwise selection (forward / backward / both) on Wilks' lambda, like `PROC STEPDISC`
* Categorical predictors: dummy (default), one-hot or label encoding, tested as blocks
* Wide data (`shrinkage`, selection, pseudo-inverse) and very long data (chunked accumulation)
* Cross-validation of the whole pipeline, discriminant axes, illustrative variables, perturbing-variable audit
* Shiny app: load data, choose variables, tune, inspect, predict one individual or a file

## Installation

```r
install.packages("remotes")
remotes::install_github("clark94/ldaR6")
```

Fallback (no internet / GitHub problem): install the archive in [`releases/`](releases/) -
in RStudio *Tools > Install Packages > Install from: Package Archive File*, or

```r
install.packages("releases/ldaR6_1.0.0.tar.gz", repos = NULL, type = "source")
```

The package is pure R (no compilation needed). The Shiny app additionally needs `shiny`, `bslib`, `DT`
(`install.packages(c("shiny", "bslib", "DT", "readxl"))`).

## Quick start

```r
library(ldaR6)
m <- LDA$new(selection = "both")          # constructor: parameters only
m$fit(iris[, 1:4], iris$Species)          # learn
m                                         # short print
m$summary()                               # detailed report
m$coefficients                            # classification functions
m$predict(iris[1:3, 1:4]); m$predict_proba(iris[1:3, 1:4])
m$cross_validate(iris[, 1:4], iris$Species, k = 10, seed = 1)
ldaR6::run_app()                          # Shiny application
```

**Full tutorial (reproducible, English): [TUTORIAL.md](TUTORIAL.md)** - source in `tutorial/TUTORIAL.Rmd`.
Help pages: `?LDA`, `?read_data`, `?simulate_lda_data`, `?run_app`, `?credit_demo`.

## Repository layout

| Path | Content |
|---|---|
| `R/` | package code (`LDA.R` class, `encoding.R`, `selection.R`, `stats_utils.R`, `utils_public.R`) |
| `man/` | Rd help files (English) |
| `inst/shiny/app/app.R` | Shiny application (`run_app()`) |
| `inst/extdata/` | example data (tab separated): `credit_demo.tsv`, `credit_new.tsv` (no target), `iris.tsv` |
| `data/`, `data-raw/` | `credit_demo` dataset and the script that generates it |
| `tests/testthat/` | 70 unit tests (equivalence with `MASS::lda`, `stats::manova`, SAS formulas, robustness) |
| `benchmarks/` | speed benchmark against `MASS::lda` (`benchmark.R`, `benchmark_results.csv`) |
| `report/` | LaTeX report (French): `rapport.tex`, `rapport.pdf`, figures script |
| `releases/` | `ldaR6_1.0.0.tar.gz` for local installation |

## Checking against SAS PROC DISCRIM

```sas
proc discrim data=mydata method=normal pool=yes crossvalidate;
  class target; var x1 x2 x3; priors proportional;
run;
```
Compare with `m$coefficients`, `m$distances$d2`, `m$wilks$global`. See the last section of the tutorial.
Note: SAS was not available when the package was written; equivalence is established through the SAS
definitions, `MASS::lda` and `stats::manova` (automatic tests). Please run the SAS comparison on your own data.

## Historical performance figures (not revalidated in this revision)

Rerun `benchmarks/benchmark.R` on your machine before reporting these figures as measured results. The GitHub installation URL assumes you publish this repository under `clark94/ldaR6`; publication has not been verified.

Original environment: R 4.5.2, Windows 11.

| n | p | `MASS::lda` | `ldaR6` | |
|---:|---:|---:|---:|---|
| 1e5 | 10 | 0.17 s | 0.12 s | same predictions |
| 1e6 | 10 | 2.79 s | 1.57 s | same predictions |
| 1e6 | 50 | 31.0 s | 9.0 s | same predictions |
| 2e4 | 200 | 5.95 s | 1.40 s | same predictions |
| 200 | 2000 | (singular) | 1.2 s with forward selection, 4.8 s with shrinkage | |

## License
MIT
