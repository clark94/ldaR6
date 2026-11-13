# ldaR6 — Predictive Linear Discriminant Analysis in R

**[Launch the Shiny application on Posit Connect Cloud](https://01a1d27f-1653-cd6f-e042-b2aaab58b216.share.connect.posit.cloud/)**

`ldaR6` is an R package for supervised classification using **linear discriminant analysis (LDA)**. The core algorithm is implemented from scratch in an R6 class with a `fit`, `predict`, and `predict_proba` interface. The package also includes a Shiny application for training models, exploring results, and making predictions without writing R code.

## Features

- Gaussian LDA classification with pooled covariance and configurable class priors.
- Global and partial Wilks’ lambda tests, Rao’s F approximation, and Pillai, Hotelling–Lawley, Roy, and Bartlett statistics.
- Forward, backward, and bidirectional variable selection based on Wilks’ lambda.
- Dummy, one-hot, and label encoding for categorical predictors, with blockwise tests for encoded variables.
- Covariance shrinkage and pseudoinverse computation for high-dimensional data, plus chunked computation for large datasets.
- Cross-validation of the modeling pipeline, discriminant axes, summaries of supplementary variables, and an audit of variables that may adversely affect the model.

## Installation

Run these commands in the R or RStudio console:

```r
install.packages("remotes")  # Only needed if not already installed
remotes::install_github("clark94/ldaR6")
```

If you have the source archive, you can install it from the repository root:

```r
install.packages(
  "releases/ldaR6_1.0.0.tar.gz",
  repos = NULL,
  type = "source"
)
```

The package code is written in R and does not require compilation. If the application dependencies are missing, install them with:

```r
install.packages(c("shiny", "bslib", "DT", "readxl"))
```

## Quick start

```r
library(ldaR6)

X <- iris[, 1:4]
y <- iris$Species

model <- LDA$new(selection = "none", encoding = "dummy")
model$fit(X, y)

model$summary()
model$coefficients
model$predict(X[1:3, , drop = FALSE])
model$predict_proba(X[1:3, , drop = FALSE])

model$cross_validate(X, y, k = 5, seed = 1)
```

The first three rows come from the training set and are used here to illustrate the prediction methods. Use cross-validation to estimate performance on held-out observations.

## Shiny application

Open the [online demo](https://connect.posit.cloud/loic/content/01a1d27f-1653-cd6f-e042-b2aaab58b216) or launch the application locally:

```r
ldaR6::run_app()
```

| Step | What you can do |
| --- | --- |
| **Data** | Import a CSV, TSV, or Excel file, or load a built-in example such as `iris` or `credit_demo`. |
| **Model setup** | Choose the target, predictors, and supplementary variables. Configure encoding, variable selection, and evaluation. |
| **Results** | Review accuracy scores, confusion matrices, Wilks’ lambda, classification functions, and discriminant plots. |
| **Prediction** | Classify one observation through the form or upload a file of new observations containing the predictors. Export predicted classes and probabilities as TSV. |

## Validation and performance

Automated tests compare selected results with `MASS::lda`, `stats::manova`, and mathematical identities for LDA. The source archive passed `R CMD check --no-manual` with `Status: OK`. Installation directly from GitHub and local launch of the installed Shiny application were also verified.

### Local performance check

In one run on simulated data with 10 predictors, fitting on **100,000 observations** took **0.45 s**, and predicting **10,000 observations** took **0.01 s**. These measurements cover the package computations and do not include file import or Shiny rendering. They do not establish a speed advantage over `MASS::lda`.

### Historical benchmarks against `MASS::lda`

**Original environment: R 4.5.2, Windows 11.** These timings were recorded on simulated data and were not reproduced on the current revision.

| Observations | Predictors | `MASS::lda` | `ldaR6` | Notes |
| ---: | ---: | ---: | ---: | --- |
| 100,000 | 10 | 0.17 s | 0.12 s | Predictions matched on the checked rows |
| 1,000,000 | 10 | 2.79 s | 1.57 s | Predictions matched on the checked rows |
| 1,000,000 | 50 | 31.0 s | 9.0 s | Predictions matched on the checked rows |
| 20,000 | 200 | 5.95 s | 1.40 s | Predictions matched on the checked rows |
| 200 | 2,000 | Not compared: singular covariance | 1.2 s with forward selection; 4.8 s with shrinkage | No direct timing comparison |

Prediction agreement was checked on the first 5,000 rows of each comparable dataset. The original timings are recorded in `benchmarks/benchmark_results.csv`. Rerun `benchmarks/benchmark.R` to measure performance for the current version on your machine.

## Documentation and repository layout

Start with the [reproducible tutorial in English](TUTORIAL.md). In R, see `?LDA`, `?read_data`, `?simulate_lda_data`, `?run_app`, and `?credit_demo` for help and examples.

| Path | Contents |
| --- | --- |
| `R/` | R6 model, encoding, variable selection, and statistical utilities |
| `man/` | Package help pages in English |
| `inst/shiny/app/` | Shiny application |
| `inst/extdata/` | Example TSV files for training and batch prediction |
| `data/`, `data-raw/` | Built-in dataset and its generation script |
| `tests/testthat/` | Automated tests |
| `benchmarks/` | Benchmark script and historical results |
| `tutorial/` | Tutorial source |
| `report/` | Project report in French and LaTeX source |
| `releases/` | Source archive for local installation |

## Author and license

**MAMPOUYA CLARK Chancy Loïc Franlly**  
Master 2 SISE · Université Lumière Lyon 2 · R Programming, 2026–2027

MIT License.
