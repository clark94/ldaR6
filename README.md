# ldaR6 — Predictive Linear Discriminant Analysis in R

**[Launch the Shiny application](https://connect.posit.cloud/loic/content/01a1d27f-1653-cd6f-e042-b2aaab58b216)**

`ldaR6` is an R package for supervised classification using **linear discriminant analysis (LDA)**. Its core algorithm is implemented from scratch as an R6 class with a `fit`, `predict`, and `predict_proba` interface. The package includes a Shiny application for training models, exploring results, and making predictions without writing R code.

## Features

- Gaussian LDA classification with a pooled covariance matrix and configurable class priors.
- Global and partial Wilks’ lambda tests, Rao’s F approximation, and Pillai, Hotelling–Lawley, Roy, and Bartlett statistics.
- Forward, backward, and bidirectional variable selection based on Wilks’ lambda.
- Dummy, one-hot, and label encoding for categorical predictors, with blockwise tests for encoded variables.
- Covariance shrinkage and a pseudoinverse option for high-dimensional data, plus chunked computation for large datasets.
- Cross-validation of the modeling pipeline, discriminant axes, and summaries of supplementary variables.

## Installation

Run the following commands in R or RStudio:

```r
install.packages("remotes")  # Only needed once
remotes::install_github("clark94/ldaR6")
```

If the source archive is available locally, it can also be installed from the repository root:

```r
install.packages("releases/ldaR6_1.0.0.tar.gz", repos = NULL, type = "source")
```

The package code is written in R and does not require compilation. If the optional application packages are missing, install them with:

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

The three rows used for prediction in this example come from the training data. They illustrate the API; use the cross-validation results to assess predictive performance.

## Shiny application

Use the [online demo](https://connect.posit.cloud/loic/content/01a1d27f-1653-cd6f-e042-b2aaab58b216) or launch the app locally:

```r
ldaR6::run_app()
```

| Step | What you can do |
| --- | --- |
| **Data** | Import a CSV, TSV, or Excel file, or load a built-in example such as `iris` or `credit_demo`. |
| **Model setup** | Select the target, predictors, and supplementary variables. Configure preprocessing, variable selection, and evaluation. |
| **Results** | Explore accuracy scores, confusion matrices, Wilks’ lambda, and discriminant plots. |
| **Prediction** | Classify one observation through the form or upload a file of new observations. Export predicted classes and probabilities as TSV. |

## Validation and performance

Automated tests compare selected results against `MASS::lda`, `stats::manova`, and mathematical identities for LDA. The source archive passed `R CMD check --no-manual` with `Status: OK`. Installation directly from GitHub and local launch of the installed Shiny app were also verified.

In one local run on simulated data with 10 predictors, fitting on 100,000 observations took 0.45 seconds, and predicting 10,000 observations took 0.01 seconds. These timings describe that run and do not establish a speed advantage over `MASS::lda`. Historical comparisons are kept in `benchmarks/benchmark_results.csv`; rerun `benchmarks/benchmark.R` before citing them as results for the current version.

## Documentation and repository layout

Start with the [reproducible tutorial in English](TUTORIAL.md). In R, see `?LDA`, `?read_data`, `?simulate_lda_data`, `?run_app`, and `?credit_demo` for function and dataset documentation.

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
Master 2 SISE — Université Lumière Lyon 2 · R Programming, 2026–2027

MIT License.
