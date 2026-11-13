#' ldaR6: Predictive Linear Discriminant Analysis
#'
#' The main entry point is the R6 class [LDA]. See also [read_data()],
#' [simulate_lda_data()] and [run_app()] (Shiny application).
#'
#' @keywords internal
#' @importFrom stats predict coef
#' @importFrom R6 R6Class
#' @importFrom graphics plot
#' @importFrom grDevices hcl.colors
"_PACKAGE"

#' Read a data file (CSV / TSV / Excel)
#'
#' Convenience reader used by the Shiny application. The default separator is
#' the tabulation, as required for the course data sets; `.xlsx` / `.xls`
#' files are read with the suggested package `readxl`.
#'
#' @param path path to the file.
#' @param sep field separator (`"\\t"` by default). Ignored for Excel files.
#' @param dec decimal mark (`"."` by default).
#' @param header does the first line contain the column names?
#' @param stringsAsFactors convert text columns to factors?
#' @return a data.frame.
#' @examples
#' f <- system.file("extdata", "credit_demo.tsv", package = "ldaR6")
#' d <- read_data(f)
#' str(d)
#' @export
read_data <- function(path, sep = "\t", dec = ".", header = TRUE, stringsAsFactors = FALSE) {
  if (!file.exists(path)) stop("File not found: ", path, call. = FALSE)
  if (grepl("\\.xlsx?$", path, ignore.case = TRUE)) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Package 'readxl' is required to read Excel files.", call. = FALSE)
    }
    d <- as.data.frame(readxl::read_excel(path, col_names = header))
    if (stringsAsFactors) d[] <- lapply(d, function(x) if (is.character(x)) factor(x) else x)
    return(d)
  }
  utils::read.table(path, header = header, sep = sep, dec = dec, quote = "\"",
                    comment.char = "", stringsAsFactors = stringsAsFactors,
                    check.names = FALSE, na.strings = c("NA", "", "NaN"),
                    fileEncoding = "UTF-8")
}

#' Simulate a labelled data set for LDA (benchmarks, tests)
#'
#' Draws `K` Gaussian classes sharing the same covariance matrix (the LDA
#' model assumption). Only the first `n_informative` variables carry class
#' information; the others are pure noise.
#'
#' @param n number of observations.
#' @param p number of numeric variables.
#' @param K number of classes.
#' @param n_informative number of informative variables (default `min(p, 5)`).
#' @param sep distance between class centres on the informative variables.
#' @param rho common correlation between variables (equicorrelation), in `[0, 1)`.
#' @param seed optional seed.
#' @return a data.frame with a factor column `y` followed by the `p` predictors
#'   `x1`, ..., `xp`.
#' @examples
#' d <- simulate_lda_data(n = 1000, p = 10, K = 3, seed = 1)
#' m <- LDA$new()$fit(d[, -1], d$y)
#' m$accuracy_train
#' @export
simulate_lda_data <- function(n = 1000, p = 10, K = 3, n_informative = min(p, 5),
                              sep = 2, rho = 0.3, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  y <- factor(sample(rep_len(paste0("C", seq_len(K)), n)), levels = paste0("C", seq_len(K)))
  # equicorrelated noise: z_j = sqrt(1 - rho) * e_j + sqrt(rho) * common factor
  E <- matrix(stats::rnorm(n * p), n, p)
  X <- sqrt(1 - rho) * E + sqrt(rho) * stats::rnorm(n)
  shifts <- matrix(stats::rnorm(K * n_informative), K, n_informative) * (sep / 2)
  X[, seq_len(n_informative)] <- X[, seq_len(n_informative)] + shifts[as.integer(y), , drop = FALSE]
  colnames(X) <- paste0("x", seq_len(p))
  data.frame(y = y, X)
}

#' Launch the Shiny application
#'
#' Starts the interactive application: load a data file, choose the target,
#' explanatory and illustrative variables, tune the model, inspect the results
#' and predict new individuals (one by one or from a file).
#'
#' @param ... arguments passed to [shiny::runApp()] (e.g. `port`, `launch.browser`).
#' @return called for its side effect.
#' @examples
#' if (interactive()) run_app()
#' @export
run_app <- function(...) {
  for (pkg in c("shiny", "DT", "bslib")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required: install.packages('", pkg, "')", call. = FALSE)
    }
  }
  app_dir <- system.file("shiny", "app", package = "ldaR6")
  if (!nzchar(app_dir)) stop("Cannot find the Shiny app; try re-installing 'ldaR6'.", call. = FALSE)
  shiny::runApp(app_dir, ...)
}

# S3 wrappers: predict(model, newdata), coef(model), summary(model), plot(model) --

#' S3 methods for `LDA` objects
#'
#' Thin wrappers around the methods of the R6 object so that the usual generics
#' work: `predict(model, newdata)`, `coef(model)`, `summary(model)`,
#' `plot(model)`.
#'
#' @param object,x an [LDA] object.
#' @param newdata a data.frame of explanatory variables.
#' @param type for `predict`: `"class"`, `"prob"` or `"score"`.
#' @param ... passed to the corresponding R6 method.
#' @return see the corresponding R6 methods.
#' @name LDA-methods
#' @examples
#' m <- LDA$new()$fit(iris[, 1:4], iris$Species)
#' head(predict(m, iris[, 1:4], type = "prob"))
#' coef(m)
NULL

#' @rdname LDA-methods
#' @export
predict.LDA <- function(object, newdata, type = c("class", "prob", "score"), ...) {
  object$predict(newdata, type = match.arg(type))
}

#' @rdname LDA-methods
#' @export
coef.LDA <- function(object, ...) object$coefficients

#' @rdname LDA-methods
#' @export
summary.LDA <- function(object, ...) object$summary(...)

#' @rdname LDA-methods
#' @export
plot.LDA <- function(x, ...) x$plot(...)
