# ldaR6 Tutorial: Predictive Linear Discriminant Analysis in R

**[Launch the Shiny application on Posit Connect Cloud](https://01a1d27f-1653-cd6f-e042-b2aaab58b216.share.connect.posit.cloud/)**

`ldaR6` implements **linear discriminant analysis (LDA)** as an R6 class. This tutorial shows how to fit a classification model, interpret its results, make predictions, and evaluate it on held-out observations. It also explains how to perform the same tasks through the Shiny application.

Run the examples in order in the same R session.

## 1. Install the package

Install `remotes` if needed, then install `ldaR6` from GitHub:

```r
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
remotes::install_github("clark94/ldaR6")
```

Alternatively, if you have the source archive, run this command from the repository root:

```r
install.packages(
  "releases/ldaR6_1.0.0.tar.gz",
  repos = NULL,
  type = "source"
)
```

Load the package:

```r
library(ldaR6)
```

## 2. Fit an LDA model

Pass a data frame of predictors to `$fit(X, y)` and a factor containing the class labels. The `iris` dataset contains four numerical measurements and three species.

```r
X_iris <- iris[, 1:4]
y_iris <- iris$Species

iris_model <- LDA$new(selection = "none")
iris_model$fit(X_iris, y_iris)

iris_model
iris_model$summary()
```

LDA estimates class means and a pooled within-class covariance matrix. Together with the class priors, these quantities determine the linear classification functions.

| Field | Contents |
| --- | --- |
| `$coefficients` | Linear classification functions, with the intercept in the first row |
| `$wilks` | Global tests, including Wilks’ lambda and Rao’s F approximation |
| `$variables` | Variable-level results, including univariate and partial tests |
| `$canonical` | Discriminant axes and class centroids |
| `$distances` | Squared Mahalanobis distances between classes |
| `$selected`, `$selection_history` | Retained variables and the steps taken during selection |
| `$confusion_train`, `$accuracy_train` | Confusion matrix and accuracy on the training data |

```r
round(iris_model$coefficients, 4)
iris_model$wilks$global
iris_model$confusion_train
```

Training accuracy describes how well the fitted model classifies the observations it has already seen. Use cross-validation or a separate test set to estimate predictive performance.

## 3. Predict classes and probabilities

Supply the predictor columns without the target. `$predict()` returns class labels, `$predict_proba()` returns posterior class probabilities, and `$transform()` returns coordinates on the discriminant axes.

```r
examples <- X_iris[c(1, 60, 120), , drop = FALSE]

iris_model$predict(examples)
round(iris_model$predict_proba(examples), 4)
round(iris_model$transform(examples), 3)
```

These rows come from the training dataset and illustrate the API. For an independent evaluation, use observations that were not included in the fit.

## 4. Explore the discriminant plots

```r
iris_model$plot("scores")
iris_model$plot("axes")
```

The score plot shows how observations are distributed in the discriminant space. The axes plot helps interpret how the predictors relate to the discriminant axes.

## 5. Work with categorical predictors

The built-in `credit_demo` dataset is synthetic. It contains numerical and categorical predictors and a target named `risk`.

```r
data("credit_demo", package = "ldaR6")

X_credit <- credit_demo[, setdiff(names(credit_demo), "risk"), drop = FALSE]
y_credit <- credit_demo$risk

credit_model <- LDA$new(encoding = "dummy")
credit_model$fit(X_credit, y_credit)

credit_model$summary()
rownames(credit_model$coefficients)
```

Dummy encoding represents a categorical predictor through indicator columns, with a reference level. The package also supports `encoding = "onehot"` and `encoding = "label"`. Label encoding assigns numbers to categories and can introduce an ordering that has no meaning for nominal variables. Full one-hot encoding can introduce linear dependencies.

## 6. Select variables and inspect their contribution

The package supports forward selection, backward elimination, and a bidirectional procedure. Selection is based on partial Wilks’ lambda and uses the F-test thresholds `alpha_enter` and `alpha_stay` to decide when to add or remove variables. Encoded columns from the same categorical predictor are considered together during selection.

```r
selected_model <- LDA$new(selection = "both", encoding = "dummy")
selected_model$fit(X_credit, y_credit)

selected_model$selected
selected_model$selection_history
selected_model$variables
selected_model$plot("importance")
```

Univariate tests assess each variable on its own. Partial tests assess its contribution given the other variables in the comparison. A predictor can show a strong univariate association and contribute little once related predictors are included.

To examine how cross-validated accuracy changes when individual predictors are removed:

```r
selected_model$variable_audit(X_credit, y_credit, k = 5, seed = 1)
```

Treat the audit as exploratory. An increase in accuracy after removing a predictor can depend on the folds used.

## 7. Evaluate the model with cross-validation

```r
cv <- selected_model$cross_validate(
  X_credit,
  y_credit,
  k = 10,
  seed = 1
)

cv
```

The modeling pipeline, including encoding and variable selection, is fitted again within each training fold. The held-out fold is used for prediction. Inspect the confusion matrix as well as overall accuracy: a single score can hide poor results for a particular class.

If you compare many settings and choose the best one using these scores, use a separate test set or nested cross-validation for the final evaluation.

## 8. Handle high-dimensional and large datasets

When the number of predictors exceeds the information available to estimate the within-class covariance matrix, the matrix can be singular. `ldaR6` can use a pseudoinverse in this situation. Covariance shrinkage or variable selection are alternatives to explore.

```r
wide <- simulate_lda_data(n = 100, p = 400, K = 3, seed = 1)
X_wide <- wide[, setdiff(names(wide), "y"), drop = FALSE]

wide_model <- LDA$new(shrinkage = 0.3)
wide_model$fit(X_wide, wide$y)

wide_model$cross_validate(X_wide, wide$y, k = 5, seed = 1)
```

For datasets with many rows, `chunk_size` controls chunked accumulation of the within-class sums of squares and cross-products. See [`benchmarks/benchmark.R`](benchmarks/benchmark.R) for the timing procedure. Historical results in the repository should not be treated as measurements of the current revision without rerunning the benchmark.

## 9. Set class priors and describe supplementary variables

The default priors follow the class proportions in the training data. You can use equal priors or specify probabilities by class name.

```r
equal_model <- LDA$new(priors = "equal")
equal_model$fit(X_credit, y_credit)
equal_model$priors

custom_model <- LDA$new(priors = c(low = 0.2, medium = 0.3, high = 0.5))
custom_model$fit(X_credit, y_credit)
custom_model$priors
```

Supplementary variables describe the discriminant space without contributing to the fitted classifier. Numerical variables are summarized through correlations with the axes. Categorical variables are summarized through the mean coordinates of their levels.

```r
core_predictors <- c("income", "debt_ratio", "savings")

small_model <- LDA$new()
small_model$fit(X_credit[, core_predictors, drop = FALSE], y_credit)

small_model$project_illustrative(credit_demo[, c("age", "job")])
```

## 10. Make batch predictions from a file

The package includes a labeled training file and a separate file of new observations. `read_data()` reads tab-separated data by default.

```r
training_file <- system.file("extdata", "credit_demo.tsv", package = "ldaR6")
prediction_file <- system.file("extdata", "credit_new.tsv", package = "ldaR6")

training_data <- read_data(training_file)
new_data <- read_data(prediction_file)

predictors <- setdiff(names(training_data), "risk")

batch_model <- LDA$new(selection = "both")
batch_model$fit(
  training_data[, predictors, drop = FALSE],
  factor(training_data$risk)
)

results <- data.frame(
  predicted_class = batch_model$predict(new_data),
  batch_model$predict_proba(new_data),
  check.names = FALSE
)

head(results)

write.table(
  results,
  file = "credit_predictions.tsv",
  sep = "\t",
  row.names = FALSE
)
```

The prediction file should contain the required predictor columns with their original names and compatible data types. It does not need the target column.

## 11. Use the Shiny application

Open the [online demonstration](https://01a1d27f-1653-cd6f-e042-b2aaab58b216.share.connect.posit.cloud/) or launch the application locally:

```r
ldaR6::run_app()
```

If the optional application packages are missing, install them first:

```r
install.packages(c("shiny", "bslib", "DT", "ggplot2", "readxl"))
```

1. **Data:** Load a built-in example or import a CSV, TSV, or Excel file. Check the preview and detected column types.
2. **Model setup:** Choose the target, predictors, and supplementary variables. Set the class priors, encoding, variable selection, and evaluation options.
3. **Results:** Review accuracy scores, confusion matrices, Wilks’ lambda, variable contributions, and the discriminant plot.
4. **Prediction:** Enter one observation in the form or upload a file of new observations. Download the predicted classes and probabilities as a TSV file.

For a first walkthrough, load `iris`, choose `Species` as the target, and use the four measurement columns as predictors.

## 12. Validate the computations in R

The automated tests compare selected results against `MASS::lda`, `stats::manova`, and mathematical identities for LDA. The following example compares posterior probabilities for the same `iris` observations.

```r
if (!requireNamespace("MASS", quietly = TRUE)) {
  install.packages("MASS")
}

reference <- MASS::lda(X_iris, grouping = y_iris)
reference_probabilities <- predict(reference, X_iris)$posterior

package_probabilities <- iris_model$predict_proba(X_iris)
package_probabilities <- package_probabilities[
  , colnames(reference_probabilities), drop = FALSE
]

max(abs(reference_probabilities - package_probabilities))
```

A difference close to zero indicates numerical agreement for this example. Both models are evaluated on the data used to fit them, so this comparison checks the implementation. It does not estimate accuracy on new observations.

## Session information

Record the R and package versions used to run the examples:

```r
packageVersion("ldaR6")
sessionInfo()
```

For more details, see `?LDA`, `?read_data`, `?simulate_lda_data`, `?run_app`, and `?credit_demo`, or return to the [project README](README.md).
