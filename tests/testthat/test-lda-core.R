test_that("posteriors and axes match MASS::lda (reference implementation)", {
  skip_if_not_installed("MASS")
  X <- iris[, 1:4]; y <- iris$Species
  m <- LDA$new()$fit(X, y)
  l <- MASS::lda(X, y)
  expect_equal(unname(m$predict_proba(X)), unname(predict(l, X)$posterior), tolerance = 1e-10)
  expect_equal(as.character(m$predict(X)), as.character(predict(l, X)$class))
  expect_equal(abs(unname(m$canonical$coefficients)), abs(unname(l$scaling)), tolerance = 1e-8)
})

test_that("classification functions follow the SAS PROC DISCRIM definition", {
  X <- as.matrix(iris[, 1:4]); y <- iris$Species
  m <- LDA$new(priors = "equal")$fit(X, y)
  mu <- rowsum(X, y) / 50
  W <- Reduce(`+`, lapply(levels(y), function(k) crossprod(scale(X[y == k, ], scale = FALSE))))
  S <- W / (150 - 3)
  b <- solve(S, t(mu))
  cst <- -0.5 * colSums(t(mu) * b) + log(1 / 3)
  expect_equal(unname(m$coefficients[-1, ]), unname(b), tolerance = 1e-10)
  expect_equal(unname(m$coefficients[1, ]), unname(cst), tolerance = 1e-10)
  # well known slopes of the SAS iris example
  expect_equal(round(unname(m$coefficients[2, ]), 5), c(23.54417, 15.69821, 12.44585))
})

test_that("Wilks lambda equals det(W)/det(T) and matches stats::manova", {
  X <- as.matrix(iris[, 1:4]); y <- iris$Species
  m <- LDA$new()$fit(X, y)
  Wm <- Reduce(`+`, lapply(levels(y), function(k) crossprod(scale(X[y == k, ], scale = FALSE))))
  Tm <- crossprod(scale(X, scale = FALSE))
  expect_equal(m$wilks$global$value, det(Wm) / det(Tm), tolerance = 1e-10)
  fit <- summary(stats::manova(X ~ y), test = "Wilks")
  expect_equal(m$wilks$global$value, fit$stats[1, "Wilks"], tolerance = 1e-8)
  expect_equal(m$wilks$global$F, fit$stats[1, "approx F"], tolerance = 1e-6)
  pil <- summary(stats::manova(X ~ y), test = "Pillai")
  expect_equal(m$wilks$multivariate$value[2], pil$stats[1, "Pillai"], tolerance = 1e-8)
})

test_that("partial Wilks matches the definition (full vs reduced model)", {
  X <- as.matrix(iris[, 1:4]); y <- iris$Species
  m <- LDA$new()$fit(X, y)
  lam <- function(cols) {
    Z <- X[, cols, drop = FALSE]
    Wm <- Reduce(`+`, lapply(levels(y), function(k) crossprod(scale(Z[y == k, , drop = FALSE], scale = FALSE))))
    det(Wm) / det(crossprod(scale(Z, scale = FALSE)))
  }
  expected <- lam(1:4) / lam(2:4)
  expect_equal(m$variables$partial_lambda[1], expected, tolerance = 1e-8)
})

test_that("two-class problem works and priors are normalised", {
  d <- iris[iris$Species != "setosa", ]; d$Species <- droplevels(d$Species)
  m <- LDA$new(priors = c(versicolor = 0.8, virginica = 0.2))$fit(d[, 1:4], d$Species)
  expect_equal(unname(m$priors), c(0.8, 0.2))
  expect_equal(ncol(m$canonical$coefficients), 1L)
  expect_true(m$accuracy_train > 0.9)
})

test_that("predict API: types, S3 wrappers, NA rows, errors", {
  m <- LDA$new()$fit(iris[, 1:4], iris$Species)
  p <- m$predict_proba(iris[1:5, 1:4])
  expect_equal(unname(rowSums(p)), rep(1, 5))
  expect_s3_class(predict(m, iris[1:5, 1:4]), "factor")
  expect_equal(dim(coef(m)), c(5, 3))
  nd <- iris[1:3, 1:4]; nd[2, 1] <- NA
  pr <- m$predict(nd)
  expect_true(is.na(pr[2])); expect_false(anyNA(pr[c(1, 3)]))
  expect_equal(ncol(m$transform(nd, n_axes = 1)), 1L)
  expect_error(LDA$new()$predict(nd), "not fitted")
  expect_error(m$predict(iris[, 1:2]), "Missing column")
})

test_that("input validation", {
  expect_error(LDA$new()$fit(iris[, 1:4], rep("a", 150)), "at least 2")
  expect_error(LDA$new(shrinkage = 2))
  expect_error(LDA$new()$fit(iris[, 1:4], iris$Species[1:10]), "same number")
  expect_warning(LDA$new()$fit(cbind(iris[, 1:4], k = 1), iris$Species), "Degenerate")
  expect_warning(LDA$new()$fit(cbind(iris[, 1:4], f = "a"), iris$Species), "Constant")
  X <- iris[, 1:4]; X[3, 2] <- NA
  expect_warning(LDA$new()$fit(X, iris$Species), "removed")
  expect_error(LDA$new(na_action = "fail")$fit(X, iris$Species), "Missing")
})
