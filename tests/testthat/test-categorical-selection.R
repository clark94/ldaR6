test_that("categorical encodings: dummy and onehot (pseudo-inverse) give the same predictions", {
  data(credit_demo)
  X <- credit_demo[, -1]; y <- credit_demo$risk
  a <- LDA$new(encoding = "dummy")$fit(X, y)
  b <- suppressWarnings(LDA$new(encoding = "onehot")$fit(X, y))
  expect_equal(as.character(a$predict(X)), as.character(b$predict(X)))
  expect_equal(ncol(LDA$new(encoding = "label")$fit(X, y)$coefficients), 3L)
  expect_warning(LDA$new(encoding = "onehot")$fit(X, y), "singular")
  expect_true(all(c("housing=rent", "housing=free") %in% rownames(a$coefficients)))
})

test_that("unseen level gives NA with a warning; factor/character are equivalent", {
  data(credit_demo)
  X <- credit_demo[, -1]; y <- credit_demo$risk
  m <- LDA$new()$fit(X, y)
  nd <- X[1:4, ]; nd$housing <- as.character(nd$housing); nd$job <- as.character(nd$job)
  expect_equal(as.character(m$predict(nd)), as.character(m$predict(X[1:4, ])))
  nd$housing[2] <- "castle"
  expect_warning(p <- m$predict(nd), "unseen")
  expect_true(is.na(p[2]))
})

test_that("stepwise selection drops noise and redundant variables", {
  data(credit_demo)
  X <- credit_demo[, -1]; y <- credit_demo$risk
  for (s in c("forward", "backward", "both")) {
    m <- LDA$new(selection = s)$fit(X, y)
    expect_false(any(c("noise1", "noise2", "job") %in% m$selected))
    expect_true(all(c("income", "debt_ratio", "savings") %in% m$selected))
    expect_equal(sum(c("income", "income_copy") %in% m$selected), 1L)
  }
  m <- LDA$new()$fit(X, y)
  expect_equal(m$variables$status[m$variables$variable == "noise1"], "uninformative")
  expect_equal(m$variables$status[m$variables$variable == "income_copy"], "redundant")
})

test_that("max_vars is honoured; selection path is recorded", {
  data(credit_demo)
  m <- LDA$new(selection = "forward", max_vars = 3)$fit(credit_demo[, -1], credit_demo$risk)
  expect_equal(length(m$selected), 3L)
  expect_equal(nrow(m$selection_history), 3L)
})

test_that("prediction needs only the selected variables", {
  data(credit_demo)
  m <- LDA$new(selection = "forward")$fit(credit_demo[, -1], credit_demo$risk)
  expect_s3_class(m$predict(credit_demo[1:5, m$selected]), "factor")
})

test_that("cross-validation is reproducible", {
  data(credit_demo)
  m <- LDA$new(selection = "forward")$fit(credit_demo[, -1], credit_demo$risk)
  a <- m$cross_validate(credit_demo[, -1], credit_demo$risk, k = 5, seed = 3)
  b <- m$cross_validate(credit_demo[, -1], credit_demo$risk, k = 5, seed = 3)
  expect_equal(a$accuracy, b$accuracy)
  expect_true(a$accuracy > 0.7 && a$accuracy <= 1)
  expect_equal(sum(a$confusion), nrow(credit_demo))
})

test_that("illustrative variables are described on the axes", {
  data(credit_demo)
  m <- LDA$new()$fit(credit_demo[, c("income", "debt_ratio", "savings")], credit_demo$risk)
  ill <- m$project_illustrative(credit_demo[, c("age", "job")])
  expect_equal(dim(ill$numeric), c(1L, 2L))
  expect_equal(nrow(ill$categorical), nlevels(credit_demo$job))
})
