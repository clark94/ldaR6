test_that("wide data (p > n) works with shrinkage and warns without", {
  d <- simulate_lda_data(n = 60, p = 150, K = 3, seed = 1)
  expect_warning(LDA$new()$fit(d[, -1], d$y), "singular")
  m <- LDA$new(shrinkage = 0.2)$fit(d[, -1], d$y)
  expect_false(anyNA(m$predict(d[, -1])))
  expect_true(m$accuracy_train > 0.8)
  m2 <- LDA$new(selection = "forward", alpha_enter = 0.01)$fit(d[, -1], d$y)
  expect_lt(length(m2$selected), 60)
})

test_that("long data is handled in chunks with identical results", {
  d <- simulate_lda_data(n = 5000, p = 6, K = 3, seed = 2)
  a <- LDA$new()$fit(d[, -1], d$y)
  b <- LDA$new(chunk_size = 700)$fit(d[, -1], d$y)
  expect_equal(a$coefficients, b$coefficients, tolerance = 1e-9)
})

test_that("predictions are invariant to scale and shift of a variable", {
  d <- simulate_lda_data(n = 400, p = 4, K = 3, seed = 3)
  X2 <- d[, -1]; X2$x1 <- X2$x1 * 1000 + 50
  a <- LDA$new()$fit(d[, -1], d$y); b <- LDA$new()$fit(X2, d$y)
  expect_equal(as.character(a$predict(d[, -1])), as.character(b$predict(X2)))
})

test_that("a class with a single observation does not crash", {
  d <- iris[c(1:50, 51:100, 101), ]
  expect_warning(m <- LDA$new()$fit(d[, 1:4], d$Species), "fewer than 2")
  expect_false(anyNA(m$predict(d[, 1:4])))
})

test_that("read_data reads tab separated files", {
  f <- system.file("extdata", "credit_demo.tsv", package = "ldaR6")
  skip_if(f == "")
  d <- read_data(f)
  expect_equal(nrow(d), 900L)
})

test_that("print/summary/plot run without error", {
  m <- LDA$new(selection = "both")$fit(iris[, 1:4], iris$Species)
  expect_output(print(m), "Linear Discriminant")
  expect_output(m$summary(), "Classification functions")
  pdf(NULL); on.exit(dev.off())
  expect_no_error(m$plot("scores"))
  expect_no_error(m$plot("importance"))
  expect_no_error(m$plot("axes"))
  expect_output(print(LDA$new()), "not fitted")
})
