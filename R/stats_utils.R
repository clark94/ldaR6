# Internal statistical helpers -------------------------------------------------
# None of these functions is exported. They operate on SSCP matrices
# (sums of squares and cross-products) so that they never need the raw data.

#' Log-determinant of a symmetric matrix
#'
#' Uses the Cholesky factor when the matrix is positive definite (fast and
#' stable); returns `NA` if the matrix is not positive definite.
#' @param M symmetric matrix.
#' @return a numeric scalar (`0` for an empty matrix).
#' @noRd
.logdet <- function(M) {
  if (length(M) == 0L) return(0)
  ch <- tryCatch(chol(M), error = function(e) NULL)
  if (is.null(ch)) return(NA_real_)
  2 * sum(log(diag(ch)))
}

#' Inverse of a symmetric (semi) positive definite matrix
#'
#' Tries Cholesky first. If the matrix is numerically singular, falls back to
#' a Moore-Penrose pseudo-inverse built from the eigen decomposition.
#' @param M symmetric matrix.
#' @param tol relative tolerance on eigenvalues for the pseudo-inverse.
#' @return list(inv, singular, rank).
#' @noRd
.spd_inverse <- function(M, tol = 1e-10) {
  p <- nrow(M)
  ch <- tryCatch(chol(M), error = function(e) NULL)
  if (!is.null(ch)) {
    # reciprocal condition check: tiny pivots signal near-singularity
    if (min(diag(ch))^2 > tol * max(diag(M))) {
      return(list(inv = chol2inv(ch), singular = FALSE, rank = p))
    }
  }
  e <- eigen(M, symmetric = TRUE)
  pos <- e$values > tol * max(e$values)
  V <- e$vectors[, pos, drop = FALSE]
  list(inv = V %*% (t(V) / e$values[pos]), singular = TRUE, rank = sum(pos))
}

#' Strict inverse: `NULL` when the matrix is (numerically) singular
#'
#' Partial tests are only defined for a non-singular SSCP matrix, so no
#' pseudo-inverse is used there and no costly eigen decomposition is attempted.
#' @noRd
.strict_inverse <- function(M, tol = 1e-10) {
  if (!nrow(M)) return(matrix(0, 0, 0))
  ch <- tryCatch(chol(M), error = function(e) NULL)
  if (is.null(ch) || min(diag(ch))^2 <= tol * max(diag(M))) return(NULL)
  chol2inv(ch)
}

#' Rao's F approximation of Wilks' lambda
#'
#' For a Wilks' lambda \eqn{\Lambda(q, h, e)} (q variables, h hypothesis degrees
#' of freedom, e error degrees of freedom) returns the F statistic and its
#' degrees of freedom. This is the approximation used by SAS PROC DISCRIM /
#' STEPDISC; it is exact when `q == 1` or `h <= 2`.
#' @param lambda numeric vector of Wilks' lambdas.
#' @param q number of variables, h hypothesis df, e error df (scalars).
#' @return data.frame(F, df1, df2, p).
#' @noRd
.rao_f <- function(lambda, q, h, e) {
  w <- e + h - (q + h + 1) / 2
  den <- q^2 + h^2 - 5
  t <- if (den > 0) sqrt((q^2 * h^2 - 4) / den) else 1
  df1 <- q * h
  df2 <- w * t - (q * h - 2) / 2
  if (!is.finite(df2) || df2 <= 0) {
    return(data.frame(F = rep(NA_real_, length(lambda)), df1 = df1, df2 = NA_real_,
                      p = NA_real_))
  }
  l1 <- lambda^(1 / t)
  Fv <- (1 - l1) / l1 * df2 / df1
  data.frame(F = Fv, df1 = df1, df2 = df2,
             p = stats::pf(Fv, df1, df2, lower.tail = FALSE))
}

#' Partial Wilks' lambda of candidate blocks given a current set of columns
#'
#' For each candidate block B of columns, computes
#' \eqn{\Lambda(B | S) = \det(W_{res,BB}) / \det(T_{res,BB})} where the residual
#' SSCP matrices are obtained after regressing B on the current set S
#' (covariance-adjusted). Singleton blocks are vectorised.
#'
#' @param W,T within-class and total SSCP matrices (full, all columns).
#' @param S integer vector of columns already in the model.
#' @param blocks list of integer vectors (candidate blocks).
#' @param n,K sample size and number of classes.
#' @return data.frame(lambda, F, df1, df2, p), one row per block.
#' @noRd
.candidate_stats <- function(W, T, S, blocks, n, K) {
  nb <- length(blocks)
  lam <- rep(NA_real_, nb)
  sizes <- lengths(blocks)
  h <- K - 1
  e0 <- n - K - length(S)

  na_out <- data.frame(lambda = lam, F = NA_real_, df1 = NA_real_, df2 = NA_real_, p = NA_real_)
  if (length(S) > 0L) {
    WSi <- .strict_inverse(W[S, S, drop = FALSE])
    TSi <- .strict_inverse(T[S, S, drop = FALSE])
    if (is.null(WSi) || is.null(TSi)) return(na_out)   # singular: partial tests undefined
  }
  resid_block <- function(M, Mi, cb) {
    if (length(S) == 0L) return(M[cb, cb, drop = FALSE])
    M[cb, cb, drop = FALSE] - crossprod(M[S, cb, drop = FALSE], Mi %*% M[S, cb, drop = FALSE])
  }
  # singletons, vectorised
  one <- which(sizes == 1L)
  if (length(one)) {
    cols <- unlist(blocks[one])
    if (length(S) == 0L) {
      wd <- diag(W)[cols]; td <- diag(T)[cols]
    } else {
      wd <- diag(W)[cols] - colSums(W[S, cols, drop = FALSE] * (WSi %*% W[S, cols, drop = FALSE]))
      td <- diag(T)[cols] - colSums(T[S, cols, drop = FALSE] * (TSi %*% T[S, cols, drop = FALSE]))
    }
    ok <- wd > 1e-10 * diag(W)[cols] & td > 0
    lam[one] <- ifelse(ok, wd / td, NA_real_)
  }
  for (b in which(sizes > 1L)) {
    cb <- blocks[[b]]
    lw <- .logdet(resid_block(W, if (length(S)) WSi, cb))
    lt <- .logdet(resid_block(T, if (length(S)) TSi, cb))
    lam[b] <- if (is.na(lw) || is.na(lt)) NA_real_ else exp(lw - lt)
  }
  out <- data.frame(lambda = lam, F = NA_real_, df1 = NA_real_, df2 = NA_real_, p = NA_real_)
  for (q in unique(sizes)) {
    idx <- which(sizes == q & !is.na(lam))
    if (length(idx)) out[idx, c("F", "df1", "df2", "p")] <- .rao_f(lam[idx], q, h, e0)
  }
  out
}

#' Partial Wilks' lambda of blocks already in the model (removal statistics)
#'
#' Uses the identity \eqn{\Lambda(B | S\setminus B) =
#' \det((T_{SS}^{-1})_{BB}) / \det((W_{SS}^{-1})_{BB})}.
#' @inheritParams .candidate_stats
#' @param blocks blocks (subsets of S) to evaluate.
#' @noRd
.removal_stats <- function(W, T, S, blocks, n, K) {
  nb <- length(blocks)
  out <- data.frame(lambda = rep(NA_real_, nb), F = NA_real_, df1 = NA_real_,
                    df2 = NA_real_, p = NA_real_)
  if (!length(S)) return(out)
  Wi <- .strict_inverse(W[S, S, drop = FALSE])
  Ti <- .strict_inverse(T[S, S, drop = FALSE])
  if (is.null(Wi) || is.null(Ti)) return(out)          # singular: partial tests undefined
  pos <- match(unlist(blocks), S)
  split_pos <- split(pos, rep(seq_len(nb), lengths(blocks)))
  h <- K - 1
  for (b in seq_len(nb)) {
    ib <- split_pos[[b]]
    q <- length(ib)
    lw <- .logdet(Wi[ib, ib, drop = FALSE]); lt <- .logdet(Ti[ib, ib, drop = FALSE])
    if (is.na(lw) || is.na(lt)) next
    lam <- exp(lt - lw)
    out[b, "lambda"] <- lam
    out[b, c("F", "df1", "df2", "p")] <- .rao_f(lam, q, h, n - K - (length(S) - q))
  }
  out
}
