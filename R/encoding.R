# Encoding of explanatory variables ---------------------------------------------
#
# LDA works on numeric matrices. Categorical explanatory variables are recoded
# upstream of the algorithm (the matrix algebra itself is untouched):
#   * "dummy"  : k-1 indicator columns per factor (reference = first level).
#                Default: keeps the pooled covariance matrix non-singular.
#   * "onehot" : k indicator columns. The k columns sum to 1 (collinear with the
#                intercept) so the pooled covariance is singular; the package
#                then falls back on a pseudo-inverse (or use `shrinkage`).
#   * "label"  : one integer column per factor (levels order). Compact, but
#                imposes an order/distance between modalities; reasonable only
#                for ordered factors.
#
# The encoder is learnt on the training data (levels, column names) and
# re-applied unchanged to new data, which guarantees consistency at predict().

.is_categorical <- function(x) is.factor(x) || is.character(x) || is.logical(x)

#' Learn an encoder from a data.frame
#' @return list(columns = per-variable metadata, names = encoded column names,
#'   blocks = list mapping each source variable to its encoded column indices)
#' @noRd
.make_encoder <- function(X, encoding = c("dummy", "onehot", "label")) {
  encoding <- match.arg(encoding)
  vars <- names(X)
  meta <- vector("list", length(vars)); names(meta) <- vars
  col_names <- character(0); blocks <- list(); cur <- 0L
  for (v in vars) {
    x <- X[[v]]
    if (.is_categorical(x)) {
      f <- if (is.factor(x)) droplevels(x) else factor(x)
      lv <- levels(f)
      if (length(lv) < 2L) {
        meta[[v]] <- list(type = "constant", levels = lv)
        next
      }
      used <- switch(encoding, dummy = lv[-1L], onehot = lv, label = lv[1L])
      nm <- if (encoding == "label") v else paste0(v, "=", used)
      meta[[v]] <- list(type = "factor", levels = lv, used = used, encoding = encoding,
                        ordered = is.ordered(f))
    } else if (is.numeric(x) || is.integer(x)) {
      nm <- v
      meta[[v]] <- list(type = "numeric", center = mean(x, na.rm = TRUE))
    } else {
      stop(sprintf("Column '%s' has unsupported type '%s'.", v, class(x)[1]), call. = FALSE)
    }
    idx <- cur + seq_along(nm)
    cur <- cur + length(nm)
    col_names <- c(col_names, nm)
    blocks[[v]] <- idx
  }
  list(columns = meta, names = col_names, blocks = blocks, encoding = encoding)
}

#' Apply a learnt encoder to a data.frame
#' @param strict if TRUE unseen levels raise an error, otherwise the row gets NA
#' @return list(matrix, unseen = logical vector of rows with unseen levels)
#' @noRd
.apply_encoder <- function(enc, X) {
  X <- as.data.frame(X, stringsAsFactors = FALSE)
  need <- names(enc$blocks)
  miss <- setdiff(need, names(X))
  if (length(miss)) {
    stop("Missing column(s) in new data: ", paste(miss, collapse = ", "), call. = FALSE)
  }
  n <- nrow(X)
  M <- matrix(NA_real_, n, length(enc$names), dimnames = list(NULL, enc$names))
  unseen <- logical(n)
  for (v in need) {
    meta <- enc$columns[[v]]
    idx <- enc$blocks[[v]]
    x <- X[[v]]
    if (meta$type == "numeric") {
      if (!is.numeric(x)) {
        x <- suppressWarnings(as.numeric(as.character(x)))
      }
      M[, idx] <- x
    } else {
      code <- match(as.character(x), meta$levels)
      unseen <- unseen | (is.na(code) & !is.na(x))
      if (meta$encoding == "label") {
        M[, idx] <- code
      } else {
        lv_idx <- match(meta$used, meta$levels)
        # indicator matrix built by vectorised comparison with the integer codes
        for (j in seq_along(idx)) M[, idx[j]] <- as.numeric(code == lv_idx[j])
      }
    }
  }
  list(matrix = M, unseen = unseen)
}
