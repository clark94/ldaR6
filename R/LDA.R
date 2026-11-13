#' Predictive Linear Discriminant Analysis (R6 class)
#'
#' @description
#' `LDA` implements the predictive linear discriminant analysis with a
#' scikit-learn like interface (`$fit()`, `$predict()`, `$predict_proba()`,
#' `$transform()`). The algorithm is programmed from scratch and follows the
#' definitions of SAS `PROC DISCRIM` (method = normal, pooled covariance matrix):
#' each class `k` gets a linear classification function
#' \deqn{f_k(x) = \ln \pi_k - \tfrac12 \mu_k' \Sigma^{-1} \mu_k + (\Sigma^{-1}\mu_k)' x}
#' where \eqn{\Sigma = W / (n - K)} is the pooled within-class covariance matrix
#' and \eqn{\pi_k} the prior probability. An observation is assigned to the
#' class with the largest score; posterior probabilities are the softmax of the
#' scores.
#'
#' @details
#' **Computation.** The model only needs the class means and the within-class
#' SSCP matrix \eqn{W}. These are obtained in one pass over the data (by
#' chunks of `chunk_size` rows, so memory stays bounded for very long data)
#' with BLAS-level `crossprod`. All tests and the variable selection work from
#' the \eqn{p \times p} matrices \eqn{W} and \eqn{T = W + B}, therefore their
#' cost does not depend on the number of observations.
#'
#' **Categorical predictors.** Explanatory variables of type `factor`,
#' `character` or `logical` are recoded before the matrix computation, see
#' the `encoding` argument. The recoding is learnt at `$fit()` and replayed
#' identically at `$predict()`.
#'
#' **Variable selection.** With `selection = "forward"`, `"backward"` or
#' `"both"` (stepwise) the variables are selected using the partial Wilks'
#' lambda and its Rao F test (equivalent to SAS `PROC STEPDISC`). A categorical
#' variable enters or leaves as a whole (multivariate test on its indicator
#' columns).
#'
#' **High dimension.** When the pooled covariance matrix is singular (more
#' columns than `n - K`, collinear columns, one-hot encoding) the package falls
#' back on a pseudo-inverse and warns. A `shrinkage` value in (0, 1] replaces
#' \eqn{\Sigma} by \eqn{(1-\gamma)\Sigma + \gamma\, \mathrm{diag}(\Sigma)},
#' which is always invertible and invariant to the scale of the variables; combine it with `selection = "forward"` for
#' very wide data.
#'
#' @section Fields (read-only active bindings):
#' * `coefficients`: matrix (p + 1) x K of classification function
#'   coefficients; first row is the constant.
#' * `classes`, `priors`, `means`, `pooled_cov`, `selected`, `variables`,
#'   `wilks`, `canonical`, `distances`, `selection_history`,
#'   `confusion_train`, `accuracy_train`, `scores_train`, `features`.
#'
#' @examples
#' m <- LDA$new()
#' m$fit(iris[, 1:4], iris$Species)
#' m
#' head(m$predict(iris[, 1:4]))
#' head(m$predict_proba(iris[, 1:4]))
#' m$coefficients
#' m$wilks$global
#' m$summary()
#'
#' # stepwise selection and categorical predictors
#' data(credit_demo)
#' m2 <- LDA$new(selection = "both", encoding = "dummy")
#' m2$fit(credit_demo[, -1], credit_demo$risk)
#' m2$variables
#' m2$cross_validate(credit_demo[, -1], credit_demo$risk, k = 5, seed = 1)
#' @export
LDA <- R6::R6Class(
  "LDA",
  public = list(

    #' @description Create an (unfitted) LDA model.
    #' @param priors `"proportional"` (class frequencies, default), `"equal"`,
    #'   or a numeric vector of prior probabilities (named by class, or in
    #'   the order of the factor levels); it is normalised to sum to 1.
    #' @param encoding encoding of categorical predictors: `"dummy"`
    #'   (k-1 indicators, default), `"onehot"` (k indicators, singular
    #'   covariance handled by pseudo-inverse) or `"label"` (integer codes).
    #' @param selection variable selection: `"none"` (default), `"forward"`,
    #'   `"backward"` or `"both"` (stepwise).
    #' @param alpha_enter,alpha_stay p-value thresholds for entering and staying
    #'   in the model (partial Wilks' lambda F-test).
    #' @param max_vars maximum number of variables retained (forward / both).
    #' @param shrinkage covariance shrinkage intensity in `[0, 1]` (0 = none).
    #' @param na_action `"omit"` (default, rows with missing values are
    #'   removed with a warning) or `"fail"`.
    #' @param chunk_size number of rows processed at once when accumulating the
    #'   within-class SSCP matrix (memory control).
    #' @return a new `LDA` object.
    initialize = function(priors = "proportional",
                          encoding = c("dummy", "onehot", "label"),
                          selection = c("none", "forward", "backward", "both"),
                          alpha_enter = 0.05, alpha_stay = 0.05, max_vars = NULL,
                          shrinkage = 0, na_action = c("omit", "fail"),
                          chunk_size = 200000L) {
      encoding <- match.arg(encoding)
      selection <- match.arg(selection)
      na_action <- match.arg(na_action)
      stopifnot(is.numeric(shrinkage), length(shrinkage) == 1L, shrinkage >= 0, shrinkage <= 1,
                alpha_enter > 0, alpha_enter <= 1, alpha_stay > 0, alpha_stay <= 1,
                chunk_size >= 1L)
      if (!(is.character(priors) && priors %in% c("proportional", "equal")) && !is.numeric(priors)) {
        stop("`priors` must be 'proportional', 'equal' or a numeric vector.", call. = FALSE)
      }
      if (selection == "both" && alpha_enter > alpha_stay) {
        warning("alpha_enter > alpha_stay may make variables enter and leave in loops.", call. = FALSE)
      }
      private$cfg <- list(priors = priors, encoding = encoding, selection = selection,
                             alpha_enter = alpha_enter, alpha_stay = alpha_stay,
                             max_vars = max_vars, shrinkage = shrinkage,
                             na_action = na_action, chunk_size = as.integer(chunk_size))
      invisible(self)
    },

    #' @description Fit the model on training data.
    #' @param X a data.frame (or matrix) of explanatory variables; numeric,
    #'   factor, character or logical columns are accepted.
    #' @param y a factor (or vector coercible to factor) with at least 2 classes.
    #' @return the object itself (invisibly), to allow chaining.
    fit = function(X, y) {
      pr <- private$cfg
      X <- private$as_frame(X)
      y <- if (is.factor(y)) y else factor(y)
      if (length(y) != nrow(X)) stop("`X` and `y` must have the same number of rows.", call. = FALSE)
      ok <- stats::complete.cases(X) & !is.na(y)
      if (!all(ok)) {
        if (pr$na_action == "fail") stop("Missing values found (na_action = 'fail').", call. = FALSE)
        warning(sprintf("%d row(s) with missing values were removed.", sum(!ok)), call. = FALSE)
      }
      kept_rows <- which(ok)
      X <- X[ok, , drop = FALSE]
      y <- droplevels(y[ok])
      n <- nrow(X); lev <- levels(y); K <- length(lev)
      if (K < 2L) stop("The target must have at least 2 classes.", call. = FALSE)
      if (n - K < 1L) stop("Not enough observations (need n > number of classes).", call. = FALSE)
      nk <- as.numeric(table(y)); names(nk) <- lev
      if (any(nk < 2L)) warning("Some classes have fewer than 2 observations.", call. = FALSE)

      # ---- 1. encoding ------------------------------------------------------
      enc <- .make_encoder(X, pr$encoding)
      const_vars <- names(Filter(function(m) m$type == "constant", enc$columns))
      if (length(const_vars)) {
        warning("Constant variable(s) ignored: ", paste(const_vars, collapse = ", "), call. = FALSE)
      }
      if (!length(enc$blocks)) stop("No usable explanatory variable.", call. = FALSE)
      M <- .apply_encoder(enc, X)$matrix
      pall <- ncol(M)

      # ---- 2. sufficient statistics: means, W, B, T --------------------------
      S1 <- rowsum(M, y, reorder = TRUE)[lev, , drop = FALSE]
      means <- S1 / nk
      grand <- colSums(S1) / n
      Bss <- crossprod(sqrt(nk) * sweep(means, 2, grand))
      Wss <- matrix(0, pall, pall)
      yi <- as.integer(y)
      for (s in seq.int(1L, n, by = pr$chunk_size)) {
        idx <- s:min(n, s + pr$chunk_size - 1L)
        Wss <- Wss + crossprod(M[idx, , drop = FALSE] - means[yi[idx], , drop = FALSE])
      }
      Tss <- Wss + Bss

      # columns with no within-class variability are removed (singular W)
      bad <- diag(Wss) <= 1e-12 * diag(Tss)
      keep <- which(!bad)
      if (!length(keep)) stop("All explanatory columns are degenerate.", call. = FALSE)
      if (any(bad)) {
        warning("Degenerate column(s) removed (no within-class variance): ",
                paste(enc$names[bad], collapse = ", "), call. = FALSE)
      }
      Wss <- Wss[keep, keep, drop = FALSE]; Tss <- Tss[keep, keep, drop = FALSE]
      Bss <- Bss[keep, keep, drop = FALSE]
      means <- means[, keep, drop = FALSE]; grand <- grand[keep]
      blocks <- lapply(enc$blocks, function(ix) stats::na.omit(match(ix, keep)))
      blocks <- lapply(blocks, as.integer)
      blocks <- blocks[lengths(blocks) > 0L]

      # ---- 3. priors ---------------------------------------------------------
      priors <- private$make_priors(pr$priors, nk, lev)

      # ---- 4. variable selection --------------------------------------------
      hist <- NULL
      if (pr$selection == "none") {
        sel <- names(blocks)
      } else {
        dir <- pr$selection
        r <- .select_variables(Wss, Tss, blocks, n, K, dir, pr$alpha_enter, pr$alpha_stay, pr$max_vars)
        sel <- r$selected; hist <- r$history
        if (!length(sel)) {
          stop("No variable passed the selection threshold; increase `alpha_enter`.", call. = FALSE)
        }
      }
      sc <- sort(unlist(blocks[sel], use.names = FALSE))   # selected columns (kept space)
      p <- length(sc)
      cols_used <- keep[sc]                               # selected columns (encoder space)

      # ---- 5. final model on the selected columns ---------------------------
      Wf <- Wss[sc, sc, drop = FALSE]; Tf <- Tss[sc, sc, drop = FALSE]
      Bf <- Bss[sc, sc, drop = FALSE]
      mf <- means[, sc, drop = FALSE]; gf <- grand[sc]
      dfe <- n - K
      Sigma <- Wf / dfe
      g <- pr$shrinkage
      if (g > 0) Sigma <- (1 - g) * Sigma + g * diag(diag(Sigma), p)   # shrink towards the diagonal (scale invariant)
      inv <- .spd_inverse(Sigma)
      if (inv$singular) {
        warning("Pooled covariance matrix is singular (rank ", inv$rank, " < ", p,
                "): a pseudo-inverse is used. Consider `shrinkage` > 0, a variable ",
                "selection or `encoding = 'dummy'`.", call. = FALSE)
      }
      cn <- enc$names[cols_used]
      B <- inv$inv %*% t(mf)
      const <- log(priors) - 0.5 * colSums(t(mf) * B)
      dimnames(B) <- list(cn, lev); names(const) <- lev
      Sigma_named <- Sigma; dimnames(Sigma_named) <- list(cn, cn)

      # ---- 6. global tests (Wilks) -------------------------------------------
      lw <- .logdet(Wf); lt <- .logdet(Tf)
      lambda <- if (is.na(lw) || is.na(lt)) NA_real_ else exp(lw - lt)
      glob <- if (is.na(lambda)) {
        data.frame(statistic = "Wilks' lambda", value = NA_real_, F = NA_real_, df1 = NA_real_,
                   df2 = NA_real_, p_value = NA_real_)
      } else {
        rf <- .rao_f(lambda, p, K - 1, dfe)
        data.frame(statistic = "Wilks' lambda", value = lambda, F = rf$F, df1 = rf$df1,
                   df2 = rf$df2, p_value = rf$p)
      }
      bart <- if (is.na(lambda)) NA_real_ else -(n - 1 - (p + K) / 2) * log(lambda)

      # ---- 7. canonical (discriminant) axes ---------------------------------
      # B = D D' has rank <= K-1, so the generalised eigenproblem B a = l Sigma a
      # reduces to the K x K matrix D' Sigma^-1 D (no p x p eigen decomposition).
      D <- t(sqrt(nk) * sweep(mf, 2, gf))                 # p x K
      SiD <- inv$inv %*% D
      Cm <- crossprod(D, SiD)
      eC <- eigen((Cm + t(Cm)) / 2, symmetric = TRUE)
      r <- max(1L, min(K - 1L, inv$rank))
      lam_c <- pmax(eC$values[seq_len(r)], 0)
      ev <- lam_c / dfe                                   # eigenvalues of W^-1 B
      A <- SiD %*% eC$vectors[, seq_len(r), drop = FALSE] %*%
        diag(1 / sqrt(pmax(lam_c, .Machine$double.eps)), r)
      for (j in seq_len(r)) if (A[which.max(abs(A[, j])), j] < 0) A[, j] <- -A[, j]
      dimnames(A) <- list(cn, paste0("LD", seq_len(r)))
      cancor <- sqrt(ev / (1 + ev))
      axis_tests <- NULL
      if (!is.na(lambda) && g == 0) {
        lam_j <- rev(cumprod(rev(1 / (1 + ev))))
        chi <- -(n - 1 - (p + K) / 2) * log(lam_j)
        dfj <- (p - seq_len(r) + 1) * (K - seq_len(r))
        axis_tests <- data.frame(axis = colnames(A), eigenvalue = ev,
                                 proportion = ev / sum(ev), canonical_corr = cancor,
                                 wilks = lam_j, chi2 = chi, df = dfj,
                                 p_value = stats::pchisq(chi, dfj, lower.tail = FALSE))
      } else {
        axis_tests <- data.frame(axis = colnames(A), eigenvalue = ev,
                                 proportion = ev / sum(ev), canonical_corr = cancor)
      }
      structure_cor <- (Sigma %*% A) / sqrt(diag(Sigma)); dimnames(structure_cor) <- dimnames(A)
      Msel <- M[, cols_used, drop = FALSE]
      scores_train <- sweep(Msel, 2, gf) %*% A; colnames(scores_train) <- colnames(A)
      centroids <- sweep(mf, 2, gf) %*% A; dimnames(centroids) <- list(lev, colnames(A))
      multi <- data.frame(
        test = c("Wilks' lambda", "Pillai's trace", "Hotelling-Lawley trace", "Roy's greatest root"),
        value = c(lambda, sum(ev / (1 + ev)), sum(ev), max(ev)))
      if (is.na(lambda) || g > 0) multi$value[] <- NA_real_

      # ---- 8. Mahalanobis distances between classes -------------------------
      Dm <- mf %*% inv$inv %*% t(mf)
      D2 <- outer(diag(Dm), diag(Dm), "+") - 2 * Dm
      dimnames(D2) <- list(lev, lev)
      df2d <- n - K - p + 1
      Pd <- matrix(NA_real_, K, K, dimnames = dimnames(D2))
      if (df2d > 0 && g == 0) {
        Fd <- df2d * outer(nk, nk) / (p * dfe * outer(nk, nk, "+")) * D2
        Pd <- stats::pf(Fd, p, df2d, lower.tail = FALSE); diag(Pd) <- NA
        dimnames(Pd) <- dimnames(D2)
      }

      # ---- 9. resubstitution -----------------------------------------------
      Z <- Msel %*% B + rep(const, each = n)
      pred <- factor(lev[max.col(Z, ties.method = "first")], levels = lev)
      conf <- table(Observed = y, Predicted = pred)

      # ---- 10. per-variable diagnostics --------------------------------------
      vt <- private$variable_table(Wss, Tss, blocks, sel, sc, n, K, pr, enc)

      pred_enc <- .subset_encoder(enc, cols_used)
      feats <- lapply(names(X), function(v) {
        x <- X[[v]]
        if (.is_categorical(x)) list(type = "factor", levels = levels(factor(x)))
        else list(type = "numeric", mean = mean(x), min = min(x), max = max(x))
      })
      names(feats) <- names(X)

      private$res <- list(
        n = n, K = K, p = p, classes = lev, n_k = nk, priors = priors, means = means[, sc, drop = FALSE],
        grand = gf, pooled = Sigma_named, coef = B, const = const, encoder = pred_enc,
        full_encoder_names = enc$names, selected = sel, history = hist,
        wilks = list(global = glob, multivariate = multi, bartlett_chi2 = bart,
                     bartlett_df = p * (K - 1), bartlett_p = if (is.na(bart)) NA_real_ else
                       stats::pchisq(bart, p * (K - 1), lower.tail = FALSE)),
        canonical = list(coefficients = A, eigenvalues = ev, axes = axis_tests,
                         structure = structure_cor, centroids = centroids, A = A, center = gf),
        distances = list(d2 = D2, p_value = Pd),
        confusion = conf, accuracy = sum(diag(conf)) / n, scores = scores_train,
        kept_rows = kept_rows, variables = vt, features = feats, y = y,
        singular = inv$singular, rank = inv$rank, const_vars = const_vars
      )
      invisible(self)
    },

    #' @description Predict with a fitted model.
    #' @param X data.frame with (at least) the variables used by the model.
    #' @param type `"class"` (default): predicted factor; `"prob"`: matrix of
    #'   posterior probabilities; `"score"`: matrix of classification function
    #'   values.
    #' @return a factor or a numeric matrix, one row per row of `X`. Rows with
    #'   missing values or unseen categorical levels get `NA`.
    predict = function(X, type = c("class", "prob", "score")) {
      type <- match.arg(type)
      Z <- private$scores_for(X)
      switch(type,
        score = Z,
        prob = {
          Zc <- Z - apply(Z, 1, max)
          Ez <- exp(Zc)
          Ez / rowSums(Ez)
        },
        class = {
          idx <- max.col(replace(Z, is.na(Z), -Inf), ties.method = "first")
          out <- factor(private$res$classes[idx], levels = private$res$classes)
          out[rowSums(is.na(Z)) > 0] <- NA
          out
        })
    },

    #' @description Posterior class probabilities (shortcut for `predict(type = "prob")`).
    #' @param X data.frame of explanatory variables.
    #' @return numeric matrix, one column per class.
    predict_proba = function(X) self$predict(X, type = "prob"),

    #' @description Values of the classification functions (SAS "linear discriminant function").
    #' @param X data.frame of explanatory variables.
    #' @return numeric matrix, one column per class.
    decision_function = function(X) self$predict(X, type = "score"),

    #' @description Project observations on the canonical (discriminant) axes.
    #' @param X data.frame of explanatory variables.
    #' @param n_axes number of axes to return (default: all).
    #' @return numeric matrix with columns `LD1`, `LD2`, ...
    transform = function(X, n_axes = NULL) {
      private$check()
      cn <- private$res$canonical
      M <- private$encode_new(X)
      Zc <- sweep(M, 2, cn$center) %*% cn$A
      if (!is.null(n_axes)) Zc <- Zc[, seq_len(min(n_axes, ncol(Zc))), drop = FALSE]
      Zc
    },

    #' @description Accuracy on a labelled data set.
    #' @param X data.frame of explanatory variables.
    #' @param y true classes.
    #' @return the proportion of correctly classified observations.
    score = function(X, y) {
      p <- self$predict(X)
      mean(as.character(p) == as.character(y), na.rm = TRUE)
    },

    #' @description Confusion matrix on a labelled data set.
    #' @param X data.frame of explanatory variables.
    #' @param y true classes.
    #' @return a contingency table (rows: observed, columns: predicted).
    confusion = function(X, y) {
      private$check()
      lev <- private$res$classes
      table(Observed = factor(y, levels = lev), Predicted = self$predict(X))
    },

    #' @description K-fold cross-validation of the whole pipeline (encoding,
    #'   selection and fit are redone inside each fold, so the estimate is not
    #'   biased by the selection).
    #' @param X,y data (as in `$fit()`).
    #' @param k number of folds.
    #' @param seed optional seed for reproducible folds.
    #' @param stratified stratify folds by class (default `TRUE`).
    #' @return an object of class `ldaR6_cv` (list with `accuracy`, `error`,
    #'   `confusion`, `fold_accuracy`, `predictions`).
    cross_validate = function(X, y, k = 10, seed = NULL, stratified = TRUE) {
      X <- private$as_frame(X)
      y <- if (is.factor(y)) y else factor(y)
      ok <- stats::complete.cases(X) & !is.na(y)
      X <- X[ok, , drop = FALSE]; y <- droplevels(y[ok])
      folds <- .make_folds(y, k, seed, stratified)
      pred <- factor(rep(NA_character_, length(y)), levels = levels(y))
      facc <- numeric(k)
      for (f in seq_len(k)) {
        te <- which(folds == f)
        m <- do.call(LDA$new, private$cfg)
        fitted <- tryCatch(suppressWarnings(m$fit(X[-te, , drop = FALSE], y[-te])),
                           error = function(e) NULL)
        if (is.null(fitted)) { facc[f] <- NA_real_; next }
        pred[te] <- suppressWarnings(m$predict(X[te, , drop = FALSE]))
        facc[f] <- mean(pred[te] == y[te], na.rm = TRUE)
      }
      conf <- table(Observed = y, Predicted = pred)
      acc <- mean(pred == y, na.rm = TRUE)
      structure(list(accuracy = acc, error = 1 - acc, confusion = conf, fold_accuracy = facc,
                     predictions = pred, k = k), class = "ldaR6_cv")
    },

    #' @description Classify each variable of the final model as contributing,
    #'   redundant, uninformative or *perturbing*. A variable is perturbing when
    #'   removing it increases the cross-validated accuracy.
    #' @param X,y training data (as in `$fit()`).
    #' @param k number of CV folds.
    #' @param seed seed for the (common) folds.
    #' @return data.frame with the CV accuracy with / without each selected
    #'   variable and a `verdict`.
    variable_audit = function(X, y, k = 5, seed = 1) {
      private$check()
      sel <- private$res$selected
      X <- private$as_frame(X)
      base_params <- private$cfg; base_params$selection <- "none"
      cv_acc <- function(vars) {
        m <- do.call(LDA$new, base_params)
        m$cross_validate(X[, vars, drop = FALSE], y, k = k, seed = seed)$accuracy
      }
      base <- cv_acc(sel)
      out <- data.frame(variable = sel, cv_accuracy_without = NA_real_, delta = NA_real_,
                        verdict = NA_character_, stringsAsFactors = FALSE)
      for (i in seq_along(sel)) {
        if (length(sel) < 2L) break
        a <- cv_acc(setdiff(sel, sel[i]))
        out$cv_accuracy_without[i] <- a
        out$delta[i] <- base - a
        out$verdict[i] <- if (a > base + 1e-12) "perturbing" else if (a < base - 1e-12) "useful" else "neutral"
      }
      attr(out, "cv_accuracy_full") <- base
      out
    },

    #' @description Describe illustrative (supplementary) variables on the
    #'   discriminant axes: correlation for numeric variables, barycentres for
    #'   categorical ones. Illustrative variables do not take part in the model.
    #' @param Z data.frame with the same rows (same order) as the data given
    #'   to `$fit()`.
    #' @return list(numeric = correlation matrix, categorical = data.frame of
    #'   barycentres).
    project_illustrative = function(Z) {
      private$check()
      Z <- as.data.frame(Z)
      rows <- private$res$kept_rows
      if (max(rows) > nrow(Z)) stop("`Z` must have the same rows as the data used in $fit().", call. = FALSE)
      Z <- Z[rows, , drop = FALSE]
      sc <- private$res$scores
      num <- Filter(is.numeric, Z)
      cor_m <- if (ncol(num)) {
        do.call(rbind, lapply(num, function(v) as.vector(stats::cor(v, sc, use = "pairwise.complete.obs"))))
      } else NULL
      if (!is.null(cor_m)) colnames(cor_m) <- colnames(sc)
      cat_df <- NULL
      for (v in names(Filter(.is_categorical, Z))) {
        f <- factor(Z[[v]])
        bc <- rowsum(sc[!is.na(f), , drop = FALSE], f[!is.na(f)]) / as.vector(table(f))
        cat_df <- rbind(cat_df, data.frame(variable = v, level = rownames(bc),
                                           n = as.vector(table(f)), bc, row.names = NULL))
      }
      list(numeric = cor_m, categorical = cat_df)
    },

    #' @description Concise information about the model.
    #' @param ... unused.
    print = function(...) {
      if (is.null(private$res)) {
        cat("<LDA> not fitted. Use $fit(X, y).\n")
        return(invisible(self))
      }
      r <- private$res
      cat("Linear Discriminant Analysis (R6 class 'LDA')\n")
      cat(sprintf("  Observations: %d | classes: %d (%s)\n", r$n, r$K, paste(r$classes, collapse = ", ")))
      cat(sprintf("  Variables kept: %d column(s) from %d variable(s)", r$p, length(r$selected)))
      cat(sprintf(" [selection: %s, encoding: %s]\n", private$cfg$selection, private$cfg$encoding))
      cat("  Priors: ", paste(sprintf("%s=%.3f", r$classes, r$priors), collapse = ", "), "\n", sep = "")
      g <- r$wilks$global
      if (!is.na(g$value)) {
        cat(sprintf("  Wilks' lambda = %.4g, Rao F(%g, %.4g) = %.4g, p %s\n", g$value, g$df1, g$df2, g$F,
                    .fmt_p(g$p_value)))
      }
      cat(sprintf("  Resubstitution accuracy: %.2f%%\n", 100 * r$accuracy))
      cat("  Use $summary() for details, $coefficients for the classification functions.\n")
      invisible(self)
    },

    #' @description Detailed report: classification functions, global tests,
    #'   canonical axes, variable contributions, class distances, confusion
    #'   matrix and selection path.
    #' @param ... unused.
    #' @return the object (invisibly).
    summary = function(...) {
      private$check()
      r <- private$res
      line <- function(t) cat("\n", t, "\n", strrep("-", nchar(t)), "\n", sep = "")
      cat("=== Linear Discriminant Analysis: summary ===\n")
      cat(sprintf("n = %d | K = %d | columns used = %d | pooled df = %d\n", r$n, r$K, r$p, r$n - r$K))
      if (r$singular) cat(sprintf("WARNING: singular pooled covariance (rank %d), pseudo-inverse used.\n", r$rank))
      line("Class sizes and priors")
      print(rbind(n = r$n_k, prior = round(r$priors, 4)))
      line("Classification functions (constant in first row)")
      print(round(self$coefficients, 5))
      line("Global tests")
      g <- r$wilks$global
      print(format(g, digits = 4), row.names = FALSE)
      cat(sprintf("Bartlett chi2 = %.4g (df = %g), p %s\n", r$wilks$bartlett_chi2, r$wilks$bartlett_df,
                  .fmt_p(r$wilks$bartlett_p)))
      print(format(r$wilks$multivariate, digits = 4), row.names = FALSE)
      line("Canonical axes")
      print(format(r$canonical$axes, digits = 4), row.names = FALSE)
      line("Variable contributions")
      print(format(r$variables, digits = 3), row.names = FALSE)
      line("Squared Mahalanobis distances between classes")
      print(round(r$distances$d2, 3))
      line("Confusion matrix (resubstitution)")
      print(r$confusion)
      cat(sprintf("Accuracy: %.2f%%\n", 100 * r$accuracy))
      if (!is.null(r$history)) {
        line("Selection path")
        print(format(r$history, digits = 4), row.names = FALSE)
      }
      invisible(self)
    },

    #' @description Base graphics.
    #' @param type `"scores"` (individuals on the first two discriminant axes),
    #'   `"importance"` (partial Wilks' lambda per variable) or `"axes"`
    #'   (eigenvalues).
    #' @param axes pair of axes for `type = "scores"`.
    #' @param ... passed to the underlying plotting function.
    plot = function(type = c("scores", "importance", "axes"), axes = c(1, 2), ...) {
      private$check()
      type <- match.arg(type)
      r <- private$res
      cols <- grDevices::hcl.colors(r$K, "Dark 3")
      if (type == "scores") {
        sc <- r$scores
        if (ncol(sc) < 2L) {
          graphics::stripchart(sc[, 1] ~ r$y, col = cols, pch = 16, method = "jitter",
                               xlab = "LD1", ylab = "Class", ...)
        } else {
          yl <- range(sc[, axes[2]]); yl[2] <- yl[2] + 0.18 * diff(yl)   # headroom for the legend
          graphics::plot(sc[, axes[1]], sc[, axes[2]], col = cols[as.integer(r$y)], pch = 19,
                         xlab = colnames(sc)[axes[1]], ylab = colnames(sc)[axes[2]],
                         ylim = yl, main = "Discriminant plane", ...)
          ce <- r$canonical$centroids
          graphics::points(ce[, axes[1]], ce[, axes[2]], pch = 8, cex = 2, lwd = 2, col = cols)
          graphics::legend("top", legend = r$classes, col = cols, pch = 19, bty = "n", horiz = TRUE)
        }
      } else if (type == "importance") {
        v <- r$variables
        v <- v[order(v$partial_lambda, decreasing = TRUE, na.last = FALSE), ]
        v$partial_lambda[is.na(v$partial_lambda)] <- 1
        # Left margin sized from the label widths but capped at 45% of the device
        # width, so the figure never fails with "figure margins too large".
        cex_n <- 0.85
        graphics::plot.new()
        wlab <- max(graphics::strwidth(v$variable, units = "inches", cex = cex_n)) + 0.25
        dev_w <- graphics::par("din")[1]
        left_in <- min(max(wlab, 0.8), 0.45 * dev_w)
        dev_h <- graphics::par("din")[2]
        op <- graphics::par(mai = c(min(0.7, 0.2 * dev_h), min(left_in, 0.45 * dev_w),
                                    min(0.5, 0.15 * dev_h), min(0.2, 0.05 * dev_w)))
        on.exit(graphics::par(op), add = TRUE)
        names_shown <- v$variable
        max_chr <- floor(left_in * 72 / (cex_n * 6))                     # ~6 pt per character
        long <- nchar(names_shown) > max_chr
        names_shown[long] <- paste0(substr(names_shown[long], 1, max(max_chr - 1, 3)), "~")
        graphics::barplot(v$partial_lambda, names.arg = names_shown, horiz = TRUE, las = 1,
                          cex.names = cex_n, xlim = c(0, 1),
                          col = ifelse(v$status == "contributing", "#2a7ab0", "grey70"),
                          xlab = "Partial Wilks' lambda (lower = more useful)",
                          main = "Variable contribution", ...)
      } else {
        graphics::barplot(r$canonical$eigenvalues, names.arg = colnames(r$canonical$A),
                          ylab = "Eigenvalue", main = "Discriminant axes", ...)
      }
      invisible(self)
    }
  ),

  active = list(
    #' @field coefficients matrix of classification function coefficients (first row: constant).
    coefficients = function() {
      private$check(); r <- private$res
      rbind(constant = r$const, r$coef)
    },
    #' @field classes class labels.
    classes = function() { private$check(); private$res$classes },
    #' @field priors prior probabilities used.
    priors = function() { private$check(); stats::setNames(private$res$priors, private$res$classes) },
    #' @field means class means of the retained columns (K x p).
    means = function() { private$check(); m <- private$res$means; dimnames(m) <- list(private$res$classes, rownames(private$res$coef)); m },
    #' @field pooled_cov pooled within-class covariance matrix.
    pooled_cov = function() { private$check(); private$res$pooled },
    #' @field selected names of the variables retained in the model.
    selected = function() { private$check(); private$res$selected },
    #' @field variables data.frame of per-variable diagnostics (univariate and partial Wilks' lambda, F, p, status).
    variables = function() { private$check(); private$res$variables },
    #' @field wilks list with the global Wilks' lambda test (`global`), the multivariate statistics and the Bartlett approximation.
    wilks = function() { private$check(); private$res$wilks },
    #' @field canonical list: discriminant axes coefficients, eigenvalues, structure correlations, class centroids.
    canonical = function() { private$check(); private$res$canonical },
    #' @field distances squared Mahalanobis distances between classes and their F-test p-values.
    distances = function() { private$check(); private$res$distances },
    #' @field selection_history data.frame describing the stepwise path (`NULL` without selection).
    selection_history = function() { private$check(); private$res$history },
    #' @field confusion_train confusion matrix on the training data.
    confusion_train = function() { private$check(); private$res$confusion },
    #' @field accuracy_train accuracy on the training data.
    accuracy_train = function() { private$check(); private$res$accuracy },
    #' @field scores_train coordinates of the training observations on the discriminant axes.
    scores_train = function() { private$check(); private$res$scores },
    #' @field y_train classes of the training observations actually used (after removal of missing values).
    y_train = function() { private$check(); private$res$y },
    #' @field features description (type, levels, range) of the explanatory variables seen at fit.
    features = function() { private$check(); private$res$features },
    #' @field params constructor parameters.
    params = function() private$cfg,
    #' @field is_fitted `TRUE` once `$fit()` has been called.
    is_fitted = function() !is.null(private$res)
  ),

  private = list(
    cfg = NULL,
    res = NULL,

    check = function() {
      if (is.null(private$res)) stop("The model is not fitted. Call $fit(X, y) first.", call. = FALSE)
    },

    as_frame = function(X) {
      if (is.matrix(X)) {
        cn <- colnames(X)
        X <- as.data.frame(X, stringsAsFactors = FALSE)
        if (is.null(cn)) names(X) <- paste0("V", seq_len(ncol(X)))
      }
      if (!is.data.frame(X)) stop("`X` must be a data.frame or a matrix.", call. = FALSE)
      X
    },

    make_priors = function(pri, nk, lev) {
      K <- length(lev)
      if (is.character(pri)) {
        return(if (pri == "equal") rep(1 / K, K) else as.numeric(nk / sum(nk)))
      }
      if (!is.null(names(pri))) {
        if (!setequal(names(pri), lev)) stop("Names of `priors` must match the classes.", call. = FALSE)
        pri <- pri[lev]
      }
      if (length(pri) != K || any(pri <= 0)) {
        stop("`priors` must be positive and of length equal to the number of classes.", call. = FALSE)
      }
      as.numeric(pri / sum(pri))
    },

    encode_new = function(X) {
      X <- private$as_frame(X)
      a <- .apply_encoder(private$res$encoder, X)
      if (any(a$unseen)) {
        warning(sum(a$unseen), " row(s) contain a level unseen at fit(): prediction set to NA.", call. = FALSE)
      }
      a$matrix
    },

    scores_for = function(X) {
      private$check()
      M <- private$encode_new(X)
      r <- private$res
      Z <- M %*% r$coef + rep(r$const, each = nrow(M))
      colnames(Z) <- r$classes
      Z
    },

    # Per-variable table: univariate test (variable alone), partial test given
    # the final model, and a status in {contributing, redundant, uninformative}.
    variable_table = function(Wss, Tss, blocks, sel, sc, n, K, pr, enc) {
      nms <- names(blocks)
      nb <- length(nms)
      uni <- data.frame(lambda = rep(NA_real_, nb), F = NA_real_, df1 = NA_real_, df2 = NA_real_,
                        p = NA_real_)
      for (q in unique(lengths(blocks))) {
        ix <- which(lengths(blocks) == q)
        lam <- vapply(ix, function(b) {
          cb <- blocks[[b]]
          lw <- .logdet(Wss[cb, cb, drop = FALSE]); lt <- .logdet(Tss[cb, cb, drop = FALSE])
          if (is.na(lw) || is.na(lt)) NA_real_ else exp(lw - lt)
        }, numeric(1))
        uni[ix, ] <- .rao_f_cols(lam, q, K - 1, n - K)
      }
      par <- data.frame(lambda = rep(NA_real_, nb), F = NA_real_, df1 = NA_real_, df2 = NA_real_,
                        p = NA_real_)
      isel <- nms %in% sel
      if (any(isel)) par[isel, ] <- .removal_stats(Wss, Tss, sc, blocks[nms[isel]], n, K)
      if (any(!isel)) par[!isel, ] <- .candidate_stats(Wss, Tss, sc, blocks[nms[!isel]], n, K)
      alpha <- pr$alpha_stay; alpha_e <- pr$alpha_enter
      status <- ifelse(isel & !is.na(par$p) & par$p <= alpha, "contributing",
                ifelse(!is.na(uni$p) & uni$p <= alpha_e, "redundant", "uninformative"))
      type <- vapply(nms, function(v) enc$columns[[v]]$type, character(1))
      data.frame(variable = nms, type = unname(type), n_columns = lengths(blocks),
                 selected = isel, univ_lambda = uni$lambda, univ_F = uni$F, univ_p = uni$p,
                 partial_lambda = par$lambda, partial_F = par$F, partial_p = par$p,
                 status = status, row.names = NULL, stringsAsFactors = FALSE)
    }
  )
)

# helpers ------------------------------------------------------------------------

# Vectorised Rao F returning exactly the five columns used in variable tables.
.rao_f_cols <- function(lambda, q, h, e) {
  r <- .rao_f(lambda, q, h, e)
  data.frame(lambda = lambda, F = r$F, df1 = r$df1, df2 = r$df2, p = r$p)
}

.fmt_p <- function(p) {
  if (is.na(p)) "NA" else if (p < 2.2e-16) "< 2.2e-16" else paste("=", format(signif(p, 3)))
}

# Restrict an encoder to a subset of encoded columns (those used by the model),
# so that predict() only needs the variables that are really in the model.
.subset_encoder <- function(enc, cols) {
  blocks <- list(); columns <- list(); nm <- character(0)
  for (v in names(enc$blocks)) {
    idx <- enc$blocks[[v]]
    inb <- idx %in% cols
    if (!any(inb)) next
    meta <- enc$columns[[v]]
    if (meta$type == "factor" && meta$encoding != "label") meta$used <- meta$used[inb]
    columns[[v]] <- meta
    blocks[[v]] <- match(idx[inb], cols)
  }
  list(columns = columns, names = enc$names[cols], blocks = blocks, encoding = enc$encoding)
}

# Stratified fold assignment with a local seed.
.make_folds <- function(y, k, seed = NULL, stratified = TRUE) {
  n <- length(y)
  if (k < 2L || k > n) stop("`k` must be between 2 and the number of observations.", call. = FALSE)
  if (!is.null(seed)) {
    old <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv())
    on.exit(if (!is.null(old)) assign(".Random.seed", old, envir = globalenv()), add = TRUE)
    set.seed(seed)
  }
  folds <- integer(n)
  if (stratified) {
    for (lv in levels(y)) {
      ix <- which(y == lv)
      folds[ix] <- sample(rep_len(seq_len(k), length(ix)))
    }
  } else {
    folds <- sample(rep_len(seq_len(k), n))
  }
  folds
}

#' @export
print.ldaR6_cv <- function(x, ...) {
  cat(sprintf("%d-fold cross-validation: accuracy = %.2f%% (error = %.2f%%)\n", x$k,
              100 * x$accuracy, 100 * x$error))
  print(x$confusion)
  invisible(x)
}
