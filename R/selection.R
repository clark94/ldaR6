# Stepwise variable selection based on Wilks' lambda -----------------------------
#
# Equivalent to SAS PROC STEPDISC. Selection works on source variables
# ("blocks"): a categorical variable encoded with several indicator columns
# enters or leaves the model as a whole, tested with the multivariate partial
# Wilks' lambda of the block. All computations use the SSCP matrices W and T
# computed once, so the cost of a step does not depend on n.

#' Run the stepwise selection
#'
#' @param W,T within-class and total SSCP matrices over all encoded columns.
#' @param blocks named list: source variable -> encoded column indices.
#' @param n,K sample size, number of classes.
#' @param direction "forward", "backward" or "both".
#' @param alpha_enter,alpha_stay p-value thresholds (entry and stay).
#' @param max_vars maximum number of source variables (forward / both).
#' @return list(selected = names of retained blocks (entry order), history = data.frame).
#' @noRd
.select_variables <- function(W, T, blocks, n, K, direction = c("forward", "backward", "both"),
                              alpha_enter = 0.05, alpha_stay = 0.05, max_vars = NULL) {
  direction <- match.arg(direction)
  nb <- length(blocks)
  if (is.null(max_vars)) max_vars <- nb
  in_model <- if (direction == "backward") names(blocks) else character(0)
  hist <- list()
  step <- 0L

  cols_of <- function(nms) if (length(nms)) sort(unlist(blocks[nms], use.names = FALSE)) else integer(0)
  wilks_of <- function(nms) {
    S <- cols_of(nms)
    if (!length(S)) return(1)
    exp(.logdet(W[S, S, drop = FALSE]) - .logdet(T[S, S, drop = FALSE]))
  }
  log_step <- function(action, v, st) {
    step <<- step + 1L
    hist[[step]] <<- data.frame(step = step, action = action, variable = v,
                                partial_lambda = st$lambda, F = st$F, p_value = st$p,
                                n_vars = length(in_model), wilks = wilks_of(in_model),
                                stringsAsFactors = FALSE)
  }

  guard <- 0L
  repeat {
    guard <- guard + 1L
    if (guard > 4L * nb + 5L) break
    changed <- FALSE

    # removal step (backward and both)
    if (direction %in% c("backward", "both") && length(in_model) > 0L) {
      S <- cols_of(in_model)
      st <- .removal_stats(W, T, S, blocks[in_model], n, K)
      pv <- st$p
      pv[is.na(pv)] <- 1  # numerically useless variable: candidate for removal
      worst <- which.max(pv)
      if (pv[worst] > alpha_stay) {
        v <- in_model[worst]
        in_model <- setdiff(in_model, v)
        log_step("remove", v, st[worst, ])
        changed <- TRUE
        if (direction == "backward") next
      }
    }
    # entry step (forward and both)
    if (direction %in% c("forward", "both") && length(in_model) < max_vars) {
      out <- setdiff(names(blocks), in_model)
      if (length(out)) {
        S <- cols_of(in_model)
        st <- .candidate_stats(W, T, S, blocks[out], n, K)
        ok <- which(!is.na(st$p))
        if (length(ok)) {
          best <- ok[order(st$p[ok], st$lambda[ok])[1L]]
          if (st$p[best] <= alpha_enter) {
            v <- out[best]
            in_model <- c(in_model, v)
            log_step("enter", v, st[best, ])
            changed <- TRUE
          }
        }
      }
    }
    if (!changed) break
  }
  history <- if (length(hist)) do.call(rbind, hist) else
    data.frame(step = integer(0), action = character(0), variable = character(0),
               partial_lambda = numeric(0), F = numeric(0), p_value = numeric(0),
               n_vars = integer(0), wilks = numeric(0))
  list(selected = in_model, history = history)
}
