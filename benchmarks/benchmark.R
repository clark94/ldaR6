# Performance benchmark: ldaR6::LDA versus MASS::lda -----------------------------
# Run from the package root: Rscript benchmarks/benchmark.R
# Output: benchmarks/benchmark_results.csv (+ console table).
library(ldaR6)
library(MASS)

timeit <- function(expr, reps = 3) {
  expr <- substitute(expr); env <- parent.frame()
  t <- vapply(seq_len(reps), function(i) {
    gc(FALSE); unname(system.time(eval(expr, env))["elapsed"])
  }, numeric(1))
  median(t)
}

scenarios <- list(
  list(name = "long",   n = 1e5, p = 10,   K = 3, reps = 3),
  list(name = "long",   n = 1e6, p = 10,   K = 3, reps = 1),
  list(name = "long",   n = 1e6, p = 50,   K = 5, reps = 1),
  list(name = "medium", n = 5e3, p = 50,   K = 4, reps = 3),
  list(name = "medium", n = 2e4, p = 200,  K = 4, reps = 3),
  list(name = "wide",   n = 200, p = 500,  K = 3, reps = 3),
  list(name = "wide",   n = 200, p = 2000, K = 3, reps = 1)
)

dir.create("benchmarks", showWarnings = FALSE)
writeLines(c(capture.output(sessionInfo()), capture.output(Sys.info()), capture.output(extSoftVersion())), "benchmarks/session_info.txt")
if (file.exists("/proc/meminfo")) file.copy("/proc/meminfo", "benchmarks/memory_info.txt", overwrite = TRUE)
if (file.exists("/proc/cpuinfo")) writeLines(readLines("/proc/cpuinfo", n = 30), "benchmarks/cpu_info.txt")
rows <- list()
for (s in scenarios) {
  d <- simulate_lda_data(s$n, s$p, s$K, seed = 42)
  X <- d[, -1]; y <- d$y
  cat(sprintf("== %s: n=%g p=%d K=%d\n", s$name, s$n, s$p, s$K))

  # our implementation (default; pseudo-inverse + warning if singular)
  t_ours <- timeit(suppressWarnings(m <- LDA$new()$fit(X, y)), s$reps)
  acc_ours <- m$accuracy_train
  Xscore <- X[seq_len(min(5000, s$n)), , drop = FALSE]
  pred_ours <- timeit(m$predict(Xscore), s$reps)
  pred_mass <- NA_real_
  # MASS::lda (fails when W is singular)
  mass_ok <- s$n - s$K > s$p
  t_mass <- NA; same <- NA
  if (mass_ok) {
    t_mass <- timeit(l <- MASS::lda(X, y), s$reps)
    pred_mass <- timeit(predict(l, Xscore), s$reps)
    pm <- predict(l, Xscore)$class
    same <- mean(as.character(pm) == as.character(m$predict(X[1:min(5000, s$n), ])))
  }
  # high dimensional options
  t_shr <- NA; t_fwd <- NA
  if (!mass_ok) {
    t_shr <- timeit(suppressWarnings(LDA$new(shrinkage = 0.2)$fit(X, y)), s$reps)
    t_fwd <- timeit(LDA$new(selection = "forward", alpha_enter = 0.01)$fit(X, y), 1)
  }
  rows[[length(rows) + 1]] <- data.frame(
    scenario = s$name, n = s$n, p = s$p, K = s$K, ldaR6_sec = t_ours, MASS_lda_sec = t_mass,
    speedup = t_mass / t_ours, same_predictions = same, train_acc = acc_ours,
    score_n = nrow(Xscore), predict_ldaR6_sec = pred_ours, predict_MASS_sec = pred_mass,
    model_size_bytes = as.numeric(object.size(m)),
    ldaR6_shrinkage_sec = t_shr, ldaR6_forward_sec = t_fwd)
  print(tail(rows, 1)[[1]])
}
res <- do.call(rbind, rows)
write.csv(res, "benchmarks/benchmark_results.csv", row.names = FALSE)

# cost of the other tools on a medium problem
d <- simulate_lda_data(2e4, 50, 4, seed = 1)
cat("\nCross-validation 10-fold n=2e4 p=50:",
    timeit(LDA$new()$cross_validate(d[, -1], d$y, k = 10, seed = 1), 1), "s\n")
cat("Stepwise (both) n=2e4 p=50:", timeit(LDA$new(selection = "both")$fit(d[, -1], d$y), 1), "s\n")
cat("sessionInfo:", R.version.string, "\n")
