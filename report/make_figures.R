# Figures for the LaTeX report. Run from the package root: Rscript report/make_figures.R
library(ldaR6)
dir.create("report/figures", showWarnings = FALSE)

# 1. discriminant plane (iris)
m <- LDA$new()$fit(iris[, 1:4], iris$Species)
pdf("report/figures/fig_plan.pdf", width = 5.2, height = 3.6)
par(mar = c(4, 4, 2, 1)); m$plot("scores"); dev.off()

# 2. variable contribution on credit_demo (stepwise 'both')
data(credit_demo)
m2 <- LDA$new(selection = "both")$fit(credit_demo[, -1], credit_demo$risk)
pdf("report/figures/fig_importance.pdf", width = 5.2, height = 3.6)
m2$plot("importance"); dev.off()

# 3. timings from the benchmark
b <- read.csv("benchmarks/benchmark_results.csv")
b <- b[!is.na(b$MASS_lda_sec), ]
lab <- sprintf("n=%s\np=%d", format(b$n, big.mark = " ", scientific = FALSE), b$p)
pdf("report/figures/fig_bench.pdf", width = 6.2, height = 3.4)
par(mar = c(5, 4.5, 1, 1))
bp <- barplot(rbind(b$MASS_lda_sec, b$ldaR6_sec), beside = TRUE, log = "y", names.arg = lab,
              col = c("grey70", "#2a7ab0"), ylab = "temps (s, échelle log)", cex.names = 0.7,
              ylim = c(0.05, 60))
legend("topleft", c("MASS::lda", "ldaR6::LDA"), fill = c("grey70", "#2a7ab0"), bty = "n", cex = 0.8)
dev.off()
cat("figures written\n")
