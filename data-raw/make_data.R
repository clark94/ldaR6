# Builds the example data sets shipped with the package.
# Run from the package root:  Rscript data-raw/make_data.R
set.seed(2026)
n <- 900
risk <- factor(sample(c("low", "medium", "high"), n, replace = TRUE, prob = c(.4, .35, .25)),
               levels = c("low", "medium", "high"))
r <- as.integer(risk)                       # 1 = low, 2 = medium, 3 = high
age         <- round(pmax(18, rnorm(n, c(46, 40, 34)[r], 11)))
income      <- round(pmax(800, rnorm(n, c(3800, 2900, 2100)[r], 800)))
debt_ratio  <- round(pmin(1, pmax(0, rnorm(n, c(.20, .33, .48)[r], .12))), 3)
savings     <- round(pmax(0, rnorm(n, c(9000, 4500, 1500)[r], 3500)))
n_late      <- rpois(n, c(0.3, 1.0, 2.4)[r])
income_copy <- round(income * 1.02 + rnorm(n, 0, 60))
noise1      <- round(rnorm(n), 3)
noise2      <- round(rnorm(n, 10, 3), 3)
housing   <- factor(sapply(r, function(k) sample(c("own", "rent", "free"), 1,
                   prob = list(c(.6, .3, .1), c(.4, .45, .15), c(.2, .6, .2))[[k]])),
                   levels = c("own", "rent", "free"))
job       <- factor(sample(c("employee", "manager", "worker", "self-employed"), n, replace = TRUE))
education <- factor(sapply(r, function(k) sample(c("primary", "secondary", "higher"), 1,
                    prob = list(c(.15, .45, .40), c(.25, .5, .25), c(.40, .45, .15))[[k]])),
                    levels = c("primary", "secondary", "higher"), ordered = TRUE)
credit_demo <- data.frame(risk, age, income, debt_ratio, savings, n_late, income_copy,
                          noise1, noise2, housing, job, education)

dir.create("data", showWarnings = FALSE)
save(credit_demo, file = "data/credit_demo.rda", compress = "xz")
dir.create("inst/extdata", recursive = TRUE, showWarnings = FALSE)
write.table(credit_demo, "inst/extdata/credit_demo.tsv", sep = "\t", quote = FALSE,
            row.names = FALSE, fileEncoding = "UTF-8")

# individuals to score in the tutorial / Shiny app: descriptors only, no target
idx <- sample(n, 40)
write.table(credit_demo[idx, -1], "inst/extdata/credit_new.tsv", sep = "\t", quote = FALSE,
            row.names = FALSE, fileEncoding = "UTF-8")

# classic Fisher iris, tab separated
write.table(iris, "inst/extdata/iris.tsv", sep = "\t", quote = FALSE, row.names = FALSE,
            fileEncoding = "UTF-8")
