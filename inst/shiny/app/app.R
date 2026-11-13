# Shiny application for the ldaR6 package ---------------------------------------
# Launch with ldaR6::run_app(). Workflow (tabs, left to right):
#   1 Data -> 2 Model setup -> 3 Results -> 4 Predict one individual / a file.

library(shiny)
library(bslib)
library(DT)
library(ldaR6)

options(shiny.maxRequestSize = 200 * 1024^2)   # 200 MB uploads

# ---- small helpers -----------------------------------------------------------
fmt_p <- function(p) ifelse(is.na(p), NA, ifelse(p < 1e-4, "<1e-4", formatC(p, digits = 3, format = "g")))

round_df <- function(d, digits = 4) {
  d[] <- lapply(d, function(x) if (is.numeric(x)) signif(x, digits) else x)
  d
}

status_colors <- c(contributing = "#2a9d5c", redundant = "#e9a23b", uninformative = "#9aa5b1")

ui <- page_navbar(
  title = "ldaR6 - Linear Discriminant Analysis",
  theme = bs_theme(bootswatch = "flatly"),
  id = "nav",

  # ---- 1. DATA ------------------------------------------------------------------
  nav_panel("1. Data",
    layout_sidebar(
      sidebar = sidebar(width = 320,
        h5("Load a data file"),
        fileInput("file", "CSV / TSV / Excel file", accept = c(".csv", ".tsv", ".txt", ".xlsx", ".xls")),
        selectInput("sep", "Field separator", c("Tab" = "\t", "Semicolon" = ";", "Comma" = ",", "Space" = " "),
                    selected = "\t"),
        selectInput("dec", "Decimal mark", c("Point" = ".", "Comma" = ",")),
        checkboxInput("header", "First row = column names", TRUE),
        hr(),
        h5("...or use an example"),
        actionButton("ex_credit", "credit_demo (mixed variables)", class = "btn-outline-primary w-100 mb-2"),
        actionButton("ex_iris", "iris", class = "btn-outline-primary w-100"),
        helpText("Text columns are treated as categorical variables.")
      ),
      uiOutput("data_info"),
      DTOutput("data_table")
    )
  ),

  # ---- 2. SETUP -------------------------------------------------------------------
  nav_panel("2. Model setup",
    layout_columns(col_widths = c(6, 6),
      card(card_header("Variables"),
        selectInput("target", "Target variable (categorical)", choices = NULL),
        selectizeInput("predictors", "Explanatory (active) variables", choices = NULL, multiple = TRUE),
        div(actionButton("all_pred", "Select all", class = "btn-sm btn-outline-secondary"),
            actionButton("no_pred", "Clear", class = "btn-sm btn-outline-secondary")),
        br(),
        selectizeInput("illustrative", "Illustrative (supplementary) variables", choices = NULL, multiple = TRUE),
        helpText("Illustrative variables do not take part in the model; they are described on the discriminant axes.")
      ),
      card(card_header("Parameters"),
        selectInput("priors", "Prior probabilities", c("Proportional to class sizes" = "proportional",
                                                       "Equal" = "equal")),
        selectInput("encoding", "Categorical encoding",
                    c("Dummy: k-1 indicators (recommended)" = "dummy",
                      "One-hot: k indicators (singular covariance)" = "onehot",
                      "Label: integer codes (use for ordered factors)" = "label")),
        selectInput("selection", "Variable selection (Wilks' lambda)",
                    c("None" = "none", "Forward" = "forward", "Backward" = "backward",
                      "Stepwise (both)" = "both")),
        conditionalPanel("input.selection != 'none'",
          layout_columns(
            numericInput("alpha_enter", "p to enter", 0.05, min = 0.0001, max = 1, step = 0.01),
            numericInput("alpha_stay", "p to stay", 0.05, min = 0.0001, max = 1, step = 0.01))),
        sliderInput("shrinkage", "Covariance shrinkage (0 = none; use for wide data)", 0, 1, 0, step = 0.01),
        sliderInput("train_pct", "Training sample (%) - the rest is a test set", 50, 100, 100, step = 5),
        numericInput("seed", "Random seed", 1, min = 1),
        numericInput("cv_k", "Cross-validation folds (0 = skip)", 5, min = 0, max = 20)
      )
    ),
    actionButton("run", "Run the analysis", class = "btn-primary btn-lg", icon = icon("play"))
  ),

  # ---- 3. RESULTS -----------------------------------------------------------------
  nav_panel("3. Results",
    navset_card_tab(
      nav_panel("Overview", uiOutput("overview")),
      nav_panel("Classification functions",
        helpText("Score of class k = constant + sum(coefficient x value). The predicted class has the largest score."),
        DTOutput("coef_table"),
        downloadButton("dl_coef", "Download coefficients (CSV)")),
      nav_panel("Tests",
        h5("Global test of the model"), tableOutput("wilks_table"),
        h5("Other multivariate statistics"), tableOutput("multi_table"),
        h5("Discriminant axes"), tableOutput("axes_table"),
        h5("Squared Mahalanobis distances between classes"), tableOutput("dist_table")),
      nav_panel("Variables",
        helpText("Status: contributing = brings information given the others; redundant = informative alone but adds nothing; uninformative = no link with the target."),
        div(style = "margin-bottom: 30px;", plotOutput("importance_plot", height = "520px")),
        div(style = "margin-top: 30px;", DTOutput("var_table")),
        h5("Selection path"), tableOutput("hist_table"),
        hr(),
        actionButton("audit", "Detect perturbing variables (cross-validated, slower)"),
        tableOutput("audit_table")),
      nav_panel("Evaluation", uiOutput("eval_ui")),
      nav_panel("Graphics",
        layout_columns(col_widths = c(3, 9),
          div(numericInput("ax1", "Horizontal axis", 1, min = 1, max = 20),
              numericInput("ax2", "Vertical axis", 2, min = 1, max = 20),
              checkboxInput("show_test", "Show test individuals", TRUE)),
          div(style = "min-height: 500px;", plotOutput("score_plot", height = "480px"))),
        h5("Illustrative variables"), uiOutput("illustrative_ui")),
      nav_panel("Print / summary", verbatimTextOutput("summary_txt"))
    )
  ),

  # ---- 4. PREDICTION --------------------------------------------------------------
  nav_panel("4. Predict",
    navset_card_tab(
      nav_panel("One individual",
        helpText("Only the variables retained by the model are requested."),
        uiOutput("indiv_inputs"),
        actionButton("predict_one", "Predict", class = "btn-primary"),
        br(), br(), uiOutput("indiv_result"), div(style = "min-height: 340px; margin-top: 15px;", plotOutput("indiv_plot", height = "320px"))),
      nav_panel("From a file",
        helpText("The file must contain the descriptors (explanatory variables) only, no target is required."),
        layout_columns(col_widths = c(4, 8),
          div(fileInput("newfile", "File with new individuals", accept = c(".csv", ".tsv", ".txt", ".xlsx", ".xls")),
              selectInput("new_sep", "Separator", c("Tab" = "\t", "Semicolon" = ";", "Comma" = ",", "Space" = " ")),
              selectInput("new_dec", "Decimal mark", c("Point" = ".", "Comma" = ",")),
              checkboxInput("with_proba", "Add posterior probabilities", TRUE),
              actionButton("example_new", "Use example individuals", class = "btn-outline-primary btn-sm"),
              br(), br(), downloadButton("dl_pred", "Download predictions (TSV)")),
          div(uiOutput("file_pred_info"), DTOutput("file_pred_table"))))
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(data = NULL, model = NULL, train = NULL, test = NULL, target = NULL,
                       cv = NULL, new_data = NULL, audit = NULL, preds = NULL, illu = NULL)

  # ---- data loading ---------------------------------------------------------------
  load_file <- function(path, name, sep, dec, header) {
    tryCatch(read_data(path, sep = sep, dec = dec, header = header),
             error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
  }
  set_data <- function(d) {
    rv$data <- d; rv$model <- NULL
    cats <- names(d)[vapply(d, function(x) !is.numeric(x) || length(unique(x)) <= 10, logical(1))]
    updateSelectInput(session, "target", choices = names(d), selected = if (length(cats)) cats[1] else names(d)[1])
  }
  observeEvent(input$file, {
    d <- load_file(input$file$datapath, input$file$name, input$sep, input$dec, input$header)
    if (!is.null(d)) set_data(d)
  })
  observeEvent(list(input$sep, input$dec, input$header), {
    req(input$file)
    d <- load_file(input$file$datapath, input$file$name, input$sep, input$dec, input$header)
    if (!is.null(d)) set_data(d)
  }, ignoreInit = TRUE)
  observeEvent(input$ex_credit, { set_data(read_data(system.file("extdata", "credit_demo.tsv", package = "ldaR6"))) })
  observeEvent(input$ex_iris, { set_data(read_data(system.file("extdata", "iris.tsv", package = "ldaR6"))) })

  output$data_info <- renderUI({
    if (is.null(rv$data)) return(div(class = "alert alert-info", "Load a file or choose an example to start."))
    d <- rv$data
    div(class = "alert alert-success", sprintf("%d rows x %d columns (%d numeric, %d text/categorical, %d with missing values).",
        nrow(d), ncol(d), sum(vapply(d, is.numeric, TRUE)), sum(!vapply(d, is.numeric, TRUE)),
        sum(vapply(d, anyNA, TRUE))))
  })
  output$data_table <- renderDT({
    req(rv$data); datatable(head(rv$data, 500), options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })

  # variable pickers follow the chosen target
  observeEvent(list(input$target, rv$data), {
    req(rv$data, input$target %in% names(rv$data))
    others <- setdiff(names(rv$data), input$target)
    updateSelectizeInput(session, "predictors", choices = others, selected = others, server = FALSE)
    updateSelectizeInput(session, "illustrative", choices = others, selected = NULL, server = FALSE)
  })
  observeEvent(input$all_pred, updateSelectizeInput(session, "predictors", selected = setdiff(names(rv$data), input$target)))
  observeEvent(input$no_pred, updateSelectizeInput(session, "predictors", selected = character(0)))

  # ---- fitting ----------------------------------------------------------------------
  observeEvent(input$run, {
    req(rv$data, input$target)
    preds <- setdiff(input$predictors, c(input$target, input$illustrative))
    if (!length(preds)) { showNotification("Select at least one explanatory variable.", type = "error"); return() }
    d <- rv$data
    y <- factor(d[[input$target]])
    if (nlevels(y) < 2 || nlevels(y) > 50) {
      showNotification("The target must be categorical with 2 to 50 classes.", type = "error"); return()
    }
    withProgress(message = "Fitting the model...", value = 0.3, {
      set.seed(input$seed)
      n <- nrow(d)
      idx_train <- if (input$train_pct >= 100) seq_len(n) else sort(sample(n, round(n * input$train_pct / 100)))
      idx_test <- setdiff(seq_len(n), idx_train)
      warn <- NULL
      res <- tryCatch(withCallingHandlers({
        m <- LDA$new(priors = input$priors, encoding = input$encoding, selection = input$selection,
                     alpha_enter = input$alpha_enter %||% 0.05, alpha_stay = input$alpha_stay %||% 0.05,
                     shrinkage = input$shrinkage)
        m$fit(d[idx_train, preds, drop = FALSE], y[idx_train])
        m
      }, warning = function(w) { warn <<- c(warn, conditionMessage(w)); invokeRestart("muffleWarning") }),
      error = function(e) { showNotification(conditionMessage(e), type = "error", duration = 10); NULL })
      if (is.null(res)) return()
      if (length(warn)) showNotification(paste(unique(warn), collapse = " | "), type = "warning", duration = 12)
      setProgress(0.6, message = "Evaluating...")
      rv$one <- NULL; rv$model <- res; rv$train <- d[idx_train, , drop = FALSE]; rv$test <- d[idx_test, , drop = FALSE]
      rv$target <- input$target; rv$audit <- NULL; rv$preds <- preds
      rv$cv <- if (input$cv_k >= 2) tryCatch(
        res$cross_validate(rv$train[, preds, drop = FALSE], y[idx_train], k = input$cv_k, seed = input$seed),
        error = function(e) NULL) else NULL
      rv$illu <- if (length(input$illustrative)) tryCatch(
        res$project_illustrative(d[idx_train, input$illustrative, drop = FALSE]), error = function(e) NULL) else NULL
      nav_select("nav", "3. Results")
    })
  })

  req_model <- function() { validate(need(!is.null(rv$model), "Run the analysis first (tab 2).")); rv$model }

  # ---- results -----------------------------------------------------------------------
  output$overview <- renderUI({
    m <- req_model(); g <- m$wilks$global
    n_test <- nrow(rv$test)
    test_acc <- if (n_test > 0) {
      ytest <- factor(rv$test[[rv$target]], levels = m$classes)
      m$score(rv$test[, rv$preds, drop = FALSE], ytest)
    } else NA
    stat <- function(label, value) div(class = "p-2 text-center", div(class = "text-muted small", label),
                                       div(class = "fs-4 fw-bold", value))
    tagList(
      layout_columns(
        stat("Training accuracy", sprintf("%.1f%%", 100 * m$accuracy_train)),
        stat("Test accuracy", if (is.na(test_acc)) "-" else sprintf("%.1f%%", 100 * test_acc)),
        stat("CV accuracy", if (is.null(rv$cv)) "-" else sprintf("%.1f%%", 100 * rv$cv$accuracy)),
        stat("Wilks' lambda", signif(g$value, 3)),
        stat("Variables kept", sprintf("%d / %d", length(m$selected), nrow(m$variables)))),
      hr(),
      tags$pre(paste(capture.output(print(m)), collapse = "\n")),
      if (nrow(m$variables[!m$variables$selected, , drop = FALSE]))
        p(strong("Left out: "), paste(m$variables$variable[!m$variables$selected], collapse = ", "))
    )
  })

  output$coef_table <- renderDT({
    m <- req_model(); datatable(round(m$coefficients, 5), options = list(scrollX = TRUE, pageLength = 25))
  })
  output$dl_coef <- downloadHandler("lda_coefficients.csv", function(file) {
    write.csv(cbind(term = rownames(req_model()$coefficients), req_model()$coefficients), file, row.names = FALSE)
  })
  output$wilks_table <- renderTable({
    g <- req_model()$wilks$global; g$p_value <- fmt_p(g$p_value); round_df(g)
  }, digits = 4)
  output$multi_table <- renderTable(round_df(req_model()$wilks$multivariate), digits = 4)
  output$axes_table <- renderTable({
    a <- req_model()$canonical$axes; if ("p_value" %in% names(a)) a$p_value <- fmt_p(a$p_value); round_df(a)
  }, digits = 4)
  output$dist_table <- renderTable(round(req_model()$distances$d2, 3), rownames = TRUE)

  # fixed-height container (see the UI): the plot adapts its own margins to the device
  output$importance_plot <- renderPlot({ req_model()$plot("importance") })
  output$var_table <- renderDT({
    v <- req_model()$variables
    v <- v[, c("variable", "type", "n_columns", "selected", "univ_lambda", "univ_p", "partial_lambda",
               "partial_F", "partial_p", "status")]
    datatable(round_df(v, 3), rownames = FALSE, options = list(pageLength = 25, scrollX = TRUE)) |>
      formatStyle("status", color = styleEqual(names(status_colors), unname(status_colors)), fontWeight = "bold")
  })
  output$hist_table <- renderTable({
    h <- req_model()$selection_history
    if (is.null(h)) data.frame(note = "No selection was requested.") else round_df(h)
  }, digits = 4)
  observeEvent(input$audit, {
    m <- req_model()
    withProgress(message = "Cross-validating each variable...", {
      rv$audit <- tryCatch(m$variable_audit(rv$train[, rv$preds, drop = FALSE],
                                            factor(rv$train[[rv$target]]), k = 5, seed = input$seed),
                           error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
    })
  })
  output$audit_table <- renderTable({ req(rv$audit); round_df(rv$audit) }, digits = 4)

  output$eval_ui <- renderUI({
    m <- req_model()
    tagList(
      h5("Confusion matrix - training data"), tableOutput("conf_train"),
      if (nrow(rv$test) > 0) tagList(h5("Confusion matrix - test data"), tableOutput("conf_test")),
      if (!is.null(rv$cv)) tagList(h5(sprintf("%d-fold cross-validation (accuracy %.1f%%)", rv$cv$k, 100 * rv$cv$accuracy)),
                                   tableOutput("conf_cv")))
  })
  output$conf_train <- renderTable(as.data.frame.matrix(unclass(req_model()$confusion_train)), rownames = TRUE)
  output$conf_test <- renderTable({
    m <- req_model(); req(nrow(rv$test) > 0)
    as.data.frame.matrix(unclass(m$confusion(rv$test[, rv$preds, drop = FALSE], rv$test[[rv$target]])))
  }, rownames = TRUE)
  output$conf_cv <- renderTable({ req(rv$cv); as.data.frame.matrix(unclass(rv$cv$confusion)) }, rownames = TRUE)

  output$score_plot <- renderPlot({
    m <- req_model()
    ax <- c(input$ax1, input$ax2); k <- ncol(m$scores_train)
    validate(need(k >= 2, "Only one discriminant axis (2-class problem)."),
             need(all(ax %in% seq_len(k)), sprintf("Axes must be between 1 and %d.", k)))
    cols <- grDevices::hcl.colors(length(m$classes), "Dark 3")
    sc <- m$scores_train; yy <- m$y_train
    yl <- range(sc[, ax[2]]); yl[2] <- yl[2] + 0.18 * diff(yl)   # headroom for the legend
    plot(sc[, ax[1]], sc[, ax[2]], col = adjustcolor(cols[as.integer(yy)], 0.7), pch = 19, ylim = yl,
         xlab = colnames(sc)[ax[1]], ylab = colnames(sc)[ax[2]], main = "Individuals on the discriminant plane")
    if (isTRUE(input$show_test) && nrow(rv$test) > 0) {
      tt <- rv$test[complete.cases(rv$test[, rv$preds, drop = FALSE]), , drop = FALSE]
      zt <- m$transform(tt[, rv$preds, drop = FALSE]); yt <- factor(tt[[rv$target]], levels = m$classes)
      points(zt[, ax[1]], zt[, ax[2]], col = cols[as.integer(yt)], pch = 1, lwd = 2)
    }
    ce <- m$canonical$centroids
    points(ce[, ax[1]], ce[, ax[2]], pch = 8, cex = 2, lwd = 2, col = cols)
    legend("top", legend = m$classes, col = cols, pch = 19, bty = "n", horiz = TRUE)
  })
  output$illustrative_ui <- renderUI({
    if (is.null(rv$illu)) return(helpText("Choose illustrative variables in tab 2 to see them here."))
    tagList(
      if (!is.null(rv$illu$numeric)) tagList(h6("Correlation of numeric variables with the axes"),
                                              tableOutput("illu_num")),
      if (!is.null(rv$illu$categorical)) tagList(h6("Barycentres of categorical modalities"),
                                                  tableOutput("illu_cat")))
  })
  output$illu_num <- renderTable(round(rv$illu$numeric, 3), rownames = TRUE)
  output$illu_cat <- renderTable(round_df(rv$illu$categorical, 3))
  output$summary_txt <- renderPrint({ m <- req_model(); m$summary() })

  # ---- individual prediction -----------------------------------------------------------
  output$indiv_inputs <- renderUI({
    m <- req_model()
    feats <- m$features[m$selected]
    inputs <- lapply(names(feats), function(v) {
      f <- feats[[v]]; id <- paste0("ind_", v)
      if (f$type == "numeric") numericInput(id, v, value = signif(f$mean, 4))
      else selectInput(id, v, choices = f$levels)
    })
    do.call(layout_columns, c(list(col_widths = rep(3, length(inputs))), inputs))
  })
  observeEvent(input$predict_one, {
    m <- req_model()
    row <- lapply(m$selected, function(v) {
      val <- input[[paste0("ind_", v)]]
      f <- m$features[[v]]
      if (f$type == "factor") factor(val, levels = f$levels) else as.numeric(val)
    })
    names(row) <- m$selected
    nd <- as.data.frame(row, check.names = FALSE, stringsAsFactors = FALSE)
    rv$one <- list(class = as.character(m$predict(nd)), prob = m$predict_proba(nd)[1, ])
  })
  output$indiv_result <- renderUI({
    req(rv$one)
    div(class = "alert alert-primary fs-5", "Predicted class: ", strong(rv$one$class))
  })
  output$indiv_plot <- renderPlot({
    req(rv$one)
    p <- rv$one$prob
    plot.new()                                   # margins in inches, capped by the device size
    din <- par("din")
    op <- par(mai = pmin(c(0.5, 0.7, 0.5, 0.2), c(0.2, 0.2, 0.2, 0.05) * c(din[2], din[1], din[2], din[1])))
    on.exit(par(op), add = TRUE)
    barplot(p, ylim = c(0, 1), col = ifelse(names(p) == rv$one$class, "#2a7ab0", "grey75"),
            ylab = "Posterior probability", main = "Class membership probabilities")
  })

  # ---- file prediction -------------------------------------------------------------------
  observeEvent(input$newfile, {
    ext <- tools::file_ext(input$newfile$name)
    local_path <- tempfile(fileext = paste0(".", ext))
    file.copy(input$newfile$datapath, local_path, overwrite = TRUE)
    on.exit(unlink(local_path), add = TRUE)
    rv$new_data <- tryCatch(read_data(local_path, sep = input$new_sep, dec = input$new_dec),
                            error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
  })
  observeEvent(input$example_new, {
    rv$new_data <- read_data(system.file("extdata", "credit_new.tsv", package = "ldaR6"))
  })
  predicted <- reactive({
    m <- req_model(); nd <- rv$new_data
    validate(need(!is.null(nd), "Load a file with the new individuals."))
    miss <- setdiff(m$selected, names(nd))
    validate(need(!length(miss), paste("Missing column(s) in the file:", paste(miss, collapse = ", "))))
    out <- nd
    pr <- withCallingHandlers(m$predict(nd), warning = function(w) {
      showNotification(conditionMessage(w), type = "warning"); invokeRestart("muffleWarning") })
    out$predicted_class <- as.character(pr)
    if (isTRUE(input$with_proba)) {
      pp <- suppressWarnings(m$predict_proba(nd)); colnames(pp) <- paste0("prob_", colnames(pp))
      out <- cbind(out, round(pp, 5))
    }
    out
  })
  output$file_pred_info <- renderUI({
    p <- predicted()
    tagList(div(class = "alert alert-success", sprintf("%d individuals scored.", nrow(p))),
            tableOutput("class_counts"))
  })
  output$class_counts <- renderTable({ as.data.frame(table(class = predicted()$predicted_class, useNA = "ifany")) })
  output$file_pred_table <- renderDT(datatable(predicted(), options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE))
  output$dl_pred <- downloadHandler("lda_predictions.tsv", function(file) {
    write.table(predicted(), file, sep = "\t", row.names = FALSE, quote = FALSE)
  })
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

shinyApp(ui, server)
