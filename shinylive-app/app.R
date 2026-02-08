# =============================================================================
# Statistical Analysis Web App — Shinylive (WebR)
# Phase 1: Complete 6-step wizard for in-browser statistical analysis
#
# All computation runs client-side via WebR/WASM. No data leaves the browser.
# =============================================================================

# --- Package installation (WebR-aware) ---------------------------------------

is_webr <- function() {
  Sys.getenv("WEBR") != "" ||
    isTRUE(grepl("wasm", R.version$platform)) ||
    exists("webr", mode = "environment")
}

install_if_needed <- function(pkg_name) {
  if (!requireNamespace(pkg_name, quietly = TRUE)) {
    tryCatch({
      if (is_webr() || exists("webr")) {
        webr::install(pkg_name, quiet = TRUE)
      } else {
        install.packages(pkg_name, repos = "https://cloud.r-project.org", quiet = TRUE)
      }
    }, error = function(e) {
      tryCatch(install.packages(pkg_name, quiet = TRUE), error = function(e2) NULL)
    })
  }
}

# Core packages — install before loading
core_pkgs <- c("gt", "gtsummary", "ggplot2", "broom", "labelled",
               "survival", "sandwich", "lmtest", "car", "emmeans",
               "haven", "readxl", "writexl", "lme4", "ggdag")

for (pkg in core_pkgs) install_if_needed(pkg)

library(shiny)
library(ggplot2)
library(broom)

# --- Helper functions ---------------------------------------------------------

#' Detect variable types in a data frame
classify_variables <- function(df) {
  types <- sapply(names(df), function(col) {
    x <- df[[col]]
    if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) return("date")
    if (is.numeric(x) || is.integer(x)) {
      n_unique <- length(unique(na.omit(x)))
      if (n_unique <= 5) return("categorical")
      return("numeric")
    }
    if (is.factor(x) || is.character(x) || is.logical(x)) return("categorical")
    "other"
  })
  data.frame(variable = names(df), type = unname(types), stringsAsFactors = FALSE)
}

#' Safe file reader — detects format from extension
safe_read_data <- function(file_path, file_name) {
  ext <- tolower(tools::file_ext(file_name))
  tryCatch({
    df <- switch(ext,
      "csv" = read.csv(file_path, stringsAsFactors = FALSE),
      "tsv" = read.delim(file_path, stringsAsFactors = FALSE),
      "xlsx" = {
        if (requireNamespace("readxl", quietly = TRUE))
          as.data.frame(readxl::read_xlsx(file_path))
        else stop("readxl not available")
      },
      "xls" = {
        if (requireNamespace("readxl", quietly = TRUE))
          as.data.frame(readxl::read_xls(file_path))
        else stop("readxl not available")
      },
      "dta" = {
        if (requireNamespace("haven", quietly = TRUE))
          as.data.frame(haven::read_dta(file_path))
        else stop("haven not available")
      },
      "sav" = {
        if (requireNamespace("haven", quietly = TRUE))
          as.data.frame(haven::read_sav(file_path))
        else stop("haven not available")
      },
      "sas7bdat" = {
        if (requireNamespace("haven", quietly = TRUE))
          as.data.frame(haven::read_sas(file_path))
        else stop("haven not available")
      },
      stop(paste("Unsupported file format:", ext))
    )
    list(data = df, error = NULL)
  }, error = function(e) {
    list(data = NULL, error = conditionMessage(e))
  })
}

#' Generate summary statistics for a variable
var_summary <- function(x, name = "") {
  n <- length(x)
  n_miss <- sum(is.na(x))
  pct_miss <- round(100 * n_miss / n, 1)

  if (is.numeric(x)) {
    vals <- na.omit(x)
    list(
      variable = name, type = "numeric",
      n = n, n_missing = n_miss, pct_missing = pct_miss,
      mean = round(mean(vals), 2), sd = round(sd(vals), 2),
      median = round(median(vals), 2),
      min = round(min(vals), 2), max = round(max(vals), 2),
      n_unique = length(unique(vals))
    )
  } else {
    vals <- na.omit(x)
    freq <- sort(table(vals), decreasing = TRUE)
    list(
      variable = name, type = "categorical",
      n = n, n_missing = n_miss, pct_missing = pct_miss,
      n_levels = length(freq),
      top_level = if (length(freq) > 0) names(freq)[1] else NA,
      top_freq = if (length(freq) > 0) unname(freq[1]) else NA
    )
  }
}


# =============================================================================
# MODULE: Step 1 — Upload Data
# =============================================================================

upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4,
        div(class = "well",
          h4("Upload Your Data"),
          fileInput(ns("file"), "Choose file",
                    accept = c(".csv", ".tsv", ".xlsx", ".xls",
                               ".dta", ".sav", ".sas7bdat")),
          p(class = "text-muted small",
            "Supported: CSV, TSV, Excel (.xlsx/.xls), Stata (.dta),",
            "SPSS (.sav), SAS (.sas7bdat)"),
          hr(),
          h5("Or use example data"),
          actionButton(ns("use_example"), "Load mtcars example",
                       class = "btn-outline-secondary btn-sm")
        )
      ),
      column(8,
        conditionalPanel(
          condition = paste0("output['", ns("has_data"), "']"),
          div(class = "card",
            div(class = "card-header", h5("Data Preview")),
            div(class = "card-body",
              uiOutput(ns("data_info")),
              div(style = "overflow-x: auto;",
                tableOutput(ns("preview_table"))
              )
            )
          )
        )
      )
    )
  )
}

upload_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {

    observeEvent(input$file, {
      req(input$file)
      result <- safe_read_data(input$file$datapath, input$file$name)
      if (!is.null(result$error)) {
        showNotification(paste("Error:", result$error), type = "error")
        return()
      }
      shared$data <- result$data
      shared$data_name <- input$file$name
      shared$var_types <- classify_variables(result$data)
      showNotification(
        paste("Loaded", nrow(result$data), "rows x", ncol(result$data), "columns"),
        type = "message"
      )
    })

    observeEvent(input$use_example, {
      shared$data <- mtcars
      shared$data$car_name <- rownames(mtcars)
      shared$data_name <- "mtcars (example)"
      shared$var_types <- classify_variables(shared$data)
      showNotification("Loaded mtcars example data (32 rows x 12 columns)", type = "message")
    })

    output$has_data <- reactive(!is.null(shared$data))
    outputOptions(output, "has_data", suspendWhenHidden = FALSE)

    output$data_info <- renderUI({
      req(shared$data)
      df <- shared$data
      n_num <- sum(sapply(df, is.numeric))
      n_cat <- ncol(df) - n_num
      n_miss <- sum(is.na(df))
      tagList(
        p(strong("File:"), shared$data_name),
        p(strong("Dimensions:"), nrow(df), "rows x", ncol(df), "columns"),
        p(strong("Variables:"), n_num, "numeric,", n_cat, "categorical"),
        if (n_miss > 0) p(strong("Missing values:"), n_miss,
                          paste0("(", round(100 * n_miss / (nrow(df) * ncol(df)), 1), "%)"))
        else p(strong("Missing values:"), "None")
      )
    })

    output$preview_table <- renderTable({
      req(shared$data)
      head(shared$data, 10)
    }, striped = TRUE, hover = TRUE, spacing = "s")
  })
}


# =============================================================================
# MODULE: Step 2 — Explore Data
# =============================================================================

explore_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4,
        div(class = "well",
          h4("Data Explorer"),
          selectInput(ns("plot_var"), "Variable to visualize:", choices = NULL),
          hr(),
          h5("Variable Labels"),
          p(class = "text-muted small",
            "Labels carry through to Table 1 and regression output."),
          uiOutput(ns("label_editor"))
        )
      ),
      column(8,
        tabsetPanel(
          tabPanel("Summary",
            div(style = "overflow-x: auto; margin-top: 10px;",
              tableOutput(ns("summary_table"))
            )
          ),
          tabPanel("Distribution",
            plotOutput(ns("dist_plot"), height = "400px")
          ),
          tabPanel("Missing Data",
            plotOutput(ns("missing_plot"), height = "350px"),
            tableOutput(ns("missing_table"))
          )
        )
      )
    )
  )
}

explore_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      req(shared$data)
      updateSelectInput(session, "plot_var", choices = names(shared$data))
    })

    output$summary_table <- renderTable({
      req(shared$data)
      df <- shared$data
      summaries <- lapply(names(df), function(col) {
        s <- var_summary(df[[col]], col)
        if (s$type == "numeric") {
          data.frame(
            Variable = s$variable, Type = "Numeric",
            N = s$n, Missing = paste0(s$n_missing, " (", s$pct_missing, "%)"),
            `Mean (SD)` = paste0(s$mean, " (", s$sd, ")"),
            `Median [Min, Max]` = paste0(s$median, " [", s$min, ", ", s$max, "]"),
            Levels = "-",
            check.names = FALSE, stringsAsFactors = FALSE
          )
        } else {
          data.frame(
            Variable = s$variable, Type = "Categorical",
            N = s$n, Missing = paste0(s$n_missing, " (", s$pct_missing, "%)"),
            `Mean (SD)` = "-",
            `Median [Min, Max]` = "-",
            Levels = paste0(s$n_levels, " (top: ", s$top_level, ")"),
            check.names = FALSE, stringsAsFactors = FALSE
          )
        }
      })
      do.call(rbind, summaries)
    }, striped = TRUE, hover = TRUE, spacing = "s")

    output$dist_plot <- renderPlot({
      req(shared$data, input$plot_var)
      df <- shared$data
      v <- input$plot_var
      if (!(v %in% names(df))) return(NULL)
      x <- df[[v]]

      lbl <- v
      if (requireNamespace("labelled", quietly = TRUE)) {
        l <- labelled::var_label(df[[v]])
        if (!is.null(l)) lbl <- paste0(l, " (", v, ")")
      }

      if (is.numeric(x)) {
        ggplot(data.frame(x = x), aes(x = x)) +
          geom_histogram(bins = 30, fill = "#4e79a7", color = "white", alpha = 0.8) +
          labs(title = paste("Distribution of", lbl), x = lbl, y = "Count") +
          theme_minimal(base_size = 14)
      } else {
        freq <- as.data.frame(table(x), stringsAsFactors = FALSE)
        names(freq) <- c("Level", "Count")
        freq <- freq[order(-freq$Count), ]
        if (nrow(freq) > 20) freq <- freq[1:20, ]
        freq$Level <- factor(freq$Level, levels = rev(freq$Level))
        ggplot(freq, aes(x = Level, y = Count)) +
          geom_col(fill = "#4e79a7", alpha = 0.8) +
          coord_flip() +
          labs(title = paste("Distribution of", lbl), x = lbl, y = "Count") +
          theme_minimal(base_size = 14)
      }
    })

    output$missing_plot <- renderPlot({
      req(shared$data)
      df <- shared$data
      miss_df <- data.frame(
        Variable = names(df),
        Pct_Missing = sapply(df, function(x) round(100 * sum(is.na(x)) / length(x), 1)),
        stringsAsFactors = FALSE
      )
      miss_df <- miss_df[order(-miss_df$Pct_Missing), ]
      miss_df$Variable <- factor(miss_df$Variable, levels = rev(miss_df$Variable))

      ggplot(miss_df, aes(x = Variable, y = Pct_Missing)) +
        geom_col(fill = ifelse(miss_df$Pct_Missing > 0, "#e15759", "#76b7b2"), alpha = 0.8) +
        coord_flip() +
        labs(title = "Missing Data by Variable", x = "", y = "% Missing") +
        theme_minimal(base_size = 14) +
        geom_hline(yintercept = c(5, 20), linetype = "dashed", color = "grey50")
    })

    output$missing_table <- renderTable({
      req(shared$data)
      df <- shared$data
      miss_df <- data.frame(
        Variable = names(df),
        N = sapply(df, length),
        N_Missing = sapply(df, function(x) sum(is.na(x))),
        Pct_Missing = sapply(df, function(x) round(100 * sum(is.na(x)) / length(x), 1)),
        stringsAsFactors = FALSE
      )
      miss_df[order(-miss_df$Pct_Missing), ]
    }, striped = TRUE, spacing = "s")

    # Variable label editor
    output$label_editor <- renderUI({
      req(shared$data)
      vars <- names(shared$data)
      # Show first 15 variables with text inputs for labels
      vars_show <- head(vars, 15)
      label_inputs <- lapply(vars_show, function(v) {
        current_label <- ""
        if (requireNamespace("labelled", quietly = TRUE)) {
          l <- labelled::var_label(shared$data[[v]])
          if (!is.null(l)) current_label <- l
        }
        textInput(ns(paste0("lbl_", v)), label = v,
                  value = current_label, placeholder = "Enter label...")
      })
      tagList(
        label_inputs,
        if (length(vars) > 15) p(class = "text-muted small",
          paste("Showing first 15 of", length(vars), "variables")),
        actionButton(ns("apply_labels"), "Apply Labels", class = "btn-primary btn-sm")
      )
    })

    observeEvent(input$apply_labels, {
      req(shared$data)
      if (!requireNamespace("labelled", quietly = TRUE)) return()
      df <- shared$data
      vars <- head(names(df), 15)
      for (v in vars) {
        lbl_val <- input[[paste0("lbl_", v)]]
        if (!is.null(lbl_val) && nchar(trimws(lbl_val)) > 0) {
          labelled::var_label(df[[v]]) <- trimws(lbl_val)
        }
      }
      shared$data <- df
      showNotification("Variable labels applied", type = "message")
    })
  })
}


# =============================================================================
# MODULE: Step 3 — Table 1 (Descriptive Statistics)
# =============================================================================

table1_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4,
        div(class = "well",
          h4("Table 1 Settings"),
          checkboxGroupInput(ns("vars"), "Variables to include:",
                             choices = NULL),
          hr(),
          selectInput(ns("by_var"), "Stratify by (optional):",
                      choices = c("(none)" = ""), selected = ""),
          checkboxInput(ns("add_overall"), "Add overall column", value = FALSE),
          checkboxInput(ns("add_p"), "Add p-values", value = FALSE),
          hr(),
          actionButton(ns("generate"), "Generate Table 1",
                       class = "btn-primary"),
          br(), br(),
          actionButton(ns("export_html"), "Copy HTML to clipboard",
                       class = "btn-outline-secondary btn-sm")
        )
      ),
      column(8,
        div(style = "margin-top: 10px;",
          uiOutput(ns("table1_output"))
        )
      )
    )
  )
}

table1_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      req(shared$data)
      vars <- names(shared$data)
      updateCheckboxGroupInput(session, "vars", choices = vars, selected = vars)
      updateSelectInput(session, "by_var",
                        choices = c("(none)" = "", vars))
    })

    table1_obj <- reactiveVal(NULL)

    observeEvent(input$generate, {
      req(shared$data)
      if (!requireNamespace("gtsummary", quietly = TRUE)) {
        showNotification("gtsummary package not available", type = "error")
        return()
      }

      selected_vars <- input$vars
      if (length(selected_vars) == 0) {
        showNotification("Select at least one variable", type = "warning")
        return()
      }

      by_var <- if (nchar(input$by_var) > 0) input$by_var else NULL

      # Build subset
      cols <- selected_vars
      if (!is.null(by_var) && !(by_var %in% cols)) cols <- c(cols, by_var)
      df_sub <- shared$data[, cols, drop = FALSE]

      # Ensure stratification variable is factor
      if (!is.null(by_var)) {
        df_sub[[by_var]] <- factor(df_sub[[by_var]])
      }

      tryCatch({
        tbl <- gtsummary::tbl_summary(
          df_sub,
          by = by_var,
          missing = "ifany"
        )
        if (input$add_p && !is.null(by_var)) {
          tbl <- gtsummary::add_p(tbl)
        }
        if (input$add_overall && !is.null(by_var)) {
          tbl <- gtsummary::add_overall(tbl)
        }
        table1_obj(tbl)
        shared$table1 <- tbl
        showNotification("Table 1 generated", type = "message")
      }, error = function(e) {
        showNotification(paste("Error:", conditionMessage(e)), type = "error")
      })
    })

    output$table1_output <- renderUI({
      tbl <- table1_obj()
      if (is.null(tbl)) {
        return(p(class = "text-muted", "Configure settings and click 'Generate Table 1'"))
      }
      tryCatch({
        if (requireNamespace("gt", quietly = TRUE)) {
          gt_tbl <- gtsummary::as_gt(tbl)
          html_str <- gt::as_raw_html(gt_tbl)
          HTML(html_str)
        } else {
          # Fallback: text output
          HTML(paste("<pre>", paste(capture.output(print(tbl)), collapse = "\n"), "</pre>"))
        }
      }, error = function(e) {
        HTML(paste("<pre>", paste(capture.output(print(tbl)), collapse = "\n"), "</pre>"))
      })
    })

    observeEvent(input$export_html, {
      tbl <- table1_obj()
      if (is.null(tbl)) {
        showNotification("Generate a table first", type = "warning")
        return()
      }
      tryCatch({
        gt_tbl <- gtsummary::as_gt(tbl)
        html_str <- gt::as_raw_html(gt_tbl)
        shared$last_export <- list(type = "table1", content = html_str)
        showNotification("Table HTML ready for export (see Results tab)", type = "message")
      }, error = function(e) {
        showNotification(paste("Export error:", conditionMessage(e)), type = "error")
      })
    })
  })
}


# =============================================================================
# MODULE: Step 4 — Regression Models
# =============================================================================

model_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4,
        div(class = "well",
          h4("Model Specification"),
          selectInput(ns("outcome"), "Outcome variable:", choices = NULL),
          selectInput(ns("model_type"), "Model type:", choices = c(
            "Linear regression" = "lm",
            "Logistic regression" = "glm",
            "Cox regression" = "cox",
            "Mixed model (Experimental)" = "lmer"
          )),
          checkboxGroupInput(ns("predictors"), "Predictor variables:", choices = NULL),
          hr(),
          conditionalPanel(
            condition = paste0("input['", ns("model_type"), "'] == 'cox'"),
            selectInput(ns("time_var"), "Time variable:", choices = NULL),
            selectInput(ns("event_var"), "Event variable:", choices = NULL)
          ),
          conditionalPanel(
            condition = paste0("input['", ns("model_type"), "'] == 'lmer'"),
            selectInput(ns("random_var"), "Random effect (grouping):", choices = NULL),
            p(class = "text-muted small",
              tags$span(class = "badge bg-warning", "Experimental"),
              "Mixed models may be slow in WebR.")
          ),
          hr(),
          checkboxInput(ns("robust_se"), "Cluster-robust SEs (sandwich)", value = FALSE),
          conditionalPanel(
            condition = paste0("input['", ns("robust_se"), "']"),
            selectInput(ns("cluster_var"), "Cluster variable:", choices = NULL)
          ),
          checkboxInput(ns("show_vif"), "Show VIF (multicollinearity)", value = FALSE),
          hr(),
          actionButton(ns("fit_model"), "Fit Model", class = "btn-primary")
        )
      ),
      column(8,
        tabsetPanel(
          tabPanel("Results",
            div(style = "margin-top: 10px;",
              uiOutput(ns("model_results"))
            )
          ),
          tabPanel("Diagnostics",
            uiOutput(ns("diagnostics_output")),
            plotOutput(ns("forest_plot"), height = "400px")
          )
        )
      )
    )
  )
}

model_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      req(shared$data)
      vars <- names(shared$data)
      num_vars <- vars[sapply(shared$data, is.numeric)]
      updateSelectInput(session, "outcome", choices = vars)
      updateCheckboxGroupInput(session, "predictors", choices = vars)
      updateSelectInput(session, "time_var", choices = num_vars)
      updateSelectInput(session, "event_var", choices = num_vars)
      updateSelectInput(session, "random_var", choices = vars)
      updateSelectInput(session, "cluster_var", choices = vars)
    })

    model_fit <- reactiveVal(NULL)
    model_tidy <- reactiveVal(NULL)

    observeEvent(input$fit_model, {
      req(shared$data)
      preds <- input$predictors
      outcome <- input$outcome

      if (length(preds) == 0 || is.null(outcome)) {
        showNotification("Select outcome and at least one predictor", type = "warning")
        return()
      }

      # Remove outcome from predictors if selected
      preds <- setdiff(preds, outcome)
      if (length(preds) == 0) {
        showNotification("Select predictors different from the outcome", type = "warning")
        return()
      }

      df <- shared$data
      formula_str <- paste(outcome, "~", paste(preds, collapse = " + "))

      tryCatch({
        mod <- switch(input$model_type,
          "lm" = lm(as.formula(formula_str), data = df),
          "glm" = {
            df[[outcome]] <- as.numeric(as.factor(df[[outcome]])) - 1
            glm(as.formula(formula_str), data = df, family = binomial)
          },
          "cox" = {
            req(input$time_var, input$event_var)
            if (!requireNamespace("survival", quietly = TRUE)) stop("survival not available")
            surv_formula <- as.formula(paste(
              "survival::Surv(", input$time_var, ",", input$event_var, ") ~",
              paste(preds, collapse = " + ")
            ))
            survival::coxph(surv_formula, data = df)
          },
          "lmer" = {
            req(input$random_var)
            if (!requireNamespace("lme4", quietly = TRUE)) stop("lme4 not available")
            mixed_formula <- as.formula(paste(
              outcome, "~", paste(preds, collapse = " + "),
              "+ (1 |", input$random_var, ")"
            ))
            lme4::lmer(mixed_formula, data = df)
          }
        )

        model_fit(mod)

        # Tidy results
        tidy_df <- broom::tidy(mod, conf.int = TRUE)

        # Apply robust SEs if requested
        if (input$robust_se && input$model_type %in% c("lm", "glm")) {
          req(input$cluster_var)
          if (requireNamespace("sandwich", quietly = TRUE) &&
              requireNamespace("lmtest", quietly = TRUE)) {
            robust <- lmtest::coeftest(mod,
              vcov = sandwich::vcovCL(mod, cluster = df[[input$cluster_var]]))
            tidy_df$estimate <- robust[, 1]
            tidy_df$std.error <- robust[, 2]
            tidy_df$statistic <- robust[, 3]
            tidy_df$p.value <- robust[, 4]
            tidy_df$conf.low <- tidy_df$estimate - 1.96 * tidy_df$std.error
            tidy_df$conf.high <- tidy_df$estimate + 1.96 * tidy_df$std.error
          }
        }

        # Exponentiate for logistic/Cox
        if (input$model_type %in% c("glm", "cox")) {
          tidy_df$OR_HR <- round(exp(tidy_df$estimate), 3)
          tidy_df$ci_lower <- round(exp(tidy_df$conf.low), 3)
          tidy_df$ci_upper <- round(exp(tidy_df$conf.high), 3)
        }

        model_tidy(tidy_df)
        shared$model_result <- list(fit = mod, tidy = tidy_df, type = input$model_type)
        showNotification("Model fitted successfully", type = "message")

      }, error = function(e) {
        showNotification(paste("Model error:", conditionMessage(e)), type = "error")
      })
    })

    output$model_results <- renderUI({
      tidy_df <- model_tidy()
      if (is.null(tidy_df)) {
        return(p(class = "text-muted", "Configure model and click 'Fit Model'"))
      }

      # Format table
      display_df <- tidy_df
      display_df$estimate <- round(display_df$estimate, 4)
      display_df$std.error <- round(display_df$std.error, 4)
      display_df$statistic <- round(display_df$statistic, 3)
      display_df$p.value <- ifelse(display_df$p.value < 0.001, "<0.001",
                                   round(display_df$p.value, 4))
      if ("conf.low" %in% names(display_df))
        display_df$conf.low <- round(as.numeric(display_df$conf.low), 4)
      if ("conf.high" %in% names(display_df))
        display_df$conf.high <- round(as.numeric(display_df$conf.high), 4)

      if (requireNamespace("gt", quietly = TRUE)) {
        tryCatch({
          gt_tbl <- gt::gt(display_df) |>
            gt::tab_header(title = paste("Regression Results —",
              switch(input$model_type,
                "lm" = "Linear Regression",
                "glm" = "Logistic Regression",
                "cox" = "Cox Proportional Hazards",
                "lmer" = "Mixed Model"
              )))
          HTML(gt::as_raw_html(gt_tbl))
        }, error = function(e) {
          HTML(paste("<pre>", paste(capture.output(print(display_df)), collapse = "\n"), "</pre>"))
        })
      } else {
        HTML(paste("<pre>", paste(capture.output(print(display_df)), collapse = "\n"), "</pre>"))
      }
    })

    output$diagnostics_output <- renderUI({
      mod <- model_fit()
      if (is.null(mod)) return(p(class = "text-muted", "Fit a model first"))

      diag_html <- ""

      # VIF
      if (input$show_vif && input$model_type %in% c("lm", "glm")) {
        if (requireNamespace("car", quietly = TRUE)) {
          tryCatch({
            vif_vals <- car::vif(mod)
            if (is.matrix(vif_vals)) vif_vals <- vif_vals[, "GVIF"]
            vif_df <- data.frame(
              Variable = names(vif_vals),
              VIF = round(vif_vals, 3),
              Flag = ifelse(vif_vals > 10, "HIGH", ifelse(vif_vals > 5, "Moderate", "OK")),
              stringsAsFactors = FALSE
            )
            diag_html <- paste0(diag_html,
              "<h5>Variance Inflation Factors (VIF)</h5>",
              "<p class='text-muted small'>VIF > 5: moderate concern. VIF > 10: serious multicollinearity.</p>")
            if (requireNamespace("gt", quietly = TRUE)) {
              diag_html <- paste0(diag_html, gt::as_raw_html(gt::gt(vif_df)))
            } else {
              diag_html <- paste0(diag_html, "<pre>",
                paste(capture.output(print(vif_df, row.names = FALSE)), collapse = "\n"),
                "</pre>")
            }
          }, error = function(e) {
            diag_html <<- paste0(diag_html, "<p>VIF error: ", conditionMessage(e), "</p>")
          })
        }
      }

      # Estimated marginal means
      if (input$model_type %in% c("lm", "glm")) {
        if (requireNamespace("emmeans", quietly = TRUE)) {
          tryCatch({
            # Find factor predictors
            preds <- input$predictors
            factor_preds <- preds[sapply(shared$data[preds], function(x) !is.numeric(x) || length(unique(x)) <= 5)]
            if (length(factor_preds) > 0) {
              emm <- emmeans::emmeans(mod, factor_preds[1])
              emm_df <- as.data.frame(summary(emm))
              diag_html <- paste0(diag_html,
                "<h5>Estimated Marginal Means: ", factor_preds[1], "</h5>")
              if (requireNamespace("gt", quietly = TRUE)) {
                emm_df[] <- lapply(emm_df, function(x) if (is.numeric(x)) round(x, 3) else x)
                diag_html <- paste0(diag_html, gt::as_raw_html(gt::gt(emm_df)))
              } else {
                diag_html <- paste0(diag_html, "<pre>",
                  paste(capture.output(print(emm_df)), collapse = "\n"), "</pre>")
              }
            }
          }, error = function(e) NULL)
        }
      }

      if (nchar(diag_html) == 0) diag_html <- "<p class='text-muted'>No diagnostics to show. Enable VIF or fit a model with factor predictors for marginal means.</p>"
      HTML(diag_html)
    })

    output$forest_plot <- renderPlot({
      tidy_df <- model_tidy()
      if (is.null(tidy_df)) return(NULL)

      # Forest plot of coefficients
      plot_df <- tidy_df[tidy_df$term != "(Intercept)", ]
      if (nrow(plot_df) == 0) return(NULL)

      # Use OR/HR if available, otherwise raw estimates
      if ("OR_HR" %in% names(plot_df)) {
        plot_df$est <- plot_df$OR_HR
        plot_df$lo <- plot_df$ci_lower
        plot_df$hi <- plot_df$ci_upper
        x_label <- if (input$model_type == "glm") "Odds Ratio" else "Hazard Ratio"
        ref_line <- 1
      } else {
        plot_df$est <- plot_df$estimate
        plot_df$lo <- plot_df$conf.low
        plot_df$hi <- plot_df$conf.high
        x_label <- "Coefficient"
        ref_line <- 0
      }

      plot_df$term <- factor(plot_df$term, levels = rev(plot_df$term))

      ggplot(plot_df, aes(x = est, y = term)) +
        geom_point(size = 3, color = "#4e79a7") +
        geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.2, color = "#4e79a7") +
        geom_vline(xintercept = ref_line, linetype = "dashed", color = "grey50") +
        labs(title = "Forest Plot", x = x_label, y = "") +
        theme_minimal(base_size = 14)
    })
  })
}


# =============================================================================
# MODULE: Step 5 — Causal Inference Guidance
# =============================================================================

causal_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4,
        div(class = "well",
          h4("Causal Structure"),
          p(class = "text-muted small",
            "Define the causal relationships between your variables.",
            "This generates a DAG and recommends adjustment sets."),
          selectInput(ns("exposure"), "Exposure (treatment):", choices = NULL),
          selectInput(ns("outcome_var"), "Outcome:", choices = NULL),
          hr(),
          h5("Covariates"),
          uiOutput(ns("covariate_roles")),
          hr(),
          actionButton(ns("build_dag"), "Build DAG", class = "btn-primary"),
          br(), br(),
          actionButton(ns("suggest_adjust"), "Suggest Adjustment Set",
                       class = "btn-outline-info btn-sm")
        )
      ),
      column(8,
        tabsetPanel(
          tabPanel("DAG",
            plotOutput(ns("dag_plot"), height = "500px")
          ),
          tabPanel("Adjustment Set",
            uiOutput(ns("adjustment_info"))
          ),
          tabPanel("Guidance",
            uiOutput(ns("causal_guidance"))
          )
        )
      )
    )
  )
}

causal_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      req(shared$data)
      vars <- names(shared$data)
      updateSelectInput(session, "exposure", choices = vars)
      updateSelectInput(session, "outcome_var", choices = vars)
    })

    # Dynamic covariate role assignment
    output$covariate_roles <- renderUI({
      req(shared$data, input$exposure, input$outcome_var)
      other_vars <- setdiff(names(shared$data), c(input$exposure, input$outcome_var))
      if (length(other_vars) == 0) return(p("No other variables"))
      other_vars <- head(other_vars, 10)  # limit to 10

      lapply(other_vars, function(v) {
        selectInput(ns(paste0("role_", v)), label = v,
                    choices = c("Not included" = "none",
                                "Confounder" = "confounder",
                                "Mediator" = "mediator",
                                "Collider" = "collider",
                                "Instrument" = "instrument",
                                "Ancestor of exposure" = "ancestor_exp",
                                "Ancestor of outcome" = "ancestor_out"),
                    selected = "none")
      })
    })

    dag_obj <- reactiveVal(NULL)

    observeEvent(input$build_dag, {
      req(input$exposure, input$outcome_var, shared$data)
      if (!requireNamespace("ggdag", quietly = TRUE)) {
        showNotification("ggdag package not available", type = "error")
        return()
      }

      other_vars <- setdiff(names(shared$data), c(input$exposure, input$outcome_var))
      other_vars <- head(other_vars, 10)

      # Build dagify formula list
      # outcome ~ exposure always
      formulas <- list()
      formulas[[1]] <- as.formula(paste(input$outcome_var, "~", input$exposure))

      for (v in other_vars) {
        role <- input[[paste0("role_", v)]]
        if (is.null(role) || role == "none") next

        if (role == "confounder") {
          # Confounder -> exposure and confounder -> outcome
          formulas[[length(formulas) + 1]] <- as.formula(paste(input$exposure, "~", v))
          formulas[[length(formulas) + 1]] <- as.formula(paste(input$outcome_var, "~", v))
        } else if (role == "mediator") {
          # exposure -> mediator -> outcome
          formulas[[length(formulas) + 1]] <- as.formula(paste(v, "~", input$exposure))
          formulas[[length(formulas) + 1]] <- as.formula(paste(input$outcome_var, "~", v))
        } else if (role == "collider") {
          # exposure -> collider <- outcome
          formulas[[length(formulas) + 1]] <- as.formula(paste(v, "~", input$exposure))
          formulas[[length(formulas) + 1]] <- as.formula(paste(v, "~", input$outcome_var))
        } else if (role == "instrument") {
          # instrument -> exposure
          formulas[[length(formulas) + 1]] <- as.formula(paste(input$exposure, "~", v))
        } else if (role == "ancestor_exp") {
          formulas[[length(formulas) + 1]] <- as.formula(paste(input$exposure, "~", v))
        } else if (role == "ancestor_out") {
          formulas[[length(formulas) + 1]] <- as.formula(paste(input$outcome_var, "~", v))
        }
      }

      tryCatch({
        dag <- do.call(ggdag::dagify, c(formulas, list(
          exposure = input$exposure,
          outcome = input$outcome_var
        )))
        dag_obj(dag)
        shared$dag <- dag
        showNotification("DAG built successfully", type = "message")
      }, error = function(e) {
        showNotification(paste("DAG error:", conditionMessage(e)), type = "error")
      })
    })

    output$dag_plot <- renderPlot({
      dag <- dag_obj()
      if (is.null(dag)) {
        plot.new()
        text(0.5, 0.5, "Define variables and click 'Build DAG'",
             cex = 1.5, col = "grey50")
        return()
      }
      tryCatch({
        ggdag::ggdag_status(dag) +
          ggdag::theme_dag() +
          labs(title = "Directed Acyclic Graph (DAG)")
      }, error = function(e) {
        ggdag::ggdag(dag) +
          ggdag::theme_dag() +
          labs(title = "Directed Acyclic Graph (DAG)")
      })
    })

    output$adjustment_info <- renderUI({
      dag <- dag_obj()
      if (is.null(dag)) {
        return(p(class = "text-muted", "Build a DAG first"))
      }

      tryCatch({
        adj_sets <- ggdag::dagify_adjust <- ggdag::ggdag_adjustment_set(dag)

        # Get adjustment set
        adj <- ggdag::dag_adjustment_sets(dag)
        if (length(adj) == 0) {
          return(tagList(
            h5("Minimal Sufficient Adjustment Set"),
            p("No adjustment needed — the causal effect is already identified",
              "without conditioning on any covariates."),
            p(class = "text-muted small",
              "This may occur when there are no backdoor paths between",
              "the exposure and outcome.")
          ))
        }

        adj_vars <- lapply(adj, function(s) paste(s, collapse = ", "))
        tagList(
          h5("Minimal Sufficient Adjustment Set(s)"),
          p("To estimate the causal effect of",
            strong(input$exposure), "on", strong(input$outcome_var),
            ", adjust for:"),
          tags$ul(
            lapply(adj_vars, function(s) tags$li(strong(s)))
          ),
          hr(),
          p(class = "text-muted small",
            "These are the minimal sets of variables to condition on",
            "to block all backdoor paths. Any one set is sufficient.")
        )
      }, error = function(e) {
        p("Could not compute adjustment sets:", conditionMessage(e))
      })
    })

    output$causal_guidance <- renderUI({
      tagList(
        h5("Causal Inference Quick Guide"),
        tags$dl(
          tags$dt("Confounder"),
          tags$dd("A variable that causes both the exposure and outcome.",
                  "You SHOULD adjust for confounders to remove bias."),
          tags$dt("Mediator"),
          tags$dd("A variable on the causal pathway (exposure -> mediator -> outcome).",
                  "Do NOT adjust if you want the total effect.",
                  "Adjust only if you want the direct effect."),
          tags$dt("Collider"),
          tags$dd("A variable caused by both the exposure and outcome.",
                  "Do NOT adjust for colliders — conditioning on a collider",
                  "introduces spurious association (collider bias)."),
          tags$dt("Instrument"),
          tags$dd("A variable that affects the exposure but has no direct effect",
                  "on the outcome except through the exposure.",
                  "Useful for instrumental variable analysis.")
        ),
        hr(),
        h5("Common Mistakes"),
        tags$ul(
          tags$li("Adjusting for a ", strong("mediator"), " when you want the total effect"),
          tags$li("Adjusting for a ", strong("collider"), " (opens a biasing pathway)"),
          tags$li("Failing to adjust for a ", strong("confounder"), " (leaves backdoor path open)"),
          tags$li("Including ", strong("descendants of the outcome"), " as predictors")
        )
      )
    })
  })
}


# =============================================================================
# MODULE: Step 6 — Results & Export
# =============================================================================

results_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12,
        div(class = "card",
          div(class = "card-header",
            h4("Results Summary"),
            p(class = "text-muted small",
              "All analysis was performed in your browser. No data was uploaded to any server.")
          ),
          div(class = "card-body",
            tabsetPanel(
              tabPanel("Table 1",
                uiOutput(ns("show_table1"))
              ),
              tabPanel("Regression",
                uiOutput(ns("show_regression"))
              ),
              tabPanel("DAG",
                plotOutput(ns("show_dag"), height = "450px")
              ),
              tabPanel("Export",
                h5("Download Results"),
                p("Click below to download a markdown report with all results."),
                actionButton(ns("download_report"), "Download Report (.md)",
                             class = "btn-primary"),
                hr(),
                h5("Results Preview"),
                tags$textarea(id = ns("report_text"), rows = 20,
                  style = "width:100%; font-family:monospace; font-size:12px;",
                  readonly = "readonly",
                  "Generate Table 1 or fit a model, then come here to export."),
                tags$script(HTML(sprintf("
                  Shiny.addCustomMessageHandler('%s-downloadReport', function(msg) {
                    var blob = new Blob([msg.content], {type: 'text/markdown'});
                    var url = URL.createObjectURL(blob);
                    var a = document.createElement('a');
                    a.href = url;
                    a.download = msg.filename;
                    document.body.appendChild(a);
                    a.click();
                    document.body.removeChild(a);
                    URL.revokeObjectURL(url);
                  });
                  Shiny.addCustomMessageHandler('%s-updateReport', function(msg) {
                    document.getElementById('%s').value = msg;
                  });
                ", ns(""), ns(""), ns("report_text"))))
              )
            )
          )
        )
      )
    )
  )
}

results_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$show_table1 <- renderUI({
      tbl <- shared$table1
      if (is.null(tbl)) return(p(class = "text-muted", "No Table 1 generated yet. Go to Step 3."))
      tryCatch({
        gt_tbl <- gtsummary::as_gt(tbl)
        HTML(gt::as_raw_html(gt_tbl))
      }, error = function(e) {
        HTML(paste("<pre>", paste(capture.output(print(tbl)), collapse = "\n"), "</pre>"))
      })
    })

    output$show_regression <- renderUI({
      res <- shared$model_result
      if (is.null(res)) return(p(class = "text-muted", "No model fitted yet. Go to Step 4."))

      display_df <- res$tidy
      display_df$estimate <- round(display_df$estimate, 4)
      display_df$std.error <- round(display_df$std.error, 4)
      display_df$statistic <- round(display_df$statistic, 3)
      display_df$p.value <- ifelse(display_df$p.value < 0.001, "<0.001",
                                   round(display_df$p.value, 4))

      tryCatch({
        gt_tbl <- gt::gt(display_df) |>
          gt::tab_header(title = paste("Regression Results"))
        HTML(gt::as_raw_html(gt_tbl))
      }, error = function(e) {
        HTML(paste("<pre>", paste(capture.output(print(display_df)), collapse = "\n"), "</pre>"))
      })
    })

    output$show_dag <- renderPlot({
      dag <- shared$dag
      if (is.null(dag)) {
        plot.new()
        text(0.5, 0.5, "No DAG built yet. Go to Step 5.", cex = 1.5, col = "grey50")
        return()
      }
      tryCatch({
        ggdag::ggdag_status(dag) +
          ggdag::theme_dag() +
          labs(title = "Directed Acyclic Graph (DAG)")
      }, error = function(e) {
        ggdag::ggdag(dag) + ggdag::theme_dag()
      })
    })

    generate_report <- function() {
      lines <- c(
        "# Statistical Analysis Report",
        "",
        paste0("**Generated:** ", Sys.time()),
        paste0("**Data:** ", shared$data_name %||% "Unknown"),
        paste0("**R Version:** ", R.version.string),
        "",
        "---",
        "",
        "*All analysis performed in-browser via WebR. No data was uploaded to any server.*",
        ""
      )

      # Data summary
      if (!is.null(shared$data)) {
        df <- shared$data
        lines <- c(lines,
          "## Data Summary",
          "",
          paste0("- **Rows:** ", nrow(df)),
          paste0("- **Columns:** ", ncol(df)),
          paste0("- **Numeric variables:** ", sum(sapply(df, is.numeric))),
          paste0("- **Missing values:** ", sum(is.na(df))),
          ""
        )
      }

      # Table 1
      if (!is.null(shared$table1)) {
        lines <- c(lines,
          "## Table 1: Descriptive Statistics",
          "",
          paste(capture.output(print(shared$table1)), collapse = "\n"),
          ""
        )
      }

      # Regression
      if (!is.null(shared$model_result)) {
        lines <- c(lines,
          "## Regression Results",
          "",
          paste0("**Model type:** ", shared$model_result$type),
          "",
          "| Term | Estimate | Std.Error | Statistic | P-value |",
          "|------|----------|-----------|-----------|---------|"
        )
        tidy_df <- shared$model_result$tidy
        for (i in seq_len(nrow(tidy_df))) {
          row <- tidy_df[i, ]
          lines <- c(lines, sprintf("| %s | %.4f | %.4f | %.3f | %s |",
            row$term, row$estimate, row$std.error, row$statistic,
            if (row$p.value < 0.001) "<0.001" else round(row$p.value, 4)
          ))
        }
        lines <- c(lines, "")
      }

      paste(lines, collapse = "\n")
    }

    observeEvent(input$download_report, {
      md <- generate_report()
      session$sendCustomMessage(paste0(ns(""), "downloadReport"), list(
        content = md,
        filename = paste0("analysis_report_", Sys.Date(), ".md")
      ))
      session$sendCustomMessage(paste0(ns(""), "updateReport"), md)
    })
  })
}


# =============================================================================
# MAIN UI
# =============================================================================

ui <- fluidPage(
  theme = NULL,  # Use default Shiny theme for Shinylive compatibility
  tags$head(
    tags$style(HTML("
      .navbar { margin-bottom: 0; }
      .step-header {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white; padding: 15px 20px; margin-bottom: 15px;
        border-radius: 4px;
      }
      .step-header h3 { margin: 0 0 5px 0; }
      .step-header p { margin: 0; opacity: 0.9; font-size: 14px; }
      .wizard-nav {
        background-color: #f8f9fa; padding: 10px 20px;
        border-top: 1px solid #dee2e6; margin-top: 20px;
        display: flex; justify-content: space-between; align-items: center;
      }
      .step-indicator {
        display: flex; gap: 8px; align-items: center;
      }
      .step-dot {
        width: 32px; height: 32px; border-radius: 50%;
        display: flex; align-items: center; justify-content: center;
        font-size: 14px; font-weight: bold; cursor: pointer;
        border: 2px solid #dee2e6; background: white; color: #6c757d;
      }
      .step-dot.active {
        background: #667eea; color: white; border-color: #667eea;
      }
      .step-dot.completed {
        background: #28a745; color: white; border-color: #28a745;
      }
      .privacy-banner {
        background-color: #d4edda; border: 1px solid #c3e6cb;
        padding: 8px 15px; text-align: center; font-size: 13px;
        color: #155724; margin-bottom: 10px; border-radius: 4px;
      }
      .card { border: 1px solid #dee2e6; border-radius: 4px; margin-bottom: 15px; }
      .card-header { background-color: #f8f9fa; padding: 10px 15px; border-bottom: 1px solid #dee2e6; }
      .card-body { padding: 15px; }
      .well { background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 4px; padding: 15px; }
    "))
  ),

  # Privacy banner
  div(class = "privacy-banner",
    "Your data never leaves your browser. All analysis runs locally via WebR."
  ),

  # Title
  titlePanel(
    div(style = "display: flex; align-items: center; gap: 15px;",
      div(
        h3("Statistical Analysis", style = "margin: 0;"),
        p("In-browser analysis powered by WebR", style = "margin: 0; color: #6c757d; font-size: 14px;")
      )
    )
  ),

  # Step indicator
  uiOutput("step_indicators"),

  hr(style = "margin: 5px 0 10px 0;"),

  # Wizard content (hidden tabset)
  tabsetPanel(
    id = "wizard",
    type = "hidden",

    tabPanelBody("step1",
      div(class = "step-header",
        h3("Step 1: Upload Data"),
        p("Import your dataset. Supported formats: CSV, Excel, Stata, SPSS.")
      ),
      upload_ui("upload")
    ),

    tabPanelBody("step2",
      div(class = "step-header",
        h3("Step 2: Explore Data"),
        p("Review variable distributions, missing data, and set labels.")
      ),
      explore_ui("explore")
    ),

    tabPanelBody("step3",
      div(class = "step-header",
        h3("Step 3: Table 1"),
        p("Generate publication-ready descriptive statistics table.")
      ),
      table1_ui("table1")
    ),

    tabPanelBody("step4",
      div(class = "step-header",
        h3("Step 4: Regression Model"),
        p("Fit linear, logistic, Cox, or mixed models with robust SEs and diagnostics.")
      ),
      model_ui("model")
    ),

    tabPanelBody("step5",
      div(class = "step-header",
        h3("Step 5: Causal Inference"),
        p("Define causal structure, build a DAG, and identify adjustment sets.")
      ),
      causal_ui("causal")
    ),

    tabPanelBody("step6",
      div(class = "step-header",
        h3("Step 6: Results & Export"),
        p("Review all results and download your analysis report.")
      ),
      results_ui("results")
    )
  ),

  # Navigation
  div(class = "wizard-nav",
    actionButton("prev_step", "Previous", class = "btn-outline-secondary"),
    uiOutput("step_label"),
    actionButton("next_step", "Next", class = "btn-primary")
  )
)


# =============================================================================
# MAIN SERVER
# =============================================================================

server <- function(input, output, session) {

  # Shared state across all modules
  shared <- reactiveValues(
    data = NULL,
    data_name = NULL,
    var_types = NULL,
    table1 = NULL,
    model_result = NULL,
    dag = NULL,
    last_export = NULL
  )

  # Current step tracker
  current_step <- reactiveVal(1)
  total_steps <- 6
  step_names <- c("Upload", "Explore", "Table 1", "Model", "Causal", "Results")
  step_ids <- paste0("step", 1:total_steps)

  # Step indicator
  output$step_indicators <- renderUI({
    step <- current_step()
    dots <- lapply(1:total_steps, function(i) {
      cls <- "step-dot"
      if (i == step) cls <- paste(cls, "active")
      else if (i < step) cls <- paste(cls, "completed")
      tag <- tags$div(class = cls, i,
                      title = paste("Step", i, "-", step_names[i]),
                      onclick = sprintf("Shiny.setInputValue('goto_step', %d, {priority: 'event'})", i))
      tagList(tag,
        if (i < total_steps) tags$div(style = "width:30px; height:2px; background:#dee2e6;"))
    })
    div(class = "step-indicator", style = "justify-content: center; margin: 10px 0;", dots)
  })

  output$step_label <- renderUI({
    step <- current_step()
    p(style = "margin: 0; font-weight: bold;",
      paste0("Step ", step, " of ", total_steps, ": ", step_names[step]))
  })

  # Navigation
  observeEvent(input$next_step, {
    step <- current_step()
    if (step < total_steps) {
      current_step(step + 1)
      updateTabsetPanel(session, "wizard", selected = step_ids[step + 1])
    }
  })

  observeEvent(input$prev_step, {
    step <- current_step()
    if (step > 1) {
      current_step(step - 1)
      updateTabsetPanel(session, "wizard", selected = step_ids[step - 1])
    }
  })

  observeEvent(input$goto_step, {
    step <- input$goto_step
    if (step >= 1 && step <= total_steps) {
      current_step(step)
      updateTabsetPanel(session, "wizard", selected = step_ids[step])
    }
  })

  # Module servers
  upload_server("upload", shared)
  explore_server("explore", shared)
  table1_server("table1", shared)
  model_server("model", shared)
  causal_server("causal", shared)
  results_server("results", shared)
}


# =============================================================================
# Launch
# =============================================================================
shinyApp(ui, server)
