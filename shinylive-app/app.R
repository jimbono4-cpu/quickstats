# =============================================================================
# Statistical Analysis Web App — Shinylive (WebR)
# Phase 1: Complete 6-step wizard for in-browser statistical analysis
#
# All computation runs client-side via WebR/WASM. No data leaves the browser.
# =============================================================================

library(shiny)

# --- WebR detection and package installation ---------------------------------

is_webr <- function() {
  Sys.getenv("WEBR") != "" ||
    isTRUE(grepl("wasm", R.version$platform)) ||
    exists("webr", mode = "environment")
}

install_if_needed <- function(pkg_name) {
  if (!requireNamespace(pkg_name, quietly = TRUE)) {
    # Try webr::install first (works in Shinylive), fall back to install.packages
    tryCatch(
      webr::install(pkg_name, quiet = TRUE),
      error = function(e) {
        tryCatch(
          install.packages(pkg_name, repos = "https://cloud.r-project.org", quiet = TRUE),
          error = function(e2) NULL
        )
      }
    )
    # Force-load into the R session
    tryCatch(
      suppressPackageStartupMessages(
        library(pkg_name, character.only = TRUE, quietly = TRUE)
      ),
      error = function(e) NULL
    )
  }
}

# --- WebR-safe HTML table rendering ------------------------------------------

#' Render a data.frame as a publication-quality HTML table
df_to_html_table <- function(df, title = NULL, id = NULL, footnote = NULL) {
  if (is.null(df) || nrow(df) == 0) return("<p class='text-muted'>No data to display.</p>")
  id_attr <- if (!is.null(id)) paste0(' id="', id, '"') else ""
  html <- paste0('<div', id_attr, '>')
  if (!is.null(title)) {
    html <- paste0(html,
      '<p style="font-weight:bold; font-size:14px; margin-bottom:4px;">',
      htmltools::htmlEscape(title), '</p>')
  }
  html <- paste0(html,
    '<table style="width:100%; border-collapse:collapse; font-size:13px; ',
    'font-family: \'Segoe UI\', Tahoma, Geneva, Verdana, sans-serif; margin-bottom:8px;">',
    '<thead><tr style="border-top:2px solid #333; border-bottom:1px solid #333;">')
  for (col in names(df)) {
    html <- paste0(html,
      '<th style="padding:6px 10px; text-align:left; font-weight:bold;">',
      htmltools::htmlEscape(col), '</th>')
  }
  html <- paste0(html, '</tr></thead><tbody>')
  for (i in seq_len(nrow(df))) {
    # Bottom border on last row
    bdr <- if (i == nrow(df)) ' style="border-bottom:2px solid #333;"' else ""
    html <- paste0(html, '<tr', bdr, '>')
    for (j in seq_len(ncol(df))) {
      val <- df[i, j]
      if (is.numeric(val)) val <- round(val, 4)
      cell_text <- htmltools::htmlEscape(as.character(val))
      # Convert **bold** markdown to HTML bold (after escaping, safe)
      cell_text <- gsub("\\*\\*(.+?)\\*\\*", "<strong>\\1</strong>", cell_text)
      # Indent lines starting with spaces
      indent <- ""
      if (grepl("^\\s{2,}", as.character(val))) {
        indent <- "padding-left:20px;"
      }
      html <- paste0(html,
        '<td style="padding:4px 10px; border-bottom:1px solid #eee;', indent, '">',
        cell_text, '</td>')
    }
    html <- paste0(html, '</tr>')
  }
  html <- paste0(html, '</tbody></table>')
  if (!is.null(footnote)) {
    html <- paste0(html,
      '<p style="font-size:11px; color:#666; margin-top:2px; font-style:italic;">',
      htmltools::htmlEscape(footnote), '</p>')
  }
  html <- paste0(html, '</div>')
  html
}

# --- Helper functions ---------------------------------------------------------

#' Detect variable types using heuristics
#' Character/factor -> categorical
#' Numeric with <=10 unique non-NA values -> categorical
#' Otherwise numeric -> numeric
classify_variables <- function(df) {
  types <- sapply(names(df), function(col) {
    x <- df[[col]]
    if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) return("date")
    if (is.factor(x) || is.character(x) || is.logical(x)) return("categorical")
    if (is.numeric(x) || is.integer(x)) {
      n_unique <- length(unique(na.omit(x)))
      if (n_unique <= 10) return("categorical")
      return("numeric")
    }
    "other"
  })
  data.frame(variable = names(df), type = unname(types), stringsAsFactors = FALSE)
}

#' Prepare data for modelling: convert detected categoricals to factor
prepare_model_data <- function(df, var_types) {
  for (i in seq_len(nrow(var_types))) {
    v <- var_types$variable[i]
    if (var_types$type[i] == "categorical" && v %in% names(df)) {
      if (!is.factor(df[[v]])) {
        df[[v]] <- factor(df[[v]])
      }
    }
  }
  df
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

  if (is.numeric(x) && length(unique(na.omit(x))) > 10) {
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
      d <- mtcars
      d$car_name <- rownames(mtcars)
      shared$data <- d
      shared$data_name <- "mtcars (example)"
      shared$var_types <- classify_variables(d)
      showNotification("Loaded mtcars example data (32 rows x 12 columns)", type = "message")
    })

    output$has_data <- reactive(!is.null(shared$data))
    outputOptions(output, "has_data", suspendWhenHidden = FALSE)

    output$data_info <- renderUI({
      req(shared$data)
      df <- shared$data
      vt <- shared$var_types
      n_num <- sum(vt$type == "numeric")
      n_cat <- sum(vt$type == "categorical")
      n_miss <- sum(is.na(df))
      tagList(
        p(strong("File:"), shared$data_name),
        p(strong("Dimensions:"), nrow(df), "rows x", ncol(df), "columns"),
        p(strong("Detected types:"), n_num, "numeric,", n_cat, "categorical"),
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
      vt <- shared$var_types
      summaries <- lapply(names(df), function(col) {
        detected <- if (!is.null(vt) && col %in% vt$variable) {
          vt$type[vt$variable == col]
        } else "unknown"
        s <- var_summary(df[[col]], col)
        if (s$type == "numeric") {
          data.frame(
            Variable = s$variable, `Detected Type` = detected,
            N = s$n, Missing = paste0(s$n_missing, " (", s$pct_missing, "%)"),
            `Mean (SD)` = paste0(s$mean, " (", s$sd, ")"),
            `Median [Min, Max]` = paste0(s$median, " [", s$min, ", ", s$max, "]"),
            Levels = "-",
            check.names = FALSE, stringsAsFactors = FALSE
          )
        } else {
          data.frame(
            Variable = s$variable, `Detected Type` = detected,
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
      tryCatch({
        if (requireNamespace("labelled", quietly = TRUE)) {
          l <- labelled::var_label(df[[v]])
          if (!is.null(l)) lbl <- paste0(l, " (", v, ")")
        }
      }, error = function(e) NULL)

      vt <- shared$var_types
      is_cat <- (!is.null(vt) && v %in% vt$variable && vt$type[vt$variable == v] == "categorical") ||
                is.character(x) || is.factor(x)

      # Use base R graphics — always available, no package dependencies
      par(mar = c(5, 4, 3, 2), family = "sans")
      if (!is_cat && is.numeric(x)) {
        x_clean <- x[!is.na(x)]
        hist(x_clean, breaks = 30, col = "#4e79a7", border = "white",
             main = paste("Distribution of", lbl), xlab = lbl, ylab = "Count",
             cex.main = 1.3, cex.lab = 1.1)
      } else {
        freq <- sort(table(x), decreasing = TRUE)
        if (length(freq) > 20) freq <- freq[1:20]
        par(mar = c(5, max(8, max(nchar(names(freq))) * 0.6), 3, 2))
        barplot(rev(freq), horiz = TRUE, las = 1, col = "#4e79a7", border = NA,
                main = paste("Distribution of", lbl), xlab = "Count",
                cex.main = 1.3, cex.lab = 1.1, cex.names = 0.9)
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
      miss_df <- miss_df[order(miss_df$Pct_Missing), ]

      # Base R horizontal bar chart — no ggplot2 dependency
      colors <- ifelse(miss_df$Pct_Missing > 0, "#e15759", "#76b7b2")
      par(mar = c(5, max(8, max(nchar(miss_df$Variable)) * 0.6), 3, 2), family = "sans")
      barplot(miss_df$Pct_Missing, names.arg = miss_df$Variable,
              horiz = TRUE, las = 1, col = colors, border = NA,
              main = "Missing Data by Variable", xlab = "% Missing",
              cex.main = 1.3, cex.lab = 1.1, cex.names = 0.8)
      abline(v = c(5, 20), lty = 2, col = "grey50")
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
          checkboxInput(ns("test_normality"), "Test normality (use Median [IQR] if non-normal)",
                        value = FALSE),
          p(class = "text-muted small",
            "Default: Mean (SD) for all continuous variables.",
            "Tick to run Shapiro-Wilk test; non-normal variables (p \u2264 0.05)",
            "switch to Median (IQR)."),
          hr(),
          actionButton(ns("generate"), "Generate Table 1",
                       class = "btn-primary"),
          br(), br(),
          actionButton(ns("export_html"), "Copy table to clipboard",
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

      selected_vars <- input$vars
      if (length(selected_vars) == 0) {
        showNotification("Select at least one variable", type = "warning")
        return()
      }

      by_var <- if (nchar(input$by_var) > 0) input$by_var else NULL

      # Build subset and apply variable type detection
      cols <- selected_vars
      if (!is.null(by_var) && !(by_var %in% cols)) cols <- c(cols, by_var)
      df_sub <- shared$data[, cols, drop = FALSE]

      # Convert detected categoricals to factor
      if (!is.null(shared$var_types)) {
        df_sub <- prepare_model_data(df_sub, shared$var_types)
      }

      # Ensure stratification variable is factor
      if (!is.null(by_var)) {
        df_sub[[by_var]] <- factor(df_sub[[by_var]])
      }

      tryCatch({
        vt <- shared$var_types
        analysis_vars <- setdiff(cols, by_var)
        normal_vars <- c()
        nonnormal_vars <- c()

        # Classify normality
        for (v in analysis_vars) {
          is_cat <- is.factor(df_sub[[v]]) || is.character(df_sub[[v]]) ||
                    (!is.null(vt) && v %in% vt$variable && vt$type[vt$variable == v] == "categorical")
          if (is_cat) next
          normal_vars <- c(normal_vars, v)
          if (isTRUE(input$test_normality)) {
            x <- na.omit(df_sub[[v]])
            if (length(x) >= 3) {
              is_normal <- tryCatch({
                if (length(x) > 5000) { set.seed(42); x <- sample(x, 5000) }
                shapiro.test(x)$p.value > 0.05
              }, error = function(e) TRUE)
              if (!is_normal) {
                normal_vars <- setdiff(normal_vars, v)
                nonnormal_vars <- c(nonnormal_vars, v)
              }
            }
          }
        }

        # --- Build Table 1 using base R (WebR-safe, no gtsummary) ---
        groups <- if (!is.null(by_var)) levels(df_sub[[by_var]]) else NULL
        n_total <- nrow(df_sub)
        rows <- list()

        for (v in analysis_vars) {
          is_cat <- is.factor(df_sub[[v]]) || is.character(df_sub[[v]]) ||
                    (!is.null(vt) && v %in% vt$variable && vt$type[vt$variable == v] == "categorical")
          if (is_cat) {
            # Categorical variable
            lvls <- if (is.factor(df_sub[[v]])) levels(df_sub[[v]]) else sort(unique(na.omit(df_sub[[v]])))
            # Header row for this variable
            row <- list(Variable = paste0("**", v, "**"), Statistic = "n (%)")
            if (!is.null(groups)) {
              for (g in groups) row[[paste0(by_var, ": ", g)]] <- ""
            }
            row[["Overall"]] <- ""
            rows <- c(rows, list(row))
            # Level rows
            for (lv in lvls) {
              row <- list(Variable = paste0("    ", lv), Statistic = "")
              if (!is.null(groups)) {
                for (g in groups) {
                  sub <- df_sub[df_sub[[by_var]] == g, , drop = FALSE]
                  n_lv <- sum(sub[[v]] == lv, na.rm = TRUE)
                  pct <- round(100 * n_lv / nrow(sub), 1)
                  row[[paste0(by_var, ": ", g)]] <- paste0(n_lv, " (", pct, "%)")
                }
              }
              n_lv_all <- sum(df_sub[[v]] == lv, na.rm = TRUE)
              pct_all <- round(100 * n_lv_all / n_total, 1)
              row[["Overall"]] <- paste0(n_lv_all, " (", pct_all, "%)")
              rows <- c(rows, list(row))
            }
            # p-value row for categorical
            if (input$add_p && !is.null(by_var)) {
              p_val <- tryCatch({
                tbl_test <- table(df_sub[[v]], df_sub[[by_var]])
                chisq.test(tbl_test)$p.value
              }, error = function(e) NA)
              rows[[length(rows)]][["p-value"]] <- if (!is.na(p_val)) {
                if (p_val < 0.001) "<0.001" else as.character(round(p_val, 3))
              } else ""
            }
          } else {
            # Continuous variable
            is_nonnormal <- v %in% nonnormal_vars
            stat_label <- if (is_nonnormal) "Median (IQR)" else "Mean (SD)"
            row <- list(Variable = paste0("**", v, "**"), Statistic = stat_label)
            fmt_stat <- function(x) {
              x <- na.omit(x)
              if (is_nonnormal) {
                paste0(round(median(x), 2), " (", round(quantile(x, 0.25), 2), ", ", round(quantile(x, 0.75), 2), ")")
              } else {
                paste0(round(mean(x), 2), " (", round(sd(x), 2), ")")
              }
            }
            if (!is.null(groups)) {
              for (g in groups) {
                sub <- df_sub[df_sub[[by_var]] == g, , drop = FALSE]
                row[[paste0(by_var, ": ", g)]] <- fmt_stat(sub[[v]])
              }
            }
            row[["Overall"]] <- fmt_stat(df_sub[[v]])
            # p-value
            if (input$add_p && !is.null(by_var)) {
              p_val <- tryCatch({
                n_groups <- length(groups)
                if (is_nonnormal) {
                  if (n_groups == 2) wilcox.test(df_sub[[v]] ~ df_sub[[by_var]])$p.value
                  else kruskal.test(df_sub[[v]] ~ df_sub[[by_var]])$p.value
                } else {
                  if (n_groups == 2) t.test(df_sub[[v]] ~ df_sub[[by_var]])$p.value
                  else summary(aov(df_sub[[v]] ~ df_sub[[by_var]]))[[1]][["Pr(>F)"]][1]
                }
              }, error = function(e) NA)
              row[["p-value"]] <- if (!is.na(p_val)) {
                if (p_val < 0.001) "<0.001" else as.character(round(p_val, 3))
              } else ""
            }
            rows <- c(rows, list(row))
          }
        }

        # Build data frame from rows
        all_cols <- unique(unlist(lapply(rows, names)))
        tbl_df <- do.call(rbind, lapply(rows, function(r) {
          vals <- sapply(all_cols, function(c) if (!is.null(r[[c]])) as.character(r[[c]]) else "")
          as.data.frame(as.list(vals), stringsAsFactors = FALSE)
        }))
        names(tbl_df) <- all_cols
        # Remove Overall column only if stratified and user didn't request it
        if (!is.null(by_var) && !input$add_overall) {
          tbl_df <- tbl_df[, names(tbl_df) != "Overall", drop = FALSE]
        }
        # Remove p-value column if not requested
        if (!input$add_p || is.null(by_var)) {
          tbl_df <- tbl_df[, names(tbl_df) != "p-value", drop = FALSE]
        }

        table1_obj(tbl_df)
        shared$table1 <- tbl_df
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
      n_total <- if (!is.null(shared$data)) nrow(shared$data) else "?"
      title <- paste0("Table 1. Characteristics of participants (N = ", n_total, ")")
      rendered_html <- df_to_html_table(tbl, title = title, id = ns("table1_html"))
      # Store for reuse in Step 5
      shared$table1_html <- rendered_html
      HTML(rendered_html)
    })

    observeEvent(input$export_html, {
      tbl <- table1_obj()
      if (is.null(tbl)) {
        showNotification("Generate a table first", type = "warning")
        return()
      }
      # Use JS to copy the rendered table HTML to clipboard
      session$sendCustomMessage("copyTableToClipboard", ns("table1_html"))
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
          div(class = "alert alert-info", style = "font-size: 13px; padding: 8px 12px;",
            tags$strong("Missing data: "),
            "Complete case analysis (listwise deletion) is used. ",
            "Observations with missing values in the outcome or any predictor variable are excluded from the model."
          ),
          selectInput(ns("outcome"), "Outcome / event variable:", choices = NULL),
          selectInput(ns("model_type"), "Model type:", choices = c(
            "Linear regression" = "lm",
            "Logistic regression" = "glm",
            "Cox regression" = "cox",
            "Mixed model (Experimental)" = "lmer"
          )),
          checkboxGroupInput(ns("predictors"), "Predictor variables:", choices = NULL),
          uiOutput(ns("predictor_types_info")),
          hr(),
          conditionalPanel(
            condition = paste0("input['", ns("model_type"), "'] == 'cox'"),
            selectInput(ns("time_var"), "Time variable:", choices = NULL)
          ),
          conditionalPanel(
            condition = paste0("input['", ns("model_type"), "'] == 'lmer'"),
            selectInput(ns("random_var"), "Random effect (grouping):", choices = NULL),
            p(class = "text-muted small",
              "Automatically fits linear (continuous outcome) or logistic ",
              "(binary outcome) mixed model based on the outcome variable. ",
              "May be slow in WebR.")
          ),
          hr(),
          checkboxInput(ns("robust_se"), "Cluster-robust SEs (sandwich)", value = FALSE),
          conditionalPanel(
            condition = paste0("input['", ns("robust_se"), "']"),
            selectInput(ns("cluster_var"), "Cluster variable:", choices = NULL)
          ),
          checkboxInput(ns("show_vif"), "Show VIF (multicollinearity)", value = FALSE),
          hr(),
          actionButton(ns("fit_model"), "Fit Model", class = "btn-primary"),
          br(), br(),
          actionButton(ns("copy_results"), "Copy table to clipboard",
                       class = "btn-outline-secondary btn-sm")
        )
      ),
      column(8,
        tabsetPanel(
          tabPanel("Results",
            div(style = "margin-top: 10px;",
              uiOutput(ns("model_results"))
            )
          ),
          tabPanel("Plots",
            div(style = "margin-top: 10px;",
              uiOutput(ns("plot_controls")),
              hr(),
              uiOutput(ns("model_plot_ui")),
              div(style = "margin-top: 10px;",
                actionButton(ns("download_plot"), "Download Plot (PNG)",
                             class = "btn-outline-secondary btn-sm"),
                actionButton(ns("copy_plot"), "Copy Plot to Clipboard",
                             class = "btn-outline-secondary btn-sm")
              )
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
      updateSelectInput(session, "random_var", choices = vars)
      updateSelectInput(session, "cluster_var", choices = vars)
    })

    # Show detected types for selected predictors
    output$predictor_types_info <- renderUI({
      req(shared$data, input$predictors, shared$var_types)
      vt <- shared$var_types
      preds <- input$predictors
      if (length(preds) == 0) return(NULL)
      info <- sapply(preds, function(v) {
        detected <- if (v %in% vt$variable) vt$type[vt$variable == v] else "unknown"
        paste0(v, " [", detected, "]")
      })
      p(class = "text-muted small",
        strong("Detected types: "),
        paste(info, collapse = ", "),
        br(),
        "Categorical variables will be auto-converted to factors.")
    })

    model_fit <- reactiveVal(NULL)
    model_tidy <- reactiveVal(NULL)
    model_missing_info <- reactiveVal(NULL)
    mixed_model_binary <- reactiveVal(FALSE)
    mixed_model_icc <- reactiveVal(NULL)

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

      # Prepare data: convert detected categoricals to factor
      df <- shared$data
      if (!is.null(shared$var_types)) {
        df <- prepare_model_data(df, shared$var_types)
      }

      formula_str <- paste(outcome, "~", paste(preds, collapse = " + "))

      tryCatch({
        mod <- switch(input$model_type,
          "lm" = lm(as.formula(formula_str), data = df),
          "glm" = {
            # For logistic: ensure outcome is numeric 0/1
            if (is.factor(df[[outcome]])) {
              df[[outcome]] <- as.numeric(as.character(df[[outcome]]))
            }
            if (!is.numeric(df[[outcome]]) || !all(na.omit(df[[outcome]]) %in% c(0, 1))) {
              df[[outcome]] <- as.numeric(as.factor(df[[outcome]])) - 1
            }
            glm(as.formula(formula_str), data = df, family = binomial)
          },
          "cox" = {
            req(input$time_var)
            if (!requireNamespace("survival", quietly = TRUE)) stop("survival not available")
            # Ensure time and event (outcome) variables are numeric (not factor)
            if (is.factor(df[[input$time_var]])) {
              df[[input$time_var]] <- as.numeric(as.character(df[[input$time_var]]))
            }
            if (is.factor(df[[outcome]])) {
              df[[outcome]] <- as.numeric(as.character(df[[outcome]]))
            }
            surv_formula <- as.formula(paste(
              "survival::Surv(", input$time_var, ",", outcome, ") ~",
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
            # Auto-detect binary vs continuous outcome
            outcome_vals <- na.omit(df[[outcome]])
            is_binary <- is.logical(outcome_vals) ||
              (is.numeric(outcome_vals) && all(outcome_vals %in% c(0, 1))) ||
              (is.factor(outcome_vals) && length(levels(outcome_vals)) == 2) ||
              (is.character(outcome_vals) && length(unique(outcome_vals)) == 2)
            mixed_model_binary(is_binary)
            if (is_binary) {
              # Convert to numeric 0/1 if needed
              if (is.factor(df[[outcome]])) {
                df[[outcome]] <- as.numeric(df[[outcome]]) - 1
              } else if (is.character(df[[outcome]])) {
                df[[outcome]] <- as.numeric(as.factor(df[[outcome]])) - 1
              }
              lme4::glmer(mixed_formula, data = df, family = binomial)
            } else {
              lme4::lmer(mixed_formula, data = df)
            }
          }
        )

        model_fit(mod)

        # Tidy results
        if (input$model_type == "lmer") {
          # Manual extraction of fixed effects (broom.mixed not available in WebR)
          coef_summary <- summary(mod)$coefficients
          # glmer uses "z value", lmer uses "t value"
          stat_col <- if ("z value" %in% colnames(coef_summary)) "z value" else "t value"
          tidy_df <- data.frame(
            term = rownames(coef_summary),
            estimate = coef_summary[, "Estimate"],
            std.error = coef_summary[, "Std. Error"],
            statistic = coef_summary[, stat_col],
            stringsAsFactors = FALSE
          )
          # p-values: glmer provides them, lmer approximates via normal distribution
          if ("Pr(>|z|)" %in% colnames(coef_summary)) {
            tidy_df$p.value <- coef_summary[, "Pr(>|z|)"]
          } else {
            tidy_df$p.value <- 2 * pnorm(abs(tidy_df$statistic), lower.tail = FALSE)
          }
          # Confidence intervals
          tidy_df$conf.low <- tidy_df$estimate - 1.96 * tidy_df$std.error
          tidy_df$conf.high <- tidy_df$estimate + 1.96 * tidy_df$std.error
          rownames(tidy_df) <- NULL

          # Calculate ICC with 95% CI
          tryCatch({
            vc <- as.data.frame(lme4::VarCorr(mod))
            if (mixed_model_binary()) {
              # For binary: ICC = sigma2_u / (sigma2_u + pi^2/3)
              sigma2_u <- vc$vcov[1]
              icc_val <- sigma2_u / (sigma2_u + (pi^2 / 3))
            } else {
              # For continuous: ICC = sigma2_u / (sigma2_u + sigma2_e)
              sigma2_u <- vc$vcov[vc$grp != "Residual"][1]
              sigma2_e <- vc$vcov[vc$grp == "Residual"][1]
              icc_val <- sigma2_u / (sigma2_u + sigma2_e)
            }
            # Bootstrap-style approximate 95% CI using delta method
            n_groups <- length(unique(na.omit(df[[input$random_var]])))
            icc_se <- sqrt((2 * (1 - icc_val)^2 * (1 + (n_groups - 1) * icc_val)^2) /
                           (n_groups * (n_groups - 1)))
            icc_low <- max(0, icc_val - 1.96 * icc_se)
            icc_high <- min(1, icc_val + 1.96 * icc_se)
            mixed_model_icc(list(
              icc = round(icc_val, 3),
              ci_low = round(icc_low, 3),
              ci_high = round(icc_high, 3),
              group_var = input$random_var,
              n_groups = n_groups
            ))
          }, error = function(e) {
            mixed_model_icc(NULL)
          })
        } else {
          tidy_df <- broom::tidy(mod, conf.int = TRUE)
          mixed_model_icc(NULL)
        }

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

        # Exponentiate for logistic/Cox/binary mixed model
        is_exp <- input$model_type %in% c("glm", "cox") ||
                  (input$model_type == "lmer" && mixed_model_binary())
        if (is_exp) {
          tidy_df$OR_HR <- round(exp(tidy_df$estimate), 3)
          tidy_df$ci_lower <- round(exp(tidy_df$conf.low), 3)
          tidy_df$ci_upper <- round(exp(tidy_df$conf.high), 3)
        }

        model_tidy(tidy_df)
        shared$model_result <- list(fit = mod, tidy = tidy_df, type = input$model_type,
                                    is_binary_mixed = mixed_model_binary())

        # Calculate observations dropped due to missing data
        n_total <- nrow(df)
        n_used <- nobs(mod)
        n_dropped <- n_total - n_used
        model_missing_info(list(n_total = n_total, n_used = n_used, n_dropped = n_dropped))

        if (n_dropped > 0) {
          showNotification(
            paste0("Model fitted. ", n_dropped, " of ", n_total,
                   " observations (", round(100 * n_dropped / n_total, 1),
                   "%) excluded due to missing data."),
            type = "warning", duration = 10)
        } else {
          showNotification("Model fitted successfully. No observations excluded.", type = "message")
        }

      }, error = function(e) {
        showNotification(paste("Model error:", conditionMessage(e)), type = "error")
      })
    })

    output$model_results <- renderUI({
      tidy_df <- model_tidy()
      if (is.null(tidy_df)) {
        return(p(class = "text-muted", "Configure model and click 'Fit Model'"))
      }

      # Missing data summary banner
      miss_info <- model_missing_info()
      missing_banner <- NULL
      if (!is.null(miss_info)) {
        if (miss_info$n_dropped > 0) {
          missing_banner <- div(class = "alert alert-warning", style = "font-size: 13px;",
            tags$strong("Missing data: "),
            paste0(miss_info$n_dropped, " of ", miss_info$n_total, " observations (",
                   round(100 * miss_info$n_dropped / miss_info$n_total, 1),
                   "%) were excluded due to missing values (complete case analysis). ",
                   "Model fitted on ", miss_info$n_used, " complete observations."))
        } else {
          missing_banner <- div(class = "alert alert-success", style = "font-size: 13px;",
            tags$strong("Missing data: "),
            paste0("No observations excluded. Model fitted on all ", miss_info$n_total, " observations."))
        }
      }

      # Get analytical sample size for table title
      n_analytical <- if (!is.null(miss_info)) miss_info$n_used else "?"

      # ICC banner for mixed models
      icc_banner <- NULL
      icc_info <- mixed_model_icc()
      if (!is.null(icc_info) && input$model_type == "lmer") {
        model_type_label <- if (mixed_model_binary()) "Binary" else "Linear"
        icc_banner <- div(class = "alert alert-info", style = "font-size: 13px;",
          tags$strong(paste0(model_type_label, " mixed model — ")),
          tags$strong("ICC: "), paste0(icc_info$icc,
            " (95% CI: ", icc_info$ci_low, " to ", icc_info$ci_high, ")"),
          br(),
          tags$span(class = "text-muted small",
            paste0("Grouping variable: ", icc_info$group_var,
                   " (", icc_info$n_groups, " groups). ",
                   "ICC represents the proportion of total variance attributable to between-group differences.")))
      }

      # Format table — unname all numeric columns to avoid c(name=value) display
      display_df <- tidy_df
      display_df$term <- as.character(display_df$term)
      display_df$estimate <- unname(round(as.numeric(display_df$estimate), 2))
      display_df$std.error <- unname(round(as.numeric(display_df$std.error), 2))
      display_df$statistic <- unname(round(as.numeric(display_df$statistic), 2))
      display_df$p.value <- ifelse(as.numeric(display_df$p.value) < 0.001, "<0.001",
                                   as.character(round(as.numeric(display_df$p.value), 3)))
      if ("OR_HR" %in% names(display_df))
        display_df$OR_HR <- unname(round(as.numeric(display_df$OR_HR), 2))
      if ("ci_lower" %in% names(display_df))
        display_df$ci_lower <- unname(round(as.numeric(display_df$ci_lower), 2))
      if ("ci_upper" %in% names(display_df))
        display_df$ci_upper <- unname(round(as.numeric(display_df$ci_upper), 2))
      if ("conf.low" %in% names(display_df))
        display_df$conf.low <- unname(round(as.numeric(display_df$conf.low), 2))
      if ("conf.high" %in% names(display_df))
        display_df$conf.high <- unname(round(as.numeric(display_df$conf.high), 2))

      # For exponentiated models (glm/cox/binary lmer), use ci_lower/ci_upper (OR/HR scale)
      # and DROP the raw log-odds conf.low/conf.high to avoid duplicate CI columns
      is_exp <- input$model_type %in% c("glm", "cox") ||
                (input$model_type == "lmer" && mixed_model_binary())
      if (is_exp && "ci_lower" %in% names(display_df)) {
        display_df$conf.low <- NULL
        display_df$conf.high <- NULL
      }

      # Rename columns for publication
      if ("ci_lower" %in% names(display_df)) {
        names(display_df)[names(display_df) == "ci_lower"] <- "CI Lower"
        names(display_df)[names(display_df) == "ci_upper"] <- "CI Upper"
      } else if ("conf.low" %in% names(display_df)) {
        names(display_df)[names(display_df) == "conf.low"] <- "CI Lower"
        names(display_df)[names(display_df) == "conf.high"] <- "CI Upper"
      }
      if ("OR_HR" %in% names(display_df)) {
        or_label <- if (input$model_type == "cox") "HR" else "OR"
        names(display_df)[names(display_df) == "OR_HR"] <- or_label
      }
      names(display_df)[names(display_df) == "term"] <- "Term"
      names(display_df)[names(display_df) == "estimate"] <- "Estimate"
      names(display_df)[names(display_df) == "std.error"] <- "Std. Error"
      names(display_df)[names(display_df) == "statistic"] <- "Statistic"
      names(display_df)[names(display_df) == "p.value"] <- "P-value"

      # Reorder columns: Term, Estimate immediately before CI, P-value after CI
      if (is_exp) {
        or_label <- if (input$model_type == "cox") "HR" else "OR"
        desired_order <- c("Term", "Estimate", or_label,
                           "CI Lower", "CI Upper", "P-value", "Std. Error", "Statistic")
      } else {
        desired_order <- c("Term", "Estimate",
                           "CI Lower", "CI Upper", "P-value", "Std. Error", "Statistic")
      }
      # Keep only columns that exist, in the desired order
      desired_order <- desired_order[desired_order %in% names(display_df)]
      # Add any remaining columns not in the desired order
      remaining <- setdiff(names(display_df), desired_order)
      display_df <- display_df[, c(desired_order, remaining), drop = FALSE]

      # Build Table 2 title naming the estimate
      estimate_desc <- switch(input$model_type,
        "lm" = "Linear regression coefficients",
        "glm" = "Odds ratios from logistic regression",
        "cox" = "Hazard ratios from Cox proportional hazards regression",
        "lmer" = if (mixed_model_binary()) "Odds ratios from mixed-effects logistic regression"
                 else "Mixed-effects linear regression coefficients"
      )
      table2_title <- paste0("Table 2. ", estimate_desc, " (N = ", n_analytical, ")")

      # AIC / BIC model fit statistics
      fit_stats_banner <- NULL
      mod <- model_fit()
      if (!is.null(mod)) {
        fit_parts <- tryCatch({
          parts <- list()
          aic_val <- tryCatch(AIC(mod), error = function(e) NULL)
          bic_val <- tryCatch(BIC(mod), error = function(e) NULL)
          if (!is.null(aic_val) && is.finite(aic_val)) parts$AIC <- round(aic_val, 1)
          if (!is.null(bic_val) && is.finite(bic_val)) parts$BIC <- round(bic_val, 1)
          parts
        }, error = function(e) list())
        if (length(fit_parts) > 0) {
          stats_text <- paste(names(fit_parts), "=", unlist(fit_parts), collapse = "    ")
          fit_stats_banner <- div(style = "font-size: 13px; color: #555; margin-top: 8px; margin-bottom: 8px;",
            tags$strong("Model fit: "), stats_text)
        }
      }

      rendered_html <- df_to_html_table(display_df, title = table2_title,
                                          id = ns("model_results_html"))
      # Store for reuse in Step 5
      shared$model_result_html <- rendered_html
      table_html <- HTML(rendered_html)
      tagList(missing_banner, icc_banner, table_html, fit_stats_banner)
    })

    # Copy regression results to clipboard
    observeEvent(input$copy_results, {
      if (is.null(model_tidy())) {
        showNotification("Fit a model first", type = "warning")
        return()
      }
      session$sendCustomMessage("copyTableToClipboard", ns("model_results_html"))
    })

    output$diagnostics_output <- renderUI({
      mod <- model_fit()
      if (is.null(mod)) return(p(class = "text-muted", "Fit a model first"))

      diag_parts <- list()

      # VIF
      if (input$show_vif && input$model_type %in% c("lm", "glm")) {
        if (requireNamespace("car", quietly = TRUE)) {
          vif_html <- tryCatch({
            vif_vals <- car::vif(mod)
            if (is.matrix(vif_vals)) vif_vals <- vif_vals[, "GVIF"]
            vif_df <- data.frame(
              Variable = names(vif_vals),
              VIF = round(vif_vals, 3),
              Flag = ifelse(vif_vals > 10, "HIGH", ifelse(vif_vals > 5, "Moderate", "OK")),
              stringsAsFactors = FALSE
            )
            tbl_html <- df_to_html_table(vif_df)
            paste0("<h5>Variance Inflation Factors (VIF)</h5>",
              "<p class='text-muted small'>VIF > 5: moderate concern. VIF > 10: serious multicollinearity.</p>",
              tbl_html)
          }, error = function(e) {
            paste0("<p>VIF error: ", conditionMessage(e), "</p>")
          })
          diag_parts <- c(diag_parts, list(vif_html))
        }
      }

      # Estimated marginal means (lm/glm only)
      if (input$model_type %in% c("lm", "glm")) {
        if (requireNamespace("emmeans", quietly = TRUE)) {
          emm_html <- tryCatch({
            preds <- input$predictors
            factor_preds <- preds[sapply(shared$data[preds], function(x) {
              is.factor(x) || is.character(x) || (is.numeric(x) && length(unique(na.omit(x))) <= 10)
            })]
            if (length(factor_preds) > 0) {
              emm <- emmeans::emmeans(mod, factor_preds[1])
              emm_df <- as.data.frame(summary(emm))
              emm_df[] <- lapply(emm_df, function(x) if (is.numeric(x)) round(x, 3) else x)
              tbl_html <- df_to_html_table(emm_df)
              paste0("<h5>Estimated Marginal Means: ", factor_preds[1], "</h5>", tbl_html)
            } else NULL
          }, error = function(e) NULL)
          if (!is.null(emm_html)) diag_parts <- c(diag_parts, list(emm_html))
        }
      }

      # Cox regression diagnostics
      if (input$model_type == "cox") {
        cox_html <- tryCatch({
          parts <- "<h5>Cox Model Diagnostics</h5>"

          # Concordance
          concordance_val <- tryCatch({
            gl <- summary(mod)
            if (!is.null(gl$concordance)) {
              c_val <- gl$concordance["C"]
              if (!is.na(c_val)) round(c_val, 3) else NULL
            } else NULL
          }, error = function(e) NULL)

          if (!is.null(concordance_val)) {
            parts <- paste0(parts,
              "<p><strong>Concordance (C-statistic):</strong> ", concordance_val,
              " <span class='text-muted small'>(0.5 = no discrimination, 1.0 = perfect)</span></p>")
          }

          # Proportional hazards test (Schoenfeld residuals)
          ph_test <- survival::cox.zph(mod)
          ph_df <- as.data.frame(ph_test$table)
          ph_df$Variable <- rownames(ph_df)
          ph_df <- ph_df[, c("Variable", "chisq", "df", "p"), drop = FALSE]
          names(ph_df) <- c("Variable", "Chi-sq", "df", "p-value")
          ph_df[["Chi-sq"]] <- round(ph_df[["Chi-sq"]], 3)
          ph_df[["p-value"]] <- ifelse(ph_df[["p-value"]] < 0.001, "<0.001",
                                       round(ph_df[["p-value"]], 4))

          tbl_html <- df_to_html_table(ph_df)
          paste0(parts,
            "<h5>Test of Proportional Hazards Assumption</h5>",
            "<p class='text-muted small'>Schoenfeld residuals test. ",
            "A significant p-value (p < 0.05) suggests the proportional hazards assumption may be violated.</p>",
            tbl_html)
        }, error = function(e) {
          paste0("<p>Cox diagnostics error: ", conditionMessage(e), "</p>")
        })
        diag_parts <- c(diag_parts, list(cox_html))
      }

      if (length(diag_parts) == 0) {
        return(HTML("<p class='text-muted'>No diagnostics to show. Enable VIF or fit a model with factor predictors for marginal means.</p>"))
      }
      HTML(paste(diag_parts, collapse = ""))
    })

    # Diagnostics forest plot as a reactive (so we can reuse it for export)
    diagnostics_forest_plot <- reactive({
      tidy_df <- model_tidy()
      if (is.null(tidy_df)) return(NULL)

      plot_df <- tidy_df[tidy_df$term != "(Intercept)", ]
      if (nrow(plot_df) == 0) return(NULL)

      if ("OR_HR" %in% names(plot_df)) {
        plot_df$est <- plot_df$OR_HR
        plot_df$lo <- plot_df$ci_lower
        plot_df$hi <- plot_df$ci_upper
        x_label <- if (input$model_type %in% c("glm", "lmer")) "Odds Ratio" else "Hazard Ratio"
        ref_line <- 1
      } else {
        plot_df$est <- plot_df$estimate
        plot_df$lo <- plot_df$conf.low
        plot_df$hi <- plot_df$conf.high
        x_label <- "Coefficient"
        ref_line <- 0
      }

      plot_df$term <- factor(plot_df$term, levels = rev(plot_df$term))

      miss_info <- model_missing_info()
      n_diag <- if (!is.null(miss_info)) miss_info$n_used else "?"

      ggplot2::ggplot(plot_df, ggplot2::aes(x = est, y = term)) +
        ggplot2::geom_point(size = 3, color = "#4e79a7") +
        ggplot2::geom_errorbarh(ggplot2::aes(xmin = lo, xmax = hi), height = 0.2, color = "#4e79a7") +
        ggplot2::geom_vline(xintercept = ref_line, linetype = "dashed", color = "grey50") +
        ggplot2::labs(title = paste0("Forest Plot — All Predictors (N = ", n_diag, ")"),
                      x = x_label, y = "") +
        ggplot2::theme_minimal(base_size = 14)
    })

    output$forest_plot <- renderPlot({
      diagnostics_forest_plot()
    })

    # Store diagnostics forest plot info for reports
    observe({
      mod <- model_fit()
      tidy_df <- model_tidy()
      if (is.null(mod) || is.null(tidy_df)) {
        shared$diagnostics_plot <- NULL
        shared$diagnostics_plot_base64 <- NULL
        return()
      }

      plot_df <- tidy_df[tidy_df$term != "(Intercept)", ]
      if (nrow(plot_df) == 0) {
        shared$diagnostics_plot <- NULL
        shared$diagnostics_plot_base64 <- NULL
        return()
      }

      # Build description
      mtype <- input$model_type
      if (mtype %in% c("glm", "cox")) {
        label <- if (mtype == "glm") "Odds Ratios" else "Hazard Ratios"
        desc_items <- sapply(seq_len(nrow(plot_df)), function(i) {
          r <- plot_df[i, ]
          paste0(r$term, ": ", round(r$OR_HR, 2),
                 " [", round(r$ci_lower, 2), "-", round(r$ci_upper, 2), "]",
                 ", p=", if (r$p.value < 0.001) "<0.001" else round(r$p.value, 3))
        })
        shared$diagnostics_plot <- list(
          type = "forest_plot_diagnostics",
          title = paste("Forest Plot:", label, "(All Predictors)"),
          description = paste0("Forest plot showing ", label,
            " with 95% CIs for all model predictors."),
          details = paste(desc_items, collapse = "; ")
        )
      } else {
        # Linear/mixed: coefficient plot
        desc_items <- sapply(seq_len(nrow(plot_df)), function(i) {
          r <- plot_df[i, ]
          paste0(r$term, ": ", round(r$estimate, 3),
                 " [", round(r$conf.low, 3), "-", round(r$conf.high, 3), "]",
                 ", p=", if (r$p.value < 0.001) "<0.001" else round(r$p.value, 3))
        })
        shared$diagnostics_plot <- list(
          type = "coefficient_plot",
          title = "Coefficient Plot (All Predictors)",
          description = "Coefficient plot showing regression coefficients with 95% CIs for all model predictors.",
          details = paste(desc_items, collapse = "; ")
        )
      }

      # Generate base64 image
      tryCatch({
        p <- diagnostics_forest_plot()
        if (!is.null(p)) {
          tmp <- tempfile(fileext = ".png")
          grDevices::png(tmp, width = 800, height = 500, res = 120)
          print(p)
          grDevices::dev.off()
          raw <- readBin(tmp, "raw", file.info(tmp)$size)
          unlink(tmp)
          shared$diagnostics_plot_base64 <- paste0("data:image/png;base64,", base64enc::base64encode(raw))
        }
      }, error = function(e) {
        shared$diagnostics_plot_base64 <- NULL
      })
    })

    # --- Plots tab: model-appropriate visualizations ---

    # Dynamic controls based on model type
    output$plot_controls <- renderUI({
      mod <- model_fit()
      if (is.null(mod)) {
        return(p(class = "text-muted", "Fit a model first (Results tab), then select variables to plot."))
      }
      mtype <- input$model_type
      preds <- input$predictors
      outcome <- input$outcome
      if (length(preds) == 0) return(NULL)

      tidy_df <- model_tidy()
      if (is.null(tidy_df)) return(NULL)
      terms_no_int <- tidy_df$term[tidy_df$term != "(Intercept)"]

      # Forest plot term selection (all model types)
      forest_section <- tagList(
        p(strong("Forest Plot"), "— select terms to display:"),
        checkboxGroupInput(ns("plot_terms"), NULL,
          choices = terms_no_int, selected = terms_no_int)
      )

      if (mtype == "cox") {
        km_choices <- intersect(preds, names(shared$data))
        tagList(
          forest_section,
          hr(),
          p(strong("Kaplan-Meier Survival Curve")),
          selectInput(ns("km_strata"), "Stratify survival curve by:",
            choices = km_choices, selected = km_choices[1])
        )
      } else if (mtype == "glm" || (mtype == "lmer" && mixed_model_binary())) {
        or_label <- if (mtype == "glm") "Odds Ratios" else "Odds Ratios (mixed-effects)"
        tagList(
          p(tags$em(paste0("Showing: ", or_label))),
          forest_section
        )
      } else {
        # lm / continuous lmer
        tagList(
          forest_section,
          hr(),
          selectInput(ns("plot_type_lm"), "Additional plots:",
            choices = c("Forest Plot only" = "forest",
                        "Also show Exposure vs Outcome" = "scatter"),
            selected = "forest"),
          conditionalPanel(
            condition = paste0("input['", ns("plot_type_lm"), "'] == 'scatter'"),
            p(strong("Exposure vs Outcome"), "— select predictors:"),
            checkboxGroupInput(ns("plot_vars"), NULL,
              choices = preds, selected = preds),
            p(class = "text-muted small",
              "Numeric: scatter + regression line. Categorical: box plot.")
          )
        )
      }
    })

    # Track number of plots for dynamic height
    n_plots <- reactiveVal(1)

    # The reactive that builds the plot object
    current_plot <- reactive({
      mod <- model_fit()
      if (is.null(mod)) return(NULL)
      mtype <- input$model_type

      # Get analytical sample size for titles
      miss_info <- model_missing_info()
      n_analytical <- if (!is.null(miss_info)) miss_info$n_used else nobs(mod)

      tidy_df <- model_tidy()
      if (is.null(tidy_df)) return(NULL)

      # Build plot list — always start with forest plot
      plot_list <- list()

      # --- Forest plot (all model types) ---
      # Use user-selected terms if available, otherwise all non-intercept terms
      all_terms <- tidy_df$term[tidy_df$term != "(Intercept)"]

      is_exp <- mtype %in% c("glm", "cox") || (mtype == "lmer" && mixed_model_binary())
      if (is_exp) {
        # Forest plot with OR/HR (exponentiated)
        sel <- if (!is.null(input$plot_terms) && length(input$plot_terms) > 0) {
          input$plot_terms
        } else {
          all_terms
        }
        plot_df <- tidy_df[tidy_df$term %in% sel, , drop = FALSE]
        if (nrow(plot_df) > 0) {
          plot_df$est <- as.numeric(plot_df$OR_HR)
          plot_df$lo <- as.numeric(plot_df$ci_lower)
          plot_df$hi <- as.numeric(plot_df$ci_upper)
          est_label <- if (mtype == "cox") "Hazard Ratios" else "Odds Ratios"
          x_label <- paste0(est_label, " (95% CI)")
          plot_df$term <- factor(plot_df$term, levels = rev(plot_df$term))
          plot_df$label <- paste0(round(plot_df$est, 2), " [",
                                  round(plot_df$lo, 2), "-", round(plot_df$hi, 2), "]")
          forest_p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = est, y = term)) +
            ggplot2::geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
            ggplot2::geom_point(size = 3.5, color = "#4e79a7") +
            ggplot2::geom_errorbarh(ggplot2::aes(xmin = lo, xmax = hi),
                                    height = 0.25, color = "#4e79a7", linewidth = 0.8) +
            ggplot2::geom_text(ggplot2::aes(label = label), hjust = -0.15, size = 3.2) +
            ggplot2::labs(
              title = paste0("Forest Plot: ", est_label, " (N = ", n_analytical, ")"),
              x = x_label, y = "") +
            ggplot2::theme_minimal(base_size = 13) +
            ggplot2::theme(
              panel.grid.major.y = ggplot2::element_blank(),
              plot.title = ggplot2::element_text(face = "bold"))
          plot_list <- c(plot_list, list(forest_p))
        }
      } else {
        # Forest plot with coefficients (lm, continuous lmer)
        sel <- if (!is.null(input$plot_terms) && length(input$plot_terms) > 0) {
          input$plot_terms
        } else {
          all_terms
        }
        plot_df <- tidy_df[tidy_df$term %in% sel, , drop = FALSE]
        if (nrow(plot_df) > 0) {
          plot_df$est <- unname(as.numeric(plot_df$estimate))
          plot_df$lo <- unname(as.numeric(plot_df$conf.low))
          plot_df$hi <- unname(as.numeric(plot_df$conf.high))
          plot_df$term <- factor(plot_df$term, levels = rev(plot_df$term))
          plot_df$label <- paste0(round(plot_df$est, 2), " [",
                                  round(plot_df$lo, 2), ", ", round(plot_df$hi, 2), "]")
          forest_p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = est, y = term)) +
            ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
            ggplot2::geom_point(size = 3.5, color = "#4e79a7") +
            ggplot2::geom_errorbarh(ggplot2::aes(xmin = lo, xmax = hi),
                                    height = 0.25, color = "#4e79a7", linewidth = 0.8) +
            ggplot2::geom_text(ggplot2::aes(label = label), hjust = -0.15, size = 3.2) +
            ggplot2::labs(
              title = paste0("Forest Plot: Coefficients (N = ", n_analytical, ")"),
              x = "Coefficient (95% CI)", y = "") +
            ggplot2::theme_minimal(base_size = 13) +
            ggplot2::theme(
              panel.grid.major.y = ggplot2::element_blank(),
              plot.title = ggplot2::element_text(face = "bold"))
          plot_list <- c(plot_list, list(forest_p))
        }
      }

      # --- KM survival curve (Cox only — always shown) ---
      if (mtype == "cox") {
        if (requireNamespace("survival", quietly = TRUE)) {
          km_var <- input$km_strata
          if (!is.null(km_var) && km_var %in% names(shared$data)) {
            km_p <- tryCatch({
              df <- shared$data
              if (!is.null(shared$var_types)) df <- prepare_model_data(df, shared$var_types)
              time_var <- input$time_var
              outcome <- input$outcome
              if (is.factor(df[[time_var]])) df[[time_var]] <- as.numeric(as.character(df[[time_var]]))
              if (is.factor(df[[outcome]])) df[[outcome]] <- as.numeric(as.character(df[[outcome]]))
              km_formula <- as.formula(paste0("survival::Surv(", time_var, ", ", outcome, ") ~ ", km_var))
              km_fit <- survival::survfit(km_formula, data = df)
              km_data <- data.frame(
                time = km_fit$time, surv = km_fit$surv,
                upper = km_fit$upper, lower = km_fit$lower,
                strata = rep(names(km_fit$strata), km_fit$strata)
              )
              km_data$strata <- gsub(paste0("^", km_var, "="), "", km_data$strata)
              ggplot2::ggplot(km_data, ggplot2::aes(x = time, y = surv, color = strata)) +
                ggplot2::geom_step(linewidth = 0.9) +
                ggplot2::geom_step(ggplot2::aes(y = lower), linetype = "dashed", alpha = 0.4, linewidth = 0.4) +
                ggplot2::geom_step(ggplot2::aes(y = upper), linetype = "dashed", alpha = 0.4, linewidth = 0.4) +
                ggplot2::labs(
                  title = paste0("Kaplan-Meier Survival Curve by ", km_var, " (N = ", n_analytical, ")"),
                  x = "Time", y = "Survival Probability",
                  color = km_var, fill = km_var) +
                ggplot2::scale_y_continuous(limits = c(0, 1)) +
                ggplot2::theme_minimal(base_size = 13) +
                ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"), legend.position = "bottom")
            }, error = function(e) NULL)
            if (!is.null(km_p)) plot_list <- c(plot_list, list(km_p))
          }
        }
      }

      # --- Exposure vs outcome plots (lm/lmer scatter option) ---
      if (mtype %in% c("lm", "lmer") && !is.null(input$plot_type_lm) && input$plot_type_lm == "scatter") {
        sel <- input$plot_vars
        if (!is.null(sel) && length(sel) > 0) {
          outcome <- input$outcome
          df <- shared$data
          if (!is.null(df) && !is.null(shared$var_types)) {
            df <- prepare_model_data(df, shared$var_types)
          }
          if (!is.null(df)) {
            scatter_plots <- lapply(sel, function(v) {
              vt <- shared$var_types
              is_cat <- (!is.null(vt) && v %in% vt$variable &&
                         vt$type[vt$variable == v] == "categorical") ||
                        is.factor(df[[v]]) || is.character(df[[v]])
              if (is_cat) {
                ggplot2::ggplot(df, ggplot2::aes(x = factor(.data[[v]]), y = .data[[outcome]])) +
                  ggplot2::geom_boxplot(fill = "#4e79a7", alpha = 0.5, outlier.size = 1) +
                  ggplot2::stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "#e15759") +
                  ggplot2::labs(title = paste0(outcome, " by ", v), x = v, y = outcome) +
                  ggplot2::theme_minimal(base_size = 12) +
                  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 11))
              } else {
                ggplot2::ggplot(df, ggplot2::aes(x = .data[[v]], y = .data[[outcome]])) +
                  ggplot2::geom_point(alpha = 0.4, color = "#4e79a7", size = 1.5) +
                  ggplot2::geom_smooth(method = "lm", se = TRUE, color = "#e15759", fill = "#e15759", alpha = 0.15) +
                  ggplot2::labs(title = paste0(outcome, " vs ", v), x = v, y = outcome) +
                  ggplot2::theme_minimal(base_size = 12) +
                  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 11))
              }
            })
            plot_list <- c(plot_list, scatter_plots)
          }
        }
      }

      # Return: if no plots, return NULL
      if (length(plot_list) == 0) return(NULL)

      # Set plot count for dynamic height
      n_plots(length(plot_list))

      # Arrange all plots vertically tiled (one per row)
      if (length(plot_list) == 1) {
        plot_list[[1]]
      } else {
        tryCatch({
          install_if_needed("gridExtra")
          do.call(gridExtra::grid.arrange,
                  c(plot_list, ncol = 1))
        }, error = function(e) plot_list[[1]])
      }
    })

    # Dynamic plot height — one plot = 500px, stacked vertically 500px each
    output$model_plot_ui <- renderUI({
      # Trigger on current_plot so we re-render when plot changes
      p <- tryCatch(current_plot(), error = function(e) NULL)
      if (is.null(p)) return(plotOutput(ns("model_plot"), height = "500px"))
      plot_height <- paste0(n_plots() * 500, "px")
      plotOutput(ns("model_plot"), height = plot_height)
    })

    output$model_plot <- renderPlot({
      p <- tryCatch(current_plot(), error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("Plot error:", conditionMessage(e)),
             cex = 1.1, col = "red")
        NULL
      })
      if (is.null(p)) return()
      if (inherits(p, "ggplot")) {
        p
      } else {
        # gridExtra gtable — must explicitly draw
        grid::grid.newpage()
        grid::grid.draw(p)
      }
    })

    # Store plot info in shared for reports/LLM
    observe({
      mod <- model_fit()
      if (is.null(mod)) {
        shared$plots <- NULL
        return()
      }
      mtype <- input$model_type

      # Build descriptions of what was plotted
      plot_descriptions <- list()

      # Forest plot description (all model types)
      is_exp <- mtype %in% c("glm", "cox") || (mtype == "lmer" && mixed_model_binary())
      sel <- input$plot_terms
      tidy_df <- model_tidy()
      # Fallback: use all non-intercept terms if checkboxes not yet available
      if ((is.null(sel) || length(sel) == 0) && !is.null(tidy_df)) {
        sel <- tidy_df$term[tidy_df$term != "(Intercept)"]
      }
      if (!is.null(sel) && length(sel) > 0 && !is.null(tidy_df)) {
        sub_df <- tidy_df[tidy_df$term %in% sel, , drop = FALSE]
        if (nrow(sub_df) > 0) {
          if (is_exp) {
            label <- if (mtype == "cox") "Hazard Ratios" else "Odds Ratios"
            desc_items <- sapply(seq_len(nrow(sub_df)), function(i) {
              r <- sub_df[i, ]
              paste0(r$term, ": ", round(as.numeric(r$OR_HR), 2),
                     " [", round(as.numeric(r$ci_lower), 2), "-",
                     round(as.numeric(r$ci_upper), 2), "]",
                     ", p=", if (as.numeric(r$p.value) < 0.001) "<0.001"
                            else round(as.numeric(r$p.value), 3))
            })
          } else {
            label <- "Coefficients"
            desc_items <- sapply(seq_len(nrow(sub_df)), function(i) {
              r <- sub_df[i, ]
              paste0(r$term, ": ", round(as.numeric(r$estimate), 2),
                     " [", round(as.numeric(r$conf.low), 2), ", ",
                     round(as.numeric(r$conf.high), 2), "]",
                     ", p=", if (as.numeric(r$p.value) < 0.001) "<0.001"
                            else round(as.numeric(r$p.value), 3))
            })
          }
          plot_descriptions <- list(list(
            type = "forest_plot",
            title = paste("Forest Plot:", label),
            description = paste0("Forest plot showing ", label,
              " with 95% CIs for: ", paste(sel, collapse = ", "), "."),
            details = paste(desc_items, collapse = "; ")
          ))
        }
      }

      # Additional exposure vs outcome plots (lm/lmer scatter)
      if (mtype %in% c("lm", "lmer") && !is.null(input$plot_type_lm) && input$plot_type_lm == "scatter") {
        sel_vars <- input$plot_vars
        outcome <- input$outcome
        if (!is.null(sel_vars) && length(sel_vars) > 0 && !is.null(outcome)) {
          vt <- shared$var_types
          if (length(sel_vars) > 1) {
            plot_descriptions <- c(plot_descriptions, list(list(
              type = "faceted_plot",
              title = paste("Outcome by Exposures:", paste(sel_vars, collapse = ", ")),
              description = paste0("Faceted plot showing ", outcome, " by ",
                length(sel_vars), " predictor variables: ", paste(sel_vars, collapse = ", "),
                ". Scatter plots for continuous; box plots for categorical.")
            )))
          } else {
            v <- sel_vars[1]
            is_cat <- (!is.null(vt) && v %in% vt$variable &&
                       vt$type[vt$variable == v] == "categorical")
            ptype <- if (is_cat) "box_plot" else "scatter_plot"
            pdesc <- if (is_cat) {
              paste0("Box plot of ", outcome, " by levels of ", v, " (categorical).")
            } else {
              paste0("Scatter plot of ", outcome, " vs ", v, " with regression line.")
            }
            plot_descriptions <- c(plot_descriptions, list(list(
              type = ptype, title = paste(outcome, if (is_cat) "by" else "vs", v),
              description = pdesc
            )))
          }
        }
      }

      shared$plots <- plot_descriptions
    })

    # Generate base64 PNG of the current plot for embedding in reports
    get_plot_base64 <- function() {
      p <- current_plot()
      if (is.null(p)) return(NULL)
      tryCatch({
        # Scale height based on number of plots
        img_height <- n_plots() * 500
        tmp <- tempfile(fileext = ".png")
        grDevices::png(tmp, width = 800, height = img_height, res = 120)
        if (inherits(p, "ggplot")) {
          print(p)
        } else {
          # gridExtra output — must use grid.newpage + grid.draw
          grid::grid.newpage()
          grid::grid.draw(p)
        }
        grDevices::dev.off()
        raw <- readBin(tmp, "raw", file.info(tmp)$size)
        unlink(tmp)
        paste0("data:image/png;base64,", base64enc::base64encode(raw))
      }, error = function(e) {
        tryCatch(grDevices::dev.off(), error = function(e2) NULL)
        NULL
      })
    }

    # Download plot as PNG
    observeEvent(input$download_plot, {
      p <- current_plot()
      if (is.null(p)) {
        showNotification("No plot to download. Select variables and fit a model first.", type = "warning")
        return()
      }
      b64 <- get_plot_base64()
      if (is.null(b64)) {
        showNotification("Could not generate plot image.", type = "error")
        return()
      }
      session$sendCustomMessage("downloadPlotPNG", list(data = b64, filename = "model_plot.png"))
    })

    # Copy plot to clipboard
    observeEvent(input$copy_plot, {
      p <- current_plot()
      if (is.null(p)) {
        showNotification("No plot to copy. Select variables and fit a model first.", type = "warning")
        return()
      }
      b64 <- get_plot_base64()
      if (is.null(b64)) {
        showNotification("Could not generate plot image.", type = "error")
        return()
      }
      session$sendCustomMessage("copyPlotToClipboard", list(data = b64))
    })

    # Store base64 for report use — re-generate whenever current_plot changes
    observe({
      p <- current_plot()
      if (is.null(p)) {
        shared$plot_base64 <- NULL
        return()
      }
      # Defer slightly so renderPlot has finished
      invalidateLater(500)
      shared$plot_base64 <- tryCatch(get_plot_base64(), error = function(e) NULL)
    })
  })
}



# =============================================================================
# MODULE: Step 5 — Results & Export
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
              tabPanel("Plots",
                uiOutput(ns("show_plots"))
              ),
              tabPanel("Export",
                h5("Download Report"),
                p("Download a report containing all your results, including a statistical methods summary."),
                div(style = "display: flex; gap: 10px; flex-wrap: wrap;",
                  actionButton(ns("download_pdf"), "Download as PDF",
                               class = "btn-primary"),
                  actionButton(ns("download_txt"), "Download as Text",
                               class = "btn-outline-primary"),
                  actionButton(ns("download_word"), "Download as Word",
                               class = "btn-outline-primary")
                )
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
      # Show the exact same table HTML that was generated in Step 3
      html <- shared$table1_html
      if (!is.null(html) && nchar(html) > 0) return(HTML(html))
      # Fallback: render from data frame
      tbl <- shared$table1
      if (is.null(tbl)) return(p(class = "text-muted", "No Table 1 generated yet. Go to Step 3."))
      if (is.data.frame(tbl)) {
        n_total <- if (!is.null(shared$data)) nrow(shared$data) else "?"
        HTML(df_to_html_table(tbl, title = paste0("Table 1. Characteristics of participants (N = ", n_total, ")")))
      } else {
        HTML(paste("<pre>", paste(capture.output(print(tbl)), collapse = "\n"), "</pre>"))
      }
    })

    output$show_plots <- renderUI({
      b64 <- shared$plot_base64
      plots_desc <- shared$plots
      diag_b64 <- shared$diagnostics_plot_base64
      diag_desc <- shared$diagnostics_plot

      has_plots <- (!is.null(b64) && nchar(b64) > 0) ||
                   (!is.null(diag_b64) && nchar(diag_b64) > 0)

      if (!has_plots) {
        return(p(class = "text-muted", "No plots generated yet. Go to Step 4 > Plots tab or Diagnostics tab."))
      }

      items <- list()
      fig_num <- 0

      # Custom plots from Plots tab (ONE image, possibly with multiple descriptions)
      if (!is.null(b64) && nchar(b64) > 0) {
        fig_num <- fig_num + 1
        plot_title <- "Model Plots"
        plot_desc_text <- ""

        if (!is.null(plots_desc) && length(plots_desc) > 0) {
          # Use first plot description's title (should be only one for faceted plots)
          plot_title <- plots_desc[[1]]$title
          # Combine all descriptions
          plot_desc_text <- paste(sapply(plots_desc, function(pd) pd$description), collapse = " ")
        }

        items <- c(items, list(
          h5(paste0("Figure ", fig_num, ": ", plot_title)),
          tags$img(src = b64, style = "max-width:100%; height:auto; border:1px solid #ddd; margin-bottom:10px;"),
          if (nchar(plot_desc_text) > 0) p(style = "color:#666; font-size:0.9em;", plot_desc_text),
          hr()
        ))
      }

      # Diagnostics forest plot
      if (!is.null(diag_b64) && nchar(diag_b64) > 0) {
        fig_num <- fig_num + 1
        diag_title <- "Forest Plot (All Predictors)"
        if (!is.null(diag_desc)) {
          diag_title <- diag_desc$title
        }
        items <- c(items, list(
          h5(paste0("Figure ", fig_num, ": ", diag_title)),
          tags$img(src = diag_b64, style = "max-width:100%; height:auto; border:1px solid #ddd; margin-bottom:10px;"),
          p(style = "color:#666; font-size:0.9em;",
            if (!is.null(diag_desc)) diag_desc$description else "")
        ))
      }

      tagList(items)
    })

    output$show_regression <- renderUI({
      # Show the exact same table HTML that was generated in Step 4
      html <- shared$model_result_html
      if (!is.null(html) && nchar(html) > 0) return(HTML(html))
      # Fallback
      res <- shared$model_result
      if (is.null(res)) return(p(class = "text-muted", "No model fitted yet. Go to Step 4."))
      HTML(df_to_html_table(res$tidy, title = "Regression Results"))
    })

    # --- Analysis manifest builder ---
    build_analysis_manifest <- function() {
      manifest <- list()

      # Data summary
      if (!is.null(shared$data)) {
        df <- shared$data
        manifest$data <- list(
          n_rows = nrow(df),
          n_cols = ncol(df),
          n_missing = sum(is.na(df)),
          pct_missing = round(100 * sum(is.na(df)) / (nrow(df) * ncol(df)), 1),
          variables = names(df)
        )
        if (!is.null(shared$var_types)) {
          vt <- shared$var_types
          manifest$data$n_numeric <- sum(vt$type == "numeric")
          manifest$data$n_categorical <- sum(vt$type == "categorical")
          manifest$data$var_types <- paste(
            apply(vt, 1, function(r) paste0(r["variable"], " [", r["type"], "]")),
            collapse = ", "
          )
        }
      }

      # Table 1 — extract as formatted text for LLM consumption
      if (!is.null(shared$table1) && is.data.frame(shared$table1)) {
        t1_df <- shared$table1
        col_widths <- sapply(names(t1_df), function(cn) {
          max(nchar(cn), max(nchar(as.character(t1_df[[cn]])), na.rm = TRUE), na.rm = TRUE)
        })
        header <- paste(mapply(function(nm, w) formatC(nm, width = w, flag = "-"),
                               names(t1_df), col_widths), collapse = " | ")
        sep_line <- paste(sapply(col_widths, function(w) paste(rep("-", w), collapse = "")),
                          collapse = "-+-")
        rows <- apply(t1_df, 1, function(row) {
          paste(mapply(function(val, w) formatC(as.character(val), width = w, flag = "-"),
                       row, col_widths), collapse = " | ")
        })
        manifest$table1 <- paste(c(header, sep_line, rows), collapse = "\n")
      }

      # Regression model
      if (!is.null(shared$model_result)) {
        res <- shared$model_result
        model_label <- switch(res$type,
          "lm" = "Linear regression (OLS)",
          "glm" = "Logistic regression (binomial, logit link)",
          "cox" = "Cox proportional hazards regression",
          "lmer" = "Linear mixed-effects model (REML)",
          res$type)
        tidy_df <- res$tidy
        coef_table <- paste(capture.output(print(
          data.frame(
            Term = tidy_df$term,
            Estimate = round(tidy_df$estimate, 4),
            SE = round(tidy_df$std.error, 4),
            Statistic = round(tidy_df$statistic, 3),
            P = ifelse(tidy_df$p.value < 0.001, "<0.001", round(tidy_df$p.value, 4)),
            CI_low = if ("conf.low" %in% names(tidy_df)) round(tidy_df$conf.low, 4) else NA,
            CI_high = if ("conf.high" %in% names(tidy_df)) round(tidy_df$conf.high, 4) else NA,
            stringsAsFactors = FALSE
          ), row.names = FALSE
        )), collapse = "\n")

        # Add OR/HR if logistic or Cox
        or_hr_text <- ""
        if (res$type %in% c("glm", "cox") && "OR_HR" %in% names(tidy_df)) {
          or_hr_text <- paste(capture.output(print(
            data.frame(
              Term = tidy_df$term,
              OR_HR = round(tidy_df$OR_HR, 3),
              CI_low = round(tidy_df$ci_lower, 3),
              CI_high = round(tidy_df$ci_upper, 3),
              stringsAsFactors = FALSE
            ), row.names = FALSE
          )), collapse = "\n")
        }

        # Model fit statistics
        fit_stats <- ""
        if (!is.null(res$fit)) {
          tryCatch({
            gl <- broom::glance(res$fit)
            stat_parts <- c()
            if ("r.squared" %in% names(gl)) stat_parts <- c(stat_parts, paste0("R-squared = ", round(gl$r.squared, 4)))
            if ("adj.r.squared" %in% names(gl)) stat_parts <- c(stat_parts, paste0("Adj. R-squared = ", round(gl$adj.r.squared, 4)))
            if ("AIC" %in% names(gl)) stat_parts <- c(stat_parts, paste0("AIC = ", round(gl$AIC, 1)))
            if ("BIC" %in% names(gl)) stat_parts <- c(stat_parts, paste0("BIC = ", round(gl$BIC, 1)))
            if ("nobs" %in% names(gl)) stat_parts <- c(stat_parts, paste0("N = ", gl$nobs))
            if ("concordance" %in% names(gl)) stat_parts <- c(stat_parts, paste0("Concordance = ", round(gl$concordance, 3)))
            fit_stats <- paste(stat_parts, collapse = "; ")
          }, error = function(e) NULL)
        }

        manifest$model <- list(
          type = model_label,
          formula = if (!is.null(res$fit)) deparse(formula(res$fit)) else "Not captured",
          coefficients = coef_table,
          or_hr = or_hr_text,
          fit_stats = fit_stats
        )
      }

      # Plots (from Plots tab)
      if (!is.null(shared$plots) && length(shared$plots) > 0) {
        manifest$plots <- shared$plots
      }

      # Diagnostics plot (forest plot from Diagnostics tab)
      if (!is.null(shared$diagnostics_plot)) {
        manifest$diagnostics_plot <- shared$diagnostics_plot
      }

      # Software
      manifest$software <- list(
        r_version = R.version.string,
        packages = paste(
          c("ggplot2", "broom", "labelled",
            "survival", "sandwich", "lmtest", "car", "emmeans",
            "haven", "readxl", "writexl", "lme4",
            "gridExtra", "base64enc"),
          collapse = ", "
        ),
        platform = "WebR (R compiled to WebAssembly, running in-browser)"
      )

      manifest
    }

    # Display the manifest
    output$analysis_manifest_display <- renderUI({
      manifest <- build_analysis_manifest()
      items <- list()

      if (!is.null(manifest$data)) {
        items <- c(items, list(
          p(strong("Data:"), manifest$data$n_rows, "observations,",
            manifest$data$n_cols, "variables.",
            manifest$data$n_numeric, "numeric,",
            manifest$data$n_categorical, "categorical.",
            manifest$data$n_missing, "missing values",
            paste0("(", manifest$data$pct_missing, "%)."))
        ))
      }

      if (!is.null(manifest$table1)) {
        items <- c(items, list(
          p(strong("Table 1:"), "Descriptive statistics generated via gtsummary.")
        ))
      }

      if (!is.null(manifest$model)) {
        items <- c(items, list(
          p(strong("Model:"), manifest$model$type),
          p(strong("Formula:"), tags$code(manifest$model$formula))
        ))
      }

      if (!is.null(manifest$plots) && length(manifest$plots) > 0) {
        plot_types <- sapply(manifest$plots, function(pd) pd$type)
        items <- c(items, list(
          p(strong("Plots:"), length(manifest$plots), "figure(s) generated —",
            paste(unique(plot_types), collapse = ", "))
        ))
      }

      if (!is.null(manifest$diagnostics_plot)) {
        items <- c(items, list(
          p(strong("Diagnostics plot:"), manifest$diagnostics_plot$title)
        ))
      }

      items <- c(items, list(
        p(strong("Software:"), manifest$software$r_version,
          "via", manifest$software$platform)
      ))

      if (length(items) == 1) {
        return(p(class = "text-muted", "Run some analyses first (Steps 3-4) to populate the manifest."))
      }

      tagList(items)
    })

    # Generate the LLM prompt
    observeEvent(input$generate_prompt, {
      manifest <- build_analysis_manifest()

      # Get study info from form
      design <- input$study_design
      if (design == "Other" && nchar(trimws(input$study_design_other)) > 0) {
        design <- trimws(input$study_design_other)
      }
      setting <- trimws(input$study_setting)
      participants <- trimws(input$participants)
      timepoints <- trimws(input$timepoints)
      outcome_defs <- trimws(input$outcome_defs)

      # Build prompt
      prompt_lines <- c(
        "# TASK: Draft Methods and Results Sections for a Peer-Reviewed Publication",
        "",
        "## MANDATORY INSTRUCTIONS",
        "",
        "- Use ONLY the information provided below.",
        "- Do NOT invent, infer, assume, or embellish any methods, results, study characteristics, or interpretations.",
        "- Do NOT introduce statistical methods, analyses, covariates, or outcomes that are not explicitly listed.",
        "- Do NOT report values, sample sizes, p-values, confidence intervals, or effect estimates that are not present in the supplied results.",
        "- If required information is missing, state this explicitly (e.g. 'The study setting was not specified').",
        "- Do not add causal language unless explicitly appropriate for the stated study design.",
        "- Do not include a discussion or interpretation of findings beyond factual reporting.",
        "- Write in a neutral, precise style appropriate for submission to a peer-reviewed journal.",
        "",
        "---",
        "",
        "## SECTION 1: USER-PROVIDED STUDY INFORMATION",
        "",
        paste0("### Study Design: ", if (nchar(design) > 0) design else "[Not specified]"),
        "",
        paste0("### Study Setting: ", if (nchar(setting) > 0) setting else "[Not specified]"),
        "",
        paste0("### Participants: ", if (nchar(participants) > 0) participants else "[Not specified]"),
        "",
        paste0("### Timepoint Definitions: ", if (nchar(timepoints) > 0) timepoints else "[Not specified]"),
        "",
        paste0("### Outcome Definitions: ", if (nchar(outcome_defs) > 0) outcome_defs else "[Not specified]"),
        "",
        "---",
        "",
        "## SECTION 2: ANALYSIS MANIFEST (auto-captured from app)",
        ""
      )

      # Data summary
      if (!is.null(manifest$data)) {
        prompt_lines <- c(prompt_lines,
          "### Data Summary",
          paste0("- Sample size: ", manifest$data$n_rows, " observations"),
          paste0("- Variables: ", manifest$data$n_cols,
                 " (", manifest$data$n_numeric, " numeric, ",
                 manifest$data$n_categorical, " categorical)"),
          paste0("- Missing values: ", manifest$data$n_missing,
                 " (", manifest$data$pct_missing, "% of all cells)"),
          paste0("- Variable classifications: ", manifest$data$var_types),
          "")
      }

      # Table 1
      if (!is.null(manifest$table1)) {
        prompt_lines <- c(prompt_lines,
          "### Table 1: Descriptive Statistics",
          "```",
          manifest$table1,
          "```",
          "")
      }

      # Model
      if (!is.null(manifest$model)) {
        prompt_lines <- c(prompt_lines,
          "### Statistical Model",
          paste0("- Type: ", manifest$model$type),
          paste0("- Formula: ", manifest$model$formula),
          "",
          "### Model Coefficients",
          "```",
          manifest$model$coefficients,
          "```",
          "")

        if (nchar(manifest$model$or_hr) > 0) {
          label <- if (grepl("ogistic", manifest$model$type)) "Odds Ratios" else "Hazard Ratios"
          prompt_lines <- c(prompt_lines,
            paste0("### ", label, " (exponentiated)"),
            "```",
            manifest$model$or_hr,
            "```",
            "")
        }

        if (nchar(manifest$model$fit_stats) > 0) {
          prompt_lines <- c(prompt_lines,
            "### Model Fit Statistics",
            paste0("- ", manifest$model$fit_stats),
            "")
        }
      }

      # Plots / Figures
      fig_num <- 0
      if (!is.null(manifest$plots) && length(manifest$plots) > 0) {
        if (fig_num == 0) {
          prompt_lines <- c(prompt_lines, "### Figures Generated", "")
        }
        for (i in seq_along(manifest$plots)) {
          fig_num <- fig_num + 1
          pd <- manifest$plots[[i]]
          prompt_lines <- c(prompt_lines,
            paste0("**Figure ", fig_num, ": ", pd$title, "**"),
            paste0("- Type: ", pd$type),
            paste0("- Description: ", pd$description))
          if (!is.null(pd$details) && nchar(pd$details) > 0) {
            prompt_lines <- c(prompt_lines,
              paste0("- Key values: ", pd$details))
          }
          prompt_lines <- c(prompt_lines, "")
        }
      }

      # Diagnostics plot (forest plot)
      if (!is.null(manifest$diagnostics_plot)) {
        if (fig_num == 0) {
          prompt_lines <- c(prompt_lines, "### Figures Generated", "")
        }
        fig_num <- fig_num + 1
        pd <- manifest$diagnostics_plot
        prompt_lines <- c(prompt_lines,
          paste0("**Figure ", fig_num, ": ", pd$title, "**"),
          paste0("- Type: ", pd$type),
          paste0("- Description: ", pd$description))
        if (!is.null(pd$details) && nchar(pd$details) > 0) {
          prompt_lines <- c(prompt_lines,
            paste0("- Key values: ", pd$details))
        }
        prompt_lines <- c(prompt_lines, "")
      }

      # Software
      prompt_lines <- c(prompt_lines,
        "### Software",
        paste0("- ", manifest$software$r_version),
        paste0("- Packages: ", manifest$software$packages),
        paste0("- Platform: ", manifest$software$platform),
        "",
        "---",
        "",
        "## REQUESTED OUTPUT",
        "",
        "Please generate the following sections. ALL tables must be **journal-ready**:",
        "formatted for direct inclusion in a peer-reviewed manuscript, with proper",
        "headers, footnotes, units, and formatting conventions (e.g. mean (SD),",
        "n (%), OR [95% CI]).",
        "",
        "### 1. Methods Section",
        "   - Study design and setting",
        "   - Participants and eligibility criteria",
        "   - Outcome definitions",
        "   - Statistical analysis methods (matching exactly what was performed)",
        "   - Software and reproducibility statement",
        "",
        "### 2. Results Section",
        "   - Participant flow and sample characteristics",
        "   - Primary and secondary outcome results",
        "   - In-text references to tables and figures (e.g. 'Table 1', 'Table 2', 'Figure 1')",
        "   - Effect estimates with confidence intervals and p-values reported in-text",
        "   - Reference any generated figures with appropriate captions (see 'Figures Generated' above)",
        "",
        "### 3. Table 1 — Baseline Characteristics",
        "Generate a **journal-ready Table 1** from the descriptive statistics above.",
        "   - Title: 'Table 1. Baseline characteristics of study participants'",
        "   - Format continuous variables as: mean (SD) or median [IQR] as appropriate",
        "   - Format categorical variables as: n (%)",
        "   - Include column headers for each group (if stratified) and overall",
        "   - Add a footnote row listing statistical tests used (e.g. t-test, chi-squared, Fisher's exact)",
        "   - Use the exact variable names and values from the supplied data — do NOT invent additional variables",
        "",
        "### 4. Table 2 — Regression Results",
        "Generate a **journal-ready Table 2** from the model coefficients above.",
        "   - Title: 'Table 2. Association between [exposure] and [outcome]'",
        "     (fill in exposure/outcome from the model formula)",
        "   - For linear regression: report beta coefficients (95% CI), p-values",
        "   - For logistic regression: report odds ratios (95% CI), p-values",
        "   - For Cox regression: report hazard ratios (95% CI), p-values",
        "   - For mixed models: report fixed-effect estimates (95% CI), p-values, and random-effect variance components",
        "   - Include intercept/reference categories as appropriate",
        "   - Add a footnote row with: model type, sample size, R-squared or C-statistic (if available),",
        "     and any notes about robust standard errors or clustering",
        "   - Use the exact terms and values from the supplied coefficients — do NOT invent additional results",
        "",
        "### 5. Figure Captions",
        "For each figure listed in the 'Figures Generated' section above,",
        "provide a journal-ready figure caption including:",
        "   - Figure number (e.g. 'Figure 1.')",
        "   - A descriptive title",
        "   - Explanation of what is shown (axes, error bars, reference lines)",
        "   - Any abbreviations used",
        "   - Use ONLY figures that were actually generated — do NOT invent additional figures",
        "",
        "Write for a general medical/scientific journal audience (e.g. BMJ, JAMA style).",
        "Tables should be formatted in plain text with clear column alignment,",
        "ready to be pasted into a manuscript or converted to a formatted table."
      )

      prompt_text <- paste(prompt_lines, collapse = "\n")
      session$sendCustomMessage("updateReportText",
        list(id = ns("prompt_text"), text = prompt_text))
      showNotification("LLM prompt generated. Copy and paste into your preferred LLM.",
                       type = "message")
    })

    # Copy prompt to clipboard
    observeEvent(input$copy_prompt, {
      session$sendCustomMessage("copyTextareaToClipboard", ns("prompt_text"))
    })

    # --- HTML report for PDF export ---
    # Generate a statistical methods summary paragraph
    build_methods_summary <- function() {
      parts <- c()

      # Data description
      if (!is.null(shared$data)) {
        df <- shared$data
        n_obs <- nrow(df)
        n_vars <- ncol(df)
        n_miss <- sum(is.na(df))
        parts <- c(parts, paste0(
          "The analytical dataset comprised ", n_obs, " observations across ",
          n_vars, " variables", if (n_miss > 0) paste0(
            " (", n_miss, " missing values, ",
            round(100 * n_miss / (n_obs * n_vars), 1), "% of all cells)") else "",
          "."))
      }

      # Descriptive statistics
      if (!is.null(shared$table1)) {
        parts <- c(parts,
          "Descriptive statistics were summarised using frequencies and percentages for categorical variables and means (standard deviations) or medians (interquartile ranges) for continuous variables, as appropriate.")
      }

      # Regression model
      if (!is.null(shared$model_result)) {
        res <- shared$model_result
        n_model <- tryCatch(nobs(res$fit), error = function(e) NULL)
        is_binary_mixed <- isTRUE(res$is_binary_mixed)
        model_desc <- switch(res$type,
          "lm" = "Multivariable linear regression was used to estimate regression coefficients with 95% confidence intervals.",
          "glm" = "Multivariable logistic regression was used to estimate odds ratios (ORs) with 95% confidence intervals.",
          "cox" = "Cox proportional hazards regression was used to estimate hazard ratios (HRs) with 95% confidence intervals.",
          "lmer" = if (is_binary_mixed) {
            "A mixed-effects logistic regression model (generalised linear mixed model with a logit link) was fitted to estimate odds ratios (ORs) with 95% confidence intervals, accounting for clustering using random intercepts."
          } else {
            "A linear mixed-effects model was fitted to estimate regression coefficients with 95% confidence intervals, accounting for clustering using random intercepts."
          },
          "")
        parts <- c(parts, model_desc)

        # Missing data handling
        if (!is.null(n_model) && !is.null(shared$data)) {
          n_total <- nrow(shared$data)
          n_dropped <- n_total - n_model
          if (n_dropped > 0) {
            parts <- c(parts, paste0(
              "Complete case analysis was used; ", n_dropped, " observations (",
              round(100 * n_dropped / n_total, 1),
              "%) were excluded due to missing data, yielding an analytical sample of ",
              n_model, " observations."))
          } else {
            parts <- c(parts, paste0(
              "There were no missing data in the analytical variables; all ",
              n_model, " observations were included."))
          }
        }

        # AIC/BIC
        tryCatch({
          aic_val <- tryCatch(round(AIC(res$fit), 1), error = function(e) NULL)
          bic_val <- tryCatch(round(BIC(res$fit), 1), error = function(e) NULL)
          fit_parts <- c()
          if (!is.null(aic_val) && is.finite(aic_val)) fit_parts <- c(fit_parts, paste0("AIC = ", aic_val))
          if (!is.null(bic_val) && is.finite(bic_val)) fit_parts <- c(fit_parts, paste0("BIC = ", bic_val))
          if (length(fit_parts) > 0) {
            parts <- c(parts, paste0("Model fit indices: ", paste(fit_parts, collapse = ", "), "."))
          }
        }, error = function(e) NULL)
      }

      # Software
      pkg_versions <- c()
      for (pkg in c("gtsummary", "ggplot2", "survival", "lme4")) {
        if (requireNamespace(pkg, quietly = TRUE)) {
          ver <- tryCatch(as.character(packageVersion(pkg)), error = function(e) NULL)
          if (!is.null(ver)) pkg_versions <- c(pkg_versions, paste0(pkg, " v", ver))
        }
      }
      software_text <- paste0(
        "All analyses were conducted in R (", R.version.string, ") running in the browser via WebR")
      if (length(pkg_versions) > 0) {
        software_text <- paste0(software_text, ", using the following packages: ",
                                paste(pkg_versions, collapse = ", "))
      }
      software_text <- paste0(software_text, ". A two-sided p-value < 0.05 was considered statistically significant.")
      parts <- c(parts, software_text)

      paste(parts, collapse = " ")
    }

    generate_html_report <- function() {
      # Use the exact same rendered HTML from the app for Table 1 and Table 2
      table1_html <- ""
      table1_title <- ""
      if (!is.null(shared$table1_html) && nchar(shared$table1_html) > 0) {
        # Use the exact HTML rendered in Step 3
        table1_html <- shared$table1_html
        n_t1 <- if (!is.null(shared$data)) nrow(shared$data) else "?"
        table1_title <- paste0("Table 1. Characteristics of participants (N = ", n_t1, ")")
      } else if (!is.null(shared$table1)) {
        # Fallback: rebuild from data frame
        n_t1 <- if (!is.null(shared$data)) nrow(shared$data) else "?"
        table1_title <- paste0("Table 1. Characteristics of participants (N = ", n_t1, ")")
        table1_html <- df_to_html_table(shared$table1, title = table1_title)
      }

      # Build regression results — use the exact HTML rendered in Step 4
      regression_html <- ""
      table2_title <- ""
      fit_stats_html <- ""
      if (!is.null(shared$model_result_html) && nchar(shared$model_result_html) > 0) {
        # Use the exact HTML rendered in Step 4
        regression_html <- shared$model_result_html
        # Extract title for the section header
        if (!is.null(shared$model_result)) {
          res <- shared$model_result
          n_model <- tryCatch(nobs(res$fit), error = function(e) "?")
          is_binary_mixed <- isTRUE(res$is_binary_mixed)
          estimate_desc <- switch(res$type,
            "lm" = "Linear regression coefficients",
            "glm" = "Odds ratios from logistic regression",
            "cox" = "Hazard ratios from Cox proportional hazards regression",
            "lmer" = if (is_binary_mixed) "Odds ratios from mixed-effects logistic regression"
                     else "Mixed-effects linear regression coefficients",
            res$type)
          table2_title <- paste0("Table 2. ", estimate_desc, " (N = ", n_model, ")")
        }
        # AIC/BIC
        if (!is.null(shared$model_result)) {
          tryCatch({
            aic_val <- tryCatch(round(AIC(shared$model_result$fit), 1), error = function(e) NULL)
            bic_val <- tryCatch(round(BIC(shared$model_result$fit), 1), error = function(e) NULL)
            parts <- c()
            if (!is.null(aic_val) && is.finite(aic_val)) parts <- c(parts, paste0("AIC = ", aic_val))
            if (!is.null(bic_val) && is.finite(bic_val)) parts <- c(parts, paste0("BIC = ", bic_val))
            if (length(parts) > 0) {
              fit_stats_html <- paste0('<p style="color:#555; font-size:0.9em;"><strong>Model fit:</strong> ',
                                      paste(parts, collapse = " &nbsp;&nbsp; "), '</p>')
            }
          }, error = function(e) NULL)
        }
      } else if (!is.null(shared$model_result)) {
        # Fallback: rebuild from data
        res <- shared$model_result
        display_df <- res$tidy
        display_df$estimate <- round(display_df$estimate, 4)
        display_df$std.error <- round(display_df$std.error, 4)
        display_df$statistic <- round(display_df$statistic, 3)
        display_df$p.value <- ifelse(display_df$p.value < 0.001, "<0.001",
                                     round(display_df$p.value, 4))
        n_model <- tryCatch(nobs(res$fit), error = function(e) "?")
        is_binary_mixed <- isTRUE(res$is_binary_mixed)
        estimate_desc <- switch(res$type,
          "lm" = "Linear regression coefficients",
          "glm" = "Odds ratios from logistic regression",
          "cox" = "Hazard ratios from Cox proportional hazards regression",
          "lmer" = if (is_binary_mixed) "Odds ratios from mixed-effects logistic regression"
                   else "Mixed-effects linear regression coefficients",
          res$type)
        table2_title <- paste0("Table 2. ", estimate_desc, " (N = ", n_model, ")")
        regression_html <- df_to_html_table(display_df, title = table2_title)
      }

      # Data summary section
      data_html <- ""
      if (!is.null(shared$data)) {
        df <- shared$data
        data_html <- paste0(
          "<ul>",
          "<li><strong>Rows:</strong> ", nrow(df), "</li>",
          "<li><strong>Columns:</strong> ", ncol(df), "</li>",
          "<li><strong>Numeric variables:</strong> ", sum(sapply(df, is.numeric)), "</li>",
          "<li><strong>Missing values:</strong> ", sum(is.na(df)), "</li>",
          "</ul>")
      }

      # Assemble full HTML page
      html <- paste0(
        '<!DOCTYPE html><html><head><meta charset="UTF-8">',
        '<title>Statistical Analysis Report</title>',
        '<style>',
        '  body { font-family: "Segoe UI", Tahoma, Geneva, Verdana, sans-serif;',
        '         max-width: 900px; margin: 0 auto; padding: 30px; color: #333;',
        '         font-size: 14px; line-height: 1.5; }',
        '  h1 { color: #667eea; border-bottom: 2px solid #667eea; padding-bottom: 10px; }',
        '  h2 { color: #444; margin-top: 30px; border-bottom: 1px solid #ddd; padding-bottom: 5px; }',
        '  table { border-collapse: collapse; width: 100%; margin: 15px 0; }',
        '  th, td { border: 1px solid #ddd; padding: 8px 12px; text-align: left; }',
        '  th { background-color: #f8f9fa; font-weight: bold; }',
        '  tr:nth-child(even) { background-color: #f9f9f9; }',
        '  .meta { color: #666; font-size: 0.9em; }',
        '  .privacy { background: #d4edda; border: 1px solid #c3e6cb; ',
        '             padding: 8px 15px; border-radius: 4px; margin: 15px 0; ',
        '             color: #155724; font-size: 0.9em; }',
        '  @media print { body { padding: 0; } }',
        '</style></head><body>',
        '<h1>Statistical Analysis Report</h1>',
        '<p class="meta"><strong>Generated:</strong> ', format(Sys.time(), "%Y-%m-%d %H:%M"), '</p>',
        '<p class="meta"><strong>Data:</strong> ',
          if (!is.null(shared$data_name)) shared$data_name else "Unknown", '</p>',
        '<p class="meta"><strong>R Version:</strong> ', R.version.string, '</p>',
        '<div class="privacy">All analysis performed in-browser via WebR. ',
        'No data was uploaded to any server.</div>')

      if (nchar(data_html) > 0) {
        html <- paste0(html, '<h2>Data Summary</h2>', data_html)
      }

      # Statistical methods summary
      methods_text <- build_methods_summary()
      if (nchar(methods_text) > 0) {
        html <- paste0(html, '<h2>Statistical Methods</h2>',
                        '<p>', htmltools::htmlEscape(methods_text), '</p>')
      }

      if (nchar(table1_html) > 0) {
        html <- paste0(html, '<h2>', htmltools::htmlEscape(table1_title), '</h2>', table1_html)
      }

      if (nchar(regression_html) > 0) {
        html <- paste0(html, '<h2>', htmltools::htmlEscape(table2_title), '</h2>',
                        regression_html, fit_stats_html)
      }

      # Plots section
      fig_num <- 0
      has_figures <- (!is.null(shared$plot_base64) && nchar(shared$plot_base64) > 0) ||
                     (!is.null(shared$diagnostics_plot_base64) && nchar(shared$diagnostics_plot_base64) > 0)

      if (has_figures) {
        html <- paste0(html, '<h2>Figures</h2>')
      }

      # Custom plots from Plots tab
      if (!is.null(shared$plot_base64) && nchar(shared$plot_base64) > 0) {
        plot_title <- "Model Plots"
        if (!is.null(shared$plots) && length(shared$plots) > 0) {
          plot_title <- shared$plots[[1]]$title
          fig_num <- fig_num + 1
        }
        html <- paste0(html,
          '<p><strong>Figure ', fig_num, ': ', htmltools::htmlEscape(plot_title), '</strong></p>',
          '<img src="', shared$plot_base64, '" ',
          'style="max-width:100%; height:auto; border:1px solid #ddd; margin:10px 0;" ',
          'alt="Model plot" />')
        # Add figure descriptions
        if (!is.null(shared$plots) && length(shared$plots) > 0) {
          html <- paste0(html, '<p style="font-size:0.9em; color:#666;">')
          for (pd in shared$plots) {
            html <- paste0(html, htmltools::htmlEscape(pd$description), '<br/>')
          }
          html <- paste0(html, '</p>')
        }
      }

      # Diagnostics forest plot
      if (!is.null(shared$diagnostics_plot_base64) && nchar(shared$diagnostics_plot_base64) > 0) {
        fig_num <- fig_num + 1
        diag_title <- "Forest Plot"
        if (!is.null(shared$diagnostics_plot)) {
          diag_title <- shared$diagnostics_plot$title
        }
        html <- paste0(html,
          '<p><strong>Figure ', fig_num, ': ', htmltools::htmlEscape(diag_title), '</strong></p>',
          '<img src="', shared$diagnostics_plot_base64, '" ',
          'style="max-width:100%; height:auto; border:1px solid #ddd; margin:10px 0;" ',
          'alt="Forest plot" />')
        if (!is.null(shared$diagnostics_plot)) {
          html <- paste0(html,
            '<p style="font-size:0.9em; color:#666;">',
            htmltools::htmlEscape(shared$diagnostics_plot$description),
            '</p>')
        }
      }

      html <- paste0(html, '</body></html>')
      html
    }

    # Build plain text report content
    build_text_report <- function() {
      lines <- c(
        "STATISTICAL ANALYSIS REPORT",
        paste0("Generated: ", Sys.time()),
        paste0("Data: ", if (!is.null(shared$data_name)) shared$data_name else "Unknown"),
        ""
      )

      if (!is.null(shared$data)) {
        df <- shared$data
        lines <- c(lines,
          "DATA SUMMARY",
          paste0("  Rows: ", nrow(df)),
          paste0("  Columns: ", ncol(df)),
          paste0("  Missing values: ", sum(is.na(df))),
          "")
      }

      # Methods summary
      methods_text <- build_methods_summary()
      if (nchar(methods_text) > 0) {
        lines <- c(lines, "STATISTICAL METHODS", "",
          strwrap(methods_text, width = 80), "")
      }

      if (!is.null(shared$table1)) {
        n_t1 <- if (!is.null(shared$data)) nrow(shared$data) else "?"
        # Strip **bold** markdown from variable labels for plain text
        tbl_txt <- shared$table1
        if (is.data.frame(tbl_txt) && "Variable" %in% names(tbl_txt)) {
          tbl_txt$Variable <- gsub("\\*\\*(.+?)\\*\\*", "\\1", tbl_txt$Variable)
        }
        lines <- c(lines,
          paste0("TABLE 1. CHARACTERISTICS OF PARTICIPANTS (N = ", n_t1, ")"), "",
          paste(capture.output(print(tbl_txt, row.names = FALSE)), collapse = "\n"), "")
      }

      if (!is.null(shared$model_result)) {
        res <- shared$model_result
        tidy_df <- res$tidy
        n_model <- tryCatch(nobs(res$fit), error = function(e) "?")
        is_binary_mixed <- isTRUE(res$is_binary_mixed)
        estimate_desc <- toupper(switch(res$type,
          "lm" = "Linear regression coefficients",
          "glm" = "Odds ratios from logistic regression",
          "cox" = "Hazard ratios from Cox proportional hazards regression",
          "lmer" = if (is_binary_mixed) "Odds ratios from mixed-effects logistic regression"
                   else "Mixed-effects linear regression coefficients",
          res$type))

        # Build text table matching the app column order
        is_exp <- res$type %in% c("glm", "cox") || (res$type == "lmer" && is_binary_mixed)
        txt_df <- data.frame(
          Term = tidy_df$term,
          Estimate = unname(round(as.numeric(tidy_df$estimate), 2)),
          stringsAsFactors = FALSE
        )
        if (is_exp && "OR_HR" %in% names(tidy_df)) {
          or_label <- if (res$type == "cox") "HR" else "OR"
          txt_df[[or_label]] <- unname(round(as.numeric(tidy_df$OR_HR), 2))
        }
        if ("conf.low" %in% names(tidy_df)) {
          txt_df[["CI Lower"]] <- unname(round(as.numeric(tidy_df$conf.low), 2))
          txt_df[["CI Upper"]] <- unname(round(as.numeric(tidy_df$conf.high), 2))
        }
        if (is_exp && "ci_lower" %in% names(tidy_df)) {
          txt_df[["CI Lower"]] <- unname(round(as.numeric(tidy_df$ci_lower), 2))
          txt_df[["CI Upper"]] <- unname(round(as.numeric(tidy_df$ci_upper), 2))
        }
        txt_df[["P-value"]] <- ifelse(as.numeric(tidy_df$p.value) < 0.001, "<0.001",
                                       as.character(round(as.numeric(tidy_df$p.value), 3)))
        txt_df[["Std. Error"]] <- unname(round(as.numeric(tidy_df$std.error), 2))
        txt_df[["Statistic"]] <- unname(round(as.numeric(tidy_df$statistic), 2))

        lines <- c(lines,
          paste0("TABLE 2. ", estimate_desc, " (N = ", n_model, ")"), "",
          paste(capture.output(print(txt_df, row.names = FALSE)), collapse = "\n"), "")
        # AIC/BIC
        tryCatch({
          aic_val <- tryCatch(round(AIC(res$fit), 1), error = function(e) NULL)
          bic_val <- tryCatch(round(BIC(res$fit), 1), error = function(e) NULL)
          parts <- c()
          if (!is.null(aic_val) && is.finite(aic_val)) parts <- c(parts, paste0("AIC = ", aic_val))
          if (!is.null(bic_val) && is.finite(bic_val)) parts <- c(parts, paste0("BIC = ", bic_val))
          if (length(parts) > 0) lines <- c(lines, paste("Model fit:", paste(parts, collapse = "  ")), "")
        }, error = function(e) NULL)
      }

      # Figures
      fig_num <- 0
      has_figures <- (!is.null(shared$plots) && length(shared$plots) > 0) ||
                     !is.null(shared$diagnostics_plot)

      if (has_figures) {
        lines <- c(lines, "FIGURES", "")
      }

      if (!is.null(shared$plots) && length(shared$plots) > 0) {
        for (i in seq_along(shared$plots)) {
          fig_num <- fig_num + 1
          pd <- shared$plots[[i]]
          lines <- c(lines, paste0("  Figure ", fig_num, ": ", pd$title),
                     paste0("  ", pd$description))
          if (!is.null(pd$details) && nchar(pd$details) > 0) {
            lines <- c(lines, paste0("  Values: ", pd$details))
          }
          lines <- c(lines, "")
        }
      }

      if (!is.null(shared$diagnostics_plot)) {
        fig_num <- fig_num + 1
        pd <- shared$diagnostics_plot
        lines <- c(lines, paste0("  Figure ", fig_num, ": ", pd$title),
                   paste0("  ", pd$description))
        if (!is.null(pd$details) && nchar(pd$details) > 0) {
          lines <- c(lines, paste0("  Values: ", pd$details))
        }
        lines <- c(lines, "")
      }

      paste(lines, collapse = "\n")
    }

    # Download as PDF via browser print dialog
    observeEvent(input$download_pdf, {
      html_report <- generate_html_report()
      session$sendCustomMessage("downloadPDF", list(content = html_report))
    })

    # Download as plain text file
    observeEvent(input$download_txt, {
      text_content <- build_text_report()
      session$sendCustomMessage("downloadTextFile",
        list(content = text_content, filename = "analysis_report.txt"))
    })

    # Download as Word (HTML with Word MIME type)
    observeEvent(input$download_word, {
      html_report <- generate_html_report()
      # Wrap with Word-compatible XML namespace header
      word_header <- paste0(
        '<html xmlns:o="urn:schemas-microsoft-com:office:office" ',
        'xmlns:w="urn:schemas-microsoft-com:office:word" ',
        'xmlns="http://www.w3.org/TR/REC-html40">',
        '<head><meta charset="utf-8"></head><body>')
      word_content <- paste0(word_header, html_report, '</body></html>')
      session$sendCustomMessage("downloadWordFile",
        list(content = word_content, filename = "analysis_report.doc"))
    })
  })
}


# =============================================================================
# MAIN UI
# =============================================================================

ui <- fluidPage(
  theme = NULL,
  tags$head(
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/html2pdf.js/0.10.1/html2pdf.bundle.min.js"),
    tags$style(HTML("
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
      #loading-overlay {
        position: fixed; top: 0; left: 0; width: 100%; height: 100%;
        background: rgba(255,255,255,0.95); z-index: 9999;
        display: flex; flex-direction: column;
        justify-content: center; align-items: center;
      }
      #loading-overlay h2 { color: #667eea; margin-bottom: 20px; }
      #loading-overlay .progress-bar-container {
        width: 400px; max-width: 80%; height: 24px;
        background: #e9ecef; border-radius: 12px; overflow: hidden;
      }
      #loading-overlay .progress-bar-fill {
        height: 100%; background: linear-gradient(90deg, #667eea, #764ba2);
        border-radius: 12px; transition: width 0.3s ease;
        width: 0%;
      }
      #loading-overlay .progress-text {
        margin-top: 10px; color: #6c757d; font-size: 14px;
      }
    ")),
    # JS handlers for clipboard, download, and loading screen
    tags$script(HTML("
      // Copy table HTML to clipboard
      Shiny.addCustomMessageHandler('copyTableToClipboard', function(elementId) {
        var el = document.getElementById(elementId);
        if (!el) {
          alert('No table to copy. Generate results first.');
          return;
        }
        // Copy as rich HTML (for pasting into Word/Docs)
        var range = document.createRange();
        range.selectNodeContents(el);
        var sel = window.getSelection();
        sel.removeAllRanges();
        sel.addRange(range);
        try {
          document.execCommand('copy');
          sel.removeAllRanges();
        } catch(e) {
          // Fallback: copy as plain text
          navigator.clipboard.writeText(el.innerText);
        }
      });

      // Download report as PDF using html2pdf.js
      Shiny.addCustomMessageHandler('downloadPDF', function(msg) {
        // Use an iframe so the full HTML (with styles) renders correctly
        var iframe = document.createElement('iframe');
        iframe.style.position = 'fixed';
        iframe.style.left = '-9999px';
        iframe.style.top = '0';
        iframe.style.width = '800px';
        iframe.style.height = '1200px';
        iframe.style.border = 'none';
        document.body.appendChild(iframe);
        var doc = iframe.contentDocument || iframe.contentWindow.document;
        doc.open();
        doc.write(msg.content);
        doc.close();
        // Wait for content to render, then generate PDF
        setTimeout(function() {
          var opt = {
            margin: [15, 15, 15, 15],
            filename: 'analysis_report.pdf',
            image: { type: 'png', quality: 1.0 },
            html2canvas: { scale: 4, useCORS: true, letterRendering: true, logging: false },
            jsPDF: { unit: 'mm', format: 'a4', orientation: 'portrait' },
            pagebreak: { mode: ['avoid-all', 'css', 'legacy'] }
          };
          html2pdf().set(opt).from(doc.body).save().then(function() {
            document.body.removeChild(iframe);
          });
        }, 500);
      });

      // Download as plain text file
      Shiny.addCustomMessageHandler('downloadTextFile', function(msg) {
        var blob = new Blob([msg.content], {type: 'text/plain;charset=utf-8'});
        var a = document.createElement('a');
        a.href = URL.createObjectURL(blob);
        a.download = msg.filename || 'report.txt';
        document.body.appendChild(a);
        a.click();
        document.body.removeChild(a);
        URL.revokeObjectURL(a.href);
      });

      // Download as Word (.doc) file
      Shiny.addCustomMessageHandler('downloadWordFile', function(msg) {
        var blob = new Blob([msg.content], {type: 'application/msword'});
        var a = document.createElement('a');
        a.href = URL.createObjectURL(blob);
        a.download = msg.filename || 'report.doc';
        document.body.appendChild(a);
        a.click();
        document.body.removeChild(a);
        URL.revokeObjectURL(a.href);
      });

      // Update report textarea
      Shiny.addCustomMessageHandler('updateReportText', function(msg) {
        var el = document.getElementById(msg.id);
        if (el) el.value = msg.text;
      });

      // Copy textarea content to clipboard
      Shiny.addCustomMessageHandler('copyTextareaToClipboard', function(elementId) {
        var el = document.getElementById(elementId);
        if (!el || !el.value || el.value.indexOf('Complete the study') === 0) {
          alert('Generate the prompt first, then copy.');
          return;
        }
        navigator.clipboard.writeText(el.value).then(function() {
          // Brief visual feedback
        }).catch(function() {
          el.select();
          document.execCommand('copy');
        });
      });

      // Download plot as PNG from base64
      Shiny.addCustomMessageHandler('downloadPlotPNG', function(msg) {
        var a = document.createElement('a');
        a.href = msg.data;
        a.download = msg.filename || 'plot.png';
        document.body.appendChild(a);
        a.click();
        document.body.removeChild(a);
      });

      // Copy plot to clipboard as PNG
      Shiny.addCustomMessageHandler('copyPlotToClipboard', function(msg) {
        fetch(msg.data)
          .then(function(res) { return res.blob(); })
          .then(function(blob) {
            var item = new ClipboardItem({'image/png': blob});
            navigator.clipboard.write([item]).then(function() {
              // success
            }).catch(function(err) {
              alert('Could not copy plot to clipboard. Try the download button instead.');
            });
          })
          .catch(function() {
            alert('Could not process plot image. Try the download button instead.');
          });
      });

      // Loading progress updates
      Shiny.addCustomMessageHandler('updateLoadingProgress', function(msg) {
        var bar = document.querySelector('#loading-overlay .progress-bar-fill');
        var text = document.querySelector('#loading-overlay .progress-text');
        if (bar) bar.style.width = msg.pct + '%';
        if (text) text.textContent = msg.message;
      });
      Shiny.addCustomMessageHandler('hideLoadingOverlay', function(msg) {
        var overlay = document.getElementById('loading-overlay');
        if (overlay) overlay.style.display = 'none';
      });
    "))
  ),

  # Loading overlay (shown until packages are installed)
  div(id = "loading-overlay",
    h2("Statistical Analysis"),
    p("Initializing R environment in your browser..."),
    div(class = "progress-bar-container",
      div(class = "progress-bar-fill")
    ),
    p(class = "progress-text", "Loading WebR runtime...")
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
        h3("Step 5: Results & Export"),
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
    table1_html = NULL,
    model_result = NULL,
    model_result_html = NULL,
    plots = NULL,
    plot_base64 = NULL,
    diagnostics_plot = NULL,
    diagnostics_plot_base64 = NULL,
    last_export = NULL,
    packages_ready = FALSE,
    modeling_ready = FALSE
  )

  # --- Package installation with progress ------------------------------------
  # Phase 1: Install ESSENTIAL packages only (Steps 1-2) — fast startup
  observe({
    essential_pkgs <- c("haven", "readxl", "labelled")
    total <- length(essential_pkgs)

    for (i in seq_along(essential_pkgs)) {
      pct <- round(100 * i / total)
      session$sendCustomMessage("updateLoadingProgress", list(
        pct = pct,
        message = paste0("Installing ", essential_pkgs[i], " (", i, "/", total, ") ... ", pct, "%")
      ))
      install_if_needed(essential_pkgs[i])
    }

    session$sendCustomMessage("updateLoadingProgress", list(
      pct = 100,
      message = "Ready! Loading additional packages in background..."
    ))
    session$sendCustomMessage("hideLoadingOverlay", list())
    shared$packages_ready <- TRUE
  }) |> bindEvent(TRUE, once = TRUE)

  # Phase 2: Install DEFERRED packages in background (Steps 3-5)
  observe({
    req(shared$packages_ready)
    deferred_pkgs <- c("munsell", "ggplot2", "broom",
                       "survival", "sandwich", "lmtest", "car", "emmeans",
                       "writexl", "lme4", "gridExtra", "base64enc")

    for (pkg in deferred_pkgs) {
      install_if_needed(pkg)
    }

    # Force-load ggplot2 into the namespace so requireNamespace() finds it
    tryCatch(library(ggplot2), error = function(e) NULL)

    shared$modeling_ready <- TRUE
  }) |> bindEvent(shared$packages_ready, once = TRUE)

  # Current step tracker
  current_step <- reactiveVal(1)
  total_steps <- 5
  step_names <- c("Upload", "Explore", "Table 1", "Model", "Results")
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

  # Helper: check if modeling packages are ready before allowing Step 3+
  navigate_to_step <- function(target_step) {
    if (target_step >= 3 && !isTRUE(shared$modeling_ready)) {
      showNotification(
        "Additional packages are still loading. Please wait a moment...",
        type = "warning", duration = 3
      )
      return(FALSE)
    }
    current_step(target_step)
    updateTabsetPanel(session, "wizard", selected = step_ids[target_step])
    return(TRUE)
  }

  # Navigation
  observeEvent(input$next_step, {
    step <- current_step()
    if (step < total_steps) {
      navigate_to_step(step + 1)
    }
  })

  observeEvent(input$prev_step, {
    step <- current_step()
    if (step > 1) {
      navigate_to_step(step - 1)
    }
  })

  observeEvent(input$goto_step, {
    step <- input$goto_step
    if (step >= 1 && step <= total_steps) {
      navigate_to_step(step)
    }
  })

  # Module servers
  upload_server("upload", shared)
  explore_server("explore", shared)
  table1_server("table1", shared)
  model_server("model", shared)
  results_server("results", shared)
}


# =============================================================================
# Launch
# =============================================================================
shinyApp(ui, server)
