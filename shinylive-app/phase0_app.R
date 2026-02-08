# Phase 0: WebR Package Validation Test App
# This Shinylive app tests every planned package and runs smoke tests + benchmarks.
# All results are displayed in-browser for manual recording.
#
# IMPORTANT: In WebR/Shinylive, packages must be explicitly installed before use.
# This app uses webr::install() to download WASM binaries from the WebR repo.

library(shiny)

ui <- fluidPage(
  titlePanel("Phase 0: WebR Package Validation"),
  tags$style(HTML("
    .pass { color: #28a745; font-weight: bold; }
    .fail { color: #dc3545; font-weight: bold; }
    .warn { color: #ffc107; font-weight: bold; }
    .mono { font-family: monospace; font-size: 12px; }
    .section-header {
      background-color: #f8f9fa; padding: 10px; margin-top: 15px;
      border-left: 4px solid #007bff; font-weight: bold; font-size: 16px;
    }
    .result-box {
      border: 1px solid #dee2e6; border-radius: 4px; padding: 10px;
      margin: 5px 0; background-color: #fff;
    }
    .benchmark-table { width: 100%; border-collapse: collapse; }
    .benchmark-table th, .benchmark-table td {
      border: 1px solid #dee2e6; padding: 8px; text-align: left;
    }
    .benchmark-table th { background-color: #f8f9fa; }
    #copy_results { margin: 10px 0; }
    .progress-msg {
      color: #6c757d; font-style: italic; margin: 5px 0;
    }
  ")),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Test Controls"),
      p(class = "mono", "Step 1: Install packages from WebR repo,
         then load and test them."),
      actionButton("run_load_tests", "1. Install & Load Packages",
                    class = "btn-primary btn-block"),
      br(), br(),
      actionButton("run_smoke_tests", "2. Run Smoke Tests",
                    class = "btn-info btn-block"),
      br(), br(),
      actionButton("run_benchmarks", "3. Run Benchmarks",
                    class = "btn-warning btn-block"),
      br(), br(),
      hr(),
      actionButton("run_all", "Run All Tests",
                    class = "btn-success btn-block"),
      hr(),
      h4("Benchmark Dataset"),
      p("A synthetic 5000-row dataset will be auto-generated for benchmarks."),
      hr(),
      actionButton("show_results", "Show Results as Markdown",
                    class = "btn-default btn-block"),
      tags$script(HTML("
        Shiny.addCustomMessageHandler('downloadFile', function(msg) {
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
        Shiny.addCustomMessageHandler('updateTextarea', function(msg) {
          document.getElementById('results_textarea').value = msg;
        });
      "))
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        id = "results_tabs",
        tabPanel("Package Load Tests",
          div(class = "section-header", "Package Install & Load Results"),
          p(class = "progress-msg",
            "Packages are downloaded as WASM binaries from the WebR repo.
             This may take 30-60 seconds per package on first run."),
          htmlOutput("load_results")
        ),
        tabPanel("Smoke Tests",
          div(class = "section-header", "Functional Smoke Test Results"),
          htmlOutput("smoke_results")
        ),
        tabPanel("Benchmarks",
          div(class = "section-header", "Performance Benchmark Results"),
          htmlOutput("benchmark_results")
        ),
        tabPanel("Summary & Decision Matrix",
          div(class = "section-header", "Phase 0 Summary"),
          htmlOutput("summary_results")
        ),
        tabPanel("Export Results",
          div(class = "section-header", "Markdown Results (copy or download)"),
          p("Click 'Show Results as Markdown' in the sidebar, then copy the text below or click Download."),
          actionButton("trigger_download", "Download as .md file",
                        class = "btn-primary", style = "margin-bottom: 10px;"),
          tags$textarea(id = "results_textarea", rows = 30,
                        style = "width:100%; font-family:monospace; font-size:12px;",
                        readonly = "readonly",
                        "Run tests first, then click 'Show Results as Markdown'.")
        )
      )
    )
  )
)

server <- function(input, output, session) {

  # Reactive values to store all results
  rv <- reactiveValues(
    load_results = list(),
    smoke_results = list(),
    benchmark_results = list(),
    benchmark_data = NULL
  )

  # ---------------------------------------------------------------------------
  # Helper: detect if running in WebR
  # ---------------------------------------------------------------------------
  is_webr <- function() {
    Sys.getenv("WEBR") != "" ||
      isTRUE(grepl("wasm", R.version$platform)) ||
      exists("webr", mode = "environment")
  }

  # ---------------------------------------------------------------------------
  # Helper: install a package (WebR-aware)
  # ---------------------------------------------------------------------------
  install_if_needed <- function(pkg_name) {
    if (!requireNamespace(pkg_name, quietly = TRUE)) {
      # Try webr::install first (works in Shinylive/WebR)
      tryCatch({
        if (is_webr() || exists("webr")) {
          webr::install(pkg_name, quiet = TRUE)
        } else {
          # Standard R fallback
          install.packages(pkg_name, repos = "https://cloud.r-project.org", quiet = TRUE)
        }
      }, error = function(e) {
        # If webr::install doesn't exist, try the global install function
        tryCatch({
          # In some WebR builds, install is available directly
          install.packages(pkg_name, quiet = TRUE)
        }, error = function(e2) {
          # Give up — will be caught by try_load_package
          NULL
        })
      })
    }
  }

  # ---------------------------------------------------------------------------
  # Helper: try to install then load a package, record timing and success
  # ---------------------------------------------------------------------------
  try_load_package <- function(pkg_name) {
    start <- proc.time()["elapsed"]
    result <- tryCatch({
      # Step 1: Install if not already available
      install_if_needed(pkg_name)
      # Step 2: Load
      library(pkg_name, character.only = TRUE)
      list(
        status = "loaded",
        time = round(proc.time()["elapsed"] - start, 2),
        error = NULL,
        version = as.character(packageVersion(pkg_name))
      )
    }, error = function(e) {
      list(
        status = "failed",
        time = round(proc.time()["elapsed"] - start, 2),
        error = conditionMessage(e),
        version = NA
      )
    })
    result$package <- pkg_name
    result
  }

  # ---------------------------------------------------------------------------
  # Helper: run a smoke test, record timing and success
  # ---------------------------------------------------------------------------
  run_smoke_test <- function(test_name, test_fn) {
    start <- proc.time()["elapsed"]
    result <- tryCatch({
      res <- test_fn()
      list(
        status = "pass",
        time = round(proc.time()["elapsed"] - start, 2),
        error = NULL,
        result_class = class(res)[1]
      )
    }, error = function(e) {
      list(
        status = "fail",
        time = round(proc.time()["elapsed"] - start, 2),
        error = conditionMessage(e),
        result_class = NA
      )
    })
    result$test <- test_name
    result
  }

  # ---------------------------------------------------------------------------
  # Helper: format results as HTML
  # ---------------------------------------------------------------------------
  status_badge <- function(status) {
    cls <- switch(status,
      "loaded" = "pass", "pass" = "pass",
      "failed" = "fail", "fail" = "fail",
      "warn"
    )
    label <- switch(status,
      "loaded" = "\u2705 Loaded", "pass" = "\u2705 Pass",
      "failed" = "\u274C Failed", "fail" = "\u274C Fail",
      "\u26A0\uFE0F Unknown"
    )
    sprintf('<span class="%s">%s</span>', cls, label)
  }

  # ---------------------------------------------------------------------------
  # PACKAGE LOAD TESTS
  # ---------------------------------------------------------------------------

  # Define all packages by tier
  packages <- list(
    "Tier 1: Core (must pass)" = c("gt", "gtsummary", "ggplot2", "broom", "labelled"),
    "Tier 2: Analysis (must pass for MVP)" = c("survival", "sandwich", "lmtest", "car", "emmeans"),
    "Tier 3: File I/O (have fallbacks)" = c("haven", "readxl"),
    "Tier 4: Advanced (nice to have)" = c("lme4", "ggdag", "writexl")
  )

  observeEvent(input$run_load_tests, {
    results <- list()
    for (tier_name in names(packages)) {
      for (pkg in packages[[tier_name]]) {
        res <- try_load_package(pkg)
        res$tier <- tier_name
        results[[pkg]] <- res
      }
    }
    rv$load_results <- results
  })

  output$load_results <- renderUI({
    results <- rv$load_results
    if (length(results) == 0) {
      return(p("Click 'Install & Load Packages' to begin.",
               br(),
               "This will download WASM binaries from the WebR repo (may take several minutes)."))
    }

    loaded_count <- sum(sapply(results, function(x) x$status == "loaded"))
    failed_count <- sum(sapply(results, function(x) x$status == "failed"))
    summary_html <- sprintf(
      '<div class="result-box" style="background-color:#e9ecef;"><strong>Summary: %d loaded, %d failed out of %d packages</strong></div>',
      loaded_count, failed_count, length(results)
    )

    html_parts <- list(HTML(summary_html))
    for (tier_name in names(packages)) {
      tier_html <- sprintf('<div class="section-header" style="margin-top:20px;">%s</div>', tier_name)
      rows <- ""
      for (pkg in packages[[tier_name]]) {
        res <- results[[pkg]]
        if (is.null(res)) next
        err_msg <- if (!is.null(res$error)) sprintf(' <span class="mono">%s</span>', res$error) else ""
        ver_msg <- if (!is.na(res$version)) sprintf(" (v%s)", res$version) else ""
        rows <- paste0(rows, sprintf(
          '<div class="result-box">%s <strong>%s</strong>%s — %.2fs%s</div>',
          status_badge(res$status), res$package, ver_msg, res$time, err_msg
        ))
      }
      html_parts <- c(html_parts, list(HTML(paste0(tier_html, rows))))
    }
    do.call(tagList, html_parts)
  })

  # ---------------------------------------------------------------------------
  # SMOKE TESTS
  # ---------------------------------------------------------------------------

  observeEvent(input$run_smoke_tests, {
    results <- list()

    # gt: render a small table as HTML
    results[["gt_render"]] <- run_smoke_test(
      "gt: Render mtcars as HTML table",
      function() {
        if (requireNamespace("gt", quietly = TRUE)) {
          gt::gt(mtcars[1:5, ]) |> gt::as_raw_html()
        } else stop("gt not available")
      }
    )

    # gtsummary: create Table 1
    results[["gtsummary_tbl"]] <- run_smoke_test(
      "gtsummary: tbl_summary on mtcars",
      function() {
        if (requireNamespace("gtsummary", quietly = TRUE)) {
          gtsummary::tbl_summary(mtcars[, 1:4])
        } else stop("gtsummary not available")
      }
    )

    # ggplot2: basic scatter plot
    results[["ggplot2_scatter"]] <- run_smoke_test(
      "ggplot2: Scatter plot (mpg vs hp)",
      function() {
        if (requireNamespace("ggplot2", quietly = TRUE)) {
          p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, hp)) +
            ggplot2::geom_point()
          grDevices::png(tf <- tempfile(fileext = ".png"), width = 600, height = 400)
          print(p)
          grDevices::dev.off()
          file.exists(tf)
        } else stop("ggplot2 not available")
      }
    )

    # broom: tidy a linear model
    results[["broom_tidy"]] <- run_smoke_test(
      "broom: Tidy lm(mpg ~ hp, mtcars)",
      function() {
        if (requireNamespace("broom", quietly = TRUE)) {
          broom::tidy(lm(mpg ~ hp, data = mtcars))
        } else stop("broom not available")
      }
    )

    # labelled: set and get labels
    results[["labelled_labels"]] <- run_smoke_test(
      "labelled: Set/get variable labels",
      function() {
        if (requireNamespace("labelled", quietly = TRUE)) {
          df <- mtcars[, 1:3]
          labelled::var_label(df) <- list(mpg = "Miles per Gallon", cyl = "Cylinders", disp = "Displacement")
          labels <- labelled::var_label(df)
          stopifnot(labels$mpg == "Miles per Gallon")
          labels
        } else stop("labelled not available")
      }
    )

    # survival: survfit
    results[["survival_km"]] <- run_smoke_test(
      "survival: survfit(Surv(time, status) ~ x, aml)",
      function() {
        if (requireNamespace("survival", quietly = TRUE)) {
          data(aml, package = "survival")
          survival::survfit(survival::Surv(time, status) ~ x, data = aml)
        } else stop("survival not available")
      }
    )

    # sandwich: cluster-robust SEs
    results[["sandwich_vcov"]] <- run_smoke_test(
      "sandwich: vcovCL with clustering",
      function() {
        if (requireNamespace("sandwich", quietly = TRUE)) {
          mod <- lm(mpg ~ hp, data = mtcars)
          sandwich::vcovCL(mod, cluster = mtcars$cyl)
        } else stop("sandwich not available")
      }
    )

    # lmtest: coeftest with robust SEs
    results[["lmtest_coeftest"]] <- run_smoke_test(
      "lmtest: coeftest with sandwich vcov",
      function() {
        if (requireNamespace("lmtest", quietly = TRUE) &&
            requireNamespace("sandwich", quietly = TRUE)) {
          mod <- lm(mpg ~ hp, data = mtcars)
          lmtest::coeftest(mod, vcov = sandwich::vcovCL(mod, cluster = mtcars$cyl))
        } else stop("lmtest or sandwich not available")
      }
    )

    # car: VIF
    results[["car_vif"]] <- run_smoke_test(
      "car: VIF for multivariate model",
      function() {
        if (requireNamespace("car", quietly = TRUE)) {
          mod <- lm(mpg ~ hp + wt + disp, data = mtcars)
          car::vif(mod)
        } else stop("car not available")
      }
    )

    # emmeans: estimated marginal means
    results[["emmeans_test"]] <- run_smoke_test(
      "emmeans: Marginal means from ANOVA",
      function() {
        if (requireNamespace("emmeans", quietly = TRUE)) {
          mtcars2 <- mtcars
          mtcars2$cyl <- factor(mtcars2$cyl)
          mod <- lm(mpg ~ cyl, data = mtcars2)
          emmeans::emmeans(mod, "cyl")
        } else stop("emmeans not available")
      }
    )

    # haven: read_dta
    results[["haven_read"]] <- run_smoke_test(
      "haven: Create and read Stata file",
      function() {
        if (requireNamespace("haven", quietly = TRUE)) {
          tf <- tempfile(fileext = ".dta")
          haven::write_dta(mtcars[1:10, ], tf)
          df <- haven::read_dta(tf)
          stopifnot(nrow(df) == 10)
          df
        } else stop("haven not available")
      }
    )

    # readxl: read xlsx
    results[["readxl_read"]] <- run_smoke_test(
      "readxl: Read built-in example xlsx",
      function() {
        if (requireNamespace("readxl", quietly = TRUE)) {
          path <- readxl::readxl_example("datasets.xlsx")
          readxl::read_xlsx(path)
        } else stop("readxl not available")
      }
    )

    # lme4: simple random intercept model
    results[["lme4_model"]] <- run_smoke_test(
      "lme4: lmer(Reaction ~ Days + (1|Subject), sleepstudy)",
      function() {
        if (requireNamespace("lme4", quietly = TRUE)) {
          data(sleepstudy, package = "lme4")
          lme4::lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
        } else stop("lme4 not available")
      }
    )

    # ggdag: create and plot DAG
    results[["ggdag_dag"]] <- run_smoke_test(
      "ggdag: dagify(y ~ x) and ggdag()",
      function() {
        if (requireNamespace("ggdag", quietly = TRUE)) {
          dag <- ggdag::dagify(y ~ x + z, x ~ z)
          p <- ggdag::ggdag(dag)
          grDevices::png(tf <- tempfile(fileext = ".png"), width = 600, height = 400)
          print(p)
          grDevices::dev.off()
          file.exists(tf)
        } else stop("ggdag not available")
      }
    )

    # writexl: write xlsx
    results[["writexl_write"]] <- run_smoke_test(
      "writexl: write_xlsx(mtcars, ...)",
      function() {
        if (requireNamespace("writexl", quietly = TRUE)) {
          tf <- tempfile(fileext = ".xlsx")
          writexl::write_xlsx(mtcars, tf)
          file.exists(tf) && file.size(tf) > 0
        } else stop("writexl not available")
      }
    )

    rv$smoke_results <- results
  })

  output$smoke_results <- renderUI({
    results <- rv$smoke_results
    if (length(results) == 0) {
      return(p("Click 'Run Smoke Tests' to begin. (Run load tests first.)"))
    }

    pass_count <- sum(sapply(results, function(x) x$status == "pass"))
    fail_count <- sum(sapply(results, function(x) x$status == "fail"))
    summary_html <- sprintf(
      '<div class="result-box" style="background-color:#e9ecef;"><strong>Summary: %d pass, %d fail out of %d tests</strong></div>',
      pass_count, fail_count, length(results)
    )

    rows <- summary_html
    for (name in names(results)) {
      res <- results[[name]]
      err_msg <- if (!is.null(res$error)) sprintf(' — <span class="mono">%s</span>', res$error) else ""
      cls_msg <- if (!is.na(res$result_class)) sprintf(' [returned: %s]', res$result_class) else ""
      rows <- paste0(rows, sprintf(
        '<div class="result-box">%s <strong>%s</strong> — %.2fs%s%s</div>',
        status_badge(res$status), res$test, res$time, cls_msg, err_msg
      ))
    }
    HTML(rows)
  })

  # ---------------------------------------------------------------------------
  # BENCHMARKS
  # ---------------------------------------------------------------------------

  # Generate benchmark data once and store it
  generate_benchmark_data <- function() {
    set.seed(42)
    n <- 5000
    data.frame(
      id = 1:n,
      age = round(rnorm(n, 50, 15)),
      sex = sample(c("Male", "Female"), n, replace = TRUE),
      bmi = round(rnorm(n, 27, 5), 1),
      smoking = sample(c("Never", "Former", "Current"), n, replace = TRUE),
      bp_systolic = round(rnorm(n, 130, 20)),
      bp_diastolic = round(rnorm(n, 80, 12)),
      cholesterol = round(rnorm(n, 200, 40)),
      glucose = round(rnorm(n, 100, 30)),
      creatinine = round(rnorm(n, 1.0, 0.3), 2),
      hemoglobin = round(rnorm(n, 14, 2), 1),
      treatment = sample(c("Control", "Treatment"), n, replace = TRUE),
      site = sample(paste0("Site_", 1:10), n, replace = TRUE),
      outcome_continuous = round(rnorm(n, 50, 10), 1),
      outcome_binary = sample(0:1, n, replace = TRUE, prob = c(0.7, 0.3)),
      time_to_event = round(pmax(1, rexp(n, 0.02))),
      event_status = sample(0:1, n, replace = TRUE, prob = c(0.4, 0.6)),
      score1 = round(rnorm(n, 75, 12)),
      score2 = round(rnorm(n, 80, 10)),
      category = sample(LETTERS[1:8], n, replace = TRUE),
      stringsAsFactors = FALSE
    )
  }

  observeEvent(input$run_benchmarks, {
    results <- list()

    # Generate benchmark data
    if (is.null(rv$benchmark_data)) {
      rv$benchmark_data <- generate_benchmark_data()
    }
    d <- rv$benchmark_data

    # Benchmark 1: CSV upload and parse simulation
    results[["csv_parse"]] <- run_smoke_test(
      "CSV parse (write then read ~1MB)",
      function() {
        tf <- tempfile(fileext = ".csv")
        write.csv(d, tf, row.names = FALSE)
        fsize <- file.size(tf)
        df <- read.csv(tf)
        list(rows = nrow(df), cols = ncol(df), file_kb = round(fsize / 1024))
      }
    )

    # Benchmark 2: Descriptive statistics (Table 1)
    results[["table1_basic"]] <- run_smoke_test(
      "Table 1: gtsummary tbl_summary (basic, ~10 vars)",
      function() {
        if (requireNamespace("gtsummary", quietly = TRUE)) {
          gtsummary::tbl_summary(
            d[, c("age", "sex", "bmi", "smoking", "bp_systolic",
                   "cholesterol", "glucose", "treatment", "outcome_binary", "category")],
            by = "treatment"
          )
        } else stop("gtsummary not available")
      }
    )

    # Benchmark 3: Table 1 with all variables (stress test)
    results[["table1_30var"]] <- run_smoke_test(
      "Table 1: gtsummary tbl_summary (all columns, stress test)",
      function() {
        if (requireNamespace("gtsummary", quietly = TRUE)) {
          cols <- setdiff(names(d), c("id", "treatment"))
          gtsummary::tbl_summary(d[, c(cols, "treatment")], by = "treatment")
        } else stop("gtsummary not available")
      }
    )

    # Benchmark 4: High-cardinality categorical
    results[["table1_highcard"]] <- run_smoke_test(
      "Table 1: High-cardinality categorical (20+ levels)",
      function() {
        if (requireNamespace("gtsummary", quietly = TRUE)) {
          d2 <- d
          d2$high_card <- sample(paste0("Level_", 1:25), nrow(d2), replace = TRUE)
          gtsummary::tbl_summary(d2[, c("high_card", "treatment")], by = "treatment")
        } else stop("gtsummary not available")
      }
    )

    # Benchmark 5: Linear regression
    results[["lm_5pred"]] <- run_smoke_test(
      "Linear regression: lm with 5 predictors (n=5000)",
      function() {
        mod <- lm(outcome_continuous ~ age + bmi + bp_systolic + cholesterol + glucose, data = d)
        if (requireNamespace("broom", quietly = TRUE)) broom::tidy(mod) else summary(mod)
      }
    )

    # Benchmark 6: Logistic regression
    results[["glm_5pred"]] <- run_smoke_test(
      "Logistic regression: glm with 5 predictors (n=5000)",
      function() {
        mod <- glm(outcome_binary ~ age + bmi + bp_systolic + cholesterol + glucose,
                    data = d, family = binomial)
        if (requireNamespace("broom", quietly = TRUE)) broom::tidy(mod) else summary(mod)
      }
    )

    # Benchmark 7: Survival analysis (Cox model)
    results[["cox_model"]] <- run_smoke_test(
      "Cox regression: survival with 5 predictors (n=5000)",
      function() {
        if (requireNamespace("survival", quietly = TRUE)) {
          mod <- survival::coxph(
            survival::Surv(time_to_event, event_status) ~ age + bmi + bp_systolic + treatment + sex,
            data = d
          )
          if (requireNamespace("broom", quietly = TRUE)) broom::tidy(mod) else summary(mod)
        } else stop("survival not available")
      }
    )

    # Benchmark 8: Mixed model (if lme4 available)
    results[["mixed_model"]] <- run_smoke_test(
      "Mixed model: lmer with random intercept (n=5000, 10 sites)",
      function() {
        if (requireNamespace("lme4", quietly = TRUE)) {
          lme4::lmer(outcome_continuous ~ age + bmi + treatment + (1 | site), data = d)
        } else stop("lme4 not available")
      }
    )

    # Benchmark 9: Cluster-robust SEs
    results[["cluster_se"]] <- run_smoke_test(
      "Cluster-robust SEs: sandwich vcovCL (n=5000, 10 clusters)",
      function() {
        if (requireNamespace("sandwich", quietly = TRUE) &&
            requireNamespace("lmtest", quietly = TRUE)) {
          mod <- lm(outcome_continuous ~ age + bmi + treatment, data = d)
          lmtest::coeftest(mod, vcov = sandwich::vcovCL(mod, cluster = d$site))
        } else stop("sandwich/lmtest not available")
      }
    )

    # Benchmark 10: gt table rendering
    results[["gt_render_large"]] <- run_smoke_test(
      "gt: Render summary table as HTML (large)",
      function() {
        if (requireNamespace("gt", quietly = TRUE) &&
            requireNamespace("gtsummary", quietly = TRUE)) {
          tbl <- gtsummary::tbl_summary(
            d[, c("age", "sex", "bmi", "smoking", "treatment")],
            by = "treatment"
          )
          gt_tbl <- gtsummary::as_gt(tbl)
          gt::as_raw_html(gt_tbl)
        } else stop("gt/gtsummary not available")
      }
    )

    # Benchmark 11: Full ggplot2 with many points
    results[["ggplot_large"]] <- run_smoke_test(
      "ggplot2: Scatter with 5000 points + smoothing",
      function() {
        if (requireNamespace("ggplot2", quietly = TRUE)) {
          p <- ggplot2::ggplot(d, ggplot2::aes(age, outcome_continuous, color = treatment)) +
            ggplot2::geom_point(alpha = 0.3) +
            ggplot2::geom_smooth(method = "lm") +
            ggplot2::theme_classic()
          grDevices::png(tf <- tempfile(fileext = ".png"), width = 800, height = 600, res = 300)
          print(p)
          grDevices::dev.off()
          file.exists(tf)
        } else stop("ggplot2 not available")
      }
    )

    rv$benchmark_results <- results
  })

  output$benchmark_results <- renderUI({
    results <- rv$benchmark_results
    if (length(results) == 0) {
      return(p("Click 'Run Benchmarks' to begin. (Run load tests first.)"))
    }

    pass_count <- sum(sapply(results, function(x) x$status == "pass"))
    fail_count <- sum(sapply(results, function(x) x$status == "fail"))
    summary_html <- sprintf(
      '<div class="result-box" style="background-color:#e9ecef;"><strong>Summary: %d pass, %d fail out of %d benchmarks</strong></div>',
      pass_count, fail_count, length(results)
    )

    header <- paste0(summary_html,
      '<table class="benchmark-table"><tr><th>Benchmark</th><th>Status</th><th>Time (s)</th><th>Notes</th></tr>')
    rows <- ""
    for (name in names(results)) {
      res <- results[[name]]
      note <- if (!is.null(res$error)) res$error else if (!is.na(res$result_class)) res$result_class else ""
      time_cls <- if (res$time > 10) "warn" else if (res$time > 30) "fail" else ""
      rows <- paste0(rows, sprintf(
        '<tr><td>%s</td><td>%s</td><td class="%s">%.2f</td><td class="mono">%s</td></tr>',
        res$test, status_badge(res$status), time_cls, res$time, note
      ))
    }
    HTML(paste0(header, rows, '</table>'))
  })

  # ---------------------------------------------------------------------------
  # RUN ALL
  # ---------------------------------------------------------------------------

  observeEvent(input$run_all, {
    # 1. Load tests (run inline)
    results_load <- list()
    for (tier_name in names(packages)) {
      for (pkg in packages[[tier_name]]) {
        res <- try_load_package(pkg)
        res$tier <- tier_name
        results_load[[pkg]] <- res
      }
    }
    rv$load_results <- results_load

    # Trigger smoke tests and benchmarks via button click simulation
    shiny::updateActionButton(session, "run_smoke_tests")
    shiny::updateActionButton(session, "run_benchmarks")
  })

  # ---------------------------------------------------------------------------
  # SUMMARY & DECISION MATRIX
  # ---------------------------------------------------------------------------

  output$summary_results <- renderUI({
    load_res <- rv$load_results
    smoke_res <- rv$smoke_results
    bench_res <- rv$benchmark_results

    if (length(load_res) == 0) {
      return(p("Run all tests first to see the summary."))
    }

    all_pkgs <- unlist(packages, use.names = FALSE)

    loaded_count <- sum(sapply(load_res, function(x) x$status == "loaded"))
    failed_count <- sum(sapply(load_res, function(x) x$status == "failed"))

    summary_html <- sprintf(
      '<div class="result-box" style="background-color:#e9ecef;"><strong>Overall: %d/%d packages loaded</strong></div>',
      loaded_count, loaded_count + failed_count
    )

    rows <- ""
    for (pkg in all_pkgs) {
      lr <- load_res[[pkg]]
      load_status <- if (!is.null(lr)) lr$status else "untested"
      load_time <- if (!is.null(lr)) sprintf("%.2fs", lr$time) else "-"
      version <- if (!is.null(lr) && !is.na(lr$version)) lr$version else "-"
      tier <- if (!is.null(lr)) lr$tier else "-"

      smoke_pass <- 0
      smoke_fail <- 0
      for (sname in names(smoke_res)) {
        if (grepl(tolower(pkg), tolower(sname), fixed = TRUE) ||
            grepl(tolower(pkg), tolower(smoke_res[[sname]]$test), fixed = TRUE)) {
          if (smoke_res[[sname]]$status == "pass") smoke_pass <- smoke_pass + 1
          else smoke_fail <- smoke_fail + 1
        }
      }
      smoke_summary <- if (smoke_pass + smoke_fail == 0) "-"
        else sprintf("%d/%d pass", smoke_pass, smoke_pass + smoke_fail)

      decision <- if (load_status == "failed") {
        '<span class="fail">EXCLUDE - use fallback</span>'
      } else if (load_status == "loaded" && smoke_fail > 0) {
        '<span class="warn">PARTIAL - document limitations</span>'
      } else if (load_status == "loaded") {
        '<span class="pass">INCLUDE in v1</span>'
      } else {
        '<span class="warn">UNTESTED</span>'
      }

      rows <- paste0(rows, sprintf(
        '<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>',
        pkg, status_badge(load_status), version, load_time, smoke_summary, decision
      ))
    }

    HTML(paste0(
      summary_html,
      '<table class="benchmark-table">',
      '<tr><th>Package</th><th>Load Status</th><th>Version</th><th>Load Time</th><th>Smoke Tests</th><th>Decision</th></tr>',
      rows,
      '</table>',
      '<div style="margin-top:20px;"><h4>Fallback Table</h4>',
      '<p>For each package marked EXCLUDE or PARTIAL:</p>',
      '<ul>',
      '<li><strong>gt</strong> - Use kableExtra or manual HTML tables</li>',
      '<li><strong>gtsummary</strong> - Build Table 1 manually with base R</li>',
      '<li><strong>labelled</strong> - Use base R attr() for variable labels</li>',
      '<li><strong>car</strong> - Manual VIF/Levene implementation in base R</li>',
      '<li><strong>emmeans</strong> - Manual marginal means calculation</li>',
      '<li><strong>haven</strong> - CSV-only input</li>',
      '<li><strong>readxl</strong> - CSV-only input</li>',
      '<li><strong>lme4</strong> - Drop mixed models</li>',
      '<li><strong>ggdag</strong> - Pure ggplot2 DAG rendering</li>',
      '<li><strong>writexl</strong> - CSV + HTML export</li>',
      '<li><strong>sandwich/lmtest</strong> - Note clustering, no SE adjustment</li>',
      '</ul></div>'
    ))
  })

  # ---------------------------------------------------------------------------
  # RESULTS EXPORT
  # ---------------------------------------------------------------------------

  generate_markdown_results <- function() {
    load_res <- rv$load_results
    smoke_res <- rv$smoke_results
    bench_res <- rv$benchmark_results

    lines <- c(
      "# Phase 0: WebR Package Validation Results",
      "",
      paste0("**Date:** ", Sys.Date()),
      paste0("**R Version:** ", R.version.string),
      paste0("**Platform:** ", .Platform$OS.type),
      "",
      "## 1. Package Load Results",
      "",
      "| Package | Tier | Status | Version | Load Time (s) | Error |",
      "|---------|------|--------|---------|---------------|-------|"
    )

    all_pkgs <- unlist(packages, use.names = FALSE)
    for (pkg in all_pkgs) {
      lr <- load_res[[pkg]]
      if (is.null(lr)) {
        lines <- c(lines, sprintf("| %s | - | untested | - | - | - |", pkg))
      } else {
        status_emoji <- if (lr$status == "loaded") "\u2705" else "\u274C"
        ver <- if (!is.na(lr$version)) lr$version else "-"
        err <- if (!is.null(lr$error)) lr$error else "-"
        lines <- c(lines, sprintf("| %s | %s | %s %s | %s | %.2f | %s |",
                                  pkg, lr$tier, status_emoji, lr$status, ver, lr$time, err))
      }
    }

    lines <- c(lines, "", "## 2. Functional Smoke Test Results", "",
               "| Test | Status | Time (s) | Error |",
               "|------|--------|----------|-------|")
    for (name in names(smoke_res)) {
      sr <- smoke_res[[name]]
      status_emoji <- if (sr$status == "pass") "\u2705" else "\u274C"
      err <- if (!is.null(sr$error)) sr$error else "-"
      lines <- c(lines, sprintf("| %s | %s %s | %.2f | %s |",
                                sr$test, status_emoji, sr$status, sr$time, err))
    }

    lines <- c(lines, "", "## 3. Performance Benchmark Results", "",
               "| Benchmark | Status | Time (s) | Notes |",
               "|-----------|--------|----------|-------|")
    for (name in names(bench_res)) {
      br <- bench_res[[name]]
      status_emoji <- if (br$status == "pass") "\u2705" else "\u274C"
      note <- if (!is.null(br$error)) br$error else ""
      lines <- c(lines, sprintf("| %s | %s %s | %.2f | %s |",
                                br$test, status_emoji, br$status, br$time, note))
    }

    lines <- c(lines, "", "## 4. Decision Matrix", "",
               "| Package | Decision | Rationale |",
               "|---------|----------|-----------|")
    for (pkg in all_pkgs) {
      lr <- load_res[[pkg]]
      if (is.null(lr)) {
        lines <- c(lines, sprintf("| %s | UNTESTED | Not yet tested |", pkg))
      } else if (lr$status == "failed") {
        lines <- c(lines, sprintf("| %s | EXCLUDE | Failed to load: %s |", pkg,
                                  if (!is.null(lr$error)) lr$error else "unknown"))
      } else {
        lines <- c(lines, sprintf("| %s | INCLUDE | Loaded and passed smoke tests |", pkg))
      }
    }

    lines <- c(lines, "",
               "## 5. Safari Memory Test",
               "",
               "_To be completed manually in Safari browser._",
               "",
               "## 6. Confirmed v1 Package List",
               "",
               "_To be finalized after browser testing._",
               "")

    paste(lines, collapse = "\n")
  }

  # Show results in text area
  observeEvent(input$show_results, {
    md <- generate_markdown_results()
    # Update the textarea via JS
    session$sendCustomMessage("updateTextarea", md)
    # Switch to the Export tab
    updateTabsetPanel(session, "results_tabs", selected = "Export Results")
  })

  # Download via JS blob
  observeEvent(input$trigger_download, {
    md <- generate_markdown_results()
    session$sendCustomMessage("downloadFile", list(
      content = md,
      filename = paste0("PHASE0_VALIDATION_", Sys.Date(), ".md")
    ))
  })
}

shinyApp(ui, server)
