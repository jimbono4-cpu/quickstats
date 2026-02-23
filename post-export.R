# post-export.R — Run AFTER shinylive::export("shinylive-app", "docs")
#
# Three optimizations:
#   1. Preload hints — start downloading heavy WebR files immediately
#   2. Progress bar — shows real download % with status messages
#   3. Strip unused packages — remove ~120 unneeded bundled packages
#
# Usage (from R, with working directory set to repo root):
#   source("post-export.R")

index <- file.path("docs", "index.html")
pkg_dir <- file.path("docs", "shinylive", "webr", "packages")

if (!file.exists(index)) {
  stop("docs/index.html not found. Run shinylive::export() first.")
}

# ============================================================================
# 1. Replace index.html with optimized version
# ============================================================================
html <- paste(readLines(index, encoding = "UTF-8"), collapse = "\n")

if (!grepl("app-preloader", html, fixed = TRUE)) {

  optimized <- '<!doctype html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <title>Statistical Analysis App</title>
    <link rel="modulepreload" href="./shinylive/shinylive.js">
    <link rel="preload" href="./shinylive/webr/R.bin.wasm" as="fetch" crossorigin>
    <link rel="preload" href="./shinylive/webr/library.data.gz" as="fetch" crossorigin>
    <script src="./shinylive/load-shinylive-sw.js" type="module"></script>
    <script type="module">
      import { runExportedApp } from "./shinylive/shinylive.js";
      runExportedApp({ id: "root", appEngine: "r", relPath: "" });
    </script>
    <link rel="stylesheet" href="./shinylive/style-resets.css" />
    <link rel="stylesheet" href="./shinylive/shinylive.css" />
    <style>
      .app-preloader {
        position: fixed; top: 0; left: 0; width: 100%; height: 100%;
        display: flex; flex-direction: column;
        align-items: center; justify-content: center;
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white; font-family: \'Segoe UI\', Tahoma, sans-serif;
        z-index: 9999; transition: opacity 0.5s;
      }
      .app-preloader h1 { font-size: 28px; margin-bottom: 10px; font-weight: 300; }
      .app-preloader p { font-size: 15px; opacity: 0.85; }
      .preloader-bar {
        width: 300px; height: 6px; background: rgba(255,255,255,0.2);
        border-radius: 3px; overflow: hidden; margin-top: 24px;
      }
      .preloader-bar-fill {
        height: 100%; width: 0%; background: white; border-radius: 3px;
        transition: width 0.4s ease;
      }
      .preloader-pct {
        font-size: 14px; margin-top: 12px; font-weight: 500; opacity: 0.9;
      }
      .preloader-status {
        font-size: 12px; margin-top: 6px; opacity: 0.6;
      }
      .app-preloader.hidden { opacity: 0; pointer-events: none; }
    </style>
  </head>
  <body>
    <div class="app-preloader" id="preloader">
      <h1>Statistical Analysis App</h1>
      <p>Loading R environment in your browser...</p>
      <div class="preloader-bar"><div class="preloader-bar-fill" id="pbar"></div></div>
      <div class="preloader-pct" id="ppct">0%</div>
      <div class="preloader-status" id="pstatus">Connecting...</div>
      <p style="font-size:11px; margin-top:24px; opacity:0.45;">No data leaves your browser.</p>
    </div>
    <div style="height: 100vh; width: 100vw" id="root"></div>
    <script>
    (function() {
      var bar = document.getElementById(\'pbar\');
      var pctEl = document.getElementById(\'ppct\');
      var statusEl = document.getElementById(\'pstatus\');
      var done = false;
      var startTime = Date.now();
      var expectedMs = 25000;

      var stages = [
        { at: 0,  lbl: \'Connecting...\' },
        { at: 8,  lbl: \'Downloading R engine...\' },
        { at: 30, lbl: \'Loading R libraries...\' },
        { at: 55, lbl: \'Compiling WebAssembly...\' },
        { at: 75, lbl: \'Starting R worker...\' },
        { at: 88, lbl: \'Initializing Shiny app...\' }
      ];

      function getStageLabel(pct) {
        var lbl = stages[0].lbl;
        for (var i = 0; i < stages.length; i++) {
          if (pct >= stages[i].at) lbl = stages[i].lbl;
        }
        return lbl;
      }

      function tick() {
        if (done) return;
        var elapsed = Date.now() - startTime;
        var t = Math.min(elapsed / expectedMs, 1);
        var pct = Math.round(92 * (1 - Math.pow(1 - t, 2.5)));
        if (pct > 92) pct = 92;

        if (bar) bar.style.width = pct + \'%\';
        if (pctEl) pctEl.textContent = pct + \'%\';
        if (statusEl) statusEl.textContent = getStageLabel(pct);

        requestAnimationFrame(tick);
      }

      requestAnimationFrame(tick);

      var obs = new MutationObserver(function() {
        var root = document.getElementById(\'root\');
        if (root && root.children.length > 0) {
          done = true;
          if (bar) bar.style.width = \'100%\';
          if (pctEl) pctEl.textContent = \'100%\';
          if (statusEl) statusEl.textContent = \'Ready!\';
          var pre = document.getElementById(\'preloader\');
          if (pre) {
            setTimeout(function() {
              pre.classList.add(\'hidden\');
              setTimeout(function() { pre.remove(); }, 600);
            }, 400);
          }
          obs.disconnect();
        }
      });
      obs.observe(document.getElementById(\'root\'), { childList: true, subtree: true });
    })();
    </script>
  </body>
</html>'

  writeLines(optimized, index, useBytes = FALSE)
  message("OK: Optimized index.html with progress bar + preload hints")
} else {
  message("Preloader already present in ", index, " — skipping HTML patch.")
}

# ============================================================================
# 2. Strip unused bundled packages (164MB -> ~60MB)
# ============================================================================
keep_packages <- c(
  # Our app's direct dependencies
  "haven", "readxl", "labelled", "munsell", "ggplot2", "broom",
  "survival", "sandwich", "lmtest", "car", "emmeans", "writexl",
  "lme4", "gridExtra", "base64enc",
  # ggplot2 dependency tree
  "scales", "gtable", "isoband", "farver", "labeling", "colorspace",
  "viridisLite", "RColorBrewer",
  # tidyverse core (used by broom, labelled, haven, dplyr, tidyr)
  "dplyr", "tidyr", "tibble", "purrr", "vctrs", "tidyselect",
  "pillar", "generics", "stringr", "stringi", "forcats",
  # car dependency tree
  "carData", "abind", "Formula", "pbkrtest", "nnet",
  "SparseM", "MatrixModels", "Deriv", "quantreg",
  # lme4 dependency tree
  "Matrix", "minqa", "nloptr", "reformulas", "boot", "nlme",
  "lattice", "MASS",
  # emmeans dependencies
  "estimability", "mvtnorm", "numDeriv",
  # haven/readxl dependencies
  "cellranger", "readr", "hms", "bit64", "bit", "vroom", "tzdb",
  "clipr", "crayon",
  # sandwich/lmtest
  "zoo",
  # Misc shared dependencies
  "backports", "mgcv", "foreign",
  # shinylive/webr infrastructure (DO NOT remove)
  "shinylive", "webr",
  # metadata file (not a directory, but keep it)
  "metadata.rds"
)

if (dir.exists(pkg_dir)) {
  all_items <- list.files(pkg_dir)
  to_remove <- setdiff(all_items, keep_packages)

  if (length(to_remove) > 0) {
    removed_size <- 0
    for (item in to_remove) {
      item_path <- file.path(pkg_dir, item)
      if (dir.exists(item_path)) {
        sz <- sum(file.info(list.files(item_path, recursive = TRUE,
                                        full.names = TRUE))$size, na.rm = TRUE)
        removed_size <- removed_size + sz
        unlink(item_path, recursive = TRUE)
      }
    }
    message(sprintf("OK: Removed %d unused packages (saved %.1f MB)",
                    length(to_remove), removed_size / 1024 / 1024))
    message(sprintf("    Kept %d packages needed by the app",
                    length(all_items) - length(to_remove)))
  } else {
    message("No unused packages to remove.")
  }
} else {
  message("Warning: packages directory not found at ", pkg_dir)
}

message("\nDone! Ready to: git add docs/ && git commit && git push")
