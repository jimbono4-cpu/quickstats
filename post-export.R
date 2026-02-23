# post-export.R — Run AFTER shinylive::export("shinylive-app", "docs")
#
# Three optimizations:
#   1. Preload hints — start downloading heavy WebR files immediately
#      instead of waiting for JS module chain to discover them
#   2. Preloader UI — show animated loading screen while WebR loads
#   3. Strip unused packages — remove ~120 unneeded bundled packages
#      to reduce metadata/VFS overhead
#
# Usage (from R, with working directory set to repo root):
#   source("post-export.R")

index <- file.path("docs", "index.html")
pkg_dir <- file.path("docs", "shinylive", "webr", "packages")

if (!file.exists(index)) {
  stop("docs/index.html not found. Run shinylive::export() first.")
}

# ============================================================================
# 1. Patch index.html — preload hints + preloader UI
# ============================================================================
html <- paste(readLines(index, encoding = "UTF-8"), collapse = "\n")

if (!grepl("app-preloader", html, fixed = TRUE)) {

  # Preload hints: tell browser to start downloading big files IMMEDIATELY
  # instead of waiting for the JS module waterfall to discover them.
  # R.bin.wasm (11MB) + library.data.gz (13MB) = 24MB that currently can't
  # start downloading until shinylive.js is fully parsed.
  preloads <- '    <!-- Preload heavy WebR assets to eliminate download waterfall -->
    <link rel="preload" href="./shinylive/shinylive.js" as="script" crossorigin>
    <link rel="preload" href="./shinylive/webr/R.bin.wasm" as="fetch" crossorigin>
    <link rel="preload" href="./shinylive/webr/library.data.gz" as="fetch" crossorigin>
    <link rel="preload" href="./shinylive/webr/webr-worker.js" as="script" crossorigin>
    <link rel="preload" href="./shinylive/webr/R.bin.js" as="script" crossorigin>\n'

  css <- '    <style>
      .app-preloader {
        position: fixed; top: 0; left: 0; width: 100%; height: 100%;
        display: flex; flex-direction: column;
        align-items: center; justify-content: center;
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white; font-family: \'Segoe UI\', Tahoma, sans-serif;
        z-index: 9999; transition: opacity 0.5s;
      }
      .app-preloader h1 { font-size: 28px; margin-bottom: 10px; font-weight: 300; }
      .app-preloader p { font-size: 15px; opacity: 0.85; margin-bottom: 30px; }
      .preloader-bar {
        width: 260px; height: 4px; background: rgba(255,255,255,0.25);
        border-radius: 2px; overflow: hidden;
      }
      .preloader-bar-fill {
        height: 100%; width: 30%; background: white; border-radius: 2px;
        animation: preload-slide 1.5s ease-in-out infinite;
      }
      @keyframes preload-slide {
        0% { transform: translateX(-100%); }
        100% { transform: translateX(400%); }
      }
      .app-preloader.hidden { opacity: 0; pointer-events: none; }
    </style>\n'

  div <- '    <div class="app-preloader" id="preloader">
      <h1>Statistical Analysis App</h1>
      <p>Loading R environment in your browser...</p>
      <div class="preloader-bar"><div class="preloader-bar-fill"></div></div>
      <p style="font-size:12px; margin-top:20px; opacity:0.6;">First load may take 15-30s. No data leaves your browser.</p>
    </div>\n'

  script <- '    <script>
      // Hide preloader once Shiny app renders
      const observer = new MutationObserver(function(mutations) {
        const root = document.getElementById(\'root\');
        if (root && root.children.length > 0) {
          const pre = document.getElementById(\'preloader\');
          if (pre) { pre.classList.add(\'hidden\'); setTimeout(() => pre.remove(), 600); }
          observer.disconnect();
        }
      });
      observer.observe(document.getElementById(\'root\'), { childList: true, subtree: true });
    </script>\n'

  # Inject preloads + CSS before </head>
  html <- sub("</head>", paste0(preloads, css, "  </head>"), html, fixed = TRUE)
  # Inject preloader div after <body>
  html <- sub("<body>", paste0("<body>\n", div), html, fixed = TRUE)
  # Inject MutationObserver script before </body>
  html <- sub("</body>", paste0(script, "  </body>"), html, fixed = TRUE)

  writeLines(html, index, useBytes = FALSE)
  message("OK: Preloader + preload hints injected into ", index)
} else {
  message("Preloader already present in ", index, " — skipping HTML patch.")
}

# ============================================================================
# 2. Strip unused bundled packages (164MB -> ~60MB)
# ============================================================================
# Shinylive statically detects all package references and bundles them,
# including packages we don't use (ggdag, gt, DT, forecast, etc.).
# Keep only packages our app actually needs + their dependencies.
# Anything removed here will be fetched from the WebR CDN if ever needed.

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
