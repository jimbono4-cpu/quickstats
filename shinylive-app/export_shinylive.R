#!/usr/bin/env Rscript
# Export the Shinylive test app for browser deployment
# Prerequisites: install.packages("shinylive")
#
# This script:
# 1. Exports the test_app.R as a Shinylive static site
# 2. Copies test data into the exported site
# 3. Provides instructions for serving/deploying
#
# Usage: Rscript export_shinylive.R

cat("=== Shinylive Export ===\n\n")

# Check shinylive is available
if (!requireNamespace("shinylive", quietly = TRUE)) {
  cat("ERROR: shinylive package not installed.\n")
  cat("Install with: install.packages('shinylive')\n")
  cat("\nAlternative: use the Python shinylive package:\n")
  cat("  pip install shinylive\n")
  cat("  shinylive export shinylive-app docs\n")
  quit(status = 1)
}

app_dir <- "."
out_dir <- "../docs"

cat(sprintf("App directory: %s\n", normalizePath(app_dir)))
cat(sprintf("Output directory: %s\n", file.path(normalizePath(".."), "docs")))

# Export
cat("\nExporting Shinylive app...\n")
shinylive::export(app_dir, out_dir)

cat("\nExport complete!\n\n")
cat("To serve locally:\n")
cat("  cd ../docs && python3 -m http.server 8080\n")
cat("  Then open http://localhost:8080 in your browser\n\n")
cat("To deploy to GitHub Pages:\n")
cat("  1. Push the docs/ directory to your repo\n")
cat("  2. In GitHub Settings > Pages, set source to 'Deploy from a branch'\n")
cat("  3. Select the branch and /docs folder\n")
