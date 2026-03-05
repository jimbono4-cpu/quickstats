#!/bin/bash
# post-export.sh — Run AFTER shinylive::export("shinylive-app", "docs")
# Patches docs/index.html to add the pre-WebR loading indicator.
# Usage: bash post-export.sh

INDEX="docs/index.html"

if [ ! -f "$INDEX" ]; then
  echo "ERROR: $INDEX not found. Run shinylive::export() first."
  exit 1
fi

# Check if preloader already present
if grep -q "app-preloader" "$INDEX"; then
  echo "Preloader already present in $INDEX — skipping."
  exit 0
fi

# --- Inject preloader CSS into <head> (before </head>) ---
PRELOADER_CSS='    <style>
      .app-preloader {
        position: fixed; top: 0; left: 0; width: 100%; height: 100%;
        display: flex; flex-direction: column;
        align-items: center; justify-content: center;
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white; font-family: '\''Segoe UI'\'', Tahoma, sans-serif;
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
    </style>'

# --- Inject preloader HTML into <body> (after <body>) ---
PRELOADER_HTML='    <div class="app-preloader" id="preloader">
      <h1>Statistical Analysis App</h1>
      <p>Loading R environment in your browser...<\/p>
      <div class="preloader-bar"><div class="preloader-bar-fill"><\/div><\/div>
      <p style="font-size:12px; margin-top:20px; opacity:0.6;">First load may take 15-30s. Your data never leaves your computer. All analysis done locally on your computer.<\/p>
    <\/div>'

# --- Inject MutationObserver script (before </body>) ---
PRELOADER_SCRIPT='    <script>
      \/\/ Hide preloader once Shiny app renders
      const observer = new MutationObserver(function(mutations) {
        const root = document.getElementById('\''root'\'');
        if (root \&\& root.children.length > 0) {
          const pre = document.getElementById('\''preloader'\'');
          if (pre) { pre.classList.add('\''hidden'\''); setTimeout(() => pre.remove(), 600); }
          observer.disconnect();
        }
      });
      observer.observe(document.getElementById('\''root'\''), { childList: true, subtree: true });
    <\/script>'

# Use Python for reliable multi-line text insertion
python3 << 'PYEOF'
import re

with open("docs/index.html", "r") as f:
    html = f.read()

# 1. Add CSS before </head>
css_block = """    <style>
      .app-preloader {
        position: fixed; top: 0; left: 0; width: 100%; height: 100%;
        display: flex; flex-direction: column;
        align-items: center; justify-content: center;
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white; font-family: 'Segoe UI', Tahoma, sans-serif;
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
    </style>
"""
html = html.replace("</head>", css_block + "  </head>")

# 2. Add preloader div after <body>
preloader_div = """    <div class="app-preloader" id="preloader">
      <h1>Statistical Analysis App</h1>
      <p>Loading R environment in your browser...</p>
      <div class="preloader-bar"><div class="preloader-bar-fill"></div></div>
      <p style="font-size:12px; margin-top:20px; opacity:0.6;">First load may take 15-30s. Your data never leaves your computer. All analysis done locally on your computer.</p>
    </div>
"""
html = html.replace("<body>", "<body>\n" + preloader_div)

# 3. Add MutationObserver script before </body>
observer_script = """    <script>
      // Hide preloader once Shiny app renders
      const observer = new MutationObserver(function(mutations) {
        const root = document.getElementById('root');
        if (root && root.children.length > 0) {
          const pre = document.getElementById('preloader');
          if (pre) { pre.classList.add('hidden'); setTimeout(() => pre.remove(), 600); }
          observer.disconnect();
        }
      });
      observer.observe(document.getElementById('root'), { childList: true, subtree: true });
    </script>
"""
goatcounter = """    <script data-goatcounter="https://jimbono4.goatcounter.com/count"
            async src="//gc.zgo.at/count.js"></script>
"""

html = html.replace("</body>", observer_script + goatcounter + "  </body>")

with open("docs/index.html", "w") as f:
    f.write(html)

print("OK: Preloader injected into docs/index.html")
PYEOF
