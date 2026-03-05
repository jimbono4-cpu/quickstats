"""
post-export.py — Run AFTER shinylive::export("shinylive-app", "docs")
Patches docs/index.html to add the pre-WebR loading indicator.

Usage (from repo root):
    python post-export.py
    python3 post-export.py
"""

import os
import sys

INDEX = os.path.join("docs", "index.html")

if not os.path.isfile(INDEX):
    print(f"ERROR: {INDEX} not found. Run shinylive::export() first.")
    sys.exit(1)

with open(INDEX, "r", encoding="utf-8") as f:
    html = f.read()

if "app-preloader" in html:
    print(f"Preloader already present in {INDEX} — skipping.")
    sys.exit(0)

CSS = """    <style>
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

DIV = """    <div class="app-preloader" id="preloader">
      <h1>Statistical Analysis App</h1>
      <p>Loading R environment in your browser...</p>
      <div class="preloader-bar"><div class="preloader-bar-fill"></div></div>
      <p style="font-size:12px; margin-top:20px; opacity:0.6;">First load may take 15-30s. Your data never leaves your computer. All analysis done locally on your computer.</p>
    </div>
"""

SCRIPT = """    <script>
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

html = html.replace("</head>", CSS + "  </head>")
html = html.replace("<body>", "<body>\n" + DIV)
html = html.replace("</body>", SCRIPT + "  </body>")

with open(INDEX, "w", encoding="utf-8") as f:
    f.write(html)

print(f"OK: Preloader injected into {INDEX}")
