#!/usr/bin/env python3
"""
Trial Randomization App - Balance Algorithm for Two-Arm Trials

Based on:
  Carter BR, Hood K (2008) "Balance algorithm for cluster randomized trials."
  BMC Medical Research Methodology 8:65

  Raab GM, Butcher I (2001) "Balance in cluster randomized trials."
  Statistics in Medicine 20:351-365

Generates minimally imbalanced treatment allocations for two-arm clinical
trials using covariate-constrained randomization (the balance algorithm).

The algorithm enumerates (or samples) all possible balanced allocations of
participants to two treatment arms, scores each allocation using a balance
metric based on standardised covariate differences, and randomly selects
from the most balanced allocations.
"""

import csv
import random
import math
from itertools import combinations
import threading
import io
import os

# ============================================================================
# Utility
# ============================================================================

def comb(n, k):
    """Binomial coefficient C(n, k)."""
    if k < 0 or k > n:
        return 0
    if k == 0 or k == n:
        return 1
    k = min(k, n - k)
    result = 1
    for i in range(k):
        result = result * (n - i) // (i + 1)
    return result


def is_numeric(value):
    """Check if a string can be interpreted as a number."""
    try:
        float(value)
        return True
    except (ValueError, TypeError):
        return False


# ============================================================================
# Balance Algorithm
# ============================================================================

class BalanceAlgorithm:
    """
    Implements the balance algorithm for two-arm trial randomization.

    Steps:
      1. Standardise each covariate by its overall standard deviation.
      2. Enumerate all C(N, N//2) balanced allocations (or sample if too many).
      3. For each allocation, compute a balance score B:
         - L2 metric (Raab & Butcher): B = sum_k w_k * ((mean1_k - mean2_k) / sd_k)^2
         - L1 metric (Li et al.):      B = sum_k w_k * |mean1_k - mean2_k| / sd_k
      4. Define a cutoff at the specified percentile of B scores.
      5. Randomly select one allocation from those with B <= cutoff.
    """

    def __init__(self, participants, factors, weights=None):
        """
        Args:
            participants: list of dicts with 'id' key and covariate keys.
            factors:      list of (name, type) tuples.
                          type is 'continuous', 'binary', or 'categorical'.
            weights:      dict mapping factor name to weight (default 1.0).
        """
        self.participants = participants
        self.factors = factors
        self.n = len(participants)
        self.weights = weights or {}
        self._prepare()

    def _prepare(self):
        """Build covariate column array with standardisation info."""
        self.columns = []  # [(values_list, sd, weight, display_name), ...]

        for fname, ftype in self.factors:
            w = self.weights.get(fname, 1.0)
            raw = [p[fname] for p in self.participants]

            if ftype == 'continuous':
                vals = [float(v) for v in raw]
                mu = sum(vals) / len(vals)
                var = sum((x - mu) ** 2 for x in vals) / len(vals)
                sd = math.sqrt(var) if var > 0 else 1.0
                self.columns.append((vals, sd, w, fname))

            elif ftype == 'binary':
                vals = [float(v) for v in raw]
                p = sum(vals) / len(vals)
                sd = math.sqrt(p * (1 - p)) if 0 < p < 1 else 1.0
                self.columns.append((vals, sd, w, fname))

            elif ftype == 'categorical':
                levels = sorted(set(raw))
                for lev in levels[:-1]:  # L-1 dummy columns
                    dummy = [1.0 if v == lev else 0.0 for v in raw]
                    p = sum(dummy) / len(dummy)
                    sd = math.sqrt(p * (1 - p)) if 0 < p < 1 else 1.0
                    self.columns.append((dummy, sd, w, f"{fname}={lev}"))

        # Pre-compute totals for fast scoring
        self.col_totals = [sum(vals) for vals, *_ in self.columns]

    def score(self, arm1_indices, metric='l2'):
        """Compute balance score for a given allocation."""
        n1 = len(arm1_indices)
        n2 = self.n - n1
        if n1 == 0 or n2 == 0:
            return float('inf')

        B = 0.0
        for k, (vals, sd, w, _name) in enumerate(self.columns):
            s1 = sum(vals[i] for i in arm1_indices)
            s2 = self.col_totals[k] - s1
            diff = s1 / n1 - s2 / n2
            if sd > 0:
                if metric == 'l2':
                    B += w * (diff / sd) ** 2
                else:
                    B += w * abs(diff / sd)
        return B

    def run(self, metric='l2', cutoff_pct=10.0, max_enum=500000,
            seed=None, progress_cb=None):
        """
        Run the balance algorithm.

        Returns dict with allocation result and statistics.
        """
        if seed is not None:
            random.seed(seed)

        n1 = self.n // 2
        total_possible = comb(self.n, n1)

        # --- enumerate or sample ---
        if progress_cb:
            progress_cb('status', 'Generating allocations...')

        allocations = []
        scores = []

        if total_possible <= max_enum:
            # Full enumeration
            for idx, combo in enumerate(combinations(range(self.n), n1)):
                s = self.score(combo, metric)
                allocations.append(combo)
                scores.append(s)
                if progress_cb and idx % 5000 == 0:
                    progress_cb('progress', idx / total_possible)
        else:
            # Random sampling
            seen = set()
            indices = list(range(self.n))
            attempts = 0
            while len(allocations) < max_enum and attempts < max_enum * 3:
                attempts += 1
                random.shuffle(indices)
                combo = tuple(sorted(indices[:n1]))
                if combo not in seen:
                    seen.add(combo)
                    s = self.score(combo, metric)
                    allocations.append(combo)
                    scores.append(s)
                    if progress_cb and len(allocations) % 5000 == 0:
                        progress_cb('progress', len(allocations) / max_enum)

        if progress_cb:
            progress_cb('progress', 1.0)
            progress_cb('status', 'Selecting balanced allocation...')

        # --- cutoff & select ---
        sorted_scores = sorted(scores)
        cutoff_idx = max(1, int(len(sorted_scores) * cutoff_pct / 100))
        cutoff_val = sorted_scores[cutoff_idx - 1]

        acceptable = [(a, s) for a, s in zip(allocations, scores)
                       if s <= cutoff_val]

        chosen_alloc, chosen_score = random.choice(acceptable)

        # --- per-covariate balance summary ---
        arm1_set = set(chosen_alloc)
        arm2_indices = [i for i in range(self.n) if i not in arm1_set]
        balance_detail = []
        for vals, sd, w, name in self.columns:
            m1 = sum(vals[i] for i in chosen_alloc) / len(chosen_alloc)
            m2 = sum(vals[i] for i in arm2_indices) / len(arm2_indices)
            std_diff = abs(m1 - m2) / sd if sd > 0 else 0.0
            balance_detail.append({
                'name': name,
                'arm1_mean': m1,
                'arm2_mean': m2,
                'std_diff': std_diff,
            })

        return {
            'arm1_indices': set(chosen_alloc),
            'score': chosen_score,
            'all_scores': scores,
            'cutoff_score': cutoff_val,
            'n_acceptable': len(acceptable),
            'total_allocations': len(allocations),
            'total_possible': total_possible,
            'metric': metric,
            'balance_detail': balance_detail,
        }


# ============================================================================
# GUI imports (deferred so algorithm can be used without tkinter)
# ============================================================================

import tkinter as tk
from tkinter import ttk, filedialog, messagebox

# ============================================================================
# Simple Histogram on Canvas
# ============================================================================

class HistogramCanvas(tk.Canvas):
    """Draws a histogram of balance scores with cutoff and selection markers."""

    def __init__(self, master, **kw):
        kw.setdefault('bg', 'white')
        kw.setdefault('highlightthickness', 0)
        super().__init__(master, **kw)

    def draw(self, scores, cutoff=None, selected=None, n_bins=50):
        self.delete('all')
        w = self.winfo_width()
        h = self.winfo_height()
        if w < 50 or h < 50 or not scores:
            return

        margin_l, margin_r, margin_t, margin_b = 60, 20, 20, 40
        pw = w - margin_l - margin_r
        ph = h - margin_t - margin_b

        lo, hi = min(scores), max(scores)
        if lo == hi:
            hi = lo + 1
        n_bins = min(n_bins, len(scores) // 2 + 1)
        n_bins = max(n_bins, 5)
        bin_width = (hi - lo) / n_bins
        bins = [0] * n_bins
        for s in scores:
            idx = int((s - lo) / bin_width)
            idx = min(idx, n_bins - 1)
            bins[idx] += 1
        max_count = max(bins) if bins else 1

        bar_w = pw / n_bins

        # bars
        for i, count in enumerate(bins):
            x0 = margin_l + i * bar_w
            bar_h = (count / max_count) * ph if max_count > 0 else 0
            y0 = margin_t + ph - bar_h
            y1 = margin_t + ph
            colour = '#4a86c8'
            if cutoff is not None:
                bin_hi = lo + (i + 1) * bin_width
                if bin_hi <= cutoff + bin_width:
                    colour = '#2ecc71'
            self.create_rectangle(x0, y0, x0 + bar_w - 1, y1,
                                  fill=colour, outline='#2c3e50')

        # cutoff line
        if cutoff is not None and lo <= cutoff <= hi:
            cx = margin_l + ((cutoff - lo) / (hi - lo)) * pw
            self.create_line(cx, margin_t, cx, margin_t + ph,
                             fill='#e74c3c', width=2, dash=(4, 2))
            self.create_text(cx, margin_t - 4, text='cutoff',
                             fill='#e74c3c', font=('Arial', 8), anchor='s')

        # selected score line
        if selected is not None and lo <= selected <= hi:
            sx = margin_l + ((selected - lo) / (hi - lo)) * pw
            self.create_line(sx, margin_t, sx, margin_t + ph,
                             fill='#8e44ad', width=2)
            self.create_text(sx, margin_t + 10, text='selected',
                             fill='#8e44ad', font=('Arial', 8), anchor='w')

        # axes
        self.create_line(margin_l, margin_t + ph,
                         margin_l + pw, margin_t + ph, fill='black')
        self.create_line(margin_l, margin_t,
                         margin_l, margin_t + ph, fill='black')

        # x-axis labels
        for i in range(6):
            frac = i / 5
            val = lo + frac * (hi - lo)
            x = margin_l + frac * pw
            self.create_text(x, margin_t + ph + 5, text=f'{val:.3f}',
                             font=('Arial', 7), anchor='n')

        # y-axis labels
        for i in range(5):
            frac = i / 4
            count_val = int(frac * max_count)
            y = margin_t + ph - frac * ph
            self.create_text(margin_l - 5, y, text=str(count_val),
                             font=('Arial', 7), anchor='e')

        # axis titles
        self.create_text(margin_l + pw / 2, h - 2,
                         text='Balance Score (B)', font=('Arial', 9),
                         anchor='s')
        self.create_text(10, margin_t + ph / 2,
                         text='Count', font=('Arial', 9),
                         anchor='w', angle=90)


# ============================================================================
# Main Application
# ============================================================================

class TrialRandomizerApp(tk.Tk):
    """Main application window for the trial randomisation tool."""

    def __init__(self):
        super().__init__()
        self.title("Trial Randomizer - Balance Algorithm (Carter & Hood 2008)")
        self.geometry("1020x740")
        self.minsize(900, 650)

        self.factors = []          # [(name, type), ...]
        self.participants = []     # [{'id': ..., factor1: ..., ...}, ...]
        self.results = None
        self.arm1_name = tk.StringVar(value="Treatment")
        self.arm2_name = tk.StringVar(value="Control")
        self.trial_name = tk.StringVar(value="My Trial")

        self._build_ui()

    # ------------------------------------------------------------------ UI
    def _build_ui(self):
        style = ttk.Style(self)
        try:
            style.theme_use('clam')
        except tk.TclError:
            pass

        style.configure('Header.TLabel', font=('Arial', 11, 'bold'))
        style.configure('Status.TLabel', font=('Arial', 9))

        self.notebook = ttk.Notebook(self)
        self.notebook.pack(fill='both', expand=True, padx=6, pady=(6, 0))

        self._build_setup_tab()
        self._build_data_tab()
        self._build_randomize_tab()
        self._build_results_tab()

        # status bar
        self.status_var = tk.StringVar(value="Ready")
        status_bar = ttk.Label(self, textvariable=self.status_var,
                               style='Status.TLabel', relief='sunken',
                               anchor='w')
        status_bar.pack(fill='x', padx=6, pady=4)

    # ---- Tab 1: Setup ----
    def _build_setup_tab(self):
        tab = ttk.Frame(self.notebook)
        self.notebook.add(tab, text=" 1. Trial Setup ")

        # trial info
        info = ttk.LabelFrame(tab, text="Trial Information", padding=10)
        info.pack(fill='x', padx=10, pady=(10, 5))
        ttk.Label(info, text="Trial Name:").grid(row=0, column=0, sticky='e',
                                                  padx=(0, 5))
        ttk.Entry(info, textvariable=self.trial_name, width=40).grid(
            row=0, column=1, sticky='w')
        ttk.Label(info, text="Arm 1 Name:").grid(row=1, column=0, sticky='e',
                                                   padx=(0, 5), pady=2)
        ttk.Entry(info, textvariable=self.arm1_name, width=25).grid(
            row=1, column=1, sticky='w')
        ttk.Label(info, text="Arm 2 Name:").grid(row=2, column=0, sticky='e',
                                                   padx=(0, 5), pady=2)
        ttk.Entry(info, textvariable=self.arm2_name, width=25).grid(
            row=2, column=1, sticky='w')

        # factors
        fframe = ttk.LabelFrame(tab, text="Prognostic Factors", padding=10)
        fframe.pack(fill='both', expand=True, padx=10, pady=5)

        add_row = ttk.Frame(fframe)
        add_row.pack(fill='x', pady=(0, 5))

        ttk.Label(add_row, text="Name:").pack(side='left')
        self.factor_name_var = tk.StringVar()
        ttk.Entry(add_row, textvariable=self.factor_name_var,
                  width=20).pack(side='left', padx=4)

        ttk.Label(add_row, text="Type:").pack(side='left', padx=(8, 0))
        self.factor_type_var = tk.StringVar(value="Continuous")
        ttk.Combobox(add_row, textvariable=self.factor_type_var,
                     values=["Continuous", "Binary", "Categorical"],
                     state='readonly', width=12).pack(side='left', padx=4)

        ttk.Label(add_row, text="Weight:").pack(side='left', padx=(8, 0))
        self.factor_weight_var = tk.StringVar(value="1.0")
        ttk.Entry(add_row, textvariable=self.factor_weight_var,
                  width=6).pack(side='left', padx=4)

        ttk.Button(add_row, text="Add Factor",
                   command=self._add_factor).pack(side='left', padx=8)
        ttk.Button(add_row, text="Remove Selected",
                   command=self._remove_factor).pack(side='left')

        cols = ('name', 'type', 'weight')
        self.factor_tree = ttk.Treeview(fframe, columns=cols,
                                         show='headings', height=8)
        self.factor_tree.heading('name', text='Factor Name')
        self.factor_tree.heading('type', text='Type')
        self.factor_tree.heading('weight', text='Weight')
        self.factor_tree.column('name', width=200)
        self.factor_tree.column('type', width=120)
        self.factor_tree.column('weight', width=80)
        self.factor_tree.pack(fill='both', expand=True)

    def _add_factor(self):
        name = self.factor_name_var.get().strip()
        ftype = self.factor_type_var.get().lower()
        try:
            weight = float(self.factor_weight_var.get())
        except ValueError:
            messagebox.showerror("Error", "Weight must be a number.")
            return
        if not name:
            messagebox.showerror("Error", "Factor name cannot be empty.")
            return
        if any(f[0] == name for f in self.factors):
            messagebox.showerror("Error", f"Factor '{name}' already exists.")
            return

        self.factors.append((name, ftype, weight))
        self.factor_tree.insert('', 'end', values=(name, ftype.title(), weight))
        self.factor_name_var.set('')
        self.factor_weight_var.set('1.0')
        self.status_var.set(f"Factor '{name}' added ({len(self.factors)} total)")

    def _remove_factor(self):
        sel = self.factor_tree.selection()
        if not sel:
            return
        for item in sel:
            vals = self.factor_tree.item(item, 'values')
            self.factors = [f for f in self.factors if f[0] != vals[0]]
            self.factor_tree.delete(item)
        self.status_var.set(f"{len(self.factors)} factors defined")

    # ---- Tab 2: Data ----
    def _build_data_tab(self):
        tab = ttk.Frame(self.notebook)
        self.notebook.add(tab, text=" 2. Participant Data ")

        btn_row = ttk.Frame(tab)
        btn_row.pack(fill='x', padx=10, pady=8)

        ttk.Button(btn_row, text="Import CSV...",
                   command=self._import_csv).pack(side='left')
        ttk.Button(btn_row, text="Load Example Data",
                   command=self._load_example).pack(side='left', padx=8)
        ttk.Button(btn_row, text="Clear All Data",
                   command=self._clear_data).pack(side='left')
        self.data_count_var = tk.StringVar(value="No data loaded")
        ttk.Label(btn_row, textvariable=self.data_count_var,
                  style='Status.TLabel').pack(side='right')

        # manual entry
        manual = ttk.LabelFrame(tab, text="Manual Entry", padding=6)
        manual.pack(fill='x', padx=10, pady=(0, 5))

        self.manual_frame = ttk.Frame(manual)
        self.manual_frame.pack(fill='x')
        ttk.Label(self.manual_frame,
                  text="(Define factors first, then fields appear here)"
                  ).pack(anchor='w')
        ttk.Button(manual, text="Add Participant",
                   command=self._add_participant_manual).pack(anchor='w',
                                                              pady=(4, 0))

        # data table
        table_frame = ttk.Frame(tab)
        table_frame.pack(fill='both', expand=True, padx=10, pady=(0, 8))

        self.data_tree = ttk.Treeview(table_frame, show='headings', height=12)
        vsb = ttk.Scrollbar(table_frame, orient='vertical',
                            command=self.data_tree.yview)
        hsb = ttk.Scrollbar(table_frame, orient='horizontal',
                            command=self.data_tree.xview)
        self.data_tree.configure(yscrollcommand=vsb.set,
                                 xscrollcommand=hsb.set)
        self.data_tree.grid(row=0, column=0, sticky='nsew')
        vsb.grid(row=0, column=1, sticky='ns')
        hsb.grid(row=1, column=0, sticky='ew')
        table_frame.rowconfigure(0, weight=1)
        table_frame.columnconfigure(0, weight=1)

    def _refresh_manual_fields(self):
        """Rebuild manual entry fields to match current factor list."""
        for w in self.manual_frame.winfo_children():
            w.destroy()
        self.manual_entries = {}

        ttk.Label(self.manual_frame, text="ID:").pack(side='left')
        self.manual_id_var = tk.StringVar()
        ttk.Entry(self.manual_frame, textvariable=self.manual_id_var,
                  width=10).pack(side='left', padx=(2, 8))

        for fname, ftype, _w in self.factors:
            ttk.Label(self.manual_frame, text=f"{fname}:").pack(side='left')
            var = tk.StringVar()
            ttk.Entry(self.manual_frame, textvariable=var,
                      width=10).pack(side='left', padx=(2, 8))
            self.manual_entries[fname] = var

    def _add_participant_manual(self):
        if not self.factors:
            messagebox.showinfo("Info", "Define factors in the Setup tab first.")
            return
        if not hasattr(self, 'manual_entries') or not self.manual_entries:
            self._refresh_manual_fields()
            return

        pid = self.manual_id_var.get().strip()
        if not pid:
            messagebox.showerror("Error", "Participant ID is required.")
            return

        row = {'id': pid}
        for fname, ftype, _w in self.factors:
            val = self.manual_entries[fname].get().strip()
            if not val:
                messagebox.showerror("Error", f"Value for '{fname}' is required.")
                return
            if ftype == 'continuous':
                if not is_numeric(val):
                    messagebox.showerror("Error",
                                         f"'{fname}' must be numeric (got '{val}').")
                    return
                row[fname] = float(val)
            elif ftype == 'binary':
                if val not in ('0', '1'):
                    messagebox.showerror(
                        "Error",
                        f"'{fname}' must be 0 or 1 (got '{val}').")
                    return
                row[fname] = float(val)
            else:
                row[fname] = val

        self.participants.append(row)
        self._refresh_data_table()
        self.manual_id_var.set('')
        for v in self.manual_entries.values():
            v.set('')

    def _import_csv(self):
        path = filedialog.askopenfilename(
            title="Import Participant Data (CSV)",
            filetypes=[("CSV files", "*.csv"), ("All files", "*.*")])
        if not path:
            return
        try:
            with open(path, newline='', encoding='utf-8-sig') as f:
                reader = csv.DictReader(f)
                rows = list(reader)
        except Exception as e:
            messagebox.showerror("Import Error", str(e))
            return

        if not rows:
            messagebox.showinfo("Info", "CSV file is empty.")
            return

        headers = list(rows[0].keys())

        # Auto-detect factors if none defined
        if not self.factors:
            id_col = headers[0]
            for col in headers[1:]:
                values = [r[col] for r in rows if r[col].strip()]
                all_numeric = all(is_numeric(v) for v in values)
                unique = set(values)
                if all_numeric and len(unique) > 2:
                    ftype = 'continuous'
                elif len(unique) == 2:
                    ftype = 'binary'
                else:
                    ftype = 'categorical'
                self.factors.append((col, ftype, 1.0))
                self.factor_tree.insert('', 'end',
                                        values=(col, ftype.title(), 1.0))
            self.status_var.set(
                f"Auto-detected {len(self.factors)} factors from CSV headers. "
                "Review types in Setup tab.")
        else:
            id_col = headers[0]

        # Parse data
        factor_names = [f[0] for f in self.factors]
        factor_types = {f[0]: f[1] for f in self.factors}
        new_participants = []
        errors = []

        for i, row in enumerate(rows):
            p = {'id': row.get(id_col, f"P{i+1}")}
            valid = True
            for fname in factor_names:
                val = row.get(fname, '').strip()
                if not val:
                    errors.append(f"Row {i+2}: missing value for '{fname}'")
                    valid = False
                    continue
                if factor_types[fname] == 'continuous':
                    if not is_numeric(val):
                        errors.append(
                            f"Row {i+2}: '{fname}' = '{val}' is not numeric")
                        valid = False
                    else:
                        p[fname] = float(val)
                elif factor_types[fname] == 'binary':
                    if is_numeric(val):
                        p[fname] = float(val)
                    else:
                        # Auto-code: first unique value = 0, second = 1
                        p[fname] = val
                else:
                    p[fname] = val
            if valid:
                new_participants.append(p)

        if errors:
            msg = f"Imported {len(new_participants)} participants.\n"
            if len(errors) <= 10:
                msg += "Warnings:\n" + "\n".join(errors)
            else:
                msg += f"{len(errors)} warnings (showing first 10):\n"
                msg += "\n".join(errors[:10])
            messagebox.showwarning("Import Warnings", msg)

        self.participants = new_participants

        # Auto-code binary factors that have string values
        for fname, ftype, _w in self.factors:
            if ftype == 'binary':
                vals = set()
                for p in self.participants:
                    v = p.get(fname)
                    if v is not None:
                        vals.add(v)
                if vals and not all(isinstance(v, float) for v in vals):
                    sorted_vals = sorted(str(v) for v in vals)
                    mapping = {sorted_vals[0]: 0.0}
                    if len(sorted_vals) > 1:
                        mapping[sorted_vals[1]] = 1.0
                    for p in self.participants:
                        if fname in p:
                            p[fname] = mapping.get(str(p[fname]), 0.0)

        self._refresh_data_table()
        self._refresh_manual_fields()
        self.status_var.set(
            f"Loaded {len(self.participants)} participants from CSV")

    def _load_example(self):
        """Load example data for demonstration."""
        self.factors = [
            ('Age', 'continuous', 1.0),
            ('Sex', 'binary', 1.0),
            ('BMI', 'continuous', 1.0),
            ('Smoking', 'binary', 1.0),
            ('Stage', 'categorical', 1.0),
        ]
        # Refresh factor tree
        for item in self.factor_tree.get_children():
            self.factor_tree.delete(item)
        for name, ftype, w in self.factors:
            self.factor_tree.insert('', 'end', values=(name, ftype.title(), w))

        random.seed(42)
        stages = ['I', 'II', 'III']
        self.participants = []
        for i in range(20):
            self.participants.append({
                'id': f'P{i+1:03d}',
                'Age': round(random.gauss(55, 12), 1),
                'Sex': float(random.randint(0, 1)),
                'BMI': round(random.gauss(27, 4), 1),
                'Smoking': float(random.randint(0, 1)),
                'Stage': random.choice(stages),
            })

        self._refresh_data_table()
        self._refresh_manual_fields()
        self.status_var.set("Example data loaded (20 participants, 5 factors)")

    def _clear_data(self):
        self.participants = []
        self._refresh_data_table()
        self.status_var.set("Data cleared")

    def _refresh_data_table(self):
        """Rebuild the data treeview from current participants."""
        self.data_tree.delete(*self.data_tree.get_children())
        if not self.participants:
            self.data_tree['columns'] = ()
            self.data_count_var.set("No data loaded")
            return

        factor_names = [f[0] for f in self.factors]
        cols = ['id'] + factor_names
        self.data_tree['columns'] = cols
        self.data_tree.heading('id', text='ID')
        self.data_tree.column('id', width=70)
        for fn in factor_names:
            self.data_tree.heading(fn, text=fn)
            self.data_tree.column(fn, width=90)

        for p in self.participants:
            vals = [p.get('id', '')] + [p.get(fn, '') for fn in factor_names]
            display = []
            for v in vals:
                if isinstance(v, float) and v == int(v):
                    display.append(str(int(v)))
                else:
                    display.append(str(v))
            self.data_tree.insert('', 'end', values=display)

        self.data_count_var.set(f"{len(self.participants)} participants loaded")

    # ---- Tab 3: Randomize ----
    def _build_randomize_tab(self):
        tab = ttk.Frame(self.notebook)
        self.notebook.add(tab, text=" 3. Randomize ")

        # settings
        settings = ttk.LabelFrame(tab, text="Algorithm Settings", padding=10)
        settings.pack(fill='x', padx=10, pady=(10, 5))

        row0 = ttk.Frame(settings)
        row0.pack(fill='x', pady=2)
        ttk.Label(row0, text="Balance Metric:").pack(side='left')
        self.metric_var = tk.StringVar(value="l2")
        ttk.Radiobutton(row0, text="L2 (Raab & Butcher - squared differences)",
                        variable=self.metric_var,
                        value="l2").pack(side='left', padx=(5, 15))
        ttk.Radiobutton(row0, text="L1 (absolute differences)",
                        variable=self.metric_var,
                        value="l1").pack(side='left')

        row1 = ttk.Frame(settings)
        row1.pack(fill='x', pady=2)
        ttk.Label(row1, text="Cutoff Percentile:").pack(side='left')
        self.cutoff_var = tk.StringVar(value="10")
        ttk.Entry(row1, textvariable=self.cutoff_var,
                  width=6).pack(side='left', padx=4)
        ttk.Label(row1,
                  text="% (allocations with B score in the lowest X% "
                       "are eligible)").pack(side='left')

        row2 = ttk.Frame(settings)
        row2.pack(fill='x', pady=2)
        ttk.Label(row2, text="Max Enumerations:").pack(side='left')
        self.maxenum_var = tk.StringVar(value="500000")
        ttk.Entry(row2, textvariable=self.maxenum_var,
                  width=10).pack(side='left', padx=4)
        ttk.Label(row2,
                  text="(full enumeration if C(N,N/2) is below this; "
                       "otherwise random sampling)").pack(side='left')

        row3 = ttk.Frame(settings)
        row3.pack(fill='x', pady=2)
        ttk.Label(row3, text="Random Seed:").pack(side='left')
        self.seed_var = tk.StringVar(value="")
        ttk.Entry(row3, textvariable=self.seed_var,
                  width=10).pack(side='left', padx=4)
        ttk.Label(row3,
                  text="(leave blank for random; set for reproducibility)"
                  ).pack(side='left')

        # run button
        btn_frame = ttk.Frame(tab)
        btn_frame.pack(fill='x', padx=10, pady=5)
        self.run_btn = ttk.Button(btn_frame, text="Run Randomization",
                                  command=self._run_randomization)
        self.run_btn.pack(side='left')
        self.progress_var = tk.DoubleVar(value=0)
        self.progress_bar = ttk.Progressbar(btn_frame,
                                             variable=self.progress_var,
                                             maximum=1.0, length=300)
        self.progress_bar.pack(side='left', padx=10)
        self.progress_label = ttk.Label(btn_frame, text="")
        self.progress_label.pack(side='left')

        # summary
        summary = ttk.LabelFrame(tab, text="Summary", padding=10)
        summary.pack(fill='x', padx=10, pady=5)
        self.summary_text = tk.Text(summary, height=5, wrap='word',
                                    font=('Consolas', 10), state='disabled')
        self.summary_text.pack(fill='x')

        # histogram
        hist_frame = ttk.LabelFrame(tab, text="Distribution of Balance Scores",
                                     padding=5)
        hist_frame.pack(fill='both', expand=True, padx=10, pady=(5, 8))
        self.histogram = HistogramCanvas(hist_frame, height=180)
        self.histogram.pack(fill='both', expand=True)
        self.histogram.bind('<Configure>', self._redraw_histogram)

    def _redraw_histogram(self, event=None):
        if self.results:
            self.histogram.draw(
                self.results['all_scores'],
                cutoff=self.results['cutoff_score'],
                selected=self.results['score'])

    def _run_randomization(self):
        # Validate
        if not self.factors:
            messagebox.showerror("Error",
                                 "No factors defined. Go to the Setup tab.")
            return
        if len(self.participants) < 4:
            messagebox.showerror(
                "Error",
                "Need at least 4 participants for balanced randomization.")
            return

        factor_names = [f[0] for f in self.factors]
        for p in self.participants:
            for fn in factor_names:
                if fn not in p:
                    messagebox.showerror(
                        "Error",
                        f"Participant '{p.get('id', '?')}' is missing "
                        f"value for factor '{fn}'.")
                    return

        try:
            cutoff = float(self.cutoff_var.get())
            max_enum = int(self.maxenum_var.get())
        except ValueError:
            messagebox.showerror("Error",
                                 "Cutoff and max enumerations must be numbers.")
            return

        seed_str = self.seed_var.get().strip()
        seed = int(seed_str) if seed_str and seed_str.isdigit() else None

        metric = self.metric_var.get()
        factor_tuples = [(f[0], f[1]) for f in self.factors]
        weights = {f[0]: f[2] for f in self.factors}

        n1 = len(self.participants) // 2
        total = comb(len(self.participants), n1)

        self.run_btn.configure(state='disabled')
        self.progress_var.set(0)
        self.progress_label.configure(text="Starting...")

        def run_in_thread():
            try:
                algo = BalanceAlgorithm(self.participants, factor_tuples,
                                        weights)

                def on_progress(ptype, val):
                    if ptype == 'status':
                        self.after(0, lambda v=val:
                                   self.progress_label.configure(text=v))
                    elif ptype == 'progress':
                        self.after(0, lambda v=val:
                                   self.progress_var.set(v))

                results = algo.run(
                    metric=metric,
                    cutoff_pct=cutoff,
                    max_enum=max_enum,
                    seed=seed,
                    progress_cb=on_progress,
                )
                self.after(0, lambda: self._on_results(results))
            except Exception as e:
                self.after(0, lambda: messagebox.showerror(
                    "Algorithm Error", str(e)))
                self.after(0, lambda: self.run_btn.configure(state='normal'))

        threading.Thread(target=run_in_thread, daemon=True).start()

    def _on_results(self, results):
        self.results = results
        self.run_btn.configure(state='normal')
        self.progress_var.set(1.0)
        self.progress_label.configure(text="Done")

        # Summary text
        enumerated = ("full enumeration"
                      if results['total_allocations'] == results['total_possible']
                      else "random sampling")
        metric_name = "L2 (squared)" if results['metric'] == 'l2' else "L1 (absolute)"
        summary = (
            f"Metric:                  {metric_name}\n"
            f"Total possible:          {results['total_possible']:,}\n"
            f"Allocations evaluated:   {results['total_allocations']:,} ({enumerated})\n"
            f"Acceptable (B <= cutoff): {results['n_acceptable']:,}\n"
            f"Selected B score:        {results['score']:.6f}\n"
            f"Cutoff B score:          {results['cutoff_score']:.6f}"
        )
        self.summary_text.configure(state='normal')
        self.summary_text.delete('1.0', 'end')
        self.summary_text.insert('1.0', summary)
        self.summary_text.configure(state='disabled')

        # Histogram
        self.histogram.draw(results['all_scores'],
                            cutoff=results['cutoff_score'],
                            selected=results['score'])

        # Populate results tab
        self._populate_results()

        self.status_var.set(
            f"Randomization complete. B = {results['score']:.6f}")

        # Switch to results tab
        self.notebook.select(3)

    # ---- Tab 4: Results & Export ----
    def _build_results_tab(self):
        tab = ttk.Frame(self.notebook)
        self.notebook.add(tab, text=" 4. Results & Export ")

        btn_row = ttk.Frame(tab)
        btn_row.pack(fill='x', padx=10, pady=8)
        ttk.Button(btn_row, text="Export Allocation to CSV...",
                   command=self._export_allocation).pack(side='left')
        ttk.Button(btn_row, text="Export Full Report to CSV...",
                   command=self._export_report).pack(side='left', padx=8)

        # allocation table
        alloc_frame = ttk.LabelFrame(tab, text="Treatment Allocation",
                                      padding=5)
        alloc_frame.pack(fill='both', expand=True, padx=10, pady=(0, 5))

        self.alloc_tree = ttk.Treeview(alloc_frame, show='headings', height=10)
        vsb = ttk.Scrollbar(alloc_frame, orient='vertical',
                            command=self.alloc_tree.yview)
        self.alloc_tree.configure(yscrollcommand=vsb.set)
        self.alloc_tree.pack(side='left', fill='both', expand=True)
        vsb.pack(side='right', fill='y')

        # balance summary
        bal_frame = ttk.LabelFrame(tab, text="Per-Covariate Balance Summary",
                                    padding=5)
        bal_frame.pack(fill='x', padx=10, pady=(0, 8))

        cols = ('covariate', 'arm1_mean', 'arm2_mean', 'std_diff')
        self.balance_tree = ttk.Treeview(bal_frame, columns=cols,
                                          show='headings', height=6)
        self.balance_tree.heading('covariate', text='Covariate')
        self.balance_tree.heading('arm1_mean', text='Arm 1 Mean')
        self.balance_tree.heading('arm2_mean', text='Arm 2 Mean')
        self.balance_tree.heading('std_diff', text='Std. Difference')
        self.balance_tree.column('covariate', width=180)
        self.balance_tree.column('arm1_mean', width=120)
        self.balance_tree.column('arm2_mean', width=120)
        self.balance_tree.column('std_diff', width=120)
        self.balance_tree.pack(fill='x')

    def _populate_results(self):
        if not self.results:
            return

        arm1_set = self.results['arm1_indices']
        arm1_name = self.arm1_name.get()
        arm2_name = self.arm2_name.get()
        factor_names = [f[0] for f in self.factors]

        # Allocation table
        self.alloc_tree.delete(*self.alloc_tree.get_children())
        cols = ['id'] + factor_names + ['arm']
        self.alloc_tree['columns'] = cols
        for c in cols:
            display = {'id': 'ID', 'arm': 'Allocated Arm'}.get(c, c)
            self.alloc_tree.heading(c, text=display)
            self.alloc_tree.column(c, width=90)

        for i, p in enumerate(self.participants):
            arm = arm1_name if i in arm1_set else arm2_name
            vals = [p.get('id', '')]
            for fn in factor_names:
                v = p.get(fn, '')
                if isinstance(v, float) and v == int(v):
                    vals.append(str(int(v)))
                else:
                    vals.append(str(v))
            vals.append(arm)
            tag = 'arm1' if i in arm1_set else 'arm2'
            self.alloc_tree.insert('', 'end', values=vals, tags=(tag,))

        self.alloc_tree.tag_configure('arm1', background='#d5f5e3')
        self.alloc_tree.tag_configure('arm2', background='#d6eaf8')

        # Update column headings to reflect arm names
        self.balance_tree.heading('arm1_mean',
                                  text=f'{arm1_name} Mean')
        self.balance_tree.heading('arm2_mean',
                                  text=f'{arm2_name} Mean')

        # Balance detail
        self.balance_tree.delete(*self.balance_tree.get_children())
        for bd in self.results['balance_detail']:
            self.balance_tree.insert('', 'end', values=(
                bd['name'],
                f"{bd['arm1_mean']:.4f}",
                f"{bd['arm2_mean']:.4f}",
                f"{bd['std_diff']:.4f}",
            ))

    def _export_allocation(self):
        if not self.results:
            messagebox.showinfo("Info", "Run randomization first.")
            return
        path = filedialog.asksaveasfilename(
            title="Export Allocation",
            defaultextension=".csv",
            filetypes=[("CSV files", "*.csv")])
        if not path:
            return

        arm1_set = self.results['arm1_indices']
        arm1_name = self.arm1_name.get()
        arm2_name = self.arm2_name.get()
        factor_names = [f[0] for f in self.factors]

        try:
            with open(path, 'w', newline='', encoding='utf-8') as f:
                writer = csv.writer(f)
                writer.writerow(['ID'] + factor_names + ['Allocated_Arm'])
                for i, p in enumerate(self.participants):
                    arm = arm1_name if i in arm1_set else arm2_name
                    row = [p.get('id', '')]
                    for fn in factor_names:
                        v = p.get(fn, '')
                        if isinstance(v, float) and v == int(v):
                            row.append(str(int(v)))
                        else:
                            row.append(str(v))
                    row.append(arm)
                    writer.writerow(row)
            self.status_var.set(f"Allocation exported to {path}")
            messagebox.showinfo("Success", f"Allocation exported to:\n{path}")
        except Exception as e:
            messagebox.showerror("Export Error", str(e))

    def _export_report(self):
        if not self.results:
            messagebox.showinfo("Info", "Run randomization first.")
            return
        path = filedialog.asksaveasfilename(
            title="Export Full Report",
            defaultextension=".csv",
            filetypes=[("CSV files", "*.csv")])
        if not path:
            return

        arm1_name = self.arm1_name.get()
        arm2_name = self.arm2_name.get()

        try:
            with open(path, 'w', newline='', encoding='utf-8') as f:
                writer = csv.writer(f)
                # Header info
                writer.writerow(['Trial Randomization Report'])
                writer.writerow(['Trial Name', self.trial_name.get()])
                writer.writerow(['Arm 1', arm1_name])
                writer.writerow(['Arm 2', arm2_name])
                writer.writerow(['N participants',
                                 str(len(self.participants))])
                writer.writerow(['Metric', self.results['metric'].upper()])
                writer.writerow(['Total allocations evaluated',
                                 str(self.results['total_allocations'])])
                writer.writerow(['Total possible allocations',
                                 str(self.results['total_possible'])])
                writer.writerow(['Acceptable allocations',
                                 str(self.results['n_acceptable'])])
                writer.writerow(['Selected B score',
                                 f"{self.results['score']:.6f}"])
                writer.writerow(['Cutoff B score',
                                 f"{self.results['cutoff_score']:.6f}"])
                writer.writerow([])

                # Balance detail
                writer.writerow(['Per-Covariate Balance Summary'])
                writer.writerow(['Covariate', f'{arm1_name} Mean',
                                 f'{arm2_name} Mean', 'Std. Difference'])
                for bd in self.results['balance_detail']:
                    writer.writerow([
                        bd['name'],
                        f"{bd['arm1_mean']:.4f}",
                        f"{bd['arm2_mean']:.4f}",
                        f"{bd['std_diff']:.4f}",
                    ])
                writer.writerow([])

                # Allocation
                factor_names = [f[0] for f in self.factors]
                writer.writerow(['Treatment Allocation'])
                writer.writerow(['ID'] + factor_names + ['Allocated_Arm'])
                arm1_set = self.results['arm1_indices']
                for i, p in enumerate(self.participants):
                    arm = arm1_name if i in arm1_set else arm2_name
                    row = [p.get('id', '')]
                    for fn in factor_names:
                        v = p.get(fn, '')
                        if isinstance(v, float) and v == int(v):
                            row.append(str(int(v)))
                        else:
                            row.append(str(v))
                    row.append(arm)
                    writer.writerow(row)

            self.status_var.set(f"Report exported to {path}")
            messagebox.showinfo("Success", f"Report exported to:\n{path}")
        except Exception as e:
            messagebox.showerror("Export Error", str(e))


# ============================================================================
# Entry Point
# ============================================================================

def main():
    app = TrialRandomizerApp()
    app.mainloop()


if __name__ == '__main__':
    main()
