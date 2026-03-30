"""
Comparison of Nelder-Mead vs SciPy optimizers on landscapes benchmark functions
and COCO/BBOB suite.
"""

import numpy as np
import time
import matplotlib.pyplot as plt
from scipy.optimize import minimize
from landscapes.single_objective import ackley, rastrigin, rosenbrock, sphere

from nelder_mead import nelder_mead

# ── helpers ──────────────────────────────────────────────────────────────────

def make_simplex(x0: np.ndarray, step: float = 0.5) -> np.ndarray:
    """Build an initial simplex around x0."""
    n = len(x0)
    S = np.tile(x0, (n + 1, 1)).astype(float)
    for i in range(n):
        S[i + 1, i] += step
    return S


def run_custom(f, x0, eps=1e-6, max_iter=1000):
    S = make_simplex(x0)
    t0 = time.perf_counter()
    best, _ = nelder_mead(f, S, eps, max_iter)
    elapsed = time.perf_counter() - t0
    return best, f(best), elapsed


def run_scipy(f, x0, method="Nelder-Mead"):
    t0 = time.perf_counter()
    opts = {"maxiter": 1000, "xatol": 1e-6, "fatol": 1e-6} if method == "Nelder-Mead" else {"maxiter": 1000}
    res = minimize(f, x0, method=method, options=opts)
    elapsed = time.perf_counter() - t0
    return res.x, res.fun, elapsed


# ── Part 1: landscapes benchmark functions ───────────────────────────────────

FUNCTIONS = {
    "ackley":      (ackley,      np.array([-1.0, -1.0])),
    "rastrigin":   (rastrigin,   np.array([-1.0, -1.0])),
    "rosenbrock":  (rosenbrock,  np.array([-1.0, -1.0])),
    "sphere":      (sphere,      np.array([-1.0, -1.0])),
}

print("=" * 70)
print(f"{'Function':<14} {'Method':<18} {'f(x*)':<14} {'Time (ms)':<12}")
print("=" * 70)

p1_data = {}
for name, (f, x0) in FUNCTIONS.items():
    p1_data[name] = {}

    best, fval, t = run_custom(f, x0)
    p1_data[name]["custom NM"] = (fval, t * 1000)
    print(f"{name:<14} {'custom NM':<18} {fval:<14.6f} {t*1000:<12.2f}")

    best, fval, t = run_scipy(f, x0, "Nelder-Mead")
    p1_data[name]["scipy NM"] = (fval, t * 1000)
    print(f"{name:<14} {'scipy NM':<18} {fval:<14.6f} {t*1000:<12.2f}")

    best, fval, t = run_scipy(f, x0, "BFGS")
    p1_data[name]["scipy BFGS"] = (fval, t * 1000)
    print(f"{name:<14} {'scipy BFGS':<18} {fval:<14.6f} {t*1000:<12.2f}")
    print("-" * 70)

# ── Plot Part 1 ───────────────────────────────────────────────────────────────

methods_p1 = ["custom NM", "scipy NM", "scipy BFGS"]
fn_names = list(p1_data.keys())
x = np.arange(len(fn_names))
width = 0.25

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(12, 5))
fig.suptitle("Landscapes benchmark — method comparison")

for i, method in enumerate(methods_p1):
    fvals = [p1_data[n][method][0] for n in fn_names]
    times = [p1_data[n][method][1] for n in fn_names]
    ax1.bar(x + i * width, fvals, width, label=method)
    ax2.bar(x + i * width, times, width, label=method)

ax1.set_title("Best f(x*)")
ax1.set_xticks(x + width)
ax1.set_xticklabels(fn_names)
ax1.set_ylabel("f(x*)")
ax1.legend()

ax2.set_title("Time (ms)")
ax2.set_xticks(x + width)
ax2.set_xticklabels(fn_names)
ax2.set_ylabel("ms")
ax2.legend()

plt.tight_layout()
plt.savefig("part1_comparison.png", dpi=150)
plt.show()


# ── Part 2: COCO/BBOB benchmark ───────────────────────────────────────────────

import cocoex

print("\n" + "=" * 70)
print("COCO/BBOB benchmark (dim=2, functions 1-5, instance 1)")
print("=" * 70)
print(f"{'Function':<12} {'Method':<18} {'best f':<14} {'evals':<8}")
print("-" * 70)

SUITE_ARGS = "dimensions:2 function_indices:1-5 instance_indices:1"

results = {}

for problem in cocoex.Suite("bbob", "", SUITE_ARGS):
    fid = problem.id_function
    x0 = problem.initial_solution.copy()
    best, fval, _ = run_custom(problem, x0, eps=1e-8, max_iter=2000)
    results[fid] = {"_name": problem.name, "custom NM": (fval, problem.evaluations)}

for problem in cocoex.Suite("bbob", "", SUITE_ARGS):
    fid = problem.id_function
    x0 = problem.initial_solution.copy()
    res = minimize(problem, x0, method="Nelder-Mead",
                   options={"maxiter": 2000, "xatol": 1e-8, "fatol": 1e-8})
    results[fid]["scipy NM"] = (res.fun, problem.evaluations)

for fid, data in sorted(results.items()):
    fname = data["_name"]
    for method in ("custom NM", "scipy NM"):
        fval, evals = data[method]
        label = fname if method == "custom NM" else ""
        print(f"{label:<12} {method:<18} {fval:<14.6f} {evals:<8}")
    print()

# ── Plot Part 2 ───────────────────────────────────────────────────────────────

methods_p2 = ["custom NM", "scipy NM"]
fids = sorted(results.keys())
labels_p2 = [f"f{fid}" for fid in fids]
x2 = np.arange(len(fids))
width2 = 0.35

fig2, (ax3, ax4) = plt.subplots(1, 2, figsize=(11, 5))
fig2.suptitle("COCO/BBOB benchmark (dim=2, f1-f5)")

for i, method in enumerate(methods_p2):
    fvals = [results[fid][method][0] for fid in fids]
    evals = [results[fid][method][1] for fid in fids]
    ax3.bar(x2 + i * width2, fvals, width2, label=method)
    ax4.bar(x2 + i * width2, evals, width2, label=method)

ax3.set_title("Best f value")
ax3.set_xticks(x2 + width2 / 2)
ax3.set_xticklabels(labels_p2)
ax3.set_ylabel("f(x*)")
ax3.legend()

ax4.set_title("Function evaluations")
ax4.set_xticks(x2 + width2 / 2)
ax4.set_xticklabels(labels_p2)
ax4.set_ylabel("evals")
ax4.legend()

plt.tight_layout()
plt.savefig("part2_comparison.png", dpi=150)
plt.show()
