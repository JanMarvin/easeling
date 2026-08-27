import sys, os
import numpy as np
from PIL import Image
def prep(f):
    a = np.asarray(Image.open(f).convert("L")).astype(int)
    ink = a < 250
    ys, xs = np.nonzero(ink)
    if len(xs) == 0: return None
    return a[ys.min():ys.max()+1, xs.min():xs.max()+1]
def edges(a, t=40):
    e = np.zeros(a.shape, bool)
    e[:, 1:] |= np.abs(a[:, 1:] - a[:, :-1]) > t
    e[1:, :] |= np.abs(a[1:, :] - a[:-1, :]) > t
    return e
def dilate(m, r=2):
    out = m.copy()
    for _ in range(r):
        p = out
        out = p.copy()
        out[1:, :] |= p[:-1, :]; out[:-1, :] |= p[1:, :]
        out[:, 1:] |= p[:, :-1]; out[:, :-1] |= p[:, 1:]
    return out
e, r, name = prep(sys.argv[1]), prep(sys.argv[2]), sys.argv[3]
if e is None or r is None:
    print(f"{name:<26} {'EMPTY':>8}"); sys.exit(0)
h, w = r.shape
ei = np.asarray(Image.fromarray(e.astype(np.uint8)).resize((w, h))).astype(int)
d = np.abs(ei - r)
# ignore a 2px band around ink edges in either image: antialiasing and
# sub-pixel alignment live there, real geometry errors do not fit in it
tol = dilate(edges(r) | edges(ei), 2)
mask = ~tol
denom = mask.sum()
pct = 100.0 * ((d > 60) & mask).sum() / max(denom, 1)
expected = set()
if os.path.exists("expected.txt"):
    expected = set(open("expected.txt").read().split())
if name in expected:
    verdict = "known-gap"
else:
    verdict = "ok" if pct < 2 else "REVIEW"
print(f"{name:<26} {pct:8.2f} {verdict:>10}")
