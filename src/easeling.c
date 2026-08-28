#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <R_ext/GraphicsEngine.h>
#include <R_ext/GraphicsDevice.h>
#include <R_ext/Rdynload.h>

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define PT_TO_EMU 12700.0

/* Growable in-memory output buffer. The whole drawing is assembled here
 and only written to disk at close when a path was given; without one it
 is handed back to R as a string. Plain C99, no open_memstream (absent
 on Windows). */
typedef struct {
  char *data;
  size_t len, cap;
} membuf;

static void mb_reserve(membuf *b, size_t extra) {
  if (b->len + extra + 1 <= b->cap) return;
  size_t cap = b->cap ? b->cap : 16384;
  while (b->len + extra + 1 > cap) cap *= 2;
  char *p = realloc(b->data, cap);
  if (p == NULL) {                       /* # nocov start */
    free(b->data);
    b->data = NULL; b->len = b->cap = 0;
    Rf_error("easeling: out of memory");
  }                                      /* # nocov end */
  b->data = p;
  b->cap = cap;
}

static void mb_printf(membuf *b, const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  va_list ap2;
  va_copy(ap2, ap);
  int n = vsnprintf(NULL, 0, fmt, ap);
  va_end(ap);
  if (n < 0) { va_end(ap2); Rf_error("easeling: formatting error"); } /* # nocov */
  mb_reserve(b, (size_t) n);
  vsnprintf(b->data + b->len, (size_t) n + 1, fmt, ap2);
  va_end(ap2);
  b->len += (size_t) n;
}

typedef struct {
  membuf out;
  char *path;      /* NULL: return the drawing as a string via result_env */
  SEXP result_env;
  SEXP glyph_fun;  /* easeling:::glyph_chars, or NULL without systemfonts */
  int glyph_warned_nosf, glyph_warned_unmapped;
  int shape_id;
  int page;
  double clip_x0, clip_y0, clip_x1, clip_y1;
  char fontname[201];
  /* shaped clip path (R >= 4.1 viewport(clip = grob)): one convex ring,
   captured by replaying the clip grob through the draw callbacks */
#define CLIP_RING_MAX 128
  int capturing, clip_shaped, cap_fail, ring_n, ring_convex;
  double ring_x[CLIP_RING_MAX], ring_y[CLIP_RING_MAX];
  double cap_bx0, cap_by0, cap_bx1, cap_by1;   /* bbox of ALL captured ink */
  int cap_any;
  /* hard-edged masks reduced to a second clip ring; soft or inverse
   masks cannot be represented and are ignored with a warning */
  int cap_kind;              /* 0 = clip path, 1 = mask */
  int cap_luminance;         /* mask capture: luminance semantics */
  int mask_soft, mask_hidden_ink;
  int mask_shaped, mask_n, mask_convex;
  double mask_x[CLIP_RING_MAX], mask_y[CLIP_RING_MAX];
  double mask_bx0, mask_by0, mask_bx1, mask_by1;
  double cw[95], ca[95], cd2[95];  /* optional per-char metrics, em */
  int have_metrics;
  double text_voff;
  Rboolean underline;
  Rboolean strikeout;
} xdrDesc;

static int is_generic_family(const char *f) {
  return f[0] == '\0' || strcmp(f, "sans") == 0 || strcmp(f, "serif") == 0 ||
    strcmp(f, "mono") == 0 || strcmp(f, "symbol") == 0;
}

static void esc_xml(const char *in, char *out, size_t outlen) {
  size_t j = 0;
  for (size_t i = 0; in[i] != '\0' && j + 6 < outlen; i++) {
    switch (in[i]) {
    case '&':  memcpy(out + j, "&amp;",  5); j += 5; break;
    case '<':  memcpy(out + j, "&lt;",   4); j += 4; break;
    case '>':  memcpy(out + j, "&gt;",   4); j += 4; break;
    case '"':  memcpy(out + j, "&quot;", 6); j += 6; break;
    default:
      /* control chars other than \t \n \r are not legal in XML 1.0 */
      if ((unsigned char) in[i] >= 0x20 ||
          in[i] == '\t' || in[i] == '\n' || in[i] == '\r')
        out[j++] = in[i];
    }
  }
  out[j] = '\0';
}

static unsigned int rgb_hex(int col) {
  return ((unsigned int) R_RED(col) << 16) |
    ((unsigned int) R_GREEN(col) << 8) |
    (unsigned int) R_BLUE(col);
}

static int alpha_pct(int col) {
  return (int) lround(R_ALPHA(col) / 255.0 * 100000.0);
}

static void srgb_clr(membuf *out, int col) {
  int a = alpha_pct(col);
  if (a >= 100000)
    mb_printf(out, "<a:srgbClr val=\"%06X\"/>", rgb_hex(col));
  else
    mb_printf(out, "<a:srgbClr val=\"%06X\"><a:alpha val=\"%d\"/></a:srgbClr>",
            rgb_hex(col), a);
}

static void sp_open(xdrDesc *d, const char *name) {
  mb_printf(&d->out,
          "<xdr:sp macro=\"\" textlink=\"\">"
            "<xdr:nvSpPr><xdr:cNvPr id=\"%d\" name=\"%s\"/><xdr:cNvSpPr/></xdr:nvSpPr>",
              d->shape_id++, name);
}

static void xfrm(xdrDesc *d, double x1, double y1, double x2, double y2) {
  double min_x = x1 < x2 ? x1 : x2;
  double min_y = y1 < y2 ? y1 : y2;
  double cx = fabs(x2 - x1) * PT_TO_EMU;
  double cy = fabs(y2 - y1) * PT_TO_EMU;

  mb_printf(&d->out, "<a:xfrm><a:off x=\"%.0f\" y=\"%.0f\"/><a:ext cx=\"%.0f\" cy=\"%.0f\"/></a:xfrm>",
          min_x * PT_TO_EMU, min_y * PT_TO_EMU, cx, cy);
}

static void fill_props(xdrDesc *d, int fill) {
  if (fill == NA_INTEGER || R_TRANSPARENT(fill)) {
    mb_printf(&d->out, "<a:noFill/>");
  } else {
    mb_printf(&d->out, "<a:solidFill>");
    srgb_clr(&d->out, fill);
    mb_printf(&d->out, "</a:solidFill>");
  }
}

#if R_GE_version >= 13
static void emit_gradient_stops_linear(xdrDesc *d, SEXP pattern) {
  int n = R_GE_linearGradientNumStops(pattern);
  mb_printf(&d->out, "<a:gsLst>");
  for (int i = 0; i < n; i++) {
    double stop = R_GE_linearGradientStop(pattern, i);
    rcolor col = R_GE_linearGradientColour(pattern, i);
    mb_printf(&d->out, "<a:gs pos=\"%d\">", (int) lround(stop * 100000.0));
    srgb_clr(&d->out, (int) col);
    mb_printf(&d->out, "</a:gs>");
  }
  mb_printf(&d->out, "</a:gsLst>");
}

static void emit_gradient_stops_radial(xdrDesc *d, SEXP pattern) {
  int n = R_GE_radialGradientNumStops(pattern);
  mb_printf(&d->out, "<a:gsLst>");
  for (int i = 0; i < n; i++) {
    double stop = R_GE_radialGradientStop(pattern, i);
    rcolor col = R_GE_radialGradientColour(pattern, i);
    mb_printf(&d->out, "<a:gs pos=\"%d\">", (int) lround(stop * 100000.0));
    srgb_clr(&d->out, (int) col);
    mb_printf(&d->out, "</a:gs>");
  }
  mb_printf(&d->out, "</a:gsLst>");
}
#endif

/* bx0..by1: the emitted shape's bounding box in device pt, needed to place
 the radial gradient focus (R gives absolute coords, fillToRect wants
 fractions of the shape box) */
static void fill_props_gc(xdrDesc *d, const pGEcontext gc,
                          double bx0, double by0, double bx1, double by1) {
#if R_GE_version >= 13
  if (gc->patternFill != R_NilValue && R_GE_isPattern(gc->patternFill)) {
    int type = R_GE_patternType(gc->patternFill);
    if (type == R_GE_linearGradientPattern) {
      double x1 = R_GE_linearGradientX1(gc->patternFill);
      double y1 = R_GE_linearGradientY1(gc->patternFill);
      double x2 = R_GE_linearGradientX2(gc->patternFill);
      double y2 = R_GE_linearGradientY2(gc->patternFill);
      double ang_deg = atan2(y2 - y1, x2 - x1) * 180.0 / M_PI;
      if (ang_deg < 0) ang_deg += 360.0;
      int ang_60000 = (int) round(ang_deg * 60000.0);
      mb_printf(&d->out, "<a:gradFill>");
      emit_gradient_stops_linear(d, gc->patternFill);
      mb_printf(&d->out, "<a:lin ang=\"%d\" scaled=\"0\"/></a:gradFill>", ang_60000);
      return;
    } else if (type == R_GE_radialGradientPattern) {
      /* place the focus at R's start-circle centre, as a fraction of the
       shape box; the end radius cannot be expressed - OOXML radial
       gradients always run to the shape edges */
      double w = bx1 - bx0, h = by1 - by0;
      double fx = 0.5, fy = 0.5;
      if (w > 0) fx = (R_GE_radialGradientCX1(gc->patternFill) - bx0) / w;
      if (h > 0) fy = (R_GE_radialGradientCY1(gc->patternFill) - by0) / h;
      fx = fmin(fmax(fx, 0.0), 1.0);
      fy = fmin(fmax(fy, 0.0), 1.0);
      int l = (int) lround(fx * 100000.0);
      int t = (int) lround(fy * 100000.0);
      mb_printf(&d->out, "<a:gradFill>");
      emit_gradient_stops_radial(d, gc->patternFill);
      mb_printf(&d->out,
              "<a:path path=\"circle\"><a:fillToRect l=\"%d\" t=\"%d\" "
              "r=\"%d\" b=\"%d\"/></a:path><a:tileRect/></a:gradFill>",
              l, t, 100000 - l, 100000 - t);
      return;
    }
    /* tiling patterns have no clean OOXML equivalent - fall through to noFill */
    mb_printf(&d->out, "<a:noFill/>");
    return;
  }
#endif
  fill_props(d, gc->fill);
}

/* R lty encoding: nibble-packed integers.
 LTY_SOLID=0 (no nibbles), others pack up to 8 nibbles of alternating on/off lengths.
 From GraphicsEngine.h:
 LTY_DASHED   = 4 + (4<<4)
 LTY_DOTTED   = 1 + (3<<4)
 LTY_DOTDASH  = 1 + (3<<4) + (4<<8) + (3<<12)
 LTY_LONGDASH = 7 + (3<<4)
 LTY_TWODASH  = 2 + (2<<4) + (6<<8) + (2<<12)
 We match named presets first to get nicer OOXML, then fall through to custDash. */
static void emit_dash(membuf *out, int lty, double lwd) {
  (void) lwd;
  if (lty == LTY_SOLID) return;
  /* All non-solid patterns as custDash from the R nibble encoding: the
   prstDash presets render at renderer-specific proportions that don't
   match R's actual patterns (dashed = 4-4, dotted = 1-3, ...), while
   custDash reproduces them exactly. OOXML <a:ds d= sp=> are
   ST_PositivePercentage of the line width in 1000ths of a percent,
   i.e. one line-width = 100000. R nibbles are dash/gap lengths in
   line-width units. */
  unsigned int u = (unsigned int) lty;
  int nib[8];
  int count = 0;
  for (int i = 0; i < 8; i++) {
    nib[i] = (int)((u >> (i * 4)) & 0xF);
    if (nib[i] != 0) count = i + 1;
  }
  if (count < 2) count = 2;
  if (count & 1) count++;
  mb_printf(out, "<a:custDash>");
  for (int i = 0; i + 1 < count; i += 2) {
    int d_val  = (nib[i]   > 0 ? nib[i]   : 1) * 100000;
    int sp_val = (nib[i+1] > 0 ? nib[i+1] : 1) * 100000;
    mb_printf(out, "<a:ds d=\"%d\" sp=\"%d\"/>", d_val, sp_val);
  }
  mb_printf(out, "</a:custDash>");
}

static void line_props(xdrDesc *d, int col, double lwd, int lty, int lend, int ljoin, double lmitre) {
  if (col == NA_INTEGER || R_TRANSPARENT(col) || lty == LTY_BLANK) {
    mb_printf(&d->out, "<a:ln><a:noFill/></a:ln>");
    return;
  }
  double w_emu = (lwd > 0 ? lwd : 1.0) * 0.75 * PT_TO_EMU;

  const char *cap = "rnd";
  if      (lend == GE_BUTT_CAP)   cap = "flat";
  else if (lend == GE_SQUARE_CAP) cap = "sq";

  mb_printf(&d->out, "<a:ln w=\"%.0f\" cap=\"%s\"><a:solidFill>", w_emu, cap);
  srgb_clr(&d->out, col);
  mb_printf(&d->out, "</a:solidFill>");
  emit_dash(&d->out, lty, lwd);

  switch (ljoin) {
  case GE_BEVEL_JOIN: mb_printf(&d->out, "<a:bevel/>"); break;
  case GE_MITRE_JOIN:
    /* OOXML miter lim is ST_PositivePercentage of line width; R's lmitre
     is a plain ratio, so ratio * 100000 */
    mb_printf(&d->out, "<a:miter lim=\"%.0f\"/>", lmitre * 100000.0);
    break;
  default: mb_printf(&d->out, "<a:round/>"); break;
  }

  mb_printf(&d->out, "</a:ln>");
}

static void points_to_pts(xdrDesc *d, const double *x, const double *y, int n,
                          double x0, double y0, double w_emu, double h_emu,
                          Rboolean closed) {
  mb_printf(&d->out,
          "<a:custGeom><a:avLst/><a:gdLst/><a:ahLst/><a:cxnLst/>"
          "<a:rect l=\"0\" t=\"0\" r=\"%.0f\" b=\"%.0f\"/><a:pathLst>"
          "<a:path w=\"%.0f\" h=\"%.0f\">",
          w_emu <= 0 ? 1.0 : w_emu, h_emu <= 0 ? 1.0 : h_emu,
                                           w_emu <= 0 ? 1.0 : w_emu, h_emu <= 0 ? 1.0 : h_emu);
  for (int i = 0; i < n; i++) {
    double px = (x[i] * PT_TO_EMU) - x0;
    double py = (y[i] * PT_TO_EMU) - y0;
    mb_printf(&d->out, "<a:%s><a:pt x=\"%.0f\" y=\"%.0f\"/></a:%s>",
            i == 0 ? "moveTo" : "lnTo", px, py,
            i == 0 ? "moveTo" : "lnTo");
  }
  if (closed) mb_printf(&d->out, "<a:close/>");
  mb_printf(&d->out, "</a:path></a:pathLst></a:custGeom>");
}

static Rboolean fully_outside_clip(xdrDesc *d, double x0, double y0, double x1, double y1) {
  double cx0 = fmin(d->clip_x0, d->clip_x1);
  double cx1 = fmax(d->clip_x0, d->clip_x1);
  double cy0 = fmin(d->clip_y0, d->clip_y1);
  double cy1 = fmax(d->clip_y0, d->clip_y1);
  double bx0 = fmin(x0, x1), bx1 = fmax(x0, x1);
  double by0 = fmin(y0, y1), by1 = fmax(y0, y1);
  if (bx1 < cx0 || bx0 > cx1 || by1 < cy0 || by0 > cy1) return TRUE;
  if (d->clip_shaped &&
      (bx1 < d->cap_bx0 || bx0 > d->cap_bx1 ||
       by1 < d->cap_by0 || by0 > d->cap_by1)) return TRUE;
  if (d->mask_shaped &&
      (bx1 < d->mask_bx0 || bx0 > d->mask_bx1 ||
       by1 < d->mask_by0 || by0 > d->mask_by1)) return TRUE;
  return FALSE;
}

/* Liang-Barsky line clipping. Returns FALSE if the segment is entirely
 outside. On TRUE, (x1,y1)-(x2,y2) are updated to the clipped endpoints. */
static Rboolean clip_line_lb(double cx0, double cy0, double cx1, double cy1,
                             double *x1, double *y1, double *x2, double *y2) {
  double dx = *x2 - *x1;
  double dy = *y2 - *y1;
  double t0 = 0.0, t1 = 1.0;
  double p[4], q[4];
  p[0] = -dx; q[0] = *x1 - cx0;
  p[1] =  dx; q[1] = cx1 - *x1;
  p[2] = -dy; q[2] = *y1 - cy0;
  p[3] =  dy; q[3] = cy1 - *y1;
  for (int i = 0; i < 4; i++) {
    if (p[i] == 0.0) {
      if (q[i] < 0.0) return FALSE;
    } else {
      double r = q[i] / p[i];
      if (p[i] < 0.0) { if (r > t0) t0 = r; }
      else             { if (r < t1) t1 = r; }
    }
  }
  if (t0 > t1) return FALSE;
  double nx1 = *x1 + t0 * dx, ny1 = *y1 + t0 * dy;
  double nx2 = *x1 + t1 * dx, ny2 = *y1 + t1 * dy;
  *x1 = nx1; *y1 = ny1; *x2 = nx2; *y2 = ny2;
  return TRUE;
}

/* Sutherland-Hodgman polygon clipping against one axis-aligned half-plane.
 out must have capacity >= 2*n. Returns number of output vertices. */
static int sh_clip_edge(const double *ix, const double *iy, int n,
                        double *ox, double *oy,
                        int axis, int sign, double bound) {
  /* axis=0 -> x, axis=1 -> y; sign=+1 -> keep >= bound, sign=-1 -> keep <= bound */
  int m = 0;
  for (int i = 0; i < n; i++) {
    int j = (i + 1) % n;
    double aval = (axis == 0) ? ix[i] : iy[i];
    double bval = (axis == 0) ? ix[j] : iy[j];
    int a_in = (sign > 0) ? (aval >= bound) : (aval <= bound);
    int b_in = (sign > 0) ? (bval >= bound) : (bval <= bound);
    if (a_in) { ox[m] = ix[i]; oy[m] = iy[i]; m++; }
    if (a_in != b_in) {
      double t = (bound - aval) / (bval - aval);
      ox[m] = ix[i] + t * (ix[j] - ix[i]);
      oy[m] = iy[i] + t * (iy[j] - iy[i]);
      m++;
    }
  }
  return m;
}

/* Clip a polygon ring against the axis-aligned clip rectangle.
 buf1/buf2 are scratch buffers each of size >= 2*n.
 Returns number of output vertices in ox/oy (pointing into buf1 or buf2). */
static int clip_polygon_sh(const double *x, const double *y, int n,
                           double cx0, double cy0, double cx1, double cy1,
                           double *buf1x, double *buf1y,
                           double *buf2x, double *buf2y,
                           double **ox, double **oy) {
  /* copy input into buf1 */
  for (int i = 0; i < n; i++) { buf1x[i] = x[i]; buf1y[i] = y[i]; }
  int m = n;
  m = sh_clip_edge(buf1x, buf1y, m, buf2x, buf2y, 0, +1, cx0); /* x >= cx0 */
  if (m == 0) { *ox = buf2x; *oy = buf2y; return 0; }
  m = sh_clip_edge(buf2x, buf2y, m, buf1x, buf1y, 0, -1, cx1); /* x <= cx1 */
  if (m == 0) { *ox = buf1x; *oy = buf1y; return 0; }
  m = sh_clip_edge(buf1x, buf1y, m, buf2x, buf2y, 1, +1, cy0); /* y >= cy0 */
  if (m == 0) { *ox = buf2x; *oy = buf2y; return 0; }
  m = sh_clip_edge(buf2x, buf2y, m, buf1x, buf1y, 1, -1, cy1); /* y <= cy1 */
  *ox = buf1x; *oy = buf1y;
  return m;
}

/* ---- shaped (convex-ring) clipping ---------------------------------- */

static int ring_is_convex(const double *x, const double *y, int n) {
  int pos = 0, neg = 0;
  for (int i = 0; i < n; i++) {
    int j = (i + 1) % n, k = (i + 2) % n;
    double c = (x[j] - x[i]) * (y[k] - y[j]) - (y[j] - y[i]) * (x[k] - x[j]);
    if (c > 1e-12) pos = 1;
    if (c < -1e-12) neg = 1;
    if (pos && neg) return 0;
  }
  return 1;
}

static double fill_luminance(int col) {
  return (0.299 * R_RED(col) + 0.587 * R_GREEN(col) + 0.114 * R_BLUE(col)) / 255.0;
}

/* During mask capture, decide whether this shape marks a visible region.
 Semi-transparent or gradient fills mean a soft mask; under luminance
 semantics dark ink means hide, which no clip can express. */
static int capture_visible(xdrDesc *d, const pGEcontext gc) {
  if (d->cap_kind == 0) return 1;                 /* clip path: geometry only */
#if R_GE_version >= 13
  if (gc->patternFill != R_NilValue) { d->mask_soft = 1; return 0; }
#endif
  int fill = gc->fill;
  if (fill == NA_INTEGER || R_TRANSPARENT(fill)) return 0;
  int a = R_ALPHA(fill);
  if (a > 5 && a < 250) { d->mask_soft = 1; return 0; }
  if (a <= 5) return 0;
  if (d->cap_luminance && fill_luminance(fill) < 0.5) {
    d->mask_hidden_ink = 1;
    return 0;
  }
  return 1;
}

static void capture_ring(xdrDesc *d, const double *x, const double *y, int n) {
  for (int i = 0; i < n; i++) {
    if (!d->cap_any || x[i] < d->cap_bx0) d->cap_bx0 = x[i];
    if (!d->cap_any || x[i] > d->cap_bx1) d->cap_bx1 = x[i];
    if (!d->cap_any || y[i] < d->cap_by0) d->cap_by0 = y[i];
    if (!d->cap_any || y[i] > d->cap_by1) d->cap_by1 = y[i];
    d->cap_any = 1;
  }
  if (d->ring_n > 0 || n < 3 || n > CLIP_RING_MAX) { d->cap_fail = 1; return; }
  for (int i = 0; i < n; i++) { d->ring_x[i] = x[i]; d->ring_y[i] = y[i]; }
  d->ring_n = n;
}

/* Sutherland-Hodgman against one directed ring edge; inside is where
 cross(edge, q - p) <= 0 (rings are normalised to negative signed area). */
static int sh_clip_ring_edge(const double *ix, const double *iy, int n,
                             double px, double py, double ex, double ey,
                             double *ox, double *oy) {
  int m = 0;
  double dx = ex - px, dy = ey - py;
  for (int i = 0; i < n; i++) {
    int j = (i + 1) % n;
    double fa = dx * (iy[i] - py) - dy * (ix[i] - px);
    double fb = dx * (iy[j] - py) - dy * (ix[j] - px);
    int ina = fa <= 0.0, inb = fb <= 0.0;
    if (ina) { ox[m] = ix[i]; oy[m] = iy[i]; m++; }
    if (ina != inb) {
      double t = fa / (fa - fb);
      ox[m] = ix[i] + t * (ix[j] - ix[i]);
      oy[m] = iy[i] + t * (iy[j] - iy[i]);
      m++;
    }
  }
  return m;
}

static int clip_polygon_ring(const double *x, const double *y, int n,
                             const double *rx, const double *ry, int rn,
                             double *buf1x, double *buf1y,
                             double *buf2x, double *buf2y,
                             double **ox, double **oy) {
  for (int i = 0; i < n; i++) { buf1x[i] = x[i]; buf1y[i] = y[i]; }
  int m = n;
  double *ax = buf1x, *ay = buf1y, *bx = buf2x, *by = buf2y;
  for (int e = 0; e < rn; e++) {
    int f = (e + 1) % rn;
    m = sh_clip_ring_edge(ax, ay, m, rx[e], ry[e], rx[f], ry[f], bx, by);
    double *t;
    t = ax; ax = bx; bx = t;
    t = ay; ay = by; by = t;
    if (m == 0) break;
  }
  *ox = ax; *oy = ay;
  return m;
}

/* ---- Greiner-Hormann: subject ∩ clip for arbitrary simple polygons ----
 Used when the captured clip/mask ring is non-convex. Degeneracies
 (subject vertices or edges exactly on clip edges) are broken by a tiny
 shear of the subject (~1e-9 pt at plot scale, far below EMU resolution),
 the standard pragmatic robustness fix. Self-intersecting rings are not
 supported (capture falls back to the bounding box for those cases that
 produce them: multi-ring regions). */

static int point_in_ring(double px, double py, const double *x,
                         const double *y, int n);

typedef struct gh_node {
  double x, y, alpha;
  struct gh_node *next, *prev, *neighbor;
  int intersect, entry, visited;
} gh_node;

/* callers guarantee n >= 3 (gh_clip guards its inputs) */
static gh_node *gh_make_list(const double *x, const double *y, int n,
                             gh_node *pool, int *used, double shear) {
  gh_node *first = &pool[(*used)++];
  memset(first, 0, sizeof(*first));
  first->x = x[0] + shear * y[0];
  first->y = y[0];
  gh_node *last = first;
  for (int i = 1; i < n; i++) {
    gh_node *nd = &pool[(*used)++];
    memset(nd, 0, sizeof(*nd));
    nd->x = x[i] + shear * y[i];
    nd->y = y[i];
    last->next = nd;
    nd->prev = last;
    last = nd;
  }
  last->next = first;
  first->prev = last;
  return first;
}

static void gh_insert_sorted(gh_node *edge_start, gh_node *edge_end,
                             gh_node *nd) {
  gh_node *p = edge_start;
  while (p->next != edge_end && p->next->intersect &&
         p->next->alpha < nd->alpha)
    p = p->next;
  nd->next = p->next;
  nd->prev = p;
  p->next->prev = nd;
  p->next = nd;
}

static int gh_point_in(const gh_node *ring, double px, double py) {
  int inside = 0;
  const gh_node *a = ring;
  do {
    const gh_node *b = a->next;
    if (((a->y > py) != (b->y > py)) &&
        (px < (b->x - a->x) * (py - a->y) / (b->y - a->y) + a->x))
      inside = !inside;
    a = b;
  } while (a != ring);
  return inside;
}

/* Intersect subject (sx,sy,sn) with clip ring (cx,cy,cn). Appends result
 pieces' vertices to (outx,outy) and their sizes to outn; returns the
 number of pieces (0 = empty intersection). out buffers must hold
 sn + cn + 2 * (#crossings) points and pieces. */
static int gh_clip(const double *sx, const double *sy, int sn,
                   const double *cx, const double *cy, int cn,
                   double *outx, double *outy, int *outn, int max_pts) {
  if (sn < 3 || cn < 3) return 0;
  const double SHEAR = 1e-9;
  /* count proper crossings first so node storage is exact */
  int ni = 0;
  for (int i = 0; i < sn; i++) {
    double ax = sx[i] + SHEAR * sy[i], ay = sy[i];
    int i2 = (i + 1) % sn;
    double bx = sx[i2] + SHEAR * sy[i2], by = sy[i2];
    for (int j = 0; j < cn; j++) {
      int j2 = (j + 1) % cn;
      double px = cx[j], py = cy[j], qx = cx[j2], qy = cy[j2];
      double d1 = (bx - ax) * (qy - py) - (by - ay) * (qx - px);
      if (d1 == 0.0) continue;
      double t = ((px - ax) * (qy - py) - (py - ay) * (qx - px)) / d1;
      double u = ((px - ax) * (by - ay) - (py - ay) * (bx - ax)) / d1;
      if (t > 0.0 && t < 1.0 && u > 0.0 && u < 1.0) ni++;
    }
  }
  if (ni == 0) {
    double s0x = sx[0] + SHEAR * sy[0];
    /* fully inside / outside / clip inside subject */
    int s_in_c = 0, c_in_s = 0;
    {
      int inside = 0;
      for (int j = 0, k = cn - 1; j < cn; k = j++) {
        if (((cy[j] > sy[0]) != (cy[k] > sy[0])) &&
            (s0x < (cx[k] - cx[j]) * (sy[0] - cy[j]) / (cy[k] - cy[j]) + cx[j]))
          inside = !inside;
      }
      s_in_c = inside;
    }
    if (s_in_c) {
      if (sn > max_pts) return 0;
      for (int i = 0; i < sn; i++) { outx[i] = sx[i]; outy[i] = sy[i]; }
      outn[0] = sn;
      return 1;
    }
    {
      int inside = 0;
      for (int i = 0, k = sn - 1; i < sn; k = i++) {
        if (((sy[i] > cy[0]) != (sy[k] > cy[0])) &&
            (cx[0] < (sx[k] - sx[i]) * (cy[0] - sy[i]) / (sy[k] - sy[i]) + sx[i]))
          inside = !inside;
      }
      c_in_s = inside;
    }
    if (c_in_s) {
      if (cn > max_pts) return 0;
      for (int j = 0; j < cn; j++) { outx[j] = cx[j]; outy[j] = cy[j]; }
      outn[0] = cn;
      return 1;
    }
    return 0;
  }

  int npool = sn + cn + 2 * ni;
  gh_node *pool = (gh_node *) R_alloc((size_t) npool, sizeof(gh_node));
  int used = 0;
  gh_node *S = gh_make_list(sx, sy, sn, pool, &used, SHEAR);
  gh_node *C = gh_make_list(cx, cy, cn, pool, &used, 0.0);

  /* create and link intersection nodes */
  gh_node *sa = S;
  for (int i = 0; i < sn; i++) {
    gh_node *sb = sa;
    do { sb = sb->next; } while (sb->intersect);
    gh_node *ca = C;
    for (int j = 0; j < cn; j++) {
      gh_node *cb = ca;
      do { cb = cb->next; } while (cb->intersect);
      double ax = sa->x, ay = sa->y, bx = sb->x, by = sb->y;
      double px = ca->x, py = ca->y, qx = cb->x, qy = cb->y;
      double d1 = (bx - ax) * (qy - py) - (by - ay) * (qx - px);
      if (d1 != 0.0) {
        double t = ((px - ax) * (qy - py) - (py - ay) * (qx - px)) / d1;
        double u = ((px - ax) * (by - ay) - (py - ay) * (bx - ax)) / d1;
        if (t > 0.0 && t < 1.0 && u > 0.0 && u < 1.0) {
          gh_node *is = &pool[used++];
          gh_node *ic = &pool[used++];
          memset(is, 0, sizeof(*is));
          memset(ic, 0, sizeof(*ic));
          is->x = ic->x = ax + t * (bx - ax);
          is->y = ic->y = ay + t * (by - ay);
          is->alpha = t;
          ic->alpha = u;
          is->intersect = ic->intersect = 1;
          is->neighbor = ic;
          ic->neighbor = is;
          gh_insert_sorted(sa, sb, is);
          gh_insert_sorted(ca, cb, ic);
        }
      }
      ca = cb;
    }
    sa = sb;
  }

  /* mark entry/exit by alternating from the containment of each start */
  int status = !gh_point_in(C, S->x, S->y);   /* 1 = next crossing enters */
  for (gh_node *p = S;;) {
    if (p->intersect) { p->entry = status; status = !status; }
    p = p->next;
    if (p == S) break;
  }
  status = !gh_point_in(S, C->x, C->y);
  for (gh_node *p = C;;) {
    if (p->intersect) { p->entry = status; status = !status; }
    p = p->next;
    if (p == C) break;
  }

  /* trace result pieces */
  int npieces = 0, total = 0;
  for (;;) {
    gh_node *start = NULL;
    for (gh_node *p = S;;) {
      if (p->intersect && !p->visited) { start = p; break; }
      p = p->next;
      if (p == S) break;
    }
    if (!start) break;
    int count = 0;
    gh_node *cur = start;
    do {
      cur->visited = 1;
      cur->neighbor->visited = 1;
      if (cur->entry) {
        do {
          if (total >= max_pts) return npieces;
          outx[total] = cur->x;
          outy[total] = cur->y;
          total++;
          count++;
          cur = cur->next;
        } while (!cur->intersect);
      } else {
        do {
          if (total >= max_pts) return npieces;
          outx[total] = cur->x;
          outy[total] = cur->y;
          total++;
          count++;
          cur = cur->prev;
        } while (!cur->intersect);
      }
      cur = cur->neighbor;
    } while (!cur->visited);
    if (count >= 3) outn[npieces++] = count;
    else total -= count;                       /* # nocov - tangential sliver */
  }
  return npieces;
}

/* Segment vs arbitrary simple ring: writes up to rn inside sub-segments as
 t pairs into ts; returns the pair count. */
static int clip_segment_ring_any(const double *rx, const double *ry, int rn,
                                 double ax, double ay, double bx, double by,
                                 double *ts) {
  double cand[2 + CLIP_RING_MAX];
  int nc = 0;
  cand[nc++] = 0.0;
  double dx = bx - ax, dy = by - ay;
  for (int j = 0; j < rn; j++) {
    int j2 = (j + 1) % rn;
    double px = rx[j], py = ry[j], qx = rx[j2], qy = ry[j2];
    double d1 = dx * (qy - py) - dy * (qx - px);
    if (d1 == 0.0) continue;
    double t = ((px - ax) * (qy - py) - (py - ay) * (qx - px)) / d1;
    double u = ((px - ax) * dy - (py - ay) * dx) / d1;
    if (t > 0.0 && t < 1.0 && u >= 0.0 && u <= 1.0) cand[nc++] = t;
  }
  cand[nc++] = 1.0;
  /* insertion sort of the small candidate list */
  for (int i = 1; i < nc; i++) {
    double v = cand[i];
    int j = i - 1;
    while (j >= 0 && cand[j] > v) { cand[j + 1] = cand[j]; j--; }
    cand[j + 1] = v;
  }
  int k = 0;
  for (int i = 0; i + 1 < nc; i++) {
    double t0 = cand[i], t1 = cand[i + 1];
    if (t1 - t0 < 1e-12) continue;
    double mt = (t0 + t1) / 2.0;
    if (point_in_ring(ax + mt * dx, ay + mt * dy, rx, ry, rn)) {
      ts[2 * k] = t0;
      ts[2 * k + 1] = t1;
      k++;
    }
  }
  return k;
}

/* Cyrus-Beck: clip a segment to the convex ring; FALSE if fully outside. */
static Rboolean clip_line_ring(const xdrDesc *d, const double *rx,
                               const double *ry, int rn,
                               double *x1, double *y1,
                               double *x2, double *y2) {
  (void) d;
  double t0 = 0.0, t1 = 1.0;
  double sx = *x1, sy = *y1, dxs = *x2 - *x1, dys = *y2 - *y1;
  for (int e = 0; e < rn; e++) {
    int f = (e + 1) % rn;
    double px = rx[e], py = ry[e];
    double dx = rx[f] - px, dy = ry[f] - py;
    double fa = dx * (sy - py) - dy * (sx - px);
    double fb = dx * (sy + dys - py) - dy * (sx + dxs - px);
    if (fa > 0.0 && fb > 0.0) return FALSE;
    if (fa > 0.0 || fb > 0.0) {
      double t = fa / (fa - fb);
      if (fa > 0.0) { if (t > t0) t0 = t; }
      else          { if (t < t1) t1 = t; }
      if (t0 > t1) return FALSE;
    }
  }
  *x1 = sx + t0 * dxs; *y1 = sy + t0 * dys;
  *x2 = sx + t1 * dxs; *y2 = sy + t1 * dys;
  return TRUE;
}


/* Clip a polygon against one ring, convex or not. For convex rings this
 is Sutherland-Hodgman (one piece); otherwise Greiner-Hormann, which can
 return several pieces. Appends to out arrays; returns piece count. */
static int clip_ring_dispatch(const double *x, const double *y, int n,
                              const double *rx, const double *ry, int rn,
                              int convex,
                              double *outx, double *outy, int *outn,
                              int max_pts) {
  if (convex) {
    int cap = 2 * (n + rn) + 8;
    double *b1x = (double *) R_alloc((size_t) cap, sizeof(double));
    double *b1y = (double *) R_alloc((size_t) cap, sizeof(double));
    double *b2x = (double *) R_alloc((size_t) cap, sizeof(double));
    double *b2y = (double *) R_alloc((size_t) cap, sizeof(double));
    double *ox, *oy;
    int m = clip_polygon_ring(x, y, n, rx, ry, rn, b1x, b1y, b2x, b2y, &ox, &oy);
    if (m < 3 || m > max_pts) return 0;
    for (int i = 0; i < m; i++) { outx[i] = ox[i]; outy[i] = oy[i]; }
    outn[0] = m;
    return 1;
  }
  return gh_clip(x, y, n, rx, ry, rn, outx, outy, outn, max_pts);
}

/* Clips against the rect and (if active) the clip and mask rings.
 Fills piece arrays (vertices flattened into px/py, sizes in pn);
 returns the piece count. Buffers via R_alloc. */
static int dev_clip_polygon_multi(const xdrDesc *d, const double *x,
                                  const double *y, int n,
                                  double **px, double **py, int **pn) {
  double cx0 = fmin(d->clip_x0, d->clip_x1), cx1 = fmax(d->clip_x0, d->clip_x1);
  double cy0 = fmin(d->clip_y0, d->clip_y1), cy1 = fmax(d->clip_y0, d->clip_y1);
  int rings = (d->clip_shaped ? d->ring_n : 0) + (d->mask_shaped ? d->mask_n : 0);
  int max_pts = 4 * (n + rings + 16);
  double *ax = (double *) R_alloc((size_t) max_pts, sizeof(double));
  double *ay = (double *) R_alloc((size_t) max_pts, sizeof(double));
  double *bx2 = (double *) R_alloc((size_t) max_pts, sizeof(double));
  double *by2 = (double *) R_alloc((size_t) max_pts, sizeof(double));
  int *an = (int *) R_alloc((size_t) max_pts, sizeof(int));
  int *bn = (int *) R_alloc((size_t) max_pts, sizeof(int));

  int cap = 2 * (n + 4) + 8;
  double *b1x = (double *) R_alloc((size_t) cap, sizeof(double));
  double *b1y = (double *) R_alloc((size_t) cap, sizeof(double));
  double *b2x = (double *) R_alloc((size_t) cap, sizeof(double));
  double *b2y = (double *) R_alloc((size_t) cap, sizeof(double));
  double *ox, *oy;
  int m = clip_polygon_sh(x, y, n, cx0, cy0, cx1, cy1, b1x, b1y, b2x, b2y, &ox, &oy);
  if (m < 3) return 0;
  int np = 1;
  for (int i = 0; i < m; i++) { ax[i] = ox[i]; ay[i] = oy[i]; }
  an[0] = m;

  for (int pass = 0; pass < 2; pass++) {
    const double *rx, *ry;
    int rn, convex, active;
    if (pass == 0) {
      active = d->clip_shaped;
      rx = d->ring_x; ry = d->ring_y; rn = d->ring_n; convex = d->ring_convex;
    } else {
      active = d->mask_shaped;
      rx = d->mask_x; ry = d->mask_y; rn = d->mask_n; convex = d->mask_convex;
    }
    if (!active) continue;
    int out_np = 0, out_total = 0, in_off = 0;
    for (int p = 0; p < np; p++) {
      int got = clip_ring_dispatch(ax + in_off, ay + in_off, an[p],
                                   rx, ry, rn, convex,
                                   bx2 + out_total, by2 + out_total,
                                   bn + out_np, max_pts - out_total);
      for (int g = 0; g < got; g++) out_total += bn[out_np + g];
      out_np += got;
      in_off += an[p];
    }
    if (out_np == 0) return 0;
    double *t;
    int *tn;
    t = ax; ax = bx2; bx2 = t;
    t = ay; ay = by2; by2 = t;
    tn = an; an = bn; bn = tn;
    np = out_np;
  }
  *px = ax;
  *py = ay;
  *pn = an;
  return np;
}


static void bbox(const double *x, const double *y, int n, double *x0, double *y0,
                 double *x1, double *y1) {
  *x0 = *x1 = x[0]; *y0 = *y1 = y[0];
  for (int i = 1; i < n; i++) {
    if (x[i] < *x0) *x0 = x[i];
    if (x[i] > *x1) *x1 = x[i];
    if (y[i] < *y0) *y0 = y[i];
    if (y[i] > *y1) *y1 = y[i];
  }
}

static void emit_polyline_shape(xdrDesc *d, const double *x, const double *y,
                                int n, const pGEcontext gc) {
  double x0, y0, x1, y1;
  bbox(x, y, n, &x0, &y0, &x1, &y1);
  sp_open(d, "");
  mb_printf(&d->out, "<xdr:spPr>");
  xfrm(d, x0, y0, x1, y1);
  points_to_pts(d, x, y, n, x0 * PT_TO_EMU, y0 * PT_TO_EMU,
                (x1 - x0) * PT_TO_EMU, (y1 - y0) * PT_TO_EMU, FALSE);
  mb_printf(&d->out, "<a:noFill/>");
  line_props(d, gc->col, gc->lwd, gc->lty, gc->lend, gc->ljoin, gc->lmitre);
  mb_printf(&d->out, "</xdr:spPr><xdr:txBody><a:bodyPr/><a:lstStyle/><a:p/></xdr:txBody></xdr:sp>\n");
}

/* Clip an open polyline and emit it as one shape per maximal visible run,
 so joins between consecutive segments render correctly and dash patterns
 don't restart at every vertex. */
static void emit_clipped_polyline(xdrDesc *d, const double *x, const double *y,
                                  int n, const pGEcontext gc) {
  if (n < 2) return;
  if (gc->col == NA_INTEGER || R_TRANSPARENT(gc->col) || gc->lty == LTY_BLANK)
    return;

  double *rx = (double *) R_alloc((size_t) n, sizeof(double));
  double *ry = (double *) R_alloc((size_t) n, sizeof(double));
  int m = 0;
  const double eps = 1e-9;

  for (int i = 0; i < n - 1; i++) {
    double ax = x[i], ay = y[i], bx = x[i + 1], by = y[i + 1];
    double cx0 = fmin(d->clip_x0, d->clip_x1), cx1 = fmax(d->clip_x0, d->clip_x1);
    double cy0 = fmin(d->clip_y0, d->clip_y1), cy1 = fmax(d->clip_y0, d->clip_y1);
    if (!clip_line_lb(cx0, cy0, cx1, cy1, &ax, &ay, &bx, &by)) {
      if (m >= 2) emit_polyline_shape(d, rx, ry, m, gc);
      m = 0;
      continue;
    }
    /* sub-segments after ring clipping; one full segment when no rings */
    double ts[2 * (2 + CLIP_RING_MAX)];
    int nseg = 1;
    ts[0] = 0.0;
    ts[1] = 1.0;
    for (int pass = 0; pass < 2; pass++) {
      const double *rgx, *rgy;
      int rgn, convex, active;
      if (pass == 0) {
        active = d->clip_shaped;
        rgx = d->ring_x; rgy = d->ring_y; rgn = d->ring_n; convex = d->ring_convex;
      } else {
        active = d->mask_shaped;
        rgx = d->mask_x; rgy = d->mask_y; rgn = d->mask_n; convex = d->mask_convex;
      }
      if (!active) continue;
      double nts[2 * (2 + CLIP_RING_MAX)];
      int nn = 0;
      for (int sgi = 0; sgi < nseg; sgi++) {
        double sax = ax + ts[2 * sgi] * (bx - ax);
        double say = ay + ts[2 * sgi] * (by - ay);
        double sbx = ax + ts[2 * sgi + 1] * (bx - ax);
        double sby = ay + ts[2 * sgi + 1] * (by - ay);
        if (convex) {
          double lax = sax, lay = say, lbx = sbx, lby = sby;
          if (clip_line_ring(d, rgx, rgy, rgn, &lax, &lay, &lbx, &lby)) {
            /* recover ts relative to the original segment */
            double denom = (fabs(bx - ax) > fabs(by - ay))
                             ? (bx - ax) : (by - ay);
            double base = (fabs(bx - ax) > fabs(by - ay)) ? ax : ay;
            double va = (fabs(bx - ax) > fabs(by - ay)) ? lax : lay;
            double vb = (fabs(bx - ax) > fabs(by - ay)) ? lbx : lby;
            if (denom != 0.0 && nn < 2 + CLIP_RING_MAX) {
              nts[2 * nn] = (va - base) / denom;
              nts[2 * nn + 1] = (vb - base) / denom;
              nn++;
            }
          }
        } else {
          double sub[2 * (2 + CLIP_RING_MAX)];
          int k = clip_segment_ring_any(rgx, rgy, rgn, sax, say, sbx, sby, sub);
          for (int q = 0; q < k && nn < 2 + CLIP_RING_MAX; q++) {
            double span = ts[2 * sgi + 1] - ts[2 * sgi];
            nts[2 * nn] = ts[2 * sgi] + sub[2 * q] * span;
            nts[2 * nn + 1] = ts[2 * sgi] + sub[2 * q + 1] * span;
            nn++;
          }
        }
      }
      nseg = nn;
      memcpy(ts, nts, sizeof(double) * 2 * (size_t) nn);
      if (nseg == 0) break;
    }
    if (nseg == 0) {
      if (m >= 2) emit_polyline_shape(d, rx, ry, m, gc);
      m = 0;
      continue;
    }
    for (int sgi = 0; sgi < nseg; sgi++) {
      double sax = ax + ts[2 * sgi] * (bx - ax);
      double say = ay + ts[2 * sgi] * (by - ay);
      double sbx = ax + ts[2 * sgi + 1] * (bx - ax);
      double sby = ay + ts[2 * sgi + 1] * (by - ay);
      if (m > 0 && fabs(rx[m - 1] - sax) < eps && fabs(ry[m - 1] - say) < eps) {
        rx[m] = sbx;
        ry[m] = sby;
        m++;
      } else {
        if (m >= 2) emit_polyline_shape(d, rx, ry, m, gc);
        rx[0] = sax;
        ry[0] = say;
        rx[1] = sbx;
        ry[1] = sby;
        m = 2;
      }
    }
  }
  if (m >= 2) emit_polyline_shape(d, rx, ry, m, gc);
}

static void emit_polygon_shape(xdrDesc *d, const double *x, const double *y,
                               int m, const pGEcontext gc, int with_border) {
  double x0, y0, x1, y1;
  bbox(x, y, m, &x0, &y0, &x1, &y1);
  sp_open(d, "");
  mb_printf(&d->out, "<xdr:spPr>");
  xfrm(d, x0, y0, x1, y1);
  points_to_pts(d, x, y, m, x0 * PT_TO_EMU, y0 * PT_TO_EMU,
                (x1 - x0) * PT_TO_EMU, (y1 - y0) * PT_TO_EMU, TRUE);
  fill_props_gc(d, gc, x0, y0, x1, y1);
  if (with_border)
    line_props(d, gc->col, gc->lwd, gc->lty, gc->lend, gc->ljoin, gc->lmitre);
  else
    mb_printf(&d->out, "<a:ln><a:noFill/></a:ln>");
  mb_printf(&d->out, "</xdr:spPr><xdr:txBody><a:bodyPr/><a:lstStyle/><a:p/></xdr:txBody></xdr:sp>\n");
}

/* Clip a closed ring and emit it. If the clip actually cut the ring, the
 fill is emitted without a stroke (so edges introduced by the clip aren't
 drawn) and the surviving pieces of the original outline are stroked as
 separate polylines. */
static void emit_clipped_ring(xdrDesc *d, const double *x, const double *y,
                              int n, const pGEcontext gc) {
  if (n < 2) return;
  double *px, *py;
  int *pn;
  int np = dev_clip_polygon_multi(d, x, y, n, &px, &py, &pn);
  if (np < 1) return;

  int cut = (np != 1 || pn[0] != n);
  if (!cut) {
    for (int i = 0; i < n; i++) {
      if (px[i] != x[i] || py[i] != y[i]) { cut = 1; break; }
    }
  }
  int has_border = !(gc->col == NA_INTEGER || R_TRANSPARENT(gc->col) ||
                     gc->lty == LTY_BLANK);

  int off = 0;
  for (int p = 0; p < np; p++) {
    emit_polygon_shape(d, px + off, py + off, pn[p], gc, has_border && !cut);
    off += pn[p];
  }

  if (cut && has_border) {
    double *rx = (double *) R_alloc((size_t) n + 1, sizeof(double));
    double *ry = (double *) R_alloc((size_t) n + 1, sizeof(double));
    for (int i = 0; i < n; i++) { rx[i] = x[i]; ry[i] = y[i]; }
    rx[n] = x[0]; ry[n] = y[0];
    emit_clipped_polyline(d, rx, ry, n + 1, gc);
  }
}

static void emit_filled_quad_piece(xdrDesc *d, const double *ox,
                                   const double *oy, int m, int col) {
  if (m < 3) return;
  double x0, y0, x1, y1;
  bbox(ox, oy, m, &x0, &y0, &x1, &y1);
  sp_open(d, "");
  mb_printf(&d->out, "<xdr:spPr>");
  xfrm(d, x0, y0, x1, y1);
  points_to_pts(d, ox, oy, m, x0 * PT_TO_EMU, y0 * PT_TO_EMU,
                (x1 - x0) * PT_TO_EMU, (y1 - y0) * PT_TO_EMU, TRUE);
  fill_props(d, col);
  mb_printf(&d->out, "<a:ln><a:noFill/></a:ln>");
  mb_printf(&d->out, "</xdr:spPr><xdr:txBody><a:bodyPr/><a:lstStyle/><a:p/></xdr:txBody></xdr:sp>\n");
}

static void emit_filled_quad(xdrDesc *d, const double *qx, const double *qy, int col) {
  double *ppx, *ppy;
  int *ppn;
  int np = dev_clip_polygon_multi(d, qx, qy, 4, &ppx, &ppy, &ppn);
  if (np < 1) return;
  int off = ppn[0];
  for (int p = 1; p < np; p++) {
    emit_filled_quad_piece(d, ppx + off, ppy + off, ppn[p], col);
    off += ppn[p];
  }
  double *ox = ppx, *oy = ppy;
  int m = ppn[0];
  if (m < 3) return;
  double x0, y0, x1, y1;
  bbox(ox, oy, m, &x0, &y0, &x1, &y1);
  sp_open(d, "");
  mb_printf(&d->out, "<xdr:spPr>");
  xfrm(d, x0, y0, x1, y1);
  points_to_pts(d, ox, oy, m, x0 * PT_TO_EMU, y0 * PT_TO_EMU,
                (x1 - x0) * PT_TO_EMU, (y1 - y0) * PT_TO_EMU, TRUE);
  fill_props(d, col);
  mb_printf(&d->out, "<a:ln><a:noFill/></a:ln>");
  mb_printf(&d->out, "</xdr:spPr><xdr:txBody><a:bodyPr/><a:lstStyle/><a:p/></xdr:txBody></xdr:sp>\n");
}

static void Xdr_Raster(unsigned int *raster, int w, int h,
                       double x, double y, double width, double height,
                       double rot, Rboolean interpolate,
                       const pGEcontext gc, pDevDesc dd) {
  (void) interpolate; (void) gc;
  xdrDesc *d = (xdrDesc *) dd->deviceSpecific;
  if (d->capturing || w <= 0 || h <= 0) return;

  int rotated = fabs(rot) > 1e-4;
  double left  = fmin(x, x + width);
  double right = fmax(x, x + width);
  double top   = fmin(y, y + height);
  double bot   = fmax(y, y + height);
  if (!rotated && fully_outside_clip(d, left, top, right, bot)) return;

  double cell_w = (right - left) / (double) w;
  double cell_h = (bot - top) / (double) h;
  double sin_r = 0.0, cos_r = 1.0;
  if (rotated) {
    /* rot is degrees anticlockwise on screen about (x, y); device y runs
     down, hence the sign flip on the y term below */
    double t = rot * M_PI / 180.0;
    sin_r = sin(t);
    cos_r = cos(t);
  }

  for (int j = 0; j < h; j++) {
    int i = 0;
    while (i < w) {
      unsigned int col = raster[(size_t) j * (size_t) w + (size_t) i];
      int run = 1;
      while (i + run < w &&
             raster[(size_t) j * (size_t) w + (size_t) (i + run)] == col) run++;
      if (!R_TRANSPARENT((int) col)) {
        double rx0 = left + i * cell_w;
        double rx1 = left + (i + run) * cell_w;
        double ry0 = top + j * cell_h;
        double ry1 = top + (j + 1) * cell_h;
        /* Opaque cells bleed half a cell right/down (renderers antialias
         the seams between adjacent rects otherwise); the neighbouring
         cells are drawn later and paint over the overlap. Translucent
         cells can't overlap or the seams would double up instead. */
        if (R_ALPHA((int) col) == 255) {
          if (i + run < w) rx1 += cell_w * 0.5;
          if (j + 1 < h)   ry1 += cell_h * 0.5;
        }
        if (!rotated) {
          sp_open(d, "");
          mb_printf(&d->out, "<xdr:spPr>");
          xfrm(d, rx0, ry0, rx1, ry1);
          mb_printf(&d->out, "<a:prstGeom prst=\"rect\"><a:avLst/></a:prstGeom>");
          fill_props(d, (int) col);
          mb_printf(&d->out, "<a:ln><a:noFill/></a:ln>");
          mb_printf(&d->out, "</xdr:spPr><xdr:txBody><a:bodyPr/><a:lstStyle/><a:p/></xdr:txBody></xdr:sp>\n");
        } else {
          double qx[4] = {rx0, rx1, rx1, rx0};
          double qy[4] = {ry0, ry0, ry1, ry1};
          double px[4], py[4];
          for (int k = 0; k < 4; k++) {
            double dx = qx[k] - x, dy = qy[k] - y;
            px[k] = x + dx * cos_r + dy * sin_r;
            py[k] = y - dx * sin_r + dy * cos_r;
          }
          emit_filled_quad(d, px, py, (int) col);
        }
      }
      i += run;
    }
  }
}

static void Xdr_Activate(const pDevDesc dd) { (void) dd; }
static void Xdr_Deactivate(pDevDesc dd) { (void) dd; }
static void Xdr_Mode(int mode, pDevDesc dd) { (void) mode; (void) dd; }

static void Xdr_NewPage(const pGEcontext gc, pDevDesc dd) {
  xdrDesc *d = (xdrDesc *) dd->deviceSpecific;
  d->clip_shaped = 0;
  d->mask_shaped = 0;
  d->page++;
  if (d->page == 2)
    Rf_warning("easeling writes a single drawing; additional pages are drawn on top of the first");
  if (gc->fill != NA_INTEGER && !R_TRANSPARENT(gc->fill)) {
    sp_open(d, "");
    mb_printf(&d->out, "<xdr:spPr>");
    xfrm(d, 0.0, 0.0, dd->right, dd->bottom);
    mb_printf(&d->out, "<a:prstGeom prst=\"rect\"><a:avLst/></a:prstGeom>");
    fill_props(d, gc->fill);
    mb_printf(&d->out, "<a:ln><a:noFill/></a:ln>");
    mb_printf(&d->out, "</xdr:spPr><xdr:txBody><a:bodyPr/><a:lstStyle/><a:p/></xdr:txBody></xdr:sp>\n");
  }
}

static void Xdr_Close(pDevDesc dd) {
  xdrDesc *d = (xdrDesc *) dd->deviceSpecific;

  mb_printf(&d->out, "</xdr:grpSp><xdr:clientData/></xdr:absoluteAnchor></xdr:wsDr>");

  if (d->path != NULL) {
    FILE *fp = fopen(d->path, "w");
    if (fp == NULL) {
      Rf_warning("easeling: cannot open '%s'", d->path);
    } else {
      size_t written = fwrite(d->out.data, 1, d->out.len, fp);
      if (written != d->out.len || fclose(fp) != 0)
        Rf_warning("easeling: error writing output file");
    }
    free(d->path);
  } else {
    SEXP str = PROTECT(Rf_ScalarString(
      Rf_mkCharLenCE(d->out.data, (int) d->out.len, CE_UTF8)));
    Rf_defineVar(Rf_install("xml"), str, d->result_env);
    UNPROTECT(1);
    R_ReleaseObject(d->result_env);
  }
  if (d->glyph_fun != NULL) R_ReleaseObject(d->glyph_fun);
  free(d->out.data);
  free(d);
}

static void Xdr_Clip(double x0, double x1, double y0, double y1, pDevDesc dd) {
  xdrDesc *d = (xdrDesc *) dd->deviceSpecific;
  /* a rect clip is a full clip reset; the engine re-issues setClipPath
   afterwards when a path clip is still meant to apply (cairo protocol) */
  d->clip_shaped = 0;
  d->clip_x0 = x0; d->clip_x1 = x1;
  d->clip_y0 = y0; d->clip_y1 = y1;
}

static void Xdr_Size(double *left, double *right, double *bottom, double *top,
                     pDevDesc dd) {
  *left = dd->left; *right = dd->right;
  *bottom = dd->bottom; *top = dd->top;
}

/* Per-character width as a fraction of pointsize, measured from a real
 Cairo-backed sans-serif font. A flat average (the previous 0.55
 constant) diverges enough from real widths that callers doing their
 own centering math (e.g. tinyplot's facet strip labels, which query
 strwidth() to pre-center text before left-aligning it) land visibly
 off. Index 0 covers ASCII space (32) through '~' (126); anything
 outside that range falls back to FALLBACK_CHAR_W. */
/* Hand-tuned approximate advance widths for a generic sans face, in em.
 These are original round-number estimates for this package; they are not
 derived from any font's metric files (compare e.g. Helvetica AFM: H 722,
 digits 556 - this table uses 750/583). Real metrics come from the
 optional systemfonts path or a user-supplied table. */
#define FALLBACK_CHAR_W 0.55

static const double CHAR_W[126 - 32 + 1] = {
  /* ' ' !  "    #    $    %    &    '    (    )    *    +    ,    -    .    / */
  0.25,0.25,0.30,0.55,0.55,0.85,0.65,0.20,0.30,0.30,0.35,0.55,0.25,0.30,0.25,0.30,
  /* 0    1    2    3    4    5    6    7    8    9    :    ;    <    =    >    ? */
  0.583,0.583,0.583,0.583,0.583,0.583,0.583,0.583,0.583,0.583,0.25,0.25,0.55,0.55,0.55,0.50,
  /* @    A    B    C    D    E    F    G    H    I    J    K    L    M    N    O */
  0.90,0.667,0.667,0.750,0.750,0.667,0.583,0.750,0.750,0.250,0.500,0.667,0.583,0.833,0.750,0.750,
  /* P    Q    R    S    T    U    V    W    X    Y    Z    [    \    ]    ^    _ */
  0.667,0.750,0.750,0.667,0.583,0.750,0.667,0.917,0.667,0.667,0.583,0.30,0.30,0.30,0.55,0.50,
  /* `    a    b    c    d    e    f    g    h    i    j    k    l    m    n    o */
  0.30,0.583,0.583,0.500,0.583,0.583,0.250,0.583,0.583,0.250,0.250,0.500,0.250,0.833,0.583,0.583,
  /* p    q    r    s    t    u    v    w    x    y    z    {    |    }    ~ */
  0.583,0.583,0.333,0.500,0.250,0.583,0.500,0.750,0.500,0.500,0.500,0.35,0.25,0.35,0.55
};

/* callers guarantee 32 <= c <= 126 (see dev_char_w) */
static double char_width_frac(unsigned char c) {
  return CHAR_W[c - 32];
}

static void char_vmetrics(int c, double *asc, double *desc);

static double dev_char_w(const xdrDesc *d, int c) {
  if (c < 32 || c > 126) return FALLBACK_CHAR_W;
  if (d->have_metrics) return d->cw[c - 32];
  return char_width_frac((unsigned char) c);
}

static void dev_char_v(const xdrDesc *d, int c, double *asc, double *desc) {
  if (d->have_metrics && c >= 32 && c <= 126) {
    *asc = d->ca[c - 32];
    *desc = d->cd2[c - 32];
    return;
  }
  char_vmetrics(c, asc, desc);
}

static double Xdr_StrWidth(const char *str, const pGEcontext gc, pDevDesc dd) {
  const xdrDesc *d = (const xdrDesc *) dd->deviceSpecific;
  double sz = gc->cex * gc->ps;
  double total = 0.0;
  for (const unsigned char *p = (const unsigned char *) str; *p != '\0'; p++) {
    /* Skip UTF-8 continuation bytes (10xxxxxx); treat each lead byte as
     one glyph at the fallback width, since we don't have real metrics
     for non-ASCII text. */
    if ((*p & 0xC0) == 0x80) continue;
    total += dev_char_w(d, (int) *p);
  }
  return total * sz;
}

/* Approximate per-character ink extents for a Calibri-like sans font, in
 em. R centres and stacks text using these (GEText vertical justification
 takes the max ascent/descent over the string's characters), so a flat
 0.75/0.25 for every character put descender-less strings ~0.07em off
 centre. Rendering position itself only depends on the baseline. */
static void char_vmetrics(int c, double *asc, double *desc) {
  *asc = 0.75; *desc = 0.21;              /* generous default (non-ASCII) */
  if (c < 32 || c > 126) return;
  char ch = (char) c;
  *desc = 0.0;
  if (strchr("gjpqy", ch))            *desc = 0.21;
  else if (strchr("()[]{}/\\|@$;,", ch)) *desc = 0.12;
  else if (ch == '_')                 *desc = 0.10;

  if (ch == ' ')                      *asc = 0.0;
  else if (strchr("acemnorsuvwxz", ch)) *asc = 0.47;
  else if (strchr("gpqy", ch))        *asc = 0.47;
  else if (strchr("ij", ch))          *asc = 0.64;
  else if (strchr(".,_", ch))         *asc = 0.12;
  else if (strchr("-~=+<>*:;", ch))   *asc = 0.50;
  else if (strchr("()[]{}/\\|$", ch)) *asc = 0.70;
  else                                *asc = 0.66;  /* caps, digits, rest */
}

static void Xdr_MetricInfo(int c, const pGEcontext gc, double *ascent,
                           double *descent, double *width, pDevDesc dd) {
  double sz = gc->cex * gc->ps;
  xdrDesc *d = (xdrDesc *) dd->deviceSpecific;
  if (c < 0) c = -c; /* negative c is a Unicode code point */
  /* c == 0 asks for whole-font metrics and lands on the generous default */
  double a, d2;
  dev_char_v(d, c, &a, &d2);
  *ascent  = a * sz;
  *descent = d2 * sz;
  *width   = dev_char_w(d, c) * sz;
}

static void Xdr_Line(double x1, double y1, double x2, double y2,
                     const pGEcontext gc, pDevDesc dd) {
  xdrDesc *d = (xdrDesc *) dd->deviceSpecific;
  if (d->capturing) return;
  double px[2] = {x1, x2};
  double py[2] = {y1, y2};
  emit_clipped_polyline(d, px, py, 2, gc);
}

static void Xdr_Rect(double x0, double y0, double x1, double y1,
                     const pGEcontext gc, pDevDesc dd) {
  xdrDesc *d = (xdrDesc *) dd->deviceSpecific;
  if (d->capturing) {
    if (capture_visible(d, gc)) {
      double qx[4] = {x0, x1, x1, x0};
      double qy[4] = {y0, y0, y1, y1};
      capture_ring(d, qx, qy, 4);
    }
    return;
  }
  if (d->clip_shaped || d->mask_shaped) {
    double qx[4] = {fmin(x0, x1), fmax(x0, x1), fmax(x0, x1), fmin(x0, x1)};
    double qy[4] = {fmin(y0, y1), fmin(y0, y1), fmax(y0, y1), fmax(y0, y1)};
    emit_clipped_ring(d, qx, qy, 4, gc);
    return;
  }
  double cx0 = fmin(d->clip_x0, d->clip_x1), cx1 = fmax(d->clip_x0, d->clip_x1);
  double cy0 = fmin(d->clip_y0, d->clip_y1), cy1 = fmax(d->clip_y0, d->clip_y1);
  double ox0 = fmin(x0, x1), ox1 = fmax(x0, x1);
  double oy0 = fmin(y0, y1), oy1 = fmax(y0, y1);
  double rx0 = fmax(ox0, cx0), rx1 = fmin(ox1, cx1);
  double ry0 = fmax(oy0, cy0), ry1 = fmin(oy1, cy1);
  if (rx1 <= rx0 || ry1 <= ry0) return;

  int cut = (rx0 != ox0 || rx1 != ox1 || ry0 != oy0 || ry1 != oy1);
  int has_border = !(gc->col == NA_INTEGER || R_TRANSPARENT(gc->col) ||
                     gc->lty == LTY_BLANK);

  sp_open(d, "");
  mb_printf(&d->out, "<xdr:spPr>");
  xfrm(d, rx0, ry0, rx1, ry1);
  mb_printf(&d->out, "<a:prstGeom prst=\"rect\"><a:avLst/></a:prstGeom>");
  fill_props_gc(d, gc, rx0, ry0, rx1, ry1);
  if (cut && has_border) {
    /* clipping cut the rect: don't stroke the edges the clip introduced;
     draw the surviving pieces of the original outline separately */
    mb_printf(&d->out, "<a:ln><a:noFill/></a:ln>");
  } else {
    line_props(d, gc->col, gc->lwd, gc->lty, gc->lend, gc->ljoin, gc->lmitre);
  }
  mb_printf(&d->out, "</xdr:spPr><xdr:txBody><a:bodyPr/><a:lstStyle/><a:p/></xdr:txBody></xdr:sp>\n");

  if (cut && has_border) {
    double bx[5] = {ox0, ox1, ox1, ox0, ox0};
    double by[5] = {oy0, oy0, oy1, oy1, oy0};
    emit_clipped_polyline(d, bx, by, 5, gc);
  }
}

static void Xdr_Circle(double x, double y, double r, const pGEcontext gc, pDevDesc dd) {
  xdrDesc *d = (xdrDesc *) dd->deviceSpecific;
  if (d->capturing) {
    if (capture_visible(d, gc)) {
      double px[64], py[64];
      for (int i = 0; i < 64; i++) {
        double t = 2.0 * M_PI * (double) i / 64.0;
        px[i] = x + r * cos(t);
        py[i] = y + r * sin(t);
      }
      capture_ring(d, px, py, 64);
    }
    return;
  }
  if (fully_outside_clip(d, x - r, y - r, x + r, y + r)) return;
  double cx0 = fmin(d->clip_x0, d->clip_x1), cx1 = fmax(d->clip_x0, d->clip_x1);
  double cy0 = fmin(d->clip_y0, d->clip_y1), cy1 = fmax(d->clip_y0, d->clip_y1);

  if (!d->clip_shaped && !d->mask_shaped &&
      x - r >= cx0 && x + r <= cx1 && y - r >= cy0 && y + r <= cy1) {
    sp_open(d, "");
    mb_printf(&d->out, "<xdr:spPr>");
    xfrm(d, x - r, y - r, x + r, y + r);
    mb_printf(&d->out, "<a:prstGeom prst=\"ellipse\"><a:avLst/></a:prstGeom>");
    fill_props_gc(d, gc, x - r, y - r, x + r, y + r);
    line_props(d, gc->col, gc->lwd, gc->lty, gc->lend, gc->ljoin, gc->lmitre);
    mb_printf(&d->out, "</xdr:spPr><xdr:txBody><a:bodyPr/><a:lstStyle/><a:p/></xdr:txBody></xdr:sp>\n");
    return;
  }

  /* Partially clipped: a native ellipse can't be cut in DrawingML, so
   approximate with a polygon and run it through the polygon clipper. */
  #define CIRCLE_SEG 64
  double px[CIRCLE_SEG], py[CIRCLE_SEG];
  for (int i = 0; i < CIRCLE_SEG; i++) {
    double t = 2.0 * M_PI * (double) i / (double) CIRCLE_SEG;
    px[i] = x + r * cos(t);
    py[i] = y + r * sin(t);
  }
  emit_clipped_ring(d, px, py, CIRCLE_SEG, gc);
  #undef CIRCLE_SEG
}

static void Xdr_Polyline(int n, double *x, double *y, const pGEcontext gc, pDevDesc dd) {
  xdrDesc *d = (xdrDesc *) dd->deviceSpecific;
  if (d->capturing) return;
  emit_clipped_polyline(d, x, y, n, gc);
}

static void Xdr_Polygon(int n, double *x, double *y, const pGEcontext gc, pDevDesc dd) {
  xdrDesc *d = (xdrDesc *) dd->deviceSpecific;
  if (d->capturing) {
    if (capture_visible(d, gc)) capture_ring(d, x, y, n);
    return;
  }
  emit_clipped_ring(d, x, y, n, gc);
}

static double ring_signed_area(const double *x, const double *y, int n) {
  double a = 0.0;
  for (int i = 0; i < n; i++) {
    int j = (i + 1) % n;
    a += x[i] * y[j] - x[j] * y[i];
  }
  return 0.5 * a;
}

static int point_in_ring(double px, double py, const double *x, const double *y, int n) {
  int inside = 0;
  for (int i = 0, j = n - 1; i < n; j = i++) {
    if (((y[i] > py) != (y[j] > py)) &&
        (px < (x[j] - x[i]) * (py - y[i]) / (y[j] - y[i]) + x[i]))
      inside = !inside;
  }
  return inside;
}

static void Xdr_Path(double *x, double *y, int npoly, int *nper,
                     Rboolean winding, const pGEcontext gc, pDevDesc dd) {
  xdrDesc *d = (xdrDesc *) dd->deviceSpecific;
  if (npoly < 1) return;
  if (d->capturing) {
    if (capture_visible(d, gc)) {
      int idx = 0;
      for (int k = 0; k < npoly; k++) {
        capture_ring(d, x + idx, y + idx, nper[k]);
        idx += nper[k];
      }
    }
    return;
  }

  int total = 0;
  for (int k = 0; k < npoly; k++) total += nper[k];
  if (total < 2) return;

  /* Renderers fill custGeom with the nonzero rule and there is no evenodd
   switch. For R's evenodd we emulate the common nested-ring case by
   normalising each ring's orientation to its nesting parity (even depth
   anticlockwise, odd clockwise), which makes nonzero cut the holes.
   Self-intersecting evenodd exotica beyond ring nesting stay nonzero. */
  if (!winding && npoly > 1) {
    int idx_k = 0;
    for (int k = 0; k < npoly; k++) {
      int depth = 0, idx_m = 0;
      for (int m = 0; m < npoly; m++) {
        if (m != k &&
            point_in_ring(x[idx_k], y[idx_k], x + idx_m, y + idx_m, nper[m]))
          depth++;
        idx_m += nper[m];
      }
      double area = ring_signed_area(x + idx_k, y + idx_k, nper[k]);
      int want_ccw = (depth % 2 == 0);       /* device y is down: ccw = area < 0 */
      int is_ccw = (area < 0.0);
      if (want_ccw != is_ccw) {
        for (int i = 0, j = nper[k] - 1; i < j; i++, j--) {
          double t;
          t = x[idx_k + i]; x[idx_k + i] = x[idx_k + j]; x[idx_k + j] = t;
          t = y[idx_k + i]; y[idx_k + i] = y[idx_k + j]; y[idx_k + j] = t;
        }
      }
      idx_k += nper[k];
    }
  }

  /* Clip each ring and collect surviving vertices to compute the global bbox. */
  int max_ring = 0;
  for (int k = 0; k < npoly; k++) if (nper[k] > max_ring) max_ring = nper[k];
  int cap = 2 * (max_ring + 4 + (d->clip_shaped ? d->ring_n : 0)) + 8;

  /* clipped ring storage: at most cap vertices per ring, npoly rings */
  double *crx = (double *) R_alloc((size_t)(cap * npoly), sizeof(double));
  double *cry = (double *) R_alloc((size_t)(cap * npoly), sizeof(double));
  int    *crn = (int *)    R_alloc((size_t) npoly, sizeof(int));

  int idx = 0, any = 0, cut = 0;
  double gx0 = R_PosInf, gy0 = R_PosInf, gx1 = R_NegInf, gy1 = R_NegInf;
  for (int k = 0; k < npoly; k++) {
    double *ppx, *ppy;
    int *ppn;
    int npc = dev_clip_polygon_multi(d, x + idx, y + idx, nper[k],
                                     &ppx, &ppy, &ppn);
    /* keep only the first piece per input ring in the fixed crx storage;
     extra pieces are emitted as separate fill-only polygons below */
    double *ox = ppx, *oy = ppy;
    int m = (npc >= 1) ? ppn[0] : 0;
    if (npc > 1) {
      int off2 = ppn[0];
      for (int p = 1; p < npc; p++) {
        emit_polygon_shape(d, ppx + off2, ppy + off2, ppn[p], gc, 0);
        off2 += ppn[p];
      }
    }
    crn[k] = m;
    if (m != nper[k]) cut = 1;
    if (m >= 2) {
      for (int i = 0; i < m; i++) {
        if (!cut && (ox[i] != x[idx + i] || oy[i] != y[idx + i])) cut = 1;
        crx[k * cap + i] = ox[i];
        cry[k * cap + i] = oy[i];
        if (ox[i] < gx0) gx0 = ox[i];
        if (ox[i] > gx1) gx1 = ox[i];
        if (oy[i] < gy0) gy0 = oy[i];
        if (oy[i] > gy1) gy1 = oy[i];
      }
      any = 1;
    }
    idx += nper[k];
  }
  if (!any) return;

  int has_border = !(gc->col == NA_INTEGER || R_TRANSPARENT(gc->col) ||
                     gc->lty == LTY_BLANK);

  double x_min = gx0 * PT_TO_EMU;
  double y_min = gy0 * PT_TO_EMU;
  double w_emu = (gx1 - gx0) * PT_TO_EMU;
  double h_emu = (gy1 - gy0) * PT_TO_EMU;
  double w_or1 = w_emu <= 0 ? 1.0 : w_emu;
  double h_or1 = h_emu <= 0 ? 1.0 : h_emu;

  sp_open(d, "");
  mb_printf(&d->out, "<xdr:spPr>");
  xfrm(d, gx0, gy0, gx1, gy1);
  mb_printf(&d->out,
          "<a:custGeom><a:avLst/><a:gdLst/><a:ahLst/><a:cxnLst/>"
          "<a:rect l=\"0\" t=\"0\" r=\"%.0f\" b=\"%.0f\"/><a:pathLst>",
            w_or1, h_or1);

  /* All rings inside ONE <a:path>: renderers fill each path element
   independently, so per-ring paths would paint holes solid on top. */
  mb_printf(&d->out, "<a:path w=\"%.0f\" h=\"%.0f\">", w_or1, h_or1);
  for (int k = 0; k < npoly; k++) {
    int m = crn[k];
    if (m < 2) continue;
    for (int i = 0; i < m; i++) {
      double px = crx[k * cap + i] * PT_TO_EMU - x_min;
      double py = cry[k * cap + i] * PT_TO_EMU - y_min;
      mb_printf(&d->out, "<a:%s><a:pt x=\"%.0f\" y=\"%.0f\"/></a:%s>",
              i == 0 ? "moveTo" : "lnTo", px, py,
              i == 0 ? "moveTo" : "lnTo");
    }
    mb_printf(&d->out, "<a:close/>");
  }
  mb_printf(&d->out, "</a:path></a:pathLst></a:custGeom>");
  fill_props_gc(d, gc, gx0, gy0, gx1, gy1);
  if (has_border && !cut)
    line_props(d, gc->col, gc->lwd, gc->lty, gc->lend, gc->ljoin, gc->lmitre);
  else
    mb_printf(&d->out, "<a:ln><a:noFill/></a:ln>");
  mb_printf(&d->out, "</xdr:spPr><xdr:txBody><a:bodyPr/><a:lstStyle/><a:p/></xdr:txBody></xdr:sp>\n");

  if (has_border && cut) {
    double *rx = (double *) R_alloc((size_t) max_ring + 1, sizeof(double));
    double *ry = (double *) R_alloc((size_t) max_ring + 1, sizeof(double));
    idx = 0;
    for (int k = 0; k < npoly; k++) {
      int nk = nper[k];
      for (int i = 0; i < nk; i++) { rx[i] = x[idx + i]; ry[i] = y[idx + i]; }
      rx[nk] = x[idx]; ry[nk] = y[idx];
      emit_clipped_polyline(d, rx, ry, nk + 1, gc);
      idx += nk;
    }
  }
}

static void Xdr_TextImpl(double x, double y, const char *str, double rot,
                         double hadj, const pGEcontext gc, pDevDesc dd) {
  xdrDesc *d = (xdrDesc *) dd->deviceSpecific;
  if (d->capturing) return;
  if (str == NULL || str[0] == '\0') return;
  size_t buflen = strlen(str) * 6 + 1;
  char *buf = R_alloc(buflen, 1);
  esc_xml(str, buf, buflen);
  double fs = gc->cex * gc->ps;
  double w = Xdr_StrWidth(str, gc, dd);
  double h = fs * 1.2;
  /* Text is centred in its box (anchor="ctr"), so the box centre must sit
   at the visual centre of the line: baseline - (ascent - descent)/2.
   With the device metrics (0.75/0.25 em) that is 0.25em above the
   baseline. Centre-anchoring is robust across renderers - Excel and
   LibreOffice disagree on how much descent/line-gap hangs below a
   bottom-anchored line, which made anchor="b" text sit visibly high in
   Excel, but a centred line splits those conventions symmetrically. */
  double base_off = d->text_voff * fs; /* baseline below box centre */

  double bx0, by0, bx1, by1;
  if (fabs(rot) > 1e-4) {
    /* Rotate around the box's own center (matching how the bodyPr rot
     attribute rotates a shape), solving for the center position such
     that the anchor point (x,y) lands correctly post-rotation. */
    double theta = -rot * M_PI / 180.0;
    double cos_r = cos(theta);
    double sin_r = sin(theta);
    double ox = hadj * w - w / 2.0;   /* local anchor x, relative to box center */
    double oy = base_off;             /* local anchor y (baseline), relative to center */
    double rot_ox = ox * cos_r - oy * sin_r;
    double rot_oy = ox * sin_r + oy * cos_r;
    double cx = x - rot_ox;
    double cy = y - rot_oy;
    bx0 = cx - w / 2.0; by0 = cy - h / 2.0; bx1 = bx0 + w; by1 = by0 + h;
  } else {
    double base_x = x - hadj * w;
    double cy = y - base_off;
    bx0 = base_x; by0 = cy - h / 2.0; bx1 = base_x + w; by1 = cy + h / 2.0;
    /* Shrink the box horizontally where algn pins the ink to an edge
     anyway (wrap is off, so the text still renders in full). Boxes
     hanging outside the canvas otherwise stick out of the group's frame
     in Excel. Vertical shrinking would move centre-anchored ink, so the
     box may overhang the canvas top by up to 0.1em - acceptable. */
    if (hadj < 0.25) {          /* algn "l": right edge is free */
      if (bx1 > dd->right && bx0 < dd->right) bx1 = dd->right;
    } else if (hadj > 0.75) {   /* algn "r": left edge is free */
      if (bx0 < 0.0 && bx1 > 0.0) bx0 = 0.0;
    }
  }
  if (fully_outside_clip(d, bx0, by0, bx1, by1)) return;

  sp_open(d, "");
  mb_printf(&d->out, "<xdr:spPr>");

  xfrm(d, bx0, by0, bx1, by1);

  mb_printf(&d->out, "<a:prstGeom prst=\"rect\"><a:avLst/></a:prstGeom><a:noFill/>"
            "<a:ln><a:noFill/></a:ln></xdr:spPr>");

  if (fabs(rot) > 1e-4) {
    int ooxml_rot = (int) (-rot * 60000.0);
    mb_printf(&d->out, "<xdr:txBody><a:bodyPr rot=\"%d\" vert=\"horz\" anchor=\"ctr\" wrap=\"none\" lIns=\"0\" tIns=\"0\" rIns=\"0\" bIns=\"0\"/><a:lstStyle/>", ooxml_rot);
  } else {
    mb_printf(&d->out, "<xdr:txBody><a:bodyPr anchor=\"ctr\" wrap=\"none\" lIns=\"0\" tIns=\"0\" rIns=\"0\" bIns=\"0\"/><a:lstStyle/>");
  }

  const char *b_attr = (gc->fontface == 2 || gc->fontface == 4) ? " b=\"1\"" : "";
  const char *i_attr = (gc->fontface == 3 || gc->fontface == 4) ? " i=\"1\"" : "";
  const char *u_attr = d->underline ? " u=\"sng\"" : "";
  const char *strike_attr = d->strikeout ? " strike=\"sngStrike\"" : "";

  const char *algn = (hadj < 0.25) ? "l" : (hadj > 0.75) ? "r" : "ctr";
  const char *font = is_generic_family(gc->fontfamily) ? d->fontname : gc->fontfamily;
  char fbuf[1301];
  esc_xml(font, fbuf, sizeof(fbuf));

  int sz = (int) lround(fs * 100.0);
  if (sz < 100) sz = 100;

  mb_printf(&d->out,
          "<a:p><a:pPr algn=\"%s\"/><a:r><a:rPr sz=\"%d\"%s%s%s%s><a:solidFill>",
          algn, sz, b_attr, i_attr, u_attr, strike_attr);
  srgb_clr(&d->out, gc->col);
  mb_printf(&d->out,
          "</a:solidFill><a:latin typeface=\"%s\"/><a:cs typeface=\"%s\"/></a:rPr>"
          "<a:t>%s</a:t></a:r></a:p></xdr:txBody></xdr:sp>\n",
          fbuf, fbuf, buf);
}

static void Xdr_Text(double x, double y, const char *str, double rot,
                     double hadj, const pGEcontext gc, pDevDesc dd) {
  Xdr_TextImpl(x, y, str, rot, hadj, gc, dd);
}

#if R_GE_version >= 13
static SEXP Xdr_SetPattern(SEXP pattern, pDevDesc dd) {
  (void) dd;
  return pattern;
}

static void Xdr_ReleasePattern(SEXP ref, pDevDesc dd) {
  (void) ref; (void) dd;
}

/* Run the capture (replaying `fn` through the draw callbacks) and, on
 success, leave a normalised convex ring in ring_x/ring_y with its bbox in
 cap_b*. Returns 1 on success, 0 when nothing usable was captured. */
static int capture_region(xdrDesc *d, SEXP fn, int kind, int luminance) {
  d->capturing = 1;
  d->cap_kind = kind;
  d->cap_luminance = luminance;
  d->ring_n = 0;
  d->cap_fail = 0;
  d->cap_any = 0;
  d->mask_soft = 0;
  d->mask_hidden_ink = 0;
  SEXP call = PROTECT(Rf_lang1(fn));
  int err = 0;
  R_tryEvalSilent(call, R_GlobalEnv, &err);
  UNPROTECT(1);
  d->capturing = 0;
  if (err || !d->cap_any) return 0;

  int ok = (!d->cap_fail && d->ring_n >= 3);
  if (!ok) {
    Rf_warning(kind == 0
      ? "easeling: multi-ring or oversized clip path; clipping to its bounding box"
      : "easeling: multi-ring or oversized mask; masking to its bounding box");
    d->ring_x[0] = d->cap_bx0; d->ring_y[0] = d->cap_by0;
    d->ring_x[1] = d->cap_bx1; d->ring_y[1] = d->cap_by0;
    d->ring_x[2] = d->cap_bx1; d->ring_y[2] = d->cap_by1;
    d->ring_x[3] = d->cap_bx0; d->ring_y[3] = d->cap_by1;
    d->ring_n = 4;
  }
  if (ring_signed_area(d->ring_x, d->ring_y, d->ring_n) > 0.0) {
    for (int i = 0, j = d->ring_n - 1; i < j; i++, j--) {
      double t;
      t = d->ring_x[i]; d->ring_x[i] = d->ring_x[j]; d->ring_x[j] = t;
      t = d->ring_y[i]; d->ring_y[i] = d->ring_y[j]; d->ring_y[j] = t;
    }
  }
  bbox(d->ring_x, d->ring_y, d->ring_n,
       &d->cap_bx0, &d->cap_by0, &d->cap_bx1, &d->cap_by1);
  d->ring_convex = ring_is_convex(d->ring_x, d->ring_y, d->ring_n);
  return 1;
}

static SEXP Xdr_SetClipPath(SEXP path, SEXP ref, pDevDesc dd) {
  (void) ref;
  xdrDesc *d = (xdrDesc *) dd->deviceSpecific;
  d->clip_shaped = 0;
  if (path == R_NilValue || !Rf_isFunction(path)) return R_NilValue;
  /* Replay the clip grob through our own draw callbacks in capture mode
   to obtain its outline (the cairo devices use the same mechanism). */
  if (capture_region(d, path, 0, 0)) d->clip_shaped = 1;
  return R_NilValue;
}

static void Xdr_ReleaseClipPath(SEXP ref, pDevDesc dd) {
  (void) ref; (void) dd;
}

static SEXP Xdr_SetMask(SEXP path, SEXP ref, pDevDesc dd) {
  (void) ref;
  xdrDesc *d = (xdrDesc *) dd->deviceSpecific;
  d->mask_shaped = 0;
  if (path == R_NilValue || !Rf_isFunction(path)) return R_NilValue;
  int luminance = 0;
#ifdef R_GE_luminanceMask
  luminance = (R_GE_maskType(path) == R_GE_luminanceMask);
#endif
  /* preserve any active clip ring: capture reuses the ring buffers */
  double sx[CLIP_RING_MAX], sy[CLIP_RING_MAX];
  double sb0 = d->cap_bx0, sb1 = d->cap_by0, sb2 = d->cap_bx1, sb3 = d->cap_by1;
  int sn = d->clip_shaped ? d->ring_n : 0;
  for (int i = 0; i < sn; i++) { sx[i] = d->ring_x[i]; sy[i] = d->ring_y[i]; }

  int got = capture_region(d, path, 1, luminance);
  if (got) {
    d->mask_n = d->ring_n;
    for (int i = 0; i < d->ring_n; i++) {
      d->mask_x[i] = d->ring_x[i];
      d->mask_y[i] = d->ring_y[i];
    }
    d->mask_bx0 = d->cap_bx0;
    d->mask_by0 = d->cap_by0;
    d->mask_bx1 = d->cap_bx1;
    d->mask_by1 = d->cap_by1;
    d->mask_convex = d->ring_convex;
    d->mask_shaped = 1;
  } else if (d->mask_soft) {
    Rf_warning("easeling: soft (semi-transparent or gradient) masks cannot be represented; mask ignored");
  } else if (d->mask_hidden_ink) {
    Rf_warning("easeling: inverse (hide-region) luminance masks cannot be represented; mask ignored");
  }

  if (sn) {
    for (int i = 0; i < sn; i++) { d->ring_x[i] = sx[i]; d->ring_y[i] = sy[i]; }
    d->ring_n = sn;
    d->cap_bx0 = sb0; d->cap_by0 = sb1; d->cap_bx1 = sb2; d->cap_by1 = sb3;
  }
  return R_NilValue;
}

static void Xdr_Text(double x, double y, const char *str, double rot,
                     double hadj, const pGEcontext gc, pDevDesc dd);

#if R_GE_version >= 16
static SEXP Xdr_Capabilities(SEXP cap) {
  /* value coding: 1 = no, 2 = yes; patterns and masks take vectors of the
   supported type constants. Hard masks and arbitrary simple clip regions
   are implemented; tiling patterns, compositing, groups, and paths take
   engine fallbacks */
  SET_VECTOR_ELT(cap, R_GE_capability_semiTransparency, Rf_ScalarInteger(2));
  SET_VECTOR_ELT(cap, R_GE_capability_transparentBackground, Rf_ScalarInteger(2));
  SET_VECTOR_ELT(cap, R_GE_capability_rasterImage, Rf_ScalarInteger(2));
  {
    SEXP pat = PROTECT(Rf_allocVector(INTSXP, 2));
    INTEGER(pat)[0] = R_GE_linearGradientPattern;
    INTEGER(pat)[1] = R_GE_radialGradientPattern;
    SET_VECTOR_ELT(cap, R_GE_capability_patterns, pat);
    UNPROTECT(1);
  }
  SET_VECTOR_ELT(cap, R_GE_capability_clippingPaths, Rf_ScalarInteger(2));
  {
    SEXP mk = PROTECT(Rf_allocVector(INTSXP, 2));
    INTEGER(mk)[0] = R_GE_alphaMask;
    INTEGER(mk)[1] = R_GE_luminanceMask;
    SET_VECTOR_ELT(cap, R_GE_capability_masks, mk);
    UNPROTECT(1);
  }
  SET_VECTOR_ELT(cap, R_GE_capability_compositing, Rf_ScalarInteger(1));
  SET_VECTOR_ELT(cap, R_GE_capability_transformations, Rf_ScalarInteger(1));
  SET_VECTOR_ELT(cap, R_GE_capability_paths, Rf_ScalarInteger(1));
  SET_VECTOR_ELT(cap, R_GE_capability_glyphs, Rf_ScalarInteger(2));
  return cap;
}

/* groups/paths declare capability "no", so the engine renders their
 fallbacks and never reaches these */
/* # nocov start */
static SEXP Xdr_DefineGroup(SEXP source, int op, SEXP destination, pDevDesc dd) {
  (void) source; (void) op; (void) destination; (void) dd;   /* # nocov */
  return R_NilValue;                                         /* # nocov */
}
static void Xdr_UseGroup(SEXP ref, SEXP trans, pDevDesc dd) {
  (void) ref; (void) trans; (void) dd;                       /* # nocov */
}
static void Xdr_ReleaseGroup(SEXP ref, pDevDesc dd) {
  (void) ref; (void) dd;
}
static void Xdr_Stroke(SEXP path, const pGEcontext gc, pDevDesc dd) {
  (void) path; (void) gc; (void) dd;                         /* # nocov */
}
static void Xdr_Fill(SEXP path, int rule, const pGEcontext gc, pDevDesc dd) {
  (void) path; (void) rule; (void) gc; (void) dd;            /* # nocov */
}
static void Xdr_FillStroke(SEXP path, int rule, const pGEcontext gc, pDevDesc dd) {
  (void) path; (void) rule; (void) gc; (void) dd;
}
/* # nocov end */

static void Xdr_Glyph(int n, int *glyphs, double *x, double *y,
                      SEXP font, double size, int colour, double rot,
                      pDevDesc dd) {
  xdrDesc *d = (xdrDesc *) dd->deviceSpecific;
  if (d->capturing || n < 1) return;
  if (d->glyph_fun == NULL) {
    if (!d->glyph_warned_nosf) {
      Rf_warning("easeling: glyph-based text (e.g. marquee) requires the "
                 "systemfonts package to map glyphs to characters; "
                 "glyphs dropped");
      d->glyph_warned_nosf = 1;
    }
    return;
  }
  SEXP ids = PROTECT(Rf_allocVector(INTSXP, n));
  memcpy(INTEGER(ids), glyphs, (size_t) n * sizeof(int));
  SEXP call = PROTECT(Rf_lang4(d->glyph_fun,
                               Rf_mkString(R_GE_glyphFontFile(font)),
                               Rf_ScalarInteger(R_GE_glyphFontIndex(font)),
                               ids));
  int err = 0;
  SEXP chars = R_tryEvalSilent(call, R_GlobalEnv, &err);
  if (err || chars == NULL || TYPEOF(chars) != STRSXP ||
      Rf_xlength(chars) != n) {
    UNPROTECT(2);
    return;
  }
  PROTECT(chars);

  R_GE_gcontext gc;
  memset(&gc, 0, sizeof(gc));
  gc.cex = 1.0;
  gc.ps = size;
  gc.col = colour;
  double weight = R_GE_glyphFontWeight(font);
  int italic = (R_GE_glyphFontStyle(font) != R_GE_text_style_normal);
  gc.fontface = (weight >= 700.0) ? (italic ? 4 : 2) : (italic ? 3 : 1);
  strncpy(gc.fontfamily, R_GE_glyphFontFamily(font),
          sizeof(gc.fontfamily) - 1);

  int unmapped = 0;
  for (int i = 0; i < n; i++) {
    const char *ch = Rf_translateCharUTF8(STRING_ELT(chars, i));
    if (ch[0] == '\0') { unmapped++; continue; }
    Xdr_Text(x[i], y[i], ch, rot, 0.0, &gc, dd);
  }
  if (unmapped && !d->glyph_warned_unmapped) {
    Rf_warning("easeling: %d glyph(s) had no character mapping in the font "
               "cmap and were dropped", unmapped);
    d->glyph_warned_unmapped = 1;
  }
  UNPROTECT(3);
}
#endif

static void Xdr_ReleaseMask(SEXP ref, pDevDesc dd) {
  (void) ref; (void) dd;
}
#endif

SEXP easeling_(SEXP path_, SEXP width_, SEXP height_, SEXP pointsize_,
               SEXP fontname_, SEXP underline_, SEXP strikeout_,
               SEXP text_voff_, SEXP result_env_, SEXP metrics_,
               SEXP glyph_fun_) {
  const char *path = (path_ == R_NilValue) ? NULL : CHAR(STRING_ELT(path_, 0));
  if (path == NULL && TYPEOF(result_env_) != ENVSXP)
    Rf_error("internal: memory output needs an environment");
  double width  = REAL(width_)[0];
  double height = REAL(height_)[0];
  double ps     = REAL(pointsize_)[0];
  const char *fontname = CHAR(STRING_ELT(fontname_, 0));
  Rboolean underline = (Rboolean) LOGICAL(underline_)[0];
  Rboolean strikeout = (Rboolean) LOGICAL(strikeout_)[0];

  R_GE_checkVersionOrDie(R_GE_version);
  R_CheckDeviceAvailable();

  pDevDesc dd = (pDevDesc) calloc(1, sizeof(DevDesc));
  if (dd == NULL) Rf_error("could not allocate device");

  xdrDesc *xd = (xdrDesc *) calloc(1, sizeof(xdrDesc));
  if (xd == NULL) { free(dd); Rf_error("could not allocate device"); }

  if (path != NULL) {
    /* fail early on an unwritable path instead of at close */
    FILE *fp = fopen(path, "w");
    if (fp == NULL) { free(xd); free(dd); Rf_error("cannot open '%s'", path); }
    fclose(fp);
    xd->path = strdup(path);
    if (xd->path == NULL) {              /* # nocov start */
      free(xd); free(dd);
      Rf_error("could not allocate device");
    }                                    /* # nocov end */
  } else {
    xd->result_env = result_env_;
    R_PreserveObject(xd->result_env);
  }
  mb_reserve(&xd->out, 0);
  xd->shape_id = 2; /* id 1 is the group */
  xd->page = 0;
  xd->clip_x0 = 0; xd->clip_y0 = 0;
  xd->clip_x1 = width * 72.0; xd->clip_y1 = height * 72.0;
  strncpy(xd->fontname, fontname, sizeof(xd->fontname) - 1);
  xd->text_voff = REAL(text_voff_)[0];
  if (Rf_isFunction(glyph_fun_)) {
    xd->glyph_fun = glyph_fun_;
    R_PreserveObject(xd->glyph_fun);
  }
  if (metrics_ != R_NilValue) {
    if (TYPEOF(metrics_) != REALSXP || Rf_xlength(metrics_) != 285)
      Rf_error("internal: metrics must be a numeric vector of length 285");
    const double *m = REAL(metrics_);
    memcpy(xd->cw,  m,       95 * sizeof(double));
    memcpy(xd->ca,  m +  95, 95 * sizeof(double));
    memcpy(xd->cd2, m + 190, 95 * sizeof(double));
    xd->have_metrics = 1;
  }
  xd->fontname[sizeof(xd->fontname) - 1] = '\0';
  xd->underline = underline;
  xd->strikeout = strikeout;

  double dev_w = width * 72.0;
  double dev_h = height * 72.0;
  double emu_w = dev_w * PT_TO_EMU;
  double emu_h = dev_h * PT_TO_EMU;

  mb_printf(&xd->out,
          "<xdr:wsDr xmlns:xdr=\"http://schemas.openxmlformats.org/drawingml/2006/spreadsheetDrawing\" "
            "xmlns:a=\"http://schemas.openxmlformats.org/drawingml/2006/main\">"
            "<xdr:absoluteAnchor>"
            "<xdr:pos x=\"0\" y=\"0\"/>"
            "<xdr:ext cx=\"%.0f\" cy=\"%.0f\"/>"
            "<xdr:grpSp><xdr:nvGrpSpPr><xdr:cNvPr id=\"1\" name=\"R_Graphics_Group\"/><xdr:cNvGrpSpPr/></xdr:nvGrpSpPr>"
            "<xdr:grpSpPr><a:xfrm><a:off x=\"0\" y=\"0\"/><a:ext cx=\"%.0f\" cy=\"%.0f\"/>"
            "<a:chOff x=\"0\" y=\"0\"/><a:chExt cx=\"%.0f\" cy=\"%.0f\"/></a:xfrm></xdr:grpSpPr>",
              emu_w, emu_h,
              emu_w, emu_h,
              emu_w, emu_h);

  dd->left = 0; dd->right = dev_w;
  dd->top = 0; dd->bottom = dev_h;
  dd->clipLeft = dd->left; dd->clipRight = dd->right;
  dd->clipTop = dd->top; dd->clipBottom = dd->bottom;

  dd->xCharOffset = 0.4900;
  dd->yCharOffset = 0.3333;
  dd->yLineBias = 0.2;
  dd->ipr[0] = dd->ipr[1] = 1.0 / 72.0;
  dd->cra[0] = 0.9 * ps;
  dd->cra[1] = 1.2 * ps;
  dd->gamma = 1;
  dd->canClip = TRUE;
  /* 2 = device handles hadj itself. Excel lays the text out with the real
   font, so aligning via the text-box (a:pPr algn + wrap=none) is robust
   to our approximate string widths; letting R pre-shift x (canHAdj=0)
   would bake the metric error into the position instead. */
  dd->canHAdj = 2;
  dd->canChangeGamma = FALSE;
  dd->startps = ps;
  dd->startcol = (int) R_RGB(0, 0, 0);
  dd->startfill = (int) R_RGBA(255, 255, 255, 0);
  dd->startlty = 0;
  dd->startfont = 1;
  dd->startgamma = 1;

  dd->deviceSpecific = xd;
  dd->displayListOn = FALSE;

  dd->activate = Xdr_Activate;
  dd->circle = Xdr_Circle;
  dd->clip = Xdr_Clip;
  dd->close = Xdr_Close;
  dd->deactivate = Xdr_Deactivate;
  dd->locator = NULL;
  dd->line = Xdr_Line;
  dd->metricInfo = Xdr_MetricInfo;
  dd->mode = Xdr_Mode;
  dd->newPage = Xdr_NewPage;
  dd->polygon = Xdr_Polygon;
  dd->polyline = Xdr_Polyline;
  dd->rect = Xdr_Rect;
  dd->path = Xdr_Path;
  dd->raster = Xdr_Raster;
  dd->cap = NULL;
  dd->size = Xdr_Size;
  dd->strWidth = Xdr_StrWidth;
  dd->text = Xdr_Text;
  dd->onExit = NULL;
  dd->getEvent = NULL;
  dd->newFrameConfirm = NULL;

  dd->hasTextUTF8 = TRUE;
  dd->textUTF8 = Xdr_Text;
  dd->strWidthUTF8 = Xdr_StrWidth;
  dd->wantSymbolUTF8 = TRUE;
  dd->useRotatedTextInContour = FALSE;

  dd->eventEnv = R_NilValue;
  dd->eventHelper = NULL;
  dd->holdflush = NULL;

  dd->haveTransparency = 2;
  dd->haveTransparentBg = 2;
  dd->haveRaster = 2; /* yes - Xdr_Raster is implemented (RLE rects) */
  dd->haveCapture = 1; /* no - dd->cap is NULL; R checks before calling, so safe */
  dd->haveLocator = 1; /* no - dd->locator is NULL; R checks before calling, so safe */

#if R_GE_version >= 16
  dd->deviceVersion = R_GE_glyphs;
#elif R_GE_version >= 13
  /* deviceVersion and deviceClip only exist from R 4.1 (GE 13); older
   engines assume version-12 behaviour and engine-side clipping checks */
  dd->deviceVersion = R_GE_definitions;
#endif
#if R_GE_version >= 13
  dd->deviceClip = FALSE;
#endif

#if R_GE_version >= 13
  dd->setPattern = Xdr_SetPattern;
  dd->releasePattern = Xdr_ReleasePattern;
  dd->setClipPath = Xdr_SetClipPath;
  dd->releaseClipPath = Xdr_ReleaseClipPath;
  dd->setMask = Xdr_SetMask;
  dd->releaseMask = Xdr_ReleaseMask;
#endif
#if R_GE_version >= 16
  dd->capabilities = Xdr_Capabilities;
  dd->defineGroup = Xdr_DefineGroup;
  dd->useGroup = Xdr_UseGroup;
  dd->releaseGroup = Xdr_ReleaseGroup;
  dd->stroke = Xdr_Stroke;
  dd->fill = Xdr_Fill;
  dd->fillStroke = Xdr_FillStroke;
  dd->glyph = Xdr_Glyph;
#endif

  pGEDevDesc gdd = GEcreateDevDesc(dd);
  GEaddDevice2(gdd, "easeling");
  GEinitDisplayList(gdd);

  return R_NilValue;
}

static const R_CallMethodDef CallEntries[] = {
  {"easeling_", (DL_FUNC) &easeling_, 11},
  {NULL, NULL, 0}
};

void R_init_easeling(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
