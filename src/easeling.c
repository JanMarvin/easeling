#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <R_ext/GraphicsEngine.h>
#include <R_ext/GraphicsDevice.h>
#include <R_ext/Rdynload.h>

#include <stdio.h>
#include <string.h>
#include <math.h>

#define PT_TO_EMU 12700.0

typedef struct {
  FILE *out;
  int shape_id;
  double clip_x0, clip_y0, clip_x1, clip_y1;
  char fontname[201];
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
    default:   out[j++] = in[i];
    }
  }
  out[j] = '\0';
}

static unsigned int rgb_hex(int col) {
  return ((unsigned int) R_RED(col) << 16) |
    ((unsigned int) R_GREEN(col) << 8) |
    (unsigned int) R_BLUE(col);
}

static void sp_open(xdrDesc *d, const char *name) {
  fprintf(d->out,
          "<xdr:sp macro=\"\" textlink=\"\">"
            "<xdr:nvSpPr><xdr:cNvPr id=\"%d\" name=\"%s\"/><xdr:cNvSpPr/></xdr:nvSpPr>",
              d->shape_id++, name);
}

static void xfrm(xdrDesc *d, double x1, double y1, double x2, double y2) {
  double min_x = x1 < x2 ? x1 : x2;
  double min_y = y1 < y2 ? y1 : y2;
  double cx = fabs(x2 - x1) * PT_TO_EMU;
  double cy = fabs(y2 - y1) * PT_TO_EMU;

  fprintf(d->out, "<a:xfrm><a:off x=\"%.0f\" y=\"%.0f\"/><a:ext cx=\"%.0f\" cy=\"%.0f\"/></a:xfrm>",
          min_x * PT_TO_EMU, min_y * PT_TO_EMU, cx, cy);
}

static void fill_props(xdrDesc *d, int fill) {
  if (fill == NA_INTEGER || R_TRANSPARENT(fill)) {
    fprintf(d->out, "<a:noFill/>");
  } else {
    int alpha = (int)(R_ALPHA(fill) / 255.0 * 100000.0);
    fprintf(d->out, "<a:solidFill><a:srgbClr val=\"%06X\"><a:alpha val=\"%d\"/></a:srgbClr></a:solidFill>",
            rgb_hex(fill), alpha);
  }
}

static void emit_gradient_stops_linear(xdrDesc *d, SEXP pattern) {
  int n = R_GE_linearGradientNumStops(pattern);
  fprintf(d->out, "<a:gsLst>");
  for (int i = 0; i < n; i++) {
    double stop = R_GE_linearGradientStop(pattern, i);
    rcolor col = R_GE_linearGradientColour(pattern, i);
    int pos = (int) round(stop * 100000.0);
    int alpha = (int)(R_ALPHA((int) col) / 255.0 * 100000.0);
    fprintf(d->out, "<a:gs pos=\"%d\"><a:srgbClr val=\"%06X\"><a:alpha val=\"%d\"/></a:srgbClr></a:gs>",
            pos, rgb_hex((int) col), alpha);
  }
  fprintf(d->out, "</a:gsLst>");
}

static void emit_gradient_stops_radial(xdrDesc *d, SEXP pattern) {
  int n = R_GE_radialGradientNumStops(pattern);
  fprintf(d->out, "<a:gsLst>");
  for (int i = 0; i < n; i++) {
    double stop = R_GE_radialGradientStop(pattern, i);
    rcolor col = R_GE_radialGradientColour(pattern, i);
    int pos = (int) round(stop * 100000.0);
    int alpha = (int)(R_ALPHA((int) col) / 255.0 * 100000.0);
    fprintf(d->out, "<a:gs pos=\"%d\"><a:srgbClr val=\"%06X\"><a:alpha val=\"%d\"/></a:srgbClr></a:gs>",
            pos, rgb_hex((int) col), alpha);
  }
  fprintf(d->out, "</a:gsLst>");
}

static void fill_props_gc(xdrDesc *d, const pGEcontext gc) {
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
      fprintf(d->out, "<a:gradFill>");
      emit_gradient_stops_linear(d, gc->patternFill);
      fprintf(d->out, "<a:lin ang=\"%d\" scaled=\"1\"/></a:gradFill>", ang_60000);
      return;
    } else if (type == R_GE_radialGradientPattern) {
      fprintf(d->out, "<a:gradFill>");
      emit_gradient_stops_radial(d, gc->patternFill);
      fprintf(d->out,
              "<a:path path=\"circle\"><a:fillToRect l=\"50000\" t=\"50000\" "
              "r=\"50000\" b=\"50000\"/></a:path></a:gradFill>");
      return;
    }
    /* tiling patterns have no clean OOXML equivalent - fall through to noFill */
    fprintf(d->out, "<a:noFill/>");
    return;
  }
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
static void emit_dash(FILE *out, int lty, double lwd) {
  (void) lwd;
  if (lty == LTY_SOLID) return;
  if (lty == LTY_DASHED)   { fprintf(out, "<a:prstDash val=\"dash\"/>");      return; }
  if (lty == LTY_DOTTED)   { fprintf(out, "<a:prstDash val=\"dot\"/>");       return; }
  if (lty == LTY_DOTDASH)  { fprintf(out, "<a:prstDash val=\"dashDot\"/>");   return; }
  if (lty == LTY_LONGDASH) { fprintf(out, "<a:prstDash val=\"lgDash\"/>");    return; }
  if (lty == LTY_TWODASH)  { fprintf(out, "<a:prstDash val=\"lgDashDot\"/>"); return; }

  /* Custom: extract nibbles, emit custDash. OOXML <a:ds d= sp=> in units of 1/1000 lwd. */
  unsigned int u = (unsigned int) lty;
  int nib[8];
  int count = 0;
  for (int i = 0; i < 8; i++) {
    nib[i] = (int)((u >> (i * 4)) & 0xF);
    if (nib[i] != 0) count = i + 1;
  }
  if (count < 2) count = 2;
  if (count & 1) count++;
  fprintf(out, "<a:custDash>");
  for (int i = 0; i + 1 < count; i += 2) {
    int d_val  = nib[i]   > 0 ? nib[i]   * 1000 : 1000;
    int sp_val = nib[i+1] > 0 ? nib[i+1] * 1000 : 1000;
    fprintf(out, "<a:ds d=\"%d\" sp=\"%d\"/>", d_val, sp_val);
  }
  fprintf(out, "</a:custDash>");
}

static void line_props(xdrDesc *d, int col, double lwd, int lty) {
  if (col == NA_INTEGER || R_TRANSPARENT(col) || lty == LTY_BLANK) {
    fprintf(d->out, "<a:ln><a:noFill/></a:ln>");
    return;
  }
  double w_emu = (lwd > 0 ? lwd : 1.0) * 0.75 * PT_TO_EMU;
  int alpha = (int)(R_ALPHA(col) / 255.0 * 100000.0);
  fprintf(d->out,
          "<a:ln w=\"%.0f\"><a:solidFill><a:srgbClr val=\"%06X\"><a:alpha val=\"%d\"/></a:srgbClr></a:solidFill>",
          w_emu, rgb_hex(col), alpha);
  emit_dash(d->out, lty, lwd);
  fprintf(d->out, "</a:ln>");
}

static void points_to_pts(xdrDesc *d, double *x, double *y, int n,
                          double x0, double y0, double w_emu, double h_emu,
                          Rboolean closed) {
  fprintf(d->out,
          "<a:custGeom><a:avLst/><a:gdLst/><a:ahLst/><a:cxnLst/>"
          "<a:rect l=\"0\" t=\"0\" r=\"%.0f\" b=\"%.0f\"/><a:pathLst>"
          "<a:path w=\"%.0f\" h=\"%.0f\">",
          w_emu <= 0 ? 1.0 : w_emu, h_emu <= 0 ? 1.0 : h_emu,
                                           w_emu <= 0 ? 1.0 : w_emu, h_emu <= 0 ? 1.0 : h_emu);
  for (int i = 0; i < n; i++) {
    double px = (x[i] * PT_TO_EMU) - x0;
    double py = (y[i] * PT_TO_EMU) - y0;
    fprintf(d->out, "<a:%s><a:pt x=\"%.0f\" y=\"%.0f\"/></a:%s>",
            i == 0 ? "moveTo" : "lnTo", px, py,
            i == 0 ? "moveTo" : "lnTo");
  }
  if (closed) fprintf(d->out, "<a:close/>");
  fprintf(d->out, "</a:path></a:pathLst></a:custGeom>");
}

static Rboolean fully_outside_clip(xdrDesc *d, double x0, double y0, double x1, double y1) {
  double cx0 = fmin(d->clip_x0, d->clip_x1);
  double cx1 = fmax(d->clip_x0, d->clip_x1);
  double cy0 = fmin(d->clip_y0, d->clip_y1);
  double cy1 = fmax(d->clip_y0, d->clip_y1);
  double bx0 = fmin(x0, x1), bx1 = fmax(x0, x1);
  double by0 = fmin(y0, y1), by1 = fmax(y0, y1);
  return (bx1 < cx0 || bx0 > cx1 || by1 < cy0 || by0 > cy1) ? TRUE : FALSE;
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

static void Xdr_Raster(unsigned int *raster, int w, int h,
                       double x, double y, double width, double height,
                       double rot, Rboolean interpolate,
                       const pGEcontext gc, pDevDesc dd) {
  (void) interpolate; (void) gc; (void) rot;
  xdrDesc *d = (xdrDesc *) dd->deviceSpecific;
  if (w <= 0 || h <= 0) return;
  double left  = fmin(x, x + width);
  double right = fmax(x, x + width);
  double top   = fmin(y, y + height);
  double bot   = fmax(y, y + height);
  if (fully_outside_clip(d, left, top, right, bot)) return;
  double cell_w = (right - left) / (double) w;
  double cell_h = (bot - top) / (double) h;
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
        sp_open(d, "");
        fprintf(d->out, "<xdr:spPr>");
        xfrm(d, rx0, ry0, rx1, ry1);
        fprintf(d->out, "<a:prstGeom prst=\"rect\"><a:avLst/></a:prstGeom>");
        fill_props(d, (int) col);
        fprintf(d->out, "<a:ln><a:noFill/></a:ln>");
        fprintf(d->out, "</xdr:spPr><xdr:txBody><a:bodyPr/><a:p/></xdr:txBody></xdr:sp>\n");
      }
      i += run;
    }
  }
}

static void Xdr_Activate(const pDevDesc dd) { (void) dd; }
static void Xdr_Deactivate(pDevDesc dd) { (void) dd; }
static void Xdr_Mode(int mode, pDevDesc dd) { (void) mode; (void) dd; }

static void Xdr_NewPage(const pGEcontext gc, pDevDesc dd) { (void) gc; (void) dd; }

static void Xdr_Close(pDevDesc dd) {
  xdrDesc *d = (xdrDesc *) dd->deviceSpecific;

  fprintf(d->out, "</xdr:grpSp><xdr:clientData/></xdr:absoluteAnchor></xdr:wsDr>");

  fclose(d->out);
  free(d);
}

static void Xdr_Clip(double x0, double x1, double y0, double y1, pDevDesc dd) {
  xdrDesc *d = (xdrDesc *) dd->deviceSpecific;
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

static double char_width_frac(unsigned char c) {
  if (c >= 32 && c <= 126) return CHAR_W[c - 32];
  return FALLBACK_CHAR_W;
}

static double Xdr_StrWidth(const char *str, const pGEcontext gc, pDevDesc dd) {
  (void) dd;
  double sz = gc->cex * gc->ps;
  double total = 0.0;
  for (const unsigned char *p = (const unsigned char *) str; *p != '\0'; p++) {
    /* Skip UTF-8 continuation bytes (10xxxxxx); treat each lead byte as
     one glyph at the fallback width, since we don't have real metrics
     for non-ASCII text. */
    if ((*p & 0xC0) == 0x80) continue;
    total += (*p < 0x80) ? char_width_frac(*p) : FALLBACK_CHAR_W;
  }
  return total * sz;
}

static void Xdr_MetricInfo(int c, const pGEcontext gc, double *ascent,
                           double *descent, double *width, pDevDesc dd) {
  (void) dd;
  double sz = gc->cex * gc->ps;
  *ascent  = 0.75 * sz;
  *descent = 0.25 * sz;
  *width   = (c < 0 || c > 126) ? FALLBACK_CHAR_W * sz : char_width_frac((unsigned char) c) * sz;
}

static void Xdr_Line(double x1, double y1, double x2, double y2,
                     const pGEcontext gc, pDevDesc dd) {
  xdrDesc *d = (xdrDesc *) dd->deviceSpecific;
  double cx0 = fmin(d->clip_x0, d->clip_x1), cx1 = fmax(d->clip_x0, d->clip_x1);
  double cy0 = fmin(d->clip_y0, d->clip_y1), cy1 = fmax(d->clip_y0, d->clip_y1);
  if (!clip_line_lb(cx0, cy0, cx1, cy1, &x1, &y1, &x2, &y2)) return;
  sp_open(d, "");
  fprintf(d->out, "<xdr:spPr>");
  xfrm(d, x1, y1, x2, y2);
  double x_min = fmin(x1, x2) * PT_TO_EMU;
  double y_min = fmin(y1, y2) * PT_TO_EMU;
  double w_emu = fabs(x2 - x1) * PT_TO_EMU;
  double h_emu = fabs(y2 - y1) * PT_TO_EMU;
  double pts_x[2] = {x1, x2};
  double pts_y[2] = {y1, y2};
  points_to_pts(d, pts_x, pts_y, 2, x_min, y_min, w_emu, h_emu, FALSE);
  line_props(d, gc->col, gc->lwd, gc->lty);
  fprintf(d->out, "</xdr:spPr><xdr:txBody><a:bodyPr/><a:p/></xdr:txBody></xdr:sp>\n");
}

static void Xdr_Rect(double x0, double y0, double x1, double y1,
                     const pGEcontext gc, pDevDesc dd) {
  xdrDesc *d = (xdrDesc *) dd->deviceSpecific;
  double cx0 = fmin(d->clip_x0, d->clip_x1), cx1 = fmax(d->clip_x0, d->clip_x1);
  double cy0 = fmin(d->clip_y0, d->clip_y1), cy1 = fmax(d->clip_y0, d->clip_y1);
  double rx0 = fmax(fmin(x0, x1), cx0), rx1 = fmin(fmax(x0, x1), cx1);
  double ry0 = fmax(fmin(y0, y1), cy0), ry1 = fmin(fmax(y0, y1), cy1);
  if (rx1 <= rx0 || ry1 <= ry0) return;
  sp_open(d, "");
  fprintf(d->out, "<xdr:spPr>");
  xfrm(d, rx0, ry0, rx1, ry1);
  fprintf(d->out, "<a:prstGeom prst=\"rect\"><a:avLst/></a:prstGeom>");
  fill_props_gc(d, gc);
  line_props(d, gc->col, gc->lwd, gc->lty);
  fprintf(d->out, "</xdr:spPr><xdr:txBody><a:bodyPr/><a:p/></xdr:txBody></xdr:sp>\n");
}

static void Xdr_Circle(double x, double y, double r, const pGEcontext gc, pDevDesc dd) {
  xdrDesc *d = (xdrDesc *) dd->deviceSpecific;
  if (fully_outside_clip(d, x - r, y - r, x + r, y + r)) return;
  sp_open(d, "");
  fprintf(d->out, "<xdr:spPr>");
  xfrm(d, x - r, y - r, x + r, y + r);
  fprintf(d->out, "<a:prstGeom prst=\"ellipse\"><a:avLst/></a:prstGeom>");
  fill_props_gc(d, gc);
  line_props(d, gc->col, gc->lwd, gc->lty);
  fprintf(d->out, "</xdr:spPr><xdr:txBody><a:bodyPr/><a:p/></xdr:txBody></xdr:sp>\n");
}

static void bbox(double *x, double *y, int n, double *x0, double *y0,
                 double *x1, double *y1) {
  *x0 = *x1 = x[0]; *y0 = *y1 = y[0];
  for (int i = 1; i < n; i++) {
    if (x[i] < *x0) *x0 = x[i];
    if (x[i] > *x1) *x1 = x[i];
    if (y[i] < *y0) *y0 = y[i];
    if (y[i] > *y1) *y1 = y[i];
  }
}

static void Xdr_Polyline(int n, double *x, double *y, const pGEcontext gc, pDevDesc dd) {
  xdrDesc *d = (xdrDesc *) dd->deviceSpecific;
  double cx0 = fmin(d->clip_x0, d->clip_x1), cx1 = fmax(d->clip_x0, d->clip_x1);
  double cy0 = fmin(d->clip_y0, d->clip_y1), cy1 = fmax(d->clip_y0, d->clip_y1);

  /* Emit each clipped segment as a separate shape. Segments that vanish after
   clipping are skipped. Consecutive visible segments could be merged but the
   complexity is not worth it — polylines in R plots rarely have >100 points. */
  for (int i = 0; i < n - 1; i++) {
    double ax = x[i], ay = y[i], bx = x[i + 1], by = y[i + 1];
    if (!clip_line_lb(cx0, cy0, cx1, cy1, &ax, &ay, &bx, &by)) continue;
    sp_open(d, "");
    fprintf(d->out, "<xdr:spPr>");
    xfrm(d, ax, ay, bx, by);
    double x_min = fmin(ax, bx) * PT_TO_EMU;
    double y_min = fmin(ay, by) * PT_TO_EMU;
    double w_emu = fabs(bx - ax) * PT_TO_EMU;
    double h_emu = fabs(by - ay) * PT_TO_EMU;
    double pts_x[2] = {ax, bx};
    double pts_y[2] = {ay, by};
    points_to_pts(d, pts_x, pts_y, 2, x_min, y_min, w_emu, h_emu, FALSE);
    fprintf(d->out, "<a:noFill/>");
    line_props(d, gc->col, gc->lwd, gc->lty);
    fprintf(d->out, "</xdr:spPr><xdr:txBody><a:bodyPr/><a:p/></xdr:txBody></xdr:sp>\n");
  }
}

static void Xdr_Polygon(int n, double *x, double *y, const pGEcontext gc, pDevDesc dd) {
  xdrDesc *d = (xdrDesc *) dd->deviceSpecific;
  double cx0 = fmin(d->clip_x0, d->clip_x1), cx1 = fmax(d->clip_x0, d->clip_x1);
  double cy0 = fmin(d->clip_y0, d->clip_y1), cy1 = fmax(d->clip_y0, d->clip_y1);

  /* Sutherland-Hodgman: output may have up to n+4 vertices per clip edge, 4 edges. */
  int cap = (n + 4) * 4;
  double *buf1x = (double *) R_alloc((size_t) cap, sizeof(double));
  double *buf1y = (double *) R_alloc((size_t) cap, sizeof(double));
  double *buf2x = (double *) R_alloc((size_t) cap, sizeof(double));
  double *buf2y = (double *) R_alloc((size_t) cap, sizeof(double));
  double *ox, *oy;
  int m = clip_polygon_sh(x, y, n, cx0, cy0, cx1, cy1,
                          buf1x, buf1y, buf2x, buf2y, &ox, &oy);
  if (m < 2) return;

  double x0, y0, x1, y1;
  bbox(ox, oy, m, &x0, &y0, &x1, &y1);
  sp_open(d, "");
  fprintf(d->out, "<xdr:spPr>");
  xfrm(d, x0, y0, x1, y1);
  double x_min = fmin(x0, x1) * PT_TO_EMU;
  double y_min = fmin(y0, y1) * PT_TO_EMU;
  double w_emu = fabs(x1 - x0) * PT_TO_EMU;
  double h_emu = fabs(y1 - y0) * PT_TO_EMU;
  points_to_pts(d, ox, oy, m, x_min, y_min, w_emu, h_emu, TRUE);
  fill_props_gc(d, gc);
  line_props(d, gc->col, gc->lwd, gc->lty);
  fprintf(d->out, "</xdr:spPr><xdr:txBody><a:bodyPr/><a:p/></xdr:txBody></xdr:sp>\n");
}

static void Xdr_Path(double *x, double *y, int npoly, int *nper,
                     Rboolean winding, const pGEcontext gc, pDevDesc dd) {
  /* DrawingML custGeom has no evenodd fill rule; all paths use nonzero winding.
   Both R fill rules produce the same output — acceptable given the format limit. */
  (void) winding;
  xdrDesc *d = (xdrDesc *) dd->deviceSpecific;
  if (npoly < 1) return;

  int total = 0;
  for (int k = 0; k < npoly; k++) total += nper[k];
  if (total < 2) return;

  double cx0 = fmin(d->clip_x0, d->clip_x1), cx1 = fmax(d->clip_x0, d->clip_x1);
  double cy0 = fmin(d->clip_y0, d->clip_y1), cy1 = fmax(d->clip_y0, d->clip_y1);

  /* Clip each ring and collect surviving vertices to compute the global bbox. */
  int max_ring = 0;
  for (int k = 0; k < npoly; k++) if (nper[k] > max_ring) max_ring = nper[k];
  int cap = (max_ring + 4) * 4;
  double *buf1x = (double *) R_alloc((size_t) cap, sizeof(double));
  double *buf1y = (double *) R_alloc((size_t) cap, sizeof(double));
  double *buf2x = (double *) R_alloc((size_t) cap, sizeof(double));
  double *buf2y = (double *) R_alloc((size_t) cap, sizeof(double));

  /* clipped ring storage: at most cap vertices per ring, npoly rings */
  double *crx = (double *) R_alloc((size_t)(cap * npoly), sizeof(double));
  double *cry = (double *) R_alloc((size_t)(cap * npoly), sizeof(double));
  int    *crn = (int *)    R_alloc((size_t) npoly, sizeof(int));

  int idx = 0, any = 0;
  double gx0 = R_PosInf, gy0 = R_PosInf, gx1 = R_NegInf, gy1 = R_NegInf;
  for (int k = 0; k < npoly; k++) {
    double *ox, *oy;
    int m = clip_polygon_sh(x + idx, y + idx, nper[k],
                            cx0, cy0, cx1, cy1,
                            buf1x, buf1y, buf2x, buf2y, &ox, &oy);
    crn[k] = m;
    if (m >= 2) {
      for (int i = 0; i < m; i++) {
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

  double x_min = gx0 * PT_TO_EMU;
  double y_min = gy0 * PT_TO_EMU;
  double w_emu = (gx1 - gx0) * PT_TO_EMU;
  double h_emu = (gy1 - gy0) * PT_TO_EMU;
  double w_or1 = w_emu <= 0 ? 1.0 : w_emu;
  double h_or1 = h_emu <= 0 ? 1.0 : h_emu;

  sp_open(d, "");
  fprintf(d->out, "<xdr:spPr>");
  xfrm(d, gx0, gy0, gx1, gy1);
  fprintf(d->out,
          "<a:custGeom><a:avLst/><a:gdLst/><a:ahLst/><a:cxnLst/>"
          "<a:rect l=\"0\" t=\"0\" r=\"%.0f\" b=\"%.0f\"/><a:pathLst>",
            w_or1, h_or1);

  for (int k = 0; k < npoly; k++) {
    int m = crn[k];
    if (m < 2) continue;
    fprintf(d->out, "<a:path w=\"%.0f\" h=\"%.0f\">", w_or1, h_or1);
    for (int i = 0; i < m; i++) {
      double px = crx[k * cap + i] * PT_TO_EMU - x_min;
      double py = cry[k * cap + i] * PT_TO_EMU - y_min;
      fprintf(d->out, "<a:%s><a:pt x=\"%.0f\" y=\"%.0f\"/></a:%s>",
              i == 0 ? "moveTo" : "lnTo", px, py,
              i == 0 ? "moveTo" : "lnTo");
    }
    fprintf(d->out, "<a:close/></a:path>");
  }
  fprintf(d->out, "</a:pathLst></a:custGeom>");
  fill_props_gc(d, gc);
  line_props(d, gc->col, gc->lwd, gc->lty);
  fprintf(d->out, "</xdr:spPr><xdr:txBody><a:bodyPr/><a:p/></xdr:txBody></xdr:sp>\n");
}

#define TEXT_Y_NUDGE 1.5

static void Xdr_TextImpl(double x, double y, const char *str, double rot,
                         double hadj, const pGEcontext gc, pDevDesc dd) {
  xdrDesc *d = (xdrDesc *) dd->deviceSpecific;
  char buf[2048];
  esc_xml(str, buf, sizeof(buf));
  double w = Xdr_StrWidth(str, gc, dd);
  double h = (gc->cex * gc->ps) * 1.2;

  double bx0, by0, bx1, by1;
  if (fabs(rot) > 1e-4) {
    /* Rotate around the box's own center (matching how the bodyPr rot
     attribute rotates a shape), solving for the center position such
     that the anchor point (x,y) lands correctly post-rotation. */
    double theta = -rot * M_PI / 180.0;
    double cos_r = cos(theta);
    double sin_r = sin(theta);
    double ox = hadj * w - w / 2.0;   /* local anchor x, relative to box center */
    double oy = h / 2.0;              /* local anchor y (baseline), relative to center */
    double rot_ox = ox * cos_r - oy * sin_r;
    double rot_oy = ox * sin_r + oy * cos_r;
    double cx = x - rot_ox;
    double cy = y - rot_oy;
    bx0 = cx - w / 2.0; by0 = cy - h / 2.0; bx1 = bx0 + w; by1 = by0 + h;
  } else {
    double base_x = x - hadj * w;
    bx0 = base_x; by0 = y - h + TEXT_Y_NUDGE; bx1 = base_x + w; by1 = y + TEXT_Y_NUDGE;
  }
  if (fully_outside_clip(d, bx0, by0, bx1, by1)) return;

  sp_open(d, "");
  fprintf(d->out, "<xdr:spPr>");

  xfrm(d, bx0, by0, bx1, by1);

  fprintf(d->out, "<a:prstGeom prst=\"rect\"><a:avLst/></a:prstGeom><a:noFill/>"
            "<a:ln><a:noFill/></a:ln></xdr:spPr>");

  if (fabs(rot) > 1e-4) {
    int ooxml_rot = (int) (-rot * 60000.0);
    fprintf(d->out, "<xdr:txBody><a:bodyPr rot=\"%d\" vert=\"horz\" anchor=\"ctr\" wrap=\"none\" lIns=\"0\" tIns=\"0\" rIns=\"0\" bIns=\"0\"/>", ooxml_rot);
  } else {
    fprintf(d->out, "<xdr:txBody><a:bodyPr anchor=\"b\" wrap=\"none\" lIns=\"0\" tIns=\"0\" rIns=\"0\" bIns=\"0\"/>");
  }

  int alpha = (int)(R_ALPHA(gc->col) / 255.0 * 100000.0 + 0.5);
  const char *b_attr = (gc->fontface == 2 || gc->fontface == 4) ? " b=\"1\"" : "";
  const char *i_attr = (gc->fontface == 3 || gc->fontface == 4) ? " i=\"1\"" : "";
  const char *u_attr = d->underline ? " u=\"sng\"" : "";
  const char *strike_attr = d->strikeout ? " strike=\"sngStrike\"" : "";

  const char *algn = (hadj < 0.25) ? "l" : (hadj > 0.75) ? "r" : "ctr";
  const char *font = is_generic_family(gc->fontfamily) ? d->fontname : gc->fontfamily;

  fprintf(d->out,
          "<a:p><a:pPr algn=\"%s\"/><a:r>"
          "<a:rPr sz=\"%d\"%s%s%s%s>"
          "<a:solidFill><a:srgbClr val=\"%06X\"><a:alpha val=\"%d\"/></a:srgbClr></a:solidFill>"
          "<a:latin typeface=\"%s\"/><a:cs typeface=\"%s\"/></a:rPr>"
          "<a:t>%s</a:t></a:r></a:p></xdr:txBody></xdr:sp>\n",
          algn,                           // %s (algn)
          (int)(gc->cex * gc->ps * 100), // %d (sz)
          b_attr,                        // %s (b_attr)
          i_attr,                        // %s (i_attr)
          u_attr,                        // %s (u_attr)
          strike_attr,                   // %s (strike_attr)
          rgb_hex(gc->col),              // %06X (color)
          alpha,                         // %d (alpha)
          font,                          // %s (latin typeface)
          font,                          // %s (cs typeface)
          buf);                          // %s (text)
}

static void Xdr_Text(double x, double y, const char *str, double rot,
                     double hadj, const pGEcontext gc, pDevDesc dd) {
  Xdr_TextImpl(x, y, str, rot, hadj, gc, dd);
}

static SEXP Xdr_SetPattern(SEXP pattern, pDevDesc dd) {
  (void) dd;
  return pattern;
}

static void Xdr_ReleasePattern(SEXP ref, pDevDesc dd) {
  (void) ref; (void) dd;
}

static SEXP Xdr_SetClipPath(SEXP path, SEXP ref, pDevDesc dd) {
  (void) path; (void) ref; (void) dd;
  return R_NilValue;
}

static void Xdr_ReleaseClipPath(SEXP ref, pDevDesc dd) {
  (void) ref; (void) dd;
}

static SEXP Xdr_SetMask(SEXP path, SEXP ref, pDevDesc dd) {
  (void) path; (void) ref; (void) dd;
  return R_NilValue;
}

static void Xdr_ReleaseMask(SEXP ref, pDevDesc dd) {
  (void) ref; (void) dd;
}

SEXP easeling_(SEXP path_, SEXP width_, SEXP height_, SEXP pointsize_,
               SEXP fontname_, SEXP underline_, SEXP strikeout_) {
  const char *path = CHAR(STRING_ELT(path_, 0));
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

  xd->out = fopen(path, "w");
  if (xd->out == NULL) { free(xd); free(dd); Rf_error("cannot open '%s'", path); }
  xd->shape_id = 1;
  xd->clip_x0 = 0; xd->clip_y0 = 0;
  xd->clip_x1 = width * 72.0; xd->clip_y1 = height * 72.0;
  strncpy(xd->fontname, fontname, sizeof(xd->fontname) - 1);
  xd->fontname[sizeof(xd->fontname) - 1] = '\0';
  xd->underline = underline;
  xd->strikeout = strikeout;

  double dev_w = width * 72.0;
  double dev_h = height * 72.0;
  double emu_w = dev_w * PT_TO_EMU;
  double emu_h = dev_h * PT_TO_EMU;

  fprintf(xd->out,
          "<xdr:wsDr xmlns:xdr=\"http://schemas.openxmlformats.org/drawingml/2006/spreadsheetDrawing\" "
            "xmlns:a=\"http://schemas.openxmlformats.org/drawingml/2006/main\">"
            "<xdr:absoluteAnchor>"
            "<xdr:pos x=\"0\" y=\"0\"/>"
            "<xdr:ext cx=\"%.0f\" cy=\"%.0f\"/>"
            "<xdr:grpSp><xdr:nvGrpSpPr><xdr:cNvPr id=\"0\" name=\"R_Graphics_Group\"/><xdr:cNvGrpSpPr/></xdr:nvGrpSpPr>"
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
  dd->canHAdj = 0;
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
  dd->wantSymbolUTF8 = FALSE;
  dd->useRotatedTextInContour = FALSE;

  dd->eventEnv = R_NilValue;
  dd->eventHelper = NULL;
  dd->holdflush = NULL;

  dd->haveTransparency = 2;
  dd->haveTransparentBg = 2;
  dd->haveRaster = 2; /* yes - Xdr_Raster is implemented (RLE rects) */
  dd->haveCapture = 1; /* no - dd->cap is NULL; R checks before calling, so safe */
  dd->haveLocator = 1; /* no - dd->locator is NULL; R checks before calling, so safe */

  dd->deviceVersion = R_GE_definitions;
  dd->deviceClip = FALSE;

  dd->setPattern = Xdr_SetPattern;
  dd->releasePattern = Xdr_ReleasePattern;
  dd->setClipPath = Xdr_SetClipPath;
  dd->releaseClipPath = Xdr_ReleaseClipPath;
  dd->setMask = Xdr_SetMask;
  dd->releaseMask = Xdr_ReleaseMask;

  pGEDevDesc gdd = GEcreateDevDesc(dd);
  GEaddDevice2(gdd, "easeling");
  GEinitDisplayList(gdd);

  return R_NilValue;
}

static const R_CallMethodDef CallEntries[] = {
  {"easeling_", (DL_FUNC) &easeling_, 7},
  {NULL, NULL, 0}
};

void R_init_easeling(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
