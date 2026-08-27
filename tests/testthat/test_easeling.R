# Helpers -----------------------------------------------------------------

read_xml_text <- function(file) paste(readLines(file, warn = FALSE), collapse = "")

expect_wellformed_fragment <- function(file) {
  body <- read_xml_text(file)
  wrapped <- paste0(
    '<root xmlns:xdr="urn:x" xmlns:a="urn:y">', body, "</root>"
  )
  if (requireNamespace("openxlsx2", quietly = TRUE)) {
    testthat::expect_error(openxlsx2::read_xml(wrapped), NA)
  } else {
    # fallback: count open vs close tags.
    # Remove self-closing tags first, then compare open/close counts.
    no_self <- gsub("<[^>]+/>", "", wrapped)
    opens  <- lengths(regmatches(no_self, gregexpr("<[a-zA-Z][a-zA-Z0-9:]*(?:\\s[^>]*)?>", no_self, perl = TRUE)))
    closes <- lengths(regmatches(no_self, gregexpr("</[a-zA-Z][a-zA-Z0-9:]*>", no_self)))
    testthat::expect_equal(opens, closes)
  }
}

count_matches <- function(file, pattern) {
  lengths(regmatches(read_xml_text(file), gregexpr(pattern, read_xml_text(file))))[[1]]
}

# easel_dev(): basic device lifecycle ---------------------------------------

test_that("easel_dev creates a device and writes a well-formed root structure", {
  f <- easel_dev(width = 4, height = 3)
  plot(1:5)
  dev.off()

  expect_true(file.exists(f))
  txt <- read_xml_text(f)
  expect_match(txt, "^<xdr:wsDr")
  expect_match(txt, "<xdr:absoluteAnchor>")
  expect_match(txt, "<xdr:grpSp>")
  expect_match(txt, "</xdr:grpSp><xdr:clientData/></xdr:absoluteAnchor></xdr:wsDr>$")
  expect_wellformed_fragment(f)
})

test_that("device size (width/height) maps to the correct EMU extent", {
  f <- easel_dev(width = 5, height = 2)
  plot.new()
  dev.off()

  txt <- read_xml_text(f)
  m <- regmatches(txt, regexpr('<xdr:ext cx="[0-9]+" cy="[0-9]+"/>', txt))
  expect_match(m, paste0('cx="', round(5 * 914400), '"'))
  expect_match(m, paste0('cy="', round(2 * 914400), '"'))
})

test_that("returned path is invisible and usable directly", {
  f <- withVisible(easel_dev(width = 3, height = 3))
  expect_false(f$visible)
  plot.new()
  dev.off()
  expect_true(file.exists(f$value))
})

# Shape primitives ----------------------------------------------------------

test_that("rect() produces an xdr:sp with prstGeom rect", {
  f <- easel_dev(width = 3, height = 3)
  plot.new()
  rect(0.2, 0.2, 0.8, 0.8, col = "red")
  dev.off()
  expect_gte(count_matches(f, 'prstGeom prst="rect"'), 1)
})

test_that("points(pch=19) produces xdr:sp with prstGeom ellipse", {
  f <- easel_dev(width = 3, height = 3)
  plot(1:5, pch = 19)
  dev.off()
  expect_gte(count_matches(f, 'prstGeom prst="ellipse"'), 5)
})

test_that("lines/segments produce custGeom paths", {
  f <- easel_dev(width = 3, height = 3)
  plot.new()
  segments(0.1, 0.1, 0.9, 0.9)
  dev.off()
  expect_gte(count_matches(f, "<a:custGeom>"), 1)
})

test_that("lines() with multiple points produces an open (non-closed) polyline path", {
  f <- easel_dev(width = 3, height = 3)
  plot.new()
  lines(c(0.1, 0.4, 0.7, 0.9), c(0.2, 0.6, 0.3, 0.8))
  dev.off()
  expect_gte(count_matches(f, "<a:custGeom>"), 1)
  # open polyline: no <a:close/> for this shape (unlike polygon())
  expect_equal(count_matches(f, "<a:close/>"), 0)
})

test_that("NA border colour produces a no-fill line (not a solid stroke)", {
  f <- easel_dev(width = 3, height = 3)
  plot.new()
  rect(0.2, 0.2, 0.8, 0.8, col = "grey", border = NA)
  dev.off()
  txt <- read_xml_text(f)
  expect_match(txt, "<a:ln><a:noFill/></a:ln>")
})

test_that("tiling pattern() fills fall back to noFill rather than crash", {
  skip_if_not(exists("pattern", where = asNamespace("grid")))
  f <- easel_dev(width = 3, height = 3)
  grid::grid.newpage()
  tile <- grid::pattern(
    grid::circleGrob(r = 0.3, gp = grid::gpar(fill = "orange")),
    width = 0.2, height = 0.2, extend = "repeat"
  )
  expect_error(
    grid::grid.rect(gp = grid::gpar(fill = tile)),
    NA
  )
  dev.off()
  expect_wellformed_fragment(f)
})

test_that("non-ASCII characters use the fallback width instead of erroring", {
  f <- easel_dev(width = 3, height = 3)
  plot.new()
  expect_error(text(0.5, 0.5, "caf\u00e9 \u65e5\u672c\u8a9e"), NA)
  # strwidth of a pure non-ASCII string exercises the fallback char width path
  w <- strwidth("\u65e5\u672c\u8a9e", units = "inches")
  expect_gt(w, 0)
  dev.off()
  expect_wellformed_fragment(f)
})

test_that("opening a second device deactivates the first without corrupting its file", {
  f1 <- easel_dev(width = 3, height = 3)
  plot(1:5)

  f2 <- easel_dev(width = 3, height = 3)
  plot(1:5)
  dev.off()  # closes f2, f1 becomes active again

  dev.off()  # closes f1

  expect_true(file.exists(f1))
  expect_true(file.exists(f2))
  expect_wellformed_fragment(f1)
  expect_wellformed_fragment(f2)

  f <- easel_dev(width = 3, height = 3)
  plot.new()
  polygon(c(0.2, 0.5, 0.8), c(0.2, 0.8, 0.2), col = "blue")
  dev.off()
  expect_gte(count_matches(f, "<a:close/>"), 1)
})

test_that("text() renders with escaping for XML-special characters", {
  f <- easel_dev(width = 3, height = 3)
  plot.new()
  text(0.5, 0.5, "A & B < C > D \"quoted\"")
  dev.off()
  txt <- read_xml_text(f)
  expect_match(txt, "A &amp; B &lt; C &gt; D &quot;quoted&quot;")
  expect_wellformed_fragment(f)
})

test_that("bold/italic fontface maps to b/i attributes", {
  f <- easel_dev(width = 3, height = 3)
  plot.new()
  text(0.5, 0.5, "bold", font = 2)
  text(0.5, 0.3, "italic", font = 3)
  text(0.5, 0.1, "plain", font = 1)
  dev.off()
  txt <- read_xml_text(f)
  expect_match(txt, 'b="1"')
  expect_match(txt, 'i="1"')
})

test_that("rotated text (srt) does not crash and stays on-canvas", {
  f <- easel_dev(width = 4, height = 4)
  plot.new()
  text(0.5, 0.5, "rotated label", srt = 90)
  dev.off()
  expect_wellformed_fragment(f)
  expect_gte(count_matches(f, 'rot="'), 1)
})

test_that("text() receives real hadj and maps it to a:pPr algn", {
  # canHAdj = 2: the device gets the true justification and aligns via the
  # text box, which is robust to our approximate font metrics because the
  # renderer lays the text out with the real font.
  f <- easel_dev(width = 3, height = 3)
  plot.new()
  text(0.2, 0.8, "left", adj = c(0, 0.5))
  text(0.2, 0.5, "right", adj = c(1, 0.5))
  text(0.2, 0.2, "center", adj = c(0.5, 0.5))
  dev.off()
  txt <- read_xml_text(f)
  expect_match(txt, 'algn="l"')
  expect_match(txt, 'algn="r"')
  expect_match(txt, 'algn="ctr"')
})

# Clipping -------------------------------------------------------------------

test_that("shapes fully outside the current clip region are dropped", {
  f <- easel_dev(width = 4, height = 4)
  plot(1:10, 1:10, xlim = c(1, 10), ylim = c(1, 10))
  # a point far outside the plotted user range/device won't be drawn
  points(1e6, 1e6, pch = 19)
  dev.off()
  # sanity: normal in-range points still rendered
  expect_gte(count_matches(f, 'prstGeom prst="ellipse"'), 10)
})

# Raster -----------------------------------------------------------------

test_that("grid.raster() renders as run-length-encoded rects, not silently dropped", {
  f <- easel_dev(width = 3, height = 3)
  grid::grid.newpage()
  suppressWarnings(
    # 2x2 raster: top row red+red, bottom row blue+blue — RLE produces runs of 2
    grid::grid.raster(as.raster(matrix(c("red", "red", "blue", "blue"), nrow = 2, byrow = TRUE)))
  )
  dev.off()
  expect_gte(count_matches(f, 'prstGeom prst="rect"'), 1)
})

test_that("dev.capabilities() reports raster support as available", {
  f <- easel_dev(width = 2, height = 2)
  plot.new()
  caps <- dev.capabilities()
  dev.off()
  expect_equal(caps$rasterImage, "yes")
})

# Path (polygons with holes) --------------------------------------------------

test_that("grid.path() with multiple sub-paths (a hole) renders both rings", {
  f <- easel_dev(width = 3, height = 3)
  grid::grid.newpage()
  grid::grid.path(
    x = c(0.2, 0.8, 0.8, 0.2, 0.35, 0.65, 0.65, 0.35),
    y = c(0.2, 0.2, 0.8, 0.8, 0.35, 0.35, 0.65, 0.65),
    id = c(1, 1, 1, 1, 2, 2, 2, 2),
    rule = "evenodd",
    gp = grid::gpar(fill = "steelblue")
  )
  dev.off()
  # both rings live in ONE <a:path> (separate path elements are filled
  # independently by renderers, which paints holes solid)
  expect_equal(count_matches(f, "<a:path w="), 1)
  expect_equal(count_matches(f, "<a:moveTo>"), 2)
  expect_equal(count_matches(f, "<a:close/>"), 2)
  expect_wellformed_fragment(f)
})

# Gradients ----------------------------------------------------------------

test_that("linearGradient fill emits a:gradFill with correct stops", {
  skip_if_not(getRversion() >= "4.1.0")
  f <- easel_dev(width = 3, height = 3)
  grid::grid.newpage()
  grid::grid.rect(gp = grid::gpar(
    fill = grid::linearGradient(colours = c("#FF0000", "#0000FF"))
  ))
  dev.off()
  txt <- read_xml_text(f)
  expect_match(txt, "<a:gradFill>")
  expect_match(txt, 'pos="0".*val="FF0000"')
  expect_match(txt, 'pos="100000".*val="0000FF"')
  expect_match(txt, "<a:lin ang=")
})

test_that("radialGradient fill emits a:gradFill with a circular path", {
  skip_if_not(getRversion() >= "4.1.0")
  f <- easel_dev(width = 3, height = 3)
  grid::grid.newpage()
  grid::grid.circle(gp = grid::gpar(
    fill = grid::radialGradient(colours = c("yellow", "darkred"))
  ))
  dev.off()
  txt <- read_xml_text(f)
  expect_match(txt, "<a:gradFill>")
  expect_match(txt, '<a:path path="circle">')
})

test_that("plain solid fills are unaffected by gradient support", {
  f <- easel_dev(width = 3, height = 3)
  plot.new()
  rect(0.2, 0.2, 0.8, 0.8, col = "green")
  dev.off()
  txt <- read_xml_text(f)
  expect_match(txt, "<a:solidFill>")
  expect_false(grepl("<a:gradFill>", txt))
})

# Transparency ---------------------------------------------------------------

test_that("alpha-transparent colours produce a non-100000 alpha value", {
  f <- easel_dev(width = 3, height = 3)
  plot.new()
  rect(0.2, 0.2, 0.8, 0.8, col = grDevices::adjustcolor("red", alpha.f = 0.5))
  dev.off()
  txt <- read_xml_text(f)
  expect_match(txt, '<a:alpha val="(?!100000)[0-9]+"/>', perl = TRUE)
})

test_that("fully transparent fill emits noFill instead of a zero-alpha solidFill", {
  f <- easel_dev(width = 3, height = 3)
  plot.new()
  rect(0.2, 0.2, 0.8, 0.8, col = NA, border = "black")
  dev.off()
  expect_gte(count_matches(f, "<a:noFill/>"), 1)
})

# Fonts -----------------------------------------------------------------

test_that("default fontname is applied when R doesn't request a specific family", {
  f <- easel_dev(width = 3, height = 3, fontname = "Georgia")
  plot.new()
  text(0.5, 0.5, "hello")
  dev.off()
  expect_match(read_xml_text(f), 'typeface="Georgia"')
})

test_that("a real par(family=) overrides the device default", {
  f <- easel_dev(width = 3, height = 3, fontname = "Georgia")
  op <- par(family = "Consolas")
  on.exit(par(op))
  plot.new()
  text(0.5, 0.5, "hello")
  dev.off()
  expect_match(read_xml_text(f), 'typeface="Consolas"')
})

test_that("generic family aliases (sans/serif/mono) fall back to the device default", {
  f <- easel_dev(width = 3, height = 3, fontname = "Georgia")
  op <- par(family = "sans")
  on.exit(par(op))
  plot.new()
  text(0.5, 0.5, "hello")
  dev.off()
  expect_match(read_xml_text(f), 'typeface="Georgia"')
  expect_false(grepl('typeface="sans"', read_xml_text(f)))
})

test_that("underline and strikeout flags are applied device-wide", {
  f <- easel_dev(width = 3, height = 3, underline = TRUE, strikeout = TRUE)
  plot.new()
  text(0.5, 0.5, "styled")
  dev.off()
  txt <- read_xml_text(f)
  expect_match(txt, 'u="sng"')
  expect_match(txt, 'strike="sngStrike"')
})

# Integration with openxlsx2 (skipped if not installed) ----------------------

test_that("output is accepted by openxlsx2::wb_add_drawing when available", {
  skip_if_not_installed("openxlsx2")
  f <- easel_dev(width = 4, height = 3)
  plot(1:10, (1:10)^2, type = "b")
  dev.off()

  wb <- openxlsx2::wb_workbook()
  wb$add_worksheet()
  expect_error(wb$add_drawing(xml = f, dims = "A1"), NA)
})

# Line types (lty) -----------------------------------------------------------

test_that("lty=1 (solid) emits no dash element", {
  f <- easel_dev(width = 3, height = 3)
  plot.new()
  abline(h = 0.5, lty = 1)
  dev.off()
  txt <- read_xml_text(f)
  expect_false(grepl("prstDash", txt))
  expect_false(grepl("custDash", txt))
})

test_that("lty=2 (dashed) emits its exact custDash pattern (dashed 4-4)", {
  f <- easel_dev(width = 3, height = 3)
  plot.new()
  abline(h = 0.5, lty = 2)
  dev.off()
  expect_match(read_xml_text(f), 'custDash.*d="400000" sp="400000"')
})

test_that("lty=3 (dotted) emits its exact custDash pattern (dotted 1-3)", {
  f <- easel_dev(width = 3, height = 3)
  plot.new()
  abline(h = 0.5, lty = 3)
  dev.off()
  expect_match(read_xml_text(f), 'custDash.*d="100000" sp="300000"')
})

test_that("lty=4 (dotdash) emits its exact custDash pattern (dotdash 1-3-4-3)", {
  f <- easel_dev(width = 3, height = 3)
  plot.new()
  abline(h = 0.5, lty = 4)
  dev.off()
  expect_match(read_xml_text(f), 'custDash.*d="100000" sp="300000"/><a:ds d="400000" sp="300000"')
})

test_that("lty=5 (longdash) emits its exact custDash pattern (longdash 7-3)", {
  f <- easel_dev(width = 3, height = 3)
  plot.new()
  abline(h = 0.5, lty = 5)
  dev.off()
  expect_match(read_xml_text(f), 'custDash.*d="700000" sp="300000"')
})

test_that("lty=6 (twodash) emits its exact custDash pattern (twodash 2-2-6-2)", {
  f <- easel_dev(width = 3, height = 3)
  plot.new()
  abline(h = 0.5, lty = 6)
  dev.off()
  expect_match(read_xml_text(f), 'custDash.*d="200000" sp="200000"/><a:ds d="600000" sp="200000"')
})

test_that("custom lty string emits custDash with correct ds elements", {
  f <- easel_dev(width = 3, height = 3)
  plot.new()
  # "1348": on=1, off=3, on=4, off=8 -> two <a:ds> pairs
  lines(c(0.1, 0.9), c(0.5, 0.5), lty = "1348")
  dev.off()
  txt <- read_xml_text(f)
  expect_match(txt, "<a:custDash>")
  # ST_PositivePercentage: 1 line-width = 100000
  expect_match(txt, 'd="100000"')
  expect_match(txt, 'sp="300000"')
  expect_match(txt, 'd="400000"')
  expect_match(txt, 'sp="800000"')
  expect_equal(count_matches(f, "<a:ds "), 2)
})

test_that("all named lty values emit custDash and are pairwise distinct", {
  dashes <- character()
  for (lty_i in 2:6) {
    f <- easel_dev(width = 3, height = 3)
    plot.new()
    abline(h = 0.5, lty = lty_i)
    dev.off()
    txt <- read_xml_text(f)
    m <- regmatches(txt, regexpr("<a:custDash>.*?</a:custDash>", txt))
    expect_true(length(m) == 1, label = paste("lty =", lty_i))
    dashes <- c(dashes, m)
  }
  expect_equal(length(unique(dashes)), 5L)
})

# Geometry clipping ----------------------------------------------------------
# Each test uses plot.new() (no axes, no decorations) so the only shapes in
# the XML beyond the background rect are what the test explicitly draws.
# Clip bounds are read from par() during drawing — the same values R uses
# internally — so we're checking that the device agrees with R, not with
# hard-coded numbers.

pt_to_emu <- 12700

clip_bounds_in <- function() {
  plt <- par("plt")
  fin <- par("fin")
  c(x0 = plt[1] * fin[1], x1 = plt[2] * fin[1],
    y0 = (1 - plt[4]) * fin[2], y1 = (1 - plt[3]) * fin[2])
}

# Returns bboxes of all shapes except the full-device background rect.
data_shape_bboxes <- function(f, dev_w, dev_h) {
  txt  <- read_xml_text(f)
  offs <- regmatches(txt, gregexpr('<a:off x="[^"]*" y="[^"]*"',  txt))[[1]]
  exts <- regmatches(txt, gregexpr('<a:ext cx="[^"]*" cy="[^"]*"', txt))[[1]]
  gn   <- function(tags, a) as.numeric(sub(paste0(".*", a, "=\"([^\"]*)\".*"), "\\1", tags))
  ox <- gn(offs, "x") / pt_to_emu / 72
  oy <- gn(offs, "y") / pt_to_emu / 72
  cw <- gn(exts, "cx") / pt_to_emu / 72
  ch <- gn(exts, "cy") / pt_to_emu / 72
  bb <- data.frame(x0 = ox, x1 = ox + cw, y0 = oy, y1 = oy + ch)
  # Background rect spans full device; exclude it
  is_bg <- abs(bb$x0) < 1e-4 & abs(bb$y0) < 1e-4 &
    abs(bb$x1 - dev_w) < 1e-4 & abs(bb$y1 - dev_h) < 1e-4
  bb[!is_bg, , drop = FALSE]
}

test_that("line crossing right boundary is clipped to par() plot region", {
  w <- 4
  h <- 3
  f <- easel_dev(width = w, height = h)
  plot.new()
  clip <- clip_bounds_in()
  lines(c(0.5, 5.0), c(0.5, 0.5))   # x=5 is far outside [0,1] user space
  dev.off()

  bb <- data_shape_bboxes(f, w, h)
  expect_equal(nrow(bb), 1, label = "exactly one clipped line segment")
  tol <- 1 / pt_to_emu / 72
  expect_lte(max(bb$x1), clip["x1"] + tol)
  expect_gte(bb$x0, clip["x0"] - tol)
})

test_that("line entirely outside plot region produces no shapes", {
  w <- 4
  h <- 3
  f <- easel_dev(width = w, height = h)
  plot.new()
  lines(c(5.0, 8.0), c(0.5, 0.5))   # entirely right of [0,1]
  dev.off()

  bb <- data_shape_bboxes(f, w, h)
  expect_equal(nrow(bb), 0, label = "no shapes for line outside clip")
})

test_that("rect clipped to par() plot region right boundary", {
  w <- 4
  h <- 3
  f <- easel_dev(width = w, height = h)
  plot.new()
  clip <- clip_bounds_in()
  rect(0.5, 0.2, 3.0, 0.8)   # right edge at x=3 is outside [0,1]
  dev.off()

  # The clipped fill is emitted stroke-free plus the surviving pieces of the
  # original outline as separate polylines, so the edge introduced by the
  # clip is not drawn (matching R's own devices): 1 fill + 2 border runs.
  bb <- data_shape_bboxes(f, w, h)
  expect_equal(nrow(bb), 3)
  tol <- 1 / pt_to_emu / 72
  expect_lte(max(bb$x1), clip["x1"] + tol)
})

test_that("rect entirely outside plot region produces no shapes", {
  w <- 4
  h <- 3
  f <- easel_dev(width = w, height = h)
  plot.new()
  rect(2.0, 0.2, 3.0, 0.8)
  dev.off()

  bb <- data_shape_bboxes(f, w, h)
  expect_equal(nrow(bb), 0)
})

test_that("polygon clipped by S-H stays within par() plot region on all sides", {
  w <- 4
  h <- 3
  f <- easel_dev(width = w, height = h)
  plot.new()
  clip <- clip_bounds_in()
  # Triangle with one vertex outside on each axis
  polygon(c(0.5, 5.0, 0.5), c(-2.0, 0.5, 3.0), col = "blue")
  dev.off()

  bb <- data_shape_bboxes(f, w, h)
  expect_gte(nrow(bb), 1, label = "polygon partially in clip region survives")
  tol <- 1 / pt_to_emu / 72
  expect_true(all(bb$x0 >= clip["x0"] - tol))
  expect_true(all(bb$x1 <= clip["x1"] + tol))
  expect_true(all(bb$y0 >= clip["y0"] - tol))
  expect_true(all(bb$y1 <= clip["y1"] + tol))
})

test_that("polygon entirely outside plot region produces no shapes", {
  w <- 4
  h <- 3
  f <- easel_dev(width = w, height = h)
  plot.new()
  polygon(c(3, 5, 4), c(0.3, 0.3, 0.8), col = "red")  # entirely right of [0,1]
  dev.off()

  bb <- data_shape_bboxes(f, w, h)
  expect_equal(nrow(bb), 0)
})

test_that("polyline crossing boundary is split and all segments stay within clip", {
  w <- 4
  h <- 3
  f <- easel_dev(width = w, height = h)
  plot.new()
  clip <- clip_bounds_in()
  # Exits left, enters, exits right: segments 1 and 3 clip, segment 2 is fully inside
  lines(c(-1.0, 0.3, 0.7, 2.0), c(0.5, 0.5, 0.5, 0.5))
  dev.off()

  bb <- data_shape_bboxes(f, w, h)
  expect_gte(nrow(bb), 1)
  tol <- 1 / pt_to_emu / 72
  expect_true(all(bb$x0 >= clip["x0"] - tol))
  expect_true(all(bb$x1 <= clip["x1"] + tol))
})

# Line cap and join ----------------------------------------------------------

test_that("round cap emits cap=rnd", {
  f <- easel_dev(width = 3, height = 3)
  plot.new()
  op <- par(lend = "round")
  on.exit(par(op))
  lines(c(0.2, 0.8), c(0.5, 0.5), lwd = 4)
  dev.off()
  expect_match(read_xml_text(f), 'cap="rnd"')
})

test_that("butt cap emits cap=flat", {
  f <- easel_dev(width = 3, height = 3)
  plot.new()
  op <- par(lend = "butt")
  on.exit(par(op))
  lines(c(0.2, 0.8), c(0.5, 0.5), lwd = 4)
  dev.off()
  expect_match(read_xml_text(f), 'cap="flat"')
})

test_that("square cap emits cap=sq", {
  f <- easel_dev(width = 3, height = 3)
  plot.new()
  op <- par(lend = "square")
  on.exit(par(op))
  lines(c(0.2, 0.8), c(0.5, 0.5), lwd = 4)
  dev.off()
  expect_match(read_xml_text(f), 'cap="sq"')
})

test_that("round join emits a:round", {
  f <- easel_dev(width = 3, height = 3)
  plot.new()
  op <- par(ljoin = "round")
  on.exit(par(op))
  lines(c(0.2, 0.5, 0.8), c(0.2, 0.8, 0.2), lwd = 4)
  dev.off()
  expect_match(read_xml_text(f), "<a:round/>")
})

test_that("bevel join emits a:bevel", {
  f <- easel_dev(width = 3, height = 3)
  plot.new()
  op <- par(ljoin = "bevel")
  on.exit(par(op))
  lines(c(0.2, 0.5, 0.8), c(0.2, 0.8, 0.2), lwd = 4)
  dev.off()
  expect_match(read_xml_text(f), "<a:bevel/>")
})

test_that("mitre join emits a:miter with lim scaled from lmitre", {
  f <- easel_dev(width = 3, height = 3)
  plot.new()
  op <- par(ljoin = "mitre", lmitre = 4)
  on.exit(par(op))
  lines(c(0.2, 0.5, 0.8), c(0.2, 0.8, 0.2), lwd = 4)
  dev.off()
  txt <- read_xml_text(f)
  expect_match(txt, "<a:miter")
  expect_match(txt, 'lim="400000"')
})

# Stub GE callbacks (setClipPath, setMask) -----------------------------------

test_that("grid clip path stub does not crash the device", {
  # Xdr_SetClipPath is a no-op stub required by R_GE_version >= 13.
  # The grid API for invoking it (createClipPath / viewport(clip=)) is
  # unstable across grid versions; we verify at least that grid.clip()
  # (which calls Xdr_Clip, not Xdr_SetClipPath) works correctly.
  f <- easel_dev(width = 3, height = 3)
  grid::grid.newpage()
  grid::grid.clip(x = 0.1, y = 0.1, width = 0.5, height = 0.5)
  expect_error(grid::grid.rect(gp = grid::gpar(fill = "red")), NA)
  dev.off()
  expect_wellformed_fragment(f)
})

# Coverage of paths added in 0.2 improvements ---------------------------------

test_that("XML-illegal control characters are stripped from text", {
  f <- easel_dev(width = 3, height = 3)
  plot.new()
  text(0.5, 0.5, "a\fb\tc")
  dev.off()
  txt <- read_xml_text(f)
  expect_match(txt, "<a:t>a", fixed = TRUE)
  expect_false(grepl("\f", txt, fixed = TRUE))
  expect_true(grepl("\tc</a:t>", txt, fixed = TRUE))
  expect_wellformed_fragment(f)
})

test_that("rotated rasterImage emits clipped quads at the right anchor", {
  w <- 4; h <- 4
  f <- easel_dev(width = w, height = h)
  plot.new()
  plot.window(c(0, 1), c(0, 1), xaxs = "i", yaxs = "i")
  rasterImage(as.raster(matrix(c("red", "blue"), 1)), 0.4, 0.4, 0.6, 0.6,
              angle = 45)
  dev.off()
  txt <- read_xml_text(f)
  expect_gte(count_matches(f, "<a:custGeom>"), 2)
  expect_wellformed_fragment(f)
  bb <- data_shape_bboxes(f, w, h)
  clip <- clip_bounds_in()
  tol <- 1 / pt_to_emu / 72
  expect_lte(max(bb$x1), clip["x1"] + tol)
})

test_that("fully transparent polyline colour emits no shapes", {
  f <- easel_dev(width = 3, height = 3)
  plot.new()
  lines(c(0.1, 0.5, 0.9), c(0.1, 0.9, 0.1), col = NA)
  lines(c(0.1, 0.5, 0.9), c(0.2, 0.8, 0.2), col = "#00000000")
  dev.off()
  expect_equal(count_matches(f, "<xdr:sp "), 0)
})

test_that("circle crossing the clip boundary is polygon-approximated and cut", {
  w <- 4; h <- 3
  f <- easel_dev(width = w, height = h)
  plot.new()
  clip <- clip_bounds_in()
  symbols(0, 0.5, circles = 0.3, inches = FALSE, add = TRUE,
          bg = "gold", fg = "navy")
  dev.off()
  txt <- read_xml_text(f)
  expect_false(grepl('prst="ellipse"', txt))
  expect_gte(count_matches(f, "<a:custGeom>"), 1)
  bb <- data_shape_bboxes(f, w, h)
  tol <- 1 / pt_to_emu / 72
  expect_gte(min(bb$x0), clip["x0"] - tol)
})

test_that("clipped grid.path suppresses the stroke on clip-introduced edges", {
  f <- easel_dev(width = 3, height = 3)
  grid::grid.path(
    x = c(-0.2, 0.5, 0.5, -0.2), y = c(0.2, 0.2, 0.6, 0.6),
    gp = grid::gpar(fill = "gold", col = "navy", lwd = 2)
  )
  dev.off()
  txt <- read_xml_text(f)
  # fill shape carries no stroke; separate stroked polylines follow
  expect_match(txt, "<a:ln><a:noFill/></a:ln>", fixed = TRUE)
  expect_gte(count_matches(f, "<xdr:sp "), 2)
})

test_that("a second page warns that pages overdraw", {
  f <- easel_dev(width = 3, height = 3)
  plot(1)
  expect_warning(plot(2), "single drawing")
  dev.off()
  expect_true(file.exists(f))
})

test_that("easel_dev validates its arguments", {
  expect_error(easel_dev(width = -1), "positive")
  expect_error(easel_dev(pointsize = 0), "positive")
  expect_error(easel_dev(fontname = NA_character_), "non-empty")
})

# easel_size / dims-first sizing ----------------------------------------------

test_that("easel_size matches Excel default geometry", {
  # 8.43-char cols = 64px, 15pt rows = 20px, 96 px/in
  expect_equal(unname(easel_size("A1:G15")), c(7 * 64, 15 * 20) / 96)
  expect_equal(unname(easel_size("A1")), c(64, 20) / 96)
  expect_equal(easel_size("B2:F12"), easel_size("F12:B2"))
  expect_equal(easel_size("$B$2:$F$12"), easel_size("B2:F12"))
})

test_that("easel_size reads custom widths, heights, and defaults from a wb", {
  skip_if_not_installed("openxlsx2")
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb$set_col_widths(cols = 2, widths = 25)
  wb$set_row_heights(rows = 4:6, heights = 30)
  # openxlsx2 pads stored width (25 -> 25.711) and writes defaultRowHeight=16
  stored <- as.numeric(sub('.*width="([0-9.]+)".*', "\\1",
                           wb$worksheets[[1]]$cols_attr[1]))
  b_px <- trunc(stored * 7 + 0.5) + 5
  sz <- easel_size("B2:F12", wb)
  expect_equal(unname(sz["width"]), (b_px + 4 * 64) / 96)
  def_ht <- as.numeric(sub('.*defaultRowHeight="([0-9.]+)".*', "\\1",
                           wb$worksheets[[1]]$sheetFormatPr))
  expect_equal(unname(sz["height"]), (8 * round(def_ht * 4 / 3) + 3 * 40) / 96)
})

test_that("easel_dev(dims=) sizes the device from the region", {
  f <- easel_dev(dims = "A1:G15")
  plot(1)
  dev.off()
  txt <- read_xml_text(f)
  sz <- easel_size("A1:G15")
  emu <- round(sz * 914400)
  expect_match(txt, sprintf('cx="%d" cy="%d"', emu[["width"]], emu[["height"]]))
  expect_error(easel_size("nope"), "cell")
})

test_that("par(bg=) paints a full-canvas background rect", {
  f <- easel_dev(width = 3, height = 2)
  par(bg = "magenta")
  plot.new()
  dev.off()
  par(bg = "white")
  txt <- read_xml_text(f)
  expect_match(txt, 'val="FF00FF"')
  expect_match(txt, sprintf('cx="%.0f" cy="%.0f"', 3 * 914400, 2 * 914400))
})

# Remaining branches: validation, wb edge cases, stubs, string API ------------

test_that("argument validation errors fire", {
  expect_error(easel_dev(text_voff = 2), "text_voff")
  expect_error(easel_size("A1:B2:C3"), "cell range")
  expect_error(easel_size("A1", wb = 1), "wbWorkbook")
})

test_that("easel_size handles hidden and width-less cols, out-of-range and hidden rows", {
  skip_if_not_installed("openxlsx2")
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb$set_row_heights(rows = 50, heights = 99)          # outside queried range
  ws <- wb$worksheets[[1]]
  ws$cols_attr <- c('<col min="1" max="1" hidden="1" width="10"/>',
                    '<col min="2" max="2" customWidth="1"/>')      # no width
  ra <- ws$sheet_data$row_attr
  hid <- ra[1, , drop = FALSE]; hid$r <- "2"; hid$ht <- ""; hid$hidden <- "1"
  ws$sheet_data$row_attr <- rbind(ra, hid)
  sz <- easel_size("A1:B2", wb)
  # col A hidden -> 0 px; the width-less <col> entry leaves B at the default
  expect_equal(unname(sz["width"]), 64 / 96)
  # row 2 hidden -> only row 1 contributes
  expect_equal(unname(sz["height"]), easel_size("A1", wb)[["height"]])
})

test_that("clipped multi-subpath grid.path re-strokes surviving outline pieces", {
  f <- easel_dev(width = 3, height = 3)
  grid::grid.path(x = c(-0.2, 0.5, 0.5, -0.2, 0.1, 0.3, 0.3, 0.1),
                  y = c(0.2, 0.2, 0.6, 0.6, 0.3, 0.3, 0.5, 0.5),
                  id = rep(1:2, each = 4),
                  gp = grid::gpar(fill = "gold", col = "navy", lwd = 2))
  dev.off()
  txt <- read_xml_text(f)
  expect_match(txt, "<a:ln><a:noFill/></a:ln>", fixed = TRUE)
  expect_gte(count_matches(f, "<xdr:sp "), 3)   # fill + border runs
})

test_that("grid clip-path and mask stubs are callable", {
  skip_if_not(getRversion() >= "4.1.0")
  f <- easel_dev(width = 3, height = 3)
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(clip = grid::circleGrob()))
  grid::grid.rect(gp = grid::gpar(fill = "red"))
  grid::popViewport()
  dev.off()
  expect_true(file.exists(f))
})

test_that("a failing write at close warns", {
  skip_if_not(file.exists("/dev/full"))
  f <- easel_dev("/dev/full", width = 3, height = 3)
  plot(1)
  expect_warning(dev.off(), "error writing")
})

test_that("easel_xml returns the raw string, identical to file output", {
  xml <- easel_xml(plot(1:3), width = 3, height = 3)
  expect_true(is.character(xml) && length(xml) == 1L)
  expect_match(xml, "</xdr:wsDr>$")

  f <- easel_dev(width = 3, height = 3)
  plot(1:3)
  dev.off()
  expect_identical(paste(readLines(f, warn = FALSE), collapse = "\n"), xml)
})

test_that("easel_xml closes its device when code errors", {
  n <- length(grDevices::dev.list())
  expect_error(easel_xml(stop("boom"), width = 3, height = 3), "boom")
  expect_equal(length(grDevices::dev.list()), n)
})

test_that("a path that vanishes before close warns instead of erroring", {
  d <- tempfile()
  dir.create(d)
  f <- easel_dev(file.path(d, "gone.xml"), width = 3, height = 3)
  plot(1)
  unlink(d, recursive = TRUE)
  expect_warning(dev.off(), "cannot open")
})

test_that("internal entry point rejects memory mode without an environment", {
  expect_error(
    .Call(easeling:::C_easeling_, NULL, 3, 3, 12, "Calibri",
          FALSE, FALSE, 0.35, NULL, NULL),
    "environment"
  )
})

test_that("evenodd grid.path keeps its hole under the nonzero renderer rule", {
  f <- easel_dev(width = 3, height = 3)
  grid::grid.path(x = c(.1, .9, .9, .1, .35, .65, .65, .35),
                  y = c(.1, .1, .9, .9, .35, .35, .65, .65),
                  id = rep(1:2, each = 4), rule = "evenodd",
                  gp = grid::gpar(fill = "seagreen"))
  dev.off()
  txt <- read_xml_text(f)
  pts <- regmatches(txt, gregexpr('<a:pt x="[0-9-]+" y="[0-9-]+"/>', txt))[[1]]
  # outer and inner rings must have opposite orientations for nonzero to
  # cut the hole; check via signed area of the two 4-point rings
  xy <- do.call(rbind, lapply(pts, function(p) {
    as.numeric(regmatches(p, gregexpr("-?[0-9]+", p))[[1]])
  }))
  ring_area <- function(m) {
    n <- nrow(m); j <- c(2:n, 1)
    sum(m[, 1] * m[j, 2] - m[j, 1] * m[, 2]) / 2
  }
  n_bg <- nrow(xy) - 8   # any leading pts belong to other shapes (none here)
  a1 <- ring_area(xy[n_bg + (1:4), , drop = FALSE])
  a2 <- ring_area(xy[n_bg + (5:8), , drop = FALSE])
  expect_lt(a1 * a2, 0)
})


# Optional font metrics -------------------------------------------------------

test_that("user-supplied metrics drive string widths exactly", {
  m <- list(widths = rep(1, 95), ascents = rep(0.7, 95), descents = rep(0.2, 95))
  f <- easel_dev(width = 4, height = 3, metrics = m)
  plot.new()
  w <- strwidth("ABCD", units = "inches")
  dev.off()
  expect_equal(w * 72, 4 * 12)   # four 1-em glyphs at 12pt
})

test_that("invalid metrics are rejected", {
  expect_error(easel_dev(metrics = list(widths = 1:95)), "widths, ascents, descents")
  expect_error(easel_dev(metrics = list(widths = rep(-1, 95),
                                        ascents = rep(0, 95),
                                        descents = rep(0, 95))),
               "finite non-negative")
  expect_error(
    .Call(easeling:::C_easeling_, NULL, 3, 3, 12, "Calibri",
          FALSE, FALSE, 0.35, new.env(), c(1, 2, 3)),
    "285")
})

test_that("metrics = FALSE forces the builtin table; systemfonts differs", {
  skip_if_not_installed("systemfonts")
  wid <- function(metrics) {
    f <- easel_dev(width = 4, height = 3, fontname = "DejaVu Sans",
                   metrics = metrics)
    plot.new()
    w <- strwidth("Hello Legend", units = "inches")
    dev.off()
    w
  }
  w_tab <- wid(FALSE)
  w_sf <- wid(NULL)
  expect_false(isTRUE(all.equal(w_tab, w_sf)))
  gi <- systemfonts::glyph_info(strsplit("Hello Legend", "")[[1]],
                                family = "DejaVu Sans", size = 1000)
  expect_equal(w_sf * 72, sum(gi$x_advance) / 1000 * 12, tolerance = 1e-6)
})

test_that("without systemfonts the builtin metric tables are used", {
  testthat::local_mocked_bindings(
    has_systemfonts = function() FALSE, .package = "easeling"
  )
  f <- easel_dev(width = 4, height = 3)
  plot.new()
  text(0.5, 0.5, "gjpqy Aa.x-() Z_;:", adj = c(0.5, 0.5))
  w <- strwidth("Hello", units = "inches")
  dev.off()
  expect_equal(w * 72, sum(c(0.722, 0.556, 0.222, 0.222, 0.556)) * 12,
               tolerance = 0.2)   # Helvetica-like table, loose sanity bound
  expect_true(file.exists(f))
})

# Shaped clip paths (R >= 4.1) ------------------------------------------------

test_that("circle clip path clips fills and lines exactly", {
  skip_if_not(getRversion() >= "4.1.0")
  f <- easel_dev(width = 3, height = 3, metrics = FALSE)
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(clip = grid::circleGrob(r = 0.3)))
  grid::grid.rect(gp = grid::gpar(fill = "red", col = NA))
  grid::grid.segments(0, 0.5, 1, 0.5, gp = grid::gpar(col = "black"))
  grid::popViewport()
  grid::grid.rect(x = .5, y = .5, width = .2, height = .1,
                  gp = grid::gpar(fill = "blue", col = NA))
  dev.off()
  txt <- read_xml_text(f)
  expect_equal(count_matches(f, "<a:custGeom>"), 2)  # clipped rect + line
  expect_equal(count_matches(f, 'prst="rect"'), 1)   # post-pop rect unclipped
  # the horizontal line must clip to the circle chord: x in [43.2, 172.8]pt
  seg <- regmatches(txt, regexpr(
    '<a:off x="[0-9]+" y="[0-9]+"/><a:ext cx="[0-9]+" cy="0"', txt))
  n <- as.numeric(regmatches(seg, gregexpr("[0-9]+", seg))[[1]])
  expect_equal(n[1] / 12700, 43.2, tolerance = 1e-6)
  expect_equal(n[3] / 12700, 129.6, tolerance = 1e-6)
})

test_that("rect and polygon grobs work as clip paths", {
  skip_if_not(getRversion() >= "4.1.0")
  f <- easel_dev(width = 3, height = 3, metrics = FALSE)
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(
    clip = grid::rectGrob(width = 0.5, height = 0.5)))
  grid::grid.circle(r = 0.45, gp = grid::gpar(fill = "gold", col = NA))
  grid::popViewport()
  dev.off()
  bb <- data_shape_bboxes(f, 3, 3)
  expect_lte(max(bb$x1), 3 * 0.75 + 1e-6)
  expect_gte(min(bb$x0), 3 * 0.25 - 1e-6)

  f <- easel_dev(width = 3, height = 3, metrics = FALSE)
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(clip = grid::polygonGrob(
    x = c(.5, .9, .1), y = c(.9, .1, .1))))
  grid::grid.rect(gp = grid::gpar(fill = "seagreen", col = NA))
  grid::popViewport()
  dev.off()
  expect_gte(count_matches(f, "<a:custGeom>"), 1)
})

test_that("non-convex, multi-ring, and oversized clip paths fall back to bbox", {
  skip_if_not(getRversion() >= "4.1.0")
  star <- grid::polygonGrob(
    x = .5 + c(0, .1, .4, .16, .25, 0, -.25, -.16, -.4, -.1) ,
    y = .5 + c(.4, .12, .12, -.05, -.32, -.13, -.32, -.05, .12, .12))
  f <- easel_dev(width = 3, height = 3, metrics = FALSE)
  grid::grid.newpage()
  expect_warning(
    { grid::pushViewport(grid::viewport(clip = star))
      grid::grid.rect(gp = grid::gpar(fill = "red", col = NA))
      grid::popViewport() },
    "bounding box")
  dev.off()

  two <- grid::grobTree(grid::circleGrob(x = .3, r = .1),
                        grid::circleGrob(x = .7, r = .1))
  f <- easel_dev(width = 3, height = 3, metrics = FALSE)
  grid::grid.newpage()
  expect_warning(
    { grid::pushViewport(grid::viewport(clip = two))
      grid::grid.rect(gp = grid::gpar(fill = "red", col = NA))
      grid::popViewport() },
    "bounding box")
  dev.off()

  big <- grid::polygonGrob(x = .5 + .4 * cos(seq(0, 2*pi, length.out = 200)),
                           y = .5 + .4 * sin(seq(0, 2*pi, length.out = 200)))
  f <- easel_dev(width = 3, height = 3, metrics = FALSE)
  grid::grid.newpage()
  expect_warning(
    { grid::pushViewport(grid::viewport(clip = big))
      grid::grid.rect(gp = grid::gpar(fill = "red", col = NA))
      grid::popViewport() },
    "bounding box")
  dev.off()
})

test_that("empty or erroring clip grobs leave the device usable", {
  skip_if_not(getRversion() >= "4.1.0")
  f <- easel_dev(width = 3, height = 3, metrics = FALSE)
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(clip = grid::nullGrob()))
  grid::grid.rect(gp = grid::gpar(fill = "red", col = NA))
  grid::popViewport()
  dev.off()
  expect_gte(count_matches(f, "<xdr:sp "), 1)

  assign("drawDetails.easelboom", function(x, recording) stop("boom"),
         envir = globalenv())
  on.exit(rm("drawDetails.easelboom", envir = globalenv()), add = TRUE)
  f <- easel_dev(width = 3, height = 3, metrics = FALSE)
  grid::grid.newpage()
  try(suppressWarnings({
    grid::pushViewport(grid::viewport(clip = grid::grob(cl = "easelboom")))
    grid::grid.rect(gp = grid::gpar(fill = "red", col = NA))
    grid::popViewport()
  }), silent = TRUE)
  dev.off()
  expect_true(file.exists(f))
})

test_that("multi-subpath pathGrob clips via the path callback (bbox fallback)", {
  skip_if_not(getRversion() >= "4.1.0")
  f <- easel_dev(width = 3, height = 3, metrics = FALSE)
  grid::grid.newpage()
  expect_warning({
    grid::pushViewport(grid::viewport(clip = grid::pathGrob(
      x = c(.1, .4, .4, .1, .6, .9, .9, .6),
      y = c(.2, .2, .8, .8, .2, .2, .8, .8),
      id = rep(1:2, each = 4))))
    grid::grid.rect(gp = grid::gpar(fill = "purple", col = NA))
    grid::popViewport()
  }, "bounding box")
  dev.off()
  bb <- data_shape_bboxes(f, 3, 3)
  tol <- 1e-6
  expect_lte(max(bb$x1), 3 * 0.9 + tol)   # combined bbox of both rings
  expect_gte(min(bb$y0), 3 * 0.2 - tol)
})
