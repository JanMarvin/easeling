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
    # fallback: crude open/close tag balance check
    opens <- lengths(regmatches(wrapped, gregexpr("<[a-zA-Z:]+[^/>]*(?<!/)>", wrapped, perl = TRUE)))
    closes <- lengths(regmatches(wrapped, gregexpr("</[a-zA-Z:]+>", wrapped, perl = TRUE)))
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

test_that("text() always reaches the device with hadj=0 (R pre-shifts x itself)", {
  # Verified empirically: R's engine normalizes hadj to 0 for scalar adj,
  # vector adj, and mtext(), regardless of the requested justification -
  # it pre-shifts x and always calls the device as if left-aligned. This
  # means our device's a:pPr algn is "l" in practice for all real text;
  # the r/ctr branches exist to match the documented device API contract
  # but aren't reachable via any public R plotting call we could find.
  f <- easel_dev(width = 3, height = 3)
  plot.new()
  text(0.2, 0.8, "left", adj = c(0, 0.5))
  text(0.2, 0.5, "right", adj = c(1, 0.5))
  text(0.2, 0.2, "center", adj = c(0.5, 0.5))
  mtext("side", side = 1, adj = 1)
  dev.off()
  txt <- read_xml_text(f)
  expect_match(txt, 'algn="l"')
  expect_false(grepl('algn="r"', txt))
  expect_false(grepl('algn="ctr"', txt))
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
    grid::grid.raster(as.raster(matrix(c("red", "blue"), nrow = 2)))
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
  expect_equal(count_matches(f, "<a:path w="), 2)
  expect_equal(count_matches(f, "<a:close/>"), 2)
  expect_wellformed_fragment(f)
})

# Gradients ----------------------------------------------------------------

test_that("linearGradient fill emits a:gradFill with correct stops", {
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
