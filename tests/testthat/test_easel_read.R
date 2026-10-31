test_that("easel_read recovers shapes and text from a drawing", {
  f <- easel_dev(width = 4, height = 3)
  plot(1:5, type = "b", main = "a & b")
  dev.off()

  d <- easel_read(f)
  expect_s3_class(d, "easel_drawing")
  expect_equal(d$width, 4 * 72)
  expect_equal(d$height, 3 * 72)

  types <- vapply(d$shapes, function(s) s$type, "")
  expect_true(all(c("path", "text") %in% types))

  labels <- unlist(lapply(d$shapes[types == "text"], function(s) s$paras[[1L]]$text))
  expect_true("a & b" %in% labels)
})

test_that("plot.easel_drawing redraws without error", {
  f <- easel_dev(width = 4, height = 3)
  plot(1:5, col = "red", lty = 2, type = "l")
  polygon(c(2, 3, 4), c(2, 4, 2), col = "#0000FF80")
  text(3, 3, "x", srt = 45)
  dev.off()

  pdf(NULL)
  on.exit(dev.off())
  expect_invisible(plot(easel_read(f)))
})

test_that("gradient fills are parsed and redrawn", {
  skip_if(getRversion() < "4.1.0")
  f <- easel_dev(width = 4, height = 3)
  grid::grid.newpage()
  grid::grid.rect(gp = grid::gpar(fill = grid::linearGradient(c("red", "blue"))))
  dev.off()

  d <- easel_read(f)
  fills <- Filter(is.list, lapply(d$shapes, function(s) s$fill))
  expect_length(fills, 1L)
  expect_equal(fills[[1L]]$cols, c("#FF0000", "#0000FF"))

  pdf(NULL)
  on.exit(dev.off())
  expect_invisible(plot(d))
})

test_that("easel_read handles a group with an offset child space and rotation", {
  xml <- paste0(
    "<xdr:wsDr xmlns:xdr=\"x\" xmlns:a=\"y\"><xdr:twoCellAnchor><xdr:grpSp>",
    "<xdr:grpSpPr><a:xfrm><a:off x=\"1000000\" y=\"2000000\"/><a:ext cx=\"2540000\" cy=\"1270000\"/>",
    "<a:chOff x=\"1000000\" y=\"2000000\"/><a:chExt cx=\"2540000\" cy=\"1270000\"/></a:xfrm></xdr:grpSpPr>",
    "<xdr:sp><xdr:spPr><a:xfrm><a:off x=\"1000000\" y=\"2000000\"/><a:ext cx=\"1270000\" cy=\"635000\"/></a:xfrm>",
    "<a:prstGeom prst=\"rect\"><a:avLst></a:avLst></a:prstGeom><a:solidFill><a:srgbClr val=\"ff0000\"/></a:solidFill>",
    "<a:ln w=\"12700\"><a:solidFill><a:srgbClr val=\"0000ff\"/></a:solidFill><a:prstDash val=\"dash\"/></a:ln></xdr:spPr></xdr:sp>",
    "<xdr:sp><xdr:spPr><a:xfrm rot=\"5400000\"><a:off x=\"2270000\" y=\"2635000\"/><a:ext cx=\"1270000\" cy=\"635000\"/></a:xfrm>",
    "<a:prstGeom prst=\"rect\"><a:avLst></a:avLst></a:prstGeom><a:noFill/><a:ln w=\"0\"><a:noFill/></a:ln></xdr:spPr>",
    "<xdr:txBody><a:bodyPr wrap=\"none\" lIns=\"0\" rIns=\"0\" tIns=\"0\" bIns=\"0\" anchor=\"ctr\"/>",
    "<a:p><a:pPr algn=\"ctr\"/><a:r><a:rPr sz=\"1000\" b=\"1\"><a:solidFill><a:srgbClr val=\"000000\"/></a:solidFill></a:rPr>",
    "<a:t>a &amp; b</a:t></a:r></a:p></xdr:txBody></xdr:sp>",
    "</xdr:grpSp></xdr:twoCellAnchor></xdr:wsDr>"
  )
  f <- tempfile(fileext = ".xml")
  writeLines(xml, f)
  on.exit(unlink(f))

  d <- easel_read(f)
  expect_equal(d$width, 200)
  expect_equal(d$height, 100)
  expect_length(d$shapes, 2L)

  rect <- d$shapes[[1L]]
  expect_equal(range(rect$rings[[1L]]$x), c(0, 100))
  expect_equal(range(rect$rings[[1L]]$y), c(0, 50))
  expect_equal(rect$fill, "#FF0000")
  expect_equal(rect$col, "#0000FF")
  expect_equal(rect$lty, "44")

  txt <- d$shapes[[2L]]
  expect_equal(txt$type, "text")
  expect_equal(txt$rot, 90)
  expect_equal(txt$paras[[1L]]$text, "a & b")
  expect_equal(txt$paras[[1L]]$font, 2L)
})

test_that("easel_read reads a drawing from a workbook", {
  skip_if_not_installed("openxlsx2")

  f <- easel_dev(width = 4, height = 3)
  plot(1:5, type = "b")
  dev.off()

  xlsx <- tempfile(fileext = ".xlsx")
  on.exit(unlink(xlsx))
  wb <- openxlsx2::wb_workbook()
  wb$add_worksheet("first")
  wb$add_worksheet("second")$add_drawing(xml = f, dims = "B2")
  openxlsx2::wb_save(wb, xlsx)

  direct <- easel_read(f)
  from_wb <- easel_read(xlsx, sheet = "second")
  expect_equal(from_wb$width, direct$width)
  expect_length(from_wb$shapes, length(direct$shapes))
  expect_error(easel_read(xlsx, sheet = 1L), "no drawing")
  expect_error(easel_read(xlsx, sheet = "second", n = 2L), "no drawing number 2")
})

test_that("easel_read picks the requested anchor", {
  one <- function(label) {
    paste0(
      "<xdr:oneCellAnchor><xdr:sp><xdr:spPr><a:xfrm><a:off x=\"0\" y=\"0\"/><a:ext cx=\"127000\" cy=\"127000\"/></a:xfrm>",
      "<a:prstGeom prst=\"rect\"/><a:noFill/><a:ln><a:solidFill><a:srgbClr val=\"", label, "\"/></a:solidFill></a:ln>",
      "</xdr:spPr></xdr:sp></xdr:oneCellAnchor>"
    )
  }
  f <- tempfile(fileext = ".xml")
  writeLines(paste0("<xdr:wsDr xmlns:xdr=\"x\" xmlns:a=\"y\">", one("FF0000"), one("00FF00"), "</xdr:wsDr>"), f)
  on.exit(unlink(f))

  expect_equal(easel_read(f, n = 1L)$shapes[[1L]]$col, "#FF0000")
  expect_equal(easel_read(f, n = 2L)$shapes[[1L]]$col, "#00FF00")
  expect_error(easel_read(f, n = 3L), "no drawing number 3")
})

test_that("easel_read uses the anchor when a shape has no transform, and reads presets and styles", {
  xml <- paste0(
    "<xdr:wsDr xmlns:xdr=\"x\" xmlns:a=\"y\"><xdr:oneCellAnchor><xdr:from><xdr:col>0</xdr:col><xdr:colOff>0</xdr:colOff>",
    "<xdr:row>0</xdr:row><xdr:rowOff>0</xdr:rowOff></xdr:from><xdr:ext cx=\"1270000\" cy=\"635000\"/>",
    "<xdr:sp><xdr:spPr><a:xfrm><a:off x=\"0\" y=\"0\"/><a:ext cx=\"0\" cy=\"0\"/></a:xfrm>",
    "<a:prstGeom prst=\"diamond\"><a:avLst/></a:prstGeom></xdr:spPr>",
    "<xdr:style><a:lnRef idx=\"2\"><a:srgbClr val=\"102030\"/></a:lnRef><a:fillRef idx=\"1\"><a:srgbClr val=\"405060\"/></a:fillRef>",
    "<a:effectRef idx=\"0\"/><a:fontRef idx=\"minor\"><a:srgbClr val=\"FFFFFF\"/></a:fontRef></xdr:style>",
    "<xdr:txBody><a:bodyPr/><a:p><a:r><a:rPr sz=\"1000\"/><a:t>x</a:t></a:r></a:p></xdr:txBody></xdr:sp>",
    "</xdr:oneCellAnchor></xdr:wsDr>"
  )
  f <- tempfile(fileext = ".xml")
  writeLines(xml, f)
  on.exit(unlink(f))

  d <- easel_read(f)
  expect_equal(c(d$width, d$height), c(100, 50))

  shape <- d$shapes[[1L]]
  expect_length(shape$rings[[1L]]$x, 4L)
  expect_equal(shape$fill, "#405060")
  expect_equal(shape$col, "#102030")
  expect_equal(d$shapes[[2L]]$paras[[1L]]$col, "#FFFFFF")
})

test_that("attributes match with any namespace prefix and colors resolve through the theme", {
  dom <- easeling:::ooxml_parse("<sheet name=\"a\" sheetId=\"1\" d3p1:id=\"rId1\"/>")
  expect_equal(easeling:::ooxml_attr(dom, 1L, "id"), "rId1")
  expect_equal(easeling:::ooxml_attr(dom, 1L, "sheetId"), "1")

  dom <- easeling:::ooxml_parse("<fill><schemeClr val=\"accent1\"><lumMod val=\"50000\"/></schemeClr></fill>")
  expect_equal(easeling:::ooxml_color(dom, 1L, c(accent1 = "FF0000")), "#800000")
  expect_true(is.na(easeling:::ooxml_color(dom, 1L)))
})

test_that("preset shapes produce finite geometry", {
  prsts <- c(
    "rect", "roundRect", "trapezoid", "hexagon", "octagon", "pentagon", "plus", "star5", "star32",
    "rightArrow", "upArrow", "leftRightArrow", "bentArrow", "uturnArrow", "curvedRightArrow",
    "donut", "frame", "bevel", "cube", "can", "heart", "moon", "sun", "pie", "blockArc",
    "wedgeRectCallout", "wedgeRoundRectCallout", "cloud", "wave", "noSmoking", "bracketPair"
  )
  for (p in prsts) {
    rings <- easeling:::ooxml_preset(p, 1000, 600, numeric())
    expect_true(length(rings) >= 1L, info = p)
    for (r in rings) {
      expect_true(all(is.finite(r$x)) && all(is.finite(r$y)), info = p)
    }
  }
  expect_null(easeling:::ooxml_preset("notAShape", 1000, 600, numeric()))
})

test_that("embedded pictures are read and drawn", {
  skip_if_not_installed("openxlsx2")
  skip_if_not_installed("png")
  img <- array(runif(40 * 60 * 3), c(40, 60, 3))
  f <- tempfile(fileext = ".png")
  png::writePNG(img, f)
  wb <- openxlsx2::wb_workbook()$add_worksheet()$add_image(
    dims = "B2", file = f, width = 3, height = 2, units = "cm"
  )
  xl <- tempfile(fileext = ".xlsx")
  wb$save(xl)
  on.exit(unlink(c(f, xl)))

  d <- easel_read(xl)
  expect_equal(d$shapes[[1L]]$type, "image")
  expect_equal(dim(d$shapes[[1L]]$img), c(40L, 60L, 3L))
  expect_equal(c(d$width, d$height), c(3, 2) / 2.54 * 72, tolerance = 1e-6)

  out <- tempfile(fileext = ".png")
  grDevices::png(out, 300, 200)
  expect_no_error(plot(d))
  grDevices::dev.off()
  unlink(out)
})

test_that("charts in a workbook are found and drawn", {
  skip_if_not_installed("openxlsx2")
  skip_if_not_installed("encharter")
  wb <- openxlsx2::wb_workbook()$add_worksheet("Data")$add_data(x = data.frame(
    Month = month.abb[1:6], Sales = c(120, 135, 128, 160, 175, 190)
  ))
  chart <- encharter::ec("line")$add_series(
    name = "Data!$B$1", label = "Data!$A$2:$A$7", data = "Data!$B$2:$B$7"
  )
  wb$add_encharter(dims = "D2:L18", graph = chart)
  xl <- tempfile(fileext = ".xlsx")
  wb$save(xl)
  on.exit(unlink(xl))

  d <- easel_read(xl)
  expect_equal(d$shapes[[1L]]$type, "chart")
  expect_equal(d$shapes[[1L]]$row, 1L)

  grDevices::png(tempfile(fileext = ".png"), 400, 300)
  expect_no_warning(plot(d))
  grDevices::dev.off()
})

test_that("notes and threaded comments are read as boxes", {
  skip_if_not_installed("openxlsx2")
  wb <- openxlsx2::wb_workbook()$add_worksheet("S")
  wb$add_comment(dims = "B2", comment = openxlsx2::wb_comment(text = "A note", author = "Ann"))
  wb$add_person(name = "Bob")
  pid <- openxlsx2::wb_get_person(wb)$id[1L]
  wb$add_thread(dims = "D4", comment = "First", person_id = pid)
  xl <- tempfile(fileext = ".xlsx")
  wb$save(xl)
  on.exit(unlink(xl))

  note <- easel_read(xl, what = "comments", n = 1L)
  expect_equal(note$shapes[[1L]]$fill, "#FFFFE1")
  txt <- unlist(lapply(note$shapes[[2L]]$paras, `[[`, "text"))
  expect_true(any(grepl("A note", txt)))

  thread <- easel_read(xl, what = "comments", n = 2L)
  expect_equal(thread$shapes[[1L]]$fill, "#FFFFFF")
  txt <- unlist(lapply(thread$shapes, function(s) lapply(s$paras, `[[`, "text")))
  expect_true(all(c("Bob", "First") %in% txt))
  expect_error(easel_read(xl, what = "comments", n = 3L), "no comment number 3")

  grDevices::png(tempfile(fileext = ".png"), 300, 200)
  expect_no_error(plot(thread))
  grDevices::dev.off()
})

test_that("a pivot slicer is drawn with its selection", {
  skip_if(Sys.which("zip") == "", "no zip program")
  d <- file.path(tempfile("slicer"))
  for (sub in c("xl/_rels", "xl/worksheets/_rels", "xl/drawings/_rels", "xl/slicers", "xl/slicerCaches", "xl/pivotCache")) {
    dir.create(file.path(d, sub), recursive = TRUE)
  }
  w <- function(path, txt) writeLines(txt, file.path(d, path))
  rel <- function(id, type, target) {
    sprintf("<Relationship Id=\"%s\" Type=\"http://x/%s\" Target=\"%s\"/>", id, type, target)
  }
  rels <- function(...) {
    paste0("<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">", ..., "</Relationships>")
  }
  w("xl/workbook.xml", paste0(
    "<workbook xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\" ",
    "xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\">",
    "<sheets><sheet name=\"S\" sheetId=\"1\" r:id=\"rId1\"/></sheets></workbook>"
  ))
  w("xl/_rels/workbook.xml.rels", rels(rel("rId1", "worksheet", "worksheets/sheet1.xml")))
  w("xl/worksheets/sheet1.xml", "<worksheet/>")
  w("xl/worksheets/_rels/sheet1.xml.rels", rels(rel("rId1", "drawing", "../drawings/drawing1.xml")))
  w("xl/drawings/drawing1.xml", paste0(
    "<xdr:wsDr xmlns:xdr=\"x\" xmlns:a=\"y\"><xdr:oneCellAnchor><xdr:from><xdr:col>0</xdr:col>",
    "<xdr:colOff>0</xdr:colOff><xdr:row>0</xdr:row><xdr:rowOff>0</xdr:rowOff></xdr:from>",
    "<xdr:ext cx=\"1828800\" cy=\"2524125\"/><xdr:graphicFrame><xdr:xfrm><a:off x=\"0\" y=\"0\"/>",
    "<a:ext cx=\"0\" cy=\"0\"/></xdr:xfrm><a:graphic><a:graphicData uri=\"slicer\">",
    "<sle:slicer name=\"Fruit\"/></a:graphicData></a:graphic></xdr:graphicFrame>",
    "</xdr:oneCellAnchor></xdr:wsDr>"
  ))
  w("xl/slicers/slicer1.xml", "<slicers><slicer name=\"Fruit\" cache=\"Slicer_Fruit\" caption=\"Fruit\"/></slicers>")
  w("xl/slicerCaches/slicerCache1.xml", paste0(
    "<slicerCacheDefinition name=\"Slicer_Fruit\" sourceName=\"Fruit\"><data><tabular pivotCacheId=\"7\">",
    "<items count=\"2\"><i x=\"1\" s=\"1\"/><i x=\"0\"/></items></tabular></data></slicerCacheDefinition>"
  ))
  w("xl/pivotCache/pivotCacheDefinition1.xml", paste0(
    "<pivotCacheDefinition><cacheFields><cacheField name=\"Fruit\"><sharedItems>",
    "<s v=\"apple\"/><s v=\"pear\"/></sharedItems></cacheField></cacheFields>",
    "<extLst><ext><x14:pivotCacheDefinition pivotCacheId=\"7\"/></ext></extLst></pivotCacheDefinition>"
  ))
  w("xl/drawings/_rels/drawing1.xml.rels", rels())
  xl <- tempfile(fileext = ".xlsx")
  old <- setwd(d)
  utils::zip(xl, list.files(".", recursive = TRUE), flags = "-r9Xq")
  setwd(old)
  on.exit(unlink(c(d, xl), recursive = TRUE))

  dr <- easel_read(xl)
  fills <- vapply(dr$shapes, function(s) if (s$type == "path") s$fill else NA_character_, "")
  labels <- unlist(lapply(dr$shapes, function(s) if (s$type == "text") s$paras[[1L]]$text))
  expect_equal(labels, c("Fruit", "pear", "apple"))
  expect_false(fills[3L] == fills[5L])
  expect_equal(fills[5L], "#F2F2F2")
})

test_that("equations in drawing text are read and plotted", {
  xml <- paste0(
    "<xdr:wsDr xmlns:xdr=\"x\" xmlns:a=\"y\" xmlns:mc=\"z\" xmlns:a14=\"w\" xmlns:m=\"v\">",
    "<xdr:twoCellAnchor><xdr:from><xdr:col>0</xdr:col><xdr:colOff>0</xdr:colOff><xdr:row>0</xdr:row><xdr:rowOff>0</xdr:rowOff></xdr:from>",
    "<xdr:to><xdr:col>3</xdr:col><xdr:colOff>0</xdr:colOff><xdr:row>3</xdr:row><xdr:rowOff>0</xdr:rowOff></xdr:to>",
    "<xdr:sp><xdr:spPr><a:xfrm><a:off x=\"0\" y=\"0\"/><a:ext cx=\"2000000\" cy=\"1000000\"/></a:xfrm>",
    "<a:prstGeom prst=\"rect\"><a:avLst/></a:prstGeom></xdr:spPr><xdr:txBody><a:bodyPr/>",
    "<a:p><mc:AlternateContent><mc:Choice Requires=\"a14\"><a14:m><m:oMathPara><m:oMath>",
    "<m:f><m:num><m:r><m:t>1</m:t></m:r></m:num><m:den><m:sSup><m:e><m:r><m:t>x</m:t></m:r></m:e><m:sup><m:r><m:t>2</m:t></m:r></m:sup></m:sSup></m:den></m:f>",
    "</m:oMath></m:oMathPara></a14:m></mc:Choice><mc:Fallback/></mc:AlternateContent></a:p>",
    "</xdr:txBody></xdr:sp></xdr:twoCellAnchor></xdr:wsDr>"
  )
  f <- tempfile(fileext = ".xml")
  writeLines(xml, f)
  on.exit(unlink(f))
  d <- easel_read(f)
  tx <- Filter(function(s) identical(s$type, "text"), d$shapes)
  expect_length(tx, 1L)
  expect_false(is.null(tx[[1L]]$paras[[1L]]$expr))
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_no_error(plot(d))
})

test_that("every preset yields geometry and a text rectangle for varied adjustments", {
  pn <- preset_ids
  tn <- textrect_ids
  expect_gt(length(pn), 50L)
  adjs <- list(
    list(),
    stats::setNames(as.list(rep(0, 8L)), paste0("adj", 1:8)),
    stats::setNames(as.list(rep(100000, 8L)), paste0("adj", 1:8)),
    stats::setNames(as.list(rep(-60000, 8L)), paste0("adj", 1:8)),
    stats::setNames(as.list(rep(40000, 8L)), paste0("adj", 1:8)),
    list(adj = 0, adj1 = 90000, adj2 = -20000, adj3 = 5000, adj4 = 70000),
    list(adj = 50000, adj1 = -90000, adj2 = 120000, adj3 = 60000, adj4 = 10000),
    list(adj1 = -90000, adj2 = -30000)
  )
  ok <- logical()
  for (nm in unique(c(pn, tn))) {
    for (ad in adjs) {
      for (dims in list(c(100, 60), c(60, 100), c(80, 80))) {
        if (nm %in% pn) {
          g <- easeling:::ooxml_preset(nm, dims[1L], dims[2L], ad)
          ok <- c(ok, is.list(g))
        }
        if (nm %in% tn) {
          tr <- easeling:::ooxml_textrect(nm, dims[1L], dims[2L], ad)
          ok <- c(ok, length(tr) == 4L)
        }
      }
    }
  }
  expect_true(all(ok))
  expect_null(easeling:::ooxml_preset("noSuchShape", 10, 10, list()))
  expect_equal(easeling:::ooxml_textrect("noSuchShape", 10, 10, list()), c(0, 0, 10, 10))
})
