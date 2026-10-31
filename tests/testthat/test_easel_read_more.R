wsdr <- function(body, anchor = "twoCellAnchor", head = NULL) {
  if (is.null(head)) {
    head <- paste0(
      "<xdr:from><xdr:col>0</xdr:col><xdr:colOff>0</xdr:colOff><xdr:row>0</xdr:row><xdr:rowOff>0</xdr:rowOff></xdr:from>",
      "<xdr:to><xdr:col>4</xdr:col><xdr:colOff>0</xdr:colOff><xdr:row>8</xdr:row><xdr:rowOff>0</xdr:rowOff></xdr:to>"
    )
  }
  paste0(
    "<xdr:wsDr xmlns:xdr=\"x\" xmlns:a=\"y\" xmlns:mc=\"z\" xmlns:a14=\"w\" xmlns:m=\"v\">",
    "<xdr:", anchor, ">", head, body, "</xdr:", anchor, "></xdr:wsDr>"
  )
}

xsp <- function(x, y, cx, cy, geom, props = "", body = "", attrs = "", style = "") {
  paste0(
    "<xdr:sp><xdr:spPr><a:xfrm", attrs, "><a:off x=\"", x, "\" y=\"", y, "\"/><a:ext cx=\"", cx, "\" cy=\"", cy, "\"/></a:xfrm>",
    geom, props, "</xdr:spPr>", style, body, "</xdr:sp>"
  )
}

prst <- function(name, av = "") paste0("<a:prstGeom prst=\"", name, "\"><a:avLst>", av, "</a:avLst></a:prstGeom>")

read_xml <- function(xml, ...) {
  f <- tempfile(fileext = ".xml")
  writeLines(xml, f, useBytes = TRUE)
  on.exit(unlink(f))
  easel_read(f, ...)
}

draw_it <- function(d) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  plot(d)
}

test_that("custom paths, line styles, flips and fills are parsed", {
  cust <- paste0(
    "<a:custGeom><a:pathLst><a:path w=\"100\" h=\"100\">",
    "<a:moveTo><a:pt x=\"0\" y=\"0\"/></a:moveTo><a:lnTo><a:pt x=\"100\" y=\"0\"/></a:lnTo>",
    "<a:cubicBezTo><a:pt x=\"100\" y=\"30\"/><a:pt x=\"70\" y=\"60\"/><a:pt x=\"50\" y=\"100\"/></a:cubicBezTo>",
    "<a:quadBezTo><a:pt x=\"20\" y=\"80\"/><a:pt x=\"0\" y=\"100\"/></a:quadBezTo><a:close/>",
    "<a:moveTo><a:pt x=\"10\" y=\"10\"/></a:moveTo><a:lnTo><a:pt x=\"20\" y=\"20\"/></a:lnTo></a:path>",
    "<a:path><a:moveTo><a:pt x=\"1\" y=\"1\"/></a:moveTo></a:path>",
    "<a:path><a:lnTo><a:pt x=\"1\" y=\"1\"/></a:lnTo><a:cubicBezTo><a:pt x=\"1\" y=\"1\"/></a:cubicBezTo>",
    "<a:quadBezTo><a:pt x=\"1\" y=\"1\"/></a:quadBezTo><a:close/></a:path>",
    "</a:pathLst></a:custGeom>"
  )
  lnx <- function(inner, attrs = "") paste0("<a:ln", attrs, ">", inner, "</a:ln>")
  sf <- "<a:solidFill><a:srgbClr val=\"336699\"/></a:solidFill>"
  radial <- function(ftr) {
    paste0(
      "<a:gradFill><a:gsLst><a:gs pos=\"0\"><a:srgbClr val=\"FF0000\"/></a:gs><a:gs pos=\"100000\"><a:srgbClr val=\"0000FF\"/></a:gs></a:gsLst>",
      "<a:path path=\"circle\">", ftr, "</a:path></a:gradFill>"
    )
  }
  body <- paste0(
    xsp(0, 0, 900000, 900000, cust, sf, attrs = " rot=\"900000\" flipH=\"1\" flipV=\"1\""),
    xsp(1000000, 0, 900000, 900000, prst("rect"), radial("<a:fillToRect l=\"30000\" t=\"40000\"/>"),
      body = "<xdr:txBody><a:bodyPr/><a:p><a:r><a:t>x</a:t></a:r></a:p></xdr:txBody>"),
    xsp(2000000, 0, 900000, 900000, prst("rect"), radial("<a:fillToRect/>")),
    xsp(0, 1000000, 900000, 900000, prst("rect"), lnx(paste0(sf, "<a:bevel/><a:prstDash val=\"dash\"/>"), " w=\"19050\" cap=\"flat\"")),
    xsp(1000000, 1000000, 900000, 900000, prst("rect"), lnx(paste0(sf, "<a:miter lim=\"800000\"/><a:prstDash val=\"bogus\"/>"), " cap=\"sq\"")),
    xsp(2000000, 1000000, 900000, 900000, prst("rect"), lnx("<a:miter/><a:custDash><a:ds d=\"200000\" sp=\"100000\"/></a:custDash><a:gradFill><a:gsLst><a:gs pos=\"0\"><a:srgbClr val=\"00FF00\"/></a:gs></a:gsLst></a:gradFill>")),
    xsp(0, 2000000, 900000, 900000, prst("noSuchShape"), sf),
    xsp(1000000, 2000000, 900000, 900000, prst("ellipse"), sf, attrs = " flipH=\"1\""),
    xsp(2000000, 2000000, 900000, 900000, prst("straightConnector1"), lnx(sf)),
    xsp(0, 3000000, 900000, 900000, prst("rect"), lnx("<a:noFill/>")),
    "<xdr:sp><xdr:spPr/></xdr:sp>",
    "<xdr:graphicFrame><xdr:xfrm><a:off x=\"0\" y=\"0\"/><a:ext cx=\"1\" cy=\"1\"/></xdr:xfrm><a:graphic><a:graphicData><c:chart id=\"rId1\"/></a:graphicData></a:graphic></xdr:graphicFrame>"
  )
  d <- read_xml(wsdr(body))
  types <- vapply(d$shapes, function(s) s$type, "")
  expect_true(all(c("path", "text") %in% types))
  paths <- Filter(function(s) s$type == "path", d$shapes)
  lends <- vapply(paths, function(s) s$lend, "")
  expect_true(all(c("butt", "square", "round") %in% lends))
  joins <- vapply(paths, function(s) s$ljoin, "")
  expect_true(all(c("bevel", "mitre") %in% joins))
  expect_no_error(draw_it(d))
})

test_that("pictures without a readable image get a frame and unsupported frames are skipped", {
  pic <- function(rid, xfrm = "") {
    paste0(
      "<xdr:pic><xdr:blipFill><a:blip embed=\"", rid, "\"/></xdr:blipFill><xdr:spPr>", xfrm, "</xdr:spPr></xdr:pic>"
    )
  }
  frame <- read_xml(wsdr(pic("rId9", "<a:xfrm><a:off x=\"0\" y=\"0\"/><a:ext cx=\"900\" cy=\"600\"/></a:xfrm>")))
  expect_equal(frame$shapes[[1L]]$fill, "#F2F2F2")
  expect_no_error(draw_it(frame))
  expect_error(read_xml(wsdr("<xdr:graphicFrame><xdr:xfrm/></xdr:graphicFrame>")), "no shapes")
  expect_error(
    read_xml(wsdr(xsp(0, 0, 0, 0, prst("rect"), "<a:solidFill><a:srgbClr val=\"000000\"/></a:solidFill>"), anchor = "oneCellAnchor",
      head = "<xdr:from><xdr:col>0</xdr:col></xdr:from>")),
    "no shapes"
  )
  expect_error(read_xml(wsdr(xsp(0, 0, 1, 1, prst("rect"))), n = 2), "no drawing number")
})

test_that("all presets are drawn", {
  pids <- preset_ids
  sf <- "<a:solidFill><a:srgbClr val=\"336699\"/></a:solidFill><a:ln><a:solidFill><a:srgbClr val=\"000000\"/></a:solidFill></a:ln>"
  body <- character()
  for (k in seq_along(pids)) {
    body <- c(body, xsp(((k - 1) %% 10) * 600000, ((k - 1) %/% 10) * 600000, 500000, 400000, prst(pids[k]), sf,
      body = "<xdr:txBody><a:bodyPr/><a:p><a:r><a:t>Some text that wraps</a:t></a:r></a:p></xdr:txBody>"))
  }
  d <- read_xml(wsdr(paste(body, collapse = "")))
  expect_gt(length(d$shapes), length(pids))
  expect_no_error(draw_it(d))
})

test_that("text wrapping handles long words, nowrap and oversized equations", {
  tx <- function(txt, wrap = "square", sz = 1100) {
    paste0(
      "<xdr:txBody><a:bodyPr wrap=\"", wrap, "\" lIns=\"0\" rIns=\"0\"/><a:p><a:r><a:rPr sz=\"", sz, "\"/><a:t>", txt, "</a:t></a:r></a:p></xdr:txBody>"
    )
  }
  math <- paste0(
    "<xdr:txBody><a:bodyPr/><a:p><mc:AlternateContent><mc:Choice Requires=\"a14\"><a14:m><m:oMathPara><m:oMathParaPr><m:jc m:val=\"right\"/></m:oMathParaPr><m:oMath>",
    "<m:r><m:t>abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz</m:t></m:r>",
    "</m:oMath></m:oMathPara></a14:m></mc:Choice></mc:AlternateContent></a:p></xdr:txBody>"
  )
  body <- paste0(
    xsp(0, 0, 400000, 400000, prst("rect"), "<a:solidFill><a:srgbClr val=\"FFFFFF\"/></a:solidFill>",
      body = tx("Supercalifragilisticexpialidocious and more words")),
    xsp(500000, 0, 400000, 400000, prst("rect"), "", body = tx("no wrap here at all", "none")),
    xsp(1000000, 0, 400000, 400000, prst("rect"), "", body = tx("")),
    xsp(1500000, 0, 400000, 400000, prst("rect"), "", body = math)
  )
  d <- read_xml(wsdr(body))
  expect_no_error(draw_it(d))
})

test_that("equation elements are translated and plotted", {
  r <- function(t) paste0("<m:r><m:t>", t, "</m:t></m:r>")
  e <- function(inner) paste0("<m:e>", inner, "</m:e>")
  ital <- intToUtf8(c(0x1D465, 0x1D466, 0x210E, 0x1D434))
  eqs <- c(
    paste0("<m:sSub>", e(r("a")), "<m:sub>", r("1"), "</m:sub></m:sSub>"),
    paste0("<m:sSubSup>", e(r("b")), "<m:sub>", r("i"), "</m:sub><m:sup>", r("2"), "</m:sup></m:sSubSup>"),
    paste0("<m:rad><m:radPr><m:degHide m:val=\"1\"/></m:radPr><m:deg/>", e(r("x")), "</m:rad>"),
    paste0("<m:rad><m:deg>", r("3"), "</m:deg>", e(r("y")), "</m:rad>"),
    paste0("<m:nary><m:naryPr><m:chr m:val=\"", intToUtf8(0x2211), "\"/></m:naryPr><m:sub>", r("i=1"), "</m:sub><m:sup>", r("n"), "</m:sup>", e(r("i")), "</m:nary>"),
    paste0("<m:nary><m:naryPr><m:chr m:val=\"", intToUtf8(0x220F), "\"/></m:naryPr><m:sub/><m:sup/>", e(r("k")), "</m:nary>"),
    paste0("<m:nary><m:naryPr/><m:sub>", r("0"), "</m:sub><m:sup/>", e(r("f")), "</m:nary>"),
    paste0("<m:d><m:dPr><m:begChr m:val=\"[\"/><m:endChr m:val=\"\"/><m:sepChr m:val=\";\"/></m:dPr>", e(r("a")), e(r("b")), "</m:d>"),
    paste0("<m:d>", e(r("a")), "</m:d>"),
    paste0("<m:func><m:fName>", r("sin"), "</m:fName>", e(r("x")), "</m:func>"),
    paste0("<m:acc><m:accPr><m:chr m:val=\"", intToUtf8(0x0304), "\"/></m:accPr>", e(r("x")), "</m:acc>"),
    paste0("<m:acc><m:accPr><m:chr m:val=\"", intToUtf8(0x0307), "\"/></m:accPr>", e(r("x")), "</m:acc>"),
    paste0("<m:acc><m:accPr><m:chr m:val=\"", intToUtf8(0x0303), "\"/></m:accPr>", e(r("x")), "</m:acc>"),
    paste0("<m:acc>", e(r("x")), "</m:acc>"),
    paste0("<m:bar>", e(r("x")), "</m:bar>"),
    paste0("<m:limLow>", e(r("lim")), "<m:lim>", r("n"), "</m:lim></m:limLow>"),
    paste0("<m:limUpp>", e(r("max")), "<m:lim>", r("n"), "</m:lim></m:limUpp>"),
    paste0("<m:f><m:num>", r("a &lt; b &amp; &quot;c&quot;"), "</m:num><m:den>", r(ital), "</m:den></m:f>"),
    "<m:f/>", "<m:r/>", r(""), r(paste0("ab", ital, "cd")),
    paste0("<m:box>", e(r("z")), "</m:box>"), "<m:sSup><m:e/><m:sup/></m:sSup>"
  )
  para <- function(inner, wrap = TRUE) {
    paste0(
      "<a:p><mc:AlternateContent><mc:Choice Requires=\"a14\"><a14:m>",
      if (wrap) "<m:oMathPara><m:oMathParaPr><m:jc m:val=\"left\"/></m:oMathParaPr>",
      "<m:oMath>", inner, "</m:oMath>", if (wrap) "</m:oMathPara>", "</a14:m></mc:Choice></mc:AlternateContent>",
      "<a:pPr><a:defRPr/></a:pPr></a:p>"
    )
  }
  body <- paste0(
    xsp(0, 0, 3000000, 600000, prst("rect"), "", body = paste0("<xdr:txBody><a:bodyPr/>", para(paste(eqs, collapse = "")), "</xdr:txBody>")),
    xsp(0, 700000, 3000000, 600000, prst("rect"), "", body = paste0("<xdr:txBody><a:bodyPr/>", para(eqs[1L], FALSE), "</xdr:txBody>")),
    xsp(0, 1400000, 3000000, 600000, prst("rect"), "", body = paste0(
      "<xdr:txBody><a:bodyPr/><a:p><mc:AlternateContent><mc:Choice Requires=\"a14\"><a14:m><m:oMathPara><m:oMath>", eqs[1L],
      "</m:oMath></m:oMathPara></a14:m></mc:Choice></mc:AlternateContent><a:endParaRPr><a:rPr/></a:endParaRPr></a:p></xdr:txBody>"
    ))
  )
  d <- read_xml(wsdr(body))
  tx <- Filter(function(s) identical(s$type, "text"), d$shapes)
  expect_length(tx, 3L)
  expect_true(all(vapply(tx, function(s) !is.null(s$paras[[1L]]$expr), NA)))
  expect_no_error(draw_it(d))
})

mkxlsx <- function(parts, ext = ".xlsx") {
  d <- tempfile()
  for (nm in names(parts)) {
    p <- file.path(d, nm)
    dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
    if (is.raw(parts[[nm]])) writeBin(parts[[nm]], p) else writeLines(parts[[nm]], p)
  }
  xl <- tempfile(fileext = ext)
  old <- setwd(d)
  on.exit(setwd(old))
  utils::zip(xl, list.files(".", recursive = TRUE), flags = "-r9Xq")
  unlink(d, recursive = TRUE)
  xl
}

rel_ns <- "http://schemas.openxmlformats.org/officeDocument/2006/relationships/"

rels_xml <- function(...) {
  paste0(
    "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">",
    paste(c(...), collapse = ""), "</Relationships>"
  )
}

rel1 <- function(id, type, target) {
  paste0("<Relationship Id=\"", id, "\" Type=\"", if (grepl("^http", type)) type else paste0(rel_ns, type), "\" Target=\"", target, "\"/>")
}

wbxml <- paste0(
  "<workbook xmlns=\"x\" xmlns:r=\"", sub("relationships/$", "relationships", rel_ns), "\">",
  "<sheets><sheet name=\"S\" sheetId=\"1\" r:id=\"rId1\"/><sheet name=\"T\" sheetId=\"2\" r:id=\"rId9\"/></sheets></workbook>"
)

base_parts <- function(sheet = "<worksheet/>", theme = NULL) {
  p <- list(
    "xl/workbook.xml" = wbxml,
    "xl/_rels/workbook.xml.rels" = rels_xml(rel1("rId1", "worksheet", "worksheets/sheet1.xml")),
    "xl/worksheets/sheet1.xml" = sheet
  )
  if (!is.null(theme)) p[["xl/theme/theme1.xml"]] <- theme
  p
}

test_that("workbook lookups fail clearly", {
  skip_if(Sys.which("zip") == "", "no zip program")
  theme <- paste0(
    "<a:theme xmlns:a=\"y\"><a:clrScheme><a:dk1><a:sysClr val=\"windowText\" lastClr=\"000000\"/></a:dk1>",
    "<a:lt1/><a:accent1><a:srgbClr val=\"ff0000\"/></a:accent1></a:clrScheme></a:theme>"
  )
  xl <- mkxlsx(base_parts(theme = theme))
  on.exit(unlink(xl))
  expect_error(easel_read(xl, sheet = "Nope"), "sheet not found")
  expect_error(easel_read(xl, sheet = 5L), "sheet not found")
  expect_error(easel_read(xl, sheet = 2L), "cannot resolve")
  expect_error(easel_read(xl), "no drawing")
  expect_error(easel_read(xl, what = "comments"), "no comments")
})

test_that("drawing lookups fail clearly", {
  skip_if(Sys.which("zip") == "", "no zip program")
  p <- base_parts()
  p[["xl/worksheets/_rels/sheet1.xml.rels"]] <- rels_xml(rel1("rId1", "hyperlink", "http://x"))
  xl <- mkxlsx(p)
  expect_error(easel_read(xl), "no drawing")
  expect_error(easel_read(xl, what = "comments"), "no comments")
  unlink(xl)
  p[["xl/worksheets/_rels/sheet1.xml.rels"]] <- rels_xml(rel1("rId1", "drawing", "../drawings/gone.xml"))
  xl <- mkxlsx(p)
  expect_error(easel_read(xl), "missing")
  unlink(xl)
})

test_that("comments of both kinds are read, ordered and sized", {
  skip_if(Sys.which("zip") == "", "no zip program")
  p <- base_parts()
  p[["xl/worksheets/_rels/sheet1.xml.rels"]] <- rels_xml(
    rel1("rId1", "comments", "../comments1.xml"),
    rel1("rId2", "vmlDrawing", "../drawings/vmlDrawing1.vml"),
    rel1("rId3", "http://schemas.microsoft.com/office/2017/10/relationships/threadedComment", "../threadedComments/threadedComment1.xml")
  )
  p[["xl/comments1.xml"]] <- paste0(
    "<comments><authors><author>Bob &amp; Co</author><author>tc={1}</author></authors><commentList>",
    "<comment ref=\"C3\" authorId=\"0\"><text><t>plain &lt;text&gt;</t></text></comment>",
    "<comment ref=\"A1\" authorId=\"0\"><text><r><rPr><b/><sz val=\"10\"/></rPr><t>Bob:\nhi\r\n</t></r><r><rPr/><t>run</t></r></text></comment>",
    "<comment ref=\"B2\" authorId=\"9\"><text><r><rPr><sz val=\"8\"/></rPr><t>x</t></r></text></comment>",
    "<comment ref=\"D4\" authorId=\"1\"><text><t>skipped</t></text></comment>",
    "</commentList></comments>"
  )
  p[["xl/drawings/vmlDrawing1.vml"]] <- paste0(
    "<xml><v:shape style='position:absolute;width:200pt;height:100pt'><x:ClientData><x:Row>0</x:Row><x:Column>0</x:Column></x:ClientData></v:shape>",
    "<v:shape style='width:50pt'><x:ClientData><x:Row>1</x:Row><x:Column>1</x:Column></x:ClientData></v:shape>",
    "<v:shape style='width:50pt;height:20pt'><x:ClientData><x:Row>5</x:Row><x:Column>5</x:Column></x:ClientData></v:shape>",
    "<v:shape style='width:50pt;height:20pt'></v:shape></xml>"
  )
  p[["xl/persons/person1.xml"]] <- "<personList><person displayName=\"Ann\" id=\"{P1}\"/></personList>"
  p[["xl/threadedComments/threadedComment1.xml"]] <- paste0(
    "<ThreadedComments><threadedComment ref=\"E5\" dT=\"2024-01-02T10:20:30.00\" personId=\"{P1}\" id=\"{T1}\"><text>Hello &amp; bye</text></threadedComment>",
    "<threadedComment ref=\"E5\" dT=\"2024-01-03T11:22:33.00\" personId=\"{P2}\" id=\"{T2}\" parentId=\"{T1}\"><text>reply</text></threadedComment></ThreadedComments>"
  )
  xl <- mkxlsx(p)
  on.exit(unlink(xl))
  d1 <- easel_read(xl, what = "comments", n = 1L)
  expect_equal(c(d1$width, d1$height), c(200, 100))
  d2 <- easel_read(xl, what = "comments", n = 2L)
  expect_equal(c(d2$width, d2$height), c(144, 79))
  expect_no_error(draw_it(d1))
  expect_no_error(draw_it(easel_read(xl, what = "comments", n = 4L)))
  expect_error(easel_read(xl, what = "comments", n = 5L), "no comment number")
})

test_that("a threaded part that is missing and an empty comment list are tolerated", {
  skip_if(Sys.which("zip") == "", "no zip program")
  p <- base_parts()
  p[["xl/worksheets/_rels/sheet1.xml.rels"]] <- rels_xml(
    rel1("rId3", "http://schemas.microsoft.com/office/2017/10/relationships/threadedComment", "../threadedComments/none.xml"),
    rel1("rId1", "comments", "../comments1.xml")
  )
  p[["xl/comments1.xml"]] <- "<comments><authors/><commentList/></comments>"
  xl <- mkxlsx(p)
  on.exit(unlink(xl))
  expect_error(easel_read(xl, what = "comments"), "no comments")
})

drawing_parts <- function(drawing, sheet = "<worksheet/>", drels = rels_xml(), extra = list(), theme = NULL) {
  p <- base_parts(sheet, theme)
  p[["xl/worksheets/_rels/sheet1.xml.rels"]] <- rels_xml(rel1("rId1", "drawing", "../drawings/drawing1.xml"))
  p[["xl/drawings/drawing1.xml"]] <- drawing
  if (!is.null(drels)) p[["xl/drawings/_rels/drawing1.xml.rels"]] <- drels
  c(p, extra)
}

test_that("shapes without a transform take the anchor size from the sheet", {
  skip_if(Sys.which("zip") == "", "no zip program")
  sheet <- paste0(
    "<worksheet><sheetFormatPr baseColWidth=\"10\" defaultRowHeight=\"20\"/><cols>",
    "<col min=\"1\" max=\"2\" width=\"12\"/><col min=\"40\" max=\"41\" width=\"5\"/><col width=\"3\"/></cols>",
    "<sheetData><row r=\"1\" ht=\"30\"/><row r=\"2\"/><row r=\"500\" ht=\"9\"/><row/></sheetData></worksheet>"
  )
  plain <- "<xdr:sp><xdr:spPr><a:xfrm><a:off x=\"0\" y=\"0\"/><a:ext cx=\"0\" cy=\"0\"/></a:xfrm><a:prstGeom prst=\"rect\"/><a:solidFill><a:srgbClr val=\"FF0000\"/></a:solidFill></xdr:spPr></xdr:sp>"
  xl <- mkxlsx(drawing_parts(wsdr(plain), sheet))
  d <- easel_read(xl)
  unlink(xl)
  expect_equal(d$width, (2 * round(12 * 7) + 2 * round((10 + 5 / 7) * 7)) * 9525 / 12700, tolerance = 0.01)
  sheet2 <- "<worksheet><sheetFormatPr defaultColWidth=\"9\"/></worksheet>"
  xl <- mkxlsx(drawing_parts(wsdr(plain), sheet2))
  d <- easel_read(xl)
  unlink(xl)
  expect_gt(d$width, 0)
  xl <- mkxlsx(drawing_parts(wsdr(plain), "<worksheet/>"))
  d <- easel_read(xl)
  unlink(xl)
  expect_gt(d$width, 0)
  bad <- wsdr(plain, head = "<xdr:from><xdr:col>0</xdr:col></xdr:from><xdr:to><xdr:col>1</xdr:col></xdr:to>")
  xl <- mkxlsx(drawing_parts(bad))
  expect_error(easel_read(xl), "no shapes")
  unlink(xl)
})

test_that("pictures are cropped, flipped and skipped when unreadable", {
  skip_if(Sys.which("zip") == "", "no zip program")
  skip_if_not_installed("png")
  skip_if_not_installed("jpeg")
  rgb <- tempfile(fileext = ".png")
  png::writePNG(array(runif(20 * 30 * 3), c(20, 30, 3)), rgb)
  gray <- tempfile(fileext = ".png")
  png::writePNG(matrix(runif(20 * 30), 20, 30), gray)
  ga <- tempfile(fileext = ".png")
  png::writePNG(array(runif(20 * 30 * 2), c(20, 30, 2)), ga)
  jp <- tempfile(fileext = ".jpeg")
  jpeg::writeJPEG(array(runif(20 * 30 * 3), c(20, 30, 3)), jp)
  on.exit(unlink(c(rgb, gray, ga, jp)))
  pic <- function(rid, xfrm = "", crop = "") {
    paste0(
      "<xdr:pic><xdr:blipFill><a:blip embed=\"", rid, "\"/>", crop, "</xdr:blipFill><xdr:spPr>", xfrm, "</xdr:spPr></xdr:pic>"
    )
  }
  xf <- function(attrs) paste0("<a:xfrm", attrs, "><a:off x=\"0\" y=\"0\"/><a:ext cx=\"900000\" cy=\"600000\"/></a:xfrm>")
  body <- paste0(
    pic("rId1", xf(" flipH=\"1\" flipV=\"1\""), "<a:srcRect l=\"10000\" t=\"20000\" r=\"5000\"/>"),
    pic("rId2", xf("")), pic("rId3", xf(" rot=\"600000\"")), pic("rId4"), pic("rId5", xf("")), pic("rId6", xf("")), pic("rId7", xf("")), pic("rId8", xf(""))
  )
  dr <- rels_xml(
    rel1("rId1", "image", "../media/a.png"), rel1("rId2", "image", "../media/b.png"),
    rel1("rId3", "image", "../media/c.jpeg"), rel1("rId4", "image", "../media/a.png"),
    rel1("rId5", "image", "../media/d.gif"), rel1("rId6", "image", "../media/missing.png"),
    rel1("rId7", "image", "../media/ga.png"), rel1("rId8", "image", "../media/a.png")
  )
  xl <- tempfile(fileext = ".xlsx")
  d <- tempfile()
  dir.create(file.path(d, "xl", "media"), recursive = TRUE)
  parts <- drawing_parts(wsdr(body), drels = dr)
  for (nm in names(parts)) {
    dir.create(dirname(file.path(d, nm)), recursive = TRUE, showWarnings = FALSE)
    writeLines(parts[[nm]], file.path(d, nm))
  }
  file.copy(rgb, file.path(d, "xl/media/a.png"))
  file.copy(gray, file.path(d, "xl/media/b.png"))
  file.copy(ga, file.path(d, "xl/media/ga.png"))
  file.copy(jp, file.path(d, "xl/media/c.jpeg"))
  writeLines("GIF89a", file.path(d, "xl/media/d.gif"))
  old <- setwd(d)
  utils::zip(xl, list.files(".", recursive = TRUE), flags = "-r9Xq")
  setwd(old)
  unlink(d, recursive = TRUE)
  on.exit(unlink(xl), add = TRUE)
  dd <- easel_read(xl)
  imgs <- Filter(function(s) s$type == "image", dd$shapes)
  expect_length(imgs, 6L)
  expect_equal(sum(vapply(dd$shapes, function(s) s$type == "path", NA)), 2L)
  expect_equal(dim(imgs[[1L]]$img)[1:2], c(16L, 25L))
  expect_no_error(draw_it(dd))
})

test_that("slicer variants are resolved or fall back to nothing", {
  skip_if(Sys.which("zip") == "", "no zip program")
  frame <- function(name, cx = 1828800, cy = 2524125) {
    paste0(
      "<xdr:graphicFrame><xdr:xfrm><a:off x=\"0\" y=\"0\"/><a:ext cx=\"", cx, "\" cy=\"", cy, "\"/></xdr:xfrm>",
      "<a:graphic><a:graphicData uri=\"s\"><sle:slicer name=\"", name, "\"/></a:graphicData></a:graphic></xdr:graphicFrame>"
    )
  }
  oneanchor <- function(body) {
    wsdr(body, "oneCellAnchor", paste0(
      "<xdr:from><xdr:col>0</xdr:col><xdr:colOff>0</xdr:colOff><xdr:row>0</xdr:row><xdr:rowOff>0</xdr:rowOff></xdr:from>",
      "<xdr:ext cx=\"1828800\" cy=\"2524125\"/>"
    ))
  }
  pivot <- function(extra_item = "") {
    paste0(
      "<pivotCacheDefinition><cacheFields><cacheField name=\"Other\"><sharedItems/></cacheField>",
      "<cacheField name=\"Fruit\"><sharedItems><s v=\"apple\"/><n v=\"3\"/><s/><s v=\"pear\"/></sharedItems></cacheField></cacheFields>",
      "<extLst><ext><x14:pivotCacheDefinition pivotCacheId=\"7\"/></ext></extLst>", extra_item, "</pivotCacheDefinition>"
    )
  }
  extra <- list(
    "xl/slicers/slicer1.xml" = paste0(
      "<slicers><slicer name=\"Skip\" cache=\"Slicer_X\"/>",
      "<slicer name=\"Fruit\" cache=\"Slicer_Fruit\" caption=\"Fruits\" rowHeight=\"300000\" columnCount=\"2\"/>",
      "<slicer name=\"Nocache\" cache=\"Slicer_None\"/><slicer name=\"NoPivot\" cache=\"Slicer_NP\"/>",
      "<slicer name=\"NoLabels\" cache=\"Slicer_NL\"/></slicers>"
    ),
    "xl/slicerCaches/slicerCache1.xml" = paste0(
      "<slicerCacheDefinition name=\"Slicer_Other\" sourceName=\"Other\"><data><tabular pivotCacheId=\"7\"><items count=\"1\"><i x=\"0\"/></items></tabular></data></slicerCacheDefinition>"
    ),
    "xl/slicerCaches/slicerCache2.xml" = paste0(
      "<slicerCacheDefinition name=\"Slicer_Fruit\" sourceName=\"Fruit\"><data><tabular pivotCacheId=\"7\">",
      "<items count=\"5\"><i x=\"0\" s=\"1\"/><i x=\"1\"/><i x=\"2\" s=\"1\"/><i x=\"3\"/><i x=\"40\"/></items></tabular></data></slicerCacheDefinition>"
    ),
    "xl/slicerCaches/slicerCache3.xml" = paste0(
      "<slicerCacheDefinition name=\"Slicer_NP\" sourceName=\"Fruit\"><data><tabular pivotCacheId=\"99\"><items count=\"1\"><i x=\"0\"/></items></tabular></data></slicerCacheDefinition>"
    ),
    "xl/slicerCaches/slicerCache4.xml" = paste0(
      "<slicerCacheDefinition name=\"Slicer_NL\" sourceName=\"Missing\"><data><tabular pivotCacheId=\"7\"><items count=\"1\"><i x=\"0\"/></items></tabular></data></slicerCacheDefinition>"
    ),
    "xl/pivotCache/pivotCacheDefinition1.xml" = pivot(),
    "xl/pivotCache/pivotCacheDefinition2.xml" = "<pivotCacheDefinition><extLst><ext><x14:pivotCacheDefinition pivotCacheId=\"5\"/></ext></extLst></pivotCacheDefinition>"
  )
  theme <- paste0(
    "<a:theme xmlns:a=\"y\"><a:clrScheme><a:accent1><a:srgbClr val=\"ff0000\"/></a:accent1></a:clrScheme></a:theme>"
  )
  body <- paste0(frame("Fruit"), frame("Unknown"), frame("Nocache"), frame("NoPivot"), frame("NoLabels"), frame("Fruit", 1828800, 400000))
  xl <- mkxlsx(drawing_parts(oneanchor(body), extra = extra, theme = theme))
  on.exit(unlink(xl))
  d <- easel_read(xl)
  tx <- Filter(function(s) identical(s$type, "text"), d$shapes)
  labels <- unlist(lapply(tx, function(s) s$paras[[1L]]$text))
  expect_true(all(c("Fruits", "apple", "3", "pear") %in% labels))
  expect_false("Unknown" %in% labels)
  expect_no_error(draw_it(d))

  nf <- wsdr(
    "<xdr:graphicFrame><xdr:xfrm><a:off x=\"0\" y=\"0\"/><a:ext cx=\"0\" cy=\"0\"/></xdr:xfrm><a:graphic><a:graphicData><sle:slicer name=\"Fruit\"/></a:graphicData></a:graphic></xdr:graphicFrame>",
    "oneCellAnchor", "<xdr:from><xdr:col>0</xdr:col></xdr:from>"
  )
  xl2 <- mkxlsx(drawing_parts(nf, extra = extra))
  on.exit(unlink(xl2), add = TRUE)
  expect_error(easel_read(xl2), "no shapes")
})

test_that("chart frames are skipped or warned about when they cannot be drawn", {
  skip_if(Sys.which("zip") == "", "no zip program")
  skip_if_not_installed("openxlsx2")
  skip_if_not_installed("encharter")
  frame <- paste0(
    "<xdr:graphicFrame><xdr:xfrm><a:off x=\"0\" y=\"0\"/><a:ext cx=\"900000\" cy=\"600000\"/></xdr:xfrm>",
    "<a:graphic><a:graphicData><c:chart id=\"rId1\"/></a:graphicData></a:graphic></xdr:graphicFrame>",
    "<xdr:graphicFrame><xdr:xfrm><a:off x=\"0\" y=\"0\"/><a:ext cx=\"900000\" cy=\"600000\"/></xdr:xfrm>",
    "<a:graphic><a:graphicData><c:chart id=\"rId2\"/></a:graphicData></a:graphic></xdr:graphicFrame>",
    "<xdr:graphicFrame><xdr:xfrm><a:off x=\"0\" y=\"0\"/><a:ext cx=\"900000\" cy=\"600000\"/></xdr:xfrm>",
    "<a:graphic><a:graphicData><c:chart id=\"rId3\"/></a:graphicData></a:graphic></xdr:graphicFrame>",
    "<xdr:sp><xdr:spPr><a:xfrm><a:off x=\"0\" y=\"0\"/><a:ext cx=\"10\" cy=\"10\"/></a:xfrm></xdr:spPr></xdr:sp>"
  )
  dr <- rels_xml(
    rel1("rId1", "chart", "../charts/chart1.xml"), rel1("rId2", "chart", "../charts/chart3.xml"),
    rel1("rId3", "chart", "../charts/none.xml")
  )
  xl <- mkxlsx(drawing_parts(wsdr(frame), drels = dr, extra = list(
    "xl/charts/chart1.xml" = "<c:chartSpace/>", "xl/charts/chart3.xml" = "<c:chartSpace/>"
  )))
  on.exit(unlink(xl))
  d <- easel_read(xl)
  charts <- Filter(function(s) s$type == "chart", d$shapes)
  expect_length(charts, 2L)
  expect_equal(vapply(charts, function(s) s$row, 1L), c(1L, 2L))

  testthat::local_mocked_bindings(
    wb_load = function(...) structure(list(), class = "fakewb"),
    .package = "openxlsx2"
  )
  testthat::local_mocked_bindings(
    ec_load = function(wb, chart) if (chart == 1L) stop("cannot load") else structure(list(), class = "boomchart"),
    .package = "encharter"
  )
  registerS3method("plot", "boomchart", function(x, ...) stop("cannot draw"))
  expect_warning(expect_warning(draw_it(d), "could not be read"), "could not be drawn")

})

test_that("odd structure in drawings is tolerated", {
  skip_if(Sys.which("zip") == "", "no zip program")
  clr <- function(inner) paste0("<a:solidFill>", inner, "</a:solidFill>")
  mods <- clr(paste0(
    "<a:srgbClr val=\"336699\"><a:lumMod val=\"50000\"/><a:lumOff val=\"20000\"/><a:tint val=\"60000\"/>",
    "<a:shade val=\"80000\"/><a:alpha val=\"50000\"/><a:satMod val=\"90000\"/></a:srgbClr>"
  ))
  body <- paste0(
    xsp(0, 0, 400000, 400000, prst("rect"), mods),
    xsp(500000, 0, 400000, 400000, prst("rect"), "<a:solidFill/>", body = paste0(
      "<xdr:txBody><a:bodyPr/><a:p><a:r><a:rPr><a:solidFill><a:srgbClr val=\"112233\"/></a:solidFill></a:rPr><a:t></a:t></a:r>",
      "<a:r><a:t>t</a:t></a:r></a:p></xdr:txBody>"
    )),
    xsp(1000000, 0, 400000, 400000, prst("rect"), clr("<a:sysClr val=\"windowText\" lastClr=\"010203\"/>"), body = paste0(
      "<xdr:txBody><a:bodyPr/><a:p><a:r><a:rPr><a:solidFill><a:schemeClr val=\"accent9\"/></a:solidFill></a:rPr><a:t>unseen</a:t></a:r></a:p></xdr:txBody>"
    )),
    xsp(1500000, 0, 400000, 400000, prst("rect"), clr("<a:schemeClr val=\"accent9\"/>")),
    xsp(2000000, 0, 400000, 400000, "<a:custGeom><a:pathLst><a:path><a:moveTo><a:pt x=\"1\" y=\"1\"/></a:moveTo></a:path></a:pathLst></a:custGeom>",
      "<a:ln><a:solidFill><a:srgbClr val=\"000000\"/></a:solidFill></a:ln>"),
    paste0(
      "<xdr:grpSp><xdr:grpSpPr><a:xfrm><a:off x=\"0\" y=\"1000000\"/><a:ext cx=\"500000\" cy=\"500000\"/><a:chOff x=\"0\" y=\"0\"/><a:chExt cx=\"0\" cy=\"0\"/></a:xfrm></xdr:grpSpPr>",
      xsp(0, 0, 100, 100, prst("rect"), clr("<a:srgbClr val=\"FF00FF\"/>")), "</xdr:grpSp>"
    )
  )
  d <- read_xml(wsdr(body))
  expect_no_error(draw_it(d))

  choice <- paste0(
    "<xdr:wsDr xmlns:xdr=\"x\" xmlns:a=\"y\" xmlns:mc=\"z\"><mc:AlternateContent><mc:Choice Requires=\"x\"><xdr:twoCellAnchor>",
    "<xdr:from><xdr:col>0</xdr:col><xdr:colOff>0</xdr:colOff><xdr:row>0</xdr:row><xdr:rowOff>0</xdr:rowOff></xdr:from>",
    "<xdr:to><xdr:col>1</xdr:col><xdr:colOff>0</xdr:colOff><xdr:row>1</xdr:row><xdr:rowOff>0</xdr:rowOff></xdr:to>",
    xsp(0, 0, 100, 100, prst("rect"), clr("<a:srgbClr val=\"FF0000\"/>")),
    "</xdr:twoCellAnchor></mc:Choice><mc:Fallback><xdr:twoCellAnchor><xdr:sp/></xdr:twoCellAnchor></mc:Fallback></mc:AlternateContent></xdr:wsDr>"
  )
  expect_length(read_xml(choice)$shapes, 1L)

  inner <- paste0(
    "<xdr:wsDr xmlns:xdr=\"x\" xmlns:a=\"y\" xmlns:mc=\"z\"><xdr:oneCellAnchor><xdr:from><xdr:col>0</xdr:col></xdr:from><xdr:ext cx=\"100\" cy=\"100\"/>",
    "<mc:AlternateContent><mc:Choice Requires=\"x\"><xdr:sp/></mc:Choice><mc:Fallback>",
    xsp(0, 0, 100, 100, prst("rect"), clr("<a:srgbClr val=\"00FF00\"/>")),
    "</mc:Fallback></mc:AlternateContent></xdr:oneCellAnchor></xdr:wsDr>"
  )
  expect_length(read_xml(inner)$shapes, 1L)

  one <- wsdr(paste0(
    xsp(0, 0, 10, 10, prst("rect"), clr("<a:srgbClr val=\"FF0000\"/>")),
    "<xdr:graphicFrame><xdr:xfrm><a:off x=\"0\" y=\"0\"/><a:ext cx=\"9\" cy=\"9\"/></xdr:xfrm><a:graphic><a:graphicData><c:chart/></a:graphicData></a:graphic></xdr:graphicFrame>",
    "<xdr:graphicFrame><xdr:xfrm><a:off x=\"0\" y=\"0\"/><a:ext cx=\"9\" cy=\"9\"/></xdr:xfrm><a:graphic><a:graphicData><c:chart id=\"rId77\"/></a:graphicData></a:graphic></xdr:graphicFrame>"
  ))
  xl <- mkxlsx(drawing_parts(one, drels = NULL))
  expect_length(easel_read(xl)$shapes, 1L)
  unlink(xl)
  xl <- mkxlsx(drawing_parts(one, drels = rels_xml(rel1("rId1", "chart", "../charts/c1.xml"))))
  expect_length(easel_read(xl)$shapes, 1L)
  unlink(xl)

  p <- drawing_parts(one)
  p[["xl/worksheets/_rels/sheet1.xml.rels"]] <- rels_xml(rel1("rId1", "drawing", "/xl/drawings/drawing1.xml"))
  xl <- mkxlsx(p)
  expect_length(easel_read(xl)$shapes, 1L)
  unlink(xl)
})

test_that("all = TRUE reads every drawing and comment and plots them together", {
  skip_if(Sys.which("zip") == "", "no zip program")
  sf <- "<a:solidFill><a:srgbClr val=\"336699\"/></a:solidFill>"
  good <- xsp(0, 0, 400000, 400000, prst("rect"), sf)
  bad <- "<xdr:sp><xdr:spPr/></xdr:sp>"
  anchor <- function(body) {
    paste0(
      "<xdr:twoCellAnchor><xdr:from><xdr:col>0</xdr:col><xdr:colOff>0</xdr:colOff><xdr:row>0</xdr:row><xdr:rowOff>0</xdr:rowOff></xdr:from>",
      "<xdr:to><xdr:col>1</xdr:col><xdr:colOff>0</xdr:colOff><xdr:row>1</xdr:row><xdr:rowOff>0</xdr:rowOff></xdr:to>", body, "</xdr:twoCellAnchor>"
    )
  }
  dr <- paste0("<xdr:wsDr xmlns:xdr=\"x\" xmlns:a=\"y\">", anchor(good), anchor(bad), anchor(good), "</xdr:wsDr>")
  xl <- mkxlsx(drawing_parts(dr))
  on.exit(unlink(xl))
  all_d <- easel_read(xl, all = TRUE)
  expect_s3_class(all_d, "easel_drawings")
  expect_equal(names(all_d), c("sheet 1, drawing 1", "sheet 1, drawing 3"))
  expect_output(print(all_d), "2 drawings")
  grDevices::pdf(NULL)
  expect_no_error(plot(all_d))
  grDevices::dev.off()

  f <- tempfile(fileext = ".xml")
  writeLines(dr, f)
  on.exit(unlink(f), add = TRUE)
  expect_equal(names(easel_read(f, all = TRUE)), c("drawing 1", "drawing 3"))

  expect_error(easel_read(xl, what = "comments", all = TRUE), "nothing to read")
})

test_that("diagrams are drawn from their stored shapes", {
  skip_if(Sys.which("zip") == "", "no zip program")
  frame <- function(dm) {
    paste0(
      "<xdr:graphicFrame><xdr:xfrm><a:off x=\"0\" y=\"0\"/><a:ext cx=\"0\" cy=\"0\"/></xdr:xfrm><a:graphic><a:graphicData>",
      "<dgm:relIds r:dm=\"", dm, "\"/></a:graphicData></a:graphic></xdr:graphicFrame>"
    )
  }
  oneanchor <- function(body) {
    wsdr(body, "oneCellAnchor", paste0(
      "<xdr:from><xdr:col>0</xdr:col><xdr:colOff>0</xdr:colOff><xdr:row>0</xdr:row><xdr:rowOff>0</xdr:rowOff></xdr:from>",
      "<xdr:ext cx=\"3000000\" cy=\"1000000\"/>"
    ))
  }
  dsp <- paste0(
    "<dsp:drawing xmlns:dsp=\"d\" xmlns:a=\"y\"><dsp:spTree><dsp:sp><dsp:spPr><a:xfrm><a:off x=\"100\" y=\"100\"/><a:ext cx=\"900000\" cy=\"600000\"/></a:xfrm>",
    "<a:prstGeom prst=\"roundRect\"><a:avLst/></a:prstGeom><a:solidFill><a:srgbClr val=\"156082\"/></a:solidFill></dsp:spPr>",
    "<dsp:txBody><a:bodyPr/><a:p><a:r><a:rPr><a:solidFill><a:srgbClr val=\"FFFFFF\"/></a:solidFill></a:rPr><a:t>step</a:t></a:r></a:p></dsp:txBody></dsp:sp></dsp:spTree></dsp:drawing>"
  )
  extra <- list(
    "xl/diagrams/data1.xml" = "<dgm:dataModel xmlns:dgm=\"g\"><dgm:extLst><a:ext><dsp:dataModelExt relId=\"rId5\"/></a:ext></dgm:extLst></dgm:dataModel>",
    "xl/diagrams/data2.xml" = "<dgm:dataModel/>",
    "xl/diagrams/drawing1.xml" = dsp
  )
  dr <- rels_xml(
    rel1("rId1", "diagramData", "../diagrams/data1.xml"), rel1("rId2", "diagramData", "../diagrams/data2.xml"),
    rel1("rId5", "http://schemas.microsoft.com/office/2007/relationships/diagramDrawing", "../diagrams/drawing1.xml")
  )
  xl <- mkxlsx(drawing_parts(oneanchor(frame("rId1")), drels = dr, extra = extra))
  on.exit(unlink(xl))
  d <- easel_read(xl)
  expect_equal(vapply(d$shapes, function(s) s$type, ""), c("path", "text"))
  expect_equal(d$shapes[[2L]]$paras[[1L]]$text, "step")
  expect_no_error(draw_it(d))

  xl2 <- mkxlsx(drawing_parts(oneanchor(paste0(frame("rId2"), frame("rId9"))), drels = dr, extra = extra))
  on.exit(unlink(xl2), add = TRUE)
  expect_error(easel_read(xl2), "no shapes")
})

brt <- function(id, payload = raw()) {
  head <- if (id < 128L) as.raw(id) else as.raw(c(128L + id %% 128L, id %/% 128L))
  len <- length(payload)
  size <- if (len < 128L) as.raw(len) else as.raw(c(128L + len %% 128L, len %/% 128L))
  c(head, size, payload)
}

u32 <- function(x) writeBin(as.integer(x), raw(), size = 4L)

u16 <- function(x) writeBin(as.integer(x), raw(), size = 2L)

wide <- function(x) c(u32(nchar(x)), iconv(x, "UTF-8", "UTF-16LE", toRaw = TRUE)[[1L]])

test_that("binary workbooks are read, including the sheet geometry", {
  skip_if(Sys.which("zip") == "", "no zip program")
  bundle <- function(name, rid) {
    brt(156L, c(u32(0L), u32(1L), if (is.na(rid)) u32(-1L) else wide(rid), wide(name)))
  }
  wbbin <- c(brt(1L, as.raw(rep(1L, 200L))), bundle("First", "rId1"), bundle("Second", NA_character_))
  sheetbin <- c(
    brt(485L, c(u32(0L), u16(8L), u16(300L), u16(0L))),
    brt(60L, c(u32(0L), u32(1L), u32(2560L), u32(0L), u16(0L))),
    brt(0L, c(u32(0L), u32(0L), u16(400L), u16(0L))),
    brt(0L, c(u32(5L), u32(0L), u16(400L), u16(0L)))
  )
  plain <- paste0(
    "<xdr:sp><xdr:spPr><a:xfrm><a:off x=\"0\" y=\"0\"/><a:ext cx=\"0\" cy=\"0\"/></a:xfrm><a:prstGeom prst=\"rect\"/>",
    "<a:solidFill><a:srgbClr val=\"FF0000\"/></a:solidFill></xdr:spPr></xdr:sp>"
  )
  head <- paste0(
    "<xdr:from><xdr:col>0</xdr:col><xdr:colOff>0</xdr:colOff><xdr:row>0</xdr:row><xdr:rowOff>0</xdr:rowOff></xdr:from>",
    "<xdr:to><xdr:col>2</xdr:col><xdr:colOff>0</xdr:colOff><xdr:row>2</xdr:row><xdr:rowOff>0</xdr:rowOff></xdr:to>"
  )
  parts <- list(
    "xl/workbook.bin" = wbbin,
    "xl/_rels/workbook.bin.rels" = rels_xml(rel1("rId1", "worksheet", "worksheets/sheet1.bin")),
    "xl/worksheets/sheet1.bin" = sheetbin,
    "xl/worksheets/_rels/sheet1.bin.rels" = rels_xml(rel1("rId1", "drawing", "../drawings/drawing1.xml")),
    "xl/drawings/drawing1.xml" = wsdr(plain, head = head)
  )
  xl <- mkxlsx(parts, ".xlsb")
  on.exit(unlink(xl))
  d <- easel_read(xl)
  expect_equal(c(d$width, d$height), c(2 * 70 * 9525, 20 * 12700 + 15 * 12700 * 2 - 15 * 12700) / 12700)
  expect_equal(c(easel_read(xl, sheet = "First")$width), d$width)
  expect_error(easel_read(xl, sheet = "Second"), "cannot resolve")
  expect_error(easel_read(xl, sheet = "Third"), "sheet not found")
  expect_error(easel_read(xl, sheet = 0L), "sheet not found")
  expect_error(easel_read(xl, what = "comments"), "binary workbook")

  parts[["xl/worksheets/sheet1.bin"]] <- c(brt(485L, c(u32(2560L), u16(8L), u16(300L), u16(0L))), sheetbin[-seq_len(13L)])
  xl2 <- mkxlsx(parts, ".xlsb")
  on.exit(unlink(xl2), add = TRUE)
  expect_gt(easel_read(xl2)$width, 0)
})

test_that("a workbook object is accepted", {
  skip_if_not_installed("openxlsx2")
  skip_if_not_installed("png")
  img <- tempfile(fileext = ".png")
  png::writePNG(array(runif(10 * 10 * 3), c(10, 10, 3)), img)
  on.exit(unlink(img))
  wb <- openxlsx2::wb_workbook()$add_worksheet()$add_image(dims = "B2", file = img, width = 3, height = 2, units = "cm")
  expect_equal(easel_read(wb)$shapes[[1L]]$type, "image")
  expect_length(easel_read(wb, all = TRUE), 1L)
})

test_that("a drawing file alone cannot size a shape from its anchor", {
  plain <- paste0(
    "<xdr:sp><xdr:spPr><a:xfrm><a:off x=\"0\" y=\"0\"/><a:ext cx=\"0\" cy=\"0\"/></a:xfrm><a:prstGeom prst=\"rect\"/>",
    "<a:solidFill><a:srgbClr val=\"FF0000\"/></a:solidFill></xdr:spPr></xdr:sp>"
  )
  expect_error(read_xml(wsdr(plain)), "no shapes")
})

test_that("binary workbooks go through openxlsx2 when it can read them", {
  skip_if_not_installed("openxlsx2")
  skip_if_not_installed("png")
  img <- tempfile(fileext = ".png")
  png::writePNG(array(runif(10 * 10 * 3), c(10, 10, 3)), img)
  on.exit(unlink(img))
  wb <- openxlsx2::wb_workbook()$add_worksheet()$add_image(dims = "B2", file = img, width = 3, height = 2, units = "cm")
  testthat::local_mocked_bindings(wb_load = function(...) wb, .package = "openxlsx2")
  skip_if(utils::packageVersion("openxlsx2") <= "1.30")
  expect_equal(easel_read("any.xlsb")$shapes[[1L]]$type, "image")
  expect_equal(easel_read("any.xls")$shapes[[1L]]$type, "image")
})

test_that("an unreadable xls file is reported", {
  f <- tempfile(fileext = ".xls")
  writeLines("junk", f)
  on.exit(unlink(f))
  expect_error(easel_read(f), "xls")
})
