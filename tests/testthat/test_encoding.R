xml_bytes <- function(file) readBin(file, "raw", file.size(file))

has_bytes <- function(haystack, needle) {
  length(grepRaw(charToRaw(enc2utf8(needle)), haystack, fixed = TRUE)) > 0
}

latin1 <- function(x) iconv(x, "UTF-8", "latin1")

test_that("text labels are written as UTF-8", {
  u <- "Ums\u00e4tze \u20ac"
  l <- latin1("Gr\u00f6\u00dfe \u00b0C")
  expect_identical(Encoding(l), "latin1")

  f <- easel_dev(width = 4, height = 3)
  plot(1, main = u, xlab = l)
  text(1, 1, l)
  grid::grid.text(l, y = 0.1)
  dev.off()

  xml <- xml_bytes(f)
  expect_true(validUTF8(rawToChar(xml)))
  expect_true(has_bytes(xml, u))
  expect_true(has_bytes(xml, l))
})

test_that("text widths do not depend on the input encoding", {
  w <- function(s) {
    easel_dev(width = 4, height = 3)
    on.exit(dev.off())
    plot.new()
    strwidth(s, "inches")
  }
  s <- "Gr\u00f6\u00dfe \u00b0C"
  expect_equal(w(latin1(s)), w(s))
})

test_that("font names are written as UTF-8", {
  fam <- "Gr\u00f6\u00dfe Sans"
  sym <- "Symb\u00f6l"
  # a C locale cannot represent the name, R replaces it before the device
  # sees it
  representable <- l10n_info()[["UTF-8"]] || l10n_info()[["Latin-1"]]

  f <- easel_dev(width = 4, height = 3, fontname = latin1(fam), symbolfamily = latin1(sym))
  plot(1, main = "x", sub = quote(alpha + beta))
  dev.off()
  xml <- xml_bytes(f)
  expect_true(validUTF8(rawToChar(xml)))
  expect_true(has_bytes(xml, fam))
  expect_true(has_bytes(xml, sym))

  # base graphics passes the family in the native encoding
  f <- easel_dev(width = 4, height = 3)
  op <- par(family = latin1(fam))
  plot(1, main = "x")
  par(op)
  dev.off()
  xml <- xml_bytes(f)
  expect_true(validUTF8(rawToChar(xml)))
  if (representable) expect_true(has_bytes(xml, fam))

  # grid passes the bytes of the R string as they are
  for (gfam in list(fam, latin1(fam))) {
    f <- easel_dev(width = 4, height = 3)
    grid::grid.newpage()
    grid::grid.text("x", gp = grid::gpar(fontfamily = gfam))
    dev.off()
    xml <- xml_bytes(f)
    expect_true(validUTF8(rawToChar(xml)))
    if (representable || Encoding(gfam) == "UTF-8") expect_true(has_bytes(xml, fam))
  }
})

test_that("easel_xml() returns a UTF-8 string", {
  l <- latin1("Gr\u00f6\u00dfe")
  x <- easel_xml(plot(1, main = l), width = 4, height = 3, fontname = latin1("Schrift \u00c4"))
  expect_identical(Encoding(x), "UTF-8")
  expect_true(validUTF8(x))
  expect_true(has_bytes(charToRaw(x), l))
  expect_true(has_bytes(charToRaw(x), "Schrift \u00c4"))
})
