#' Open a DrawingML graphics device for openxlsx2
#'
#' Draws directly to a standalone DrawingML XML file containing `xdr:sp`
#' shapes, suitable for `openxlsx2::wb_add_drawing(xml = file)`. No
#' dependency on Cairo, FreeType, fontconfig, or xml2.
#'
#' @param file Path to the output XML file. Defaults to a temp file.
#' @param width,height Device size in inches.
#' @param pointsize Default font pointsize.
#' @param fontname Default font typeface (matches `openxlsx2::wb_add_font()`'s
#'   `name` argument), e.g. `"Calibri"`, `"Arial"`. Used whenever R itself
#'   doesn't request a specific family — i.e. whenever a plot's own
#'   `family`/`fontfamily` is unset or is one of R's generic aliases
#'   (`"sans"`, `"serif"`, `"mono"`, `"symbol"`, or `""`). If a plot sets an
#'   actual font name (e.g. `par(family = "Georgia")` or
#'   `theme_minimal(base_family = "Georgia")`), that takes priority over
#'   this default.
#' @param underline,strikeout Apply underline/strikeout to all text on this
#'   device. Unlike `fontname`, these have no per-call R equivalent (base
#'   graphics has no underline/strikeout concept), so they're a device-wide
#'   setting.
#'
#' @return The output file path, invisibly.
#'
#' @useDynLib xdrxlsx, .registration=TRUE, .fixes="C_"
#' @export
#'
#' @examples
#' \donttest{
#' f <- xdr_xlsx(width = 6, height = 4, fontname = "Georgia")
#' plot(1:10, (1:10)^2, type = "b")
#' dev.off()
#' }
xdr_xlsx <- function(file = tempfile(fileext = ".xml"), width = 6, height = 6,
                     pointsize = 12, fontname = "Calibri",
                     underline = FALSE, strikeout = FALSE) {
  file <- path.expand(file)
  invisible(.Call(C_xdrxlsx_, file, as.double(width), as.double(height),
                  as.double(pointsize), as.character(fontname),
                  as.logical(underline), as.logical(strikeout)))
  invisible(file)
}

#' Rescale an xdrxlsx drawing to a target physical size
#'
#' `xdr_xlsx()` renders at a fixed physical size (`width`/`height` in
#' inches). When that drawing is placed into a `twoCellAnchor` (e.g.
#' `openxlsx2::wb_add_drawing(dims = "A1:G15")`), openxlsx2 repositions the
#' anchor but does **not** rescale the drawing's own bounding box to fit the
#' target cell range — so if the cell range's physical size differs from
#' the size the drawing was rendered at, it will overflow or leave a gap.
#'
#' This function rewrites only the drawing's outer display box (its
#' `a:off`/`a:ext`) to a given target size, leaving every shape's own
#' coordinates (the `chOff`/`chExt` child coordinate space) untouched. This
#' is exactly what `a:xfrm`'s off/ext-vs-chOff/chExt split is designed for:
#' every shape scales proportionally to fit the new box.
#'
#' @param file Path to an XML file produced by [xdr_xlsx()] (already closed
#'   via `dev.off()`).
#' @param width,height Target physical size in inches — normally the actual
#'   rendered size of the cell range you'll pass as `dims` to
#'   `wb_add_drawing()`.
#'
#' @return The file path, invisibly. The file is rewritten in place.
#' @export
#'
#' @examples
#' \donttest{
#' f <- xdr_xlsx(width = 6, height = 4)
#' plot(1:10, (1:10)^2, type = "b")
#' dev.off()
#' xdr_fit(f, width = 4.67, height = 3.13)  # match a specific A1:G15 range
#' }
xdr_fit <- function(file, width, height) {
  xml <- readLines(file, warn = FALSE)
  xml <- paste(xml, collapse = "\n")

  emu_w <- as.integer(round(width * 914400))
  emu_h <- as.integer(round(height * 914400))

  # Only the FIRST <a:ext> (the outer group's own display box, immediately
  # inside <xdr:grpSpPr><a:xfrm>) should change. chOff/chExt and every
  # shape's own a:ext must stay exactly as rendered.
  pattern <- '(<xdr:grpSpPr><a:xfrm><a:off x="0" y="0"/><a:ext cx=")[0-9]+(" cy=")[0-9]+("/>)'
  replacement <- paste0("\\1", emu_w, "\\2", emu_h, "\\3")
  new_xml <- sub(pattern, replacement, xml)

  if (identical(new_xml, xml)) {
    warning("xdr_fit(): could not find the expected group xfrm pattern; ",
            "file left unchanged. Was this file produced by xdr_xlsx()?")
  }

  writeLines(new_xml, file)
  invisible(file)
}
