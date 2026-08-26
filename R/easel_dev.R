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
#' @useDynLib easeling, .registration=TRUE, .fixes="C_"
#' @export
#'
#' @examples
#' f <- easel_dev(width = 6, height = 4, fontname = "Georgia")
#' plot(1:10, (1:10)^2, type = "b")
#' dev.off()
easel_dev <- function(file = tempfile(fileext = ".xml"), width = 6, height = 6,
                     pointsize = 12, fontname = "Calibri",
                     underline = FALSE, strikeout = FALSE) {
  file <- path.expand(file[1L])
  width <- as.double(width[1L])
  height <- as.double(height[1L])
  pointsize <- as.double(pointsize[1L])
  if (!is.finite(width) || width <= 0 || !is.finite(height) || height <= 0)
    stop("'width' and 'height' must be positive")
  if (!is.finite(pointsize) || pointsize <= 0)
    stop("'pointsize' must be positive")
  fontname <- as.character(fontname[1L])
  if (is.na(fontname) || !nzchar(fontname))
    stop("'fontname' must be a non-empty string")
  invisible(.Call(C_easeling_, file, width, height, pointsize, fontname,
                  isTRUE(as.logical(underline[1L])),
                  isTRUE(as.logical(strikeout[1L]))))
  invisible(file)
}
