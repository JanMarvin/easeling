#' Open a DrawingML graphics device for openxlsx2
#'
#' Draws directly to a standalone DrawingML XML file containing `xdr:sp`
#' shapes, suitable for `openxlsx2::wb_add_drawing(xml = file)`. No
#' dependency on Cairo, FreeType, fontconfig, or xml2.
#'
#' @param file Path to the output XML file. Defaults to a temp file.
#' @param width,height Device size in inches.
#' @param pointsize Default font pointsize.
#'
#' @return The output file path, invisibly.
#'
#' @useDynLib xdrxlsx, .registration=TRUE, .fixes="C_"
#' @export
#'
#' @examples
#' \donttest{
#' f <- xdr_xlsx(width = 6, height = 4)
#' plot(1:10, (1:10)^2, type = "b")
#' dev.off()
#' }
xdr_xlsx <- function(file = tempfile(fileext = ".xml"), width = 6, height = 6,
                     pointsize = 12) {
  file <- path.expand(file)
  invisible(.Call(C_xdrxlsx_, file, as.double(width), as.double(height),
                  as.double(pointsize)))
  invisible(file)
}
