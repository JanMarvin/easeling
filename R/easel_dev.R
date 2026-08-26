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
#' @param dims Optionally, a cell range such as `"A1:G15"`. If given,
#'   `width` and `height` are ignored and computed from the region via
#'   [easel_size()], so the plot fills that region exactly when later
#'   anchored to the same `dims`.
#' @param wb,sheet Passed to [easel_size()] when `dims` is given: the
#'   `openxlsx2` workbook (and sheet) to read actual column widths and row
#'   heights from.
#' @param text_voff Vertical text calibration in em: text boxes are
#'   centre-anchored, and the baseline is placed `text_voff` em below the
#'   box centre. The default `0.35` was calibrated against Excel's line layout for
#'   Calibri; LibreOffice's optimum is around 0.24, so text there sits
#'   ~0.1 em low. Increase to shift rendered text up,
#'   decrease to shift it down, if your spreadsheet application's line
#'   layout places it visibly off; see
#'   `system.file("examples", "calibrate_text.R", package = "easeling")`.
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
                     underline = FALSE, strikeout = FALSE,
                     dims = NULL, wb = NULL, sheet = 1, text_voff = 0.35) {
  file <- path.expand(file[1L])
  if (!is.null(dims)) {
    sz <- easel_size(dims, wb = wb, sheet = sheet)
    width <- sz[["width"]]; height <- sz[["height"]]
  }
  width <- as.double(width[1L]); height <- as.double(height[1L])
  pointsize <- as.double(pointsize[1L])
  if (!is.finite(width) || width <= 0 || !is.finite(height) || height <= 0)
    stop("'width' and 'height' must be positive")
  if (!is.finite(pointsize) || pointsize <= 0)
    stop("'pointsize' must be positive")
  fontname <- as.character(fontname[1L])
  if (is.na(fontname) || !nzchar(fontname))
    stop("'fontname' must be a non-empty string")
  text_voff <- as.double(text_voff[1L])
  if (!is.finite(text_voff) || abs(text_voff) > 1)
    stop("'text_voff' must be a finite value in [-1, 1]")
  invisible(.Call(C_easeling_, file, width, height, pointsize, fontname,
                  isTRUE(as.logical(underline[1L])),
                  isTRUE(as.logical(strikeout[1L])), text_voff))
  invisible(file)
}

#' Compute the device size for a spreadsheet cell region
#'
#' Translates a cell range like `"A1:G15"` into a device size in inches, so
#' that a plot drawn at that size fills the region exactly when anchored to
#' the same `dims` — the dims-first workflow: pick the region, size the
#' device from it, then plot. Because the device is opened at the final
#' size, R lays out text, margins, and legends for that size; nothing needs
#' to be rescaled afterwards (DrawingML cannot rescale text in groups).
#'
#' Column widths and row heights are taken from `wb` when given, including
#' custom widths/heights and the sheet defaults stored in its
#' `sheetFormatPr`. Without `wb`, Excel's defaults are assumed (Calibri 11:
#' 8.43 character column width = 64px, 15pt row height = 20px). The
#' character-to-pixel conversion uses Excel's formula for a maximum digit
#' width of 7px; workbooks with a different base font will be off by the
#' ratio of their digit widths.
#'
#' @param dims A cell range such as `"A1:G15"` (both corners inclusive).
#' @param wb Optionally, an `openxlsx2` workbook (`wbWorkbook`) to read the
#'   actual column widths and row heights from.
#' @param sheet Sheet index in `wb`. Default `1`.
#'
#' @return Named numeric vector `c(width = , height = )` in inches.
#' @export
#'
#' @examples
#' easel_size("A1:G15")
easel_size <- function(dims, wb = NULL, sheet = 1) {
  dims <- gsub("$", "", as.character(dims[1L]), fixed = TRUE)
  parts <- strsplit(dims, ":", fixed = TRUE)[[1L]]
  if (length(parts) == 1L) parts <- c(parts, parts)
  if (length(parts) != 2L)
    stop("'dims' must be a cell range like \"A1:G15\"")
  ref <- function(x) {
    m <- regmatches(x, regexec("^([A-Za-z]+)([0-9]+)$", x))[[1L]]
    if (length(m) != 3L) stop("invalid cell reference: ", x)
    letters <- utf8ToInt(toupper(m[2L])) - 64L
    col <- sum(letters * 26L^rev(seq_along(letters) - 1L))
    c(col = col, row = as.integer(m[3L]))
  }
  a <- ref(parts[1L]); b <- ref(parts[2L])
  cols <- seq.int(min(a["col"], b["col"]), max(a["col"], b["col"]))
  rows <- seq.int(min(a["row"], b["row"]), max(a["row"], b["row"]))

  def_chars <- 8.43
  def_ht <- 15
  col_chars <- rep(def_chars, length(cols))
  row_ht <- rep(def_ht, length(rows))

  if (!is.null(wb)) {
    if (!inherits(wb, "wbWorkbook"))
      stop("'wb' must be an openxlsx2 wbWorkbook")
    ws <- wb$worksheets[[sheet]]
    num_attr <- function(xml, attr) {
      m <- regmatches(xml, regexec(paste0(attr, "=\"([0-9.]+)\""), xml))[[1L]]
      if (length(m) == 2L) as.numeric(m[2L]) else NA_real_
    }
    fmt <- ws$sheetFormatPr
    if (length(fmt) == 1L && nzchar(fmt)) {
      v <- num_attr(fmt, "defaultColWidth")
      if (is.na(v)) {
        base <- num_attr(fmt, "baseColWidth")
        if (!is.na(base)) v <- base + 0.43
      }
      if (!is.na(v)) col_chars[] <- v
      v <- num_attr(fmt, "defaultRowHeight")
      if (!is.na(v)) row_ht[] <- v
    }
    for (cl in ws$cols_attr) {
      cmin <- num_attr(cl, "min"); cmax <- num_attr(cl, "max")
      wdt <- num_attr(cl, "width")
      if (anyNA(c(cmin, cmax, wdt))) next
      hit <- cols >= cmin & cols <= cmax
      col_chars[hit] <- if (grepl("hidden=\"(1|true)\"", cl)) 0 else wdt
    }
    ra <- ws$sheet_data$row_attr
    if (is.data.frame(ra) && nrow(ra)) {
      r <- suppressWarnings(as.integer(ra$r))
      ht <- suppressWarnings(as.numeric(ra$ht))
      hid <- if ("hidden" %in% names(ra)) ra$hidden %in% c("1", "true") else FALSE
      for (i in seq_along(r)) {
        j <- match(r[i], rows)
        if (is.na(j)) next
        if (isTRUE(hid[i])) row_ht[j] <- 0
        else if (!is.na(ht[i])) row_ht[j] <- ht[i]
      }
    }
  }

  col_px <- ifelse(col_chars <= 0, 0, trunc(col_chars * 7 + 0.5) + 5)
  row_px <- round(row_ht * 4 / 3)
  c(width = sum(col_px) / 96, height = sum(row_px) / 96)
}
