#' Read a drawing back into R
#'
#' Parses a DrawingML drawing into a list of shapes that can be redrawn with
#' [plot()]. The drawing can come from a file written by [easel_dev()], or
#' from a workbook that contains one, including a workbook that has been
#' saved again by a spreadsheet application. This closes the loop
#' plot -> drawing -> workbook -> plot, without access to the original plot.
#' The result is a redraw of the picture, not of the original plot: axes,
#' legends and points come back as the paths and text runs they were written
#' as.
#'
#' Understood are most of the common preset shapes (basic shapes, polygons,
#' stars, block arrows, flowchart symbols, callouts, `donut`, `frame`, `cube`,
#' `can`, `heart`, `cloud`, `wave` and more; shadings and 3-D lighting are not
#' reproduced and presets not in the list are drawn as their bounding box),
#' shapes without a transform (sized from their anchor), theme and style
#' colors, `custGeom` paths made of `moveTo`, `lnTo`,
#' `cubicBezTo`, `quadBezTo` and `close`, solid, linear and radial gradient
#' fills (R >= 4.1 for gradients, older versions use the first stop color),
#' line properties, shape rotation and flips, nested groups, and text.
#' Embedded PNG and JPEG pictures are drawn when the png or jpeg package is
#' installed, including cropping, rotation and flips. Charts are drawn with
#' the encharter package (with openxlsx2) when both are installed. Slicers
#' of pivot tables are drawn with their items and which of them are selected;
#' for any other object, such as a slicer of a table, the placeholder shape
#' stored for older readers is drawn. Diagrams (SmartArt) are drawn from the
#' shapes Office stores with them. Pictures that cannot be decoded, such as
#' EMF or GIF, are drawn as an empty gray frame. Equations (Office Math in text) are
#' drawn for fractions, scripts, roots, sums, integrals, brackets and accents.
#' Arcs, underlines and strike-through are ignored.
#'
#' @param file Path of an XML file written by [easel_dev()], of a drawing XML
#'   file, or of an `.xlsx`, `.xlsm`, `.xlsb` or `.xls` workbook, or an
#'   openxlsx2 `wbWorkbook` object. Binary workbooks (`.xlsb`, `.xls`) are
#'   converted with openxlsx2 (> 1.30) when it is installed. Without it,
#'   `.xlsb` files are read directly, but comments and slicers are not
#'   available and shapes without a size are sized from the sheet's records;
#'   `.xls` files cannot be read.
#' @param sheet For a workbook, the sheet (name or position) holding the
#'   drawing.
#' @param n Which drawing on the sheet to read, in the order of the
#'   drawing's anchors, or which comment, in the order of the cells.
#' @param all If `TRUE`, reads every drawing of every sheet of a workbook (or
#'   every drawing of an XML file, or every note and threaded comment with
#'   `what = "comments"`) and returns them as one object that `plot()` draws
#'   as a grid of panels, each labeled with its sheet and position. `sheet`
#'   and `n` are ignored.
#' @param what `"drawing"` reads a drawing. `"comments"` reads a note or a
#'   threaded comment of a worksheet and returns it as the box a spreadsheet
#'   application shows for it.
#' @param x An `easel_drawing` object, or the list of them returned with
#'   `all = TRUE`.
#' @param family Font family used for text. The typefaces named in the file
#'   are not used, since a graphics device rarely knows them.
#' @param pad Extra margin in points around the drawing, so that thick strokes
#'   at the edge are not cut off. The default of 0 reproduces the drawing's
#'   frame exactly.
#' @param text_voff Baseline offset in em below the center of a text box. The
#'   file does not record it, so it must match the `text_voff` the drawing
#'   was written with (see [easel_dev()]).
#' @param ... Ignored by the plot method of a single drawing; the plot method
#'   of several drawings passes them on to it.
#' @return `easel_read()` returns an object of class `easel_drawing`, a list
#'   with the drawing `width` and `height` in points and a list of `shapes`.
#'   With `all = TRUE` it returns a named list of such objects of class
#'   `easel_drawings`.
#'   `plot()` returns `x` invisibly.
#' @examples
#' f <- easel_dev(width = 4, height = 3)
#' plot(1:10, main = "round trip")
#' dev.off()
#'
#' d <- easel_read(f)
#' plot(d)
#' @importFrom graphics par plot.new plot.window polygon polypath lines rasterImage text strwidth
#' @importFrom graphics title
#' @importFrom utils unzip
#' @export
easel_read <- function(file, sheet = 1L, n = 1L, what = c("drawing", "comments"), all = FALSE) {
  what <- match.arg(what)
  if (inherits(file, "wbWorkbook")) {
    wb_file <- tempfile(fileext = ".xlsx")
    file$save(wb_file)
    file <- wb_file
  }
  if (is.character(file) && grepl("\\.xlsb?$", file, ignore.case = TRUE)) {
    wb_loaded <- NULL
    if (requireNamespace("openxlsx2", quietly = TRUE) && utils::packageVersion("openxlsx2") > "1.30") {
      wb_loaded <- tryCatch(openxlsx2::wb_load(file), error = function(e) NULL)
    }
    if (!is.null(wb_loaded)) {
      wb_file <- tempfile(fileext = ".xlsx")
      wb_loaded$save(wb_file)
      file <- wb_file
    } else if (grepl("\\.xls$", file, ignore.case = TRUE)) {
      stop("could not read the .xls file (reading .xls needs openxlsx2 > 1.30)", call. = FALSE)
    }
  }
  if (isTRUE(all)) {
    book <- grepl("\\.xls[xmb]$", file, ignore.case = TRUE)
    out <- list()
    labels <- character()
    done <- FALSE
    max_sheet <- 1L
    if (book) max_sheet <- 1000L
    for (s in seq_len(max_sheet)) {
      for (k in seq_len(1000L)) {
        d <- tryCatch(easel_read(file, sheet = s, n = k, what = what), error = function(e) e)
        if (inherits(d, "error")) {
          msg <- conditionMessage(d)
          done <- grepl("sheet not found", msg, fixed = TRUE)
          if (done || !grepl("no shapes", msg, fixed = TRUE)) break
          next
        }
        out[[length(out) + 1L]] <- d
        labels <- c(labels, paste0(if (book) paste0("sheet ", s, ", "), what, " ", k))
      }
      if (done) break
    }
    if (length(out) == 0L) stop("nothing to read in ", file, call. = FALSE)
    names(out) <- labels
    return(structure(out, class = "easel_drawings"))
  }
  theme <- character()
  file_path <- NULL
  sheet_xml <- function(max_row) NULL
  target_of <- function(rid) NULL
  media_of <- function(rid) NULL
  chart_row <- function(path) NA_integer_
  slicer_of <- function(name) NULL
  diagram_of <- function(rid) NULL
  if (grepl("\\.xls[xmb]$", file, ignore.case = TRUE)) {
    xlsb <- grepl("\\.xlsb$", file, ignore.case = TRUE)
    entries <- unzip(file, list = TRUE)$Name
    file_path <- normalizePath(file)
    td <- tempfile()
    on.exit(unlink(td, recursive = TRUE))
    rd <- function(path, sep = "") {
      out <- unzip(file, files = path, exdir = td)
      paste(readLines(out, warn = FALSE, encoding = "UTF-8"), collapse = sep)
    }
    resolve <- function(base, target) {
      if (startsWith(target, "/")) return(substring(target, 2L))
      parts <- strsplit(paste0(base, "/", target), "/", fixed = TRUE)[[1L]]
      out <- character()
      for (p in parts) {
        if (p == "..") out <- out[-length(out)] else if (p != "." && p != "") out <- c(out, p)
      }
      paste(out, collapse = "/")
    }

    wb_part <- if (xlsb) "xl/workbook.bin" else "xl/workbook.xml"
    snames <- srids <- character()
    if (xlsb) {
      bin_file <- unzip(file, files = wb_part, exdir = td)
      bin <- readBin(bin_file, "raw", n = file.size(bin_file))
      pos <- 1L
      while (pos <= length(bin)) {
        id <- as.integer(bin[pos])
        pos <- pos + 1L
        if (id >= 128L) {
          id <- (id - 128L) + (as.integer(bin[pos]) %% 128L) * 128L
          pos <- pos + 1L
        }
        len <- 0L
        mul <- 1L
        for (k in 1:4) {
          b <- as.integer(bin[pos])
          pos <- pos + 1L
          len <- len + (b %% 128L) * mul
          mul <- mul * 128L
          if (b < 128L) break
        }
        if (id == 156L) {
          rec <- bin[pos + seq_len(len) - 1L]
          nrid <- readBin(rec[9:12], "integer", size = 4L)
          rid_end <- 12L + 2L * max(nrid, 0L)
          srids <- c(srids, if (nrid > 0L) iconv(list(rec[13:rid_end]), "UTF-16LE", "UTF-8") else "")
          nnm <- readBin(rec[rid_end + 1:4], "integer", size = 4L)
          snames <- c(snames, iconv(list(rec[rid_end + 4L + seq_len(2L * nnm)]), "UTF-16LE", "UTF-8"))
        }
        pos <- pos + len
      }
    } else {
      wb <- ooxml_parse(rd(wb_part))
      sheets <- which(wb$name == "sheet")
      snames <- vapply(sheets, function(i) ooxml_attr(wb, i, "name"), "")
      srids <- vapply(sheets, function(i) ooxml_attr(wb, i, "id"), "")
    }
    pick <- if (is.character(sheet)) match(sheet, snames) else sheet
    if (length(pick) != 1L || is.na(pick) || pick < 1L || pick > length(snames)) {
      stop("sheet not found in ", file, call. = FALSE)
    }
    rid <- srids[pick]

    rels <- ooxml_parse(rd(paste0("xl/_rels/", basename(wb_part), ".rels")))
    rel <- which(rels$name == "Relationship")
    rel <- rel[vapply(rel, function(i) identical(ooxml_attr(rels, i, "Id"), rid), NA)]
    if (length(rel) == 0L) stop("cannot resolve the sheet in ", file, call. = FALSE)
    sheet_path <- resolve("xl", ooxml_attr(rels, rel[1L], "Target"))

    if ("xl/theme/theme1.xml" %in% entries) {
      th <- ooxml_parse(rd("xl/theme/theme1.xml"))
      scheme <- ooxml_kids(th, which(th$name == "clrScheme")[1L], c("dk1", "lt1", "dk2", "lt2", paste0("accent", 1:6), "hlink", "folHlink"))
      for (k in scheme) {
        c1 <- ooxml_kids(th, k, c("srgbClr", "sysClr"))[1L]
        if (is.na(c1)) next
        theme[th$name[k]] <- toupper(ooxml_attr(th, c1, if (th$name[c1] == "srgbClr") "val" else "lastClr"))
      }
    }

    if (what == "comments") {
      if (xlsb) stop("comments cannot be read from a binary workbook (xlsb)", call. = FALSE)
      crel_path <- paste0(dirname(sheet_path), "/_rels/", basename(sheet_path), ".rels")
      if (!crel_path %in% entries) stop("the sheet has no comments", call. = FALSE)
      crels <- ooxml_parse(rd(crel_path))
      rel <- which(crels$name == "Relationship")
      rtype <- vapply(rel, function(j) ooxml_attr(crels, j, "Type"), "")
      rpath <- vapply(rel, function(j) resolve(dirname(sheet_path), ooxml_attr(crels, j, "Target")), "")
      decode <- function(x) {
        x <- gsub("&lt;", "<", x, fixed = TRUE)
        x <- gsub("&gt;", ">", x, fixed = TRUE)
        x <- gsub("&quot;", "\"", x, fixed = TRUE)
        x <- gsub("&apos;", "'", x, fixed = TRUE)
        gsub("&amp;", "&", x, fixed = TRUE)
      }
      cell <- function(ref) {
        letters_ <- utf8ToInt(sub("[0-9]+$", "", ref)) - 64L
        c(row = as.numeric(sub("^[A-Z]+", "", ref)), col = sum(letters_ * 26^(rev(seq_along(letters_)) - 1L)))
      }
      items <- list()

      vml <- character()
      if (any(grepl("/vmlDrawing$", rtype))) {
        vp <- rpath[grepl("/vmlDrawing$", rtype)][1L]
        if (vp %in% entries) vml <- strsplit(rd(vp), "</v:shape>", fixed = TRUE)[[1L]]
      }
      box_size <- function(ref, def) {
        rc <- cell(ref)
        for (v in vml) {
          r0 <- suppressWarnings(as.numeric(sub("^.*<x:Row>([0-9]+)</x:Row>.*$", "\\1", v)))
          c0 <- suppressWarnings(as.numeric(sub("^.*<x:Column>([0-9]+)</x:Column>.*$", "\\1", v)))
          if (is.na(r0) || is.na(c0) || r0 + 1 != rc[["row"]] || c0 + 1 != rc[["col"]]) next
          wv <- suppressWarnings(as.numeric(sub("^.*[; ']width:([0-9.]+)pt.*$", "\\1", v)))
          hv <- suppressWarnings(as.numeric(sub("^.*[; ']height:([0-9.]+)pt.*$", "\\1", v)))
          if (!is.na(wv) && !is.na(hv)) return(c(wv, hv))
        }
        def
      }

      tpath <- rpath[grepl("/threadedComment$", rtype)]
      if (length(tpath) > 0L && tpath[1L] %in% entries) {
        persons <- character()
        for (pp in entries[grepl("^xl/persons/.*\\.xml$", entries)]) {
          pd <- ooxml_parse(rd(pp))
          for (j in which(pd$name == "person")) persons[ooxml_attr(pd, j, "id")] <- ooxml_attr(pd, j, "displayName")
        }
        thd <- ooxml_parse(rd(tpath[1L], "\n"))
        tc <- which(thd$name == "threadedComment")
        parent <- vapply(tc, function(j) ooxml_attr(thd, j, "parentId"), "")
        for (k in tc[is.na(parent)]) {
          members <- c(k, tc[!is.na(parent) & parent == ooxml_attr(thd, k, "id")])
          bw_t <- 260
          msgs <- list()
          ytot <- 0
          for (m in members) {
            who <- persons[ooxml_attr(thd, m, "personId")]
            stamp <- sub("T", " ", sub("^([0-9-]+T[0-9]+:[0-9]+).*$", "\\1", ooxml_attr(thd, m, "dT")))
            body <- decode(thd$text[ooxml_kids(thd, m, "text")[1L]])
            ind <- if (m == k) 0 else 22
            nl <- ceiling(max(nchar(body), 1) / floor((bw_t - ind - 10) / 5.8))
            mh <- 16 + 13.2 + 10.8 + nl * 13.2
            msgs[[length(msgs) + 1L]] <- list(
              indent = ind, y = ytot, h = mh,
              paras = list(
                list(algn = "l", text = if (is.na(who)) "" else who, size = 11, font = 2L, col = "#000000", rise = 0),
                list(algn = "l", text = stamp, size = 9, font = 1L, col = "#605E5C", rise = 0),
                list(algn = "l", text = body, size = 11, font = 1L, col = "#000000", rise = 0)
              )
            )
            ytot <- ytot + mh
          }
          items[[length(items) + 1L]] <- list(
            ref = ooxml_attr(thd, k, "ref"), msgs = msgs, fill = "#FFFFFF", col = "#C8C6C4",
            size = c(bw_t, ytot + 4)
          )
        }
      }

      cpath <- rpath[grepl("/comments$", rtype)]
      if (length(cpath) > 0L && cpath[1L] %in% entries) {
        cd <- ooxml_parse(rd(cpath[1L], "\n"))
        authors <- vapply(ooxml_kids(cd, which(cd$name == "authors")[1L], "author"), function(j) decode(cd$text[j]), "")
        for (k in which(cd$name == "comment")) {
          who <- authors[as.numeric(ooxml_attr(cd, k, "authorId")) + 1]
          if (!is.na(who) && startsWith(who, "tc=")) next
          paras <- list(list(algn = "l", text = character(), size = numeric(), font = integer(), col = character(), rise = numeric()))
          tx <- ooxml_kids(cd, k, "text")[1L]
          runs <- ooxml_kids(cd, tx, c("r", "t"))
          for (r in runs) {
            if (cd$name[r] == "t") {
              txt <- decode(cd$text[r])
              sz <- 9
              bold <- FALSE
            } else {
              rpr <- ooxml_kids(cd, r, "rPr")[1L]
              txt <- decode(cd$text[ooxml_kids(cd, r, "t")[1L]])
              szn <- ooxml_kids(cd, rpr, "sz")[1L]
              sz <- if (is.na(szn)) 9 else as.numeric(ooxml_attr(cd, szn, "val"))
              bold <- length(ooxml_kids(cd, rpr, "b")) > 0L
            }
            pieces <- strsplit(gsub("\r", "", txt), "\n", fixed = TRUE)[[1L]]
            if (grepl("\n$", txt)) pieces <- c(pieces, "")
            for (q in seq_along(pieces)) {
              if (q > 1L) paras[[length(paras) + 1L]] <- paras[[1L]]
              cur <- length(paras)
              if (q > 1L) paras[[cur]] <- list(algn = "l", text = character(), size = numeric(), font = integer(), col = character(), rise = numeric())
              paras[[cur]]$text <- c(paras[[cur]]$text, pieces[q])
              paras[[cur]]$size <- c(paras[[cur]]$size, sz)
              paras[[cur]]$font <- c(paras[[cur]]$font, if (bold) 2L else 1L)
              paras[[cur]]$col <- c(paras[[cur]]$col, "#000000")
              paras[[cur]]$rise <- c(paras[[cur]]$rise, 0)
            }
          }
          items[[length(items) + 1L]] <- list(
            ref = ooxml_attr(cd, k, "ref"), paras = paras, fill = "#FFFFE1", col = "#000000",
            size = box_size(ooxml_attr(cd, k, "ref"), c(144, 79))
          )
        }
      }

      if (length(items) == 0L) stop("the sheet has no comments", call. = FALSE)
      pos <- t(vapply(items, function(it) cell(it$ref), c(0, 0)))
      items <- items[order(pos[, 1L], pos[, 2L])]
      if (n > length(items)) stop("no comment number ", n, " found", call. = FALSE)
      it <- items[[n]]
      bw <- it$size[1L]
      bh <- it$size[2L]
      shp <- list(list(
        type = "path", fill = it$fill, col = it$col, lwd = 0.75, lty = "solid", lend = "round",
        ljoin = "round", lmitre = 10,
        rings = list(list(x = c(0, bw, bw, 0), y = c(0, 0, bh, bh), closed = TRUE))
      ))
      if (is.null(it$msgs)) {
        shp[[2L]] <- list(
          type = "text", box = c(0, 0, bw, bh), rot = 0, trect = c(0, 0, bw, bh), anchor = "t",
          wrap = TRUE, fit = 1, ins = c(5, 3, 5, 3), paras = it$paras
        )
      } else {
        for (q in seq_along(it$msgs)) {
          ms <- it$msgs[[q]]
          if (q > 1L) {
            shp[[length(shp) + 1L]] <- list(
              type = "path", fill = NA_character_, col = "#E1DFDD", lwd = 0.75, lty = "solid", lend = "butt",
              ljoin = "round", lmitre = 10,
              rings = list(list(x = c(0, bw), y = c(ms$y, ms$y), closed = FALSE))
            )
          }
          shp[[length(shp) + 1L]] <- list(
            type = "text", box = c(ms$indent, ms$y, bw - ms$indent, ms$h), rot = 0,
            trect = c(0, 0, bw - ms$indent, ms$h), anchor = "t", wrap = TRUE, fit = 1,
            ins = c(5, 8, 5, 3), paras = ms$paras
          )
        }
      }
      return(structure(list(width = bw, height = bh, shapes = shp), class = "easel_drawing"))
    }

    srel_path <- paste0(dirname(sheet_path), "/_rels/", basename(sheet_path), ".rels")
    if (!srel_path %in% entries) stop("the sheet has no drawing", call. = FALSE)
    srels <- ooxml_parse(rd(srel_path))
    rel <- which(srels$name == "Relationship")
    rel <- rel[grepl("/drawing$", vapply(rel, function(i) ooxml_attr(srels, i, "Type"), ""))]
    if (length(rel) == 0L) stop("the sheet has no drawing", call. = FALSE)
    drawing_path <- resolve(dirname(sheet_path), ooxml_attr(srels, rel[1L], "Target"))
    if (!drawing_path %in% entries) stop("the drawing part is missing from the workbook", call. = FALSE)
    xml <- rd(drawing_path)
    if (!xlsb) sheet_xml <- function(max_row) rd(sheet_path)
    if (xlsb) {
      sheet_xml <- function(max_row) {
        sheet_file <- unzip(file, files = sheet_path, exdir = td)
        bs <- readBin(sheet_file, "raw", n = file.size(sheet_file))
        fmt_xml <- ""
        cols_xml <- character()
        rows_xml <- character()
        pos <- 1L
        while (pos <= length(bs)) {
          id <- as.integer(bs[pos])
          pos <- pos + 1L
          if (id >= 128L) {
            id <- (id - 128L) + (as.integer(bs[pos]) %% 128L) * 128L
            pos <- pos + 1L
          }
          len <- 0L
          mul <- 1L
          for (k in 1:4) {
            b <- as.integer(bs[pos])
            pos <- pos + 1L
            len <- len + (b %% 128L) * mul
            mul <- mul * 128L
            if (b < 128L) break
          }
          rec <- bs[pos + seq_len(len) - 1L]
          if (id == 485L) {
            dxg <- readBin(rec[1:4], "integer", size = 4L)
            base_w <- readBin(rec[5:6], "integer", size = 2L, signed = FALSE)
            row_h <- readBin(rec[7:8], "integer", size = 2L, signed = FALSE)
            fmt_xml <- paste0(
              "<sheetFormatPr baseColWidth=\"", base_w, "\"",
              if (dxg > 0L) paste0(" defaultColWidth=\"", dxg / 256, "\""),
              " defaultRowHeight=\"", row_h / 20, "\"/>"
            )
          } else if (id == 60L) {
            first <- readBin(rec[1:4], "integer", size = 4L)
            last <- readBin(rec[5:8], "integer", size = 4L)
            dx <- readBin(rec[9:12], "integer", size = 4L)
            cols_xml <- c(cols_xml, paste0("<col min=\"", first + 1L, "\" max=\"", last + 1L, "\" width=\"", dx / 256, "\"/>"))
          } else if (id == 0L) {
            rw <- readBin(rec[1:4], "integer", size = 4L)
            if (rw > max_row) break
            ht <- readBin(rec[9:10], "integer", size = 2L, signed = FALSE)
            rows_xml <- c(rows_xml, paste0("<row r=\"", rw + 1L, "\" ht=\"", ht / 20, "\"/>"))
          }
          pos <- pos + len
        }
        paste0(fmt_xml, paste(cols_xml, collapse = ""), paste(rows_xml, collapse = ""))
      }
    }
    drel_path <- paste0(dirname(drawing_path), "/_rels/", basename(drawing_path), ".rels")
    drels <- if (drel_path %in% entries) ooxml_parse(rd(drel_path)) else NULL
    target_of <- function(rid) {
      if (is.null(drels) || is.na(rid)) return(NULL)
      rel <- which(drels$name == "Relationship")
      rel <- rel[vapply(rel, function(i) identical(ooxml_attr(drels, i, "Id"), rid), NA)]
      if (length(rel) == 0L) return(NULL)
      path <- resolve(dirname(drawing_path), ooxml_attr(drels, rel[1L], "Target"))
      if (!path %in% entries) return(NULL)
      path
    }
    slicer_of <- function(name) {
      cache <- caption <- NA_character_
      rowh <- 241300
      ncol <- 1
      for (pt in entries[grepl("^xl/slicers/slicer[0-9]+\\.xml$", entries)]) {
        sd <- ooxml_parse(rd(pt))
        for (k in which(sd$name == "slicer")) {
          if (!identical(ooxml_attr(sd, k, "name"), name)) next
          cache <- ooxml_attr(sd, k, "cache")
          caption <- ooxml_attr(sd, k, "caption")
          rh <- as.numeric(ooxml_attr(sd, k, "rowHeight"))
          if (!is.na(rh)) rowh <- rh
          cc <- as.numeric(ooxml_attr(sd, k, "columnCount"))
          if (!is.na(cc)) ncol <- cc
        }
      }
      if (is.na(cache)) return(NULL)
      src <- pcid <- NA_character_
      idx <- sel <- numeric()
      for (pt in entries[grepl("^xl/slicerCaches/slicerCache[0-9]+\\.xml$", entries)]) {
        cd <- ooxml_parse(rd(pt))
        root <- which(cd$name == "slicerCacheDefinition")[1L]
        if (!identical(ooxml_attr(cd, root, "name"), cache)) next
        src <- ooxml_attr(cd, root, "sourceName")
        tab <- which(cd$name == "tabular")[1L]
        pcid <- ooxml_attr(cd, tab, "pivotCacheId")
        its <- ooxml_kids(cd, ooxml_kids(cd, tab, "items")[1L], "i")
        idx <- as.numeric(vapply(its, function(j) ooxml_attr(cd, j, "x"), ""))
        sel <- vapply(its, function(j) identical(ooxml_attr(cd, j, "s"), "1"), NA)
      }
      if (is.na(pcid) || length(idx) == 0L) return(NULL)
      labels <- NULL
      for (pt in entries[grepl("^xl/pivotCache/pivotCacheDefinition[0-9]+\\.xml$", entries)]) {
        pd <- ooxml_parse(rd(pt))
        ext <- which(pd$name == "pivotCacheDefinition" & !is.na(vapply(seq_along(pd$name), function(j) ooxml_attr(pd, j, "pivotCacheId"), "")))
        if (!any(vapply(ext, function(j) identical(ooxml_attr(pd, j, "pivotCacheId"), pcid), NA))) next
        for (f in which(pd$name == "cacheField")) {
          if (!identical(ooxml_attr(pd, f, "name"), src)) next
          si <- ooxml_kids(pd, ooxml_kids(pd, f, "sharedItems")[1L], c("s", "n", "d", "b", "e", "m"))
          labels <- vapply(si, function(j) {
            v <- ooxml_attr(pd, j, "v")
            if (is.na(v)) "" else v
          }, "")
        }
      }
      if (is.null(labels)) return(NULL)
      ok <- idx + 1 <= length(labels)
      list(
        caption = if (is.na(caption)) name else caption, items = labels[idx[ok] + 1],
        selected = sel[ok], ncol = ncol, rowh = rowh
      )
    }
    diagram_of <- function(rid) {
      dm <- target_of(rid)
      if (is.null(dm)) return(NULL)
      dd <- ooxml_parse(rd(dm))
      dpath <- target_of(ooxml_attr(dd, which(dd$name == "dataModelExt")[1L], "relId"))
      if (is.null(dpath)) return(NULL)
      ooxml_parse(rd(dpath))
    }
    chart_parts <- sort(entries[grepl("^xl/charts/chart(Ex)?[0-9]+\\.xml$", entries)])
    chart_row <- function(path) {
      key <- as.numeric(sub("^.*chart(Ex)?([0-9]+)\\.xml$", "\\2", chart_parts))
      mine <- as.numeric(sub("^.*chart(Ex)?([0-9]+)\\.xml$", "\\2", path))
      match(mine, sort(unique(key)))
    }
    media_of <- function(rid) {
      path <- target_of(rid)
      if (is.null(path)) return(NULL)
      ext <- tolower(sub("^.*\\.", "", path))
      if (ext == "png" && requireNamespace("png", quietly = TRUE)) {
        return(png::readPNG(unzip(file, files = path, exdir = td)))
      }
      if (ext %in% c("jpg", "jpeg") && requireNamespace("jpeg", quietly = TRUE)) {
        return(jpeg::readJPEG(unzip(file, files = path, exdir = td)))
      }
      NULL
    }
  } else {
    xml <- paste(readLines(file, warn = FALSE, encoding = "UTF-8"), collapse = "")
  }

  dom <- ooxml_parse(xml)
  anchors <- which(dom$name %in% c("absoluteAnchor", "oneCellAnchor", "twoCellAnchor"))
  fallback <- vapply(anchors, function(i) {
    while (i > 0L) {
      if (dom$name[i] == "Fallback") return(TRUE)
      i <- dom$parent[i]
    }
    FALSE
  }, NA)
  anchors <- anchors[!fallback]
  if (n > length(anchors)) stop("no drawing number ", n, " found", call. = FALSE)
  anchor <- anchors[n]

  anchor_size <- function() {
    aext <- ooxml_kids(dom, anchor, "ext")[1L]
    if (!is.na(aext)) {
      return(c(as.numeric(ooxml_attr(dom, aext, "cx")), as.numeric(ooxml_attr(dom, aext, "cy"))))
    }
    from <- ooxml_kids(dom, anchor, "from")[1L]
    to <- ooxml_kids(dom, anchor, "to")[1L]
    if (is.na(from) || is.na(to)) return(NULL)
    pos <- function(node) {
      as.numeric(vapply(c("col", "colOff", "row", "rowOff"), function(nm) {
        dom$text[ooxml_kids(dom, node, nm)[1L]]
      }, ""))
    }
    f <- pos(from)
    t <- pos(to)
    if (anyNA(c(f, t))) return(NULL)
    sx_ <- sheet_xml(max(f[3L], t[3L]))
    if (is.null(sx_)) return(NULL)

    fmt <- regmatches(sx_, regexpr("<sheetFormatPr[^>]*>", sx_))
    dcw <- as.numeric(sub("^.* defaultColWidth=\"([^\"]*)\".*$", "\\1", fmt[grepl("defaultColWidth", fmt)]))
    if (length(dcw) == 0L) {
      bcw <- as.numeric(sub("^.* baseColWidth=\"([^\"]*)\".*$", "\\1", fmt[grepl("baseColWidth", fmt)]))
      dcw <- (if (length(bcw) == 0L) 8 else bcw) + 5 / 7
    }
    drh <- as.numeric(sub("^.* defaultRowHeight=\"([^\"]*)\".*$", "\\1", fmt[grepl("defaultRowHeight", fmt)]))
    if (length(drh) == 0L) drh <- 15

    cw <- rep(round(dcw * 7) * 9525, max(f[1L], t[1L]) + 1L)
    for (cl in regmatches(sx_, gregexpr("<col [^>]*>", sx_))[[1L]]) {
      lo <- suppressWarnings(as.numeric(sub("^.* min=\"([0-9]+)\".*$", "\\1", cl)))
      hi <- suppressWarnings(as.numeric(sub("^.* max=\"([0-9]+)\".*$", "\\1", cl)))
      wd <- suppressWarnings(as.numeric(sub("^.* width=\"([^\"]*)\".*$", "\\1", cl)))
      if (anyNA(c(lo, hi, wd)) || lo > length(cw)) next
      cw[lo:min(hi, length(cw))] <- round(wd * 7) * 9525
    }
    rh <- rep(drh * 12700, max(f[3L], t[3L]) + 1L)
    for (rw in regmatches(sx_, gregexpr("<row [^>]*>", sx_))[[1L]]) {
      r <- suppressWarnings(as.numeric(sub("^.* r=\"([0-9]+)\".*$", "\\1", rw)))
      ht <- as.numeric(sub("^.* ht=\"([^\"]*)\".*$", "\\1", rw[grepl(" ht=", rw, fixed = TRUE)]))
      if (!is.na(r) && length(ht) == 1L && r <= length(rh)) rh[r] <- ht * 12700
    }
    w <- sum(cw[seq_len(t[1L])]) - sum(cw[seq_len(f[1L])]) - f[2L] + t[2L]
    h <- sum(rh[seq_len(t[3L])]) - sum(rh[seq_len(f[3L])]) - f[4L] + t[4L]
    c(w, h)
  }

  acc <- new.env(parent = emptyenv())
  acc$shapes <- list()

  walk <- function(i, sx, sy, tx, ty) {
    if (dom$name[i] == "grpSp") {
      xf <- ooxml_kids(dom, ooxml_kids(dom, i, "grpSpPr")[1L], "xfrm")[1L]
      if (!is.na(xf)) {
        g_off <- ooxml_kids(dom, xf, "off")[1L]
        g_ext <- ooxml_kids(dom, xf, "ext")[1L]
        g_choff <- ooxml_kids(dom, xf, "chOff")[1L]
        g_chext <- ooxml_kids(dom, xf, "chExt")[1L]
        gx <- sx * as.numeric(ooxml_attr(dom, g_ext, "cx")) / as.numeric(ooxml_attr(dom, g_chext, "cx"))
        gy <- sy * as.numeric(ooxml_attr(dom, g_ext, "cy")) / as.numeric(ooxml_attr(dom, g_chext, "cy"))
        if (!is.finite(gx)) gx <- sx
        if (!is.finite(gy)) gy <- sy
        tx <- sx * as.numeric(ooxml_attr(dom, g_off, "x")) + tx - gx * as.numeric(ooxml_attr(dom, g_choff, "x"))
        ty <- sy * as.numeric(ooxml_attr(dom, g_off, "y")) + ty - gy * as.numeric(ooxml_attr(dom, g_choff, "y"))
        sx <- gx
        sy <- gy
      }
      for (k in ooxml_kids(dom, i, c("sp", "cxnSp", "pic", "graphicFrame", "grpSp"))) walk(k, sx, sy, tx, ty)
      return(invisible(NULL))
    }

    if (dom$name[i] == "graphicFrame") {
      gd <- ooxml_kids(dom, ooxml_kids(dom, i, "graphic")[1L], "graphicData")[1L]
      cn <- ooxml_kids(dom, gd, "chart")[1L]
      sl <- ooxml_kids(dom, gd, "slicer")[1L]
      path <- if (is.na(cn)) NULL else target_of(ooxml_attr(dom, cn, "id"))
      info <- if (is.na(sl)) NULL else slicer_of(ooxml_attr(dom, sl, "name"))
      dg <- ooxml_kids(dom, gd, "relIds")[1L]
      ddom <- if (is.na(dg)) NULL else diagram_of(ooxml_attr(dom, dg, "dm"))
      if ((is.null(path) && is.null(info) && is.null(ddom)) || is.null(file_path)) return(invisible(NULL))
      xf <- ooxml_kids(dom, i, "xfrm")[1L]
      off <- ooxml_kids(dom, xf, "off")[1L]
      ext <- ooxml_kids(dom, xf, "ext")[1L]
      ex <- as.numeric(ooxml_attr(dom, ext, "cx"))
      ey <- as.numeric(ooxml_attr(dom, ext, "cy"))
      gx0 <- as.numeric(ooxml_attr(dom, off, "x")) * sx + tx
      gy0 <- as.numeric(ooxml_attr(dom, off, "y")) * sy + ty
      gw <- ex * sx
      gh <- ey * sy
      if (is.na(xf) || isTRUE(ex == 0 && ey == 0)) {
        asz <- anchor_size()
        if (is.null(asz) || anyNA(asz)) return(invisible(NULL))
        gx0 <- 0
        gy0 <- 0
        gw <- asz[1L]
        gh <- asz[2L]
      }
      if (!is.null(ddom)) {
        saved <- dom
        assign("dom", ddom, envir = environment(walk))
        for (k in which(ddom$name == "sp")) walk(k, 1, 1, gx0, gy0)
        assign("dom", saved, envir = environment(walk))
        return(invisible(NULL))
      }
      if (!is.null(path)) {
        acc$shapes[[length(acc$shapes) + 1L]] <- list(
          type = "chart", box = c(gx0, gy0, gw, gh), rot = 0, file = file_path, row = chart_row(path)
        )
        return(invisible(NULL))
      }

      accent <- if ("accent1" %in% names(theme)) paste0("#", theme[["accent1"]]) else "#4472C4"
      light <- paste0("#", paste(sprintf("%02X", round(255 - (255 - grDevices::col2rgb(accent)[, 1L]) * 0.2)), collapse = ""))
      pad <- 3 * 12700
      pshape <- function(x0_, y0_, x1_, y1_, fill_, col_) {
        list(
          type = "path", fill = fill_, col = col_, lwd = 0.75, lty = "solid", lend = "round", ljoin = "round", lmitre = 10,
          rings = list(list(x = c(x0_, x1_, x1_, x0_), y = c(y0_, y0_, y1_, y1_), closed = TRUE))
        )
      }
      tshape <- function(x0_, y0_, w_, h_, label, bold, col_, algn_) {
        list(
          type = "text", box = c(x0_, y0_, w_, h_), rot = 0, trect = c(0, 0, w_, h_), anchor = "ctr",
          wrap = FALSE, ins = c(3.6, 1.8, 3.6, 1.8),
          paras = list(list(algn = algn_, text = label, size = 11, font = if (bold) 2L else 1L, col = col_, rise = 0))
        )
      }
      acc$shapes[[length(acc$shapes) + 1L]] <- pshape(gx0, gy0, gx0 + gw, gy0 + gh, "#FFFFFF", "#BFBFBF")
      acc$shapes[[length(acc$shapes) + 1L]] <- tshape(gx0 + pad, gy0 + pad, gw - 2 * pad, info$rowh, info$caption, TRUE, "#000000", "l")
      iw <- (gw - 2 * pad - (info$ncol - 1) * pad) / info$ncol
      for (k in seq_along(info$items)) {
        cx_ <- gx0 + pad + ((k - 1) %% info$ncol) * (iw + pad)
        cy_ <- gy0 + pad + info$rowh + pad + ((k - 1) %/% info$ncol) * (info$rowh + pad)
        if (cy_ + info$rowh > gy0 + gh) break
        acc$shapes[[length(acc$shapes) + 1L]] <- pshape(
          cx_, cy_, cx_ + iw, cy_ + info$rowh,
          if (info$selected[k]) light else "#F2F2F2", if (info$selected[k]) accent else "#D9D9D9"
        )
        acc$shapes[[length(acc$shapes) + 1L]] <- tshape(
          cx_, cy_, iw, info$rowh, info$items[k], FALSE,
          if (info$selected[k]) "#000000" else "#A6A6A6", "l"
        )
      }
      return(invisible(NULL))
    }

    sppr <- ooxml_kids(dom, i, "spPr")[1L]
    xf <- ooxml_kids(dom, sppr, "xfrm")[1L]
    no_xf <- is.na(sppr) || is.na(xf)
    if (no_xf && dom$name[i] != "pic") return(invisible(NULL))
    off <- ooxml_kids(dom, xf, "off")[1L]
    ext <- ooxml_kids(dom, xf, "ext")[1L]
    ex <- as.numeric(ooxml_attr(dom, ext, "cx"))
    ey <- as.numeric(ooxml_attr(dom, ext, "cy"))
    x0 <- as.numeric(ooxml_attr(dom, off, "x")) * sx + tx
    y0 <- as.numeric(ooxml_attr(dom, off, "y")) * sy + ty
    w <- ex * sx
    h <- ey * sy
    if (no_xf || isTRUE(ex == 0 && ey == 0)) {
      asz <- anchor_size()
      if (is.null(asz) || anyNA(asz)) return(invisible(NULL))
      x0 <- 0
      y0 <- 0
      w <- asz[1L]
      h <- asz[2L]
      ex <- w
      ey <- h
    }
    rot <- as.numeric(ooxml_attr(dom, xf, "rot"))
    rot <- if (is.na(rot)) 0 else rot / 60000
    flip_h <- isTRUE(ooxml_attr(dom, xf, "flipH") %in% c("1", "true"))
    flip_v <- isTRUE(ooxml_attr(dom, xf, "flipV") %in% c("1", "true"))
    bcx <- x0 + w / 2
    bcy <- y0 + h / 2

    if (dom$name[i] == "pic") {
      bf <- ooxml_kids(dom, i, "blipFill")[1L]
      blip <- ooxml_kids(dom, bf, "blip")[1L]
      img <- media_of(ooxml_attr(dom, blip, "embed"))
      if (is.null(img)) {
        acc$shapes[[length(acc$shapes) + 1L]] <- list(
          type = "path", fill = "#F2F2F2", col = "#BFBFBF", lwd = 0.75, lty = "solid", lend = "round", ljoin = "round", lmitre = 10,
          rings = list(list(x = c(x0, x0 + w, x0 + w, x0), y = c(y0, y0, y0 + h, y0 + h), closed = TRUE))
        )
        return(invisible(NULL))
      }
      src <- ooxml_kids(dom, bf, "srcRect")[1L]
      crop <- as.numeric(c(
        ooxml_attr(dom, src, "l"), ooxml_attr(dom, src, "t"),
        ooxml_attr(dom, src, "r"), ooxml_attr(dom, src, "b")
      )) / 100000
      crop[is.na(crop)] <- 0
      if (length(dim(img)) == 2L) img <- array(img, c(dim(img), 1L))
      nplane <- dim(img)[3L]
      if (nplane < 3L) {
        img <- array(
          c(rep(img[, , 1L], 3L), if (nplane == 2L) img[, , 2L]),
          c(dim(img)[1:2], 3L + (nplane == 2L))
        )
      }
      nr <- dim(img)[1L]
      nc <- dim(img)[2L]
      rows <- seq(floor(crop[2L] * nr) + 1L, max(floor((1 - crop[4L]) * nr), 1L))
      cols <- seq(floor(crop[1L] * nc) + 1L, max(floor((1 - crop[3L]) * nc), 1L))
      img <- img[rows, cols, , drop = FALSE]
      if (flip_h) img <- img[, rev(seq_len(dim(img)[2L])), , drop = FALSE]
      if (flip_v) img <- img[rev(seq_len(dim(img)[1L])), , , drop = FALSE]
      acc$shapes[[length(acc$shapes) + 1L]] <- list(type = "image", box = c(x0, y0, w, h), rot = rot, img = img)
      return(invisible(NULL))
    }

    # geometry, in absolute EMU
    rings <- list()
    cg <- ooxml_kids(dom, sppr, "custGeom")[1L]
    pg <- ooxml_kids(dom, sppr, "prstGeom")[1L]
    av <- if (is.na(pg)) integer() else ooxml_kids(dom, ooxml_kids(dom, pg, "avLst")[1L], "gd")
    adj <- as.numeric(sub("^val ", "", vapply(av, function(g) ooxml_attr(dom, g, "fmla"), "")))
    names(adj) <- vapply(av, function(g) ooxml_attr(dom, g, "name"), "")
    prst <- if (!is.na(cg)) "custom" else if (is.na(pg)) "rect" else ooxml_attr(dom, pg, "prst")
    tr <- if (is.na(cg)) ooxml_textrect(prst, w, h, adj) else c(0, 0, w, h)
    if (flip_h) tr[c(1L, 3L)] <- w - tr[c(3L, 1L)]
    if (flip_v) tr[c(2L, 4L)] <- h - tr[c(4L, 2L)]
    if (!is.na(cg)) {
      for (p in ooxml_kids(dom, ooxml_kids(dom, cg, "pathLst")[1L], "path")) {
        pw <- as.numeric(ooxml_attr(dom, p, "w"))
        ph <- as.numeric(ooxml_attr(dom, p, "h"))
        if (is.na(pw) || pw == 0) pw <- ex
        if (is.na(ph) || ph == 0) ph <- ey
        cur <- NULL
        for (op in dom$kids[[as.character(p)]]) {
          opn <- dom$name[op]
          pts <- ooxml_kids(dom, op, "pt")
          px <- x0 + as.numeric(sub("^.* x=\"([^\"]*)\".*$", "\\1", dom$attr[pts])) / pw * w
          py <- y0 + as.numeric(sub("^.* y=\"([^\"]*)\".*$", "\\1", dom$attr[pts])) / ph * h
          if (opn == "moveTo") {
            if (!is.null(cur)) rings[[length(rings) + 1L]] <- cur
            cur <- list(x = px[1L], y = py[1L], closed = FALSE)
          } else if (opn == "lnTo" && !is.null(cur)) {
            cur$x <- c(cur$x, px[1L])
            cur$y <- c(cur$y, py[1L])
          } else if (opn == "cubicBezTo" && !is.null(cur) && length(pts) == 3L) {
            t <- (1:16) / 16
            p0x <- cur$x[length(cur$x)]
            p0y <- cur$y[length(cur$y)]
            cur$x <- c(cur$x, (1 - t)^3 * p0x + 3 * (1 - t)^2 * t * px[1L] +
                         3 * (1 - t) * t^2 * px[2L] + t^3 * px[3L])
            cur$y <- c(cur$y, (1 - t)^3 * p0y + 3 * (1 - t)^2 * t * py[1L] +
                         3 * (1 - t) * t^2 * py[2L] + t^3 * py[3L])
          } else if (opn == "quadBezTo" && !is.null(cur) && length(pts) == 2L) {
            t <- (1:16) / 16
            p0x <- cur$x[length(cur$x)]
            p0y <- cur$y[length(cur$y)]
            cur$x <- c(cur$x, (1 - t)^2 * p0x + 2 * (1 - t) * t * px[1L] + t^2 * px[2L])
            cur$y <- c(cur$y, (1 - t)^2 * p0y + 2 * (1 - t) * t * py[1L] + t^2 * py[2L])
          } else if (opn == "close" && !is.null(cur)) {
            cur$closed <- TRUE
          }
        }
        if (!is.null(cur)) rings[[length(rings) + 1L]] <- cur
      }
    } else {
      if (identical(prst, "ellipse")) {
        t <- seq(0, 2 * pi, length.out = 73L)[-73L]
        rings[[1L]] <- list(x = bcx + w / 2 * cos(t), y = bcy + h / 2 * sin(t), closed = TRUE)
      } else if (prst %in% c("line", "straightConnector1")) {
        rings[[1L]] <- list(x = c(x0, x0 + w), y = c(y0, y0 + h), closed = FALSE)
      } else {
        pr <- ooxml_preset(prst, w, h, adj)
        if (is.null(pr)) pr <- ooxml_preset("rect", w, h, adj)
        for (r in pr) {
          r$x <- x0 + r$x
          r$y <- y0 + r$y
          rings[[length(rings) + 1L]] <- r
        }
      }
    }

    a <- rot * pi / 180
    for (k in seq_along(rings)) {
      rx <- rings[[k]]$x
      ry <- rings[[k]]$y
      if (flip_h) rx <- 2 * bcx - rx
      if (flip_v) ry <- 2 * bcy - ry
      if (rot != 0) {
        dx <- rx - bcx
        dy <- ry - bcy
        rx <- bcx + dx * cos(a) - dy * sin(a)
        ry <- bcy + dx * sin(a) + dy * cos(a)
      }
      rings[[k]]$x <- rx
      rings[[k]]$y <- ry
    }

    # fill
    fill <- NA_character_
    fnode <- ooxml_kids(dom, sppr, c("noFill", "solidFill", "gradFill"))[1L]
    st <- ooxml_kids(dom, i, "style")[1L]
    fill_ref <- ooxml_kids(dom, st, "fillRef")[1L]
    ln_ref <- ooxml_kids(dom, st, "lnRef")[1L]
    font_ref <- ooxml_kids(dom, st, "fontRef")[1L]
    if (is.na(fnode) && !is.na(fill_ref) && !identical(ooxml_attr(dom, fill_ref, "idx"), "0")) {
      fill <- ooxml_color(dom, fill_ref, theme)
    } else if (!is.na(fnode) && dom$name[fnode] == "solidFill") {
      fill <- ooxml_color(dom, fnode, theme)
    } else if (!is.na(fnode) && dom$name[fnode] == "gradFill") {
      gs <- ooxml_kids(dom, ooxml_kids(dom, fnode, "gsLst")[1L], "gs")
      fill <- list(
        stops = vapply(gs, function(g) as.numeric(ooxml_attr(dom, g, "pos")), 0) / 100000,
        cols = vapply(gs, function(g) ooxml_color(dom, g, theme), "")
      )
      lin <- ooxml_kids(dom, fnode, "lin")[1L]
      if (!is.na(lin)) {
        fill$kind <- "linear"
        fill$ang <- as.numeric(ooxml_attr(dom, lin, "ang")) / 60000
      } else {
        ftr <- ooxml_kids(dom, ooxml_kids(dom, fnode, "path")[1L], "fillToRect")[1L]
        fill$kind <- "radial"
        fill$focus <- c(as.numeric(ooxml_attr(dom, ftr, "l")), as.numeric(ooxml_attr(dom, ftr, "t"))) / 100000
        if (anyNA(fill$focus)) fill$focus <- c(0.5, 0.5)
      }
    }

    # line
    col <- NA_character_
    lwd <- 1
    lty <- "solid"
    lend <- "round"
    ljoin <- "round"
    lmitre <- 10
    ln <- ooxml_kids(dom, sppr, "ln")[1L]
    ln_idx <- as.numeric(ooxml_attr(dom, ln_ref, "idx"))
    if (!is.na(ln_idx) && ln_idx > 0 && (is.na(ln) || length(ooxml_kids(dom, ln, c("noFill", "solidFill", "gradFill"))) == 0L)) {
      col <- ooxml_color(dom, ln_ref, theme)
      lwd <- c(9525, 25400, 38100)[min(ln_idx, 3)] / 12700 / 0.75
    }
    if (!is.na(ln)) {
      lfill <- ooxml_kids(dom, ln, c("noFill", "solidFill", "gradFill"))[1L]
      if (!is.na(lfill) && dom$name[lfill] == "solidFill") {
        col <- ooxml_color(dom, lfill, theme)
      } else if (!is.na(lfill) && dom$name[lfill] == "gradFill") {
        col <- ooxml_color(dom, ooxml_kids(dom, ooxml_kids(dom, lfill, "gsLst")[1L], "gs")[1L], theme)
      }
      lw <- as.numeric(ooxml_attr(dom, ln, "w"))
      if (!is.na(lw)) lwd <- lw / 12700 / 0.75
      cap <- ooxml_attr(dom, ln, "cap")
      if (identical(cap, "flat")) lend <- "butt"
      if (identical(cap, "sq")) lend <- "square"
      if (length(ooxml_kids(dom, ln, "bevel")) > 0L) ljoin <- "bevel"
      mit <- ooxml_kids(dom, ln, "miter")[1L]
      if (!is.na(mit)) {
        ljoin <- "mitre"
        lim <- as.numeric(ooxml_attr(dom, mit, "lim"))
        if (!is.na(lim)) lmitre <- lim / 100000
      }
      cd <- ooxml_kids(dom, ln, "custDash")[1L]
      pd <- ooxml_kids(dom, ln, "prstDash")[1L]
      if (!is.na(cd)) {
        nib <- unlist(lapply(ooxml_kids(dom, cd, "ds"), function(q) {
          c(as.numeric(ooxml_attr(dom, q, "d")), as.numeric(ooxml_attr(dom, q, "sp")))
        })) / 100000
        lty <- paste(sprintf("%X", as.integer(pmin(pmax(round(nib), 1), 15))), collapse = "")
      } else if (!is.na(pd)) {
        lty <- switch(ooxml_attr(dom, pd, "val"),
          dash = "44", sysDash = "33", dot = "13", sysDot = "11", dashDot = "4313",
          sysDashDot = "3313", lgDash = "74", lgDashDot = "7413", "solid")
      }
    }

    if (is.list(fill) || !is.na(fill) || !is.na(col)) {
      acc$shapes[[length(acc$shapes) + 1L]] <- list(
        type = "path", rings = rings, fill = fill,
        col = col, lwd = lwd, lty = lty, lend = lend, ljoin = ljoin, lmitre = lmitre
      )
    }

    # text
    tb <- ooxml_kids(dom, i, "txBody")[1L]
    if (is.na(tb)) return(invisible(NULL))
    bp <- ooxml_kids(dom, tb, "bodyPr")[1L]
    ins <- as.numeric(c(
      ooxml_attr(dom, bp, "lIns"), ooxml_attr(dom, bp, "tIns"),
      ooxml_attr(dom, bp, "rIns"), ooxml_attr(dom, bp, "bIns")
    ))
    ins[is.na(ins)] <- c(91440, 45720, 91440, 45720)[is.na(ins)]
    anchor_v <- ooxml_attr(dom, bp, "anchor")
    font_col <- if (is.na(font_ref)) NA_character_ else ooxml_color(dom, font_ref, theme)
    if (is.na(font_col)) font_col <- "#000000"
    paras <- list()
    for (pp in ooxml_kids(dom, tb, "p")) {
      algn <- ooxml_attr(dom, ooxml_kids(dom, pp, "pPr")[1L], "algn")
      rs <- ooxml_kids(dom, pp, "r")
      ac <- ooxml_kids(dom, pp, "AlternateContent")[1L]
      mnode <- ooxml_kids(dom, ooxml_kids(dom, ac, "Choice")[1L], "m")[1L]
      if (!is.na(mnode)) {
        para_m <- ooxml_kids(dom, mnode, "oMathPara")[1L]
        omath <- ooxml_kids(dom, if (is.na(para_m)) mnode else para_m, "oMath")
        szs <- as.numeric(vapply(
          which(dom$name == "rPr" & dom$parent %in% which(dom$name == "ctrlPr")),
          function(j) ooxml_attr(dom, j, "sz"), ""
        ))
        szs <- szs[!is.na(szs)]
        jc <- ooxml_attr(dom, ooxml_kids(dom, ooxml_kids(dom, para_m, "oMathParaPr")[1L], "jc")[1L], "val")
        src <- paste(vapply(omath, function(j) ooxml_math(dom, j), ""), collapse = ", ")
        expr <- tryCatch(parse(text = paste0("paste(", src, ")"))[[1L]], error = function(e) NULL)
        if (!is.null(expr)) {
          paras[[length(paras) + 1L]] <- list(
            algn = if (is.na(para_m) || identical(jc, "left")) "l" else if (identical(jc, "right")) "r" else "ctr",
            text = "", size = if (length(szs) > 0L) szs[1L] / 100 else 11, font = 1L, col = font_col, rise = 0,
            expr = expr
          )
        }
        next
      }
      if (length(rs) == 0L) next
      label <- character(length(rs))
      size <- numeric(length(rs))
      face <- integer(length(rs))
      rcol <- character(length(rs))
      rise <- numeric(length(rs))
      for (k in seq_along(rs)) {
        rpr <- ooxml_kids(dom, rs[k], "rPr")[1L]
        tn <- ooxml_kids(dom, rs[k], "t")[1L]
        lab <- if (is.na(tn)) "" else dom$text[tn]
        lab <- gsub("&lt;", "<", lab, fixed = TRUE)
        lab <- gsub("&gt;", ">", lab, fixed = TRUE)
        lab <- gsub("&quot;", "\"", lab, fixed = TRUE)
        lab <- gsub("&apos;", "'", lab, fixed = TRUE)
        label[k] <- gsub("&amp;", "&", lab, fixed = TRUE)
        sz <- as.numeric(ooxml_attr(dom, rpr, "sz"))
        size[k] <- if (is.na(sz)) 11 else sz / 100
        face[k] <- 1L + isTRUE(ooxml_attr(dom, rpr, "b") %in% c("1", "true")) +
          2L * isTRUE(ooxml_attr(dom, rpr, "i") %in% c("1", "true"))
        sf <- ooxml_kids(dom, rpr, "solidFill")[1L]
        rcol[k] <- if (!is.na(sf)) ooxml_color(dom, sf, theme) else font_col
        bl <- as.numeric(ooxml_attr(dom, rpr, "baseline"))
        rise[k] <- if (is.na(bl)) 0 else bl / 100000 * size[k]
      }
      paras[[length(paras) + 1L]] <- list(
        algn = if (is.na(algn)) "l" else algn,
        text = label, size = size, font = face, col = rcol, rise = rise
      )
    }
    if (length(paras) > 0L && (any(nzchar(unlist(lapply(paras, `[[`, "text")))) || any(vapply(paras, function(p) !is.null(p$expr), NA)))) {
      acc$shapes[[length(acc$shapes) + 1L]] <- list(
        type = "text", box = c(x0, y0, w, h), rot = rot,
        trect = c((tr[1L] + tr[3L]) / 2 - w / 2, (tr[2L] + tr[4L]) / 2 - h / 2, tr[3L] - tr[1L], tr[4L] - tr[2L]),
        anchor = if (is.na(anchor_v)) "t" else anchor_v,
        wrap = !identical(ooxml_attr(dom, bp, "wrap"), "none"),
        ins = ins / 12700, paras = paras
      )
    }
    invisible(NULL)
  }

  top <- ooxml_kids(dom, anchor, "grpSp")[1L]
  for (k in ooxml_kids(dom, anchor, c("grpSp", "sp", "cxnSp", "pic", "graphicFrame"))) walk(k, 1, 1, 0, 0)
  ac <- ooxml_kids(dom, anchor, "AlternateContent")[1L]
  ch <- ooxml_kids(dom, ac, "Choice")[1L]
  for (k in ooxml_kids(dom, ch, c("grpSp", "sp", "cxnSp", "pic", "graphicFrame"))) walk(k, 1, 1, 0, 0)
  if (length(acc$shapes) == 0L) {
    fb <- ooxml_kids(dom, ac, "Fallback")[1L]
    for (k in ooxml_kids(dom, fb, c("grpSp", "sp", "cxnSp", "pic"))) walk(k, 1, 1, 0, 0)
  }
  shapes <- acc$shapes
  if (length(shapes) == 0L) {
    stop(
      "the drawing has no shapes that can be read ",
      "(pictures need the png or jpeg package)",
      call. = FALSE
    )
  }

  if (!is.na(top)) {
    xf <- ooxml_kids(dom, ooxml_kids(dom, top, "grpSpPr")[1L], "xfrm")[1L]
    off <- ooxml_kids(dom, xf, "off")[1L]
    ext <- ooxml_kids(dom, xf, "ext")[1L]
    frame <- c(
      as.numeric(ooxml_attr(dom, off, "x")), as.numeric(ooxml_attr(dom, off, "y")),
      as.numeric(ooxml_attr(dom, ext, "cx")), as.numeric(ooxml_attr(dom, ext, "cy"))
    )
  } else {
    xs <- unlist(lapply(shapes, function(s) {
      if (s$type == "path") unlist(lapply(s$rings, `[[`, "x")) else c(s$box[1L], s$box[1L] + s$box[3L])
    }))
    ys <- unlist(lapply(shapes, function(s) {
      if (s$type == "path") unlist(lapply(s$rings, `[[`, "y")) else c(s$box[2L], s$box[2L] + s$box[4L])
    }))
    frame <- c(min(xs), min(ys), max(xs) - min(xs), max(ys) - min(ys))
  }

  frame[3:4] <- pmax(frame[3:4], 12700)

  for (k in seq_along(shapes)) {
    if (shapes[[k]]$type == "path") {
      for (r in seq_along(shapes[[k]]$rings)) {
        shapes[[k]]$rings[[r]]$x <- (shapes[[k]]$rings[[r]]$x - frame[1L]) / 12700
        shapes[[k]]$rings[[r]]$y <- (shapes[[k]]$rings[[r]]$y - frame[2L]) / 12700
      }
    } else {
      if (shapes[[k]]$type == "text") shapes[[k]]$trect <- shapes[[k]]$trect / 12700
      shapes[[k]]$box <- c(
        (shapes[[k]]$box[1L] - frame[1L]) / 12700, (shapes[[k]]$box[2L] - frame[2L]) / 12700,
        shapes[[k]]$box[3L] / 12700, shapes[[k]]$box[4L] / 12700
      )
    }
  }

  structure(
    list(width = frame[3L] / 12700, height = frame[4L] / 12700, shapes = shapes),
    class = "easel_drawing"
  )
}

ooxml_parse <- function(xml) {
  parts <- strsplit(xml, "<", fixed = TRUE)[[1L]][-1L]
  gt <- regexpr(">", parts, fixed = TRUE)
  tag <- substr(parts, 1L, gt - 1L)
  text <- substr(parts, gt + 1L, nchar(parts))
  skip <- startsWith(tag, "?") | startsWith(tag, "!")
  close <- startsWith(tag, "/")
  self <- endsWith(tag, "/")
  name <- sub("^.*:", "", sub("[ \t\r\n/].*$", "", sub("^/", "", tag)))
  name[skip | close] <- ""
  attr <- sub("^[^ \t\r\n]*", "", tag)

  parent <- integer(length(tag))
  stack <- integer(0L)
  for (i in seq_along(tag)) {
    if (skip[i]) next
    if (close[i]) {
      if (length(stack) > 0L) stack <- stack[-length(stack)]
      next
    }
    parent[i] <- if (length(stack) > 0L) stack[length(stack)] else 0L
    if (!self[i]) stack <- c(stack, i)
  }
  open <- which(!skip & !close)
  list(name = name, attr = attr, text = text, parent = parent, kids = split(open, parent[open]))
}

ooxml_kids <- function(dom, i, names) {
  if (length(i) != 1L || is.na(i)) return(integer(0L))
  k <- dom$kids[[as.character(i)]]
  if (is.null(k)) integer(0L) else k[dom$name[k] %in% names]
}

ooxml_attr <- function(dom, i, key) {
  if (length(i) != 1L || is.na(i)) return(NA_character_)
  m <- regmatches(
    dom$attr[i],
    regexec(paste0("[ \t\r\n](?:[^ \t\r\n=:]+:)?", key, "=\"([^\"]*)\""), dom$attr[i], perl = TRUE)
  )[[1L]]
  if (length(m) == 2L) m[2L] else NA_character_
}

ooxml_color <- function(dom, i, theme = character()) {
  clr <- ooxml_kids(dom, i, c("srgbClr", "sysClr", "schemeClr"))[1L]
  if (is.na(clr)) return(NA_character_)
  if (dom$name[clr] == "srgbClr") {
    val <- ooxml_attr(dom, clr, "val")
  } else if (dom$name[clr] == "sysClr") {
    val <- ooxml_attr(dom, clr, "lastClr")
  } else {
    key <- ooxml_attr(dom, clr, "val")
    key <- switch(key, tx1 = "dk1", bg1 = "lt1", tx2 = "dk2", bg2 = "lt2", key)
    val <- if (key %in% names(theme)) theme[[key]] else switch(key, dk1 = "000000", lt1 = "FFFFFF", NA_character_)
  }
  if (is.na(val)) return(NA_character_)

  rgb <- strtoi(substring(val, c(1L, 3L, 5L), c(2L, 4L, 6L)), 16L) / 255
  mods <- dom$kids[[as.character(clr)]]
  lum_mod <- 1
  lum_off <- 0
  alpha <- 1
  for (m in mods) {
    v <- as.numeric(ooxml_attr(dom, m, "val")) / 100000
    switch(dom$name[m],
      lumMod = lum_mod <- v,
      lumOff = lum_off <- v,
      tint = rgb <- rgb * v + (1 - v),
      shade = rgb <- rgb * v,
      alpha = alpha <- v,
      NULL
    )
  }
  if (lum_mod != 1 || lum_off != 0) {
    mx <- max(rgb)
    mn <- min(rgb)
    l <- (mx + mn) / 2
    d <- mx - mn
    s <- if (d == 0) 0 else d / (1 - abs(2 * l - 1))
    h <- if (d == 0) 0 else if (mx == rgb[1L]) ((rgb[2L] - rgb[3L]) / d) %% 6 else if (mx == rgb[2L]) (rgb[3L] - rgb[1L]) / d + 2 else (rgb[1L] - rgb[2L]) / d + 4
    l <- min(max(l * lum_mod + lum_off, 0), 1)
    cc <- (1 - abs(2 * l - 1)) * s
    xx <- cc * (1 - abs(h %% 2 - 1))
    base <- switch(floor(h) + 1, c(cc, xx, 0), c(xx, cc, 0), c(0, cc, xx), c(0, xx, cc), c(xx, 0, cc), c(cc, 0, xx), c(0, 0, 0))
    rgb <- base + (l - cc / 2)
  }
  out <- paste0("#", paste(sprintf("%02X", as.integer(round(pmin(pmax(rgb, 0), 1) * 255))), collapse = ""))
  if (alpha < 1) out <- paste0(out, sprintf("%02X", as.integer(round(alpha * 255))))
  out
}

#' @rdname easel_read
#' @export
plot.easel_drawing <- function(x, family = "", text_voff = 0.35, pad = 0, ...) {
  op <- par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
  on.exit(par(op))
  plot.new()
  plot.window(xlim = c(-pad, x$width + pad), ylim = c(x$height + pad, -pad), asp = 1)

  scale <- par("pin")[1L] * 72 / diff(par("usr")[1:2])
  ps <- par("ps")
  fig <- par("fig")
  plt <- par("plt")
  usr <- par("usr")
  pin <- par("pin")

  wbs <- list()
  for (sh in x$shapes) {
    if (sh$type == "path") {
      sh$rings <- Filter(function(r) length(r$x) > 1L, sh$rings)
      if (length(sh$rings) == 0L) next

      if (is.list(sh$fill) && getRversion() >= "4.1.0") {
        px <- unlist(lapply(sh$rings, function(r) r$x))
        py <- unlist(lapply(sh$rings, function(r) r$y))
        x0 <- min(px)
        y1 <- max(py)
        w <- max(px) - x0
        h <- y1 - min(py)
        cx <- x0 + w / 2
        cy <- min(py) + h / 2
        if (sh$fill$kind == "linear") {
          a <- sh$fill$ang * pi / 180
          half <- (abs(w * cos(a)) + abs(h * sin(a))) / 2
          pattern <- grid::linearGradient(
            colours = sh$fill$cols, stops = sh$fill$stops,
            x1 = (cx - half * cos(a) - x0) / w, y1 = (y1 - (cy - half * sin(a))) / h,
            x2 = (cx + half * cos(a) - x0) / w, y2 = (y1 - (cy + half * sin(a))) / h,
            default.units = "npc", extend = "pad"
          )
        } else {
          fx <- x0 + sh$fill$focus[1L] * w
          fy <- min(py) + sh$fill$focus[2L] * h
          reach <- max(fx - x0, max(px) - fx, fy - min(py), y1 - fy)
          pattern <- grid::radialGradient(
            colours = sh$fill$cols, stops = sh$fill$stops,
            cx1 = (fx - x0) / w, cy1 = (y1 - fy) / h, r1 = 0,
            cx2 = (fx - x0) / w, cy2 = (y1 - fy) / h, r2 = grid::unit(reach * scale, "bigpts"),
            default.units = "npc", extend = "pad"
          )
        }
        grid::pushViewport(grid::viewport(
          x = fig[1L] + plt[1L] * (fig[2L] - fig[1L]),
          y = fig[3L] + plt[3L] * (fig[4L] - fig[3L]),
          width = (plt[2L] - plt[1L]) * (fig[2L] - fig[1L]),
          height = (plt[4L] - plt[3L]) * (fig[4L] - fig[3L]),
          just = c("left", "bottom")
        ))
        grid::grid.path(
          (px - usr[1L]) * scale, (py - usr[3L]) / (usr[4L] - usr[3L]) * pin[2L] * 72,
          id.lengths = vapply(sh$rings, function(r) length(r$x), 0L),
          default.units = "bigpts", rule = "evenodd",
          gp = grid::gpar(fill = pattern, col = NA)
        )
        grid::popViewport()
      } else if (is.list(sh$fill) || !is.na(sh$fill)) {
        fill <- if (is.list(sh$fill)) sh$fill$cols[1L] else sh$fill
        fr <- Filter(function(r) !isTRUE(r$nofill), sh$rings)
        shades <- vapply(fr, function(r) if (is.null(r$shade)) 0 else r$shade, 0)
        rgba <- grDevices::col2rgb(fill, alpha = TRUE)[, 1L]
        for (sd in unique(shades)) {
          grp <- fr[shades == sd]
          px <- unlist(lapply(grp, function(r) c(r$x, NA)))
          py <- unlist(lapply(grp, function(r) c(r$y, NA)))
          rgb3 <- if (sd < 0) rgba[1:3] * (1 + sd) else rgba[1:3] + (255 - rgba[1:3]) * sd
          polypath(
            px[-length(px)], py[-length(py)], rule = "evenodd", border = NA,
            col = grDevices::rgb(rgb3[1L], rgb3[2L], rgb3[3L], rgba[4L], maxColorValue = 255)
          )
        }
      }

      if (!is.na(sh$col)) {
        for (r in sh$rings) {
          if (isTRUE(r$nostroke)) next
          if (r$closed) {
            polygon(
              r$x, r$y, col = NA, border = sh$col, lwd = max(sh$lwd * scale, 0.1), lty = sh$lty,
              lend = sh$lend, ljoin = sh$ljoin, lmitre = sh$lmitre
            )
          } else {
            lines(
              r$x, r$y, col = sh$col, lwd = max(sh$lwd * scale, 0.1), lty = sh$lty,
              lend = sh$lend, ljoin = sh$ljoin, lmitre = sh$lmitre
            )
          }
        }
      }
    } else if (sh$type == "chart") {
      if (!requireNamespace("encharter", quietly = TRUE) || !requireNamespace("openxlsx2", quietly = TRUE)) {
        warning("charts are only drawn when the encharter and openxlsx2 packages are installed", call. = FALSE) # nocov
        next # nocov
      }
      if (is.null(wbs[[sh$file]])) wbs[[sh$file]] <- openxlsx2::wb_load(sh$file)
      ch <- tryCatch(encharter::ec_load(wbs[[sh$file]], chart = sh$row), error = function(e) conditionMessage(e))
      if (is.character(ch)) {
        warning("a chart could not be read: ", ch, call. = FALSE)
        next
      }
      grid::pushViewport(grid::viewport(
        x = fig[1L] + plt[1L] * (fig[2L] - fig[1L]),
        y = fig[3L] + plt[3L] * (fig[4L] - fig[3L]),
        width = (plt[2L] - plt[1L]) * (fig[2L] - fig[1L]),
        height = (plt[4L] - plt[3L]) * (fig[4L] - fig[3L]),
        just = c("left", "bottom")
      ))
      grid::pushViewport(grid::viewport(
        x = (sh$box[1L] + sh$box[3L] / 2 - usr[1L]) * scale,
        y = (sh$box[2L] + sh$box[4L] / 2 - usr[3L]) / (usr[4L] - usr[3L]) * pin[2L] * 72,
        width = sh$box[3L] * scale, height = sh$box[4L] * scale,
        default.units = "bigpts"
      ))
      res <- tryCatch(plot(ch, wb = wbs[[sh$file]], newpage = FALSE), error = function(e) conditionMessage(e))
      if (is.character(res)) warning("a chart could not be drawn: ", res, call. = FALSE)
      grid::popViewport(2L)
    } else if (sh$type == "image" && sh$rot %% 360 == 0) {
      rasterImage(
        sh$img, sh$box[1L], sh$box[2L] + sh$box[4L], sh$box[1L] + sh$box[3L], sh$box[2L],
        interpolate = TRUE
      )
    } else if (sh$type == "image") {
      grid::pushViewport(grid::viewport(
        x = fig[1L] + plt[1L] * (fig[2L] - fig[1L]),
        y = fig[3L] + plt[3L] * (fig[4L] - fig[3L]),
        width = (plt[2L] - plt[1L]) * (fig[2L] - fig[1L]),
        height = (plt[4L] - plt[3L]) * (fig[4L] - fig[3L]),
        just = c("left", "bottom")
      ))
      grid::pushViewport(grid::viewport(
        x = (sh$box[1L] + sh$box[3L] / 2 - usr[1L]) * scale,
        y = (sh$box[2L] + sh$box[4L] / 2 - usr[3L]) / (usr[4L] - usr[3L]) * pin[2L] * 72,
        width = sh$box[3L] * scale, height = sh$box[4L] * scale,
        default.units = "bigpts", angle = -sh$rot
      ))
      grid::grid.raster(sh$img, width = grid::unit(1, "npc"), height = grid::unit(1, "npc"), interpolate = TRUE)
      grid::popViewport(2L)
    } else {
      a <- sh$rot * pi / 180
      bcx <- sh$box[1L] + sh$box[3L] / 2
      bcy <- sh$box[2L] + sh$box[4L] / 2
      ins <- sh$ins

      tw_box <- sh$trect[3L]
      th_box <- sh$trect[4L]
      avail <- (tw_box - ins[1L] - ins[3L]) * (if (is.null(sh$fit)) 1.12 else sh$fit)
      lns <- list()
      for (p in sh$paras) {
        if (!is.null(p$expr)) {
          cex_m <- p$size * scale / ps
          tall <- grepl("frac|sqrt|sum|prod|integral|bgroup", paste(deparse(p$expr), collapse = ""))
          mw <- strwidth(p$expr, units = "user", cex = cex_m, family = family)
          avail_m <- (sh$trect[3L] - ins[1L] - ins[3L]) * 1.12
          if (mw > avail_m && avail_m > 0) cex_m <- cex_m * avail_m / mw
          lns[[length(lns) + 1L]] <- list(
            expr = p$expr, col = p$col, size = p$size, algn = p$algn, cex = cex_m,
            w = strwidth(p$expr, units = "user", cex = cex_m, family = family),
            h = (if (tall) 2.4 else 1.5) * p$size
          )
          next
        }
        tok <- list()
        for (k in seq_along(p$text)) {
          pieces <- regmatches(p$text[k], gregexpr("[^ ]+ *| +", p$text[k]))[[1L]]
          for (pc in pieces) {
            tok[[length(tok) + 1L]] <- list(
              text = pc, size = p$size[k], font = p$font[k], col = p$col[k], rise = p$rise[k]
            )
          }
        }
        if (length(tok) == 0L) next
        k <- 1L
        while (k <= length(tok)) {
          t <- tok[[k]]
          nm <- nchar(sub(" +$", "", t$text))
          wd <- strwidth(sub(" +$", "", t$text), units = "user", cex = t$size * scale / ps, font = t$font, family = family)
          if (isTRUE(sh$wrap) && wd > avail && nm > 1L) {
            nfit <- max(1L, floor(nm * avail / wd))
            head_tok <- t
            head_tok$text <- substr(t$text, 1L, nfit)
            t$text <- substr(t$text, nfit + 1L, nchar(t$text))
            tok <- append(tok, list(head_tok), after = k - 1L)
            tok[[k + 1L]] <- t
          }
          k <- k + 1L
        }
        tw <- vapply(tok, function(t) {
          strwidth(sub(" +$", "", t$text), units = "user", cex = t$size * scale / ps, font = t$font, family = family)
        }, 0)
        tf <- vapply(tok, function(t) {
          strwidth(t$text, units = "user", cex = t$size * scale / ps, font = t$font, family = family)
        }, 0)
        cur <- integer()
        used <- 0
        flush <- function() {
          list(
            tok = tok[cur], w = used, algn = p$algn,
            h = 1.2 * max(vapply(tok[cur], function(t) t$size, 0))
          )
        }
        for (k in seq_along(tok)) {
          if (isTRUE(sh$wrap) && length(cur) > 0L && used + tw[k] > avail) {
            lns[[length(lns) + 1L]] <- flush()
            cur <- integer()
            used <- 0
          }
          used <- if (length(cur) == 0L) tw[k] else used - tw[cur[length(cur)]] + tf[cur[length(cur)]] + tw[k]
          cur <- c(cur, k)
        }
        lns[[length(lns) + 1L]] <- flush()
      }
      if (length(lns) == 0L) next
      block <- sum(vapply(lns, function(l) l$h, 0))
      top <- switch(sh$anchor,
        ctr = -block / 2 + (ins[2L] - ins[4L]) / 2,
        b = th_box / 2 - ins[4L] - block,
        -th_box / 2 + ins[2L]
      )

      yoff <- 0
      for (l in lns) {
        if (!is.null(l$expr)) {
          x0 <- switch(l$algn,
            ctr = -l$w / 2 + (ins[1L] - ins[3L]) / 2,
            r = tw_box / 2 - ins[3L] - l$w,
            -tw_box / 2 + ins[1L]
          )
          lx <- x0 + sh$trect[1L]
          ly <- top + sh$trect[2L] + yoff + l$h / 2
          yoff <- yoff + l$h
          text(
            bcx + lx * cos(a) - ly * sin(a), bcy + lx * sin(a) + ly * cos(a), l$expr, adj = c(0, 0.5),
            srt = -sh$rot, cex = l$cex, col = l$col, family = family
          )
          next
        }
        n <- length(l$tok)
        adv <- vapply(l$tok, function(t) {
          strwidth(t$text, units = "user", cex = t$size * scale / ps, font = t$font, family = family)
        }, 0)
        x0 <- switch(l$algn,
          ctr = -l$w / 2 + (ins[1L] - ins[3L]) / 2,
          r = tw_box / 2 - ins[3L] - l$w,
          -tw_box / 2 + ins[1L]
        )
        lx <- x0 + c(0, cumsum(adv))[seq_len(n)] + sh$trect[1L]
        ly <- top + sh$trect[2L] + yoff + (0.6 + text_voff) * max(vapply(l$tok, function(t) t$size, 0)) -
          vapply(l$tok, function(t) t$rise, 0)
        yoff <- yoff + l$h

        gx <- bcx + lx * cos(a) - ly * sin(a)
        gy <- bcy + lx * sin(a) + ly * cos(a)
        for (k in seq_len(n)) {
          t <- l$tok[[k]]
          if (is.na(t$col)) next
          text(
            gx[k], gy[k], t$text, adj = c(0, 0), srt = -sh$rot, cex = t$size * scale / ps,
            font = t$font, col = t$col, family = family
          )
        }
      }
    }
  }

  invisible(x)
}

#' @rdname easel_read
#' @export
print.easel_drawings <- function(x, ...) {
  nm <- names(x)
  lab <- paste(nm, collapse = ", ")
  cat(length(x), " drawings: ", lab, "\n", sep = "")
  invisible(x)
}

#' @rdname easel_read
#' @param ncol Number of panels per row when plotting all drawings. The default
#'   is 3 or fewer.
#' @export
plot.easel_drawings <- function(x, ncol = NULL, ...) {
  if (is.null(ncol)) ncol <- min(length(x), 3L)
  op <- par(mfrow = c(ceiling(length(x) / ncol), ncol), mar = c(0.5, 0.5, 1.5, 0.5))
  on.exit(par(op))
  for (k in seq_along(x)) {
    plot(x[[k]], ...)
    title(main = names(x)[k], cex.main = 0.8, line = 0.3)
  }
  invisible(x)
}
