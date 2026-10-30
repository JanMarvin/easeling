ooxml_math <- function(dom, i) {
  q <- function(x) paste0("\"", gsub("([\"\\\\])", "\\\\\\1", x), "\"")
  decode <- function(x) {
    Encoding(x) <- "UTF-8"
    x <- gsub("&lt;", "<", x, fixed = TRUE)
    x <- gsub("&gt;", ">", x, fixed = TRUE)
    x <- gsub("&quot;", "\"", x, fixed = TRUE)
    x <- gsub("&apos;", "'", x, fixed = TRUE)
    gsub("&amp;", "&", x, fixed = TRUE)
  }
  text_expr <- function(tx) {
    cp <- utf8ToInt(tx)
    if (length(cp) == 0L) return(q(""))
    low <- cp >= 0x1D44E & cp <= 0x1D467
    up <- cp >= 0x1D434 & cp <= 0x1D44D
    ital <- low | up | cp == 0x210E
    base <- cp
    base[up] <- cp[up] - 0x1D434L + 65L
    base[low] <- cp[low] - 0x1D44EL + 97L
    base[cp == 0x210E] <- 104L
    runs <- rle(ital)
    parts <- character()
    pos <- 0L
    for (k in seq_along(runs$lengths)) {
      seg <- intToUtf8(base[pos + seq_len(runs$lengths[k])])
      pos <- pos + runs$lengths[k]
      parts <- c(parts, if (runs$values[k]) paste0("italic(", q(seg), ")") else q(seg))
    }
    if (length(parts) == 1L) parts else paste0("paste(", paste(parts, collapse = ", "), ")")
  }
  cp1 <- function(x) if (is.na(x) || !nzchar(x)) 0L else utf8ToInt(x)[1L]
  attr_of <- function(node, container, nm, key) {
    pr <- ooxml_kids(dom, node, container)[1L]
    nd <- ooxml_kids(dom, pr, nm)[1L]
    v <- ooxml_attr(dom, nd, key)
    Encoding(v) <- "UTF-8"
    v
  }
  seq_expr <- function(node) {
    kids <- dom$kids[[as.character(node)]]
    kids <- kids[!grepl("Pr$", dom$name[kids]) & dom$name[kids] != "t"]
    parts <- vapply(kids, ex, "")
    if (length(parts) == 0L) q("") else if (length(parts) == 1L) parts else paste0("paste(", paste(parts, collapse = ", "), ")")
  }
  ch <- function(node, nm) {
    k <- ooxml_kids(dom, node, nm)[1L]
    if (is.na(k)) q("") else seq_expr(k)
  }
  has <- function(node, nm) {
    k <- ooxml_kids(dom, node, nm)[1L]
    !is.na(k) && length(dom$kids[[as.character(k)]]) > 0L
  }
  ex <- function(node) {
    nm <- dom$name[node]
    switch(nm,
      r = {
        tn <- ooxml_kids(dom, node, "t")[1L]
        if (is.na(tn)) q("") else text_expr(decode(dom$text[tn]))
      },
      f = paste0("frac(", ch(node, "num"), ", ", ch(node, "den"), ")"),
      sSup = paste0("{", ch(node, "e"), "}^{", ch(node, "sup"), "}"),
      sSub = paste0("{", ch(node, "e"), "}[", ch(node, "sub"), "]"),
      sSubSup = paste0("{", ch(node, "e"), "}[", ch(node, "sub"), "]^{", ch(node, "sup"), "}"),
      rad = paste0("sqrt(", ch(node, "e"), if (has(node, "deg")) paste0(", ", ch(node, "deg")) else "", ")"),
      nary = {
        chr <- attr_of(node, "naryPr", "chr", "val")
        fn <- if (isTRUE(cp1(chr) == 0x2211)) "sum" else if (isTRUE(cp1(chr) == 0x220F)) "prod" else "integral"
        lim <- c(
          if (has(node, "sub")) ch(node, "sub"),
          if (has(node, "sub") && has(node, "sup")) ch(node, "sup")
        )
        paste0(fn, "(", paste(c(ch(node, "e"), lim), collapse = ", "), ")")
      },
      d = {
        beg <- attr_of(node, "dPr", "begChr", "val")
        end <- attr_of(node, "dPr", "endChr", "val")
        sep <- attr_of(node, "dPr", "sepChr", "val")
        if (is.na(beg)) beg <- "("
        if (is.na(end)) end <- ")"
        if (is.na(sep)) sep <- "|"
        inner <- vapply(ooxml_kids(dom, node, "e"), seq_expr, "")
        inner <- if (length(inner) == 1L) inner else paste0("paste(", paste(inner, collapse = paste0(", ", q(paste0(sep, " ")), ", ")), ")")
        paste0("bgroup(", if (nzchar(beg)) q(beg) else q("."), ", ", inner, ", ", if (nzchar(end)) q(end) else q("."), ")")
      },
      func = paste0("paste(", ch(node, "fName"), ", ", q(" "), ", ", ch(node, "e"), ")"),
      acc = {
        chr <- attr_of(node, "accPr", "chr", "val")
        fn <- if (isTRUE(cp1(chr) == 0x0304)) "bar" else if (isTRUE(cp1(chr) == 0x0307)) "dot" else if (isTRUE(cp1(chr) == 0x0303)) "tilde" else "hat"
        paste0(fn, "(", ch(node, "e"), ")")
      },
      bar = paste0("bar(", ch(node, "e"), ")"),
      limLow = paste0("{", ch(node, "e"), "}[", ch(node, "lim"), "]"),
      limUpp = paste0("{", ch(node, "e"), "}^{", ch(node, "lim"), "}"),
      seq_expr(node)
    )
  }
  seq_expr(i)
}
