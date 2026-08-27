source("scenarios.R")
suppressPackageStartupMessages(library(easeling))
dir.create("out", showWarnings = FALSE)
res <- data.frame(name=character(), xml_ok=logical(), stringsAsFactors=FALSE)
for (sc in scenarios) {
  nm <- sc[[1]]; code <- sc[[2]]; w <- sc[[3]]; h <- sc[[4]]
  ok <- TRUE
  # easeling render
  r1 <- tryCatch({
    f <- easel_dev(file.path("out", paste0(nm, ".xml")), width=w, height=h,
                   fontname="DejaVu Sans", metrics = FALSE)
    op <- par(no.readonly = TRUE); code(); par(op); dev.off(); TRUE
  }, error = function(e) { try(dev.off(), silent=TRUE); message(nm, " EASEL ERROR: ", conditionMessage(e)); FALSE })
  # reference render
  r2 <- tryCatch({
    pdf(file.path("out", paste0(nm, "_ref.pdf")), width=w, height=h, family="sans")
    op <- par(no.readonly = TRUE); code(); par(op); dev.off(); TRUE
  }, error = function(e) { try(dev.off(), silent=TRUE); FALSE })
  xml_ok <- r1 && system2("xmllint", c("--noout", file.path("out", paste0(nm, ".xml"))),
                          stderr=FALSE) == 0
  res <- rbind(res, data.frame(name=nm, xml_ok=xml_ok))
}
exp_names <- vapply(Filter(function(sc) length(sc) >= 5 && isTRUE(sc[[5]]) || (length(sc) == 5), scenarios), function(sc) sc[[1]], "")
exp_names <- vapply(Filter(function(sc) length(sc) >= 5, scenarios), function(sc) sc[[1]], "")
writeLines(exp_names, "out/expected.txt")
print(res, row.names = FALSE)
cat("well-formed XML:", sum(res$xml_ok), "/", nrow(res), "\n")
