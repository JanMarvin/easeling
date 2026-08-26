# Text placement calibration sheet -------------------------------------------
# Generates calibration.xlsx: one drawing per candidate `text_voff`, each
# showing sample text against geometry-drawn reference lines. Geometry
# placement is exact, so the solid red line IS the intended baseline and the
# dashed grey lines are the intended ink extents (0.75 em ascent / 0.25 em
# descent). Open the file in your spreadsheet application and pick the row
# where the glyph baseline sits on the red line; pass that value as
# `easel_dev(text_voff = ...)`.
library(easeling)
library(openxlsx2)

offsets <- seq(0.10, 0.45, by = 0.05)
ps <- 24
wb <- wb_workbook()$add_worksheet(grid_lines = FALSE)

anchor_row <- 1
for (v in offsets) {
  w_in <- 6.5; h_in <- 0.7
  f <- easel_dev(width = w_in, height = h_in, pointsize = ps,
                 fontname = "Calibri", text_voff = v)
  op <- par(mar = rep(0, 4))
  plot.new()
  plot.window(xlim = c(0, w_in * 72), ylim = c(h_in * 72, 0),
              xaxs = "i", yaxs = "i")
  yb <- 34                                  # baseline, pt from top
  segments(120, yb - 0.75 * ps, 320, yb - 0.75 * ps, col = "grey60", lty = 3)
  segments(120, yb + 0.25 * ps, 320, yb + 0.25 * ps, col = "grey60", lty = 3)
  segments(100, yb, 340, yb, col = "red", lwd = 1.5)
  text(125, yb, "Hxg 3.5", adj = c(0, 0), cex = 1)      # adj[2]=0: baseline at y
  text(10, yb, sprintf("voff %.2f", v), adj = c(0, 0), cex = 0.6)
  # rotated sample: red vertical line = intended baseline of 90-degree text
  xv <- 400
  segments(xv, yb + 10, xv, yb - 32, col = "red", lwd = 1.5)
  text(xv, yb + 8, "Hxg", adj = c(0, 0), srt = 90, cex = 0.75)
  # centred sample: red cross at the text() anchor with default centring
  points(470, yb - 8, pch = 3, col = "red", cex = 1.5)
  text(470, yb - 8, "MMM", cex = 0.75)
  par(op)
  dev.off()
  wb$add_drawing(xml = read_xml(f, pointer = FALSE),
                 dims = paste0("A", anchor_row))
  anchor_row <- anchor_row + 4
}
wb$save("calibration.xlsx")
cat("wrote calibration.xlsx - screenshot the whole column after opening\n")
