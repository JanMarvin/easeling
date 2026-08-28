# Showcase gallery: one workbook, several plot styles, one drawing each.
# Requires openxlsx2; uses ggplot2 when installed. Run, then open
# showcase.xlsx in your spreadsheet application.
library(easeling)
library(openxlsx2)

wb <- wb_workbook()$add_worksheet("gallery", grid_lines = FALSE)
gallery <- new.env()
gallery$row <- 1
put <- function(xml) {
  wb$add_drawing(xml = xml, dims = paste0("A", gallery$row))
  assign("row", gallery$row + 22, envir = gallery)
}

put(easel_xml({
  par(mfrow = c(1, 2), mar = c(4, 4, 2, 1), bg = "white")
  plot(pressure, type = "b", pch = 19, col = "steelblue",
       main = "Vapor pressure")
  hist(rnorm(500), col = hcl.colors(12), border = "white",
       main = "Histogram", xlab = "z")
}, width = 8, height = 4))

put(easel_xml({
  persp(volcano, theta = 35, phi = 25, col = "lightblue",
        border = "grey40", main = "Maunga Whau")
}, width = 8, height = 4))

put(easel_xml({
  grid::grid.rect(gp = grid::gpar(
    fill = grid::linearGradient(c("#0b1e3f", "#2a6f97", "#a9d6e5"))))
  grid::pushViewport(grid::viewport(clip = grid::circleGrob(r = .35)))
  grid::grid.rect(gp = grid::gpar(fill = "#ffd166", col = NA))
  for (i in seq(.05, .95, by = .06))
    grid::grid.segments(0, i, 1, i, gp = grid::gpar(col = "#ef476f", lwd = 3))
  grid::popViewport()
  grid::grid.text("clip paths, gradients", y = .06,
                  gp = grid::gpar(col = "white", cex = 1.2))
}, width = 8, height = 4))

# nolint start: object_usage_linter. (ggplot2 is optional; aes() columns
# cannot be resolved by static analysis when it is absent)
if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)
  put(easel_xml(print(
    ggplot(mtcars, aes(mpg, fill = factor(gear))) +
      geom_density(alpha = .5) + ggtitle("Gas mileage by gear")
  ), width = 8, height = 4))
  put(easel_xml(print(
    ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
      geom_raster() + scale_fill_viridis_c() + ggtitle("Old Faithful")
  ), width = 8, height = 4))
}

# nolint end
wb$save("showcase.xlsx")
cat("wrote showcase.xlsx\n")
