library(easeling)
library(openxlsx2)
library(grid)

f <- easel_dev(width = 5, height = 3)

grid::grid.rect(
  gp = grid::gpar(fill = grid::linearGradient(colours = c("steelblue", "white")))
)
grid::grid.circle(
  x = 0.5, y = 0.5, r = 0.25,
  gp = grid::gpar(fill = grid::radialGradient(colours = c("gold", "darkred")))
)

dev.off()

wb <- wb_workbook()$add_worksheet()$add_drawing(xml = f, dims = "A1")
wb$open()
