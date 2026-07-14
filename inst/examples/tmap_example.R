library(easeling)
library(openxlsx2)
library(tmap)


f <- easel_dev(width = 6, height = 4)
tm_shape(World) +
  tm_polygons(fill = "HPI")
dev.off()

x <- read_xml(f, pointer = FALSE)

wb <- wb_workbook()$add_worksheet()$add_drawing(xml = x, dims = "A1:G15")
wb$open()
