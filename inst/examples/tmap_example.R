library(easeling)
library(openxlsx2)
library(tmap)

# tmap objects (like ggplot2 objects) only draw when printed, so a bare
# tm_shape(...) + ... inside a script or easel_xml() draws nothing.
xml <- easel_xml(print(
  tm_shape(World) +
    tm_polygons(fill = "HPI")
), width = 6, height = 4)

wb <- wb_workbook()$add_worksheet()$add_drawing(xml = xml, dims = "A1:G15")

if (interactive()) wb$open()
