library(easeling)
library(openxlsx2)
library(ggplot2)

wb <- wb_workbook()

xml <- easel_xml(print(
  ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
    geom_raster() +
    scale_fill_viridis_c()
), width = 6, height = 4)
wb$add_worksheet("raster")$add_drawing(xml = xml, dims = "A1:G15")

xml <- easel_xml(print(
  ggplot(mtcars, aes(x = mpg, fill = as.factor(gear))) +
    ggtitle("Distribution of Gas Mileage") +
    geom_density(alpha = 0.5)
), width = 6, height = 4)
wb$add_worksheet("density")$add_drawing(xml = xml, dims = "A1:G15")

if (interactive()) wb$open()
