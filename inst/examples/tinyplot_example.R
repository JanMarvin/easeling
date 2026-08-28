library(easeling)
library(openxlsx2)
library(tinyplot)

tinytheme("clean2")
wb <- wb_workbook()

xml <- easel_xml({
  plt(
    Sepal.Length ~ Petal.Length | Sepal.Length, data = iris,
    facet = ~Species, pch = 19,
    main = "Faceted flowers", sub = "Brought to you by tinyplot"
  )
}, width = 6, height = 4)
wb$add_worksheet("facets", grid_lines = FALSE)$add_drawing(xml = xml)

xml <- easel_xml({
  plt(Sepal.Length ~ Petal.Length | Species, data = iris)
  plt_add(type = "lm")
}, width = 6, height = 4)
wb$add_worksheet("fit", grid_lines = FALSE)$add_drawing(xml = xml)

xml <- easel_xml({
  plt(
    ~ Petal.Length | Species,
    data = iris,
    type = "density",
    fill = "by",
    main = "Distribution of petal lengths",
    sub = "Grouped by species"
  )
}, width = 6, height = 4)
wb$add_worksheet("density", grid_lines = FALSE)$add_drawing(xml = xml)

if (interactive()) wb$open()
