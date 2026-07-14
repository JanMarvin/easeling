library(easeling)
library(openxlsx2)
library(tinyplot)

f <- easel_dev(width = 6, height = 4)
tinytheme("clean2")

# plt(Sepal.Length ~ Petal.Length | Species, data = iris)
# plt_add(type = "lm")

# plt(
#   ~ Petal.Length | Species,
#   data = iris,
#   type = "density",
#   fill = "by",
#   main = "Distribution of petal lengths",
#   sub = "Grouped by species"
# )

plt(
  Sepal.Length ~ Petal.Length | Sepal.Length, data = iris,
  facet = ~Species, pch = 19,
  main = "Faceted flowers", sub = "Brought to you by tinyplot"
)
dev.off()

x <- read_xml(f, pointer = FALSE)

wb <- wb_workbook()$add_worksheet(grid_lines = FALSE)$add_drawing(xml = x)
wb$open()
