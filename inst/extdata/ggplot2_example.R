library(easeling)
library(openxlsx2)
library(ggplot2)

f <- easel_dev(width = 6, height = 4)
p <- ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
  geom_raster() +
  scale_fill_viridis_c()
# p <- ggplot(mtcars, aes(x = mpg, fill = as.factor(gear))) +
#   ggtitle("Distribution of Gas Mileage") +
#   geom_density(alpha = 0.5)
print(p)
dev.off()

x <- read_xml(f, pointer = FALSE)

wb <- wb_workbook()$add_worksheet()$add_drawing(xml = x, dims = "A1:G15")
wb$open()
