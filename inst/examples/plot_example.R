library(easeling)
library(openxlsx2)

f <- easel_dev(width = 6, height = 4)
plot(1:10, (1:10)^2, type = "b")
dev.off()

x <- read_xml(f, pointer = FALSE)

wb <- wb_workbook()$add_worksheet()$add_drawing(xml = x, dims = "A1:G15")
wb$open()
