library(easeling)
library(openxlsx2)

f <- easel_dev(width = 10, height = 10, fontname = "Comic Sans")

x <- seq(-10, 10, length = 50)
y <- seq(-10, 10, length = 50)
f2 <- function(x, y) {
  r <- sqrt(x^2 + y^2)
  z <- sin(r) / r
  z[is.na(z)] <- 1
  z
}
z <- outer(x, y, f2)
persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue",
      shade = 0.75, border = NA, ticktype = "detailed")

dev.off()

wb <- wb_workbook()$add_worksheet()$add_drawing(xml = f, dims = "A1")
wb$open()
