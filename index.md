# easeling

A small graphics device that writes R plots directly as OOXML DrawingML
shapes, for use with
[`openxlsx2::wb_add_drawing()`](https://janmarvin.github.io/openxlsx2/reference/wb_add_drawing.html).
No Cairo, FreeType, fontconfig, or xml2 dependency.

## Install

You can install development versions via r-universe:

``` r

install.packages(
  "easeling",
  repos = c("https://janmarvin.r-universe.dev", "https://cloud.r-project.org")
)
```

Or from GitHub directly:

``` r

# install.packages("remotes")
remotes::install_github("JanMarvin/easeling")
```

## Usage

[`easel_dev()`](reference/easel_dev.md) opens the device and returns a
file path. Draw with any R plotting system, call
[`dev.off()`](https://rdrr.io/r/grDevices/dev.html), then hand the file
to
[`openxlsx2::wb_add_drawing()`](https://janmarvin.github.io/openxlsx2/reference/wb_add_drawing.html).

``` r

library(easeling)
library(openxlsx2)

f <- easel_dev(width = 6, height = 4)
plot(1:10, (1:10)^2, type = "b")
dev.off()

wb <- wb_workbook()$add_worksheet()$add_drawing(xml = f, dims = "A1")
```

### tinyplot

``` r

library(tinyplot)

f <- easel_dev(width = 6, height = 4)
plt(mpg ~ wt | factor(cyl), data = mtcars)
dev.off()

wb$add_worksheet()$add_drawing(xml = f, dims = "A1")
```

### ggplot2

``` r

library(ggplot2)

f <- easel_dev(width = 6, height = 4)
print(
  ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) +
    geom_point() +
    theme_minimal()
)
dev.off()

wb$add_worksheet()$add_drawing(xml = f, dims = "A1")

if (interactive()) wb$open()
```

## Notes

- Text uses estimated font metrics, not real font data — layout that
  depends on precise text measurement (legend spacing, centered labels)
  may be slightly off, especially with unusual fonts.
- Fonts are referenced by name only, not embedded. The viewer needs the
  font installed, or it falls back silently.
- Gradient fills
  ([`grid::linearGradient()`](https://rdrr.io/r/grid/patterns.html)/`radialGradient()`)
  are supported; tiling patterns and clip paths/masks are not.
