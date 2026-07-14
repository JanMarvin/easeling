# Open a DrawingML graphics device for openxlsx2

Draws directly to a standalone DrawingML XML file containing \`xdr:sp\`
shapes, suitable for \`openxlsx2::wb_add_drawing(xml = file)\`. No
dependency on Cairo, FreeType, fontconfig, or xml2.

## Usage

``` r
easel_dev(
  file = tempfile(fileext = ".xml"),
  width = 6,
  height = 6,
  pointsize = 12,
  fontname = "Calibri",
  underline = FALSE,
  strikeout = FALSE
)
```

## Arguments

- file:

  Path to the output XML file. Defaults to a temp file.

- width, height:

  Device size in inches.

- pointsize:

  Default font pointsize.

- fontname:

  Default font typeface (matches \`openxlsx2::wb_add_font()\`'s \`name\`
  argument), e.g. \`"Calibri"\`, \`"Arial"\`. Used whenever R itself
  doesn't request a specific family — i.e. whenever a plot's own
  \`family\`/\`fontfamily\` is unset or is one of R's generic aliases
  (\`"sans"\`, \`"serif"\`, \`"mono"\`, \`"symbol"\`, or \`""\`). If a
  plot sets an actual font name (e.g. \`par(family = "Georgia")\` or
  \`theme_minimal(base_family = "Georgia")\`), that takes priority over
  this default.

- underline, strikeout:

  Apply underline/strikeout to all text on this device. Unlike
  \`fontname\`, these have no per-call R equivalent (base graphics has
  no underline/strikeout concept), so they're a device-wide setting.

## Value

The output file path, invisibly.

## Examples

``` r
# \donttest{
f <- easel_dev(width = 6, height = 4, fontname = "Georgia")
plot(1:10, (1:10)^2, type = "b")
dev.off()
#> agg_record_19db7e1b22c7 
#>                       2 
# }
```
