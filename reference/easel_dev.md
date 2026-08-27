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
  strikeout = FALSE,
  dims = NULL,
  wb = NULL,
  sheet = 1,
  text_voff = 0.35,
  metrics = NULL
)
```

## Arguments

- file:

  Path of the XML file to write. Defaults to a temp file. For a plain
  string instead of a file, see \[easel_xml()\].

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

- dims:

  Optionally, a cell range such as \`"A1:G15"\`. If given, \`width\` and
  \`height\` are ignored and computed from the region via
  \[easel_size()\], so the plot fills that region exactly when later
  anchored to the same \`dims\`.

- wb, sheet:

  Passed to \[easel_size()\] when \`dims\` is given: the \`openxlsx2\`
  workbook (and sheet) to read actual column widths and row heights
  from.

- text_voff:

  Vertical text calibration in em: text boxes are centre-anchored, and
  the baseline is placed \`text_voff\` em below the box centre. The
  default \`0.35\` was calibrated against Excel's line layout for
  Calibri; LibreOffice's optimum is around 0.24, so text there sits ~0.1
  em low. Increase to shift rendered text up, decrease to shift it down,
  if your spreadsheet application's line layout places it visibly off;
  see \`system.file("examples", "calibrate_text.R", package =
  "easeling")\`.

- metrics:

  Font metrics used for text layout (string widths, vertical centring,
  margins). \`NULL\` (default): use real metrics for \`fontname\` via
  the \`systemfonts\` package when it is installed, otherwise the
  built-in Calibri-like table. \`FALSE\`: always use the built-in table.
  Or a list with numeric components \`widths\`, \`ascents\`,
  \`descents\`, each of length 95 giving em fractions for the ASCII
  characters 32..126. Metrics only affect what R computes - rendering is
  always done by the spreadsheet application with the real font - but
  better metrics mean legend boxes, margins, and centring are sized for
  the text that will actually appear.

## Value

The output file path, invisibly.

## Examples

``` r
f <- easel_dev(width = 6, height = 4, fontname = "Georgia")
plot(1:10, (1:10)^2, type = "b")
dev.off()
#> agg_record_1a192a28ee9b 
#>                       2 
```
