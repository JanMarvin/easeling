# Compute the device size for a spreadsheet cell region

Translates a cell range like \`"A1:G15"\` into a device size in inches,
so that a plot drawn at that size fills the region exactly when anchored
to the same \`dims\` — the dims-first workflow: pick the region, size
the device from it, then plot. Because the device is opened at the final
size, R lays out text, margins, and legends for that size; nothing needs
to be rescaled afterwards (DrawingML cannot rescale text in groups).

## Usage

``` r
easel_size(dims, wb = NULL, sheet = 1)
```

## Arguments

- dims:

  A cell range such as \`"A1:G15"\` (both corners inclusive).

- wb:

  Optionally, an \`openxlsx2\` workbook (\`wbWorkbook\`) to read the
  actual column widths and row heights from.

- sheet:

  Sheet index in \`wb\`. Default \`1\`.

## Value

Named numeric vector \`c(width = , height = )\` in inches.

## Details

Column widths and row heights are taken from \`wb\` when given,
including custom widths/heights and the sheet defaults stored in its
\`sheetFormatPr\`. Without \`wb\`, Excel's defaults are assumed (Calibri
11: 8.43 character column width = 64px, 15pt row height = 20px). The
character-to-pixel conversion uses Excel's formula for a maximum digit
width of 7px; workbooks with a different base font will be off by the
ratio of their digit widths.

## Examples

``` r
easel_size("A1:G15")
#>    width   height 
#> 4.666667 3.125000 
```
