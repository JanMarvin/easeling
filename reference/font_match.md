# Which font is actually measured for a typeface

The machine writing a drawing need not have the font the drawing will be
opened with. \`systemfonts\` substitutes without saying so, and the plot
is then laid out with one font's widths while the drawing asks the
spreadsheet application for another. Often that costs nothing, because
some substitutes exist to match: Carlito for Calibri, Liberation Sans
for Arial, Liberation Serif for Times New Roman. Others are unrelated
fonts of quite different width. The device does not check, so call this
when a drawing's layout looks wrong and you want to know which font
produced it.

## Usage

``` r
font_match(fontname)
```

## Arguments

- fontname:

  Typeface the drawing will name.

## Value

A list with \`requested\`, \`matched\` and \`substituted\`, or \`NULL\`
when \`systemfonts\` is not installed.

## Examples

``` r
font_match("Calibri")
#> $requested
#> [1] "Calibri"
#> 
#> $matched
#> [1] "DejaVu Sans"
#> 
#> $substituted
#> [1] TRUE
#> 
```
