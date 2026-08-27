# Render plotting code straight to a DrawingML string

Evaluates \`code\` on an in-memory easeling device and returns the
resulting drawing as a single character string - no file is involved.
The device is opened before and closed after \`code\`, also on error.

## Usage

``` r
easel_xml(code, ...)
```

## Arguments

- code:

  Plotting code; a braced block for multiple statements. Remember that
  ggplot2/tmap/lattice objects only draw when printed, so wrap them in
  \`print()\`.

- ...:

  Passed to \[easel_dev()\] (everything except \`file\`).

## Value

The DrawingML as a length-one character vector, ready for
\`openxlsx2::wb_add_drawing(xml = )\`.

## Examples

``` r
xml <- easel_xml(plot(1:10), width = 4, height = 3)
```
