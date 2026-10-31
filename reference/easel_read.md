# Read a drawing back into R

Parses a DrawingML drawing into a list of shapes that can be redrawn
with \[plot()\]. The drawing can come from a file written by
\[easel_dev()\], or from a workbook that contains one, including a
workbook that has been saved again by a spreadsheet application. This
closes the loop plot -\> drawing -\> workbook -\> plot, without access
to the original plot. The result is a redraw of the picture, not of the
original plot: axes, legends and points come back as the paths and text
runs they were written as.

## Usage

``` r
easel_read(
  file,
  sheet = 1L,
  n = 1L,
  what = c("drawing", "comments"),
  all = FALSE
)

# S3 method for class 'easel_drawing'
plot(x, family = "", text_voff = 0.35, pad = 0, ...)

# S3 method for class 'easel_drawings'
print(x, ...)

# S3 method for class 'easel_drawings'
plot(x, ncol = NULL, ...)
```

## Arguments

- file:

  Path of an XML file written by \[easel_dev()\], of a drawing XML file,
  or of an \`.xlsx\` workbook.

- sheet:

  For a workbook, the sheet (name or position) holding the drawing.

- n:

  Which drawing on the sheet to read, in the order of the drawing's
  anchors, or which comment, in the order of the cells.

- what:

  \`"drawing"\` reads a drawing. \`"comments"\` reads a note or a
  threaded comment of a worksheet and returns it as the box a
  spreadsheet application shows for it.

- all:

  If \`TRUE\`, reads every drawing of every sheet of a workbook (or
  every drawing of an XML file, or every note and threaded comment with
  \`what = "comments"\`) and returns them as one object that \`plot()\`
  draws as a grid of panels, each labeled with its sheet and position.
  \`sheet\` and \`n\` are ignored.

- x:

  An \`easel_drawing\` object, or the list of them returned with \`all =
  TRUE\`.

- family:

  Font family used for text. The typefaces named in the file are not
  used, since a graphics device rarely knows them.

- text_voff:

  Baseline offset in em below the center of a text box. The file does
  not record it, so it must match the \`text_voff\` the drawing was
  written with (see \[easel_dev()\]).

- pad:

  Extra margin in points around the drawing, so that thick strokes at
  the edge are not cut off. The default of 0 reproduces the drawing's
  frame exactly.

- ...:

  Ignored by the plot method of a single drawing; the plot method of
  several drawings passes them on to it.

- ncol:

  Number of panels per row when plotting all drawings. The default is 3
  or fewer.

## Value

\`easel_read()\` returns an object of class \`easel_drawing\`, a list
with the drawing \`width\` and \`height\` in points and a list of
\`shapes\`. With \`all = TRUE\` it returns a named list of such objects
of class \`easel_drawings\`. \`plot()\` returns \`x\` invisibly.

## Details

Understood are most of the common preset shapes (basic shapes, polygons,
stars, block arrows, flowchart symbols, callouts, \`donut\`, \`frame\`,
\`cube\`, \`can\`, \`heart\`, \`cloud\`, \`wave\` and more; shadings and
3-D lighting are not reproduced and presets not in the list are drawn as
their bounding box), shapes without a transform (sized from their
anchor), theme and style colors, \`custGeom\` paths made of \`moveTo\`,
\`lnTo\`, \`cubicBezTo\`, \`quadBezTo\` and \`close\`, solid, linear and
radial gradient fills (R \>= 4.1 for gradients, older versions use the
first stop color), line properties, shape rotation and flips, nested
groups, and text. Embedded PNG and JPEG pictures are drawn when the png
or jpeg package is installed, including cropping, rotation and flips.
Charts are drawn with the encharter package (with openxlsx2) when both
are installed. Slicers of pivot tables are drawn with their items and
which of them are selected; for any other object, such as a slicer of a
table, the placeholder shape stored for older readers is drawn. Diagrams
(SmartArt) are drawn from the shapes Office stores with them. Pictures
that cannot be decoded, such as EMF or GIF, are drawn as an empty gray
frame. Equations (Office Math in text) are drawn for fractions, scripts,
roots, sums, integrals, brackets and accents. Arcs, underlines and
strike-through are ignored.

## Examples

``` r
f <- easel_dev(width = 4, height = 3)
plot(1:10, main = "round trip")
dev.off()
#> agg_record_188810baefb8 
#>                       2 

d <- easel_read(f)
plot(d)
```
