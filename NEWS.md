# easeling 0.3.1

* Words drawn as separate calls on one baseline now end up in a single
  shape, one run per word. gridtext, and through it ggtext, lays out
  marked-up titles that way, and every word used to become its own box
  sized by our estimate of its width.
* Super- and subscripts stay on the line as shifted runs instead of
  breaking it into separate shapes.
* Glyphs (R >= 4.3) keep the positions the graphics engine gave them and
  are never merged.
* Nothing is drawn outside the canvas any more. Clip rectangles are
  intersected with the device, which `xpd = NA` can otherwise exceed, and
  raster cells are clipped one by one instead of only as a whole image.
* New `font_match()` reports which font was really measured. Writing a
  drawing on a machine that lacks the font it will be opened with is
  normal, and `systemfonts` substitutes without saying so.

# easeling 0.3.0

* `easel_xml()` renders plotting code straight to a DrawingML string with no
  file involved; `easel_dev(file=)` remains the file-based device.
* `easel_size()` and `easel_dev(dims =, wb =)` size the device from real
  workbook cell geometry.
* Optional real font metrics through 'systemfonts' (`metrics =`), with a
  built-in table and user-supplied tables as alternatives.
* Text placement calibrated against Excel's line layout (`text_voff`),
  with per-character vertical metrics for correct centring.
* Shaped clip paths (R >= 4.1): exact for arbitrary simple regions
  (Greiner-Hormann polygon intersection); multi-ring and oversized
  regions fall back to their bounding box with a warning.
* Hard-edged alpha and luminance masks render as clips; soft and inverse
  masks warn and draw unmasked (not representable in DrawingML).
* Linear and radial gradient fills, including the radial focus position.
* Glyph API support (R >= 4.3, e.g. 'marquee' typeset text) via an
  optional 'systemfonts' glyph-to-character mapping.
* Exact `custDash` patterns for all line types; evenodd paths keep their
  holes; runs on R >= 3.6 with all newer graphics API behind version
  guards.
