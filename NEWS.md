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
