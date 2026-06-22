(* font.sig

   Pure-Standard-ML bitmap font rendering for the sml-image RGBA8 raster.

   A `font` is parsed from a BDF (Glyph Bitmap Distribution Format) source: the
   subset of BDF emitted by real bitmap fonts (per-glyph BBX, DWIDTH, and a
   hex-encoded BITMAP block).  Each glyph is an immutable on/off bitmap plus an
   advance width; rendering scales each glyph pixel into a `scale`x`scale`
   opaque block and writes it into a copy of the destination image (the input
   image is never mutated).

   Everything here is total and deterministic: parsing, glyph lookup, measuring,
   and drawing all produce byte-identical results under MLton and Poly/ML.
   Malformed BDF input raises `Font`. *)

signature FONT =
sig
  exception Font of string

  type font

  (* Parse a BDF font from its full text.  Raises `Font` on malformed input. *)
  val parseBdf : string -> font

  (* The glyph for a character as a row-major on/off bitmap: `bits` has length
     `w * h`, indexed `row * w + col`, top-left origin.  Characters with no
     glyph fall back to the font's default glyph (BDF DEFAULT_CHAR, else '?',
     else a blank cell of the font's bounding-box size). *)
  val glyph : font -> char -> { w : int, h : int, bits : bool array }

  (* Draw `text` into a copy of `img` with the glyph origin (top-left of the
     first glyph cell) at (x, y).  Each on-pixel becomes a `scale`x`scale`
     opaque block of `color`; off-pixels are left untouched.  Drawing is clipped
     to the image bounds, so off-screen text is safe.  Newlines ('\n') move to
     the next line (advancing y by the font height) and reset x.  Advancing uses
     each glyph's BDF DWIDTH. *)
  val drawText :
       Image.image
    -> { x : int, y : int, scale : int, color : Image.rgba8 }
    -> font
    -> string
    -> Image.image

  (* Pixel extent (width, height) of `text` at scale 1: width is the sum of the
     glyph advances (the max line width when `text` spans multiple lines), and
     height is the font's pixel height times the number of lines.  The empty
     string measures (0, 0). *)
  val measure : font -> string -> int * int

  (* --- extras (stable, used by sml-plot and the demos) --- *)

  (* Font vertical extent in pixels (FONTBOUNDINGBOX height / ascent+descent). *)
  val height : font -> int

  (* Advance width in pixels of a single character (BDF DWIDTH). *)
  val advance : font -> char -> int

  (* Number of glyphs defined in the font. *)
  val numGlyphs : font -> int
end
