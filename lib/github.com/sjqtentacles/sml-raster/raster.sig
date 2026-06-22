(* raster.sig

   A small, pure 2D rasterizer over the sml-image RGBA8 representation.

   Every public operation takes an `Image.image`, draws into a copy, and
   returns a new `Image.image`; the input is never mutated.  Internally each
   op works over a row-major `Word8Array.array` (4 bytes/pixel, RGBA, top-left
   origin) for efficiency and freezes it back into an `image` on the way out.

   All drawing is CLIPPED to the image bounds: pixels that fall outside the
   image are silently skipped, so drawing partially off-screen shapes is safe
   and never raises.

   Colors are `Image.rgba8` records.  `setPixel` overwrites the destination
   pixel; `blendPixel` performs straight alpha-over compositing:

       out = (src * a + dst * (255 - a) + 127) div 255

   per channel (rounded to nearest), where `a` is the source alpha.  The
   result alpha is composited the same way against the destination alpha. *)

signature RASTER =
sig
  type image = Image.image
  type rgba8 = Image.rgba8

  (* Make a blank w*h image filled with the given color (wraps Image.fill). *)
  val blank : int * int -> rgba8 -> image

  (* Opaque set of a single pixel (clipped). *)
  val setPixel : image -> int * int -> rgba8 -> image

  (* Alpha-over blend of a single pixel against the existing pixel (clipped). *)
  val blendPixel : image -> int * int -> rgba8 -> image

  (* Bresenham line between two endpoints (inclusive, clipped). *)
  val line : image -> { x0 : int, y0 : int, x1 : int, y1 : int } -> rgba8 -> image

  (* Rectangle outline. *)
  val rect : image -> { x : int, y : int, w : int, h : int } -> rgba8 -> image

  (* Filled rectangle. *)
  val fillRect : image -> { x : int, y : int, w : int, h : int } -> rgba8 -> image

  (* Midpoint circle outline. *)
  val circle : image -> { cx : int, cy : int, r : int } -> rgba8 -> image

  (* Filled circle (disk). *)
  val fillCircle : image -> { cx : int, cy : int, r : int } -> rgba8 -> image

  (* Triangle outline (three edges). *)
  val triangle : image -> (int * int) * (int * int) * (int * int) -> rgba8 -> image

  (* Filled triangle (scanline). *)
  val fillTriangle : image -> (int * int) * (int * int) * (int * int) -> rgba8 -> image

  (* Connected sequence of line segments through the given points. *)
  val polyline : image -> (int * int) list -> rgba8 -> image

  (* Filled polygon, even-odd scanline rule. *)
  val fillPolygon : image -> (int * int) list -> rgba8 -> image

  (* Copy `src` over the destination with its top-left at `dst` (clipped). *)
  val blit : image -> { dst : int * int, src : image } -> image
end
