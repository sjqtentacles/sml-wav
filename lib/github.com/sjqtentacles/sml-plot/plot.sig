(* plot.sig

   Pure-Standard-ML charting to a raster image: line, bar, scatter, and
   histogram plots rendered onto the sml-image RGBA8 canvas, with automatic
   axis ranges, "nice"-number tick selection, gridlines, a title, axis labels,
   and an optional legend.  All text (title / axis labels / tick labels /
   legend) is drawn with the vendored sml-font bitmap font, so a chart is fully
   self-contained: no fonts, files, or FFI are read at runtime.

   `render` builds the image from scratch and never performs I/O; the caller is
   responsible for encoding it (e.g. `Image.encodePng`).  Everything here is
   total and deterministic: the same chart spec yields a byte-identical image
   under both MLton and Poly/ML.

   The numeric helpers (`clamp`, `niceNum`, `niceAxis`, `ticks`, `extent`,
   `project`) are the checkable core of the layout maths and are exposed so the
   data->pixel mapping can be tested independently of pixel rendering.  They use
   no transcendental functions (no `log`/`pow`), so axis selection is bit-stable
   across compilers. *)

signature PLOT =
sig
  type rgba8 = Image.rgba8

  (* A plottable data set.  Numeric series (`Line`, `Scatter`, `Hist`) live on a
     numeric x-axis; `Bar` is categorical and is laid out at integer slots
     0, 1, 2, ... with the supplied labels under each bar.  `Hist` bins its
     samples into equal-width buckets over their range. *)
  datatype series =
      Line    of (real * real) list   (* connected points, in list order *)
    | Bar     of (string * real) list (* labelled bars from a zero baseline *)
    | Scatter of (real * real) list   (* discrete points (drawn as marks) *)
    | Hist    of real list            (* samples, auto-binned into buckets *)

  (* Axis decoration.  `xlabel`/`ylabel` are drawn beside their axes ("" hides
     them); `grid` toggles the light gridlines at each tick. *)
  type axes = { xlabel : string, ylabel : string, grid : bool }

  (* A full chart specification.  `width`/`height` are the output pixel size;
     `series` are drawn back-to-front in list order; `title` is centred at the
     top ("" hides it); `legend` draws a per-series colour key when true. *)
  type chart =
    { width  : int
    , height : int
    , series : series list
    , title  : string
    , axes   : axes
    , legend : bool }

  (* Render a chart to an RGBA8 image.  Degenerate inputs are handled gracefully
     (empty series, flat data, zero/negative sizes clamp to a 1x1 image). *)
  val render : chart -> Image.image

  (* --- pure layout maths (deterministic, no rendering) --- *)

  (* `clamp lo hi v` constrains v to the closed interval [lo, hi].  If hi < lo
     the bounds are treated as swapped. *)
  val clamp : real -> real -> real -> real

  (* `niceNum (x, round)` returns a "nice" number near x: a value of the form
     1, 2, 5 (or 10) times a power of ten.  When `round` is true the closest
     nice number is chosen; otherwise the smallest nice number >= x.  `x` must
     be positive; non-positive x yields 0.0.  Heckbert's algorithm, computed
     with integer powers of ten only (bit-stable across compilers). *)
  val niceNum : real * bool -> real

  (* `niceAxis (lo, hi, target)` fits a nice axis covering [lo, hi] with about
     `target` ticks: it returns the rounded-out bounds and the step between
     ticks.  Flat input (hi = lo) is padded to a unit interval first. *)
  val niceAxis : real * real * int -> { lo : real, hi : real, step : real }

  (* The tick values for `niceAxis (lo, hi, target)`, from the nice low bound up
     to the nice high bound inclusive, evenly spaced by the chosen step. *)
  val ticks : real * real * int -> real list

  (* Data extent (min, max) of a list of reals.  The empty list yields (0, 1);
     a flat/singleton list is padded by 1 on each side so the range is never
     zero-width. *)
  val extent : real list -> real * real

  (* Linear map of a value from the data interval [dlo, dhi] onto the pixel
     interval [plo, phi] (reals; the caller rounds to a pixel).  A zero-width
     data interval maps everything to the midpoint.  `plo`/`phi` may be given
     in either order, so passing the bottom pixel as `plo` and the top as `phi`
     gives a y-axis that grows upward on screen. *)
  val project : { dlo : real, dhi : real, plo : real, phi : real } -> real -> real
end
