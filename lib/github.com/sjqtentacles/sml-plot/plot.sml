(* plot.sml -- pure-SML charting onto the sml-image RGBA8 canvas.

   See plot.sig for the contract.  The file has two halves:

     1. the layout maths (clamp / niceNum / niceAxis / ticks / extent /
        project) -- small, total, deterministic, and unit-tested; it uses only
        the four basic arithmetic ops plus integer powers of ten, so axis
        selection is bit-identical across compilers; and

     2. `render`, which wires that maths to sml-raster (shapes) and sml-font
        (all text) to compose a chart.  Rendering builds the image from
        scratch, performs no I/O, and never mutates its inputs. *)

structure Plot :> PLOT =
struct
  structure I = Image
  structure R = Raster
  structure F = Font

  type rgba8 = I.rgba8

  datatype series =
      Line    of (real * real) list
    | Bar     of (string * real) list
    | Scatter of (real * real) list
    | Hist    of real list

  type axes = { xlabel : string, ylabel : string, grid : bool }

  type chart =
    { width  : int
    , height : int
    , series : series list
    , title  : string
    , axes   : axes
    , legend : bool }

  (* The bundled bitmap font, parsed once at module load (no runtime I/O). *)
  val font = F.parseBdf FontData.bdf

  (* ================= layout maths (pure, tested) ================= *)

  fun clamp (lo : real) (hi : real) (v : real) : real =
    let val (lo, hi) = if hi < lo then (hi, lo) else (lo, hi)
    in if v < lo then lo else if v > hi then hi else v end

  (* Deterministic round-half-up: floor(x + 0.5).  Unlike Real.round (ties to
     even, and historically compiler-dependent on exact halves) this is
     bit-identical across MLton and Poly/ML for identical real inputs, which is
     what keeps a rendered chart byte-stable across compilers. *)
  fun iround x = Real.floor (x + 0.5)

  (* 10^e for integer e, by repeated multiply/divide (deterministic). *)
  fun ipow10 e =
    if e = 0 then 1.0
    else if e > 0 then 10.0 * ipow10 (e - 1)
    else ipow10 (e + 1) / 10.0

  (* floor(log10 x) for x > 0, found by comparison only -- no transcendentals,
     so it is bit-stable across MLton and Poly/ML even at exact powers of ten. *)
  fun exp10Floor x =
    let
      fun up e = if ipow10 (e + 1) <= x then up (e + 1) else e
      fun dn e = if ipow10 e > x then dn (e - 1) else e
    in
      if x >= 1.0 then up 0 else dn ~1
    end

  fun niceNum (x, round) =
    if x <= 0.0 then 0.0
    else
      let
        val e = exp10Floor x
        val p = ipow10 e
        val f = x / p
        val nf =
          if round then
            (if f < 1.5 then 1.0
             else if f < 3.0 then 2.0
             else if f < 7.0 then 5.0
             else 10.0)
          else
            (if f <= 1.0 then 1.0
             else if f <= 2.0 then 2.0
             else if f <= 5.0 then 5.0
             else 10.0)
      in
        nf * p
      end

  fun niceAxis (lo, hi, target) =
    let
      val target = if target < 2 then 2 else target
      val (lo, hi) =
        if Real.== (lo, hi) then (lo - 0.5, hi + 0.5)
        else (Real.min (lo, hi), Real.max (lo, hi))
      val range = niceNum (hi - lo, false)
      val step = niceNum (range / real (target - 1), true)
      val step = if step <= 0.0 then 1.0 else step
      val nlo = Real.realFloor (lo / step) * step
      val nhi = Real.realCeil (hi / step) * step
    in
      { lo = nlo, hi = nhi, step = step }
    end

  fun ticks (lo, hi, target) =
    let
      val { lo = a, hi = b, step } = niceAxis (lo, hi, target)
      val cnt = iround ((b - a) / step)
      val cnt = if cnt < 0 then 0 else cnt
    in
      List.tabulate (cnt + 1, fn i => a + real i * step)
    end

  fun extent xs =
    case xs of
      [] => (0.0, 1.0)
    | x0 :: rest =>
        let
          val lo = foldl (fn (x, m) => Real.min (x, m)) x0 rest
          val hi = foldl (fn (x, m) => Real.max (x, m)) x0 rest
        in
          if Real.== (lo, hi) then (lo - 1.0, hi + 1.0) else (lo, hi)
        end

  fun project { dlo, dhi, plo, phi } v =
    if Real.== (dlo, dhi) then (plo + phi) / 2.0
    else plo + (v - dlo) / (dhi - dlo) * (phi - plo)

  (* ================= small rendering helpers ================= *)

  fun rgba (r, g, b) : rgba8 =
    { r = Word8.fromInt r, g = Word8.fromInt g, b = Word8.fromInt b, a = 0w255 }

  (* palette *)
  val cBg     = rgba (18, 20, 28)
  val cPanel  = rgba (28, 32, 44)
  val cBorder = rgba (70, 78, 104)
  val cGrid   = rgba (44, 50, 68)
  val cAxis   = rgba (150, 160, 185)
  val cText   = rgba (224, 230, 242)
  val cMuted  = rgba (150, 160, 185)

  val seriesPalette =
    Vector.fromList
      [ rgba (120, 210, 235)   (* cyan *)
      , rgba (240, 190,  96)   (* amber *)
      , rgba (130, 220, 150)   (* green *)
      , rgba (235, 120, 140)   (* pink *)
      , rgba (180, 150, 240)   (* violet *)
      , rgba (240, 140,  90) ] (* orange *)

  fun seriesColor i = Vector.sub (seriesPalette, i mod Vector.length seriesPalette)

  fun seriesLabel s =
    case s of
      Line _    => "line"
    | Bar _     => "bars"
    | Scatter _ => "scatter"
    | Hist _    => "histogram"

  (* integer 10^d for d >= 0 *)
  fun ipow10i d = if d <= 0 then 1 else 10 * ipow10i (d - 1)

  (* decimal places needed to show a tick step without losing it *)
  fun decimalsFor step =
    if step <= 0.0 then 0
    else
      let
        fun go (s, d) = if s >= 0.999999 orelse d >= 6 then d else go (s * 10.0, d + 1)
      in
        go (step, 0)
      end

  fun padLeftZeros (s, width) =
    if String.size s >= width then s
    else padLeftZeros ("0" ^ s, width)

  (* deterministic fixed-decimal formatting via integer rounding *)
  fun fmtNum (v, d) =
    let
      val neg = v < 0.0
      val sc = ipow10i d
      val n = iround (Real.abs v * real sc)
      val ip = n div sc
      val fp = n mod sc
      val body =
        if d <= 0 then Int.toString ip
        else Int.toString ip ^ "." ^ padLeftZeros (Int.toString fp, d)
    in
      if neg andalso n <> 0 then "~" ^ body else body
    end

  (* sml-font has no '-' glyph distinct from '~'? It does have '-'. Use it. *)
  fun fmtTick (v, d) =
    let val s = fmtNum (v, d)
    in if String.size s > 0 andalso String.sub (s, 0) = #"~"
       then "-" ^ String.extract (s, 1, NONE) else s end

  (* draw text at scale, returning the new image *)
  fun text img (x, y) scale color s =
    F.drawText img { x = x, y = y, scale = scale, color = color } font s

  (* draw `s` vertically (one char per line) for a y-axis label *)
  fun vtext img (x, y) scale color s =
    let
      val chars = List.tabulate (String.size s, fn i => String.str (String.sub (s, i)))
      val stacked = String.concatWith "\n" chars
    in
      F.drawText img { x = x, y = y, scale = scale, color = color } font stacked
    end

  (* ================= series -> numeric (x, y) contributions ================= *)

  (* integer ceil(sqrt n), by comparison only (deterministic) *)
  fun isqrtCeil n =
    if n <= 1 then 1
    else
      let fun go k = if k * k >= n then k else go (k + 1)
      in go 1 end

  fun histBins samples =
    let
      val len = List.length samples
      val nb = Int.min (20, Int.max (1, isqrtCeil len))
      val (lo, hi) = extent samples
      val (lo, hi) = if Real.== (lo, hi) then (lo, lo + 1.0) else (lo, hi)
      val width = (hi - lo) / real nb
      val counts = Array.array (nb, 0)
      fun bump s =
        let
          val raw = Real.floor ((s - lo) / width)
          val idx = if raw < 0 then 0 else if raw >= nb then nb - 1 else raw
        in
          Array.update (counts, idx, Array.sub (counts, idx) + 1)
        end
      val () = List.app bump samples
    in
      { lo = lo, width = width, nb = nb, counts = counts }
    end

  (* x and y values a series contributes to the auto-range computation *)
  fun seriesXY s =
    case s of
      Line pts    => (map #1 pts, map #2 pts)
    | Scatter pts => (map #1 pts, map #2 pts)
    | Bar cells   =>
        let val n = List.length cells
        in (List.tabulate (n, fn i => real i), 0.0 :: map #2 cells) end
    | Hist samples =>
        if List.null samples then ([], [0.0])
        else
          let
            val { lo, width, nb, counts } = histBins samples
            val xs = [lo, lo + width * real nb]
            val ys = 0.0 :: List.tabulate (nb, fn i => real (Array.sub (counts, i)))
          in (xs, ys) end

  (* ================= render ================= *)

  fun render (chart : chart) =
    let
      val W = Int.max (#width chart, 1)
      val Ht = Int.max (#height chart, 1)
      val { series, title, axes, legend, ... } = chart
      val { xlabel, ylabel, grid } = axes

      (* margins *)
      val mL = 54
      val mR = 16
      val mT = if title = "" then 18 else 32
      val mB = 42
      val plotX0 = mL
      val plotY0 = mT
      val plotX1 = W - mR
      val plotY1 = Ht - mB
      val plotW = plotX1 - plotX0
      val plotH = plotY1 - plotY0
      val canPlot = plotW > 4 andalso plotH > 4

      (* combined data ranges *)
      val allXs = List.concat (map (#1 o seriesXY) series)
      val allYs = List.concat (map (#2 o seriesXY) series)
      val (xlo0, xhi0) = extent allXs
      val (ylo0, yhi0) = extent allYs
      val xa = niceAxis (xlo0, xhi0, 6)
      val ya = niceAxis (ylo0, yhi0, 5)
      val xlo = #lo xa and xhi = #hi xa
      val ylo = #lo ya and yhi = #hi ya
      val xStep = #step xa and yStep = #step ya

      fun fx x = project { dlo = xlo, dhi = xhi, plo = real plotX0, phi = real plotX1 } x
      fun fy y = project { dlo = ylo, dhi = yhi, plo = real plotY1, phi = real plotY0 } y
      fun px x = iround (fx x)
      fun py y = iround (fy y)

      (* --- background + panel + frame --- *)
      val img = R.blank (W, Ht) cBg
      val img =
        if canPlot then
          let
            val img = R.fillRect img { x = plotX0, y = plotY0, w = plotW, h = plotH } cPanel
          in img end
        else img

      (* --- gridlines + ticks + tick labels --- *)
      val img =
        if not canPlot then img
        else
          let
            val xticks = ticks (xlo0, xhi0, 6)
            val yticks = ticks (ylo0, yhi0, 5)
            val xd = decimalsFor xStep
            val yd = decimalsFor yStep

            (* y gridlines + labels *)
            fun doY (t, im) =
              let
                val yy = py t
                val im =
                  if grid then
                    R.line im { x0 = plotX0, y0 = yy, x1 = plotX1, y1 = yy } cGrid
                  else im
                val im = R.line im { x0 = plotX0 - 4, y0 = yy, x1 = plotX0, y1 = yy } cAxis
                val lbl = fmtTick (t, yd)
                val (lw, _) = F.measure font lbl
                val im = text im (plotX0 - 6 - lw, yy - 3) 1 cMuted lbl
              in im end
            val img = foldl doY img yticks

            (* x gridlines + labels *)
            fun doX (t, im) =
              let
                val xx = px t
                val im =
                  if grid then
                    R.line im { x0 = xx, y0 = plotY0, x1 = xx, y1 = plotY1 } cGrid
                  else im
                val im = R.line im { x0 = xx, y0 = plotY1, x1 = xx, y1 = plotY1 + 4 } cAxis
                val lbl = fmtTick (t, xd)
                val (lw, _) = F.measure font lbl
                val im = text im (xx - lw div 2, plotY1 + 6) 1 cMuted lbl
              in im end
            val img = foldl doX img xticks
          in img end

      (* --- axis lines + frame --- *)
      val img =
        if not canPlot then img
        else
          let
            val img = R.line img { x0 = plotX0, y0 = plotY0, x1 = plotX0, y1 = plotY1 } cAxis
            val img = R.line img { x0 = plotX0, y0 = plotY1, x1 = plotX1, y1 = plotY1 } cAxis
            val img = R.rect img { x = plotX0, y = plotY0, w = plotW, h = plotH } cBorder
          in img end

      (* --- the series themselves --- *)
      fun drawLine (pts, color, im) =
        let
          fun seg ((x0, y0), (x1, y1), im) =
            R.line im { x0 = px x0, y0 = py y0, x1 = px x1, y1 = py y1 } color
        in
          case pts of
            [] => im
          | first :: rest =>
              let
                fun loop (_, [], im) = im
                  | loop (prev, p :: more, im) = loop (p, more, seg (prev, p, im))
              in loop (first, rest, im) end
        end

      fun drawScatter (pts, color, im) =
        foldl (fn ((x, y), im) =>
                  R.fillCircle im { cx = px x, cy = py y, r = 3 } color)
              im pts

      fun drawBars (cells, color, im) =
        let
          val n = List.length cells
          fun bar (i, (lbl, v), im) =
            let
              val xc = real i
              val x0 = px (xc - 0.4)
              val x1 = px (xc + 0.4)
              val yb = py 0.0
              val yv = py v
              val top = Int.min (yb, yv)
              val ht = Int.max (1, Int.abs (yb - yv))
              val w = Int.max (1, x1 - x0)
              val im = R.fillRect im { x = x0, y = top, w = w, h = ht } color
              (* category label centred under the bar *)
              val (lw, _) = F.measure font lbl
              val cx = (x0 + x1) div 2
              val im = text im (cx - lw div 2, plotY1 + 6) 1 cMuted lbl
            in im end
          fun loop (_, [], im) = im
            | loop (i, c :: rest, im) = loop (i + 1, rest, bar (i, c, im))
        in loop (0, cells, im) end

      fun drawHist (samples, color, im) =
        if List.null samples then im
        else
          let
            val { lo, width, nb, counts } = histBins samples
            fun bin (j, im) =
              if j >= nb then im
              else
                let
                  val c = Array.sub (counts, j)
                  val x0 = px (lo + width * real j)
                  val x1 = px (lo + width * real (j + 1))
                  val yb = py 0.0
                  val yv = py (real c)
                  val top = Int.min (yb, yv)
                  val ht = Int.max (1, Int.abs (yb - yv))
                  val w = Int.max (1, x1 - x0 - 1)
                  val im =
                    if c > 0 then R.fillRect im { x = x0, y = top, w = w, h = ht } color
                    else im
                in bin (j + 1, im) end
          in bin (0, im) end

      fun drawSeries (i, s, im) =
        let val color = seriesColor i in
          case s of
            Line pts    => drawLine (pts, color, im)
          | Scatter pts => drawScatter (pts, color, im)
          | Bar cells   => drawBars (cells, color, im)
          | Hist xs     => drawHist (xs, color, im)
        end

      val img =
        if not canPlot then img
        else
          let
            fun loop (_, [], im) = im
              | loop (i, s :: rest, im) = loop (i + 1, rest, drawSeries (i, s, im))
          in loop (0, series, img) end

      (* --- title --- *)
      val img =
        if title = "" then img
        else
          let
            val (tw, _) = F.measure font title
            val tx = plotX0 + (plotW - tw * 2) div 2
            val tx = if tx < 4 then 4 else tx
          in text img (tx, 9) 2 cText title end

      (* --- axis labels --- *)
      val img =
        if xlabel = "" orelse not canPlot then img
        else
          let
            val (lw, _) = F.measure font xlabel
            val lx = plotX0 + (plotW - lw * 2) div 2
            val lx = if lx < 4 then 4 else lx
          in text img (lx, plotY1 + 18) 2 cAxis xlabel end

      val img =
        if ylabel = "" orelse not canPlot then img
        else
          let
            val lh = String.size ylabel * F.height font   (* stacked, scale 1 *)
            val ly = plotY0 + (plotH - lh) div 2
            val ly = if ly < plotY0 then plotY0 else ly
          in vtext img (6, ly) 1 cAxis ylabel end

      (* --- legend --- *)
      val img =
        if not legend orelse not canPlot orelse List.null series then img
        else
          let
            val labels = List.tabulate (List.length series, fn i =>
              seriesLabel (List.nth (series, i)))
            val maxLw = foldl (fn (l, m) => Int.max (m, #1 (F.measure font l))) 0 labels
            val rowH = 12
            val boxW = maxLw + 22
            val boxH = rowH * List.length series + 6
            val bx = plotX1 - boxW - 6
            val by = plotY0 + 6
            val bx = if bx < plotX0 + 2 then plotX0 + 2 else bx
            val img = R.fillRect img { x = bx, y = by, w = boxW, h = boxH } cPanel
            val img = R.rect img { x = bx, y = by, w = boxW, h = boxH } cBorder
            val n = List.length series
            fun row (i, im) =
              if i >= n then im
              else
                let
                  val ry = by + 4 + i * rowH
                  val sw = seriesColor i
                  val im = R.fillRect im { x = bx + 5, y = ry + 1, w = 10, h = 6 } sw
                  val im = text im (bx + 19, ry) 1 cText (List.nth (labels, i))
                in row (i + 1, im) end
          in
            row (0, img)
          end
    in
      img
    end
end
