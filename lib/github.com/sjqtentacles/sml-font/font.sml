(* font.sml -- BDF bitmap font parsing and rendering into sml-image.

   See font.sig for the contract.  The parser handles the common BDF subset:
   a header (FONTBOUNDINGBOX, FONT_ASCENT/DESCENT, DEFAULT_CHAR), then a series
   of STARTCHAR..ENDCHAR records each carrying ENCODING, DWIDTH, BBX, and a
   hex-encoded BITMAP (one hex value per glyph row, leftmost pixel in the MSB,
   each row padded to a whole number of bytes). *)

structure Font :> FONT =
struct
  exception Font of string

  structure I = Image

  type glyph = { w : int, h : int, dwidth : int, bits : bool array }

  type font =
    { height  : int            (* pixel height (ascent + descent) *)
    , default : glyph          (* glyph used for missing characters *)
    , glyphs  : glyph option array  (* indexed by codepoint, 0..255 *)
    , ndef    : int }          (* number of glyphs defined *)

  (* --- small helpers --- *)

  fun toInt what s =
    case Int.fromString s of
        SOME n => n
      | NONE => raise Font ("expected integer in " ^ what ^ ": " ^ s)

  fun hexToInt s =
    case StringCvt.scanString (Int.scan StringCvt.HEX) s of
        SOME n => n
      | NONE => raise Font ("bad hex in BITMAP: " ^ s)

  fun pow2 n = if n <= 0 then 1 else 2 * pow2 (n - 1)
  fun bitOf (value, pos) = (value div pow2 pos) mod 2 = 1

  fun stripCR s =
    let val n = String.size s in
      if n > 0 andalso String.sub (s, n - 1) = #"\r"
      then String.substring (s, 0, n - 1) else s
    end

  fun toks line = String.tokens Char.isSpace line

  (* --- parser --- *)

  fun parseBdf text =
    let
      val lines = Vector.fromList (map stripCR (String.fields (fn c => c = #"\n") text))
      val n = Vector.length lines
      fun lineAt i = Vector.sub (lines, i)

      val bboxW = ref 0
      val bboxH = ref 0
      val ascent = ref ~1
      val descent = ref 0
      val defaultChar = ref ~1
      val sawStartFont = ref false
      val glyphArr = Array.array (256, NONE : glyph option)
      val count = ref 0

      (* Build a glyph record from its BBX dimensions and hex row values. *)
      fun makeGlyph (w, h, dwidth, hexRows) =
        let
          val numBytes = (w + 7) div 8
          val totalBits = numBytes * 8
          val bits = Array.array (Int.max (w * h, 0), false)
          fun setRow (r, value) =
            let
              fun col c =
                if c >= w then ()
                else ( if bitOf (value, totalBits - 1 - c)
                       then Array.update (bits, r * w + c, true) else ()
                     ; col (c + 1) )
            in col 0 end
          fun rows (_, []) = ()
            | rows (r, v :: rest) = (setRow (r, v); rows (r + 1, rest))
        in
          rows (0, hexRows);
          { w = w, h = h, dwidth = dwidth, bits = bits }
        end

      (* Parse one STARTCHAR..ENDCHAR block beginning at line i; return the
         index just past ENDCHAR. *)
      fun parseChar i =
        let
          val enc = ref ~1
          val dwx = ref 0
          val bw = ref 0
          val bh = ref 0

          fun finish (i, hexRows) =
            let
              val g = makeGlyph (!bw, !bh, !dwx, hexRows)
            in
              if !enc >= 0 andalso !enc < 256
              then Array.update (glyphArr, !enc, SOME g) else ();
              count := !count + 1;
              i
            end

          (* read exactly !bh hex lines, then expect ENDCHAR *)
          fun readBits (i, k, acc) =
            if k >= !bh then
              (case toks (lineAt i) of
                   ("ENDCHAR" :: _) => finish (i + 1, List.rev acc)
                 | _ => raise Font "expected ENDCHAR after BITMAP")
            else if i >= n then raise Font "unterminated BITMAP"
            else (case toks (lineAt i) of
                      [] => readBits (i + 1, k, acc)
                    | (hx :: _) => readBits (i + 1, k + 1, hexToInt hx :: acc))

          fun go i =
            if i >= n then raise Font "unterminated STARTCHAR"
            else (case toks (lineAt i) of
                      [] => go (i + 1)
                    | (kw :: rest) =>
                        (case kw of
                             "ENCODING" =>
                               (enc := toInt "ENCODING" (hd rest); go (i + 1))
                           | "DWIDTH" =>
                               (dwx := toInt "DWIDTH" (hd rest); go (i + 1))
                           | "BBX" =>
                               (case rest of
                                    (w :: h :: _) =>
                                      ( bw := toInt "BBX" w
                                      ; bh := toInt "BBX" h
                                      ; go (i + 1) )
                                  | _ => raise Font "malformed BBX")
                           | "BITMAP" => readBits (i + 1, 0, [])
                           | "ENDCHAR" => finish (i + 1, [])
                           | _ => go (i + 1)))
        in
          go i
        end

      fun parse i =
        if i >= n then ()
        else (case toks (lineAt i) of
                  [] => parse (i + 1)
                | (kw :: rest) =>
                    (case kw of
                         "STARTFONT" => (sawStartFont := true; parse (i + 1))
                       | "FONTBOUNDINGBOX" =>
                           (case rest of
                                (w :: h :: _) =>
                                  ( bboxW := toInt "FONTBOUNDINGBOX" w
                                  ; bboxH := toInt "FONTBOUNDINGBOX" h
                                  ; parse (i + 1) )
                              | _ => raise Font "malformed FONTBOUNDINGBOX")
                       | "FONT_ASCENT" =>
                           (ascent := toInt "FONT_ASCENT" (hd rest); parse (i + 1))
                       | "FONT_DESCENT" =>
                           (descent := toInt "FONT_DESCENT" (hd rest); parse (i + 1))
                       | "DEFAULT_CHAR" =>
                           (defaultChar := toInt "DEFAULT_CHAR" (hd rest); parse (i + 1))
                       | "STARTCHAR" => parse (parseChar (i + 1))
                       | _ => parse (i + 1)))

      val () = parse 0

      val () =
        if not (!sawStartFont) then raise Font "missing STARTFONT"
        else if !count = 0 then raise Font "no glyphs defined" else ()

      val pixHeight =
        if !ascent >= 0 then !ascent + !descent
        else if !bboxH > 0 then !bboxH else 0

      fun pick code =
        if code >= 0 andalso code < 256 then Array.sub (glyphArr, code) else NONE

      val blank =
        { w = Int.max (!bboxW, 0), h = Int.max (!bboxH, 0)
        , dwidth = Int.max (!bboxW, 1)
        , bits = Array.array (Int.max (!bboxW * !bboxH, 0), false) }

      val default =
        case pick (!defaultChar) of
            SOME g => g
          | NONE => (case pick (Char.ord #"?") of
                         SOME g => g
                       | NONE => blank)
    in
      { height = pixHeight, default = default, glyphs = glyphArr, ndef = !count }
    end

  (* --- lookup --- *)

  fun lookup (f : font) ch =
    let val code = Char.ord ch in
      if code >= 0 andalso code < 256 then
        (case Array.sub (#glyphs f, code) of
             SOME g => g
           | NONE => #default f)
      else #default f
    end

  fun glyph f ch =
    let
      val g = lookup f ch
      val src = #bits g
      val bits = Array.tabulate (Array.length src, fn i => Array.sub (src, i))
    in
      { w = #w g, h = #h g, bits = bits }
    end

  fun height (f : font) = #height f
  fun numGlyphs (f : font) = #ndef f
  fun advance f ch = #dwidth (lookup f ch)

  (* --- measure --- *)

  fun measure f s =
    if s = "" then (0, 0)
    else
      let
        val lns = String.fields (fn c => c = #"\n") s
        fun lineWidth l = CharVector.foldl (fn (c, acc) => acc + advance f c) 0 l
        val w = List.foldl (fn (l, m) => Int.max (m, lineWidth l)) 0 lns
        val h = (List.length lns) * (height f)
      in
        (w, h)
      end

  (* --- drawText --- *)

  fun drawText (img : I.image)
               ({ x, y, scale, color }
                  : { x : int, y : int, scale : int, color : I.rgba8 })
               f text =
    let
      val iw = #width img
      val ih = #height img
      val idata = #data img
      val arr = Word8Array.array (Word8Vector.length idata, 0w0)
      val () = Word8Array.copyVec { src = idata, dst = arr, di = 0 }

      val cr = #r color
      val cg = #g color
      val cb = #b color
      val ca = #a color
      val sc = if scale < 1 then 1 else scale

      fun setpx (px, py) =
        if px < 0 orelse px >= iw orelse py < 0 orelse py >= ih then ()
        else
          let val off = 4 * (py * iw + px) in
            Word8Array.update (arr, off, cr);
            Word8Array.update (arr, off + 1, cg);
            Word8Array.update (arr, off + 2, cb);
            Word8Array.update (arr, off + 3, ca)
          end

      fun block (bx, by) =
        let
          fun yy j =
            if j >= sc then ()
            else
              let
                fun xx i =
                  if i >= sc then () else (setpx (bx + i, by + j); xx (i + 1))
              in xx 0; yy (j + 1) end
        in yy 0 end

      fun drawGlyph (penx, peny, g : glyph) =
        let
          val w = #w g and h = #h g and bits = #bits g
          fun loop idx =
            if idx >= w * h then ()
            else
              ( if Array.sub (bits, idx) then
                  let val r = idx div w and c = idx mod w in
                    block (penx + c * sc, peny + r * sc)
                  end
                else ()
              ; loop (idx + 1) )
        in loop 0 end

      fun run (i, penx, peny) =
        if i >= String.size text then ()
        else
          let val ch = String.sub (text, i) in
            if ch = #"\n" then run (i + 1, x, peny + (height f) * sc)
            else
              let val g = lookup f ch in
                drawGlyph (penx, peny, g);
                run (i + 1, penx + (#dwidth g) * sc, peny)
              end
          end

      val () = run (0, x, y)
    in
      { width = iw, height = ih, data = Word8Array.vector arr }
    end
end
