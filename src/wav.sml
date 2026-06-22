(* wav.sml

   WAV (RIFF/PCM) container I/O plus oscillators, an ADSR envelope, amplitude
   helpers and RBJ biquad filters, sealed behind `WAV`.  Everything is pure and
   deterministic: byte building goes through a single `Word8Array`, sample
   quantization uses `floor (x + 0.5)` (not `Real.round`), and the only
   transcendental code is `Math.sin`/`Math.cos` (which both compilers defer to
   `libm`), so encoded output is byte-identical across MLton and Poly/ML. *)

structure Wav :> WAV =
struct
  exception Wav of string

  type pcm = { rate : int, channels : int, samples : real array }

  val pi = Math.pi

  (* ------------------------------------------------------------------ *)
  (* sample <-> integer-code conversion                                 *)
  (* ------------------------------------------------------------------ *)

  fun clampS s = if s > 1.0 then 1.0 else if s < ~1.0 then ~1.0 else s

  (* Symmetric fixed-point: full scale maps to +/- (2^(bits-1) - 1). *)
  fun scaleOf 16 = 32767.0
    | scaleOf 24 = 8388607.0
    | scaleOf 8  = 127.0
    | scaleOf 32 = 2147483647.0
    | scaleOf _  = raise Wav "unsupported bit depth"

  (* Round-half-up to a signed integer code; deterministic across compilers. *)
  fun encodeSample (bits, s) = Real.floor (clampS s * scaleOf bits + 0.5)

  (* ------------------------------------------------------------------ *)
  (* encode                                                             *)
  (* ------------------------------------------------------------------ *)

  fun encodeBits bits ({ rate, channels, samples } : pcm) =
    let
      val () = if bits = 16 orelse bits = 24 then ()
               else raise Wav "encode supports only 16- or 24-bit PCM"
      val () = if channels < 1 then raise Wav "channels must be >= 1" else ()
      val n   = Array.length samples
      val () = if n mod channels <> 0 then
                 raise Wav "sample count is not a whole number of frames"
               else ()
      val bps      = bits div 8
      val dataSize = n * bps
      val total    = 44 + dataSize
      val out      = Word8Array.array (total, 0w0)

      fun pb (i, b) = Word8Array.update (out, i, Word8.fromInt (b mod 256))
      fun putTag (i, s) =
        CharVector.appi (fn (k, c) => pb (i + k, Char.ord c)) s
      fun putU16 (i, v) = (pb (i, v); pb (i + 1, v div 256))
      fun putU32 (i, v) =
        ( pb (i, v); pb (i + 1, v div 256)
        ; pb (i + 2, v div 65536); pb (i + 3, v div 16777216) )

      val byteRate   = rate * channels * bps
      val blockAlign = channels * bps
      val limit      = if bits = 16 then 65536 else 16777216

      fun putSample (i, code) =
        let val u = if code < 0 then code + limit else code
        in
          if bits = 16 then putU16 (i, u)
          else (pb (i, u); pb (i + 1, u div 256); pb (i + 2, u div 65536))
        end

      val () = putTag (0, "RIFF")
      val () = putU32 (4, 36 + dataSize)
      val () = putTag (8, "WAVE")
      val () = putTag (12, "fmt ")
      val () = putU32 (16, 16)             (* PCM fmt chunk body size *)
      val () = putU16 (20, 1)              (* AudioFormat = 1 (PCM)   *)
      val () = putU16 (22, channels)
      val () = putU32 (24, rate)
      val () = putU32 (28, byteRate)
      val () = putU16 (32, blockAlign)
      val () = putU16 (34, bits)
      val () = putTag (36, "data")
      val () = putU32 (40, dataSize)

      fun loop i =
        if i >= n then ()
        else ( putSample (44 + i * bps, encodeSample (bits, Array.sub (samples, i)))
             ; loop (i + 1) )
      val () = loop 0
    in
      Word8Array.vector out
    end

  fun encode p = encodeBits 16 p

  (* ------------------------------------------------------------------ *)
  (* decode                                                             *)
  (* ------------------------------------------------------------------ *)

  fun decode (v : Word8Vector.vector) =
    let
      val len = Word8Vector.length v
      fun byte i = Word8.toInt (Word8Vector.sub (v, i))
      fun u16 i = byte i + byte (i + 1) * 256
      fun u32 i =
        byte i + byte (i + 1) * 256 + byte (i + 2) * 65536 + byte (i + 3) * 16777216
      fun tag i = String.implode (List.tabulate (4, fn k => Char.chr (byte (i + k))))

      val () = if len < 12 then raise Wav "input too short for a WAVE header"
               else ()
      val () = if tag 0 = "RIFF" andalso tag 8 = "WAVE" then ()
               else raise Wav "not a RIFF/WAVE file"

      (* Walk the chunk list looking for `fmt ` and `data`. *)
      fun scan (off, fmt, data) =
        if off + 8 > len then (fmt, data)
        else
          let
            val id   = tag off
            val sz   = u32 (off + 4)
            val body = off + 8
            val fmt'  = if id = "fmt " then SOME body else fmt
            val data' = if id = "data" then SOME (body, sz) else data
            val next  = body + sz + (sz mod 2)   (* chunks are word-aligned *)
          in
            case (fmt', data') of
              (SOME _, SOME _) => (fmt', data')
            | _ => scan (next, fmt', data')
          end
      val (fmtO, dataO) = scan (12, NONE, NONE)
      val fmt = case fmtO of SOME f => f | NONE => raise Wav "missing fmt chunk"
      val (dataOff, dataSz0) =
        case dataO of SOME d => d | NONE => raise Wav "missing data chunk"

      val channels = u16 (fmt + 2)
      val rate     = u32 (fmt + 4)
      val bits     = u16 (fmt + 14)
      val bps      = bits div 8
      val () = if bps = 0 then raise Wav "bad bit depth in fmt chunk" else ()

      val dataSz   = Int.min (dataSz0, len - dataOff)
      val nSamples = dataSz div bps
      val scale    = scaleOf bits

      fun sampleAt k =
        let val i = dataOff + k * bps in
          case bits of
            8 => (real (byte i) - 128.0) / 128.0   (* 8-bit WAV is unsigned *)
          | 16 =>
              let val u = u16 i
                  val s = if u >= 32768 then u - 65536 else u
              in real s / scale end
          | 24 =>
              let val u = byte i + byte (i + 1) * 256 + byte (i + 2) * 65536
                  val s = if u >= 8388608 then u - 16777216 else u
              in real s / scale end
          | 32 =>
              let
                val b3 = byte (i + 3)
                val hi = if b3 >= 128 then b3 - 256 else b3   (* signed top byte *)
                val s  = real (byte i) + real (byte (i + 1)) * 256.0
                         + real (byte (i + 2)) * 65536.0 + real hi * 16777216.0
              in s / scale end
          | _ => raise Wav "unsupported bit depth"
        end
    in
      { rate = rate, channels = channels
      , samples = Array.tabulate (nSamples, sampleAt) }
    end

  (* ------------------------------------------------------------------ *)
  (* oscillators                                                        *)
  (* ------------------------------------------------------------------ *)

  fun numSamples (dur, rate) = Int.max (0, Real.floor (dur * real rate))

  fun frac x = x - Real.realFloor x

  fun sine { freq, dur, rate } =
    Array.tabulate
      (numSamples (dur, rate),
       fn j => Math.sin (2.0 * pi * freq * real j / real rate))

  fun saw { freq, dur, rate } =
    Array.tabulate
      (numSamples (dur, rate),
       fn j => 2.0 * frac (freq * real j / real rate) - 1.0)

  fun square { freq, dur, rate } =
    Array.tabulate
      (numSamples (dur, rate),
       fn j => if frac (freq * real j / real rate) < 0.5 then 1.0 else ~1.0)

  (* ------------------------------------------------------------------ *)
  (* amplitude helpers                                                  *)
  (* ------------------------------------------------------------------ *)

  fun peak a =
    Array.foldl (fn (x, m) => Real.max (m, Real.abs x)) 0.0 a

  fun rms a =
    let val n = Array.length a in
      if n = 0 then 0.0
      else Math.sqrt (Array.foldl (fn (x, s) => s + x * x) 0.0 a / real n)
    end

  fun gain k a = Array.tabulate (Array.length a, fn i => k * Array.sub (a, i))

  fun normalize target a =
    let val p = peak a
    in if p <= 0.0 then a else gain (target / p) a end

  fun mix [] = Array.fromList []
    | mix arrays =
        let
          val n = List.foldl (fn (a, m) => Int.max (m, Array.length a)) 0 arrays
          fun sumAt i =
            List.foldl
              (fn (a, acc) =>
                 if i < Array.length a then acc + Array.sub (a, i) else acc)
              0.0 arrays
        in
          Array.tabulate (n, sumAt)
        end

  (* ------------------------------------------------------------------ *)
  (* ADSR envelope                                                      *)
  (* ------------------------------------------------------------------ *)

  structure Adsr =
  struct
    type t = { attack : real, decay : real, sustain : real, release : real }

    fun envCore ({ attack, decay, sustain, release } : t, rate, n) =
      let
        val aN = Int.max (0, Real.floor (attack  * real rate))
        val dN = Int.max (0, Real.floor (decay   * real rate))
        val rN = Int.max (0, Real.floor (release * real rate))
        val sStart = n - rN
        fun g i =
          if i < aN then
            (if aN = 0 then 1.0 else real i / real aN)                 (* attack *)
          else if i < aN + dN then
            (if dN = 0 then sustain
             else 1.0 - (1.0 - sustain) * (real (i - aN) / real dN))   (* decay *)
          else if i < sStart then sustain                             (* sustain *)
          else
            (if rN = 0 then 0.0
             else sustain * (1.0 - real (i - sStart) / real rN))       (* release *)
      in
        Array.tabulate (n, g)
      end

    fun envelope params { rate, dur } =
      envCore (params, rate, Int.max (0, Real.floor (dur * real rate)))

    fun apply params rate signal =
      let
        val n = Array.length signal
        val e = envCore (params, rate, n)
      in
        Array.tabulate (n, fn i => Array.sub (signal, i) * Array.sub (e, i))
      end
  end

  (* ------------------------------------------------------------------ *)
  (* biquad filters (RBJ audio-EQ cookbook, Direct Form I, zero state)  *)
  (* ------------------------------------------------------------------ *)

  fun runBiquad (b0, b1, b2, a0, a1, a2) signal =
    let
      val n   = Array.length signal
      val nb0 = b0 / a0 and nb1 = b1 / a0 and nb2 = b2 / a0
      val na1 = a1 / a0 and na2 = a2 / a0
      val out = Array.array (n, 0.0)
      val x1 = ref 0.0 and x2 = ref 0.0 and y1 = ref 0.0 and y2 = ref 0.0
      fun loop i =
        if i >= n then ()
        else
          let
            val x0 = Array.sub (signal, i)
            val y0 = nb0 * x0 + nb1 * (!x1) + nb2 * (!x2)
                     - na1 * (!y1) - na2 * (!y2)
          in
            Array.update (out, i, y0);
            x2 := !x1; x1 := x0; y2 := !y1; y1 := y0;
            loop (i + 1)
          end
    in
      loop 0; out
    end

  fun coeffs { cutoff, q, rate } =
    let
      val w0    = 2.0 * pi * cutoff / real rate
      val cosw  = Math.cos w0
      val sinw  = Math.sin w0
      val alpha = sinw / (2.0 * q)
    in
      { cosw = cosw, alpha = alpha }
    end

  fun biquadLowpass spec =
    let
      val { cosw, alpha } = coeffs spec
      val b0 = (1.0 - cosw) / 2.0
      val b1 = 1.0 - cosw
      val b2 = (1.0 - cosw) / 2.0
      val a0 = 1.0 + alpha
      val a1 = ~2.0 * cosw
      val a2 = 1.0 - alpha
    in
      runBiquad (b0, b1, b2, a0, a1, a2)
    end

  fun biquadHighpass spec =
    let
      val { cosw, alpha } = coeffs spec
      val b0 = (1.0 + cosw) / 2.0
      val b1 = ~(1.0 + cosw)
      val b2 = (1.0 + cosw) / 2.0
      val a0 = 1.0 + alpha
      val a1 = ~2.0 * cosw
      val a2 = 1.0 - alpha
    in
      runBiquad (b0, b1, b2, a0, a1, a2)
    end
end
