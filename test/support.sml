(* support.sml -- shared helpers for the sml-wav suite.

   Audio is real-valued and several paths (sine, the FFT, the biquad) use
   transcendental `libm` code, so numeric comparisons go through an absolute
   epsilon rather than the harness's structural equality.  A handful of
   little-endian byte readers let the I/O suite assert the exact RIFF layout. *)

structure Support =
struct
  structure W = Wav
  structure H = Harness

  val pi  = Math.pi
  val eps = 1E~9

  fun approx (a, b) = Real.abs (a - b) <= eps
  fun approxT tol (a, b) = Real.abs (a - b) <= tol

  (* Delegate to the harness's epsilon comparisons. Computed values must not
     appear in pass messages: their formatting (Real.fmt/toString) is not
     byte-identical across MLton and Poly/ML. *)
  fun checkReal name (expected, actual) = H.checkReal name (expected, actual)

  fun checkRealTol tol name (expected, actual) =
    H.checkRealTol tol name (expected, actual)

  (* Elementwise real-array comparison within eps (lengths must match). *)
  fun approxArr tol (xs, ys) =
    Array.length xs = Array.length ys andalso
    let
      fun loop i =
        i >= Array.length xs orelse
        (approxT tol (Array.sub (xs, i), Array.sub (ys, i)) andalso loop (i + 1))
    in loop 0 end

  (* --- little-endian byte readers over a Word8Vector --- *)
  fun byte (v, i) = Word8.toInt (Word8Vector.sub (v, i))

  fun u16 (v, i) = byte (v, i) + byte (v, i + 1) * 256
  fun u32 (v, i) =
    byte (v, i) + byte (v, i + 1) * 256
    + byte (v, i + 2) * 65536 + byte (v, i + 3) * 16777216

  (* The 4-byte ASCII tag at offset i, as a string. *)
  fun tag (v, i) =
    String.implode (List.tabulate (4, fn k => Char.chr (byte (v, i + k))))

  (* Energy / RMS of a real array (for filter attenuation checks). *)
  fun energy (x : real array) =
    Array.foldl (fn (v, a) => a + v * v) 0.0 x

  (* Magnitude of the largest spectral bin of a real signal, via the vendored
     FFT.  Used to confirm a filter removes a tone's spectral content. *)
  fun peakMag (x : real array) =
    let val X = Fft.rfft x
    in Array.foldl (fn (z, m) => Real.max (m, Complex.abs z)) 0.0 X end

  (* Magnitude at a specific FFT bin. *)
  fun binMag (x : real array, k) = Complex.abs (Array.sub (Fft.rfft x, k))
end
