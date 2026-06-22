(* test_dsp.sml -- gain / mix / normalize / rms / peak.

   Small exact-arithmetic checks of the amplitude helpers, plus a convolution
   sanity check against the vendored FFT (moving-sum kernel) to confirm the
   wav layer and sml-fft compose. *)

structure DspTests =
struct
  open Support
  structure H = Harness

  val arr = Array.fromList

  fun run () =
    let
      val () = H.section "gain"
      val g = W.gain 0.5 (arr [1.0, ~2.0, 4.0])
      val () = H.check "gain 0.5 scales each sample"
                 (approxArr eps (arr [0.5, ~1.0, 2.0], g))

      val () = H.section "mix (zero-padded sum)"
      val m = W.mix [ arr [1.0, 2.0, 3.0], arr [0.5, 0.5], arr [10.0] ]
      val () = H.checkInt "mix length = longest input" (3, Array.length m)
      val () = H.check "mix sums with padding"
                 (approxArr eps (arr [11.5, 2.5, 3.0], m))
      val () = H.checkInt "mix [] is empty" (0, Array.length (W.mix []))

      val () = H.section "normalize"
      val n = W.normalize 1.0 (arr [0.1, ~0.2, 0.4])
      val () = checkReal "normalized peak = target" (1.0, W.peak n)
      val () = H.check "normalize preserves shape (scaled by 2.5)"
                 (approxArr eps (arr [0.25, ~0.5, 1.0], n))
      val () = H.checkInt "normalize of silence is unchanged length"
                 (3, Array.length (W.normalize 1.0 (arr [0.0, 0.0, 0.0])))

      val () = H.section "rms / peak basics"
      val () = checkReal "rms of empty = 0" (0.0, W.rms (arr []))
      val () = checkReal "peak of empty = 0" (0.0, W.peak (arr []))
      val () = checkReal "rms of constant 0.5" (0.5, W.rms (arr [0.5, ~0.5, 0.5, ~0.5]))
      val () = checkReal "peak picks max abs" (4.0, W.peak (arr [1.0, ~4.0, 2.0]))

      (* ---- FFT convolution sanity (moving-sum of width 3) ---- *)
      val () = H.section "fft convolution composes with wav layer"
      val conv = Fft.convolve (arr [1.0, 2.0, 3.0, 4.0, 5.0], arr [1.0, 1.0, 1.0])
      val () = H.check "convolve = [1,3,6,9,12,9,5]"
                 (approxArr 1E~9
                    (arr [1.0, 3.0, 6.0, 9.0, 12.0, 9.0, 5.0], conv))
    in () end
end
