(* test_filter.sml -- biquad filters verified spectrally via the vendored FFT.

   A low tone (250 Hz) and a high tone (3000 Hz) are run through a 600 Hz
   lowpass.  Using `Fft.rfft` we confirm the high tone's spectral peak (and
   energy) is strongly attenuated while the low tone passes near unity; the
   highpass is checked to do the opposite.  This is the DSP+FFT integration
   that motivates vendoring sml-fft. *)

structure FilterTests =
struct
  open Support
  structure H = Harness

  val rate = 8000
  val n    = 1024                        (* power of two: clean radix-2 FFT *)

  fun tone f = W.sine { freq = f, dur = real n / real rate, rate = rate }

  val low  = tone 250.0
  val high = tone 3000.0

  fun ratioE (aft, bef) = energy aft / energy bef

  fun run () =
    let
      val () = H.section "lowpass attenuates highs, passes lows"
      val lp = W.biquadLowpass { cutoff = 600.0, q = 0.70710678, rate = rate }
      val lpHigh = lp high
      val lpLow  = lp low

      (* spectral peak magnitude before/after *)
      val () = H.check "lowpass: high-tone spectral peak attenuated > 5x"
                 (peakMag lpHigh < 0.2 * peakMag high)
      val () = H.check "lowpass: low-tone spectral peak preserved (>0.7x)"
                 (peakMag lpLow > 0.7 * peakMag low)

      (* energy ratios *)
      val () = H.check "lowpass: high-tone energy ratio small (<0.1)"
                 (ratioE (lpHigh, high) < 0.1)
      val () = H.check "lowpass: low-tone energy ratio near 1 (>0.6)"
                 (ratioE (lpLow, low) > 0.6)

      val () = H.section "highpass does the opposite"
      val hp = W.biquadHighpass { cutoff = 600.0, q = 0.70710678, rate = rate }
      val hpHigh = hp high
      val hpLow  = hp low
      val () = H.check "highpass: low-tone spectral peak attenuated > 5x"
                 (peakMag hpLow < 0.2 * peakMag low)
      val () = H.check "highpass: high-tone spectral peak preserved (>0.7x)"
                 (peakMag hpHigh > 0.7 * peakMag high)

      val () = H.section "filters preserve length and are pure"
      val () = H.checkInt "lowpass output length = input"
                 (n, Array.length lpHigh)
      val () = H.check "lowpass is repeatable (pure)"
                 (approxArr 1E~12 (lp high, lpHigh))
    in () end
end
