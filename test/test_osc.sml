(* test_osc.sml -- oscillators.

   Sample-exact assertions at known phases for sine/saw/square, plus length and
   amplitude-bound checks.  `saw`/`square` are exact arithmetic so they are
   checked to 1e-9; `sine` goes through `Math.sin` and is checked to a slightly
   looser tolerance. *)

structure OscTests =
struct
  open Support
  structure H = Harness

  fun at (a, i) = Array.sub (a, i)

  fun run () =
    let
      (* ---- lengths ---- *)
      val () = H.section "oscillator length = floor (dur * rate)"
      val s = W.sine { freq = 1.0, dur = 1.0, rate = 100 }
      val () = H.checkInt "sine length" (100, Array.length s)
      val () = H.checkInt "saw length"
                 (50, Array.length (W.saw { freq = 2.0, dur = 0.5, rate = 100 }))

      (* ---- sine sample-exact at quarter phases (freq=1, rate=4) ----
         phase j/4 -> sin(pi*j/2): [0, 1, 0, ~1] *)
      val () = H.section "sine known phases"
      val sq = W.sine { freq = 1.0, dur = 1.0, rate = 4 }
      val () = checkRealTol 1E~9 "sin@0  = 0"  (0.0,  at (sq, 0))
      val () = checkRealTol 1E~9 "sin@1  = 1"  (1.0,  at (sq, 1))
      val () = checkRealTol 1E~9 "sin@2  = 0"  (0.0,  at (sq, 2))
      val () = checkRealTol 1E~9 "sin@3  = -1" (~1.0, at (sq, 3))

      (* ---- saw exact: 2*frac(phase) - 1, freq=1 rate=8 ----
         phases 0,1/8,...; values -1, -0.75, -0.5, ... *)
      val () = H.section "saw known values (exact)"
      val sw = W.saw { freq = 1.0, dur = 1.0, rate = 8 }
      val () = checkReal "saw@0 = -1"    (~1.0,  at (sw, 0))
      val () = checkReal "saw@1 = -0.75" (~0.75, at (sw, 1))
      val () = checkReal "saw@2 = -0.5"  (~0.5,  at (sw, 2))
      val () = checkReal "saw@4 = 0"     (0.0,   at (sw, 4))
      val () = checkReal "saw@6 = 0.5"   (0.5,   at (sw, 6))

      (* ---- square exact: +1 first half cycle, -1 second ---- *)
      val () = H.section "square known values (exact)"
      val sqr = W.square { freq = 1.0, dur = 1.0, rate = 8 }
      val () = checkReal "sqr@0 = +1" (1.0,  at (sqr, 0))
      val () = checkReal "sqr@3 = +1" (1.0,  at (sqr, 3))
      val () = checkReal "sqr@4 = -1" (~1.0, at (sqr, 4))
      val () = checkReal "sqr@7 = -1" (~1.0, at (sqr, 7))

      (* ---- amplitude bounds ---- *)
      val () = H.section "amplitude bounds"
      (* sine over an integer number of periods: RMS = 1/sqrt 2 *)
      val tone = W.sine { freq = 5.0, dur = 1.0, rate = 1000 }   (* 5 whole periods *)
      val () = checkRealTol 1E~3 "sine RMS = 0.7071" (0.70710678, W.rms tone)
      val () = H.check "sine peak <= 1" (W.peak tone <= 1.0 + eps)
      (* saw freq=1 rate=8 hits -1.0 at sample 0, so peak abs = 1.0 *)
      val () = checkReal "saw peak = 1 (ramp reaches -1)"
                 (1.0, W.peak (W.saw { freq = 1.0, dur = 1.0, rate = 8 }))
      val () = checkReal "square peak = 1" (1.0, W.peak sqr)
      val () = checkReal "square RMS = 1"  (1.0, W.rms sqr)
    in () end
end
