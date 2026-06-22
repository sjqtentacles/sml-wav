(* test_adsr.sml -- ADSR envelope shape.

   Uses a clean 1 kHz rate with 100-sample (0.1 s) attack/decay/release and a
   0.5 level so every region boundary lands on an exact sample, then asserts
   the piecewise-linear gain at known indices and the global shape (starts at
   0, peaks at 1, ends near 0). *)

structure AdsrTests =
struct
  open Support
  structure H = Harness
  structure A = Wav.Adsr

  val env : A.t = { attack = 0.1, decay = 0.1, sustain = 0.5, release = 0.1 }

  fun at (a, i) = Array.sub (a, i)

  fun run () =
    let
      val () = H.section "envelope piecewise-linear shape"
      val e = A.envelope env { rate = 1000, dur = 0.5 }   (* 500 samples *)
      val () = H.checkInt "envelope length = 500" (500, Array.length e)
      (* attack 0 -> 1 over [0,100) *)
      val () = checkReal "attack start = 0"     (0.0,  at (e, 0))
      val () = checkReal "attack midpoint = .5" (0.5,  at (e, 50))
      val () = checkReal "attack/decay seam = 1"(1.0,  at (e, 100))
      (* decay 1 -> 0.5 over [100,200) *)
      val () = checkReal "decay midpoint = .75" (0.75, at (e, 150))
      (* sustain hold at 0.5 over [200,400) *)
      val () = checkReal "sustain at 250" (0.5, at (e, 250))
      val () = checkReal "sustain at 399" (0.5, at (e, 399))
      (* release 0.5 -> 0 over [400,500) *)
      val () = checkReal "release start = .5"   (0.5,  at (e, 400))
      val () = checkReal "release midpoint = .25"(0.25, at (e, 450))

      val () = H.section "global shape"
      val () = H.check "peak gain = 1.0" (approx (1.0, W.peak e))
      val () = H.check "attack is non-decreasing"
                 (let fun loop i = i >= 100 orelse
                        (at (e, i) <= at (e, i + 1) + eps andalso loop (i + 1))
                  in loop 0 end)
      val () = H.check "release tail is small" (at (e, 499) < 0.01)

      val () = H.section "apply multiplies signal by envelope"
      val sig0 = Array.tabulate (500, fn _ => 1.0)
      val applied = A.apply env 1000 sig0
      val () = H.checkInt "apply preserves length" (500, Array.length applied)
      val () = H.check "apply equals envelope on a DC signal"
                 (approxArr 1E~9 (e, applied))
    in () end
end
