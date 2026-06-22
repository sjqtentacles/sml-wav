(* sml-wav demo (`make example`)

   Synthesizes a one-second A-major chord from three sawtooth oscillators,
   shapes it with an ADSR envelope and a gentle lowpass, normalizes it, and
   writes it as 16-bit PCM to assets/tone.wav.  It then renders two PNGs via the
   vendored sml-plot + sml-fft:

     - assets/waveform.png : the first ~25 ms of the signal (amplitude vs time);
     - assets/spectrum.png : the magnitude spectrum (via Fft.rfft) up to 4 kHz,
                             where the sawtooth harmonic series is visible.

   The whole pipeline is deterministic: the oscillators and envelope are exact
   arithmetic, and the only transcendental code (the biquad coefficients and the
   FFT twiddles) defers to libm, so the .wav and both PNGs are byte-identical
   across MLton and Poly/ML. *)

structure W = Wav

val rate = 16000
val dur  = 1.0

(* --- A-major triad: A3, C#4, E4 (just-intonation-ish equal temperament) --- *)
fun note f = W.saw { freq = f, dur = dur, rate = rate }
val chord = W.mix [ note 220.0, note 277.18, note 329.63 ]

(* shape: ADSR envelope, gentle lowpass to tame the highest harmonics *)
val adsr : W.Adsr.t =
  { attack = 0.02, decay = 0.10, sustain = 0.6, release = 0.30 }
val enveloped = W.Adsr.apply adsr rate chord
val filtered  = W.biquadLowpass { cutoff = 3000.0, q = 0.7071, rate = rate } enveloped
val signal    = W.normalize 0.9 filtered

val pcm : W.pcm = { rate = rate, channels = 1, samples = signal }

(* --- write the .wav --- *)
val () =
  let val os = BinIO.openOut "assets/tone.wav"
  in
    BinIO.output (os, W.encode pcm);
    BinIO.closeOut os;
    print ("wrote assets/tone.wav (" ^ Int.toString (Array.length signal)
           ^ " samples @ " ^ Int.toString rate ^ " Hz)\n")
  end

(* --- waveform PNG: the first ~25 ms (400 samples) --- *)
val waveN = 400
val wavePts =
  List.tabulate (waveN, fn i =>
    (1000.0 * real i / real rate, Array.sub (signal, i)))   (* ms, amplitude *)

val waveChart : Plot.chart =
  { width = 760, height = 320
  , series = [ Plot.Line wavePts ]
  , title  = "sml-wav: A-major sawtooth chord (waveform, first 25 ms)"
  , axes   = { xlabel = "time (ms)", ylabel = "amplitude", grid = true }
  , legend = false }

val () =
  let val os = BinIO.openOut "assets/waveform.png"
  in
    BinIO.output (os, Image.encodePng (Plot.render waveChart));
    BinIO.closeOut os;
    print "wrote assets/waveform.png\n"
  end

(* --- spectrum PNG: magnitude of Fft.rfft over a 4096-sample window --- *)
val fftN = 4096
val window = Array.tabulate (fftN, fn i => Array.sub (signal, i))
val spectrum = Fft.rfft window
val binHz = real rate / real fftN
val maxBin = 1024                              (* up to ~4 kHz *)
val specPts =
  List.tabulate (maxBin, fn k =>
    (real k * binHz, Complex.abs (Array.sub (spectrum, k))))

val specChart : Plot.chart =
  { width = 760, height = 320
  , series = [ Plot.Line specPts ]
  , title  = "sml-wav: magnitude spectrum via sml-fft (0-4 kHz)"
  , axes   = { xlabel = "frequency (Hz)", ylabel = "magnitude", grid = true }
  , legend = false }

val () =
  let val os = BinIO.openOut "assets/spectrum.png"
  in
    BinIO.output (os, Image.encodePng (Plot.render specChart));
    BinIO.closeOut os;
    print "wrote assets/spectrum.png\n"
  end

val () =
  print ("peak = " ^ Real.fmt (StringCvt.FIX (SOME 4)) (W.peak signal)
         ^ ", rms = " ^ Real.fmt (StringCvt.FIX (SOME 4)) (W.rms signal) ^ "\n")
