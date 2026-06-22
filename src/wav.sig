(* wav.sig

   Pure-Standard-ML audio: WAV (RIFF/PCM) container I/O plus a small synthesis
   and DSP toolkit.  Audio lives in two representations:

     - on disk / on the wire as a `Word8Vector.vector` of little-endian PCM
       (the canonical Microsoft WAVE layout: a `RIFF` chunk containing a
       `fmt ` chunk and a `data` chunk);
     - in memory as a `pcm` record whose `samples` are normalized `real`s in
       [~1.0, 1.0], interleaved by channel (frame 0 ch 0, frame 0 ch 1, ...).

   Everything is pure and deterministic: no FFI, no clock, no randomness.  The
   oscillators, envelopes and filters use only IEEE arithmetic and `libm`
   trig, so identical inputs yield identical outputs under both MLton and
   Poly/ML.

   Sample/byte conversion is fixed-point and symmetric: a real `s` is quantized
   to a 16-bit code with `floor (clamp (s, ~1, 1) * 32767 + 0.5)` (24-bit uses
   8388607), and a code `k` decodes back to `k / 32767.0`.  Rounding uses
   `floor (x + 0.5)` rather than `Real.round` (whose tie-to-even has differed
   between compilers), so encoded bytes are byte-identical across compilers. *)

signature WAV =
sig
  (* Raised on malformed WAV input or invalid PCM (e.g. a sample count that is
     not a whole number of frames, an unsupported bit depth). *)
  exception Wav of string

  (* In-memory audio.  `rate` is the sample rate in Hz, `channels` >= 1, and
     `samples` is interleaved by channel with length a multiple of `channels`.
     Sample magnitudes are expected in [~1.0, 1.0]; `encode` clamps. *)
  type pcm = { rate : int, channels : int, samples : real array }

  (* --- WAV container I/O --- *)

  (* Decode a little-endian PCM WAVE blob (8/16/24/32-bit integer samples) into
     normalized reals.  Skips unknown chunks; raises `Wav` on a bad header. *)
  val decode : Word8Vector.vector -> pcm

  (* Encode as 16-bit PCM WAVE (the default).  Equivalent to `encodeBits 16`. *)
  val encode : pcm -> Word8Vector.vector

  (* Encode as 16- or 24-bit PCM WAVE; any other depth raises `Wav`. *)
  val encodeBits : int -> pcm -> Word8Vector.vector

  (* --- oscillators ---

     Each builds a single-channel `real array` of `floor (dur * rate)` samples.
     Sample j has phase `freq * j / rate` (cycles); `freq`/`dur` in seconds,
     `rate` in Hz.  `saw` and `square` are exact (no transcendentals); `sine`
     uses `Math.sin`. *)

  (* sin (2*pi*phase): a band-unlimited sine in [~1, 1], sample 0 = 0.0. *)
  val sine : { freq : real, dur : real, rate : int } -> real array

  (* Rising sawtooth `2*frac(phase) - 1` in [~1, 1), sample 0 = ~1.0. *)
  val saw : { freq : real, dur : real, rate : int } -> real array

  (* Square wave `+1` on the first half of each cycle, `~1` on the second. *)
  val square : { freq : real, dur : real, rate : int } -> real array

  (* --- amplitude helpers --- *)

  (* Root-mean-square level; 0.0 for an empty array. *)
  val rms : real array -> real

  (* Peak absolute amplitude; 0.0 for an empty array. *)
  val peak : real array -> real

  (* Scale every sample by a constant gain (linear, not dB). *)
  val gain : real -> real array -> real array

  (* Sample-wise sum of equal-or-unequal-length arrays (result length = the
     longest input; shorter inputs are zero-padded).  `mix []` is empty. *)
  val mix : real array list -> real array

  (* Normalize so the peak is exactly `target` (no-op on silence). *)
  val normalize : real -> real array -> real array

  (* --- ADSR envelope --- *)

  structure Adsr :
  sig
    (* `attack`/`decay`/`release` are durations in seconds; `sustain` is the
       held level in [0, 1]. *)
    type t = { attack : real, decay : real, sustain : real, release : real }

    (* The gain curve over `floor (dur * rate)` samples: a linear attack
       0 -> 1, linear decay 1 -> sustain, a sustain hold, then a linear
       release sustain -> 0 occupying the final `floor (release * rate)`
       samples.  Regions clamp gracefully when `dur` is short. *)
    val envelope : t -> { rate : int, dur : real } -> real array

    (* Multiply a signal by the envelope generated for its own length at
       `rate` (dur = length / rate); lengths are matched by truncation. *)
    val apply : t -> int -> real array -> real array
  end

  (* --- biquad filters (RBJ audio-EQ cookbook) ---

     `biquadLowpass {cutoff, q, rate}` returns a filter: a function from an
     input signal to a same-length, zero-state Direct-Form-I filtered signal.
     `cutoff` is the ~3 dB corner in Hz, `q` the resonance (0.707 = Butterworth). *)
  val biquadLowpass  : { cutoff : real, q : real, rate : int } -> real array -> real array
  val biquadHighpass : { cutoff : real, q : real, rate : int } -> real array -> real array
end
