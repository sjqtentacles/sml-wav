(* test_io.sml -- WAV container I/O.

   Asserts the exact RIFF/fmt /data byte layout of an encoded blob, then checks
   that decode is a left inverse of encode on PCM whose samples sit exactly on
   the fixed-point grid (k/32767 for 16-bit, k/8388607 for 24-bit), so the
   round-trip is exact rather than merely close. *)

structure IoTests =
struct
  open Support
  structure H = Harness

  (* A PCM whose samples are exact 16-bit codes / 32767, so encode->decode is
     lossless.  Two channels, 4 frames. *)
  fun gridPcm () =
    let
      val codes = Array.fromList [0, 32767, ~32767, 16384, ~8192, 12345, ~30000, 1]
      val samples = Array.tabulate (8, fn i => real (Array.sub (codes, i)) / 32767.0)
    in { rate = 44100, channels = 2, samples = samples } : W.pcm end

  fun run () =
    let
      val pcm = gridPcm ()
      val bytes = W.encode pcm                 (* 16-bit *)
      val nData = 8 * 2                         (* 8 samples * 2 bytes *)

      (* ---- header byte assertions ---- *)
      val () = H.section "RIFF/WAVE/fmt /data byte layout"
      val () = H.checkString "RIFF tag" ("RIFF", tag (bytes, 0))
      val () = H.checkInt "RIFF chunk size = 36 + dataSize"
                 (36 + nData, u32 (bytes, 4))
      val () = H.checkString "WAVE tag" ("WAVE", tag (bytes, 8))
      val () = H.checkString "fmt  tag" ("fmt ", tag (bytes, 12))
      val () = H.checkInt "fmt chunk size = 16" (16, u32 (bytes, 16))
      val () = H.checkInt "audioFormat = 1 (PCM)" (1, u16 (bytes, 20))
      val () = H.checkInt "numChannels = 2" (2, u16 (bytes, 22))
      val () = H.checkInt "sampleRate = 44100" (44100, u32 (bytes, 24))
      val () = H.checkInt "byteRate = rate*ch*bytes" (44100 * 2 * 2, u32 (bytes, 28))
      val () = H.checkInt "blockAlign = ch*bytes" (2 * 2, u16 (bytes, 32))
      val () = H.checkInt "bitsPerSample = 16" (16, u16 (bytes, 34))
      val () = H.checkString "data tag" ("data", tag (bytes, 36))
      val () = H.checkInt "data chunk size" (nData, u32 (bytes, 40))
      val () = H.checkInt "total blob length = 44 + dataSize"
                 (44 + nData, Word8Vector.length bytes)

      (* ---- exact round-trip: decode o encode = id on grid PCM ---- *)
      val () = H.section "16-bit round-trip (decode o encode = id)"
      val dec = W.decode bytes
      val () = H.checkInt "rate preserved" (44100, #rate dec)
      val () = H.checkInt "channels preserved" (2, #channels dec)
      val () = H.checkInt "sample count preserved"
                 (8, Array.length (#samples dec))
      val () = H.check "samples bit-exact through 16-bit grid"
                 (approxArr 1E~12 (#samples pcm, #samples dec))

      (* ---- 24-bit round-trip ---- *)
      val () = H.section "24-bit round-trip"
      val codes24 = Array.fromList [0, 8388607, ~8388607, 4194304, ~1000000]
      val s24 = Array.tabulate (5, fn i => real (Array.sub (codes24, i)) / 8388607.0)
      val pcm24 = { rate = 22050, channels = 1, samples = s24 } : W.pcm
      val b24 = W.encodeBits 24 pcm24
      val () = H.checkInt "24-bit bitsPerSample" (24, u16 (b24, 34))
      val () = H.checkInt "24-bit blockAlign = 3" (3, u16 (b24, 32))
      val () = H.checkInt "24-bit data size = 5*3" (5 * 3, u32 (b24, 40))
      val dec24 = W.decode b24
      val () = H.checkInt "24-bit channels" (1, #channels dec24)
      val () = H.check "24-bit samples bit-exact"
                 (approxArr 1E~12 (s24, #samples dec24))

      (* ---- mono round-trip + clamping ---- *)
      val () = H.section "mono + clamping"
      val mono = { rate = 8000, channels = 1
                 , samples = Array.fromList [0.0, 0.5, ~0.5, 0.25] } : W.pcm
      val decM = W.decode (W.encode mono)
      val () = H.checkInt "mono channels" (1, #channels decM)
      val () = H.checkInt "mono sample count" (4, Array.length (#samples decM))
      (* an out-of-range sample clamps to full scale, then decodes to ~1.0 *)
      val hot = { rate = 8000, channels = 1, samples = Array.fromList [2.0, ~2.0] } : W.pcm
      val decH = W.decode (W.encode hot)
      val () = checkRealTol 1E~3 "over-range clamps to +1"
                 (1.0, Array.sub (#samples decH, 0))
      val () = checkRealTol 1E~3 "under-range clamps to -1"
                 (~1.0, Array.sub (#samples decH, 1))

      (* ---- error handling ---- *)
      val () = H.section "malformed input / bad depth"
      val () = H.checkRaises "decode of junk raises Wav"
                 (fn () => W.decode (Word8Vector.tabulate (10, fn _ => 0w0)))
      val () = H.checkRaises "encodeBits 8 raises Wav"
                 (fn () => W.encodeBits 8 mono)
    in () end
end
