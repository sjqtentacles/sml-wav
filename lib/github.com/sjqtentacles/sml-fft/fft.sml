(* fft.sml

   The DFT over `Complex.t`, sealed behind `FFT`. Power-of-two lengths run an
   in-place iterative radix-2 Cooley-Tukey transform; arbitrary lengths use
   Bluestein's chirp-z algorithm (which reduces a length-n DFT to a single
   power-of-two convolution). The inverse transform is the conjugate trick
   `ifft X = conj (fft (conj X)) / n`, so only the forward kernel is real
   code. Everything is pure: each entry point copies its input and returns a
   fresh array. *)

structure Fft :> FFT =
struct
  structure C = Complex

  val pi = Math.pi

  fun zero () = C.complex (0.0, 0.0)
  fun one () = C.complex (1.0, 0.0)

  (* True iff n is a positive power of two (uses the n & (n-1) == 0 trick). *)
  fun isPow2 n =
    n > 0 andalso Word.andb (Word.fromInt n, Word.fromInt (n - 1)) = 0w0

  (* Smallest power of two >= k (and at least 1). *)
  fun nextPow2 k =
    let
      fun loop p = if p >= k then p else loop (p * 2)
    in
      loop 1
    end

  (* Number of bits b with 2^b = n, for a power-of-two n. *)
  fun ilog2 n =
    let
      fun loop (m, acc) = if m <= 1 then acc else loop (m div 2, acc + 1)
    in
      loop (n, 0)
    end

  (* Reverse the low `bits` bits of x. *)
  fun reverseBits (x, bits) =
    let
      fun loop (b, src, acc) =
        if b >= bits then acc
        else
          loop (b + 1, Word.>> (src, 0w1),
                Word.orb (Word.<< (acc, 0w1), Word.andb (src, 0w1)))
    in
      loop (0, x, 0w0)
    end

  (* In-place iterative radix-2 forward FFT on a power-of-two-length array. *)
  fun fftPow2InPlace (b : C.t array) =
    let
      val n = Array.length b
    in
      if n <= 1 then ()
      else
        let
          val bits = ilog2 n
          (* bit-reversal permutation *)
          val () =
            let
              fun loop i =
                if i >= n then ()
                else
                  let
                    val r = Word.toInt (reverseBits (Word.fromInt i, bits))
                  in
                    (if i < r then
                       let val t = Array.sub (b, i)
                       in Array.update (b, i, Array.sub (b, r));
                          Array.update (b, r, t)
                       end
                     else ());
                    loop (i + 1)
                  end
            in
              loop 0
            end
          (* butterfly stages: len = 2, 4, ..., n *)
          fun stages len =
            if len > n then ()
            else
              let
                val half = len div 2
                val ang = ~2.0 * pi / real len
                val wlen = C.complex (Math.cos ang, Math.sin ang)
                fun block i =
                  if i >= n then ()
                  else
                    let
                      fun inner (j, w) =
                        if j >= half then ()
                        else
                          let
                            val u = Array.sub (b, i + j)
                            val v = C.mul (w, Array.sub (b, i + j + half))
                          in
                            Array.update (b, i + j, C.add (u, v));
                            Array.update (b, i + j + half, C.sub (u, v));
                            inner (j + 1, C.mul (w, wlen))
                          end
                    in
                      inner (0, one ());
                      block (i + len)
                    end
              in
                block 0;
                stages (len * 2)
              end
          val () = stages 2
        in
          ()
        end
    end

  (* Forward FFT for a power-of-two-length array (returns a fresh array). *)
  fun fftPow2 (a : C.t array) =
    let
      val b = Array.tabulate (Array.length a, fn i => Array.sub (a, i))
    in
      fftPow2InPlace b; b
    end

  (* Inverse FFT for a power-of-two-length array via the conjugate trick. *)
  fun ifftPow2 (a : C.t array) =
    let
      val n = Array.length a
      val b = Array.tabulate (n, fn i => C.conj (Array.sub (a, i)))
      val () = fftPow2InPlace b
      val inv = 1.0 / real n
    in
      Array.tabulate (n, fn i => C.scale (inv, C.conj (Array.sub (b, i))))
    end

  (* Bluestein's algorithm for an arbitrary length n.

     Using j*k = (j^2 + k^2 - (k-j)^2)/2, the DFT becomes a convolution with a
     chirp. Let f[m] = exp(-i*pi*m^2/n). Then
        X[k] = f[k] * sum_j (x[j] f[j]) * conj(f[k-j]),
     which is a (zero-padded, power-of-two) convolution of a[j] = x[j] f[j]
     with the symmetric kernel g where g[m] = conj(f[m]) = exp(+i*pi*m^2/n).

     `m^2 mod (2n)` is used for the angle: exp(-i*pi*m^2/n) has period 2n in
     m^2, so the reduction keeps the argument small without changing the
     value. *)
  fun bluestein (x : C.t array) =
    let
      val n = Array.length x
      (* chirp f[m] = exp(-i*pi*m^2/n) *)
      fun chirp m =
        let
          val r = (m * m) mod (2 * n)
          val ang = ~pi * real r / real n
        in
          C.complex (Math.cos ang, Math.sin ang)
        end

      val m = nextPow2 (2 * n - 1)

      (* a[j] = x[j] * f[j], zero-padded to length m *)
      val a =
        Array.tabulate
          (m, fn j => if j < n then C.mul (Array.sub (x, j), chirp j) else zero ())

      (* kernel g: g[0..n-1] = conj(f), and g[m-j] = conj(f[j]) for j in 1..n-1 *)
      val g = Array.array (m, zero ())
      val () =
        let
          fun loop j =
            if j >= n then ()
            else
              let val gj = C.conj (chirp j)
              in
                Array.update (g, j, gj);
                if j > 0 then Array.update (g, m - j, gj) else ();
                loop (j + 1)
              end
        in
          loop 0
        end

      val fa = fftPow2 a
      val fg = fftPow2 g
      val prod =
        Array.tabulate (m, fn i => C.mul (Array.sub (fa, i), Array.sub (fg, i)))
      val conv = ifftPow2 prod
    in
      (* X[k] = f[k] * conv[k] *)
      Array.tabulate (n, fn k => C.mul (chirp k, Array.sub (conv, k)))
    end

  fun fft (a : C.t array) =
    let
      val n = Array.length a
    in
      if n = 0 then Array.fromList []
      else if n = 1 then Array.tabulate (1, fn _ => Array.sub (a, 0))
      else if isPow2 n then fftPow2 a
      else bluestein a
    end

  fun ifft (a : C.t array) =
    let
      val n = Array.length a
    in
      if n = 0 then Array.fromList []
      else
        let
          val conjd = Array.tabulate (n, fn i => C.conj (Array.sub (a, i)))
          val f = fft conjd
          val inv = 1.0 / real n
        in
          Array.tabulate (n, fn i => C.scale (inv, C.conj (Array.sub (f, i))))
        end
    end

  fun rfft (xs : real array) =
    fft (Array.tabulate (Array.length xs, fn i => C.complex (Array.sub (xs, i), 0.0)))

  fun convolve (a : real array, b : real array) =
    let
      val la = Array.length a
      val lb = Array.length b
    in
      if la = 0 orelse lb = 0 then Array.fromList []
      else
        let
          val outLen = la + lb - 1
          val m = nextPow2 outLen
          fun pad (src, len) =
            Array.tabulate
              (m, fn i => if i < len then C.complex (Array.sub (src, i), 0.0)
                          else zero ())
          val fa = fftPow2 (pad (a, la))
          val fb = fftPow2 (pad (b, lb))
          val prod =
            Array.tabulate (m, fn i => C.mul (Array.sub (fa, i), Array.sub (fb, i)))
          val conv = ifftPow2 prod
        in
          Array.tabulate (outLen, fn i => C.re (Array.sub (conv, i)))
        end
    end
end
