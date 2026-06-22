(* fft.sig

   The discrete Fourier transform over `Complex.t` arrays, plus a couple of
   real-input conveniences. All functions are pure and allocate fresh output
   arrays; the input is never mutated.

   Conventions (unnormalized forward transform):

     fft  x : X[k] = sum_{j=0}^{n-1} x[j] * exp(-2*pi*i*j*k/n)
     ifft X : x[j] = (1/n) sum_{k=0}^{n-1} X[k] * exp(+2*pi*i*j*k/n)

   so `ifft (fft x) = x` up to floating-point rounding. Power-of-two lengths
   use an iterative radix-2 Cooley-Tukey transform; other lengths use
   Bluestein's chirp-z algorithm, so every length is handled in O(n log n).
   An empty array transforms to an empty array. *)

signature FFT =
sig
  (* Forward DFT. *)
  val fft : Complex.t array -> Complex.t array

  (* Inverse DFT (the 1/n-normalized conjugate transform). *)
  val ifft : Complex.t array -> Complex.t array

  (* Forward DFT of a purely real signal (each sample embedded as x + 0i). *)
  val rfft : real array -> Complex.t array

  (* Linear convolution of two real sequences via the FFT. The result has
     length `length a + length b - 1` (empty if either input is empty), and
     equals the direct sum-of-products convolution up to rounding. *)
  val convolve : real array * real array -> real array
end
