(* complex.sig

   Complex numbers over the SML `real` type. Values are immutable and the
   representation is abstract; build them with `complex` or `fromPolar` and
   take them apart with `re`/`im` or `toPolar`.

   Transcendental operations use the standard principal-branch definitions:
   `exp (a + bi) = e^a (cos b + i sin b)`, `ln z = ln |z| + i arg z` with
   `arg z` in (~pi, pi], principal `sqrt`, and `pow (z, w) = exp (w * ln z)`. *)

signature COMPLEX =
sig
  type t

  (* `complex (a, b)` is the number a + bi. *)
  val complex : real * real -> t

  (* Real and imaginary parts. *)
  val re : t -> real
  val im : t -> real

  (* Complex conjugate: conj (a + bi) = a - bi. *)
  val conj : t -> t

  (* Modulus |z| and argument arg z (radians, in (~pi, pi]). *)
  val abs : t -> real
  val arg : t -> real

  (* Field operations. `divide` divides the first argument by the second. *)
  val add    : t * t -> t
  val sub    : t * t -> t
  val mul    : t * t -> t
  val divide : t * t -> t

  (* Multiply by a real scalar: scale (k, z) = k * z. *)
  val scale : real * t -> t

  (* Principal-branch transcendentals. *)
  val exp  : t -> t
  val ln   : t -> t
  val sqrt : t -> t
  val pow  : t * t -> t

  (* Polar form. `theta` is in radians. *)
  val fromPolar : {r : real, theta : real} -> t
  val toPolar   : t -> {r : real, theta : real}

  (* Human-readable rendering, e.g. "1.0 + 2.0i" or "3.0 - 4.0i". *)
  val toString : t -> string
end
