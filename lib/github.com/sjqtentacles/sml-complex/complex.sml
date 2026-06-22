(* complex.sml

   Complex numbers over `real`, represented as a (re, im) pair. The
   representation is sealed behind `COMPLEX` so callers go through the
   constructors and accessors. Transcendental functions use the standard
   principal-branch formulas (see complex.sig). *)

structure Complex :> COMPLEX =
struct
  type t = real * real

  fun complex (a, b) = (a, b)

  fun re ((a, _) : t) = a
  fun im ((_, b) : t) = b

  fun conj ((a, b) : t) = (a, ~b)

  fun abs ((a, b) : t) = Math.sqrt (a * a + b * b)
  fun arg ((a, b) : t) = Math.atan2 (b, a)

  fun add ((a, b) : t, (c, d) : t) = (a + c, b + d)
  fun sub ((a, b) : t, (c, d) : t) = (a - c, b - d)
  fun mul ((a, b) : t, (c, d) : t) = (a * c - b * d, a * d + b * c)

  fun divide ((a, b) : t, (c, d) : t) =
    let val den = c * c + d * d
    in ((a * c + b * d) / den, (b * c - a * d) / den) end

  fun scale (k : real, (a, b) : t) = (k * a, k * b)

  (* exp(a + bi) = e^a (cos b + i sin b) *)
  fun exp ((a, b) : t) =
    let val ea = Math.exp a
    in (ea * Math.cos b, ea * Math.sin b) end

  (* ln z = ln|z| + i arg z, principal branch (arg in (~pi, pi]). *)
  fun ln z = (Math.ln (abs z), arg z)

  (* Principal square root: sqrt(r e^{i t}) = sqrt r e^{i t/2}. *)
  fun sqrt z =
    let
      val r = Math.sqrt (abs z)
      val t = arg z / 2.0
    in
      (r * Math.cos t, r * Math.sin t)
    end

  (* z^w = exp(w * ln z), principal branch. *)
  fun pow (z, w) = exp (mul (w, ln z))

  fun fromPolar {r, theta} = (r * Math.cos theta, r * Math.sin theta)
  fun toPolar z = {r = abs z, theta = arg z}

  (* `Real.toString` differs between compilers (MLton prints 1.0 as "1",
     Poly/ML as "1.0"); normalize so output is identical everywhere by
     ensuring a decimal point on plain integer-valued reals. *)
  fun fmt x =
    let
      val s = Real.toString x
      val plain =
        CharVector.all (fn c => c <> #"." andalso c <> #"E" andalso c <> #"e") s
    in
      if plain then s ^ ".0" else s
    end

  fun toString ((a, b) : t) =
    if b < 0.0
    then fmt a ^ " - " ^ fmt (Real.abs b) ^ "i"
    else fmt a ^ " + " ^ fmt b ^ "i"
end
