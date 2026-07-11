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

  (* Handy constants for the inverse functions below. *)
  val oneC  : t = (1.0, 0.0)
  val iC    : t = (0.0, 1.0)
  val negIC : t = (0.0, ~1.0)

  (* sin(a + bi) = sin a cosh b + i cos a sinh b. *)
  fun sin ((a, b) : t) = (Math.sin a * Math.cosh b, Math.cos a * Math.sinh b)
  (* cos(a + bi) = cos a cosh b - i sin a sinh b. *)
  fun cos ((a, b) : t) = (Math.cos a * Math.cosh b, ~ (Math.sin a) * Math.sinh b)
  fun tan z = divide (sin z, cos z)

  (* sinh(a + bi) = sinh a cos b + i cosh a sin b. *)
  fun sinh ((a, b) : t) = (Math.sinh a * Math.cos b, Math.cosh a * Math.sin b)
  (* cosh(a + bi) = cosh a cos b + i sinh a sin b. *)
  fun cosh ((a, b) : t) = (Math.cosh a * Math.cos b, Math.sinh a * Math.sin b)
  fun tanh z = divide (sinh z, cosh z)

  (* asin z = -i ln (iz + sqrt (1 - z^2)). *)
  fun asin z =
    mul (negIC, ln (add (mul (iC, z), sqrt (sub (oneC, mul (z, z))))))
  (* acos z = pi/2 - asin z (principal branch). *)
  fun acos z = sub ((Math.pi / 2.0, 0.0), asin z)
  (* atan z = (i/2)(ln (1 - iz) - ln (1 + iz)). *)
  fun atan z =
    let val iz = mul (iC, z)
    in mul ((0.0, 0.5), sub (ln (sub (oneC, iz)), ln (add (oneC, iz)))) end

  (* asinh z = ln (z + sqrt (z^2 + 1)). *)
  fun asinh z = ln (add (z, sqrt (add (mul (z, z), oneC))))
  (* acosh z = ln (z + sqrt (z^2 - 1)). *)
  fun acosh z = ln (add (z, sqrt (sub (mul (z, z), oneC))))
  (* atanh z = (1/2) ln ((1 + z)/(1 - z)). *)
  fun atanh z = scale (0.5, ln (divide (add (oneC, z), sub (oneC, z))))

  (* The n n-th roots of r e^{i t}: r^{1/n} e^{i (t + 2 pi k)/n}, k = 0..n-1. *)
  fun nthRoots (z, n) =
    if n < 1 then raise Domain
    else
      let
        val rr = Math.pow (abs z, 1.0 / Real.fromInt n)
        val t0 = arg z
        val twoPi = 2.0 * Math.pi
      in
        List.tabulate
          (n, fn k =>
             fromPolar
               {r = rr, theta = (t0 + twoPi * Real.fromInt k) / Real.fromInt n})
      end

  (* `Real.toString` differs between compilers (MLton prints 1.0 as "1",
     Poly/ML as "1.0", and they disagree on digit count and exponent style).
     Emit a forced-decimal representation instead: always a decimal point,
     a leading "-" (not "~"), and a fixed 6 decimal places, so the output is
     byte-identical under both MLton and Poly/ML. *)
  fun fmt r =
    let
      val s = if Real.signBit r then "-" else ""
      val a = Real.abs r
      val scaled = Real.realRound (a * 1000000.0)
      val whole = Real.floor (scaled / 1000000.0)
      val frac  = Real.floor scaled - whole * 1000000
    in
      s ^ Int.toString whole ^ "." ^ StringCvt.padLeft #"0" 6 (Int.toString frac)
    end

  fun toString ((a, b) : t) =
    if b < 0.0
    then fmt a ^ " - " ^ fmt (Real.abs b) ^ "i"
    else fmt a ^ " + " ^ fmt b ^ "i"
end
