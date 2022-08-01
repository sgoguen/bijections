module BigRational

//  A conversion of:
//  https://github.com/FaustVX/BigFloat/blob/master/BigFloat.cs

open System.Numerics

// http://fsharpnews.blogspot.com/2013/08/implementing-rationals-in-f.html
type Rational(p: BigInteger, q: BigInteger) =
  let rec gcd a (b: BigInteger) =
    if b.IsZero then a else
      gcd b (a % b)

  let fixSign(p: BigInteger, q: BigInteger) =
    if q.Sign > 0 then p, q else -p, -q

  let p, q =
    if q.IsZero then raise(System.DivideByZeroException())
    let g = gcd q p
    fixSign(p/g, q/g)

  member __.Numerator = p
  member __.Denominator = q

  override __.ToString() =
    if q.IsOne then p.ToString() else sprintf "%A/%A" p q

  static member (+) (m: Rational, n: Rational) =
    Rational(m.Numerator*n.Denominator + n.Numerator*m.Denominator,
             m.Denominator*n.Denominator)

  static member (-) (m: Rational, n: Rational) =
    Rational(m.Numerator*n.Denominator - n.Numerator*m.Denominator,
             m.Denominator*n.Denominator)

  static member (*) (m: Rational, n: Rational) =
    Rational(m.Numerator*n.Numerator, m.Denominator*n.Denominator)

  static member (/) (m: Rational, n: Rational) =
    Rational(m.Numerator*n.Denominator, m.Denominator*n.Numerator)

let recip (r : Rational) =
    Rational(r.Denominator, r.Numerator)

let fromInteger n =
    Rational(n, 1I)

let properFraction (r : Rational) =
    let whole = r.Numerator / r.Denominator
    let part = Rational(r.Numerator % r.Denominator, r.Denominator)
    whole, part

let one = fromInteger 1I

let iterate f x =
    let rec loop x =
        seq {
            yield x
            yield! loop (f x)
        }
    loop x

/// https://www.cs.ox.ac.uk/jeremy.gibbons/publications/rationals.pdf
let next x =
    let n, y = properFraction x
    recip (fromInteger n + one - y)

let rationals =
    iterate next one