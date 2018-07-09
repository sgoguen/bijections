module NatSet

open Pairing
open System

type Cardinality = Finite of uint64 | Infinite

let getMaxSize = function 
    | Finite(n) -> n
    | Infinite -> UInt64.MaxValue

[<Struct>]
type LongBijection<'a> = 
    | LongBijection of encode: ('a -> uint64) * decode: (uint64 -> 'a) * cardinality: Cardinality

let getSetSize = function LongBijection(_, _, c) -> getMaxSize c

type LongBijection<'a> with
    member this.MaxIndex = (getSetSize this) - 1UL

let AllIntegers = LongBijection(id, id, Infinite)
let Bounded(max) = LongBijection(id, id, Finite(max))

let fromList list = 
    let length = uint64(List.length list)
    let card = Finite(length)
    let encode(x) = List.findIndex(fun item -> item = x) list |> uint64
    let decode(x) = List.item (int(x)) list
    LongBijection(encode, decode, card)

let fromInt (LongBijection(_, d, _)) x = d(x)
let toInt (LongBijection(e, _, _)) x = e(x)

open Pairing

let rec combine = function
    | LongBijection(e1, d1, Finite(c1)), LongBijection(e2, d2, Finite(c2)) -> combineFinite(e1,d1,c1)(e2,d2,c2)
    | LongBijection(e1, d1, Finite(c1)), LongBijection(e2, d2, Infinite) -> combineLeftFinite(e1,d1,c1)(e2,d2)
    | LongBijection(e1, d1, Infinite), LongBijection(e2, d2, Finite(c2)) -> combineRightFinite(e1,d1)(e2,d2,c2)
    | LongBijection(e1, d1, Infinite), LongBijection(e2, d2, Infinite) -> combineInfinite(e1,d1)(e2,d2)

and combineFinite(e1,d1,c1)(e2,d2,c2) = 
    let length = c1 * c2
    let decode(n) = 
        let x, y = BoundedPair.unpairMax c1 n
        let left = d1(x)
        let right = d2(y)
        (left, right)
    let encode(left, right) = 
        let x = e1(left)
        let y = e2(right)
        BoundedPair.pairMax c1 (x, y)
    LongBijection(encode, decode, Finite(length))
and combineLeftFinite(e1,d1,c1)(e2,d2) = 
    let decode(n) = 
        let x, y = BoundedPair.unpairMax c1 n
        let left = d1(x)
        let right = d2(y)
        (left, right)
    let encode(left, right) = 
        let x = e1(left)
        let y = e2(right)
        BoundedPair.pairMax c1 (x, y)
    LongBijection(encode, decode, Infinite)
and combineRightFinite(e1,d1)(e2,d2,c2) = 
    let decode(n) = 
        let x, y = BoundedPair.unpairMax c2 n
        let left = d1(y)
        let right = d2(x)
        (left, right)
    let encode(left, right) = 
        let x = e1(left)
        let y = e2(right)
        BoundedPair.pairMax c2 (y, x)
    LongBijection(encode, decode, Infinite)
and combineInfinite(e1,d1)(e2,d2) = 
    let decode(n) = 
        let x, y = Elegant.unpair n
        let left = d1(x)
        let right = d2(y)
        (left, right)
    let encode(left, right) = 
        let x = e1(left)
        let y = e2(right)
        Elegant.pair(x, y)
    LongBijection(encode, decode, Infinite)