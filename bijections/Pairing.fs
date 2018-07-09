namespace Pairing

module NTuples = begin

    // uint64 list -> uint64
    let rec combine (pair: (uint64 * uint64) -> uint64) = function
        | [] -> 0UL
        | x::[] -> x
        | x::[y] -> pair(y, x)
        | x::rest -> pair(combine pair rest, x)

    // int -> uint64 -> uint64 list
    let rec split unpair size n =
        match size with
        | 0 -> []
        | 1 -> [n]
        | 2 -> let (x, y) = unpair(n)
               [y; x]
        | s -> let (x, y) = unpair(n)
               y::(split unpair (s - 1) x)

end

/// A collection of functions use to map the infinite set of natural numbers (Nat)
/// to the infinite set of natural pairs (Nat * Nat)
module Cantor = begin

    open System

    // (uint64 * uint64) -> uint64
    let pair(k1:uint64, k2:uint64) : uint64 = 
        ((k1 + k2) * (k1 + k2 + 1UL)) / 2UL + k2

    // uint64 -> (uint64 * uint64)
    let unpair(z:uint64) = 
        let w = uint64(Math.Floor((Math.Sqrt(8.0 * double(z) + 1.0) - 1.0) / 2.0))
        let t = (w * w + w) / 2UL
        let y = z - t
        let x = w - y
        (x, y)

    let toList size = NTuples.split unpair size
    let fromList list = NTuples.combine pair list

end

///  Elegant Pairing courtesey of Matthew Szudzik - http://szudzik.com/ElegantPairing.pdf

module Elegant = begin

    open System

    let pair(x:uint64, y:uint64) = 
        if max x y <> x then y * y + x
        else (x * x) + x + y

    let unpair(z:uint64) = 
        let z = double(z)
        let r = uint64(Math.Floor(Math.Sqrt(z)))
        let rl = uint64(z) - (r * r)
        if rl < r then (rl, r)
        else (r, rl - r)

    let toList size = NTuples.split unpair size
    let fromList list = NTuples.combine pair list

    module Tests = begin
        let source = [0UL..1000UL]
        let e1 = source |> List.map unpair
        let e2 = e1 |> List.map pair
        let works = (e2 = source)    
    end

end

module BoundedPair = begin

    let pairMax (maxX:uint64) (x:uint64, y:uint64) = (y * maxX) + x
    let unpairMax (maxX:uint64) (n:uint64) = 
        let x = n % maxX
        let y = n / maxX
        (x, y)

    module Tests = begin
        let source = [0UL..1000UL]
        let e1 = source |> List.map (unpairMax 5UL)
        let e2 = e1 |> List.map (pairMax 5UL)
        let works = (e2 = source)    
    end
    

end

