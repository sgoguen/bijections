namespace Pairing

module Nat64 = begin
    type Nat = uint64
    let inline nat(x:^a) : Nat = uint64(x)
end

open Nat64

module NTuples = begin

    // nat list -> nat
    let rec combine (pair: (Nat * Nat) -> Nat) = function  
        | [] -> nat(0)
        | x::[] -> x
        | x::[y] -> pair(y, x)
        | x::rest -> pair(combine pair rest, x)

    // int -> Nat -> Nat list
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

    // (Nat * Nat) -> Nat
    let pair(k1:Nat, k2:Nat) : Nat = 
        ((k1 + k2) * (k1 + k2 + 1UL)) / 2UL + k2

    // Nat -> (Nat * Nat)
    let unpair(z:Nat) = 
        let w = nat(Math.Floor((Math.Sqrt(8.0 * double(z) + 1.0) - 1.0) / 2.0))
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

    let pair(x:Nat, y:Nat) = 
        if max x y <> x then y * y + x
        else (x * x) + x + y

    let unpair(z:Nat) = 
        let z = double(z)
        let r = nat(Math.Floor(Math.Sqrt(z)))
        let rl = nat(z) - (r * r)
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

module RosenbergStrong = begin
    open System
    open Nat64

    let pair(x:Nat, y:Nat) = 
        let mxy = max x y
        mxy * mxy + mxy + x - y

    let unpair(z:Nat) =
        let m = nat(Math.Floor(Math.Sqrt(double(z))))
        let ml = z - (m * m)
        if ml < m then (ml, m)
        else (m, (m * m) + m + m - z)
end

module RosenbergStrongTests = begin
    open RosenbergStrong
    let source = [0UL..1000UL]
    let e1 = source |> List.map unpair
    let e2 = e1 |> List.map pair
    let works = (e2 = source)    
end

module BoundedPair = begin

    let pairMax (maxX:Nat) (x:Nat, y:Nat) = (y * maxX) + x
    let unpairMax (maxX:Nat) (n:Nat) = 
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

module SzudzikPTupling = begin

    open System

    let unpair a b z =
        let a = double(a)
        let b = double(b)
        let z = double(z)
        let m = Math.Floor(Math.Pow(double(z), 1.0 / (a + b)))
        let m1b = Math.Pow(m + 1.0, b)
        let ma = Math.Pow(m, a)
        if z < ma * m1b then 
            (z % a, Math.Floor(z / ma))
        else 
            (Math.Floor(z / m1b), z % m1b)

    let (|Int|) (f:double) = int(f)

    let rec ntuple n (z:int) =
        [
            if n = 1 then 
                yield z
            if n = 2 then 
                let (Int(x1), Int(x2)) = unpair 1 1 z
                yield x1
                yield x2
            elif n > 2 then 
                let (Int(x1), Int(x2)) = unpair (n - 1) 1 z
                yield! tuple (n - 1) (int x1)
                yield x2
        ]

    // let maxL xs = List.fold max 0 xs 
    // let invert = List.map (fun x -> double(x))

    // let calcLength tsize m =
    //     seq { 0..1000000 } |> Seq.map (ntuple tsize) |> Seq.takeWhile (fun l -> maxL l <= m)
    //     |> Seq.filter (fun l -> maxL l = m)
    //     |> Seq.length

    // [0..7] |> List.map (calcLength 6)

    // |> Seq.iter(printfn "%A")
    

    

end


module Patterns = begin

    let (|CantorPair|) = Cantor.unpair

    let (|CantorList|) length = Cantor.toList length
    
    let (|ElegantPair|) = Elegant.unpair

    let (|ElegantList|) length = Cantor.toList length

    let (|BoundedPair|) = BoundedPair.unpairMax

end