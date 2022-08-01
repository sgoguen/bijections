module Tests

open System
open Xunit

module Nat64 = begin
    type Nat = uint64
    let inline nat(x:^a) : Nat = uint64(x)
end

open Nat64

module Cantor = begin
    let inline unpair(z:^a) = 
        let z = nat(z)
        let w = nat(Math.Floor((Math.Sqrt(8.0 * double(z) + 1.0) - 1.0) / 2.0))
        let t = (w * w + w) / 2UL
        let y = z - t
        let x = w - y
        (x, y)
end

module Elegant = begin
    let unpair(z:Nat) = 
        let z = double(z)
        let r = nat(Math.Floor(Math.Sqrt(z)))
        let rl = nat(z) - (r * r)
        if rl < r then (rl, r)
        else (r, rl - r)

end

let (==>) x y = Assert.StrictEqual(y, x)



let inline print(x) = printfn "%A" x
let inline printList(list) = for item in list do print(item)
let inline (|Int|) (x:^a) = int(x)
let countBy f list = 
    [ for (k,v) in List.groupBy f list -> (k, List.length v) ]

let info = [ for z in 0..100 do 
               let (Int(x), Int(y)) = Elegant.unpair(nat(z))
               let m = max x y
               yield (z, m, (x, y)) ]

[<Fact>]
let ``My test`` () =

    

    // printList(info)
    printList(info |> List.groupBy (fun (_,x,_) -> x) )
    // unpair(3) ==> (0UL, 1UL)
    // 2 + 4 ==> 6
    true ==> true

