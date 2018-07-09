#quit

#load "Pairing.fs"
#load "NatSet.fs"

module Tests = 
    let checkBijection s1 s2 = 
        let s3 = NatSet.combine(s1, s2)

        let maxS = if s3.MaxIndex > 1000UL then 1000UL else s3.MaxIndex

        let source = [0UL..maxS]
        let e1 = source |> List.map (NatSet.fromInt s3)
        let e2 = e1 |> List.map (NatSet.toInt s3)
        let allMatch = (source = e2)
        allMatch


    let boundedInts = NatSet.Bounded(10UL)
    let list = NatSet.fromList ["Zero"; "One"; "Two"]
    let allInts = NatSet.AllIntegers

    let bounded1 = checkBijection boundedInts list
    let bounded2 = checkBijection list boundedInts 
    let leftBounded = checkBijection list allInts
    let rightBounded = checkBijection allInts list
    let unbounded = checkBijection allInts allInts
