//  This is an F# translation of Peter Norvig's lisp.py
//  See http://norvig.com/lispy.html


type Expr = 
    | Symbol of string
    | Int of int
    | Float of double
    | EList of Expr list

type Env = Map<string, Expr>

open System

let (|Int|_|) (x:string) = match Int32.TryParse(x) with | (true, v) -> Some(v) | _ -> None
let (|Float|_|) (x:string) = match Double.TryParse(x) with | (true, v) -> Some(v) | _ -> None
let (|Symbol|_|) = function | "(" -> None | ")" -> None | x -> Some(x)
let (|Atom|Begin|End|) = function
    | "(" -> Begin
    | ")" -> End
    | Int(x) -> Atom(Int(x))
    | Float(x) -> Atom(Float(x))
    | x -> Atom(Symbol(x))


let tokenize (chars: string): string list = 
    chars.Replace("(", " ( ").Replace(")", " ) ").Replace("'", " ' ").Split(" ")
    |> List.ofArray |> List.filter( fun x -> x.Trim() <> "")

// let rec parseExp (input: string list) = function
//     | [] -> None
//     | [Atom(x)] -> Some(x)
//     | Begin::Atom(x)::xs -> 
//         let atom, rest = parseList(xs)
//         match atom with
//             | None -> None
//             | Some(x2) -> Some(x::x2::parseList(rest))
//     | End -> None
// and  parseList (input: string list) = function
//     | Atom(x)::xs -> parseLisp(xs)


//  Parse a list of tokens into an expression
//  All ELists must begin and end with parentheses
//  Elements can be atoms or elists
let rec (|EList|_|) = input |>
    | [] -> None   //  An empty list is not allowed
    | Begin::Atom(e)::es -> Some([e], es)   //  A single item is allowed
and rec (|ListEnd|) = input |>
    | Atom(e)::[End] -> Some([e])
    | Atom(e)::ListEnd(es) -> Some([e] :: es)
    | _ -> None

let rec (|ExprList|_|) = function
    | End::rest -> Some([], rest)
    | Atom(e)::End::rest -> Some([e], rest)
    | Atom(e)::ExprList(es, rest) -> Some(e::es, rest)
    | es -> None

let rec (|Expr|BadInput|) (input: string list) = input |> function
    | Atom(x)::rest -> Expr(x, rest)
    | Begin::Atom(e)::End::rest -> Expr(EList([e]), rest)
    | Begin::Atom(e)::Expr(EList(es), rest) -> Expr(EList(e::es), rest)
    // | Begin::Expr(e, rest) -> rest |> function
    //     | End::rest -> Expr(EList[e], rest)
    //     | Expr(EList(es), rest) -> Expr(EList(e::es), rest)
    //     | r -> BadInput(r)
    | es -> BadInput(es)
        

let parse = tokenize >> function
    | Expr(e, rest) -> Ok(e, rest)
    | BadInput(e) -> Error(e)


parse "1"
parse "hi"
parse "("
parse "(one)"
parse "(one two three)"
parse "(one (two))"

parse "(begin (123))"

parse "(begin (define r 10) (* pi (* r r)))"


let rec read_from_tokens (tokens: string list): Exp * string list = 
    match tokens with
    | [] -> raise( new exn("unexpected EOF"))
    | "(" :: tail -> 
        EList [
            for t in tail do
                yield read_from_tokens(tail)
            // for t in tail |> List.takeWhile (fun t -> t <> ")") do
            //     yield read_from_tokens(tail)
        ]
    | ")" :: tail -> raise (new exn("unexpected )"))
    | Int(x) :: tail -> Int(x), tail
    | Float(x) :: tail -> Float(x), tail
    | x :: tail -> Symbol(x), tail

let parse (program: string) = 
    read_from_tokens(tokenize(program))


let program = "(begin (define r 10) (* pi (* r r)))"
let tokens = tokenize(program)
let exp = parse(program)



        
