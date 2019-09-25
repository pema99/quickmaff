open Representation
open Lexer
open Parser
open Runtime
open Reduce

open System

[<EntryPoint>]
let main args =
  let mutable running = true
  
  let testPattern = PBinary(Invalid, PNonConstant(Invalid, 1), TokenType.Multiply, PNonConstant(Invalid, 1))
  while running do
    printf "> "
    try
      let input = Console.ReadLine()
      match input with
      | "exit" | "quit" -> running <- false
      | text when text.StartsWith("simplify") ->
        printfn "%A" (reduce (parse (lex text.[8..])))
      | text when text.StartsWith("match") ->
        printfn "%A" (matchPattern (parse (lex text.[5..])) testPattern)
      | _ -> printfn "%A" (execute (parse (lex input)))
    with
      | _ -> printfn "Invalid expression"
  0
