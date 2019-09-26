open Representation
open Lexer
open Parser
open Runtime
open Reduce

open System

[<EntryPoint>]
let main args =
  let mutable running = true
  

  while running do
    printf "> "
    //try
    let input = Console.ReadLine()
    match input with
    | "exit" | "quit" -> running <- false
    | text when text.StartsWith("simplify") ->
      printfn "%A" (reduce (parse (lex text.[8..])))
    | _ -> printfn "%A" (execute (parse (lex input)))
    //with
    //  | _ -> printfn "Invalid expression"
  0
