open Representation
open Lexer
open Parser
open Runtime

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
    | _ -> printfn "%A" (execute (parse (lex input)))
    //with
    //  | _ -> printfn "Invalid expression"
  0
