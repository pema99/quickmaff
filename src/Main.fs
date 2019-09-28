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
    try
      let input = Console.ReadLine()
      match input with
      | "exit" | "quit" ->
        running <- false
      | text when text.StartsWith("simplify") ->
        for i in reduce (parse (lex text.[8..])) do
          printfn "%s" (print i)
          //printfn "%A" i
      | _ ->
        printfn "%A" (execute (parse (lex input)))
    with
      | _ -> printfn "Invalid expression"
  0
