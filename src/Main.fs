open Representation
open Lexer
open Parser
open Runtime

open System

[<EntryPoint>]
let main args =
  while true do
    printf "> "
    try
      printfn "%A" (execute (parse (lex (Console.ReadLine()))))
    with
      | _ -> printfn "Invalid expression"
  0
