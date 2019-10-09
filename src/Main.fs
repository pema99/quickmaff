open Representation
open Lexer
open Parser
open Runtime
open Reduce

open Sparse

open System

type Arguments =
  | [<AltName("h")>] Help
  | [<AltName("f"); ParamName("path")>] File of string
  | [<AltName("s")>] StdLib
  | [<AltName("q")>] Quiet
  | [<AltName("v")>] Verbose
  | [<AltName("d")>] Debug

  member this.Usage =
    match this with
    | Help _ -> "Show this help message"
    | File _ -> "Load and execute the given file"
    | StdLib _ -> "Load the standard math library"
    | Quiet _ -> "Don't show initial greeting"
    | Verbose -> "Start in verbose mode"
    | Debug _ -> "Start in debug mode"

[<EntryPoint>]
let main args =
  //Parse cli args
  let argParser = new ArgParser<Arguments>("qm")
  let args = argParser.Parse args

  let mutable running = true
  let mutable quiet = false
  let mutable verbose = false
  let mutable debug = false
  
  match args with
  | Ok(cases) ->
    for case in cases do
      match case with
      | Help ->
        argParser.Usage |> printfn "%s"
        running <- false
      | Quiet -> quiet <- true
      | Verbose -> verbose <- true
      | Debug -> debug <- true
      | _ -> ()
  | Error(errors) -> errors |> List.iter (printfn "Error: %s")
  
  //Greeting
  if not quiet && running then
    printfn "Welcome to QuickMaff."
    printfn "This is free software with no warranty."
    printfn "For help type 'help'."

  //Main loop
  while running do
    printf "> "
    try
      let input = Console.ReadLine()
      match input with
      | "exit" | "quit" ->
        running <- false
      | "help" ->
        printfn "Commands:"
        printfn "%-25s%s" "exit, quit" "Exits the program"
        printfn "%-25s%s" "simplify <expression>" "Attempt to simplify expression"
        printfn "%-25s%s" "<expression>" "Expression are evaluted by default"
        printfn ""
        printfn "Variables can be assigned to via this syntax:"
        printfn "<variable name> = <expression>"
      | text when text.StartsWith("simplify") ->
        for i in reduce (parse (lex text.[8..])) do
          printfn "%s" (print i)
      | _ ->
        printfn "%A" (execute (parse (lex input)))
    with
      | e ->
        if debug then raise e
        elif verbose then printfn "%A" e
        else printfn "Invalid expression"
  0
