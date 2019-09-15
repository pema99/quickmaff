module Lexer

open Representation

//Lexing
let takeLexeme text op =
  let rec takeLexemeCont (text:string) (out:string) op =
    if text.Length = 0 then
      out
    else
      match text.[0] with
      | c when op c -> takeLexemeCont text.[1..] (out + string c) op
      | _ -> out
  takeLexemeCont text "" op

let takeNumber text =
  let lexeme = takeLexeme text (fun c -> c >= '0' && c <= '9' || c = '.')
  Number (double lexeme), lexeme.Length

let takeIdentifier text =
  let lexeme = takeLexeme text (fun c ->
      (c >= 'A' && c <= 'Z') ||
      (c >= 'a' && c <= 'z') ||
      (c >= '0' && c <= '9'))
  String lexeme, lexeme.Length

let lex text =
  let rec lexCont (text:string) (tokens:list<Token>) =
    if text.Length = 0 then
      tokens |> List.rev
    else
      match text.[0] with
      | '+' -> lexCont text.[1..] ({ Type = TokenType.Plus; Lexeme = None }::tokens) 
      | '-' -> lexCont text.[1..] ({ Type = TokenType.Minus; Lexeme = None }::tokens)
      | '*' -> lexCont text.[1..] ({ Type = TokenType.Multiply; Lexeme = None }::tokens)
      | '/' -> lexCont text.[1..] ({ Type = TokenType.Divide; Lexeme = None }::tokens) 
      | '^' -> lexCont text.[1..] ({ Type = TokenType.Power; Lexeme = None }::tokens)
      | '%' -> lexCont text.[1..] ({ Type = TokenType.Modulo; Lexeme = None }::tokens)
      | '=' -> lexCont text.[1..] ({ Type = TokenType.Equal; Lexeme = None }::tokens)
      | '(' -> lexCont text.[1..] ({ Type = TokenType.LeftParen; Lexeme = None }::tokens)
      | ')' -> lexCont text.[1..] ({ Type = TokenType.RightParen; Lexeme = None }::tokens)
        
      | c when c >= '0' && c <= '9' ->
        let num, numLength = takeNumber text
        lexCont text.[numLength..] ({ Type = TokenType.Number; Lexeme = Some (num) }::tokens)

      | c when (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') ->
        let str, strLength = takeIdentifier text
        lexCont text.[strLength..] ({ Type = TokenType.Identifier; Lexeme = Some (str) }::tokens)

      | _ -> lexCont text.[1..] tokens
  lexCont text []
