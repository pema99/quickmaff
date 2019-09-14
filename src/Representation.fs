module Representation

//Representation
type TokenType =
  | Plus
  | Minus
  | Multiply
  | Divide
  | Power
  | Modulo
  | LeftParen
  | RightParen
  | Identifier
  | Number

type Literal =
  | Number of double
  | String of string

let extractNumber = function
  | Number(s) -> s
  | _ -> failwith "Attempted to extract wrong union case"

let extractString = function
  | String(s) -> s
  | _ -> failwith "Attempted to extract wrong union case"

type Token = {
  Type: TokenType
  Lexeme: Literal option 
}

type Expression =
  | Constant of double
  | Binary of Expression * TokenType * Expression
  | Unary of TokenType * Expression
