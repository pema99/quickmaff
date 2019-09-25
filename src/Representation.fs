module Representation

//Representation
type TokenType =
  | Plus
  | Minus
  | Multiply
  | Divide
  | Power
  | Modulo
  | Bang
  | EqualEqual
  | BangEqual
  | Less
  | Greater
  | LessEqual
  | GreaterEqual
  | And
  | Or
  | Equal
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
  | VarAssign of string * Expression
  | VarGet of string
  | Invalid

type PatternNode =
  | PAnyConstant  of Expression * int  
  | PConstant     of Expression * int * double
  | PNonConstant  of Expression * int
  | PBinary      of Expression * PatternNode * TokenType * PatternNode
  | PUnary       of Expression * TokenType * PatternNode
  | PWildCard    of Expression
