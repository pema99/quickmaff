module Representation

//Token types
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

//Literals
type Literal =
  | Number of double
  | String of string

let extractNumber = function
  | Number(s) -> s
  | _ -> failwith "Attempted to extract wrong union case"

let extractString = function
  | String(s) -> s
  | _ -> failwith "Attempted to extract wrong union case"

//Single token
type Token = {
  Type: TokenType
  Lexeme: Literal option 
}

//AST
type Expression =
  | Constant of double
  | Binary of Expression * TokenType * Expression
  | Unary of TokenType * Expression
  | VarAssign of string * Expression
  | VarGet of string
  | Invalid

//Pattern tree
type Pattern =
  | PAnyConstant of int  
  | PConstant    of double
  | PNonConstant of int
  | PBinary      of Pattern * TokenType * Pattern
  | PUnary       of TokenType * Pattern
  | PWildCard    of int

//AST transformation
type Transform = Expression -> Expression
