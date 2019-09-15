module Runtime

open Representation

let rec execute expression =
  let fail() = failwith "Invalid expression"
  match expression with
  | Binary(left, op, right) ->
    match op with
    | TokenType.Plus -> execute left + execute right
    | TokenType.Minus -> execute left - execute right
    | TokenType.Multiply -> execute left * execute right
    | TokenType.Divide -> execute left / execute right
    | TokenType.Modulo -> execute left % execute right
    | TokenType.Power -> execute left ** execute right
    | _ -> fail()
  | Unary(op, operand) ->
    match op with
    | TokenType.Plus -> execute operand
    | TokenType.Minus -> - execute operand
    | _ -> fail()
  | Constant(num) -> num
