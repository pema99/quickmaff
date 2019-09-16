module Runtime

open Representation

open System.Collections.Generic;

let variables = new Dictionary<string, double>();

let rec execute expression =
  let fail() = failwith "Invalid expression"
  match expression with
  | Binary(left, op, right) ->
    match op with
    | TokenType.Plus ->         execute left + execute right
    | TokenType.Minus ->        execute left - execute right
    | TokenType.Multiply ->     execute left * execute right
    | TokenType.Divide ->       execute left / execute right
    | TokenType.Modulo ->       execute left % execute right
    | TokenType.Power ->        execute left ** execute right
    | TokenType.EqualEqual ->   if execute left = execute right  then 1.0 else 0.0
    | TokenType.BangEqual ->    if execute left <> execute right then 1.0 else 0.0
    | TokenType.Less ->         if execute left < execute right  then 1.0 else 0.0
    | TokenType.Greater ->      if execute left > execute right  then 1.0 else 0.0
    | TokenType.LessEqual ->    if execute left <= execute right then 1.0 else 0.0
    | TokenType.GreaterEqual -> if execute left >= execute right then 1.0 else 0.0
    | TokenType.And ->          if execute left <> 0.0 && execute right <> 0.0 then 1.0 else 0.0
    | TokenType.Or ->           if execute left <> 0.0 || execute right <> 0.0 then 1.0 else 0.0
    | _ -> fail()
  | Unary(op, operand) ->
    match op with
    | TokenType.Plus ->    execute operand
    | TokenType.Minus -> - execute operand
    | TokenType.Bang ->    if execute operand = 0.0 then 1.0 else 0.0
    | _ -> fail()
  | Constant(num) -> num
  | VarAssign(iden, operand) ->
    variables.[iden] <- execute operand;
    variables.[iden]
  | VarGet(iden) ->
    if variables.ContainsKey(iden) then
      variables.[iden]
    else
      0.0
