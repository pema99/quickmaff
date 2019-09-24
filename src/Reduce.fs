module Reduce

open Representation

type Transform = Expression -> Expression

let rec apply (ast:Expression) (t:Transform) =
  match ast with
  | Constant(num) ->           t (Constant(num))  
  | Binary(left, op, right) -> t (Binary(apply left t, op, apply right t))
  | Unary(op, right) ->        t (Unary(op, apply right t))
  | VarAssign(iden, expr) ->   t (VarAssign(iden, apply expr t))
  | VarGet(iden) ->            t (VarGet(iden))

//L1 _ L2 = L3
let ruleCollapseLiterals:Transform = fun expr ->
  match expr with
  | Binary(left, op, right) ->
    match left, right with
    | Constant(numL), Constant(numR) ->
      match op with
      | TokenType.Plus ->     Constant(numL + numR)
      | TokenType.Minus ->    Constant(numL - numR)
      | TokenType.Multiply -> Constant(numL * numR)
      | TokenType.Divide ->   Constant(numL / numR)
      | TokenType.Modulo ->   Constant(numL % numR)
      | TokenType.Power ->    Constant(numL ** numR)
      | _ -> failwith "Unexpected token" //TODO: Support boolean ops
    | _ -> expr
  | Unary(op, right) ->
    match right with
    | Constant(num) ->
      match op with
      | TokenType.Plus ->  Constant(num)
      | TokenType.Minus -> Constant(-num)
      | _ -> failwith "Unexpected token" //TODO: Support boolean ops
    | _ -> expr
  | _ -> expr

let applyOp a op b =
  match op with
  | TokenType.Plus     -> a + b 
  | TokenType.Multiply -> a * b 
  | TokenType.Divide   -> a / b 
  | TokenType.Minus    -> a - b 
  | _ -> failwith "Fail"

let invOp op = 
  match op with
  | TokenType.Plus     -> TokenType.Minus 
  | TokenType.Multiply -> TokenType.Divide
  | TokenType.Divide   -> TokenType.Multiply
  | TokenType.Minus    -> TokenType.Plus
  | _ -> failwith "Fail"

let applyInvOp a op b =
  applyOp a (invOp op) b
  
//L1 * (L2 * N1) = (L1 * L2) * N1
//(L1 * N1) * L2 = (L1 * L2) * N1
//L1 + (L2 + N1) = (L1 + L2) + N1
//(L1 + N1) + L2 = (L1 + L2) + N1
let ruleMulLiteralsLeft:Transform = fun expr ->
  match expr with
  | Binary(left, op, right) when op = TokenType.Multiply || op = TokenType.Plus ->
    let cont exprs = 
      match exprs with
      | Constant(numL1), Binary(exprRL, opR, exprRR) when op = opR ->
        match exprRL, exprRR with
        | Constant(numL2), VarGet(_) -> Binary(Constant(applyOp numL1 op numL2), op, exprRR) 
        | VarGet(_), Constant(numL2) -> Binary(Constant(applyOp numL1 op numL2), op, exprRL)
        | _ -> expr
      | _ -> expr
    let normal = cont (left, right)
    if normal = expr then cont (right, left)
    else normal
  | _ -> expr


//L1 / (L2 / N1) = (L1 / L2) * N1    O      addendum: L1 / (L2 * N1) = (L1 / L2) / N1     
//(L1 / N1) / L2 = (L1 / L2) / N1    
//L1 / (N1 / L2) = (L1 * L2) / N1    O
//(N1 / L1) / L2 = N1 / (L1 * L2)    


//L1 - (L2 - N1) = (L1 - L2) + N1    O
//(L1 - N1) - L2 = (L1 - L2) - N1    
//L1 - (N1 - L2) = (L1 + L2) - N1    O
//(N1 - L1) - L2 = N1 - (L1 + L2)    
let ruleDivLiteralsLeft:Transform = fun expr ->
  match expr with
  | Binary(left, op, right) when op = TokenType.Divide || op = TokenType.Minus ->
    match left, right with
    | Constant(numL1), Binary(exprRL, opR, exprRR) when op = opR ->
      match exprRL, exprRR with
      | Constant(numL2), VarGet(_) -> Binary(Constant(applyOp numL1 op numL2), invOp op, exprRR) 
      | VarGet(_), Constant(numL2) -> Binary(Constant(applyInvOp numL1 op numL2), op, exprRL)
      | _ -> expr
    | _ -> expr
  | _ -> expr

let reduce (ast:Expression) =
  apply (apply (apply ast ruleMulLiteralsLeft) ruleCollapseLiterals) ruleDivLiteralsLeft
