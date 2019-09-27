module Reduce
  
open Representation
open System.Collections.Generic

//Default rules
let patterns = [
  (PBinary(PAnyConstant(1), TokenType.Multiply, PBinary(PAnyConstant(2), TokenType.Multiply, PNonConstant(1))), PBinary(PBinary(PAnyConstant(1), TokenType.Multiply, PAnyConstant(2)), TokenType.Multiply, PNonConstant(1)))
]

//Apply to entire ast
let rec applyTransform (ast:Expression) (t:Transform) =
  match ast with
  | Constant(num) ->           t (Constant(num))  
  | Binary(left, op, right) -> t (Binary(applyTransform left t, op, applyTransform right t))
  | Unary(op, right) ->        t (Unary(op, applyTransform right t))
  | VarAssign(iden, expr) ->   t (VarAssign(iden, applyTransform expr t))
  | VarGet(iden) ->            t (VarGet(iden))
  | Invalid ->                 failwith "This should never happen"

//Match the expression with a given pattern
let matchPattern expr pattern =
  let symTable = new Dictionary<int, string>()
  let numTable = new Dictionary<int, double>()
  let exprTable = new Dictionary<int, Expression>()
  let checkOrAdd (key:'A) (value:'B) (table:Dictionary<'A, 'B>) =
    if not (table.ContainsKey(key)) then
      table.[key] <- value
    table.[key] = value
  let rec matchPatternCont (expr:Expression) (pattern:Pattern) =
    match expr, pattern with
    | Constant(num), PAnyConstant(id) -> checkOrAdd id num numTable
    | Constant(num), PConstant(target) -> num = target
    | VarGet(iden), PNonConstant(id) -> checkOrAdd id iden symTable
    | Binary(left, op, right), PBinary(leftPattern, opTarget, rightPattern) ->
      if op = opTarget then
        let leftMatched = matchPatternCont left leftPattern
        let rightMatched = matchPatternCont right rightPattern
        leftMatched && rightMatched
      else
        false
    | Unary(op, right), PUnary(opTarget, rightPattern) -> 
      if op = opTarget then
        matchPatternCont right rightPattern
      else
        false
    | _, PWildCard(id) -> checkOrAdd id expr exprTable 
    | _ -> false
  (matchPatternCont expr pattern), symTable, numTable, exprTable

//Replace a matched expression with a given replacement
let replacePattern replacement (symTable:Dictionary<int, string>) (numTable:Dictionary<int, double>) (exprTable:Dictionary<int, Expression>) =    
  let rec replacePatternCont (replacement:Pattern) =
    match replacement with
    | PAnyConstant(id) ->         Constant(numTable.[id])
    | PConstant(num) ->           Constant(num)
    | PNonConstant(id) ->         VarGet(symTable.[id])
    | PBinary(left, op, right) -> Binary(replacePatternCont left, op, replacePatternCont right)
    | PUnary(op, right) ->        Unary(op, replacePatternCont right)
    | PWildCard(id) ->            exprTable.[id]
  replacePatternCont replacement

//Match and replace a pattern with a different one if possible
let applyPattern expr pattern replacement =
  let matched, symTable, numTable, exprTable = matchPattern expr pattern
  if matched then replacePattern replacement symTable numTable exprTable
  else expr

//Special pattern, collapse all constants
let collapseConstants:Transform = fun expr ->
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

//Apply all possible patterns and return the set of nonreducable expressions
let reduce (ast:Expression) =
  let addUnique elem (perms:list<Expression>, seen:Set<Expression>) =
    if seen.Contains(elem) then (perms, seen)
    else (elem::perms, seen.Add elem)
  let rec reduceCont (coll:list<Expression>) (seen:Set<Expression>) (leaf:list<Expression>) =
    if coll.Length > 0 then
      let curr = coll.[0]
      let (perms, seen) = (([], seen), patterns) ||> List.fold (fun acc rule ->
        let (perms, seen) = acc
        let (pattern, replacement) = rule
        let transform = fun expr -> applyPattern expr pattern replacement
        let next = applyTransform curr transform
        (perms, seen) |> addUnique next
      ) 
      let collapsed = applyTransform curr collapseConstants
      let (perms, seen) = (perms, seen) |> addUnique collapsed 
      let coll = List.append coll.[1..] perms
      let leaf = if perms.Length = 0 then (curr::leaf) else leaf
      reduceCont coll seen leaf
    else leaf
  reduceCont [ast] (set[ast]) []
  
