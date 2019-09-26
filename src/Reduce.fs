module Reduce
  
open Representation
open System.Collections.Generic

//AST transformation
type Transform = Expression -> Expression

//Default rules
let patterns = [
  (PBinary(PAnyConstant(1), TokenType.Multiply, PBinary(PAnyConstant(2), TokenType.Multiply, PNonConstant(1))), PBinary(PBinary(PAnyConstant(1), TokenType.Multiply, PAnyConstant(2)), TokenType.Multiply, PNonConstant(1)))
]

//Apply to entire ast
let rec apply (ast:Expression) (t:Transform) =
  match ast with
  | Constant(num) ->           t (Constant(num))  
  | Binary(left, op, right) -> t (Binary(apply left t, op, apply right t))
  | Unary(op, right) ->        t (Unary(op, apply right t))
  | VarAssign(iden, expr) ->   t (VarAssign(iden, apply expr t))
  | VarGet(iden) ->            t (VarGet(iden))
  | Invalid ->                 failwith "This should never happen"

//Table operations
let checkOrAdd (key:'A) (value:'B) (table:Dictionary<'A, 'B>) =
  if not (table.ContainsValue(value)) then
    table.[key] <- value
  table.ContainsKey(key) && table.[key] = value

let revTable (table:Dictionary<'A, 'B>) =
  let result = new Dictionary<'B, 'A>()
  for i in table do
    result.[i.Value] <- i.Key
  result

//Match the expression with a given pattern
let matchPattern expr pattern =
  let symTable = new Dictionary<string, int>()
  let numTable = new Dictionary<double, int>()
  let exprTable = new Dictionary<Expression, int>()
  let rec matchPatternCont (expr:Expression) (pattern:PatternNode) =
    match expr, pattern with
    | Constant(num), PAnyConstant(id) -> checkOrAdd num id numTable
    | Constant(num), PConstant(target) -> num = target
    | VarGet(iden), PNonConstant(id) -> checkOrAdd iden id symTable
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
    | _, PWildCard(id) -> checkOrAdd expr id exprTable 
    | _ -> false
  (matchPatternCont expr pattern), symTable, numTable, exprTable

//Replace a matched expression with a given replacement
let replacePattern replacement (symTable:Dictionary<string, int>) (numTable:Dictionary<double, int>) (exprTable:Dictionary<Expression, int>) =    
  let symTable = revTable symTable
  let numTable = revTable numTable
  let exprTable = revTable exprTable  
  let rec replacePatternCont (replacement:PatternNode) =
    match replacement with
    | PAnyConstant(id) -> Constant(numTable.[id])
    | PConstant(num) -> Constant(num)
    | PNonConstant(id) -> VarGet(symTable.[id])
    | PBinary(left, op, right) -> Binary(replacePatternCont left, op, replacePatternCont right)
    | PUnary(op, right) -> Unary(op, replacePatternCont right)
    | PWildCard(id) -> exprTable.[id]
  replacePatternCont replacement

//Match and replace a pattern with a different one if possible
let matchAndReplacePattern expr pattern replacement =
  let matched, symTable, numTable, exprTable = matchPattern expr pattern
  if matched then
    replacePattern replacement symTable numTable exprTable
  else
    expr

//Single pattern applicator
let makePatternApplicator pattern replacement =
  fun expr ->
    matchAndReplacePattern expr pattern replacement

//Applicator for all patterns
let applyPatterns:Transform = fun expr ->
  let mutable result = expr
  for (pattern, replacement) in patterns do
    result <- matchAndReplacePattern result pattern replacement
  result

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
  let seen = new HashSet<Expression>()
  seen.Add(ast) |> ignore
  let addUnique elem =
    if seen.Contains(elem) then false
    else
      seen.Add(elem) |> ignore
      true
  let rec reduceCont (coll:list<Expression>) (leaf:list<Expression>) =
    if coll.Length > 0 then
      let curr = coll.[0]
      let perms = [
        for (pattern, replacement) in patterns do
          let transform = makePatternApplicator pattern replacement
          let next = apply curr transform
          if addUnique next then yield next 
        let collapsed = apply curr collapseConstants
        if addUnique collapsed then yield collapsed
      ]
      let coll = List.append coll.[1..] perms
      let leaf = if perms.Length = 0 then (curr::leaf) else leaf
      reduceCont coll leaf
    else leaf
  reduceCont [ast] []
