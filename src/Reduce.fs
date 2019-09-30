module Reduce
  
open Representation
open System.Collections.Generic

//Default rules
let newRule pattern replacement =
  (Parser.parsePattern (Lexer.lex pattern), Parser.parsePattern (Lexer.lex replacement))

let rules = [
  newRule "L1*(L2*N1)" "(L1*L2)*N1"
  newRule "L1+(L2+N1)" "(L1+L2)+N1"
  newRule "N1+(L1+N2)" "L1+(N1+N2)"
  
  newRule "N1*L1" "L1*N1"
  newRule "N1+L1" "L1+N1"

  newRule "0+W1" "W1" 
  newRule "0*W1" "0"
  newRule "W1-0" "W1"
  newRule "0-W1" "-W1"
  newRule "0/W1" "0"
  newRule "1*W1" "W1"
  newRule "W1/1" "W1"

  newRule "W1*W1" "W1^2"
  newRule "W1-W1" "0"
  newRule "W1+W1" "2*W1"
  newRule "W1/W1" "1" //EDGE CASE: 0/0

  newRule "(L1^W1)*(L2^W1)" "(L1*L2)^W1"
  newRule "(W1^L1)*(W1^L2)" "W1^(L1+L2)"
  newRule "W1*(W1^L1)" "W1^(L1+1)"
  newRule "(W1^L1)^L2" "W1^(L1*L2)"
  newRule "W1^0" "1"
  newRule "1^W1" "1"
  newRule "W1^1" "W1"
  newRule "W1^-1" "1/W1"
  newRule "W1*(W1*W2)" "W2*(W1^2)"
  newRule "W1+(W1+W2)" "W1*2+W2"
  newRule "W1+(W1*W2)" "W1*(W2+1)"
  newRule "W1*(W1^L1*W2)" "W2*W1^(L1+1)"
  
  newRule "(L1*N1)+N1" "(L1+1)*N1"
  newRule "N1+(L1*N1)" "(L1+1)*N1"

  newRule "(W1*W2)+(W3*W2)" "(W1+W3)*W2"
  newRule "(W1*W2)*(W3*W2)" "(W1*W3)*(W2^2)"
  newRule "(W1+W2)+(W3+W2)" "(W1+W3)+(W2*2)"
  newRule "(L1*W1)/L1" "W1"

  newRule "W1*W2" "W2*W1"
  newRule "W1+W2" "W2+W1"
]

//Apply to entire ast
let rec applyTransform (ast:Expression) (t:Transform) =
  match ast with
  | Constant(num) ->           t (Constant(num))  
  | Binary(left, op, right) -> t (Binary(applyTransform left t, op, applyTransform right t))
  | Unary(op, right) ->        t (Unary(op, applyTransform right t))
  | VarAssign(iden, expr) ->   t (VarAssign(iden, applyTransform expr t))
  | VarGet(iden) ->            t (VarGet(iden))

//Check if an expression is fully constant or nonconstant
let rec checkConstant expr =
  match expr with
  | Constant(num) -> true
  | VarGet(iden) -> false
  | Binary(left, op, right) -> checkConstant left && checkConstant right
  | Unary(op, right) -> checkConstant right
  | _ -> false
  
//Match the expression with a given pattern
let matchPattern expr pattern =
  let symTable = new Dictionary<int, Expression>()
  let numTable = new Dictionary<int, Expression>()
  let exprTable = new Dictionary<int, Expression>()
  let checkOrAdd (key:'A) (value:'B) (table:Dictionary<'A, 'B>) =
    if not (table.ContainsKey(key)) then
      table.[key] <- value
    table.[key] = value
  let rec matchPatternCont (expr:Expression) (pattern:Pattern) =
    match expr, pattern with
    | _, PAnyConstant(id) ->
      if checkConstant expr then
        checkOrAdd id expr numTable
      else
        false
    | Constant(num), PConstant(target) ->
      num = target
    | _, PNonConstant(id) ->
      if checkConstant expr then
        false
      else
        checkOrAdd id expr symTable
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
let replacePattern replacement (symTable:Dictionary<int, Expression>) (numTable:Dictionary<int, Expression>) (exprTable:Dictionary<int, Expression>) =    
  let rec replacePatternCont (replacement:Pattern) =
    match replacement with
    | PAnyConstant(id) ->         numTable.[id]
    | PConstant(num) ->           Constant(num)
    | PNonConstant(id) ->         symTable.[id]
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
    | Constant(numL), Constant(numR) -> Constant(Runtime.execute expr)
    | _ -> expr
  | Unary(op, right) ->
    match right with
    | Constant(num) -> Constant(Runtime.execute expr)
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
      let (perms, seen) = (([], seen), rules) ||> List.fold (fun acc rule ->
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
  let res = reduceCont [ast] (set[ast]) []
  let depths = res |> List.map treeDepth
  let minDepth = depths |> List.min
  let filtered = (List.zip res depths) |> List.filter (fun (x, y) -> y <= minDepth)
  let res = fst (List.unzip filtered)
  printfn "%A" res
  res
