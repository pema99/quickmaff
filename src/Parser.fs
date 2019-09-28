module Parser

open Representation

//Parsing
let parse (tokens:list<Token>) =
  let fail() = failwith "Unexpected token"

  //State
  let mutable offset = 0
  let advance() =
    offset <- offset + 1
    tokens.[offset - 1]
  let eat token =
    if advance().Type <> token then
      fail()
  
  //Precedence level template for infix operators
  let parsePrecedence next matches =
    let mutable higher = next()
    while offset < tokens.Length - 1 && List.contains tokens.[offset].Type matches do
      let token = advance()
      higher <- Binary (higher, token.Type, next())
    higher

  //Expression precedence levels
  let rec parsePrimary() =
    match tokens.[offset].Type with
    | TokenType.Number -> Constant (extractNumber (Option.get (advance().Lexeme)))
    | TokenType.Plus | TokenType.Minus | TokenType.Bang -> Unary (advance().Type, parsePrimary())
    | TokenType.Identifier -> VarGet (extractString (Option.get (advance().Lexeme)))
    | TokenType.LeftParen ->
      advance() |> ignore
      let higher = parseBoolean();
      eat TokenType.RightParen
      higher
    | _ -> fail()
  and parseExponent() =
    parsePrecedence parsePrimary [TokenType.Power; TokenType.Modulo]
  and parseFactor() =
    parsePrecedence parseExponent [TokenType.Multiply; TokenType.Divide]
  and parseTerm() =
    parsePrecedence parseFactor [TokenType.Plus; TokenType.Minus]
  and parseComparison() =
    parsePrecedence parseTerm [TokenType.Less; TokenType.Greater; TokenType.LessEqual; TokenType.GreaterEqual]
  and parseEquality() =
    parsePrecedence parseComparison [TokenType.EqualEqual; TokenType.BangEqual]
  and parseBoolean() =
    parsePrecedence parseEquality [TokenType.And; TokenType.Or]

  //Statement precedence level
  let parseStatement() =
    match tokens.[offset].Type with
    | TokenType.Identifier ->
      if offset < tokens.Length - 1 && tokens.[offset+1].Type = TokenType.Equal then
        let token = advance()
        advance() |> ignore
        VarAssign (extractString (Option.get (token.Lexeme)), parseBoolean())
      else
        parseBoolean()              
    | _ -> parseBoolean()
  
  let res = parseStatement()
  //Only 1 expression allowed
  if offset <> tokens.Length then fail() 
  res

let parsePattern (tokens:list<Token>) =
  let fail() = failwith "Unexpected token in pattern string"
  let rec parsePatternCont expr =
    match expr with
    | Constant(num) -> PConstant(num)
    | Binary(left, op, right) -> PBinary(parsePatternCont left, op, parsePatternCont right)
    | Unary(op, right) -> PUnary(op, parsePatternCont right)
    | VarGet(iden) ->
      if iden.Length < 2 then fail()
      let id = int (iden.[1..])
      match iden.[0] with
      | 'L' -> PAnyConstant(id)
      | 'N' -> PNonConstant(id)
      | 'W' -> PWildCard(id)
      | _ -> fail()
    | _ -> fail()
  parsePatternCont (parse tokens)
