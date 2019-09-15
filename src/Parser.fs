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
  
  //Precedence level template for binary operators
  let parsePrecedence next matches =
    let mutable higher = next()
    while offset < tokens.Length - 1 && List.contains tokens.[offset].Type matches do
      let token = advance()
      higher <- Binary (higher, token.Type, next())
    higher

  //Algebraic precedence level
  let rec parseFactor() =
    match tokens.[offset].Type with
    | TokenType.Number -> Constant (extractNumber (Option.get (advance().Lexeme)))
    | TokenType.Plus | TokenType.Minus -> Unary (advance().Type, parseFactor())
    | TokenType.Identifier -> VarGet (extractString (Option.get (advance().Lexeme)))
    | TokenType.LeftParen ->
      advance() |> ignore
      let higher = parseAddSub();
      eat TokenType.RightParen
      higher
    | _ -> fail()
  and parsePowMod() = parsePrecedence parseFactor [TokenType.Power; TokenType.Modulo]
  and parseMulDiv() = parsePrecedence parsePowMod [TokenType.Multiply; TokenType.Divide]
  and parseAddSub() = parsePrecedence parseMulDiv [TokenType.Plus; TokenType.Minus]

  //Statement precedence level
  let parseStatement() =
    match tokens.[offset].Type with
    | TokenType.Identifier ->
      if offset < tokens.Length - 1 && tokens.[offset+1].Type = TokenType.Equal then
        let token = advance()
        advance() |> ignore
        VarAssign (extractString (Option.get (token.Lexeme)), parseAddSub())
      else
        parseAddSub()              
    | _ -> parseAddSub()
  
  parseStatement()
