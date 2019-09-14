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
  
  //Precedence level template
  let parsePrecedence next matches =
    let mutable higher = next()
    while offset < tokens.Length - 1 && List.contains tokens.[offset].Type matches do
      let token = advance()
      higher <- Binary (higher, token.Type, next())
    higher

  //Precedence levels
  let rec parseFactor() =
    match tokens.[offset].Type with
    | TokenType.Number -> Constant (extractNumber (Option.get (advance().Lexeme)))
    | TokenType.Plus | TokenType.Minus -> Unary (advance().Type, parseFactor())
    | TokenType.LeftParen -> eat TokenType.LeftParen
                             let higher = parseAddSub();
                             eat TokenType.RightParen
                             higher
    | _ -> fail()
  and parsePowMod() = parsePrecedence parseFactor [TokenType.Power; TokenType.Modulo]
  and parseMulDiv() = parsePrecedence parsePowMod [TokenType.Multiply; TokenType.Divide]
  and parseAddSub() = parsePrecedence parseMulDiv [TokenType.Plus; TokenType.Minus]
    
  parseAddSub()
