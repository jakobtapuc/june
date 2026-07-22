structure Parser :> JUNE_PARSER =
struct
  exception Parser of string * Token.position

  type 'a t = Token.t list -> ('a * Token.t list) option

  fun pure x tokens = SOME (x, tokens)

  fun fail _ = NONE

  fun map f parser tokens =
    case parser tokens of
    | NONE => NONE
    | SOME (value, rest) => SOME (f value, rest)

  fun bind parser f tokens =
    case parser tokens of
    | NONE => NONE
    | SOME (value, rest) => f value rest

  fun op>>= (p, f) = bind p f

  fun orElse p1 p2 tokens =
    case p1 tokens of
    | NONE => p2 tokens
    | result => result

  fun op<|> (p1, p2) = orElse p1 p2

  fun satisfy predicate (tokens: Token.t list) =
    case tokens of
    | [] => NONE
    | tok :: rest => if predicate tok then SOME (tok, rest) else NONE

  fun many p tokens =
    case p tokens of
    | NONE => SOME ([], tokens)
    | SOME (value, rest) =>
        case many p rest of
        | SOME (values, final) => SOME (value :: values, final)
        | NONE => SOME ([value], rest)

  fun token predicate =
    satisfy (fn t => predicate (#token t))

  val isEof = (fn Token.Eof => true | _ => false)

  val eof = token isEof

  val lparen = token (fn Token.LParen => true | _ => false)

  val rparen = token (fn Token.RParen => true | _ => false)

  val integer =
    map
      (fn t =>
         case (#token t) of
         | Token.Integer n => Ast.Integer (n, (#pos t))
         | _ => raise Parser ("Impossible path", (#pos t)))
      (satisfy (fn t =>
         case (#token t) of
         | Token.Integer _ => true
         | _ => false))

  val float =
    map
      (fn t =>
         case (#token t) of
           Token.Float n => Ast.Float (n, #pos t)
         | _ => raise Parser ("Impossible path", #pos t))
      (satisfy (fn t =>
         case (#token t) of
         | Token.Float _ => true
         | _ => false))

  val string' =
    map
      (fn t =>
         case (#token t) of
         | Token.String s => Ast.String (s, (#pos t))
         | _ => raise Parser ("Impossible path", (#pos t)))
      (satisfy (fn t =>
         case (#token t) of
         | Token.String _ => true
         | _ => false))

  val symbol =
    map
      (fn t =>
         case (#token t) of
         | Token.Symbol s => Ast.Symbol (s, (#pos t))
         | _ => raise Parser ("Impossible path", (#pos t)))
      (satisfy (fn t =>
         case (#token t) of
         | Token.Symbol _ => true
         | _ => false))

  fun quoteExpr tokens =
    case
      satisfy
        (fn t =>
           case #token t of
           | Token.Quote => true
           | _ => false) tokens
    of
    | NONE => NONE

    | SOME (quoteTok, rest) =>
        case expr rest of
        | NONE =>
            raise Parser ("Expected expression after quote", #pos quoteTok)
        | SOME (value, rest') =>
            SOME
              ( Ast.List
                  ([Ast.Symbol ("quote", #pos quoteTok), value], #pos quoteTok)
              , rest'
              )

  and list' tokens =
    case lparen tokens of
    | NONE => NONE
    | SOME (lp, rest) =>
        case many expr rest of
        | NONE => NONE
        | SOME (items, rest') =>
            case rparen rest' of
            | NONE => raise Parser ("Unclosed parenthesis", (#pos lp))
            | SOME (_, rest'') => SOME (Ast.List (items, (#pos lp)), rest'')

  and expr tokens =
    integer <|> float <|> string' <|> symbol <|> list' <|> quoteExpr <| tokens

  fun seq p1 p2 =
    p1 >>= (fn x => map (fn y => (x, y)) p2)

  fun top tokens =
    case many expr tokens of
      SOME (expressions, rest) =>
        (case token isEof rest of
         | SOME (_, []) => SOME (expressions, [])
         | SOME (_, tok :: _) => raise Parser ("Unexpected tokens", #pos tok)
         | NONE => raise Parser ("Expected EOF", #pos (hd rest)))
    | NONE => NONE

  fun run p tokens = p tokens
end
