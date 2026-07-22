structure Lexer :> JUNE_LEXER =
struct
  exception Lexer of string * Token.position

  open Token

  fun advance (pos: Token.position) c =
    if c = #"\n" then {line = (#line pos) + 1, column = 1}
    else {line = #line pos, column = (#column pos) + 1}

  fun takeWhile _ pos [] = ([], pos, [])
    | takeWhile predicate pos (c :: cs) =
        if predicate c then
          let val (taken, pos', rest) = takeWhile predicate (advance pos c) cs
          in (c :: taken, pos', rest)
          end
        else
          ([], pos, c :: cs)

  fun lexNumber pos chars =
    let
      val (digits, endPos, rest) = takeWhile Char.isDigit pos chars
    in
      case Int.fromString (String.implode digits) of
      | SOME n => ({token = Integer n, pos}, endPos, rest)
      | NONE => raise Lexer ("Non-digit characters in number literal", pos)
    end

  fun isDelimiter c =
    Char.isSpace c orelse c = #"(" orelse c = #")" orelse c = #"'"
    orelse c = #";"

  fun lexSymbol pos chars =
    let
      val (name, endPos, rest) = takeWhile (not o isDelimiter) pos chars
      val name' = String.implode name
    in
      ({token = Symbol name', pos}, endPos, rest)
    end

  fun lexComment pos [] = (pos, [])
    | lexComment pos ((nl as #"\n") :: rest) =
        (advance pos nl, rest)
    | lexComment pos (c :: rest) =
        lexComment (advance pos c) rest

  fun lexChars pos [] = [{token = Eof, pos = pos}]
    | lexChars pos ((c as #"(") :: rest) =
        {token = LParen, pos = pos} :: lexChars (advance pos c) rest
    | lexChars pos ((c as #")") :: rest) =
        {token = RParen, pos = pos} :: lexChars (advance pos c) rest
    | lexChars pos ((c as #"'") :: rest) =
        {token = Quote, pos = pos} :: lexChars (advance pos c) rest
    | lexChars pos ((c as #";") :: rest) =
        let val (nextPos, rest') = lexComment (advance pos c) rest
        in lexChars nextPos rest'
        end
    | lexChars pos (chars as c :: rest) =
        if Char.isSpace c then
          lexChars (advance pos c) rest
        else if Char.isDigit c then
          let val (token, nextPos, rest') = lexNumber pos chars
          in token :: lexChars nextPos rest'
          end
        else
          let val (tok, nextPos, rest') = lexSymbol pos chars
          in tok :: lexChars nextPos rest'
          end

  fun lex input =
    lexChars {line = 1, column = 1} (String.explode input)

  fun toString l =
    String.concatWith ", " (List.map Token.toString l)
end
