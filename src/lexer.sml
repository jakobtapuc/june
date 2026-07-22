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

  fun lexNumeric pos chars =
    let
      val (whole, pos1, rest1) = takeWhile Char.isDigit pos chars
    in
      case rest1 of
        #"." :: rest2 =>
          let
            val pos2 = advance pos1 #"."
            val (frac, endPos, rest3) = takeWhile Char.isDigit pos2 rest2

            val literal = String.implode whole ^ "." ^ String.implode frac
          in
            case Real.fromString literal of
              SOME r => ({token = Float r, pos = pos}, endPos, rest3)
            | NONE => raise Lexer ("Invalid float literal", pos)
          end

      | _ =>
          let
            val literal = String.implode whole
          in
            case Int.fromString literal of
              SOME n => ({token = Integer n, pos = pos}, pos1, rest1)
            | NONE => raise Lexer ("Invalid integer literal", pos)
          end
    end

  fun lexString pos chars =
    let
      fun lexString' _ _ [] =
            raise Lexer ("Unterminated string", pos)
        | lexString' current acc (#"\"" :: rest) =
            ( {token = String (String.implode (List.rev acc)), pos = pos}
            , advance current #"\""
            , rest
            )
        | lexString' current acc (c :: rest) =
            lexString' (advance current c) (c :: acc) rest
    in
      lexString' (advance pos #"\"") [] chars
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
    | lexChars pos (#"\"" :: rest) =
        let val (tok, nextPos, rest') = lexString pos rest
        in tok :: lexChars nextPos rest'
        end
    | lexChars pos (chars as c :: rest) =
        if Char.isSpace c then
          lexChars (advance pos c) rest
        else if Char.isDigit c then
          let val (tok, nextPos, rest') = lexNumeric pos chars
          in tok :: lexChars nextPos rest'
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
