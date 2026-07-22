structure ErrorReporter: JUNE_ERROR_REPORTER =
struct
  fun report fileName exn' =
    let
      fun whereIs {line, column} =
        fileName ^ ":" ^ (Int.toString line) ^ ":" ^ (Int.toString column)

      fun withOrigin kind msg pos =
        "Error [" ^ kind ^ "] - " ^ (whereIs pos) ^ ": " ^ msg

      fun generic name message =
        "Error [" ^ name ^ "] - " ^ message
    in
      case exn' of
      | Parser.Parser (msg, pos) => withOrigin "Parser" msg pos
      | Lexer.Lexer (msg, pos) => withOrigin "Lexer" msg pos
      | Value.Value (msg, pos) => withOrigin "Eval" msg pos
      | e => generic (exnName e) (exnMessage e)
    end
end
