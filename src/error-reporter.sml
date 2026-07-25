structure ErrorReporter: JUNE_ERROR_REPORTER =
struct
  fun report fileName exn' =
    let
      fun whereIs {line, column} =
        fileName ^ ":" ^ (Int.toString line) ^ ":" ^ (Int.toString column)

      fun withOrigin kind msg pos =
        "Error [" ^ kind ^ "] - " ^ (whereIs pos) ^ ": " ^ msg

      fun formatTrace [] = ""
        | formatTrace ({name, def, call} :: rest) =
            let
              val nameStr =
                case name of
                | NONE => "in <closure>"
                | SOME n => " in function `" ^ n ^ "`"

              val defStr =
                case def of
                | NONE => ""
                | SOME pos => "\tdefined at " ^ (whereIs pos)

              val callStr =
                case call of
                | NONE => ""
                | SOME pos => "\tcalled at " ^ (whereIs pos)
            in
              nameStr ^ "\n" ^ defStr ^ "\n" ^ callStr ^ "\n" ^ formatTrace rest
            end

      fun generic name message =
        "Error [" ^ name ^ "] - " ^ message
    in
      case exn' of
      | Parser.Parser (msg, pos) => withOrigin "Parser" msg pos
      | Lexer.Lexer (msg, pos) => withOrigin "Lexer" msg pos
      | Evaluator.Evaluator {message, pos, trace} =>
          withOrigin "Evaluation" (message ^ " " ^ (formatTrace trace)) pos
      | Env.Unbound {name, pos, trace} =>
          withOrigin "Environment"
            ("Unbound variable: " ^ name ^ " " ^ (formatTrace trace)) pos
      | Value.Type {f, expected, actual, pos, trace} =>
          withOrigin "Type"
            ("Function `" ^ f
             ^
             ("` expected type `" ^ expected ^ "`, but got: `" ^ actual ^ "` ")
             ^ (formatTrace trace)) pos
      | Value.Argument {f, expected, actual, pos, trace} =>
          withOrigin "Argument"
            ("Function `" ^ f ^ "` expected " ^ (Int.toString expected)
             ^ " arguments, but got " ^ (Int.toString actual)
             ^ (formatTrace trace)) pos
      | Expander.Expander {msg, pos, trace} =>
          withOrigin "Expansion" (msg ^ " " ^ (formatTrace trace)) pos
      | e => generic (exnName e) (exnMessage e)
    end
end
