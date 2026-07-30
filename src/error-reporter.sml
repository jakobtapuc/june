structure ErrorReporter: JUNE_ERROR_REPORTER =
struct
  fun reportFromMachine fileName error trace =
    let
      fun whereIs {line, column} =
        fileName ^ ":" ^ (Int.toString line) ^ ":" ^ (Int.toString column)

      fun formatRuntimeError err =
        case err of
          Machine.TypeError {proc, pos, expected, actual} =>
            whereIs pos ^ ": Procedure `" ^ proc ^ "` expected `" ^ expected
            ^ "`, but got `" ^ actual ^ "`."

        | Machine.ArityError {proc, pos, expected, actual} =>
            whereIs pos ^ ": Procedure `" ^ (Option.getOpt (proc, "<closure>"))
            ^ "` expected " ^ Int.toString expected ^ " argument"
            ^ (if expected = 1 then "" else "s") ^ ", but got "
            ^ Int.toString actual ^ "."

        | Machine.GenericError msg => msg

      fun formatMachineTrace [] = ""
        | formatMachineTrace (frame :: rest) =
            let
              val formatted =
                case frame of
                  Machine.CallFrame {proc, call} =>
                    let
                      val procName =
                        case proc of
                          SOME (Value.VClosure {name, ...}) =>
                            (case !name of
                               SOME n => n
                             | NONE => "<closure>")
                        | SOME (Value.VPrimitive _) => "<primitive>"
                        | SOME _ => "<procedure>"
                        | NONE => "<unknown>"
                    in
                      "\tcalled `" ^ procName ^ "` at " ^ whereIs call ^ "\n"
                    end

                | Machine.DefineFrame {name, pos} =>
                    "\tdefined `" ^ name ^ "` at " ^ whereIs pos ^ "\n"
            in
              formatted ^ formatMachineTrace rest
            end
    in
      "Error [Runtime] - " ^ formatRuntimeError error ^ "\n\nStack trace:\n"
      ^ formatMachineTrace trace
    end

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
      | Expander.Expander {msg, pos, trace} =>
          withOrigin "Expansion" (msg ^ " " ^ (formatTrace trace)) pos
      | e => generic (exnName e) (exnMessage e)
    end
end
