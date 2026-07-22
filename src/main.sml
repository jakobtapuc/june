val fileName =
  case CommandLine.arguments () of
  | [] => raise Fail "No file provided"
  | fileName' :: _ => fileName'

val input = FileSlurper.slurp fileName

val _ =
  let
    val lexed = Lexer.lex input
    val parsed = Parser.top lexed

    val (_, finalEnv) =
      case parsed of
      | NONE => raise Fail "Parse error"

      | SOME (exprs, _) =>
          let
            fun evalList env [] = (Value.Undef, env)
              | evalList env (expr :: rest) =
                  let val (_, env') = Evaluator.evaluate env expr
                  in evalList env' rest
                  end
          in
            evalList Evaluator.initialEnv exprs
          end
  in
    ()
  end
  handle | exn' => print <| ErrorReporter.report fileName exn' ^ "\n"
