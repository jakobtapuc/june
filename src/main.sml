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
            fun evalList env [] = (Value.Unit, env)
              | evalList env (expr :: rest) =
                  let
                    val expanded = Expander.expand expr
                    val (_, env') = Evaluator.evaluate env expanded
                  in
                    evalList env' rest
                  end
          in
            evalList Env.prim exprs
          end
  in
    ()
  end
  handle | exn' => print <| ErrorReporter.report fileName exn' ^ "\n"
