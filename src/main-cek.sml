val fileName =
  case CommandLine.arguments () of
    [] => raise Fail "No file provided"
  | fileName' :: _ => fileName'

val input = FileSlurper.slurp fileName

val _ =
  let
    val lexed = Lexer.lex input
    val parsed = Parser.top lexed

    fun runProgram env [] = (Value.VUnit, env)
      | runProgram env (expr :: rest) =
          let
            val expanded = Expander.expand expr

            val (_, env') = Machine.run
              (Machine.Running
                 { control = Machine.Expr expanded
                 , env = env
                 , kont = Machine.Done
                 })
          in
            runProgram env' rest
          end

  in
    case parsed of
    | NONE => raise Fail "Parse error"
    | SOME (exprs, _) => let val (_, _) = runProgram Env.prim exprs in () end
  end
  handle exn' => print (ErrorReporter.report fileName exn' ^ "\n")
