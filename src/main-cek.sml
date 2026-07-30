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
            val initState = Machine.Running
              { control = Machine.Expr {expr, isTail = false}
              , env
              , kont = Machine.Done
              , trace = []
              }
          in
            case Machine.run initState of
            | Result.Ok (_, env') => runProgram env' rest
            | Result.Err (error, trace) =>
                let
                  val () =
                    print
                    <| ErrorReporter.reportFromMachine fileName error trace
                in
                  runProgram env []
                end
          end

  in
    case parsed of
    | NONE => raise Fail "Parse error"
    | SOME (exprs, _) =>
        let
          val expanded = List.map Expander.expand exprs
          val (_, _) = runProgram Env.prim expanded
        in
          ()
        end
  end
  handle exn' => print (ErrorReporter.report fileName exn' ^ "\n")
