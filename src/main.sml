(* val fileName = case CommandLine.arguments () of
  | [] => raise Fail "No file provided"
  | f :: _ => f

val input = FileSlurper.slurp fileName

val lexed = Lexer.lex input
val parsed = Parser.top lexed
  handle
    | Parser.Parser (msg', pos) =>
        let
          val line = Int.toString (#line pos)
          val column = Int.toString (#column pos)
        in
          print (fileName ^ ":"  ^ line ^ ":" ^ column ^ ": " ^ msg')
        end
(* val parseString =
  case Parser.top lexed of
    NONE =>
      "Parse error"
  | SOME (ast, _) =>
      String.concatWith "\n" (List.map Ast.toString ast)


do print (parseString ^ "\n") *)

val _ = 
  case parsed of
    | NONE => print "Parse error\n"
    | SOME (exprs, _) =>
        let
          val values =
            List.map (Evaluator.evaluate Evaluator.initialEnv) exprs
        in
          List.app
            ignore
            values
        end
  handle
    | Value.Value msg =>
        print (fileName ^ ": Evaluation failed: " ^ msg ^ "\n")
       *)

val fileName = case CommandLine.arguments () of
  | [] => raise Fail "No file provided"
  | f :: _ => f

val input = FileSlurper.slurp fileName

val _ =
  let
    val lexed = Lexer.lex input
    val parsed = Parser.top lexed

    val (_, finalEnv) =
      case parsed of
      | NONE =>
          raise Fail "Parse error"

      | SOME (exprs, _) =>
          let
            fun evalList env [] = (Value.Undef, env)
              | evalList env (expr :: rest) =
                  let
                    val (_, env') = Evaluator.evaluate env expr
                  in
                    evalList env' rest
                  end
          in
            evalList Evaluator.initialEnv exprs
          end
  in
    ()
  end
  handle
    | exn' => print ((ErrorReporter.report fileName exn') ^ "\n")
    (* | Value.Value (msg, _) =>
        print (fileName ^ ": Evaluation failed: " ^ msg ^ "\n")

    | Parser.Parser (msg', pos) =>
        let
          val line = Int.toString (#line pos)
          val column = Int.toString (#column pos)
        in
          print (fileName ^ ":" ^ line ^ ":" ^ column ^ ": " ^ msg' ^ "\n")
        end *)