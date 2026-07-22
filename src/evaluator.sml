structure Evaluator :> JUNE_EVALUATOR = struct
  (* open Prim *)
  open Value

  fun lookup (Env []) (name, pos) =
      raise Value ("Unbound variable: " ^ name, pos)
    | lookup (Env ((name', value) :: rest)) (name, pos) =
        if name = name' then
          value
        else
      lookup (Env rest) (name, pos)

  fun insert (Env env) name value =
      Env ((name, value) :: env)

  val initialEnv = 
    Env
      [ ("+", Primitive Prim.add)
      , ("-", Primitive Prim.sub)
      , ("*", Primitive Prim.mult)
      , ("/", Primitive Prim.div')
      , ("and", Primitive Prim.and')
      , ("or", Primitive Prim.or')
      , ("eq?", Primitive Prim.eq)
      , ("show", Primitive Prim.show)
      , ("show-ln", Primitive Prim.showLn)]

  fun evaluateArgs (Env env) [] =
        ([], Env env)
    | evaluateArgs (Env env) (expr :: rest) =
      let
        val (value, (Env env')) = evaluate (Env env) expr
        val (values, env'') = evaluateArgs (Env env') rest
      in
        (value :: values, env'')
      end

  and apply fnValue args pos =
    case fnValue of
      | Primitive f =>
          f args pos
      | Closure { params, body, env } =>
          let
            fun zip [] [] = []
              | zip (x :: xs) (y :: ys) = (x, y) :: zip xs ys
              | zip _ _ = raise Value ("Invalid number of arguments", pos)
            
            val bound = zip params args
            val env' = List.foldl
              (fn ((p, a), env') => insert env' p a)
              env
              bound
            val (result, _)  = evaluate env' body
          in
            result
          end

        (* Insert them into the env *)
        (* Eval the body *)
      | _ => raise Value ("Attempt to call a non-function value", pos)

  and evaluate (Env env) expr =
    case expr of
      | Ast.Integer (n, _) =>
          (Integer n, (Env env))
      | Ast.Symbol ("#t", _) =>
          (Boolean true, (Env env))
      | Ast.Symbol ("#f", _) =>
          (Boolean false, (Env env))
      | Ast.Symbol ("#undef", _) =>
          (Undef, (Env env))
      | Ast.Symbol (s, pos) =>
          (lookup (Env env) (s, pos), (Env env))
      | Ast.List ([], pos) =>
          raise Value ("Cannot evaluate an empty list", pos)
      
      | Ast.List (Ast.Symbol ("lambda", pos) :: rest, _) =>
          (case rest of
            | [Ast.List (params, _), body] =>
                  let
                    fun paramName (Ast.Symbol (p, _)) = p
                      | paramName _ = raise Value ("Param name has to be a symbol", pos)
                    
                    val paramNames = List.map paramName params
                  in
                    (Closure
                        { params = paramNames
                        , body = body
                        , env = (Env env) }
                    , (Env env))
                  end
            | _ => raise Value ("Malformed `lambda`", pos))

      | Ast.List (Ast.Symbol ("if", pos) :: rest, _) =>
          (case rest of
            | [cond, onTrue, onFalse] =>
                let
                  val (condValue, (Env env')) = evaluate (Env env) cond
                in
                  (case condValue of
                    | Boolean true =>
                        let
                          val (trueValue, (Env env'')) = evaluate (Env env') onTrue
                        in
                          (trueValue, (Env env''))
                        end
                    | Boolean false =>
                        let
                          val (falseValue, (Env env'')) = evaluate (Env env') onFalse
                        in
                          (falseValue, (Env env''))
                        end
                    | _ =>
                        let
                          val (trueValue, (Env env'')) = evaluate (Env env') onTrue
                        in
                          (trueValue, (Env env''))
                        end)
                end
            | _ =>
                raise Value ("Malformed `if`", pos))
      | Ast.List (Ast.Symbol ("define", pos) :: rest, _) =>
          (case rest of
            | [Ast.Symbol (name, _), expr'] =>
                let
                  val (value, (Env env')) = evaluate (Env env) expr'
                  val env'' = insert (Env env') name value
                in
                  (Undef, env'')
                end
            | _ =>
                raise Value ("Malformed `define`", pos))
      | Ast.List (f :: args, pos) =>
          let
            val (fnValue, (Env env')) = evaluate (Env env) f
            val (argValues, env'') = evaluateArgs (Env env') args
            val applied = apply fnValue argValues pos
          in
            (applied, env'')
          end
end