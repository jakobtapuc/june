structure Evaluator :> JUNE_EVALUATOR =
struct
  local open Value
  in
    fun lookup (Env []) (name, pos) =
          raise Value ("Unbound variable: " ^ name, pos)
      | lookup (Env ((name', value) :: rest)) (name, pos) =
          if name = name' then value else lookup (Env rest) (name, pos)

    fun insert (Env env) name value =
      Env ((name, value) :: env)

    val initialEnv = Env
      [ ("+", Primitive Prim.add)
      , ("-", Primitive Prim.sub)
      , ("*", Primitive Prim.mult)
      , ("/", Primitive Prim.div')
      , ("and", Primitive Prim.and')
      , ("or", Primitive Prim.or')
      , ("eq?", Primitive Prim.eq)
      , ("show", Primitive Prim.show)
      , ("show-ln", Primitive Prim.showLn)
      ]
  end

  local open Ast
  in
    fun evaluateArgs (Value.Env env) [] = ([], Value.Env env)
      | evaluateArgs (Value.Env env) (expr :: rest) =
          let
            val (value, (Value.Env env')) = evaluate (Value.Env env) expr
            val (values, env'') = evaluateArgs (Value.Env env') rest
          in
            (value :: values, env'')
          end

    and apply fnValue args pos =
      case fnValue of
      | Value.Primitive f => f args pos
      | Value.Closure {params, body, env, pos = pos'} =>
          let
            fun zip [] [] = []
              | zip (x :: xs) (y :: ys) =
                  (x, y) :: zip xs ys
              | zip _ _ =
                  raise Value.Value ("Invalid number of arguments", pos')

            val bound = zip params args
            val env' =
              List.foldl (fn ((p, a), env') => insert env' p a) env bound
            val (result, _) = evaluateSeq pos' env' body
          in
            result
          end
      | _ => raise Value.Value ("Attempt to call a non-function value", pos)

    and evaluate (Value.Env env) expr =
      case expr of
      | Integer (n, _) => (Value.Integer n, (Value.Env env))
      | Symbol ("#t", _) => (Value.Boolean true, (Value.Env env))
      | Symbol ("#f", _) => (Value.Boolean false, (Value.Env env))
      | Symbol ("#undef", _) => (Value.Undef, (Value.Env env))
      | Symbol (s, pos) => (lookup (Value.Env env) (s, pos), (Value.Env env))
      | List ([], pos) =>
          raise Value.Value ("Cannot evaluate an empty list", pos)
      | List (Symbol ("lambda", pos) :: List (params, _) :: body, _) =>
          let
            fun paramName (Symbol (p, _)) = p
              | paramName _ =
                  raise Value.Value ("Param name has to be a symbol", pos)

            val paramNames = List.map paramName params
          in
            if null body then
              raise Value.Value ("Lambda needs a body", pos)
            else
              ( Value.Closure
                  { params = paramNames
                  , body = body
                  , env = Value.Env env
                  , pos = pos
                  }
              , Value.Env env
              )
          end

      | List (Symbol ("if", pos) :: rest, _) =>
          (case rest of
           | [cond, onTrue, onFalse] =>
               let
                 val (condValue, (Value.Env env')) =
                   evaluate (Value.Env env) cond
               in
                 (case condValue of
                  | Value.Boolean true =>
                      let
                        val (trueValue, (Value.Env env'')) =
                          evaluate (Value.Env env') onTrue
                      in
                        (trueValue, (Value.Env env''))
                      end
                  | Value.Boolean false =>
                      let
                        val (falseValue, (Value.Env env'')) =
                          evaluate (Value.Env env') onFalse
                      in
                        (falseValue, (Value.Env env''))
                      end
                  | _ =>
                      let
                        val (trueValue, (Value.Env env'')) =
                          evaluate (Value.Env env') onTrue
                      in
                        (trueValue, (Value.Env env''))
                      end)
               end
           | _ => raise Value.Value ("Malformed `if`", pos))
      | List (Symbol ("define", pos) :: rest, _) =>
          (case rest of
           | [Symbol (name, _), expr'] =>
               let
                 val (value, (Value.Env env')) = evaluate (Value.Env env) expr'
                 val env'' = insert (Value.Env env') name value
               in
                 (Value.Undef, env'')
               end
           | _ => raise Value.Value ("Malformed `define`", pos))
      | List (f :: args, pos) =>
          let
            val (fnValue, (Value.Env env')) = evaluate (Value.Env env) f
            val (argValues, env'') = evaluateArgs (Value.Env env') args
            val applied = apply fnValue argValues pos
          in
            (applied, env'')
          end
    and evaluateSeq _ env [expr] = evaluate env expr
      | evaluateSeq pos env (expr :: rest) =
          let val (_, env') = evaluate env expr
          in evaluateSeq pos env' rest
          end
      | evaluateSeq pos _ [] =
          raise Value.Value ("Empty body", pos)
  end
end
