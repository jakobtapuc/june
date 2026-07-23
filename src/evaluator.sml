structure Evaluator :> JUNE_EVALUATOR =
struct
  local open Ast Env
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
              List.foldl (fn ((p, a), env') => insert env' p (ref a)) env bound
            val (result, _) = evaluateSeq pos' env' body
          in
            result
          end
      | _ => raise Value.Value ("Attempt to call a non-function value", pos)

    and evaluate (Value.Env env) expr =
      case expr of
      | Integer (n, _) => (Value.Integer n, (Value.Env env))
      | Float (f, _) => (Value.Float f, (Value.Env env))
      | String (s, _) => (Value.String s, (Value.Env env))
      | Symbol ("#t", _) => (Value.Boolean true, (Value.Env env))
      | Symbol ("#f", _) => (Value.Boolean false, (Value.Env env))
      | Symbol ("#undef", _) => (Value.Undef, (Value.Env env))
      | Symbol (s, pos) => (lookup (Value.Env env) (s, pos), (Value.Env env))
      | List ([], pos) =>
          raise Value.Value ("Cannot evaluate an empty list", pos)
      | List (Symbol ("quote", _) :: [quoted], _) =>
          (quoteValue quoted, Value.Env env)
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
      | (let' as (List (Symbol ("let", _) :: List (_, _) :: _, _))) =>
          let val expanded = Expander.expandLet let'
          in evaluate (Value.Env env) expanded
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
      | (and' as (List (Symbol ("and", _) :: _, _))) =>
          let val expanded = Expander.expandAnd and'
          in evaluate (Value.Env env) expanded
          end
      | (or' as (List (Symbol ("or", _) :: _, _))) =>
          let val expanded = Expander.expandOr or'
          in evaluate (Value.Env env) expanded
          end
      | List (Symbol ("define", pos) :: rest, _) =>
          (case rest of
           | [Symbol (name, _), expr'] =>
               let
                 val cell = ref Value.Undef
                 val env' = insert (Value.Env env) name cell
                 val (value, _) = evaluate env' expr'
                 val () = cell := value
                 val env'' = insert env' name (ref value)
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
    and quoteList [] = Value.Nil
      | quoteList (x :: xs) =
          Value.Pair (quoteValue x, quoteList xs)
    and quoteValue ast =
      case ast of
      | Integer (n, _) => Value.Integer n
      | Float (f, _) => Value.Float f
      | String (s, _) => Value.String s
      | Symbol (s, _) => Value.Symbol s
      | List (xs, _) => quoteList xs
  end
end
