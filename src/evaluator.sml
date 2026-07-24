structure Evaluator :> JUNE_EVALUATOR =
struct
  local open Ast Env Value
  in
    fun evaluateArgs (Env env) [] = ([], Env env)
      | evaluateArgs (Env env) (expr :: rest) =
          let
            val (value, (Env env')) = evaluate (Env env) expr
            val (values, env'') = evaluateArgs (Env env') rest
          in
            (value :: values, env'')
          end

    and apply fnValue args pos =
      case fnValue of
      | VPrimitive f => f args pos
      | VClosure {params, body, env, pos = pos'} =>
          let
            fun zip [] [] = []
              | zip (x :: xs) (y :: ys) =
                  (x, y) :: zip xs ys
              | zip _ _ =
                  raise Value ("Invalid number of arguments", pos')

            val bound = zip params args

            val env' = extend env

            val () = List.app (fn (p, a) => insert env' p a) bound

            val (result, _) = evaluateSeq pos' env' body
          in
            result
          end
      | _ => raise Value ("Attempt to call a non-function value", pos)

    and evaluate (Env env) expr =
      case expr of
      | Integer (n, _) => (VInteger n, (Env env))
      | Float (f, _) => (VFloat f, (Env env))
      | String (s, _) => (VString s, (Env env))
      | Symbol ("#t", _) => (VBoolean true, (Env env))
      | Symbol ("#f", _) => (VBoolean false, (Env env))
      | Symbol ("#unit", _) => (VUnit, (Env env))
      | Symbol (s, pos) => (lookup (Env env) (s, pos), (Env env))
      | List ([], pos) =>
          raise Value ("Cannot evaluate an empty list", pos)
      | List (Symbol ("quote", _) :: [quoted], _) =>
          (quoteValue quoted, Env env)
      | List (Symbol ("lambda", pos) :: List (params, _) :: body, _) =>
          let
            fun paramName (Symbol (p, _)) = p
              | paramName _ =
                  raise Value ("Param name has to be a symbol", pos)

            val paramNames = List.map paramName params
          in
            if null body then
              raise Value ("Lambda needs a body", pos)
            else
              ( VClosure
                  { params = paramNames
                  , body = body
                  , env = Env env
                  , pos = pos
                  }
              , Env env
              )
          end
      | List (Symbol ("if", pos) :: rest, _) =>
          (case rest of
           | [cond, onTrue, onFalse] =>
               let
                 val (condValue, (Env env')) =
                   evaluate (Env env) cond
               in
                 (case condValue of
                  | VBoolean true =>
                      let
                        val (trueValue, (Env env'')) =
                          evaluate (Env env') onTrue
                      in
                        (trueValue, (Env env''))
                      end
                  | VBoolean false =>
                      let
                        val (falseValue, (Env env'')) =
                          evaluate (Env env') onFalse
                      in
                        (falseValue, (Env env''))
                      end
                  | _ =>
                      let
                        val (trueValue, (Env env'')) =
                          evaluate (Env env') onTrue
                      in
                        (trueValue, (Env env''))
                      end)
               end
           | _ => raise Value ("Malformed `if`", pos))
      | List (Symbol ("define", pos) :: rest, _) =>
          (case rest of
           | [Symbol (name, _), expr'] =>
               let
                 val () = insert (Env env) name VUnit
                 val (value, _) = evaluate (Env env) expr'
                 val () = set (Env env) (name, pos) value
               in
                 (VUnit, (Env env))
               end
           | _ => raise Value ("Malformed `define`", pos))
      | List (Symbol ("set!", pos) :: rest, _) =>
          (case rest of
           | [Symbol (name, _), expr'] =>
               let
                 val _ = lookup (Env env) (name, pos)
                 val (value, _) = evaluate (Env env) expr'
                 val () = set (Env env) (name, pos) value
               in
                 (VUnit, (Env env))
               end
           | _ => raise Value ("Malformed `set!`", pos))
      | List (f :: args, pos) =>
          let
            val (fnValue, (Env env')) = evaluate (Env env) f
            val (argValues, env'') = evaluateArgs (Env env') args
            val applied = apply fnValue argValues pos
          in
            (applied, env'')
          end


    (* 
    expected `{ column : int, line : int } -> Env -> Ast.ast list -> Value.v * Env`
       found `{ column : int, line : int } -> Env -> Value.v list -> Value.v * Env`
    
     *)

    and evaluateSeq _ env [expr] = evaluate env expr
      | evaluateSeq pos env (expr :: rest) =
          let val (_, env') = evaluate env expr
          in evaluateSeq pos env' rest
          end
      | evaluateSeq pos _ [] =
          raise Value ("Empty body", pos)
    and quoteList [] = VNil
      | quoteList (x :: xs) =
          VPair (quoteValue x, quoteList xs)
    and quoteValue ast =
      case ast of
      | Integer (n, _) => VInteger n
      | Float (f, _) => VFloat f
      | String (s, _) => VString s
      | Symbol (s, _) => VSymbol s
      | List (xs, _) => quoteList xs
  end
end
