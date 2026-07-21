structure Evaluator :> JUNE_EVALUATOR = struct
  (* open Prim *)
  open Value

  fun apply fnValue args pos =
    case fnValue of
      | Primitive f =>
          f args pos
      | _ => raise Value ("Attempt to call a non-function value", pos)

  val initialEnv = 
    Env
      [ ("+", Primitive Prim.add)
      , ("-", Primitive Prim.sub)
      , ("*", Primitive Prim.mult)
      , ("/", Primitive Prim.div')
      , ("and", Primitive Prim.and')
      , ("or", Primitive Prim.or')
      , ("show", Primitive Prim.show)
      , ("show-ln", Primitive Prim.showLn)]

  fun lookup (Env []) (name, pos) =
    raise Value ("Unbound variable: " ^ name, pos)
    | lookup (Env ((name', value) :: rest)) (name, pos) =
      if name = name' then
        value
      else
        lookup (Env rest) (name, pos)

  fun insert (Env env) name value =
      Env ((name, value) :: env)

  fun evaluateArgs (Env env) [] =
        ([], Env env)
    | evaluateArgs (Env env) (expr :: rest) =
      let
        val (value, (Env env')) = evaluate (Env env) expr
        val (values, env'') = evaluateArgs (Env env') rest
      in
        (value :: values, env'')
      end

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