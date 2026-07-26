structure Machine :> JUNE_MACHINE =
struct
  open Ast Value Env

  datatype control =
  | Expr of ast
  | Val of v
  | Apply of Value.v * Value.v list * Token.position

  datatype kont =
  | Done
  | IfK of {onTrue: Ast.ast, onFalse: Ast.ast, env: Value.env, next: kont}
  | DefineK of {name: string, pos: Token.position, env: Value.env, next: kont}
  | ApplyFunK of
      {args: Ast.ast list, pos: Token.position, env: Value.env, next: kont}
  | ApplyArgsK of
      { f: Value.v
      , pos: Token.position
      , evaledArgs: Value.v list
      , restArgs: Ast.ast list
      , env: Value.env
      , next: kont
      }
  (* | ApplyOneArgK of
      { f : Value.v
      , pos : Token.position
      , next : kont
      } *)
  | SeqK of {exprs: Ast.ast list, env: Value.env, next: kont}
  | ForceK of {next: kont, pos: Token.position}
  | MemoizeK of {cell: Value.v option ref, next: kont}
  | SetK of {name: string, env: Value.env, pos: Token.position, next: kont}

  datatype state =
  | Running of {control: control, env: env, kont: kont}
  | Failed of string list

  val continueWith = Running

  val fail = Failed

  fun withTrace message kont =
    case kont of
    | Done => []
    | IfK {next, ...} => "if" :: withTrace message next
    | ApplyFunK {pos, next, ...} =>
        ("Call at: " ^ "[POS]") :: withTrace message next
    (* | ApplyLastArgK {pos, next, ...} =>
        ("Arg at: " ^ "[POS]") :: withTrace message next *)
    | ApplyArgsK {pos, next, ...} =>
        ("Arg at: " ^ "[POS]") :: withTrace message next
    | SeqK {next, ...} => "Begin" :: withTrace message next
    | DefineK {pos, next, ...} =>
        ("Define at: " ^ "[POS]") :: withTrace message next
    | SetK {pos, next, ...} => ("Set at: " ^ "[POS]") :: withTrace message next
    | ForceK {pos, next, ...} =>
        ("Force at: " ^ "[POS]") :: withTrace message next
    | MemoizeK {next, ...} => withTrace message next


  fun bindAll _ [] [] = ()
    | bindAll env (p :: ps) (a :: as') =
        (insert env p a; bindAll env ps as')
    | bindAll _ _ _ = raise Fail "Impossible" (* FIXME: should not fail here *)

  fun applyClosure {body, env = closureEnv, params, pos} args kont =
    if length params <> length args then
      fail <| withTrace "Argument count mismatch" kont
    else
      let
        val env' = extend closureEnv

        val () = bindAll env' params args
      in
        case body of
        | [] => fail <| withTrace "Empty lambda body" kont
        | [expr] => Running {control = Expr expr, env = env', kont}
        | expr :: exprs =>
            Running
              { control = Expr expr
              , env = env'
              , kont = SeqK {exprs, env = env', next = kont}
              }
      end

  fun apply f pos args env kont =
    case f of
      VPrimitive f' =>
        Running {control = Val (f' args pos), env = env, kont = kont}

    | VClosure {body, env = env', params, pos = pos', ...} =>
        applyClosure {body, env = env', params, pos = pos'} args kont

    | _ => fail <| withTrace "Trying to apply a non-procedure value" kont

  fun evalFirstArg f pos [] env kont =
        Running {control = Apply (f, [], pos), env, kont}

    | evalFirstArg f pos (arg :: args) env kont =
        Running
          { control = Expr arg
          , env
          , kont = ApplyArgsK
              {f, pos, evaledArgs = [], restArgs = args, env, next = kont}
          }

  fun quoteList [] = VNil
    | quoteList (x :: xs) =
        VPair (quoteValue x, quoteList xs)
  and quoteValue ast =
    case ast of
    | Integer (n, _) => VInteger n
    | Float (f, _) => VFloat f
    | String (s, _) => VString s
    | Symbol (s, _) => VSymbol s
    | List (xs, _) => quoteList xs

  fun step state =
    case state of
    | Running {control = Expr (Integer (n, _)), env, kont} =>
        continueWith {control = Val <| VInteger n, env, kont}
    | Running {control = Expr (Float (n, _)), env, kont} =>
        continueWith {control = Val <| VFloat n, env, kont}
    | Running {control = Expr (String (s, _)), env, kont} =>
        continueWith {control = Val <| VString s, env, kont}
    | Running {control = Expr (Symbol ("#t", _)), env, kont} =>
        continueWith {control = Val (VBoolean true), env, kont}
    | Running {control = Expr (Symbol ("#f", _)), env, kont} =>
        continueWith {control = Val (VBoolean false), env, kont}
    | Running {control = Expr (Symbol ("#unit", _)), env, kont} =>
        continueWith {control = Val VUnit, env, kont}
    | Running {control = Expr (Symbol (s, pos)), env, kont} =>
        continueWith {control = Val (lookup env (s, pos)), env, kont}
    | Running {control = Expr (List ([], _)), kont, ...} =>
        fail <| withTrace "Cannot evaluate an empty expression" kont
    | Running
        {control = Expr (List (Symbol ("quote", _) :: [quoted], _)), env, kont} =>
        continueWith {control = Val (quoteValue quoted), env, kont}
    | Running
        { control =
            Expr (List (Symbol ("lambda", pos) :: List (params, _) :: body, _))
        , env
        , kont
        } =>
        let
          fun paramNames [] = SOME []
            | paramNames ((Symbol (p, _)) :: xs) =
                Option.map (fn ps => p :: ps) (paramNames xs)
            | paramNames _ = NONE
        in
          if null body then
            fail <| withTrace "Lambda needs a body" kont
          else
            case paramNames params of
            | SOME names =>
                continueWith
                  { control = Val (VClosure
                      { objectId = ObjectId.newId ()
                      , params = names
                      , body = body
                      , env = env
                      , pos = pos
                      })
                  , env
                  , kont
                  }
            | NONE => fail <| withTrace "Param name has to be a symbol" kont
        end
    | Running {control = Expr (List (Symbol ("if", _) :: rest, _)), env, kont} =>
        (case rest of
         | [cond, onTrue, onFalse] =>
             continueWith
               { control = Expr cond
               , env
               , kont = IfK {onTrue, onFalse, env, next = kont}
               }
         | _ => fail <| withTrace "Malformed if" kont)
    | Running
        { control = Val (VBoolean true)
        , kont = IfK {onTrue, env = ifEnv, next, ...}
        , ...
        } => Running {control = Expr onTrue, env = ifEnv, kont = next}

    | Running
        { control = Val (VBoolean false)
        , kont = IfK {onFalse, env = ifEnv, next, ...}
        , ...
        } => Running {control = Expr onFalse, env = ifEnv, kont = next}
    | Running
        {control = Expr (List (Symbol ("begin", _) :: exprs, _)), env, kont} =>
        (case exprs of
         | [] => fail <| withTrace "Empty `begin` body" kont
         | [expr] => Running {control = Expr expr, env, kont}
         | expr :: rest =>
             Running
               { control = Expr expr
               , env
               , kont = SeqK {exprs = rest, env, next = kont}
               })
    | Running {control = Val value, env, kont = SeqK {exprs = [], next, ...}} =>
        Running {control = Val value, env, kont = next}
    | Running
        {control = Val _, kont = SeqK {exprs = [expr], env = seqEnv, next}, ...} =>
        Running {control = Expr expr, env = seqEnv, kont = next}
    | Running
        { control = Val _
        , kont = SeqK {exprs = nextExpr :: rest, env = seqEnv, next}
        , ...
        } =>
        Running
          { control = Expr nextExpr
          , env = seqEnv
          , kont = SeqK {exprs = rest, env = seqEnv, next = next}
          }
    | Running
        {control = Expr (List (Symbol ("lazy", pos) :: [thunk], _)), env, kont} =>
        Running
          { control =
              Val
              <|
              VPromise
                { objectId = ObjectId.newId ()
                , body = thunk
                , env
                , value = ref NONE
                , pos
                }
          , env
          , kont
          }
    | Running
        {control = Expr (List (Symbol ("force", pos) :: [expr], _)), env, kont} =>
        Running {control = Expr expr, env, kont = ForceK {next = kont, pos}}
    | Running {control = Val value, env, kont = ForceK {next, pos}} =>
        (case value of
         | VPromise {body, env = promiseEnv, value = cell, ...} =>
             (case !cell of
              | SOME result =>
                  Running {control = Val result, env, kont = ForceK {next, pos}}
              | NONE =>
                  Running
                    { control = Expr body
                    , env = promiseEnv
                    , kont = MemoizeK {cell, next}
                    })
         | _ =>
             fail
             <|
             withTrace "Cannot force a non-promise value" (ForceK {next, pos}))
    | Running
        {control = Expr (List (Symbol ("define", pos) :: rest, _)), env, kont} =>
        (case rest of
         | [Symbol (name, _), expr'] =>
             let
               val () = insert env name VUnit
             in
               continueWith
                 { control = Expr expr'
                 , env
                 , kont = DefineK {name, pos, env, next = kont}
                 }
             end
         | _ => fail <| withTrace "Malformed `define`" kont)
    | Running
        { control = Val value
        , kont = DefineK {name, pos, env = defineEnv, next}
        , ...
        } =>
        let val () = set defineEnv (name, pos) value
        in continueWith {control = Val VUnit, env = defineEnv, kont = next}
        end
    | Running
        {control = Expr (List (Symbol ("set!", pos) :: rest, _)), env, kont} =>
        (case rest of
         | [Symbol (name, _), expr] =>
             Running
               { control = Expr expr
               , env
               , kont = SetK {name, env, pos, next = kont}
               }
         | _ => fail <| withTrace "Malformed `set!`" kont)
    | Running
        {control = Val value, env, kont = SetK {name, env = setEnv, next, pos}} =>
        let val () = set setEnv (name, pos) value
        in Running {control = Val VUnit, env, kont = next}
        end
    | Running {control = Expr (List (f :: args, pos)), env, kont} =>
        Running
          { control = Expr f
          , env
          , kont = ApplyFunK {args, pos, env, next = kont}
          }
    | Running {control = Apply (f, args, pos), env, kont} =>
        apply f pos args env kont
    | Running
        { control = Val f
        , kont = ApplyFunK {args, pos, env = callEnv, next}
        , ...
        } => evalFirstArg f pos args callEnv next
    | Running
        { control = Val arg
        , kont =
            ApplyArgsK
              {f, pos, evaledArgs, restArgs = nextArg :: rest, env, next}
        , ...
        } =>
        Running
          { control = Expr nextArg
          , env = env
          , kont = ApplyArgsK
              { f = f
              , pos = pos
              , evaledArgs = arg :: evaledArgs
              , restArgs = rest
              , env = env
              , next = next
              }
          }
    | Running
        { control = Val arg
        , kont = ApplyArgsK {f, pos, evaledArgs, restArgs = [], env, next}
        , ...
        } => apply f pos (List.rev (arg :: evaledArgs)) env next
    | _ => raise Fail "Unimplemented"


  (* val steps = ref 0
  val maxKont = ref 0 *)

  (* fun kontDepth Done = 0
    | kontDepth (IfK {next, ...}) = 1 + kontDepth next
    | kontDepth (ApplyFunK {next, ...}) = 1 + kontDepth next
    | kontDepth (ApplyArgsK {next, ...}) = 1 + kontDepth next
    | kontDepth (SeqK {next, ...}) = 1 + kontDepth next
    | kontDepth (DefineK {next, ...}) = 1 + kontDepth next
    | kontDepth (SetK {next, ...}) = 1 + kontDepth next
    | kontDepth (ForceK {next, ...}) = 1 + kontDepth next
    | kontDepth (MemoizeK {next, ...}) = 1 + kontDepth next

  fun showControl (Expr _) = "Expr"
    | showControl (Val _) = "Val"
    | showControl (Apply _) = "Apply" *)

  fun run state =
    let
      fun loop state' =
        case state' of
        | Running {control = Val v, env, kont = Done} => (v, env)
        | Failed e => raise Fail (String.concatWith "\n" e)
        | _ => loop (step state')

        (* | Running {control, kont, ...} =>
            let
              val depth = kontDepth kont
              val () = print
                ("step=" ^ Int.toString (!steps) ^ " | kont depth="
                 ^ Int.toString depth ^ " | control=" ^ showControl control
                 ^ "\n")
              val () = steps := !steps + 1
            in
              loop (step state')
            end *)
    in
      loop state
    end
end
