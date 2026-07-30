structure Machine :> JUNE_MACHINE =
struct
  open Ast Value Env

  datatype control =
  | Expr of {expr: ast, isTail: bool}
  | Val of v
  | Apply of {f: v, args: v list, pos: Token.position, isTail: bool}

  datatype kont =
  | Done
  | IfK of {onTrue: ast, onFalse: ast, env: env, isTail: bool, next: kont}
  | DefineK of {name: string, pos: Token.position, env: env, next: kont}
  | ApplyFunK of
      {args: ast list, pos: Token.position, env: env, isTail: bool, next: kont}
  | ApplyArgsK of
      { f: v
      , pos: Token.position
      , evaledArgs: v list
      , restArgs: ast list
      , env: env
      , isTail: bool
      , next: kont
      }
  | SeqK of {exprs: ast list, env: env, next: kont}
  | ForceK of {next: kont, pos: Token.position}
  | MemoizeK of {cell: v option ref, next: kont}
  | SetK of {name: string, env: env, pos: Token.position, next: kont}

  datatype frame =
  | CallFrame of {proc: v option, call: Token.position}
  | DefineFrame of {name: string, pos: Token.position}

  type trace = frame list

  datatype runtime_error =
  | TypeError of
      {proc: string, pos: Token.position, expected: string, actual: string}
  | ArityError of
      {proc: string option, pos: Token.position, expected: int, actual: int}
  | GenericError of string

  datatype state =
  | Running of {control: control, env: Value.env, kont: kont, trace: trace}
  | Failed of {error: runtime_error, trace: trace}

  exception Machine of runtime_error * trace

  val continueWith = Running

  fun fail error trace = Failed {error, trace}

  (* fun withTrace kont =
    case kont of
    | Done => []
    | IfK {next, ...} => withTrace next
    | SeqK {next, ...} => withTrace next
    | MemoizeK {next, ...} => withTrace next
    | SetK {next, ...} => withTrace next
    | ForceK {next, ...} => withTrace next
    | ApplyArgsK {next, ...} => withTrace next
    | DefineK {name, pos, next, ...} =>
        DefineFrame {name, pos} :: withTrace next
    | ApplyFunK {pos, next, ...} =>
        CallFrame {proc = NONE, call = pos} :: withTrace next *)

  fun catchEnv trace thunk =
    (thunk ())
    handle Unbound name =>
      fail (GenericError ("Unbound variable: " ^ name)) trace

  fun bindAll _ [] [] = ()
    | bindAll env (p :: ps) (a :: as') =
        (insert env p a; bindAll env ps as')
    | bindAll _ _ _ = ()

  fun applyClosure (c as VClosure {body, env = closureEnv, params, name, ...})
        args pos isTail kont trace =
        if length params <> length args then
          fail
            (ArityError
               { proc = !name
               , expected = length params
               , pos
               , actual = length args
               }) trace
        else
          let
            val env' = extend closureEnv

            val () = bindAll env' params args

            val trace' =
              if isTail then
                case trace of
                | CallFrame _ :: rest => rest
                | _ => trace
              else
                trace

            val trace'' = CallFrame {proc = SOME c, call = pos} :: trace'
          in
            case body of
            | [] => fail (GenericError "Empty lambda body") trace
            | [expr] =>
                Running
                  { control = Expr {expr, isTail = true}
                  , env = env'
                  , kont
                  , trace = trace''
                  }
            | expr :: exprs =>
                Running
                  { control = Expr {expr, isTail = false}
                  , env = env'
                  , kont = SeqK {exprs, env = env', next = kont}
                  , trace = trace''
                  }
          end
    | applyClosure _ _ _ _ _ trace =
        fail (GenericError "Trying to apply a non-closure value") trace

  fun apply f args pos isTail env kont trace =
    case f of
      VPrimitive f' =>
        (case f' args of
         | Result.Ok x => Running {control = Val x, env, kont, trace}
         | Result.Err (Value.Type {f = f'', actual, expected}) =>
             fail (TypeError {proc = f'', pos, actual, expected}) trace
         | Result.Err (Value.Arity {f = f'', actual, expected}) =>
             fail (ArityError {proc = SOME f'', pos, actual, expected}) trace)
    | VClosure c => applyClosure (VClosure c) args pos isTail kont trace
    | _ => fail (GenericError "Trying to apply a non-procedure value") trace

  fun evalFirstArg f pos [] env isTail kont trace =
        Running {control = Apply {f, args = [], pos, isTail}, env, kont, trace}
    | evalFirstArg f pos (arg :: args) env isTail kont trace =
        Running
          { control = Expr {expr = arg, isTail = false}
          , env
          , kont = ApplyArgsK
              { f
              , pos
              , evaledArgs = []
              , restArgs = args
              , env
              , isTail
              , next = kont
              }
          , trace
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
    | Running {control = Expr {expr = (Integer (n, _)), ...}, env, kont, trace} =>
        continueWith {control = Val <| VInteger n, env, kont, trace}
    | Running {control = Expr {expr = (Float (n, _)), ...}, env, kont, trace} =>
        continueWith {control = Val <| VFloat n, env, kont, trace}
    | Running {control = Expr {expr = (String (s, _)), ...}, env, kont, trace} =>
        continueWith {control = Val <| VString s, env, kont, trace}
    | Running
        {control = Expr {expr = (Symbol ("#t", _)), ...}, env, kont, trace} =>
        continueWith {control = Val (VBoolean true), env, kont, trace}
    | Running
        {control = Expr {expr = (Symbol ("#f", _)), ...}, env, kont, trace} =>
        continueWith {control = Val (VBoolean false), env, kont, trace}
    | Running
        {control = Expr {expr = (Symbol ("#unit", _)), ...}, env, kont, trace} =>
        continueWith {control = Val VUnit, env, kont, trace}
    | Running {control = Expr {expr = (Symbol (s, pos)), ...}, env, kont, trace} =>
        catchEnv trace (fn () =>
          continueWith {control = Val (lookup env (s, pos)), env, kont, trace})
    | Running {control = Expr {expr = (List ([], _)), ...}, trace, ...} =>
        fail (GenericError "Cannot evaluate an empty expression") trace
    | Running
        { control =
            Expr {expr = (List (Symbol ("quote", _) :: [quoted], _)), ...}
        , env
        , kont
        , trace
        } => continueWith {control = Val (quoteValue quoted), env, kont, trace}
    | Running
        { control =
            Expr
              { expr = (List (Symbol ("fn", _) :: List (params, _) :: body, _))
              , ...
              }
        , env
        , kont
        , trace
        } =>
        let
          fun paramNames [] = SOME []
            | paramNames ((Symbol (p, _)) :: xs) =
                Option.map (fn ps => p :: ps) (paramNames xs)
            | paramNames _ = NONE
        in
          if null body then
            fail (GenericError "Lambda needs a body") trace
          else
            case paramNames params of
            | SOME names =>
                continueWith
                  { control = Val (VClosure
                      { objectId = ObjectId.newId ()
                      , name = ref NONE
                      , params = names
                      , body
                      , env
                      })
                  , env
                  , kont
                  , trace
                  }
            | NONE => fail (GenericError "Param name has to be a symbol") trace
        end
    | Running
        { control =
            Expr {expr = (List (Symbol ("if", _) :: rest, _)), isTail = false}
        , env
        , kont
        , trace
        } =>
        (case rest of
         | [cond, onTrue, onFalse] =>
             continueWith
               { control = Expr {expr = cond, isTail = false}
               , env
               , kont = IfK {onTrue, onFalse, env, isTail = false, next = kont}
               , trace
               }
         | _ => fail (GenericError "Malformed if") trace)
    | Running
        { control =
            Expr {expr = (List (Symbol ("if", _) :: rest, _)), isTail = true}
        , env
        , kont
        , trace
        } =>
        (case rest of
         | [cond, onTrue, onFalse] =>
             continueWith
               { control = Expr {expr = cond, isTail = true}
               , env
               , kont = IfK {onTrue, onFalse, env, isTail = true, next = kont}
               , trace
               }
         | _ => fail (GenericError "Malformed if") trace)
    | Running
        { control = Val (VBoolean true)
        , kont = IfK {onTrue, env = ifEnv, isTail, next, ...}
        , trace
        , ...
        } =>
        Running
          { control = Expr {expr = onTrue, isTail}
          , env = ifEnv
          , kont = next
          , trace
          }

    | Running
        { control = Val (VBoolean false)
        , kont = IfK {onFalse, env = ifEnv, isTail, next, ...}
        , trace
        , ...
        } =>
        Running
          { control = Expr {expr = onFalse, isTail}
          , env = ifEnv
          , kont = next
          , trace
          }
    | Running
        { control = Expr {expr = (List (Symbol ("begin", _) :: exprs, _)), ...}
        , env
        , kont
        , trace
        } =>
        (case exprs of
         | [] => fail (GenericError "Empty `begin` body") trace
         | [expr] =>
             Running {control = Expr {expr, isTail = true}, env, kont, trace}
         | expr :: rest =>
             Running
               { control = Expr {expr, isTail = false}
               , env
               , kont = SeqK {exprs = rest, env, next = kont}
               , trace
               })
    | Running
        { control = Expr {expr = (List (f :: args, pos)), isTail = true}
        , env
        , kont
        , trace
        } =>
        Running
          { control = Expr {expr = f, isTail = true}
          , env
          , kont = ApplyFunK {args, pos, env, isTail = true, next = kont}
          , trace
          }
    | Running
        {control = Val value, env, kont = SeqK {exprs = [], next, ...}, trace} =>
        Running {control = Val value, env, kont = next, trace}
    | Running
        { control = Val _
        , kont = SeqK {exprs = [expr], env = seqEnv, next}
        , trace
        , ...
        } =>
        Running
          { control = Expr {expr, isTail = true}
          , env = seqEnv
          , kont = next
          , trace
          }
    | Running
        { control = Val _
        , kont = SeqK {exprs = nextExpr :: rest, env = seqEnv, next}
        , trace
        , ...
        } =>
        Running
          { control = Expr {expr = nextExpr, isTail = false}
          , env = seqEnv
          , kont = SeqK {exprs = rest, env = seqEnv, next = next}
          , trace
          }
    | Running
        { control = Expr {expr = (List (Symbol ("lazy", _) :: [thunk], _)), ...}
        , env
        , kont
        , trace
        } =>
        Running
          { control =
              Val
              <|
              VPromise
                { objectId = ObjectId.newId ()
                , body = thunk
                , env
                , value = ref NONE
                }
          , env
          , kont
          , trace
          }
    | Running
        { control =
            Expr {expr = (List (Symbol ("force", pos) :: [expr], _)), ...}
        , env
        , kont
        , trace
        } =>
        Running
          { control = Expr {expr, isTail = false}
          , env
          , kont = ForceK {next = kont, pos}
          , trace
          }
    | Running {control = Val value, env, kont = ForceK {next, pos}, trace} =>
        (case value of
         | VPromise {body, env = promiseEnv, value = cell, ...} =>
             (case !cell of
              | SOME result =>
                  Running
                    { control = Val result
                    , env
                    , kont = ForceK {next, pos}
                    , trace
                    }
              | NONE =>
                  Running
                    { control = Expr {expr = body, isTail = false}
                    , env = promiseEnv
                    , kont = MemoizeK {cell, next}
                    , trace
                    })
         | _ => fail (GenericError "Cannot force a non-promise value") trace)
    | Running
        { control = Expr {expr = (List (Symbol ("def", pos) :: rest, _)), ...}
        , env
        , kont
        , trace
        } =>
        (case rest of
         | [Symbol (name, _), expr'] =>
             let
               val () = insert env name VUnit
             in
               continueWith
                 { control = Expr {expr = expr', isTail = false}
                 , env
                 , kont = DefineK {name, pos, env, next = kont}
                 , trace
                 }
             end
         | _ => fail (GenericError "Malformed `def`") trace)
    | Running
        { control = Val value
        , kont = DefineK {name, pos, env = defineEnv, next}
        , trace
        , ...
        } =>
        catchEnv trace (fn () =>
          let
            val () =
              case value of
              | VClosure {name = name', ...} => name' := SOME name
              | _ => ()
            val () = set defineEnv (name, pos) value
            val trace' = DefineFrame {name, pos} :: trace
          in
            continueWith
              { control = Val VUnit
              , env = defineEnv
              , kont = next
              , trace = trace'
              }
          end)
    | Running
        { control =
            Expr
              {expr = (List (Symbol ("set!", pos) :: rest, _)), isTail = false}
        , env
        , kont
        , trace
        } =>
        (case rest of
         | [Symbol (name, _), expr] =>
             Running
               { control = Expr {expr, isTail = false}
               , env
               , kont = SetK {name, env, pos, next = kont}
               , trace
               }
         | _ => fail (GenericError "Malformed `set!`") trace)
    | Running
        { control = Val value
        , env
        , kont = SetK {name, env = setEnv, next, pos}
        , trace
        } =>
        catchEnv trace (fn () =>
          let val () = set setEnv (name, pos) value
          in Running {control = Val VUnit, env, kont = next, trace}
          end)
    | Running
        { control = Expr {expr = (List (f :: args, pos)), isTail = false}
        , env
        , kont
        , trace
        } =>
        Running
          { control = Expr {expr = f, isTail = false}
          , env
          , kont = ApplyFunK {args, pos, env, isTail = false, next = kont}
          , trace
          }
    | Running
        { control = Val f
        , kont = ApplyFunK {args, pos, env = callEnv, isTail, next, ...}
        , trace
        , ...
        } => evalFirstArg f pos args callEnv isTail next trace
    | Running {control = Apply {f, args, pos, isTail}, env, kont, trace} =>
        apply f args pos isTail env kont trace
    | Running
        { control = Val arg
        , kont =
            ApplyArgsK
              { f
              , pos
              , evaledArgs
              , restArgs = nextArg :: rest
              , env
              , isTail
              , next
              }
        , trace
        , ...
        } =>
        Running
          { control = Expr {expr = nextArg, isTail}
          , env = env
          , kont = ApplyArgsK
              { f
              , pos
              , evaledArgs = arg :: evaledArgs
              , restArgs = rest
              , env
              , isTail
              , next
              }
          , trace
          }
    | Running
        { control = Val arg
        , kont =
            ApplyArgsK {f, evaledArgs, restArgs = [], env, isTail, next, pos}
        , trace
        , ...
        } => apply f (List.rev (arg :: evaledArgs)) pos isTail env next trace
    | _ => fail (GenericError "Unimplemented") []

  fun kontDepth Done = 0
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
    | showControl (Apply _) = "Apply"

  fun run state =
    let
      fun loop state' =
        case state' of
        | Running {control = Val v, env, kont = Done, ...} => Result.Ok (v, env)
        | Failed {error, trace} => Result.Err (error, trace)

        | _ =>
            (* let *)
            (* val depth = kontDepth kont *)
            (* val () = print
              ("step=" ^ Int.toString (!steps) ^ " | kont depth="
               ^ Int.toString depth ^ " | control=" ^ showControl control
               ^ "\n")
            val () = steps := !steps + 1 *)
            (* in *)
            loop (step state')
    (* end *)
    (* | _ => loop (step state') *)
    in
      loop state
    end
end
