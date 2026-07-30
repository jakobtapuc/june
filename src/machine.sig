signature JUNE_MACHINE =
sig
  datatype control =
  | Expr of {expr: Ast.ast, isTail: bool}
  | Val of Value.v
  | Apply of {f: Value.v, args: Value.v list, pos: Token.position, isTail: bool}

  datatype kont =
  | Done
  | IfK of
      { onTrue: Ast.ast
      , onFalse: Ast.ast
      , env: Value.env
      , isTail: bool
      , next: kont
      }
  | DefineK of {name: string, pos: Token.position, env: Value.env, next: kont}
  | ApplyFunK of
      { args: Ast.ast list
      , pos: Token.position
      , env: Value.env
      , isTail: bool
      , next: kont
      }
  | ApplyArgsK of
      { f: Value.v
      , pos: Token.position
      , evaledArgs: Value.v list
      , restArgs: Ast.ast list
      , env: Value.env
      , isTail: bool
      , next: kont
      }
  | SeqK of {exprs: Ast.ast list, env: Value.env, next: kont}
  | ForceK of {next: kont, pos: Token.position}
  | MemoizeK of {cell: Value.v option ref, next: kont}
  | SetK of {name: string, env: Value.env, pos: Token.position, next: kont}

  datatype frame =
  | CallFrame of {proc: Value.v option, call: Token.position}
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

  val step: state -> state

  val run: state -> ((Value.v * Value.env), (runtime_error * trace)) Result.r
end
