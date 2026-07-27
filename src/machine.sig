signature JUNE_MACHINE =
sig
  datatype control =
  | Expr of Ast.ast
  | Val of Value.v
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
  | SeqK of {exprs: Ast.ast list, env: Value.env, next: kont}
  | ForceK of {next: kont, pos: Token.position}
  | MemoizeK of {cell: Value.v option ref, next: kont}
  | SetK of {name: string, env: Value.env, pos: Token.position, next: kont}

  datatype state =
  | Running of {control: control, env: Value.env, kont: kont}
  | Failed of string list

  val step: state -> state

  val run: state -> (Value.v * Value.env)
end
