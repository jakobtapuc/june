signature JUNE_VALUE =
sig
  exception Value of (string * Token.position)

  datatype v =
  | Integer of int
  | Float of real
  | String of string
  | Boolean of bool
  | Symbol of string
  | Pair of v * v
  | Nil
  | Primitive of (v list -> Token.position -> v)
  | Closure of
      {params: string list, body: Ast.ast list, env: env, pos: Token.position}
  | Undef

  and env =
    Env of (string * v ref) list

  val toString: v -> string
end
