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
  | Unit

  and env =
    Env of {bindings: (string, v ref) HashTable.hash_table, parent: env option}

  val toString: v -> string
end
