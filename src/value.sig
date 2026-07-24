signature JUNE_VALUE =
sig
  exception Value of (string * Token.position)

  datatype v =
  | VInteger of int
  | VFloat of real
  | VString of string
  | VBoolean of bool
  | VSymbol of string
  | VPair of v * v
  | VNil
  | VPrimitive of (v list -> Token.position -> v)
  | VClosure of
      {params: string list, body: Ast.ast list, env: env, pos: Token.position}
  | VUnit

  and env =
    Env of {bindings: (string, v ref) HashTable.hash_table, parent: env option}

  val toString: v -> string
end
