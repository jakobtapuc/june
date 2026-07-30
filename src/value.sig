signature JUNE_VALUE =
sig
  datatype error =
  | Type of {f: string, actual: string, expected: string}
  | Arity of {f: string, actual: int, expected: int}

  datatype v =
  | VInteger of IntInf.int
  | VFloat of real
  | VString of string
  | VBoolean of bool
  | VSymbol of string
  | VPair of v * v
  | VNil
  | VPrimitive of (v list -> (v, error) Result.r)
  | VClosure of
      { objectId: ObjectId.t
      , name: string option ref
      , params: string list
      , body: Ast.ast list
      , env: env
      }
  | VPromise of
      {objectId: ObjectId.t, body: Ast.ast, env: env, value: v option ref}
  | VUnit
  | VType of string

  and env =
    Env of {bindings: (string, v ref) HashTable.hash_table, parent: env option}

  val toString: v -> string

  val typeOf: v -> v

  val stringOf: v -> string
end
