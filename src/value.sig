signature JUNE_VALUE =
sig
  exception Type of
    { f: string
    , actual: string
    , expected: string
    , pos: Token.position
    , trace: StackTrace.t
    }

  exception Argument of
    { f: string
    , actual: int
    , expected: int
    , pos: Token.position
    , trace: StackTrace.t
    }

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
      { objectId: ObjectId.t
      , params: string list
      , body: Ast.ast list
      , env: env
      , pos: Token.position
      }
  | VPromise of
      { objectId: ObjectId.t
      , body: Ast.ast
      , env: env
      , value: v option ref
      , pos: Token.position
      }
  | VUnit
  | VType of string

  and env =
    Env of {bindings: (string, v ref) HashTable.hash_table, parent: env option}

  val failTypeWithTrace: string -> string -> string -> Token.position -> 'a

  val failArgWithTrace: string -> int -> int -> Token.position -> 'a

  val toString: v -> string

  val typeOf: v -> v

  val stringOf: v -> string
end
