structure Value :> JUNE_VALUE =
struct
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
  | VInteger of IntInf.int
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

  fun failTypeWithTrace f actual expected pos =
    raise Type {f, actual, expected, pos, trace = ! StackTrace.st}

  fun failArgWithTrace f actual expected pos =
    raise Argument {f, actual, expected, pos, trace = ! StackTrace.st}

  fun toString (VInteger n) =
        if n < 0 then "-" ^ (IntInf.toString <| IntInf.abs n)
        else IntInf.toString n
    | toString (VFloat f) = Real.toString f
    | toString (VString s) = "\"" ^ s ^ "\""
    | toString (VBoolean true) = "#t"
    | toString (VBoolean false) = "#f"
    | toString (VSymbol s) = s
    | toString (VPair (x, y)) = stringifyPair x y
    | toString VNil = "#nil"
    | toString (VPrimitive _) = "<primitive>"
    | toString (VClosure {objectId, ...}) =
        "<closure:" ^ ObjectId.toString objectId ^ ">"
    | toString (VPromise {objectId, ...}) =
        "<promise:" ^ ObjectId.toString objectId ^ ">"
    | toString VUnit = "#unit"
    | toString (VType t) = "<type:" ^ t ^ ">"
  and stringifyPair x y =
    let
      fun loop (VPair (h, t)) acc =
            loop t (toString h :: acc)
        | loop VNil acc =
            "(" ^ String.concatWith " " (List.rev acc) ^ ")"
        | loop tail acc =
            "(" ^ String.concatWith " " (List.rev acc) ^ " . " ^ toString tail
            ^ ")"
    in
      loop (VPair (x, y)) []
    end

  fun typeOf (VInteger _) = VType "int"
    | typeOf (VFloat _) = VType "float"
    | typeOf (VString _) = VType "str"
    | typeOf (VBoolean _) = VType "bool"
    | typeOf (VSymbol _) = VType "sym"
    | typeOf (VPair _) = VType "pair"
    | typeOf VNil = VType "nil"
    | typeOf (VPrimitive _) = VType "prim"
    | typeOf (VClosure _) = VType "closure"
    | typeOf (VPromise _) = VType "promise"
    | typeOf VUnit = VType "unit"
    | typeOf (VType _) = VType "type"

  fun stringOf (VType t) = t
    | stringOf v =
        stringOf (typeOf v)
end
