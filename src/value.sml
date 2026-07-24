structure Value :> JUNE_VALUE =
struct
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

  fun toString (VInteger n) =
        if n < 0 then "-" ^ (Int.toString <| Int.abs n) else Int.toString n
    | toString (VFloat f) = Real.toString f
    | toString (VString s) = "\"" ^ s ^ "\""
    | toString (VBoolean true) = "#t"
    | toString (VBoolean false) = "#f"
    | toString (VSymbol s) = s
    | toString (VPair (x, y)) = stringifyPair x y
    | toString VNil = "#nil"
    | toString (VPrimitive _) = "<primitive>"
    | toString (VClosure _) = "<closure>"
    | toString VUnit = "#unit"
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
end
