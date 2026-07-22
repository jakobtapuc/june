structure Value :> JUNE_VALUE =
struct
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

  fun toString (Integer n) = Int.toString n
    | toString (Float f) = Real.toString f
    | toString (String s) = "\"" ^ s ^ "\""
    | toString (Boolean true) = "#t"
    | toString (Boolean false) = "#f"
    | toString (Symbol s) = s
    | toString (Pair (x, y)) = stringifyPair x y
    | toString Nil = "#nil"
    | toString (Primitive _) = "<primitive>"
    | toString (Closure _) = "<closure>"
    | toString Undef = "#undef"
  and stringifyPair x y =
    let
      fun loop (Pair (h, t)) acc =
            loop t (toString h :: acc)
        | loop Nil acc =
            "(" ^ String.concatWith " " (List.rev acc) ^ ")"
        | loop tail acc =
            "(" ^ String.concatWith " " (List.rev acc) ^ " . " ^ toString tail
            ^ ")"
    in
      loop (Pair (x, y)) []
    end
end
