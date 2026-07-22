structure Value :> JUNE_VALUE = struct
  exception Value of (string * Token.position)

  datatype v =
    | Integer of int
    | Boolean of bool
    | Primitive of (v list -> Token.position -> v)
    | Closure of
        { params : string list
        , body : Ast.ast list
        , env : env
        , pos : Token.position }
    | Quoted of v
    | Undef
  
  and env = Env of (string * v) list

  fun toString (Integer n) = Int.toString n
    | toString (Boolean true) = "#t"
    | toString (Boolean false) = "#f"
    | toString (Primitive _) = "Prim.Primitive"
    | toString (Closure _) = "<closure>"
    | toString (Quoted _) = "Prim.Quoted"
    | toString Undef = "#undef"
end
