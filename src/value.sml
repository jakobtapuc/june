structure Value :> JUNE_VALUE = struct
  exception Value of (string * Token.position)

  datatype t =
    | Integer of int
    | Boolean of bool
    | Primitive of (t list -> Token.position -> t)
    | Closure of
        { params : string list
        , body : Ast.t
        , env : env }
    | Quoted of t
    | Undef
  
  and env = Env of (string * t) list

  fun toString (Integer n) = Int.toString n
    | toString (Boolean true) = "#t"
    | toString (Boolean false) = "#f"
    | toString (Primitive _) = "Prim.Primitive"
    | toString (Closure _) = "<closure>"
    | toString (Quoted _) = "Prim.Quoted"
    | toString Undef = "#undef"
end
