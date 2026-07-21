signature JUNE_VALUE = sig
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

  val toString : t -> string
end
