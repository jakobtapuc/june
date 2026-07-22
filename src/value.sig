signature JUNE_VALUE = sig
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

  val toString : v -> string
end
