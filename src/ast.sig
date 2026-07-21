signature JUNE_AST = sig
  datatype t =
    | Integer of int * Token.position
    | Symbol of string * Token.position
    | List of t list * Token.position

  val toString : t -> string
end