signature JUNE_AST = sig
  datatype ast =
    | Integer of int * Token.position
    | Symbol of string * Token.position
    | List of ast list * Token.position

  val toString : ast -> string
end