signature JUNE_AST =
sig
  datatype ast =
  | Integer of IntInf.int * Token.position
  | Float of real * Token.position
  | String of string * Token.position
  | Symbol of string * Token.position
  | List of ast list * Token.position

  val toString: ast -> string
end
