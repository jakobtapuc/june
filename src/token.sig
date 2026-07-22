signature JUNE_TOKEN =
sig
  type position = {line: int, column: int}

  datatype token =
  | LParen
  | RParen
  | Quote
  | Integer of int
  | Float of real
  | Boolean of bool
  | Symbol of string
  | String of string
  | Eof

  type t = {token: token, pos: position}

  val toString: t -> string
end
