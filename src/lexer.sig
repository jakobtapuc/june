signature JUNE_LEXER = sig
  exception Lexer of string * Token.position

  val lex : string -> Token.t list

  val toString : Token.t list -> string
end