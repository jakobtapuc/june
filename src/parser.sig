signature JUNE_PARSER = sig
  exception Parser of string * Token.position

  type 'a t

  val pure : 'a -> 'a t

  val fail : 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val orElse : 'a t -> 'a t -> 'a t

  val satisfy : (Token.t -> bool) -> Token.t t

  val run : 'a t -> Token.t list -> ('a * Token.t list) option

  val top : Token.t list -> (Ast.t list * Token.position list) option
end
