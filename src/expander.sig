signature JUNE_EXPANDER =
sig
  exception Expander of {msg: string, pos: Token.position, trace: StackTrace.t}

  type t = Ast.ast -> Ast.ast

  val expand: t
end
