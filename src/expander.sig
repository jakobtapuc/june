signature JUNE_EXPANDER =
sig
  type t = Ast.ast -> Ast.ast

  val expand: t
end
