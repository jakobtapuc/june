signature JUNE_EXPANDER =
sig
  type t = Ast.ast -> Ast.ast

  val expandLet: t

  val expandAnd: t

  val expandOr: t
end
