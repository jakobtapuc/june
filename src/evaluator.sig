signature JUNE_EVALUATOR =
sig
  val evaluate: Value.env -> Ast.ast -> (Value.v * Value.env)
end
