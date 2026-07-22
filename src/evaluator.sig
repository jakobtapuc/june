signature JUNE_EVALUATOR =
sig
  val initialEnv: Value.env

  val apply: Value.v -> Value.v list -> Token.position -> Value.v

  val evaluate: Value.env -> Ast.ast -> (Value.v * Value.env)
end
