signature JUNE_EVALUATOR =
sig
  exception Evaluator of
    {message: string, pos: Token.position, trace: StackTrace.t}

  val evaluate: Value.env -> Ast.ast -> (Value.v * Value.env)
end
