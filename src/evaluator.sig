signature JUNE_EVALUATOR = sig
  include JUNE_VALUE

  val initialEnv : env

  val apply : t -> t list -> Token.position -> t

  val evaluate : env -> Ast.t -> (t * env)
end
