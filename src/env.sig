signature JUNE_ENV =
sig
  val initialEnv: Value.env

  val lookup: Value.env -> (string * Token.position) -> Value.v

  val insert: Value.env -> string -> Value.v ref -> Value.env
end
