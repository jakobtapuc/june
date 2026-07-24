signature JUNE_ENV =
sig
  val empty: unit -> Value.env

  val extend: Value.env -> Value.env

  val lookup: Value.env -> (string * Token.position) -> Value.v

  val insert: Value.env -> string -> Value.v -> unit

  val set: Value.env -> string -> Value.v -> unit

  val prim: Value.env
end
