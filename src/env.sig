signature JUNE_ENV =
sig
  exception Unbound of {name: string, pos: Token.position, trace: StackTrace.t}

  val empty: unit -> Value.env

  val extend: Value.env -> Value.env

  val lookup: Value.env -> (string * Token.position) -> Value.v

  val insert: Value.env -> string -> Value.v -> unit

  val set: Value.env -> (string * Token.position) -> Value.v -> unit

  val prim: Value.env
end
