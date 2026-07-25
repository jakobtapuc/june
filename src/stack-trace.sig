signature JUNE_STACK_TRACE =
sig
  type frame =
    { name: string option
    , def: Token.position option
    , call: Token.position option
    }

  type t = frame list

  val st: t ref

  val push: frame -> unit

  val pop: unit -> unit
end
