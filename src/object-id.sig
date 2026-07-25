signature JUNE_OBJECT_ID =
sig
  type t

  val newId: unit -> t

  val toString: t -> string
end
