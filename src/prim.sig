signature JUNE_PRIM =
sig
  type v = Value.v

  type pos = Token.position

  type prim_func = v list -> pos -> v

  val add: prim_func
  val sub: prim_func
  val mult: prim_func
  val div': prim_func

  val eq: prim_func
  val not': prim_func

  val show: prim_func
  val showLn: prim_func

  val list': prim_func
  val cons: prim_func
  val car: prim_func
  val cdr: prim_func
(* val read : t *)
end
