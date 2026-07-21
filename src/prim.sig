signature JUNE_PRIM = sig
  include JUNE_VALUE

  type prim_func = t list -> Token.position -> t

  val add : prim_func
  val sub : prim_func
  val mult : prim_func
  val div' : prim_func

  val and' : prim_func
  val or' : prim_func

  val show : prim_func
  val showLn : prim_func
  (* val read : t *)
end
