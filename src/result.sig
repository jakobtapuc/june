signature JUNE_RESULT =
sig
  datatype ('a, 'e) r = | Ok of 'a | Err of 'e

  val map: ('a -> 'b) -> ('a, 'e) r -> ('b, 'e) r

  val fold: ('a -> 'b -> ('a, 'e) r) -> 'a -> 'b list -> ('a, 'e) r

  val bind: ('a, 'e) r -> ('a -> ('b, 'e) r) -> ('b, 'e) r
end
