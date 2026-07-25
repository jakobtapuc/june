structure StackTrace :> JUNE_STACK_TRACE =
struct
  type frame =
    { name: string option
    , def: Token.position option
    , call: Token.position option
    }

  type t = frame list

  val st = ref ([] : frame list)

  fun push frame =
    st := frame :: !st

  fun pop () =
    st := tl (!st)
end
