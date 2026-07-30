structure Env :> JUNE_ENV =
struct
  exception Unbound of string

  open Value

  fun empty () =
    Env
      { bindings =
          HashTable.mkTable (MLton.hash, op=) (16, Fail "Missing binding")
      , parent = NONE
      }

  fun extend (Env env) =
    Env
      { bindings =
          HashTable.mkTable (MLton.hash, op=) (16, Fail "Missing binding")
      , parent = SOME <| Env env
      }

  fun lookup (Env {bindings, parent}) (name, pos) =
    case HashTable.find bindings name of
    | SOME cell => !cell
    | NONE =>
        case parent of
        | SOME parent' => lookup parent' (name, pos)
        | NONE => raise Unbound name

  fun insert (Env {bindings, ...}) name value =
    HashTable.insert bindings (name, ref value)

  fun set (Env {bindings, parent}) (name, pos) value =
    case HashTable.find bindings name of
    | SOME cell => cell := value
    | NONE =>
        case parent of
        | SOME parent' => set parent' (name, pos) value
        | NONE => raise Unbound name

  val prim =
    let
      val prim' = empty ()

      val () = insert prim' "+" <| VPrimitive Prim.add
      val () = insert prim' "-" <| VPrimitive Prim.sub
      val () = insert prim' "*" <| VPrimitive Prim.mult
      val () = insert prim' "/" <| VPrimitive Prim.div'
      val () = insert prim' "eq?" <| VPrimitive Prim.eq
      val () = insert prim' "not" <| VPrimitive Prim.not'
      val () = insert prim' "show" <| VPrimitive Prim.show
      val () = insert prim' "show-ln" <| VPrimitive Prim.showLn
      val () = insert prim' "list" <| VPrimitive Prim.list'
      val () = insert prim' "cons" <| VPrimitive Prim.cons
      val () = insert prim' "car" <| VPrimitive Prim.car
      val () = insert prim' "cdr" <| VPrimitive Prim.cdr
      val () = insert prim' "typeof" <| VPrimitive Prim.typeOf
    in
      prim'
    end
end
