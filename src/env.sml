structure Env :> JUNE_ENV =
struct
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
        | NONE => raise Value ("Unbound variable: " ^ name, pos)

  fun insert (Env {bindings, ...}) name value =
    HashTable.insert bindings (name, ref value)

  fun set (Env {bindings, ...}) name value =
    case HashTable.find bindings name of
    | SOME cell => cell := value
    | NONE => raise Fail "Impossible path"

  val prim =
    let
      val prim' = empty ()

      val () = insert prim' "+" <| Primitive Prim.add
      val () = insert prim' "-" <| Primitive Prim.sub
      val () = insert prim' "*" <| Primitive Prim.mult
      val () = insert prim' "/" <| Primitive Prim.div'
      val () = insert prim' "eq?" <| Primitive Prim.eq
      val () = insert prim' "not" <| Primitive Prim.not'
      val () = insert prim' "show" <| Primitive Prim.show
      val () = insert prim' "show-ln" <| Primitive Prim.showLn
      val () = insert prim' "list" <| Primitive Prim.list'
      val () = insert prim' "cons" <| Primitive Prim.cons
      val () = insert prim' "car" <| Primitive Prim.car
      val () = insert prim' "cdr" <| Primitive Prim.cdr
    in
      prim'
    end
end
