structure Env :> JUNE_ENV =
struct
  open Value

  fun lookup (Env envRef) (name, pos) =
    let
      fun loop [] =
            raise Value ("Unbound variable: " ^ name, pos)
        | loop ((name', value) :: rest) =
            if name = name' then !value else loop rest
    in
      loop envRef
    end

  fun insert (Env env) name value =
    Env ((name, value) :: env)

  val initialEnv = Env
    [ ("+", ref <| Primitive Prim.add)
    , ("-", ref <| Primitive Prim.sub)
    , ("*", ref <| Primitive Prim.mult)
    , ("/", ref <| Primitive Prim.div')
    , ("and", ref <| Primitive Prim.and')
    , ("or", ref <| Primitive Prim.or')
    , ("eq?", ref <| Primitive Prim.eq)
    , ("show", ref <| Primitive Prim.show)
    , ("show-ln", ref <| Primitive Prim.showLn)
    , ("list", ref <| Primitive Prim.list')
    , ("cons", ref <| Primitive Prim.cons)
    , ("car", ref <| Primitive Prim.car)
    , ("cdr", ref <| Primitive Prim.cdr)
    ]
end
