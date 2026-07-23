structure Prim :> JUNE_PRIM =
struct
  open Value

  type v = Value.v

  type pos = Token.position

  type prim_func = v list -> pos -> v

  type prim_func = v list -> Token.position -> v

  fun add xs pos =
    let
      fun unwrap (Integer x) = x
        | unwrap _ =
            raise Value ("Non-integer argument supplied to `+`", pos)
      val unwrapped = List.map unwrap xs
      val folded = List.foldl op+ 0 unwrapped
    in
      Integer folded
    end

  fun sub xs pos =
    let
      fun unwrap (Integer x) = x
        | unwrap _ =
            raise Value ("Non-integer argument supplied to `-`", pos)
      val unwrapped = List.map unwrap xs
      val folded = List.foldl op- 0 unwrapped
    in
      Integer folded
    end

  fun mult xs pos =
    let
      fun unwrap (Integer x) = x
        | unwrap _ =
            raise Value ("Non-integer argument supplied to `*`", pos)
      val unwrapped = List.map unwrap xs
      val folded = List.foldl op* 0 unwrapped
    in
      Integer folded
    end

  fun div' xs pos =
    let
      fun unwrap (Integer x) = x
        | unwrap _ =
            raise Value ("Non-integer argument supplied to `/`", pos)
      val unwrapped = List.map unwrap xs
      val folded = List.foldl (op div) 0 unwrapped
    in
      Integer folded
    end

  fun eq [a, b] _ =
        let
          fun eq' (Integer x) (Integer y) = (x = y)
            | eq' (String x) (String y) = (x = y)
            | eq' (Boolean x) (Boolean y) = (x = y)
            | eq' (Symbol x) (Symbol y) = (x = y)
            | eq' Nil Nil = true
            | eq' Undef Undef = true
            | eq' (Float x) (Float y) =
                let
                  fun floatEq x' y' =
                    let val order = Real.compare (x', y')
                    in order = General.EQUAL
                    end
                in
                  floatEq x y
                end
            | eq' (Pair (x, y)) (Pair (x', y')) =
                let
                  val aEqual = eq' x x'
                  val bEqual = eq' y y'
                in
                  aEqual = bEqual
                end
            | eq' _ _ = false
        in
          Boolean (eq' a b)
        end
    | eq _ pos =
        raise Value ("Invalid number of arguments supplied to `eq?`", pos)

  fun not' [Boolean x] _ =
        Boolean (not x)
    | not' [_] pos =
        raise Value ("Non-boolean argument supplied to `not`", pos)
    | not' _ pos =
        raise Value ("Invalid number of arguments supplied to `not`", pos)

  fun show xs _ =
    let
      fun unwrap (String s) = s
        | unwrap v = toString v
      val unwrapped = List.map unwrap xs
      val folded = String.concatWith " " unwrapped

      val () = print folded
    in
      Undef
    end

  fun showLn x pos =
    let
      val () = ignore (show x pos)
      val () = print "\n"
    in
      Undef
    end

  fun list' xs _ =
    List.foldr (fn (x, acc) => Pair (x, acc)) Nil xs

  fun cons [x, Pair (y, z)] _ =
        Pair (x, Pair (y, z))
    | cons [x, Nil] _ = Pair (x, Nil)
    | cons [x, y] _ = Pair (x, y)
    | cons _ pos =
        raise Value ("Invalid number of arguments supplied to `cons`", pos)

  fun car [Pair (x, _)] _ = x
    | car [_] pos =
        raise Value ("`car` expects a pair", pos)
    | car _ pos =
        raise Value ("Invalid number of arguments supplied to `car`", pos)

  fun cdr [Pair (_, y)] _ = y
    | cdr [_] pos =
        raise Value ("`cdr` expects a pair", pos)
    | cdr _ pos =
        raise Value ("Invalid number of arguments supplied to `cdr`", pos)
end
