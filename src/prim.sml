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

  fun and' xs pos =
    let
      fun unwrap (Boolean x) = x
        | unwrap _ =
            raise Value ("Non-boolean argument supplied to `and`", pos)
      val unwrapped = List.map unwrap xs
      val folded = List.foldl (fn (x, acc) => x andalso acc) true unwrapped
    in
      Boolean folded
    end

  fun or' xs pos =
    let
      fun unwrap (Boolean x) = x
        | unwrap _ =
            raise Value ("Non-boolean argument supplied to `or`", pos)
      val unwrapped = List.map unwrap xs
      val folded = List.foldl (fn (x, acc) => x orelse acc) false unwrapped
    in
      Boolean folded
    end

  fun eq [a, b] pos =
        let
          fun eq' (Integer x) (Integer y) _ = (x = y)
            | eq' (String x) (String y) _ = (x = y)
            | eq' (Boolean x) (Boolean y) _ = (x = y)
            | eq' (Symbol x) (Symbol y) _ = (x = y)
            | eq' Nil Nil _ = true
            | eq' Undef Undef _ = true
            | eq' (Float x) (Float y) _ =
                let
                  fun floatEq x' y' =
                    let val order = Real.compare (x', y')
                    in order = General.EQUAL
                    end
                in
                  floatEq x y
                end
            | eq' (Pair (x, y)) (Pair (x', y')) pos' =
                let
                  val aEqual = eq' x x' pos'
                  val bEqual = eq' y y' pos'
                in
                  aEqual = bEqual
                end
            | eq' _ _ _ = false
        in
          Boolean (eq' a b pos)
        end
    | eq _ pos =
        raise Value ("Invalid number of arguments supplied to `eq?`", pos)

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
