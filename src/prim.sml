structure Prim :> JUNE_PRIM =
struct
  open Value

  type v = Value.v

  type pos = Token.position

  type prim_func = v list -> pos -> v

  type prim_func = v list -> Token.position -> v

  fun add xs pos =
    let
      fun unwrap (VInteger x) = x
        | unwrap _ =
            raise Value ("Non-integer argument supplied to `+`", pos)
      val unwrapped = List.map unwrap xs
      val folded = List.foldl op+ 0 unwrapped
    in
      VInteger folded
    end

  fun sub [] pos =
        raise Value ("`-` expects at least 1 argument, 0 given", pos)
    | sub [VInteger x] _ =
        VInteger (~x)
    | sub (VInteger x :: rest) pos =
        let
          fun unwrap (VInteger n) = n
            | unwrap _ =
                raise Value ("Non-integer argument supplied to `-`", pos)

          val ns = List.map unwrap rest
          val result = List.foldl (fn (n, acc) => acc - n) x ns
        in
          VInteger result
        end
    | sub _ pos =
        raise Value ("Non-integer argument supplied to `-`", pos)

  fun mult xs pos =
    let
      fun unwrap (VInteger x) = x
        | unwrap _ =
            raise Value ("Non-integer argument supplied to `*`", pos)
      val unwrapped = List.map unwrap xs
      val folded = List.foldl op* 1 unwrapped
    in
      VInteger folded
    end

  fun div' xs pos =
    let
      fun unwrap (VInteger x) = x
        | unwrap _ =
            raise Value ("Non-integer argument supplied to `/`", pos)
      val unwrapped = List.map unwrap xs
      val folded = List.foldl (op div) 1 unwrapped
    in
      VInteger folded
    end

  fun eq [a, b] _ =
        let
          fun eq' (VInteger x) (VInteger y) = (x = y)
            | eq' (VString x) (VString y) = (x = y)
            | eq' (VBoolean x) (VBoolean y) = (x = y)
            | eq' (VSymbol x) (VSymbol y) = (x = y)
            | eq' VNil VNil = true
            | eq' VUnit VUnit = true
            | eq' (VFloat x) (VFloat y) =
                let
                  fun floatEq x' y' =
                    let val order = Real.compare (x', y')
                    in order = General.EQUAL
                    end
                in
                  floatEq x y
                end
            | eq' (VPair (x, y)) (VPair (x', y')) =
                let
                  val aEqual = eq' x x'
                  val bEqual = eq' y y'
                in
                  aEqual = bEqual
                end
            | eq' _ _ = false
        in
          VBoolean (eq' a b)
        end
    | eq (args as _) pos =
        raise Value
          ( "Invalid number of arguments supplied to `eq?`. 2 expected but "
            ^ (Int.toString (List.length args)) ^ " given."
          , pos
          )

  fun not' [VBoolean x] _ =
        VBoolean (not x)
    | not' [_] pos =
        raise Value ("Non-boolean argument supplied to `not`", pos)
    | not' _ pos =
        raise Value ("Invalid number of arguments supplied to `not`", pos)

  fun show xs _ =
    let
      fun unwrap (VString s) = s
        | unwrap v = toString v
      val unwrapped = List.map unwrap xs
      val folded = String.concatWith " " unwrapped

      val () = print folded
    in
      VUnit
    end

  fun showLn x pos =
    let
      val () = ignore (show x pos)
      val () = print "\n"
    in
      VUnit
    end

  fun list' xs _ =
    List.foldr (fn (x, acc) => VPair (x, acc)) VNil xs

  fun cons [x, VPair (y, z)] _ =
        VPair (x, VPair (y, z))
    | cons [x, VNil] _ = VPair (x, VNil)
    | cons [x, y] _ = VPair (x, y)
    | cons _ pos =
        raise Value ("Invalid number of arguments supplied to `cons`", pos)

  fun car [VPair (x, _)] _ = x
    | car [_] pos =
        raise Value ("`car` expects a pair", pos)
    | car _ pos =
        raise Value ("Invalid number of arguments supplied to `car`", pos)

  fun cdr [VPair (_, y)] _ = y
    | cdr [_] pos =
        raise Value ("`cdr` expects a pair", pos)
    | cdr _ pos =
        raise Value ("Invalid number of arguments supplied to `cdr`", pos)
end
