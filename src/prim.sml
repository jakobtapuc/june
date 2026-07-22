structure Prim :> JUNE_PRIM = struct
  open Value

  type v = Value.v

  type pos = Token.position

  type prim_func = v list -> pos -> v

  type prim_func = v list -> Token.position -> v

  fun add xs pos =
      let
        fun unwrap (Integer x) = x
          | unwrap _ = raise Value ("Non-integer argument when applying `+`", pos)
        val unwrapped = List.map unwrap xs
        val folded = List.foldl op+ 0 unwrapped
      in
        Integer folded
      end

  fun sub xs pos =
      let
        fun unwrap (Integer x) = x
          | unwrap _ = raise Value ("Non-integer argument when applying `-`", pos)
        val unwrapped = List.map unwrap xs
        val folded = List.foldl op- 0 unwrapped
      in
        Integer folded
      end

  fun mult xs pos =
      let
        fun unwrap (Integer x) = x
          | unwrap _ = raise Value ("Non-integer argument when applying `*`", pos)
        val unwrapped = List.map unwrap xs
        val folded = List.foldl op* 0 unwrapped
      in
        Integer folded
      end

  fun div' xs pos =
      let
        fun unwrap (Integer x) = x
          | unwrap _ = raise Value ("Non-integer argument when applying `/`", pos)
        val unwrapped = List.map unwrap xs
        val folded = List.foldl (op div) 0 unwrapped
      in
        Integer folded
      end

  fun and' xs pos =
      let
        fun unwrap (Boolean x) = x
          | unwrap _ = raise Value ("Non-boolean argument when applying `and`", pos)
        val unwrapped = List.map unwrap xs
        val folded =
          List.foldl (fn (x, acc) => x andalso acc) true unwrapped
      in
        Boolean folded
      end

  fun or' xs pos =
    let
      fun unwrap (Boolean x) = x
        | unwrap _ = raise Value ("Non-boolean argument when applying `or`", pos)
      val unwrapped = List.map unwrap xs
      val folded =
        List.foldl (fn (x, acc) => x orelse acc) false unwrapped
    in
      Boolean folded
    end

  fun eq [Integer x, Integer y] _ =
    Boolean (x = y)
    | eq _ pos = raise Value ("Non-integer argument when applying `eq`", pos)

  fun show xs _ =
    let
      fun unwrap x = toString x
      val unwrapped = List.map unwrap xs
      val folded = String.concatWith " " unwrapped

      do print folded
    in
      Undef
    end

  fun showLn x pos =
    let
      do ignore (show x pos)
      do print "\n"
    in
      Undef
    end
end