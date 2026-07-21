structure Prim : JUNE_PRIM = struct
  open Value

  type prim_func = t list -> Token.position -> t

  fun add xs pos =
      let
        fun unwrap (Integer x) = x
          | unwrap _ = raise Value ("Invalid arguments to `+`", pos)
        val unwrapped = List.map unwrap xs
        val folded = List.foldl op+ 0 unwrapped
      in
        Integer folded
      end

  fun sub xs pos =
      let
        fun unwrap (Integer x) = x
          | unwrap _ = raise Value ("Invalid arguments to `-`", pos)
        val unwrapped = List.map unwrap xs
        val folded = List.foldl op- 0 unwrapped
      in
        Integer folded
      end

  fun mult xs pos =
      let
        fun unwrap (Integer x) = x
          | unwrap _ = raise Value ("Invalid arguments to `*`", pos)
        val unwrapped = List.map unwrap xs
        val folded = List.foldl op* 0 unwrapped
      in
        Integer folded
      end

  fun div' xs pos =
      let
        fun unwrap (Integer x) = x
          | unwrap _ = raise Value ("Invalid arguments to `/`", pos)
        val unwrapped = List.map unwrap xs
        val folded = List.foldl (op div) 0 unwrapped
      in
        Integer folded
      end

  fun and' xs pos =
      let
        fun unwrap (Boolean x) = x
          | unwrap _ = raise Value ("Invalid arguments to `and`", pos)
        val unwrapped = List.map unwrap xs
        val folded =
          List.foldl (fn (x, acc) => x andalso acc) true unwrapped
      in
        Boolean folded
      end

  fun or' xs pos =
      let
        fun unwrap (Boolean x) = x
          | unwrap _ = raise Value ("Invalid arguments to `or`", pos)
        val unwrapped = List.map unwrap xs
        val folded =
          List.foldl (fn (x, acc) => x orelse acc) false unwrapped
      in
        Boolean folded
      end

  fun show xs _ =
      let
        fun unwrap x = toString x
        val unwrapped = List.map unwrap xs
        val folded = String.concatWith " " unwrapped

        do print folded
      in
        Undef
      end

  fun showLn x _ =
    let
      do ignore (show x)
      do print "\n"
    in
      Undef
    end
end