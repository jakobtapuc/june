structure Prim :> JUNE_PRIM =
struct
  open Value

  type v = Value.v

  type pos = Token.position

  type prim_func = v list -> pos -> v

  type prim_func = v list -> Token.position -> v

  (* fun add xs pos =
    let
      fun unwrap (VInteger x) = x
        | unwrap x =
            failTypeWithTrace "+" (stringOf x) "integer" pos
      val unwrapped = List.map unwrap xs
      val folded = List.foldl op+ 0 unwrapped
    in
      VInteger folded
    end *)
  fun add xs pos =
    let
      fun sum [] acc = VInteger acc
        | sum (VInteger n :: rest) acc =
            sum rest (acc + n)
        | sum (x :: _) _ =
            failTypeWithTrace "+" (stringOf x) "integer" pos
    in
      sum xs 0
    end

  fun sub [] pos =
        failArgWithTrace "-" 0 1 pos
    | sub [VInteger x] _ =
        VInteger (~x)
    | sub (VInteger x :: rest) pos =
        let
          fun unwrap (VInteger n) = n
            | unwrap x =
                failTypeWithTrace "-" (stringOf x) "integer" pos
          val ns = List.map unwrap rest
          val result = List.foldl (fn (n, acc) => acc - n) x ns
        in
          VInteger result
        end
    | sub args pos =
        failTypeWithTrace "-" (stringOf <| hd args) "integer" pos

  fun mult xs pos =
    let
      fun unwrap (VInteger x) = x
        | unwrap x =
            failTypeWithTrace "*" (stringOf x) "integer" pos
      val unwrapped = List.map unwrap xs
      val folded = List.foldl op* 1 unwrapped
    in
      VInteger folded
    end

  fun div' xs pos =
    let
      fun unwrap (VInteger x) = x
        | unwrap x =
            failTypeWithTrace "/" (stringOf x) "integer" pos
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
            | eq' (VType t) (VType t') = (t = t')
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
            | eq' (VPromise {objectId = id, ...})
                (VPromise {objectId = id', ...}) =
                (ObjectId.toString id = ObjectId.toString id')
            | eq' (VClosure {objectId = id, ...})
                (VClosure {objectId = id', ...}) =
                (ObjectId.toString id = ObjectId.toString id')
            | eq' _ _ = false
        in
          VBoolean (eq' a b)
        end
    | eq args pos =
        failArgWithTrace "eq?" (length args) 2 pos

  fun not' [VBoolean x] _ =
        VBoolean (not x)
    | not' [x] pos =
        failTypeWithTrace "not" (stringOf x) "bool" pos
    | not' args pos =
        failArgWithTrace "not" (length args) 1 pos

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
    | cons args pos =
        failArgWithTrace "cons" (length args) 2 pos

  fun car [VPair (x, _)] _ = x
    | car [x] pos =
        failTypeWithTrace "car" (stringOf x) "pair" pos
    | car args pos =
        failArgWithTrace "car" (length args) 1 pos

  fun cdr [VPair (_, y)] _ = y
    | cdr [x] pos =
        failTypeWithTrace "cdr" (stringOf x) "pair" pos
    | cdr args pos =
        failArgWithTrace "cdr" (length args) 1 pos

  fun typeOf [x] _ = Value.typeOf x
    | typeOf args pos =
        failArgWithTrace "typeof" (length args) 1 pos
end
