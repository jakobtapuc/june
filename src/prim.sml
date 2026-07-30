structure Prim :> JUNE_PRIM =
struct
  open Value Result

  type prim_func = Value.v list -> (Value.v, error) r

  fun typeError f actual expected = Err <| Type {f, actual, expected}

  fun arityError f actual expected = Err <| Arity {f, actual, expected}

  fun add [] =
        arityError "+" 0 1
    | add xs =
        let
          fun add' [] acc = Ok <| VInteger acc
            | add' (VInteger n :: rest) acc =
                add' rest (acc + n)
            | add' (x :: _) _ =
                typeError "+" (stringOf x) "integer"
        in
          add' xs 0
        end

  fun sub [VInteger x] =
        Ok <| VInteger (~x)
    | sub xs =
        let
          fun sub' [] acc = Ok <| VInteger acc
            | sub' (VInteger n :: rest) acc =
                sub' rest (acc - n)
            | sub' (x :: _) _ =
                typeError "-" (stringOf x) "integer"
        in
          sub' xs 0
        end

  fun mult [] =
        arityError "+" 0 1
    | mult xs =
        let
          fun mult' [] acc = Ok <| VInteger acc
            | mult' (VInteger n :: rest) acc =
                mult' rest (acc * n)
            | mult' (x :: _) _ =
                typeError "-" (stringOf x) "integer"
        in
          mult' xs 1
        end

  fun div' [] =
        arityError "+" 0 1
    | div' xs =
        let
          fun div'' [] acc = Ok <| VInteger acc
            | div'' (VInteger n :: rest) acc =
                div'' rest (acc div n)
            | div'' (x :: _) _ =
                typeError "-" (stringOf x) "integer"
        in
          div'' xs 1
        end

  fun eq [a, b] =
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
          Ok <| VBoolean (eq' a b)
        end
    | eq args =
        arityError "eq?" (length args) 2

  fun not' [VBoolean x] =
        Ok <| VBoolean (not x)
    | not' [x] =
        typeError "not" (stringOf x) "bool"
    | not' args =
        arityError "not" (length args) 1

  fun show xs =
    let
      fun unwrap (VString s) = s
        | unwrap v = toString v
      val unwrapped = List.map unwrap xs
      val folded = String.concatWith " " unwrapped

      val () = print folded
    in
      Ok VUnit
    end

  fun showLn x =
    let
      val () = ignore (show x)
      val () = print "\n"
    in
      Ok VUnit
    end

  fun list' xs =
    Ok <| List.foldr (fn (x, acc) => VPair (x, acc)) VNil xs

  fun cons [x, VPair (y, z)] =
        Ok <| VPair (x, VPair (y, z))
    | cons [x, VNil] =
        Ok <| VPair (x, VNil)
    | cons [x, y] =
        Ok <| VPair (x, y)
    | cons args =
        arityError "cons" (length args) 2

  fun car [VPair (x, _)] = Ok x
    | car [x] =
        typeError "car" (stringOf x) "pair"
    | car args =
        arityError "car" (length args) 1

  fun cdr [VPair (_, y)] = Ok y
    | cdr [x] =
        typeError "cdr" (stringOf x) "pair"
    | cdr args =
        arityError "cdr" (length args) 1

  fun typeOf [x] = Ok <| Value.typeOf x
    | typeOf args =
        arityError "typeof" (length args) 1
end
