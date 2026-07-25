structure ListExt =
struct
  type 'a t = 'a list

  fun zip [] [] = []
    | zip (x :: xs) (y :: ys) =
        (x, y) :: zip xs ys
    | zip _ _ = raise Fail "Invalid list lengths"
end
