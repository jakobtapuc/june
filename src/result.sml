structure Result :> JUNE_RESULT =
struct
  datatype ('a, 'e) r = | Ok of 'a | Err of 'e

  fun map f (Ok x) = Ok <| f x
    | map _ (Err e) = Err e

  fun fold _ acc [] = Ok acc
    | fold f acc (x :: xs) =
        case f acc x of
        | Ok acc' => fold f acc' xs
        | e => e

  fun bind (Ok x) f = f x
    | bind (Err e) _ = Err e
end
