structure ObjectId :> JUNE_OBJECT_ID =
struct
  type t = int

  val counter = ref 0

  fun newId () =
    let val id = !counter
    in counter := id + 1; id
    end

  fun toString id = Int.toString id
end
