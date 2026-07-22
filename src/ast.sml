structure Ast :> JUNE_AST =
struct
  datatype ast =
  | Integer of int * Token.position
  | Float of real * Token.position
  | String of string * Token.position
  | Symbol of string * Token.position
  | List of ast list * Token.position

  fun toString expr =
    let
      fun indent n =
        List.tabulate (n, fn _ => #" ") |> String.implode

      fun toString' depth expr' =
        case expr' of
        | Integer (n, _) => indent depth ^ "Integer(" ^ Int.toString n ^ ")"
        | Float (f, _) => indent depth ^ "Float(" ^ Real.toString f ^ ")"
        | String (s, _) => indent depth ^ "String(" ^ s ^ ")"
        | Symbol (s, _) => indent depth ^ "Symbol(" ^ s ^ ")"
        | List (xs, _) =>
            let
              fun stringifyChildren [] = ""
                | stringifyChildren (x :: xs') =
                    "\n" ^ toString' (depth + 2) x ^ stringifyChildren xs'
            in
              indent depth ^ "List(" ^ stringifyChildren xs ^ "\n"
              ^ indent depth ^ ")"
            end
    in
      toString' 0 expr
    end
end
