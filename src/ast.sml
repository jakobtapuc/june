structure Ast :> JUNE_AST = struct
  datatype t =
    | Integer of int * Token.position
    | Symbol of string * Token.position
    | List of t list * Token.position

fun toString expr =
  let
    fun indent n =
      String.implode (List.tabulate (n, fn _ => #" "))

    fun toString' depth expr' =
      case expr' of
        | Integer (n, _) =>
            indent depth ^
              "Integer(" ^ Int.toString n ^ ")"
        | Symbol (s, _) =>
            indent depth ^
              "Symbol(" ^ s ^ ")"
        | List (xs, _) =>
            let
              fun stringifyChildren [] = ""
                | stringifyChildren (x :: xs') =
                    "\n" ^
                      toString' (depth + 2) x ^
                        stringifyChildren xs'
            in
              indent depth ^
                "List(" ^
                  stringifyChildren xs ^
                    "\n" ^
                      indent depth ^
                        ")"
            end
  in
    toString' 0 expr
  end
end
