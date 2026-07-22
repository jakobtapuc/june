structure Expander :> JUNE_EXPANDER =
struct
  open Ast

  fun expandLet (List (Symbol ("let", pos) :: List (bindings, _) :: body, _)) =
        let
          fun transformBinding (List ([Symbol (name, _), expr'], _)) =
                (name, expr')
            | transformBinding _ =
                raise Value.Value ("Malformed let binding", pos)

          val transformed = List.map transformBinding bindings

          val names = List.map (fn (name, _) => Symbol (name, pos)) transformed

          val exprs = List.map (fn (_, expr') => expr') transformed

          val lambda = List
            (Symbol ("lambda", pos) :: List (names, pos) :: body, pos)
        in
          List (lambda :: exprs, pos)
        end
    | expandLet _ = raise Fail "Unreachable"
end
