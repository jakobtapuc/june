structure Expander :> JUNE_EXPANDER =
struct
  open Ast

  type t = ast -> ast

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
    | expandLet _ = raise Fail "Unreachable let"

  fun expandAnd (List (Symbol ("and", pos) :: exprs, _)) =
        let
          fun expand [] = Symbol ("#t", pos)
            | expand [x] = x
            | expand (x :: xs) =
                List
                  ([Symbol ("if", pos), x, expand xs, Symbol ("#f", pos)], pos)
        in
          expand exprs
        end

    | expandAnd _ = raise Fail "Unreachable and"

  (*!
   * This expander makes sure that `or` doesn't evaluate its arguments twice.
   *
   * If it didn't, a naive implementation would expand an `or` like this:
   * ```scheme
   * (or (foo) (bar))
   * ```
   * To:
   * ```scheme
   * (if (foo)
   *     (foo)
   *     (bar))
   * ```
   * Which is bad because this code would print "hello" twice:
   * ```scheme
   * (or (show-ln "hello") (bar))
   * ```
   * The expander wraps the first case in a temporary variable which makes
   * sure that the value is only evaluated once.
  *)
  fun expandOr (List (Symbol ("or", pos) :: exprs, _)) =
        let
          fun expand [] = Symbol ("#f", pos)
            | expand [x] = x
            | expand (x :: xs) =
                expandLet (List
                  ( [ Symbol ("let", pos)
                    , List ([List ([Symbol ("%%%tmp", pos), x], pos)], pos)
                    , List
                        ( [ Symbol ("if", pos)
                          , Symbol ("%%%tmp", pos)
                          , Symbol ("%%%tmp", pos)
                          , expand xs
                          ]
                        , pos
                        )
                    ]
                  , pos
                  ))
        in
          expand exprs
        end

    | expandOr _ = raise Fail "Unreachable"

  fun expandCond (List (Symbol ("cond", pos) :: clauses, _)) =
        let
          fun expand [] =
                raise Value.Value ("Empty cond", pos)
            | expand (List ([Symbol ("else", _), expr], _) :: []) = expr
            | expand (List ([test, expr], _) :: rest) =
                List ([Symbol ("if", pos), test, expr, expand rest], pos)
            | expand _ =
                raise Value.Value ("Malformed cond", pos)
        in
          expand clauses
        end
    | expandCond _ = raise Fail "Unreachable cond"

  fun expand ast =
    case ast of
    | List (Symbol ("let", _) :: List (_, _) :: _, _) => expand (expandLet ast)
    | List (Symbol ("and", _) :: _, _) => expand (expandAnd ast)
    | List (Symbol ("or", _) :: _, _) => expand (expandOr ast)
    | List (Symbol ("cond", _) :: List (_, _) :: _, _) =>
        expand (expandCond ast)
    | List (xs, pos) => List (List.map expand xs, pos)
    | x => x
end
