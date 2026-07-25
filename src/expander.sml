structure Expander :> JUNE_EXPANDER =
struct
  exception Expander of {msg: string, pos: Token.position, trace: StackTrace.t}

  open Ast

  type t = ast -> ast

  fun failWithTrace msg pos =
    raise Expander {msg, pos, trace = ! StackTrace.st}

  fun transformBinding (List ([Symbol (name, _), expr'], _)) _ = (name, expr')
    | transformBinding _ pos = failWithTrace "Malformed let binding" pos

  fun expandLet (List (Symbol ("let", pos) :: List (bindings, _) :: body, _)) _ =
        let

          val transformed = List.map (fn x => transformBinding x pos) bindings

          val names = List.map (fn (name, _) => Symbol (name, pos)) transformed

          val exprs = List.map (fn (_, expr') => expr') transformed

          val lambda = List
            (Symbol ("lambda", pos) :: List (names, pos) :: body, pos)
        in
          List (lambda :: exprs, pos)
        end
    | expandLet _ pos = failWithTrace "Malformed `let`" pos

  fun expandLetStar
        (List (Symbol ("let*", pos) :: List (bindings, _) :: body, _)) _ =
        let
          val transformed = List.map (fn x => transformBinding x pos) bindings

          fun expand [] =
                List (Symbol ("begin", pos) :: body, pos)
            | expand ((name, expr) :: xs) =
                expandLet
                  (List
                     ( [ Symbol ("let", pos)
                       , List ([List ([Symbol (name, pos), expr], pos)], pos)
                       , expand xs
                       ]
                     , pos
                     )) pos
        in
          expand transformed
        end
    | expandLetStar _ pos = failWithTrace "Malformed `let*`" pos

  fun expandLetRec
        (List (Symbol ("letrec", pos) :: List (bindings, _) :: body, _)) _ =
        let
          val transformed = List.map (fn x => transformBinding x pos) bindings

          val letrecBindings =
            List.map
              (fn (name, _) =>
                 List ([Symbol (name, pos), Symbol ("#unit", pos)], pos))
              transformed

          val filledCells =
            List.map
              (fn (name, expr') =>
                 List ([Symbol ("set!", pos), Symbol (name, pos), expr'], pos))
              transformed

        in
          expandLet
            (List
               ( [Symbol ("let", pos), List (letrecBindings, pos)] @ filledCells
                 @ body
               , pos
               )) pos
        end
    | expandLetRec _ pos = failWithTrace "Malformed `letrec`" pos

  fun expandAnd (List (Symbol ("and", pos) :: exprs, _)) _ =
        let
          fun expand [] = Symbol ("#t", pos)
            | expand [x] = x
            | expand (x :: xs) =
                List
                  ([Symbol ("if", pos), x, expand xs, Symbol ("#f", pos)], pos)
        in
          expand exprs
        end

    | expandAnd _ pos = failWithTrace "Malformed `and`" pos

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
  fun expandOr (List (Symbol ("or", pos) :: exprs, _)) _ =
        let
          fun expand [] = Symbol ("#f", pos)
            | expand [x] = x
            | expand (x :: xs) =
                expandLet
                  (List
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
                     )) pos
        in
          expand exprs
        end

    | expandOr _ pos = failWithTrace "Malformed `or`" pos

  fun expandCond (List (Symbol ("cond", pos) :: clauses, _)) _ =
        let
          fun expand [] = failWithTrace "Empty cond" pos
            | expand (List ([Symbol ("else", _), expr], _) :: []) = expr
            | expand (List ([test, expr], _) :: rest) =
                List ([Symbol ("if", pos), test, expr, expand rest], pos)
            | expand _ = failWithTrace "Malformed cond" pos
        in
          expand clauses
        end
    | expandCond _ pos = failWithTrace "Malformed `cond`" pos

  fun expandWhen (List (Symbol ("when", pos) :: cond :: exprs, _)) _ =
        List
          ( [ Symbol ("if", pos)
            , cond
            , List (Symbol ("begin", pos) :: exprs, pos)
            , Symbol ("#unit", pos)
            ]
          , pos
          )
    | expandWhen _ pos = failWithTrace "Malformed `when`" pos

  fun expand ast =
    case ast of
    | List (Symbol ("let", pos) :: _, _) => expand (expandLet ast pos)
    | List (Symbol ("let*", pos) :: _, _) => expand (expandLetStar ast pos)
    | List (Symbol ("letrec", pos) :: _, _) => expand (expandLetRec ast pos)
    | List (Symbol ("and", pos) :: _, _) => expand (expandAnd ast pos)
    | List (Symbol ("or", pos) :: _, _) => expand (expandOr ast pos)
    | List (Symbol ("cond", pos) :: _, _) => expand (expandCond ast pos)
    | List (Symbol ("when", pos) :: _, _) => expand (expandWhen ast pos)
    | List (xs, pos) => List (List.map expand xs, pos)
    | x => x
end
