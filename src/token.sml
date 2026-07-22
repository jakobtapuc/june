structure Token :> JUNE_TOKEN =
struct
  type position = {line: int, column: int}

  datatype token =
  | LParen
  | RParen
  | Quote
  | Integer of int
  | Float of real
  | Boolean of bool
  | Symbol of string
  | String of string
  | Eof

  type t = {token: token, pos: position}

  fun toString {token, pos} =
    let
      fun stringifyToken LParen = "LParen"
        | stringifyToken RParen = "RParen"
        | stringifyToken Quote = "Quote"
        | stringifyToken (Integer i) =
            "Integer(" ^ (Int.toString i) ^ ")"
        | stringifyToken (Float f) =
            "Float(" ^ (Real.toString f) ^ ")"
        | stringifyToken (Boolean b) =
            "Boolean(" ^ (Bool.toString b) ^ ")"
        | stringifyToken (Symbol s) = "Symbol(" ^ s ^ ")"
        | stringifyToken (String s) = "String(\"" ^ s ^ "\")"
        | stringifyToken Eof = "Eof"

      fun stringifyPosition {line, column} =
        "{ line: " ^ (Int.toString line) ^ ", column: " ^ (Int.toString column)
        ^ " }"
    in
      "{ token: " ^ (stringifyToken token) ^ ", pos: " ^ (stringifyPosition pos)
      ^ " }"
    end
end
