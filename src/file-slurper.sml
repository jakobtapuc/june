structure FileSlurper =
struct
  fun slurp filename =
    let
      val ins = TextIO.openIn filename

      fun slurp' acc =
        case TextIO.inputLine ins of
        | NONE => String.concat (List.rev acc)
        | SOME line => slurp' (line :: acc)

      val contents = slurp' []

      val () = TextIO.closeIn ins
    in
      contents
    end
end
