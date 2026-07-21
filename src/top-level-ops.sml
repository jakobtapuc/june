infixr 0 <|
infix 1 |>

fun op<| (f, x) = f x

fun op|> (x, f) = f x
