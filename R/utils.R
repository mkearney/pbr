`%>>%` <- function(x, y) {
  if (!is.null(y) && x > y)
    y
  else
    x
}
`%<<%` <- function(x, y) {
  if (!is.null(y) && x < y)
    y
  else
    x
}
