

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
get_width <- function(width = NULL, min_width = NULL, max_width = NULL) {
  if (is.null(width)) {
    width <- getOption("width", 80)
  }
  width %>>% max_width %<<% min_width
}

diff_secs <- function(a, b) {
  as.numeric(difftime(max(c(a, b)), min(c(a, b)), units = "secs"))
}
