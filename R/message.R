
## add print colors to text
pblue <- function(x) {
    paste0("\033[38;5;110m", x, "\033[39m")
}
pmint <- function(x) {
    paste0("\033[38;5;36m", x, "\033[39m")
}
ppink <- function(x) {
    paste0("\033[38;5;198m", x, "\033[39m")
}
ppurp <- function(x) {
    paste0("\033[38;5;99m", x, "\033[39m")
}
pgray <- function(x) {
    paste0("\033[38;5;241m", x, "\033[39m")
}

## quick functions for horizontal spaces and bars
hbar0 <- function() "\u2015"
hbar <- function() "\u2501"
hbars0 <- function(n) if (n > 0) paste(rep(hbar0(), n), collapse = "") else ""
hbars <- function(n) if (n > 0) paste(rep(hbar(), n), collapse = "") else ""
hspace <- function() " "
hspaces <- function(n) if (n > 0) paste(rep(hspace(), n), collapse = "") else ""

## format elements
format_secs <- function(secs = NULL) {
  if (is.null(secs)) {
    return("     ")
  }
  secs <- if (secs > 129599) {
    sprintf(" %3dd", as.integer(secs / 86400))
  } else if (secs > 3599) {
    sprintf(" %3dh", as.integer(secs / 3600))
  } else if (secs > 90) {
    sprintf(" %3dm", as.integer(secs / 60))
  } else if (secs > 0) {
    sprintf(" %3ds", as.integer(secs))
  } else {
    ""
  }
  ppurp(secs)
}
format_verb <- function(verb) {
  pmint(sprintf("..%s..  ", verb))
}

format_prop <- function(prop, width) {
  done <- round(prop * width)
  left <- width - done
  sprintf("%s%s", ppink(hbars(done)), pgray(hbars0(left)))
}

## adjust width given width of verb and secs
calc_width <- function(width = NULL, verb, secs = 5L) {
  verb <- (if (is.character(verb)) nchar(verb) else verb) + 6
  if (is.null(width)) {
    width <- getOption("width", 80)
  }
  width - verb - secs
}

as_pbr_msg <- function(x) structure(x, class = "pbr_msg")
as_pbr <- function(x) structure(x, class = c("pbr", "list"))

print.pbr_msg <- function(x, ..) {
  cat(x)
  cat(.makeMessage("\r"))
}

#' @export
print.pbr <- function(x, ...) {
  cat(sprintf(
    "# A pbr progress bar (N = %d)", x$iterations), fill = TRUE)
  cat("$tick() = iterate\n$done() = complete\n")
}


## build the actual bar
pb_build_msg <- function(verb, prop, width = NULL, secs = NULL) {
  as_pbr_msg(
    paste0(
      format_verb(verb),
      format_prop(prop, calc_width(width, verb)),
      format_secs(secs)
    )
  )
}