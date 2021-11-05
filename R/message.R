
## add print colors to text
pdone <- function(x) {
  if (x == "") {
    return(x)
  }
  paste0("\033[38;5;155m", x, "\033[39m")
}
prem <- function(x) {
  if (x == "") {
    return(x)
  }
  paste0("\033[38;5;242m", x, "\033[39m")
}
pnew <- function(x) {
  if (x == "") {
    return(x)
  }
  paste0("\033[38;5;255m", x, "\033[39m")
}

## quick functions for horizontal spaces and bars
hbar0 <- function() "\u2592"
hbar1 <- function() "\u2588"
hbarr <- function() "\u2593"

hbars0 <- function(n) if (n > 0) paste(rep(hbar0(), n), collapse = "") else ""
hbars1 <- function(n) if (n > 0) paste(rep(hbar1(), n), collapse = "") else ""
hbarsr <- function(n) if (n > 0) paste(rep(hbarr(), n), collapse = "") else ""

hspace <- function() " "
hspaces <- function(n) if (n > 0) paste(rep(hspace(), n), collapse = "") else ""

## format elements
format_secs <- function(secs = NULL) {
  if (is.null(secs)) {
    return("      ")
  }
  secs <- if (secs > 129599) {
    sprintf(" %3dd ", as.integer(secs / 86400))
  } else if (secs > 3599) {
    sprintf(" %3dh ", as.integer(secs / 3600))
  } else if (secs > 90) {
    sprintf(" %3dm ", as.integer(secs / 60))
  } else if (secs > 0) {
    sprintf(" %3ds ", as.integer(secs))
  } else {
    "   0s "
  }
  pdone(secs)
}

format_prop <- function(prop, width) {
  done <- round(prop * width)
  left <- width - done
  sprintf("%s%s%s",
    pdone(hbars1(done - 1)),
    pnew(hbarsr(as.integer(done - 1 > 0))),
    prem(hbars0(left)))
}

## adjust width given width of secs
calc_width <- function(width = NULL, secs = 6L) {
  if (is.null(width)) {
    width <- getOption("width", 80)
  }
  width - secs
}

## build the actual bar
pb_build_msg <- function(prop, width = NULL, secs = NULL) {
  paste0("\r",
    format_prop(prop, calc_width(width)),
    format_secs(secs),
    "")
}