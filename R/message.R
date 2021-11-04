
## add print colors to text
pdone <- function(x) {
    paste0("\033[38;5;155m", x, "\033[39m")
}
prem <- function(x) {
    paste0("\033[38;5;244m", x, "\033[39m")
}

## quick functions for horizontal spaces and bars
hbar0 <- function() "\u2588"
hbar1 <- function() "\u2588"
hbars0 <- function(n) if (n > 0) paste(rep(hbar0(), n), collapse = "") else ""
hbars1 <- function(n) if (n > 0) paste(rep(hbar1(), n), collapse = "") else ""
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
  pdone(secs)
}

format_prop <- function(prop, width) {
  done <- round(prop * width)
  left <- width - done
  sprintf("%s%s", pdone(hbars1(done)), prem(hbars0(left)))
}

## adjust width given width of secs
calc_width <- function(width = NULL, secs = 5L) {
  if (is.null(width)) {
    width <- getOption("width", 80)
  }
  width - secs
}

as_pbr_msg <- function(x) structure(x, class = "pbr_msg")
as_pbr <- function(x) structure(x, class = c("pbr", "list"))

print.pbr_msg <- function(x, ...) {
  cat(.makeMessage(x))
  cat(.makeMessage("\r"))
}

#' @export
print.pbr <- function(x, ...) {
  cat(sprintf(
    "# A pbr progress bar (N = %d)", x$iterations), fill = TRUE)
  cat("$tick() = iterate\n$done() = complete\n")
}


## build the actual bar
pb_build_msg <- function(prop, width = NULL, secs = NULL) {
  as_pbr_msg(
    paste0(
      format_prop(prop, calc_width(width)),
      format_secs(secs)
    )
  )
}