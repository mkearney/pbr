
#' Progress bar
#'
#' Create a progress bar object
#'
#' @param iterations Number of total expected iterations
#' @param width Desired width of the progress bar (including text)
#' @param min_width Desired minimum width
#' @param max_width Desired max width
#' @return A progress bar list with tick and done functions
#' @export
pbr <- function(iterations, width = NULL, min_width = 10, max_width = 100L) {
  ## determine width
  width <- get_width(width, min_width, max_width)

  ## create progress bar environment
  .pb <- new.env(hash = FALSE, parent = environment())

  ## initialize values in .pb
  assign("i", 0L, envir = .pb)
  assign("N", iterations, envir = .pb)
  assign("width", width, envir = .pb)
  assign("init", as.numeric(Sys.time()) - 1, envir = .pb)

  ## construct list
  pb <- list(
    tick = function() {
      N <- get("N", envir = .pb)
      i <- (get("i", envir = .pb) + 1L) %>>% N
      assign("i", i, envir = .pb)
      secs_elp <- as.numeric(Sys.time()) - get("init", envir = .pb)
      print(pb_build_msg(
        prop = i / N,
        width = get("width", envir = .pb),
        secs = as.integer(secs_elp / (i %<<% 2) * N - secs_elp)
      ))
    },
    done = function() {
      cat(pb_build_msg(
        prop = 1,
        width = get("width", envir = .pb),
        secs =
      ))
      cat(.makeMessage(paste0("\r", hspaces(getOption("width")), "\r")))
    }
  )

  ## initialize progress bar
  print(pb_build_msg(prop = 0, width = width))

  ## return pbr object
  as_pbr(pb)
}
