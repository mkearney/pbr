
#' Progress bar
#'
#' Create a progress bar object
#'
#' @param iterations Number of total expected iterations
#' @param verb Character string describing what's happening
#' @param width Desired width of the progress bar (including text)
#' @param min_width Desired minimum width
#' @param max_width Desired max width
#' @return A progress bar list with tick and done functions
#' @export
pbr <- function(iterations, verb = "working", width = NULL, min_width = nchar(verb) + 5, max_width = 100L) {
  ## determine width
  width <- get_width(width, min_width, max_width)

  ## create progress bar environment
  .pb <- new.env()

  ## initialize values in .pb
  assign("verb" , verb, envir = .pb)
  assign("i", 0L, envir = .pb)
  assign("N", iterations, envir = .pb)
  assign("width", width, envir = .pb)
  assign("init", Sys.time() - 1, envir = .pb)

  ## construct list
  pb <- list(
    iterations = iterations,
    init = function() {
      print(pb_build_msg(
        verb = get("verb", envir = .pb),
        prop = 0,
        width = get("width", envir = .pb)
      ))
    },
    tick = function() {
      ## get and update iterator info
      N <- get("N", envir = .pb)
      i <- (get("i", envir = .pb) + 1L) %>>% N
      assign("i", i, envir = .pb)

      ## get and calculate time info
      init <- get("init", envir = .pb)
      secs_elp <- diff_secs(Sys.time(), init)
      secs_per <- secs_elp / (i %<<% 2)
      secs_exp <- secs_per * N
      secs_rem <- as.integer(secs_exp - secs_elp)

      ## build message
      msg <- pb_build_msg(
        verb = get("verb", envir = .pb),
        prop = i / N,
        width = get("width", envir = .pb),
        secs = secs_rem
      )

      ## print message
      print(msg)
    },
    done = function() {
      cat(.makeMessage("\r"))
      cat(pb_build_msg(
        verb = get("verb", envir = .pb),
        prop = 1,
        width = get("width", envir = .pb),
        secs =
      ), fill = TRUE)
    }
  )
  pb$init()
  as_pbr(pb)
}

pb_clear_line <- function(width = getOption("width")) {
  sp <- hspaces(width)
  str <- paste0(c("\r", sp), collapse = "")
  cat(.makeMessage(str))
}

pb_end <- function(width = getOption("width")) {
  str <- "\n"
  cat(.makeMessage(str))
}