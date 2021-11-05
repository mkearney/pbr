
#' Progress bar
#'
#' Create a progress bar object
#'
#' @param iterations Number of total expected iterations
#' @param width Desired width of the progress bar (including text)
#' @param min_width Desired minimum width
#' @param max_width Desired max width
#' @examples
#' ## define test function with a for loop
#' pb_test <- function(n = 50, secs = 3) {
#'   pb <- pbr(n)
#'   on.exit(pb$done(), add = TRUE)
#'   for (i in seq_len(n)) {
#'     Sys.sleep(secs / n)
#'     pb$tick()
#'   }
#' }
#' ## execute test function
#' pb_test()
#' @return A progress bar list with tick and done functions
#' @export
pbr <- function(
  iterations = 10,
  width = NULL,
  min_width = 10,
  max_width = 1000L
) {
  ## create progress bar environment for storing params
  .pb <- as.environment(
    list(
      i = 0L,
      width = get_width(width, min_width, max_width),
      N = iterations,
      init = as.integer(Sys.time() - 0L)
    )
  )
  ## initialize progress bar
  cat(pb_build_msg(prop = 0, width = width))
  ## return list with tick() and done() functions
  list(
    tick = function() {
      N <- get("N", envir = .pb)
      i <- (get("i", envir = .pb) + 1L) %>>% N
      assign("i", i, envir = .pb)
      secs_elp <- as.integer(Sys.time()) - get("init", envir = .pb)
      cat(pb_build_msg(
        prop = i / N,
        width = get("width", envir = .pb),
        secs = as.integer(((secs_elp / i) %>>% 1) * N - secs_elp)
      ))
    },
    done = function() {
      cat(pb_build_msg(
        prop = 1,
        width = get("width", envir = .pb),
        secs = NULL
      ))
      cat(paste0("\r", hspaces(getOption("width")), "\r"))
    }
  )
}
