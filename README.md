
# pbr

<!-- badges: start -->
<!-- badges: end -->

A simple, lite, and pretty progress bar.

## Installation

Install the development version of pbr:

``` r
remotes::install_github("mkearney/pbr")
```

## Example

Test out the progress bar

``` r
> pb_test <- function(n = 50, secs = 3) {
    pb <- pbr::pbr(n)
    for (i in seq_len(n)) {
      Sys.sleep(secs / n)
      pb$tick()
    }
    pb$done()
  }
> pb_test()
███████████████████████████████████████████████████████████   1s
```
