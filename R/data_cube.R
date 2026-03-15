.data = chl_i1
.key = c("code", "data_id")


new_data_cube <- function(.data, .key) {
  stopifnot(is.data.frame(.data) )
  structure( tibble:::tibble(!!!.data), class = "data_cube", hi = 3:5)
}

validate_data_cube <- function() {

}

data_cube


dc_mutate <- function(.cube, ..., .tracker_callback) {

}

usethis::use_github(visibility = "public")
library(sloop)
sum.POSIXct

s3_dispatch( print(new_data_cube(chl_i1, "data_id") ) )
