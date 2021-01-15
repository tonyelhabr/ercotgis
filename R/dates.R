
#' @noRd
.date_diff_frac <- function(d1, d2, unit) {
  {d1 - d2} %>% lubridate::time_length(unit = unit)
}

#' @noRd
.date_diff <- function(d1, d2, unit) {
  {d1 - d2} %>%
    lubridate::time_length(unit = unit) %>%
    round() %>%
    as.integer()
}


# `%m+%` <- lubridate::`%m+%`
# `%m-%` <- lubridate::`%m-%`
#' @importFrom lubridate `%m+%`
#' @noRd
.add_months <- function(x, n = 1) {
  x %m+% lubridate::period(n, unit = 'month')
}

#' @importFrom lubridate `%m-%`
#' @noRd
.subtract_months <- function(x, n = 1) {
  x %m-% lubridate::period(n, unit = 'month')
}

