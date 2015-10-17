#' Checks if a string is a simple string.
#'
#' A simple string is (a) character, (b) length 1, and (c) not blank or NA.
#'
#' @param string ANY. An R object.
#' @return TRUE or FALSE according it is a simple string.
is.simple_string <- function(string) {
  is.character(string) && length(string) == 1 && nzchar(string) && !is.na(string)
}

#' Checks if a string is a simple string.
#'
#' A logical string is (a) logical, (b) length 1, and (c) not NA.
#'
#' @param string ANY. An R object.
#' @return TRUE or FALSE according it is a simple logical.
is.simple_logical <- function(logical) {
  is.logical(logical) && length(logical) == 1 && !is.na(logical)
}

