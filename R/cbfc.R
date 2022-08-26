library(tidyverse)

#' CBF making a list
#'
#' @param listargs
#' @param split character vector (or object which can be coerced to such) containing regular expression(s) (unless fixed = TRUE) to use for splitting. If empty matches occur, in particular if split has length 0, x is split into single characters. If split has length greater than 1, it is re-cycled along x.
#'
#' @return
#' @export
#' @import
#'
#' @examples
cbf.c <- function(listargs, split = " ", fixed = T) {

  split_list = strsplit(listargs, split = split, fixed = fixed)

  return(split_list[[1]])
}

cbf.named_list <- function(listargs, name_separator = "=", split = " ", fixed = T) {
  split_list = sapply(strsplit(listargs, split = split, fixed = fixed)[[1]], strsplit, name_separator)
  contents = purrr::map_chr(split_list, purrr::pluck, 2)
  names(contents) = gsub(pattern = "=.*$", replacement = "", x = names(contents))
  return(contents)
}

cbf.mutate_type <- function(.f) {
  function(.x, .cbfcols, ...) {
    .x %>% mutate(across(.cols = cbf.c(.cbfcols), .fns = .f, ...))
  }
}

cbf.tofactor <- cbf.mutate_type(as.factor)
cbf.tonumeric <- cbf.mutate_type(as.numeric)
cbf.tochar <- cbf.mutate_type(as.character)
cbf.toint <- cbf.mutate_type(as.integer)

# tibble(x = 1:2, y = 1:2, c = 1:2) %>%
#   cbf.tofactor("x y") %>%
#   print() %>%
#   cbf.tonumeric("x y") %>%
#   print() %>%
#   cbf.tocharacter("x y") %>%
#   print() %>%
#   cbf.toint("x y")

cbf.b_not_in_a()

cbf.totalunique()

cbf.reverse_scale <- function(x, scale_max) {
  return(scale_max + 1 - x)
}

tibble(x = rep(1:5,2)) %>%
  mutate(reverse = cbf.reverse_scale(x, 5))
