#' CBF lazy bloody list
#'
#' @param listargs a string containing the list items
#' @param split character to split by. If FALSE, a character vector (or object which can be coerced to such) containing regular expression(s) to use for splitting. If empty matches occur, in particular if split has length 0, x is split into single characters. If split has length greater than 1, it is re-cycled along x.
#' @param fixed passed to strsplit. logical. If TRUE match split exactly, otherwise use regular expressions. Has priority over perl.
#'
#' @example
#' cbfit = cbf.c("item1 item2 item3 item4")
#' cbfit
#' length(cbfit)
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

# CBF Mutate Type function family
cbf.coltype <- function(.f) {
  function(.x, cols, ...) {
    .x %>% mutate(across(.cols = all_of(cols), .fns = .f, ...))
  }
}

#' CBF Mutate Type function family
#'
#' This family of functions mutates a named set of columns (as a string input for now) to the desired type.
#' Note that the input is a cbf.c list.
#'
#' @family coltype
#'
#' @param .x A dataframe (tibble) with column to be mutated
#' @param cols A list of columns to mutate
#' @param ... Arguments passed to across
#' @example
#' tibble(x = 1:2, y = 1:2, c = 1:2) %>%
#'   cbf.tofactor("x y") %>%
#'   print() %>%
#'   cbf.tonumeric("x y") %>%
#'   print() %>%
#'   cbf.tocharacter("x y") %>%
#'   print() %>%
#'   cbf.toint("x y")
#' @export
#' @docType
#' @rdname cbf.coltype
#' @name cbf.coltype
NULL

#' CBF Mutate Type Family: to Factor
#'
#'
#' @param .x A dataframe (tibble) with column to be mutated
#' @param .cbfcols A [cbf.c()] or string of column name(s) to mutate
#' @inherit cbf.coltype
#' @family coltype
#' @export
cbf.coltype_factor <- cbf.coltype(as.factor)

#' CBF Mutate Type to numeric
#'
#' @inherit cbf.coltype
#' @family coltype
#' @export
cbf.coltype_numeric <- cbf.coltype(as.numeric)

#' CBF Mutate Type to character
#'
#' @inherit cbf.coltype
#' @family coltype
#' @export
cbf.coltype_char <- cbf.coltype(as.character)

#' CBF Mutate Type to integer
#'
#' @inherit cbf.coltype
#' @family coltype
#' @export
cbf.coltype_int <- cbf.coltype(as.integer)



# // cbf.b_not_in_a()

# // cbf.totalunique()

cbf.reverse_scale <- function(x, scale_max) {
  return(scale_max + 1 - x)
}

# tibble(x = rep(1:5,2)) %>%
#   mutate(reverse = cbf.reverse_scale(x, 5))
