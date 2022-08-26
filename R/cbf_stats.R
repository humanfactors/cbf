#' Returns mode of a variable
#'
#' This function takes a vector and returns the statistical mode, i.e. the most
#'  common value the vector takes.
#'
#' @param x vector.
#' @return Most common value.
#' @source \url{https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode}
#' @examples
#' stat_mode(ggplot2::mpg$manufacturer)
#' stat_mode(ggplot2::mpg$model)
#'
#' stat_mode(rpois(100, 1))
#' @export
cbf_statmode <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}


#' Cummulative Skip NA
#'
#' cbf.cumsumskipna from https://stackoverflow.com/questions/25576358/calculate-cumsum-while-ignoring-na-values
#' It calculates the cumulative FUNC of x while ignoring NA.
#'
#'
#' @param x a numeric vector
#' @param FUNC FUNC can be any one of sum, prod, min, or max, and x is a numeric vector
#'
#' @return
#' @export
#'
#' @examples
cbf.cumsumskipna <- function(x, FUNC) {
  d <- deparse(substitute(FUNC))
  funs <- c("max", "min", "prod", "sum")
  stopifnot(is.vector(x), is.numeric(x), d %in% funs)
  FUNC <- match.fun(paste0("cum", d))
  x[!is.na(x)] <- FUNC(x[!is.na(x)])
  return(x)
}


# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flatten_corr_matrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
