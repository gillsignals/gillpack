#' Split a string
#'
#' @param x string to split (character vector with one element)
#' @param split delimiter to split on (character vector with one element)
#'
#' @returns a character vector with elements matching the original string split by the delimiter
#' @export
#'
#' @examples
#' x <- "alpha,bravo,charlie,delta"
#' strsplit1(x, split = ",)


strsplit1 <- function(x, split) {
    strsplit(x, split = split)[[1]]


}
