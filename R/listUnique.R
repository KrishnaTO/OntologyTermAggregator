#' Get unique list terms
#'
#' Convenience function for converting list of lists into an unique only list.
#'
#' @param list list containing lists which are to be used
#' @export
#'

listUnique <- function(list){
  unique(unlist(list))
}
