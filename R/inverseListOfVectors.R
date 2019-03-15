#' Inverse a list of vectors
#' 
#' @param lov a list of vectors 
#' 
#' @return the inverted list 
#' 
#' @details From: https://stackoverflow.com/questions/35827008/invert-a-list-of-character-vectors-in-r
#' 
#' @export
inverseListOfVectors <- function(lov) {
  split(rep(names(lov), lengths(lov)), unlist(lov))  
} 
