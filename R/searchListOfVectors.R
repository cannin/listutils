#' Search List of Vectors
#'
#' @param q query vector
#' @param lst list of vectors to search
#' @param useNames return names rather than indicies; ignored if no names. Default: FALSE
#' @param queryAll query for union of q entries
#'
#' @return a list of vectors with the same length as the query vector, each list
#'   entry will have indicies for lst where there was a match with the query
#'   vector. Return NA if there were no matches.
#'
#' @details
#' Taken from: http://stackoverflow.com/questions/11002391/fast-way-of-getting-index-of-match-in-list
#'
#' @examples
#' lst <- list(1:3, 3:5, 3:7)
#' q <- c(3, 5)
#' results <- searchListOfVectors(q, lst)
#' names(results) <- q
#'
#' lst <- list(LETTERS[1:3], LETTERS[3:5], LETTERS[3:7])
#' q <- c("C", "E")
#' searchListOfVectors(q, lst)
#'
#' lst <- list(LETTERS[3], LETTERS[4:6])
#' q <- "C"
#' searchListOfVectors(q, lst)
#'
#' lst <- list(LETTERS[3], LETTERS[4:6])
#' q <- c("C")
#' searchListOfVectors(q, lst)
#'
#' lst <- list(LETTERS[3], LETTERS[4:6])
#' q <- c("C", "E")
#' searchListOfVectors(q, lst)
#'
#' lst <- list(a=LETTERS[3], b=LETTERS[4:6])
#' q <- c("C", "E")
#' searchListOfVectors(q, lst, useNames=TRUE)
#' 
#' lst <- list(a=LETTERS[3], b=LETTERS[3:6])
#' q <- c("C", "E")
#' searchListOfVectors(q, lst, useNames=TRUE)
#'
#' lst <- list(LETTERS[3], LETTERS[4:6])
#' q <- "Z"
#' searchListOfVectors(q, lst)
#' 
#' lst <- list(a=LETTERS[3], b=LETTERS[3:6])
#' q <- c("C", "E")
#' searchListOfVectors(q, lst, useNames=TRUE, queryAll=TRUE)
#' 
#' lst <- list(a=LETTERS[3], b=LETTERS[3:6])
#' q <- c("C", "E")
#' searchListOfVectors(q, lst, useNames=FALSE, queryAll=TRUE)
#' 
#' #lst <- list(LETTERS[3], LETTERS[3:6])
#' #q <- c("C", "E")
#' #searchListOfVectors(q, lst, useNames=TRUE, queryAll=TRUE)
#'   
#' @concept listutils
#' @export
searchListOfVectors <- function(q, lst, useNames=FALSE, queryAll=FALSE) {
  if(useNames && is.null(names(lst))) {
    stop("ERROR: List is not named")
  }
  
  if(useNames) {
    tmp <- rep(names(lst), sapply(lst, length))
  } else {
    tmp <- rep(seq_along(lst), sapply(lst, length))
  }

  if(queryAll) {
    resultsSe <- tmp[which(unlist(lst) %in% q)]
    idx <- which(table(resultsSe) == length(q))
    
    if(useNames) {
      return(names(idx))
    } else {
      return(unname(idx))
    }
  } else {
    resultsSe <- sapply(q, function(x) tmp[which(unlist(lst) %in% x)], simplify=FALSE)
    
    if(class(resultsSe) != "list") {
      return(NA)
    }
    
    return(resultsSe)
  }
}
