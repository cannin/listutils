#' Convert a data.frame containing lists of vectors to a simple data.frame
#'
#' @param x a data.frame
#' @param sep2 a separator to use between vector entries
#'
#' @return a data.frame
#'   
#' @concept listutils
#' @export
convertListOfVectorsDataFrameToDataFrame <- function(x, sep2) {
  selCols <- sapply(1:ncol(x), function(i) {
    is.list(x[,i])
  }, simplify = TRUE)

  idx <- which(selCols)

  for(i in idx) {
    t1 <- lapply(x[,i], function(x) {
      if(length(x) > 1) {
        y <- paste(x, collapse=sep2)
      } else {
        y <- x
      }
      return(y)
    })

    x[,i] <- unlist(t1)
  }

  return(x)
}

