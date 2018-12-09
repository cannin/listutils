#' Convert (simplify) data.frames with list of vectors to single entries 
#' 
#' @param df a data.frame 
#' @param delimiter a delimiter between list items; default: | (vertical bar)
#' 
#' @return a new data.frame 
#' 
#' @examples 
#' b <- list() 
#' b[[1]] <- c("a","b","c")
#' b[[2]] <- c("d","e","f")
#' b[[3]] <- c("g","h","i")
#' df <- data.frame(a=c(1,2,3), b=I(b), stringsAsFactors=FALSE)
#' convertDataFrameListOfVectors(df, ";")
#'   
#' @concept listutils
#' @export
convertDataFrameListOfVectors <- function(df, delimiter="|") {
  newDf <- df 
  
  for(col in colnames(newDf)) {
    if(is.list(newDf[, col])) {
      newDf[, col] <- sapply(newDf[, col], function(x) { paste(x, collapse=delimiter) })
    }
  }
  
  return(newDf)
}


