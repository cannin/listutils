#' Convert List of Vectors to data.frame 
#' 
#' @param lst a list
#' @param colNames an optional vector of column names for the resulting data.frame
#' @param collapseDelimiter a delimiter to collapse the vector; optional 
#'
#' @examples 
#' lov <- list(v1=c("a", "b"), v2=c("a", "c"), v3=c("a"))
#' convertListOfVectorsToDataFrame(lov)
#' 
#' @return a data.frame
#'   
#' @concept listutils
#' @export
convertListOfVectorsToDataFrame <- function(lst, colNames=NULL, collapseDelimiter=NULL) {
  results <- NULL
  
  for(i in 1:length(lst)) {
    name <- names(lst)[i]
    tmp <- lst[[i]]
    
    if(is.null(collapseDelimiter)) {
        for(j in 1:length(tmp)) {
      		value <- lst[[i]][j]
        	results <- rbind(results, c(name, value))
    	}
	} else {
		value <- paste(tmp, collapse=collapseDelimiter)
		results <- rbind(results, c(name, value))
	}
    

  }
  
  results <- as.data.frame(results, stringsAsFactors=FALSE)
  
  if(!is.null(colNames)) {
    colnames(results) <- colNames
  }
  
  return(results)
}