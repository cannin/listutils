#' Convert data.frame to a list of vectors
#' 
#' @param df a data.frame where some entries may be delimited
#' @param delimiter a delimiter between entries; default: ";"
#'
#' @return a list entries converted to data.frame with lists of vectors 
#' 
#' @examples 
#' "TODO"
#'   
#' @concept listutils
#' @export
convertToDataFrameWithListOfVectors <- function(df, delimiter=";") {
    tmp <- df
    for(col in colnames(tmp)) {
        tmp[, col] <- I(strsplit(tmp[, col], delimiter))
    }
    
    return(tmp)
}
