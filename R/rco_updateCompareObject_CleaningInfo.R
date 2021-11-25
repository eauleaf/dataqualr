#' Updates cleaning info in the revare object
#' 
#' @param x list of type cleaninginfo with data types
#' @param revObj dataReviewRobject to be updated
#' @return \code{revObj} updated dataReviewRobject
updateReviewObject.cleaninginfo <- function(x, revObj){
  revObj$cleaninginfo <- x
  return(revObj)
}
