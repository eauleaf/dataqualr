#' Adds a freq block to the output
#' 
#' @param x List of class 'freq' with column matching info
#' @param revObj dataReviewRobject instance to be updated
#' @return \code{revObj} Updated dataReviewRobject
updateReviewObject.freq <- function(x, revObj){
  revObj$freq <- x
  return(revObj)
} 
