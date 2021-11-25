#' Generates an empty list of the correct class to store results
#' 
#' @return A list of class dataReviewRObject
#'  

createReviewObject <- function() {
  
  rRevObj <- vector("list")
  class(rRevObj) <- 'dataReviewRobject'
  return(rRevObj)
  
}