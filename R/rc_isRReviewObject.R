#' Check object is of class dataReviewRobject
#' 
#' @param x An object
#' @return A boolean: TRUE if object is class dataReviewRobject and FALSE if not 

is.dataReviewRobject <- function(x) inherits(x, "dataReviewRobject")


