#' Generic function for updating a review object with 
#' information passed to it, that has methods based on the class
#' of the info argument.
#' 
#' @param x Object of information with classes related to the relevant section of the dataReviewRobject
#' @param revObj dataReviewRobject to be updated
#' @return revObj Updated dataReviewRobject
updateReviewObject <- function(x, revObj){
  UseMethod("updateReviewObject", x)
}

