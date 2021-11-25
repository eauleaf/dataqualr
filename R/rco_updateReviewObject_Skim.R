#'Takes raw info for skim and adds it to the review object
#'
#'@param x List of class 'skim' with data related to skim
#'@param revObj dataReviewRobject to be appended
#'@return \code{revObj} dataReviewRobject updated with skim block
updateReviewObject.skim <- function(x, revObj){
  
  revObj$skim <- list()
  
  # One-for-one x to revObj additions
  revObj$skim$keys <- x$keys
  revObj$skim$skim <- x$skim
  
  
  return(revObj)
}
