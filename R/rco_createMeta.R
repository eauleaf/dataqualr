#' Takes the raw info for the meta block of the output
#' and puts it in a format usable by the updateReviewObject
#' function
#' 
#' @param dataReviewRobject Object of class dataReviewRobject
#' @param df Data set passed in to the dataReviewR function 
#' @param arguments Collection of arguments passed to Review object with labels that match the dataReviewR arg definitions
#' @param timestamp Timestamp 
#' @param roundDigits The number of digits to round to, using \link[base]{round}
#' @return \code{dataReviewRobject}
#' 
createMeta <- function(dataReviewRobject, df, arguments, timestamp, roundDigits){
  
  # Add meta info to a list
  metaObject <- list()
  metaObject$df <- df
  metaObject$args <- arguments
  metaObject$runTimestamp <- timestamp
  metaObject$roundDigits <- roundDigits
  
  # Change the class of the list to 'meta'
  class(metaObject) <- c("meta", "list")
  
  # Update the Review object and return it
  dataReviewRobject <- updateReviewObject(metaObject, dataReviewRobject)
  return(dataReviewRobject)
}