#'Takes raw info for meta and adds it to the review object
#'
#'@param x List of class 'meta' with data related to meta
#'@param compObj dataReviewRobject to be appended
#'@return \code{compObj} dataReviewRobject updated with meta block
updateReviewObject.meta <- function(x, compObj){
  
  compObj$meta <- list()
  
  #print(x$args)
  
  # One-for-one x to compObj additions
  compObj$meta$args <- x$args
  compObj$meta$runTimestamp <- x$runTimestamp
  
  # Info about the data
  #     If the arguments don't have these labels, 
  #     these will be 'character(0)'
  compObj$meta$df <- metaDataInfo(as.character(x$args$df), x$df)
  
  # Object version for future compatibility handling
  compObj$meta$objVersion <- currentObjVersion()
  
  compObj$meta$roundDigits <- x$roundDigits
  
  return(compObj)
}

#' Creates a list of info about the dataframe.
#' 
#' @param name The variable name of the df from the dataReviewR function call
#' @param df A data frame
#' @return \code{dfInfo} A list of info about the data frame
metaDataInfo <- function(name, df){
  
  dfInfo <- list()
  dfInfo$name <- name
  # Removed summary as it was quite slow for large datasets
  #dfInfo$summary <- summary(df)
  dfInfo$rows <- nrow(df)
  dfInfo$cols <- ncol(df)
  return(dfInfo)
}
