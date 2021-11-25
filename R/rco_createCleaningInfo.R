#' Converts cleaning info into a format consumable by updateReviewObject.
#' 
#' @param revObj dataReviewRobject to be updated
#' @param cleaningInfo list of cleaning information
#' @return \code{revObj} updated dataReviewRobject
#' 
createCleaningInfo <- function(revObj, cleaningInfo){
  # cleanInfoObj <- as.list(getCoercions(cleaningInfo$data_types))
  data_types <- as.list(getCoercions(cleaningInfo$data_types))
  data_names <- as.list(cleaningInfo$data_names)
  
  cleanInfoObj <-  list()
  cleanInfoObj$data_types <- data_types
  cleanInfoObj$data_names <- data_names

  class(cleanInfoObj) <- c("cleaninginfo", "list")
  
  
  #class(cleanInfoObj) <- c("cleaninginfo")
  return(updateReviewObject(cleanInfoObj, revObj))
}

#' Subsets on the variables that have a coercion. 
#' 
#' @param typesDf Dataframe of type information from the executeCoercion function
#' @return \code{coercedT} Subset version of typesDf where a coercion occurred
getCoercions <- function(typesDf){
  typesDfT <- as.data.frame(t(typesDf), row.names=names(typesDf))
  typesDfT <- cbind(names(typesDf),typesDfT, stringsAsFactors = FALSE)
  names(typesDfT) <- c("colName","orig","new")
  coercedCols <- dplyr::filter(typesDfT, as.character(typesDfT[,"orig"]) != 
                          as.character(typesDfT[,"new"]))
  coercedT <- as.data.frame(t(coercedCols[,-1]))
  names(coercedT) <- as.vector(coercedCols[,1])
  return(coercedT)
}


#' variables new and old names
#' 
#' @param namesDF Dataframe of type information from the executeCoercion function
#' @return \code{coercedT} 
getNames <- function(namesDF){
  namesDFT <- as.data.frame(t(namesDF), row.names=names(namesDF))
  namesDFT <- cbind(namesDFT$dfNamesNew,namesDFT, stringsAsFactors = FALSE)
  names(namesDFT) <- c("colName","orig","new")
  coercedCols <- dplyr::filter(namesDFT, as.character(namesDFT[,"orig"]) != 
                                 as.character(namesDFT[,"new"]))
  coercedT <- as.data.frame(t(namesDFT[,-1]))
  names(coercedT) <- as.vector(namesDFT[,1])
  return(coercedT)
}
