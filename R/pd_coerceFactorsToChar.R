#' coerceFactorsToChar: convert all factor type fields to characters
#'
#' @param df Input dataframe
#' @return \code{df} with factor fields converted to character type
coerceFactorsToChar<-function(df){

df<-data.frame(sapply(df, function(x) if (collapseClasses(x)=="factor") {as.character(x)} else {x}, simplify= FALSE), stringsAsFactors=FALSE)

return(df)
}
