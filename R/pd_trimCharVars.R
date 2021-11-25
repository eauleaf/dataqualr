#' trimCharVars: trim white spaces in character variables from an input dataframe
#'
#' @param df Input dataframe
#' @return \code{df} with preceding and trailing white spaces removed from character fields
trimCharVars<-function(df){

  df<-data.frame(sapply(df, function(x) if (collapseClasses(x)=="character") {trimws(x, which = "both")} else {x}, simplify= FALSE), stringsAsFactors=FALSE)

  return(df)
}
