#' collapseClasses. Collapse the classes of an object to a single string
#'
#' @param x any object
#' @return a string listing the classes of x, separated by commas
collapseClasses <- function(x) {
  return(paste(class(x),collapse = ","))
}

