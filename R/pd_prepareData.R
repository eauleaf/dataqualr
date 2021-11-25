#' prepareData Prepares data for review summaries in 1 stage.
#'             1. coerce data
#'
#' @inheritParams rReview
#'
#' @return \code{dataReviewRObject} containing details of the comparison
prepareData <- function(df, trimChars = TRUE, clean_var_names = TRUE) {

  # ----- 1. Coercions -----
  coerceData <- executeCoercions(df,trimChars, clean_var_names)

  toReturn <- list()

  toReturn$coerceData <- coerceData


  return(toReturn)

}
