#' Create distribution and frequency data for each column of a data frame
#' @description Create distribution and frequency data for each column of a data frame and produce a dataReviewR object containing
#' details of the frequency of variables and summary stats for each column of the data. See \code{vignette("dataReviewR")} for more details.
#' @family dataqualr.functions
#' @param df data frame. The data object. dataReviewR will attempt to coerce all data objects to data frames.
#' @param roundDigits Integer. If NA, numerics are not rounded before comparison. If specified, numerics are
#'                    rounded to the specified number of decimal places using \link[base]{round}.
#' @param trimChars Boolean. If true, strings and factors have whitespace trimmed before comparison.
#' @param clean_var_names Boolean.
#'
#' @return An dataReviewR object.
#'          An S3 object containing details of the comparison between the two data objects. Can be used with \link{summary},
#'          \link{print} and \link{saveReport}
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select_
#' @importFrom dplyr arrange
#' @importFrom dplyr arrange_
#' @importFrom dplyr funs
#' @importFrom dplyr sample_n
#' @importFrom dplyr inner_join
#' @importFrom dplyr distinct
#' @importFrom utils head
#' @importFrom utils tail
#' @importFrom utils capture.output
#' @importFrom utils packageVersion
#' @importFrom stringr str_c
#' @importFrom magrittr %>%
#'
#' @export
rReview <- function(df, roundDigits = NA, trimChars = FALSE, clean_var_names = TRUE) {

  message('Running rReview...')

  # Get args
  argsIn <- match.call()

  # Validate arguments
  validateArguments(roundDigits = roundDigits, coerceCols =  trimChars)
  checkNA(df)

  # Make syntactically valid names
  df <- makeValidNames(df)

  # Coerce data
  coercedData <- coerceData(df)

  # Warn if data is large
  warnLargeData(as.double(nrow(coercedData)),
                as.double(ncol(coercedData)))

  # Round data if needed
  if(!is.na(roundDigits)) {
    coercedData <- rounddf(coercedData, roundDigits)
  }

  # Call process flow
  outObj <- processFlow(df = coercedData, roundDigits, trimChars, clean_var_names, argsIn)

  return(outObj)

}

#' Warn users if the calculation is likely to be slow
#'
#' @description Checks if there are more than 20E6 elements for comparison. If there are, spits out a warning
#' message that the calculation may run slowly
#'
#' @param nrow_df number of rows in data frame
#' @param ncol_df number of columns in data frame

#'
#' @return Nothing
warnLargeData <- function(nrow_df, ncol_df) {

  # Check for total number of cells
  totalSize <- as.double(nrow_df)*as.double(ncol_df)

  # If this is too large, warn the user...
  if(totalSize > 20E6) {
    message(paste0("CAUTION - There are ", totalSize, " elements in the data frame.",
                   "dataReviewR may take a little longer than usual for large data sizes."))
  }

}


