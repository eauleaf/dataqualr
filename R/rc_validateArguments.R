#' validateArguments
#'
#' @param roundDigits Number of decimal places to round numeric data column
#' @param coerceCols Boolean - do we coerce columns names?
#' @param trimChars Boolean. If true, strings and factors have whitespace trimmed before comparison.
#'
#' @return Nothing. Errors if any parameters are invalid.
validateArguments <- function(roundDigits = NA, coerceCols = TRUE, trimChars = TRUE)
  {


  # Check that roundDigits is numeric or NA
  if (is.numeric(roundDigits) | is.na(roundDigits)) {
  } else {
    stop("ERROR: roundDigits must be an integer or NA ")
  }

  # Check that roundDigits is numeric or NA
  if (is.numeric(roundDigits) | is.na(roundDigits)) {
  } else {
    stop("ERROR: roundDigits must be an integer or NA ")
  }

  # Check that coerceCols is boolean
  if (!is.logical(coerceCols))  {
    stop("ERROR: Coerce flag must be Boolean")
  }

  # # Check that maxMismatch is numeric, > 0
  # if (is.numeric(maxMismatch) | is.na(maxMismatch)) {
  # }
  # else {
  #   stop("ERROR: Mismatch must be numeric or NA")
  # }
  #
  # if (is.numeric(maxMismatch) & maxMismatch <= 0)  {
  #   stop("ERROR: Mismatch must be greater than 0")
  # }
  #
  # if (is.numeric(maxMismatch) & maxMismatch%%1!=0)  {
  #   stop("ERROR: Mismatch must be an integer")
  # }

}

#' makeValidNames
#'
#' Correct syntactically invalid names in a data frame
#' @param df A data frame
#' @return A data frame with syntactically valid names
#' @examples
#' \dontrun{makeValidNames(iris)}
makeValidNames <- function(df) {

  # Get make names version of names
  nm <- make.names(names(df), unique = TRUE, allow_ = TRUE)

  if(!all(nm == names(df))) {
    message('Fixing syntactically invalid names')

    message(paste('    Names changed from - ', paste(names(df)[!(nm == names(df))], collapse = ", ")))
    message(paste('                    to - ', paste(nm[!(nm == names(df))], collapse = ", ")))

    names(df) <- nm
  }

  return(df)

}
