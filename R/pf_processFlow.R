#' processFlow Handles the process flow for the whole package
#'
#' @param df Dataframe. The data frame to be reviewed
#' @param roundDigits Integer. If NA, numerics are not rounded before comparison. If /code{roundDigits} is specified, numerics are
#'                    rounded to /code{roundDigits} decimal places using \link[base]{round}.
#' @param trimChars Boolean. Do we trim characters before comparing?
#' @param clean_var_names Boolean.
#' @param argsIn The arguments that were passed to the main data_reviewer R function
#'
#' (without producing a dataCompareR object). Designed to improve performance for large datasets.
#' @return \code{dataCompareRObject} containing details of the comparison
#'
processFlow <- function(df, roundDigits, trimChars, clean_var_names, argsIn) {


  # 1. Create an r review object
  rRevObj <- createReviewObject()

  # 2. Validate data
  validateData(df)

  # 3. Create meta and update
  rRevObj <- createMeta(rRevObj, df, argsIn, timestamp = Sys.time(), roundDigits)

  # 4. Prepare data, identify key columns, and create skim on updated data
  preparedData <- prepareData(df, trimChars, clean_var_names)

  # 5. createCleaning and update
  rRevObj <- createCleaningInfo(rRevObj,cleaningInfo = preparedData$coerceData)

  # 6. identify key columns, and create skim on updated data
  rRevObj <- createSkimInfo(rRevObj, cleaned_df = preparedData$coerceData$df)

  # 7. create freq data and update
  rRevObj <- createFreq(rRevObj, preparedData$coerceData$df)


  return(rRevObj)

}



