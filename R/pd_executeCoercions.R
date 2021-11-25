#' executeCoercions:
#'
#' @param df Input dataframe
#' @param whitespace_trim User defined boolean for whether leading/trailing white space is trimmed in strings (TRUE / FALSE)
#' @param clean_var_names Boolean.
#'
#' @return \code{out} list containing 3 data frames df, data_types, data_names
#' @return \code{df} Dataframe with factor fields converted to character type and white space trimming (if option is selected by the user)
#' @return \code{data_types} Dataframe with field types before and after cleaning for both df and DFB
#' @importFrom janitor clean_names
executeCoercions<-function(df, whitespace_trim = TRUE, clean_var_names = TRUE){

  # Store original data names
  dfNamesOrig <- names(df)

  # clean data set names
  if(clean_var_names) {
    df <- df %>% janitor::clean_names()
  }

  # Store new data names
  dfNamesNew <- names(df)

  # Store original data types
  dfTypesOrig <- sapply(df, function(x) class(x))


  # Execute Factor to Character string coercion
  df <- coerceFactorsToChar(df)


  # If whitespace_trim parameter is TRUE, execute white space trimming
  if (whitespace_trim== TRUE){
    df<-trimCharVars(df)
  }

  # Store new data types
  dfTypesNew <-sapply(df, function(x) class(x))

  # Save data name information
  data_names<-data.frame(rbind(dfNamesOrig,dfNamesNew), stringsAsFactors=FALSE)
  names(data_names) <- names(df)

  # Save data type information
  data_types<-data.frame(rbind(dfTypesOrig,dfTypesNew), stringsAsFactors=FALSE)

  # Output list containing dataframes df, data_types, data_names
  out            <-list()
  out$df         <- df
  out$data_types <- data_types
  out$data_names <- data_names
  return(out)

}
