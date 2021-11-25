#' Converts skim info into a format consumable by updateReviewObject.
#'
#' @param revObj dataReviewRobject to be updated
#' @param cleaned_df cleaned dataframe returned by prepareDate (the coerceData portion)
#' @importFrom skimr skim
#' @return \code{revObj} updated dataReviewRobject
#'
createSkimInfo <- function(revObj, cleaned_df){

  # create skim information from cleaned df
  df_skim <- skimr::skim(cleaned_df)
  df_nrow <- nrow(cleaned_df)

  # get names of key columns
  df_keys <- get_df_key_names(df_skim, df_nrow)

  # Add skim info to a list
  skimObject <- list()

  skimObject$keys <- df_keys
  skimObject$skim <- df_skim

  # Change the class of the list to 'skim'
  class(skimObject) <- c("skim", "list")

  #  update
  return(updateReviewObject(skimObject, revObj))
}



#' Returns character vector of the dataset columns that appear to be keys (i.e. the column contains unique values)
#'
#' @param df_skim skim output from createSkimInfo
#' @param df_nrow integer. Number of rows in the data set
#' @return \code{key_var_names} Character vector of dataset key columns (i.e. column names where column data is unique)
get_df_key_names <- function(df_skim, df_nrow){
  key_var_names<- df_skim %>% dplyr::as_tibble() %>%
    dplyr::filter_at(dplyr::vars(dplyr::contains("n_unique")), dplyr::any_vars(. == df_nrow)) %>%
    select(skim_variable) %>%
    dplyr::pull()

  return(key_var_names)
}
