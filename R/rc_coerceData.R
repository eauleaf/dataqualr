#' coerceData
#'
#' @param df Data object (any object that can be coerced to a data frame)
#' @return A data frames, which is DO as data.frames
coerceData <- function(df)
{

  # Simple case when we have a data frame
  if (is.data.frame(df) & length(class(df)) == 1) {
    return(df)
  }

  # Otherwise we have to coerce df
  message('Coercing input data to data.frame')
  tryCatch({
    df <- as.data.frame(df)
    }, warning = function(w) {
        print(paste0('WARNING: ', w))
      }, error = function(e) {
        stop(paste0('ERROR:', e))
      }
  )


  # If the input is a vector, the coercion works, but results in a dataframe with column names
  # matching the name of the vector

  if(is.vector(df) && ncol(df)){
    message('Detected vector input - renaming columns')

    names(df) <- c('VectorCol')
  }


  return(df)


}
