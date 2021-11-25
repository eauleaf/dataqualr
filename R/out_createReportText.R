#' createReportText: prepares text which is used in the summary report
#' Saves R markdown and HTML reports in the area specified by the user. Reports are called RreviewReport.Rmd (.html)
#' Uses knitr package to create tables in the markdown (createReportText function) and HTML report.
#'
#' @param x input object which summary comparison information
#' @return text in R markdown format
createReportText <- function(x){
  y <- createTextSummary(x)
  invisible(y)
}
