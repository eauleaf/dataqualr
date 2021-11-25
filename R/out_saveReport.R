#' Save a report based on a dataReviewR object
#' @description Saves R markdown and HTML reports in the area specified by the user.
#'
#' Uses knitr and markdown to create reports. Reports have the extensions .Rmd or .html.
#' By default the \code{table.css} style sheet is used for format the html output.
#'
#' @family dataQual.functions
#' @param reviewObject a dataReviewR object.
#' @param reportLocation String. Location to save reports specified by the user. The R markdown and (optionally) HTML reports will
#' be saved in this area
#' @param reportName String. The name of the report. Reports will be saved as reportName.Rmd and (optionally) reportName.html in
#' \code{reportLocation}
#' @param HTMLReport Boolean. Option to output html report.
#' @param showInViewer Boolean. Does the html report open automatically in the viewer?
#' @param stylesheet String. Optional link to customised css stylesheet
#' @param printAll Boolean. If TRUE, all mis-matches in the data are printed to the file. This acts as a shortcut
#' @param head_tail_n Integer. The number of rows to keep in the head and tail in the output sample data
#' @param max_table_length Integer. The maximum number of most frequent entries to show in output summaries
#' to get all mismatches in the report, reviewd to passing the number in \code{mismatchCount}. When TRUE, overrides the
#' \code{mismatchCount} field passed via ellipses
#' @param ... Optional arguments which will be passed to \code{summary}, for example \code{mismatchCount}
#' @import knitr
#' @import markdown
#' @export
saveReport <- function(reviewObject, reportName, reportLocation = '.', HTMLReport= TRUE,
                       showInViewer = TRUE, stylesheet = NA, printAll = FALSE,
                       head_tail_n = 20, max_table_length = 50, ...) {


    # Check that head_tail_n is numeric
  if (is.numeric(head_tail_n) & head_tail_n%%1!=0)  {
    stop("ERROR: head_tail_n must be an integer")
  }

  # make sure that max_table_length is an integer
  max_table_length <- as.integer(max_table_length)
  # Check that max_table_length is numeric
  if (is.numeric(max_table_length) & max_table_length%%1!=0)  {
    stop("ERROR: max_table_length must be an integer")
  }


  # Argument checkin
  if(!is.dataReviewRobject(reviewObject)) {
    stop("Invalid review object")
  }

  if(!file.exists(reportLocation)) {
    stop("Invalid reportLocation")
  }

  if(!is.logical(HTMLReport)) {
    stop("HTMLReport must be T/F")
  }

  if(!is.logical(showInViewer)) {
    stop("showInViewer must be T/F")
  }

  if(!is.character(reportName)) {
    stop("Report name must be a character")
  }

  if(length(reportName)!=1) {
    stop("Report name must be a single character")
  }

  # may get rid of this
  if(!is.logical(printAll)) {
    stop("printAll must be T/F")
  }

  # Determine where the stylesheet is coming from
  # and if custom stylesheet exists
  if(is.na(stylesheet)) {
    message('Using default stylesheet')
    stylesheetToUse <- system.file("css", "table.css", package = "dataQual")
  } else {
    if(file.exists(stylesheet)) {
      message(paste0('Using custom stylesheet at ',stylesheet ))
      stylesheetToUse <- stylesheet
    } else {
      stop(paste0("Cannot find stylesheet at ", stylesheet))
    }
  }

  # Create file locations for saving
  RmdLocn <- file.path(reportLocation, paste0(reportName, '.Rmd'))
  HTMLLocn <- file.path(reportLocation, paste0(reportName, '.html'))
  MdLocn <- file.path(reportLocation, paste0(reportName, '.md'))

  # If printAll is TRUE, set the number of rows to pass to reviewObject
  if(printAll) {
    noRowsToPrint <- if_else(reviewObject$meta$df$rows < max_table_length, reviewObject$meta$df$rows, max_table_length)
    summaryReviewObject <- summary(reviewObject, mismatchCount = noRowsToPrint)
  } else {
    summaryReviewObject <- summary(reviewObject, ...)
  }

  # Create R markdown file from summary.dataReviewRobject function

  capture.output(createReportText(x=summaryReviewObject), file=RmdLocn)

  # Compile a report based on the R markdown file
  if (HTMLReport==TRUE){
    message('Producing HTML output')
    # Force a 2 step process to avoid the .md file being in the working directory
    knit(input = RmdLocn, output = MdLocn)
    markdown::markdownToHTML(file = MdLocn, output = HTMLLocn, stylesheet= stylesheetToUse )
  }

  if (HTMLReport == FALSE & showInViewer == TRUE) {
    message('Cannot display in viewer if HTML report is not enabled')
  }
  else {
    # Display this to user
    if(showInViewer) {

      viewer <- getOption("viewer")
      # RSTUDIO
      if (!is.null(viewer)) {
        viewer(HTMLLocn)
      }
      # OTHER
      else {
        utils::browseURL(HTMLLocn)
      }
    }
  }


}
