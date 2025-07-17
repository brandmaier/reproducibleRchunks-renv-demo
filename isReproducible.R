

#' @title Get the total number of failed reproduction attempts
#'
#' @param envir Environment to retrieve data from. This defaults to an internal package namespace.
#'
#' @returns Returns the number of errors encountered when reproducing a Markdown document
#' @export
get_num_reproducibility_errors <- function(envir=.cache) {
  num_errors <- get0(x = "repror_error_counter", envir=envir)
  if (is.null(num_errors)) num_errors <- 0
  return(num_errors)
}


#' @title Get a summary about all reproduction attempts
#' @description This function returns a data frame, in which details
#' about reproduction attempts are collected. The data frame has
#' three columns named "Chunk","Variable", and "Success". Every row
#' in the data frame corresponds to one variable, for which reproducibility
#' was tested. `Chunk` stores the name of the surrounding chunk, `Variable`
#' stores the name of the variable, and `Success` is a boolean variable,
#' which indicates whether the reproduction attempt was successful.
#'
#' @param envir Environment to retrieve data from. This defaults to an internal package namespace.
#'
#' @returns Returns a data.frame with three columns.
#' @export
get_reproducibility_summary <- function(envir=.cache) {
  get0(x = "repror_summary", envir=envir)
}



#' @export
#' @title Test reproducibility of an R Markdown file
#'
#' @param filename Character. An R Markdown file to check for reproducibility
#' @param resetOptions Boolean. Should all package options be reset to defaults? TRUE by default. This avoids problems if multiple checks on multiple documents with varying options are made in a row
#' @param \ldots Optional arguments passed down to \code{rmarkdown::render()}
#'
isReproducible <- function(filename, resetOptions = TRUE, run_pandoc = FALSE,
                           ...)
{
  if (!file.exists(filename)) stop("File does not exist")
  if (!endsWith(tolower(filename),"rmd")) warning("Possibly not an Rmd file")

  if (resetOptions) {
    ids <- (startsWith(names(options()),"reproducibleRchunks."))
    opnames <- names(options())[ids]
    for (name in opnames)    options(structure(list(NULL), names = name))
  }

 # .reset()

  rmarkdown::render(input = filename,
                    quiet = TRUE,
                    runtime = "static",
                    run_pandoc = run_pandoc,
                    ...)

  if (nrow(get_reproducibility_summary()) == 0) {
    message("No reproducibility information found! Could not evaluate reproducibility status.")
    return(NA)
  }

  return(all(get_reproducibility_summary()$Success))

}
