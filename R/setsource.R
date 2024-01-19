#' Set Working Directory to the Source File Location
#'
#' This function sets the working directory to the source file location (i.e.,
#' path of the current R script) in RStudio and is equivalent to using the menu
#' item \code{Session - Set Working Directory - To Source File Location}.
#' Note that the R script needs to have a file location before this function can
#' be used.
#'
#' @param path  logical: if \code{TRUE} (default), the path of the source file is
#'              shown on the console.
#' @param check logical: if \code{TRUE}, argument specification is checked.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{script.close}}, \code{\link{script.new}}, \code{\link{script.open}},
#' \code{\link{script.save}}
#'
#' @references
#' Ushey, K., Allaire, J., Wickham, H., & Ritchie, G. (2023). \emph{rstudioapi:
#' Safely access the RStudio API}. R package version 0.15.0
#' https://CRAN.R-project.org/package=rstudioapi
#'
#' @return
#' Returns the path of the source file location.
#'
#' @note
#' This function uses the \code{documentPath()} function in the \pkg{rstudioapi}
#' package by Kevin Ushey, JJ Allaire, Hadley Wickham, and Gary Ritchie (2023).
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Example 1: Set working directory to the source file location
#' setsource()
#'
#' # Example 2: Set working directory to the source file location
#' # and assign path to an object
#' path <- setsource()
#' path
#' }
setsource <- function(path = TRUE, check = TRUE) {

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # rstudioapi package
    if (isTRUE(!nzchar(system.file(package = "rstudioapi")))) { stop("Package \"rstudioapi\" is needed for this function, please install the package.", call. = FALSE) }

    # Check input 'path'
    if (isTRUE(!is.logical(path))) { stop("Please specify TRUE or FALSE for the argument 'path'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  # Path of the current source file
  path.source <- try(dirname(rstudioapi::documentPath()), silent = TRUE)

  # Check
  if (isTRUE(class(path.source) == "try-error")) { stop("The current R script does not have a file location yet, please save the R script first.", call. = FALSE) }

  # Set working directory
  setwd(path.source)

  # Print source file location
  if (isTRUE(path)) { cat(paste0("  ", path.source)) }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  return(invisible(path.source))

}
