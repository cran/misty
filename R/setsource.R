#' Set Working Directory to the Source File Location
#'
#' This function is used to set the working directory to the source file location
#' (i.e., path of the current R script) in RStudio and is equivalent to using the
#' menu item \code{Session - Set Working Directory - To Source File Location}.
#'
#' The function \code{documentPath()} in the package \pkg{rstudioapi} is used to
#' retrieve the path of the source file. Note that the R script needs to have a
#' file location before this function can be used to set the working directory
#' to the source file location.
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
#' Ushey, K., Allaire, J., Wickham, H., & Ritchie, G. (2022). rstudioapi: Safely
#' access the RStudio API. R package version 0.14.
#' https://CRAN.R-project.org/package=rstudioapi
#'
#' @return
#' Returns the path of the source file location.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Set working directory to the source file location
#' setsource()
#'
#' # Set working directory to the source file location
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

    # Check input 'path'
    if (isTRUE(!is.logical(path))) { stop("Please specify TRUE or FALSE for the argument 'path'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Package --------------------------------------------------------------------

  # rstudioapi package
  if (isTRUE(!nzchar(system.file(package = "rstudioapi")))) { stop("Package \"rstudioapi\" is needed for this function, please install the package.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  # Path of the current source file
  path.source <- try(dirname(rstudioapi::documentPath()), silent = TRUE)

  # Check
  if (isTRUE(class(path.source) == "try-error")) {

    stop("The current R script does not have a file location yet, please save the R script first.", call. = FALSE)

  }

  # Set working directory
  setwd(path.source)

  # Print source file location
  if (isTRUE(path)) { cat(paste0("  ", path.source)) }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  return(invisible(path.source))

}
