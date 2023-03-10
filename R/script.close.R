#' Close R Script in RStudio
#'
#' This function is used to close the current R script in RStudio. Note that by
#' default the function closes the script without saving unless the argument
#' \code{save} is set to \code{TRUE}.
#'
#' The function \code{documentClose()} in the package \pkg{rstudioapi} is used
#' to close the R script.
#'
#' @param save  logical: if \code{TRUE}, the script is saved before closing.
#' @param check logical: if \code{TRUE}, argument specification is checked.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{script.new}}, \code{\link{script.open}}, \code{\link{script.save}},
#' \code{\link{setsource}},
#'
#' @references
#' Ushey, K., Allaire, J., Wickham, H., & Ritchie, G. (2022). rstudioapi: Safely
#' access the RStudio API. R package version 0.14.
#' https://CRAN.R-project.org/package=rstudioapi
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Close current R script file
#' script.close()
#' }
script.close <- function(save = FALSE, check = TRUE) {

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # Check input 'save'
    if (isTRUE(!is.logical(save))) { stop("Please specify TRUE or FALSE for the argument 'save'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Package --------------------------------------------------------------------

  # rstudioapi package
  if (isTRUE(!nzchar(system.file(package = "rstudioapi")))) { stop("Package \"rstudioapi\" is needed for this function, please install the package.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  if (isTRUE(save)) {

    # Path of the current R script
    path.script <- try(dirname(rstudioapi::documentPath()), silent = TRUE)

    # Check
    if (isTRUE(class(path.script) == "try-error")) {

      stop("The current R script does not have a file location yet, please save the R script or set the argument 'save' to 'FALSE'.", call. = FALSE)

    }

  }

  # Close R script
  invisible(rstudioapi::documentClose(save = save))

}
