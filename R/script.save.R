#' Save R Script in RStudio
#'
#' This function is used to save the current or all R scripts in RStudio.
#'
#' The function \code{documentSave()} or \code{documentSaveAll()} in the package
#' \pkg{rstudioapi} is used to save the R script. Note that R scripts need to
#' have a file location before this function can be used.
#'
#' @param all logical: if \code{TRUE}, all scripts opened in RStudio are saved.
#' @param check logical: if \code{TRUE}, argument specification is checked.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{script.close}}, \code{\link{script.new}}, \code{\link{script.open}},
#' \code{\link{setsource}}
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
#' # Save current R script
#' script.save()
#'
#' # Save all R scripts
#' script.save(all = TRUE)
#' }
script.save <- function(all = FALSE, check = TRUE) {

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # Check input 'all'
    if (isTRUE(!is.logical(all))) { stop("Please specify TRUE or FALSE for the argument 'all'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Package --------------------------------------------------------------------

  # rstudioapi package
  if (isTRUE(!nzchar(system.file(package = "rstudioapi")))) { stop("Package \"rstudioapi\" is needed for this function, please install the package.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  # Path of the current R script
  path.script <- try(dirname(rstudioapi::documentPath()), silent = TRUE)

  # Check
  if (isTRUE(class(path.script) == "try-error")) {

    stop("The current R script does not have a file location yet, please save the R script first.", call. = FALSE)

  }

  if (isTRUE(all)) {

    # Save all R scripts
    invisible(rstudioapi::documentSaveAll())

  } else {

    # Save R script
    invisible(rstudioapi::documentSave())

  }

}
