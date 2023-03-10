#' Restart R Session
#'
#' This function restarts the RStudio session and is equivalent to using the menu
#' item \code{Session - Restart R}.
#'
#' The function call \code{executeCommand("restartR")} in the package \pkg{rstudioapi}
#' is used to restart the R session. Note that the function \code{restartSession()}
#' in the package \pkg{rstudioapi} is not equivalent to the menu item
#' \code{Session - Restart R} since it does not unload packages loaded during an
#' R session.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
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
#' # Restart the R Session
#' restart()
#' }
restart <- function() {

  #_____________________________________________________________________________
  #
  # Package --------------------------------------------------------------------

  # rstudioapi package
  if (isTRUE(!nzchar(system.file(package = "rstudioapi")))) { stop("Package \"rstudioapi\" is needed for this function, please install the package.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  rstudioapi::executeCommand("restartR")

}
