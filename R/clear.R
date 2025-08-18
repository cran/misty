#' Clear Console in RStudio
#'
#' This function clears the console equivalent to \code{Ctrl + L} in RStudio on
#' Windows, Mac, UNIX, or Linux operating system.
#'
#' @author
#' Takuya Yanagida
#'
#' @seealso
#' \code{\link{restart}}, \code{\link{setsource}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Clear console
#' clear()
#' }
clear <- function() {

  if (isTRUE(.Platform$GUI == "RStudio")) {

    # Windows
    if (isTRUE(.Platform$OS.type == "windows")) {

      shell("cls")

    # UNIX
    } else if (isTRUE(.Platform$OS.type == "unix")) {

      system('clear')

    # Mac or Linux
    } else {

      shell("clear")

    }

  } else {

    stop("This function can only be used in RStudio.", call. = FALSE)

  }

}

#_______________________________________________________________________________
