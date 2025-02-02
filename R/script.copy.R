 #' Save Copy of the Current Script in RStudio
#'
#' This function saves a copy of the current script in RStudio. By default, a
#' folder called \code{_R_Script_Archive} will be created to save the copy of
#' the current R script with the current date and time into the folder. Note that
#' the current R script needs to have a file location before the script can be
#' copied.
#'
#' @param file          a character string naming the file of the copy without
#'                      the file extension \code{".R"}. By default, the file of
#'                      the copy has the same name as the original file.
#' @param folder        a character string naming the folder in which the file
#'                      of the copy is saved. If \code{NULL}, the file of the
#'                      copy is saved in the same folder as the original file.
#'                      By default, the file of the copy is saved into a folder
#'                      called \code{"_R_Script_Archive"}.
#' @param create.folder logical: if \code{TRUE} (default), folder(s) specified in
#'                      the \code{file} argument is created. If \code{FALSE} and
#'                      the folder does not exist, then a error message is printed
#'                      on the console.
#' @param time          logical: if \code{TRUE} (default), the current time is
#'                      attached to the name of the file specified in the argument
#'                      \code{file}.
#' @param format        a character string indicating the format if the
#'                      \code{POSIXct} class resulting from the \code{Sys.time}
#'                      function. The default setting provides a character string
#'                      indicating the year, month, day, minutes, and seconds. See
#'                      the help page of the \code{\link{format.POSIXct}} function.
#' @param overwrite     logical: if \code{TRUE} (default) an existing destination
#'                      file is overwritten.
#' @param check         logical: if \code{TRUE} (default), argument specification
#'                      is checked.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{script.new}}, \code{\link{script.close}}, \code{\link{script.close}},
#' \code{\link{script.open}}, \code{\link{script.save}}, \code{\link{setsource}}
#'
#' @references
#' Ushey, K., Allaire, J., Wickham, H., & Ritchie, G. (2023). \emph{rstudioapi:
#' Safely access the RStudio API}. R package version 0.15.0
#' https://CRAN.R-project.org/package=rstudioapi
#'
#' @note
#' This function uses the \code{getSourceEditorContext()} function in the
#' \pkg{rstudioapi} package by Kevin Ushey, JJ Allaire, Hadley Wickham, and Gary
#' Ritchie (2023).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1: Save copy current R script into the folder '_R_Script_Archive'
#' script.copy()
#'
#' # Exmample 2: Save current R script as 'R_Script.R' into the folder 'Archive'
#' script.copy("R_Script", folder = "Archive", time = FALSE)
#' }
script.copy <- function(file = NULL, folder = "_R_Script_Archive", create.folder = TRUE,
                        time = TRUE, format = "%Y-%m-%d_%H%M", overwrite = TRUE, check = TRUE) {

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("create.folder", "time", "overwrite"),
               character = list(file = 1L, folder = 1L),
               package = "rstudioapi", envir = environment(), input.check = check)

  #_____________________________________________________________________________
  #
  # Argument -------------------------------------------------------------------

  #...................
  ### File name ####

  # File path
  path <- rstudioapi::getSourceEditorContext()$path

  # File name
  if (isTRUE(is.null(file))) {

    file.name <- gsub(".R", "", rev(unlist(strsplit(path, "/")))[1L])

  } else {

    file.name <- file

  }

  #...................
  ### Time ####

  if (isTRUE(time)) {

    file.time <- format(Sys.time(), format = format)

  } else {

    file.time <- NULL

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Create Folder ####

  if (isTRUE(create.folder)) {

    folder.i <- NULL
    folder.split <- unlist(strsplit(folder, "/"))
    for (i in folder.split) {

      folder.i <- paste0(c(folder.i, i), collapse = "/")

      if (isTRUE(!file.exists(folder.i))) { dir.create(folder.i) }

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Save Copy ####

  invisible(file.copy(path, to = paste0(folder, ifelse(!is.null(folder), "/", ""), file.name, ifelse(!is.null(file.time), "_", ""), file.time, ".R"), overwrite = overwrite))

}

#_______________________________________________________________________________
