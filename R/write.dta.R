#' Write Stata DTA File
#'
#' This function writes a data frame or matrix into a Stata data file.
#'
#' @param x         a matrix or data frame to be written in Stata, vectors are
#'                  coerced to a data frame.
#' @param file      a character string naming a file with or without file extension
#'                  '.dta', e.g., \code{"Stata_Data.dta"} or \code{"Stata_Data"}.
#' @param version   Stats file version to use. Supports versions 8-15.
#' @param label     Sataset label to use, or \code{NULL}. Defaults to the value
#'                  stored in the "label" attribute pf data. Must be <= 80
#'                  characters.
#' @param str.thres any chracter vector with a maximum length greater than
#'                  \code{str.thre} bytes wil be stored as a long string
#'                  \code{strL} instead of a standard string \code{str}
#'                  variable if \code{version} is greater or equal 13.
#' @param adjust.tz this argument controls how the timezone of date-time values
#'                  is treated when writing, see 'Details' in the
#'                  in the \code{\link{write_dta}} function in the \code{havan}
#'                  package.
#' @param check     logical: if \code{TRUE} (default), variable attributes
#'                  specified in the argument \code{var.attr} is checked.
#'
#' @author
#' Hadley Wickham, Evan Miller and Danny Smith
#'
#' @seealso
#' \code{\link{read.dta}}, \code{\link{write.sav}}, \code{\link{write.mplus}},
#' \code{\link{write.xlsx}}
#'
#' @references
#' Wickham H, Miller E, Smith D (2023). \emph{haven: Import and Export 'SPSS',
#' 'Stata' and 'SAS' Files}. R package version 2.5.3.
#' \url{https://CRAN.R-project.org/package=haven}
#'
#' @note
#' This function is a modified copy of the \code{read_dta()} function in the
#' \pkg{haven} package by Hadley Wickham, Evan Miller and Danny Smith (2023).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1: Write data frame 'mtcars' into the State data file 'mtcars.dta'
#' write.dta(mtcars, "mtcars.dta")
#' }
write.dta <- function(x, file = "Stata_Data.dta", version = 14, label = NULL,
                      str.thres = 2045, adjust.tz = TRUE, check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'x' is missing
  if (isTRUE(missing(x))) { stop("Please specify a matrix or data frame for the argument 'x'.", call. = FALSE) }

  # Matrix or data frame for the argument 'x'?
  if (isTRUE(!is.matrix(x) && !is.data.frame(x))) { stop("Please specifiy a matrix or data frame for the argument 'x'.", call. = FALSE) }

  # Check input 'file'
  if (isTRUE(missing(file))) { stop("Please specify a character string indicating the name of the Stata data file for the argument 'file'", call. = FALSE) }

  # File extension .dta
  file <- ifelse(!grepl(".dta", file), file <- paste0(file, ".dta"), file)

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # Package haven installed?
    if (isTRUE(!requireNamespace("haven", quietly = TRUE))) { stop("Package \"haven\" is needed for this function to work, please install it.", call. = FALSE ) }

    # Check input 'version'
    if (isTRUE(version < 8L || version > 15L)) { stop("This function does not support the Stat file version specified in 'version'", call. = FALSE)}

    # Check input 'adjust.tz'
    if (isTRUE(!is.logical(adjust.tz))) { stop("Please specify TRUE or FALSE for the argument 'adjust.tz'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  # Write Stata data file
  haven::write_dta(x, path = file, version = version, label = label, strl_threshold = str.thres,
                   adjust_tz = adjust.tz)

}
