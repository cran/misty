#' Write Mplus Data File
#'
#' This function writes a matrix or data frame to a tab-delimited file without
#' variable names, a Mplus input template, and a text file with variable names.
#' Note that only numeric variables are allowed, i.e., non-numeric variables will
#' be removed from the data set. Missing data will be coded as a single numeric
#' value.
#'
#' @param x      a matrix or data frame to be written to a tab-delimited file.
#' @param file   a character string naming a file with or without the file extension
#'               '.dat', e.g., \code{"Mplus_Data.dat"} or \code{"Mplus_Data"}.
#' @param data   logical: if \code{TRUE} (default), Mplus data file is written in a
#'               text file named according to the argument\code{file}.
#' @param input  logical: if \code{TRUE} (default), Mplus input template is written
#'               in a text file named according to the argument\code{file} with
#'               the extension \code{_INPUT.inp}.
#' @param var    logical: if \code{TRUE}, variable names are written in a text file
#'               named according to the argument\code{file} with the extension
#'               \code{_VARNAMES.txt}.
#' @param na     a numeric value or character string representing missing values
#'               (\code{NA}) in the data set.
#' @param check  logical: if \code{TRUE} (default), argument specification is
#'               checked.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @references
#' Muthen, L. K., & Muthen, B. O. (1998-2017). \emph{Mplus User's Guide} (8th ed.).
#' Muthen & Muthen.
#'
#' @seealso
#' \code{\link{read.mplus}}, \code{\link{mplus.run}}, \code{\link{write.sav}},
#' \code{\link{write.xlsx}}, \code{\link{write.dta}}
#'
#' @return
#' Returns a character string indicating the variable names for the Mplus input file.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1: Write Mplus Data File and a Mplus input template
#' write.mplus(mtcars)
#'
#' # Example 2: Write Mplus Data File "mtcars.dat" and a Mplus input template "mtcars_INPUT.inp",
#' # missing values coded with -999, write variable names in a text file called "mtcars_VARNAMES.inp"
#' write.mplus(mtcars, file = "mtcars.dat", var = TRUE, na = -999)
#' }
write.mplus <- function(x, file = "Mplus_Data.dat", data = TRUE, input = TRUE,
                        var = FALSE, na = -99, check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'x' is missing
  if (isTRUE(missing(x))) { stop("Please specify a matrix or data frame for the argument 'x'.", call. = FALSE) }

  # Matrix or data frame for the argument 'x'?
  if (isTRUE(!is.matrix(x) && !is.data.frame(x))) { stop("Please specifiy a matrix or data frame for the argument 'x'.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data frame ####

  x <- as.data.frame(x)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert Factors in Numeric Variables ####

  x.factor <- sapply(x, is.factor)
  if (isTRUE(any(x.factor))) { x[, x.factor] <- sapply(x[, x.factor], as.numeric) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Numeric Variables ####

  x.numeric <- vapply(x, is.numeric, FUN.VALUE = logical(1L))

  if (isTRUE(any(!x.numeric))) {

    x <- x[, x.numeric]

    warning(paste0("Non-numeric variables were excluded from the data set: ", paste(names(which(!x.numeric)), collapse = ", ")), call. = FALSE)

    if (isTRUE(ncol(x) == 0L)) { stop("No variables left for the data set after excluding non-numeric variables.", call. = FALSE)

    }

  }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical =  c("data", "input", "var"), character = list(file = 1L), envir = environment(), input.check = check)

  if (isTRUE(check)) {

    # Variable names with .
    names. <- grep("\\.", names(x))

    if (isTRUE(length(names.) > 0L)) {

      names(x) <- gsub("\\.", "_", names(x))

      warning("Special character \".\" (dot) in the variable names were replaced with  \"_\" (underscore).", call. = FALSE)

    }

    # Variable names begin with an alphabet character
    names.a <- tolower(substr(names(x), 1L, 1L)) %in% letters
    if (isTRUE(any(!names.a))) { warning(paste0("Variable names must begin with an alphabet character, please modify variable names: ", paste(names(which(!names.a)), collapse = ", ")) , call. = FALSE) }

    # Variable names have max. 75 characters
    names.l <- nchar(names(x)) <= 75L

    if (isTRUE(any(!names.l))) { warning(paste0("Variable names must be no more than 77 characters, please modify variable names: ", paste(names(x)[!names.l], collapse = ", ")), call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## File Extension .dat, .txt. or .csv ####

  file <- ifelse(isTRUE(length(grep(".dat", file)) == 0L && length(grep(".txt", file)) == 0L && length(grep(".csv", file)) == 0L), file <- paste0(file, ".dat"), file)

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Save .dat File ####

  if (isTRUE(data)) { write.table(x, file = file, quote = FALSE, na = as.character(na), row.names = FALSE, col.names = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Prepare and Save Variable Names ####

  #...................
  ### Prepare Variable Names ####

  names.are <- names.temp <- names.length <- "           "
  for (i in names(x)) {

    names.temp <- paste(names.are, i, collapse = " ")
    names.length <- paste(names.length, i, collapse = " ")

    if (isTRUE(nchar(names.length) < 89L)) {

      names.are <- names.temp

    } else {

      names.are <- paste(names.are, "\n           ", i, collapse = " ")
      names.length <- paste("           ", i, collapse = " ")

    }

  }

  names.are <- paste0(names.are, ";")

  #...................
  ### Save Variable Names ####

  if (isTRUE(var)) {

    file.var <- sub(names(which(vapply(sapply(c(".dat", ".txt", ".csv"), grep, file), length, FUN.VALUE = integer(1L)) != 0L)), "_VARNAMES.txt", file, fixed = TRUE)

    writeLines(names.are, con = file.var)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Save Mplus Input ####

  if (isTRUE(input)) {

    # Mplus input template
    temp <- paste0("DATA:       FILE IS ", file, ";\n\n",
                   "VARIABLE:   NAMES ARE \n",
                                names.are, "\n\n",
                   "            USEVARIABLES ARE ", ";\n\n",
                   "            MISSING IS ALL(", na, ");\n\n",
                   "ANALYSIS:   \n\n",
                   "MODEL:      \n\n",
                   "OUTPUT:     \n")

    # Input file name
    file.inp <- sub(names(which(vapply(sapply(c(".dat", ".txt", ".csv"), grep, file), length, FUN.VALUE = integer(1L)) != 0L)), "_INPUT.inp", file, fixed = TRUE)

    # Write Input file
    writeLines(temp, con = file.inp)

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  return(invisible(names.are))

}

#_______________________________________________________________________________
