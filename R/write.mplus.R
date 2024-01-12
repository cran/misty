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
#' @param input  logical: if \code{TRUE} (default), Mplus input template is written
#'               in a text file named according to the argument\code{file} with
#'               the extension \code{_INPUT.inp}.
#' @param n.var  a numeric value indicating the number of variables in each line
#'               under \code{NAMES ARE} in the the Mplus input template.
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
#' \code{\link{read.mplus}}, \code{\link{run.mplus}}, \code{\link{write.sav}},
#' \code{\link{write.xlsx}}, \code{\link{write.dta}}
#'
#' @return
#' None.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1: Write Mplus Data File and a Mplus input template
#' write.mplus(mtcars)
#'
#' # Example 2: Write Mplus Data File "mtcars.dat" and a Mplus input template "mtcars_INPUT.inp",
#' # missing values coded with -999, 4 variables in each line under "NAMES ARE"
#' # write variable names in a text file called "mtcars_VARNAMES.inp"
#' write.mplus(mtcars, file = "mtcars.dat", n.var = 4, var = TRUE, na = -999)
#' }
write.mplus <- function(x, file = "Mplus_Data.dat", input = TRUE, n.var = 8, var = FALSE,
                        na = -99, check = TRUE) {

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

  x <- as.data.frame(x, stringsAsFactors = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Numeric variables ####

  x.numeric <- vapply(x, is.numeric, FUN.VALUE = logical(1))

  if (isTRUE(any(!x.numeric))) {

    x <- x[, x.numeric]

    warning(paste0("Non-numeric variables were excluded from the data set: ", paste(names(which(!x.numeric)), collapse = ", ")), call. = FALSE)

    if (isTRUE(ncol(x) == 0L)) { stop("No variables left for the data set after excluding non-numeric variables.", call. = FALSE)

    }

  }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # Check input 'input'
    if (isTRUE(!is.logical(input))) { stop("Please specify TRUE or FALSE for the argument 'input'.", call. = FALSE) }

    # Check input 'n.var'
    if (isTRUE(n.var %% 1L != 0L || n.var < 0L)) { stop("Specify a positive integer number for the argument 'n.var'.", call. = FALSE) }

    # Check input 'var'
    if (isTRUE(!is.logical(var))) { stop("Please specify TRUE or FALSE for the argument 'var'.", call. = FALSE) }

    # Variable names with .
    names. <- grep("\\.", names(x))

    if (isTRUE(length(names.) > 0L)) {

      names(x) <- gsub("\\.", "_", names(x))

      warning("Special character \".\" (dot) in the variable names were replaced with  \"_\" (underscore).", call. = FALSE)

    }

    # Variable names begin with an alphabet character
    names.a <- tolower(substr(names(x), 1L, 1L)) %in% letters

    if (isTRUE(any(!names.a))) {

      warning(paste0("Variable names must begin with an alphabet character, please modify variable names: ",
                     paste(names(which(!names.a)), collapse = ", ")) , call. = FALSE)

    }

    # Variable names have max. 8 characters
    names.l <- nchar(names(x)) <= 8L

    if (isTRUE(any(!names.l))) {

      warning(paste0("Variable names must be no more than 8 characters, please modify variable names: ",
                     paste(names(x)[!names.l], collapse = ", ")), call. = FALSE)

    }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## File extension .dat, .txt. or .csv ####

  file <- ifelse(length(grep(".dat", file)) == 0L && length(grep(".txt", file)) == 0L && length(grep(".csv", file)) == 0L,
                 file <- paste0(file, ".dat"), file)

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Save .dat file ####

  write.table(x, file = file, quote = FALSE, na = as.character(na), row.names = FALSE, col.names = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Save variable names ####

  if (isTRUE(var)) {

    file.var <- sub(names(which(vapply(sapply(c(".dat", ".txt", ".csv"), grep, file), length, FUN.VALUE = integer(1L)) != 0L)),
                    "_VARNAMES.txt", file, fixed = TRUE)

    writeLines(paste(names(x), collapse = " "), con = file.var)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Save Mplus input ####

  if (isTRUE(input)) {

    # Length of variable names divided by n.var
    var.n <- length(names(x)) / n.var

    temp <- NULL

    # Length of variable names greater than n.var
    if (isTRUE(floor(var.n) > 0L)) {

      var.x <- c(0L, sapply(1L:floor(var.n), function(y) y*n.var))

      for (i in seq_along(var.x)[-length(var.x)] ) {

          temp <- c(temp,
                    paste0("            ", paste(names(x)[(var.x[i] + 1L):var.x[i + 1L]], collapse = " "), "\n"))

      }

      # Add remaining variables
      if (isTRUE(var.n %% 1L != 0L)) {

        temp <- c(temp,
                  paste0("            ", paste(names(x)[(var.x[length(var.x)] + 1L):length(names(x))], collapse = " "), ";\n"))

      } else {

        temp[length(temp)] <- sub("\n", ";\n", temp[length(temp)])

      }

    # Length of variable names smaller than n.var
    } else {

      temp <- c(temp,
                paste0("            ", paste(names(x), collapse = " "), ";\n"))

    }

    # Mplus input template
    temp <- paste0("DATA:       FILE IS ", file, ";\n\n",
                   "VARIABLE:   NAMES ARE \n", paste(temp, collapse = ""), "\n",
                   "            USEVARIABLES ARE ", ";\n\n",
                   "            MISSING IS ALL(", na, ");\n\n",
                   "ANALYSIS:   \n\n",
                   "MODEL:      \n\n",
                   "OUTPUT:     \n")

    file.inp <- sub(names(which(vapply(sapply(c(".dat", ".txt", ".csv"), grep, file), length, FUN.VALUE = integer(1)) != 0L)),
                    "_INPUT.inp", file, fixed = TRUE)

    writeLines(temp, con = file.inp)

  }

}
