#' Write Mplus Data File
#'
#' This function writes a matrix or data frame to a tab-delimited file without variable names and
#' a text file with variable names. Only numeric values are allowed, missing data will be coded
#' as a single numeric value.
#'
#' @param x           a matrix or data frame to be written to a tab-delimited file.
#' @param file        a character string naming a file with or without the file extention '.dat',
#'                    e.g., \code{"Mplus_Data.dat"} or \code{"Mplus_Data"}.
#' @param var         logical: if \code{TRUE}, variable names are written in a text file named
#'                    according to the argument\code{file} with the extention \code{_VARNAMES.txt}.
#' @param print       logical: if \code{TRUE}, variable names are printed on the console.
#' @param na          a numeric value or character string representing missing values (\code{NA})
#'                    in the data set.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @references
#' Muthen, L. K., & Muthen, B. O. (1998-2017). \emph{Mplus User's Guide} (8th ed.). Muthen & Muthen.
#'
#' @seealso
#' \code{\link{read.mplus}}, \code{\link{run.mplus}}
#'
#' @return
#' None.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(id = 1:5,
#'                   x = c(NA, 2, 1, 5, 6),
#'                   y = c(5, 3, 6, 8, 2),
#'                   z = c(2, 1, 1, NA, 4))
#'
#' # Write Mplus Data File and a text file with variable name
#' write.mplus(dat)
#'
#' # Write Mplus Data File "Data.dat" and a text file with variable name,
#' # print variable names on the console, missing values coded with -999
#' write.mplus(dat, file = "Data.dat", print = TRUE, na = -999)
#' }
write.mplus <- function(x, file = "Mplus_Data.dat", var = TRUE, print = FALSE, na = -99, check = TRUE) {

  ####################################################################################
  # Data

  #......
  # Check input 'x'
  if (missing(x)) {

    stop("Please specify a matrix or data frame for the argument 'x'", call. = FALSE)

  }

  #......
  # Matrix or data frame for the argument 'x'?
  if (!is.matrix(x) && !is.data.frame(x)) {

    stop("Please specifiy a matrix or data frame for the argument 'x'.", call. = FALSE)

  }

  df <- as.data.frame(x)

  ####################################################################################
  # Input Check

  #......
  # Check input 'check'
  if (isFALSE(isTRUE(check) | isFALSE(check))) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }

  #-----------------------------------------

  if (isTRUE(check)) {

    #......
    # Numeric variables
    df.numeric <-  sapply(df, is.numeric)

    if (any(!df.numeric)) {

      df <- df[, df.numeric]

      warning(paste0("Non-numeric variables were excluded from the data set: ", paste(names(which(!df.numeric)), collapse = ", ")),
              call. = FALSE)

      if (ncol(df) == 0) {

        stop("No variables left for the data set after excluding non-numeric variables.", call. = FALSE)

      }

    }

    #......
    # Variable names with .
    names. <- grep("\\.", names(df))

    if (length(names.) > 0) {

      names(df) <- gsub("\\.", "_", names(df))

      warning("Special character \".\" (dot) in the variable names were replaced with  \"_\" (underscore).", call. = FALSE)

    }

    #......
    # Variable names begin with an alphabet character
    names.a <- tolower(substr(names(df), 1, 1)) %in% letters

    if (any(!names.a)) {

      warning(paste0("Variable names must begin with an alphabet character, please modify variable names: ",
                     paste(names(which(!names.a)), collapse = ", ")) , call. = FALSE)

    }

    #......
    # Variable names have max. 8 characters
    names.l <- nchar(names(df)) <= 8

    if (any(!names.l)) {

      warning(paste0("Variable names must be no more than 8 characters, please modify variable names: ",
                     paste(names(df)[!names.l], collapse = ", ")), call. = FALSE)

    }

  }

  ####################################################################################
  # Arguments

  #-----------------------------------------
  # File extention .dat, .txt. or .csv

  file <- ifelse(length(grep(".dat", file)) == 0 && length(grep(".txt", file)) == 0 && length(grep(".csv", file)) == 0,
                 file <- paste0(file, ".dat"), file)

  ####################################################################################
  # Main Function

  #-----------------------------------------
  # Save .dat file

  write.table(df, file = file, quote = FALSE, na = as.character(na),
              row.names = FALSE, col.names = FALSE)

  #-----------------------------------------
  # Save variable names
  if (isTRUE(var)) {

    file <- sub(names(which(sapply(sapply(c(".dat", ".txt", ".csv"), grep, file), length) != 0)),
                "_VARNAMES.txt", file)

    writeLines(paste(names(df), collapse = " "), con = file)

  }

  #-----------------------------------------
  # Print variable names on console
  if (isTRUE(print)) {

    print(names(df))

  }

}