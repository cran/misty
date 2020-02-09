#' Read Mplus Data File and Variable Names
#'
#' This function reads a Mplus data file and/or Mplus input/output file to return a data frame with variable names
#' extracted from the Mplus input/output file.
#'
#' @param file          a character string indicating the name of the Mplus data file with or without
#'                      the file extention \code{.dat}, e.g., \code{"Mplus_Data.dat"} or \code{"Mplus_Data"}.
#'                      Note that it is not necessary to specify this argument when \code{return.var = TRUE}.
#' @param sep           a character string indicating the field separator (i.e., delimiter) used in the data file
#'                      specified in \code{file}. By default, the separator is 'white space', i.e., one or more
#'                      spaces, tabs, newlines or carriage returns.
#' @param input         a character string indicating the Mplus input (\code{.inp}) or output file (\code{.out})
#'                      in which the variable names are specified in the \code{VARIABLE:} section.
#'                      Note that if \code{input = NULL}, this function is equivalent to \code{read.table(file)}.
#' @param print         logical: if \code{TRUE}, variable names are printed on the console.
#' @param return.var    logical: if \code{TRUE}, the function returns the variable names extracted from the
#'                      Mplus input or output file only.
#' @param fileEncoding  character string declaring the encoding used on \code{file} so the character data can be
#'                      re-encoded. See \code{\link{df.sort}}.
#' @param check         logical: if \code{TRUE}, argument specification is checked.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @references
#' Muthen, L. K., & Muthen, B. O. (1998-2017). \emph{Mplus User's Guide} (8th ed.). Muthen & Muthen.
#'
#' @seealso
#' \code{\link{run.mplus}}, \code{\link{write.mplus}}
#'
#' @return
#' A data frame containing a representation of the data in the file.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Read Mplus data file and variable names extracted from the Mplus input file
#' dat <- read.mplus("Mplus_Data.dat", input = "Mplus_Input.inp")
#'
#' # Read Mplus data file and variable names extracted from the Mplus input file,
#' # print variable names on the console
#' dat <- read.mplus("Mplus_Data.dat", input = "Mplus_Input.inp", print = TRUE)
#'
#' # Read variable names extracted from the Mplus input file
#' varnames <- read.mplus(input = "Mplus_Input.inp", return.var = TRUE)
#' }
read.mplus <- function(file, sep = "", input = NULL, print = FALSE, return.var = FALSE,
                       fileEncoding = "UTF-8-BOM", check = TRUE) {

  ######################################################################################################################
  #---------------------------------------------------------------------------------------------------------------------
  # Mplus Variable names

  #----------------------------------------
  # Check input 'file'
  if (missing(input)) {

    stop("Please specify a character string indicating the name of the Mplus input/output file for the argument 'input'",
         call. = FALSE)

  }

  #----------------------------------------
  # Mplus input/output file

  if (!is.null(input)) {

    #......
    # File extention .inp or .out
    input <- ifelse(length(grep(".inp", input)) == 0 && length(grep(".out", input)) == 0,
                    input <- paste0(input, ".inp"), input)

    #......
    # Read input text lines
    inp.lines <- suppressWarnings(readLines(input))

    #......
    # Extract VARIABLE section
    var.ind1 <- grep("VARIABLE:", toupper(inp.lines))
    var.ind2 <- grep(";", inp.lines)

    inp.variable <- inp.lines[var.ind1:var.ind2[which(var.ind2 > var.ind1)][1]]

    #......
    # Remove comments
    if (length(grep("!", inp.variable)) > 0) {

      inp.comments <- inp.variable[grep("!", inp.variable)]

      inp.variable[grep("!", inp.variable)] <- unlist(lapply(strsplit(inp.comments, split = " "), function(y) paste(y[1:(grep("!", y) - 1)], collapse = " ")))

    }

    #......
    # Extract variable names
    varnames <- misty::stromit(unlist(strsplit(misty::trim(gsub("VARIABLE:|variable:|Variable:|NAMES ARE|names ARE|Names ARE|NAMES are|names are|Names are|NAMES Are|names Are|Names Are|NAMES =|names =|Names =|;|\n|\t|\r|\r\n", "",
                                                                inp.variable)), " ")), check = FALSE)

  }

  #----------------------------------------
  # Return Mplus variable names only?
  if (isTRUE(return.var)) {

    return(varnames)

  ######################################################################################################################
  #---------------------------------------------------------------------------------------------------------------------
  # Mplus Data
  } else {


    ####################################################################################
    # Input Check

    #......
    # Check input 'file'
    if (missing(file)) {

      stop("Please specify a character string indicating the name of the Mplus data file for the argument 'file'",
           call. = FALSE)

    }

    #......
    # File extention .dat, .txt. or .csv
    file <- ifelse(length(grep(".dat", file)) == 0 && length(grep(".txt", file)) == 0 && length(grep(".csv", file)) == 0,
                   file <- paste0(file, ".dat"), file)

    #......
    # Check input 'check'
    if (isFALSE(isTRUE(check) | isFALSE(check))) {

      stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

    }

    #----------------------------------------

    if (isTRUE(check)) {

      # Read data text lines
      df.lines <- suppressWarnings(readLines(file))

      #......
      # Dot (.) as decimal separator

      if (length(grep(",", df.lines)) > 0) {

        stop(paste0("Data file ", file, " uses the wrong decimal separator, i.e., \",\" instead of \".\""),
             call. = FALSE)

      }

      #......
      # Number of rows
      df.lines.nrows <- unname(sapply(sapply(df.lines, function(y) strsplit(y, " ")), length))

      if (length(unique(df.lines.nrows)) != 1) {

        stop(paste0("Data file ", file, " does not have the same number of entries in each line."), call. = FALSE)

      }


      #......
      # Number of columns match with number of variable names
      if (!is.null(input)) {

        df.check <- read.table(file, fileEncoding = fileEncoding)

        if (ncol(df.check) != length(varnames)) {

          # Print variable names on console
          if (isTRUE(print)) {

            print(varnames)

          }

          stop(paste0("Number of columns in data file ", file, " (", ncol(df.check), ")", " does not match with the number of variables specified in ", input,
                      " (", length(varnames), ")."), call. = FALSE)

        }

      }

    }

    ####################################################################################
    # Main Function

    #----------------------------------------
    # Mplus data

    #......
    # Read data
    object <- read.table(file, sep = sep, stringsAsFactors = FALSE, fileEncoding = fileEncoding)

    #----------------------------------------
    # Assign variable names
    if (!is.null(input)) {

      colnames(object) <- varnames

      # Print variable names on console
      if (isTRUE(print)) {

        print(varnames)

      }

    }

    return(object)

  }

}