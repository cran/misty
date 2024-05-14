#' Read Mplus Data File and Variable Names
#'
#' This function reads a Mplus data file and/or Mplus input/output file to return
#' a data frame with variable names extracted from the Mplus input/output file.
#' Note that by default \code{-99} in the Mplus data file is replaced with to
#' \code{NA}.
#'
#' @param file       a character string indicating the name of the Mplus data
#'                   file with or without the file extension \code{.dat}, e.g.,
#'                   \code{"Mplus_Data.dat"} or \code{"Mplus_Data"}.
#'                   Note that it is not necessary to specify this argument
#'                   when \code{return.var = TRUE}.
#' @param sep        a character string indicating the field separator (i.e.,
#'                   delimiter) used in the data file specified in \code{file}.
#'                   By default, the separator is 'white space', i.e., one or
#'                   more spaces, tabs, newlines or carriage returns.
#' @param input      a character string indicating the Mplus input (\code{.inp})
#'                   or output file (\code{.out}) in which the variable names
#'                   are specified in the \code{VARIABLE:} section. Note that
#'                   if \code{input = NULL}, this function is equivalent to
#'                   \code{read.table(file)}.
#' @param na         a numeric vector indicating values to replace with \code{NA}.
#'                   By default, \code{-99} is replaced with \code{NA}. If
#'                   \code{-99} is not a missing value change the argument to
#'                   \code{NULL}.
#' @param print      logical: if \code{TRUE}, variable names are printed on
#'                   the console.
#' @param return.var logical: if \code{TRUE}, the function returns the variable
#'                   names extracted from the Mplus input or output file only.
#' @param encoding   character string declaring the encoding used on \code{file}
#'                   so the character data can be re-encoded. See the 'Encoding'
#'                   section of the help page for the \code{file} function, the
#'                   'R Data Import/Export Manual' and 'Note'.
#' @param check      logical: if \code{TRUE} (default), argument specification
#'                   is checked.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @references
#' Muthen, L. K., & Muthen, B. O. (1998-2017). \emph{Mplus User's Guide} (8th ed.).
#' Muthen & Muthen.
#'
#' @seealso
#' \code{\link{read.dta}}, \code{\link{write.dta}}, \code{\link{read.sav}},
#' \code{\link{write.sav}}, \code{\link{read.xlsx}}, \code{\link{write.xlsx}}
#'
#' @return
#' A data frame containing a representation of the data in the file.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1: Read Mplus data file and variable names extracted from the Mplus input file
#' dat <- read.mplus("Mplus_Data.dat", input = "Mplus_Input.inp")
#'
#' # Example 2: Read Mplus data file and variable names extracted from the Mplus input file,
#' # print variable names on the console
#' dat <- read.mplus("Mplus_Data.dat", input = "Mplus_Input.inp", print = TRUE)
#'
#' # Example 3: Read variable names extracted from the Mplus input file
#' varnames <- read.mplus(input = "Mplus_Input.inp", return.var = TRUE)
#' }
read.mplus <- function(file, sep = "", input = NULL, na = -99, print = FALSE,
                       return.var = FALSE, encoding = "UTF-8-BOM", check = TRUE) {

  #_____________________________________________________________________________
  #
  # Mplus Variable Names -------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Check input 'input' ####

  if (isTRUE(return.var && missing(input))) { stop("Please specify a character string indicating the name of the Mplus input/output file for the argument 'input'.", call. = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Mplus input/output file ####

  if (isTRUE(!is.null(input))) {

    #...................
    ### File extension .inp or .out ####

    input <- ifelse(length(grep(".inp", input)) == 0L && length(grep(".out", input)) == 0L, input <- paste0(input, ".inp"), input)

    #...................
    ### Read input text lines ####

    # Remove comments
    inp.lines <- sapply(strsplit(iconv(suppressWarnings(readLines(input)), from = "ISO-8859-1", to = "UTF-8"), "!"), function(y) ifelse(length(y) > 1L, y[1L], y))

    #...................
    ### Extract VARIABLE section ####

    var.ind1 <- grep("VARIABLE:", toupper(inp.lines))
    var.ind2 <- grep(";", inp.lines)

    inp.variable <- inp.lines[var.ind1:var.ind2[which(var.ind2 > var.ind1)][1]]

    #...................
    ### Extract variable names ####

    varnames <- misty::chr.omit(unlist(strsplit(misty::chr.trim(gsub("VARIABLE:|variable:|Variable:|NAMES ARE|names ARE|Names ARE|NAMES are|names are|Names are|NAMES Are|names Are|Names Are|NAMES =|names =|Names =|NAMES=|names=|Names=|;|\n|\t|\r|\r\n", "",
                                                                inp.variable)), " ")), check = FALSE)

    #...................
    ### Consecutive variable names ####

    if (isTRUE(length(grep("-", varnames)) > 0L)) {

      # Variable positive with consecutive variable names
      convar.pos <- grep("-", varnames)
      convar.list <- NULL
      for (i in convar.pos) {

        # Split consecutive variable names to start and end
        temp <- unlist(strsplit(varnames[i], "-"))

        # Split consecutive variable names
        split1.chr <- unlist(strsplit(temp[1L], ""))
        split2.chr <- unlist(strsplit(temp[2L], ""))

        # Extract number of digits
        rle1 <- rle(suppressWarnings(rev(!is.na(as.numeric(split1.chr)))))$lengths[1L]
        rle2 <- rle(suppressWarnings(rev(!is.na(as.numeric(split2.chr)))))$lengths[1L]

        # Extract starting and ending number
        start.n <- as.numeric(paste(rev(rev(split1.chr)[1:rle1]), collapse = ""))
        end.n <- as.numeric(paste(rev(rev(split2.chr)[1:rle2]), collapse = ""))

        # Expand variable names
        convar.list[[i]] <- paste0(paste(rev(rev(split1.chr)[-c(1:rle1)]), collapse = ""), start.n:end.n)

      }

      # Combine variables names and extended consecutive variable names
      for (i in (1L:length(varnames))[!1L:length(varnames) %in% convar.pos]) {

        convar.list[[i]] <- varnames[i]

      }

      varnames <- unlist(convar.list)

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Return Mplus variable names only ####

  if (isTRUE(return.var)) {

    return(varnames)

  #_____________________________________________________________________________
  #
  # Mplus Data -----------------------------------------------------------------

  } else {


    #___________________________________________________________________________
    #
    # Input Check --------------------------------------------------------------

    #...................
    ### Check input 'file' ####

    if (isTRUE(missing(file))) { stop("Please specify a character string indicating the name of the Mplus data file for the argument 'file'.", call. = FALSE) }

    #...................
    ### File extension .dat, .txt. or .csv ####

    file <- ifelse(length(grep(".dat", file)) == 0L && length(grep(".txt", file)) == 0L && length(grep(".csv", file)) == 0L, file <- paste0(file, ".dat"), file)

    if (isTRUE(!file.exists(file))) { stop(paste0("Unable to open Mplus data file: ", sQuote(file), " does not exist."), call. = FALSE) }

    #...................
    ### Check input 'check' ####

    if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

    #----------------------------------------

    if (isTRUE(check)) {

      # Read data text lines
      df.lines <- suppressWarnings(readLines(file))

      #...................
      ### Dot (.) as decimal separator ####

      if (isTRUE(length(grep(",", df.lines)) > 0L)) { stop(paste0("Data file ", file, " uses the wrong decimal separator, i.e., \",\" instead of \".\""), call. = FALSE) }

      #...................
      ### Number of rows ####

      invisible(tryCatch(read.table(file, sep = sep, stringsAsFactors = FALSE, fileEncoding = encoding),
                         error = function(y) { stop(paste0("Data file ", file, " does not have the same number of entries in each line."), call. = FALSE) }))

      #...................
      ### Number of columns match with number of variable names ####

      if (isTRUE(!is.null(input))) {

        df.check <- read.table(file, fileEncoding = encoding, stringsAsFactors = FALSE)

        if (isTRUE(ncol(df.check) != length(varnames))) {

          # Print variable names on console
          if (isTRUE(print)) { print(varnames) }

          stop(paste0("Number of columns in data file ", file, " (", ncol(df.check), ")", " does not match with the number of variables specified in ", input, " (", length(varnames), ")."), call. = FALSE)

        }

      }

    }

    #_____________________________________________________________________________
    #
    # Main Function --------------------------------------------------------------

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Mplus data ####

    # Read data
    object <- read.table(file, sep = sep, stringsAsFactors = FALSE, fileEncoding = encoding)

    #...................
    ### Missing values ####

    if (isTRUE(!is.null(na))) { object <- misty::as.na(object, na = na, check = FALSE) }

    #...................
    ### Assign variable names ####

    if (isTRUE(!is.null(input))) {

      colnames(object) <- varnames

      # Print variable names on console
      if (isTRUE(print)) { print(varnames) }

    }

    return(object)

  }

}
