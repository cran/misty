#' Read Data File in Table format, SPSS, Excel, or Stata DTA File
#'
#' This function reads a (1) data file in CSV (\code{.csv}), DAT (\code{.dat}),
#' or TXT (\code{.txt}) format using the \code{fread} function from the \pkg{data.table}
#' package, (2) SPSS file (\code{.sav}) using the \code{read.sav} function, (3)
#' Excel file (\code{.xlsx}) using the \code{read.xlsx} function, or a (4) Stata
#' DTA file (\code{.dta}) using the \code{read.dta} function in the \pkg{misty}
#' package.
#'
#' @param file             a character string indicating the name of the data file
#'                         with the file extension \code{.csv}, \code{.dat},
#'                         \code{.txt}, \code{.sav}, \code{.xlsx}, or \code{.dta}.
#'                         Note that the function will select an appropriate
#'                         \code{read}-function depending on the file extension.
#' @param sheet            a character string indicating the name of a Excel sheet
#'                         or a numeric value indicating the position of the Excel
#'                         sheet to read. By default the first sheet will be read
#'                         when reading an Excel file (\code{.xlsx}).
#' @param header           logical: if \code{TRUE} (default), the first row is used
#'                         as column names when reading an Excel file (\code{.xlsx}),
#'                         if \code{FALSE} default names are used. A character vector
#'                         giving a name for each column can also be used.
#' @param select           a character vector of column names or numeric vector to
#'                         keep, drop the rest. See the help page of the \code{fread}
#'                         function in the \pkg{data.table} package.
#' @param drop             a character vector of column names or numeric vector
#'                         to drop, keep the rest.
#' @param sep              a character string indicating the separator between
#'                         columns for the \code{fread} function when reading data
#'                         in CSV (\code{.csv}), DAT (\code{.dat}), or TXT (\code{.txt})
#'                         format.
#' @param dec              a character string indicating the decimal separator
#'                         for the \code{fread} function when reading data in CSV
#'                         (\code{.csv}), DAT (\code{.dat}), or TXT (\code{.txt})
#'                         format.
#' @param use.value.labels logical: if \code{TRUE}, variables with value labels
#'                         are converted into factors.
#' @param use.missings     logical: if \code{TRUE} (default), user-defined missing
#'                         values are converted into NAs.
#' @param na.strings       a character vector of strings which are to be interpreted
#'                         as NA values.
#' @param stringsAsFactors logical: if \code{TRUE}, character vectors are converted
#'                         to factors.
#' @param formats          logical: if \code{TRUE}, variable formats are shown in
#'                         an attribute for all variables.
#' @param label            logical: if \code{TRUE}, variable labels are shown in
#'                         an attribute for all variables.
#' @param labels           logical: if \code{TRUE}, value labels are shown in an
#'                         attribute for all variables.
#' @param missing          logical: if \code{TRUE}, value labels for user-defined
#'                         missings are shown in an attribute for all variables.
#' @param widths           logical: if \code{TRUE}, widths are shown in an attribute
#'                         for all variables.
#' @param as.data.frame    logical: if \code{TRUE} (default), function returns a
#'                         regular data frame; if \code{FALSE} function returns
#'                         a tibble or data.table.
#' @param encoding         a character string indicating the encoding, i.e.,
#'                         \code{"unknown"}, \code{"UTF-8"}, or \code{"Latin-1"}
#'                         (default).
#' @param check            logical: if \code{TRUE} (default), argument specification
#'                         is checked.
#'
#' @author
#' Takuya Yanagida
#'
#' @seealso
#' \code{\link{read.sav}}, \code{\link{read.xlsx}}, \code{\link{read.dta}},
#' \code{\link{read.mplus}}
#'
#' @references
#' Barrett, T., Dowle, M., Srinivasan, A., Gorecki, J., Chirico, M., Hocking, T.,
#' & Schwendinger, B. (2024). data.table: Extension of 'data.frame'. R package
#' version 1.16.0. \url{https://CRAN.R-project.org/package=data.table}
#'
#' Wickham H, Miller E, Smith D (2023). \emph{haven: Import and Export 'SPSS',
#' 'Stata' and 'SAS' Files}. R package version 2.5.3.
#' \url{https://CRAN.R-project.org/package=haven}
#'
#' @return
#' Returns a data frame, tibble, or data table.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Read CSV data file
#' dat <- read.data("CSV_Data.csv")
#'
#' # Read DAT data file
#' dat <- read.data("DAT_Data.dat")
#'
#' # Read TXT data file
#' dat <- read.data("TXT_Data.txt")
#'
#' # Read SPSS data file
#' dat <- read.data("SPSS_Data.sav")
#'
#' # Read Excel data file
#' dat <- read.data("Excel_Data.xlsx")
#'
#' # Read Stata data file
#' dat <- read.data("Stata_Data.dta")
#' }
read.data <- function(file, sheet = NULL, header = TRUE, select = NULL, drop = NULL,
                      sep = "auto", dec = "auto", use.value.labels = FALSE,
                      use.missings = TRUE, na.strings = c("NA", ""), stringsAsFactors = FALSE,
                      formats = FALSE, label = FALSE, labels = FALSE,
                      missing = FALSE, widths = FALSE, as.data.frame = TRUE,
                      encoding = c("unknown", "UTF-8", "Latin-1"), check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check input 'file'
  if (isTRUE(missing(file))) { stop("Please specify a character string indicating the name of the data file for the argument 'file'", call. = FALSE) }

  # File extension
  if (isTRUE(!grepl("\\.", file))) { stop("Please specify a data file with extension \"csv\", \"dat\", \"txt\", \"sav\", \"xlsx\", or \"dts\" for the argument 'file'.", call. = FALSE) }

  assign("file.exten", rev(unlist(strsplit(file, "\\.")))[1L]) |>
    (\(y) if (isTRUE(!y %in% c("csv", "dat", "txt", "dta", "sav", "xlsx"))) { stop("Data file with extension ", dQuote(y), " is not support by this function.", call. = FALSE) })()

  # Check if 'file' exists
  if (isTRUE(!file.exists(file))) { stop(paste0("Unable to open the data file: ", sQuote(file), " does not exist."), call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("header", "use.value.labels", "use.missings", "stringsAsFactors", "formats", "label", "labels", "missing", "widths", "as.data.frame"),
               character = list(file = 1L, sep = 1L, dec = 1L),
               s.character = list(encoding = c("unknown", "UTF-8", "Latin-1")), envir = environment(), input.check = check)

  # Additional checks
  if (isTRUE(check)) {

    if (isTRUE(file.exten %in% c("csv", "dat", "txt"))) {

      # R package data.table
      if (isTRUE(!requireNamespace("data.table", quietly = TRUE))) { stop("Package \"data.table\" is needed for this function to work, please install it.", call. = FALSE ) }

    } else {

      # R package haven
      if (isTRUE(!requireNamespace("haven", quietly = TRUE))) { stop("Package \"haven\" is needed for this function to work, please install it.", call. = FALSE ) }

    }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## encoding Argument ####

  if (isTRUE(all(c("unknown", "UTF-8", "Latin-1") %in% encoding))) { encoding <- "Latin-1" }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## CSV, DAT, or TXT file ####

  if (isTRUE(file.exten %in% c("csv", "dat", "txt"))) {

    if (isTRUE(sep == "auto" && dec == "auto")) {

      # Separator ";"
      if (isTRUE(all(grepl(";", readLines(file, n = 10L))))) {

        object <- data.table::fread(file = file, sep = ";", dec = ",", select = select, drop = drop, na.strings = na.strings, stringsAsFactors = stringsAsFactors, data.table = !as.data.frame, encoding = encoding)

      # Separator ","
      } else if (isTRUE(all(grepl(",", readLines(file, n = 10L))))) {

        object <- data.table::fread(file = file, sep = ",", dec = ".", select = select, drop = drop, na.strings = na.strings, stringsAsFactors = stringsAsFactors, data.table = !as.data.frame, encoding = encoding)

      # Separator not ";" and ","
      } else {

        object <- data.table::fread(file = file, sep = sep, dec = dec, select = select, drop = drop, na.strings = na.strings, stringsAsFactors = stringsAsFactors, data.table = !as.data.frame, encoding = encoding)

      }

    } else {

      object <- data.table::fread(file = file, sep = sep, dec = dec, select = select, drop = drop, na.strings = na.strings, stringsAsFactors = stringsAsFactors, data.table = !as.data.frame, encoding = encoding)

    }

  } else {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## SPSS file ####

    switch(file.exten, "sav" = {

      object <- misty::read.sav(file = file, use.value.labels = use.value.labels, use.missings = use.missings, formats = formats, label = label, labels = labels, missing = missing, widths = widths, as.data.frame = as.data.frame, check = check)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Excel file ####

    }, "xlsx" =  {

      object <- misty::read.xlsx(file = file, sheet = sheet, header = header, na = na.strings, as.data.frame = as.data.frame, check = TRUE)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Stata file ####

    }, "dta" = {

      object <- misty::read.dta(file = file, use.value.labels = use.value.labels, formats = formats, label = label, labels = labels, missing = missing, widths = widths, as.data.frame = as.data.frame, check = check)

    })

  }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  return(object)

}

#_______________________________________________________________________________
