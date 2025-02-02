#' Read Excel File
#'
#' This function calls the \code{read_xlsx()} function in the \pkg{readxl} package
#' by Hadley Wickham and Jennifer Bryan (2023) to read an Excel file (.xlsx).
#'
#' @param file          a character string indicating the name of the Excel data
#'                      file with or without file extension '.xlsx', e.g.,
#'                      \code{"My_Excel_Data.xlsx"} or \code{"My_Excel_Data"}.
#' @param sheet         a character string indicating the name of a sheet or a
#'                      numeric value indicating the position of the sheet to read.
#'                      By default the first sheet will be read.
#' @param header        logical: if \code{TRUE} (default), the first row is used
#'                      as column names, if \code{FALSE} default names are used.
#'                      A character vector giving a name for each column can also
#'                      be used. If \code{coltypes} as a vector is provided,
#'                      \code{colnames} can have one entry per column, i.e. have
#'                      the same length as \code{coltypes}, or one entry per
#'                      unskipped column.
#' @param range         a character string indicating the cell range to read from,
#'                      e.g. typical Excel ranges like \code{"B3:D87"}, possibly
#'                      including the sheet name like \code{"Data!B2:G14"}.
#'                      Interpreted strictly, even if the range forces the
#'                      inclusion of leading or trailing empty rows or columns.
#'                      Takes precedence over \code{skip}, \code{nmax} and
#'                      \code{sheet}.
#' @param coltypes      a character vector containing one entry per column from
#'                      these options \code{"skip"}, \code{"guess"},
#'                      \code{"logical"}, \code{"numeric"}, \code{"date"},
#'                      \code{"text"} or \code{"list"}. If exactly one \code{coltype}
#'                      is specified, it will be recycled. By default (i.e.,
#'                      \code{coltypes = NULL}) coltypes will be guessed. The
#'                      content of a cell in a skipped column is never read and
#'                      that column will not appear in the data frame output. A
#'                      list cell loads a column as a list of length 1 vectors,
#'                      which are typed using the type guessing logic from
#'                      \code{coltypes = NULL}, but on a cell-by-cell basis.
#' @param na            a character vector indicating strings to interpret as
#'                      missing values. By default, blank cells will be treated
#'                      as missing data.
#' @param trim          logical: if \code{TRUE} (default), leading and trailing
#'                      whitespace will be trimmed.
#' @param skip          a numeric value indicating the minimum number of rows to
#'                      skip before reading anything, be it column names or data.
#'                      Leading empty rows are automatically skipped, so this is
#'                      a lower bound. Ignored if the argument \code{range} is
#'                      specified.
#' @param nmax          a numeric value indicating the maximum number of data rows
#'                      to read. Trailing empty rows are automatically skipped,
#'                      so this is an upper bound on the number of rows in the
#'                      returned data frame. Ignored if the argument \code{range}
#'                      is specified.
#' @param guessmax      a numeric value indicating the maximum number of data rows
#'                      to use for guessing column types.
#' @param progress      display a progress spinner? By default, the spinner appears
#'                      only in an interactive session, outside the context of
#'                      knitting a document, and when the call is likely to run
#'                      for several seconds or more.
#' @param name.repair   a character string indicating the handling of column names.
#'                      By default, the function ensures column names are not empty
#'                      and are unique.
#' @param as.data.frame logical: if \code{TRUE} (default), function returns a
#'                      regular data frame; if \code{FALSE} function returns a
#'                      tibble.
#' @param check         logical: if \code{TRUE} (default), argument specification
#'                      is checked.
#'
#' @author
#' Hadley Wickham and Jennifer Bryan
#'
#' @seealso
#' \code{\link{read.dta}}, \code{\link{write.dta}}, \code{\link{read.sav}},
#' \code{\link{write.sav}}, \code{\link{read.mplus}}, \code{\link{write.mplus}}
#'
#' @references
#' Wickham H, Miller E, Smith D (2023). \emph{readxl: Read Excel Files}. R package
#' version 1.4.3. \url{https://CRAN.R-project.org/package=readxl}
#'
#' @return
#' Returns a data frame or tibble.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1: Read Excel file (.xlsx)
#' read.xlsx("data.xlsx")
#'
#' # Example 2: Read Excel file (.xlsx), use default names as column names
#' read.xlsx("data.xlsx", header = FALSE)
#'
#' # Example 3: Read Excel file (.xlsx), interpret -99 as missing values
#' read.xlsx("data.xlsx", na = "-99")
#'
#' # Example 4: Read Excel file (.xlsx), use x1, x2, and x3 as column names
#' read.xlsx("data.xlsx", header = c("x1", "x2", "x3"))
#'
#' # Example 5: Read Excel file (.xlsx), read cells A1:B5
#' read.xlsx("data.xlsx", range = "A1:B5")
#'
#' # Example 6: Read Excel file (.xlsx), skip 2 rows before reading data
#' read.xlsx("data.xlsx", skip = 2)
#'
#' # Example 7: Read Excel file (.xlsx), return a tibble
#' read.xlsx("data.xlsx", as.data.frame = FALSE)
#' }
read.xlsx <- function(file, sheet = NULL, header = TRUE, range = NULL,
                      coltypes = c("skip", "guess", "logical", "numeric", "date", "text", "list"),
                      na = "", trim = TRUE, skip = 0, nmax = Inf, guessmax = min(1000, nmax),
                      progress = readxl::readxl_progress(), name.repair = "unique", as.data.frame = TRUE,
                      check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check input 'file'
  if (isTRUE(missing(file))) { stop("Please specify a character string indicating the name of the SPSS data file for the argument 'file'", call. = FALSE) }

  # File extension .xlsx
  file <- ifelse(length(grep(".xlsx", file)) == 0L, file <- paste0(file, ".xlsx"), file)

  # Check if file exists
  if (isTRUE(!file.exists(file))) { stop(paste0("Unable to open Excel data file: ", sQuote(file), " does not exist."), call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("trim", "as.data.frame"),
               numeric = list(skip = 1L, nmax = 1L, guessmax = 1L),
               character = list(file = 1L, sheet = 1L, range = 1L, name.repair = 1L),
               package = "readxl", envir = environment(), input.check = check)

  #_____________________________________________________________________________
  #
  # Data and Arguments ---------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Argument 'coltypes' ####

  if (isTRUE(all(c("skip", "guess", "logical", "numeric", "date", "text", "list") %in% coltypes))) {

    coltypes <- NULL

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data frame ####

  if (isTRUE(as.data.frame)) {

    object <- as.data.frame(readxl::read_xlsx(path = file, sheet = sheet, range = range, col_names = header,
                                              col_types = coltypes, na = na, trim_ws = trim, skip = skip,
                                              n_max = nmax, guess_max = guessmax, progress = progress,
                                             .name_repair = name.repair), stringsAsFactors = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Tibble ####

  } else {

    object <- readxl::read_xlsx(path = file, sheet = sheet, range = range, col_names = header,
                                col_types = coltypes, na = na, trim_ws = trim, skip = skip,
                                n_max = nmax, guess_max = guessmax, progress = progress,
                                .name_repair = name.repair)

  }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  return(object)

}

#_______________________________________________________________________________
