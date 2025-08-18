#' Write Data File in Table Format, SPSS, Excel, or Stata DTA File
#'
#' This function writes a (1) data file in CSV (\code{.csv}), DAT (\code{.dat}),
#' or TXT (\code{.txt}) format using the \code{fwrite} function from the
#' \pkg{data.table} package, (2) SPSS file (\code{.sav}) using the \code{write.sav}
#' function, (3) Excel file (\code{.xlsx}) using the \code{write.xlsx} function,
#' or a (4) Stata DTA file (\code{.dta}) using the \code{write.dta} function in
#' the \pkg{misty} package. Note that the function \code{write.data} uses \code{","}
#' for decimal point and a semicolon \code{";"} for the separator, while the function
#' \code{write.data1} uses \code{"."} for decimal point and a comma \code{","}
#' for the separator when writing a CSV file.
#'
#' @param x         a matrix or data frame to be written.
#' @param file      a character string indicating the name of the data file
#'                  with the file extension \code{.csv}, \code{.dat},
#'                  \code{.txt}, \code{.sav}, \code{.xlsx}, or \code{.dta}.
#'                  Note that the function will select an appropriate
#'                  \code{write}-function depending on the file extension.
#' @param sep       a character string indicating the field separator, i.e.,
#'                  string for the delimiter. By default, the \code{write.data}
#'                  function uses a semicolon \code{";"}, while the function
#'                  \code{write.data1} function uses a comma \code{","} for
#'                  writing a CSV, DAT, or TXT data file
#' @param dec       a character string indicating the decimal separator, i.e.,
#'                  string for decimal points. By default, the \code{write.data}
#'                  function uses are comma \code{","}, while the function
#'                  \code{write.data1} function uses a decimal point \code{"."}
#'                  for writing a CSV, DAT, or TXT data file.
#' @param na        a character string to use for missing values in the data.
#'                  By default, a blank string \code{""} is used.
#' @param row.names logical: if \code{FALSE}, row names are written.
#' @param col.names logical: if \code{TRUE} (default), column names are written.
#' @param check     logical: if \code{TRUE} (default), argument specification
#'                  is checked.
#' @param ...       additional arguments to pass to the \code{fwrite}
#'                  \code{\link{write.sav}}, \code{\link{write.xlsx}}, or
#'                  \code{\link{write.dta}} function, see \code{Arguments}
#'                  section in the help pages.
#'
#' \describe{
#' \item{\strong{Comma-Separated Values (CSV) File}}{The function \code{write.data}
#' writes CSV files based on the Excel convention for CSV files in some Western
#' European locales by default, i.e., \code{";"} as delimiter and \code{","} for
#' decimal points. Depending on the language setting of the operating system of
#' the computer, the arguments \code{sep} and \code{dec} need to be specified to
#' \code{","} and \code{"."} (see Example 1b). Alternatively, the function
#' \code{write.data1} that uses \code{","} as delimiter and \code{"."} for
#' decimal points by default can be used (see Example 1c).}
#' }
#'
#' @author
#' Takuya Yanagida
#'
#' @name write.data
#'
#' @seealso
#' \code{\link{read.data}}, \code{\link{read.sav}},
#' \code{\link{write.sav}}, \code{\link{read.xlsx}}, \code{\link{write.xlsx}},
#' \code{\link{read.dta}}, \code{\link{write.dta}}, \code{\link{read.mplus}},
#' \code{\link{write.mplus}}
#'
#' @references
#' Barrett, T., Dowle, M., Srinivasan, A., Gorecki, J., Chirico, M., Hocking, T.,
#' & Schwendinger, B. (2024). data.table: Extension of 'data.frame'. R package
#' version 1.16.0. \url{https://CRAN.R-project.org/package=data.table}
#'
#' Jeroen O. (2021). \emph{writexl: Export Data Frames to Excel 'xlsx' Format}.
#' R package version 1.4.0. https://CRAN.R-project.org/package=writexl
#'
#' Wickham H, Miller E, Smith D (2023). \emph{haven: Import and Export 'SPSS',
#' 'Stata' and 'SAS' Files}. R package version 2.5.3.
#' \url{https://CRAN.R-project.org/package=haven}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1a: Write CSV data file, European format
#' write.data(mtcars, "European_CSV_Data.csv")
#'
#' # Example 1b: Write CSV data file, American format
#' write.data(mtcars, "American_CSV_Data.csv", sep = ",", dec = ".")
#'
#' # Example 1c: Write CSV data file, American format
#' write.data1(mtcars)
#'
#' # Example 2: Write SPSS data file
#' write.data(mtcars, "SPSS_Data.sav")
#'
#' # Example 3:  Write Excel data file
#' write.data(mtcars, "Excel_Data.xlsx")
#'
#' # Example 4:  Write Stata data file
#' write.data(mtcars, "Stata_Data.dta")
#' }
write.data <- function(x, file = "Data.csv", sep = ";", dec = ",", na = "",
                       row.names = FALSE, col.names = TRUE, check = TRUE, ...) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'x' is not missing and a matrix or data frame
  if (isTRUE(missing(x) || !is.matrix(x) && !is.data.frame(x))) { stop("Please specify a matrix or data frame for the argument 'x'.", call. = FALSE) }

  # File extension
  if (isTRUE(!grepl("\\.", file))) { stop("Please specify a data file with extension \"csv\", \"dat\", \"txt\", \"sav\", \"xlsx\", or \"dts\" for the argument 'file'.", call. = FALSE) }

  assign("file.exten", rev(unlist(strsplit(file, "\\.")))[1L]) |> (\(y) if (isTRUE(!y %in% c("csv", "dat", "txt", "dta", "sav", "xlsx"))) { stop("Data file with extension ", dQuote(y), " is not support by this function.", call. = FALSE) })()

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("row.names", "col.names"), character = list(file = 1L, sep = 1L, dec = 1L, na = 1L),
               s.character = list(dec = c(".", ",")), envir = environment(), input.check = check)

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
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## CSV, DAT, or TXT file ####

  if (isTRUE(file.exten %in% c("csv", "dat", "txt"))) {

    suppressMessages(data.table::fwrite(x = x, file = file, sep = sep, dec = dec, na = na, row.names = row.names, col.names = col.names, ...))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## SPSS, Excel, or Stat file ####

  } else {

    #...................
    ### SPSS File ####

    switch(file.exten, "sav" = {

      misty::write.sav(x = x, file = file, ...)

    #...................
    ### Excel File ####

    }, "xlsx" =  {

      misty::write.xlsx(x = x, file = file, ...)

    #...................
    ### Stata File ####

    }, "dta" = {

      misty::write.dta(x = x, file = file, ...)

    })

  }

}

#_______________________________________________________________________________

#' @rdname write.data1
write.data1 <- function(x, file = "Data.csv", sep = ",", dec = ".", na = "",
                        row.names = FALSE, col.names = TRUE, check = TRUE, ...) {

  misty::write.data(x = x, file = file, sep = sep, dec = dec, na = na, row.names = row.names, col.names = col.names, check = check, ... = ...)


}

#_______________________________________________________________________________
