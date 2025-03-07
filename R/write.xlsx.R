#' Write Excel File
#'
#' This function calls the \code{write_xlsx()} function in the \pkg{writexl}
#' package by Jeroen Ooms to write an Excel file (.xlsx).
#'
#' This function supports strings, numbers, booleans, and dates.
#'
#' @param x         a matrix, data frame or (named) list of matrices or data
#'                  frames that will be written in the Excel file.
#' @param file      a character string naming a file with or without file extension
#'                  '.xlsx', e.g., \code{"My_Excle.xlsx"} or \code{"My_Excel"}.
#' @param col.names logical: if \code{TRUE} (default), column names are written
#'                  at the top of the Excel sheet.
#' @param format    logical: if \code{TRUE}, column names in the Excel file are
#'                  centered and bold.
#' @param use.zip64 logical: if \code{TRUE}, zip64 to enable support for 4GB+ Excel
#'                  files is used.
#' @param check     logical: if \code{TRUE} (default), argument specification is
#'                  checked.
#'
#' @author
#' Jeroen Ooms
#'
#' @seealso
#' \code{\link{read.xlsx}}, \code{\link{write.sav}}, \code{\link{write.dta}},
#' \code{\link{write.mplus}}
#'
#' @references
#' Jeroen O. (2021). \emph{writexl: Export Data Frames to Excel 'xlsx' Format}.
#' R package version 1.4.0. https://CRAN.R-project.org/package=writexl
#'
#' @note
#' The function was adapted from the \code{write_xlsx()} function in the \pkg{writexl}
#' package by Jeroen Ooms (2021).
#'
#' @export
#'
#' @examples
#' # Example 1: Write Excel file (.xlsx)
#' write.xlsx(mtcars, file = "mtcars.xlsx")
#'
#' # Example 2: Write Excel file with multiple sheets (.xlsx)
#' write.xlsx(list(cars = cars, mtcars = mtcars), file = "Excel_Sheets.xlsx")
write.xlsx <- function(x, file = "Excel_Data.xlsx", col.names = TRUE, format = FALSE,
                       use.zip64 = FALSE, check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'x' is missing
  if (isTRUE(missing(x))) { stop("Please specify a matrix, data frame or list of matrices or data frames for the argument 'x'.", call. = FALSE) }

  # Check if input 'x' is NULL
  if (isTRUE(is.null(x))) { stop("Input specified for the argument 'x' is NULL.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("col.names", "use.zip64"), character = list(file = 1L), envir = environment(), input.check = check)

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  # File extension .xlsx
  file <- ifelse(length(grep(".xlsx", file)) == 0L, file <- paste0(file, ".xlsx"), file)

  # Matrix

  if (isTRUE(is.list(x))) {

    if(any(sapply(x, is.matrix))) {

      x <- lapply(x, as.data.frame)

    }

  } else {

    if (is.matrix(x)) {

      x <- as.data.frame(x)

    }

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  writexl::write_xlsx(x = x, path = file, col_names = col.names, format_headers = format, use_zip64 = use.zip64)

}

#_______________________________________________________________________________
