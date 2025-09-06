#' Frequency Table
#'
#' This function computes a frequency table with absolute and percentage frequencies
#' for one or more than one variable. By default, the function displays the absolute
#' and percentage frequencies when specifying one variable, while the function displays
#' only the absolute frequencies when specifying more than one variable.
#'
#' @param data     a vector, factor, or data frame.
#' @param ...      an expression indicating the variable names in \code{data},
#'                 e.g., \code{freq(dat, x1, x2, x3)}. Note that the operators
#'                 \code{+}, \code{-}, \code{~}, \code{:}, \code{::},
#'                 and \code{!} can also be used to select variables, see 'Details'
#'                 in the \code{\link{df.subset}} function.
#' @param print    a character string indicating which percentage(s) to be printed
#'                 on the console, i.e., no percentages (\code{"no"}), all percentages
#'                 (\code{"all"}), percentage frequencies (\code{"print"}), and
#'                 valid percentage frequencies \code{"v.perc"}). Default setting
#'                 when specifying one variable is \code{print = "all"}, while
#'                 default setting when specifying more than one variable is
#'                 \code{print = "no"} unless \code{split = TRUE}.
#' @param freq     logical: if \code{TRUE} (default), absolute frequencies will
#'                 be shown on the console.
#' @param split    logical: if \code{TRUE}, output table is split by variables
#'                 when specifying more than one variable in \code{data}.
#' @param labels   logical: if \code{TRUE} (default), labels for the factor
#'                 levels will be used.
#' @param val.col  logical: if \code{TRUE}, values are shown in the columns,
#'                 variables in the rows.
#' @param round    an integer value indicating the number of decimal places to be
#'                 used for rounding numeric variables.
#' @param exclude  an integer value indicating the maximum number of unique values
#'                 for variables to be included in the analysis when specifying
#'                 more than one variable, i.e., variables with the number of
#'                 unique values exceeding \code{exclude} will be excluded from
#'                 the analysis. It is also possible to specify \code{exclude = FALSE}
#'                 to include all variables in the analysis.
#' @param digits   an integer value indicating the number of decimal places to be
#'                 used for displaying percentages.
#' @param as.na    a numeric vector indicating user-defined missing values, i.e.
#'                 these values are converted to \code{NA} before conducting the
#'                 analysis.
#' @param write    a character string naming a file for writing the output into
#'                 either a text file with file extension \code{".txt"} (e.g.,
#'                 \code{"Output.txt"}) or Excel file with file extension
#'                 \code{".xlsx"}  (e.g., \code{"Output.xlsx"}). If the file
#'                 name does not contain any file extension, an Excel file will
#'                 be written.
#' @param append   logical: if \code{TRUE} (default), output will be appended
#'                 to an existing text file with extension \code{.txt} specified
#'                 in \code{write}, if \code{FALSE} existing text file will be
#'                 overwritten.
#' @param check    logical: if \code{TRUE} (default), argument specification is checked.
#' @param output   logical: if \code{TRUE} (default), output is shown on the console.
#'
#' @details
#' The function displays valid percentage frequencies only in the presence of
#' missing values and excludes variables with all values missing from the analysis.
#' Note that it is possible to mix numeric variables, factors, and character variables
#' in the data frame specified in the argument \code{data}. By default, numeric
#' variables are rounded to three digits before computing the frequency table.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{crosstab}}, \code{\link{des cript}}, \code{\link{multilevel.descript}},
#' \code{\link{na.descript}}, \code{\link{write.result}}
#'
#' @references
#' Becker, R. A., Chambers, J. M., & Wilks, A. R. (1988). \emph{The New  S Language}.
#' Wadsworth & Brooks/Cole.
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{data}}{data frame used for the current analysis}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{result}}{data frame with absolute frequencies and percentages or
#'                      list with result tables, i.e., \code{freq} for absolute
#'                      frequencies, \code{perc} for percentages, and \code{v.perc}
#'                      for valid percentages}
#'
#' @export
#'
#' @examples
#' # Example 1: Frequency table for 'cyl'
#' freq(mtcars, cyl)
#'
#' # Alternative specification without using the '...' argument
#' freq(mtcars$cyl)
#'
#' # Example 2: Frequency table, values shown in columns
#' freq(mtcars, cyl, val.col = TRUE)
#'
#' # Example 3: Frequency table, use 3 digit for displaying percentages
#' freq(mtcars, cyl, digits = 3)
#'
#' # Example 4: Frequency table for 'cyl', 'gear', and 'carb'
#' freq(mtcars, cyl, gear, carb)
#'
#' # Alternative specification without using the '...' argument
#' freq(mtcars[, c("cyl", "gear", "carb")])
#'
#' # Example 5: Frequency table, with percentage frequencies
#' freq(mtcars, cyl, gear, carb, print = "all")
#'
#' # Example 6: Frequency table, split output table
#' freq(mtcars, cyl, gear, carb, split = TRUE)
#'
#' # Example 7: Frequency table, exclude variables with more than 5 unique values
#' freq(mtcars, exclude = 5)
#'
#' # Example 8a: Write Results into a text file
#' freq(mtcars, cyl, gear, carb, split = TRUE, write = "Frequencies.txt")
#'
#' # Example 8b: Write Results into a Excel file
#' freq(mtcars, cyl, gear, carb, split = TRUE, write = "Frequencies.xlsx")
freq <- function(data, ..., print = c("no", "all", "perc", "v.perc"),
                 freq = TRUE, split = FALSE, labels = TRUE, val.col = FALSE,
                 round = 3, exclude = 15, digits = 2, as.na = NULL,
                 write = NULL, append = TRUE, check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'data' is missing or NULL
  if (isTRUE(missing(data) || is.null(data))) { stop("Please specify a vector, factor, or data frame for the argument 'data'", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data Using the Argument '...' ####

  if (isTRUE(!missing(...))) {

    x <- as.data.frame(data[, .var.names(data = data, ...), drop = FALSE])

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data Without Using the Argument '...' ####

  } else {

    x <- as.data.frame(data)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Missing Data ####

  #...................
  ### Convert User-Missing Values into NA ####

  if (isTRUE(!is.null(as.na))) { x <- .as.na(x, na = as.na) }

  #...................
  ### Check Missing Values ####

  # All values missing
  if (isTRUE(all(is.na(x)))) { stop("All values in the vector or data frame specified in the argument 'data' are missing.", call. = FALSE) }

  # Exclude variables with all missing
  if (isTRUE(ncol(x) > 1L)) {

    vapply(x, function(y) all(is.na(y)), FUN.VALUE = logical(1L)) |> (\(p) if (isTRUE(any(p))) {

        # Exclude variables with all values missing
        x <<- x[, !p, drop = FALSE]

        if (isTRUE(ncol(x) == 0L)) {

          stop(paste0("No variable left for the analysis after excluding variables with all values missing."), call. = FALSE)

        } else {

          warning(paste0("Variables with all values missing were excluded from the analysis: ", paste(names(p), collapse = ", ")), call. = FALSE)

        }

      })()

  }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("freq", "labels", "val.col", "append", "split", "output"), numeric = list(round = 1L), s.character = list(print = c("no", "all", "perc", "v.perc")),
               args = c("digits", "write2"), envir = environment(), input.check = check)

  # Additional checks
  if (isTRUE(check)) {

    # No frequencies and percentages
    if (isTRUE(!isTRUE(freq) && all(print == "no"))) { stop("Please specify \"all\", \"perc\", or \"v.perc\" for the argument 'print' when specifying freq = FALSE.", call. = FALSE) }

    # Argument 'split'
    if (isTRUE(split && ncol(x) == 1L)) { split <- FALSE }

  }

  #_____________________________________________________________________________
  #
  # Data and Arguments ---------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Argument 'print' ####

  #...................
  ### One Variable ####

  if (isTRUE(ncol(x) == 1L)) {

    if (isTRUE(all(c("no", "all", "perc", "v.perc") %in% print))) { print <- c("perc", "v.perc") }

  #...................
  ### More than one variable ####

  } else {

    if (isTRUE(freq)) {

      if (!isTRUE(split)) {

        if (isTRUE(all(c("no", "all", "perc", "v.perc") %in% print))) { print <- "no" }

      } else {

        if (isTRUE(all(c("no", "all", "perc", "v.perc") %in% print))) { print <- c("perc", "v.perc") }

      }

    } else {

      if (isTRUE(all(c("no", "all", "perc", "v.perc") %in% print))) { print <- c("perc", "v.perc") }

    }

  }

  if (isTRUE(all(print == "all"))) { print <- c("perc", "v.perc") }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Factor Labels ####

  vapply(x, function(y) is.factor(y), FUN.VALUE = logical(1L)) |> (\(p) if (isTRUE(any(p)) && !isTRUE(labels)) {

      # Check factor levels across factors
      if (isTRUE(sum(p) > 1L)) { lapply(x[, p, drop = FALSE], levels) |> (\(q) if (isTRUE(any(apply(combn(length(x[, p]), 2L), 2L, function(y) !identical(q[[y[1L]]], q[[y[2L]]]))))) { warning("Variable do not have the same factor levels, i.e., values may not be comparable across variables.", call. = FALSE) })() }

      # Remove labels
      x <<- data.frame(lapply(x, function(y) if (isTRUE(is.factor(y))) as.numeric(y) else y))

    })()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Exclude Variables ####

  if (isTRUE(!isFALSE(exclude))) {

    if (isTRUE(ncol(x) > 1L && !split)) {

      # Default setting exclude variables with more than 15 unique values
      if (isTRUE(exclude)) { exclude <- 15L }

      which(vapply(x, function(y) length(unique(na.omit(y))), FUN.VALUE = 1L) > exclude) |> (\(p) if (isTRUE(length(p) > 0L)) {

          x <<- x[, -p, drop = FALSE]

          if (isTRUE(ncol(x) == 0L)) {

            stop(paste0("No variable left for the analysis after excluding variables with all values missing."), call. = FALSE)

          } else {

            warning(paste0("Variables with more than ", exclude, " unique values were excluded: ", paste(names(p), collapse = ", ")), call. = FALSE)

          }

        })()

    }

  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Round Numeric Variables ####

  # Numeric variable with more than 'round' digits
  names(which(vapply(x[, vapply(x, function(y) is.numeric(y) & !is.integer(y), FUN.VALUE = logical(1L)), drop = FALSE], function(y) any(na.omit(nchar(gsub("(.*)(\\.)|([0]*$)","", y)) > round)), FUN.VALUE = logical(1L)))) |> (\(p) if (isTRUE(length(p) > 0L)) {

      x[, p] <<- lapply(x[, p, drop = FALSE], base::round, digits = round)

      warning(paste0("Numeric variables with more than ", round, " digits were rounded: ", paste(p, collapse = ", ")), call. = FALSE)

    })()

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## One Variable ####

  if (isTRUE(ncol(x) == 1L)) {

    # Absolute frequencies
    x.abs <- table(x, useNA = "always")

    # Percentages
    x.perc <- prop.table(x.abs) * 100L

    # Valid percentages
    x.v.perc <- prop.table(table(x, useNA = "no")) * 100L

    #...................
    ### Values in Columns ####

    if (!isTRUE(val.col)) {

      result <- data.frame(Value = c(na.omit(suppressWarnings(iconv(na.omit(names(x.abs)), from = "ISO-8859-1", to = "UTF-8"))), NA),
                           Freq = as.numeric(x.abs),
                           Perc = as.numeric(x.perc),
                           V.Perc = c(as.numeric(x.v.perc), NA))

      # Convert 'Value' in numeric
      if(all(grepl("(^(-|\\+)?((\\.?\\d+)|(\\d+\\.\\d+)|(\\d+\\.?))$)|(^(-|\\+)?((\\.?\\d+)|(\\d+\\.\\d+)|(\\d+\\.?))e(-|\\+)?(\\d+)$)", x = na.omit(result$Value)))) {

        # Do not convert "000", "001", "010" etc. into numeric
        if (isTRUE(all(substr(na.omit(result$Value), 1L, 1L) != "0"))) { result$Value <- as.numeric(result$Value) }

      }

    #...................
    ### Values in Rows ####

    } else {

      result <- data.frame(matrix(c(x.abs, x.perc, x.v.perc, NA), nrow = 3L, dimnames = list(c ("Freq", "Perc", "V.Perc"), names(x.abs)), byrow = TRUE), check.names = FALSE)

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## More than One Variable ####

  } else {

    #...................
    ### split = FALSE ####

    if (!isTRUE(split)) {

      # Unique levels
      x.levels <- NULL

      # Numeric variables
      if (isTRUE(any(vapply(x, is.numeric, FUN.VALUE = logical(1L))))) { x.levels <- c(x.levels, na.omit(sort(unique(unlist(x[, vapply(x, is.numeric, FUN.VALUE = logical(1L))]))))) }

      # Factors
      if (isTRUE(any(vapply(x, is.factor, FUN.VALUE = logical(1L))))) {

        sort(unique(unname(unlist(sapply(x[, vapply(x, is.factor, FUN.VALUE = logical(1L))], function(y) (as.character(y))))))) |>
          # Factor levels are numbers
          (\(p) if (isTRUE(all(unlist(strsplit(p, "")) %in% 0L:9L))) {

             x.levels <<- sort(c(x.levels, as.numeric(p)))

           # Factor levels are character
           } else {

             x.levels <<- c(x.levels, p)

           })()

       }

      # Character
      if (isTRUE(any(vapply(x, is.character, FUN.VALUE = logical(1L))))) { x.levels <- c(x.levels, na.omit(sort(unique(unlist(x[, vapply(x, is.character, FUN.VALUE = logical(1L))]))))) }

      # Logical
      if (isTRUE(any(vapply(x, is.logical, FUN.VALUE = logical(1L))))) { x.levels <- c(x.levels, c(TRUE, FALSE)) }

      # Absolute frequencies
      unique(x.levels) |> (\(p) {

         x.abs <<- vapply(x, function(y) table(factor(y, levels = p), useNA = "always"), FUN.VALUE = integer(length(p) + 1L))

         # Percentages
         x.perc <<- vapply(x, function(y) prop.table(table(factor(y, levels = p), useNA = "always")) * 100L, FUN.VALUE = numeric(length(p) + 1L))

         # Valid Percentages
         x.v.perc <<- vapply(x, function(y) prop.table(table(factor(y, levels = p), useNA = "no")) * 100L, FUN.VALUE = numeric(length(p))) |>
           # More than one value or only one value
           (\(q) if (isTRUE(is.matrix(q))) { apply(q, 2L, function(y) prop.table(y) * 100L) } else { matrix(prop.table(q) * 100L, nrow = 1L, dimnames = list(na.omit(row.names(x.abs)), names(q))) })()

         })()

      # Values in rows
      if (!isTRUE(val.col)) {

        # Absolute frequencies table
        abs.result <- data.frame(Value = rownames(x.abs), x.abs, row.names = NULL)

        # Percentages table
        perc.result <- data.frame(Value = rownames(x.perc), x.perc, row.names = NULL)

        # Valid Percentages table
        v.perc.result <- data.frame(Value = rownames(x.v.perc), x.v.perc, row.names = NULL)

        # Convert 'Value' in numeric
        if(all(grepl("(^(-|\\+)?((\\.?\\d+)|(\\d+\\.\\d+)|(\\d+\\.?))$)|(^(-|\\+)?((\\.?\\d+)|(\\d+\\.\\d+)|(\\d+\\.?))e(-|\\+)?(\\d+)$)", x = na.omit(abs.result$Value)))) {

          abs.result$Value <- as.numeric(abs.result$Value)
          perc.result$Value <- as.numeric(perc.result$Value)
          v.perc.result$Value <- as.numeric(v.perc.result$Value)

        }

      # Values in columns
      } else {

        # Absolute frequencies table
        abs.result <- data.frame(Var = colnames(x.abs), matrix(t(x.abs), nrow = ncol(x.abs), dimnames = list(NULL, rownames(x.abs))), check.names = FALSE)

        # Percentages table
        perc.result <- data.frame(Var = colnames(x.perc), matrix(t(x.perc), nrow = ncol(x.perc), dimnames = list(NULL, rownames(x.perc))), check.names = FALSE)

        # Valid Percentages table
        v.perc.result <- data.frame(Var = colnames(x.v.perc), matrix(t(x.v.perc), nrow = ncol(x.v.perc), dimnames = list(NULL, rownames(x.v.perc))), check.names = FALSE)

      }

      result <- list(freq = abs.result, perc = perc.result, v.perc = v.perc.result)

    #...................
    ### split = TRUE ####

    } else {

      result <- lapply(x, function(y) misty::freq(y, labels = labels, val.col = val.col, round = round, as.na = as.na, check = FALSE, output = FALSE)$result)
    }

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "freq",
                 data = x,
                 args = list(print = print, freq = freq, split = split, labels = labels, val.col = val.col, round = round, digits = digits, as.na = as.na, write = write, append = append, check = check, output = output),
                 result = result)

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Write Results --------------------------------------------------------------

  if (isTRUE(!is.null(write))) { .write.result(object = object, write = write, append = append) }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}

#_______________________________________________________________________________
