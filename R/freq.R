#' Frequency Table
#'
#' This function computes a frequency table with absolute and percentage frequencies
#' for one or more than one variable.
#'
#' By default, the function displays the absolute and percentage frequencies when
#' specifying one variable, while the function displays only the absolute frequencies
#' when more than one variable is specified. The function displays valid percentage
#' frequencies only in the presence of missing values and excludes variables with
#' all values missing from the analysis. Note that it is possible to mix numeric
#' variables, factors, and character variables in the data frame specified in the
#' argument \code{data}. By default, numeric variables are rounded to three digits
#' before computing the frequency table.
#'
#' @param data     a vector, factor, or data frame.
#' @param ...      an expression indicating the variable names in \code{data},
#'                 e.g., \code{freq(dat, x1, x2, x3)}. Note that the operators
#'                 \code{.}, \code{+}, \code{-}, \code{~}, \code{:}, \code{::},
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
#' \item{\code{result}}{list with result tables, i.e., \code{freq} for absolute
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
#' \dontrun{
#' # Example 8a: Write Results into a text file
#' freq(mtcars, cyl, gear, carb, split = TRUE, write = "Frequencies.txt")
#'
#' # Example 8b: Write Results into a Excel file
#' freq(mtcars, cyl, gear, carb, split = TRUE, write = "Frequencies.xlsx")
#' }
freq <- function(data, ..., print = c("no", "all", "perc", "v.perc"),
                 freq = TRUE, split = FALSE, labels = TRUE, val.col = FALSE,
                 round = 3, exclude = 15, digits = 2, as.na = NULL,
                 write = NULL, append = TRUE, check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'data' is missing
  if (isTRUE(missing(data))) { stop("Please specify a vector, factor, or data frame for the argument 'data'", call. = FALSE) }

  # Check if input 'data' is NULL
  if (isTRUE(is.null(data))) { stop("Input specified for the argument 'data' is NULL.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data using the argument 'data' ####

  if (isTRUE(!missing(...))) {

    # Extract data and convert tibble into data frame or vector
    x <- as.data.frame(data[, .var.names(..., data = data), drop = FALSE])

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data without using the argument 'data' ####

  } else {

    # Data frame
    x <- as.data.frame(data)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) { x <- .as.na(x, na = as.na) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Split message
  message.split <- FALSE

  # Check inputs
  .check.input(logical = c("freq", "labels", "val.col", "append", "output"),
               numeric = list(round = 1L, exclude = 1L),
               s.character = list(print = c("no", "all", "perc", "v.perc")),
               args = c("digits", "write2"), envir = environment(), input.check = check)

  # Additional checks
  if (isTRUE(check)) {

    # Check input 'data': All values missing
    if (isTRUE(all(is.na(x)))) { stop("All values in the vector or data frame specified in the argument 'data' are missing.", call. = FALSE) }

    # Check input 'data': Exclude variables with all missing
    if (isTRUE(length(x) > 1L)) {

      x <- vapply(x, function(y) all(is.na(y)), FUN.VALUE = logical(1L)) |>
        (\(y) if (isTRUE(any(y))) {

          warning(paste0(ifelse(sum(y) == 1L, "Variable ", "Variables "), "with all values missing ", ifelse(sum(y) == 1L, "was ", "were "), "excluded from the analysis: ", paste(names(x)[y], collapse = ", ")), call. = FALSE)

          # Exclude variables with all values missing
          return(x[, !y, drop = FALSE])

        } else {

          return(x)

        })()

    }

    # No frequencies and percentages
    if (isTRUE(all(print == "no") && !isTRUE(freq))) { stop("Please specify \"all\", \"perc\", or \"v.perc\" for the argument 'print' when specifying freq = FALSE.", call. = FALSE) }

    # Check input 'split'
    if (isTRUE(!is.logical(split))) { stop("Please specify TRUE or FALSE for the argument 'split'.", call. = FALSE) }

    if (isTRUE(split && ncol(x) == 1L)) {

      split <- FALSE
      message.split <- TRUE

    }

  }

  #_____________________________________________________________________________
  #
  # Data and Arguments ---------------------------------------------------------

  # Global variable
  na <- NULL

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Argument 'print' ####

  # One variable
  if (isTRUE(length(x) == 1L)) {

    if (isTRUE(all(c("no", "all", "perc", "v.perc") %in% print))) { print <- c("perc", "v.perc") }

  # More than one variable
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
  ## Factor labels ####

  # Factors
  x.factor <- vapply(x, function(y) is.factor(y), FUN.VALUE = logical(1L))

  # If at least one factor and !isTRUE(labels)
  if (isTRUE(any(x.factor)) && !isTRUE(labels)) {

    # More than one factor
    if (isTRUE(sum(x.factor) > 1L)) {

      # Unique factor levels for each variable
      factor.unique <- lapply(x[, x.factor, drop = FALSE], levels)

      if (isTRUE(any(apply(combn(length(x[, x.factor]), 2L), 2L, function(y) !identical(factor.unique[[y[1L]]], factor.unique[[y[2L]]]))))) {

        warning("Variable do not have the same factor levels, i.e., values may not be comparable across variables.", call. = FALSE)

      }

    }

    # Remove labels
    x <- data.frame(lapply(x, function(y) if (isTRUE(is.factor(y))) as.numeric(y) else y))

  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Round numeric variables ####

  # Numeric with more than 'round' digits
  x.numeric <- names(which(vapply(x[, vapply(x, function(y) is.numeric(y) & !is.integer(y), FUN.VALUE = logical(1L)), drop = FALSE],
                     function(y) any(na.omit(nchar(gsub("(.*)(\\.)|([0]*$)","", y)) > round)), FUN.VALUE = logical(1L))))

  # If at least one numeric variable
  if (isTRUE(length(x.numeric) > 0L)) { x[, x.numeric] <- lapply(x[, x.numeric, drop = FALSE], base::round, digits = round) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Exclude variables ####

  if (isTRUE(!isFALSE(exclude))) {

    if (isTRUE(length(x) > 1L && !split)) {

      # Default setting exclude variables with more than 15 unique values
      if (isTRUE(exclude)) { exclude <- 15L }

      x.exclude <- which(vapply(x, function(y) length(unique(na.omit(y))), FUN.VALUE = 1L) > exclude)

      if (isTRUE(length(x.exclude) > 0L)) {

        x <- x[, -x.exclude, drop = FALSE]

        if (isTRUE(length(x) == 0L)) {

          stop(paste0("After excluding variables with more than ", exclude, " unique values, no variables are left for the analysis."), call. = FALSE)

        } else {

          # Rounded numeric variables
          if (isTRUE(length(setdiff(x.numeric, names(x.exclude))) > 0L)) {

            warning(paste0("Numeric variables with more than ", round, " digits were rounded: ", paste(setdiff(x.numeric, names(x.exclude)), collapse = ", ")), call. = FALSE)

          }

          warning(paste0("Variables with more than ", exclude, " unique values were excluded: ", paste(names(x.exclude), collapse = ", ")), call. = FALSE)

        }

      }

    }

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## One variable ####

  if (isTRUE(length(x) == 1L)) {

    # Absolute frequencies
    x.abs <- table(x, useNA = "always")

    # Percentages
    x.perc <- prop.table(x.abs) * 100L

    # Valid percentages
    x.v.perc <- prop.table(table(x, useNA = "no")) * 100L

    #...................
    ### Values in columns ####
    if (!isTRUE(val.col)) {

      freqtab <- data.frame(Value = c(na.omit(suppressWarnings(iconv(na.omit(names(x.abs)), from = "ISO-8859-1", to = "UTF-8"))), NA),
                            Freq = as.numeric(x.abs),
                            Perc = as.numeric(x.perc),
                            V.Perc = c(as.numeric(x.v.perc), NA))

      # Convert 'Value' in numeric
      if(all(grepl("(^(-|\\+)?((\\.?\\d+)|(\\d+\\.\\d+)|(\\d+\\.?))$)|(^(-|\\+)?((\\.?\\d+)|(\\d+\\.\\d+)|(\\d+\\.?))e(-|\\+)?(\\d+)$)", x = na.omit(freqtab$Value)))) {

        # Do not convert "000", "001", "010" etc. into numeric
        if (isTRUE(all(substr(na.omit(freqtab$Value), 1L, 1L) != "0"))) { freqtab$Value <- as.numeric(freqtab$Value) }

      }

    #...................
    ### Values in rows ####
    } else {

      freqtab <- data.frame(matrix(c(x.abs, x.perc, x.v.perc, NA), nrow = 3L, dimnames = list(c ("Freq", "Perc", "V.Perc"), names(x.abs)), byrow = TRUE), check.names = FALSE)

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## More than one variable ####

  if (isTRUE(length(x) > 1L)) {

    #...................
    ### split = FALSE ####
    if (!isTRUE(split)) {

      # Unique levels
      x.levels <- NULL

      # Numeric variables
      if (isTRUE(any(vapply(x, is.numeric, FUN.VALUE = logical(1L))))) {

        x.levels <- c(x.levels, na.omit(sort(unique(unlist(x[, vapply(x, is.numeric, FUN.VALUE = logical(1L))])))))

      }

      # Factors
      if (isTRUE(any(vapply(x, is.factor, FUN.VALUE = logical(1L))))) {

        f.levels <- sort(unique(unname(unlist(sapply(x[, vapply(x, is.factor, FUN.VALUE = logical(1L))], function(y) (as.character(y)))))))

        # Factor levels are numbers
        if (isTRUE(all(unlist(strsplit(f.levels, "")) %in% 0L:9L))) {

          x.levels <- sort(c(x.levels, as.numeric(f.levels)))

        # Factor levels are character
        } else {

          x.levels <- c(x.levels, f.levels)

        }

      }

      # Character
      if (isTRUE(any(vapply(x, is.character, FUN.VALUE = logical(1L))))) { x.levels <- c(x.levels, na.omit(sort(unique(unlist(x[, vapply(x, is.character, FUN.VALUE = logical(1L))]))))) }

      # Logical
      if (isTRUE(any(vapply(x, is.logical, FUN.VALUE = logical(1L))))) { x.levels <- c(x.levels, c(TRUE, FALSE)) }

      x.levels <- unique(x.levels)

      # Absolute frequencies
      x.abs <- vapply(x, function(y) table(factor(y, levels = x.levels), useNA = "always"), FUN.VALUE = integer(length(x.levels) + 1L))

      # Percentages
      x.perc <- apply(x.abs, 2, function(y) ifelse(y != 0L, prop.table(y) * 100L, 0L))

      # Valid Percentages
      x.v.perc <- vapply(x, function(y) table(factor(y, levels = x.levels), useNA = "no"), FUN.VALUE = integer(length(x.levels)))

      # More than one value
      if (isTRUE(is.matrix(x.v.perc))) {

        x.v.perc <- apply(x.v.perc, 2L, function(z) prop.table(z) * 100L)

      # One value
      } else {

        x.v.perc <- matrix(prop.table(x.v.perc) * 100L, nrow = 1, dimnames = list(na.omit(row.names(x.abs)), names(x.v.perc)))

      }

      # Values in rows
      if (!isTRUE(val.col)) {

        # Absolute frequencies table
        abs.freqtab <- data.frame(Value = rownames(x.abs), x.abs, row.names = NULL)

        # Percentages table
        perc.freqtab <- data.frame(Value = rownames(x.perc), x.perc, row.names = NULL)

        # Valid Percentages table
        v.perc.freqtab <- data.frame(Value = rownames(x.v.perc), x.v.perc, row.names = NULL)

        # Convert 'Value' in numeric
        if(all(grepl("(^(-|\\+)?((\\.?\\d+)|(\\d+\\.\\d+)|(\\d+\\.?))$)|(^(-|\\+)?((\\.?\\d+)|(\\d+\\.\\d+)|(\\d+\\.?))e(-|\\+)?(\\d+)$)",
                     x = na.omit(abs.freqtab$Value)))) {

          abs.freqtab$Value <- as.numeric(abs.freqtab$Value)
          perc.freqtab$Value <- as.numeric(perc.freqtab$Value)
          v.perc.freqtab$Value <- as.numeric(v.perc.freqtab$Value)

        }

      # Values in columns
      } else {

        # Absolute frequencies table
        abs.freqtab <- data.frame(Var = colnames(x.abs), matrix(t(x.abs), nrow = ncol(x.abs), dimnames = list(NULL, rownames(x.abs))), check.names = FALSE)

        # Percentages table
        perc.freqtab <- data.frame(Var = colnames(x.perc), matrix(t(x.perc), nrow = ncol(x.perc), dimnames = list(NULL, rownames(x.perc))), check.names = FALSE)

        # Valid Percentages table
        v.perc.freqtab <- data.frame(Var = colnames(x.v.perc), matrix(t(x.v.perc), nrow = ncol(x.v.perc), dimnames = list(NULL, rownames(x.v.perc))), check.names = FALSE)

      }

      freqtab <- list(freq = abs.freqtab, perc = perc.freqtab, v.perc = v.perc.freqtab)

    #...................
    ### split = TRUE ####
    } else {

      freqtab <- lapply(x, function(y) misty::freq(y, labels = labels, val.col = val.col, round = round, as.na = as.na, check = FALSE, output = FALSE)$result)
    }

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "freq",
                 data = x,
                 args = list(print = print, freq = freq, split = split, labels = labels, val.col = val.col, round = round, digits = digits, as.na = as.na, write = write, append = append, check = check, output = output),
                 result = freqtab)

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Write Results --------------------------------------------------------------

  if (isTRUE(!is.null(write))) { .write.result(object = object, write = write, append = append) }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Message ####

  if (isTRUE(message.split)) { message("Argument 'split' set to FALSE because only one variable was specified in 'data'.") }

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}

#_______________________________________________________________________________
