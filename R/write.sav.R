#' Write SPSS File
#'
#' This function writes a data frame or matrix into a SPSS file by either using
#' the \code{write_sav()} function in the \pkg{haven} package by Hadley Wickham
#' and Evan Miller (2019) or the free software \emph{PSPP} (see:
#' \url{https://www.gnu.org/software/pspp/pspp.html}).
#'
#' If arguments \code{pspp.path} is not specified (i.e., \code{pspp.path = NULL}),
#' \code{write_sav()} function in the \pkg{haven} is used. Otherwise the object
#' \code{x} is written as CSV file, which is subsequently imported into SPSS using
#' the free software \emph{PSPP} by executing a SPSS syntax written in R. Note that
#' \emph{PSPP} needs to be installed on your computer when using the \code{pspp.path}
#' argument.
#'
#' A SPSS file with 'variable labels', 'value labels', and 'user-missing values' is
#' written by specifying the \code{var.attr} argument. Note that the number of rows
#' in the matrix or data frame specified in \code{var.attr} needs to match with the
#' number of columns in the data frame or matrix specified in \code{x}, i.e., each
#' row in \code{var.attr} represents the variable attributes of the corresponding
#' variable in \code{x}. In addition, column names of the matrix or data frame
#' specified in \code{var.attr} needs to be labeled as \code{label} for 'variable
#' labels, \code{values} for 'value labels', and \code{missing} for 'user-missing
#' values'.
#'
#' Labels for the values are defined in the column \code{values} of the matrix or
#' data frame in \code{var.attr} using the equal-sign (e.g., \code{0 = female}) and
#' are separated by a semicolon (e.g., \code{0 = female; 1 = male}).
#'
#' User-missing values are defined in the column \code{missing} of the matrix or
#' data frame in \code{var.attr}, either specifying one user-missing value (e.g.,
#' \code{-99}) or more than one but up to three user-missing values separated
#' by a semicolon (e.g., \code{-77; -99}.
#'
#' @param x         a matrix or data frame to be written in SPSS, vectors are
#'                  coerced to a data frame.
#' @param file      a character string naming a file with or without file extension
#'                  '.sav', e.g., \code{"My_SPSS_Data.sav"} or \code{"My_SPSS_Data"}.
#' @param var.attr  a matrix or data frame with variable attributes used in the
#'                  SPSS file, only 'variable labels' (column name \code{label}),
#'                  'value labels' column name \code{values}, and 'user-missing values'
#'                  column name \code{missing} are supported (see 'Details').
#' @param pspp.path a character string indicating the path where the PSPP folder
#'                  is located on the computer, e.g.\code{C:/Program Files/PSPP/}.
#' @param digits    an integer value indicating the number of decimal places shown
#'                  in the SPSS file for non-integer variables.
#' @param write.csv logical: if \code{TRUE}, CSV file is written along with the
#'                  SPSS file.
#' @param sep       a character string for specifying the CSV file, either \code{";"}
#'                  for the separator and \code{"."}
#'                  for the decimal point (default, i.e. equivalent to
#'                  \code{write.csv2}) or \code{"."} for the decimal point and
#'                  \code{","} for the separator (i.e. equivalent to
#'                  \code{write.csv}), must be one of both \code{";"} (default)
#'                  or \code{","}.
#' @param na        a character string for specifying missing values in the CSV file.
#' @param write.sps logical: if \code{TRUE}, SPSS syntax is written along with
#'                  the SPSS file when using PSPP.
#' @param check     logical: if \code{TRUE} (default), variable attributes
#'                  specified in the argument \code{var.attr} is checked.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{read.sav}}, \code{\link{write.xlsx}}, \code{\link{write.dta}},
#' \code{\link{write.mplus}}
#'
#' @references
#' GNU Project (2018). \emph{GNU PSPP for GNU/Linux} (Version 1.2.0).
#' Boston, MA: Free Software Foundation. \url{https://www.gnu.org/software/pspp/}
#'
#' Wickham H., & Miller, E. (2019). \emph{haven: Import and Export 'SPSS', 'Stata'
#' and 'SAS' Files}. R package version 2.2.0. \url{https://CRAN.R-project.org/package=haven}
#'
#' Robitzsch, A., Grund, S., & Henke, T. (2019). \emph{miceadds: Some additional
#' multiple imputation functions, especially for mice}. R package version 3.4-17.
#' \url{https://CRAN.R-project.org/package=miceadds}
#'
#' @note
#' Part of the function using \emph{PSPP} was adapted from the \code{write.pspp()}
#' function in the \pkg{miceadds} package by Alexander Robitzsch, Simon Grund and
#' Thorsten Henke (2019).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(id = 1:5,
#'                   gender = c(NA, 0, 1, 1, 0),
#'                   age = c(16, 19, 17, NA, 16),
#'                   status = c(1, 2, 3, 1, 4),
#'                   score = c(511, 506, 497, 502, 491))
#'
#' # Example 1: Write SPSS file using the haven package
#' write.sav(dat, file = "Dataframe_haven.sav")
#'
#' # Example 2: Write SPSS file using PSPP,
#' # write CSV file and SPSS syntax along with the SPSS file
#' write.sav(dat, file = "Dataframe_PSPP.sav", pspp.path = "C:/Program Files/PSPP",
#'           write.csv = TRUE, write.sps = TRUE)
#'
#' # Example 3: Specify variable attributes
#' # Note that it is recommended to manually specify the variables attritbues in a CSV or
#' # Excel file which is subsequently read into R
#' attr <- data.frame(# Variable names
#'                    var = c("id", "gender", "age", "status", "score"),
#'                    # Variable labels
#'                    label = c("Identification number", "Gender", "Age in years",
#'                              "Migration background", "Achievement test score"),
#'                    # Value labels
#'                    values = c("", "0 = female; 1 = male", "",
#'                               "1 = Austria; 2 = former Yugoslavia; 3 = Turkey; 4 = other",
#'                               ""),
#'                    # User-missing values
#'                    missing = c("", "-99", "-99", "-99", "-99"), stringsAsFactors = FALSE)
#'
#' # Example 4: Write SPSS file with variable attributes using the haven package
#' write.sav(dat, file = "Dataframe_haven_Attr.sav", var.attr = attr)
#'
#' # Example 5: Write SPSS with variable attributes using PSPP
#' write.sav(dat, file = "Dataframe_PSPP_Attr.sav", var.attr = attr,
#'           pspp.path = "C:/Program Files/PSPP")
#' }
write.sav <- function(x, file = "SPSS_Data.sav", var.attr = NULL, pspp.path = NULL, digits = 2,
                      write.csv = FALSE, sep = c(";", ","), na = "", write.sps = FALSE, check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'x' is missing
  if (isTRUE(missing(x))) { stop("Please specify a matrix or data frame for the argument 'x'.", call. = FALSE) }

  # Matrix or data frame for the argument 'x'?
  if (isTRUE(!is.matrix(x) && !is.data.frame(x))) { stop("Please specifiy a matrix or data frame for the argument 'x'.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data and Variables ---------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data frame ####

  x <- as.data.frame(x, stringsAsFactors = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Variable names ####

  varnames <- colnames(x)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Number of variables ####

  var.length <- length(varnames)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## File extension .sav ####

  file <- ifelse(length(grep(".sav", file)) == 1L, file <- gsub(".sav", "", file), file)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Separator ####

  sep <- ifelse(all(c(";", ".") %in% sep), ";", sep)

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # Check input 'pspp.path'
    if (isTRUE(!is.null(pspp.path))) {

      if (isTRUE(length(grep("pspp.exe", list.files(paste0(pspp.path, "/bin/")))) != 1L)) { stop("PSPP file \'pspp.exe\' was not found in the folder specified in the pspp.path argument.", call. = FALSE) }

    }

    # Check input 'var.attr'
    if (isTRUE(!is.null(var.attr))) {

      # Number of rows in var.attr match with number of columns in x?
      if (isTRUE(nrow(var.attr) != ncol(x))) { stop("Number of rows in the data frame or matrix specified in the argument var.attr does not match with the number of columns in x.", call. = FALSE) }

      # Any column name "label", "values", "missing"?
      if (isTRUE(all(is.na(match(names(var.attr), c("label", "values", "missing")))))) { stop("None of the column names of the data frame or matrix specified in the argument var.attr match with \"label\", \"values\" or \"missing\".", call. = FALSE) }

      # Value labels match with data?
      if (isTRUE(any(!is.na(match(names(var.attr), "values"))))) {

        for (i in seq_len(var.length)) {

          value.labels <- as.character(var.attr[i, "values"])

          if (isTRUE(value.labels != "")) {

            value.labels.split <- unlist(strsplit(value.labels, ";"))

            value.labels.split.matrix <- matrix(misty::chr.trim(unlist(sapply(value.labels.split, function(y) strsplit(y, "=")))), ncol = length(value.labels.split))

            # Duplicated values
            if (isTRUE(any(duplicated(value.labels.split.matrix[1L, ])))) {

              stop(paste0("Values in the column \"values\" specified in 'var.attr' are duplicated for the variable '", varnames[i], "'."), call. = FALSE)

            }

            # Values specified in 'values' in the data
            if(isTRUE(!all(as.numeric(value.labels.split.matrix[1, ]) %in% x[, varnames[i]]))) {

              warning(paste0("Values in the column \"values\" specified in 'var.attr' do not all match with the variable '", varnames[i], "'."), call. = FALSE)

            }

          }

        }

      }

    }

    # Check input 'digits'
    if (isTRUE(digits %% 1L != 0L || digits < 0L)) { stop("Specify a positive integer number for the argument digits.", call. = FALSE) }

    # Check input 'sep'
    if (isTRUE(write.csv & any(!sep %in% c(";", ",")))) { stop("Specify either \";\" or \",\" for the argument sep.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Variable attributes ####

  if (isTRUE("label" %in% names(var.attr))) { var.attr[, "label"] <- na.as(var.attr[, "label"], na = "", check = FALSE) }
  if (isTRUE("values" %in% names(var.attr))) { var.attr[, "values"] <- na.as(var.attr[, "values"], na = "", check = FALSE) }
  if (isTRUE("missing" %in% names(var.attr))) { var.attr[, "missing"] <- na.as(var.attr[, "missing"], na = "", check = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Use haven package ####

  if (isTRUE(is.null(pspp.path))) {

    # Package haven installed?
    if (isTRUE(!requireNamespace("haven", quietly = TRUE))) { stop("Package \"haven\" is needed for this function to work, please install it.", call. = FALSE ) }

    #...................
    ### Without variable attributes ####
    if (isTRUE(is.null(var.attr))) {

      # Write .sav
      haven::write_sav(x, paste0(file, ".sav"), compress = FALSE)

    #...................
    ### Witht variable attributes ####
    } else {

      # Variable labels, value labels, and user-missing values
      if (isTRUE("label" %in% names(var.attr))) { label <- as.character(var.attr[, match("label", colnames(var.attr))]) } else { label <- rep("", times = nrow(var.attr)) }
      if (isTRUE("values" %in% names(var.attr))) { labels <- as.character(var.attr[, match("values", colnames(var.attr))]) } else { labels <- rep("", times = nrow(var.attr)) }
      if (isTRUE("missing" %in% names(var.attr))) { na <- as.character(var.attr[, match("missing", colnames(var.attr))]) } else { na <- rep("", times = nrow(var.attr)) }

      # For numeric variables only, i.e., exclude factors, strings, and dates
      for (i in which(vapply(x, is.numeric, FUN.VALUE = logical(1L)))) {

        ##### Value labels ####
        if (misty::chr.trim(labels[i]) != "") {

          x.labels <- unlist(strsplit(labels[i], ";"))

          x.labels <- matrix(misty::chr.trim(unlist(sapply(x.labels, function(y) strsplit(y, "=")))), ncol = length(x.labels))

          labels.i <- paste0("c(", paste(apply(x.labels, 2L, function(y) paste(paste0("\"", y[2L], "\""), y[1L], sep = " = ")), collapse = ", "), ")")

        } else {

          labels.i <- NULL

        }

        ##### User-missing values ####
        if (isTRUE(misty::chr.trim(na[i]) != "")) {

          na.i <- paste0("c(", paste(misty::chr.trim(unlist(strsplit(na[i], ";"))), collapse = ", "), ")")

        } else {

          na.i <- NULL

        }

        # Labelled vectors for SPSS
        eval(parse(text = paste0("x$", colnames(x)[i],  " <- haven::labelled_spss(as.double(x$", colnames(x)[i], "), labels = ", ifelse(is.null(labels.i), "NULL", labels.i), ", na_values = ", ifelse(is.null(na.i), "NULL", na.i), ", label = \"", label[i], "\")")))

        # Zero digits for integer values
        if (isTRUE(all(na.omit(x[, i]) %% 1L == 0L))) {

          eval(parse(text = paste0("attr(x$", colnames(x)[i], ", \"format.spss\") <- \"F8.0\"")))

        } else {

          eval(parse(text = paste0("attr(x$", colnames(x)[i], ", \"format.spss\") <- \"F8.", digits, "\"")))

        }

      }

      # Write .sav
      haven::write_sav(x, paste0(file, ".sav"), compress = FALSE)

    }

    # Save CSV file
    if (isTRUE(write.csv)) {

      if (isTRUE(sep == ";")) {

        write.csv2(x, paste0(file, ".csv"), row.names = FALSE, quote = FALSE, na = na)

      } else {

        write.csv(x, paste0(file, ".csv"), row.names = FALSE, quote = FALSE, na = na)

      }

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Use PSPP ####
  } else {

    # Function to add quotes
    add.quote <- function(x) { paste0("\"", x, "\"") }

    # Convert factors and logical to numeric
    any.factors <- any(vapply(x, is.factor, FUN.VALUE = logical(1L)))

    if (isTRUE(any.factors)) {

      xf <- data.frame(lapply(x, function(x) if (is.factor(x) | is.logical(x)) as.numeric(x) else x), stringsAsFactors = FALSE)

    } else {

      xf <- x

    }

    # Write CSV

    utils::write.csv2(xf, paste0(file, ".csv"), row.names = FALSE, quote = FALSE, na = na)

    #...................
    ### Variable formats ####

    type <- rep("F", times = var.length)
    width <- rep(8L, times = var.length)
    decimals <- rep(NA, times = var.length)

    for (i in seq_len(var.length)) {

      # Numeric
      if (isTRUE(is.numeric(xf[, i]))) {

        i.nchar <- nchar(round(xf[, i], digits = digits))

        # Size of the elements > 8
        if (isTRUE(any(na.omit(i.nchar) > 8L))) { width[i] <- max(i.nchar) }

        # Digits for numeric = 2, digits for integer = 0
        decimals[i] <- ifelse(is.integer(xf[, i]), 0L, digits)
        decimals[i] <- ifelse(all(xf[, i] %% 1L == 0L), 0, digits)

      } else {

        # Character
        type[i] <- "A"
        width[i] <- max(nchar(xf[, i]))

      }

    }

    # PSPP variable format
    variables <- paste(varnames, ifelse(!is.na(decimals), paste0(type, paste(width, decimals, sep = ".")), paste0(type, width)), collapse = "\n  ")

    #...................
    ### Write PSPP syntax ####
    code <- paste0(file, ".sps")

    cat(paste0("GET DATA\n",
               "  /TYPE=TXT \n",
               "  /FILE='",  getwd(), "/", file, ".csv' \n",
               "  /ARRANGEMENT=DELIMITED\n",
               "  /DELCASE=LINE \n",
               "  /FIRSTCASE=2 \n",
               "  /DELIMITERS=';'\n" ,
               "  /QUALIFIER='' \n" ,
               "  /VARIABLES=\n"),  file = code)
    cat(paste0("  ", variables, " ."), file = code, append = TRUE)

    #...................
    ### Variable attributes ####

    # Attributes from object var.attr
    if (isTRUE(!is.null(var.attr))) {

      ###
      # Define variable labels

      label <- as.character(var.attr[, match("label", colnames(var.attr))])

      indices <- which(label != "")

      # PSPP variable labels
      variable.label <- paste(varnames[indices], add.quote(label[indices]), collapse = " \n  ")

      cat("\nVARIABLE LABELS\n ", file = code, append = TRUE)
      cat(paste0(" ", variable.label, " ."), file = code, append = TRUE)

      ###
      # Define value labels

      # Values from variable attributes
      if (isTRUE(any(var.attr[, match("values", colnames(var.attr))] != ""))) {

        for (i in seq_len(var.length)) {

          value.labels <- as.character(var.attr[i, match("values", colnames(var.attr))])

          if (isTRUE(value.labels != "")) {

            x <- unlist(strsplit(value.labels, ";"))

            x <- matrix(misty::chr.trim(unlist(sapply(x, function(x) strsplit(x, "=")))), ncol = length(x))

            cat("\nVALUE LABELS\n",
                paste0(" ", varnames[i], paste0(paste0(" ", x[1L, ], " '", x[2L, ], sep = "'"), collapse = "")), ".", file = code, append = TRUE)

          }

        }

      }

      ###
      # Values from factor levels
      if (isTRUE(any.factors)) {

        x.factor <- which(vapply(x, is.factor, FUN.VALUE = logical(1)))

        for (i in x.factor) {

          values <- unique(as.numeric(x[, i]))
          labels <- levels(x[, i])

          cat("\nVALUE LABELS\n",
              paste0(" ", names(xf)[i], paste0(paste0(" ", values, " '", labels, sep = "'"), collapse = "")), ".", file = code, append = TRUE)

        }

      }

      ###
      # Define missing values

      miss.unique <- unique(misty::chr.trim(as.character(unique(var.attr[, match("missing", colnames(var.attr))]))))
      miss.unique <- miss.unique[!miss.unique %in% c("", NA)]

      # One pattern of missing data values
      if (isTRUE(length(miss.unique) == 1L)) {

        cat(paste0("\nMISSING VALUES\n  ", paste(varnames[which(var.attr$missing == miss.unique)], collapse = " "),
                  " (", gsub(";", " ", miss.unique), ")", "."), file = code, append = TRUE)

      }

      ###
      # More than one pattern of missing data values
      if (isTRUE(length(miss.unique) > 1L)) {

        for (i in seq_len(var.length)) {

          missing.values <- var.attr[i, match("missing", colnames(var.attr))]
          if (isTRUE(missing.values != "")) {

            cat("\nMISSING VALUES\n  " , paste0(varnames[i],  " (", paste(gsub(";", " ", missing.values), collapse = " "), ")", "."), file = code, append = TRUE)

          }

        }

      }

    #......
    # Object var.attr not available
    } else {

      # Values from factor levels
      if (isTRUE(any.factors)) {

        x.factor <- which(vapply(x, is.factor, FUN.VALUE = logical(1)))

        for (i in x.factor) {

          values <- unique(as.numeric(x[, i]))
          labels <- levels(x[, i])

          cat("\nVALUE LABELS\n",
              paste0(" ", names(xf)[i], paste0(paste0(" ", values, " '", labels, sep = "'"), collapse = "")), ".", file = code, append = TRUE)

        }

      }

    }

    #...................
    ### Save PSPP ####

    cat("\nEXECUTE.\n", file = code, append = TRUE)

    cat(paste0( "\nSAVE OUTFILE='", getwd() , "/" , file , ".sav'.\nEXECUTE."),
        file = code, append = TRUE)

    #...................
    ### Run PSPP ####

    system(paste0("\"", pspp.path, "/bin/pspp.exe\" ", code))

    #...................
    ### Remove sps and csv file ####

    if (!isTRUE(write.sps)) { unlink(paste0(file, ".sps")) }

    if (!isTRUE(write.csv) | sep == ",") { unlink(paste0(file, ".csv")) }

    if (isTRUE(write.csv) & sep == ",") { utils::write.csv(xf, paste0(file, ".csv"), row.names = FALSE, quote = FALSE, na = na) }

  }

}
