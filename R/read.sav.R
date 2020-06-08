#' Read SPSS File
#'
#' This function calls the \code{read_spss} function in the \pkg{haven} package by Hadley Wickham and Evan Miller (2019)
#' to read an SPSS file.
#'
#' @param file             a character string indicating the name of the SPSS data file
#'                         with or without file extension '.sav', e.g., \code{"My_SPSS_Data.sav"}
#'                         or \code{"My_SPSS_Data"}.
#' @param use.value.labels logical: if \code{TRUE}, variables with value labels are converted into factors.
#' @param use.missings     logical: if \code{TRUE} (default), user-defined missing values are converted into NAs.
#' @param as.data.frame    logical: if \code{TRUE} (default), function returns a regular data frame (default);
#'                         if \code{FALSE} function returns a tibble.
#' @param check            logical: if \code{TRUE}, argument specification is checked.
#'
#' @author
#' Hadley Wickham and Evan Miller
#'
#' @seealso
#' \code{\link{write.sav}}, \code{\link{read.xlsx}}, \code{\link{read.mplus}}
#'
#' @references
#' Hadley Wickham and Evan Miller (2019). \emph{haven: Import and Export 'SPSS', 'Stata' and 'SAS' Files}.
#' R package version 2.1.1.\url{https://CRAN.R-project.org/package=haven}
#'
#' @return
#' Returns a data frame or tibble.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Read SPSS data
#' read.sav("SPSS_Data.sav")
#' read.sav("SPSS_Data")
#'
#' # Read SPSS data, convert variables with value labels into factors
#' read.sav("SPSS_Data.sav", use.value.labels = TRUE)
#'
#' # Read SPSS data, user-defined missing values are not converted into NAs
#' read.sav("SPSS_Data.sav", use.missing = FALSE)
#'
#' # Read SPSS data as tibble
#' read.sav("SPSS_Data.sav", as.data.frame = FALSE)
#' }
read.sav <- function(file, use.value.labels = FALSE, use.missings = TRUE, as.data.frame = TRUE, check = TRUE) {

  ####################################################################################
  # Input Check

  #......
  # Package haven installed?
  if (!requireNamespace("haven", quietly = TRUE)) {

    stop("Package \"haven\" is needed for this function to work, please install it.",
         call. = FALSE )

  }

  #......
  # Check input 'file'
  if (missing(file)) {

    stop("Please specify a character string indicating the name of the SPSS data file for the argument 'file'",
         call. = FALSE)

  }

  #......
  # File extension .sav
  file <- ifelse(length(grep(".sav", file)) == 0, file <- paste0(file, ".sav"), file)

  #......
  # Check if 'file' exists
  if (!file.exists(file)) {

    stop(paste0("Unable to open SPSS data file: ", sQuote(file), " does not exist."),
         call. = FALSE)

  }

  #......
  # Check input 'check',
  if (!is.logical(check)) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }

  #-----------------------------------------

  if (isTRUE(check)) {

    #......
    # Check input 'use.value.labels'
    if (!is.logical(use.value.labels)) {

      stop("Please specify TRUE or FALSE for the argument 'use.value.labels'.", call. = FALSE)

    }

    #......
    # Check input 'use.missings'
    if (!is.logical(use.missings)) {

      stop("Please specify TRUE or FALSE for the argument 'use.missings'.", call. = FALSE)

    }

    #......
    # Check input 'as.data.frame'
    if (!is.logical(as.data.frame)) {

      stop("Please specify TRUE or FALSE for the argument 'as.data.frame'.", call. = FALSE)

    }

  }

  ####################################################################################
  # Data and Arguments

  # User-defined missing values
  use_na <- ifelse(isTRUE(use.missings), FALSE, TRUE)

  ####################################################################################
  # Main Function

  #-----------------------------------------
  # Data as data frame
  if (isTRUE(as.data.frame)) {

    object <- as.data.frame(haven::read_spss(file, user_na = use_na), stringsAsFactors = FALSE)

    object.attributes <- lapply(object, function(y) names(attributes(y)))

    #......
    # Factors
    if (any(unlist(object.attributes) == "labels") && isTRUE(use.value.labels)) {

      var.labels.na <- NULL
      for (i in which(vapply(object.attributes, function(y) any(y == "labels"), FUN.VALUE = logical(1)))) {

        # Labels
        labels <- attributes(object[, i])$labels

        # Labels for all values?
        if (any(!na.omit(unique(object[, i])) %in% labels)) {

          var.labels.na <- c(var.labels.na, i)

          # Attach values without labels to levels
          labels.na <- unique(object[, i])[!unique(object[, i]) %in% labels]

          object[, i] <- factor(object[, i], levels = c(labels, labels.na), labels = c(names(labels), labels.na))

        } else {

          object[, i] <- factor(object[, i], levels = labels, labels = names(labels))

        }

      }

      if (!is.null(var.labels.na)) {

        warning(paste0("Value labels are not specified for all values of the variable: ",
                       paste(colnames(object[, var.labels.na, drop = FALSE]), collapse = ", ")), call. = FALSE)

      }

    }

  #-----------------------------------------
  # Data as tibble
  } else {

    object <- haven::read_spss(file, user_na = use_na)

  }

  return(object)

}
