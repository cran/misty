#' Read Stata DTA File
#'
#' This function calls the \code{read_dta} function in the \pkg{haven} package
#' by Hadley Wickham, Evan Miller and Danny Smith (2023) to read a Stata DTA file.
#'
#' @param file             a character string indicating the name of the Stata
#'                         data file with or without file extension '.dta', e.g.,
#'                         \code{"Stata_Data.dta"} or \code{"Stata_Data"}.
#' @param use.value.labels logical: if \code{TRUE}, variables with value labels
#'                         are converted into factors.
#' @param formats          logical: if \code{TRUE} (default), variable formats are
#'                         shown in an attribute for all variables.
#' @param label            logical: if \code{TRUE}, variable labels are
#'                         shown in an attribute for all variables.
#' @param labels           logical: if \code{TRUE}, value labels are
#'                         shown in an attribute for all variables.
#' @param missing          logical: if \code{TRUE}, convert tagged missing values
#'                         to regular R \code{NA}.
#' @param widths           logical: if \code{TRUE}, widths are shown in an attribute
#'                         for all variables.
#' @param as.data.frame    logical: if \code{TRUE} (default), function returns a
#'                         regular data frame;
#'                         if \code{FALSE} function returns a tibble.
#' @param check            logical: if \code{TRUE} (default), argument specification
#'                         is checked.
#'
#' @author
#' Hadley Wickham and Evan Miller
#'
#' @seealso
#' \code{\link{read.sav}}, \code{\link{write.sav}}, \code{\link{read.xlsx}},
#'  \code{\link{write.xlsx}}, \code{\link{read.mplus}}, \code{\link{write.mplus}}
#'
#' @references
#' Wickham H, Miller E, Smith D (2023). \emph{haven: Import and Export 'SPSS',
#' 'Stata' and 'SAS' Files}. R package version 2.5.3.
#' \url{https://CRAN.R-project.org/package=haven}
#'
#' @return
#' Returns a data frame or tibble.
#'
#' @note
#' This function is a modified copy of the \code{read_dta()} function in the
#' \pkg{haven} package by Hadley Wickham, Evan Miller and Danny Smith (2023).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1: Read Stata data file
#' read.dta("Stata_Data.dta")
#' read.dta("Stata_Data")
#'
#' # Example 2: Read Stata data, convert variables with value labels into factors
#' read.dta("Stata_Data.dta", use.value.labels = TRUE)
#'
#' # Example 3: Read Stata data as tibble
#' read.dta("Stata_Data.dta", as.data.frame = FALSE)
#' }
read.dta <- function(file, use.value.labels = FALSE, formats = FALSE, label = FALSE,
                     labels = FALSE, missing = FALSE, widths = FALSE,
                     as.data.frame = TRUE, check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check input 'file'
  if (isTRUE(missing(file))) { stop("Please specify a character string indicating the name of the Stata data file for the argument 'file'", call. = FALSE) }

  # File extension .dta
  file <- ifelse(!grepl(".dta", file), file <- paste0(file, ".dta"), file)

  # Check if 'file' exists
  if (isTRUE(!file.exists(file))) { stop(paste0("Unable to open Stata data file: ", sQuote(file), " does not exist."), call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check',
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # Package haven installed?
    if (isTRUE(!requireNamespace("haven", quietly = TRUE))) { stop("Package \"haven\" is needed for this function to work, please install it.", call. = FALSE ) }

    # Check input 'use.value.labels'
    if (isTRUE(!is.logical(use.value.labels))) { stop("Please specify TRUE or FALSE for the argument 'use.value.labels'.", call. = FALSE) }

    # Check input 'formats'
    if (isTRUE(!is.logical(formats))) { stop("Please specify TRUE or FALSE for the argument 'formats'.", call. = FALSE) }

    # Check input 'label'
    if (isTRUE(!is.logical(label))) { stop("Please specify TRUE or FALSE for the argument 'label'.", call. = FALSE) }

    # Check input 'labels'
    if (isTRUE(!is.logical(labels))) { stop("Please specify TRUE or FALSE for the argument 'labels'.", call. = FALSE) }

    # Check input 'missing'
    if (isTRUE(!is.logical(missing))) { stop("Please specify TRUE or FALSE for the argument 'missing'.", call. = FALSE) }

    # Check input 'widths'
    if (isTRUE(!is.logical(widths))) { stop("Please specify TRUE or FALSE for the argument 'widths'.", call. = FALSE) }

    # Check input 'as.data.frame'
    if (isTRUE(!is.logical(as.data.frame))) { stop("Please specify TRUE or FALSE for the argument 'as.data.frame'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Data and Arguments ---------------------------------------------------------

  # Value labels
  labels <- ifelse(isTRUE(use.value.labels), TRUE, labels)

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  # Read Stata data
  object <- haven::read_dta(file)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Remove format ####

  if (isTRUE(!formats)) {

    for (i in names(object)) {

      object[, i] <- haven::zap_formats(object[, i])

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Remove Variable Label ####

  if (isTRUE(!label)) {

    for (i in names(object)) {

      object[, i] <- haven::zap_label(object[, i])

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Remove Value Labels ####

  if (isTRUE(!labels)) {

    for (i in names(object)) {

      object[, i] <- haven::zap_labels(object[, i])

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Remove tagged missing values ####

  if (isTRUE(!missing)) {

    for (i in names(object)) {

      object[, i] <- haven::zap_missing(object[, i])

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Remove widths ####

  if (isTRUE(!widths)) {

    for (i in names(object)) {

      object[, i] <- haven::zap_widths(object[, i])

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## As data frame ####

  if (isTRUE(as.data.frame)) {

    object <- as.data.frame(object, stringsAsFactors = FALSE)

    object.attributes <- lapply(object, function(y) names(attributes(y)))

    #...................
    ### Factors ####
    if (isTRUE(any(unlist(object.attributes) == "labels") && use.value.labels)) {

      var.labels.na <- NULL
      for (i in which(vapply(object.attributes, function(y) any(y == "labels"), FUN.VALUE = logical(1L)))) {

        # Labels
        labels <- attributes(object[, i])$labels

        # Labels for all values?
        if (isTRUE(any(!na.omit(unique(object[, i])) %in% labels))) {

          var.labels.na <- c(var.labels.na, i)

          # Attach values without labels to levels
          labels.na <- unique(object[, i])[!unique(object[, i]) %in% labels]

          object[, i] <- factor(object[, i], levels = c(labels, labels.na), labels = c(names(labels), labels.na))

        } else {

          object[, i] <- factor(object[, i], levels = labels, labels = names(labels))

        }

      }

      if (isTRUE(!is.null(var.labels.na))) {

        warning(paste0("Value labels are not specified for all values of the variable: ", paste(colnames(object[, var.labels.na, drop = FALSE]), collapse = ", ")), call. = FALSE)

      }

    }

  }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  return(object)

}
