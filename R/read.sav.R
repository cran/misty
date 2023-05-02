#' Read SPSS File
#'
#' This function calls the \code{read_spss} function in the \pkg{haven} package
#' by Hadley Wickham and Evan Miller (2019) to read an SPSS file.
#'
#' @param file             a character string indicating the name of the SPSS data file
#'                         with or without file extension '.sav', e.g., \code{"My_SPSS_Data.sav"}
#'                         or \code{"My_SPSS_Data"}.
#' @param use.value.labels logical: if \code{TRUE}, variables with value labels are converted into factors.
#' @param use.missings     logical: if \code{TRUE} (default), user-defined missing values are converted into NAs.
#' @param formats          logical: if \code{TRUE}, variable formats are shown in an attribute for all variables.
#' @param label            logical: if \code{TRUE} (default), variable labels are shown in an attribute for all variables.
#' @param labels           logical: if \code{TRUE} (default), value labels are shown in an attribute for all variables.
#' @param missing          logical: if \code{TRUE}, value labels for user-defined missings are shown in an attribute.
#'                         for all variables.
#' @param widths           logical: if \code{TRUE}, widths are shown in an attribute for all variables.
#' @param as.data.frame    logical: if \code{TRUE} (default), function returns a regular data frame (default);
#'                         if \code{FALSE} function returns a tibble.
#' @param check            logical: if \code{TRUE}, argument specification is checked.
#'
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
read.sav <- function(file, use.value.labels = FALSE, use.missings = TRUE, formats = FALSE,
                     label = TRUE, labels = TRUE, missing = FALSE, widths = FALSE,
                     as.data.frame = TRUE, check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Package haven installed?
  if (isTRUE(!requireNamespace("haven", quietly = TRUE))) { stop("Package \"haven\" is needed for this function to work, please install it.", call. = FALSE ) }

  # Check input 'file'
  if (isTRUE(missing(file))) { stop("Please specify a character string indicating the name of the SPSS data file for the argument 'file'", call. = FALSE) }

  # File extension .sav
  file <- ifelse(length(grep(".sav", file)) == 0, file <- paste0(file, ".sav"), file)

  # Check if 'file' exists
  if (isTRUE(!file.exists(file))) { stop(paste0("Unable to open SPSS data file: ", sQuote(file), " does not exist."), call. = FALSE) }

  # Check input 'check',
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  if (isTRUE(check)) {

    # Check input 'use.value.labels'
    if (isTRUE(!is.logical(use.value.labels))) { stop("Please specify TRUE or FALSE for the argument 'use.value.labels'.", call. = FALSE) }

    # Check input 'use.missings'
    if (isTRUE(!is.logical(use.missings))) { stop("Please specify TRUE or FALSE for the argument 'use.missings'.", call. = FALSE) }

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

  # User-defined missing values
  use_na <- ifelse(isTRUE(use.missings), FALSE, TRUE)

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  # Read SPSS data
  object <- haven::read_spss(file, user_na = use_na)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Remove format ####

  if (!isTRUE(formats)) {

    for (i in names(object)) {

      object[, i] <- haven::zap_formats(object[, i])

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Remove label ####

  if (!isTRUE(label)) {

    for (i in names(object)) {

      object[, i] <- haven::zap_label(object[, i])

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Remove value labels for user-defined missing values ####

  if (!isTRUE(missing)) {

    for (i in names(object)) {

      object[, i] <- haven::zap_missing(object[, i])

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Remove widths ####

  if (!isTRUE(widths)) {

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
    if (isTRUE(any(unlist(object.attributes) == "labels") && isTRUE(use.value.labels))) {

      var.labels.na <- NULL
      for (i in which(vapply(object.attributes, function(y) any(y == "labels"), FUN.VALUE = logical(1)))) {

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

        warning(paste0("Value labels are not specified for all values of the variable: ",
                       paste(colnames(object[, var.labels.na, drop = FALSE]), collapse = ", ")), call. = FALSE)

      }

    }

  }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  return(object)

}
