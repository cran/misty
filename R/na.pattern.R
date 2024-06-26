#' Missing Data Pattern
#'
#' This function computes a summary of missing data patterns, i.e., number (%)
#' of cases with a specific missing data pattern.
#'
#' @param ...     a matrix or data frame with incomplete data, where missing
#'                values are coded as \code{NA}. Alternatively, an expression
#'                indicating the variable names in \code{data} e.g.,
#'                \code{na.pattern(x1, x2, x3, data = dat)}. Note that the operators
#'                \code{.}, \code{+}, \code{-}, \code{~}, \code{:}, \code{::},
#'                and \code{!} can also be used to select variables, see 'Details'
#'                in the \code{\link{df.subset}} function.
#' @param data    a data frame when specifying one or more variables in the
#'                argument \code{...}. Note that the argument is \code{NULL}
#'                when specifying a matrix or data frame for the argument \code{...}.
#' @param order   logical: if \code{TRUE}, variables are ordered from left to
#'                right in increasing order of missing values.
#' @param digits  an integer value indicating the number of decimal places to
#'                be used for displaying percentages.
#' @param as.na   a numeric vector indicating user-defined missing values,
#'                i.e. these values are converted to NA before conducting the
#'                analysis.
#' @param write   a character string naming a file for writing the output into
#'                either a text file with file extension \code{".txt"} (e.g.,
#'                \code{"Output.txt"}) or Excel file with file extention
#'                \code{".xlsx"}  (e.g., \code{"Output.xlsx"}). If the file
#'                name does not contain any file extension, an Excel file will
#'                be written.
#' @param append  logical: if \code{TRUE} (default), output will be appended
#'                to an existing text file with extension \code{.txt} specified
#'                in \code{write}, if \code{FALSE} existing text file will be
#'                overwritten.
#' @param check   logical: if \code{TRUE} (default), argument specification is checked.
#' @param output  logical: if \code{TRUE} (default), output is shown.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{as.na}}, \code{\link{na.as}}, \code{\link{na.auxiliary}},
#' \code{\link{na.coverage}}, \code{\link{na.descript}}, \code{\link{na.indicator}},
#' \code{\link{na.prop}}, \code{\link{na.test}}, \code{\link{write.result}}
#'
#' @references
#' Enders, C. K. (2010). \emph{Applied missing data analysis}. Guilford Press.
#'
#' Graham, J. W. (2009). Missing data analysis: Making it work in the real world.
#' \emph{Annual Review of Psychology, 60}, 549-576.
#' https://doi.org/10.1146/annurev.psych.58.110405.085530
#'
#' van Buuren, S. (2018). \emph{Flexible imputation of missing data} (2nd ed.).
#' Chapman & Hall.
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#' \tabular{ll}{
#' \code{call} \tab function call \cr
#' \code{type} \tab type of analysis \cr
#' \code{data} \tab data frame used for the current analysis \cr
#' \code{args} \tab specification of function arguments \cr
#' \code{result} \tab result table \cr
#' }
#'
#' @export
#'
#' @examples
#' # Example 1a: Compute a summary of missing data patterns
#' dat.pattern <- na.pattern(airquality)
#'
#' # Example 1b: Alternative specification using the 'data' argument
#' dat.pattern <- na.pattern(., data = airquality)
#'
#' # Example 2: Vector of missing data pattern for each case
#' dat.pattern$pattern
#
#' # Data frame without cases with missing data pattern 2 and 4
#' airquality[!dat.pattern$pattern %in% c(2, 4), ]
#'
#' \dontrun{
#' # Example 3a: Write Results into a text file
#' result <- na.pattern(airquality, write = "NA_Pattern.xlsx")
#'
#' # Example 3b: Write Results into a Excel file
#' result <- na.pattern(airquality, write = "NA_Pattern.xlsx")
#'
#' result <- na.pattern(dat, output = FALSE)
#' write.result(result, "NA_Pattern.xlsx")
#' }
na.pattern <- function(..., data = NULL, order = FALSE, digits = 2, as.na = NULL,
                       write = NULL, append = TRUE, check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input '...' is missing
  if (isTRUE(missing(...))) { stop("Please specify the argument '...'.", call. = FALSE) }

  # Check if input '...' is NULL
  if (isTRUE(is.null(substitute(...)))) { stop("Input specified for the argument '...' is NULL.", call. = FALSE) }

  # Check if input 'data' is data frame
  if (isTRUE(!is.null(data) && !is.data.frame(data))) { stop("Please specify a data frame for the argument 'data'.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data using the argument 'data' ####

  if (isTRUE(!is.null(data))) {

    # Variable names
    var.names <- .var.names(..., data = data, check.chr = "a matrix or data frame")

    # Extract data
    x <- data[, var.names]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data without using the argument 'data' ####

  } else {

    # Extract data
    x <- eval(..., enclos = parent.frame())

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## As data frame ####

  x <- as.data.frame(x, stringsAsFactors = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) { x <- .as.na(x, na = as.na) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # Check input 'order'
    if (isTRUE(!is.logical(order))) { stop("Please specify TRUE or FALSE for the argument 'order'.", call. = FALSE) }

    # Check input 'digits'
    if (isTRUE(digits %% 1L != 0L || digits < 0L)) { stop("Please specify a positive integer value for the argument 'digits'.", call. = FALSE) }

    # Check input 'append'
    if (isTRUE(!is.logical(append))) { stop("Please specify TRUE or FALSE for the argument 'append'.", call. = FALSE) }

    # Check input 'output'
    if (isTRUE(!is.logical(output))) { stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  # Missing data TRUE/FALSE matrix
  x.na <- is.na(x)

  # Number of missing values for each variable
  x.na.var <- colSums(x.na)

  if (isTRUE(order)) {

    x.na <- x.na[, order(x.na.var)]

  }

  # Missing data pattern
  patt <- apply(x.na, 1L, function(y) paste(as.numeric(y), collapse = ""))

  # Order NA matrix
  x.na.order <- x.na[order(patt), ]

  # Remove duplicated rows
  x.na.order.dupl <- x.na.order[!duplicated(x.na.order), ]

  if (isTRUE(!is.null(dim(x.na.order.dupl)))) {

    restab <- rbind(data.frame(pattern = seq_len(nrow(x.na.order.dupl)),
                               n = as.vector(table(patt)),
                               perc = as.vector(table(patt) / nrow(x.na) * 100L),
                               abs(x.na.order.dupl - 1L),
                               nNA = rowSums(x.na.order.dupl),
                               pNA = rowSums(x.na.order.dupl) / ncol(x.na) * 100L,
                               row.names = NULL, stringsAsFactors = FALSE),
                    c(NA, sum(as.vector(table(patt))), sum(as.vector(table(patt) / nrow(x.na) * 100L)), colSums(x.na), NA, NA))

    # Number of missing data pattern
    pattern <- unname(vapply(apply(x.na[, colnames(x.na.order.dupl)], 1L,  paste, collapse = " "), function(y) match(y, apply(x.na.order.dupl, 1L, paste, collapse = " ")), FUN.VALUE = 1L))

  } else {

    restab <- rbind(data.frame(pattern = 1L,
                               n = as.vector(table(patt)),
                               perc = as.vector(table(patt) / nrow(x.na) * 100L),
                               matrix(abs(x.na.order.dupl - 1L), ncol = length(x.na.order.dupl), dimnames = list(NULL, colnames(x.na.order.dupl))),
                               nNA = sum(x.na.order.dupl),
                               pNA = sum(x.na.order.dupl) / ncol(x.na) * 100L,
                               row.names = NULL, stringsAsFactors = FALSE),
                    c(NA, sum(as.vector(table(patt))), sum(as.vector(table(patt) / nrow(x.na) * 100L)), colSums(x.na), NA, NA))

    pattern <- rep(1, times = nrow(x))

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "na.pattern",
                 data = x,
                 args = list(order = order, digits = digits, as.na = as.na,
                             write = write, append = append, check = check, output = output),
                 result = restab,
                 pattern = pattern)

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Write results ####

  if (isTRUE(!is.null(write))) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Text file ####

    if (isTRUE(grepl("\\.txt", write))) {

      # Send R output to textfile
      sink(file = write, append = ifelse(isTRUE(file.exists(write)), append, FALSE), type = "output", split = FALSE)

      if (isTRUE(append && file.exists(write))) { write("", file = write, append = TRUE) }

      # Print object
      print(object, check = FALSE)

      # Close file connection
      sink()

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Excel file ####

    } else {

      misty::write.result(object, file = write)

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## output ####

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
