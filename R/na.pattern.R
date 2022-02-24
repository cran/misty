#' Missing Data Pattern
#'
#' This function computes a summary of missing data patterns, i.e., number (%) of
#' cases with a specific missing data pattern.
#'
#' @param x           a matrix or data frame with incomplete data, where missing
#'                    values are coded as \code{NA}.
#' @param order       logical: if \code{TRUE}, variables are ordered from left to
#'                    right in increasing order of missing values.
#' @param digits      an integer value indicating the number of decimal places to
#'                    be used for displaying percentages.
#' @param as.na       a numeric vector indicating user-defined missing values,
#'                    i.e. these values are converted to NA before conducting the
#'                    analysis.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#' @param output      logical: if \code{TRUE}, output is shown.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{write.result}}, \code{\link{as.na}}, \code{\link{na.as}},
#' \code{\link{na.auxiliary}}, \code{\link{na.coverage}}, \code{\link{na.descript}},
#' \code{\link{na.indicator}}, \code{\link{na.prop}}, \code{\link{na.test}}
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
#' entries: function call (\code{call}), type of analysis \code{type}, matrix or
#' data frame specified in \code{x} (\code{data}), specification of function arguments
#' (\code{args}), list with results (\code{result}), and a vector with the number
#' of missing data pattern for each case (\code{pattern}).
#'
#' @export
#'
#' @examples
#' dat <- data.frame(x = c(1, NA, NA, 6, 3),
#'                   y = c(7, NA, 8, 9, NA),
#'                   z = c(2, NA, 3, NA, 5))
#'
#' # Compute a summary of missing data patterns
#' dat.pattern <- na.pattern(dat)
#'
#' # Vector of missing data pattern for each case
#' dat.pattern$pattern
#
#' # Data frame without cases with missing data pattern 2 and 5
#' dat[!dat.pattern$pattern %in% c(2, 5), ]
#'
#' \dontrun{
#' # Write Results into a Excel file
#' result <- na.pattern(dat, output = FALSE)
#' write.result(result, "NA_Pattern.xlsx")
#' }
na.pattern <- function(x, order = FALSE, digits = 2, as.na = NULL, check = TRUE,
                       output = TRUE) {

  ####################################################################################
  # Input Check

  #......
  # Check if input 'x' is missing
  if (isTRUE(missing(x))) {

    stop("Please specify a matrix or data frame for the argument 'x'.",
         call. = FALSE)

  }

  #......
  # Check if input 'x' is NULL
  if (isTRUE(is.null(x))) {

    stop("Input specified for the argument 'x' is NULL.", call. = FALSE)

  }

  #......
  # Check input 'check'
  if (isTRUE(!is.logical(check))) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }

  #.........................................

  if (isTRUE(check)) {

    #......
    # Matrix or data frame for the argument 'x'?
    if (isTRUE(!is.matrix(x) && !is.data.frame(x))) {

      stop("Please specify a matrix or data frame for the argument 'x'.", call. = FALSE)

    }

    #......
    # Check input 'order'
    if (isTRUE(!is.logical(order))) {

      stop("Please specify TRUE or FALSE for the argument 'order'.", call. = FALSE)

    }

    #......
    # Check input 'digits'
    if (isTRUE(digits %% 1L != 0L || digits < 0L)) {

      stop("Please specify a positive integer value for the argument 'digits'.", call. = FALSE)

    }

    #......
    # Check input 'output'
    if (isTRUE(!is.logical(output))) {

      stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE)

    }

  }

  ####################################################################################
  # Data and Arguments

  #----------------------------------------
  # Convert user-missing values into NA

  if (isTRUE(!is.null(as.na))) {

    x <- misty::as.na(x, na = as.na, check = check)

  }

  #----------------------------------------
  # As data.frame

  x <- as.data.frame(x, stringsAsFactors = FALSE)

  ####################################################################################
  # Main Function

  # Missing data TRUE/FALSE matrix
  x.na <- is.na(x)

  # Number of missing values for each variable
  x.na.var <- colSums(x.na)

  if (isTRUE(order)) {

    x.na <- x.na[, order(x.na.var)]

  }

  # Missing data pattern
  patt <- apply(x.na, 1, function(y) paste(as.numeric(y), collapse = ""))

  # Order NA matrix
  x.na.order <- x.na[order(patt), ]

  # Remove duplicated rows
  x.na.order.dupl <- x.na.order[!duplicated(x.na.order), ]

  if (isTRUE(!is.null(dim(x.na.order.dupl)))) {

    restab <- rbind(data.frame(pattern = seq_len(nrow(x.na.order.dupl)),
                               n = as.vector(table(patt)),
                               perc = as.vector(table(patt) / nrow(x.na) * 100L),
                               abs(x.na.order.dupl - 1),
                               nNA = rowSums(x.na.order.dupl),
                               pNA = rowSums(x.na.order.dupl) / ncol(x.na) * 100L,
                               row.names = NULL, stringsAsFactors = FALSE),
                    c(NA, sum(as.vector(table(patt))), sum(as.vector(table(patt) / nrow(x.na) * 100L)), colSums(x.na), NA, NA))

    # Number of missing data pattern
    pattern <- unname(vapply(apply(x.na[, colnames(x.na.order.dupl)], 1,  paste, collapse = " "), function(y) match(y, apply(x.na.order.dupl, 1, paste, collapse = " ")), FUN.VALUE = 1L))

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

  ####################################################################################
  # Return object

  object <- list(call = match.call(),
                 type = "na.pattern",
                 data = x,
                 args = list(order = order, digits = digits, as.na = as.na, check = check, output = output),
                 result = restab,
                 pattern = pattern)

  class(object) <- "misty.object"

  ####################################################################################
  # Output

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
