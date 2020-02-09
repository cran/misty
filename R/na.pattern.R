#' Missing Data Pattern
#'
#' This function computes a summary of missing data patterns, i.e., number (%) of cases with a specific missing data pattern.
#'
#' @param x           a matrix or data frame with incomplete data, where missing values are coded as \code{NA}.
#' @param order       logical: if \code{TRUE}, variables are ordered from left to right in increasing order
#'                    of missing values.
#' @param digits      an integer value indicating the number of decimal places to be used for displaying percentages.
#' @param as.na       a numeric vector indicating user-defined missing values,
#'                    i.e. these values are converted to NA before conducting the analysis.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#' @param output      logical: if \code{TRUE}, output is shown.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{as.na}}, \code{\link{na.as}}, \code{\link{na.auxiliary}}, \code{\link{na.coverage}}, \code{\link{na.descript}},
#' \code{\link{na.indicator}}, \code{\link{na.prop}}.
#'
#' @references
#' Enders, C. K. (2010). \emph{Applied missing data analysis}. Guilford Press.
#'
#' Graham, J. W. (2009). Missing data analysis: Making it work in the real world.
#' \emph{Annual Review of Psychology, 60}, 549-576. https://doi.org/10.1146/annurev.psych.58.110405.085530
#'
#' van Buuren, S. (2018). \emph{Flexible imputation of missing data} (2nd ed.). Chapman & Hall.
#'
#' @return
#' Returns an object of class \code{na.pattern}, which is a list with following entries: function call (\code{call}),
#' matrix or data frame specified in \code{x} (\code{data}), specification of function arguments (\code{args}),
#' list with results (\code{result}), and a vector with the number of missing data pattern for each case (\code{pattern}),
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
na.pattern <- function(x, order = FALSE, digits = 2, as.na = NULL, check = TRUE, output = TRUE) {

  ####################################################################################
  # Input Check

  #............................
  # Check input 'x'
  if (missing(x)) {

    stop("Please specify a matrix or data frame for the argument 'x'",
         call. = FALSE)

  }

  #......
  # Check input 'x'
  if (!is.matrix(x) && !is.data.frame(x)) {

    stop("Please specify a matrix or data frame for the argument 'x'", call. = FALSE)

  }

  #............................
  # Check input 'check'
  if (isFALSE(isTRUE(check) | isFALSE(check))) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }

  #.........................................

  if (isTRUE(check)) {

    #......
    # Check input 'order'
    if (isFALSE(isTRUE(order) | isFALSE(order))) {

      stop("Please specify TRUE or FALSE for the argument 'order'.", call. = FALSE)

    }

    #......
    # Check input 'digits'
    if (digits %% 1 != 0 | digits < 0) {

      stop("Please specify a positive integer value for the argument 'digits'", call. = FALSE)

    }

    #......
    # Check input 'output'
    if (isFALSE(isTRUE(output) | isFALSE(output))) {

      stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE)

    }

  }

  ####################################################################################
  # Data and Arguments

  #-----------------------------------------------------------------------------------
  # Convert user-missing values into NA

  if (!is.null(as.na)) {

    x <- misty::as.na(x, na = as.na, check = check)

  }

  #-----------------------------------------------------------------------------------
  # As data.frame

  df <- as.data.frame(x)

  ####################################################################################
  # Main Function

  # Missing data TRUE/FALSE matrix
  df.na <- is.na(df)

  # Number of missing values for each variable
  df.na.var <- colSums(df.na)

  if (isTRUE(order)) {

    df.na <- df.na[, order(df.na.var)]

  }

  # Missing data pattern
  patt <- apply(df.na, 1, function(y) paste(as.numeric(y), collapse = ""))

  # Order NA matrix
  df.na.order <- df.na[order(patt), ]

  # Remove duplicated rows
  df.na.order.dupl <- df.na.order[!duplicated(df.na.order), ]

  if (!is.null(dim(df.na.order.dupl))) {

    restab <- rbind(data.frame(pattern = 1:nrow(df.na.order.dupl),
                               n = as.vector(table(patt)),
                               Perc = as.vector(table(patt) / nrow(df.na) * 100),
                               abs(df.na.order.dupl - 1),
                               nNA = rowSums(df.na.order.dupl),
                               pNA = rowSums(df.na.order.dupl) / ncol(df.na) * 100,
                               row.names = NULL),
                    c("", sum(as.vector(table(patt))), sum(as.vector(table(patt) / nrow(df.na) * 100)), colSums(df.na), "", ""))

    # Number of missing data pattern
    pattern <- unname(sapply(apply(df.na[, colnames(df.na.order.dupl)], 1,  paste, collapse = " "), function(y) match(y, apply(df.na.order.dupl, 1, paste, collapse = " "))))

  } else {

    restab <- rbind(data.frame(pattern = 1,
                               n = as.vector(table(patt)),
                               Perc = as.vector(table(patt) / nrow(df.na) * 100),
                               matrix(abs(df.na.order.dupl - 1), ncol = length(df.na.order.dupl), dimnames = list(NULL, colnames(df.na.order.dupl))),
                               nNA = sum(df.na.order.dupl),
                               pNA = sum(df.na.order.dupl) / ncol(df.na) * 100,
                               row.names = NULL),
                    c("", sum(as.vector(table(patt))), sum(as.vector(table(patt) / nrow(df.na) * 100)), colSums(df.na), "", ""))

    pattern <- rep(1, times = nrow(df))

  }

  ####################################################################################
  # Return object

  object <- list(call = match.call(),
                 data = x,
                 args = list(order = order, digits = digits, as.na = as.na, check = check, output = output),
                 result = restab,
                 pattern = pattern)

  class(object) <- "na.pattern"

  ####################################################################################
  # Output

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
