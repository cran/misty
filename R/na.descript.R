#' Descriptive Statistics for Missing Data
#'
#' This function computes descriptive statistics for missing data, e.g. number (%) of incomplete cases, number (%)
#' of missing values, and summary statistics for the number (%) of missing values across all variables.
#'
#' @param x           a matrix or data frame.
#' @param table       logical: if \code{TRUE}, a frequency table with number of observed values (\code{"nObs"}),
#'                    percent of observed values (\code{"pObs"}), number of missing values (\code{"nNA"}), and
#'                    percent of missing values (\code{"pNA"}) is printed for each variable on the console.
#' @param digits      an integer value indicating the number of decimal places to be used for displaying percentages.
#' @param as.na       a numeric vector indicating user-defined missing values,
#'                    i.e. these values are converted to \code{NA} before conducting the analysis.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#' @param output      logical: if \code{TRUE}, output is shown on the console.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{as.na}}, \code{\link{na.as}}, \code{\link{na.auxiliary}}, \code{\link{na.coverage}},
#' \code{\link{na.indicator}}, \code{\link{na.pattern}}, \code{\link{na.prop}}.
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
#' Returns an object of class \code{na.descript}, which is a list with following entries: function call (\code{call}),
#' matrix or data frame specified in \code{x} (\code{data}), specification of function arguments (\code{args}), and
#' list with results (\code{result}).
#'
#' @export
#'
#' @examples
#'dat <- data.frame(x1 = c(1, NA, 2, 5, 3, NA, 5, 2),
#'                  x2 = c(4, 2, 5, 1, 5, 3, 4, 5),
#'                  x3 = c(NA, 3, 2, 4, 5, 6, NA, 2),
#'                  x4 = c(5, 6, 3, NA, NA, 4, 6, NA))
#'
#' # Descriptive statistics for missing data
#' na.descript(dat)
#'
#' # Descriptive statistics for missing data, print results with 3 digits
#' na.descript(dat, digits = 3)
#'
#' # Descriptive statistics for missing data, convert value 2 to NA
#' na.descript(dat, as.na = 2)
#'
#' # Descriptive statistics for missing data with frequency table
#' na.descript(dat, table = TRUE)
na.descript <- function(x, table = FALSE, digits = 2, as.na = NULL, check = TRUE, output = TRUE) {

  ####################################################################################
  # Data

  #----------------------------------------
  # Check input 'x'
  if (missing(x)) {

    stop("Please specify a matrix or data frame for the argument 'x'.",
         call. = FALSE)

  }

  #----------------------------------------
  # Matrix or data frame for the argument 'x'?
  if (!is.matrix(x) && !is.data.frame(x)) {

    stop("Please specify a matrix or data frame for the argument 'x'.",
         call. = FALSE)

  }

  #----------------------------------------
  # Data frame

  df <- as.data.frame(x)

  #----------------------------------------
  # Convert user-missing values into NA

  if (!is.null(as.na)) {

    df <- misty::as.na(df, na = as.na, check = check)

  }

  ####################################################################################
  # Input Check

  # Check input 'check'
  if (isFALSE(isTRUE(check) | isFALSE(check))) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }

  #-----------------------------------------

  if (isTRUE(check)) {

    #......
    # Check input 'table'
    if (isFALSE(isTRUE(table) | isFALSE(table))) {

      stop("Please specify TRUE or FALSE for the argument 'table'", call. = FALSE)

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
  # Main Function

  # Number of cases
  no.cases <- nrow(df)

  # Number of complete cases
  no.complete <- sum(apply(df, 1, function(y) all(!is.na(y))))
  perc.complete <- no.complete / no.cases * 100

  # Number and percentage of imcomplete cases
  no.incomplete <- sum(apply(df, 1, function(y) any(is.na(y))))
  perc.incomplete <- no.incomplete / no.cases * 100

  ###

  # Number of values
  no.values <- length(unlist(df))

  # Number of observed values
  no.observed.values <- sum(!is.na(unlist(df)))
  perc.observed.values <- no.observed.values / no.values *100

  # Number and percentage of missing values
  no.missing.values <- sum(is.na(unlist(df)))
  perc.missing.values <- no.missing.values / no.values * 100

  ###

  # Number of variables
  no.var <- ncol(df)

  # Number and percentage of observed values for each variable
  no.observed.var <- sapply(df, function(y) sum(!is.na(y)))
  perc.observed.var <- no.observed.var / no.cases * 100

  # Number and percentage of missing values for each variable
  no.missing.var <- sapply(df, function(y) sum(is.na(y)))
  perc.missing.var <- no.missing.var / no.cases * 100

  no.missing.mean <- mean(no.missing.var)
  perc.missing.mean <- no.missing.mean / no.cases * 100

  no.missing.sd <- sd(no.missing.var)
  perc.missing.sd <- no.missing.sd / no.cases * 100

  no.missing.min <- min(no.missing.var)
  perc.missing.min <- no.missing.min / no.cases * 100

  no.missing.p25 <- quantile(no.missing.var, probs = 0.25)
  perc.missing.p25 <- no.missing.p25 / no.cases * 100

  no.missing.p75 <- quantile(no.missing.var, probs = 0.75)
  perc.missing.p75 <- no.missing.p75 / no.cases * 100

  no.missing.max <- max(no.missing.var)
  perc.missing.max <- no.missing.max / no.cases * 100

  ###

  # Frequency table
  table.missing <- data.frame(Var = colnames(df),
                              matrix(c(no.observed.var, perc.observed.var, no.missing.var, perc.missing.var), ncol = 4,
                                     dimnames = list(NULL, c("nObs", "pObs", "nNA", "pNA"))),
                              stringsAsFactors = FALSE)

  ####################################################################################
  # Return object

  # Return object
  object <- list(call = match.call(),
                 data = x,
                 args = list(digits = digits, table = table, as.na = as.na, check = check, output = output),
                 result = list(no.cases = no.cases, no.complete = no.complete, perc.complete = perc.complete,
                               no.incomplete = no.incomplete, perc.incomplete = perc.incomplete,
                               no.values = no.values, no.observed.values = no.observed.values,
                               perc.observed.values = perc.observed.values, no.missing.values = no.missing.values,
                               perc.missing.values = perc.missing.values, no.var = no.var,
                               no.missing.mean = no.missing.mean, perc.missing.mean = perc.missing.mean,
                               no.missing.sd = no.missing.sd, perc.missing.sd = perc.missing.sd,
                               no.missing.min = no.missing.min, perc.missing.min = perc.missing.min,
                               no.missing.p25 = no.missing.p25, perc.missing.p25 = perc.missing.p25,
                               no.missing.p75 = no.missing.p75, perc.missing.p75 = perc.missing.p75,
                               no.missing.max = no.missing.max, perc.missing.max = perc.missing.max,
                               table.miss = table.missing))

  class(object) <- "na.descript"

  ####################################################################################
  # Output

  if (isTRUE(output)) { print(object) }

  return(invisible(object))

}
