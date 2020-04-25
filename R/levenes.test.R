#' Levene's Test for Homogeneity of Variance
#'
#' This function computes Levene's test for homogeneity of variance across two or more independent groups.
#'
#' Levene's test is equivalent to a one-way analysis of variance (ANOVA) with the absolute deviations
#' of observations from the mean of each group as dependent variable (\code{center = "mean"}). Brown
#' and Forsythe (1974) modified the Levene's test by using the absolute deviations of observations
#' from the median (\code{center = "median"}). By default, the Levene's test uses the absolute
#' deviations of observations from the median.
#'
#' @param formula    a formula of the form \code{y ~ group} where \code{y} is a numeric variable giving the
#'                   data values and \code{group} a numeric variable, character variable or factor with
#'                   two or more than two values or factor levels giving the corresponding groups.
#' @param data       a matrix or data frame containing the variables in the formula \code{formula}.
#' @param method     a character string specifying the method to compute the center of each group, i.e.
#'                   \code{method = "median"} (default) to compute the Levene's test basd on the median
#'                   (aka Brown-Forsythe test) or \code{method = "mean"} to compute the Levene's test
#'                   based on the arithmetic mean.
#' @param as.na      a numeric vector indicating user-defined missing values,
#'                   i.e. these values are converted to \code{NA} before conducting the analysis.
#' @param conf.level a numeric value between 0 and 1 indicating the confidence level of the interval.
#' @param digits     an integer value indicating the number of decimal places to be used for displaying results.
#' @param p.digits   an integer value indicating the number of decimal places to be used for displaying the
#'                   \emph{p}-value.
#' @param check      logical: if \code{TRUE}, argument specification is checked.
#' @param output     logical: if \code{TRUE}, output is shown.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{t.test}}, \code{\link{aov}}
#'
#' @references
#' Brown, M. B., & Forsythe, A. B. (1974). Robust tests for the equality of variances. \emph{Journal of the American
#' Statistical Association, 69}, 364-367.
#'
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology - Using R and SPSS}.
#' John Wiley & Sons.
#'
#' @return
#' Returns an object of class \code{test}, which is a list with following entries:
#' function call (\code{call}), formula (\code{formula}), data frame with the outcome and grouping variable,
#' (\code{data}), specification of function arguments (\code{args}), and a list with descriptive statistics
#' including confidence interval and an object of class \code{"anova"} (\code{result}).
#'
#' @export
#'
#' @examples
#' dat <- data.frame(y = c(2, 3, 4, 5, 5, 7, 8, 4, 5, 2, 4, 3),
#'                   group = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3))
#'
#' # Levene's test based on the median with 95% confidence interval
#' levenes.test(y ~ group, data = dat)
#'
#' # Levene's test based on the arithmetic mean  with 95% confidence interval
#' levenes.test(y ~ group, data = dat, method = "mean")
#'
#' # Levene's test based on the median with 99% confidence interval
#' levenes.test(y ~ group, data = dat, conf.level = 0.99)

levenes.test <- function(formula, data, method = c("median", "mean"),
                         conf.level = 0.95, digits = 2, p.digits = 3, as.na = NULL,
                         check = TRUE, output = TRUE) {

  ####################################################################################
  # Input Check

  #......
  # Check if input 'formula' is missing
  if (missing(formula)) {

    stop("Please specify a formula using the argument 'formula'", call. = FALSE)

  }

  #......
  # Check if input 'data' is missing
  if (missing(data)) {

    stop("Please specify a matrix or data frame for the argument 'data'.", call. = FALSE)

  }

  #......
  # Check if input 'data' is NULL
  if (is.null(data)) {

    stop("Input specified for the argument 'x' is NULL.", call. = FALSE)

  }

  #-----------------------------------------------------------------------------------
  # Formula

  #.........................................
  # Variables

  var.formula <- all.vars(as.formula(formula))

  # Grouping variable
  group.var <- attr(terms(formula[-2L]), "term.labels")

  # Outcome
  y.var <- var.formula[-grep(group.var, var.formula)]

  #.........................................
  # Check

  # Check if variables are in the data
  var.data <- !var.formula %in% colnames(data)
  if (any(var.data)) {

    stop(paste0("Variables specified in the the formula were not found in 'data': ",
                paste(var.formula[which(var.data)], collapse = ", ")), call. = FALSE)

  }

  # Check if input 'formula' has only one grouping variable
  if (length(y.var) != 1L) { stop("Please specify a formula with only one outcome variable.", call. = FALSE) }

  # Check if input 'formula' has only one grouping variable
  if (length(group.var) != 1L) { stop("Please specify a formula with only one grouping variable.", call. = FALSE) }

  #----------------------------------------
  # Convert user-missing values into NA

  if (!is.null(as.na)) {

    # Replace user-specified values with missing values
    data[, y.var] <- misty::as.na(data[, y.var], as.na = as.na, check = check)

    # Variable with missing values only
    if (all(is.na(data[, y.var]))) {

      stop(paste0("After converting user-missing values into NA, ", y.var, "is completely missing."),
           call. = FALSE)

    }

  }

  #.........................................
  # Listwise deletion

  data <- na.omit(data[, var.formula])

  #----------------------------------------

  if (isTRUE(check)) {

    #......
    # Variance zero
    y.var0 <- tapply(data[, y.var], data[, group.var], var, na.rm = TRUE) == 0L

    if (any(y.var0)) {

      stop(paste0("There are groups with 0 variance: Group ", paste(which(y.var0), collapse = ", ")),
           call. = FALSE)

    }

    #......
    # Check input 'method'
    if (!all(method %in% c("median", "mean"))) {

      stop("Character string in the argument 'method' does not match with \"median\", or \"mean\".",
           call. = FALSE)

    }

    #......
    # Check input 'conf.level'
    if (conf.level >= 1L|| conf.level <= 0L) {

      stop("Please specifiy a numeric value between 0 and 1 for the argument 'conf.level'.",
           call. = FALSE)

    }

    #......
    # Check input 'digits'
    if (digits %% 1 != 0L || digits < 0L) {

      stop("Specify a positive integer number for the argument 'digits'.", call. = FALSE)

    }

    #......
    # Check input 'p.digits'
    if (p.digits %% 1 != 0L || p.digits < 0L) {

      stop("Specify a positive integer number for the argument 'p.digits'.", call. = FALSE)

    }

    #......
    # Check input 'output'
    if (!isTRUE(isTRUE(output) || !isTRUE(output))) {

        stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE)

    }

  }

  ####################################################################################
  # Arguments

  #----------------------------------------
  # Method

  if (all(c("median", "mean") %in% method)) { method <- "median" }

  ####################################################################################
  # Main Function

  # Outcome
  y <- data[, y.var]

  # Grouping variable
  group <- data[, group.var]

  #----------------------------------------
  # Confidence interval

  result.ci <- ci.var(y, group = group, conf.level = conf.level, output = FALSE)$result

  #----------------------------------------
  # Analysis of Variance

  #......
  # Brown-Forsythe test
  if (method == "median") {

    # Median by grouping variable
    y.center <- tapply(y, group, median)

  #......
  # Levene's test
  } else if (method == "mean") {

    # Mean by grouping variable
    y.center <- tapply(y, group, mean)

  }

  # Deviation from the median or mean
  y.dev <- abs(y - y.center[ group])

  # Analysis of Variance
  result.aov <- summary(aov(y.dev ~ as.factor(group)))[[1]]

  row.names(result.aov) <- c("Group", "Residuals")

  ####################################################################################
  # Return object

  #----------------------------------------
  # Return object

  object <- list(call = match.call(),
                 type = "levene",
                 formula = formula,
                 data = data.frame(y, group, stringsAsFactors = FALSE),
                 args = list(method = method, digits = digits, p.digits = p.digits,
                             as.na = as.na, check = check, output = output),
                 result = list(descript = result.ci, aov = result.aov))

  class(object) <- "test"

  ####################################################################################
  # Output

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
