#' Little's Missing Completely at Random (MCAR) Test
#'
#' This function performs Little's Missing Completely at Random (MCAR) test
#'
#' Little (1988) proposed a multivariate test of Missing Completely at Random (MCAR)
#' that tests for mean differences on every variable in the data set across subgroups
#' that share the same missing data pattern by comparing the observed variable means
#' for each pattern of missing data with the expected population means estimated using
#' the expectation-maximization (EM) algorithm (i.e., EM maximum likelihood estimates).
#' The test statistic is the sum of the squared standardized differences between the
#' subsample means and the expected population means weighted by the estimated
#' variance-covariance matrix and the number of observations within each subgroup
#' (Enders, 2010). Under the null hypothesis that data are MCAR, the test statistic
#' follows asymptotically a chi-square distribution with \eqn{\sum k_j - k} degrees
#' of freedom, where \eqn{k_j} is the number of complete variables for missing data
#' pattern \eqn{j}, and \eqn{k} is the total number of variables. A statistically
#' significant result provides evidence against MCAR.
#'
#' Note that Little's MCAR test has a number of problems (see Enders, 2010).
#' \strong{First}, the test does not identify the specific variables that violates
#' MCAR, i.e., the test does not identify potential correlates of missingness (i.e.,
#' auxiliary variables). \strong{Second}, the test is based on multivariate normality,
#' i.e., under departure from the normality assumption the test might be unreliable
#' unless the sample size is large and is not suitable for categorical variables.
#' \strong{Third}, the test investigates mean  differences assuming that the missing
#' data pattern share a common covariance matrix, i.e., the test cannot detect
#' covariance-based deviations from MCAR stemming from a Missing at Random (MAR)
#' or Missing Not at Random (MNAR) mechanism because MAR and MNAR mechanisms can
#' also produce missing data subgroups with equal means. \strong{Fourth}, simulation
#' studies suggest that Little's MCAR test suffers from low statistical power,
#' particularly when the number of variables that violate MCAR is small, the
#' relationship between the data and missingness is weak, or the data are MNAR
#' (Thoemmes & Enders, 2007). \strong{Fifth}, the test can only reject, but cannot
#' prove the MCAR assumption, i.e., a statistically not significant result and failing
#' to reject the null hypothesis of the MCAR test does not prove the null hypothesis
#' that the data is MCAR. \strong{Finally}, under the null hypothesis the data are
#' actually MCAR or MNAR, while a statistically significant result indicates that
#' missing data are MAR or MNAR, i.e., MNAR cannot be ruled out regardless of the
#' result of the test.
#'
#' This function is based on the \code{prelim.norm} function in the \pkg{norm}
#' package which can handle about 30 variables. With more than 30 variables
#' specified in the argument \code{x}, the \code{prelim.norm} function might run
#' into numerical problems leading to results that are not trustworthy. In this
#' case it is recommended to reduce the number of variables specified in the argument
#' \code{x}. If the number of variables cannot be reduced, it is recommended to
#' use the \code{LittleMCAR} function in the \pkg{BaylorEdPsych} package which can
#' deal with up to 50 variables. However, this package was removed from the CRAN
#' repository and needs to be obtained from the archive along with the \pkg{mvnmle}
#' which is needed for using the \code{LittleMCAR} function. Note that the
#' \code{mcar_test} function in the \pkg{naniar} package is also based on the
#' \code{prelim.norm} function which results are not trustworthy whenever the warning
#' message \code{In norm::prelim.norm(data) : NAs introduced by coercion to integer range}
#' is printed on the console.
#'
#' @param x           a matrix or data frame with incomplete data, where missing
#'                    values are coded as \code{NA}.
#' @param digits      an integer value indicating the number of decimal places to
#'                    be used for displaying results.
#' @param p.digits    an integer value indicating the number of decimal places to be
#'                    used for displaying the \emph{p}-value.
#' @param as.na       a numeric vector indicating user-defined missing values, i.e.
#'                    these values are converted to NA before conducting the analysis.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#' @param output      logical: if \code{TRUE}, output is shown.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{as.na}}, \code{\link{na.as}}, \code{\link{na.auxiliary}},
#' \code{\link{na.coverage}}, \code{\link{na.descript}}, \code{\link{na.indicator}},
#' \code{\link{na.pattern}}, \code{\link{na.prop}}.
#'
#' @references
#' Enders, C. K. (2010). \emph{Applied missing data analysis}. Guilford Press.
#'
#' Thoemmes, F., & Enders, C. K. (2007, April). \emph{A structural equation model for
#' testing whether data are missing completely at random}. Paper presented at the
#' annual meeting of the American Educational Research Association, Chicago, IL.
#'
#' Little, R. J. A. (1988). A test of Missing Completely at Random for multivariate
#' data with missing values. \emph{Journal of the American Statistical Association, 83},
#' 1198-1202. https://doi.org/10.2307/2290157
#'
#' @note Code is adapted from the R function by Eric Stemmler:
#' tinyurl.com/r-function-for-MCAR-test
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries: function call (\code{call}), type of analysis \code{type}, matrix or
#' data frame specified in \code{x} (\code{data}), specification of function arguments
#' (\code{args}), list with results (\code{result}).
#'
#' @export
#'
#' @examples
#' na.test(airquality)
na.test <- function(x, digits = 2, p.digits = 3, as.na = NULL, check = TRUE,
                    output = TRUE) {

  ####################################################################################
  # R package

  if (isTRUE(!nzchar(system.file(package = "norm")))) {

    stop("Package \"norm\" is needed for this function, please install the package.", call. = FALSE)

  }

  ####################################################################################
  # Data

  #......
  # Check if input 'x' is missing
  if (isTRUE(missing(x))) {

    stop("Please specify a matrix or data frame for the argument 'x'.", call. = FALSE)

  }

  #......
  # Check if input 'x' is NULL
  if (isTRUE(is.null(x))) {

    stop("Input specified for the argument 'x' is NULL.", call. = FALSE)

  }

  #......
  # Matrix or data frame for the argument 'x'?
  if (isTRUE(!is.matrix(x) && !is.data.frame(x))) {

    stop("Please specifiy a matrix or data frame for the argument 'x'.", call. = FALSE)

  }

  #----------------------------------------
  # Convert user-missing values into NA

  if (isTRUE(!is.null(as.na))) {

    x <- misty::as.na(x, na = as.na, check = check)

  }

  #----------------------------------------
  # As data matrix

  # Coerce to a data matrix
  x.matrix <- data.matrix(x)

  ####################################################################################
  # Input Check

  #......
  # Check input 'check'
  if (isTRUE(!is.logical(check))) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }

  #.........................................

  if (isTRUE(check)) {

    #......
    # No missing values
    if (isTRUE(all(!is.na(x.matrix)))) {

      stop("There are no missing values (NA) in the matrix or data frame specified in 'x'.",
           call. = FALSE)

    }

    #......
    # Variables with completely missing
    all.na <- apply(x.matrix, 2, function(y) all(is.na(y)))
    if (isTRUE(any(all.na))) {

      stop(paste("Following variables are completely missing:", paste(names(all.na)[which(all.na)], collapse = ", ")),
           call. = FALSE)

    }

    #......
    # Variables without variance
    var.0 <- apply(x.matrix, 2, function(y) var(y, na.rm = TRUE) == 0)
    if (isTRUE(any(var.0))) {

      stop(paste("Following variables have no variance:", paste(names(var.0)[which(var.0)], collapse = ", ")),
           call. = FALSE)

    }

    #......
    # Check input 'digits'
    if (isTRUE(digits %% 1L != 0L || digits < 0L)) {

      stop("Please specify a positive integer number for the argument 'digits'.", call. = FALSE)

    }

    #......
    # Check input 'p.digits'
    if (isTRUE(p.digits %% 1L != 0L || p.digits < 0L)) {

      stop("Please specify a positive integer number for the argument 'p.digits'.", call. = FALSE)

    }

  }

  ####################################################################################
  # Main Function

  # Global variable
  pNA <- NULL

  # Variable names
  var.names <- colnames(x.matrix)

  # Number of variables
  var.n <- ncol(x.matrix)

  #-----------------------------------------
  # Missing data pattern

  # Missing data pattern
  pattern <- misty::na.pattern(x, output = FALSE)

  # Pattern with complete missingness
  na.complete <- subset(pattern$result, pNA == 100, select = pattern, drop = TRUE)

  # Data frame with missing data pattern
  if (length(na.complete) == 1) {

    x.pattern <- subset(data.frame(x.matrix, pattern = pattern$pattern), pattern != na.complete)

  } else {

    x.pattern <- data.frame(x.matrix, pattern = pattern$pattern)

  }

  #-----------------------------------------
  # Maximum-likelihood estimation

   s <- tryCatch(norm::prelim.norm(x.matrix), warning = function(z) {

    warning("Function run into numerical problems, i.e., results are not trustworthy.",
            call. = FALSE)

     suppressWarnings(return(norm::prelim.norm(x.matrix)))

    })

  fit <- norm::getparam.norm(s = s, theta = norm::em.norm(s, showits = FALSE))

  # Grand mean
  grand.mean <- fit$mu

  # Grand covariance
  grand.cov <- fit$sigma

  # Column and row names
  colnames(grand.cov) <- rownames(grand.cov) <- var.names

  #-----------------------------------------
  # Data for each missing data pattern

  dat.pattern <- split(x.pattern[, var.names], f = x.pattern$pattern)

  #-----------------------------------------
  # Degrees of freedom

  df <- sum(sapply(dat.pattern, function(y) sapply(y, function(z) all(!is.na(z))))) - var.n

  #-----------------------------------------
  # Little's chi-square

  # Likelihood ratio test statistic d2
  d2 <- 0

  for (i in unique(x.pattern$pattern)) {

    mean <- na.omit(colMeans(dat.pattern[[i]]) - grand.mean)

    keep <- names(unlist(lapply(dat.pattern[[i]], function(y) which(all(!is.na(y))))))

    d2 <- as.numeric(d2 + (sum(x.pattern$pattern == i) * (t(mean) %*% solve(grand.cov[which(rownames(grand.cov) %in% keep), which(colnames(grand.cov) %in% keep)]) %*% mean)))

  }

  #-----------------------------------------
  # Descriptive statistics

  x.descript <- misty::na.descript(x = x, output = FALSE)$result

  #-----------------------------------------
  # Result

  restab <- data.frame(no.cases = nrow(x),
                       no.incomplete = x.descript$no.incomplete,
                       no.pattern = max(x.pattern$pattern),
                       statistic = d2,
                       df = df,
                       pval = pchisq(d2, df, lower.tail = FALSE), row.names = NULL)

  ####################################################################################
  # Return object

  object <- list(call = match.call(),
                 type = "na.test",
                 data = x,
                 args = list(digits = 2, p.digits = 3, as.na = NULL, check = TRUE,
                             output = TRUE),
                 result = restab)

  class(object) <- "misty.object"

  ####################################################################################
  # Output

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
