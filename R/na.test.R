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
#' package which is needed for using the \code{LittleMCAR} function. Note that the
#' \code{mcar_test} function in the \pkg{naniar} package is also based on the
#' \code{prelim.norm} function which results are not trustworthy whenever the warning
#' message \code{In norm::prelim.norm(data) : NAs introduced by coercion to integer range}
#' is printed on the console.
#'
#' @param ...      a matrix or data frame with incomplete data, where missing
#'                 values are coded as \code{NA}. Alternatively, an expression
#'                 indicating the variable names in \code{data} e.g.,
#'                 \code{na.test(x1, x2, x3, data = dat)}. Note that the operators
#'                 \code{.}, \code{+}, \code{-}, \code{~}, \code{:}, \code{::},
#'                 and \code{!} can also be used to select variables, see 'Details'
#'                 in the \code{\link{df.subset}} function.
#' @param data     a data frame when specifying one or more variables in the
#'                 argument \code{...}. Note that the argument is \code{NULL}
#'                 when specifying a atrix or data frame for the argument \code{...}.
#' @param digits   an integer value indicating the number of decimal places to
#'                 be used for displaying results.
#' @param p.digits an integer value indicating the number of decimal places to be
#'                 used for displaying the \emph{p}-value.
#' @param as.na    a numeric vector indicating user-defined missing values, i.e.
#'                 these values are converted to NA before conducting the analysis.
#' @param write    a character string naming a text file with file extension
#'                 \code{".txt"} (e.g., \code{"Output.txt"}) for writing the
#'                 output into a text file.
#' @param append   logical: if \code{TRUE} (default), output will be appended
#'                 to an existing text file with extension \code{.txt} specified
#'                 in \code{write}, if \code{FALSE} existing text file will be
#'                 overwritten.
#' @param check    logical: if \code{TRUE} (default), argument specification is checked.
#' @param output   logical: if \code{TRUE} (default), output is shown.
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
#' entries:
#' \tabular{ll}{
#' \code{call} \tab function call \cr
#' \code{type} \tab type of analysis \cr
#' \code{data} \tab matrix or data frame specified in \code{x} \cr
#' \code{args} \tab specification of function arguments \cr
#' \code{result} \tab result table \cr
#' }
#'
#' @export
#'
#' @examples
#' # Example 1a: Conduct Little's MCAR test
#' na.test(airquality)
#'
#' # Example b: Alternative specification using the 'data' argument,
#' na.test(., data = airquality)
#'
#' \dontrun{
#' # Example 2: Write results into a text file
#' na.test(airquality, write = "NA_Test.txt")
#' }
na.test <- function(..., data = NULL, digits = 2, p.digits = 3, as.na = NULL,
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

  # Matrix or data frame for the argument 'x'?
  if (isTRUE(!is.matrix(x) && !is.data.frame(x))) { stop("Please specifiy a matrix or data frame for the argument 'x'.", call. = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) { x <- .as.na(x, na = as.na) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## As data matrix ####

  # Coerce to a data matrix
  x.matrix <- data.matrix(x)

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # No missing values
    if (isTRUE(all(!is.na(x.matrix)))) { stop("There are no missing values (NA) in the matrix or data frame specified in 'x'.", call. = FALSE) }

    # Variables with completely missing
    all.na <- apply(x.matrix, 2L, function(y) all(is.na(y)))
    if (isTRUE(any(all.na))) { stop(paste("Following variables are completely missing:", paste(names(all.na)[which(all.na)], collapse = ", ")), call. = FALSE) }

    # Variables without variance
    var.0 <- apply(x.matrix, 2L, function(y) var(y, na.rm = TRUE) == 0L)
    if (isTRUE(any(var.0))) { stop(paste("Following variables have no variance:", paste(names(var.0)[which(var.0)], collapse = ", ")), call. = FALSE) }

    # R package 'norm'
    if (isTRUE(!nzchar(system.file(package = "norm")))) { stop("Package \"norm\" is needed for this function, please install the package.", call. = FALSE) }

    # Check input 'digits'
    if (isTRUE(digits %% 1L != 0L || digits < 0L)) { stop("Please specify a positive integer number for the argument 'digits'.", call. = FALSE) }

    # Check input 'p.digits'
    if (isTRUE(p.digits %% 1L != 0L || p.digits < 0L)) { stop("Please specify a positive integer number for the argument 'p.digits'.", call. = FALSE) }

    # Check input 'write'
    if (isTRUE(!is.null(write) && substr(write, nchar(write) - 3L, nchar(write)) != ".txt")) { stop("Please specify a character string with file extenstion '.txt' for the argument 'write'.") }

    # Check input 'append'
    if (isTRUE(!is.logical(append))) { stop("Please specify TRUE or FALSE for the argument 'append'.", call. = FALSE) }

    # Check input 'output'
    if (isTRUE(!is.logical(output))) { stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  # Global variable
  pNA <- NULL

  # Variable names
  var.names <- colnames(x.matrix)

  # Number of variables
  var.n <- ncol(x.matrix)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Missing data pattern ####

  # Missing data pattern
  pattern <- misty::na.pattern(x, output = FALSE)

  # Pattern with complete missingness
  na.complete <- subset(pattern$result, pNA == 100L, select = pattern, drop = TRUE)

  # Data frame with missing data pattern
  if (length(na.complete) == 1L) {

    x.pattern <- subset(data.frame(x.matrix, pattern = pattern$pattern), pattern != na.complete)

  } else {

    x.pattern <- data.frame(x.matrix, pattern = pattern$pattern)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Maximum-likelihood estimation ####

   s <- tryCatch(norm::prelim.norm(x.matrix), warning = function(z) {

    warning("Function run into numerical problems, i.e., results are not trustworthy.", call. = FALSE)

     suppressWarnings(return(norm::prelim.norm(x.matrix)))

    })

  fit <- norm::getparam.norm(s = s, theta = norm::em.norm(s, showits = FALSE))

  # Grand mean
  grand.mean <- fit$mu

  # Grand covariance
  grand.cov <- fit$sigma

  # Column and row names
  colnames(grand.cov) <- rownames(grand.cov) <- var.names

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data for each missing data pattern ####

  dat.pattern <- split(x.pattern[, var.names], f = x.pattern$pattern)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Degrees of freedom ####

  df <- sum(sapply(dat.pattern, function(y) sapply(y, function(z) all(!is.na(z))))) - var.n

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Little's chi-square ####

  # Likelihood ratio test statistic d2
  d2 <- 0L

  for (i in unique(x.pattern$pattern)) {

    mean <- na.omit(colMeans(dat.pattern[[i]]) - grand.mean)

    keep <- names(unlist(lapply(dat.pattern[[i]], function(y) which(all(!is.na(y))))))

    d2 <- as.numeric(d2 + (sum(x.pattern$pattern == i) * (t(mean) %*% solve(grand.cov[which(rownames(grand.cov) %in% keep), which(colnames(grand.cov) %in% keep)]) %*% mean)))

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Result table ####

  restab <- data.frame(no.cases = nrow(x),
                       no.incomplete = misty::na.descript(x, data = NULL, output = FALSE)$result$L1$no.incomplete.l1,
                       no.pattern = max(x.pattern$pattern),
                       statistic = d2,
                       df = df,
                       pval = pchisq(d2, df, lower.tail = FALSE), row.names = NULL)

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "na.test",
                 data = x,
                 args = list(digits = digits, p.digits = p.digits, as.na = NULL,
                             write = write, append = append, check = TRUE, output = TRUE),
                 result = restab)

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Write Results --------------------------------------------------------------

  if (isTRUE(!is.null(write))) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Text file ####

    # Send R output to textfile
    sink(file = write, append = ifelse(isTRUE(file.exists(write)), append, FALSE), type = "output", split = FALSE)

    if (append && isTRUE(file.exists(write))) { write("", file = write, append = TRUE) }

    # Print object
    print(object, check = FALSE)

    # Close file connection
    sink()

  }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
