#' Missing Completely at Random (MCAR) Test
#'
#' This function performs Little's Missing Completely at Random (MCAR) test and
#' Jamshidian and Jalalꞌs approach for testing the MCAR assumption. By default,
#' the function performs the Little's MCAR test.
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
#'                 when specifying a matrix or data frame for the argument \code{...}.
#' @param print    a character vector indicating which results to be printed on
#'                 the console, i.e. \code{"all"} for Little's MCAR test and
#'                 Jamshidian and Jalalꞌs approach, \code{"little"} (defaut) for
#'                 Little's MCAR test, and \code{"jamjal} for Jamshidian and
#'                 Jalalꞌs approach.
#' @param impdat   an object of class \code{mids} from the \pkg{mice} package to
#'                 provide a data set multiply imputed in the \pkg{mice} package.
#'                 The function will not impute the data data set specified in
#'                 the argument \code{data} when specifying this argument and will
#'                 use the imputed data sets provided in the argument \code{impdat}
#'                 for performing the Jamshidian and Jalalꞌs approach. Note that
#'                 the argument \code{data} still needs to be specified because
#'                 the variables used for the analysis are extracted from the
#'                 data frame specified in \code{data}.
#' @param delete   an integer value indicating missing data patterns consisting
#'                 of \code{delete} number of cases or less removed from the
#'                 Jamshidian and Jalalꞌs approach. The default setting is
#'                 \code{delete = 6}.
#' @param method   a character string indicating the imputation method, i.e.,
#'                 \code{"npar"} for using a non-parametric imputation method
#'                 by Sirvastava and Dolatabadi (2009) or \code{"normal"} for
#'                 imputing missing data assuming that the data come from a
#'                 multivariate normal distribution (see Jamshidian & Jalal, 2010).
#' @param m        an integer value indicating the number of multiple imputations.
#'                 The default setting is \code{m = 20}.
#' @param seed     an integer value that is used as argument by the \code{set.seed}
#'                 function for offsetting the random number generator before
#'                 performing Jamshidian and Jalalꞌs approach. The default
#'                 setting is \code{seed = 123}. Set the value to \code{NULL} to
#'                 specify a system selected seed.
#'                 is not used before performing Jamshidian and Jalalꞌs approach.
#' @param nrep     an integer value indicating the replications used to simulate
#'                 the Neyman distribution to determine the cut off value for the
#'                 Neyman test. Larger values increase the accuracy of the Neyman
#'                 test. The default setting is \code{nrep = 10000}.
#' @param n.min    an integer value indicating the minimum number of cases in a
#'                 group that triggers the use of asymptotic Chi-square distribution
#'                 in place of the empirical distribution in the Neyman test of
#'                 uniformity.
#' @param pool     a character string indicating the pooling method, i.e.,
#'                 \code{"m"} for computing the average test statistic and p-values,
#'                 \code{"med"} for computing the median test statistic and p-values,
#'                 \code{"min"} for computing the maximum test statistic and minimum p-values,
#'                 \code{"max"} for computing the minimum test statistic and maximum p-values,
#'                 and \code{"random"} for randomly choosing a test statistic and
#'                 corresponding p-value from repeated complete data analyses.
#'                 The default setting is \code{pool = "med"}, i.e., median test
#'                 statistic and p-values are computed as suggested by
#'                 Eekhout, Wiel and Heymans (2017).
#' @param alpha    a numeric value between 0 and 1 indicating the significance
#'                 level of the Hawkins test. The default setting is \code{alpha = 0.05},
#'                 i.e., the Anderson-Darling non-parametric test is provided
#'                 when the p-value of the Hawkins test is less than or equal
#'                 \code{0.05}.
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
#' @details
#' \describe{
#'   \item{\strong{Little's MCAR Test}}{
#'   Little (1988) proposed a multivariate test of Missing Completely at Random
#'   (MCAR) that tests for mean differences on every variable in the data set
#'   across subgroups that share the same missing data pattern by comparing the
#'   observed variable means for each pattern of missing data with the expected
#'   population means estimated using the expectation-maximization (EM) algorithm
#'   (i.e., EM maximum likelihood estimates). The test statistic is the sum of
#'   the squared standardized differences between the subsample means and the
#'   expected population means weighted by the estimated variance-covariance
#'   matrix and the number of observations within each subgroup (Enders, 2010).
#'   Under the null hypothesis that data are MCAR, the test statistic follows
#'   asymptotically a chi-square distribution with \eqn{\sum k_j - k} degrees of
#'   freedom, where \eqn{k_j} is the number of complete variables for missing data
#'   pattern \eqn{j}, and \eqn{k} is the total number of variables. A statistically
#'   significant result provides evidence against MCAR.
#'
#'   Note that Little's MCAR test has a number of problems (see Enders, 2010).
#'     \itemize{
#'        \item{\strong{First}}{, the test does not identify the specific variables
#'        that violates MCAR, i.e., the test does not identify potential correlates
#'        of missingness (i.e., auxiliary variables).}
#'        \item{\strong{Second}}{, the test is based on multivariate normality,
#'        i.e., under departure from the normality assumption the test might be
#'        unreliable unless the sample size is large and is not suitable for
#'        categorical variables.}
#'        \item{\strong{Third}}{, the test investigates mean differences assuming
#'        that the missing data pattern share a common covariance matrix, i.e.,
#'        the test cannot detect covariance-based deviations from MCAR stemming
#'        from a Missing at Random (MAR) or Missing Not at Random (MNAR) mechanism
#'        because MAR and MNAR mechanisms can also produce missing data subgroups
#'        with equal means.}
#'        \item{\strong{Fourth}}{, simulation studies suggest that Little's MCAR
#'        test suffers from low statistical power, particularly when the number
#'        of variables that violate MCAR is small, the relationship between the
#'        data and missingness is weak, or the data are MNAR (Thoemmes & Enders,
#'        2007).}
#'        \item{\strong{Fifth}}{, the test can only reject, but cannot prove the
#'        MCAR assumption, i.e., a statistically not significant result and failing
#'        to reject the null hypothesis of the MCAR test does not prove the null
#'        hypothesis that the data is MCAR.}
#'        \item{\strong{Sixth}}{, under the null hypothesis the data are actually
#'        MCAR or MNAR, while a statistically significant result indicates that
#'        missing data are MAR or MNAR, i.e., MNAR cannot be ruled out regardless
#'        of the result of the test.}
#'   }
#'   The function for performing Little's MCAR test is based on the \code{mlest}
#'   function from the \pkg{mvnmle} package which can handle up to 50 variables.
#'   Note that the \code{mcar_test} function in the \pkg{naniar} package is based
#'   on the \code{prelim.norm} function from the \pkg{norm} package. This function
#'   can handle about 30 variables, but with more than 30 variables specified in
#'   the argument \code{data}, the \code{prelim.norm} function might run into
#'   numerical problems leading to results that are not trustworthy (i.e.,
#'   \code{p.value = 1}). In that case, the warning message
#'   \code{In norm::prelim.norm(data) : NAs introduced by coercion to integer range}
#'   is printed on the console.}
#'
#'   \item{\strong{Jamshidian and Jalalꞌs Approach for Testing MCAR}}{Jamshidian
#'   and Jalal (2010) proposed an approach for testing the Missing Completely at
#'   Random (MCAR) assumption based on two tests of multivariate normality and
#'   homogeneity of covariances among groups of cases with identical missing data
#'   patterns. In the first step, missing data are multiply imputed (\code{m = 20}
#'   times by default) using a non-parametric imputation method (\code{method = "npar"}
#'   by default) by Sirvastava and Dolatabadi (2009) or using a parametric
#'   imputation method assuming multivariate normality of data (\code{method = "normal"})
#'   for each group of cases sharing a common missing data pattern. In the second
#'   step, a modified Hawkins test for multivariate normality and homogeneity of
#'   covariances applicable to complete data consisting of groups with a small
#'   number of cases is performed. A statistically not significant result indicates
#'   no evidence against multivariate normality of data or homogeneity of covariances,
#'   while a statistically significant result provides evidence against multivariate
#'   normality of data or homogeneity of covariances (i.e., violation of the MCAR
#'   assumption). Note that the Hawkins test is a test of multivariate normality
#'   as well as homogeneity of covariance. Hence, a statistically significant test
#'   is ambiguous unless the researcher assumes multivariate normality of data.
#'   In the third step, if the Hawkins test is statistically significant, the
#'   Anderson-Darling non-parametric test is performed. A statistically not
#'   significant result indicates evidence against multivariate normality of data
#'   but no evidence against homogeneity of covariances, while a statistically
#'   significant result provides evidence against homogeneity of covariances
#'   (i.e., violation of the MCAR assumption). However, no conclusions can be
#'   made about the multivariate normality of data when the Anderson-Darling
#'   non-parametric test is statistically significant. In summary, a statistically
#'   significant result of both the Hawkins and the Anderson-Darling non-parametric
#'   test provides evidence against the MCAR assumption. The test statistic and
#'   the significance values of the Hawkins test and the Anderson-Darling
#'   non-parametric based on multiply imputed data sets are pooled by computing
#'   the median test statistic and significance value (\code{pool = "med"} by
#'   default) as suggested by Eekhout, Wiel, and Heymans (2017).
#'
#'   Note that out of the problems listed for the Little's MCAR test the first,
#'   second (i.e., approach is not suitable for categorical variables), fifth,
#'   and sixth problems also apply to the Jamshidian and Jalalꞌs approach for
#'   testing the MCAR assumption.
#'   }
#'   In practice, rejecting or not rejecting the MCAR assumption may not be relevant
#'   as modern missing data handling methods like full information maximum likelihood
#'   (FIML) estimation, Bayesian estimation, or multiple imputation are asymptotically
#'   valid under the missing at random (MAR) assumption (Jamshidian & Yuan, 2014).
#'   It is more important to distinguish MAR from missing not at random (MNAR),
#'   but MAR and MNAR mechanisms cannot be distinguished without auxiliary
#'   information.
#' }
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
#' Beaujean, A. A. (2012). \emph{BaylorEdPsych: R Package for Baylor University
#' Educational Psychology Quantitative Courses}. R package version 0.5.
#' http://cran.nexr.com/web/packages/BaylorEdPsych/index.html
#'
#' Eekhout, I., M. A. Wiel, & M. W. Heymans (2017). Methods for significance
#' testing of categorical covariates in logistic regression models after multiple
#' imputation: Power and applicability analysis. \emph{BMC Medical Research
#' Methodology}, 17:129. https://doi.org/10.1186/s12874-017-0404-7
#'
#' Enders, C. K. (2010). \emph{Applied missing data analysis}. Guilford Press.
#'
#' Little, R. J. A. (1988). A test of Missing Completely at Random for multivariate
#' data with missing values. \emph{Journal of the American Statistical Association,
#' 83}, 1198-1202. https://doi.org/10.2307/2290157
#'
#' Jamshidian, M., & Jalal, S. (2010). Tests of homoscedasticity, normality, and
#' missing completely at random for incomplete multivariate data. \emph{Psychometrika,
#' 75}(4), 649-674. https://doi.org/10.1007/s11336-010-9175-3
#'
#' Jamshidian, M., & Yuan, K.H. (2014). Examining missing data mechanisms via
#' homogeneity of parameters, homogeneity of distributions, and multivariate
#' normality. \emph{WIREs Computational Statistics, 6}(1), 56-73.
#' https://doi.org/10.1002/wics.1287
#'
#' Mortaza, J., Siavash, J., Camden, J., & Kobayashi, M. (2024). \emph{MissMech:
#' Testing Homoscedasticity, Multivariate Normality, and Missing Completely at
#' Random}. R package version 1.0.4. https://doi.org/10.32614/CRAN.package.MissMech
#'
#' Srivastava, M.S., & Dolatabadi, M. (2009). Multiple imputation and other
#' resampling scheme for imputing missing observations. \emph{Journal of Multivariate
#' Analysis, 100}, 1919-1937. https://doi.org/10.1016/j.jmva.2009.06.003
#'
#' Thoemmes, F., & Enders, C. K. (2007, April). \emph{A structural equation model for
#' testing whether data are missing completely at random}. Paper presented at the
#' annual meeting of the American Educational Research Association, Chicago, IL.
#'
#' @note The code for Little's MCAR test is a modified copy of the \code{LittleMCAR}
#' function in the \pkg{BaylorEdPsych} package by A. Alexander Beaujean. The code
#' for Jamshidian and Jalalꞌs approach is a modified copy of the \code{TestMCARNormality}
#' function in the \pkg{MissMech} package by Mortaza Jamshidian, Siavash Jalal,
#' Camden Jansen, and Mao Kobayashi (2024).
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#'
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{data}}{matrix or data frame specified in \code{x}}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{result}}{list with result tables, i.e., \code{little} for the
#'                      result table of the Little's MCAR test, \code{jamjal}
#'                      for the list with results of the Jamshidian and Jalalꞌs
#'                      approach, \code{hawkins} for the result table of the
#'                      Hawkins test, and \code{anderson} for the result table of
#'                      the Anderson-Darling non-parametric test}
#'
#' @export
#'
#' @examples
#' # Example 1a: Perform Little's MCAR test and Jamshidian and Jalalꞌs approach
#' na.test(airquality)
#'
#' # Example 1b: Alternative specification using the 'data' argument,
#' na.test(., data = airquality)
#'
#' # Example 2: Perform Jamshidian and Jalalꞌs approach
#' na.test(airquality, print = "jamjal")
#'
#' \dontrun{
#' # Example 3: Write results into a text file
#' na.test(airquality, write = "NA_Test.txt")
#' }
na.test <- function(..., data = NULL, print = c("all", "little", "jamjal"),
                    impdat = NULL, delete = 6, method = c("npar", "normal"),
                    m = 20, seed = 123, nrep = 10000, n.min = 30,
                    pool = c("m", "med", "min", "max", "random"),
                    alpha = 0.05, digits = 2, p.digits = 3, as.na = NULL,
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
    var.names <- .var.names(..., data = data, check.chr = "data frame")

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
  ## All Missing ####

  prop.na <- misty::na.prop(x)
  if (isTRUE(any(prop.na == 1L))) {

    x <- x[-which(prop.na == 1L), ]

    warning(sum(prop.na == 1L), " cases with missing on all variables were removed from the analysis.", call. = FALSE)

  }

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

    # Check input 'print'
    if (isTRUE(!all(print %in% c("all", "little", "jamjal")))) { stop("Character strings in the argument 'print' do not all match with \"all\", \"little\", or \"jamjal\".", call. = FALSE) }

    # R package 'mvnmle'
    if (isTRUE(print %in% c("all", "little"))) { if (isTRUE(!nzchar(system.file(package = "mvnmle")))) { stop("Package \"mvnmle\" is needed for this function, please install the package.", call. = FALSE) } }

    # Check input 'impdat'
    if (isTRUE(!is.null(impdat))) {

      # R package mice
      if (isTRUE(!nzchar(system.file(package = "mice")))) { stop("Package \"mice\" is needed for this function, please install the package.", call. = FALSE) }

      # mids object
      if (isTRUE(!inherits(impdat, "mids"))) { stop("Please specify a \"mids\" object from the mice package for the argument 'impdat'.", call. = FALSE) }

      # Variables in mids object
      if (isTRUE(any(!colnames(data) %in% colnames(mice::complete(impdat))))) { stop("Variables specified for the analysis are not all avaialble in the \"mids\" object in the argument 'impdat'.", call. = FALSE) }

    }

    # Check input 'delete'
    if (isTRUE(delete %% 1L != 0L || delete < 0L || delete < 2L)) { stop("Please specify a positive integer number greater than 2 for the argument 'delete'.", call. = FALSE) }

    # Check input 'method'
    if (isTRUE(!all(method  %in% c("npar", "normal")))) { stop("Character string in the argument 'method' does not match with \"npar\", or \"normal\".", call. = FALSE) }

    # Check input 'm'
    if (isTRUE(m %% 1L != 0L || m < 0L || m < 1L)) { stop("Please specify a positive integer number for the argument 'm'.", call. = FALSE) }

    # Check input 'nrep'
    if (isTRUE(nrep %% 1L != 0L || nrep < 0L || nrep < 1L)) { stop("Please specify a positive integer number for the argument 'nrep'.", call. = FALSE) }

    # Check input 'n.min'
    if (isTRUE(n.min %% 1L != 0L || n.min < 0L || n.min < 1L)) { stop("Please specify a positive integer number for the argument 'n.min'.", call. = FALSE) }

    # Check input 'pool'
    if (isTRUE(!all(pool %in% c("m", "med", "min", "max", "random")))) { stop("Character string in the argument 'pool' does not match with \"m\", \"med\", \"min\", \"max\", or \"random\".", call. = FALSE) }

    # Check input 'alpha'
    if (isTRUE(alpha <= 0L || alpha >= 1L)) { stop("Please specify a value between 0 aund 1 for the argument 'alpha'.", call. = FALSE) }

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
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'print' Argument ####

  if (isTRUE(all(c("all", "little", "jamjal") %in% print))) {

    print <- "little"

  } else if (isTRUE("all" %in% print)) {

    print <- c("little", "jamjal")

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'print' Argument ####

  if (isTRUE(all(c("npar", "normal") %in% method))) { method <- "npar" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'pool' Argument ####

  if (isTRUE(all(c("med", "min", "max", "random") %in% pool))) { pool <- "med" }

  #_____________________________________________________________________________
  #
  # Main Function: Little's MCAR Test ------------------------------------------

  if (isTRUE("little" %in% print)) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Check ####

    # More than 50 variables
    if (isTRUE(ncol(x.matrix) > 50L)) { stop("The mlest function from the mvnmle package used to perform Little's MCAR test cannot handle more than 50 variables.", call. = FALSE) }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Little's MCAR Test ####

    restab.LittleMCAR <- .LittleMCAR(x.matrix)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Result table ####

    restab.little <- data.frame(no.cases = nrow(x), no.incomplete = misty::na.descript(x, data = NULL, output = FALSE)$result$L1$no.incomplete.l1, no.pattern = restab.LittleMCAR$missing.patterns,
                                statistic = restab.LittleMCAR$chi.square, df = restab.LittleMCAR$df, pval = restab.LittleMCAR$p.value, row.names = NULL)

  } else {

    restab.little <- NULL

  }

  #_____________________________________________________________________________
  #
  # Main Function: Jamshidian and Jalalꞌs Approach -----------------------------

  if (isTRUE("jamjal" %in% print)) {

    restab.TestMCARNormality <- .TestMCARNormality(data = x, delete = delete, m = m, method = method, nrep = nrep, n.min = n.min, seed = seed, pool = pool, impdat = impdat)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Result table ####

    restab.hawkins <- data.frame(no.cases = nrow(restab.TestMCARNormality$dat.analysis), no.incomplete = misty::na.descript(restab.TestMCARNormality$dat.analysis, data = NULL, output = FALSE)$result$L1$no.incomplete.l1, no.pattern = restab.TestMCARNormality$g,
                                 statistic = restab.TestMCARNormality$tsa.hawkins, df = 2L*restab.TestMCARNormality$g, pval = restab.TestMCARNormality$pa.hawkins, row.names = NULL)

    restab.anderson <- data.frame(no.cases = nrow(restab.TestMCARNormality$dat.analysis), no.incomplete = misty::na.descript(restab.TestMCARNormality$dat.analysis, data = NULL, output = FALSE)$result$L1$no.incomplete.l1, no.pattern = restab.TestMCARNormality$g,
                                  statistic = restab.TestMCARNormality$tsa.anderson, pval = restab.TestMCARNormality$pa.anderson, row.names = NULL)

  } else {

    restab.TestMCARNormality <- restab.hawkins <- restab.anderson <- NULL

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "na.test",
                 data = x,
                 args = list(print = print, impdat = impdat, delete = delete, method = method,
                             m = m, seed = seed, nrep = nrep, n.min = n.min, pool = pool,
                             alpha = alpha, digits = digits, p.digits = p.digits, as.na = NULL,
                             write = write, append = append, check = TRUE, output = TRUE),
                 result = list(little = restab.little,
                               jamjal = restab.TestMCARNormality, hawkins = restab.hawkins, anderson = restab.anderson))

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Write Results --------------------------------------------------------------

  if (isTRUE(!is.null(write))) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Text file ####

    # Send R output to text file
    sink(file = write, append = ifelse(isTRUE(file.exists(write)), append, FALSE), type = "output", split = FALSE)

    if (isTRUE(append && file.exists(write))) { write("", file = write, append = TRUE) }

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
