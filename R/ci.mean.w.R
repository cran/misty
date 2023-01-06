#' Within-Subject Confidence Intervals for the Arithmetic Mean
#'
#' This function computes difference-adjusted Cousineau-Morey within-subject
#' confidence interval for the arithmetic mean.
#'
#' The Cousineau within-subject confidence interval(CI, Cousineau, 2005) is an
#' alternative to the Loftus-Masson within-subject CI (Loftus & Masson, 1994)
#' that does not assume sphericity or homogeneity of covariances. This approach
#' removes individual differences by normalizing the raw scores using
#' participant-mean centering and adding the grand mean back to every score:
#'
#' \deqn{Y_{ij}^{'} = Y_{ij} - \hat{\mu}_i + \hat{\mu}_{grand}}
#'
#' where \eqn{Y_^'{ij}} is the score of the \eqn{i}th participant in condition
#' \eqn{j} (for \eqn{i = 1} to \eqn{n}), \eqn{\hat{\mu}_i} is the mean of
#' participant \eqn{i} across all \eqn{J} levels (for \eqn{j = 1} to \eqn{J}),
#' and \eqn{\hat{\mu}_{grand}} is the grand mean.
#'
#' Morey (2008) pointed out that Cousineau's (2005) approach produces intervals
#' that are consistently too narrow due to inducing a positive covariance
#' between normalized scores within a condition introducing bias into the
#' estimate of the sample variances. The degree of bias is proportional to the
#' number of means and can be removed by rescaling the confidence interval by
#' a factor of \eqn{\sqrt{J - 1}/J}:
#'
#' \deqn{\hat{\mu}_j \pm t_{n - 1, 1 - \alpha/2} \sqrt{\frac{J}{J - 1}} \hat{\sigma}^{'}_{{\hat{\mu}}_j}
#'
#' where \eqn{\hat{\sigma}^{'}_{{\mu}_j}} is the standard error of the mean computed
#' from the normalized scores of he \eqn{j}th factor level.
#'
#' Baguley (2012) pointed out that the Cousineau-Morey interval is larger than
#' that for a difference in means by a factor of \eqn{\sqrt{2}} leading to a
#' misinterpretation of these intervals that overlap of 95% confidence intervals
#' around individual means is indicates that a 95% confidence interval for the
#' difference in means would include zero. Hence, following adjustment to the
#' Cousineau-Morey interval was proposed:
#'
#' \deqn{\hat{\mu}_j \pm \frac{\sqrt{2}}{2} (t_{n - 1, 1 - \alpha/2} \sqrt{\frac{J}{J - 1}} \hat{\sigma}^'_{\hat{\mu}}_j)}
#'
#' The adjusted Cousineau-Morey interval is informative about the pattern of
#' differences between means and is computed by default (i.e., \code{adjust = TRUE}).
#'
#' @param x            a matrix or data frame with numeric variables representing
#'                     the levels of the within-subject factor, i.e., data are
#'                     specified in wide-format (i.e., multivariate person level
#'                     format).
#' @param adjust       logical: if \code{TRUE} (default), difference-adjustment
#'                     for the Cousineau-Morey within-subject confidence intervals
#'                     is applied.
#' @param alternative  a character string specifying the alternative hypothesis,
#'                     must be one of \code{"two.sided"} (default), \code{"greater"}
#'                     or \code{"less"}.
#' @param conf.level   a numeric value between 0 and 1 indicating the confidence
#'                     level of the interval.
#' @param na.omit       logical: if \code{TRUE}, incomplete cases are removed
#'                      before conducting the analysis (i.e., listwise deletion).
#' @param digits       an integer value indicating the number of decimal places
#'                     to be used.
#' @param as.na         a numeric vector indicating user-defined missing values,
#'                      i.e. these values are converted to \code{NA} before
#'                      conducting the analysis.
#' @param check        logical: if \code{TRUE}, argument specification is checked.
#' @param output       logical: if \code{TRUE}, output is shown on the console.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{aov.w}}, \code{\link{test.z}}, \code{\link{test.t}},
#' \code{\link{ci.mean.diff}},' \code{\link{ci.median}}, \code{\link{ci.prop}},
#' \code{\link{ci.var}}, \code{\link{ci.sd}}, \code{\link{descript}}
#'
#' @references
#' Baguley, T. (2012). Calculating and graphing within-subject confidence intervals
#' for ANOVA. \emph{Behavior Research Methods, 44}, 158-175.
#' https://doi.org/10.3758/s13428-011-0123-7
#'
#' Cousineau, D. (2005) Confidence intervals in within-subject designs: A simpler
#' solution to Loftus and Masson’s Method. \emph{Tutorials in Quantitative Methods
#' for Psychology, 1}, 42–45.  https://doi.org/10.20982/tqmp.01.1.p042
#'
#' Loftus, G. R., and Masson, M. E. J. (1994). Using confidence intervals in
#' within-subject designs. \emph{Psychonomic Bulletin and Review, 1}, 476–90.
#' https://doi.org/10.3758/BF03210951
#'
#' Morey, R. D. (2008). Confidence intervals from normalized data: A correction
#' to Cousineau. \emph{Tutorials in Quantitative Methods for Psychology, 4}, 61–4.
#' https://doi.org/10.20982/tqmp.01.1.p042
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries: function call (\code{call}), type of analysis \code{type}, list with
#' the input specified in \code{x} (\code{data}), specification of function arguments
#' (\code{args}), and result table (\code{result}).
#'
#' @export
#'
#' @examples
#' dat <- data.frame(time1 = c(3, 2, 1, 4, 5, 2, 3, 5, 6, 7),
#'                   time2 = c(4, 3, 6, 5, 8, 6, 7, 3, 4, 5),
#'                   time3 = c(1, 2, 2, 3, 6, 5, 1, 2, 4, 6))
#'
#' # Difference-adjusted Cousineau-Morey confidence intervals
#' ci.mean.w(dat)
#'
#' # Cousineau-Morey confidence intervals
#' ci.mean.w(dat, adjust = FALSE)
ci.mean.w <- function(x, adjust = TRUE, alternative = c("two.sided", "less", "greater"),
                      conf.level = 0.95, na.omit = TRUE, digits = 2, as.na = NULL,
                      check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'x' is missing
  if (isTRUE(missing(x))) { stop("Please specify a matrix or data frame for the argument 'data'.", call. = FALSE) }

  # Check if input 'x' is NULL
  if (isTRUE(is.null(x))) { stop("Input specified for the argument 'data' is NULL.", call. = FALSE) }


  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # Check input 'adjust'
    if (isTRUE(!is.logical(adjust))) { stop("Please specify TRUE or FALSE for the argument 'adjust'.", call. = FALSE) }

    # Check input 'alternative'
    if (isTRUE(!all(alternative %in% c("two.sided", "less", "greater")))) { stop("Character string in the argument 'alternative' does not match with \"two.sided\", \"less\", or \"greater\".", call. = FALSE) }

    # Check input 'conf.level'
    if (isTRUE(conf.level >= 1L || conf.level <= 0L)) { stop("Please specifiy a numeric value between 0 and 1 for the argument 'conf.level'.", call. = FALSE) }

    # Check input 'na.omit'
    if (isTRUE(!is.logical(na.omit))) { stop("Please specify TRUE or FALSE for the argument 'na.omit'.", call. = FALSE) }

    # Check input 'digits'
    if (isTRUE(digits %% 1L != 0L || digits < 0L)) { stop("Please specify a positive integer number for the argument 'digits'.", call. = FALSE) }

    # Check input 'output'
    if (isTRUE(!is.logical(output))) { stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Data and Arguments ---------------------------------------------------------

  # Global variable
  data.id <- var.formula <- variable <- NULL

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data frame ####

  x <- as.data.frame(x, stringsAsFactors = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) {

    # Replace user-specified values with missing values
    x[, var.formula] <- misty::as.na(x[, var.formula], na = as.na, check = check)

    # Dependent variable with missing values only
    if (isTRUE(any(sapply(x, function(y) all(is.na(y)))))) { stop("After converting user-missing values into NA, a variable is completely missing.", call. = FALSE) }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Listwise deletion ####

  if (isTRUE(any(is.na(x)))) {

    if (isTRUE(na.omit)) {

      # Listwise deletion
      x <- na.omit(x)

      warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ",
                     length(attributes(x)$na.action)), call. = FALSE, immediate. = FALSE)

      # Check if at least 2 cases are available
      if (isTRUE(nrow(x) < 2L)) { stop("After listwise deletion, there are not enough cases for conducting the analysis.", call. = FALSE) }

    } else {

      # Remove cases with NA on all variables
      x <- data.id[which(apply(x, 1L, function(y) !all(is.na(y)))), ]

      if (nrow(x) < 2L) { stop("After removing cases with NA on all variables, there are not enough cases for conducting the analysis.", call. = FALSE) }

      if (isTRUE(any(is.na(x)))) {

        warning("Confidence intervals might not be reliable due to the presence of missing data.", call. = FALSE)

      }

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Alternative hypothesis ####

  if (isTRUE(all(c("two.sided", "less", "greater") %in% alternative))) { alternative <- "two.sided" }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  # Number of within-subject factor levels
  k <- ncol(x)

  # Number of cases
  n <- nrow(x)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Correction and adjustment factor ####

  # Correction factor
  c.factor <- sqrt(k / (k - 1L))

  # Difference-adjustment factor
  adjust.factor <- ifelse(isTRUE(adjust), sqrt(2L) / 2L, 1L)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Normalizing data ####

  # Participant mean
  p.means <- rowMeans(x, na.rm = TRUE)

  # Normalized scores
  norm.x <- x - p.means + mean(unlist(x), na.rm = TRUE)

  # Standard error of the mean
  norm.se <- misty::descript(norm.x, output = FALSE)$result[c("n", "nNA", "pNA", "se.m")]

  # Means and standard deviation
  norm.m <- sapply(norm.x, function(y) mean(y, na.rm = TRUE))
  norm.sd <- sapply(norm.x, function(y) sd(y, na.rm = TRUE))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Confidence intervals ####

  # t quantile
  crit <- qt(switch(alternative,
                    two.sided = 1L - (1L - conf.level) / 2L,
                    less = conf.level,
                    greater = conf.level), df = n - 1L)

  # Confidence interval
  ci <- switch(alternative,
               two.sided = data.frame(low = norm.m - adjust.factor * (crit * c.factor * norm.se$se.m),
                                      upp = norm.m + adjust.factor * (crit * c.factor * norm.se$se.m)),
               less = data.frame(low = -Inf,
                                 upp = norm.m + adjust.factor * (crit * c.factor * norm.se$se.m)),
               greater = data.frame(low = norm.m - adjust.factor * (crit * c.factor * norm.se$se.m),
                                    upp = Inf))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Result object ####

  result <- data.frame(variable = names(x), norm.se[, c("n", "nNA", "pNA")],
                       m = norm.m, sd = norm.sd, se = norm.se[, "se.m"],
                       low = ci$low, upp = ci$upp, row.names = NULL)

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "ci",
                 ci = "mean.w",
                 data = x,
                 args = list(adjust = adjust, alternative = alternative,
                             conf.level = conf.level, na.omit = na.omit,
                             digits = digits, as.na = as.na,
                             check = check, output = output),
                 result = result)

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
