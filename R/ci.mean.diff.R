#' Confidence Interval for the Difference in Arithmetic Means
#'
#' This function computes a confidence interval for the difference in arithmetic
#' means in a one-sample, two-sample and paired-sample design with known or unknown
#' population standard deviation or population variance for one or more variables,
#' optionally by a grouping and/or split variable.
#'
#' @param x              a numeric vector of data values.
#' @param y              a numeric vector of data values.
#' @param mu             a numeric value indicating the population mean under the
#'                       null hypothesis. Note that the argument \code{mu} is only
#'                       used when \code{y = NULL}.
#' @param sigma          a numeric vector indicating the population standard deviation(s)
#'                       when computing confidence intervals for the difference in
#'                       arithmetic means with known standard deviation(s). In case
#'                       of independent samples, equal standard deviations are assumed
#'                       when specifying one value for the argument \code{sigma}; when
#'                       specifying two values for the argument \code{sigma}, unequal
#'                       standard deviations are assumed. Note that either argument
#'                       \code{sigma} or argument \code{sigma2} is specified and it
#'                       is only possible to specify one value (i.e., equal variance
#'                       assumption) or two values (i.e., unequal variance assumption)
#'                       for the argument \code{sigma} even though multiple variables
#'                       are specified in \code{x}.
#' @param sigma2         a numeric vector indicating the population variance(s) when
#'                       computing confidence intervals for the difference in arithmetic
#'                       means with known variance(s). In case of independent samples,
#'                       equal variances are assumed when specifying one value for the
#'                       argument \code{sigma2}; when specifying two values for the
#'                       argument \code{sigma}, unequal variances are assumed. Note
#'                       that either argument \code{sigma} or argument \code{sigma2}
#'                       is specified and it is only possible to specify one value
#'                       (i.e., equal variance assumption) or two values (i.e., unequal
#'                       variance assumption) for the argument \code{sigma} even though
#'                       multiple variables are specified in \code{x}.
#' @param var.equal      logical: if \code{TRUE}, the population variance in the
#'                       independent samples are assumed to be equal.
#' @param paired         logical: if \code{TRUE}, confidence interval for the difference
#'                       of arithmetic means in paired samples is computed.
#' @param alternative    a character string specifying the alternative hypothesis,
#'                       must be one of \code{"two.sided"} (default), \code{"greater"}
#'                       or \code{"less"}.
#' @param conf.level     a numeric value between 0 and 1 indicating the confidence
#'                       level of the interval.
#' @param group          a numeric vector, character vector or factor as grouping
#'                       variable. Note that a grouping variable can only be used
#'                       when computing confidence intervals with unknown population
#'                       standard deviation and population variance.
#' @param split          a numeric vector, character vector or factor as split variable.
#'                       Note that a split variable can only be used when computing
#'                       confidence intervals with unknown population
#' @param sort.var       logical: if \code{TRUE}, output table is sorted by variables
#'                       when specifying \code{group}.
#' @param digits         an integer value indicating the number of decimal places to
#'                       be used.
#' @param as.na          a numeric vector indicating user-defined missing values,
#'                       i.e. these values are converted to \code{NA} before conducting
#'                       the analysis. Note that \code{as.na()} function is only applied
#'                       to \code{x}, but not to \code{group} or \code{split}.
#' @param check          logical: if \code{TRUE}, argument specification is checked.
#' @param output         logical: if \code{TRUE}, output is shown on the console.
#' @param formula        a formula of the form \code{y ~ group} for one outcome variable
#'                       or \code{cbind(y1, y2, y3) ~ group} for more than one outcome
#'                       variable where \code{y} is a numeric variable giving the data
#'                       values and \code{group} a numeric variable, character variable
#'                       or factor with two values or factor levels giving the corresponding
#'                       groups.
#' @param data           a matrix or data frame containing the variables in the formula
#'                       \code{formula}.
#' @param na.omit        logical: if \code{TRUE}, incomplete cases are removed before
#'                       conducting the analysis (i.e., listwise deletion) when specifying
#'                       more than one outcome variable.
#' @param ...            further arguments to be passed to or from methods.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{test.z}}, \code{\link{test.t}}, \code{\link{ci.mean}}, \code{\link{ci.median}},
#' \code{\link{ci.prop}}, \code{\link{ci.var}}, \code{\link{ci.sd}}, \code{\link{descript}}
#'
#' @references
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology
#' - Using R and SPSS}. John Wiley & Sons.
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#' \tabular{ll}{
#' \code{call} \tab function call \cr
#' \code{type} \tab type of analysis \cr
#' \code{data} \tab data frame with variables used in the current analysis \cr
#' \code{args} \tab specification of function arguments  \cr
#' \code{result} \tab result table \cr
#' }
#'
#' @export
#'
#' @examples
#' dat1 <- data.frame(group1 = c(1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2,
#'                               1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2),
#'                    group2 = c(1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 2, 2, 2,
#'                               1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2),
#'                    group3 = c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
#'                               1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2),
#'                    x1 = c(3, 1, 4, 2, 5, 3, 2, 3, 6, 4, 3, NA, 5, 3,
#'                           3, 2, 6, 3, 1, 4, 3, 5, 6, 7, 4, 3, 6, 4),
#'                    x2 = c(4, NA, 3, 6, 3, 7, 2, 7, 3, 3, 3, 1, 3, 6,
#'                           3, 5, 2, 6, 8, 3, 4, 5, 2, 1, 3, 1, 2, NA),
#'                    x3 = c(7, 8, 5, 6, 4, 2, 8, 3, 6, 1, 2, 5, 8, 6,
#'                           2, 5, 3, 1, 6, 4, 5, 5, 3, 6, 3, 2, 2, 4))
#'
#' #--------------------------------------
#' # One-sample design
#'
#' # Two-Sided 95% CI for x1
#' # population mean = 3
#' ci.mean.diff(dat1$x1, mu = 3)
#'
#' #--------------------------------------
#' # Two-sample design
#'
#' # Two-Sided 95% CI for y1 by group1
#' # unknown population variances, unequal variance assumption
#' ci.mean.diff(x1 ~ group1, data = dat1)
#'
#' # Two-Sided 95% CI for y1 by group1
#' # unknown population variances, equal variance assumption
#' ci.mean.diff(x1 ~ group1, data = dat1, var.equal = TRUE)
#'
#' # Two-Sided 95% CI with known standard deviations for x1 by group1
#' # known population standard deviations, equal standard deviation assumption
#' ci.mean.diff(x1 ~ group1, data = dat1, sigma = 1.2)
#'
#' # Two-Sided 95% CI with known standard deviations for x1 by group1
#' # known population standard deviations, unequal standard deviation assumption
#' ci.mean.diff(x1 ~ group1, data = dat1, sigma = c(1.5, 1.2))
#'
#' # Two-Sided 95% CI with known variance for x1 by group1
#' # known population variances, equal variance assumption
#' ci.mean.diff(x1 ~ group1, data = dat1, sigma2 = 1.44)
#'
#' # Two-Sided 95% CI with known variance for x1 by group1
#' # known population variances, unequal variance assumption
#' ci.mean.diff(x1 ~ group1, data = dat1, sigma2 = c(2.25, 1.44))
#'
#' # One-Sided 95% CI for y1 by group1
#' # unknown population variances, unequal variance assumption
#' ci.mean.diff(x1 ~ group1, data = dat1, alternative = "less")
#'
#' # Two-Sided 99% CI for y1 by group1
#' # unknown population variances, unequal variance assumption
#' ci.mean.diff(x1 ~ group1, data = dat1, conf.level = 0.99)
#'
#' # Two-Sided 95% CI for y1 by group1
#' # unknown population variances, unequal variance assumption
#' # print results with 3 digits
#' ci.mean.diff(x1 ~ group1, data = dat1, digits = 3)
#'
#' # Two-Sided 95% CI for y1 by group1
#' # unknown population variances, unequal variance assumption
#' # convert value 4 to NA
#' ci.mean.diff(x1 ~ group1, data = dat1, as.na = 4)
#'
#' # Two-Sided 95% CI for y1, y2, and y3 by group1
#' # unknown population variances, unequal variance assumption
#' ci.mean.diff(cbind(x1, x2, x3) ~ group1, data = dat1)
#'
#' # Two-Sided 95% CI for y1, y2, and y3 by group1
#' # unknown population variances, unequal variance assumption,
#' # listwise deletion for missing data
#' ci.mean.diff(cbind(x1, x2, x3) ~ group1, data = dat1, na.omit = TRUE)
#'
#' # Two-Sided 95% CI for y1, y2, and y3 by group1
#' # unknown population variances, unequal variance assumption,
#' # analysis by group2 separately
#' ci.mean.diff(cbind(x1, x2, x3) ~ group1, data = dat1, group = dat1$group2)
#'
#' # Two-Sided 95% CI for y1, y2, and y3 by group1
#' # unknown population variances, unequal variance assumption,
#' # analysis by group2 separately, sort by variables
#' ci.mean.diff(cbind(x1, x2, x3) ~ group1, data = dat1, group = dat1$group2,
#'              sort.var = TRUE)# Check if input 'y' is NULL
#'
#' # Two-Sided 95% CI for y1, y2, and y3 by group1
#' # unknown population variances, unequal variance assumption,
#' # split analysis by group2
#' ci.mean.diff(cbind(x1, x2, x3) ~ group1, data = dat1, split = dat1$group2)
#'
#' # Two-Sided 95% CI for y1, y2, and y3 by group1
#' # unknown population variances, unequal variance assumption,
#' # analysis by group2 separately, split analysis by group3
#' ci.mean.diff(cbind(x1, x2, x3) ~ group1, data = dat1,
#'              group = dat1$group2, split = dat1$group3)
#'
#' #-----------------
#'
#' group1 <- c(3, 1, 4, 2, 5, 3, 6, 7)
#' group2 <- c(5, 2, 4, 3, 1)
#'
#' # Two-Sided 95% CI for the mean difference between group1 and group2
#' # unknown population variances, unequal variance assumption
#' ci.mean.diff(group1, group2)
#'
#' # Two-Sided 95% CI for the mean difference between group1 and group2
#' # unknown population variances, equal variance assumption
#' ci.mean.diff(group1, group2, var.equal = TRUE)
#'
#' #--------------------------------------
#' # Paired-sample design
#'
#' dat2 <- data.frame(pre = c(1, 3, 2, 5, 7, 6),
#'                    post = c(2, 2, 1, 6, 8, 9),
#'                    group = c(1, 1, 1, 2, 2, 2), stringsAsFactors = FALSE)
#'
#' # Two-Sided 95% CI for the mean difference in pre and post
#' # unknown poulation variance of difference scores
#' ci.mean.diff(dat2$pre, dat2$post, paired = TRUE)
#'
#' # Two-Sided 95% CI for the mean difference in pre and post
#' # unknown poulation variance of difference scores
#' # analysis by group separately
#' ci.mean.diff(dat2$pre, dat2$post, paired = TRUE, group = dat2$group)
#'
#' # Two-Sided 95% CI for the mean difference in pre and post
#' # unknown poulation variance of difference scores
#' # analysis by group separately
#' ci.mean.diff(dat2$pre, dat2$post, paired = TRUE, split = dat2$group)
#'
#' # Two-Sided 95% CI for the mean difference in pre and post
#' # known population standard deviation of difference scores
#' ci.mean.diff(dat2$pre, dat2$post, sigma = 2, paired = TRUE)
#'
#' # Two-Sided 95% CI for the mean difference in pre and post
#' # known population variance of difference scores
#' ci.mean.diff(dat2$pre, dat2$post, sigma2 = 4, paired = TRUE)
#'
#' # One-Sided 95% CI for the mean difference in pre and post
#' # unknown poulation variance of difference scores
#' ci.mean.diff(dat2$pre, dat2$post, alternative = "less", paired = TRUE)
#'
#' # Two-Sided 99% CI for the mean difference in pre and post
#' # unknown poulation variance of difference scores
#' ci.mean.diff(dat2$pre, dat2$post, conf.level = 0.99, paired = TRUE)
#'
#' # Two-Sided 95% CI for for the mean difference in pre and post
#' # unknown poulation variance of difference scores
#' # print results with 3 digits
#' ci.mean.diff(dat2$pre, dat2$post, paired = TRUE, digits = 3)
#'
#' # Two-Sided 95% CI for for the mean difference in pre and post
#' # unknown poulation variance of difference scores
#' # convert value 1 to NA
#' ci.mean.diff(dat2$pre, dat2$post, as.na = 1, paired = TRUE)
ci.mean.diff <- function(x, ...) {

  UseMethod("ci.mean.diff")

}

#_______________________________________________________________________________
#
# Confidence interval for the difference of arithmetic means -------------------

m.diff.conf <- function(x, y, sigma, var.equal, alternative, paired, conf.level,
                        side) {


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Independent samples ####
  if (!isTRUE(paired)) {

    #...................
    ### Data ####

    x <- na.omit(x)
    y <- na.omit(y)

    x.n <- length(x)
    y.n <- length(y)

    yx.mean <- mean(y) - mean(x)

    x.var <- var(x)
    y.var <- var(y)

    # At least 2 observations for x and y
    if (isTRUE(x.n >= 2L && y.n >= 2L & (x.var != 0L && y.var != 0L))) {

      #### Known Population SD ####
      if (isTRUE(!is.null(sigma))) {

        se <- sqrt((sigma[1L]^2L / x.n) + (sigma[2L]^2L / y.n))

        crit <- qnorm(switch(alternative,
                             two.sided = 1L - (1L - conf.level) / 2L,
                             less = conf.level,
                             greater = conf.level))

        term <- crit*se

      #### Unknown Population SD ####
      } else {

        #### Equal variance ####
        if (isTRUE(var.equal)) {

         se <- sqrt(((x.n - 1L)*x.var + (y.n - 1L)*y.var) / (x.n + y.n - 2L)) * sqrt(1 / x.n + 1L / y.n)

         crit <- qt(switch(alternative,
                           two.sided = 1L - (1L - conf.level) / 2L,
                           less = conf.level,
                           greater = conf.level), df = sum(x.n, y.n) - 2L)

         term <- crit*se

        #### Unequal variance ####
        } else {

          se <- sqrt(x.var / x.n + y.var / y.n)

          df <- (x.var / x.n + y.var / y.n)^2L / (((x.var / x.n)^2L / (x.n - 1L)) + ((y.var / y.n)^2L / (y.n - 1L)))

          crit <- qt(switch(alternative,
                            two.sided = 1L - (1L - conf.level) / 2L,
                            less = conf.level,
                            greater = conf.level), df = df)

          term <- crit*se

        }

      }

      #...................
      ### Confidence interval ####
      ci <- switch(alternative,
                   two.sided = c(low = yx.mean - term, upp = yx.mean + term),
                   less = c(low = -Inf, upp = yx.mean + term),
                   greater = c(low = yx.mean - term, upp = Inf))

    # Less than  2 observations for x and y
    } else {

      ci <- c(NA, NA)

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Dependent samples ####
  } else {

    xy.dat <- na.omit(data.frame(x = x, y = y, stringsAsFactors = FALSE))

    xy.diff <- xy.dat$y - xy.dat$x

    xy.diff.mean <- mean(xy.diff)

    xy.diff.sd <- sd(xy.diff)

    xy.diff.n <- nrow(xy.dat)

    # At least 2 observations for x
    if (isTRUE(xy.diff.n >= 2L && xy.diff.sd != 0L)) {

      #...................
      ### Known Population SD ####
      if (isTRUE(!is.null(sigma))) {

        se <- sigma / sqrt(xy.diff.n)

        crit <- qnorm(switch(alternative,
                             two.sided = 1L - (1L - conf.level) / 2L,
                             less = conf.level,
                             greater = conf.level))

        term <- crit*se

      #...................
      ### Unknown Population SD ####
      } else {

        se <- xy.diff.sd / sqrt(xy.diff.n)

        crit <- qt(switch(alternative,
                          two.sided = 1L - (1L - conf.level) / 2L,
                          less = conf.level,
                          greater = conf.level), df = xy.diff.n - 1L)

        term <- crit*se

      }

      ci <- switch(alternative,
                   two.sided = c(low = xy.diff.mean - term, upp = xy.diff.mean + term),
                   less = c(low = -Inf, upp = xy.diff.mean + term),
                   greater = c(low = xy.diff.mean - term, upp = Inf))

    # Less than 2 observations for x
    } else {

      ci <- c(NA, NA)

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Return Object ####
  object <- switch(side, both = ci, low = ci[1L], upp = ci[2L])

  return(object)

}

#_______________________________________________________________________________
#
# Default S3 method ------------------------------------------------------------

ci.mean.diff.default <- function(x, y = NULL, mu = 0, sigma = NULL, sigma2 = NULL, var.equal = FALSE,
                                 paired = FALSE, alternative = c("two.sided", "less", "greater"),
                                 conf.level = 0.95, group = NULL, split = NULL, sort.var = FALSE,
                                 digits = 2, as.na = NULL, check = TRUE, output = TRUE, ...) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'x' is missing
  if (isTRUE(missing(x))) { stop("Please specify a numeric vector for the argument 'x'", call. = FALSE) }

  # Check if input 'x' is NULL
  if (isTRUE(is.null(x))) { stop("Input specified for the argument 'x' is NULL.", call. = FALSE) }

  # Check if only one variable specified in the input 'x'
  if (ncol(data.frame(x)) != 1L) { stop("More than one variable specified for the argument 'x'.",call. = FALSE) }

  # Convert 'x' into a vector
  x <- unlist(x, use.names = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Check 'y' ####

  if (isTRUE(!is.null(y))) {

    # Check if input 'y' is missing
    if (isTRUE(missing(y))) { stop("Please specify a numeric vector for the argument 'y'", call. = FALSE) }

    # Check if only one variable specified in the input 'y'
    if (ncol(data.frame(y)) != 1L) { stop("More than one variable specified for the argument 'y'.",call. = FALSE) }

    # Convert 'y' into a vector
    y <- unlist(y, use.names = FALSE)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Check 'paired' ####

  if (isTRUE(!is.logical(paired))) { stop("Please specify TRUE or FALSE for the argument 'paired'.", call. = FALSE) }


  if (isTRUE(paired)) {

    # Length of 'x' and 'y'
    if (isTRUE(nrow(data.frame(x)) != nrow(data.frame(y)))) {

      stop("Length of the vector specified in 'x' does not match the length of the vector specified in 'y'.", call. = FALSE)

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Check 'group' ####

  if (isTRUE(!is.null(group))) {

    if (isTRUE(!paired)) { stop("Please use formula notation for using a grouping variable in independent samples.", call. = FALSE) }

    if (ncol(data.frame(group)) != 1) { stop("More than one grouping variable specified for the argument 'group'.",call. = FALSE) }

    if (isTRUE(paired)) {

      if (nrow(data.frame(group)) != nrow(data.frame(x))) {

        stop("Length of the vector or factor specified in the argument 'group' does not match with 'x'.", call. = FALSE)

      }

    }

    # Convert 'group' into a vector
    group <- unlist(group, use.names = FALSE)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Check 'split' ####

  if (isTRUE(!is.null(split))) {

    if (isTRUE(!paired)) { stop("Please use formula notation for using a split variable in independent samples.", call. = FALSE) }

    if (ncol(data.frame(split)) != 1) { stop("More than one split variable specified for the argument 'split'.",call. = FALSE) }

    if (isTRUE(paired)) {

      if (nrow(data.frame(split)) != nrow(data.frame(x))) { stop("Length of the vector or factor specified in the argument 'split' does not match with 'x'.", call. = FALSE) }

    }

    # Convert 'split' into a vector
    split <- unlist(split, use.names = FALSE)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## List or Dataframe ####

  # One sample
  if (isTRUE(is.null(y))) {

    xy <- data.frame(x = x, stringsAsFactors = FALSE)

  # Independent samples
  } else if (!isTRUE(paired)) {

    xy <- list(x = x, y = y)

  # Paired samples
  } else {

    xy <- data.frame(x = x, y = y, stringsAsFactors = FALSE)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Replace user-specified values with missing values ####

  if (isTRUE(!is.null(as.na))) {

    # Check input 'as.na'

    # Factor or Vector
    if (isTRUE(is.atomic(xy) || is.factor(xy))) {

      as.na.x <- !as.na %in% xy

    # Matrix or data frame
    } else if (isTRUE(is.matrix(xy) || is.data.frame(xy))) {

      as.na.x <- vapply(as.character(as.na), function(y) !y %in% misty::chr.trim(apply(as.matrix(x), 2, as.character)),
                        FUN.VALUE = logical(1L))

    # List
    } else if (isTRUE(is.list(xy))) {

      as.na.x <- !as.na %in% unlist(xy)

    }

    if (isTRUE(any(as.na.x))) {

      warning(paste0("Values specified in the argument 'as.na' were not found in 'x': ",
                     paste(as.na[as.na.x], collapse = ", ")), call. = FALSE)
    }

    # Replace user-specified values with NAs
    xy <- misty::as.na(xy, na = as.na, check = FALSE)

    # Variable with missing values only
    xy.miss <- vapply(xy, function(y) all(is.na(y)), FUN.VALUE = logical(1L))
    if (isTRUE(any(xy.miss))) {

      stop(paste0("After converting user-missing values into NA, following variables are completely missing: ",
                  paste(names(which(xy.miss)), collapse = ", ")), call. = FALSE)

    }

  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Listwise deletion ####

  if (isTRUE(paired && nrow(na.omit(xy)) < 2L)) {

    stop("After listwise deletion, the number of pairs of observations is less than two.", call. = FALSE)

  }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # Check input 'mu'
    if (isTRUE(length(mu) > 1L)) { stop("Please specify one numeric value for the argument 'mu'.", call. = FALSE) }

    # Check input 'sigma' and 'sigma2'
    if (isTRUE(!is.null(sigma) && !is.null(sigma2))) {

      if (isTRUE(!identical(sigma^2, sigma2))) { stop("Arguments 'sigma' and 'sigma2' do not match.", call. = FALSE) }

    }

    # Check input 'sigma'
    if (isTRUE(!is.null(sigma))) {

      # SD smaller or equal 0
      if (isTRUE(any(sigma <= 0L))) { stop("Please specify numeric values grater than 0 for the argument 'sigma'.", call. = FALSE) }

      if (isTRUE(paired)) {

        # Length of 'sigma'
        if (isTRUE(length(sigma) > 1L)) { stop("Please specify one numeric values for the argument 'sigma' in dependent samples.", call. = FALSE) }

      } else {

        # Length of 'sigma'
        if (isTRUE(length(sigma) > 2L)) { stop("Please specify one or two numeric values for the argument 'sigma' in independent samples.", call. = FALSE) }

      }

    }

    # Check input 'sigma2'
    if (isTRUE(!is.null(sigma2))) {

      # Variance smaller or equal 0
      if (isTRUE(any(sigma2 <= 0L))) { stop("Please specify numeric values grater than 0 for the argument 'sigma2'.", call. = FALSE) }

      if (isTRUE(paired)) {

        # Length of 'sigma2'
        if (isTRUE(length(sigma2) > 1L)) { stop("Please specify one numeric values for the argument 'sigma2' in dependent samples.", call. = FALSE) }

      } else {

        # Length of 'sigma2'
        if (isTRUE(length(sigma2) > 2L)) { stop("Please specify one or two numeric values for the argument 'sigma2' in independent samples.", call. = FALSE) }

      }

    }

    # Check input 'var.equal'
    if (isTRUE(!is.logical(var.equal))) { stop("Please specify TRUE or FALSE for the argument 'var.equal'.", call. = FALSE) }

    # Check input 'alternative'
    if (isTRUE(!all(alternative %in%  c("two.sided", "less", "greater")))) {

      stop("Character string in the argument 'alternative' does not match with \"two.sided\", \"less\", or \"greater\".", call. = FALSE)

    }

    # Check input 'conf.level'
    if (isTRUE(conf.level >= 1L || conf.level <= 0L)) { stop("Please specifiy a numeric value between 0 and 1 for the argument 'conf.level'.", call. = FALSE) }

    # Check input 'group'
    if (isTRUE(!is.null(group))) {

      # Population standard deviation
      if (isTRUE(!is.null(sigma))) { stop("Grouping variable cannot be used for confidence intervals with known population standard deviation.", call. = FALSE) }

      # Population variance
      if (isTRUE(!is.null(sigma2))) { stop("Grouping variable cannot be used for confidence intervals with known population variance.", call. = FALSE) }

      # Input 'group' completely missing
      if (isTRUE(all(is.na(group)))) { stop("The grouping variable specified in 'group' is completely missing.", call. = FALSE) }

      # Only one group in 'group'
      if (isTRUE(length(na.omit(unique(group))) == 1L)) { warning("There is only one group represented in the grouping variable specified in 'group'.", call. = FALSE) }

    }

    # Check input 'split'
    if (isTRUE(!is.null(split))) {

      # Independent samples
      if (!isTRUE(paired)) { stop("Please use formula notation for using a split variable in paired samples.", call. = FALSE) }

      # Population standard deviation
      if (isTRUE(!is.null(sigma))) { stop("Split variable cannot be used for confidence intervals with known population standard deviation.", call. = FALSE) }

      # Population variance
      if (isTRUE(!is.null(sigma2))) { stop("Split variable cannot be used for confidence intervals with known population variance.", call. = FALSE) }

      # Input 'split' completely missing
      if (isTRUE(all(is.na(split)))) { stop("The split variable specified in 'split' is completely missing.", call. = FALSE) }

      # Only one group in 'split'
      if (isTRUE(length(na.omit(unique(split))) == 1L)) { warning("There is only one group represented in the split variable specified in 'split'.", call. = FALSE) }

    }

    # Check input 'sort.var'
    if (isTRUE(!is.logical(sort.var))) { stop("Please specify TRUE or FALSE for the argument 'sort.var'.", call. = FALSE) }

    # Check input 'digits'
    if (isTRUE(digits %% 1L != 0L || digits < 0L)) { stop("Please specify a positive integer number for the argument 'digits'.", call. = FALSE) }

    # Check input output
    if (isTRUE(!is.logical(output))) { stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Population standard deviation and variance ####

  if (isTRUE(is.null(sigma) && !is.null(sigma2))) { sigma <- sqrt(sigma2) }

  if (isTRUE(!is.null(sigma) && is.null(sigma2))) { sigma2 <- sigma^2 }

  if (!isTRUE(paired)) {

    if (isTRUE(!is.null(sigma) && length(sigma) == 1L)) { sigma <- c(sigma, sigma) }

    if (isTRUE(!is.null(sigma2) && length(sigma2) == 1L)) { sigma2 <- c(sigma2, sigma2) }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Alternative hypothesis ####

  if (isTRUE(all(c("two.sided", "less", "greater") %in% alternative))) { alternative <- "two.sided" }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## No Grouping, No Split ####

  if (isTRUE(is.null(group) && is.null(split))) {

    #...................
    ### One-sample ####
    if (isTRUE(is.null(y))) {

      result <- data.frame(variable = "y",
                           n = length(na.omit(xy$x)),
                           nNA = length(attributes(na.omit(xy))$na.action),
                           m = mean(xy$x, na.rm = TRUE), sd = sd(xy$x, na.rm = TRUE),
                           mu = mu,
                           m.diff = mean(xy$x - mu, na.rm = TRUE),
                           low = m.diff.conf(x = mu, y = xy$x, sigma = sigma,
                                             var.equal = var.equal, alternative = alternative,
                                             paired = TRUE, conf.level = conf.level, side = "low"),
                           upp = m.diff.conf(x = mu, y = xy$x, sigma = sigma,
                                             var.equal = var.equal, alternative = alternative,
                                             paired = TRUE, conf.level = conf.level, side = "upp"),
                           stringsAsFactors = FALSE, row.names = NULL)


    #...................
    ### Two-samples ####
    } else if (isTRUE(!is.null(y) && !isTRUE(paired))) {

    result <- misty::df.rbind(data.frame(variable = "y",
                                         between = 1,
                                         n = length(na.omit(xy$x)),
                                         nNA = sum(is.na(xy$x)),
                                         m = mean(xy$x, na.rm = TRUE),
                                         sd = sd(xy$x, na.rm = TRUE),
                                         stringsAsFactors = FALSE),
                              data.frame(variable = "y",
                                         n = length(na.omit(xy$y)),
                                         between = 2,
                                         nNA = sum(is.na(xy$y)),
                                         m = mean(xy$y, na.rm = TRUE),
                                         sd = sd(xy$y, na.rm = TRUE),
                                         m.diff = mean(xy$y, na.rm = TRUE) - mean(xy$x, na.rm = TRUE),
                                         low = m.diff.conf(x = xy$x, y = xy$y, sigma = sigma,
                                                           var.equal = var.equal, alternative = alternative,
                                                           paired = FALSE, conf.level = conf.level, side = "low"),
                                         upp = m.diff.conf(x = xy$x, y = xy$y, sigma = sigma,
                                                           var.equal = var.equal, alternative = alternative,
                                                            paired = FALSE, conf.level = conf.level, side = "upp"),
                                         stringsAsFactors = FALSE, row.names = NULL))

    #...................
    ### Paired-samples ####
    } else if (isTRUE(!is.null(y) && isTRUE(paired))) {

      result <- data.frame(variable = "y",
                           n = nrow(na.omit(xy)),
                           nNA = length(attributes(na.omit(xy))$na.action),
                           m1 = mean(xy$x, na.rm = TRUE), sd1 = sd(xy$x, na.rm = TRUE),
                           m2 = mean(xy$y, na.rm = TRUE), sd2 = sd(xy$y, na.rm = TRUE),
                           m.diff = mean(xy$y - xy$x, na.rm = TRUE),
                           sd.diff = sd(xy$y - xy$x, na.rm = TRUE),
                           low = m.diff.conf(x = xy$x, y = xy$y, sigma = sigma,
                                             var.equal = var.equal, alternative = alternative,
                                             paired = TRUE, conf.level = conf.level, side = "low"),
                           upp = m.diff.conf(x = xy$x, y = xy$y, sigma = sigma,
                                             var.equal = var.equal, alternative = alternative,
                                             paired = TRUE, conf.level = conf.level, side = "upp"),
                           stringsAsFactors = FALSE, row.names = NULL)

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Grouping, No Split ####
  } else if (isTRUE(!is.null(group) && is.null(split))) {

    object.group <- lapply(split(xy, f = group),
                           function(y) ci.mean.diff.default(x = y$x, y = y$y, sigma = NULL, sigma2 = NULL,
                                                            var.equal = var.equal, alternative = alternative,
                                                            conf.level = conf.level, paired = paired,
                                                            group = NULL, split = NULL, sort.var = sort.var,
                                                            na.omit = na.omit, as.na = as.na, check = FALSE,
                                                            output = FALSE)$result)

    result <- data.frame(group = names(object.group),
                         eval(parse(text = paste0("rbind(", paste0("object.group[[", seq_len(length(object.group)), "]]",
                                                                   collapse = ", "), ")"))), stringsAsFactors = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##  No Grouping, Split ####
  } else if (isTRUE(is.null(group) && !is.null(split))) {

    result <- lapply(split(data.frame(xy, stringsAsFactors = FALSE), f = split),
                     function(y) ci.mean.diff.default(x = y$x, y = y$y, sigma = NULL, sigma2 = NULL,
                                                      var.equal = var.equal, alternative = alternative,
                                                      conf.level = conf.level, paired = paired,
                                                      group = NULL, split = NULL, sort.var = sort.var,
                                                      na.omit = na.omit, as.na = as.na, check = FALSE,
                                                      output = FALSE)$result)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##  Grouping, Split ####
  } else if (isTRUE(!is.null(group) && !is.null(split))) {

    result <- lapply(split(data.frame(xy, .group = group, stringsAsFactors = FALSE, row.names = NULL), f = split),
                     function(y) ci.mean.diff.default(x = y$x, y = y$y, sigma = NULL, sigma2 = NULL,
                                                      var.equal = var.equal, alternative = alternative,
                                                      conf.level = conf.level, paired = paired,
                                                      group = y$.group, split = NULL, sort.var = sort.var,
                                                      na.omit = na.omit, as.na = as.na, check = FALSE,
                                                      output = FALSE)$result)

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "ci", ci = ifelse(isTRUE(is.null(y)), "mean.diff.o", ifelse(!isTRUE(paired), "mean.diff.i", "mean.diff.p")),
                 data = list(x = x, y = y, group = group, split = split),
                 args = list(sigma = sigma, sigma2 = sigma2,
                             var.equal = var.equal, alternative = alternative,
                             conf.level = conf.level, paired = paired,
                             sort.var = sort.var, na.omit = na.omit, digits = digits,
                             as.na = as.na, check = check, output = output),
                 result = result)

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}

#_______________________________________________________________________________
#
# S3 method for class 'formula' ------------------------------------------------

ci.mean.diff.formula <- function(formula, data, sigma = NULL, sigma2 = NULL, var.equal = FALSE,
                                 alternative = c("two.sided", "less", "greater"),
                                 conf.level = 0.95, group = NULL, split = NULL,
                                 sort.var = FALSE, na.omit = FALSE, digits = 2, as.na = NULL,
                                 check = TRUE, output = TRUE, ...) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'formula' is missing
  if (isTRUE(missing(formula))) { stop("Please specify a formula using the argument 'formula'.", call. = FALSE) }

  # Check if input 'data' is missing
  if (isTRUE(missing(data))) { stop("Please specify a matrix or data frame for the argument 'x'.", call. = FALSE) }

  # Check if input 'data' is NULL
  if (isTRUE(is.null(data))) { stop("Input specified for the argument 'data' is NULL.", call. = FALSE) }

  # Check 'group'
  if (isTRUE(!is.null(group))) {

    if (ncol(data.frame(group)) != 1) { stop("More than one grouping variable specified for the argument 'group'.",call. = FALSE) }

    if (nrow(data.frame(group)) != nrow(data)) { stop("Length of the vector or factor specified in the argument 'group' does not match the number of rows in 'data'.", call. = FALSE) }

    # Convert 'group' into a vector
    group <- unlist(group, use.names = FALSE)

  }

  # Check 'split'
  if (isTRUE(!is.null(split))) {

    if (ncol(data.frame(split)) != 1L) { stop("More than one split variable specified for the argument 'split'.",call. = FALSE) }

    if (nrow(data.frame(split)) != nrow(data)) { stop("Length of the vector or factor specified in the argument 'split' does not match the number of rows in 'data'.", call. = FALSE)}

    # Convert 'split' into a vector
    split <- unlist(split, use.names = FALSE)

  }

  #_____________________________________________________________________________
  #
  # Data and Variables ---------------------------------------------------------

  # Dataframe
  data <- as.data.frame(data, stringsAsFactors = FALSE)

  # Variables
  var.formula <- all.vars(as.formula(formula))

  # Grouping variable
  group.var <- attr(terms(formula[-2L]), "term.labels")

  # Outcome(s)
  y.vars <- var.formula[-grep(group.var, var.formula)]

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check if variables are in the data
  var.data <- !var.formula %in% colnames(data)
  if (isTRUE(any(var.data))) {

    stop(paste0("Variables specified in the the formula were not found in 'data': ",
                paste(var.formula[which(var.data)], collapse = ", ")), call. = FALSE)

  }

  # Check if input 'formula' has only one grouping variable
  if (isTRUE(length(group.var) != 1L)) { stop("Please specify a formula with only one grouping variable.", call. = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) {

    #......
    # Check input 'as.na'

    # Factor or Vector
    if (isTRUE(is.atomic(data[, y.vars]) || is.factor(data[, y.vars]))) {

      as.na.x <- !as.na %in% data[, y.vars]

    # Matrix or data frame
    } else if (isTRUE(is.matrix(data[, y.vars]) || is.data.frame(data[, y.vars]))) {

      as.na.x <- vapply(as.character(as.na), function(y) !y %in% misty::chr.trim(apply(as.matrix(x), 2, as.character)),
                        FUN.VALUE = logical(1))

    # List
    } else if (isTRUE(is.list(data[, y.vars]))) {

      as.na.x <- !as.na %in% unlist(data[, y.vars])

    }

    if (isTRUE(any(as.na.x))) {

      warning(paste0("Values specified in the argument 'as.na' were not found in 'x': ",
                     paste(as.na[as.na.x], collapse = ", ")), call. = FALSE)
    }

    # Replace user-specified values with missing values
    data[, y.vars] <- misty::as.na(data[, y.vars], na = as.na, check = FALSE)

    # Variable with missing values only
    data.miss <- vapply(data[, y.vars, drop = FALSE], function(y) all(is.na(y)), FUN.VALUE = logical(1))
    if (isTRUE(any(data.miss))) {

      stop(paste0("After converting user-missing values into NA, following variables are completely missing: ",
                  paste(names(which(data.miss)), collapse = ", ")), call. = FALSE)

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Listwise deletion ####

  if (isTRUE(na.omit && any(is.na(data[, var.formula])))) {

    # No group and split variable
    if (isTRUE(is.null(group) && is.null(split))) {

      x <- na.omit(as.data.frame(data[, var.formula], stringsAsFactors = FALSE))

      warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ",
                     length(attributes(x)$na.action)), call. = FALSE)

    }

    # Group variable, no split variable
    if (isTRUE(!is.null(group) && is.null(split))) {

      data.group <- na.omit(data.frame(data[, var.formula], group = group, stringsAsFactors = FALSE))

      data <- data.group[, -grep("group", names(data.group)), drop = FALSE]
      group <- data.group$group

      warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ",
                     length(attributes(data.group)$na.action)), call. = FALSE)

    }

    # No group variable, split variable
    if (isTRUE(is.null(group) && !is.null(split))) {

      data.split <- na.omit(data.frame(data[, var.formula], split = split, stringsAsFactors = FALSE))

      data <- data.split[, -grep("split", names(data.split)), drop = FALSE]
      split <- data.split$split

      warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ",
                     length(attributes(data.split)$na.action)), call. = FALSE)

    }

    # Group variable, split variable
    if (isTRUE(!is.null(group) && !is.null(split))) {

      data.group.split <- na.omit(data.frame(data[, var.formula], group = group, split = split,
                                             stringsAsFactors = FALSE))

      data <- data.group.split[,  !names(data.group.split) %in% c("group", "split"), drop = FALSE]
      group <- data.group.split$group
      split <- data.group.split$split

      warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ",
                     length(attributes(data.group.split)$na.action)), call. = FALSE)

    }

    # Variable with missing values only
    data.miss <- vapply(data[, var.formula], function(y) all(is.na(y)), FUN.VALUE = logical(1))
    if (isTRUE(any(data.miss))) {

      stop(paste0("After listwise deletion, following variables are completely missing: ",
                  paste(names(which(data.miss)), collapse = ", ")), call. = FALSE)

    }

  }

  # Check if grouping variable has two levels
  if (isTRUE(length(na.omit(unique(unlist(data[, group.var])))) != 2L)) {

    stop("Please specify a grouping variable with exactly two levels.", call. = FALSE)

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Population standard deviation and variance ####

  if (isTRUE(is.null(sigma) && !is.null(sigma2))) { sigma <- sqrt(sigma2) }

  if (isTRUE(!is.null(sigma) && is.null(sigma2))) { sigma2 <- sigma^2 }

  if (isTRUE(!is.null(sigma) && length(sigma) == 1L)) { sigma <- c(sigma, sigma) }

  if (isTRUE(!is.null(sigma2) && length(sigma2) == 1L)) { sigma2 <- c(sigma2, sigma2) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Alternative hypothesis ####

  if (isTRUE(all(c("two.sided", "less", "greater") %in% alternative))) { alternative <- "two.sided" }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## No Grouping, No Split ####
  if (isTRUE(is.null(group) && is.null(split))) {

    result <- data.frame(matrix(NA, ncol = 9L, nrow = length(y.vars)*2,
                                dimnames = list(NULL, c("variable", "between", "n", "nNA", "m", "sd", "m.diff", "low", "upp"))),
                         stringsAsFactors = FALSE)

    loop.mat <- matrix(1:(length(y.vars)*2L), ncol = 2L, byrow = TRUE)

    for (i in seq_along(y.vars)) {

      data.split <- split(unlist(data[, y.vars[i]]), f = unlist(data[, group.var]))

      result[loop.mat[i, ], ] <- data.frame(variable = y.vars[i],
                                            ci.mean.diff.default(x = data.split[[1L]], y = data.split[[2L]],
                                                                 sigma = sigma, sigma2 = sigma2, var.equal = var.equal,
                                                                 paired = FALSE, alternative = alternative,
                                                                 conf.level = conf.level, group = NULL, split = NULL, sort.var = sort.var,
                                                                 digits = digits, as.na = NULL, check = check, output = FALSE)$result[, -1L],
                                            stringsAsFactors = FALSE)

      result[loop.mat[i, ], "between"] <- names(data.split)

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Grouping, No Split ####
  } else if (isTRUE(!is.null(group) && is.null(split))) {

    object.group <- lapply(split(data[, var.formula], f = group),
                           function(y) misty::ci.mean.diff(formula, data = y, sigma = NULL, sigma2 = NULL,
                                                           var.equal = var.equal, alternative = alternative,
                                                           conf.level = conf.level, group = NULL, split = NULL,
                                                           sort.var = sort.var, na.omit = na.omit,
                                                           as.na = as.na, check = FALSE, output = FALSE)$result)

    result <- data.frame(group = rep(names(object.group), each = length(y.vars)*2),
                         eval(parse(text = paste0("rbind(", paste0("object.group[[", seq_len(length(object.group)), "]]",
                                                                   collapse = ", "), ")"))), stringsAsFactors = FALSE)


  } else if (isTRUE(is.null(group) && !is.null(split))) {

    result <- lapply(split(data.frame(data[, var.formula], stringsAsFactors = FALSE), f = split),
                     function(y) misty::ci.mean.diff(formula, data = y, sigma = NULL, sigma2 = NULL, var.equal = var.equal,
                                                     alternative = alternative, conf.level = conf.level,
                                                     group = NULL, split = NULL, sort.var = sort.var, na.omit = na.omit,
                                                     as.na = as.na, check = FALSE, output = FALSE)$result)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Grouping, Split ####
  } else if (isTRUE(!is.null(group) && !is.null(split))) {

    result <- lapply(split(data.frame(data[, var.formula], .group = group, stringsAsFactors = FALSE), f = split),
                     function(y) misty::ci.mean.diff(formula, data = y, sigma = NULL, sigma2 = NULL,
                                                     var.equal = var.equal, alternative = alternative, conf.level = conf.level,
                                                     group = y$.group, split = NULL, sort.var = sort.var, na.omit = na.omit,
                                                     as.na = as.na, check = FALSE, output = FALSE)$result)

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "ci", ci = "mean.diff.i",
                 data = list(data = data[, var.formula], group = group, split = split),
                 args = list(formula = formula, sigma = sigma, sigma2 = sigma2,
                             var.equal = var.equal, alternative = alternative,
                             conf.level = conf.level, sort.var = sort.var,
                             na.omit = na.omit, digits = digits, as.na = as.na,
                             check = check, output = output),
                 result = result)

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
