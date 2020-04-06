#' Confidence Interval for the Difference in Arithmetic Means
#'
#' This function computes a confidence interval for the difference in arithmetic means from independent and
#' paired samples with known or unknown population standard deviation or population variance for one or more
#' variables, optionally by a grouping and/or split variable.
#'
#' @param x              a numeric vector of data values.
#' @param y              a numeric vector of data values.
#' @param sigma          a numeric vector indicating the population standard deviation(s) when computing confidence
#'                       intervals for the difference in arithmetic means with known standard deviation(s). In case
#'                       of independent samples, equal standard deviation is assumed when specifying one value for
#'                       the argument \code{sigma}; when specifying two values for the argument \code{sigma}, unequal
#'                       variance is assumed Note that either argument \code{sigma} or argument \code{sigma2} is
#'                       specified and it is only possible to specify one value (i.e., equal variance assumption) or
#'                       two values (i.e., unequal variance assumption) for the argument \code{sigma} even though
#'                       multiple variables are specified in \code{x}.
#' @param sigma2         a numeric vector indiating the popualation variance(s) when computing confidence intervals
#'                       for the difference in arithmetic means with known variance(s). In case of independent samples,
#'                       equal variance is assumed when specifying one value for the argument \code{sigma2}; when
#'                       specifying two values for the argument \code{sigma}, unequal variance is aussumed. Note that
#'                       either argument \code{sigma} or argument \code{sigma2} is specified and it is only possible
#'                       to specify one value (i.e., equal variance assumption) or two values (i.e., unequal variance
#'                       assumption) for the argument \code{sigma} even though multiple variables are specified in
#'                       \code{x}.
#' @param var.equal      logical: if \code{TRUE}, the population variance in the independent samples are assumed to
#'                       be equal.
#' @param paired         logical: if \code{TRUE}, confidence interval for the difference of arithmetic means
#'                       in paired samples is computed.
#' @param alternative    a character string specifying the alternative hypothesis, must be one of
#'                       \code{"two.sided"} (default), \code{"greater"} or \code{"less"}.
#' @param conf.level     a numeric value between 0 and 1 indicating the confidence level of the interval.
#' @param group          a numeric vector, character vector or factor as grouping variable. Note that a grouping
#'                       variable can only be used when computing confidence intervals with unknown population
#'                       standard deviation and population variance.
#' @param split          a numeric vector, character vector or factor as split variable. Note that a split
#'                       variable can only be used when computing confidence intervals with unknown population
#'                       standard deviation and population variance.
#' @param sort.var       logical: if \code{TRUE}, output table is sorted by variables when specifying \code{group}.
#' @param digits         an integer value indicating the number of decimal places to be used.
#' @param as.na          a numeric vector indicating user-defined missing values,
#'                       i.e. these values are converted to \code{NA} before conducting the analysis.
#'                       Note that \code{as.na()} function is only applied to \code{x}, but
#'                       not to \code{group} or \code{split}.
#' @param check          logical: if \code{TRUE}, argument specification is checked.
#' @param output         logical: if \code{TRUE}, output is shown on the console.
#' @param formula        in case of a between-subject design (i.e., \code{paired = FALSE}), a formula of the
#'                       form \code{y ~ group} for one outcome variable or \code{cbind(y1, y2, y3) ~ group}
#'                       for more than one outcome variable where \code{y} is a numeric variable giving the
#'                       data values and \code{group} a numeric variable, character variable or factor with
#'                       two values or factor levels giving the corresponding groups; in case of a within-subjetc
#'                       design (i.e., \code{paired = TRUE}), a formula of the form \code{post ~ pre} where
#'                       \code{post} and \code{pre} are numeric variables. Note that analysis for more than
#'                       one outcome variable is not permitted in within-subject design.
#' @param data           a matrix or data frame containing the variables in the formula \code{formula}.
#' @param na.omit        logical: if \code{TRUE}, incomplete cases are removed before conducting the analysis
#'                       (i.e., listwise deletion) when specifying more than one outcome variable.
#' @param ...            further arguments to be passed to or from methods.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{ci.mean}}, \code{\link{ci.median}}, \code{\link{ci.prop}}, \code{\link{ci.var}}, \code{\link{ci.sd}},
#' \code{\link{descript}}
#'
#' @references
#' Fagerland, M. W., Lydersen S., & Laake, P. (2011). Recommended confidence intervals for two independent binomial
#' proportions. \emph{Statistical Methods in Medical Research, 24}, 224-254.
#'
#' Newcombe, R. G. (1998a). Interval estimation for the difference between independent proportions: Comparison of
#' eleven methods. \emph{Statistics in Medicine, 17}, 873-890.
#'
#' Newcombe, R. G. (1998b). Improved confidence intervals for the difference between binomial proportions based on
#' paired data. \emph{Statistics in Medicine, 17}, 2635-2650.
#'
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology - Using R and SPSS}.
#' John Wiley & Sons.
#'
#' @return
#' Returns an object of class \code{ci}, which is a list with following entries:
#' function call (\code{call}), type of confidence interval (\code{type}), list with the input specified in \code{x},
#' \code{group}, and \code{split} (\code{data}), specification of function arguments (\code{args}),
#' and result table (\code{result}).
#'
#' @export
#'
#' @examples
#' dat.bs <- data.frame(group1 = c(1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2,
#'                                 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2),
#'                      group2 = c(1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 2, 2, 2,
#'                                 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2),
#'                      group3 = c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
#'                                 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2),
#'                      x1 = c(3, 1, 4, 2, 5, 3, 2, 3, 6, 4, 3, NA, 5, 3,
#'                             3, 2, 6, 3, 1, 4, 3, 5, 6, 7, 4, 3, 6, 4),
#'                      x2 = c(4, NA, 3, 6, 3, 7, 2, 7, 3, 3, 3, 1, 3, 6,
#'                             3, 5, 2, 6, 8, 3, 4, 5, 2, 1, 3, 1, 2, NA),
#'                      x3 = c(7, 8, 5, 6, 4, 2, 8, 3, 6, 1, 2, 5, 8, 6,
#'                             2, 5, 3, 1, 6, 4, 5, 5, 3, 6, 3, 2, 2, 4),
#'                      stringsAsFactors = FALSE)
#'
#' #--------------------------------------
#' # Between-Subject Design
#'
#' # Two-Sided 95% Confidence Interval for y1 by group1
#' # unknown population variances, unequal variance assumption
#' ci.mean.diff(x1 ~ group1, data = dat.bs)
#'
#' # Two-Sided 95% Confidence Interval for y1 by group1
#' # unknown population variances, equal variance assumption
#' ci.mean.diff(x1 ~ group1, data = dat.bs, var.equal = TRUE)
#'
#' # Two-Sided 95% Confidence Interval with known standard deviations for x1 by group1
#' # known population standard deviations, equal standard deviation asssumption
#' ci.mean.diff(x1 ~ group1, data = dat.bs, sigma = 1.2)
#'
#' # Two-Sided 95% Confidence Interval with known standard deviations for x1 by group1
#' # known population standard deviations, unequal standard deviation asssumption
#' ci.mean.diff(x1 ~ group1, data = dat.bs, sigma = c(1.5, 1.2))
#'
#' # Two-Sided 95% Confidence Interval with known variance for x1 by group1
#' # known population variances, equal variance asssumption
#' ci.mean.diff(x1 ~ group1, data = dat.bs, sigma2 = 1.44)
#'
#' # Two-Sided 95% Confidence Interval with known variance for x1 by group1
#' # known population variances, unequal variance asssumption
#' ci.mean.diff(x1 ~ group1, data = dat.bs, sigma2 = c(2.25, 1.44))
#'
#' # One-Sided 95% Confidence Interval for y1 by group1
#' # unknown population variances, unequal variance assumption
#' ci.mean.diff(x1 ~ group1, data = dat.bs, alternative = "less")
#'
#' # Two-Sided 95% Confidence Interval for y1 by group1
#' # unknown population variances, unequal variance assumption
#' ci.mean.diff(x1 ~ group1, data = dat.bs, conf.level = 0.99)
#'
#' # Two-Sided 95% Confidence Interval for y1 by group1
#' # unknown population variances, unequal variance assumption
#' # print results with 3 digits
#' ci.mean.diff(x1 ~ group1, data = dat.bs, digits = 3)
#'
#' # Two-Sided 95% Confidence Interval for y1 by group1
#' # unknown population variances, unequal variance assumption
#' # convert value 4 to NA
#' ci.mean.diff(x1 ~ group1, data = dat.bs, as.na = 4)
#'
#' # Two-Sided 95% Confidence Interval for y1, y2, and y3 by group1
#' # unknown population variances, unequal variance assumption
#' ci.mean.diff(cbind(x1, x2, x3) ~ group1, data = dat.bs)
#'
#' # Two-Sided 95% Confidence Interval for y1, y2, and y3 by group1
#' # unknown population variances, unequal variance assumption,
#' # listwise deletion for missing data
#' ci.mean.diff(cbind(x1, x2, x3) ~ group1, data = dat.bs, na.omit = TRUE)
#'
#' # Two-Sided 95% Confidence Interval for y1, y2, and y3 by group1
#' # unknown population variances, unequal variance assumption,
#' # analysis by group2 separately
#' ci.mean.diff(cbind(x1, x2, x3) ~ group1, data = dat.bs, group = dat.bs$group2)
#'
#' # Two-Sided 95% Confidence Interval for y1, y2, and y3 by group1
#' # unknown population variances, unequal variance assumption,
#' # analysis by group2 separately, sort by variables
#' ci.mean.diff(cbind(x1, x2, x3) ~ group1, data = dat.bs, group = dat.bs$group2,
#'              sort.var = TRUE)
#'
#' # Two-Sided 95% Confidence Interval for y1, y2, and y3 by group1
#' # unknown population variances, unequal variance assumption,
#' # split analysis by group2
#' ci.mean.diff(cbind(x1, x2, x3) ~ group1, data = dat.bs, split = dat.bs$group2)
#'
#' # Two-Sided 95% Confidence Interval for y1, y2, and y3 by group1
#' # unknown population variances, unequal variance assumption,
#' # analysis by group2 separately, split analysis by group3
#' ci.mean.diff(cbind(x1, x2, x3) ~ group1, data = dat.bs,
#'              group = dat.bs$group2, split = dat.bs$group3)
#'
#' #-----------------
#'
#' group1 <- c(3, 1, 4, 2, 5, 3, 6, 7)
#' group2 <- c(5, 2, 4, 3, 1)
#'
#' # Two-Sided 95% Confidence Interval for the mean difference between group1 amd group2
#' # unknown population variances, unequal variance assumption
#' ci.mean.diff(group1, group2)
#'
#' # Two-Sided 95% Confidence Interval for the mean difference between group1 amd group2
#' # unknown population variances, equal variance assumption
#' ci.mean.diff(group1, group2, var.equal = TRUE)
#'
#' #--------------------------------------
#' # Within-Subject Design
#' dat.ws <- data.frame(pre = c(1, 3, 2, 5, 7),
#'                      post = c(2, 2, 1, 6, 8), stringsAsFactors = FALSE)
#'
#' # Two-Sided 95% Confidence Interval for the mean difference in x1 and x2
#' # unknown poulation variance of difference scores
#' ci.mean.diff(dat.ws$pre, dat.ws$post, paired = TRUE)
#'
#' # Two-Sided 95% Confidence Interval for the mean difference in x1 and x2
#' # known population standard deviation of difference scores
#' ci.mean.diff(dat.ws$pre, dat.ws$post, sigma = 2, paired = TRUE)
#'
#' # Two-Sided 95% Confidence Interval for the mean difference in x1 and x2
#' # known population variance of difference scores
#' ci.mean.diff(dat.ws$pre, dat.ws$post, sigma2 = 4, paired = TRUE)
#'
#' # One-Sided 95% Confidence Interval for the mean difference in x1 and x2
#' # unknown population variances, unequal variance assumption
#' ci.mean.diff(dat.ws$pre, dat.ws$post, alternative = "less", paired = TRUE)
#'
#' # Two-Sided 95% Confidence Interval for the mean difference in x1 and x2
#' # unknown population variances, unequal variance assumption
#' ci.mean.diff(dat.ws$pre, dat.ws$post, conf.level = 0.99, paired = TRUE)
#'
#' # Two-Sided 95% Confidence Interval for for the mean difference in x1 and x2
#' # unknown population variances, unequal variance assumption
#' # print results with 3 digits
#' ci.mean.diff(dat.ws$pre, dat.ws$post, paired = TRUE, digits = 3)
#'
#' # Two-Sided 95% Confidence Interval for y1 by group1
#' # unknown population variances, unequal variance assumption
#' # convert value 1 to NA
#' ci.mean.diff(dat.ws$pre, dat.ws$post, as.na = 1, paired = TRUE)
ci.mean.diff <- function(x, ...) {

  UseMethod("ci.mean.diff")

}

####################################################################################
# Confidence interval for the difference of arithmetic means

m.diff.conf <- function(x, y, sigma, var.equal, alternative,
                        paired, conf.level, side) {

  #-----------------------------------------
  # Independent samples
  if (isFALSE(paired)) {

    #.................
    # Data
    x <- na.omit(x)
    y <- na.omit(y)

    x.n <- length(x)
    y.n <- length(y)

    yx.mean <- mean(y) - mean(x)

    x.var <- var(x)
    y.var <- var(y)

    #......
    # At least 2 observations for x and y
    if (x.n >= 2L && y.n >= 2L & (x.var != 0L && y.var != 0L)) {

      #.................
      # Known Population SD
      if (!is.null(sigma)) {

        se <- sqrt((sigma[1L]^2L / x.n) + (sigma[2L]^2L / y.n))

        crit <- qnorm(switch(alternative,
                             two.sided = 1L - (1L - conf.level) / 2L,
                             less = conf.level,
                             greater = conf.level))

        term <- crit*se

      #.................
      # Unknown Population SD
      } else {

        # Equal variance
        if (isTRUE(var.equal)) {

         se <- sqrt(((x.n - 1L)*x.var + (y.n - 1L)*y.var) / (x.n + y.n - 2L)) * sqrt(1 / x.n + 1L / y.n)

         crit <- qt(switch(alternative,
                           two.sided = 1L - (1L - conf.level) / 2L,
                           less = conf.level,
                           greater = conf.level), df = sum(x.n, y.n) - 2L)

         term <- crit*se

        # Unequal variance
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

      #......
      # Confidence interval
      ci <- switch(alternative,
                   two.sided = c(low = yx.mean - term, upp = yx.mean + term),
                   less = c(low = -Inf, upp = yx.mean + term),
                   greater = c(low = yx.mean - term, upp = Inf))

    #......
    # Less than  2 observations for x and y
    } else {

      ci <- c(NA, NA)

    }

  #-----------------------------------------
  # Dependent samples
  } else {

    xy.dat <- na.omit(data.frame(x = x, y = y, stringsAsFactors = FALSE))

    xy.diff <- xy.dat$y - xy.dat$x

    xy.diff.mean <- mean(xy.diff)

    xy.diff.sd <- sd(xy.diff)

    xy.diff.n <- nrow(xy.dat)

    #......
    # At least 2 observations for x
    if (xy.diff.n >= 2L && xy.diff.sd != 0L) {

      #.................
      # Known Population SD
      if (!is.null(sigma)) {

        se <- sigma / sqrt(xy.diff.n)

        crit <- qnorm(switch(alternative,
                             two.sided = 1L - (1L - conf.level) / 2L,
                             less = conf.level,
                             greater = conf.level))

        term <- crit*se

      #.................
      # Unknown Population SD
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

    #......
    # Less than 2 observations for x
    } else {

      ci <- c(NA, NA)

    }

  }

  #......
  # Return object
  object <- switch(side, both = ci, low = ci[1L], upp = ci[2L])

  return(object)

}

####################################################################################
# Default S3 method

ci.mean.diff.default <- function(x, y, sigma = NULL, sigma2 = NULL, var.equal = FALSE,
                                 paired = FALSE, alternative = c("two.sided", "less", "greater"),
                                 conf.level = 0.95, group = NULL, split = NULL, sort.var = FALSE,
                                 digits = 2, as.na = NULL, check = TRUE, output = TRUE, ...) {

  #......
  # Check if input 'x' is missing
  if (missing(x) || is.null(x)) {

    stop("Please specify a numeric vector for the argument 'x'", call. = FALSE)

  }

  #......
  # Check if input 'y' is missing
  if (missing(y) || is.null(x)) {

    stop("Please specify a numeric vector for the argument 'y'", call. = FALSE)

  }

  #......
  # Check input 'paired'
  if (isFALSE(isTRUE(paired) || isFALSE(paired))) {

    stop("Please specify TRUE or FALSE for the argument 'paired'.", call. = FALSE)

  }


  #----------------------------------------
  # List or Dataframe

  #......
  # Independent samples
  if (isFALSE(paired)) {

    xy <- list(x = x, y = y)

  #......
  # Paired samples
  } else {

    # Length of 'x' and 'y'
    if (length(x) != length(y)) {

      stop("Length of the vector specified in 'x' does not match the length of the vector specified in 'y'.",
           call. = FALSE)

    }

    xy <- data.frame(x = x, y = y, stringsAsFactors = FALSE)

  }

  #----------------------------------------
  # Convert user-missing values into NA

  if (!is.null(as.na)) {

    # Replace user-specified values with missing values
    xy <- misty::as.na(xy, as.na = as.na, check = check)

    # Variable with missing values only
    xy.miss <- vapply(xy, function(y) all(is.na(y)), FUN.VALUE = logical(1))
    if (any(xy.miss)) {

      stop(paste0("After converting user-missing values into NA, following variables are completely missing: ",
                  paste(names(which(xy.miss)), collapse = ", ")), call. = FALSE)

    }

  }

  #----------------------------------------
  # Listwise deletion

  if (isTRUE(paired)) {

    if (nrow(na.omit(xy)) < 2) {

      stop("After listwise deletion, there is only one or no pair of observations left for the analysis.",
           call. = FALSE)

    }

  }

  #-----------------------------------------------------------------------------------
  # Input Check

  #......
  # Check input 'check'
  if (isFALSE(isTRUE(check) || isFALSE(check))) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }

  if (isTRUE(check)) {

    #......
    # Check input 'sigma' and 'sigma2'
    if (!is.null(sigma) && !is.null(sigma2)) {

      if (!identical(sigma^2, sigma2)) {

        stop("Arguments 'sigma' and 'sigma2' do not match.", call. = FALSE)

      }

    }

    #......
    # Check input 'sigma'
    if (!is.null(sigma)) {

      # SD smaller or equal 0
      if (any(sigma <= 0L)) {

        stop("Please specify numeric values grater than 0 for the argument 'sigma'.", call. = FALSE)

      }

      if (isFALSE(paired)) {

        # Length of 'sigma'
        if (length(sigma) > 2L) {

          stop("Please specify one or two numeric values for the argument 'sigma' in independent samples.", call. = FALSE)

        }

      } else {

        # Length of 'sigma'
        if (length(sigma) > 1L) {

          stop("Please specify one numeric values for the argument 'sigma' in dependent samples.", call. = FALSE)

        }

      }

    }

    #......
    # Check input 'sigma2'
    if (!is.null(sigma2)) {

      # Variance smaller or equal 0
      if (any(sigma2 <= 0L)) {

        stop("Please specify numeric values grater than 0 for the argument 'sigma2'.", call. = FALSE)

      }

      if (isFALSE(paired)) {

        # Length of 'sigma2'
        if (length(sigma2) > 2L) {

          stop("Please specify one or two numeric values for the argument 'sigma2' in independent samples.", call. = FALSE)

        }

      } else {

        # Length of 'sigma2'
        if (length(sigma2) > 1L) {

          stop("Please specify one numeric values for the argument 'sigma2' in dependent samples.", call. = FALSE)

        }

      }

    }

    #......
    # Check input 'paired'
    if (isFALSE(isTRUE(var.equal) || isFALSE(var.equal))) {

      stop("Please specify TRUE or FALSE for the argument 'var.equal'.", call. = FALSE)

    }

    #......
    # Check input 'alternative'
    if (!all(alternative %in%  c("two.sided", "less", "greater"))) {

      stop("Character string in the argument 'alternative' does not match with \"two.sided\", \"less\", or \"greater\".",
           call. = FALSE)

    }

    #......
    # Check input 'conf.level'
    if (conf.level >= 1L || conf.level <= 0L) {

      stop("Please specifiy a numeric value between 0 and 1 for the argument 'conf.level'.",
           call. = FALSE)

    }

    #......
    # Check input 'group'
    if (!is.null(group)) {

      # Independent samples
      if (isFALSE(paired)) {

        stop("Please use formula notation for using a grouping variable in independent samples.",
             call. = FALSE)

      }

      # Population standard deviation
      if (!is.null(sigma)) {

        stop("Grouping variable cannot be used for confidence intervals with known population standard deviation.",
             call. = FALSE)

      }

      # Population variance
      if (!is.null(sigma2)) {

        stop("Grouping variable cannot be used for confidence intervals with known population variance.",
             call. = FALSE)

      }

      # Vector or factor for the argument 'group'?
      if (!is.vector(group) && !is.factor(group)) {

        stop("Please specify a vector or factor for the argument 'group'.", call. = FALSE)

      }

      # Length of 'group' match with 'x'?
      if (length(group) != nrow(xy)) {

        stop("Length of the vector or factor specified in 'group' does not match the number of rows of the matrix or data frame in 'data'.",
             call. = FALSE)

      }

      # Input 'group' completely missing
      if (all(is.na(group))) {

        stop("The grouping variable specified in 'group' is completely missing.", call. = FALSE)

      }

      # Only one group in 'group'
      if (length(na.omit(unique(group))) == 1L) {

        warning("There is only one group represented in the grouping variable specified in 'group'.", call. = FALSE)

      }

    }

    #......
    # Check input 'split'
    if (!is.null(split)) {

      # Independent samples
      if (isFALSE(paired)) {

        stop("Please use formula notation for using a split variable in independent samples.",
             call. = FALSE)

      }

      # Population standard deviation
      if (!is.null(sigma)) {

        stop("Split variable cannot be used for confidence intervals with known population standard deviation.",
             call. = FALSE)

      }

      # Population variance
      if (!is.null(sigma2)) {

        stop("Split variable cannot be used for confidence intervals with known population variance.",
             call. = FALSE)

      }

      # Vector or factor for the argument 'split'?
      if (!is.atomic(split) && !is.factor(split)) {

        stop("Please specify a vector or factor for the argument 'split'.", call. = FALSE)

      }

      # Length of 'split' doest not match with 'x'
      if (length(split) != nrow(xy)) {

        stop("Length of the vector or factor specified in 'split' does not match the number of rows in 'data'.",
               call. = FALSE)

      }

      # Input 'split' completely missing
      if (all(is.na(split))) {

        stop("The split variable specified in 'split' is completely missing.", call. = FALSE)

      }

      # Only one group in 'split'
      if (length(na.omit(unique(split))) == 1L) {

        warning("There is only one group represented in the split variable specified in 'split'.", call. = FALSE)

      }

    }

    #......
    # Check input 'sort.var'
    if (isFALSE(isTRUE(sort.var) || isFALSE(sort.var))) {

      stop("Please specify TRUE or FALSE for the argument 'sort.var'.", call. = FALSE)

    }

    #......
    # Check input 'digits'
    if (digits %% 1L != 0L || digits < 0L) {

      stop("Specify a positive integer number for the argument 'digits'.", call. = FALSE)

    }

    #......
    # Check input output
    if (isFALSE(isTRUE(output) || isFALSE(output))) {

      stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE)

    }

  }

  ####################################################################################
  # Arguments

  #----------------------------------------
  # Population standard deviation and variance

  if (is.null(sigma) && !is.null(sigma2)) { sigma <- sqrt(sigma2) }

  if (!is.null(sigma) && is.null(sigma2)) { sigma2 <- sigma^2 }

  if (isFALSE(paired)) {

    if (!is.null(sigma) && length(sigma) == 1L) { sigma <- c(sigma, sigma) }

    if (!is.null(sigma2) && length(sigma2) == 1L) { sigma2 <- c(sigma2, sigma2) }

  }

  #----------------------------------------
  # Alternative hypothesis

  if (all(c("two.sided", "less", "greater") %in% alternative)) { alternative <- "two.sided" }

  #-----------------------------------------------------------------------------------
  # Main Function

  #----------------------------------------
  # No Grouping, No Split
  if (is.null(group) && is.null(split)) {

    # Independent sample
    if (isFALSE(paired)) {

    result <- data.frame(variable = "y",
                         n1 = length(na.omit(xy$x)), nNA1 = sum(is.na(xy$x)), m1 = mean(xy$x, na.rm = TRUE), sd1 = sd(xy$x, na.rm = TRUE),
                         n2 = length(na.omit(xy$y)), nNA2 = sum(is.na(xy$y)), m2 = mean(xy$y, na.rm = TRUE), sd2 = sd(xy$y, na.rm = TRUE),
                         m.diff = mean(xy$y, na.rm = TRUE) - mean(xy$x, na.rm = TRUE),
                         low = m.diff.conf(x = xy$x, y = xy$y, sigma = sigma, var.equal = var.equal, alternative = alternative,
                                           paired = FALSE, conf.level = conf.level, side = "low"),
                         upp = m.diff.conf(x = xy$x, y = xy$y, sigma = sigma, var.equal = var.equal, alternative = alternative,
                                           paired = FALSE, conf.level = conf.level, side = "upp"),
                         stringsAsFactors = FALSE, row.names = NULL)

    # Dependent sample
    } else {

      result <- data.frame(variable = "y",
                           n = nrow(na.omit(xy)), nNA1 = sum(is.na(xy$x)), m1 = mean(xy$x, na.rm = TRUE), sd1 = sd(xy$x, na.rm = TRUE),
                                                  nNA2 = sum(is.na(xy$y)), m2 = mean(xy$y, na.rm = TRUE), sd2 = sd(xy$y, na.rm = TRUE),
                           m.diff = mean(xy$y - xy$x, na.rm = TRUE),
                           sd.diff = sd(xy$y - xy$x, na.rm = TRUE),
                           low = m.diff.conf(x = xy$x, y = xy$y, sigma = sigma, var.equal = var.equal, alternative = alternative,
                                             paired = TRUE, conf.level = conf.level, side = "low"),
                           upp = m.diff.conf(x = xy$x, y = xy$y, sigma = sigma, var.equal = var.equal, alternative = alternative,
                                             paired = TRUE, conf.level = conf.level, side = "upp"),
                           stringsAsFactors = FALSE, row.names = NULL)

    }

  #----------------------------------------
  # Grouping, No Split
  } else if (!is.null(group) && is.null(split)) {

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

  #----------------------------------------
  # No Grouping, Split
  } else if (is.null(group) && !is.null(split)) {

    result <- lapply(split(data.frame(xy, stringsAsFactors = FALSE), f = split),
                     function(y) ci.mean.diff.default(x = y$x, y = y$y, sigma = NULL, sigma2 = NULL,
                                                      var.equal = var.equal, alternative = alternative,
                                                      conf.level = conf.level, paired = paired,
                                                      group = NULL, split = NULL, sort.var = sort.var,
                                                      na.omit = na.omit, as.na = as.na, check = FALSE,
                                                      output = FALSE)$result)

  #----------------------------------------
  # Grouping, Split
  } else if (!is.null(group) && !is.null(split)) {

    result <- lapply(split(data.frame(xy, .group = group, stringsAsFactors = FALSE, row.names = NULL), f = split),
                     function(y) ci.mean.diff.default(x = y$x, y = y$y, sigma = NULL, sigma2 = NULL,
                                                      var.equal = var.equal, alternative = alternative,
                                                      conf.level = conf.level, paired = paired,
                                                      group = y$.group, split = NULL, sort.var = sort.var,
                                                      na.omit = na.omit, as.na = as.na, check = FALSE,
                                                      output = FALSE)$result)

  }

  ####################################################################################
  # Return object and output

  object <- list(call = match.call(),
                 type = ifelse(isFALSE(paired), "mean.diff.i",  "mean.diff.p"),
                 data = list(x = x, y = y, group = group, split = split),
                 args = list(sigma = sigma, sigma2 = sigma2,
                             var.equal = var.equal, alternative = alternative,
                             conf.level = conf.level, paired = paired,
                             sort.var = sort.var, na.omit = na.omit, digits = digits,
                             as.na = as.na, check = check, output = output),
                 result = result)

  class(object) <- "ci"

  #-----------------------------------------------------------------------------------
  # Output

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}

####################################################################################
# S3 method for class 'formula'

ci.mean.diff.formula <- function(formula, data, sigma = NULL, sigma2 = NULL, var.equal = FALSE,
                                 alternative = c("two.sided", "less", "greater"),
                                 conf.level = 0.95, group = NULL, split = NULL,
                                 sort.var = FALSE, na.omit = FALSE, digits = 2, as.na = NULL,
                                 check = TRUE, output = TRUE, ...) {

  #......
  # Check if input 'formula' is missing
  if (missing(formula)) {

    stop("Please specify a formula using the argument 'formula'", call. = FALSE)

  }

  #......
  # Check if input 'data' is missing
  if (missing(data)) {

    stop("Please specify a matrix or data frame for the argument 'x'.", call. = FALSE)

  }


  #-----------------------------------------------------------------------------------
  # Formula

  #.........................................
  # Variables

  var.formula <- all.vars(as.formula(formula))

  # Grouping variable
  group.var <- attr(terms(formula[-2L]), "term.labels")

  # Outcome(s)
  y.vars <- var.formula[-grep(group.var, var.formula)]

  #.........................................
  # Check

  # Check if variables are in the data
  var.data <- !var.formula %in% colnames(data)
  if (any(var.data)) {

    stop(paste0("Variables specified in the the formula were not found in 'data': ",
                paste(var.formula[which(var.data)], collapse = ", ")), call. = FALSE)

  }

  # Check if input 'formula' has only one grouping variable
  if (length(group.var) != 1L) { stop("Please specify a formula with only one grouping variable.", call. = FALSE) }

  #----------------------------------------
  # Convert user-missing values into NA

  if (!is.null(as.na)) {

    # Replace user-specified values with missing values
    data[, y.vars] <- misty::as.na(data[, y.vars], as.na = as.na, check = check)

    # Variable with missing values only
    data.miss <- vapply(data[, y.vars, drop = FALSE], function(y) all(is.na(y)), FUN.VALUE = logical(1))
    if (any(data.miss)) {

      stop(paste0("After converting user-missing values into NA, following variables are completely missing: ",
                  paste(names(which(data.miss)), collapse = ", ")), call. = FALSE)

    }

  }

  #----------------------------------------
  # Listwise deletion

  if (isTRUE(na.omit) && any(is.na(data[, var.formula]))) {

    #......
    # No group and split variable
    if (is.null(group) && is.null(split)) {

      x <- na.omit(as.data.frame(data[, var.formula], stringsAsFactors = FALSE))

      warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ",
                     length(attributes(x)$na.action)), call. = FALSE)

    }

    #......
    # Group variable, no split variable
    if (!is.null(group) && is.null(split)) {

      data.group <- na.omit(data.frame(data[, var.formula], group = group, stringsAsFactors = FALSE))

      data <- data.group[, -grep("group", names(data.group)), drop = FALSE]
      group <- data.group$group

      warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ",
                     length(attributes(data.group)$na.action)), call. = FALSE)

    }

    #......
    # No group variable, split variable
    if (is.null(group) && !is.null(split)) {

      data.split <- na.omit(data.frame(data[, var.formula], split = split, stringsAsFactors = FALSE))

      data <- data.split[, -grep("split", names(data.split)), drop = FALSE]
      split <- data.split$split

      warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ",
                     length(attributes(data.split)$na.action)), call. = FALSE)

    }

    #......
    # Group variable, split variable
    if (!is.null(group) && !is.null(split)) {

      data.group.split <- na.omit(data.frame(data[, var.formula], group = group, split = split,
                                             stringsAsFactors = FALSE))

      data <- data.group.split[,  !names(data.group.split) %in% c("group", "split"), drop = FALSE]
      group <- data.group.split$group
      split <- data.group.split$split

      warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ",
                     length(attributes(data.group.split)$na.action)), call. = FALSE)

    }

    #......
    # Variable with missing values only
    data.miss <- vapply(data[, var.formula], function(y) all(is.na(y)), FUN.VALUE = logical(1))
    if (any(data.miss)) {

      stop(paste0("After listwise deletion, following variables are completely missing: ",
                  paste(names(which(data.miss)), collapse = ", ")), call. = FALSE)

    }

  }

  #.........................................
  # Check

  # Check if grouping varibale has two levels
  if (length(na.omit(unique(data[, group.var]))) != 2L) {

    stop("Please specify a grouping variable with exactly two levels.", call. = FALSE)

  }

  ####################################################################################
  # Arguments

  #----------------------------------------
  # Population standard deviation and variance

  if (is.null(sigma) && !is.null(sigma2)) { sigma <- sqrt(sigma2) }

  if (!is.null(sigma) && is.null(sigma2)) { sigma2 <- sigma^2 }

  if (!is.null(sigma) && length(sigma) == 1L) { sigma <- c(sigma, sigma) }

  if (!is.null(sigma2) && length(sigma2) == 1L) { sigma2 <- c(sigma2, sigma2) }

  #----------------------------------------
  # Alternative hypothesis

  if (all(c("two.sided", "less", "greater") %in% alternative)) { alternative <- "two.sided" }

  #-----------------------------------------------------------------------------------
  # Main Function

  #----------------------------------------
  # No Grouping, No Split
  if (is.null(group) && is.null(split)) {

    result <- data.frame(matrix(NA, ncol = 12L, nrow = length(y.vars),
                                dimnames = list(NULL, c("variable", "n1", "nNA1", "m1", "sd1", "n2", "nNA2", "m2", "sd2", "m.diff", "low", "upp"))),
                         stringsAsFactors = FALSE)

    for (i in seq_along(y.vars)) {

      data.split <- split(data[, y.vars[i]], f = data[, group.var])

      result[i, ] <- data.frame(variable = y.vars[i],
                                ci.mean.diff.default(x = data.split[[1L]], y = data.split[[2L]],
                                                     sigma = sigma, sigma2 = sigma2, var.equal = var.equal,
                                                     paired = FALSE, alternative = alternative,
                                                     conf.level = conf.level, group = NULL, split = NULL, sort.var = sort.var,
                                                     digits = digits, as.na = NULL, check = check, output = FALSE)$result[, -1L],
                                stringsAsFactors = FALSE)

    }

  #----------------------------------------
  # Grouping, No Split
  } else if (!is.null(group) && is.null(split)) {

    object.group <- lapply(split(data[, var.formula], f = group),
                           function(y) misty::ci.mean.diff(formula, data = y, sigma = NULL, sigma2 = NULL,
                                                           var.equal = var.equal, alternative = alternative,
                                                           conf.level = conf.level, group = NULL, split = NULL,
                                                           sort.var = sort.var, na.omit = na.omit,
                                                           as.na = as.na, check = FALSE, output = FALSE)$result)

    result <- data.frame(group = rep(names(object.group), each = length(y.vars)),
                         eval(parse(text = paste0("rbind(", paste0("object.group[[", seq_len(length(object.group)), "]]",
                                                                   collapse = ", "), ")"))), stringsAsFactors = FALSE)

  #----------------------------------------
  # No Grouping, Split
  } else if (is.null(group) && !is.null(split)) {

    result <- lapply(split(data.frame(data[, var.formula], stringsAsFactors = FALSE), f = split),
                     function(y) misty::ci.mean.diff(formula, data = y, sigma = NULL, sigma2 = NULL, var.equal = var.equal,
                                                     alternative = alternative, conf.level = conf.level,
                                                     group = NULL, split = NULL, sort.var = sort.var, na.omit = na.omit,
                                                     as.na = as.na, check = FALSE, output = FALSE)$result)

  #----------------------------------------
  # Grouping, Split
  } else if (!is.null(group) && !is.null(split)) {

    result <- lapply(split(data.frame(data[, var.formula], .group = group, stringsAsFactors = FALSE), f = split),
                     function(y) misty::ci.mean.diff(formula, data = y, sigma = NULL, sigma2 = NULL,
                                                     var.equal = var.equal, alternative = alternative, conf.level = conf.level,
                                                     group = y$.group, split = NULL, sort.var = sort.var, na.omit = na.omit,
                                                     as.na = as.na, check = FALSE, output = FALSE)$result)

  }

  ####################################################################################
  # Return object and output

  object <- list(call = match.call(),
                 type = "mean.diff.i",
                 data = list(data = data[, var.formula], group = group, split = split),
                 args = list(formula = formula, sigma = sigma, sigma2 = sigma2,
                             var.equal = var.equal, alternative = alternative,
                             conf.level = conf.level, sort.var = sort.var,
                             na.omit = na.omit, digits = digits, as.na = as.na,
                             check = check, output = output),
                 result = result)

  class(object) <- "ci"

  #-----------------------------------------------------------------------------------
  # Output

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
