#' z-Test
#'
#' This function performs one-sample, two-sample, and paired-sample z-tests.
#'
#' Cohen's d reported when argument \code{effsize = TRUE} is based on the population standard deviation
#' specified in \code{sigma} or the square root of the population variance specified in \code{sigma2}.
#' In a one-sample and paired-sample design, Cohen's d is the mean of the difference scores divided
#' by the population standard deviation of the difference scores (i.e., equivalent to Cohen's \eqn{d_z}
#' according to Lakens, 2013). In a two-sample design, Cohen's d is the difference between means of the
#' two groups of observations divided by either the population standard deviation when assuming and
#' specifying equal standard deviations or the unweighted pooled population standard deviation
#' when assuming and specifying unequal standard deviations.
#'
#' @param x           a numeric vector of data values.
#' @param y           a numeric vector of data values.
#' @param sigma       a numeric vector indicating the population standard deviation(s). In case
#'                    of two-sample z-test, equal standard deviations are assumed when specifying one value
#'                    for the argument \code{sigma}; when specifying two values for the argument \code{sigma},
#'                    unequal standard deviations are assumed. Note that either argument \code{sigma} or
#'                    argument \code{sigma2} is specified.
#' @param sigma2      a numeric vector indicating the population variance(s). In case of two-sample z-test,
#'                    equal variances are assumed when specifying one value for the argument \code{sigma2};
#'                    when specifying two values for the argument \code{sigma}, unequal variance are assumed.
#'                    Note that either argument \code{sigma} or argument \code{sigma2} is specified.
#' @param mu          a numeric value indicating the population mean under the null hypothesis. Note that
#'                    the argument \code{mu} is only used when computing a one-sample z-test.
#' @param paired      logical: if \code{TRUE}, paired-sample z-test is computed.
#' @param alternative a character string specifying the alternative hypothesis, must be one of
#'                    \code{"two.sided"} (default), \code{"greater"} or \code{"less"}.
#' @param hypo        logical: if \code{TRUE}, null and alternative hypothesis are shown on the console.
#' @param descript    logical: if \code{TRUE}, descriptive statistics are shown on the console.
#' @param effsize     logical: if \code{TRUE}, effect size measure Cohen's d is shown on the console.
#' @param digits      an integer value indicating the number of decimal places to be used for displaying
#'                    descriptive statistics and confidence interval.
#' @param p.digits    an integer value indicating the number of decimal places to be used for displaying
#'                    the \emph{p}-value.
#' @param as.na       a numeric vector indicating user-defined missing values,
#'                    i.e. these values are converted to \code{NA} before conducting the analysis.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#' @param output      logical: if \code{TRUE}, output is shown on the console.
#' @param formula     in case of two sample z-test (i.e., \code{paired = FALSE}), a formula of the
#'                    form \code{y ~ group} where \code{group} is a numeric variable, character variable
#'                    or factor with two values or factor levels giving the corresponding groups.
#' @param data        a matrix or data frame containing the variables in the formula \code{formula}.
#' @param ...         further arguments to be passed to or from methods.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{test.t}}, \code{\link{cohens.d}}, \code{\link{ci.mean.diff}}, \code{\link{ci.mean}}
#'
#' @references
#' Lakens, D. (2013). Calculating and reporting effect sizes to facilitate cumulative science:
#' A practical primer for t-tests and ANOVAs. \emph{Frontiers in Psychology, 4}, 1-12.
#' https://doi.org/10.3389/fpsyg.2013.00863
#'
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology - Using R and SPSS}.
#' John Wiley & Sons.
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following entries:
#' function call (\code{call}), type of analysis \code{type}, list with the input specified in \code{x}
#' (\code{data}), specification of function arguments (\code{args}), and result table (\code{result}).
#'
#' @export
#'
#' @examples
#' dat1 <- data.frame(group = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2),
#'                    x = c(3, 1, 4, 2, 5, 3, 2, 3, 6, 4, 3, NA))
#'
#' #--------------------------------------
#' # One-Sample Design
#'
#' # Two-sided one-sample z-test
#' # population mean = 3, population standard deviation = 1.2
#' test.z(dat1$x, sigma = 1.2, mu = 3)
#'
#' # Two-sided one-sample z-test
#' # population mean = 3, population variance = 1.44
#' test.z(dat1$x, sigma2 = 1.44, mu = 3)
#'
#' # One-sided one-sample z-test
#' # population mean = 3, population standard deviation = 1.2
#' test.z(dat1$x, sigma = 1.2, mu = 3, alternative = "greater")
#'
#' # Two-sided one-sample z-test
#' # population mean = 3, population standard deviation = 1.2
#' # convert value 3 to NA
#' test.z(dat1$x, sigma = 1.2, mu = 3, as.na = 3)
#'
#' # Two-sided one-sample z-test
#' # population mean = 3, population standard deviation = 1.2
#' # print Cohen's d
#' test.z(dat1$x, sigma = 1.2, mu = 3, effsize = TRUE)
#'
#' # Two-sided one-sample z-test
#' # population mean = 3, population standard deviation = 1.2
#' # do not print hypotheses and descriptive statistics
#' test.z(dat1$x, sigma = 1.2, mu = 3, hypo = FALSE, descript = FALSE)
#'
#' # Two-sided one-sample z-test
#' # population mean = 3, population standard deviation = 1.2
#' # print descriptive statistics with 3 digits and p-value with 5 digits
#' test.z(dat1$x, sigma = 1.2, mu = 3, digits = 3, p.digits = 5)
#'
#' #--------------------------------------
#' # Two-Sample Design
#'
#' # Two-sided two-sample z-test
#' # population standard deviation (SD) = 1.2, equal SD assumption
#' test.z(x ~ group, sigma = 1.2, data = dat1)
#'
#' # Two-sided two-sample z-test
#' # population standard deviation (SD) = 1.2 and 1.5, unequal SD assumption
#' test.z(x ~ group, sigma = c(1.2, 1.5), data = dat1)
#'
#' # Two-sided two-sample z-test
#' # population variance (Var) = 1.44 and 2.25, unequal Var assumption
#' test.z(x ~ group, sigma2 = c(1.44, 2.25), data = dat1)
#'
#' # One-sided two-sample z-test
#' # population standard deviation (SD) = 1.2, equal SD assumption
#' test.z(x ~ group, sigma = 1.2, data = dat1, alternative = "greater")
#'
#' # Two-sided two-sample z-test
#' # population standard deviation (SD) = 1.2, equal SD assumption
#' # print Cohen's d
#' test.z(x ~ group, sigma = 1.2, data = dat1, effsize = TRUE)
#'
#' # Two-sided two-sample z-test
#' # population standard deviation (SD) = 1.2, equal SD assumption
#' # do not print hypotheses and descriptive statistics,
#' # print Cohen's d
#' test.z(x ~ group, sigma = 1.2, data = dat1, descript = FALSE, hypo = FALSE)
#'
#' # Two-sided two-sample z-test
#' # population mean = 3, population standard deviation = 1.2
#' # print descriptive statistics with 3 digits and p-value with 5 digits
#' test.z(x ~ group, sigma = 1.2, data = dat1, digits = 3, p.digits = 5)
#'
#' #-----------------
#'
#' group1 <- c(3, 1, 4, 2, 5, 3, 6, 7)
#' group2 <- c(5, 2, 4, 3, 1)
#'
#' # Two-sided two-sample z-test
#' # population standard deviation (SD) = 1.2, equal SD assumption
#' test.z(group1, group2, sigma = 1.2, data = dat1)
#'
#' #--------------------------------------
#' # Paired-Sample Design
#'
#' dat2 <- data.frame(pre = c(1, 3, 2, 5, 7),
#'                    post = c(2, 2, 1, 6, 8), stringsAsFactors = FALSE)
#'
#' # Two-sided paired-sample z-test
#' # population standard deviation of difference score = 1.2
#' test.z(dat2$pre, dat2$post, sigma = 1.2, paired = TRUE)
#'
#' # Two-sided paired-sample z-test
#' # population variance of difference score = 1.44
#' test.z(dat2$pre, dat2$post, sigma2 = 1.44, paired = TRUE)
#'
#' # One-sided paired-sample z-test
#' # population standard deviation of difference score = 1.2
#' test.z(dat2$pre, dat2$post, sigma = 1.2, paired = TRUE,
#'        alternative = "greater")
#'
#' # Two-sided paired-sample z-test
#' # population standard deviation of difference score = 1.2
#' # convert value 1 to NA
#' test.z(dat2$pre, dat2$post, sigma = 1.2, as.na = 1, paired = TRUE)
#'
#' # Two-sided paired-sample z-test
#' # population standard deviation of difference score = 1.2
#' # print Cohen's d
#' test.z(dat2$pre, dat2$post, sigma = 1.2, paired = TRUE, effsize = TRUE)
#'
#' # Two-sided paired-sample z-test
#' # population standard deviation of difference score = 1.2
#' # do not print hypotheses and descriptive statistics
#' test.z(dat2$pre, dat2$post, sigma = 1.2, mu = 3, paired = TRUE,
#'        hypo = FALSE, descript = FALSE)
#'
#' # Two-sided paired-sample z-test
#' # population standard deviation of difference score = 1.2
#' # print descriptive statistics with 3 digits and p-value with 5 digits
#' test.z(dat2$pre, dat2$post, sigma = 1.2, paired = TRUE,
#'        digits = 3, p.digits = 5)
test.z <- function(x, ...) {

  UseMethod("test.z")

}

####################################################################################
# Default S3 method

test.z.default <- function(x, y = NULL, sigma = NULL, sigma2 = NULL, mu = 0,
                           paired = FALSE, alternative = c("two.sided", "less", "greater"),
                           hypo = TRUE, descript = TRUE, effsize = FALSE, digits = 2,
                           p.digits = 4, as.na = NULL, check = TRUE, output = TRUE, ...) {

  #......
  # Check if input 'x' is missing
  if (isTRUE(missing(x))) {

    stop("Please specify a numeric vector for the argument 'x'", call. = FALSE)

  }

  #......
  # Check if input 'x' is NULL
  if (isTRUE(is.null(x))) {

    stop("Input specified for the argument 'x' is NULL.", call. = FALSE)

  }

  #......
  # Check if input 'x' is NULL
  if (isTRUE(is.null(sigma) && is.null(sigma2))) {

    stop("Please specify either argument 'sigma' or argument 'sigma2'.", call. = FALSE)

  }

  #......
  # Check input 'paired'
  if (isTRUE(!is.logical(paired))) {

    stop("Please specify TRUE or FALSE for the argument 'paired'.", call. = FALSE)

  }

  #----------------------------------------
  # Convert user-missing values into NA

  if (isTRUE(!is.null(as.na))) {

    # One sample
    if (isTRUE(is.null(y))) {

      # Replace user-specified values with missing values
      x <- misty::as.na(x, na = as.na, check = check)

      if (isTRUE(all(is.na(x)))) {

        stop("After converting user-missing values into NA, 'x' is completely missing.", call. = FALSE)

      }

    # Two or paired sample
    } else {

      # Replace user-specified values with missing values
      x <- misty::as.na(x, na = as.na, check = check)
      y <- misty::as.na(y, na = as.na, check = check)

      if (isTRUE(!is.null(y))) {

        # Variable with missing values only
        xy.miss <- vapply(list(x = x, y = y), function(y) all(is.na(y)), FUN.VALUE = logical(1))
        if (isTRUE(any(xy.miss))) {

          stop(paste0("After converting user-missing values into NA, following variables are completely missing: ",
                      paste(names(which(xy.miss)), collapse = ", ")), call. = FALSE)

        }

      }

    }

  }

  #----------------------------------------
  # Paired sample

  if (isTRUE(is.null(y) && isTRUE(paired))) {

    # Length of 'x' and 'y'
    if (isTRUE(length(x) != length(y))) {

      stop("Length of the vector specified in 'x' does not match the length of the vector specified in 'y'.",
           call. = FALSE)

    }

    # Listwise deletion
    if (isTRUE(nrow(na.omit(data.frame(x = x, y = y))) < 2)) {

      stop("After listwise deletion, the number of pairs of observations is less than two.",call. = FALSE)

    }

  }

  #-----------------------------------------------------------------------------------
  # Input Check

  #......
  # Check input 'check'
  if (isTRUE(!is.logical(check))) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }

  if (isTRUE(check)) {

    #......
    # Check input 'sigma' and 'sigma2'
    if (isTRUE(!is.null(sigma) && !is.null(sigma2))) {

      if (isTRUE(!identical(sigma^2, sigma2))) {

        stop("Arguments 'sigma' and 'sigma2' do not match.", call. = FALSE)

      }

    }

    #......
    # Check input 'sigma'
    if (isTRUE(!is.null(sigma))) {

      # SD smaller or equal 0
      if (isTRUE(any(sigma <= 0L))) {

        stop("Please specify numeric values grater than 0 for the argument 'sigma'.", call. = FALSE)

      }

      # One sample
      if (isTRUE(is.null(y))) {

        # Length of 'sigma'
        if (isTRUE(length(sigma) > 1L)) {

          stop("Please specify one numeric values for the argument 'sigma' for one sample.", call. = FALSE)

        }

      # Two samples
      } else if (isTRUE(!is.null(y) && !isTRUE(paired))) {

        # Length of 'sigma'
        if (isTRUE(length(sigma) > 2L)) {

          stop("Please specify one or two numeric values for the argument 'sigma' in independent samples.", call. = FALSE)

        }

      # Paired samples
      } else if (isTRUE(!is.null(y) && isTRUE(paired))) {

        # Length of 'sigma'
        if (isTRUE(length(sigma) > 1L)) {

          stop("Please specify one numeric values for the argument 'sigma' in paired samples.", call. = FALSE)

        }

      }

    }

    #......
    # Check input 'sigma2'
    if (isTRUE(!is.null(sigma2))) {

      # Variance smaller or equal 0
      if (isTRUE(any(sigma2 <= 0L))) {

        stop("Please specify numeric values grater than 0 for the argument 'sigma2'.", call. = FALSE)

      }

      if (!isTRUE(paired)) {

        # Length of 'sigma2'
        if (length(sigma2) > 2L) {

          stop("Please specify one or two numeric values for the argument 'sigma2' in paired samples.", call. = FALSE)

        }

      } else {

        # Length of 'sigma2'
        if (isTRUE(length(sigma2) > 1L)) {

          stop("Please specify one numeric values for the argument 'sigma2' in dependent samples.", call. = FALSE)

        }

      }

    }

    #......
    # Check input 'mu'
    if (isTRUE(length(mu) > 1L)) {

      stop("Please specify one numeric value for the argument 'mu'.", call. = FALSE)

    }

    #......
    # Check input 'alternative'
    if (isTRUE(!all(alternative %in%  c("two.sided", "less", "greater")))) {

      stop("Character string in the argument 'alternative' does not match with \"two.sided\", \"less\", or \"greater\".",
           call. = FALSE)

    }

    #......
    # Check input 'hypo'
    if (isTRUE(!is.logical(hypo))) {

      stop("Please specify TRUE or FALSE for the argument 'hypo'.", call. = FALSE)

    }

    #......
    # Check input 'descript'
    if (isTRUE(!is.logical(descript))) {

      stop("Please specify TRUE or FALSE for the argument 'descript'.", call. = FALSE)

    }

    #......
    # Check input 'effsize'
    if (isTRUE(!is.logical(effsize))) {

      stop("Please specify TRUE or FALSE for the argument 'effsize'.", call. = FALSE)

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

    #......
    # Check input 'output'
    if (isTRUE(!is.logical(output))) {

      stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE)

    }

  }

  ####################################################################################
  # Arguments

  #----------------------------------------
  # Population standard deviation and variance

  if (isTRUE(is.null(sigma) && !is.null(sigma2))) { sigma <- sqrt(sigma2) }

  if (isTRUE(!is.null(sigma) && is.null(sigma2))) { sigma2 <- sigma^2 }

  #....
  # Two-sample design
  if (isTRUE(!is.null(y) && !isTRUE(paired))) {

    if (isTRUE(!is.null(sigma) && length(sigma) == 1L)) { sigma <- c(sigma, sigma) }

    if (isTRUE(!is.null(sigma2) && length(sigma2) == 1L)) { sigma2 <- c(sigma2, sigma2) }

  }

  #----------------------------------------
  # Alternative hypothesis

  if (all(c("two.sided", "less", "greater") %in% alternative)) { alternative <- "two.sided" }

  #-----------------------------------------------------------------------------------
  # Main Function

  #...
  # One-sample design
  if (isTRUE(is.null(y))) {

    # Descriptive statistics
    x.ci <- misty::ci.mean(x = x, sigma = sigma, alternative = alternative,
                           output = FALSE)$result

    # Standard error of the mean
    se <- (sigma / sqrt(x.ci[["n"]]))

    # Test statistic
    z <- (x.ci[["m"]] - mu) / se

    # Cohen's d
    d <- (x.ci[["m"]] - mu) / sigma

    result <- data.frame(n = x.ci[["n"]],
                         nNA = x.ci[["nNA"]],
                         m = x.ci[["m"]],
                         sd = x.ci[["sd"]],
                         m.diff = x.ci[["m"]] - mu,
                         se = se,
                         z = z,
                         pval = switch(alternative,
                                       two.sided = pnorm(abs(z), lower.tail = FALSE) * 2,
                                       less = pnorm(z, lower.tail = TRUE),
                                       greater = pnorm(z, lower.tail = FALSE)),
                        d = d, row.names = NULL)

    sample <- "one"

  #...
  # Two samples design
  } else if (isTRUE(!is.null(y) && !isTRUE(paired))) {

    # Descriptive statistics
    x.ci <- misty::ci.mean.diff(x = x, y = y, sigma = sigma, alternative = alternative,
                                output = FALSE)$result

    # Standard error of the mean difference
    se <- sqrt((sigma2[1] / x.ci[1, "n"]) + (sigma2[2] / x.ci[2, "n"]))

    # Test statistic
    z <- x.ci[2, "m.diff"] / se

    # Cohen's d
    d <- x.ci[2, "m.diff"] / mean(sigma)

    result <- data.frame(cbind(x.ci[, -which(colnames(x.ci) %in% c("variable", "between", "low", "upp"))],
                               se = c(NA, se),
                               z = c(NA, z),
                               pval = c(NA, switch(alternative,
                                                   two.sided = pnorm(abs(z), lower.tail = FALSE) * 2,
                                                   less = pnorm(z, lower.tail = TRUE),
                                                   greater = pnorm(z, lower.tail = FALSE))),
                               d = c(NA, d)), row.names = NULL)

    sample <- "two"

  #...
  # Paired samples
  } else if (isTRUE(!is.null(y) && isTRUE(paired))) {

    # Descriptive statistics
    x.ci <- misty::ci.mean.diff(x = x, y = y, sigma = sigma, paired = TRUE,
                                alternative = alternative, output = FALSE)$result

    # Standard error of the mean difference
    se <- (sigma / sqrt(x.ci[["n"]]))

    # Test statistic
    z <- (x.ci[["m.diff"]]) / se

    # Cohen's d
    d <- (x.ci[["m.diff"]]) / sigma

    result <- data.frame(n  = x.ci[["n"]],
                         nNA = x.ci[["nNA"]],
                         m1 = x.ci[["m1"]],
                         m2 = x.ci[["m2"]],
                         m.diff = x.ci[["m.diff"]],
                         sd.diff = x.ci[["sd.diff"]],
                         se = se,
                         z = z,
                         pval = switch(alternative,
                                       two.sided = pnorm(abs(z), lower.tail = FALSE) * 2,
                                       less = pnorm(z, lower.tail = TRUE),
                                       greater = pnorm(z, lower.tail = FALSE)),
                         d = d, row.names = NULL)


    sample <- "paired"

  }

  ####################################################################################
  # Return object and output

  object <- list(call = match.call(),
                 type = "test.z",
                 sample = sample,
                 data = list(x, y),
                 args = list(sigma = sigma, sigma2 = sigma2, mu = mu,
                             paired = paired, alternative = alternative,
                             hypo = hypo, descript = descript,
                             effsize = effsize, digits = digits, p.digits = p.digits,
                             as.na = as.na, check = check, output = output),
                 result = result)

  class(object) <- "misty.object"

  #-----------------------------------------------------------------------------------
  # Output

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}

####################################################################################
# S3 method for class 'formula'

test.z.formula <- function(formula, data, sigma = NULL, sigma2 = NULL,
                           alternative = c("two.sided", "less", "greater"),
                           hypo = TRUE, descript = TRUE, effsize = FALSE,
                           digits = 2, p.digits = 4, as.na = NULL, check = TRUE,
                           output = TRUE, ...) {

  #......
  # Check if input 'formula' is missing
  if (isTRUE(missing(formula))) {

    stop("Please specify a formula using the argument 'formula'", call. = FALSE)

  }

  #......
  # Check if input 'data' is missing
  if (isTRUE(missing(data))) {

    stop("Please specify a matrix or data frame for the argument 'x'.", call. = FALSE)

  }

  #......
  # Check if input 'data' is NULL
  if (isTRUE(is.null(data))) {

    stop("Input specified for the argument 'data' is NULL.", call. = FALSE)

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

  #......
  # Check input 'check'
  if (isTRUE(!is.logical(check))) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }

  if (isTRUE(check)) {

    # Check if variables are in the data
    var.data <- !var.formula %in% colnames(data)
    if (isTRUE(any(var.data))) {

      stop(paste0("Variables specified in the the formula were not found in 'data': ",
                  paste(var.formula[which(var.data)], collapse = ", ")), call. = FALSE)

    }

    # Check if input 'formula' has only one grouping variable
    if (isTRUE(length(group.var) != 1L)) {

      stop("Please specify a formula with only one grouping variable.", call. = FALSE)

    }

    # Check if input 'formula' has only one outcome variable
    if (isTRUE(length(y.vars) != 1L)) {

      stop("Please specify a formula with only one outcome variable.", call. = FALSE)

    }

  }

  #-----------------------------------------------------------------------------------
  # Main Function

  data.split <- split(data[, y.vars], f = data[, group.var])

  object <- test.z.default(x = data.split[[1L]], y = data.split[[2L]],
                           sigma = sigma, sigma2 = sigma2,
                           alternative = alternative, hypo = hypo,
                           descript = descript, effsize = effsize,
                           digits = digits, p.digits = p.digits, as.na = as.na,
                           check = check, output = FALSE)

  ####################################################################################
  # Return object and output

  object <- list(call = match.call(),
                 type = "test.z",
                 sample = "two",
                 data = data[, var.formula],
                 args = list(formula = object$args$formula,
                             sigma = object$args$sigma, sigma2 = object$args$sigma2,
                             alternative = object$args$alternative,
                             hypo = object$args$hypo, descript = object$args$descript,
                             effsize = object$args$effsize, digits = object$args$digits,
                             p.digits = object$args$p.digits, as.na = object$args$as.na,
                             check = check, output = object$args$output),
                 result = object$result)

  class(object) <- "misty.object"

  #-----------------------------------------------------------------------------------
  # Output

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
