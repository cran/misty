#' z-Test
#'
#' This function computes one sample, two sample, and paired sample z-test.
#'
#' @param x              a numeric vector of data values.
#' @param y              a numeric vector of data values.
#' @param sigma          a numeric vector indicating the population standard deviation(s). In case
#'                       of two sample z-test, equal standard deviations are assumed when specifying one value
#'                       for the argument \code{sigma}; when specifying two values for the argument \code{sigma},
#'                       unequal standard deviations are assumed. Note that either argument \code{sigma} or
#'                       argument \code{sigma2} is specified.
#' @param sigma2         a numeric vector indicating the population variance(s). In case of two sample z-test,
#'                       equal variances are assumed when specifying one value for the argument \code{sigma2};
#'                       when specifying two values for the argument \code{sigma}, unequal variance are aussumed.
#'                       Note that either argument \code{sigma} or argument \code{sigma2} is specified.
#' @param mu             a numeric value indicating the population mean under the null hypothesis. Note that
#'                       the argument \code{mu} is only used when computing a one sample z-test.
#' @param paired         logical: if \code{TRUE}, paired sample z-test is computed.
#' @param alternative    a character string specifying the alternative hypothesis, must be one of
#'                       \code{"two.sided"} (default), \code{"greater"} or \code{"less"}.
#' @param conf.level     a numeric value between 0 and 1 indicating the confidence level of the interval.
#' @param digits         an integer value indicating the number of decimal places to be used for displaying
#'                       descriptive statistics and confidence interval.
#' @param p.digits       an integer value indicating the number of decimal places to be used for displaying
#'                       the \emph{p}-value.
#' @param as.na          a numeric vector indicating user-defined missing values,
#'                       i.e. these values are converted to \code{NA} before conducting the analysis.
#' @param check          logical: if \code{TRUE}, argument specification is checked.
#' @param output         logical: if \code{TRUE}, output is shown on the console.
#' @param formula        in case of two sample z-test (i.e., \code{paired = FALSE}), a formula of the
#'                       form \code{y ~ group} where \code{group} is a numeric variable, character variable
#'                       or factor with two values or factor levels giving the corresponding groups.
#' @param data           a matrix or data frame containing the variables in the formula \code{formula}.
#' @param ...            further arguments to be passed to or from methods.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{t.test}}, \code{\link{ci.mean.diff}}, \code{\link{ci.mean}}
#'
#' @references
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology - Using R and SPSS}.
#' John Wiley & Sons.
#'
#' @return
#' Returns an object of class \code{test}, which is a list with following entries:
#' function call (\code{call}), type of confidence interval (\code{type}), list with the input specified in \code{x}
#' (\code{data}), specification of function arguments (\code{args}), and result table (\code{result}).
#'
#' @export
#'
#' @examples
#' dat.bs <- data.frame(group = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2),
#'                     x = c(3, 1, 4, 2, 5, 3, 2, 3, 6, 4, 3, NA),
#'                     stringsAsFactors = FALSE)
#'
#' #--------------------------------------
#' # Between-Subject Design
#'
#' # Two-sided one sample z-test with 95% confidence interval
#' # population mean = 3, population standard deviation = 1.2
#' z.test(dat.bs$x, sigma = 1.2, mu = 3)
#'
#' # Two-sided one sample z-test with 95% confidence interval
#' # population mean = 3, population variance = 1.44
#' z.test(dat.bs$x, sigma2 = 1.44, mu = 3)
#'
#' # One-sided one sample z-test with 95% confidence interval
#' # population mean = 3, population standard deviation = 1.2
#' z.test(dat.bs$x, sigma = 1.2, mu = 3, alternative = "greater")
#'
#' # Two-sided one sample z-test with 95% confidence interval
#' # population mean = 3, population standard deviation = 1.2
#' # # convert value 3 to NA
#' z.test(dat.bs$x, sigma = 1.2, mu = 3, as.na = 3)
#'
#' # Two-sided one sample z-test with 99% confidence interval
#' # population mean = 3, population standard deviation = 1.2
#' z.test(dat.bs$x, sigma = 1.2, mu = 3, conf.level = 0.99)
#'
#' # Two-sided one sample z-test with 95% confidence interval
#' # population mean = 3, population standard deviation = 1.2
#' # print descriptive statistics with 3 digits and p-value with 5 digits
#' z.test(dat.bs$x, sigma = 1.2, mu = 3, digits = 3, p.digits = 5)
#'
#' # Two-sided two sample z-test with 95% confidence interval
#' # population standard deviation (SD) = 1.2, equal SD assumption
#' z.test(x ~ group, sigma = 1.2, data = dat.bs)
#'
#' # Two-sided two sample z-test with 95% confidence interval
#' # population standard deviation = 1.2 and 1.5
#' z.test(x ~ group, sigma = c(1.2, 1.5), data = dat.bs)
#'
#' # Two-sided two sample z-test with 95% confidence interval
#' # population variance = 1.44 and 2.25
#' z.test(x ~ group, sigma = c(1.44, 2.25), data = dat.bs)
#'
#' # One-sided two sample z-test with 95% confidence interval
#' # population standard deviation (SD) = 1.2, equal SD assumption
#' z.test(x ~ group, sigma = 1.2, data = dat.bs, alternative = "less")
#'
#' #-----------------
#'
#' group1 <- c(3, 1, 4, 2, 5, 3, 6, 7)
#' group2 <- c(5, 2, 4, 3, 1)
#'
#' # Two-sided two sample z-test with 95% confidence interval
#' # population standard deviation (SD) = 1.2, equal SD assumption
#' z.test(group1, group2, sigma = 1.2, data = dat.bs)
#'
#' #--------------------------------------
#' # Within-Subject Design
#' dat.ws <- data.frame(pre = c(1, 3, 2, 5, 7),
#'                      post = c(2, 2, 1, 6, 8), stringsAsFactors = FALSE)
#'
#' # Two-sided paired sample z-test with 95% confidence interval
#' # population standard deviation of difference score = 1.2
#' z.test(dat.ws$pre, dat.ws$post, sigma = 1.2, paired = TRUE)
#'
#' # Two-sided paired sample z-test with 95% confidence interval
#' # population variance of difference score = 1.44
#' z.test(dat.ws$pre, dat.ws$post, sigma2 = 1.44, paired = TRUE)
#'
#' # One-sided paired sample z-test with 95% confidence interval
#' # population standard deviation of difference score = 1.2
# z.test(dat.ws$pre, dat.ws$post, sigma = 1.2, paired = TRUE,
#        alternative = "greater")
#
# # Two-sided paired sample z-test with 95% confidence interval
# # population standard deviation of difference score = 1.2
# # convert value 1 to NA
# z.test(dat.ws$pre, dat.ws$post, sigma = 1.2, as.na = 1, paired = TRUE)
z.test <- function(x, ...) {

  UseMethod("z.test")

}

####################################################################################
# Default S3 method

z.test.default <- function(x, y = NULL, sigma = NULL, sigma2 = NULL, mu = 0,
                           paired = FALSE, alternative = c("two.sided", "less", "greater"),
                           conf.level = 0.95, digits = 2, p.digits = 3, as.na = NULL,
                           check = TRUE, output = TRUE, ...) {

  #......
  # Check if input 'x' is missing
  if (missing(x)) {

    stop("Please specify a numeric vector for the argument 'x'", call. = FALSE)

  }

  #......
  # Check if input 'x' is NULL
  if (is.null(x)) {

    stop("Input specified for the argument 'x' is NULL.", call. = FALSE)

  }

  #......
  # Check if input 'x' is NULL
  if (is.null(sigma) && is.null(sigma2)) {

    stop("Please specify either argument 'sigma' or argument 'sigma2'.", call. = FALSE)

  }

  #......
  # Check input 'paired'
  if (!isTRUE(isTRUE(paired) || !isTRUE(paired))) {

    stop("Please specify TRUE or FALSE for the argument 'paired'.", call. = FALSE)

  }

  #----------------------------------------
  # Convert user-missing values into NA

  if (!is.null(as.na)) {

    # One sample
    if (is.null(y)) {

      # Replace user-specified values with missing values
      x <- misty::as.na(x, as.na = as.na, check = check)

      if (all(is.na(x))) {

        stop("After converting user-missing values into NA, 'x' is completely missing.", call. = FALSE)

      }

    # Two or paired sample
    } else {

      # Replace user-specified values with missing values
      x <- misty::as.na(x, as.na = as.na, check = check)
      y <- misty::as.na(y, as.na = as.na, check = check)

      if (!is.null(y)) {

        # Variable with missing values only
        xy.miss <- vapply(list(x = x, y = y), function(y) all(is.na(y)), FUN.VALUE = logical(1))
        if (any(xy.miss)) {

          stop(paste0("After converting user-missing values into NA, following variables are completely missing: ",
                      paste(names(which(xy.miss)), collapse = ", ")), call. = FALSE)

        }

      }

    }

  }

  #----------------------------------------
  # Paired sample

  if (is.null(y) && isTRUE(paired)) {

    # Length of 'x' and 'y'
    if (length(x) != length(y)) {

      stop("Length of the vector specified in 'x' does not match the length of the vector specified in 'y'.",
           call. = FALSE)

    }

    # Listwise deletion
    if (nrow(na.omit(data.frame(x = x, y = y))) < 2) {

      stop("After listwise deletion, the number of pairs of observations is less than two.",call. = FALSE)

    }

  }

  #-----------------------------------------------------------------------------------
  # Input Check

  #......
  # Check input 'check'
  if (!isTRUE(isTRUE(check) || !isTRUE(check))) {

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

      # One sample
      if (is.null(y)) {

        # Length of 'sigma'
        if (length(sigma) > 1L) {

          stop("Please specify one numeric values for the argument 'sigma' for one sample.", call. = FALSE)

        }

      # Two samples
      } else if (!is.null(y) && !isTRUE(paired)) {

        # Length of 'sigma'
        if (length(sigma) > 2L) {

          stop("Please specify one or two numeric values for the argument 'sigma' in independent samples.", call. = FALSE)

        }

      # Paired samples
      } else if (!is.null(y) && isTRUE(paired)) {

        # Length of 'sigma'
        if (length(sigma) > 1L) {

          stop("Please specify one numeric values for the argument 'sigma' in paired samples.", call. = FALSE)

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

      if (!isTRUE(paired)) {

        # Length of 'sigma2'
        if (length(sigma2) > 2L) {

          stop("Please specify one or two numeric values for the argument 'sigma2' in paired samples.", call. = FALSE)

        }

      } else {

        # Length of 'sigma2'
        if (length(sigma2) > 1L) {

          stop("Please specify one numeric values for the argument 'sigma2' in dependent samples.", call. = FALSE)

        }

      }

    }

    #......
    # Check input 'mu'
    if (length(mu) > 1L) {

      stop("Please specify one numeric value for the argument 'mu'.", call. = FALSE)

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
    # Check input 'digits'
    if (digits %% 1L != 0L || digits < 0L) {

      stop("Please specify a positive integer number for the argument 'digits'.", call. = FALSE)

    }

    #......
    # Check input 'digits'
    if (p.digits %% 1L != 0L || p.digits < 0L) {

      stop("Please specify a positive integer number for the argument 'p.digits'.", call. = FALSE)

    }

    #......
    # Check input output
    if (!isTRUE(isTRUE(output) || !isTRUE(output))) {

      stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE)

    }

  }

  ####################################################################################
  # Arguments

  #----------------------------------------
  # Population standard deviation and variance

  if (is.null(sigma) && !is.null(sigma2)) { sigma <- sqrt(sigma2) }

  if (!is.null(sigma) && is.null(sigma2)) { sigma2 <- sigma^2 }

  # Two-sample
  if (!is.null(y) && !isTRUE(paired)) {

    if (!is.null(sigma) && length(sigma) == 1L) { sigma <- c(sigma, sigma) }

    if (!is.null(sigma2) && length(sigma2) == 1L) { sigma2 <- c(sigma2, sigma2) }

  }

  #----------------------------------------
  # Alternative hypothesis

  if (all(c("two.sided", "less", "greater") %in% alternative)) { alternative <- "two.sided" }

  #-----------------------------------------------------------------------------------
  # Main Function

  # One-sample
  if (is.null(y)) {

    x.ci <- misty::ci.mean(x, sigma = sigma, alternative = alternative,
                           conf.level = conf.level, output = FALSE)$result

    z <- (x.ci[["m"]] - mu) / (sigma / sqrt(x.ci[["n"]]))

    result <- data.frame(n = x.ci[["n"]],
                         nNA = x.ci[["nNA"]],
                         m = x.ci[["m"]],
                         sd = x.ci[["sd"]],
                         low = x.ci[["low"]],
                         upp = x.ci[["upp"]],
                         z = z,
                         pval = switch(alternative,
                                       two.sided = pnorm(abs(z), lower.tail = FALSE) * 2,
                                       less = pnorm(z, lower.tail = TRUE),
                                       greater = pnorm(z, lower.tail = FALSE)))

    type <- "z.test.one"

  # Two samples
  } else if (!is.null(y) && !isTRUE(paired)) {

    x.ci <- misty::ci.mean.diff(x = x, y = y, sigma = sigma, alternative = alternative,
                                conf.level  = conf.level, output = FALSE)$result

    z <- x.ci[["m.diff"]] / sqrt((sigma2[1] / x.ci[["n1"]]) + (sigma2[2] / x.ci[["n2"]]))

    result <- data.frame(n1 = x.ci[["n1"]],
                         nNA1 = x.ci[["nNA1"]],
                         m1 = x.ci[["m1"]],
                         sd1 = x.ci[["sd1"]],
                         n2 = x.ci[["n2"]],
                         nNA2 = x.ci[["nNA2"]],
                         m2 = x.ci[["m2"]],
                         sd2 = x.ci[["sd2"]],
                         m.diff = x.ci[["m.diff"]],
                         low = x.ci[["low"]],
                         upp = x.ci[["upp"]],
                         z = z,
                         pval = switch(alternative,
                                       two.sided = pnorm(abs(z), lower.tail = FALSE) * 2,
                                       less = pnorm(z, lower.tail = TRUE),
                                       greater = pnorm(z, lower.tail = FALSE)))

    type <- "z.test.two"


  # Paired samples
  } else if (!is.null(y) && isTRUE(paired)) {

    x.ci <- misty::ci.mean.diff(x = x, y = y, sigma = sigma, paired = TRUE,
                                alternative = alternative, conf.level = conf.level, output = FALSE)$result

    z <- (x.ci[["m.diff"]]) / (sigma / sqrt(x.ci[["n"]]))

    result <- data.frame(n = x.ci[["n"]],
                         nNA1 = x.ci[["nNA1"]],
                         nNA2 = x.ci[["nNA2"]],
                         m1 = x.ci[["m1"]],
                         sd1 = x.ci[["sd1"]],
                         m2 = x.ci[["m2"]],
                         sd2 = x.ci[["sd2"]],
                         m.diff = x.ci[["m.diff"]],
                         sd.diff = x.ci[["sd.diff"]],
                         low = x.ci[["low"]],
                         upp = x.ci[["upp"]],
                         z = z,
                         pval = switch(alternative,
                                       two.sided = pnorm(abs(z), lower.tail = FALSE) * 2,
                                       less = pnorm(z, lower.tail = TRUE),
                                       greater = pnorm(z, lower.tail = FALSE)))


    type <- "z.test.paired"

  }

  ####################################################################################
  # Return object and output

  object <- list(call = match.call(),
                 type = type,
                 data = list(x, y),
                 args = list(sigma = sigma, sigma2 = sigma2, mu = mu,
                             paired = paired, alternative = alternative,
                             conf.level = conf.level, digits = digits,
                             p.digits = p.digits, as.na = as.na,
                             check = check, output = output),
                 result = result)

  class(object) <- "test"

  #-----------------------------------------------------------------------------------
  # Output

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}

####################################################################################
# S3 method for class 'formula'

z.test.formula <- function(formula, data, as.na = NULL, check = TRUE, output = TRUE, ...) {

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

  #......
  # Check if input 'data' is NULL
  if (is.null(data)) {

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
  if (!isTRUE(isTRUE(check) || !isTRUE(check))) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }

  if (isTRUE(check)) {

    # Check if variables are in the data
    var.data <- !var.formula %in% colnames(data)
    if (any(var.data)) {

      stop(paste0("Variables specified in the the formula were not found in 'data': ",
                  paste(var.formula[which(var.data)], collapse = ", ")), call. = FALSE)

    }

    # Check if input 'formula' has only one grouping variable
    if (length(group.var) != 1L) {

      stop("Please specify a formula with only one grouping variable.", call. = FALSE)

    }

    # Check if input 'formula' has only one outcome variable
    if (length(y.vars) != 1L) {

      stop("Please specify a formula with only one outcome variable.", call. = FALSE)

    }

  }

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

  #-----------------------------------------------------------------------------------
  # Main Function

  data.split <- split(data[, y.vars], f = data[, group.var])

  object <- z.test.default(x = data.split[[1L]], y = data.split[[2L]], output = FALSE, ...)

  ####################################################################################
  # Return object and output

  object <- list(call = match.call(),
                 type = "z.test.two",
                 data = data[, var.formula],
                 args = list(formula = object$args$formula, sigma = object$args$sigma,
                             sigma2 = object$args$sigma2, alternative = object$args$alternative,
                             conf.level = object$args$conf.level, digits = object$args$digits,
                             p.digits = object$args$p.digits, as.na = object$args$as.na,
                             check = check, output = object$args$output),
                 result = object$result)

  class(object) <- "test"

  #-----------------------------------------------------------------------------------
  # Output

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
