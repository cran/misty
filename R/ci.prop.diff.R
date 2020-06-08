#' Confidence Interval for the Difference in Proportions
#'
#' This function computes a confidence interval for the difference in proportions from independent and
#' paired samples for one or more variables, optionally by a grouping and/or split variable.
#'
#' The Wald confidence interval which is based on the normal approximation to the binomial distribution are
#' computed by specifying \code{method = "wald"}, while the Newcombe Hybrid Score interval (Newcombe, 1998a;
#' Newcombe, 1998b) is requested by specifying \code{method = "newcombe"}. By default, Newcombe Hybrid Score
#' interval is computed which have been shown to be reliable in small samples (less than n = 30 in each sample)
#' as well as moderate to larger samples(n > 30 in each sample) and with proportions close to 0 or 1, while the
#' Wald confidence intervals does not perform well unless the sample size is large (Fagerland, Lydersen & Laake, 2011).
#'
#' @param x              a numeric vector with 0 and 1 values.
#' @param y              a numeric vector with 0 and 1 values.
#' @param method         a character string specifying the method for computing the confidence interval,
#'                       must be one of \code{"wald"}, or \code{"newcombe"} (default).
#' @param paired         logical: if \code{TRUE}, confidence interval for the difference of proportions
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
#' @param formula        a formula of the form \code{y ~ group} for one outcome variable or
#'                       \code{cbind(y1, y2, y3) ~ group} for more than one outcome variable where
#'                       \code{y} is a numeric variable with 0 and 1 values and \code{group} a numeric
#'                       variable, character variable or factor with two values or factor levelsgiving
#'                       the corresponding group.
#' @param data           a matrix or data frame containing the variables in the formula \code{formula}.
#' @param na.omit        logical: if \code{TRUE}, incomplete cases are removed before conducting the analysis
#'                       (i.e., listwise deletion) when specifying more than one outcome variable.
#' @param ...            further arguments to be passed to or from methods.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{ci.prop}}, \code{\link{ci.mean}}, \code{\link{ci.mean.diff}}, \code{\link{ci.median}},
#' \code{\link{ci.var}}, \code{\link{ci.sd}}, \code{\link{descript}}
#'
#' @exportMethod  ci.prop.diff default
#'
#' @exportMethod  ci.prop.diff formula
#'
#' @references
#' Fagerland, M. W., Lydersen S., & Laake, P. (2011) Recommended confidence intervals for two independent binomial
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
#' Returns an object of class \code{misty.object}, which is a list with following entries:
#' function call (\code{call}),type of analysis \code{type}, list with the input specified in \code{x},
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
#'                      x1 = c(0, 1, 1, 1, 0, 1, 0, 0, 1, 1, 1, NA, 0, 0,
#'                             1, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 1, 0, 0),
#'                      x2 = c(0, 0, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 1,
#'                             1, 0, 1, 0, 1, 1, 1, NA, 1, 0, 0, 1, 1, 1),
#'                      x3 = c(1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0,
#'                             1, 0, 1, 1, 0, 1, 1, 1, 0, 1, NA, 1, 0, 1))
#'
#' #--------------------------------------
#' # Between-Subject Design
#'
#' # Two-Sided 95% Confidence Interval for x1 by group1
#' # Newcombes Hybrid Score interval
#' ci.prop.diff(x1 ~ group1, data = dat.bs)
#'
#' # Two-Sided 95% Confidence Interval for x1 by group1
#' # Wald confidence interval
#' ci.prop.diff(x1 ~ group1, data = dat.bs, method = "wald")
#'
#' # One-Sided 95% Confidence Interval for x1 by group1
#' # Newcombes Hybrid Score interval
#' ci.prop.diff(x1 ~ group1, data = dat.bs, alternative = "less")
#'
#' # Two-Sided 99% Confidence Interval for x1 by group1
#' # Newcombes Hybrid Score interval
#' ci.prop.diff(x1 ~ group1, data = dat.bs, conf.level = 0.99)
#'
#' # Two-Sided 95% Confidence Interval for y1 by group1
#' # # Newcombes Hybrid Score interval, print results with 3 digits
#' ci.prop.diff(x1 ~ group1, data = dat.bs, digits = 3)
#'
#' # Two-Sided 95% Confidence Interval for y1 by group1
#' # # Newcombes Hybrid Score interval, convert value 0 to NA
#' ci.prop.diff(x1 ~ group1, data = dat.bs, as.na = 0)
#'
#' # Two-Sided 95% Confidence Interval for y1, y2, and y3 by group1
#' # Newcombes Hybrid Score interval
#' ci.prop.diff(cbind(x1, x2, x3) ~ group1, data = dat.bs)
#'
#' # Two-Sided 95% Confidence Interval for y1, y2, and y3 by group1
#' # # Newcombes Hybrid Score interval, listwise deletion for missing data
#' ci.prop.diff(cbind(x1, x2, x3) ~ group1, data = dat.bs, na.omit = TRUE)
#'
#' # Two-Sided 95% Confidence Interval for y1, y2, and y3 by group1
#' # Newcombes Hybrid Score interval, analysis by group2 separately
#' ci.prop.diff(cbind(x1, x2, x3) ~ group1, data = dat.bs, group = dat.bs$group2)
#'
#' # Two-Sided 95% Confidence Interval for y1, y2, and y3 by group1
#' # Newcombes Hybrid Score interval, analysis by group2 separately, sort by variables
#' ci.prop.diff(cbind(x1, x2, x3) ~ group1, data = dat.bs, group = dat.bs$group2,
#'              sort.var = TRUE)
#'
#' # Two-Sided 95% Confidence Interval for y1, y2, and y3 by group1
#' # split analysis by group2
#' ci.prop.diff(cbind(x1, x2, x3) ~ group1, data = dat.bs, split = dat.bs$group2)
#'
#' # Two-Sided 95% Confidence Interval for y1, y2, and y3 by group1
#' # Newcombes Hybrid Score interval, analysis by group2 separately, split analysis by group3
#' ci.prop.diff(cbind(x1, x2, x3) ~ group1, data = dat.bs,
#'              group = dat.bs$group2, split = dat.bs$group3)
#'
#' #-----------------
#'
#' group1 <- c(0, 1, 1, 0, 0, 1, 0, 1)
#' group2 <- c(1, 1, 1, 0, 0)
#'
#' # Two-Sided 95% Confidence Interval for the mean difference between group1 amd group2
#' # Newcombes Hybrid Score interval
#' ci.prop.diff(group1, group2)
#'
#' #--------------------------------------
#' # Within-Subject Design
#' dat.ws <- data.frame(pre = c(0, 1, 1, 0, 1),
#'                      post = c(1, 1, 0, 1, 1), stringsAsFactors = FALSE)
#'
#' # Two-Sided 95% Confidence Interval for the mean difference in x1 and x2
#' # Newcombes Hybrid Score interval
#' ci.prop.diff(dat.ws$pre, dat.ws$post, paired = TRUE)
#'
#' # Two-Sided 95% Confidence Interval for the mean difference in x1 and x2
#' # Wald confidence interval
#' ci.prop.diff(dat.ws$pre, dat.ws$post, method = "wald", paired = TRUE)
#'
#' # One-Sided 95% Confidence Interval for the mean difference in x1 and x2
#' # Newcombes Hybrid Score interval
#' ci.prop.diff(dat.ws$pre, dat.ws$post, alternative = "less", paired = TRUE)
#'
#' # Two-Sided 99% Confidence Interval for the mean difference in x1 and x2
#' # Newcombes Hybrid Score interval
#' ci.prop.diff(dat.ws$pre, dat.ws$post, conf.level = 0.99, paired = TRUE)
#'
#' # Two-Sided 95% Confidence Interval for for the mean difference in x1 and x2
#' # Newcombes Hybrid Score interval, print results with 3 digits
#' ci.prop.diff(dat.ws$pre, dat.ws$post, paired = TRUE, digits = 3)
ci.prop.diff <- function(x, ...) {

  UseMethod("ci.prop.diff")

}

####################################################################################
# Confidence interval for the difference of arithmetic means

prop.diff.conf <- function(x, y, method, alternative, paired, conf.level, side) {

  crit <- qnorm(switch(alternative,
                       two.sided = 1L - (1L - conf.level) / 2L,
                       less = conf.level,
                       greater = conf.level))

  #-----------------------------------------
  # Independent samples
  if (!isTRUE(paired)) {

    #.................
    # Data
    x <- na.omit(x)
    y <- na.omit(y)

    x.n <- length(x)
    y.n <- length(y)

    p1 <- sum(x) / x.n
    p2 <- sum(y) / y.n

    p.diff <- p2 - p1

    #.................
    # Wald confidence interval
    if (method == "wald") {

      #......
      # At least 2 observations for x or y
      if ((x.n >= 2L || y.n >= 2L) && (var(x) != 0 || var(y) != 0)) {

        term <- crit * sqrt(p1*(1 - p1) / x.n + p2*(1 - p2) / y.n)

        #......
        # Confidence interval
        ci <- switch(alternative,
                     two.sided = c(low = max(-1, p.diff - term), upp = min(1, p.diff + term)),
                     less = c(low = -1, upp = min(1, p.diff + term)),
                     greater = c(low = max(-1, p.diff - term), upp = 1))
      #......
      # Less than 2 observations for x or y
      } else {

        ci <- c(NA, NA)

      }

    #.................
    # Newcombes Hybrid Score interval
    } else if (method == "newcombe") {

      #......
      # At least 1 observations for x and y
      if ((x.n >= 1L && y.n >= 1L)) {

        if (alternative == "two.sided") {

          x.ci.wilson <- misty::ci.prop(x, method = "wilson", conf.level = conf.level, output = FALSE)$result
          y.ci.wilson <- misty::ci.prop(y, method = "wilson", conf.level = conf.level, output = FALSE)$result

        } else if (alternative == "less") {

          x.ci.wilson <- misty::ci.prop(x, method = "wilson", alternative = "greater", conf.level = conf.level, output = FALSE)$result
          y.ci.wilson <- misty::ci.prop(y, method = "wilson", alternative = "less", conf.level = conf.level, output = FALSE)$result

        } else if (alternative == "greater") {

          x.ci.wilson <- misty::ci.prop(x, method = "wilson", alternative = "less", conf.level = conf.level, output = FALSE)$result
          y.ci.wilson <- misty::ci.prop(y, method = "wilson", alternative = "greater", conf.level = conf.level, output = FALSE)$result

        }

        #......
        # Confidence interval
        ci <- switch(alternative,
                     two.sided = c(p.diff - crit * sqrt((x.ci.wilson$upp*(1 - x.ci.wilson$upp) / x.n) + (y.ci.wilson$low*(1 - y.ci.wilson$low) / y.n)),
                                   p.diff + crit * sqrt((x.ci.wilson$low*(1 - x.ci.wilson$low) / x.n) + (y.ci.wilson$upp*(1 - y.ci.wilson$upp) / y.n))),
                     less = c(-1, p.diff + crit * sqrt((x.ci.wilson$low*(1 - x.ci.wilson$low) / x.n) + (y.ci.wilson$upp*(1 - y.ci.wilson$upp) / y.n))),
                     greater = c(p.diff - crit * sqrt((x.ci.wilson$upp*(1 - x.ci.wilson$upp) / x.n) + (y.ci.wilson$low*(1 - y.ci.wilson$low) / y.n)), 1))

      #......
      # Less than 0 observations for x or y
      } else {

        ci <- c(NA, NA)

      }

    }

  #-----------------------------------------
  # Dependent samples
  } else {

    xy.dat <- na.omit(data.frame(x = x, y = y, stringsAsFactors = FALSE))

    x.p <- mean(xy.dat$x)
    y.p <- mean(xy.dat$y)

    xy.diff.mean <- y.p - x.p

    xy.diff.n <- nrow(xy.dat)

    a <- sum(xy.dat$x == 1 & xy.dat$y == 1)
    b <- sum(xy.dat$x == 1 & xy.dat$y == 0)
    c <- sum(xy.dat$x == 0 & xy.dat$y == 1)
    d <- sum(xy.dat$x == 0 & xy.dat$y == 0)

    #.................
    # Wald confidence interval
    if (method == "wald") {

      #......
      # At least 2 observations for x or y
      if (xy.diff.n >= 2 && (var(xy.dat$x) != 0 || var(xy.dat$y) != 0)) {

        term <- crit * sqrt((b + c) - (b - c)^2 / xy.diff.n) / xy.diff.n

        #......
        # Confidence interval
        ci <- switch(alternative,
                     two.sided = c(low = max(-1, xy.diff.mean - term), upp = min(1, xy.diff.mean + term)),
                     less = c(low = -1, upp = min(1, xy.diff.mean + term)),
                     greater = c(low = max(-1, xy.diff.mean - term), upp = 1))

      } else {

        ci <- c(NA, NA)

      }

    #.................
    # Newcombes Hybrid Score interval
    } else if (method == "newcombe") {

      # At least 1 observations for x and y
      if (xy.diff.n >= 1L) {

        if (alternative == "two.sided") {

          x.ci.wilson <- misty::ci.prop(x, method = "wilson", conf.level = conf.level, output = FALSE)$result
          y.ci.wilson <- misty::ci.prop(y, method = "wilson", conf.level = conf.level, output = FALSE)$result

        } else if (alternative == "less") {

          x.ci.wilson <- misty::ci.prop(x, method = "wilson", alternative = "greater", conf.level = conf.level, output = FALSE)$result
          y.ci.wilson <- misty::ci.prop(y, method = "wilson", alternative = "less", conf.level = conf.level, output = FALSE)$result

        } else if (alternative == "greater") {

          x.ci.wilson <- misty::ci.prop(x, method = "wilson", alternative = "less", conf.level = conf.level, output = FALSE)$result
          y.ci.wilson <- misty::ci.prop(y, method = "wilson", alternative = "greater", conf.level = conf.level, output = FALSE)$result

        }

        A <- (a + b) * (c + d) * (a + c) * (b + d)

        if (A == 0) {

          phi <- 0

        } else {

          phi <- (a * d - b * c) / sqrt(A)

        }

        ci <- switch(alternative,
                     two.sided = c(xy.diff.mean - sqrt((y.p - y.ci.wilson$low)^2 - 2 * phi * (y.p - y.ci.wilson$low) * (x.ci.wilson$upp - x.p) + (x.ci.wilson$upp - x.p)^2),
                                   xy.diff.mean + sqrt((x.p - x.ci.wilson$low)^2 - 2 * phi * (x.p - x.ci.wilson$low) * (y.ci.wilson$upp - y.p) + (y.ci.wilson$upp - y.p)^2)),
                     less = c(-1, xy.diff.mean + sqrt((x.p - x.ci.wilson$low)^2 - 2 * phi * (x.p - x.ci.wilson$low) * (y.ci.wilson$upp - y.p) + (y.ci.wilson$upp - y.p)^2)),
                      greater = c(xy.diff.mean - sqrt((y.p - y.ci.wilson$low)^2 - 2 * phi * (y.p - y.ci.wilson$low) * (x.ci.wilson$upp - x.p) + (x.ci.wilson$upp - x.p)^2), 1))

      } else {

        ci <- c(NA, NA)

      }

    }

  }

  #......
  # Return object
  object <- switch(side, both = ci, low = ci[1L], upp = ci[2L])

  return(object)

}

####################################################################################
# Default S3 method

ci.prop.diff.default <- function(x, y, method = c("wald", "newcombe"), paired = FALSE,
                                 alternative = c("two.sided", "less", "greater"),
                                 conf.level = 0.95, group = NULL, split = NULL, sort.var = FALSE,
                                 digits = 2, as.na = NULL, check = TRUE, output = TRUE, ...) {

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
  # Check if input 'y' is missing
  if (missing(y)) {

    stop("Please specify a numeric vector for the argument 'y'", call. = FALSE)

  }

  #......
  # Check if input 'x' is NULL
  if (is.null(y)) {

    stop("Input specified for the argument 'y' is NULL.", call. = FALSE)

  }


  #......
  # Check input 'paired'
  if (!is.logical(paired)) {

    stop("Please specify TRUE or FALSE for the argument 'paired'.", call. = FALSE)

  }


  #----------------------------------------
  # List or Dataframe

  #......
  # Independent samples
  if (!isTRUE(paired)) {

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
  if (!is.logical(check)) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }

  if (isTRUE(check)) {

    #......
    # Check input 'x'
    if (!all(unlist(x) %in% c(0L, 1L, NA))) {

      stop("Please specify a numeric vector with 0 and 1 values for the argument 'x'.", call. = FALSE)

    }

    #......
    # Check input 'y'
    if (!all(unlist(x) %in% c(0L, 1L, NA))) {

      stop("Please specify a numeric vector with 0 and 1 values for the argument 'y.", call. = FALSE)

    }

    #......
    # Check input 'method'
    if (!all(method %in% c("wald", "newcombe"))) {

      stop("Character string in the argument 'method' does not match with \"wald\", or \"newcombe\".",
           call. = FALSE)

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
      if (!isTRUE(paired)) {

        stop("Please use formula notation for using a grouping variable in independent samples.",
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
      if (!isTRUE(paired)) {

        stop("Please use formula notation for using a split variable in independent samples.",
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
    if (!is.logical(sort.var)) {

      stop("Please specify TRUE or FALSE for the argument 'sort.var'.", call. = FALSE)

    }

    #......
    # Check input 'digits'
    if (digits %% 1L != 0L || digits < 0L) {

      stop("Specify a positive integer number for the argument 'digits'.", call. = FALSE)

    }

    #......
    # Check input output
    if (!is.logical(output)) {

      stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE)

    }

  }

  ####################################################################################
  # Arguments

  #----------------------------------------
  # Method

  if (all(c("wald", "newcombe") %in% method)) { method <- "newcombe" }

  #----------------------------------------
  # Alternative hypothesis

  if (all(c("two.sided", "less", "greater") %in% alternative)) { alternative <- "two.sided" }

  #-----------------------------------------------------------------------------------
  # Main Function

  #----------------------------------------
  # No Grouping, No Split
  if (is.null(group) && is.null(split)) {

    # Independent sample
    if (!isTRUE(paired)) {

      result <- data.frame(variable = "y",
                           n1 = length(na.omit(xy$x)), nNA1 = sum(is.na(xy$x)), p1 = mean(xy$x, na.rm = TRUE),
                           n2 = length(na.omit(xy$y)), nNA2 = sum(is.na(xy$y)), p2 = mean(xy$y, na.rm = TRUE),
                           p.diff = mean(xy$y, na.rm = TRUE) - mean(xy$x, na.rm = TRUE),
                           low = prop.diff.conf(x = xy$x, y = xy$y, method = method, alternative = alternative,
                                               paired = FALSE, conf.level = conf.level, side = "low"),
                           upp = prop.diff.conf(x = xy$x, y = xy$y, method = method, alternative = alternative,
                                                paired = FALSE, conf.level = conf.level, side = "upp"),
                           stringsAsFactors = FALSE, row.names = NULL)

    # Dependent sample
    } else {

      result <- data.frame(variable = "y",
                           n = nrow(na.omit(xy)), nNA1 = sum(is.na(xy$x)), p1 = mean(xy$x, na.rm = TRUE),
                                                  nNA2 = sum(is.na(xy$y)), p2 = mean(xy$y, na.rm = TRUE),
                           p.diff = mean(xy$y - xy$x, na.rm = TRUE),
                           low = prop.diff.conf(x = xy$x, y = xy$y, method = method, alternative = alternative,
                                                paired = TRUE, conf.level = conf.level, side = "low"),
                           upp = prop.diff.conf(x = xy$x, y = xy$y, method = method, alternative = alternative,
                                                paired = TRUE, conf.level = conf.level, side = "upp"),
                           stringsAsFactors = FALSE, row.names = NULL)

    }

  #----------------------------------------
  # Grouping, No Split
  } else if (!is.null(group) && is.null(split)) {

    object.group <- lapply(split(xy, f = group),
                           function(y) ci.prop.diff.default(x = y$x, y = y$y, method = method,
                                                                   alternative = alternative,
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
                     function(y) ci.prop.diff.default(x = y$x, y = y$y,
                                                             alternative = alternative, method = method,
                                                             conf.level = conf.level, paired = paired,
                                                             group = NULL, split = NULL, sort.var = sort.var,
                                                             na.omit = na.omit, as.na = as.na, check = FALSE,
                                                             output = FALSE)$result)

  #----------------------------------------
  # Grouping, Split
  } else if (!is.null(group) && !is.null(split)) {

    result <- lapply(split(data.frame(xy, .group = group, stringsAsFactors = FALSE, row.names = NULL), f = split),
                     function(y) ci.prop.diff.default(x = y$x, y = y$y, method = method,
                                                      alternative = alternative,
                                                      conf.level = conf.level, paired = paired,
                                                      group = y$.group, split = NULL, sort.var = sort.var,
                                                      na.omit = na.omit, as.na = as.na, check = FALSE,
                                                      output = FALSE)$result)

  }

  ####################################################################################
  # Return object and output

  object <- list(call = match.call(),
                 type = "ci", ci = ifelse(!isTRUE(paired), "prop.diff.i",  "prop.diff.p"),
                 data = list(x = x, y = y, group = group, split = split),
                 args = list(method = method, alternative = alternative,
                             conf.level = conf.level, paired = paired,
                             sort.var = sort.var, na.omit = na.omit, digits = digits,
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

ci.prop.diff.formula <- function(formula, data, method = c("wald", "newcombe"),
                                 alternative = c("two.sided", "less", "greater"),
                                 conf.level = 0.95, group = NULL, split = NULL,
                                 sort.var = FALSE, na.omit = FALSE, digits = 2,
                                 as.na = NULL, check = TRUE, output = TRUE, ...) {

  #......
  # Check if input 'formula' is missing
  if (missing(formula) || is.null(formula)) {

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

  #-----------------------------------------------------------------------------------
  # Arguments

  #----------------------------------------
  # Method

  if (all(c("wald", "newcombe") %in% method)) { method <- "newcombe" }

  #----------------------------------------
  # Alternative hypothesis

  if (all(c("two.sided", "less", "greater") %in% alternative)) { alternative <- "two.sided" }

  #-----------------------------------------------------------------------------------
  # Main Function

  #----------------------------------------
  # No Grouping, No Split
  if (is.null(group) && is.null(split)) {

    result <- data.frame(matrix(NA, ncol = 10L, nrow = length(y.vars),
                                dimnames = list(NULL, c("variable", "n1", "nNA1", "p1", "n2", "nNA2", "p2", "p.diff", "low", "upp"))),
                         stringsAsFactors = FALSE)

    for (i in seq_along(y.vars)) {

      data.split <- split(data[, y.vars[i]], f = data[, group.var])

      result[i, ] <- data.frame(variable = y.vars[i],
                                ci.prop.diff.default(x = data.split[[1L]], y = data.split[[2L]], method = method,
                                                     paired = FALSE, alternative = alternative,
                                                     conf.level = conf.level, group = NULL, split = NULL, sort.var = sort.var,
                                                     digits = digits, as.na = NULL, check = check, output = FALSE)$result[, -1L],
                                stringsAsFactors = FALSE)

    }

  #----------------------------------------
  # Grouping, No Split
  } else if (!is.null(group) && is.null(split)) {

    object.group <- lapply(split(data[, var.formula], f = group),
                           function(y) ci.prop.diff(formula, data = y, method = method,
                                                           alternative = alternative,
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
                     function(y) ci.prop.diff(formula, data = y, method = method,
                                                     alternative = alternative, conf.level = conf.level,
                                                     group = NULL, split = NULL, sort.var = sort.var, na.omit = na.omit,
                                                     as.na = as.na, check = FALSE, output = FALSE)$result)

  #----------------------------------------
  # Grouping, Split
  } else if (!is.null(group) && !is.null(split)) {

    result <- lapply(split(data.frame(data[, var.formula], .group = group, stringsAsFactors = FALSE), f = split),
                     function(y) ci.prop.diff(formula, data = y, method = method,
                                                     alternative = alternative, conf.level = conf.level,
                                                     group = y$.group, split = NULL, sort.var = sort.var, na.omit = na.omit,
                                                     as.na = as.na, check = FALSE, output = FALSE)$result)

  }

  #-----------------------------------------------------------------------------------
  # Return object and output

  object <- list(call = match.call(),
                 type = "ci", ci = "prop.diff.i",
                 data = list(data = data[, var.formula], group = group, split = split),
                 args = list(formula = formula, method = method,
                             alternative = alternative, conf.level = conf.level,
                             sort.var = sort.var, na.omit = na.omit, digits = digits,
                             as.na = as.na, check = check, output = output),
                 result = result)

  class(object) <- "misty.object"

  #-----------------------------------------------------------------------------------
  # Output

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
