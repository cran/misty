#' t-Test
#'
#' This function performs one-sample, two-sample, and paired-sample t-tests.
#'
#' @param x           a numeric vector of data values.
#' @param y           a numeric vector of data values.
#' @param mu          a numeric value indicating the population mean under the null hypothesis. Note that
#'                    the argument \code{mu} is only used when computing a one sample t-test.
#' @param paired      logical: if \code{TRUE}, paired-samples t-test is computed.
#' @param alternative a character string specifying the alternative hypothesis, must be one of
#'                    \code{"two.sided"} (default), \code{"greater"} or \code{"less"}.
#' @param conf.level  a numeric value between 0 and 1 indicating the confidence level of the interval.
#' @param hypo        logical: if \code{TRUE}, null and alternative hypothesis are shown on the console.
#' @param descript    logical: if \code{TRUE}, descriptive statistics are shown on the console.
#' @param effsize     logical: if \code{TRUE}, effect size measure Cohen's d is shown on the console, see
#'                    \code{\link{cohens.d}} function.
#' @param weighted    logical: if \code{TRUE} (default), the weighted pooled standard deviation is used
#'                    to compute Cohen's d for a two-sample design (i.e., \code{paired = FALSE}), while
#'                    standard deviation of the difference scores is used to compute Cohen's d for a
#'                    paired-sample design (i.e., \code{paired = TRUE}).
#' @param cor         logical: if \code{TRUE} (default), \code{paired = TRUE}, and \code{weighted = FALSE},
#'                    Cohen's d for a paired-sample design while controlling for the correlation between
#'                    the two sets of measurement is computed. Note that this argument is only used in
#'                    a paired-sample design (i.e., \code{paired = TRUE}) when specifying \code{weighted = FALSE}.
#' @param ref         character string \code{"x"} or \code{"y"} for specifying the reference reference
#'                    group when using the default \code{test.t()} function or a numeric value or
#'                    character string indicating the reference group in a two-sample design when using
#'                    the formula \code{test.t()} function. The standard deviation of the reference variable
#'                    or reference group is used to standardized the mean difference to compute Cohen's d.
#'                    Note that this argument is only used in a two-sample design (i.e., \code{paired = FALSE}).
#' @param correct     logical: if \code{TRUE}, correction factor to remove positive bias in small samples is
#'                    used.
#' @param digits      an integer value indicating the number of decimal places to be used for displaying
#'                    descriptive statistics and confidence interval.
#' @param p.digits    an integer value indicating the number of decimal places to be used for displaying
#'                    the \emph{p}-value.
#' @param as.na       a numeric vector indicating user-defined missing values,
#'                    i.e. these values are converted to \code{NA} before conducting the analysis.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#' @param output      logical: if \code{TRUE}, output is shown on the console.
#' @param formula     in case of two sample t-test (i.e., \code{paired = FALSE}), a formula of the
#'                    form \code{y ~ group} where \code{group} is a numeric variable, character variable
#'                    or factor with two values or factor levels giving the corresponding groups.
#' @param data        a matrix or data frame containing the variables in the formula \code{formula}.
#' @param ...         further arguments to be passed to or from methods.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{test.welch}}, \code{\link{test.z}}, \code{\link{test.levene}}, \code{\link{cohens.d}},
#' \code{\link{ci.mean.diff}}, \code{\link{ci.mean}}
#'
#' @references
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
#'                    x = c(3, 1, 4, 2, 5, 3, 2, 3, 6, 6, 3, NA))
#'
#' #--------------------------------------
#' # One-Sample Design
#'
#' # Two-sided one-sample t-test
#' # population mean = 3
#' test.t(dat1$x, mu = 3)
#'
#' # One-sided one-sample t-test
#' # population mean = 3, population standard deviation = 1.2
#' test.t(dat1$x, mu = 3, alternative = "greater")
#'
#' # Two-sided one-sample t-test
#' # population mean = 3, convert value 3 to NA
#' test.t(dat1$x, mu = 3, as.na = 3)
#'
#' # Two-sided one-sample t-test
#' # population mean = 3, print Cohen's d
#' test.t(dat1$x, sigma = 1.2, mu = 3, effsize = TRUE)
#'
#' # Two-sided one-sample t-test
#' # population mean = 3, print Cohen's d with small sample correction factor
#' test.t(dat1$x, sigma = 1.2, mu = 3, effsize = TRUE, correct = TRUE)
#'
#' # Two-sided one-sample t-test
#' # population mean = 3,
#' # do not print hypotheses and descriptive statistics
#' test.t(dat1$x, sigma = 1.2, mu = 3, hypo = FALSE, descript = FALSE)
#'
#' # Two-sided one-sample t-test
#' # print descriptive statistics with 3 digits and p-value with 5 digits
#' test.t(dat1$x,  mu = 3, digits = 3, p.digits = 5)
#'
#' #--------------------------------------
#' # Two-Sample Design
#'
#' # Two-sided two-sample t-test
#' test.t(x ~ group, data = dat1)
#'
#' # One-sided two-sample t-test
#' test.t(x ~ group, data = dat1, alternative = "greater")
#'
#' # Two-sided two-sample t-test
#' # print Cohen's d with weighted pooled SD
#' test.t(x ~ group, data = dat1, effsize = TRUE)
#'
#' # Two-sided two-sample t-test
#' # print Cohen's d with unweighted pooled SD
#' test.t(x ~ group, data = dat1, effsize = TRUE, weighted = FALSE)
#'
#' # Two-sided two-sample t-test
#' # print Cohen's d with weighted pooled SD and
#' # small sample correction factor
#' test.t(x ~ group, data = dat1, effsize = TRUE, correct = TRUE)
#'
#' # Two-sided two-sample t-test
#' # print Cohen's d with SD of the reference group 1
#' test.t(x ~ group, data = dat1, effsize = TRUE,
#'        ref = 1)
#'
#' # Two-sided two-sample t-test
#' # print Cohen's d with weighted pooled SD and
#' # small sample correction factor
#' test.t(x ~ group, data = dat1, effsize = TRUE,
#'        correct = TRUE)
#'
#' # Two-sided two-sample t-test
#' # do not print hypotheses and descriptive statistics,
#' test.t(x ~ group, data = dat1, descript = FALSE, hypo = FALSE)
#'
#' # Two-sided two-sample t-test
#' # print descriptive statistics with 3 digits and p-value with 5 digits
#' test.t(x ~ group, data = dat1, digits = 3, p.digits = 5)
#'
#' #-----------------
#'
#' group1 <- c(3, 1, 4, 2, 5, 3, 6, 7)
#' group2 <- c(5, 2, 4, 3, 1)
#'
#' # Two-sided two-sample t-test
#' test.t(group1, group2)
#'
#' #--------------------------------------
#' # Paired-Sample Design
#'
#' dat2 <- data.frame(pre = c(1, 3, 2, 5, 7),
#'                    post = c(2, 2, 1, 6, 8), stringsAsFactors = FALSE)
#'
#' # Two-sided paired-sample t-test
#' test.t(dat2$pre, dat2$post, paired = TRUE)
#'
#' # One-sided paired-sample t-test
#' test.t(dat2$pre, dat2$post, paired = TRUE,
#'        alternative = "greater")
#'
#' # Two-sided paired-sample t-test
#' # convert value 1 to NA
#' test.t(dat2$pre, dat2$post, as.na = 1, paired = TRUE)
#'
#' # Two-sided paired-sample t-test
#' # print Cohen's d based on the standard deviation of the difference scores
#' test.t(dat2$pre, dat2$post, paired = TRUE, effsize = TRUE)
#'
#' # Two-sided paired-sample t-test
#' # print Cohen's d based on the standard deviation of the difference scores
#' # with small sample correction factor
#' test.t(dat2$pre, dat2$post, paired = TRUE, effsize = TRUE,
#'        correct = TRUE)
#'
#' # Two-sided paired-sample t-test
#' # print Cohen's d controlling for the correlation between measures
#' test.t(dat2$pre, dat2$post, paired = TRUE, effsize = TRUE,
#'        weighted = FALSE)
#'
#' # Two-sided paired-sample t-test
#' # print Cohen's d controlling for the correlation between measures
#' # with small sample correction factor
#' test.t(dat2$pre, dat2$post, paired = TRUE, effsize = TRUE,
#'        weighted = FALSE, correct = TRUE)
#'
#' # Two-sided paired-sample t-test
#' # print Cohen's d ignoring the correlation between measures
#' test.t(dat2$pre, dat2$post, paired = TRUE, effsize = TRUE,
#'        weighted = FALSE, cor = FALSE)
#'
#' # Two-sided paired-sample t-test
#' # do not print hypotheses and descriptive statistics
#' test.t(dat2$pre, dat2$post, paired = TRUE, hypo = FALSE, descript = FALSE)
#'
#' # Two-sided paired-sample t-test
#' # population standard deviation of difference score = 1.2
#' # print descriptive statistics with 3 digits and p-value with 5 digits
#' test.t(dat2$pre, dat2$post, paired = TRUE, digits = 3,
#'        p.digits = 5)
test.t <- function(x, ...) {

  UseMethod("test.t")

}

####################################################################################
# Default S3 method

test.t.default <- function(x, y = NULL, mu = 0, paired = FALSE,
                           alternative = c("two.sided", "less", "greater"),
                           conf.level = 0.95, hypo = TRUE, descript = TRUE,
                           effsize = FALSE, weighted = TRUE, cor = TRUE, ref = NULL,
                           correct = FALSE, digits = 2, p.digits = 4, as.na = NULL,
                           check = TRUE, output = TRUE, ...) {

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
  # Check input 'paired'
  if (isTRUE(!is.logical(paired))) {

    stop("Please specify TRUE or FALSE for the argument 'paired'.", call. = FALSE)

  }

  #......
  # Check if only one variable specified in the input 'x'
  if (ncol(data.frame(x)) != 1) {

    stop("More than one variable specified for the argument 'x'.",call. = FALSE)

  }

  #......
  # Convert 'x' into a vector
  x <- unlist(x, use.names = FALSE)

  if (!is.null(y)) {

    # Check if only one variable specified in the input 'y'
    if (ncol(data.frame(y)) != 1) {

      stop("More than one variable specified for the argument 'y'.",call. = FALSE)

    }

    #......
    # Convert 'y' into a vector
    y <- unlist(y, use.names = FALSE)

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
    # Check input 'conf.level'
    if (isTRUE(conf.level >= 1L || conf.level <= 0L)) {

      stop("Please specifiy a numeric value between 0 and 1 for the argument 'conf.level'.",
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
    # Check input 'weighted'
    if (isTRUE(!is.logical(weighted))) {

      stop("Please specify TRUE or FALSE for the argument 'weighted'.", call. = FALSE)

    }

    #......
    # Check input 'cor'
    if (isTRUE(!is.logical(cor))) {

      stop("Please specify TRUE or FALSE for the argument 'cor'.", call. = FALSE)

    }

    #......
    # Check input 'ref'
    if (isTRUE(!is.null(ref))) {

      if (isTRUE(!isTRUE(ref %in% c("x", "y")))) {

        stop("Please specify \"x\" or \"y\" for the argument 'ref'.", call. = FALSE)

      }

    }

    #......
    # Check input 'correct'
    if (isTRUE(!is.logical(correct))) {

      stop("Please specify TRUE or FALSE for the argument 'correct'.", call. = FALSE)

    }

    #......
    # Check input 'digits'
    if (isTRUE(digits %% 1L != 0L || digits < 0L)) {

      stop("Please specify a positive integer number for the argument 'digits'.", call. = FALSE)

    }

    #......
    # Check input 'digits'
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
  # Alternative hypothesis

  if (isTRUE(all(c("two.sided", "less", "greater") %in% alternative))) { alternative <- "two.sided" }

  #-----------------------------------------------------------------------------------
  # Main Function

  #...
  # One-sample
  if (isTRUE(is.null(y))) {

    # Confidence intervals
    x.ci <- misty::ci.mean(x = x, alternative = alternative,
                           conf.level = conf.level, check = FALSE, output = FALSE)$result

    # Cohen's d
    d <- misty::cohens.d(x = x, y = NULL, paired = FALSE, mu = mu,
                         weighted = FALSE, cor = TRUE, ref = NULL, correct = correct,
                         alternative = alternative, conf.level = conf.level,
                         group = NULL, split = NULL, sort.var = FALSE,
                         check = FALSE, output = FALSE)$result

    # t-test
    t <- t.test(x = x, y = y, mu = mu, alternative = alternative)

    result <- data.frame(n = x.ci[["n"]],
                         nNA = x.ci[["nNA"]],
                         m = x.ci[["m"]],
                         sd = x.ci[["sd"]],
                         m.diff = x.ci[["m"]] - mu,
                         se = t$stderr,
                         t = t$statistic,
                         df = t$parameter,
                         pval = t$p.value,
                         d = d$d,
                         low = d$low,
                         upp = d$upp)

    sample <- "one"

  #...
  # Two samples
  } else if (isTRUE(!is.null(y) && !isTRUE(paired))) {

    # Confidence intervals
    x.ci <- misty::ci.mean.diff(x = x, y = y, alternative = alternative,
                                conf.level = conf.level, output = FALSE)$result

    # Cohen's d
    d <- misty::cohens.d(x = x, y = y, paired = FALSE, mu = 0,
                         weighted = weighted, cor = TRUE, ref = ref, correct = correct,
                         alternative = alternative, conf.level = conf.level,
                         group = NULL, split = NULL, sort.var = FALSE,
                         check = FALSE, output = FALSE)$result
    # t-test
    t <- t.test(x = x, y = y, alternative = switch(alternative,
                                                   two.sided = "two.sided",
                                                   greater = "less",
                                                   less = "greater"), var.equal = TRUE)

    result <- data.frame(cbind(x.ci[, -which(colnames(x.ci) %in% c("variable", "between", "low", "upp"))],
                         se = c(NA, t$stderr),
                         t = c(NA, t$statistic)*-1,
                         df = c(NA, t$parameter),
                         pval = c(NA, t$p.value),
                         d = d$d,
                         low = d$low,
                         upp = d$upp), row.names = NULL)

    sample <- "two"

  #...
  # Paired samples
  } else if (isTRUE(!is.null(y) && isTRUE(paired))) {

    # Confidence intervals
    x.ci <- misty::ci.mean.diff(x = x, y = y,  paired = TRUE,
                                alternative = alternative, conf.level = conf.level, output = FALSE)$result

    # Cohen's d
    d <- misty::cohens.d(x = x, y = y, paired = TRUE, mu = 0,
                         weighted = weighted, cor = cor, ref = ref, correct = correct,
                         alternative = alternative, conf.level = conf.level,
                         group = NULL, split = NULL, sort.var = FALSE,
                         check = FALSE, output = FALSE)$result

    # t-test
    t <- t.test(x = x, y = y, alternative = switch(alternative,
                                                   two.sided = "two.sided",
                                                   greater = "less",
                                                   less = "greater"), paired = TRUE)

    result <- data.frame(n = x.ci[["n"]],
                         nNA = x.ci[["nNA"]],
                         m1 = x.ci[["m1"]],
                         m2 = x.ci[["m2"]],
                         m.diff = x.ci[["m.diff"]],
                         sd.diff = x.ci[["sd.diff"]],
                         se = t$stderr,
                         t = t$statistic*-1,
                         df = t$parameter,
                         pval = t$p.value,
                         d = d$d,
                         low = d$low,
                         upp = d$upp)

    sample <- "paired"

  }

  ####################################################################################
  # Return object and output

  object <- list(call = match.call(),
                 type = "test.t",
                 sample = sample,
                 data = list(x, y),
                 args = list(mu = mu, paired = paired, alternative = alternative,
                             conf.level = conf.level,  hypo = hypo, descript = descript,
                             effsize = effsize, weighted = weighted, cor = cor,
                             ref = ref, correct = correct, digits = digits, p.digits = p.digits,
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

test.t.formula <- function(formula, data, alternative = c("two.sided", "less", "greater"),
                           conf.level = 0.95, hypo = TRUE, descript = TRUE,
                           effsize = FALSE, weighted = TRUE, cor = TRUE, ref = NULL,
                           correct = FALSE, digits = 2, p.digits = 4, as.na = NULL,
                           check = TRUE, output = TRUE, ...) {

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

  #----------------------------------------
  # Convert user-missing values into NA

  if (isTRUE(!is.null(as.na))) {

    # Replace user-specified values with missing values
    data[, y.vars] <- misty::as.na(data[, y.vars], na = as.na, check = check)

    # Variable with missing values only
    data.miss <- vapply(data[, y.vars, drop = FALSE], function(y) all(is.na(y)), FUN.VALUE = logical(1))
    if (any(data.miss)) {

      stop(paste0("After converting user-missing values into NA, following variables are completely missing: ",
                  paste(names(which(data.miss)), collapse = ", ")), call. = FALSE)

    }

  }

  #-----------------------------------------------------------------------------------
  # Arguments

  #----------------------------------------
  # Alternative hypothesis
  if (isTRUE(all(c("two.sided", "less", "greater") %in% alternative))) { alternative <- "two.sided" }

  #----------------------------------------
  # Reference group

  ref.return <- ref

  #......
  # Check if input 'data' is NULL
  if (isTRUE(!is.null(ref))) {

    if (isTRUE(!ref %in% na.omit(unlist(data[, group.var])))) {

      stop("Reference group specified in the argument 'ref' is not represented in the grouping variable.",
           call. = FALSE)

    }

    ifelse(which(unique(sort(na.omit(unlist(data[, group.var])))) %in% ref) == 1, ref <- "x", ref <- "y")

  }

  #-----------------------------------------------------------------------------------
  # Main Function

  data.split <- split(unlist(data[, y.vars]), f = unlist(data[, group.var]))

  object <- test.t.default(x = data.split[[1L]], y = data.split[[2L]],
                           alternative = alternative, conf.level = conf.level,
                           weighted = weighted, cor = cor, ref = ref, correct = correct,
                           output = FALSE)

  ####################################################################################
  # Return object and output

  object <- list(call = match.call(),
                 type = "test.t",
                 sample = "two",
                 data = data[, var.formula],
                 args = list(formula = formula, alternative = alternative,
                             conf.level = conf.level, hypo = hypo, descript = descript,
                             effsize = effsize, weighted = weighted, cor = cor,
                             ref = ref.return, correct = correct, digits = digits,
                             p.digits = p.digits, as.na = as.na, check = check, output = output),
                 result = object$result)

  class(object) <- "misty.object"

  #-----------------------------------------------------------------------------------
  # Output

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
