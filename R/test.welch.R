#' Welch's Test
#'
#' This function performs Welch's two-sample t-test and Welch's ANOVA.
#'
#' @param formula     a formula of the form \code{y ~ group} where \code{y} is a numeric variable
#'                    giving the data values and \code{group} a numeric variable, character variable
#'                    or factor with two or more than two values or factor levelsgiving the
#'                    corresponding groups.
#' @param data        a matrix or data frame containing the variables in the formula \code{formula}.
#' @param alternative a character string specifying the alternative hypothesis, must be one of
#'                    code{"two.sided"} (default), \code{"greater"} or \code{"less"}. Note that this
#'                    argument is only used when conducting Welch's two-sample t-test.
#' @param conf.level  a numeric value between 0 and 1 indicating the confidence level of the interval
#'                    for Cohen's d.
#'                    Note that this argument is only used when conducting Welch's two-sample t-test.
#' @param hypo        logical: if \code{TRUE}, null and alternative hypothesis are shown on the console.
#' @param descript    logical: if \code{TRUE}, descriptive statistics are shown on the console.
#' @param effsize     logical: if \code{TRUE}, effect size measure Cohen's d for Welch's two-sample t-test
#'                    (see \code{\link{cohens.d}}), \eqn{\eta^2} and \eqn{\omega^2} for Welch's ANOVA are
#'                    shown on the console.
#' @param weighted    logical: if \code{TRUE} (default), the weighted pooled standard deviation is used
#'                    to compute Cohen's d for a two-sample design (i.e., \code{paired = FALSE}), while
#'                    standard deviation of the difference scores is used to compute Cohen's d for a
#'                    paired-sample design (i.e., \code{paired = TRUE}).
#' @param ref         character string \code{"x"} or \code{"y"} for specifying the reference reference
#'                    group when using the default \code{test.t()} function or a numeric value or
#'                    character string indicating the reference group in a two-sample design when using
#'                    the formula \code{test.t()} function. The standard deviation of the reference variable
#'                    or reference group is used to standardized the mean difference to compute Cohen's d.
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
#' @param ...         further arguments to be passed to or from methods.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{test.t}}, \code{\link{test.z}}, \code{\link{test.levene}}, \code{\link{cohens.d}},
#' \code{\link{ci.mean.diff}}, \code{\link{ci.mean}}
#'
#' @references
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology - Using R and SPSS}.
#' John Wiley & Sons.
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following entries:
#' function call (\code{call}), type of analysis \code{type}, list with the input specified in \code{x}
#' (\code{data}), specification of function arguments (\code{args}), and result table(s) (\code{result}).
#'
#' @export
#'
#' @examples
#' dat1 <- data.frame(group1 = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2),
#'                    group2 = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3),
#'                    x = c(3, 1, 4, 2, 5, 3, 2, 3, 6, 6, 3, NA))
#'
#' #--------------------------------------
#' # Two-Sample Design
#'
#' # Two-sided two-sample Welch-test
#' test.welch(x ~ group1, data = dat1)
#'
#' # One-sided two-sample Welch-test
#' test.welch(x ~ group1, data = dat1, alternative = "greater")
#'
#' # Two-sided two-sample Welch-test
#' # print Cohen's d with weighted pooled SD
#' test.welch(x ~ group1, data = dat1, effsize = TRUE)
#'
#' # Two-sided two-sample Welch-test
#' # print Cohen's d with unweighted pooled SD
#' test.welch(x ~ group1, data = dat1, effsize = TRUE, weighted = FALSE)
#'
#' # Two-sided two-sample Welch-test
#' # print Cohen's d with weighted pooled SD and
#' # small sample correction factor
#' test.welch(x ~ group1, data = dat1, effsize = TRUE, correct = TRUE)
#'
#' # Two-sided two-sample Welch-test
#' # print Cohen's d with SD of the reference group 1
#' test.welch(x ~ group1, data = dat1, effsize = TRUE,
#'            ref = 1)
#'
#' # Two-sided two-sample Welch-test
#' # print Cohen's d with weighted pooled SD and
#' # small sample correction factor
#' test.welch(x ~ group1, data = dat1, effsize = TRUE,
#'            correct = TRUE)
#'
#' # Two-sided two-sample Welch-test
#' # do not print hypotheses and descriptive statistics,
#' test.welch(x ~ group1, data = dat1, descript = FALSE, hypo = FALSE)
#'
#' # Two-sided two-sample Welch-test
#' # print descriptive statistics with 3 digits and p-value with 5 digits
#' test.welch(x ~ group1, data = dat1, digits = 3, p.digits = 5)
#'
#' #--------------------------------------
#' # Multiple-Sample Design
#'
#' # Welch's ANOVA
#' test.welch(x ~ group2, data = dat1)
#'
#' # Welch's ANOVA
#' # print eta-squared and omega-squared
#' test.welch(x ~ group2, data = dat1, effsize = TRUE)
#'
#' # Welch's ANOVA
#' # do not print hypotheses and descriptive statistics,
#' test.welch(x ~ group2, data = dat1, descript = FALSE, hypo = FALSE)
test.welch <- function(formula, data, alternative = c("two.sided", "less", "greater"),
                       conf.level = 0.95, hypo = TRUE, descript = TRUE, effsize = FALSE,
                       weighted = TRUE, ref = NULL, correct = FALSE, digits = 2,
                       p.digits = 4, as.na = NULL, check = TRUE, output = TRUE, ...) {

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

  # Outcome variable
  y.var <- var.formula[-grep(group.var, var.formula)]

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
    if (isTRUE(length(y.var) != 1L)) {

      stop("Please specify a formula with only one outcome variable.", call. = FALSE)

    }

    #......
    # Check input 'descript'
    if (isTRUE(!is.logical(descript))) {

      stop("Please specify TRUE or FALSE for the argument 'descript'.", call. = FALSE)

    }

    #......
    # Check input 'alternative'
    if (isTRUE(!all(alternative %in%  c("two.sided", "less", "greater")))) {

      stop("Character string in the argument 'alternative' does not match with \"two.sided\", \"less\", or \"greater\".",
           call. = FALSE)

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
    # Check input 'correct'
    if (isTRUE(!is.logical(correct))) {

      stop("Please specify TRUE or FALSE for the argument 'correct'.", call. = FALSE)

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

  #----------------------------------------
  # Convert user-missing values into NA

  if (isTRUE(!is.null(as.na))) {

    # Replace user-specified values with missing values
    data[, y.var] <- misty::as.na(data[, y.var], na = as.na, check = check)

    # Variable with missing values only
    data.miss <- vapply(data[, y.var, drop = FALSE], function(y) all(is.na(y)), FUN.VALUE = logical(1))
    if (any(data.miss)) {

      stop(paste0("After converting user-missing values into NA, following variables are completely missing: ",
                  paste(names(which(data.miss)), collapse = ", ")), call. = FALSE)

    }

  }

  #-----------------------------------------------------------------------------------
  # Data

  # Outcome
  y <- data[, y.var]

  # Grouping
  group <- data[, group.var]

  ####################################################################################
  # Arguments

  #----------------------------------------
  # Alternative hypothesis

  if (isTRUE(all(c("two.sided", "less", "greater") %in% alternative))) { alternative <- "two.sided" }

  #-----------------------------------------------------------------------------------
  # Main Function

  #......................
  # Two-sample Welch test
  if (isTRUE(length(unique(group)) == 2)) {

    # Descriptive statistics
    ci <- misty::ci.mean.diff(formula = formula, data = data,  paired = FALSE,
                              alternative = alternative, conf.level = conf.level,
                              output = FALSE)$result

    # Cohen's d
    d <- misty::cohens.d(formula = formula, data = data, paired = FALSE, mu = 0,
                         weighted = weighted, cor = TRUE,  ref = ref, correct = correct,
                         alternative = alternative, conf.level = conf.level,
                         group = NULL, split = NULL, sort.var = FALSE,
                         check = FALSE, output = FALSE)$result

    # Welch's test for two groups
    welch <- t.test(formula = formula, data = data,
                    alternative = switch(alternative,
                                         two.sided = "two.sided",
                                         greater = "less",
                                         less = "greater"),
                    paired = FALSE, var.equal = FALSE)

    result <- data.frame(cbind(ci[, -which(colnames(ci) %in% c("variable", "between", "low", "upp"))],
                               se = c(NA, welch$stderr),
                               t = c(NA, welch$statistic)*-1,
                               df = c(NA, welch$parameter),
                               pval = c(NA, welch$p.value),
                               d = d$d,
                               low = d$low,
                               upp = d$upp), row.names = NULL)

    sample <- "two"

  #......................
  # Welch's test for more than two groups
  } else {

    # Descriptive statistics
    ci <- misty::ci.mean(y, group = group, output = FALSE)$result[, -c(2, 5)]

    # ANOVA table
    aov.table <- summary(aov(y ~ factor(group)))[[1]]

    ss.m <- aov.table[["Sum Sq"]][1]
    df.m <- aov.table[["Df"]][1]

    ms.r <- aov.table[["Mean Sq"]][2]
    ss.t <- sum(aov.table[["Sum Sq"]])

    # Eta squared
    eta.sq <- ss.m / ss.t

    # Omega squared
    omega.sq<- (ss.m - df.m*ms.r) / (ss.t + ms.r)
    omega.sq <- ifelse(omega.sq < 0, 0, omega.sq)

    # Welch's ANOVA
    welch <- oneway.test(formula = formula, data = data, var.equal = FALSE)

    result <- list(descript = ci,
                   test = data.frame(F = welch$statistic,
                                     df1 = welch$parameter["num df"],
                                     df2 = welch$parameter["denom df"],
                                     pval = welch$p.value,
                                     eta.sq = eta.sq, omega.sq = omega.sq, row.names = NULL))

    sample <- "multiple"

  }

  ####################################################################################
  # Return object and output

  object <- list(call = match.call(),
                 type = "test.welch",
                 sample = sample,
                 data = data.frame(y, group, stringsAsFactors = FALSE),
                 args = list(formula = formula, alternative = alternative,
                             conf.level = conf.level,  hypo = hypo,
                             descript = descript, digits = digits,
                             effsize = effsize, weighted = weighted,
                             ref = ref, correct = correct,
                             p.digits = p.digits, as.na = as.na,
                             check = check, output = output),
                 result = result)

  class(object) <- "misty.object"

  #-----------------------------------------------------------------------------------
  # Output

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
