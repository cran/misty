#' Confidence Interval for the Arithmetic Mean
#'
#' This function computes a confidence interval for the arithmetic mean with known
#' or unknown population standard deviation or population variance for one or more
#' variables, optionally by a grouping and/or split variable.
#'
#' A difference-adjusted confidence interval (Baguley, 2012) can be computed by
#' specifying \code{adjust = TRUE}.
#'
#' @param x           a numeric vector, matrix or data frame with numeric variables,
#'                    i.e., factors and character variables are excluded from \code{x}
#'                    before conducting the analysis.
#' @param sigma       a numeric vector indicating the population standard deviation
#'                    when computing confidence intervals for the arithmetic mean
#'                    with known standard deviation Note that either argument
#'                    \code{sigma} or argument \code{sigma2} is specified and it
#'                    is only possible to specify one value for the argument
#'                    \code{sigma} even though multiple variables are specified
#'                    in \code{x}.
#' @param sigma2      a numeric vector indicating the population variance when
#'                    computing confidence intervals for the arithmetic mean with
#'                    known variance. Note that either argument \code{sigma}
#'                    or argument \code{sigma2} is specified and it is only possible
#'                    to specify one value for the argument \code{sigma2} even
#'                    though multiple variables are specified in \code{x}.
#' @param adjust      logical: if \code{TRUE} (default), difference-adjustment
#'                    for the confidence intervals is applied.
#' @param alternative a character string specifying the alternative hypothesis,
#'                    must be one of \code{"two.sided"} (default), \code{"greater"}
#'                    or \code{"less"}.
#' @param conf.level  a numeric value between 0 and 1 indicating the confidence
#'                    level of the interval.
#' @param group       a numeric vector, character vector or factor as grouping
#'                    variable. Note that a grouping variable can only be used
#'                    when computing confidence intervals with unknown population
#'                    standard deviation and population variance.
#' @param split       a numeric vector, character vector or factor as split variable.
#'                    Note that a split variable can only be used when computing
#'                    confidence intervals with unknown population standard deviation
#'                    and population variance.
#' @param sort.var    logical: if \code{TRUE}, output table is sorted by variables
#'                    when specifying \code{group}.
#' @param na.omit     logical: if \code{TRUE}, incomplete cases are removed before
#'                    conducting the analysis (i.e., listwise deletion) when
#'                    specifying more than one outcome variable.
#' @param digits      an integer value indicating the number of decimal places to
#'                    be used.
#' @param as.na       a numeric vector indicating user-defined missing values,
#'                    i.e. these values are converted to \code{NA} before conducting
#'                    the analysis. Note that \code{as.na()} function is only
#'                    applied to \code{x}, but not to \code{group} or \code{split}.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#' @param output      logical: if \code{TRUE}, output is shown on the console.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{test.z}}, \code{\link{test.t}}, \code{\link{ci.mean.diff}},
#' \code{\link{ci.median}}, \code{\link{ci.prop}}, \code{\link{ci.var}},
#' \code{\link{ci.sd}}, \code{\link{descript}}
#'
#' @references
#' Baguley, T. S. (2012). \emph{Serious stats: A guide to advanced statistics for
#' the behavioral sciences}. Palgrave Macmillan.
#'
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology
#' - Using R and SPSS}. John Wiley & Sons.
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#' \tabular{ll}{
#' \code{call} \tab function call \cr
#' \code{type} \tab type of analysis \cr
#' \code{data} \tab list with the input specified in \code{x}, \code{group}, and \code{split} \cr
#' \code{args} \tab specification of function arguments \cr
#' \code{result} \tab result table \cr
#' }
#'
#' @export
#'
#' @examples
#' dat <- data.frame(group1 = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2),
#'                   group2 = c(1, 1, 1, 2, 2, 2, 1, 1, 1, 2, 2, 2),
#'                   x1 = c(3, 1, 4, 2, 5, 3, 2, 4, NA, 4, 5, 3),
#'                   x2 = c(4, NA, 3, 6, 3, 7, 2, 7, 5, 1, 3, 6),
#'                   x3 = c(7, 8, 5, 6, 4, NA, 8, NA, 6, 5, 8, 6))
#'
#' # Two-Sided 95% Confidence Interval for x1
#' ci.mean(dat$x1)
#'
#' # Two-Sided 95% Difference-Adjusted Confidence Interval for x1
#' ci.mean(dat$x1, adjust = TRUE)
#'
#' # Two-Sided 95% Confidence Interval with known standard deviation for x1
#' ci.mean(dat$x1, sigma = 1.2)
#'
#' # Two-Sided 95% Confidence Interval with known variance for x1
#' ci.mean(dat$x1, sigma2 = 2.5)
#'
#' # One-Sided 95% Confidence Interval for x1
#' ci.mean(dat$x1, alternative = "less")
#'
#' # Two-Sided 99% Confidence Interval
#' ci.mean(dat$x1, conf.level = 0.99)
#'
#' # Two-Sided 95% Confidence Interval, print results with 3 digits
#' ci.mean(dat$x1, digits = 3)
#'
#' # Two-Sided 95% Confidence Interval for x1, convert value 4 to NA
#' ci.mean(dat$x1, as.na = 4)
#'
#' # Two-Sided 95% Confidence Interval for x1, x2, and x3,
#' # listwise deletion for missing data
#' ci.mean(dat[, c("x1", "x2", "x3")], na.omit = TRUE)
#'
#' # Two-Sided 95% Confidence Interval for x1, x2, and x3,
#' # analysis by group1 separately
#' ci.mean(dat[, c("x1", "x2", "x3")], group = dat$group1)
#'
#' # Two-Sided 95% Confidence Interval for x1, x2, and x3,
#' # analysis by group1 separately, sort by variables
#' ci.mean(dat[, c("x1", "x2", "x3")], group = dat$group1, sort.var = TRUE)
#'
#' # Two-Sided 95% Confidence Interval for x1, x2, and x3,
#' # split analysis by group1
#' ci.mean(dat[, c("x1", "x2", "x3")], split = dat$group1)
#'
#' # Two-Sided 95% Confidence Interval for x1, x2, and x3,
#' # analysis by group1 separately, split analysis by group2
#' ci.mean(dat[, c("x1", "x2", "x3")], group = dat$group1, split = dat$group2)
ci.mean <- function(x, sigma = NULL, sigma2 = NULL, adjust = FALSE,
                    alternative = c("two.sided", "less", "greater"), conf.level = 0.95,
                    group = NULL, split = NULL, sort.var = FALSE, na.omit = FALSE,
                    digits = 2, as.na = NULL, check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------


  # Check if input 'x' is missing
  if (isTRUE(missing(x))) { stop("Please specify a numeric vector, matrix or data frame with numeric variables for the argument 'x'.", call. = FALSE) }

  # Check if input 'x' is NULL
  if (isTRUE(is.null(x))) { stop("Input specified for the argument 'x' is NULL.", call. = FALSE) }

  # Check 'group'
  if (isTRUE(!is.null(group))) {

    if (ncol(data.frame(group)) != 1L) { stop("More than one grouping variable specified for the argument 'group'.", call. = FALSE) }

    if (nrow(data.frame(group)) != nrow(data.frame(x))) { stop("Length of the vector or factor specified in the argument 'group' does not match with 'x'.", call. = FALSE) }

    # Convert group into a vector
    group <- unlist(group, use.names = FALSE)

  }

  # Check 'split'
  if (isTRUE(!is.null(split))) {

    if (ncol(data.frame(split)) != 1L) { stop("More than one split variable specified for the argument 'split'.",call. = FALSE) }

    if (nrow(data.frame(split)) != nrow(data.frame(x))) { stop("Length of the vector or factor specified in the argument 'split' does not match with 'x'.", call. = FALSE) }

    # Convert 'split' into a vector
    split <- unlist(split, use.names = FALSE)

  }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data Frame ####

  x <- as.data.frame(x, stringsAsFactors = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) {

    # Replace user-specified values with missing values
    x <- misty::as.na(x, na = as.na, check = check)

    # Variable with missing values only
    x.miss <- vapply(x, function(y) all(is.na(y)), FUN.VALUE = logical(1L))
    if (isTRUE(any(x.miss))) {

      stop(paste0("After converting user-missing values into NA, following variables are completely missing: ",
                  paste(names(which(x.miss)), collapse = ", ")), call. = FALSE)

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Numeric Variables ####

  # Non-numeric variables
  non.num <- !vapply(x, is.numeric, FUN.VALUE = logical(1))

  if (isTRUE(any(non.num))) {

    x <- x[, -which(non.num), drop = FALSE]

    # Variables left
    if (isTRUE(ncol(x) == 0L)) { stop("No variables left for analysis after excluding non-numeric variables.", call. = FALSE) }

    warning(paste0("Non-numeric variables were excluded from the analysis: ", paste(names(which(non.num)), collapse = ", ")), call. = FALSE)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Listwise deletion ####

  if (isTRUE(na.omit && any(is.na(x)))) {

    #...................
    ### No group and split variable ####
    if (isTRUE(is.null(group) && is.null(split))) {

      x <- na.omit(as.data.frame(x, stringsAsFactors = FALSE))

      warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ",
                     length(attributes(x)$na.action)), call. = FALSE)

    }

    #...................
    ### Group variable, no split variable ####
    if (isTRUE(!is.null(group) && is.null(split))) {

      x.group <- na.omit(data.frame(x, group = group, stringsAsFactors = FALSE))

      x <- x.group[, -grep("group", names(x.group)), drop = FALSE]
      group <- x.group$group

      warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ",
                     length(attributes(x.group)$na.action)), call. = FALSE)

    }

    #...................
    ### No group variable, split variable ####
    if (isTRUE(is.null(group) && !is.null(split))) {

      x.split <- na.omit(data.frame(x, split = split, stringsAsFactors = FALSE))

      x <- x.split[, -grep("split", names(x.split)), drop = FALSE]
      split <- x.split$split

      warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ",
                     length(attributes(x.split)$na.action)), call. = FALSE)

    }

    #...................
    ### Group variable, split variable ####
    if (isTRUE(!is.null(group) && !is.null(split))) {

      x.group.split <- na.omit(data.frame(x, group = group, split = split, stringsAsFactors = FALSE))

      x <- x.group.split[,  !names(x.group.split) %in% c("group", "split"), drop = FALSE]
      group <- x.group.split$group
      split <- x.group.split$split

      warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ",
                     length(attributes(x.group.split)$na.action)), call. = FALSE)

    }

    #...................
    ### Variable with missing values only ####
    x.miss <- vapply(x, function(y) all(is.na(y)), FUN.VALUE = logical(1L))
    if (isTRUE(any(x.miss))) {

      stop(paste0("After listwise deletion, following variables are completely missing: ",
                  paste(names(which(x.miss)), collapse = ", ")), call. = FALSE)

    }

  }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(isTRUE(check))) {

    # Check input 'sigma' and 'sigma2'
    if (isTRUE(!is.null(sigma) && !is.null(sigma2))) { stop("Please specify either argument 'sigma' or argument 'sigma2', but not both.", call. = FALSE) }

    # Check input 'sigma'
    if (isTRUE(!is.null(sigma))) {

      # SD smaller or equal 0
      if (isTRUE(any(sigma <= 0L))) { stop("Please specify a numeric value grater than 0 for the argument 'sigma'.", call. = FALSE) }

      # Length of 'sigma'
      if (isTRUE(length(sigma) != 1L)) { stop("Please specify a numeric value for the argument 'sigma'.", call. = FALSE) }

    }

    # Check input 'sigma2'
    if (isTRUE(!is.null(sigma2))) {

      # Variance smaller or equal 0
      if (isTRUE(any(sigma2 <= 0L))) { stop("Please specify a numeric value grater than 0 for the argument 'sigma2'.", call. = FALSE) }

      # Length of 'sigma2'
      if (isTRUE(length(sigma2) != 1L)) { stop("Please specify a numeric value for the argument 'sigma2'.", call. = FALSE) }

    }

    # Check input 'adjust'
    if (isTRUE(!is.logical(adjust))) { stop("Please specify TRUE or FALSE for the argument 'adjust'.", call. = FALSE) }

    # Check input 'alternative'
    if (isTRUE(!all(alternative %in% c("two.sided", "less", "greater")))) {

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

    # Check input 'na.omit'
    if (isTRUE(!is.logical(na.omit))) { stop("Please specify TRUE or FALSE for the argument 'na.omit'.", call. = FALSE) }

    # Check input 'digits'
    if (isTRUE(digits %% 1L != 0L || digits < 0L)) { stop("Please specify a positive integer number for the argument 'digits'.", call. = FALSE) }

    # Check input 'output'
    if (isTRUE(!is.logical(output))) { stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Population standard deviation and variance ####

  if (isTRUE(is.null(sigma) && !is.null(sigma2))) { sigma <- sqrt(sigma2) }

  if (isTRUE(!is.null(sigma) && is.null(sigma2))) { sigma2 <- sigma^2 }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Alternative hypothesis ####

  if (isTRUE(all(c("two.sided", "less", "greater") %in% alternative))) {

    alternative <- "two.sided"

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Confidence interval for the mean ####

  m.conf <- function(x, sigma, adjust, alternative, conf.level, side) {

    # Data
    x <- na.omit(x)

    # Difference-adjustment factor
    adjust.factor <- ifelse(isTRUE(adjust), sqrt(2L) / 2L, 1L)

    # One observation or SD = 0
    if (isTRUE(length(x) <= 1L || sd(x) == 0L)) {

      ci <- c(NA, NA)

    # More than one observation
    } else {

      x.m <- mean(x)

      #...................
      ### Known population standard deviation ####

      if (isTRUE(!is.null(sigma))) {

        crit <- qnorm(switch(alternative,
                             two.sided = 1L - (1L - conf.level) / 2L,
                             less = conf.level,
                             greater = conf.level))

        se <- sigma / sqrt(length(na.omit(x)))

      #...................
      ### Unknown population standard deviation ####
      } else {

        crit <- qt(switch(alternative,
                          two.sided = 1L - (1L - conf.level) / 2L,
                          less = conf.level,
                          greater = conf.level), df = length(x) - 1L)

        se <- sd(x) / sqrt(length(x))

      }

      #...................
      ### Confidence interval ####
      ci <- switch(alternative,
                   two.sided = c(low = x.m - adjust.factor * crit * se,
                                 upp = x.m + adjust.factor * crit * se),
                   less = c(low = -Inf,
                            upp = x.m + adjust.factor * crit * se),
                   greater = c(low = x.m - adjust.factor * crit * se,
                               upp = Inf))

    }

    #...................
    ### Return object ####
    object <- switch(side, both = ci, low = ci[1L], upp = ci[2L])

    return(object)

  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## No Grouping, No Split ####
  if (isTRUE(is.null(group) && is.null(split))) {

    result <- data.frame(variable = colnames(x),
                         n = vapply(x, function(y) length(na.omit(y)), FUN.VALUE = 1L),
                         nNA = vapply(x, function(y) sum(is.na(y)), FUN.VALUE = 1L),
                         pNA = vapply(x, function(y) sum(is.na(y)) / length(y) * 100, FUN.VALUE = double(1)),
                         # Arithmetic mean
                         m = vapply(x, function(y) ifelse(length(na.omit(y)) <= 1L, NA, mean(y, na.rm = TRUE)), FUN.VALUE = double(1L)),
                         # Standard deviation
                         sd = vapply(x, function(y) ifelse(length(na.omit(y)) <= 1L, NA, sd(y, na.rm = TRUE)), FUN.VALUE = double(1L)),
                         # Confidence interval for the arithmetic mean
                         low = vapply(x, m.conf, sigma = sigma, adjust = adjust, alternative = alternative, conf.level = conf.level, side = "low", FUN.VALUE = double(1L)),
                         upp = vapply(x, m.conf, sigma = sigma, adjust = adjust, alternative = alternative, conf.level = conf.level, side = "upp", FUN.VALUE = double(1L)),
                         stringsAsFactors = FALSE, row.names = NULL, check.names = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##  Grouping, No Split ####
  } else if (isTRUE(!is.null(group) && is.null(split))) {

    object.group <- lapply(split(x, f = group), function(y) misty::ci.mean(y, sigma = NULL, sigma2 = NULL, adjust = adjust, alternative = alternative, conf.level = conf.level,
                                                                           group = NULL, split = NULL, sort.var = sort.var, na.omit = na.omit,
                                                                           as.na = as.na, check = FALSE, output = FALSE)$result)

    result <- data.frame(group = rep(names(object.group), each = ncol(x)),
                         eval(parse(text = paste0("rbind(", paste0("object.group[[", seq_len(length(object.group)), "]]", collapse = ", "), ")"))),
                         stringsAsFactors = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## No Grouping, Split ####
  } else if (isTRUE(is.null(group) && !is.null(split))) {

    result <- lapply(split(data.frame(x, stringsAsFactors = FALSE), f = split),
                     function(y) misty::ci.mean(y, sigma = NULL, sigma2 = NULL, adjust = adjust, alternative = alternative, conf.level = conf.level,
                                                group = NULL, split = NULL, sort.var = sort.var, na.omit = na.omit,
                                                as.na = as.na, check = FALSE, output = FALSE)$result)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Grouping, Split ####
  } else if (isTRUE(!is.null(group) && !is.null(split))) {

    result <- lapply(split(data.frame(x, group = group, stringsAsFactors = FALSE), f = split),
                     function(y) misty::ci.mean(y[, -grep("group", names(y))], sigma = NULL, sigma2 = NULL,
                                                adjust = adjust, alternative = alternative, conf.level = conf.level,
                                                group = y$group, split = NULL, sort.var = sort.var,
                                                na.omit = na.omit, as.na = as.na,
                                                check = FALSE, output = FALSE)$result)

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "ci", ci = "mean",
                 data = list(x = x, group = group, split = split),
                 args = list(sigma = sigma, sigma2 = sigma2, adjust = adjust,
                             alternative = alternative, conf.level = conf.level,
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
