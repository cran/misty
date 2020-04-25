#' Confidence Interval for the Standard Deviation
#'
#' This function computes a confidence interval for the standard deviation for one or more variables, optionally
#' by a grouping and/or split variable.
#'
#' The confidence interval based on the chi-square distribution is computed by specifying \code{method = "chisq"},
#' while the Bonett (2006) confidence interval is requested by specifying \code{method = "bonett"}. By default,
#' the Bonett confidence interval interval is computed which performs well under moderate departure from
#' normality, while the confidence interval based on the chi-square distribution is highly sensitive to minor
#' violations of the normality assumption and its performance does not improve with increasing sample size.
#'
#' @param x              a numeric vector, matrix or data frame with numeric variables, i.e.,
#'                       factors and character variables are excluded from \code{x} before conducting the analysis.
#' @param method         a character string specifying the method for computing the confidence interval,
#'                       must be one of \code{"chisq"}, or \code{"bonett"} (default).
#' @param alternative    a character string specifying the alternative hypothesis, must be one of
#'                       \code{"two.sided"} (default), \code{"greater"} or \code{"less"}.
#' @param conf.level     a numeric value between 0 and 1 indicating the confidence level of the interval.
#' @param group          a numeric vector, character vector or factor as grouping variable.
#' @param split          a numeric vector, character vector or factor as split variable.
#' @param sort.var       logical: if \code{TRUE}, output table is sorted by variables when specifying \code{group}.
#' @param na.omit        logical: if \code{TRUE}, incomplete cases are removed before conducting the analysis
#'                       (i.e., listwise deletion) when specifying more than one outcome variable.
#' @param digits         an integer value indicating the number of decimal places to be used.
#' @param as.na          a numeric vector indicating user-defined missing values,
#'                       i.e. these values are converted to \code{NA} before conducting the analysis.
#'                       Note that \code{as.na()} function is only applied to \code{x}, but
#'                       not to \code{group} or \code{split}.
#' @param check          logical: if \code{TRUE}, argument specification is checked.
#' @param output         logical: if \code{TRUE}, output is shown on the console.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{ci.mean}}, \code{\link{ci.mean.diff}}, \code{\link{ci.median}}, \code{\link{ci.prop}}, \code{\link{ci.prop.diff}},
#' \code{\link{ci.var}}, \code{\link{descript}}
#'
#' @references
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology - Using R and SPSS}.
#' John Wiley & Sons.
#'
#' Bonett, D. G. (2006). Approximate confidence interval for standard deviation of nonnormal distributions.
#' \emph{Computational Statistics and Data Analysis, 50}, 775-782. https://doi.org/10.1016/j.csda.2004.10.003
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
#' dat <- data.frame(group1 = c(1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2,
#'                              1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2),
#'                   group2 = c(1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 2, 2, 2,
#'                              1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2),
#'                   x1 = c(3, 1, 4, 2, 5, 3, 2, 3, 6, 4, 3, NA, 5, 3,
#'                          3, 2, 6, 3, 1, 4, 3, 5, 6, 7, 4, 3, 5, 4),
#'                   x2 = c(4, NA, 3, 6, 3, 7, 2, 7, 3, 3, 3, 1, 3, 6,
#'                          3, 5, 2, 6, 8, 3, 4, 5, 2, 1, 3, 1, 2, NA),
#'                   x3 = c(7, 8, 5, 6, 4, 2, 8, 3, 6, 1, 2, 5, 8, 6,
#'                          2, 5, 3, 1, 6, 4, 5, 5, 3, 6, 3, 2, 2, 4),
#'                   stringsAsFactors = FALSE)
#'
#' # Two-Sided 95% Confidence Interval for x1
#' ci.sd(dat$x1)
#'
#' # Two-Sided 95% Confidence Interval for x1 using chi square distribution
#' ci.sd(dat$x1, method = "chisq")
#'
#' # One-Sided 95% Confidence Interval for x1
#' ci.sd(dat$x1, alternative = "less")
#'
#' # Two-Sided 99% Confidence Interval
#' ci.sd(dat$x1, conf.level = 0.99)
#'
#' # Two-Sided 95% Confidence Interval, print results with 3 digits
#' ci.sd(dat$x1, digits = 3)
#'
#' # Two-Sided 95% Confidence Interval for x1, convert value 4 to NA
#' ci.sd(dat$x1, as.na = 4)
#'
#' # Two-Sided 95% Confidence Interval for x1, x2, and x3,
#' # listwise deletion for missing data
#' ci.sd(dat[, c("x1", "x2", "x3")], na.omit = TRUE)
#'
#' # Two-Sided 95% Confidence Interval for x1, x2, and x3,
#' # analysis by group1 separately
#' ci.sd(dat[, c("x1", "x2", "x3")], group = dat$group1)
#'
#' # Two-Sided 95% Confidence Interval for x1, x2, and x3,
#' # analysis by group1 separately, sort by variables
#' ci.sd(dat[, c("x1", "x2", "x3")], group = dat$group1, sort.var = TRUE)
#'
#' # Two-Sided 95% Confidence Interval for x1, x2, and x3,
#' # split analysis by group1
#' ci.sd(dat[, c("x1", "x2", "x3")], split = dat$group1)
#'
#' # Two-Sided 95% Confidence Interval for x1, x2, and x3,
#' # analysis by group1 separately, split analysis by group2
#' ci.sd(dat[, c("x1", "x2", "x3")],
#'       group = dat$group1, split = dat$group2)
ci.sd <- function(x, method = c("chisq", "bonett"), alternative = c("two.sided", "less", "greater"),
                  conf.level = 0.95, group = NULL, split = NULL, sort.var = FALSE, na.omit = FALSE,
                  digits = 2, as.na = NULL, check = TRUE, output = TRUE) {

  ####################################################################################
  # Data

  #......
  # Check if input 'x' is missing
  if (missing(x)) {

    stop("Please specify a numeric vector, matrix or data frame with numeric variables for the argument 'x'.",
         call. = FALSE)

  }

  #......
  # Check if input 'x' is NULL
  if (is.null(x)) {

    stop("Input specified for the argument 'x' is NULL.", call. = FALSE)

  }

  #......
  # Vector, matrix or data frame for the argument 'x'?
  if (!is.atomic(x) && !is.matrix(x) && !is.data.frame(x)) {

    stop("Please specify a numeric vector, matrix or data frame with numeric variables for the argument 'x'.",
         call. = FALSE)

  }

  #----------------------------------------
  # Data frame

  x <- as.data.frame(x, stringsAsFactors = FALSE)

  #----------------------------------------
  # Convert user-missing values into NA

  if (!is.null(as.na)) {

    # Replace user-specified values with missing values
    x <- misty::as.na(x, as.na = as.na, check = check)

    # Variable with missing values only
    x.miss <- vapply(x, function(y) all(is.na(y)), FUN.VALUE = logical(1))
    if (any(x.miss)) {

      stop(paste0("After converting user-missing values into NA, following variables are completely missing: ",
                  paste(names(which(x.miss)), collapse = ", ")), call. = FALSE)

    }

  }

  #----------------------------------------
  # Numeric Variables

  # Non-numeric variables
  non.num <- !vapply(x, is.numeric, FUN.VALUE = logical(1))

  if (any(non.num)) {

    x <- x[, -which(non.num), drop = FALSE]

    #......
    # Variables left

    if (ncol(x) == 0L) {

      stop("No variables left for analysis after excluding non-numeric variables.", call. = FALSE)

    }

    warning(paste0("Non-numeric variables were excluded from the analysis: ", paste(names(which(non.num)), collapse = ", ")),
            call. = FALSE)

  }

  #----------------------------------------
  # Listwise deletion

  if (isTRUE(na.omit) && any(is.na(x))) {

    #......
    # No group and split variable
    if (is.null(group) && is.null(split)) {

      x <- na.omit(as.data.frame(x, stringsAsFactors = FALSE))

      warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ",
                     length(attributes(x)$na.action)), call. = FALSE)

    }

    #......
    # Group variable, no split variable
    if (!is.null(group) && is.null(split)) {

      x.group <- na.omit(data.frame(x, group = group, stringsAsFactors = FALSE))

      x <- x.group[, -grep("group", names(x.group)), drop = FALSE]
      group <- x.group$group

      warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ",
                     length(attributes(x.group)$na.action)), call. = FALSE)

    }

    #......
    # No group variable, split variable
    if (is.null(group) && !is.null(split)) {

      x.split <- na.omit(data.frame(x, split = split, stringsAsFactors = FALSE))

      x <- x.split[, -grep("split", names(x.split)), drop = FALSE]
      split <- x.split$split

      warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ",
                     length(attributes(x.split)$na.action)), call. = FALSE)

    }

    #......
    # Group variable, split variable
    if (!is.null(group) && !is.null(split)) {

      x.group.split <- na.omit(data.frame(x, group = group, split = split, stringsAsFactors = FALSE))

      x <- x.group.split[,  !names(x.group.split) %in% c("group", "split"), drop = FALSE]
      group <- x.group.split$group
      split <- x.group.split$split

      warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ",
                     length(attributes(x.group.split)$na.action)), call. = FALSE)

    }

    #......
    # Variable with missing values only
    x.miss <- vapply(x, function(y) all(is.na(y)), FUN.VALUE = logical(1))
    if (any(x.miss)) {

      stop(paste0("After listwise deletion, following variables are completely missing: ",
                  paste(names(which(x.miss)), collapse = ", ")), call. = FALSE)

    }

  }

  ####################################################################################
  # Input Check

  #......
  # Check input 'check'
  if (!isTRUE(isTRUE(check) || !isTRUE(check))) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }

  #----------------------------------------

  if (isTRUE(check)) {

    #......
    # Check input 'method'
    if (!all(method %in%  c("chisq", "bonett"))) {

      stop("Character string in the argument 'method' does not match with \"chisq\", or \"bonett\".",
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

      # Vector or factor for the argument 'group'?
      if (!is.vector(group) && !is.factor(group)) {

        stop("Please specify a vector or factor for the argument 'group'.", call. = FALSE)

      }

      # Length of 'group' match with 'x'?
      if (length(group) != nrow(x)) {

        if (ncol(x) == 1L) {

          stop("Length of the vector or factor specified in 'group' does not match the length of the vector in 'x'.",
               call. = FALSE)

        } else {

          stop("Length of the vector or factor specified in 'group' does not match the number of rows in 'x'.",
               call. = FALSE)

        }

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

      # Vector or factor for the argument 'split'?
      if (!is.atomic(split) && !is.factor(split)) {

        stop("Please specify a vector or factor for the argument 'split'.", call. = FALSE)

      }

      # Length of 'split' doest not match with 'x'
      if (length(split) != nrow(x)) {

        if (ncol(x) == 1L) {

          stop("Length of the vector or factor specified in 'split' does not match the length of the vector in 'x'.",
               call. = FALSE)

        } else {

          stop("Length of the vector or factor specified in 'split' does not match the number of rows of the matrix or data frame in 'x'.",
               call. = FALSE)

        }

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
    if (!isTRUE(isTRUE(sort.var) || !isTRUE(sort.var))) {

      stop("Please specify TRUE or FALSE for the argument 'sort.var'.", call. = FALSE)

    }

    #......
    # Check input 'na.omit'
    if (!isTRUE(isTRUE(na.omit) || !isTRUE(na.omit))) {

      stop("Please specify TRUE or FALSE for the argument 'na.omit'.", call. = FALSE)

    }

    #......
    # Check input 'digits'
    if (digits %% 1L != 0L || digits < 0L) {

      stop("Specify a positive integer number for the argument 'digits'.", call. = FALSE)

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
  # Method

  if (all(c("chisq", "bonett") %in% method)) {

    method <- "bonett"

  }

  #----------------------------------------
  # Alternative hypothesis

  if (all(c("two.sided", "less", "greater") %in% alternative)) {

    alternative <- "two.sided"

  }

  ####################################################################################
  # Main Function

  #----------------------------------------
  # Confidence interval for the variance

  sd.conf <- function(x, method, alternative, conf.level, side) {

    #......
    # Data
    x <- na.omit(x)
    x.var <- var(x)

    #......
    # Number of observations
    if ((length(x) < 2L && method == "chisq") || (length(x) < 4L && method == "bonett")) {

      ci <- c(NA, NA)

    } else {

      #--------------------------------------------------------------------
      # Chi square method
      if (method == "chisq") {

        df <- length(x) - 1L

        # Two-sided CI
        switch(alternative, two.sided = {

          crit.low <- qchisq((1L - conf.level)/2L, df = df, lower.tail = FALSE)
          crit.upp <- qchisq((1L - conf.level)/2L, df = df, lower.tail = TRUE)

          ci <- sqrt(c(low = df*x.var / crit.low, upp = df*x.var / crit.upp))

        # One-sided CI: less
        }, less = {

          crit.upp <- qchisq((1L - conf.level), df = df, lower.tail = TRUE)

          ci <- c(low = 0L, upp = sqrt(df*x.var / crit.upp))

          # One-sided CI: greater
        }, greater = {

          crit.low <- qchisq((1L - conf.level), df = df, lower.tail = FALSE)

          ci <- c(low = sqrt(df*x.var / crit.low), upp = Inf)

        })

      #--------------------------------------------------------------------
      # Bonett
      } else if (method == "bonett") {

        n <- length(x)

        z <- switch(alternative,
                    two.sided = qnorm(1L - (1L - conf.level)/2L),
                    less = qnorm(1L - (1L - conf.level)),
                    greater = qnorm(1L - (1L - conf.level)))

        cc <- n/(n - z)

        gam4 <- n * sum((x - mean(x, trim = 1L / (2L * (n - 4L)^0.5)))^4L) / (sum((x - mean(x))^2))^2L

        se <- cc * sqrt((gam4 - (n - 3L)/n) / (n - 1L))

        ci <- switch(alternative,
                     two.sided = sqrt(c(low = exp(log(cc * x.var) - z * se), upp = exp(log(cc * x.var) + z * se))),
                     less = c(low = 0, upp = sqrt(exp(log(cc * x.var) + z * se))),
                     greater = c(low = sqrt(exp(log(cc * x.var) - z * se)), upp = Inf))

        #--------------------------------------------------------------------

      }

    }

    #......
    # Return object

    # Lower or upper limit
    object <- switch(side, both = ci, low = ci[1L], upp = ci[2L])

    return(object)

  }


  #----------------------------------------
  # No Grouping, No Split

  if (is.null(group) && is.null(split)) {

    result <- data.frame(variable = colnames(x),
                         n = vapply(x, function(y) length(na.omit(y)), FUN.VALUE = 1L),
                         nNA = vapply(x, function(y) sum(is.na(y)), FUN.VALUE = 1L),
                         pNA = vapply(x, function(y) sum(is.na(y)) / length(y) * 100L, FUN.VALUE = double(1)),
                         # Arithmetic mean
                         m = vapply(x, function(y) ifelse(length(na.omit(y)) <= 1L, NA, mean(y, na.rm = TRUE)), FUN.VALUE = double(1L)),
                         # Standard deviation
                         sd = vapply(x, function(y) ifelse(length(na.omit(y)) <= 1L, NA, sd(y, na.rm = TRUE)), FUN.VALUE = double(1L)),
                         # Confidence interval for the variance
                         low = vapply(x, sd.conf, method = method, alternative = alternative, conf.level = conf.level, side = "low", FUN.VALUE = double(1L)),
                         upp = vapply(x, sd.conf, method = method, alternative = alternative, conf.level = conf.level, side = "upp", FUN.VALUE = double(1L)),
                         stringsAsFactors = FALSE, row.names = NULL, check.names = FALSE)

  #----------------------------------------
  # Grouping, No Split

  } else if (!is.null(group) && is.null(split)) {

    object.group <- lapply(split(x, f = group), function(y) misty::ci.sd(y, method = method, alternative = alternative, conf.level = conf.level,
                                                                         group = NULL, split = NULL, sort.var = sort.var, na.omit = na.omit,
                                                                         as.na = as.na, check = FALSE, output = FALSE)$result)

    result <- data.frame(group = rep(names(object.group), each = ncol(x)),
                         eval(parse(text = paste0("rbind(", paste0("object.group[[", seq_len(length(object.group)), "]]", collapse = ", "), ")"))),
                         stringsAsFactors = FALSE)

  #----------------------------------------
  # No Grouping, Split

  } else if (is.null(group) && !is.null(split)) {

      result <- lapply(split(data.frame(x, stringsAsFactors = FALSE), f = split),
                       function(y) misty::ci.sd(y, method = method, alternative = alternative, conf.level = conf.level,
                                                group = NULL, split = NULL, sort.var = sort.var, na.omit = na.omit,
                                                as.na = as.na, check = FALSE, output = FALSE)$result)

  #----------------------------------------
  # Grouping, Split

  } else if (!is.null(group) && !is.null(split)) {

    result <- lapply(split(data.frame(x, group = group, stringsAsFactors = FALSE), f = split),
                       function(y) misty::ci.sd(y[, -grep("group", names(y))], method = method,
                                                alternative = alternative, conf.level = conf.level,
                                                group = y$group, split = NULL, sort.var = sort.var,
                                                na.omit = na.omit, as.na = as.na,
                                                check = FALSE, output = FALSE)$result)

  }

  ####################################################################################
  # Return object and output

  object <- list(call = match.call(),
                 type = "sd",
                 data = list(x = x, group = group, split = split),
                 args = list(method = method, alternative = alternative, conf.level = conf.level,
                             sort.var = sort.var, na.omit = na.omit, digits = digits, as.na = as.na,
                             check = check, output = output),
                 result = result)

  class(object) <- "ci"

  ####################################################################################
  # Output

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
