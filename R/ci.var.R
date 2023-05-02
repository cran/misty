#' Confidence Interval for the Variance
#'
#' This function computes a confidence interval for the variance for one or more variables, optionally
#' by a grouping and/or split variable.
#'
#' The confidence interval based on the chi-square distribution is computed by specifying \code{method = "chisq"},
#' while the Bonett (2006) confidence interval is requested by specifying \code{method = "bonett"}. By default,
#' the Bonett confidence interval interval is computed which performs well under moderate departure from
#' normality, while the confidence interval based on the chi-square distribution is highly sensitive to minor
#' violations of the normality assumption and its performance does not improve with increasing sample size.
#' Note that at least four valid observations are needed to compute the Bonett confidence interval.
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
#' \code{\link{ci.mean}}, \code{\link{ci.mean.diff}}, \code{\link{ci.median}},
#' \code{\link{ci.prop}}, \code{\link{ci.prop.diff}}, \code{\link{ci.sd}},
#' \code{\link{descript}}
#'
#' @references
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology - Using R and SPSS}.
#' John Wiley & Sons.
#'
#' Bonett, D. G. (2006). Approximate confidence interval for standard deviation of nonnormal distributions.
#' \emph{Computational Statistics and Data Analysis, 50}, 775-782. https://doi.org/10.1016/j.csda.2004.10.003
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#' \tabular{ll}{
#' \code{call} \tab function call \cr
#' \code{type} \tab type of analysis \cr
#' \code{data} \tab list with the input specified in \code{x}, \code{group}, and
#'                  \code{split} \cr
#' \code{args} \tab specification of function arguments  \cr
#' \code{result} \tab result table \cr
#' }
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
#'                          2, 5, 3, 1, 6, 4, 5, 5, 3, 6, 3, 2, 2, 4))
#'
#' # Two-Sided 95% CI for x1
#' ci.var(dat$x1)
#'
#' # Two-Sided 95% CI for x1 using chi square distribution
#' ci.var(dat$x1, method = "chisq")
#'
#' # One-Sided 95% CI for x1
#' ci.var(dat$x1, alternative = "less")
#'
#' # Two-Sided 99% CI
#' ci.var(dat$x1, conf.level = 0.99)
#'
#' # Two-Sided 95% CI, print results with 3 digits
#' ci.var(dat$x1, digits = 3)
#'
#' # Two-Sided 95% CI for x1, convert value 4 to NA
#' ci.var(dat$x1, as.na = 4)
#'
#' # Two-Sided 95% CI for x1, x2, and x3,
#' # listwise deletion for missing data
#' ci.var(dat[, c("x1", "x2", "x3")], na.omit = TRUE)
#'
#' # Two-Sided 95% CI for x1, x2, and x3,
#' # analysis by group1 separately
#' ci.var(dat[, c("x1", "x2", "x3")], group = dat$group1)
#'
#' # Two-Sided 95% CI for x1, x2, and x3,
#' # analysis by group1 separately, sort by variables
#' ci.var(dat[, c("x1", "x2", "x3")], group = dat$group1, sort.var = TRUE)
#'
#' # Two-Sided 95% CI for x1, x2, and x3,
#' # split analysis by group1
#' ci.var(dat[, c("x1", "x2", "x3")], split = dat$group1)
#'
#' # Two-Sided 95% CI for x1, x2, and x3,
#' # analysis by group1 separately, split analysis by group2
#' ci.var(dat[, c("x1", "x2", "x3")],
#'        group = dat$group1, split = dat$group2)
ci.var <- function(x, method = c("chisq", "bonett"), alternative = c("two.sided", "less", "greater"),
                   conf.level = 0.95, group = NULL, split = NULL, sort.var = FALSE, na.omit = FALSE,
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

    if (ncol(data.frame(group)) != 1L) { stop("More than one grouping variable specified for the argument 'group'.",call. = FALSE) }

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

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## As data frame ####

  x <- as.data.frame(x, stringsAsFactors = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) {

    # Replace user-specified values with missing values
    x <- misty::as.na(x, na = as.na, check = check)

    # Variable with missing values only
    x.miss <- vapply(x, function(y) all(is.na(y)), FUN.VALUE = logical(1))
    if (isTRUE(any(x.miss))) {

      stop(paste0("After converting user-missing values into NA, following variables are completely missing: ", paste(names(which(x.miss)), collapse = ", ")), call. = FALSE)

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Numeric Variables ####

  # Non-numeric variables
  non.num <- !vapply(x, is.numeric, FUN.VALUE = logical(1L))

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

      warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ", length(attributes(x)$na.action)), call. = FALSE)

    }

    #...................
    ### Group variable, no split variable ####
    if (isTRUE(!is.null(group) && is.null(split))) {

      x.group <- na.omit(data.frame(x, group = group, stringsAsFactors = FALSE))

      x <- x.group[, -grep("group", names(x.group)), drop = FALSE]
      group <- x.group$group

      warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ", length(attributes(x.group)$na.action)), call. = FALSE)

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

    #......
    # Group variable, split variable
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

      stop(paste0("After listwise deletion, following variables are completely missing: ", paste(names(which(x.miss)), collapse = ", ")), call. = FALSE)

    }

  }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # Check input 'method'
    if (isTRUE(!all(method %in%  c("chisq", "bonett")))) { stop("Character string in the argument 'method' does not match with \"chisq\", or \"bonett\".", call. = FALSE) }

    # Check input 'alternative'
    if (isTRUE(!all(alternative %in%  c("two.sided", "less", "greater")))) { stop("Character string in the argument 'alternative' does not match with \"two.sided\", \"less\", or \"greater\".", call. = FALSE) }

    # Check input 'conf.level'
    if (isTRUE(conf.level >= 1L || conf.level <= 0L)) { stop("Please specifiy a numeric value between 0 and 1 for the argument 'conf.level'.", call. = FALSE) }

    # Check input 'group'
    if (isTRUE(!is.null(group))) {

      # Input 'group' completely missing
      if (isTRUE(all(is.na(group)))) { stop("The grouping variable specified in 'group' is completely missing.", call. = FALSE) }

      # Only one group in 'group'
      if (isTRUE(length(na.omit(unique(group))) == 1L)) { warning("There is only one group represented in the grouping variable specified in 'group'.", call. = FALSE) }

    }

    # Check input 'split'
    if (isTRUE(!is.null(split))) {

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
    if (isTRUE(digits %% 1L != 0L || digits < 0L)) { stop("Specify a positive integer number for the argument 'digits'.", call. = FALSE) }

    # Check input output
    if (isTRUE(!is.logical(output))) { stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Method ####

  if (isTRUE(all(c("chisq", "bonett") %in% method))) { method <- "bonett" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Alternative hypothesis ####

  if (isTRUE(all(c("two.sided", "less", "greater") %in% alternative))) { alternative <- "two.sided" }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Confidence interval for the variance ####

  var.conf <- function(x, method, alternative, conf.level, side) {

    # Data
    x <- na.omit(x)
    x.var <- var(x)

    # Number of observations
    if (isTRUE((length(x) < 2L && method == "chisq") || (length(x) < 4L && method == "bonett"))) {

      ci <- c(NA, NA)

    } else {

      #...................
      ### Chi square method ####
      if (isTRUE(method == "chisq")) {

        df <- length(x) - 1L

        # Two-sided CI
        switch(alternative, two.sided = {

          crit.low <- qchisq((1L - conf.level)/2L, df = df, lower.tail = FALSE)
          crit.upp <- qchisq((1L - conf.level)/2L, df = df, lower.tail = TRUE)

          ci <- c(low = df*x.var / crit.low, upp = df*x.var / crit.upp)

        # One-sided CI: less
        }, less = {

          crit.upp <- qchisq((1L - conf.level), df = df, lower.tail = TRUE)

          ci <- c(low = 0L, upp = df*x.var / crit.upp)

          # One-sided CI: greater
        }, greater = {

          crit.low <- qchisq((1L - conf.level), df = df, lower.tail = FALSE)

          ci <- c(low = df*x.var / crit.low, upp = Inf)

        })

      #...................
      ### Bonett method ####
      } else if (isTRUE(method == "bonett")) {

        n <- length(x)

        z <- switch(alternative,
                    two.sided = qnorm(1L - (1L - conf.level)/2L),
                    less = qnorm(1L - (1L - conf.level)),
                    greater = qnorm(1L - (1L - conf.level)))

        cc <- n/(n - z)

        gam4 <- n * sum((x - mean(x, trim = 1L / (2L * (n - 4L)^0.5)))^4L) / (sum((x - mean(x))^2L))^2L

        se <- cc * sqrt((gam4 - (n - 3L)/n) / (n - 1L))

        ci <- switch(alternative,
                     two.sided = c(low = exp(log(cc * x.var) - z * se), upp = exp(log(cc * x.var) + z * se)),
                     less = c(low = 0L, upp = exp(log(cc * x.var) + z * se)),
                     greater = c(low = exp(log(cc * x.var) - z * se), upp = Inf))

      }

    }

    # Lower or upper limit
    object <- switch(side, both = ci, low = ci[1], upp = ci[2L])

    return(object)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## No Grouping, No Split ####

  if (isTRUE(is.null(group) && is.null(split))) {

    result <- data.frame(variable = colnames(x),
                         n = vapply(x, function(y) length(na.omit(y)), FUN.VALUE = 1L),
                         nNA = vapply(x, function(y) sum(is.na(y)), FUN.VALUE = 1L),
                         pNA = vapply(x, function(y) sum(is.na(y)) / length(y) * 100L, FUN.VALUE = double(1)),
                         # Arithmetic mean
                         m = vapply(x, function(y) ifelse(length(na.omit(y)) <= 1L, NA, mean(y, na.rm = TRUE)), FUN.VALUE = double(1L)),
                         # Variance
                         var = vapply(x, function(y) ifelse(length(na.omit(y)) <= 1L, NA, var(y, na.rm = TRUE)), FUN.VALUE = double(1L)),
                         # Confidence interval for the variance
                         low = vapply(x, var.conf, method = method, alternative = alternative, conf.level = conf.level, side = "low", FUN.VALUE = double(1L)),
                         upp = vapply(x, var.conf, method = method, alternative = alternative, conf.level = conf.level, side = "upp", FUN.VALUE = double(1L)),
                         stringsAsFactors = FALSE, row.names = NULL, check.names = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Grouping, No Split ####

  } else if (isTRUE(!is.null(group) && is.null(split))) {

    object.group <- lapply(split(x, f = group), function(y) misty::ci.var(y, method = method, alternative = alternative, conf.level = conf.level,
                                                                          group = NULL, split = NULL, sort.var = sort.var, na.omit = na.omit,
                                                                          as.na = as.na, check = FALSE, output = FALSE)$result)

    result <- data.frame(group = rep(names(object.group), each = ncol(x)),
                         eval(parse(text = paste0("rbind(", paste0("object.group[[", seq_len(length(object.group)), "]]", collapse = ", "), ")"))),
                         stringsAsFactors = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## No Grouping, Split ####

  } else if (isTRUE(is.null(group) && !is.null(split))) {

      result <- lapply(split(data.frame(x, stringsAsFactors = FALSE), f = split),
                       function(y) misty::ci.var(y, method = method, alternative = alternative, conf.level = conf.level,
                                                 group = NULL, split = NULL, sort.var = sort.var, na.omit = na.omit,
                                                 as.na = as.na, check = FALSE, output = FALSE)$result)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Grouping, Split ####

  } else if (isTRUE(!is.null(group) && !is.null(split))) {

    result <- lapply(split(data.frame(x, group = group, stringsAsFactors = FALSE), f = split),
                       function(y) misty::ci.var(y[, -grep("group", names(y))], method = method,
                                                 alternative = alternative, conf.level = conf.level,
                                                 group = y$group, split = NULL, sort.var = sort.var,
                                                 na.omit = na.omit, as.na = as.na,
                                                 check = FALSE, output = FALSE)$result)

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "ci", ci = "var",
                 data = list(x = x, group = group, split = split),
                 args = list(method = method, alternative = alternative, conf.level = conf.level,
                             sort.var = sort.var, na.omit = na.omit, digits = digits, as.na = as.na,
                             check = check, output = output),
                 result = result)

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
