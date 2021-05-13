#' Multilevel Descriptive Statistics
#'
#' This function computes descriptive statistics for multilevel data, e.g. average group size, intraclass correlation
#' coefficient, design effect and effective sample size.
#'
#' Note that this function is restricted to two-level models.
#'
#' @param x           a vector, matrix or data frame.
#' @param group       a vector representing the grouping structure (i.e., group variable).
#' @param method      a character string indicating the method used to estimate intraclass correlation coefficients,
#'                    i.e., \code{"aov"} ICC estimated using the \code{aov} function,
#'                    \code{"lme4"} (default) ICC estimated using the \code{lmer} function in the \pkg{lme4} package,
#'                    \code{"nlme"} ICC estimated using the \code{lme} function in the \pkg{nlme} package.
#'                    Note that if the lme4 package is not installed, method = "aov" will be used.
#' @param REML        logical: if \code{TRUE} (default), restricted maximum likelihood is used to estimate the null model
#'                    when using the \code{lmer()} function in the \pkg{lme4} package or the \code{lme()} function in
#'                    the \pkg{nlme} package.
#' @param digits      an integer value indicating the number of decimal places to be used.
#' @param icc.digits  an integer indicating the number of decimal places to be used for displaying
#'                    intraclass correlation coefficients.
#' @param as.na       a numeric vector indicating user-defined missing values,
#'                    i.e. these values are converted to \code{NA} before conducting the analysis.
#'                    Note that \code{as.na()} function is only applied to \code{x} but not to
#'                    \code{group}.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#' @param output      logical: if \code{TRUE}, output is shown on the console.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{multilevel.icc}}
#'
#' @references
#' Hox, J., Moerbeek, M., & van de Schoot, R. (2018). \emph{Multilevel analysis: Techniques and applications} (3rd. ed.).
#' Routledge.
#'
#' Snijders, T. A. B., & Bosker, R. J. (2012). \emph{Multilevel analysis: An introduction to basic and advanced multilevel
#' modeling} (2nd ed.). Sage Publishers.
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following entries:
#' function call (\code{call}), type of analysis \code{type}, matrix or data frame specified in
#' \code{x} (\code{data}), specification of function arguments (\code{args}), and
#' list with results (\code{result}).
#'
#' @export
#'
#' @examples
#' dat <- data.frame(id = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
#'                   group = c(1, 1, 1, 1, 2, 2, 3, 3, 3),
#'                   x1 = c(2, 3, 2, 2, 1, 2, 3, 4, 2),
#'                   x2 = c(3, 2, 2, 1, 2, 1, 3, 2, 5),
#'                   x3 = c(2, 1, 2, 2, 3, 3, 5, 2, 4))
#'
#' # Multilevel descriptive statistics for x1
#' multilevel.descript(dat$x1, group = dat$group)
#'
#' # Multilevel descriptive statistics for x1, print ICC with 5 digits
#' multilevel.descript(dat$x1, group = dat$group, icc.digits = 5)
#'
#' # Multilevel descriptive statistics for x1, convert value 1 to NA
#' multilevel.descript(dat$x1, group = dat$group, as.na = 1)
#'
#' # Multilevel descriptive statistics for x1,
#' # use lmer() function in the lme4 package to estimate ICC
#' multilevel.descript(dat$x1, group = dat$group, method = "lme4")
#'
#' # Multilevel descriptive statistics for x1, x2, and x3
#' multilevel.descript(dat[, c("x1", "x2", "x3")], group = dat$group)
multilevel.descript <- function(x, group, method = c("aov", "lme4", "nlme"), REML = TRUE,
                                digits = 2, icc.digits = 3, as.na = NULL, check = TRUE,
                                output = TRUE) {

  ####################################################################################
  # Data

  #......
  # Check if input 'x' is missing
  if (isTRUE(missing(x))) {

    stop("Please specify a vector, matrix or data frame for the argument 'x'.", call. = FALSE)

  }

  #......
  # Check if input 'x' is NULL
  if (isTRUE(is.null(x))) {

    stop("Input specified for the argument 'x' is NULL.", call. = FALSE)

  }

  #......
  # Vector, matrix or data frame for the argument 'x'?
  if (isTRUE(!is.atomic(x) && !is.matrix(x) && !is.data.frame(x))) {

    stop("Please specify a numeric vector, matrix or data frame with numeric variables for the argument 'x'.",
         call. = FALSE)

  }

  #......
  # Check if input 'group' is missing
  if (isTRUE(missing(group))) {

    stop("Please specify a vector representing the grouping structure for the argument 'group'.", call. = FALSE)

  }

  #----------------------------------------
  # Data frame

  x <- as.data.frame(x, stringsAsFactors = FALSE)

  #----------------------------------------
  # Convert user-missing values into NA

  if (isTRUE(!is.null(as.na))) {

    x <- misty::as.na(x, na = as.na, check = check)

    # Variable with missing values only
    x.miss <- vapply(x, function(y) all(is.na(y)), FUN.VALUE = logical(1))
    if (isTRUE(any(x.miss))) {

      stop(paste0("After converting user-missing values into NA, following variables are completely missing: ",
                  paste(names(which(x.miss)), collapse = ", ")), call. = FALSE)

    }

  }

  ####################################################################################
  # Input Check

  # Check input 'check'
  if (isTRUE(!is.logical(check))) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }

  #-----------------------------------------

  if (isTRUE(check)) {

    #......
    # Check input 'group'
    if (isTRUE(is.null(dim(x)))) {

      # Numeric vector and group?
      if (isTRUE(length(x) != length(group))) {

        stop("Length of the vector 'x' does not match with the length of the grouping variable 'group'.",
             call. = FALSE)

      }

    } else {

      # Numeric vector and group?
      if (isTRUE(nrow(x) != length(group))) {

        stop("Number of rows in 'x' does not match with the length of the grouping variable 'group'.",
             call. = FALSE)

      }

    }

    #......
    # Check input 'group'
    if (isTRUE(length(unique(na.omit(group))) == 1L)) {

      stop("There is only one group represented in the grouping variable specified in 'group'.", call. = FALSE)

    }

    #......
    # Check input 'x': Zero variance?
    x.check <- vapply(x, function(y) length(na.omit(unique(y))) == 1L, FUN.VALUE = logical(1))

    if (isTRUE(any(x.check))) {

      if (isTRUE(length(x.check) > 1L)) {

        warning(paste0("Following variables in the matrix or data frame specified in 'x' have zero variance: ",
                       paste(names(which(x.check)), collapse = ", ")), call. = FALSE)

      } else {

        stop("Variable specified in 'x' has zero variance.", call. = FALSE)

      }

    }

    #......
    # Check input 'method'
    if (isTRUE(any(!method %in% c("aov", "lme4", "nlme")))) {

      stop("Character string in the argument 'method' does not match with \"aov\", \"lme4\", or \"nlme\".",
            call. = FALSE)

    }

    #......
    # Check input 'REML'
    if (isTRUE(!is.logical(REML))) {

      stop("Please specify TRUE or FALSE for the argument 'REML'", call. = FALSE)

    }

    #......
    # Check digits argument
    if (isTRUE(digits %% 1L != 0L || digits < 0L)) {

      stop("Specify a positive integer value for the argument 'digits'.", call. = FALSE)

    }

    #......
    # Check icc.digits argument
    if (isTRUE(icc.digits %% 1L != 0L || icc.digits < 0L)) {

      stop("Specify a positive integer value for the argument 'icc.digits'.", call. = FALSE)

    }

    #......
    # Variance within groups
    if (isTRUE(ncol(x) == 1L)) {

      if (isTRUE(all(tapply(unlist(x), group, function(y) length(na.omit(y))) <= 1L))) {

        stop("Variable specified in 'x' does not have any within-group variance.", call. = FALSE)

      }

    } else {

      x.check <- vapply(x, function(y) all(tapply(y, group, function(z) length(na.omit(z))) <= 1L), FUN.VALUE = logical(1))

      if (isTRUE(any(x.check))) {

        stop(paste0("Following variables specified in 'x' do not have any within-group variance: ",
                    paste(names(which(x.check)), collapse = ", ")), call. = FALSE)

      }

    }

    #......
    # Check input 'output'
    if (isTRUE(!is.logical(output))) {

      stop("Please specify TRUE or FALSE for the argument 'output'", call. = FALSE)

    }

  }

  ####################################################################################
  # Data and Arguments

  #-----------------------------------------
  # Argument method
  if (isTRUE(all(c("aov", "lme4", "nlme") %in% method))) {

    if (isTRUE(nzchar(system.file(package = "lme4")))) {

      method <- "lme4"

    } else {

      method <- "aov"

    }

  }

  ####################################################################################
  # Main Function

  # No. of observations
  no.obs <- vapply(x, function(y) length(na.omit(y)), FUN.VALUE = 1L)

  # No. of missing values
  no.miss <- vapply(x, function(y) sum(is.na(y)), FUN.VALUE = 1L)

  # No. of groups
  no.group <- vapply(x, function(y) length(unique(na.omit(cbind(y, group))[, "group"])), FUN.VALUE = 1L)

  # Average group size
  m.group.size <- vapply(x, function(y) mean(table(na.omit(cbind(y, group))[, "group"])), FUN.VALUE = double(1L))

  # Standard deviation group size
  sd.group.size <- vapply(x, function(y) sd(table(na.omit(cbind(y, group))[, "group"])), FUN.VALUE = double(1L))

  # Minimum group size
  min.group.size <- vapply(x, function(y) min(table(na.omit(cbind(y, group))[, "group"])), FUN.VALUE = 1L)

  # Maximum group size
  max.group.size <- vapply(x, function(y) max(table(na.omit(cbind(y, group))[, "group"])), FUN.VALUE = 1L)

  # ICC(1)
  icc1 <- misty::multilevel.icc(x, group = group, type = 1, method = method, REML = REML, check = FALSE)

  # ICC(2)
  icc2 <- misty::multilevel.icc(x, group = group, type = 2, method = method, REML = REML, check = FALSE)

  # Design effect
  deff <- 1 + icc1*(m.group.size - 1L)

  deff.sqrt <- sqrt(deff)

  # Effective sample size
  n.effect <- no.obs / deff

  ####################################################################################
  # Return object

  object <- list(call = match.call(),
                 type = "multilevel.descript",
                 data = data.frame(x = x, group = group, stringsAsFactors = FALSE),
                 args = list(method = method, REML = REML,
                             digits = digits, icc.digits = icc.digits, as.na = as.na, check = check, output = output),
                 result = list(no.obs = no.obs, no.miss = no.miss, no.group = no.group,
                               m.group.size = m.group.size, sd.group.size = sd.group.size,
                               min.group.size = min.group.size, max.group.size = max.group.size,
                               icc1 = icc1, icc2 = icc2, deff = deff, deff.sqrt = deff.sqrt, n.effect = n.effect))

  class(object) <- "misty.object"

  ####################################################################################
  # Output

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
