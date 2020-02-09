#' Multilevel Descriptve Statistics
#'
#' This function computes descriptive statistics for multilevel data, e.g. average group size, intraclass correlation
#' coefficient, design effect and effectice sample size.
#'
#' Note that this function is restricted to two-level models.
#'
#' @param x           a vector, matrix or data frame.
#' @param group       a vector representing the grouping structure (i.e., group variable).
#' @param method      a character string indicating the method used to estimate intraclass correlation coefficients,
#'                    i.e., \code{"aov"} (default) ICC estimated using the \code{aov} function,
#'                    \code{"lme4"} ICC estimated using the \code{lmer} function in the \pkg{lme4} package,
#'                    \code{"nlme"} ICC estimated using the \code{lme} function in the \pkg{nlme} package.
#' @param REML        logical: if \code{TRUE}, restricted maximum likelihood is used to estimate the null model when
#'                    using the \code{lmer()} function in the \pkg{lme4} package or the \code{lme()} function in
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
#' Returns an object of class \code{multilevel.descript}, which is a list with following entries: function call (\code{call}),
#' matrix or data frame specified in \code{x} (\code{data}), specification of function arguments (\code{args}), and
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
#' # Multilevel descriptve statistics for x1
#' multilevel.descript(dat$x1, group = dat$group)
#'
#' # Multilevel descriptve statistics for x1, print ICC with 5 digits
#' multilevel.descript(dat$x1, group = dat$group, icc.digits = 5)
#'
#' # Multilevel descriptve statistics for x1, convert value 1 to NA
#' multilevel.descript(dat$x1, group = dat$group, as.na = 1)
#'
#' # Multilevel descriptve statistics for x1,
#' # use lmer() function in the lme4 package to estimate ICC
#' multilevel.descript(dat$x1, group = dat$group, method = "lme4")
#'
#' # Multilevel descriptve statistics for x1, x2, and x3
#' multilevel.descript(dat[, c("x1", "x2", "x3")], group = dat$group)
multilevel.descript <- function(x, group, method = c("aov", "lme4", "nlme"), REML = TRUE,
                                digits = 2, icc.digits = 3, as.na = NULL, check = TRUE, output = TRUE) {

  ####################################################################################
  # Data

  #-----------------------------------------
  # Check input 'x'
  if (missing(x)) {

    stop("Please specify a vector, matrix or data frame for the argument 'x'", call. = FALSE)

  }

  #----------------------------------------
  # Vector, matrix or data frame for the argument 'x'?
  if (!is.vector(x) && !is.matrix(x) && !is.data.frame(x)) {

    stop("Please specify a numeric vector, matrix or data frame with numeric variables for the argument 'x'",
         call. = FALSE)

  }

  #-----------------------------------------
  # Check input 'group'
  if (missing(group)) {

    stop("Please specify a vector representing the grouping structure for the argument 'group'", call. = FALSE)

  }

  #----------------------------------------
  # Data frame

  df <- as.data.frame(x)

  #----------------------------------------
  # Convert user-missing values into NA

  if (!is.null(as.na)) {

    df <- misty::as.na(df, na = as.na, check = check)

    # Variable with missing values only
    df.miss <- sapply(df, function(y) all(is.na(y)))
    if (any(df.miss)) {

      stop(paste0("After converting user-missing values into NA, following variables are completely missing: ",
                  paste(names(which(df.miss)), collapse = ", ")), call. = FALSE)

    }

  }

  ####################################################################################
  # Input Check

  # Check input 'check'
  if (isFALSE(isTRUE(check) | isFALSE(check))) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }

  #-----------------------------------------

  if (isTRUE(check)) {

    #......
    # Check input 'group'
    if (is.null(dim(df))) {

      # Numeric vector and group?
      if (length(df) != length(group)) {

        stop("Length of the vector 'x' does not match with the length of the grouping variable 'group'.",
             call. = FALSE)

      }

    } else {

      # Numeric vector and group?
      if (nrow(df) != length(group)) {

        stop("Number of rows in 'x' does not match with the length of the grouping variable 'group'.",
             call. = FALSE)

      }

    }

    #......
    # Check input 'group'
    if (length(unique(na.omit(group))) == 1) {

      stop("There is only one group represented in the grouping variable in 'group'.", call. = FALSE)

    }

    #......
    # Check input 'method'
    if (any(!method %in% c("aov", "lme4", "nlme"))) {

      stop("Character string in the argument 'method' does not match with \"aov\", \"lme4\", or \"nlme\".",
            call. = FALSE)

    }

    #......
    # Check input 'REML'
    if (isFALSE(isTRUE(REML) | isFALSE(REML))) {

      stop("Please specify TRUE or FALSE for the argument 'REML'", call. = FALSE)

    }

    #......
    # Check digits argument
    if (digits %% 1 != 0 | digits < 0) {

      stop("Specify a positive integer value for the argument 'digits'.", call. = FALSE)

    }

    #......
    # Check icc.digits argument
    if (icc.digits %% 1 != 0 | icc.digits < 0) {

      stop("Specify a positive integer value for the argument 'icc.digits'.", call. = FALSE)

    }

    #......
    # Variance within groups
    if (is.null(dim(x))) {

      if (all(tapply(unlist(df), group, function(y) length(na.omit(y))) <= 1)) {

        stop("Variable specified in 'x' does not have any within-group variance.", call. = FALSE)

      }

    } else {

      if (any(sapply(df, function(y) all(tapply(y, group, function(z) length(na.omit(z))) <= 1)))) {

        stop("There are variables in 'x' without any within-group variance.", call. = FALSE)

      }

    }

    #......
    # Check input 'output'
    if (isFALSE(isTRUE(output) | isFALSE(output))) {

      stop("Please specify TRUE or FALSE for the argument 'output'", call. = FALSE)

    }

  }

  ####################################################################################
  # Data and Arguments

  #-----------------------------------------
  # Argument method
  if (all(c("aov", "lme4", "nlme") %in% method)) { method <- "aov"}

  ####################################################################################
  # Main Function

  # No. of observations
  no.obs <- sapply(df, function(y) length(na.omit(y)))

  # No. of missing values
  no.miss <- sapply(df, function(y) sum(is.na(y)))

  # No. of groups
  no.group <- sapply(df, function(y) length(unique(na.omit(cbind(y, group))[, "group"])))

  # Average group size
  m.group.size <- sapply(df, function(y) mean(table(na.omit(cbind(y, group))[, "group"])))

  # Standard deviation group size
  sd.group.size <- sapply(df, function(y) sd(table(na.omit(cbind(y, group))[, "group"])))

  # Minimum group size
  min.group.size <- sapply(df, function(y) min(table(na.omit(cbind(y, group))[, "group"])))

  # Maximum group size
  max.group.size <- sapply(df, function(y) max(table(na.omit(cbind(y, group))[, "group"])))

  # ICC(1)
  icc1 <- misty::multilevel.icc(df, group = group, type = 1, method = method, REML = REML, check = FALSE)

  # ICC(2)
  icc2 <- misty::multilevel.icc(df, group = group, type = 2, method = method, REML = REML, check = FALSE)

  # Design effect
  deff <- 1 + icc1*(m.group.size - 1)

  deff.sqrt <- sqrt(deff)

  # Effectice sampel size
  n.effect <- no.obs / deff

  ####################################################################################
  # Return object

  object <- list(call = match.call(),
                 data = data.frame(x = x, group = group),
                 args = list(method = method, REML = REML,
                             digits = digits, icc.digits = icc.digits, as.na = as.na, check = check, output = output),
                 result = list(no.obs = no.obs, no.miss = no.miss, no.group = no.group,
                               m.group.size = m.group.size, sd.group.size = sd.group.size,
                               min.group.size = min.group.size, max.group.size = max.group.size,
                               icc1 = icc1, icc2 = icc2, deff = deff, deff.sqrt = deff.sqrt, n.effect = n.effect))

  class(object) <- "multilevel.descript"

  ####################################################################################
  # Output

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
