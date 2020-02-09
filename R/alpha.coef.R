#' Coefficient Alpha and Item Statistics
#'
#' This function computes point estimate and confidence interval for the coefficient alpha (aka Cronbach's alpha)
#' along with the corrected item-total correlation and coefficient alpha if item deleted.
#'
#' Confidence intervals are computed using the procedure by Feldt, Woodruff and Salih (1987). When computing
#' confidence intervals using pairwise deletion, the average sample size from all pairwise samples is used.
#' Note that there are at least 10 other procedures for computing the confidence interval (see Kelley and
#' Pornprasertmanit, 2016), which are implemented in the \code{ci.reliability()} function in the
#' \pkg{MBESSS} package by Ken Kelley (2019).
#'
#' @param x          a matrix, data frame, variance-covariance or correlation matrix.
#' @param exclude    a character vector indicating items to be excluded from the analysis.
#' @param std        logical: if \code{TRUE} the standardized coefficient alpha is computed.
#' @param use        a character string indicating a method for computing a correlation or variance-covariance
#'                   matrix in the presence of missing values, i.e., \code{"listwise"} for listwise deletion
#'                   and \code{"pairwise"} (default) for pairwise deletion.
#' @param print      a character vector indicating which results to show, i.e. \code{"all"} (default), for all
#'                   results \code{"alpha"} for the coefficient alpha, and \code{"item"} for item statistics.
#' @param digits     an integer value indicating the number of decimal places to be used for displaying
#'                   coefficient alpha and item-total correlations.
#' @param conf.level a numeric value between 0 and 1 indicating the confidence level of the interval.
#' @param as.na      a numeric vector indicating user-defined missing values,
#'                   i.e. these values are converted to \code{NA} before conducting the analysis.
#' @param check      logical: if \code{TRUE}, argument specification is checked.
#' @param output     logical: if \code{TRUE}, output is shown.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{reverse.item}}, \code{\link{scores}}
#'
#' @references
#' Cronbach, L.J. (1951). Coefficient alpha and the internal structure of tests. \emph{Psychometrika, 16}, 297-334.
#'
#' Cronbach, L.J. (2004). My current thoughts on coefficient alpha and successor procedures. \emph{Educational
#' and Psychological Measurement, 64}, 391-418.
#'
#' Feldt, L. S., Woodruff, D. J., & Salih, F. A. (1987). Statistical inference for coefficient alpha.
#' \emph{Applied Psychological Measurement}, 11 93-103.
#'
#' Kelley, K., & Pornprasertmanit, S. (2016). Confidence intervals for population reliability coefficients:
#' Evaluation of methods, recommendations, and software for composite measures. \emph{Psychological Methods, 21}, 69-92.
#'
#' Ken Kelley (2019). \emph{MBESS: The MBESS R Package}. R package version 4.6.0.
#' https://CRAN.R-project.org/package=MBESS
#'
#' @return
#' Returns an object of class \code{alpha.coef}, which is a list with following entries: function call (\code{call}),
#' matrix or data frame specified in \code{x} (\code{data}), specification of function arguments (\code{args}), and
#' list with results (\code{result}).
#'
#' @export
#'
#' @examples
#' dat <- data.frame(item1 = c(5, 2, 3, 4, 1, 2, 4, 2),
#'                   item2 = c(5, 1, 3, 5, 2, 2, 5, 1),
#'                   item3 = c(4, 2, 4, 5, 1, 3, 5, 1),
#'                   item4 = c(5, 1, 2, 5, 2, 3, 4, 2))
#'
#' # Compute unstandardized coefficient alpha and item statistics
#' alpha.coef(dat)
#'
#' # Compute standardized coefficient alpha and item statistics
#' alpha.coef(dat, std = TRUE)
#'
#' # Compute unstandardized coefficient alpha
#' alpha.coef(dat, print = "alpha")
#'
#' # Compute item statistics
#' alpha.coef(dat, print = "item")
#'
#' # Compute unstandardized coefficient alpha and item statistics while excluding item3
#' alpha.coef(dat, exclude = "item3")
#'
#' # Compute variance-covariance matrix
#' dat.cov <- cov(dat)
#' # Compute unstandardized coefficient alpha based on the variance-covariance matrix
#' alpha.coef(dat.cov)
#'
#' # Compute variance-covariance matrix
#' dat.cor <- cor(dat)
#' # Compute standardized coefficient alpha based on the correlation matrix
#' alpha.coef(dat.cor)
alpha.coef <- function(x, exclude = NULL, std = FALSE, use = c("listwise", "pairwise"),
                       print = c("all", "alpha", "item"), digits = 2, conf.level = 0.95,
                       as.na = NULL, check = TRUE, output = TRUE) {

  ####################################################################################
  # Input Check

  #......
  # Check if input 'x' is missing
  if (missing(x)) {

    stop("Please specify a matrix, data frame, variance-covariance or correlation matrix for the argument 'x'.",
         call. = FALSE)

  }

  #......
  # Check input 'check'
  if (isFALSE(isTRUE(check) || isFALSE(check))) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }

  #----------------------------------------

  if (isTRUE(check)) {

    #......
    # Matrix or data frame for the argument 'x'?
    if (!is.matrix(x) && !is.data.frame(x)) {

      stop("Please specify a matrix, a data frame, a variance-covariance or correlation matrix for the argument 'x'.",
           call. = FALSE)

    }

    #......
    # Check input 'x': One item
    if (ncol(x) == 1) {

      stop("Please specify at least two items to compute coefficient alpha.", call. = FALSE)

    }

    #......
    # Check input 'x': Zero variance
    if (nrow(x) != ncol(x)) {

      x.check <- vapply(as.data.frame(x), function(y) length(na.omit(unique(y))) == 1, FUN.VALUE = logical(1))

      if (any(x.check)) {

        stop(paste0("Following variables in the matrix or data frame specified in 'x' have zero variance: ",
                     paste(names(which(x.check)), collapse = ", ")), call. = FALSE)

      }

    }

    #......
    # Check input 'exclude'
    check.ex <- !exclude %in% colnames(x)
    if (any(check.ex)) {

      stop(paste0("Items to be excluded from the analysis were not found in 'x': ", paste(exclude[check.ex], collapse = ", ")),
           call. = FALSE)
    }

    #......
    # Check input 'std'
    if (isFALSE(isTRUE(std) || isFALSE(std))) {

      stop("Please specify TRUE or FALSE for the argument 'std'.", call. = FALSE)

    }

    #......
    # Check input 'use'
    if (!all(use %in% c("listwise", "pairwise"))) {

      stop("Character strings in the argument 'use' do not all match with \"listwise\", or \"pairwise\".",
           call. = FALSE)

    }

    #......
    # Check input 'print'
    if (!all(print %in% c("all", "alpha", "item"))) {

      stop("Character strings in the argument 'print' do not all match with \"all\", \"alpha\", or \"item\".",
           call. = FALSE)

    }

    #......
    # Check input 'conf.level'
    if (conf.level >= 1 || conf.level <= 0) {

      stop("Please specifiy a numeric value between 0 and 1 for the argument 'conf.level'.",
           call. = FALSE)

    }

    #......
    # Check input 'digits'
    if (digits %% 1 != 0 || digits < 0) {

      stop("Specify a positive integer number for the argument 'digits'.", call. = FALSE)

    }

    #......
    # Check input 'output'
    if (isFALSE(isTRUE(output) || isFALSE(output))) {

        stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE)

    }

  }

  ####################################################################################
  # Data and Arguments

  #----------------------------------------
  # Raw data or cor/cov matrix?

  if (nrow(x) == ncol(x)) {

    if (isSymmetric(x)) {

      sym <- TRUE
      x.raw <- FALSE

    } else {

      sym <- FALSE
      x.raw <- TRUE

    }

    # Diagonal is all 1?
    if (isTRUE(sym)) {

      std <- ifelse(all(diag(x) == 1), TRUE, FALSE)

    }

  } else {

    x.raw <- TRUE

  }

  #----------------------------------------
  # Data frame

  x <- as.data.frame(x)

  #----------------------------------------
  # Exclude items (exclude) and specify user-defined NA (as.na)

  if (isTRUE(x.raw)) {

    if (!is.null(exclude)) {

      x <- x[, which(!colnames(x) %in% exclude)]

    }

    #----------------------------------------
    # Convert user-missing values into NA

    if (!is.null(as.na)) {

      x <- misty::as.na(x, as.na = as.na, check = check)

      # Variable with missing values only
      x.miss <- vapply(x, function(y) all(is.na(y)), FUN.VALUE = logical(1))
      if (any(x.miss)) {

        stop(paste0("After converting user-missing values into NA, following items are completely missing: ",
                    paste(names(which(x.miss)), collapse = ", ")), call. = FALSE)

      }

      # Zero variance
      x.zero.var <- vapply(x, function(y) length(na.omit(unique(y))) == 1, FUN.VALUE = logical(1))
      if (any(x.zero.var)) {

        stop(paste0("After converting user-missing values into NA, following items have zero variance: ",
                    paste(names(which(x.zero.var)), collapse = ", ")), call. = FALSE)

      }

    }

  } else {

    if (!is.null(exclude)) {

      x <- x[, which(!colnames(x) %in% exclude)]
      x <- x[which(!rownames(x) %in% exclude), ]

    }

  }

  #----------------------------------------
  # One item left

  if (is.null(dim(x))) {

    stop("At least two items after excluding items are needed to compute coefficient alpha.", call. = FALSE)

  }

  #----------------------------------------
  # Method for handling missing data

  use <- ifelse(all(c("listwise", "pairwise") %in% use) | all(use == "pairwise"), "pairwise.complete.obs", "complete.obs")

  #----------------------------------------
  # Print coefficient alpha and/or item statistic

  if (all(c(c("all", "alpha", "item")) %in% print)) { print <- c("alpha", "item") }

  if (length(print) == 1 && "all" %in% print) { print <- c("alpha", "item") }

  ####################################################################################
  # Main Function

  #----------------------------------------
  # Correlation or variance-covariance matrix

  if (isTRUE(x.raw)) {

    if (isTRUE(std)) {

      mat.sigma <- cor(x, use = use, method = "pearson")

    } else {

      mat.sigma <- cov(x, use = use, method = "pearson")

    }

  } else {

    mat.sigma <- x

  }

  #----------------------------------------
  # Coefficient Alpha

  # Define Coefficient alpha function
  alpha.function <- function(mat.sigma, p) {

    return((p / (p - 1)) * (1 - sum(diag(as.matrix(mat.sigma))) / sum(as.matrix(mat.sigma))))

  }

  p <- ncol(mat.sigma)

  alpha.mat.sigma <- alpha.function(mat.sigma, p)

  alpha.x <- data.frame(items = ncol(mat.sigma), alpha = alpha.mat.sigma)

  #----------------------------------------
  # Confidence interval

  if (isTRUE(x.raw)) {

    if (any(is.na(x)) && use == "pairwise.complete.obs") {

      df1 <- mean(apply(combn(ncol(x), 2), 2, function(y) nrow(na.omit(cbind(x[, y[1]], x[, y[2]]))))) - 1

    } else {

      df1 <- nrow(na.omit(x)) - 1

    }

    df2 <- (ncol(x) -  1) * df1

    alpha.low <- 1 - (1 - alpha.mat.sigma) * qf(1 - (1 - conf.level) / 2, df1, df2)
    alpha.upp <- 1 - (1 - alpha.mat.sigma) * qf((1 - conf.level) / 2, df1, df2)

    alpha.x <- data.frame(alpha.x, low = alpha.low, upp = alpha.upp)

  }

  #----------------------------------------
  # Corrected item-total correlation

  if (isTRUE(x.raw)) {

    itemstat <- matrix(rep(NA, times = ncol(x)*2), ncol = 2,
                       dimnames = list(NULL, c("it cor", "alpha")))

    for (i in seq_len(ncol(x))) {

      var <- colnames(x)[i]

      itemstat[i, 1] <- ifelse(ncol(x) > 2, cor(x[, i], rowMeans(x[, -grep(var, colnames(x))], na.rm = TRUE), use = use), NA)

      if (isTRUE(std)) {

        itemstat[i, 2] <- ifelse(ncol(x) > 2, alpha.function(cor(x[, -grep(var, colnames(x))], use = use, method = "pearson"), p = (ncol(x) - 1)), NA)


      } else {

        itemstat[i, 2] <- ifelse(ncol(x) > 2, alpha.function(cov(x[, -grep(var, colnames(x))], use = use, method = "pearson"), p = (ncol(x) - 1)), NA)

      }

    }

    #........................................
    # Descriptive statistics
    if (use == "complete.obs") {

      x <- na.omit(x)

    }

    itemstat <- data.frame(var = colnames(x),
                           n = vapply(x, function(y) length(na.omit(y)), FUN.VALUE = 1),
                           nNA = vapply(x, function(y) sum(is.na(y)), FUN.VALUE = 1),
                           pNA = vapply(x, function(y) sum(is.na(y)) / length(y), FUN.VALUE = 1) * 100,
                           m = colMeans(x, na.rm = TRUE),
                           sd = vapply(x, sd, na.rm = TRUE, FUN.VALUE = 1),
                           min = vapply(x, function(y) min(y, na.rm = TRUE), FUN.VALUE = 1),
                           max = vapply(x, function(y) max(y, na.rm = TRUE), FUN.VALUE = 1),
                           itemstat,
                           stringsAsFactors = FALSE)

  } else {

    itemstat <- NULL

  }

  ####################################################################################
  # Return object

  #----------------------------------------
  # Return object

  object <- list(call = match.call(),
                 data = x,
                 args = list(exclude = exclude, std = std, use = use, print = print,
                             digits = digits, conf.level = conf.level, as.na = as.na,
                             check = check, output = output),
                 result = list(alpha = alpha.x, itemstat = itemstat))

  class(object) <- "alpha.coef"

  ####################################################################################
  # Output

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
