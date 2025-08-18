#' Univariate and Multivariate Skewness and Kurtosis
#'
#' The function \code{skewness} computes the univariate sample or population
#' skewness and conduct's Mardia's test for multivariate skewness, while the
#' function \code{kurtosis} computes the univariate sample or population (excess)
#' kurtosis or the multivariate (excess) kurtosis and conduct's Mardia's test for
#' multivariate kurtosis. By default, the function computes the sample univariate
#' skewness or multivariate skewness and the univariate sample excess kurtosis or
#' multivariate excess kurtosis.
#'
#' @param data     a numeric vector or data frame.
#' @param ...      an expression indicating the variable names in \code{data}, e.g.,
#'                 \code{skewness(dat, x1)}. Note that the operators \code{+},
#'                 \code{-}, \code{~}, \code{:}, \code{::}, and \code{!}
#'                 can also be used to select variables, see 'Details' in the
#'                 \code{\link{df.subset}} function.
#' @param sample   logical: if \code{TRUE} (default), the univariate sample skewness
#'                 or kurtosis is computed, while the population skewness or kurtosis
#'                 is computed when \code{sample = FALSE}.
#' @param center   logical: if \code{TRUE} (default), the univariate or multivariate
#'                 kurtosis is centered, so that the expected kurtosis under
#'                 univariate or multivariate normality is 0, while the expected
#'                 kurtosis under univariate or multivariate normality is 3 when
#'                 \code{center = FALSE}.
#' @param digits   an integer value indicating the number of decimal places to be
#'                 used. Note that this argument only applied when computing
#'                 multivariate skewness and kurtosis.
#' @param p.digits an integer value indicating the number of decimal places
#'                 to be used for displaying the \emph{p}-values.
#' @param as.na    a numeric vector indicating user-defined missing values, i.e.,
#'                 these values are converted to \code{NA} before conducting the
#'                 analysis.
#' @param check    logical: if \code{TRUE} (default), argument specification is checked.
#' @param output   logical: if \code{TRUE} (default), output is shown on the console.
#'                 Note that this argument only applied when computing multivariate
#'                 skewness and kurtosis.
#'
#' @details
#' \describe{
#' \item{\strong{Univariate Skewness and Kurtosis}}{Univariate skewness and kurtosis
#' are computed based on the same formula as in SAS and SPSS:
#'
#' \itemize{
#'      \item{\emph{Population Skewness}}
#'      \deqn{\sqrt{n}\frac{\sum_{i=1}^{n}(X_i - \bar{X})^3}{(\sum_{i=1}^{n}(X_i - \bar{X})^2)^{3/2}}}
#'
#'      \item{\emph{Sample Skewness}}
#'      \deqn{\frac{n\sqrt{n - 1}}{n-2}\frac{\sum_{i=1}^{n}(X_i - \bar{X})^3}{(\sum_{i=1}^{n}(X_i - \bar{X})^2)^{3/2}}}
#'
#'      \item{\emph{Population Excess Kurtosis}}
#'      \deqn{n\frac{\sum_{i=1}^{n}(X_i - \bar{X})^4}{(\sum_{i=1}^{n}(X_i - \bar{X})^2)^2} - 3}
#'
#'      \item{\emph{Sample Excess Kurtosis}}
#'      \deqn{(n + 1)\frac{\sum_{i=1}^{n}(X_i - \bar{X})^4}{(\sum_{i=1}^{n}(X_i - \bar{X})^2)^2} - 3 + 6\frac{n - 1}{(n - 2)(n - 3)}}
#'
#' }
#'
#' Note that missing values (\code{NA}) are stripped before the computation and
#' that at least 3 observations are needed to compute skewness and at least
#' 4 observations are needed to compute kurtosis.}
#' \item{\strong{Multivariate Skewness and Kurtosis}}{Mardia's multivariate skewness
#' and kurtosis compares the joint distribution of several variables against a
#' multivariate normal distribution. The expected skewness is 0 for a multivariate
#' normal distribution, while the expected kurtosis is \eqn{p(p + 2)} for a
#' multivariate distribution of \eqn{p} variables. However, this function scales
#' the multivariate kurtosis on \eqn{p(p + 2)} according to the default setting
#' \code{center = TRUE} so that the expected kurtosis under multivariate normality
#' is 0. Multivariate skewness and kurtosis are tested for statistical significance
#' based on the chi-square distribution for skewness and standard normal distribution
#' for the kurtosis. If at least one of the tests is statistically significant,
#' the underlying joint population is inferred to be non-normal. Note that
#' non-significance of these statistical tests do not imply multivariate normality.}
#' }
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @name skewness
#'
#' @seealso
#' \code{\link{descript}}
#'
#' @references
#' Cain, M. K., Zhang, Z., & Yuan, KH. (2024). Univariate and multivariate skewness
#' and kurtosis for measuring nonnormality: Prevalence, influence and estimation.
#' \emph{Behavior Research Methods, 49}, 1716–1735. https://doi.org/10.3758/s13428-016-0814-1
#'
#' Mardia, K. V. (1970). Measures of multivariate skewness and kurtosis with applications.
#' \emph{Biometrika, 57}(3), 519-530. https://doi.org/10.2307/2334770
#'
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology
#' - Using R and SPSS}. John Wiley & Sons.
#'
#' William Revelle (2024). \emph{psych: Procedures for Psychological, Psychometric, and
#' Personality Research}. Northwestern University, Evanston, Illinois.
#' R package version 2.4.6, https://CRAN.R-project.org/package=psych.
#'
#' @return
#' Returns univariate skewness or kurtosis of \code{data} or an object of class
#' \code{misty.object}, which is a list with following entries:
#'
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{data}}{a numeric vector or data frame specified in \code{data}}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{result}}{result table}
#'
#' @note
#' These functions implemented a modified copy of the \code{mardia()} function
#' in the \pkg{psych} package by William Revelle (2024).
#'
#' @export
#'
#' @examples
#' # Example 1a: Compute univariate sample skewness
#' skewness(mtcars, mpg)
#'
#' # Example 1b: Compute univariate sample excess kurtosis
#' kurtosis(mtcars, mpg)
#'
#' # Example 2a: Compute multivariate skewness
#' skewness(mtcars)
#'
#' # Example 2b: Compute multivariate excess kurtosis
#' kurtosis(mtcars)
skewness <- function(data, ..., sample = TRUE, digits = 2, p.digits = 3,
                     as.na = NULL, check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'data' is missing
  if (isTRUE(missing(data))) { stop("Please specify a numeric vector or data frame for the argument 'data'", call. = FALSE) }

  # Check if input 'data' is NULL
  if (isTRUE(is.null(data))) { stop("Input specified for the argument 'data' is NULL.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data using the argument '...' ####

  if (isTRUE(!missing(...))) {

    # Extract data and convert tibble into data frame or vector
    x <- data[, .var.names(data = data, ...)] |> (\(y) if (isTRUE("tbl" %in% substr(class(y), 1L, 3L))) { if (isTRUE(ncol(as.data.frame(y)) == 1L)) { unname(unlist(y)) } else { as.data.frame(y) } } else { y })()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data without using the argument '...' ####

  } else {

    # Convert 'data' as tibble into data frame
    x <- data |> (\(y) if (isTRUE("tbl" %in% substr(class(y), 1L, 3L))) { if (isTRUE(ncol(as.data.frame(y)) == 1L)) { unname(unlist(y)) } else { as.data.frame(y) } } else { y })()

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) { x <- .as.na(x, na = as.na) }

  #_____________________________________________________________________________
  #
  # Univariate -----------------------------------------------------------------

  if (isTRUE(ncol(as.data.frame(x)) == 1L)) {

    # Omit missing values
    x <- na.omit(x)

    if (isTRUE(!is.null(attributes(x)$na.action))) { warning(paste0("Number of observations removed from the analysis: ", length(attributes(x)$na.action)), call. = FALSE) }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Input Check ####

    # Check inputs
    .check.input(logical = "sample", args = c("digits", "p.digits"), envir = environment(), input.check = check)

    # Additional checks
    if (isTRUE(check)) {

      # Numeric vector for the argument 'data'?
      if (isTRUE(mode(x) != "numeric")) { stop("Please specify a numeric vector for the argument 'data'.", call. = FALSE) }

      # Check input 'data': Zero variance
      if (isTRUE(length(na.omit(unique(x))) == 1L)) { stop("Vector specified in the argument 'data' has zero variance.", call. = FALSE) }

      # At least 3 observations
      if (isTRUE(length(x) < 3L)) { stop("At least 3 observations are needed to compute skewness.", call. = FALSE) }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Main Function ####

    # Number of observations
    n <- length(x)

    # Skewness
    object <- (mean((x - mean(x))^3L) / mean((x - mean(x))^2L)^(3L/2L)) |>
      (\(y) if (isTRUE(sample)) { y * sqrt(n * (n - 1L)) / (n - 2L) } else { y })() |>
      (\(z) ifelse(is.nan(z), NA, z))()

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Output ####

    return(object)

  #_____________________________________________________________________________
  #
  # Multivariate -----------------------------------------------------------------

  } else {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Missing Data ####



    # Omit complete missing columns
    x <- x[, misty::na.prop(t(x), append = FALSE) != 1L]

    # Listwise deletion
    x <- na.omit(x)

    if (isTRUE(!is.null(attributes(x)$na.action))) { warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ", length(attributes(x)$na.action)), call. = FALSE) }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Input Check ####

    # Check inputs
    .check.input(logical = "output", args = c("digits", "p.digits"), envir = environment(), input.check = check)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Main Function ####

    # Number of rows
    n <- nrow(x)

    # Number of columns
    p <- ncol(x)

    # Multivariate skewness
    skew <- scale(x, center = TRUE, scale = FALSE) |>
      (\(y) sum((y %*% solve(cov(y)) %*% t(y))^3L) / n^2L)()

    # Test statistic
    chi2 <- n * skew / 6L

    # Degrees of freedom
    df <- p * (p + 1L) * (p + 2L) / 6L

    # Result table
    result <- data.frame(n = n, var = p, skew = skew, chi2 = chi2, df = df, pval = pchisq(chi2, df = df, lower.tail = FALSE))

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Return Object ####

    object <- list(call = match.call(),
                   type = "skewness",
                   data = x,
                   args = list(digits = digits, p.digits = p.digits, as.na = as.na, check = check, output = output),
                   result = result)

    class(object) <- "misty.object"

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Output ####

    if (isTRUE(output)) { print(object, check = FALSE) }

    return(invisible(object))

  }

}

#_____________________________________________________________________________
#_____________________________________________________________________________

#' @rdname kurtosis
kurtosis <- function(data, ..., sample = TRUE, center = TRUE, digits = 2,
                     p.digits = 3, as.na = NULL, check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'data' is missing
  if (isTRUE(missing(data))) { stop("Please specify a numeric vector for the argument 'data'", call. = FALSE) }

  # Check if input 'data' is NULL
  if (isTRUE(is.null(data))) { stop("Input specified for the argument 'data' is NULL.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data using the argument 'data' ####

  if (isTRUE(!missing(...))) {

    # Extract data and convert tibble into data frame or vector
    x <- data[, .var.names(..., data = data)] |> (\(y) if (isTRUE("tbl" %in% substr(class(y), 1L, 3L))) { if (isTRUE(ncol(as.data.frame(y)) == 1L)) { unname(unlist(y)) } else { as.data.frame(y) } } else { y })()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data without using the argument 'data' ####

  } else {

    # Convert 'data' as tibble into data frame
    x <- data |> (\(y) if (isTRUE("tbl" %in% substr(class(y), 1L, 3L))) { if (isTRUE(ncol(as.data.frame(y)) == 1L)) { unname(unlist(y)) } else { as.data.frame(y) } } else { y })()

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  # Convert user-missing values into NA
  if (isTRUE(!is.null(as.na))) { x <- .as.na(x, na = as.na) }

  #_____________________________________________________________________________
  #
  # Univariate -----------------------------------------------------------------

  if (isTRUE(ncol(as.data.frame(x)) == 1L)) {

    # Omit missing values
    x <- na.omit(x)

    if (isTRUE(!is.null(attributes(x)$na.action))) { warning(paste0("Number of observations removed from the analysis: ", length(attributes(x)$na.action)), call. = FALSE) }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Input Check ####

    # Check inputs
    .check.input(logical = c("sample", "center"), args = c("digits", "p.digits"), envir = environment(), input.check = check)

    # Additional checks
    if (isTRUE(check)) {

      # Numeric vector for the argument 'data'?
      if (isTRUE(mode(x) != "numeric")) { stop("Please specify a numeric vector for the argument 'data'.", call. = FALSE) }

      # Check input 'data': Zero variance
      if (isTRUE(length(na.omit(unique(x))) == 1L)) { stop("Vector specified in the argument 'data' has zero variance.", call. = FALSE) }

      # At least 4 observations
      if (isTRUE(length(x) < 4L)) { stop("At least 4 observations are needed to compute kurtosis.", call. = FALSE) }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Main Function ####

    # Number of observations
    n <- length(x)

    # Kurtosis
    object <- (n * sum((x - mean(x))^4L) / (sum((x - mean(x))^2)^2)) |>
      (\(y) if (isTRUE(center)) { y - 3L } else { y })() |>
      (\(z) if (isTRUE(sample)) { ((n + 1L) * z + 6L) * (n - 1L) / ((n - 2L) * (n - 3L)) } else { z })() |>
      (\(w) ifelse(is.nan(w), NA, w))()

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Output ####

    return(object)

  #_____________________________________________________________________________
  #
  # Multivariate -----------------------------------------------------------------

  } else {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Missing Data ####

    # Omit complete missing columns
    x <- x[, misty::na.prop(t(x), append = FALSE) != 1L]

    # Listwise deletion
    x <- na.omit(x)

    if (isTRUE(!is.null(attributes(x)$na.action))) { warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ", length(attributes(x)$na.action)), call. = FALSE) }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Input Check ####

    # Check inputs
    .check.input(logical = c("center", "output"), args = c("digits", "p.digits"), envir = environment(), input.check = check)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Main Function ####

    # Number of rows
    n <- nrow(x)

    # Number of columns
    p <- ncol(x)

    # Multivariate kurtosis
    kurt <- scale(x, center = TRUE, scale = FALSE) |>
      (\(y) sum(diag(y %*% solve(cov(y)) %*% t(y))^2L) / n)()

    # Test statistic
    z <- (kurt - p * (p + 2L)) / sqrt(8L * p * (p + 2L) / nrow(x))

    # Center multivariate kurtosis on p(p + 2)
    if (isTRUE(center)) { kurt <- kurt - p*(p + 2L) }

    # Result table
    result <- data.frame(n = nrow(x), var = p, kurt = kurt, z = z, pval = pnorm(-abs(z)) * 2L)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Return Object ####

    object <- list(call = match.call(),
                   type = "kurtosis",
                   data = x,
                   args = list(center = center, digits = digits, p.digits = p.digits, as.na = as.na, check = check, output = output),
                   result = result)

    class(object) <- "misty.object"

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Output ####

    if (isTRUE(output)) { print(object, check = FALSE) }

    return(invisible(object))

  }

}

#_______________________________________________________________________________
