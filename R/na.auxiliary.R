#' Auxiliary variables
#'
#' This function computes (1) Pearson product-moment correlation matrix to identify variables related
#' to the incomplete variable and (2) Cohen's d comparing cases with and without missing values to
#' identify variables related to the probability of missigness.
#'
#' Note that non-numeric variables (i.e., factors, character vectors, and logical vectors) are excluded from to the analysis.
#'
#' @param x           a matrix or data frame with numeric vectors.
#' @param tri         a character string indicating which triangular of the correlation matrix to show on the console,
#'                    i.e., \code{both} for upper and lower triangular, \code{lower} (default) for the
#'                    lower triangular, and \code{upper} for the upper triangular.
#' @param weighted    logical: if \code{TRUE} (default), the weighted pooled standard deviation is used.
#' @param correct     logical: if \code{TRUE}, correction factor for Cohen's d to remove positive bias
#'                    in small samples is used.
#' @param digits      integer value indicating the number of decimal places digits to be used for displaying
#'                    correlation coefficients and Cohen's d estimates.
#' @param as.na       a numeric vector indicating user-defined missing values,
#'                    i.e. these values are converted to \code{NA} before conducting the analysis.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#' @param output      logical: if \code{TRUE}, output is shown on the console.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{as.na}}, \code{\link{na.as}}, \code{\link{na.coverage}}, \code{\link{na.descript}},
#' \code{\link{na.indicator}}, \code{\link{na.pattern}}, \code{\link{na.prop}}.
#'
#' @references
#' Enders, C. K. (2010). \emph{Applied missing data analysis}. Guilford Press.
#'
#' Graham, J. W. (2009). Missing data analysis: Making it work in the real world.
#' \emph{Annual Review of Psychology, 60}, 549-576. https://doi.org/10.1146/annurev.psych.58.110405.085530
#'
#' van Buuren, S. (2018). \emph{Flexible imputation of missing data} (2nd ed.). Chapman & Hall.
#'
#' @return
#' Returns an object of class \code{na.auxiliary}, which is a list with following entries: function call (\code{call}),
#' matrix or data frame specified in \code{x} (\code{data}), specification of function arguments (\code{args}), and
#' list with results (\code{result}).
#'
#' @export
#'
#' @examples
#' dat <- data.frame(x1 = c(1, NA, 2, 5, 3, NA, 5, 2),
#'                   x2 = c(4, 2, 5, 1, 5, 3, 4, 5),
#'                   x3 = c(NA, 3, 2, 4, 5, 6, NA, 2),
#'                   x4 = c(5, 6, 3, NA, NA, 4, 6, NA))
#'
#' # Auxiliary variables
#' na.auxiliary(dat)
na.auxiliary <- function(x, tri = c("both", "lower", "upper"), weighted = TRUE, correct = FALSE,
                         digits = 2, as.na = NULL, check = TRUE, output = TRUE) {


  ####################################################################################
  # Function

  cohens.d.na.auxiliary <- function(formula, data, weighted = TRUE, correct = FALSE) {

    #-----------------------------------------------------------------------------------
    # Formula

    # Variables
    var.formula <- all.vars(as.formula(formula))

    # Outcome(s)
    y.var <- var.formula[-length(var.formula)]

    # Grouping variable
    group.var <- var.formula[length(var.formula)]

    # Data
    data <- as.data.frame(data[, var.formula])

    #...................
    # Data and Arguments

    # Outcome
    x.dat <- data[, y.var]

    # Grouping
    group.dat <- data[, group.var]

    #...................
    # Descriptives

    # Mean difference
    x.diff <- diff(tapply(x.dat, group.dat, mean, na.rm = TRUE))

    # Sample size by group
    n.group <- tapply(x.dat, group.dat, function(y) length(na.omit(y)))

    #...................
    # Standard deviation

    # Variance by group
    var.group <- tapply(x.dat, group.dat, var, na.rm = TRUE)

   # Weighted pooled standard deviation
   if (isTRUE(weighted)) {

    sd.group <- sqrt(((n.group[1] - 1)*var.group[1] + (n.group[2] - 1)*var.group[2]) / (sum(n.group) - 2))

    # Unweigted pooled standard deviation
    } else {

      sd.group <- sum(var.group) / 2

    }

    #........................................
    # Cohen's d estimate

    estimate <- x.diff / sd.group

    #........................................
    # Correction factor

    # Bias-corrected Cohen's d
    if (isTRUE(correct) && isTRUE(weighted)) {

      v <- sum(n.group) - 2

      # Correction factor based on gamma function
      if (sum(n.group) < 200) {

        corr.factor <- gamma(0.5*v) / ((sqrt(v / 2)) * gamma(0.5 * (v - 1)))

      # Correction factor based on approximation
      } else {

        corr.factor <- (1 - (3 / (4 * v - 1)))

      }

      estimate <- estimate*corr.factor

    }

    return(estimate)

  }

  ####################################################################################
  # Data

  # Check input 'x'
  if (missing(x)) {

    stop("Please specify a matrix or data frame for the argument 'x'", call. = FALSE)

  }

  if (!is.matrix(x) && !is.data.frame(x)) {

    stop("Please specify a matrix or data frame for the argument 'x'.", call. = FALSE)

  }

  #----------------------------------------
  # Data frame

  df <- as.data.frame(x)

  #-----------------------------------------
  # Exclude factors, character vectors and logical vectors

  df.num <- sapply(df, function(y) is.numeric(y))

  if (any(!df.num)) {


    # Select numeric variables
    df <- df[, which(df.num)]

    warning(paste0("Non-numeric variables excluded from the analysis: ",
                   paste(names(df.num)[!df.num], collapse = ", ")), call. = FALSE)


  }

  #-----------------------------------------
  # Convert user-missing values into NA
  if (!is.null(as.na)) {

    df <- misty::as.na(df, na = as.na, check = check)

  }

  ####################################################################################
  # Input Check

  #......
  # Check input 'check'
  if (isFALSE(isTRUE(check) | isFALSE(check))) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }

  #-----------------------------------------

  if (isTRUE(check)) {

    #......
    # No missing values
    if (all(!is.na(df))) {

      stop("There are no missing values (NA) in the matrix or data frame specified in 'x'", call. = FALSE)

    }

    #......
    # Check input 'tri'
    if (any(!tri %in% c("both", "lower", "upper"))) {

      stop("Character string in the argument 'tri' does not match with \"both\", \"lower\", or \"upper\".",
              call. = FALSE)

    }

    #......
    # Check input 'weighted'
    if (isFALSE(isTRUE(weighted) | isFALSE(weighted))) {

      stop("Please specify TRUE or FALSE for the argument 'weighted'.", call. = FALSE)

    }

    #......
    # Check input 'correct'
    if (isFALSE(isTRUE(correct) | isFALSE(correct))) {

      stop("Please specify TRUE or FALSE for the argument 'correct'.", call. = FALSE)

    }

    #......
    # Check input 'digits'
    if (digits %% 1 != 0 | digits < 0) {

      stop("Specify a positive integer number for the argument 'digits'", call. = FALSE)

    }

    #......
    # Check input 'output'
    if (isFALSE(isTRUE(output) | isFALSE(output))) {

      stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE)

    }

  }

  ####################################################################################
  # Arguments

  #-----------------------------------------
  # Print triangular
  tri <- ifelse(all(c("both", "lower", "upper") %in% tri), "lower", tri)

  ####################################################################################
  # Main Function

  #-----------------------------------------
  # Variables related to the incomplete variable

  cor.mat <- cor(df, use = "pairwise.complete.obs")

  #-----------------------------------------
  # Variables related to the probability of missingness

  #......
  # Indicator matrix
  ind <- misty::na.indicator(df, check = check)
  colnames(ind) <- paste0(colnames(ind), "_ind")

  #......
  # Pairwise combinations
  df.combn <- combn(ncol(df), m = 2)

  #......
  # Data
  df.ind <- data.frame(df, ind)

  #......
  # Cohen's d

  result.d.upp <- numeric(ncol(df.combn))
  result.d.low <- numeric(ncol(df.combn))
  for (i in 1:ncol(df.combn)) {

    temp <- df.combn[, i]

    if (length(unique(df.ind[, colnames(ind)[temp[1]]])) == 2 &&
        all(tapply(df.ind[,  names(df)[temp[2]]], df.ind[colnames(ind)[temp[1]]], function(y) length(unique(na.omit(y)))) > 0)) {

      result.d.upp[i] <- eval(parse(text = paste0("cohens.d.na.auxiliary(", names(df)[temp[2]], " ~ ", colnames(ind)[temp[1]],
                                                  ", data = df.ind, weighted = weighted, correct = correct)")))

    } else {

      result.d.upp[i] <- NA

    }

    if (length(unique(df.ind[, colnames(ind)[temp[2]]])) == 2 &&
        all(tapply(df.ind[,  names(df)[temp[1]]], df.ind[colnames(ind)[temp[2]]], function(y) length(unique(na.omit(y)))) > 0)) {

      result.d.low[i] <- eval(parse(text = paste0("cohens.d.na.auxiliary(", names(df)[temp[1]], " ~ ", colnames(ind)[temp[2]],
                                                  ", data = df.ind, weighted = weighted, correct = correct)")))

    } else {

      result.d.low[i] <- NA

    }

  }

  #-----------------------------------------
  # Cohen's d matrix
  d.mat <-  matrix(NA, ncol = ncol(df), nrow = ncol(df), dimnames = list(names(df), names(df)))

  d.mat[rev(upper.tri(d.mat))] <- result.d.upp
  d.mat <- t(d.mat)

  d.mat[lower.tri(d.mat)] <- result.d.low

  ####################################################################################
  # Return object

  object <- list(call = match.call(),
                 data = list(x = x, df = df),
                 args = list(tri = tri, weighted = weighted, correct = correct, digits = digits,
                             as.na = as.na, check = check, output = output),
                 result = list(cor = cor.mat, d = d.mat))

  class(object) <- "na.auxiliary"

  ####################################################################################
  # Output

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}