#' Coefficient Alpha and Item Statistics
#'
#' This function computes point estimate and confidence interval for the (ordinal)
#' coefficient alpha (aka Cronbach's alpha) along with the corrected item-total
#' correlation and coefficient alpha if item deleted.
#'
#' Ordinal coefficient alpha was introduced by Zumbo, Gadermann and Zeisser (2007)
#' which is obtained by applying the formula for computing coefficient alpha to the
#' polychoric correlation matrix instead of the variance-covariance or product-moment
#' correlation matrix. Note that Chalmers (2018) highlighted that the ordinal
#' coefficient alpha should be interpreted only as a hypothetical estimate of an
#' alternative reliability, whereby a test's ordinal categorical response options
#' have be modified to include an infinite number of ordinal response options and
#' concludes that coefficient alpha should not be reported as a measure of a test's
#' reliability. However, Zumbo and Kroc (2019) argued that Chalmers' critique of
#' ordinal coefficient alpha is unfounded and that ordinal coefficient alpha may
#' be the most appropriate quantifier of reliability when using Likert-type measurement
#' to study a latent continuous random variable.
#' Confidence intervals are computed using the procedure by Feldt, Woodruff and Salih
#' (1987). When computing confidence intervals using pairwise deletion, the average
#' sample size from all pairwise samples is used. Note that there are at least 10
#' other procedures for computing the confidence interval (see Kelley and
#' Pornprasertmanit, 2016), which are implemented in the \code{ci.reliability()}
#' function in the \pkg{MBESSS} package by Ken Kelley (2019).
#'
#' @param ...        a matrix, data frame, variance-covariance or correlation
#'                   matrix. Note that raw data is needed to compute ordinal
#'                   coefficient alpha, i.e., \code{ordered = TRUE}. Alternatively,
#'                   an expression indicating the variable names in \code{data}
#'                   e.g., \code{item.alpha(x1, x2, x3, data = dat)}. Note that
#'                   the operators \code{.}, \code{+}, \code{-}, \code{~}, \code{:},
#'                   \code{::}, and \code{!} can also be used to select variables,
#'                   see 'Details' in the \code{\link{df.subset}} function.
#' @param data       a data frame when specifying one or more variables in the
#'                   argument \code{...}. Note that the argument is \code{NULL}
#'                   when specifying a matrix, data frame, variance-covariance
#'                   or correlation matrix for the argument \code{...}.
#' @param exclude    a character vector indicating items to be excluded from the
#'                   analysis.
#' @param std        logical: if \code{TRUE}, the standardized coefficient alpha
#'                   is computed.
#' @param ordered    logical: if \code{TRUE}, variables are treated as ordered (ordinal)
#'                   variables to compute ordinal coefficient alpha.
#' @param na.omit    logical: if \code{TRUE}, incomplete cases are removed before
#'                   conducting the analysis (i.e., listwise deletion); if
#'                   \code{FALSE} (default), pairwise deletion is used.
#' @param print      a character vector indicating which results to show, i.e.
#'                   \code{"all"} (default), for all results \code{"alpha"} for
#'                   the coefficient alpha, and \code{"item"} for item statistics.
#' @param digits     an integer value indicating the number of decimal places to
#'                   be used for displaying coefficient alpha and item-total correlations.
#' @param conf.level a numeric value between 0 and 1 indicating the confidence level
#'                   of the interval.
#' @param as.na      a numeric vector indicating user-defined missing values,
#'                   i.e. these values are converted to \code{NA} before conducting
#'                   the analysis.
#' @param write      a character string for writing the results into a Excel file
#'                   naming a file with or without file extension '.xlsx', e.g.,
#'                   \code{"Results.xlsx"} or \code{"Results"}.
#' @param check      logical: if \code{TRUE} (default), argument specification
#'                   is checked.
#' @param output     logical: if \code{TRUE} (default), output is shown.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{item.omega}}, \code{\link{item.cfa}}, \code{\link{item.invar}},
#' \code{\link{item.reverse}}, \code{\link{item.scores}}, \code{\link{write.result}}
#'
#' @references
#' Chalmers, R. P. (2018). On misconceptions and the limited usefulness of ordinal alpha.
#' \emph{Educational and Psychological Measurement, 78}, 1056-1071.
#' https://doi.org/10.1177/0013164417727036
#'
#' Cronbach, L.J. (1951). Coefficient alpha and the internal structure of tests.
#' \emph{Psychometrika, 16}, 297-334. https://doi.org/10.1007/BF02310555
#'
#' Cronbach, L.J. (2004). My current thoughts on coefficient alpha and successor
#' procedures. \emph{Educational and Psychological Measurement, 64}, 391-418.
#' https://doi.org/10.1177/0013164404266386
#'
#' Feldt, L. S., Woodruff, D. J., & Salih, F. A. (1987). Statistical inference for
#' coefficient alpha. \emph{Applied Psychological Measurement}, 11 93-103.
#' https://doi.org/10.1177/014662168701100107
#'
#' Kelley, K., & Pornprasertmanit, S. (2016). Confidence intervals for population
#' reliability coefficients: Evaluation of methods, recommendations, and software
#' for composite measures. \emph{Psychological Methods, 21}, 69-92.
#' https://doi.org/10.1037/a0040086.
#'
#' Ken Kelley (2019). \emph{MBESS: The MBESS R Package}. R package version 4.6.0.
#' https://CRAN.R-project.org/package=MBESS
#'
#' Zumbo, B. D., & Kroc, E. (2019). A measurement is a choice and Stevens' scales
#' of measurement do not help make it: A response to Chalmers. \emph{Educational
#' and Psychological Measurement, 79}, 1184-1197.
#' https://doi.org/10.1177/0013164419844305
#'
#' Zumbo, B. D., Gadermann, A. M., & Zeisser, C. (2007). Ordinal versions of coefficients
#' alpha and theta for Likert rating scales. \emph{Journal of Modern Applied Statistical
#' Methods, 6}, 21-29. https://doi.org/10.22237/jmasm/1177992180
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#' \tabular{ll}{
#' \code{call} \tab function call \cr
#' \code{type} \tab type of analysis \cr
#' \code{data} \tab data frame used for the current analysis \cr
#' \code{args} \tab specification of function arguments \cr
#' \code{result} \tab list with result tables \cr
#' }
#'
#' @export
#'
#' @examples
#' dat <- data.frame(item1 = c(4, 2, 3, 4, 1, 2, 4, 2),
#'                   item2 = c(4, 3, 3, 3, 2, 2, 4, 1),
#'                   item3 = c(3, 2, 4, 2, 1, 3, 4, 1),
#'                   item4 = c(4, 1, 2, 3, 2, 3, 4, 2))
#'
#' # Example 1a: Compute unstandardized coefficient alpha and item statistics
#' item.alpha(dat)
#'
#' # Example 1b: Alternative specification using the 'data' argument
#' item.alpha(., data = dat)
#'
#' # Example 2: Compute standardized coefficient alpha and item statistics
#' item.alpha(dat, std = TRUE)
#'
#' # Example 3: Compute unstandardized coefficient alpha
#' item.alpha(dat, print = "alpha")
#'
#' # Example 4: Compute item statistics
#' item.alpha(dat, print = "item")
#'
#' # Example 5: Compute unstandardized coefficient alpha and item statistics while excluding item3
#' item.alpha(dat, exclude = "item3")
#'
#' # Example 6: Compute variance-covariance matrix
#' dat.cov <- cov(dat)
#' # Compute unstandardized coefficient alpha based on the variance-covariance matrix
#' item.alpha(dat.cov)
#'
#' # Compute correlation matrix
#' dat.cor <- cor(dat)
#' # Example 7: Compute standardized coefficient alpha based on the correlation matrix
#' item.alpha(dat.cor)
#'
#' # Example 8: Compute ordinal coefficient alpha
#' item.alpha(dat, ordered = TRUE)
#'
#' \dontrun{
#' # Example 9a: Write Results into a text file
#' result <- item.alpha(dat, write = "Alpha.xlsx")
#'
#' # Example 9b: Write Results into a Excel file
#' result <- item.alpha(dat, write = "Alpha.xlsx")
#'
#' result <- item.alpha(dat, output = FALSE)
#' write.result(result, "Alpha.xlsx")
#' }
item.alpha <- function(..., data = NULL, exclude = NULL, std = FALSE, ordered = FALSE,
                       na.omit = FALSE, print = c("all", "alpha", "item"), digits = 2,
                       conf.level = 0.95, as.na = NULL, write = NULL, append = TRUE,
                       check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input '...' is missing
  if (isTRUE(missing(...))) { stop("Please specify the argument '...'.", call. = FALSE) }

  # Check if input '...' is NULL
  if (isTRUE(is.null(substitute(...)))) { stop("Input specified for the argument '...' is NULL.", call. = FALSE) }

  # Check if input 'data' is data frame
  if (isTRUE(!is.null(data) && !is.data.frame(data))) { stop("Please specify a data frame for the argument 'data'.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data using the argument 'data' ####

  if (isTRUE(!is.null(data))) {

    # Variable names
    var.names <- .var.names(..., data = data, check.chr = "a matrix, data frame, variance-covariance or correlation matrix")

    # Extract variables
    x <- data[, var.names]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data without using the argument 'data' ####

  } else {

    # Extract data
    x <- eval(..., enclos = parent.frame())

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Raw data or cor/cov matrix ####

  if (isTRUE(nrow(x) == ncol(x))) {

    if (isTRUE(isSymmetric(x))) {

      sym <- TRUE
      x.raw <- FALSE

    } else {

      sym <- FALSE
      x.raw <- TRUE

    }

    # Diagonal is all 1?
    if (isTRUE(sym)) {

      std <- ifelse(all(diag(x) == 1L), TRUE, FALSE)

    }

  } else {

    x.raw <- TRUE

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Ordered ####

  if (isTRUE(ordered)) {

    # Check if raw data is availeble
    if (!isTRUE(x.raw)) {

      stop("Please submit raw data to the argument 'x' to compute ordinal coefficient alpha.", call. = FALSE)

    }

    # Compute polychoric correlation matrix
    x <- misty::cor.matrix(x, method = "poly", output = FALSE)$result$cor

    x.raw <- FALSE
    std <- TRUE

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## As data frame ####

  x <- as.data.frame(x, stringsAsFactors = FALSE)

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
  ## Exclude items and specify user-defined NA ####

  # Raw data
  if (isTRUE(x.raw)) {

    if (isTRUE(!is.null(exclude))) {

      # Check input 'exclude'
      check.ex <- !exclude %in% colnames(x)
      if (isTRUE(any(check.ex))) { stop(paste0("Items to be excluded from the analysis were not found in 'x': ", paste(exclude[check.ex], collapse = ", ")), call. = FALSE) }

      x <- x[, which(!colnames(x) %in% exclude)]

      # One item left
      if (isTRUE(is.null(dim(x)))) {

        stop("At least two items after excluding items are needed to compute coefficient alpha.", call. = FALSE)

      }

    }

    # Convert user-missing values into NA
    if (isTRUE(!is.null(as.na))) { x <- .as.na(x, na = as.na) }

  # Covariance or correlation matrix
  } else {

    if (isTRUE(!is.null(exclude))) {

      x <- x[, which(!colnames(x) %in% exclude)]
      x <- x[which(!rownames(x) %in% exclude), ]

      # One item left
      if (isTRUE(is.null(dim(x)))) {

        stop("At least two items after excluding items are needed to compute coefficient alpha.", call. = FALSE)

      }

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Method for handling missing data ####

  # Listwise deletion
  if (isTRUE(na.omit)) {

    x <- na.omit(x)

  }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # Matrix or data frame for the argument 'x'?
    if (isTRUE(!is.matrix(x) && !is.data.frame(x))) { stop("Please specify a matrix, a data frame, a variance-covariance or correlation matrix for the argument 'x'.", call. = FALSE) }

    # Check input 'x': One item
    if (isTRUE(ncol(x) == 1L)) { stop("Please specify at least two items to compute coefficient alpha.", call. = FALSE) }

    # Check input 'x': Zero variance
    if (isTRUE(nrow(x) != ncol(x))) {

      x.check <- vapply(as.data.frame(x, stringsAsFactors = FALSE), function(y) length(na.omit(unique(y))) == 1L, FUN.VALUE = logical(1L))

      if (isTRUE(any(x.check))) {

        stop(paste0("Following variables in the matrix or data frame specified in 'x' have zero variance: ", paste(names(which(x.check)), collapse = ", ")), call. = FALSE)

      }

    }

    # Check input 'std'
    if (isTRUE(!is.logical(std))) { stop("Please specify TRUE or FALSE for the argument 'std'.", call. = FALSE) }

    # Check input 'ordered'
    if (isTRUE(!is.logical(ordered))) { stop("Please specify TRUE or FALSE for the argument 'ordered'.", call. = FALSE) }

    # Check input 'na.omit'
    if (isTRUE(!is.logical(na.omit))) { stop("Please specify TRUE or FALSE for the argument 'na.omit'.", call. = FALSE) }

    # Check input 'print'
    if (isTRUE(!all(print %in% c("all", "alpha", "item")))) { stop("Character strings in the argument 'print' do not all match with \"all\", \"alpha\", or \"item\".", call. = FALSE) }

    # Check input 'conf.level'
    if (isTRUE(conf.level >= 1L || conf.level <= 0L)) { stop("Please specifiy a numeric value between 0 and 1 for the argument 'conf.level'.", call. = FALSE) }

    # Check input 'digits'
    if (isTRUE(digits %% 1L != 0L || digits < 0L)) { stop("Specify a positive integer number for the argument 'digits'.", call. = FALSE) }

    # Check input 'append'
    if (isTRUE(!is.logical(append))) { stop("Please specify TRUE or FALSE for the argument 'append'.", call. = FALSE) }

    # Check input 'output'
    if (isTRUE(!is.logical(output))) { stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Print coefficient alpha and/or item statistic ####

  if (isTRUE(all(c("all", "alpha", "item") %in% print))) { print <- c("alpha", "item") }

  if (isTRUE(length(print) == 1L && "all" %in% print)) { print <- c("alpha", "item") }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Correlation or variance-covariance matrix ####

  if (isTRUE(x.raw)) {

    if (isTRUE(std)) {

      mat.sigma <- cor(x, use = "pairwise.complete.obs", method = "pearson")

    } else {

      mat.sigma <- cov(x, use = "pairwise.complete.obs", method = "pearson")

    }

  } else {

    mat.sigma <- x

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Coefficient Alpha ####

  # Define Coefficient alpha function
  alpha.function <- function(mat.sigma, p) {

    return((p / (p - 1)) * (1L - sum(diag(as.matrix(mat.sigma))) / sum(as.matrix(mat.sigma))))

  }

  p <- ncol(mat.sigma)

  alpha.mat.sigma <- alpha.function(mat.sigma, p)

  if (isTRUE(x.raw)) {

    alpha.x <- data.frame(n = nrow(x), items = ncol(mat.sigma), alpha = alpha.mat.sigma,
                          stringsAsFactors = FALSE)

  } else {

    alpha.x <- data.frame(items = ncol(mat.sigma), alpha = alpha.mat.sigma,
                          stringsAsFactors = FALSE)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Confidence interval ####

  if (isTRUE(x.raw)) {

    if (isTRUE(any(is.na(x)) && !isTRUE(na.omit))) {

      df1 <- mean(apply(combn(ncol(x), 2L), 2L, function(y) nrow(na.omit(cbind(x[, y[1L]], x[, y[2L]]))))) - 1L

    } else {

      df1 <- nrow(na.omit(x)) - 1L

    }

    df2 <- (ncol(x) -  1L) * df1

    alpha.low <- 1L - (1L - alpha.mat.sigma) * qf(1L - (1L - conf.level) / 2L, df1, df2)
    alpha.upp <- 1L - (1L - alpha.mat.sigma) * qf((1L - conf.level) / 2L, df1, df2)

    alpha.x <- data.frame(alpha.x, low = alpha.low, upp = alpha.upp, stringsAsFactors = FALSE)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Corrected item-total correlation and alpha if item deleted ####

  if (isTRUE(x.raw)) {

    itemstat <- matrix(rep(NA, times = ncol(x)*2L), ncol = 2L,
                       dimnames = list(NULL, c("it.cor", "alpha")))

    for (i in seq_len(ncol(x))) {

      var <- colnames(x)[i]

      itemstat[i, 1L] <- ifelse(ncol(x) > 2L, cor(x[, i], rowMeans(x[, -grep(var, colnames(x))], na.rm = TRUE),
                                                  use = "pairwise.complete.obs"), NA)

      if (isTRUE(std)) {

        itemstat[i, 2L] <- ifelse(ncol(x) > 2L, alpha.function(cor(x[, -grep(var, colnames(x))],
                                                               use = "pairwise.complete.obs", method = "pearson"), p = (ncol(x) - 1L)), NA)


      } else {

        itemstat[i, 2L] <- ifelse(ncol(x) > 2L, alpha.function(cov(x[, -grep(var, colnames(x))],
                                                               use = "pairwise.complete.obs", method = "pearson"), p = (ncol(x) - 1L)), NA)

      }

    }

    #...................
    ### Descriptive statistics ####

    itemstat <- data.frame(var = colnames(x),
                           misty::descript(x, output = FALSE)$result[, c("n", "nNA", "pNA", "m", "sd", "min", "max")],
                           itemstat,
                           stringsAsFactors = FALSE)

  } else {

    itemstat <- NULL

  }

  #_____________________________________________________________________________
  #
  # Return object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "item.alpha",
                 data = x,
                 args = list(exclude = exclude, std = std, ordered = ordered, na.omit = na.omit,
                             print = print, digits = digits, conf.level = conf.level, as.na = as.na,
                             write = write, append = append, check = check, output = output),
                 result = list(alpha = alpha.x, itemstat = itemstat))

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Write Results --------------------------------------------------------------

  if (isTRUE(!is.null(write))) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Text file ####

    if (isTRUE(grepl("\\.txt", write))) {

      # Send R output to textfile
      sink(file = write, append = ifelse(isTRUE(file.exists(write)), append, FALSE), type = "output", split = FALSE)

      if (isTRUE(append && file.exists(write))) { write("", file = write, append = TRUE) }

      # Print object
      print(object, check = FALSE)

      # Close file connection
      sink()

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Excel file ####

    } else {

      misty::write.result(object, file = write)

    }

  }

  #_____________________________________________________________________________
  #
  # output ---------------------------------------------------------------------

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
