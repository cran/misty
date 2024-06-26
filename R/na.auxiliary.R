#' Auxiliary variables analysis
#'
#' This function computes (1) Pearson product-moment correlation matrix to identify
#' variables related to the incomplete variable and (2) Cohen's d comparing cases
#' with and without missing values to identify variables related to the probability
#' of missingness.
#'
#' Note that non-numeric variables (i.e., factors, character vectors, and logical
#' vectors) are excluded from to the analysis.
#'
#' @param ...      a matrix or data frame with incomplete data, where missing
#'                 values are coded as \code{NA}. Alternatively, an expression
#'                 indicating the variable names in \code{data} e.g.,
#'                 \code{na.auxiliary(x1, x2, x3, data = dat)}. Note that the
#'                 operators \code{.}, \code{+}, \code{-}, \code{~}, \code{:},
#'                 \code{::}, and \code{!} can also be used to select variables,
#'                 see 'Details' in the \code{\link{df.subset}} function.
#' @param tri      a character string indicating which triangular of the correlation
#'                 matrix to show on the console, i.e., \code{both} for upper and
#'                 lower triangular, \code{lower} (default) for the lower triangular,
#'                 and \code{upper} for the upper triangular.
#' @param weighted logical: if \code{TRUE} (default), the weighted pooled standard
#'                 deviation is used.
#' @param correct  logical: if \code{TRUE}, correction factor for Cohen's d to
#'                 remove positive bias in small samples is used.
#' @param digits   integer value indicating the number of decimal places digits
#'                 to be used for displaying correlation coefficients and Cohen's d
#'                 estimates.
#' @param as.na    a numeric vector indicating user-defined missing values,
#'                 i.e. these values are converted to \code{NA} before conducting
#'                 the analysis.
#' @param write    a character string naming a text file with file extension
#'                 \code{".txt"} (e.g., \code{"Output.txt"}) for writing the
#'                 output into a text file.
#' @param append   logical: if \code{TRUE} (default), output will be appended
#'                 to an existing text file with extension \code{.txt} specified
#'                 in \code{write}, if \code{FALSE} existing text file will be
#'                 overwritten.
#' @param check    logical: if \code{TRUE} (default), argument specification is checked.
#' @param output   logical: if \code{TRUE} (default), output is shown on the console.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{as.na}}, \code{\link{na.as}}, \code{\link{na.coverage}},
#' \code{\link{na.descript}}, \code{\link{na.indicator}}, \code{\link{na.pattern}},
#' \code{\link{na.prop}}, \code{\link{na.test}}
#'
#' @references
#' Enders, C. K. (2010). \emph{Applied missing data analysis}. Guilford Press.
#'
#' Graham, J. W. (2009). Missing data analysis: Making it work in the real world.
#' \emph{Annual Review of Psychology, 60}, 549-576.
#' https://doi.org/10.1146/annurev.psych.58.110405.085530
#'
#' van Buuren, S. (2018). \emph{Flexible imputation of missing data} (2nd ed.).
#' Chapman & Hall.
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
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries: function call (\code{call}), type of analysis \code{type}, matrix or
#' data frame specified in \code{x} (\code{data}), specification of function arguments
#' (\code{args}), and list with results (\code{result}).
#'
#' @export
#'
#' @examples
#' # Example 1a: Auxiliary variables
#' na.auxiliary(airquality)
#'
#' # Example 1b: Alternative specification using the 'data' argument
#' na.auxiliary(., data = airquality)
#'
#' \dontrun{
#' # Example 2: Write Results into a text file
#' na.auxiliary(airquality, write = "NA_Auxiliary.txt")
#' }
na.auxiliary <- function(..., data = NULL, tri = c("both", "lower", "upper"),
                         weighted = FALSE, correct = FALSE, digits = 2, as.na = NULL,
                         write = NULL, append = TRUE, check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Functions ------------------------------------------------------------------

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
    data <- as.data.frame(data[, var.formula], stringsAsFactors = FALSE)

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

    sd.group <- sqrt(((n.group[1L] - 1L)*var.group[1] + (n.group[2L] - 1L)*var.group[2L]) / (sum(n.group) - 2L))

    # Unweigted pooled standard deviation
    } else {

      sd.group <- sum(var.group) / 2L

    }

    #........................................
    # Cohen's d estimate

    estimate <- x.diff / sd.group

    #........................................
    # Correction factor

    # Bias-corrected Cohen's d
    if (isTRUE(correct && weighted)) {

      v <- sum(n.group) - 2L

      # Correction factor based on gamma function
      if (isTRUE(sum(n.group) < 200L)) {

        corr.factor <- gamma(0.5*v) / ((sqrt(v / 2)) * gamma(0.5 * (v - 1L)))

      # Correction factor based on approximation
      } else {

        corr.factor <- (1L - (3L / (4L * v - 1L)))

      }

      estimate <- estimate*corr.factor

    }

    return(estimate)

  }

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
    var.names <- .var.names(..., data = data, check.chr = "a matrix or data frame")

    # Extract data
    x <- data[, var.names]

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Data without using the argument 'data' ####

  } else {

    # Extract data
    x <- eval(..., enclos = parent.frame())

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## As data frame ####

  x <- as.data.frame(x, stringsAsFactors = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Exclude factors, character vectors and logical vectors ####

  x.num <- vapply(x, function(y) is.numeric(y), FUN.VALUE = logical(1))

  if (isTRUE(any(!x.num))) {

    # Select numeric variables
    x <- x[, which(x.num)]

    warning(paste0("Non-numeric variables excluded from the analysis: ", paste(names(x.num)[!x.num], collapse = ", ")), call. = FALSE)


  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) { x <- .as.na(x, na = as.na) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # No missing values
    if (isTRUE(all(!is.na(x)))) { stop("There are no missing values (NA) in the matrix or data frame specified in 'x'.", call. = FALSE) }

    # Check input 'tri'
    if (isTRUE(any(!tri %in% c("both", "lower", "upper")))) { stop("Character string in the argument 'tri' does not match with \"both\", \"lower\", or \"upper\".", call. = FALSE) }

    # Check input 'weighted'
    if (isTRUE(!is.logical(weighted))) { stop("Please specify TRUE or FALSE for the argument 'weighted'.", call. = FALSE) }

    # Check input 'correct'
    if (isTRUE(!is.logical(correct))) { stop("Please specify TRUE or FALSE for the argument 'correct'.", call. = FALSE) }

    # Check input 'digits'
    if (isTRUE(digits %% 1L != 0L | digits < 0L)) { stop("Specify a positive integer number for the argument 'digits'.", call. = FALSE) }

    # Check input 'write'
    if (isTRUE(!is.null(write) && substr(write, nchar(write) - 3L, nchar(write)) != ".txt")) { stop("Please specify a character string with file extenstion '.txt' for the argument 'write'.") }

    # Check input 'append'
    if (isTRUE(!is.logical(append))) { stop("Please specify TRUE or FALSE for the argument 'append'.", call. = FALSE) }

    # Check input 'output'
    if (isTRUE(!is.logical(output))) { stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Print triangular ####

  tri <- ifelse(all(c("both", "lower", "upper") %in% tri), "lower", tri)

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Variables related to the incomplete variable ####

  cor.mat <- cor(x, use = "pairwise.complete.obs")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Variables related to the probability of missingness ####

  # Indicator matrix
  ind <- misty::na.indicator(x)
  colnames(ind) <- paste0(colnames(ind), "_ind")

  # Pairwise combinations
  x.combn <- combn(ncol(x), m = 2L)

  # Data
  x.ind <- data.frame(x, ind, stringsAsFactors = FALSE)

  #...................
  ### Cohen's d ####

  result.d.upp <- numeric(ncol(x.combn))
  result.d.low <- numeric(ncol(x.combn))
  for (i in seq_len(ncol(x.combn))) {

    temp <- x.combn[, i]

    if (isTRUE(length(unique(x.ind[, colnames(ind)[temp[1L]]])) == 2L &&
        all(tapply(x.ind[,  names(x)[temp[2L]]], x.ind[colnames(ind)[temp[1]]], function(y) length(unique(na.omit(y)))) > 0L))) {

      result.d.upp[i] <- eval(parse(text = paste0("cohens.d.na.auxiliary(", names(x)[temp[2L]], " ~ ", colnames(ind)[temp[1L]],
                                                  ", data = x.ind, weighted = weighted, correct = correct)")))

    } else {

      result.d.upp[i] <- NA

    }

    if (isTRUE(length(unique(x.ind[, colnames(ind)[temp[2L]]])) == 2L &&
        all(tapply(x.ind[,  names(x)[temp[1L]]], x.ind[colnames(ind)[temp[2]]], function(y) length(unique(na.omit(y)))) > 0L))) {

      result.d.low[i] <- eval(parse(text = paste0("cohens.d.na.auxiliary(", names(x)[temp[1L]], " ~ ", colnames(ind)[temp[2L]],
                                                  ", data = x.ind, weighted = weighted, correct = correct)")))

    } else {

      result.d.low[i] <- NA

    }

  }

  #...................
  ### Cohen's d matrix ####

  d.mat <-  matrix(NA, ncol = ncol(x), nrow = ncol(x), dimnames = list(names(x), names(x)))

  d.mat[rev(upper.tri(d.mat))] <- result.d.upp
  d.mat <- t(d.mat)

  d.mat[lower.tri(d.mat)] <- result.d.low

  #_____________________________________________________________________________
  #
  # Return object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "na.auxiliary",
                 data = x,
                 args = list(tri = tri, weighted = weighted, correct = correct, digits = digits,
                             as.na = as.na, write = write, append = append, check = check, output = output),
                 result = list(cor = cor.mat, d = d.mat))

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Write Results --------------------------------------------------------------

  if (isTRUE(!is.null(write))) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Text file ####

    # Send R output to textfile
    sink(file = write, append = ifelse(isTRUE(file.exists(write)), append, FALSE), type = "output", split = FALSE)

    if (isTRUE(append && file.exists(write))) { write("", file = write, append = TRUE) }

    # Print object
    print(object, check = FALSE)

    # Close file connection
    sink()

  }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
