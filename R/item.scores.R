#' Scale Scores
#'
#' This function computes (prorated) scale scores by averaging the (available)
#' items that measure a single construct by default.
#'
#' Prorated mean scale scores are computed by averaging the available items, e.g.,
#' if a participant answers 4 out of 8 items, the prorated scale score is the
#' average of the 4 responses. Averaging the available items is equivalent to
#' substituting the mean of a participant's own observed items for each of the
#' participant's missing items, i.e., \emph{person mean imputation} (Mazza, Enders
#' & Ruehlman, 2015) or \emph{ipsative mean imputation} (Schafer & Graham, 2002).
#'
#' Proration may be reasonable when (1) a relatively high proportion of the items
#' (e.g., 0.8) and never fewer than half are used to form the scale score, (2)
#' means of the items comprising a scale are similar and (3) the item-total
#' correlations are similar (Enders, 2010; Graham, 2009; Graham, 2012). Results
#' of simulation studies indicate that proration is prone to substantial bias
#' when either the item means or the inter-item correlation vary (Lee, Bartholow,
#' McCarthy, Pederson & Sher, 2014; Mazza et al., 2015).
#'
#' @param ...      a matrix or data frame with numeric vectors. Alternatively, an
#'                 expression indicating the variable names in \code{data} e.g.,
#'                 \code{item.scores(x1, x2, x3, data = dat)}. Note that the
#'                 operators \code{.}, \code{+}, \code{-}, \code{~}, \code{:},
#'                 \code{::}, and \code{!} can also be used to select variables,
#'                 see 'Details' in the \code{\link{df.subset}} function.
#' @param data     a data frame when specifying one or more variables in the
#'                 argument \code{...}. Note that the argument is \code{NULL}
#'                 when specifying a matrix or data frame for the argument
#'                 \code{...}.
#' @param fun      a character string indicating the function used to compute
#'                 scale scores, default: \code{"mean"}.
#' @param prorated logical: if \code{TRUE} (default), prorated scale scores are
#'                 computed (see 'Details'); if \code{FALSE}, scale scores of
#'                 only complete cases are computed.
#' @param p.avail  a numeric value indicating the minimum proportion of available
#'                 item responses needed for computing a prorated scale score for
#'                 each case, e.g. \code{p.avail = 0.8} indicates that scale scores
#'                 are only computed for cases with at least 80\% of item responses
#'                 available. By default prorated scale scores are computed for
#'                 all cases with at least one item response. Note that either
#'                 argument \code{p.avail} or \code{n.avail} is used to specify
#'                 the proration criterion.
#' @param n.avail  an integer indicating the minimum number of available item
#'                 responses needed for computing a prorated scale score for each
#'                 case, e.g. \code{n.avail = 2} indicates that scale scores are
#'                 only computed for cases with item responses on at least 2 items.
#'                 By default prorated scale scores are computed for all cases
#'                 with at least one item response. Note that either argument
#'                 \code{p.avail} or \code{n.avail} is used to specify the proration
#'                 criterion.
#' @param append   logical: if \code{TRUE} (default), a variable with scale scores
#'                 is appended to the data frame specified in the argument \code{data}.
#' @param name     a character string indicating the names of the variable appended
#'                 to the data frame specified in the argument \code{data} when
#'                 \code{append = TRUE}. By default, the variable is named \code{scores}.
#' @param as.na    a numeric vector indicating user-defined missing values,
#'                 i.e. these values are converted to \code{NA} before conducting
#'                 the analysis.
#' @param check    logical: if \code{TRUE} (default), argument specification is checked.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{cluster.scores}}, \code{\link{item.alpha}}, \code{\link{item.cfa}},
#' \code{\link{item.omega}},
#'
#' @references
#' Enders, C. K. (2010). \emph{Applied missing data analysis}. New York, NY: Guilford
#' Press.
#'
#' Graham, J. W. (2009). Missing data analysis: Making it work in the real world.
#' \emph{Annual Review of Psychology, 60}, 549-576.
#' https://doi.org/10.1146/annurev.psych.58.110405.085530
#'
#' Graham, J. W. (2012). Missing data: Analysis and design. New York, NY: Springer
#'
#' Lee, M. R., Bartholow, B. D., McCarhy, D. M., Pederson, S. L., & Sher, K. J. (2014).
#' Two alternative approaches to conventional person-mean imputation scoring of the
#' self-rating of the effects of alcohol scale (SRE). \emph{Psychology of Addictive Behaviors, 29},
#' 231-236. https://doi.org/10.1037/adb0000015
#'
#' Mazza, G. L., Enders, C. G., & Ruehlman, L. S. (2015). Addressing item-level missing
#' data: A comparison of proration and full information maximum likelihood estimation.
#' \emph{Multivariate Behavioral Research, 50}, 504-519.
#' https://doi.org/10.1080/00273171.2015.1068157
#'
#' Schafer, J. L., & Graham, J. W. (2002). Missing data: Our view of the state of
#' the art. \emph{Psychological Methods, 7}, 147-177.' https://doi.org/10.1037/1082-989X.7.2.147
#'
#' @return
#' Returns a numeric vector with the same length as \code{nrow(x)} containing (prorated)
#' scale scores.
#'
#' @export
#'
#' @examples
#' dat <- data.frame(item1 = c(3,  2,  4, 1,  5, 1,  3, NA),
#'                   item2 = c(2,  2, NA, 2,  4, 2, NA,  1),
#'                   item3 = c(1,  1,  2, 2,  4, 3, NA, NA),
#'                   item4 = c(4,  2,  4, 4, NA, 2, NA, NA),
#'                   item5 = c(3, NA, NA, 2,  4, 3, NA,  3))
#'
#' # Example 1a: Prorated mean scale scores
#' item.scores(dat)
#'
#' # Example 1b: Alternative specification using the 'data' argument
#' item.scores(., data = dat)
#'
#' # Example 2: Prorated standard deviation scale scores
#' item.scores(dat, fun = "sd")
#'
#' # Example 3: Sum scale scores without proration
#' item.scores(dat, fun = "sum", prorated = FALSE)
#'
#' # Example 4: Prorated mean scale scores,
#' # minimum proportion of available item responses = 0.8
#' item.scores(dat, p.avail = 0.8)
#'
#' # Example 5: Prorated mean scale scores,
#' # minimum number of available item responses = 3
#' item.scores(dat, n.avail = 3)
item.scores <- function(..., data = NULL, fun = c("mean", "sum", "median", "var", "sd", "min", "max"),
                        prorated = TRUE, p.avail = NULL, n.avail = NULL,
                        append = TRUE, name = "scores", as.na = NULL, check = TRUE) {

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

    # Extract variables
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

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # Check input 'x'
    if (isTRUE(!is.matrix(x) && !is.data.frame(x))) { stop("Please specify a matrix or data frame for the argument 'x'.", call. = FALSE) }

    # Check input 'x'
    if (isTRUE(any(apply(x, 2L, function(y) !is.numeric(y))))) { stop("Please specify a matrix or data frame with numeric vectors for the argument 'x'.", call. = FALSE) }

    # Check input 'fun'
    if (isTRUE(!all(fun %in%  c("mean", "sum", "median", "var", "sd", "min", "max")))) { stop("Character strings in the argument 'fun' dos not match with \"mean\", \"sum\", \"median\", \"var\", \"sd\", \"min\", or \"max\".", call. = FALSE) }

    # Check argument p.avail
    if (isTRUE(!is.null(p.avail))) { if (isTRUE(!p.avail > 0L || !p.avail <= 1L)) { stop("Please specify a number greater than 0 and less than or equal 1 for the argument 'p.avail'.", call. = FALSE) } }

    # Check argument n.avail
    if (isTRUE(!is.null(n.avail))) { if (isTRUE(!n.avail >= 1L || !n.avail <= ncol(x))) { stop("Please specify a number greater than or equal 1 and less than or equal the number of items for the argument 'n.avail'.", call. = FALSE) } }

    # Check argument p.avail
    if (isTRUE(!is.null(p.avail) && !is.null(n.avail))) { stop("Please specify either argument 'p.avail' or 'n.avail', but not both arguments.", call. = FALSE) }

    # Check argument p.avail and prorated
    if (isTRUE(!prorated && (!is.null(p.avail) | !is.null(n.avail)))) { warning("Argument specification 'prorated = FALSE' is equivalent to 'p.avail = 1' or 'n.avail = ncol(x).", call. = FALSE) }

    # Check input 'append'
    if (isTRUE(!is.logical(append))) { stop("Please specify TRUE or FALSE for the argument 'append'.", call. = FALSE) }

   }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Function used to compute scale scores ####

  fun <- ifelse(all(c("mean", "sum", "median", "var", "sd", "min", "max") %in% fun), "mean", fun)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Number of item responses ####

  x.miss <- apply(x, 1L, function(y) sum(!is.na(y)))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Proration ####

  if (isTRUE(prorated)) {

    # avail = NULL, n.avail = NULL
    if (isTRUE(is.null(p.avail) && is.null(n.avail))) {

      # Thresdhold for proraton = p.avail
      n.items <- 1L

    }


    if (isTRUE(!is.null(p.avail) && is.null(n.avail))) {

      # Thresdhold for proraton = p.avail
      n.items <- ceiling(p.avail * ncol(x))

    }


    if (isTRUE(is.null(p.avail) && !is.null(n.avail))) {

      # Thresdhold for proraton = n.avail
      n.items <- n.avail

    }

  } else {

    # Thresdhold for proration = 1
    n.items <- ncol(x)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) { x <- .as.na(x, na = as.na) }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Mean ####
  switch(fun, mean = {

    object <- misty::as.na(rowMeans(x, na.rm = TRUE), na = "NaN", check = FALSE)
    object[which(x.miss < n.items)] <- NA

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Sum ####
  }, sum = {

    object <- misty::as.na(rowMeans(x, na.rm = TRUE)*ncol(x), na = "NaN", check = FALSE)
    object[which(x.miss < n.items)] <- NA

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Median ####
  }, median = {

    object <- apply(x, 1L, function(y) ifelse(length(na.omit(y)) > 0L, median(y, na.rm = TRUE), NA))
    object[which(x.miss < n.items)] <- NA

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Variance ####
  }, var = {

    object <- apply(x, 1L, function(y) ifelse(length(na.omit(y)) > 0L, var(y, na.rm = TRUE), NA))
    object[which(x.miss < n.items)] <- NA

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Standard deviation ####
  }, sd = {

    object <- apply(x, 1L, function(y) ifelse(length(na.omit(y)) > 0L, sd(y, na.rm = TRUE), NA))
    object[which(x.miss < n.items)] <- NA

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Minimum ####
  }, min = {

    object <- apply(x, 1L, function(y) ifelse(length(na.omit(y)) > 0L, min(y, na.rm = TRUE), NA))
    object[which(x.miss < n.items)] <- NA

  #-----------------------------------------
  # Maximum
  }, max = {

    object <- apply(x, 1L, function(y) ifelse(length(na.omit(y)) > 0L, max(y, na.rm = TRUE), NA))
    object[which(x.miss < n.items)] <- NA

  })

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Append ####

  if (isTRUE(!is.null(data) && append)) { object <- data.frame(data,  setNames(as.data.frame(object), nm = name)) }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  return(object)

}
