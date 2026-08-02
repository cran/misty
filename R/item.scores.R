#' Scale Scores
#'
#' This function computes (prorated) scale scores by averaging the (available)
#' items that measure a single construct by default.
#'
#' @param data     a data frame with numeric vectors.
#' @param ...      an expression indicating the variable names in \code{data},
#'                 e.g., \code{item.scores(dat, x1, x2, x3)}. Note that the
#'                 operators \code{+}, \code{-}, \code{~}, \code{:},
#'                 \code{::}, and \code{!} can also be used to select variables,
#'                 see 'Details' in the \code{\link{df.subset}} function.
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
#' @details
#' #' Prorated mean scale scores are computed by averaging the available items, e.g.,
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
#' # Reverse code inverted item
#' data.items <- item.reverse(data.items, pitem2, pitem3, min = 0, max = 3)
#'
#' # Example 1: Prorated mean scale scores
#' item.scores(data.items, pitem1, pitem2.r, pitem3.r, pitem4::pitem6)
#'
#' # Example 2: Prorated standard deviation scale scores
#' item.scores(data.items, pitem1, pitem2.r, pitem3.r, pitem4::pitem6, fun = "sd")
#'
#' # Example 3: Sum scale scores without proration
#' item.scores(data.items, pitem1, pitem2.r, pitem3.r, pitem4::pitem6, fun = "sum",
#'             prorated = FALSE)
#'
#' # Example 4: Prorated mean scale scores,
#' # minimum proportion of available item responses = 0.8
#' item.scores(data.items, pitem1, pitem2.r, pitem3.r, pitem4::pitem6, p.avail = 0.8)
#'
#' # Example 5: Prorated mean scale scores,
#' # minimum number of available item responses = 3
#' item.scores(data.items, pitem1, pitem2.r, pitem3.r, pitem4::pitem6, n.avail = 3)
item.scores <- function(data, ..., fun = c("mean", "sum", "median", "var", "sd", "min", "max"),
                        prorated = TRUE, p.avail = NULL, n.avail = NULL,
                        append = TRUE, name = "scores", as.na = NULL, check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'data' is missing or NULL
  if (isTRUE(missing(data) || is.null(data))) { stop("Please specify a data frame for the argument 'data'", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Using the Argument '...' ####

  if (isTRUE(!missing(...))) {

    # Extract data and convert tibble into data frame or vector
    x <- as.data.frame(data[, .var.names(data = data, ...)])

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Without Using the Argument '...' ####

  } else {

    # Data frame
    x <- as.data.frame(data)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert User-Missing Values into NA ####

  if (isTRUE(!is.null(as.na))) { x <- .as.na(x, na = as.na) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Number of Item Responses ####

  x.miss <- apply(x, 1L, function(y) sum(!is.na(y)))

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("prorated", "append"),
               numeric = list(p.avail = 1L, n.avail = 1L), character = list(name = 1L),
               s.character = list(fun = c("mean", "sum", "median", "var", "sd", "min", "max")), envir = environment(), input.check = check)

  # Additional checks
  if (isTRUE(check)) {

    # Check input 'data'
    if (isTRUE(any(apply(x, 2L, function(y) !is.numeric(y))))) { stop("Please specify a data frame with numeric vectors for the argument 'data'.", call. = FALSE) }

    # Check argument p.avail
    if (isTRUE(!is.null(p.avail) && !is.null(n.avail))) { stop("Please specify either argument 'p.avail' or 'n.avail', but not both arguments.", call. = FALSE) }

    # Check argument p.avail and prorated
    if (isTRUE(!prorated && (!is.null(p.avail) | !is.null(n.avail)))) { warning("Argument specification 'prorated = FALSE' is equivalent to 'p.avail = 1' or 'n.avail = ncol(x).", call. = FALSE) }

   }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Function Used to Compute Scale Scores ####

  fun <- ifelse(all(c("mean", "sum", "median", "var", "sd", "min", "max") %in% fun), "mean", fun)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Proration ####

  if (isTRUE(prorated)) {

    # avail = NULL, n.avail = NULL
    if (isTRUE(is.null(p.avail) && is.null(n.avail))) {

      # Threshold for proration = p.avail
      n.items <- 1L

    }


    if (isTRUE(!is.null(p.avail) && is.null(n.avail))) {

      # Threshold for proration = p.avail
      n.items <- ceiling(p.avail * ncol(x))

    }


    if (isTRUE(is.null(p.avail) && !is.null(n.avail))) {

      # Threshold for proration = n.avail
      n.items <- n.avail

    }

  } else {

    # Threshold for proration = 1
    n.items <- ncol(x)

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Mean ####

  switch(fun, mean = {

    object <- misty::as.na(rowMeans(x, na.rm = TRUE), na = "NaN", check = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Sum ####

  }, sum = {

    object <- misty::as.na(rowMeans(x, na.rm = TRUE)*ncol(x), na = "NaN", check = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Median ####

  }, median = {

    object <- apply(x, 1L, function(y) ifelse(length(na.omit(y)) > 0L, median(y, na.rm = TRUE), NA))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Variance ####

  }, var = {

    object <- apply(x, 1L, function(y) ifelse(length(na.omit(y)) > 0L, var(y, na.rm = TRUE), NA))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Standard deviation ####

  }, sd = {

    object <- apply(x, 1L, function(y) ifelse(length(na.omit(y)) > 0L, sd(y, na.rm = TRUE), NA))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Minimum ####

  }, min = {

    object <- apply(x, 1L, function(y) ifelse(length(na.omit(y)) > 0L, min(y, na.rm = TRUE), NA))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Maximum ####

  }, max = {

    object <- apply(x, 1L, function(y) ifelse(length(na.omit(y)) > 0L, max(y, na.rm = TRUE), NA))

  })

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Missing ####

  if (isTRUE(any(x.miss < n.items))) { object[which(x.miss < n.items)] <- NA }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Append ####

  if (isTRUE(!missing(...) && append)) { object <- data.frame(data,  setNames(as.data.frame(object), nm = name)) }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  return(object)

}

#_______________________________________________________________________________
