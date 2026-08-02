#' Coefficient Alpha, Hierarchical Alpha, and Ordinal Alpha
#'
#' This function computes point estimate and confidence interval for the coefficient
#' alpha (aka Cronbach's alpha) and ordinal coefficient alpha (aka categorical
#' alpha) along with corrected item-total correlations or standardized factor loadings
#' and coefficient alphas if the item is deleted. By default, the function computes
#' the formula-based coefficient alpha using pairwise deletion in the presence
#' of missing data.
#'
#' @param data       a data frame. Note that at least two items are needed for
#'                   computing coefficient alpha
#' @param ...        an expression indicating the variable names in \code{data}
#'                   e.g., \code{item.alpha(dat, x1, x2, x3)}. Note that the
#'                   operators \code{+}, \code{-}, \code{~}, \code{:},
#'                   \code{::}, and \code{!} can also be used to select variables,
#'                   see 'Details' in the \code{\link{df.subset}} function.
#' @param rescov     a character vector or a list of character vectors for
#'                   specifying residual covariances when computing coefficient
#'                   alpha, e.g. \code{rescov = c("x1", "x2")} for specifying
#'                   a residual covariance between items \code{x1} and \code{x2}
#'                   or \code{rescov = list(c("x1", "x2"), c("x3", "x4"))} for
#'                   specifying residual covariances between items \code{x1} and
#'                   \code{x2}, and items \code{x3} and \code{x4}.
#' @param ordered    logical: if \code{TRUE}, variables are treated as ordered
#'                   (ordinal) variables to compute ordinal coefficient alpha.
#' @param exclude    a character vector indicating items to be excluded from the
#'                   analysis.
#' @param correct    logical: if \code{TRUE} (default), the corrected item-total
#'                   correlation is computed.
#' @param std        logical: if \code{TRUE}, the standardized coefficient alpha
#'                   is computed.
#' @param estimator  a character string indicating the estimator to be used
#'                   (see 'Details' in the \code{\link{item.cfa}} function) when
#'                   specifying residual covariances using the \code{rescov}
#'                   argument or when using full information maximum likelihood
#'                   method for missing data handling, i.e., \code{missing = "fiml"}.
#'                   By default, \code{"ULS"} is used for computing coefficient
#'                   alpha in the presence of residual covariances. Note that
#'                   the argument \code{estimator} switches to \code{"ML"} when
#'                   specifying \code{missing = "fiml"}.
#' @param missing    a character string indicating how to deal with missing data.
#'                   (see 'Details' in the \code{\link{item.cfa}} function). By
#'                   default, pairwise deletion (\code{missing = "pairwise"}) is
#'                   used for computing coefficient alpha and ordinal coefficient
#'                   alpha. Full information maximum likelihood (FIML) method is
#'                   available for estimating coefficient alpha and is requested
#'                   by specifying \code{missing = "fiml"} along with \code{estimator = "ML"}.
#'                   Note that FIML method is not available for computing ordinal
#'                   coefficient alpha.
#' @param print      a character vector indicating which results to show, i.e.
#'                   \code{"all"} for all results \code{"alpha"} (default) for
#'                   the coefficient alpha, and \code{"item"} for item statistics.
#' @param digits     an integer value indicating the number of decimal places to
#'                   be used for displaying mean, standard deviation, minimum,
#'                   and maximum.
#' @param r.digits   an integer value indicating the number of decimal places to
#'                   be used for displaying alpha, item-total correlations, and
#'                   standardized factor loadings.
#' @param conf.level a numeric value between 0 and 1 indicating the confidence
#'                   level of the interval.
#' @param as.na      a numeric vector indicating user-defined missing values,
#'                   i.e. these values are converted to \code{NA} before conducting
#'                   the analysis.
#' @param write      a character string naming a file for writing the output into
#'                   either a text file with file extension \code{".txt"} (e.g.,
#'                   \code{"Output.txt"}) or Excel file with file extension
#'                   \code{".xlsx"}  (e.g., \code{"Output.xlsx"}). If the file
#'                   name does not contain any file extension, an Excel file will
#'                   be written.
#' @param append     logical: if \code{TRUE} (default), output will be appended
#'                   to an existing text file with extension \code{.txt} specified
#'                   in \code{write}, if \code{FALSE} existing text file will be
#'                    overwritten.
#' @param check      logical: if \code{TRUE} (default), argument specification
#'                   is checked.
#' @param output     logical: if \code{TRUE} (default), output is shown.
#'
#' @details
#'
#' \describe{
# \describe{
#   \item{\strong{Coefficient Alpha}}{This function computes the coefficient alpha
#   using either a formula-based method or a confirmatory factor analysis (CFA).
#   The latter conducts a CFA based on the essentially tau-equivalent measurement
#   model (Graham, 2006) using the \code{cfa()} function in the \pkg{lavaan}
#   package by Yves Rosseel (2019). By default, the function employs the formula-based
#   method and uses listwise deletion to handle missing values. The function
#   switches to the CFA-based method when residual covariances are specified using
#   the \code{rescov} argument, when full information maximum likelihood (FIML)
#   method is requested for missing data handling by specifying \code{missing = "fiml"},
#   or when the \code{estimator} argument is set to any other estimation method other
#   than the default estimator \code{ULS}.}
#   \item{\strong{Ordinal Coefficient Alpha}}{The ordinal coefficient alpha (Zumbo
#   et al., 2007) is calculated by applying the formula for coefficient alpha to
#   the polychoric correlation matrix, rather than to the the variance-covariance
#   or product-moment correlation matrix. The ordinal coefficient alpha
#   should be interpreted only as a hypothetical estimate of an alternative reliability,
#   where a test's ordinal categorical response options have been modified to include
#   an infinite number of response options and concludes that coefficient
#   alpha should not be reported as a measure of a test's reliability. However,
#   Zumbo and Kroc (2019) argued that Chalmers' critique of ordinal coefficient
#   alpha is unfounded, and that ordinal coefficient alpha may be the most
#   appropriate quantifier of reliability when using Likert-type measurement to
#   study a latent continuous random variable.}
#   \item{\strong{Confidence Interval}}{The confidence interval for the (ordinal)
#   coefficient alpha is computed using the procedure by Feldt et al. (1987).
#   Note that there are at least 10 other procedures for computing the confidence
#   interval (see Kelley and Pornprasertmanit, 2016), which are implemented in the
#   \code{ci.reliability()} function in the \pkg{MBESSS} package by Ken Kelley (2019).}
# }
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
#' Fox, J. (2025). \emph{polycor: Polychoric and polyserial correlations}.
#' R package version 0.8-2. https://doi.org/10.32614/CRAN.package.polycor
#'
#' Graham, J. M. (2006). Congeneric and (essentially) tau-equivalent estimates of
#' score reliability: What they are and how to use them. \emph{Educational and
#' Psychological Measurement, 66}(6), 930–944. https://doi.org/10.1177/0013164406288165
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
#'
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{data}}{data frame used for the current analysis}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{model.fit}}{fitted lavaan object (\code{mod.fit})}
#' \item{\code{result}}{list with result tables, i.e., \code{alpha} for a table
#'                      with coefficient alpha and \code{itemstat} for a table
#'                      with item statistics}
#'
#' @note
#' Computation of the polyserial correlation coefficient is based on the \code{polyserial()}
#' function in the \pkg{polycor} package by John Fox (2025)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load data set "HolzingerSwineford1939" in the lavaan package
#' data("HolzingerSwineford1939", package = "lavaan")
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Continuous Data
#'
#' # Example 1a: Coefficient alpha, listwise deletion
#' item.alpha(HolzingerSwineford1939, x1::x9)
#'
#' # Example 1b: Full information maximum likelihood method
#' item.alpha(HolzingerSwineford1939, x1::x9, estimator = "ML", missing = "fiml")
#'
#' # Example 2: Coefficient alpha and item statistics after excluding 'x3'
#' item.alpha(HolzingerSwineford1939, x1::x9, exclude = "x3", print = "all")
#'
#' # Example 3a: Residual covariance between 'x1' and 'x2'
#' item.alpha(HolzingerSwineford1939, x1::x9, rescov = c("x1", "x2"))
#'
#' # Example 3b: Residual covariances between 'x1' and 'x2', and 'x2' and 'x3'
#' item.alpha(HolzingerSwineford1939, x1::x9, rescov = list(c("x1", "x2"), c("x2", "x3")))
#'
#' # Example 4: Summary of the CFA model used to compute coefficient alpha
#' lavaan::summary(item.alpha(HolzingerSwineford1939, x1::x9, estimator = "ML", output = FALSE)$model.fit,
#'                 standardized = TRUE)
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Polytomous Data
#'
#' # Example 5: Ordinal coefficient alpha and item statistics
#' item.alpha(data.items, pitem1, pitem2r, pitem3r, pitem4::pitem6, ordered = TRUE,
#'            print = "all")
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Write Results
#'
#' # Example 6a: Write Results into a text file
#' item.alpha(HolzingerSwineford1939, x1::x9, print = "all", write = "Alpha.txt")
#'
#' # Example 6b: Write Results into an Excel file
#' item.alpha(HolzingerSwineford1939, x1::x9, print = "all", write = "Alpha.xlsx")
#' }
item.alpha <- function(data, ..., rescov = NULL, ordered = FALSE, exclude = NULL,
                       correct = TRUE, std = FALSE,
                       estimator = c("ML", "GLS", "WLS", "DWLS", "ULS", "PML"),
                       missing = c("listwise", "pairwise", "fiml"),
                       print = c("all", "alpha", "item"), digits = 2, r.digits = 3,
                       conf.level = 0.95, as.na = NULL, write = NULL, append = TRUE,
                       check = TRUE, output = TRUE) {

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
    x <- as.data.frame(data[, .var.names(data = data, ...), drop = FALSE])

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Without Using the Argument '...' ####

  } else {

    # Data frame
    x <- as.data.frame(data)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Non-Numeric Variables ####

  x <- .exclude.non.numeric(x, func = "item.alpha")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Exclude Items ####

  if (isTRUE(!is.null(exclude))) { x <- x[, which(!colnames(x) %in% exclude)] |> (\(y) if (isTRUE(ncol(y) < 2L)) { stop("At least two items after excluding items are needed to compute coefficient alpha.", call. = FALSE) } else { return(y) })() }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert User-missing Values into NA ####

  if (isTRUE(!is.null(as.na))) { x <- .as.na(x, na = as.na) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("ordered", "std", "append", "output"),
               s.character = list(estimator = c("ML", "GLS", "WLS", "DWLS", "ULS", "PML"), missing = c("listwise", "pairwise", "fiml")),
               m.character = list(print = c("all", "alpha", "item")), args = c("digits", "r.digits", "conf.level"), package = "lavaan", envir = environment(), input.check = check)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Additional Checks ####

  if (isTRUE(check)) {

    #—————————————————————————————————————— #
    ### 'resocv' Argument ####

    if (isTRUE(!is.null(rescov))) {

      # Ordinal coefficient alpha
      if (isTRUE(ordered)) { stop("Residual covariances cannot be specified when computing ordinal coefficient alpha.", call. = FALSE) }

      # More than one residual covariance specified as list
      if (isTRUE(is.list(rescov))) {

        if (isTRUE(any(sapply(rescov, length) != 2L))) { stop("Please specify a list of character vectors, each with two variable names, for the argument 'rescov'.", call. = FALSE) }

      # One residual covariance specified as vector
      } else {

        if (isTRUE(length(rescov) != 2L)) { stop("Please specify a character vector with two variable names, for the argument 'rescov'.", call. = FALSE) }

      }

      # Variables in 'data'
      if (isTRUE(!is.null(rescov))) { unique(unlist(rescov)) |> (\(y) if (isTRUE(any(!y %in% colnames(x)))) { stop(paste0("Items specified in the argument 'rescov' were not found in 'data': ", paste(y[!y %in% colnames(x)], collapse = ", ")), call. = FALSE) })() }

    }

    #—————————————————————————————————————— #
    ### 'estimator' and 'missing' Arguments ####

    if (isTRUE(all(estimator == "ULS") && all(missing == "pairwise"))) { stop("Pairwise deletion is not available when estimator = \"ULS\".", call. = FALSE) }

    if (isTRUE(ordered && all(missing == "fiml"))) { stop("FIML method for missing data handling is not available when computing ordinal coefficient alpha.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'estimator' Argument ####

  if (isTRUE(all(c("ML", "GLS", "WLS", "DWLS", "ULS", "PML") %in% estimator))) {

    estimator <- "ULS"

  } else {

    if (isTRUE(ordered && estimator %in% c("ML", "GLS", "WLS", "DWLS", "PML"))) {

      stop(paste0("Estimator ", estimator, " is not available when computing ordinal coefficient alpha."), call. = FALSE)

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'missing' Argument ####

  #—————————————————————————————————————— #
  ### Missing Values ####

  if (isTRUE(any(is.na(x)))) {

    #···················
    #### Continuous Coefficient Alpha ####

    if (isTRUE(!ordered)) {

      # Default setting
      if (isTRUE(all(c("listwise", "pairwise", "fiml") %in% missing))) {

        missing <- "pairwise"

      # User-specified setting
      } else if (isTRUE(missing == "fiml" && estimator != "ML")) {

        estimator <- "ML"

        warning("Argument 'estimator' switched to \"ML\" to use FIML method for missing data handling.", call. = FALSE)

      }

    #···················
    #### Ordinal Coefficient Alpha ####

    } else {

      # Default setting
      if (isTRUE(all(c("listwise", "pairwise", "fiml") %in% missing))) {

        missing <- "pairwise"

      }

    }

  #—————————————————————————————————————— #
  ### No Missing Values ####

  } else {

    missing <- "listwise"

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'rescov' Argument ####

  if (isTRUE(!is.null(rescov))) {

    if (isTRUE(!is.list(rescov))) { rescov <- list(rescov) }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'print' Argument ####

  if (isTRUE(all(c("all", "alpha", "item") %in% print))) {

    print <- "alpha"

  } else if (isTRUE("all" %in% print)) {

    print <- c("alpha", "item")

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Alpha ####

  alpha <- suppressWarnings(.alpha(y = x, ordered = ordered, rescov = rescov, std = std, estimator = estimator, missing = missing, check = check))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Confidence Interval ####

  #—————————————————————————————————————— #
  ### Formula-Based Coefficient Alpha ####

  if (isTRUE(is.null(alpha$mod.fit))) {

    # Sample size
    n <- ifelse(missing == "listwise", sum(complete.cases(x)), sum(rowMeans(is.na(x)) != 1L))

    # Degrees of freedom
    df1 <- n - 1L
    df2 <- (ncol(x) - 1L) * df1

    # Confidence interval
    restab <- data.frame(n = n, nNA = nrow(x) - n, n.items = ncol(x),
                         alpha = alpha$alpha,
                         low = 1L - (1L - alpha$alpha) * qf(1L - (1L - conf.level) / 2L, df1, df2),
                         upp = 1L - (1L - alpha$alpha) * qf((1L - conf.level) / 2L, df1, df2))

  #—————————————————————————————————————— #
  ### CFA-Based Coefficient Alpha ####

  } else {

    # Degrees of freedom
    df1 <- lavaan::lavInspect(alpha$mod.fit, what = "nobs") - 1L
    df2 <- (ncol(lavaan::lavInspect(alpha$mod.fit, what = "data")) - 1L) * df1

    # Confidence interval
    restab <- data.frame(n = lavaan::lavInspect(alpha$mod.fit, what = "nobs"),
                         nNA = nrow(x) - lavaan::lavInspect(alpha$mod.fit, what = "nobs"),
                         n.items = ncol(lavaan::lavInspect(alpha$mod.fit, what = "data")),
                         alpha = alpha$alpha,
                         low = 1L - (1L - alpha$alpha) * qf(1L - (1L - conf.level) / 2L, df1, df2),
                         upp = 1L - (1L - alpha$alpha) * qf((1L - conf.level) / 2L, df1, df2))

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Descriptive Statistics, Item-Total Correlation or Std. Factor Loadings, and Alpha if Item Deleted ####

  itemstat <- NULL
  if (isTRUE("item" %in% print)) {

    #—————————————————————————————————————— #
    ### Formula-Based Coefficient Alpha ####

    if (isTRUE(is.null(alpha$mod.fit))) {

      # Result table
      itemstat <- matrix(rep(NA, times = ncol(x)*3L), ncol = 3L, dimnames = list(NULL, c("r", "alpha", "d.alpha")))

      #···················
      #### Continuous Data ####

      if (isTRUE(!ordered)) {

        #···················
        #### Item-Total Correlation ####

        if (isTRUE(correct)) {

          itemstat[, "r"] <- sapply(seq_len(ncol(x)), function(y) cor(x[, y], rowMeans(x[, -y, drop = FALSE], na.rm = TRUE), method = "pearson", use = ifelse(missing == "listwise", "complete.obs", "pairwise.complete.obs")))

        } else {

          itemstat[, "r"] <- rowMeans(x, na.rm = TRUE) |> (\(p) sapply(seq_len(ncol(x)), function(y) cor(x[, y], p, method = "pearson", use = ifelse(missing == "listwise", "complete.obs", "pairwise.complete.obs"))))()

        }

        #···················
        #### Coefficient Alpha if Item Deleted ####

        if (isTRUE(ncol(x) > 2L)) {

          itemstat[, "alpha"] <- sapply(seq_len(ncol(x)), function(y) suppressWarnings(misty::item.alpha(x[, -y, drop = FALSE], ordered = FALSE, missing = missing, print = "alpha", check = FALSE, output = FALSE)$result$alpha$alpha))

          itemstat[, "d.alpha"] <- itemstat[, "alpha"] - alpha$alpha

        }

      #···················
      #### Ordered-Categorical Data ####

      } else {

        #···················
        #### Item-Total Correlation ####

        if (isTRUE(correct)) {

          itemstat[, "r"] <- sapply(seq_len(ncol(x)), function(y) suppressWarnings(.cor.polyserial(x[, y], rowMeans(x[, -y, drop = FALSE], na.rm = TRUE), se = FALSE)))

        } else {

          itemstat[, "r"] <- rowMeans(x, na.rm = TRUE) |> (\(p) sapply(seq_len(ncol(x)), function(y) suppressWarnings(.cor.polyserial(x[, y], p, se = FALSE))))()

        }

        #···················
        #### Coefficient Alpha if Item Deleted ####

        if (isTRUE(ncol(x) > 2L)) {

          itemstat[, "alpha"] <- sapply(seq_len(ncol(x)), function(y) suppressWarnings(misty::item.alpha(x[, -y, drop = FALSE], ordered = TRUE, missing = missing, print = "alpha", check = FALSE, output = FALSE)$result$alpha$alpha))

          itemstat[, "d.alpha"] <- itemstat[, "alpha"] - alpha$alpha

        }

      }

    #—————————————————————————————————————— #
    ### CFA-Based Coefficient Alpha ####

    } else {

      # Result table
      itemstat <- matrix(rep(NA, times = ncol(x)*3L), ncol = 3L, dimnames = list(NULL, c("std.ld", "alpha", "d.alpha")))

      #···················
      #### Standardized Factor Loadings ####

      itemstat[, "std.ld"] <- lavaan::inspect(alpha$mod.fit, what = "std")$lambda

      #···················
      #### Coefficient Alpha if Item Deleted ####

      if (isTRUE(ncol(x) > 2L)) {

        ##### No Residual Covariances ####
        if (isTRUE(is.null(rescov))) {

          itemstat[, "alpha"] <- sapply(seq_len(ncol(x)), function(y) suppressWarnings(misty::item.alpha(x[, -y, drop = FALSE], ordered = FALSE, missing = missing, print = "alpha", check = FALSE, output = FALSE)$result$alpha$alpha))

        ##### Residual Covariances ####
        } else {

          for (i in seq_len(ncol(x))) {

            # Residual covariance
            if (isTRUE(!is.null(rescov))) {

              rescov.i <- rescov[-which(unlist(lapply(rescov, function(y) any(y %in% colnames(x)[i]))))] |> (\(y) if (isTRUE(length(y) == 0L)) { NULL } else { y })()

            } else {

              rescov.i <- NULL

            }

            itemstat[i, "alpha"] <- suppressWarnings(.alpha(y = x[, -i], ordered = FALSE, rescov = rescov.i, std = std, estimator = estimator, missing = missing, check = FALSE)$alpha)

          }

        }

        # Difference in coefficient alpha
        itemstat[, "d.alpha"] <- itemstat[, "alpha"] - alpha$alpha

      }

    }

    #—————————————————————————————————————— #
    ### Descriptive Statistics ####

    itemstat <- data.frame(item = colnames(x), misty::descript(x, output = FALSE)$result[, c("n", "nNA", "pNA", "m", "sd", "min", "max")], itemstat)

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "item.alpha",
                 data = x,
                 args = list(rescov = rescov, ordered = ordered, exclude = exclude, correct = correct, std = std, estimator = estimator, missing = missing, print = print, digits = digits, r.digits = r.digits, conf.level = conf.level, as.na = as.na, write = write, append = append, check = check, output = output),
                 model.fit = alpha$mod.fit,
                 result = list(alpha = restab, itemstat = itemstat))

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Write Results --------------------------------------------------------------

  if (isTRUE(!is.null(write))) { .write.result(object = object, write = write, append = append) }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  if (isTRUE(output)) { print(object, check = FALSE) }

  #_____________________________________________________________________________
  #
  # Return ---------------------------------------------------------------------

  return(invisible(object))

}

#_______________________________________________________________________________
