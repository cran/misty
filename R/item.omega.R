#' Coefficient Omega, Hierarchical Omega, and Categorical Omega
#'
#' This function computes point estimate and confidence interval for the coefficient
#' omega (McDonald, 1978), hierarchical coefficient omega (Kelley & Pornprasertmanit,
#' 2016), and categorical coefficient omega (Green & Yang, 2009) along with
#' standardized factor loadings and omega if item deleted. By default, the function
#' computes coefficient omega based on maximum likelihood parameter (ML) estimates
#' using full information maximum likelihood (FIML) method in the presence of
#' missing data.
#'
#' Coefficient omega is computed by conducting a confirmatory factor analysis based
#' on the congeneric measurement model (Graham, 2006) using the \code{cfa()} function in the
#' \pkg{lavaan} package by Yves Rosseel (2019).
#'
#' Approximate confidence intervals are computed using the procedure by Feldt,
#' Woodruff and Salih (1987). Note that there are at least 10 other procedures
#' for computing the confidence interval (see Kelley and Pornprasertmanit, 2016),
#' which are implemented in the \code{ci.reliability()} function in the
#' \pkg{MBESSS} package by Ken Kelley (2019).
#'
#' @param data       a data frame. Note that at least three items are needed for
#'                   computing coefficient omega
#' @param ...        an expression indicating the variable names in \code{data}
#'                   e.g., \code{item.omega(dat, x1, x2, x3)}. Note that the
#'                   operators \code{.}, \code{+}, \code{-}, \code{~}, \code{:},
#'                   \code{::}, and \code{!} can also be used to select variables,
#'                   see 'Details' in the \code{\link{df.subset}} function.
#' @param rescov     a character vector or a list of character vectors for
#'                   specifying residual covariances when computing coefficient
#'                   omega, e.g. \code{rescov = c("x1", "x2")} for specifying
#'                   a residual covariance between items \code{x1} and \code{x2}
#'                   or \code{rescov = list(c("x1", "x2"), c("x3", "x4"))} for
#'                   specifying residual covariances between items \code{x1} and
#'                   \code{x2}, and items \code{x3} and \code{x4}.
#' @param type       a character string indicating the type of omega to be computed,
#'                   i.e., \code{omega} (default) for coefficient omega, \code{hierarch}
#'                   for hierarchical coefficient omega, and \code{categ} for
#'                   categorical coefficient omega.
#' @param exclude    a character vector indicating items to be excluded from the
#'                   analysis.
#' @param std        logical: if \code{TRUE}, the standardized coefficient omega
#'                   is computed.
#' @param estimator  a character string indicating the estimator to be used
#'                   (see 'Details' in the \code{\link{item.cfa}} function).
#'                   By default, \code{"ULS"} is used for computing (hierarchical)
#'                   coefficient omega and \code{"DWLS"} is used for computing
#'                   ordinal coefficient omega.
#' @param missing    a character string indicating how to deal with missing data.
#'                   (see 'Details' in the \code{\link{item.cfa}} function). By
#'                   default, pairwise deletion (\code{missing = "pairwise"}) is
#'                   used for computing (hierarchical) coefficient omega and ordinal
#'                   coefficient omega. Full information maximum likelihood method
#'                   is available for estimating (hierarchical) coefficient omega
#'                   and is requested by specifying \code{missing = "fiml"}
#'                   along with \code{estimator = "ML"}.
#' @param print      a character vector indicating which results to show, i.e.
#'                   \code{"all"} for all results \code{"omega"} (default) for
#'                   the coefficient omega, and \code{"item"} for item statistics.
#' @param digits     an integer value indicating the number of decimal places to
#'                   be used for displaying omega and standardized factor loadings.
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
#' Revelle, W. (2025). \emph{psych: Procedures for psychological, psychometric,
#' and personality research}.  Northwestern University, Evanston, Illinois.
#' R package version 2.5.3,  https://CRAN.R-project.org/package=psych.
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
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{data}}{data frame used for the current analysis}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{model.fit}}{fitted lavaan object (\code{mod.fit})}
#' \item{\code{result}}{list with result tables, i.e., \code{omega} for a table
#'                      with coefficient omega and \code{itemstat} for a table
#'                      with item statistics}
#'
#' @note
#' Computation of the hierarchical and categorical omega is based on the
#' \code{ci.reliability()} function in the \pkg{MBESS} package by Ken Kelley
#' (2019).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(item1 = c(3, NA, 3, 4, 1, 2, 4, 2), item2 = c(5, 3, 3, 2, 2, 1, 3, 1),
#'                   item3 = c(4, 2, 4, 2, 1, 3, 4, 1), item4 = c(4, 1, 2, 2, 1, 3, 4, 3))
#'
#' # Example 1a: Coefficient omega and item statistics, pairwise deletion
#' item.omega(dat)
#'
#' # Example 1b: Coefficient omega and item statistics, listwise deletion
#' item.omega(dat, missing = "listwise")
#'
#' # Example 2: Coefficient omega and item statistics after excluding item3
#' item.omega(dat, exclude = "item3")
#'
#' # Example 3a: Coefficient omega with a residual covariance
#' # and item statistics
#' item.omega(dat, rescov = c("item1", "item2"))
#'
#' # Example 3b: Coefficient omega with residual covariances
#' # and item statistics
#' item.omega(dat, rescov = list(c("item1", "item2"), c("item1", "item3")))
#'
#' # Example 4: Ordinal coefficient omega and item statistics
#' item.omega(dat, type = "categ")
#'
#' # Example 6: Summary of the CFA model used to compute coefficient omega
#' lavaan::summary(item.omega(dat, output = FALSE)$model.fit,
#'                 fit.measures = TRUE, standardized = TRUE)
#'
#' # Example 7a: Write Results into a text file
#' item.omega(dat, write = "Omega.txt")
#'
#' # Example 7b: Write Results into a Excel file
#' item.omega(dat, write = "Omega.xlsx")
#' }
item.omega <- function(data, ..., rescov = NULL,
                       type = c("omega", "hierarch", "categ"), exclude = NULL,
                       std = FALSE, estimator = c("ML", "GLS", "WLS", "DWLS", "ULS", "PML"),
                       missing = c("listwise", "pairwise", "fiml"), print = c("all", "omega", "item"),
                       digits = 2, conf.level = 0.95, as.na = NULL, write = NULL,
                       append = TRUE, check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'data' is missing
  if (isTRUE(missing(data))) { stop("Please specify a data frame for the argument 'data'", call. = FALSE) }

  # Check if input 'data' is NULL
  if (isTRUE(is.null(data))) { stop("Input specified for the argument 'data' is NULL.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data Using the Argument 'data' ####

  if (isTRUE(!missing(...))) {

    # Extract data and convert tibble into data frame or vector
    x <- as.data.frame(data[, .var.names(..., data = data), drop = FALSE])

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data Without Using the Argument 'data' ####

  } else {

    # Data frame
    x <- as.data.frame(data)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Numeric Variables ####

  # Non-numeric variables
  x <- (!vapply(x, function(z) is.numeric(z) | is.ordered(z), FUN.VALUE = logical(1L))) |>
    (\(y) if (isTRUE(any(y))) {

      return(x[, -which(y), drop = FALSE])

      warning(paste0("Non-numeric variables were excluded from the analysis: ", paste(names(which(y)), collapse = ", ")), call. = FALSE)

      # Variables left
      if (isTRUE(all(y))) { stop("No variables left for analysis after excluding non-numeric variables.", call. = FALSE) }

    } else {

      return(x)

    })()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Exclude Items ####

  if (isTRUE(!is.null(exclude))) { x <- x[, which(!colnames(x) %in% exclude)] |> (\(y) if (isTRUE(ncol(y) < 3L)) { stop("At least three items after excluding items are needed to compute coefficient omega.", call. = FALSE) } else { return(y) })() }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert User-missing Values into NA ####

  if (isTRUE(!is.null(as.na))) { x <- .as.na(x, na = as.na) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("std", "append", "output"),
               s.character = list(type = c("omega", "hierarch", "categ"), estimator = c("ML", "GLS", "WLS", "DWLS", "ULS", "PML"), missing = c("listwise", "pairwise", "fiml")),
               m.character = list(print = c("all", "omega", "item")), args = c("digits", "conf.level"), package = "lavaan", envir = environment(), input.check = check)

  # Additional checks
  if (isTRUE(check)) {

    # Package 'mnormt' installed?
    if (isTRUE(all(type == "categ"))) { if (isTRUE(!requireNamespace("mnormt", quietly = TRUE))) { stop("Package \"mnormt\" is needed for this function to work, please install it.", call. = FALSE) } }

    ## Check input 'data' ##

    # At least three items
    if (isTRUE(ncol(x) < 3L)) { stop("Please specify at least three items to compute coefficient omega", call. = FALSE) }

    # Zero variance
    if (isTRUE(nrow(x) != ncol(x))) { vapply(as.data.frame(x), function(y) length(na.omit(unique(y))) == 1L, FUN.VALUE = logical(1L)) |> (\(y) if (isTRUE(any(y))) { stop(paste0("Following variables in the data frame specified in 'data' have zero variance: ", paste(names(which(y)), collapse = ", ")), call. = FALSE) })() }

    ## Check input 'rescov' ##
    if (isTRUE(!is.null(rescov))) {

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

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Type of Omega ####

  if (isTRUE(all(c("omega", "hierarch", "categ") %in% type))) { type <- "omega" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Estimator ####

  # Coefficient omega for continuous Items or hierarchical omega
  if (isTRUE(type %in% c("omega", "hierarch"))) {

    if (isTRUE(all(c("ML", "GLS", "WLS", "DWLS", "ULS", "PML") %in% estimator))) {

      estimator <- "ML"

    }

  # Categorical omega
  } else {

    if (isTRUE(all(c("ML", "GLS", "WLS", "DWLS", "ULS", "PML") %in% estimator))) {

      estimator <- "DWLS"

    } else {

      if (isTRUE(estimator == "ML")) { stop("Estimator \"ML\" is not available for estimating categorical omega.") }

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Missing Data ####

  ### Coefficient omega for continuous Items or hierarchical omega ###
  if (isTRUE(type %in% c("omega", "hierarch"))) {

    if (isTRUE(any(is.na(x)))) {

      if (isTRUE(all(c("listwise", "pairwise", "fiml") %in% missing))) {

        missing <- "fiml"

      } else if (isTRUE(missing == "listwise")) {

        if (any(is.na(x))) { assign("x", na.omit(x)) |> (\(y) warning(paste("Listwise deletion of incomplete data, number of cases removed from the analysis:", length(attributes(y)$na.action)), call. = FALSE))() }

      }

    } else {

      missing <- "listwise"

    }

  ### Categorical omega ###
  } else {

    if (isTRUE(any(is.na(x)))) {

      if (isTRUE(all(c("listwise", "pairwise", "fiml") %in% missing))) {

        missing <- "pairwise"

      } else if (isTRUE(missing == "listwise")) {

        if (any(is.na(x))) { assign("x", na.omit(x)) |> (\(y) warning(paste("Listwise deletion of incomplete data, number of cases removed from the analysis:", length(attributes(y)$na.action)), call. = FALSE))() }

      } else if (isTRUE(missing == "fiml")) {

        missing <- "pairwise"

        warning("FIML method is not available for estimator = \"ML\", argument 'missing' switched to \"pairwise\".", call. = FALSE)

      }

    } else {

      missing <- "listwise"

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Residual Covariance ####

  if (isTRUE(!is.null(rescov) & !is.list(rescov))) { rescov <- list(rescov) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Print Coefficient Omega and/or Item Statistic ####

  if (isTRUE(all(c("all", "omega", "item") %in% print))) { print <- "omega" }

  if (isTRUE(length(print) == 1L && "all" %in% print)) { print <- c("omega", "item") }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  omega.mod <- .alpha.omega(y = x, alpha = FALSE, y.rescov = rescov, y.type = type, y.std = std, estimator = estimator, missing = missing, check = TRUE)

  omega.x <- data.frame(n = lavaan::lavInspect(omega.mod$mod.fit, "nobs"), items = ncol(lavaan::lavInspect(omega.mod$mod.fit, "data")), omega = omega.mod$coef.alpha.omega)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Confidence Interval ####

  df1 <- omega.x$n - 1L
  df2 <- (omega.x$items - 1) * df1

  omega.x <- data.frame(omega.x,
                        low = 1L - (1L - omega.x$omega) * qf(1L - (1L - conf.level) / 2L, df1, df2),
                        upp = 1L - (1L - omega.x$omega) * qf((1L - conf.level) / 2L, df1, df2))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Standardized Factor Loading and Omega if Item Deleted ####

  itemstat <- matrix(rep(NA, times = ncol(x)*2L), ncol = 2L, dimnames = list(NULL, c("std.ld", "omega")))

  # Standardized factor loadings
  itemstat[, "std.ld"] <- lavaan::inspect(omega.mod$mod.fit, what = "std")$lambda

  if (isTRUE("item" %in% print && ncol(x) > 3L)) {

    for (i in seq_len(ncol(x))) {

      var <- colnames(x)[i]

      # Residual covariance
      if (isTRUE(!is.null(rescov))) {

        rescov.i <- rescov[-which(unlist(lapply(rescov, function(y) any(y %in% var))))] |> (\(y) if (isTRUE(length(y) == 0L)) { NULL } else { y })()

      } else {

        rescov.i <- NULL

      }

      itemstat[i, 2L] <- .alpha.omega(y = x[, -grep(var, colnames(x))], alpha = FALSE, y.rescov = rescov.i, y.type = type, y.std = std, estimator = estimator, missing = missing, check = FALSE)$coef.alpha.omega

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Descriptive Statistics ####

  itemstat <- data.frame(var = colnames(x), misty::descript(x, output = FALSE)$result[, c("n", "nNA", "pNA", "m", "sd", "min", "max")], itemstat)

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "item.omega",
                 data = x,
                 args = list(rescov = rescov, type = type, exclude = exclude, std = std, estimator = estimator, missing = missing, print = print, digits = digits, conf.level = conf.level, as.na = as.na, write = write, append = append, check = check, output = output),
                 model.fit = omega.mod$mod.fit,
                 result = list(omega = omega.x, itemstat = itemstat))

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
