#' Coefficient Omega, Hierarchical Omega, and Categorical Omega
#'
#' This function computes point estimate and confidence interval for the coefficient
#' omega (McDonald, 1990), hierarchical coefficient omega (Kelley & Pornprasertmanit,
#' 2016), and categorical coefficient omega (Green & Yang, 2009) along with
#' standardized factor loadings and omega if item deleted. By default, the function
#' computes coefficient omega based on maximum likelihood parameter (ML) estimates
#' using full information maximum likelihood (FIML) method in the presence of
#' missing data.
#'
#' @param data       a data frame. Note that at least three items are needed for
#'                   computing coefficient omega
#' @param ...        an expression indicating the variable names in \code{data}
#'                   e.g., \code{item.omega(dat, x1, x2, x3)}. Note that the
#'                   operators \code{+}, \code{-}, \code{~}, \code{:},
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
#'                   By default, \code{"ML"} is used for computing (hierarchical)
#'                   coefficient omega and \code{"DWLS"} is used for computing
#'                   ordinal coefficient omega.
#' @param missing    a character string indicating how to deal with missing data.
#'                   (see 'Details' in the \code{\link{item.cfa}} function). By
#'                   default, full information maximum likelihood method (\code{missing = "fiml"})
#'                   is used for computing (hierarchical) coefficient omega and
#'                   pairwise deletion (\code{missing = "pairwise"}) is used to
#'                   compute coefficient omega.
#' @param print      a character vector indicating which results to show, i.e.
#'                   \code{"all"} for all results \code{"omega"} (default) for
#'                   the coefficient omega, and \code{"item"} for item statistics.
#' @param digits     an integer value indicating the number of decimal places to
#'                   be used for displaying mean, standard deviation, minimum,
#'                   and maximum.
#' @param r.digits   an integer value indicating the number of decimal places to
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
#' @details
#' \describe{
#  \item{\strong{Coefficient Omega}}{This function computes the coefficient omega
#' by conducting a confirmatory factor analysis based on the congeneric measurement
#' model (Graham, 2006) using the \code{cfa()} function in the \pkg{lavaan} package
#' by Yves Rosseel (2019).}
#' \item{\strong{Confidence Interval}}{The confidence interval for the (ordinal)
#'  coefficient alpha is computed using the procedure by Feldt et al. (1987).
#'  Note that there are at least 10 other procedures for computing the confidence
#'  interval (see Kelley and Pornprasertmanit, 2016), which are implemented in the
#'  \code{ci.reliability()} function in the \pkg{MBESSS} package by Ken Kelley (2019).}
#' }
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{item.omega}}, \code{\link{item.cfa}}, \code{\link{item.invar}},
#' \code{\link{item.reverse}}, \code{\link{item.scores}}, \code{\link{write.result}}
#'
#' @references
#' Feldt, L. S., Woodruff, D. J., & Salih, F. A. (1987). Statistical inference for
#' coefficient alpha. \emph{Applied Psychological Measurement}, 11 93-103.
#' https://doi.org/10.1177/014662168701100107
#'
#' Graham, J. M. (2006). Congeneric and (essentially) tau-equivalent estimates of
#' score reliability: What they are and how to use them. \emph{Educational and
#' Psychological Measurement, 66}(6), 930–944. https://doi.org/10.1177/0013164406288165
#'
#' Green, S. B., & Yang, Y. (2009). Reliability of summed item scores using structural
#' equation modeling: An alternative to coefficient alpha. \emph{Psychometrika, 74}, 155167.
#' https://doi.org/10.1007/s11336-008-9099-3
#'
#' Kelley, K., & Pornprasertmanit, S. (2016). Confidence intervals for population
#' reliability coefficients: Evaluation of methods, recommendations, and software
#' for composite measures. \emph{Psychological Methods, 21}, 69-92.
#' https://doi.org/10.1037/a0040086.
#'
#' Ken Kelley (2019). \emph{MBESS: The MBESS R Package}. R package version 4.6.0.
#' https://CRAN.R-project.org/package=MBESS
#'
#' McDonald, R. P. (1999). \emph{Test theory: A unified treatment}. Lawrence
#' Erlbaum Associates Publishers.
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
#' # Load data set "HolzingerSwineford1939" in the lavaan package
#' data("HolzingerSwineford1939", package = "lavaan")
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Continuous Data
#'
#' # Example 1a: Coefficient omega
#' item.omega(HolzingerSwineford1939, x1::x9)
#'
#' # Example 1b: Coefficient omega and item statistics after excluding 'x3'
#' item.omega(HolzingerSwineford1939, x1::x9, exclude = "x3", print = "all")
#'
#' # Example 2: Hierarchical Omega
#' item.omega(HolzingerSwineford1939, x1::x9, type = "hierarch")
#'
#' # Example 3a: Residual covariance between 'x1' and 'x2'
#' item.omega(HolzingerSwineford1939, x1::x9, rescov = c("x1", "x2"))
#'
#' # Example 3b: Residual covariances between 'x1' and 'x2', and 'x2' and 'x3'
#' item.omega(HolzingerSwineford1939, x1::x9, rescov = list(c("x1", "x2"), c("x2", "x3")))
#'
#' # Example 4: Summary of the CFA model used to compute coefficient omega
#' lavaan::summary(item.omega(HolzingerSwineford1939, x1::x9, output = FALSE)$model.fit,
#'                 standardized = TRUE)
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Polytomous Data
#'
#' # Example 5: Ordinal coefficient omega and item statistics
#' item.omega(data.items, pitem1, pitem2r, pitem3r, pitem4::pitem6, type = "categ",
#'            print = "all")
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Write Results
#'
#' # Example 6a: Write Results into a text file
#' item.omega(HolzingerSwineford1939, x1::x9, print = "all", write = "Omega.txt")
#'
#' # Example 6b: Write Results into an Excel file
#' item.omega(HolzingerSwineford1939, x1::x9, print = "all", write = "Omega.xlsx")
#' }
item.omega <- function(data, ..., rescov = NULL,
                       type = c("omega", "hierarch", "categ"), exclude = NULL,
                       std = FALSE, estimator = c("ML", "GLS", "WLS", "DWLS", "ULS", "PML"),
                       missing = c("listwise", "pairwise", "fiml"), print = c("all", "omega", "item"),
                       digits = 2, r.digits = 3, conf.level = 0.95, as.na = NULL,
                       write = NULL, append = TRUE, check = TRUE, output = TRUE) {

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

  x <- .exclude.non.numeric(x, func = "item.omega")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Exclude Items ####

  if (isTRUE(!is.null(exclude))) { x <- x[, which(!colnames(x) %in% exclude)] |> (\(y) if (isTRUE(ncol(y) < 3L)) { stop("At least three items after excluding items are needed to compute coefficient omega.", call. = FALSE) } else { return(y) })() }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert User-missing Values into NA ####

  if (isTRUE(!is.null(as.na))) { x <- .as.na(x, na = as.na) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("std", "append", "output"),
               s.character = list(type = c("omega", "hierarch", "categ"), estimator = c("ML", "GLS", "WLS", "DWLS", "ULS", "PML"), missing = c("listwise", "pairwise", "fiml")),
               m.character = list(print = c("all", "omega", "item")), args = c("digits", "r.digits", "conf.level"), package = "lavaan", envir = environment(), input.check = check)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Additional Checks ####

  if (isTRUE(check)) {

    # Package 'mnormt' installed?
    if (isTRUE(all(type == "categ"))) { if (isTRUE(!requireNamespace("mnormt", quietly = TRUE))) { stop("Package \"mnormt\" is needed for this function to work, please install it.", call. = FALSE) } }

    #—————————————————————————————————————— #
    ### 'data' Argument ####

    # At least three items
    if (isTRUE(ncol(x) < 3L)) { stop("Please specify at least three items to compute coefficient omega", call. = FALSE) }

    #—————————————————————————————————————— #
    ### 'resocv' Argument ####

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

    #—————————————————————————————————————— #
    ### 'estimator' and 'missing' Arguments ####

    if (isTRUE(all(estimator == "ULS") && all(missing == "pairwise"))) { stop("Pairwise deletion is not available when estimator = \"ULS\".", call. = FALSE) }

    if (isTRUE(all(estimator == "DWLS") && all(missing == "pairwise"))) { stop("Pairwise deletion is not available when estimator = \"DWLS\".", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Type of Omega ####

  if (isTRUE(all(c("omega", "hierarch", "categ") %in% type))) { type <- "omega" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Estimator ####

  #—————————————————————————————————————— #
  ### Coefficient Omega or Hierarchical Omega ####

  if (isTRUE(type %in% c("omega", "hierarch"))) {

    if (isTRUE(all(c("ML", "GLS", "WLS", "DWLS", "ULS", "PML") %in% estimator))) { estimator <- "ML" }

  #—————————————————————————————————————— #
  ### Categorical Coefficient Omega ####

  } else {

    if (isTRUE(all(c("ML", "GLS", "WLS", "DWLS", "ULS", "PML") %in% estimator))) {

      estimator <- "DWLS"

    } else {

      if (isTRUE(estimator == "ML")) { stop("Estimator \"ML\" is not available for estimating categorical omega.") }

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Missing Data ####

  #—————————————————————————————————————— #
  ### Missing Values ####

  if (isTRUE(any(is.na(x)))) {

    #···················
    #### Coefficient Omega or Hierarchical Omega ####

    if (isTRUE(type %in% c("omega", "hierarch"))) {

      if (isTRUE(all(c("listwise", "pairwise", "fiml") %in% missing))) { missing <- "fiml" }

    #···················
    #### Categorical Coefficient Omega ####

    } else {

      if (isTRUE(all(c("listwise", "pairwise", "fiml") %in% missing))) {

        missing <- "pairwise"

      } else if (isTRUE(missing == "fiml")) {

        missing <- "pairwise"

        warning("FIML method is not available for estimator = \"ML\", argument 'missing' switched to \"pairwise\".", call. = FALSE)

      }

    }

  #—————————————————————————————————————— #
  ### No Missing Values ####

  } else {

    missing <- "listwise"

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Residual Covariance ####

  if (isTRUE(!is.null(rescov) & !is.list(rescov))) { rescov <- list(rescov) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Print Coefficient Omega and/or Item Statistic ####

  if (isTRUE(all(c("all", "omega", "item") %in% print))) {

    print <- "omega"

  } else if (isTRUE(all(print == "all"))) {

    print <- c("omega", "item")

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  omega <- .omega(y = x, rescov = rescov, type = type, std = std, estimator = estimator, missing = missing, check = check)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Confidence Interval ####

  df1 <- lavaan::lavInspect(omega$mod.fit, "nobs") - 1L
  df2 <- (n.items = ncol(lavaan::lavInspect(omega$mod.fit, "data")) - 1L) * df1

  restab <- data.frame(n = lavaan::lavInspect(omega$mod.fit, what = "nobs"),
                       nNA = nrow(x) - lavaan::lavInspect(omega$mod.fit, what = "nobs"),
                       n.items = ncol(lavaan::lavInspect(omega$mod.fit, what = "data")),
                       omega = omega$omega,
                       low = 1L - (1L - omega$omega) * qf(1L - (1L - conf.level) / 2L, df1, df2),
                       upp = 1L - (1L - omega$omega) * qf((1L - conf.level) / 2L, df1, df2))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Descriptive Statistics, Std. Factor Loadings, and Omega if Item Deleted ####

  itemstat <- NULL
  if (isTRUE("item" %in% print)) {

    #—————————————————————————————————————— #
    ### Standardized Factor Loading and Omega if Item Deleted ####

    # Result table
    itemstat <- matrix(rep(NA, times = ncol(x)*3L), ncol = 3L, dimnames = list(NULL, c("std.ld", "omega", "d.omega")))

    # Standardized factor loadings
    itemstat[, "std.ld"] <- lavaan::inspect(omega$mod.fit, what = "std")$lambda

    if (isTRUE(ncol(x) > 3L)) {

      #···················
      #### No Residual Covariances ####

      if (isTRUE(is.null(rescov))) {

        itemstat[, "omega"] <- sapply(seq_len(ncol(x)), function(y) suppressWarnings(.omega(x[, -y, drop = FALSE], rescov = NULL, type = type, std = std, estimator = estimator, missing = missing, check = check)$omega))

      #···················
      #### Residual Covariances ####

      } else {

        for (i in seq_len(ncol(x))) {

          var <- colnames(x)[i]

          # Residual covariance
          if (isTRUE(!is.null(rescov))) {

            rescov.i <- rescov[-which(unlist(lapply(rescov, function(y) any(y %in% var))))] |> (\(y) if (isTRUE(length(y) == 0L)) { NULL } else { y })()

          } else {

            rescov.i <- NULL

          }

          itemstat[i, "omega"] <- .omega(x[, -grep(var, colnames(x))], rescov = rescov.i, type = type, std = std, estimator = estimator, missing = missing, check = FALSE)$omega

        }

      }

      # Difference in coefficient omega
      itemstat[, "d.omega"] <- itemstat[, "omega"] - restab$omega

    }

    #—————————————————————————————————————— #
    ### Descriptive Statistics ####

    itemstat <- data.frame(item = colnames(x), misty::descript(x, output = FALSE)$result[, c("n", "nNA", "pNA", "m", "sd", "min", "max")], itemstat)

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "item.omega",
                 data = x,
                 args = list(rescov = rescov, type = type, exclude = exclude, estimator = estimator, missing = missing, print = print, digits = digits, r.digits = r.digits, conf.level = conf.level, as.na = as.na, write = write, append = append, check = check, output = output),
                 model.fit = omega$mod.fit,
                 result = list(omega = restab, itemstat = itemstat))

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
