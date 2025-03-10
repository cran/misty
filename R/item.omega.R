#' Coefficient Omega, Hierarchical Omega, and Categorical Omega
#'
#' This function computes point estimate and confidence interval for the coefficient
#' omega (McDonald, 1978), hierarchical omega (Kelley & Pornprasertmanit, 2016),
#' and categorical omega (Green & Yang, 2009) along with standardized factor loadings
#' and omega if item deleted.
#'
#' Omega is computed by estimating a confirmatory factor analysis model using the
#' \code{cfa()} function in the \pkg{lavaan} package by Yves Rosseel (2019).
#' Maximum likelihood (\code{"ML"}) estimator is used for computing coefficient
#' omega and hierarchical omega, while diagonally weighted least squares estimator
#' (\code{"DWLS"}) is used for computing categorical omega.
#'
#' Approximate confidence intervals are computed using the procedure by Feldt,
#' Woodruff and Salih (1987). Note that there are at least 10 other procedures
#' for computing the confidence interval (see Kelley and Pornprasertmanit, 2016),
#' which are implemented in the \code{ci.reliability()} function in the
#' \pkg{MBESSS} package by Ken Kelley (2019).
#'
#' @param data       a data frame. Note that at least three items are needed for
#'                   computing coefficient omega.
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
#'                   i.e., \code{omega} (default) for coefficient omega,
#'                   \code{hierarch} for hierarchical omega, and \code{categ} for
#'                   categorical omega.
#' @param exclude    a character vector indicating items to be excluded from the
#'                   analysis.
#' @param std        logical: if \code{TRUE}, the standardized coefficient omega
#'                   is computed.
#' @param na.omit    logical: if \code{TRUE}, incomplete cases are removed before
#'                   conducting the analysis (i.e., listwise deletion); if
#'                   \code{FALSE}, full information maximum likelihood (FIML) is
#'                   used for computing coefficient omega or hierarchical omega,
#'                   while pairwise deletion is used for computing categorical
#'                   omega.
#' @param print      a character vector indicating which results to show, i.e.
#'                   \code{"all"} (default), for all results \code{"omega"} for
#'                   omega, and \code{"item"} for item statistics.
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
#' \code{\link{item.alpha}}, \code{\link{item.cfa}}, \code{\link{item.invar}},
#' \code{\link{item.reverse}}, \code{\link{item.scores}}, \code{\link{write.result}}
#'
#' @references
#' Feldt, L. S., Woodruff, D. J., & Salih, F. A. (1987). Statistical inference for
#' coefficient alpha. \emph{Applied Psychological Measurement}, 11 93-103.
#'
#' Green, S. B., & Yang, Y. (2009). Reliability of summed item scores using structural
#' equation modeling: An alternative to coefficient alpha. \emph{Psychometrika, 74},
#' 155-167. https://doi.org/10.1007/s11336-008-9099-3
#'
#' Kelley, K., & Pornprasertmanit, S. (2016). Confidence intervals for population
#' reliability coefficients: Evaluation of methods, recommendations, and software
#' for composite measures. \emph{Psychological Methods, 21}, 69-92.
#' http://dx.doi.org/10.1037/a0040086
#'
#' Ken Kelley (2019). \emph{MBESS: The MBESS R Package}. R package version 4.6.0.
#' https://CRAN.R-project.org/package=MBESS
#'
#' McDonald, R. P. (1978). Generalizability in factorable domains: Domain validity
#' and generalizability. \emph{Educational and Psychological Measurement, 38}, 75-79.
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{data}}{data frame used for the current analysis}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{model.fit}}{fitted lavaan object (\code{mod.fit})}
#' \item{\code{result}}{list with result tables, i.e., \code{ometa} for a table
#'                      with coefficient omega and \code{itemstat} for a table with
#'                      item statistics}
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
#' dat <- data.frame(item1 = c(5, 2, 3, 4, 1, 2, 4, 2), item2 = c(5, 3, 3, 5, 2, 2, 5, 1),
#'                   item3 = c(4, 2, 4, 5, 1, 3, 5, 1), item4 = c(5, 1, 2, 5, 2, 3, 4, 2))
#'
#' # Example 1a: Compute unstandardized coefficient omega and item statistics
#' item.omega(dat)
#'
#' # Example 2: Compute unstandardized coefficient omega and item statistics while excluding item3
#' item.omega(dat, exclude = "item3")
#'
#' # Example 3: Compute unstandardized coefficient omega with a residual covariance
#' # and item statistics
#' item.omega(dat, rescov = c("item1", "item2"))
#'
#' # Example 4: Compute unstandardized coefficient omega with residual covariances
#' # and item statistics
#' item.omega(dat, rescov = list(c("item1", "item2"), c("item1", "item3")))
#'
#' # Example 5: Compute unstandardized hierarchical omega and item statistics
#' item.omega(dat, type = "hierarch")
#'
#' # Example 6: Compute categorical omega and item statistics
#' item.omega(dat, type = "categ")
#'
#' # Example 7: Compute standardized coefficient omega and item statistics
#' item.omega(dat, std = TRUE)
#'
#' # Example 8: Compute unstandardized coefficient omega
#' item.omega(dat, print = "omega")
#'
#' # Example 9: Print only item statistics
#' item.omega(dat, print = "item")
#'
#' # Example 10: Summary of the CFA model used to compute coefficient omega
#' lavaan::summary(item.omega(dat, output = FALSE)$model.fit,
#'                 fit.measures = TRUE, standardized = TRUE)
#'
#' # Example 11a: Write Results into a text file
#' item.omega(dat, write = "Omega.txt")
#'
#' # Example 11b: Write Results into a Excel file
#' item.omega(dat, write = "Omega.xlsx")
#' }
item.omega <- function(data, ..., rescov = NULL,
                       type = c("omega", "hierarch", "categ"), exclude = NULL,
                        std = FALSE, na.omit = FALSE, print = c("all", "omega", "item"),
                        digits = 2, conf.level = 0.95, as.na = NULL, write = NULL,
                        append = TRUE, check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'data' is missing
  if (isTRUE(missing(data))) { stop("Please specify a data frame for the argument 'data'", call. = FALSE) }

  # Check if input 'data' is NULL
  if (isTRUE(is.null(data))) { stop("Input specified for the argument 'data' is NULL.", call. = FALSE) }

  # Package 'mnormt' installed?
  if (isTRUE(ordered)) { if (isTRUE(!requireNamespace("mnormt", quietly = TRUE))) { stop("Package \"mnormt\" is needed for this function to work, please install it.", call. = FALSE) } }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data using the argument 'data' ####

  if (isTRUE(!missing(...))) {

    # Extract data and convert tibble into data frame or vector
    x <- as.data.frame(data[, .var.names(..., data = data), drop = FALSE])

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data without using the argument 'data' ####

  } else {

    # Data frame
    x <- as.data.frame(data)

  }

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
  ## Exclude items ####

  if (isTRUE(!is.null(exclude))) {

    x <- x[, which(!colnames(x) %in% exclude)]

    # One or two items left
    if (isTRUE(ncol(x) <= 2L)) { stop("At least three items after excluding items are needed to compute coefficient omega.", call. = FALSE) }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) { x <- .as.na(x, na = as.na) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Listwise deletion ####

  if (isTRUE(any(is.na(x)) && na.omit)) { assign("x", na.omit(x)) |> (\(y) warning(paste("Listwise deletion of incomplete data, number of cases removed from the analysis:", length(attributes(y)$na.action)), call. = FALSE))() }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("std", "na.omit", "append", "output"),
               s.character = list(type = c("omega", "hierarch", "categ")),
               m.character = list(print = c("all", "omega", "item")),
               args = c("digits", "conf.level"),
               package = "lavaan", envir = environment(), input.check = check)

  # Additional checks
  if (isTRUE(check)) {

    # Check input 'data': One or two item
    if (isTRUE(ncol(x) < 3L)) { stop("Please specify at least three items to compute coefficient omega", call. = FALSE) }

    # Check input 'data': Zero variance
    if (isTRUE(nrow(x) != ncol(x))) {

      vapply(as.data.frame(x, stringsAsFactors = FALSE), function(y) length(na.omit(unique(y))) == 1L, FUN.VALUE = logical(1L)) |>
        (\(y) if (isTRUE(any(y))) { stop(paste0("Following variables in the data frame specified in 'data' have zero variance: ", paste(names(which(y)), collapse = ", ")), call. = FALSE) })()


    }

    # Check input 'rescov'
    if (isTRUE(!is.null(rescov))) {

      unique(unlist(rescov)) |>
        (\(y) if (isTRUE(any(!y %in% colnames(x)))) {

          stop(paste0("Items specified in the argument 'rescov' were not found in 'data': ", paste(y[!y %in% colnames(x)], collapse = ", ")), call. = FALSE)

        })()

    }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Type of omega ####

  if (isTRUE(all(c("omega", "hierarch", "categ") %in% type))) { type <- "omega" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Residual covariance ####

  if (isTRUE(!is.null(rescov) & !is.list(rescov))) { rescov <- list(rescov) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Standardize ####

  # Unstandardized data
  x.raw <- x

  if (isTRUE(std && type != "categ")) { x <- as.data.frame(scale(x), stringsAsFactors = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Print coefficient omega and/or item statistic ####

  if (isTRUE(all(c(c("all", "omega", "item")) %in% print))) { print <- c("omega", "item") }

  if (isTRUE(length(print) == 1L && "all" %in% print)) { print <- c("omega", "item") }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  omega.mod <- .omega.function(y = x, y.rescov = rescov, y.type = type, y.std = std, check = TRUE)

  omega.x <- data.frame(n = lavaan::lavInspect(omega.mod$mod.fit, "nobs"),
                        items = ncol(lavaan::lavInspect(omega.mod$mod.fit, "data")),
                        omega = omega.mod$omega, stringsAsFactors = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Confidence interval ####

  df1 <- omega.x$n - 1L
  df2 <- (omega.x$items - 1) * df1

  omega.low <- 1L - (1L - omega.x$omega) * qf(1L - (1L - conf.level) / 2L, df1, df2)
  omega.upp <- 1L - (1L - omega.x$omega) * qf((1L - conf.level) / 2L, df1, df2)

  omega.x <- data.frame(omega.x, low = omega.low, upp = omega.upp, stringsAsFactors = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Standardized factor loading and omega if item deleted ####

  itemstat <- matrix(rep(NA, times = ncol(x)*2L), ncol = 2L, dimnames = list(NULL, c("std.ld", "omega")))

  # Standardized factor loadings
  lambda.std <- lavaan::inspect(omega.mod$mod.fit, what = "std")$lambda

  for (i in seq_len(ncol(x))) {

    var <- colnames(x)[i]

    #...................
    ### Omega for continuous item ####

    if (isTRUE(type != "categ")) {

      # Standardized factor loading
      itemstat[i, 1L] <- lambda.std[i]

      # Omega if item deleted
      if (isTRUE(ncol(x) > 3L)) {

        # Residual covariance
        if (isTRUE(!is.null(rescov))) {

          rescov.i <- rescov[-which(vapply(rescov, function(y) any(y %in% var), FUN.VALUE = logical(1L)))]

          if (isTRUE(length(rescov.i) == 0L)) { rescov.i <- NULL }

        } else {

          rescov.i <- NULL

        }

        itemstat[i, 2L] <- .omega.function(y = x[, -grep(var, colnames(x))], y.rescov = rescov.i, y.type = type, y.std = std, check = FALSE)$omega

      } else {

        itemstat[i, 2L] <- NA

      }

    #...................
    ### Omega for ordered-categorical items ####

    } else {

      # Standardized factor loading
      itemstat[i, 1L] <- lambda.std[i]

      # Omega if item deleted
      if (isTRUE(ncol(x) > 3L)) {

        itemstat[i, 2L] <- .omega.function(y = x[, -grep(var, colnames(x))], y.rescov = NULL, y.type = "categ", y.std = std, check = FALSE)$omega

      } else {

        itemstat[i, 2L] <- NA

      }

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Descriptive statistics ####

  itemstat <- data.frame(var = colnames(x),
                         misty::descript(x.raw, output = FALSE)$result[, c("n", "nNA", "pNA", "m", "sd", "min", "max")],
                         itemstat,
                         stringsAsFactors = FALSE)

  #_____________________________________________________________________________
  #
  # Return object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "item.omega",
                 data = x.raw,
                 args = list(rescov = rescov, type = type, exclude = exclude, std = std, na.omit = na.omit, print = print, digits = digits, conf.level = conf.level, as.na = as.na, write = write, append = append, check = check, output = output),
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

  return(invisible(object))

}

#_______________________________________________________________________________
