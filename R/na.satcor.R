#' Fit a Saturated Correlates Model
#'
#' This function estimates a confirmatory factor analysis model (\code{cfa.satcor}
#' function), structural equation model (\code{sem.satcor} function), growth curve
#' model (\code{growth.satcor} function), or latent variable model (\code{lavaan.satcor}
#' function) in the R package \pkg{lavaan} using full information maximum likelihood
#' (FIML) method to handle missing data while automatically specifying a saturated
#' correlates model to incorporate auxiliary variables into a substantive model
#' without affecting the parameter estimates, the standard errors, or the estimates
#' of quality of fit (Graham, 2003).
#'
#' @param model a character string indicating the lavaan model syntax without the
#'              auxiliary variables specified in \code{aux}.
#' @param data  a data frame containing the observed variables used in the lavaan
#'              model syntax specified in \code{model} and the auxiliary variables
#'              specified in \code{aux}.
#' @param aux   a character vector indicating the names of the auxiliary variables
#'              in the data frame specified in \code{data} that will be added to
#'              the lavaan model syntax specified in \code{model}. Note that
#'              this function can only incorporate continuous auxiliary variables,
#'              i.e., the function cannot deal with categorical auxiliary variables.
#' @param fun   a character string indicating the name of a specific lavaan function
#'              used to fit \code{model}, i.e., \code{cfa}, \code{sem}, \code{growth},
#'              or \code{lavaan}. Note that this argument is only required for
#'              the function \code{na.satcor}.
#' @param check logical: if \code{TRUE} (default), argument specification is
#'              checked.
#' @param ...   additional arguments passed to the lavaan function.
#'
#' @author
#' Takuya Yanagida
#'
#' @references
#' Graham, J. W. (2003). Adding missing-data-relevant variables to FIML-based
#' structural equation models. \emph{Structural Equation Modeling, 10}(1), 80-100.
#' https://doi.org/10.1207/S15328007SEM1001_4
#'
#' Jorgensen, T. D., Pornprasertmanit, S., Schoemann, A. M., & Rosseel, Y. (2022).
#' \emph{semTools: Useful tools for structural equation modeling}. R package version
#' 0.5-6. Retrieved from https://CRAN.R-project.org/package=semTools
#'
#' @return
#' An object of class lavaan, for which several methods are available in the
#' R package \pkg{lavaan}, including  a summary method.
#'
#' @note
#' This function is a modified copy of the \code{auxiliary()}, \code{cfa.auxiliary()},
#' \code{sem.auxiliary()}, \code{growth.auxiliary()}, and \code{lavaan.auxiliary()}
#' functions in the \pkg{semTools} package by Terrence D. Jorgensen et al.
#' (2022).
#'
#' @export
#'
#' @examples
#' # Load lavaan package
#' library(lavaan)
#'
#' #----------------------------------------------------------------------------
#' # Example 1: Saturated correlates model for the sem function
#'
#' # Model specification
#' model <- 'Ozone ~ Wind'
#'
#' # Model estimation using the sem.satcor function
#' mod.fit <- sem.satcor(model, data = airquality, aux = c("Temp", "Month"))
#'
#' # Model estimation using the na.satcor function
#' mod.fit <- na.satcor(model, data = airquality, fun = "sem", aux = c("Temp", "Month"),
#'                      estimator = "MLR")
#'
#' # Result summary
#' summary(mod.fit)
na.satcor <- function(model, data, aux, fun = c("cfa", "sem", "growth", "lavaan"),
                      check = TRUE, ...) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'model' is a character string
  if (isTRUE(missing(model) || is.null(model) || !is.character(model) || !length(model) == 1L)) { stop("Please specify a character string for the argument 'model'.", call. = FALSE) }

  # Check if input 'data' is a data frame
  if (isTRUE(missing(data) || is.null(data) || !is.data.frame(data))) { stop("Please specify a data frame for the argument 'data'.", call. = FALSE) }

  # Check if input 'aux' is a character vector
  if (isTRUE(missing(aux)) || is.null(aux) || !is.character(aux)) { stop("Please specify a character vector for the argument 'aux'", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(s.character = list(fun = c("cfa", "sem", "growth", "lavaan")), package = "lavaan", envir = environment(), input.check = check)

  if (isTRUE(check)) {

    # Check input 'model'
    lavaan::lavNames(model, type = "ov") |>
      (\(y) if (isTRUE(any(!y %in% names(data)))) { stop("Data frame does not contain all variables specified in the argument 'model': ", paste(y[!y %in% names(data)], collapse = ", ")) })()

    # Check input 'aux'
    which(!aux %in% names(data)) |>
      (\(y) if (isTRUE(length(y) != 0L)) { stop("Data frame does not contain all auxiliary variables specified in 'aux': ", paste(aux[y], collapse = ", ")) })()

    which(!sapply(data[aux], is.numeric)) |>
      (\(y) if (isTRUE(length(y) != 0L)) { stop("Auxiliary variables specified in 'aux' are not all numeric: ", paste(aux[y], collapse = ", ")) })()

    which(aux %in% lavaan::lavNames(model, type = "ov")) |>
      (\(y) if (isTRUE(length(y) != 0L)) { stop("Variables specified in the model syntax 'model' must not be declared as auxiliary variables: ", paste(aux[y], collapse = ", ")) })()

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## fun Argument ####

  if (isTRUE(all(c("cfa", "sem", "growth", "lavaan") %in% fun))) { fun <- "sem" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## lavaan Environment ####

  envir <- getNamespace("lavaan")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## lavaan Arguments ####

  lavArgs <- list(...)
  lavArgs$data <- substitute(data)
  lavArgs$fixed.x <- FALSE
  lavArgs$missing <- "fiml"
  lavArgs$meanstructure <- TRUE
  lavArgs$ordered <- NULL

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Parameter table ####

  ptArgs <- lavArgs
  ptArgs$model <- model
  ptArgs$do.fit <- FALSE

  PT <- lavaan::parTable(do.call(fun, ptArgs, envir = envir))[c("lhs", "op", "rhs", "user", "block", "group", "free", "label", "plabel", "start")]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Constraints and User-Defined Parameters ####

  conRows <- PT$op %in% c("==", "<", ">", ":=")
  if (isTRUE(any(conRows))) {

    CON <- PT[conRows, ]
    PT <-  PT[!conRows, ]

  } else {

    CON <- data.frame(NULL)

  }

  #_____________________________________________________________________________
  #
  # Model specification --------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Specify Saturated Correlates Model ####

  satPT <- outer(aux, aux, function(x, y) paste(x, "~~", y)) |>
    (\(y) lavaan::lavaanify(c(y[lower.tri(y, diag = TRUE)], paste(aux, "~ 1"),
                              outer(aux, lavaan::lavNames(PT, type = "ov"), function(x, y) paste(x, "~~", y))),
                            ngroups = max(PT$group))[c("lhs", "op", "rhs", "user", "block", "group")])()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Check Number of Added Parameters and Add Columns ####

  mergedPT <- lavaan::lav_partable_merge(PT, satPT, remove.duplicated = TRUE, warn = FALSE)
  nAuxPar <- nrow(mergedPT) - nrow(PT)
  newRows <- seq_len(nAuxPar) + nrow(PT)
  mergedPT$free[newRows] <- seq_len(nAuxPar) + max(PT$free)
  mergedPT$plabel[newRows] <- paste0(".p", seq_len(nAuxPar) + nrow(PT), ".")

  # Merge parameter table
  lavArgs$model <- lavaan::lav_partable_complete(rbind(mergedPT, CON))

  #_____________________________________________________________________________
  #
  # Model Estimation -----------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Main Model ####

  model.fit <- do.call(fun, lavArgs, envir = envir)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Baseline Model ####

  baseArgs <- list()
  baseArgs$model              <- lavaan::lav_partable_complete(satPT)
  baseArgs$data               <- data
  baseArgs$group              <- lavArgs$group
  baseArgs$group.label        <- lavArgs$group.label
  baseArgs$missing            <- "fiml"
  baseArgs$cluster            <- lavArgs$cluster
  baseArgs$sample.cov.rescale <- lavArgs$sample.cov.rescale
  baseArgs$estimator          <- lavArgs$estimator
  baseArgs$information        <- lavArgs$information
  baseArgs$se                 <- lavArgs$se
  baseArgs$test               <- lavArgs$test
  baseArgs$bootstrap          <- lavArgs$bootstrap
  baseArgs$control            <- lavArgs$control
  baseArgs$optim.method       <- lavArgs$optim.method

  baseArgs$fixed.x <- FALSE
  baseArgs$missing <- "fiml"

  # Baseline model estimation
  model.fit@external$baseline.model <- suppressWarnings(do.call("lavaan", baseArgs, envir = envir))

  #_____________________________________________________________________________
  #
  # Return object --------------------------------------------------------------

  return(model.fit)

}

#_______________________________________________________________________________
#_______________________________________________________________________________

#' @rdname na.satcor
cfa.satcor <- function(model, data, aux, check = TRUE, ...) {

  mc <- match.call(expand.dots = TRUE)
  mc$fun <- "cfa"
  mc$check <- check
  mc[[1L]] <- quote(misty::na.satcor)

  eval(mc, parent.frame())

}

#' @rdname na.satcor
sem.satcor <- function(model, data, aux, check = TRUE, ...) {

  mc <- match.call(expand.dots = TRUE)
  mc$fun <- "sem"
  mc$check <- check
  mc[[1L]] <- quote(misty::na.satcor)

  eval(mc, parent.frame())

}

#' @rdname na.satcor
growth.satcor <- function(model, data, aux, check = TRUE, ...) {

  mc <- match.call(expand.dots = TRUE)
  mc$fun <- "growth"
  mc$check <- check
  mc[[1L]] <- quote(misty::na.satcor)

  eval(mc, parent.frame())

}

#' @rdname na.satcor
lavaan.satcor <- function(model, data, aux, check = TRUE, ...) {

  mc <- match.call(expand.dots = TRUE)
  mc$fun <- "lavaan"
  mc$check <- check
  mc[[1L]] <- quote(misty::na.satcor)

  eval(mc, parent.frame())

}


#_______________________________________________________________________________
