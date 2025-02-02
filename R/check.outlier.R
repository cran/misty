#' Statistical Measures for Leverage, Distance, and Influence
#'
#' This function computes statistical measures for leverage, distance, and
#' influence for linear models estimated by using the \code{lm()} function.
#' \emph{Mahalanobis distance} and \emph{hat values} are computed for quantifying
#' \strong{leverage}, \emph{standardized leverage-corrected residuals} and
#' \emph{studentized leverage-corrected residuals} are computed for quantifying
#' \strong{distance}, and \emph{Cookꞌs distance} and \emph{DfBetas} are computed
#' for quantifying \strong{influence}.
#'
#' @param model  a fitted model of class \code{"lm"}.
#' @param append logical: if \code{TRUE} (default), statistical measures for
#'               leverage, distance, and influence are appended to the data frame
#'               in \code{model$model}.
#' @param check  logical: if \code{TRUE} (default), argument specification is checked.
#' @param ...    further arguments to be passed to or from methods.
#'
#' @details
#' In regression analysis, an observation can be extreme in three major ways (see
#' Darlington & Hayes, p. 484): (1) An observation has high \strong{leverage} if
#' it has a atypical pattern of values on the predictors, (2) an observation has
#' high \strong{distance} if its observed outcome value \eqn{Y_i} has a large
#' deviation from the predicted value \eqn{\hat{Y}_i}, and (3) an observation has
#' high \strong{influence} if its inclusion substantially changes the estimates
#' for the intercept and/or slopes.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{check.collin}}, \code{\link{lm}}
#'
#' @references
#' Darlington, R. B., &, Hayes, A. F. (2017). \emph{Regression analysis and linear
#' models}: Concepts, applications, and implementation. The Guilford Press.
#'
#' @return
#' Returns a data frame with following entries:
#'
#' \item{\code{idout}}{ID variabl}
#' \item{\code{mahal}}{Mahalanobis distance}
#' \item{\code{hat}}{hat values}
#' \item{\code{rstand}}{standardized leverage-corrected residuals}
#' \item{\code{rstud}}{studentized leverage-corrected residuals}
#' \item{\code{cook}}{Cookꞌs distance}
#' \item{\code{Intercept.dfb}}{DFBetas for the intercept}
#' \item{\code{pred1.dfb}}{DFBetas for the slope of the predictor 'pred1'}
#' \item{\code{....dfb}}{DFBetas for the slope of the predictor '...'}
#'
#' @export
#'
#' @examples
#' # Example 1: Statistical measures for leverage, distance, and influence
#' check.outlier(lm(mpg ~ cyl + disp + hp, data = mtcars))
#'
#' # Example 2: Append statistical measures to the mtcars data frame
#' cbind(mtcars,
#'       check.outlier(lm(mpg ~ cyl + disp + hp, data = mtcars), append = FALSE))
check.outlier <- function(model, append = TRUE, check = TRUE, ...) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'model' is missing
  if (isTRUE(missing(model))) { stop("Input for the argument 'model' is missing.", call. = FALSE) }

  # Check if input 'model' is NULL
  if (isTRUE(is.null(model))) { stop("Input specified for the argument 'model' is NULL.", call. = FALSE) }

  # Check if input 'model' is not 'lm'
  if (isTRUE(!inherits(model, "lm"))) { stop("Please specify an \"lm\" object for the argument 'model'.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'append'
  .check.input(logical = "append", envir = environment(), input.check = check)

  #_____________________________________________________________________________
  #
  # Data and Variables ---------------------------------------------------------

  # Data
  mod.dat <- model$model

  # Intercept and predictors
  mod.int.pred <- names(model$coefficients)

  # Predictors
  mod.pred <- misty::chr.omit(mod.int.pred, "(Intercept)")

  #idout <- NULL

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Leverage ####

  # Mahalanobis distance
  mod.mahal <- mahalanobis(mod.dat[, mod.pred], center = colMeans(mod.dat[, mod.pred], na.rm = TRUE), cov = cov(mod.dat[, mod.pred], use = "pairwise.complete.obs"))

  # Hat values
  mod.hat <- hatvalues(model)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Distance ####

  # Standardized leverage-corrected residual
  mod.rstand <- rstandard(model)

  # Studentized leverage-corrected residual
  mod.rstud <- rstudent(model)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Influence ####

  # Cook's distance
  mod.cook <- cooks.distance(model)

  # DfBetas
  mod.dfbeta <- setNames(as.data.frame(dfbetas(model)), nm = paste0(misty::chr.gsub(c("\\(", "\\)"), c("", ""), mod.int.pred), ".dfb"))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Result table ####

  result <- merge(data.frame(idout = if (isTRUE(all(!is.na(suppressWarnings(as.numeric(row.names(mod.dat))))))) { as.numeric(row.names(mod.dat)) } else { row.names(mod.dat) }, mahal = mod.mahal),
                  data.frame(idout = if (isTRUE(all(!is.na(suppressWarnings(as.numeric(row.names(mod.dfbeta))))))) { as.numeric(row.names(mod.dfbeta)) } else { row.names(mod.dfbeta) }, hat = mod.hat, rstand = mod.rstand, rstud = mod.rstud, cook = mod.cook, mod.dfbeta),
                  by = "idout", sort = FALSE, all.x = TRUE)

  # Missing values
  if (isTRUE(length(model[[9L]]) != 0L)) {

    na <- model[[9L]]
    insert <- sapply(colnames(result), function(z) z = NA)

    idout <- result$idout
    inerst.row <- names(na)

    for (i in seq_along(na)) {

      na.i <- seq(from = na[i], to = nrow(result))
      result[na.i + 1L, ] <- result[na.i, ]
      result[na[i], ] <- insert

      idout[na.i + 1] <- idout[na.i]
      idout[na[i]] <- inerst.row[i]

    }

    result$idout <- idout

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Append ####

  if (isTRUE(append)) {

    # Missing values
    if (isTRUE(length(model[[9L]]) != 0L)) {

      insert <- sapply(colnames(model$model), function(z) z = NA)

      for (i in seq_along(na)) {

        na.i <- seq(from = na[i], to = nrow(mod.dat))
        mod.dat[na.i + 1L, ] <- mod.dat[na.i, ]
        mod.dat[na[i], ] <- insert

      }

    }

    object <- data.frame(idout = result$idout, mod.dat, result[, -grep("idout", colnames(result))], row.names = NULL)

  } else {

    object <- result

  }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  return(object)

}

#_______________________________________________________________________________
