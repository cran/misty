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
#' @param check  logical: if \code{TRUE}, argument specification is checked.
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
#' \tabular{ll}{
#' \code{idout} \tab ID variable \cr
#' \code{mahal} \tab Mahalanobis distance \cr
#' \code{hat} \tab hat values \cr
#' \code{rstand} \tab standardized leverage-corrected residuals \cr
#' \code{rstud} \tab studentized leverage-corrected residuals \cr
#' \code{cook} \tab Cookꞌs distance \cr
#' \code{Intercept.dfb} \tab DFBetas for the intercept \cr
#' \code{pred1.dfb} \tab DFBetas for the slope of the predictor 'pred1' \cr
#' \code{....dfb} \tab DFBetas for the slope of the predictor '...' \cr
#' }
#'
#' @export
#'
#' @examples
#' dat <- data.frame(x1 = c(3, 2, 4, 9, 5, 3, 6, 4, 5, 6, 3, 5),
#'                   x2 = c(1, 4, 3, 1, 2, 4, 3, 5, 1, 7, 8, 7),
#'                   x3 = c(0, NA, 1, 0, 1, 1, NA, 1, 0, 0, 1, 1),
#'                   y = c(2, 7, 4, 4, 7, 8, 4, 2, 5, 1, 3, 8))
#'
#' # Regression model and measures for leverage, distance, and influence
#' mod.lm <- lm(y ~ x1 + x2 + x3, data = dat)
#' check.outlier(mod.lm)
#'
#' # Merge result table with the data
#' dat1 <- cbind(dat, check.outlier(mod.lm))
check.outlier <- function(model, check = TRUE, ...) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'model' is missing
  if (isTRUE(missing(model))) { stop("Input for the argument 'model' is missing.", call. = FALSE) }

  # Check if input 'model' is NULL
  if (isTRUE(is.null(model))) { stop("Input specified for the argument 'model' is NULL.", call. = FALSE) }

  # Check if input 'model' is not 'lm'
  if (isTRUE(!all(class(model) %in% "lm"))) { stop("Please specify an \"lm\" object for the argument 'model'.", call. = FALSE) }

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data and Variables ---------------------------------------------------------

  # Data
  mod.dat <- model$model

  # Intercept and predictors
  mod.int.pred <- names(model$coefficients)

  # Predictors
  mod.pred <- misty::chr.omit(mod.int.pred, "(Intercept)")

  idout <- NULL

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  if (isTRUE(check)) {

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Leverage ####

  # Mahalanobis distance
  mod.mahal <- mahalanobis(mod.dat[, mod.pred],
                           center = colMeans(mod.dat[, mod.pred], na.rm = TRUE),
                           cov = cov(mod.dat[, mod.pred], use = "pairwise.complete.obs"))

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
  mod.dfbeta <- dfbetas(model)
  colnames(mod.dfbeta) <- paste0(misty::chr.gsub(c("\\(", "\\)"), c("", ""), mod.int.pred), ".dfb")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Result table ####

  result <- merge(data.frame(idout = as.numeric(row.names(mod.dat)), mahal = mod.mahal),
                  data.frame(idout = as.numeric(row.names(mod.dfbeta)),
                             hat = mod.hat, rstand = mod.rstand, rstud = mod.rstud, cook = mod.cook, mod.dfbeta),
                  by = "idout", all.x = TRUE)

  # Missing values
  if (isTRUE(length(model[[9L]]) != 0L)) {

    result <- misty::df.sort(rbind(result,
                                   data.frame(idout = unclass(model[[9L]]),
                                              matrix(NA, ncol = ncol(result) - 1L, nrow = length(model[[9L]]),
                                                     dimnames = list(NULL, colnames(result)[-1L])))), idout)

  }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  return(result)

}
