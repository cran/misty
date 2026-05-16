#' Generates Simulated Data from a lavaan Model Syntax
#'
#' This function simulates data from a lavaan model syntax with unstandardized
#' or standardized parameters. By default, the function simulates observed variables
#' based on the model specified in the argument \code{model} in a unstandardized
#' metric.
#'
#' @param model      a character string indicating the lavaan model syntax.
#' @param n          a numeric value indicating the number of observations. By
#'                   default, 500 simulated cases are simulated.
#' @param std        logical: if \code{TRUE}, lavaan model syntax specified in
#'                   the argument \code{model} is based on standardized parameters.
#' @param skew       a numeric vector indicating the skewness values for the
#'                   observed variables. Note that this argument is only used when
#'                   \code{std = FALSE}.
#' @param kurt       a numeric vector indicating the kurtosis values for the
#'                   observed variables. Note that this argument is only used
#'                   when \code{std = FALSE}.
#'                   Note that this argument is only used when \code{std = FALSE}.
#' @param observed   logical: if \code{TRUE} (default), observed variables are
#'                   included. Note that this argument is only used when
#'                   \code{std = TRUE}.
#' @param latent     logical: if \code{TRUE}, latent variables are included.
#'                   Note that this argument is only used when \code{std = TRUE}.
#' @param fscores    logical: if \code{TRUE}, factor score are included.
#'                   Note that this argument is only used when \code{std = TRUE}.
#' @param composites logical: if \code{TRUE}, composite  variables are included.
#'                   Note that this argument is only used when \code{std = TRUE}.
#' @param errors     logical: if \code{TRUE}, observed error and latent disturbance
#'                   variables are included. Note that this argument is only used
#'                   when \code{std = TRUE}.
#' @param matrices   logical: if \code{TRUE}, matrices are included as attributes.
#'                   Note that this argument is only used when \code{std = TRUE}.
#' @param method     a character string indicating the matrix decomposition used
#'                   to determine the matrix root of \code{sigma} in the random
#'                   number generator for the multivariate normal distribution,
#'                   i.e., \code{"eigen"} (default) for eigenvalue decomposition,
#'                   \code{"svd"} for singular value decomposition, and \code{"chol"}
#'                   for Cholesky decomposition. Note that this argument is only
#'                   used when \code{std = TRUE}.
#' @param seed       a numeric value specifying the seed of the pseudo-random
#'                   numbers used when simulating multivariate normal data.
#' @param max.iter   a numeric value indicating the maximum number of iterations
#'                   when solving for error variances and correlation matrix.
#'                   Note that this argument is only used when \code{std = TRUE}.
#'                   argument is only used when \code{std = TRUE}.
#' @param check      logical: if \code{TRUE} (default), argument specification
#'                   is checked.
#'
#' @author
#' Takuya Yanagida
#'
#' @references
#' Genz, A., & Bretz, F. (2026). \emph{mvtnorm: Multivariate Normal and t Distributions}.
#' R package version 1.3-6. https://doi.org/10.32614/CRAN.package.mvtnorm
#'
#' Rosseel, Y. (2012). lavaan: An R Package for Structural Equation Modeling.
#' \emph{Journal of Statistical Software, 48}, 1-36. https://doi.org/10.18637/jss.v048.i02
#'
#' Schneider, W. J. (2021). \emph{simstandard: Generate standardized data}. R package
#' version 0.6.3. https://doi.org/10.32614/CRAN.package.simstandard
#'
#' @note This function uses the function \code{simulateData} from the R package
#' \pkg{lavaan} by Yves Rosseel (2012) when \code{std = FALSE} and is based on
#' modified copies of the function \code{sim_standardized} from the \pkg{simstandard}
#' package by W. Joel Schneider (2021) and the function \code{rmvnorm} from the
#' package \pkg{mvtnorm} by Alan Genz and Frank Bretz (2026) when \code{std = TRUE}.
#'
#' @return
#' Returns a data frame.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Model specification
#' model <- '# Measurement model
#'           f1 =~ 0.8*x1 + 0.7*x2 + 0.5*x3
#'           f2 =~ 0.7*x4 + 0.8*x5 + 0.6*x6
#'           # Factor correlation
#'           f1 ~~ 0.4*f2'
#'
#' # Example 1: Unstandardized parameters, simulate 200 cases
#' simdat1 <- sim.lavaan(model, n = 200)
#'
#' # Example 2: Standardized parameters, simulate 200 cases
#' simdat2 <- sim.lavaan(model, std = TRUE, n = 200)
#' }
sim.lavaan <- function(model, n = 500, std = FALSE, skew = NULL, kurt = NULL,
                       observed = TRUE, latent = FALSE, fscores = FALSE,
                       composites = FALSE, errors = FALSE, matrices = FALSE,
                       method = c("eigen", "svd", "chol"), seed = NULL,
                       max.iter = 100, check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'model' and object are NULL
  if (isTRUE(missing(model) || is.null(model))) { stop("Please specify the argument 'model'.", call. = FALSE) }

  # Check if input 'model' is a character string
  if (isTRUE(!is.character(model) || length(model) != 1L)) { stop("Please specify a character string for the argument 'model'.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  .check.input(logical = c("std", "observed", "latent", "errors", "fscores", "composites", "matrices"), s.character = list(method = c("eigen", "svd", "chol")), args = "n", envir = environment(), input.check = check)

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  # 'method' Argument
  if (isTRUE(all(c("eigen", "svd", "chol") %in% method))) { method <- "eigen" }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Standardized Parameters ####

  if (isTRUE(std)) {

    # Get main object from .sim.standardized.matrices
    model.char <- .sim.standardized.matrices(model, max.iter = max.iter, check = check)

    # Names of variables in S (Symmetric) matrix
    S_names <- c(model.char$v_names$v_observed_exogenous, model.char$v_names$v_observed_endogenous, model.char$v_names$v_latent_exogenous, model.char$v_names$v_latent_endogenous)

    # Set seed
    if (isTRUE(!is.null(seed))) { set.seed(seed) }

    # Simulate exogenous variables in S matrix
    u <- .rmvnorm(n = n, sigma = model.char$RAM_matrices$S[S_names, S_names, drop = FALSE], method = method)
    colnames(u) <- c(model.char$v_names$v_observed_exogenous, model.char$v_names$v_error, model.char$v_names$v_latent_exogenous, model.char$v_names$v_disturbance)

    # Create all variables from exogenous variables
    v <- u %*% t(model.char$RAM_matrices$iA[S_names, S_names, drop = FALSE])

    # Make blank matrix with n rows
    d.blank <- matrix(nrow = n, ncol = 0L)

    # Extract observed indicators of latent variables
    d.observed.indicators <- v[, model.char$v_names$v_observed_indicator, drop = FALSE]

    # Calculate estimated factor scores
    if (isTRUE(fscores && length(model.char$v_names$v_observed_indicator) > 0L)) {

      d_fscores <- d.observed.indicators %*% model.char$coefficients$factor_score

    } else {

      d_fscores <- d.blank

    }

    # Calculate composite scores
    if (isTRUE(composites && length(model.char$v_names$v_observed_indicator) > 0L)) {

      d_composite_scores <- d.observed.indicators %*% model.char$coefficients$composite_score

    } else {

      d_composite_scores <- d.blank

    }

    #——————————————————————————————————————
    ### Return Object ####

    object <- cbind(v[, c(model.char$v_names$v_observed, model.char$v_names$v_latent), drop = FALSE],
                    u[, c(model.char$v_names$v_disturbance, model.char$v_names$v_error), drop = FALSE],
                    d_fscores, d_composite_scores)

    # Decide which variables to return
    v_include <- character(0L)

    if (isTRUE(observed))   { v_include <- c(v_include, model.char$v_names$v_observed) }
    if (isTRUE(latent))     { v_include <- c(v_include, model.char$v_names$v_latent) }
    if (isTRUE(errors))     { v_include <- c(v_include, model.char$v_names$v_error) }
    if (isTRUE(errors))     { v_include <- c(v_include, model.char$v_names$v_disturbance) }
    if (isTRUE(fscores))    { v_include <- c(v_include, model.char$v_names$v_fscore) }
    if (isTRUE(composites)) { v_include <- c(v_include, model.char$v_names$v_composite_score) }

    object <- as.data.frame(object[, v_include])

    # Attach metadata as attribute
    if (isTRUE(matrices)) { attr(object, "matrices") <- model.char }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Unstandardized Parameters ####

  } else {

    object <- lavaan::simulateData(model, sample.nobs = n, seed = seed, skewness = skew, kurtosis = kurt)

  }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  return(object)

}

#_______________________________________________________________________________
