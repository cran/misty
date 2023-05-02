#' Confirmatory Factor Analysis
#'
#' This function is a wrapper function for conducting confirmatory factor analysis
#' with continuous and/or ordered-categorical indicators by calling the \code{cfa}
#' function in the R package \pkg{lavaan}.
#'
#' @param x                a matrix or data frame. If \code{model = NULL},
#'                         confirmatory factor analysis based on a measurement
#'                         model with one factor labeled \code{f} comprising all
#'                         variables in the matrix or data frame is conducted.
#'                         Note that the cluster variable is excluded from \code{x}
#'                         when specifying \code{cluster}. If \code{model} is
#'                         specified, the matrix or data frame needs to contain
#'                         all variables used in the argument \code{model} and
#'                         the cluster variable when specifying \code{cluster}.
#' @param model            a character vector specifying a measurement model with
#'                         one factor, or a list of character vectors for specifying
#'                         a measurement model with more than one factor, e.g.,
#'                         \code{model = c("x1", "x2", "x3", "x4")} for specifying
#'                         a measurement model with one factor labeled \code{f}
#'                         comprising four indicators, or
#'                         \code{model = list(factor1 = c("x1", "x2", "x3", "x4"),
#'                         factor2 = c("x5", "x6", "x7", "x8"))} for specifying a
#'                         measurement model with two latent factors labeled
#'                         \code{factor1} and \code{factor2} each comprising four
#'                         indicators. Note that the name of each list element is
#'                         used to label factors, i.e., all list elements need to
#'                         be named, otherwise factors are labeled with
#'                         \code{"f1", "f2", "f3"} and so on.
#' @param rescov           a character vector or a list of character vectors for
#'                         specifying residual covariances, e.g.
#'                         \code{rescov = c("x1", "x2")} for specifying a residual
#'                         covariance between items \code{x1} and \code{x2}, or
#'                         \code{rescov = list(c("x1", "x2"), c("x3", "x4"))} for
#'                         specifying residual covariances between items \code{x1}
#'                         and \code{x2}, and items \code{x3} and \code{x4}.
#' @param hierarch         logical: if \code{TRUE}, a second-order factor model
#'                         is specified given at least three first-order factors
#'                         were specified in \code{model}. Note that it is not
#'                         possible to specify more than one second-order factor.
#' @param meanstructure    logical: if \code{TRUE} (default), intercept/means of
#'                         observed variables means of latent variables will be
#'                         added to the model. Note that \code{meanstructure = FALSE}
#'                         is only applicable when the \code{missing} is
#'                         \code{listwise}, \code{pairwise}, or \code{doubly-robust}.
#' @param ident            a character string indicating the method used for
#'                         identifying and scaling latent variables, i.e.,
#'                         \code{"marker"} for the marker variable method fixing
#'                         the first factor loading of each latent variable to 1,
#'                         \code{"var"} for the fixed variance method fixing the
#'                         variance of each latent variable to 1, or \code{"effect"}
#'                         for the effects-coding method using equality constraints
#'                         so that the average of the factor loading for each
#'                         latent variable equals 1. By default, fixed variance
#'                         method is used when \code{hierarch = FALSE}, whereas
#'                         marker variable method is used when
#'                         \code{hierarch = TRUE}.
#' @param parameterization a character string indicating the method used for
#'                         identifying and scaling latent variables when indicators
#'                         are ordered, i.e., \code{"delta"} (default) for delta
#'                         parameterization and \code{"theta"} for theta
#'                         parameterization.
#' @param ordered          if \code{NULL} (default), all indicators of the
#'                         measurement model are treated as continuous. If
#'                         \code{TRUE}, all indicators of the measurement model
#'                         are treated as ordered (ordinal). Alternatively, a
#'                         character vector indicating which variables to treat
#'                         as ordered (ordinal) variables can be specified.
#' @param cluster          either a character string indicating the variable name
#'                         of the cluster variable in 'x' or a vector representing
#'                         the nested grouping structure (i.e., group or cluster
#'                         variable) for computing cluster-robust standard errors.
#'                         Note that cluster-robust standard errors are not
#'                         available when treating indicators of the measurement
#'                         model as ordered (ordinal).
#' @param estimator        a character string indicating the estimator to be used
#'                         (see 'Details'). By default, \code{"MLR"} is used for
#'                         CFA models with continuous indicators (i.e.,
#'                         \code{ordered = FALSE}) and \code{"WLSMV"} is used for
#'                         CFA model with ordered-categorical indicators (i.e.,
#'                         ordered = TRUE).
#' @param missing          a character string indicating how to deal with missing
#'                         data, i.e., \code{"listwise"} for listwise deletion,
#'                         \code{"pairwise"} for pairwise deletion, \code{"fiml"}
#'                         for full information maximum likelihood method,
#'                         \code{two.stage} for two-stage maximum likelihood
#'                         method, \code{robust.two.stage} for robust two-stage
#'                         maximum likelihood method, and \code{doubly-robust}
#'                         for doubly-robust method (see 'Details'). By default,
#'                         \code{"fiml"} is used for CFA models with continuous
#'                         indicators which are estimated by using
#'                         \code{estimator = "MLR"}, and \code{"pairwise"} for
#'                         CFA models with ordered-categorical indicators which
#'                         are estimated by using \code{estimator = "pairwise"}
#'                         by default.
#' @param print            a character string or character vector indicating which
#'                         results to show on the console, i.e. \code{"all"} for
#'                         all results, \code{"summary"} for a summary of the
#'                         specification of the estimation method and missing
#'                         data handling in lavaan, \code{"coverage"} for the
#'                         variance-covariance coverage of the data,
#'                         \code{"descript"} for descriptive statistics,
#'                         \code{"fit"} for model fit, \code{"est"} for parameter
#'                         estimates, and \code{"modind"} for modification
#'                         indices. By default, a summary of the specification,
#'                         model fit, and parameter estimates are printed.
#' @param min.value        numeric value to filter modification indices and only
#'                         show modifications with a modification index value
#'                         equal or higher than this minimum value. By default,
#'                         modification indices equal or higher 10 is printed.
#' @param digits           an integer value indicating the number of decimal places
#'                         to be used for displaying results.
#' @param p.digits         an integer value indicating the number of decimal places
#'                         to be used for displaying the \emph{p}-value.
#' @param as.na            a numeric vector indicating user-defined missing values,
#'                         i.e. these values are converted to \code{NA} before
#'                         conducting the analysis. Note that \code{as.na()}
#'                         function is only applied to \code{x} but not to
#'                         \code{cluster}.
#' @param write            a character string for writing the results into a Excel
#'                         file naming a file with or without file extension '.xlsx',
#'                         e.g., \code{"Results.xlsx"} or \code{"Results"}.
#' @param check            logical: if \code{TRUE}, argument specification is checked.
#' @param output           logical: if \code{TRUE}, output is shown.
#'
#' @details
#' \describe{
#' \item{\strong{Estimator}}{The R package \pkg{lavaan} provides seven estimators
#' that affect the estimation, namely \code{"ML"}, \code{"GLS"}, \code{"WLS"},
#' \code{"DWLS"}, \code{"ULS"}, \code{"DLS"}, and \code{"PML"}. All other options
#' for the argument \code{estimator} combine these estimators with various standard
#' error and chi-square test statistic computation. Note that the estimators also
#' differ in how missing values can be dealt with (e.g., listwise deletion,
#' pairwise deletion, or full information maximum likelihood, FIML).
#'   \itemize{
#'      \item{\code{"ML"}}: Maximum likelihood with conventional standard errors
#'      and conventional test statistic. For both complete and incomplete data
#'      using pairwise deletion or FIML.
#'      \item{\code{"MLM"}}: Maximum likelihood parameter estimates with conventional
#'      robust standard errors and a Satorra-Bentler scaled test statistic that
#'      are robust to non-normality. For complete data only.
#'      \item{\code{"MLMV"}}: Maximum likelihood parameter estimates with conventional
#'      robust standard errors and a mean and a variance adjusted test statistic
#'      using a scale-shifted approach that are robust to non-normality. For complete
#'      data only.
#'      \item{\code{"MLMVS"}}: Maximum likelihood parameter estimates with conventional
#'      robust standard errors and a mean and a variance adjusted test statistic
#'      using the Satterthwaite approach that are robust to non-normality. For complete
#'      data only.
#'      \item{\code{"MLF"}}: Maximum likelihood parameter estimates with standard
#'      errors approximated by first-order derivatives and conventional test statistic.
#'      For both complete and incomplete data using pairwise deletion or FIML.
#'      \item{\code{"MLR"}}: Maximum likelihood parameter estimates with Huber-White
#'      robust standard errors a test statistic which is asymptotically equivalent
#'      to the Yuan-Bentler T2* test statistic that are robust to non-normality
#'      and non-independence of observed when specifying a cluster variable using
#'      the argument \code{cluster}. For both complete and incomplete data using
#'      pairwise deletion or FIML.
#'      \item{\code{"GLS"}}: Generalized least squares parameter estimates with
#'      conventional standard errors and conventional test statistic that uses a
#'      normal-theory based weight matrix. For complete data only.
#'      and conventional chi-square test. For both complete and incomplete data.
#'      \item{\code{"WLS"}}: Weighted least squares parameter estimates (sometimes
#'      called ADF estimation) with conventional standard errors and conventional
#'      test statistic that uses a full weight matrix. For complete data only.
#'      \item{\code{"DWLS"}}: Diagonally weighted least squares parameter estimates
#'      which uses the diagonal of the weight matrix for estimation with conventional
#'      standard errors and conventional test statistic. For both complete and
#'      incomplete data using pairwise deletion.
#'      \item{\code{"WLSM"}}: Diagonally weighted least squares parameter estimates
#'      which uses the diagonal of the weight matrix for estimation, but uses the
#'      full weight matrix for computing the conventional robust standard errors
#'      and a Satorra-Bentler scaled test statistic. For both complete and incomplete
#'      data using pairwise deletion.
#'      \item{\code{"WLSMV"}}: Diagonally weighted least squares parameter estimates
#'      which uses the diagonal of the weight matrix for estimation, but uses the
#'      full weight matrix for computing the conventional robust standard errors
#'      and a mean and a variance adjusted test statistic using a scale-shifted
#'      approach. For both complete and incomplete data using pairwise deletion.
#'      \item{\code{"ULS"}}: Unweighted least squares parameter estimates with
#'      conventional standard errors and conventional test statistic. For both
#'      complete and incomplete data using pairwise deletion.
#'      \item{\code{"ULSM"}}: Unweighted least squares parameter estimates with
#'      conventional robust standard errors and a Satorra-Bentler scaled test
#'      statistic. For both complete and incomplete data using pairwise deletion.
#'      \item{\code{"ULSMV"}}: Unweighted least squares parameter estimates with
#'      conventional robust standard errors and a mean and a variance adjusted
#'      test statistic using a scale-shifted approach. For both complete and
#'      incomplete data using pairwise deletion.
#'      \item{\code{"DLS"}}: Distributionally-weighted least squares parameter
#'      estimates with conventional robust standard errors and a Satorra-Bentler
#'      scaled test statistic. For complete data only.
#'      \item{\code{"PML"}}: Pairwise maximum likelihood parameter estimates
#'      with Huber-White robust standard errors and a mean and a variance adjusted
#'      test statistic using the Satterthwaite approach. For both complete and
#'      incomplete data using pairwise deletion.
#'   }
#' }
#' \item{\strong{Missing Data}}{The R package \pkg{lavaan} provides six methods
#' for dealing with missing data:
#'   \itemize{
#'      \item{\code{"listwise"}}: Listwise deletion, i.e., all cases with missing
#'      values are removed from the data before conducting the analysis. This is
#'      only valid if the data are missing completely at random (MCAR).
#'      \item{\code{"pairwise"}}: Pairwise deletion, i.e., each element of a
#'      variance-covariance matrix is computed using cases that have data needed
#'      for estimating that element. This is only valid if the data are missing
#'      completely at random (MCAR).
#'      \item{\code{"fiml"}}: Full information maximum likelihood (FIML) method,
#'      i.e., likelihood is computed case by case using all available data from
#'      that case. FIML method is only applicable for following estimators:
#'      \code{"ML"}, \code{"MLF"}, and \code{"MLR"}.
#'      \item{\code{"two.stage"}}: Two-stage maximum likelihood estimation, i.e.,
#'      sample statistics is estimated using EM algorithm in the first step. Then,
#'      these estimated sample statistics are used as input for a regular analysis.
#'      Standard errors and test statistics are adjusted correctly to reflect the
#'      two-step procedure. Two-stage method is only applicable for following
#'      estimators: \code{"ML"}, \code{"MLF"}, and \code{"MLR"}.
#'      \item{\code{"robust.two.stage"}}: Robust two-stage maximum likelihood
#'      estimation, i.e., two-stage maximum likelihood estimation with standard
#'      errors and a test statistic that are robust against non-normality. Robust
#'      two-stage method is only applicable for following estimators: \code{"ML"},
#'      \code{"MLF"}, and \code{"MLR"}.
#'      \item{\code{"doubly.robust"}}: Doubly-robust method only applicable for
#'      pairwise maximum likelihood estimation (i.e., \code{estimator = "PML"}.
#'   }
#' }
#' \item{\strong{Convergence and model idenfitification checks}}{In line with the
#' R package \pkg{lavaan}, this functions provides several checks for model
#' convergence and model identification:
#'   \itemize{
#'      \item{\code{Degrees of freedom}}: An error message is printed if the number
#'      of degrees of freedom is negative, i.e., the model is not identified.
#'      \item{\code{Model convergence}}: An error message is printed if the
#'      optimizer has not converged, i.e., results are most likely unreliable.
#'      \item{\code{Standard errors}}: An error message is printed if the standard
#'      errors could not be computed, i.e., the model might not be identified.
#'      \item{\code{Variance-covariance matrix of the estimated parameters}}: A
#'      warning message is printed if the variance-covariance matrix of the
#'      estimated parameters is not positive definite, i.e., the smallest eigenvalue
#'      of the matrix is smaller than zero or very close to zero.
#'      \item{\code{Negative variances of observed variables}}: A warning message
#'      is printed if the estimated variances of the observed variables are
#'      negative.
#'      \item{\code{Variance-covariance matrix of observed variables}}: A warning
#'      message is printed if the estimated variance-covariance matrix of the
#'      observed variables is not positive definite, i.e., the smallest eigenvalue
#'      of the matrix is smaller than zero or very close to zero.
#'      \item{\code{Negative variances of latent variables}}: A warning message
#'      is printed if the estimated variances of the latent variables are
#'      negative.
#'      \item{\code{Variance-covariance matrix of latent variables}}: A warning
#'      message is printed if the estimated variance-covariance matrix of the
#'      latent variables is not positive definite, i.e., the smallest eigenvalue
#'      of the matrix is smaller than zero or very close to zero.
#'   }
#'   Note that unlike the R package \pkg{lavaan}, the \code{item.cfa} function does
#'   not provide any results when the degrees of freedom is negative, the model
#'   has not converged, or standard errors could not be computed.
#' }
#' \item{\strong{Model Fit}}{The \code{item.cfa} function provides the chi-square
#' test, incremental fit indices (i.e., CFI and TLI), and absolute fit indices
#' (i.e., RMSEA, and SRMR) to evaluate overall model fit. However, different
#' versions of the CFI, TLI, and RMSEA are provided depending on the estimator.
#' Unlike the R package \pkg{lavaan}, the different versions are labeled with
#' \code{Standard}, \code{Ad hoc}, and \code{Robust} in the output:
#'   \itemize{
#'      \item{\code{"Standard"}}: CFI, TLI, and RMSEA without any non-normality
#'      corrections. These fit measures based on the normal theory maximum
#'      likelihood test statistic are sensitive to deviations from multivariate
#'      normality of endogenous variables. Simulation studies by Brosseau-Liard
#'      et al. (2012), and Brosseau-Liard and Savalei (2014) showed that the
#'      uncorrected fit indices are affected by non-normality, especially at small
#'      and medium sample sizes (e.g., n < 500).
#'      \item{\code{"Ad hoc"}}: Population-corrected robust CFI, TLI, and RMSEA
#'      with ad hoc non-normality corrections that simply replace the maximum
#'      likelihood test statistic with a robust test statistic (e.g., mean-adjusted
#'      chi-square). These fit indices change the population value being estimated
#'      depending on the degree of non-normality present in the data. Brosseau-Liard
#'      et al. (2012) demonstrated that the ad hoc corrected RMSEA increasingly
#'      accepts poorly fitting models as non-normality in the data increases, while
#'      the effect of the ad hoc correction on the CFI and TLI is less predictable
#'      with non-normality making fit appear worse, better, or nearly unchanged
#'      (Brosseau-Liard & Savalei, 2014).
#'      \item{\code{"Robust"}}: Sample-corrected robust CFI, TLI, and RMSEA
#'      with non-normality corrections based on formula provided by Li and Bentler
#'      (2006) and Brosseau-Liard and Savalei (2014). These fit indices do not
#'      change the population value being estimated and can be interpreted the
#'      same way as the uncorrected fit indices when the data would have been
#'      normal.
#'   }
#'   In conclusion, the use of sample-corrected fit indices (\code{Robust})
#'   instead of population-corrected fit indices (\code{Ad hoc}) is recommended.
#'   Note that when sample size is very small (e.g., n < 200), non-normality
#'   correction does not appear to adjust fit indices sufficiently to counteract
#'   the effect of non-normality (Brosseau-Liard & Savalei, 2014).
#' }
#' }
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#'\code{\link{item.alpha}}, \code{\link{item.omega}}, \code{\link{item.scores}}
#'
#' @references
#' Brosseau-Liard, P. E., Savalei, V., & Li. L. (2012). An investigation of the
#' sample performance of two nonnormality corrections for RMSEA,
#' \emph{Multivariate Behavioral Research, 47}, 904-930.
#' https://doi.org/10.1080/00273171.2014.933697
#'
#' Brosseau-Liard, P. E., & Savalei, V. (2014) Adjusting incremental fit indices
#' for nonnormality. \emph{Multivariate Behavioral Research, 49}, 460-470.
#' https://doi.org/10.1080/00273171.2014.933697
#'
#' Li, L., & Bentler, P. M. (2006). Robust statistical tests for evaluating the
#' hypothesis of close fit of misspecified mean and covariance structural models.
#' \emph{UCLA Statistics Preprint #506}. University of California.
#'
#' Rosseel, Y. (2012). lavaan: An R Package for Structural Equation Modeling.
#' \emph{Journal of Statistical Software, 48}, 1-36. https://doi.org/10.18637/jss.v048.i02
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#' \tabular{ll}{
#' \code{call} \tab function call \cr
#' \code{type} \tab type of analysis \cr
#' \code{data} \tab matrix or data frame specified in \code{x} \cr
#' \code{args} \tab specification of function arguments \cr
#' \code{model} \tab specified model \cr
#' \code{model.fit} \tab fitted lavaan object (\code{mod.fit}) \cr
#' \code{check} \tab results of the convergence and model identification check \cr
#' \code{result} \tab list with result tables \cr
#' }
#'
#' @note
#' The function uses the functions \code{cfa}, \code{lavInspect}, \code{lavTech},
#' \code{modindices}, \code{parameterEstimates}, and \code{standardizedsolution}
#' provided in the R package \pkg{lavaan} by Yves Rosseel (2012).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' Load data set "HolzingerSwineford1939" in the lavaan package
#' data("HolzingerSwineford1939", package = "lavaan")
#'
#' #---------------------------
#' # Measurement model with one factor
#'
#' # Specification using the argument 'x'
#' item.cfa(HolzingerSwineford1939[, c("x1", "x2", "x3")])
#'
#' # Alternative specification using the argument 'model'
#' item.cfa(HolzingerSwineford1939, model = c("x1", "x2", "x3"))
#'
#' # Alternative specification using the argument 'model'
#' item.cfa(HolzingerSwineford1939, model = list(visual = c("x1", "x2", "x3")))
#'
#' #---------------------------
#' # Measurement model with three factors
#'
#' # Specification using the argument 'model'
#' item.cfa(HolzingerSwineford1939,
#'          model = list(visual = c("x1", "x2", "x3"),
#'                       textual = c("x4", "x5", "x6"),
#'                       speed = c("x7", "x8", "x9")))
#'
#' #---------------------------
#' # Residual covariances
#'
#' # One residual covariance
#' item.cfa(HolzingerSwineford1939,
#'          model = list(visual = c("x1", "x2", "x3"),
#'                       textual = c("x4", "x5", "x6"),
#'                       speed = c("x7", "x8", "x9")),
#'          rescov = c("x1", "x2"))
#'
#' # Two residual covariances
#' item.cfa(HolzingerSwineford1939,
#'          model = list(visual = c("x1", "x2", "x3"),
#'                       textual = c("x4", "x5", "x6"),
#'                       speed = c("x7", "x8", "x9")),
#'          rescov = list(c("x1", "x2"), c("x4", "x5")))
#'
#' #---------------------------
#' # Second-order factor model based on three first-order factors
#'
#' item.cfa(HolzingerSwineford1939,
#'          model = list(visual = c("x1", "x2", "x3"),
#'                       textual = c("x4", "x5", "x6"),
#'                       speed = c("x7", "x8", "x9")),
#'          hierarch = TRUE)
#'
#' #---------------------------
#' # Measurement model with ordered-categorical indicators
#'
#' item.cfa(round(HolzingerSwineford1939[, c("x4", "x5", "x6")]), ordered = TRUE)
#'
#' #---------------------------
#' # Cluster-robust standard errors
#'
#' # Load data set "Demo.twolevel" in the lavaan package
#' data("Demo.twolevel", package = "lavaan")
#'
#' # Specification using a variable in 'x'
#' item.cfa(Demo.twolevel[, c("y4", "y5", "y6", "cluster")], cluster = "cluster")
#'
#' # Specification of the cluster variable in 'cluster'
#' item.cfa(Demo.twolevel[, c("y4", "y5", "y6")], cluster = Demo.twolevel$cluster)
#'
#' # Specification using a variable in 'x'
#' item.cfa(Demo.twolevel, model = c("y4", "y5", "y6"), cluster = "cluster")
#'
#' # Specification of the cluster variable in 'cluster'
#' item.cfa(Demo.twolevel, model = c("y4", "y5", "y6"), cluster = Demo.twolevel$cluster)
#'
#' #---------------------------
#' # Print argument
#'
#' # Request all results
#' item.cfa(HolzingerSwineford1939[, c("x1", "x2", "x3")], print = "all")
#'
#' # Request modification indices with value equal or higher than 5
#' item.cfa(HolzingerSwineford1939[, c("x1", "x2", "x3", "x4")],
#'          print = "modind", min.value = 5)
#'
#' #---------------------------
#' # lavaan summary of the estimated model
#'
#' mod <- item.cfa(HolzingerSwineford1939[, c("x1", "x2", "x3")], output = FALSE)
#'
#' lavaan::summary(mod$mod.fit, standardized = TRUE, fit.measures = TRUE)
#'
#' #---------------------------
#' # Write Results into a Excel file
#' item.cfa(HolzingerSwineford1939[, c("x1", "x2", "x3")], write = "CFA.xlsx")
#'
#' result <- item.cfa(HolzingerSwineford1939[, c("x1", "x2", "x3")], output = FALSE)
#' write.result(result, "CFA.xlsx")
#' }
item.cfa <- function(x, model = NULL, rescov = NULL, hierarch = FALSE, meanstructure = TRUE,
                     ident = c("marker", "var", "effect"), parameterization = c("delta", "theta"),
                     ordered = NULL, cluster = NULL,
                     estimator = c("ML", "MLM", "MLMV", "MLMVS", "MLF", "MLR",
                                   "GLS", "WLS", "DWLS", "WLSM", "WLSMV",
                                   "ULS", "ULSM", "ULSMV", "DLS", "PML"),
                     missing = c("listwise", "pairwise", "fiml", "two.stage", "robust.two.stage", "doubly.robust"),
                     print = c("all", "summary", "coverage", "descript", "fit", "est", "modind"),
                     min.value = 10, digits = 3, p.digits = 3, as.na = NULL, write = NULL,
                     check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # R package lavaan
  if (isTRUE(!nzchar(system.file(package = "lavaan")))) { stop("Package \"lavaan\" is needed for this function, please install the package.", call. = FALSE) }

  # Check if input 'x' is missing
  if (isTRUE(missing(x))) { stop("Please specify a matrix or data frame for the argument 'x'.", call. = FALSE) }

  # Check if input 'x' is NULL
  if (isTRUE(is.null(x))) { stop("Input specified for the argument 'x' is NULL.", call. = FALSE) }

  # Check if input 'x' is a matrix or a data frame
  if (isTRUE(!is.matrix(x) && !is.data.frame(x))) { stop("Please specify a matrix or a data frame for the argument 'x'.", call. = FALSE) }

  # Check if input 'model' is a character vector or list of character vectors
  if (isTRUE(!is.null(model) && !all(sapply(model, is.character)))) { stop("Please specify a character vector or list of character vectors for the argument 'model'.", call. = FALSE) }

  # Check if variables in input 'model' are available in input 'x'
  if (isTRUE(!is.null(model) && any(!unlist(model) %in% colnames(x)))) {

    stop(paste0("Items specified in the argument 'model' were not found in 'x': ", paste(unique(unlist(model))[!unique(unlist(model)) %in% colnames(x)], collapse = ", ")), call. = FALSE)

  }

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  if (isTRUE(check)) {

    # Check input 'cluster'
    if (isTRUE(!is.null(cluster))) {

      # Name of the cluster variable in 'x'
      if (isTRUE(length(cluster) == 1L)) {

        # One cluster variable
        if (isTRUE(!is.character(cluster))) { stop("Please specify a character string for the name of the cluster variable in 'x'", call. = FALSE) }

        # Cluster variable in 'x'
        if (isTRUE(!cluster %in% colnames(x))) { stop(paste0("Cluster variable \"", cluster, "\" specified in 'cluster' was not found in 'x'"), call. = FALSE) }

      # Cluster variable
      } else {

        # Length of cluster variable
        if (isTRUE(nrow(x) != length(cluster))) { stop("The cluster variable does not match with the number of rows in 'x'.",call. = FALSE) }

        # Number of groups
        if (isTRUE(length(unique(na.omit(cluster))) == 1L)) { stop("There is only one group represented in the cluster variable 'cluster'.", call. = FALSE) }

      }

    }

    # Check input 'x'
    if (isTRUE(is.null(cluster) || length(cluster) != 1L)) {

      if (isTRUE(is.null(model) && ncol(data.frame(x)) < 3L)) { stop("Please specify at least three indicators for the measurement model in 'x'.", call. = FALSE) }

    } else if (isTRUE(cluster == 1L)) {

      if (isTRUE(is.null(model) && ncol(data.frame(x)[, !colnames(data.frame(x)) %in% cluster, drop = FALSE]) < 3L)) { stop("Please specify at least three indicators for the measurement model in 'x'.", call. = FALSE) }

    }

    # Check input 'model'
    if (isTRUE(!is.null(model))) {

      if (isTRUE(!is.list(model))) {

        if (isTRUE(length(unique(model)) < 3L)) { stop("Please specify at least three indicators for the measurement model.", call. = FALSE) }

      } else {

        if (isTRUE(length(model) == 1L && length(unique(unlist(model))) < 3L)) { stop("Please specify at least three indicators for the measurement model.", call. = FALSE) }

      }

    }

    # Check input 'rescov'
    if (isTRUE(!is.null(rescov))) {

      # Two variables for each residual covariance
      if (isTRUE(is.list(rescov)) && any(sapply(rescov, length) != 2L)) {

        stop("Please specify a list of character vectors for the argument 'rescov', where each element has two variable names", call. = FALSE)

      } else {

        if (isTRUE(length(rescov) != 2L)) {

          stop("Please specify a character vector with two variable names for the argument 'rescov'", call. = FALSE)

        }

      }

      # Model specification without 'model'
      if (isTRUE(is.null(model))) {

        rescov.items <- !unique(unlist(rescov)) %in% colnames(x)
        if (isTRUE(any(rescov.items))) {

          stop(paste0("Items specified in the argument 'rescov' were not found in 'x': ", paste(unique(unlist(rescov))[rescov.items], collapse = ", ")), call. = FALSE)

        }

      # Model specification with 'model'
      } else {

        rescov.items <- !unique(unlist(rescov)) %in% unique(unlist(model))
        if (isTRUE(any(rescov.items))) {

          stop(paste0("Items specified in the argument 'rescov' were not found in 'model': ", paste(unique(unlist(rescov))[rescov.items], collapse = ", ")), call. = FALSE)

        }

      }

    }

    # Check input 'hierarch'
    if (isTRUE(!is.logical(hierarch))) { stop("Please specify TRUE or FALSE for the argument 'hierarch'.", call. = FALSE) }

    # Check input 'meanstructure'
    if (isTRUE(!is.logical(meanstructure))) { stop("Please specify TRUE or FALSE for the argument 'meanstructure'.", call. = FALSE) }

    # Check input 'ident'
    if (isTRUE(!all(ident %in% c("marker", "var", "effect")))) { stop("Character string in the argument 'ident' does not match with \"marker\", \"var\", or \"effect\".", call. = FALSE) }

    # Check input 'parameterization'
    if (isTRUE(!all(parameterization %in% c("delta", "theta")))) { stop("Character string in the argument 'parameterization' does not match with \"delta\" or \"theta\".", call. = FALSE) }

    # Check input 'ordered'
    if (isTRUE(!is.null(ordered) && !is.logical(ordered))) {

      # Model specification without 'model'
      if (isTRUE(is.null(model))) {

        if (isTRUE(any(!ordered %in% colnames(x)))) {

          stop(paste0("Variables specified in the argument 'ordered' were not found in 'x': ", paste(x[!ordered %in% colnames(x)], collapse = ", ")), call. = FALSE)

        }

      # Model specification with 'model'
      } else {

        if (isTRUE(any(!ordered %in% unlist(model)))) {

          stop(paste0("Variables specified in the argument 'ordered' were not found in 'model': ", paste(ordered[!ordered %in% unlist(model)], collapse = ", ")), call. = FALSE)

        }

      }

    }

    #......
    # Check input 'estimator'
    if (isTRUE(!all(estimator %in% c("ML", "MLM", "MLMV", "MLMVS", "MLF", "MLR",
                                     "GLS", "WLS", "DWLS", "WLSM", "WLSMV",
                                     "ULS", "ULSM", "ULSMV", "DLS", "PML")))) {

      stop("Character string in the argument 'estimator' does not match with \"ML\", \"MLM\", \"MLMV\", \"MLMVS\", \"MLF\", \"MLR\", \"GLS\", \"WLS\", \"DWLS\", \"WLSM\", \"WLSMV\", \"ULS\", \"ULSM\", \"ULSMV\", \"DLS\", or \"PML\".", call. = FALSE)

    }

    # Check input 'missing'
    if (isTRUE(!all(missing %in% c("listwise", "pairwise", "fiml", "two.stage", "robust.two.stage", "doubly.robust")))) {

      stop("Character string in the argument 'missing' does not match with \"listwise\", \"pairwise\", \"fiml\", \"two.stage\", \"robust.two.stage\", or \"doubly.robust\".", call. = FALSE)

    }

    # Check input 'print'
    if (isTRUE(!all(print %in% c("all", "summary", "coverage", "descript", "fit", "est", "modind")))) {

      stop("Character strings in the argument 'print' do not all match with \"summary\", \"coverage\", \"descript\", \"fit\", \"est\", or \"modind\".", call. = FALSE)

    }

    # Check input 'min.value'
    if (isTRUE(min.value <= 0L)) { stop("Please specify a value greater than 0 for the argument 'min.value'.", call. = FALSE) }

    # Check input 'digits'
    if (isTRUE(digits %% 1L != 0L || digits < 0L)) { stop("Specify a positive integer number for the argument 'digits'.", call. = FALSE) }

    # Check input 'p.digits'
    if (isTRUE(p.digits %% 1L != 0L || p.digits < 0L)) { stop("Specify a positive integer number for the argument 'p.digits'.", call. = FALSE) }

    # Check input 'output'
    if (isTRUE(!is.logical(output))) { stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Data and Arguments ---------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Manifest variables ####

  # Model specification with 'x'
  if (isTRUE(is.null(model))) {

    # No cluster variable
    if (isTRUE(is.null(cluster))) {

      var <- colnames(x)

    # Cluster variable
    } else {

      if (isTRUE(length(cluster) == 1L)) {

        var <- colnames(x)[!colnames(x) %in% cluster]

      } else {

        var <- colnames(x)

      }

    }

  # Model specification with 'model'
  } else {

    var <- unique(unlist(model))

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data frame with Cluster Variable ####

  # No cluster variable
  if (isTRUE(is.null(cluster))) {

    x <- data.frame(x[, var], stringsAsFactors = FALSE)

  # Cluster variable
  } else {

    if (isTRUE(length(cluster) == 1L)) {

      x <- data.frame(x[, var], .cluster = x[, cluster], stringsAsFactors = FALSE)

    } else {

      x <- data.frame(x[, var], .cluster = cluster, stringsAsFactors = FALSE)

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) {

    x[, var] <- misty::as.na(x[, var], na = as.na, check = check)

    # Variable with missing values only
    x.miss <- vapply(x[, var], function(y) all(is.na(y)), FUN.VALUE = logical(1L))
    if (isTRUE(any(x.miss))) {

      stop(paste0("After converting user-missing values into NA, following ",
                  ifelse(length(which(x.miss)) == 1L, "variable is ", "variables are "), "completely missing: ",
                  paste(names(which(x.miss)), collapse = ", ")), call. = FALSE)

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model ####

  # Factor labels
  if (isTRUE(!is.null(model) && is.list(model) && (is.null(names(model)) || any(names(model) == "")))) {

    names(model) <- paste0("f", seq_along(model))

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Residual covariance ####

  if (isTRUE(!is.null(rescov) && !is.list(rescov))) {

    rescov <- list(rescov)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Hierarch ####

  if (isTRUE(hierarch)) {

    if (isTRUE(is.null(model) || !is.list(model) || length(model) < 3L)) {

      stop("Please specify at least three first-order factors for the second-order factor model.", call. = FALSE)

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model identification ####

  if (isTRUE(all(c("marker", "var", "effect") %in% ident))) {

    if (isTRUE(hierarch)) {

      ident  <- "marker"

    } else {

      ident <- "var"

    }

  }

  if (isTRUE(ident == "marker")) {

    std.lv <- FALSE
    effect.coding <- FALSE

  } else if (isTRUE(ident == "var")) {

    std.lv <- TRUE
    effect.coding <- FALSE

  } else if (isTRUE(ident == "effect")) {

    std.lv <- FALSE
    effect.coding <- TRUE

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Parameterization ####

  if (isTRUE(all(c("delta", "theta") %in% parameterization))) {

    parameterization <- "delta"

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Estimator ####

  #...................
  ### Default setting ####
  if (isTRUE(all(c("ML", "MLM", "MLMV", "MLMVS", "MLF", "MLR",
                   "GLS", "WLS", "DWLS", "WLSM", "WLSMV",
                   "ULS", "ULSM", "ULSMV", "DLS", "PML") %in% estimator))) {

    # Continuous indicators
    if (isTRUE(is.null(ordered) || !isTRUE(ordered))) {

      if (isTRUE(all(c("ML", "MLM", "MLMV", "MLMVS", "MLF", "MLR",
                       "GLS", "WLS", "DWLS", "WLSM", "WLSMV", "ULS", "ULSM", "ULSMV", "DLS", "PML") %in% estimator ))) { estimator  <- "MLR" }

    # Categorical indicators
    } else {

      if (isTRUE(all(c("ML", "MLM", "MLMV", "MLMVS", "MLF", "MLR",
                       "GLS", "WLS", "DWLS", "WLSM", "WLSMV", "ULS", "ULSM", "ULSMV", "DLS", "PML") %in% estimator))) { estimator  <- "WLSMV" }

      # Cluster-robust standard errors
      if (isTRUE(!is.null(cluster))) {

        stop("Cluster-robust standard errors are not available with ordered-categorical indicators.", call. = FALSE)

      }

    }

  #...................
  ### User-specified ####
  } else {

    # Continuous indicators
    if (isTRUE(is.null(ordered))) {

      # Cluster-robust standard errors
      if (isTRUE(!is.null(cluster) && !estimator %in% c("MLR", "MLM", "MLMV", "MLMVS"))) {

        warning("Estimator switched to \"MLR\" to computer cluster-robust standard errors.", call. = FALSE)

        estimator <- "MLR"

      }

    # Categorical indicators
    } else {

      # Cluster-robust standard errors
      if (isTRUE(is.null(cluster))) {

        if (isTRUE(!estimator %in% c("WLS", "DWLS", "WLSM", "WLSMV", "ULS", "ULSM", "ULSMV", "PML"))) {

          warning("Estimator switched to \"WLSMV\" to deal with ordered-categorical indicators.", call. = FALSE)

          estimator <- "WLSMV"

        }

      } else {

        stop("Cluster-robust standard errors are not available with ordered-categorical indicators.", call. = FALSE)

      }

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Missing ####

  # Any missing values
  if (isTRUE(any(is.na(x[, var])))) {

    complete <- FALSE

    #...................
    ### Default setting ####
    if (isTRUE(all(c("listwise", "pairwise", "fiml", "two.stage", "robust.two.stage", "doubly.robust") %in% missing))) {

      if (isTRUE(estimator %in% c("ML", "MLF", "MLR")))  {

        missing <- "fiml"

      } else if (isTRUE(estimator %in% c("MLM", "MLMV", "MLMVS", "GLS", "WLS")))  {

        missing <- "listwise"

      } else if (isTRUE(estimator %in% c("DWLS", "WLSM", "WLSMV", "ULS", "ULSM", "ULSMV", "DLS", "PML")))  {

        missing <- "pairwise"

      }

    #...................
    ### User-specified ####
    } else {

      # FIML
      if (isTRUE(missing == "fiml" && !estimator %in% c("ML", "MLF", "MLR"))) {

        warning(paste0("FIML method is not available for estimator = \"", estimator, "\", argument 'missing' switched to ",
                       ifelse(estimator %in% c("MLM", "MLMV", "MLMVS", "GLS", "WLS", "DLS"), "\"listwise\"", "\"pairwise\""), "."), call. = FALSE)

        missing <- ifelse(estimator %in% c("MLM", "MLMV", "MLMVS", "GLS", "WLS", "DLS"), "listwise", "pairwise")

      }

      # Pairwise deletion
      if (isTRUE(missing == "pairwise" && !estimator %in% c("ML", "WLS", "DWLS", "WLSM", "WLSMV", "ULS", "ULSM", "ULSMV", "PML"))) {

        warning(paste0("Pairwise deletion is not available for estimator = \"", estimator, "\", argument 'missing' switched to ",
                       ifelse(estimator %in% c("MLF", "MLR"), "\"fiml\"", "\"listwise\""), "."), call. = FALSE)

        missing <- ifelse(estimator %in% c("MLF", "MLR"), "fiml", "listwise")

      }

      # (Robust) Two-stage
      if (isTRUE(missing %in% c("two.stage", "robust.two.stage") && !estimator %in% c("ML", "MLF", "MLR"))) {

        warning(paste0("Two-stage method is not available for estimator = \"", estimator, "\", argument 'missing' switched to ",
                       ifelse(estimator %in% c("WLS", "DWLS", "WLSM", "WLSMV", "ULS", "ULSM", "PML"), "\"pairwise\"", "\"listwise\""), "."),
                call. = FALSE)

        missing <- ifelse(estimator %in% c("WLS", "DWLS", "WLSM", "WLSMV", "ULS", "ULSM", "PML"), "pairwise", "listwise")

      }

      # Doubly-robust
      if (isTRUE(missing == "doubly.robust" && estimator != "PML")) {

        warning(paste0("Doubly-robust method is not available for estimator = \"", estimator, "\", argument 'missing' switched to ",
                       ifelse(estimator %in% c("ML", "MLF", "MLR"), "fiml\"", ifelse(estimator %in% c("MLM", "MLMV", "MLMVS", "GLS", "WLS"), "\"listwise\"", "\"pairwise\"")), "."),
                call. = FALSE)

        missing <- ifelse(estimator %in% c("ML", "MLF", "MLR"), "fiml", ifelse(estimator %in% c("MLM", "MLMV", "MLMVS", "GLS", "WLS"), "listwise", "pairwise"))

      }

    }

  } else {

    missing <- "listwise"
    complete <- TRUE

  }

  # Cases with missing on all variables
  if (isTRUE(missing %in% c("fiml", "two.stage", "robust.two.stage"))) {

    x.na.prop <- misty::na.prop(x[, var])
    if (any(x.na.prop == 1L)) {

      warning(paste("Data set contains", sum(x.na.prop == 1L), "cases with missing on all variables which were not included in the analysis."),
              call. = FALSE)

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Print ####

  if (isTRUE(all(c("all", "summary", "coverage", "descript", "fit", "est", "modind") %in% print))) {

    print  <- c("summary", "fit", "est")

  }

  if (isTRUE(length(print) == 1L && "all" %in% print)) {

    print <- c("summary", "coverage", "descript", "fit", "est", "modind")

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Covariance coverage ####

  coverage <- misty::na.coverage(x[, var], output = FALSE)$result

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Sample Statistics ####

  # Descriptive statistics
  itemstat <- misty::descript(x[, var], output = FALSE)$result[, c("variable", "n", "nNA", "pNA", "m", "sd", "min", "max", "skew", "kurt")]

  # Frequency table
  itemfreq <- misty::freq(x[, var], val.col = TRUE, exclude = 9999, output = FALSE)$result

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model specification ####

  #...................
  ### Latent variable ####

  # One-factor
  if (isTRUE(is.null(model))) {

    mod.factor <- paste("f =~", paste(var, collapse = " + "))

  } else {

    # One-factor
    if (isTRUE(!is.list(model))) {

      mod.factor <- paste("f =~", paste(model, collapse = " + "))

    # One or more than one factor
    } else {

      mod.factor <- paste(sapply(names(model), function(y) paste(y, "=~", paste(model[[y]], collapse = " + "))),
                          collapse = " \n ")

    }

  }

  #...................
  ### Second-order factor ####
  if (isTRUE(hierarch)) {

    mod.factor <- paste(mod.factor, "\n",
                        paste("sec_order", "=~", paste(names(model), collapse = " + ")))

  }

  #...................
  ### Residual covariance ####
  if (isTRUE(!is.null(rescov))) {

    # Paste residual covariances
    mod.factor <- paste(mod.factor, "\n",
                        paste(vapply(rescov, function(y) paste(y, collapse = " ~~ "), FUN.VALUE = character(1L)), collapse = " \n "))

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model estimation ####

  mod.fit <- suppressWarnings(lavaan::cfa(mod.factor, data = x, ordered = ordered,
                                          parameterization = parameterization,
                                          cluster = if (isTRUE(is.null(cluster))) { NULL } else { ".cluster" },
                                          std.lv = std.lv, effect.coding = effect.coding,
                                          meanstructure = meanstructure,
                                          estimator = estimator, missing = missing))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convergence and model identification checks ####

  if (isTRUE(check)) {

    check.vcov <- check.theta <- check.cov.lv <- TRUE

    #...................
    ### Degrees of freedom ####

    if (isTRUE(lavaan::lavInspect(mod.fit, what = "fit")["df"] < 0L)) {

      stop("CFA model has negative degrees of freedom, model is not identified.", call. = FALSE)

    }

    #...................
    ### Model convergence ####

    if (isTRUE(!lavaan::lavInspect(mod.fit, what = "converged"))) {

      stop("CFA model did not converge.", call. = FALSE)

    }

    #...................
    ### Standard error ####

    if (isTRUE(any(is.na(unlist(lavaan::lavInspect(mod.fit, what = "se")))))) {

      stop("Standard errors could not be computed.", call. = FALSE)

    }

    #...................
    ### Variance-covariance matrix of the estimated parameters ####

    eigvals <- eigen(lavaan::lavInspect(mod.fit, what = "vcov"), symmetric = TRUE, only.values = TRUE)$values

    # If effect coding method is used, correct for equality constraints
    if (isTRUE(ident == "effect")) { eigvals <- rev(eigvals)[-seq_len(sum(lavaan::parameterTable(mod.fit)$op == "=="))] }

    if (isTRUE(min(eigvals) < .Machine$double.eps^(3L/4L))) {

      warning("The variance-covariance matrix of the estimated parameters is not positive definite. This may be a symptom that the model is not identified.", call. = FALSE)

      check.vcov <- FALSE

    }

    #...................
    ### Negative variance of observed variables ####

    if (isTRUE(any(diag(lavaan::lavInspect(mod.fit, what = "theta")) < 0L))) {

      warning("Some estimated variances of the observed variables are negative.")

      check.theta <- FALSE

    } else if (isTRUE(any(eigen(lavaan::lavTech(mod.fit, what = "theta")[[1L]], symmetric = TRUE, only.values = TRUE)$values < (-1L * .Machine$double.eps^(3/4))))) {

      warning("The model-implied variance-covariance matrix of the residuals of the observed variables is not positive definite.", call. = FALSE)

      check.theta <- FALSE

    }

    #...................
    ### Negative variance of latent variables ####

    if (isTRUE(any(diag(lavaan::lavTech(mod.fit, what = "cov.lv")[[1L]]) < 0L))) {

      warning("Some estimated variances of the latent variables are negative.", call. = FALSE)

      check.cov.lv <- FALSE

    # Model-implied variance-covariance matrix of the latent variables
    } else if (isTRUE(any(eigen(lavaan::lavTech(mod.fit, what = "cov.lv")[[1L]], symmetric = TRUE, only.values = TRUE)$values < (-1L * .Machine$double.eps^(3/4))))) {

      warning("The model-implied variance-covariance matrix of the latent variables is not positive definite.", call. = FALSE)

      check.cov.lv <- FALSE

    }

  } else {

    check.vcov <- check.theta <- check.cov.lv <- NULL

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model fit ####

  lav.fit <- lavaan::lavInspect(mod.fit, what = "fit")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Parameter estimates ####

  model.param <- data.frame(lavaan::parameterEstimates(mod.fit),
                            stdyx = lavaan::standardizedsolution(mod.fit)[, "est.std"])[, c("lhs", "op", "rhs", "est", "se", "z", "pvalue", "stdyx")]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Modification indices ####

  if (isTRUE(check.vcov && estimator != "PML")) {

    model.modind <- lavaan::modindices(mod.fit, minimum.value = min.value)

  } else {

    model.modind <- NULL

  }

  #_____________________________________________________________________________
  #
  # Return object --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## lavaan summary ####

  lavaan.summary <- data.frame(c(paste("lavaan", lavaan::lavInspect(mod.fit, what = "version")), "", "Estimator", "Standard errors", "Test statistic", "Missing data", "", "",
                                 "Number of observations", "Number of clusters", "", "Indicators", "Identification"),
                               c("",  "",
                                 # Estimator
                                 estimator,
                                 # Standard errors
                                 switch(lavaan::lavTech(mod.fit, what = "options")$se,
                                        "standard" = "Conventional",
                                        "robust.sem" = "Conventional Robust",
                                        "robust.huber.white" = "Huber-White",
                                        "robust.cluster" = "Cluster-Robust H-W",
                                        "robust.cluster.sem" = "Cluster-Robust Conven",
                                        "two.stage" = "Two-Stage",
                                        "robust.two.stage" = "Robust Two-Stage"),
                                 # Test statistic
                                 switch(lavaan::lavTech(mod.fit, what = "options")$test,
                                        "standard" = "Conventional",
                                        "satorra.bentler" = "Satorra-Bentler",
                                        "scaled.shifted" = "Scale-Shifted",
                                        "mean.var.adjusted" = "Satterthwaite",
                                        "yuan.bentler.mplus" = "Asymtotic Yuan-Bentler"),
                                 # Missing data
                                 ifelse(isTRUE(complete), "None",
                                        switch(missing,
                                        "listwise" = "Listwise Deletion",
                                        "pairwise" = "Pairwise Deletion",
                                        "fiml" = "FIML",
                                        "two.stage" = "Two-Stage",
                                        "robust.two.stage" = "Robust Two-Stage",
                                        "doubly.robust" = "Doubly-Robust")), "", "Used",
                                 # Number of observations
                                 lavaan::lavInspect(mod.fit, what = "nobs"),
                                 # Number of clusters
                                 ifelse(!is.null(cluster),
                                        length(unique(x[lavaan::lavInspect(mod.fit, "case.idx"), ".cluster"])), 1), "",
                                 # Variables
                                 ifelse(is.null(ordered), "Continuous",
                                                ifelse(isTRUE(ordered), "Ordered-Categorical",
                                                       ifelse(all(var %in% ordered), "Ordered", "Continous and Ordered"))),
                                 # Identification
                                 switch(ident,
                                        "marker" = "Marker Variable",
                                        "var" = "Factor Variance",
                                        "effect" = "Effects Coding")),
                               c(rep("", times = 7L),  "Total", lavaan::lavInspect(mod.fit, what = "norig"),rep("", times = 4L)),
                               fix.empty.names = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model fit ####

  model.fit <- data.frame(c("Number of Free Parameters", "",
                            "Chi-Square Test of Model Fit", "Test statistic", "Degrees of freedom", "P-value", "Scaling correction factor", "",
                            "Incremental Fit Indices", "CFI", "TLI", "",
                            "Absolute Fit Indices", "RMSEA", "90 Percent CI - lower", "90 Percent CI - upper", "P-value RMSEA <= 0.05", "", "SRMR", "",
                            "Information Criteria", "Akaike (AIC)", "Bayesian (BIC)", "Sample-size adjusted BIC"),
                          c(# Number of Free Parameters
                            lavaan::lavInspect(mod.fit, what = "npar"), NA, NA,
                            # Test statistic, df, and p-value
                            lav.fit["chisq"], lav.fit["df"], lav.fit["pvalue"], NA, NA, NA,
                            # CFI / TLI
                            lav.fit["cfi"], lav.fit["tli"], NA, NA,
                            # RMSEA
                            lav.fit["rmsea"], lav.fit["rmsea.ci.lower"], lav.fit["rmsea.ci.upper"], lav.fit["rmsea.pvalue"], NA,
                            # SRMR
                            ifelse(isTRUE(lavaan::lavInspect(mod.fit, what = "meanstructure")), lav.fit["srmr_bentler"], lav.fit["srmr_bentler_nomean"]), NA, NA,
                            # Information criteria
                            lav.fit["aic"], lav.fit["bic"], lav.fit["bic2"]),
                          c(lavaan::lavInspect(mod.fit, what = "npar"), NA, NA,
                            # Test statistic, df, p-value, and scaling correction factor
                            lav.fit["chisq.scaled"], lav.fit["df.scaled"], lav.fit["pvalue.scaled"], lav.fit["chisq.scaling.factor"], NA, NA,
                            # CFI / TLI
                            lav.fit["cfi.scaled"], lav.fit["tli.scaled"], NA, NA,
                            # RMSEA
                            ifelse(isTRUE(lav.fit["df"] == 0 && estimator == "PML"), NA, lav.fit["rmsea.scaled"]),
                            lav.fit["rmsea.ci.lower.scaled"], lav.fit["rmsea.ci.upper.scaled"], lav.fit["rmsea.pvalue.scaled"], NA,
                            # SRMR
                            ifelse(isTRUE(lavaan::lavInspect(mod.fit, what = "meanstructure")), lav.fit["srmr_bentler"], lav.fit["srmr_bentler_nomean"]), NA, NA,
                            # Information criteria
                            lav.fit["aic"], lav.fit["bic"], lav.fit["bic2"]),
                          c(rep(NA, times = 9L),
                            # CFI / TLI
                            ifelse(isTRUE(lav.fit["df"] == 0 && estimator %in% c("MLR", "WLSM", "ULSM")), lav.fit["cfi.scaled"], lav.fit["cfi.robust"]),
                            ifelse(isTRUE(lav.fit["df"] == 0 && estimator %in% c("MLR", "WLSM", "ULSM")), lav.fit["tli.scaled"], lav.fit["tli.robust"]), NA, NA,
                            # RMSEA
                            lav.fit["rmsea.robust"], lav.fit["rmsea.ci.lower.robust"], lav.fit["rmsea.ci.upper.robust"],
                            rep(NA, times = 8L)),
                          fix.empty.names = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Parameter estimates ####

  # Latent variables
  print.latent <- model.param[which(model.param$op == "=~"), ]

  # Latent variable covariances
  print.lv.cov <- model.param[which(model.param$op == "~~" & (model.param$lhs != model.param$rhs) & (model.param$lhs %in% print.latent$lhs) & (model.param$rhs %in% print.latent$lhs)), ]

  # Residual covariances
  print.res.cov <- model.param[which(model.param$op == "~~" & (model.param$lhs != model.param$rhs) & (!model.param$lhs %in% print.latent$lhs) & (!model.param$rhs %in% print.latent$lhs)), ]

  # Latent mean
  print.mean <- model.param[which(model.param$op == "~1" & model.param$lhs %in% print.latent$lhs), ]

  # Latent variance
  print.var <- model.param[which(model.param$op == "~~" & (model.param$lhs %in% print.latent$lhs) & (model.param$lhs == model.param$rhs)), ]

  # Intercepts
  print.interc <- model.param[which(model.param$op == "~1" & !model.param$lhs %in% print.latent$lhs), ]

  # Thresholds
  print.thres <- model.param[which(model.param$op == "|"), ]

  # Scales
  print.scale <- model.param[which(model.param$op == "~*~"), ]

  # Residual variance
  print.resid <- model.param[which(model.param$op == "~~" & (model.param$lhs == model.param$rhs) & (!model.param$lhs %in% print.latent$lhs) & (!model.param$rhs %in% print.latent$lhs)), ]

  # Model parameters
  model.param <- rbind(data.frame(param = "latent variable", print.latent),
                       if (nrow(print.lv.cov) > 0L) { data.frame(param = "latent variable covariance", print.lv.cov) } else { NULL },
                       if (nrow(print.res.cov) > 0L) { data.frame(param = "residual covariance", print.res.cov) } else { NULL },
                       if (nrow(print.mean) > 0L) { data.frame(param = "latent mean", print.mean) } else { NULL },
                       if (nrow(print.var) > 0L) { data.frame(param = "latent variance", print.var) } else { NULL },
                       if (nrow(print.interc) > 0L) { data.frame(param = "intercept", print.interc) } else { NULL },
                       if (nrow(print.thres) > 0L) { data.frame(param = "threshold", print.thres) } else { NULL },
                       if (nrow(print.scale) > 0L) { data.frame(param = "scale", print.scale) } else { NULL },
                       if (nrow(print.resid) > 0L) { data.frame(param = "residual variance", print.resid) } else { NULL })

  #...................
  ### Add labels ####

  # Latent mean, intercept, and threshold
  model.param[model.param$param %in% c("latent mean", "intercept"), "rhs"] <- model.param[model.param$param %in% c("latent mean", "intercept"), "lhs"]

  if (isTRUE(any(model.param$param == "threshold"))) {

    model.param[model.param$param == "threshold", "rhs"] <- apply(model.param[model.param$param == "threshold", c("lhs", "rhs")], 1L, paste, collapse = "|")

  }

  # Latent variables
  print.lv <- NULL
  for (i in unique(model.param[which(model.param$param == "latent variable"), "lhs"])) {

    print.lv <- rbind(print.lv,
                      data.frame(param = "latent variable", lhs = i, op = "", rhs = paste(i, "=~"), est = NA, se = NA, z = NA, pvalue = NA, stdyx = NA),
                      model.param[which(model.param$param == "latent variable" & model.param$lhs == i), ])

  }

  # Latent variable covariances
  print.lv.cov <- NULL
  for (i in unique(model.param[which(model.param$param == "latent variable covariance"), "lhs"])) {

    print.lv.cov <- rbind(print.lv.cov,
                          data.frame(param = "latent variable covariance", lhs = i, op = "", rhs = paste(i, "~~"), est = NA, se = NA, z = NA, pvalue = NA, stdyx = NA),
                          model.param[which(model.param$param == "latent variable covariance" & model.param$lhs == i), ])

  }

  # Residual covariances
  print.res.cov <- NULL
  for (i in unique(model.param[which(model.param$param == "residual covariance"), "lhs"])) {

    print.res.cov <- rbind(print.res.cov,
                           data.frame(param = "residual covariance", lhs = i, op = "", rhs = paste(i, "~~"), est = NA, se = NA, z = NA, pvalue = NA, stdyx = NA),
                           model.param[which(model.param$param == "residual covariance" & model.param$lhs == i), ])

  }

  model.param <- rbind(print.lv, print.lv.cov, print.res.cov,
                       model.param[which(!model.param$param %in% c("latent variable", "latent variable covariance", "residual covariance")), ])

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Modification indices ####

  if (isTRUE(check.vcov && estimator != "PML")) {

  model.modind <- misty::df.rename(model.modind[, c("lhs", "op", "rhs", "mi", "epc", "sepc.all")],
                                   from = "sepc.all", to = "stdyx.epc")
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Return object ####

  object <- list(call = match.call(),
                 type = "item.cfa",
                 data = x,
                 args = list(rescov = rescov, hierarch = hierarch,
                             meanstructure = meanstructure, ident = ident,
                             parameterization  = parameterization,
                             ordered = ordered, cluster = cluster,
                             estimator = estimator, missing = missing,
                             print = print, min.value = min.value,
                             digits = digits, p.digits = p.digits,
                             as.na = as.na, check = check,
                             output = output),
                 model = mod.factor,
                 model.fit = mod.fit,
                 check = list(vcov = check.vcov, theta = check.theta, cov.lv = check.cov.lv),
                 result = list(summary = lavaan.summary, coverage = coverage,
                               descript = itemstat, itemfreq = itemfreq,
                               fit = model.fit, param = model.param,
                               modind = model.modind))

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Write Results --------------------------------------------------------------

  if (isTRUE(!is.null(write))) { misty::write.result(object, file = write) }

  #_____________________________________________________________________________
  #
  # Putput ---------------------------------------------------------------------

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
