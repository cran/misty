#' Between-Group and Longitudinal Measurement Invariance Evaluation
#'
#' This function is a wrapper function for evaluating configural, metric, scalar,
#' and strict between-group or longitudinal (partial) measurement invariance using
#' confirmatory  factor analysis with continuous indicators by calling the \code{cfa}
#' function in the R package \pkg{lavaan}. By default, the function evaluates
#' configural, metric, and scalar measurement invariance by providing a table
#' with model fit information (i.e., chi-square test, fit indices based on a proper
#' null model, and information criteria) and model comparison (i.e., chi-square
#' difference test, change in fit indices, and change in information criteria).
#' Additionally, variance-covariance coverage of the data, descriptive statistics,
#' parameter estimates, modification indices, and residual correlation matrix can
#' be requested by specifying the argument \code{print}.
#'
#' @param ...          a matrix or data frame. If \code{model = NULL}, confirmatory
#'                     factor analysis based on a measurement model with one factor
#'                     labeled \code{f} comprising all variables in the matrix or
#'                     data frame specified in \code{x} for evaluating between-group
#'                     measurement invariance for the grouping variable specified
#'                     in the argument \code{group} is conducted. Longitudinal
#'                     measurement invariance evaluation can only be conducted by
#'                     specifying the model using the argument \code{model}. Note
#'                     that the cluster variable is excluded from \code{x} when
#'                     specifying \code{cluster}. If \code{model} is specified,
#'                     the matrix or data frame needs to contain all variables
#'                     used in the argument \code{model} and the cluster variable
#'                     when specifying the name of the cluster variable in the
#'                     argument \code{cluster}. Alternatively, an expression
#'                     indicating the variable names in \code{data} e.g.,
#'                     \code{item.invar(x1, x2, x2, data = dat)}. Note that the
#'                     operators \code{.}, \code{+}, \code{-}, \code{~}, \code{:},
#'                     \code{::}, and \code{!} can also be used to select variables,
#'                     see 'Details' in the \code{\link{df.subset}} function.
#' @param data         a data frame when specifying one or more variables in the
#'                     argument \code{...}. Note that the argument is \code{NULL}
#'                     when specifying a a matrix or data frame for the argument
#'                     \code{...}.
#' @param model        a character vector specifying a measurement model with one
#'                     factor, or a list of character vectors for specifying a
#'                     measurement model with more than one factor for evaluating
#'                     between-group measurement invariance when \code{long = FALSE}
#'                     or a list of character vectors for specifying a measurement
#'                     model with one factor for each time of measurement for
#'                     evaluating longitudinal measurement invariance when
#'                     specifying \code{long = TRUE}. For example,
#'                     \code{model = c("x1", "x2", "x3", "x4")} for specifying a
#'                     measurement model with one factor labeled \code{f} comprising
#'                     four indicators, or \code{model = list(factor1 = c("x1", "x2", "x3", "x4"),
#'                     factor2 = c("x5", "x6", "x7", "x8"))} for specifying a
#'                     measurement model with two latent factors labeled \code{factor1}
#'                     and \code{factor2} each comprising four indicators for
#'                     evaluating between-group measurement invariance, or
#'                     \code{model = list(time1 = c("ax1", "ax2", "ax3", "ax4"),
#'                     time2 = c("bx1", "bx2", "bx3", "bx4"),
#'                     time3 = c("cx1", "cx2", "cx3", "cx4"))} for specifying a
#'                     longitudinal measurement model with three time points comprising
#'                     four indicators at each time point. This function cannot
#'                     evaluate longitudinal measurement invariance for a measurement
#'                     model with more than one factor. Note that the name of each
#'                     list element is used to label factors, i.e., all list elements
#'                     need to be named, otherwise factors are labeled with \code{"f1", "f2", "f3"}
#'                     when \code{long = FALSE} and with \code{"t1", "t2", "t3"}
#'                     when \code{long = TRUE} and so on.
#' @param rescov       a character vector or a list of character vectors for specifying
#'                     residual covariancesindiator, e.g., \code{rescov = c("x1", "x2")}
#'                     for specifying a residual covariance between items \code{x1}
#'                     and \code{x2}, or \code{rescov = list(c("x1", "x2"), c("x3", "x4"))}
#'                     for specifying residual covariances between items \code{x1}
#'                     and \code{x2}, and items \code{x3} and \code{x4}.
#' @param rescov.long  logical: if \code{TRUE} (default), residual covariances between
#'                     parallel indicators are estimated across time when evaluating
#'                     longitudinal measurement invariance (\code{long = TRUE}),
#'                     i.e., residual variances of the same indicators that are
#'                     measured at different time points are correlated across all
#'                     possible time points. Note that residual covariances should
#'                     be estimated even if the parameter estimates are statistically
#'                     not significant since indicator-specific systematic variance
#'                     is likely to correlate with itself over time (Little, 2013,
#'                     p. 164).
#' @param group        either a character string indicating the variable name of
#'                     the grouping variable in the matrix or data frame specified
#'                     in \code{x} or a vector representing the groups
#'                     for conducting multiple-group analysis to evaluate between-group
#'                     measurement invariance.
#' @param long         logical: if \code{TRUE}, longitudinal measurement invariance
#'                     evaluation is conducted. The longitudinal measurement model
#'                     is specified by using the argument \code{model}. Note that
#'                     this function can only evaluate either between-group or
#'                     longitudinal measurement invariance, but not both at the
#'                     same time.
#' @param cluster      either a character string indicating the variable name
#'                     of the cluster variable in \code{...} or \code{data},
#'                     or a vector representing the nested grouping structure
#'                     (i.e., group or cluster variable) for computing
#'                     cluster-robust standard errors.
#' @param invar        a character string indicating the level of measurement
#'                     invariance to be evaluated, i.e., \code{config} to evaluate
#'                     configural measurement invariance (i.e., same factor structure
#'                     across groups or time), \code{metric} to evaluate configural
#'                     and metric measurement invariance (i.e., equal factor loadings
#'                     across groups or time), \code{scalar} (default) to evaluate
#'                     configural, metric and scalar measurement invariance (i.e.,
#'                     equal intercepts or thresholds across groups or time), and
#'                     \code{strict} to evaluate configural, metric, scalar, and
#'                     strict measurement invariance (i.e., equal residual variances
#'                     across groups or time).
#' @param partial      a character string or character vector containing the labels
#'                     of the parameters which should be free in all groups or across
#'                     time to specify a partial measurement invariance model. Note
#'                     that the labels of the parameters need to match the labels
#'                     shown in the output, i.e., \code{"L"} with a number for factor
#'                     loadings, \code{"T"} with a number for intercepts, and
#'                     \code{"E"} with a number for factor residual variances. The
#'                     number attached to the \code{"L"}, \code{"T"}, or \code{"E"}
#'                     label corresponds to the number of the indicator in the
#'                     measurement model (e.g., \code{"T3"} for the intercept of
#'                     the third indicator). When specifying the model using the
#'                     argument \code{model}, however, the number for the factor
#'                     loading is a combination of the number of the factor and
#'                     the number of the indicator (e.g., \code{"L23"} is the third
#'                     indicator of the second factor). Note that at least two
#'                     invariant indicators are needed for a partial measurement
#'                     invariance model. Otherwise there might be issues with model
#'                     non-identification.
#' @param ident        a character string indicating the method used for identifying
#'                     and scaling latent variables, i.e., \code{"marker"} for the
#'                     marker variable method fixing the first factor loading of
#'                     each latent variable to 1, \code{"var"} (default) for the
#'                     fixed variance method fixing the variance of each latent
#'                     variable to 1, or \code{"effect"} for the effects-coding
#'                     method using equality constraints so that the average of
#'                     the factor loading for each latent variable equals 1.
#' @param estimator    a character string indicating the estimator to be used
#'                     (see 'Details' in the help page of the \code{item.cfa()}
#'                     function). By default, \code{"MLR"} is used for CFA models
#'                     with continuous indicators.
#' @param missing      a character string indicating how to deal with missing data,
#'                     i.e., \code{"listwise"} for listwise deletion, \code{"pairwise"}
#'                     for pairwise deletion, \code{"fiml"} for full information
#'                     maximum likelihood method, \code{two.stage} for two-stage
#'                     maximum likelihood method, \code{robust.two.stage} for robust
#'                     two-stage maximum likelihood method, and \code{doubly-robust}
#'                     for doubly-robust method (see 'Details' in the help page
#'                     of the\code{item.cfa()} function). By default, \code{"fiml"}
#'                     is used for CFA models with continuous indicators which are
#'                     estimated by using \code{estimator = "MLR"}. However, argument
#'                     \code{missing} switches to \code{listwise} when the data
#'                     set is complete, i.e., it is not possible to use FIML in
#'                     complete data. Note that the robust CFI, TLI, and RMSEA
#'                     are different in complete data depending on whether FIML or
#'                     listwise deletion was specified when estimating the model
#'                     in lavaan.
#' @param null.model   logical: if \code{TRUE} (default), the proper null model
#'                     for computing incremental fit indices (i.e., CFI and TLI)
#'                     is used, i.e., means and variances of the indicators are
#'                     constrained to be equal across group or time in the null
#'                     model (Little, 2013, p. 112).
#' @param print        a character string or character vector indicating which results
#'                     to show on the console, i.e. \code{"all"} for all results,
#'                     \code{"summary"} for a summary of the specification of the
#'                     estimation method and missing data handling in lavaan,
#'                     \code{"coverage"} for the variance-covariance coverage of
#'                     the data, \code{"descript"} for descriptive statistics,
#'                     \code{"fit"} for model fit and model comparison, \code{"est"}
#'                     for parameter estimates, \code{"modind"} for modification
#'                     indices, and \code{"resid"} for the residual correlation
#'                     matrix and standardized residual means. By default, a summary
#'                     of the specification, model fit, and parameter estimates
#'                     are printed. Note that parameter estimates, modification
#'                     indices, and residual correlation matrix is only provided
#'                     for the model investigating the level of measurement
#'                     invariance specified in the argument \code{"invar"}.
#' @param print.fit    a character string or character vector indicating which
#'                     version of the CFI, TLI, and RMSEA to show on the console
#'                     when using a robust estimation method involving a scaling
#'                     correction factor, i.e., \code{"all"} for all versions of
#'                     the CFI, TLI, and RMSEA, \code{"standard"} (default when
#'                     \code{estimator} is one of \code{"ML", "MLF", "GLS", "WLS", "DWLS", "ULS", "PML"})
#'                     for fit indices without any non-normality correction,
#'                     \code{"scaled"} for population-corrected robust fit indices
#'                     with ad hoc non-normality correction, and \code{robust}
#'                     (default when \code{estimator} is one of \code{"MLM", "MLMV,
#'                     "MLMVS", "MLR", "WLSM", "WLSMV", "ULSM", "ULSMV", "DLS"})
#'                     for sample-corrected robust fit indices based on formula
#'                     provided by Li and Bentler (2006) and Brosseau-Liard and
#'                     Savalei (2014).
#' @param mod.minval   numeric value to filter modification indices and only show
#'                     modifications with a modification index value equal or higher
#'                     than this minimum value. By default, modification indices
#'                     equal or higher 6.63 are printed. Note that a modification
#'                     index value of 6.63 is equivalent to a significance level
#'                     of \eqn{\alpha = .01}.
#' @param resid.minval numeric value indicating the minimum absolute residual
#'                     correlation coefficients and standardized means to highlight
#'                     in boldface. By default, absolute residual correlation
#'                     coefficients and standardized means equal or higher 0.1
#'                     are highlighted. Note that highlighting can be disabled by
#'                     setting the minimum value to 1.
#' @param digits       an integer value indicating the number of decimal places
#'                     to be used for displaying results. Note that information
#'                     criteria and chi-square test statistic are printed with
#'                     \code{digits} minus 1 decimal places.
#' @param p.digits     an integer value indicating the number of decimal places
#'                     to be used for displaying \emph{p}-values, covariance coverage
#'                     (i.e., \code{p.digits - 1}), and residual correlation
#'                     coefficients.
#' @param as.na        a numeric vector indicating user-defined missing values, i.e.,
#'                     these values are converted to \code{NA} before conducting
#'                     the analysis. Note that \code{as.na()} function is only
#'                     applied to \code{x} but not to \code{group} or \code{cluster}.
#' @param write        a character string naming a file for writing the output into
#'                     either a text file with file extension \code{".txt"} (e.g.,
#'                     \code{"Output.txt"}) or Excel file with file extention
#'                     \code{".xlsx"}  (e.g., \code{"Output.xlsx"}). If the file
#'                     name does not contain any file extension, an Excel file will
#'                     be written.
#' @param append       logical: if \code{TRUE} (default), output will be appended
#'                     to an existing text file with extension \code{.txt} specified
#'                     in \code{write}, if \code{FALSE} existing text file will be
#'                     overwritten.
#' @param check        logical: if \code{TRUE} (default), argument specification
#'                     is checked and convergence and model identification checks
#'                     are conducted for all estimated models.
#' @param output       logical: if \code{TRUE} (default), output is shown.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{item.cfa}}, \code{\link{multilevel.invar}}, \code{\link{write.result}}
#'
#' @references
#' Brosseau-Liard, P. E., & Savalei, V. (2014) Adjusting incremental fit indices
#' for nonnormality. \emph{Multivariate Behavioral Research, 49}, 460-470.
#' https://doi.org/10.1080/00273171.2014.933697
#'
#' Li, L., & Bentler, P. M. (2006). Robust statistical tests for evaluating the
#' hypothesis of close fit of misspecified mean and covariance structural models.
#' \emph{UCLA Statistics Preprint #506}. University of California.
#'
#' Little, T. D. (2013). \emph{Longitudinal structural equation modeling}. Guilford
#' Press.
#'
#' Rosseel, Y. (2012). lavaan: An R Package for Structural Equation Modeling.
#' \emph{Journal of Statistical Software, 48}, 1-36. https://doi.org/10.18637/jss.v048.i02
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#'
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{data}}{data frame including all variables used in the analysis, i.e.,
#'                    indicators for the factor, grouping variable and cluster
#'                    variable}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{model}}{list with specified model for the configural, metric, scalar,
#'                     and strict invariance model}
#' \item{\code{model.fit}}{list with fitted lavaan object of the configural, metric,
#'                         scalar, and strict invariance model}
#' \item{\code{check}}{list with the results of the convergence and model identification
#'                     check for the configural, metric, scalar, and strict invariance
#'                     model}
#' \item{\code{result}}{list with result tables, i.e., \code{summary} for the
#'                      summary of the specification of the estimation method and
#'                      missing data handling in lavaan, \code{coverage} for the
#'                      variance-covariance coverage of the data, \code{descript}
#'                      for descriptive statistics, \code{fit} for a list with
#'                      model fit based on standard, scaled, and robust fit indices,
#'                      \code{est} for a list with parameter estimates for the
#'                      configural, metric, scalar, and strict invariance model,
#'                      \code{modind} for the list with modification indices for
#'                      the configural, metric, scalar, and strict invariance model,
#'                      \code{score} for the list with result of the score tests
#'                      for constrained parameters for the configural, metric,
#'                      scalar, and strict invariance model, and \code{resid} for
#'                      the list with residual correlation matrices and standardized
#'                      residual means for the configural, metric, scalar, and
#'                      strict invariance model}
#'
#' @note
#' The function uses the functions \code{cfa}, \code{fitmeasures} ,\code{lavInspect},
#' \code{lavTech}, \code{lavTestLRT}, \code{lavTestScore}, \code{modindices},
#' \code{parameterEstimates}, \code{parTable}, and \code{standardizedsolution}
#' provided in the R package \pkg{lavaan} by Yves Rosseel (2012).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load data set "HolzingerSwineford1939" in the lavaan package
#' data("HolzingerSwineford1939", package = "lavaan")
#'
#' #----------------------------------------------------------------------------
#' # Between-Group Measurement Invariance Evaluation
#'
#' #..................
#' # Measurement model with one factor
#'
#' # Example 1a: Specification of the grouping variable in 'x'
#' item.invar(HolzingerSwineford1939[, c("x1", "x2", "x3", "x4", "sex")], group = "sex")
#'
#' # Example 1b: Specification of the grouping variable in 'group'
#' item.invar(HolzingerSwineford1939[, c("x1", "x2", "x3", "x4")],
#'            group = HolzingerSwineford1939$sex)
#'
#' # Example 1c: Alternative specification using the 'data' argument
#' item.invar(x1:x4, data = HolzingerSwineford1939, group = "sex")
#'
#' # Example 1d: Alternative specification using the argument 'model'
#' item.invar(HolzingerSwineford1939, model = c("x1", "x2", "x3", "x4"), group = "sex")
#'
#' # Example 1e: Alternative specification using the 'data' and 'model' argument
#' item.invar(., data = HolzingerSwineford1939, model = c("x1", "x2", "x3", "x4"), group = "sex")
#'
#' #..................
#' # Measurement model with two factors
#'
#' item.invar(HolzingerSwineford1939,
#'            model = list(c("x1", "x2", "x3", "x4"),
#'                         c("x5", "x6", "x7", "x8")), group = "sex")
#'
#' #..................
#' # Configural, metric, scalar, and strict measurement invariance
#'
#' # Example 2: Evaluate configural, metric, scalar, and strict measurement invariance
#' item.invar(HolzingerSwineford1939, model = c("x1", "x2", "x3", "x4"),
#'            group = "sex", invar = "strict")
#'
#' #..................
#' # Partial measurement invariance
#'
#' # Example 3: Free second factor loading (L2) and third intercept (T3)
#' item.invar(HolzingerSwineford1939, model = c("x1", "x2", "x3", "x4"),
#'            group = "sex", partial = c("L2", "T3"), print = c("fit", "est"))
#'
#' #..................
#' # Residual covariances
#'
#' # Example 4a: One residual covariance
#' item.invar(HolzingerSwineford1939, model = c("x1", "x2", "x3", "x4"),
#'            rescov = c("x3", "x4"), group = "sex")
#'
#' # Example 4b: Two residual covariances
#' item.invar(HolzingerSwineford1939, model = c("x1", "x2", "x3", "x4"),
#'            rescov = list(c("x1", "x2"), c("x3", "x4")), group = "sex")
#'
#' #..................
#' # Scaled test statistic and cluster-robust standard errors
#'
#' # Example 5a: Specify cluster variable using a variable name in 'x'
#' item.invar(HolzingerSwineford1939, model = c("x1", "x2", "x3", "x4"),
#'            group = "sex", cluster = "agemo")
#'
#' # Example 5b: Specify vector of the cluster variable in the argument 'cluster'
#' item.invar(HolzingerSwineford1939, model = c("x1", "x2", "x3", "x4"),
#'            group = "sex", cluster = HolzingerSwineford1939$agemo)
#'
#' #..................
#' # Default Null model
#'
#' # Example 6: Specify default null model for computing incremental fit indices
#' item.invar(HolzingerSwineford1939, model = c("x1", "x2", "x3", "x4"),
#'            group = "sex", null.model = FALSE)
#'
#' #..................
#' # Print argument
#'
#' # Example 7a: Request all results
#' item.invar(HolzingerSwineford1939, model = c("x1", "x2", "x3", "x4"),
#'            group = "sex", print = "all")
#'
#' # Example 7b: Request fit indices with ad hoc non-normality correction
#' item.invar(HolzingerSwineford1939, model = c("x1", "x2", "x3", "x4"),
#'            group = "sex", print.fit = "scaled")
#'
#' # Example 7c: Request modification indices with value equal or higher than 10
#' # and highlight residual correlations equal or higher than 0.3
#' item.invar(HolzingerSwineford1939, model = c("x1", "x2", "x3", "x4"),
#'            group = "sex", print = c("modind", "resid"),
#'            mod.minval = 10, resid.minval = 0.3)
#'
#' #..................
#' # Model syntax and lavaan summary of the estimated model
#'
#' # Example 8
#' mod <- item.invar(HolzingerSwineford1939, model = c("x1", "x2", "x3", "x4"),
#'                   group = "sex", output = FALSE)
#'
#' # lavaan model syntax scalar invariance model
#' cat(mod$model$scalar)
#'
#' # lavaan summary of the scalar invariance model
#' lavaan::summary(mod$model.fit$scalar, standardized = TRUE, fit.measures = TRUE)
#'
#' #----------------------------------------------------------------------------
#' # Longitudinal Measurement Invariance Evaluation
#'
#' # Example 9: Two time points with three indicators at each time point
#' item.invar(HolzingerSwineford1939,
#'            model = list(c("x1", "x2", "x3"),
#'                         c("x5", "x6", "x7")), long = TRUE)
#'
#' #------------------------------------------------
#' # Write Results
#'
#' # Example 10a: Write Results into a text file
#' item.invar(HolzingerSwineford1939, model = c("x1", "x2", "x3", "x4"),
#'            group = "sex", print = "all", write = "Invariance.txt", output = FALSE)
#'
#' # Example 10b: Write Results into a Excel file
#' item.invar(HolzingerSwineford1939, model = c("x1", "x2", "x3", "x4"),
#'            group = "sex", print = "all", write = "Invariance.xlsx", output = FALSE)
#'
#' result <- item.invar(HolzingerSwineford1939, model = c("x1", "x2", "x3", "x4"),
#'                      group = "sex", print = "all", output = FALSE)
#' write.result(result, "Invariance.xlsx")
#' }
item.invar <- function(..., data = NULL, model = NULL, rescov = NULL, rescov.long = TRUE,
                       group = NULL, long = FALSE, cluster = NULL,
                       invar = c("config", "metric", "scalar", "strict"),
                       partial = NULL, ident = c("marker", "var", "effect"),
                       estimator = c("ML", "MLM", "MLMV", "MLMVS", "MLF", "MLR",
                                     "GLS", "WLS", "DWLS", "WLSM", "WLSMV",
                                     "ULS", "ULSM", "ULSMV", "DLS", "PML"),
                       missing = c("listwise", "pairwise", "fiml", "two.stage", "robust.two.stage", "doubly.robust"),
                       null.model = TRUE, print = c("all", "summary", "coverage", "descript", "fit", "est", "modind", "resid"),
                       print.fit = c("all", "standard", "scaled", "robust"), mod.minval = 6.63, resid.minval = 0.1,
                       digits = 3, p.digits = 3, as.na = NULL, write = NULL, append = TRUE, check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input '...' is missing
  if (isTRUE(missing(...))) { stop("Please specify the argument '...'.", call. = FALSE) }

  # Check if input '...' is NULL
  if (isTRUE(is.null(substitute(...)))) { stop("Input specified for the argument '...' is NULL.", call. = FALSE) }

  # Check if input 'data' is data frame
  if (isTRUE(!is.null(data) && !is.data.frame(data))) { stop("Please specify a data frame for the argument 'data'.", call. = FALSE) }

  # Check if input 'model' is a character vector or list of character vectors
  if (isTRUE(!is.null(model) && !all(sapply(model, is.character)))) { stop("Please specify a character vector or list of character vectors for the argument 'model'.", call. = FALSE) }

  # Check if 'group' or 'long' is specified
  if (isTRUE((is.null(group) && !long) || (!is.null(group) && long))) { stop("Please specify the argument 'group' to evaluate between-group measurement invariance or the argument 'long' to evaluate longitudinal measurement invariance.", call. = FALSE) }

  # Check if 'model' is specified when evaluating longitudinal measurement invariance
  if (isTRUE((long && is.null(model)) || (long && !is.list(model)))) { stop("Please specify a list of character vectors for the argument 'model' to evaluate longitudinal measurement invariance.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data using the argument 'data' ####

  if (isTRUE(!is.null(data))) {

    # Variable names
    var.names <- .var.names(..., data = data, group = group, cluster = cluster, check.chr = "a matrix or data frame")

    # Extract data
    x <- data[, var.names]

    # Grouping variable
    if (isTRUE(!is.null(group))) { group <- data[, group] }

    # Cluster variable
    if (isTRUE(!is.null(cluster))) { cluster <- data[, cluster] }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data without using the argument 'data' ####

  } else {

    # Extract data
    x <- eval(..., enclos = parent.frame())

    # Data and cluster
    var.group <- .var.group(data = x, group = group)

    if (isTRUE(!is.null(var.group$data)))  { x <- var.group$data }
    if (isTRUE(!is.null(var.group$group))) { group <- var.group$group }

  }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    ## R package lavaan ##
    if (isTRUE(!nzchar(system.file(package = "lavaan")))) { stop("Package \"lavaan\" is needed for this function, please install the package.", call. = FALSE) }

    ## Check input 'x' ##
    if (isTRUE(is.null(model))) {

      # No cluster or grouping variable in 'x'
      if (isTRUE((is.null(cluster) || length(cluster) != 1L) && (is.null(group) || length(group) != 1L))) {

        if (isTRUE(ncol(data.frame(x)) < 3L)) { stop("Please specify at least three indicators for the measurement model in 'x'.", call. = FALSE) }

      # Cluster or grouping variable in 'x'
      } else if (isTRUE( length(cluster) == 1L || length(group) == 1L)) {

        if (isTRUE(ncol(data.frame(x)[, !colnames(data.frame(x)) %in% c(cluster, group), drop = FALSE]) < 3L)) { stop("Please specify at least three indicators for the measurement model in 'x'.", call. = FALSE) }

      }

    ## Check input 'model' ##
    } else {

      # Character vector
      if (isTRUE(!is.list(model))) {

        if (isTRUE(length(unique(model)) < 3L)) { stop("Please specify at least three indicators for the measurement model.", call. = FALSE) }

      # List
      } else {

        # One-factor model
        if (isTRUE(length(model) == 1L && length(unique(unlist(model))) < 3L)) {

          stop("Please specify at least three indicators for the measurement model.", call. = FALSE)

        # Multiple-factor model
        } else if (isTRUE(any(sapply(model, function(y) length(y) < 3L)))) {

          stop("Please specify at least three indicators for each factor of the measurement model.", call. = FALSE)

        }

      }

    }

    ## Check input 'rescov' ##
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

    ## Check input 'rescov.long' ##
    if (isTRUE(!is.logical(rescov.long))) { stop("Please specify TRUE or FALSE for the argument 'rescov.long'.", call. = FALSE) }

    ## Check input 'group' ##
    if (isTRUE(!is.null(group))) {

      # Name of the grouping variable in 'x'
      if (isTRUE(length(group) == 1L)) {

        # One grouping variable
        if (isTRUE(!is.character(group))) { stop("Please specify a character string for the name of the grouping variable in 'x'", call. = FALSE) }

        # Grouping variable in 'x'
        if (isTRUE(!group %in% colnames(x))) { stop(paste0("Grouping variable \"", group, "\" specified in 'group' was not found in 'x'"), call. = FALSE) }

      # Group variable
      } else {

        # Length of grouping variable
        if (isTRUE(nrow(x) != length(group))) { stop("The grouping variable does not match with the number of rows in 'x'.",call. = FALSE) }

        # Number of groups
        if (isTRUE(length(unique(na.omit(group))) == 1L)) { stop("There is only one group represented in the grouping variable 'group'.", call. = FALSE) }

      }

    }

    ## Check input 'long' ##
    if (isTRUE(!is.logical(long))) { stop("Please specify TRUE or FALSE for the argument 'long'.", call. = FALSE) }

    if (isTRUE(long && !is.null(group))) { stop("Please specify the arguments for evaluating either between-group or longitudinal measurement invariance.", call. = FALSE) }

    ## Longitudinal measurement invariance ##
    if (isTRUE(long)) {

      # Model argument is NULL or not a list
      if (isTRUE(is.null(model) || !is.list(model))) { stop("Please specify the measurement model for evaluating longitudinal measurement invariance using the argument 'model'.", call. = FALSE) }

      # Not the same number of item across time
      if (isTRUE(length(unique(sapply(model, length))) != 1L)) { stop("Please specify the same number of items measured at each time point in the argument 'model'.", call. = FALSE) }

    }

    ## Check input 'cluster' ##
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

    ## Check input 'invar' ##
    if (isTRUE(!all(invar %in% c("config", "metric", "scalar", "strict")))) { stop("Character string in the argument 'invar' does not match with \"config\", \"metric\", \"scalar\", or \"scalar\".", call. = FALSE) }

    ## Check input 'partial' ##
    if (isTRUE(any(!sapply(partial, function(y) sapply(strsplit(y, split = ""), function(z) any(c("L", "T", "E") %in% z)))))) { warning("Character string(s) in the argument 'partial' need to match the labels of the parameters starting with \"L\", \"T\", or \"E\".", call. = FALSE) }

    ## Check input 'ident' ##
    if (isTRUE(!all(ident %in% c("marker", "var", "effect")))) { stop("Character string in the argument 'ident' does not match with \"marker\", \"var\", or \"effect\".", call. = FALSE) }

    ## Check input 'estimator' ##
    if (isTRUE(!all(estimator %in% c("ML", "MLM", "MLMV", "MLMVS", "MLF", "MLR", "GLS", "WLS", "DWLS", "WLSM", "WLSMV", "ULS", "ULSM", "ULSMV", "DLS", "PML")))) { stop("Character string in the argument 'estimator' does not match with \"ML\", \"MLM\", \"MLMV\", \"MLMVS\", \"MLF\", \"MLR\", \"GLS\", \"WLS\", \"DWLS\", \"WLSM\", \"WLSMV\", \"ULS\", \"ULSM\", \"ULSMV\", \"DLS\", or \"PML\".", call. = FALSE) }

    ## Check input 'missing' ##
    if (isTRUE(!all(missing %in% c("listwise", "pairwise", "fiml", "two.stage", "robust.two.stage", "doubly.robust")))) { stop("Character string in the argument 'missing' does not match with \"listwise\", \"pairwise\", \"fiml\", \"two.stage\", \"robust.two.stage\", or \"doubly.robust\".", call. = FALSE) }

    ## Check input 'null.model' ##
    if (isTRUE(!is.logical(long))) { stop("Please specify TRUE or FALSE for the argument 'null.model'.", call. = FALSE) }

    ## Check input 'print' ##
    if (isTRUE(!all(print %in% c("all", "summary", "coverage", "descript", "fit", "est", "modind", "resid")))) { stop("Character strings in the argument 'print' do not all match with \"summary\", \"coverage\", \"descript\", \"fit\", \"est\", \"modind\", or \"resid\".", call. = FALSE) }

    ## Check input 'print.fit' ##
    if (isTRUE(!all(print.fit %in% c("all", "standard", "scaled", "robust")))) { stop("Character strings in the argument 'print.fit' do not all match with \"standard\", \"scaled\", or \"robust\".", call. = FALSE) }

    ## Check input 'mod.minval' ##
    if (isTRUE(mod.minval < 0L)) { stop("Please specify a value greater than or equal 0 for the argument 'mod.minval'.", call. = FALSE) }

    ## Check input 'resid.minval' ##
    if (isTRUE(resid.minval < 0L)) { stop("Please specify a value greater than or equal 0 for the argument 'resid.minval'.", call. = FALSE) }

    ## Check input 'digits' ##
    if (isTRUE(digits %% 1L != 0L || digits < 0L || digits == 0L)) { stop("Specify a positive integer number for the argument 'digits'.", call. = FALSE) }

    # Check input 'p.digits'
    if (isTRUE(p.digits %% 1L != 0L || p.digits < 0L)) { stop("Specify a positive integer number for the argument 'p.digits'.", call. = FALSE) }

    # Check input 'append'
    if (isTRUE(!is.logical(append))) { stop("Please specify TRUE or FALSE for the argument 'append'.", call. = FALSE) }

    ## Check input 'output' ##
    if (isTRUE(!is.logical(output))) { stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Data and Arguments ---------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Manifest Variables ####

  #...................
  ### Model specification with 'x' ####
  if (isTRUE(is.null(model))) {

    #### No cluster variable
    if (isTRUE(is.null(cluster))) {

      # Grouping variable by variable name
      if (isTRUE(length(group) == 1L)) {

        var <- colnames(x)[!colnames(x) %in% group]

      # Grouping variable by vector
      } else {

        var <- colnames(x)

      }

    #### Cluster variable
    } else {

      if (isTRUE(length(cluster) == 1L)) {

        # Grouping variable by variable name
        if (isTRUE(length(group) == 1L)) {

          var <- colnames(x)[!colnames(x) %in% c(group, cluster)]

        # Grouping variable by vector
        } else {

          var <- colnames(x)[!colnames(x) %in% cluster]

        }

      }

    }

  #...................
  ### Model specification with 'model' ####
  } else {

    var <- unique(unlist(model))

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data Frame with Cluster and Grouping Variable ####

  #...................
  ### No cluster variable ####
  if (isTRUE(is.null(cluster))) {

    # Grouping variable by variable name
    if (isTRUE(!is.null(group) && length(group) == 1L)) {

      x <- data.frame(x[, var], .group = x[, group], stringsAsFactors = FALSE)

    # Grouping variable by vector
    } else if (isTRUE(!is.null(group) && length(group) > 1L)) {

      x <- data.frame(x[, var], .group = group, stringsAsFactors = FALSE)

    # No grouping variable
    } else if (isTRUE(is.null(group))) {

      x <- data.frame(x[, var], stringsAsFactors = FALSE)

    }

  #...................
  ### Cluster variable ####
  } else {

    # Cluster variable by variable name
    if (isTRUE(length(cluster) == 1L)) {

      # Grouping variable by variable name
      if (isTRUE(!is.null(group) && length(group) == 1L)) {

        x <- data.frame(x[, var], .group = x[, group], .cluster = x[, cluster], stringsAsFactors = FALSE)

      # Grouping variable by vector
      } else if (isTRUE(!is.null(group) && length(group) > 1L)) {

        x <- data.frame(x[, var], .group = group, .cluster = x[, cluster], stringsAsFactors = FALSE)

      # No grouping variable
      } else if (isTRUE(is.null(group))) {

        x <- data.frame(x[, var], .cluster = x[, cluster], stringsAsFactors = FALSE)

      }

    # Cluster variable by vector
    } else {

      # Grouping variable by variable name
      if (isTRUE(!is.null(group) && length(group) == 1L)) {

        x <- data.frame(x[, var], .group = x[, group], .cluster = cluster, stringsAsFactors = FALSE)

      # Grouping variable by vector
      } else if (isTRUE(!is.null(group) && length(group) > 1L)) {

        x <- data.frame(x[, var], .group = group, .cluster = cluster, stringsAsFactors = FALSE)

      # No grouping variable
      } else if (isTRUE(is.null(group))) {

        x <- data.frame(x[, var], .cluster = cluster, stringsAsFactors = FALSE)

      }

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Number of Groups ####

  if (isTRUE(!is.null(group))) { ngroups <- length(unique(na.omit(x$.group))) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert User-Missing Values into NA ####

  if (isTRUE(!is.null(as.na))) { x[, var] <- .as.na(x[, var], na = as.na) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Check variance within groups ##

  if (isTRUE(!long)) {

    check.var <- sapply(split(x, f = x$.group), function(y) apply(y[, var], 2L, var, na.rm = TRUE))

    if (isTRUE(any(check.var == 0 | is.na(check.var)))) {

      stop(paste0("There is no variance in group ", paste(names(which(apply(check.var, 2L, function(y) any(y == 0 | is.na(y))))), collapse = ", "), " for following variable: ", paste(names(which(apply(check.var, 1L, function(y) any(y == 0 | is.na(y))))), collapse = ", ")), call. = FALSE)

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model ####

  # Factor labels for evaluating between-group measurement invariance
  if (isTRUE(!is.null(model) && is.list(model) && (is.null(names(model)) || any(names(model) == "")))) {

    if (isTRUE(!long)) {

      names(model) <- paste0("f", seq_along(model))

    } else {

      names(model) <- paste0("t", seq_along(model))

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Residual Covariance ####

  if (isTRUE(!is.null(rescov) && !is.list(rescov))) { rescov <- list(rescov) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Level of invariance ####

  if (isTRUE(all(c("config", "metric", "scalar", "strict") %in% invar))) {

    invar <- "scalar"

  } else if (isTRUE(length(invar) != 1L)) {

    stop("Please specify a character string for the argument 'invar'.", call. = FALSE)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model identification ####

  # Default setting, fixed factor method
  if (isTRUE(all(c("marker", "var", "effect") %in% ident))) {

    ident <- "var"

  } else if (isTRUE(length(ident) != 1L)) {

    stop("Please specify a character string for the argument 'ident'.", call. = FALSE)

  }

  # Specify arguments 'std.lv' and 'effet.coding'
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
  ## Ordered ####

  # Ordered-categorical indicators
  ordered <- FALSE

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Estimator ####

  #...................
  ### Default setting ####
  if (isTRUE(all(c("ML", "MLM", "MLMV", "MLMVS", "MLF", "MLR", "GLS", "WLS", "DWLS", "WLSM", "WLSMV", "ULS", "ULSM", "ULSMV", "DLS", "PML") %in% estimator))) {

    # Continuous indicators
    if (isTRUE(!ordered)) {

      if (isTRUE(all(c("ML", "MLM", "MLMV", "MLMVS", "MLF", "MLR", "GLS", "WLS", "DWLS", "WLSM", "WLSMV", "ULS", "ULSM", "ULSMV", "DLS", "PML") %in% estimator ))) { estimator  <- "MLR" }

    # Categorical indicators
    } else {

      if (isTRUE(all(c("ML", "MLM", "MLMV", "MLMVS", "MLF", "MLR", "GLS", "WLS", "DWLS", "WLSM", "WLSMV", "ULS", "ULSM", "ULSMV", "DLS", "PML") %in% estimator))) { estimator  <- "WLSMV" }

      # Cluster-robust standard errors
      if (isTRUE(!is.null(cluster))) {

        stop("Cluster-robust standard errors are not available with ordered-categorical indicators.", call. = FALSE)

      }

    }

  #...................
  ### User-specified ####
  } else {

    # Continuous indicators
    if (isTRUE(!ordered)) {

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
                       ifelse(estimator %in% c("WLS", "DWLS", "WLSM", "WLSMV", "ULS", "ULSM", "PML"), "\"pairwise\"", "\"listwise\""), "."), call. = FALSE)

        missing <- ifelse(estimator %in% c("WLS", "DWLS", "WLSM", "WLSMV", "ULS", "ULSM", "PML"), "pairwise", "listwise")

      }

      # Doubly-robust
      if (isTRUE(missing == "doubly.robust" && estimator != "PML")) {

        warning(paste0("Doubly-robust method is not available for estimator = \"", estimator, "\", argument 'missing' switched to ",
                       ifelse(estimator %in% c("ML", "MLF", "MLR"), "fiml\"", ifelse(estimator %in% c("MLM", "MLMV", "MLMVS", "GLS", "WLS"), "\"listwise\"", "\"pairwise\"")), "."), call. = FALSE)

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

      warning(paste("Data set contains", sum(x.na.prop == 1L), "cases with missing on all variables which were not included in the analysis."), call. = FALSE)

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Print ####

  if (isTRUE(all(c("all", "summary", "coverage", "descript", "fit", "est", "modind", "resid") %in% print))) {

    print  <- c("summary", "fit")

  } else if (isTRUE(length(print) == 1L && "all" %in% print)) {

    print <- c("all", "summary", "coverage", "descript", "fit", "est", "modind", "resid")

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Print Fit ####

  if (isTRUE(all(c("all", "standard", "scaled", "robust") %in% print.fit))) {

    print.fit <- "robust"

  } else if (isTRUE(length(print.fit) == 1L && "all" %in% print.fit)) {

    if (isTRUE(estimator == "ML")) { print.fit <- "standard" } else { print.fit <- c("standard", "scaled", "robust") }

  }

  # Standard fit measures
  if (isTRUE(estimator %in% c("ML", "MLMVS", "MLF", "GLS", "WLS", "DWLS", "ULS", "PML"))) { print.fit <- "standard" }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Covariance Coverage ####

  coverage <- NULL

  if (isTRUE("coverage" %in% print)) {

    ### Grouping variable
    if (isTRUE(is.null(group))) {

      coverage <- suppressWarnings(misty::na.coverage(x[, var], output = FALSE)$result)

    ### No grouping variable
    } else {

      coverage <- suppressWarnings(lapply(split(x[, var], f = x[, ".group"]), function(y) misty::na.coverage(y, output = FALSE)$result))

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Sample Statistics ####

  itemstat <- itemfreq <- NULL

  if (isTRUE("descript" %in% print)) {

    ### No grouping variable
    if (isTRUE(is.null(group))) {

      # Descriptive statistics
      itemstat <- suppressWarnings(misty::descript(x[, var], output = FALSE)$result[, c("variable", "n", "nNA", "pNA", "m", "sd", "min", "max", "skew", "kurt")])

      # Frequency table
      itemfreq <-  suppressWarnings(misty::freq(x[, var], val.col = TRUE, exclude = 9999, output = FALSE)$result)

    ### Grouping variable
    } else {

      # Descriptive statistics
      itemstat <- suppressWarnings(misty::descript(x[, var], group = x[, ".group"], output = FALSE)$result[, c("group", "variable", "n", "nNA", "pNA", "m", "sd", "min", "max", "skew", "kurt")])

      # Frequency table
      itemfreq <- suppressWarnings(lapply(split(x[, var], f = x[, ".group"]), function(y) misty::freq(y, val.col = TRUE, exclude = 9999, output = FALSE)$result))

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model Syntax ####

  #...................
  ### Latent variable ####

  #### Between-group measurement invariance ####
  if (isTRUE(!long)) {

    ##### Model specification with 'x'
    if (isTRUE(is.null(model) || !is.list(model))) {

      # Configural
      mod.factor.config <- paste("f =~", paste(var, collapse = " + "))

      # Metric
      mod.factor.metric <- paste("f =~", paste0("L", seq_along(var), "*", var, collapse = " + "))

    ##### Model specification with 'model'
    } else if (isTRUE(!is.null(model) && is.list(model))) {

       # Configural
       mod.factor.config <- paste(sapply(seq_along(model), function(y) paste(names(model)[[y]], "=~", paste0(model[[y]], collapse = " + "))), collapse = " \n")

       # Metric
       mod.factor.metric <- paste(sapply(seq_along(model), function(y) paste(names(model)[[y]], "=~", paste0("L", y, seq_along(model[[y]]), "*", model[[y]], collapse = " + "))), collapse = " \n")

    }

    #...................
    ### Intercept ####

    # Scalar
    mod.inter.scalar <- paste0(var, " ~ T", seq_along(var), "*1", collapse = " \n")

    #...................
    ### Residual Variance ####

    # Strict
    mod.resid.strict <- paste0(var, " ~~ E", seq_along(var), "*", var, collapse = " \n")

  #### Longitudinal measurement invariance ####
  } else {

    #...................
    ### Latent variable ####

    # Configural
    mod.factor.config <- paste(sapply(seq_along(model), function(y) paste(names(model)[[y]], "=~", paste0(model[[y]], collapse = " + "))), collapse = " \n")

    # Metric
    mod.factor.metric <- paste(sapply(seq_along(model), function(y) paste(names(model)[[y]], "=~", paste0("L", seq_along(model[[y]]), "*",  model[[y]], collapse = " + "))), collapse = " \n")

    #...................
    ### Intercept ####

    # Scalar
    mod.inter.scalar <- paste(sapply(seq_along(model), function(y) paste(model[[y]], "~", paste0("T", seq_along(model[[y]]), "*1"))), collapse = " \n")

    #...................
    ### Residual Variance ####

    # Strict
    mod.resid.strict <- paste(sapply(seq_along(model), function(y) paste(model[[y]], "~~", paste0("E", seq_along(model[[y]]), "*", model[[y]]))), collapse = " \n")

  }

  #...................
  ### Residual covariances ####
  if (isTRUE(!is.null(rescov))) {

    # Paste residual covariances
    mod.rescov <- paste(vapply(rescov, function(y) paste(y, collapse = " ~~ "), FUN.VALUE = character(1L)), collapse = " \n ")

  }

  #...................
  ### Residual covariances between parallel indicators ####

  mod.rescov.long <- NULL

  if (isTRUE(long && rescov.long)) {

    # Paste longitudinal residual covariances
    mod.rescov.long <- paste(apply(matrix(unlist(model), nrow = length(model), byrow = TRUE), 2L, function(y) apply(combn(y, m = 2L), 2L, function(z) paste0(z[1L], " ~~ ", z[2L]))), collapse = "\n")

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model Specification ####

  mod.metric <- mod.scalar <- mod.strict <- NULL

  #...................
  ### Between-group measurement invariance ####
  if (isTRUE(!long)) {

    #### Configural measurement invariance ####
    mod.config <- mod.factor.config

    ##### Marker variable method
    if (isTRUE(ident == "marker")) {

      # One factor model
      if (isTRUE(is.null(model) || !is.list(model) || (is.list(model) && length(model) == 1L))) {

        # Fix intercept of first indicator at 0 and estimate latent mean
        mod.config <- paste0(mod.config, "\n",
                             paste0(misty::chr.trim(unlist(strsplit(mod.config, "=~"))[[1L]]), " ~ NA*1"), "\n",
                             paste0(var[1L], " ~ 0*1"))

      # Multiple factor model
      } else {

        # Fix intercept of first indicators at 0 and estimate latent means
        mod.config <- paste0(mod.config, "\n",
                             paste0(c(sapply(names(model), function(y) paste0(y, " ~ NA*1")),
                                      sapply(model, function(y) paste0(y[1L], " ~ 0*1"))), collapse = "\n"))

      }

    }

    #### Metric measurement invariance ####
    if (isTRUE(any(c("metric", "scalar", "strict") %in% invar))) {

      mod.metric <- mod.factor.metric

      ##### Marker variable method
      switch (ident, marker = {

        # One factor model
        if (isTRUE(is.null(model) || !is.list(model) || (is.list(model) && length(model) == 1L))) {

          # Fix intercept of first indicator at 0 and estimate latent mean
          mod.metric <- paste0(mod.metric, "\n",
                               paste0(misty::chr.trim(unlist(strsplit(mod.metric, "=~"))[[1L]]), " ~ NA*1"), "\n",
                               paste0(var[1L], " ~ 0*1"))

        # Multiple factor model
        } else {

          # Fix intercept of first indicators at 0 and estimate latent means
          mod.metric <- paste0(mod.metric, "\n",
                               paste0(c(sapply(names(model), function(y) paste0(y, " ~ NA*1")),
                                        sapply(model, function(y) paste0(y[1L], " ~ 0*1"))), collapse = "\n"))

        }

      ##### Fixed factor method
      }, var = {

        # One factor model
        if (isTRUE(is.null(model) || !is.list(model) || (is.list(model) && length(model) == 1L))) {

          factor <- misty::chr.trim(unlist(strsplit(mod.metric, "=~"))[[1L]])

          # Fix factor variance in the first group at 1, but estimate parameter in all other groups
          mod.metric <- paste0(mod.metric, "\n",
                               paste0(factor, " ~~ c(1, ", paste(rep(NA, times = length(unique(na.omit(x$.group))) - 1L), collapse = ", ") ,  ")", "*", factor))

        # Multiple factor model
        } else {

          # Fix factor variance in the first group at 1, but estimate parameter in all other groups
          mod.metric <- paste0(mod.metric, "\n",
                               paste(sapply(names(model), function(y) paste0(y, " ~~ c(1, ", paste(rep(NA, times = length(unique(na.omit(x$.group))) - 1L), collapse = ", ") ,  ")", "*", y)), collapse = " \n"))

        }

      })

      ##### Free loadings for a partial measurement invariance model
      if (isTRUE(!is.null(partial) && any(grepl("L", partial)))) { mod.metric <- misty::chr.gsub(paste0(partial[grepl("L", partial)], "*"), "", mod.metric, fixed = TRUE, recycle = TRUE) }

    }

    #### Scalar measurement invariance ####
    if (isTRUE(any(c("scalar", "strict") %in% invar))) {

      ##### Marker variable method
      switch (ident, marker = {

        # One factor model
        if (isTRUE(is.null(model) || !is.list(model) || (is.list(model) && length(model) == 1L))) {

          # Constrain intercepts to be equal across groups (except the first intercept)
          mod.scalar <- paste0(mod.metric, "\n",
                               paste0(unlist(strsplit(mod.inter.scalar, "\n"))[-1L], collapse = "\n"))

        } else {

          # Constrain intercepts to be equal across groups (except the first intercept)
          mod.scalar <- paste0(mod.metric, "\n",
                               paste(unlist(strsplit(mod.inter.scalar, "\n"))[-sapply(model, function(y) grep(y[1L], unlist(strsplit(mod.inter.scalar, "\n"))))], collapse = "\n"))

        }

      ##### Fixed factor method
      }, var = {

        # One factor model
        if (isTRUE(is.null(model) || !is.list(model) || (is.list(model) && length(model) == 1L))) {

          # Constrain intercepts to be equal across groups and estimate latent means (except the first group)
          mod.scalar <- paste0(mod.metric, "\n",
                               paste0(factor, " ~ c(0, ", paste(rep(NA, times = length(unique(na.omit(x$.group))) - 1L), collapse = ", ") ,  ")", "*", "1"), "\n",
                               mod.inter.scalar)

        } else {

          # Constrain intercepts to be equal across groups and estimate latent means (except the first group)
          mod.scalar <- paste0(mod.metric, "\n",
                               paste(sapply(names(model), function(y) paste0(y, " ~ c(0, ", paste(rep(NA, times = length(unique(na.omit(x$.group))) - 1L), collapse = ", ") ,  ")", "*1")), collapse = " \n"), "\n",
                               mod.inter.scalar)

        }

      ##### Effects coding method
      }, effect = {

        mod.scalar <- paste0(mod.metric, "\n", mod.inter.scalar)

      })

      ##### Free intercepts for a partial measurement invariance model
      if (isTRUE(!is.null(partial) && any(grepl("T", partial)))) { mod.scalar <- misty::chr.gsub(paste0(partial[grepl("T", partial)], "*"), "", mod.scalar, fixed = TRUE, recycle = TRUE) }

    }

    #### Strict measurement invariance ####
    if (isTRUE("strict" %in% invar)) {

      mod.strict <- paste0(mod.scalar, "\n", mod.resid.strict)

      ##### Free residuals for a partial measurement invariance model
      if (isTRUE(!is.null(partial) && any(grepl("E", partial)))) { mod.strict <- misty::chr.gsub(paste0(partial[grepl("E", partial)], "*"), "", mod.strict, fixed = TRUE, recycle = TRUE) }

    }

  #...................
  ### Longitudinal measurement invariance ####
  } else {

    #### Configural measurement invariance ####
    mod.config <- mod.factor.config

    ##### Marker variable method
    if (isTRUE(ident == "metric")) {

      # Fix intercept of first indicators at 0 and estimate latent means
      mod.config <- paste0(mod.config, "\n",
                           paste0(c(sapply(names(model), function(y) paste0(y, " ~ NA*1")),
                                    sapply(model, function(y) paste0(y[1L], " ~ 0*1"))), collapse = "\n"))

    }

    #### Metric measurement invariance ####
    if (isTRUE(any(c("metric", "scalar", "strict") %in% invar))) {

      mod.metric <- mod.factor.metric

      ##### Marker variable method
      switch (ident, marker = {

        # Fix intercept of first indicators at 0 and estimate latent means
        mod.metric <- paste0(mod.metric, "\n",
                               paste0(c(sapply(names(model), function(y) paste0(y, " ~ NA*1")),
                                        sapply(model, function(y) paste0(y[1L], " ~ 0*1"))), collapse = "\n"))

      ##### Fixed factor method
      }, var = {

        # Fix factor variance in the first group at 1, but estimate parameter in all other groups
        mod.metric <- paste0(mod.metric, "\n",
                             paste0(sapply(names(model)[-1L], function(y) paste0(y, " ~~ NA*", y)), collapse = " \n"))

      })

      ##### Free loadings for a partial measurement invariance model
      if (isTRUE(!is.null(partial) && any(grepl("L", partial)))) { mod.metric <- misty::chr.gsub(paste0(partial[grepl("L", partial)], "*"), "", mod.metric, fixed = TRUE, recycle = TRUE) }

    }

    #### Scalar measurement invariance ####
    if (isTRUE(any(c("scalar", "strict") %in% invar))) {

      ##### Marker variable method
      switch (ident, marker = {

        # Constrain intercepts to be equal across groups (except the first intercept)
        mod.scalar <- paste0(mod.metric, "\n",
                             paste(unlist(strsplit(mod.inter.scalar, "\n"))[-sapply(model, function(y) grep(y[1L], unlist(strsplit(mod.inter.scalar, "\n"))))], collapse = "\n"))

      ##### Fixed factor method
      }, var = {

        # Constrain intercepts to be equal across groups and estimate latent means (except the first group)
        mod.scalar <- paste0(mod.metric, "\n",
                             paste0(sapply(names(model)[-1L], function(y) paste0(y, " ~ NA*1")), collapse = " \n"), "\n",
                             mod.inter.scalar)

      ##### Effects coding method
      }, effect = {

        mod.scalar <- paste0(mod.metric, "\n", mod.inter.scalar)

      })

      ##### Free intercepts for a partial measurement invariance model
      if (isTRUE(!is.null(partial) && any(grepl("T", partial)))) { mod.scalar <- misty::chr.gsub(paste0(partial[grepl("T", partial)], "*"), "", mod.scalar, fixed = TRUE, recycle = TRUE) }

    }

    #### Strict measurement invariance ####
    if (isTRUE("strict" %in% invar)) {

      mod.strict <- paste0(mod.scalar, "\n", mod.resid.strict)

    }

    ##### Free residual variances for a partial measurement invariance model
    if (isTRUE(!is.null(partial) && any(grepl("E", partial)))) { mod.strict <- misty::chr.gsub(paste0(partial[grepl("E", partial)], "*"), "", mod.strict, fixed = TRUE, recycle = TRUE) }

  }

  #...................
  ### Residual covariances ####

  if (isTRUE(!is.null(rescov))) {

    mod.config <- paste0(mod.config, "\n", mod.rescov)

    if (isTRUE(any(c("metric", "scalar", "strict") %in% invar))) { mod.metric <- paste0(mod.metric, "\n", mod.rescov) }

    if (isTRUE(any(c("scalar", "strict") %in% invar))) { mod.scalar <- paste0(mod.scalar, "\n", mod.rescov) }

    if (isTRUE("strict" %in% invar)) { mod.strict <- paste0(mod.strict, "\n", mod.rescov) }

  }

  #...................
  ### Residual covariances between parallel indicators ####

  if (isTRUE(long)) {

    mod.config <- paste0(mod.config, "\n", mod.rescov.long)

    if (isTRUE(any(c("metric", "scalar", "strict") %in% invar))) { mod.metric <- paste0(mod.metric, "\n", mod.rescov.long) }

    if (isTRUE(any(c("scalar", "strict") %in% invar))) { mod.scalar <- paste0(mod.scalar, "\n", mod.rescov.long) }

    if (isTRUE("strict" %in% invar)) { mod.strict <- paste0(mod.strict, "\n", mod.rescov.long) }

  }

  #...................
  ### Proper Null model ####

  if (isTRUE(null.model)) {

    #...................
    ### Between-group measurement invariance ####
    if (isTRUE(!long)) {

      mod.null <- paste0(# Covariances
                       c(apply(combn(var, m = 2L), 2L, function(y) paste0(y[1L], " ~~ 0*", y[2L])),
                         # Intercepts
                         sapply(seq_along(var), function(y) paste0(var[y], " ~ T", y, "*1")),
                         # Variance
                         sapply(seq_along(var), function(y) paste0(var[y], " ~~ V", y, "*", var[y]))), collapse = "\n")

    #...................
    ### Longitudinal measurement invariance ####
    } else {

      mod.null <- paste0(# Covariances
                       c(apply(combn(var, m = 2L), 2L, function(y) paste0(y[1L], " ~~ 0*", y[2L])),
                         # Intercepts
                         sapply(seq_along(model), function(y) paste0(model[[y]], " ~ T", seq_along(model[[y]]),  "*1")),
                         # Variance
                         sapply(seq_along(model), function(y) paste0(model[[y]], " ~~ V", seq_along(model[[y]]),  "*", model[[y]]))), collapse = "\n")

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Function for Convergence and Model Identification Checks ####

  conv.ident <- function(model.fit, invar) {

    check.vcov <- check.theta <- check.cov.lv <- TRUE

    switch(invar, config = {

      invar.upp <- "Configural"
      invar.low <- "configural"

    }, metric = {

      invar.upp <- "Metric"
      invar.low <- "metric"

    }, scalar =  {

      invar.upp <- "Scalar"
      invar.low <- "scalar"

    }, strict = {

      invar.upp <- "Strict"
      invar.low <- "strict"

    })

    #...................
    ### Model convergence ####

    #### Model not converged
    if (isTRUE(!lavaan::lavInspect(model.fit, what = "converged"))) {

      stop(paste0(invar.upp, " invariance model did not converge."), call. = FALSE)

    #### Model converged
    } else {

      #...................
      ### Degrees of freedom ####

      if (isTRUE(suppressWarnings(lavaan::lavInspect(model.fit, what = "fit")["df"] < 0L))) { stop(paste0(invar.upp, " invariance model has negative degrees of freedom, model is not identified."), call. = FALSE) }

      #...................
      ### Standard error ####

      if (isTRUE(any(is.na(unlist(lavaan::lavInspect(model.fit, what = "se")))))) { stop(paste0("Standard errors of the ", invar.low, " invariance model could not be computed."), call. = FALSE) }

      #...................
      ### Variance-covariance matrix of the estimated parameters ####

      eigvals <- eigen(lavaan::lavInspect(model.fit, what = "vcov"), symmetric = TRUE, only.values = TRUE)$values

      # Correct for equality constraints
      if (isTRUE(any(lavaan::parTable(model.fit)$op == "=="))) { eigvals <- rev(eigvals)[-seq_len(sum(lavaan::parTable(model.fit)$op == "=="))] }

      if (isTRUE(min(eigvals) < .Machine$double.eps^(3L/4L))) {

        warning(paste0("The variance-covariance matrix of the estimated parameters in the ", invar.low, " invariance model is not positive definite."), call. = FALSE)

        check.vcov <- FALSE

      }

      #...................
      ### Negative variance of observed variables ####

      if (isTRUE(!long)) {

        cond.theta1 <- any(sapply(lavaan::lavInspect(model.fit, what = "theta"), diag) < 0)
        cond.theta2 <- any(sapply(lavaan::lavTech(model.fit, what = "theta"), function(y) eigen(y, symmetric = TRUE, only.values = TRUE)$values) < (-1L * .Machine$double.eps^(3L/4L)))

      } else {

        cond.theta1 <- any(diag(lavaan::lavInspect(model.fit, what = "theta")) < 0L)
        cond.theta2 <- any(eigen(lavaan::lavTech(model.fit, what = "theta")[[1L]], symmetric = TRUE, only.values = TRUE)$values < (-1L * .Machine$double.eps^(3L/4L)))

      }

      if (isTRUE(cond.theta1)) {

        warning(paste0("Some estimated variances of the observed variables in the ", invar.low, " invariance model are negative."), call. = FALSE)

        check.theta <- FALSE

      } else if (isTRUE(cond.theta2)) {

        warning(paste0("The model-implied variance-covariance matrix of the residuals of the observed variables in the ", invar.low, " invariance model is not positive definite."), call. = FALSE)

        check.theta <- FALSE

      }

      #...................
      ### Negative variance of latent variables ####

      if (isTRUE(!long)) {

        cond.theta1 <- any(sapply(lavaan::lavTech(model.fit, what = "cov.lv"), diag) < 0L)
        cond.theta1 <- any(sapply(lavaan::lavTech(model.fit, what = "cov.lv"), function(y) eigen(y, symmetric = TRUE, only.values = TRUE)$values) < (-1L * .Machine$double.eps^(3L/4L)))

      } else {

        cond.theta1 <- any(diag(lavaan::lavTech(model.fit, what = "cov.lv")[[1L]]) < 0L)
        cond.theta2 <- any(eigen(lavaan::lavTech(model.fit, what = "cov.lv")[[1L]], symmetric = TRUE, only.values = TRUE)$values < (-1L * .Machine$double.eps^(3L/4L)))

      }

      if (isTRUE(cond.theta1)) {

        warning(paste0("Some estimated variances of the latent variables in the ", invar.low, " invariance model are negative."), call. = FALSE)

        check.cov.lv <- FALSE

      # Model-implied variance-covariance matrix of the latent variables
      } else if (isTRUE(cond.theta2)) {

        warning("The model-implied variance-covariance matrix of the latent variables in the ", invar.low, " invariance model is not positive definite.", call. = FALSE)

        check.cov.lv <- FALSE

      }

    }

    # Return object
    return(c(check.vcov = check.vcov, check.theta = check.theta, check.cov.lv = check.cov.lv))

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model Estimation ####

  mod.config.fit <- mod.metric.fit <- mod.scalar.fit <- mod.strict.fit <- mod.config.fit.check <- mod.metric.fit.check <- mod.scalar.fit.check <- mod.strict.fit.check <- NULL

  #...................
  ### Configural measurement invariance

  mod.config.fit <- suppressWarnings(lavaan::cfa(mod.config, data = x, ordered = ordered, meanstructure = TRUE,
                                                 group = if (isTRUE(long)) { NULL } else { ".group" },
                                                 cluster = if (isTRUE(is.null(cluster))) { NULL } else { ".cluster" },
                                                 std.lv = std.lv, effect.coding = effect.coding,
                                                 estimator = estimator, missing = missing))

  #### Convergence and model identification checks ####
  if (isTRUE(check)) { mod.config.fit.check <- conv.ident(mod.config.fit, invar = "config") }

  #...................
  ### Metric measurement invariance
  if (isTRUE(any(c("metric", "scalar", "strict") %in% invar))) {

    mod.metric.fit <- suppressWarnings(lavaan::cfa(mod.metric, data = x, ordered = ordered, meanstructure = TRUE,
                                                   group = if (isTRUE(long)) { NULL } else { ".group" },
                                                   cluster = if (isTRUE(is.null(cluster))) { NULL } else { ".cluster" },
                                                   std.lv = std.lv, effect.coding = effect.coding,
                                                   estimator = estimator, missing = missing))

    #### Convergence and model identification checks ####
    if (isTRUE(check)) { mod.metric.fit.check <- conv.ident(mod.metric.fit, invar = "metric") }

  }

  #...................
  ### Scalar measurement invariance
  if (isTRUE(any(c("scalar", "strict") %in% invar))) {

    mod.scalar.fit <- suppressWarnings(lavaan::cfa(mod.scalar, data = x, ordered = ordered, meanstructure = TRUE,
                                                   group = if (isTRUE(long)) { NULL } else { ".group" },
                                                   cluster = if (isTRUE(is.null(cluster))) { NULL } else { ".cluster" },
                                                   std.lv = std.lv, effect.coding = effect.coding,
                                                   estimator = estimator, missing = missing))

    #### Convergence and model identification checks ####
    if (isTRUE(check)) { mod.scalar.fit.check <- conv.ident(mod.scalar.fit, invar = "scalar") }

  }

  #...................
  ### Strict measurement invariance
  if (isTRUE("strict" %in% invar)) {

    mod.strict.fit <- suppressWarnings(lavaan::cfa(mod.strict, data = x, ordered = ordered, meanstructure = TRUE,
                                                   group = if (isTRUE(long)) { NULL } else { ".group" },
                                                   cluster = if (isTRUE(is.null(cluster))) { NULL } else { ".cluster" },
                                                   std.lv = std.lv, effect.coding = effect.coding,
                                                   estimator = estimator, missing = missing))

    #### Convergence and model identification checks ####
    if (isTRUE(check)) { mod.strict.fit.check <- conv.ident(mod.strict.fit, invar = "strict") }

  }

  #...................
  ### Proper Null model ####

  if (isTRUE(null.model)) {

    mod.null.fit <- suppressWarnings(lavaan::cfa(mod.null, data = x, ordered = ordered, meanstructure = TRUE,
                                                 group = if (isTRUE(long)) { NULL } else { ".group" },
                                                 cluster = if (isTRUE(is.null(cluster))) { NULL } else { ".cluster" },
                                                 std.lv = std.lv, effect.coding = effect.coding,
                                                 estimator = estimator, missing = missing))

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Chi-Squared Difference Test ####

  #...................
  ### Configural vs. metric measurement invariance
  if (isTRUE(any(c("metric", "scalar", "strict") %in% invar))) {

    chidiff.conf.met <- tryCatch(lavaan::lavTestLRT(mod.config.fit, mod.metric.fit),
                                 error = function(y) { data.frame(matrix(NA, ncol = 7L, dimnames = list(NULL, c("Df", "AIC", "BIC", "Chisq", "Chisq diff", "Df diff", "Pr(>Chisq)"))), check.names = FALSE) },
                                 warning = function(z) { suppressWarnings(lavaan::lavTestLRT(mod.config.fit, mod.metric.fit, method = "satorra.bentler.2010")) })

  }

  #...................
  ### Metric vs. scalar measurement invariance
  if (isTRUE(any(c("scalar", "strict") %in% invar))) {

    chidiff.met.sca <- tryCatch(lavaan::lavTestLRT(mod.metric.fit, mod.scalar.fit),
                                error = function(y) { data.frame(matrix(NA, ncol = 7L, dimnames = list(NULL, c("Df", "AIC", "BIC", "Chisq", "Chisq diff", "Df diff", "Pr(>Chisq)"))), check.names = FALSE) },
                                warning = function(z) { suppressWarnings(lavaan::lavTestLRT(mod.metric.fit, mod.scalar.fit, method = "satorra.bentler.2010")) })

  }

  #...................
  ### Strong vs. strict measurement invariance
  if (isTRUE(invar == "strict")) {

    chidiff.sca.str <- tryCatch(lavaan::lavTestLRT(mod.scalar.fit, mod.strict.fit),
                                error = function(y) { data.frame(matrix(NA, ncol = 7L, dimnames = list(NULL, c("Df", "AIC", "BIC", "Chisq", "Chisq diff", "Df diff", "Pr(>Chisq)"))), check.names = FALSE) },
                                warning = function(z) { suppressWarnings(lavaan::lavTestLRT(mod.scalar.fit, mod.strict.fit, method = "satorra.bentler.2010")) })

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model Fit ####

  #### Configural invariance model
  lav.fit.config <- suppressWarnings(lavaan::fitmeasures(mod.config.fit, baseline.model = if (isTRUE(null.model)) { mod.null.fit } else { NULL }))

  #### Metric invariance model
  if (isTRUE(any(c("metric", "scalar", "strict") %in% invar))) { lav.fit.metric <- suppressWarnings(lavaan::fitmeasures(mod.metric.fit, baseline.model = if (isTRUE(null.model)) { mod.null.fit } else { NULL })) } else { lav.fit.metric <- NULL }

  #### Scalar invariance model
  if (isTRUE(any(c("scalar", "strict") %in% invar))) { lav.fit.scalar <- suppressWarnings(lavaan::fitmeasures(mod.scalar.fit, baseline.model = if (isTRUE(null.model)) { mod.null.fit } else { NULL })) } else { lav.fit.scalar <- NULL }

  #### Strict invariance model
  if (isTRUE(invar == "strict")) { lav.fit.strict <- suppressWarnings(lavaan::fitmeasures(mod.strict.fit, baseline.model = if (isTRUE(null.model)) { mod.null.fit } else { NULL })) } else { lav.fit.strict <- NULL }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Parameter estimates ####

  mod.metric.param <- mod.scalar.param <- mod.strict.param <- NULL

  if (isTRUE(!long)) {

    #### Configural invariance model
    mod.config.param <- within(data.frame(lavaan::parameterEstimates(mod.config.fit), stdyx = lavaan::standardizedsolution(mod.config.fit)[, "est.std"]), assign("label", ""))[, c("group", "lhs", "op", "rhs", "label", "est", "se", "z", "pvalue", "stdyx")]

    #### Metric invariance model
    if (isTRUE(any(c("metric", "scalar", "strict") %in% invar))) { mod.metric.param <- data.frame(lavaan::parameterEstimates(mod.metric.fit), stdyx = lavaan::standardizedsolution(mod.metric.fit)[, "est.std"])[, c("group", "lhs", "op", "rhs", "label", "est", "se", "z", "pvalue", "stdyx")] }

    #### Scalar invariance model
    if (isTRUE(any(c("scalar", "strict") %in% invar))) { mod.scalar.param <- data.frame(lavaan::parameterEstimates(mod.scalar.fit), stdyx = lavaan::standardizedsolution(mod.scalar.fit)[, "est.std"])[, c("group", "lhs", "op", "rhs", "label", "est", "se", "z", "pvalue", "stdyx")] }

    #### Strict invariance model
    if (isTRUE(invar == "strict")) { mod.strict.param <- data.frame(lavaan::parameterEstimates(mod.strict.fit), stdyx = lavaan::standardizedsolution(mod.strict.fit)[, "est.std"])[, c("group", "lhs", "op", "rhs", "label", "est", "se", "z", "pvalue", "stdyx")] }

  } else {

    #### Configural invariance model
    mod.config.param <- within(data.frame(lavaan::parameterEstimates(mod.config.fit), stdyx = lavaan::standardizedsolution(mod.config.fit)[, "est.std"]), assign("label", ""))[, c("lhs", "op", "rhs", "label", "est", "se", "z", "pvalue", "stdyx")]

    #### Metric invariance model
    if (isTRUE(any(c("metric", "scalar", "strict") %in% invar))) { mod.metric.param <- data.frame(lavaan::parameterEstimates(mod.metric.fit), stdyx = lavaan::standardizedsolution(mod.metric.fit)[, "est.std"])[, c("lhs", "op", "rhs", "label", "est", "se", "z", "pvalue", "stdyx")] }

    #### Scalar invariance model
    if (isTRUE(any(c("scalar", "strict") %in% invar))) { mod.scalar.param <- data.frame(lavaan::parameterEstimates(mod.scalar.fit), stdyx = lavaan::standardizedsolution(mod.scalar.fit)[, "est.std"])[, c("lhs", "op", "rhs", "label", "est", "se", "z", "pvalue", "stdyx")] }

    #### Strict invariance model
    if (isTRUE(invar == "strict")) { mod.strict.param <- data.frame(lavaan::parameterEstimates(mod.strict.fit), stdyx = lavaan::standardizedsolution(mod.strict.fit)[, "est.std"])[, c("lhs", "op", "rhs", "label", "est", "se", "z", "pvalue", "stdyx")] }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Modification Indices ####

  mod.config.modind <- mod.metric.modind <- mod.scalar.modind <- mod.strict.modind <- NULL

  if (isTRUE(estimator != "PML" && "modind" %in% print)) {

    #### Configural invariance model
    mod.config.modind <- tryCatch(lavaan::modindices(mod.config.fit), error = function(y) NULL, warning = function(z) {})
    if (isTRUE(!is.null(mod.config.modind))) { mod.config.modind <- misty::df.rename(mod.config.modind[, if (isTRUE(!long)) { c("group", "lhs", "op", "rhs", "mi", "epc", "sepc.all") } else { c("lhs", "op", "rhs", "mi", "epc", "sepc.all") } ], from = "sepc.all", to = "stdyx") }

    #### Metric invariance model
    if (isTRUE(any(c("metric", "scalar", "strict") %in% invar))) {

      mod.metric.modind <- tryCatch(lavaan::modindices(mod.metric.fit), error = function(y) NULL, warning = function(z) {})
      if (isTRUE(!is.null(mod.metric.modind))) { mod.metric.modind <- misty::df.rename(mod.metric.modind[, if (isTRUE(!long)) { c("group", "lhs", "op", "rhs", "mi", "epc", "sepc.all") } else { c("lhs", "op", "rhs", "mi", "epc", "sepc.all") } ], from = "sepc.all", to = "stdyx") }

    }

    #### Scalar invariance model
    if (isTRUE(any(c("scalar", "strict") %in% invar))) {

      mod.scalar.modind <- tryCatch(lavaan::modindices(mod.scalar.fit), error = function(y) NULL, warning = function(z) {})
      if (isTRUE(!is.null(mod.scalar.modind))) { mod.scalar.modind <- misty::df.rename(mod.scalar.modind[, if (isTRUE(!long)) { c("group", "lhs", "op", "rhs", "mi", "epc", "sepc.all") } else { c("lhs", "op", "rhs", "mi", "epc", "sepc.all") } ], from = "sepc.all", to = "stdyx") }

    }

    #### Strict invariance model
    if (isTRUE(invar == "strict")) {

      mod.strict.modind <- tryCatch(lavaan::modindices(mod.strict.fit), error = function(y) NULL, warning = function(z) {})
      if (isTRUE(!is.null(mod.strict.modind))) { mod.strict.modind <- misty::df.rename(mod.strict.modind[, if (isTRUE(!long)) { c("group", "lhs", "op", "rhs", "mi", "epc", "sepc.all") } else { c("lhs", "op", "rhs", "mi", "epc", "sepc.all") } ], from = "sepc.all", to = "stdyx") }

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Score Test ####

  mod.metric.score <- mod.scalar.score <- mod.strict.score <- NULL

  if (isTRUE("modind" %in% print)) {

    if (isTRUE(estimator != "PML")) {

      #### Metric invariance model
      if (isTRUE(any(c("metric", "scalar", "strict") %in% invar))) { mod.metric.score <- tryCatch(lavaan::lavTestScore(mod.metric.fit, epc = TRUE, warn = FALSE), error = function(y) NULL, warning = function(z) {}) }

      #### Scalar invariance model
      if (isTRUE(any(c("scalar", "strict") %in% invar))) { mod.scalar.score <- tryCatch(lavaan::lavTestScore(mod.scalar.fit, epc = TRUE, warn = FALSE), error = function(y) NULL, warning = function(z) {}) }

      #### Strict invariance model
      if (isTRUE(invar == "strict")) { mod.strict.score <- tryCatch(lavaan::lavTestScore(mod.strict.fit, epc = TRUE, warn = FALSE), error = function(y) NULL, warning = function(z) {}) }

    }

    #### Metric invariance model
    if (isTRUE(!is.null(mod.metric.score))) {

      # Parameter table
      partable <- lavaan::parTable(mod.metric.fit)

      # Univariate score statistics
      uniscore <- mod.metric.score$uni

      # Effects coding
      if (isTRUE(ident == "effect")) { uniscore <- uniscore[-grep("-", uniscore$rhs), ] }

      # Expected parameter change
      epcscore <- mod.metric.score$epc

      # Between-group measurement invariance
      if (isTRUE(!long)) {

        mod.metric.score <- data.frame(label = NA, group.lhs = NA, group.rhs = NA, lhs = NA, op = NA, rhs = NA, mi = NA, df = NA, pvalue = NA, lhs.epc = NA, rhs.epc = NA, lhs.stdyx = NA, rhs.stdyx = NA)

        for (i in seq_len(nrow(uniscore))) {

          mod.metric.score[i, ] <- data.frame(label = partable[partable$plabel == uniscore[i, "lhs"], "label"],
                                              group.lhs = partable[partable$plabel == uniscore[i, "lhs"], "group"],
                                              group.rhs = partable[partable$plabel == uniscore[i, "rhs"], "group"],
                                              lhs = paste0(partable[partable$plabel == uniscore[i, "lhs"], c("lhs", "op", "rhs")], collapse = " "),
                                              op = "==",
                                              rhs = paste0(partable[partable$plabel == uniscore[i, "rhs"], c("lhs", "op", "rhs")], collapse = " "),
                                              mi = uniscore[i, "X2"], df = uniscore[i, "df"], pvalue = uniscore[i, "p.value"],
                                              lhs.epc = epcscore[partable$plabel == uniscore[i, "lhs"], "epc"],
                                              rhs.epc = epcscore[partable$plabel == uniscore[i, "rhs"], "epc"],
                                              lhs.stdyx = epcscore[partable$plabel == uniscore[i, "lhs"], "sepc.all"],
                                              rhs.stdyx = epcscore[partable$plabel == uniscore[i, "rhs"], "sepc.all"])

        }

      # Longitudinal measurement invariance
      } else {

        mod.metric.score <- data.frame(label = NA, lhs = NA, op = NA, rhs = NA, mi = NA, df = NA, pvalue = NA, lhs.epc = NA, rhs.epc = NA, lhs.stdyx = NA, rhs.stdyx = NA)

        for (i in seq_len(nrow(uniscore))) {

          mod.metric.score[i, ] <- data.frame(label = partable[partable$plabel == uniscore[i, "lhs"], "label"],
                                              lhs = paste0(partable[partable$plabel == uniscore[i, "lhs"], c("lhs", "op", "rhs")], collapse = " "),
                                              op = "==",
                                              rhs = paste0(partable[partable$plabel == uniscore[i, "rhs"], c("lhs", "op", "rhs")], collapse = " "),
                                              mi = uniscore[i, "X2"], df = uniscore[i, "df"], pvalue = uniscore[i, "p.value"],
                                              lhs.epc = epcscore[partable$plabel == uniscore[i, "lhs"], "epc"],
                                              rhs.epc = epcscore[partable$plabel == uniscore[i, "rhs"], "epc"],
                                              lhs.stdyx = epcscore[partable$plabel == uniscore[i, "lhs"], "sepc.all"],
                                              rhs.stdyx = epcscore[partable$plabel == uniscore[i, "rhs"], "sepc.all"])

        }

      }

    }

    #### Scalar invariance model
    if (isTRUE(!is.null(mod.scalar.score))) {

      # Parameter table
      partable <- lavaan::parTable(mod.scalar.fit)

      # Univariate score statistics
      uniscore <- mod.scalar.score$uni

      # Expected parameter change
      epcscore <- mod.scalar.score$epc

      # Between-group measurement invariance
      if (isTRUE(!long)) {

        mod.scalar.score <- data.frame(label = NA, group.lhs = NA, group.rhs = NA, lhs = NA, op = NA, rhs = NA, mi = NA, df = NA, pvalue = NA, lhs.epc = NA, rhs.epc = NA, lhs.stdyx = NA, rhs.stdyx = NA)

        for (i in seq_len(nrow(uniscore))) {

          mod.scalar.score[i, ] <- data.frame(label = partable[partable$plabel == uniscore[i, "lhs"], "label"],
                                              group.lhs = partable[partable$plabel == uniscore[i, "lhs"], "group"],
                                              group.rhs = partable[partable$plabel == uniscore[i, "rhs"], "group"],
                                              lhs = paste0(partable[partable$plabel == uniscore[i, "lhs"], c("lhs", "op", "rhs")], collapse = " "),
                                              op = "==",
                                              rhs = paste0(partable[partable$plabel == uniscore[i, "rhs"], c("lhs", "op", "rhs")], collapse = " "),
                                              mi = uniscore[i, "X2"], df = uniscore[i, "df"], pvalue = uniscore[i, "p.value"],
                                              lhs.epc = epcscore[partable$plabel == uniscore[i, "lhs"], "epc"],
                                              rhs.epc = epcscore[partable$plabel == uniscore[i, "rhs"], "epc"],
                                              lhs.stdyx = epcscore[partable$plabel == uniscore[i, "lhs"], "sepc.all"],
                                              rhs.stdyx = epcscore[partable$plabel == uniscore[i, "rhs"], "sepc.all"])

        }

      # Longitudinal measurement invariance
      } else {

        mod.scalar.score <- data.frame(label = NA, lhs = NA, op = NA, rhs = NA, mi = NA, df = NA, pvalue = NA, lhs.epc = NA, rhs.epc = NA, lhs.stdyx = NA, rhs.stdyx = NA)

        for (i in seq_len(nrow(uniscore))) {

          mod.scalar.score[i, ] <- data.frame(label = partable[partable$plabel == uniscore[i, "lhs"], "label"],
                                              lhs = paste0(partable[partable$plabel == uniscore[i, "lhs"], c("lhs", "op", "rhs")], collapse = " "),
                                              op = "==",
                                              rhs = paste0(partable[partable$plabel == uniscore[i, "rhs"], c("lhs", "op", "rhs")], collapse = " "),
                                              mi = uniscore[i, "X2"], df = uniscore[i, "df"], pvalue = uniscore[i, "p.value"],
                                              lhs.epc = epcscore[partable$plabel == uniscore[i, "lhs"], "epc"],
                                              rhs.epc = epcscore[partable$plabel == uniscore[i, "rhs"], "epc"],
                                              lhs.stdyx = epcscore[partable$plabel == uniscore[i, "lhs"], "sepc.all"],
                                              rhs.stdyx = epcscore[partable$plabel == uniscore[i, "rhs"], "sepc.all"])

        }

      }

    }

    #### Strict invariance model
    if (isTRUE(!is.null(mod.strict.score))) {

      # Parameter table
      partable <- lavaan::parTable(mod.strict.fit)

      # Univariate score statistics
      uniscore <- mod.strict.score$uni

      # Expected parameter change
      epcscore <- mod.strict.score$epc

      # Between-group measurement invariance
      if (isTRUE(!long)) {

        mod.strict.score <- data.frame(label = NA, group.lhs = NA, group.rhs = NA, lhs = NA, op = NA, rhs = NA, mi = NA, df = NA, pvalue = NA, lhs.epc = NA, rhs.epc = NA, lhs.stdyx = NA, rhs.stdyx = NA)

        for (i in seq_len(nrow(uniscore))) {

          mod.strict.score[i, ] <- data.frame(label = partable[partable$plabel == uniscore[i, "lhs"], "label"],
                                              group.lhs = partable[partable$plabel == uniscore[i, "lhs"], "group"],
                                              group.rhs = partable[partable$plabel == uniscore[i, "rhs"], "group"],
                                              lhs = paste0(partable[partable$plabel == uniscore[i, "lhs"], c("lhs", "op", "rhs")], collapse = " "),
                                              op = "==",
                                              rhs = paste0(partable[partable$plabel == uniscore[i, "rhs"], c("lhs", "op", "rhs")], collapse = " "),
                                              mi = uniscore[i, "X2"], df = uniscore[i, "df"], pvalue = uniscore[i, "p.value"],
                                              lhs.epc = epcscore[partable$plabel == uniscore[i, "lhs"], "epc"],
                                              rhs.epc = epcscore[partable$plabel == uniscore[i, "rhs"], "epc"],
                                              lhs.stdyx = epcscore[partable$plabel == uniscore[i, "lhs"], "sepc.all"],
                                              rhs.stdyx = epcscore[partable$plabel == uniscore[i, "rhs"], "sepc.all"])

        }

      # Longitudinal measurement invariance
      } else {

        mod.strict.score <- data.frame(label = NA, lhs = NA, op = NA, rhs = NA, mi = NA, df = NA, pvalue = NA, lhs.epc = NA, rhs.epc = NA, lhs.stdyx = NA, rhs.stdyx = NA)

        for (i in seq_len(nrow(uniscore))) {

          mod.strict.score[i, ] <- data.frame(label = partable[partable$plabel == uniscore[i, "lhs"], "label"],
                                              lhs = paste0(partable[partable$plabel == uniscore[i, "lhs"], c("lhs", "op", "rhs")], collapse = " "),
                                              op = "==",
                                              rhs = paste0(partable[partable$plabel == uniscore[i, "rhs"], c("lhs", "op", "rhs")], collapse = " "),
                                              mi = uniscore[i, "X2"], df = uniscore[i, "df"], pvalue = uniscore[i, "p.value"],
                                              lhs.epc = epcscore[partable$plabel == uniscore[i, "lhs"], "epc"],
                                              rhs.epc = epcscore[partable$plabel == uniscore[i, "rhs"], "epc"],
                                              lhs.stdyx = epcscore[partable$plabel == uniscore[i, "lhs"], "sepc.all"],
                                              rhs.stdyx = epcscore[partable$plabel == uniscore[i, "rhs"], "sepc.all"])

        }

      }

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Residual Correlation Matrix ####

  mod.config.resid <- mod.metric.resid <- mod.scalar.resid <- mod.strict.resid <- NULL

  if (isTRUE("resid" %in% print)) {

    #### Configural invariance model
    mod.config.resid <- tryCatch(lavaan::lavResiduals(mod.config.fit, type = "cor.bollen"), error = function(y) NULL, warning = function(z) {})

    # Combine residual correlation matrix and standardized residual means
    if (isTRUE(!long)) {

      mod.config.resid <- lapply(mod.config.resid, function(y) rbind(y$cov, mean = y$mean))

    } else {

      mod.config.resid <- rbind(mod.config.resid$cov, mean = mod.config.resid$mean)

    }

    #### Metric invariance model
    if (isTRUE(any(c("metric", "scalar", "strict") %in% invar))) {

      mod.metric.resid <- tryCatch(lavaan::lavResiduals(mod.metric.fit, type = "cor.bollen"), error = function(y) NULL, warning = function(z) {})

      # Combine residual correlation matrix and standardized residual means
      if (isTRUE(!long)) {

        mod.metric.resid <- lapply(mod.metric.resid, function(y) rbind(y$cov, mean = y$mean))

      } else {

        mod.metric.resid <- rbind(mod.metric.resid$cov, mean = mod.metric.resid$mean)

      }

    }

    #### Scalar invariance model
    if (isTRUE(any(c("scalar", "strict") %in% invar))) {

      mod.scalar.resid <- tryCatch(lavaan::lavResiduals(mod.scalar.fit, type = "cor.bollen"), error = function(y) NULL, warning = function(z) {})

      # Combine residual correlation matrix and standardized residual means
      if (isTRUE(!long)) {

        mod.scalar.resid <- lapply(mod.scalar.resid, function(y) rbind(y$cov, mean = y$mean))

      } else {

        mod.scalar.resid <- rbind(mod.scalar.resid$cov, mean = mod.scalar.resid$mean)

      }

    }

    #### Strict invariance model
    if (isTRUE(invar == "strict")) {

      mod.strict.resid <- tryCatch(lavaan::lavResiduals(mod.strict.fit, type = "cor.bollen"), error = function(y) NULL, warning = function(z) {})

      # Combine residual correlation matrix and standardized residual means
      if (isTRUE(!long)) {

        mod.strict.resid <- lapply(mod.strict.resid, function(y) rbind(y$cov, mean = y$mean))

      } else {

        mod.strict.resid <- rbind(mod.strict.resid$cov, mean = mod.strict.resid$mean)

      }

    }

  }

  #_____________________________________________________________________________
  #
  # Return object --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## lavaan summary ####

  #...................
  ### Cluster

  cluster.unique <- 1

  if (isTRUE(!is.null(cluster))) {

    #### Between-group measurement invariance
    if (isTRUE(!long)) {

      # Number of clusters
      cluster.unique <- tapply(x$.cluster, x$.group, function(y) length(unique(na.omit(y))))

    #### Longitudinal measurement invariance
    } else {

      # Number of clusters
      cluster.unique <- length(unique(na.omit(x$.cluster)))

    }

  }

  lavaan.summary <- data.frame(### First column
                               c(paste("lavaan", lavaan::lavInspect(mod.config.fit, what = "version")), "",
                                 "Estimator", "Optimization Method", "",
                                 "Test Statistic", "Standard Errors", "Missing Data",  "",
                                 "Indicators", "Identification", "", "",
                                 "Number of Model Parameters", "Number of Equality Constraints", "", "",
                                 if (isTRUE(!long)) { "Total Number of Observations" } else { "Number of Observations" }, "Number of Observations per Group", if (isTRUE(lavaan::lavInspect(mod.config.fit, what = "ngroups") == 1L)) { "1" } else { lavaan::lavInspect(mod.config.fit, what = "group.label") },
                                 "Number of Clusters", if (isTRUE(!is.null(cluster) && !is.null(group))) { lavaan::lavInspect(mod.config.fit, what = "group.label") }),
                               ### Second column
                               c("", "",
                                 # Estimator
                                 estimator,
                                 # Optimization method
                                 toupper(lavaan::lavTech(mod.config.fit, what = "options")$optim.method), "",
                                 # Test statistic
                                 switch(lavaan::lavTech(mod.config.fit, what = "options")$test[1L],
                                        "standard" = "Conventional",
                                        "satorra.bentler" = "Satorra-Bentler",
                                        "scaled.shifted" = "Scale-Shifted",
                                        "mean.var.adjusted" = "Satterthwaite",
                                        "yuan.bentler.mplus" = "Yuan-Bentler"),
                                 # Standard errors
                                 switch(lavaan::lavTech(mod.config.fit, what = "options")$se,
                                        "standard" = "Conventional",
                                        "robust.sem" = "Conventional Robust",
                                        "robust.huber.white" = "Huber-White",
                                        "robust.cluster" = "Cluster-Robust H-W",
                                        "robust.cluster.sem" = "Cluster-Robust Conven",
                                        "two.stage" = "Two-Stage",
                                        "robust.two.stage" = "Robust Two-Stage"),
                                 # Missing data
                                 ifelse(isTRUE(complete), "None",
                                        switch(missing,
                                               "listwise" = "Listwise Deletion",
                                               "pairwise" = "Pairwise Deletion",
                                               "fiml" = "FIML",
                                               "two.stage" = "Two-Stage",
                                               "robust.two.stage" = "Robust Two-Stage",
                                               "doubly.robust" = "Doubly-Robust")), "",
                                 # Indicators
                                 ifelse(isTRUE(!ordered), "Continuous", "Ordered-Categorical"),
                                 # Identification
                                 switch(ident, "marker" = "Marker Variable", "var" = "Factor Variance", "effect" = "Effects Coding"), "", "Config",
                                 # Number of model parameters
                                 max(lavaan::parTable(mod.config.fit)$free),
                                 # Number of equality constraints
                                 sum(lavaan::parTable(mod.config.fit)$op == "=="), "", "Used",
                                 # Total number of observations
                                 sum(lavaan::lavInspect(mod.config.fit, what = "nobs")), "",
                                 # Number of observations per group
                                 lavaan::lavInspect(mod.config.fit, what = "nobs"), if (isTRUE(!is.null(cluster) && !is.null(group))) { "" },
                                 # Number of clusters
                                 if (isTRUE(!is.null(cluster))) { cluster.unique } else { "1" }),
                               ### Third column
                               c(rep("", times = 12L), "Metric",
                                 # Number of model parameters
                                 if (isTRUE(!is.null(mod.metric.fit))) { c(max(lavaan::parTable(mod.metric.fit)$free), sum(lavaan::parTable(mod.metric.fit)$op == "==")) } else { c("", "") }, "", "Total",
                                 # Total number of observations
                                 sum(lavaan::lavInspect(mod.config.fit, what = "norig")), "",
                                 # Number of observations per group
                                 lavaan::lavInspect(mod.config.fit, what = "norig"), if (isTRUE(!is.null(group) && !is.null(cluster))) { "" },
                                 # Number of clusters
                                 if (isTRUE(!is.null(cluster))) { rep("", times = length(cluster.unique)) } else { "" }),
                               ### Fourth column
                               c(rep("", times = 12L), "Scalar",
                                 # Number of model parameters
                                 if (isTRUE(!is.null(mod.scalar.fit))) { c(max(lavaan::parTable(mod.scalar.fit)$free), sum(lavaan::parTable(mod.scalar.fit)$op == "==")) } else { c("", "") }, "", "",
                                 # Number of observations used
                                 "", "", rep("", times = length(lavaan::lavInspect(mod.config.fit, what = "nobs"))), if (isTRUE(!is.null(group) && !is.null(cluster))) { "" },
                                 # Number of clusters
                                 if (isTRUE(!is.null(cluster))) { rep("", times = length(cluster.unique)) } else { "" }),
                               ### Fifth column
                               c(rep("", times = 12L), "Strict",
                                 # Number of model parameters
                                 if (isTRUE(!is.null(mod.strict.fit))) { c(max(lavaan::parTable(mod.strict.fit)$free), sum(lavaan::parTable(mod.strict.fit)$op == "==")) } else { c("", "") }, "", "",
                                 # Number of observations used
                                 "", "", rep("", times = length(lavaan::lavInspect(mod.config.fit, what = "nobs"))), if (isTRUE(!is.null(group) && !is.null(cluster))) { "" },
                                 # Number of clusters
                                 if (isTRUE(!is.null(cluster))) { rep("", times = length(cluster.unique)) } else { "" }), fix.empty.names = FALSE)

  #### Configural invariance model
  if (isTRUE(invar == "config")) { lavaan.summary <- lavaan.summary[, c(1L:3L)]; lavaan.summary[c(13L:15L), 3L] <- "" }

  #### Metric invariance model
  if (isTRUE(invar == "metric")) { lavaan.summary <- lavaan.summary[, c(1L:3L)] }

  #### Scalar invariance model
  if (isTRUE(invar == "scalar")) { lavaan.summary <- lavaan.summary[, c(1L:4L)] }

  #...................
  ### No grouping variable
  if (isTRUE(is.null(group))) { lavaan.summary <- lavaan.summary[-c(19L:20L), ] }

  #...................
  ### No cluster variable
  if (isTRUE(is.null(cluster))) { lavaan.summary <- lavaan.summary[-c(which(lavaan.summary[, 1L] %in% c("Number of Clusters")):nrow(lavaan.summary)), ] }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Extract and Combine Model Fit Indices ####

  #...................
  ### Configural invariance model ####

  #### Standard fit indices
  fit.stand <- data.frame(### Fist column
                          c("Chi-Square Test of Model Fit", "Test statistic", if (isTRUE(!is.null(group))) { lavaan::lavInspect(mod.config.fit, what = "group.label") }, "Degrees of freedom", "P-value", "",
                            "Incremental Fit Indices", "CFI", "TLI", "",
                            "Absolute Fit Indices", "RMSEA", "90 Percent CI - lower", "90 Percent CI - upper", "P-value RMSEA <= 0.05", "", "SRMR", "",
                            "Information Criteria", "Akaike (AIC)", "Bayesian (BIC)", "Sample-Size Adjusted BIC"),
                          ### Second column
                          config = c(NA, lav.fit.config["chisq"], if (isTRUE(!is.null(group))) { lavaan::lavInspect(mod.config.fit, what = "test")$standard$stat.group }, lav.fit.config[c("df", "pvalue")], NA,
                                     NA, lav.fit.config[c("cfi", "tli")], NA,
                                     NA, lav.fit.config[c("rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue")], NA, lav.fit.config["srmr_bentler"], NA,
                                     NA, lav.fit.config[c("aic", "bic", "bic2")]), fix.empty.names = FALSE)

  #### Scaled fit indices
  fit.scaled <- data.frame(### Fist column
                           c("Chi-Square Test of Model Fit", "Test statistic", if (isTRUE(!is.null(group))) { lavaan::lavInspect(mod.config.fit, what = "group.label") }, "Degrees of freedom", "P-value", "Scaling Correction Factor", "",
                             "Incremental Fit Indices", "CFI", "TLI", "",
                             "Absolute Fit Indices", "RMSEA", "90 Percent CI - lower", "90 Percent CI - upper", "P-value RMSEA <= 0.05", "", "SRMR", "",
                             "Information Criteria", "Akaike (AIC)", "Bayesian (BIC)", "Sample-Size Adjusted BIC"),
                           ### Second column
                           config = c(NA, lav.fit.config["chisq.scaled"], if (isTRUE(!is.null(group))) {

                             if (isTRUE(estimator %in% c("MLM", "WLSM", "DLS"))) {

                               stat <- lavaan::lavInspect(mod.config.fit, what = "test")$satorra.bentler$stat.group

                             } else if(isTRUE(estimator %in% c("WLSMV", "ULSM", "MLMV", "ULSMV", "ULS"))) {

                               stat <- lavaan::lavInspect(mod.config.fit, what = "test")$scaled.shifted$stat.group

                             } else {

                               stat <- lavaan::lavInspect(mod.config.fit, what = "test")$yuan.bentler.mplus$stat.group

                             }

                             if (isTRUE(is.null(stat))) {

                               rep(NA, times = ngroups)

                             } else {

                               stat

                             }

                           }, lav.fit.config[c("df.scaled", "pvalue.scaled", "chisq.scaling.factor")], NA,
                           NA, lav.fit.config[c("cfi.scaled", "tli.scaled")], NA,
                           NA, lav.fit.config[c("rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "rmsea.pvalue.scaled")], NA, lav.fit.config["srmr_bentler"], NA,
                           NA, lav.fit.config[c("aic", "bic", "bic2")]), fix.empty.names = FALSE)

  #### Robust fit indices
  fit.robust <- data.frame(### Fist column
                           c("Chi-Square Test of Model Fit", "Test statistic", if (isTRUE(!is.null(group))) { lavaan::lavInspect(mod.config.fit, what = "group.label") }, "Degrees of freedom", "P-value", "Scaling Correction Factor", "",
                             "Incremental Fit Indices", "CFI", "TLI", "",
                             "Absolute Fit Indices", "RMSEA", "90 Percent CI - lower", "90 Percent CI - upper", "P-value RMSEA <= 0.05", "", "SRMR", "",
                             "Information Criteria", "Akaike (AIC)", "Bayesian (BIC)", "Sample-Size Adjusted BIC"),
                           ### Second column
                           config = c(NA, lav.fit.config["chisq.scaled"], if (isTRUE(!is.null(group))) {

                             if (isTRUE(estimator %in% c("MLM", "WLSM", "DLS"))) {

                               stat <- lavaan::lavInspect(mod.config.fit, what = "test")$satorra.bentler$stat.group

                             } else if(isTRUE(estimator %in% c("WLSMV", "ULSM", "MLMV", "ULSMV", "ULS"))) {

                               stat <- lavaan::lavInspect(mod.config.fit, what = "test")$scaled.shifted$stat.group

                             } else {

                               stat <- lavaan::lavInspect(mod.config.fit, what = "test")$yuan.bentler.mplus$stat.group

                             }

                             if (isTRUE(is.null(stat))) {

                               rep(NA, times = ngroups)

                             } else {

                               stat

                             }

                           }, lav.fit.config[c("df.scaled", "pvalue.scaled", "chisq.scaling.factor")], NA,
                           NA, lav.fit.config[c("cfi.robust", "tli.robust")], NA,
                           NA, lav.fit.config[c("rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust", "rmsea.pvalue.robust")], NA, lav.fit.config["srmr_bentler"], NA,
                           NA, lav.fit.config[c("aic", "bic", "bic2")]), fix.empty.names = FALSE)

  #...................
  ### Metric invariance model ####

  switch (invar, metric = {

    #### Standard fit indices
    fit.stand <- data.frame(fit.stand,
                            ### Third column
                            metric = c(NA, lav.fit.metric["chisq"], if (isTRUE(!is.null(group))) { lavaan::lavInspect(mod.metric.fit, what = "test")$standard$stat.group }, lav.fit.metric[c("df", "pvalue")], NA,
                                       NA, lav.fit.metric[c("cfi", "tli")], NA,
                                       NA, lav.fit.metric[c("rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue")], NA, lav.fit.metric["srmr_bentler"], NA,
                                       NA, lav.fit.metric[c("aic", "bic", "bic2")]),
                            ### Fourth column
                            dmetric = c(NA, unlist(chidiff.conf.met[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.conf.met[2L, c("Df diff", "Pr(>Chisq)")]), NA,
                                        NA, lav.fit.metric[c("cfi", "tli")] - lav.fit.config[c("cfi", "tli")], NA,
                                        NA, lav.fit.metric["rmsea"] - lav.fit.config["rmsea"], NA, NA, NA, NA, lav.fit.metric["srmr_bentler"] - lav.fit.metric["srmr_bentler"], NA,
                                        NA, lav.fit.metric[c("aic", "bic", "bic2")] - lav.fit.config[c("aic", "bic", "bic2")]), fix.empty.names = FALSE)

    #### Scaled fit indices
    fit.scaled <- data.frame(fit.scaled,
                             ### Third column
                             metric = c(NA, lav.fit.metric["chisq.scaled"], if (isTRUE(!is.null(group))) {

                               if (isTRUE(estimator %in% c("MLM", "WLSM", "DLS"))) {

                                 stat <- lavaan::lavInspect(mod.metric.fit, what = "test")$satorra.bentler$stat.group

                               } else if(isTRUE(estimator %in% c("WLSMV", "ULSM", "MLMV", "ULSMV", "ULS"))) {

                                 stat <- lavaan::lavInspect(mod.metric.fit, what = "test")$scaled.shifted$stat.group

                               } else {

                                 stat <- lavaan::lavInspect(mod.metric.fit, what = "test")$yuan.bentler.mplus$stat.group

                               }

                               if (isTRUE(is.null(stat))) {

                                 rep(NA, times = ngroups)

                               } else {

                                 stat

                               }

                             }, lav.fit.metric[c("df.scaled", "pvalue.scaled", "chisq.scaling.factor")], NA,
                             NA, lav.fit.metric[c("cfi.scaled", "tli.scaled")], NA,
                             NA, lav.fit.metric[c("rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "rmsea.pvalue.scaled")], NA, lav.fit.metric["srmr_bentler"], NA,
                             NA, lav.fit.metric[c("aic", "bic", "bic2")]),
                             ### Fourth column
                             dmetric = c(NA, unlist(chidiff.conf.met[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.conf.met[2L, c("Df diff", "Pr(>Chisq)")]), NA, NA,
                                         NA, lav.fit.metric[c("cfi.scaled", "tli.scaled")] - lav.fit.config[c("cfi.scaled", "tli.scaled")], NA,
                                         NA, lav.fit.metric["rmsea.scaled"] - lav.fit.config["rmsea.scaled"], NA, NA, NA, NA, lav.fit.metric["srmr_bentler"] - lav.fit.config["srmr_bentler"], NA,
                                         NA, lav.fit.metric[c("aic", "bic", "bic2")] - lav.fit.config[c("aic", "bic", "bic2")]), fix.empty.names = FALSE)

    #### Robust fit indices
    fit.robust <- data.frame(fit.robust,
                             ### Third column
                             metric = c(NA, lav.fit.metric["chisq.scaled"], if (isTRUE(!is.null(group))) {

                               if (isTRUE(estimator %in% c("MLM", "WLSM", "DLS"))) {

                                 stat <- lavaan::lavInspect(mod.metric.fit, what = "test")$satorra.bentler$stat.group

                               } else if(isTRUE(estimator %in% c("WLSMV", "ULSM", "MLMV", "ULSMV", "ULS"))) {

                                 stat <- lavaan::lavInspect(mod.metric.fit, what = "test")$scaled.shifted$stat.group

                               } else {

                                 stat <- lavaan::lavInspect(mod.metric.fit, what = "test")$yuan.bentler.mplus$stat.group

                               }

                               if (isTRUE(is.null(stat))) {

                                 rep(NA, times = ngroups)

                               } else {

                                 stat

                               }

                             }, lav.fit.metric[c("df.scaled", "pvalue.scaled", "chisq.scaling.factor")], NA,
                             NA, lav.fit.metric[c("cfi.robust", "tli.robust")], NA,
                             NA, lav.fit.metric[c("rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust", "rmsea.pvalue.robust")], NA, lav.fit.metric["srmr_bentler"], NA,
                             NA, lav.fit.metric[c("aic", "bic", "bic2")]),
                             ### Fourth column
                             dmetric = c(NA, unlist(chidiff.conf.met[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.conf.met[2L, c("Df diff", "Pr(>Chisq)")]), NA, NA,
                                         NA, lav.fit.metric[c("cfi.robust", "tli.robust")] - lav.fit.config[c("cfi.robust", "tli.robust")], NA,
                                         NA, lav.fit.metric["rmsea.robust"] - lav.fit.config["rmsea.robust"], NA, NA, NA, NA, lav.fit.metric["srmr_bentler"] - lav.fit.config["srmr_bentler"], NA,
                                         NA, lav.fit.metric[c("aic", "bic", "bic2")] - lav.fit.config[c("aic", "bic", "bic2")]), fix.empty.names = FALSE)

  #...................
  ### Scalar invariance model ####

  }, scalar = {

    fit.stand <- data.frame(fit.stand,
                            ### Third column
                            metric = c(NA, lav.fit.metric["chisq"], if (isTRUE(!is.null(group))) { lavaan::lavInspect(mod.metric.fit, what = "test")$standard$stat.group }, lav.fit.metric[c("df", "pvalue")], NA,
                                       NA, lav.fit.metric[c("cfi", "tli")], NA,
                                       NA, lav.fit.metric[c("rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue")], NA, lav.fit.metric["srmr_bentler"], NA,
                                       NA, lav.fit.metric[c("aic", "bic", "bic2")]),
                            ### Fourth column
                            scalar = c(NA, lav.fit.scalar["chisq"], if (isTRUE(!is.null(group))) { lavaan::lavInspect(mod.scalar.fit, what = "test")$standard$stat.group }, lav.fit.scalar[c("df", "pvalue")], NA,
                                       NA, lav.fit.scalar[c("cfi", "tli")], NA,
                                       NA, lav.fit.scalar[c("rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue")], NA, lav.fit.scalar["srmr_bentler"], NA,
                                       NA, lav.fit.scalar[c("aic", "bic", "bic2")]),
                            ### Fifth column
                            dmetric = c(NA, unlist(chidiff.conf.met[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.conf.met[2L, c("Df diff", "Pr(>Chisq)")]), NA,
                                        NA, lav.fit.metric[c("cfi", "tli")] - lav.fit.config[c("cfi", "tli")], NA,
                                        NA, lav.fit.metric["rmsea"] - lav.fit.config["rmsea"], NA, NA, NA, NA, lav.fit.metric["srmr_bentler"] - lav.fit.config["srmr_bentler"], NA,
                                        NA, lav.fit.metric[c("aic", "bic", "bic2")] - lav.fit.config[c("aic", "bic", "bic2")]),
                            ### Sixth column
                            dscalar = c(NA, unlist(chidiff.met.sca[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.met.sca[2L, c("Df diff", "Pr(>Chisq)")]), NA,
                                        NA, lav.fit.scalar[c("cfi", "tli")] - lav.fit.metric[c("cfi", "tli")], NA,
                                        NA, lav.fit.scalar["rmsea"] - lav.fit.metric["rmsea"], NA, NA, NA, NA, lav.fit.scalar["srmr_bentler"] - lav.fit.metric["srmr_bentler"], NA,
                                        NA, lav.fit.scalar[c("aic", "bic", "bic2")] - lav.fit.metric[c("aic", "bic", "bic2")]), fix.empty.names = FALSE)


    #### Scaled fit indices
    fit.scaled <- data.frame(fit.scaled,
                             ### Third column
                             metric = c(NA, lav.fit.metric["chisq.scaled"], if (isTRUE(!is.null(group))) {

                               if (isTRUE(estimator %in% c("MLM", "WLSM", "DLS"))) {

                                 stat <- lavaan::lavInspect(mod.metric.fit, what = "test")$satorra.bentler$stat.group

                               } else if(isTRUE(estimator %in% c("WLSMV", "ULSM", "MLMV", "ULSMV", "ULS"))) {

                                 stat <- lavaan::lavInspect(mod.metric.fit, what = "test")$scaled.shifted$stat.group

                               } else {

                                 stat <- lavaan::lavInspect(mod.metric.fit, what = "test")$yuan.bentler.mplus$stat.group

                               }

                               if (isTRUE(is.null(stat))) {

                                 rep(NA, times = ngroups)

                               } else {

                                 stat

                               }

                             }, lav.fit.metric[c("df.scaled", "pvalue.scaled", "chisq.scaling.factor")], NA,
                             NA, lav.fit.metric[c("cfi.scaled", "tli.scaled")], NA,
                             NA, lav.fit.metric[c("rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "rmsea.pvalue.scaled")], NA, lav.fit.metric["srmr_bentler"], NA,
                             NA, lav.fit.metric[c("aic", "bic", "bic2")]),
                             ### Fourth column
                             scalar = c(NA, lav.fit.scalar["chisq.scaled"], if (isTRUE(!is.null(group))) {

                               if (isTRUE(estimator %in% c("MLM", "WLSM", "DLS"))) {

                                 stat <- lavaan::lavInspect(mod.scalar.fit, what = "test")$satorra.bentler$stat.group

                               } else if(isTRUE(estimator %in% c("WLSMV", "ULSM", "MLMV", "ULSMV", "ULS"))) {

                                 stat <- lavaan::lavInspect(mod.scalar.fit, what = "test")$scaled.shifted$stat.group

                               } else {

                                 stat <- lavaan::lavInspect(mod.scalar.fit, what = "test")$yuan.bentler.mplus$stat.group

                               }

                               if (isTRUE(is.null(stat))) {

                                 rep(NA, times = ngroups)

                               } else {

                                 stat

                               }

                             }, lav.fit.scalar[c("df.scaled", "pvalue.scaled", "chisq.scaling.factor")], NA,
                             NA, lav.fit.scalar[c("cfi.scaled", "tli.scaled")], NA,
                             NA, lav.fit.scalar[c("rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "rmsea.pvalue.scaled")], NA, lav.fit.scalar["srmr_bentler"], NA,
                             NA, lav.fit.scalar[c("aic", "bic", "bic2")]),
                             ### Fifth column
                             dmetric = c(NA, unlist(chidiff.conf.met[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.conf.met[2L, c("Df diff", "Pr(>Chisq)")]), NA, NA,
                                         NA, lav.fit.metric[c("cfi.scaled", "tli.scaled")] - lav.fit.config[c("cfi.scaled", "tli.scaled")], NA,
                                         NA, lav.fit.metric["rmsea.scaled"] - lav.fit.config["rmsea.scaled"], NA, NA, NA, NA, lav.fit.metric["srmr_bentler"] - lav.fit.config["srmr_bentler"], NA,
                                         NA, lav.fit.metric[c("aic", "bic", "bic2")] - lav.fit.config[c("aic", "bic", "bic2")]),
                             ### Sixth column
                             dscalar = c(NA, unlist(chidiff.met.sca[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.met.sca[2L, c("Df diff", "Pr(>Chisq)")]), NA, NA,
                                         NA, lav.fit.scalar[c("cfi.scaled", "tli.scaled")] - lav.fit.metric[c("cfi.scaled", "tli.scaled")], NA,
                                         NA, lav.fit.scalar["rmsea.scaled"] - lav.fit.metric["rmsea.scaled"], NA, NA, NA, NA, lav.fit.scalar["srmr_bentler"] - lav.fit.metric["srmr_bentler"], NA,
                                         NA, lav.fit.scalar[c("aic", "bic", "bic2")] - lav.fit.metric[c("aic", "bic", "bic2")]), fix.empty.names = FALSE)

    #### Robust fit indices
    fit.robust <- data.frame(fit.robust,
                             ### Third column
                             metric = c(NA, lav.fit.metric["chisq.scaled"], if (isTRUE(!is.null(group))) {

                               if (isTRUE(estimator %in% c("MLM", "WLSM", "DLS"))) {

                                 stat <- lavaan::lavInspect(mod.metric.fit, what = "test")$satorra.bentler$stat.group

                               } else if(isTRUE(estimator %in% c("WLSMV", "ULSM", "MLMV", "ULSMV", "ULS"))) {

                                 stat <- lavaan::lavInspect(mod.metric.fit, what = "test")$scaled.shifted$stat.group

                               } else {

                                 stat <- lavaan::lavInspect(mod.metric.fit, what = "test")$yuan.bentler.mplus$stat.group

                               }

                               if (isTRUE(is.null(stat))) {

                                 rep(NA, times = ngroups)

                               } else {

                                 stat

                               }

                             }, lav.fit.metric[c("df.scaled", "pvalue.scaled", "chisq.scaling.factor")], NA,
                             NA, lav.fit.metric[c("cfi.robust", "tli.robust")], NA,
                             NA, lav.fit.metric[c("rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust", "rmsea.pvalue.robust")], NA, lav.fit.metric["srmr_bentler"], NA,
                             NA, lav.fit.metric[c("aic", "bic", "bic2")]),
                             ### Fourth column
                             scalar = c(NA, lav.fit.scalar["chisq.scaled"], if (isTRUE(!is.null(group))) {

                               if (isTRUE(estimator %in% c("MLM", "WLSM", "DLS"))) {

                                 stat <- lavaan::lavInspect(mod.scalar.fit, what = "test")$satorra.bentler$stat.group

                               } else if(isTRUE(estimator %in% c("WLSMV", "ULSM", "MLMV", "ULSMV", "ULS"))) {

                                 stat <- lavaan::lavInspect(mod.scalar.fit, what = "test")$scaled.shifted$stat.group

                               } else {

                                 stat <- lavaan::lavInspect(mod.scalar.fit, what = "test")$yuan.bentler.mplus$stat.group

                               }

                               if (isTRUE(is.null(stat))) {

                                 rep(NA, times = ngroups)

                               } else {

                                 stat

                               }

                             }, lav.fit.scalar[c("df.scaled", "pvalue.scaled", "chisq.scaling.factor")], NA,
                             NA, lav.fit.scalar[c("cfi.robust", "tli.robust")], NA,
                             NA, lav.fit.scalar[c("rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust", "rmsea.pvalue.robust")], NA, lav.fit.scalar["srmr_bentler"], NA,
                             NA, lav.fit.scalar[c("aic", "bic", "bic2")]),
                             ### Fifth column
                             dmetric = c(NA, unlist(chidiff.conf.met[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.conf.met[2L, c("Df diff", "Pr(>Chisq)")]), NA, NA,
                                         NA, lav.fit.metric[c("cfi.robust", "tli.robust")] - lav.fit.config[c("cfi.robust", "tli.robust")], NA,
                                         NA, lav.fit.metric["rmsea.robust"] - lav.fit.config["rmsea.robust"], NA, NA, NA, NA, lav.fit.metric["srmr_bentler"] - lav.fit.config["srmr_bentler"], NA,
                                         NA, lav.fit.metric[c("aic", "bic", "bic2")] - lav.fit.config[c("aic", "bic", "bic2")]),
                             ### Sixth column
                             dscalar = c(NA, unlist(chidiff.met.sca[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.met.sca[2L, c("Df diff", "Pr(>Chisq)")]), NA, NA,
                                         NA, lav.fit.scalar[c("cfi.robust", "tli.robust")] - lav.fit.metric[c("cfi.robust", "tli.robust")], NA,
                                         NA, lav.fit.scalar["rmsea.robust"] - lav.fit.metric["rmsea.robust"], NA, NA, NA, NA, lav.fit.scalar["srmr_bentler"] - lav.fit.metric["srmr_bentler"], NA,
                                         NA, lav.fit.scalar[c("aic", "bic", "bic2")] - lav.fit.metric[c("aic", "bic", "bic2")]), fix.empty.names = FALSE)

  #...................
  ### Strict invariance model ####

  }, strict = {

    #### Standard fit indices
    fit.stand <- data.frame(fit.stand,
                            ### Third column
                            metric = c(NA, lav.fit.metric["chisq"], if (isTRUE(!is.null(group))) { lavaan::lavInspect(mod.metric.fit, what = "test")$standard$stat.group }, lav.fit.metric[c("df", "pvalue")], NA,
                                       NA, lav.fit.metric[c("cfi", "tli")], NA,
                                       NA, lav.fit.metric[c("rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue")], NA, lav.fit.metric["srmr_bentler"], NA,
                                       NA, lav.fit.metric[c("aic", "bic", "bic2")]),
                            ### Fourth column
                            scalar = c(NA, lav.fit.scalar["chisq"], if (isTRUE(!is.null(group))) { lavaan::lavInspect(mod.scalar.fit, what = "test")$standard$stat.group }, lav.fit.scalar[c("df", "pvalue")], NA,
                                       NA, lav.fit.scalar[c("cfi", "tli")], NA,
                                       NA, lav.fit.scalar[c("rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue")], NA, lav.fit.scalar["srmr_bentler"], NA,
                                       NA, lav.fit.scalar[c("aic", "bic", "bic2")]),
                            ### Fifth column
                            strict = c(NA, lav.fit.strict["chisq"], if (isTRUE(!is.null(group))) { lavaan::lavInspect(mod.strict.fit, what = "test")$standard$stat.group }, lav.fit.strict[c("df", "pvalue")], NA,
                                       NA, lav.fit.strict[c("cfi", "tli")], NA,
                                       NA, lav.fit.strict[c("rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue")], NA, lav.fit.strict["srmr_bentler"], NA,
                                       NA, lav.fit.strict[c("aic", "bic", "bic2")]),
                            ### Sixth column
                            dmetric = c(NA, unlist(chidiff.conf.met[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.conf.met[2L, c("Df diff", "Pr(>Chisq)")]), NA,
                                        NA, lav.fit.metric[c("cfi", "tli")] - lav.fit.config[c("cfi", "tli")], NA,
                                        NA, lav.fit.metric["rmsea"] - lav.fit.config["rmsea"], NA, NA, NA, NA, lav.fit.metric["srmr_bentler"] - lav.fit.config["srmr_bentler"], NA,
                                        NA, lav.fit.metric[c("aic", "bic", "bic2")] - lav.fit.config[c("aic", "bic", "bic2")]),
                            ### Seventh column
                            dscalar = c(NA, unlist(chidiff.met.sca[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.met.sca[2L, c("Df diff", "Pr(>Chisq)")]), NA,
                                        NA, lav.fit.scalar[c("cfi", "tli")] - lav.fit.metric[c("cfi", "tli")], NA,
                                        NA, lav.fit.scalar["rmsea"] - lav.fit.metric["rmsea"], NA, NA, NA, NA, lav.fit.scalar["srmr_bentler"] - lav.fit.metric["srmr_bentler"], NA,
                                        NA, lav.fit.scalar[c("aic", "bic", "bic2")] - lav.fit.metric[c("aic", "bic", "bic2")]),
                            ### Eight column
                            dstrict = c(NA, unlist(chidiff.sca.str[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.sca.str[2L, c("Df diff", "Pr(>Chisq)")]), NA,
                                        NA, lav.fit.strict[c("cfi", "tli")] - lav.fit.scalar[c("cfi", "tli")], NA,
                                        NA, lav.fit.strict["rmsea"] - lav.fit.scalar["rmsea"], NA, NA, NA, NA, lav.fit.strict["srmr_bentler"] - lav.fit.scalar["srmr_bentler"], NA,
                                        NA, lav.fit.strict[c("aic", "bic", "bic2")] - lav.fit.scalar[c("aic", "bic", "bic2")]), fix.empty.names = FALSE)

    #### Scaled fit indices
    fit.scaled <- data.frame(fit.scaled,
                             ### Third column
                             metric = c(NA, lav.fit.metric["chisq.scaled"], if (isTRUE(!is.null(group))) {

                               if (isTRUE(estimator %in% c("MLM", "WLSM", "DLS"))) {

                                 stat <- lavaan::lavInspect(mod.metric.fit, what = "test")$satorra.bentler$stat.group

                               } else if(isTRUE(estimator %in% c("WLSMV", "ULSM", "MLMV", "ULSMV", "ULS"))) {

                                 stat <- lavaan::lavInspect(mod.metric.fit, what = "test")$scaled.shifted$stat.group

                               } else {

                                 stat <- lavaan::lavInspect(mod.metric.fit, what = "test")$yuan.bentler.mplus$stat.group

                               }

                               if (isTRUE(is.null(stat))) {

                                 rep(NA, times = ngroups)

                               } else {

                                 stat

                               }

                             }, lav.fit.metric[c("df.scaled", "pvalue.scaled", "chisq.scaling.factor")], NA,
                             NA, lav.fit.metric[c("cfi.scaled", "tli.scaled")], NA,
                             NA, lav.fit.metric[c("rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "rmsea.pvalue.scaled")], NA, lav.fit.metric["srmr_bentler"], NA,
                             NA, lav.fit.metric[c("aic", "bic", "bic2")]),
                             ### Fourth column
                             scalar = c(NA, lav.fit.scalar["chisq.scaled"], if (isTRUE(!is.null(group))) {

                               if (isTRUE(estimator %in% c("MLM", "WLSM", "DLS"))) {

                                 stat <- lavaan::lavInspect(mod.scalar.fit, what = "test")$satorra.bentler$stat.group

                               } else if(isTRUE(estimator %in% c("WLSMV", "ULSM", "MLMV", "ULSMV", "ULS"))) {

                                 stat <- lavaan::lavInspect(mod.scalar.fit, what = "test")$scaled.shifted$stat.group

                               } else {

                                 stat <- lavaan::lavInspect(mod.scalar.fit, what = "test")$yuan.bentler.mplus$stat.group

                               }

                               if (isTRUE(is.null(stat))) {

                                 rep(NA, times = ngroups)

                               } else {

                                 stat

                               }

                             }, lav.fit.scalar[c("df.scaled", "pvalue.scaled", "chisq.scaling.factor")], NA,
                             NA, lav.fit.scalar[c("cfi.scaled", "tli.scaled")], NA,
                             NA, lav.fit.scalar[c("rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "rmsea.pvalue.scaled")], NA, lav.fit.scalar["srmr_bentler"], NA,
                             NA, lav.fit.scalar[c("aic", "bic", "bic2")]),
                             ### Fifth column
                             strict = c(NA, lav.fit.strict["chisq.scaled"], if (isTRUE(!is.null(group))) {

                               if (isTRUE(estimator %in% c("MLM", "WLSM", "DLS"))) {

                                 stat <- lavaan::lavInspect(mod.strict.fit, what = "test")$satorra.bentler$stat.group

                               } else if(isTRUE(estimator %in% c("WLSMV", "ULSM", "MLMV", "ULSMV", "ULS"))) {

                                 stat <- lavaan::lavInspect(mod.strict.fit, what = "test")$scaled.shifted$stat.group

                               } else {

                                 stat <- lavaan::lavInspect(mod.strict.fit, what = "test")$yuan.bentler.mplus$stat.group

                               }

                               if (isTRUE(is.null(stat))) {

                                 rep(NA, times = ngroups)

                               } else {

                                 stat

                               }

                             }, lav.fit.strict[c("df.scaled", "pvalue.scaled", "chisq.scaling.factor")], NA,
                             NA, lav.fit.strict[c("cfi.scaled", "tli.scaled")], NA,
                             NA, lav.fit.strict[c("rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "rmsea.pvalue.scaled")], NA, lav.fit.strict["srmr_bentler"], NA,
                             NA, lav.fit.strict[c("aic", "bic", "bic2")]),
                             ### Sixth column
                             dmetric = c(NA, unlist(chidiff.conf.met[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.conf.met[2L, c("Df diff", "Pr(>Chisq)")]), NA, NA,
                                         NA, lav.fit.metric[c("cfi.scaled", "tli.scaled")] - lav.fit.config[c("cfi.scaled", "tli.scaled")], NA,
                                         NA, lav.fit.metric["rmsea.scaled"] - lav.fit.config["rmsea.scaled"], NA, NA, NA, NA, lav.fit.metric["srmr_bentler"] - lav.fit.config["srmr_bentler"], NA,
                                         NA, lav.fit.metric[c("aic", "bic", "bic2")] - lav.fit.config[c("aic", "bic", "bic2")]),
                             ### Seventh column
                             dscalar = c(NA, unlist(chidiff.met.sca[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.met.sca[2L, c("Df diff", "Pr(>Chisq)")]), NA, NA,
                                         NA, lav.fit.scalar[c("cfi.scaled", "tli.scaled")] - lav.fit.metric[c("cfi.scaled", "tli.scaled")], NA,
                                         NA, lav.fit.scalar["rmsea.scaled"] - lav.fit.metric["rmsea.scaled"], NA, NA, NA, NA, lav.fit.scalar["srmr_bentler"] - lav.fit.metric["srmr_bentler"], NA,
                                         NA, lav.fit.scalar[c("aic", "bic", "bic2")] - lav.fit.metric[c("aic", "bic", "bic2")]),
                             ### Eight column
                             dstrict = c(NA, unlist(chidiff.sca.str[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.sca.str[2L, c("Df diff", "Pr(>Chisq)")]), NA, NA,
                                         NA, lav.fit.strict[c("cfi.scaled", "tli.scaled")] - lav.fit.scalar[c("cfi.scaled", "tli.scaled")], NA,
                                         NA, lav.fit.strict["rmsea.scaled"] - lav.fit.scalar["rmsea.scaled"], NA, NA, NA, NA, lav.fit.strict["srmr_bentler"] - lav.fit.scalar["srmr_bentler"], NA,
                                         NA, lav.fit.strict[c("aic", "bic", "bic2")] - lav.fit.scalar[c("aic", "bic", "bic2")]), fix.empty.names = FALSE)

    #### Robust fit indices
    fit.robust <- data.frame(fit.robust,
                             ### Third column
                             metric = c(NA, lav.fit.metric["chisq.scaled"], if (isTRUE(!is.null(group))) {

                               if (isTRUE(estimator %in% c("MLM", "WLSM", "DLS"))) {

                                 stat <- lavaan::lavInspect(mod.metric.fit, what = "test")$satorra.bentler$stat.group

                               } else if(isTRUE(estimator %in% c("WLSMV", "ULSM", "MLMV", "ULSMV", "ULS"))) {

                                 stat <- lavaan::lavInspect(mod.metric.fit, what = "test")$scaled.shifted$stat.group

                               } else {

                                 stat <- lavaan::lavInspect(mod.metric.fit, what = "test")$yuan.bentler.mplus$stat.group

                               }

                               if (isTRUE(is.null(stat))) {

                                 rep(NA, times = ngroups)

                               } else {

                                 stat

                               }

                             }, lav.fit.metric[c("df.scaled", "pvalue.scaled", "chisq.scaling.factor")], NA,
                             NA, lav.fit.metric[c("cfi.robust", "tli.robust")], NA,
                             NA, lav.fit.metric[c("rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust", "rmsea.pvalue.robust")], NA, lav.fit.metric["srmr_bentler"], NA,
                             NA, lav.fit.metric[c("aic", "bic", "bic2")]),
                             ### Fourth column
                             scalar = c(NA, lav.fit.scalar["chisq.scaled"], if (isTRUE(!is.null(group))) {

                               if (isTRUE(estimator %in% c("MLM", "WLSM", "DLS"))) {

                                 stat <- lavaan::lavInspect(mod.scalar.fit, what = "test")$satorra.bentler$stat.group

                               } else if(isTRUE(estimator %in% c("WLSMV", "ULSM", "MLMV", "ULSMV", "ULS"))) {

                                 stat <- lavaan::lavInspect(mod.scalar.fit, what = "test")$scaled.shifted$stat.group

                               } else {

                                 stat <- lavaan::lavInspect(mod.scalar.fit, what = "test")$yuan.bentler.mplus$stat.group

                               }

                               if (isTRUE(is.null(stat))) {

                                 rep(NA, times = ngroups)

                               } else {

                                 stat

                               }

                             }, lav.fit.scalar[c("df.scaled", "pvalue.scaled", "chisq.scaling.factor")], NA,
                             NA, lav.fit.scalar[c("cfi.robust", "tli.robust")], NA,
                             NA, lav.fit.scalar[c("rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust", "rmsea.pvalue.robust")], NA, lav.fit.scalar["srmr_bentler"], NA,
                             NA, lav.fit.scalar[c("aic", "bic", "bic2")]),
                             ### Fifth column
                             strict = c(NA, lav.fit.strict["chisq.scaled"], if (isTRUE(!is.null(group))) {

                               if (isTRUE(estimator %in% c("MLM", "WLSM", "DLS"))) {

                                 stat <- lavaan::lavInspect(mod.strict.fit, what = "test")$satorra.bentler$stat.group

                               } else if(isTRUE(estimator %in% c("WLSMV", "ULSM", "MLMV", "ULSMV", "ULS"))) {

                                 stat <- lavaan::lavInspect(mod.strict.fit, what = "test")$scaled.shifted$stat.group

                               } else {

                                 stat <- lavaan::lavInspect(mod.strict.fit, what = "test")$yuan.bentler.mplus$stat.group

                               }

                               if (isTRUE(is.null(stat))) {

                                 rep(NA, times = ngroups)

                               } else {

                                 stat

                               }

                             }, lav.fit.strict[c("df.scaled", "pvalue.scaled", "chisq.scaling.factor")], NA,
                             NA, lav.fit.strict[c("cfi.robust", "tli.robust")], NA,
                             NA, lav.fit.strict[c("rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust", "rmsea.pvalue.robust")], NA, lav.fit.strict["srmr_bentler"], NA,
                             NA, lav.fit.strict[c("aic", "bic", "bic2")]),
                             ### Sixth column
                             dmetric = c(NA, unlist(chidiff.conf.met[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.conf.met[2L, c("Df diff", "Pr(>Chisq)")]), NA, NA,
                                         NA, lav.fit.metric[c("cfi.robust", "tli.robust")] - lav.fit.config[c("cfi.robust", "tli.robust")], NA,
                                         NA, lav.fit.metric["rmsea.robust"] - lav.fit.config["rmsea.robust"], NA, NA, NA, NA, lav.fit.metric["srmr_bentler"] - lav.fit.config["srmr_bentler"], NA,
                                         NA, lav.fit.metric[c("aic", "bic", "bic2")] - lav.fit.config[c("aic", "bic", "bic2")]),
                             ### Seventh column
                             dscalar = c(NA, unlist(chidiff.met.sca[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.met.sca[2L, c("Df diff", "Pr(>Chisq)")]), NA, NA,
                                         NA, lav.fit.scalar[c("cfi.robust", "tli.robust")] - lav.fit.metric[c("cfi.robust", "tli.robust")], NA,
                                         NA, lav.fit.scalar["rmsea.robust"] - lav.fit.metric["rmsea.robust"], NA, NA, NA, NA, lav.fit.scalar["srmr_bentler"] - lav.fit.metric["srmr_bentler"], NA,
                                         NA, lav.fit.scalar[c("aic", "bic", "bic2")] - lav.fit.metric[c("aic", "bic", "bic2")]),
                             ### Eight column
                             dstrict = c(NA, unlist(chidiff.sca.str[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.sca.str[2L, c("Df diff", "Pr(>Chisq)")]), NA, NA,
                                         NA, lav.fit.strict[c("cfi.robust", "tli.robust")] - lav.fit.scalar[c("cfi.robust", "tli.robust")], NA,
                                         NA, lav.fit.strict["rmsea.robust"] - lav.fit.scalar["rmsea.robust"], NA, NA, NA, NA, lav.fit.strict["srmr_bentler"] - lav.fit.scalar["srmr_bentler"], NA,
                                         NA, lav.fit.strict[c("aic", "bic", "bic2")] - lav.fit.scalar[c("aic", "bic", "bic2")]), fix.empty.names = FALSE)

  })

  # Remove scaled / robust fit indices and information criteria
  if (isTRUE(estimator %in% c("ML", "MLF", "GLS", "WLS", "DWLS", "ULS", "PML"))) {

    fit.scaled <- NULL
    fit.robust <- NULL

    if (isTRUE(estimator %in% c("GLS", "WLS", "DWLS", "ULS", "PML"))) {

      fit.stand <- fit.stand[-c((which(fit.stand[, 1L] == "Information Criteria") - 1L):nrow(fit.stand)), ]

    }

  }

  # Remove information criteria
  if (isTRUE(estimator %in% c("DLS", "WLSM", "WLSMV", "ULSM", "ULSMV"))) {

     fit.stand <- fit.stand[-c((which(fit.stand[, 1L] == "Information Criteria") - 1L):nrow(fit.stand)), ]
     fit.scaled <- fit.scaled[-c((which(fit.scaled[, 1L] == "Information Criteria") - 1L):nrow(fit.scaled)), ]
     fit.robust <- fit.robust[-c((which(fit.robust[, 1L] == "Information Criteria") - 1L):nrow(fit.robust)), ]

   }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Parameter estimates ####

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Function for Parameter Estimates ####

  model.fit.param <- function(model.param, long = long) {

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

    #if (isTRUE(!is.null(model))) { print.var <- print.var[match(names(model), print.var$lhs), ] }

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

    #### Latent variables

    ##### Between-group measurement invariance
    if (isTRUE(!long)) {

      print.lv <- NULL
      for (i in unique(model.param[which(model.param$param == "latent variable"), "lhs"])) {

        # Loop across groups
        for (j in unique(model.param$group)) {

          print.lv <- rbind(print.lv,
                            data.frame(param = "latent variable", group = j, lhs = i, op = "", rhs = paste(i, "=~"), label = "", est = NA, se = NA, z = NA, pvalue = NA, stdyx = NA),
                            model.param[which(model.param$param == "latent variable" & model.param$lhs == i & model.param$group == j), ])

        }

      }

    ##### Longitudinal measurement invariance
    } else {

      print.lv <- NULL
      for (i in unique(model.param[which(model.param$param == "latent variable"), "lhs"])) {

        print.lv <- rbind(print.lv,
                            data.frame(param = "latent variable", lhs = i, op = "", rhs = paste(i, "=~"), label = "", est = NA, se = NA, z = NA, pvalue = NA, stdyx = NA),
                            model.param[which(model.param$param == "latent variable" & model.param$lhs == i), ])

      }

    }

    #### Latent variable covariances

    ##### Between-group measurement invariance
    if (isTRUE(!long)) {

      print.lv.cov <- NULL
      for (i in unique(model.param[which(model.param$param == "latent variable covariance"), "lhs"])) {

        # Loop across groups
        for (j in unique(model.param$group)) {

          print.lv.cov <- rbind(print.lv.cov,
                                data.frame(param = "latent variable covariance", group = j, lhs = i, op = "", rhs = paste(i, "~~"), label = "", est = NA, se = NA, z = NA, pvalue = NA, stdyx = NA),
                                model.param[which(model.param$param == "latent variable covariance" & model.param$lhs == i & model.param$group == j), ])

        }

      }

    ##### Longitudinal measurement invariance
    } else {

      print.lv.cov <- NULL
      for (i in unique(model.param[which(model.param$param == "latent variable covariance"), "lhs"])) {

        print.lv.cov <- rbind(print.lv.cov,
                              data.frame(param = "latent variable covariance", lhs = i, op = "", rhs = paste(i, "~~"), label = "", est = NA, se = NA, z = NA, pvalue = NA, stdyx = NA),
                              model.param[which(model.param$param == "latent variable covariance" & model.param$lhs == i), ])

      }

    }

    #### Residual covariances

    ##### Between-group measurement invariance
    if (isTRUE(!long)) {

      print.res.cov <- NULL
      for (i in unique(model.param[which(model.param$param == "residual covariance"), "lhs"])) {

        # Loop across groups
        for (j in unique(model.param$group)) {

          print.res.cov <- rbind(print.res.cov,
                                 data.frame(param = "residual covariance", group = j, lhs = i, op = "", rhs = paste(i, "~~"), label = "", est = NA, se = NA, z = NA, pvalue = NA, stdyx = NA),
                                 model.param[which(model.param$param == "residual covariance" & model.param$lhs == i & model.param$group == j), ])
        }

      }

    ##### Longitudinal measurement invariance
    } else {

      print.res.cov <- NULL
      for (i in unique(model.param[which(model.param$param == "residual covariance"), "lhs"])) {

        print.res.cov <- rbind(print.res.cov,
                               data.frame(param = "residual covariance", lhs = i, op = "", rhs = paste(i, "~~"), label = "", est = NA, se = NA, z = NA, pvalue = NA, stdyx = NA),
                               model.param[which(model.param$param == "residual covariance" & model.param$lhs == i), ])

      }

    }

    #...................
    ### Merge parameter tables ####

    model.param <- data.frame(rbind(print.lv, print.lv.cov, print.res.cov,
                                    model.param[which(!model.param$param %in% c("latent variable", "latent variable covariance", "residual covariance")), ]), row.names = NULL)

    # Sort by group
    if (isTRUE(!long)) { model.param <- data.frame(misty::df.sort(model.param, group), row.names = NULL) }

    #...................
    ### Labels in parentheses ####

    model.param$label <- sapply(model.param$label, function(y) ifelse(y != "", paste0("(", y, ")"), y))

    #...................
    ### Return object ####

    return(model.param)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Parameter Estimates Tables ####

  param.metric <- param.scalar <- param.strict <- NULL

  ### Configural invariance model
  param.config <- model.fit.param(mod.config.param, long = long)

  ### Metric invariance model
  if (isTRUE(any(c("metric", "scalar", "strict") %in% invar))) { param.metric <- model.fit.param(mod.metric.param, long = long) }

  ### Scalar invariance model
  if (isTRUE(any(c("scalar", "strict") %in% invar))) { param.scalar <- model.fit.param(mod.scalar.param, long = long) }

  ### Strict invariance model
  if (isTRUE(invar == "strict")) { param.strict <- model.fit.param(mod.strict.param, long = long) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Return object ####

  object <- list(call = match.call(),
                 type = "item.invar",
                 data = x,
                 args = list(model = model, rescov = rescov, rescov.long = rescov.long,
                             long = long, cluster = cluster, invar = invar, partial = partial,
                             ident = ident, ordered = ordered, estimator = estimator,
                             missing = missing, null.model = null.model, print = print, print.fit = print.fit,
                             mod.minval = mod.minval, resid.minval = resid.minval,
                             digits = digits, p.digits = p.digits, as.na = as.na, write = write, append = append,
                             check = check, output = output),
                 model = list(config = mod.config, metric = mod.metric, scalar = mod.scalar, strict = mod.strict),
                 model.fit = list(config = mod.config.fit, metric = mod.metric.fit, scalar = mod.scalar.fit, strict = mod.strict.fit),
                 check = list(config = list(vcov = mod.config.fit.check["check.vcov"], theta = mod.config.fit.check["check.theta"], cov.lv = mod.config.fit.check["check.cov.lv"]),
                              metric = list(vcov = mod.metric.fit.check["check.vcov"], theta = mod.metric.fit.check["check.theta"], cov.lv = mod.metric.fit.check["check.cov.lv"]),
                              scalar = list(vcov = mod.scalar.fit.check["check.vcov"], theta = mod.scalar.fit.check["check.theta"], cov.lv = mod.scalar.fit.check["check.cov.lv"]),
                              strict = list(vcov = mod.strict.fit.check["check.vcov"], theta = mod.strict.fit.check["check.theta"], cov.lv = mod.strict.fit.check["check.cov.lv"])),
                 result = list(summary = unname(lavaan.summary), coverage = coverage,
                               descript = itemstat, itemfreq = itemfreq,
                               fit = list(stand = fit.stand, scaled = fit.scaled, robust = fit.robust),
                               param = list(config = param.config, metric = param.metric, scalar = param.scalar, strict = param.strict),
                               modind = list(config = mod.config.modind, metric = mod.metric.modind, scalar = mod.scalar.modind, strict = mod.strict.modind),
                               score = list(metric = mod.metric.score, scalar = mod.scalar.score, strict = mod.strict.score),
                               resid = list(config = mod.config.resid, metric = mod.metric.resid, scalar = mod.scalar.resid, strict = mod.strict.resid)))

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Write Results --------------------------------------------------------------

  if (isTRUE(!is.null(write))) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Text file ####

    if (isTRUE(grepl("\\.txt", write))) {

      # Send R output to textfile
      sink(file = write, append = ifelse(isTRUE(file.exists(write)), append, FALSE), type = "output", split = FALSE)

      if (append && isTRUE(file.exists(write))) { write("", file = write, append = TRUE) }

      # Print object
      print(object, check = FALSE)

      # Close file connection
      sink()

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Excel file ####

    } else {

      misty::write.result(object, file = write)

    }

  }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
