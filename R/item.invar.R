#' Between-Group and Longitudinal Measurement Invariance Evaluation
#'
#' This function evaluates configural, (threshold), metric, scalar, and strict
#' between-group or longitudinal (partial) measurement invariance using confirmatory
#' factor analysis with continuous or ordered categorical indicators by calling
#' the \code{cfa} function in the R package \pkg{lavaan}. Measurement invariance
#' evaluation for measurement models with ordered categorical indicators utilizes
#' the Wu and Estabrook (2016) approach to model identification and constraints
#' to investigate measurement invariance. By default, the function evaluates
#' configural, metric, and scalar measurement invariance for measurement models
#' with continuous indicators, while the function evaluates configural, threshold,
#' metric, scalar, and strict measurement invariance for measurement models with
#' ordered categorical indicators given at least four response categories for each
#' indicator by providing a table with model fit information (i.e., chi-square
#' test, fit indices based on a proper null model, and information criteria) and
#' model comparison (i.e., chi-square difference test, change in fit indices, and
#' change in information criteria). Additionally, variance-covariance coverage of
#' the data, descriptive statistics, parameter estimates, modification indices,
#' and residual correlation matrix can be requested by specifying the argument
#' \code{print}.
#'
#' @param data             a data frame. If \code{model = NULL}, confirmatory
#'                         factor analysis based on a measurement model with one
#'                         factor labeled \code{f} comprising all variables in
#'                         the data frame specified in \code{data} for evaluating
#'                         between-group measurement invariance for the grouping
#'                         variable specified in the argument \code{group} is
#'                         conducted. Longitudinal measurement invariance evaluation
#'                         can only be conducted by specifying the model using
#'                         the argument \code{model}. Note that the cluster
#'                         variable is excluded from \code{data} when specifying
#'                         \code{cluster}. If \code{model} is specified, the data
#'                         frame needs to contain all variables used in the argument
#'                         \code{model} and the cluster variable when specifying
#'                         the name of the cluster variable in the argument
#'                         \code{cluster}.
#' @param ...              an expression indicating the variable names in \code{data},
#'                         e.g., \code{item.invar(dat, x1, x2, x2, group = "group")}.
#'                         Note that the operators \code{+}, \code{-},
#'                         \code{~}, \code{:}, \code{::}, and \code{!} can also be
#'                         used to select variables, see 'Details' in the
#'                         \code{\link{df.subset}} function.
#' @param model            a character vector specifying a measurement model with
#'                         one factor, or a list of character vectors for specifying
#'                         a measurement model with more than one factor for
#'                         evaluating between-group measurement invariance when
#'                         \code{long = FALSE} or a list of character vectors for
#'                         specifying a measurement model with one factor for each
#'                         time of measurement for evaluating longitudinal
#'                         measurement invariance when specifying \code{long = TRUE}.
#'                         For example, \code{model = c("x1", "x2", "x3", "x4")}
#'                         for specifying a measurement model with one factor
#'                         labeled \code{f} comprising four indicators, or
#'                         \code{model = list(factor1 = c("x1", "x2", "x3", "x4"),
#'                         factor2 = c("x5", "x6", "x7", "x8"))} for specifying a
#'                         measurement model with two latent factors labeled
#'                         \code{factor1} and \code{factor2} each comprising four
#'                         indicators for evaluating between-group measurement
#'                         invariance, or
#'                         \code{model = list(time1 = c("ax1", "ax2", "ax3", "ax4"),
#'                         time2 = c("bx1", "bx2", "bx3", "bx4"),
#'                         time3 = c("cx1", "cx2", "cx3", "cx4"))} for specifying
#'                         a longitudinal measurement model with three time points
#'                         comprising four indicators at each time point. This
#'                         function cannot evaluate longitudinal measurement
#'                         invariance for a measurement model with more than one
#'                         factor. Note that the name of each list element is used
#'                         to label factors, i.e., all list elements need to be
#'                         named, otherwise factors are labeled with \code{"f1", "f2", "f3"}
#'                         when \code{long = FALSE} and with \code{"t1", "t2", "t3"}
#'                         when \code{long = TRUE} and so on.
#' @param group            either a character string indicating the variable name
#'                         of the grouping variable in the data frame specified
#'                         in \code{data} or a vector representing the groups for
#'                         conducting multiple-group analysis to evaluate
#'                         between-group measurement invariance.
#' @param cluster          either a character string indicating the variable name
#'                         of the cluster variable in \code{data}, or a vector
#'                         representing the nested grouping structure (i.e., group
#'                         or cluster variable) for computing scaled chi-square
#'                         test statistic that takes into account non-independence
#'                         of observations. Note that this option is not available
#'                         when evaluating measurement invariance for ordered
#'                         categorical indicators by specifying \code{ordered = TRUE).
#' @param long             logical: if \code{TRUE}, longitudinal measurement
#'                         invariance evaluation is conducted. The longitudinal
#'                         measurement model is specified by using the argument
#'                         \code{model}. Note that this function can only deal
#'                         with a measurement model with one factor at each time
#'                         point when investigating longitudinal measurement
#'                         invariance. Moreover, this function can only evaluate
#'                         either between-group or longitudinal measurement
#'                         invariance, but not both at the same time.
#' @param ordered          logical: if \code{TRUE}, all indicator variables of
#'                         the measurement model are treated as ordered categorical
#'                         variables, i.e., measurement invariance evaluation
#'                         utilizes the Wu and Estabrook (2016) approach to model
#'                         identification and constraints for investigating
#'                         measurement invariance. Note that all indicators variables
#'                         need to have the same number of response categories,
#'                         either two (binary), three (ternary), or more than
#'                         three response categories. Accordingly, zero cell
#'                         counts are not allowed, e.g., zero observations for
#'                         a response category of an indicator within a group when
#'                         investigating between-group measurement invariance or
#'                         zero observations for a response category of an indicator
#'                         at a time point when investigating longitudinal measurement
#'                         invariance.
#' @param parameterization a character string only used when treating indicators
#'                         of the measurement model as ordered categorical (i.e.,
#'                         \code{ordinal = TRUE}), i.e., \code{"delta"} (default)
#'                         for delta parameterization or \code{"theta"} for theta
#'                         parameterization.
#' @param rescov           a character vector or a list of character vectors for
#'                         specifying residual covariances, e.g., \code{rescov = c("x1", "x2")}
#'                         for specifying a residual covariance between items \code{x1}
#'                         and \code{x2}, or \code{rescov = list(c("x1", "x2"), c("x3", "x4"))}
#'                         for specifying residual covariances between items \code{x1}
#'                         and \code{x2}, and items \code{x3} and \code{x4}.
#' @param rescov.long      logical: if \code{TRUE} (default), residual covariances
#'                         between parallel indicators are estimated across time
#'                         when evaluating longitudinal measurement invariance
#'                         (\code{long = TRUE}), i.e., residual variances of the
#'                         same indicators that are measured at different time
#'                         points are correlated across all possible time points.
#'                         Note that residual covariances should be estimated even
#'                         if the parameter estimates are statistically not
#'                         significant since indicator-specific systematic variance
#'                         is likely to correlate with itself over time (Little,
#'                         2013, p. 164).
#' @param invar            a character string indicating the level of measurement
#'                         invariance to be evaluated, i.e., \code{config} to evaluate
#'                         configural measurement invariance (i.e., same factor
#'                         structure across groups or time), \code{thres} to
#'                         evaluate configural, and threshold measurement invariance
#'                         (i.e., equal item-specific threshold parameters across
#'                         group or time), \code{metric} to evaluate configural,
#'                         threshold and metric measurement invariance (i.e.,
#'                         equal factor loadings across groups or time), \code{scalar}
#'                         (default when \code{ordered = FALSE}) to evaluate
#'                         configural, threshold, metric and scalar measurement
#'                         invariance (i.e., equal intercepts across groups or
#'                         time), and \code{strict} (default when \code{ordered = TRUE})
#'                         to evaluate configural, threshold, metric, scalar, and
#'                         strict measurement invariance (i.e., equal residual
#'                         variances or scaling factors across groups or time).
#'                         Note that threshold measurement invariance is only
#'                         available when evaluating measurement invariance for
#'                         ordered categorical indicators. In this case, threshold
#'                         measurement invariance can only be investigated when
#'                         all indicators have at least four response categories.
#'                         In addition, metric measurement invariance cannot be
#'                         investigated when all indicators have only two response
#'                         categories, i.e., binary indicators.
#' @param partial          a list of character vectors and/or list(s) named
#'                         \code{thres} for freeing thresholds, \code{load} for
#'                         freeing factor loadings, \code{inter} for freeing
#'                         intercepts, and/or \code{resid} for freeing residual
#'                         variances. More specifically, a list of named character
#'                         vectors \code{load}, \code{inter}, and/or \code{resid}
#'                         are needed when investigating partial between-group
#'                         measurement invariance for continuous indicators based
#'                         on two groups (see Example 11a) or longitudinal partial
#'                         measurement invariance for continuous indicators based
#'                         on two or more than two time points (see Example 12).
#'                         However, a list of named lists \code{load}, \code{inter},
#'                         and/or \code{resid} are needed when investigating partial
#'                         between-group measurement invariance for continuous
#'                         indicators based on more than two groups (see Example
#'                         11b). A list including a list named \code{thres} of
#'                         character vectors named after the indicators for which
#'                         threshold parameters (e.g., \code{"e1"} for threshold
#'                         1) are to be freed along with named character vectors
#'                         \code{load}, \code{inter}, and/or \code{resid} are
#'                         needed when investigating partial between-group
#'                         measurement invariance for ordered categorical indicators
#'                         based on two groups (see Example 13a) or longitudinal
#'                         partial measurement invariance for ordered categorical
#'                         indicators based on two or more than two time points
#'                         (see Example 14). However, a list including a list named
#'                         \code{thres} of lists named after the indicators for
#'                         which threshold parameters are freed (e.g., \code{item1})
#'                         of named character vectors specifying the threshold
#'                         parameter along with lists named \code{load}, \code{inter},
#'                         and/or \code{resid} are needed when investigating partial
#'                         between-group measurement invariance for ordered categorical
#'                         indicators based on more than two groups (see Example 13b).
#'                         Note that at least two invariant indicators per latent
#'                         variable are needed for a partial measurement invariance
#'                         model. Otherwise there might be issues with model
#'                         non-identification.
#' @param ident            a character string indicating the method used for identifying
#'                         and scaling latent variables, i.e., \code{"marker"} for
#'                         the marker variable method fixing the first factor
#'                         loading of the latent variable to 1 and fixing the
#'                         first intercept to 0, \code{"var"} (default) for the
#'                         fixed variance method fixing the variance of the latent
#'                         variable to 1 and the latent mean to 0, or \code{"effect"}
#'                         for the effects-coding method using equality constraints
#'                         so that the average of the factor loading of the latent
#'                         variable equals 1 and the sum of intercepts equals 0.
#'                         Note that measurement invariance evaluation for ordered
#'                         categorical indicators can only be conducted based on
#'                         the fixed variance method (\code{"var"}).
#' @param estimator        a character string indicating the estimator to be used
#'                         (see 'Details' in the help page of the \code{item.cfa()}
#'                         function). By default, \code{"MLR"} is used for CFA
#'                         models with continuous indicators and \code{"WLSMV"}
#'                         is used for CFA models with ordered categorical
#'                         indicators. Note that the estimators
#'                         \code{"ML", "MLM", "MLMV", "MLMVS", "MLF"} and
#'                         \code{"MLR"} are not available when \code{ordered = TRUE}.
#' @param missing          a character string indicating how to deal with missing data,
#'                         i.e., \code{"listwise"} for listwise deletion, \code{"pairwise"}
#'                         for pairwise deletion, \code{"fiml"} for full information
#'                         maximum likelihood method, \code{"two.stage"} for two-stage
#'                         maximum likelihood method, \code{"robust.two.stage"} for robust
#'                         two-stage maximum likelihood method, and \code{"doubly-robust"}
#'                         for doubly-robust method (see 'Details' in the help page
#'                         of the\code{item.cfa()} function). By default, \code{"fiml"}
#'                         is used for CFA models with continuous indicators and
#'                         \code{"listwise"} is used for CFA models with ordered
#'                         categorical indicators given that \code{"fiml"} is not
#'                         available for a limited-information estimator used
#'                         to estimate the CFA model with ordered categorical
#'                         indicators. Note that the argument \code{missing}
#'                         switches to \code{listwise} when the data set is complete.
#'                         Also note that the robust CFI, TLI, and RMSEA are different
#'                         in complete data depending on whether FIML or listwise
#'                         deletion was specified when estimating the model in lavaan.
#' @param null.model       logical: if \code{TRUE} (default), the proper null model
#'                         for computing incremental fit indices (i.e., CFI and TLI)
#'                         is used, i.e., means and variances of the indicators are
#'                         constrained to be equal across group or time in the null
#'                         model (Little, 2013, p. 112). Note that the function does
#'                         not provide the proper null model specification when
#'                         evaluating measurement invariance for ordered categorical
#'                         indicators i.e., the argument will switch to \code{FALSE}
#'                         when specifying \code{ordered = TRUE).
#' @param print            a character string or character vector indicating which
#'                         results to show on the console, i.e. \code{"all"} for
#'                         all results, \code{"summary"} for a summary of the
#'                         specification (e.g., estimation and optimization method,
#'                         test statistic, missing data handling, and identification
#'                         method) \code{"partial"} for a summary of the partial
#'                         measurement invariance specification listing parameters
#'                         that are freely estimated when \code{partial} is not
#'                         \code{NULL}, \code{"coverage"} for the variance-covariance
#'                         coverage of the data, \code{"descript"} for descriptive
#'                         statistics for continuous variables (\code{ordered = FALSE})
#'                         and item frequencies for ordered categorical variable
#'                         (\code{ordered = TRUE}), \code{"fit"} for model fit and
#'                         model comparison, \code{"est"} for parameter estimates,
#'                         \code{"modind"} for modification indices, and \code{"resid"}
#'                         for the residual correlation matrix and standardized residual
#'                         means. By default, a summary of the specification,
#'                         the partial measurement invariance specification, and model
#'                         fit are printed. Note that parameter estimates, modification
#'                         indices, and residual correlation matrix is only provided
#'                         for the model investigating the level of measurement
#'                         invariance specified in the argument \code{"invar"}.
#' @param print.fit        a character string or character vector indicating which
#'                         version of the CFI, TLI, and RMSEA to show on the console
#'                         when using a robust estimation method involving a scaling
#'                         correction factor, i.e., \code{"all"} for all versions of
#'                         the CFI, TLI, and RMSEA, \code{"standard"} (default when
#'                         \code{estimator} is one of
#'                         \code{"ML", "MLF", "GLS", "WLS", "DWLS", "ULS", "PML"})
#'                         for fit indices without any non-normality correction,
#'                         \code{"scaled"} (default when \code{ordered = TRUE})
#'                         for population-corrected robust fit indices with ad hoc
#'                         non-normality correction, and \code{robust} (default when
#'                         \code{estimator} is one of \code{"MLM", "MLMV, "MLMVS",
#'                         "MLR", "WLSM", "WLSMV", "ULSM", "ULSMV", "DLS"}) for
#'                         sample-corrected robust fit indices based on formula
#'                         provided by Li and Bentler (2006) and Brosseau-Liard
#'                         and Savalei (2014).
#' @param mod.minval       numeric value to filter modification indices and only
#'                         show modifications with a modification index value
#'                         equal or higher than this minimum value. By default,
#'                         modification indices equal or higher 6.63 are printed.
#'                         Note that a modification index value of 6.63 is equivalent
#'                         to a significance level of \eqn{\alpha = .01}.
#' @param resid.minval     numeric value indicating the minimum absolute residual
#'                         correlation coefficients and standardized means to highlight
#'                         in boldface. By default, absolute residual correlation
#'                         coefficients and standardized means equal or higher 0.1
#'                         are highlighted. Note that highlighting can be disabled
#'                         by setting the minimum value to 1.
#' @param lavaan.run       logical: if \code{TRUE} (default), all models for
#'                         evaluating measurement invariance will be estimated by
#'                         using the \code{cfa()} function from the R package
#'                         lavaan.
#' @param se               internal argument only used in the \code{item.nonequi()}
#'                         function, this argument should never be specified.
#' @param digits           an integer value indicating the number of decimal places
#'                         to be used for displaying results. Note that information
#'                         criteria and chi-square test statistic are printed with
#'                         \code{digits} minus 1 decimal places.
#' @param p.digits         an integer value indicating the number of decimal places
#'                         to be used for displaying \emph{p}-values, covariance
#'                         coverage (i.e., \code{p.digits - 1}), and residual
#'                         correlation coefficients.
#' @param as.na            a numeric vector indicating user-defined missing values,
#'                         i.e., these values are converted to \code{NA} before
#'                         conducting the analysis. Note that \code{as.na()}
#'                         function is only applied to \code{data} but not to
#'                         \code{group} or \code{cluster}.
#' @param write            a character string naming a file for writing the output
#'                         into either a text file with file extension \code{".txt"}
#'                         (e.g., \code{"Output.txt"}) or Excel file with file
#'                         extension \code{".xlsx"}  (e.g., \code{"Output.xlsx"}).
#'                         If the file name does not contain any file extension,
#'                         an Excel file will be written.
#' @param append           logical: if \code{TRUE} (default), output will be
#'                         appended to an existing text file with extension
#'                         \code{.txt} specified in \code{write}, if \code{FALSE}
#'                         existing text file will be overwritten.
#' @param check            logical: if \code{TRUE} (default), argument specification
#'                         is checked and convergence and model identification
#'                         checks are conducted for all estimated models.
#' @param output           logical: if \code{TRUE} (default), output is shown.
#'
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{item.noninvar}}, \code{\link{item.cfa}}, \code{\link{multilevel.invar}}
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
#' Wu, H., & Estabrook, R. (2016). Identification of confirmatory factor analysis
#' models of different levels of invariance for ordered categorical outcomes.
#' \emph{Psychometrika, 81}(4), 1014–1045. doi:10.1007/s11336-016-9506-0
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
#' \item{\code{model}}{list with specified model for the for the configural
#'                     (\code{config}), threshold (\code{thresh}), metric (\code{metric}),
#'                     scalar (\code{scalar}), and strict invariance model (\code{strict})}}
#' \item{\code{model.fit}}{list with fitted lavaan object of the configural, metric,
#'                         scalar, and strict invariance model}
#' \item{\code{check}}{list with the results of the convergence and model identification
#'                     check for the configural (\code{config}), threshold (\code{thresh}),
#'                     metric (\code{metric}), scalar (\code{scalar}), and
#'                     strict invariance model (\code{strict})}
#' \item{\code{result}}{list with result tables, i.e., \code{summary} for the
#'                      summary of the specification, e.g., estimation method or
#'                      missing data handling in lavaan, \code{partial} for the
#'                      summary of the partial invariance specification, \code{coverage}
#'                      for the variance-covariance coverage of the data, \code{descript}
#'                      list with descriptive statistics (\code{stat}) and
#'                      frequencies (\code{freq}), \code{fit} for a list with
#'                      model fit based on standard, scaled, and robust fit indices,
#'                      \code{param} for a list with parameter estimates for the
#'                      configural, metric, scalar, and strict invariance model,
#'                      \code{modind} for the list with modification indices for
#'                      the configural, metric, scalar, and strict invariance model,
#'                      \code{score} for the list with result of the score tests
#'                      for constrained parameters for the threshold, metric,
#'                      scalar, and strict invariance model, and \code{resid} for
#'                      the list with residual correlation matrices and standardized
#'                      residual means for the configural, threshold, metric, scalar,
#'                      and strict invariance model}
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
#' # Between-Group Measurement Invariance: Continuous Indicators
#'
#' #..................
#' # Measurement model with one factor
#'
#' # Example 1a: Model specification using the argument '...'
#' item.invar(HolzingerSwineford1939, x1, x2, x3, x4, group = "school")
#'
#' # Example 1b: Alternative model specification without using the argument '...'
#' item.invar(HolzingerSwineford1939[, c("x1", "x2", "x3", "x4")],
#'            group = HolzingerSwineford1939$sex)
#'
#' # Example 1c: Alternative model specification without using the argument '...'
#' item.invar(HolzingerSwineford1939[, c("x1", "x2", "x3", "x4", "school")], group = "school")
#'
#' # Example 1d: Alternative model specification using the argument 'model'
#' item.invar(HolzingerSwineford1939, model = c("x1", "x2", "x3", "x4"), group = "school")
#'
#' #..................
#' # Measurement model with two factors
#'
#' # Example 2: Model specification using the argument 'model'
#' item.invar(HolzingerSwineford1939,
#'            model = list(c("x1", "x2", "x3", "x4"), c("x5", "x6", "x7", "x8")),
#'            group = "school")
#'
#' #..................
#' # Configural, metric, scalar, and strict measurement invariance
#'
#' # Example 3: Evaluate configural, metric, scalar, and strict measurement invariance
#' item.invar(HolzingerSwineford1939, model = c("x1", "x2", "x3", "x4"),
#'            group = "school", invar = "strict")
#'
#' #..................
#' # Between-group partial measurement invariance
#'
#' # Example 4a: Two Groups
#' #             Free factor loadings for 'x2' and 'x3'
#' #             Free intercept for 'x1'
#' #             Free residual variance for 'x4'
#' item.invar(HolzingerSwineford1939, model = c("x1", "x2", "x3", "x4"),
#'            group = "school", invar = "strict",
#'            partial = list(load = c("x2", "x3"),
#'                           inter = "x1",
#'                           resid = "x4"))
#'
#' # Example 4b: More than Two Groups
#' #             Free factor loading for 'x2' in group 2
#' #             Free factor loading for 'x4' in group 1 and 3
#' #             Free intercept for 'x1' in group 3
#' #             Free residual variance for 'x3' in group 1 and 3
#' item.invar(HolzingerSwineford1939, model = c("x1", "x2", "x3", "x4"),
#'            group = "ageyr", invar = "strict",
#'            partial = list(load = list(x2 = "g2", x4 = c("g1", "g3")),
#'                           inter = list(x1 = "g3"),
#'                           resid = list(x3 = c("g1", "g3"))))
#'
#' #..................
#' # Residual covariances
#'
#' # Example 5a: One residual covariance
#' item.invar(HolzingerSwineford1939, model = c("x1", "x2", "x3", "x4"),
#'            rescov = c("x3", "x4"), group = "school")
#'
#' # Example 5b: Two residual covariances
#' item.invar(HolzingerSwineford1939, model = c("x1", "x2", "x3", "x4"),
#'            rescov = list(c("x1", "x4"), c("x3", "x4")), group = "school")
#'
#' #..................
#' # Scaled test statistic
#'
#' # Example 6a: Specify cluster variable using a variable name in 'data'
#' item.invar(HolzingerSwineford1939, model = c("x1", "x2", "x3", "x4"),
#'            group = "school", cluster = "agemo")
#'
#' # Example 6b: Specify cluster variable as vector
#' item.invar(HolzingerSwineford1939, model = c("x1", "x2", "x3", "x4"),
#'            group = "school", cluster = HolzingerSwineford1939$agemo)
#'
#' #..................
#' # Default Null model
#'
#' # Example 7: Specify default null model for computing incremental fit indices
#' item.invar(HolzingerSwineford1939, model = c("x1", "x2", "x3", "x4"),
#'            group = "school", null.model = FALSE)
#'
#' #..................
#' # Print argument
#'
#' # Example 8a: Request all results
#' item.invar(HolzingerSwineford1939, model = c("x1", "x2", "x3", "x4"),
#'            group = "school", print = "all")
#'
#' # Example 8b: Request fit indices with ad hoc non-normality correction
#' item.invar(HolzingerSwineford1939, model = c("x1", "x2", "x3", "x4"),
#'            group = "school", print.fit = "scaled")
#'
#' # Example 8c: Request modification indices with value equal or higher than 2
#' # and highlight residual correlations equal or higher than 0.3
#' item.invar(HolzingerSwineford1939, model = c("x1", "x2", "x3", "x4"),
#'            group = "school", print = c("modind", "resid"),
#'            mod.minval = 2, resid.minval = 0.3)
#'
#' #..................
#' # Model syntax and lavaan summary of the estimated model
#'
#' # Example 9a: Model specification using the argument '...'
#' mod1 <- item.invar(HolzingerSwineford1939, x1, x2, x3, x4, group = "school",
#'                    output = FALSE)
#'
#' # lavaan summary of the scalar invariance model
#' lavaan::summary(mod1$model.fit$scalar, standardized = TRUE, fit.measures = TRUE)
#'
#' # Example 9b: Do not estimate any models
#' mod2 <- item.invar(HolzingerSwineford1939, x1, x2, x3, x4, group = "school",
#'                    lavaan.run = FALSE)
#'
#' # lavaan model syntax metric invariance model
#' cat(mod2$model$metric)
#'
#' # lavaan model syntax scalar invariance model
#' cat(mod2$model$scalar)
#'
#' #----------------------------------------------------------------------------
#' # Longitudinal Measurement Invariance: Continuous Indicators
#'
#' # Example 10: Two time points with three indicators at each time point
#' item.invar(HolzingerSwineford1939,
#'            model = list(c("x1", "x2", "x3"), c("x5", "x6", "x7")), long = TRUE)
#'
#' #..................
#' # Longitudinal partial measurement invariance
#'
#' # Example 11: Two Time Points with three indicators at each time point
#' #             Free factor loading for 'x2'
#' #             Free intercepts for 'x1' and x2
#' item.invar(HolzingerSwineford1939,
#'            model = list(c("x1", "x2", "x3"), c("x5", "x6", "x7")), long = TRUE,
#'            partial = list(load = "x2",
#'                           inter = c("x1", "x2")))
#'
#' #----------------------------------------------------------------------------
#' # Between-Group Measurement Invariance: Ordered Categorical Indicators
#' #
#' # Note that the example analysis for ordered categorical indicators cannot be
#' # conduct since the data set 'data' is not available.
#'
#' # Example 12a: Delta parameterization (default)
#' item.invar(data, item1, item2, item3, item4, group = "two.group", ordered = TRUE)
#'
#' # Example 12a: Theta parameterization
#' item.invar(data, item1, item2, item3, item4, group = "two.group", ordered = TRUE,
#'            parameterization = "theta")
#'
#' #----------------------------------------------------------------------------
#' # Between-Group Partial Measurement Invariance: Ordered Categorical Indicators
#'
#' # Example 13a: Two Groups
#' #              Free 2nd and 4th threshold of 'item1'
#' #              Free 1st threshold of 'item3'
#' #              Free factor loadings for 'item2' and 'item4'
#' #              Free intercept for 'item1'
#' #              Free residual variance for 'item3'
#' item.invar(data, item1, item2, item3, item4, group = "two.group", ordered = TRUE,
#'            partial = list(thres = list(item1 = c("t2", "t4"),
#'                                        item3 = "t1"),
#'                           load = c("item2", "item4"),
#'                           inter = "item1",
#'                           resid = "item3"))
#'
#' # Example 13b: More than Two Groups
#' #              Free 1st threshold of 'item1' in group 1 and 2
#' #              Free 3rd threshold of 'item3' in group 3
#' #              Free factor loadings for 'item2' in group 1
#' #              Free intercept for 'item2' in group 1
#' #              Free intercept for 'item3' in group 2 and 4
#' #              Free residual variance for 'item1' in group 1 and 3
#' item.invar(data, item1, item2, item3, item4, group = "four.group", ordered = TRUE,
#'            partial = list(thres = list(item1 = list(t1 = c("g1", "g2")),
#'                                        item3 = list(t3 = "g3")),
#'                           load  = list(item2 = "g1"),
#'                           inter = list(item2 = "g1", item3 = c("g2", "g4")),
#'                           resid = list(item1 = c("g1", "g3"))))
#'
#' #----------------------------------------------------------------------------
#' # Longitudinal Measurement Invariance: Ordered Categorical Indicators
#'
#' # Example 14: Two Time Points
#' item.invar(data, model = list(c("aitem1", "aitem2", "aitem3"),
#'                               c("bitem1", "bitem2", "bitem3")),
#'            long = TRUE, ordered = TRUE)
#'
#' #..................
#' # Longitudinal partial measurement invariance: Ordered Categorical Indicators
#'
#' # Example 15: Two Time Points
#' #             Free 2nd and 4th threshold of 'aitem1'
#' #             Free 1st threshold of 'aitem4'
#' #             Free factor loading for 'aitem2
#' #             Free intercepts for 'aitem1' and 'bitem2'
#' #             Free residual variance for 'aitem3'
#' item.invar(data, model = list(c("aitem1", "aitem2", "aitem3"),
#'                               c("bitem1", "bitem2", "bitem3")),
#'            long = TRUE, ordered = TRUE, invar = "strict",
#'            partial = list(thres = list(aitem1 = c("t2", "t4"), aitem3 = "t1"),
#'                           load = "aitem2",
#'                           inter = c("aitem1", "bitem2"),
#'                           resid = "aitem3"))
#'
#' #------------------------------------------------
#' # Write Results
#'
#' # Example 16a: Write Results into a text file
#' item.invar(HolzingerSwineford1939, model = c("x1", "x2", "x3", "x4"),
#'            group = "school", print = "all", write = "Invariance.txt", output = FALSE)
#'
#' # Example 16b: Write Results into a Excel file
#' item.invar(HolzingerSwineford1939, model = c("x1", "x2", "x3", "x4"),
#'            group = "school", print = "all", write = "Invariance.xlsx", output = FALSE)
#' }
item.invar <- function(data, ..., model = NULL, group = NULL, cluster = NULL,
                       long = FALSE, ordered = FALSE, parameterization = c("delta", "theta"),
                       rescov = NULL, rescov.long = TRUE,
                       invar = c("config", "thres", "metric", "scalar", "strict"),
                       partial = NULL, ident = c("marker", "var", "effect"),
                       estimator = c("ML", "MLM", "MLMV", "MLMVS", "MLF", "MLR",
                                     "GLS", "WLS", "DWLS", "WLSM", "WLSMV",
                                     "ULS", "ULSM", "ULSMV", "DLS", "PML"),
                       missing = c("listwise", "pairwise", "fiml", "two.stage", "robust.two.stage", "doubly.robust"),
                       null.model = TRUE, print = c("all", "summary", "partial", "coverage", "descript", "fit", "est", "modind", "resid"),
                       print.fit = c("all", "standard", "scaled", "robust"), mod.minval = 6.63, resid.minval = 0.1,
                       lavaan.run = TRUE, se = NULL, digits = 3, p.digits = 3, as.na = NULL, write = NULL, append = TRUE,
                       check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'data' is missing or NULL
  if (isTRUE(missing(data) || is.null(data))) { stop("Please specify a data frame for the argument 'data'", call. = FALSE) }

  # Check if input 'model' is a character vector or list of character vectors
  if (isTRUE(!is.null(model) && !all(sapply(model, is.character)))) { stop("Please specify a character vector or list of character vectors for the argument 'model'.", call. = FALSE) }

  # Check if 'group' or 'long' is specified
  if (isTRUE((is.null(group) && !long) || (!is.null(group) && long))) { stop("Please specify the argument 'group' to evaluate between-group measurement invariance or the argument 'long' to evaluate longitudinal measurement invariance.", call. = FALSE) }

  # Check if 'model' is specified when evaluating longitudinal measurement invariance
  if (isTRUE((long && is.null(model)) || (long && !is.list(model)))) { stop("Please specify a list of character vectors for the argument 'model' to evaluate longitudinal measurement invariance.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data using the argument '...' ####

  if (isTRUE(!missing(...))) {

    # Extract data and convert tibble into a data frame
    x <- data[, .var.names(data = data, ..., group = group, cluster = cluster)] |> (\(p) if (isTRUE("tbl" %in% substr(class(p), 1L, 3L))) { as.data.frame(p) } else { return(p) })()

    # Grouping variable
    if (isTRUE(!is.null(group))) { group <- data[, group] }

    # Cluster variable
    if (isTRUE(!is.null(cluster))) { cluster <- data[, cluster] }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data without using the argument '...' ####

  } else {

    # Convert 'data' as tibble into a data frame
    x <- data |> (\(p) if (isTRUE("tbl" %in% substr(class(p), 1L, 3L))) { as.data.frame(p) } else { return(p) })()

    # Data and cluster
    var.group <- .var.group(data = x, group = group, cluster = cluster)

    # Data
    if (isTRUE(!is.null(var.group$data))) { x <- var.group$data }

    # Grouping variable
    if (isTRUE(!is.null(var.group$group))) { group <- var.group$group }

    # Cluster variable
    if (isTRUE(!is.null(var.group$cluster))) { cluster <- var.group$cluster }

  }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Check Inputs ####

  .check.input(logical = c("ordered", "long", "rescov.long", "null.model", "append", "output"),
               numeric = list(mod.minval = 1L, resid.minval = 1L),
               s.character = list(parameterization = c("delta", "theta"), invar = c("config", "thres", "metric", "scalar", "strict"), ident = c("marker", "var", "effect"), estimator = c("ML", "MLM", "MLMV", "MLMVS", "MLF", "MLR", "GLS", "WLS", "DWLS", "WLSM", "WLSMV", "ULS", "ULSM", "ULSMV", "DLS", "PML"), missing = c("listwise", "pairwise", "fiml", "two.stage", "robust.two.stage", "doubly.robust")),
               m.character = list(print = c("all", "summary", "partial", "coverage", "descript", "fit", "est", "modind", "resid"), print.fit = c("all", "standard", "scaled", "robust")),
               args = c("digits", "p.digits", "write2"), package = "lavaan", envir = environment(), input.check = check)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Additional Checks ####

  if (isTRUE(check)) {

    #--------------------------------------
    ### Check input 'data' ####

    if (isTRUE(is.null(model))) {

      # No cluster or grouping variable in 'data'
      if (isTRUE((is.null(cluster) || length(cluster) != 1L) && (is.null(group) || length(group) != 1L))) {

        if (isTRUE(ncol(data.frame(x)) < 3L)) { stop("Please specify at least three indicators for the measurement model in 'data'.", call. = FALSE) }

      # Cluster or grouping variable in 'data'
      } else if (isTRUE( length(cluster) == 1L || length(group) == 1L)) {

        if (isTRUE(ncol(data.frame(x)[, !colnames(data.frame(x)) %in% c(cluster, group), drop = FALSE]) < 3L)) { stop("Please specify at least three indicators for the measurement model in 'data'.", call. = FALSE) }

      }

    #--------------------------------------
    ### Check input 'model' ####

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

    #--------------------------------------
    ### Check input 'rescov' ####

    if (isTRUE(!is.null(rescov))) {

      # More than one residual covariance specified as list
      if (isTRUE(is.list(rescov))) {

        if (isTRUE(any(sapply(rescov, length) != 2L))) { stop("Please specify a list of character vectors, each with two variable names, for the argument 'rescov'.", call. = FALSE) }

      # One residual covariance specified as vector
      } else {

        if (isTRUE(length(rescov) != 2L)) { stop("Please specify a character vector with two variable names, for the argument 'rescov'", call. = FALSE) }

      }

      # Model specification without 'model'
      if (isTRUE(is.null(model))) {

        (!unique(unlist(rescov)) %in% colnames(x)) |> (\(y) if (isTRUE(any(y))) { stop(paste0("Items specified in the argument 'rescov' were not found in 'data': ", paste(unique(unlist(rescov))[y], collapse = ", ")), call. = FALSE) })()

      # Model specification with 'model'
      } else {

        (!unique(unlist(rescov)) %in% unique(unlist(model))) |> (\(y) if (isTRUE(any(y))) { stop(paste0("Items specified in the argument 'rescov' were not found in 'model': ", paste(unique(unlist(rescov))[y], collapse = ", ")), call. = FALSE) })()

      }

    }

    #--------------------------------------
    ### Check input 'group' ####

    if (isTRUE(!is.null(group))) {

      # Name of the grouping variable in 'data'
      if (isTRUE(length(group) == 1L)) {

        # One grouping variable
        if (isTRUE(!is.character(group))) { stop("Please specify a character string for the name of the grouping variable in 'data'", call. = FALSE) }

        # Grouping variable in 'data'
        if (isTRUE(!group %in% colnames(x))) { stop(paste0("Grouping variable \"", group, "\" specified in 'group' was not found in 'data'"), call. = FALSE) }

      # Group variable
      } else {

        # Length of grouping variable
        if (isTRUE(nrow(x) != length(group))) { stop("The grouping variable does not match with the number of rows in 'data'.",call. = FALSE) }

        # Number of groups
        if (isTRUE(length(unique(na.omit(group))) == 1L)) { stop("There is only one group represented in the grouping variable 'group'.", call. = FALSE) }

      }

    }

    # Argument 'long' or 'group'
    if (isTRUE(long && !is.null(group))) { stop("Please specify the arguments for evaluating either between-group or longitudinal measurement invariance.", call. = FALSE) }

    #--------------------------------------
    ### Check Input 'model' when long = TRUE ####

    if (isTRUE(long)) {

      # Model argument is NULL or not a list
      if (isTRUE(is.null(model) || !is.list(model))) { stop("Please specify the measurement model for evaluating longitudinal measurement invariance using the argument 'model'.", call. = FALSE) }

      # Not the same number of item across time
      if (isTRUE(length(unique(sapply(model, length))) != 1L)) { stop("Please specify the same number of items measured at each time point in the argument 'model'.", call. = FALSE) }

    }

    #--------------------------------------
    ### Check Input 'cluster' ####

    if (isTRUE(!is.null(cluster))) {

      # Name of the cluster variable in 'data'
      if (isTRUE(length(cluster) == 1L)) {

        # One cluster variable
        if (isTRUE(!is.character(cluster))) { stop("Please specify a character string for the name of the cluster variable in 'data'", call. = FALSE) }

        # Cluster variable in 'x'
        if (isTRUE(!cluster %in% colnames(x))) { stop(paste0("Cluster variable \"", cluster, "\" specified in 'cluster' was not found in 'data'"), call. = FALSE) }

      # Cluster variable
      } else {

        # Length of cluster variable
        if (isTRUE(nrow(x) != length(cluster))) { stop("The cluster variable does not match with the number of rows in 'data'.",call. = FALSE) }

        # Number of groups
        if (isTRUE(length(unique(na.omit(cluster))) == 1L)) { stop("There is only one group represented in the cluster variable 'cluster'.", call. = FALSE) }

      }

    }

    #--------------------------------------
    ### Check Input 'partial' ####

    if (isTRUE(!is.null(partial))) { if (isTRUE(!is.list(partial))) { stop("Please specify a list for the argument 'partial'.", call. = FALSE) } }

    #--------------------------------------
    ### Check Input 'estimator' ####

    if (isTRUE(ordered)) {

      if (isTRUE(!all(c("ML", "MLM", "MLMV", "MLMVS", "MLF", "MLR", "GLS", "WLS", "DWLS", "WLSM", "WLSMV", "ULS", "ULSM", "ULSMV", "DLS", "PML") %in% estimator))) {

        if (isTRUE(estimator %in% c("ML", "MLM", "MLMV", "MLMVS", "MLF", "MLR"))) { stop(paste0("The estimator \"", estimator, "\" is not available when ordered = TRUE."), call. = FALSE) }

      }

    }

  }

  #_____________________________________________________________________________
  #
  # Data and Arguments ---------------------------------------------------------

  # Visible binding for global variable
  .group <- idvar <- ind <- thres <- NULL

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Manifest Variables ####

  #--------------------------------------
  ### Model Specification with 'data' or with 'model' ####

  if (isTRUE(is.null(model))) { var.mod <- colnames(x) } else { var.mod <- unique(unlist(model)) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data Frame with Cluster and Grouping Variable ####

  #--------------------------------------
  ### No Cluster Variable ####

  if (isTRUE(is.null(cluster))) {

    # Grouping variable or no grouping variable
    if (isTRUE(!is.null(group))) { x <- data.frame(x[, var.mod], .group = group) |> (\(y) misty::df.sort(y, .group))() } else if (isTRUE(is.null(group))) { x <- data.frame(x[, var.mod]) }

  #--------------------------------------
  ### Cluster variable ####

  } else {

    # Grouping variable or no grouping variable
    if (isTRUE(!is.null(group))) { x <- data.frame(x[, var.mod], .group = group, .cluster = cluster) } else if (isTRUE(is.null(group))) { x <- data.frame(x[, var.mod], .cluster = cluster) }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Number of Groups ####

  if (isTRUE(!is.null(group))) { ngroups <- length(unique(na.omit(x$.group))) } else { ngroups <- 1L }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert User-Missing Values into NA ####

  if (isTRUE(!is.null(as.na))) { x[, var.mod] <- .as.na(x[, var.mod], na = as.na) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Check Variance within Groups ##

  if (isTRUE(!long)) { sapply(split(x, f = x$.group), function(y) apply(y[, var.mod], 2L, var, na.rm = TRUE)) |> (\(y) if (isTRUE(any(y == 0L | is.na(y)))) { stop(paste0("There is no variance in group ", paste(names(which(apply(y, 2L, function(y) any(y == 0L | is.na(y))))), collapse = ", "), " for following variable: ", paste(names(which(apply(y, 1L, function(y) any(y == 0L | is.na(y))))), collapse = ", ")), call. = FALSE) })() }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model ####

  # Specification with the argument 'model'
  if (isTRUE(!is.null(model) && is.list(model))) {

    # No factor labels
    if (isTRUE(is.null(names(model)))) {

      # Factor labels for between-group measurement invariance
      if (isTRUE(!long)) {

        names(model) <- paste0("f", seq_along(model))

      # Factor labels for within-group measurement invariance
      } else {

        names(model) <- paste0("t", seq_along(model))

      }

    }

    lv.label <- names(model)

  # Specification without the argument 'model'
  } else {

    lv.label <- "f"

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Parameterization ####

  if (isTRUE(all(c("delta", "theta") %in% parameterization))) { parameterization <- "delta" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Residual Covariance ####

  if (isTRUE(!is.null(rescov) && !is.list(rescov))) { rescov <- list(rescov) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Level of Invariance ####

  # Default setting: "scalar" for continuous indicators and "strict" for ordered categorical indicators
  if (isTRUE(ordered)) {

    if (isTRUE(all(c("config", "thres", "metric", "scalar", "strict") %in% invar))) { invar <- "strict" }

  } else {

    if (isTRUE(all(c("config", "thres", "metric", "scalar", "strict") %in% invar))) { invar <- "scalar" }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model Identification ####

  #--------------------------------------
  ### Argument 'ident' ####

  # Default setting: Fixed Factor Method
  if (isTRUE(ordered)) {

    ident <- "var"

  } else {

    if (isTRUE(all(c("marker", "var", "effect") %in% ident))) { ident <- "var" } else if (isTRUE(length(ident) != 1L)) { stop("Please specify a character string for the argument 'ident'.", call. = FALSE) }

  }

  #--------------------------------------
  ### Arguments 'std.lv' and 'effect.coding' ####

  # Marker variable method
  if (isTRUE(ident == "marker")) {

    std.lv <- FALSE
    effect.coding <- FALSE

  # Fixed variance method
  } else if (isTRUE(ident == "var")) {

    std.lv <- TRUE
    effect.coding <- FALSE

  # Effects coding method
  } else if (isTRUE(ident == "effect")) {

    std.lv <- FALSE
    effect.coding <- TRUE

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Estimator ####

  #--------------------------------------
  ### Default Setting ####

  if (isTRUE(all(c("ML", "MLM", "MLMV", "MLMVS", "MLF", "MLR", "GLS", "WLS", "DWLS", "WLSM", "WLSMV", "ULS", "ULSM", "ULSMV", "DLS", "PML") %in% estimator))) {

    # Continuous indicators: MLR
    if (isTRUE(!ordered)) {

      if (isTRUE(all(c("ML", "MLM", "MLMV", "MLMVS", "MLF", "MLR", "GLS", "WLS", "DWLS", "WLSM", "WLSMV", "ULS", "ULSM", "ULSMV", "DLS", "PML") %in% estimator ))) { estimator  <- "MLR" }

    # Categorical indicators: WLSMV
    } else {

      if (isTRUE(all(c("ML", "MLM", "MLMV", "MLMVS", "MLF", "MLR", "GLS", "WLS", "DWLS", "WLSM", "WLSMV", "ULS", "ULSM", "ULSMV", "DLS", "PML") %in% estimator))) { estimator  <- "WLSMV" }

      # Cluster-robust standard errors
      if (isTRUE(!is.null(cluster))) { stop("Cluster-robust standard errors are not available with ordere categorical indicators.", call. = FALSE) }

    }

  #--------------------------------------
  ### User-Specified Setting ####

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

          warning("Estimator switched to \"WLSMV\" to deal with ordered categorical indicators.", call. = FALSE)

          estimator <- "WLSMV"

        }

      } else {

        stop("Cluster-robust standard errors are not available with ordered categorical indicators.", call. = FALSE)

      }

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Missing ####

  #--------------------------------------
  ### Any Missing Values ####

  if (isTRUE(any(is.na(x[, var.mod])))) {

    complete <- FALSE

    #### Default Setting ####

    if (isTRUE(all(c("listwise", "pairwise", "fiml", "two.stage", "robust.two.stage", "doubly.robust") %in% missing))) {

      if (isTRUE(estimator %in% c("ML", "MLF", "MLR")))  {

        missing <- "fiml"

      } else if (isTRUE(estimator %in% c("MLM", "MLMV", "MLMVS", "GLS", "WLS")))  {

        missing <- "listwise"

      } else if (isTRUE(estimator %in% c("DWLS", "WLSM", "WLSMV", "ULS", "ULSM", "ULSMV", "DLS", "PML")))  {

        missing <- "pairwise"

      }

    #### User-Specified Setting ####

    } else {

      # FIML
      if (isTRUE(missing == "fiml" && !estimator %in% c("ML", "MLF", "MLR"))) {

        warning(paste0("FIML method is not available for estimator = \"", estimator, "\", argument 'missing' switched to ", ifelse(estimator %in% c("MLM", "MLMV", "MLMVS", "GLS", "WLS", "DLS"), "\"listwise\"", "\"pairwise\""), "."), call. = FALSE)

        missing <- ifelse(estimator %in% c("MLM", "MLMV", "MLMVS", "GLS", "WLS", "DLS"), "listwise", "pairwise")

      }

      # Pairwise deletion
      if (isTRUE(missing == "pairwise" && !estimator %in% c("ML", "WLS", "DWLS", "WLSM", "WLSMV", "ULS", "ULSM", "ULSMV", "PML"))) {

        warning(paste0("Pairwise deletion is not available for estimator = \"", estimator, "\", argument 'missing' switched to ", ifelse(estimator %in% c("MLF", "MLR"), "\"fiml\"", "\"listwise\""), "."), call. = FALSE)

        missing <- ifelse(estimator %in% c("MLF", "MLR"), "fiml", "listwise")

      }

      # (Robust) Two-stage
      if (isTRUE(missing %in% c("two.stage", "robust.two.stage") && !estimator %in% c("ML", "MLF", "MLR"))) {

        warning(paste0("Two-stage method is not available for estimator = \"", estimator, "\", argument 'missing' switched to ", ifelse(estimator %in% c("WLS", "DWLS", "WLSM", "WLSMV", "ULS", "ULSM", "PML"), "\"pairwise\"", "\"listwise\""), "."), call. = FALSE)

        missing <- ifelse(estimator %in% c("WLS", "DWLS", "WLSM", "WLSMV", "ULS", "ULSM", "PML"), "pairwise", "listwise")

      }

      # Doubly-robust
      if (isTRUE(missing == "doubly.robust" && estimator != "PML")) {

        warning(paste0("Doubly-robust method is not available for estimator = \"", estimator, "\", argument 'missing' switched to ", ifelse(estimator %in% c("ML", "MLF", "MLR"), "fiml\"", ifelse(estimator %in% c("MLM", "MLMV", "MLMVS", "GLS", "WLS"), "\"listwise\"", "\"pairwise\"")), "."), call. = FALSE)

        missing <- ifelse(estimator %in% c("ML", "MLF", "MLR"), "fiml", ifelse(estimator %in% c("MLM", "MLMV", "MLMVS", "GLS", "WLS"), "listwise", "pairwise"))

      }

    }

  #--------------------------------------
  ### No Missing Values ####

  } else {

    complete <- TRUE
    missing <- "listwise"

  }

  # Cases with missing on all variables
  if (isTRUE(missing %in% c("fiml", "two.stage", "robust.two.stage"))) { misty::na.prop(x[, var.mod], append = FALSE) |> (\(y) if (any(y == 1L)) { warning(paste("Data set contains", sum(y == 1L), "cases with missing on all variables which were removed from the analysis."), call. = FALSE) })() }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Missing Data on the Cluster Variable ####

  if (isTRUE(".cluster" %in% colnames(x) && any(is.na(x$.cluster)))) {

    warning(paste0("Data contains missing values on the cluster variable, number of cases removed from the analysis: ", sum(is.na(x$.cluster))), call. = FALSE)

    x <- x[!is.na(x$.cluster), ]

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Proper Null Model ####

  if (isTRUE(ordered)) { null.model <- FALSE }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Print ####

  # Default Setting
  if (isTRUE(all(c("all", "summary", "partial", "coverage", "descript", "fit", "est", "modind", "resid") %in% print))) {

    print  <- c("summary", "partial", "fit")

  # User-Specified Setting
  } else if (isTRUE(length(print) == 1L && "all" %in% print)) {

    print <- c("all", "summary", "partial", "coverage", "descript", "fit", "est", "modind", "resid")

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Print Fit ####

  if (isTRUE(all(c("all", "standard", "scaled", "robust") %in% print.fit))) {

    if (isTRUE(!ordered)) { print.fit <- "robust" } else { print.fit <- "scaled" }

  } else if (isTRUE(length(print.fit) == 1L && "all" %in% print.fit)) {

    if (isTRUE(estimator == "ML")) { print.fit <- "standard" } else { print.fit <- c("standard", "scaled", "robust") }

  }

  # Standard fit measures
  if (isTRUE(estimator %in% c("ML", "MLMVS", "MLF", "GLS", "WLS", "DWLS", "ULS", "PML"))) { print.fit <- "standard" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Not Estimate Models ####

  if (isTRUE(!lavaan.run)) { output <- FALSE; write <- NULL }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Covariance Coverage ####

  coverage <- NULL

  if (isTRUE("coverage" %in% print)) {

    ### Grouping variable
    if (isTRUE(is.null(group))) {

      coverage <- suppressWarnings(misty::na.coverage(x[, var.mod], output = FALSE)$result)

    ### No grouping variable
    } else {

      coverage <- suppressWarnings(lapply(split(x[, var.mod], f = x[, ".group"]), function(y) misty::na.coverage(y, output = FALSE)$result))

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Sample Statistics ####

  itemstat <- itemfreq <- NULL

  if (isTRUE("descript" %in% print)) {

    #--------------------------------------
    ### No grouping variable

    if (isTRUE(is.null(group))) {

      # Descriptive statistics
      itemstat <- suppressWarnings(misty::descript(x[, var.mod], output = FALSE)$result[, c("variable", "n", "nNA", "pNA", "m", "sd", "min", "max", "skew", "kurt")])

      # Frequency table
      itemfreq <- suppressWarnings(misty::freq(x[, var.mod], val.col = TRUE, exclude = 9999, output = FALSE)$result$freq)

    #--------------------------------------
    ### Grouping variable

    } else {

      # Descriptive statistics
      itemstat <- suppressWarnings(misty::descript(x[, var.mod], group = x[, ".group"], output = FALSE)$result[, c("group", "variable", "n", "nNA", "pNA", "m", "sd", "min", "max", "skew", "kurt")])

      # Frequency table
      itemfreq <- suppressWarnings(lapply(split(x[, var.mod], f = x[, ".group"]), function(y) misty::freq(y, val.col = TRUE, exclude = 9999, output = FALSE)$result$freq))

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model Specification for Models with Continuous Indicators ####

  if (isTRUE(!ordered)) {

    mod.thres <- mod.config <- mod.metric <- mod.scalar <- mod.strict <- NULL

    #--------------------------------------
    ### Between-Group Measurement Invariance ####

    if (isTRUE(!long)) {

      #......................................
      #### Model Syntax: Factor Loadings, Intercepts, and Residual Variances ####

      ##### Factor Loadings: Configural and Metric Measurement Invariance

      # Model specification with 'data'
      if (isTRUE(is.null(model) || !is.list(model))) {

        # Configural
        mod.load.config <- paste("# Factor loadings\nf =~", paste(var.mod, collapse = " + "))

        # Metric
        mod.load.metric <- paste0("# Equal factor loadings across groups\n", paste("f =~", paste(sapply(var.mod, function(y) paste0("c(", paste(rep(paste0(y, ".L"), times = ngroups), collapse = ", "),  ")*", y)  ), collapse = " + ")))

      # Model specification with 'model'
      } else if (isTRUE(!is.null(model) && is.list(model))) {

        # Configural
        mod.load.config <- paste0("# Factor loadings\n", paste(sapply(seq_along(model), function(y) paste(names(model)[[y]], "=~", paste0(model[[y]], collapse = " + "))), collapse = " \n"), collapse = "")

        # Metric
        mod.load.metric <- paste0("# Equal factor loadings across groups\n", paste0(sapply(seq_along(model), function(y) paste(names(model)[[y]], " =~", paste(sapply(model[[y]], function(z) paste0("c(", paste(rep(paste0(z, ".L"), times = ngroups), collapse = ", "),  ")*", z)  ), collapse = " + "))), collapse = "\n"), collapse = "\n")

      }

      ##### Intercepts: Scalar Measurement Invariance

      mod.inter.scalar <- paste0("# Equal intercepts across groups\n", paste0(sapply(var.mod, function(y) paste0(y, " ~ ", paste0("c(", paste(rep(paste0(y, ".I"), times = ngroups), collapse = ", "),  ")*1", collapse = "")  )), collapse = " \n"))

      ##### Residual Variances: Strict Measurement Invariance

      mod.resid.strict <- paste0("# Equal residual variances across groups\n", paste0(sapply(var.mod, function(y) paste0(y, " ~~ ", paste0("c(", paste(rep(paste0(y, ".R"), times = ngroups), collapse = ", "),  ")*", y, collapse = "")  )), collapse = " \n"))

      #......................................
      #### Partial Measurement Invariance ####

      if (isTRUE(!is.null(partial))) {

        ##### Check Partial Invariance Specification
        if (isTRUE(check)) {

          ####### Two groups
          if (isTRUE(ngroups == 2L)) {

            # List with character vector
            if (isTRUE(any(sapply(partial, function(y) !is.character(y))))) { stop("Please specify a list with character vectors named 'load', 'inter, and 'resid' for the argument 'partial'.", call. = FALSE) }

            # List elements
            if (isTRUE(any(!names(partial) %in% c("load", "inter", "resid")))) { stop("Please name the character vectors within the list specified in the argument 'partial' with 'load', 'inter', and 'resid'.", call. = FALSE) }

            # Duplicated character vector
            if (isTRUE(any(duplicated(names(partial))))) { stop(paste0("Duplicated character vector names within the list specified in the argument 'partial': ", paste0(names(partial)[duplicated(names(partial))], collapse = ", ")), call. = FALSE) }

            # Variable names
            if (isTRUE(any(!unlist(partial) %in% var.mod))) { stop(paste0("Variables specified in the argument 'partial' are not all specified in the model: ", paste0(unlist(partial)[(!unlist(partial) %in% var.mod)], collapse = ", ")), call. = FALSE) }

            # Duplicated variable names
            if (isTRUE(any(sapply(partial, function(y) any(duplicated(y)))))) { stop(paste0("Duplicated variable names in the character vectors within the list specified in the argument 'partial': ", paste0(names(which(sapply(partial, function(y) any(duplicated(y))))), collapse = ", ")), call. = FALSE) }

          ####### More than two groups
          } else {

            # List with character vector
            if (isTRUE(any(sapply(partial, function(y) !is.list(y))))) { stop("Please specify a list of lists named 'load', 'inter, and 'resid' for the argument 'partial'.", call. = FALSE) }

            # List elements
            if (isTRUE(any(!names(partial) %in% c("load", "inter", "resid")))) { stop("Please name the lists within the list specified in the argument 'partial' with 'load', 'inter', and 'resid'.", call. = FALSE) }

            # Duplicated list
            if (isTRUE(any(duplicated(names(partial))))) { stop(paste0("Duplicated list names within the list specified in the argument 'partial': ", paste0(names(partial)[duplicated(names(partial))], collapse = ", ")), call. = FALSE) }

            # Variable names
            if (isTRUE(any(!unlist(sapply(partial, names)) %in% var.mod))) { stop(paste0("Variables specified in the argument 'partial' are not all specified in the model: ", paste0(unlist(sapply(partial, names)) |> (\(p) p[!p %in% var.mod])(), collapse = ", ")), call. = FALSE) }

            # Duplicated variable names
            if (isTRUE(any(sapply(lapply(partial, names), function(y) any(duplicated(y)))))) { stop(paste0("Duplicated variable names in lists within the list specified in the argument 'partial': ", paste0(names(which(sapply(lapply(partial, names), function(y) any(duplicated(y))))), collapse = ", ")), call. = FALSE) }

            # Character vector
            if (isTRUE(any(unlist(lapply(partial, function(y) lapply(y, function(z) !is.character(z))))))) { stop(paste0("Please specify character vectors with the elements ", paste0(sapply(paste0("g", seq_len(ngroups)), function(z) paste0("\"", z, "\"")) , collapse = ", "), " for the 'load', 'inter' and 'resid' lists for the argument 'partial'."), call. = FALSE)  }

            # Duplicated group labels
            if (isTRUE(any(sapply(partial, function(y) any(sapply(y, function(z) any(duplicated(z)))))))) { stop(paste0("Duplicated group labels in lists within the list specified in the argument 'partial': ", paste0(names(which(sapply(partial, function(y) any(sapply(y, function(z) any(duplicated(z))))))), collapse = ", ")), call. = FALSE) }

            # Groups labels
            unlist(sapply(partial, function(y) unlist(y))) |> (\(p) { if (isTRUE(any(!p %in% paste0("g", seq_len(ngroups))))) { stop(paste0("Group labels specified in the argument 'partial' do not all exist given the ", ngroups, " groups ", paste0(sapply(paste0("g", seq_len(ngroups)), function(z) paste0("\"", z, "\"")) , collapse = ", "), collapse = ", ", "."), call. = FALSE) }})()

          }

        }

        ##### Two Groups
        if (isTRUE(ngroups == 2L)) {

          # Split partial invariance specification
          partial.load  <- if (isTRUE("load"  %in% names(partial))) { partial[["load"]]  } else { NULL }
          partial.inter <- if (isTRUE("inter" %in% names(partial))) { partial[["inter"]] } else { NULL }
          partial.resid <- if (isTRUE("resid" %in% names(partial))) { partial[["resid"]] } else { NULL }

          ###### Factor loadings
          if (isTRUE(!is.null(partial.load))) {

            # Marker variable method: Loading of the first indicator variable
            if (isTRUE(ident == "marker")) {

              if (isTRUE(is.null(model) || !is.list(model))) {

                if (isTRUE(var.mod[1L] %in% partial.load)) { stop(paste0("In the marker variable method, factor loading of the first indicator variable ", var.mod[1L], " is fixed to 1."), call. = FALSE) }

              } else {

                if (isTRUE(any(sapply(model, function(y) y[1L]) %in% partial.load))) { stop(paste0("In the marker variable method, factor loadings of the first indicator variable ", paste0(sapply(model, function(y) y[1L]), collapse = " and "), " are fixed to 1."), call. = FALSE) }

              }

            }

            # Free parameters constraints
            mod.load.metric <- misty::chr.gsub(sapply(partial.load, function(y) paste0("\\c\\(", paste(rep(paste0(y, ".L"), times = ngroups), collapse = ", "),  "\\)\\*")), replacement = sapply(partial.load, function(y) paste0("\\c\\(NA, NA\\)\\*")), mod.load.metric)

          }

          ###### Intercepts
          if (isTRUE(!is.null(partial.inter))) {

            # Marker variable method: Intercept of the first indicator variable
            if (isTRUE(ident == "marker")) {

              if (isTRUE(is.null(model) || !is.list(model))) {

                if (isTRUE(var.mod[1L] %in% partial.inter)) { stop(paste0("In the marker variable method, intercept of the first indicator variable ", var.mod[1L], " is fixed to 0."), call. = FALSE) }

              } else {

                if (isTRUE(any(sapply(model, function(y) y[1L]) %in% partial.inter))) { stop(paste0("In the marker variable method, intercepts of the first indicator variable ", paste0(sapply(model, function(y) y[1L]), collapse = " and "), " are fixed to 0."), call. = FALSE) }

              }

            }

            # Free parameters constraints
            mod.inter.scalar <- misty::chr.gsub(sapply(partial.inter, function(y) paste0("\\c\\(", paste(rep(paste0(y, ".I"), times = ngroups), collapse = ", "),  "\\)\\*")), replacement = sapply(partial.inter, function(y) paste0("\\c\\(NA, NA\\)\\*")), mod.inter.scalar)

          }

          ###### Residuals
          if (isTRUE(!is.null(partial.resid))) { mod.resid.strict <- misty::chr.gsub(sapply(partial.resid, function(y) paste0("\\c\\(", paste(rep(paste0(y, ".R"), times = ngroups), collapse = ", "),  "\\)\\*")), replacement = sapply(partial.resid, function(y) paste0("\\c\\(NA, NA\\)\\*")), mod.resid.strict) }

        ##### More than Two Groups
        } else {

          # Split partial invariance specification
          partial.load  <- if (isTRUE("load"  %in% names(partial))) { partial[["load"]]  } else { NULL }
          partial.inter <- if (isTRUE("inter" %in% names(partial))) { partial[["inter"]] } else { NULL }
          partial.resid <- if (isTRUE("resid" %in% names(partial))) { partial[["resid"]] } else { NULL }

          ###### Factor loadings
          if (isTRUE(!is.null(partial.load))) {

            invisible(sapply(names(partial.load), function(y) misty::chr.omit(misty::chr.trim(unlist(strsplit(mod.load.metric, paste0(y, ".L")))), omit = ",", check = FALSE) |>
                               (\(p) mod.load.metric <<- paste0(c(p[1L], paste0(sapply(paste0("g", seq_len(ngroups)), function(w) ifelse(w %in% partial.load[[y]], paste0(y, ".L.", w), paste0(y, ".L"))), collapse = ", "),  p[2L]), collapse = ""))()))

          }

          ###### Intercepts
          if (isTRUE(!is.null(partial.inter))) {

            # Marker variable method: Intercept of the first indicator variable
            if (isTRUE(ident == "marker")) {

              if (isTRUE(is.null(model) || !is.list(model))) {

                if (isTRUE(var.mod[1L] %in% unlist(partial.inter))) { stop(paste0("In the marker variable method, the intercept of the first indicator variable ", var.mod[1L], " is fixed to 0."), call. = FALSE) }

              } else {

                if (isTRUE(any(sapply(model, function(y) y[1L]) %in% unlist(partial.inter)))) { stop(paste0("In the marker variable method, intercepts of the first indicator variable ", paste0(sapply(model, function(y) y[1L]), collapse = " and "), " are fixed to 0."), call. = FALSE) }

              }

            }

            invisible(sapply(names(partial.inter), function(y) misty::chr.omit(misty::chr.trim(unlist(strsplit(mod.inter.scalar, paste0(y, ".I")))), omit = ",", check = FALSE) |>
                               (\(p) mod.inter.scalar <<- paste0(c(p[1L], paste0(sapply(paste0("g", seq_len(ngroups)), function(w) ifelse(w %in% partial.inter[[y]], paste0(y, ".I.", w), paste0(y, ".I"))), collapse = ", "),  p[2L]), collapse = ""))()))

          }

          ###### Residuals
          if (isTRUE(!is.null(partial.resid))) {

            invisible(sapply(names(partial.resid), function(y) misty::chr.omit(misty::chr.trim(unlist(strsplit(mod.resid.strict, paste0(y, ".R")))), omit = ",", check = FALSE) |>
                              (\(p) mod.resid.strict <<- paste0(c(p[1L], paste0(sapply(paste0("g", seq_len(ngroups)), function(w) ifelse(w %in% partial.resid[[y]], paste0(y, ".R.", w), paste0(y, ".R"))), collapse = ", "),  p[2L]), collapse = ""))()))

          }

        }

      }

      #......................................
      #### Model Specification: Configural Measurement Invariance ####

      ##### Marker variable method
      if (isTRUE(ident == "marker")) {

        # One factor model
        if (isTRUE(is.null(model) || !is.list(model) || (is.list(model) && length(model) == 1L))) {

          # Fix intercept of first indicator to 0 and estimate latent means
          mod.config <- paste0(mod.load.config, "\n",
                               "# Estimate latent means\n",
                               paste0("f ~ c(", paste0(rep("NA", times = ngroups), collapse = ", "), ")*1"), "\n",
                               "# Fix first intercept in each group to 0\n",
                               paste0(var.mod[1L], paste0(" ~ c(", paste0(rep("0", times = ngroups), collapse = ", "), ")*1", collapse = "")))

        # Multiple factor model
        } else {

          # Fix intercept of first indicator to 0 and estimate latent means
          mod.config <- paste0(mod.load.config, "\n",
                               "# Estimate latent means\n",
                               paste0(sapply(lv.label, function(y) paste0(y, " ~ c(", paste0(rep("NA", times = ngroups), collapse = ", "), ")*1")), collapse = "\n"),
                               "\n# Fix first intercept in each group to 0\n",
                               paste0(sapply(lv.label, function(y) paste0(model[[y]][1L], " ~ c(", paste0(rep("0", times = ngroups), collapse = ", "), ")*1")), collapse = "\n"))

        }

      ##### Fixed factor or effects coding method
      } else {

        mod.config <- mod.load.config

      }

      #......................................
      #### Model Specification: Metric Measurement Invariance ####
      if (isTRUE(any(c("metric", "scalar", "strict") %in% invar))) {

        ##### Marker variable method
        switch (ident, marker = {

          # One factor model
          if (isTRUE(is.null(model) || !is.list(model) || (is.list(model) && length(model) == 1L))) {

            # Fix intercept of first indicator to 0 and estimate latent mean
            mod.metric <- paste0(mod.load.metric, "\n",
                                 "# Estimate latent means\n",
                                 paste0("f ~ c(", paste0(rep("NA", times = ngroups), collapse = ", "), ")*1"), "\n",
                                 "# Fix first intercept in each group to 0\n",
                                 paste0(var.mod[1L], paste0(" ~ c(", paste0(rep("0", times = ngroups), collapse = ", "), ")*1", collapse = "")))

          # Multiple factor model
          } else {

            # Fix intercept of first indicators at 0 and estimate latent means
            mod.metric <- paste0(mod.load.metric, "\n",
                                 "# Estimate latent means\n",
                                 paste0(sapply(lv.label, function(y) paste0(y, " ~ c(", paste0(rep("NA", times = ngroups), collapse = ", "), ")*1")), collapse = "\n"),
                                 "\n# Fix first intercept in each group to 0\n",
                                 paste0(sapply(lv.label, function(y) paste0(model[[y]][1L], " ~ c(", paste0(rep("0", times = ngroups), collapse = ", "), ")*1")), collapse = "\n"))

          }

        ##### Fixed factor method
        }, var = {

          # One factor model
          if (isTRUE(is.null(model) || !is.list(model) || (is.list(model) && length(model) == 1L))) {

            # Fix factor variance in the first group to 1, but estimate parameter in all other groups
            mod.metric <- paste0(mod.load.metric, "\n",
                                 "# Estimate latent variances except in the first group\n",
                                 paste0("f ~~ c(1, ", paste(rep(NA, times = ngroups - 1L), collapse = ", ") ,  ")", "*f"))

          # Multiple factor model
          } else {

            # Fix factor variance in the first group at 1, but estimate parameter in all other groups
            mod.metric <- paste0(mod.load.metric, "\n",
                                 "# Estimate latent variances except in the first group\n",
                                 paste0(sapply(lv.label, function(y) paste0(y, " ~~ c(1, ", paste(rep(NA, times = ngroups - 1L), collapse = ", ") ,  ")", "*", y)), collapse = "\n"))

          }

        ##### Effects coding method
        }, effect = {

          mod.metric <- mod.load.metric

        })

      }

      #......................................
      #### Model Specification: Scalar Measurement Invariance ####
      if (isTRUE(any(c("scalar", "strict") %in% invar))) {

        ##### Marker variable method
        switch (ident, marker = {

          # One factor model
          if (isTRUE(is.null(model) || !is.list(model) || (is.list(model) && length(model) == 1L))) {

            # Constrain intercepts to be equal across groups (except the first intercept)
            mod.scalar <- paste0(mod.metric, "\n",
                                 "# Equal intercepts across groups\n",
                                 paste0(unlist(strsplit(mod.inter.scalar, "\n"))[-c(1L, 2L)], collapse = "\n"))

          # Multiple factor model
          } else {

            # Constrain intercepts to be equal across groups except the first intercept
            mod.scalar <- misty::chr.gsub(sapply(names(model), function(y) paste0(model[[y]][1L], " ~ c(", paste0(rep("0", times = ngroups), collapse = ", "), ")*1")),
                                          replacement = rep("", times = length(model)),
                                          paste0(mod.metric, "\n",
                                                 "# Equal intercepts across groups\n",
                                                 paste0(unlist(strsplit(mod.inter.scalar, "\n"))[-1L], collapse = "\n")))

          }

        ##### Fixed factor method
        }, var = {

          # One factor model
          if (isTRUE(is.null(model) || !is.list(model) || (is.list(model) && length(model) == 1L))) {

            # Constrain intercepts to be equal across groups and estimate latent means (except the first group)
            mod.scalar <- paste0(mod.load.metric, "\n",
                                 "# Estimate latent means except in the first group\n",
                                 paste0("f ~ c(0, ", paste(rep(NA, times = ngroups - 1L), collapse = ", ") ,  ")", "*", "1"), "\n",
                                 "# Estimate latent variances except in the first group\n",
                                 paste0("f ~~ c(1, ", paste(rep(NA, times = ngroups - 1L), collapse = ", ") ,  ")", "*f"), "\n",
                                 mod.inter.scalar)

          # Multiple factor model
          } else {

            # Constrain intercepts to be equal across groups and estimate latent means (except the first group)
            mod.scalar <- paste0(mod.load.metric, "\n",
                                 "# Estimate latent means except in the first group\n",
                                 paste0(sapply(lv.label, function(y) paste0(y, " ~ c(0, ", paste(rep(NA, times = ngroups - 1L), collapse = ", ") ,  ")", "*1")), collapse = "\n"), "\n",
                                 "# Estimate latent variances except in the first group\n",
                                 paste0(sapply(lv.label, function(y) paste0(y, " ~~ c(1, ", paste(rep(NA, times = ngroups - 1L), collapse = ", ") ,  ")", "*", y)), collapse = "\n"), "\n",
                                 mod.inter.scalar)

          }

        ##### Effects coding method
        }, effect = {

          mod.scalar <- paste0(mod.metric, "\n", mod.inter.scalar)

        })

      }

      #...................
      #### Strict Measurement Invariance ####
      if (isTRUE("strict" %in% invar)) {

        mod.strict <- paste0(mod.scalar, "\n", mod.resid.strict)

      }

    #--------------------------------------
    ### Longitudinal Measurement Invariance ####

    } else {

      #......................................
      #### Model Syntax: Factor Loadings, Intercepts, Residual Variances, and Longitudinal Residual Covariances ####

      ##### Factor Loadings: Configural and Metric Measurement Invariance

      # Configural
      mod.load.config <- paste0("# Factor loadings\n", paste(sapply(seq_along(model), function(y) paste(names(model)[[y]], "=~", paste0(model[[y]], collapse = " + "))), collapse = " \n"))

      # Metric
      mod.load.metric <- paste0("# Equal factor loadings across time\n", paste(sapply(seq_along(model), function(y) paste(names(model)[[y]], "=~", paste0("L", seq_along(model[[y]]), "*",  model[[y]], collapse = " + "))), collapse = " \n"), collapse = "\n")

      ##### Intercepts: Scalar Measurement Invariance

      mod.inter.scalar <- paste0("# Equal intercepts across time\n", paste(sapply(seq_along(model), function(y) paste(model[[y]], "~", paste0("I", seq_along(model[[y]]), "*1"))), collapse = " \n"), collapse = "\n")

      ##### Residual Variances: Strict Measurement Invariance

      mod.resid.strict <- paste0("# Equal residual variances across time\n", paste(sapply(seq_along(model), function(y) paste(model[[y]], "~~", paste0("R", seq_along(model[[y]]), "*", model[[y]]))), collapse = " \n"), collapse = "\n")

      #......................................
      #### Partial Measurement Invariance ####

      if (isTRUE(!is.null(partial))) {

        ##### Check Partial Invariance Specification
        if (isTRUE(check)) {

          # List with character vector
          if (isTRUE(any(sapply(partial, function(y) !is.character(y))))) { stop("Please specify a list with character vectors named 'load', 'inter, and 'resid' for the argument 'partial'.", call. = FALSE) }

          # List elements
          if (isTRUE(any(!names(partial) %in% c("load", "inter", "resid")))) { stop("Please name the character vectors within the list specified in the argument 'partial' with 'load', 'inter', and 'resid'.", call. = FALSE) }

          # Duplicated character vector
          if (isTRUE(any(duplicated(names(partial))))) { stop(paste0("Duplicated character vector names within the list specified in the argument 'partial': ", paste0(names(partial)[duplicated(names(partial))], collapse = ", ")), call. = FALSE) }

          # Variable names
          if (isTRUE(any(!unlist(partial) %in% var.mod))) { stop(paste0("Variables specified in the argument 'partial' are not all specified in the model: ", paste0(unlist(partial)[(!unlist(partial) %in% var.mod)], collapse = ", ")), call. = FALSE) }

          # Duplicated variable names
          if (isTRUE(any(sapply(partial, function(y) any(duplicated(y)))))) { stop(paste0("Duplicated variable names in the character vectors within the list specified in the argument 'partial': ", paste0(names(which(sapply(partial, function(y) any(duplicated(y))))), collapse = ", ")), call. = FALSE) }

        }

        ##### Factor loadings
        if (isTRUE(any(names(partial) == "load"))) {

          # Marker variable method: Loading of the first indicator variable
          if (isTRUE(ident == "marker")) { if (isTRUE(any(sapply(model, function(y) y[1L]) %in% partial$load))) { stop(paste0("In the marker variable method, factor loadings of the first indicator variable ", paste0(sapply(model, function(y) y[1L]), collapse = " and "), " are fixed to 1."), call. = FALSE) } }

          invisible(sapply(partial[["load"]], function(y) mod.load.metric <<- sub(paste0("L", unlist(lapply(model, function(z) which(z %in% y))), "\\*", y), paste0("NA*", y), mod.load.metric)))

        }

        ##### Intercepts
        if (isTRUE(any(names(partial) == "inter"))) {

          # Marker variable method: Loading of the first indicator variable
          if (isTRUE(ident == "marker")) { if (isTRUE(any(sapply(model, function(y) y[1L]) %in% partial$inter))) { stop(paste0("In the marker variable method, intercepts of the first indicator variable ", paste0(sapply(model, function(y) y[1L]), collapse = " and "), " are fixed to 0."), call. = FALSE) } }

          invisible(sapply(partial[["inter"]], function(y) mod.inter.scalar <<- sub(paste0(y, " ~ I", unlist(lapply(model, function(z) which(z %in% y))), "\\*1"), paste0(y, " ~ NA*1"), mod.inter.scalar)))

        }

        ##### Residuals
        if (isTRUE(any(names(partial) == "resid"))) { invisible(sapply(partial[["resid"]], function(y) mod.resid.strict <<- sub(paste0("R", unlist(lapply(model, function(z) which(z %in% y))), "\\*", y), paste0("NA*", y), mod.resid.strict))) }

      }

      #......................................
      #### Model Specification: Configural Measurement Invariance ####

      ##### Marker variable method
      if (isTRUE(ident == "marker")) {

        # Fix intercept of first indicators at 0 and estimate latent means
        mod.config <- paste0(mod.load.config, "\n",
                             "# Estimate latent means\n",
                             paste0(sapply(names(model), function(y) paste0(y, " ~ NA*1")), collapse = "\n"), "\n",
                             "# Fix first intercept at each time point to 0\n",
                             paste0(sapply(model, function(y) paste0(y[1L], " ~ 0*1")), collapse = "\n"))

      ##### Fixed factor or effects coding method
      } else {

        mod.config <- mod.load.config

      }

      #......................................
      #### Model Specification: Metric Measurement Invariance ####
      if (isTRUE(any(c("metric", "scalar", "strict") %in% invar))) {

        ##### Marker variable method
        switch (ident, marker = {

          # Fix intercept of first indicators to 0 and estimate latent means
          mod.metric <- paste0(mod.load.metric, "\n",
                               "# Estimate latent means\n",
                               paste0(sapply(names(model), function(y) paste0(y, " ~ NA*1")), collapse = "\n"), "\n",
                               "# Fix first intercept at each time point to 0\n",
                               paste0(sapply(model, function(y) paste0(y[1L], " ~ 0*1")), collapse = "\n"))

        ##### Fixed factor method
        }, var = {

          # Fix factor variance in the first group to 1, but estimate parameters at all other time points
          mod.metric <- paste0(mod.load.metric, "\n",
                               "# Estimate latent variances except at the first time point\n",
                               paste0(sapply(seq_along(names(model)), function(y) if (isTRUE(y == 1L)) { paste0(names(model)[y], " ~~ 1*", names(model)[y]) } else { paste0(names(model)[y], " ~~ NA*", names(model)[y]) }), collapse = " \n"))

        ##### Effects coding method
        }, effect =  {

          mod.metric <- mod.load.metric

        })

      }

      #......................................
      #### Model Specification: Scalar Measurement Invariance ####
      if (isTRUE(any(c("scalar", "strict") %in% invar))) {

        ##### Marker variable method
        switch (ident, marker = {

          # Constrain intercepts to be equal across time except at the first time point
          mod.scalar <- paste0(mod.load.metric, "\n",
                               "# Estimate latent means\n",
                               paste0(sapply(names(model), function(y) paste0(y, " ~ NA*1")), collapse = "\n"), "\n",
                               "# Fix first intercept at each time point to 0\n",
                               paste0(sapply(model, function(y) paste0(y[1L], " ~ 0*1")), collapse = "\n"), "\n",
                               paste(unlist(strsplit(mod.inter.scalar, "\n"))[-sapply(model, function(y) grep(y[1L], unlist(strsplit(mod.inter.scalar, "\n"))))], collapse = "\n"))

        ##### Fixed factor method
        }, var = {

          # Constrain intercepts to be equal across time and estimate latent means except at the first time point
          mod.scalar <- paste0(mod.load.metric, "\n",
                               "# Estimate latent means except at the first time point\n",
                               paste0(sapply(seq_along(names(model)), function(y) if (isTRUE(y == 1L)) { paste0(names(model)[y], " ~ 0*1") } else { paste0(names(model)[y], " ~ NA*1") }), collapse = " \n"), "\n",
                               "# Estimate latent variances except at the first time point\n",
                               paste0(sapply(seq_along(names(model)), function(y) if (isTRUE(y == 1L)) { paste0(names(model)[y], " ~~ 1*", names(model)[y]) } else { paste0(names(model)[y], " ~~ NA*", names(model)[y]) }), collapse = " \n"), "\n",
                               mod.inter.scalar)

        ##### Effects coding method
        }, effect = {

          mod.scalar <- paste0(mod.metric, "\n", mod.inter.scalar)

        })

      }

      #......................................
      #### Model Specification: Strict Measurement Invariance ####
      if (isTRUE("strict" %in% invar)) {

        mod.strict <- paste0(mod.scalar, "\n", mod.resid.strict)

      }

      #......................................
      #### Longitudinal Residual Covariances between Parallel Indicators ####

      if (isTRUE(rescov.long)) {

        mod.rescov.long <- paste0("# Longitudinal residual covariances\n", paste(apply(matrix(unlist(model), nrow = length(model), byrow = TRUE), 2L, function(y) apply(combn(y, m = 2L), 2L, function(z) paste0(z[1L], " ~~ ", z[2L]))), collapse = "\n"))

        if (isTRUE(!is.null(mod.config))) { mod.config <- paste0(mod.config, "\n", mod.rescov.long) }

        if (isTRUE(!is.null(mod.metric))) { mod.metric <- paste0(mod.metric, "\n", mod.rescov.long) }

        if (isTRUE(!is.null(mod.scalar))) { mod.scalar <- paste0(mod.scalar, "\n", mod.rescov.long) }

        if (isTRUE(!is.null(mod.strict))) { mod.strict <- paste0(mod.strict, "\n", mod.rescov.long) }

      }

    }

    #--------------------------------------
    ### Proper Null Model ####

    if (isTRUE(null.model)) {

      #......................................
      #### Between-Group Measurement Invariance ####

      if (isTRUE(!long)) {

        mod.null <- paste0(# Covariances
                         c(apply(combn(var.mod, m = 2L), 2L, function(y) paste0(y[1L], " ~~ 0*", y[2L])),
                           # Intercepts
                           sapply(seq_along(var.mod), function(y) paste0(var.mod[y], " ~ T", y, "*1")),
                           # Variance
                           sapply(seq_along(var.mod), function(y) paste0(var.mod[y], " ~~ V", y, "*", var.mod[y]))), collapse = "\n")

      #......................................
      #### Longitudinal Measurement Invariance ####

      } else {

        mod.null <- paste0(# Covariances
                         c(apply(combn(var.mod, m = 2L), 2L, function(y) paste0(y[1L], " ~~ 0*", y[2L])),
                           # Intercepts
                           sapply(seq_along(model), function(y) paste0(model[[y]], " ~ T", seq_along(model[[y]]),  "*1")),
                           # Variance
                           sapply(seq_along(model), function(y) paste0(model[[y]], " ~~ V", seq_along(model[[y]]),  "*", model[[y]]))), collapse = "\n")

      }

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model Specification for Models with Ordered Categorical Indicators ####

  } else {

    mod.thres <- mod.config <- mod.metric <- mod.scalar <- mod.strict <- NULL

    #--------------------------------------
    ### Response Categories: Binary, Ternary, or Multiple ####

    #...................
    #### Between-Group Measurement Invariance ####
    if (!long) {

      resp.cat <- suppressWarnings(lapply(split(x[, var.mod], f = x[, ".group"]), function(y) misty::freq(y, exclude = 9999, output = FALSE)$result$freq |> (\(p) p[-which(is.na(p[, 1L])), ])())) |>
        ##### Binary response categories
        (\(p) if (isTRUE(all(unlist(lapply(p, function(z) nrow(z) == 2L))))) {

          if (isTRUE(misty::uniq.n(unlist(lapply(p, function(z) z$Value))) != 2L)) {

            # Print frequency table
            print(lapply(split(x[, var.mod], f = x[, ".group"]), function(y) misty::freq(y, exclude = 9999, output = FALSE)))

            stop("Please specify the same binary coding for each indicator variable within each group, e.g., 0 and 1.", call. = FALSE)

          } else { return("binary") }

        ##### Ternary response categories
        } else if (isTRUE(all(unlist(lapply(p, function(z) nrow(z) == 3L))))) {

          if (isTRUE(misty::uniq.n(unlist(lapply(p, function(z) z$Value))) != 3L)) {

            # Print frequency table
            print(lapply(split(x[, var.mod], f = x[, ".group"]), function(y) misty::freq(y, exclude = 9999, output = FALSE)))

            stop("Please specify the same ternary coding of consecutive integer values for each indicator variable within each group, e.g., 1, 2, and 3.", call. = FALSE)

          } else { return("ternary") }

        ##### Multiple or more than three response categories
        } else if (isTRUE(all(sapply(p, function(z) nrow(z) > 3L)))) {

          if (isTRUE(any(sapply(p, function(z) any(!unique(unlist(sapply(p, function(w) w$Value))) %in% z$Value))) || any(sapply(p, function(z) any(z[, -1] == 0L))))) {

            # Print frequency table
            print(lapply(split(x[, var.mod], f = x[, ".group"]), function(y) misty::freq(y, exclude = 9999, output = FALSE)))

            stop("Please specify the same coding of consecutive integer values for each indicator variable within each group, e.g., 1, 2, 3, and 4.", call. = FALSE)

          } else { return("multiple") }

        ##### Not the same number of response categories within each group
        } else {

          # Print frequency table
          print(lapply(split(x[, var.mod], f = x[, ".group"]), function(y) misty::freq(y, exclude = 9999, output = FALSE)))

          # All indicators with the same number of response categories
          stop("All indicator variables need to have the same number of binary, ternary, or multiple response categories with each group.", call. = FALSE)

        })()

    #...................
    #### Longitudinal Measurement Invariance ####
    } else {

      resp.cat <- lapply(x[, var.mod], function(y) misty::uniq(y, output = FALSE)$result) |> (\(p) if (isTRUE(all(sapply(p, function(z) length(z) == 2L)))) {

          # Binary response categories
          if (isTRUE(misty::uniq.n(unique(unlist(p))) != 2L)) {

            # Print frequency table
            print(misty::freq(x[, var.mod], exclude = 9999, output = FALSE))

            stop("Please specify the same binary coding for each indicator variable, e.g., 0 and 1.", call. = FALSE)

          } else { return("binary") }

        } else if (isTRUE(all(sapply(p, function(z) length(z) == 3L)))) {

          # Ternary response categories
          if (isTRUE(misty::uniq.n(unique(unlist(p))) != 3L)) {

            # Print frequency table
            print(misty::freq(x[, var.mod], exclude = 9999, output = FALSE))

            stop("Please specify the same ternary coding of consecutive integer values for each indicator variable, e.g., 1, 2, and 3.", call. = FALSE)

          } else { return("ternary") }

        } else if (isTRUE(all(sapply(p, function(z) length(z) > 3L)))) {

          # Multiple or more than three response categories
          if (isTRUE(any(sapply(p, misty::uniq.n) !=  misty::uniq.n(unique(unlist(p)))))) {

            # Print frequency table
            print(misty::freq(x[, var.mod], exclude = 9999, output = FALSE))

            stop("Please specify the same coding of consecutive integer values for each indicator variable, e.g., 1, 2, 3, and 4.", call. = FALSE)

          } else { return("multiple") }

        }  else if (isTRUE(any(sapply(p, function(z) length(z) < 2L)))) {

          # Print frequency table
          print(misty::freq(x[, var.mod], exclude = 9999, output = FALSE))

          # Indicator variables with only unique response
          stop(paste0("Please remove indicator variables with only one unique response: ", paste0(names(which(sapply(p, function(z) length(z) < 2L))), collapse = ", ")), call. = FALSE)

        } else {

          # Print frequency table
          print(misty::freq(x[, var.mod], exclude = 9999, output = FALSE))

          # All indicators with the same number of response categories
          stop("All indicator variables need to have the same number of binary, ternary, or multiple response categories.", call. = FALSE)

        })()

    }

    # Number of response categories
    nresp.cat <- misty::uniq.n(x[, var.mod[1L]])

    #--------------------------------------
    ### Between-Group Measurement Invariance ####

    if (isTRUE(!long)) {

      #...................
      #### Model Syntax: Factor Loadings, Intercepts, and Residual Variances ####

      ##### Factor Loadings: Configural and Metric Measurement Invariance

      # Model specification with 'data'
      if (isTRUE(is.null(model) || !is.list(model))) {

        # Configural
        mod.load.config <- paste("# Factor loadings\nf =~", paste0(var.mod, collapse = " + "))

        # Metric
        mod.load.metric <- paste0("# Equal factor loadings across groups\n", paste("f =~", paste0(sapply(var.mod, function(y) paste0("c(", paste0(rep(paste0(y, ".L"), times = ngroups), collapse = ", "),  ")*", y)  ), collapse = " + ")))

      # Model specification with 'model'
      } else if (isTRUE(!is.null(model) && is.list(model))) {

        # Configural
        mod.load.config <- paste0("# Factor loadings\n", paste(sapply(seq_along(model), function(y) paste0(names(model)[[y]], " =~ ", paste0(model[[y]], collapse = " + "))), collapse = " \n"), collapse = "")

        # Metric
        mod.load.metric <- paste0("# Equal factor loadings across groups\n", paste0(sapply(seq_along(model), function(y) paste0(names(model)[[y]], " =~ ", paste(sapply(model[[y]], function(z) paste0("c(", paste(rep(paste0(z, ".L"), times = ngroups), collapse = ", "),  ")*", z)  ), collapse = " + "))), collapse = "\n"), collapse = "\n")

      }

      ##### Thresholds: Threshold Measurement Invariance

      # Thresholds freely estimated across groups
      mod.thres.free <- paste0("# Thresholds\n", paste0(sapply(var.mod, function(y) paste0(y, " | ", paste0(sapply(seq_len(nresp.cat - 1L), function(z) paste0("t", z)), collapse = " + "))), collapse = "\n"))

      # Thresholds constrained to be equal across groups
      mod.thres.fixed <- paste0("# Equal thresholds across groups\n", paste0(sapply(var.mod, function(y) paste0(y, " | ", paste0(sapply(seq_len(nresp.cat - 1L), function(z) paste0("c(", paste0(rep(paste0(y, ".T", z), times = ngroups), collapse = ", "), ")*t", z)), collapse = " + "))), collapse = "\n"))

      ##### Intercepts: Scalar Measurement Invariance

      # Intercepts fixed to 0 in all groups
      mod.inter.fixed <- mod.inter.fixed.config <- paste0("# Fix intercepts to 0 across groups\n", paste0(sapply(var.mod, function(y) paste0(y, " ~ ", paste0("c(", paste(rep("0", times = ngroups), collapse = ", "),  ")*1", collapse = "")  )), collapse = " \n"))

      # Intercepts freely estimated in all but the first group
      mod.inter.free <- paste0("# Fix intercepts in the first group to 0\n", paste0(sapply(var.mod, function(y) paste0(y, " ~ ", paste0("c(0, ", paste(rep("NA", times = ngroups - 1L), collapse = ", "),  ")*1", collapse = "")  )), collapse = " \n"))

      ##### Residual Variances and Scaling Factors: Strict Measurement Invariance

      # Delta parameterization
      mod.resid.delta.fixed <- mod.resid.delta.fixed.config <- paste0("# Fix scaling factors to 1 across groups\n", paste0(sapply(var.mod, function(y) paste0(y, " ~*~ ", paste0("c(", paste(rep("1", times = ngroups), collapse = ", "),  ")*", y, collapse = "")  )), collapse = " \n"))
      mod.resid.delta.free <- paste0("# Fix scaling factors in the first group to 1\n", paste0(sapply(var.mod, function(y) paste0(y, " ~*~ ", paste0("c(1, ", paste(rep(NA, times = ngroups - 1L), collapse = ", "),  ")*", y, collapse = "")  )), collapse = " \n"))

      # Theta parameterization
      mod.resid.theta.fixed <- mod.resid.theta.fixed.config <- paste0("# Fix residual variances to 1 across groups\n", paste0(sapply(var.mod, function(y) paste0(y, " ~~ ", paste0("c(", paste(rep("1", times = ngroups), collapse = ", "),  ")*", y, collapse = "")  )), collapse = " \n"))
      mod.resid.theta.free <- paste0("# Fix residual variances in the first group to 1\n", paste0(sapply(var.mod, function(y) paste0(y, " ~~ ", paste0("c(1, ", paste(rep(NA, times = ngroups - 1L), collapse = ", "),  ")*", y, collapse = "")  )), collapse = " \n"))

      ##### Latent Means

      mod.mean.fixed <- paste0("# Fix latent means to 0 across groups\n", paste0(sapply(lv.label, function(y) paste0(y, " ~ c(", paste(rep("0", times = ngroups), collapse = ", "),  ")*1", collapse = "")), collapse = "\n"))
      mod.mean.free <- paste0("# Estimate latent means except in the first group\n", paste0(sapply(seq_along(lv.label), function(y) paste0(lv.label[[y]], " ~ c(", paste0(sapply(seq_len(ngroups), function(z) if (isTRUE(z == 1L)) { "0" } else { "NA" }), collapse = ", "), ")*1")), collapse = "\n"))

      ##### Latent Variances

      mod.var.fixed <- paste0("# Fix latent variances to 1 across groups\n", paste0(sapply(lv.label, function(y) paste0(y, " ~~ c(", paste(rep("1", times = ngroups), collapse = ", "),  ")*", y, collapse = "")), collapse = "\n"))
      mod.var.free <- paste0("# Estimate latent variances except in the first group\n", paste0(sapply(seq_along(lv.label), function(y) paste0(lv.label[[y]], " ~~ c(", paste0(sapply(seq_len(ngroups), function(z) if (isTRUE(z == 1L)) { "1" } else { "NA" }), collapse = ", "), ")*", lv.label[[y]])), collapse = "\n"))

      #...................
      #### Partial Measurement Invariance ####

      if (isTRUE(!is.null(partial))) {

        ##### Check Partial Invariance Specification
        if (isTRUE(check)) {

          ####### Two groups
          if (isTRUE(ngroups == 2L)) {

            # List elements
            if (isTRUE(any(!names(partial) %in% c("thres", "load", "inter", "resid")))) { stop("Please name the list and character vectors within the list specified in the argument 'partial' with 'thres', 'load', 'inter', and 'resid'.", call. = FALSE) }

            # Duplicated names
            if (isTRUE(any(duplicated(names(partial))))) { stop(paste0("Duplicated element names within the list specified in the argument 'partial': ", paste0(names(partial)[duplicated(names(partial))], collapse = ", ")), call. = FALSE) }

            # Threshold
            if (isTRUE("thres" %in% names(partial))) {

              # List
              if (isTRUE(!is.list(partial[["thres"]]))) { stop("Please specify a list for the element 'thres' specified in the list for the argument 'partial.", call. = FALSE) }

              partial[["thres"]] |>
                (\(p) {
                  # Character vector
                  if (isTRUE(any(sapply(p, class) != "character"))) { stop("Please specify named character vectors for the list element 'thres' for the argument 'partial'.", call. = FALSE) }

                  # Duplicated variable names
                  if (isTRUE(any(duplicated(names(p))))) { stop(paste0("Duplicated variable names in the 'thres' list of the argument 'partial': ", paste0(names(p)[duplicated(names(p))], collapse = ", ")), call. = FALSE) }

                  # Variable names
                  if (isTRUE((any(!names(p) %in% var.mod)))) { stop(paste0("Variables specified in the 'thres' list of the argument 'partial' are not all specified in the model: ", paste0(names(p)[!names(p) %in% var.mod], collapse = ", ")), call. = FALSE) }

                  # Threshold labels
                  if (isTRUE(any(!unlist(p) %in% paste0("t", seq_len(nresp.cat - 1L))))) { stop(paste0("Threshold labels specified in the 'thres' list of the argument 'partial' do not all exist given the ", nresp.cat - 1L, " thresholds ", paste0(sapply(paste0("t", seq_len(nresp.cat - 1L)), function(z) paste0("\"", z, "\"")), collapse = ", "), " for each indicator."), call. = FALSE) }

                  # Duplicated threshold labels
                  if (isTRUE(any(sapply(p, function(y) any(duplicated(y)))))) { stop(paste0("Duplicated threshold labels in the 'thres' list of the argument 'partial': ", paste0(names(which(sapply(p, function(y) any(duplicated(y))))), collapse = ", ")), call. = FALSE) }

                })()

            }

            # Loading, intercept, and residual variance
            if (isTRUE(any(c("load", "inter", "resid") %in% names(partial)))) {

              partial[which(names(partial) != "thres")] |>
                (\(p) {

                  # List with character vector
                  if (isTRUE(any(sapply(p, function(y) !is.character(y))))) { stop("Please specify a list of named character vectors for the list elements 'load', 'inter' and 'resid' for the argument 'partial'.", call. = FALSE) }

                  # Variable names
                  if (isTRUE(any(!unlist(p) %in% var.mod))) { stop(paste0("Variables specified in the argument 'partial' are not all specified in the model: ", paste0(unlist(p)[!unlist(p) %in% var.mod], collapse = ", ")), call. = FALSE) }

                  # Duplicated variable names
                  if (isTRUE(any(sapply(p, function(y) any(duplicated(y)))))) { stop(paste0("Duplicated variable names in character vectors within the list specified in the argument 'partial': ", paste0(names(which(sapply(p, function(y) any(duplicated(y))))), collapse = ", ")), call. = FALSE) }

                })()

            }

          ####### More than two groups
          } else {

            # List with lists
            if (isTRUE(any(sapply(partial, function(y) class(y) != "list")))) { stop("Please specify a list with list elements 'thres', 'load', 'inter' and 'resid' for the argument 'partial'.", call. = FALSE) }

            # Name of list elements
            if (isTRUE(any(!names(partial) %in% c("thres", "load", "inter", "resid")))) { stop("Please name the lists within the list specified in the argument 'partial' with 'thres', 'load', 'inter', and 'resid'.", call. = FALSE) }

            # Duplicated names
            if (isTRUE(any(duplicated(names(partial))))) { stop(paste0("Duplicated list names within the list specified in the argument 'partial': ", paste0(names(partial)[duplicated(names(partial))], collapse = ", ")), call. = FALSE) }

            # Threshold
            if (isTRUE("thres" %in% names(partial))) {

              # List
              if (isTRUE(!is.list(partial[["thres"]]))) { stop("Please specify a list for the element 'thres' specified in the list for the argument 'partial.", call. = FALSE) }

              partial[["thres"]] |>
                (\(p) {
                  # Lists
                  if (isTRUE(any(sapply(p, function(y) !is.list(y))))) { stop("Please specify named lists for the list element 'thres' for the argument 'partial'.", call. = FALSE) }

                  # Character vectors
                  if (isTRUE(any(unlist(lapply(p, function(y) lapply(y, function(z) !is.character(z))))))) { stop(paste0("Please specify a vector with the character strings ", paste0(sapply(paste0("t", seq_len(nresp.cat - 1L)), function(z) paste0("\"", z, "\"")), collapse = ", "), " within the list elements of the 'thres' list for the argument 'partial'."), call. = FALSE) }

                  # Variable names
                  if (isTRUE(any(!names(p) %in% var.mod))) { stop(paste0("Variables specified in the 'thres' list of the argument 'partial' are not all specified in the model: ", paste0(names(p)[!names(p) %in% var.mod], collapse = ", ")), call. = FALSE) }

                  # Duplicated variable names
                  if (isTRUE(any(duplicated(names(p))))) { stop(paste0("Duplicated variable names in the 'thres' list of the argument 'partial': ", paste0(names(p)[duplicated(names(p))], collapse = ", ")), call. = FALSE) }

                  # Threshold labels
                  if (isTRUE(any(!unlist(lapply(p, names)) %in% paste0("t", seq_len(nresp.cat - 1L))))) { stop(paste0("Threshold labels specified in the 'thres' list of the argument 'partial' do not all exist given the ", nresp.cat - 1L, " thresholds ", paste0(sapply(paste0("t", seq_len(nresp.cat - 1L)), function(z) paste0("\"", z, "\"")), collapse = ", "), " for each indicator."), call. = FALSE) }

                  # Duplicated threshold labels
                  if (isTRUE(any(sapply(p, function(y) any(duplicated(names(y))))))) { stop(paste0("Duplicated threshold labels in the 'thres' list of the argument 'partial': ", paste0(names(which(sapply(p, function(y) any(duplicated(names(y)))))), collapse = ", ")), call. = FALSE) }

                  # Group labels
                  if (isTRUE(any(!unlist(p) %in% paste0("g", seq_len(ngroups))))) { stop(paste0("Group labels specified within the 'thres' list specified for the argument 'partial' do not all exist given the ", ngroups, " groups ", paste0(sapply(paste0("g", seq_len(ngroups)), function(z) paste0("\"", z, "\"")) , collapse = ", "), collapse = ", ", "."), call. = FALSE) }

                  # Duplicated group labels
                  if (isTRUE(any(sapply(p, function(y) any(sapply(y, function(z) any(duplicated(z)))))))) { stop(paste0("Duplicated group labels within the 'thres' list specified for the argument 'partial': ", paste0(names(which(sapply(p, function(y) any(sapply(y, function(z) any(duplicated(z))))))), collapse = ", ")), call. = FALSE) }

                })()

            }

            # List elements 'load', 'inter', and 'resid'
            if (isTRUE(any(c("load", "inter", "resid") %in% names(partial)))) {

              partial[which(names(partial) != "thres")] |>
                (\(p) {

                  # Character vector
                  if (isTRUE(any(unlist(lapply(p, function(y) lapply(y, function(z) !is.character(z))))))) { stop(paste0("Please specify lists of character vectors for the 'load', 'inter' and 'resid' lists for the argument 'partial'."), call. = FALSE) }

                  # Variable names
                  if (isTRUE(any(sapply(p, function(y) any(!names(y) %in% var.mod))))) { stop(paste0("Variables specified in the argument 'partial' are not all specified in the model: ", paste0(names(which(sapply(p, function(y) any(!names(y) %in% var.mod)))), collapse = ", ")), call. = FALSE) }

                  # Duplicated variable names
                  if (isTRUE(any(sapply(p, function(y) any(duplicated(names(y))))))) { stop(paste0("Duplicated variable names in lists within the list specified in the argument 'partial': ", paste0(names(which(sapply(p, function(y) any(duplicated(names(y)))))), collapse = ", ")), call. = FALSE) }

                  # Group labels
                  if (isTRUE(any(!unlist(p) %in% paste0("g", seq_len(ngroups))))) { stop(paste0("Group labels specified in the argument 'partial' do not all exist given the ", ngroups, " groups ", paste0(sapply(paste0("g", seq_len(ngroups)), function(z) paste0("\"", z, "\"")) , collapse = ", "), collapse = ", ", "."), call. = FALSE) }

                  # Duplicated group labels
                  if (isTRUE(any(sapply(p, function(y) any(unlist(lapply(y, function(z) any(duplicated(z))))))))) { stop(paste0("Duplicated group labels in lists within the list specified in the argument 'partial': ", paste0(names(which(sapply(p, function(y) any(unlist(lapply(y, function(z) any(duplicated(z)))))))), collapse = ", ")), call. = FALSE) }

                })()

            }

          }

        }

        ##### Two Groups
        if (isTRUE(ngroups == 2L)) {

          # Split partial invariance specification
          partial.thres <- if (isTRUE("thres" %in% names(partial))) { partial[["thres"]] } else { NULL }
          partial.load  <- if (isTRUE("load"  %in% names(partial))) { partial[["load"]]  } else { NULL }
          partial.inter <- if (isTRUE("inter" %in% names(partial))) { partial[["inter"]] } else { NULL }
          partial.resid <- if (isTRUE("resid" %in% names(partial))) { partial[["resid"]] } else { NULL }

          ###### Thresholds
          if (isTRUE(!is.null(partial.thres))) {

            invisible(sapply(names(partial.thres), function(y) sapply(partial.thres[[y]], function(z) mod.thres.fixed <<- paste0(unlist(strsplit(mod.thres.fixed, "\n")) |>
                                                                                                                                   (\(p) {

                                                                                                                                     p[grep(y, p)] <- gsub(paste0(y, ".", toupper(z)), replacement = "NA", p[grep(y, p)])

                                                                                                                                     return(p)

                                                                                                                                    })(), collapse = "\n"))))

          }

          ###### Factor loadings
          if (isTRUE(!is.null(partial.load))) { mod.load.metric <- misty::chr.gsub(sapply(partial.load, function(y) paste0("\\c\\(", paste(rep(paste0(y, ".L"), times = ngroups), collapse = ", "),  "\\)\\*")), replacement = sapply(partial.load, function(y) paste0("\\c\\(", paste(rep("NA", times = ngroups), collapse = ", "),  "\\)\\*")), mod.load.metric) }

          ###### Intercepts
          if (isTRUE(!is.null(partial.inter))) { mod.inter.fixed <- misty::chr.gsub(sapply(partial.inter, function(y) paste0(y, " \\~ \\c\\(", paste(rep("0", times = ngroups), collapse = ", "),  "\\)\\*")), replacement = sapply(partial.inter, function(y) paste0(y, " \\~ \\c\\(", paste(rep("NA", times = ngroups), collapse = ", "),  "\\)\\*")), mod.inter.fixed) }

          ###### Residuals
          if (isTRUE(!is.null(partial.resid))) {

            mod.resid.delta.fixed <- misty::chr.gsub(sapply(partial.resid, function(y) paste0(y, " \\~\\*\\~ \\c\\(", paste(rep("1", times = ngroups), collapse = ", "),  "\\)\\*", y)), replacement = sapply(partial.resid, function(y) paste0(y, " \\~\\*\\~ \\c\\(", paste(rep("NA", times = ngroups), collapse = ", "),  "\\)\\*", y)), mod.resid.delta.fixed)
            mod.resid.theta.fixed <- misty::chr.gsub(sapply(partial.resid, function(y) paste0(y, " \\~\\~ \\c\\(", paste(rep("1", times = ngroups), collapse = ", "),  "\\)\\*", y)), replacement = sapply(partial.resid, function(y) paste0(y, " \\~\\~ \\c\\(", paste(rep("NA", times = ngroups), collapse = ", "),  "\\)\\*", y)), mod.resid.theta.fixed)

          }

        ##### More than Two Groups
        } else {

          # Split partial invariance specification
          partial.thres <- if (isTRUE("thres" %in% names(partial))) { partial[["thres"]] } else { NULL }
          partial.load  <- if (isTRUE("load"  %in% names(partial))) { partial[["load"]]  } else { NULL }
          partial.inter <- if (isTRUE("inter" %in% names(partial))) { partial[["inter"]] } else { NULL }
          partial.resid <- if (isTRUE("resid" %in% names(partial))) { partial[["resid"]] } else { NULL }

          ###### Thresholds
          if (isTRUE(!is.null(partial.thres))) {

            for (i in names(partial.thres)) {

              sapply(names(partial.thres[[i]]), function(y) mod.thres.fixed <<- paste0(unlist(strsplit(mod.thres.fixed, "\n")) |>
                                                                                         (\(p) {

                                                                                           p[grep(i, p)] <- unlist(strsplit(p[grep(i, p)], split = paste0("c\\(", paste0(rep(paste0(i, ".", toupper(y)), times = ngroups), collapse = ", "), "\\)\\*", y))) |>
                                                                                             (\(q) paste0(q[1], paste0("c(", paste0(sapply(seq_len(ngroups), function(z) ifelse(z %in% as.numeric(gsub("g", "", partial.thres[[i]][[y]])), paste0(i, ".", toupper(y), ".g", z), paste0(i, ".", toupper(y)))), collapse = ", "), ")*", y), q[2L]))()

                                                                                           return(p)

                                                                                          })(), collapse = "\n"))

            }

          }


          ###### Factor loadings
          if (isTRUE(!is.null(partial.load))) {

            invisible(sapply(names(partial.load), function(y) misty::chr.omit(misty::chr.trim(unlist(strsplit(mod.load.metric, split = paste0(y, ".L")))), omit = ",", check = FALSE) |>
                               (\(p) mod.load.metric <<- paste0(c(p[1L], paste0(sapply(paste0("g", seq_len(ngroups)), function(w) ifelse(w %in% partial.load[[y]], paste0(y, ".L.", w), paste0(y, ".L"))), collapse = ", "),  p[2L]), collapse = ""))()))

          }

          ###### Intercepts
          if (isTRUE(!is.null(partial.inter))) {

            invisible(sapply(names(partial.inter), function(y) misty::chr.omit(misty::chr.trim(unlist(strsplit(mod.inter.fixed, split = paste0(y, " \\~ c\\(", paste0(rep(0, times = ngroups), collapse = ", "), ")")))), omit = ",", check = FALSE) |>
                               (\(p) mod.inter.fixed <<- paste0(c(p[1L], "\n", paste0(y, " ~ c(", paste0(sapply(paste0("g", seq_len(ngroups)), function(w) ifelse(w %in% partial.inter[[y]], "NA", "0")), collapse = ", "), ")"), p[2L]), collapse = ""))()))

          }

          ###### Residuals
          if (isTRUE(!is.null(partial.resid))) {

            invisible(sapply(names(partial.resid), function(y) misty::chr.omit(misty::chr.trim(unlist(strsplit(mod.resid.delta.fixed, split = paste0(y, " \\~\\*\\~ c\\(", paste0(rep("1", times = ngroups), collapse = ", "), ")")))), omit = ",", check = FALSE) |>
                               (\(p) mod.resid.delta.fixed <<- paste0(c(p[1L], "\n", paste0(y, " ~*~ c(", paste0(sapply(paste0("g", seq_len(ngroups)), function(w) ifelse(w %in% partial.resid[[y]], "NA", "1")), collapse = ", "), ")"), p[2L]), collapse = ""))()))

            invisible(sapply(names(partial.resid), function(y) misty::chr.omit(misty::chr.trim(unlist(strsplit(mod.resid.theta.fixed, split = paste0(y, " \\~\\~ c\\(", paste0(rep("1", times = ngroups), collapse = ", "), ")")))), omit = ",", check = FALSE) |>
                               (\(p) mod.resid.theta.fixed <<- paste0(c(p[1L], "\n", paste0(y, " ~~ c(", paste0(sapply(paste0("g", seq_len(ngroups)), function(w) ifelse(w %in% partial.resid[[y]], "NA", "1")), collapse = ", "), ")"), p[2L]), collapse = ""))()))

          }

        }

      }

      #...................
      #### Model Specification: Configural Measurement Invariance ####

      mod.config <- paste0(mod.load.config, "\n", mod.thres.free, "\n", mod.inter.fixed.config, "\n",
                           if (isTRUE(parameterization == "delta")) { mod.resid.delta.fixed.config } else { mod.resid.theta.fixed.config }, "\n",
                           mod.mean.fixed, "\n", mod.var.fixed)

      #...................
      #### Model Specification: Threshold Measurement Invariance ####

      # More than three response categories, i.e., multiple
      if (isTRUE(resp.cat == "multiple")) {

        mod.thres <- paste0(mod.load.config, "\n", mod.thres.fixed, "\n", mod.inter.free, "\n",
                            if (isTRUE(parameterization == "delta")) { mod.resid.delta.free } else { mod.resid.theta.free }, "\n",
                            mod.mean.fixed, "\n", mod.var.fixed)

      }

      #...................
      #### Model Specification: Metric Measurement Invariance ####

      # More than two response categories, i.e., ternary or multiple
      if (isTRUE(resp.cat %in% c("ternary", "multiple"))) {

        mod.metric <- paste0(mod.load.metric, "\n", mod.thres.fixed, "\n", mod.inter.free, "\n",
                            if (isTRUE(parameterization == "delta")) { mod.resid.delta.free } else { mod.resid.theta.free }, "\n",
                            mod.mean.fixed, "\n", mod.var.free)

      }

      #...................
      #### Model Specification: Scalar Measurement Invariance ####

      mod.scalar <- paste0(mod.load.metric, "\n", mod.thres.fixed, "\n", mod.inter.fixed, "\n",
                           if (isTRUE(parameterization == "delta")) { mod.resid.delta.free } else { mod.resid.theta.free }, "\n",
                           mod.mean.free, "\n", mod.var.free)

      #...................
      #### Model Specification: Strict Measurement Invariance ####

      mod.strict <- paste0(mod.load.metric, "\n", mod.thres.fixed, "\n", mod.inter.fixed, "\n",
                           if (isTRUE(parameterization == "delta")) { mod.resid.delta.fixed } else { mod.resid.theta.fixed }, "\n",
                           mod.mean.free, "\n", mod.var.free)

    #--------------------------------------
    ### Longitudinal Measurement Invariance ####

    } else {

      #...................
      #### Model Syntax: Factor Loadings, Intercepts, Residual Variances, Latent Means and Variances ####

      ##### Factor Loadings: Configural and Metric Measurement Invariance

      # Configural
      mod.load.config <- paste0("# Factor loadings\n", paste(sapply(seq_along(model), function(y) paste(names(model)[[y]], "=~", paste0(model[[y]], collapse = " + "))), collapse = " \n"), collapse = "")

      # Metric
      mod.load.metric <- paste0("# Equal factor loadings across time\n", paste(sapply(seq_along(model), function(y) paste(names(model)[[y]], "=~", paste0("L", seq_along(model[[y]]), "*",  model[[y]], collapse = " + "))), collapse = " \n"), collapse = "\n")

      ##### Thresholds: Threshold Measurement Invariance

      # Thresholds freely estimated across groups
      mod.thres.free <- paste0("# Thresholds\n", paste0(sapply(var.mod, function(y) paste0(y, " | ", paste0(sapply(seq_len(nresp.cat - 1L), function(z) paste0("t", z)), collapse = " + "))), collapse = "\n"))

      # Thresholds constrained to be equal across groups
      mod.thres.fixed <- paste0("# Equal thresholds across time\n", paste0(sapply(seq_along(model), function(y) paste0(sapply(seq_along(model[[y]]), function(z) paste0(model[[y]][z], " | ", paste0(sapply(seq_len(nresp.cat - 1L), function(v) paste0("I", z, ".T", v,  "*t", v)), collapse = " + "), collapse = " \n")), collapse = "\n")), collapse = "\n"))

      ##### Intercepts: Scalar Measurement Invariance

      # Intercepts freely estimated in all but the first time point
      mod.inter.free <- paste0("# Fix intercepts to 0 at the first time point\n", paste0(sapply(seq_along(model), function(y) if (isTRUE(y == 1L)) { paste0(model[[y]], " ~ 0*1 ") } else { paste0(model[[y]], " ~ 1 ") }), collapse = "\n"))

      # Intercepts fixed to 0 across groups
      mod.inter.fixed <- mod.inter.fixed.config <- paste0("# Fix intercepts to 0 across time\n", paste0(sapply(seq_along(model), function(y) paste(model[[y]], "~ 0*1")), collapse = " \n"), collapse = "\n")

      ##### Residual Variances and Scaling Factors: Strict Measurement Invariance

      # Delta parameterization
      mod.resid.delta.fixed <- mod.resid.delta.fixed.config <- paste0("# Fix scaling factors to 1 across time\n", paste0(sapply(var.mod, function(y) paste0(y, " ~*~ 1*", y, collapse = "")), collapse = " \n"))
      mod.resid.delta.free <- paste0("# Fix scaling factors to 1 at the first time point\n", paste0(sapply(seq_along(model), function(y) if (isTRUE(y == 1L)) { paste0(model[[y]], " ~*~ 1*", model[[y]]) } else { paste0(model[[y]], " ~*~ ", model[[y]]) }), collapse = "\n"))

      # Theta parameterization
      mod.resid.theta.fixed <- mod.resid.theta.fixed.config <- paste0("# Fix residual variances to 1 across time\n", paste0(sapply(var.mod, function(y) paste0(y, " ~~ 1*", y, collapse = "")), collapse = " \n"))
      mod.resid.theta.free <- paste0("# Fix residual variances to 1 at the first time point\n", paste0(sapply(seq_along(model), function(y) if (isTRUE(y == 1L)) { paste0(model[[y]], " ~~ 1*", model[[y]]) } else { paste0(model[[y]], " ~~ ", model[[y]]) }), collapse = "\n"))

      ##### Latent Means

      mod.mean.fixed <- paste0("# Fix latent means to 0 across time\n", paste0(sapply(names(model), function(y) paste0(y, " ~ 0*1")), collapse = "\n"))
      mod.mean.free <- paste0("# Fix latent mean at the first time point to 0\n", paste0(sapply(seq_along(model), function(y) if (isTRUE(y == 1L)) { paste0(names(model)[[y]], " ~ 0*1") } else { paste0(names(model)[[y]], " ~ NA*1") }), collapse = "\n"))

      ##### Latent Variances

      mod.var.fixed <- paste0("# Fix latent variances to 1 across time\n", paste0(sapply(names(model), function(y) paste0(y, " ~~ 1*", y)), collapse = "\n"))
      mod.var.free <- paste0("# Fix latent variance at the first time point to 1\n", paste0(sapply(seq_along(model), function(y) if (isTRUE(y == 1L)) { paste0(names(model)[[y]], " ~~ 1*", names(model)[[y]]) } else { paste0(names(model)[[y]], " ~~ NA*", names(model)[[y]]) }), collapse = "\n"))

      ##### Longitudinal Residual Covariances between Parallel Indicators

      if (isTRUE(rescov.long)) { mod.rescov.long <- paste0("# Longitudinal residual covariances\n", paste(apply(matrix(unlist(model), nrow = length(model), byrow = TRUE), 2L, function(y) apply(combn(y, m = 2L), 2L, function(z) paste0(z[1L], " ~~ ", z[2L]))), collapse = "\n"))  }

      #...................
      #### Partial Measurement Invariance ####

      if (isTRUE(!is.null(partial))) {

        ##### Check Partial Invariance Specification
        if (isTRUE(check)) {

          # List elements
          if (isTRUE(any(!names(partial) %in% c("thres", "load", "inter", "resid")))) { stop("Please name the list and character vectors within the list specified in the argument 'partial' with 'thres', 'load', 'inter', and 'resid'.", call. = FALSE) }

          # Duplicated names
          if (isTRUE(any(duplicated(names(partial))))) { stop(paste0("Duplicated element names within the list specified in the argument 'partial': ", paste0(names(partial)[duplicated(names(partial))], collapse = ", ")), call. = FALSE) }

          # Threshold
          if (isTRUE("thres" %in% names(partial))) {

            # List
            if (isTRUE(!is.list(partial[["thres"]]))) { stop("Please specify a list for the element 'thres' specified in the list for the argument 'partial.", call. = FALSE) }

            partial[["thres"]] |>
              (\(p) {
                # Character vector
                if (isTRUE(any(sapply(p, class) != "character"))) { stop("Please specify named character vectors for the list element 'thres' for the argument 'partial'.", call. = FALSE) }

                # Duplicated variable names
                if (isTRUE(any(duplicated(names(p))))) { stop(paste0("Duplicated variable names in the 'thres' list of the argument 'partial': ", paste0(names(p)[duplicated(names(p))], collapse = ", ")), call. = FALSE) }

                # Variable names
                if (isTRUE((any(!names(p) %in% var.mod)))) { stop(paste0("Variables specified in the 'thres' list of the argument 'partial' are not all specified in the model: ", paste0(names(p)[!names(p) %in% var.mod], collapse = ", ")), call. = FALSE) }

                # Threshold labels
                if (isTRUE(any(!unlist(p) %in% paste0("t", seq_len(nresp.cat - 1L))))) { stop(paste0("Threshold labels specified in the 'thres' list of the argument 'partial' do not all exist given the ", nresp.cat - 1L, " thresholds ", paste0(sapply(paste0("t", seq_len(nresp.cat - 1L)), function(z) paste0("\"", z, "\"")), collapse = ", "), " for each indicator."), call. = FALSE) }

                # Duplicated threshold labels
                if (isTRUE(any(sapply(p, function(y) any(duplicated(y)))))) { stop(paste0("Duplicated threshold labels in the 'thres' list of the argument 'partial': ", paste0(names(which(sapply(p, function(y) any(duplicated(y))))), collapse = ", ")), call. = FALSE) }

              })()

          }

          # Loading, intercept, and residual variance
          if (isTRUE(any(c("load", "inter", "resid") %in% names(partial)))) {

            partial[which(names(partial) != "thres")] |>
              (\(p) {

                # List with character vector
                if (isTRUE(any(sapply(p, function(y) !is.character(y))))) { stop("Please specify a list of named character vectors for the list elements 'load', 'inter' and 'resid' for the argument 'partial'.", call. = FALSE) }

                # Variable names
                if (isTRUE(any(!unlist(p) %in% var.mod))) { stop(paste0("Variables specified in the argument 'partial' are not all specified in the model: ", paste0(unlist(p)[!unlist(p) %in% var.mod], collapse = ", ")), call. = FALSE) }

                # Duplicated variable names
                if (isTRUE(any(sapply(p, function(y) any(duplicated(y)))))) { stop(paste0("Duplicated variable names in character vectors within the list specified in the argument 'partial': ", paste0(names(which(sapply(p, function(y) any(duplicated(y))))), collapse = ", ")), call. = FALSE) }

              })()

          }

        }

        # Split partial invariance specification
        partial.thres <- if (isTRUE("thres" %in% names(partial))) { partial[["thres"]] } else { NULL }
        partial.load  <- if (isTRUE("load"  %in% names(partial))) { partial[["load"]]  } else { NULL }
        partial.inter <- if (isTRUE("inter" %in% names(partial))) { partial[["inter"]] } else { NULL }
        partial.resid <- if (isTRUE("resid" %in% names(partial))) { partial[["resid"]] } else { NULL }

        ##### Thresholds
        if (isTRUE(!is.null(partial.thres))) {

          invisible(sapply(names(partial.thres), function(y) sapply(partial.thres[[y]], function(z) mod.thres.fixed <<- paste0(unlist(strsplit(mod.thres.fixed, "\n")) |>
                                                                                                                                 (\(p) {

                                                                                                                                   p[grep(y, p)] <- sub(paste0("I", unlist(lapply(model, function(v) which(v == y))), ".", toupper(z)), replacement = paste0(y, ".", toupper(z)), p[grep(y, p)])

                                                                                                                                   return(p)

                                                                                                                                  })(), collapse = "\n"))))

        }

        ##### Factor loadings
        if (isTRUE(!is.null(partial.load))) { invisible(sapply(partial.load, function(y) mod.load.metric <<- sub(paste0("L", unlist(lapply(model, function(z) which(z %in% y))), "\\*", y), replacement = paste0("NA*", y), mod.load.metric))) }

        ##### Intercepts
        if (isTRUE(!is.null(partial.inter))) { invisible(sapply(partial.inter, function(y) mod.inter.fixed <<- sub(paste0(y, " \\~ 0\\*1"), replacement = paste0(y, " ~ NA*1"), mod.inter.fixed))) }

        ##### Residuals
        if (isTRUE(!is.null(partial.resid))) {

          mod.resid.delta.fixed <- misty::chr.gsub(sapply(partial.resid, function(y) paste0(y, " \\~\\*\\~ 1\\*", y)), replacement = sapply(partial.resid, function(y) paste0(y, " \\~\\*\\~ NA\\*", y)), mod.resid.delta.fixed)
          mod.resid.theta.fixed <- misty::chr.gsub(sapply(partial.resid, function(y) paste0(y, " \\~\\~ 1\\*", y)), replacement = sapply(partial.resid, function(y) paste0(y, " \\~\\~ NA\\*", y)), mod.resid.theta.fixed)

        }

      }

      #...................
      #### Model Specification: Configural Measurement Invariance ####

      mod.config <- paste0(mod.load.config, "\n", mod.thres.free, "\n", mod.inter.fixed.config, "\n",
                           if (isTRUE(parameterization == "delta")) { mod.resid.delta.fixed.config } else { mod.resid.theta.fixed.config }, "\n",
                           mod.mean.fixed, "\n", mod.var.fixed)

      #...................
      #### Model Specification: Threshold Measurement Invariance ####

      # More than three response categories, i.e., multiple
      if (isTRUE(resp.cat == "multiple")) {

        mod.thres <- paste0(mod.load.config, "\n", mod.thres.fixed, "\n", mod.inter.free, "\n",
                            if (isTRUE(parameterization == "delta")) { mod.resid.delta.free } else { mod.resid.theta.free }, "\n",
                            mod.mean.fixed, "\n", mod.var.fixed)

      }

      #...................
      #### Model Specification: Metric Measurement Invariance ####

      # More than two response categories, i.e., ternary or multiple
      if (isTRUE(resp.cat %in% c("ternary", "multiple"))) {

        mod.metric <- paste0(mod.load.metric, "\n", mod.thres.fixed, "\n", mod.inter.free, "\n",
                             if (isTRUE(parameterization == "delta")) { mod.resid.delta.free } else { mod.resid.theta.free }, "\n",
                             mod.mean.fixed, "\n", mod.var.free)

      }

      #...................
      #### Model Specification: Scalar Measurement Invariance ####

      mod.scalar <- paste0(mod.load.metric, "\n", mod.thres.fixed, "\n", mod.inter.fixed, "\n",
                           if (isTRUE(parameterization == "delta")) { mod.resid.delta.free } else { mod.resid.theta.free }, "\n",
                           mod.mean.free, "\n", mod.var.free)

      #...................
      #### Model Specification: Strict Measurement Invariance ####

      mod.strict <- paste0(mod.load.metric, "\n", mod.thres.fixed, "\n", mod.inter.fixed, "\n",
                           if (isTRUE(parameterization == "delta")) { mod.resid.delta.fixed } else { mod.resid.theta.fixed }, "\n",
                           mod.mean.free, "\n", mod.var.free)

      #...................
      #### Longitudinal Residual Covariances between Parallel Indicators ####

      if (isTRUE(rescov.long)) {

        mod.rescov.long <- paste0("# Longitudinal residual covariances\n", paste(apply(matrix(unlist(model), nrow = length(model), byrow = TRUE), 2L, function(y) apply(combn(y, m = 2L), 2L, function(z) paste0(z[1L], " ~~ ", z[2L]))), collapse = "\n"))

        if (isTRUE(!is.null(mod.config))) { mod.config <- paste0(mod.config, "\n", mod.rescov.long) }

        if (isTRUE(!is.null(mod.thres)))  { mod.thres  <- paste0(mod.thres,  "\n", mod.rescov.long) }

        if (isTRUE(!is.null(mod.metric))) { mod.metric <- paste0(mod.metric, "\n", mod.rescov.long) }

        if (isTRUE(!is.null(mod.scalar))) { mod.scalar <- paste0(mod.scalar, "\n", mod.rescov.long) }

        if (isTRUE(!is.null(mod.strict))) { mod.strict <- paste0(mod.strict, "\n", mod.rescov.long) }

      }

    }

  }

  #--------------------------------------
  ### Residual Covariances ####

  if (isTRUE(!is.null(rescov))) {

    mod.rescov <- paste0("# Residual covariances\n", paste0(vapply(rescov, function(y) paste(y, collapse = " ~~ "), FUN.VALUE = character(1L)), collapse = " \n"))

    if (isTRUE(!is.null(mod.config))) { mod.config <- paste0(mod.config, "\n", mod.rescov) }

    if (isTRUE(!is.null(mod.thres)))  { mod.thres  <- paste0(mod.thres,  "\n", mod.rescov) }

    if (isTRUE(!is.null(mod.metric))) { mod.metric <- paste0(mod.metric, "\n", mod.rescov) }

    if (isTRUE(!is.null(mod.scalar))) { mod.scalar <- paste0(mod.scalar, "\n", mod.rescov) }

    if (isTRUE(!is.null(mod.strict))) { mod.strict <- paste0(mod.strict, "\n", mod.rescov) }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model Estimation ####

  mod.config.fit <- mod.thres.fit <- mod.metric.fit <- mod.scalar.fit <- mod.strict.fit <- mod.config.fit.check <- mod.thres.fit.check <- mod.metric.fit.check <- mod.scalar.fit.check <- mod.strict.fit.check <- NULL

  if (isTRUE(lavaan.run)) {

    #--------------------------------------
    ### Configural Measurement Invariance

    if (isTRUE(!is.null(mod.config))) {

      # Function not used in the item.nonequi() function
      if (isTRUE(is.null(se))) {

        mod.config.fit <- suppressWarnings(lavaan::cfa(mod.config, data = x, group = if (isTRUE(long)) { NULL } else { ".group" },
                                                       ordered = ordered, parameterization = parameterization, meanstructure = TRUE,
                                                       cluster = if (isTRUE(is.null(cluster))) { NULL } else { ".cluster" },
                                                       std.lv = std.lv, effect.coding = effect.coding, estimator = estimator, missing = missing))

      # Function used in the item.nonequi() function
      } else {

        mod.config.fit <- suppressWarnings(lavaan::cfa(mod.config, data = x, group = if (isTRUE(long)) { NULL } else { ".group" },
                                                       ordered = ordered, parameterization = "delta", meanstructure = TRUE,
                                                       cluster = if (isTRUE(is.null(cluster))) { NULL } else { ".cluster" },
                                                       se = "none", std.lv = std.lv, effect.coding = effect.coding, estimator = estimator, missing = missing))

      }

      #### Convergence and model identification checks ####
      if (isTRUE(check && is.null(se))) { mod.config.fit.check <- .conv.ident(mod.config.fit, invar = "config", long = long) }

    }

    #--------------------------------------
    ### Threshold Measurement Invariance

    if (isTRUE(any(c("thres", "metric", "scalar", "strict") %in% invar) && !is.null(mod.thres))) {

      #### Model estimation ####
      mod.thres.fit <- suppressWarnings(lavaan::cfa(mod.thres, data = x, group = if (isTRUE(long)) { NULL } else { ".group" },
                                                    ordered = ordered, parameterization = parameterization, meanstructure = TRUE,
                                                    cluster = if (isTRUE(is.null(cluster))) { NULL } else { ".cluster" },
                                                    std.lv = std.lv, effect.coding = effect.coding, estimator = estimator, missing = missing))

      #### Convergence and model identification checks ####
      if (isTRUE(check)) { mod.thres.fit.check <- .conv.ident(mod.thres.fit, invar = "thres", long = long) }

    }

    #--------------------------------------
    ### Metric measurement invariance
    if (isTRUE(any(c("metric", "scalar", "strict") %in% invar) && !is.null(mod.metric))) {

      #### Model estimation ####
      mod.metric.fit <- suppressWarnings(lavaan::cfa(mod.metric, data = x, group = if (isTRUE(long)) { NULL } else { ".group" },
                                                     ordered = ordered, parameterization = parameterization, meanstructure = TRUE,
                                                     cluster = if (isTRUE(is.null(cluster))) { NULL } else { ".cluster" },
                                                     std.lv = std.lv, effect.coding = effect.coding, estimator = estimator, missing = missing))

      #### Convergence and model identification checks ####
      if (isTRUE(check)) { mod.metric.fit.check <- .conv.ident(mod.metric.fit, invar = "metric", long = long) }

    }

    #--------------------------------------
    ### Scalar Measurement Invariance

    if (isTRUE(any(c("scalar", "strict") %in% invar) && !is.null(mod.scalar))) {

      #### Model estimation ####
      mod.scalar.fit <- suppressWarnings(lavaan::cfa(mod.scalar, data = x, group = if (isTRUE(long)) { NULL } else { ".group" },
                                                     ordered = ordered, parameterization = parameterization, meanstructure = TRUE,
                                                     cluster = if (isTRUE(is.null(cluster))) { NULL } else { ".cluster" },
                                                     std.lv = std.lv, effect.coding = effect.coding, estimator = estimator, missing = missing))

      #### Convergence and model identification checks ####
      if (isTRUE(check)) { mod.scalar.fit.check <- .conv.ident(mod.scalar.fit, invar = "scalar", long = long) }

    }

    #--------------------------------------
    ### Strict Measurement Invariance

    if (isTRUE("strict" %in% invar && !is.null(mod.strict))) {

      #### Model estimation ####
      mod.strict.fit <- suppressWarnings(lavaan::cfa(mod.strict, data = x, group = if (isTRUE(long)) { NULL } else { ".group" },
                                                     ordered = ordered, parameterization = parameterization, meanstructure = TRUE,
                                                     cluster = if (isTRUE(is.null(cluster))) { NULL } else { ".cluster" },
                                                     std.lv = std.lv, effect.coding = effect.coding, estimator = estimator, missing = missing))

      #### Convergence and model identification checks ####
      if (isTRUE(check)) { mod.strict.fit.check <- .conv.ident(mod.strict.fit, invar = "strict", long = long) }

    }

    #--------------------------------------
    ### Proper Null Model ####

    if (isTRUE(null.model)) {

      mod.null.fit <- suppressWarnings(lavaan::cfa(mod.null, data = x, group = if (isTRUE(long)) { NULL } else { ".group" },
                                                   ordered = FALSE, meanstructure = TRUE,
                                                   cluster = if (isTRUE(is.null(cluster))) { NULL } else { ".cluster" },
                                                   std.lv = std.lv, effect.coding = effect.coding, estimator = estimator, missing = missing))

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Chi-Squared Difference Test ####

  chidiff.conf.met <- chidiff.met.sca <- chidiff.sca.str <- chidiff.conf.sca <- chidiff.conf.thr <- chidiff.thr.met <- NULL

  #--------------------------------------
  ###  Models with Continuous Indicators

  if (isTRUE(!ordered)) {

    #### Config vs. Metric Measurement Invariance ####

    if (isTRUE(!is.null(mod.config.fit) && !is.null(mod.metric.fit))) {

      chidiff.conf.met <- tryCatch(lavaan::lavTestLRT(mod.config.fit, mod.metric.fit),
                                   error = function(y) { data.frame(matrix(NA, ncol = 7L, dimnames = list(NULL, c("Df", "AIC", "BIC", "Chisq", "Chisq diff", "Df diff", "Pr(>Chisq)"))), check.names = FALSE) },
                                   warning = function(z) { tryCatch(suppressWarnings(lavaan::lavTestLRT(mod.config.fit, mod.metric.fit, method = "satorra.bentler.2010")),
                                                                    error = function(y) { data.frame(matrix(NA, ncol = 7L, dimnames = list(NULL, c("Df", "AIC", "BIC", "Chisq", "Chisq diff", "Df diff", "Pr(>Chisq)"))), check.names = FALSE) }) })

    }

    #### Metric vs. Scalar Measurement Invariance ####

    if (isTRUE(!is.null(mod.metric.fit) && !is.null(mod.scalar.fit))) {

      chidiff.met.sca <- tryCatch(lavaan::lavTestLRT(mod.metric.fit, mod.scalar.fit),
                                  error = function(y) { data.frame(matrix(NA, ncol = 7L, dimnames = list(NULL, c("Df", "AIC", "BIC", "Chisq", "Chisq diff", "Df diff", "Pr(>Chisq)"))), check.names = FALSE) },
                                  warning = function(z) { tryCatch(suppressWarnings(lavaan::lavTestLRT(mod.metric.fit, mod.scalar.fit, method = "satorra.bentler.2010")),
                                                                   error = function(y) { data.frame(matrix(NA, ncol = 7L, dimnames = list(NULL, c("Df", "AIC", "BIC", "Chisq", "Chisq diff", "Df diff", "Pr(>Chisq)"))), check.names = FALSE) }) })

    }

    #### Scalar vs. Strict Measurement Invariance ####

    if (isTRUE(!is.null(mod.scalar.fit) && !is.null(mod.strict.fit))) {

      chidiff.sca.str <- tryCatch(lavaan::lavTestLRT(mod.scalar.fit, mod.strict.fit),
                                  error = function(y) { data.frame(matrix(NA, ncol = 7L, dimnames = list(NULL, c("Df", "AIC", "BIC", "Chisq", "Chisq diff", "Df diff", "Pr(>Chisq)"))), check.names = FALSE) },
                                  warning = function(z) { tryCatch(suppressWarnings(lavaan::lavTestLRT(mod.scalar.fit, mod.strict.fit, method = "satorra.bentler.2010")),
                                                                   error = function(y) { data.frame(matrix(NA, ncol = 7L, dimnames = list(NULL, c("Df", "AIC", "BIC", "Chisq", "Chisq diff", "Df diff", "Pr(>Chisq)"))), check.names = FALSE) }) })

    }

  #--------------------------------------
  ###  Models with Ordered Categorical Indicators

  } else {

    switch(resp.cat,
           #### Binary Response Categories ####
           "binary" = {

             ##### Configural vs. Scalar Measurement Invariance
             if (isTRUE(!is.null(mod.config.fit) && !is.null(mod.scalar.fit))) {

               chidiff.conf.sca <- tryCatch(lavaan::lavTestLRT(mod.config.fit, mod.scalar.fit),
                                            error = function(y) { data.frame(matrix(NA, ncol = 7L, dimnames = list(NULL, c("Df", "AIC", "BIC", "Chisq", "Chisq diff", "Df diff", "Pr(>Chisq)"))), check.names = FALSE) },
                                            warning = function(z) { tryCatch(suppressWarnings(lavaan::lavTestLRT(mod.config.fit, mod.scalar.fit, method = "satorra.bentler.2010")),
                                                                             error = function(y) { data.frame(matrix(NA, ncol = 7L, dimnames = list(NULL, c("Df", "AIC", "BIC", "Chisq", "Chisq diff", "Df diff", "Pr(>Chisq)"))), check.names = FALSE) }) })

             }

           #### Ternary Response Categories ####
           }, "ternary" = {

             ##### Configural vs. Metric Measurement Invariance
             if (isTRUE(!is.null(mod.config.fit) && !is.null(mod.metric.fit))) {

               chidiff.conf.met <- tryCatch(lavaan::lavTestLRT(mod.config.fit, mod.metric.fit),
                                            error = function(y) { data.frame(matrix(NA, ncol = 7L, dimnames = list(NULL, c("Df", "AIC", "BIC", "Chisq", "Chisq diff", "Df diff", "Pr(>Chisq)"))), check.names = FALSE) },
                                            warning = function(z) { tryCatch(suppressWarnings(lavaan::lavTestLRT(mod.config.fit, mod.metric.fit, method = "satorra.bentler.2010")),
                                                                             error = function(y) { data.frame(matrix(NA, ncol = 7L, dimnames = list(NULL, c("Df", "AIC", "BIC", "Chisq", "Chisq diff", "Df diff", "Pr(>Chisq)"))), check.names = FALSE) }) })

             }

             ##### Metric vs. Scalar Invariance
             if (isTRUE(!is.null(mod.metric.fit) && !is.null(mod.scalar.fit))) {

               chidiff.met.sca <- tryCatch(lavaan::lavTestLRT(mod.metric.fit, mod.scalar.fit),
                                            error = function(y) { data.frame(matrix(NA, ncol = 7L, dimnames = list(NULL, c("Df", "AIC", "BIC", "Chisq", "Chisq diff", "Df diff", "Pr(>Chisq)"))), check.names = FALSE) },
                                            warning = function(z) { tryCatch(suppressWarnings(lavaan::lavTestLRT(mod.metric.fit, mod.scalar.fit, method = "satorra.bentler.2010")),
                                                                             error = function(y) { data.frame(matrix(NA, ncol = 7L, dimnames = list(NULL, c("Df", "AIC", "BIC", "Chisq", "Chisq diff", "Df diff", "Pr(>Chisq)"))), check.names = FALSE) }) })

             }


           #### Multiple Response Categories ####
           }, "multiple" = {

             ##### Configural vs. Threshold Measurement Invariance
             if (isTRUE(!is.null(mod.config.fit) && !is.null(mod.thres.fit))) {

               chidiff.conf.thr <- tryCatch(lavaan::lavTestLRT(mod.config.fit, mod.thres.fit),
                                            error = function(y) { data.frame(matrix(NA, ncol = 7L, dimnames = list(NULL, c("Df", "AIC", "BIC", "Chisq", "Chisq diff", "Df diff", "Pr(>Chisq)"))), check.names = FALSE) },
                                            warning = function(z) { tryCatch(suppressWarnings(lavaan::lavTestLRT(mod.config.fit, mod.thres.fit, method = "satorra.bentler.2010")),
                                                                             error = function(y) { data.frame(matrix(NA, ncol = 7L, dimnames = list(NULL, c("Df", "AIC", "BIC", "Chisq", "Chisq diff", "Df diff", "Pr(>Chisq)"))), check.names = FALSE) }) })

             }

             ##### Threshold vs. Metric Invariance ####
             if (isTRUE(!is.null(mod.thres.fit) && !is.null(mod.metric.fit))) {

               chidiff.thr.met <- tryCatch(lavaan::lavTestLRT(mod.thres.fit, mod.metric.fit),
                                           error = function(y) { data.frame(matrix(NA, ncol = 7L, dimnames = list(NULL, c("Df", "AIC", "BIC", "Chisq", "Chisq diff", "Df diff", "Pr(>Chisq)"))), check.names = FALSE) },
                                           warning = function(z) { tryCatch(suppressWarnings(lavaan::lavTestLRT(mod.thres.fit, mod.metric.fit, method = "satorra.bentler.2010")),
                                                                            error = function(y) { data.frame(matrix(NA, ncol = 7L, dimnames = list(NULL, c("Df", "AIC", "BIC", "Chisq", "Chisq diff", "Df diff", "Pr(>Chisq)"))), check.names = FALSE) }) })

             }

             ##### Metric vs. Scalar Invariance
             if (isTRUE(!is.null(mod.metric.fit) && !is.null(mod.scalar.fit))) {

               chidiff.met.sca <- tryCatch(lavaan::lavTestLRT(mod.metric.fit, mod.scalar.fit),
                                            error = function(y) { data.frame(matrix(NA, ncol = 7L, dimnames = list(NULL, c("Df", "AIC", "BIC", "Chisq", "Chisq diff", "Df diff", "Pr(>Chisq)"))), check.names = FALSE) },
                                            warning = function(z) { tryCatch(suppressWarnings(lavaan::lavTestLRT(mod.metric.fit, mod.scalar.fit, method = "satorra.bentler.2010")),
                                                                             error = function(y) { data.frame(matrix(NA, ncol = 7L, dimnames = list(NULL, c("Df", "AIC", "BIC", "Chisq", "Chisq diff", "Df diff", "Pr(>Chisq)"))), check.names = FALSE) }) })

             }


           })

    #### Scalar vs. Strict Measurement Invariance ####

    if (isTRUE(!is.null(mod.scalar.fit) && !is.null(mod.strict.fit))) {

      chidiff.sca.str <- tryCatch(lavaan::lavTestLRT(mod.scalar.fit, mod.strict.fit),
                                  error = function(y) { data.frame(matrix(NA, ncol = 7L, dimnames = list(NULL, c("Df", "AIC", "BIC", "Chisq", "Chisq diff", "Df diff", "Pr(>Chisq)"))), check.names = FALSE) },
                                  warning = function(z) { tryCatch(suppressWarnings(lavaan::lavTestLRT(mod.scalar.fit, mod.strict.fit, method = "satorra.bentler.2010")),
                                                                   error = function(y) { data.frame(matrix(NA, ncol = 7L, dimnames = list(NULL, c("Df", "AIC", "BIC", "Chisq", "Chisq diff", "Df diff", "Pr(>Chisq)"))), check.names = FALSE) }) })

    }

  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model Fit ####

  lav.fit.config <- lav.fit.thres <- lav.fit.metric <- lav.fit.scalar <- lav.fit.strict <- NULL

  if (isTRUE("fit" %in% print)) {

    # Configural invariance model
    if (isTRUE(!is.null(mod.config.fit))) { lav.fit.config <- suppressWarnings(lavaan::fitmeasures(mod.config.fit, baseline.model = if (isTRUE(null.model)) { mod.null.fit } else { NULL })) }

    # Threshold invariance model
    if (isTRUE(!is.null(mod.thres.fit))) { lav.fit.thres <- suppressWarnings(lavaan::fitmeasures(mod.thres.fit)) }

    # Metric invariance model
    if (isTRUE(!is.null(mod.metric.fit))) { lav.fit.metric <- suppressWarnings(lavaan::fitmeasures(mod.metric.fit, baseline.model = if (isTRUE(null.model)) { mod.null.fit } else { NULL })) }

    # Scalar invariance model
    if (isTRUE(!is.null(mod.scalar.fit))) { lav.fit.scalar <- suppressWarnings(lavaan::fitmeasures(mod.scalar.fit, baseline.model = if (isTRUE(null.model)) { mod.null.fit } else { NULL })) }

    # Strict invariance model
    if (isTRUE(!is.null(mod.strict.fit))) { lav.fit.strict <- suppressWarnings(lavaan::fitmeasures(mod.strict.fit, baseline.model = if (isTRUE(null.model)) { mod.null.fit } else { NULL })) }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Parameter Estimates ####

  mod.config.param <- mod.thres.param <- mod.metric.param <- mod.scalar.param <- mod.strict.param <- NULL

  if (isTRUE("est" %in% print)) {

    # Configural invariance model
    if (isTRUE(!is.null(mod.config.fit))) { mod.config.param <- within(data.frame(lavaan::parameterEstimates(mod.config.fit), stdyx = lavaan::standardizedsolution(mod.config.fit)[, "est.std"]), assign("label", "")) |>
       (\(p) if (isTRUE(!long)) { p[, c("group", "lhs", "op", "rhs", "label", "est", "se", "z", "pvalue", "stdyx")] } else { p[, c("lhs", "op", "rhs", "label", "est", "se", "z", "pvalue", "stdyx")] })() }

     # Threshold invariance model
     if (isTRUE(!is.null(mod.thres.fit))) { mod.thres.param <- within(data.frame(lavaan::parameterEstimates(mod.thres.fit), stdyx = lavaan::standardizedsolution(mod.config.fit)[, "est.std"]), assign("label", "")) |>
       (\(p) if (isTRUE(!long)) { p[, c("group", "lhs", "op", "rhs", "label", "est", "se", "z", "pvalue", "stdyx")] } else { p[, c("lhs", "op", "rhs", "label", "est", "se", "z", "pvalue", "stdyx")] })() }

     # Metric invariance model
     if (isTRUE(!is.null(mod.metric.fit))) { mod.metric.param <- data.frame(lavaan::parameterEstimates(mod.metric.fit), stdyx = lavaan::standardizedsolution(mod.metric.fit)[, "est.std"]) |>
       (\(p) if (isTRUE(!long)) { p[, c("group", "lhs", "op", "rhs", "label", "est", "se", "z", "pvalue", "stdyx")] } else { p[, c("lhs", "op", "rhs", "label", "est", "se", "z", "pvalue", "stdyx")] })() }

     # Scalar invariance model
     if (isTRUE(!is.null(mod.scalar.fit))) { mod.scalar.param <- data.frame(lavaan::parameterEstimates(mod.scalar.fit), stdyx = lavaan::standardizedsolution(mod.scalar.fit)[, "est.std"]) |>
       (\(p) if (isTRUE(!long)) { p[, c("group", "lhs", "op", "rhs", "label", "est", "se", "z", "pvalue", "stdyx")] } else { p[, c("lhs", "op", "rhs", "label", "est", "se", "z", "pvalue", "stdyx")] })() }

     # Strict invariance model
     if (isTRUE(!is.null(mod.strict.fit))) { mod.strict.param <- data.frame(lavaan::parameterEstimates(mod.strict.fit), stdyx = lavaan::standardizedsolution(mod.strict.fit)[, "est.std"])|>
       (\(p) if (isTRUE(!long)) { p[, c("group", "lhs", "op", "rhs", "label", "est", "se", "z", "pvalue", "stdyx")] } else { p[, c("lhs", "op", "rhs", "label", "est", "se", "z", "pvalue", "stdyx")] })() }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Modification Indices ####

  mod.config.modind <- mod.thres.modind <- mod.metric.modind <- mod.scalar.modind <- mod.strict.modind <- mod.thres.score <- mod.metric.score <- mod.scalar.score <- mod.strict.score <- NULL

  if (isTRUE("modind" %in% print && estimator != "PML")) {

    # Configural invariance model
    if (isTRUE(!is.null(mod.config.fit))) { mod.config.modind <- tryCatch(suppressWarnings(lavaan::modindices(mod.config.fit)), error = function(y) return(NULL)) |> (\(p) if (isTRUE(!is.null(p))) { misty::df.rename(p[, if (isTRUE(!long)) { c("group", "lhs", "op", "rhs", "mi", "epc", "sepc.all") } else { c("lhs", "op", "rhs", "mi", "epc", "sepc.all") } ], from = "sepc.all", to = "stdyx") } else { return(p) })() }

    # Threshold invariance model
    if (isTRUE(!is.null(mod.thres.fit)))  { mod.thres.modind  <- tryCatch(suppressWarnings(lavaan::modindices(mod.thres.fit)), error = function(y) return(NULL)) |> (\(p) if (isTRUE(!is.null(p))) { misty::df.rename(p[, if (isTRUE(!long)) { c("group", "lhs", "op", "rhs", "mi", "epc", "sepc.all") } else { c("lhs", "op", "rhs", "mi", "epc", "sepc.all") } ], from = "sepc.all", to = "stdyx") } else { return(p) })() }

    # Metric invariance model
    if (isTRUE(!is.null(mod.metric.fit))) { mod.metric.modind <- tryCatch(suppressWarnings(lavaan::modindices(mod.metric.fit)), error = function(y) return(NULL)) |> (\(p) if (isTRUE(!is.null(p))) { misty::df.rename(p[, if (isTRUE(!long)) { c("group", "lhs", "op", "rhs", "mi", "epc", "sepc.all") } else { c("lhs", "op", "rhs", "mi", "epc", "sepc.all") } ], from = "sepc.all", to = "stdyx") } else { return(p) })() }

    # Scalar invariance model
    if (isTRUE(!is.null(mod.scalar.fit))) { mod.scalar.modind <- tryCatch(suppressWarnings(lavaan::modindices(mod.scalar.fit)), error = function(y) return(NULL)) |> (\(p) if (isTRUE(!is.null(p))) { misty::df.rename(p[, if (isTRUE(!long)) { c("group", "lhs", "op", "rhs", "mi", "epc", "sepc.all") } else { c("lhs", "op", "rhs", "mi", "epc", "sepc.all") } ], from = "sepc.all", to = "stdyx") } else { return(p) })() }

    # Strict invariance model
    if (isTRUE(!is.null(mod.strict.fit))) { mod.strict.modind <- tryCatch(suppressWarnings(lavaan::modindices(mod.strict.fit)), error = function(y) return(NULL)) |> (\(p) if (isTRUE(!is.null(p))) { misty::df.rename(p[, if (isTRUE(!long)) { c("group", "lhs", "op", "rhs", "mi", "epc", "sepc.all") } else { c("lhs", "op", "rhs", "mi", "epc", "sepc.all") } ], from = "sepc.all", to = "stdyx") } else { return(p) })() }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Score Test ####

    # Threshold invariance model
    if (isTRUE(!is.null(mod.thres.fit))) { mod.thres.score <- tryCatch(suppressWarnings(lavaan::lavTestScore(mod.thres.fit, epc = TRUE, warn = FALSE)), error = function(y) return(NULL)) }

    # Metric invariance model
    if (isTRUE(!is.null(mod.metric.fit))) { mod.metric.score <- tryCatch(suppressWarnings(lavaan::lavTestScore(mod.metric.fit, epc = TRUE, warn = FALSE)), error = function(y) return(NULL)) }

    # Scalar invariance model
    if (isTRUE(!is.null(mod.scalar.fit))) { mod.scalar.score <- tryCatch(suppressWarnings(lavaan::lavTestScore(mod.scalar.fit, epc = TRUE, warn = FALSE)), error = function(y) return(NULL)) }

    # Strict invariance model
    if (isTRUE(!is.null(mod.strict.fit))) { mod.strict.score <- tryCatch(suppressWarnings(lavaan::lavTestScore(mod.strict.fit, epc = TRUE, warn = FALSE)), error = function(y) return(NULL)) }

    #--------------------------------------
    ### Threshold Invariance Model ####

    if (isTRUE(!is.null(mod.thres.score))) {

      # Parameter table
      partable <- lavaan::parTable(mod.thres.fit)

      # Univariate score statistics
      uniscore <- mod.thres.score$uni

      # Expected parameter change
      epcscore <- mod.thres.score$epc

      #### Between-Group Measurement Invariance ####

      if (isTRUE(!long)) {

        mod.thres.score <- data.frame(label = NA, group.lhs = NA, group.rhs = NA, lhs = NA, op = NA, rhs = NA, mi = NA, df = NA, pvalue = NA, lhs.epc = NA, rhs.epc = NA, lhs.stdyx = NA, rhs.stdyx = NA)

        for (i in seq_len(nrow(uniscore))) {

          mod.thres.score[i, ] <- data.frame(label = partable[partable$plabel == uniscore[i, "lhs"], "label"],
                                             group.lhs = partable[partable$plabel == uniscore[i, "lhs"], "group"], group.rhs = partable[partable$plabel == uniscore[i, "rhs"], "group"],
                                             lhs = paste0(partable[partable$plabel == uniscore[i, "lhs"], c("lhs", "op", "rhs")], collapse = " "), op = "==",
                                             rhs = paste0(partable[partable$plabel == uniscore[i, "rhs"], c("lhs", "op", "rhs")], collapse = " "), mi = uniscore[i, "X2"], df = uniscore[i, "df"], pvalue = uniscore[i, "p.value"],
                                             lhs.epc = epcscore[partable$plabel == uniscore[i, "lhs"], "epc"], rhs.epc = epcscore[partable$plabel == uniscore[i, "rhs"], "epc"], lhs.stdyx = epcscore[partable$plabel == uniscore[i, "lhs"], "sepc.all"], rhs.stdyx = epcscore[partable$plabel == uniscore[i, "rhs"], "sepc.all"])

        }

      #### Longitudinal Measurement Invariance ####

      } else {

        mod.thres.score <- data.frame(label = NA, lhs = NA, op = NA, rhs = NA, mi = NA, df = NA, pvalue = NA, lhs.epc = NA, rhs.epc = NA, lhs.stdyx = NA, rhs.stdyx = NA)

        for (i in seq_len(nrow(uniscore))) {

          mod.thres.score[i, ] <- data.frame(label = partable[partable$plabel == uniscore[i, "lhs"], "label"],
                                             lhs = paste0(partable[partable$plabel == uniscore[i, "lhs"], c("lhs", "op", "rhs")], collapse = " "), op = "==", rhs = paste0(partable[partable$plabel == uniscore[i, "rhs"], c("lhs", "op", "rhs")], collapse = " "),
                                             mi = uniscore[i, "X2"], df = uniscore[i, "df"], pvalue = uniscore[i, "p.value"],
                                             lhs.epc = epcscore[partable$plabel == uniscore[i, "lhs"], "epc"], rhs.epc = epcscore[partable$plabel == uniscore[i, "rhs"], "epc"], lhs.stdyx = epcscore[partable$plabel == uniscore[i, "lhs"], "sepc.all"], rhs.stdyx = epcscore[partable$plabel == uniscore[i, "rhs"], "sepc.all"])

        }

      }

    }

    #--------------------------------------
    ### Metric Invariance Model ####

    if (isTRUE(!is.null(mod.metric.score))) {

      # Parameter table
      partable <- lavaan::parTable(mod.metric.fit)

      # Univariate score statistics
      uniscore <- mod.metric.score$uni

      # Effects coding
      if (isTRUE(ident == "effect")) { uniscore <- uniscore[-grep("-", uniscore$rhs), ] }

      # Expected parameter change
      epcscore <- mod.metric.score$epc

      #### Between-Group Measurement Invariance ####

      if (isTRUE(!long)) {

        mod.metric.score <- data.frame(label = NA, group.lhs = NA, group.rhs = NA, lhs = NA, op = NA, rhs = NA, mi = NA, df = NA, pvalue = NA, lhs.epc = NA, rhs.epc = NA, lhs.stdyx = NA, rhs.stdyx = NA)

        for (i in seq_len(nrow(uniscore))) {

          mod.metric.score[i, ] <- data.frame(label = partable[partable$plabel == uniscore[i, "lhs"], "label"],
                                              group.lhs = partable[partable$plabel == uniscore[i, "lhs"], "group"], group.rhs = partable[partable$plabel == uniscore[i, "rhs"], "group"],
                                              lhs = paste0(partable[partable$plabel == uniscore[i, "lhs"], c("lhs", "op", "rhs")], collapse = " "), op = "==", rhs = paste0(partable[partable$plabel == uniscore[i, "rhs"], c("lhs", "op", "rhs")], collapse = " "),
                                              mi = uniscore[i, "X2"], df = uniscore[i, "df"], pvalue = uniscore[i, "p.value"],
                                              lhs.epc = epcscore[partable$plabel == uniscore[i, "lhs"], "epc"], rhs.epc = epcscore[partable$plabel == uniscore[i, "rhs"], "epc"], lhs.stdyx = epcscore[partable$plabel == uniscore[i, "lhs"], "sepc.all"], rhs.stdyx = epcscore[partable$plabel == uniscore[i, "rhs"], "sepc.all"])

        }

      #### Longitudinal Measurement Invariance ####

      } else {

        mod.metric.score <- data.frame(label = NA, lhs = NA, op = NA, rhs = NA, mi = NA, df = NA, pvalue = NA, lhs.epc = NA, rhs.epc = NA, lhs.stdyx = NA, rhs.stdyx = NA)

        for (i in seq_len(nrow(uniscore))) {

          mod.metric.score[i, ] <- data.frame(label = partable[partable$plabel == uniscore[i, "lhs"], "label"],
                                              lhs = paste0(partable[partable$plabel == uniscore[i, "lhs"], c("lhs", "op", "rhs")], collapse = " "), op = "==", rhs = paste0(partable[partable$plabel == uniscore[i, "rhs"], c("lhs", "op", "rhs")], collapse = " "),
                                              mi = uniscore[i, "X2"], df = uniscore[i, "df"], pvalue = uniscore[i, "p.value"],
                                              lhs.epc = epcscore[partable$plabel == uniscore[i, "lhs"], "epc"], rhs.epc = epcscore[partable$plabel == uniscore[i, "rhs"], "epc"], lhs.stdyx = epcscore[partable$plabel == uniscore[i, "lhs"], "sepc.all"], rhs.stdyx = epcscore[partable$plabel == uniscore[i, "rhs"], "sepc.all"])

        }

      }

    }

    #--------------------------------------
    ### Scalar Invariance Model ####

    if (isTRUE(!is.null(mod.scalar.score))) {

      # Parameter table
      partable <- lavaan::parTable(mod.scalar.fit)

      # Univariate score statistics
      uniscore <- mod.scalar.score$uni

      # Effects coding
      if (isTRUE(ident == "effect")) { uniscore <- uniscore[-grep("-", uniscore$rhs), ] }

      # Expected parameter change
      epcscore <- mod.scalar.score$epc

      #### Between-Group Measurement Invariance ####

      if (isTRUE(!long)) {

        mod.scalar.score <- data.frame(label = NA, group.lhs = NA, group.rhs = NA, lhs = NA, op = NA, rhs = NA, mi = NA, df = NA, pvalue = NA, lhs.epc = NA, rhs.epc = NA, lhs.stdyx = NA, rhs.stdyx = NA)

        for (i in seq_len(nrow(uniscore))) {

          mod.scalar.score[i, ] <- data.frame(label = partable[partable$plabel == uniscore[i, "lhs"], "label"],
                                              group.lhs = partable[partable$plabel == uniscore[i, "lhs"], "group"], group.rhs = partable[partable$plabel == uniscore[i, "rhs"], "group"],
                                              lhs = paste0(partable[partable$plabel == uniscore[i, "lhs"], c("lhs", "op", "rhs")], collapse = " "), op = "==", rhs = paste0(partable[partable$plabel == uniscore[i, "rhs"], c("lhs", "op", "rhs")], collapse = " "),
                                              mi = uniscore[i, "X2"], df = uniscore[i, "df"], pvalue = uniscore[i, "p.value"],
                                              lhs.epc = epcscore[partable$plabel == uniscore[i, "lhs"], "epc"], rhs.epc = epcscore[partable$plabel == uniscore[i, "rhs"], "epc"], lhs.stdyx = epcscore[partable$plabel == uniscore[i, "lhs"], "sepc.all"], rhs.stdyx = epcscore[partable$plabel == uniscore[i, "rhs"], "sepc.all"])

        }

      #### Longitudinal Measurement Invariance ####

      } else {

        mod.scalar.score <- data.frame(label = NA, lhs = NA, op = NA, rhs = NA, mi = NA, df = NA, pvalue = NA, lhs.epc = NA, rhs.epc = NA, lhs.stdyx = NA, rhs.stdyx = NA)

        for (i in seq_len(nrow(uniscore))) {

          mod.scalar.score[i, ] <- data.frame(label = partable[partable$plabel == uniscore[i, "lhs"], "label"],
                                              lhs = paste0(partable[partable$plabel == uniscore[i, "lhs"], c("lhs", "op", "rhs")], collapse = " "), op = "==", rhs = paste0(partable[partable$plabel == uniscore[i, "rhs"], c("lhs", "op", "rhs")], collapse = " "),
                                              mi = uniscore[i, "X2"], df = uniscore[i, "df"], pvalue = uniscore[i, "p.value"],
                                              lhs.epc = epcscore[partable$plabel == uniscore[i, "lhs"], "epc"], rhs.epc = epcscore[partable$plabel == uniscore[i, "rhs"], "epc"],
                                              lhs.stdyx = epcscore[partable$plabel == uniscore[i, "lhs"], "sepc.all"], rhs.stdyx = epcscore[partable$plabel == uniscore[i, "rhs"], "sepc.all"])

        }

      }

    }

    #--------------------------------------
    ### Strict Invariance Model ####

    if (isTRUE(!is.null(mod.strict.score))) {

      # Parameter table
      partable <- lavaan::parTable(mod.strict.fit)

      # Univariate score statistics
      uniscore <- mod.strict.score$uni

      # Effects coding
      if (isTRUE(ident == "effect")) { uniscore <- uniscore[-grep("-", uniscore$rhs), ] }

      # Expected parameter change
      epcscore <- mod.strict.score$epc

      #### Between-Group Measurement Invariance ####

      if (isTRUE(!long)) {

        mod.strict.score <- data.frame(label = NA, group.lhs = NA, group.rhs = NA, lhs = NA, op = NA, rhs = NA, mi = NA, df = NA, pvalue = NA, lhs.epc = NA, rhs.epc = NA, lhs.stdyx = NA, rhs.stdyx = NA)

        for (i in seq_len(nrow(uniscore))) {

          mod.strict.score[i, ] <- data.frame(label = partable[partable$plabel == uniscore[i, "lhs"], "label"],
                                              group.lhs = partable[partable$plabel == uniscore[i, "lhs"], "group"], group.rhs = partable[partable$plabel == uniscore[i, "rhs"], "group"],
                                              lhs = paste0(partable[partable$plabel == uniscore[i, "lhs"], c("lhs", "op", "rhs")], collapse = " "), op = "==", rhs = paste0(partable[partable$plabel == uniscore[i, "rhs"], c("lhs", "op", "rhs")], collapse = " "),
                                              mi = uniscore[i, "X2"], df = uniscore[i, "df"], pvalue = uniscore[i, "p.value"],
                                              lhs.epc = epcscore[partable$plabel == uniscore[i, "lhs"], "epc"], rhs.epc = epcscore[partable$plabel == uniscore[i, "rhs"], "epc"], lhs.stdyx = epcscore[partable$plabel == uniscore[i, "lhs"], "sepc.all"], rhs.stdyx = epcscore[partable$plabel == uniscore[i, "rhs"], "sepc.all"])

        }

      #### Longitudinal Measurement Invariance ####

      } else {

        mod.strict.score <- data.frame(label = NA, lhs = NA, op = NA, rhs = NA, mi = NA, df = NA, pvalue = NA, lhs.epc = NA, rhs.epc = NA, lhs.stdyx = NA, rhs.stdyx = NA)

        for (i in seq_len(nrow(uniscore))) {

          mod.strict.score[i, ] <- data.frame(label = partable[partable$plabel == uniscore[i, "lhs"], "label"],
                                              lhs = paste0(partable[partable$plabel == uniscore[i, "lhs"], c("lhs", "op", "rhs")], collapse = " "), op = "==", rhs = paste0(partable[partable$plabel == uniscore[i, "rhs"], c("lhs", "op", "rhs")], collapse = " "),
                                              mi = uniscore[i, "X2"], df = uniscore[i, "df"], pvalue = uniscore[i, "p.value"],
                                              lhs.epc = epcscore[partable$plabel == uniscore[i, "lhs"], "epc"], rhs.epc = epcscore[partable$plabel == uniscore[i, "rhs"], "epc"], lhs.stdyx = epcscore[partable$plabel == uniscore[i, "lhs"], "sepc.all"], rhs.stdyx = epcscore[partable$plabel == uniscore[i, "rhs"], "sepc.all"])

        }

      }

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Residual Correlation Matrix ####

  mod.config.resid <- mod.thres.resid <- mod.metric.resid <- mod.scalar.resid <- mod.strict.resid <- NULL

  if (isTRUE("resid" %in% print)) {

    #--------------------------------------
    ### Configural Invariance Model ####

    if (isTRUE(!is.null(mod.config.fit))) {

      mod.config.resid <- tryCatch(suppressWarnings(lavaan::lavResiduals(mod.config.fit, type = "cor.bollen")), error = function(y) return(NULL))

      #### Continuous Indicators ####
      if (isTRUE(!ordered)) {

        # Combine residual correlation matrix and standardized residual means
        mod.config.resid <- if (isTRUE(!long)) { lapply(mod.config.resid, function(y) rbind(y$cov, mean = y$mean)) } else { rbind(mod.config.resid$cov, mean = mod.config.resid$mean) }

      #### Ordered Categorical Indicators ####
      } else {

        # Combine residual correlation matrix and standardized thresholds
        mod.config.resid <- if (isTRUE(!long)) { lapply(mod.config.resid, function(y) rbind(y$cov, matrix(y$th, ncol = ncol(y$cov), byrow = TRUE, dimnames = list(paste0("t", seq_len(nresp.cat - 1L)), colnames(x$cov))))) } else { rbind(mod.config.resid$cov, matrix(mod.config.resid$th, ncol = ncol(mod.config.resid$cov), byrow = TRUE, dimnames = list(paste0("t", seq_len(nresp.cat - 1L)), colnames(mod.config.resid$cov)))) }

      }

    }

    #--------------------------------------
    ### Threshold Invariance Model ####

    if (isTRUE(!is.null(mod.thres.fit))) {

      mod.thres.resid <- tryCatch(suppressWarnings(lavaan::lavResiduals(mod.metric.fit, type = "cor.bollen")), error = function(y) return(NULL))

      #### Continuous Indicators ####
      if (isTRUE(!ordered)) {

        # Combine residual correlation matrix and standardized residual means
        mod.thres.resid <- if (isTRUE(!long)) { lapply(mod.thres.resid, function(y) rbind(y$cov, mean = y$mean)) } else { rbind(mod.thres.resid$cov, mean = mod.thres.resid$mean) }

      #### Ordered Categorical Indicators ####
      } else {

        # Combine residual correlation matrix and standardized thresholds
        mod.thres.resid <- if (isTRUE(!long)) { lapply(mod.thres.resid, function(y) rbind(y$cov, matrix(y$th, ncol = ncol(y$cov), byrow = TRUE, dimnames = list(paste0("t", seq_len(nresp.cat - 1L)), colnames(x$cov))))) } else { rbind(mod.thres.resid$cov, matrix(mod.thres.resid$th, ncol = ncol(mod.thres.resid$cov), byrow = TRUE, dimnames = list(paste0("t", seq_len(nresp.cat - 1L)), colnames(mod.thres.resid$cov)))) }

      }

    }

    #--------------------------------------
    ### Metric Invariance Model ####

    if (isTRUE(!is.null(mod.metric.fit))) {

      mod.metric.resid <- tryCatch(suppressWarnings(lavaan::lavResiduals(mod.metric.fit, type = "cor.bollen")), error = function(y) return(NULL))

      #### Continuous Indicators ####
      if (isTRUE(!ordered)) {

        # Combine residual correlation matrix and standardized residual means
        mod.metric.resid <- if (isTRUE(!long)) { lapply(mod.metric.resid, function(y) rbind(y$cov, mean = y$mean)) } else { rbind(mod.metric.resid$cov, mean = mod.metric.resid$mean) }

      #### Ordered Categorical Indicators ####
      } else {

        # Combine residual correlation matrix and standardized thresholds
        mod.metric.resid <- if (isTRUE(!long)) { lapply(mod.metric.resid, function(y) rbind(y$cov, matrix(y$th, ncol = ncol(y$cov), byrow = TRUE, dimnames = list(paste0("t", seq_len(nresp.cat - 1L)), colnames(x$cov))))) } else { rbind(mod.metric.resid$cov, matrix(mod.metric.resid$th, ncol = ncol(mod.metric.resid$cov), byrow = TRUE, dimnames = list(paste0("t", seq_len(nresp.cat - 1L)), colnames(mod.metric.resid$cov)))) }

      }

    }

    #--------------------------------------
    ### Scalar Invariance Model ####

    if (isTRUE(!is.null(mod.scalar.fit))) {

      mod.scalar.resid <- tryCatch(suppressWarnings(lavaan::lavResiduals(mod.scalar.fit, type = "cor.bollen")), error = function(y) return(NULL))

      #### Continuous Indicators ####
      if (isTRUE(!ordered)) {

        # Combine residual correlation matrix and standardized residual means
        mod.scalar.resid <- if (isTRUE(!long)) { lapply(mod.scalar.resid, function(y) rbind(y$cov, mean = y$mean)) } else { rbind(mod.scalar.resid$cov, mean = mod.scalar.resid$mean) }

      #### Ordered Categorical Indicators ####
      } else {

        # Combine residual correlation matrix and standardized thresholds
        mod.scalar.resid <- if (isTRUE(!long)) { lapply(mod.scalar.resid, function(y) rbind(y$cov, matrix(y$th, ncol = ncol(y$cov), byrow = TRUE, dimnames = list(paste0("t", seq_len(nresp.cat - 1L)), colnames(x$cov))))) } else { rbind(mod.scalar.resid$cov, matrix(mod.scalar.resid$th, ncol = ncol(mod.scalar.resid$cov), byrow = TRUE, dimnames = list(paste0("t", seq_len(nresp.cat - 1L)), colnames(mod.scalar.resid$cov)))) }

      }

    }

    #--------------------------------------
    ### Strict Invariance Model ####

    if (isTRUE(!is.null(mod.strict.fit))) {

      mod.strict.resid <- tryCatch(suppressWarnings(lavaan::lavResiduals(mod.strict.fit, type = "cor.bollen")), error = function(y) return(NULL))

      #### Continuous Indicators ####
      if (isTRUE(!ordered)) {

        # Combine residual correlation matrix and standardized residual means
        mod.strict.resid <- if (isTRUE(!long)) { lapply(mod.strict.resid, function(y) rbind(y$cov, mean = y$mean)) } else { rbind(mod.strict.resid$cov, mean = mod.strict.resid$mean) }

        #### Ordered Categorical Indicators ####
      } else {

        # Combine residual correlation matrix and standardized thresholds
        mod.strict.resid <- if (isTRUE(!long)) { lapply(mod.strict.resid, function(y) rbind(y$cov, matrix(y$th, ncol = ncol(y$cov), byrow = TRUE, dimnames = list(paste0("t", seq_len(nresp.cat - 1L)), colnames(x$cov))))) } else { rbind(mod.strict.resid$cov, matrix(mod.strict.resid$th, ncol = ncol(mod.strict.resid$cov), byrow = TRUE, dimnames = list(paste0("t", seq_len(nresp.cat - 1L)), colnames(mod.strict.resid$cov)))) }

      }

    }

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## lavaan summary ####

  lavaan.summary <- NULL

  if (isTRUE(lavaan.run && "summary" %in% print)) {

    #--------------------------------------
    ### Cluster ####

    # Number of clusters
    cluster.unique <- 1L
    if (isTRUE(!is.null(cluster))) { if (isTRUE(!long)) { cluster.unique <- tapply(x$.cluster, x$.group, function(y) length(unique(na.omit(y)))) } else { cluster.unique <- length(unique(na.omit(x$.cluster))) } }

    #--------------------------------------
    ### Summary ####

    lavaan.summary <- data.frame(### First column
                                 label = c(paste("lavaan", lavaan::lavInspect(mod.config.fit, what = "version")), "",
                                           "Indicators", "Estimator", "Optimization Method", "",
                                           "Test Statistic", "Standard Errors", "Missing Data",  "",
                                           "Identification", "Parameterization", "", "",
                                           "Number of Model Parameters", "Number of Equality Constraints", "", "",
                                           if (isTRUE(!long)) { "Total Number of Observations" } else { "Number of Observations" }, "Number of Observations per Group", if (isTRUE(lavaan::lavInspect(mod.config.fit, what = "ngroups") == 1L)) { "1" } else { lavaan::lavInspect(mod.config.fit, what = "group.label") },
                                           "Number of Clusters", if (isTRUE(!is.null(cluster) && !is.null(group))) { lavaan::lavInspect(mod.config.fit, what = "group.label") }),
                                  ### Second column
                                  config = c("", "",
                                             # Indicators
                                             ifelse(isTRUE(!ordered), "Continuous", "Ordered Categorical"),
                                             # Estimator
                                             estimator,
                                             # Optimization method
                                             toupper(lavaan::lavTech(mod.config.fit, what = "options")$optim.method), "",
                                             # Test statistic
                                             switch(lavaan::lavTech(mod.config.fit, what = "options")$test |> (\(p) if (isTRUE(length(p) != 1L)) { misty::chr.omit(p, omit = "standard") } else { p })(),
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
                                             # Identification
                                             switch(ident, "marker" = "Marker Variable", "var" = "Std. LV", "effect" = "Effects Coding"),
                                             # Parameterization
                                             ifelse(isTRUE(parameterization == "delta"), "Delta", "Theta"), "", "Config",
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
                                    #### Continuous Indicators ####
                                    if (isTRUE(!ordered)) {

                                                 ### Third column
                                      data.frame(metric = c(rep("", times = 13L), "Metric",
                                                            # Number of model parameters
                                                            if (isTRUE(!is.null(mod.metric.fit))) { c(max(lavaan::parTable(mod.metric.fit)$free), sum(lavaan::parTable(mod.metric.fit)$op == "==")) } else { c("", "") }, "", "Total",
                                                            # Total number of observations
                                                            sum(lavaan::lavInspect(mod.config.fit, what = "norig")), "",
                                                            # Number of observations per group
                                                            lavaan::lavInspect(mod.config.fit, what = "norig"), if (isTRUE(!is.null(group) && !is.null(cluster))) { "" },
                                                            # Number of clusters
                                                            if (isTRUE(!is.null(cluster))) { rep("", times = length(cluster.unique)) } else { "" }),
                                                 ### Fourth column
                                                 scalar = c(rep("", times = 13L), "Scalar",
                                                            # Number of model parameters
                                                            if (isTRUE(!is.null(mod.scalar.fit))) { c(max(lavaan::parTable(mod.scalar.fit)$free), sum(lavaan::parTable(mod.scalar.fit)$op == "==")) } else { c("", "") }, "", "",
                                                            # Number of observations used
                                                            "", "", rep("", times = length(lavaan::lavInspect(mod.config.fit, what = "nobs"))), if (isTRUE(!is.null(group) && !is.null(cluster))) { "" },
                                                            # Number of clusters
                                                            if (isTRUE(!is.null(cluster))) { rep("", times = length(cluster.unique)) } else { "" }),
                                                 ### Fifth column
                                                 strict = c(rep("", times = 13L), "Strict",
                                                            # Number of model parameters
                                                            if (isTRUE(!is.null(mod.strict.fit))) { c(max(lavaan::parTable(mod.strict.fit)$free), sum(lavaan::parTable(mod.strict.fit)$op == "==")) } else { c("", "") }, "", "",
                                                            # Number of observations used
                                                            "", "", rep("", times = length(lavaan::lavInspect(mod.config.fit, what = "nobs"))), if (isTRUE(!is.null(group) && !is.null(cluster))) { "" },
                                                            # Number of clusters
                                                            if (isTRUE(!is.null(cluster))) { rep("", times = length(cluster.unique)) } else { "" }))

                                      #### Ordered Categorical Indicators ####
                                      } else {

                                        switch(resp.cat, "binary" = {

                                                     ### Third column
                                          data.frame(scalar = c(rep("", times = 13L), "Scalar",
                                                                # Number of model parameters
                                                                if (isTRUE(!is.null(mod.scalar.fit))) { c(max(lavaan::parTable(mod.scalar.fit)$free), sum(lavaan::parTable(mod.scalar.fit)$op == "==")) } else { c("", "") }, "", "Total",
                                                                # Total number of observations
                                                                sum(lavaan::lavInspect(mod.config.fit, what = "norig")), "",
                                                                # Number of observations per group
                                                                lavaan::lavInspect(mod.config.fit, what = "norig"), if (isTRUE(!is.null(group) && !is.null(cluster))) { "" },
                                                                # Number of clusters
                                                                if (isTRUE(!is.null(cluster))) { rep("", times = length(cluster.unique)) } else { "" }),
                                                     ### Fourth column
                                                     strict = c(rep("", times = 13L), "Strict",
                                                                # Number of model parameters
                                                                if (isTRUE(!is.null(mod.strict.fit))) { c(max(lavaan::parTable(mod.strict.fit)$free), sum(lavaan::parTable(mod.strict.fit)$op == "==")) } else { c("", "") }, "", "",
                                                                # Number of observations used
                                                                "", "", rep("", times = length(lavaan::lavInspect(mod.config.fit, what = "nobs"))), if (isTRUE(!is.null(group) && !is.null(cluster))) { "" },
                                                                # Number of clusters
                                                                if (isTRUE(!is.null(cluster))) { rep("", times = length(cluster.unique)) } else { "" }))

                                        }, "ternary" =  {

                                                     ### Third column
                                          data.frame(metric = c(rep("", times = 13L), "Metric",
                                                                # Number of model parameters
                                                                if (isTRUE(!is.null(mod.metric.fit))) { c(max(lavaan::parTable(mod.metric.fit)$free), sum(lavaan::parTable(mod.metric.fit)$op == "==")) } else { c("", "") }, "", "Total",
                                                                # Total number of observations
                                                                sum(lavaan::lavInspect(mod.config.fit, what = "norig")), "",
                                                                # Number of observations per group
                                                                lavaan::lavInspect(mod.config.fit, what = "norig"), if (isTRUE(!is.null(group) && !is.null(cluster))) { "" },
                                                                # Number of clusters
                                                                if (isTRUE(!is.null(cluster))) { rep("", times = length(cluster.unique)) } else { "" }),
                                                     ### Fourth column
                                                     scalar = c(rep("", times = 13L), "Scalar",
                                                                # Number of model parameters
                                                                if (isTRUE(!is.null(mod.scalar.fit))) { c(max(lavaan::parTable(mod.scalar.fit)$free), sum(lavaan::parTable(mod.scalar.fit)$op == "==")) } else { c("", "") }, "", "",
                                                                # Number of observations used
                                                                "", "", rep("", times = length(lavaan::lavInspect(mod.config.fit, what = "nobs"))), if (isTRUE(!is.null(group) && !is.null(cluster))) { "" },
                                                                # Number of clusters
                                                                if (isTRUE(!is.null(cluster))) { rep("", times = length(cluster.unique)) } else { "" }),
                                                     ### Fifth column
                                                     strict = c(rep("", times = 13L), "Strict",
                                                                # Number of model parameters
                                                                if (isTRUE(!is.null(mod.strict.fit))) { c(max(lavaan::parTable(mod.strict.fit)$free), sum(lavaan::parTable(mod.strict.fit)$op == "==")) } else { c("", "") }, "", "",
                                                                # Number of observations used
                                                                "", "", rep("", times = length(lavaan::lavInspect(mod.config.fit, what = "nobs"))), if (isTRUE(!is.null(group) && !is.null(cluster))) { "" },
                                                                # Number of clusters
                                                                if (isTRUE(!is.null(cluster))) { rep("", times = length(cluster.unique)) } else { "" }))

                                        }, "multiple" = {

                                                     ### Third column
                                          data.frame(thres = c(rep("", times = 13L), "Thresh",
                                                               # Number of model parameters
                                                               if (isTRUE(!is.null(mod.thres.fit))) { c(max(lavaan::parTable(mod.thres.fit)$free), sum(lavaan::parTable(mod.thres.fit)$op == "==")) } else { c("", "") }, "", "Total",
                                                               # Total number of observations
                                                               sum(lavaan::lavInspect(mod.config.fit, what = "norig")), "",
                                                               # Number of observations per group
                                                               lavaan::lavInspect(mod.config.fit, what = "norig"), if (isTRUE(!is.null(group) && !is.null(cluster))) { "" },
                                                               # Number of clusters
                                                               if (isTRUE(!is.null(cluster))) { rep("", times = length(cluster.unique)) } else { "" }),
                                                     ### Fourth column
                                                     metric = c(rep("", times = 13L), "Metric",
                                                                # Number of model parameters
                                                                if (isTRUE(!is.null(mod.metric.fit))) { c(max(lavaan::parTable(mod.metric.fit)$free), sum(lavaan::parTable(mod.metric.fit)$op == "==")) } else { c("", "") }, "", "",
                                                                # Number of observations used
                                                                "", "", rep("", times = length(lavaan::lavInspect(mod.config.fit, what = "nobs"))), if (isTRUE(!is.null(group) && !is.null(cluster))) { "" },
                                                                # Number of clusters
                                                                if (isTRUE(!is.null(cluster))) { rep("", times = length(cluster.unique)) } else { "" }),
                                                     ### Fifth column
                                                     scalar = c(rep("", times = 13L), "Scalar",
                                                                # Number of model parameters
                                                                if (isTRUE(!is.null(mod.scalar.fit))) { c(max(lavaan::parTable(mod.scalar.fit)$free), sum(lavaan::parTable(mod.scalar.fit)$op == "==")) } else { c("", "") }, "", "",
                                                                # Number of observations used
                                                                "", "", rep("", times = length(lavaan::lavInspect(mod.config.fit, what = "nobs"))), if (isTRUE(!is.null(group) && !is.null(cluster))) { "" },
                                                                # Number of clusters
                                                                if (isTRUE(!is.null(cluster))) { rep("", times = length(cluster.unique)) } else { "" }),
                                                     ### Sixth column
                                                     strict = c(rep("", times = 13L), "Strict",
                                                                # Number of model parameters
                                                                if (isTRUE(!is.null(mod.strict.fit))) { c(max(lavaan::parTable(mod.strict.fit)$free), sum(lavaan::parTable(mod.strict.fit)$op == "==")) } else { c("", "") }, "", "",
                                                                # Number of observations used
                                                                "", "", rep("", times = length(lavaan::lavInspect(mod.config.fit, what = "nobs"))), if (isTRUE(!is.null(group) && !is.null(cluster))) { "" },
                                                                # Number of clusters
                                                                if (isTRUE(!is.null(cluster))) { rep("", times = length(cluster.unique)) } else { "" }))

                                        })

                                      })

    #### Remove Empty Columns ####

    ##### Continuous Indicators
    if (isTRUE(!ordered)) {

      if (isTRUE(is.null(mod.strict.fit))) { lavaan.summary <- lavaan.summary[, setdiff(colnames(lavaan.summary), "strict")]  }

      if (isTRUE(is.null(mod.scalar.fit))) { lavaan.summary <- lavaan.summary[, setdiff(colnames(lavaan.summary), "scalar")]  }

      if (isTRUE(is.null(mod.metric.fit))) { lavaan.summary[14L:16L, "metric"] <- "" ; colnames(lavaan.summary) <- sub("metric", "", colnames(lavaan.summary)) }

    ##### Ordered Categorical Indicators
    } else {

      switch(resp.cat, "binary" = {

              if (isTRUE(is.null(mod.strict.fit))) { lavaan.summary <- lavaan.summary[, setdiff(colnames(lavaan.summary), "strict")]  }

              if (isTRUE(is.null(mod.scalar.fit))) { lavaan.summary[14L:16L, "scalar"] <- "" ; colnames(lavaan.summary) <- sub("scalar", "", colnames(lavaan.summary)) }

            }, "ternary" =  {

              if (isTRUE(is.null(mod.strict.fit))) { lavaan.summary <- lavaan.summary[, setdiff(colnames(lavaan.summary), "strict")]  }

              if (isTRUE(is.null(mod.scalar.fit))) { lavaan.summary <- lavaan.summary[, setdiff(colnames(lavaan.summary), "scalar")]  }

              if (isTRUE(is.null(mod.metric.fit))) { lavaan.summary[14L:16L, "metric"] <- "" ; colnames(lavaan.summary) <- sub("metric", "", colnames(lavaan.summary)) }

             }, "multiple" = {


               if (isTRUE(is.null(mod.strict.fit))) { lavaan.summary <- lavaan.summary[, setdiff(colnames(lavaan.summary), "strict")]  }

               if (isTRUE(is.null(mod.scalar.fit))) { lavaan.summary <- lavaan.summary[, setdiff(colnames(lavaan.summary), "scalar")]  }

               if (isTRUE(is.null(mod.metric.fit))) { lavaan.summary <- lavaan.summary[, setdiff(colnames(lavaan.summary), "metric")]  }

               if (isTRUE(is.null(mod.metric.fit))) { lavaan.summary[14L:16L, "thresh"] <- "" ; colnames(lavaan.summary) <- sub("thresh", "", colnames(lavaan.summary)) }

             })

    }

    #### Remove Number of Observations per Group ####

    if (isTRUE(is.null(group))) { lavaan.summary <- lavaan.summary[-(which(lavaan.summary$label == "Number of Observations per Group"):(which(lavaan.summary$label == "Number of Clusters") - 1L)), ] }

    #### Remove Number of Clusters ####

    if (isTRUE(is.null(cluster))) { lavaan.summary <- lavaan.summary[-which(lavaan.summary$label == "Number of Clusters"), ] }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Partial Invariance Specification Table ####

  partial.summary <- NULL
  if (isTRUE(!is.null(partial))) {

    #--------------------------------------
    ### Continuous Indicators ####

    if (isTRUE(!ordered)) {

      #### Between-Group based on Two Groups or Longitudinal Measurement Invariance ####

      if (isTRUE((!long && ngroups == 2L) || long)) {

        partial.summary <- max(sapply(partial, length)) |>  (\(p) cbind(names(partial), matrix(unlist(lapply(partial, function(y) c(unique(y), rep(NA, p - length(unique(y)))))), byrow = TRUE, ncol = p)))() |>
          (\(q) as.data.frame(matrix(q, ncol = ncol(q), byrow = FALSE, dimnames = list(NULL, c("param", paste0("X", seq_len(ncol(q) - 1L)))))) |> (\(r) r[na.omit(match(c("load", "inter", "resid"), r$param)), ])())()

      #### Between-Group based on More than Two Groups ####

      } else {

        # Split partial invariance specification
        partial.load  <- if (isTRUE("load"  %in% names(partial))) { partial[["load"]]  } else { NULL }
        partial.inter <- if (isTRUE("inter" %in% names(partial))) { partial[["inter"]] } else { NULL }
        partial.resid <- if (isTRUE("resid" %in% names(partial))) { partial[["resid"]] } else { NULL }

        if (isTRUE(!is.null(partial.load)))  { partial.load  <- lapply(paste0("g", seq_len(ngroups)), function(y) names(which(sapply(partial.load, function(z) y %in% z)))) |> (\(p) data.frame(param = "load", group = paste0("g", seq_len(ngroups)), matrix(unlist(lapply(p, function(v) c(v, rep(NA, times = max(sapply(p, length)) - length(v))))), byrow = TRUE, ncol = max(sapply(p, length)), dimnames = list(NULL, seq_len(max(sapply(p, length)))))))() }
        if (isTRUE(!is.null(partial.inter))) { partial.inter <- lapply(paste0("g", seq_len(ngroups)), function(y) names(which(sapply(partial.inter, function(z) y %in% z)))) |> (\(p) data.frame(param = "inter", group = paste0("g", seq_len(ngroups)), matrix(unlist(lapply(p, function(v) c(v, rep(NA, times = max(sapply(p, length)) - length(v))))), byrow = TRUE, ncol = max(sapply(p, length)), dimnames = list(NULL, seq_len(max(sapply(p, length)))) )))() }
        if (isTRUE(!is.null(partial.resid))) { partial.resid <- lapply(paste0("g", seq_len(ngroups)), function(y) names(which(sapply(partial.resid, function(z) y %in% z)))) |> (\(p) data.frame(param = "resid", group = paste0("g", seq_len(ngroups)), matrix(unlist(lapply(p, function(v) c(v, rep(NA, times = max(sapply(p, length)) - length(v))))), byrow = TRUE, ncol = max(sapply(p, length)), dimnames = list(NULL, seq_len(max(sapply(p, length)))))))() }

        # Combine tables
        partial.summary <- misty::df.rbind(partial.load, partial.inter, partial.resid) |> (\(p) misty::df.rename(p, from = colnames(p), to = c("param", "group", paste0("X", seq_len(ncol(p) - 2L)))))() |> (\(q) q[apply(q[, -c(1L, 2L), drop = FALSE], 1L, function(y) any(!is.na(y))), ])()

      }

    #--------------------------------------
    ### Ordered Categorical Indicators ####

    } else {

      #### Between-Group based on Two Groups or Longitudinal Measurement Invariance ####

      if (isTRUE((!long && ngroups == 2L) || long)) {

        # Split partial threshold invariance specification
        partial.thres <- if (isTRUE("thres" %in% names(partial))) { partial[["thres"]]  } else { NULL }

        # Summarize threshold specification
        if (isTRUE(!is.null(partial.thres))) { partial.thres <- lapply(paste0("t", seq_len(nresp.cat - 1L)), function(y) names(which(sapply(partial.thres, function(z) y %in% z)))) |> (\(p) data.frame(param = "thres", thres = paste0("t", seq_len(length(p))), matrix(unlist(lapply(p, function(v) c(v, rep(NA, times = max(sapply(p, length)) - length(v))))), byrow = TRUE, ncol = max(sapply(p, length)))))() }

        # Summarize loading, intercept, residual specification and combine tables
        if (isTRUE(length(partial[setdiff(names(partial), "thres")]) != 0L)) {

          partial.summary <- max(sapply(partial[setdiff(names(partial), "thres")], length)) |> (\(p) cbind(setdiff(names(partial), "thres"), matrix(unlist(lapply(partial[setdiff(names(partial), "thres")], function(y) c(y, rep(NA, p - length(y))))), byrow = TRUE, ncol = p)))() |>
            (\(q) if (isTRUE(nrow(q) == 1L)) { as.data.frame(matrix(q[na.omit(match(c("load", "inter", "resid"), q[, 1L])), ], byrow = TRUE, ncol = ncol(q), dimnames = list(NULL, c("param", paste0("X", seq_len(ncol(q) - 1L)))  ))) } else { data.frame(q[na.omit(match(c("load", "inter", "resid"), q[, 1L])), ]) } )() |>
            (\(r) misty::df.rbind(partial.thres, data.frame(param = r[, 1L], thres = NA, r[, -1L, drop = FALSE])))()

        } else {

          partial.summary <- partial.thres

        }

      ##### More than two groups
      } else {

        # Split partial invariance specification
        partial.thres <- if (isTRUE("thres" %in% names(partial))) { partial[["thres"]] } else { NULL }
        partial.load  <- if (isTRUE("load"  %in% names(partial))) { partial[["load"]]  } else { NULL }
        partial.inter <- if (isTRUE("inter" %in% names(partial))) { partial[["inter"]] } else { NULL }
        partial.resid <- if (isTRUE("resid" %in% names(partial))) { partial[["resid"]] } else { NULL }

        # Summarize threshold specification
        if (isTRUE(!is.null(partial.thres))) {

          partial.thres.temp <- NULL
          lapply(partial.thres, function(y) lapply(paste0("g", seq_len(ngroups)), function(z) names(which(sapply(y, function(v) z %in% v))))) |>
            (\(p) for (i in names(p)) { partial.thres.temp <<- rbind(partial.thres.temp, data.frame(ind = i, thres = unlist(p[[i]]), group = unlist(sapply(seq_len(length(p[[i]])), function(y) rep(paste0("g", y), times = length(p[[i]][[y]])))))) })()

          partial.thres.temp <- misty::df.wide(data.frame(idvar = paste0(partial.thres.temp$group, partial.thres.temp$thres), partial.thres.temp), idvar, ind, thres,  time = "ind", idvar = "idvar", var = "thres")

          for (i in colnames(partial.thres.temp)[-1L]) { partial.thres.temp[, i] <- ifelse(!is.na(partial.thres.temp[, i]), i, partial.thres.temp[, i]) }

          partial.thres <- data.frame(param = "thres", group = substr(partial.thres.temp$idvar, 1L, 2L), thres = substr(partial.thres.temp$idvar, 3L, 4L), t(apply(partial.thres.temp[, -1L, drop = FALSE], 1L, function(y) misty::chr.omit(y, na.omit = TRUE, check = FALSE) |> (\(p) c(p, rep(NA, times = (ncol(partial.thres.temp) - 1L) - length(p))))()))) |> (\(p) p[, sapply(p, function(y) any(!is.na(y)))])() |> (\(p) setNames(p, nm = c("param", "group", "thres", paste0("X", seq_len(ncol(p) - 3L)))) )()

        }

        # Summarize loading, intercept, and residual specification
        if (isTRUE(!is.null(partial.load))) {

          partial.load <- misty::df.wide(do.call("rbind", lapply(seq_along(partial.load), function(y) { data.frame(param = "load", group = partial.load[[y]], names(partial.load)[y]) })) |> (\(p) setNames(p, nm = c("param", "group", paste0("X", seq_len(ncol(p) - 2L)))))(), var = "X1", idvar = "group", time = "X1") |>
            (\(q) data.frame(q[, c(1L, 2L)], matrix(apply(q[, -c(1L, 2L), drop = FALSE], 1L, function(z) c(z[!is.na(z)], rep(NA, times = length(z) - length(z[!is.na(z)])))), byrow = TRUE, ncol = ncol(q[, -c(1L, 2L), drop = FALSE]))))() |>
            (\(r) r[, sapply(r, function(v) any(!is.na(v)))] )() |> (\(s) setNames(s, nm = c("group", "param", paste0("X", seq_len(ncol(s) - 2L)))))() |> (\(t) t[, c("param", "group", setdiff(colnames(t), c("param", "group")))])()

        }

        if (isTRUE(!is.null(partial.inter))) {

          partial.inter <- misty::df.wide(do.call("rbind", lapply(seq_along(partial.inter), function(y) { data.frame(param = "inter", group = partial.inter[[y]], names(partial.inter)[y]) })) |> (\(p) setNames(p, nm = c("param", "group", paste0("X", seq_len(ncol(p) - 2L)))) )(), var = "X1", idvar = "group", time = "X1") |>
            (\(q) data.frame(q[, c(1L, 2L)], matrix(apply(q[, -c(1L, 2L), drop = FALSE], 1L, function(z) c(z[!is.na(z)], rep(NA, times = length(z) - length(z[!is.na(z)])))), byrow = TRUE, ncol = ncol(q[, -c(1L, 2L), drop = FALSE]))))() |>
            (\(r) r[, sapply(r, function(v) any(!is.na(v)))])() |> (\(s) setNames(s, nm = c("group", "param", paste0("X", seq_len(ncol(s) - 2L)))))() |> (\(t) t[, c("param", "group", setdiff(colnames(t), c("param", "group")))])()

        }

        if (isTRUE(!is.null(partial.resid))) {

          partial.resid <- misty::df.wide(do.call("rbind", lapply(seq_along(partial.resid), function(y) { data.frame(param = "resid", group = partial.resid[[y]], names(partial.resid)[y]) })) |> (\(p) setNames(p, nm = c("param", "group", paste0("X", seq_len(ncol(p) - 2L)))) )(), var = "X1", idvar = "group", time = "X1") |>
            (\(q) data.frame(q[, c(1L, 2L)], matrix(apply(q[, -c(1L, 2L), drop = FALSE], 1L, function(z) c(z[!is.na(z)], rep(NA, times = length(z) - length(z[!is.na(z)])))), byrow = TRUE, ncol = ncol(q[, -c(1L, 2L), drop = FALSE]))))() |>
            (\(r) r[, sapply(r, function(v) any(!is.na(v)))])() |> (\(s) setNames(s, nm = c("group", "param", paste0("X", seq_len(ncol(s) - 2L)))))() |> (\(t) t[, c("param", "group", setdiff(colnames(t), c("param", "group")))])()
        }

        # Combine tables
        partial.summary <- misty::df.rbind(partial.thres, partial.load, partial.inter, partial.resid)

      }

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Extract and Combine Model Fit Indices ####

  fit.stand <- fit.scaled <- fit.robust <- NULL

  #--------------------------------------
  ### Configural Invariance Model ####

  if (isTRUE(!is.null(mod.config.fit) && !is.null(lav.fit.config))) {

    #### Standard fit indices
    fit.stand <- data.frame(### Fist column
                            label = c("Chi-Square Test of Model Fit", "Test statistic", if (isTRUE(!is.null(group))) { lavaan::lavInspect(mod.config.fit, what = "group.label") }, "Degrees of freedom", "P-value", "",
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
                             label = c("Chi-Square Test of Model Fit", "Test statistic", if (isTRUE(!is.null(group))) { lavaan::lavInspect(mod.config.fit, what = "group.label") }, "Degrees of freedom", "P-value", "Scaling Correction Factor", "",
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
                            label = c("Chi-Square Test of Model Fit", "Test statistic", if (isTRUE(!is.null(group))) { lavaan::lavInspect(mod.config.fit, what = "group.label") }, "Degrees of freedom", "P-value", "Scaling Correction Factor", "",
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

    #--------------------------------------
    ### Continuous Indicators ####

    if (isTRUE(!ordered)) {

      #...................
      #### Configural, Metric, Scalar, and Strict Invariance Model ####

      if (isTRUE(!is.null(mod.metric.fit) && !is.null(lav.fit.metric) && !is.null(mod.scalar.fit) && !is.null(lav.fit.scalar) && !is.null(mod.strict.fit) && !is.null(lav.fit.strict))) {

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

      #...................
      #### Configural, Metric, and Scalar Invariance Model ####

      } else if (isTRUE(!is.null(mod.metric.fit) && !is.null(lav.fit.metric) && !is.null(mod.scalar.fit) && !is.null(lav.fit.scalar))) {

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
      #### Configural and Metric Invariance Model ####

      } else if (isTRUE(!is.null(mod.metric.fit) && !is.null(lav.fit.metric))) {

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
                                            NA, lav.fit.metric["rmsea"] - lav.fit.config["rmsea"], NA, NA, NA, NA, lav.fit.metric["srmr_bentler"] - lav.fit.config["srmr_bentler"], NA,
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

      }

    #--------------------------------------
    ### Ordered Categorical Indicators ####

    } else {

      #### Binary Response Categories ####

      switch(resp.cat, "binary" = {

        ##### Configural, Scalar, and Strict Invariance Model
        if (isTRUE(!is.null(mod.scalar.fit) && !is.null(lav.fit.scalar) && !is.null(mod.strict.fit) && !is.null(lav.fit.strict))) {

          ###### Standard fit indices
          fit.stand <- data.frame(fit.stand,
                                  ### Third column
                                  scalar = c(NA, lav.fit.scalar["chisq"], if (isTRUE(!is.null(group))) { lavaan::lavInspect(mod.scalar.fit, what = "test")$standard$stat.group }, lav.fit.scalar[c("df", "pvalue")], NA,
                                             NA, lav.fit.scalar[c("cfi", "tli")], NA,
                                             NA, lav.fit.scalar[c("rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue")], NA, lav.fit.scalar["srmr_bentler"], NA,
                                             NA, lav.fit.scalar[c("aic", "bic", "bic2")]),
                                  ### Forth column
                                  strict = c(NA, lav.fit.strict["chisq"], if (isTRUE(!is.null(group))) { lavaan::lavInspect(mod.strict.fit, what = "test")$standard$stat.group }, lav.fit.strict[c("df", "pvalue")], NA,
                                             NA, lav.fit.strict[c("cfi", "tli")], NA,
                                             NA, lav.fit.strict[c("rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue")], NA, lav.fit.strict["srmr_bentler"], NA,
                                             NA, lav.fit.strict[c("aic", "bic", "bic2")]),
                                  ### Fifth column
                                  dscalar = c(NA, unlist(chidiff.conf.sca[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.conf.sca[2L, c("Df diff", "Pr(>Chisq)")]), NA,
                                              NA, lav.fit.scalar[c("cfi", "tli")] - lav.fit.config[c("cfi", "tli")], NA,
                                              NA, lav.fit.scalar["rmsea"] - lav.fit.config["rmsea"], NA, NA, NA, NA, lav.fit.scalar["srmr_bentler"] - lav.fit.config["srmr_bentler"], NA,
                                              NA, lav.fit.scalar[c("aic", "bic", "bic2")] - lav.fit.config[c("aic", "bic", "bic2")]),
                                  ### Sixth column
                                  dstrict = c(NA, unlist(chidiff.sca.str[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.sca.str[2L, c("Df diff", "Pr(>Chisq)")]), NA,
                                              NA, lav.fit.strict[c("cfi", "tli")] - lav.fit.scalar[c("cfi", "tli")], NA,
                                              NA, lav.fit.strict["rmsea"] - lav.fit.scalar["rmsea"], NA, NA, NA, NA, lav.fit.strict["srmr_bentler"] - lav.fit.scalar["srmr_bentler"], NA,
                                              NA, lav.fit.strict[c("aic", "bic", "bic2")] - lav.fit.scalar[c("aic", "bic", "bic2")]), fix.empty.names = FALSE)

          ###### Scaled fit indices
          fit.scaled <- data.frame(fit.scaled,
                                   ### Third column
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
                                   ### Fourth column
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
                                   ### Fifth column
                                   dscalar = c(NA, unlist(chidiff.conf.sca[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.conf.sca[2L, c("Df diff", "Pr(>Chisq)")]), NA, NA,
                                               NA, lav.fit.scalar[c("cfi.scaled", "tli.scaled")] - lav.fit.config[c("cfi.scaled", "tli.scaled")], NA,
                                               NA, lav.fit.scalar["rmsea.scaled"] - lav.fit.config["rmsea.scaled"], NA, NA, NA, NA, lav.fit.scalar["srmr_bentler"] - lav.fit.config["srmr_bentler"], NA,
                                               NA, lav.fit.scalar[c("aic", "bic", "bic2")] - lav.fit.config[c("aic", "bic", "bic2")]),
                                   ### Sixth column
                                   dstrict = c(NA, unlist(chidiff.sca.str[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.sca.str[2L, c("Df diff", "Pr(>Chisq)")]), NA, NA,
                                               NA, lav.fit.strict[c("cfi.scaled", "tli.scaled")] - lav.fit.scalar[c("cfi.scaled", "tli.scaled")], NA,
                                               NA, lav.fit.strict["rmsea.scaled"] - lav.fit.scalar["rmsea.scaled"], NA, NA, NA, NA, lav.fit.strict["srmr_bentler"] - lav.fit.scalar["srmr_bentler"], NA,
                                               NA, lav.fit.strict[c("aic", "bic", "bic2")] - lav.fit.scalar[c("aic", "bic", "bic2")]), fix.empty.names = FALSE)

          ###### Robust fit indices
          fit.robust <- data.frame(fit.robust,
                                   ### Third column
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
                                   ### Fourth column
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
                                   ### Fifth column
                                   dscalar = c(NA, unlist(chidiff.conf.sca[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.conf.sca[2L, c("Df diff", "Pr(>Chisq)")]), NA, NA,
                                               NA, lav.fit.scalar[c("cfi.robust", "tli.robust")] - lav.fit.config[c("cfi.robust", "tli.robust")], NA,
                                               NA, lav.fit.scalar["rmsea.robust"] - lav.fit.config["rmsea.robust"], NA, NA, NA, NA, lav.fit.scalar["srmr_bentler"] - lav.fit.config["srmr_bentler"], NA,
                                               NA, lav.fit.scalar[c("aic", "bic", "bic2")] - lav.fit.config[c("aic", "bic", "bic2")]),
                                   ### Sixth column
                                   dstrict = c(NA, unlist(chidiff.sca.str[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.sca.str[2L, c("Df diff", "Pr(>Chisq)")]), NA, NA,
                                               NA, lav.fit.strict[c("cfi.robust", "tli.robust")] - lav.fit.scalar[c("cfi.robust", "tli.robust")], NA,
                                               NA, lav.fit.strict["rmsea.robust"] - lav.fit.scalar["rmsea.robust"], NA, NA, NA, NA, lav.fit.strict["srmr_bentler"] - lav.fit.scalar["srmr_bentler"], NA,
                                               NA, lav.fit.strict[c("aic", "bic", "bic2")] - lav.fit.scalar[c("aic", "bic", "bic2")]), fix.empty.names = FALSE)

        ##### Configural and Scalar Invariance Model
        } else if (isTRUE(!is.null(mod.scalar.fit) && !is.null(lav.fit.scalar))) {

                fit.scalar.stand <- data.frame(fit.stand,
                                               ### Third column
                                               scalar = c(NA, lav.fit.scalar["chisq"], if (isTRUE(!is.null(group))) { lavaan::lavInspect(mod.scalar.fit, what = "test")$standard$stat.group }, lav.fit.scalar[c("df", "pvalue")], NA,
                                                          NA, lav.fit.scalar[c("cfi", "tli")], NA,
                                                          NA, lav.fit.scalar[c("rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue")], NA, lav.fit.scalar["srmr_bentler"], NA,
                                                          NA, lav.fit.scalar[c("aic", "bic", "bic2")]),
                                               ### Fourth column
                                               dscalar = c(NA, unlist(chidiff.conf.sca[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.conf.sca[2L, c("Df diff", "Pr(>Chisq)")]), NA,
                                                           NA, lav.fit.scalar[c("cfi", "tli")] - lav.fit.config[c("cfi", "tli")], NA,
                                                           NA, lav.fit.scalar["rmsea"] - lav.fit.config["rmsea"], NA, NA, NA, NA, lav.fit.scalar["srmr_bentler"] - lav.fit.config["srmr_bentler"], NA,
                                                           NA, lav.fit.scalar[c("aic", "bic", "bic2")] - lav.fit.config[c("aic", "bic", "bic2")]), fix.empty.names = FALSE)

                #### Scaled fit indices
                fit.scalar.scaled <- data.frame(fit.scaled,
                                                ### Third column
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
                                                ### Fourth column
                                                dmetric = c(NA, unlist(chidiff.conf.met[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.conf.met[2L, c("Df diff", "Pr(>Chisq)")]), NA, NA,
                                                            NA, lav.fit.metric[c("cfi.scaled", "tli.scaled")] - lav.fit.config[c("cfi.scaled", "tli.scaled")], NA,
                                                            NA, lav.fit.metric["rmsea.scaled"] - lav.fit.config["rmsea.scaled"], NA, NA, NA, NA, lav.fit.metric["srmr_bentler"] - lav.fit.config["srmr_bentler"], NA,
                                                            NA, lav.fit.metric[c("aic", "bic", "bic2")] - lav.fit.config[c("aic", "bic", "bic2")]),
                                                ### Fifth column
                                                dscalar = c(NA, unlist(chidiff.conf.sca[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.conf.sca[2L, c("Df diff", "Pr(>Chisq)")]), NA, NA,
                                                            NA, lav.fit.scalar[c("cfi.scaled", "tli.scaled")] - lav.fit.metric[c("cfi.scaled", "tli.scaled")], NA,
                                                            NA, lav.fit.scalar["rmsea.scaled"] - lav.fit.metric["rmsea.scaled"], NA, NA, NA, NA, lav.fit.scalar["srmr_bentler"] - lav.fit.metric["srmr_bentler"], NA,
                                                            NA, lav.fit.scalar[c("aic", "bic", "bic2")] - lav.fit.metric[c("aic", "bic", "bic2")]), fix.empty.names = FALSE)

                #### Robust fit indices
                fit.scalar.robust <- data.frame(fit.robust,
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
                                                dscalar = c(NA, unlist(chidiff.conf.sca[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.conf.sca[2L, c("Df diff", "Pr(>Chisq)")]), NA, NA,
                                                            NA, lav.fit.scalar[c("cfi.robust", "tli.robust")] - lav.fit.metric[c("cfi.robust", "tli.robust")], NA,
                                                            NA, lav.fit.scalar["rmsea.robust"] - lav.fit.metric["rmsea.robust"], NA, NA, NA, NA, lav.fit.scalar["srmr_bentler"] - lav.fit.metric["srmr_bentler"], NA,
                                                            NA, lav.fit.scalar[c("aic", "bic", "bic2")] - lav.fit.metric[c("aic", "bic", "bic2")]), fix.empty.names = FALSE)

              }

             #### Ternary Response Categories ####

             }, "ternary" =  {

               ##### Configural, Metric, Scalar, and Strict Invariance Model
               if (isTRUE(!is.null(mod.metric.fit) && !is.null(lav.fit.metric) && !is.null(mod.scalar.fit) && !is.null(lav.fit.scalar) && !is.null(mod.strict.fit) && !is.null(lav.fit.strict))) {

                 ###### Standard fit indices
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

                 ###### Scaled fit indices
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

                 ###### Robust fit indices
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

               ##### Configural, Metric, and Scalar Invariance Model
               } else if (isTRUE(!is.null(mod.metric.fit) && !is.null(lav.fit.metric) && !is.null(mod.scalar.fit) && !is.null(lav.fit.scalar))) {

                 ###### Standard fit indices
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

                 ###### Scaled fit indices
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

                 ###### Robust fit indices
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

               ##### Configural and Metric Invariance Model
               } else if (isTRUE(!is.null(mod.metric.fit) && !is.null(lav.fit.metric))) {

                 ###### Standard fit indices
                 fit.stand <- data.frame(fit.stand,
                                         ### Third column
                                         metric = c(NA, lav.fit.metric["chisq"], if (isTRUE(!is.null(group))) { lavaan::lavInspect(mod.metric.fit, what = "test")$standard$stat.group }, lav.fit.metric[c("df", "pvalue")], NA,
                                                    NA, lav.fit.metric[c("cfi", "tli")], NA,
                                                    NA, lav.fit.metric[c("rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue")], NA, lav.fit.metric["srmr_bentler"], NA,
                                                    NA, lav.fit.metric[c("aic", "bic", "bic2")]),
                                         ### Fourth column
                                         dmetric = c(NA, unlist(chidiff.conf.met[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.conf.met[2L, c("Df diff", "Pr(>Chisq)")]), NA,
                                                     NA, lav.fit.metric[c("cfi", "tli")] - lav.fit.config[c("cfi", "tli")], NA,
                                                     NA, lav.fit.metric["rmsea"] - lav.fit.config["rmsea"], NA, NA, NA, NA, lav.fit.metric["srmr_bentler"] - lav.fit.config["srmr_bentler"], NA,
                                                     NA, lav.fit.metric[c("aic", "bic", "bic2")] - lav.fit.config[c("aic", "bic", "bic2")]), fix.empty.names = FALSE)

                 ###### Scaled fit indices
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

                 ###### Robust fit indices
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

               }

             #### Multiple Response Categories ####

             }, "multiple" = {

               ##### Configural, Threshold, Metric, Scalar, and Strict Invariance Model
               if (isTRUE(!is.null(mod.thres.fit) && !is.null(lav.fit.thres) && !is.null(mod.metric.fit) && !is.null(lav.fit.metric) && !is.null(mod.scalar.fit) && !is.null(lav.fit.scalar) && !is.null(mod.strict.fit) && !is.null(lav.fit.strict))) {

                 ###### Standard fit indices
                 fit.stand <- data.frame(fit.stand,
                                         ### Third column
                                         thres = c(NA, lav.fit.thres["chisq"], if (isTRUE(!is.null(group))) { lavaan::lavInspect(mod.thres.fit, what = "test")$standard$stat.group }, lav.fit.thres[c("df", "pvalue")], NA,
                                                   NA, lav.fit.thres[c("cfi", "tli")], NA,
                                                   NA, lav.fit.thres[c("rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue")], NA, lav.fit.thres["srmr_bentler"], NA,
                                                   NA, lav.fit.thres[c("aic", "bic", "bic2")]),
                                         ### Fourth column
                                         metric = c(NA, lav.fit.metric["chisq"], if (isTRUE(!is.null(group))) { lavaan::lavInspect(mod.metric.fit, what = "test")$standard$stat.group }, lav.fit.metric[c("df", "pvalue")], NA,
                                                    NA, lav.fit.metric[c("cfi", "tli")], NA,
                                                    NA, lav.fit.metric[c("rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue")], NA, lav.fit.metric["srmr_bentler"], NA,
                                                    NA, lav.fit.metric[c("aic", "bic", "bic2")]),
                                         ### Fifth column
                                         scalar = c(NA, lav.fit.scalar["chisq"], if (isTRUE(!is.null(group))) { lavaan::lavInspect(mod.scalar.fit, what = "test")$standard$stat.group }, lav.fit.scalar[c("df", "pvalue")], NA,
                                                    NA, lav.fit.scalar[c("cfi", "tli")], NA,
                                                    NA, lav.fit.scalar[c("rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue")], NA, lav.fit.scalar["srmr_bentler"], NA,
                                                    NA, lav.fit.scalar[c("aic", "bic", "bic2")]),
                                         ### Sixth column
                                         strict = c(NA, lav.fit.strict["chisq"], if (isTRUE(!is.null(group))) { lavaan::lavInspect(mod.strict.fit, what = "test")$standard$stat.group }, lav.fit.strict[c("df", "pvalue")], NA,
                                                    NA, lav.fit.strict[c("cfi", "tli")], NA,
                                                    NA, lav.fit.strict[c("rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue")], NA, lav.fit.strict["srmr_bentler"], NA,
                                                    NA, lav.fit.strict[c("aic", "bic", "bic2")]),
                                         ### Seventh column
                                         dthres = c(NA, unlist(chidiff.conf.thr[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.conf.thr[2L, c("Df diff", "Pr(>Chisq)")]), NA,
                                                    NA, lav.fit.thres[c("cfi", "tli")] - lav.fit.config[c("cfi", "tli")], NA,
                                                    NA, lav.fit.thres["rmsea"] - lav.fit.config["rmsea"], NA, NA, NA, NA, lav.fit.thres["srmr_bentler"] - lav.fit.config["srmr_bentler"], NA,
                                                    NA, lav.fit.thres[c("aic", "bic", "bic2")] - lav.fit.config[c("aic", "bic", "bic2")]),
                                         ### Eight column
                                         dmetric = c(NA, unlist(chidiff.thr.met[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.thr.met[2L, c("Df diff", "Pr(>Chisq)")]), NA,
                                                     NA, lav.fit.metric[c("cfi", "tli")] - lav.fit.thres[c("cfi", "tli")], NA,
                                                     NA, lav.fit.metric["rmsea"] - lav.fit.thres["rmsea"], NA, NA, NA, NA, lav.fit.metric["srmr_bentler"] - lav.fit.thres["srmr_bentler"], NA,
                                                     NA, lav.fit.metric[c("aic", "bic", "bic2")] - lav.fit.thres[c("aic", "bic", "bic2")]),
                                         ### Ninth column
                                         dscalar = c(NA, unlist(chidiff.met.sca[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.met.sca[2L, c("Df diff", "Pr(>Chisq)")]), NA,
                                                     NA, lav.fit.scalar[c("cfi", "tli")] - lav.fit.metric[c("cfi", "tli")], NA,
                                                     NA, lav.fit.scalar["rmsea"] - lav.fit.metric["rmsea"], NA, NA, NA, NA, lav.fit.scalar["srmr_bentler"] - lav.fit.metric["srmr_bentler"], NA,
                                                     NA, lav.fit.scalar[c("aic", "bic", "bic2")] - lav.fit.metric[c("aic", "bic", "bic2")]),
                                         ### Tenth column
                                         dstrict = c(NA, unlist(chidiff.sca.str[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.sca.str[2L, c("Df diff", "Pr(>Chisq)")]), NA,
                                                     NA, lav.fit.strict[c("cfi", "tli")] - lav.fit.scalar[c("cfi", "tli")], NA,
                                                     NA, lav.fit.strict["rmsea"] - lav.fit.scalar["rmsea"], NA, NA, NA, NA, lav.fit.strict["srmr_bentler"] - lav.fit.scalar["srmr_bentler"], NA,
                                                     NA, lav.fit.strict[c("aic", "bic", "bic2")] - lav.fit.scalar[c("aic", "bic", "bic2")]), fix.empty.names = FALSE)

                 ###### Scaled fit indices
                 fit.scaled <- data.frame(fit.scaled,
                                          ### Third column
                                          thres = c(NA, lav.fit.thres["chisq.scaled"], if (isTRUE(!is.null(group))) {

                                            if (isTRUE(estimator %in% c("MLM", "WLSM", "DLS"))) {

                                              stat <- lavaan::lavInspect(mod.thres.fit, what = "test")$satorra.bentler$stat.group

                                            } else if(isTRUE(estimator %in% c("WLSMV", "ULSM", "MLMV", "ULSMV", "ULS"))) {

                                              stat <- lavaan::lavInspect(mod.thres.fit, what = "test")$scaled.shifted$stat.group

                                            } else {

                                              stat <- lavaan::lavInspect(mod.thres.fit, what = "test")$yuan.bentler.mplus$stat.group

                                            }

                                            if (isTRUE(is.null(stat))) {

                                              rep(NA, times = ngroups)

                                            } else {

                                              stat

                                            }

                                          }, lav.fit.thres[c("df.scaled", "pvalue.scaled", "chisq.scaling.factor")], NA,
                                          NA, lav.fit.thres[c("cfi.scaled", "tli.scaled")], NA,
                                          NA, lav.fit.thres[c("rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "rmsea.pvalue.scaled")], NA, lav.fit.thres["srmr_bentler"], NA,
                                          NA, lav.fit.thres[c("aic", "bic", "bic2")]),
                                          ### Fourth column
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
                                          ### Fifth column
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
                                          ### Sixth column
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
                                          ### Seventh column
                                          dthres = c(NA, unlist(chidiff.conf.thr[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.conf.thr[2L, c("Df diff", "Pr(>Chisq)")]), NA, NA,
                                                     NA, lav.fit.thres[c("cfi.scaled", "tli.scaled")] - lav.fit.config[c("cfi.scaled", "tli.scaled")], NA,
                                                     NA, lav.fit.thres["rmsea.scaled"] - lav.fit.config["rmsea.scaled"], NA, NA, NA, NA, lav.fit.thres["srmr_bentler"] - lav.fit.config["srmr_bentler"], NA,
                                                     NA, lav.fit.thres[c("aic", "bic", "bic2")] - lav.fit.config[c("aic", "bic", "bic2")]),
                                          ### Eight column
                                          dmetric = c(NA, unlist(chidiff.thr.met[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.thr.met[2L, c("Df diff", "Pr(>Chisq)")]), NA, NA,
                                                      NA, lav.fit.metric[c("cfi.scaled", "tli.scaled")] - lav.fit.thres[c("cfi.scaled", "tli.scaled")], NA,
                                                      NA, lav.fit.metric["rmsea.scaled"] - lav.fit.thres["rmsea.scaled"], NA, NA, NA, NA, lav.fit.metric["srmr_bentler"] - lav.fit.thres["srmr_bentler"], NA,
                                                      NA, lav.fit.metric[c("aic", "bic", "bic2")] - lav.fit.thres[c("aic", "bic", "bic2")]),
                                          ### Ninth column
                                          dscalar = c(NA, unlist(chidiff.met.sca[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.met.sca[2L, c("Df diff", "Pr(>Chisq)")]), NA, NA,
                                                      NA, lav.fit.scalar[c("cfi.scaled", "tli.scaled")] - lav.fit.metric[c("cfi.scaled", "tli.scaled")], NA,
                                                      NA, lav.fit.scalar["rmsea.scaled"] - lav.fit.metric["rmsea.scaled"], NA, NA, NA, NA, lav.fit.scalar["srmr_bentler"] - lav.fit.metric["srmr_bentler"], NA,
                                                      NA, lav.fit.scalar[c("aic", "bic", "bic2")] - lav.fit.metric[c("aic", "bic", "bic2")]),
                                          ### Tenth column
                                          dstrict = c(NA, unlist(chidiff.sca.str[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.sca.str[2L, c("Df diff", "Pr(>Chisq)")]), NA, NA,
                                                      NA, lav.fit.strict[c("cfi.scaled", "tli.scaled")] - lav.fit.scalar[c("cfi.scaled", "tli.scaled")], NA,
                                                      NA, lav.fit.strict["rmsea.scaled"] - lav.fit.scalar["rmsea.scaled"], NA, NA, NA, NA, lav.fit.strict["srmr_bentler"] - lav.fit.scalar["srmr_bentler"], NA,
                                                      NA, lav.fit.strict[c("aic", "bic", "bic2")] - lav.fit.scalar[c("aic", "bic", "bic2")]), fix.empty.names = FALSE)

                 ###### Robust fit indices
                 fit.robust <- data.frame(fit.robust,
                                          ### Third column
                                          thres = c(NA, lav.fit.thres["chisq.scaled"], if (isTRUE(!is.null(group))) {

                                            if (isTRUE(estimator %in% c("MLM", "WLSM", "DLS"))) {

                                              stat <- lavaan::lavInspect(mod.thres.fit, what = "test")$satorra.bentler$stat.group

                                            } else if(isTRUE(estimator %in% c("WLSMV", "ULSM", "MLMV", "ULSMV", "ULS"))) {

                                              stat <- lavaan::lavInspect(mod.thres.fit, what = "test")$scaled.shifted$stat.group

                                            } else {

                                              stat <- lavaan::lavInspect(mod.thres.fit, what = "test")$yuan.bentler.mplus$stat.group

                                            }

                                            if (isTRUE(is.null(stat))) {

                                              rep(NA, times = ngroups)

                                            } else {

                                              stat

                                            }

                                          }, lav.fit.thres[c("df.scaled", "pvalue.scaled", "chisq.scaling.factor")], NA,
                                          NA, lav.fit.thres[c("cfi.robust", "tli.robust")], NA,
                                          NA, lav.fit.thres[c("rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust", "rmsea.pvalue.robust")], NA, lav.fit.thres["srmr_bentler"], NA,
                                          NA, lav.fit.thres[c("aic", "bic", "bic2")]),
                                          ### Fourth column
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
                                          ### Fifth column
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
                                          ### Sixth column
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
                                          ### Seventh column
                                          dthres = c(NA, unlist(chidiff.conf.thr[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.conf.thr[2L, c("Df diff", "Pr(>Chisq)")]), NA, NA,
                                                      NA, lav.fit.thres[c("cfi.robust", "tli.robust")] - lav.fit.config[c("cfi.robust", "tli.robust")], NA,
                                                      NA, lav.fit.thres["rmsea.robust"] - lav.fit.config["rmsea.robust"], NA, NA, NA, NA, lav.fit.thres["srmr_bentler"] - lav.fit.config["srmr_bentler"], NA,
                                                      NA, lav.fit.thres[c("aic", "bic", "bic2")] - lav.fit.config[c("aic", "bic", "bic2")]),
                                          ### Eight column
                                          dmetric = c(NA, unlist(chidiff.thr.met[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.thr.met[2L, c("Df diff", "Pr(>Chisq)")]), NA, NA,
                                                      NA, lav.fit.metric[c("cfi.robust", "tli.robust")] - lav.fit.thres[c("cfi.robust", "tli.robust")], NA,
                                                      NA, lav.fit.metric["rmsea.robust"] - lav.fit.thres["rmsea.robust"], NA, NA, NA, NA, lav.fit.metric["srmr_bentler"] - lav.fit.thres["srmr_bentler"], NA,
                                                      NA, lav.fit.metric[c("aic", "bic", "bic2")] - lav.fit.thres[c("aic", "bic", "bic2")]),
                                          ### Ninth column
                                          dscalar = c(NA, unlist(chidiff.met.sca[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.met.sca[2L, c("Df diff", "Pr(>Chisq)")]), NA, NA,
                                                      NA, lav.fit.scalar[c("cfi.robust", "tli.robust")] - lav.fit.metric[c("cfi.robust", "tli.robust")], NA,
                                                      NA, lav.fit.scalar["rmsea.robust"] - lav.fit.metric["rmsea.robust"], NA, NA, NA, NA, lav.fit.scalar["srmr_bentler"] - lav.fit.metric["srmr_bentler"], NA,
                                                      NA, lav.fit.scalar[c("aic", "bic", "bic2")] - lav.fit.metric[c("aic", "bic", "bic2")]),
                                          ### Tenth column
                                          dstrict = c(NA, unlist(chidiff.sca.str[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.sca.str[2L, c("Df diff", "Pr(>Chisq)")]), NA, NA,
                                                      NA, lav.fit.strict[c("cfi.robust", "tli.robust")] - lav.fit.scalar[c("cfi.robust", "tli.robust")], NA,
                                                      NA, lav.fit.strict["rmsea.robust"] - lav.fit.scalar["rmsea.robust"], NA, NA, NA, NA, lav.fit.strict["srmr_bentler"] - lav.fit.scalar["srmr_bentler"], NA,
                                                      NA, lav.fit.strict[c("aic", "bic", "bic2")] - lav.fit.scalar[c("aic", "bic", "bic2")]), fix.empty.names = FALSE)

               ##### Configural, Threshold, Metric, and Scalar Invariance Model
               } else if (isTRUE(!is.null(mod.thres.fit) && !is.null(lav.fit.thres) && !is.null(mod.metric.fit) && !is.null(lav.fit.metric) && !is.null(mod.scalar.fit) && !is.null(lav.fit.scalar))) {

                 ###### Standard fit indices
                 fit.stand <- data.frame(fit.stand,
                                         ### Third column
                                         thres = c(NA, lav.fit.thres["chisq"], if (isTRUE(!is.null(group))) { lavaan::lavInspect(mod.thres.fit, what = "test")$standard$stat.group }, lav.fit.thres[c("df", "pvalue")], NA,
                                                   NA, lav.fit.thres[c("cfi", "tli")], NA,
                                                   NA, lav.fit.thres[c("rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue")], NA, lav.fit.thres["srmr_bentler"], NA,
                                                   NA, lav.fit.thres[c("aic", "bic", "bic2")]),
                                         ### Fourth column
                                         metric = c(NA, lav.fit.metric["chisq"], if (isTRUE(!is.null(group))) { lavaan::lavInspect(mod.metric.fit, what = "test")$standard$stat.group }, lav.fit.metric[c("df", "pvalue")], NA,
                                                    NA, lav.fit.metric[c("cfi", "tli")], NA,
                                                    NA, lav.fit.metric[c("rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue")], NA, lav.fit.metric["srmr_bentler"], NA,
                                                    NA, lav.fit.metric[c("aic", "bic", "bic2")]),
                                         ### Fifth column
                                         scalar = c(NA, lav.fit.scalar["chisq"], if (isTRUE(!is.null(group))) { lavaan::lavInspect(mod.scalar.fit, what = "test")$standard$stat.group }, lav.fit.scalar[c("df", "pvalue")], NA,
                                                    NA, lav.fit.scalar[c("cfi", "tli")], NA,
                                                    NA, lav.fit.scalar[c("rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue")], NA, lav.fit.scalar["srmr_bentler"], NA,
                                                    NA, lav.fit.scalar[c("aic", "bic", "bic2")]),
                                         ### Sixth column
                                         dthres = c(NA, unlist(chidiff.conf.thr[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.conf.thr[2L, c("Df diff", "Pr(>Chisq)")]), NA,
                                                    NA, lav.fit.thres[c("cfi", "tli")] - lav.fit.config[c("cfi", "tli")], NA,
                                                    NA, lav.fit.thres["rmsea"] - lav.fit.config["rmsea"], NA, NA, NA, NA, lav.fit.thres["srmr_bentler"] - lav.fit.config["srmr_bentler"], NA,
                                                    NA, lav.fit.thres[c("aic", "bic", "bic2")] - lav.fit.config[c("aic", "bic", "bic2")]),
                                         ### Seventh column
                                         dmetric = c(NA, unlist(chidiff.thr.met[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.thr.met[2L, c("Df diff", "Pr(>Chisq)")]), NA,
                                                     NA, lav.fit.metric[c("cfi", "tli")] - lav.fit.thres[c("cfi", "tli")], NA,
                                                     NA, lav.fit.metric["rmsea"] - lav.fit.thres["rmsea"], NA, NA, NA, NA, lav.fit.metric["srmr_bentler"] - lav.fit.thres["srmr_bentler"], NA,
                                                     NA, lav.fit.metric[c("aic", "bic", "bic2")] - lav.fit.thres[c("aic", "bic", "bic2")]),
                                         ### Eight column
                                         dscalar = c(NA, unlist(chidiff.met.sca[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.met.sca[2L, c("Df diff", "Pr(>Chisq)")]), NA,
                                                     NA, lav.fit.scalar[c("cfi", "tli")] - lav.fit.metric[c("cfi", "tli")], NA,
                                                     NA, lav.fit.scalar["rmsea"] - lav.fit.metric["rmsea"], NA, NA, NA, NA, lav.fit.scalar["srmr_bentler"] - lav.fit.metric["srmr_bentler"], NA,
                                                     NA, lav.fit.scalar[c("aic", "bic", "bic2")] - lav.fit.metric[c("aic", "bic", "bic2")]), fix.empty.names = FALSE)

                 ###### Scaled fit indices
                 fit.scaled <- data.frame(fit.scaled,
                                          ### Third column
                                          thres = c(NA, lav.fit.thres["chisq.scaled"], if (isTRUE(!is.null(group))) {

                                            if (isTRUE(estimator %in% c("MLM", "WLSM", "DLS"))) {

                                              stat <- lavaan::lavInspect(mod.thres.fit, what = "test")$satorra.bentler$stat.group

                                            } else if(isTRUE(estimator %in% c("WLSMV", "ULSM", "MLMV", "ULSMV", "ULS"))) {

                                              stat <- lavaan::lavInspect(mod.thres.fit, what = "test")$scaled.shifted$stat.group

                                            } else {

                                              stat <- lavaan::lavInspect(mod.thres.fit, what = "test")$yuan.bentler.mplus$stat.group

                                            }

                                            if (isTRUE(is.null(stat))) {

                                              rep(NA, times = ngroups)

                                            } else {

                                              stat

                                            }

                                          }, lav.fit.thres[c("df.scaled", "pvalue.scaled", "chisq.scaling.factor")], NA,
                                          NA, lav.fit.thres[c("cfi.scaled", "tli.scaled")], NA,
                                          NA, lav.fit.thres[c("rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "rmsea.pvalue.scaled")], NA, lav.fit.thres["srmr_bentler"], NA,
                                          NA, lav.fit.thres[c("aic", "bic", "bic2")]),
                                          ### Fourth column
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
                                          ### Fifth column
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
                                          ### Sixth column
                                          dthres = c(NA, unlist(chidiff.conf.thr[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.conf.thr[2L, c("Df diff", "Pr(>Chisq)")]), NA, NA,
                                                     NA, lav.fit.thres[c("cfi.scaled", "tli.scaled")] - lav.fit.config[c("cfi.scaled", "tli.scaled")], NA,
                                                     NA, lav.fit.thres["rmsea.scaled"] - lav.fit.config["rmsea.scaled"], NA, NA, NA, NA, lav.fit.thres["srmr_bentler"] - lav.fit.config["srmr_bentler"], NA,
                                                     NA, lav.fit.thres[c("aic", "bic", "bic2")] - lav.fit.config[c("aic", "bic", "bic2")]),
                                          ### Seventh column
                                          dmetric = c(NA, unlist(chidiff.thr.met[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.thr.met[2L, c("Df diff", "Pr(>Chisq)")]), NA, NA,
                                                      NA, lav.fit.metric[c("cfi.scaled", "tli.scaled")] - lav.fit.thres[c("cfi.scaled", "tli.scaled")], NA,
                                                      NA, lav.fit.metric["rmsea.scaled"] - lav.fit.thres["rmsea.scaled"], NA, NA, NA, NA, lav.fit.metric["srmr_bentler"] - lav.fit.thres["srmr_bentler"], NA,
                                                      NA, lav.fit.metric[c("aic", "bic", "bic2")] - lav.fit.thres[c("aic", "bic", "bic2")]),
                                          ### Eight column
                                          dscalar = c(NA, unlist(chidiff.met.sca[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.met.sca[2L, c("Df diff", "Pr(>Chisq)")]), NA, NA,
                                                      NA, lav.fit.scalar[c("cfi.scaled", "tli.scaled")] - lav.fit.metric[c("cfi.scaled", "tli.scaled")], NA,
                                                      NA, lav.fit.scalar["rmsea.scaled"] - lav.fit.metric["rmsea.scaled"], NA, NA, NA, NA, lav.fit.scalar["srmr_bentler"] - lav.fit.metric["srmr_bentler"], NA,
                                                      NA, lav.fit.scalar[c("aic", "bic", "bic2")] - lav.fit.metric[c("aic", "bic", "bic2")]), fix.empty.names = FALSE)

                 ###### Robust fit indices
                 fit.robust <- data.frame(fit.robust,
                                          ### Third column
                                          thres = c(NA, lav.fit.thres["chisq.scaled"], if (isTRUE(!is.null(group))) {

                                            if (isTRUE(estimator %in% c("MLM", "WLSM", "DLS"))) {

                                              stat <- lavaan::lavInspect(mod.thres.fit, what = "test")$satorra.bentler$stat.group

                                            } else if(isTRUE(estimator %in% c("WLSMV", "ULSM", "MLMV", "ULSMV", "ULS"))) {

                                              stat <- lavaan::lavInspect(mod.thres.fit, what = "test")$scaled.shifted$stat.group

                                            } else {

                                              stat <- lavaan::lavInspect(mod.thres.fit, what = "test")$yuan.bentler.mplus$stat.group

                                            }

                                            if (isTRUE(is.null(stat))) {

                                              rep(NA, times = ngroups)

                                            } else {

                                              stat

                                            }

                                          }, lav.fit.thres[c("df.scaled", "pvalue.scaled", "chisq.scaling.factor")], NA,
                                          NA, lav.fit.thres[c("cfi.robust", "tli.robust")], NA,
                                          NA, lav.fit.thres[c("rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust", "rmsea.pvalue.robust")], NA, lav.fit.thres["srmr_bentler"], NA,
                                          NA, lav.fit.thres[c("aic", "bic", "bic2")]),
                                          ### Fourth column
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
                                          ### Fifth column
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
                                          ### Sixth column
                                          dthres = c(NA, unlist(chidiff.conf.thr[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.conf.thr[2L, c("Df diff", "Pr(>Chisq)")]), NA, NA,
                                                     NA, lav.fit.thres[c("cfi.robust", "tli.robust")] - lav.fit.config[c("cfi.robust", "tli.robust")], NA,
                                                     NA, lav.fit.thres["rmsea.robust"] - lav.fit.config["rmsea.robust"], NA, NA, NA, NA, lav.fit.thres["srmr_bentler"] - lav.fit.config["srmr_bentler"], NA,
                                                     NA, lav.fit.thres[c("aic", "bic", "bic2")] - lav.fit.config[c("aic", "bic", "bic2")]),
                                          ### Seventh column
                                          dmetric = c(NA, unlist(chidiff.thr.met[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.thr.met[2L, c("Df diff", "Pr(>Chisq)")]), NA, NA,
                                                      NA, lav.fit.metric[c("cfi.robust", "tli.robust")] - lav.fit.thres[c("cfi.robust", "tli.robust")], NA,
                                                      NA, lav.fit.metric["rmsea.robust"] - lav.fit.thres["rmsea.robust"], NA, NA, NA, NA, lav.fit.metric["srmr_bentler"] - lav.fit.thres["srmr_bentler"], NA,
                                                      NA, lav.fit.metric[c("aic", "bic", "bic2")] - lav.fit.thres[c("aic", "bic", "bic2")]),
                                          ### Eight column
                                          dscalar = c(NA, unlist(chidiff.met.sca[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.met.sca[2L, c("Df diff", "Pr(>Chisq)")]), NA, NA,
                                                      NA, lav.fit.scalar[c("cfi.robust", "tli.robust")] - lav.fit.metric[c("cfi.robust", "tli.robust")], NA,
                                                      NA, lav.fit.scalar["rmsea.robust"] - lav.fit.metric["rmsea.robust"], NA, NA, NA, NA, lav.fit.scalar["srmr_bentler"] - lav.fit.metric["srmr_bentler"], NA,
                                                      NA, lav.fit.scalar[c("aic", "bic", "bic2")] - lav.fit.metric[c("aic", "bic", "bic2")]), fix.empty.names = FALSE)

               ##### Configural, Threshold, and Metric Invariance Model
               } else if (isTRUE(!is.null(mod.thres.fit) && !is.null(lav.fit.thres) && !is.null(mod.metric.fit) && !is.null(lav.fit.metric))) {

                 ###### Standard fit indices
                 fit.stand <- data.frame(fit.stand,
                                         ### Third column
                                         thres = c(NA, lav.fit.thres["chisq"], if (isTRUE(!is.null(group))) { lavaan::lavInspect(mod.thres.fit, what = "test")$standard$stat.group }, lav.fit.thres[c("df", "pvalue")], NA,
                                                   NA, lav.fit.thres[c("cfi", "tli")], NA,
                                                   NA, lav.fit.thres[c("rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue")], NA, lav.fit.thres["srmr_bentler"], NA,
                                                   NA, lav.fit.thres[c("aic", "bic", "bic2")]),
                                         ### Fourth column
                                         metric = c(NA, lav.fit.metric["chisq"], if (isTRUE(!is.null(group))) { lavaan::lavInspect(mod.metric.fit, what = "test")$standard$stat.group }, lav.fit.metric[c("df", "pvalue")], NA,
                                                    NA, lav.fit.metric[c("cfi", "tli")], NA,
                                                    NA, lav.fit.metric[c("rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue")], NA, lav.fit.metric["srmr_bentler"], NA,
                                                    NA, lav.fit.metric[c("aic", "bic", "bic2")]),
                                         ### Fifth column
                                         dthres = c(NA, unlist(chidiff.conf.thr[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.conf.thr[2L, c("Df diff", "Pr(>Chisq)")]), NA,
                                                    NA, lav.fit.thres[c("cfi", "tli")] - lav.fit.config[c("cfi", "tli")], NA,
                                                    NA, lav.fit.thres["rmsea"] - lav.fit.config["rmsea"], NA, NA, NA, NA, lav.fit.thres["srmr_bentler"] - lav.fit.config["srmr_bentler"], NA,
                                                    NA, lav.fit.thres[c("aic", "bic", "bic2")] - lav.fit.config[c("aic", "bic", "bic2")]),
                                         ### Sixth column
                                         dmetric = c(NA, unlist(chidiff.thr.met[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.thr.met[2L, c("Df diff", "Pr(>Chisq)")]), NA,
                                                     NA, lav.fit.metric[c("cfi", "tli")] - lav.fit.thres[c("cfi", "tli")], NA,
                                                     NA, lav.fit.metric["rmsea"] - lav.fit.thres["rmsea"], NA, NA, NA, NA, lav.fit.metric["srmr_bentler"] - lav.fit.thres["srmr_bentler"], NA,
                                                     NA, lav.fit.metric[c("aic", "bic", "bic2")] - lav.fit.thres[c("aic", "bic", "bic2")]), fix.empty.names = FALSE)

                 ###### Scaled fit indices
                 fit.scaled <- data.frame(fit.scaled,
                                          ### Third column
                                          thres = c(NA, lav.fit.thres["chisq.scaled"], if (isTRUE(!is.null(group))) {

                                            if (isTRUE(estimator %in% c("MLM", "WLSM", "DLS"))) {

                                              stat <- lavaan::lavInspect(mod.thres.fit, what = "test")$satorra.bentler$stat.group

                                            } else if(isTRUE(estimator %in% c("WLSMV", "ULSM", "MLMV", "ULSMV", "ULS"))) {

                                              stat <- lavaan::lavInspect(mod.thres.fit, what = "test")$scaled.shifted$stat.group

                                            } else {

                                              stat <- lavaan::lavInspect(mod.thres.fit, what = "test")$yuan.bentler.mplus$stat.group

                                            }

                                            if (isTRUE(is.null(stat))) {

                                              rep(NA, times = ngroups)

                                            } else {

                                              stat

                                            }

                                          }, lav.fit.thres[c("df.scaled", "pvalue.scaled", "chisq.scaling.factor")], NA,
                                          NA, lav.fit.thres[c("cfi.scaled", "tli.scaled")], NA,
                                          NA, lav.fit.thres[c("rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "rmsea.pvalue.scaled")], NA, lav.fit.thres["srmr_bentler"], NA,
                                          NA, lav.fit.thres[c("aic", "bic", "bic2")]),
                                          ### Fourth column
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
                                          ### Fifth column
                                          dthres = c(NA, unlist(chidiff.conf.thr[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.conf.thr[2L, c("Df diff", "Pr(>Chisq)")]), NA, NA,
                                                     NA, lav.fit.thres[c("cfi.scaled", "tli.scaled")] - lav.fit.config[c("cfi.scaled", "tli.scaled")], NA,
                                                     NA, lav.fit.thres["rmsea.scaled"] - lav.fit.config["rmsea.scaled"], NA, NA, NA, NA, lav.fit.thres["srmr_bentler"] - lav.fit.config["srmr_bentler"], NA,
                                                     NA, lav.fit.thres[c("aic", "bic", "bic2")] - lav.fit.config[c("aic", "bic", "bic2")]),
                                          ### Sixth column
                                          dmetric = c(NA, unlist(chidiff.thr.met[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.thr.met[2L, c("Df diff", "Pr(>Chisq)")]), NA, NA,
                                                      NA, lav.fit.metric[c("cfi.scaled", "tli.scaled")] - lav.fit.thres[c("cfi.scaled", "tli.scaled")], NA,
                                                      NA, lav.fit.metric["rmsea.scaled"] - lav.fit.thres["rmsea.scaled"], NA, NA, NA, NA, lav.fit.metric["srmr_bentler"] - lav.fit.thres["srmr_bentler"], NA,
                                                      NA, lav.fit.metric[c("aic", "bic", "bic2")] - lav.fit.thres[c("aic", "bic", "bic2")]), fix.empty.names = FALSE)

                 ###### Robust fit indices
                 fit.robust <- data.frame(fit.robust,
                                          ### Third column
                                          thre = c(NA, lav.fit.thres["chisq.scaled"], if (isTRUE(!is.null(group))) {

                                            if (isTRUE(estimator %in% c("MLM", "WLSM", "DLS"))) {

                                              stat <- lavaan::lavInspect(mod.thres.fit, what = "test")$satorra.bentler$stat.group

                                            } else if(isTRUE(estimator %in% c("WLSMV", "ULSM", "MLMV", "ULSMV", "ULS"))) {

                                              stat <- lavaan::lavInspect(mod.thres.fit, what = "test")$scaled.shifted$stat.group

                                            } else {

                                              stat <- lavaan::lavInspect(mod.thres.fit, what = "test")$yuan.bentler.mplus$stat.group

                                            }

                                            if (isTRUE(is.null(stat))) {

                                              rep(NA, times = ngroups)

                                            } else {

                                              stat

                                            }

                                          }, lav.fit.thres[c("df.scaled", "pvalue.scaled", "chisq.scaling.factor")], NA,
                                          NA, lav.fit.thres[c("cfi.robust", "tli.robust")], NA,
                                          NA, lav.fit.thres[c("rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust", "rmsea.pvalue.robust")], NA, lav.fit.thres["srmr_bentler"], NA,
                                          NA, lav.fit.thres[c("aic", "bic", "bic2")]),
                                          ### Fourth column
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

                                          },  lav.fit.metric[c("df.scaled", "pvalue.scaled", "chisq.scaling.factor")], NA,
                                          NA, lav.fit.metric[c("cfi.robust", "tli.robust")], NA,
                                          NA, lav.fit.metric[c("rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust", "rmsea.pvalue.robust")], NA, lav.fit.metric["srmr_bentler"], NA,
                                          NA, lav.fit.metric[c("aic", "bic", "bic2")]),
                                          ### Fifth column
                                          dthres = c(NA, unlist(chidiff.conf.thr[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.conf.thr[2L, c("Df diff", "Pr(>Chisq)")]), NA, NA,
                                                     NA, lav.fit.thres[c("cfi.robust", "tli.robust")] - lav.fit.config[c("cfi.robust", "tli.robust")], NA,
                                                     NA, lav.fit.thres["rmsea.robust"] - lav.fit.config["rmsea.robust"], NA, NA, NA, NA, lav.fit.thres["srmr_bentler"] - lav.fit.config["srmr_bentler"], NA,
                                                     NA, lav.fit.thres[c("aic", "bic", "bic2")] - lav.fit.config[c("aic", "bic", "bic2")]),
                                          ### Sixth column
                                          dmetric = c(NA, unlist(chidiff.thr.met[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.thr.met[2L, c("Df diff", "Pr(>Chisq)")]), NA, NA,
                                                      NA, lav.fit.metric[c("cfi.robust", "tli.robust")] - lav.fit.thres[c("cfi.robust", "tli.robust")], NA,
                                                      NA, lav.fit.metric["rmsea.robust"] - lav.fit.thres["rmsea.robust"], NA, NA, NA, NA, lav.fit.metric["srmr_bentler"] - lav.fit.thres["srmr_bentler"], NA,
                                                      NA, lav.fit.metric[c("aic", "bic", "bic2")] - lav.fit.thres[c("aic", "bic", "bic2")]), fix.empty.names = FALSE)

               ##### Configural and Threshold
               } else if (isTRUE(!is.null(mod.thres.fit) && !is.null(lav.fit.thres) )) {

                 ###### Standard fit indices
                 fit.stand <- data.frame(fit.stand,
                                         ### Third column
                                         thres = c(NA, lav.fit.thres["chisq"], if (isTRUE(!is.null(group))) { lavaan::lavInspect(mod.thres.fit, what = "test")$standard$stat.group }, lav.fit.thres[c("df", "pvalue")], NA,
                                                   NA, lav.fit.thres[c("cfi", "tli")], NA,
                                                   NA, lav.fit.thres[c("rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue")], NA, lav.fit.thres["srmr_bentler"], NA,
                                                   NA, lav.fit.thres[c("aic", "bic", "bic2")]),
                                         ### Fourth column
                                         dthres = c(NA, unlist(chidiff.conf.thr[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.conf.thr[2L, c("Df diff", "Pr(>Chisq)")]), NA,
                                                    NA, lav.fit.thres[c("cfi", "tli")] - lav.fit.config[c("cfi", "tli")], NA,
                                                    NA, lav.fit.thres["rmsea"] - lav.fit.config["rmsea"], NA, NA, NA, NA, lav.fit.thres["srmr_bentler"] - lav.fit.config["srmr_bentler"], NA,
                                                    NA, lav.fit.thres[c("aic", "bic", "bic2")] - lav.fit.config[c("aic", "bic", "bic2")]), fix.empty.names = FALSE)

                 ###### Scaled fit indices
                 fit.scaled <- data.frame(fit.scaled,
                                          ### Third column
                                          thres = c(NA, lav.fit.thres["chisq.scaled"], if (isTRUE(!is.null(group))) {

                                            if (isTRUE(estimator %in% c("MLM", "WLSM", "DLS"))) {

                                              stat <- lavaan::lavInspect(mod.thres.fit, what = "test")$satorra.bentler$stat.group

                                            } else if(isTRUE(estimator %in% c("WLSMV", "ULSM", "MLMV", "ULSMV", "ULS"))) {

                                              stat <- lavaan::lavInspect(mod.thres.fit, what = "test")$scaled.shifted$stat.group

                                            } else {

                                              stat <- lavaan::lavInspect(mod.thres.fit, what = "test")$yuan.bentler.mplus$stat.group

                                            }

                                            if (isTRUE(is.null(stat))) {

                                              rep(NA, times = ngroups)

                                            } else {

                                              stat

                                            }

                                          }, lav.fit.thres[c("df.scaled", "pvalue.scaled", "chisq.scaling.factor")], NA,
                                          NA, lav.fit.thres[c("cfi.scaled", "tli.scaled")], NA,
                                          NA, lav.fit.thres[c("rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "rmsea.pvalue.scaled")], NA, lav.fit.thres["srmr_bentler"], NA,
                                          NA, lav.fit.thres[c("aic", "bic", "bic2")]),
                                          ### Fourth column
                                          dthres = c(NA, unlist(chidiff.conf.thr[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.conf.thr[2L, c("Df diff", "Pr(>Chisq)")]), NA, NA,
                                                     NA, lav.fit.thres[c("cfi.scaled", "tli.scaled")] - lav.fit.config[c("cfi.scaled", "tli.scaled")], NA,
                                                     NA, lav.fit.thres["rmsea.scaled"] - lav.fit.config["rmsea.scaled"], NA, NA, NA, NA, lav.fit.thres["srmr_bentler"] - lav.fit.config["srmr_bentler"], NA,
                                                     NA, lav.fit.thres[c("aic", "bic", "bic2")] - lav.fit.config[c("aic", "bic", "bic2")]), fix.empty.names = FALSE)

                 ###### Robust fit indices
                 fit.robust <- data.frame(fit.robust,
                                          ### Third column
                                          thres = c(NA, lav.fit.thres["chisq.scaled"], if (isTRUE(!is.null(group))) {

                                            if (isTRUE(estimator %in% c("MLM", "WLSM", "DLS"))) {

                                              stat <- lavaan::lavInspect(mod.thres.fit, what = "test")$satorra.bentler$stat.group

                                            } else if(isTRUE(estimator %in% c("WLSMV", "ULSM", "MLMV", "ULSMV", "ULS"))) {

                                              stat <- lavaan::lavInspect(mod.thres.fit, what = "test")$scaled.shifted$stat.group

                                            } else {

                                              stat <- lavaan::lavInspect(mod.thres.fit, what = "test")$yuan.bentler.mplus$stat.group

                                            }

                                            if (isTRUE(is.null(stat))) {

                                              rep(NA, times = ngroups)

                                            } else {

                                              stat

                                            }

                                          }, lav.fit.thres[c("df.scaled", "pvalue.scaled", "chisq.scaling.factor")], NA,
                                          NA, lav.fit.thres[c("cfi.robust", "tli.robust")], NA,
                                          NA, lav.fit.thres[c("rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust", "rmsea.pvalue.robust")], NA, lav.fit.thres["srmr_bentler"], NA,
                                          NA, lav.fit.thres[c("aic", "bic", "bic2")]),
                                          ### Fourth column
                                          dthres = c(NA, unlist(chidiff.conf.thr[2L, "Chisq diff"]), if (isTRUE(!is.null(group))) { rep(NA, times = ngroups) }, unlist(chidiff.conf.thr[2L, c("Df diff", "Pr(>Chisq)")]), NA, NA,
                                                     NA, lav.fit.thres[c("cfi.robust", "tli.robust")] - lav.fit.config[c("cfi.robust", "tli.robust")], NA,
                                                     NA, lav.fit.thres["rmsea.robust"] - lav.fit.config["rmsea.robust"], NA, NA, NA, NA, lav.fit.thres["srmr_bentler"] - lav.fit.config["srmr_bentler"], NA,
                                                     NA, lav.fit.thres[c("aic", "bic", "bic2")] - lav.fit.config[c("aic", "bic", "bic2")]), fix.empty.names = FALSE)

               }

             })


    }

    #--------------------------------------
    ### Remove information criteria ####

    if (isTRUE(!estimator %in% c("ML", "MLM", "MLMV", "MLMVS", "MLF", "MLR"))) {

      fit.stand  <- fit.stand[-c((which(fit.stand$label == "Information Criteria") - 1L):which(fit.stand$label == "Sample-Size Adjusted BIC")), ]
      fit.scaled <- fit.scaled[-c((which(fit.scaled$label == "Information Criteria") - 1L):which(fit.scaled$label == "Sample-Size Adjusted BIC")), ]
      fit.robust <- fit.robust[-c((which(fit.robust$label == "Information Criteria") - 1L):which(fit.robust$label == "Sample-Size Adjusted BIC")), ]

    }

    #--------------------------------------
    ### Remove Scaled and Robust Model Fit Indices ####

    # Remove scaled / robust fit indices and information criteria
    if (isTRUE(estimator %in% c("ML", "MLF", "GLS", "WLS", "DWLS", "ULS", "PML"))) { fit.scaled <- fit.robust <- NULL }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Parameter Estimates Tables ####

  param.config <- param.metric <- param.thres <- param.scalar <- param.strict <- NULL

  if (isTRUE("est" %in% print)) {

    #--------------------------------------
    ### Configural invariance model

    if (isTRUE(!is.null(mod.config.fit))) { param.config <- .model.fit.param(mod.config.param, long = long) }

    #--------------------------------------
    ### Threshold invariance model

    if (isTRUE(!is.null(mod.thres.fit)))  { param.thres <- .model.fit.param(mod.thres.param, long = long)  }

    #--------------------------------------
    ### Metric invariance model

    if (isTRUE(!is.null(mod.metric.fit))) { param.metric <- .model.fit.param(mod.metric.param, long = long) }

    #--------------------------------------
    ### Scalar invariance model

    if (isTRUE(!is.null(mod.scalar.fit))) { param.scalar <- .model.fit.param(mod.scalar.param, long = long) }

    #--------------------------------------
    ### Strict invariance model

    if (isTRUE(!is.null(mod.strict.fit))) { param.strict <- .model.fit.param(mod.strict.param, long = long) }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Return Object ####

  object <- list(call = match.call(),
                 type = "item.invar",
                 data = x,
                 args = list(model = model, group = group, cluster = cluster, long = long, ordered = ordered, parameterization = parameterization, rescov = rescov, rescov.long = rescov.long, invar = invar,
                             partial = partial, ident = ident, estimator = estimator, missing = missing, null.model = null.model, print = print, print.fit = print.fit, mod.minval = mod.minval, resid.minval = resid.minval,
                             lavaan.run = lavaan.run, digits = digits, p.digits = p.digits, as.na = as.na, write = write, append = append, check = check, output = output),
                 model = list(config = mod.config, thres = mod.thres, metric = mod.metric, scalar = mod.scalar, strict = mod.strict),
                 model.fit = list(config = mod.config.fit, thres = mod.thres.fit, metric = mod.metric.fit, scalar = mod.scalar.fit, strict = mod.strict.fit),
                 check = list(config = list(vcov = mod.config.fit.check["check.vcov"], theta = mod.config.fit.check["check.theta"], cov.lv = mod.config.fit.check["check.cov.lv"]),
                              thres = list(vcov = mod.thres.fit.check["check.vcov"],  theta = mod.thres.fit.check["check.theta"],  cov.lv = mod.thres.fit.check["check.cov.lv"]),
                              metric = list(vcov = mod.metric.fit.check["check.vcov"], theta = mod.metric.fit.check["check.theta"], cov.lv = mod.metric.fit.check["check.cov.lv"]),
                              scalar = list(vcov = mod.scalar.fit.check["check.vcov"], theta = mod.scalar.fit.check["check.theta"], cov.lv = mod.scalar.fit.check["check.cov.lv"]),
                              strict = list(vcov = mod.strict.fit.check["check.vcov"], theta = mod.strict.fit.check["check.theta"], cov.lv = mod.strict.fit.check["check.cov.lv"])),
                 result = list(summary = lavaan.summary, partial = partial.summary, coverage = coverage, descript = list(stat = itemstat, freq = itemfreq),
                               fit = list(stand = fit.stand, scaled = fit.scaled, robust = fit.robust),
                               param = list(config = param.config, thres = param.thres, metric = param.metric, scalar = param.scalar, strict = param.strict),
                               modind = list(config = mod.config.modind, thres = mod.thres.modind, metric = mod.metric.modind, scalar = mod.scalar.modind, strict = mod.strict.modind),
                               score = list(thres = mod.thres.score, metric = mod.metric.score, scalar = mod.scalar.score, strict = mod.strict.score),
                               resid = list(config = mod.config.resid, thres = mod.thres.resid, metric = mod.metric.resid, scalar = mod.scalar.resid, strict = mod.strict.resid)))

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
