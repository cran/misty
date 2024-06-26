#' Confirmatory Factor Analysis
#'
#' This function is a wrapper function for conducting confirmatory factor analysis
#' with continuous and/or ordered-categorical indicators by calling the \code{cfa}
#' function in the R package \pkg{lavaan}. By default, the function provides a
#' table with univariate sample statistics, model fit information, and parameter
#' estimates. Additionally, variance-covariance coverage of the data, modification
#' indices, and residual correlation matrix can be requested by specifying the
#' argument \code{print}.
#'
#' @param ...              a matrix or data frame. If \code{model = NULL},
#'                         confirmatory factor analysis based on a measurement
#'                         model with one factor labeled \code{f} comprising all
#'                         variables in the matrix or data frame is conducted.
#'                         Note that the cluster variable is excluded from \code{x}
#'                         when specifying \code{cluster}. If \code{model} is
#'                         specified, the matrix or data frame needs to contain
#'                         all variables used in the argument \code{model} and
#'                         the cluster variable when specifying \code{cluster}.
#'                         Alternatively, an expression indicating the variable
#'                         names in \code{data} e.g.,
#'                         \code{item.cfa(x1, x2, x3, data = dat)}. Note that the
#'                         operators \code{.}, \code{+}, \code{-}, \code{~},
#'                         \code{:}, \code{::}, and \code{!} can also be used to
#'                         select variables, see 'Details' in the
#'                         \code{\link{df.subset}} function.
#' @param data             a data frame when specifying one or more variables in
#'                         the argument \code{...}. Note that the argument is
#'                         \code{NULL} when specifying a a matrix or data frame
#'                         for the argument \code{...}.
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
#'                         of the cluster variable in \code{...} or \code{data},
#'                         or a vector representing the nested grouping structure
#'                         (i.e., group or cluster variable) for computing
#'                         cluster-robust standard errors. Note that cluster-robust
#'                         standard errors are not available when treating indicators
#'                         of the measurement model as ordered (ordinal).
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
#'                         estimates, \code{"modind"} for modification indices,
#'                         and and \code{"resid"} for the residual correlation
#'                         matrix and standardized residual means By default, a
#'                         summary of the specification, model fit, and parameter
#'                         estimates are printed.
#' @param mod.minval       numeric value to filter modification indices and only
#'                         show modifications with a modification index value equal
#'                         or higher than this minimum value. By default, modification
#'                         indices equal or higher 6.63 are printed. Note that a
#'                         modification index value of 6.63 is equivalent to a
#'                         significance level of \eqn{\alpha = .01}.
#' @param resid.minval     numeric value indicating the minimum absolute residual
#'                         correlation coefficients and standardized means to
#'                         highlight in boldface. By default, absolute residual
#'                         correlation coefficients and standardized means equal
#'                         or higher 0.1 are highlighted. Note that highlighting
#'                         can be disabled by setting the minimum value to 1.
#' @param digits           an integer value indicating the number of decimal places
#'                         to be used for displaying results. Note that
#'                         loglikelihood, information criteria and chi-square
#'                         test statistic are printed with \code{digits} minus
#'                         1 decimal places.
#' @param p.digits         an integer value indicating the number of decimal places
#'                         to be used for displaying \emph{p}-values, covariance
#'                         coverage (i.e., \code{p.digits - 1}), and residual
#'                         correlation coefficients.
#' @param as.na            a numeric vector indicating user-defined missing values,
#'                         i.e. these values are converted to \code{NA} before
#'                         conducting the analysis. Note that \code{as.na()}
#'                         function is only applied to \code{x} but not to
#'                         \code{cluster}.
#' @param write            a character string naming a file for writing the output into
#'                         either a text file with file extension \code{".txt"} (e.g.,
#'                         \code{"Output.txt"}) or Excel file with file extention
#'                         \code{".xlsx"}  (e.g., \code{"Output.xlsx"}). If the file
#'                         name does not contain any file extension, an Excel file will
#'                         be written.
#' @param append           logical: if \code{TRUE} (default), output will be appended
#'                         to an existing text file with extension \code{.txt} specified
#'                         in \code{write}, if \code{FALSE} existing text file will be
#'                         overwritten.
#' @param check            logical: if \code{TRUE} (default), argument specification
#'                         is checked and convergence and model identification
#'                         checks are conducted for the estimated model.
#' @param output           logical: if \code{TRUE} (default), output is shown.
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
#' In line with the R package \pkg{lavaan}, the different versions are labeled with
#' \code{Standard}, \code{Scaled}, and \code{Robust} in the output:
#'   \itemize{
#'      \item{\code{"Standard"}}: CFI, TLI, and RMSEA without any non-normality
#'      correction. These fit measures based on the normal theory maximum
#'      likelihood test statistic are sensitive to deviations from multivariate
#'      normality of endogenous variables. Simulation studies by Brosseau-Liard
#'      et al. (2012), and Brosseau-Liard and Savalei (2014) showed that the
#'      uncorrected fit indices are affected by non-normality, especially at small
#'      and medium sample sizes (e.g., n < 500).
#'      \item{\code{"Scaled"}}: Population-corrected robust CFI, TLI, and RMSEA
#'      with ad hoc non-normality corrections that simply replace the maximum
#'      likelihood test statistic with a robust test statistic (i.e., scaled
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
#'   instead of population-corrected fit indices (\code{Scaled}) is recommended.
#'   Note that when sample size is very small (e.g., n < 200), non-normality
#'   correction does not appear to adjust fit indices sufficiently to counteract
#'   the effect of non-normality (Brosseau-Liard & Savalei, 2014).
#' }
#' \item{\strong{Modification Indices and Residual Correlation Matrix}}{The \code{item.cfa}
#' function provides modification indices and the residual correlation matrix when
#' requested by using the \code{print} argument. Modification indices (aka score
#' tests) are univariate Lagrange Multipliers (LM) representing a chi-square
#' statistic with a single degree of freedom. LM approximates the amount by which
#' the chi-square test statistic would decrease if a fixed or constrained parameter
#' is freely estimated (Kline, 2023). However, (standardized) expected parameter
#' change (EPC) values should also be inspected since modification indices are
#' sensitive to sample size. EPC values are an estimate of how much the parameter
#' would be expected to change if it were freely estimated (Brown, 2023). The residual
#' correlation matrix is computed by separately converting the sample covariance
#' and model-implied covariance matrices to correlation matrices before calculation
#' differences between observed and predicted covariances (i.e., \code{type = "cor.bollen"}).
#' As a rule of thumb, absolute correlation residuals greater than .10 indicate
#' possible evidence for poor local fit, whereas smaller correlation residuals
#' than 0.05 indicate negligible degree of model misfit (Maydeu-Olivares, 2017).
#' There is no reliable connection between the size of diagnostic statistics
#' (i.e., modification indices and residuals) and the type or amount of model
#' misspecification since (1) diagnostic statistics are themselves affected by
#' misspecification, (2) misspecification in one part of the model distorts estimates
#' in other parts of the model (i.e., error propagation), and (3) equivalent models
#' have identical residuals but contradict the pattern of causal effects (Kline, 2023).
#' Note that according to Kline' (2023) "any report of the results without information
#' about the residuals is deficient" (p. 172).}
#' }
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{item.alpha}}, \code{\link{item.omega}}, \code{\link{item.invar}},
#' \code{\link{item.scores}}, \code{\link{write.result}}
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
#' Brown, T. A. (2023). Confirmatory factor analysis. In R. H. Hoyle (Ed.),
#' \emph{Handbook of structural equation modeling} (2nd ed.) (pp. 361–379). The
#' Guilford Press.
#'
#' Kline, R. B. (2023). \emph{Principles and practice of structural equation modeling}
#' (5th ed.). Guilford Press.
#'
#' Li, L., & Bentler, P. M. (2006). Robust statistical tests for evaluating the
#' hypothesis of close fit of misspecified mean and covariance structural models.
#' \emph{UCLA Statistics Preprint #506}. University of California.
#'
#' Maydeu-Olivares, A. (2017). Assessing the size of model misfit in structural
#' equation models. \emph{Psychometrika, 82}(3), 533–558. https://doi.org/10.1007/s11336-016-9552-7
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
#'                    indicators for the factors and cluster variable}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{model}}{specified model}
#' \item{\code{model.fit}}{fitted lavaan object}
#' \item{\code{check}}{results of the convergence and model identification check}
#' \item{\code{result}}{list with result tables, i.e., \code{summary} for the
#'                      summary of the specification of the estimation method and
#'                      missing data handling in lavaan, \code{coverage} for the
#'                      variance-covariance coverage of the data, \code{descript}
#'                      for descriptive statistics, \code{fit} for model fit
#'                      \code{est} for  parameter estimates, \code{modind} for
#'                      modification indices, and \code{resid} for the residual
#'                      correlation matrices and standardized residual means}
#'
#' @note
#' The function uses the functions \code{cfa}, \code{lavInspect}, \code{lavTech},
#' \code{modindices}, \code{parameterEstimates}, \code{parTable}, and
#' \code{standardizedsolution} provided in the R package \pkg{lavaan} by Yves
#' Rosseel (2012).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load data set "HolzingerSwineford1939" in the lavaan package
#' data("HolzingerSwineford1939", package = "lavaan")
#'
#' #----------------------------------------------------------------------------
#' # Measurement model with one factor
#'
#' # Example 1a: Specification using the argument 'x'
#' item.cfa(HolzingerSwineford1939[, c("x1", "x2", "x3")])
#'
#' # Example 1b: Alternative specification using the 'data' argument
#' item.cfa(x1:x3, data = HolzingerSwineford1939)
#'
#' # Example 1c: Alternative specification using the argument 'model'
#' item.cfa(HolzingerSwineford1939, model = c("x1", "x2", "x3"))
#'
#' # Example 1d: Alternative specification using the 'data' and 'model' argument
#' item.cfa(., data = HolzingerSwineford1939, model = c("x1", "x2", "x3"))
#'
#' # Example 1e: Alternative specification using the argument 'model'
#' item.cfa(HolzingerSwineford1939, model = list(visual = c("x1", "x2", "x3")))
#'
#' # Example 1f: Alternative specification using the  'data' and 'model' argument
#' item.cfa(., data = HolzingerSwineford1939, model = list(visual = c("x1", "x2", "x3")))
#'
#' #----------------------------------------------------------------------------
#' # Measurement model with three factors
#'
#' # Example 2: Specification using the argument 'model'
#' item.cfa(HolzingerSwineford1939,
#'          model = list(visual = c("x1", "x2", "x3"),
#'                       textual = c("x4", "x5", "x6"),
#'                       speed = c("x7", "x8", "x9")))
#'
#' #----------------------------------------------------------------------------
#' # Residual covariances
#'
#' # Example 3a: One residual covariance
#' item.cfa(HolzingerSwineford1939,
#'          model = list(visual = c("x1", "x2", "x3"),
#'                       textual = c("x4", "x5", "x6"),
#'                       speed = c("x7", "x8", "x9")),
#'          rescov = c("x1", "x2"))
#'
#' # Example 3b: Two residual covariances
#' item.cfa(HolzingerSwineford1939,
#'          model = list(visual = c("x1", "x2", "x3"),
#'                       textual = c("x4", "x5", "x6"),
#'                       speed = c("x7", "x8", "x9")),
#'          rescov = list(c("x1", "x2"), c("x4", "x5")))
#'
#' #----------------------------------------------------------------------------
#' # Second-order factor model based on three first-order factors
#'
#' # Example 4
#' item.cfa(HolzingerSwineford1939,
#'          model = list(visual = c("x1", "x2", "x3"),
#'                       textual = c("x4", "x5", "x6"),
#'                       speed = c("x7", "x8", "x9")),
#'          hierarch = TRUE)
#'
#' #----------------------------------------------------------------------------
#' # Measurement model with ordered-categorical indicators
#'
#' # Example 5
#' item.cfa(round(HolzingerSwineford1939[, c("x4", "x5", "x6")]), ordered = TRUE)
#'
#' #----------------------------------------------------------------------------
#' # Cluster-robust standard errors
#'
#' # Load data set "Demo.twolevel" in the lavaan package
#' data("Demo.twolevel", package = "lavaan")
#'
#' # Example 6a: Specification using a variable in 'x'
#' item.cfa(Demo.twolevel[, c("y4", "y5", "y6", "cluster")], cluster = "cluster")
#'
#' # Example 6b: Specification of the cluster variable in 'cluster'
#' item.cfa(Demo.twolevel[, c("y4", "y5", "y6")], cluster = Demo.twolevel$cluster)
#'
#' # Example 6c: Alternative specification using the 'data' argument
#' item.cfa(y4:y6, data = Demo.twolevel, cluster = "cluster")
#'
#' #----------------------------------------------------------------------------
#' # Print argument
#'
#' # Example 7a: Request all results
#' item.cfa(HolzingerSwineford1939[, c("x1", "x2", "x3")], print = "all")
#'
#' # Example 7b: Request modification indices with value equal or higher than 5
#' item.cfa(HolzingerSwineford1939[, c("x1", "x2", "x3", "x4")],
#'          print = "modind", mod.minval = 5)
#'
#' #----------------------------------------------------------------------------
#' # lavaan summary of the estimated model
#'
#' # Example 8
#' mod <- item.cfa(HolzingerSwineford1939[, c("x1", "x2", "x3")], output = FALSE)
#'
#' lavaan::summary(mod$model.fit, standardized = TRUE, fit.measures = TRUE)
#'
#' #----------------------------------------------------------------------------
#' # Write Results
#'
#' # Example 9a: Write Results into a text file
#' item.cfa(HolzingerSwineford1939[, c("x1", "x2", "x3")], write = "CFA.txt")
#'
#' # Example 9b: Write Results into a Excel file
#' item.cfa(HolzingerSwineford1939[, c("x1", "x2", "x3")], write = "CFA.xlsx")
#'
#' result <- item.cfa(HolzingerSwineford1939[, c("x1", "x2", "x3")], output = FALSE)
#' write.result(result, "CFA.xlsx")
#' }
item.cfa <- function(..., data = NULL, model = NULL, rescov = NULL, hierarch = FALSE,
                     meanstructure = TRUE, ident = c("marker", "var", "effect"),
                     parameterization = c("delta", "theta"), ordered = NULL, cluster = NULL,
                     estimator = c("ML", "MLM", "MLMV", "MLMVS", "MLF", "MLR",
                                   "GLS", "WLS", "DWLS", "WLSM", "WLSMV",
                                   "ULS", "ULSM", "ULSMV", "DLS", "PML"),
                     missing = c("listwise", "pairwise", "fiml", "two.stage", "robust.two.stage", "doubly.robust"),
                     print = c("all", "summary", "coverage", "descript", "fit", "est", "modind", "resid"),
                     mod.minval = 6.63, resid.minval = 0.1, digits = 3, p.digits = 3, as.na = NULL, write = NULL,
                     append = TRUE, check = TRUE, output = TRUE) {

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

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data using the argument 'data' ####

  if (isTRUE(!is.null(data))) {

    # Variable names
    var.names <- .var.names(..., data = data, cluster = cluster, check.chr = "a matrix or data frame")

    # Extract data
    x <- data[, var.names]

    # Cluster variable
    if (isTRUE(!is.null(cluster))) { cluster <- data[, cluster] }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data without using the argument 'data' ####

  } else {

    # Extract data
    x <- eval(..., enclos = parent.frame())

    # Data and cluster
    var.group <- .var.group(data = x, cluster = cluster)

    # Data
    if (isTRUE(!is.null(var.group$data)))  { x <- var.group$data }

    # Cluster variable
    if (isTRUE(!is.null(var.group$cluster))) { cluster <- var.group$cluster }

  }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # R package lavaan
    if (isTRUE(!nzchar(system.file(package = "lavaan")))) { stop("Package \"lavaan\" is needed for this function, please install the package.", call. = FALSE) }

    # Check input 'x'
    if (isTRUE(is.null(model) && ncol(data.frame(x)) < 3L)) { stop("Please specify at least three indicators for the measurement model in 'x'.", call. = FALSE) }

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
    if (isTRUE(!all(estimator %in% c("ML", "MLM", "MLMV", "MLMVS", "MLF", "MLR", "GLS", "WLS", "DWLS", "WLSM", "WLSMV", "ULS", "ULSM", "ULSMV", "DLS", "PML")))) { stop("Character string in the argument 'estimator' does not match with \"ML\", \"MLM\", \"MLMV\", \"MLMVS\", \"MLF\", \"MLR\", \"GLS\", \"WLS\", \"DWLS\", \"WLSM\", \"WLSMV\", \"ULS\", \"ULSM\", \"ULSMV\", \"DLS\", or \"PML\".", call. = FALSE) }

    # Check input 'missing'
    if (isTRUE(!all(missing %in% c("listwise", "pairwise", "fiml", "two.stage", "robust.two.stage", "doubly.robust")))) { stop("Character string in the argument 'missing' does not match with \"listwise\", \"pairwise\", \"fiml\", \"two.stage\", \"robust.two.stage\", or \"doubly.robust\".", call. = FALSE) }

    # Check input 'print'
    if (isTRUE(!all(print %in% c("all", "summary", "coverage", "descript", "fit", "est", "modind", "resid")))) { stop("Character strings in the argument 'print' do not all match with \"summary\", \"coverage\", \"descript\", \"fit\", \"est\", \"modind\", or \"resid\".", call. = FALSE) }

    # Check input 'mod.minval'
    if (isTRUE(mod.minval <= 0L)) { stop("Please specify a value greater than 0 for the argument 'mod.minval'.", call. = FALSE) }

    ## Check input 'resid.minval'
    if (isTRUE(resid.minval < 0L)) { stop("Please specify a value greater than or equal 0 for the argument 'resid.minval'.", call. = FALSE) }

    # Check input 'digits'
    if (isTRUE(digits %% 1L != 0L || digits < 0L || digits == 0L)) { stop("Specify a positive integer number for the argument 'digits'.", call. = FALSE) }

    # Check input 'p.digits'
    if (isTRUE(p.digits %% 1L != 0L || p.digits < 0L)) { stop("Specify a positive integer number for the argument 'p.digits'.", call. = FALSE) }

    # Check input 'append'
    if (isTRUE(!is.logical(append))) { stop("Please specify TRUE or FALSE for the argument 'append'.", call. = FALSE) }

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

  if (isTRUE(!is.null(as.na))) { x[, var] <- .as.na(x[, var], na = as.na) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model ####

  # Factor labels
  if (isTRUE(!is.null(model) && is.list(model) && (is.null(names(model)) || any(names(model) == "")))) { names(model) <- paste0("f", seq_along(model)) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Residual covariance ####

  if (isTRUE(!is.null(rescov) && !is.list(rescov))) { rescov <- list(rescov) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Hierarch ####

  if (isTRUE(hierarch)) { if (isTRUE(is.null(model) || !is.list(model) || length(model) < 3L)) { stop("Please specify at least three first-order factors for the second-order factor model.", call. = FALSE) } }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model identification ####

  if (isTRUE(all(c("marker", "var", "effect") %in% ident))) {

    if (isTRUE(hierarch)) {

      ident  <- "marker"

    } else {

      ident <- "var"

    }

  } else if (isTRUE(length(ident) != 1)) {

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
  ## Parameterization ####

  if (isTRUE(all(c("delta", "theta") %in% parameterization))) { parameterization <- "delta" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Estimator ####

  #...................
  ### Default setting ####
  if (isTRUE(all(c("ML", "MLM", "MLMV", "MLMVS", "MLF", "MLR", "GLS", "WLS", "DWLS", "WLSM", "WLSMV", "ULS", "ULSM", "ULSMV", "DLS", "PML") %in% estimator))) {

    # Continuous indicators
    if (isTRUE(is.null(ordered))) {

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

    print  <- c("summary", "descript", "fit", "est")

  } else if (isTRUE(length(print) == 1L && "all" %in% print)) {

    print <- c("all", "summary", "coverage", "descript", "fit", "est", "modind", "resid")

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

  model.fit <- suppressWarnings(lavaan::cfa(mod.factor, data = x, ordered = ordered,
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

    if (isTRUE(lavaan::lavInspect(model.fit, what = "fit")["df"] < 0L)) { stop("CFA model has negative degrees of freedom, model is not identified.", call. = FALSE) }

    #...................
    ### Model convergence ####

    if (isTRUE(!lavaan::lavInspect(model.fit, what = "converged"))) { stop("CFA model did not converge.", call. = FALSE) }

    #...................
    ### Standard error ####

    if (isTRUE(any(is.na(unlist(lavaan::lavInspect(model.fit, what = "se")))))) { stop("Standard errors could not be computed.", call. = FALSE) }

    #...................
    ### Variance-covariance matrix of the estimated parameters ####

    eigvals <- eigen(lavaan::lavInspect(model.fit, what = "vcov"), symmetric = TRUE, only.values = TRUE)$values

    # Correct for equality constraints
    if (isTRUE(any(lavaan::parTable(model.fit)$op == "=="))) { eigvals <- rev(eigvals)[-seq_len(sum(lavaan::parTable(model.fit)$op == "=="))] }

    if (isTRUE(min(eigvals) < .Machine$double.eps^(3L/4L))) {

      warning("The variance-covariance matrix of the estimated parameters is not positive definite. This may be a symptom that the model is not identified.", call. = FALSE)

      check.vcov <- FALSE

    }

    #...................
    ### Negative variance of observed variables ####

    if (isTRUE(any(diag(lavaan::lavInspect(model.fit, what = "theta")) < 0L))) {

      warning("Some estimated variances of the observed variables are negative.")

      check.theta <- FALSE

    } else if (isTRUE(any(eigen(lavaan::lavTech(model.fit, what = "theta")[[1L]], symmetric = TRUE, only.values = TRUE)$values < (-1L * .Machine$double.eps^(3/4))))) {

      warning("The model-implied variance-covariance matrix of the residuals of the observed variables is not positive definite.", call. = FALSE)

      check.theta <- FALSE

    }

    #...................
    ### Negative variance of latent variables ####

    if (isTRUE(any(diag(lavaan::lavTech(model.fit, what = "cov.lv")[[1L]]) < 0L))) {

      warning("Some estimated variances of the latent variables are negative.", call. = FALSE)

      check.cov.lv <- FALSE

    # Model-implied variance-covariance matrix of the latent variables
    } else if (isTRUE(any(eigen(lavaan::lavTech(model.fit, what = "cov.lv")[[1L]], symmetric = TRUE, only.values = TRUE)$values < (-1L * .Machine$double.eps^(3/4))))) {

      warning("The model-implied variance-covariance matrix of the latent variables is not positive definite.", call. = FALSE)

      check.cov.lv <- FALSE

    }

  } else {

    check.vcov <- check.theta <- check.cov.lv <- NULL

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model fit ####

  lav.fit <- lavaan::fitmeasures(model.fit)

  # Saturated model
  if (isTRUE(lav.fit["df"] == 0L)) {

    lav.fit[c("cfi.robust", "tli.robust")] <- 1
    lav.fit[c("rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")] <- 0

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Parameter estimates ####

  model.param <- data.frame(lavaan::parameterEstimates(model.fit),
                            stdyx = lavaan::standardizedsolution(model.fit)[, "est.std"])[, c("lhs", "op", "rhs", "est", "se", "z", "pvalue", "stdyx")]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Modification indices ####

  if (isTRUE(check.vcov && estimator != "PML")) {

    model.modind <- lavaan::modindices(model.fit)

  } else {

    model.modind <- NULL

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Residual Correlation Matrix ####

  model.resid <- tryCatch(suppressWarnings(lavaan::lavResiduals(model.fit, type = "cor.bollen")),
                          error = function(y) { return(NULL) })

  #_____________________________________________________________________________
  #
  # Return object --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## lavaan summary ####

  lavaan.summary <- data.frame(### First column
                               c(paste("lavaan", lavaan::lavInspect(model.fit, what = "version")), "", "Estimator", "Optimization Method", "",
                                "Test Statistic", "Standard Errors", "Missing Data", "",
                                "Indicators", "Identification", "", "Number of Model Parameters", "", "",
                                "Number of Observations", "Number of Clusters"),
                               ### Second column
                               c("", "",
                                 # Estimator
                                 estimator,
                                 # Optimization method
                                 toupper(lavaan::lavTech(model.fit, what = "options")$optim.method), "",
                                 # Test statistic
                                 switch(lavaan::lavTech(model.fit, what = "options")$test,
                                        "standard" = "Conventional",
                                        "satorra.bentler" = "Satorra-Bentler",
                                        "scaled.shifted" = "Scale-Shifted",
                                        "mean.var.adjusted" = "Satterthwaite",
                                        "yuan.bentler.mplus" = "Yuan-Bentler"),
                                 # Standard errors
                                 switch(lavaan::lavTech(model.fit, what = "options")$se,
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
                                 # Variables
                                 ifelse(is.null(ordered), "Continuous",
                                        ifelse(isTRUE(ordered), "Ordered-Categorical",
                                               ifelse(all(var %in% ordered), "Ordered", "Continous and Ordered"))),
                                 # Identification
                                 switch(ident,
                                        "marker" = "Marker Variable",
                                        "var" = "Factor Variance",
                                        "effect" = "Effects Coding"), "",
                                 # Number of Model Parameters
                                 max(lavaan::parTable(model.fit)$free),"", "Used",
                                 # Number of observations
                                 lavaan::lavInspect(model.fit, what = "nobs"),
                                 # Number of clusters
                                 ifelse(!is.null(cluster),
                                        length(unique(x[lavaan::lavInspect(model.fit, "case.idx"), ".cluster"])), 1L)),
                               ### Third column
                               c(rep("", times = 14L), "Total", lavaan::lavInspect(model.fit, what = "norig"), ""),
                               fix.empty.names = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model fit ####

  model.fit.measures <- data.frame(# Fist column
                                   c("Loglikelihood",
                                     "H0 Value, Specified Model", "Scaling Correction Factor", "H1 Value, Unrestricted Model", "Scaling Correction Factor", "",
                                     "Information Criteria", "Akaike (AIC)", "Bayesian (BIC)", "Sample-Size Adjusted BIC", "",
                                     "Chi-Square Test of Model Fit", "Test statistic", "Degrees of freedom", "P-value", "Scaling Correction Factor", "",
                                     "Incremental Fit Indices", "CFI", "TLI", "",
                                     "Absolute Fit Indices", "RMSEA", "90 Percent CI - lower", "90 Percent CI - upper", "P-value RMSEA <= 0.05", "", "SRMR"),
                                   # Second column
                                   standard = c(# Loglikelihood
                                                NA, lav.fit[c("logl", "scaling.factor.h0", "unrestricted.logl", "scaling.factor.h1")], NA, NA,
                                                # Information criteria
                                                lav.fit["aic"], lav.fit["bic"], lav.fit["bic2"], NA, NA,
                                                # Test statistic, df, and p-value
                                                lav.fit["chisq"], lav.fit["df"], lav.fit["pvalue"], NA, NA, NA,
                                                # CFI / TLI
                                                lav.fit["cfi"], lav.fit["tli"], NA, NA,
                                                # RMSEA
                                                lav.fit["rmsea"], lav.fit["rmsea.ci.lower"], lav.fit["rmsea.ci.upper"], lav.fit["rmsea.pvalue"], NA,
                                                # SRMR
                                                ifelse(isTRUE(lavaan::lavInspect(model.fit, what = "meanstructure")), lav.fit["srmr_bentler"], lav.fit["srmr_bentler_nomean"])),
                                   # Third column
                                   scaled = c(# Loglikelihood and Information criteria
                                              rep(NA, times = 12L),
                                              # Test statistic, df, p-value, and scaling correction factor
                                              lav.fit["chisq.scaled"], lav.fit["df.scaled"], lav.fit["pvalue.scaled"], lav.fit["chisq.scaling.factor"], NA, NA,
                                              # CFI / TLI
                                              lav.fit["cfi.scaled"], lav.fit["tli.scaled"], NA, NA,
                                              # RMSEA
                                              ifelse(isTRUE(lav.fit["df"] == 0L && estimator == "PML"), NA, lav.fit["rmsea.scaled"]),
                                              lav.fit["rmsea.ci.lower.scaled"], lav.fit["rmsea.ci.upper.scaled"], lav.fit["rmsea.pvalue.scaled"], NA,
                                              # SRMR
                                              NA),
                                   # Fourth column
                                   robust = c(rep(NA, times = 18L),
                                              # CFI / TLI
                                              ifelse(isTRUE(lav.fit["df"] == 0L && estimator %in% c("MLR", "WLSM", "ULSM")), NA, lav.fit["cfi.robust"]),
                                              ifelse(isTRUE(lav.fit["df"] == 0L && estimator %in% c("MLR", "WLSM", "ULSM")), NA, lav.fit["tli.robust"]), NA, NA,
                                              # RMSEA
                                              lav.fit["rmsea.robust"], lav.fit["rmsea.ci.lower.robust"], lav.fit["rmsea.ci.upper.robust"],
                                              rep(NA, times = 3L)),
                                   fix.empty.names = FALSE)


  if (isTRUE(estimator %in% c("ML", "MLF", "GLS", "WLS", "DWLS", "ULS"))) {

    model.fit.measures <- model.fit.measures[-c(1L, 3L, 5L, 16L), c(1L, 2L)]

  } else if (isTRUE(estimator %in% c("MLMV", "MLMVS"))) {

    model.fit.measures <- model.fit.measures[-c(1L, 3L, 5L), ]

  } else if (isTRUE(estimator %in% c("MLM", "WLSM", "ULSM", "DLS", "WLSMV", "ULSMV"))) {

    model.fit.measures <- model.fit.measures[-c(1L:11L), ]

  } else if (isTRUE(estimator %in% "PML")) {

    model.fit.measures <- model.fit.measures[-c(1L:11L), c(1L:3L)]

  }

  # Zero degrees of freedom
  if (isTRUE(lav.fit["df"] == 0)) { model.fit.measures <- model.fit.measures[-which(model.fit.measures[, 1L] %in% c("Scaling Correction Factor", "P-value", "P-value RMSEA <= 0.05")), ] }

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

    model.modind <- misty::df.rename(model.modind[, c("lhs", "op", "rhs", "mi", "epc", "sepc.all")], from = "sepc.all", to = "stdyx")

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Residual Correlation Matrix ####

  # Combine residual correlation matrix and standardized residual means
  if (isTRUE(!is.null(model.resid))) {

    model.resid <- do.call("rbind", model.resid[c("cov", "mean")])

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Return object ####

  object <- list(call = match.call(),
                 type = "item.cfa",
                 data = x,
                 args = list(model = model, rescov = rescov, hierarch = hierarch,
                             meanstructure = meanstructure, ident = ident,
                             parameterization = parameterization, ordered = ordered,
                             cluster = cluster, estimator = estimator, missing = missing,
                             print = print, mod.minval = mod.minval, resid.minval = resid.minval,
                             digits = digits, p.digits = p.digits,
                             as.na = as.na, write = write, append = append,
                             check = check, output = output),
                 model = mod.factor,
                 model.fit = model.fit,
                 check = list(vcov = check.vcov, theta = check.theta, cov.lv = check.cov.lv),
                 result = list(summary = lavaan.summary, coverage = coverage,
                               descript = itemstat, itemfreq = itemfreq,
                               fit = model.fit.measures, param = model.param,
                               modind = model.modind, resid = model.resid))

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

      if (isTRUE(append && file.exists(write))) { write("", file = write, append = TRUE) }

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
