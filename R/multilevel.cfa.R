#' Multilevel Confirmatory Factor Analysis
#'
#' This function conducts multilevel confirmatory factor analysis to investigate
#' different types of constructs with distinct construct meanings at the individual
#' and cluster level (Stapleton et al., 2016) by calling the \code{cfa} function
#' in the R package \pkg{lavaan}.  By default, the function specifies and estimates
#' a measurement model for an individual and configural construct to provides a
#' table with univariate sample statistics, model fit information, and parameter
#' estimates. Additionally, variance-covariance coverage of the data, modification
#' indices, residual correlation matrix, and relative Opdyke distribution percentile
#' matrix can be requested by specifying the argument \code{print}.
#'
#' @param data          a data frame. If \code{model}, \code{model.w}, and \code{model.b}
#'                      are \code{NULL}, multilevel confirmatory factor analysis
#'                      based on a measurement model with one factor labeled \code{wf}
#'                      at the Within level and one factor labeled \code{bf} at the
#'                      Between level comprising all variables in the data frame
#'                      is conducted. Note that the cluster variable specified in
#'                      \code{cluster} is excluded from \code{data} when specifying
#'                      the argument \code{cluster} using the variable name of the
#'                      cluster variable. If \code{model} or \code{mode.w}
#'                      and \code{model.b} is specified, the data frame needs to
#'                      contain all variables used in the \code{model} argument(s).
#' @param ...           an expression indicating the variable names in \code{data}.
#'                      Note that the operators \code{+}, \code{-},
#'                      \code{~}, \code{:}, \code{::}, and \code{!} can also be
#'                      used to select variables, see 'Details' in the
#'                      \code{\link{df.subset}} function.
#' @param cluster       either a character string indicating the variable name of
#'                      the cluster variable in \code{data} or \code{data}, or a
#'                      vector representing the nested grouping structure (i.e.,
#'                      group or cluster variable).
#' @param model         a character vector for specifying the same factor structure
#'                      with one factor at the Within and Between Level, or a list
#'                      of character vectors for specifying the same measurement
#'                      model with more than one factor at the Within and Between
#'                      Level, e.g.,\code{model = c("x1", "x2", "x3", "x4")} for
#'                      specifying a measurement model with one factor labeled \code{wf}
#'                      at the Within level and a measurement model with one factor
#'                      labeled \code{bf} at the Between level each comprising four
#'                      indicators, or \code{model = list(factor1 = c("x1", "x2", "x3", "x4"),
#'                      factor2 = c("x5", "x6", "x7", "x8"))} for specifying a
#'                      measurement model with two latent factors labeled \code{wfactor1}
#'                      and \code{wfactor2} at the Within level and a measurement
#'                      model with two latent factors labeled \code{bfactor1} and
#'                      \code{bfactor2} at the Between level each comprising four
#'                      indicators. Note that the name of each list element is used
#'                      to label factors, where prefixes \code{w} and \code{b} are
#'                      added the labels to distinguish factor labels at the Within
#'                      and Between level, i.e., all list elements need to be named,
#'                      otherwise factors are labeled with \code{"wf1", "wf2", "wf3"}
#'                      for labels at the Within level and \code{"bf1", "bf2", "bf3"}
#'                      for labels at the Between level and so on.
#' @param rescov        a character vector or a list of character vectors for specifying
#'                      residual covariances at the Within level, e.g. \code{rescov = c("x1", "x2")}
#'                      for specifying a residual covariance between indicators \code{x1}
#'                      and \code{x2} at the Within level or \code{rescov = list(c("x1", "x2"), c("x3", "x4"))}
#'                      for specifying residual covariances between indicators \code{x1}
#'                      and \code{x2}, and indicators \code{x3} and \code{x4} at
#'                      the Within level. Note that residual covariances at the
#'                      Between level can only be specified by using the arguments
#'                      \code{model.w} and \code{model.b}.
#' @param model.w       a character vector specifying a measurement model with one
#'                      factor at the Within level, or a list of character vectors
#'                      for specifying a measurement model with more than one factor
#'                      at the Within level.
#' @param model.b       a character vector specifying a measurement model with one
#'                      factor at the Between level, or a list of character vectors
#'                      for specifying a measurement model with more than one factor
#'                      at the Between level.
#' @param rescov.w      a character vector or a list of character vectors for
#'                      specifying residual covariances at the Within level.
#'                      Note that this argument applies only when the model is
#'                      specified by using the arguments
#'                      \code{model.w} and \code{model.b}.
#' @param rescov.b      a character vector or a list of character vectors for
#'                      specifying residual covariances at the Between level.
#'                      Note that this argument applies only when the model is
#'                      specified by using the arguments
#'                      \code{model.w} and \code{model.b}.
#' @param const         a character string indicating the type of construct(s), i.e.,
#'                      \code{"within"} for within-cluster constructs, \code{"shared"}
#'                      for shared cluster-level constructs, \code{"config"} (default)
#'                      for configural cluster constructs, and \code{"shareconf"}
#'                      for simultaneous shared and configural cluster constructs.
#' @param fix.resid     a character vector for specifying residual variances to be
#'                      fixed at 0 at the Between level, e.g., \code{fix.resid = c("x1", "x3")}
#'                      to fix residual variances of indicators \code{x1} and \code{x2}
#'                      at the Between level at 0. Note that it is also possible
#'                      to specify \code{fix.resid = "all"} which fixes all residual
#'                      variances at the Between level at 0 in line with the strong
#'                      factorial measurement invariance assumption across cluster.
#' @param ident         a character string indicating the method used for identifying
#'                      and scaling latent variables, i.e., \code{"marker"} for the
#'                      marker variable method fixing the first factor loading of
#'                      each latent variable to 1, \code{"var"} for the fixed variance
#'                      method fixing the variance of each latent variable to 1,
#'                      or \code{"effect"} for the effects-coding method using equality
#'                      constraints so that the average of the factor loading for
#'                      each latent variable equals 1.
#' @param ls.fit        logical: if \code{TRUE} (default) level-specific fit indices
#'                      are computed when specifying a model using the arguments
#'                      \code{model.w} and \code{model.b} given the model does not
#'                      contain any cross-level equality constraints.
#' @param estimator     a character string indicating the estimator to be used:
#'                      \code{"ML"} for maximum likelihood with conventional standard
#'                      errors and \code{"MLR"} (default) for maximum likelihood
#'                      with Huber-White robust standard errors and a scaled test
#'                      statistic that is asymptotically equal to the Yuan-Bentler
#'                      test statistic. Note that by default, full information maximum
#'                      likelihood (FIML) method is used to deal with missing data
#'                      when using \code{"ML"} (\code{missing = "fiml"}), whereas
#'                      incomplete cases are removed listwise (i.e., \code{missing = "listwise"})
#'                      when using \code{"MLR"}.
#' @param test          a character string indicating the test statistics, i.e.,
#'                      \code{"none"} for no test statistic, \code{"standard"}
#'                      for a conventional chi-square test, \code{"yuan.bentler"}
#'                      for a Yuan-Bentler scaled test statistic, and \code{"yuan.bentler.mplus"}
#'                      for a test statistic which is asymptotically equivalent to
#'                      the Yuan-Bentler T2-star test statistic. Note that the
#'                      default setting is depending on the argument \code{estimator},
#'                      i.e., \code{test = "standard"} when \code{estimator = "ML"}
#'                      and \code{test = "yuan.bentler.mplus"} when \code{estimator = "MLR"}.
#' @param se            a character string indicating the standard errors, i.e.,
#'                      \code{"none"} for no standard errors, \code{"standard"}
#'                      for conventional standard error based on inverting the
#'                      expected observed or first.order information matrix, and
#'                      \code{"robust.huber.white"} for the 'MLR' (aka pseudo ML,
#'                      Huber-White) approach. Note that the default setting is
#'                      depending on the argument \code{estimator}, i.e.,
#'                      \code{test = "standard"} when \code{estimator = "ML"}
#'                      and \code{test = "robust.huber.white"} when \code{estimator = "MLR"}.
#' @param optim.method  a character string indicating the optimizer, i.e., \code{"nlminb"}
#'                      (default) for the unconstrained and bounds-constrained
#'                      quasi-Newton method optimizer and \code{"em"} for the
#'                      Expectation Maximization (EM) algorithm.
#' @param missing       a character string indicating how to deal with missing data,
#'                      i.e., \code{"listwise"} for listwise deletion or \code{"fiml"}
#'                      (default) for full information maximum likelihood (FIML)
#'                      method. Note that it takes longer to estimate the model
#'                      using FIML, and that FIML is prone to convergence issues
#'                      which might be resolved by switching to listwise deletion.
#' @param print         a character string or character vector indicating which
#'                      results to show on the console, i.e. \code{"all"} for all
#'                      results, \code{"summary"} for a summary of the specification
#'                      of the estimation method and missing data handling in lavaan,
#'                      \code{"coverage"} for the variance-covariance coverage of
#'                      the data, \code{"descript"} for descriptive statistics,
#'                      \code{"fit"} for model fit,  \code{"est"} for parameter
#'                      estimates, \code{"modind"} for modification indices,
#'                      \code{"resid"} for the residual correlation matrix and
#'                      standardized residual means, and \code{"opdyke"} for the
#'                      discrepancy between model-implied and observed correlation
#'                      expressed in terms of the relative percentile of an Opdyke
#'                      distribution (McNeish, 2025). By default, a summary of
#'                      the specification, model fit, and parameter estimates are
#'                      printed. By default, a summary of the specification,
#'                      descriptive statistics, model fit, and parameter estimates
#'                      are printed.
#' @param mod.minval    numeric value to filter modification indices and only show
#'                      modifications with a modification index value equal or higher
#'                      than this minimum value. By default, modification indices
#'                      equal or higher 6.63 are printed. Note that a modification
#'                      index value of 6.63 is equivalent to a significance level
#'                      of \eqn{\alpha = .01}.
#' @param resid.minval  numeric value indicating the minimum absolute residual
#'                      correlation coefficients and standardized means to highlight
#'                      By default, absolute residual correlation coefficients and
#'                      standardized means equal or higher 0.1 are highlighted.
#'                      Note that highlighting can be disabled by setting the
#'                      minimum value to 1 or by setting the argument \code{color}
#'                      to \code{"default"}.
#' @param opdyke.prec   a numeric value indicating the precision of the probability
#'                      density function calculations of the Opdyke distribution.
#'                      The default is \code{1} which calculates the PDF
#'                      for polar angles between \eqn{(0, pi)} in \eqn{0.01}
#'                      increments. Specifying \code{10} calculates the PDF
#'                      polar angles between \eqn{(0, pi)} in 0.001 increments,
#'                      which takes considerably longer, especially if there
#'                      are many correlation elements.
#' @param opdyke.minmax a numeric vector with two elements indicating the
#'                      minimum and maximum percentile of the Opdyke distribution
#'                      that is considered to be acceptably close to the
#'                      observed correlation represented by the Opdyke distribution
#'                      median. Predicted correlation outside the range will be
#'                      color highlighted in line with to the argument \code{color}.
#' @param color         a character string indicating the text color for
#'                      highlighting absolute residual correlation coefficients
#'                      and standardized means equal or higher \code{resid.minval}
#'                      and predicted correlations outside the minimum and
#'                      maximum percentile of the Opdyke distribution, i.e.,
#'                      \code{"default"} for the default text color without
#'                      color coding and various text colors for highlighting
#'                      \code{"red"}, \code{"b.red"} (default), \code{"green"},
#'                      \code{"b.green"}, \code{"blue"}, or \code{"b.blue"},
#'                      see the help page of the \code{\link{chr.color}} function.
#'                      Note that this option is not supported when using R
#'                      Markdown and when writing the output into a text file
#'                      (\code{.txt}).
#' @param style         a character vector indicating the font style for
#'                      highlighting absolute residual correlation coefficients
#'                      and standardized means equal or higher \code{resid.minval},
#'                      i.e., \code{"regular"} (default) for regular text, \code{"bold"}
#'                      for bold text, and \code{"italic"} for italic text. Note
#'                      that the font style \code{"bold"} and \code{"italic"} can
#'                      be combined, i.e., style = c("bold", "italic") provides a
#'                      bold and italic text. Note that the argument \code{color}
#'                      needs to be specified to change the style of the text, e.g.
#'                      \code{color = "black"} and \code{style = "bold"} to for
#'                      bold text.
#' @param digits        an integer value indicating the number of decimal places
#'                      to be used for displaying results. Note that loglikelihood,
#'                      information criteria and chi-square test statistic are
#'                      printed with \code{digits} minus 1 decimal places.
#' @param p.digits      an integer value indicating the number of decimal places
#'                      to be used for displaying \emph{p}-values, covariance
#'                      coverage (i.e., \code{p.digits - 1}), and residual
#'                      correlation coefficients.
#' @param as.na         a numeric vector indicating user-defined missing values,
#'                      i.e. these values are converted to \code{NA} before conducting
#'                      the analysis. Note that \code{as.na()} function is only
#'                      applied to \code{data} but not to \code{cluster}.
#' @param write         a character string naming a file for writing the output into
#'                      either a text file with file extension \code{".txt"} (e.g.,
#'                      \code{"Output.txt"}) or Excel file with file extension
#'                      \code{".xlsx"} (e.g., \code{"Output.xlsx"}). If the file
#'                      name does not contain any file extension, an Excel file will
#'                      be written.
#' @param append        logical: if \code{TRUE} (default), output will be appended
#'                      to an existing text file with extension \code{.txt} specified
#'                      in \code{write}, if \code{FALSE} existing text file will be
#'                      overwritten.
#' @param check         logical: if \code{TRUE} (default), argument specification, convergence
#'                      and model identification is checked.
#' @param output        logical: if \code{TRUE} (default), output is shown.
#'
#' @details
#' Stapleton et al. (2016) identified different types of constructs when data are
#' collected from individuals nested within clusters characterized by different
#' interpretations of construct meaning. At the individual level, the hypothesized
#' construct is expected to be relevant to individual item responses, i.e., scores
#' on the measure reflect individual variability. At the cluster level, there are
#' two types of constructs measured using item responses from individuals:
#' \emph{shared construct}, a characteristic of the cluster itself and
#' \emph{configural construct}, which reflects a construct at the individual level.
#' Marsh et al. (2012) introduced the terms \emph{climate construct} and
#' \emph{contextual construct}, while Lüdtke et al., (2011) used the term
#' \emph{reflective construct} and \emph{formative construct} to differentiate
#' between shared and configural constructs.
#'
#' Five types of confirmatory factor analysis (CFA) models were proposed for construct
#' validation with nested data, depending on whether the construct of interest exists
#' solely at the individual level or is also relevant at the cluster level (Stapleton
#' et al., 2016).
#'
#' \describe{
#'    \item{\strong{Model 1: Individual Constructs, Single-Level Model}}{Model 1 is
#'     a single-level CFA model whose parameter estimates provide an aggregate of
#'     the relationship between item responses and the individual construct. A design-based
#'     estimation approach is used to adjust the standard error estimates, chi-square test
#'     statistic, and fit indices to account for the dependency of item responses
#'     among individuals within clusters. Note that this model is estimated using
#'     the \code{item.cfa} with specifying the \code{cluster} argument.
#'
#'     This model is appropriate when a positive ICC(1) is spurious, resulting from a
#'     selection process. For example, lactose intolerance is an entirely individual
#'     characteristic, unaffected by environment. However, item responses measuring
#'     lactose intolerance might exhibit a non-negligible ICC(1) due to the selection
#'     of individuals across different clusters, rather than reflecting a true
#'     cluster-level construct. That is, lactose intolerance is a completely
#'     individual measure, not able to be influenced by environmental effect.
#'
#'     The model is designed for application across a broad population that may or may
#'     not be nested within clusters in a single-level model. However, if the measure
#'     aims to compare the relative positions of individuals within a cluster, a
#'     within-cluster approach is more suitable.}
#'
#'    \item{\strong{Model 2: Within-Cluster Constructs}}{Model 2 is a two-level CFA model
#'     specified by \code{const = "within"} that includes a within-cluster construct
#'     representing an individual's standing within the cluster. The model does not assume
#'     the existence of a cluster-level construct but allows for cluster-level variability
#'     in each measure through a saturated variance-covariance model.
#'
#'     The model is designed for application within clusters to compare individuals and
#'     their relative positions within a specific cluster, while between-cluster constructs
#'     are not meaningful or not of interest.}
#'
#'     \item{\strong{Model 3: Shared Cluster-Level Constructs}}{Model 3 is a two-level
#'     CFA model specified by \code{const = "shared"} that includes a between-cluster
#'     construct representing a cluster-level characteristic that is measured using
#'     individuals within a cluster as the information source. The model does not assume
#'     the existence of an individual-level construct but allows for individual-level
#'     variability in each measure through a saturated variance-covariance model.
#'
#'     For a shared construct, individual item responses should be interchangeable,
#'     reflecting perfect interrater reliability and complete within-cluster agreement.
#'     However, within-cluster variation can exist for shared constructs, as these
#'     constructs stem from the experiences, attitudes, perceptions, values, or cognitions
#'     of individual cluster members. Given evidence of adequate agreement within clusters,
#'     the aggregate value of the measure can be assigned to the cluster, i.e., the shared
#'     perceptions of individuals in a cluster are interpreted as a proxy for the
#'     cluster-level characteristic (Jak et al., 2023). From this perspective, the greater
#'     the agreement within the cluster, the more reliably the shared construct is measured,
#'     although perfect agreement is not required. A reasonable step in the shared construct
#'     modeling process is to examine the ICC(2) values for each item measuring the shared
#'     construct. Klein et al. (2000) suggested that ICC(2) values of at least 0.7 indicate
#'     acceptable reliability of the measured shared construct, values between 0.5 and 0.7
#'     indicate marginal reliability, and values below 0.5 are considered poor.
#'
#'     Note that the wording of questionnaire items plays a crucial role in the context
#'     of multilevel measurement. For example, the statement "The instructor presents
#'     materials in ways that keep it interesting." would likely to elicit responses
#'     reflecting the instructor's qualities, whereas the statement "I find the class
#'     meeting interest." would likely to reflect both a characteristic of the cluster
#'     and the individual's intrinsic interest in the class topic. Accordingly, an argument
#'     should be made regarding how the item wording targets cluster characteristics.}
#'
#'     \item{\strong{Model 4: Individual and Configural Constructs}}{Model 4 is a two-level
#'     CFA model specified by \code{const = "config"} that includes a within-cluster
#'     construct representing an individual's standing within the cluster and a
#'     between-cluster construct representing an aggregate of the measurements of
#'     individuals within the cluster. The configural cluster construct model requires
#'     cross-level measurement invariance of factor loadings to interpret the individual-
#'     and cluster-level common factors as reflecting the individual- and cluster-level
#'     components of the same construct (Jak et al., 2023).
#'
#'     For a configural construct, individual item responses are not interchangeable, and
#'     it is not expected that individuals within a cluster respond similarly to  the items.
#'     Marsh et al. (2012) noted that if item responses have an ICC(1) of zero, there is
#'     little justification for continuing to examine the configural cluster-level construct.
#'     However, Stapleton et al. (2016) argued that it may still be of interest to investigate
#'     the variability in individual hypothetical latent scores across clusters, i.e.,
#'     the dispersion of the construct may differ across clusters and represent an important
#'     cluster characteristic.}
#'
#'     \item{\strong{Model 5: Simultaneous Shared and Configural Constructs}}{Model 5 is
#'     a two-level CFA model specified by \code{const = "shareconf"} that includes a
#'     within-cluster construct representing an individual's standing within the cluster
#'     and two between-cluster constructs: a configural construct representing the
#'     individual-level construct, and a shared construct modeling additional covariation
#'     among cluster-average item responses, representing rater effects. The factor loadings
#'     of the configural construct are constrained to be equal across levels, i.e., metric
#'     cross-level measurement invariance.
#'
#'     For example, suppose that teachers have provided ratings of their students' motivation
#'     using multiple items for each child. It is possible that some teachers rate more
#'     positively compared to others, resulting in two sources of covariation at the
#'     cluster level: variation due to the fact that students in some classes are truly
#'     more motivated on average than those in other classes, and variation due to rater
#'     effects, because some teachers tend to rate their students more positively or more
#'     negatively on average.
#'
#'     Note that what may appear to be an additional shared construct could instead reflect
#'     a spurious contextual effect. Specifically, when constraining factor loadings across
#'     levels results in a relatively poor fit compared to a model with unconstrained
#'     loadings in a configural model, this suggests that the apparent additional shared
#'     construct is actually due to measurement noninvariance at the within-cluster level.}
#' }
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{item.cfa}}, \code{\link{multilevel.fit}}, \code{\link{multilevel.invar}},
#' \code{\link{multilevel.omega}}, \code{\link{multilevel.cor}}, \code{\link{multilevel.descript}},
#' \code{\link{write.result}}
#'
#' @references
#' Jak, S., Jorgensen, T. D., ten Hove, D., & Nevicka, B. (2023). Modeling cluster-level
#' constructs measured by individual responses: Configuring a shared approach.
#' \emph{Advances in Methods and Practices in Psychological Science, 6}(3),
#' Article 25152459231182319. https://doi.org/10.1177/25152459231182319
#'
#' Klein, K. J., & Kozlowski, S. W. (2000). Multilevel theory, research, and methods
#' in organizations: Foundations, extensions, and new directions. Jossey-Bass.
#'
#' Lüdtke, O., Marsh, H. W., Robitzsch, A., & Trautwein, U. (2011). A 2x2 taxonomy of
#' multilevel latent contextual models: Accuracy-bias trade-offs in full and partial
#' error correction models. \emph{Psychological Methods, 16}, 444-467.
#' https://doi.org/10.1037/a0024376
#'
#' Marsh, H. W., Ludtke, O., Nagengast, B., Trautwein, U., Morin, A. J. S., Abduljabbar, A.
#' S., & Koller, O. (2012). Classroom climate and contextual effects: Conceptual and
#' methodological issues in the evaluation of group-level effects. \emph{Educational Psychologist, 47},
#' 106-124. https://doi.org/10.1080/00461520.2012.670488
#'
#' McNeish, D. (2025). Less Heuristic Approximate Local Fit Evaluation in Structural
#' Equation Models. \emph{Structural Equation Modeling: A Multidisciplinary Journal, 32}(4),
#' 590-605. https://doi.org/10.1080/10705511.2025.2473342
#'
#' Rosseel, Y. (2012). lavaan: An R Package for Structural Equation Modeling.
#' \emph{Journal of Statistical Software, 48}, 1-36. https://doi.org/10.18637/jss.v048.i02
#'
#' Stapleton, L. M., Yang, J. S., & Hancock, G. R. (2016). Construct meaning
#' in multilevel settings. \emph{Journal of Educational and Behavioral Statistics, 41},
#' 481-520. http://dx.doi.org/10.3102/1076998616646200
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{data}}{data frame used for the current analysis}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{model}}{specified model}
#' \item{\code{model.fit}}{fitted lavaan object (\code{mod.fit})}
#' \item{\code{check}}{results of the convergence and model identification check}
#' \item{\code{result}}{list with result tables, i.e., \code{summary} for the
#'                      summary of the specification of the estimation method
#'                      and missing data handling in lavaan, \code{coverage} for
#'                      the variance-covariance coverage of the data, \code{descript}
#'                      for descriptive statistics, \code{fit} for model fit,
#'                      \code{est} for a list with parameter estimates for Within
#'                      and Between, \code{score} fir a list with modification
#'                      indices for parameter constraints for Within and Between,
#'                      \code{resid} for a list with residual correlation matrices
#'                      and standardized residual means for the Within and Between
#'                      level, and \code{opdyke} for the relative Opdyke distribution
#'                      percentile matrix for the Within and Between level}
#'
#' @note
#' The function uses the functions \code{cfa}, \code{lavInspect},\code{lavResiduals},
#' \code{lavTech}, \code{lavTestScore}, \code{modindices}, \code{parameterEstimates},
#' and \code{standardizedsolution} provided in the R package \pkg{lavaan} by Yves
#' Rosseel (2012).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load data set "Demo.twolevel" in the lavaan package
#' data("Demo.twolevel", package = "lavaan")
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Model Specification Using 'data' for a One-Factor Model
#' #
#' # Same factor structure with one factor at the Within and Between Level
#'
#' #——————————————————————————————————————
#' ## Cluster Variable Specification
#'
#' # Example 1a: Specification using the argument '...'
#' multilevel.cfa(Demo.twolevel, y1:y4, cluster = "cluster")
#'
#' # Example 1b: Alternative specification with cluster variable 'cluster' in 'data'
#' multilevel.cfa(Demo.twolevel[, c("y1", "y2", "y3", "y4", "cluster")], cluster = "cluster")
#'
#' # Example 1c: Alternative specification with cluster variable 'cluster' not in 'data'
#' multilevel.cfa(Demo.twolevel[, c("y1", "y2", "y3", "y4")], cluster = Demo.twolevel$cluster)
#'
#' #——————————————————————————————————————
#' ## Type of Construct
#'
#' # Example 2a: Within-cluster constructs
#' multilevel.cfa(Demo.twolevel, y1:y4, cluster = "cluster", const = "within")
#'
#' # Example 2b: Shared cluster-level construct
#' multilevel.cfa(Demo.twolevel, y1:y4, cluster = "cluster", const = "shared")
#'
#' # Example 2c: Individual and configural cluster construct (default)
#' multilevel.cfa(Demo.twolevel, y1:y4, cluster = "cluster", const = "config")
#'
#' # Example 2d: Simultaneous shared and configural cluster construct
#' multilevel.cfa(Demo.twolevel, y1:y4, cluster = "cluster", const = "shareconf")
#'
#' #——————————————————————————————————————
#' ## Residual Covariances at the Within Level
#'
#' # Example 3a: Residual covariance between 'y1' and 'y3'
#' multilevel.cfa(Demo.twolevel, y1:y4, cluster = "cluster", rescov = c("y1", "y3"))
#'
#' # Example 3b: Residual covariance between 'y1' and 'y3', and 'y2' and 'y4'
#' multilevel.cfa(Demo.twolevel, y1:y4, cluster = "cluster",
#'                rescov = list(c("y1", "y3"), c("y2", "y4")))
#'
#' #——————————————————————————————————————
#' ## Residual Variances at the Between Level fixed at 0
#'
#' # Example 4a: All residual variances fixed at 0
#' # i.e., strong factorial invariance across clusters
#' multilevel.cfa(Demo.twolevel, y1:y4, cluster = "cluster", fix.resid = "all")
#'
#' # Example 4b: Residual variances of 'y1', 'y2', and 'y4' fixed at 0
#' # i.e., partial strong factorial invariance across clusters
#' multilevel.cfa(Demo.twolevel, y1:y4, cluster = "cluster", fix.resid = c("y1", "y2"))
#'
#' #——————————————————————————————————————
#' ## Arguments 'print', 'mod.minval', 'resid.minval', and 'opdyke.minmax'
#'
#' # Example 5a: Request all results
#' multilevel.cfa(Demo.twolevel, y1:y6, cluster = "cluster", print = "all")
#'
#' # Example 5b: Request modification indices with value equal or higher than 2
#' multilevel.cfa(Demo.twolevel, y1:y6, cluster = "cluster", print = "modind", mod.minval = 2)
#'
#' # Example 5c: Highlight absolute residual correlation equal or higher than 0.05
#' multilevel.cfa(Demo.twolevel, y1:y6, cluster = "cluster", print = "resid", resid.minval = 0.05,
#'                color = "b.blue")
#'
#' # Example 5d: Highlight Opdyke distribution percentiles outside 0.45 and 0.55
#' multilevel.cfa(Demo.twolevel, y1:y6, cluster = "cluster", print = "opdyke", opdyke.minmax = c(0.45, 0.55),
#'                color = "black", style = "bold")
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Model Specification using 'model' for One or Multiple-Factor Model
#' #
#' # Same factor structure at the Within and Between Level
#'
#' # Example 6a: One-factor model
#' multilevel.cfa(Demo.twolevel, cluster = "cluster", model = c("y1", "y2", "y3", "y4"))
#'
#' # Example 6b: Two-factor model
#' multilevel.cfa(Demo.twolevel, cluster = "cluster",
#'                model = list(c("y1", "y2", "y3"), c("y4", "y5", "y6")))
#'
#' # Example 6c: Two-factor model with user-specified labels for the factors
#' multilevel.cfa(Demo.twolevel, cluster = "cluster",
#'                model = list(factor1 = c("y1", "y2", "y3"), factor2 = c("y4", "y5", "y6")))
#'
#' #——————————————————————————————————————
#' # Type of Construct
#'
#' # Example 7a: Within-cluster constructs
#' multilevel.cfa(Demo.twolevel, cluster = "cluster", const = "within",
#'                model = list(c("y1", "y2", "y3"), c("y4", "y5", "y6")))
#'
#' # Example 7b: Shared cluster-level construct
#' multilevel.cfa(Demo.twolevel, cluster = "cluster", const = "shared",
#'                model = list(c("y1", "y2", "y3"), c("y4", "y5", "y6")))
#'
#' # Example 7c: Individual and configural cluster construct (default)
#' multilevel.cfa(Demo.twolevel, cluster = "cluster", const = "config",
#'                model = list(c("y1", "y2", "y3"), c("y4", "y5", "y6")))
#'
#' # Example 7d: Simultaneous shared and configural cluster construct
#' multilevel.cfa(Demo.twolevel, cluster = "cluster", const = "shareconf",
#'                model = list(c("y1", "y2", "y3"), c("y4", "y5", "y6")))
#'
#' #——————————————————————————————————————
#' ## Residual Covariances at the Within Level
#'
#' # Example 8a: Residual covariance between 'y1' and 'y4' at the Within level
#' multilevel.cfa(Demo.twolevel, cluster = "cluster",
#'                model = list(c("y1", "y2", "y3"), c("y4", "y5", "y6")),
#'                rescov = c("y1", "y4"))
#'
#' # Example 8b: Fix all residual variances at 0
#' # i.e., strong factorial invariance across clusters
#' multilevel.cfa(Demo.twolevel, cluster = "cluster",
#'                model = list(c("y1", "y2", "y3"), c("y4", "y5", "y6")),
#'                fix.resid = "all")
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Model Specification Using 'model.w' and 'model.b' for One or Multiple_Factor Model
#' #
#' # Different factor structure at the Within and Between Level
#'
#' # Example 9a: Simultaneous and level-specific fit indices
#' multilevel.cfa(Demo.twolevel, cluster = "cluster",
#'                model.w = c("y1", "y2", "y3", "y4", "y5", "y6"),
#'                model.b = c("y1", "y2", "y3", "y4", "y5", "y6"), ls.fit = TRUE)
#'
#' # Example 9b: Two-factor model at the Within level and one-factor model at the Between level
#' multilevel.cfa(Demo.twolevel, cluster = "cluster",
#'                model.w = list(c("y1", "y2", "y3"), c("y4", "y5", "y6")),
#'                model.b = c("y1", "y2", "y3", "y4", "y5", "y6"))
#'
#' # Example 9c: Residual covariance between 'y1' and 'y4' at the Within level
#' # Residual covariance between 'y5' and 'y6' at the Between level
#' multilevel.cfa(Demo.twolevel, cluster = "cluster",
#'                model.w = list(c("y1", "y2", "y3"), c("y4", "y5", "y6")),
#'                model.b = c("y1", "y2", "y3", "y4", "y5", "y6"),
#'                rescov.w = c("y1", "y4"),
#'                rescov.b = c("y5", "y6"))
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # lavaan Model and Summary of the Estimated Model
#'
#' # Example 10
#' mod <- multilevel.cfa(Demo.twolevel, y1:y4, cluster = "cluster", output = FALSE)
#'
#' # lavaan model syntax
#' cat(mod$model)
#'
#' # Fitted lavaan object
#' lavaan::summary(mod$model.fit, standardized = TRUE, fit.measures = TRUE)
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Write Results
#'
#' # Example 11a: Write results into a text file
#' multilevel.cfa(Demo.twolevel, y1:y4, cluster = "cluster", print = "all",
#'                       write = "Multilevel_CFA.txt", output = FALSE)
#'
#' # Example 11b: Write results into an Excel file
#' multilevel.cfa(Demo.twolevel, y1:y4, cluster = "cluster", print = "all",
#'                write = "Multilevel_CFA.xlsx", output = FALSE)
#' }
multilevel.cfa <- function(data, ..., cluster, model = NULL, rescov = NULL,
                           model.w = NULL, model.b = NULL, rescov.w = NULL, rescov.b = NULL,
                           const = c("within", "shared", "config", "shareconf"), fix.resid = NULL,
                           ident = c("marker", "var", "effect"), ls.fit = FALSE, estimator = c("ML", "MLR"),
                           test = c("none", "standard", "yuan.bentler", "yuan.bentler.mplus"),
                           se = c("none", "standard", "robust.huber.white"),
                           optim.method = c("nlminb", "em"), missing = c("listwise", "fiml"),
                           print = c("all", "summary", "coverage", "descript", "fit", "est", "modind", "resid", "opdyke"),
                           mod.minval = 6.63, resid.minval = 0.1, opdyke.prec = 1, opdyke.minmax = c(0.40, 0.60),
                           color = "b.red", style = c("regular", "bold", "italic"),
                           digits = 3, p.digits = 3, as.na = NULL, write = NULL, append = TRUE,
                           check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'data' is missing or NULL
  if (isTRUE(missing(data) || is.null(data))) { stop("Please specify a data frame for the argument 'data'", call. = FALSE) }

  # Check input 'cluster'
  if (isTRUE(missing(cluster) ||is.null(cluster))) { stop("Please specify a variable name or vector representing the grouping structure for the argument 'cluster'.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Using the Argument '...' ####

  if (isTRUE(!missing(...))) {

    # Extract data
    x <- as.data.frame(data[, .var.names(data = data, ..., cluster = cluster), drop = FALSE])

    # Extract cluster variable and convert tibble into data frame or vector
    cluster <- data[, cluster] |> (\(p) if (isTRUE("tbl" %in% substr(class(p), 1L, 3L))) { unname(unlist(p)) } else { return(p) })()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Without using the Argument '...' ####

  } else {

    # Data frame
    x <- as.data.frame(data)

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

  .check.input(logical = c("ls.fit", "append", "output"),
               numeric = list(mod.minval = 1L, resid.minval = 1L),
               s.character = list(const = c("within", "shared", "config", "shareconf"), ident = c("marker", "var", "effect"),
                                  estimator = c("ML", "MLR"), test = c("none", "standard", "yuan.bentler", "yuan.bentler.mplus"),
                                  se = c("none", "standard", "robust.huber.white"), optim.method = c("nlminb", "em"), missing = c("listwise", "fiml"),
                                  style = c("regular", "bold", "italic")),
               m.character = list(print = c("all", "summary", "coverage", "descript", "fit", "est", "modind", "resid", "opdyke")),
               args = c("color", "digits", "p.digits", "write2"),
               package = "lavaan", envir = environment(), input.check = check)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Additional Checks

  if (isTRUE(check)) {

    #—————————————————————————————————————— #
    ### Check lavaan Version ####

    if (isTRUE(substr(packageDescription("lavaan")$Version, 3L, 3L) %in% seq_len(6L))) { stop("This function requires at least lavaan version 0.7-2 (published 2026-07-16), please update the package.", call. = FALSE) }

    #—————————————————————————————————————— #
    ### Check 'model', 'model.w', and 'model.b' ####

    # Check if input 'model' is a character vector or list of character vectors
    if (isTRUE(!is.null(model) && !all(sapply(model, is.character)))) { stop("Please specify a character vector or list of character vectors for the argument 'model'.", call. = FALSE) }

    # Check if input 'model.w' is a character vector or list of character vectors
    if (isTRUE(!is.null(model.w) && !all(sapply(model.w, is.character)))) { stop("Please specify a character vector or list of character vectors for the argument 'model.w'.", call. = FALSE) }

    # Check if input 'model.b' is a character vector or list of character vectors
    if (isTRUE(!is.null(model.b) && !all(sapply(model.w, is.character)))) { stop("Please specify a character vector or list of character vectors for the argument 'model.b'.", call. = FALSE) }

    # Check if 'model.w' and 'model.b' is NULL when model specified using 'model'
    if (isTRUE(!is.null(model) && (!is.null(model.w) || !is.null(model.b)))) { stop("Please specifiy the model either using the argument 'model' or the arguments 'model.w' and 'model.w'.", call. = FALSE) }

    # Model specification with 'model'
    if (isTRUE(!is.null(model))) { (!unique(unlist(model)) %in% colnames(x)) |> (\(p) if (isTRUE(any(p))) { stop(paste0("Variables specified in the argument 'model' were not found in 'data': ", paste(unique(unlist(model))[p], collapse = ", ")), call. = FALSE) })() }

    # Model specification with 'model.w'
    if (isTRUE(!is.null(model.w))) { (!unique(unlist(model.w)) %in% colnames(x)) |> (\(p) if (isTRUE(any(p))) { stop(paste0("Variables specified in the argument 'model.w' were not found in 'data': ", paste(unique(unlist(model))[model.w], collapse = ", ")), call. = FALSE) })() }

    # Model specification with 'model.b'
    if (isTRUE(!is.null(model.b))) { (!unique(unlist(model.b)) %in% colnames(x)) |> (\(p) if (isTRUE(any(p))) { stop(paste0("Variables specified in the argument 'model.b' were not found in 'data': ", paste(unique(unlist(model))[model.b], collapse = ", ")), call. = FALSE) })() }

    #—————————————————————————————————————— #
    ### Check Input 'rescov' ####

    if (isTRUE(!is.null(rescov))) {

      # More than one residual covariance specified as list
      if (isTRUE(is.list(rescov))) {

        if (isTRUE(any(sapply(rescov, length) != 2L))) { stop("Please specify a list of character vectors, each with two variable names, for the argument 'rescov'.", call. = FALSE) }

      # One residual covariance specified as vector
      } else {

        if (isTRUE(length(rescov) != 2L)) { stop("Please specify a character vector with two variable names, for the argument 'rescov'", call. = FALSE) }

      }

    }

    #—————————————————————————————————————— #
    ### Check Input 'rescov.w' ####

    if (isTRUE(!is.null(rescov.w))) {

      # More than one residual covariance specified as list
      if (isTRUE(is.list(rescov.w))) {

        if (isTRUE(any(sapply(rescov.w, length) != 2L))) { stop("Please specify a list of character vectors, each with two variable names, for the argument 'rescov.w'.", call. = FALSE) }

      # One residual covariance specified as vector
      } else {

        if (isTRUE(length(rescov.w) != 2L)) { stop("Please specify a character vector with two variable names, for the argument 'rescov.w'", call. = FALSE) }

      }

    }

    #—————————————————————————————————————— #
    ### Check Input 'rescov.b' ####

    if (isTRUE(!is.null(rescov.b))) {

      # More than one residual covariance specified as list
      if (isTRUE(is.list(rescov.b))) {

        if (isTRUE(any(sapply(rescov.b, length) != 2L))) { stop("Please specify a list of character vectors, each with two variable names, for the argument 'rescov.b'.", call. = FALSE) }

      # One residual covariance specified as vector
      } else {

        if (isTRUE(length(rescov.b) != 2L)) { stop("Please specify a character vector with two variable names, for the argument 'rescov.b'", call. = FALSE) }

      }

    }

    #—————————————————————————————————————— #
    ### Check Input 'fix.resid', 'mod.minval', and 'resid.minval' ####

    # Check input 'fix.resid'
    (!unique(fix.resid) %in% colnames(x)) |> (\(p) if (isTRUE(any(p) &&  all(fix.resid != "all"))) { stop(paste0("Variables specified in the argument 'fix.resid' were not found in 'data': ", paste(fix.resid[p], collapse = ", ")), call. = FALSE) })()

    # Check input 'mod.minval'
    if (isTRUE(mod.minval < 0L)) { stop("Please specify a value greater than or equal 0 for the argument 'mod.minval'.", call. = FALSE) }

    ## Check input 'resid.minval' ##
    if (isTRUE(resid.minval < 0L)) { stop("Please specify a value greater than or equal 0 for the argument 'resid.minval'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Model and Arguments --------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Manifest variables ####

  #—————————————————————————————————————— #
  ### Model Specification with 'data' ####

  if (isTRUE(is.null(model) && is.null(model.w) && is.null(model.b))) {

    var <- colnames(x)

  #—————————————————————————————————————— #
  ### Model Specification with 'model' ####

  } else if (isTRUE(!is.null(model) && is.null(model.w) && is.null(model.b))) {

    var <- unique(unlist(model))

  #—————————————————————————————————————— #
  ### Model Specification with 'model.w' and 'model.b' ####

  } else if (isTRUE(is.null(model) && (!is.null(model.w) || !is.null(model.b)))) {

    var <- unique(c(unlist(model.w), unlist(model.b)))

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data frame with Cluster Variable ####

  x <- data.frame(x[, var], .cluster = cluster)

  n.total <- nrow(x)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Missing Data on the Cluster Variable ####

  if (isTRUE(any(is.na(x$.cluster)))) {

    warning(paste0("Data contains missing values on the cluster variable, number of cases removed from the analysis: ", sum(is.na(x$.cluster))), call. = FALSE)

    x <- x[!is.na(x$.cluster), ]

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert User-Missing Values into NA ####

  if (isTRUE(!is.null(as.na))) { x[, var] <- .as.na(x[, var], na = as.na) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Factor Labels ####

  #—————————————————————————————————————— #
  ### Model Specification with 'model ####

  if (isTRUE(!is.null(model) && is.null(model.w) && is.null(model.b))) {

    # 'model' is a list
    if (is.list(model)) {

      # List elements not all named
      if (isTRUE(is.null(names(model)) || any(names(model) == ""))) { names(model) <- paste0("f", seq_along(model)) }

    # 'model' is not a list
    } else {

      model <- list(f = model)

    }

  #—————————————————————————————————————— #
  ### Model Specification with 'model.w' and 'model.b' ####

  } else if (isTRUE(!is.null(model.w) || !is.null(model.w))) {

    # 'model.w' is a list
    if (is.list(model.w)) {

      # List elements not all named
      if (isTRUE(is.null(names(model.w)) || any(names(model.w) == ""))) { names(model.w) <- paste0("f", seq_along(model.w)) }

    # 'model.w' is not a list
    } else {

      model.w <- list(f = model.w)

    }

    # 'model.b' is a list
    if (is.list(model.b)) {

      # List elements not all named
      if (isTRUE(is.null(names(model.b)) || any(names(model.b) == ""))) { names(model.b) <- paste0("f", seq_along(model.b)) }

    # 'model.b' is not a list
    } else {

      model.b <- list(f = model.b)

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'rescov' Argument ####

  if (isTRUE(!is.null(rescov) && !is.list(rescov))) {

    rescov <- list(rescov)

  }

  if (isTRUE(!is.null(rescov.w) && !is.list(rescov.w))) {

    rescov.w <- list(rescov.w)

  }

  if (isTRUE(!is.null(rescov.b) && !is.list(rescov.b))) {

    rescov.b <- list(rescov.b)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'const' Argument ####

  if (isTRUE(all(c("within", "shared", "config", "shareconf") %in% const))) { const <- "config" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'fix.resid' Argument ####

  if (isTRUE(fix.resid == "all")) { fix.resid <- var }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'ident' Argument ####

  if (isTRUE(all(c("marker", "var", "effect") %in% ident))) { ident <- "var" }

  switch(ident,
         marker = { std.lv <- FALSE; effect.coding <- FALSE },
         # Fixed factor method needs to be specified manually
         var = { if (isTRUE(is.null(model.w) && is.null(model.b))) { std.lv <- FALSE; effect.coding <- FALSE } else { std.lv <- TRUE; effect.coding <- FALSE } },
         effect = {std.lv <- FALSE; effect.coding <- TRUE })

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'ls.fit' Argument ####

  if (isTRUE(ls.fit)) {

    if (isTRUE(is.null(model.w) && is.null(model.b))) {

      warning("Level-specific fit indices are computed only when specifying a model using the arguments 'model.w' and 'model.b'.", call. = FALSE)

      ls.fit <- FALSE

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'estimator' Argument ####

  if (isTRUE(all(c("ML", "MLR") %in% estimator))) { estimator <- "MLR" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'test' Argument ####

  #—————————————————————————————————————— #
  ## Default Setting ####

  if (isTRUE(all(c("none", "standard", "yuan.bentler", "yuan.bentler.mplus") %in% test))) {

    test <- "default"

  #—————————————————————————————————————— #
  ### Setting "none" ####

  } else if (isTRUE(test == "none")) {

    estimator <- "ML"

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'se' Argument ####

  #—————————————————————————————————————— #
  ### Default Setting ####

  if (isTRUE(all(c("none", "standard", "robust.huber.white") %in% se) && test != "none")) {

    se <- "default"

  #—————————————————————————————————————— #
  ### Default Setting when Setting test = "none" ####

  } else if (isTRUE(all(c("none", "standard", "robust.huber.white") %in% se) && test == "none")) {

    se <- "robust.huber.white"

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'optim.method' Argument ####

  if (isTRUE(all(c("nlminb", "em") %in% optim.method))) { optim.method <- "nlminb" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'missing' Argument ####

  # Complete data
  if (isTRUE(all(!is.na(x[, var])))) {

    missing <- "listwise"

  # Data with missing values
  } else {

    if (isTRUE(all(c("listwise", "fiml") %in% missing))) {

      missing <- "fiml"

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Missing Data on All Variable ####

  (misty::na.prop(x[, var], append = FALSE) == 1L) |> (\(p) if (isTRUE(any(p) && missing == "fiml")) { warning(paste0("Data contains cases with missing values on all variables, number of cases removed from the analysis: ", sum(p)), call. = FALSE) })()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'print' Argument ####

  if (isTRUE(all(c("all", "summary", "coverage", "descript", "fit", "est", "modind", "resid", "opdyke") %in% print))) {

    print  <- c("summary", "descript", "fit", "est")

  } else if (isTRUE(all(print == "all"))) {

    print <- c("summary", "coverage", "descript", "fit", "est", "modind", "resid", "opdyke")

  }

  # Remove 'fit' when test == "none"
  if (isTRUE(test == "none")) { print <- setdiff(print, "fit") }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'style' Argument ####

  if (isTRUE(all(c("regular", "bold", "italic") %in% style))) { style <- "regular" }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Covariance Coverage ####

  coverage <- NULL
  if (isTRUE("coverage" %in% print)) {

    coverage <- misty::na.coverage(x[, var], check = FALSE, output = FALSE)$result

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Sample Statistics ####

  descript.var <- NULL
  if (isTRUE("descript" %in% print)) {

    # Descriptive statistics and Intraclass Correlation Coefficient, ICC(1)
    descript.var <- data.frame(misty::descript(x[, var], check = FALSE, output = FALSE)$result[, c("variable", "n", "nNA", "pNA", "m", "sd", "min", "max", "skew", "kurt")],
                               ICC = misty::multilevel.icc(x[, var], cluster = x$.cluster))

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model Specification ####

  #—————————————————————————————————————— #
  ### Model Specification with 'data' ####

  if (isTRUE(is.null(model) && is.null(model.w) && is.null(model.b))) {

    switch(const,
    #···················
    #### Within-Cluster Constructs ####
    within = {

      # Fixed factor method
      if (isTRUE(ident == "var")) {

        # Model specification
        mod.l12 <- paste( # Within level
          c("level: 1", "\n",
            # Within-cluster construct
            paste0("  wf =~", " NA*", var[1L], " + ",  paste(paste0("L", 1L:length(var), "*", var), collapse = " + ")), "\n",
            # Fix variance of latent variables at 1
            "  wf ~~ 1*wf", "\n",
            # Residual covariance
            if (isTRUE(!is.null(rescov))) { paste0("\n ", vapply(lapply(rescov, function(y) paste("", y)), function(z) paste(z, collapse = " ~~"), FUN.VALUE = character(1L)), collapse = "") }, "\n",
            # Between level
            "level: 2", "\n  ",
            # Covariances among all indicators
            paste(apply(combn(var, m = 2L), 2L, paste, collapse = " ~~ "), collapse = " \n  ")), collapse = "")

      } else {

        # Model specification
        mod.l12 <- paste( # Within level
                         c("level: 1", "\n",
                          # Within-cluster construct
                          paste0("  wf =~ ", paste(var, collapse = " + ")),
                          # Residual covariance
                          if (isTRUE(!is.null(rescov))) { paste0("\n ", vapply(lapply(rescov, function(y) paste("", y)), function(z) paste(z, collapse = " ~~"), FUN.VALUE = character(1L)), collapse = "") }, "\n",
                          # Between level
                          "level: 2", "\n  ",
                          # Covariances among all indicators
                          paste(apply(combn(var, m = 2L), 2L, paste, collapse = " ~~ "), collapse = " \n  ")), collapse = "")

      }

    },
    #···················
    #### Shared Cluster Constructs ####
    shared = {

      # Fixed factor method
      if (isTRUE(ident == "var")) {

        # Model specification
        mod.l12 <- paste(  # Within level
                         c("level: 1", "\n  ",
                           # Covariances among all indicators
                           paste(apply(combn(var, m = 2L), 2L, paste, collapse = " ~~ "), collapse = " \n  "), "\n",
                           # Between level
                           "level: 2", "\n  ",
                           # Shared cluster-level construct
                           paste0("  bf =~", " NA*", var[1L], " + ", paste(paste0("L", 1L:length(var), "*", var), collapse = " + ")), "\n",
                           # Estimate variance of latent variables
                           "  bf ~~ 1*bf",
                           # Residual variances fixed at 0
                           if (isTRUE(!is.null(fix.resid))) { paste0("\n", sapply(fix.resid, function(y) paste0("  ", y, " ~~ 0*", y)), collapse = "") }), collapse = "")

      } else {

        # Model specification
        mod.l12 <- paste(# Within level
                         c("level: 1", "\n  ",
                           # Covariances among all indicators
                           paste(apply(combn(var, m = 2L), 2L, paste, collapse = " ~~ "), collapse = " \n  "), "\n",
                           # Between level
                           "level: 2", "\n  ",
                           # Shared cluster-level construct
                           paste("bf =~", paste(var, collapse = " + ")),
                           # Residual variances fixed at 0
                           if (isTRUE(!is.null(fix.resid))) { paste0("\n", sapply(fix.resid, function(y) paste0("  ", y, " ~~ 0*", y)), collapse = "") }), collapse = "")

      }

    },
    #···················
    #### Configural Cluster Constructs ####
    config =  {

      # Fixed factor method
      if (isTRUE(ident == "var")) {

        # Model specification
        mod.l12 <- paste(  # Within level
                         c("level: 1", "\n",
                           # Within-cluster construct
                           paste0("  wf =~", " NA*", var[1L], " + ",  paste(paste0("L", 1L:length(var), "*", var), collapse = " + ")), "\n",
                           # Fix variance of latent variables at 1
                           "  wf ~~ 1*wf",
                           # Residual covariance
                           if (isTRUE(!is.null(rescov))) { paste0("\n ", vapply(lapply(rescov, function(y) paste("", y)), function(z) paste(z, collapse = " ~~"), FUN.VALUE = character(1L)), collapse = "") }, "\n",
                           # Between level
                           "level: 2", "\n",
                           # Configural cluster construct
                           paste0("  bf =~", " NA*", var[1L], " + ", paste(paste0("L", 1L:length(var), "*", var), collapse = " + ")), "\n",
                           # Estimate variance of latent variables
                           "  bf ~~ bf",
                           # Residual variances fixed at 0
                           if (isTRUE(!is.null(fix.resid))) { paste0("\n", sapply(fix.resid, function(y) paste0("  ", y, " ~~ 0*", y)), collapse = "") }), collapse = "")

      } else {

        # Model specification
        mod.l12 <- paste(  # Within level
                         c("level: 1 ", "\n",
                           # Within-cluster construct
                           paste("  wf =~", paste(paste0("L", 1L:length(var), "*", var), collapse = " + ")),
                           # Residual covariance
                           if (isTRUE(!is.null(rescov))) { paste0("\n ", vapply(lapply(rescov, function(y) paste("", y)), function(z) paste(z, collapse = " ~~"), FUN.VALUE = character(1L)), collapse = "") }, "\n",
                           # Between level
                           "level: 2 ", "\n",
                           # Configural cluster construct
                           paste("  bf =~", paste(paste0("L", 1L:length(var), "*", var), collapse = " + ")),
                           # Residual variances fixed at 0
                           if (isTRUE(!is.null(fix.resid))) { paste0("\n", sapply(fix.resid, function(y) paste0("  ", y, " ~~ 0*", y)), collapse = "") }), collapse = "")

      }

    },
    #···················
    #### Simultaneous Shared and Configural Cluster Constructs ####
    shareconf = {

      # Fixed factor method
      if (isTRUE(ident == "var")) {

        # Model specification
        mod.l12 <- paste(  # Within level
                         c("level: 1", "\n",
                           # Within-cluster constructs
                           paste0("  wf =~", " NA*", var[1L], " + ",  paste(paste0("L", 1L:length(var), "*", var), collapse = " + ")), "\n",
                           # Fix variance of latent variable at 1
                           "  wf ~~ 1*wf",
                           # Residual covariance
                           if (isTRUE(!is.null(rescov))) { paste0("\n ", vapply(lapply(rescov, function(y) paste("", y)), function(z) paste(z, collapse = " ~~"), FUN.VALUE = character(1L)), collapse = "") }, "\n",
                           # Between level
                           "level: 2", "\n",
                           # Configural cluster construct
                           paste0("  bf =~", " NA*", var[1L], " + ", paste(paste0("L", 1L:length(var), "*", var), collapse = " + ")), "\n",
                           # Shared cluster-level construct
                           paste0("  bs =~",  " NA*", var[1L], " + ", paste(var, collapse = " + ")), "\n",
                           # Estimate variance of latent variable
                           "  bf ~~ bf", "\n",
                           # Fix variance latent variable at 1
                           "  bs ~~ 1*bs", "\n",
                           # Fix covariance at 0
                           "  bf ~~ 0*bs",
                           # Residual variances fixed at 0
                           if (isTRUE(!is.null(fix.resid))) { paste0("\n", sapply(fix.resid, function(y) paste0("  ", y, " ~~ 0*", y)), collapse = "") }), collapse = "")

      } else if (isTRUE(ident == "marker")) {

        # Model specification
        mod.l12 <- paste(  # Within level
                         c("level: 1 ", "\n",
                           # Within-cluster constructs
                           paste("  wf =~", paste(paste0("L", 1L:length(var), "*", var), collapse = " + ")),
                           # Residual covariance
                           if (isTRUE(!is.null(rescov))) { paste0("\n ", vapply(lapply(rescov, function(y) paste("", y)), function(z) paste(z, collapse = " ~~"), FUN.VALUE = character(1L)), collapse = "") }, "\n",
                           # Between level
                           "level: 2 ", "\n",
                           # Configural cluster construct
                           paste("  bf =~", paste(paste0("L", 1L:length(var), "*", var), collapse = " + ")), "\n",
                           # Shared cluster-level construct
                           paste("  bs =~", paste(var, collapse = " + ")), "\n",
                           # Fix covariance at 0
                           "  bf ~~ 0*bs",
                           # Residual variances fixed at 0
                           if (isTRUE(!is.null(fix.resid))) { paste0("\n", sapply(fix.resid, function(y) paste0("  ", y, " ~~ 0*", y)), collapse = "") }), collapse = "")

      } else if (isTRUE(ident == "effect")) {

        # Model specification
        mod.l12 <- paste(  # Within level
                         c("level: 1 ", "\n",
                           # Within-cluster constructs
                           paste("  wf =~", paste(paste0("L", 1L:length(var), "*", var), collapse = " + ")),
                           # Residual covariance
                           if (isTRUE(!is.null(rescov))) { paste0("\n ", vapply(lapply(rescov, function(y) paste("", y)), function(z) paste(z, collapse = " ~~"), FUN.VALUE = character(1L)), collapse = "") }, "\n",
                           # Between level
                           "level: 2 ", "\n",
                           # Configural cluster construct
                           paste("  bf =~", paste(paste0("L", 1L:length(var), "*", var), collapse = " + ")), "\n",
                           # Shared cluster-level construct
                           paste("  bs =~", paste(var, collapse = " + ")), "\n",
                           # Fix covariance at 0
                           "  bf ~~ 0*bs", "\n",
                           # Fix variance latent variable at 1
                           "  bs ~~ 1*bs",
                           # Residual variances fixed at 0
                           if (isTRUE(!is.null(fix.resid))) { paste0("\n", sapply(fix.resid, function(y) paste0("  ", y, " ~~ 0*", y)), collapse = "") }), collapse = "")

      }

    })

  #—————————————————————————————————————— #
  ### Model Specification with 'model' ####

  } else if (isTRUE(!is.null(model) && is.null(model.w) && is.null(model.b))) {

    switch(const,
           #···················
           #### Within-Cluster Constructs ####
           within = {

             # Fixed factor method
             if (isTRUE(ident == "var")) {

               # Labels for parameter constraints
               model.label <- paste0("L", 1L:length(unlist(model)), "*", unlist(model))
               attr(model.label, "skeleton") <- attr(unlist(as.relistable(model)), "skeleton")

               # Model specification
               mod.l12 <- paste(  # Within level
                                c("level: 1", "\n ",
                                  # Within-cluster constructs
                                  paste0(sapply(names(model), function(y) paste0(" w", y, " =~", " NA*", model[[y]][1L], " + ",  paste(relist(model.label)[[y]], collapse = " + "))), collapse = "\n "), "\n",
                                  # Fix variance of latent variables at 1
                                  paste0(sapply(names(model), function(y) paste0("  w", y, " ~~ ", "1*w", y)), collapse = "\n "),
                                  # Residual covariance
                                  if (isTRUE(!is.null(rescov))) { paste0("\n ", vapply(lapply(rescov, function(y) paste("", y)), function(z) paste(z, collapse = " ~~"), FUN.VALUE = character(1L)), collapse = "") }, "\n",
                                  # Between level
                                  "level: 2", "\n  ",
                                  # Covariances among all indicators
                                  paste(apply(combn(var, m = 2L), 2L, paste, collapse = " ~~ "), collapse = " \n  ")), collapse = "")

             } else {

               # Model specification
               mod.l12 <- paste(  # Within level
                                c("level: 1", "\n ",
                                  # Within-cluster constructs
                                  paste0(sapply(names(model), function(y) paste0("w", y, " =~ ", paste(model[[y]], collapse = " + "))), collapse = "\n "),
                                  # Residual covariance
                                  if (isTRUE(!is.null(rescov))) { paste0("\n ", vapply(lapply(rescov, function(y) paste("", y)), function(z) paste(z, collapse = " ~~"), FUN.VALUE = character(1L)), collapse = "") }, "\n",
                                  # Between level
                                  "level: 2", "\n  ",
                                  # Covariances among all indicators
                                  paste0(apply(combn(var, m = 2L), 2L, paste0, collapse = " ~~ "), collapse = " \n  ")), collapse = "")

             }

           },
           #···················
           #### Shared Cluster Constructs ####
           shared = {

             # Fixed factor method
             if (isTRUE(ident == "var")) {

               # Labels for parameter constraints
               model.label <- paste0("L", 1L:length(unlist(model)), "*", unlist(model))
               attr(model.label, "skeleton") <- attr(unlist(as.relistable(model)), "skeleton")

               # Model specification
               mod.l12 <- paste(  # Within level
                                c("level: 1", "\n  ",
                                  # # Covariances among all indicators
                                  paste(apply(combn(var, m = 2L), 2L, paste, collapse = " ~~ "), collapse = " \n  "), "\n",
                                  # Between level
                                  "level: 2", "\n ",
                                  # Shared cluster-level constructs
                                  paste0(sapply(names(model), function(y) paste0(" b", y, " =~", " NA*", model[[y]][1L], " + ",  paste(relist(model.label)[[y]], collapse = " + "))), collapse = "\n "), "\n ",
                                  # Fix variance of latent variables at 1
                                  paste0(sapply(names(model), function(y) paste0(" b", y, " ~~ ", "1*b", y)), collapse = "\n "),
                                  # Residual variances fixed at 0
                                  if (isTRUE(!is.null(fix.resid))) { paste0("\n", sapply(fix.resid, function(y) paste0("  ", y, " ~~ 0*", y)), collapse = "") }), collapse = "")

             } else {

               # Model specification
               mod.l12 <- paste(  # Within level
                                c("level: 1", "\n  ",
                                  # Covariances among all indicators
                                  paste(apply(combn(var, m = 2L), 2L, paste, collapse = " ~~ "), collapse = " \n  "), "\n",
                                  # Between level
                                  "level: 2", "\n ",
                                  # Shared cluster-level constructs
                                  paste0(sapply(names(model), function(y) paste0(" b", y, " =~ ", paste(model[[y]], collapse = " + "))), collapse = "\n "),
                                  # Residual variances fixed at 0
                                  if (isTRUE(!is.null(fix.resid))) { paste0("\n", sapply(fix.resid, function(y) paste0("  ", y, " ~~ 0*", y)), collapse = "") }), collapse = "")

             }

           },
           #···················
           #### Configural Cluster Constructs ####
           config =  {

             # Labels for parameter constraints
             model.label <- paste0("L", 1L:length(unlist(model)), "*", unlist(model))
             attr(model.label, "skeleton") <- attr(unlist(as.relistable(model)), "skeleton")

             # Fixed factor method
             if (isTRUE(ident == "var")) {

               # Model specification
               mod.l12 <- paste(  # Within level
                                c("level: 1", "\n ",
                                  # Within-cluster constructs
                                  paste0(sapply(names(model), function(y) paste0(" w", y, " =~", " NA*", model[[y]][1L], " + ",  paste(relist(model.label)[[y]], collapse = " + "))), collapse = "\n "), "\n ",
                                  # Fix variance of latent variables at 1
                                  paste0(sapply(names(model), function(y) paste0(" w", y, " ~~ ", "1*w", y)), collapse = "\n "),
                                  # Residual covariance
                                  if (isTRUE(!is.null(rescov))) { paste0("\n ", vapply(lapply(rescov, function(y) paste("", y)), function(z) paste(z, collapse = " ~~"), FUN.VALUE = character(1L)), collapse = "") }, "\n",
                                  # Between level
                                  "level: 2", "\n ",
                                  # Configural cluster constructs
                                  paste0(sapply(names(model), function(y) paste0(" b", y, " =~", " NA*", model[[y]][1L], " + ",  paste(relist(model.label)[[y]], collapse = " + "))), collapse = "\n "), "\n ",
                                  # Estimate variance of latent variables
                                  paste0(sapply(names(model), function(y) paste0(" b", y, " ~~ b", y)), collapse = "\n "),
                                  # Residual variances fixed at 0
                                  if (isTRUE(!is.null(fix.resid))) { paste0("\n", sapply(fix.resid, function(y) paste0("  ", y, " ~~ 0*", y)), collapse = "") }), collapse = "")

             } else {

               # Model specification
               mod.l12 <- paste(  # Within level
                                c("level: 1", "\n ",
                                  # Within-cluster constructs
                                  paste0(sapply(names(model), function(y) paste0(" w", y, " =~ ", paste(relist(model.label)[[y]], collapse = " + "))), collapse = "\n "),
                                  # Residual covariance
                                  if (isTRUE(!is.null(rescov))) { paste0("\n ", vapply(lapply(rescov, function(y) paste("", y)), function(z) paste(z, collapse = " ~~"), FUN.VALUE = character(1L)), collapse = "") }, "\n",
                                  # Between level
                                  "level: 2", "\n ",
                                  # Configural cluster constructs
                                  paste0(sapply(names(model), function(y) paste0(" w", y, " =~ ", paste(relist(model.label)[[y]], collapse = " + "))), collapse = "\n "),
                                  # Residual variances fixed at 0
                                  if (isTRUE(!is.null(fix.resid))) { paste0("\n", sapply(fix.resid, function(y) paste0("  ", y, " ~~ 0*", y)), collapse = "") }), collapse = "")

             }

           },
           #···················
           #### Simultaneous Shared and Configural Cluster Construct ####
           shareconf = {

             # Labels for parameter constraints
             model.label <- paste0("L", seq_len(length(unlist(model))), "*", unlist(model))
             attr(model.label, "skeleton") <- attr(unlist(as.relistable(model)), "skeleton")

             # Fixed factor method
             if (isTRUE(ident == "var")) {

               # Model specification
               mod.l12 <- paste(  # Within level
                                c("level: 1", "\n ",
                                  # Within-cluster constructs
                                  paste0(sapply(names(model), function(y) paste0(" w", y, " =~", " NA*", model[[y]][1L], " + ", paste(relist(model.label)[[y]], collapse = " + "))), sep = "\n "),
                                  # Fix variance of latent variables at 1
                                  paste0(sapply(names(model), function(y) paste0(" w", y, " ~~ ", "1*w", y)), collapse = "\n "),
                                  # Residual covariance
                                  if (isTRUE(!is.null(rescov))) { paste0("\n ", vapply(lapply(rescov, function(y) paste("", y)), function(z) paste(z, collapse = " ~~"), FUN.VALUE = character(1L)), collapse = "") }, "\n",
                                  # Between level
                                  "level: 2", "\n ",
                                  # Configural cluster constructs
                                  paste0(sapply(names(model), function(y) paste0(" b", y, " =~", " NA*", model[[y]][1L], " + ",  paste(relist(model.label)[[y]], collapse = " + "))), collapse = "\n "), "\n ",
                                  # Shared cluster-level constructs
                                  paste0(sapply(names(model), function(y) paste0(" bs", if (isTRUE(length(model) > 1L)) { y } else { }, " =~", " NA*", model[[y]][1L], " + ",  paste(model[[y]], collapse = " + "))), collapse = "\n "), "\n ",
                                  # Estimate variance of latent variables
                                  paste0(sapply(names(model), function(y) paste0(" b", y, " ~~ b", y)), collapse = "\n "), "\n ",
                                  # Fix variance of shared latent variables at 1
                                  paste0(sapply(names(model), function(y) paste0(" bs", if (isTRUE(length(model) > 1L)) { y } else { }, " ~~ 1*bs", if (isTRUE(length(model) > 1L)) { y } else { } )), collapse = "\n "), "\n ",
                                  # Fix covariances among shared and configural cluster constructs at 0
                                  paste0(sapply(names(model), function(y) paste0(" bs", if (isTRUE(length(model) > 1L)) { y } else { }, " ~~ ", paste0("0*b", names(model), collapse = " + "))), collapse = "\n "), "\n  ",
                                  # Fix covariances among shared cluster constructs at 0
                                  if (isTRUE(length(model) > 1L)) { paste(apply(combn(paste0("bs", names(model)), m = 2L), 2L, paste, collapse = " ~~ 0*"), collapse = " \n  ") },
                                  # Residual variances fixed at 0
                                  if (isTRUE(!is.null(fix.resid))) { paste0("\n", sapply(fix.resid, function(y) paste0("  ", y, " ~~ 0*", y)), collapse = "") }), collapse = "")

             } else {

               # Model specification
               mod.l12 <- paste(  # Within level
                                c("level: 1", "\n ",
                                  # Within-cluster constructs
                                  paste0(sapply(names(model), function(y) paste0(" w", y, "  =~ ", paste(relist(model.label)[[y]], collapse = " + "))), sep = "\n "),
                                  # Residual covariance
                                  if (isTRUE(!is.null(rescov))) { paste0("\n ", vapply(lapply(rescov, function(y) paste("", y)), function(z) paste(z, collapse = " ~~"), FUN.VALUE = character(1L)), collapse = "") },
                                  # Between level
                                  "level: 2", "\n ",
                                  # Configural cluster constructs
                                  paste0(sapply(names(model), function(y) paste0(" b", y, " =~ ", paste(relist(model.label)[[y]], collapse = " + "))), collapse = "\n "), "\n ",
                                  # Shared cluster-level constructs
                                  paste0(sapply(names(model), function(y) paste0(" bs", if (isTRUE(length(model) > 1L)) { y } else { }, " =~", " NA*", model[[y]][1L], " + ",  paste(model[[y]], collapse = " + "))), collapse = "\n "), "\n ",
                                  # Fix covariances among shared and configural cluster constructs at 0
                                  paste0(sapply(names(model), function(y) paste0(" bs", if (isTRUE(length(model) > 1L)) { y } else { }, " ~~ ", paste0("0*b", names(model), collapse = " + "))), collapse = "\n "), "\n  ",
                                  # Fix covariances among shared cluster constructs at 0
                                  if (isTRUE(length(model) > 1L)) { paste(apply(combn(paste0("bs", names(model)), m = 2L), 2L, paste, collapse = " ~~ 0*"), collapse = " \n  ") },
                                  # Residual variances fixed at 0
                                  if (isTRUE(!is.null(fix.resid))) { paste0("\n", sapply(fix.resid, function(y) paste0("  ", y, " ~~ 0*", y)), collapse = "") }), collapse = "")

             }

           })

  #—————————————————————————————————————— #
  ### Model Specification with 'model.w' and 'model.b' ####

  } else if (isTRUE(is.null(model) && (!is.null(model.w) || !is.null(model.b)))) {

    mod.l12 <- paste(  # Within level
                     c("level: 1 ", "\n ",
                       # Within constructs
                       paste0(sapply(names(model.w), function(y) paste("", y, "=~", paste(model.w[[y]], collapse = " + "))), collapse = "\n "),
                       # Residual covariance
                       if (isTRUE(!is.null(rescov.w))) { paste0("\n ", vapply(lapply(rescov.w, function(y) paste("", y)), function(z) paste(z, collapse = " ~~"), FUN.VALUE = character(1L)), collapse = "") }, "\n",
                       # Between level
                       "level: 2 ", "\n ",
                       # Between constructs
                       paste(sapply(names(model.b), function(y) paste("", y, "=~", paste(model.b[[y]], collapse = " + "))), collapse = "\n "),
                       # Residual covariance
                       if (isTRUE(!is.null(rescov.b))) { paste0("\n ", vapply(lapply(rescov.b, function(y) paste("", y)), function(z) paste(z, collapse = " ~~"), FUN.VALUE = character(1L)), collapse = "") },
                       # Residual variances fixed at 0
                       if (isTRUE(!is.null(fix.resid))) { paste0("\n", sapply(fix.resid, function(y) paste0("  ", y, " ~~ 0*", y)), collapse = "") }), collapse = "")

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model Estimation ####

  model.fit <- tryCatch(suppressWarnings(lavaan::cfa(mod.l12, data = x, cluster = ".cluster",
                                                     estimator = estimator, optim.method = optim.method, missing = missing,
                                                     std.lv = std.lv, effect.coding = effect.coding, test = test, se = se,
                                                     fit.by.level = ifelse(ls.fit, TRUE, FALSE),
                                                     check.gradient = FALSE, check.post = FALSE, check.vcov = FALSE)),
                        error = function(y) {

                          if (isTRUE(missing == "fiml")) {

                            stop("Estimation problem in lavaan, switching to missing = \"listwise\" might solve the problem.", call. = FALSE)

                          } else if (isTRUE(estimator == "MLR")) {

                            stop("Estimation problem in lavaan, switching to estimator = \"ML\" might solve the problem.", call. = FALSE)

                          } else {

                            stop("Estimation problem in lavaan, measurement model could not be estimated.", call. = FALSE)

                          }})

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convergence and Model Identification Checks ####

  if (isTRUE(check)) {

    check.vcov <- check.theta.w <- check.theta.b <- check.cov.lv.w <- check.cov.lv.b <- TRUE

    #—————————————————————————————————————— #
    ### Model Convergence ####

    if (isTRUE(!lavaan::lavInspect(model.fit, what = "converged"))) { stop("CFA model did not converge.", call. = FALSE) }

    #—————————————————————————————————————— #
    ### Degrees of Freedom ####

    if (isTRUE(test != "none")) { if (isTRUE(suppressWarnings(lavaan::lavInspect(model.fit, what = "fit")["df"] < 0L))) { stop("CFA model has negative degrees of freedom, model is not identified.", call. = FALSE) } }

    #—————————————————————————————————————— #
    ### Standard Error ####

    if (isTRUE(se != "none")) { if (isTRUE(any(is.na(unlist(lavaan::lavInspect(model.fit, what = "se")))))) { stop("Standard errors could not be computed.", call. = FALSE) } }

    #—————————————————————————————————————— #
    ### Variance-Covariance Matrix of the Estimated Parameters ####

    if (isTRUE(se != "none")) {

      eigvals <- eigen(lavaan::lavInspect(model.fit, what = "vcov"), symmetric = TRUE, only.values = TRUE)$values

      # Correct for equality constraints
      if (isTRUE(any(lavaan::parTable(model.fit)$op == "=="))) { eigvals <- rev(eigvals)[-seq_len(sum(lavaan::parTable(model.fit)$op == "=="))] }

      if (isTRUE(min(eigvals) < .Machine$double.eps^(3L/4L))) {

        warning("The variance-covariance matrix of the estimated parameters is not positive definite. This may be a symptom that the model is not identified.", call. = FALSE)

        check.vcov <- FALSE

      }

    }

    #—————————————————————————————————————— #
    ### Negative Variance of Observed Variables ####

    #···················
    #### Within Level ####

    if (isTRUE(any(diag(lavaan::lavInspect(model.fit, what = "theta")$within) < 0L))) {

      warning("Some estimated variances of the observed variables at the Within level are negative.", call. = FALSE)

      check.theta.w <- FALSE

    } else if (isTRUE(any(eigen(lavaan::lavTech(model.fit, what = "theta")$within, symmetric = TRUE, only.values = TRUE)$values < (-1L * .Machine$double.eps^(3/4))))) {

      warning("The model-implied variance-covariance matrix of the residuals of the observed variables is not positive definite.", call. = FALSE)

      check.theta.w <- FALSE

    }

    #···················
    #### Between Level ####

    if (isTRUE(any(diag(lavaan::lavInspect(model.fit, what = "theta")$.cluster) < 0L))) {

      warning("Some estimated variances of the observed variables at the Between level are negative.", call. = FALSE)

      check.theta.b <- FALSE

    } else if (isTRUE(any(eigen(lavaan::lavTech(model.fit, what = "theta")$.cluster, symmetric = TRUE, only.values = TRUE)$values < (-1L * .Machine$double.eps^(3/4))))) {

      warning("The model-implied variance-covariance matrix of the residuals of the observed variables at the Between level is not positive definite.", call. = FALSE)

      check.theta.b <- FALSE

    }

    #—————————————————————————————————————— #
    ### Negative Variance of Latent Variables ####

    #···················
    #### Within Level ####

    if (isTRUE(!is.null(lavaan::lavTech(model.fit, what = "cov.lv")$within))) {

        if (isTRUE(any(diag(lavaan::lavTech(model.fit, what = "cov.lv")$within) < 0L))) {

        warning("Some estimated variances of the latent variables at the Within level are negative.", call. = FALSE)

      check.cov.lv.w <- FALSE

      }

    # Model-implied variance-covariance matrix of the latent variables
    } else if (any(dim(lavaan::lavTech(model.fit, what = "cov.lv")$within) != 0L)) {

      if (isTRUE(any(eigen(lavaan::lavTech(model.fit, what = "cov.lv")$within, symmetric = TRUE, only.values = TRUE)$values < (-1L * .Machine$double.eps^(3/4))))) {

        warning("The model-implied variance-covariance matrix of the latent variables at the Within level is not positive definite.", call. = FALSE)

        check.cov.lv.w <- FALSE

      }

    }

    #···················
    #### Between Level ####

    if (isTRUE(!is.null(lavaan::lavTech(model.fit, what = "cov.lv")$cluster))) {

      if (isTRUE(any(diag(lavaan::lavTech(model.fit, what = "cov.lv")$.cluster) < 0L))) {

      warning("Some estimated variances of the latent variables at the Between level are negative.", call. = FALSE)

      check.cov.lv.b <- FALSE

      }

    # Model-implied variance-covariance matrix of the latent variables
    } else if (any(dim(lavaan::lavTech(model.fit, what = "cov.lv")$.cluster) != 0L)) {

      if (isTRUE(any(eigen(lavaan::lavTech(model.fit, what = "cov.lv")$.cluster, symmetric = TRUE, only.values = TRUE)$values < (-1L * .Machine$double.eps^(3/4))))) {

      warning("The model-implied variance-covariance matrix of the latent variables at the Between level is not positive definite.", call. = FALSE)

      check.cov.lv.b <- FALSE

      }

    }

  } else {

    check.vcov <- check.theta.w <- check.theta.b <- check.cov.lv.w <- check.cov.lv.b <- NULL

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model Fit ####

  # Parameter table
  mod.par <- lavaan::parTable(model.fit)

  model.fit.measures <- NULL
  if (isTRUE("fit" %in% print)) {

    #—————————————————————————————————————— #
    ### Model Specified with 'data' or 'model' ####

    if (isTRUE(is.null(model.w) && is.null(model.b))) {

      model.fit.measures <- suppressWarnings(lavaan::lavInspect(model.fit, what = "fit"))

    #—————————————————————————————————————— #
    ### Model Specified with 'model.w' and 'model.b' and No Cross-Level Constraints ####

    } else {

      #···················
      #### Level-Specific Fit Indices ####

      if (isTRUE(ls.fit)) {

        model.fit.measures <- list(simul = lavaan::fitmeasures(model.fit), fit.l1 = lavaan::fitMeasures(model.fit, level = 1L), fit.l2 = lavaan::fitMeasures(model.fit, level = 2L))

      #···················
      #### No Level-Specific Fit Indices ####

      } else {

        model.fit.measures <- lavaan::lavInspect(model.fit, what = "fit")

      }

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Parameter Estimates ####

  model.param <- NULL
  if (isTRUE("est" %in% print)) {

    model.param <- data.frame(lavaan::parameterEstimates(model.fit), stdyx = lavaan::standardizedsolution(model.fit)[, "est.std"]) |>
      (\(p) if (isTRUE(se != "none")) { p[, c("lhs", "op", "rhs", "level", "est", "se", "z", "pvalue", "stdyx")] } else { p[, c("lhs", "op", "rhs", "level", "est", "stdyx")] })()

    if (isTRUE(se != "none")) { model.param[apply(model.param[, c("z", "pvalue")], 1L, function(y) all(is.na(y))), "se"] <- NA }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Modification Indices ####

  model.modind <- NULL
  if (isTRUE("modind" %in% print)) {

    model.modind <- tryCatch(suppressWarnings(lavaan::modindices(model.fit)),
                             error = function(y) {

                               warning("Modification indices could not be computed.", call. = FALSE)

                               return(NULL)

                             })

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Score Test ####

  model.score <- NULL
  if (isTRUE("modind" %in% print)) {

    model.score <- tryCatch(lavaan::lavTestScore(model.fit, epc = TRUE, warn = FALSE), error = function(y) {

        warning("Modification indices for parameter constraints could not be computed.", call. = FALSE)

        return(NULL)

        }, warning = function(z) {})

    # Combine score tests and expected parameter changes
    if (isTRUE(!is.null(model.score))) {

      # Parameter table
      partable <- lavaan::parTable(model.fit)

      # Univariate score statistics
      uniscore <- model.score$uni

      # Effects coding
      if (isTRUE(ident == "effect")) { uniscore <- uniscore[-grep("-", uniscore$rhs), ] }

      # Expected parameter change
      epcscore <- model.score$epc

      model.score <- data.frame(label = NA, lhs = NA, op = NA, rhs = NA, mi = NA, df = NA, pvalue = NA, lhs.epc = NA, rhs.epc = NA, lhs.stdyx = NA, rhs.stdyx = NA)

      for (i in seq_len(nrow(uniscore))) {

        model.score[i, ] <- data.frame(label = partable[partable$plabel == uniscore[i, "lhs"], "label"],
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

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Residual Correlation Matrix ####

  model.resid <- NULL
  if (isTRUE("resid" %in% print)) {

    model.resid <- tryCatch(lavaan::lavResiduals(model.fit, type = "cor.bollen"), error = function(y) {

      warning("Residual correlation matrix indices could not be computed.", call. = FALSE)

      return(NULL)

      }, warning = function(z) {})

    # Combine residual correlation matrix and standardized residual means
    if (isTRUE(!is.null(model.resid))) {

      model.resid <- list(within = do.call("rbind", model.resid$within[c("cov", "mean")]), between = do.call("rbind", model.resid$.cluster[c("cov", "mean")]))

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Relative Opdyke Distribution Percentile Matrix ####

  model.opdyke <- NULL
  if (isTRUE("opdyke" %in% print)) {

    # Within
    model.opdyke.w <- tryCatch(suppressWarnings(.opdyke.percentiles(lavaan::lavInspect(model.fit, what = "sampstat.std")$within$cov,
                                                                    lavaan::lavInspect(model.fit, what = "cor.ov")$within, prec = opdyke.prec)),
                             error = function(y) {

                               warning("Opdyke percentile matrix could not be computed at the Within level.", call. = FALSE)

                               return(NULL)

                             })

    # Between
    model.opdyke.b <- tryCatch(suppressWarnings(.opdyke.percentiles(lavaan::lavInspect(model.fit, what = "sampstat.std")$.cluster$cov,
                                                                    lavaan::lavInspect(model.fit, what = "cor.ov")$.cluster, prec = opdyke.prec)),
                               error = function(y) {

                                 warning("Opdyke percentile matrix could not be computed at the Between level.", call. = FALSE)

                                 return(NULL)

                               })

    # Combine percentile matrices
    model.opdyke <- list(within = model.opdyke.w, between = model.opdyke.b)


  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Test Statistic and Standard Error ####

  # Test statistic
  test <- rev(lavaan::inspect(model.fit, what = "options")$test)[1L]

  # Standard error
  se <- lavaan::inspect(model.fit, what = "options")$se

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## lavaan Summary ####

  lavaan.summary <- NULL
  if (isTRUE("summary" %in% print)) {

    #—————————————————————————————————————— #
    ### Number of Model Parameters ####

    # Level 1 model parameters
    npar.l1 <- sum(mod.par$level == 1L & mod.par$free != 0L)

    # Level 2 model parameters
    npar.l2 <- sum(mod.par$level == 2L & mod.par$free != 0L)

    # Number of model parameters
    npar <- npar.l1 + npar.l2

    # Number of equality constraints
    npar.eq <- sum(table(misty::chr.omit(mod.par$label)) - 1L)

    #—————————————————————————————————————— #
    ### Summary Table ####

    lavaan.summary <- data.frame(# First column
                                 c(paste("lavaan", lavaan::lavInspect(model.fit, what = "version")), "", "Estimator", "Optimization Method", "",
                                   "Test Statistic", "Standard Errors", "Missing Data", "Identification","",
                                   "Number of Model Parameters", "Within", "Between",
                                   "Number of Equality Constraints", "", "",
                                   "Number of Observations", "Number of Clusters", "Average Cluster Size"),
                                 # Second column
                                 unlist(c("", "",
                                          # Estimator
                                          lavaan::lavTech(model.fit, what = "options")$estimator,
                                          # Optimization method
                                          toupper(lavaan::lavTech(model.fit, what = "options")$optim.method), "",
                                          # Test statistic
                                          switch(test,
                                                 "none" = "None",
                                                 "standard" = "Conventional",
                                                 "yuan.bentler" = "Yuan-Bentler",
                                                 "yuan.bentler.mplus" = "Asymptotic Yuan-Bentler"),
                                          # Standard errors
                                          switch(se,
                                                 "none" = "None",
                                                 "standard" = "Conventional",
                                                 "robust.huber.white" = "Huber-White"),
                                          # Missing data
                                          ifelse(any(is.na(x[, var])), ifelse(missing == "listwise", "Listwise", "FIML"), "None"),
                                          # Identification
                                          switch(ident,
                                                 "marker" = "Marker Variable",
                                                 "var" = "Std. LV",
                                                 "effect" = "Effects Coding"), "",
                                          # Number of model parameters
                                          npar, npar.l1, npar.l2,
                                          # Number of equality constraints
                                          npar.eq, "", "Used",
                                          # Number of observations
                                          lavaan::lavInspect(model.fit, what = "nobs"),
                                          # Number of clusters
                                          lavaan::lavInspect(model.fit, what = "nclusters"),
                                          # Average cluster size
                                          lavaan::lavInspect(model.fit, what = "ncluster.size"))),
                                  # Third column
                                  c(rep("", times = 15L), "Total", n.total, "", ""),
                                  fix.empty.names = FALSE)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model Fit ####

  if (isTRUE(!is.null(model.fit.measures))) {

    #—————————————————————————————————————— #
    ### Simultaneous Model Fit Information only ####

    if (isTRUE(!ls.fit)) {

      model.fit.measures <- data.frame(# Fist column
                                       c("Loglikelihood",
                                         "H0 Value, Specified Model", "Scaling Correction Factor", "H1 Value, Unrestricted Model", "Scaling Correction Factor", "",
                                         "Information Criteria", "Akaike (AIC)", "Bayesian (BIC)", "Sample-Size Adjusted BIC", "",
                                         "Chi-Square Test of Model Fit",
                                         "Test statistic", "Degrees of freedom", "P-value", "Scaling Correction Factor", "",
                                         "Incremental Fit Indices",
                                         "CFI", "TLI", "",
                                         "Absolute Fit Indices",
                                         "RMSEA", "90 Percent CI - lower", "90 Percent CI - upper", "P-value RMSEA <= 0.05", "",
                                         "SRMR", "Within", "Between", "",
                                         "Coefficient of Determination", "GFI", "90 Percent CI - lower", "90 Percent CI - upper"),
                                       # Second column
                                       standard = c(# Loglikelihood
                                                    NA, model.fit.measures[c("logl", "scaling.factor.h0", "unrestricted.logl", "scaling.factor.h1")], NA, NA,
                                                    # Information Criteria
                                                    model.fit.measures[c("aic", "bic", "bic2")], NA, NA,
                                                    # Test statistic, Degrees of freedom, P-value, and Scaling correction factor
                                                    model.fit.measures[c("chisq", "df", "pvalue", "chisq.scaling.factor")], NA, NA,
                                                    # CFI and TLI
                                                    model.fit.measures[c("cfi", "tli")], NA, NA,
                                                    # RMSEA
                                                    model.fit.measures[c("rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue")], NA,
                                                    # SRMR
                                                    model.fit.measures[c("srmr", "srmr_within", "srmr_between")], NA, NA,
                                                    # GFI
                                                    model.fit.measures[c("gfi", "gfi.ci.lower", "gfi.ci.upper")]),
                                       # Third column
                                       scaled = c(# Loglikelihood and information criteria
                                                  rep(NA, times = 12L),
                                                  # Test statistic, Degrees of freedom, P-value, and Scaling correction factor
                                                  model.fit.measures[c("chisq.scaled", "df.scaled", "pvalue.scaled", "chisq.scaling.factor")] |> (\(p) if (isTRUE(is.na(p["chisq.scaled"]))) { rep(NA, times = 4L) } else { p })(), NA, NA,
                                                  # Scaled CFI and TLI
                                                  model.fit.measures[c("cfi.scaled", "tli.scaled")], NA, NA,
                                                  # Scaled RMSEA
                                                  model.fit.measures[c("rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "rmsea.pvalue.scaled")] |> (\(p) if (isTRUE(is.na(model.fit.measures["chisq.scaled"]))) { rep(NA, times = 4L) } else { p })(), NA,
                                                  # SRMR
                                                  rep(NA, times = 2L), NA, NA,
                                                  # GFI
                                                  rep(NA, times = 4L)),
                                       # Fourth column
                                       robust = c(# Loglikelihood and information criteria
                                                  rep(NA, times = 18L),
                                                  # Robust CFI and TLI
                                                  model.fit.measures[c("cfi.robust", "tli.robust")], NA, NA,
                                                  # Robust RMSEA
                                                  model.fit.measures[c("rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust", "rmsea.pvalue.robust")], NA,
                                                  # SRMR
                                                  rep(NA, times = 3L), NA, NA,
                                                  # GFI
                                                  model.fit.measures[c("gfi.robust", "gfi.ci.lower.robust", "gfi.ci.upper.robust")]),
                                       fix.empty.names = FALSE)

    } else {

      #—————————————————————————————————————— #
      ### Simultaneous and Level-specific Model Fit Information ####

      model.fit.measures <- data.frame(# Fist column
                                       c("Loglikelihood",
                                         "H0 Value, Specified Model", "Scaling Correction Factor", "H1 Value, Unrestricted Model", "Scaling Correction Factor", "",
                                         "Information Criteria",
                                         "Akaike (AIC)", "Bayesian (BIC)", "Sample-Size Adjusted BIC", "",
                                         "Chi-Square Test of Model Fit",
                                         "Test statistic", "Within", "Between",
                                         "Degrees of Freedom", "Within", "Between",
                                         "P-value", "Within", "Between",
                                         "Scaling Correction Factor", "Within", "Between", "",
                                         "Incremental Fit Indices",
                                         "CFI", "Within", "Between", "",
                                         "TLI", "Within", "Between", "",
                                         "Absolute Fit Indices",
                                         "RMSEA", "Within", "Between", "",
                                         "90 Percent CI - Lower", "Within", "Between",
                                         "90 Percent CI - Upper", "Within", "Between",
                                         "P-value RMSEA <= 0.05", "Within", "Between", "",
                                         "SRMR", "Within", "Between", "",
                                         "Coefficient of Determination",
                                         "GFI", "Within", "Between", "", "90 Percent CI - Lower", "Within", "Between", "90 Percent CI - Upper", "Within", "Between"),
                                       # Second column
                                       standard = c(# Loglikelihood
                                                    NA, model.fit.measures$simul[c("logl", "scaling.factor.h0", "unrestricted.logl", "scaling.factor.h1")], NA, NA,
                                                    # Information Criteria
                                                    model.fit.measures$simul[c("aic", "bic", "bic2")], NA, NA,
                                                    # Test statistic
                                                    model.fit.measures$simul["chisq"], model.fit.measures$fit.l1["chisq"], model.fit.measures$fit.l2["chisq"],
                                                    # Degrees of freedom
                                                    model.fit.measures$simul["df"], model.fit.measures$fit.l1["df"],  model.fit.measures$fit.l2["df"],
                                                    # P-value
                                                    model.fit.measures$simul["pvalue"], model.fit.measures$fit.l1["pvalue"],  model.fit.measures$fit.l2["pvalue"],
                                                    # Scaling correction factor
                                                    model.fit.measures$simul["chisq.scaling.factor"], model.fit.measures$fit.l1["chisq.scaling.factor"],  model.fit.measures$fit.l2["chisq.scaling.factor"], NA, NA,
                                                    # CFI
                                                    model.fit.measures$simul["cfi"], model.fit.measures$fit.l1["cfi"], model.fit.measures$fit.l2["cfi"], NA,
                                                    # TLI
                                                    model.fit.measures$simul["tli"], model.fit.measures$fit.l1["tli"], model.fit.measures$fit.l2["tli"], NA, NA,
                                                    # RMSEA
                                                    model.fit.measures$simul["rmsea"], model.fit.measures$fit.l1["rmsea"], model.fit.measures$fit.l2["rmsea"], NA,
                                                    model.fit.measures$simul["rmsea.ci.lower"], model.fit.measures$fit.l1["rmsea.ci.lower"], model.fit.measures$fit.l2["rmsea.ci.lower"],
                                                    model.fit.measures$simul["rmsea.ci.upper"], model.fit.measures$fit.l1["rmsea.ci.upper"], model.fit.measures$fit.l2["rmsea.ci.upper"],
                                                    model.fit.measures$simul["rmsea.pvalue"], model.fit.measures$fit.l1["rmsea.pvalue"], model.fit.measures$fit.l2["rmsea.pvalue"], NA,
                                                    # SRMR
                                                    model.fit.measures$simul["srmr"], model.fit.measures$simul["srmr_within"], model.fit.measures$simul["srmr_between"], NA, NA,
                                                    # GFI
                                                    model.fit.measures$simul["gfi"], model.fit.measures$fit.l1["gfi"], model.fit.measures$fit.l2["gfi"], NA,
                                                    model.fit.measures$simul["gfi.ci.lower"], model.fit.measures$fit.l1["gfi.ci.lower"], model.fit.measures$fit.l2["gfi.ci.lower"],
                                                    model.fit.measures$simul["gfi.ci.upper"], model.fit.measures$fit.l1["gfi.ci.upper"], model.fit.measures$fit.l2["gfi.ci.upper"]),
                                      # Third column
                                      scaled = c(# Loglikelihood and Information Criteria
                                                 rep(NA, times = 12L),
                                                 # Test statistic
                                                 model.fit.measures$simul["chisq.scaled"], model.fit.measures$fit.l1["chisq.scaled"], model.fit.measures$fit.l2["chisq.scaled"],
                                                 # Degrees of freedom
                                                 model.fit.measures$simul["df.scaled"], model.fit.measures$fit.l1["df.scaled"], model.fit.measures$fit.l2["df.scaled"],
                                                 # P-value
                                                 model.fit.measures$simul["pvalue.scaled"], model.fit.measures$fit.l1["pvalue.scaled"], model.fit.measures$fit.l2["pvalue.scaled"],
                                                 # Scaling correction factor
                                                 model.fit.measures$simul["chisq.scaling.factor"], model.fit.measures$fit.l1["chisq.scaling.factor"], model.fit.measures$fit.l2["chisq.scaling.factor"], NA, NA,
                                                 # CFI
                                                 model.fit.measures$simul["cfi.scaled"], model.fit.measures$fit.l1["cfi.scaled"], model.fit.measures$fit.l2["cfi.scaled"], NA,
                                                 # TLI
                                                 model.fit.measures$simul["tli.scaled"], model.fit.measures$fit.l1["tli.scaled"], model.fit.measures$fit.l2["tli.scaled"], NA, NA,
                                                 # RMSEA
                                                 model.fit.measures$simul["rmsea.scaled"], model.fit.measures$fit.l1["rmsea.scaled"], model.fit.measures$fit.l2["rmsea.scaled"], NA,
                                                 model.fit.measures$simul["rmsea.ci.lower.scaled"], model.fit.measures$fit.l1["rmsea.ci.lower.scaled"], model.fit.measures$fit.l2["rmsea.ci.lower.scaled"],
                                                 model.fit.measures$simul["rmsea.ci.upper.scaled"], model.fit.measures$fit.l1["rmsea.ci.upper.scaled"], model.fit.measures$fit.l2["rmsea.ci.upper.scaled"],
                                                 model.fit.measures$simul["rmsea.pvalue.scaled"], model.fit.measures$fit.l1["rmsea.pvalue.scaled"], model.fit.measures$fit.l2["rmsea.pvalue.scaled"], NA,
                                                 # SRMR
                                                 rep(NA, times = 3L), NA, NA,
                                                 # GFI
                                                 rep(NA, times = 10)),
                                      # Fourth column
                                      robust = c(# Loglikelihood and Information Criteria
                                                 rep(NA, times = 12L),
                                                 # Test statistic
                                                 rep(NA, times = 3L),
                                                 # Degrees of freedom
                                                 rep(NA, times = 3L),
                                                 # P-value
                                                 rep(NA, times = 3L),
                                                 # Scaling correction factor
                                                 rep(NA, times = 3L), NA, NA,
                                                 # CFI
                                                 model.fit.measures$simul["cfi.robust"], model.fit.measures$fit.l1["cfi.robust"], model.fit.measures$fit.l2["cfi.robust"], NA,
                                                 # TLI
                                                 model.fit.measures$simul["tli.robust"], model.fit.measures$fit.l1["tli.robust"], model.fit.measures$fit.l2["tli.robust"], NA, NA,
                                                 # RMSEA
                                                 model.fit.measures$simul["rmsea.robust"], model.fit.measures$fit.l1["rmsea.robust"], model.fit.measures$fit.l2["rmsea.robust"], NA,
                                                 model.fit.measures$simul["rmsea.ci.lower.robust"], model.fit.measures$fit.l1["rmsea.ci.lower.robust"], model.fit.measures$fit.l2["rmsea.ci.lower.robust"],
                                                 model.fit.measures$simul["rmsea.ci.upper.robust"], model.fit.measures$fit.l1["rmsea.ci.upper.robust"], model.fit.measures$fit.l2["rmsea.ci.upper.robust"],
                                                 model.fit.measures$simul["rmsea.pvalue.robust"], model.fit.measures$fit.l1["rmsea.pvalue.robust"], model.fit.measures$fit.l2["rmsea.pvalue.robust"], NA,
                                                 # SRMR
                                                 rep(NA, times = 3L), NA, NA,
                                                 # GFI
                                                 model.fit.measures$simul["gfi.robust"], model.fit.measures$fit.l1["gfi.robust"], model.fit.measures$fit.l2["gfi.robust"], NA,
                                                 model.fit.measures$simul["gfi.ci.lower.robust"], model.fit.measures$fit.l1["gfi.ci.lower.robust"], model.fit.measures$fit.l2["gfi.ci.lower.robust"],
                                                 model.fit.measures$simul["gfi.ci.upper.robust"], model.fit.measures$fit.l1["gfi.ci.upper.robust"], model.fit.measures$fit.l2["gfi.ci.upper.robust"]), fix.empty.names = FALSE)

    }

    # Remove empty rows
    which(model.fit.measures[, 1L] != "" & !model.fit.measures[, 1L] %in% c("Loglikelihood", "Information Criteria", "Chi-Square Test of Model Fit", "Incremental Fit Indices", "Absolute Fit Indices", "Coefficient of Determination") & apply(model.fit.measures[, -1L], 1L, function(y) all(is.na(y)))) |> (\(p) if (isTRUE(length(p) > 0L)) { model.fit.measures <<- model.fit.measures[-p, ]  } )()

    # Remove empty columns
    which(apply(model.fit.measures, 2L, function(y) all(is.na(y)))) |> (\(p) if (isTRUE(length(p) > 0L)) { model.fit.measures <<- model.fit.measures[, -p]  } )()

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Parameter Estimates ####

  if (isTRUE(!is.null(model.param))) {

    # Within parameters
    param.w <- model.param[model.param$level == 1L, ]

    # Between parameters
    param.b <- model.param[model.param$level == 2L, ]

    #—————————————————————————————————————— #
    ### Within Parameter Estimates ####

    # Latent variables
    latent.w <- param.w[which(param.w$op == "=~"), ]

    # Latent variable covariances
    lv.cov.w <- param.w[which(param.w$op == "~~" & (param.w$lhs != param.w$rhs) & (param.w$lhs %in% latent.w$lhs) & (param.w$rhs %in% latent.w$lhs)), ]

    # Residual covariances
    res.cov.w <- param.w[which(param.w$op == "~~" & (param.w$lhs != param.w$rhs) & (!param.w$lhs %in% latent.w$lhs) & (!param.w$rhs %in% latent.w$lhs)), ]

    # Latent mean
    mean.w <- param.w[which(param.w$op == "~1" & param.w$lhs %in% latent.w$lhs), ]

    # Latent variance
    var.w <- param.w[which(param.w$op == "~~" & (param.w$lhs %in% latent.w$lhs) & (param.w$lhs == param.w$rhs)), ]

    # Intercepts
    interc.w <- param.w[which(param.w$op == "~1" & !param.w$lhs %in% latent.w$lhs), ]

    # Residual variance
    resid.w <- param.w[which(param.w$op == "~~" & (param.w$lhs == param.w$rhs) & (!param.w$lhs %in% latent.w$lhs) & (!param.w$rhs %in% latent.w$lhs)), ]

    #—————————————————————————————————————— #
    ### Between Parameter Estimates ####

    # Latent variables
    latent.b <- param.b[which(param.b$op == "=~"), ]

    # Latent variable covariances
    lv.cov.b <- param.b[which(param.b$op == "~~" & (param.b$lhs != param.b$rhs) & (param.b$lhs %in% latent.b$lhs) & (param.b$rhs %in% latent.b$lhs)), ]

    # Residual covariances
    res.cov.b <- param.b[which(param.b$op == "~~" & (param.b$lhs != param.b$rhs) & (!param.b$lhs %in% latent.b$lhs) & (!param.b$rhs %in% latent.b$lhs)), ]

    # Latent mean
    mean.b <- param.b[which(param.b$op == "~1" & param.b$lhs %in% latent.b$lhs), ]

    # Latent variance
    var.b <- param.b[which(param.b$op == "~~" & (param.b$lhs %in% latent.b$lhs) & (param.b$lhs == param.b$rhs)), ]

    # Intercepts
    interc.b <- param.b[which(param.b$op == "~1" & !param.b$lhs %in% latent.b$lhs), ]

    # Residual variance
    resid.b <- param.b[which(param.b$op == "~~" & (param.b$lhs == param.b$rhs) & (!param.b$lhs %in% latent.b$lhs) & (!param.b$rhs %in% latent.b$lhs)), ]

    # Model parameters
    model.param <- list(# Model parameter Within
                        within = rbind(if (isTRUE(nrow(latent.w) > 0L)) { data.frame(param = "latent variable", latent.w) } else { NULL },
                                       if (isTRUE(nrow(lv.cov.w) > 0L)) { data.frame(param = "latent variable covariance", lv.cov.w) } else { NULL },
                                       if (isTRUE(nrow(res.cov.w) > 0L)) { data.frame(param = "residual covariance", res.cov.w) } else { NULL },
                                       if (isTRUE(nrow(mean.w) > 0L)) { data.frame(param = "latent mean", mean.w) } else { NULL },
                                       if (isTRUE(nrow(var.w) > 0L)) { data.frame(param = "latent variance", var.w) } else { NULL },
                                       if (isTRUE(nrow(interc.w) > 0L)) { data.frame(param = "intercept", interc.w) } else { NULL },
                                       if (isTRUE(nrow(resid.w) > 0L)) { data.frame(param = "residual variance", resid.w) } else { NULL }),
                        # Model parameter Between
                        between = rbind(if (isTRUE(nrow(latent.b) > 0L)) { data.frame(param = "latent variable", latent.b) } else { NULL },
                                        if (isTRUE(nrow(lv.cov.b) > 0L)) { data.frame(param = "latent variable covariance", lv.cov.b) } else { NULL },
                                        if (isTRUE(nrow(res.cov.b) > 0L)) { data.frame(param = "residual covariance", res.cov.b) } else { NULL },
                                        if (isTRUE(nrow(mean.b) > 0L)) { data.frame(param = "latent mean", mean.b) } else { NULL },
                                        if (isTRUE(nrow(var.b) > 0L)) { data.frame(param = "latent variance", var.b) } else { NULL },
                                        if (isTRUE(nrow(interc.b) > 0L)) { data.frame(param = "intercept", interc.b) } else { NULL },
                                        if (isTRUE(nrow(resid.b) > 0L)) { data.frame(param = "residual variance", resid.b) } else { NULL }))

    #—————————————————————————————————————— #
    ### Within Labels ####

    # Latent mean and intercept
    model.param$within[model.param$within$param %in% c("latent mean", "intercept"), "rhs"] <- model.param$within[model.param$within$param %in% c("latent mean", "intercept"), "lhs"]

    # Latent variables
    param.lv.w <- NULL
    for (i in unique(model.param$within[which(model.param$within$param == "latent variable"), "lhs"])) {

      param.lv.w <- rbind(param.lv.w,
                          if (isTRUE(se != "none")) { data.frame(param = "latent variable", lhs = i, op = "", rhs = paste(i, "=~"), est = NA, se = NA, z = NA, pvalue = NA, stdyx = NA) } else { data.frame(param = "latent variable", lhs = i, op = "", rhs = paste(i, "=~"), est = NA, stdyx = NA) },
                          model.param$within[which(model.param$within$param == "latent variable" & model.param$within$lhs == i), colnames(model.param$within) != "level"])



    }

    # Latent variable covariances
    param.lv.cov.w <- NULL
    for (i in unique(model.param$within[which(model.param$within$param == "latent variable covariance"), "lhs"])) {

      param.lv.cov.w <- rbind(param.lv.cov.w,
                              if (isTRUE(se != "none")) { data.frame(param = "latent variable covariance", lhs = i, op = "", rhs = paste(i, "~~"), est = NA, se = NA, z = NA, pvalue = NA, stdyx = NA) } else { data.frame(param = "latent variable covariance", lhs = i, op = "", rhs = paste(i, "~~"), est = NA, stdyx = NA) },
                              model.param$within[which(model.param$within$param == "latent variable covariance" & model.param$within$lhs == i), colnames(model.param$within) != "level"])

    }

    # Residual covariances
    param.res.cov.w <- NULL
    for (i in unique(model.param$within[which(model.param$within$param == "residual covariance"), "lhs"])) {

      param.res.cov.w <- rbind(param.res.cov.w,
                               if (isTRUE(se != "none")) { data.frame(param = "residual covariance", lhs = i, op = "", rhs = paste(i, "~~"), est = NA, se = NA, z = NA, pvalue = NA, stdyx = NA) } else { data.frame(param = "residual covariance", lhs = i, op = "", rhs = paste(i, "~~"), est = NA, stdyx = NA) },
                               model.param$within[which(model.param$within$param == "residual covariance" & model.param$within$lhs == i), colnames(model.param$within) != "level"])

    }

    model.param$within <- rbind(param.lv.w, param.lv.cov.w, param.res.cov.w,
                                model.param$within[which(!model.param$within$param %in% c("latent variable", "latent variable covariance", "residual covariance")), colnames(model.param$within) != "level"])

    #—————————————————————————————————————— #
    ### Between Labels ####

    # Latent mean and intercept
    model.param$between[model.param$between$param %in% c("latent mean", "intercept"), "rhs"] <- model.param$between[model.param$between$param %in% c("latent mean", "intercept"), "lhs"]

    # Latent variables
    param.lv.b <- NULL
    for (i in unique(model.param$between[which(model.param$between$param == "latent variable"), "lhs"])) {

      param.lv.b <- rbind(param.lv.b,
                          if (isTRUE(se != "none")) { data.frame(param = "latent variable", lhs = i, op = "", rhs = paste(i, "=~"), est = NA, se = NA, z = NA, pvalue = NA, stdyx = NA) } else { data.frame(param = "latent variable", lhs = i, op = "", rhs = paste(i, "=~"), est = NA, stdyx = NA)  },
                          model.param$between[which(model.param$between$param == "latent variable" & model.param$between$lhs == i), colnames(model.param$between) != "level"])

    }

    # Latent variable covariances
    param.lv.cov.b <- NULL
    for (i in unique(model.param$between[which(model.param$between$param == "latent variable covariance"), "lhs"])) {

      param.lv.cov.b <- rbind(param.lv.cov.b,
                              if (isTRUE(se != "none")) { data.frame(param = "latent variable covariance", lhs = i, op = "", rhs = paste(i, "~~"), est = NA, se = NA, z = NA, pvalue = NA, stdyx = NA) } else { data.frame(param = "latent variable covariance", lhs = i, op = "", rhs = paste(i, "~~"), est = NA, stdyx = NA) },
                              model.param$between[which(model.param$between$param == "latent variable covariance" & model.param$between$lhs == i), colnames(model.param$between) != "level"])

    }

    # Residual covariances
    param.res.cov.b <- NULL
    for (i in unique(model.param$between[which(model.param$between$param == "residual covariance"), "lhs"])) {

      param.res.cov.b <- rbind(param.res.cov.b,
                               if (isTRUE(se != "none")) { data.frame(param = "residual covariance", lhs = i, op = "", rhs = paste(i, "~~"), est = NA, se = NA, z = NA, pvalue = NA, stdyx = NA) } else { data.frame(param = "residual covariance", lhs = i, op = "", rhs = paste(i, "~~"), est = NA, stdyx = NA) },
                               model.param$between[which(model.param$between$param == "residual covariance" & model.param$between$lhs == i), colnames(model.param$between) != "level"])

    }

    model.param$between <- rbind(param.lv.b, param.lv.cov.b, param.res.cov.b,
                                 model.param$between[which(!model.param$between$param %in% c("latent variable", "latent variable covariance", "residual covariance")), colnames(model.param$between) != "level"])

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Modification Indices ####

  if (isTRUE(!is.null(model.modind))) {

    if (isTRUE("level" %in% colnames(model.modind))) {

      model.modind <- list(within = misty::df.rename(model.modind[which(model.modind$level == 1L), c("lhs", "op", "rhs", "mi", "epc", "sepc.all")], from = "sepc.all", to = "stdyx"),
                           between = misty::df.rename(model.modind[which(model.modind$level == 2L), c("lhs", "op", "rhs", "mi", "epc", "sepc.all")], from = "sepc.all", to = "stdyx"))

    } else {

      model.modind <- list(within = misty::df.rename(model.modind[, c("lhs", "op", "rhs", "mi", "epc", "sepc.all")], from = "sepc.all", to = "stdyx"),
                           between = misty::df.rename(model.modind[-c(1L:nrow(model.modind)), c("lhs", "op", "rhs", "mi", "epc", "sepc.all")], from = "sepc.all", to = "stdyx"))

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Return Object ####

  object <- list(call = match.call(),
                 type = "multilevel.cfa",
                 data = x,
                 args = list(model = model, rescov = rescov, model.w = model.w, model.b = model.b, rescov.w = rescov.w, rescov.b = rescov.b,
                             const = const, fix.resid = fix.resid, ident = ident, ls.fit = ls.fit, estimator = estimator, test = test, se = se, optim.method = optim.method,
                             missing = missing, print = print, mod.minval = mod.minval, resid.minval = resid.minval, opdyke.prec = opdyke.prec, opdyke.minmax = opdyke.minmax, color = color, style = style, digits = digits, p.digits = p.digits,
                             as.na = as.na, write = write, append = append, check = check, output = output),
                 model = mod.l12,
                 model.fit = model.fit,
                 check = list(vcov = check.vcov, theta.w = check.theta.w, theta.b = check.theta.b, cov.lv.w = check.cov.lv.w, cov.lv.b = check.cov.lv.b),
                 result = list(summary = lavaan.summary, coverage = coverage, descript = descript.var, fit = model.fit.measures,
                               param = model.param, modind = model.modind, score = model.score, resid = model.resid, opdyke = model.opdyke))

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
