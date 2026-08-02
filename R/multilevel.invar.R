#' Cross-Level Measurement Invariance Evaluation
#'
#' This function evaluates configural, metric, and scalar cross-level measurement
#' invariance using multilevel confirmatory factor analysis with continuous indicators
#' by calling the \code{cfa} function in the R package \pkg{lavaan}. By default,
#' the function evaluates configural and metric cross-level measurement invariance
#' Additionally, variance-covariance coverage of the data, descriptive statistics,
#' parameter estimates, modification indices, residual correlation matrix, and
#' relative Opdyke distribution percentile matrix can be requested by specifying
#' the argument \code{print}.
#'
#' @param data          a data frame. If \code{model} is \code{NULL},
#'                      multilevel confirmatory factor analysis based on a
#'                      measurement model with one factor at the Within and Between
#'                      level comprising all variables in the data frame is conducted
#'                      to evaluate cross-level measurement invariance. Note that
#'                      the cluster variable specified in \code{cluster} is excluded
#'                      from \code{data} when specifying the argument \code{cluster}
#'                      using the variable name of the cluster variable. If \code{model}
#'                      is specified, the matrix or data frame needs to contain
#'                      all variables used in the \code{model} argument.
#' @param ...           an expression indicating the variable names in \code{data},
#'                      e.g., \code{multilevel.invar(dat, x1, x2, x3, cluster = "cluster")}.
#'                      Note that the operators \code{+}, \code{-}, \code{~},
#'                      \code{:}, \code{::}, and \code{!} can also be used to
#'                      select variables, see 'Details' in the \code{\link{df.subset}}
#'                      function.
#' @param cluster       either a character string indicating the variable name of
#'                      the cluster variable in \code{data}, or a vector representing
#'                      the nested grouping structure (i.e., group or cluster variable).
#' @param model         a character vector specifying the same factor structure
#'                      with one factor at the Within and Between Level, or a list
#'                      of character vectors for specifying the same measurement
#'                      model with more than one factor at the Within and Between
#'                      Level, e.g.,\code{model = c("x1", "x2", "x3", "x4")} for
#'                      specifying a measurement model with one factor labeled
#'                      \code{wf} at the Within level and a measurement model with
#'                      one factor labeled \code{bf} at the Between level each
#'                      comprising four indicators, or \code{model = list(factor1 = c("x1", "x2", "x3", "x4"),
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
#'                      \code{model.w}, \code{model.b}, and \code{model.b}.
#' @param invar         a character string indicating the level of measurement invariance
#'                      to be evaluated, i.e., \code{config} to evaluate configural
#'                      measurement invariance (i.e., same factor structure across
#'                      levels), \code{metric} (default) to evaluate configural and
#'                      metric measurement invariance (i.e., equal factor loadings
#'                      across level), and \code{scalar} to evaluate configural,
#'                      metric and scalar measurement invariance (i.e., all residual
#'                      variances at the Between level equal zero).
#' @param fix.resid     a character vector for specifying residual variances to be
#'                      fixed at 0 at the Between level for the configural and metric
#'                      invariance model, e.g., \code{fix.resid = c("x1", "x3")}
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
#' @param estimator     a character string indicating the estimator to be used:
#'                      \code{"ML"} for maximum likelihood with conventional standard
#'                      errors and \code{"MLR"} (default) for maximum likelihood
#'                      with Huber-White robust standard errors and a scaled test
#'                      statistic that is asymptotically equal to the Yuan-Bentler
#'                      test statistic. Note that by default, full information maximum
#'                      likelihood (FIML) method is used to deal with missing data
#'                      when using \code{"ML"} (\code{missing = "fiml"}), whereas
#'                      incomplete cases are removed listwise (i.e., \code{missing = "listwise"})
#'                       when using \code{"MLR"}.
#' @param optim.method  a character string indicating the optimizer, i.e., \code{"nlminb"}
#'                      (default) for the unconstrained and bounds-constrained
#'                      quasi-Newton method optimizer and \code{"em"} for the
#'                      Expectation Maximization (EM) algorithm.
#' @param missing       a character string indicating how to deal with missing data,
#'                      i.e., \code{"listwise"} (default) for listwise deletion or
#'                      \code{"fiml"} for full information maximum likelihood (FIML)
#'                      method. Note that FIML method is only available when
#'                      \code{estimator = "ML"}, that it takes longer to estimate
#'                      the model  using FIML, and that FIML is prone to convergence
#'                      issues which might be resolved by switching to listwise deletion.
#' @param print         a character string or character vector indicating which
#'                      results to show on the console, i.e. \code{"all"} for all
#'                      results, \code{"summary"} for a summary of the specification
#'                      of the estimation method and missing data handling in lavaan,
#'                      \code{"coverage"} for the variance-covariance coverage of
#'                      the data, \code{"descript"} for descriptive statistics,
#'                      \code{"fit"} for model fit and  model comparison, \code{"est"}
#'                      for parameter estimates, and \code{"modind"} for modification
#'                      indices. By default, a summary of the specification and model fit
#'                      and model comparison are printed.
#' @param print.fit     a character string or character vector indicating which
#'                      version of the CFI, TLI, and RMSEA to show on the console,
#'                      i.e., \code{"all"} for all versions of the CFI, TLI, and
#'                      RMSEA, \code{"standard"} (default when \code{estimator = "ML"})
#'                      for fit indices without any non-normality correction,
#'                      \code{"scaled"} for population-corrected robust fit indices
#'                      with ad hoc non-normality correction, and \code{robust}
#'                      (default when \code{estimator = "MLR"}) for sample-corrected
#'                      robust fit indices based on formula provided by Li and Bentler
#'                      (2006) and Brosseau-Liard and Savalei (2014).
#' @param mod.minval    numeric value to filter modification indices and only show
#'                      modifications with a modification index value equal or higher
#'                      than this minimum value. By default, modification indices
#'                      equal or higher 6.63 are printed. Note that a modification
#'                      index value of 6.63 is equivalent to a significance level
#'                      of \eqn{\alpha = .01}.
#' @param resid.minval  numeric value indicating the minimum absolute residual
#'                      correlation coefficients and standardized means to highlight
#'                      in boldface. By default, absolute residual correlation
#'                      coefficients and standardized means equal or higher 0.1
#'                      are highlighted. Note that highlighting can be disabled by
#'                      setting the minimum value to 1.
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
#'                      \code{".xlsx"}  (e.g., \code{"Output.xlsx"}). If the file
#'                      name does not contain any file extension, an Excel file will
#'                      be written.
#' @param append        logical: if \code{TRUE} (default), output will be appended
#'                      to an existing text file with extension \code{.txt} specified
#'                      in \code{write}, if \code{FALSE} existing text file will be
#'                      overwritten.
#' @param check         logical: if \code{TRUE} (default), argument specification,
#'                      convergence and model identification is checked.
#' @param output        logical: if \code{TRUE} (default), output is shown.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{multilevel.cfa}}, \code{\link{multilevel.fit}}, \code{\link{multilevel.omega}},
#' \code{\link{multilevel.cor}}, \code{\link{multilevel.descript}}, \code{\link{item.invar}},
#' \code{\link{write.result}}
#'
#' @references
#' Rosseel, Y. (2012). lavaan: An R Package for Structural Equation Modeling.
#' \emph{Journal of Statistical Software, 48}, 1-36. https://doi.org/10.18637/jss.v048.i02
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#'
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{data}}{data frame specified in \code{data}}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{model}}{list with specified model for the configural, metric, and
#'                     scalar invariance model}
#' \item{\code{model.fit}}{list with fitted lavaan object of the configural, metric,
#'                         and scalar invariance model}
#' \item{\code{check}}{list with the results of the convergence and model identification
#'                     check for the configural, metric, and scalar invariance model}
#' \item{\code{result}}{list with result tables, i.e., \code{summary} for the
#'                      summary of the specification of the estimation method and
#'                      missing data handling in lavaan, \code{coverage} for the
#'                      variance-covariance coverage of the data, \code{descript}
#'                      for descriptive statistics, \code{fit} for a list with
#'                      model fit based on standard, scaled, and robust fit indices,
#'                      \code{est} for a list with parameter estimates for the
#'                      configural, metric, and scalar invariance model, and
#'                      \code{modind} for the list with modification indices for
#'                      the configural, metric, and scalar invariance model,
#'                      \code{score} for the list with result of the score tests
#'                      for constrained parameters for the configural, metric,
#'                      and scalar invariance model, and \code{resid} for the list
#'                      with residual correlation matrices and standardized
#'                      residual means for the configural, metric, and scalar
#'                      invariance model}
#'
#' @note
#' The function uses the functions \code{lavTestLRT} provided in the R package
#' \pkg{lavaan} by Yves Rosseel (2012).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load data set "Demo.twolevel" in the lavaan package
#' data("Demo.twolevel", package = "lavaan")
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Cluster Variable Specification
#'
#' # Example 1a: Specification using the argument '...'
#' multilevel.invar(Demo.twolevel, y1:y4, cluster = "cluster")
#'
#' # Example 1b: Alternative specification with cluster variable 'cluster' in 'data'
#' multilevel.invar(Demo.twolevel[, c("y1", "y2", "y3", "y4", "cluster")], cluster = "cluster")
#'
#' # Example 1c: Alternative specification with cluster variable 'cluster' not in 'data'
#' multilevel.invar(Demo.twolevel[, c("y1", "y2", "y3", "y4")], cluster = Demo.twolevel$cluster)
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Model Specification Using 'data' for an One-Factor Model
#'
#' #——————————————————————————————————————
#' ## Level of Measurement Invariance
#'
#' # Example 2a: Configural invariance
#' multilevel.invar(Demo.twolevel, y1, y2, y3, y4, cluster = "cluster", invar = "config")
#'
#' # Example 2b: Metric invariance
#' multilevel.invar(Demo.twolevel, y1, y2, y3, y4, cluster = "cluster", invar = "metric")
#'
#' # Example 2c: Scalar invariance
#' multilevel.invar(Demo.twolevel, y1, y2, y3, y4, cluster = "cluster", invar = "scalar")
#'
#' #——————————————————————————————————————
#' ## Residual (Co-)Variance at the Within and Between Level
#'
#' # Example 3a: Residual covariance between "y3" and "y4" at the Within level
#' multilevel.invar(Demo.twolevel, y1, y2, y3, y4, cluster = "cluster",
#'                  rescov = c("y3", "y4"))
#'
#' # Example 3b: Residual variances of 'y1' at the Between level fixed at 0
#' multilevel.invar(Demo.twolevel, y1, y2, y3, y4, cluster = "cluster", fix.resid = "y1")
#'
#' #——————————————————————————————————————
#' ## Arguments 'print', 'mod.minval', 'resid.minval', and 'opdyke.minmax'
#'
#' # Example 4a: Request all results
#' multilevel.invar(Demo.twolevel, y1:y4, cluster = "cluster", print = "all")
#'
#' # Example 4b: Request modification indices with value equal or higher than 2
#' multilevel.invar(Demo.twolevel, y1:y4, cluster = "cluster", print = "modind", mod.minval = 2)
#'
#' # Example 4c: Highlight absolute residual correlation equal or higher than 0.05
#' multilevel.invar(Demo.twolevel, y1:y4, cluster = "cluster", print = "resid", resid.minval = 0.05,
#'                  color = "b.blue")
#'
#' # Example 4d: Highlight Opdyke distribution percentiles outside 0.45 and 0.55
#' multilevel.invar(Demo.twolevel, y1:y4, cluster = "cluster", print = "opdyke", opdyke.minmax = c(0.45, 0.55),
#'                  color = "black", style = "bold")
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # lavaan Summary of the Estimated Model
#'
#' # Example 5: lavaan model and summary of the estimated model
#' mod <- multilevel.invar(Demo.twolevel, y1, y2, y3, y4, cluster = "cluster", output = FALSE)
#'
#' # lavaan syntax of the metric invariance model
#' mod$model$metric
#'
#' # Fitted lavaan object of the metric invariance model
#' lavaan::summary(mod$model.fit$metric, standardized = TRUE, fit.measures = TRUE)
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Model Specification Using 'model' for an One- or Multiple-Factor Model
#'
#' # Example 6a: One-factor model
#' multilevel.invar(Demo.twolevel, cluster = "cluster", model = c("y1", "y2", "y3", "y4"))
#'
#' # Example 6b:  Two-factor model
#' multilevel.invar(Demo.twolevel, cluster = "cluster",
#'                  model = list(c("y1", "y2", "y3"), c("y4", "y5", "y6")))
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Write Results
#'
#' # Example 7a: Write Results into a Excel file
#' multilevel.invar(Demo.twolevel, y1, y2, y3, y4, cluster = "cluster", print = "all",
#'                  write = "Multilevel_Invariance.txt")
#'
#' # Example 7b:  Write Results into a Excel file
#' multilevel.invar(Demo.twolevel, y1, y2, y3, y4, cluster = "cluster", print = "all",
#'                  write = "Multilevel_Invariance.xlsx")
#' }
multilevel.invar <- function(data, ..., cluster, model = NULL, rescov = NULL,
                             invar = c("config", "metric", "scalar"), fix.resid = NULL,
                             ident = c("marker", "var", "effect"), estimator = c("ML", "MLR"),
                             optim.method = c("nlminb", "em"), missing = c("listwise", "fiml"),
                             print = c("all", "summary", "coverage", "descript", "fit", "est", "modind", "resid", "opdyke"),
                             print.fit = c("all", "standard", "scaled", "robust"), mod.minval = 6.63,
                             resid.minval = 0.1, opdyke.prec = 1, opdyke.minmax = c(0.40, 0.60),
                             color = "b.red", style = c("regular", "bold", "italic"),
                             digits = 3, p.digits = 3, as.na = NULL, write = NULL,
                             append = TRUE, check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'data' is missing
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

    # Cluster variable
    cluster <- data[, cluster]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Without Using the Argument '...' ####

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

  # Convert 'cluster' as tibble into a vector
  if (!is.null(cluster) && isTRUE("tbl" %in% substr(class(cluster), 1L, 3L))) { cluster <- unname(unlist(cluster)) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  .check.input(logical = c("append", "output"),
               numeric = list(mod.minval = 1L, resid.minval = 1L),
               s.character = list(invar = c("config", "metric", "scalar"), ident = c("marker", "var", "effect"), estimator = c("ML", "MLR"), optim.method = c("nlminb", "em"), missing = c("listwise", "fiml"), style = c("regular", "bold", "italic")),
               m.character = list(print = c("all", "summary", "coverage", "descript", "fit", "est", "modind", "resid", "opdyke"), print.fit = c("all", "standard", "scaled", "robust")),
               args = c("color", "digits", "p.digits", "write2"), package = "lavaan", envir = environment(), input.check = check)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Additional Checks

  if (isTRUE(check)) {

    #—————————————————————————————————————— #
    ### Check lavaan Version ####

    if (isTRUE(substr(packageDescription("lavaan")$Version, 3L, 3L) %in% seq_len(6L))) { stop("This function requires at least lavaan version 0.7-2 (published 2026-07-16), please update the package.", call. = FALSE) }

    #—————————————————————————————————————— #
    ### Check 'model' ####

    # Check if input 'model' is a character vector or list of character vectors
    if (isTRUE(!is.null(model) && !all(sapply(model, is.character)))) { stop("Please specify a character vector or list of character vectors for the argument 'model'.", call. = FALSE) }

    # Model specification with 'model'
    if (isTRUE(!is.null(model))) { (!unique(unlist(model)) %in% colnames(x)) |> (\(y) if (isTRUE(any(y))) { stop(paste0("Variables specified in the argument 'model' were not found in 'data': ", paste(unique(unlist(model))[y], collapse = ", ")), call. = FALSE) })() }

    #—————————————————————————————————————— #
    ### Check 'rescov' ####

    if (isTRUE(!is.null(rescov))) {

      # Two variables for each residual covariance
      if (isTRUE(is.list(rescov) && any(sapply(rescov, length) != 2L))) { stop("Please specify a list of character vectors for the argument 'rescov', where each element has two variable names", call. = FALSE)

      } else { if (isTRUE(length(rescov) != 2L)) { stop("Please specify a character vector with two variable names for the argument 'rescov'", call. = FALSE) } }

      # Variable in 'data'
      (!unique(unlist(rescov)) %in% colnames(x)) |> (\(y) if (isTRUE(any(y))) { stop(paste0("Variables specified in the argument 'rescov' were not found in 'data': ", paste(unique(unlist(rescov))[y], collapse = ", ")), call. = FALSE) })()

    }

    #—————————————————————————————————————— #
    ### Check 'fix.resid', 'mod.minval', and 'resid.minval' ####

    # Check input 'fix.resid'
    (!unique(fix.resid) %in% colnames(x)) |> (\(y) if (isTRUE(any(y) &&  all(fix.resid != "all"))) { stop(paste0("Variables specified in the argument 'fix.resid' were not found in 'data': ", paste(fix.resid[y], collapse = ", ")), call. = FALSE) })()

    # Check input 'mod.minval'
    if (isTRUE(mod.minval < 0L)) { stop("Please specify a value greater than or equal 0 for the argument 'mod.minval'.", call. = FALSE) }

    ## Check input 'resid.minval'
    if (isTRUE(resid.minval < 0L)) { stop("Please specify a value greater than or equal 0 for the argument 'resid.minval'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Data and Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Manifest variables ####

  #—————————————————————————————————————— #
  ### Model Specification with 'data' ####

  if (isTRUE(is.null(model))) {

    var <- colnames(x)

  #—————————————————————————————————————— #
  ### Model Specification with 'model' ####

  } else if (isTRUE(!is.null(model))) {

    var <- unique(unlist(model))

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data frame with Cluster Variable ####

  x <- data.frame(x, .cluster = cluster, row.names = NULL)

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
  ### Model Specification with 'model' ####

  if (isTRUE(!is.null(model))) {

    # 'model' is a list
    if (isTRUE(is.list(model))) {

      # List elements not all named
      if (isTRUE(is.null(names(model)) || any(names(model) == ""))) { names(model) <- paste0("f", seq_along(model)) }

    # 'model' is not a list
    } else {

      model <- list(f = model)

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'invar' Argument ####

  if (isTRUE(all(c("config", "metric", "scalar") %in% invar))) {

    invar <- "metric"

  } else if (isTRUE(length(invar) != 1)) {

    stop("Please specify a character string for the argument 'invar'.", call. = FALSE)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'fix.resid' Argument ####

  if (isTRUE(fix.resid == "all")) { fix.resid <- var }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'ident' Argument ####

  if (isTRUE(all(c("marker", "var", "effect") %in% ident))) {

    ident <- "var"

  } else if (isTRUE(length(ident) != 1)) {

    stop("Please specify a character string for the argument 'ident'.", call. = FALSE)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'estimator' Argument  ####

  if (isTRUE(all(c("ML", "MLR") %in% estimator))) { estimator <- "MLR" }

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

      missing <- "listwise"

    } else if (isTRUE(estimator == "MLR" && missing == "fiml")) {

      warning("FIML method is currently not available for estimator = \"MLR\", argument 'estimator' switched to \"ML\".", call. = FALSE)

      estimator <- "ML"

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Missing Data on All Variable ####

  (misty::na.prop(x[, var], append = FALSE) == 1L) |> (\(y) if (isTRUE(any(y) && missing == "fiml")) { warning(paste0("Data contains cases with missing values on all variables, number of cases removed from the analysis: ", sum(y)), call. = FALSE) })()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'print' Argument ####

  if (isTRUE(all(c("all", "summary", "coverage", "descript", "fit", "est", "modind", "resid", "opdyke") %in% print))) {

    print  <- c("summary", "fit")

  } else if (isTRUE(all(print == "all"))) {

    print <- c("summary", "coverage", "descript", "fit", "est", "modind", "resid", "opdyke")

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'print.fit' Argument ####

  if (isTRUE(all(c("all", "standard", "scaled", "robust") %in% print.fit))) {

    print.fit <- ifelse(isTRUE(estimator == "ML"), "standard", "robust")

  } else if (isTRUE(length(print.fit) == 1L && "all" %in% print.fit)) {

    if (isTRUE(estimator == "ML")) { print.fit <- "standard" } else { print.fit <- c("standard", "scaled", "robust") }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'style' Argument ####

  if (isTRUE(all(c("regular", "bold", "italic") %in% style))) { style <- "regular" }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model Estimation ####

  #—————————————————————————————————————— #
  ### Model Specification with 'data' ####

  model.fit.metric <- model.fit.scalar <- warn.config <- warn.metric <- warn.scalar <- NULL

  if (isTRUE(is.null(model))) {

    #···················
    #### Configural Measurement Invariance ####

    model.fit.config <- withCallingHandlers(misty::multilevel.cfa(x, cluster = ".cluster", model = NULL, rescov = NULL,
                                                                  model.w = var, model.b = var, rescov.w = rescov, rescov.b = NULL,
                                                                  const = "config", fix.resid = fix.resid, ident = ident, ls.fit = FALSE,
                                                                  estimator = estimator, optim.method = optim.method,
                                                                  missing = missing, print = print, opdyke.prec = opdyke.prec, output = FALSE),
                                            warning = function(y) { warn.config <<- c(warn.config, conditionMessage(y)); invokeRestart("muffleWarning") })

    #···················
    #### Metric Measurement Invariance ####

    if (isTRUE(any(c("metric", "scalar") %in% invar))) {

      model.fit.metric <- withCallingHandlers(misty::multilevel.cfa(x, cluster = ".cluster", model = var, rescov = rescov,
                                                                    model.w = NULL, model.b = NULL, rescov.w = NULL, rescov.b = NULL,
                                                                    fix.resid = fix.resid, ident = ident, ls.fit = FALSE,
                                                                    estimator = estimator, optim.method = optim.method,
                                                                    missing = missing, print = print, opdyke.prec = opdyke.prec, output = FALSE),
                                              warning = function(y) { warn.metric <<- c(warn.metric, conditionMessage(y)); invokeRestart("muffleWarning") })

    }

    #···················
    #### Scalar Measurement Invariance ####

    if (isTRUE(any("scalar" %in% invar))) {

      model.fit.scalar <- withCallingHandlers(misty::multilevel.cfa(x, cluster = ".cluster", model = var, rescov = rescov,
                                                                    model.w = NULL, model.b = NULL, rescov.w = NULL, rescov.b = NULL,
                                                                    const = "config", fix.resid = "all",
                                                                    ident = ident, ls.fit = FALSE, estimator = estimator, optim.method = optim.method,
                                                                    missing = missing, print = print, opdyke.prec = opdyke.prec, output = FALSE),
                                              warning = function(y) { warn.scalar <<- c(warn.scalar, conditionMessage(y)); invokeRestart("muffleWarning") })

    }

    for(i in unique(c(warn.config, warn.metric, warn.scalar))) { warning(i, call. = FALSE) }

  #—————————————————————————————————————— #
  ### Model Specification with 'model' ####

  } else {

    #···················
    #### Configural Measurement Invariance ####

    model.fit.config <- withCallingHandlers(misty::multilevel.cfa(x, cluster = ".cluster", model = NULL, rescov = NULL,
                                                                  model.w = model, model.b = model, rescov.w = rescov, rescov.b = NULL,
                                                                  fix.resid = fix.resid, ident = ident, ls.fit = FALSE,
                                                                  estimator = estimator, optim.method = optim.method,
                                                                  missing = missing, print = print, opdyke.prec = opdyke.prec, output = FALSE),
                                            warning = function(y) { warn.config <<- c(warn.config, conditionMessage(y)); invokeRestart("muffleWarning") })

    #···················
    #### Metric Measurement Invariance ####

    if (isTRUE(any(c("metric", "scalar") %in% invar))) {

      model.fit.metric <- withCallingHandlers(misty::multilevel.cfa(x, cluster = ".cluster", model = model, rescov = rescov,
                                                                    model.w = NULL, model.b = NULL, rescov.w = NULL, rescov.b = NULL,
                                                                    const = "config", fix.resid = fix.resid, ident = ident, ls.fit = FALSE,
                                                                    estimator = estimator, optim.method = optim.method,
                                                                    missing = missing, print = print, opdyke.prec = opdyke.prec, output = FALSE),
                                              warning = function(y) { warn.metric <<- c(warn.metric, conditionMessage(y)); invokeRestart("muffleWarning") })

    }

    #···················
    #### Scalar Measurement Invariance ####

    if (isTRUE(any("scalar" %in% invar))) {

      model.fit.scalar <- withCallingHandlers(misty::multilevel.cfa(x, cluster = ".cluster", model = model, rescov = rescov,
                                                                    model.w = NULL, model.b = NULL, rescov.w = NULL, rescov.b = NULL,
                                                                    const = "config", fix.resid = "all",
                                                                    ident = ident, ls.fit = FALSE, estimator = estimator, optim.method = optim.method,
                                                                    missing = missing, print = print, opdyke.prec = opdyke.prec, output = FALSE),
                                              warning = function(y) { warn.scalar <<- c(warn.scalar, conditionMessage(y)); invokeRestart("muffleWarning") })

    }

  }

  #_____________________________________________________________________________
  #
  # Return object --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## lavaan Summary ####

  switch(invar,
         #—————————————————————————————————————— #
         ### Configural Measurement Invariance ####
         config = {

           lavaan.summary <- rbind(model.fit.config$result$summary[1L:10L, ], c("", "Config", ""), model.fit.config$result$summary[11L:19L, ])

         #—————————————————————————————————————— #
         ### Metric Measurement Invariance ####
         }, metric = {

           lavaan.summary <- rbind(model.fit.config$result$summary[1L:10L, ],
                                   c("", "Config", "Metric"),
                                   data.frame(model.fit.config$result$summary[11L:14L, 1L], model.fit.config$result$summary[11L:14L, 2L], model.fit.metric$result$summary[11L:14L, 2L], fix.empty.names = FALSE),
                                   c("", "", ""),
                                   model.fit.config$result$summary[16L:19L, ])

         #—————————————————————————————————————— #
         ### Scalar Measurement Invariance ####
         }, scalar = {

           lavaan.summary <- rbind(data.frame(model.fit.config$result$summary[1L:10L, ], "", fix.empty.names = FALSE),
                                   c("", "Config", "Metric", "Scalar"),
                                   data.frame(model.fit.config$result$summary[11L:14L, 1L], model.fit.config$result$summary[11L:14L, 2L], model.fit.metric$result$summary[11L:14L, 2L],  model.fit.scalar$result$summary[11L:14L, 2L], fix.empty.names = FALSE),
                                   c("", "", "", ""),
                                   data.frame(model.fit.config$result$summary[16L:19L, ], "", fix.empty.names = FALSE))

         })

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model Fit ####

  fit.stand <- fit.scaled <- fit.robust <- fit.config.scaled <- fit.config.robust <- fit.metric.scaled <- fit.metric.robust <- fit.scalar.scaled <- fit.scalar.robust <- NULL
  if (isTRUE("fit" %in% print)) {

    #—————————————————————————————————————— #
    ### Fit Indices for Configural Measurement Invariance ####

    # Extract model fit information
    fit.config <- model.fit.config$result$fit

    #···················
    #### Standard Fit Indices ####

    fit.config.stand <- fit.config[c(which(fit.config[, 1L] == "Chi-Square Test of Model Fit"):(which(fit.config[, 1L] == "GFI") + 2L), (which(fit.config[, 1L] == "Information Criteria") - 1L):(which(fit.config[, 1L] == "Sample-Size Adjusted BIC"))), c(1L, 2L)]

    #···················
    #### Scaled Fit Indices ####

    if (isTRUE("scaled" %in% colnames(fit.config))) {

      fit.config.scaled <- fit.config[c(which(fit.config[, 1L] == "Chi-Square Test of Model Fit"):(which(fit.config[, 1L] == "SRMR") + 2L), (which(fit.config[, 1L] == "Information Criteria") - 1L):(which(fit.config[, 1L] == "Sample-Size Adjusted BIC"))), c(1L, 3L)]

      fit.config.scaled[c(which(fit.config.scaled[, 1L] == "SRMR"):(which(fit.config.scaled[, 1L] == "Between")), which(fit.config.scaled[, 1L] == "Information Criteria"):(which(fit.config.scaled[, 1L] == "Sample-Size Adjusted BIC"))), 2L] <- fit.config[c(which(fit.config[, 1L] == "SRMR"):(which(fit.config[, 1L] == "Between")), which(fit.config[, 1L] == "Information Criteria"):(which(fit.config[, 1L] == "Sample-Size Adjusted BIC"))), 2L]

    }

    #···················
    #### Robust Fit Indices ####

    if (isTRUE("robust" %in% colnames(fit.config))) {

      fit.config.robust <- fit.config[c(which(fit.config[, 1L] == "Chi-Square Test of Model Fit"):(which(fit.config[, 1L] == "GFI") + 2L), (which(fit.config[, 1L] == "Information Criteria") - 1L):(which(fit.config[, 1L] == "Sample-Size Adjusted BIC"))), c(1L, 4L)]

      fit.config.robust[c(which(fit.config.robust[, 1L] == "SRMR"):(which(fit.config.robust[, 1L] == "Between")), which(fit.config.robust[, 1L] == "Information Criteria"):(which(fit.config.robust[, 1L] == "Sample-Size Adjusted BIC"))), 2L] <- fit.config[c(which(fit.config[, 1L] == "SRMR"):(which(fit.config[, 1L] == "Between")), which(fit.config[, 1L] == "Information Criteria"):(which(fit.config[, 1L] == "Sample-Size Adjusted BIC"))), 2L]

      fit.config.robust[which(fit.config.robust[, 1L] == "Chi-Square Test of Model Fit"):(which(fit.config.robust[, 1L] == "Incremental Fit Indices") - 2L), c(1L, 2L)] <- fit.config[which(fit.config[, 1L] == "Chi-Square Test of Model Fit"):(which(fit.config[, 1L] == "Incremental Fit Indices") - 2L), c(1L, 3L)]

    }

    #—————————————————————————————————————— #
    ### Fit Indices for Metric Measurement Invariance ####

    if (isTRUE(any(c("metric", "scalar") %in% invar))) {

      # Extract model fit information
      fit.metric <- model.fit.metric$result$fit

      #···················
      #### Standard Fit Indices ####

      fit.metric.stand <- fit.metric[c(which(fit.metric[, 1L] == "Chi-Square Test of Model Fit"):(which(fit.metric[, 1L] == "GFI") + 2L), (which(fit.metric[, 1L] == "Information Criteria") - 1L):(which(fit.metric[, 1L] == "Sample-Size Adjusted BIC"))), c(1L, 2L)]

      #···················
      #### Scaled Fit Indices ####

      if (isTRUE("scaled" %in% colnames(fit.metric))) {

        fit.metric.scaled <- fit.metric[c(which(fit.metric[, 1L] == "Chi-Square Test of Model Fit"):(which(fit.metric[, 1L] == "SRMR") + 2L), (which(fit.metric[, 1L] == "Information Criteria") - 1L):(which(fit.metric[, 1L] == "Sample-Size Adjusted BIC"))), c(1L, 3L)]

        fit.metric.scaled[c(which(fit.metric.scaled[, 1L] == "SRMR"):(which(fit.metric.scaled[, 1L] == "Between")), which(fit.metric.scaled[, 1L] == "Information Criteria"):(which(fit.metric.scaled[, 1L] == "Sample-Size Adjusted BIC"))), 2L] <- fit.metric[c(which(fit.metric[, 1L] == "SRMR"):(which(fit.metric[, 1L] == "Between")), which(fit.metric[, 1L] == "Information Criteria"):(which(fit.metric[, 1L] == "Sample-Size Adjusted BIC"))), 2L]

      }

      #···················
      #### Robust Fit Indices ####

      if (isTRUE("robust" %in% colnames(fit.metric))) {

        fit.metric.robust <- fit.metric[c(which(fit.metric[, 1L] == "Chi-Square Test of Model Fit"):(which(fit.metric[, 1L] == "GFI") + 2L), (which(fit.metric[, 1L] == "Information Criteria") - 1L):(which(fit.metric[, 1L] == "Sample-Size Adjusted BIC"))), c(1L, 4L)]

        fit.metric.robust[c(which(fit.metric.robust[, 1L] == "SRMR"):(which(fit.metric.robust[, 1L] == "Between")), which(fit.metric.robust[, 1L] == "Information Criteria"):(which(fit.metric.robust[, 1L] == "Sample-Size Adjusted BIC"))), 2L] <- fit.metric[c(which(fit.metric[, 1L] == "SRMR"):(which(fit.metric[, 1L] == "Between")), which(fit.metric[, 1L] == "Information Criteria"):(which(fit.metric[, 1L] == "Sample-Size Adjusted BIC"))), 2L]

        fit.metric.robust[which(fit.metric.robust[, 1L] == "Chi-Square Test of Model Fit"):(which(fit.metric.robust[, 1L] == "Incremental Fit Indices") - 2L), c(1L, 2L)] <- fit.metric[which(fit.metric[, 1L] == "Chi-Square Test of Model Fit"):(which(fit.metric[, 1L] == "Incremental Fit Indices") - 2L), c(1L, 3L)]

      }

    }

    #—————————————————————————————————————— #
    ### Fit Indices for Scalar Measurement Invariance ####

    if (isTRUE("scalar" %in% invar)) {

      # Extract model fit information
      fit.scalar <- model.fit.scalar$result$fit

      #···················
      #### Standard Fit Indices ####

      fit.scalar.stand <- fit.scalar[c(which(fit.scalar[, 1L] == "Chi-Square Test of Model Fit"):(which(fit.scalar[, 1L] == "GFI") + 2L), (which(fit.scalar[, 1L] == "Information Criteria") - 1L):(which(fit.scalar[, 1L] == "Sample-Size Adjusted BIC"))), c(1L, 2L)]

      #···················
      #### Scaled Fit Indices ####

      if (isTRUE("scaled" %in% colnames(fit.scalar))) {

        fit.scalar.scaled <- fit.scalar[c(which(fit.scalar[, 1L] == "Chi-Square Test of Model Fit"):(which(fit.scalar[, 1L] == "SRMR") + 2L), (which(fit.scalar[, 1L] == "Information Criteria") - 1L):(which(fit.scalar[, 1L] == "Sample-Size Adjusted BIC"))), c(1L, 3L)]

        fit.scalar.scaled[c(which(fit.scalar.scaled[, 1L] == "SRMR"):(which(fit.scalar.scaled[, 1L] == "Between")), which(fit.scalar.scaled[, 1L] == "Information Criteria"):(which(fit.scalar.scaled[, 1L] == "Sample-Size Adjusted BIC"))), 2L] <- fit.scalar[c(which(fit.scalar[, 1L] == "SRMR"):(which(fit.scalar[, 1L] == "Between")), which(fit.scalar[, 1L] == "Information Criteria"):(which(fit.scalar[, 1L] == "Sample-Size Adjusted BIC"))), 2L]

      }

      #···················
      #### Robust Fit Indices ####

      if (isTRUE("robust" %in% colnames(fit.scalar))) {

        fit.scalar.robust <- fit.scalar[c(which(fit.scalar[, 1L] == "Chi-Square Test of Model Fit"):(which(fit.scalar[, 1L] == "GFI") + 2L), (which(fit.scalar[, 1L] == "Information Criteria") - 1L):(which(fit.scalar[, 1L] == "Sample-Size Adjusted BIC"))), c(1L, 4L)]

        fit.scalar.robust[c(which(fit.scalar.robust[, 1L] == "SRMR"):(which(fit.scalar.robust[, 1L] == "Between")), which(fit.scalar.robust[, 1L] == "Information Criteria"):(which(fit.scalar.robust[, 1L] == "Sample-Size Adjusted BIC"))), 2L] <- fit.scalar[c(which(fit.scalar[, 1L] == "SRMR"):(which(fit.scalar[, 1L] == "Between")), which(fit.scalar[, 1L] == "Information Criteria"):(which(fit.scalar[, 1L] == "Sample-Size Adjusted BIC"))), 2L]

        fit.scalar.robust[which(fit.scalar.robust[, 1L] == "Chi-Square Test of Model Fit"):(which(fit.scalar.robust[, 1L] == "Incremental Fit Indices") - 2L), c(1L, 2L)] <- fit.scalar[which(fit.scalar[, 1L] == "Chi-Square Test of Model Fit"):(which(fit.scalar[, 1L] == "Incremental Fit Indices") - 2L), c(1L, 3L)]

      }

    }

    #—————————————————————————————————————— #
    ### Combine Fit Indices ####

    #···················
    #### Configural Measurement Invariance ####

    switch(invar, config = {

      ##### Standard Fit Indices ####

      fit.stand <- fit.config.stand

      fit.scaled <- fit.robust <- NULL

      ##### Robust Maximum Likelihood ####

      if (isTRUE(estimator == "MLR")) {

        ###### Scaled fit indices

        if (isTRUE(!is.null(fit.config.scaled))) { fit.scaled <- data.frame(fit.config.scaled[, 1L], config = fit.config.scaled[, 2L], fix.empty.names = FALSE) }

        ###### Robust fit indices

        if (isTRUE(!is.null(fit.config.robust))) { fit.robust <- data.frame(fit.config.robust[, 1L], config = fit.config.robust[, 2L], fix.empty.names = FALSE) }

        ###### Scaling correction factor

        if (isTRUE(!is.null(fit.scaled))) { if (isTRUE(is.na(fit.scaled[5L, "config", drop = FALSE]))) { warning("Scaling correction factor could not be computed for following model(s): Configural", call. = FALSE) } }

      }

      colnames(fit.stand) <- c("", "config")
      if (isTRUE(!is.null(fit.scaled))) { colnames(fit.scaled) <- c("", "config") }
      if (isTRUE(!is.null(fit.scaled))) { colnames(fit.robust) <- c("", "config") }

      #···················
      #### Configural and Metric Measurement Invariance ####

      }, metric = {

        setdiff(fit.config.stand[, 1L], fit.metric.stand[, 1L]) |>
          (\(p) if (isTRUE(length(p) != 0L)) {

            fit.config.stand <<- fit.config.stand[which(fit.config.stand[, 1L] != p), ]
            fit.metric.stand <<- fit.metric.stand[which(fit.metric.stand[, 1L] != p), ]

          })()

      ##### Chi-Squared Difference Test, Config vs. Metric ####
      chidiff.confmet <- tryCatch(lavaan::lavTestLRT(model.fit.config$model.fit, model.fit.metric$model.fit),
                                  error = function(y) { warning("test"); data.frame(matrix(NA, ncol = 7L, dimnames = list(NULL, c("Df", "AIC", "BIC", "Chisq", "Chisq diff", "Df diff", "Pr(>Chisq)"))), check.names = FALSE) },
                                  warning = function(z) { suppressWarnings(lavaan::lavTestLRT(model.fit.config$model.fit, model.fit.metric$model.fit, method = "satorra.bentler.2010")) })

      if (isTRUE(all(is.na(chidiff.confmet[, "Pr(>Chisq)"])))) { warning("Chi-square difference test Configural vs. Metric could not be computed.", call. = FALSE) }

      # Negative chi-squared value even though model fit decreased
      if (isTRUE(chidiff.confmet[2L, "Chisq"] - chidiff.confmet[1L, "Chisq"] > 0L && chidiff.confmet[2L, "Chisq diff"] < 0L)) { chidiff.confmet <- data.frame(matrix(NA, ncol = 7L, dimnames = list(NULL, c("Df", "AIC", "BIC", "Chisq", "Chisq diff", "Df diff", "Pr(>Chisq)"))), check.names = FALSE) }

      ##### Standard Fit Indices ####

      # Combine fit indices for configural and metric measurement invariance
      fit.stand <- data.frame(fit.config.stand[, 1L], config = fit.config.stand[, 2L], metric = fit.metric.stand[, 2L],
                              dmetric = fit.metric.stand[, 2L] - fit.config.stand[, 2L], fix.empty.names = FALSE)

      # Chi-squared difference test, config vs. metric
      fit.stand[2L:4L, "dmetric"] <- unlist(chidiff.confmet[2L, c("Chisq diff", "Df diff", "Pr(>Chisq)")])

      fit.stand[fit.stand[, 1L] %in% c("Scaling Correction Factor", "90 Percent CI - lower", "90 Percent CI - upper", "P-value RMSEA <= 0.05"), "dmetric"] <- NA

      fit.scaled <- fit.robust <- NULL

      ##### Robust Maximum Likelihood ####

      if (isTRUE(estimator == "MLR")) {

        ###### Scaled fit indices
        if (isTRUE(!is.null(fit.config.scaled))) {

          # Joint rows
          setdiff(fit.config.scaled[, 1L], fit.metric.scaled[, 1L]) |>
            (\(p) if (isTRUE(length(p) != 0L)) {

              fit.config.scaled <<- fit.config.scaled[which(fit.config.scaled[, 1L] != p), ]
              fit.metric.scaled <<- fit.metric.scaled[which(fit.metric.scaled[, 1L] != p), ]

            })()

          # Combine fit indices for configural and metric measurement invariance
          fit.scaled <- data.frame(fit.config.scaled[, 1L], config = fit.config.scaled[, 2L], metric = fit.metric.scaled[, 2L],
                                   dmetric = fit.metric.scaled[, 2L] - fit.config.scaled[, 2L], fix.empty.names = FALSE)

          fit.scaled[fit.scaled[, 1L] %in% c("Scaling Correction Factor", "90 Percent CI - lower", "90 Percent CI - upper", "P-value RMSEA <= 0.05"), "dmetric"] <- NA

          # Chi-squared difference test, config vs. metric
          fit.scaled[2L:4L, "dmetric"] <- unlist(chidiff.confmet[2L, c("Chisq diff", "Df diff", "Pr(>Chisq)")])

          # Set difference in scaling correction factor to 0
          fit.scaled[5L, "dmetric"] <- NA

        }

        ###### Robust fit indices
        if (isTRUE(!is.null(fit.config.robust))) {

          # Joint rows
          setdiff(fit.config.robust[, 1L], fit.metric.robust[, 1L]) |>
            (\(p) if (isTRUE(length(p) != 0L)) {

              fit.config.robust <<- fit.config.robust[which(fit.config.robust[, 1L] != p), ]
              fit.metric.robust <<- fit.metric.robust[which(fit.metric.robust[, 1L] != p), ]

            })()

          # Combine fit indices for configural and metric measurement invariance
          fit.robust <- data.frame(fit.config.robust[, 1L], config = fit.config.robust[, 2L], metric = fit.metric.robust[, 2L],
                                   dmetric = fit.metric.robust[, 2L] - fit.config.robust[, 2L], fix.empty.names = FALSE)

          fit.robust[fit.robust[, 1L] %in% c("Scaling Correction Factor", "90 Percent CI - lower", "90 Percent CI - upper", "P-value RMSEA <= 0.05"), "dmetric"] <- NA

          # Chi-squared difference test, config vs. metric
          fit.robust[2L:4L, "dmetric"] <- unlist(chidiff.confmet[2L, c("Chisq diff", "Df diff", "Pr(>Chisq)")])

          # Set difference in scaling correction factor to 0
          fit.robust[5L, "dmetric"] <- NA

        }

        ###### Scaling correction factor
        if (isTRUE(!is.null(fit.scaled))) {

          scale.corr <- is.na(fit.scaled[5L, which(colnames(fit.scaled) %in% c("config", "metric"))])
          if (isTRUE(any(scale.corr))) {

            warning(paste0("Scaling correction factor could not be computed for following model(s): ",
                           paste(c("Configural", "Metric")[match(colnames(scale.corr)[which(scale.corr)], c("config", "metric"))], collapse = ", ")), call. = FALSE)

          }

        }

      }

    #···················
    #### Configural, Metric, and Scalar Measurement Invariance ####

    }, scalar = {

      ##### Joint Rows ####
      unique(c(setdiff(fit.config.stand[, 1L], fit.metric.stand[, 1L]), setdiff(fit.config.stand[, 1L], fit.scalar.stand[, 1L]), setdiff(fit.metric.stand[, 1L], fit.scalar.stand[, 1L]))) |>
        (\(p) if (isTRUE(length(p) != 0L)) {

          fit.config.stand <<- fit.config.stand[which(fit.config.stand[, 1L] != p), ]
          fit.metric.stand <<- fit.metric.stand[which(fit.metric.stand[, 1L] != p), ]
          fit.scalar.stand <<- fit.scalar.stand[which(fit.scalar.stand[, 1L] != p), ]

        })()

      ##### Chi-Squared Difference Test, Config vs. Metric ####
      chidiff.confmet <- tryCatch(lavaan::lavTestLRT(model.fit.config$model.fit, model.fit.metric$model.fit),
                                  error = function(y) { data.frame(matrix(NA, ncol = 7L, dimnames = list(NULL, c("Df", "AIC", "BIC", "Chisq", "Chisq diff", "Df diff", "Pr(>Chisq)"))), check.names = FALSE) },
                                  warning = function(z) { suppressWarnings(lavaan::lavTestLRT(model.fit.config$model.fit, model.fit.metric$model.fit, method = "satorra.bentler.2010")) })

      if (isTRUE(all(is.na(chidiff.confmet[, "Pr(>Chisq)"])))) { warning("Chi-square difference test Configural vs. Metric could not be computed.", call. = FALSE) }

      ##### Chi-Squared Difference Test, Metric vs. Scalar ####
      chidiff.metsca <- tryCatch(lavaan::lavTestLRT(model.fit.metric$model.fit, model.fit.scalar$model.fit),
                                 error = function(y) { data.frame(matrix(NA, ncol = 7L, dimnames = list(NULL, c("Df", "AIC", "BIC", "Chisq", "Chisq diff", "Df diff", "Pr(>Chisq)"))), check.names = FALSE) },
                                 warning = function(z) { suppressWarnings(lavaan::lavTestLRT(model.fit.metric$model.fit, model.fit.scalar$model.fit, method = "satorra.bentler.2010")) })

      if (isTRUE(all(is.na(chidiff.metsca[, "Pr(>Chisq)"])))) { warning("Chi-square difference test Metric vs. Scalar could not be computed.", call. = FALSE) }

      # Negative chi-squared value even though model fit decreased
      if (isTRUE(chidiff.confmet[2L, "Chisq"] - chidiff.confmet[1L, "Chisq"] > 0L && chidiff.confmet[2L, "Chisq diff"] < 0L)) { chidiff.confmet <- data.frame(matrix(NA, ncol = 7L, dimnames = list(NULL, c("Df", "AIC", "BIC", "Chisq", "Chisq diff", "Df diff", "Pr(>Chisq)"))), check.names = FALSE) }
      if (isTRUE(chidiff.metsca[2L, "Chisq"] - chidiff.metsca[1L, "Chisq"] > 0L && chidiff.metsca[2L, "Chisq diff"] < 0L)) { chidiff.metsca <- data.frame(matrix(NA, ncol = 7L, dimnames = list(NULL, c("Df", "AIC", "BIC", "Chisq", "Chisq diff", "Df diff", "Pr(>Chisq)"))), check.names = FALSE) }

      ##### Standard Fit Indices ####

      # Combine fit indices for configural and metric measurement invariance
      fit.stand <- data.frame(fit.config.stand[, 1L],
                              config = fit.config.stand[, 2L],
                              metric = fit.metric.stand[, 2L],
                              scalar = fit.scalar.stand[, 2L],
                              dmetric = fit.metric.stand[, 2L] - fit.config.stand[, 2L],
                              dscalar = fit.scalar.stand[, 2L] - fit.metric.stand[, 2L], fix.empty.names = FALSE)

      # Chi-squared difference test, config vs. metric
      fit.stand[2L:4L, "dmetric"] <- unlist(chidiff.confmet[2L, c("Chisq diff", "Df diff", "Pr(>Chisq)")])

      # Chi-squared difference test, metric vs. scalar
      fit.stand[2L:4L, "dscalar"] <- unlist(chidiff.metsca[2L, c("Chisq diff", "Df diff", "Pr(>Chisq)")])

      fit.stand[fit.stand[, 1L] %in% c("Scaling Correction Factor", "90 Percent CI - lower", "90 Percent CI - upper", "P-value RMSEA <= 0.05"), c("dmetric", "dscalar")] <- NA

      fit.scaled <- fit.robust <- NULL

      ##### Robust Maximum Likelihood ####

      if (isTRUE(estimator == "MLR")) {

        ###### Scaled fit indices
        if (isTRUE(!is.null(fit.config.scaled) && !is.null(fit.metric.scaled) && !is.null(fit.scalar.scaled))) {

          # Combine fit indices for configural and metric measurement invariance
          fit.scaled <- data.frame(fit.config.scaled[, 1L], config = fit.config.scaled[, 2L], metric = fit.metric.scaled[, 2L], scalar = fit.scalar.scaled[, 2L],
                                   dmetric = fit.metric.scaled[, 2L] - fit.config.scaled[, 2L], dscalar = fit.scalar.scaled[, 2L] - fit.metric.scaled[, 2L], fix.empty.names = FALSE)

          fit.scaled[fit.scaled[, 1L] %in% c("Scaling Correction Factor", "90 Percent CI - lower", "90 Percent CI - upper", "P-value RMSEA <= 0.05"), c("dmetric", "dscalar")] <- NA

          # Chi-squared difference test, config vs. metric
          fit.scaled[2L:4L, "dmetric"] <- unlist(chidiff.confmet[2L, c("Chisq diff", "Df diff", "Pr(>Chisq)")])
          # Chi-squared difference test, metric vs. scalar
          fit.scaled[2L:4L, "dscalar"] <- unlist(chidiff.metsca[2L, c("Chisq diff", "Df diff", "Pr(>Chisq)")])

        }

        ###### Robust fit indices
        if (isTRUE(!is.null(fit.config.robust) && !is.null(fit.metric.robust) && !is.null(fit.scalar.robust))) {

          # Combine fit indices for configural and metric measurement invariance
          fit.robust <- data.frame(fit.config.robust[, 1L], config = fit.config.robust[, 2L], metric = fit.metric.robust[, 2L], scalar = fit.scalar.robust[, 2L],
                                   dmetric = fit.metric.robust[, 2L] - fit.config.robust[, 2L], dscalar = fit.scalar.robust[, 2L] - fit.metric.robust[, 2L], fix.empty.names = FALSE)

          fit.robust[fit.robust[, 1L] %in% c("Scaling Correction Factor", "90 Percent CI - lower", "90 Percent CI - upper", "P-value RMSEA <= 0.05"), c("dmetric", "dscalar")] <- NA

          # Chi-squared difference test, config vs. metric
          fit.robust[2L:4L, "dmetric"] <- unlist(chidiff.confmet[2L, c("Chisq diff", "Df diff", "Pr(>Chisq)")])
          # Chi-squared difference test, metric vs. scalar
          fit.robust[2L:4L, "dscalar"] <- unlist(chidiff.metsca[2L, c("Chisq diff", "Df diff", "Pr(>Chisq)")])

        }

        ###### Scaling correction factor
        if (isTRUE(!is.null(fit.scaled))) {

          scale.corr <- is.na(fit.scaled[5L, which(colnames(fit.scaled) %in% c("config", "metric", "scalar"))])
          if (isTRUE(any(scale.corr))) {

            warning(paste0("Scaling correction factor could not be computed for following model(s): ",
                           paste(c("Configural", "Metric", "Scalar")[match(colnames(scale.corr)[which(scale.corr)], c("config", "metric", "scalar"))], collapse = ", ")), call. = FALSE)

          }

        }

      }

    })

    row.names(fit.stand) <- seq_len(nrow(fit.stand))
    if (isTRUE(!is.null(fit.scaled))) { row.names(fit.scaled) <- seq_len(nrow(fit.scaled)) }
    if (isTRUE(!is.null(fit.robust))) { row.names(fit.robust) <- seq_len(nrow(fit.robust)) }

    #—————————————————————————————————————— #
    ### 'print.fit' Argument ####

    print.fit <- ifelse(print.fit == "robust" && is.null(fit.robust), ifelse(!is.null(fit.scaled), "scaled", "standard"), print.fit)
    print.fit <- ifelse(print.fit == "scaled" && is.null(fit.scaled), "standard", print.fit)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Return Object ####

  object <- list(call = match.call(),
                 type = "multilevel.invar",
                 data = x,
                 args = list(model = model, rescov = rescov, invar = invar, ident = ident, estimator = estimator, optim.method = optim.method, missing = missing, print = print, print.fit = print.fit,
                             mod.minval = mod.minval, resid.minval = resid.minval, opdyke.prec = opdyke.prec, opdyke.minmax = opdyke.minmax, color = color, style = style, digits = digits, p.digits = p.digits, write = write, append = append, as.na = as.na, check = check, output = output),
                 model = list(config = model.fit.config$model, metric = model.fit.metric$model, scalar = model.fit.scalar$model),
                 model.fit = list(config = model.fit.config$model.fit, metric = model.fit.metric$model.fit, scalar = model.fit.scalar$model.fit),
                 check = list(config = list(vcov = model.fit.config$check$check.vcov, theta.w = model.fit.config$check$check.theta.w, theta.b = model.fit.config$check$check.theta.b, cov.lv.w = model.fit.config$check$check.cov.lv.w, cov.lv.b = model.fit.config$check$check.cov.lv.b),
                              metric = list(vcov = model.fit.metric$check$check.vcov, theta.w = model.fit.metric$check$check.theta.w, theta.b = model.fit.metric$check$check.theta.b, cov.lv.w = model.fit.metric$check$check.cov.lv.w, cov.lv.b = model.fit.metric$check$check.cov.lv.b),
                              scalar = list(vcov = model.fit.scalar$check$check.vcov, theta.w = model.fit.scalar$check$check.theta.w, theta.b = model.fit.scalar$check$check.theta.b, cov.lv.w = model.fit.scalar$check$check.cov.lv.w, cov.lv.b = model.fit.scalar$check$check.cov.lv.b)),
                 result = list(summary = lavaan.summary, coverage = model.fit.config$result$coverage,
                               descript = model.fit.config$result$descript,
                               fit = list(stand = fit.stand, scaled = fit.scaled, robust = fit.robust),
                               param = list(config = model.fit.config$result$param, metric = model.fit.metric$result$param, scalar = model.fit.scalar$result$param),
                               modind = list(config = model.fit.config$result$modind, metric = model.fit.metric$result$modind, scalar = model.fit.scalar$result$modind),
                               score = list(metric = model.fit.metric$result$score, scalar = model.fit.scalar$result$score),
                               resid = list(config = model.fit.config$result$resid, metric = model.fit.metric$result$resid, scalar = model.fit.scalar$result$resid),
                               opdyke = list(config = model.fit.config$result$opdyke, metric = model.fit.metric$result$opdyke, scalar = model.fit.scalar$result$opdyke)))

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
