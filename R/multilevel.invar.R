#' Cross-Level Measurement Invariance Evaluation
#'
#' This function is a wrapper function for evaluating configural, metric, and
#' scalar cross-level measurement invariance using multilevel confirmatory factor
#' analysis with continuous indicators by calling the \code{cfa} function in the
#' R package \pkg{lavaan}. By default, the function evaluates configural and metric
#' cross-level measurement invariance by providing a table with model fit
#' information (i.e., chi-square test, fit indices and information criteria), and
#' model comparison (i.e., chi-square difference test, change in fit indices, and
#' change in information criteria). Additionally, variance-covariance coverage of
#' the data, descriptive statistics, parameter estimates, modification indices,
#' and residual correlation matrix can be requested by specifying the argument
#' \code{print}.
#'
#' @param data         a data frame. If \code{model} is \code{NULL},
#'                     multilevel confirmatory factor analysis based on a
#'                     measurement model with one factor at the Within and Between
#'                     level comprising all variables in the data frame is conducted
#'                     to evaluate cross-level measurement invariance. Note that
#'                     the cluster variable specified in \code{cluster} is excluded
#'                     from \code{data} when specifying the argument \code{cluster}
#'                     using the variable name of the cluster variable. If \code{model}
#'                     is specified, the matrix or data frame needs to contain
#'                     all variables used in the \code{model} argument.
#' @param ...          an expression indicating the variable names in \code{data},
#'                     e.g., \code{multilevel.invar(dat, x1, x2, x3, cluster = "cluster")}.
#'                     Note that the operators \code{.}, \code{+}, \code{-}, \code{~},
#'                     \code{:}, \code{::}, and \code{!} can also be used to
#'                     select variables, see 'Details' in the \code{\link{df.subset}}
#'                     function.
#' @param cluster      either a character string indicating the variable name of
#'                     the cluster variable in \code{data}, or a vector representing
#'                     the nested grouping structure (i.e., group or cluster variable).
#' @param model        a character vector specifying the same factor structure
#'                     with one factor at the Within and Between Level, or a list
#'                     of character vectors for specifying the same measurement
#'                     model with more than one factor at the Within and Between
#'                     Level, e.g.,\code{model = c("x1", "x2", "x3", "x4")} for
#'                     specifying a measurement model with one factor labeled
#'                     \code{wf} at the Within level and a measurement model with
#'                     one factor labeled \code{bf} at the Between level each
#'                     comprising four indicators, or \code{model = list(factor1 = c("x1", "x2", "x3", "x4"),
#'                     factor2 = c("x5", "x6", "x7", "x8"))} for specifying a
#'                     measurement model with two latent factors labeled \code{wfactor1}
#'                     and \code{wfactor2} at the Within level and a measurement
#'                     model with two latent factors labeled \code{bfactor1} and
#'                     \code{bfactor2} at the Between level each comprising four
#'                     indicators. Note that the name of each list element is used
#'                     to label factors, where prefixes \code{w} and \code{b} are
#'                     added the labels to distinguish factor labels at the Within
#'                     and Between level, i.e., all list elements need to be named,
#'                     otherwise factors are labeled with \code{"wf1", "wf2", "wf3"}
#'                     for labels at the Within level and \code{"bf1", "bf2", "bf3"}
#'                     for labels at the Between level and so on.
#' @param rescov       a character vector or a list of character vectors for specifying
#'                     residual covariances at the Within level, e.g. \code{rescov = c("x1", "x2")}
#'                     for specifying a residual covariance between indicators \code{x1}
#'                     and \code{x2} at the Within level or \code{rescov = list(c("x1", "x2"), c("x3", "x4"))}
#'                     for specifying residual covariances between indicators \code{x1}
#'                     and \code{x2}, and indicators \code{x3} and \code{x4} at
#'                     the Within level. Note that residual covariances at the
#'                     Between level can only be specified by using the arguments
#'                     \code{model.w}, \code{model.b}, and \code{model.b}.
#' @param invar        a character string indicating the level of measurement invariance
#'                     to be evaluated, i.e., \code{config} to evaluate configural
#'                     measurement invariance (i.e., same factor structure across
#'                     levels), \code{metric} (default) to evaluate configural and
#'                     metric measurement invariance (i.e., equal factor loadings
#'                     across level), and \code{scalar} to evaluate configural,
#'                     metric and scalar measurement invariance (i.e., all residual
#'                     variances at the Between level equal zero).
#' @param fix.resid    a character vector for specifying residual variances to be
#'                     fixed at 0 at the Between level for the configural and metric
#'                     invariance model, e.g., \code{fix.resid = c("x1", "x3")}
#'                     to fix residual variances of indicators \code{x1} and \code{x2}
#'                     at the Between level at 0. Note that it is also possible
#'                     to specify \code{fix.resid = "all"} which fixes all residual
#'                     variances at the Between level at 0 in line with the strong
#'                     factorial measurement invariance assumption across cluster.
#' @param ident        a character string indicating the method used for identifying
#'                     and scaling latent variables, i.e., \code{"marker"} for the
#'                     marker variable method fixing the first factor loading of
#'                     each latent variable to 1, \code{"var"} for the fixed variance
#'                     method fixing the variance of each latent variable to 1,
#'                     or \code{"effect"} for the effects-coding method using equality
#'                     constraints so that the average of the factor loading for
#'                     each latent variable equals 1.
#' @param estimator    a character string indicating the estimator to be used:
#'                     \code{"ML"} for maximum likelihood with conventional standard
#'                     errors and \code{"MLR"} (default) for maximum likelihood
#'                     with Huber-White robust standard errors and a scaled test
#'                     statistic that is asymptotically equal to the Yuan-Bentler
#'                     test statistic. Note that by default, full information maximum
#'                     likelihood (FIML) method is used to deal with missing data
#'                     when using \code{"ML"} (\code{missing = "fiml"}), whereas
#'                     incomplete cases are removed listwise (i.e., \code{missing = "listwise"})
#'                     when using \code{"MLR"}.
#' @param optim.method a character string indicating the optimizer, i.e., \code{"nlminb"}
#'                     (default) for the unconstrained and bounds-constrained
#'                     quasi-Newton method optimizer and \code{"em"} for the
#'                     Expectation Maximization (EM) algorithm.
#' @param missing      a character string indicating how to deal with missing data,
#'                     i.e., \code{"listwise"} (default) for listwise deletion or
#'                     \code{"fiml"} for full information maximum likelihood (FIML)
#'                     method. Note that FIML method is only available when
#'                     \code{estimator = "ML"}, that it takes longer to estimate
#'                     the model  using FIML, and that FIML is prone to convergence
#'                     issues which might be resolved by switching to listwise deletion.
#' @param print        a character string or character vector indicating which
#'                     results to show on the console, i.e. \code{"all"} for all
#'                     results, \code{"summary"} for a summary of the specification
#'                     of the estimation method and missing data handling in lavaan,
#'                     \code{"coverage"} for the variance-covariance coverage of
#'                     the data, \code{"descript"} for descriptive statistics,
#'                     \code{"fit"} for model fit and  model comparison, \code{"est"}
#'                     for parameter estimates, and \code{"modind"} for modification
#'                     indices. By default, a summary of the specification and model fit
#'                     and model comparison are printed.
#' @param print.fit    a character string or character vector indicating which
#'                     version of the CFI, TLI, and RMSEA to show on the console,
#'                     i.e., \code{"all"} for all versions of the CFI, TLI, and
#'                     RMSEA, \code{"standard"} (default when \code{estimator = "ML"})
#'                     for fit indices without any non-normality correction,
#'                     \code{"scaled"} for population-corrected robust fit indices
#'                     with ad hoc non-normality correction, and \code{robust}
#'                     (default when \code{estimator = "MLR"}) for sample-corrected
#'                     robust fit indices based on formula provided by Li and Bentler
#'                     (2006) and Brosseau-Liard and Savalei (2014).
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
#'                     to be used for displaying results. Note that loglikelihood,
#'                     information criteria and chi-square test statistic are
#'                     printed with \code{digits} minus 1 decimal places.
#' @param p.digits     an integer value indicating the number of decimal places
#'                     to be used for displaying \emph{p}-values, covariance
#'                     coverage (i.e., \code{p.digits - 1}), and residual
#'                     correlation coefficients.
#' @param as.na        a numeric vector indicating user-defined missing values,
#'                     i.e. these values are converted to \code{NA} before conducting
#'                     the analysis. Note that \code{as.na()} function is only
#'                     applied to \code{data} but not to \code{cluster}.
#' @param write        a character string naming a file for writing the output into
#'                     either a text file with file extension \code{".txt"} (e.g.,
#'                     \code{"Output.txt"}) or Excel file with file extension
#'                     \code{".xlsx"}  (e.g., \code{"Output.xlsx"}). If the file
#'                     name does not contain any file extension, an Excel file will
#'                     be written.
#' @param append       logical: if \code{TRUE} (default), output will be appended
#'                     to an existing text file with extension \code{.txt} specified
#'                     in \code{write}, if \code{FALSE} existing text file will be
#'                     overwritten.
#' @param check        logical: if \code{TRUE} (default), argument specification,
#'                     convergence and model identification is checked.
#' @param output       logical: if \code{TRUE} (default), output is shown.
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
#' #----------------------------------------------------------------------------
#' # Cluster variable specification
#'
#' # Example 1a: Specification using the argument '...'
#' multilevel.invar(Demo.twolevel, y1:y4, cluster = "cluster")
#'
#' # Example 1b: Alternative specification with cluster variable 'cluster' in 'data'
#' multilevel.invar(Demo.twolevel[, c("y1", "y2", "y3", "y4", "cluster")], cluster = "cluster")
#'
#' # Example 1b: Alternative specification with cluster variable 'cluster' not in 'data'
#' multilevel.invar(Demo.twolevel[, c("y1", "y2", "y3", "y4")], cluster = Demo.twolevel$cluster)
#'
#' #----------------------------------------------------------------------------
#' # Model specification using 'data' for a one-factor model
#'
#' #..........
#' # Level of measurement invariance
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
#' #..........
#' # Residual covariance at the Within level and residual variance at the Between level
#'
#' # Example 3a: Residual covariance between "y3" and "y4" at the Within level
#' multilevel.invar(Demo.twolevel, y1, y2, y3, y4, cluster = "cluster",
#'                  rescov = c("y3", "y4"))
#'
#' # Example 3b: Residual variances of 'y1' at the Between level fixed at 0
#' multilevel.invar(Demo.twolevel, y1, y2, y3, y4, cluster = "cluster", fix.resid = "y1")
#'
#' #..........
#' # Example 4: Print all results
#' multilevel.invar(Demo.twolevel, y1, y2, y3, y4, cluster = "cluster", print = "all")
#'
#' #..........
#' # Example 5: lavaan model and summary of the estimated model
#' mod <- multilevel.invar(Demo.twolevel, y1, y2, y3, y4, cluster = "cluster", output = FALSE)
#'
#' # lavaan syntax of the metric invariance model
#' mod$model$metric
#'
#' # Fitted lavaan object of the metric invariance model
#' lavaan::summary(mod$model.fit$metric, standardized = TRUE, fit.measures = TRUE)
#'
#' #----------------------------------------------------------------------------
#' # Model specification using 'model' for one or multiple factor model
#'
#' # Example 6a: One-factor model
#' multilevel.invar(Demo.twolevel, cluster = "cluster", model = c("y1", "y2", "y3", "y4"))
#'
#' # Example 6b:  Two-factor model
#' multilevel.invar(Demo.twolevel, cluster = "cluster",
#'                  model = list(c("y1", "y2", "y3"), c("y4", "y5", "y6")))
#'
#' #----------------------------------------------------------------------------
#' # Write results
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
                             print = c("all", "summary", "coverage", "descript", "fit", "est", "modind", "resid"),
                             print.fit = c("all", "standard", "scaled", "robust"), mod.minval = 6.63,
                             resid.minval = 0.1, digits = 3, p.digits = 3, as.na = NULL, write = NULL,
                             append = TRUE, check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'data' is missing
  if (isTRUE(missing(data))) { stop("Please specify a data frame for the argument 'data'", call. = FALSE) }

  # Check if input 'data' is NULL
  if (isTRUE(is.null(data))) { stop("Input specified for the argument 'data' is NULL.", call. = FALSE) }

  # Check input 'cluster'
  if (isTRUE(missing(cluster))) { stop("Please specify a variable name or vector representing the grouping structure for the argument 'cluster'.", call. = FALSE) }

  # Check if input 'cluster' is NULL
  if (isTRUE(is.null(cluster))) { stop("Input specified for the argument 'cluster' is NULL.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data using the argument 'data' ####

  if (isTRUE(!missing(...))) {

    # Extract data
    x <- as.data.frame(data[, .var.names(..., data = data, cluster = cluster), drop = FALSE])

    # Cluster variable
    cluster <- data[, cluster]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data without using the argument 'data' ####

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

  # Check inputs
  .check.input(logical = c("append", "output"),
               numeric = list(mod.minval = 1L, resid.minval = 1L),
               s.character = list(invar = c("config", "metric", "scalar"), ident = c("marker", "var", "effect"), estimator = c("ML", "MLR"), optim.method = c("nlminb", "em"), missing = c("listwise", "fiml")),
               m.character = list(print = c("all", "summary", "coverage", "descript", "fit", "est", "modind", "resid"), print.fit = c("all", "standard", "scaled", "robust")),
               args = c("digits", "p.digits", "write2"), package = "lavaan", envir = environment(), input.check = check)

  # Additional checks
  if (isTRUE(check)) {

    # Check if input 'model' is a character vector or list of character vectors
    if (isTRUE(!is.null(model) && !all(sapply(model, is.character)))) { stop("Please specify a character vector or list of character vectors for the argument 'model'.", call. = FALSE) }

    # Model specification with 'model'
    if (isTRUE(!is.null(model))) { (!unique(unlist(model)) %in% colnames(x)) |> (\(y) if (isTRUE(any(y))) { stop(paste0("Variables specified in the argument 'model' were not found in 'data': ", paste(unique(unlist(model))[y], collapse = ", ")), call. = FALSE) })() }

    # Check input 'rescov'
    if (isTRUE(!is.null(rescov))) {

      # Two variables for each residual covariance
      if (isTRUE(is.list(rescov) && any(sapply(rescov, length) != 2L))) { stop("Please specify a list of character vectors for the argument 'rescov', where each element has two variable names", call. = FALSE)

      } else { if (isTRUE(length(rescov) != 2L)) { stop("Please specify a character vector with two variable names for the argument 'rescov'", call. = FALSE) } }

      # Variable in 'data'
      (!unique(unlist(rescov)) %in% colnames(x)) |> (\(y) if (isTRUE(any(y))) { stop(paste0("Variables specified in the argument 'rescov' were not found in 'data': ", paste(unique(unlist(rescov))[y], collapse = ", ")), call. = FALSE) })()

    }

    # Check input 'fix.resid'
    (!unique(fix.resid) %in% colnames(x)) |> (\(y) if (isTRUE(any(y) &&  all(fix.resid != "all"))) { stop(paste0("Variables specified in the argument 'fix.resid' were not found in 'data': ", paste(fix.resid[y], collapse = ", ")), call. = FALSE) })()

    # Check input 'mod.minval'
    if (isTRUE(mod.minval <= 0L)) { stop("Please specify a value greater than 0 for the argument 'mod.minval'.", call. = FALSE) }

    ## Check input 'resid.minval'
    if (isTRUE(resid.minval < 0L)) { stop("Please specify a value greater than or equal 0 for the argument 'resid.minval'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Data and Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Manifest variables ####

  #...................
  ### Model specification with 'data' ####
  if (isTRUE(is.null(model))) {

    var <- colnames(x)

  #...................
  ### Model specification with 'model' ####
  } else if (isTRUE(!is.null(model))) {

    var <- unique(unlist(model))

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data frame with Cluster Variable ####

  x <- data.frame(x, .cluster = cluster, row.names = NULL)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Missing Data on the Cluster Variable ####

  if (isTRUE(any(is.na(x$.cluster)))) {

    warning(paste0("Data contains missing values on the cluster variable, number of cases removed from the analysis: ", sum(is.na(x$.cluster))), call. = FALSE)

    x <- x[!is.na(x$.cluster), ]

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) { x[, var] <- .as.na(x[, var], na = as.na) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Factor labels ####

  #...................
  ### Model specification with 'model' ####
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

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Level of invariance ####

  if (isTRUE(all(c("config", "metric", "scalar") %in% invar))) {

    invar <- "metric"

  } else if (isTRUE(length(invar) != 1)) {

    stop("Please specify a character string for the argument 'invar'.", call. = FALSE)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Residual variances fixed at 0 ####

  if (isTRUE(fix.resid == "all")) { fix.resid <- var }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model identification ####

  if (isTRUE(all(c("marker", "var", "effect") %in% ident))) {

    ident <- "var"

  } else if (isTRUE(length(ident) != 1)) {

    stop("Please specify a character string for the argument 'ident'.", call. = FALSE)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Estimator ####

  if (isTRUE(all(c("ML", "MLR") %in% estimator))) { estimator <- "MLR" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Optimizer ####

  if (isTRUE(all(c("nlminb", "em") %in% optim.method))) { optim.method <- "nlminb" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Missing ####

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

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Missing Data on All Variable ####

  (misty::na.prop(x[, var], append = FALSE) == 1L) |> (\(y) if (isTRUE(any(y) && missing == "fiml")) { warning(paste0("Data contains cases with missing values on all variables, number of cases removed from the analysis: ", sum(y)), call. = FALSE) })()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Print ####

  if (isTRUE(all(c("all", "summary", "coverage", "descript", "fit", "est", "modind", "resid") %in% print))) {

    print  <- c("summary", "fit")

  } else if (isTRUE(length(print) == 1L && "all" %in% print)) {

    print <- c("all", "summary", "coverage", "descript", "fit", "est", "modind", "resid")

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Print fit ####

  if (isTRUE(all(c("all", "standard", "scaled", "robust") %in% print.fit))) {

    print.fit <- ifelse(isTRUE(estimator == "ML"), "standard", "robust")

  } else if (isTRUE(length(print.fit) == 1L && "all" %in% print.fit)) {

    if (isTRUE(estimator == "ML")) { print.fit <- "standard" } else { print.fit <- c("standard", "scaled", "robust") }

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model estimation ####

  #...................
  ### Model specification with 'data' ####
  if (isTRUE(is.null(model))) {

    model.fit.metric <- model.fit.scalar <- warn.config <- warn.metric <- warn.scalar <- NULL

    model.fit.config <- withCallingHandlers(misty::multilevel.cfa(x, cluster = ".cluster", model = NULL, rescov = NULL,
                                                                  model.w = var, model.b = var, rescov.w = rescov, rescov.b = NULL,
                                                                  const = "config", fix.resid = fix.resid, ident = ident, ls.fit = FALSE,
                                                                  estimator = estimator, optim.method = optim.method,
                                                                  missing = missing, output = FALSE),
                                            warning = function(y) { warn.config <<- c(warn.config, conditionMessage(y)); invokeRestart("muffleWarning") })

    # Metric measurement invariance
    if (isTRUE(any(c("metric", "scalar") %in% invar))) {

      model.fit.scalar <- warn.metric <- warn.scalar <- NULL

      model.fit.metric <- withCallingHandlers(misty::multilevel.cfa(x, cluster = ".cluster", model = var, rescov = rescov,
                                                                    model.w = NULL, model.b = NULL, rescov.w = NULL, rescov.b = NULL,
                                                                    fix.resid = fix.resid, ident = ident, ls.fit = FALSE,
                                                                    estimator = estimator, optim.method = optim.method,
                                                                    missing = missing, output = FALSE),
                                              warning = function(y) { warn.metric <<- c(warn.metric, conditionMessage(y)); invokeRestart("muffleWarning") })

    }

    # Scalar measurement invariance
    if (isTRUE(any("scalar" %in% invar))) {

      warn.scalar <- NULL

      model.fit.scalar <- withCallingHandlers(misty::multilevel.cfa(x, cluster = ".cluster", model = var, rescov = rescov,
                                                                    model.w = NULL, model.b = NULL, rescov.w = NULL, rescov.b = NULL,
                                                                    const = "config", fix.resid = "all",
                                                                    ident = ident, ls.fit = FALSE, estimator = estimator, optim.method = optim.method,
                                                                    missing = missing, output = FALSE),
                                              warning = function(y) { warn.scalar <<- c(warn.scalar, conditionMessage(y)); invokeRestart("muffleWarning") })

    }

    for(i in unique(c(warn.config, warn.metric, warn.scalar))) { warning(i, call. = FALSE) }

  #...................
  ### Model specification with 'model' ####
  } else {

    # Configural measurement invariance
    model.fit.metric <- model.fit.scalar <- warn.config <- warn.metric <- warn.scalar <- NULL

    model.fit.config <- withCallingHandlers(misty::multilevel.cfa(x, cluster = ".cluster", model = NULL, rescov = NULL,
                                                                  model.w = model, model.b = model,  rescov.w = rescov, rescov.b = NULL,
                                                                  fix.resid = fix.resid, ident = ident, ls.fit = FALSE,
                                                                  estimator = estimator, optim.method = optim.method,
                                                                  missing = missing, output = FALSE),
                                            warning = function(y) { warn.config <<- c(warn.config, conditionMessage(y)); invokeRestart("muffleWarning") })

    # Metric measurement invariance
    if (isTRUE(any(c("metric", "scalar") %in% invar))) {

      model.fit.scalar <- warn.metric <- warn.scalar <- NULL

      model.fit.metric <- withCallingHandlers(misty::multilevel.cfa(x, cluster = ".cluster", model = model, rescov = rescov,
                                                                    model.w = NULL, model.b = NULL, rescov.w = NULL, rescov.b = NULL,
                                                                    const = "config", fix.resid = fix.resid, ident = ident, ls.fit = FALSE,
                                                                    estimator = estimator, optim.method = optim.method,
                                                                    missing = missing, output = FALSE),
                                              warning = function(y) { warn.metric <<- c(warn.metric, conditionMessage(y)); invokeRestart("muffleWarning") })

    }

    # Scalar measurement invariance
    if (isTRUE(any("scalar" %in% invar))) {

      warn.scalar <- NULL

      model.fit.scalar <- withCallingHandlers(misty::multilevel.cfa(x, cluster = ".cluster", model = model, rescov = rescov,
                                                                    model.w = NULL, model.b = NULL, rescov.w = NULL, rescov.b = NULL,
                                                                    const = "config", fix.resid = "all",
                                                                    ident = ident, ls.fit = FALSE, estimator = estimator, optim.method = optim.method,
                                                                    missing = missing, output = FALSE),
                                              warning = function(y) { warn.scalar <<- c(warn.scalar, conditionMessage(y)); invokeRestart("muffleWarning") })

    }

  }

  #_____________________________________________________________________________
  #
  # Return object --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## lavaan summary ####

  switch(invar,
         #...................
         ### Configural measurement invariance ####
         config = {

           lavaan.summary <- rbind(model.fit.config$result$summary[1L:10L, ], c("", "Config", ""), model.fit.config$result$summary[11L:19L, ])

         #...................
         ### Metric measurement invariance ####
         }, metric = {

           lavaan.summary <- rbind(model.fit.config$result$summary[1L:10L, ],
                                   c("", "Config", "Metric"),
                                   data.frame(model.fit.config$result$summary[11L:14L, 1L], model.fit.config$result$summary[11L:14L, 2L], model.fit.metric$result$summary[11L:14L, 2L], fix.empty.names = FALSE),
                                   c("", "", ""),
                                   model.fit.config$result$summary[16L:19L, ])

         #...................
         ### Scalar measurement invariance ####
         }, scalar = {

           lavaan.summary <- rbind(data.frame(model.fit.config$result$summary[1L:10L, ], "", fix.empty.names = FALSE),
                                   c("", "Config", "Metric", "Scalar"),
                                   data.frame(model.fit.config$result$summary[11L:14L, 1L], model.fit.config$result$summary[11L:14L, 2L], model.fit.metric$result$summary[11L:14L, 2L],  model.fit.scalar$result$summary[11L:14L, 2L], fix.empty.names = FALSE),
                                   c("", "", "", ""),
                                   data.frame(model.fit.config$result$summary[16L:19L, ], "", fix.empty.names = FALSE))

         })

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model fit ####

  fit.config.scaled <- fit.config.robust <- fit.metric.scaled <- fit.metric.robust <- fit.scalar.scaled <- fit.scalar.robust <- NULL

  # Extract model fit information
  fit.config <- model.fit.config$result$fit

  #...................
  ### Fit indices for config measurement invariance ####
  switch(estimator,
         #### Maximum Likelihood
         ML = {

           # Standard fit indices
           fit.config.stand <- fit.config[c(10L:27L, 4L:8L), ]

         #### Robust maximum Likelihood
         }, MLR = {

           # Standard fit indices
           fit.config.stand <- fit.config[c(12L:15, 17:30L, 6L:10L), c(1L, 2L)]

           # Scaled fit indices
           fit.config.scaled <- fit.config[c(12L:30L, 6L:10L), c(1L, 3L)]

           fit.config.scaled[c(16L:24L), 2L] <- fit.config[c(27L:30L, 6L:10L), 2L]

           # Robust
           fit.config.robust <- fit.config[c(12L:30L, 6L:10L), c(1L, 3L)]

           fit.config.robust[c(17L:19L, 22L:24L), 2L] <- fit.config[c(28L:30L, 8L:10L), 2L]
           fit.config.robust[c(8L:9L, 12L:15L), 2L] <- fit.config[c(19L:20L, 23L:26L), 4L]

         })

  #...................
  ### Fit indices for metric measurement invariance ####
  if (isTRUE(any(c("metric", "scalar") %in% invar))) {

    # Extract model fit information
    fit.metric <- model.fit.metric$result$fit

    # Fit indices for metric measurement invariance
    switch(estimator,
           #### Maximum Likelihood
           ML = {

             # Standard fit indices
             fit.metric.stand <- fit.metric[c(10L:27, 4L:8L), ]

          #### Robust maximum Likelihood
           }, MLR = {

             # Standard fit indices
             fit.metric.stand <- fit.metric[c(12L:15, 17:30L, 6L:10L), c(1L, 2L)]

             # Scaled fit indices
             fit.metric.scaled <- fit.metric[c(12L:30, 6L:10L), c(1L, 3L)]

             fit.metric.scaled[c(16L:24), 2L] <- fit.metric[c(27L:30L, 6L:10L), 2L]

             # Robust
             fit.metric.robust <- fit.metric[c(12L:30, 6L:10L), c(1L, 3L)]

             fit.metric.robust[c(8L:9L, 12L:15L), ] <- fit.metric[c(19L:20L, 23L:26L), 4L]

             fit.metric.robust[c(16L:24), 2L] <- fit.metric[c(27L:30L, 6L:10L), 2L]

           })

  }

  #...................
  ### Fit indices for scalar measurement invariance ####
  if (isTRUE("scalar" %in% invar)) {

    # Extract model fit information
    fit.scalar <- model.fit.scalar$result$fit

    # Fit indices for metric measurement invariance
    switch(estimator,
           #### Maximum Likelihood
           ML = {

             # Standard fit indices
             fit.scalar.stand <- fit.scalar[c(10L:15, 17:27, 4L:8L), ]

           #### Robust maximum Likelihood
           }, MLR = {

             # Standard fit indices
             fit.scalar.stand <- fit.scalar[c(12L:15, 17:30, 6L:10L), c(1L, 2L)]

             # Scaled fit indices
             fit.scalar.scaled <- fit.scalar[c(12L:30, 6L:10L), c(1L, 3L)]

             fit.scalar.scaled[c(16L:24), 2L] <- fit.scalar[c(27L:30L, 6L:10L), 2L]

             # Robust
             fit.scalar.robust <- fit.scalar[c(12L:30, 6L:10L), c(1L, 3L)]

             fit.scalar.robust[c(8L:9L, 12L:15L), 2L] <- fit.scalar[c(19L:20L, 23L:26L), 4L]
             fit.scalar.robust[c(16L:24), 2L] <- fit.scalar[c(27L:30L, 6L:10L), 2L]

           })

  }

  #...................
  ### Combine fit indices ####

  #### Configural measurement invariance
  switch(invar, config = {

    ##### Standard fit indices

    fit.stand <- fit.config.stand

    fit.scaled <- fit.robust <- NULL

    ##### Robust maximum likelihood
    if (isTRUE(estimator == "MLR")) {

      ###### Scaled fit indices

      fit.scaled <- data.frame(fit.config.scaled[, 1L], config = fit.config.scaled[, 2L], fix.empty.names = FALSE)

      ###### Robust fit indices

      fit.robust <- data.frame(fit.config.robust[, 1L], config = fit.config.robust[, 2L], fix.empty.names = FALSE)

      ###### Scaling correction factor

      scale.corr <- is.na(fit.scaled[5L, "config", drop = FALSE])
      if (isTRUE(scale.corr)) { warning("Scaling correction factor could not be computed for following model(s): Configural", call. = FALSE) }

    }

    colnames(fit.stand) <- c("", "config")
    if (isTRUE(!is.null(fit.scaled))) { colnames(fit.scaled) <- c("", "config") }
    if (isTRUE(!is.null(fit.scaled))) { colnames(fit.robust) <- c("", "config") }

    #### Configural and metric measurement invariance
    }, metric = {

    ##### Chi-squared difference test, config vs. metric
    chidiff.confmet <- tryCatch(lavaan::lavTestLRT(model.fit.config$model.fit, model.fit.metric$model.fit),
                                error = function(y) { warning("test"); data.frame(matrix(NA, ncol = 7L, dimnames = list(NULL, c("Df", "AIC", "BIC", "Chisq", "Chisq diff", "Df diff", "Pr(>Chisq)"))), check.names = FALSE) },
                                warning = function(z) { suppressWarnings(lavaan::lavTestLRT(model.fit.config$model.fit, model.fit.metric$model.fit, method = "satorra.bentler.2010")) })

    if (isTRUE(all(is.na(chidiff.confmet[, "Pr(>Chisq)"])))) { warning("Chi-square difference test Configural vs. Metric could not be computed.", call. = FALSE) }

    # Negative chi-squared value even though model fit decreased
    if (isTRUE(chidiff.confmet[2L, "Chisq"] - chidiff.confmet[1L, "Chisq"] > 0L && chidiff.confmet[2L, "Chisq diff"] < 0L)) { chidiff.confmet <- data.frame(matrix(NA, ncol = 7L, dimnames = list(NULL, c("Df", "AIC", "BIC", "Chisq", "Chisq diff", "Df diff", "Pr(>Chisq)"))), check.names = FALSE) }

    ##### Standard fit indices

    # Combine fit indices for configural and metric measurement invariance
    fit.stand <- data.frame(fit.config.stand[, 1L], config = fit.config.stand[, 2L], metric = fit.metric.stand[, 2L],
                            dmetric = fit.metric.stand[, 2L] - fit.config.stand[, 2L], fix.empty.names = FALSE)

    # Chi-squared difference test, config vs. metric
    fit.stand[2L:4L, "dmetric"] <- unlist(chidiff.confmet[2L, c("Chisq diff", "Df diff", "Pr(>Chisq)")])

    fit.stand[fit.stand[, 1L] %in% c("Scaling Correction Factor", "90 Percent CI - lower", "90 Percent CI - upper", "P-value RMSEA <= 0.05"), "dmetric"] <- NA

    fit.scaled <- fit.robust <- NULL

    ##### Robust maximum likelihood
    if (isTRUE(estimator == "MLR")) {

      ###### Scaled fit indices

      # Combine fit indices for configural and metric measurement invariance
      fit.scaled <- data.frame(fit.config.scaled[, 1L], config = fit.config.scaled[, 2L], metric = fit.metric.scaled[, 2L],
                               dmetric = fit.metric.scaled[, 2L] - fit.config.scaled[, 2L], fix.empty.names = FALSE)

      fit.scaled[c(13L:15L), "dmetric"] <- NA

      # Chi-squared difference test, config vs. metric
      fit.scaled[2L:4L, "dmetric"] <- unlist(chidiff.confmet[2L, c("Chisq diff", "Df diff", "Pr(>Chisq)")])

      # Set difference in scaling correction factor to 0
      fit.scaled[5L, "dmetric"] <- NA

      ###### Robust fit indices

      # Combine fit indices for configural and metric measurement invariance
      fit.robust <- data.frame(fit.config.robust[, 1L], config = fit.config.robust[, 2L], metric = fit.metric.robust[, 2L],
                               dmetric = fit.metric.robust[, 2L] - fit.config.robust[, 2L], fix.empty.names = FALSE)

      fit.robust[c(13L:15L), "dmetric"] <- NA

      # Chi-squared difference test, config vs. metric
      fit.robust[2L:4L, "dmetric"] <- unlist(chidiff.confmet[2L, c("Chisq diff", "Df diff", "Pr(>Chisq)")])

      # Set difference in scaling correction factor to 0
      fit.robust[5L, "dmetric"] <- NA

      ###### Scaling correction factor

      scale.corr <- is.na(fit.scaled[5L, which(colnames(fit.scaled) %in% c("config", "metric"))])
      if (isTRUE(any(scale.corr))) {

        warning(paste0("Scaling correction factor could not be computed for following model(s): ",
                       paste(c("Configural", "Metric")[match(colnames(scale.corr)[which(scale.corr)], c("config", "metric"))], collapse = ", ")), call. = FALSE)

      }

    }

  #### Configural, metric and scalar measurement invariance
  }, scalar = {

    ##### Chi-squared difference test, config vs. metric
    chidiff.confmet <- tryCatch(lavaan::lavTestLRT(model.fit.config$model.fit, model.fit.metric$model.fit),
                                error = function(y) { data.frame(matrix(NA, ncol = 7L, dimnames = list(NULL, c("Df", "AIC", "BIC", "Chisq", "Chisq diff", "Df diff", "Pr(>Chisq)"))), check.names = FALSE) },
                                warning = function(z) { suppressWarnings(lavaan::lavTestLRT(model.fit.config$model.fit, model.fit.metric$model.fit, method = "satorra.bentler.2010")) })

    if (isTRUE(all(is.na(chidiff.confmet[, "Pr(>Chisq)"])))) { warning("Chi-square difference test Configural vs. Metric could not be computed.", call. = FALSE) }

    ##### Chi-squared difference test, metric vs. scalar
    chidiff.metsca <- tryCatch(lavaan::lavTestLRT(model.fit.metric$model.fit, model.fit.scalar$model.fit),
                               error = function(y) { data.frame(matrix(NA, ncol = 7L, dimnames = list(NULL, c("Df", "AIC", "BIC", "Chisq", "Chisq diff", "Df diff", "Pr(>Chisq)"))), check.names = FALSE) },
                               warning = function(z) { suppressWarnings(lavaan::lavTestLRT(model.fit.metric$model.fit, model.fit.scalar$model.fit, method = "satorra.bentler.2010")) })

    if (isTRUE(all(is.na(chidiff.metsca[, "Pr(>Chisq)"])))) { warning("Chi-square difference test Metric vs. Scalar could not be computed.", call. = FALSE) }

    # Negative chi-squared value even though model fit decreased
    if (isTRUE(chidiff.confmet[2L, "Chisq"] - chidiff.confmet[1L, "Chisq"] > 0L && chidiff.confmet[2L, "Chisq diff"] < 0L)) { chidiff.confmet <- data.frame(matrix(NA, ncol = 7L, dimnames = list(NULL, c("Df", "AIC", "BIC", "Chisq", "Chisq diff", "Df diff", "Pr(>Chisq)"))), check.names = FALSE) }
    if (isTRUE(chidiff.metsca[2L, "Chisq"] - chidiff.metsca[1L, "Chisq"] > 0L && chidiff.metsca[2L, "Chisq diff"] < 0L)) { chidiff.metsca <- data.frame(matrix(NA, ncol = 7L, dimnames = list(NULL, c("Df", "AIC", "BIC", "Chisq", "Chisq diff", "Df diff", "Pr(>Chisq)"))), check.names = FALSE) }

    ##### Standard fit indices

    # Combine fit indices for configural and metric measurement invariance
    fit.stand <- data.frame(fit.config.stand[, 1L], config = fit.config.stand[, 2L], metric = fit.metric.stand[, 2L], scalar = fit.scalar.stand[, 2L],
                            dmetric = fit.metric.stand[, 2L] - fit.config.stand[, 2L], dscalar = fit.scalar.stand[, 2L] - fit.metric.stand[, 2L], fix.empty.names = FALSE)

    # Chi-squared difference test, config vs. metric
    fit.stand[2L:4L, "dmetric"] <- unlist(chidiff.confmet[2L, c("Chisq diff", "Df diff", "Pr(>Chisq)")])
    # Chi-squared difference test, metric vs. scalar
    fit.stand[2L:4L, "dscalar"] <- unlist(chidiff.metsca[2L, c("Chisq diff", "Df diff", "Pr(>Chisq)")])

    fit.stand[fit.stand[, 1L] %in% c("Scaling Correction Factor", "90 Percent CI - lower", "90 Percent CI - upper", "P-value RMSEA <= 0.05"), c("dmetric", "dscalar")] <- NA

    fit.scaled <- fit.robust <- NULL

    ##### Robust maximum likelihood
    if (isTRUE(estimator == "MLR")) {

      ###### Scaled fit indices

      # Combine fit indices for configural and metric measurement invariance
      fit.scaled <- data.frame(fit.config.scaled[, 1L], config = fit.config.scaled[, 2L], metric = fit.metric.scaled[, 2L], scalar = fit.scalar.scaled[, 2L],
                               dmetric = fit.metric.scaled[, 2L] - fit.config.scaled[, 2L], dscalar = fit.scalar.scaled[, 2L] - fit.metric.scaled[, 2L], fix.empty.names = FALSE)

      fit.scaled[c(5L, 13L:15L), c("dmetric", "dscalar")] <- NA

      # Chi-squared difference test, config vs. metric
      fit.scaled[2L:4L, "dmetric"] <- unlist(chidiff.confmet[2L, c("Chisq diff", "Df diff", "Pr(>Chisq)")])
      # Chi-squared difference test, metric vs. scalar
      fit.scaled[2L:4L, "dscalar"] <- unlist(chidiff.metsca[2L, c("Chisq diff", "Df diff", "Pr(>Chisq)")])

      ###### Robust fit indices

      # Combine fit indices for configural and metric measurement invariance
      fit.robust <- data.frame(fit.config.robust[, 1L], config = fit.config.robust[, 2L], metric = fit.metric.robust[, 2L], scalar = fit.scalar.robust[, 2L],
                               dmetric = fit.metric.robust[, 2L] - fit.config.robust[, 2L], dscalar = fit.scalar.robust[, 2L] - fit.metric.robust[, 2L], fix.empty.names = FALSE)

      fit.robust[c(5L, 13L:15L), c("dmetric", "dscalar")] <- NA

      # Chi-squared difference test, config vs. metric
      fit.robust[2L:4L, "dmetric"] <- unlist(chidiff.confmet[2L, c("Chisq diff", "Df diff", "Pr(>Chisq)")])
      # Chi-squared difference test, metric vs. scalar
      fit.robust[2L:4L, "dscalar"] <- unlist(chidiff.metsca[2L, c("Chisq diff", "Df diff", "Pr(>Chisq)")])


      ###### Scaling correction factor

      scale.corr <- is.na(fit.scaled[5L, which(colnames(fit.scaled) %in% c("config", "metric", "scalar"))])
      if (isTRUE(any(scale.corr))) {

        warning(paste0("Scaling correction factor could not be computed for following model(s): ",
                       paste(c("Configural", "Metric", "Scalar")[match(colnames(scale.corr)[which(scale.corr)], c("config", "metric", "scalar"))], collapse = ", ")), call. = FALSE)

      }

    }

  })

  row.names(fit.stand) <- seq_len(nrow(fit.stand))
  if (isTRUE(!is.null(fit.scaled))) { row.names(fit.scaled) <- seq_len(nrow(fit.scaled)) }
  if (isTRUE(!is.null(fit.robust))) { row.names(fit.robust) <- seq_len(nrow(fit.robust)) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Return object ####

  object <- list(call = match.call(),
                 type = "multilevel.invar",
                 data = x,
                 args = list(model = model, rescov = rescov,
                             invar = invar, ident = ident,
                             estimator = estimator, optim.method = optim.method,
                             missing = missing, print = print, print.fit = print.fit,
                             mod.minval = mod.minval, resid.minval = resid.minval,
                             digits = digits, p.digits = p.digits, write = write, append = append,
                             as.na = as.na, check = check, output = output),
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
                               resid = list(config = model.fit.config$result$resid, metric = model.fit.metric$result$resid, scalar = model.fit.scalar$result$resid)))

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
