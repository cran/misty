#' Cross-Level Measurement Invariance Evaluation
#'
#' This function is a wrapper function for evaluating configural, metric, and
#' scalar cross-level measurement invariance using multilevel confirmatory factor
#' analysis with continuous indicators by calling the \code{cfa} function in the
#' R package \pkg{lavaan}.
#'
#' @param x            a matrix or data frame. If \code{model} is \code{NULL},
#'                     multilevel confirmatory factor analysis based on a
#'                     measurement model with one factor at the Within and Between
#'                     level comprising all variables in the matrix or data frame
#'                     is conducted to evaluate cross-level measurement invariance.
#'                     Note that the cluster variable specified in \code{cluster}
#'                     is excluded from \code{x} when specifying the argument
#'                     \code{cluster} using the variable name of the cluster
#'                     variable. If \code{model} is specified, the matrix or data
#'                     frame needs to contain all variables used in the \code{model}
#'                     argument.
#' @param cluster      either a character string indicating the variable name of
#'                     the cluster variable in 'x' or a vector representing the
#'                     nested grouping structure (i.e., group or cluster variable).
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
#'                     added the labels to distingish factor labels at the Within
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
#'                     metric and scalar measurementinvariance (i.e., all residual
#'                     variances at the Between level equal zero).
#' @param fix.resid    a charcter vector for specifying residual variances to be
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
#' @param optim.method a chracter string indicating the optimizer, i.e., \code{"nlminb"}
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
#' @param min.value    numeric value to filter modification indices and only show
#'                     modifications with a modification index value equal or higher
#'                     than this minimum value. By default, modification indices
#'                     equal or higher 10 is printed.
#' @param digits       an integer value indicating the number of decimal places
#'                     to be used for displaying results. Note that information
#'                     criteria and chi-square test statistic is printed with
#'                     \code{digits} minus 1 decimal places.
#' @param p.digits     an integer value indicating the number of decimal places
#'                     to be used for displaying the \emph{p}-value.
#' @param as.na        a numeric vector indicating user-defined missing values,
#'                     i.e. these values are converted to \code{NA} before conducting
#'                     the analysis. Note that \code{as.na()} function is only
#'                     applied to \code{x} but not to \code{cluster}.
#' @param write        a character string for writing the results into a Excel
#'                     file naming a file with or without file extension '.xlsx',
#'                     e.g., \code{"Results.xlsx"} or \code{"Results"}.
#' @param check        logical: if \code{TRUE}, argument specification, convergence
#'                     and model identification is checked.
#' @param output       logical: if \code{TRUE}, output is shown.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{multilevel.cfa}}, \code{\link{multilevel.fit}}, \code{\link{multilevel.omega}},
#' \code{\link{multilevel.cor}}, \code{\link{multilevel.descript}}
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
#' \item{\code{data}}{matrix or data frame specified in \code{x}}
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
#'                      the configural, metric, and scalar invariance model}
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
#' #---------------------------
#' # Cluster variable specification
#'
#' # Cluster variable 'cluster' in 'x'
#' multilevel.invar(Demo.twolevel[,c("y1", "y2", "y3", "y4", "cluster")],
#'                  cluster = "cluster")
#'
#' # Cluster variable 'cluster' not in 'x'
#' multilevel.invar(Demo.twolevel[,c("y1", "y2", "y3", "y4")],
#'                  cluster = Demo.twolevel$cluster)
#'
#' #---------------------------
#' # Model specification using 'x' for a one-factor model
#'
#' #..........
#' # Level of measurement invariance
#'
#' # Configural invariance
#' multilevel.invar(Demo.twolevel[,c("y1", "y2", "y3", "y4")],
#'                  cluster = Demo.twolevel$cluster, invar = "config")
#'
#' # Metric invariance
#' multilevel.invar(Demo.twolevel[,c("y1", "y2", "y3", "y4")],
#'                  cluster = Demo.twolevel$cluster, invar = "metric")
#'
#' # Scalar invariance
#' multilevel.invar(Demo.twolevel[,c("y1", "y2", "y3", "y4")],
#'                  cluster = Demo.twolevel$cluster, invar = "scalar")
#'
#' #..........
#' # Residual covariance at the Within level and residual variance at the Between level
#'
#' # Residual covariance between "y3" and "y4" at the Within level
#' multilevel.invar(Demo.twolevel[,c("y1", "y2", "y3", "y4")],
#'                  cluster = Demo.twolevel$cluster, rescov = c("y3", "y4"))
#'
#' # Residual variances of 'y1' at the Between level fixed at 0
#' multilevel.invar(Demo.twolevel[,c("y1", "y2", "y3", "y4")],
#'                  cluster = Demo.twolevel$cluster, fix.resid = "y1")
#'
#' #..........
#' # Print all results
#' multilevel.invar(Demo.twolevel[,c("y1", "y2", "y3", "y4")],
#'                  cluster = Demo.twolevel$cluster, print = "all")
#'
#' #..........
#' # lavaan model and summary of the estimated model
#' mod <- multilevel.invar(Demo.twolevel[,c("y1", "y2", "y3", "y4")],
#'                         cluster = Demo.twolevel$cluster, output = FALSE)
#'
#' # lavaan syntax of the metric invariance model
#' mod$model$metric
#'
#' # Fitted lavaan object of the metric invariance model
#' lavaan::summary(mod$model.fit$metric, standardized = TRUE, fit.measures = TRUE)
#'
#' #..........
#' # Write results
#'
#' # Assign results into an object and write results into an Excel file
#' mod <- multilevel.invar(Demo.twolevel[,c("y1", "y2", "y3", "y4")],
#'                         cluster = Demo.twolevel$cluster, print = "all",
#'                         output = FALSE)
#'
#' # Write results into an Excel file
#' write.result(mod, "Multilevel_Invariance.xlsx")
#'
#' # Estimate models and write results into an Excel file
#' multilevel.invar(Demo.twolevel[,c("y1", "y2", "y3", "y4")],
#'                  cluster = Demo.twolevel$cluster, print = "all",
#'                  write = "Multilevel_Invariance.xlsx")
#'
#' #---------------------------
#' # Model specification using 'model' for one or multiple factor model
#'
#' # One-factor model
#' multilevel.invar(Demo.twolevel, cluster = "cluster", model = c("y1", "y2", "y3", "y4"))
#'
#' # Two-factor model
#' multilevel.invar(Demo.twolevel, cluster = "cluster",
#'                  model = list(c("y1", "y2", "y3"), c("y4", "y5", "y6")))
#' }
multilevel.invar <- function(x, cluster, model = NULL, rescov = NULL, invar = c("config", "metric", "scalar"),
                             fix.resid = NULL, ident = c("marker", "var", "effect"), estimator = c("ML", "MLR"),
                             optim.method = c("nlminb", "em"), missing = c("listwise", "fiml"),
                             print = c("all", "summary", "coverage", "descript", "fit", "est", "modind"),
                             print.fit = c("all", "standard", "scaled", "robust"), min.value = 10,
                             digits = 3, p.digits = 3, as.na = NULL, write = NULL,
                             check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'x' is missing or Null
  if (isTRUE(missing(x) || is.null(x))) { stop("Please specify a matrix or data frame for the argument 'x'.", call. = FALSE) }

  # Check if input 'x' is a matrix or a data frame
  if (isTRUE(!is.matrix(x) && !is.data.frame(x))) { stop("Please specify a matrix or a data frame for the argument 'x'.", call. = FALSE) }

  # Check if input 'cluster' is missing or null
  if (isTRUE(missing(cluster) || is.null(cluster))) { stop("Please specify a character string or a vector for the argument 'cluster'.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # R package lavaan
    if (isTRUE(!nzchar(system.file(package = "lavaan")))) { stop("Package \"lavaan\" is needed for this function, please install the package.", call. = FALSE) }

    # Check if input 'model' is a character vector or list of character vectors
    if (isTRUE(!is.null(model) && !all(sapply(model, is.character)))) { stop("Please specify a character vector or list of character vectors for the argument 'model'.", call. = FALSE) }

    # Model specification with 'model'
    if (isTRUE(!is.null(model))) {

      var.model <- !unique(unlist(model)) %in% colnames(x)
      if (isTRUE(any(var.model))) { stop(paste0("Variables specified in the argument 'model' were not found in 'x': ", paste(unique(unlist(model))[var.model], collapse = ", ")), call. = FALSE) }

    }

    # Check input 'rescov'
    if (isTRUE(!is.null(rescov))) {

      # Two variables for each residual covariance
      if (isTRUE(is.list(rescov) && any(sapply(rescov, length) != 2L))) { stop("Please specify a list of character vectors for the argument 'rescov', where each element has two variable names", call. = FALSE)

      } else { if (isTRUE(length(rescov) != 2L)) { stop("Please specify a character vector with two variable names for the argument 'rescov'", call. = FALSE) } }

      # Variable in 'x'
      rescov.var <- !unique(unlist(rescov)) %in% colnames(x)
      if (isTRUE(any(rescov.var))) { stop(paste0("Variables specified in the argument 'rescov' were not found in 'x': ", paste(unique(unlist(rescov))[rescov.var], collapse = ", ")), call. = FALSE) }

    }

    # Cluster specified with variable name
    if (isTRUE(length(cluster) == 1L)) {

      # Character cluster variable
      if (isTRUE(!is.character(cluster))) { stop("Please specify a character string for the name of the cluster variable in 'x'", call. = FALSE) }

      # Cluster variable in 'x'
      if (isTRUE(!cluster %in% colnames(x))) { stop(paste0("Cluster variable \"", cluster, "\" specified in the argument 'cluster' was not found in 'x'"), call. = FALSE) }

    # Cluster specified with vector
    } else {

      # Length of cluster variable
      if (isTRUE(nrow(x) != length(cluster))) { stop("Cluster variable specified in the argument 'cluster' does not match with the number of rows in 'x'.", call. = FALSE) }

    }

    # Check input 'invar'
    if (isTRUE(!all(invar %in%c("config", "metric", "scalar")))) { stop("Character string in the argument 'invar' does not match with \"config\", \"metric\", or \"scalar\".", call. = FALSE) }


    # Check input 'ident'
    if (isTRUE(!all(ident %in% c("marker", "var", "effect")))) { stop("Character string in the argument 'ident' does not match with \"marker\", \"var\", or \"effect\".", call. = FALSE) }

    # Check input 'fix.resid'
    fix.resid.var <- !unique(fix.resid) %in% colnames(x)
    if (isTRUE(any(fix.resid.var) &&  all(fix.resid != "all"))) { stop(paste0("Variables specified in the argument 'fix.resid' were not found in 'x': ", paste(fix.resid[fix.resid.var], collapse = ", ")), call. = FALSE) }

    # Check input 'estimator'
    if (isTRUE(!all(estimator %in% c("ML", "MLR")))) { stop("Character string in the argument 'estimator' does not match with \"ML\" or \"MLR\".", call. = FALSE) }

    # Check input 'optim.method'
    if (isTRUE(!all(optim.method  %in% c("nlminb", "em")))) { stop("Character string in the argument 'optim.method' does not match with \"nlminb\" or \"em\".", call. = FALSE) }

    # Check input 'missing'
    if (isTRUE(!all(missing %in% c("listwise", "fiml")))) { stop("Character string in the argument 'missing' does not match with \"listwise\" or \"fiml\".", call. = FALSE) }

    # Check input 'print'
    if (isTRUE(!all(print %in% c("all", "summary", "coverage", "descript", "fit", "est", "modind")))) { stop("Character strings in the argument 'print' do not all match with \"summary\", \"coverage\", \"descript\", \"fit\", \"est\", or \"modind\".", call. = FALSE) }

    # Check input 'print.fit'
    if (isTRUE(!all(print.fit %in% c("all", "standard", "scaled", "robust")))) { stop("Character strings in the argument 'print.fit' do not all match with \"standard\", \"scaled\", or \"robust\".", call. = FALSE) }

    # Check input 'min.value'
    if (isTRUE(min.value <= 0L)) { stop("Please specify a value greater than 0 for the argument 'min.value'.", call. = FALSE) }

    # Check input 'digits'
    if (isTRUE(digits %% 1L != 0L || digits < 0L || digits == 0L)) { stop("Specify a positive integer number for the argument 'digits'.", call. = FALSE) }

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

  #...................
  ### Model specification with 'x' ####
  if (isTRUE(is.null(model))) {

    # Cluster variable in the data
    if (isTRUE(length(cluster) == 1L)) { var <- colnames(x)[!colnames(x) %in% cluster] } else { var <- colnames(x) }

  #...................
  ### Model specification with 'model' ####
  } else if (isTRUE(!is.null(model))) {

    var <- unique(unlist(model))

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data frame with Cluster Variable ####

  # Cluster variable in the data
  if (isTRUE(length(cluster) == 1L)) {

    x <- data.frame(x[, var], .cluster = x[, cluster], stringsAsFactors = FALSE)

  # Cluster variable specified in the argument 'cluster'
  } else {

    x <- data.frame(x[, var], .cluster = cluster, stringsAsFactors = FALSE)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) {

    x[, var] <- misty::as.na(x[, var], na = as.na, check = check)

    # Variable with missing values only
    x.miss <- vapply(x[, var], function(y) all(is.na(y)), FUN.VALUE = logical(1L))
    if (isTRUE(any(x.miss))) { stop(paste0("After converting user-missing values into NA, following ", ifelse(length(which(x.miss)) == 1L, "variable is ", "variables are "), "completely missing: ", paste(names(which(x.miss)), collapse = ", ")), call. = FALSE) }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Factor labels ####

  #...................
  ### Model specification with 'model' ####
  if (isTRUE(!is.null(model))) {

    # 'model' is a list
    if (is.list(model)) {

      # List elements not all named
      if (isTRUE(is.null(names(model)) || any(names(model) == ""))) { names(model) <- paste0("f", seq_along(model)) }

    # 'model' is not a list
    } else {

      model <- list(f = model)

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Level of invariance ####

  if (isTRUE(all(c("config", "metric", "scalar") %in% invar))) { invar <- "metric" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Residual variances fixed at 0 ####

  if (isTRUE(fix.resid == "all")) { fix.resid <- var }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model identification ####

  if (isTRUE(all(c("marker", "var", "effect") %in% ident))) { ident <- "var" }

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
  ## Print ####

  if (isTRUE(all(c("all", "summary", "coverage", "descript", "fit", "est", "modind") %in% print))) {

    print  <- c("summary", "fit")

  } else if (isTRUE(length(print) == 1L && "all" %in% print)) {

    print <- c("all", "summary", "coverage", "descript", "fit", "est", "modind")

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
  ###  Model specification with 'x' ####
  if (isTRUE(is.null(model))) {

    model.fit.metric <- model.fit.scalar <- warn.config <- warn.metric <- warn.scalar <- NULL

    model.fit.config <- withCallingHandlers(misty::multilevel.cfa(x = x, cluster = ".cluster", model = NULL, rescov = NULL,
                                                                  model.w = var, model.b = var, rescov.w = rescov, rescov.b = NULL,
                                                                  const = "config", fix.resid = fix.resid, ident = ident, ls.fit = FALSE,
                                                                  estimator = estimator, optim.method = optim.method,
                                                                  missing = missing, output = FALSE),
                                            warning = function(y) { warn.config <<- c(warn.config, conditionMessage(y)); invokeRestart("muffleWarning") })

    # Metric measurement invariance
    if (isTRUE(any(c("metric", "scalar") %in% invar))) {

      model.fit.scalar <- warn.metric <- warn.scalar <- NULL

      model.fit.metric <- withCallingHandlers(misty::multilevel.cfa(x = x, cluster = ".cluster", model = var, rescov = rescov,
                                                                    model.w = NULL, model.b = NULL, rescov.w = NULL, rescov.b = NULL,
                                                                    fix.resid = fix.resid, ident = ident, ls.fit = FALSE,
                                                                    estimator = estimator, optim.method = optim.method,
                                                                    missing = missing, output = FALSE),
                                              warning = function(y) { warn.metric <<- c(warn.metric, conditionMessage(y)); invokeRestart("muffleWarning") })

    }

    # Scalar measurement invariance
    if (isTRUE(any("scalar" %in% invar))) {

      warn.scalar <- NULL

      model.fit.scalar <- withCallingHandlers(misty::multilevel.cfa(x = x, cluster = ".cluster", model = var, rescov = rescov,
                                                                    model.w = NULL, model.b = NULL, rescov.w = NULL, rescov.b = NULL,
                                                                    const = "config", fix.resid = "all",
                                                                    ident = ident, ls.fit = FALSE, estimator = estimator, optim.method = optim.method,
                                                                    missing = missing, output = FALSE),
                                              warning = function(y) { warn.scalar <<- c(warn.scalar, conditionMessage(y)); invokeRestart("muffleWarning") })

    }

    for(i in unique(c(warn.config, warn.metric, warn.scalar))) { warning(i, call. = FALSE) }

  #...................
  ###  Model specification with 'model' ####
  } else {

    # Configural measurement invariance
    model.fit.metric <- model.fit.scalar <- warn.config <- warn.metric <- warn.scalar <- NULL

    model.fit.config <- withCallingHandlers(misty::multilevel.cfa(x = x, cluster = ".cluster", model = NULL, rescov = NULL,
                                                                  model.w = model, model.b = model,  rescov.w = rescov, rescov.b = NULL,
                                                                  fix.resid = fix.resid, ident = ident, ls.fit = FALSE,
                                                                  estimator = estimator, optim.method = optim.method,
                                                                  missing = missing, output = FALSE),
                                            warning = function(y) { warn.config <<- c(warn.config, conditionMessage(y)); invokeRestart("muffleWarning") })

    # Metric measurement invariance
    if (isTRUE(any(c("metric", "scalar") %in% invar))) {

      model.fit.scalar <- warn.metric <- warn.scalar <- NULL

      model.fit.metric <- withCallingHandlers(misty::multilevel.cfa(x = x, cluster = ".cluster", model = model, rescov = rescov,
                                                                    model.w = NULL, model.b = NULL, rescov.w = NULL, rescov.b = NULL,
                                                                    const = "config", fix.resid = fix.resid, ident = ident, ls.fit = FALSE,
                                                                    estimator = estimator, optim.method = optim.method,
                                                                    missing = missing, output = FALSE),
                                              warning = function(y) { warn.metric <<- c(warn.metric, conditionMessage(y)); invokeRestart("muffleWarning") })

    }

    # Scalar measurement invariance
    if (isTRUE(any("scalar" %in% invar))) {

      warn.scalar <- NULL

      model.fit.scalar <- withCallingHandlers(misty::multilevel.cfa(x = x, cluster = ".cluster", model = model, rescov = rescov,
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
         ### # Configural measurement invariance ####
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
           fit.config.stand <- fit.config[c(12L:30L, 6L:10L), c(1L, 2L)]

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
             fit.metric.stand <- fit.metric[c(12L:30, 6L:10L), c(1L, 2L)]

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
             fit.scalar.stand <- fit.scalar[c(10L:27, 4L:8L), ]

           #### Robust maximum Likelihood
           }, MLR = {

             # Standard fit indices
             fit.scalar.stand <- fit.scalar[c(12L:30, 6L:10L), c(1L, 2L)]

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

      fit.scaled <- fit.config.scaled

      ###### Robust fit indices

      fit.robust <- fit.config.robust[, 1L:2L]

    }

    colnames(fit.stand) <- c("", "config")
    if (isTRUE(!is.null(fit.scaled))) { colnames(fit.scaled) <- c("", "config") }
    if (isTRUE(!is.null(fit.scaled))) { colnames(fit.robust) <- c("", "config") }

    #### Configural and metric measurement invariance
    }, metric = {

    ##### Chi-squared difference test, config vs. metric
    chidiff.confmet <- tryCatch(lavaan::lavTestLRT(model.fit.config$model.fit, model.fit.metric$model.fit),
                                error = function(y) { data.frame(matrix(NA, ncol = 7L, dimnames = list(NULL, c("Df", "AIC", "BIC", "Chisq", "Chisq diff", "Df diff", "Pr(>Chisq)"))), check.names = FALSE) },
                                warning = function(z) { suppressWarnings(lavaan::lavTestLRT(model.fit.config$model.fit, model.fit.metric$model.fit, method = "satorra.bentler.2010")) })

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

    }

  #### Configural, metric and scalar measurement invariance
  }, scalar = {

    ##### Chi-squared difference test, config vs. metric
    chidiff.confmet <- tryCatch(lavaan::lavTestLRT(model.fit.config$model.fit, model.fit.metric$model.fit),
                                error = function(y) { data.frame(matrix(NA, ncol = 7L, dimnames = list(NULL, c("Df", "AIC", "BIC", "Chisq", "Chisq diff", "Df diff", "Pr(>Chisq)"))), check.names = FALSE) },
                                warning = function(z) { suppressWarnings(lavaan::lavTestLRT(model.fit.config$model.fit, model.fit.metric$model.fit, method = "satorra.bentler.2010")) })

    ##### Chi-squared difference test, metric vs. scalar
    chidiff.metsca <- tryCatch(lavaan::lavTestLRT(model.fit.metric$model.fit, model.fit.scalar$model.fit),
                               error = function(y) { data.frame(matrix(NA, ncol = 7L, dimnames = list(NULL, c("Df", "AIC", "BIC", "Chisq", "Chisq diff", "Df diff", "Pr(>Chisq)"))), check.names = FALSE) },
                               warning = function(z) { suppressWarnings(lavaan::lavTestLRT(model.fit.metric$model.fit, model.fit.scalar$model.fit, method = "satorra.bentler.2010")) })

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
                             min.value = min.value, digits = digits, p.digits = p.digits,
                             as.na = as.na, check = check, output = output),
                 model = list(config = model.fit.config$model, metric = model.fit.metric$model, scalar = model.fit.scalar$model),
                 model.fit = list(config = model.fit.config$model.fit, metric = model.fit.metric$model.fit, scalar = model.fit.scalar$model.fit),
                 check = list(config = list(vcov = model.fit.config$check$check.vcov, theta.w = model.fit.config$check$check.theta.w, theta.b = model.fit.config$check$check.theta.b, cov.lv.w = model.fit.config$check$check.cov.lv.w, cov.lv.b = model.fit.config$check$check.cov.lv.b),
                              metric = list(vcov = model.fit.metric$check$check.vcov, theta.w = model.fit.metric$check$check.theta.w, theta.b = model.fit.metric$check$check.theta.b, cov.lv.w = model.fit.metric$check$check.cov.lv.w, cov.lv.b = model.fit.metric$check$check.cov.lv.b),
                              config = list(vcov = model.fit.scalar$check$check.vcov, theta.w = model.fit.scalar$check$check.theta.w, theta.b = model.fit.scalar$check$check.theta.b, cov.lv.w = model.fit.scalar$check$check.cov.lv.w, cov.lv.b = model.fit.scalar$check$check.cov.lv.b)),
                 result = list(summary = lavaan.summary, coverage = model.fit.config$result$coverage,
                               descript = model.fit.config$result$descript,
                               fit = list(stand = fit.stand, scaled = fit.scaled, robust = fit.robust),
                               param = list(config = model.fit.config$result$param, metric = model.fit.metric$result$param, scalar = model.fit.scalar$result$param),
                               modind = list(config = model.fit.config$result$modind, metric = model.fit.metric$result$modind, scalar = model.fit.scalar$result$modind)))

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Write Results --------------------------------------------------------------

  if (isTRUE(!is.null(write))) { misty::write.result(object, file = write) }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
