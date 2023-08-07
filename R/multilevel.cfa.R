#' Multilevel Confirmatory Factor Analysis
#'
#' This function is a wrapper function for conducting multilevel confirmatory factor
#' analysis to investigate four types of constructs, i.e., within-cluster constructs,
#' shared cluster-level constructs, configural cluster constructs, and simultaneous
#' shared and configural cluster constructs by calling the \code{cfa} function in
#' the R package \pkg{lavaan}. By default, the function specifies and estimates
#' a configural cluster and provides a table with univariate sample statistics,
#' model fit information, and parameter estimates. Additionally, variance-covariance
#' coverage of the data, modification indices, and residual correlation matrix can
#' be requested by specifying the argument \code{print}.
#'
#'
#' @param x            a matrix or data frame. If \code{model}, \code{model.w},
#'                     and \code{model.b} are \code{NULL}, multilevel confirmatory
#'                     factor analysis based on a measurement model with one factor
#'                     labeled \code{wf} at the Within level and one factor labeled
#'                     \code{bf} at the Between level comprising all variables in
#'                     the matrix or data frame is conducted. Note that the cluster
#'                     variable specified in \code{cluster} is excluded from \code{x}
#'                     when specifying the argument \code{cluster} using the variable
#'                     name of the cluster variable. If \code{model} or \code{mode.w}
#'                     and \code{model.b} is specified, the matrix or data frame
#'                     needs to contain all variables used in the \code{model}
#'                     argument(s).
#' @param cluster      either a character string indicating the variable name of
#'                     the cluster variable in 'x' or a vector representing the
#'                     nested grouping structure (i.e., group or cluster variable).
#' @param model        a character vector for specifying the same factor structure
#'                     with one factor at the Within and Between Level, or a list
#'                     of character vectors for specifying the same measurement
#'                     model with more than one factor at the Within and Between
#'                     Level, e.g.,\code{model = c("x1", "x2", "x3", "x4")} for
#'                     specifying a measurement model with one factor labeled \code{wf}
#'                     at the Within level and a measurement model with one factor
#'                     labeled \code{bf} at the Between level each comprising four
#'                     indicators, or \code{model = list(factor1 = c("x1", "x2", "x3", "x4"),
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
#'                     \code{model.w} and \code{model.b}.
#' @param model.w      a character vector specifying a measurement model with one
#'                     factor at the Within level, or a list of character vectors
#'                     for specifying a measurement model with more than one factor
#'                     at the Within level.
#' @param model.b      a character vector specifying a measurement model with one
#'                     factor at the Between level, or a list of character vectors
#'                     for specifying a measurement model with more than one factor
#'                     at the Between level.
#' @param rescov.w     a character vector or a list of character vectors for
#'                     specifying residual covariances at the Within level.
#'                     Note that this argument applies only
#'                     when the model is specified by using the arguments
#'                     \code{model.w} and \code{model.b}.
#' @param rescov.b     a character vector or a list of character vectors for
#'                     specifying residual covariances at the Between level.
#'                     Note that this argument applies only
#'                     when the model is specified by using the arguments
#'                     \code{model.w} and \code{model.b}.
#' @param const        a character string indicating the type of construct(s), i.e.,
#'                     \code{"within"} for within-cluster constructs, \code{"shared"}
#'                     for shared cluster-level constructs, \code{"config"} (default)
#'                     for configural cluster constructs, and \code{"shareconf"}
#'                     for simultaneous shared and configural cluster constructs.
#' @param fix.resid    a character vector for specifying residual variances to be
#'                     fixed at 0 at the Between level, e.g., \code{fix.resid = c("x1", "x3")}
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
#' @param ls.fit       logical: if \code{TRUE} (default) level-specific fit indices
#'                     are computed when specifying a model using the arguments
#'                     \code{model.w} and \code{model.b} given the model does not
#'                     contain any cross-level equality constraints.
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
#'                     method. Note that FIML method is only available when \code{estimator = "ML"},
#'                     that it takes longer to estimate the model  using FIML, and
#'                     that FIML is prone to convergence issues which might be
#'                     resolved by switching to listwise deletion.
#' @param print        a character string or character vector indicating which
#'                     results to show on the console, i.e. \code{"all"} for all
#'                     results, \code{"summary"} for a summary of the specification
#'                     of the estimation method and missing data handling in lavaan,
#'                     \code{"coverage"} for the variance-covariance coverage of
#'                     the data, \code{"descript"} for descriptive statistics,
#'                     \code{"fit"} for model fit,  \code{"est"} for parameter
#'                     estimates, \code{"modind"} for modification indices, and
#'                     \code{"resid"} for the residual correlation matrix and
#'                     standardized residual means. By default, a summary of the
#'                     specification, descriptive statistics, model fit, and
#'                     parameter estimates are printed.
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
#' \code{\link{item.cfa}}, \code{\link{multilevel.fit}}, \code{\link{multilevel.invar}},
#' \code{\link{multilevel.omega}}, \code{\link{multilevel.cor}}, \code{\link{multilevel.descript}},
#' \code{\link{write.result}}
#'
#' @references
#' Rosseel, Y. (2012). lavaan: An R Package for Structural Equation Modeling.
#' \emph{Journal of Statistical Software, 48}, 1-36. https://doi.org/10.18637/jss.v048.i02
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{data}}{matrix or data frame specified in \code{x}}
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
#'                      and \code{resid} for a list with residual correlation
#'                      matrices and standardized residual means for the Within
#'                      and Between level}
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
#' #---------------------------
#' # Model specification using 'x' for a one-factor model
#' # with the same factor structure with one factor at the Within and Between Level
#'
#' #..........
#' # Cluster variable specification
#'
#' # Cluster variable 'cluster' in 'x'
#' multilevel.cfa(Demo.twolevel[, c("y1", "y2", "y3", "y4", "cluster")], cluster = "cluster")
#'
#' # Cluster variable 'cluster' not in 'x'
#' multilevel.cfa(Demo.twolevel[, c("y1", "y2", "y3", "y4")], cluster = Demo.twolevel$cluster)
#'
#' #..........
#' # Type of construct
#'
#' # Within-cluster constructs
#' multilevel.cfa(Demo.twolevel[, c("y1", "y2", "y3", "y4")], cluster = Demo.twolevel$cluster,
#'                const = "within")
#'
#' # Shared cluster-level construct
#' multilevel.cfa(Demo.twolevel[, c("y1", "y2", "y3", "y4")], cluster = Demo.twolevel$cluster,
#'                const = "shared")
#'
#' # Configural cluster construct (default)
#' multilevel.cfa(Demo.twolevel[, c("y1", "y2", "y3", "y4")], cluster = Demo.twolevel$cluster,
#'                const = "config")
#'
#' # Simultaneous shared and configural cluster construct
#' multilevel.cfa(Demo.twolevel[, c("y1", "y2", "y3", "y4")], cluster = Demo.twolevel$cluster,
#'                const = "shareconf")
#'
#' #..........
#' # Residual covariances at the Within level
#'
#' # Residual covariance between 'y1' and 'y3'
#' multilevel.cfa(Demo.twolevel[, c("y1", "y2", "y3", "y4")], cluster = Demo.twolevel$cluster,
#'                rescov = c("y1", "y3"))
#'
#' # Residual covariance between 'y1' and 'y3', and 'y2' and 'y4'
#' multilevel.cfa(Demo.twolevel[, c("y1", "y2", "y3", "y4")], cluster = Demo.twolevel$cluster,
#'                rescov = list(c("y1", "y3"), c("y2", "y4")))
#'
#' #..........
#' # Residual variances at the Between level fixed at 0
#'
#' # All residual variances fixed at 0
#' # i.e., strong factorial invariance across clusters
#' multilevel.cfa(Demo.twolevel[, c("y1", "y2", "y3", "y4")], cluster = Demo.twolevel$cluster,
#'                fix.resid = "all")
#'
#' # Fesidual variances of 'y1', 'y2', and 'y4' fixed at 0
#' # i.e., partial strong factorial invariance across clusters
#' multilevel.cfa(Demo.twolevel[, c("y1", "y2", "y3", "y4")], cluster = Demo.twolevel$cluster,
#'                fix.resid = c("y1", "y2", "y4"))
#'
#' #..........
#' # Print all results
#'
#' # Set minimum value for modification indices to 1
#' multilevel.cfa(Demo.twolevel[, c("y1", "y2", "y3", "y4")], cluster = Demo.twolevel$cluster,
#'                print = "all", mod.minval = 1)
#'
#' #..........
#' # lavaan model and summary of the estimated model
#'
#' mod <- multilevel.cfa(Demo.twolevel[, c("y1", "y2", "y3", "y4")], cluster = Demo.twolevel$cluster,
#'                       output = FALSE)
#'
#' # lavaan model syntax
#' cat(mod$model)
#'
#' # Fitted lavaan object
#' lavaan::summary(mod$model.fit, standardized = TRUE, fit.measures = TRUE)
#'
#' #..........
#' # Write results
#'
#' # Assign results into an object and write results into an Excel file
#' mod <- multilevel.cfa(Demo.twolevel[, c("y1", "y2", "y3", "y4")], cluster = Demo.twolevel$cluster,
#'                       print = "all", output = FALSE)
#'
#' # Write results into an Excel file
#' write.result(mod, "Multilevel_CFA.xlsx")
#'
#' # Estimate model and write results into an Excel file
#' multilevel.cfa(Demo.twolevel[, c("y1", "y2", "y3", "y4")], cluster = Demo.twolevel$cluster,
#'                print = "all", write = "Multilevel_CFA.xlsx")
#'
#' #---------------------------
#' # Model specification using 'model' for one or multiple factor model
#' # with the same factor structure at the Within and Between Level
#'
#' # One-factor model
#' multilevel.cfa(Demo.twolevel, cluster = "cluster", model = c("y1", "y2", "y3", "y4"))
#'
#' # Two-factor model
#' multilevel.cfa(Demo.twolevel, cluster = "cluster",
#'                model = list(c("y1", "y2", "y3"), c("y4", "y5", "y6")))
#'
#' # Two-factor model with user-specified labels for the factors
#' multilevel.cfa(Demo.twolevel, cluster = "cluster",
#'                model = list(factor1 = c("y1", "y2", "y3"), factor2 = c("y4", "y5", "y6")))
#'
#' #..........
#' # Type of construct
#'
#' # Within-cluster constructs
#' multilevel.cfa(Demo.twolevel, cluster = "cluster", const = "within",
#'                model = list(c("y1", "y2", "y3"), c("y4", "y5", "y6")))
#'
#' # Shared cluster-level construct
#' multilevel.cfa(Demo.twolevel, cluster = "cluster", const = "shared",
#'                model = list(c("y1", "y2", "y3"), c("y4", "y5", "y6")))
#'
#' # Configural cluster construct (default)
#' multilevel.cfa(Demo.twolevel, cluster = "cluster", const = "config",
#'                model = list(c("y1", "y2", "y3"), c("y4", "y5", "y6")))
#'
#' # Simultaneous shared and configural cluster construct
#' multilevel.cfa(Demo.twolevel, cluster = "cluster", const = "shareconf",
#'                model = list(c("y1", "y2", "y3"), c("y4", "y5", "y6")))
#'
#' #..........
#' # Residual covariances at the Within level
#'
#' # Residual covariance between 'y1' and 'y4' at the Within level
#' multilevel.cfa(Demo.twolevel, cluster = "cluster",
#'                model = list(c("y1", "y2", "y3"), c("y4", "y5", "y6")),
#'                rescov = c("y1", "y4"))
#'
#' # Fix all residual variances at 0
#' # i.e., strong factorial invariance across clusters
#' multilevel.cfa(Demo.twolevel, cluster = "cluster",
#'                model = list(c("y1", "y2", "y3"), c("y4", "y5", "y6")),
#'                fix.resid = "all")
#'
#' #---------------------------
#' # Model specification using 'model.w' and 'model.b' for one or multiple factor model
#' # with different factor structure at the Within and Between Level
#'
#' # Two-factor model at the Within level and one-factor model at the Between level
#' multilevel.cfa(Demo.twolevel, cluster = "cluster",
#'                model.w = list(c("y1", "y2", "y3"), c("y4", "y5", "y6")),
#'                model.b = c("y1", "y2", "y3", "y4", "y5", "y6"))
#'
#' # Residual covariance between 'y1' and 'y4' at the Within level
#' # Residual covariance between 'y5' and 'y6' at the Between level
#' multilevel.cfa(Demo.twolevel, cluster = "cluster",
#'                model.w = list(c("y1", "y2", "y3"), c("y4", "y5", "y6")),
#'                model.b = c("y1", "y2", "y3", "y4", "y5", "y6"),
#'                rescov.w = c("y1", "y4"),
#'                rescov.b = c("y5", "y6"))
#' }
multilevel.cfa <- function(x, cluster, model = NULL, rescov = NULL,
                           model.w = NULL, model.b = NULL, rescov.w = NULL, rescov.b = NULL,
                           const = c("within", "shared", "config", "shareconf"), fix.resid = NULL,
                           ident = c("marker", "var", "effect"), ls.fit = TRUE, estimator = c("ML", "MLR"),
                           optim.method = c("nlminb", "em"), missing = c("listwise", "fiml"),
                           print = c("all", "summary", "coverage", "descript", "fit", "est", "modind", "resid"),
                           mod.minval = 6.63, resid.minval = 0.1, digits = 3, p.digits = 3, as.na = NULL,
                           write = NULL, check = TRUE, output = TRUE) {

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

    # Check if input 'model.w' is a character vector or list of character vectors
    if (isTRUE(!is.null(model.w) && !all(sapply(model.w, is.character)))) { stop("Please specify a character vector or list of character vectors for the argument 'model.w'.", call. = FALSE) }

    # Check if input 'model.b' is a character vector or list of character vectors
    if (isTRUE(!is.null(model.b) && !all(sapply(model.w, is.character)))) { stop("Please specify a character vector or list of character vectors for the argument 'model.b'.", call. = FALSE) }

    # Check if 'model.w' and 'model.b' is NULL when model specified using 'model'
    if (isTRUE(!is.null(model) && (!is.null(model.w) || !is.null(model.b)))) { stop("Please specifiy the model either using the argument 'model' or the arguments 'model.w' and 'model.w'.", call. = FALSE) }

    # Model specification with 'model'
    if (isTRUE(!is.null(model))) {

      var.model <- !unique(unlist(model)) %in% colnames(x)
      if (isTRUE(any(var.model))) { stop(paste0("Variables specified in the argument 'model' were not found in 'x': ", paste(unique(unlist(model))[var.model], collapse = ", ")), call. = FALSE) }

    }

    # Model specification with 'model.w'
    if (isTRUE(!is.null(model.w))) {

      var.model.w <- !unique(unlist(model.w)) %in% colnames(x)
      if (isTRUE(any(var.model.w))) { stop(paste0("Variables specified in the argument 'model.w' were not found in 'x': ", paste(unique(unlist(model))[model.w], collapse = ", ")), call. = FALSE) }

    }

    # Model specification with 'model.b'
    if (isTRUE(!is.null(model.b))) {

      var.model.b <- !unique(unlist(model.b)) %in% colnames(x)
      if (isTRUE(any(var.model.b))) { stop(paste0("Variables specified in the argument 'model.b' were not found in 'x': ", paste(unique(unlist(model))[model.b], collapse = ", ")), call. = FALSE) }

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

    # Check input 'rescov.w'
    if (isTRUE(!is.null(rescov.w))) {

      # Two variables for each residual covariance
      if (isTRUE(is.list(rescov.w) && any(sapply(rescov.w, length) != 2L))) { stop("Please specify a list of character vectors for the argument 'rescov.w', where each element has two variable names", call. = FALSE)

      } else { if (isTRUE(length(rescov.w) != 2L)) { stop("Please specify a character vector with two variable names for the argument 'rescov.w'", call. = FALSE) } }

      # Variable in 'x'
      rescov.w.var <- !unique(unlist(rescov.w)) %in% colnames(x)
      if (isTRUE(any(rescov.w.var))) { stop(paste0("Variables specified in the argument 'rescov.w' were not found in 'x': ", paste(unique(unlist(rescov.w))[rescov.w.var], collapse = ", ")), call. = FALSE) }

    }

    # Check input 'rescov.b'
    if (isTRUE(!is.null(rescov.b))) {

      # Two variables for each residual covariance
      if (isTRUE(is.list(rescov.b) && any(sapply(rescov.b, length) != 2L))) { stop("Please specify a list of character vectors for the argument 'rescov.b', where each element has two variable names", call. = FALSE)

      } else { if (isTRUE(length(rescov.b) != 2L)) { stop("Please specify a character vector with two variable names for the argument 'rescov.b'", call. = FALSE) } }

      # Variable in 'x'
      rescov.b.var <- !unique(unlist(rescov.b)) %in% colnames(x)
      if (isTRUE(any(rescov.b.var))) { stop(paste0("Variables specified in the argument 'rescov.w' were not found in 'x': ", paste(unique(unlist(rescov.b))[rescov.b.var], collapse = ", ")), call. = FALSE) }

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

    # Check input 'const'
    if (isTRUE(!all(const %in% c("within", "shared", "config", "shareconf")))) { stop("Character string in the argument 'const' does not match with \"within\", \"shared\", \"config\", or \"shareconf\".", call. = FALSE) }

    # Check input 'fix.resid'
    fix.resid.var <- !unique(fix.resid) %in% colnames(x)
    if (isTRUE(any(fix.resid.var) &&  all(fix.resid != "all"))) { stop(paste0("Variables specified in the argument 'fix.resid' were not found in 'x': ", paste(fix.resid[fix.resid.var], collapse = ", ")), call. = FALSE) }

    # Check input 'ident'
    if (isTRUE(!all(ident %in% c("marker", "var", "effect")))) { stop("Character string in the argument 'ident' does not match with \"marker\", \"var\", or \"effect\".", call. = FALSE) }

    # Check input 'ls.fit'
    if (isTRUE(!is.logical(ls.fit))) { stop("Please specify TRUE or FALSE for the argument 'ls.fit'.", call. = FALSE) }

    # Check input 'estimator'
    if (isTRUE(!all(estimator %in% c("ML", "MLR")))) { stop("Character string in the argument 'estimator' does not match with \"ML\" or \"MLR\".", call. = FALSE) }

    # Check input 'optim.method'
    if (isTRUE(!all(optim.method  %in% c("nlminb", "em")))) { stop("Character string in the argument 'optim.method' does not match with \"nlminb\" or \"em\".", call. = FALSE) }

    # Check input 'missing'
    if (isTRUE(!all(missing %in% c("listwise", "fiml")))) { stop("Character string in the argument 'missing' does not match with \"listwise\" or \"fiml\".", call. = FALSE) }

    # Check input 'print'
    if (isTRUE(!all(print %in% c("all", "summary", "coverage", "descript", "fit", "est", "modind", "resid")))) { stop("Character strings in the argument 'print' do not all match with \"summary\", \"coverage\", \"descript\", \"fit\", \"est\", \"modind\", or \"resid\".", call. = FALSE) }

    # Check input 'mod.minval'
    if (isTRUE(mod.minval <= 0L)) { stop("Please specify a value greater than 0 for the argument 'mod.minval'.", call. = FALSE) }

    ## Check input 'resid.minval' ##
    if (isTRUE(resid.minval < 0L)) { stop("Please specify a value greater than or equal 0 for the argument 'resid.minval'.", call. = FALSE) }

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
  if (isTRUE(is.null(model) && is.null(model.w) && is.null(model.b))) {

    # Cluster variable in the data
    if (isTRUE(length(cluster) == 1L)) { var <- colnames(x)[!colnames(x) %in% cluster] } else { var <- colnames(x) }

  #...................
  ### Model specification with 'model' ####
  } else if (isTRUE(!is.null(model) && is.null(model.w) && is.null(model.b))) {

    var <- unique(unlist(model))

  #...................
  ### Model specification with 'model.w' and 'model.b' ####
  } else if (isTRUE(is.null(model) && (!is.null(model.w) || !is.null(model.b)))) {

    var <- unique(c(unlist(model.w), unlist(model.b)))

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
  if (isTRUE(!is.null(model) && is.null(model.w) && is.null(model.b))) {

    # 'model' is a list
    if (is.list(model)) {

      # List elements not all named
      if (isTRUE(is.null(names(model)) || any(names(model) == ""))) { names(model) <- paste0("f", seq_along(model)) }

    # 'model' is not a list
    } else {

      model <- list(f = model)

    }

  #...................
  ### Model specification with 'model.w' and 'model.b' ####
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

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Residual covariance ####

  if (isTRUE(!is.null(rescov) && !is.list(rescov))) {

    rescov <- list(rescov)

  }

  if (isTRUE(!is.null(rescov.w) && !is.list(rescov.w))) {

    rescov.w <- list(rescov.w)

  }

  if (isTRUE(!is.null(rescov.b) && !is.list(rescov.b))) {

    rescov.b <- list(rescov.b)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Type of construct(s) ####

  if (isTRUE(all(c("within", "shared", "config", "shareconf") %in% const))) { const <- "config" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Residual variances fixed at 0 ####

  if (isTRUE(fix.resid == "all")) { fix.resid <- var }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model identification ####

  if (isTRUE(all(c("marker", "var", "effect") %in% ident))) { ident <- "var" }

  switch(ident,
         marker = { std.lv <- FALSE; effect.coding <- FALSE },
         # Fixed factor method needs to be specified manually
         var = { if (isTRUE(is.null(model.w) && is.null(model.b))) { std.lv <- FALSE; effect.coding <- FALSE } else { std.lv <- TRUE; effect.coding <- FALSE } },
         effect = {std.lv <- FALSE; effect.coding <- TRUE })

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

  if (isTRUE(all(c("all", "summary", "coverage", "descript", "fit", "est", "modind", "resid") %in% print))) {

    print  <- c("summary", "descript", "fit", "est")

  }

  if (isTRUE(length(print) == 1L && "all" %in% print)) {

    print <- c("summary", "coverage", "descript", "fit", "est", "modind", "resid")

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Covariance coverage ####

  coverage <- misty::na.coverage(x[, var], check = FALSE, output = FALSE)$result

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Sample Statistics ####

  # Descriptive statistics and Intraclass Correlation Coefficient, ICC(1)
  descript.var <- data.frame(misty::descript(x[, var], check = FALSE, output = FALSE)$result[, c("variable", "n", "nNA", "pNA", "m", "sd", "min", "max", "skew", "kurt")],
                             ICC = misty::multilevel.icc(x[, var], cluster = x$.cluster))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model Specification ####

  #...................
  ### Model specification with 'x' ####
  if (isTRUE(is.null(model) && is.null(model.w) && is.null(model.b))) {

    switch(const,
    #### Within-cluster constructs ####
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
    #### Shared cluster-level construct ####
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
        mod.l12 <- paste(  # Within level
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
    #### Configural cluster construct ####
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
    #### Simultaneous shared and configural cluster construct ####
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

  #...................
  ### Model specification with 'model' ####
  } else if (isTRUE(!is.null(model) && is.null(model.w) && is.null(model.b))) {

    switch(const,
           #### Within-cluster constructs ####
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
           #### Shared cluster-level construct ####
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
           #### Configural cluster construct ####
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
           #### Simultaneous shared and configural cluster construct ####
           shareconf = {

             # Labels for parameter constraints
             model.label <- paste0("L", 1L:length(unlist(model)), "*", unlist(model))
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

  #...................
  ### Model specification with 'model.w' and 'model.b' ####
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

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model estimation ####

  model.fit <- tryCatch(suppressWarnings(lavaan::cfa(mod.l12, data = x, cluster = ".cluster",
                                                     estimator = estimator, optim.method = optim.method,
                                                     missing = missing, std.lv = std.lv, effect.coding = effect.coding,
                                                     se = ifelse(estimator == "MLR", "robust.huber.white", "standard"),
                                                     check.gradient = FALSE, check.post = FALSE, check.vcov = FALSE)),
                        error = function(y) {

                          if (isTRUE(missing == "fiml")) {

                            stop("There was an estimation problem in lavaan, switching to missing = \"listwise\" might solve the problem.", call. = FALSE)

                          } else if (isTRUE(estimator == "MLR")){

                            stop("There was an estimation problem in lavaan, switching to estimator = \"ML\" might solve the problem.", call. = FALSE)

                          } else {

                            stop("There was an estimation problem in lavaan, model could not be estimated.", call. = FALSE)

                          }})

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convergence and model identification checks ####

  if (isTRUE(check)) {

    check.vcov <- check.theta.w <- check.theta.b <- check.cov.lv.w <- check.cov.lv.b <- TRUE

    #...................
    ### Model convergence ####

    if (isTRUE(!lavaan::lavInspect(model.fit, what = "converged"))) { stop("CFA model did not converge.", call. = FALSE) }

    #...................
    ### Degrees of freedom ####

    if (isTRUE(suppressWarnings(lavaan::lavInspect(model.fit, what = "fit")["df"] < 0L))) { stop("CFA model has negative degrees of freedom, model is not identified.", call. = FALSE) }

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

    #### Within Level
    if (isTRUE(any(diag(lavaan::lavInspect(model.fit, what = "theta")$within) < 0L))) {

      warning("Some estimated variances of the observed variables at the Within level are negative.", call. = FALSE)

      check.theta.w <- FALSE

    } else if (isTRUE(any(eigen(lavaan::lavTech(model.fit, what = "theta")$within, symmetric = TRUE, only.values = TRUE)$values < (-1L * .Machine$double.eps^(3/4))))) {

      warning("The model-implied variance-covariance matrix of the residuals of the observed variables is not positive definite.", call. = FALSE)

      check.theta.w <- FALSE

    }

    #### Between Level
    if (isTRUE(any(diag(lavaan::lavInspect(model.fit, what = "theta")$.cluster) < 0L))) {

      warning("Some estimated variances of the observed variables at the Between level are negative.", call. = FALSE)

      check.theta.b <- FALSE

    } else if (isTRUE(any(eigen(lavaan::lavTech(model.fit, what = "theta")$.cluster, symmetric = TRUE, only.values = TRUE)$values < (-1L * .Machine$double.eps^(3/4))))) {

      warning("The model-implied variance-covariance matrix of the residuals of the observed variables at the Between level is not positive definite.", call. = FALSE)

      check.theta.b <- FALSE

    }

    #...................
    ### Negative variance of latent variables ####

    #### Within Level
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

    #### Between Level
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

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model fit ####

  # Parameter table
  mod.par <- lavaan::parTable(model.fit)

  #...................
  ### Model specified with 'x' or 'model' ####
  if (isTRUE(is.null(model.w) && is.null(model.b))) {

    model.fit.measu <- suppressWarnings(lavaan::lavInspect(model.fit, what = "fit"))

  #...................
  ### Model specified with 'model.w' and 'model.b' and no cross-level constraints ####
  } else {

    ##### Level-specific fit indices
    if (isTRUE(ls.fit)) {

      # Check if model includes cross-level constraints
      if (isTRUE(any(mod.par$op == "=="))) {

        # L1 Parameters
        l1.par <- mod.par[mod.par$level == 1L, "plabel"]
        # L2 Parameters
        l2.par <- mod.par[mod.par$level == 2L, "plabel"]

        # Cross-level constraints
        cl.const <- apply(mod.par[mod.par$op == "==", ], 1L, function(y) (y["lhs"] %in% l1.par && y["rhs"] %in% l2.par) | (y["lhs"] %in% l2.par && y["rhs"] %in% l1.par))

        # Model contains cross-level constraints
        if (isTRUE(any(cl.const))) {

          model.fit.measu <- lavaan::lavInspect(model.fit, what = "fit")

        # Model does not contain cross-level constraints, i.e., compute level-specific fit indices
        } else {

          model.fit.measu <- suppressWarnings(misty::multilevel.fit(model.fit, check = FALSE, output = FALSE))$result$fit

        }

      }

      model.fit.measu <- suppressWarnings(misty::multilevel.fit(model.fit, check = FALSE, output = FALSE))$result$fit

    ##### No level-specific fit indices
    } else {

      model.fit.measu <- lavaan::lavInspect(model.fit, what = "fit")

    }

  }

  # Just identified model, df = 0
  if (isTRUE(!is.data.frame(model.fit.measu) && model.fit.measu["df"] == 0L)) {

    model.fit.measu[c("cfi", "tli", "cfi.scaled", "tli.scaled", "cfi.robust", "tli.robust")] <- 1L
    model.fit.measu[c("rmsea", "rmsea.scaled", "rmsea.robust")] <- 0L

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Parameter estimates ####

  model.param <- data.frame(lavaan::parameterEstimates(model.fit),
                            stdyx = lavaan::standardizedsolution(model.fit)[, "est.std"])[, c("lhs", "op", "rhs", "level", "est", "se", "z", "pvalue", "stdyx")]

  model.param[apply(model.param[, c("z", "pvalue")], 1L, function(y) all(is.na(y))), "se"] <- NA

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Modification indices ####

  model.modind <- tryCatch(suppressWarnings(lavaan::modindices(model.fit)),
                           error = function(y) {

                             if (isTRUE("modind" %in% print)) { warning("Modification indices could not be computed.", call. = FALSE) }

                             return(NULL)

                            })

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Score Test ####

  model.score <- tryCatch(lavaan::lavTestScore(model.fit, epc = TRUE, warn = FALSE), error = function(y) {

      if (isTRUE("modind" %in% print)) { warning("Modification indices for parameter constraints could not be computed.", call. = FALSE) }

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

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Residual Correlation Matrix ####

  model.resid <- tryCatch(lavaan::lavResiduals(model.fit, type = "cor.bollen"), error = function(y) {

    if (isTRUE("resid" %in% print)) { warning("Residual correlation matrix indices could not be computed.", call. = FALSE) }

    return(NULL)

    }, warning = function(z) {})

  # Combine residual correlation matrix and standardized residual means
  if (isTRUE(!is.null(model.resid))) {

    model.resid <- list(within = do.call("rbind", model.resid$within[c("cov", "mean")]),
                        between = do.call("rbind", model.resid$.cluster[c("cov", "mean")]))

  }

  #_____________________________________________________________________________
  #
  # Return object --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## lavaan summary ####

  # Level 1 model parameters
  npar.l1 <- sum(mod.par$level == 1L & mod.par$free != 0L)

  # Level 2 model parameters
  npar.l2 <- sum(mod.par$level == 2L & mod.par$free != 0L)

  # Number of model parameters
  npar <- npar.l1 + npar.l2

  # Number of equality constraints
  npar.eq <- sum(table(misty::chr.omit(mod.par$label)) - 1L)

  # Summary
  lavaan.summary <- data.frame(# First column
                               c(paste("lavaan", lavaan::lavInspect(model.fit, what = "version")), "", "Estimator", "Optimization Method", "",
                                 "Test Statistic", "Standard Errors", "Missing Data", "Identification","",
                                 "Number of Model Parameters", "Within", "Between",
                                 "Number of Equality Constraints", "", "",
                                 "Number of Observations", "Number of Clusters", "Average Cluster Size"),
                               # Second column
                               unlist(c("", "",
                                        # Estimator
                                        ifelse(lavaan::lavInspect(model.fit, what = "options")$test == "standard", "ML", "MLR"),
                                        # Optimization method
                                        toupper(lavaan::lavTech(model.fit, what = "options")$optim.method), "",
                                        # Test statistic
                                        switch(lavaan::lavTech(model.fit, what = "options")$test,
                                               "standard" = "Conventional",
                                               "satorra.bentler" = "Satorra-Bentler",
                                               "scaled.shifted" = "Scale-Shifted",
                                               "mean.var.adjusted" = "Satterthwaite",
                                               "yuan.bentler.mplus" = "Yuan-Bentler"),
                                        switch(lavaan::lavTech(model.fit, what = "options")$se,
                                               "standard" = "Conventional",
                                               "robust.sem" = "Conventional Robust",
                                               "robust.huber.white" = "Huber-White",
                                               "robust.cluster" = "Cluster-Robust H-W",
                                               "robust.cluster.sem" = "Cluster-Robust Conven",
                                               "two.stage" = "Two-Stage",
                                               "robust.two.stage" = "Robust Two-Stage"),
                                        # Missing data
                                        ifelse(lavaan::lavInspect(model.fit, what = "nobs") != lavaan::lavInspect(model.fit, what = "norig"), "Listwise",
                                               ifelse(lavaan::lavInspect(model.fit, what = "nobs") == lavaan::lavInspect(model.fit, what = "norig") && any(is.na(lavaan::lavInspect(model.fit, what = "data"))), "FIML", "None")),
                                        # Identification
                                        switch(ident,
                                               "marker" = "Marker Variable",
                                               "var" = "Factor Variance",
                                               "effect" = "Effects Coding"), "",
                                        # Numer of model parameters
                                        npar, npar.l1, npar.l2,
                                        # Numer of equality constraints
                                        npar.eq, "", "Used",
                                        # Number of observations
                                        lavaan::lavInspect(model.fit, what = "nobs"),
                                        # Number of clusters
                                        lavaan::lavInspect(model.fit, what = "nclusters"),
                                        # Average cluster size
                                        lavaan::lavInspect(model.fit, what = "ncluster.size"))),
                                # Third column
                                c(rep("", times = 15L), "Total", lavaan::lavInspect(model.fit, what = "norig"), "", ""),
                                fix.empty.names = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model fit ####

  #...................
  ### Simultaneous model fit information only ####
  if (isTRUE(!is.data.frame(model.fit.measu))) {

    model.fit.measures <- data.frame(# Fist column
                                     c("Loglikelihood",
                                       "H0 Value, Specified Model", "Scaling Correction Factor", "H1 Value, Unrestricted Model", "Scaling Correction Factor", "",
                                       "Information Criteria", "Akaike (AIC)", "Bayesian (BIC)", "Sample-size Adjusted BIC", "",
                                       "Chi-Square Test of Model Fit",
                                       "Test statistic", "Degrees of freedom", "P-value", "Scaling Correction Factor", "",
                                       "Incremental Fit Indices",
                                       "CFI", "TLI", "",
                                       "Absolute Fit Indices",
                                       "RMSEA", "90 Percent CI - lower", "90 Percent CI - upper", "P-value RMSEA <= 0.05", "",
                                       "SRMR", "Within", "Between"),
                                     # Second column
                                     standard = c(# Loglikelihood
                                                  NA, model.fit.measu[c("logl", "scaling.factor.h0", "unrestricted.logl", "scaling.factor.h1")], NA, NA,
                                                  # Information Criteria
                                                  model.fit.measu[c("aic", "bic", "bic2")], NA, NA,
                                                  # Test statistic, Degrees of freedom, P-value, and Scaling correction factor
                                                  model.fit.measu[c("chisq", "df", "pvalue", "chisq.scaling.factor")], NA, NA,
                                                  # CFI and TLI
                                                  model.fit.measu[c("cfi", "tli")], NA, NA,
                                                  # RMSEA
                                                  model.fit.measu[c("rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue")], NA,
                                                  # SRMR
                                                  model.fit.measu[c("srmr", "srmr_within", "srmr_between")]),
                                     # Third column
                                     scaled = c(# Loglikelihood and information criteria
                                                rep(NA, times = 12L),
                                                # Test statistic, Degrees of freedom, P-value, and Scaling correction factor
                                                model.fit.measu[c("chisq.scaled", "df.scaled", "pvalue.scaled", "chisq.scaling.factor")], NA, NA,
                                                # Scaled CFI and TLI
                                                model.fit.measu[c("cfi.scaled", "tli.scaled")], NA, NA,
                                                # Scaled RMSEA
                                                model.fit.measu[c("rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "rmsea.pvalue.scaled")], NA,
                                                # SRMR
                                                rep(NA, times = 3L)),
                                     # Fourth column
                                     robust = c(# Loglikelihood and information criteria
                                                rep(NA, times = 18L),
                                                # Robust CFI and TLI
                                                model.fit.measu[c("cfi.robust", "tli.robust")], NA, NA,
                                                # Robust RMSEA
                                                model.fit.measu[c("rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust", "rmsea.pvalue.robust")], NA,
                                                # SRMR
                                                rep(NA, times = 3L)),
                                     fix.empty.names = FALSE)

    # Empty third and fourth column
    if (isTRUE(estimator == "ML")) {

      model.fit.measures <- model.fit.measures[-c(3L, 5L, 16L), c(1L, 2L)]

      rownames(model.fit.measures) <- seq_len(nrow(model.fit.measures))

    }

  #...................
  ### Simultaneous and level-specific model fit information ####
  } else {

    model.fit.measures <- model.fit.measu

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Parameter estimates ####

  # Within parameters
  param.w <- model.param[model.param$level == 1L, ]

  # Between parameters
  param.b <- model.param[model.param$level == 2L, ]

  #...................
  ### Within Parameter estimates ####

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

  #...................
  ### Between Parameter estimates ####

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
                      within =
                      rbind(if (nrow(latent.w) > 0L) { data.frame(param = "latent variable", latent.w) } else { NULL },
                            if (nrow(lv.cov.w) > 0L) { data.frame(param = "latent variable covariance", lv.cov.w) } else { NULL },
                            if (nrow(res.cov.w) > 0L) { data.frame(param = "residual covariance", res.cov.w) } else { NULL },
                            if (nrow(mean.w) > 0L) { data.frame(param = "latent mean", mean.w) } else { NULL },
                            if (nrow(var.w) > 0L) { data.frame(param = "latent variance", var.w) } else { NULL },
                            if (nrow(interc.w) > 0L) { data.frame(param = "intercept", interc.w) } else { NULL },
                            if (nrow(resid.w) > 0L) { data.frame(param = "residual variance", resid.w) } else { NULL }),
                      # Model parameter Between
                      between =
                      rbind(if (nrow(latent.b) > 0L) { data.frame(param = "latent variable", latent.b) } else { NULL },
                            if (nrow(lv.cov.b) > 0L) { data.frame(param = "latent variable covariance", lv.cov.b) } else { NULL },
                            if (nrow(res.cov.b) > 0L) { data.frame(param = "residual covariance", res.cov.b) } else { NULL },
                            if (nrow(mean.b) > 0L) { data.frame(param = "latent mean", mean.b) } else { NULL },
                            if (nrow(var.b) > 0L) { data.frame(param = "latent variance", var.b) } else { NULL },
                            if (nrow(interc.b) > 0L) { data.frame(param = "intercept", interc.b) } else { NULL },
                            if (nrow(resid.b) > 0L) { data.frame(param = "residual variance", resid.b) } else { NULL }))

  #...................
  ### Within add labels ####

  # Latent mean and intercept
  model.param$within[model.param$within$param %in% c("latent mean", "intercept"), "rhs"] <- model.param$within[model.param$within$param %in% c("latent mean", "intercept"), "lhs"]

  # Latent variables
  param.lv.w <- NULL
  for (i in unique(model.param$within[which(model.param$within$param == "latent variable"), "lhs"])) {

    param.lv.w <- rbind(param.lv.w,
                        data.frame(param = "latent variable", lhs = i, op = "", rhs = paste(i, "=~"), est = NA, se = NA, z = NA, pvalue = NA, stdyx = NA),
                        model.param$within[which(model.param$within$param == "latent variable" & model.param$within$lhs == i), colnames(model.param$within) != "level"])

  }

  # Latent variable covariances
  param.lv.cov.w <- NULL
  for (i in unique(model.param$within[which(model.param$within$param == "latent variable covariance"), "lhs"])) {

    param.lv.cov.w <- rbind(param.lv.cov.w,
                            data.frame(param = "latent variable covariance", lhs = i, op = "", rhs = paste(i, "~~"), est = NA, se = NA, z = NA, pvalue = NA, stdyx = NA),
                            model.param$within[which(model.param$within$param == "latent variable covariance" & model.param$within$lhs == i), colnames(model.param$within) != "level"])

  }

  # Residual covariances
  param.res.cov.w <- NULL
  for (i in unique(model.param$within[which(model.param$within$param == "residual covariance"), "lhs"])) {

    param.res.cov.w <- rbind(param.res.cov.w,
                             data.frame(param = "residual covariance", lhs = i, op = "", rhs = paste(i, "~~"), est = NA, se = NA, z = NA, pvalue = NA, stdyx = NA),
                             model.param$within[which(model.param$within$param == "residual covariance" & model.param$within$lhs == i), colnames(model.param$within) != "level"])

  }

  model.param$within <- rbind(param.lv.w, param.lv.cov.w, param.res.cov.w,
                              model.param$within[which(!model.param$within$param %in% c("latent variable", "latent variable covariance", "residual covariance")), colnames(model.param$within) != "level"])

  #...................
  ### Between add labels ####

  # Latent mean and intercept
  model.param$between[model.param$between$param %in% c("latent mean", "intercept"), "rhs"] <- model.param$between[model.param$between$param %in% c("latent mean", "intercept"), "lhs"]

  # Latent variables
  param.lv.b <- NULL
  for (i in unique(model.param$between[which(model.param$between$param == "latent variable"), "lhs"])) {

    param.lv.b <- rbind(param.lv.b,
                        data.frame(param = "latent variable", lhs = i, op = "", rhs = paste(i, "=~"), est = NA, se = NA, z = NA, pvalue = NA, stdyx = NA),
                        model.param$between[which(model.param$between$param == "latent variable" & model.param$between$lhs == i), colnames(model.param$between) != "level"])

  }

  # Latent variable covariances
  param.lv.cov.b <- NULL
  for (i in unique(model.param$between[which(model.param$between$param == "latent variable covariance"), "lhs"])) {

    param.lv.cov.b <- rbind(param.lv.cov.b,
                            data.frame(param = "latent variable covariance", lhs = i, op = "", rhs = paste(i, "~~"), est = NA, se = NA, z = NA, pvalue = NA, stdyx = NA),
                            model.param$between[which(model.param$between$param == "latent variable covariance" & model.param$between$lhs == i), colnames(model.param$between) != "level"])

  }

  # Residual covariances
  param.res.cov.b <- NULL
  for (i in unique(model.param$between[which(model.param$between$param == "residual covariance"), "lhs"])) {

    param.res.cov.b <- rbind(param.res.cov.b,
                             data.frame(param = "residual covariance", lhs = i, op = "", rhs = paste(i, "~~"), est = NA, se = NA, z = NA, pvalue = NA, stdyx = NA),
                             model.param$between[which(model.param$between$param == "residual covariance" & model.param$between$lhs == i), colnames(model.param$between) != "level"])

  }

  model.param$between <- rbind(param.lv.b, param.lv.cov.b, param.res.cov.b,
                               model.param$between[which(!model.param$between$param %in% c("latent variable", "latent variable covariance", "residual covariance")), colnames(model.param$between) != "level"])

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Modification indices ####

  if (isTRUE(!is.null(model.modind))) {

    if (isTRUE("level" %in% colnames(model.modind))) {

      model.modind <- list(within = misty::df.rename(model.modind[which(model.modind$level == 1L), c("lhs", "op", "rhs", "mi", "epc", "sepc.all")], from = "sepc.all", to = "stdyx"),
                           between = misty::df.rename(model.modind[which(model.modind$level == 2L), c("lhs", "op", "rhs", "mi", "epc", "sepc.all")], from = "sepc.all", to = "stdyx"))

    } else {

      model.modind <- list(within = misty::df.rename(model.modind[, c("lhs", "op", "rhs", "mi", "epc", "sepc.all")], from = "sepc.all", to = "stdyx"),
                          between = misty::df.rename(model.modind[-c(1L:nrow(model.modind)), c("lhs", "op", "rhs", "mi", "epc", "sepc.all")], from = "sepc.all", to = "stdyx"))

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Return object ####

  object <- list(call = match.call(),
                 type = "multilevel.cfa",
                 data = x,
                 args = list(model = model, rescov = rescov,
                             model.w = model.w, model.b = model.b, rescov.w = rescov.w, rescov.b = rescov.b,
                             const = const, fix.resid = fix.resid, ident = ident,
                             estimator = estimator, optim.method = optim.method,
                             missing = missing, print = print, mod.minval = mod.minval,
                             resid.minval = resid.minval, digits = digits, p.digits = p.digits,
                             as.na = as.na, check = check, output = output),
                 model = mod.l12,
                 model.fit = model.fit,
                 check = list(vcov = check.vcov, theta.w = check.theta.w, theta.b = check.theta.b,
                              cov.lv.w = check.cov.lv.w, cov.lv.b = check.cov.lv.b),
                 result = list(summary = lavaan.summary, coverage = coverage,
                               descript = descript.var, fit = model.fit.measures,
                               param = model.param, modind = model.modind, score = model.score, resid = model.resid))

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
