#' Multilevel Composite Reliability
#'
#' This function computes point estimate and Monte Carlo confidence interval for
#' the multilevel composite reliability defined by Lai (2021) for a within-cluster
#' construct, shared cluster-level construct, and configural cluster construct by
#' calling the \code{cfa} function in the R package \pkg{lavaan}.
#'
#' @param ...          a matrix or data frame. Multilevel confirmatory factor
#'                     analysis based on a measurement model with one factor
#'                     at the Within level and one factor at the Between level
#'                     comprising all variables in the matrix or data frame is
#'                     conducted. Note that the cluster variable specified in
#'                     \code{cluster} is excluded from \code{x} when specifying
#'                     the argument \code{cluster} using the variable name of the
#'                     cluster variable. Alternatively, an expression indicating
#'                     the variable names in \code{data} e.g.,
#'                     \code{multilevel.omega(x1, x2, x3, data = dat, cluster = "cluster)}.
#'                     Note that the operators \code{.}, \code{+}, \code{-},
#'                     \code{~}, \code{:}, \code{::}, and \code{!} can also be
#'                     used to select variables, see 'Details' in the
#'                     \code{\link{df.subset}} function.
#' @param data         a data frame when specifying one or more variables in the
#'                     argument \code{...}. Note that the argument is \code{NULL}
#'                     when specifying a matrix or data frame for the argument
#'                     \code{...}.
#' @param cluster      either a character string indicating the variable name of
#'                     the cluster variable in \code{...} or \code{data}, or a
#'                     vector representing the nested grouping structure (i.e.,
#'                     group or cluster variable).
#' @param rescov       a character vector or a list of character vectors for specifying
#'                     residual covariances at the Within level, e.g. \code{rescov = c("x1", "x2")}
#'                     for specifying a residual covariance between indicators \code{x1}
#'                     and \code{x2} at the Within level or \code{rescov = list(c("x1", "x2"), c("x3", "x4"))}
#'                     for specifying residual covariances between indicators \code{x1}
#'                     and \code{x2}, and indicators \code{x3} and \code{x4} at
#'                     the Within level. Note that residual covariances at the
#'                     Between level cannot be  specified using this function.
#' @param const        a character string indicating the type of construct(s), i.e.,
#'                     \code{"within"} for within-cluster constructs, \code{"shared"}
#'                     for shared cluster-level constructs, and \code{"config"}
#'                     (default) for configural cluster constructs.
#' @param fix.resid    a character vector for specifying residual variances to be
#'                     fixed at 0 at the Between level, e.g., \code{fix.resid = c("x1", "x3")}
#'                     to fix residual variances of indicators \code{x1} and \code{x2}
#'                     at the Between level at 0. Note that it is also possible
#'                     to specify \code{fix.resid = "all"} which fixes all residual
#'                     variances at the Between level at 0 in line with the strong
#'                     factorial measurement invariance assumption across cluster.
#' @param optim.method a character string indicating the optimizer, i.e., \code{"nlminb"}
#'                     (default) for the unconstrained and bounds-constrained
#'                     quasi-Newton method optimizer and \code{"em"} for the
#'                     Expectation Maximization (EM) algorithm.
#' @param missing      a character string indicating how to deal with missing data,
#'                     i.e., \code{"listwise"} for listwise deletion or \code{"fiml"}
#'                     (default) for full information maximum likelihood (FIML)
#'                     method.
#' @param nrep         an integer value indicating the number of Monte Carlo
#'                     repetitions for computing confidence intervals.
#' @param seed         a numeric value specifying the seed of the random number
#'                     generator for computing the Monte Carlo confidence interval.
#' @param conf.level   a numeric value between 0 and 1 indicating the confidence
#'                     level of the interval.
#' @param print        a character vector indicating which results to show, i.e.
#'                     \code{"all"} (default), for all results \code{"omega"} for
#'                     omega, and \code{"item"} for item statistics.
#' @param digits       an integer value indicating the number of decimal places
#'                     to be used for displaying results. Note that loglikelihood,
#'                     information criteria and chi-square test statistic is
#'                     printed with \code{digits} minus 1 decimal places.
#' @param as.na        a numeric vector indicating user-defined missing values,
#'                     i.e. these values are converted to \code{NA} before conducting
#'                     the analysis. Note that \code{as.na()} function is only
#'                     applied to \code{x} but not to \code{cluster}.
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
#' \code{\link{item.omega}}, \code{\link{multilevel.cfa}}, \code{\link{multilevel.fit}},
#' \code{\link{multilevel.invar}}, \code{\link{multilevel.cor}},
#' \code{\link{multilevel.descript}}, \code{\link{write.result}}
#'
#' @references
#' Lai, M. H. C. (2021). Composite reliability of multilevel data: It’s about
#' observed scores and construct meanings. \emph{Psychological Methods, 26}(1),
#' 90–102. https://doi.org/10.1037/met0000287
#'
#' Rosseel, Y. (2012). lavaan: An R Package for Structural Equation Modeling.
#' \emph{Journal of Statistical Software, 48}, 1-36. https://doi.org/10.18637/jss.v048.i02
#'
#' Venables, W. N., Ripley, B. D. (2002).\emph{Modern Applied Statistics with S} (4th ed.).
#' Springer. https://www.stats.ox.ac.uk/pub/MASS4/.
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{data}}{data frame specified in \code{x} including the group variable
#'                    specified in \code{cluster}}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{model}}{specified model}
#' \item{\code{model.fit}}{fitted lavaan object (\code{mod.fit})}
#' \item{\code{check}}{results of the convergence and model identification check}
#' \item{\code{result}}{list with result tables, i.e., \code{omega} for the coefficient
#'                      omega including Monte Carlo confidence interval and
#'                      \code{itemstat} for descriptive statistics}
#'
#' @note
#' The function uses the functions \code{lavInspect}, \code{lavTech}, and \code{lavNames},
#' provided in the R package \pkg{lavaan} by Yves Rosseel (2012). The internal function
#' \code{.internal.mvrnorm} is a copy of the \code{mvrnorm} function in the package
#' \pkg{MASS} by Venables and Ripley (2002).
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
#' # Example 1a: Cluster variable 'cluster' in 'x'
#' multilevel.omega(Demo.twolevel[, c("y1", "y2", "y3", "y4", "cluster")], cluster = "cluster")
#'
#' # Example 1b: Cluster variable 'cluster' not in 'x'
#' multilevel.omega(Demo.twolevel[, c("y1", "y2", "y3", "y4")], cluster = Demo.twolevel$cluster)
#'
#' # Alternative specification using the 'data' argument
#' multilevel.omega(y1:y4, data = Demo.twolevel, cluster = "cluster")
#'
#' #----------------------------------------------------------------------------
#' # Type of construct
#'
#' # Example 2a: Within-Cluster Construct
#' multilevel.omega(Demo.twolevel[, c("y1", "y2", "y3", "y4")],
#'                  cluster = Demo.twolevel$cluster, const = "within")
#'
#' # Example 2b: Shared Cluster-Level Construct
#' multilevel.omega(Demo.twolevel[, c("y1", "y2", "y3", "y4")],
#'                  cluster = Demo.twolevel$cluster, const = "shared")
#'
#' # Example 2c: Configural Construct
#' multilevel.omega(Demo.twolevel[, c("y1", "y2", "y3", "y4")],
#'                  cluster = Demo.twolevel$cluster, const = "config")
#'
#' #----------------------------------------------------------------------------
#' # Residual covariance at the Within level and residual variance at the Between level
#'
#' # Example 3a: Residual covariance between "y4" and "y5" at the Within level
#' multilevel.omega(Demo.twolevel[,c("y1", "y2", "y3", "y4")],
#'                  cluster = Demo.twolevel$cluster, const = "config",
#'                  rescov = c("y3", "y4"))
#'
#' # Example 3b: Residual variances of 'y1' at the Between level fixed at 0
#' multilevel.omega(Demo.twolevel[, c("y1", "y2", "y3", "y4")],
#'                  cluster = Demo.twolevel$cluster, const = "config",
#'                  fix.resid = c("y1", "y2"), digits = 3)
#'
#' #----------------------------------------------------------------------------
#' # Write results
#'
#' # Example 4a: Write results into a text file
#' multilevel.omega(Demo.twolevel[, c("y1", "y2", "y3", "y4")],
#'                  cluster = Demo.twolevel$cluster, write = "Multilevel_Omega.txt")
#'
#' # Example 4b: Write results into a Excel file
#' multilevel.omega(Demo.twolevel[, c("y1", "y2", "y3", "y4")],
#'                  cluster = Demo.twolevel$cluster, write = "Multilevel_Omega.xlsx")
#'
#' # Example 4b: Assign results into an object and write results into an Excel file
#' mod <- multilevel.omega(Demo.twolevel[,c("y1", "y2", "y3", "y4")],
#'                         cluster = Demo.twolevel$cluster, output = FALSE)
#'
#' # Write results into an Excel file
#' write.result(mod, "Multilevel_Omega.xlsx")
#' }
multilevel.omega <- function(..., data = NULL, cluster, rescov = NULL,
                             const = c("within", "shared", "config"),
                             fix.resid = NULL, optim.method = c("nlminb", "em"),
                             missing = c("listwise", "fiml"), nrep = 100000, seed = NULL,
                             conf.level = 0.95, print = c("all", "omega", "item"),
                             digits = 2, as.na = NULL, write = NULL, append = TRUE,
                             check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input '...' is missing
  if (isTRUE(missing(...))) { stop("Please specify the argument '...'.", call. = FALSE) }

  # Check if input '...' is NULL
  if (isTRUE(is.null(substitute(...)))) { stop("Input specified for the argument '...' is NULL.", call. = FALSE) }

  # Check input 'cluster'
  if (isTRUE(missing(cluster))) { stop("Please specify a variable name or vector representing the grouping structure for the argument 'cluster'.", call. = FALSE) }

  # Check if input 'cluster' is NULL
  if (isTRUE(is.null(cluster))) { stop("Input specified for the argument 'cluster' is NULL.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data using the argument 'data' ####

  if (isTRUE(!is.null(data))) {

    # Convert tibble into data frame
    if (isTRUE("tbl" %in% substr(class(data), 1L, 3L))) { data <- as.data.frame(data) }

    # Extract data
    x <- data[, .var.names(..., data = data, cluster = cluster, check.chr = "a matrix or data frame")]

    # Cluster variable
    cluster <- unlist(data[, cluster])

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data without using the argument 'data' ####

  } else {

    # Extract data
    x <- eval(..., enclos = parent.frame())

    # Convert tibble into data frame
    if (isTRUE("tbl" %in% substr(class(x), 1L, 3L))) { if (isTRUE(ncol(as.data.frame(x)) == 1L)) { x <- unlist(x) } else { x <- as.data.frame(x) } }
    if (isTRUE("tbl" %in% substr(class(cluster), 1L, 3L))) { if (isTRUE(ncol(as.data.frame(cluster)) == 1L)) { cluster <- unlist(cluster) } else { cluster <- as.data.frame(cluster) } }

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

  # Check inputs
  .check.input(logical =  c("append", "output"),
               numeric = list(nrep = 1L, seed = 1L),
               s.character = list(const = c("within", "shared", "config"), optim.method = c("nlminb", "em"), missing = c("listwise", "fiml")),
               m.character = list(print = c("all", "omega", "item")),
               args = c("conf.level", "digits", "write2"), envir = environment(), input.check = check)

  # Additional checks
  if (isTRUE(check)) {

    # Check input 'rescov'
    if (isTRUE(!is.null(rescov))) {

      # Two variables for each residual covariance
      if (isTRUE(is.list(rescov) && any(sapply(rescov, length) != 2L))) { stop("Please specify a list of character vectors for the argument 'rescov', where each element has two variable names", call. = FALSE)

      } else { if (isTRUE(length(rescov) != 2L)) { stop("Please specify a character vector with two variable names for the argument 'rescov'", call. = FALSE) } }

      # Variable in 'x'
      (!unique(unlist(rescov)) %in% colnames(x)) |> (\(y) if (isTRUE(any(y))) { stop(paste0("Variables specified in the argument 'rescov' were not found in 'x': ", paste(unique(unlist(rescov))[y], collapse = ", ")), call. = FALSE) })()

    }

    # Check input 'rescov'
    if (isTRUE(!is.null(rescov))) {

      # Two variables for each residual covariance
      if (isTRUE(is.list(rescov) && any(sapply(rescov, length) != 2L))) { stop("Please specify a list of character vectors for the argument 'rescov', where each element has two variable names", call. = FALSE)

      } else { if (isTRUE(length(rescov) != 2L)) { stop("Please specify a character vector with two variable names for the argument 'rescov'", call. = FALSE) } }

      # Variable in 'x'
      (!unique(unlist(rescov)) %in% colnames(x)) |> (\(y) if (isTRUE(any(y))) { stop(paste0("Variables specified in the argument 'rescov' were not found in 'x': ", paste(unique(unlist(rescov))[y], collapse = ", ")), call. = FALSE) })()

    }

    # Check input 'fix.resid'
    (!unique(fix.resid) %in% colnames(x)) |> (\(y) if (isTRUE(any(y) &&  all(fix.resid != "all"))) { stop(paste0("Variables specified in the argument 'fix.resid' were not found in 'x': ", paste(fix.resid[y], collapse = ", ")), call. = FALSE) })()

    # Check input 'nrep'
    if (isTRUE(mode(nrep) != "numeric" || nrep <= 1L)) { stop("Please specify a positive numeric value greater 1 for the argument 'nrep'.", call. = FALSE) }

    # Check input 'seed'
    if (isTRUE(mode(seed) != "numeric" && !is.null(seed))) { stop("Please specify a numeric value for the argument 'seed'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Data and Arguments ---------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Manifest variables ####

  var <- colnames(x)

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
  ## Type of construct(s) ####

  if (isTRUE(all(c("within", "shared", "config") %in% const))) { const <- "config" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Residual variances fixed at 0 ####

  if (isTRUE(fix.resid == "all")) { fix.resid <- var }

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

      missing <- "fiml"

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Missing Data on All Variable ####

  (misty::na.prop(x[, var]) == 1L) |> (\(y) if (isTRUE(any(y) && missing == "fiml")) { warning(paste0("Data contains cases with missing values on all variables, number of cases removed from the analysis: ", sum(y)), call. = FALSE) })()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Print ####

  if (isTRUE(all(c("all", "omega", "item") %in% print))) {

    print  <- c("omega", "item")

  }

  if (isTRUE(length(print) == 1L && "all" %in% print)) {

    print <- c("omega", "item")

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model Estimation ####

  model.fit <- tryCatch(suppressWarnings(misty::multilevel.cfa(x, cluster = ".cluster", model = NULL, rescov = rescov,
                                         model.w = NULL, model.b = NULL, rescov.w = NULL, rescov.b = NULL,
                                         const = const, fix.resid = fix.resid, ident = "var", ls.fit = FALSE,
                                         estimator = "ML", optim.method = optim.method,
                                         missing = missing, output = FALSE, check = FALSE)),
                        error = function(y) {

                          if (isTRUE(missing == "fiml")) {

                            stop("There was an estimation problem in lavaan, switching to missing = \"listwise\" might solve the problem.", call. = FALSE)

                          } else {

                            stop("There was an estimation problem in lavaan, model could not be estimated.", call. = FALSE)

                          }})

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convergence and model identification checks ####

  if (isTRUE(check)) {

    #...................
    ### Model convergence ####

    if (isTRUE(!lavaan::lavInspect(model.fit$model.fit, what = "converged"))) { stop("CFA model did not converge.", call. = FALSE) }

    #...................
    ### Degrees of freedom ####

    if (isTRUE(suppressWarnings(lavaan::lavInspect(model.fit$model.fit, what = "fit")["df"] < 0L))) { stop("CFA model has negative degrees of freedom, model is not identified.", call. = FALSE) }

    #...................
    ### Variance-covariance matrix of the estimated parameters ####

    eigvals <- eigen(lavaan::lavInspect(model.fit$model.fit, what = "vcov"), symmetric = TRUE, only.values = TRUE)$values

    # Model contains equality constraints
    model.fit.par <- lavaan::parameterTable(model.fit$model.fit)$op == "=="

    if (isTRUE(any(model.fit.par))) { eigvals <- rev(eigvals)[-seq_len(sum(model.fit.par))] }

    if (isTRUE(min(eigvals) < .Machine$double.eps^(3L/4L))) {

      warning("The variance-covariance matrix of the estimated parameters is not positive definite. This may be a symptom that the model is not identified.", call. = FALSE)

    }

    #...................
    ### Negative variance of observed variables ####

    #### Within Level
    if (isTRUE(any(diag(lavaan::lavInspect(model.fit$model.fit, what = "theta")$within) < 0L))) {

      warning("Some estimated variances of the observed variables at the Within level are negative.", call. = FALSE)

    } else if (isTRUE(any(eigen(lavaan::lavTech(model.fit$model.fit, what = "theta")$within, symmetric = TRUE, only.values = TRUE)$values < (-1L * .Machine$double.eps^(3/4))))) {

      warning("The model-implied variance-covariance matrix of the residuals of the observed variables is not positive definite.", call. = FALSE)

    }

    #### Between Level
    if (isTRUE(any(diag(lavaan::lavInspect(model.fit$model.fit, what = "theta")$.cluster) < 0L))) {

      warning("Some estimated variances of the observed variables at the Between level are negative.", call. = FALSE)

    } else if (isTRUE(any(eigen(lavaan::lavTech(model.fit$model.fit, what = "theta")$.cluster, symmetric = TRUE, only.values = TRUE)$values < (-1L * .Machine$double.eps^(3/4))))) {

      warning("The model-implied variance-covariance matrix of the residuals of the observed variables at the Between level is not positive definite.", call. = FALSE)

    }

    #...................
    ### Negative variance of latent variables ####

    #### Within Level
    if (isTRUE(!is.null(lavaan::lavTech(model.fit$model.fit, what = "cov.lv")$within))) {

      if (isTRUE(any(diag(lavaan::lavTech(model.fit$model.fit, what = "cov.lv")$within) < 0L))) {

        warning("Some estimated variances of the latent variables at the Within level are negative.", call. = FALSE)

      }

    # Model-implied variance-covariance matrix of the latent variables
    } else if (any(dim(lavaan::lavTech(model.fit$model.fit, what = "cov.lv")$within) != 0L)) {

      if (isTRUE(any(eigen(lavaan::lavTech(model.fit$model.fit, what = "cov.lv")$within, symmetric = TRUE, only.values = TRUE)$values < (-1L * .Machine$double.eps^(3/4))))) {

        warning("The model-implied variance-covariance matrix of the latent variables at the Within level is not positive definite.", call. = FALSE)

      }

    }

    #### Between Level
    if (isTRUE(!is.null(lavaan::lavTech(model.fit$model.fit, what = "cov.lv")$cluster))) {

      if (isTRUE(any(diag(lavaan::lavTech(model.fit$model.fit, what = "cov.lv")$.cluster) < 0L))) {

        warning("Some estimated variances of the latent variables at the Between level are negative.", call. = FALSE)

        check.cov.lv.b <- FALSE

      }

      # Model-implied variance-covariance matrix of the latent variables
    } else if (any(dim(lavaan::lavTech(model.fit$model.fit, what = "cov.lv")$.cluster) != 0L)) {

      if (isTRUE(any(eigen(lavaan::lavTech(model.fit$model.fit, what = "cov.lv")$.cluster, symmetric = TRUE, only.values = TRUE)$values < (-1L * .Machine$double.eps^(3/4))))) {

        warning("The model-implied variance-covariance matrix of the latent variables at the Between level is not positive definite.", call. = FALSE)

        check.cov.lv.b <- FALSE

      }

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Reliability ####

  # Within model parameter
  param.w <- model.fit$result$param$within
  # Between model parameter
  param.b <- model.fit$result$param$between

  # Within factor loading
  load.w <- na.omit(param.w[param.w$param == "latent variable", "est"])
  # Within residual variances
  resid.w <- param.w[param.w$param == "residual variance", "est"]
  # Fix negative residual variances at 0
  resid.w <- ifelse(resid.w < 0L, 0L, resid.w)
  # Within residual covariances
  rescov.w <- na.omit(param.w[param.w$param == "residual covariance", "est"])

  # Between factor loading
  load.b <- na.omit(param.b[param.b$param == "latent variable", "est"])
  # Between residual variances
  resid.b <- param.b[param.b$param == "residual variance", "est"]
  # Fix negative residual variances at 0
  resid.b <- ifelse(resid.b < 0L, 0L, resid.b)
  # Between factor variance
  var.b <- param.b[param.b$param == "latent variance", "est"]

  # Harmonic mean
  hmean <- length(unique(model.fit$data$.cluster)) / sum(1L / table(model.fit$data$.cluster))

  switch(const,
         #...................
         ### Within-Cluster Construct ####
         within = {

           # Omega Within
           omega.w <- sum(load.w)^2L / (sum(load.w)^2L + sum(resid.w) + 2L*sum(rescov.w))

         #...................
         ### Shared Cluster-Level Construct ####
         }, shared = {

           # Omega Between
           omega.b <- sum(load.b)^2L  / (sum(load.b)^2L + sum(resid.b) + ((sum(resid.w) + 2L*sum(rescov.w)) / hmean))

         #...................
         ### Configural Construct ####
         }, config = {

           # Omega Within
           omega.w <- sum(load.w)^2L / (sum(load.w)^2L + sum(resid.w) + 2L*sum(rescov.w))

           # Omega Between
           omega.b <- (sum(load.b)^2L * var.b) / (sum(load.b)^2L * (1L / hmean + var.b) + sum(resid.b) + ((sum(resid.w) + 2L*sum(rescov.w)) / hmean))

           # Overall Omega
           omega.2l <- (sum(load.b)^2L * (1L + var.b)) / (sum(load.b)^2L * (1L + var.b) + sum(resid.b) + sum(resid.w) + 2L*sum(rescov.w))

         })

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Confidence Interval for the Reliability ####

  omega.2l.sim <- omega.b.sim <- omega.w.sim <- NULL

  #...................
  ### Parameter names ####

  # Within factor loading
  load.w.par <- paste0("L", seq_len(length(load.w)))
  # Within residual variances
  resid.w.par <- apply(param.w[param.w$param == "residual variance", c("lhs", "op", "rhs")], 1L, paste, collapse = "")
  # Within residual covariances
  rescov.w.par <- apply(param.w[param.w$param == "residual covariance" & !is.na(param.w$est), c("lhs", "op", "rhs")], 1L, paste, collapse = "")

  # Between factor loading
  load.b.par <- paste0("L", seq_len(length(load.b)))
  # Between residual variances
  resid.b.par <- paste0(apply(param.b[param.b$param == "residual variance", c("lhs", "op", "rhs")], 1L, paste, collapse = ""), ".l2")
  # Between factor variance
  var.b.par <- paste0(apply(param.b[param.b$param == "latent variance", c("lhs", "op", "rhs")], 1L, paste, collapse = ""), ".l2")

  switch(const, within = {

            parname <- c(load.w.par, resid.w.par, rescov.w.par)

         }, shared = {

            parname <- c(load.b.par, resid.b.par, resid.w.par, rescov.w.par)

         }, config = {

            parname <- c(load.w.par, load.b.par, var.b.par, resid.w.par, resid.b.par, rescov.w.par)

         })

  # Parameter estimates
  fit.est <- na.omit(lavaan::coef(model.fit$model.fit)[parname])
  # Variance-covariance matrix
  fit.vcov <- lavaan::lavInspect(model.fit$model.fit, what = "vcov")[names(fit.est), names(fit.est)]

  # Set seed
  if (isTRUE(!is.null(seed))) { set.seed(seed) }

  # Simulate from a multivariate normal distribution
  simdata <- .internal.mvrnorm(nrep, mu = fit.est, Sigma = fit.vcov)

  # Adapt parameter names
  resid.w.par <- gsub("~~", "..", resid.w.par)
  resid.b.par <- gsub("~~", "..", resid.b.par)
  rescov.w.par <- gsub("~~", "..", rescov.w.par)
  var.b.par <- gsub("~~", "..", var.b.par)

  # Remove fixed Between residuals
  if (isTRUE(!is.null(fix.resid))) {

    resid.b.par <- resid.b.par[which(!resid.b.par %in% sapply(fix.resid, function(y) paste0(y, "..", y, ".l2")))]

    # All Between residuals fixed at 0
    if (isTRUE(length(resid.b.par) == 0L)) {

      resid.b.par <- ".resid.b"
      simdata <- data.frame(simdata, .resid.b = 0L)

    }

  }

  switch(const,
         #...................
         ### Within-Cluster Construct ####
         within = {

           # No residual covariances at the Within level
           if (isTRUE(length(rescov.w.par) == 0L)) {

             # Omega Within
             eval(parse(text = paste0("omega.w.sim <- with(data.frame(simdata), ", paste0("(", paste0(load.w.par, collapse = " + "), ")^2"), " / (", paste0("(", paste0(load.w.par, collapse = " + "), ")^2"), " + ", paste0(resid.w.par, collapse = " + "), "))")))

           } else {

             # Omega Within
             eval(parse(text = paste0("omega.w.sim <- with(data.frame(simdata), ", paste0("(", paste0(load.w.par, collapse = " + "), ")^2"), " / (", paste0("(", paste0(load.w.par, collapse = " + "), ")^2"), " + ", paste0(resid.w.par, collapse = " + "), " + 2*(", paste0(rescov.w.par, collapse = " + "), ")))")))

           }

           # Result table
           omega <- data.frame(type = "omega.w", items = length(lavaan::lavNames(model.fit$model.fit)), omega = omega.w,
                               low = quantile(omega.w.sim, probs = (1L - conf.level) / 2L),
                               upp = quantile(omega.w.sim, probs = 1L - (1L - conf.level) / 2L), row.names = NULL)

         #...................
         ### Shared Cluster-Level Construct ####
         }, shared = {

           # Omega Between
           eval(parse(text = paste0("omega.b.sim <- with(data.frame(simdata), ", paste0("(", paste0(load.b.par, collapse = " + "), ")^2"), " / (", paste0("(", paste0(load.b.par, collapse = " + "), ")^2"), " + ", paste0(resid.b.par, collapse = " + "), " + (", paste0(resid.w.par, collapse = " + "), " + 2*(", paste0(rescov.w.par, collapse = " + "), ")) / hmean)", ")")))

           # Result table
           omega <- data.frame(type = "omega.b", items = length(lavaan::lavNames(model.fit$model.fit)), omega = omega.b,
                               low = quantile(omega.b.sim, probs = (1L - conf.level) / 2L),
                               upp = quantile(omega.b.sim, probs = 1L - (1L - conf.level) / 2L), row.names = NULL)

         #...................
         ### Configural Construct ####
         }, config = {

           # No residual covariances at the Within level
           if (isTRUE(length(rescov.w.par) == 0L)) {

             # Omega Within
             eval(parse(text = paste0("omega.w.sim <- with(data.frame(simdata), ", paste0("(", paste0(load.w.par, collapse = " + "), ")^2"), " / (", paste0("(", paste0(load.w.par, collapse = " + "), ")^2"), " + ", paste0(resid.w.par, collapse = " + "), "))")))

             # Omega Between
             eval(parse(text = paste0("omega.b.sim <- with(data.frame(simdata), ", paste0("((", paste0(load.b.par, collapse = " + "), ")^2 * ", var.b.par, ") / (", paste0("(", paste0(load.b.par, collapse = " + "), ")^2"), " * (1 / ", hmean, " + ", var.b.par, ") + ", paste0(resid.b.par, collapse = " + "), " + ((", paste0(resid.w.par, collapse = " + "), ") / hmean)", "))"))))

             # Overall Omega
             eval(parse(text = paste0("omega.2l.sim <- with(data.frame(simdata), ", paste0("((", paste0(load.b.par, collapse = " + "), ")^2L * (1L + var.b)) / ((", paste0(load.b.par, collapse = " + "), ")^2 * (1L + var.b) + ", paste0(resid.b.par, collapse = " + "), " + ",  paste0(resid.w.par, collapse = " + "), "))"))))

           } else {

             # Omega Within
             eval(parse(text = paste0("omega.w.sim <- with(data.frame(simdata), ", paste0("(", paste0(load.w.par, collapse = " + "), ")^2"), " / (", paste0("(", paste0(load.w.par, collapse = " + "), ")^2"), " + ", paste0(resid.w.par, collapse = " + "), " + 2*( ", paste0(rescov.w.par, collapse = " + "), ")))")))

             # Omega Between
             eval(parse(text = paste0("omega.b.sim <- with(data.frame(simdata), ", paste0("((", paste0(load.b.par, collapse = " + "), ")^2 * ", var.b.par, ") / (", paste0("(", paste0(load.b.par, collapse = " + "), ")^2"), " * (1 / ", hmean, " + ", var.b.par, ") + ", paste0(resid.b.par, collapse = " + "), " + (", paste0(resid.w.par, collapse = " + "), " + 2*(", paste0(rescov.w.par, collapse = " + "), ")) / hmean)", ")"))))

             # Overall Omega
             eval(parse(text = paste0("omega.2l.sim <- with(data.frame(simdata), ", paste0("((", paste0(load.b.par, collapse = " + "), ")^2L * (1L + var.b)) / ((", paste0(load.b.par, collapse = " + "), ")^2 * (1L + var.b) + ", paste0(resid.b.par, collapse = " + "), " + ",  paste0(resid.w.par, collapse = " + "), " + 2L*(", paste0(rescov.w.par, collapse = " + "), ")))"))))

           }

           # Result table
           omega <- data.frame(type = c("omega.w", "omega.b", "omega.2l"),
                               items = length(lavaan::lavNames(model.fit$model.fit)),
                               omega = c(omega.w, omega.b, omega.2l),
                               low = c(quantile(omega.w.sim, probs = (1L - conf.level) / 2L),
                                       quantile(omega.b.sim, probs = (1L - conf.level) / 2L),
                                       quantile(omega.2l.sim, probs = (1L - conf.level) / 2L)),
                               upp = c(quantile(omega.w.sim, probs = 1L - (1L - conf.level) / 2L),
                                       quantile(omega.b.sim, probs = 1L - (1L - conf.level) / 2L),
                                       quantile(omega.2l.sim, probs = 1L - (1L - conf.level) / 2L)), row.names = NULL)

         })

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Sample Statistics ####

  # Descriptive statistics and Intraclass Correlation Coefficient, ICC(1)
  switch(const,
         within = {

           itemstat <- data.frame(model.fit$result$descript, wstd.ld = na.omit(param.w[param.w$param == "latent variable", "stdyx"]), fix.empty.names = FALSE)

         }, shared = {

           itemstat <- data.frame(model.fit$result$descript, bstd.ld = na.omit(param.b[param.b$param == "latent variable", "stdyx"]), fix.empty.names = FALSE)

         }, config = {

           itemstat <- data.frame(model.fit$result$descript, wstd.ld = na.omit(param.w[param.w$param == "latent variable", "stdyx"]), bstd.ld = na.omit(param.b[param.b$param == "latent variable", "stdyx"]), fix.empty.names = FALSE)

         })

  #_____________________________________________________________________________
  #
  # Return object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "multilevel.omega",
                 data = x,
                 args = list(rescov = rescov, const = const, fix.resid = fix.resid,
                             optim.method = optim.method, missing = missing,
                             nrep = nrep, seed = seed, conf.level = conf.level,
                             print = print, digits = digits, as.na = as.na,
                             write = write, append = append, check = check, output = output),
                 model = model.fit$model,
                 model.fit = model.fit$model.fit,
                 result = list(omega = omega, itemstat = itemstat))

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
