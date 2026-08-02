#' Multilevel Coefficient Alpha
#'
#' This function computes point estimate and Monte Carlo confidence interval for
#' the multilevel coefficient alpha defined by Lai (2021) by calling the \code{cfa}
#' function in the R package \pkg{lavaan}. By default, the function prints level-specific
#' multilevel coefficient alpha with 95% Monte Carlo confidence interval based on
#' Huber-White standard errors.
#'
#' @param data         a data frame. A saturated multilevel model is estimated to
#'                     obtain variances and covariances at the Within and Between
#'                     level. Note that the cluster variable specified in \code{cluster}
#'                     is excluded from \code{data} when specifying the argument
#'                     \code{cluster} using the variable name of the cluster variable.
#' @param ...          an expression indicating the variable names in \code{data},
#'                     e.g., \code{multilevel.alpha(dat, x1, x2, x3, cluster = "cluster")}.
#'                     Note that the operators \code{+}, \code{-}, \code{~}, \code{:},
#'                     \code{::}, and \code{!} can also be used to select variables,
#'                     see 'Details' in the \code{\link{df.subset}} function.
#' @param cluster      either a character string indicating the variable name of
#'                     the cluster variable in \code{data}, or a vector representing
#'                     the nested grouping structure (i.e., group or cluster variable).
#' @param se           a character string indicating the standard errors used for
#'                     computing Monte Carlo confidence intervals, i.e., \code{"none"}
#'                     for no standard errors, \code{"standard"} for conventional
#'                     standard error based on inverting the expected observed or
#'                     first.order information matrix, and \code{"robust.huber.white"}
#'                     for the 'MLR' (aka pseudo ML, Huber-White) approach. Note
#'                     that \code{se = "none"} saves computation time by not
#'                     computing standard errors and Monte Carlo confidence intervals.
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
#'                     \code{"all"} (default) for all results \code{"alpha"} for
#'                     the composite reliability alpha and \code{"item"} for item
#'                     statistics. Note that standardized factor loadings at the
#'                     within and between level printed when requesting \code{"item"}
#'                     are based on a multilevel CFA model with factor loadings
#'                     freely estimated across both levels.
#' @param digits       an integer value indicating the number of decimal places
#'                     to be used for displaying mean, standard deviation, minimum,
#'                     maximum, skewness, and kurtosis.
#' @param r.digits     an integer value indicating the number of decimal places
#'                     to be used for displaying multilevel coefficient alpha,
#'                     ICC(1), and standardized factor loadings.
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
#' @details
#' \describe{
#' This function computes point estimate and Monte Carlo confidence interval for
#' the multilevel coefficient alpha defined by Lai (2021) by calling the \code{cfa}
#' function in the R package \pkg{lavaan}. By default, the function prints level-specific
#' multilevel coefficient alpha with 95\% Monte Carlo confidence interval based on
#' Huber-White standard errors.
#' \details{
#'   In single-level data, coefficient \eqn{\alpha} (Cronbach, 1951) can be
#'   computed as
#'
#'   \deqn{\alpha = \frac{2p\Sigma_{k=2}^p\Sigma_{k^{\prime}=1}^{k-1}\sigma_{kk^{\prime}}}{(p - 1)\mathbf{1}^{\prime}\mathbf{\Sigma}\mathbf{1}}}
#'
#'   assuming that the covariance matrix of \eqn{p} items is \eqn{\Sigma} with
#'   elements \eqn{\sigma_{ij^{\prime}}}. Note that coefficients \eqn{\alpha} and
#'   \eqn{\omega} represent the same population quantities under unidimensionality
#'   and essential tau-equivalence. The multilevel extension of coefficient
#'   \eqn{\alpha}, however, does not require estimation of parameters of a factor
#'   model. Conseuqntly, its computation at the between-level remains the same
#'   regardless a composite is used to measure a configural construct or a shared
#'   construct.
#'   \describe{
#'
#'     \item{Within-Level Composite}{The reliability for an observed within-level
#'     composite, \eqn{Z^w_{ij}}, is
#'
#'     \deqn{\alpha^w = \frac{2p\Sigma_{k=2}^p\Sigma_{k^{\prime}=1}^{k-1}\sigma_{kk^{\prime}}^w}{(p - 1)\mathbf{1}^{\prime}\mathbf{\Sigma}^w\mathbf{1}}}
#'
#'     }
#'
#'     \item{Between-Level Composite}{The reliability for an observed between-level
#'       composite, \eqn{Z^b_{j}}, is
#'
#'     \deqn{\alpha^b = \frac{2p\Sigma_{k=2}^p\Sigma_{k^{\prime}=1}^{k-1}\sigma_{kk^{\prime}}^b}{(p - 1)[\mathbf{1}^{\prime}\mathbf{\Sigma}^b\mathbf{1} + \mathbf{1}^{\prime}\mathbf{\Sigma}^w\mathbf{1} / \tilde{n}]}}
#'
#'     }
#'
#'     \item{Overall Composite}{The reliability for an overall observed
#'     composite, \eqn{Z_{ij}}, capturing the population variance of both the
#'     within-level and the between-level components of the true score and the
#'     errors is
#'
#'     \deqn{\alpha^{2l} = \frac{2p\Sigma_{k=2}^p\Sigma_{k^{\prime}=1}^{k-1}(\sigma_{kk^{\prime}}^w + \sigma_{kk^{\prime}}^b)}{(p - 1)[\mathbf{1}^{\prime}\mathbf{\Sigma}^b\mathbf{1} + \mathbf{1}^{\prime}\mathbf{\Sigma}^w\mathbf{1}]}}
#'
#'     Note that \eqn{\alpha^{2l}} is simply the ratio of true variance to total
#'     variance of the observed scores. In practice, \eqn{\alpha^{2l}} is not the
#'     most interesting coefficient because researchers are mainly interested in
#'     distinguishing within-and between effects (Castro-Alvarez et al., 2026).}}
#' }
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{multilevel.omega}}, \code{\link{item.omega}}, \code{\link{multilevel.cfa}},
#' \code{\link{multilevel.fit}}, \code{\link{multilevel.invar}}, \code{\link{multilevel.cor}},
#' \code{\link{multilevel.descript}}, \code{\link{write.result}}
#'
#' @references
#' Cronbach, L. J. (1951). Coefficient alpha and the internal structure of tests.
#' \emph{Psychometrika, 16}, 297-334. http://dx.doi.org/10.1007/BF02310555
#'
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
#' \item{\code{data}}{data frame specified in \code{data} including the group variable
#'                    specified in \code{cluster}}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{model}}{specified model}
#' \item{\code{model.fit}}{fitted lavaan object (\code{mod.fit})}
#' \item{\code{check}}{results of the convergence and model identification check}
#' \item{\code{result}}{list with result tables, i.e., \code{alpha} for the coefficient
#'                      alpha including Monte Carlo confidence interval and
#'                      \code{itemstat} for descriptive statistics}
#'
#' @note
#' This function is based on the function \code{multilevel_alpha} from Mark Lai
#' (2021 supplementary codes) and  uses the functions \code{lavInspect}, \code{lavTech},
#' \code{lavNames}, \code{parameterEstimates}, and \code{sem} provided in the R
#' package \pkg{lavaan} by Yves Rosseel (2012). The internal function \code{.internal.mvrnorm}
#' is a copy of the \code{mvrnorm} function in the package \pkg{MASS} by Venables
#' and Ripley (2002).
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
#' multilevel.alpha(Demo.twolevel, y1:y4, cluster = "cluster")
#'
#' # Example 1b: Alternative specification with cluster variable 'cluster' in 'data'
#' multilevel.alpha(Demo.twolevel[, c("y1", "y2", "y3", "y4", "cluster")], cluster = "cluster")
#'
#' # Example 1c: Alternative specification with cluster variable 'cluster' not in 'data'
#' multilevel.alpha(Demo.twolevel[, c("y1", "y2", "y3", "y4")], cluster = Demo.twolevel$cluster)
#'
#' #————————————————————————————————————————————————————————————————————————————
#  # Argument 'se' and 'print'
#'
#' # Example 4a: No confidence intervals to speed up computation
#' multilevel.alpha(Demo.twolevel, y1:y4, cluster = "cluster", se = "none")
#'
#' # Example 4b: Alpha and item statistics
#' multilevel.alpha(Demo.twolevel, y1:y4, cluster = "cluster", print = "all")
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Write Results
#'
#' # Example 5a: Write results into a text file
#' multilevel.alpha(Demo.twolevel, y1, y2, y3, y4, cluster = "cluster",
#'                  write = "Multilevel_Alpha.txt")
#'
#' # Example 5b: Write results into a Excel file
#' multilevel.alpha(Demo.twolevel, y1, y2, y3, y4, cluster = "cluster",
#'                  write = "Multilevel_Alpha.xlsx")
#' }
multilevel.alpha <- function(data, ..., cluster, se = c("none", "standard", "robust.huber.white"),
                             optim.method = c("nlminb", "em"), missing = c("listwise", "fiml"),
                             nrep = 100000, seed = NULL, conf.level = 0.95,
                             print = c("all", "alpha", "item"), digits = 2,
                             r.digits = 3, as.na = NULL, write = NULL,
                             append = TRUE, check = TRUE, output = TRUE) {

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
    cluster <- data[, cluster] |> (\(y) if (isTRUE("tbl" %in% substr(class(y), 1L, 3L))) { unname(unlist(y)) } else { return(y) })()

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

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical =  c("append", "output"),
               s.character = list(se = c("none", "standard", "robust.huber.white"), optim.method = c("nlminb", "em"), missing = c("listwise", "fiml")),
               m.character = list(print = c("all", "alpha", "item")),
               args = c("conf.level", "digits", "r.digits", "nrep", "seed", "write2"), envir = environment(), input.check = check)

  #_____________________________________________________________________________
  #
  # Data and Arguments ---------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Manifest Variables ####

  var <- colnames(x)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data Frame with Cluster Variable ####

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
  ## Standard Error ####

  if (isTRUE(all(c("none", "standard", "robust.huber.white") %in% se))) { se <- "robust.huber.white" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Optimizer ####

  if (isTRUE(all(c("nlminb", "em") %in% optim.method))) { optim.method <- "nlminb" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Missing Data on All Variable ####

  (misty::na.prop(x[, var], append = FALSE) == 1L) |> (\(y) if (isTRUE(any(y) && missing == "fiml")) { warning(paste0("Data contains cases with missing values on all variables, number of cases removed from the analysis: ", sum(y)), call. = FALSE) })()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Print ####

  if (isTRUE(all(c("all", "alpha", "item") %in% print))) {

    print  <- "alpha"

  } else if (isTRUE(all(print == "all"))) {

    print <- c("alpha", "item")

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  alpha <- mod <- model.fit <- NULL
  if (isTRUE("alpha" %in% print)) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Model Specification ####

    mod <- paste("  # Within model\n",
                 " level: 1\n  ",
                 paste(apply(combn(length(var), 2L), 2L, function(y) paste(var[y[1L]], var[y[2L]], sep = " ~~ " )), collapse = "\n   "),
                 "\n\n  # Between model\n",
                 " level: 2\n  ",
                 paste(apply(combn(length(var), 2L), 2L, function(y) paste(var[y[1L]], var[y[2L]], sep = " ~~ " )), collapse = "\n   "))


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Model Estimation ####

    model.fit <- tryCatch(suppressWarnings(lavaan::sem(mod, data = x, cluster = ".cluster", estimator = "ML",
                                                       missing = missing, optim.method = optim.method, se = se, test = "none",
                                                       check.gradient = FALSE, check.post = FALSE, check.vcov = FALSE)),
                          error = function(y) {

                            if (isTRUE(missing == "fiml")) {

                              stop("There was an estimation problem in lavaan, switching to missing = \"listwise\" might solve the problem.", call. = FALSE)

                            } else {

                              stop("There was an estimation problem in lavaan, correlation matrix could not be computed.", call. = FALSE)

                            }})

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Convergence and Model Identification Checks ####

    if (isTRUE(check)) {

      #—————————————————————————————————————— #
      ### Model Convergence ####

      if (isTRUE(!lavaan::lavInspect(model.fit, what = "converged"))) { stop("CFA model did not converge.", call. = FALSE) }

      #—————————————————————————————————————— #
      ### Standard Error ####

      if (isTRUE(se != "none")) { if (isTRUE(any(is.na(unlist(lavaan::lavInspect(model.fit, what = "se")))))) { stop("Standard errors could not be computed.", call. = FALSE) } }

      #—————————————————————————————————————— #
      ### Variance-Covariance Matrix of the Estimated Parameters ####

      if (isTRUE(se != "none")) {

        eigvals <- eigen(lavaan::lavInspect(model.fit, what = "vcov"), symmetric = TRUE, only.values = TRUE)$values

        # Model contains equality constraints
        model.fit.par <- lavaan::parameterTable(model.fit)$op == "=="

        if (isTRUE(any(model.fit.par))) { eigvals <- rev(eigvals)[-seq_len(sum(model.fit.par))] }

        if (isTRUE(min(eigvals) < .Machine$double.eps^(3L/4L))) {

          warning("The variance-covariance matrix of the estimated parameters is not positive definite. This may be a symptom that the model is not identified.", call. = FALSE)

        }

      }

      #—————————————————————————————————————— #
      ### Negative Variance of Observed Variables ####

      #···················
      #### Within Level

      if (isTRUE(any(diag(lavaan::lavInspect(model.fit, what = "theta")$within) < 0L))) {

        warning("Some estimated variances of the observed variables at the Within level are negative.", call. = FALSE)

      } else if (isTRUE(any(eigen(lavaan::lavTech(model.fit, what = "theta")$within, symmetric = TRUE, only.values = TRUE)$values < (-1L * .Machine$double.eps^(3/4))))) {

        warning("The model-implied variance-covariance matrix of the residuals of the observed variables is not positive definite.", call. = FALSE)

      }

      #···················
      #### Between Level

      if (isTRUE(any(diag(lavaan::lavInspect(model.fit, what = "theta")$.cluster) < 0L))) {

        warning("Some estimated variances of the observed variables at the Between level are negative.", call. = FALSE)

      } else if (isTRUE(any(eigen(lavaan::lavTech(model.fit, what = "theta")$.cluster, symmetric = TRUE, only.values = TRUE)$values < (-1L * .Machine$double.eps^(3/4))))) {

        warning("The model-implied variance-covariance matrix of the residuals of the observed variables at the Between level is not positive definite.", call. = FALSE)

      }

      #—————————————————————————————————————— #
      ### Negative Variance of Latent Variables ####

      #···················
      #### Within Level

      # Negative variance estimates
      if (isTRUE(!is.null(lavaan::lavTech(model.fit, what = "cov.lv")$within))) {

        if (isTRUE(any(diag(lavaan::lavTech(model.fit, what = "cov.lv")$within) < 0L))) {

          warning("Some estimated variances of the latent variables at the Within level are negative.", call. = FALSE)

        }

      # Model-implied variance-covariance matrix of the latent variables
      } else if (any(dim(lavaan::lavTech(model.fit, what = "cov.lv")$within) != 0L)) {

        if (isTRUE(any(eigen(lavaan::lavTech(model.fit, what = "cov.lv")$within, symmetric = TRUE, only.values = TRUE)$values < (-1L * .Machine$double.eps^(3/4))))) {

          warning("The model-implied variance-covariance matrix of the latent variables at the Within level is not positive definite.", call. = FALSE)

        }

      }

      #···················
      #### Between Level

      # Negative variance estimates
      if (isTRUE(!is.null(lavaan::lavTech(model.fit, what = "cov.lv")$cluster))) {

        if (isTRUE(any(diag(lavaan::lavTech(model.fit, what = "cov.lv")$.cluster) < 0L))) {

          warning("Some estimated variances of the latent variables at the Between level are negative.", call. = FALSE)

        }

      # Model-implied variance-covariance matrix of the latent variables
      } else if (any(dim(lavaan::lavTech(model.fit, what = "cov.lv")$.cluster) != 0L)) {

        if (isTRUE(any(eigen(lavaan::lavTech(model.fit, what = "cov.lv")$.cluster, symmetric = TRUE, only.values = TRUE)$values < (-1L * .Machine$double.eps^(3/4))))) {

          warning("The model-implied variance-covariance matrix of the latent variables at the Between level is not positive definite.", call. = FALSE)

        }

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Multilevel Reliability ####

    # Parameter estimates
    param <- lavaan::parameterEstimates(model.fit) |> (\(p) p[p$op == "~~", ])()

    #—————————————————————————————————————— #
    ### Within ####

    # Parameter estimates
    param.w <- param[param$level == 1L, ]

    # Covariances
    covar.w <- param.w[param.w$lhs != param.w$rhs, "est"]

    # Variances
    var.w <- param.w[param.w$lhs == param.w$rhs, "est"]

    # Total variance
    var.w.tot <- sum(var.w, 2L*covar.w)

    #—————————————————————————————————————— #
    ### Between ####

    # Parameter estimates
    param.b <- param[param$level == 2L, ]

    # Covariances
    covar.b <- param.b[param.b$lhs != param.b$rhs, "est"]

    # Variances
    var.b <- param.b[param.b$lhs == param.b$rhs, "est"]

    # Total variance
    var.b.tot <- sum(var.b, 2L*covar.b)

    #—————————————————————————————————————— #
    ### Scaling Correction ####

    scaling <- lavaan::lavNames(model.fit) |> (\(p) length(p) / (length(p) - 1L))()

    #—————————————————————————————————————— #
    ### Harmonic mean ####

    hmean <- length(unique(x$.cluster)) / sum(1L / table(x$.cluster) |> (\(p) p[p > 0L])())

    #—————————————————————————————————————— #
    ### Level-Specific Alphas ####

    # Alpha Within
    alpha.w <- scaling * sum(2L * covar.w) / var.w.tot

    # Alpha Between
    alpha.b <- scaling * sum(2L * covar.b) / (var.b.tot + (var.w.tot / hmean))

    # Alpha Overall
    alpha.2l <- scaling * sum(2L * c(covar.w, covar.b)) / (var.w.tot + var.b.tot)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Monte Carlo Confidence Intervals ####

    #—————————————————————————————————————— #
    ### Standard Errors and Confidence Intervals ####

    alpha.2l.sim <- alpha.b.sim <- alpha.w.sim <- NULL
    if (isTRUE(se != "none")) {

      # Parameter estimates
      fit.est <- lavaan::coef(model.fit) |> (\(p) p[grep("~~", names(p))])()

      # Variance-covariance matrix
      fit.vcov <- lavaan::lavInspect(model.fit, what = "vcov") |> (\(p) grep("~~", colnames(p)) |> (\(q) p[q, q])())()

      # Set seed
      if (isTRUE(!is.null(seed))) { set.seed(seed) }

      # Simulate from a multivariate normal distribution
      simdata <- .internal.mvrnorm(nrep, mu = fit.est, Sigma = fit.vcov)

      #···················
      #### Variable Names ####

      # Variances
      vnam.var.w <- names(fit.est)[sapply(strsplit(names(fit.est), "~~"), function(y) y[1L] == sub(".l2", "", y[2L])) & !grepl(".l2", names(fit.est))]
      vnam.var.b <- names(fit.est)[sapply(strsplit(names(fit.est), "~~"), function(y) y[1L] == sub(".l2", "", y[2L])) & grepl(".l2", names(fit.est))]

      # Covariances
      vnam.covar.w <- names(fit.est)[sapply(strsplit(names(fit.est), "~~"), function(y) y[1L] != sub(".l2", "", y[2L])) & !grepl(".l2", names(fit.est))]
      vnam.covar.b <- names(fit.est)[sapply(strsplit(names(fit.est), "~~"), function(y) y[1L] != sub(".l2", "", y[2L])) & grepl(".l2", names(fit.est))]

      #···················
      #### Total Variance ####

      # Within
      var.w.tot <- rowSums(cbind(simdata[, vnam.var.w], 2L * simdata[, vnam.covar.w]))

      # Between
      var.b.tot <- rowSums(cbind(simdata[, vnam.var.b], 2L * simdata[, vnam.covar.b]))

      #···················
      #### Level-Specific Alphas ####

      alpha.w.sim <- scaling * rowSums(2 * simdata[ , vnam.covar.w]) / var.w.tot
      alpha.b.sim <- scaling * rowSums(2 * simdata[ , vnam.covar.b]) / (var.b.tot + var.w.tot / hmean)
      alpha.2l.sim <- scaling * rowSums(2 * simdata[ , c(vnam.covar.w, vnam.covar.b)]) / (var.b.tot + var.w.tot)

      #···················
      #### Result Table ####

      alpha <- data.frame(type = c("alpha.w", "alpha.b", "alpha.2l"),
                          n.items = length(lavaan::lavNames(model.fit)),
                          alpha = c(alpha.w, alpha.b, alpha.2l),
                          low = c(quantile(alpha.w.sim, probs = (1L - conf.level) / 2L),
                                  quantile(alpha.b.sim, probs = (1L - conf.level) / 2L),
                                  quantile(alpha.2l.sim, probs = (1L - conf.level) / 2L)),
                          upp = c(quantile(alpha.w.sim, probs = 1L - (1L - conf.level) / 2L),
                                  quantile(alpha.b.sim, probs = 1L - (1L - conf.level) / 2L),
                                  quantile(alpha.2l.sim, probs = 1L - (1L - conf.level) / 2L)), row.names = NULL)

    #—————————————————————————————————————— #
    ### No Standard Errors and Confidence Intervals ####

    } else {

      #···················
      #### Result Table ####

      alpha <- data.frame(type = c("alpha.w", "alpha.b", "alpha.2l"), n.items = length(lavaan::lavNames(model.fit)), alpha = c(alpha.w, alpha.b, alpha.2l), row.names = NULL)

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Descriptive Statistics and Intraclass Correlation Coefficient, ICC(1) ####

  itemstat <- NULL
  if (isTRUE("item" %in% print)) {

    #—————————————————————————————————————— #
    ### Estimate Multilevel CFA Model without Cross-Level Measurement Invariance ####

    model.fit.descript <- tryCatch(suppressWarnings(misty::multilevel.cfa(x, cluster = ".cluster", model = NULL, ident = "var", estimator = "ML",
                                                                          model.w = setdiff(colnames(x), ".cluster"),
                                                                          model.b = setdiff(colnames(x), ".cluster"),
                                                                          optim.method = optim.method, test = "none", se = "none",
                                                                          print = c("descript", "est"), missing = missing, output = FALSE, check = FALSE)),
                                   error = function(y) {

                                     warning("Item statistics could not be computed.", call. = FALSE)

                                   })

    #—————————————————————————————————————— #
    ### Convergence and Model Identification Checks ####

    if (isTRUE(check)) {

      #···················
      #### Model Convergence

      if (isTRUE(!lavaan::lavInspect(model.fit.descript$model.fit, what = "converged"))) { stop("CFA model did not converge.", call. = FALSE) }

      #···················
      #### Negative Variance of Observed Variables

      ##### Within Level ####

      if (isTRUE(any(diag(lavaan::lavInspect(model.fit.descript$model.fit, what = "theta")$within) < 0L))) {

        warning("Some estimated variances of the observed variables at the Within level are negative.", call. = FALSE)

      } else if (isTRUE(any(eigen(lavaan::lavTech(model.fit.descript$model.fit, what = "theta")$within, symmetric = TRUE, only.values = TRUE)$values < (-1L * .Machine$double.eps^(3/4))))) {

        warning("The model-implied variance-covariance matrix of the residuals of the observed variables is not positive definite.", call. = FALSE)

      }

      ##### Between Level ####

      if (isTRUE(any(diag(lavaan::lavInspect(model.fit.descript$model.fit, what = "theta")$.cluster) < 0L))) {

        warning("Some estimated variances of the observed variables at the Between level are negative.", call. = FALSE)

      } else if (isTRUE(any(eigen(lavaan::lavTech(model.fit.descript$model.fit, what = "theta")$.cluster, symmetric = TRUE, only.values = TRUE)$values < (-1L * .Machine$double.eps^(3/4))))) {

        warning("The model-implied variance-covariance matrix of the residuals of the observed variables at the Between level is not positive definite.", call. = FALSE)

      }

      #···················
      #### Negative Variance of Latent Variables

      ##### Within Level ####

      # Negative variance estimates
      if (isTRUE(!is.null(lavaan::lavTech(model.fit.descript$model.fit, what = "cov.lv")$within))) {

        if (isTRUE(any(diag(lavaan::lavTech(model.fit.descript$model.fit, what = "cov.lv")$within) < 0L))) {

          warning("Some estimated variances of the latent variables at the Within level are negative.", call. = FALSE)

        }

      # Model-implied variance-covariance matrix of the latent variables
      } else if (any(dim(lavaan::lavTech(model.fit.descript$model.fit, what = "cov.lv")$within) != 0L)) {

        if (isTRUE(any(eigen(lavaan::lavTech(model.fit.descript$model.fit, what = "cov.lv")$within, symmetric = TRUE, only.values = TRUE)$values < (-1L * .Machine$double.eps^(3/4))))) {

          warning("The model-implied variance-covariance matrix of the latent variables at the Within level is not positive definite.", call. = FALSE)

        }

      }

      ##### Between Level ####

      # Negative variance estimates
      if (isTRUE(!is.null(lavaan::lavTech(model.fit.descript$model.fit, what = "cov.lv")$cluster))) {

        if (isTRUE(any(diag(lavaan::lavTech(model.fit.descript$model.fit, what = "cov.lv")$.cluster) < 0L))) {

          warning("Some estimated variances of the latent variables at the Between level are negative.", call. = FALSE)

        }

      # Model-implied variance-covariance matrix of the latent variables
      } else if (any(dim(lavaan::lavTech(model.fit.descript$model.fit, what = "cov.lv")$.cluster) != 0L)) {

        if (isTRUE(any(eigen(lavaan::lavTech(model.fit.descript$model.fit, what = "cov.lv")$.cluster, symmetric = TRUE, only.values = TRUE)$values < (-1L * .Machine$double.eps^(3/4))))) {

          warning("The model-implied variance-covariance matrix of the latent variables at the Between level is not positive definite.", call. = FALSE)

        }

      }

    }

    #—————————————————————————————————————— #
    ### Result Table ####

    itemstat <- data.frame(misty::df.rename(model.fit.descript$result$descript, from = "variable", to = "item"),
                           wstd.ld = na.omit(model.fit.descript$result$param$within |> (\(p) p[p$param == "latent variable", "stdyx"])()),
                           bstd.ld = na.omit(model.fit.descript$result$param$between |> (\(p) p[p$param == "latent variable", "stdyx"])()),
                           fix.empty.names = FALSE)

  }


  #_____________________________________________________________________________
  #
  # Return object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "multilevel.alpha",
                 data = x,
                 args = list(se = se, optim.method = optim.method, missing = missing, nrep = nrep, seed = seed, conf.level = conf.level, print = print, digits = digits, r.digits = r.digits, as.na = as.na, write = write, append = append, check = check, output = output),
                 model = mod,
                 model.fit = model.fit,
                 result = list(alpha = alpha, itemstat = itemstat))

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
