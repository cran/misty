#' Mplus Summary Measures, Convergence and Efficiency Diagnostics
#'
#' This function uses the \code{h5file} function in the \pkg{hdf5r} package to
#' read a Mplus GH5 file that is requested by the command \code{PLOT: TYPE IS PLOT2}
#' in Mplus to compute point estimates (i.e., mean, median, and MAP), measures of
#' dispersion (i.e., standard deviation and mean absolute deviation), measures of
#' shape (i.e., skewness and kurtosis), credible intervals (i.e., equal-tailed
#' intervals and highest density interval), convergence and efficiency diagnostics
#' (i.e., potential scale reduction factor R-hat, effective sample size, and Monte
#' Carlo standard error), probability of direction, and probability of being in
#' the region of practical equivalence for the posterior distribution for each
#' parameter. By default, the function computes the maximum of rank-normalized
#' split-R-hat and rank normalized folded-split-R-hat, Bulk effective sample size
#' (Bulk-ESS) for rank-normalized values using split chains, tail effective sample
#' size (Tail-ESS) defined as the minimum of the effective sample size for 0.025
#' and 0.975 quantiles, the Bulk Monte Carlo standard error (Bulk-MCSE) for the
#' median and Tail Monte Carlo standard error (Tail-MCSE) defined as the maximum
#' of the MCSE for 0.025 and 0.975 quantiles.
#'
#' @param x           a character string indicating the name of the Mplus GH5 file
#'                    (HDF5 format) with or without the file extension \code{.gh5},
#'                    e.g., \code{"Mplus_Plot.gh5"} or \code{"Mplus_Plot"}.
#' @param print       a character vector indicating which summary measures,
#'                    convergence, and efficiency diagnostics to be printed on
#'                    the console, i.e. \code{"all"} for all summary measures,
#'                    convergence, and efficiency diagnostics, \code{"m"} for the
#'                    mean, \code{"med"} for the median, \code{"MAP"} for the
#'                    maximum a posteriori probability estimate, \code{"med"}
#'                    for the standard deviation, \code{"mad"} for the mean
#'                    absolute deviation, \code{"skew"} for the skewness,
#'                    \code{"kurt"} for the kurtosis, \code{"eti"} for the
#'                    equal-tailed credible interval, \code{"hdi"} for the
#'                    highest density credible interval, \code{"rhat"} for the
#'                    potential scale reduction (PSR) factor R-hat convergence
#'                    diagnostic, \code{"b.ess"} for the bulk effective sample
#'                    size (ESS), \code{"t.ess"} for the tail ESS, \code{"b.mcse"}
#'                    for the bulk Monte Carlo standard error (MCSE), and
#'                    \code{"t.mcse"} for the tail MCSE. The default setting is
#'                    \code{print = c("med", "sd", "skew", "kurt", "eti", "rhat", "b.ess", "t.ess", "b.mcse", "t.mcse")}.
#' @param param       character vector indicating which parameters to print
#'                    for the summary measures, convergence, and efficiency
#'                    diagnostics, i.e., \code{"all"} for all parameters,
#'                    \code{"on"} (default), for regression slopes, \code{"by"}
#'                    for factor loadings, \code{"with"} for covariances,
#'                    \code{"inter"} for intercepts and thresholds, \code{"var"}
#'                    for (residual) variances, \code{"r2"} for r-square, and
#'                    \code{"new"} for parameters not in the analysis model
#'                    specified in the \code{NEW} option. The default setting
#'                    is \code{"on"} if regression slopes are available. Otherwise,
#'                    the default setting switches to \code{"by"} and to
#'                    \code{"with"} if factor loadings are not available.
#' @param std         a character vector indicating the standardized
#'                    parameters to print for the summary measures, convergence,
#'                    and efficiency diagnostics, i.e., \code{"all"} for all
#'                    standardized parameters, \code{"none"} (default) for not
#'                    printing any standardized parameters, \code{"stdyx"} for
#'                    StdYX standardized parameters, \code{"stdy"} for StdY
#'                    standardized parameters, and \code{"std"} for StdX
#'                    standardized parameters.
#' @param m.bulk      logical: if \code{TRUE} the Monte Carlo standard error
#'                    for the mean is computed. The default setting is
#'                    \code{m.bulk = FALSE}, i.e., the Monte Carlo standard error
#'                    for the median is computed.
#' @param split       logical: if \code{TRUE} (default), each MCMC chain is split
#'                    in half before computing R-hat. Note that the argument
#'                    \code{split} is always set to \code{FALSE} when computing
#'                    ESS.
#' @param rank        logical: if \code{TRUE} (default), rank-normalization is
#'                    applied to the posterior draws before computing R-hat and
#'                    ESS. Note that the argument \code{rank} is always set to
#'                    \code{FALSE} when computing MCSE.
#' @param fold        logical: if \code{TRUE} (default), the maximum of
#'                    rank-normalized split-R-hat and rank normalized folded-split-R-hat
#'                    is computed. Note that the arguments \code{split} and
#'                    \code{rank} are always set to \code{TRUE} when specifying
#'                    \code{fold = TRUE}.
#' @param pd          logical: if \code{TRUE}, the probability of direction is
#'                    printed on the console.
#' @param null        a numeric value considered as a null effect for the probability
#'                    of direction (default is \code{0}). Note that the value
#'                    specified in the argument \code{null} applies to al
#'                    parameters
#'                    which might not be sensible for all parameters.
#' @param rope        a numeric vector with two elements indicating the ROPE's
#'                    lower and upper bounds. ROPE is also depending on the argument
#'                    \code{alternative}, e.g., if \code{rope = c(-0.1, 0.1)},
#'                    then the actual ROPE is \code{[-0.1, 0.1]} given
#'                    \code{alternative = "two.sided} (default), \code{[-Inf, 0.1]}
#'                    given \code{alternative = "greater}, and \code{[-0.1, Inf]}
#'                    given \code{alternative = "less"}. Note that the interval
#'                    specified in the argument \code{rope} applies to all parameters
#'                    which might not be sensible for all parameters.
#' @param ess.tail    a numeric vector with two elements to specify the quantiles
#'                    for computing the tail ESS. The default setting is
#'                    \code{tail = c(0.025, 0.975)}, i.e., tail ESS is the minimum
#'                    of effective sample sizes for 0.025 and 0.975 quantiles.
#' @param mcse.tail   a numeric vector with two elements to specify the quantiles
#'                    for computing the tail MCSE. The default setting is
#'                    \code{tail = c(0.025, 0.975)}, i.e., tail MCSE is the maximum
#'                    of Monte Carlo standard error for 0.025 and 0.975 quantiles.
#' @param alternative a character string specifying the alternative hypothesis
#'                    for the credible intervals, must be one of \code{"two.sided"}
#'                    (default), \code{"greater"} or \code{"less"}.
#' @param conf.level  a numeric value between 0 and 1 indicating the confidence
#'                    level of the credible interval. The default setting is
#'                    \code{conf.level = 0.95}.
#' @param digits      an integer value indicating the number of decimal places
#'                    to be used for displaying point estimates, measures of
#'                    dispersion, and credible intervals.
#' @param r.digits    an integer value indicating the number of decimal places
#'                    to be used for displaying R-hat values.
#' @param ess.digits  an integer value indicating the number of decimal places
#'                    to be used for displaying effective sample sizes.
#' @param mcse.digits an integer value indicating the number of decimal places
#'                    to be used for displaying Monte Carlo standard errors.
#' @param p.digits    an integer value indicating the number of decimal places
#'                    to be used for displaying the probability of direction
#'                    and the probability of being in the region of practical
#'                    equivalence (ROPE).
#' @param write       a character string naming a file for writing the output into
#'                    either a text file with file extension \code{".txt"} (e.g.,
#'                    \code{"Output.txt"}) or Excel file with file extension
#'                    \code{".xlsx"}  (e.g., \code{"Output.xlsx"}). If the file
#'                    name does not contain any file extension, an Excel file will
#'                    be written.
#' @param append      logical: if \code{TRUE} (default), output will be appended
#'                    to an existing text file with extension \code{.txt} specified
#'                    in \code{write}, if \code{FALSE} existing text file will be
#'                    overwritten.
#' @param check       logical: if \code{TRUE} (default), argument specification is
#'                    checked.
#' @param output      logical: if \code{TRUE} (default), output is shown on the
#'                    console by using the function \code{mplus.print()}.
#'
#' @details
#' \describe{
#' \item{\strong{Convergence and Efficiency Diagnostics for Markov Chains}}{
#' Convergence and efficiency diagnostics for Markov chains is based on following
#' numeric measures:
#'    \itemize{
#'      \item{\strong{Potential Scale Reduction (PSR) factor R-hat:}}{ The PSR factor
#'      R-hat compares the between- and within-chain variance for a model
#'      parameter, i.e., R-hat larger than 1 indicates that the between-chain
#'      variance is greater than the within-chain variance and chains have not
#'      mixed well. According to the default setting, the function computes the
#'      improved R-hat as recommended by Vehtari et al. (2020) based on rank-normalizing
#'      (i.e., \code{rank = TRUE}) and folding (i.e., \code{fold = TRUE}) the
#'      posterior draws after splitting each MCMC chain in half (i.e.,
#'      \code{split = TRUE}). The traditional R-hat used in Mplus can be requested
#'      by specifying \code{split = FALSE}, \code{rank = FALSE}, and
#'      \code{fold = FALSE}. Note that the traditional R-hat can catch many
#'      problems of poor convergence, but fails if the chains have different
#'      variances with the same mean parameter or if the chains have infinite
#'      variance with one of the chains having a different location parameter to
#'      the others (Vehtari et al., 2020). According to Gelman et al. (2014) a
#'      R-hat value of 1.1 or smaller for all parameters can be considered evidence
#'      for convergence. The Stan Development Team (2024) recommends running at
#'      least four chains and a convergence criterion of less than 1.05 for the
#'      maximum of rank normalized split-R-hat and rank normalized folded-split-R-hat.
#'      Vehtari et al. (2020), however, recommended to only use the posterior
#'      samples if R-hat is less than 1.01 because the R-hat can fall below 1.1
#'      well before convergence in some scenarios (Brooks & Gelman, 1998; Vats &
#'       Knudon, 2018).}
#'      \item{\strong{Effective Sample Size (ESS):}}{ The ESS is the estimated number
#'      of independent samples from the posterior distribution that would lead
#'      to the same precision as the autocorrelated samples at hand. According
#'      to the default setting, the function computes the ESS based on rank-normalized
#'      split-R-hat and within-chain autocorrelation. The function provides the
#'      estimated Bulk-ESS (\code{B.ESS}) and the Tail-ESS (\code{T.ESS}). The
#'      Bulk-ESS is a useful measure for sampling efficiency in the bulk of the
#'      distribution (i.e, efficiency of the posterior mean), and the Tail-ESS
#'      is useful measure for sampling efficiency in the tails of the distribution
#'      (e.g., efficiency of tail quantile estimates). Note that by default, the
#'      Tail-ESS is the minimum of the effective sample sizes for 5% and 95%
#'      quantiles (\code{tail = c(0.025, 0.975)}). According to Kruschke (2015),
#'      a rank-normalized ESS greater than 400 is usually sufficient to get a
#'      stable estimate of the Monte Carlo standard error. However, a ESS of
#'      at least 1000 is considered optimal (Zitzmann & Hecht, 2019).}
#'      \item{\strong{Monte Carlo Standard Error (MCSE):}}{ The MCSE is defined as
#'      the standard deviation of the chains divided by their effective sample
#'      size and reflects uncertainty due to the stochastic algorithm of the
#'      Markov Chain Monte Carlo method. The function provides the estimated
#'      Bulk-MCSE (\code{B.MCSE}) for the margin of error when using the MCMC
#'      samples to estimate the posterior mean and the Tail-ESS (\code{T.MCSE})
#'      for the margin of error when using the MCMC samples for interval
#'      estimation.}
#'    }
#' }
#' }
#'
#' @author
#' Takuya Yanagida
#'
#' @seealso
#' \code{\link{read.mplus}}, \code{\link{write.mplus}}, \code{\link{mplus}},
#' \code{\link{mplus.update}}, \code{\link{mplus.print}}, \code{\link{mplus.plot}},
#' \code{\link{mplus.run}}, \code{\link{mplus.lca}}
#'
#' @references
#' Brooks, S. P. and Gelman, A. (1998). General Methods for Monitoring Convergence
#' of Iterative Simulations. \emph{Journal of Computational and Graphical Statistics, 7}(4):
#' 434–455. MR1665662.
#'
#' Gelman, A., & Rubin, D.B. (1992). Inference from iterative simulation using
#' multiple sequences. \emph{Statistical Science, 7}, 457-472.
#' https://doi.org/10.1214/ss/1177011136
#'
#' Kruschke, J. (2015). \emph{Doing Bayesian data analysis: A tutorial with R, JAGS, and Stan}.
#' Academic Press.
#'
#' Makowski, D., Ben-Shachar, M., & Lüdecke, D. (2019). bayestestR: Describing
#' effects and their uncertainty, existence and significance within the Bayesian
#' framework. \emph{Journal of Open Source Software, 4}(40), 1541.
#' https://doi.org/10.21105/joss.01541
#'
#' Stan Development Team (2024). \emph{RStan: the R interface to Stan}. R package
#' version 2.32.6. https://mc-stan.org/.
#'
#' Vats, D. and Knudson, C. (2018). Revisiting the Gelman-Rubin Diagnostic.
#' arXiv:1812.09384.
#'
#' Vehtari, A., Gelman, A., Simpson, D., Carpenter, B., & Bürkner, P.-C. (2020).
#' Rank-normalization, folding, and localization: An improved R-hat for assessing
#' convergence of MCMC. \emph{Bayesian analysis, 16}(2), 667-718.
#' https://doi.org/110.1214/20-BA1221
#'
#' Zitzmann, S., & Hecht, M. (2019). Going beyond convergence in Bayesian estimation:
#' Why precision matters too and how to assess it. \emph{Structural Equation Modeling, 26}(4),
#' 646–661. https://doi.org/10.1080/10705511.2018.1545232
#'
#' @note
#' This function is a modified copy of functions provided in the \pkg{rstan}
#' package by Stan Development Team (2024) and \pkg{bayestestR} package by
#' Makowski et al. (2019)
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#'
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{x}}{Mplus GH5 file}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{data}}{three-dimensional array parameter x iteration x chain of
#'                    the posterior}
#' \item{\code{result}}{result table with summary measures, convergence, and
#'                      efficiency diagnostics}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #----------------------------------------------------------------------------
#' # Mplus Example 3.18: Moderated Mediation with a Plot of the Indirect Effect
#'
#' # Example 1: Default setting
#' mplus.bayes("ex3.18.gh5")
#'
#' # Example 2: Print all parameters
#' mplus.bayes("ex3.18.gh5", param = "all")
#'
#' # Example 3: Print parameters not in the analysis model
#' mplus.bayes("ex3.18.gh5", param = "new")
#'
#' # Example 4a: Print all summary measures, convergence, and efficiency diagnostics
#' mplus.bayes("ex3.18.gh5", print = "all")
#'
#' # Example 4a: Print default measures plus MAP
#' mplus.bayes("ex3.18.gh5", print = c("default", "map"))
#'
#' # Example 5: Print traditional R-hat in line with Mplus
#' mplus.bayes("ex3.18.gh5", split = FALSE, rank = FALSE, fold = FALSE)
#'
#' # Example 6: Print probability of direction and the probability of
#' # being ROPE [-0.1, 0.1]
#' mplus.bayes("ex3.18.gh5", pd = TRUE, rope = c(-0.1, 0.1))
#'
#' # Example 7: Write Results into a text file
#' mplus.bayes("ex3.18.gh5", write = "Bayes_Summary.txt")
#'
#' # Example 8b: Write Results into a Excel file
#' mplus.bayes("ex3.18.gh5", write = "Bayes_Summary.xlsx")
#' }
mplus.bayes <- function(x,
                        print = c("all", "default", "m", "med", "map", "sd", "mad", "skew", "kurt", "eti", "hdi", "rhat", "b.ess", "t.ess", "b.mcse", "t.mcse"),
                        param = c("all", "on", "by", "with", "inter", "var", "r2", "new"),
                        std = c("all", "none", "stdyx", "stdy", "std"),
                        m.bulk = FALSE, split = TRUE, rank = TRUE, fold = TRUE,
                        pd = FALSE, null = 0, rope = NULL,
                        ess.tail = c(0.025, 0.975), mcse.tail = c(0.025, 0.975),
                        alternative = c("two.sided", "less", "greater"), conf.level = 0.95,
                        digits = 2, r.digits = 3, ess.digits = 0, mcse.digits = 3, p.digits = 3,
                        write = NULL, append = TRUE, check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check input 'x'
  if (isTRUE(missing(x))) { stop("Please specify a character string indicating the name of a Mplus GH5 file for the argument 'x'", call. = FALSE) }

  # Check if input 'x' is NULL
  if (isTRUE(is.null(x))) { stop("Input specified for the argument 'x' is NULL.", call. = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Character string ####

  # Character string
  if (isTRUE(!is.character(x) || length(x) != 1L)) { stop("Please specify a character string indicating the name of a Mplus GH5 file for the argument 'x'", call. = FALSE) }

  # File extension .gh5
  x <- ifelse(isTRUE(!grepl(".gh5", x)), file <- paste0(x, ".gh5"), x)

  # Check if 'x' exists
  if (isTRUE(!file.exists(x))) { stop(paste0("Unable to read the Mplus GH5 file: ", sQuote(x), " does not exist."), call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  .check.input(logical = c("m.bulk", "split", "rank", "fold", "pd", "append", "output"),
               numeric = list(null = 1L, rope = 2L, ess.tail = 2L, mcse.tail = 2L),
               m.character = list(print = c("all", "default", "m", "med", "map", "sd", "mad", "skew", "kurt", "eti", "hdi", "rhat", "b.ess", "t.ess", "b.mcse", "t.mcse"),
                                  param = c("all", "on", "by", "with", "inter", "var", "r2", "new"),
                                  std = c("all", "none", "stdyx", "stdy", "std")),
               args = c("alternative", "conf.level", "digits", "r.digits", "ess.digits", "mcse.digits", "p.digits", "write2"),
               package = "hdf5r", envir = environment(), input.check = check)

  # Additional check
  if (isTRUE(check)) {

    # Check input 'rope'
    if (isTRUE(rope[2L] - rope[1L] < 0L)) { stop("Please specify the lower and higher bounds of the ROPE for the argument 'rope'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'print' Argument ####

  print.all <- c("m", "med", "map", "sd", "mad", "skew", "kurt", "eti", "hdi", "rhat", "b.ess", "t.ess", "b.mcse", "t.mcse")

  # Default setting
  if (isTRUE(all(c("all", "default", "m", "med", "map", "sd", "mad", "skew", "kurt", "eti", "hdi", "rhat", "b.ess", "t.ess", "b.mcse", "t.mcse") %in% print))) {

    print <- c("med", "sd", "skew", "kurt", "eti", "rhat", "b.ess", "t.ess", "b.mcse", "t.mcse")

  # All print commands
  } else if (isTRUE("all" %in% print)) {

    print <- print.all

  # Default setting with additional print commands
  } else if (isTRUE("default" %in% print && length(print > 1L))) {

    print <- print.all[print.all %in% misty::chr.omit(union(c("med", "sd", "skew", "kurt", "eti", "rhat", "b.ess", "t.ess", "b.mcse", "t.mcse"), print), "default", check = FALSE)]

  # Manual default setting
  } else if (isTRUE(all(print == "default"))) {

    print <- c("med", "sd", "skew", "kurt", "eti", "rhat", "b.ess", "t.ess", "b.mcse", "t.mcse")

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'param' Argument ####

  # Default setting
  if (isTRUE(all(c("all", "on", "by", "with", "inter", "var", "r2", "new") %in% param))) {

    param <- "on"

  # All input commands
  } else if (isTRUE("all" %in% param)) {

    param <- c("on", "by", "with", "inter", "var", "r2", "new")

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'std' Argument ####

  # Default setting
  if (isTRUE(all(c("all", "none", "stdyx", "stdy", "std") %in% std))) {

    std <- "none"

    # All input commands
  } else if (isTRUE("all" %in% std)) {

    std <- c("stdyx", "stdy", "std")

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'alternative' Argument ####

  if (isTRUE(all(c("two.sided", "less", "greater") %in% alternative))) { alternative  <- "two.sided" }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #----------------------------------------
  # Mplus GH5 File

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Read Mplus GH5 File ####

  gh5 <- tryCatch(hdf5r::h5file(x), error = function(y) { stop("Reading Mplus GH5 file using the h5file() function from the hdf5r package failed.", call. = FALSE) })

  # "bayesian_data" or "loop_data" section
  if (isTRUE(!"bayesian_data" %in% names(gh5))) { stop("There is no \"bayesian_data\" section in the Mplus GH5 file specified in the argument 'x'", call. = FALSE) }

  #...................
  ### Extract Posterior Data, Labels, and Dimensionality ####

  # Posterior
  post <- gh5[["bayesian_data/parameters_autocorr/parameters"]][, , ]

  if (isTRUE(all(post == 0L))) {  stop("Reading Mplus GH5 file using the h5file() function from the hdf5r package failed.", call. = FALSE) }

  # Parameter Labels
  labels <- gh5[["bayesian_data/parameters_autocorr/statements"]][] |>
    misty::chr.trim() |>
    misty::chr.gsub(pattern = c("^Parameter [0-9]+, ", "\\[ ", " \\]", "  "), replacement = c("", "\\[", "\\]", " "))

  # Dimensionality
  post.dim <- dim(post)

  # Number of parameters
  n.parameters <- post.dim[1L]

  # Number of iterations
  n.iterations <- post.dim[2L]

  # Number of chains
  n.chains <- post.dim[3L]

  #...................
  ### Select Parameters ####

  #### Default or User-Specified Setting Setting: ON ####
  if (isTRUE(all(param == "on"))) {

    # ON parameter not available
    if (isTRUE(all(!grepl(" ON ", labels)))) {

      param <- "by"

      # BY parameter not available
      if (isTRUE(all(!grepl(" BY ", labels)))) {

        param <- "with"

        # WITH parameter not available
        if (isTRUE(all(!grepl(" WITH ", labels)))) {

          stop("There are no 'ON', 'BY', or 'WITH' parameters available in the Mplus GH5 file.", call. = FALSE)

        } else {

          message("There are no 'ON' or 'BY' parameters available in the Mplus GH5 file, param argument switched to \"with\".")

        }

      } else {

        message("There are no 'ON' parameters available in the Mplus GH5 file, param argument switched to \"by\".")

      }

    }

  #### User-Specified Setting: BY ####
  } else if (isTRUE(all(param == "by") && all(!grepl(" BY ", labels)))) {

    param <- "with"

    # WITH parameter not available
    if (isTRUE(all(!grepl(" WITH ", labels)))) {

      stop("There are no 'BY', or 'WITH' parameters available in the Mplus GH5 file.", call. = FALSE)

    } else {

      message("There are no 'BY' parameters available in the Mplus GH5 file, param argument switched to \"with\".")

    }

  #### User-Specified Setting: WITH ####
  } else if (isTRUE(all(param == "with") && all(!grepl(" WITH ", labels)))) {

    stop("There are no 'WITH' parameters available in the Mplus GH5 file.", call. = FALSE)

  }

  #### Extract Variables involved in BY, WITH, ON, [, $, or # ####

  var <- unname(unique(unlist(sapply(misty::chr.grep(c(" BY ", " WITH ", " ON ", "\\[", "\\]", "\\$"), labels, value = TRUE), function(y) {

    strsplit(unlist(strsplit(unlist(strsplit(misty::chr.gsub(c("\\[", "\\]", paste0("#", 1L:20L), paste0("$", 1L:20L), paste0("&", 1L:20L)), rep("", times = 62L), y), " ON ")), " BY ")), " WITH ")

  }))))

  # % statement
  if (isTRUE(any(grepl("%", var)))) {

    var <- unique(misty::chr.trim(sub(":", "", apply(rbind(var, unlist(lapply(strsplit(var, ""), function(y) grep(":", y)[1L])), nchar(var)), 2L, function(z) if (isTRUE(!is.na(z[2L]))) { substr(z[1L], start = z[2L], z[3L]) } else { z[1L]} )))) |>
                                                     (\(x) misty::chr.grep(paste0(": ", x, " "), misty::chr.grep(paste0(": ", x), labels, value = TRUE, fixed = TRUE), invert = TRUE, value = TRUE))()

  }

  #### Select Parameters ####

  # Select ON, BY, WITH, intercept/threshold
  param.ind <- misty::chr.grep(misty::rec(misty::chr.omit(param, omit = "var", check = FALSE), spec = "'on' = ' ON '; 'by' = ' BY '; 'with' = ' WITH '; 'inter' = '['; 'r2' = 'R-SQUARE'"), labels, fixed = TRUE)

  # Select (residual) variance
  if (isTRUE("var" %in% param)) { param.ind <- c(param.ind, which(labels %in% intersect(misty::chr.grep(c(" ON ", " BY ", " WITH ", "[", "R-SQUARE"), labels, fixed = TRUE, invert = TRUE, value = TRUE), var))) }

  # Select additional parameter
  if (isTRUE("new" %in% param)) { param.ind <- c(param.ind, which(labels %in% setdiff(misty::chr.grep(c(" ON ", " BY ", " WITH ", "[", "R-SQUARE"), labels, fixed = TRUE, invert = TRUE, value = TRUE), var))) }

  # Exclude standardized
  if (isTRUE(std == "none")) {

    param.ind <- setdiff(param.ind, misty::chr.grep(c("STDYX,", "STDY,", "STD,"), labels, fixed = TRUE))

  } else {

    std.exclude <- setdiff(c("stdyx", "stdy", "std"), std)

    if (isTRUE(length(std.exclude) != 0L)) {

      param.ind <- setdiff(param.ind, misty::chr.grep(misty::rec(std.exclude, spec = "'stdyx' = 'STDYX,'; 'stdy' = 'STDY,'; 'std' = 'STD,'"), labels, fixed = TRUE))

    }

  }

  # No parameter selected
  if (isTRUE(length(param.ind) == 0L)) { stop("There are no parameters selected for the trace, posterior distribution, or autocorrelation plots.", call. = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Summary Measures, Convergence and Efficiency Diagnostics ####

  post.summary <- sapply(seq_len(n.parameters), function(y) {

    # Extract posterior
    x <- do.call(cbind, list(sapply(seq_len(n.chains), function(z) post[y, , z])))

    # Discard burnin
    x <- x[ceiling(n.iterations / 2L + 1L):n.iterations, ]

    # Combine chains
    x.comb <- as.vector(x)

    if (isTRUE(var(x.comb) != 0L)) {

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ## Point Estimates ####

      # Mean
      x.mean <- mean(x.comb)

      # Median
      x.med <- median(x.comb)

      # Maximum A Posteriori
      x.map <- .map(x.comb)

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ## Measures of Dispersion ####

      # Standard Deviation
      x.sd <- sd(x.comb)

      # Mean Absolute Deviation
      x.mad <- stats::mad(x.comb)

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ## Measures of Shape ####

      # Skewness
      x.skew <- misty::skewness(x.comb, check = FALSE)

      # Kurtosis
      x.kurt <- misty::kurtosis(x.comb, check = FALSE)

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ## Credible Interval ####

      # Equal-Tailed Interval
      x.eti <- switch(alternative,
                      two.sided = quantile(x.comb, prob = c((1L - conf.level) / 2L, 1L - (1L - conf.level) / 2L)),
                      less = c(low = -Inf, upp = quantile(x.comb, prob = 1L - (1L - conf.level))),
                      greater = c(quantile(x.comb, prob = 1L - conf.level), upp = Inf))

      # Highest Density Interval
      x.hdi <- .hdi(x.comb, conf.level = conf.level)

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ## Convergence and Efficiency Diagnostics ####

      # R-hat Convergence Diagnostic
      if (isTRUE(fold)) {

        # Maximum of Rank normalized (split-)R-hat and Rank normalized (split-)R-hat
        x.rhat <- max(.rhat(x, split = split, rank = rank, fold = FALSE), .rhat(x, split = split, rank = rank, fold = TRUE))

      } else {

        x.rhat <- .rhat(x, split = split, rank = rank, fold = FALSE)

      }

      # Bulk Effective Sample Size
      x.b.ess <- .ess(x, split = FALSE, rank = rank)

      # Tail Effective Sample Size
      x.t.ess <- min(.ess(x <= quantile(x, ess.tail[1L]), split = FALSE, rank = rank), .ess(x <= quantile(x, ess.tail[2L]), split = FALSE, rank = rank))

      # Bulk Monte Carlo Standard Error
      if (isTRUE(m.bulk)) {

        x.b.mcse <- .mcse(x, quant = FALSE, split = FALSE, rank = FALSE)

      } else {

        x.b.mcse <- .mcse(x, quant = TRUE, prob = 0.50, split = FALSE, rank = FALSE)

      }

      # Tail Monte Carlo Standard Error
      x.t.mcse <- max(.mcse(x, quant = TRUE, prob = mcse.tail[1L], split = FALSE, rank = FALSE),
                      .mcse(x, quant = TRUE, prob = mcse.tail[2L], split = FALSE, rank = FALSE))

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ## Probability of Direction ####

      x.pd <- max(sum(x.comb < null) / length(x.comb), sum(x.comb > null) / length(x.comb))

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ## Probability of being in the ROPE ####

      if (isTRUE(!is.null(rope))) {

        x.rope <- switch(alternative,
                         two.sided = sum(x.comb >= rope[1L] & x.comb <= rope[2L]) / length(x.comb),
                         less = sum(x.comb >= rope[1L]) / length(x.comb),
                         greater = sum(x.comb <= rope[2L]) / length(x.comb))

      } else {

        x.rope <- NA

      }

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ## Result Table ####

      result.table <- data.frame(m = x.mean, med = x.med, map = x.map, sd = x.sd, mad = x.mad,
                                 skew = x.skew, kurt = x.kurt,
                                 eti.low = x.eti[1L], eti.upp = x.eti[2L], hdi.low = x.hdi[1L], hdi.upp = x.hdi[2L],
                                 rhat = x.rhat, b.ess = x.b.ess, t.ess = x.t.ess, b.mcse = x.b.mcse, t.mcse = x.t.mcse,
                                 pd = x.pd, rope = x.rope)

    # Posterior with zero variance
    } else {

      result.table <- data.frame(m = mean(x.comb), med = median(x.comb), map = mean(x.comb), sd = sd(x.comb), mad = mad(x.comb),
                                 skew = NA, kurt = NA, eti.low = NA, eti.upp = NA, hdi.low = NA, hdi.upp = NA,
                                 rhat = NA, b.ess = NA, t.ess = NA, b.mcse = NA, t.mcse = NA, pd = NA, rope = NA)
    }

    return(result.table)

  })

  # Merge with labels
  result.table <- data.frame(param = labels, apply(t(post.summary), 2L, unlist))[param.ind, ]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Close Mplus GH5 File ####

  hdf5r::h5close(gh5)

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "mplus.bayes",
                 x = gh5,
                 args = list(print = print, param = param, std = std, param.ind = param.ind,
                             m.bulk = m.bulk, split = split, rank = rank, fold = fold,
                             pd = pd, null = null, rope = rope,
                             ess.tail = ess.tail, mcse.tail = mcse.tail,
                             conf.level = conf.level, alternative = alternative,
                             digits = digits, r.digits = r.digits, ess.digits = ess.digits,
                             mcse.digits = mcse.digits, p.digits = p.digits,
                             write = write, append = append, check = check, output = output),
                 data = post,
                 result = result.table)

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
