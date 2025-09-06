#' Blimp Summary Measures, Convergence and Efficiency Diagnostics
#'
#' This function reads the posterior distribution for all parameters saved in
#' long format in a file called \code{posterior.*} by the function \code{blimp.run}
#' or \code{blimp} when specifying \code{posterior = TRUE} to compute point estimates
#' (i.e., mean, median, and MAP), measures of dispersion (i.e., standard deviation
#' and mean absolute deviation), measures of shape (i.e., skewness and kurtosis),
#' credible intervals (i.e., equal-tailed intervals and highest density interval),
#' convergence and efficiency diagnostics (i.e., potential scale reduction factor
#' R-hat, effective sample size, and Monte Carlo standard error), probability of
#' direction, and probability of being in the region of practical equivalence for
#' the posterior distribution for each parameter. By default, the function computes
#' the maximum of rank-normalized split-R-hat and rank normalized folded-split-R-hat,
#' Bulk effective sample size (Bulk-ESS) for rank-normalized values using split
#' chains, tail effective sample size (Tail-ESS) defined as the minimum of the
#' effective sample size for 0.025 and 0.975 quantiles, the Bulk Monte Carlo
#' standard error (Bulk-MCSE) for the median and Tail Monte Carlo standard error
#' (Tail-MCSE) defined as the maximum of the MCSE for 0.025 and 0.975 quantiles.
#'
#' @param x           a character string indicating the name of folder containing
#'                    the \code{posterior.*} file, e.g., \code{"Posterior_Ex4.3"}
#'                    or the name of the \code{posterior.*} file with or without
#'                    any file extension, e.g., \code{"Posterior_ExEx4.3/posterior.csv"}
#'                    or \code{"Posterior_ExEx4.3/posterior"}. Alternatively, a
#'                    \code{misty.object} of type \code{blimp} can be specified,
#'                    i.e., result object of the \code{blimp.plot()} function.
#'                    Note that if the \code{posterior} file is specified without
#'                    file extension while multiple \code{posterior.*} files in
#'                    different file formats are available, then the file is read
#'                    in following order: \code{csv},\code{RData}, \code{rds},
#'                    and \code{xlsx}.
#' @param param       a numeric vector indicating which parameters to print.
#'                    Note that the number of the parameter (\code{Param}) and
#'                    the parameter specification (\code{L1}, \code{L2}, and
#'                    \code{L3}) are provided in the text file \code{"partable.txt"}.
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
#'                    specified in the argument \code{null} applies to all parameters
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
#'                    console by using the function \code{blimp.print()}.
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
#'      \code{split = TRUE}). The traditional R-hat used in Blimp can be requested
#'      by specifying \code{split = TRUE}, \code{rank = FALSE}, and \code{fold = FALSE}.
#'      Note that the traditional R-hat can catch many problems of poor convergence,
#'      but fails if the chains have different variances with the same mean parameter
#'      or if the chains have infinite variance with one of the chains having a
#'      different location parameter to the others (Vehtari et al., 2020). According
#'      to Gelman et al. (2014) a R-hat value of 1.1 or smaller for all parameters
#'      can be considered evidence for convergence. The Stan Development Team (2024)
#'      recommends running at least four chains and a convergence criterion of
#'      less than 1.05 for the maximum of rank normalized split-R-hat and rank
#'      normalized folded-split-R-hat. Vehtari et al. (2020), however, recommended
#'      to only use the posterior samples if R-hat is less than 1.01 because the
#'      R-hat can fall below 1.1 well before convergence in some scenarios
#'      (Brooks & Gelman, 1998; Vats & Knudon, 2018).}
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
#'      Tail-ESS is the minimum of the effective sample sizes for 2.5% and 97.5%
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
#' \code{\link{blimp}}, \code{\link{blimp.update}}, \code{\link{blimp.run}},
#' \code{\link{blimp.plot}},\code{\link{blimp.print}}, \code{\link{blimp.plot}},
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
#' Keller, B. T., & Enders, C. K. (2023). \emph{Blimp user’s guide} (Version 3).
#' Retrieved from www.appliedmissingdata.com/blimp
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
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#'
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{x}}{a character string indicating the name of the \code{posterior.*}}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{data}}{posterior distribution of each parameter estimate
#'                    in long format}
#' \item{\code{result}}{result table with summary measures, convergence, and
#'                      efficiency diagnostics}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #----------------------------------------------------------------------------
#' # Blimp Example 4.3: Linear Regression
#'
#' # Example 1a: Default setting, specifying name of the folder
#' blimp.bayes("Posterior_Ex4.3")
#'
#' # Example 1b: Default setting, specifying the posterior file
#' blimp.bayes("Posterior_Ex4.3/posterior.csv")
#'
#' # Example 2a: Print all summary measures, convergence, and efficiency diagnostics
#' blimp.bayes("Posterior_Ex4.3", print = "all")
#'
#' # Example 3a: Print default measures plus MAP
#' blimp.bayes("Posterior_Ex4.3", print = c("default", "map"))
#'
#' # Example 4: Print traditional R-hat in line with Blimp
#' blimp.bayes("Posterior_Ex4.3", split = TRUE, rank = FALSE, fold = FALSE)
#'
#' # Example 5: Print probability of direction and the probability of
#' # being ROPE [-0.1, 0.1]
#' blimp.bayes("Posterior_Ex4.3", pd = TRUE, rope = c(-0.1, 0.1))
#'
#' # Example 6: Write Results into a text file
#' blimp.bayes("Posterior_Ex4.3", write = "Bayes_Summary.txt")
#'
#' # Example 7b: Write Results into a Excel file
#' blimp.bayes("Posterior_Ex4.3", write = "Bayes_Summary.xlsx")
#' }
blimp.bayes <- function(x, param = NULL,
                        print = c("all", "default", "m", "med", "map", "sd", "mad", "skew", "kurt", "eti", "hdi", "rhat", "b.ess", "t.ess", "b.mcse", "t.mcse"),
                        m.bulk = FALSE, split = TRUE, rank = TRUE, fold = TRUE,
                        pd = FALSE, null = 0, rope = NULL,
                        ess.tail = c(0.025, 0.975), mcse.tail = c(0.025, 0.975),
                        alternative = c("two.sided", "less", "greater"), conf.level = 0.95,
                        digits = 2, r.digits = 3, ess.digits = 0, mcse.digits = 3, p.digits = 3,
                        write = NULL, append = TRUE, check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'x' is missing or NULL
  if (isTRUE(missing(x) || is.null(x))) { stop("Please specify a character string indicating the name of folder or name of the posterior data file for the argument 'x'", call. = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Character string ####

  # Character string
  if (isTRUE(!is.character(x) || length(x) != 1L)) { stop("Please specify a character string indicating the name of the folder or name of the posterior data file for the argument 'x'", call. = FALSE) }

  # Folder
  if (isTRUE(dir.exists(x))) {

    x <- list.files(x, pattern = "posterior.", full.names = TRUE) |>
      (\(z) if (isTRUE(length(z) == 0L)) { stop("There is no \"posterior\" file in the folder specified in 'x'.", call. = FALSE) } else { return(z[1L]) })()

  # Data file
  } else {

    # With file extension
    if (isTRUE(any(sapply(c(".csv", ".xlsx", ".rds", ".RData"), grepl, x)))) {

      if (isTRUE(!file.exists(x))) { stop(paste0("Unable to read the \"posterior\" file: ", sQuote(x), " does not exist."), call. = FALSE) }

    # No file extension provided
    } else {

      x <- which(sapply(c(".csv", ".xlsx", ".rds", ".RData"), function(y) file.exists(paste0(x, y)))) |>
        (\(z) if(length(z) == 0L) { stop(paste0("Unable to read the \"posterior\" file: ", sQuote(x), " does not exist."), call. = FALSE) } else { return(names(z)) } )() |>
        (\(w) paste0(x, w[1L]))()

    }

  }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("m.bulk", "split", "rank", "fold", "pd", "append", "output"), numeric = list(null = 1L, rope = 2L, ess.tail = 2L, mcse.tail = 2L),
               m.character = list(print = c("all", "default", "m", "med", "map", "sd", "mad", "skew", "kurt", "eti", "hdi", "rhat", "b.ess", "t.ess", "b.mcse", "t.mcse")),
               args = c("digits", "p.digits", "r.digits", "ess.digits", "mcse.digits", "conf.level", "alternative", "write2"), envir = environment(), input.check = check)

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
  ## 'alternative' Argument ####

  if (isTRUE(all(c("two.sided", "less", "greater") %in% alternative))) { alternative  <- "two.sided" }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Read Blimp Posterior File ####

  . <- posterior <- NULL

  tryCatch(

    switch(names(which(sapply(c(".csv", ".xlsx", ".rds", ".RData"), grepl, x))),
           # CSV format
           ".csv" =   {

             if (isTRUE(ncol(read.csv(x, nrows = 1L)) > 1L)) {

               postdat <- read.csv(x)

             } else {

               postdat <- utils::read.csv2(x)

             }

           },
           # Excel format
           ".xlsx" =  { postdat <- misty::read.xlsx(x) },
           # RDS format
           ".rds" =   { postdat <- readRDS(x) },
           # R workspace
           ".RData" = {

             load(x)

             postdat <- posterior

             rm(posterior)

           }),

    error = function(y) { stop("Reading posterior file specified in the argument 'x' failed.", call. = FALSE) })

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data Preparation ####

  #...................
  ### Number of Iterations and Number of Chains ####

  # Number of iterations
  n.burnin <- max(postdat[which(postdat$postburn == 0), "iter"])
  n.postburn <- max(postdat[which(postdat$postburn == 1), "iter"])

  # Number of chains
  n.chains <- max(postdat$chain)

  #...................
  ### Select Parameters ####

  if (isTRUE(!is.null(param))) { postdat <- postdat[which(postdat$param %in% param), ] }

  #...................
  ### Discard burn-in iterations ####

  postdat <- postdat[which(postdat$postburn != 0L), ]

  #...................
  ### Shorten Labels ####

  # Shorten labels
  postdat[, c("latent1", "latent2", "latent3")] <- sapply(postdat[, c("latent1", "latent2", "latent3")], function(y) {

    misty::rec(y, spec = "'Correlations' = 'Cor.'; 'Exponentiated Coefficients' = 'Exp. Coef'; 'Standardized Beta' = 'Std. Beta'; 'Coefficients' = 'Coef'; 'Dispersion Parameter' = 'Disp. Par.'; 'Heterogeneity Index' = 'Hetero. Index'; 'Level-1 Residual Variation' = 'L1 Resid. Var.'; 'Level-2 Random Intercepts' = 'L2 Rand. Inter.'; 'Level-3 Random Intercepts' = 'L3 Rand. Inter.'; 'Level-2 Random Slopes' = 'L2 Random Slope'; 'Level-3 Random Slopes' = 'L3 Random Slope'; 'Q25% Residual Var.' = 'Q25% Resid. Var.'; 'Residual Var.' = 'Resid. Var.'; 'Residual Variation' = 'Resid. Var.'; 'Yeo-Johnson (lambda)' = 'Yeo-Johnson'", check = FALSE)

  })

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Summary Measures, Convergence and Efficiency Diagnostics ####

  post.summary <- sapply(sort(unique(postdat$param)), function(y) {

    # Extract posterior in Wide format
    x <- do.call("cbind", split(postdat[postdat$param == y, "value"], postdat[postdat$param == y, "chain"]))

    # Extract posterior in Long format
    x.comb <- postdat[postdat$param == y, "value"]

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
      x.t.mcse <- max(.mcse(x, quant = TRUE, prob = mcse.tail[1L], split = split, rank = FALSE),
                      .mcse(x, quant = TRUE, prob = mcse.tail[2L], split = split, rank = FALSE))

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

      result.table <- data.frame(param = y, m = x.mean, med = x.med, map = x.map, sd = x.sd, mad = x.mad,
                                 skew = x.skew, kurt = x.kurt,
                                 eti.low = x.eti[1L], eti.upp = x.eti[2L], hdi.low = x.hdi[1L], hdi.upp = x.hdi[2L],
                                 rhat = x.rhat, b.ess = x.b.ess, t.ess = x.t.ess, b.mcse = x.b.mcse, t.mcse = x.t.mcse,
                                 pd = x.pd, rope = x.rope)

    # Posterior with zero variance
    } else {

      result.table <- data.frame(param = y, m = mean(x.comb), med = median(x.comb), map = mean(x.comb), sd = sd(x.comb), mad = mad(x.comb),
                                 skew = NA, kurt = NA, eti.low = NA, eti.upp = NA, hdi.low = NA, hdi.upp = NA,
                                 rhat = NA, b.ess = NA, t.ess = NA, b.mcse = NA, t.mcse = NA, pd = NA, rope = NA)
    }

    return(result.table)

  })

  # Merge with labels
  result.table <- merge(misty::df.unique(data = postdat[, c("param", "latent1", "latent2", "latent3")]), apply(t(post.summary), 2L, unlist), by = "param")

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "blimp.bayes",
                 x = x,
                 args = list(param = param, print = print, m.bulk = m.bulk,
                             split = split, rank = rank, fold = fold,
                             pd = pd, null = null, rope = rope,
                             ess.tail = ess.tail, mcse.tail = mcse.tail,
                             conf.level = conf.level, alternative = alternative,
                             digits = digits, r.digits = r.digits, ess.digits = ess.digits,
                             mcse.digits = mcse.digits, p.digits = p.digits,
                             write = write, append = append, check = check, output = output),
                 data = postdat, result = result.table)

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
