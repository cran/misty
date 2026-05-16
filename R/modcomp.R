#' Model Comparison
#'
#' This function performs model comparison by providing a table with fit indices
#' for lavaan model objects, information criteria, and F-tests or likelihood ratio
#' tests for models estimated by the function \code{cfa()}, \code{sem()}, \code{growth()},
#' \code{lavaan()} from the \pkg{lavaan} package, \code{lm()}, \code{glm()}, \code{nls()}
#' from the \pkg{stats} package, \code{lmer()}, \code{glmer()}, \code{glmer.nb()}
#' from the \pkg{lme4} package, \code{lme()}, \code{nlme()} from the \pkg{nlme}
#' package, \code{glmmTMB()} from the \pkg{glmmTMB} package, \code{betareg} from
#' the \pkg{betareg} package, or \code{glm.nb()} and \code{polr()} from the \pkg{MASS}
#' package. By default, the function provides the fit indices CFI, TLI, RMSEA,
#' and SRMR for lavaan model objects and the information criteria AIC, CAIC, BIC,
#' and SABIC.
#'
#' @param ...        a fitted model object or sequence of fitted model objects of
#'                   class \code{"lavaan"}, \code{"lm"}, \code{"glm"}, \code{"nls"},
#'                   \code{"lmerMod"}, \code{"lmerModLmerTest"}, \code{"glmerMod"},
#'                   \code{"lme"}, \code{"nlme"}, \code{"glmmTMB"}, \code{"betareg"},
#'                   \code{"negbin"}, or \code{"polr"}.
#' @param difftest   logical: if \code{TRUE}, results of the F-test, chi-square
#'                   difference test, or likelihood ratio test are printed on the
#'                   console. Note that the function does not provide difference
#'                   tests for models fitted by using the \code{betareg()} function
#'                   from the \pkg{betareg} package.
#' @param print.fit  a character vector indicating which fit indices to be printed
#'                   on the console when specifying lavaan objects for the argument
#'                   \code{...}, i.e., \code{"none"} for no fit indices,
#'                   \code{"deviance"} for the deviance (i.e., log-likelihood
#'                   multiplied by -2), \code{"chisq"} for the chi-square value,
#'                   \code{"cfi"} for the comparative fit index, \code{"tli"} for
#'                   the Tucker-Lewis-index, \code{"rmsea"} for the root mean
#'                   square error of approximation, and \code{"srmr"} for the
#'                   standardized root mean squared residual. By default, all fit
#'                   indices are printed on the console.
#' @param fit.robust a character string indicating which version of the CFI, TLI,
#'                   and RMSEA to show on the console when using a robust estimation
#'                   method involving a scaling correction factor for model estimation
#'                   in lavaan, i.e., \code{"standard"} (default when \code{estimator}
#'                   is one of \code{"ML", "MLF", "GLS", "WLS", "DWLS"}, or \code{"ULS"})
#'                   for fit indices without any non-normality correction,
#'                   \code{"scaled"} (default when \code{estimator} is one of
#'                   \code{"MLMVS", "ULSM", "ULSMV", "DLS"}, or \code{"PML"})
#'                   for population-corrected robust fit indices with ad hoc
#'                   non-normality correction, and \code{robust} (default when
#'                   \code{estimator} is one of \code{"MLM", "MLMV", "MLR", "WLSM"},
#'                   or \code{"WLSMV"}) for sample-corrected robust fit indices
#'                   based on formula provided by Li and Bentler (2006) and
#'                   Brosseau-Liard and Savalei (2014).
#' @param print.ic   a character vector indicating which information criteria to
#'                   be printed on the console, i.e., \code{"all"} for printing
#'                   all information criteria, \code{"default"} for printing
#'                   the default set of information criteria (i.e., AIC, CAIC,
#'                   BIC, and SABIC), \code{"none"} for no information criteria,
#'                   \code{"aic"} for the Akaike information criterion (AIC),
#'                   \code{"caic"} for the Consistent Akaike's information criterion
#'                   (CAIC), \code{"bic"} for the Bayesian information criterion
#'                   (BIC), \code{"sabic"} for the sample-size adjusted BIC (SABIC),
#'                   \code{"aicc"} for the corrected Akaike information criterion
#'                   (AICc), \code{"hqc"} for the Hannan–Quinn criterion (HQC),
#'                   \code{"hbic"} for the Haughton’s BIC (HBIC), \code{"spbic"}
#'                   for the scaled unit-information prior BIC (SPBIC), \code{"ibic"}
#'                   for the information-matrix-based BIC, \code{"sic"} for the
#'                   stochastic information criterion (SIC), and \code{"icomp"}
#'                   for the Bozdogan information complexity (ICOMP) criterion.
#'                   The default setting is \code{print.ic = c("aic", "caic", "bic", "sabic)}
#'                   when specifying more than one model object for the argument
#'                   \code{...}, otherwise the default setting is \code{print.ic = "none}.
#'                   Note that \code{"spbic"}, \code{"ibic"}, \code{"sic"}, and
#'                   \code{"icomp"} are only available for lavaan model objects.
#' @param fit.digits an integer value indicating the number of decimal places to
#'                   be used for displaying fit indices when comparing lavaan
#'                   models.
#' @param ic.digits  an integer value indicating the number of decimal places to
#'                   be used for displaying information criteria.
#' @param p.digits   an integer value indicating the number of decimal places to
#'                   be used for displaying the \emph{p}-value in the F-test or
#'                   chi-square difference test.
#' @param write      a character string naming a file for writing the output into
#'                   either a text file with file extension \code{".txt"} (e.g.,
#'                   \code{"Output.txt"}) or Excel file with file extension
#'                   \code{".xlsx"}  (e.g., \code{"Output.xlsx"}). If the file
#'                   name does not contain any file extension, an Excel file will
#'                   be written.
#' @param append     logical: if \code{TRUE} (default), output will be appended
#'                   to an existing text file with extension \code{.txt} specified
#'                   in \code{write}, if \code{FALSE} existing text file will be
#'                   overwritten.
#' @param check      logical: if \code{TRUE} (default), argument specification is checked.
#' @param output     logical: if \code{TRUE} (default), output is shown on the console.
#'
#' @details
#' \describe{
#' \item{\strong{Information Criteria}}{Information criteria are statistical measures
#' that attempt to balance model fit and model complexity to compare competing models
#' for model selection. Most information criteria are based on the log-likelihood
#' with a penalty for complexity, and typically have the following form (Preacher
#' & Yaremych, 2023):
#'
#' \deqn{D + f(q, N)}
#'
#' where \eqn{D} is a function of the model's log-likelihood at convergence, whereas
#' \eqn{f} is a function of the number estimated parameters (\eqn{q}) and the
#' sample size (\eqn{N}).
#'
#' \itemize{
#'    \item{The \strong{Akaike Information Criterion}} (AIC; Akaike, 1973) is
#'    defined as
#'
#'      \deqn{\textrm{AIC} = -2LL + 2q}
#'
#'    The AIC is an efficient information criterion, i.e., it will asymptotically
#'    choose whichever model minimizes the mean square error of prediction
#'    (Vrieze, 2012). However, the AIC is not consistent, i.e., it is expected
#'    to pick different models at different \eqn{N}'s (Kuha, 2004). Accordingly,
#'    AIC is expected to select more complex models as \eqn{N} increases, while
#'    in relatively small samples, the penalty for complexity has a greater
#'    influence and simpler models are selected (Preacher & Yaremych, 2023).
#'
#'    \item{The \strong{Consistent Akaike Information Criterion}} (CAIC; Bozdogan,
#'    1987) is defined as
#'
#'    \deqn{\textrm{CAIC} = -2LL + q(ln(N) + 1)}
#'
#'    The CAIC modifies the standard AIC to be asymptotically consistent, i.e.,
#'    it is expected to pick the true model as sample size increases. However, the
#'    CAIC is not considered an efficient information criterion. Compared to the
#'    BIC, the CAIC has a higher penalty for model complexity making it more
#'    consistent but also less efficient than the BIC.
#'
#'    \item{The \strong{Bayesian Information Criterion}} (BIC; Schwarz, 1978)
#'    is defined as
#'
#'    \deqn{\textrm{BIC} = -2LL + q\cdot ln(N)}
#'
#'    The BIC is a consistent information criterion, i.e., it will select the true
#'    model with probability approach 1 as \eqn{N} increases based on the assumption
#'    that (a) the true model is under consideration, (b) the true model's dimension
#'    remains fixed as \eqn{N} increases, and (c) the number of parameters in the
#'    true model is finite (Vrieze, 2012). Accordingly, BIC tends to select more
#'    parsimonious models than AIC and is less subject to choosing more complex
#'    models as \eqn{N} increase because the penalty term increases with \eqn{N}.
#'
#'    \item{The \strong{Sample-Size Adjusted Bayesian Information Criterion}}
#'    (SABIC; Sclove, 1987) is defined as
#'
#'      \deqn{\textrm{SABIC} = -2LL + q\cdot ln(\frac{N + 2}{24})}
#'
#'    The SABIC is a variant of the BIC that reduces the penalty for complex
#'    models and seems to perfom bettern than BIC when the sample size is small
#'    to moderate (Chen et al., 2017).
#'
#'    \item{The \strong{Corrected Akaike Information Criterion}} (AICc; Burnham
#'    & Anderson, 2003) is defined as
#'
#'      \deqn{\textrm{AICc} = AIC + \frac{2q(q + 1)}{N - q - 1}}
#'
#'    The AICc is a corrected version of the AIC for small sample sizes or when
#'    the number of parameters is large relative to the sample size. Note that
#'    as the sample size increases the AICc converges to the standard AIC.
#'
#'    \item{The \strong{Hannan–Quinn Criterion}} (HQC; Hannan & Quinn,
#'    1979) is defined as
#'
#'      \deqn{\textrm{HQC} = -2LL + 2q\log{(\log{N})}}
#'
#'    The HQC imposes a penalty that is stronger than AIC but weaker than BIC in
#'    large sample as the penalty function decreases with increasing sample size
#'    and is often used to select the order of autoregressive processes.
#'
#'    \item{The \strong{Haughton Bayesian Information Criterion}} (HBIC; Haughton,
#'    1988) is defined as
#'
#'      \deqn{ \textrm{HBIC} = -2LL + q\log{\frac{N}{2\pi}}}
#'
#'    The HBIC performed well in model selection in simulation studies for structural
#'    equation models and had the best overall performance among the investigated
#'    information criteria along with the SPBIC (Haughton et al., 1997; Bollen et
#'    al., 2014).
#'
#'    \item{The \strong{Scaled Unit-Information Prior Bayesian Information Criterion}}
#'    (SPBIC; Bollen et al., 2012) is defined as
#'
#'      \deqn{\textrm{SPBIC}_{\textrm{Case 1}} = -2LL + q(1 - \frac{q}{\hat{\theta}^{'} \textrm{FIM} \hat{\theta}}), \textrm{or}}
#'      \deqn{\textrm{SPBIC}_{\textrm{Case 2}} = -2LL + \hat{\theta}^{'} \textrm{FIM} \hat{\theta},}
#'
#'    depending on whether the product of the vector of estimated model parameters
#'    (\eqn{\hat{\theta}}) and the observed information matrix (FIM) exceeds the number
#'    of estimated parameters (Case 1) or not (Case 2). The SPBIC performed well
#'    in model selection in a simulation study for structural equation models,
#'    had the best overall performance among the investigated information criteria
#'    along with the HBIC (Bollen et al., 2014), and exhibited a better performance
#'    along with the IBIC than BIC and HBIC when the sample size was small (Bollen
#'    et al., 2012).
#'
#'    \item{The \strong{Information Matrix-Based Bayesian Information Criterion}}
#'    (IBIC; Bollen et al., 2014) is defined as
#'
#'      \deqn{\textrm{IBIC} = -2LL + q\log{\frac{N}{2 \pi}} + \log{\det{\textrm{FIM}}}}
#'
#'    The IBIC performed well in model selection in a simulation study for structural
#'    equation models (Bollen et al., 2014) and exhibited a better performance
#'    along with the SPBIC than BIC and HBIC when the sample size was small (Bollen
#'    et al., 2012).
#'
#'    \item{The \strong{Stochastic Information Criterion}} (SIC; Rissanen, 1989)
#'    is defined as
#'
#'      \deqn{\textrm{SIC} = -2LL + q\log{N} + \log{\det{\textrm{FIM}}} = -2LL - \log{\det{\textrm{ACOV}}}}
#'
#'    The SIC performed well relative to other information criteria in two simulation
#'    studies of structural equation models applied to behavior genetic models
#'    (Markon & Krueger, 2004).
#'
#'    \item{The \strong{Information Complexity Criterion}} (ICOMP; Bozdogan & Haughton,
#'    1988) is defined as
#'
#'      \deqn{\textrm{ICOMP} = -2LL + 2C(\hat{\Sigma}_{Model})}
#'
#'    where \eqn{C} represents a complexity measure and \eqn{\hat{\Sigma}_{Model}}
#'    represents the estimated covariance matrix of the parameter vector estimated
#'    by the model, i.e., inverse Fisher information matrix (see Akman, 2010).
#'    The ICOMP penalizes the covariance complexity of the model instead of the
#'    number of estimated parameters.
#' }
#' In practice, it may be sensible to choose information criteria that emphasize
#' \emph{consistency} or \emph{efficiency} as consistency and efficiency cannot
#' be maximized simultaneously (Claeskens & Hjort, 2008). More specifically, a
#' criterion that emphasizes \emph{efficiency} such as AIC should be used when
#' prediction or cross-validation is important, whereas a criterion that emphasizes
#' \emph{consistency} such as BIC should be used when we want to identify a model
#' that best approximates the truth. Note that there is no such thing as a correct
#' model, there are models that cross-validate better than others, and there are
#' models that better reflect the true data-generating process (Preacher & Yaremych,
#' 2023).
#' }
#' }
#'
#' @author
#' Takuya Yanagida
#'
#' @references
#' Akaike, H. (1973). Information theory and an extension of the maximum likelihood
#' principle. In B. N. Petrov & B. F. Csaki (Eds.), \emph{Second International
#' Symposium on Information Theory}, (pp. 267-281). Academiai Kiado.
#'
#' Akman, O. (2010). Information complexity based modeling in the presence of
#' length-biased sampling. \emph{Journal of Statistical Theory and Practice, 4}(1),
#' 45-55. https://doi.org/10.1080/15598608.2010.10411972
#'
#' Bollen, K. A., Harden, J. J., Ray, S., & Zavisca, J. (2014). BIC and alternative
#' Bayesian information criteria in the selection of structural equation models.
#' \emph{Structural Equation Modeling: A Multidisciplinary Journal, 21}(1), 1–19.
#' https://doi.org/10.1080/10705511.2014.856691
#'
#' Bollen, K. A., Ray, S., Zavisca, J., & Harden, J. J. (2012). A comparison of
#' Bayes factor approximation methods including two new methods. \emph{Sociological
#' Methods & Research, 41}(2), 294-324. https://doi.org/10.1177/00491241124523
#'
#' Burnham, K., & Anderson, D. (2003). \emph{Model selection and multimodel inference:
#' A practical–theoretic approach}. Springer.
#'
#' Brosseau-Liard, P. E., & Savalei, V. (2014) Adjusting incremental fit indices
#' for nonnormality. \emph{Multivariate Behavioral Research, 49}, 460-470.
#' https://doi.org/10.1080/00273171.2014.933697
#'
#' Chen, Q., Luo, W., Palardy, G. J., Glaman, R., & McEnturff, A. (2017). The
#' efficacy of common fit indices for enumerating classes in growth mixture models
#' when nested data structure is ignored: A Monte Carlo study. \emph{SAGE Open, 7}(1).
#' https://doi:10.1177/2158244017700459
#'
#' Claeskins, G., & Hjort, N. L. (2008). \emph{Model selection and model averaging}.
#' Cambridge University Press.
#'
#' Hannan, E.J. and Quinn, B.G. (1979) The determination of the order of an
#' autoregression. \emph{Journal of the Royal Statistical Society, 41}, 190-195.
#'
#' Haughton, D. M. A. (1988). On the choice of a model to fit data from an exponential
#' family. \emph{The Annals of Statistics, 16}(1), 342-355.
#'
#' Haughton, D., Oud, J., & Jansen, R. (1997). Information and other criteria in
#' structural equation model selection. \emph{Communications in Statistics, Part B -
#' Simulation and Computation, 26}(4), 1477-1516.
#'
#' Kuha, J. (2004). AIC and BIC: Comparisons of assumptions and performance.
#' \emph{Sociological Methods & Research, 33}, 188-229.
#'
#' Li, L., & Bentler, P. M. (2006). Robust statistical tests for evaluating the
#' hypothesis of close fit of misspecified mean and covariance structural models.
#' \emph{UCLA Statistics Preprint #506}. University of California.
#'
#' Markon, K. E., & Krueger, R. F. (2004). An empirical comparison of information-theoretic
#' selection criteria for multivariate behavior genetic models. \emph{Behavior Genetics, 34},
#' 593-610.
#'
#' Preacher, K. K., & Yaremych, H. E. (2023). Model selection in structural equation
#' modeling. In R. H. Hoyle (Ed.), \emph{Handbook of structural equation modeling}
#' (2nd ed., pp. 206-222). The Guilford Press.
#'
#' Rissanen, J. (1989). \emph{Stochastic complexity in statistical inquiry}. World
#' Scientific.
#'
#' Schwarz, G. (1978). Estimating the dimension of a model. \emph{The Annals of Statistics,
#' 6}(2), 461-464.
#'
#' Sclove, L. (1987). Application of model-selection criteria to some problems
#' in multivariate analysis. \emph{Psychometrika, 52}(3), 333-343.
#'
#' Jorgensen, T. D., Pornprasertmanit, S., Schoemann, A. M., & Rosseel, Y. (2025).
#' semTools: Useful tools for structural equation modeling. R package version 0.5-7.
#' Retrieved from https://CRAN.R-project.org/package=semTools
#'
#' Vrieze, S.I. (2012) Model selection and psychological theory: A discussion of
#' the differences between the Akaike Information Criterion (AIC) and the Bayesian
#' Information Criterion (BIC). \emph{Psychological Methods, 17}, 228-243.
#' https://doi.org/10.1037/a0027127
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#'
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{class}}{object class of the models specified in the argument \code{...}}
#' \item{\code{model}}{models specified in the argument \code{...}}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{result}}{result table}
#'
#' @note
#' The computation of AICc, HQC, HBIC, SPBIC, IBIC, SIC, and ICOMP are based on
#' the \code{moreFitIndices} function from the \pkg{semTools} package by Terrence
#' D. Jorgensen, Sunthud Pornprasertmanit, Alexander M. Schoemann, and Yves Rosseel.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #————————————————————————————————————————————————————————————————————————————
#' # lavaan Model Objects
#'
#' # Load lavaan package
#' library(lavaan)
#'
#' # Model specification
#' HS.model <- 'visual =~ x1 + b1*x2 + x3
#'              textual =~ x4 + b2*x5 + x6
#'              speed =~ x7 + b3*x8 + x9'
#'
#' # Model estimation
#' fit1 <- cfa(HS.model, data = HolzingerSwineford1939)
#' fit2 <- cfa(HS.model, data = HolzingerSwineford1939, orthogonal = TRUE)
#'
#' # Example 1a: Model comparison, default setting
#' modcomp(fit1, fit2)
#'
#' # Example 1b: Model comparison, request likelihood ratio test
#' modcomp(fit1, fit2, difftest = TRUE)
#'
#' # Example 1c: Model comparison, request default information criteria and AICc
#' modcomp(fit1, fit2, print.ic = c("default", "aicc"))
#'
#' # Example 1d: Model comparison, request all information criteria
#' modcomp(fit1, fit2, print.ic = "all")
#'
#' # Example 1e: Model fit indices, request all information criteria
#' modcomp(fit1, print.ic = "all")
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # lm Model Objects
#'
#' # Model estimation
#' fit1 <- lm(mpg ~ cyl, data = mtcars)
#' fit2 <- lm(mpg ~ cyl + disp, data = mtcars)
#'
#' # Example 2: Model comparison, requested F test
#' modcomp(fit1, fit2, difftest = TRUE)
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Write Results
#'
#' # Example 3a: Write Results into a text file
#' modcomp(fit1, fit2, difftest = TRUE, write = "Model_Comparison.txt")
#'
#' # Example 3b: Write Results into a Excel file
#' modcomp(fit1, fit2, difftest = TRUE, write = "Model_Comparison.xlsx")
#' }
modcomp <- function(..., difftest = FALSE, print.fit = c("none", "deviance", "chisq", "cfi", "tli", "rmsea", "srmr"),
                    fit.robust = c("standard", "scaled", "robust"),
                    print.ic = c("all", "default", "none", "aic", "caic", "bic", "sabic", "aicc", "hqc", "hbic", "spbic", "ibic", "sic", "icomp"),
                    fit.digits = 3, ic.digits = 0, p.digits = 3, write = NULL, append = TRUE, check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Extract list of models
  mod <- list(...) |> (\(p) p[sapply(p, function(y) !is.null(y))])()

  # Extract object names
  mod.names <- sapply(match.call(expand.dots = FALSE)$..., as.character)

  # Extract object class
  mod.class <- unique(unlist(lapply(mod, function(y) class(y)[1L])))

  # Check if models are all NULL
  if (isTRUE(all(unlist(lapply(mod, is.null))))) { stop("Models specified in the argument '...' are all NULL.", call. = FALSE) }

  # Check if models specified in the input '...' have the same object class
  if (isTRUE(length(mod.class != 1L))) { stop("Please specify models of the same object class for the argument '...'.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  .check.input(logical = c("difftest", "output"), m.character = list(print.fit = c("none", "deviance", "chisq", "cfi", "tli", "rmsea", "srmr"), print.ic = c("all", "default", "none" ,"aic", "caic", "bic", "sabic", "aicc", "hqc", "hbic", "spbic", "ibic", "sic", "icomp")),
                s.character = list(fit.robust = c("standard", "scaled", "robust")), args = c("fit.digits", "ic.digits", "p.digits", "write2"), envir = environment(), input.check = check)

  # Additional checks
  if (isTRUE(check)) {

    # lavaan Object
    if (isTRUE(mod.class == "lavaan")) {

      # Check if package is installed
      if (isTRUE(!"lavaan" %in% row.names(installed.packages()))) { stop(paste0("Package \"lavaan\" is needed for this function to work, please install it."), call. = FALSE) }

      # Estimation method
      if (isTRUE(misty::uniq.n(sapply(mod, function(y) lavaan::inspect(y, what = "options")$estimator.orig)) != 1L)) { stop("Please specify model objects that used the same estimation method for the argument '...'.", call. = FALSE) }

    }

    # Model object class
    if (isTRUE(!mod.class %in% c("lavaan", "lm", "glm", "nls", "lmerMod", "lmerModLmerTest", "glmerMod", "lme", "nlme", "glmmTMB", "betareg", "negbin", "polr"))) { stop(paste0("This function does not support the model object class \"", mod.class, "\"."), call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Difference Test ####

  # 'difftest' Argument
  if (isTRUE(difftest)) {

    # One model object
    if (isTRUE(length(mod) == 1L)) { warning("The argument 'difftest' was set to FALSE as only one model object was specified for the argument '...'.", call. = FALSE); difftest <- FALSE }

    # Model object "betareg"
    if (isTRUE(mod.class == "betareg")) { warning("The argument 'difftest' was set to FALSE as the function does not support difference testing for \"betareg\" objects.", call. = FALSE); difftest <- FALSE }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Fit Measures ####

  # Default setting
  if (isTRUE(all(c("none", "deviance", "chisq", "cfi", "tli", "rmsea", "srmr") %in% print.fit))) {

    print.fit <- c("deviance", "chisq", "cfi", "tli", "rmsea", "srmr")

  # No fit measures
  } else if (isTRUE(all("none" %in% print.fit))) {

    print.fit <- NULL

  # User-specified fit measures
  } else {

    print.fit <- misty::chr.omit(print.fit, omit = "none", check = FALSE)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Type of Fit Measures ####

  if (isTRUE(all(mod.class == "lavaan"))) {

    # Default setting
    if (isTRUE(all(c("standard", "scaled", "robust") %in% fit.robust))) {

      names(suppressWarnings(lavaan::fitmeasures(mod[[1L]]))) |>
        (\(p) if (isTRUE("cfi.robust" %in% p)) {

          fit.robust <<- "robust"

        } else if (isTRUE("cfi.scaled" %in% p)) {

          fit.robust <<- "scaled"

        } else {

          fit.robust <<- "standard"

        })()

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Information Criteria ####

  print.all <- c("aic", "caic", "bic", "sabic", "aicc", "hqc", "hbic", "spbic", "ibic", "sic", "icomp")

  # Default setting
  if (isTRUE(all(c("all", "default", print.all) %in% print.ic))) {

    if (isTRUE(length(mod) == 1L)) { print.ic <- NULL } else { print.ic <- c("aic", "caic", "bic", "sabic") }

  # All information criteria
  } else if (isTRUE(all("all" %in% print.ic))) {

    print.ic <- print.all

  # No information criteria
  } else if (isTRUE(all("none" %in% print.ic))) {

    print.ic <- NULL

  # Default setting with additional statistical measures
  } else if (isTRUE("default" %in% print.ic && length(print.ic > 1L))) {

    print.ic <- print.all[print.all %in% misty::chr.omit(union(c("aic", "caic", "bic", "sabic"), print.ic), "default", check = FALSE)]

  # Manual default setting
  } else if (isTRUE(all(print.ic == "default"))) {

    print.ic <- c("aic", "caic", "bic", "sabic")

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model Fit and Information Criteria ####

  #--------------------------------------
  ### Latent Variable Models, lavaan package ####

  if (isTRUE(all(mod.class == "lavaan"))) {

    #...................
    #### Model Fit ####

    # Model fit measures
    mod.fit <- lapply(mod, function(y) suppressWarnings(lavaan::fitmeasures(y)))

    # Data frame for model fit measures
    restab <- data.frame(model = mod.names,
                         do.call("rbind",
                                 lapply(seq_along(mod), function(y) mod.fit[[y]] |>
                                          # Single-level analysis
                                          (\(p) if (isTRUE(!lavaan::inspect(mod[[y]], what = "options")$.multilevel)) {

                                            c(deviance = unname(-2L*p["logl"]), p[c("npar", "chisq", "df", switch(fit.robust, standard = c("cfi", "tli", "rmsea"), scaled = c("cfi.scaled", "tli.scaled", "rmsea.scaled"), robust =  c("cfi.robust", "tli.robust", "rmsea.robust")), "srmr")])

                                          # Multi-level analysis
                                          } else {

                                            c(deviance = unname(-2L*p["logl"]), p[c("npar", "chisq", "df", switch(fit.robust, standard = c("cfi", "tli", "rmsea"), scaled = c("cfi.scaled", "tli.scaled", "rmsea.scaled"), robust =  c("cfi.robust", "tli.robust", "rmsea.robust")), "srmr_within", "srmr_between")])

                                          })() |> (\(q) setNames(q, nm = misty::chr.gsub(pattern = c(".scaled", ".robust", "srmr_within", "srmr_between"), c("", "", "srmrw", "srmrb"), names(q), check = FALSE)))())))

    #...................
    #### Information Criteria ####

    ##### Akaike Information Criterion ####
    if (isTRUE("aic" %in% print.ic)) { restab <- data.frame(restab, aic = sapply(mod.fit, function(y) y["aic"])) }

    ##### Consistent Akaike Information Criterion ####
    if (isTRUE("caic" %in% print.ic)) { restab <- data.frame(restab, caic = sapply(mod.fit, function(y) unname(y["bic"] + y["npar"]))) }

    ##### Bayesian Information Criterion ####
    if (isTRUE("bic" %in% print.ic)) { restab <- data.frame(restab, bic = sapply(mod.fit, function(y) y["bic"])) }

    ##### Sample-Size adjusted BIC ####
    if (isTRUE("sabic" %in% print.ic)) { restab <- data.frame(restab, sabic = sapply(mod.fit, function(y) y["bic2"]))  }

    ##### Corrected Akaike Information Criterion ####
    if (isTRUE("aicc" %in% print.ic)) { restab <- data.frame(restab, aicc = sapply(mod, .aicc)) }

    ##### Hannan-Quinn Criterion ####
    if (isTRUE("hqc" %in% print.ic)) { restab <- data.frame(restab, hqc = sapply(mod, .hqc)) }

    ##### Haughton’s BIC ####
    if (isTRUE("hbic" %in% print.ic)) { restab <- data.frame(restab, hbic = sapply(mod, .hbic)) }

    ##### Scaled Unit-Information Prior BIC ####
    if (isTRUE("spbic" %in% print.ic)) { restab <- data.frame(restab, spbic = sapply(mod, .spbic)) }

    ##### Information-Matrix-based BIC ####
    if (isTRUE("ibic" %in% print.ic)) { restab <- data.frame(restab, ibic = sapply(mod, .ibic)) }

    ##### Stochastic Information Criterion ####
    if (isTRUE("sic" %in% print.ic)) { restab <- data.frame(restab, sic = sapply(mod, .sic)) }

    ##### Bozdogan Information Complexity ####
    if (isTRUE("icomp" %in% print.ic)) { restab <- data.frame(restab, icomp = sapply(mod, .icomp)) }

  #--------------------------------------
  ### All other Models ####
  #
  # Linear, Generalized, and Nonlinear Models: lm(), glm(), nls() functions
  # Linear and Generalized Mixed-Effects Models: lmer(), glmer(), glmer.nb(), lme(), nlme() functions
  # Generalized Linear Mixed Model using Template Model Builder: glmmTMB() function
  # Beta Regression Model for Rates and Proportions: betareg() function
  # Negative Binomial Generalized Linear Model: glm.nb() function
  # Ordered Logistic or Probit Regression Model: polr() function

  } else {

    restab <- data.frame(model = mod.names, do.call("rbind", lapply(mod, function(y) c(npar = attr(logLik(y), which = "df"), deviance = -2L*logLik(y)))))

    #...................
    #### Information Criteria ####

    ##### Akaike Information Criterion ####
    if (isTRUE("aic" %in% print.ic)) { restab <- data.frame(restab, aic = sapply(mod, AIC)) }

    ##### Consistent Akaike Information Criterion ####
    if (isTRUE("caic" %in% print.ic)) { restab <- data.frame(restab, caic = sapply(mod, .caic)) }

    ##### Bayesian Information Criterion ####
    if (isTRUE("bic" %in% print.ic)) { restab <- data.frame(restab, bic = sapply(mod, BIC)) }

    ##### Sample-Size adjusted BIC ####
    if (isTRUE("sabic" %in% print.ic)) { restab <- data.frame(restab, sabic = sapply(mod, .sabic))  }

    ##### Corrected Akaike Information Criterion ####
    if (isTRUE("aicc" %in% print.ic)) { restab <- data.frame(restab, aicc = sapply(mod, .aicc)) }

    ##### Hannan-Quinn Criterion ####
    if (isTRUE("hqc" %in% print.ic)) { restab <- data.frame(restab, hqc = sapply(mod, .hqc)) }

    ##### Haughton’s BIC ####
    if (isTRUE("hbic" %in% print.ic)) { restab <- data.frame(restab, hbic = sapply(mod, .hbic)) }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Chi-Squared Difference, Likelihood-Ratio, or F-Test ####

  if (isTRUE(difftest)) {

    #--------------------------------------
    ### Latent Variable Models, lavaan package ####

    if (isTRUE(all(mod.class == "lavaan"))) {

      restab <- suppressMessages(suppressWarnings(lavaan::anova(...))) |> (\(p) data.frame(restab[as.numeric(gsub("\\.", "", row.names(p))), ], d.chisq = p[, "Chisq diff"], d.df = p[, "Df diff"], p = p[, "Pr(>Chisq)"]))()

    #--------------------------------------
    ### Linear Models ####

    } else if (isTRUE(all(mod.class == "lm"))) {

      restab <- suppressMessages(suppressWarnings(eval(parse(text = paste0("anova(", paste(mod.names[order(restab$npar)], collapse = ", "), ")"))))) |> (\(p) data.frame(restab[order(restab$npar), ], F = p[, "F"], p = p[, "Pr(>F)"]))()

    #--------------------------------------
    ### Generalize Linear Models ####

    } else if (isTRUE(all(mod.class == "glm"))) {

      restab <- suppressMessages(suppressWarnings(eval(parse(text = paste0("anova(", paste(mod.names[order(restab$npar)], collapse = ", "), ")"))))) |> (\(p) data.frame(restab[order(restab$npar), ], chisq = p[, "Deviance"], df = p[, "Df"], p = p[, "Pr(>Chi)"]))()

    #--------------------------------------
    ### Nonlinear Models ####

    } else if (isTRUE(all(mod.class == "nls"))) {

      restab <- suppressMessages(suppressWarnings(eval(parse(text = paste0("anova(", paste(mod.names[order(restab$npar)], collapse = ", "), ")"))))) |> (\(p) data.frame(restab[order(restab$npar), ], F = p[, "F value"], p = p[, "Pr(>F)"]))()

    #--------------------------------------
    ### Linear and Generalized Mixed-Effects Models, lme4 package ####

    } else if (isTRUE(all(mod.class %in% c("lmerMod", "lmerModLmerTest", "glmerMod")))) {

      # REML estimation and varying fixed effects
      if (isTRUE(lme4::isREML(mod[[1L]]) && any(lapply(mod, function(y) names(lme4::fixef(y))) |> (\(p) as.vector(!sapply(p, function(x) sapply(p, function(y) identical(x,y)))))()))) {

        warning("Difference tests for models with varying fixed effects are not meaningful when using REML estimation method.", call. = FALSE)

      } else {

        # Check if package is installed
        if (isTRUE(!"lme4" %in% row.names(installed.packages()))) { stop(paste0("Package \"lme4\" is needed for performing difference tests, please install it."), call. = FALSE) }

        # Check if package is loaded
        if (isTRUE(!"lme4" %in% .packages())) { stop(paste0("Package \"lme4\" is needed for performing difference tests, please load it."), call. = FALSE) }

        #### Likelihood Ratio Test ####
        restab <- suppressMessages(suppressWarnings(anova(..., refit = FALSE))) |> (\(p) data.frame(restab[as.numeric(gsub("\\.", "", row.names(p))), ], chisq = p[, "Chisq"], df = p[, "Df"], p = p[, "Pr(>Chisq)"]))()

      }

    #--------------------------------------
    ### Linear and Nonlinear Mixed-Effects Models, nlme package ####

    } else if (isTRUE(all(mod.class %in% c("lme", "nlme")))) {

      # REML estimation and varying fixed effects
      if (isTRUE(mod[[1L]]$method == "REML" && any(lapply(mod, function(y) names(nlme::fixef(y))) |> (\(p) as.vector(!sapply(p, function(x) sapply(p, function(y) identical(x,y)))))()))) {

        warning("Difference tests for models with varying fixed effects are not meaningful when using REML estimation method.", call. = FALSE)

      } else {

        restab <- suppressMessages(suppressWarnings(eval(parse(text = paste0("anova(", paste(mod.names[order(restab$npar)], collapse = ", "), ")"))))) |> (\(p) data.frame(restab[order(restab$npar), ], chisq = p[, "L.Ratio"], df = c(NA, diff(p[, "df"])), p = p[, "p-value"]))()

      }

    #--------------------------------------
    ### Generalized Linear Mixed Model Using Template Model Builder, glmmTMB package ####

    } else if (isTRUE(all(mod.class %in% "glmmTMB"))) {

      # REML estimation and varying fixed effects
      if (isTRUE(mod[[1L]]$modelInfo$REML && any(lapply(mod, function(y) unlist(names(lme4::fixef(y)))) |> (\(p) as.vector(!sapply(p, function(x) sapply(p, function(y) identical(x, y)))))()))) {

        warning("Difference tests for models with varying fixed effects are not meaningful when using REML estimation method.", call. = FALSE)

      } else {

        restab <- suppressMessages(suppressWarnings(anova(...))) |> (\(p) data.frame(restab[as.numeric(gsub("\\.", "", row.names(p))), ], chisq = p[, "Chisq"], df = p[, "Chi Df"], p = p[, "Pr(>Chisq)"]))()

      }

    #--------------------------------------
    ### Negative Binomial Generalized Linear Models: glm.nb() function, MASS package ####

    } else if (isTRUE(all(mod.class == "negbin"))) {

      restab <- suppressMessages(suppressWarnings(anova(...))) |> (\(p) data.frame(restab[as.numeric(gsub("\\.", "", row.names(p))), ], chisq = p[, "LR stat."], df = p[, "   df"], p = p[, "Pr(Chi)"]))()

    #--------------------------------------
    ### Ordered Logistic or Probit Regression Model: polr() function, MASS package ####

    } else if (isTRUE(all(mod.class == "polr"))) {

      restab <- suppressMessages(suppressWarnings(anova(...))) |> (\(p) data.frame(restab[as.numeric(gsub("\\.", "", row.names(p))), ], chisq = p[, "LR stat."], df = p[, "   Df"], p = p[, "Pr(Chi)"]))()

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Select Model Fit Measures and Remove NA Columns ####

  #--------------------------------------
  ### Model Fit ####

  if (isTRUE(all(mod.class == "lavaan"))) { restab <- restab[, which(!colnames(restab) %in% setdiff(c("deviance", "chisq", "cfi", "tli", "rmsea", "srmr"), print.fit))] }

  #--------------------------------------
  ### Remove NA columns ####

  restab <- restab[, sapply(restab, function(y) any(!is.na(y)))]

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "modcomp",
                 class = mod.class,
                 model = mod,
                 args = list(difftest = difftest, print.fit = print.fit, fit.robust = fit.robust, print.ic = print.ic, fit.digits = fit.digits, ic.digits = ic.digits, p.digits = p.digits, write = write, append = append, check = check, output = output),
                 result = restab)

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
