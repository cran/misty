#' Confidence Intervals for the Indirect Effect
#'
#' This function computes confidence intervals for the indirect effect based on
#' the asymptotic normal method, distribution of the product method and the Monte
#' Carlo method. By default, the function uses the distribution of the product
#' method for computing the two-sided 95\% asymmetric confidence intervals for
#' the indirect effect product of coefficient estimator \eqn{\hat{a}\hat{b}}.
#'
#' In statistical mediation analysis (MacKinnon & Tofighi, 2013), the indirect
#' effect refers to the effect of the independent variable \eqn{X} on the outcome
#' variable \eqn{Y} transmitted by the mediator variable \eqn{M}. The magnitude
#' of the indirect effect \eqn{ab} is quantified by the product of the the
#' coefficient \eqn{a} (i.e., effect of \eqn{X} on \eqn{M}) and the coefficient
#' \eqn{b} (i.e., effect of \eqn{M} on \eqn{Y} adjusted for \eqn{X}). In practice,
#' researchers are often interested in confidence limit estimation for the indirect
#' effect. This function offers three different methods for computing the confidence
#' interval for the product of coefficient estimator \eqn{\hat{a}\hat{b}}:
#'
#' \strong{(1) Asymptotic normal method}
#'
#' In the asymptotic normal method, the standard error for the product of the
#' coefficient estimator \eqn{\hat{a}\hat{b}} is computed which is used to create
#' a symmetrical confidence interval based on the z-value of the standard normal
#' (\eqn{z}) distribution assuming that the indirect effect is normally distributed.
#' Note that the function provides three formulas for computing the standard error
#' by specifying the argument \code{se}:
#'
#' \describe{
#'   \item{\code{"sobel"}}{Approximate standard error by Sobel (1982) using the
#'   multivariate delta method based on a first order Taylor series approximation:
#'   \deqn{\sqrt(a^2 \sigma^2_a + b^2 \sigma^2_b)}}
#'
#'   \item{\code{"aroian"}}{Exact standard error by Aroian (1947) based on a first
#'   and second order Taylor series approximation:
#'   \deqn{\sqrt(a^2 \sigma^2_a + b^2 \sigma^2_b + \sigma^2_a \sigma^2_b)}}
#'
#'   \item{\code{"goodman"}}{Unbiased standard error by Goodman (1960):
#'   \deqn{\sqrt(a^2 \sigma^2_a + b^2 \sigma^2_b - \sigma^2_a \sigma^2_b)}
#'   Note that the unbiased standard error is often negative and is hence
#'   undefined for zero or small effects or small sample sizes.}
#' }
#'
#' The asymptotic normal method is known to have low statistical power because
#' the distribution of the product \eqn{\hat{a}\hat{b}} is not normally distributed.
#' (Kisbu-Sakarya, MacKinnon, & Miocevic, 2014). In the null case, where both
#' random variables have mean equal to zero, the distribution is symmetric with
#' kurtosis of six. When the product of the means of the two random variables is
#' nonzero, the distribution is skewed (up to a maximum value of \eqn{\pm} 1.5)
#' and has a excess kurtosis (up to a maximum value of 6). However, the product
#' approaches a normal distribution as one or both of the ratios of the means to
#' standard errors of each random variable get large in absolute value (MacKinnon,
#' Lockwood & Williams, 2004).
#'
#' \strong{(2) Distribution of the product method}
#'
#' The distribution of the product method (MacKinnon et al., 2002) relies on an
#' analytical approximation of the distribution of the product of two normally
#' distributed variables. The method uses the standardized \eqn{a} and \eqn{b}
#' coefficients to compute \eqn{ab} and then uses the critical values for the
#' distribution of the product (Meeker, Cornwell, & Aroian, 1981) to create
#' asymmetric confidence intervals. The distribution of the product approaches
#' the gamma distribution (Aroian, 1947). The analytical solution for the
#' distribution of the product is provided by the Bessel function used to the
#' solution of differential equations and is approximately proportional to the
#' Bessel function of the second kind with a purely imaginary argument (Craig,
#' 1936).
#'
#' \strong{(3) Monte Carlo method}
#'
#' The Monte Carlo (MC) method (MacKinnon et al., 2004) relies on the assumption
#' that the parameters \eqn{a} and \eqn{b} have a joint normal sampling
#' distribution. Based on the parametric assumption, a sampling distribution of
#' the product \eqn{a}\eqn{b} using random samples with population values equal
#' to the sample estimates \eqn{\hat{a}}, \eqn{\hat{b}}, \eqn{\hat{\sigma}_a},
#' and \eqn{\hat{\sigma}_b} is generated. Percentiles of the sampling distribution
#' are identified to serve as limits for a \eqn{100(1 - \alpha)}\% asymmetric
#' confidence interval about the sample \eqn{\hat{a}\hat{b}} (Preacher & Selig,
#' 2012). Note that parametric assumptions are invoked for \eqn{\hat{a}} and
#' \eqn{\hat{b}}, but no parametric assumptions are made about the distribution
#' of \eqn{\hat{a}\hat{b}}.
#'
#' @param a           a numeric value indicating the coefficient \eqn{a}, i.e.,
#'                    effect of \eqn{X} on \eqn{M}.
#' @param b           a numeric value indicating the coefficient \eqn{b}, i.e.,
#'                    effect of \eqn{M} on \eqn{Y} adjusted for \eqn{X}.
#' @param se.a        a positive numeric value indicating the standard error of
#'                    \eqn{a}.
#' @param se.b        a positive numeric value indicating the standard error of
#'                    \eqn{b}.
#' @param print       a character string or character vector indicating which
#'                    confidence intervals (CI) to show on the console, i.e.
#'                    \code{"all"} for all CIs, \code{"asymp"} for the CI based
#'                    on the asymptotic normal method, \code{"dop"} (default) for
#'                    the CI based on the distribution of the product method, and
#'                    \code{"mc"} for the CI based on the Monte Carlo method.
#' @param se          a character string indicating which standard error (SE) to
#'                    compute for the asymptotic normal method, i.e., \code{"sobel"}
#'                    for the approximate standard error by Sobel (1982) using the
#'                    multivariate delta method based on a first order Taylor series
#'                    approximation, \code{"aroian"} (default) for the exact standard
#'                    error by Aroian (1947) based on a first and second order Taylor
#'                    series approximation, and \code{"goodman"} for the unbiased
#'                    standard error by Goodman (1960).
#' @param nrep        an integer value indicating the number of Monte Carlo repetitions.
#' @param alternative a character string specifying the alternative hypothesis,
#'                    must be one of \code{"two.sided"} (default), \code{"greater"}
#'                    or \code{"less"}.
#' @param seed        a numeric value specifying the seed of the random number
#'                    generator when using the Monte Carlo method.
#' @param conf.level  a numeric value between 0 and 1 indicating the confidence
#'                    level of the interval.
#' @param digits      an integer value indicating the number of decimal places
#'                    to be used for displaying
#' @param write       a character string naming a text file with file extension
#'                    \code{".txt"} (e.g., \code{"Output.txt"}) for writing the
#'                    output into a text file.
#' @param append      logical: if \code{TRUE} (default), output will be appended
#'                    to an existing text file with extension \code{.txt} specified
#'                    in \code{write}, if \code{FALSE} existing text file will be
#'                    overwritten.
#' @param check        logical: if \code{TRUE} (default), argument specification
#'                    is checked.
#' @param output      logical: if \code{TRUE} (default), output is shown on the
#'                    console.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{multilevel.indirect}}
#'
#' @references
#' Aroian, L. A. (1947). The probability function of the product of two normally distributed variables.
#' \emph{Annals of Mathematical Statistics, 18}, 265-271. https://doi.org/10.1214/aoms/1177730442
#'
#' Craig,C.C. (1936). On the frequency function of xy. \emph{Annals of Mathematical Statistics, 7}, 1–15.
#' https://doi.org/10.1214/aoms/1177732541
#'
#' Goodman, L. A. (1960). On the exact variance of products. \emph{Journal of the American Statistical
#' Association, 55}, 708-713. https://doi.org/10.1080/01621459.1960.10483369
#'
#' Kisbu-Sakarya, Y., MacKinnon, D. P., & Miocevic M. (2014). The distribution of the product explains
#' normal theory mediation confidence interval estimation. \emph{Multivariate Behavioral Research, 49},
#' 261–268. https://doi.org/10.1080/00273171.2014.903162
#'
#' MacKinnon, D. P., Lockwood, C. M., Hoffman, J. M., West, S. G., & Sheets, V. (2002). Comparison of methods
#' to test mediation and other intervening variable effects. \emph{Psychological Methods, 7}, 83–104.
#' https://doi.org/10.1037/1082-989x.7.1.83
#'
#' MacKinnon, D. P., Lockwood, C. M., & Williams, J. (2004). Confidence limits for the indirect effect:
#' Distribution of the product and resampling methods. \emph{Multivariate Behavioral Research, 39}, 99-128.
#' https://doi.org/10.1207/s15327906mbr3901_4
#'
#' MacKinnon, D. P., & Tofighi, D. (2013). Statistical mediation analysis. In J. A. Schinka, W. F. Velicer,
#' & I. B. Weiner (Eds.), \emph{Handbook of psychology: Research methods in psychology} (pp. 717-735).
#' John Wiley & Sons, Inc..
#'
#' Meeker, W. Q., Jr., Cornwell, L. W., & Aroian, L. A. (1981). The product of two normally distributed
#' random variables. In W. J. Kennedy & R. E. Odeh (Eds.), \emph{Selected tables in mathematical statistics}
#' (Vol. 7, pp. 1–256). Providence, RI: American Mathematical Society.
#'
#' Preacher, K. J., & Selig, J. P. (2012). Advantages of Monte Carlo confidence intervals for indirect effects.
#' \emph{Communication Methods and Measures, 6}, 77–98. http://dx.doi.org/10.1080/19312458.2012.679848
#'
#' Sobel, M. E. (1982). Asymptotic confidence intervals for indirect effects in structural equation models.
#' In S. Leinhardt (Ed.), \emph{Sociological methodology 1982} (pp. 290-312). Washington, DC: American
#' Sociological Association.
#'
#' Tofighi, D. & MacKinnon, D. P. (2011). RMediation: An R package for mediation analysis
#' confidence intervals. \emph{Behavior Research Methods, 43}, 692-700.
#' https://doi.org/10.3758/s13428-011-0076-x
#'
#' @note The function was adapted from the \code{medci()} function in the \pkg{RMediation}
#' package by Davood Tofighi and David P. MacKinnon (2016).
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#' \tabular{ll}{
#' \code{call} \tab function call \cr
#' \code{type} \tab type of analysis \cr
#' \code{data} \tab list with the input specified in \code{a} \code{b}, \code{se.a},
#'                  and \code{se.b}  \cr
#' \code{args} \tab specification of function arguments \cr
#' \code{result} \tab list with result tables \cr
#' }
#'
#' @export
#'
#' @examples
#' # Example 1: Distribution of the Product Method
#' indirect(a = 0.35, b = 0.27, se.a = 0.12, se.b = 0.18)
#'
#' # Example 2: Monte Carlo Method
#' indirect(a = 0.35, b = 0.27, se.a = 0.12, se.b = 0.18, print = "mc")
#'
#' # Example 3: Asymptotic Normal Method
#' indirect(a = 0.35, b = 0.27, se.a = 0.12, se.b = 0.18, print = "asymp")
#'
#' \dontrun{
#' # Example 4: Write Results into a text file
#' indirect(a = 0.35, b = 0.27, se.a = 0.12, se.b = 0.18, write = "Indirect.txt")
#' }
indirect <- function(a, b, se.a, se.b, print = c("all", "asymp", "dop", "mc"),
                     se = c("sobel", "aroian", "goodman"), nrep = 100000,
                     alternative = c("two.sided", "less", "greater"), seed = NULL,
                     conf.level = 0.95, digits = 3, write = NULL, append = TRUE,
                     check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # Check input 'a', 'b', 'se.a', and 'se.b'
    if (isTRUE(mode(a) != "numeric")) { stop("Please specify a numeric value for the argument 'a'.", call. = FALSE) }

    if (isTRUE(mode(b) != "numeric")) { stop("Please specify a numeric value for the argument 'b'.", call. = FALSE) }

    if (isTRUE(mode(se.a) != "numeric" || se.a <= 0L)) { stop("Please specify a positive numeric value for the argument 'se.a'.", call. = FALSE) }

    if (isTRUE(mode(se.b) != "numeric" || se.a <= 0L)) { stop("Please specify a positive numeric value for the argument 'se.b'.", call. = FALSE) }

    # Check input 'print'
    if (isTRUE(any(!print %in% c("all", "asymp", "dop", "mc")))) { stop("Character string(s) in the argument 'print' does not match with \"all\", \"asymp\", \"dop\", or \"mc\".", call. = FALSE) }

    # Check input 'se'
    if (isTRUE(any(!se %in% c("sobel", "aroian", "goodman")))) { stop("Character string(s) in the argument 'se' does not match with \"sobel\", \"aroian\", or \"goodman\".", call. = FALSE) }

    # Check input 'nrep'
    if (isTRUE(mode(nrep) != "numeric" || nrep <= 1L)) { stop("Please specify a positive numeric value greater 1 for the argument 'nrep'.", call. = FALSE) }

    # Check input 'alternative'
    if (isTRUE(!all(alternative %in% c("two.sided", "less", "greater")))) { stop("Character string in the argument 'alternative' does not match with \"two.sided\", \"less\", or \"greater\".", call. = FALSE) }

    # Check input 'seed'
    if (isTRUE(mode(seed) != "numeric" && !is.null(seed))) { stop("Please specify a numeric value for the argument 'seed'.", call. = FALSE) }

    # Check input 'conf.level'
    if (isTRUE(conf.level >= 1L || conf.level <= 0L)) { stop("Please specifiy a numeric value between 0 and 1 for the argument 'conf.level'.", call. = FALSE) }

    # Check input 'digits'
    if (isTRUE(digits %% 1L != 0L || digits < 0L)) { stop("Specify a positive integer number for the argument 'digits'.", call. = FALSE) }

    # Check input 'write'
    if (isTRUE(!is.null(write) && substr(write, nchar(write) - 3L, nchar(write)) != ".txt")) { stop("Please specify a character string with file extenstion '.txt' for the argument 'write'.") }

    # Check input 'append'
    if (isTRUE(!is.logical(append))) { stop("Please specify TRUE or FALSE for the argument 'append'.", call. = FALSE) }

    # Check input 'output'
    if (isTRUE(!is.logical(output))) { stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Print Confidence intervals ####

  if(all(c("all", "asymp", "dop", "mc") %in% print)) { print <- "dop" }

  if (isTRUE(all(print == "all"))) { print <- c("asymp", "dop", "mc") }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Method Used to Compute Standard Errors ####

  se <- ifelse(all(c("sobel", "aroian", "goodman") %in% se), "aroian", se)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Alternative hypothesis ####

  if (isTRUE(all(c("two.sided", "less", "greater") %in% alternative))) { alternative <- "two.sided" }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Asymptotic Normal Confidence Interval ####

  #...................
  ### Point estimate ####

  asymp.ab <- a*b

  #...................
  ### Standard error ####

  switch(se, "sobel" = {

    asmyp.se.ab <- sqrt(a^2L*se.b^2L + b^2L*se.a^2L)

  }, "aroian" = {

    asmyp.se.ab <- sqrt(a^2L*se.b^2L + b^2L*se.a^2L + se.a^2L*se.b^2L)

  }, "goodman" = {

    asmyp.se.ab <- sqrt(a^2L*se.b^2L + b^2L*se.a^2L - se.a^2L*se.b^2L)

    # Negative standard error
    if (isTRUE(is.nan(asmyp.se.ab))) {

      if (isTRUE(all(print == "asymp"))) {

        stop("Goodman standard error is undefined, please use a different method for standard error computation.",
             call. = FALSE)

        asmyp.se.ab <- NA

      } else {

        warning("Goodman standard error is undefined, please use a different method for standard error computation.",
                call. = FALSE)

        asmyp.se.ab <- NA

      }

    }

  })

  #...................
  ### Confidence interval ####

  # Non-negative standard error
  if (isTRUE(!is.na(asmyp.se.ab))) {

    asymp.ci <- switch(alternative,
                       two.sided = c(low = asymp.ab - qnorm((1L - conf.level) / 2L, lower.tail = FALSE) * asmyp.se.ab,
                                     upp = asymp.ab + qnorm((1L - conf.level) / 2L, lower.tail = FALSE) * asmyp.se.ab),
                       less = c(low = -Inf,
                                upp = asymp.ab + qnorm(1L - conf.level, lower.tail = FALSE) * asmyp.se.ab),
                       greater = c(low = asymp.ab - qnorm(1L - conf.level, lower.tail = FALSE) * asmyp.se.ab,
                                   upp = Inf))

  # Negative standard error
  } else {

    asymp.ci <- c(low = NA, upp = NA)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Distribution of Product Method ####

  qprodnormalMeeker <- function(p, a, b, se.a, se.b, lower.tail = TRUE) {

    max.iter <- 1000

    mu.a <- a / se.a
    mu.b <- b / se.b

    se.ab <- sqrt(1L + mu.a^2L + mu.b^2L)

    if (lower.tail == FALSE) {

      u0 <- mu.a*mu.b + 6L*se.ab
      l0 <- mu.a*mu.b - 6L*se.ab
      alpha <- 1L - p

    } else {

      l0 <- mu.a*mu.b - 6L*se.ab
      u0 <- mu.a*mu.b + 6L*se.ab
      alpha <- p

    }

    gx <- function(x, z) {

      mu.a.on.b <- mu.a
      integ <- pnorm(sign(x)*(z / x - mu.a.on.b))*dnorm(x - mu.b)

      return(integ)

    }

    fx <- function(z) {

      return(integrate(gx, lower = -Inf, upper = Inf, z = z)$value - alpha)

    }

    p.l <- fx(l0)
    p.u <- fx(u0)
    iter <- 0L

    while (p.l > 0L) {

      iter <- iter + 1
      l0 <- l0 - 0.5*se.ab
      p.l <- fx(l0)

      if (iter > max.iter) {

        cat(" No initial valid lower bound interval!\n")
        return(list(q = NA, error = NA))

      }

    }

    iter <- 0 #Reset iteration counter
    while (p.u < 0L) {

      iter <- iter + 1L
      u0 <- u0 + 0.5*se.ab
      p.u <- fx(u0)

      if (iter > max.iter) {

        cat(" No initial valid upper bound interval!\n")
        return(list(q = NA,error = NA))

      }

    }

    res <- uniroot(fx, c(l0, u0))
    new <- res$root
    new <- new*se.a*se.b

    return(new)

  }

  #...................
  ### Point estimate ####

  dop.ab <- asymp.ab

  #...................
  ### Standard error ####

  dop.se.ab <- asmyp.se.ab

  # Negative standard error
  if (isTRUE(is.na(dop.se.ab) & !"asymp" %in% print)) {

    warning("Goodman standard error is undefined.", call. = FALSE)

    dop.se.ab <- NA

  }

  #...................
  ### Confidence interval ####

  dop.ci <- switch(alternative,
                   two.sided = c(low = qprodnormalMeeker((1L - conf.level) / 2L, a = a, b = b, se.a = se.a, se.b = se.b, lower.tail = TRUE),
                                 upp = qprodnormalMeeker((1L - conf.level) / 2L, a = a, b = b, se.a = se.a, se.b = se.b, lower.tail = FALSE)),
                   less = c(low = -Inf,
                            upp = qprodnormalMeeker((1L - conf.level), a = a, b = b, se.a = se.a, se.b = se.b, lower.tail = FALSE)),
                   greater = c(low = qprodnormalMeeker((1L - conf.level), a = a, b = b, se.a = se.a, se.b = se.b, lower.tail = TRUE),
                               upp = Inf))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Monte Carlo Method ####

  #...................
  ### Random Number Generation ####

  if (isTRUE(!is.null(seed))) {

    set.seed(seed)

  }

  #...................
  ### Monte Carlo ####

  a_b <- matrix(rnorm(2L*nrep), ncol = nrep)

  a_b <- t(crossprod(chol(matrix(c(se.a^2L, se.a*se.b*0L, se.a*se.b*0L, se.b^2L), nrow = 2L)), a_b) + c(a, b))

  ab <- a_b[, 1L]*a_b[, 2L]

  #...................
  ### Point estimate ####

  mc.ab <- mean(ab)

  #...................
  ### Standard error ####

  mc.se.ab <- sd(ab)

  #...................
  ### Confidence interval ####

  mc.ci <- switch(alternative,
                  two.sided = c(low = quantile(ab, (1L - conf.level) / 2L),
                                upp = quantile(ab, 1L - (1L - conf.level) / 2L)),
                  less = c(low = -Inf,
                           upp = quantile(ab, conf.level)),
                  greater = c(low = quantile(ab, 1L - conf.level),
                              upp = Inf))

  #_____________________________________________________________________________
  #
  # Return object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "indirect",
                 data = list(a = a, b = b, se.a = se.b, se.b = se.b),
                 args = list(print = print, se = se, nrep = nrep, alternative = alternative,
                             seed = seed, conf.level = conf.level, digits = digits,
                             write = write, append = append, check = check, output = output),
                 result = list(asymp = data.frame(est = asymp.ab, se = asmyp.se.ab,
                                                  low = asymp.ci[1L], upp = asymp.ci[2L], row.names = NULL),
                               dop = data.frame(est = dop.ab, se = dop.se.ab,
                                                low = dop.ci[1L], upp = dop.ci[2L], row.names = NULL),
                               mc = data.frame(est = mc.ab, se = mc.se.ab,
                                               low = mc.ci[1L], upp = mc.ci[2L], row.names = NULL)))

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Write Results --------------------------------------------------------------

  #_____________________________________________________________________________
  #
  # Write Results --------------------------------------------------------------

  if (isTRUE(!is.null(write))) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Text file ####

    # Send R output to textfile
    sink(file = write, append = ifelse(isTRUE(file.exists(write)), append, FALSE), type = "output", split = FALSE)

    if (isTRUE(append && file.exists(write))) { write("", file = write, append = TRUE) }

    # Print object
    print(object, check = FALSE)

    # Close file connection
    sink()

  }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

   if (isTRUE(output)) { print(object, check = FALSE) }

   return(invisible(object))

}
