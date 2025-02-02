#' Confidence Interval for the Indirect Effect in a 1-1-1 Multilevel Mediation Model
#'
#' This function computes the confidence interval for the indirect effect in a
#' 1-1-1 multilevel mediation model with random slopes based on the Monte Carlo
#' method.
#'
#' In statistical mediation analysis (MacKinnon & Tofighi, 2013), the indirect effect
#' refers to the effect of the independent variable \eqn{X} on the outcome variable
#' \eqn{Y} transmitted by the mediator variable \eqn{M}. The magnitude of the indirect
#' effect \eqn{ab} is quantified by the product of the the coefficient \eqn{a}
#' (i.e., effect of \eqn{X} on \eqn{M}) and the coefficient \eqn{b} (i.e., effect of
#' \eqn{M} on \eqn{Y} adjusted for \eqn{X}). However, mediation in the context of a
#' 1-1-1 multilevel mediation model where variables \eqn{X}, \eqn{M}, and \eqn{Y}
#' are measured at level 1, the coefficients \eqn{a} and \eqn{b} can vary across
#' level-2 units (i.e., random slope). As a result, \eqn{a} and \eqn{b} may covary
#' so that the estimate of the indirect effect is no longer simply the product of
#' the coefficients \eqn{\hat{a}\hat{b}}, but \eqn{\hat{a}\hat{b} + \tau_{a,b}},
#' where \eqn{\tau_{a,b}} (i.e., \code{cov.rand}) is the level-2 covariance between
#' the random slopes \eqn{a} and \eqn{b}. The covariance term needs to be added to
#' \eqn{\hat{a}\hat{b}} only when random slopes are estimated for both \eqn{a} and
#' \eqn{b}. Otherwise, the simple product is sufficient to quantify the indirect
#' effect, and the \code{\link{indirect}} function can be used instead.
#'
#' In practice, researchers are often interested in confidence limit estimation
#' for the indirect effect. There are several methods for computing a confidence
#' interval for the indirect effect in a single-level mediation models (see
#' \code{\link{indirect}} function). The Monte Carlo (MC) method (MacKinnon et al.,
#' 2004) is a promising method in single-level mediation model which was also adapted
#' to the multilevel mediation model (Bauer, Preacher & Gil, 2006). This method
#' requires seven pieces of information available from the results of a multilevel
#' mediation model:
#'
#' \describe{
#'   \item{a}{Coefficient \eqn{a}, i.e., average effect of \eqn{X} on \eqn{M}
#'            on the cluster or between-group level. In Mplus, \code{Estimate}
#'            of the random slope \eqn{a} under \code{Means} at the
#'            \code{Between Level}.}
#'   \item{b}{Coefficient \eqn{b}, i.e., average effect of \eqn{M} on \eqn{Y}
#'            on the cluster or between-group level. In Mplus, \code{Estimate}
#'            of the random slope \eqn{b} under \code{Means} at the
#'                   \code{Between Level}.}
#'   \item{se.a}{Standard error of \code{a}. In Mplus, \code{S.E.}
#'               of the random slope \eqn{a} under \code{Means} at the
#'               \code{Between Level}.}
#'   \item{se.b}{Standard error of \code{b}. In Mplus, \code{S.E.}
#'               of the random slope \eqn{b} under \code{Means} at the
#'               \code{Between Level}.}
#'   \item{cov.ab}{Covariance between \eqn{a} and \eqn{b}. In Mplus, the
#'                 estimated covariance matrix for the parameter estimates
#'                 (i.e., asymptotic covariance matrix) need to be requested
#'                 by specifying \code{TECH3} along with \code{TECH1} in the
#'                 \code{OUTPUT} section. In the \code{TECHNICAL 1 OUTPUT}
#'                 under \code{PARAMETER SPECIFICATION FOR BETWEEN}, the
#'                 numbers of the parameter for the coefficients \eqn{a} and
#'                 \eqn{b} need to be identified under \code{ALPHA} to look
#'                 up \code{cov.av} in the corresponding row and column in
#'                 the \code{TECHNICAL 3 OUTPUT} under \code{ESTIMATED COVARIANCE
#'                 MATRIX FOR PARAMETER ESTIMATES}.}
#'   \item{cov.rand}{Covariance between the random slopes for \eqn{a} and
#'                   \eqn{b}. In Mplus, \code{Estimate} of the covariance
#'                   \eqn{a} \code{WITH} \eqn{b} at the \code{Between Level}}.
#'   \item{se.cov.rand}{Standard error of the covariance between the random
#'                      slopes for \eqn{a} and \eqn{b}. In Mplus, \code{S.E.}
#'                      of the covariance \eqn{a} \code{WITH} \eqn{b} at the
#'                      \code{Between Level}}.
#' }
#'
#' Note that all pieces of information except \code{cov.ab} can be looked up in
#' the standard output of the multilevel mediation model. In order to specify
#' \code{cov.ab}, the covariance matrix for the parameter estimates (i.e.,
#' asymptotic covariance matrix) is required. In practice, \code{cov.ab} will
#' oftentimes be very small so that \code{cov.ab} may be set to 0 (i.e., default
#' value) with negligible impact on the results.
#'
#' @param a           a numeric value indicating the coefficient \eqn{a}, i.e.,
#'                    average effect of \eqn{X} on \eqn{M} on the cluster or
#'                    between-group level.
#' @param b           a numeric value indicating the coefficient \eqn{b}, i.e.,
#'                    average effect of \eqn{M} on \eqn{Y} adjusted for \eqn{X}
#'                    on the cluster or between-group level.
#' @param se.a        a positive numeric value indicating the standard error of
#'                    \eqn{a}.
#' @param se.b        a positive numeric value indicating the standard error of
#'                    \eqn{b}.
#' @param cov.ab      a positive numeric value indicating the covariance between
#'                    \eqn{a} and \eqn{b}.
#' @param cov.rand    a positive numeric value indicating the covariance between
#'                    the random slopes for \eqn{a} and \eqn{b}.
#' @param se.cov.rand a positive numeric value indicating the standard error of the
#'                    covariance between the random slopes for \eqn{a} and \eqn{b}.
#' @param nrep        an integer value indicating the number of Monte Carlo repetitions.
#' @param alternative a character string specifying the alternative hypothesis, must be
#'                    one of \code{"two.sided"} (default), \code{"greater"} or \code{"less"}.
#' @param seed        a numeric value specifying the seed of the random number generator
#'                    when using the Monte Carlo method.
#' @param conf.level  a numeric value between 0 and 1 indicating the confidence level
#'                    of the interval.
#' @param digits      an integer value indicating the number of decimal places
#'                    to be used for displaying
#' @param write       a character string naming a text file with file extension
#'                    \code{".txt"} (e.g., \code{"Output.txt"}) for writing the
#'                    output into a text file.
#' @param check       logical: if \code{TRUE} (default), argument specification
#'                    is checked.
#' @param output      logical: if \code{TRUE} (default), output is shown on the
#'                    console.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{indirect}}
#'
#' @references
#' Bauer, D. J., Preacher, K. J., & Gil, K. M. (2006). Conceptualizing and testing
#' random indirect effects and moderated Mediation in multilevel models: New procedures
#' and recommendations. \emph{Psychological Methods, 11}, 142-163.
#' https://doi.org/10.1037/1082-989X.11.2.142
#'
#' Kenny, D. A., Korchmaros, J. D., & Bolger, N. (2003). Lower level Mediation in
#' multilevel models. \emph{Psychological Methods, 8}, 115-128.
#' https://doi.org/10.1037/1082-989x.8.2.115
#'
#' MacKinnon, D. P., Lockwood, C. M., & Williams, J. (2004). Confidence limits for the indirect effect:
#' Distribution of the product and resampling methods. \emph{Multivariate Behavioral Research, 39}, 99-128.
#' https://doi.org/10.1207/s15327906mbr3901_4
#'
#' MacKinnon, D. P., & Tofighi, D. (2013). Statistical mediation analysis. In J. A. Schinka, W. F. Velicer,
#' & I. B. Weiner (Eds.), \emph{Handbook of psychology: Research methods in psychology} (pp. 717-735).
#' John Wiley & Sons, Inc..
#'
#' Preacher, K. J., & Selig, J. P. (2010). \emph{Monte Carlo method for assessing
#' multilevel Mediation: An interactive tool for creating confidence intervals for
#' indirect effects in 1-1-1 multilevel models} [Computer software]. Available from
#' http://quantpsy.org/.
#'
#' @note
#' The function was adapted from the interactive web tool by Preacher and
#' Selig (2010).
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#' \tabular{ll}{
#' \code{call} \tab function call \cr
#' \code{type} \tab type of analysis \cr
#' \code{data} \tab list with the input specified in \code{a}, \code{b},
#'              \code{se.a}, \code{se.b}, \code{cov.ab}, \code{cov.rand}, and
#'              \code{se.cov.rand} \cr
#' \code{args} \tab specification of function arguments \cr
#' \code{result} \tab list with result tables \cr
#' }
#'
#' @export
#'
#' @examples
#' # Example 1: Confidence Interval for the Indirect Effect
#' multilevel.indirect(a = 0.25, b = 0.20, se.a = 0.11, se.b = 0.13,
#'                     cov.ab = 0.01, cov.rand = 0.40, se.cov.rand = 0.02)
#'
#' # Example 2: Save results of the Monte Carlo method
#' ab <- multilevel.indirect(a = 0.25, b = 0.20, se.a = 0.11, se.b = 0.13,
#'                           cov.ab = 0.01, cov.rand = 0.40, se.cov.rand = 0.02,
#'                           output = FALSE)$result$ab
#'
#' # Histogram of the distribution of the indirect effect
#' hist(ab)
#'
#' # Example 3: Write Results into a text file
#' multilevel.indirect(a = 0.25, b = 0.20, se.a = 0.11, se.b = 0.13,
#'                     cov.ab = 0.01, cov.rand = 0.40, se.cov.rand = 0.02,
#'                     write = "ML-Indirect.txt")
multilevel.indirect <- function(a, b, se.a, se.b, cov.ab = 0, cov.rand, se.cov.rand,
                                nrep = 100000, alternative = c("two.sided", "less", "greater"),
                                seed = NULL, conf.level = 0.95, digits = 3, write = NULL,
                                append = TRUE, check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("append", "output"),
               numeric = list(cov.ab = 1L, nrep = 1L),
               args = c("alterantive", "conf.level", "digits", "write1"), envir = environment(), input.check = check)

  # Additional checks
  if (isTRUE(check)) {

    # Check input 'a', 'b', 'se.a', 'se.b', cov.ab, cov.rand, and se.cov.rand
    if (isTRUE(mode(a) != "numeric")) { stop("Please specify a numeric value for the argument 'a'.", call. = FALSE) }

    if (isTRUE(mode(b) != "numeric")) { stop("Please specify a numeric value for the argument 'b'.", call. = FALSE) }

    if (isTRUE(mode(cov.ab) != "numeric")) { stop("Please specify a numeric value for the argument 'cov.ab'.", call. = FALSE) }

    if (isTRUE(mode(cov.rand) != "numeric")) { stop("Please specify a numeric value for the argument 'cov.rand'.", call. = FALSE) }

    if (isTRUE(mode(se.a) != "numeric" || se.a <= 0L)) { stop("Please specify a positive numeric value for the argument 'se.a'.", call. = FALSE) }

    if (isTRUE(mode(se.b) != "numeric" || se.a <= 0L)) { stop("Please specify a positive numeric value for the argument 'se.b'.", call. = FALSE) }

    if (isTRUE(mode(se.cov.rand) != "numeric" || se.cov.rand <= 0L)) { stop("Please specify a positive numeric value for the argument 'se.cov.rand'.", call. = FALSE) }

    # Check input 'nrep'
    if (isTRUE(mode(nrep) != "numeric" || nrep <= 1L)) { stop("Please specify a positive numeric value greater 1 for the argument 'nrep'.", call. = FALSE) }

    # Check input 'seed'
    if (isTRUE(mode(seed) != "numeric" && !is.null(seed))) { stop("Please specify a numeric value greater for the argument 'seed'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Alternative hypothesis ####

  if (isTRUE(all(c("two.sided", "less", "greater") %in% alternative))) { alternative <- "two.sided" }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Random Number Generation ####

  if (isTRUE(!is.null(seed))) {

    set.seed(seed)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Monte Carlo Method ####

  dvec <- rnorm(nrep)

  avec <- dvec * se.a + a

  sd <- suppressWarnings(sqrt(1L - (cov.ab^2L) / (se.a^2 * se.b^2L)))

  if (isTRUE(is.nan(sd))) {

    while (isTRUE(is.nan(sd))) {

      cov.ab <- cov.ab - 0.00001
      sd <- suppressWarnings(sqrt(1L - (cov.ab^2L) / (se.a^2L * se.b^2L)))

    }

    warning(paste("Argument 'cov.ab' had to be adjusted to", cov.ab, "to resolve a numerical problem."), call. = FALSE)

  }

  bvec <- dvec * cov.ab / se.a + se.b * rnorm(nrep, sd = sqrt(1L - (cov.ab^2L) / (se.a^2L * se.b^2L))) + b

  cvec <- rnorm(nrep) * se.cov.rand + cov.rand

  ab <- avec * bvec + cvec

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Point estimate ####

  mc.ab <- mean(ab)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Standard error ####

  mc.se.ab <- sd(ab)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Confidence interval ####

  mc.ci <- switch(alternative,
                  two.sided = c(low = quantile(ab, (1L - conf.level) / 2),
                                upp = quantile(ab, 1L - (1L - conf.level) / 2)),
                  less = c(low = -Inf,
                           upp = quantile(ab, conf.level)),
                  greater = c(low = quantile(ab, 1L - conf.level),
                              upp = Inf))

  #_____________________________________________________________________________
  #
  # Return object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "multilevel.indirect",
                 data = list(a = a, b = b, se.a = se.b, se.b = se.b, cov.ab = cov.ab,
                             cov.rand = cov.rand, se.cov.rand = se.cov.rand),
                 args = list(nrep = nrep, alternative = alternative, seed = seed,
                             conf.level = conf.level, digits = digits, write = write,
                             append = append, check = check, output = output),
                 result = list(ab = ab,
                               mc = data.frame(est = mc.ab, se = mc.se.ab,
                                               low = mc.ci[1L], upp = mc.ci[2L], row.names = NULL)))

  class(object) <- "misty.object"


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

#_______________________________________________________________________________
