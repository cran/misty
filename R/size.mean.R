#' Sample size determination for testing arithmetic means
#'
#' This function performs sample size computation for the one-sample and two-sample t-test
#' based on precision requirements (i.e., type-I-risk, type-II-risk and an effect size).
#'
#' @param theta          a numeric value indicating the relative minimum difference
#'                       to be detected, \eqn{\theta}.
#' @param sample         a character string specifying one- or two-sample t-test,
#'                       must be one of \code{"two.sample"} (default) or \code{"one.sample"}.
#' @param alternative    a character string specifying the alternative hypothesis,
#'                       must be one of \code{"two.sided"} (default), \code{"greater"} or \code{"less"}.
#' @param alpha          type-I-risk, \eqn{\alpha}.
#' @param beta           type-II-risk, \eqn{\beta}.
#' @param check          logical: if \code{TRUE}, argument specification is checked.
#' @param output         logical: if \code{TRUE}, output is shown.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at},
#'
#' @seealso
#' \code{\link{size.prop}}, \code{\link{size.cor}}
#'
#' @references
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology - Using R and SPSS}.
#' New York: John Wiley & Sons.
#'
#' Rasch, D., Pilz, J., Verdooren, L. R., & Gebhardt, G. (2011).
#' \emph{Optimal experimental design with R}. Boca Raton: Chapman & Hall/CRC.
#'
#' @return Returns an object of class \code{size} with following entries:
#'
#' \tabular{ll}{
#'   \code{call}      \tab function call \cr
#'   \code{type}      \tab type of the test (i.e., arithmetic mean) \cr
#'   \code{spec}      \tab specification of function arguments \cr
#'   \code{res}       \tab list with the result, i.e., optimal sample size \cr
#' }
#'
#' @export
#'
#' @examples
#' #--------------------------------------
#' # Two-sided one-sample test
#' # H0: mu = mu.0, H1: mu != mu.0
#' # alpha = 0.05, beta = 0.2, theta = 0.5
#'
#' size.mean(theta = 0.5, sample = "one.sample",
#'           alternative = "two.sided", alpha = 0.05, beta = 0.2)
#'
#' #--------------------------------------
#' # One-sided one-sample test
#' # H0: mu <= mu.0, H1: mu > mu.0
#' # alpha = 0.05, beta = 0.2, theta = 0.5
#'
#' size.mean(theta = 0.5, sample = "one.sample",
#'           alternative = "greater", alpha = 0.05, beta = 0.2)
#'
#' #--------------------------------------
#' # Two-sided two-sample test
#' # H0: mu.1 = mu.2, H1: mu.1 != mu.2
#' # alpha = 0.01, beta = 0.1, theta = 1
#'
#' size.mean(theta = 1, sample = "two.sample",
#'           alternative = "two.sided", alpha = 0.01, beta = 0.1)
#'
#' #--------------------------------------
#' # One-sided two-sample test
#' # H0: mu.1 <= mu.2, H1: mu.1 > mu.2
#' # alpha = 0.01, beta = 0.1, theta = 1
#'
#' size.mean(theta = 1, sample = "two.sample",
#'           alternative = "greater", alpha = 0.01, beta = 0.1)
size.mean <- function(theta, sample = c("two.sample", "one.sample"),
                      alternative = c("two.sided", "less", "greater"),
                      alpha = 0.05, beta = 0.1, check = TRUE, output = TRUE) {

  ####################################################################################
  # Input check

  # Check input 'check'
  if (isFALSE(isTRUE(check) | isFALSE(check))) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }

  #.........................................

  if (isTRUE(check)) {

    # Check input 'theta'
    if (missing(theta)) {

      stop("Please specify a numeric value for the argument 'theta'", call. = FALSE)

    }

    if (theta <= 0) {

      stop("Argument theta out of bound, specify a value > 0", call. = FALSE)

    }

    ###

    if (!all(sample %in% c("two.sample", "one.sample"))) {

      stop("Argument sample should be \"two.siample\" or \"one.sample\"", call. = FALSE)

    }

    ###

    if (!all(alternative %in% c("two.sided", "less", "greater"))) {

      stop("Argument alternative should be \"two.sided\", \"less\" or \"greater\"", call. = FALSE)

    }

    ###

    if (alpha <= 0 || alpha >= 1) {

      stop("Argument alpha out of bound, specify a value between 0 and 1", call. = FALSE)

    }

    ###

    if (beta <= 0 || beta >= 1) {

      stop("Argument beta out of bound, specify a value between 0 and 1", call. = FALSE)

    }

  }

  ####################################################################################
  # Main function

  # one or two sample
  sample <- ifelse(all(c("two.sample", "one.sample") %in% alternative), "two.sample", sample)
  samp <- switch(sample, one.sample = 1, two.sample = 2)

  # two- or one-sided test
  alternative <- ifelse(all(c("two.sided", "less", "greater") %in% alternative), "two.sided", alternative)

  ###

  #-------------------------------------------------
  # two.sided
  if (alternative == "two.sided") {

    p.body <- quote({
      nu <- (n - 1) * samp
      qu <- qt(alpha / 2, nu, lower = FALSE)
      pt(qu, nu, ncp = sqrt(n / samp) * theta, lower = FALSE) + pt(-qu, nu, ncp = sqrt(n / samp) * theta, lower = TRUE)
    })

  #-------------------------------------------------
  # one-sided
  } else {

    p.body <- quote({
      nu <- (n - 1) * samp
      pt(qt(alpha, nu, lower = FALSE), nu, ncp = sqrt(n / samp) * theta, lower = FALSE)
    })

  }

  #-----------------------------------------------------------------------------------

  n <- uniroot(function(n) eval(p.body) - (1 - beta) , c(2 + 1e-10, 1e+07))$root

  ####################################################################################
  # Return object

  object <- list(call = match.call(),
                 type = "mean",
                 spec = list(theta = theta, sample = sample, alternative = alternative, alpha = alpha, beta = beta),
                 res = list(n = n))

  class(object) <- "size"

  ####################################################################################
  # Output

  if (isTRUE(output)) { print(object) }

  return(invisible(object))

}