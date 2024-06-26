#' Sample Size Determination for Testing Proportions
#'
#' This function performs sample size computation for the one-sample and two-sample
#' test for proportions based on precision requirements (i.e., type-I-risk,
#' type-II-risk and an effect size).
#'
#' @param pi          a number indicating the true value of the probability
#'                    under the null hypothesis (one-sample test), \eqn{\pi}.0
#'                    or a number indicating the true value of the probability
#'                    in group 1 (two-sample test), \eqn{\pi}.1.
#' @param delta       minimum difference to be detected, \eqn{\delta}.
#' @param sample      a character string specifying one- or two-sample
#'                    proportion test, must be one of \code{"two.sample"} (default)
#'                    or \code{"one.sample"}.
#' @param alternative a character string specifying the alternative hypothesis,
#'                    must be one of \code{"two.sided"} (default), \code{"less"}
#'                    or \code{"greater"}.
#' @param alpha       type-I-risk, \eqn{\alpha}.
#' @param beta        type-II-risk, \eqn{\beta}.
#' @param correct     a logical indicating whether continuity correction should
#'                    be applied.
#' @param write       a character string naming a text file with file extension
#'                    \code{".txt"} (e.g., \code{"Output.txt"}) for writing the
#'                    output into a text file.
#' @param append      logical: if \code{TRUE} (default), output will be appended
#'                    to an existing text file with extension \code{.txt} specified
#'                    in \code{write}, if \code{FALSE} existing text file will be
#'                    overwritten.
#' @param check       logical: if \code{TRUE} (default), argument specification
#'                    is checked.
#' @param output      logical: if \code{TRUE} (default), output is shown.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at},
#'
#' @seealso
#' \code{\link{size.mean}}, \code{\link{size.cor}}
#'
#' @references
#' Fleiss, J. L., Levin, B., & Paik, M. C. (2003). \emph{Statistical methods for
#' rates and proportions} (3rd ed.). John Wiley & Sons.
#'
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology
#' - Using R and SPSS}. John Wiley & Sons.
#'
#' Rasch, D., Pilz, J., Verdooren, L. R., & Gebhardt, G. (2011).
#' \emph{Optimal experimental design with R}. Chapman & Hall/CRC.
#'
#' @return Returns an object of class \code{misty.object} with following entries:
#'
#' \tabular{ll}{
#'   \code{call}      \tab function call \cr
#'   \code{type}      \tab type of the test (i.e., proportion) \cr
#'   \code{args}      \tab specification of function arguments \cr
#'   \code{result}    \tab list with the result, i.e., optimal sample size \cr
#' }
#'
#' @export
#'
#' @examples
#' #----------------------------------------------------------------------------
#' # Example 1: Two-sided one-sample test
#' # H0: pi = 0.5, H1: pi != 0.5
#' # alpha = 0.05, beta = 0.2, delta = 0.2
#'
#' size.prop(pi = 0.5, delta = 0.2, sample = "one.sample",
#'           alternative = "two.sided", alpha = 0.05, beta = 0.2)
#'
#' #----------------------------------------------------------------------------
#' # Example 2: Two-sided one-sample test
#' # H0: pi = 0.5, H1: pi != 0.5
#' # alpha = 0.05, beta = 0.2, delta = 0.2
#' # with continuity correction
#'
#' size.prop(pi = 0.5, delta = 0.2, sample = "one.sample",
#'           alternative = "two.sided", alpha = 0.05, beta = 0.2,
#'           correct = TRUE)
#'
#' #----------------------------------------------------------------------------
#' # Example 3: One-sided one-sample test
#' # H0: pi <= 0.5, H1: pi > 0.5
#' # alpha = 0.05, beta = 0.2, delta = 0.2
#'
#' size.prop(pi = 0.5, delta = 0.2, sample = "one.sample",
#'           alternative = "less", alpha = 0.05, beta = 0.2)
#'
#' #----------------------------------------------------------------------------
#' # Example 3: Two-sided two-sample test
#' # H0: pi.1 = pi.2 = 0.5, H1: pi.1 != pi.2
#' # alpha = 0.01, beta = 0.1, delta = 0.2
#'
#' size.prop(pi = 0.5, delta = 0.2, sample = "two.sample",
#'           alternative = "two.sided", alpha = 0.01, beta = 0.1)
#'
#' #----------------------------------------------------------------------------
#' # Example 4: One-sided two-sample test
#' # H0: pi.1 <=  pi.1 = 0.5, H1: pi.1 > pi.2
#' # alpha = 0.01, beta = 0.1, delta = 0.2
#'
#' size.prop(pi = 0.5, delta = 0.2, sample = "two.sample",
#'           alternative = "greater", alpha = 0.01, beta = 0.1)
size.prop <- function(pi = 0.5, delta, sample = c("two.sample", "one.sample"),
                      alternative = c("two.sided", "less", "greater"),
                      alpha = 0.05, beta = 0.1, correct = FALSE, write = NULL,
                      append = TRUE, check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # Check input 'delta'
    if (isTRUE(missing(delta))) { stop("Please specify a numeric value for the argument 'delta'.", call. = FALSE) }

    if (isTRUE(delta <= 0L)) { stop("Argument delta out of bound, specify a value > 0.", call. = FALSE) }

    if (isTRUE(pi >= 1L|| pi <= 0L)) { stop("Argument pi out of bound, specify a value between 0 and 1.", call. = FALSE) }

    if (isTRUE(!all(sample %in% c("two.sample", "one.sample")))) { stop("Argument sample should be \"two.siample\" or \"one.sample\".", call. = FALSE) }

    if (isTRUE(!all(alternative %in% c("two.sided", "less", "greater")))) { stop("Argument alternative should be \"two.sided\", \"less\", or \"greater\".", call. = FALSE) }

    if (isTRUE(alpha <= 0L || alpha >= 1L)) { stop("Argument alpha out of bound, specify a value between 0 and 1.", call. = FALSE) }

    if (isTRUE(beta <= 0L || beta >= 1L)) { stop("Argument beta out of bound, specify a value between 0 and 1.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## sample ####

  # one or two sample
  sample <- ifelse(all(c("two.sample", "one.sample") %in% alternative), "two.sample", sample)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## alternative ####

  # two- or one-sided test
  alternative <- ifelse(all(c("two.sided", "less", "greater") %in% alternative), "two.sided", alternative)


  if (isTRUE(alternative == "two.sided")) {

    if (isTRUE((pi + delta) >= 1L || (pi - delta) <= 0L)) { stop("Value (pi + delta) or (pi - delta) out of bound", call. = FALSE) }

  } else {

    # one-sample
    if (isTRUE(sample == "one.sample")) {

      if (isTRUE(alternative == "less")) {

        if (isTRUE((pi - delta) <= 0L)) { stop("Value (pi - delta) out of bound", call. = FALSE) }

      } else {

        if (isTRUE((pi + delta) >= 1L)) { stop("Value (pi + delta) out of bound", call. = FALSE) }

      }

    # two-sample
    } else {

      if (isTRUE(alternative == "less")) {

        if (isTRUE((pi + delta) >= 1L)) { stop("Value (pi + delta) out of bound", call. = FALSE) }

      } else {

        if (isTRUE((pi - delta) <= 0L)) { stop("Value (pi - delta) out of bound", call. = FALSE) }

      }

    }

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  side <- switch(alternative, two.sided = 2L, less = 1L, greater = 1L)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Two-sample ####

  if (isTRUE(sample == "two.sample")) {

    pi.1 <- pi
    pi.2 <- switch(alternative, two.sided = pi.1 + delta, less = pi.1 + delta, greater = pi.1 - delta)

    p.body <- quote({
      pnorm((sqrt(n) * abs(pi.1 - pi.2) - (qnorm(1L - alpha / side) * sqrt((pi.1 + pi.2) * (1L - (pi.1 + pi.2) / 2L)))) / sqrt(pi.1 * (1L - pi.1) + pi.2 * (1L - pi.2)))
    })

    n <- uniroot(function(n) eval(p.body) - (1L - beta), c(1L, 1e+07))$root

    if (isTRUE(correct == TRUE)) {

      n <- ceiling(n)
      n <- (n / 4L) * (1L + sqrt(1L + 4L / (n * delta)))^2L

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## One-sample ####

  } else {

    pi.0 <- pi
    pi.1 <- switch(alternative, two.sided = pi.0 + delta, less = pi.0 - delta, greater = pi.0 + delta)

    n <- ((qnorm(1L - alpha / side) * sqrt(pi.0 * (1L - pi.0)) + qnorm(1L - beta) * sqrt(pi.1 * (1L - pi.1))) / (pi.1 - pi.0))^2L

    if (isTRUE(correct == TRUE)) {

      n <- ceiling(n)
      n <- n + 1L / (qnorm(1L - alpha / side) * sqrt(pi.0 * (1L - pi.0) / n) + qnorm(1L - beta) * sqrt(pi.1 * (1L - pi.1) / n))

    }

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "size", size = "prop",
                 args = list(delta = delta, pi = pi, sample = sample, alternative = alternative,
                             alpha = alpha, beta = beta, correct = correct,
                             write = write, append = append),
                 result = list(n = n))

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

  if (isTRUE(output)) { print(object) }

  return(invisible(object))

}
