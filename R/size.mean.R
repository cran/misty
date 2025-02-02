#' Sample Size Determination
#'
#' This function performs sample size determination the one-sample and two-sample
#' t-tests, proportions, and Pearson product-moment correlation coefficients
#' based on precision requirements (i.e., type-I-risk, type-II-risk and an effect
#' size).
#'
#' @param delta       a numeric value indicating the minimum mean difference to
#'                    be detected, \eqn{\delta}.
#' @param pi          a numeric value specified in the function \code{size.prop}
#'                    indicating the true value of the probability under the null
#'                    hypothesis in the one-sample test \eqn{\pi}.0 or a number
#'                    indicating the true value of the probability in group 1 in
#'                    the two-sample test \eqn{\pi}.1.
#' @param rho         a numeric value specified in the function \code{size.cor}
#'                    indicating the correlation coefficient under the null
#'                    hypothesis \eqn{\rho}.0.
#' @param sample      a character string specified in the function \code{size.mean}
#'                    or \code{size.prop} specifying a one- or two-sample t-test
#'                    or a proportion test, i.e., \code{"two.sample"} (default)
#'                    for a two-sample test and \code{"one.sample"} for a one-sample
#'                    test.
#' @param alternative a character string specifying the alternative hypothesis,
#'                    must be one of \code{"two.sided"} (default), \code{"greater"}
#'                    or \code{"less"}.
#' @param alpha       a numeric value indicating the type-I-risk, \eqn{\alpha}.
#' @param beta        a numeric value indicating the type-II-risk, \eqn{\beta}.
#' @param correct     logical: if \code{TRUE}, continuity correction is applied.
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
#' @name size.mean
#'
#' @seealso
#' \code{\link{test.t}}, \code{\link{prop.test}}, \code{\link{cor.test}},
#' \code{\link{cor.matrix}}
#'
#' @references
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology
#' - Using R and SPSS}. John Wiley & Sons.
#'
#' Rasch, D., Pilz, J., Verdooren, L. R., & Gebhardt, G. (2011).
#' \emph{Optimal experimental design with R}.Chapman & Hall/CRC.
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#'
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{size}}{type of test}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{result}}{list with the result, i.e., optimal sample size}
#'
#' @export
#'
#' @examples
#' #----------------------------------------------------------------------------
#' # Example 1: One- and two-sample t-test
#'
#' # Example 1a: One-sample t-test
#' # H0: mu = mu.0, H1: mu != mu.0
#' # alpha = 0.05, beta = 0.2, delta = 0.5
#' size.mean(delta = 0.5, sample = "one.sample",
#'           alternative = "two.sided", alpha = 0.05, beta = 0.2)
#'
#' # Example 1b: One-sided two-sample test
#' # H0: mu.1 >= mu.2, H1: mu.1 < mu.2
#' # alpha = 0.01, beta = 0.1, delta = 1
#' size.mean(delta = 1, sample = "two.sample",
#'           alternative = "less", alpha = 0.01, beta = 0.1)
#'
#' #----------------------------------------------------------------------------
#' # Example 2: One- and two-sample test for proportions
#'
#' # Example 2a: Two-sided one-sample test
#' # H0: pi = 0.5, H1: pi != 0.5
#' # alpha = 0.05, beta = 0.2, delta = 0.2
#' size.prop(pi = 0.5, delta = 0.2, sample = "one.sample",
#'           alternative = "two.sided", alpha = 0.05, beta = 0.2)
#'
#' # Example 2b: One-sided two-sample test
#' # H0: pi.1 <=  pi.1 = 0.5, H1: pi.1 > pi.2
#' # alpha = 0.01, beta = 0.1, delta = 0.2
#' size.prop(pi = 0.5, delta = 0.2, sample = "two.sample",
#'           alternative = "greater", alpha = 0.01, beta = 0.1)
#'
#' #----------------------------------------------------------------------------
#' # Example 3: Testing the Pearson product-moment correlation coefficient
#'
#  # Example 3a: Two-sided test
#' # H0: rho = 0.3, H1: rho != 0.3
#' # alpha = 0.05, beta = 0.2, delta = 0.2
#' size.cor(rho = 0.3, delta = 0.2, alpha = 0.05, beta = 0.2)
#'
#  # Example 3b: One-sided test
#' # H0: rho <= 0.3, H1: rho > 0.3
#' # alpha = 0.05, beta = 0.2, delta = 0.2
#' size.cor(rho = 0.3, delta = 0.2,
#'          alternative = "greater", alpha = 0.05, beta = 0.2)
size.mean <- function(delta, sample = c("two.sample", "one.sample"),
                      alternative = c("two.sided", "less", "greater"),
                      alpha = 0.05, beta = 0.1, write = NULL, append = TRUE,
                      check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("append", "output"), numeric = list(alpha = 1L, beta = 1L),
               s.character = list(sample = c("two.sample", "one.sample")), args = c("alternative", "write1"), envir = environment(), input.check = check)

  # Additional checks
  if (isTRUE(check)) {

    # Check input 'delta'
    if (isTRUE(missing(delta))) { stop("Please specify a numeric value for the argument 'delta'.", call. = FALSE) }

    if (isTRUE(delta <= 0L)) { stop("Argument delta out of bound, specify a value > 0.", call. = FALSE) }

    if (isTRUE(!all(sample %in% c("two.sample", "one.sample")))) { stop("Argument sample should be \"two.siample\" or \"one.sample\".", call. = FALSE) }

    if (isTRUE(!all(alternative %in% c("two.sided", "less", "greater")))) { stop("Argument alternative should be \"two.sided\", \"less\" or \"greater\"", call. = FALSE) }

    if (isTRUE(alpha <= 0L || alpha >= 1L)) { stop("Argument alpha out of bound, specify a value between 0 and 1.", call. = FALSE) }

    if (isTRUE(beta <= 0L || beta >= 1L)) { stop("Argument beta out of bound, specify a value between 0 and 1.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  # One or two sample
  sample <- ifelse(all(c("two.sample", "one.sample") %in% alternative), "two.sample", sample)

  samp <- switch(sample, one.sample = 1L, two.sample = 2L)

  # Two- or one-sided test
  alternative <- ifelse(all(c("two.sided", "less", "greater") %in% alternative), "two.sided", alternative)

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Two-sided ####

  if (isTRUE(alternative == "two.sided")) {

    p.body <- quote({
      nu <- (n - 1L) * samp
      qu <- qt(alpha / 2L, nu, lower = FALSE)
      pt(qu, nu, ncp = sqrt(n / samp) * delta, lower = FALSE) + pt(-qu, nu, ncp = sqrt(n / samp) * delta, lower = TRUE)
    })

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## One-sided ####

  } else {

    p.body <- quote({
      nu <- (n - 1L) * samp
      pt(qt(alpha, nu, lower = FALSE), nu, ncp = sqrt(n / samp) * delta, lower = FALSE)
    })

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Optimal sample size ####

  n <- uniroot(function(n) eval(p.body) - (1L - beta) , c(2L + 1e-10, 1e+07))$root

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "size", size = "mean",
                 args = list(delta = delta, sample = sample, alternative = alternative, alpha = alpha, beta = beta, write = write, append = append),
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

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#' @rdname size.prop
size.prop <- function(pi = 0.5, delta, sample = c("two.sample", "one.sample"),
                      alternative = c("two.sided", "less", "greater"),
                      alpha = 0.05, beta = 0.1, correct = FALSE, write = NULL,
                      append = TRUE, check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("correct", "append", "output"), numeric = list(pi = 1L, alpha = 1L, beta = 1L),
               s.character = list(sample = c("two.sample", "one.sample")), args = c("alternative", "write1"), envir = environment(), input.check = check)

  # Additional checks
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
  ## Sample ####

  # one or two sample
  sample <- ifelse(all(c("two.sample", "one.sample") %in% alternative), "two.sample", sample)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Alternative ####

  # two- or one-sided test
  alternative <- ifelse(all(c("two.sided", "less", "greater") %in% alternative), "two.sided", alternative)


  if (isTRUE(alternative == "two.sided")) {

    if (isTRUE((pi + delta) >= 1L || (pi - delta) <= 0L)) { stop("Value (pi + delta) or (pi - delta) out of bound", call. = FALSE) }

  } else {

    # One-sample
    if (isTRUE(sample == "one.sample")) {

      if (isTRUE(alternative == "less")) {

        if (isTRUE((pi - delta) <= 0L)) { stop("Value (pi - delta) out of bound", call. = FALSE) }

      } else {

        if (isTRUE((pi + delta) >= 1L)) { stop("Value (pi + delta) out of bound", call. = FALSE) }

      }

    # Two-sample
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

    if (isTRUE(correct)) {

      n <- ceiling(n)
      n <- n + 1L / (qnorm(1L - alpha / side) * sqrt(pi.0 * (1L - pi.0) / n) + qnorm(1L - beta) * sqrt(pi.1 * (1L - pi.1) / n))

    }

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "size", size = "prop",
                 args = list(delta = delta, pi = pi, sample = sample, alternative = alternative, alpha = alpha, beta = beta, correct = correct, write = write, append = append),
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

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#' @rdname size.cor
size.cor <- function(rho, delta,
                     alternative = c("two.sided", "less", "greater"),
                     alpha = 0.05, beta = 0.1, write = NULL, append = TRUE,
                     check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("append", "output"), numeric = list(alpha = 1L, beta = 1L), args = c("alternative", "write1"), envir = environment(), input.check = check)

  # Additional checks
  if (isTRUE(check)) {

    # Check input 'delta'
    if (isTRUE(missing(delta))) { stop("Please specify a numeric value for the argument 'delta'.", call. = FALSE) }

    if (isTRUE(delta <= 0L)) { stop("Argument delta out of bound, specify a value > 0.", call. = FALSE) }

    if (isTRUE(is.null(rho))) { rho <- 0L }

    if (isTRUE(rho <= -1L || rho >= 1L)) { stop("Argument rho out of bound, specify a value between -1 and 1.", call. = FALSE) }

    if (isTRUE(alpha <= 0L || alpha >= 1L)) { stop("Argument alpha out of bound, specify a value between 0 and 1.", call. = FALSE) }

    if (isTRUE(beta <= 0L || beta >= 1L)) { stop("Argument beta out of bound, specify a value between 0 and 1.", call. = FALSE) }

    #-----------------------------------------------------------------------------------

    if (isTRUE(alternative == "two.sided")) {

      if (isTRUE((rho + delta) >= 1L || (rho - delta) <= -1L)) { stop("Value (rho + delta) or (rho - delta) out of bound.", call. = FALSE) }

    } else {

      if (isTRUE(alternative == "less")) {

        if (isTRUE((rho - delta) <= -1L)) { stop("Value (rho - delta) out of bound.", call. = FALSE) }

      } else {

        if (isTRUE((rho + delta) >= 1L)) { stop("Value (rho + delta) out of bound.", call. = FALSE) }

      }

    }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  # Two- or one-sided test
  alternative <- ifelse(all(c("two.sided", "less", "greater") %in% alternative), "two.sided", alternative)

  side <- switch(alternative, two.sided = 2L, less = 1L, greater = 1L)

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  rho.0 <- rho
  rho.1 <- switch(alternative, two.sided = rho.0 + delta, less = rho.0 - delta, greater = rho.0 + delta)

  n <- 3L + 4L * ((qnorm(1L - alpha / side) + qnorm(1L - beta)) / (log((1L + rho.1) / (1L - rho.1)) - log((1L + rho.0) / (1L - rho.0))))^2L

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "size", size = "cor",
                 args = list(delta = delta, rho = rho, alternative = alternative, alpha = alpha, beta = beta, write = write, append = append),
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

#_______________________________________________________________________________
