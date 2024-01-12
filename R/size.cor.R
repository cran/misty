#' Sample Size Determination for Testing Pearson's Correlation Coefficient
#'
#' This function performs sample size computation for testing Pearson's product-moment
#' correlation coefficient based on precision requirements (i.e., type-I-risk,
#' type-II-risk and an effect size).
#'
#' @param rho            a number indicating the correlation coefficient under
#'                       the null hypothesis, \eqn{\rho}.0.
#' @param delta          a numeric value indicating the minimum difference to be
#'                       detected, \eqn{\delta}.
#' @param alternative    a character string specifying the alternative hypothesis,
#'                       must be one of \code{"two.sided"} (default), \code{"greater"}
#'                       or \code{"less"}.
#' @param alpha          type-I-risk, \eqn{\alpha}.
#' @param beta           type-II-risk, \eqn{\beta}.
#' @param write          a character string naming a text file with file extension
#'                       \code{".txt"} (e.g., \code{"Output.txt"}) for writing the
#'                       output into a text file.
#' @param append         logical: if \code{TRUE} (default), output will be appended
#'                       to an existing text file with extension \code{.txt} specified
#'                       in \code{write}, if \code{FALSE} existing text file will be
#'                       overwritten.
#' @param check          logical: if \code{TRUE} (default), argument specification
#'                       is checked.
#' @param output         logical: if \code{TRUE} (default), output is shown.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at},
#'
#' @seealso
#' \code{\link{size.mean}}, \code{\link{size.prop}}
#'
#' @references
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology -
#' Using R and SPSS}. New York: John Wiley & Sons.
#'
#' Rasch, D., Pilz, J., Verdooren, L. R., & Gebhardt, G. (2011).
#' \emph{Optimal experimental design with R}. Boca Raton: Chapman & Hall/CRC.
#'
#' @return Returns an object of class \code{misty.object} with following entries:
#'
#' \tabular{ll}{
#'   \code{call}    \tab function call \cr
#'   \code{type}    \tab type of the test (i.e., correlation coefficient) \cr
#'   \code{args}    \tab specification of function arguments \cr
#'   \code{result}  \tab list with the result, i.e., optimal sample size \cr
#' }
#'
#' @export
#'
#' @examples
#' #----------------------------------------------------------------------------
#  # Example 1: Two-sided test
#' # H0: rho = 0.3, H1: rho != 0.3
#' # alpha = 0.05, beta = 0.2, delta = 0.2
#'
#' size.cor(rho = 0.3, delta = 0.2, alpha = 0.05, beta = 0.2)
#'
#' #----------------------------------------------------------------------------
#  # Example 2: One-sided test
#' # H0: rho <= 0.3, H1: rho > 0.3
#' # alpha = 0.05, beta = 0.2, delta = 0.2
#'
#' size.cor(rho = 0.3, delta = 0.2, alternative = "greater", alpha = 0.05, beta = 0.2)
size.cor <- function(rho, delta,
                     alternative = c("two.sided", "less", "greater"),
                     alpha = 0.05, beta = 0.1, write = NULL, append = TRUE,
                     check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # Check input 'delta'
    if (isTRUE(missing(delta))) { stop("Please specify a numeric value for the argument 'delta'.", call. = FALSE) }

    if (isTRUE(delta <= 0L)) { stop("Argument delta out of bound, specify a value > 0.", call. = FALSE) }

    if (isTRUE(is.null(rho))) { rho <- 0L }

    if (isTRUE(rho <= -1L || rho >= 1L)) { stop("Argument rho out of bound, specify a value between -1 and 1.", call. = FALSE) }

    if (isTRUE(!all(alternative %in% c("two.sided", "less", "greater")))) { stop("Argument alternative should be \"two.sided\", \"less\" or \"greater\".", call. = FALSE) }

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

    # Check input 'write'
    if (isTRUE(!is.null(write) && substr(write, nchar(write) - 3L, nchar(write)) != ".txt")) { stop("Please specify a character string with file extenstion '.txt' for the argument 'write'.") }

    # Check input 'append'
    if (isTRUE(!is.logical(append))) { stop("Please specify TRUE or FALSE for the argument 'append'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  # two- or one-sided test
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
                 args = list(delta = delta, rho = rho, alternative = alternative, alpha = alpha, beta = beta,
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

    if (append && isTRUE(file.exists(write))) { write("", file = write, append = TRUE) }

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
