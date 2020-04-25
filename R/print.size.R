#' Print size object
#'
#' This function prints the \code{size} object
#'
#' @param x           \code{size} object.
#' @param ...         further arguments passed to or from other methods.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{size.mean}}, \code{\link{size.prop}}, \code{\link{size.cor}}
#'
#' @references
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology - Using R and SPSS}.
#' New York: John Wiley & Sons.
#'
#' @export
#'
#' @examples
#'
#' #--------------------------------------
#' # Two-sided one-sample test
#' # delta = 0.5
#' # alpha = 0.05, beta = 0.2
#'
#' n <- size.mean(delta = 0.5, sample = "one.sample",
#'                alternative = "two.sided", alpha = 0.05, beta = 0.2,
#'                output = FALSE)
#'
#' print(n)
#'
#' #--------------------------------------
#' # Two-sided one-sample test
#' # H0: pi = 0.5, H1: pi != 0.5
#' # alpha = 0.05, beta = 0.2, delta = 0.2
#'
#' n <- size.prop(delta = 0.2, pi = 0.5, sample = "one.sample",
#'                alternative = "two.sided", alpha = 0.05, beta = 0.2,
#'                output = FALSE)
#'
#' print(n)
#'
#' #--------------------------------------
#  # Two-sided test
#' # H0: rho = 0.3, H1: rho != 0.3
#' # alpha = 0.05, beta = 0.2, delta = 0.2
#'
#' n <- size.cor(delta = 0.2, rho = 0.3, alpha = 0.05, beta = 0.2,
#'               output = FALSE)
#'
#' print(n)
print.size <- function(x, ...) {

  ####################################################################################
  # Main function

  #----------------------------------------
  # Arithmetic mean
  switch(x$type, mean = {

    cat("\nSample size determination for the", ifelse(x$args$sample == "one.sample", "one-sample", "two-sample"), "t-test\n\n")

    ###

    # one-sample
    if (x$args$sample == "one.sample") {

      switch(x$args$alternative,
             two.sided = {

               cat("  Null hypothesis        H0: \u03BC =", x$args$delta, "\n",
                   " Alternative hypothesis H1: \u03BC \u2260", x$args$delta, "\n\n")

             }, less = {

               cat("  Null hypothesis        H0: \u03BC \u2265", x$args$delta, "\n",
                   " Alternative hypothesis H1: \u03BC \u003C", x$args$delta, "\n\n")


             }, greater = {

               cat("  Null hypothesis        H0: \u03BC \u2264", x$args$delta, "\n",
                   " Alternative hypothesis H1: \u03BC \u003E", x$args$delta, "\n\n")

             })

    # two-sample
    } else {

      switch(x$args$alternative,
             two.sided = {

               cat("  Null hypothesis        H0: \u03BC\u2081 = \u03BC\u2082\n",
                   " Alternative hypothesis H1: \u03BC\u2081 \u2260 \u03BC\u2082\n\n")

             }, less = {

               cat("  Null hypothesis        H0: \u03BC\u2081 \u2265 \u03BC\u2082\n",
                   " Alternative hypothesis H1: \u03BC\u2081 \u003C \u03BC\u2082\n\n")


             }, greater = {

               cat("  Null hypothesis        H0: \u03BC\u2081 \u2264 \u03BC\u2082\n",
                   " Alternative hypothesis H1: \u03BC\u2081 \u003E \u03BC\u2082\n\n")

             })

    }

    ###

    cat("  \u03B1 =", x$args$alpha, " \u03B2 =", x$args$beta, " \u03B4 =", x$args$delta, "\n\n")

    if (x$args$sample == "one.sample") {

        cat("  optimal sample size: n =", ceiling(x$result$n), "\n\n")

      } else {

        cat("  optimal sample size: n =", ceiling(x$result$n), "in each group \n\n")

    }

  #----------------------------------------
  # Proportion
  }, prop = {

    if (x$args$correct == TRUE) {

      cat("\nSample size determination for the", ifelse(x$args$sample == "one.sample", "one-sample", "two-sample"), "proportion test with continuity correction\n\n")

    } else {

      cat("\nSample size determination for the", ifelse(x$args$sample == "one.sample", "one-sample", "two-sample"), "proportion test without continuity correction\n\n")

    }

    ###

    # one-sample
    if (x$args$sample == "one.sample") {

      switch(x$args$alternative,
             two.sided = {

               cat("  Null hypothesis        H0: \u03C0 =", x$args$pi, "\n",
                   " Alternative hypothesis H1: \u03C0 \u2260", x$args$pi, "\n\n")

             }, less = {

               cat("  Null hypothesis        H0: \u03C0 \u2265", x$args$pi, "\n",
                   " Alternative hypothesis H1: \u03C0 \u003C", x$args$pi, "\n\n")


             }, greater = {

               cat("  Null hypothesis        H0: \u03C0 \u2264", x$args$pi, "\n",
                   " Alternative hypothesis H1: \u03C0 \u003E", x$args$pi, "\n\n")

             })

    # two-sample
    } else {

      switch(x$args$alternative,
             two.sided = {

               cat("  Null hypothesis        H0: \u03C0\u2081 = \u03C0\u2082\n",
                   " Alternative hypothesis H1: \u03C0\u2081 \u2260 \u03C0\u2082\n\n")

             }, less = {

               cat("  Null hypothesis        H0: \u03C0\u2081 \u2265 \u03C0\u2082\n",
                   " Alternative hypothesis H1: \u03C0\u2081 \u003C \u03C0\u2082\n\n")


             }, greater = {

               cat("  Null hypothesis        H0: \u03C0\u2081 \u2264 \u03C0\u2082\n",
                   " Alternative hypothesis H1: \u03C0\u2081 \u003E \u03C0\u2082\n\n")

             })

    }

    ###

    cat("  \u03B1 =", x$args$alpha, " \u03B2 =", x$args$beta, " \u03B4 =", x$args$delta, "\n\n")

    ###

    if (x$args$sample == "one.sample") {

      cat("  optimal sample size: n =", ceiling(x$result$n), "\n\n")

    } else {

      cat("  optimal sample size: n =", ceiling(x$result$n), "in each group \n\n")

    }

  #----------------------------------------
  # Correlation coefficient
  }, cor = {

    cat("\nSample size determination for Pearson's product-moment correlation coefficient\n\n")

    ###

    switch(x$args$alternative,
           two.sided = {

             cat("  Null hypothesis        H0: \u03C1 =", x$args$rho, "\n",
                 " Alternative hypothesis H1: \u03C1 \u2260", x$args$rho, "\n\n")

           }, less = {

             cat("  Null hypothesis        H0: \u03C1 \u2265", x$args$rho, "\n",
                 " Alternative hypothesis H1: \u03C1 \u003C", x$args$rho, "\n\n")


           }, greater = {

             cat("  Null hypothesis        H0: \u03C1 \u2264", x$args$rho, "\n",
                 " Alternative hypothesis H1: \u03C1 \u003E", x$args$rho, "\n\n")

           })

    ###

    cat("  \u03B1 =", x$args$alpha, " \u03B2 =", x$args$beta, " \u03B4 =", x$args$delta, "\n\n")


    ###

    cat("  optimal sample size: n =", ceiling(x$result$n), "\n\n")

  })

  #-----------------------------------------------------------------------------------

}
