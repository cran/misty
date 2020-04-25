#' Print test object
#'
#' This function prints the \code{test} object
#'
#' @param x         \code{test} object.
#' @param digits    an integer value indicating the number of decimal places to be used for
#'                  displaying results.
#' @param p.digits  an integer value indicating the number of decimal places to be used for
#'                  displaying the \emph{p}-value.
#' @param check     logical: if \code{TRUE}, argument specification is checked.
#' @param ...       further arguments passed to or from other methods.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{levenes.test}}, \code{\link{z.test}}
#'
#' @method print test
#'
#' @export
#'
#' @examples
#' #--------------------------------------
#' # Levene's test
#' dat <- data.frame(y = c(2, 1, 4, 5, 3, 7, 8, 4, 1),
#'                   group = c(1, 1, 1, 2, 2, 2, 3, 3, 3))
#'
#' # Levene's test based on the median
#' dat.levene <- levenes.test(y ~ group, data = dat, output = FALSE)
#'
#' # Print test object with 5 digits for the p-value
#' print(dat.levene, p.digits = 5)
#'
#' #--------------------------------------
#' # z-test
#' dat <- data.frame(y = c(2, 1, 4, 5, 3, 7, 8, 1),
#'                   group = c(1, 1, 1, 1, 2, 2, 2, 2))
#'
#' # Two-sided two sample z-test with 95% confidence interval
#' # population standard deviation (SD) = 1.2, equal SD assumption
#' dat.z <- z.test(y ~ group, data = dat, sigma = 1.2, mu = 3, output = FALSE)
#'
#' # Print test object with 5 digits for the p-value
#' print(dat.z, p.digits = 5)
print.test <- function(x, digits = x$args$digits, p.digits = x$args$p.digits, check = TRUE, ...) {

  #----------------------------------------
  # Input Check

  if (isTRUE(check)) {

    #......
    # Check input 'digits'
    if (digits %% 1 != 0L || digits < 0L) {

      warning("Specify a positive integer number for the argument 'digits'", call. = FALSE)

    }

    #......
    # Check input 'p.digits'
    if (p.digits %% 1L != 0L || p.digits < 0L) {

      warning("Specify a positive integer number for the argument 'digits'", call. = FALSE)

    }

  }

  # Print object
  print.object <- x$result

  ####################################################################################
  # Levene's test
  switch(x$type, levene = {

    #---------------------------------------------------------
    # descript object

    # Round
    print.object[["descript"]][, c("m", "var", "low", "upp")] <- sapply(c("m", "var", "low", "upp"),
                                                                        function(y) ifelse(!is.na(print.object[["descript"]][, y]),
                                                                                           formatC(print.object[["descript"]][, y], digits = digits, format = "f"), NA))

    print.object[["descript"]] <- print.object[["descript"]][, -2L]

    # Col names
    print.object[["descript"]] <- rbind(c("Group", "n", "nNA", "pNA", "M", "Var", "Low", "Upp"), print.object[["descript"]])

    # Format
    print.object[["descript"]][, 1L] <- format(print.object[["descript"]][, 1L], justify = "left")

    print.object[["descript"]][, -1L] <- apply(print.object[["descript"]][, -1L], 2, format, justify = "right")

    print.object[["descript"]][1L, 1L] <- paste0(" ", print.object[["descript"]][1L, 1L], collapse = "")
    print.object[["descript"]][-1L, 1L] <- paste0("  ", print.object[["descript"]][-1L, 1L])

    print.object[["descript"]][, 1L] <- format(misty::trim(print.object[["descript"]][, 1L], side = "right"), justify = "left")

    print.object[["descript"]][, 1L] <- paste("", print.object[["descript"]][, 1L])

    #---------------------------------------------------------
    # aov object

    #.....................................
    # Round

    print.object[["aov"]][, "Sum Sq"] <- formatC(print.object[["aov"]][, "Sum Sq"], digits = digits, format = "f")
    print.object[["aov"]][, "Mean Sq"] <- formatC(print.object[["aov"]][, "Mean Sq"], digits = digits, format = "f")
    print.object[["aov"]][1, "F value"] <- formatC(print.object[["aov"]][1, "F value"], digits = digits, format = "f")
    print.object[["aov"]][1, "Pr(>F)"] <- formatC(print.object[["aov"]][1, "Pr(>F)"], digits = p.digits, format = "f")

    #.....................................
    # Format

    print.object[["aov"]] <- rbind(c("Df", "Sum Sq", "Mean Sq", "F", "pval"), print.object[["aov"]])
    print.object[["aov"]] <- cbind(c("", "  Group", "  Residuals"), print.object[["aov"]])

    print.object[["aov"]][3L, c("F value", "Pr(>F)")] <- ""

    print.object[["aov"]][, -1L] <- apply(print.object[["aov"]][, -1L], 2, format, justify = "right")
    print.object[["aov"]][, 1L] <- format(print.object[["aov"]][, 1L], justify = "left")

    #---------------------------------------------------------
    # Print output

    cat(" Levene's Test based on the", switch(x$args$method, median = "Median\n\n",
                                                             mean = "Arithmetic Mean\n\n"))

    if (length(unique(x$data[, 2])) == 2) {

      cat("  Null hypothesis        H0: \u03C3\u00B2\u2081 = \u03C3\u00B2\u2082\n",
          " Alternative hypothesis H1: \u03C3\u00B2\u2081 \u2260 \u03C3\u00B2\u2082\n\n")

    } else {


      cat("  Null hypothesis        H0: \u03C3\u00B2\u1D62 = \u03C3\u00B2\u2C7C for all i and j\n",
          " Alternative hypothesis H1: \u03C3\u00B2\u1D62 \u2260 \u03C3\u00B2\u2C7C for at least one i \u2260 j \n\n")
    }


    write.table(print.object[["descript"]], quote = FALSE, row.names = FALSE, col.names = FALSE, na = "")

    cat("\n")

    write.table(print.object[["aov"]], quote = FALSE, row.names = FALSE, col.names = FALSE, na = "")

  ####################################################################################
  # z-test

  #---------------------------------------------------------
  # One sample z-test
  }, z.test.one = {

    #.....................................
    # Round

    print.object[, c("m", "sd", "low", "upp", "z")] <- vapply(print.object[, c("m", "sd", "low", "upp", "z")], formatC,
                                                              digits = digits, format = "f", FUN.VALUE = character(1))

    print.object[, "pval"] <- formatC(print.object[, "pval"], digits = p.digits, format = "f")

    #.....................................
    # Format

    print.object <- rbind(c("n", "nNA", "M", "SD", "Low", "Upp", "z", "pval"), print.object)

    print.object <- apply(print.object, 2L, format, justify = "right")

    print.object[, 1] <- paste(" ", print.object[, 1])

    #.....................................
    # Print output

    cat(paste0(" One sample z-test with ", "\u03c3 = ", round(x$args$sigma, digits = digits), "\n\n"))

    switch(x$args$alternative,
           two.sided = {

             cat("  Null hypothesis        H0: \u03BC =", x$args$mu, "\n",
                 " Alternative hypothesis H1: \u03BC \u2260", x$args$mu, "\n\n")

           }, less = {

             cat("  Null hypothesis        H0: \u03BC \u2265", x$args$mu, "\n",
                 " Alternative hypothesis H1: \u03BC \u003C", x$args$mu, "\n\n")


           }, greater = {

             cat("  Null hypothesis        H0: \u03BC \u2264", x$args$mu, "\n",
                 " Alternative hypothesis H1: \u03BC \u003E", x$args$mu, "\n\n")

           })

    write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE, na = "")

  #---------------------------------------------------------
  # Two sample z-test
  }, z.test.two = {

    #.....................................
    # Round

    print.object[, c("m1", "sd1", "m2", "sd2", "m.diff", "low", "upp", "z")] <- vapply(print.object[, c("m1", "sd1", "m2", "sd2", "m.diff", "low", "upp", "z")], formatC,
                                                                                       digits = digits, format = "f", FUN.VALUE = character(1))

    print.object[, "pval"] <- formatC(print.object[, "pval"], digits = p.digits, format = "f")

    #.....................................
    # Format

    print.object <- rbind(c("n1", "nNA1", "M1", "SD1", "n2", "nNA2", "M2", "SD2", "M.Diff", "Low", "Upp", "z", "pval"), print.object)

    print.object <- apply(print.object, 2L, format, justify = "right")

    print.object[, 1] <- paste(" ", print.object[, 1])

    #.....................................
    # Print output

    if (length(unique(x$args$sigma)) == 1L) {

      cat(paste0(" Two sample z-test with ", "\u03c3\u2081 = \u03c3\u2082 = ", round(x$args$sigma[1], digits = digits), "\n\n"))

    } else {

      cat(paste0(" Two sample z-test with ", "\u03c3\u2081 = ", x$args$sigma[1], " and ", "\u03c3\u2082 = ", round(x$args$sigma[2], digits = digits), "\n\n"))

    }

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

    write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE, na = "")

  #---------------------------------------------------------
  # Paired z-test
  }, z.test.paired = {

    #.....................................
    # Round

    print.object[, c("m1", "sd1", "m2", "sd2", "m.diff", "sd.diff", "low", "upp", "z")] <- vapply(print.object[, c("m1", "sd1", "m2", "sd2", "m.diff", "sd.diff", "low", "upp", "z")], formatC,
                                                                                                  digits = digits, format = "f", FUN.VALUE = character(1))

    print.object[, "pval"] <- formatC(print.object[, "pval"], digits = p.digits, format = "f")

    #.....................................
    # Format

    print.object <- rbind(c("n", "nNA1", "nNA2", "M1", "SD1", "M2", "SD2", "M.Diff", "SD.Diff", "Low", "Upp", "z", "pval"), print.object)

    print.object <- apply(print.object, 2L, format, justify = "right")

    print.object[, 1] <- paste(" ", print.object[, 1])

    #.....................................
    # Print output

    cat(paste0(" Paired sample z-test with ", "\u03c3(diff) = ", round(x$args$sigma, digits = digits), "\n\n"))

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

    write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE, na = "")

  })

}
