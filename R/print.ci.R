#' Print ci object
#'
#' This function prints the \code{ci} object
#'
#' @param x         \code{ci} object.
#' @param sort.var  logical: if \code{TRUE}, output is sorted by variables.
#' @param digits    an integer value indicating the number of decimal places to be used.
#' @param check     logical: if \code{TRUE}, argument specification is checked.
#' @param ...       further arguments passed to or from other methods.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{ci.mean}}, \code{\link{ci.mean.diff}}, \code{\link{ci.median}},
#' \code{\link{ci.prop}}, \code{\link{ci.var}}, \code{\link{ci.sd}}
#'
#' @method print ci
#'
#' @export
#'
#' @examples
#' dat <- data.frame(x = c(3, 2, 4, 5, 1, 6, 7, 4),
#'                   group = c(1, 1, 1, 1, 2, 2, 2, 2),
#'                   stringsAsFactors = FALSE)
#'
#' # Two-Sided 95% Confidence Interval for x
#' dat.mean <- ci.mean(dat$x, output = FALSE)
#'
#' # Print ci object with 3 digits
#' print(dat.mean, digits = 3)
print.ci <- function(x, sort.var = x$args$sort.var, digits = x$args$digits,
                     check = TRUE, ...) {

  ####################################################################################
  # Input Check

  if (isTRUE(check)) {

    #......
    # Check input 'sort.var'
    if (isFALSE(isTRUE(sort.var) || isFALSE(sort.var))) {

      stop("Please specify TRUE or FALSE for the argument 'sort.var'.", call. = FALSE)

    }

    #......
    # Check input 'digits'
    if (digits %% 1L != 0L || digits < 0L) {

      stop("Specify a positive integer number for the argument 'digits'", call. = FALSE)

    }

  }

  #......
  # Print object
  print.object <- x$result

  #......
  # Variables to round
  print.round <- switch(x$type,
                        mean = c("m", "sd", "low", "upp"),
                        mean.diff.i = c("m1", "sd1", "m2", "sd2", "m.diff", "low", "upp"),
                        mean.diff.p = c("m1", "sd1", "m2", "sd2", "m.diff", "sd.diff", "low", "upp"),
                        prop.diff.i = c("p1", "p2", "p.diff", "low", "upp"),
                        prop.diff.p = c("p1", "p2", "p.diff", "low", "upp"),
                        median = c("med", "iqr", "low", "upp"),
                        prop = c("prop", "low", "upp"),
                        var = c("m", "var", "low", "upp"),
                        sd = c("m", "sd", "low", "upp"))

  ####################################################################################
  # Main Function

  #----------------------------------------
  # No grouping

  if (is.null(x$data$group) && is.null(x$data$split)) {

    #......
    # Print names
    print.names <- switch(x$type,
                          mean = c("Variable", "n", "nNA", "pNA", "M", "SD", "Low", "Upp"),
                          mean.diff.i = c("Variable", "n1", "nNA1", "M1", "SD1", "n2", "nNA2", "M2", "SD2", "M.Diff", "Low", "Upp"),
                          mean.diff.p = c("Variable", "n", "nNA1", "M1", "SD1", "nNA2", "M2", "SD2", "M.Diff", "SD.Diff", "Low", "Upp"),
                          prop.diff.i = c("Variable", "n1", "nNA1", "p1", "n2", "nNA2", "p2", "p.Diff", "Low", "Upp"),
                          prop.diff.p = c("Variable", "n", "nNA1", "p1", "nNA2", "p2", "p.Diff", "Low", "Upp"),
                          median = c("Variable", "n", "nNA", "pNA", "Med", "IQR", "Low", "Upp"),
                          prop = c("Variable", "n", "nNA", "pNA", "Prop", "Low", "Upp"),
                          var = c("Variable", "n", "nNA", "pNA", "M", "Var", "Low", "Upp"),
                          sd = c("Variable", "n", "nNA", "pNA", "M", "SD", "Low", "Upp"))

    #......
    # Round
    print.object[, print.round] <- sapply(print.round, function(y) ifelse(!is.na(print.object[, y]),
                                                                          formatC(print.object[, y], digits = digits, format = "f"), NA))

    #......
    # Percentages
    if (!x$type %in% c("mean.diff.i", "mean.diff.p", "prop.diff.i", "prop.diff.p")) {

      print.object[, "pNA"] <- round(print.object[, "pNA"], digits = 2L)
      print.object[, "pNA"] <- paste0(print.object[, "pNA"], "%")

    }

    #......
    # Col names
    print.object <- rbind(print.names, print.object)

    #......
    # Format
    print.object[, 1L] <- format(print.object[, 1L, drop = FALSE], justify = "left")

    print.object[, -1L] <- apply(print.object[, -1L, drop = FALSE], 2, function(y) format(y, justify = "right"))

    if (length(unique(x$result$variable)) == 1L) {

      print.object <- print.object[, -1L]

    } else {

      print.object[1L, 1L] <- paste0(" ", print.object[1L, 1L], " ", collapse = "")
      print.object[-1L, 1L] <- paste0("  ", print.object[-1L, 1L])

    }

    print.object[, 1L] <- paste0(" ", print.object[, 1L])

    #......
    # Print output
    cat(paste(switch(x$type,
                     mean = "Arithmetic Mean with",
                     mean.diff.i = "Difference in Arithmetics from Independent Samples with",
                     mean.diff.p = "Difference in Arithmetics from Paired Samples with",
                     prop.diff.i = "Difference in Proportions from Independent Samples with",
                     prop.diff.p = "Difference in Proportions from Paired Samples with",
                     median = "Median with",
                     prop = "Proportion with",
                     var = "Variance with",
                     sd = "Standard Deviation with"), switch(x$args$alternative,
                                                             two.sided = "Two-Sided",
                                                             less = "One-Sided",
                                                             greater = "One-Sided"),
              paste0(round(x$args$conf.level * 100L, digits = 2L), "%"), "Confidence Interval\n\n"))

    write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE)

    #......
    # Notes

    # Arithmetic mean
    if (x$type == "mean") {

      if (!is.null(x$args$sigma)) {

        cat(paste0("\n Note. Known population SD: Sigma = ", round(x$args$sigma, digits = 2L), "\n"))

      }

    }

    # Difference in arithmetic mean from independent samples
    if (x$type == "mean.diff.i") {

      if (isTRUE(x$args$var.equal) && is.null(x$args$sigma)) {

        cat(paste0("\n Note. Equal population variance assumption"))

      }

      if (!is.null(x$args$sigma)) {

        if (length(unique(x$args$sigma)) == 1L) {

          cat(paste0("\n Note. Known equal population SD: Sigma = ", round(unique(x$args$sigma), digits = 2L), "\n"))

        } else if (length(unique(x$args$sigma)) == 2L) {

          cat(paste0("\n Note. Known unequal population SDs: Sigma1 = ", round(x$args$sigma[1L], digits = 2L), ", ",
                                                            "Sigma2 = ", round(x$args$sigma[2L], digits = 2L), "\n"))

        }

      }

    }

    # Difference in arithmetic mean from paired samples
    if (x$type == "mean.diff.p") {

      if (!is.null(x$args$sigma)) {

        cat(paste0("\n Note. Known population SD of difference scores: Sigma = ", round(x$args$sigma, digits = 2L), "\n"))

      }

    }

  #----------------------------------------
  # Grouping
  } else if (!is.null(x$data$group) && is.null(x$data$split)) {

    #......
    # Print names
    print.names <- switch(x$type,
                          mean = c("Group", "Variable", "n", "nNA", "pNA", "M", "SD", "Low", "Upp"),
                          mean.diff.i = c("Group", "Variable", "n1", "nNA1", "M1", "SD1", "n2", "nNA2", "M2", "SD2", "M.Diff", "Low", "Upp"),
                          mean.diff.p = c("Group", "Variable", "n", "nNA1", "M1", "SD1", "nNA2", "M2", "SD2", "M.Diff", "SD.Diff", "Low", "Upp"),
                          prop.diff.i = c("Group", "Variable", "n1", "nNA1", "p1", "n2", "nNA2", "p2", "p.Diff", "Low", "Upp"),
                          prop.diff.p = c("Group", "Variable", "n", "nNA1", "p1", "nNA2", "p2", "p.Diff", "Low", "Upp"),
                          median = c("Group", "Variable", "n", "nNA", "pNA", "Med", "IQR", "Low", "Upp"),
                          prop = c("Group", "Variable", "n", "nNA", "pNA", "Prop", "Low", "Upp"),
                          var = c("Group", "Variable", "n", "nNA", "pNA", "M", "Var", "Low", "Upp"),
                          sd = c("Group", "Variable", "n", "nNA", "pNA", "M", "SD", "Low", "Upp"))

    # Sort by variables
    if (isTRUE(sort.var)) {

      print.object <- print.object[order(print.object[, "variable"]), ]

    }

    # Round
    print.object[, print.round] <- sapply(print.round, function(y) ifelse(!is.na(print.object[, y]),
                                                                          formatC(print.object[, y], digits = digits, format = "f"), NA))


    # Percentages
    if (!x$type %in% c("mean.diff.i", "mean.diff.p", "prop.diff.i", "prop.diff.p")) {

      print.object[, "pNA"] <- round(print.object[, "pNA"], digits = 2)
      print.object[, "pNA"] <- paste0(print.object[, "pNA"], "%")

    }

    # Col names
    print.object <- rbind(print.names, print.object)

    # Format
    print.object[, 1L] <- format(print.object[, 1L], justify = "left")
    print.object[, 2L] <- format(print.object[, 2L], justify = "left")

    print.object[, -c(1L:2L)] <- apply(print.object[, -c(1L:2L)], 2, format, justify = "right")

    if (length(unique(x$result$variable)) == 1L) {

      print.object <- print.object[, -2L]

    }

    print.object[1L, 1L] <- paste0(" ", print.object[1L, 1L], " ", collapse = "")
    print.object[-1L, 1L] <- paste0("  ", print.object[-1L, 1L])

    print.object[1L, 2L] <- paste0(" ", print.object[1L, 2L], " ", collapse = "")
    print.object[-1L, 2L] <- paste0("  ", print.object[-1L, 2L])

    print.object[, -c(1L:2L)] <- apply(print.object[, -c(1L:2L)], 2, format, justify = "right")

    #......
    # Print output
    cat(paste(switch(x$type,
                     mean = "Arithmetic Mean with",
                     mean.diff.i = "Difference in Arithmetic Means from Independent Samples with",
                     mean.diff.p = "Difference in Arithmetic Means from Paired Samples with",
                     prop.diff.i = "Difference in Proportions from Independent Samples with",
                     prop.diff.p = "Difference in Proportions from Paired Samples with",
                     median = "Median with",
                     prop = "Proportion with",
                     var = "Variance with",
                     sd = "Standard Deviation with"), switch(x$args$alternative,
                                                             two.sided = "Two-Sided",
                                                             less = "One-Sided",
                                                             greater = "One-Sided"),
              paste0(round(x$args$conf.level * 100L, digits = 2L), "%"), "Confidence Interval\n\n"))

    write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE)

    #......
    # Note

    # Difference in arithmetic mean from independent samples
    if (x$type == "mean.diff.i") {

      if (isTRUE(x$args$var.equal)) {

        cat(paste0("\n Note. Equal population variance assumption\n"))

      }
    }

  #----------------------------------------
  # Split
  } else if (!is.null(x$data$split)) {

    #......
    # Format
    for (i in names(print.object)) {

      # Round
      print.object[[i]][, print.round] <- sapply(print.round, function(y) ifelse(!is.na(print.object[[i]][, y]),
                                                                                 formatC(print.object[[i]][, y], digits = digits, format = "f"), NA))

      # Percentages
      if (!x$type %in% c("mean.diff.i", "mean.diff.p", "prop.diff.i", "prop.diff.p")) {

        print.object[[i]][, "pNA"] <- round(print.object[[i]][, "pNA"], digits = 2L)
        print.object[[i]][, "pNA"] <- paste0(print.object[[i]][, "pNA"], "%")

      }

      #......
      # No grouping
      if (is.null(x$data$group)) {

        #......
        # Print names
        print.names <- switch(x$type,
                              mean = c("Variable", "n", "nNA", "pNA", "M", "SD", "Low", "Upp"),
                              mean.diff.i = c("Variable", "n1", "nNA1", "M1", "SD1", "n2", "nNA2", "M2", "SD2", "M.Diff", "Low", "Upp"),
                              mean.diff.p = c("Variable", "n", "nNA1", "M1", "SD1", "nNA2", "M2", "SD2", "M.Diff", "SD.Diff", "Low", "Upp"),
                              prop.diff.i = c("Variable", "n1", "nNA1", "p1", "n2", "nNA2", "p2", "p.Diff", "Low", "Upp"),
                              prop.diff.p = c("Variable", "n", "nNA1", "p1", "nNA2", "p2", "p.Diff", "Low", "Upp"),
                              median = c("Variable", "n", "nNA", "pNA", "Med", "IQR", "Low", "Upp"),
                              prop = c("Variable", "n", "nNA", "pNA", "Prop", "Low", "Upp"),
                              var = c("Variable", "n", "nNA", "pNA", "M", "Var", "Low", "Upp"),
                              sd = c("Variable", "n", "nNA", "pNA", "M", "SD", "Low", "Upp"))

        # Col names
        print.object[[i]] <- rbind(print.names, print.object[[i]])

        if (length(unique(x$result[[i]]$variable)) == 1L) {

          print.object[[i]] <- print.object[[i]][, -1L]

        }

      #......
      # Grouping
      } else {

        #......
        # Print names
        print.names <- switch(x$type,
                              mean = c("Group", "Variable", "n", "nNA", "pNA", "M", "SD", "Low", "Upp"),
                              mean.diff.i = c("Group", "Variable", "n1", "nNA1", "M1", "SD1", "n2", "nNA2", "M2", "SD2", "M.Diff", "Low", "Upp"),
                              mean.diff.p = c("Group", "Variable", "n", "nNA1", "M1", "SD1", "nNA2", "M2", "SD2", "M.Diff", "SD.Diff", "Low", "Upp"),
                              prop.diff.i = c("Group", "Variable", "n1", "nNA1", "p1", "n2", "nNA2", "p2", "p.Diff", "Low", "Upp"),
                              prop.diff.p = c("Group", "Variable", "n", "nNA1", "p1", "nNA2", "p2", "p.Diff", "Low", "Upp"),
                              median = c("Group", "Variable", "n", "nNA", "pNA", "Med", "IQR", "Low", "Upp"),
                              prop = c("Group", "Variable", "n", "nNA", "pNA", "Prop", "Low", "Upp"),
                              var = c("Group", "Variable", "n", "nNA", "pNA", "M", "Var", "Low", "Upp"),
                              sd = c("Group", "Variable", "n", "nNA", "pNA", "M", "SD", "Low", "Upp"))

        # Sort by variables
        if (isTRUE(sort.var)) {

          print.object[[i]] <- print.object[[i]][order(print.object[[i]][, "variable"]), ]

        }

        # Col names
        print.object[[i]] <- rbind(print.names, print.object[[i]])

        if (length(unique(x$result[[i]]$variable)) == 1L) {

          print.object[[i]] <- print.object[[i]][, -2L]

        }

      }

      #......
      # Format
      if (length(unique(x$result[[i]]$variable)) == 1L && is.null(x$data$group)) {

        print.object[[i]][-1L, 1L] <- paste0("  ", print.object[[i]][-1L, 1L])
        print.object[[i]] <- format(print.object[[i]], justify = "right")

      } else {

        print.object[[i]][, 1L] <- format(print.object[[i]][, 1L], justify = "left")
        print.object[[i]][, 2L] <- format(print.object[[i]][, 2L], justify = "left")

        print.object[[i]][1L, 1L] <- paste0(" ", print.object[[i]][1L, 1L], " ", collapse = "")
        print.object[[i]][-1L, 1L] <- paste0("  ", print.object[[i]][-1L, 1L])

        print.object[[i]][1L, 2L] <- paste0(" ", print.object[[i]][1L, 2L], " ", collapse = "")
        print.object[[i]][-1L, 2L] <- paste0("  ", print.object[[i]][-1L, 2L])

        print.object[[i]][, -c(1L:2L)] <- apply(print.object[[i]][, -c(1L:2L), drop = FALSE], 2, function(y) format(y, justify = "right"))

        print.object[[i]][, 1L] <- paste0("  ", print.object[[i]][, 1])

      }

    }

    # Print object
    cat(paste(switch(x$type,
                     mean = "Arithmetic Mean with",
                     mean.diff.i = "Difference in Arithmetic Means from Independent Samples with",
                     mean.diff.p = "Difference in Arithmetic Means from Dependent Samples with",
                     prop.diff.i = "Difference in Proportions from Independent Samples with",
                     prop.diff.p = "Difference in Proportions from Dependent Samples with",
                     median = "Median with",
                     prop = "Proportion with",
                     var = "Variance with",
                     sd = "Standard Deviation with"), switch(x$args$alternative,
                                                             two.sided = "Two-Sided",
                                                             less = "One-Sided",
                                                             greater = "One-Sided"),
              paste0(round(x$args$conf.level * 100L, digits = 2L), "%"), "Confidence Interval\n\n"))

    for (i in names(print.object)) {

      cat(" Split Group:", i, "\n")

      write.table(print.object[[i]], quote = FALSE, row.names = FALSE, col.names = FALSE)

      if (i != names(print.object)[length(print.object)]) { cat("\n") }

    }

    #......
    # Note

    # Difference in arithmetic mean from independent samples
    if (x$type == "mean.diff.i") {

      if (isTRUE(x$args$var.equal)) {

        cat(paste0("\n Note. Equal population variance assumption\n"))

      }
    }

  }

}
