#' Print misty.object object
#'
#' This function prints the \code{misty.object} object
#'
#' @param x          \code{misty.object} object.
#' @param print      a character string or character vector indicating which results to to be printed
#'                   on the console.
#' @param tri        a character string or character vector indicating which triangular of the matrix
#'                   to show on the console, i.e., \code{both} for upper and lower triangular,
#'                   \code{lower} for the lower triangular, and \code{upper} for the upper
#'                   triangular.
#' @param freq       logical: if \code{TRUE}, absolute frequencies will be included in the cross
#'                   tabulation (\code{freq()} function).
#' @param split      logical: if \code{TRUE}, cross table is split in absolute frequencies and percentage(s)
#'                   (\code{crosstab()} function).
#' @param table      logical: if \code{TRUE}, a frequency table with number of observed values (\code{"nObs"}),
#'                   percent of observed values (\code{"pObs"}), number of missing values (\code{"nNA"}), and
#'                   percent of missing values (\code{"pNA"}) is printed for each variable on the console
#'                   (\code{na.descript()} function).
#' @param digits     an integer value indicating the number of decimal places digits to be used for
#'                   displaying results.
#' @param p.digits   an integer indicating the number of decimal places to be used for displaying
#'                   \emph{p}-values.
#' @param icc.digits an integer indicating the number of decimal places to be used for displaying
#'                   intraclass correlation coefficients (\code{multilevel.descript()} function).
#' @param sort.var   logical: if \code{TRUE}, output is sorted by variables.
#' @param order      logical: if \code{TRUE}, variables are ordered from left to right in increasing order
#'                   of missing values (\code{na.descript()} function).
#' @param check      logical: if \code{TRUE}, argument specification is checked.
#' @param ...        further arguments passed to or from other methods.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{alpha.coef}}, \code{\link{ci.mean.diff}}, \code{\link{ci.mean}}, \code{\link{ci.median}},
#' \code{\link{ci.prop.diff}}, \code{\link{ci.prop}}, \code{\link{ci.sd}}, \code{\link{ci.var}},
#' \code{\link{cohens.d}}, \code{\link{collin.diag}}, \code{\link{cont.coef}}, \code{\link{cor.matrix}},
#' \code{\link{cramers.v}}, \code{\link{crosstab}}, \code{\link{descript}}, \code{\link{eta.sq}},
#' \code{\link{freq}}, \code{\link{levenes.test}}, \code{\link{multilevel.descript}},
#' \code{\link{na.auxiliary}}, \code{\link{na.coverage}}, \code{\link{na.descript}},
#' \code{\link{na.pattern}}, \code{\link{omega.coef}}, \code{\link{phi.coef}}, \code{\link{poly.cor}},
#' \code{\link{size.cor}}, \code{\link{size.mean}}, \code{\link{size.prop}}, \code{\link{z.test}},
#'
#' @method print misty.object
#'
#' @export
#'
#' @examples
print.misty.object <- function(x, print = x$args$print, tri = x$args$tri, freq = x$args$freq,
                               split = x$args$split,table = x$args$table, digits = x$args$digits,
                               p.digits = x$args$p.digits, icc.digits = x$args$icc.digits,
                               sort.var = x$args$sort.var, order = x$args$order, check = TRUE, ...) {

  ####################################################################################
  # Input Check

  if (isTRUE(check)) {

    #......
    # Check input 'tri'
    if (!is.null(table)) {

      if (any(!tri %in% c("both", "lower", "upper"))) {

        stop("Character string in the argument 'tri' does not match with \"both\", \"lower\", or \"upper\".",
             call. = FALSE)

      }

    }

    #......
    # Check input 'table'
    if (!is.null(table)) {

      if (!is.logical(table)) {

        stop("Please specify TRUE or FALSE for the argument 'table'.", call. = FALSE)

      }

    }

    #......
    # Check input 'freq'
    if (!is.null(freq)) {

      if (!is.logical(freq)) {

        stop("Please specify TRUE or FALSE for the argument 'freq'.", call. = FALSE)

      }

    }

    #......
    # Check input 'digits'
    if (!is.null(digits)) {

      if (digits %% 1L != 0L || digits < 0L) {

        stop("Specify a positive integer number for the argument 'digits'", call. = FALSE)

      }

    }

    #......
    # Check input 'p.digits'
    if (!is.null(p.digits)) {

      if (p.digits %% 1L != 0L || p.digits < 0L) {

        stop("Specify a positive integer number for the argument 'p.digits'", call. = FALSE)

      }

    }

    #......
    # Check input 'icc.digits'
    if (!is.null(icc.digits)) {

      if (icc.digits %% 1L != 0L || icc.digits < 0L) {

        stop("Specify a positive integer number for the argument 'icc.digits'", call. = FALSE)

      }

    }

    #......
    # Check input 'sort.var'
    if (!is.null(sort.var)) {

      if (!is.logical(sort.var)) {

        stop("Please specify TRUE or FALSE for the argument 'sort.var'.", call. = FALSE)

      }

    }

    #......
    # Check input 'order'
    if (!is.null(order)) {

      if (!is.logical(order)) {

        stop("Please specify TRUE or FALSE for the argument 'order'.", call. = FALSE)

      }

    }

  }

  #......
  # Print object
  print.object <- x$result

  ####################################################################################
  #-----------------------------------------------------------------------------------
  # Coefficient Alpha
  switch(x$type, alpha.coef = {

    if (isTRUE(check)) {

      #......
      # Check input 'print'
      if (!all(print %in% c("all", "alpha", "item"))) {

        stop("Character strings in the argument 'print' do not all match with \"all\", \"alpha\", or \"item\".",
             call. = FALSE)

      }

    }

    #----------------------------------------
    # Arguments

    # Print coefficient alpha and/or item statistic
    if (length(print) == 1L && "all" %in% print) { print <- c("alpha", "item") }

    #-----------------------------------------------------------------------------------
    # Main Function

    #-----------------------------------------
    # Alpha
    if ("alpha" %in% print) {

      if (all(c("low", "upp") %in% names(print.object$alpha))) {

        print.object$alpha$n <- format(paste("", print.object$alpha$n), justify = "right")

        print.object$alpha$items <- format(print.object$alpha$items, justify = "right")

        print.object$alpha$alpha <- formatC(print.object$alpha$alpha, digits = digits, format = "f")

        print.object$alpha$low <- formatC(print.object$alpha$low, digits = digits, format = "f")
        print.object$alpha$upp <- formatC(print.object$alpha$upp, digits = digits, format = "f")

        print.object$alpha <- rbind(c("n", "Items", "Alpha", "Low", "Upp"), print.object$alpha)

        print.object$alpha <- apply(print.object$alpha, 2, function(y) format(y, justify = "right"))

        cat(paste0(ifelse(isTRUE(x$args$std), "Standardized ", "Unstandardized "), "Coefficient Alpha with ",
                   x$args$conf.level*100L, "% Confidence Interval\n\n"))

        write.table(print.object$alpha, quote = FALSE, row.names = FALSE, col.names = FALSE)

      } else {

        print.object$alpha$alpha <- formatC(print.object$alpha$alpha, digits = digits, format = "f")

        print.object$alpha <- rbind(c(" Items", "Alpha"), print.object$alpha)

        print.object$alpha <- apply(print.object$alpha, 2, function(y) format(y, justify = "right"))

        if (!isTRUE(x$args$ordered)) {

          cat(paste0(ifelse(isTRUE(x$args$std), "Standardized ", "Unstandardized "), "Coefficient Alpha\n\n"))

        } else {

          cat("Ordinal Coefficient Alpha\n\n")

        }

        write.table(print.object$alpha, quote = FALSE, row.names = FALSE, col.names = FALSE)

      }

    }

    #-----------------------------------------
    # Item statistics
    if ("item" %in% print && !is.null(print.object$itemstat) && nrow(print.object$itemstat) > 2L) {

      print.object$item$pNA <- paste0(formatC(print.object$item$pNA, digits = 2L, format = "f"), "%")
      print.object$item$m <- formatC(print.object$item$m, digits = 2L, format = "f")
      print.object$item$sd <- formatC(print.object$item$sd, digits = 2L, format = "f")
      print.object$item$min <- formatC(print.object$item$min, digits = 2L, format = "f")
      print.object$item$max <- formatC(print.object$item$max, digits = 2L, format = "f")

      print.object$item$it.cor <- formatC(print.object$item$it.cor, digits = digits, format = "f")
      print.object$item$alpha <- formatC(print.object$item$alpha, digits = digits, format = "f")

      print.object$item <- rbind(c("Variable", "n", "nNA", "pNA", "M", "SD", "Min", "Max", "It.Cor", "Alpha"),
                                 print.object$item)

      # Format
      print.object$item[, 1L] <- format(paste("", print.object$item[, 1]), justify = "left")
      print.object$item[, -1L] <- apply(print.object$item[, -1L], 2, function(y) format(y, justify = "right"))


      if ("alpha" %in% print) { cat("\n") }

      cat("Item-Total Correlation and Coefficient Alpha if Item Deleted\n\n")

      write.table(print.object$item, quote = FALSE, row.names = FALSE, col.names = FALSE)

    }

  ####################################################################################
  #-----------------------------------------------------------------------------------
  # Confidence intervals
  }, ci = {

    #......
    # Variables to round
    print.round <- switch(x$ci,
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
      print.names <- switch(x$ci,
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
      if (!x$ci %in% c("mean.diff.i", "mean.diff.p", "prop.diff.i", "prop.diff.p")) {

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
        print.object[, 1L] <- paste0(" ", print.object[, 1L])

      } else {

        print.object[1L, 1L] <- paste0(" ", print.object[1L, 1L], " ", collapse = "")
        print.object[-1L, 1L] <- paste0("  ", print.object[-1L, 1L])
        print.object[, 1L] <- format(misty::trim(print.object[, 1L], side = "right"), justify = "left")

      }

      #......
      # Print output
      cat(paste(switch(x$ci,
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
      if (x$ci == "mean") {

        if (!is.null(x$args$sigma)) {

          cat(paste0("\n Note. Known population SD: Sigma = ", round(x$args$sigma, digits = 2L), "\n"))

        }

      }

      # Difference in arithmetic mean from independent samples
      if (x$ci == "mean.diff.i") {

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
      if (x$ci == "mean.diff.p") {

        if (!is.null(x$args$sigma)) {

          cat(paste0("\n Note. Known population SD of difference scores: Sigma = ", round(x$args$sigma, digits = 2L), "\n"))

        }

      }

    #----------------------------------------
    # Grouping
    } else if (!is.null(x$data$group) && is.null(x$data$split)) {

      #......
      # Print names
      print.names <- switch(x$ci,
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
      if (!x$ci %in% c("mean.diff.i", "mean.diff.p", "prop.diff.i", "prop.diff.p")) {

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

      print.object[1L, 1L] <- paste0(" ", print.object[1L, 1L], collapse = "")
      print.object[-1L, 1L] <- paste0("  ", print.object[-1L, 1L])

      print.object[, 1L] <- format(misty::trim(print.object[, 1L], side = "right"), justify = "left")

      if (length(unique(x$result$variable)) != 1L) {

        print.object[-1L, 2L] <- paste0(" ", print.object[-1L, 2L])
        print.object[, 2L] <- format(misty::trim(print.object[, 2L], side = "right"), justify = "left")

      }

      #......
      # Print output
      cat(paste(switch(x$ci,
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
      if (x$ci == "mean.diff.i") {

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
        if (!x$ci %in% c("mean.diff.i", "mean.diff.p", "prop.diff.i", "prop.diff.p")) {

          print.object[[i]][, "pNA"] <- round(print.object[[i]][, "pNA"], digits = 2L)
          print.object[[i]][, "pNA"] <- paste0(print.object[[i]][, "pNA"], "%")

        }

        #......
        # No grouping
        if (is.null(x$data$group)) {

          #......
          # Print names
          print.names <- switch(x$ci,
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
          print.names <- switch(x$ci,
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

        # One variable, no group
        if (length(unique(x$result[[i]]$variable)) == 1L && is.null(x$data$group)) {

          print.object[[i]][-1L, 1L] <- paste0("  ", print.object[[i]][-1L, 1L])
          print.object[[i]] <- format(print.object[[i]], justify = "right")

        # One variable, group
        } else if (length(unique(x$result[[i]]$variable)) == 1L && !is.null(x$data$group)) {

          print.object[[i]][, 1L] <- format(print.object[[i]][, 1L], justify = "left")

          print.object[[i]][1L, 1L] <- paste0(" ", print.object[[i]][1L, 1L], " ", collapse = "")
          print.object[[i]][-1L, 1L] <- paste0("  ", print.object[[i]][-1L, 1L])
          print.object[[i]][, 1L] <- format(misty::trim(print.object[[i]][, 1L], side = "right"), justify = "left")

          #print.object[[i]][-1L, 2L] <- paste0(" ", print.object[[i]][-1L, 2L])
          #print.object[[i]][, 2L] <- format(misty::trim(print.object[[i]][, 2L], side = "right"), justify = "left")

          print.object[[i]][, -1L] <- apply(print.object[[i]][, -1L, drop = FALSE], 2, function(y) format(y, justify = "right"))

          print.object[[i]][, 1L] <- paste0("  ", print.object[[i]][, 1])

          # More than one variable, no group
        } else if (length(unique(x$result[[i]]$variable)) > 1L && is.null(x$data$group)) {

          print.object[[i]][, 1L] <- format(print.object[[i]][, 1L], justify = "left")

          print.object[[i]][1L, 1L] <- paste0(" ", print.object[[i]][1L, 1L], " ", collapse = "")
          print.object[[i]][-1L, 1L] <- paste0("  ", print.object[[i]][-1L, 1L])
          print.object[[i]][, 1L] <- format(misty::trim(print.object[[i]][, 1L], side = "right"), justify = "left")

          print.object[[i]][, -1L] <- apply(print.object[[i]][, -1L, drop = FALSE], 2, function(y) format(y, justify = "right"))

          print.object[[i]][, 1L] <- paste0("  ", print.object[[i]][, 1])

        # More than one variable, group
        } else if (length(unique(x$result[[i]]$variable)) > 1L && !is.null(x$data$group)) {

          print.object[[i]][, 1L] <- format(print.object[[i]][, 1L], justify = "left")

          print.object[[i]][1L, 1L] <- paste0(" ", print.object[[i]][1L, 1L], " ", collapse = "")
          print.object[[i]][-1L, 1L] <- paste0("  ", print.object[[i]][-1L, 1L])
          print.object[[i]][, 1L] <- format(misty::trim(print.object[[i]][, 1L], side = "right"), justify = "left")

          print.object[[i]][-1L, 2L] <- paste0(" ", print.object[[i]][-1L, 2L])
          print.object[[i]][, 2L] <- format(misty::trim(print.object[[i]][, 2L], side = "right"), justify = "left")

          print.object[[i]][, -c(1L:2L)] <- apply(print.object[[i]][, -c(1L:2L), drop = FALSE], 2, function(y) format(y, justify = "right"))

          print.object[[i]][, 1L] <- paste0("  ", print.object[[i]][, 1])

        }

      }

      # Print object
      cat(paste(switch(x$ci,
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
      if (x$ci == "mean.diff.i") {

        if (isTRUE(x$args$var.equal)) {

          cat(paste0("\n Note. Equal population variance assumption\n"))

        }
      }

    }

  ####################################################################################
  #-----------------------------------------------------------------------------------
  # Cohen's d
  }, cohens.d = {

    ####################################################################################
    # Main Function

    #---------------------------------------
    # Between-subject design
    if (!isTRUE(x$args$paired)) {

      print.object[, !names(print.object) %in% c("variable", "n1", "n2", "nNA1", "nNA2")] <- vapply(print.object[,  !names(print.object) %in% c("variable", "n1", "n2", "nNA1", "nNA2")], function(y) ifelse(!is.na(y), formatC(y, format = "f", digits = digits), NA), FUN.VALUE = character(nrow(x$result)))
      print.object[, names(print.object) %in% c("n1", "n2", "nNA1", "nNA2")] <- vapply(print.object[,  names(print.object) %in% c("n1", "n2", "nNA1", "nNA2")], function(y) formatC(y, format = "f", digits = 0L), FUN.VALUE = character(nrow(x$result)))

      print.object <- rbind(c("Variable", "n1", "nNA1", "M1", "SD1",  "n2", "nNA2", "M2", "SD2", "M.Diff", "SD", "Estimate", "SE", "Low", "Upp"),
                            print.object)

      print.object[, 1L] <- format(print.object[, 1L], justify = "left")

      # Format
      print.object[, -1L] <- vapply(print.object[, -1L], function(y) format(y, justify = "right"), FUN.VALUE = character(nrow(x$result) + 1L))

      print.object <- vapply(print.object, formatC, format = "f", FUN.VALUE = character(nrow(x$result) + 1L))

      print.object[, 1L] <- paste(" ", print.object[, 1L])

    #---------------------------------------
    # Within-subject design
    } else {

      print.object[, !names(print.object) %in% c("var", "n", "nNA")] <- vapply(print.object[,  !names(print.object) %in% c("var", "n", "nNA")], function(y) formatC(y, format = "f", digits = digits), FUN.VALUE = character(1L))
      print.object[, names(print.object) %in% c("n", "nNA")] <- vapply(print.object[,  names(print.object) %in% c("n", "nNA")], function(y) formatC(y, format = "f", digits = 0L), FUN.VALUE = character(1L))

      print.object <- rbind(c("n", "nNA", "Variable1", "M1", "SD1", "Variable2", "M2", "SD2", "M.Diff", "SD", "Estimate", "SE", "Low", "Upp"),
                            print.object)

      print.object[, 1L] <- format(print.object[, 1L], justify = "left")

      # Format
      print.object[, -1L] <- vapply(print.object[, -1L], function(y) format(y, justify = "right"), FUN.VALUE = character(2))

      print.object <- vapply(print.object, formatC, format = "f", FUN.VALUE = character(2L))

      print.object[, 1L] <- paste(" ", print.object[, 1L])

    }

    ####################################################################################
    # Output

    #---------------------------------------
    # Between-subject design
    if(!isTRUE(x$args$paired)) {

      # Cohens d
      cat(paste0(" Cohen's d for bewteen-subject design with ", round(x$args$conf.level * 100L), "% confidence interval\n\n"))

    } else {

      # Cohens d
      cat(paste0(" Cohen's d for within-subject design with ", round(x$args$conf.level * 100L), "% confidence interval\n\n"))

    }

    # Output
    write.table(print.object, row.names = FALSE, col.names = FALSE, quote = FALSE)

    #---------------------------------------
    # Within-subject design
    if (!isTRUE(x$args$paired)) {

      if (is.null(x$args$ref)) {

        if (isTRUE(x$args$weighted)) {

          if (isTRUE(x$args$correct)) {

            cat("\n Note. SD = weighted pooled standard deviation \n       Applying small sample correction factor")

          } else {

            cat("\n Note. SD = weighted pooled standard deviation\n")

          }

        } else {

          cat("\n Note. SD = unweighted pooled standard deviation\n")

        }

      } else {

        cat(paste0("\n Note. SD = standard deviation of the reference group: ", x$args$ref, "\n"))

      }


    } else {

      if (is.null(x$args$ref)) {

        if (isTRUE(x$args$weighted)) {

          if (isTRUE(x$args$correct)) {

            cat("\n Note. SD = controlling for the correlation between measures \n       Applying small sample correction factor")

          } else {

            cat("\n Note. SD = controlling for the correlation between measures\n")

          }

        } else {

          cat("\n Note. SD = ignoring the correlation between measures\n")

        }

      } else {

        # Between-subject design
        if (!isTRUE(x$args$paired)) {

          cat(paste0("\n Note. SD = standard deviation of the reference group: ", x$args$ref, "\n"))

          # Within-subject design
        } else {

          cat(paste0("\n Note. SD = standard deviation of the reference variable: ", x$args$ref, "\n"))

        }

      }

    }

  ####################################################################################
  #-----------------------------------------------------------------------------------
  # Collinearity Diagnostics
  }, collin.diag = {

    if (isTRUE(check)) {

      #......
      # Check input 'print'
      if (any(!print %in% c("all", "vif", "eigen"))) {

        stop("Character string(s) in the argument 'print' does not match with \"all\", \"vif\", \"eigen\".",
             call. = FALSE)

      }

    }

    #----------------------------------------
    # Arguments

    # Print variance inflation factor and/or eigenvalue
    if (length(print) == 1L && "all" %in% print) { print <- c("vif", "eigen") }

    #-----------------------------------------------------------------------------------
    # Main Function

    cat(" Collinearity Diagnostics\n")

    #-----------------------------------------
    # Tolerance, std. error inflation factor, and variance inflation factor
    if ("vif" %in% print) {

      # Exclude variables "df" and "GVIF"
      print.object$coef <- print.object$coef[, which(!colnames(print.object$coef) %in% c("df", "GVIF"))]

      # Round
      if (any(class(x$model) == "lmerMod")) {

        print.object$coef <- apply(print.object$coef, 2L, function(y) formatC(y, digits = digits, format = "f"))

      } else if (any(class(x$model) == "lme")) {

        print.object$coef[, -5] <- apply(print.object$coef[, -5], 2L, function(y) formatC(y, digits = digits, format = "f"))

        print.object$coef[, 5] <- formatC(print.object$coef[, 5], digits = p.digits, format = "f")

      } else {

        print.object$coef[, -4] <- apply(print.object$coef[, -4], 2L, function(y) formatC(y, digits = digits, format = "f"))

        print.object$coef[, 4] <- formatC(print.object$coef[, 4], digits = p.digits, format = "f")

      }

      # Repace NA with ""
      print.object$coef <- apply(print.object$coef, 2L, function(y) gsub("NA", "", y))

      # Format
      print.object$coef <- apply(print.object$coef, 2L, function(y) format(y, justify = "right"))

      # Format row names
      row.names(print.object$coef) <- paste0("   ", row.names(print.object$coef))

      # Rename variables
      colnames(print.object$coef) <- sub("aGSIF", "SIF", colnames(print.object$coef))
      colnames(print.object$coef) <- sub("aGVIF", "VIF", colnames(print.object$coef))

      # Print object
      cat("\n  Tolerance (Tol), Std. Error Inflation Factor (SIF), and Variance Inflation Factor (VIF)\n\n")

      print(print.object$coef, quote = FALSE, right = TRUE)

      # Note for model involving categorical predictors
      if (any(x$result$vif$df > 1)) {

        cat("\n   Note. Generalized SIF/VIF are computed for terms with more than 1 df\n")

      }

    }

    #-----------------------------------------
    # Eigenvalue, condition index, and variance proportions
    if ("eigen" %in% print) {

      # Round
      print.object$eigen[, -1L] <- apply(print.object$eigen[, -1L], 2L, function(y) formatC(y, digits = digits, format = "f"))

      # Format
      print.object$eigen[, 1L] <- paste0("    ", print.object$eigen[, 1L])

      # Rename variables
      colnames(print.object$eigen) <- sub("dim", "Dim", colnames(print.object$eigen))
      colnames(print.object$eigen) <- sub("eigen", "Eigen", colnames(print.object$eigen))
      colnames(print.object$eigen) <- sub("ci", "CI", colnames(print.object$eigen))

      # Print object
      cat("\n  Eigenvalue (Eigen), Condition Index (CI), and Variance Proportions\n\n")

      print(print.object$eigen, quote = FALSE, right = TRUE, max.levels = 999L,
            row.names = FALSE)

    }

  ####################################################################################
  #-----------------------------------------------------------------------------------
  # Pearson's Contingency Coefficient
  }, cont.coef = {

    ####################################################################################
    # Data and Arguments

    # Print object
    print.object <- x$result

    # Print triangular
    tri <- ifelse(all(c("both", "lower", "upper") %in% tri), "lower", tri)

    ####################################################################################
    # Main Function

    #----------------------------------------
    # Two variables
    if (is.null(dim(print.object))) {

      print.object <- cbind("  Estimate: ", ifelse(!is.na(print.object), formatC(print.object, digits = digits, format = "f"), print.object))

    #----------------------------------------
    # More than two variables
    } else {

      # Format contingenccy coefficients
      print.object <- formatC(print.object, digits = digits, format = "f")

      # Lower and/or upper triangular
      if (tri == "lower") {

        print.object[upper.tri(print.object)] <- ""

      }

      if (tri == "upper") {

        print.object[lower.tri(print.object)] <- ""

      }

      # Set diagonal to "
      diag(print.object) <- ""

      # Format
      row.names(print.object) <- paste("", row.names(print.object))

    }

    ####################################################################################
    # Output

    if (is.null(dim(x$result))) {

      if (isTRUE(x$args$adjust)) {

        cat("Adjusted Contingency Coefficient\n\n")

      } else {

        cat("Contingency Coefficient\n\n")

      }

    } else {

      if (isTRUE(x$args$adjust)) {

        cat("Adjusted Contingency Coefficient Matrix\n\n")

      } else {

        cat("Contingency Coefficient Matrix\n\n")

      }

    }

    if (is.null(dim(x$result))) {

      write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE)

    } else {

      print(print.object, quote = FALSE, right = TRUE)

    }

  ####################################################################################
  #-----------------------------------------------------------------------------------
  # Correlation Matrix with Statistical Significance Testing
  }, cor.matrix = {

    if (isTRUE(check)) {

      #......
      # Check input 'print'
      if (any(!print %in% c("all", "cor", "n", "p"))) {

        stop("Character string(s) in the argument 'print' does not match with \"all\", \"cor\", \"n\", or \"p\".",
             call. = FALSE)

      }

    }

    ####################################################################################
    # Arguments

    #......
    # Print correlation, sample size or significance values
    if (all(c("all", "cor", "n", "p") %in% print)) { print <- "cor" }

    if (length(print) == 1L && "all" %in% print) { print <- c("cor", "n", "p") }

    #......
    # Print triangular
    tri <- ifelse(all(c("both", "lower", "upper") %in% tri), "lower", tri)

    ####################################################################################
    # Main Function

    #------------------------------------
    # No grouping
    if (is.null(x$args$group)) {

      #........................................
      # Round and format

      print.object$cor <- formatC(print.object$cor, digits = digits, format = "f")
      print.object$p <- formatC(print.object$p, digits = p.digits, format = "f")
      print.object$n <- formatC(print.object$n)

      diag(print.object$cor) <- ""
      diag(print.object$p) <- ""
      diag(print.object$n) <- ""

      #........................................
      # Lower and/or upper triangular

      if (tri == "lower") {

        print.object$cor[upper.tri(print.object$cor)] <- ""
        print.object$n[upper.tri(print.object$n)] <- ""
        print.object$p[upper.tri(print.object$p)] <- ""

      }

      if (tri == "upper") {

        print.object$cor[lower.tri(print.object$cor)] <- ""
        print.object$n[lower.tri(print.object$n)] <- ""
        print.object$p[lower.tri(print.object$p)] <- ""

      }

      #........................................
      # Row names

      if (!is.null(row.names(print.object$cor))) {

        # Rownames
        row.names(print.object$cor) <- paste0("  ", row.names(print.object$cor))
        row.names(print.object$n) <- paste0("  ", row.names(print.object$n))
        row.names(print.object$p) <- paste0("  ", row.names(print.object$p))

      }

      print.object$cor <- apply(print.object$cor, 2, function(y) format(y, justify = "right"))
      print.object$n <- apply(print.object$n, 2, function(y) format(y, justify = "right"))
      print.object$p <- apply(print.object$p, 2, function(y) format(y, justify = "right"))

    #------------------------------------
    # Grouping
    } else {

      #........................................
      # Round and format

      print.object.g1 <- print.object$group1
      print.object.g2 <- print.object$group2

      x.ncol <- ncol(print.object.g1$cor)

      #....
      # Group 1
      print.object.g1$cor <- formatC(print.object.g1$cor, digits = digits, format = "f")
      print.object.g1$p <- formatC(print.object.g1$p, digits = p.digits, format = "f")
      print.object.g1$n <- formatC(print.object.g1$n)

      diag(print.object.g1$cor) <- ""
      diag(print.object.g1$p) <- ""
      diag(print.object.g1$n) <- ""

      print.object.g1$cor <- format(print.object.g1$cor, justify = "left")
      print.object.g1$n <- format(print.object.g1$n, justify = "left")
      print.object.g1$p <- format(print.object.g1$p, justify = "left")

      print.object.g1$cor <- vapply(as.data.frame(print.object.g1$cor, stringsAsFactors = FALSE), function(y) format(y, justify = "right"), FUN.VALUE = character(x.ncol))
      print.object.g1$n <- vapply(as.data.frame(print.object.g1$n, stringsAsFactors = FALSE), function(y) format(y, justify = "right"), FUN.VALUE = character(x.ncol))
      print.object.g1$p <- vapply(as.data.frame(print.object.g1$p, stringsAsFactors = FALSE), function(y) format(y, justify = "right"), FUN.VALUE = character(x.ncol))

      #....
      ### Group 2
      print.object.g2$cor <- formatC(print.object.g2$cor, digits = digits, format = "f")
      print.object.g2$p <- formatC(print.object.g2$p, digits = p.digits, format = "f")
      print.object.g2$n <- formatC(print.object.g2$n)

      diag(print.object.g2$cor) <- ""
      diag(print.object.g2$p) <- ""
      diag(print.object.g2$n) <- ""

      print.object.g2$cor <- format(print.object.g2$cor, justify = "left")
      print.object.g2$n <- format(print.object.g2$n, justify = "left")
      print.object.g2$p <- format(print.object.g2$p, justify = "left")

      print.object.g2$cor <- vapply(as.data.frame(print.object.g2$cor, stringsAsFactors = FALSE), function(y) format(y, justify = "right"), FUN.VALUE = character(x.ncol))
      print.object.g2$n <- vapply(as.data.frame(print.object.g2$n, stringsAsFactors = FALSE), function(y) format(y, justify = "right"), FUN.VALUE = character(x.ncol))
      print.object.g2$p <- vapply(as.data.frame(print.object.g2$p, stringsAsFactors = FALSE), function(y) format(y, justify = "right"), FUN.VALUE = character(x.ncol))

      # Print object
      print.object <- print.object.g1

      #........................................
      # Lower triangular: Group 1; Upper triangular: Group 2

      print.object$cor[upper.tri(print.object$cor)] <- print.object.g2$cor[upper.tri(print.object.g2$cor)]
      print.object$n[upper.tri(print.object$cor)] <- print.object.g2$n[upper.tri(print.object.g2$n)]
      print.object$p[upper.tri(print.object$p)] <- print.object.g2$p[upper.tri(print.object.g2$p)]

      # Row names
      row.names(print.object$cor) <- paste0("  ", colnames(print.object$cor))
      row.names(print.object$n) <- paste0("  ", colnames(print.object$n))
      row.names(print.object$p) <- paste0("  ", colnames(print.object$p))

    }

    #------------------------------------
    # Print

    #........................
    # Correlation coefficient
    if ("cor" %in% print) {

      if (x$args$method == "pearson") {

        cat(" Pearson Product-Moment Correlation Coefficient\n\n")

      }

      if (x$args$method == "spearman") {

        cat(" Spearman's Rank-Order Correlation Coefficient\n\n")

      }

      if (x$args$method == "kendall-b") {

        cat(" Kendall's Tau-b Correlation Coefficient\n\n")

      }

      if (x$args$method == "kendall-c") {

        cat(" Kendall-Stuart's Tau-c Correlation Coefficient\n\n")

      }

      print(print.object$cor, quote = FALSE, right = TRUE, max = 99999)

    }

    #........................
    # Sample size
    if ("n" %in% print) {

      if ("cor" %in% print) { cat("\n") }

      if (!is.null(x$args$group)) {

        x$args$use <- ifelse(all(c("listwise", "pairwise") %in% x$args$use) || x$args$use == "pairwise", "pairwise.complete.obs", "complete.obs")

      }

      if (x$args$use == "pairwise.complete.obs") {

        cat(" Sample Size Using Pairwise Deletion\n\n")
        print(print.object$n, quote = FALSE, right = TRUE, max = 99999)

      }

      if (x$args$use == "complete.obs") {

        if (is.null(x$args$group)) {

          cat(paste(" Sample Size Using Listwise Deletion\n  n =", nrow(na.omit(x$data)), "\n"))

        } else {

          cat(paste(" Sample Size Using Listwise Deletion\n  n in group 1 =", nrow(na.omit(x$data$group1)),
                    "\n  n in group 2 =", nrow(na.omit(x$data$group2)),"\n"))

        }

      }

    }

    #........................
    # p.values
    if ("p" %in% print) {

      if (any(c("cor", "n") %in% print)) { cat("\n") }

      cat(" Significance Value (p-value)\n\n")
      print(print.object$p, quote = FALSE, right = TRUE, max = 99999L)
      cat(paste0("\n  Adjustment for multiple testing: ", x$args$p.adj, "\n"))

    }

    #........................
    # Grouping
    if (!is.null(x$args$group)) {

      cat(paste0("\n Note. Lower triangular: Results for group = ", sort(unique(x$args$group))[1],
                 "\n       Upper triangular: Results for group = ", sort(unique(x$args$group))[2]), "\n")

    }

  ####################################################################################
  #-----------------------------------------------------------------------------------
  # Cramer's V
  }, cramers.v = {

    #-----------------------------------------------------------------------------------
    # Data and Arguments

    # Print object
    print.object <- x$result

    # Print triangular
    tri <- ifelse(all(c("both", "lower", "upper") %in% tri), "lower", tri)

    #-----------------------------------------------------------------------------------
    # Main Function

    #........................................
    # Two variables
    if (is.null(dim(print.object))) {

      print.object <- cbind("  Estimate: ", formatC(print.object, digits = digits, format = "f"))

      #........................................
      # More than two variables
    } else {

      # Format contingenccy coefficients
      print.object <- formatC(print.object, digits = digits, format = "f")

      # Lower and/or upper triangular
      if (tri == "lower") {

        print.object[upper.tri(print.object)] <- ""

      }

      if (tri == "upper") {

        print.object[lower.tri(print.object)] <- ""

      }

      # Diagonal
      diag(print.object) <- ""

      # Format
      row.names(print.object) <- paste("", row.names(print.object))

    }

    #-----------------------------------------------------------------------------------
    # Output

    if (is.null(dim(x$result))) {

      if (isTRUE(x$args$correct)) {

        cat("Bias-Corrected Cramer's V\n\n")

      } else {

        cat("Cramer's V\n\n")

      }

    } else {

      if (isTRUE(x$args$correct)) {

        cat("Bias-Corrected Cramer's V Matrix\n\n")

      } else {

        cat("Cramer's V Matrix\n\n")

      }

    }

    if (is.null(dim(x$result))) {

      write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE)

    } else {

      print(print.object, quote = FALSE, right = TRUE)

    }

  ####################################################################################
  #-----------------------------------------------------------------------------------
  # Cross Tabulation
  }, crosstab = {

    if (isTRUE(check)) {

      #......
      # Check input print
      if (any(!print %in% c("no", "all", "row", "col", "total"))) {

        stop("Character string(s) in the argument 'print' does not match with \"no\", \"all\", \"row\", \"col\" or \"total\".",
             call. = FALSE)

      }

    }

    ####################################################################################
    # Main Function

    #......
    # Percentages
    no.perc <- if (any("no" %in% print)) {

      no.perc <- NULL

    } else {

      no.perc <- c("row", "col", "total")[!c("row", "col", "total") %in% print]

    }

    #......
    # Print object
    print.object <- x$result

    #----------------------------------------
    # Two-Dimensional Matrix

    if (ncol(x$data) == 2L) {

      restab <- cbind(rep(names(print.object$freq.a[, 1L]), times = 4L),
                      rep(c("Freq", "Row %", "Col %", "Tot %"), each = nrow(print.object$freq.a)),
                      rbind(print.object$freq.a, print.object$perc.r, print.object$perc.c, print.object$perc.t),
                      c(apply(print.object$freq.a, 1, sum), rep("", times = 3*nrow(print.object$freq.a))))

      #......
      # Sort table

      # First variable is a factor
      if (is.factor(x$data[, 1L])) {

        # Sort with NA
        if (any(is.na(x$data)) && !isTRUE(x$args$na.omit)) {

          restab <- restab[order(factor(restab[, 1L], levels = c(levels(x$data[, 1L]), "NA"), labels = c(levels(x$data[, 1L]), "NA"))), ]

          # Sort without NA
        } else {

          restab <- restab[order(factor(restab[, 1L], levels = levels(x$data[, 1L]), labels = levels(x$data[, 1L]))), ]

        }

        # First variable is not a factor
      } else {

        restab <- restab[order(restab[, 1L]), ]

      }

      #......
      # Frequencies and percentages

      # No absolute frequencies
      if (!isTRUE(freq)) {

        restab <- restab[-grep("Freq", restab[, 2L]), ]

      }

      # No percentages
      if (any(print == "no")) {

        restab <- restab[-grep(" %", restab[, 2L]), -2L]

      } else {

        # No total percentages
        if ("total" %in% no.perc) {

          restab <- restab[-grep("Tot %", restab[, 2]), ]

        }

        # No row percentages
        if ("row" %in% no.perc) {

          restab <- restab[-grep("Row %", restab[, 2L]), ]

        }

        # No col percentages
        if ("col" %in% no.perc) {

          restab <- restab[-grep("Col %", restab[, 2L]), ]

        }

      }

      #......
      # Format

      # Frequencies only
      if (any(print == "no")) {

        #......
        # Variable names and column sum
        restab <- rbind(c("", colnames(x$data)[2], rep("", times = (ncol(restab) - 2L))),
                        c(colnames(x$data)[1], colnames(restab)[-c(1L, ncol(restab))], "Total"),
                        restab,
                        c("Total", apply(print.object$freq.a, 2, sum), sum(print.object$freq.a)))

        # Format
        restab[2L, 1L] <- paste0(" ", restab[2L, 1L])
        restab[-c(1L, 2L), 1L] <- paste0("  ", restab[-c(1L, 2L), 1L])

        # Justify right
        restab[-2L, 1L] <- format(restab[-2L, 1L], justify = "right")
        restab[, 1L] <- format(restab[, 1L])

        restab[-1L, -1L] <- apply(restab[-1L, -1L], 2, function(y) format(y, justify = "right"))

        restab[-1L, 2L] <- paste0(" ", restab[-1L, 2L])

      # Percentage(s)
      } else {

        #......
        # Variable names and column sum
        restab <- rbind(c("", "", colnames(x$data)[2L], rep("", times = (ncol(restab) - 3L))),
                        c(colnames(x$data)[1], colnames(restab)[-c(1L, ncol(restab))], "Total"),
                        restab,
                        c("Total", "", apply(print.object$freq.a, 2, sum), sum(print.object$freq.a)))

        # Format percentages
        restab[grep("%", restab[, 2L]), -c(1L:2L, ncol(restab))] <- apply(restab[grep("%", restab[, 2L]), -c(1L:2L, ncol(restab))], 2, function(y) paste0(format(formatC(as.numeric(y), digits = digits, format = "f"), justify = "right"), "%"))

        restab <- gsub("NaN", "NA", restab)

        # Justify right and left
        restab[, 1L] <- format(restab[, 1L], justify = "right")

        restab[2L, 1L] <- paste0(" ", sub("^\\s+", "", restab[2L, 1L]), paste0(rep(" ", times = max(nchar(restab[, 1L])) - nchar(colnames(x$data)[1L])), collapse = ""), " ")
        restab[-2L, 1L] <- paste0("  ", restab[-2L, 1L])

        restab[, 2L] <- format(restab[, 2L], justify = "left")

        restab[-1L, -c(1L:2L)] <- apply(restab[-1L, -c(1L:2L)], 2, format, justify = "right")

        restab[-1L, 3L] <- paste0(" ", restab[-1L, 3L])

      }

      #......
      # Output table not split
      if (!isTRUE(split) || all(print == "no")) {

        # Remove Total row and column
        if (!isTRUE(freq)) {

          restab <- restab[-nrow(restab), -ncol(restab)]

        }

        # Remove duplicated row labels
        restab[, 1L] <- ifelse(duplicated(restab[, 1L]), paste(rep(" ", times = unique(nchar(restab[, 1L]))), collapse = ""), restab[, 1L])

        # Print results
        write.table(restab, col.names = FALSE, row.names = FALSE, quote = FALSE)

      # Output table split
      } else {

        if (isTRUE(freq)) {

          # Frequencies
          restab.abs <- restab[-grep("%", restab[, 2L]), -2L]

          restab.abs[-1L, -1L] <- apply(restab.abs[-1L, -1L], 2, function(y) formatC(sub("^\\s+", "", y), format = "f"))

          restab.abs[-1L, 2L] <- paste0(" ", restab.abs[-1L, 2L])

          restab.abs[, 1L] <- paste0(" ", restab.abs[, 1L])

          cat(" Frequencies\n")
          write.table(restab.abs, col.names = FALSE, row.names = FALSE, quote = FALSE)

        }

        # Row-wise percentages
        if ("row" %in% print) {

          if (isTRUE(freq)) { cat("\n") }

          restab.row <- cbind(rbind(restab[1L:2L, -c(2L, ncol(restab))], restab[grep("Row", restab[, 2L]), -c(2L, ncol(restab))]),
                              c("", "Total",
                                rep(ifelse(digits == 0L, "100", paste0("100.", paste(rep("0", times = digits), collapse = ""), "%")), times = nrow(restab[grep("Row", restab[, 2L]), ]))))

          restab.row[which(apply(restab.row, 1, function(y) length(grep("NA%", y)) != 0L)), ncol(restab.row)] <- "NA%"

          restab.row[, ncol(restab.row)] <- format(restab.row[, ncol(restab.row)], justify = "right")

          restab.row[-1L, -1L] <- apply(restab.row[-1L, -1L], 2, function(y) formatC(sub("^\\s+", "", y), format = "f"))

          restab.row[-1L, 2L] <- paste0(" ", restab.row[-1L, 2L])

          restab.row[, 1L] <- paste0(" ", restab.row[, 1L])

          cat(" Row-Wise Percentages\n")
          write.table(restab.row, col.names = FALSE, row.names = FALSE, quote = FALSE)

        }

        # Column-wise percentages
        if ("col" %in% print) {

          if (isTRUE(freq) || "row" %in% print) { cat("\n") }

          restab.col <- rbind(restab[1L:2L, -c(2L, ncol(restab))], restab[grep("Col", restab[, 2]), -c(2L, ncol(restab))],
                              c("Total", rep(ifelse(digits == 0L, "100", paste0("100.", paste(rep("0", times = digits), collapse = ""), "%")), times = ncol(restab) - 3L)))

          restab.col[nrow(restab.col), which(apply(restab.col, 2, function(y) length(grep("NA%", y)) != 0L))] <- "NA%"

          restab.col[-1L, ] <- apply(restab.col[-1L, ], 2, function(y) formatC(sub("^\\s+", "", y), format = "f"))

          restab.col[, 1L] <- format(restab.col[, 1L])

          restab.col[-1L, 2L] <- paste0(" ", restab.col[-1L, 2L])

          restab.col[, 1L] <- paste0("  ", restab.col[, 1L])

          cat(" Column-Wise Percentages\n")
          write.table(restab.col, col.names = FALSE, row.names = FALSE, quote = FALSE)

        }

        # Total percentages
        if ("total" %in% print) {

          if (isTRUE(freq) || "row" %in% print || "col" %in% print) { cat("\n") }

          restab.total <- rbind(restab[1L:2L, -c(2L, ncol(restab))], restab[grep("Tot", restab[, 2L]), -c(2L, ncol(restab))])

          restab.total[-1L, -1L] <- apply(restab.total[-1L, -1L], 2, function(y) formatC(sub("^\\s+", "", y), format = "f"))

          restab.total <- rbind(cbind(restab.total, c("", "Total", rep("", times = nrow(restab.total) - 2L))), c(rep("", times = ncol(restab.total)),
                                                                                                                 ifelse(digits == 0, "100", paste0("100.", paste(rep("0", times = digits), collapse = ""), "%"))))

          restab.total[, ncol(restab.total)] <- format(restab.total[, ncol(restab.total)], justify = "right")

          restab.total[-1L, ] <- apply(restab.total[-1L, ], 2, format)

          restab.total[-1L, 2L] <- paste0(" ", restab.total[-1L, 2L])

          restab.total[, 1L] <- paste0(" ", restab.total[, 1L])

          cat(" Total Percentages\n")
          write.table(restab.total, col.names = FALSE, row.names = FALSE, quote = FALSE)

        }

      }

    }

    #----------------------------------------
    # Three-Dimensional Matrix

    if (ncol(x$data) == 3L) {

      # Absolute frequencies
      freq.a.print <- NULL
      for (i in seq_len(nrow(print.object$freq.a[[1]]))) {

        freq.a.print <- cbind(freq.a.print,
                              unlist(lapply(print.object$freq.a, function(y) y[i, ])))

      }

      # Row %
      perc.r.print <- NULL
      for (i in seq_len(nrow(print.object$perc.c[[1L]]))) {

        perc.r.print <- cbind(perc.r.print,
                              unlist(lapply(print.object$perc.c, function(y) y[i, ])))

      }

      # Column %
      perc.c.print <- NULL
      for (i in seq_len(nrow(print.object$perc.r[[1]]))) {

        perc.c.print <- cbind(perc.c.print,
                              unlist(lapply(print.object$perc.r, function(y) y[i, ])))

      }

      # Total %
      perc.t.print <- NULL
      for (i in seq_len(nrow(print.object$perc.t[[1]]))) {

        perc.t.print <- cbind(perc.t.print,
                              unlist(lapply(print.object$perc.t, function(y) y[i, ])))

      }

      # Result table
      restab <- cbind(rep(names(print.object$freq.a), each = ncol(print.object$freq.a[[1]]), time = 4L),
                      rep(colnames(print.object$freq.a[[1L]]), times = 4L*length(print.object$freq.a)),
                      rep(c("Freq", "Row %", "Col %", "Tot %"), each =  ncol(print.object$freq.a[[1L]])*length(print.object$freq.a)),
                      rbind(freq.a.print, perc.r.print, perc.c.print, perc.t.print),
                      c(apply(freq.a.print, 1, sum), rep("", times = 3L*length(print.object$freq.a)*ncol(print.object$freq.a[[1]]))))

      # Convert NaN in NA
      restab <- gsub("NaN", NA, restab)

      #......
      # Sort table

      # First and scond variable are a factor
      if (is.factor(x$data[, 1L]) && is.factor(x$data[, 2L])) {

        # Sort with NA
        if (any(is.na(x$data)) && !isTRUE(x$args$na.omit)) {

          restab <-restab[order(factor(restab[, 1L], levels = c(levels(x$data[, 1L]), "NA"), labels = c(levels(x$data[, 1L]), "NA")),
                                factor(restab[, 2L], levels = c(levels(x$data[, 2L]), "NA"), labels = c(levels(x$data[, 2L]), "NA"))), ]

          # Sort without NA
        } else {

          restab <- restab[order(factor(restab[, 1L], levels = levels(x$data[, 1L]), labels = levels(x$data[, 1L])),
                                 factor(restab[, 2L], levels = levels(x$data[, 2L]), labels = levels(x$data[, 2L]))), ]

        }

      }

      # First variable is a factor, second variable is not a factor
      if (is.factor(x$data[, 1L]) && !is.factor(x$data[, 2L])) {

        # Sort with NA
        if (any(is.na(x$data)) && !isTRUE(x$args$na.omit)) {

          restab <- restab[order(factor(restab[, 1L], levels = c(levels(x$data[, 1L]), "NA"), labels = c(levels(x$data[, 1L]), "NA")),
                                 restab[, 2L]), ]

        # Sort without NA
        } else {

          restab <- restab[order(factor(restab[, 1L], levels = levels(x$data[, 1L]), labels = levels(x$data[, 1L])),
                                 restab[, 2L]), ]

        }

      }

      # First variable is not a factor, second variable is a factor
      if (!is.factor(x$data[, 1L]) && is.factor(x$data[, 2L])) {

        # Sort with NA
        if (any(is.na(x$data)) && !isTRUE(x$args$na.omit)) {

          restab <- restab[order(restab[, 1L],
                                 factor(restab[, 2L], levels = c(levels(x$data[, 2L]), "NA"), labels = c(levels(x$data[, 2L]), "NA"))), ]

        # Sort without NA
        } else {

          restab <- restab[order(restab[, 1L],
                                 factor(restab[, 2L], levels = levels(x$data[, 2L]), labels = levels(x$data[, 2L]))), ]

        }

      }

      # First and second variable are not a factor
      if (!is.factor(x$data[, 1L]) && !is.factor(x$data[, 2L])) {

        # Sort with NA
        if (any(is.na(x$data)) && !isTRUE(x$args$na.omit)) {

          restab <- restab[order(restab[, 1L],
                                 factor(restab[, 2L], levels = c(levels(x$data[, 2L]), "NA"), labels = c(levels(x$data[, 2L]), "NA"))), ]

        # Sort without NA
        } else {

          restab <- restab[order(restab[, 1L], restab[, 2L]), ]

        }

      }

      #......
      # Frequencies and percentages

      # No absolute frequencies
      if (!isTRUE(freq)) {

        restab <- restab[-grep("Freq", restab[, 3L]), ]

      }

      # No percentages
      if (isTRUE(print == "no")) {

        restab <- restab[-grep(" %", restab[, 3L]), -3L]

      } else {

        # No total percentages
        if ("total" %in% no.perc) {

          restab <- restab[-grep("Tot %", restab[, 3L]), ]

        }

        # No row percentages
        if ("row" %in% no.perc) {

          restab <- restab[-grep("Row %", restab[, 3L]), ]

        }

        # No col percentages
        if ("col" %in% no.perc) {

          restab <- restab[-grep("Col %", restab[, 3L]), ]

        }

      }

      #......
      # Format

      if (any(print == "no")) {

        #......
        # Variable names
        restab <- rbind(c(rep("", times = 2L), colnames(x$data)[3L], rep("", times = (ncol(restab) - 3L))),
                        c(colnames(x$dat)[1L], colnames(x$data)[2L], row.names(print.object$freq.a[[1L]]), "Total"),
                        restab,
                        c("Total", "", apply(freq.a.print, 2, sum), sum(freq.a.print)))

        # Justify right
        restab[-1L, ] <- apply(restab[-1L, ], 2, function(y) format(y, justify = "right"))

        # First variable
        if (nchar(colnames(x$data)[1]) < max(nchar(restab[, 1L]))) {

          restab[2L, 1L] <- paste(sub("^\\s+", "", restab[2L, 1L]), paste(rep(" ", times = max(nchar(restab[, 1L])) - nchar(colnames(x$data)[1L]) - 1L) , collapse = ""))

        }

        if (nchar(colnames(x$data)[1L]) == max(nchar(restab[, 1L]))) {

          restab[2L, 1L] <- paste(sub("^\\s+", "", restab[2L, 1L]), paste(rep(" ", times = max(nchar(restab[, 1L])) - nchar(colnames(x$data)[1L])) , collapse = ""))

        }

        # Second variable
        if (nchar(colnames(x$data)[2L]) < max(nchar(restab[, 2L]))) {

          restab[2L, 2L] <- paste(sub("^\\s+", "", restab[2L, 2L]), paste(rep(" ", times = max(nchar(restab[, 2L])) - nchar(colnames(x$data)[2L]) - 1L), collapse = ""))

        }

        if (nchar(colnames(x$data)[2L]) == max(nchar(restab[, 2L]))) {

          restab[2L, 2L] <- paste(sub("^\\s+", "", restab[2L, 2L]), paste(rep(" ", times = max(nchar(restab[, 2L])) - nchar(colnames(x$data)[2L])), collapse = ""))

        }

        # Format
        restab[, 1L:2L] <- apply(restab[, 1L:2L], 2, format)

        restab[-2L, 1L] <- paste0(" ", restab[-2L, 1L])
        restab[-2L, 2L] <- paste0(" ", restab[-2L, 2L])
        restab[-1L, 3L] <- paste0(" ", restab[-1L, 3L])

        restab[, 1L:2L] <- apply(restab[, 1L:2L], 2, format)

        restab[, 1L] <- paste0(" ", restab[, 1L])

      # Percentage(s)
      } else {

        #......
        # Variable names
        restab <- rbind(c(rep("", times = 3L), colnames(x$data)[3L], rep("", times = (ncol(restab) - 4L))),
                        c(colnames(x$dat)[1L], colnames(x$data)[2L], "", row.names(print.object$freq.a[[1L]]), "Total"),
                        restab,
                        c("Total", "", "", apply(freq.a.print, 2, sum), sum(freq.a.print)))

        # Format percentages
        restab[grep("%", restab[, 3L]), -c(1:3, ncol(restab))] <- apply(restab[grep("%", restab[, 3L]), -c(1L:3L, ncol(restab))], 2, function(y) paste0(format(formatC(as.numeric(y), digits = digits, format = "f"), justify = "right"), "%"))

        # Format variable names
        restab[2L, 1L] <- format(restab[2L, 1L], justify = "left", width = max(nchar(restab[, 1L])))
        restab[2L, 2L] <- format(restab[2L, 2L], justify = "left", width = max(nchar(restab[, 2L])))

        # Format values
        restab[-2L, 1L] <- format(restab[-2L, 1L], justify = "right", width = max(nchar(restab[, 1L])))
        restab[-2L, 2L] <- format(restab[-2L, 2L], justify = "right", width = max(nchar(restab[, 2L])))

        # Justify right
        restab[-1L, -c(1L:3L)] <- apply(restab[-1L, -c(1L:3L)], 2, function(y) format(y, justify = "right"))

        restab[, 3L] <- format(restab[, 3L], justify = "left")

        # First variable
        restab[-2L, 1L] <- paste0("  ", restab[-2L, 1L])
        restab[2L, 1L] <- paste0(" ", restab[2L, 1L], " ")

        # Second variable
        restab[-2L, 2L] <- paste0(" ", restab[-2L, 2L])
        restab[2L, 2L] <- paste0(restab[2L, 2L], " ")

        # Third variable
        restab[-1L, 4L] <- paste0(" ", restab[-1L, 4L])

      }

      #......
      # Output table not split
      if (!isTRUE(split) || all(print == "no")) {

        # Remove Total row and column
        if (!isTRUE(freq)) {

          restab <- restab[-nrow(restab), -ncol(restab)]

        }

        # Remove duplicated row labels
        restab[-c(1L:2L, nrow(restab)), 2L] <- unlist(tapply(restab[-c(1L:2L, nrow(restab)), 2L], restab[-c(1L:2L, nrow(restab)), 1], function(y) ifelse(duplicated(y), paste0(rep(" ", times = unique(nchar(restab[, 2]))), collapse = ""), restab[-c(1L:2L, nrow(restab)), 2L])))
        restab[, 1L] <- ifelse(duplicated(restab[, 1L]), paste(rep(" ", times = unique(nchar(restab[, 1L]))), collapse = ""), restab[, 1])

        # Print results
        write.table(restab, col.names = FALSE, row.names = FALSE, quote = FALSE)

      # Output table split
      } else {

        if (isTRUE(freq)) {

          # Frequencies
          restab.abs <- restab[-grep("%", restab[, 3L]), -3L]

          restab.abs[-1L, -1L] <- apply(restab.abs[-1L, -1L], 2, function(y) formatC(sub("^\\s+", "", y), format = "f"))

          restab.abs <- rbind(c(paste(rep(" ", times = max(nchar(restab.abs[, 1L]))), collapse = ""),
                                paste(rep(" ", times = max(nchar(restab.abs[, 2L]))), collapse = ""),
                                colnames(x$data)[3L], rep("", times = ncol(restab.abs) - 3L)),
                              restab.abs[-1L, ])

          restab.abs[-1L, 3L] <- paste0(" ",restab.abs[-1L, 3L])
          restab.abs[1L, 3L] <- paste0(restab.abs[1L, 3L], " ")

          restab.abs[, 1L] <- paste0(" ", restab.abs[, 1L])

          cat(" Frequencies\n")
          write.table(restab.abs, col.names = FALSE, row.names = FALSE, quote = FALSE)

        }

        # Row-wise percentages
        if ("row" %in% print) {

          if (isTRUE(freq)) { cat("\n") }

          restab.row <- cbind(rbind(restab[1L:2L, -c(3L, ncol(restab))], restab[grep("Row", restab[, 3L]), -c(3L, ncol(restab))]),
                              c("", "Total",
                                rep(ifelse(digits == 0L, "100", paste0("100.", paste(rep("0", times = digits), collapse = ""), "%")), times = nrow(restab[grep("Row", restab[, 3L]), ]))))

          restab.row[which(apply(restab.row, 1, function(y) length(grep("NA%", y)) != 0L)), ncol(restab.row)] <- "NA%"

          restab.row[, ncol(restab.row)] <- format(restab.row[, ncol(restab.row)], justify = "right")

          restab.row[-1L, -1L] <- apply(restab.row[-1L, -1L], 2, function(y) formatC(sub("^\\s+", "", y), format = "f"))

          restab.row <- rbind(c(paste(rep(" ", times = max(nchar(restab.row[, 1L]))), collapse = ""),
                                paste(rep(" ", times = max(nchar(restab.row[, 2L]))), collapse = ""), colnames(x$data)[3L], rep("", times = ncol(restab.row) - 3L)),
                              restab.row[-1L, ])

          restab.row[1L, 3L] <- format(restab.row[1L, 3L], justify = "left", width = max(nchar(restab.row[, 3L])))

          restab.row[-1L, 3L] <- paste0(" ",restab.row[-1L, 3L])
          restab.row[1L, 3L] <- paste0(restab.row[1L, 3L], " ")

          restab.row[, 1L] <- paste0(" ", restab.row[, 1L])

          cat("Row-Wise Percentages\n")
          write.table(restab.row, col.names = FALSE, row.names = FALSE, quote = FALSE)

        }

        # Column-wise percentages
        if ("col" %in% print) {

          if (isTRUE(freq) || "row" %in% print) { cat("\n") }

          restab.col <- restab[grep("Col", restab[, 3]), -c(3, ncol(restab))]

          p <- c(paste(paste(rep(" ", times = max(nchar(restab.col[, 1L])) - 6L), collapse = ""), "Total", collapse = ""), "",
                 rep(ifelse(digits == 0, "100", paste0("100.", paste(rep("0", times = digits), collapse = ""), "%")), times = ncol(restab.col) - 2L))

          restab.col.p <- NULL
          for (i in unique(restab.col[, 1L])) {

            temp <- rbind(restab.col[restab.col[, 1L] == i, ], p)
            temp[nrow(temp), which(apply(temp, 2, function(y) length(grep("NA%", y)) != 0L))] <- "NA%"

            restab.col.p  <- rbind(restab.col.p, temp)

          }

          restab.col <- rbind(restab[1L:2L, -c(3L, ncol(restab))], restab.col.p)

          restab.col[-1L, -1L] <- apply(restab.col[-1L, -1L], 2, function(y) formatC(sub("^\\s+", "", y), format = "f"))

          restab.col <- rbind(c(paste0(rep(" ", times = max(nchar(restab.col[, 1L]))), collapse = ""),
                                paste0(rep(" ", times = max(nchar(restab.col[, 2L]))), collapse = ""), colnames(x$data)[3L], rep("", times = ncol(restab.col) - 3L)),
                              restab.col[-1L, ])

          restab.col[-1L, 3L] <- paste0(" ", restab.col[-1L, 3L])
          restab.col[1L, 3L] <- paste0(restab.col[1L, 3L], " ")

          restab.col[, 1L] <- paste0(" ", restab.col[, 1L])

          cat(" Column-Wise Percentages\n")
          write.table(restab.col, col.names = FALSE, row.names = FALSE, quote = FALSE)

        }

        # Total percentages
        if ("total" %in% print) {

          if (isTRUE(freq) || "row" %in% print || "col" %in% print) { cat("\n") }

          restab.total <- rbind(restab[1L:2L, -c(3L, ncol(restab))], restab[grep("Tot", restab[, 3L]), -c(3L, ncol(restab))])
          restab.total <- rbind(cbind(restab.total, c("", "Total", rep("", times = nrow(restab.total) - 2L))), c(rep("", times = ncol(restab.total)),
                                                                                                                 ifelse(digits == 0L, "100", paste0("100.", paste(rep("0", times = digits), collapse = ""), "%"))))
          restab.total[, ncol(restab.total)] <- format(restab.total[, ncol(restab.total)], justify = "right")
          restab.total <- apply(restab.total, 2, format)

          restab.total[-1L, -1L] <- apply(restab.total[-1L, -1L], 2, function(y) formatC(sub("^\\s+", "", y), format = "f"))

          restab.total <- rbind(c(paste(rep(" ", times = max(nchar(restab.total[, 1L]))), collapse = ""),
                                  paste(rep(" ", times = max(nchar(restab.total[, 2L]))), collapse = ""), colnames(x$data)[3L], rep("", times = ncol(restab.total) - 3L)),
                                restab.total[-1L, ])

          restab.total[-1L, 3L] <- paste0(" ", restab.total[-1L, 3L])
          restab.total[1L, 3L] <- paste0(restab.total[1L, 3L], " ")

          restab.total[, 1L] <- paste0(" ", restab.total[, 1L])

          cat(" Total Percentages\n")
          write.table(restab.total, col.names = FALSE, row.names = FALSE, quote = FALSE)

        }

      }

    }

  ####################################################################################
  #-----------------------------------------------------------------------------------
  # Descriptive Statistics
  }, descript = {

    if (isTRUE(check)) {

      #......
      # Check input 'print'
      if (!all(print %in%  c("all", "n", "nNA", "pNA", "m", "var", "sd", "min", "p25", "med", "p75", "max", "skew",  "range", "iqr", "kurt"))) {

        stop("Character strings in the argument 'print' do not all match with \"all\", \"n\", \"nNA\", \"pNA\", \"m\", \"var\", \"sd\", \"min\", \"p25\", \"med\", \"p75\", \"max\", \"range\", \"iqr\", \"skew\", or \"kurt\".",
             call. = FALSE)

      }

    }

    ####################################################################################
    # Arguments

    if (length(print) == 1L && print == "all") {

      print <- c("n", "nNA", "pNA", "m", "var", "sd", "min", "p25", "med", "p75", "max", "range", "iqr", "skew", "kurt")

    }

    ####################################################################################
    # Main Function

    #......
    # Variables to round
    print.round <- c("pNA", "m", "var", "sd", "min", "p25", "med", "p75", "max", "range", "iqr", "skew", "kurt")

    #......
    # Print object
    print.object <- x$result

    #----------------------------------------
    # No grouping

    if (is.null(x$data$group) && is.null(x$data$split)) {

      #......
      # Round
      print.object[, print.round] <- sapply(print.round, function(y) ifelse(!is.na(print.object[, y]), formatC(print.object[, y], digits = digits, format = "f"), NA))

      #......
      # Percentages
      print.object[, "pNA"] <- paste0(print.object[, "pNA"], "%")

      #......
      # Row names
      print.object <- rbind(c("Variable", "n", "nNA", "pNA", "M", "Var", "SD", "Min", "p25", "Med", "p75", "Max", "Range", "IQR", "Skew", "Kurt"), print.object)

      #......
      # Select statistical measures and add variable names
      print.object <- data.frame(var = print.object[, "variable"], print.object[, print, drop = FALSE], stringsAsFactors = FALSE,
                                 check.names = FALSE)

      #......
      # Format
      print.object[, 1L] <- format(print.object[, 1L, drop = FALSE], justify = "left")

      print.object[, -1L] <- apply(print.object[, -1L, drop = FALSE], 2, function(y) format(y, justify = "right"))

      if (ncol(x$data$x) == 1L) {

        print.object <- print.object[, -1L]

      }

      print.object[, 1L] <- paste0(" ", print.object[, 1L])

      write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE)

    }

    #----------------------------------------
    # Grouping

    if (!is.null(x$data$group) && is.null(x$data$split)) {

      # Sort by variables
      if (isTRUE(sort.var)) {

        print.object <- print.object[order(print.object[, "variable"]), ]

      }

      # Round
      print.object[, print.round] <- sapply(print.round, function(y) ifelse(!is.na(print.object[, y]),
                                                                            formatC(print.object[, y], digits = digits, format = "f"), NA))

      # Percentages
      print.object[, "pNA"] <- paste0(print.object[, "pNA"], "%")

      # Col names
      print.object <- rbind(c("Group", "Variable", "n", "nNA", "pNA", "M", "Var", "SD", "Min", "p25", "Med", "p75", "Max", "Range", "IQR", "Skew", "Kurt"),
                            print.object)

      # Select statistical measures and add variable names
      print.object <- data.frame(print.object[, c("group", "variable")], print.object[, -c(1, 2)][, print, drop = FALSE], stringsAsFactors = FALSE)

      # Format
      print.object[, 1L] <- format(print.object[, 1L], justify = "left")
      print.object[, 2L] <- format(print.object[, 2L], justify = "left")

      print.object[, -c(1L:2L)] <- apply(print.object[, -c(1L:2L)], 2, format, justify = "right")

      if (ncol(x$data$x) == 1L) {

        print.object <- print.object[, -2L]

      }

      print.object[1L, 1L] <- paste0(" ", print.object[1L, 1L], " ", collapse = "")
      print.object[-1L, 1L] <- paste0("  ", print.object[-1L, 1L])

      print.object[, -c(1L:2L)] <- apply(print.object[, -c(1L:2L)], 2, format, justify = "right")

      # Print Output
      write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE)

    #----------------------------------------
    # Split
    } else if (!is.null(x$data$split)) {

      # Format
      for (i in names(print.object)) {

        # Round
        print.object[[i]][, print.round] <- sapply(print.round, function(y) ifelse(!is.na(print.object[[i]][, y]), formatC(print.object[[i]][, y], digits = digits, format = "f"), NA))

        # Percentages
        print.object[[i]][, "pNA"] <- paste0(print.object[[i]][, "pNA"], "%")

        #......
        # No grouping
        if (is.null(x$data$group)) {

          # Col names
          print.object[[i]] <- rbind(c("Variable", "n", "nNA", "pNA", "M", "Var", "SD", "Min", "p25", "Med", "p75", "Max", "Range", "IQR", "Skew", "Kurt"), print.object[[i]])

          # Select statistical measures and add variable names
          print.object[[i]] <- data.frame(variable = print.object[[i]][, "variable"], print.object[[i]][, print, drop = FALSE], stringsAsFactors = FALSE)

          if (ncol(x$data$x) == 1L) {

            print.object[[i]] <- print.object[[i]][, -1L]

          }

        #......
        # Grouping
        } else {

          # Sort by variables
          if (isTRUE(sort.var)) {

            print.object[[i]] <- print.object[[i]][order(print.object[[i]][, "variable"]), ]

          }

          # Col names
          print.object[[i]] <- rbind(c("Group", "Variable", "n", "nNA", "pNA", "M", "Var", "SD", "Min", "p25", "Med", "p75", "Max", "Range", "IQR", "Skew", "Kurt"), print.object[[i]])

          # Select statistical measures and add variable names
          print.object[[i]] <- data.frame(variable = print.object[[i]][, c("group", "variable")], print.object[[i]][, print, drop = FALSE], stringsAsFactors = FALSE)

          if (ncol(x$data$x) == 1L) {

            print.object[[i]] <- print.object[[i]][, -2L]

          }

        }

        # Format
        if (ncol(x$data$x) == 1L && is.null(x$data$group)) {

          print.object[[i]][-1L, 1L] <- paste0("  ", print.object[[i]][-1L, 1L])
          print.object[[i]] <- format(print.object[[i]], justify = "right")

        } else {

          print.object[[i]][, 1L] <- format(print.object[[i]][, 1L], justify = "left")
          print.object[[i]][, 2L] <- format(print.object[[i]][, 2L], justify = "right")

          print.object[[i]][1L, 1L] <- paste0(" ", print.object[[i]][1L, 1L], " ", collapse = "")
          print.object[[i]][-1L, 1L] <- paste0("  ", print.object[[i]][-1L, 1L])

          print.object[[i]][, -c(1L:2L)] <- apply(print.object[[i]][, -c(1L:2L), drop = FALSE], 2, function(y) format(y, justify = "right"))

          print.object[[i]][, 1L] <- paste0("  ", print.object[[i]][, 1L])

        }

      }

      # Print object
      for (i in names(print.object)) {

        cat(" Split Group:", i, "\n")

        write.table(print.object[[i]], quote = FALSE, row.names = FALSE, col.names = FALSE)

        if (i != names(print.object)[length(print.object)]) { cat("\n") }

      }

    }


  ####################################################################################
  #-----------------------------------------------------------------------------------
  # Eta Squared
  }, eta.sq = {

    ####################################################################################
    # Data and Arguments

    #-----------------------------------------
    # Number of dependent variables, number of independent variables

    print.object.nrow <- ncol(x$dat$x) == 1L
    print.object.ncol <- is.null(dim(x$dat$group))

    ####################################################################################
    # Main Function

    #-----------------------------------------
    # One dependent variable, one independent variable

    if (print.object.nrow && print.object.ncol) {

      # Print object
      print.object <- cbind("  Estimate  ", formatC(print.object, digits = digits, format = "f"))

    } else {

      #-----------------------------------------
      # More than one dependent variable, more than one independent variable

      if (!print.object.nrow && !print.object.ncol) {

        # Variable names and format digis
        print.object <- rbind(c("", "Outcome", rep("", times = ncol(print.object) - 1L)),
                              c("Group", colnames(print.object)),
                              cbind(rownames(print.object),
                                    formatC(print.object, digits = digits, format = "f")))

        # Format
        print.object[-c(1L, 2L), 1L] <- paste("", print.object[-c(1L, 2L), 1L])
        print.object[, 1L] <- format(print.object[, 1], justify = "left")

        print.object[-1L, 2L] <- paste("", print.object[-1L, 2L])

        print.object[1L, 2L] <- format(print.object[1L, 2L], justify = "left", width = max(nchar(print.object[, 2L])) )
        print.object[-1L, 2L] <- format(print.object[-1L, 2L], justify = "right")

        print.object[-1L, -1L] <- apply(print.object[-1L, -1L], 2, function(y) format(y, justify = "right"))

      }

      #-----------------------------------------
      # More than one dependent variable, one independent variable

      if (print.object.nrow && !print.object.ncol) {

        # Variable names and format digis
        print.object <- rbind(colnames(print.object),
                              formatC(print.object, digits = digits, format = "f"))

        # Format
        print.object <- format(print.object, justify = "right")

      }

      #-----------------------------------------
      # One dependent variable, more than one independent variable

      if (!print.object.nrow && print.object.ncol) {

        # Variable names and format digis
        print.object <- cbind(rownames(print.object),
                              formatC(print.object, digits = digits, format = "f"))

        # Format
        print.object <- format(print.object, justify = "right")

      }

    }

    ####################################################################################
    # Output

    if (print.object.nrow && print.object.ncol) {

      cat("Eta Squared\n\n")

    } else {

      cat("Eta Squared Matrix\n\n")

    }

    write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE)

  ####################################################################################
  #-----------------------------------------------------------------------------------
  # Frequency Table
  }, freq = {

    if (isTRUE(check)) {

      #..................
      # Check input 'print'
      if (any(!print %in% c("no", "all", "perc", "v.perc"))) {

        stop("Character string in the argument 'print' does not match with \"no\", \"all\", \"perc\", or \"v.perc\".",
             call. = FALSE)

      }

    }

    ####################################################################################
    # Main function

    #-----------------------------------------
    # One variable

    if (ncol(as.data.frame(x$data, stringsAsFactors = FALSE)) == 1L || (isTRUE(x$args$split) && ncol(x$data) == 1L)) {

      #..................
      # Values in rows
      if (!isTRUE(x$args$val.col)) {

        print.object <- data.frame(x = c("Value", rep("", nrow(print.object) - 1), "Missing", "Total"),
                                   val = c(print.object[1L:(grep("NA", print.object$Value) - 1L), 1L], "Total", "NA", ""),
                                   rbind(print.object[1L:(grep("NA", print.object$Value) - 1L), -1L],
                                         apply(print.object[1L:(grep("NA", print.object$Value) - 1L), -1L], 2, function(y) sum(as.numeric(y), na.rm = TRUE)),
                                         print.object[grep("NA", print.object$Value), -1L],
                                         c(sum(as.numeric(print.object$Freq), na.rm = TRUE), "100", "")),
                                   stringsAsFactors = FALSE, row.names = NULL, check.names = FALSE)

        # Round digits
        print.object[, c("Perc", "V.Perc")] <- suppressWarnings(apply(print.object[, c("Perc", "V.Perc")], 2, function(y) paste0(formatC(as.numeric(y), digits = digits, format = "f"), "%")))

        # Remove NA
        print.object[, "V.Perc"] <- gsub("NA%", "  ", print.object[, "V.Perc"])

        # Format
        print.object[, 1L:2L] <- apply(print.object[, 1L:2L], 2, function(y) format(y, justify = "left"))

        print.object[, -c(1L:2L)] <- apply(print.object[, -(1L:2L)], 2, function(y) format(y, justify = "right"))

        #......
        # Omit Total row if there are no missing values
        if (all(!is.na(x$data))) {

          # Object without Total column
          print.object <- print.object[-grep("Total", print.object$x),  ]

          # Object without valid percentage
          print.object <- print.object[, -grep("V.Perc", colnames(print.object))]

        }

        #......
        # Omit Missing and Total row if print = "v.valid" and freq = FALSE
        if (length(print) == 1L && print == "v.perc" && !isTRUE(freq)) {

          # Object without Total row
          print.object <- print.object[-grep("Total", print.object$x),  ]

          # Object without Missing row
          print.object <- print.object[-grep("Missing", print.object$x),  ]

        }

        #......
        # Omit absolute frequencies if freq = FALSE
        if (!isTRUE(freq)) {

          print.object <- print.object[, -grep("Freq", colnames(print.object))]

        }

        #......
        # Omit percentages

        ###
        # Omit percentages if !"perc" %in% print
        if (!"perc" %in% print || "no" %in% print) {

          # Object without percentage
          print.object <- print.object[, -which(colnames(print.object) == "Perc")]

        }

        ###
        # Omit valid percentages if !"v.perc" %in% print
        if ("V.Perc" %in% colnames(print.object)) {

          if (!"v.perc" %in% print || "no" %in% print) {

            # Object without valid percentage
            print.object <- print.object[, -which(colnames(print.object) == "V.Perc")]

          }

        }

        # Column names
        colnames(print.object)[1L:2L] <- c("", "")

        #..................
        # Values in columns
      } else {

        print.object <- data.frame(print.object[, -ncol(print.object)],
                                   val = apply(print.object[, -ncol(print.object)], 1, sum, na.rm = TRUE),
                                   nNA = print.object[, ncol(print.object)],
                                   total = c(sum(print.object[1, ], na.rm = TRUE), "100", ""),
                                   stringsAsFactors = FALSE, check.names = FALSE)

        print.object[1L, ] <- as.character(print.object[1L, ])
        print.object[2L, ] <- paste0(formatC(as.numeric(print.object[2L, ]), digits = digits, format = "f"), "%")
        print.object[3L, ] <- paste0(formatC(as.numeric(print.object[3L, ]), digits = digits, format = "f"), "%")

        print.object[3L, ] <- gsub("NA%", "", print.object[3L,  ])

        # Row names
        print.object <- cbind(x = c("Freq", "Perc", "V.Perc"), print.object)

        # Column names
        colnames(print.object) <- c("Value", colnames(x$result)[-length(x$result)], "Total", "Missing", "Total")

        # Format
        print.object[, 1L] <- format(print.object[, 1L], justify = "left")
        colnames(print.object)[1] <- format(c(colnames(print.object)[1L], print.object[, 1L]), justify = "left")[1]

        print.object[, -1L] <- apply(print.object[, -1L], 2, function(y) format(y, justify = "right"))

        #......
        # Omit Total and V.Perc column if there are no missing values
        if (all(!is.na(x$data))) {

          # Object without Total column
          print.object <- print.object[, -ncol(print.object)]

          # Object without valid percentage
          print.object <- print.object[-grep("V.Perc", print.object[, 1L]), ]

        }

        #......
        # Omit Missing and Total row if perc = "v.valid" and freq = FALSE
        if (length(print) == 1L && print == "v.perc" && !isTRUE(freq)) {

          # Object without Total column
          print.object <- print.object[, -ncol(print.object)]

          # Object without Missing column
          print.object <- print.object[, -ncol(print.object)]

        }

        #......
        # Omit absolute frequencies if freq = FALSE
        if (!isTRUE(freq)) {

          print.object <- print.object[-grep("Freq", print.object[, 1L]), ]

        }

        #......
        # Omit percentages

        ###
        # Omit percentages if !"perc" %in% print
        if (!"perc" %in% print || "no" %in% print) {

          # Object without percentage
          print.object <- print.object[-which(row.names(print.object) == "Perc"), ]

        }

        ###
        # Omit valid percentages if !"v.perc" %in% print
        if ("V.Perc" %in% row.names(print.object)) {

          if (!"v.perc" %in% print || "no" %in% print) {

            # Object without valid percentage
            print.object <- print.object[-which(row.names(print.object) == "V.Perc"), ]

          }

        }

      }

      ####################################################################################
      # Output

      print(print.object, row.names = FALSE, max = 99999L)

    }

    #-----------------------------------------
    # More than one variable

    if (ncol(as.data.frame(x$data, stringsAsFactors = FALSE)) > 1L) {

      #........................................
      # split = FALSE
      if (!isTRUE(x$args$split)) {

        #..................
        # Values in rows
        if (!isTRUE(x$args$val.col)) {

          #....
          # Absolute frequencies
          if (isTRUE(freq)) {

            print.object$freq <- data.frame(x = c("Value", rep("", nrow(print.object$freq) - 1L), "Missing", "Total"),
                                            val = c(print.object$freq[1:(nrow(print.object$freq) - 1L), 1L], "Total", "NA", ""),
                                            rbind(print.object$freq[1:(nrow(print.object$freq) - 1L), -1L],
                                                  apply(print.object$freq[1:(nrow(print.object$freq) - 1L), -1], 2, function(y) sum(as.numeric(y), na.rm = TRUE)),
                                                  print.object$freq[nrow(print.object$freq), -1L],
                                                  apply(print.object$freq[, -1L], 2, function(y) sum(as.numeric(y), na.rm = TRUE))),
                                            stringsAsFactors = FALSE, row.names = NULL, check.names = FALSE)

            # Format
            print.object$freq[, 1L:2L] <- apply(print.object$freq[, 1L:2L], 2, function(y) format(y, justify = "left"))

            print.object$freq[, -c(1L:2L)] <- apply(print.object$freq[, -(1L:2L)], 2, function(y) format(y, justify = "right"))

            # No missing data
            if (all(!is.na(x$data))) {

              print.object$freq <- print.object$freq[-grep("Total",  print.object$freq$x), ]
              print.object$freq$val <- format(misty::trim(print.object$freq$val), justify = "left")

            }

            # Column names
            colnames(print.object$freq)[1L:2L] <- c("", "")

          }

          #....
          # Percentages
          if (all(print != "no") && "perc" %in% print) {

            print.object$perc <- data.frame(x = c("Value", rep("", nrow(print.object$perc) - 1L), "Missing", "Total"),
                                            val = c(print.object$perc[1:(nrow(print.object$perc) - 1L), 1L], "Total", "NA", ""),
                                            rbind(print.object$perc[1:(nrow(print.object$perc) - 1L), -1L],
                                                  apply(print.object$perc[1:(nrow(print.object$perc) - 1L), -1L], 2, function(y) sum(as.numeric(y), na.rm = TRUE)),
                                                  print.object$perc[nrow(print.object$perc), -1L],
                                                  apply(print.object$perc[, -1L], 2, function(y) sum(as.numeric(y), na.rm = TRUE))),
                                            stringsAsFactors = FALSE, row.names = NULL, check.names = FALSE)

            # Round digits
            print.object$perc[, -c(1L:2L)] <- apply(print.object$perc[, -c(1L:2L)], 2, function(y) paste0(formatC(as.numeric(y), digits = digits, format = "f"), "%"))

            # Format
            print.object$perc[, 1L:2L] <- apply(print.object$perc[, 1L:2L], 2, function(y) format(y, justify = "left"))

            print.object$perc[, -c(1L:2L)] <- apply(print.object$perc[, -(1L:2L)], 2, function(y) format(y, justify = "right"))

            # No missing data
            if (all(!is.na(x$data))) {

              print.object$perc <- print.object$perc[-grep("Total",  print.object$perc$x), ]
              print.object$perc$val <- format(misty::trim(print.object$perc$val), justify = "left")

            }

            # Column names
            colnames(print.object$perc)[1L:2L] <- c("", "")

          }

          #....
          # Valid percentages
          if (all(print != "no") && "v.perc" %in% print) {

            print.object$v.perc <- data.frame(x = c("Value", rep("", nrow(print.object$v.perc) - 1L), "Total"),
                                              val = c(print.object$v.perc[, 1L], ""),
                                              rbind(print.object$v.perc[, -1L],
                                                    apply(print.object$v.perc[, -1L], 2, function(y) sum(as.numeric(y), na.rm = TRUE))),
                                              stringsAsFactors = FALSE, row.names = NULL, check.names = FALSE)

            # Round digits
            print.object$v.perc[, -c(1L:2L)] <- apply(print.object$v.perc[, -c(1L:2L)], 2, function(y) paste0(formatC(as.numeric(y), digits = digits, format = "f"), "%"))

            # Format
            print.object$v.perc[, 1L:2L] <- apply(print.object$v.perc[, 1L:2L], 2, function(y) format(y, justify = "left"))

            # Format
            print.object$v.perc[, -c(1L:2L)] <- apply(print.object$v.perc[, -(1L:2L)], 2, function(y) format(y, justify = "right"))

            # Column names
            colnames(print.object$v.perc)[1L:2L] <- c("", "")

          }

        #..................
        # Values in columns
        } else {

          #....
          # Absolute frequencies
          if (isTRUE(freq)) {

            print.object$freq <- data.frame(print.object$freq[, 1L:(ncol(print.object$freq) - 1L)],
                                            val = apply(print.object$freq[, 2L:(ncol(print.object$freq) - 1L)], 1, function(y) sum(as.numeric(y), na.rm = TRUE)),
                                            miss = print.object$freq[, ncol(print.object$freq)],
                                            total = apply(print.object$freq[, 2L:(ncol(print.object$freq))], 1, function(y) sum(as.numeric(y), na.rm = TRUE)),
                                            check.names = FALSE, stringsAsFactors = FALSE)

            # Add variable names
            colnames(print.object$freq) <- c("", colnames(print.object$freq)[2L:(ncol(print.object$freq) - 3L)], "Total", "Missing", "Total")

            # Format
            print.object$freq[, 1L] <- format(print.object$freq[, 1L], justify = "left")

            print.object$freq[, -1L] <- apply(print.object$freq[, -1L], 2, function(y) format(y, justify = "right"))

            # No missing data
            if (all(!is.na(x$data))) {

              print.object$freq <- print.object$freq[, -ncol(print.object$freq)]

            }

          }

          #....
          # Percentages
          if (all(print != "no") && "perc" %in% print) {

            print.object$perc <- data.frame(print.object$perc[, 1L:(ncol(print.object$perc) - 1L)],
                                            val = apply(print.object$perc[, 2L:(ncol(print.object$perc) - 1)], 1, function(y) sum(as.numeric(y), na.rm = TRUE)),
                                            miss = print.object$perc[, ncol(print.object$perc)],
                                            total = apply(print.object$perc[, 2L:(ncol(print.object$perc))], 1, function(y) sum(as.numeric(y), na.rm = TRUE)),
                                            check.names = FALSE, stringsAsFactors = FALSE)

            # Add variable names
            colnames(print.object$perc) <- c("", colnames(print.object$perc)[2:(ncol(print.object$perc) - 3L)], "Total", "Missing", "Total")

            # Round digits
            print.object$perc[, -1L] <- apply(print.object$perc[, -1L], 2, function(y) paste0(formatC(as.numeric(y), digits = digits, format = "f"), "%"))

            # Format
            print.object$perc[, 1L] <- format(print.object$perc[, 1L], justify = "left")

            print.object$perc[, -1L] <- apply(print.object$perc[, -1L], 2, function(y) format(y, justify = "right"))

            # No missing data
            if (all(!is.na(x$data))) {

              print.object$perc <- print.object$perc[, -ncol(print.object$perc)]

            }

          }

          #....
          # Valid percentages
          if (all(print != "no") && "v.perc" %in% print) {

            print.object$v.perc <- data.frame(print.object$v.perc,
                                              total = apply(print.object$v.perc[, 2L:(ncol(print.object$v.perc))], 1, function(y) sum(as.numeric(y), na.rm = TRUE)),
                                              check.names = FALSE, stringsAsFactors = FALSE)

            # Add variable names
            colnames(print.object$v.perc) <- c("", colnames(print.object$v.perc)[2L:(ncol(print.object$v.perc) - 1L)], "Total")

            # Round digits
            print.object$v.perc[, -1L] <- apply(print.object$v.perc[, -1L], 2, function(y) paste0(formatC(as.numeric(y), digits = digits, format = "f"), "%"))

            # Format
            print.object$v.perc[, 1L] <- format(print.object$v.perc[, 1L], justify = "left")

            print.object$v.perc[, -1L] <- apply(print.object$v.perc[, -1L], 2, function(y) format(y, justify = "right"))

          }

        }

        ####################################################################################
        # Output

        #..................
        # Absolute frequencies
        if (isTRUE(freq)) {

          cat("Frequencies\n")
          print(print.object$freq, row.names = FALSE, max = 99999L)

        }

        #..................
        # Percentage frequencies
        if (all(print != "no")) {

          if (isTRUE(freq)) { cat("\n") }

          # Percentages
          if ("perc" %in% print) {

            cat("Percentages\n")
            print(print.object$perc, row.names = FALSE, max = 99999L)

          }

          if (any(is.na(x$data))) {

            # Valid percentages
            if ("v.perc" %in% print) {

              if ("perc" %in% print) { cat("\n") }

              cat("Valid Percentages\n")
              print(print.object$v.perc, quote = FALSE, row.names = FALSE, max = 99999L)

            }

          }

        }

        # split = TRUE
      } else {

        for (i in names(x$result)) {

          cat("\n", paste0("$", i), "\n", sep = "")

          temp <- list(call = x$call, data = x$data[, i], args = x$args, result = x$result[[i]])
          class(temp) <- "freq"

          print(temp, check = FALSE)

        }

      }

    }

  ####################################################################################
  #-----------------------------------------------------------------------------------
  #  Levene's Test for Homogeneity of Variance
  }, levenes.test = {

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
  #-----------------------------------------------------------------------------------
  # Multilevel Descriptive Statistics
  }, multilevel.descript = {

    ####################################################################################
    # Main Function

    print.object <- data.frame(cbind(c("No. of cases", "No. of missing values",
                                       "", "No. of groups", "Average group size", "SD group size", "Min group size", "Max group size",
                                       "", "ICC(1)", "ICC(2)",
                                       "", "Design effect", "Design effect sqrt", "Effective sample size"),
                                     rbind(x$result$no.obs, x$result$no.miss,
                                           "", x$result$no.group, x$result$m.group.size, x$result$sd.group.size, x$result$min.group.size, x$result$max.group.size,
                                           "", x$result$icc1, x$result$icc2,
                                           "", x$result$deff, x$result$deff.sqrt, x$result$n.effect)),
                               stringsAsFactors = FALSE)

    #-----------------------------------------
    # One variable
    if (length(x$result$no.obs) == 1L) {

      # Format
      for (i in c(5L, 6L, 13L, 14L, 15L)) {

        print.object[i, 2L] <- formatC(as.numeric(unlist(print.object[i, 2L])), digits = digits, format = "f")

      }

      print.object[10L, 2L] <- formatC(as.numeric(unlist(print.object[10L, 2L])), digits = icc.digits, format = "f")
      print.object[11L, 2L] <- formatC(as.numeric(unlist(print.object[11L, 2L])), digits = icc.digits, format = "f")

      print.object[, 1L] <- paste("", print.object[, 1L])


      print.object[, 1L] <- format(print.object[, 1L, drop = FALSE])

      print.object[, 1L] <- format(unlist(print.object[, 1L]), justify = "left")
      print.object[, 2L] <- format(as.character(print.object[, 2L]), justify = "right")

    #-----------------------------------------
    # More than one variable
    } else {

      print.object <- rbind(c("", names(x$result$no.obs)), print.object)

      # Format
      for (i in c(6L, 7L, 14L, 15L, 16L)) {

        print.object[i, 2L:ncol(print.object)] <- formatC(as.numeric(unlist(print.object[i, 2L:ncol(print.object)])), digits = digits, format = "f")

      }

      print.object[11L, 2L:ncol(print.object)] <- formatC(as.numeric(unlist(print.object[11L, 2L:ncol(print.object)])), digits = icc.digits, format = "f")
      print.object[12L, 2L:ncol(print.object)] <- formatC(as.numeric(unlist(print.object[12L, 2L:ncol(print.object)])), digits = icc.digits, format = "f")

      print.object[, 1L] <- paste("", print.object[, 1L])


      print.object[, 1L] <- format(print.object[, 1L, drop = FALSE])

      print.object[, 1L] <- format(unlist(print.object[, 1L]), justify = "left")
      print.object[, 2L:ncol(print.object)] <- apply(print.object[, 2L:ncol(print.object)], 2, function(y) format(as.character(y), justify = "right"))

    }

    ####################################################################################
    # Output

    cat("Multilevel Descriptive Statistics\n\n")

    write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE)

  ####################################################################################
  #-----------------------------------------------------------------------------------
  # Auxiliary variables analysis
  }, na.auxiliary = {

    ####################################################################################
    # Data and Arguments

    ####################################################################################
    # Main Function

    #-----------------------------------------
    # Format correlation matrix
    print.object$cor <- apply(print.object$cor, 2, function(y) formatC(as.numeric(y), digits = digits, format = "f"))

    # Lower and/or upper triangular
    if (tri == "lower") {

      print.object$cor[upper.tri(print.object$cor)] <- ""

    }

    if (tri == "upper") {

      print.object$cor[lower.tri(print.object$cor)] <- ""

    }

    diag(print.object$cor) <- ""

    #-----------------------------------------
    # Format Cohen's d matrix

    print.object$d <- apply(print.object$d, 2, function(y) formatC(as.numeric(y), digits = digits, format = "f"))
    diag(print.object$d) <- ""

    # Print table
    print.object  <- data.frame(cbind(c("", colnames(print.object$cor), "", colnames(print.object$cor)),
                                      rbind(colnames(print.object$cor), print.object$cor, colnames(print.object$cor), print.object$d)),
                                stringsAsFactors = FALSE)

    # Format
    print.object[, 1L] <- paste0("   ", print.object[, 1L])

    print.object[, 1L] <- format(print.object[, 1L], justify = "left")

    print.object[, -1L] <- apply(print.object[, -1L], 2, function(y) format(y, justify = "right"))

    ####################################################################################
    # Output

    cat(" Auxiliary Variables\n\n",
        " Variables related to the incomplete variable\n\n",
        "  Pearson product-moment correlation matrix\n")

    write.table(print.object[1:(nrow(x$result$cor) + 1L), ], quote = FALSE, row.names = FALSE, col.names = FALSE)

    cat("\n")

    cat("  Variables related to the probability of missigness\n\n",
        "  Cohen's d\n")

    write.table(print.object[(nrow(x$result$cor) + 2L):nrow(print.object), ], quote = FALSE, row.names = FALSE, col.names = FALSE)

    cat("\n",
        " Note. Indicator variables are in the rows (0 = miss, 1 = obs)\n")

  ####################################################################################
  #-----------------------------------------------------------------------------------
  # Variance-Covariance Coverage
  }, na.coverage = {

    #-----------------------------------------------------------------------------------
    # Main Function

    #........................................
    # Lower and/or upper triangular

    if (tri == "lower") {

      print.object[upper.tri(print.object)] <- ""

    }

    if (tri == "upper") {

      print.object[lower.tri(print.object)] <- ""

    }

    #-----------------------------------------------------------------------------------
    # Main Function

    # Format proportions
    print.object <- apply(print.object, 2, function(y) ifelse(!is.na(as.numeric(y)), formatC(as.numeric(y), digits = digits, format = "f"), ""))

    # Row names
    row.names(print.object) <- paste0("  ", row.names(x$result))

    #-----------------------------------------------------------------------------------
    # Output

    cat(" Variance-Covariance Coverage\n\n")

    print(print.object,  quote = FALSE, row.names = FALSE, max = 99999L, right = TRUE)

  ####################################################################################
  #-----------------------------------------------------------------------------------
  # Descriptive Statistics for Missing Data
  }, na.descript = {

    ####################################################################################
    # Main Function

    #----------------------------------------
    # Result table
    restab <- data.frame(statistic = c("No. of cases", "No. of complete cases", "No. of incomplete cases", "No. of values",
                                       "No. of observed values", "No. of missing values", "No. of variables",
                                       "Mean", "SD", "Minimum", "P25", "P75", "Maximum"),
                         no = c(print.object$no.cases, print.object$no.complete, print.object$no.incomplete,
                                print.object$no.values, print.object$no.observed.values, print.object$no.missing.values,
                                print.object$no.var, print.object$no.missing.mean, print.object$no.missing.sd,
                                print.object$no.missing.min, print.object$no.missing.p25, print.object$no.missing.p75, print.object$no.missing.max),
                         perc = c("", print.object$perc.complete, print.object$perc.incomplete, "", print.object$perc.observed.values,
                                  print.object$perc.missing.values, "", print.object$perc.missing.mean, print.object$perc.missing.sd,
                                  print.object$perc.missing.min, print.object$perc.missing.p25, print.object$perc.missing.p75, print.object$perc.missing.max),
                         stringsAsFactors = FALSE)

    #----------------------------------------
    # Format
    restab$statistic <- paste0("  ", restab$statistic)

    restab$statistic[8L:13L] <- paste0("  ", restab$statistic[8L:13L] )
    restab$statistic <- format(restab$statistic, justify = "left", width = max(nchar(restab$statistic)) + 1L)

    restab$no[8L:13L] <- format(formatC(as.numeric(restab$no[8L:13L]), digits = digits, format = "f"), justify = "right")
    restab$no[1L:7L] <- format(formatC(as.numeric(restab$no[1:7]), digits = 0L, format = "f"), justify = "right")
    restab$no <- format(restab$no, justify = "right")

    restab$perc[restab$perc != ""] <- paste0("(", formatC(as.numeric(restab$perc[restab$perc != ""]), digits = digits, format = "f"), "%)")

    restab$perc <- format(restab$perc, width = max(nchar(restab$perc)), justify = "right")

    ####################################################################################
    # Output

    cat(" Descriptive Statistics for Missing Data\n\n")

    write.table(restab[1L:3L, ], quote = FALSE, row.names = FALSE, col.names = FALSE)

    cat("\n")
    write.table(restab[4L:6L, ], quote = FALSE, row.names = FALSE, col.names = FALSE)

    cat("\n")
    write.table(restab[7L, ], quote = FALSE, row.names = FALSE, col.names = FALSE)
    cat("  No. of missing values across all variables\n")
    write.table(restab[8L:13L, ], quote = FALSE, row.names = FALSE, col.names = FALSE)

    #----------------------------------------
    # Frequency table
    if (isTRUE(table)) {

      freqtab <- x$result$table.miss

      freqtab[, c("pObs", "pNA")] <- apply(freqtab[, c("pObs", "pNA")], 2, function(y) paste0(formatC(y, digits = digits, format = "f"), "%"))
      freqtab <- rbind(colnames(freqtab), freqtab)

      freqtab[, -1L] <- apply(freqtab[ -1L], 2, format, justify = "right")

      freqtab[, 1L] <- paste0("    ", format(freqtab[, 1L], justify = "left"))

      cat("\n")
      write.table(freqtab, quote = FALSE, row.names = FALSE, col.names = FALSE)

    }

  ####################################################################################
  #-----------------------------------------------------------------------------------
  # Missing Data Pattern
  }, na.pattern = {

    ####################################################################################
    # Main Function

    # Percentages
    print.object[, "Perc"] <- paste0(formatC(as.numeric(print.object[, "Perc"]), digits = digits, format = "f"), "%")
    print.object[, "pNA"] <- paste0(formatC(as.numeric(print.object[, "pNA"]), digits = digits, format = "f"), "%")
    print.object[nrow(print.object), ncol(print.object)] <- ""

    # Format
    colnames(print.object)[1L] <- " Pattern"

    ####################################################################################
    # Output

    cat(" Missing Data Pattern\n\n")

    print(print.object, row.names = FALSE, max = 99999L, right = TRUE)

  ####################################################################################
  #-----------------------------------------------------------------------------------
  # Coefficient Omega
  }, omega.coef = {



    #----------------------------------------
    # Input Check

    if (isTRUE(check)) {

      #......
      # Check input 'print'
      if (!all(print %in% c("all", "omega", "item"))) {

        stop("Character strings in the argument 'print' do not all match with \"all\", \"omega\", or \"item\".",
             call. = FALSE)

      }

    }

    #----------------------------------------
    # Arguments

    # Print coefficient omega and/or item statistic
    if (length(print) == 1L && "all" %in% print) { print <- c("omega", "item") }

    #-----------------------------------------------------------------------------------
    # Main Function

    #-----------------------------------------
    # Omega
    if ("omega" %in% print) {

      print.object$omega$n <- format(paste("", print.object$omega$n), justify = "right")

      print.object$omega$items <- format(print.object$omega$items, justify = "right")

      print.object$omega$omega <- formatC(print.object$omega$omega, digits = digits, format = "f")

      print.object$omega$low <- formatC(print.object$omega$low, digits = digits, format = "f")
      print.object$omega$upp <- formatC(print.object$omega$upp, digits = digits, format = "f")

      print.object$omega <- rbind(c(" n", "Items", "Omega", "Low", "Upp"), print.object$omega)

      print.object$omega <- apply(print.object$omega, 2, function(y) format(y, justify = "right"))

      if (x$args$type == "omega") {

        cat(paste0(ifelse(isTRUE(x$args$std), "Standardized ", "Unstandardized "), "Coefficient Omega with ",
                   x$args$conf.level*100L, "% Confidence Interval\n\n"))

      } else  if (x$args$type == "hierarch") {

        cat(paste0(ifelse(isTRUE(x$args$std), "Standardized ", "Unstandardized "), "Hierarchical Omega with ",
                   x$args$conf.level*100L, "% Confidence Interval\n\n"))

      } else if (x$args$type == "categ") {

        cat(paste0("Categorical Omega with ", x$args$conf.level*100L, "% Confidence Interval\n\n"))

      }

      write.table(print.object$omega, quote = FALSE, row.names = FALSE, col.names = FALSE)

    }

    #-----------------------------------------
    # Item statistics

    if ("item" %in% print) {

      print.object$item$pNA <- paste0(formatC(print.object$item$pNA, digits = 2L, format = "f"), "%")
      print.object$item$m <- formatC(print.object$item$m, digits = 2L, format = "f")
      print.object$item$sd <- formatC(print.object$item$sd, digits = 2L, format = "f")
      print.object$item$min <- formatC(print.object$item$min, digits = 2L, format = "f")
      print.object$item$max <- formatC(print.object$item$max, digits = 2L, format = "f")

      print.object$item$std.ld <- formatC(print.object$item$std.ld, digits = digits, format = "f")
      print.object$item$omega <- formatC(print.object$item$omega, digits = digits, format = "f")

      print.object$item <- rbind(c("Variable", "n", "nNA", "pNA", "M", "SD", "Min", "Max", "Std.Ld", "Omega"),
                                 print.object$item)

      # Format
      print.object$item[, 1L] <- format(paste("", print.object$item[, 1L]), justify = "left")
      print.object$item[, -1L] <- apply(print.object$item[, -1L], 2, function(y) format(y, justify = "right"))

      if ("omega" %in% print) { cat("\n") }

      if (x$args$type == "omega") {

        cat("Standardized Factor Loadings and Coefficient Omega if Item Deleted\n\n")

      } else  if (x$args$type == "hierarch") {

        cat("Standardized Factor Loadings and Hierarchical Omega if Item Deleted\n\n")

      } else if (x$args$type == "categ") {

        cat("Standardized Factor Loadings and Categorical Omega if Item Deleted\n\n")

      }

      write.table(print.object$item, quote = FALSE, row.names = FALSE, col.names = FALSE)

    }

  ####################################################################################
  #-----------------------------------------------------------------------------------
  # Phi Coefficient
  }, phi.coef = {

    ####################################################################################
    # Data and Arguments

    # Print object
    print.object <- x$result

    # Print triangular
    tri <- ifelse(all(c("both", "lower", "upper") %in% tri), "lower", tri)

    ####################################################################################
    # Main Function

    #----------------------------------------
    # Two variables
    if (is.null(dim(print.object))) {

      print.object <- cbind("  Estimate: ", formatC(print.object, digits = digits, format = "f"))

    #----------------------------------------
    # More than two variables
    } else {

      # Format contingenccy coefficients
      print.object <- formatC(print.object, digits = digits, format = "f")

      # Lower and/or upper triangular
      if (tri == "lower") {

        print.object[upper.tri(print.object)] <- ""

      }

      if (tri == "upper") {

        print.object[lower.tri(print.object)] <- ""

      }

      # Set diagonal to "
      diag(print.object) <- ""

      # Format
      row.names(print.object) <- paste("", row.names(print.object))

    }

    ####################################################################################
    # Output

    if (is.null(dim(x$result))) {

      if (isTRUE(x$args$adjust)) {

        cat("Adjusted Phi Coefficient\n\n")

      } else {

        cat("Phi Coefficient\n\n")

      }

    } else {

      if (isTRUE(x$args$adjust)) {

        cat("Adjusted Phi Coefficient Matrix\n\n")

      } else {

        cat("Phi Coefficient Matrix\n\n")

      }

    }

    if (is.null(dim(x$result))) {

      write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE)

    } else {

      print(print.object, quote = FALSE, right = TRUE)

    }

  ####################################################################################
  #-----------------------------------------------------------------------------------
  # Polychoric Correlation Matrix
  }, poly.cor = {

    ####################################################################################
    # Data and Arguments

    #----------------------------------------
    # Print object

    print.object <- x$result$cor

    #----------------------------------------
    # Print triangular

    tri <- ifelse(all(c("both", "lower", "upper") %in% tri), "lower", tri)

    ####################################################################################
    # Main Function

    #----------------------------------------
    # Print object

    print.object <- formatC(print.object, digits = digits, format = "f")
    row.names(print.object) <- paste("", row.names(print.object))


    # Empty matrix diagonal
    diag(print.object) <- ""

    #----------------------------------------
    # Lower and/or upper triangular

    if (tri == "lower") {

      print.object[upper.tri(print.object)] <- ""

    }

    if (tri == "upper") {

      print.object[lower.tri(print.object)] <- ""

    }

    #----------------------------------------
    # Row names
    if (!is.null(rownames(print.object))) {

      row.names(print.object) <- format(row.names(print.object), justify = "left")

    }

    ####################################################################################
    # Output

    cat("Polychoric Correlation Matrix\n\n")

    print(print.object, quote = FALSE, right = TRUE, max = 99999L)

  ####################################################################################
  #-----------------------------------------------------------------------------------
  # Sample Size Determination
  }, size = {

    ####################################################################################
    # Main function

    #----------------------------------------
    # Arithmetic mean
    switch(x$size, mean = {

      cat("\nSample Size Determination for the", ifelse(x$args$sample == "one.sample", "One-Sample", "Two-Sample"), "t-Test\n\n")

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

        cat("\nSample Size Determination for the", ifelse(x$args$sample == "one.sample", "One-Sample", "Two-Sample"), "Proportion Test with Continuity Correction\n\n")

      } else {

        cat("\nSample Size Determination for the", ifelse(x$args$sample == "one.sample", "One-Sample", "Two-Sample"), "Proportion Test without Continuity Correction\n\n")

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

      cat("\nSample Size Determination for Pearson's Product-Moment Correlation Coefficient\n\n")

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

  ####################################################################################
  #-----------------------------------------------------------------------------------
  # Standardized Coefficients
  }, std.coef = {

    if (isTRUE(check)) {

      #......
      # Check input 'print'
      if (!all(print %in% c("all", "stdx", "stdy", "stdyx"))) {

        stop("Character strings in the argument 'print' do not all match with \"all\", \"stdx\", \"stdy\", or \"stdyx\".",
             call. = FALSE)

      }

    }

    #......
    # Print object
    print.object <- print.object$coefficients

    #----------------------------------------
    # Arguments

    #................
    # Print = "all"
    if (length(print) == 1L && "all" %in% print) { print <- c("stdx", "stdy", "stdyx") }

    #-----------------------------------------------------------------------------------
    # Main Function

    #-----------------------------------------
    # Exclude columns
    if (!"stdx" %in% print) {

      print.object <- print.object[, which(colnames(print.object) != "StdX")]

    }

    if (!"stdy" %in% print) {

      print.object <- print.object[, which(colnames(print.object) != "StdY")]

    }

    if (!"stdyx" %in% print) {

      print.object <- print.object[, which(colnames(print.object) != "StdYX")]

    }

    #-----------------------------------------
    # Round
    print.object[, -4] <- apply(print.object[, -4], 2L, function(y) formatC(y, digits = digits, format = "f"))

    print.object[, 4] <- formatC(as.numeric(print.object[, 4]), digits = p.digits, format = "f")

    #-----------------------------------------
    # Repace NA with ""
    print.object <- apply(print.object, 2L, function(y) gsub("NA", "", y))

    #-----------------------------------------
    # Format
    print.object <- apply(print.object, 2L, function(y) format(y, justify = "right"))

    row.names(print.object) <- paste(" ", row.names(print.object))

    #-----------------------------------------
    # Print
    cat(" Unstandardized and Standardized Coefficients\n\n")

    # Print object
    print(print.object, quote = FALSE, right = TRUE)

    # Note for model involving categorical predictors
    if (any(c("stdy", "stdyx") %in% print)) {

      cat("\n  Note. SD of the criterion variable", names(x$result$sd)[1], "=", round(x$result$sd[1], digits = digits))

    }

  ####################################################################################
  #-----------------------------------------------------------------------------------
  # z-Test
  }, z.test = {

    cat("TEST")

    switch(x$test, z.test.one = {

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

      cat(paste0(" One sample z-Test with ", "\u03c3 = ", round(x$args$sigma, digits = digits), "\n\n"))

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

        cat(paste0(" Two Sample z-Test with ", "\u03c3\u2081 = \u03c3\u2082 = ", round(x$args$sigma[1], digits = digits), "\n\n"))

      } else {

        cat(paste0(" Two Sample z-Test with ", "\u03c3\u2081 = ", x$args$sigma[1], " and ", "\u03c3\u2082 = ", round(x$args$sigma[2], digits = digits), "\n\n"))

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

      cat(paste0(" Paired Sample z-Test with ", "\u03c3(diff) = ", round(x$args$sigma, digits = digits), "\n\n"))

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

  })

}
