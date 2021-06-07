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
#'                   tabulation (\code{crosstab()} function).
#' @param hypo       logical: if \code{TRUE}, null and alternative hypothesis are shown on the console
#'                   (\code{\link{test.t}}, \code{\link{test.welch}}, \code{\link{test.z}} function).
#' @param descript   logical: if \code{TRUE}, descriptive statistics are shown on the console
#'                   (\code{\link{test.t}}, \code{\link{test.welch}}, \code{\link{test.z}} function).
#' @param effsize    logical: if \code{TRUE}, effect size measure(s) is shown on the console
#'                   (\code{\link{test.t}}, \code{\link{test.welch}}, \code{\link{test.z}} function).
#' @param split      logical: if \code{TRUE}, output table is split by variables when specifying more than
#'                   one variable in \code{x} (\code{\link{freq}}).
#' @param table      logical: if \code{TRUE}, a frequency table with number of observed values (\code{"nObs"}),
#'                   percent of observed values (\code{"pObs"}), number of missing values (\code{"nNA"}), and
#'                   percent of missing values (\code{"pNA"}) is printed for each variable on the console
#'                   (\code{na.descript()} function).
#' @param digits     an integer value indicating the number of decimal places digits to be used for
#'                   displaying results.
#' @param p.digits   an integer indicating the number of decimal places to be used for displaying
#'                   \emph{p}-values.
#' @param icc.digits an integer indicating the number of decimal places to be used for displaying
#'                   intraclass correlation coefficients (\code{multilevel.descript()} and
#'                   \code{multilevel.icc()} function).
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
#' \code{\link{item.alpha}}, \code{\link{ci.mean.diff}}, \code{\link{ci.mean}}, \code{\link{ci.median}},
#' \code{\link{ci.prop.diff}}, \code{\link{ci.prop}}, \code{\link{ci.sd}}, \code{\link{ci.var}},
#' \code{\link{cohens.d}}, \code{\link{collin.diag}}, \code{\link{cor.cont}}, \code{\link{cor.matrix}},
#' \code{\link{cor.cramer}}, \code{\link{crosstab}}, \code{\link{descript}}, \code{\link{eta.sq}},
#' \code{\link{freq}}, \code{\link{test.levene}}, \code{\link{multilevel.descript}},
#' \code{\link{na.auxiliary}}, \code{\link{na.coverage}}, \code{\link{na.descript}},
#' \code{\link{na.pattern}}, \code{\link{item.omega}}, \code{\link{cor.phi}}, \code{\link{cor.poly}},
#' \code{\link{size.cor}}, \code{\link{size.mean}}, \code{\link{size.prop}}, \code{\link{test.levene}},
#' \code{\link{test.t}}, \code{\link{test.welch}}, \code{\link{test.z}}.
#'
#' @method print misty.object
#'
#' @export
#'
#' @examples
print.misty.object <- function(x, print = x$args$print, tri = x$args$tri, freq = x$args$freq,
                               hypo = x$args$hypo, descript = x$args$descript, effsize = x$args$effsize,
                               split = x$args$split, table = x$args$table, digits = x$args$digits,
                               p.digits = x$args$p.digits, icc.digits = x$args$icc.digits,
                               sort.var = x$args$sort.var, order = x$args$order, check = TRUE, ...) {

  ####################################################################################
  # Input Check

  if (isTRUE(check)) {

    #......
    # Check input 'tri'
    if (isTRUE(!is.null(tri))) {

      if (isTRUE(any(!tri %in% c("both", "lower", "upper")))) {

        stop("Character string in the argument 'tri' does not match with \"both\", \"lower\", or \"upper\".",
             call. = FALSE)

      }

    }

    #......
    # Check input 'table'
    if (isTRUE(!is.null(table))) {

      if (isTRUE(!is.logical(table))) {

        stop("Please specify TRUE or FALSE for the argument 'table'.", call. = FALSE)

      }

    }

    #......
    # Check input 'freq'
    if (isTRUE(!is.null(freq))) {

      if (isTRUE(!is.logical(freq))) {

        stop("Please specify TRUE or FALSE for the argument 'freq'.", call. = FALSE)

      }

    }

    #......
    # Check input 'hypo'
    if (isTRUE(!is.null(hypo))) {

      if (isTRUE(!is.logical(hypo))) {

        stop("Please specify TRUE or FALSE for the argument 'hypo'.", call. = FALSE)

      }

    }

    #......
    # Check input 'descript'
    if (isTRUE(!is.null(descript))) {

      if (isTRUE(!is.logical(descript))) {

        stop("Please specify TRUE or FALSE for the argument 'descript'.", call. = FALSE)

      }

    }

    #......
    # Check input 'effsize'
    if (isTRUE(!is.null(effsize))) {

      if (isTRUE(!is.logical(effsize))) {

        stop("Please specify TRUE or FALSE for the argument 'effsize'.", call. = FALSE)

      }

    }

    #......
    # Check input 'digits'
    if (isTRUE(!is.null(digits))) {

      if (isTRUE(digits %% 1L != 0L || digits < 0L)) {

        stop("Specify a positive integer number for the argument 'digits'", call. = FALSE)

      }

    }

    #......
    # Check input 'p.digits'
    if (isTRUE(!is.null(p.digits))) {

      if (isTRUE(p.digits %% 1L != 0L || p.digits < 0L)) {

        stop("Specify a positive integer number for the argument 'p.digits'", call. = FALSE)

      }

    }

    #......
    # Check input 'icc.digits'
    if (isTRUE(!is.null(icc.digits))) {

      if (isTRUE(icc.digits %% 1L != 0L || icc.digits < 0L)) {

        stop("Specify a positive integer number for the argument 'icc.digits'", call. = FALSE)

      }

    }

    #......
    # Check input 'sort.var'
    if (isTRUE(!is.null(sort.var))) {

      if (isTRUE(!is.logical(sort.var))) {

        stop("Please specify TRUE or FALSE for the argument 'sort.var'.", call. = FALSE)

      }

    }

    #......
    # Check input 'order'
    if (isTRUE(!is.null(order))) {

      if (isTRUE(!is.logical(order))) {

        stop("Please specify TRUE or FALSE for the argument 'order'.", call. = FALSE)

      }

    }

  }

  #......
  # Print object
  print.object <- x$result

  ####################################################################################
  #-----------------------------------------------------------------------------------
  # Confidence intervals
  switch(x$type, ci = {

    #......
    # Variables to round
    print.round <- switch(x$ci,
                          mean = c("m", "sd", "low", "upp"),
                          mean.diff.i = c("m", "sd", "m.diff", "low", "upp"),
                          mean.diff.p = c("m1", "sd1", "m2", "sd2", "m.diff", "sd.diff", "low", "upp"),
                          prop.diff.i = c("p", "p.diff", "low", "upp"),
                          prop.diff.p = c("p1", "p2", "p.diff", "low", "upp"),
                          median = c("med", "iqr", "low", "upp"),
                          prop = c("prop", "low", "upp"),
                          var = c("m", "var", "low", "upp"),
                          sd = c("m", "sd", "low", "upp"))

    ####################################################################################
    # Main Function

    #----------------------------------------
    # No grouping

    if (isTRUE(is.null(x$data$group) && is.null(x$data$split))) {

      #......
      # Print names
      print.names <- switch(x$ci,
                            mean = c("Variable", "n", "nNA", "pNA", "M", "SD", "Low", "Upp"),
                            mean.diff.i = c("Variable", "Between", "n", "nNA", "M", "SD", "M.Diff", "Low", "Upp"),
                            mean.diff.p = c("Variable", "n", "nNA", "M1", "SD1", "M2", "SD2", "M.Diff", "SD.Diff", "Low", "Upp"),
                            prop.diff.i = c("Variable", "Between", "n", "nNA", "p", "p.Diff", "Low", "Upp"),
                            prop.diff.p = c("Variable", "n", "nNA", "p1", "p2", "p.Diff", "Low", "Upp"),
                            median = c("Variable", "n", "nNA", "pNA", "Med", "IQR", "Low", "Upp"),
                            prop = c("Variable", "n", "nNA", "pNA", "Prop", "Low", "Upp"),
                            var = c("Variable", "n", "nNA", "pNA", "M", "Var", "Low", "Upp"),
                            sd = c("Variable", "n", "nNA", "pNA", "M", "SD", "Low", "Upp"))

      #......
      # Round
      print.object[, print.round] <- sapply(print.round, function(y) ifelse(!is.na(print.object[, y]),
                                                                            formatC(print.object[, y], digits = digits, format = "f",
                                                                                    zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

      #......
      # Percentages
      if (isTRUE(!x$ci %in% c("mean.diff.i", "mean.diff.p", "prop.diff.i", "prop.diff.p"))) {

        print.object[, "pNA"] <- round(print.object[, "pNA"], digits = 2L)
        print.object[, "pNA"] <- paste0(print.object[, "pNA"], "%")

      }

      #......
      # Col names
      print.object <- rbind(print.names, print.object)

      #......
      # Format
      # Remove duplicated values
      print.object[duplicated(print.object$variable) , "variable"] <- ""

      col.format <- which(colnames(print.object) %in% c("variable", "between"))

      # Justify left and right
      print.object[, col.format] <- format(print.object[, col.format], justify = "left")
      print.object[, -col.format] <- apply(print.object[, -col.format], 2, format, justify = "right")

      # Add blank space
      print.object[, "variable"] <- c(paste0(" ", print.object[1L, "variable"]), paste0("  ", print.object[-1L, "variable"]))
      print.object[, "variable"] <- format(c(print.object[1L, "variable"], misty::chr.trim(print.object[-1L, "variable"], side = "right")), justify = "left")

      if (isTRUE(x$ci %in% c("mean.diff.i", "prop.diff.i"))) {

        print.object[, "between"] <- c(print.object[1L, "between"], paste0(" ", print.object[-1L, "between"]))
        print.object[, "between"] <- format(c(print.object[1L, "between"], misty::chr.trim(print.object[-1L, "between"], side = "right")), justify = "left")

        # NAs
        colnames.NA <- c("m.diff", "p.diff", "low", "upp")

        print.object[, which(colnames(print.object) %in% colnames.NA)] <- apply(print.object[, which(colnames(print.object) %in% colnames.NA), drop = FALSE], 2, function(y) gsub("NA", "  ", y))

      }

      # One Variable
      if (isTRUE(length(unique(x$result$variable)) == 1L)) {

        print.object <- print.object[, -1L]

        if (isTRUE(!isTRUE(x$args$paired))) {

          print.object[, "between"] <- c(paste0(" ", print.object[1L, "between"]), paste0(" ", print.object[-1L, "between"]))
          print.object[, "between"] <- format(c(print.object[1L, "between"], misty::chr.trim(print.object[-1L, "between"], side = "right")), justify = "left")

        } else {

          print.object[, "n"] <- c(paste0(" ", print.object[1L, "n"]), paste0(" ", print.object[-1L, "n"]))
          print.object[, "n"] <- format(c(print.object[1L, "n"], misty::chr.trim(print.object[-1L, "n"], side = "right")), justify = "right")

        }

      }

      #......
      # Print output
      cat(paste(switch(x$ci,
                       mean = "Arithmetic Mean with",
                       mean.diff.i = "Difference in Means from Independent Samples with",
                       mean.diff.p = "Difference in Means from Paired Samples with",
                       prop.diff.i = "Difference in Proportions from Independent Samples with",
                       prop.diff.p = "Difference in Proportions from Paired Samples with",
                       median = "Median with",
                       prop = "Proportion with",
                       var = "Variance with",
                       sd = "Standard Deviation with"), switch(x$args$alternative,
                                                               two.sided = "Two-Sided",
                                                               less = "One-Sided",
                                                               greater = "One-Sided"),
                paste0(round(x$args$conf.level * 100L, digits = 2L), "%"), "CI\n\n"))

      write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE)

      #......
      # Notes

      # Arithmetic mean
      if (isTRUE(x$ci == "mean")) {

        if (isTRUE(!is.null(x$args$sigma))) {

          cat(paste0("\n Note. Known population SD: Sigma = ", round(x$args$sigma, digits = 2L), "\n"))

        }

      }

      # Difference in arithmetic mean from independent samples
      if (isTRUE(x$ci == "mean.diff.i")) {

        if (isTRUE(x$args$var.equal && is.null(x$args$sigma))) {

          cat(paste0("\n Note. Equal population variance assumption"))

        }

        if (isTRUE(!is.null(x$args$sigma))) {

          if (isTRUE(length(unique(x$args$sigma)) == 1L)) {

            cat(paste0("\n Note. Known equal population SD: Sigma = ", round(unique(x$args$sigma), digits = 2L), "\n"))

          } else if (isTRUE(length(unique(x$args$sigma)) == 2L)) {

            cat(paste0("\n Note. Known unequal population SDs: Sigma1 = ", round(x$args$sigma[1L], digits = 2L), ", ",
                       "Sigma2 = ", round(x$args$sigma[2L], digits = 2L), "\n"))

          }

        }

      }

      # Difference in arithmetic mean from paired samples
      if (isTRUE(x$ci == "mean.diff.p")) {

        if (isTRUE(!is.null(x$args$sigma))) {

          cat(paste0("\n Note. Known population SD of difference scores: Sigma = ", round(x$args$sigma, digits = 2L), "\n"))

        }

      }

    #----------------------------------------
    # Grouping
    } else if (isTRUE(!is.null(x$data$group) && is.null(x$data$split))) {

      #......
      # Print names
      print.names <- switch(x$ci,
                            mean = c("Group", "Variable", "n", "nNA", "pNA", "M", "SD", "Low", "Upp"),
                            mean.diff.i = c("Group", "Variable", "Between", "n", "nNA", "M", "SD", "M.Diff", "Low", "Upp"),
                            mean.diff.p = c("Group", "Variable", "n", "nNA", "M1", "SD1", "M2", "SD2", "M.Diff", "SD.Diff", "Low", "Upp"),
                            prop.diff.i = c("Group", "Variable", "Between", "n", "nNA", "p", "p.Diff", "Low", "Upp"),
                            prop.diff.p = c("Group", "Variable", "n", "nNA", "p1", "p2", "p.Diff", "Low", "Upp"),
                            median = c("Group", "Variable", "n", "nNA", "pNA", "Med", "IQR", "Low", "Upp"),
                            prop = c("Group", "Variable", "n", "nNA", "pNA", "Prop", "Low", "Upp"),
                            var = c("Group", "Variable", "n", "nNA", "pNA", "M", "Var", "Low", "Upp"),
                            sd = c("Group", "Variable", "n", "nNA", "pNA", "M", "SD", "Low", "Upp"))

      #......
      # Sort by variables
      if (isTRUE(sort.var)) {

        print.object <- print.object[order(print.object[, "variable"]), ]

      }

      #......
      # Round
      print.object[, print.round] <- sapply(print.round, function(y) ifelse(!is.na(print.object[, y]),
                                                                            formatC(print.object[, y], digits = digits, format = "f",
                                                                                    zero.print = paste0("0.", paste(rep(0, times = digits), collapse = ""))), NA))

      #......
      # Percentages
      if (isTRUE(!x$ci %in% c("mean.diff.i", "mean.diff.p", "prop.diff.i", "prop.diff.p"))) {

        print.object[, "pNA"] <- round(print.object[, "pNA"], digits = 2)
        print.object[, "pNA"] <- paste0(print.object[, "pNA"], "%")

      }

      #......
      # Col names
      print.object <- rbind(print.names, print.object)

      #......
      # Format

      # Remove duplicated labels
      print.object[duplicated(paste(print.object$group, print.object$variable)) , c("group", "variable")] <- ""

      col.format <- which(colnames(print.object) %in% c("group", "variable", "between"))

      # Justify left and right
      print.object[, col.format] <- format(print.object[, col.format], justify = "left")
      print.object[, -col.format] <- apply(print.object[, -col.format], 2, format, justify = "right")

      # Add blank space
      print.object[, "group"] <- c(paste0(" ", print.object[1L, "group"]), paste0("  ", print.object[-1L, "group"]))
      print.object[, "group"] <- format(c(print.object[1L, "group"], misty::chr.trim(print.object[-1L, "group"], side = "right")), justify = "left")

      print.object[, "variable"] <- c(print.object[1L, "variable"], paste0(" ", print.object[-1L, "variable"]))
      print.object[, "variable"] <- format(c(print.object[1L, "variable"], misty::chr.trim(print.object[-1L, "variable"], side = "right")), justify = "left")

      if (isTRUE(x$ci %in% c("mean.diff.i", "prop.diff.i"))) {

        print.object[, "between"] <- c(print.object[1L, "between"], paste0(" ", print.object[-1L, "between"]))
        print.object[, "between"] <- format(c(print.object[1L, "between"], misty::chr.trim(print.object[-1L, "between"], side = "right")), justify = "left")

        # NAs
        colnames.NA <- c("m.diff", "p.diff", "low", "upp")

        print.object[, which(colnames(print.object) %in% colnames.NA)] <- apply(print.object[, which(colnames(print.object) %in% colnames.NA), drop = FALSE], 2, function(y) gsub("NA", "  ", y))

      }

      # Only one variable
      if (isTRUE(length(unique(x$result$variable)) == 1L)) {

        print.object <- print.object[, -2L]

      }

      #......
      # Print output
      cat(paste(switch(x$ci,
                       mean = "Arithmetic Mean with",
                       mean.diff.i = "Difference in Means from Independent Samples with",
                       mean.diff.p = "Difference in Means from Paired Samples with",
                       prop.diff.i = "Difference in Proportions from Independent Samples with",
                       prop.diff.p = "Difference in Proportions from Paired Samples with",
                       median = "Median with",
                       prop = "Proportion with",
                       var = "Variance with",
                       sd = "Standard Deviation with"), switch(x$args$alternative,
                                                               two.sided = "Two-Sided",
                                                               less = "One-Sided",
                                                               greater = "One-Sided"),
                paste0(round(x$args$conf.level * 100L, digits = 2L), "%"), "CI\n\n"))

      write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE)

      #......
      # Note

      # Difference in arithmetic mean from independent samples
      if (isTRUE(x$ci == "mean.diff.i")) {

        if (isTRUE(x$args$var.equal)) {

          cat(paste0("\n Note. Equal population variance assumption\n"))

        }
      }

    #----------------------------------------
    # Split
    } else if (isTRUE(!is.null(x$data$split))) {

      #......
      # No grouping
      if (isTRUE(is.null(x$data$group))) {

        for (i in names(print.object)) {

          #......
          # Print names
          print.names <- switch(x$ci,
                                mean = c("Variable", "n", "nNA", "pNA", "M", "SD", "Low", "Upp"),
                                mean.diff.i = c("Variable", "Between", "n", "nNA", "M", "SD", "M.Diff", "Low", "Upp"),
                                mean.diff.p = c("Variable", "n", "nNA", "M1", "SD1", "M2", "SD2", "M.Diff", "SD.Diff", "Low", "Upp"),
                                prop.diff.i = c("Variable", "Between", "n", "nNA", "p", "p.Diff", "Low", "Upp"),
                                prop.diff.p = c("Variable", "n", "nNA", "p1", "p2", "p.Diff", "Low", "Upp"),
                                median = c("Variable", "n", "nNA", "pNA", "Med", "IQR", "Low", "Upp"),
                                prop = c("Variable", "n", "nNA", "pNA", "Prop", "Low", "Upp"),
                                var = c("Variable", "n", "nNA", "pNA", "M", "Var", "Low", "Upp"),
                                sd = c("Variable", "n", "nNA", "pNA", "M", "SD", "Low", "Upp"))

          #......
          # Round
          print.object[[i]][, print.round] <- sapply(print.round, function(y) ifelse(!is.na(print.object[[i]][, y]),
                                                                                formatC(print.object[[i]][, y], digits = digits, format = "f",
                                                                                        zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

          #......
          # Percentages
          if (isTRUE(!x$ci %in% c("mean.diff.i", "mean.diff.p", "prop.diff.i", "prop.diff.p"))) {

            print.object[[i]][, "pNA"] <- round(print.object[[i]][, "pNA"], digits = 2L)
            print.object[[i]][, "pNA"] <- paste0(print.object[[i]][, "pNA"], "%")

          }

          #......
          # Col names
          print.object[[i]] <- rbind(print.names, print.object[[i]])

          #......
          # Format
          # Remove duplicated values
          print.object[[i]][duplicated(print.object[[i]]$variable) , "variable"] <- ""

          col.format <- which(colnames(print.object[[i]]) %in% c("variable", "between"))

          # Justify left and right
          print.object[[i]][, col.format] <- format(print.object[[i]][, col.format], justify = "left")
          print.object[[i]][, -col.format] <- apply(print.object[[i]][, -col.format], 2, format, justify = "right")

          # Add blank space
          print.object[[i]][, "variable"] <- c(paste0(" ", print.object[[i]][1L, "variable"]), paste0("  ", print.object[[i]][-1L, "variable"]))
          print.object[[i]][, "variable"] <- format(c(print.object[[i]][1L, "variable"], misty::chr.trim(print.object[[i]][-1L, "variable"], side = "right")), justify = "left")

          if (isTRUE(x$ci %in% c("mean.diff.i", "prop.diff.i"))) {

            print.object[[i]][, "between"] <- c(print.object[[i]][1L, "between"], paste0(" ", print.object[[i]][-1L, "between"]))
            print.object[[i]][, "between"] <- format(c(print.object[[i]][1L, "between"], misty::chr.trim(print.object[[i]][-1L, "between"], side = "right")), justify = "left")

            # NAs
            colnames.NA <- c("m.diff", "p.diff", "low", "upp")

            print.object[[i]][, which(colnames(print.object[[i]]) %in% colnames.NA)] <- apply(print.object[[i]][, which(colnames(print.object[[i]]) %in% colnames.NA), drop = FALSE], 2, function(y) gsub("NA", "  ", y))

          }

          # One Variable
          if (isTRUE(length(unique(x$result[[1]]$variable)) == 1L)) {

            print.object[[i]] <- print.object[[i]][, -1L]

            print.object[[i]][, "between"] <- c(paste0(" ", print.object[[i]][1L, "between"]), paste0(" ", print.object[[i]][-1L, "between"]))
            print.object[[i]][, "between"] <- format(c(print.object[[i]][1L, "between"], misty::chr.trim(print.object[[i]][-1L, "between"], side = "right")), justify = "left")

          }

        }

      #......
      # Grouping
      } else {

        for (i in names(print.object)) {

          #......
          # Print names
          print.names <- switch(x$ci,
                                mean = c("Group", "Variable", "n", "nNA", "pNA", "M", "SD", "Low", "Upp"),
                                mean.diff.i = c("Group", "Variable", "Between", "n", "nNA", "M", "SD", "M.Diff", "Low", "Upp"),
                                mean.diff.p = c("Group", "Variable", "n", "nNA", "M1", "SD1", "M2", "SD2", "M.Diff", "SD.Diff", "Low", "Upp"),
                                prop.diff.i = c("Group", "Variable", "Beween", "n", "nNA", "p", "p.Diff", "Low", "Upp"),
                                prop.diff.p = c("Group", "Variable", "n", "nNA", "p1", "p2", "p.Diff", "Low", "Upp"),
                                median = c("Group", "Variable", "n", "nNA", "pNA", "Med", "IQR", "Low", "Upp"),
                                prop = c("Group", "Variable", "n", "nNA", "pNA", "Prop", "Low", "Upp"),
                                var = c("Group", "Variable", "n", "nNA", "pNA", "M", "Var", "Low", "Upp"),
                                sd = c("Group", "Variable", "n", "nNA", "pNA", "M", "SD", "Low", "Upp"))

          #......
          # Sort by variables
          if (isTRUE(sort.var)) {

            print.object[[i]] <- print.object[[i]][order(print.object[[i]][, "variable"]), ]

          }

          #......
          # Round
          print.object[[i]][, print.round] <- sapply(print.round, function(y) ifelse(!is.na(print.object[[i]][, y]),
                                                                                formatC(print.object[[i]][, y], digits = digits, format = "f",
                                                                                        zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))
          #......
          # Percentages
          if (isTRUE(!x$ci %in% c("mean.diff.i", "mean.diff.p", "prop.diff.i", "prop.diff.p"))) {

            print.object[[i]][, "pNA"] <- round(print.object[[i]][, "pNA"], digits = 2)
            print.object[[i]][, "pNA"] <- paste0(print.object[[i]][, "pNA"], "%")

          }

          #......
          # Col names
          print.object[[i]] <- rbind(print.names, print.object[[i]])

          #......
          # Format

          # Remove duplicated values
          print.object[[i]][duplicated(paste(print.object[[i]]$group, print.object[[i]]$variable)) , c("group", "variable")] <- ""

          col.format <- which(colnames(print.object[[i]]) %in% c("group", "variable", "between"))

          # Justify left and right
          print.object[[i]][, col.format] <- format(print.object[[i]][, col.format], justify = "left")
          print.object[[i]][, -col.format] <- apply(print.object[[i]][, -col.format], 2, format, justify = "right")

          # Add blank space
          print.object[[i]][, "group"] <- c(paste0(" ", print.object[[i]][1L, "group"]), paste0("  ", print.object[[i]][-1L, "group"]))
          print.object[[i]][, "group"] <- format(c(print.object[[i]][1L, "group"], misty::chr.trim(print.object[[i]][-1L, "group"], side = "right")), justify = "left")

          print.object[[i]][, "variable"] <- c(print.object[[i]][1L, "variable"], paste0(" ", print.object[[i]][-1L, "variable"]))
          print.object[[i]][, "variable"] <- format(c(print.object[[i]][1L, "variable"], misty::chr.trim(print.object[[i]][-1L, "variable"], side = "right")), justify = "left")

          if (isTRUE(x$ci %in% c("mean.diff.i", "prop.diff.i"))) {

            print.object[[i]][, "between"] <- c(print.object[[i]][1L, "between"], paste0(" ", print.object[[i]][-1L, "between"]))
            print.object[[i]][, "between"] <- format(c(print.object[[i]][1L, "between"], misty::chr.trim(print.object[[i]][-1L, "between"], side = "right")), justify = "left")

            # NAs
            colnames.NA <- c("m.diff", "p.diff", "low", "upp")

            print.object[[i]][, which(colnames(print.object[[i]]) %in% colnames.NA)] <- apply(print.object[[i]][, which(colnames(print.object[[i]]) %in% colnames.NA), drop = FALSE], 2, function(y) gsub("NA", "  ", y))

          }

          # Only one variable
          if (isTRUE(length(unique(x$result[[1]]$variable)) == 1L)) {

            print.object[[i]] <- print.object[[i]][, -2L]

          }

        }

      }

      # Print object
      cat(paste(switch(x$ci,
                       mean = "Arithmetic Mean with",
                       mean.diff.i = "Difference in Means from Independent Samples with",
                       mean.diff.p = "Difference in Means from Dependent Samples with",
                       prop.diff.i = "Difference in Proportions from Independent Samples with",
                       prop.diff.p = "Difference in Proportions from Dependent Samples with",
                       median = "Median with",
                       prop = "Proportion with",
                       var = "Variance with",
                       sd = "Standard Deviation with"), switch(x$args$alternative,
                                                               two.sided = "Two-Sided",
                                                               less = "One-Sided",
                                                               greater = "One-Sided"),
                paste0(round(x$args$conf.level * 100L, digits = 2L), "%"), "CI\n\n"))

      for (i in names(print.object)) {

        cat(" Split Group:", i, "\n")

        write.table(print.object[[i]], quote = FALSE, row.names = FALSE, col.names = FALSE)

        if (isTRUE(i != names(print.object)[length(print.object)])) { cat("\n") }

      }

      #......
      # Note

      # Difference in arithmetic mean from independent samples
      if (isTRUE(x$ci == "mean.diff.i")) {

        if (isTRUE(x$args$var.equal)) {

          cat(paste0("\n Note. Equal population variance assumption\n"))

        }
      }

    }

  ####################################################################################
  #-----------------------------------------------------------------------------------
  # Cohen's d
  }, cohens.d = {

    #......
    # Variables to round
    print.round <- switch(x$sample,
                          one = c("m", "m.diff", "sd", "d", "se", "low", "upp"),
                          two = c("m", "m.diff", "sd", "d", "se", "low", "upp"),
                          paired = c("m1", "m2", "m.diff", "sd", "d", "se", "low", "upp"))

    ####################################################################################
    # Main Function

    #----------------------------------------
    # No grouping

    if (isTRUE(is.null(x$data$group) && is.null(x$data$split))) {

      #......
      # Print names
      print.names <- switch(x$sample,
                            one = c("Variable", "n", "nNA", "M", "M.Diff", "SD", "d", "SE", "Low", "Upp"),
                            two = c("Variable", "Between", "n", "nNA", "M", "M.Diff", "SD", "d", "SE", "Low", "Upp"),
                            paired = c("Variable", "n", "nNA", "M1", "M2", "M.Diff", "SD", "d", "SE", "Low", "Upp"))

      #......
      # Round
      print.object[, print.round] <- sapply(print.round, function(y) ifelse(!is.na(print.object[, y]),
                                                                            formatC(print.object[, y], digits = digits, format = "f",
                                                                                    zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

      #......
      # Col names
      print.object <- rbind(print.names, print.object)

      #......
      # Format
      # Remove duplicated values
      print.object[duplicated(print.object$variable) , "variable"] <- ""

      col.format <- which(colnames(print.object) %in% c("variable", "between"))

      # Justify left and right
      print.object[, col.format] <- format(print.object[, col.format], justify = "left")
      print.object[, -col.format] <- apply(print.object[, -col.format], 2, format, justify = "right")

      # Add blank space
      print.object[, "variable"] <- c(paste0(" ", print.object[1L, "variable"]), paste0("  ", print.object[-1L, "variable"]))
      print.object[, "variable"] <- format(c(print.object[1L, "variable"], misty::chr.trim(print.object[-1L, "variable"], side = "right")), justify = "left")

      if (isTRUE(x$sample == "two")) {

        print.object[, "between"] <- c(print.object[1L, "between"], paste0(" ", print.object[-1L, "between"]))
        print.object[, "between"] <- format(c(print.object[1L, "between"], misty::chr.trim(print.object[-1L, "between"], side = "right")), justify = "left")

        # NAs
        colnames.NA <- c("m.diff", "sd", "d", "se", "low", "upp")

        print.object[, which(colnames(print.object) %in% colnames.NA)] <- apply(print.object[, which(colnames(print.object) %in% colnames.NA), drop = FALSE], 2, function(y) gsub("NA", "  ", y))

      }

      # One Variable
      if (isTRUE(length(unique(x$result$variable)) == 1L)) {

        print.object <- print.object[, -1L]

        if (isTRUE(x$sample == "two")) {

          print.object[, "between"] <- c(paste0(" ", print.object[1L, "between"]), paste0(" ", print.object[-1L, "between"]))
          print.object[, "between"] <- format(c(print.object[1L, "between"], misty::chr.trim(print.object[-1L, "between"], side = "right")), justify = "left")

        } else {

          print.object[, "n"] <- c(paste0(" ", print.object[1L, "n"]), paste0(" ", print.object[-1L, "n"]))
          print.object[, "n"] <- format(c(print.object[1L, "n"], misty::chr.trim(print.object[-1L, "n"], side = "right")), justify = "right")

        }

      }

      #......
      # Print output
      cat(paste(switch(x$sample,
                       one = paste0("Cohen's d for One-Sample Design with \u03BC = ", round(x$args$mu, digits = 2), " and"),
                       two = "Cohen's d for Two-Sample Design with",
                       paired = "Cohen's d for Paired-Sample Design with"),
                       switch(x$args$alternative,
                              two.sided = "Two-Sided",
                              less = "One-Sided",
                              greater = "One-Sided"),
                paste0(round(x$args$conf.level * 100L, digits = 2L), "%"), "CI\n\n"))

      write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE)

    #----------------------------------------
    # Grouping
    } else if (isTRUE(!is.null(x$data$group) && is.null(x$data$split))) {

      #......
      # Print names
      print.names <- switch(x$sample,
                            one = c("Group", "Variable", "n", "nNA", "M", "M.Diff", "SD", "d", "SE", "Low", "Upp"),
                            two = c("Group", "Variable", "Between", "n", "nNA", "M", "M.Diff", "SD", "d", "SE", "Low", "Upp"),
                            paired = c("Group", "Variable", "n", "nNA", "M1", "M2", "M.Diff", "SD", "d", "SE", "Low", "Upp"))

      #......
      # Sort by variables
      if (isTRUE(sort.var)) {

        print.object <- print.object[order(print.object[, "variable"]), ]

      }

      #......
      # Round
      print.object[, print.round] <- sapply(print.round, function(y) ifelse(!is.na(print.object[, y]),
                                                                            formatC(print.object[, y], digits = digits, format = "f",
                                                                                    zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

      #......
      # Col names
      print.object <- rbind(print.names, print.object)

      #......
      # Format

      # Remove duplicated labels
      print.object[duplicated(paste(print.object$group, print.object$variable)) , c("group", "variable")] <- ""

      col.format <- which(colnames(print.object) %in% c("group", "variable", "between"))

      # Justify left and right
      print.object[, col.format] <- format(print.object[, col.format], justify = "left")
      print.object[, -col.format] <- apply(print.object[, -col.format], 2, format, justify = "right")

      # Add blank space
      print.object[, "group"] <- c(paste0(" ", print.object[1L, "group"]), paste0("  ", print.object[-1L, "group"]))
      print.object[, "group"] <- format(c(print.object[1L, "group"], misty::chr.trim(print.object[-1L, "group"], side = "right")), justify = "left")

      print.object[, "variable"] <- c(print.object[1L, "variable"], paste0(" ", print.object[-1L, "variable"]))
      print.object[, "variable"] <- format(c(print.object[1L, "variable"], misty::chr.trim(print.object[-1L, "variable"], side = "right")), justify = "left")

      if (isTRUE(x$sample == "two")) {

        print.object[, "between"] <- c(print.object[1L, "between"], paste0(" ", print.object[-1L, "between"]))
        print.object[, "between"] <- format(c(print.object[1L, "between"], misty::chr.trim(print.object[-1L, "between"], side = "right")), justify = "left")

        # NAs
        colnames.NA <- c("m.diff", "sd", "d", "se", "low", "upp")

        print.object[, which(colnames(print.object) %in% colnames.NA)] <- apply(print.object[, which(colnames(print.object) %in% colnames.NA), drop = FALSE], 2, function(y) gsub("NA", "  ", y))

      }

      # Only one variable
      if (isTRUE(length(unique(x$result$variable)) == 1L)) {

        print.object <- print.object[, -2L]

      }

      #......
      # Print output
      cat(paste(switch(x$sample,
                       one = paste0("Cohen's d for One-Sample Design with \u03BC = ", round(x$args$mu, digits = 2), " and"),
                       two = "Cohen's d for Two-Sample Design with",
                       paired = "Cohen's d for Paired-Sample Design with"),
                       switch(x$args$alternative,
                              two.sided = "Two-Sided",
                              less = "One-Sided",
                              greater = "One-Sided"),
                       paste0(round(x$args$conf.level * 100L, digits = 2L), "%"), "CI\n\n"))

      write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE)

    #----------------------------------------
    # Split
    } else if (isTRUE(!is.null(x$data$split))) {

      #......
      # No grouping
      if (isTRUE(is.null(x$data$group))) {

        for (i in names(print.object)) {

          #......
          # Print names
          print.names <- switch(x$sample,
                                one = c("Variable", "n", "nNA", "M", "M.Diff", "SD", "d", "SE", "Low", "Upp"),
                                two = c("Variable", "Between", "n", "nNA", "M", "M.Diff", "SD", "d", "SE", "Low", "Upp"),
                                paired = c("Variable", "n", "nNA", "M1", "M2", "M.Diff", "SD", "d", "SE", "Low", "Upp"))

          #......
          # Round
          print.object[[i]][, print.round] <- sapply(print.round, function(y) ifelse(!is.na(print.object[[i]][, y]),
                                                                                     formatC(print.object[[i]][, y], digits = digits, format = "f",
                                                                                             zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

          #......
          # Col names
          print.object[[i]] <- rbind(print.names, print.object[[i]])

          #......
          # Format
          # Remove duplicated values
          print.object[[i]][duplicated(print.object[[i]]$variable) , "variable"] <- ""

          col.format <- which(colnames(print.object[[i]]) %in% c("variable", "between"))

          # Justify left and right
          print.object[[i]][, col.format] <- format(print.object[[i]][, col.format], justify = "left")
          print.object[[i]][, -col.format] <- apply(print.object[[i]][, -col.format], 2, format, justify = "right")

          # Add blank space
          print.object[[i]][, "variable"] <- c(paste0(" ", print.object[[i]][1L, "variable"]), paste0("  ", print.object[[i]][-1L, "variable"]))
          print.object[[i]][, "variable"] <- format(c(print.object[[i]][1L, "variable"], misty::chr.trim(print.object[[i]][-1L, "variable"], side = "right")), justify = "left")

          if (isTRUE(x$sample == "two")) {

            print.object[[i]][, "between"] <- c(print.object[[i]][1L, "between"], paste0(" ", print.object[[i]][-1L, "between"]))
            print.object[[i]][, "between"] <- format(c(print.object[[i]][1L, "between"], misty::chr.trim(print.object[[i]][-1L, "between"], side = "right")), justify = "left")

            # NAs
            colnames.NA <- c("m.diff", "sd", "d", "se", "low", "upp")

            print.object[[i]][, which(colnames(print.object[[i]]) %in% colnames.NA)] <- apply(print.object[[i]][, which(colnames(print.object[[i]]) %in% colnames.NA), drop = FALSE], 2, function(y) gsub("NA", "  ", y))

          }

          # One Variable
          if (isTRUE(length(unique(x$result[[1]]$variable)) == 1L)) {

            print.object[[i]] <- print.object[[i]][, -1L]

            if (isTRUE(x$sample == "two")) {

              print.object[[i]][, "between"] <- c(paste0(" ", print.object[[i]][1L, "between"]), paste0(" ", print.object[[i]][-1L, "between"]))
              print.object[[i]][, "between"] <- format(c(print.object[[i]][1L, "between"], misty::chr.trim(print.object[[i]][-1L, "between"], side = "right")), justify = "left")

            }

          }

          print.object[[i]][, 1] <- paste0(" ", print.object[[i]][, 1])

        }

      #......
      # Grouping
      } else {

        for (i in names(print.object)) {

          #......
          # Print names
          print.names <- switch(x$sample,
                                one = c("Group", "Variable", "n", "nNA", "M", "M.Diff", "SD", "d", "SE", "Low", "Upp"),
                                two = c("Group", "Variable", "Between", "n", "nNA", "M", "M.Diff", "SD", "d", "SE", "Low", "Upp"),
                                paired = c("Group", "Variable", "n", "nNA", "M1", "M2", "M.Diff", "SD", "d", "SE", "Low", "Upp"))

          #......
          # Sort by variables
          if (isTRUE(sort.var)) {

            print.object[[i]] <- print.object[[i]][order(print.object[[i]][, "variable"]), ]

          }

          #......
          # Round
          print.object[[i]][, print.round] <- sapply(print.round, function(y) ifelse(!is.na(print.object[[i]][, y]),
                                                                                     formatC(print.object[[i]][, y], digits = digits, format = "f",
                                                                                             zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

          #......
          # Col names
          print.object[[i]] <- rbind(print.names, print.object[[i]])

          #......
          # Format

          # Remove duplicated values
          print.object[[i]][duplicated(paste(print.object[[i]]$group, print.object[[i]]$variable)) , c("group", "variable")] <- ""

          col.format <- which(colnames(print.object[[i]]) %in% c("group", "variable", "between"))

          # Justify left and right
          print.object[[i]][, col.format] <- format(print.object[[i]][, col.format], justify = "left")
          print.object[[i]][, -col.format] <- apply(print.object[[i]][, -col.format], 2, format, justify = "right")

          # Add blank space
          print.object[[i]][, "group"] <- c(paste0(" ", print.object[[i]][1L, "group"]), paste0("  ", print.object[[i]][-1L, "group"]))
          print.object[[i]][, "group"] <- format(c(print.object[[i]][1L, "group"], misty::chr.trim(print.object[[i]][-1L, "group"], side = "right")), justify = "left")

          print.object[[i]][, "variable"] <- c(print.object[[i]][1L, "variable"], paste0(" ", print.object[[i]][-1L, "variable"]))
          print.object[[i]][, "variable"] <- format(c(print.object[[i]][1L, "variable"], misty::chr.trim(print.object[[i]][-1L, "variable"], side = "right")), justify = "left")

          if (isTRUE(x$sample == "two")) {

            print.object[[i]][, "between"] <- c(print.object[[i]][1L, "between"], paste0(" ", print.object[[i]][-1L, "between"]))
            print.object[[i]][, "between"] <- format(c(print.object[[i]][1L, "between"], misty::chr.trim(print.object[[i]][-1L, "between"], side = "right")), justify = "left")

            # NAs
            colnames.NA <- c("m.diff", "sd", "d", "se", "low", "upp")

            print.object[[i]][, which(colnames(print.object[[i]]) %in% colnames.NA)] <- apply(print.object[[i]][, which(colnames(print.object[[i]]) %in% colnames.NA), drop = FALSE], 2, function(y) gsub("NA", "  ", y))

          }

          # Only one variable
          if (isTRUE(length(unique(x$result[[1]]$variable)) == 1L)) {

            print.object[[i]] <- print.object[[i]][, -2L]

          }

          print.object[[i]][, 1] <- paste0(" ", print.object[[i]][, 1])

        }

      }

      # Print object
      cat(paste(switch(x$sample,
                       one = paste0("Cohen's d for One-Sample Design with \u03BC = ", x$args$mu, " and"),
                       two = "Cohen's d for Two-Sample Design with",
                       paired = "Cohen's d for Paired-Sample Design with"),
                       switch(x$args$alternative,
                              two.sided = "Two-Sided",
                              less = "One-Sided",
                              greater = "One-Sided"),
                       paste0(round(x$args$conf.level * 100L, digits = 2L), "%"), "CI\n\n"))

      for (i in names(print.object)) {

        cat(" Split Group:", i, "\n")

        write.table(print.object[[i]], quote = FALSE, row.names = FALSE, col.names = FALSE)

        if (isTRUE(i != names(print.object)[length(print.object)])) { cat("\n") }

      }

    }

    #......
    # Notes

    # One-sample design
    if (isTRUE(x$sample == "one")) {

      if (isTRUE(x$args$correct)) {

        cat("\n Note. Applying small sample correction factor\n")

      }

    }

    # Two-sample design
    else if (isTRUE(x$sample == "two")) {

      if (isTRUE(is.null(x$args$ref))) {

        if (isTRUE(x$args$weighted)) {

          if (isTRUE(x$args$correct)) {

            cat("\n Note. SD = weighted pooled standard deviation \n       Applying small sample correction factor")

          } else {

            cat("\n Note. SD = weighted pooled standard deviation\n")

          }

        } else {

          if (isTRUE(x$args$correct)) {

            cat("\n Note. SD = unweighted pooled standard deviation \n       Applying small sample correction factor")

          } else {

            cat("\n Note. SD = unweighted pooled standard deviation\n")

          }

        }

      } else {

        if (isTRUE(x$args$correct)) {

          cat(paste0("\n Note. SD = standard deviation of the reference group: ", x$args$ref), "\n       Applying small sample correction factor")

        } else {

          cat(paste0("\n Note. SD = standard deviation of the reference group: ", x$args$ref, "\n"))

        }

      }

    # Paired
    } else if (isTRUE(x$sample == "paired")) {

      if (isTRUE(x$args$weighted)) {

        if (isTRUE(x$args$correct)) {

          cat("\n Note. SD = standard deviation of the difference scores\n       Applying small sample correction factor")

        } else {

          cat("\n Note. SD = standard deviation of the difference scores\n")

        }

      } else {

        if (isTRUE(x$args$cor)) {

          if (isTRUE(x$args$correct)) {

            cat("\n Note. SD = controlling for the correlation between measures \n       Applying small sample correction factor")

          } else {

            cat("\n Note. SD = controlling for the correlation between measures\n")

          }

        } else {

          if (isTRUE(x$args$correct)) {

            cat("\n Note. SD = without controlling for the correlation between measures \n       Applying small sample correction factor")

          } else {

            cat("\n Note. SD = without controlling for the correlation between measures\n")

          }

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
      if (isTRUE(any(!print %in% c("all", "vif", "eigen")))) {

        stop("Character string(s) in the argument 'print' does not match with \"all\", \"vif\", \"eigen\".",
             call. = FALSE)

      }

    }

    #----------------------------------------
    # Arguments

    # Print variance inflation factor and/or eigenvalue
    if (isTRUE(length(print) == 1L && "all" %in% print)) { print <- c("vif", "eigen") }

    #-----------------------------------------------------------------------------------
    # Main Function

    cat(" Collinearity Diagnostics\n")

    #-----------------------------------------
    # Tolerance, std. error inflation factor, and variance inflation factor
    if (isTRUE("vif" %in% print)) {

      # Exclude variables "df" and "GVIF"
      print.object$coef <- print.object$coef[, which(!colnames(print.object$coef) %in% c("df", "GVIF"))]

      # Round
      if (isTRUE(any(class(x$model) == "lmerMod"))) {

        print.object$coef <- apply(print.object$coef, 2L, function(y) formatC(y, digits = digits, format = "f",
                                                                              zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")))

      } else if (isTRUE(any(class(x$model) == "lme"))) {

        print.object$coef[, -5] <- apply(print.object$coef[, -5], 2L, function(y) formatC(y, digits = digits, format = "f",
                                                                                          zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")))

        print.object$coef[, 5] <- formatC(print.object$coef[, 5], digits = p.digits, format = "f",
                                          zero.print = ifelse(p.digits > 0, paste0("0.", paste(rep(0, times = p.digits), collapse = "")), "0"))

      } else {

        print.object$coef[, -4] <- apply(print.object$coef[, -4], 2L, function(y) formatC(y, digits = digits, format = "f",
                                                                                          zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")))

        print.object$coef[, 4] <- formatC(print.object$coef[, 4], digits = p.digits, format = "f",
                                          zero.print = ifelse(p.digits > 0, paste0("0.", paste(rep(0, times = p.digits), collapse = "")), "0"))

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
      if (isTRUE(any(x$result$vif$df > 1))) {

        cat("\n   Note. Generalized SIF/VIF are computed for terms with more than 1 df\n")

      }

    }

    #-----------------------------------------
    # Eigenvalue, condition index, and variance proportions
    if (isTRUE("eigen" %in% print)) {

      # Round
      print.object$eigen[, -1L] <- apply(print.object$eigen[, -1L], 2L, function(y) formatC(y, digits = digits, format = "f",
                                                                                            zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")))

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
  }, cor.cont = {

    ####################################################################################
    # Data and Arguments

    # Print triangular
    tri <- ifelse(all(c("both", "lower", "upper") %in% tri), "lower", tri)

    ####################################################################################
    # Main Function

    #----------------------------------------
    # Two variables
    if (isTRUE(is.null(dim(print.object)))) {

      print.object <- cbind("  Estimate: ", ifelse(!is.na(print.object), formatC(print.object, digits = digits, format = "f",
                                                                                 zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), print.object))

    #----------------------------------------
    # More than two variables
    } else {

      # Format contingency coefficients
      print.object <- formatC(print.object, digits = digits, format = "f",
                              zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0"))

      # Lower and/or upper triangular
      if (isTRUE(tri == "lower")) {

        print.object[upper.tri(print.object)] <- ""

      }

      if (isTRUE(tri == "upper")) {

        print.object[lower.tri(print.object)] <- ""

      }

      # Set diagonal to "
      diag(print.object) <- ""

      # Format
      row.names(print.object) <- paste("", row.names(print.object))

    }

    ####################################################################################
    # Output

    if (isTRUE(is.null(dim(x$result)))) {

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

    if (isTRUE(is.null(dim(x$result)))) {

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
      if (isTRUE(any(!print %in% c("all", "cor", "n", "p")))) {

        stop("Character string(s) in the argument 'print' does not match with \"all\", \"cor\", \"n\", or \"p\".",
             call. = FALSE)

      }

    }

    ####################################################################################
    # Arguments

    #......
    # Print correlation, sample size or significance values
    if (isTRUE(all(c("all", "cor", "n", "p") %in% print))) { print <- "cor" }

    if (isTRUE(length(print) == 1L && "all" %in% print)) { print <- c("cor", "n", "p") }

    #......
    # Print triangular
    tri <- ifelse(all(c("both", "lower", "upper") %in% tri), "lower", tri)

    ####################################################################################
    # Main Function

    #........................................
    # Round and format

    print.object$cor <- formatC(print.object$cor, digits = digits, format = "f",
                                zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0"))
    print.object$p <- formatC(print.object$p, digits = p.digits, format = "f",
                              zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0"))
    print.object$n <- formatC(print.object$n, zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0"))

    diag(print.object$cor) <- ""
    diag(print.object$p) <- ""
    diag(print.object$n) <- ""

    #........................................
    # Lower and/or upper triangular

    if (isTRUE(is.null(x$args$group))) {

      if (isTRUE(tri == "lower")) {

        print.object$cor[upper.tri(print.object$cor)] <- ""
        print.object$n[upper.tri(print.object$n)] <- ""
        print.object$p[upper.tri(print.object$p)] <- ""

      }

      if (isTRUE(tri == "upper")) {

        print.object$cor[lower.tri(print.object$cor)] <- ""
        print.object$n[lower.tri(print.object$n)] <- ""
        print.object$p[lower.tri(print.object$p)] <- ""

      }

    }

    #........................................
    # Row names

    if (isTRUE(!is.null(row.names(print.object$cor)))) {

      # Rownames
      row.names(print.object$cor) <- paste0("  ", row.names(print.object$cor))
      row.names(print.object$n) <- paste0("  ", row.names(print.object$n))
      row.names(print.object$p) <- paste0("  ", row.names(print.object$p))

    }

    print.object$cor <- apply(print.object$cor, 2, function(y) format(y, justify = "right"))
    print.object$n <- apply(print.object$n, 2, function(y) format(y, justify = "right"))
    print.object$p <- apply(print.object$p, 2, function(y) format(y, justify = "right"))

    #------------------------------------
    # Print

    #........................
    # Correlation coefficient
    if (isTRUE("cor" %in% print)) {

      if (isTRUE(x$args$method == "pearson")) {

        cat(" Pearson Product-Moment Correlation Coefficient\n\n")

      }

      if (isTRUE(x$args$method == "spearman")) {

        cat(" Spearman's Rank-Order Correlation Coefficient\n\n")

      }

      if (isTRUE(x$args$method == "kendall-b")) {

        cat(" Kendall's Tau-b Correlation Coefficient\n\n")

      }

      if (isTRUE(x$args$method == "kendall-c")) {

        cat(" Kendall-Stuart's Tau-c Correlation Coefficient\n\n")

      }

      print(print.object$cor, quote = FALSE, right = TRUE, max = 99999)

    }

    #........................
    # Sample size
    if (isTRUE("n" %in% print)) {

      if (isTRUE("cor" %in% print)) { cat("\n") }

      # Pairwise deletion
      if (!isTRUE(x$args$na.omit)) {

        cat(" Sample Size Using Pairwise Deletion\n\n")
        print(print.object$n, quote = FALSE, right = TRUE, max = 99999)

      # Listwise deletion
      } else {

        if (isTRUE(is.null(x$args$group))) {

          cat(paste(" Sample Size Using Listwise Deletion\n  n =", nrow(na.omit(x$data)), "\n"))

        } else {

          cat(paste(" Sample Size Using Listwise Deletion\n  n in group 1 =", nrow(na.omit(x$data$group1)),
                    "\n  n in group 2 =", nrow(na.omit(x$data$group2)),"\n"))

        }

      }

    }

    #........................
    # p.values
    if (isTRUE("p" %in% print)) {

      if (isTRUE(any(c("cor", "n") %in% print))) { cat("\n") }

      cat(" Significance Value (p-value)\n\n")
      print(print.object$p, quote = FALSE, right = TRUE, max = 99999L)
      cat(paste0("\n  Adjustment for multiple testing: ", x$args$p.adj, "\n"))

    }

    #........................
    # Grouping
    if (isTRUE(!is.null(x$args$group))) {

      cat(paste0("\n Note. Lower triangular: Results for group = ", sort(unique(x$args$group))[1],
                 "\n       Upper triangular: Results for group = ", sort(unique(x$args$group))[2]), "\n")

    }

  ####################################################################################
  #-----------------------------------------------------------------------------------
  # Cramer's V
  }, cor.cramer = {

    #-----------------------------------------------------------------------------------
    # Data and Arguments

    # Print triangular
    tri <- ifelse(all(c("both", "lower", "upper") %in% tri), "lower", tri)

    #-----------------------------------------------------------------------------------
    # Main Function

    #........................................
    # Two variables
    if (isTRUE(is.null(dim(print.object)))) {

      print.object <- cbind("  Estimate: ", formatC(print.object, digits = digits, format = "f",
                                                    zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")))

    #........................................
    # More than two variables
    } else {

      # Format contingency coefficients
      print.object <- formatC(print.object, digits = digits, format = "f", zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0"))

      # Lower and/or upper triangular
      if (isTRUE(tri == "lower")) {

        print.object[upper.tri(print.object)] <- ""

      }

      if (isTRUE(tri == "upper")) {

        print.object[lower.tri(print.object)] <- ""

      }

      # Diagonal
      diag(print.object) <- ""

      # Format
      row.names(print.object) <- paste("", row.names(print.object))

    }

    #-----------------------------------------------------------------------------------
    # Output

    if (isTRUE(is.null(dim(x$result)))) {

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

    if (isTRUE(is.null(dim(x$result)))) {

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
      if (isTRUE(any(!print %in% c("no", "all", "row", "col", "total")))) {

        stop("Character string(s) in the argument 'print' does not match with \"no\", \"all\", \"row\", \"col\" or \"total\".",
             call. = FALSE)

      }

    }

    ####################################################################################
    # Main Function

    #......
    # Percentages
    no.perc <- if (isTRUE(any("no" %in% print))) {

      no.perc <- NULL

    } else {

      no.perc <- c("row", "col", "total")[!c("row", "col", "total") %in% print]

    }

    #......
    # Print object
    print.object <- x$result

    #----------------------------------------
    # Two-Dimensional Matrix

    if (isTRUE(ncol(x$data) == 2L)) {

      restab <- cbind(rep(names(print.object$freq.a[, 1L]), times = 4L),
                      rep(c("Freq", "Row %", "Col %", "Tot %"), each = nrow(print.object$freq.a)),
                      rbind(print.object$freq.a, print.object$perc.r, print.object$perc.c, print.object$perc.t),
                      c(apply(print.object$freq.a, 1, sum), rep("", times = 3*nrow(print.object$freq.a))))

      #......
      # Sort table

      # First variable is a factor
      if (isTRUE(is.factor(x$data[, 1L]))) {

        # Sort with NA
        if (isTRUE(any(is.na(x$data)) && !isTRUE(x$args$na.omit))) {

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
      if (isTRUE(any(print == "no"))) {

        restab <- restab[-grep(" %", restab[, 2L]), -2L]

      } else {

        # No total percentages
        if (isTRUE("total" %in% no.perc)) {

          restab <- restab[-grep("Tot %", restab[, 2]), ]

        }

        # No row percentages
        if (isTRUE("row" %in% no.perc)) {

          restab <- restab[-grep("Row %", restab[, 2L]), ]

        }

        # No col percentages
        if (isTRUE("col" %in% no.perc)) {

          restab <- restab[-grep("Col %", restab[, 2L]), ]

        }

      }

      #......
      # Format

      # Frequencies only
      if (isTRUE(any(print == "no"))) {

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
        restab[grep("%", restab[, 2L]), -c(1L:2L, ncol(restab))] <- apply(restab[grep("%", restab[, 2L]), -c(1L:2L, ncol(restab))], 2, function(y) paste0(format(formatC(as.numeric(y), digits = digits, format = "f",
                                                                                                                                                                         zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), justify = "right"), "%"))

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

        cat("Cross Tabulation\n\n")

        # Print results
        write.table(restab, col.names = FALSE, row.names = FALSE, quote = FALSE)

      # Output table split
      } else {

        cat("Cross Tabulation\n\n")

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
        if (isTRUE("row" %in% print)) {

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
        if (isTRUE("col" %in% print)) {

          if (isTRUE(freq) || isTRUE("row" %in% print)) { cat("\n") }

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
        if (isTRUE("total" %in% print)) {

          if (isTRUE(freq) || isTRUE("row" %in% print) || isTRUE("col" %in% print)) { cat("\n") }

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

    if (isTRUE(ncol(x$data) == 3L)) {

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
      if (isTRUE(is.factor(x$data[, 1L]) && is.factor(x$data[, 2L]))) {

        # Sort with NA
        if (isTRUE(any(is.na(x$data)) && !isTRUE(x$args$na.omit))) {

          restab <-restab[order(factor(restab[, 1L], levels = c(levels(x$data[, 1L]), "NA"), labels = c(levels(x$data[, 1L]), "NA")),
                                factor(restab[, 2L], levels = c(levels(x$data[, 2L]), "NA"), labels = c(levels(x$data[, 2L]), "NA"))), ]

          # Sort without NA
        } else {

          restab <- restab[order(factor(restab[, 1L], levels = levels(x$data[, 1L]), labels = levels(x$data[, 1L])),
                                 factor(restab[, 2L], levels = levels(x$data[, 2L]), labels = levels(x$data[, 2L]))), ]

        }

      }

      # First variable is a factor, second variable is not a factor
      if (isTRUE(is.factor(x$data[, 1L]) && !is.factor(x$data[, 2L]))) {

        # Sort with NA
        if (isTRUE(any(is.na(x$data)) && !isTRUE(x$args$na.omit))) {

          restab <- restab[order(factor(restab[, 1L], levels = c(levels(x$data[, 1L]), "NA"), labels = c(levels(x$data[, 1L]), "NA")),
                                 restab[, 2L]), ]

        # Sort without NA
        } else {

          restab <- restab[order(factor(restab[, 1L], levels = levels(x$data[, 1L]), labels = levels(x$data[, 1L])),
                                 restab[, 2L]), ]

        }

      }

      # First variable is not a factor, second variable is a factor
      if (isTRUE(!is.factor(x$data[, 1L]) && is.factor(x$data[, 2L]))) {

        # Sort with NA
        if (isTRUE(any(is.na(x$data)) && !isTRUE(x$args$na.omit))) {

          restab <- restab[order(restab[, 1L],
                                 factor(restab[, 2L], levels = c(levels(x$data[, 2L]), "NA"), labels = c(levels(x$data[, 2L]), "NA"))), ]

        # Sort without NA
        } else {

          restab <- restab[order(restab[, 1L],
                                 factor(restab[, 2L], levels = levels(x$data[, 2L]), labels = levels(x$data[, 2L]))), ]

        }

      }

      # First and second variable are not a factor
      if (isTRUE(!is.factor(x$data[, 1L]) && !is.factor(x$data[, 2L]))) {

        # Sort with NA
        if (isTRUE(any(is.na(x$data)) && !isTRUE(x$args$na.omit))) {

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
        if (isTRUE("total" %in% no.perc)) {

          restab <- restab[-grep("Tot %", restab[, 3L]), ]

        }

        # No row percentages
        if (isTRUE("row" %in% no.perc)) {

          restab <- restab[-grep("Row %", restab[, 3L]), ]

        }

        # No col percentages
        if (isTRUE("col" %in% no.perc)) {

          restab <- restab[-grep("Col %", restab[, 3L]), ]

        }

      }

      #......
      # Format

      if (isTRUE(any(print == "no"))) {

        #......
        # Variable names
        restab <- rbind(c(rep("", times = 2L), colnames(x$data)[3L], rep("", times = (ncol(restab) - 3L))),
                        c(colnames(x$dat)[1L], colnames(x$data)[2L], row.names(print.object$freq.a[[1L]]), "Total"),
                        restab,
                        c("Total", "", apply(freq.a.print, 2, sum), sum(freq.a.print)))

        # Justify right
        restab[-1L, ] <- apply(restab[-1L, ], 2, function(y) format(y, justify = "right"))

        # First variable
        if (isTRUE(nchar(colnames(x$data)[1]) < max(nchar(restab[, 1L])))) {

          restab[2L, 1L] <- paste(sub("^\\s+", "", restab[2L, 1L]), paste(rep(" ", times = max(nchar(restab[, 1L])) - nchar(colnames(x$data)[1L]) - 1L) , collapse = ""))

        }

        if (isTRUE(nchar(colnames(x$data)[1L]) == max(nchar(restab[, 1L])))) {

          restab[2L, 1L] <- paste(sub("^\\s+", "", restab[2L, 1L]), paste(rep(" ", times = max(nchar(restab[, 1L])) - nchar(colnames(x$data)[1L])) , collapse = ""))

        }

        # Second variable
        if (isTRUE(nchar(colnames(x$data)[2L]) < max(nchar(restab[, 2L])))) {

          restab[2L, 2L] <- paste(sub("^\\s+", "", restab[2L, 2L]), paste(rep(" ", times = max(nchar(restab[, 2L])) - nchar(colnames(x$data)[2L]) - 1L), collapse = ""))

        }

        if (isTRUE(nchar(colnames(x$data)[2L]) == max(nchar(restab[, 2L])))) {

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
        restab[grep("%", restab[, 3L]), -c(1:3, ncol(restab))] <- apply(restab[grep("%", restab[, 3L]), -c(1L:3L, ncol(restab))], 2, function(y) paste0(format(formatC(as.numeric(y), digits = digits, format = "f",
                                                                                                                                                                       zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), justify = "right"), "%"))

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
        if (isTRUE("row" %in% print)) {

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
        if (isTRUE("col" %in% print)) {

          if (isTRUE(freq) || isTRUE("row" %in% print)) { cat("\n") }

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
        if (isTRUE("total" %in% print)) {

          if (isTRUE(freq) || isTRUE("row" %in% print) || isTRUE("col" %in% print)) { cat("\n") }

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
      if (isTRUE(!all(print %in%  c("all", "n", "nNA", "pNA", "m", "se.m", "var", "sd", "min", "p25", "med", "p75", "max", "skew",  "range", "iqr", "kurt")))) {

        stop("Character strings in the argument 'print' do not all match with \"all\", \"n\", \"nNA\", \"pNA\", \"m\", \"se.m\", \"var\", \"sd\", \"min\", \"p25\", \"med\", \"p75\", \"max\", \"range\", \"iqr\", \"skew\", or \"kurt\".",
             call. = FALSE)

      }

    }

    ####################################################################################
    # Arguments

    if (isTRUE(length(print) == 1L && print == "all")) {

      print <- c("n", "nNA", "pNA", "m", "se.m", "var", "sd", "min", "p25", "med", "p75", "max", "range", "iqr", "skew", "kurt")

    }

    ####################################################################################
    # Main Function

    #......
    # Variables to round
    print.round <- c("pNA", "m", "se.m", "var", "sd", "min", "p25", "med", "p75", "max", "range", "iqr", "skew", "kurt")

    #----------------------------------------
    # No Grouping, No Split

    if (isTRUE(is.null(x$data$group) && is.null(x$data$split))) {

      #......
      # Round
      print.object[, print.round] <- sapply(print.round, function(y) ifelse(!is.na(print.object[, y]), formatC(print.object[, y], digits = digits, format = "f",
                                                                                                               zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

      #......
      # Percentages
      print.object[, "pNA"] <- paste0(print.object[, "pNA"], "%")

      #......
      # Row names
      print.object <- rbind(c("Variable", "n", "nNA", "pNA", "M", "SE.M", "Var", "SD", "Min", "p25", "Med", "p75", "Max", "Range", "IQR", "Skew", "Kurt"), print.object)

      #......
      # Select statistical measures and add variable names
      print.object <- data.frame(variable = print.object[, "variable"], print.object[, print, drop = FALSE], stringsAsFactors = FALSE,
                                 check.names = FALSE)

      #......
      # Format

      # Justify left and right
      print.object[, 1L] <- format(print.object[, 1L, drop = FALSE], justify = "left")
      print.object[, -1L] <- apply(print.object[, -1L, drop = FALSE], 2, function(y) format(y, justify = "right"))

      # Add blank space
      print.object[, "variable"] <- c(paste0(" ", print.object[1L, "variable"], collapse = ""), paste0("  ", print.object[-1L, "variable"]))
      print.object[, "variable"] <- format(c(print.object[1L, "variable"], misty::chr.trim(print.object[-1L, "variable"], side = "right")), justify = "left")

      if (isTRUE(ncol(x$data$x) == 1L)) {

        print.object <- print.object[, -1L]

        print.object[, 1L] <- paste0(" ", print.object[, 1L])

      }

      cat("Descriptive Statistics\n\n")

      write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE)

    #----------------------------------------
    # Grouping, No Split
    } else if (isTRUE(!is.null(x$data$group) && is.null(x$data$split))) {

      # Sort by variables
      if (ncol(x$data$x) > 1L && isTRUE(sort.var)) {

        print.object <- print.object[order(print.object[, "variable"]), ]

        # Remove duplicated labels
        print.object[duplicated(print.object$variable) , "variable"] <- ""

      } else {

        # Remove duplicated labels
        print.object[duplicated(print.object$group) , "group"] <- ""

      }

      # Round
      print.object[, print.round] <- sapply(print.round, function(y) ifelse(!is.na(print.object[, y]),
                                                                            formatC(print.object[, y], digits = digits, format = "f",
                                                                                    zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

      # Percentages
      print.object[, "pNA"] <- paste0(print.object[, "pNA"], "%")

      # Col names
      print.object <- rbind(c("Group", "Variable", "n", "nNA", "pNA", "M", "SE M", "Var", "SD", "Min", "p25", "Med", "p75", "Max", "Range", "IQR", "Skew", "Kurt"),
                            print.object)

      # Select statistical measures and add variable names
      print.object <- data.frame(print.object[, c("group", "variable")], print.object[, -c(1, 2)][, print, drop = FALSE], stringsAsFactors = FALSE)

      # Format
      # Justify left and right
      print.object[, c("group", "variable")] <- format(print.object[, c("group", "variable")], justify = "left")
      print.object[, -c(1L, 2L)] <- format(print.object[, -c(1L, 2L)], justify = "right")

      # Add blank space
      print.object[, "group"] <- c(paste0(" ", print.object[1L, "group"], collapse = ""), paste0("  ", print.object[-1L, "group"]))
      print.object[, "group"] <- format(c(print.object[1L, "group"], misty::chr.trim(print.object[-1L, "group"], side = "right")), justify = "left")

      print.object[, "variable"] <- c(print.object[1L, "variable"], paste0(" ", print.object[-1L, "variable"]))
      print.object[, "variable"] <- format(c(print.object[1L, "variable"], misty::chr.trim(print.object[-1L, "variable"], side = "right")), justify = "left")

      if (isTRUE(ncol(x$data$x) == 1L)) {

        print.object <- print.object[, -2L]

        print.object[, 1L] <- paste0(" ", print.object[, 1L])

      }

      cat("Descriptive Statistics\n\n")

      # Print Output
      write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE)

    #----------------------------------------
    # Split, without or with Grouping
    } else if (isTRUE(!is.null(x$data$split))) {

      # Format
      for (i in names(print.object)) {

        # Round
        print.object[[i]][, print.round] <- sapply(print.round, function(y) ifelse(!is.na(print.object[[i]][, y]), formatC(print.object[[i]][, y], digits = digits, format = "f",
                                                                                                                           zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

        # Percentages
        print.object[[i]][, "pNA"] <- paste0(print.object[[i]][, "pNA"], "%")

        #......
        # No grouping
        if (isTRUE(is.null(x$data$group))) {

          # Col names
          print.object[[i]] <- rbind(c("Variable", "n", "nNA", "pNA", "M", "SE M", "Var", "SD", "Min", "p25", "Med", "p75", "Max", "Range", "IQR", "Skew", "Kurt"), print.object[[i]])

          # Select statistical measures and add variable names
          print.object[[i]] <- data.frame(variable = print.object[[i]][, "variable"], print.object[[i]][, print, drop = FALSE], stringsAsFactors = FALSE)

          print.object[[i]][, "variable"] <- format(print.object[[i]][, "variable"], justify = "left")
          print.object[[i]][, -1L] <- format(print.object[[i]][, -1L], justify = "right")

          # Add blank space
          print.object[[i]][, "variable"] <- c(paste0(" ", print.object[[i]][1L, "variable"], collapse = ""), paste0("  ", print.object[[i]][-1L, "variable"]))
          print.object[[i]][, "variable"] <- format(c(print.object[[i]][1L, "variable"], misty::chr.trim(print.object[[i]][-1L, "variable"], side = "right")), justify = "left")

        #......
        # Grouping
        } else {

          # Sort by variables
          if (isTRUE(ncol(x$data$x) > 1L && isTRUE(sort.var))) {

            print.object[[i]] <- print.object[[i]][order(print.object[[i]][, "variable"]), ]

            # Remove duplicated labels
            print.object[[i]][duplicated(print.object[[i]]$variable) , "variable"] <- ""

          } else {

            # Remove duplicated labels
            print.object[[i]][duplicated(print.object[[i]]$group) , "group"] <- ""

          }

          # Col names
          print.object[[i]] <- rbind(c("Group", "Variable", "n", "nNA", "pNA", "M", "SE.M", "Var", "SD", "Min", "p25", "Med", "p75", "Max", "Range", "IQR", "Skew", "Kurt"), print.object[[i]])

          # Select statistical measures and add variable names
          print.object[[i]] <- data.frame(variable = print.object[[i]][, "variable"],
                                          group = print.object[[i]][, "group"],
                                          print.object[[i]][, print, drop = FALSE], stringsAsFactors = FALSE)

          # Justify left and right
          print.object[[i]][, c("variable", "group")] <- format(print.object[[i]][, c("variable", "group")], justify = "left")
          print.object[[i]][, -c(1L, 2L)] <- format(print.object[[i]][, -c(1L, 2L)], justify = "right")

          # Add blank space
          print.object[[i]][, "variable"] <- c(paste0(" ", print.object[[i]][1L, "variable"], collapse = ""), paste0("  ", print.object[[i]][-1L, "variable"]))
          print.object[[i]][, "variable"] <- format(c(print.object[[i]][1L, "variable"], misty::chr.trim(print.object[[i]][-1L, "variable"], side = "right")), justify = "left")

          print.object[[i]][, "group"] <- c(print.object[[i]][1L, "group"], paste0(" ", print.object[[i]][-1L, "group"]))
          print.object[[i]][, "group"] <- format(c(print.object[[i]][1L, "group"], misty::chr.trim(print.object[[i]][-1L, "group"], side = "right")), justify = "left")

        }

        # Only one variable
        if (isTRUE(ncol(x$data$x) == 1L)) {

          print.object[[i]] <- print.object[[i]][, -grep("variable", colnames(print.object[[i]]))]

          print.object[[i]][, 1L] <- paste0(" ", print.object[[i]][, 1L])

        }

      }

      # Print object
      cat("Descriptive Statistics\n\n")

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

    if (isTRUE(print.object.nrow && print.object.ncol)) {

      # Print object
      print.object <- cbind("  Estimate  ", formatC(print.object, digits = digits, format = "f",
                                                    zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")))

    } else {

      #-----------------------------------------
      # More than one dependent variable, more than one independent variable

      if (isTRUE(!print.object.nrow && !print.object.ncol)) {

        # Variable names and format digits
        print.object <- rbind(c("", "Outcome", rep("", times = ncol(print.object) - 1L)),
                              c("Group", colnames(print.object)),
                              cbind(rownames(print.object),
                                    formatC(print.object, digits = digits, format = "f",
                                            zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0"))))

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

      if (isTRUE(print.object.nrow && !print.object.ncol)) {

        # Variable names and format digis
        print.object <- rbind(colnames(print.object),
                              formatC(print.object, digits = digits, format = "f",
                                      zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")))

        # Format
        print.object <- format(print.object, justify = "right")

      }

      #-----------------------------------------
      # One dependent variable, more than one independent variable

      if (isTRUE(!print.object.nrow && print.object.ncol)) {

        # Variable names and format digis
        print.object <- cbind(rownames(print.object),
                              formatC(print.object, digits = digits, format = "f",
                                      zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")))

        # Format
        print.object <- format(print.object, justify = "right")

      }

    }

    ####################################################################################
    # Output

    if (isTRUE(print.object.nrow && print.object.ncol)) {

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
      if (isTRUE(any(!print %in% c("no", "all", "perc", "v.perc")))) {

        stop("Character string in the argument 'print' does not match with \"no\", \"all\", \"perc\", or \"v.perc\".",
             call. = FALSE)

      }

    }

    ####################################################################################
    # Main function

    #-----------------------------------------
    # One variable

    if (isTRUE(ncol(as.data.frame(x$data, stringsAsFactors = FALSE)) == 1L || (x$args$split && ncol(x$data) == 1L))) {

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
        print.object[, c("Perc", "V.Perc")] <- suppressWarnings(apply(print.object[, c("Perc", "V.Perc")], 2, function(y) paste0(formatC(as.numeric(y), digits = digits, format = "f",
                                                                                                                                         zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), "%")))

        # Remove NA
        print.object[, "V.Perc"] <- gsub("NA%", "  ", print.object[, "V.Perc"])

        # Format
        print.object[, 1L:2L] <- apply(print.object[, 1L:2L], 2, function(y) format(y, justify = "left"))

        print.object[, -c(1L:2L)] <- apply(print.object[, -(1L:2L)], 2, function(y) format(y, justify = "right"))

        #......
        # Omit Total row if there are no missing values
        if (isTRUE(all(!is.na(x$data)))) {

          # Object without Total column
          print.object <- print.object[-grep("Total", print.object$x),  ]

          # Object without valid percentage
          print.object <- print.object[, -grep("V.Perc", colnames(print.object))]

        }

        #......
        # Omit Missing and Total row if print = "v.valid" and freq = FALSE
        if (isTRUE(length(print) == 1L && print == "v.perc" && !isTRUE(freq))) {

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
        if (isTRUE(!"perc" %in% print || "no" %in% print)) {

          # Object without percentage
          print.object <- print.object[, -which(colnames(print.object) == "Perc")]

        }

        ###
        # Omit valid percentages if !"v.perc" %in% print
        if (isTRUE("V.Perc" %in% colnames(print.object))) {

          if (isTRUE(!"v.perc" %in% print || "no" %in% print)) {

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
        print.object[2L, ] <- paste0(formatC(as.numeric(print.object[2L, ]), digits = digits, format = "f",
                                             zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), "%")
        print.object[3L, ] <- paste0(formatC(as.numeric(print.object[3L, ]), digits = digits, format = "f",
                                             zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), "%")

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
        if (isTRUE(all(!is.na(x$data)))) {

          # Object without Total column
          print.object <- print.object[, -ncol(print.object)]

          # Object without valid percentage
          print.object <- print.object[-grep("V.Perc", print.object[, 1L]), ]

        }

        #......
        # Omit Missing and Total row if perc = "v.valid" and freq = FALSE
        if (isTRUE(length(print) == 1L && print == "v.perc" && !isTRUE(freq))) {

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
        if (isTRUE(!"perc" %in% print || "no" %in% print)) {

          # Object without percentage
          print.object <- print.object[-which(row.names(print.object) == "Perc"), ]

        }

        ###
        # Omit valid percentages if !"v.perc" %in% print
        if (isTRUE("V.Perc" %in% row.names(print.object))) {

          if (isTRUE(!"v.perc" %in% print || "no" %in% print)) {

            # Object without valid percentage
            print.object <- print.object[-which(row.names(print.object) == "V.Perc"), ]

          }

        }

      }

      ####################################################################################
      # Output

      if (!isTRUE(split)) {

        cat("Frequency Table\n")

      }

      print(print.object, row.names = FALSE, max = 99999L)

    }

    #-----------------------------------------
    # More than one variable

    if (isTRUE(ncol(as.data.frame(x$data, stringsAsFactors = FALSE)) > 1L)) {

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
              print.object$freq$val <- format(misty::chr.trim(print.object$freq$val), justify = "left")

            }

            # Column names
            colnames(print.object$freq)[1L:2L] <- c("", "")

          }

          #....
          # Percentages
          if (isTRUE(all(print != "no") && "perc" %in% print)) {

            print.object$perc <- data.frame(x = c("Value", rep("", nrow(print.object$perc) - 1L), "Missing", "Total"),
                                            val = c(print.object$perc[1:(nrow(print.object$perc) - 1L), 1L], "Total", "NA", ""),
                                            rbind(print.object$perc[1:(nrow(print.object$perc) - 1L), -1L],
                                                  apply(print.object$perc[1:(nrow(print.object$perc) - 1L), -1L], 2, function(y) sum(as.numeric(y), na.rm = TRUE)),
                                                  print.object$perc[nrow(print.object$perc), -1L],
                                                  apply(print.object$perc[, -1L], 2, function(y) sum(as.numeric(y), na.rm = TRUE))),
                                            stringsAsFactors = FALSE, row.names = NULL, check.names = FALSE)

            # Round digits
            print.object$perc[, -c(1L:2L)] <- apply(print.object$perc[, -c(1L:2L)], 2, function(y) paste0(formatC(as.numeric(y), digits = digits, format = "f",
                                                                                                                  zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), "%"))

            # Format
            print.object$perc[, 1L:2L] <- apply(print.object$perc[, 1L:2L], 2, function(y) format(y, justify = "left"))

            print.object$perc[, -c(1L:2L)] <- apply(print.object$perc[, -(1L:2L)], 2, function(y) format(y, justify = "right"))

            # No missing data
            if (isTRUE(all(!is.na(x$data)))) {

              print.object$perc <- print.object$perc[-grep("Total",  print.object$perc$x), ]
              print.object$perc$val <- format(misty::chr.trim(print.object$perc$val), justify = "left")

            }

            # Column names
            colnames(print.object$perc)[1L:2L] <- c("", "")

          }

          #....
          # Valid percentages
          if (isTRUE(all(print != "no") && "v.perc" %in% print)) {

            print.object$v.perc <- data.frame(x = c("Value", rep("", nrow(print.object$v.perc) - 1L), "Total"),
                                              val = c(print.object$v.perc[, 1L], ""),
                                              rbind(print.object$v.perc[, -1L],
                                                    apply(print.object$v.perc[, -1L], 2, function(y) sum(as.numeric(y), na.rm = TRUE))),
                                              stringsAsFactors = FALSE, row.names = NULL, check.names = FALSE)

            # Round digits
            print.object$v.perc[, -c(1L:2L)] <- apply(print.object$v.perc[, -c(1L:2L)], 2, function(y) paste0(formatC(as.numeric(y), digits = digits, format = "f",
                                                                                                                      zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), "%"))

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
            if (isTRUE(all(!is.na(x$data)))) {

              print.object$freq <- print.object$freq[, -ncol(print.object$freq)]

            }

          }

          #....
          # Percentages
          if (isTRUE(all(print != "no") && "perc" %in% print)) {

            print.object$perc <- data.frame(print.object$perc[, 1L:(ncol(print.object$perc) - 1L)],
                                            val = apply(print.object$perc[, 2L:(ncol(print.object$perc) - 1)], 1, function(y) sum(as.numeric(y), na.rm = TRUE)),
                                            miss = print.object$perc[, ncol(print.object$perc)],
                                            total = apply(print.object$perc[, 2L:(ncol(print.object$perc))], 1, function(y) sum(as.numeric(y), na.rm = TRUE)),
                                            check.names = FALSE, stringsAsFactors = FALSE)

            # Add variable names
            colnames(print.object$perc) <- c("", colnames(print.object$perc)[2:(ncol(print.object$perc) - 3L)], "Total", "Missing", "Total")

            # Round digits
            print.object$perc[, -1L] <- apply(print.object$perc[, -1L], 2, function(y) paste0(formatC(as.numeric(y), digits = digits, format = "f",
                                                                                                      zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), "%"))

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
          if (isTRUE(all(print != "no") && "v.perc" %in% print)) {

            print.object$v.perc <- data.frame(print.object$v.perc,
                                              total = apply(print.object$v.perc[, 2L:(ncol(print.object$v.perc))], 1, function(y) sum(as.numeric(y), na.rm = TRUE)),
                                              check.names = FALSE, stringsAsFactors = FALSE)

            # Add variable names
            colnames(print.object$v.perc) <- c("", colnames(print.object$v.perc)[2L:(ncol(print.object$v.perc) - 1L)], "Total")

            # Round digits
            print.object$v.perc[, -1L] <- apply(print.object$v.perc[, -1L], 2, function(y) paste0(formatC(as.numeric(y), digits = digits, format = "f",
                                                                                                          zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), "%"))

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
        if (isTRUE(all(print != "no"))) {

          if (isTRUE(freq)) { cat("\n") }

          # Percentages
          if (isTRUE("perc" %in% print)) {

            cat("Percentages\n")
            print(print.object$perc, row.names = FALSE, max = 99999L)

          }

          if (isTRUE(any(is.na(x$data)))) {

            # Valid percentages
            if (isTRUE("v.perc" %in% print)) {

              if (isTRUE("perc" %in% print)) { cat("\n") }

              cat("Valid Percentages\n")
              print(print.object$v.perc, quote = FALSE, row.names = FALSE, max = 99999L)

            }

          }

        }

      # split = TRUE
      } else {

        for (i in names(x$result)) {

          cat(i, "\n")

          temp <- list(call = x$call, type = "freq", data = x$data[, i], args = x$args, result = x$result[[i]])
          class(temp) <- "misty.object"

          print(temp, check = FALSE)

          if (isTRUE(i != names(x$result)[length(names(x$result))])) {

            cat("\n")

          }

        }

      }

    }

  ####################################################################################
  #-----------------------------------------------------------------------------------
  # Coefficient Alpha
  }, item.alpha = {

    if (isTRUE(check)) {

      #......
      # Check input 'print'
      if (isTRUE(!all(print %in% c("all", "alpha", "item")))) {

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
    if (isTRUE("alpha" %in% print)) {

      if (isTRUE(all(c("low", "upp") %in% names(print.object$alpha)))) {

        print.object$alpha$n <- format(paste("", print.object$alpha$n), justify = "right")

        print.object$alpha$items <- format(print.object$alpha$items, justify = "right")

        print.object$alpha$alpha <- formatC(print.object$alpha$alpha, digits = digits, format = "f",
                                            zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0"))

        print.object$alpha$low <- formatC(print.object$alpha$low, digits = digits, format = "f",
                                          zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0"))
        print.object$alpha$upp <- formatC(print.object$alpha$upp, digits = digits, format = "f",
                                          zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0"))

        print.object$alpha <- rbind(c("n", "Items", "Alpha", "Low", "Upp"), print.object$alpha)

        print.object$alpha <- apply(print.object$alpha, 2, function(y) format(y, justify = "right"))

        cat(paste0(ifelse(isTRUE(x$args$std), "Standardized ", "Unstandardized "), "Coefficient Alpha with ",
                   x$args$conf.level*100L, "% Confidence Interval\n\n"))

          write.table(print.object$alpha, quote = FALSE, row.names = FALSE, col.names = FALSE)

        } else {

          print.object$alpha$alpha <- formatC(print.object$alpha$alpha, digits = digits, format = "f",
                                              zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0"))

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
    if (isTRUE("item" %in% print && !is.null(print.object$itemstat) && nrow(print.object$itemstat) > 2L)) {

      print.object$item$pNA <- paste0(formatC(print.object$item$pNA, digits = 2L, format = "f",
                                              zero.print = paste0("0.", paste(rep(0, times = 2L), collapse = ""))), "%")
      print.object$item$m <- formatC(print.object$item$m, digits = 2L, format = "f",
                                     zero.print = paste0("0.", paste(rep(0, times = 2L), collapse = "")))
      print.object$item$sd <- formatC(print.object$item$sd, digits = 2L, format = "f",
                                      zero.print = paste0("0.", paste(rep(0, times = 2L), collapse = "")))
      print.object$item$min <- formatC(print.object$item$min, digits = 2L, format = "f",
                                       zero.print = paste0("0.", paste(rep(0, times = 2L), collapse = "")))
      print.object$item$max <- formatC(print.object$item$max, digits = 2L, format = "f",
                                       zero.print = paste0("0.", paste(rep(0, times = 2L), collapse = "")))

      print.object$item$it.cor <- formatC(print.object$item$it.cor, digits = digits, format = "f",
                                          zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0"))
      print.object$item$alpha <- formatC(print.object$item$alpha, digits = digits, format = "f",
                                         zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0"))

      print.object$item <- rbind(c("Variable", "n", "nNA", "pNA", "M", "SD", "Min", "Max", "It.Cor", "Alpha"),
                                 print.object$item)

      # Format
      print.object$item[, 1L] <- format(paste("", print.object$item[, 1]), justify = "left")
      print.object$item[, -1L] <- apply(print.object$item[, -1L], 2, function(y) format(y, justify = "right"))


      if (isTRUE("alpha" %in% print)) { cat("\n") }

      cat("Item-Total Correlation and Coefficient Alpha if Item Deleted\n\n")

      write.table(print.object$item, quote = FALSE, row.names = FALSE, col.names = FALSE)

    }

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
    if (isTRUE(length(x$result$no.obs) == 1L)) {

      # Format
      for (i in c(5L, 6L, 13L, 14L, 15L)) {

        print.object[i, 2L] <- formatC(as.numeric(unlist(print.object[i, 2L])), digits = digits, format = "f",
                                       zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0"))

      }

      print.object[10L, 2L] <- formatC(as.numeric(unlist(print.object[10L, 2L])), digits = icc.digits, format = "f",
                                       zero.print = ifelse(icc.digits > 0, paste0("0.", paste(rep(0, times = icc.digits), collapse = "")), "0"))
      print.object[11L, 2L] <- formatC(as.numeric(unlist(print.object[11L, 2L])), digits = icc.digits, format = "f",
                                       zero.print = ifelse(icc.digits > 0, paste0("0.", paste(rep(0, times = icc.digits), collapse = "")), "0"))

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

        print.object[i, 2L:ncol(print.object)] <- formatC(as.numeric(unlist(print.object[i, 2L:ncol(print.object)])), digits = digits, format = "f",
                                                          zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0"))

      }

      print.object[11L, 2L:ncol(print.object)] <- formatC(as.numeric(unlist(print.object[11L, 2L:ncol(print.object)])), digits = icc.digits, format = "f",
                                                          zero.print = ifelse(icc.digits > 0, paste0("0.", paste(rep(0, times = icc.digits), collapse = "")), "0"))
      print.object[12L, 2L:ncol(print.object)] <- formatC(as.numeric(unlist(print.object[12L, 2L:ncol(print.object)])), digits = icc.digits, format = "f",
                                                          zero.print = ifelse(icc.digits > 0, paste0("0.", paste(rep(0, times = icc.digits), collapse = "")), "0"))

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
    print.object$cor <- apply(print.object$cor, 2, function(y) formatC(as.numeric(y), digits = digits, format = "f",
                                                                       zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")))

    # Lower and/or upper triangular
    if (isTRUE(tri == "lower")) {

      print.object$cor[upper.tri(print.object$cor)] <- ""

    }

    if (isTRUE(tri == "upper")) {

      print.object$cor[lower.tri(print.object$cor)] <- ""

    }

    diag(print.object$cor) <- ""

    #-----------------------------------------
    # Format Cohen's d matrix

    print.object$d <- apply(print.object$d, 2, function(y) formatC(as.numeric(y), digits = digits, format = "f",
                                                                   zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")))
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

    if (isTRUE(tri == "lower")) {

      print.object[upper.tri(print.object)] <- ""

    }

    if (isTRUE(tri == "upper")) {

      print.object[lower.tri(print.object)] <- ""

    }

    #-----------------------------------------------------------------------------------
    # Main Function

    # Format proportions
    print.object <- apply(print.object, 2, function(y) ifelse(!is.na(as.numeric(y)), formatC(as.numeric(y), digits = digits, format = "f",
                                                                                             zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), ""))

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

    restab$no[8L:13L] <- format(formatC(as.numeric(restab$no[8L:13L]), digits = digits, format = "f",
                                        zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), justify = "right")
    restab$no[1L:7L] <- format(formatC(as.numeric(restab$no[1:7]), digits = 0L, format = "f"), justify = "right")
    restab$no <- format(restab$no, justify = "right")

    restab$perc[restab$perc != ""] <- paste0("(", formatC(as.numeric(restab$perc[restab$perc != ""]), digits = digits, format = "f",
                                                          zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), "%)")

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

      freqtab[, c("pObs", "pNA")] <- apply(freqtab[, c("pObs", "pNA")], 2, function(y) paste0(formatC(y, digits = digits, format = "f",
                                                                                                      zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), "%"))
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
    print.object[, "Perc"] <- paste0(formatC(as.numeric(print.object[, "Perc"]), digits = digits, format = "f",
                                             zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), "%")
    print.object[, "pNA"] <- paste0(formatC(as.numeric(print.object[, "pNA"]), digits = digits, format = "f",
                                            zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), "%")
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
  }, item.omega = {

    #----------------------------------------
    # Input Check

    if (isTRUE(check)) {

      #......
      # Check input 'print'
      if (isTRUE(!all(print %in% c("all", "omega", "item")))) {

        stop("Character strings in the argument 'print' do not all match with \"all\", \"omega\", or \"item\".",
             call. = FALSE)

      }

    }

    #----------------------------------------
    # Arguments

    # Print coefficient omega and/or item statistic
    if (isTRUE(length(print) == 1L && "all" %in% print)) { print <- c("omega", "item") }

    #-----------------------------------------------------------------------------------
    # Main Function

    #-----------------------------------------
    # Omega
    if (isTRUE("omega" %in% print)) {

      print.object$omega$n <- format(paste("", print.object$omega$n), justify = "right")

      print.object$omega$items <- format(print.object$omega$items, justify = "right")

      print.object$omega$omega <- formatC(print.object$omega$omega, digits = digits, format = "f",
                                          zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0"))

      print.object$omega$low <- formatC(print.object$omega$low, digits = digits, format = "f",
                                        zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0"))
      print.object$omega$upp <- formatC(print.object$omega$upp, digits = digits, format = "f",
                                        zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0"))

      print.object$omega <- rbind(c(" n", "Items", "Omega", "Low", "Upp"), print.object$omega)

      print.object$omega <- apply(print.object$omega, 2, function(y) format(y, justify = "right"))

      if (isTRUE(x$args$type == "omega")) {

        cat(paste0(ifelse(isTRUE(x$args$std), "Standardized ", "Unstandardized "), "Coefficient Omega with ",
                   x$args$conf.level*100L, "% Confidence Interval\n\n"))

      } else  if (isTRUE(x$args$type == "hierarch")) {

        cat(paste0(ifelse(isTRUE(x$args$std), "Standardized ", "Unstandardized "), "Hierarchical Omega with ",
                   x$args$conf.level*100L, "% Confidence Interval\n\n"))

      } else if (isTRUE(x$args$type == "categ")) {

        cat(paste0("Categorical Omega with ", x$args$conf.level*100L, "% Confidence Interval\n\n"))

      }

      write.table(print.object$omega, quote = FALSE, row.names = FALSE, col.names = FALSE)

    }

    #-----------------------------------------
    # Item statistics

    if (isTRUE("item" %in% print)) {

      print.object$item$pNA <- paste0(formatC(print.object$item$pNA, digits = 2L, format = "f",
                                              zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), "%")
      print.object$item$m <- formatC(print.object$item$m, digits = 2L, format = "f",
                                     zero.print = paste0("0.", paste(rep(0, times = digits), collapse = "")))
      print.object$item$sd <- formatC(print.object$item$sd, digits = 2L, format = "f",
                                      zero.print = paste0("0.", paste(rep(0, times = digits), collapse = "")))
      print.object$item$min <- formatC(print.object$item$min, digits = 2L, format = "f",
                                       zero.print = paste0("0.", paste(rep(0, times = digits), collapse = "")))
      print.object$item$max <- formatC(print.object$item$max, digits = 2L, format = "f",
                                       zero.print = paste0("0.", paste(rep(0, times = digits), collapse = "")))

      print.object$item$std.ld <- formatC(print.object$item$std.ld, digits = digits, format = "f",
                                          zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0"))
      print.object$item$omega <- formatC(print.object$item$omega, digits = digits, format = "f",
                                         zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0"))

      print.object$item <- rbind(c("Variable", "n", "nNA", "pNA", "M", "SD", "Min", "Max", "Std.Ld", "Omega"),
                                 print.object$item)

      # Format
      print.object$item[, 1L] <- format(paste("", print.object$item[, 1L]), justify = "left")
      print.object$item[, -1L] <- apply(print.object$item[, -1L], 2, function(y) format(y, justify = "right"))

      if (isTRUE("omega" %in% print)) { cat("\n") }

      if (isTRUE(x$args$type == "omega")) {

        cat("Standardized Factor Loadings and Coefficient Omega if Item Deleted\n\n")

      } else if (isTRUE(x$args$type == "hierarch")) {

        cat("Standardized Factor Loadings and Hierarchical Omega if Item Deleted\n\n")

      } else if (isTRUE(x$args$type == "categ")) {

        cat("Standardized Factor Loadings and Categorical Omega if Item Deleted\n\n")

      }

      write.table(print.object$item, quote = FALSE, row.names = FALSE, col.names = FALSE)

    }

  ####################################################################################
  #-----------------------------------------------------------------------------------
  # Phi Coefficient
  }, cor.phi = {

    ####################################################################################
    # Data and Arguments

    # Print triangular
    tri <- ifelse(all(c("both", "lower", "upper") %in% tri), "lower", tri)

    ####################################################################################
    # Main Function

    #----------------------------------------
    # Two variables
    if (isTRUE(is.null(dim(print.object)))) {

      print.object <- cbind("  Estimate: ", formatC(print.object, digits = digits, format = "f",
                                                    zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")))

    #----------------------------------------
    # More than two variables
    } else {

      # Format contingency coefficients
      print.object <- formatC(print.object, digits = digits, format = "f",
                              zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0"))

      # Lower and/or upper triangular
      if (isTRUE(tri == "lower")) {

        print.object[upper.tri(print.object)] <- ""

      }

      if (isTRUE(tri == "upper")) {

        print.object[lower.tri(print.object)] <- ""

      }

      # Set diagonal to "
      diag(print.object) <- ""

      # Format
      row.names(print.object) <- paste("", row.names(print.object))

    }

    ####################################################################################
    # Output

    if (isTRUE(is.null(dim(x$result)))) {

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

    if (isTRUE(is.null(dim(x$result)))) {

      write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE)

    } else {

      print(print.object, quote = FALSE, right = TRUE)

    }

  ####################################################################################
  #-----------------------------------------------------------------------------------
  # Polychoric Correlation Matrix
  }, cor.poly = {

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

    print.object <- formatC(print.object, digits = digits, format = "f",
                            zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0"))
    row.names(print.object) <- paste("", row.names(print.object))


    # Empty matrix diagonal
    diag(print.object) <- ""

    #----------------------------------------
    # Lower and/or upper triangular

    if (isTRUE(tri == "lower")) {

      print.object[upper.tri(print.object)] <- ""

    }

    if (isTRUE(tri == "upper")) {

      print.object[lower.tri(print.object)] <- ""

    }

    #----------------------------------------
    # Row names
    if (isTRUE(!is.null(rownames(print.object)))) {

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
      if (isTRUE(x$args$sample == "one.sample")) {

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

      if (isTRUE(x$args$sample == "one.sample")) {

        cat("  optimal sample size: n =", ceiling(x$result$n), "\n\n")

      } else {

        cat("  optimal sample size: n =", ceiling(x$result$n), "in each group \n\n")

      }

    #----------------------------------------
    # Proportion
    }, prop = {

      if (isTRUE(x$args$correct)) {

        cat("\nSample Size Determination for the", ifelse(x$args$sample == "one.sample", "One-Sample", "Two-Sample"), "Proportion Test with Continuity Correction\n\n")

      } else {

        cat("\nSample Size Determination for the", ifelse(x$args$sample == "one.sample", "One-Sample", "Two-Sample"), "Proportion Test without Continuity Correction\n\n")

      }

      ###

      # one-sample
      if (isTRUE(x$args$sample == "one.sample")) {

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

      if (isTRUE(x$args$sample == "one.sample")) {

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
      if (isTRUE(!all(print %in% c("all", "stdx", "stdy", "stdyx")))) {

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
    if (isTRUE(length(print) == 1L && "all" %in% print)) { print <- c("stdx", "stdy", "stdyx") }

    #-----------------------------------------------------------------------------------
    # Main Function

    #-----------------------------------------
    # Exclude columns
    if (isTRUE(!"stdx" %in% print)) {

      print.object <- print.object[, which(colnames(print.object) != "StdX")]

    }

    if (isTRUE(!"stdy" %in% print)) {

      print.object <- print.object[, which(colnames(print.object) != "StdY")]

    }

    if (isTRUE(!"stdyx" %in% print)) {

      print.object <- print.object[, which(colnames(print.object) != "StdYX")]

    }

    #-----------------------------------------
    # Round
    print.object[, -4] <- apply(print.object[, -4], 2L, function(y) formatC(y, digits = digits, format = "f",
                                                                            zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")))

    print.object[, 4] <- formatC(as.numeric(print.object[, 4]), digits = p.digits, format = "f",
                                 zero.print = ifelse(p.digits > 0, paste0("0.", paste(rep(0, times = p.digits), collapse = "")), "0"))

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
    if (isTRUE(any(c("stdy", "stdyx") %in% print))) {

      cat("\n  Note. SD of the criterion variable", names(x$result$sd)[1], "=", round(x$result$sd[1], digits = digits))

    }

    ####################################################################################
    #-----------------------------------------------------------------------------------
    #  Levene's Test for Homogeneity of Variance
    }, test.levene = {

      #---------------------------------------------------------
      # descript object

      # Round
      print.object[["descript"]][, c("m", "sd", "var", "low", "upp")] <- sapply(c("m", "sd", "var", "low", "upp"),
                                                                          function(y) ifelse(!is.na(print.object[["descript"]][, y]),
                                                                                             formatC(print.object[["descript"]][, y], digits = digits, format = "f",
                                                                                                     zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

      print.object[["descript"]] <- print.object[["descript"]][, -2L]

      # Col names
      print.object[["descript"]] <- rbind(c("Group", "n", "nNA", "M", "SD", "Var", "Low", "Upp"), print.object[["descript"]])

      # Format
      print.object[["descript"]][, 1L] <- format(print.object[["descript"]][, 1L], justify = "left")

      print.object[["descript"]][, -1L] <- apply(print.object[["descript"]][, -1L], 2, format, justify = "right")

      print.object[["descript"]][1L, 1L] <- paste0(" ", print.object[["descript"]][1L, 1L], collapse = "")
      print.object[["descript"]][-1L, 1L] <- paste0("  ", print.object[["descript"]][-1L, 1L])

      print.object[["descript"]][, 1L] <- format(misty::chr.trim(print.object[["descript"]][, 1L], side = "right"), justify = "left")

      print.object[["descript"]][, 1L] <- paste("", print.object[["descript"]][, 1L])

      #---------------------------------------------------------
      # test object

      #.....................................
      # Round

      print.object[["test"]][, "Sum Sq"] <- formatC(print.object[["test"]][, "Sum Sq"], digits = digits, format = "f",
                                                    zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0"))
      print.object[["test"]][, "Mean Sq"] <- formatC(print.object[["test"]][, "Mean Sq"], digits = digits, format = "f",
                                                     zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0"))
      print.object[["test"]][1, "F value"] <- formatC(print.object[["test"]][1, "F value"], digits = digits, format = "f",
                                                      zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0"))
      print.object[["test"]][1, "Pr(>F)"] <- formatC(print.object[["test"]][1, "Pr(>F)"], digits = p.digits, format = "f",
                                                     zero.print = ifelse(p.digits > 0, paste0("0.", paste(rep(0, times = p.digits), collapse = "")), "0"))

      #.....................................
      # Format

      print.object[["test"]] <- rbind(c("Df", "Sum Sq", "Mean Sq", "F", "pval"), print.object[["test"]])
      print.object[["test"]] <- cbind(c("", "  Group", "  Residuals"), print.object[["test"]])

      print.object[["test"]][3L, c("F value", "Pr(>F)")] <- ""

      print.object[["test"]][, -1L] <- apply(print.object[["test"]][, -1L], 2, format, justify = "right")
      print.object[["test"]][, 1L] <- format(print.object[["test"]][, 1L], justify = "left")

      #---------------------------------------------------------
      # Print output

      cat(" Levene's Test based on the", switch(x$args$method, median = "Median\n\n",
                                                mean = "Arithmetic Mean\n\n"))

      # Print hypotheses
      if (isTRUE(hypo)) {

        if (isTRUE(length(unique(x$data[, 2])) == 2)) {

          cat("  Null hypothesis        H0: \u03C3\u00B2\u2081 = \u03C3\u00B2\u2082\n",
              " Alternative hypothesis H1: \u03C3\u00B2\u2081 \u2260 \u03C3\u00B2\u2082\n\n")

        } else {


          cat("  Null hypothesis        H0: \u03C3\u00B2\u1D62 = \u03C3\u00B2\u2C7C for all i and j\n",
              " Alternative hypothesis H1: \u03C3\u00B2\u1D62 \u2260 \u03C3\u00B2\u2C7C for at least one i \u2260 j \n\n")
        }

      }

    # Print descriptive statistics
    if (isTRUE(descript)) {

      write.table(print.object[["descript"]], quote = FALSE, row.names = FALSE, col.names = FALSE, na = "")

      cat("\n")

    }

    write.table(print.object[["test"]], quote = FALSE, row.names = FALSE, col.names = FALSE, na = "")

  ####################################################################################
  #-----------------------------------------------------------------------------------
  # t-Test
  }, test.t = {

    #---------------------------------------------------------
    # One sample t-test
    switch(x$sample, one = {

      #.....................................
      # Round

      print.object[, c("m", "sd", "m.diff", "se", "t", "d", "low", "upp")] <- vapply(print.object[, c("m", "sd", "m.diff", "se", "t", "d", "low", "upp")], formatC,
                                                                                     digits = digits, format = "f",
                                                                                     zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0"), FUN.VALUE = character(1))

      print.object[, "pval"] <- formatC(print.object[, "pval"], digits = p.digits, format = "f",
                                        zero.print = ifelse(p.digits > 0, paste0("0.", paste(rep(0, times = p.digits), collapse = "")), "0"))

      #.....................................
      # Format

      print.object <- rbind(c("n", "nNA", "M", "SD", "M.Diff", "SE", "t", "df", "pval", "d", "Low", "Upp"), print.object)

      print.object <- apply(print.object, 2L, format, justify = "right")

      print.object[, 1] <- paste(" ", print.object[, 1])

      #.....................................
      # Print output

      cat(paste0(" One-Sample t-Test\n\n"))

      # Print hypotheses
      if (isTRUE(hypo)) {

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

      }

      # Print Cohen's d
      if (!isTRUE(effsize)) {

        print.object <- print.object[, -which(colnames(print.object) %in% c("d", "low", "upp"))]

      }

      # Print descriptive statistics
      if (!isTRUE(descript)) {

        print.object <- print.object[, -which(colnames(print.object) %in% c("n", "nNA", "m", "sd", "m.diff", "se"))]

        print.object[, 1] <- paste(" ", print.object[, 1])

      }

      write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE, na = "")

      # Note
      if (isTRUE(effsize)) {

        cat(paste("\n  Note. d = Cohen's d with",
                   switch(x$args$alternative,
                          two.sided = "Two-Sided",
                          less = "One-Sided",
                          greater = "One-Sided"),
                          paste0(round(x$args$conf.level * 100L, digits = 2L), "%"), "CI\n"))

        if (isTRUE(x$args$correct)) {

          cat("        Applying small sample correction factor\n")

        }

      }

    #---------------------------------------------------------
    # Two sample t-test
    }, two = {

      #.....................................
      # Round

      print.object[, c("m", "sd", "m.diff", "se", "t", "d", "low", "upp")] <- vapply(print.object[, c("m", "sd", "m.diff", "se", "t", "d", "low", "upp")], formatC,
                                                                                     digits = digits, format = "f",
                                                                                     zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0"), FUN.VALUE = character(2))

      print.object[, "pval"] <- formatC(print.object[, "pval"], digits = p.digits, format = "f",
                                        zero.print = ifelse(p.digits > 0, paste0("0.", paste(rep(0, times = p.digits), collapse = "")), "0"))

      #.....................................
      # Format

      print.object <- rbind(c("n", "nNA", "M", "SD", "M.Diff", "SE", "t", "df", "pval", "d", "Low", "Upp"), print.object)

      print.object <- apply(print.object, 2L, format, justify = "right")

      print.object[, 1] <- paste(" ", print.object[, 1])

      # NAs
      colnames.NA <- c("m.diff", "se", "t", "df", "pval", "d", "low", "upp")

      print.object[, which(colnames(print.object) %in% colnames.NA)] <- apply(print.object[, which(colnames(print.object) %in% colnames.NA), drop = FALSE], 2, function(y) gsub("NA", "  ", y))

      #.....................................
      # Print output

      cat(paste0(" Two-Sample t-Test\n\n"))

      # Print hypotheses
      if (isTRUE(hypo)) {

        switch(x$args$alternative,
               two.sided = {

                 cat("  Null hypothesis        H0: \u03BC\u2081 = \u03BC\u2082\n",
                     " Alternative hypothesis H1: \u03BC\u2081 \u2260 \u03BC\u2082\n\n")

               }, less = {

                 cat("  Null hypothesis        H0: \u03BC\u2081 \u2264 \u03BC\u2082\n",
                     " Alternative hypothesis H1: \u03BC\u2081 \u003E \u03BC\u2082\n\n")

               }, greater = {

                 cat("  Null hypothesis        H0: \u03BC\u2081 \u2265 \u03BC\u2082\n",
                     " Alternative hypothesis H1: \u03BC\u2081 \u003C \u03BC\u2082\n\n")

               })

      }

      # Print Cohen's d
      if (!isTRUE(effsize)) {

        print.object <- print.object[, -which(colnames(print.object) %in% c("d", "low", "upp"))]

      }

      # Print descriptive statistics
      if (!isTRUE(descript)) {

        print.object <- print.object[, -which(colnames(print.object) %in% c("n", "nNA", "m", "sd", "m.diff", "se"))]

        print.object[, 1] <- paste(" ", print.object[, 1])

      }

      write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE, na = "")

      # Note
      if (isTRUE(effsize)) {

        cat(paste("\n  Note. d = Cohen's d with",
                  switch(x$args$alternative,
                         two.sided = "Two-Sided",
                         less = "One-Sided",
                         greater = "One-Sided"),
                  paste0(round(x$args$conf.level * 100L, digits = 2L), "%"), "CI\n"))

        if (isTRUE(is.null(x$args$ref))) {

          if (isTRUE(x$args$weighted)) {

            if (isTRUE(x$args$correct)) {

              cat("        SD = weighted pooled standard deviation \n        Applying small sample correction factor")

            } else {

              cat("        SD = weighted pooled standard deviation\n")

            }

          } else {

            if (isTRUE(x$args$correct)) {

              cat("        SD = unweighted pooled standard deviation \n        Applying small sample correction factor")

            } else {

              cat("        SD = unweighted pooled standard deviation\n")

            }

          }

        } else {

          if (isTRUE(x$args$correct)) {

            cat(paste0("        SD = standard deviation of the reference group: ", x$args$ref), "\n        Applying small sample correction factor")

          } else {

            cat(paste0("        SD = standard deviation of the reference group: ", x$args$ref, "\n"))

          }

        }

      }

    #---------------------------------------------------------
    # Paired t-test
    }, paired = {

      #.....................................
      # Round

      print.object[, c("m1", "m2", "m.diff", "sd.diff", "se", "t", "d", "low", "upp")] <- vapply(print.object[, c("m1", "m2", "m.diff", "sd.diff", "se", "t", "d", "low", "upp")], formatC,
                                                                                                 digits = digits, format = "f",
                                                                                                 zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0"), FUN.VALUE = character(1))

      print.object[, "pval"] <- formatC(print.object[, "pval"], digits = p.digits, format = "f",
                                        zero.print = ifelse(p.digits > 0, paste0("0.", paste(rep(0, times = p.digits), collapse = "")), "0"))

      #.....................................
      # Format

      print.object <- rbind(c("n", "nNA", "M1", "M2", "M.Diff", "SD.Diff", "SE", "t", "df", "pval", "d", "Low", "Upp"), print.object)

      print.object <- apply(print.object, 2L, format, justify = "right")

      print.object[, 1] <- paste(" ", print.object[, 1])

      #.....................................
      # Print output

      cat(paste0(" Paired-Sample t-Test\n\n"))

      # Print hypotheses
      if (isTRUE(hypo)) {

        switch(x$args$alternative,
               two.sided = {

                 cat("  Null hypothesis        H0: \u03BC\u2081 = \u03BC\u2082\n",
                     " Alternative hypothesis H1: \u03BC\u2081 \u2260 \u03BC\u2082\n\n")

               }, less = {

                 cat("  Null hypothesis        H0: \u03BC\u2081 \u2264 \u03BC\u2082\n",
                     " Alternative hypothesis H1: \u03BC\u2081 \u003E \u03BC\u2082\n\n")

               }, greater = {

                 cat("  Null hypothesis        H0: \u03BC\u2081 \u2265 \u03BC\u2082\n",
                     " Alternative hypothesis H1: \u03BC\u2081 \u003C \u03BC\u2082\n\n")

               })

      }

      # Print Cohen's d
      if (!isTRUE(effsize)) {

        print.object <- print.object[, -which(colnames(print.object) %in% c("d", "low", "upp"))]

      }

      # Print descriptive statistics
      if (!isTRUE(descript)) {

        print.object <- print.object[, -which(colnames(print.object) %in% c("n", "nNA", "m1", "m2", "m.diff", "sd.diff", "se"))]

      }

      write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE, na = "")

      # Note
      if (isTRUE(effsize)) {

        cat(paste("\n  Note. d = Cohen's d with",
                  switch(x$args$alternative,
                         two.sided = "Two-Sided",
                         less = "One-Sided",
                         greater = "One-Sided"),
                  paste0(round(x$args$conf.level * 100L, digits = 2L), "%"), "CI\n"))

        if (isTRUE(x$args$weighted)) {

          if (isTRUE(x$args$correct)) {

            cat("        SD = standard deviation of the difference scores\n        Applying small sample correction factor")

          } else {

            cat("        SD = standard deviation of the difference scores\n")

          }

        } else {

          if (isTRUE(x$args$cor)) {

            if (isTRUE(x$args$correct)) {

              cat("        SD = controlling for the correlation between measures \n        Applying small sample correction factor")

            } else {

              cat("        SD = controlling for the correlation between measures\n")

            }

          } else {

            if (isTRUE(x$args$correct)) {

              cat("        SD = without controlling for the correlation between measures \n        Applying small sample correction factor")

            } else {

              cat("        SD = without controlling for the correlation between measures\n")

            }

          }

        }

      }

    })

  ####################################################################################
  #-----------------------------------------------------------------------------------
  # Welch Test
  }, test.welch = {

    #---------------------------------------------------------
    # Welch t-Test
    switch(x$sample, two = {

      #.....................................
      # Round

      print.object[, c("m", "sd", "m.diff", "se", "t", "df", "d", "low", "upp")] <- vapply(print.object[, c("m", "sd", "m.diff", "se", "t", "df", "d", "low", "upp")], formatC,
                                                                                           digits = digits, format = "f",
                                                                                           zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0"), FUN.VALUE = character(2))

      print.object[, "pval"] <- formatC(print.object[, "pval"], digits = p.digits, format = "f",
                                        zero.print = ifelse(p.digits > 0, paste0("0.", paste(rep(0, times = p.digits), collapse = "")), "0"))

      #.....................................
      # Format

      print.object <- rbind(c("n", "nNA", "M", "SD", "M.Diff", "SE", "t", "df", "pval", "d", "low", "upp"), print.object)

      print.object <- apply(print.object, 2L, format, justify = "right")

      print.object[, 1] <- paste(" ", print.object[, 1])

      # NAs
      colnames.NA <- c("m.diff", "se", "t", "df", "pval", "d", "low", "upp")

      print.object[, which(colnames(print.object) %in% colnames.NA)] <- apply(print.object[, which(colnames(print.object) %in% colnames.NA), drop = FALSE], 2, function(y) gsub("NA", "  ", y))

      #.....................................
      # Print output

      cat(paste0(" Welch's Two-Sample t-Test\n\n"))

      # Print hypotheses
      if (isTRUE(hypo)) {

        switch(x$args$alternative,
               two.sided = {

                 cat("  Null hypothesis        H0: \u03BC\u2081 = \u03BC\u2082\n",
                     " Alternative hypothesis H1: \u03BC\u2081 \u2260 \u03BC\u2082\n\n")

               }, less = {

                 cat("  Null hypothesis        H0: \u03BC\u2081 \u2264 \u03BC\u2082\n",
                     " Alternative hypothesis H1: \u03BC\u2081 \u003E \u03BC\u2082\n\n")

               }, greater = {

                 cat("  Null hypothesis        H0: \u03BC\u2081 \u2265 \u03BC\u2082\n",
                     " Alternative hypothesis H1: \u03BC\u2081 \u003C \u03BC\u2082\n\n")

               })

      }

      # Print Cohen's d
      if (!isTRUE(effsize)) {

        print.object <- print.object[, -which(colnames(print.object) %in% c("d", "low", "upp"))]

      }

      # Print descriptive statistics
      if (!isTRUE(descript)) {

        print.object <- print.object[, -which(colnames(print.object) %in% c("n", "nNA", "m", "sd", "m.diff", "se"))]

      }

      write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE, na = "")

      # Note
      if (isTRUE(effsize)) {

        cat(paste("\n  Note. d = Cohen's d with",
                  switch(x$args$alternative,
                         two.sided = "Two-Sided",
                         less = "One-Sided",
                         greater = "One-Sided"),
                  paste0(round(x$args$conf.level * 100L, digits = 2L), "%"), "CI\n"))

        if (isTRUE(is.null(x$args$ref))) {

          if (isTRUE(x$args$weighted)) {

            cat("        Weighted pooled standard deviation \n")

            if (isTRUE(x$args$correct)) {

              cat("        Applying small sample correction factor")

            }

          } else {

            cat("        Unweighted pooled standard deviation\n")

          }

        } else {

          cat(paste0("          Standard deviation of the reference group: ", x$args$ref, "\n"))

        }

      }

    #---------------------------------------------------------
    # Welch ANOVA
    }, multiple = {

      #.....................................
      # Round

      print.object[["descript"]][, c("m", "sd", "low", "upp")] <- vapply(print.object[["descript"]][, c("m", "sd", "low", "upp")], formatC,
                                                                         digits = digits, format = "f",
                                                                         zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0"), FUN.VALUE = character(nrow(print.object[["descript"]])))

      print.object[["test"]][, c("F", "df2", "eta.sq", "omega.sq")] <- vapply(print.object[["test"]][, c(c("F", "df2", "eta.sq", "omega.sq"))], formatC,
                                                        digits = digits, format = "f",
                                                        zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0"), FUN.VALUE = character(1))

      print.object[["test"]][, "pval"] <- formatC(print.object[["test"]][, "pval"], digits = p.digits, format = "f",
                                                  zero.print = ifelse(p.digits > 0, paste0("0.", paste(rep(0, times = p.digits), collapse = "")), "0"))

      #.....................................
      # Format

      print.object[["descript"]] <- rbind(c("Group", "n", "nNA", "M", "SD", "Low", "Upp"), print.object[["descript"]])
      print.object[["test"]] <- rbind(c("F", "df1", "df2", "pval", "et", "om"), print.object[["test"]])

      print.object[["descript"]][, -1] <- apply(print.object[["descript"]][, -1], 2L, format, justify = "right")
      print.object[["descript"]][-1, 1] <- paste0(" ", print.object[["descript"]][-1, 1])
      print.object[["descript"]][, 1] <- apply(print.object[["descript"]][, 1, drop = FALSE], 2L, format, justify = "left")

      print.object[["test"]] <- apply(print.object[["test"]], 2L, format, justify = "right")

      print.object[["test"]][1, "eta.sq"] <- paste0(paste0(rep(" ", times = nchar(print.object[["test"]][1, "eta.sq"]) - 2), collapse = ""), "\u03B7\u00B2", collapes = "")
      print.object[["test"]][1, "omega.sq"] <- paste0(paste0(rep(" ", times = nchar(print.object[["test"]][1, "eta.sq"]) - 2), collapse = ""), "\u03C9\u00B2", collapes = "")

      print.object[["descript"]][, 1] <- paste(" ", print.object[["descript"]][, 1])
      print.object[["test"]][, 1] <- paste(" ", print.object[["test"]][, 1])

      #.....................................
      # Print output

      cat(paste0(" Welch's ANOVA\n\n"))

      # Print hypotheses
      if (isTRUE(hypo)) {

        cat("  Null hypothesis        H0: \u03BC\u00B2\u1D62 = \u03C3\u00B2\u2C7C for all i and j\n",
            " Alternative hypothesis H1: \u03BC\u00B2\u1D62 \u2260 \u03C3\u00B2\u2C7C for at least one i \u2260 j \n\n")

      }

      # Print descriptive statistics
      if (isTRUE(descript)) {

        write.table(print.object[["descript"]], quote = FALSE, row.names = FALSE, col.names = FALSE, na = "")

        cat("\n")

      }

      # Print eta2 and ometa2
      if (!isTRUE(effsize)) {

        print.object[["test"]] <- print.object[["test"]][, -which(colnames(print.object[["test"]]) %in% c("eta.sq", "omega.sq"))]

      }

      cat(paste(print.object[["test"]][1, ], collapse = " "), "\n")
      write.table(print.object[["test"]][-1, , drop = FALSE], quote = FALSE, row.names = FALSE, col.names = FALSE, na = "")

    })

  ####################################################################################
  #-----------------------------------------------------------------------------------
  # z-Test
  }, test.z = {

    #---------------------------------------------------------
    # One sample z-test
    switch(x$sample, one = {

      #.....................................
      # Round

      print.object[, c("m", "sd", "m.diff", "se", "z", "d")] <- vapply(print.object[, c("m", "sd", "m.diff", "se", "z", "d")], formatC,
                                                                       digits = digits, format = "f",
                                                                       zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0"), FUN.VALUE = character(1))

      print.object[, "pval"] <- formatC(print.object[, "pval"], digits = p.digits, format = "f",
                                        zero.print = ifelse(p.digits > 0, paste0("0.", paste(rep(0, times = p.digits), collapse = "")), "0"))

      #.....................................
      # Format

      print.object <- rbind(c("n", "nNA", "M", "SD", "M.Diff", "SE", "z", "pval", "d"), print.object)

      print.object <- apply(print.object, 2L, format, justify = "right")

      print.object[, 1] <- paste(" ", print.object[, 1])

      #.....................................
      # Print output

      cat(paste0(" One-Sample z-Test with ", "\u03c3 = ", round(x$args$sigma, digits = digits), "\n\n"))

      # Print hypotheses
      if (isTRUE(hypo)) {

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

      }

      # Print Cohen's d
      if (!isTRUE(effsize)) {

        print.object <- print.object[, -which(colnames(print.object) %in% c("d"))]

      }

      # Print descriptive statistics
      if (!isTRUE(descript)) {

        print.object <- print.object[, -which(colnames(print.object) %in% c("n", "nNA", "m", "sd", "m.diff", "se"))]

        print.object[, 1] <- paste(" ", print.object[, 1])

      }

      write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE, na = "")

    #---------------------------------------------------------
    # Two sample z-test
    }, two = {

      #.....................................
      # Round

      print.object[, c("m", "sd", "m.diff", "se", "z", "d")] <- vapply(print.object[, c("m", "sd", "m.diff", "se", "z", "d")], formatC,
                                                                       digits = digits, format = "f",
                                                                       zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0"), FUN.VALUE = character(2))

      print.object[, "pval"] <- formatC(print.object[, "pval"], digits = p.digits, format = "f",
                                        zero.print = ifelse(p.digits > 0, paste0("0.", paste(rep(0, times = p.digits), collapse = "")), "0"))

      #.....................................
      # Format

      print.object <- rbind(c("n", "nNA", "M", "SD", "M.Diff", "SE", "z", "pval", "d"), print.object)

      print.object <- apply(print.object, 2L, format, justify = "right")

      print.object[, 1] <- paste(" ", print.object[, 1])

      # NAs
      print.object[, which(colnames(print.object) %in% c("m.diff", "se", "z", "pval", "d"))] <-
        apply(print.object[, which(colnames(print.object) %in% c("m.diff", "se", "z", "pval", "d")), drop = FALSE], 2, function(y) gsub("NA", "  ", y))

      #.....................................
      # Print output

      if (isTRUE(length(unique(x$args$sigma)) == 1L)) {

        cat(paste0(" Two-Sample z-Test with ", "\u03c3\u2081 = \u03c3\u2082 = ", round(x$args$sigma[1], digits = digits), "\n\n"))

      } else {

        cat(paste0(" Two-Sample z-Test with ", "\u03c3\u2081 = ", x$args$sigma[1], " and ", "\u03c3\u2082 = ", round(x$args$sigma[2], digits = digits), "\n\n"))

      }

      # Print hypotheses
      if (isTRUE(hypo)) {

        switch(x$args$alternative,
               two.sided = {

                 cat("  Null hypothesis        H0: \u03BC\u2081 = \u03BC\u2082\n",
                     " Alternative hypothesis H1: \u03BC\u2081 \u2260 \u03BC\u2082\n\n")

               }, less = {

                 cat("  Null hypothesis        H0: \u03BC\u2081 \u2264 \u03BC\u2082\n",
                     " Alternative hypothesis H1: \u03BC\u2081 \u003E \u03BC\u2082\n\n")

               }, greater = {

                 cat("  Null hypothesis        H0: \u03BC\u2081 \u2265 \u03BC\u2082\n",
                     " Alternative hypothesis H1: \u03BC\u2081 \u003C \u03BC\u2082\n\n")

               })

      }

      # Print Cohen's d
      if (!isTRUE(effsize)) {

        print.object <- print.object[, -which(colnames(print.object) %in% c("d"))]

      }

      # Print descriptive statistics
      if (!isTRUE(descript)) {

        print.object <- print.object[-2, -which(colnames(print.object) %in% c("n", "nNA", "m", "sd", "m.diff", "se"))]

        print.object[, 1] <- paste(" ", print.object[, 1])

      }

      write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE, na = "")

    #---------------------------------------------------------
    # Paired z-test
    }, paired = {

      #.....................................
      # Round

      print.object[, c("m1", "m2", "m.diff", "sd.diff", "se", "z", "d")] <- vapply(print.object[, c("m1", "m2", "m.diff", "sd.diff", "se", "z", "d")], formatC,
                                                                                   digits = digits, format = "f",
                                                                                   zero.print = ifelse(digits > 0, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0"), FUN.VALUE = character(1))

      print.object[, "pval"] <- formatC(print.object[, "pval"], digits = p.digits, format = "f",
                                        zero.print = ifelse(p.digits > 0, paste0("0.", paste(rep(0, times = p.digits), collapse = "")), "0"))

      #.....................................
      # Format

      print.object <- rbind(c("n", "nNA", "M1", "M2", "M.Diff", "SD.Diff", "SE", "z", "pval", "d"), print.object)

      print.object <- apply(print.object, 2L, format, justify = "right")

      print.object[, 1] <- paste(" ", print.object[, 1])

      #.....................................
      # Print output

      cat(paste0(" Paired-Sample z-Test with ", "\u03c3(diff) = ", round(x$args$sigma, digits = digits), "\n\n"))

      # Print hypotheses
      if (isTRUE(hypo)) {

        switch(x$args$alternative,
               two.sided = {

                 cat("  Null hypothesis        H0: \u03BC\u2081 = \u03BC\u2082\n",
                     " Alternative hypothesis H1: \u03BC\u2081 \u2260 \u03BC\u2082\n\n")

               }, less = {

                 cat("  Null hypothesis        H0: \u03BC\u2081 \u2264 \u03BC\u2082\n",
                     " Alternative hypothesis H1: \u03BC\u2081 \u003E \u03BC\u2082\n\n")

               }, greater = {

                 cat("  Null hypothesis        H0: \u03BC\u2081 \u2265 \u03BC\u2082\n",
                     " Alternative hypothesis H1: \u03BC\u2081 \u003C \u03BC\u2082\n\n")

               })

      }

      # Print Cohen's d
      if (!isTRUE(effsize)) {

        print.object <- print.object[, -which(colnames(print.object) %in% c("d"))]

      }

      # Print descriptive statistics
      if (!isTRUE(descript)) {

        print.object <- print.object[, -which(colnames(print.object) %in% c("n", "nNA", "m1", "m2", "m.diff", "sd.diff", "se"))]

      }

      write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE, na = "")

    })

  })

}
