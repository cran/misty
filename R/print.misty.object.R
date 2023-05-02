#' Print misty.object object
#'
#' This function prints the \code{misty.object} object
#'
#' @param x          \code{misty.object} object.
#' @param print      a character string or character vector indicating which
#'                   results to to be printed on the console.
#' @param tri        a character string or character vector indicating which
#'                   triangular of the matrix to show on the console, i.e.,
#'                   \code{both} for upper and lower triangular, \code{lower}
#'                   for the lower triangular, and \code{upper} for the upper
#'                   triangular.
#' @param freq       logical: if \code{TRUE}, absolute frequencies will be included
#'                   in the cross tabulation (\code{crosstab()} function).
#' @param hypo       logical: if \code{TRUE}, null and alternative hypothesis are
#'                   shown on the console (\code{\link{test.t}},
#'                   \code{\link{test.welch}}, \code{\link{test.z}} function).
#' @param descript   logical: if \code{TRUE}, descriptive statistics are shown on
#'                   the console (\code{\link{test.t}}, \code{\link{test.welch}},
#'                   \code{\link{test.z}} function).
#' @param epsilon    logical: if \code{TRUE}, box indices of sphericity (epsilon)
#'                   are shown on the console (\code{\link{aov.w}}).
#' @param effsize    logical: if \code{TRUE}, effect size measure(s) is shown on
#'                   the console (\code{\link{test.t}}, \code{\link{test.welch}},
#'                   \code{\link{test.z}} function).
#'                   \code{\link{test.z}} function).
#' @param posthoc    logical: if \code{TRUE},post hoc test for multiple comparison
#'                   is shown on the console (\code{\link{test.welch}}).
#' @param split      logical: if \code{TRUE}, output table is split by variables
#'                   when specifying more than one variable in \code{x}
#'                   (\code{\link{freq}}).
#' @param table      logical: if \code{TRUE}, a frequency table with number of
#'                   observed values (\code{"nObs"}), percent of observed values
#'                   (\code{"pObs"}), number of missing values (\code{"nNA"}),
#'                   and percent of missing values (\code{"pNA"}) is printed for
#'                   each variable on the console (\code{na.descript()} function).
#' @param digits     an integer value indicating the number of decimal places digits
#'                   to be used for displaying results.
#' @param p.digits   an integer indicating the number of decimal places to be used
#'                   for displaying \emph{p}-values.
#' @param icc.digits an integer indicating the number of decimal places to be used
#'                   for displaying intraclass correlation coefficients
#'                   (\code{multilevel.descript()} and \code{multilevel.icc()}
#'                   function).
#' @param sort.var   logical: if \code{TRUE}, output is sorted by variables.
#' @param order      logical: if \code{TRUE}, variables are ordered from left to
#'                   right in increasing order
#'                   of missing values (\code{na.descript()} function).
#' @param check      logical: if \code{TRUE}, argument specification is checked.
#' @param ...        further arguments passed to or from other methods.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{aov.b}}, \code{\link{aov.w}}, \code{\link{aov.w}},
#' \code{\link{item.alpha}},
#' \code{\link{ci.mean.diff}}, \code{\link{ci.mean}}, \code{\link{ci.mean.w}},
#' \code{\link{ci.median}}, \code{\link{ci.prop.diff}}, \code{\link{ci.prop}},
#' \code{\link{ci.sd}}, \code{\link{ci.var}}, \code{\link{cohens.d}},
#' \code{\link{check.collin}}, \code{\link{cor.cont}}, \code{\link{cor.matrix}},
#' \code{\link{cor.cramer}}, \code{\link{crosstab}}, \code{\link{descript}},
#' \code{\link{eta.sq}}, \code{\link{freq}}, \code{\link{test.levene}},
#' \code{\link{multilevel.descript}}, \code{\link{multilevel.r2}},
#' \code{\link{multilevel.r2.manual}},\code{\link{na.auxiliary}},
#' \code{\link{na.coverage}}, \code{\link{na.descript}}, \code{\link{na.pattern}},
#' \code{\link{item.omega}}, \code{\link{cor.phi}}, \code{\link{cor.poly}},
#' \code{\link{size.cor}}, \code{\link{size.mean}}, \code{\link{size.prop}},
#' \code{\link{test.levene}}, \code{\link{test.t}}, \code{\link{test.welch}},
#' \code{\link{test.z}}
#'
#' @method print misty.object
#'
#' @export
print.misty.object <- function(x, print = x$args$print, tri = x$args$tri,
                               freq = x$args$freq, hypo = x$args$hypo,
                               descript = x$args$descript, epsilon = x$args$epsilon,
                               effsize = x$args$effsize, posthoc = x$args$posthoc,
                               split = x$args$split, table = x$args$table,
                               digits = x$args$digits, p.digits = x$args$p.digits,
                               icc.digits = x$args$icc.digits, sort.var = x$args$sort.var,
                               order = x$args$order, check = TRUE, ...) {

  #_____________________________________________________________________________
  #
  # Global binding -------------------------------------------------------------

  group <- NULL

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # Check input 'tri'
    if (isTRUE(!is.null(tri))) { if (isTRUE(any(!tri %in% c("both", "lower", "upper")))) { stop("Character string in the argument 'tri' does not match with \"both\", \"lower\", or \"upper\".", call. = FALSE) } }

    # Check input 'table'
    if (isTRUE(!is.null(table))) { if (isTRUE(!is.logical(table))) { stop("Please specify TRUE or FALSE for the argument 'table'.", call. = FALSE) } }

    # Check input 'freq'
    if (isTRUE(!is.null(freq))) { if (isTRUE(!is.logical(freq))) { stop("Please specify TRUE or FALSE for the argument 'freq'.", call. = FALSE) } }

    # Check input 'hypo'
    if (isTRUE(!is.null(hypo))) { if (isTRUE(!is.logical(hypo))) { stop("Please specify TRUE or FALSE for the argument 'hypo'.", call. = FALSE)  } }

    # Check input 'descript'
    if (isTRUE(!is.null(descript))) { if (isTRUE(!is.logical(descript))) { stop("Please specify TRUE or FALSE for the argument 'descript'.", call. = FALSE) } }

    # Check input 'effsize'
    if (isTRUE(!is.null(effsize))) { if (isTRUE(!is.logical(effsize))) { stop("Please specify TRUE or FALSE for the argument 'effsize'.", call. = FALSE) } }

    # Check input 'digits'
    if (isTRUE(!is.null(digits))) { if (isTRUE(digits %% 1L != 0L || digits < 0L)) { stop("Specify a positive integer number for the argument 'digits'", call. = FALSE) } }

    # Check input 'p.digits'
    if (isTRUE(!is.null(p.digits))) { if (isTRUE(p.digits %% 1L != 0L || p.digits < 0L)) { stop("Specify a positive integer number for the argument 'p.digits'", call. = FALSE) } }

    # Check input 'icc.digits'
    if (isTRUE(!is.null(icc.digits))) { if (isTRUE(icc.digits %% 1L != 0L || icc.digits < 0L)) { stop("Specify a positive integer number for the argument 'icc.digits'", call. = FALSE) } }

    # Check input 'sort.var'
    if (isTRUE(!is.null(sort.var))) { if (isTRUE(!is.logical(sort.var))) { stop("Please specify TRUE or FALSE for the argument 'sort.var'.", call. = FALSE) } }

    # Check input 'order'
    if (isTRUE(!is.null(order))) { if (isTRUE(!is.logical(order))) { stop("Please specify TRUE or FALSE for the argument 'order'.", call. = FALSE) } }

  }

  # Print object
  print.object <- x$result

  #_____________________________________________________________________________
  #
  # Between-Subject Analysis of Variance (ANOVA) -------------------------------
  switch(x$type, aov.b = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Round ####

    print.object[["descript"]][, c("m", "sd", "low", "upp")] <- vapply(print.object[["descript"]][, c("m", "sd", "low", "upp")], formatC,
                                                                       digits = digits, format = "f",
                                                                       zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"), FUN.VALUE = character(nrow(print.object[["descript"]])))

    print.object[["test"]][, c("sum.sq", "mean.sq", "F", "eta.sq", "omega.sq")] <- vapply(print.object[["test"]][, c("sum.sq", "mean.sq", "F", "eta.sq", "omega.sq")], formatC,
                                                                                          digits = digits, format = "f",
                                                                                          zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"), FUN.VALUE = character(nrow(print.object[["test"]])))

    print.object[["test"]][, "pval"] <- formatC(print.object[["test"]][, "pval"], digits = p.digits, format = "f",
                                                zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

    print.object[["posthoc"]][, c("m.diff", "m.low", "m.upp", "d", "d.low", "d.upp")] <- vapply(print.object[["posthoc"]][, c("m.diff", "m.low", "m.upp", "d", "d.low", "d.upp")], formatC,
                                                                                                digits = digits, format = "f",
                                                                                                zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"), FUN.VALUE = character(nrow(print.object[["posthoc"]])))

    print.object[["posthoc"]][, "pval"] <- formatC(print.object[["posthoc"]][, "pval"], digits = p.digits, format = "f",
                                                   zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Format ####

    print.object[["descript"]] <- rbind(c("Group", "n", "nNA", "M", "SD", "Low", "Upp"), print.object[["descript"]])
    print.object[["test"]] <- rbind(c("Source", "Sum Sq", "df", "Mean Sq", "F", "pval", "et", "om"), print.object[["test"]])
    print.object[["posthoc"]] <- rbind(c("Group1", "Group2", "M.diff", "Low", "Upp", "pval", "d", "Low", "Upp"), print.object[["posthoc"]])

    print.object[["descript"]][, -1L] <- apply(print.object[["descript"]][, -1L], 2L, format, justify = "right")
    print.object[["descript"]][-1L, 1L] <- paste0(" ", print.object[["descript"]][-1L, 1L])
    print.object[["descript"]][, 1L] <- apply(print.object[["descript"]][, 1L, drop = FALSE], 2L, format, justify = "left")

    print.object[["test"]][, c("F", "pval", "eta.sq", "omega.sq")] <- apply(print.object[["test"]][, c("F", "pval", "eta.sq", "omega.sq")], 2L, function(x) gsub("NA", "", x))
    print.object[["test"]][, -1L] <- apply(print.object[["test"]][, -1L], 2L, format, justify = "right")
    print.object[["test"]][-1L, 1L] <- paste0(" ", print.object[["test"]][-1L, 1L])
    print.object[["test"]][, 1L] <- format(print.object[["test"]][, 1L], justify = "left")

    print.object[["posthoc"]][, -c(1L, 2L)] <- apply(print.object[["posthoc"]][, -c(1L, 2L)], 2L, format, justify = "right")
    print.object[["posthoc"]][-1L, 1L] <- paste0(" ", print.object[["posthoc"]][-1L, 1L])
    print.object[["posthoc"]][-1L, 2L] <- paste0(" ", print.object[["posthoc"]][-1L, 2L])
    print.object[["posthoc"]][, c(1L, 2L)] <- apply(print.object[["posthoc"]][, c(1L, 2L)], 2L, format, justify = "left")

    print.object[["test"]][1L, "eta.sq"] <- paste0(paste0(rep(" ", times = nchar(print.object[["test"]][1L, "eta.sq"]) - 2L), collapse = ""), "\u03B7\u00B2", collapes = "")
    print.object[["test"]][1L, "omega.sq"] <- paste0(paste0(rep(" ", times = nchar(print.object[["test"]][1L, "eta.sq"]) - 2L), collapse = ""), "\u03C9\u00B2", collapes = "")

    print.object[["descript"]][, 1L] <- paste(" ", print.object[["descript"]][, 1L])
    print.object[["test"]][, 1L] <- paste(" ", print.object[["test"]][, 1L])
    print.object[["posthoc"]][, 1L] <- paste("  ", print.object[["posthoc"]][, 1L])

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Print output ####

    cat(paste0(" Between-Subject Analysis of Variance\n\n"))

    ###
    # Print hypotheses
    if (isTRUE(hypo)) {

      # R Markdown in progress
      if (isTRUE(getOption("knitr.in.progress"))) {

        cat("  Null hypothesis        H0: mu.i = mu.j for all i and j\n",
            " Alternative hypothesis H1: mu.i != mu.j for at least one i != j \n\n")

      } else {

        cat("  Null hypothesis        H0: \u03BC\u1D62 = \u03BC\u2C7C for all i and j\n",
            " Alternative hypothesis H1: \u03BC\u1D62 \u2260 \u03BC\u2C7C for at least one i \u2260 j \n\n")

      }

    }

    # Print descriptive statistics
    if (isTRUE(descript)) {

      write.table(print.object[["descript"]], quote = FALSE, row.names = FALSE, col.names = FALSE, na = "")

      cat("\n")

    }

    # Print effects size
    if (isTRUE(!effsize)) {

      print.object[["test"]] <- print.object[["test"]][, -which(colnames(print.object[["test"]]) %in% c("eta.sq", "omega.sq"))]

      print.object[["posthoc"]] <- print.object[["posthoc"]][, -which(colnames(print.object[["posthoc"]]) %in% c("d", "d.low", "d.upp"))]

    }

    cat(paste(print.object[["test"]][1L, ], collapse = " "), "\n")
    write.table(print.object[["test"]][-1L, , drop = FALSE], quote = FALSE, row.names = FALSE, col.names = FALSE, na = "")

    ###
    # Print post-hoc test
    if (isTRUE(posthoc)) {

      cat(paste0("\n  Tukey HSD Post Hoc Test for Multiple Comparison\n\n"))

      write.table(print.object[["posthoc"]], quote = FALSE, row.names = FALSE, col.names = FALSE, na = "")

    }

  #_____________________________________________________________________________
  #
  # Repeated Measures Analysis of Variance -------------------------------------
  }, aov.w = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Rename ####

    names(print.object[["epsilon"]]) <- c("index", "ep")
    print.object[["epsilon"]][, 1L] <- c("Lower Bound:", "Greenhouse and Geisser (GG):", "Huynh and Feldt (HF):", "Average of GG and HF:")

    names(print.object[["test"]][["none"]]) <- c("Source", "Sum Sq", "df", "Mean Sq", "F", "pval", "et", "et.p", "om", "om.p")
    names(print.object[["test"]][["lb"]]) <- c("Source", "Sum Sq", "df", "Mean Sq", "F", "pval", "et", "et.p", "om", "om.p")
    names(print.object[["test"]][["gg"]]) <- c("Source", "Sum Sq", "df", "Mean Sq", "F", "pval", "et", "et.p", "om", "om.p")
    names(print.object[["test"]][["hf"]]) <- c("Source", "Sum Sq", "df", "Mean Sq", "F", "pval", "et", "et.p", "om", "om.p")

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Round ####

    # Descriptive statistics
    print.object[["descript"]][, c("m", "sd", "low", "upp")] <- vapply(print.object[["descript"]][, c("m", "sd", "low", "upp")], formatC,
                                                                       digits = digits, format = "f",
                                                                       zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"), FUN.VALUE = character(nrow(print.object[["descript"]])))

    # Box Index of Sphericity
    print.object[["epsilon"]][, "ep"] <- formatC(print.object[["epsilon"]][, "ep"] , digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

    # ANOVA tables
    print.object[["test"]][["none"]][, c("Sum Sq", "Mean Sq", "F", "et", "et.p", "om", "om.p")] <- vapply(print.object[["test"]][["none"]][, c("Sum Sq", "Mean Sq", "F", "et", "et.p", "om", "om.p")], formatC,
                                                                                                                              digits = digits, format = "f",
                                                                                                                              zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"), FUN.VALUE = character(nrow(print.object[["test"]][["none"]])))

    print.object[["test"]][["none"]][, "df"] <- formatC(print.object[["test"]][["none"]][, "df"], digits = 0L, mode = "integer")

    print.object[["test"]][["none"]][, "pval"] <- formatC(print.object[["test"]][["none"]][, "pval"], digits = p.digits, format = "f",
                                                          zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

    ##

    print.object[["test"]][["lb"]][, c("Sum Sq", "df", "Mean Sq", "F", "et", "et.p", "om", "om.p")] <- vapply(print.object[["test"]][["lb"]][, c("Sum Sq", "df", "Mean Sq", "F", "et", "et.p", "om", "om.p")], formatC,
                                                                                                              digits = digits, format = "f",
                                                                                                              zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"), FUN.VALUE = character(nrow(print.object[["test"]][["lb"]])))

    print.object[["test"]][["lb"]][, "pval"] <- formatC(print.object[["test"]][["lb"]][, "pval"], digits = p.digits, format = "f",
                                                        zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

    ##

    print.object[["test"]][["gg"]][, c("Sum Sq", "df", "Mean Sq", "F", "et", "et.p", "om", "om.p")] <- vapply(print.object[["test"]][["gg"]][, c("Sum Sq", "df", "Mean Sq", "F", "et", "et.p", "om", "om.p")], formatC,
                                                                                                              digits = digits, format = "f",
                                                                                                              zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"), FUN.VALUE = character(nrow(print.object[["test"]][["gg"]])))

    print.object[["test"]][["gg"]][, "pval"] <- formatC(print.object[["test"]][["gg"]][, "pval"], digits = p.digits, format = "f",
                                                        zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))


    ###

    print.object[["test"]][["hf"]][, c("Sum Sq", "df", "Mean Sq", "F", "et", "et.p", "om", "om.p")] <- vapply(print.object[["test"]][["hf"]][, c("Sum Sq", "df", "Mean Sq", "F", "et", "et.p", "om", "om.p")], formatC,
                                                                                                              digits = digits, format = "f",
                                                                                                              zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"), FUN.VALUE = character(nrow(print.object[["test"]][["hf"]])))

    print.object[["test"]][["hf"]][, "pval"] <- formatC(print.object[["test"]][["hf"]][, "pval"], digits = p.digits, format = "f",
                                                        zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

    # Post hoc tests
    print.object[["posthoc"]][, c("m.diff", "t", "d", "d.low", "d.upp")] <- vapply(print.object[["posthoc"]][, c("m.diff", "t", "d", "d.low", "d.upp")], formatC,
                                                                                   digits = digits, format = "f",
                                                                                   zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"), FUN.VALUE = character(nrow(print.object[["posthoc"]])))

    print.object[["posthoc"]][, "pval"] <- formatC(print.object[["posthoc"]][, "pval"], digits = p.digits, format = "f",
                                                   zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Format ####

    print.object[["descript"]] <- rbind(c("Variable", "n", "nNA", "M", "SD", "Low", "Upp"), print.object[["descript"]])
    print.object[["epsilon"]] <- rbind(c("Box Index of Sphericity", "ep"), print.object[["epsilon"]])
    print.object[["test"]] <- lapply(print.object[["test"]], function(y) rbind(c("Source", "Sum Sq", "df", "Mean Sq", "F", "pval", "et", "et.p", "om", "om.p"), y))
    print.object[["posthoc"]] <- rbind(c("Variable1", "Variable2", "M.diff", "t", "df", "pval", "d", "Low", "Upp"), print.object[["posthoc"]])

    # Descriptive statistics
    print.object[["descript"]][, -1L] <- apply(print.object[["descript"]][, -1L], 2L, format, justify = "right")
    print.object[["descript"]][-1L, 1L] <- paste0(" ", print.object[["descript"]][-1L, 1L])
    print.object[["descript"]][, 1L] <- apply(print.object[["descript"]][, 1L, drop = FALSE], 2L, format, justify = "left")

    # Epsilon, Eta and omega squared
    print.object[["epsilon"]][1L, "ep"] <- "\u03B5"

    print.object[["test"]][["none"]][1L, "et"] <- paste0(paste0(rep(" ", times = nchar(print.object[["test"]][["none"]][1L, "et"]) - 2L), collapse = ""), "\u03B7\u00B2", collapes = "")
    print.object[["test"]][["none"]][1L, "et.p"] <- paste0(paste0(rep(" ", times = nchar(print.object[["test"]][["none"]][1L, "et.p"]) - 2L), collapse = ""), "\u03B7\u00B2p", collapes = "")
    print.object[["test"]][["none"]][1L, "om"] <- paste0(paste0(rep(" ", times = nchar(print.object[["test"]][["none"]][1L, "om"]) - 2L), collapse = ""), "\u03C9\u00B2", collapes = "")
    print.object[["test"]][["none"]][1L, "om.p"] <- paste0(paste0(rep(" ", times = nchar(print.object[["test"]][["none"]][1L, "om.p"]) - 2L), collapse = ""), "\u03C9\u00B2p", collapes = "")

    print.object[["test"]][["lb"]][1L, "et"] <- paste0(paste0(rep(" ", times = nchar(print.object[["test"]][["lb"]][1L, "et"]) - 2L), collapse = ""), "\u03B7\u00B2", collapes = "")
    print.object[["test"]][["lb"]][1L, "et.p"] <- paste0(paste0(rep(" ", times = nchar(print.object[["test"]][["lb"]][1L, "et.p"]) - 2L), collapse = ""), "\u03B7\u00B2p", collapes = "")
    print.object[["test"]][["lb"]][1L, "om"] <- paste0(paste0(rep(" ", times = nchar(print.object[["test"]][["lb"]][1L, "om"]) - 2L), collapse = ""), "\u03C9\u00B2", collapes = "")
    print.object[["test"]][["lb"]][1L, "om.p"] <- paste0(paste0(rep(" ", times = nchar(print.object[["test"]][["lb"]][1L, "om.p"]) - 2L), collapse = ""), "\u03C9\u00B2p", collapes = "")

    print.object[["test"]][["gg"]][1L, "et"] <- paste0(paste0(rep(" ", times = nchar(print.object[["test"]][["gg"]][1L, "et"]) - 2L), collapse = ""), "\u03B7\u00B2", collapes = "")
    print.object[["test"]][["gg"]][1L, "et.p"] <- paste0(paste0(rep(" ", times = nchar(print.object[["test"]][["gg"]][1L, "et.p"]) - 2L), collapse = ""), "\u03B7\u00B2p", collapes = "")
    print.object[["test"]][["gg"]][1L, "om"] <- paste0(paste0(rep(" ", times = nchar(print.object[["test"]][["gg"]][1L, "om"]) - 2L), collapse = ""), "\u03C9\u00B2", collapes = "")
    print.object[["test"]][["gg"]][1L, "om.p"] <- paste0(paste0(rep(" ", times = nchar(print.object[["test"]][["gg"]][1L, "om.p"]) - 2L), collapse = ""), "\u03C9\u00B2p", collapes = "")

    print.object[["test"]][["hf"]][1L, "et"] <- paste0(paste0(rep(" ", times = nchar(print.object[["test"]][["hf"]][1L, "et"]) - 2L), collapse = ""), "\u03B7\u00B2", collapes = "")
    print.object[["test"]][["hf"]][1L, "et.p"] <- paste0(paste0(rep(" ", times = nchar(print.object[["test"]][["hf"]][1L, "et.p"]) - 2L), collapse = ""), "\u03B7\u00B2p", collapes = "")
    print.object[["test"]][["hf"]][1L, "om"] <- paste0(paste0(rep(" ", times = nchar(print.object[["test"]][["hf"]][1L, "om"]) - 2L), collapse = ""), "\u03C9\u00B2", collapes = "")
    print.object[["test"]][["hf"]][1L, "om.p"] <- paste0(paste0(rep(" ", times = nchar(print.object[["test"]][["hf"]][1L, "om.p"]) - 2L), collapse = ""), "\u03C9\u00B2p", collapes = "")

    # Box Index of Sphericity
    print.object[["epsilon"]][, 1L] <- format(print.object[["epsilon"]][, 1L], justify = "left")
    print.object[["epsilon"]][-1L, 1L] <- paste0(" ", print.object[["epsilon"]][-1L, 1L])
    print.object[["epsilon"]][, 2L] <-format(print.object[["epsilon"]][, 2L], justify = "right")
    print.object[["epsilon"]][, 1L] <- format(print.object[["epsilon"]][, 1L], justify = "left")

    # ANOVA tables
    print.object[["test"]][["none"]] <- apply(print.object[["test"]][["none"]], 2L, function(x) gsub("NA", "", x))
    print.object[["test"]][["none"]][, -1L] <- apply(print.object[["test"]][["none"]][, -1L], 2L, format, justify = "right")
    print.object[["test"]][["none"]][-1L, 1L] <- paste0(" ", print.object[["test"]][["none"]][-1L, 1L])
    print.object[["test"]][["none"]][c(3L, 4L), 1L] <- paste0(" ", print.object[["test"]][["none"]][c(3L, 4L), 1L])
    print.object[["test"]][["none"]][, 1L] <- format(print.object[["test"]][["none"]][, 1L], justify = "left")

    print.object[["test"]][["lb"]] <- apply(print.object[["test"]][["lb"]], 2L, function(x) gsub("NA", "", x))
    print.object[["test"]][["lb"]][, -1L] <- apply(print.object[["test"]][["lb"]][, -1L], 2L, format, justify = "right")
    print.object[["test"]][["lb"]][-1L, 1L] <- paste0(" ", print.object[["test"]][["lb"]][-1L, 1L])
    print.object[["test"]][["lb"]][c(3L, 4L), 1L] <- paste0(" ", print.object[["test"]][["lb"]][c(3L, 4L), 1L])
    print.object[["test"]][["lb"]][, 1L] <- format(print.object[["test"]][["lb"]][, 1L], justify = "left")

    print.object[["test"]][["gg"]] <- apply(print.object[["test"]][["gg"]], 2L, function(x) gsub("NA", "", x))
    print.object[["test"]][["gg"]][, -1L] <- apply(print.object[["test"]][["gg"]][, -1L], 2L, format, justify = "right")
    print.object[["test"]][["gg"]][-1L, 1L] <- paste0(" ", print.object[["test"]][["gg"]][-1L, 1L])
    print.object[["test"]][["gg"]][c(3L, 4L), 1L] <- paste0(" ", print.object[["test"]][["gg"]][c(3L, 4L), 1L])
    print.object[["test"]][["gg"]][, 1L] <- format(print.object[["test"]][["gg"]][, 1L], justify = "left")

    print.object[["test"]][["hf"]] <- apply(print.object[["test"]][["hf"]], 2L, function(x) gsub("NA", "", x))
    print.object[["test"]][["hf"]][, -1L] <- apply(print.object[["test"]][["hf"]][, -1L], 2L, format, justify = "right")
    print.object[["test"]][["hf"]][-1L, 1L] <- paste0(" ", print.object[["test"]][["hf"]][-1L, 1L])
    print.object[["test"]][["hf"]][c(3L, 4L), 1L] <- paste0(" ", print.object[["test"]][["hf"]][c(3L, 4L), 1L])
    print.object[["test"]][["hf"]][, 1L] <- format(print.object[["test"]][["hf"]][, 1L], justify = "left")

    # Post hoc tests
    print.object[["posthoc"]][, -c(1L, 2L)] <- apply(print.object[["posthoc"]][, -c(1L, 2L)], 2L, format, justify = "right")
    print.object[["posthoc"]][-1L, 1L] <- paste0(" ", print.object[["posthoc"]][-1L, 1L])
    print.object[["posthoc"]][-1L, 2L] <- paste0(" ", print.object[["posthoc"]][-1L, 2L])
    print.object[["posthoc"]][, c(1L, 2L)] <- apply(print.object[["posthoc"]][, c(1L, 2L)], 2L, format, justify = "left")

    print.object[["descript"]][, 1L] <- paste(" ", print.object[["descript"]][, 1L])
    print.object[["epsilon"]][, 1L] <- paste(" ", print.object[["epsilon"]][, 1L])
    print.object[["test"]][["none"]][, 1L] <- paste("  ", print.object[["test"]][["none"]][, 1L])
    print.object[["test"]][["lb"]][, 1L] <- paste("  ", print.object[["test"]][["lb"]][, 1L])
    print.object[["test"]][["gg"]][, 1L] <- paste("  ", print.object[["test"]][["gg"]][, 1L])
    print.object[["test"]][["hf"]][, 1L] <- paste("  ", print.object[["test"]][["hf"]][, 1L])
    print.object[["posthoc"]][, 1L] <- paste("  ", print.object[["posthoc"]][, 1L])

    # Print effects size
    if (isTRUE(!effsize)) {

      print.object[["test"]][["none"]] <- print.object[["test"]][["none"]][, -which(colnames(print.object[["test"]][["none"]]) %in% c("et", "et.p", "om", "om.p"))]
      print.object[["test"]][["lb"]] <- print.object[["test"]][["lb"]][, -which(colnames(print.object[["test"]][["lb"]]) %in% c("et", "et.p", "om", "om.p"))]
      print.object[["test"]][["gg"]] <- print.object[["test"]][["gg"]][, -which(colnames(print.object[["test"]][["gg"]]) %in% c("et", "et.p", "om", "om.p"))]
      print.object[["test"]][["hf"]] <- print.object[["test"]][["hf"]][, -which(colnames(print.object[["test"]][["hf"]]) %in% c("et", "et.p", "om", "om.p"))]

      print.object[["posthoc"]] <- print.object[["posthoc"]][, -which(colnames(print.object[["posthoc"]]) %in% c("d", "d.low", "d.upp"))]

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Print output ####

    cat(paste0(" Repeated Measures Analysis of Variance\n\n"))

    ###
    # Print hypotheses
    if (isTRUE(hypo)) {

      # R Markdown in progress
      if (isTRUE(getOption("knitr.in.progress"))) {

        cat("  Null hypothesis        H0: mu.i = mu.j for all i and j\n",
            " Alternative hypothesis H1: mu.i != mu.j for at least one i != j \n\n")

      } else {

        cat("  Null hypothesis        H0: \u03BC\u1D62 = \u03BC\u2C7C for all i and j\n",
            " Alternative hypothesis H1: \u03BC\u1D62 \u2260 \u03BC\u2C7C for at least one i \u2260 j \n\n")

       }

    }

    ###
    # Print descriptive statistics
    if (isTRUE(descript)) {

      write.table(print.object[["descript"]], quote = FALSE, row.names = FALSE, col.names = FALSE, na = "")

      cat("\n")

    }

    ###
    # Print Box indices of sphericity
    if (isTRUE(epsilon)) {

      write.table(print.object[["epsilon"]], quote = FALSE, row.names = FALSE, col.names = FALSE, na = "")

      cat("\n")

    }

    ###
    # Print ANOVA tables

    # Sphericity correction: none
    if (isTRUE("none" %in% x$args$print)) {

      cat(paste0("  Sphericity Correction: None\n"))
      cat(paste(print.object[["test"]][["none"]][1L, ], collapse = " "), "\n")
      write.table(print.object[["test"]][["none"]][-1L, , drop = FALSE], quote = FALSE, row.names = FALSE, col.names = FALSE, na = "")

      if (isTRUE(any(x$args$print %in% c("LB", "GG", "HF")) ||  posthoc)) { cat("\n") }

    }

    # Sphericity correction: lower bound
    if (isTRUE("LB" %in% x$args$print)) {

      cat(paste0("  Sphericity Correction: Lower Bound\n"))
      cat(paste(print.object[["test"]][["lb"]][1L, ], collapse = " "), "\n")
      write.table(print.object[["test"]][["lb"]][-1L, , drop = FALSE], quote = FALSE, row.names = FALSE, col.names = FALSE, na = "")

      if (isTRUE(any(x$args$print %in% c("GG", "HF")) ||  posthoc)) { cat("\n") }

    }

    # Sphericity correction: Greenhouse-Geisser
    if (isTRUE("GG" %in% x$args$print)) {

      cat(paste0("  Sphericity Correction: Greenhouse-Geisser \n"))
      cat(paste(print.object[["test"]][["gg"]][1L, ], collapse = " "), "\n")
      write.table(print.object[["test"]][["gg"]][-1L, , drop = FALSE], quote = FALSE, row.names = FALSE, col.names = FALSE, na = "")

      if (isTRUE(any(x$args$print %in% c("HF")) ||  posthoc)) { cat("\n") }

    }

    # Sphericity correction: Huynh-Feldt
    if (isTRUE("HF" %in% x$args$print)) {

      cat(paste0("  Sphericity Correction: Huynh-Feldt \n"))
      cat(paste(print.object[["test"]][["hf"]][1L, ], collapse = " "), "\n")
      write.table(print.object[["test"]][["hf"]][-1L, , drop = FALSE], quote = FALSE, row.names = FALSE, col.names = FALSE, na = "")

      if (isTRUE(posthoc)) { cat("\n") }

    }

    ###
    # Print post-hoc test
    if (isTRUE(posthoc)) {

      cat(paste0("  Paired-Samples t-Tests for Multiple Comparison\n\n"))

      write.table(print.object[["posthoc"]], quote = FALSE, row.names = FALSE, col.names = FALSE, na = "")

      cat(paste0("\n   Adjustment for multiple testing: ", x$args$p.adj))

      cat("\n")

    }

  #_____________________________________________________________________________
  #
  # Confidence intervals -------------------------------------------------------
  }, ci = {

    #......
    # Variables to round
    print.round <- switch(x$ci,
                          mean = c("m", "sd", "low", "upp"),
                          mean.w = c("m", "sd", "se", "low", "upp"),
                          mean.diff.o = c("m", "sd", "mu", "m.diff", "low", "upp"),
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

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## No grouping ####

    if (isTRUE(is.null(x$data$group) && is.null(x$data$split))) {

      #......
      # Print names
      print.names <- switch(x$ci,
                            mean = c("Variable", "n", "nNA", "pNA", "M", "SD", "Low", "Upp"),
                            mean.w = c("Variable", "n", "nNA", "pNA", "M", "SD",  "SE", "Low", "Upp"),
                            mean.diff.o = c("Variable", "n", "nNA", "M", "SD", "Mu", "M.Diff", "Low", "Upp"),
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
                                                                                    zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

      #......
      # Percentages
      if (isTRUE(!x$ci %in% c("mean.diff.o", "mean.diff.i", "mean.diff.p", "prop.diff.i", "prop.diff.p"))) {

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
      print.object[, -col.format] <- apply(print.object[, -col.format], 2L, format, justify = "right")

      # Add blank space
      print.object[, "variable"] <- c(paste0("  ", print.object[1L, "variable"]), paste0("   ", print.object[-1L, "variable"]))
      print.object[, "variable"] <- format(c(print.object[1L, "variable"], misty::chr.trim(print.object[-1L, "variable"], side = "right")), justify = "left")

      if (isTRUE(x$ci %in% c("mean.diff.i", "prop.diff.i"))) {

        print.object[, "between"] <- c(print.object[1L, "between"], paste0(" ", print.object[-1L, "between"]))
        print.object[, "between"] <- format(c(print.object[1L, "between"], misty::chr.trim(print.object[-1L, "between"], side = "right")), justify = "left")

        # NAs
        colnames.NA <- c("m.diff", "p.diff", "low", "upp")

        print.object[, which(colnames(print.object) %in% colnames.NA)] <- apply(print.object[, which(colnames(print.object) %in% colnames.NA), drop = FALSE], 2L, function(y) gsub("NA", "  ", y))

      }

      # One Variable
      if (isTRUE(length(unique(x$result$variable)) == 1L)) {

        print.object <- print.object[, -1L]

        # First column "n"
        if (isTRUE(x$ci %in% c("mean", "mean.diff.o", "mean.diff.p", "prop.diff.p", "median", "prop", "var", "sd"))) {

          print.object[, "n"] <- c(paste0("  ", print.object[1L, "n"]), paste0("  ", print.object[-1L, "n"]))
          print.object[, "n"] <- format(c(print.object[1L, "n"], misty::chr.trim(print.object[-1L, "n"], side = "right")), justify = "right")

        } else {

          print.object[, "between"] <- c(paste0("  ", print.object[1L, "between"]), paste0("  ", print.object[-1L, "between"]))
          print.object[, "between"] <- format(c(print.object[1L, "between"], misty::chr.trim(print.object[-1L, "between"], side = "right")), justify = "left")

        }

      }

      #......
      # Print output
      cat(paste(switch(x$args$alternative,
                       two.sided = " Two-Sided",
                       less = " One-Sided",
                       greater = " One-Sided"),
                paste0(round(x$args$conf.level * 100L, digits = 2L), "%"),
                ifelse(x$ci == "mean.w", "Within-Subject Confidence Intervals:", "Confidence Interval:"),
                switch(x$ci,
                       mean = "Arithmetic Mean\n\n",
                       mean.w = "Arithmetic Mean\n\n",
                       mean.diff.o = "Difference in Means from a Population Value\n\n",
                       mean.diff.i = "Difference in Means from Independent Samples\n\n",
                       mean.diff.p = "Difference in Means from Paired Samples\n\n",
                       prop.diff.i = "Difference in Proportions from Independent Samples\n\n",
                       prop.diff.p = "Difference in Proportions from Paired Samples\n\n",
                       median = "Median\n\n",
                       prop = "Proportion\n\n",
                       var = "Variance\n\n",
                       sd = "Standard Deviation\n\n")))

      write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE)

      #......
      # Notes

      # Arithmetic mean
      if (isTRUE(x$ci == "mean")) {

        if (isTRUE(!is.null(x$args$sigma))) {

          cat(paste0("\n  Note. Known population SD: Sigma = ", round(x$args$sigma, digits = 2L), "\n"))

        }

      }

      # Arithmetic mean
      if (isTRUE(x$ci == "mean.w")) {

        if (isTRUE(x$args$adjust)) {

          cat(paste0("\n  Note. Difference-Adjusted Cousineau-Morey Intervals."))

        } else {

          cat(paste0("\n  Note. Cousineau-Morey Intervals."))

        }

      }

      # Difference in arithmetic mean from independent samples
      if (isTRUE(x$ci == "mean.diff.i")) {

        if (isTRUE(x$args$var.equal && is.null(x$args$sigma))) {

          cat(paste0("\n  Note. Equal population variance assumption"))

        }

        if (isTRUE(!is.null(x$args$sigma))) {

          if (isTRUE(length(unique(x$args$sigma)) == 1L)) {

            cat(paste0("\n  Note. Known equal population SD: Sigma = ", round(unique(x$args$sigma), digits = 2L), "\n"))

          } else if (isTRUE(length(unique(x$args$sigma)) == 2L)) {

            cat(paste0("\n  Note. Known unequal population SDs: Sigma1 = ", round(x$args$sigma[1L], digits = 2L), ", ",
                       "Sigma2 = ", round(x$args$sigma[2L], digits = 2L), "\n"))

          }

        }

      }

      # Difference in arithmetic mean from paired samples
      if (isTRUE(x$ci == "mean.diff.p")) {

        if (isTRUE(!is.null(x$args$sigma))) {

          cat(paste0("\n  Note. Known population SD of difference scores: Sigma = ", round(x$args$sigma, digits = 2L), "\n"))

        }

      }


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Grouping ####
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
                                                                                    zero.print = paste0("0.", paste(rep(0L, times = digits), collapse = ""))), NA))

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

      # Remove duplicated labels
      print.object[duplicated(paste(print.object$group, print.object$variable)) , c("group", "variable")] <- ""

      col.format <- which(colnames(print.object) %in% c("group", "variable", "between"))

      # Justify left and right
      print.object[, col.format] <- format(print.object[, col.format], justify = "left")
      print.object[, -col.format] <- apply(print.object[, -col.format], 2L, format, justify = "right")

      # Add blank space
      print.object[, "group"] <- c(paste0("  ", print.object[1L, "group"]), paste0("   ", print.object[-1L, "group"]))
      print.object[, "group"] <- format(c(print.object[1L, "group"], misty::chr.trim(print.object[-1L, "group"], side = "right")), justify = "left")

      print.object[, "variable"] <- c(paste0("  ", print.object[1L, "variable"]), paste0("   ", print.object[-1L, "variable"]))
      print.object[, "variable"] <- format(c(print.object[1L, "variable"], misty::chr.trim(print.object[-1L, "variable"], side = "right")), justify = "left")

      if (isTRUE(x$ci %in% c("mean.diff.i", "prop.diff.i"))) {

        print.object[, "between"] <- c(print.object[1L, "between"], paste0("  ", print.object[-1L, "between"]))
        print.object[, "between"] <- format(c(print.object[1L, "between"], misty::chr.trim(print.object[-1L, "between"], side = "right")), justify = "left")

        # NAs
        colnames.NA <- c("m.diff", "p.diff", "low", "upp")

        print.object[, which(colnames(print.object) %in% colnames.NA)] <- apply(print.object[, which(colnames(print.object) %in% colnames.NA), drop = FALSE], 2L, function(y) gsub("NA", "  ", y))

      }

      # Only one variable
      if (isTRUE(length(unique(x$result$variable)) == 1L)) {

        print.object <- print.object[, -2L]

      }

      #......
      # Print output
      cat(paste(switch(x$args$alternative,
                       two.sided = " Two-Sided",
                       less = " One-Sided",
                       greater = " One-Sided"),
                paste0(round(x$args$conf.level * 100L, digits = 2L), "%"), "Confidence Interval:",
                switch(x$ci,
                             mean = "Arithmetic Mean\n\n",
                             mean.diff.i = "Difference in Means from Independent Samples\n\n",
                             mean.diff.p = "Difference in Means from Paired Samples\n\n",
                             prop.diff.i = "Difference in Proportions from Independent Samples\n\n",
                             prop.diff.p = "Difference in Proportions from Paired Samples\n\n",
                             median = "Median\n\n",
                             prop = "Proportion\n\n",
                             var = "Variance\n\n",
                             sd = "Standard Deviation\n\n")))

      write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE)

      #......
      # Note

      # Difference in arithmetic mean from independent samples
      if (isTRUE(x$ci == "mean.diff.i")) {

        if (isTRUE(x$args$var.equal)) {

          cat(paste0("\n  Note. Equal population variance assumption\n"))

        }
      }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Split ####
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
                                                                                             zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

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
          print.object[[i]][, -col.format] <- apply(print.object[[i]][, -col.format], 2L, format, justify = "right")

          # Add blank
          print.object[[i]][, "variable"] <- c(paste0("   ", print.object[[i]][1L, "variable"]), paste0("    ", print.object[[i]][-1L, "variable"]))
          print.object[[i]][, "variable"] <- format(c(print.object[[i]][1L, "variable"], misty::chr.trim(print.object[[i]][-1L, "variable"], side = "right")), justify = "left")

          print.object[[i]][, "between"] <- c(paste0("   ", print.object[[i]][1L, "between"]), paste0("    ", print.object[[i]][-1L, "between"]))
          print.object[[i]][, "between"] <- format(c(print.object[[i]][1L, "between"], misty::chr.trim(print.object[[i]][-1L, "between"], side = "right")), justify = "left")

          if (isTRUE(x$ci %in% c("mean.diff.i", "prop.diff.i"))) {

            print.object[[i]][, "between"] <- c(print.object[[i]][1L, "between"], paste0("", print.object[[i]][-1L, "between"]))
            print.object[[i]][, "between"] <- format(c(print.object[[i]][1L, "between"], misty::chr.trim(print.object[[i]][-1L, "between"], side = "right")), justify = "left")

            # NAs
            colnames.NA <- c("m.diff", "p.diff", "low", "upp")

            print.object[[i]][, which(colnames(print.object[[i]]) %in% colnames.NA)] <- apply(print.object[[i]][, which(colnames(print.object[[i]]) %in% colnames.NA), drop = FALSE], 2L, function(y) gsub("NA", "  ", y))

          }

          # One Variable
          if (isTRUE(length(unique(x$result[[1L]]$variable)) == 1L)) {

            print.object[[i]] <- print.object[[i]][, -1L]

            if (isTRUE(!isTRUE(x$args$paired))) {

              print.object[[i]][, "between"] <- c(paste0("   ", print.object[[i]][1L, "between"]), paste0("   ", print.object[[i]][-1L, "between"]))
              print.object[[i]][, "between"] <- format(c(print.object[[i]][1L, "between"], misty::chr.trim(print.object[[i]][-1L, "between"], side = "right")), justify = "left")

            } else {

              print.object[[i]][, "n"] <- c(paste0("   ", print.object[[i]][1L, "n"]), paste0("   ", print.object[[i]][-1L, "n"]))
              print.object[[i]][, "n"] <- format(c(print.object[[i]][1L, "n"], misty::chr.trim(print.object[[i]][-1L, "n"], side = "right")), justify = "left")

            }

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
                                                                                        zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))
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
          print.object[[i]][duplicated(paste(print.object[[i]]$group, print.object[[i]]$variable)) , c("group", "variable")] <- ""

          col.format <- which(colnames(print.object[[i]]) %in% c("group", "variable", "between"))

          # Justify left and right
          print.object[[i]][, col.format] <- format(print.object[[i]][, col.format], justify = "left")
          print.object[[i]][, -col.format] <- apply(print.object[[i]][, -col.format], 2L, format, justify = "right")

          # Add blank space
          print.object[[i]][, "group"] <- c(paste0("   ", print.object[[i]][1L, "group"]), paste0("    ", print.object[[i]][-1L, "group"]))
          print.object[[i]][, "group"] <- format(c(print.object[[i]][1L, "group"], misty::chr.trim(print.object[[i]][-1L, "group"], side = "right")), justify = "left")

          print.object[[i]][, "variable"] <- c(paste0("   ", print.object[[i]][1L, "variable"]), paste0("    ", print.object[[i]][-1L, "variable"]))
          print.object[[i]][, "variable"] <- format(c(print.object[[i]][1L, "variable"], misty::chr.trim(print.object[[i]][-1L, "variable"], side = "right")), justify = "left")

          if (isTRUE(x$ci %in% c("mean.diff.i", "prop.diff.i"))) {

            print.object[[i]][, "between"] <- c(print.object[[i]][1L, "between"], paste0(" ", print.object[[i]][-1L, "between"]))
            print.object[[i]][, "between"] <- format(c(print.object[[i]][1L, "between"], misty::chr.trim(print.object[[i]][-1L, "between"], side = "right")), justify = "left")

            # NAs
            colnames.NA <- c("m.diff", "p.diff", "low", "upp")

            print.object[[i]][, which(colnames(print.object[[i]]) %in% colnames.NA)] <- apply(print.object[[i]][, which(colnames(print.object[[i]]) %in% colnames.NA), drop = FALSE], 2L, function(y) gsub("NA", "  ", y))

          }

          # Only one variable
          if (isTRUE(length(unique(x$result[[1L]]$variable)) == 1L)) {

            print.object[[i]] <- print.object[[i]][, -2L]

          }

        }

      }

      # Print object
      cat(paste(switch(x$args$alternative,
                       two.sided = " Two-Sided",
                       less = " One-Sided",
                       greater = " One-Sided"),
                paste0(round(x$args$conf.level * 100L, digits = 2L), "%"), "Confidence Interval:",
                switch(x$ci,
                       mean = "Arithmetic Mean\n\n",
                       mean.diff.i = "Difference in Means from Independent Samples\n\n",
                       mean.diff.p = "Difference in Means from Paired Samples\n\n",
                       prop.diff.i = "Difference in Proportions from Independent Samples\n\n",
                       prop.diff.p = "Difference in Proportions from Paired Samples\n\n",
                       median = "Median\n\n",
                       prop = "Proportion\n\n",
                       var = "Variance\n\n",
                       sd = "Standard Deviation\n\n")))

      for (i in names(print.object)) {

        cat("  Split Group:", i, "\n")

        write.table(print.object[[i]], quote = FALSE, row.names = FALSE, col.names = FALSE)

        if (isTRUE(i != names(print.object)[length(print.object)])) { cat("\n") }

      }

      #......
      # Note

      # Difference in arithmetic mean from independent samples
      if (isTRUE(x$ci == "mean.diff.i")) {

        if (isTRUE(x$args$var.equal)) {

          cat(paste0("\n  Note. Equal population variance assumption\n"))

        }
      }

    }

  #_____________________________________________________________________________
  #
  # Cohen's d ..----------------------------------------------------------------
  }, cohens.d = {

    #......
    # Variables to round
    print.round <- switch(x$sample,
                          one = c("m", "m.diff", "sd", "d", "se", "low", "upp"),
                          two = c("m", "m.diff", "sd", "d", "se", "low", "upp"),
                          paired = c("m1", "m2", "m.diff", "sd", "d", "se", "low", "upp"))

    ####################################################################################
    # Main Function

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## No grouping ####

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
                                                                                    zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

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
      print.object[, -col.format] <- apply(print.object[, -col.format], 2L, format, justify = "right")

      # Add blank space
      print.object[, "variable"] <- c(paste0("  ", print.object[1L, "variable"]), paste0("   ", print.object[-1L, "variable"]))
      print.object[, "variable"] <- format(c(print.object[1L, "variable"], misty::chr.trim(print.object[-1L, "variable"], side = "right")), justify = "left")

      if (isTRUE(x$sample == "two")) {

        print.object[, "between"] <- c(print.object[1L, "between"], paste0(" ", print.object[-1L, "between"]))
        print.object[, "between"] <- format(c(print.object[1L, "between"], misty::chr.trim(print.object[-1L, "between"], side = "right")), justify = "left")

        # NAs
        colnames.NA <- c("m.diff", "sd", "d", "se", "low", "upp")

        print.object[, which(colnames(print.object) %in% colnames.NA)] <- apply(print.object[, which(colnames(print.object) %in% colnames.NA), drop = FALSE], 2L, function(y) gsub("NA", "  ", y))

      }

      # One Variable
      if (isTRUE(length(unique(x$result$variable)) == 1L)) {

        print.object <- print.object[, -1L]

        if (isTRUE(x$sample == "two")) {

          print.object[, "between"] <- c(paste0("  ", print.object[1L, "between"]), paste0("  ", print.object[-1L, "between"]))
          print.object[, "between"] <- format(c(print.object[1L, "between"], misty::chr.trim(print.object[-1L, "between"], side = "right")), justify = "left")

        } else {

          print.object[, "n"] <- c(paste0("  ", print.object[1L, "n"]), paste0("  ", print.object[-1L, "n"]))
          print.object[, "n"] <- format(c(print.object[1L, "n"], misty::chr.trim(print.object[-1L, "n"], side = "right")), justify = "right")

        }

      }

      #......
      # Print output
      cat(paste(switch(x$sample,
                       one = paste0(" Cohen's d: One-Sample Design for \u03BC = ", round(x$args$mu, digits = 2L), " with"),
                       two = " Cohen's d: Two-Sample Design with",
                       paired = " Cohen's d: Paired-Sample Design with"),
                       switch(x$args$alternative,
                              two.sided = "Two-Sided",
                              less = "One-Sided",
                              greater = "One-Sided"),
                paste0(round(x$args$conf.level * 100L, digits = 2L), "%"), "CI\n\n"))

      write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Grouping ####
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
                                                                                    zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

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
      print.object[, -col.format] <- apply(print.object[, -col.format], 2L, format, justify = "right")

      # Add blank space
      print.object[, "group"] <- c(paste0("  ", print.object[1L, "group"]), paste0("   ", print.object[-1L, "group"]))
      print.object[, "group"] <- format(c(print.object[1L, "group"], misty::chr.trim(print.object[-1L, "group"], side = "right")), justify = "left")

      print.object[, "variable"] <- c(print.object[1L, "variable"], paste0(" ", print.object[-1L, "variable"]))
      print.object[, "variable"] <- format(c(print.object[1L, "variable"], misty::chr.trim(print.object[-1L, "variable"], side = "right")), justify = "left")

      if (isTRUE(x$sample == "two")) {

        print.object[, "between"] <- c(print.object[1L, "between"], paste0(" ", print.object[-1L, "between"]))
        print.object[, "between"] <- format(c(print.object[1L, "between"], misty::chr.trim(print.object[-1L, "between"], side = "right")), justify = "left")

        # NAs
        colnames.NA <- c("m.diff", "sd", "d", "se", "low", "upp")

        print.object[, which(colnames(print.object) %in% colnames.NA)] <- apply(print.object[, which(colnames(print.object) %in% colnames.NA), drop = FALSE], 2L, function(y) gsub("NA", "  ", y))

      }

      # Only one variable
      if (isTRUE(length(unique(x$result$variable)) == 1L)) {

        print.object <- print.object[, -2L]

      }

      #......
      # Print output
      cat(paste(switch(x$sample,
                       one = paste0(" Cohen's d: One-Sample Design for \u03BC = ", round(x$args$mu, digits = 2L), " with"),
                       two = " Cohen's d: Two-Sample Design with",
                       paired = " Cohen's d: Paired-Sample Design with"),
                       switch(x$args$alternative,
                              two.sided = "Two-Sided",
                              less = "One-Sided",
                              greater = "One-Sided"),
                       paste0(round(x$args$conf.level * 100L, digits = 2L), "%"), "CI\n\n"))

      write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Split ####
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
                                                                                             zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

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
          print.object[[i]][, -col.format] <- apply(print.object[[i]][, -col.format], 2L, format, justify = "right")

          # Add blank space
          print.object[[i]][, "variable"] <- c(paste0("  ", print.object[[i]][1L, "variable"]), paste0("   ", print.object[[i]][-1L, "variable"]))
          print.object[[i]][, "variable"] <- format(c(print.object[[i]][1L, "variable"], misty::chr.trim(print.object[[i]][-1L, "variable"], side = "right")), justify = "left")

          if (isTRUE(x$sample == "two")) {

            print.object[[i]][, "between"] <- c(print.object[[i]][1L, "between"], paste0(" ", print.object[[i]][-1L, "between"]))
            print.object[[i]][, "between"] <- format(c(print.object[[i]][1L, "between"], misty::chr.trim(print.object[[i]][-1L, "between"], side = "right")), justify = "left")

            # NAs
            colnames.NA <- c("m.diff", "sd", "d", "se", "low", "upp")

            print.object[[i]][, which(colnames(print.object[[i]]) %in% colnames.NA)] <- apply(print.object[[i]][, which(colnames(print.object[[i]]) %in% colnames.NA), drop = FALSE], 2L, function(y) gsub("NA", "  ", y))

          }

          # One Variable
          if (isTRUE(length(unique(x$result[[1L]]$variable)) == 1L)) {

            print.object[[i]] <- print.object[[i]][, -1L]

            if (isTRUE(x$sample == "two")) {

              print.object[[i]][, "between"] <- c(paste0(" ", print.object[[i]][1L, "between"]), paste0(" ", print.object[[i]][-1L, "between"]))
              print.object[[i]][, "between"] <- format(c(print.object[[i]][1L, "between"], misty::chr.trim(print.object[[i]][-1L, "between"], side = "right")), justify = "left")

            } else {

              print.object[[i]][, "n"] <- c(paste0("  ", print.object[[i]][1L, "n"]), paste0("  ", print.object[[i]][-1L, "n"]))
              print.object[[i]][, "n"] <- format(c(print.object[[i]][1L, "n"], misty::chr.trim(print.object[[i]][-1L, "n"], side = "right")), justify = "left")

            }

          }

          print.object[[i]][, 1L] <- paste0(" ", print.object[[i]][, 1L])

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
                                                                                             zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

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
          print.object[[i]][, -col.format] <- apply(print.object[[i]][, -col.format], 2L, format, justify = "right")

          # Add blank space
          print.object[[i]][, "group"] <- c(paste0("  ", print.object[[i]][1L, "group"]), paste0("   ", print.object[[i]][-1L, "group"]))
          print.object[[i]][, "group"] <- format(c(print.object[[i]][1L, "group"], misty::chr.trim(print.object[[i]][-1L, "group"], side = "right")), justify = "left")

          print.object[[i]][, "variable"] <- c(print.object[[i]][1L, "variable"], paste0(" ", print.object[[i]][-1L, "variable"]))
          print.object[[i]][, "variable"] <- format(c(print.object[[i]][1L, "variable"], misty::chr.trim(print.object[[i]][-1L, "variable"], side = "right")), justify = "left")

          if (isTRUE(x$sample == "two")) {

            print.object[[i]][, "between"] <- c(print.object[[i]][1L, "between"], paste0(" ", print.object[[i]][-1L, "between"]))
            print.object[[i]][, "between"] <- format(c(print.object[[i]][1L, "between"], misty::chr.trim(print.object[[i]][-1L, "between"], side = "right")), justify = "left")

            # NAs
            colnames.NA <- c("m.diff", "sd", "d", "se", "low", "upp")

            print.object[[i]][, which(colnames(print.object[[i]]) %in% colnames.NA)] <- apply(print.object[[i]][, which(colnames(print.object[[i]]) %in% colnames.NA), drop = FALSE], 2L, function(y) gsub("NA", "  ", y))

          }

          # Only one variable
          if (isTRUE(length(unique(x$result[[1L]]$variable)) == 1L)) {

            print.object[[i]] <- print.object[[i]][, -2L]

          }

          print.object[[i]][, 1L] <- paste0(" ", print.object[[i]][, 1L])

        }

      }

      # Print object
      cat(paste(switch(x$sample,
                       one = paste0(" Cohen's d: One-Sample Design for \u03BC = ", x$args$mu, " with"),
                       two = " Cohen's d: Two-Sample Design with",
                       paired = " Cohen's d: Paired-Sample Design with"),
                       switch(x$args$alternative,
                              two.sided = "Two-Sided",
                              less = "One-Sided",
                              greater = "One-Sided"),
                       paste0(round(x$args$conf.level * 100L, digits = 2L), "%"), "CI\n\n"))

      for (i in names(print.object)) {

        cat("  Split Group:", i, "\n")

        write.table(print.object[[i]], quote = FALSE, row.names = FALSE, col.names = FALSE)

        if (isTRUE(i != names(print.object)[length(print.object)])) { cat("\n") }

      }

    }

    #......
    # Notes

    # One-sample design
    if (isTRUE(x$sample == "one")) {

      if (isTRUE(x$args$correct)) {

        cat("\n  Note. Applying small sample correction factor\n")

      }

    }

    # Two-sample design
    else if (isTRUE(x$sample == "two")) {

      if (isTRUE(is.null(x$args$ref))) {

        if (isTRUE(x$args$weighted)) {

          if (isTRUE(x$args$correct)) {

            cat("\n  Note. SD = weighted pooled standard deviation \n        Applying small sample correction factor")

          } else {

            cat("\n  Note. SD = weighted pooled standard deviation\n")

          }

        } else {

          if (isTRUE(x$args$correct)) {

            cat("\n  Note. SD = unweighted pooled standard deviation \n        Applying small sample correction factor")

          } else {

            cat("\n  Note. SD = unweighted pooled standard deviation\n")

          }

        }

      } else {

        if (isTRUE(x$args$correct)) {

          cat(paste0("\n  Note. SD = standard deviation of the reference group: ", x$args$ref), "\n        Applying small sample correction factor")

        } else {

          cat(paste0("\n  Note. SD = standard deviation of the reference group: ", x$args$ref, "\n"))

        }

      }

    # Paired
    } else if (isTRUE(x$sample == "paired")) {

      if (isTRUE(x$args$weighted)) {

        if (isTRUE(x$args$correct)) {

          cat("\n  Note. SD = standard deviation of the difference scores\n        Applying small sample correction factor")

        } else {

          cat("\n  Note. SD = standard deviation of the difference scores\n")

        }

      } else {

        if (isTRUE(x$args$cor)) {

          if (isTRUE(x$args$correct)) {

            cat("\n  Note. SD = controlling for the correlation between measures \n        Applying small sample correction factor")

          } else {

            cat("\n  Note. SD = controlling for the correlation between measures\n")

          }

        } else {

          if (isTRUE(x$args$correct)) {

            cat("\n  Note. SD = without controlling for the correlation between measures \n        Applying small sample correction factor")

          } else {

            cat("\n  Note. SD = without controlling for the correlation between measures\n")

          }

        }

      }

    }


  #_____________________________________________________________________________
  #
  # Collinearity Diagnostics --------------------------------------------------
  }, check.collin = {

    if (isTRUE(check)) {

      #......
      # Check input 'print'
      if (isTRUE(any(!print %in% c("all", "vif", "eigen")))) {

        stop("Character string(s) in the argument 'print' does not match with \"all\", \"vif\", \"eigen\".",
             call. = FALSE)

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Arguments ####

    # Print variance inflation factor and/or eigenvalue
    if (isTRUE(length(print) == 1L && "all" %in% print)) { print <- c("vif", "eigen") }

    #-----------------------------------------------------------------------------------
    # Main Function

    cat(" Collinearity Diagnostics\n")

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Tolerance, std. error inflation factor, and variance inflation factor ####
    if (isTRUE("vif" %in% print)) {

      # Exclude variables "df" and "GVIF"
      print.object$coef <- print.object$coef[, which(!colnames(print.object$coef) %in% c("df", "GVIF"))]

      # Round
      if (isTRUE(any(class(x$model) == "lmerMod"))) {

        print.object$coef <- apply(print.object$coef, 2L, function(y) formatC(y, digits = digits, format = "f",
                                                                              zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")))

      } else if (isTRUE(any(class(x$model) == "lme"))) {

        print.object$coef[, -5L] <- apply(print.object$coef[, -5L], 2L, function(y) formatC(y, digits = digits, format = "f",
                                                                                          zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")))

        print.object$coef[, 5L] <- formatC(print.object$coef[, 5L], digits = p.digits, format = "f",
                                          zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

      } else {

        print.object$coef[, -4L] <- apply(print.object$coef[, -4L], 2L, function(y) formatC(y, digits = digits, format = "f",
                                                                                          zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")))

        print.object$coef[, 4L] <- formatC(print.object$coef[, 4L], digits = p.digits, format = "f",
                                          zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

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
      if (isTRUE(any(x$result$vif$df > 1L))) {

        cat("\n   Note. Generalized SIF/VIF are computed for terms with more than 1 df\n")

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Eigenvalue, condition index, and variance proportions ####
    if (isTRUE("eigen" %in% print)) {

      # Round
      print.object$eigen[, -1L] <- apply(print.object$eigen[, -1L], 2L, function(y) formatC(y, digits = digits, format = "f",
                                                                                            zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")))

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

  #_____________________________________________________________________________
  #
  # Pearson's Contingency Coefficient ------------------------------------------
  }, cor.cont = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Data and Arguments ####

    # Print triangular
    tri <- ifelse(all(c("both", "lower", "upper") %in% tri), "lower", tri)

    ####################################################################################
    # Main Function

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Two variables ####
    if (isTRUE(is.null(dim(print.object)))) {

      print.object <- cbind("  Estimate: ", ifelse(!is.na(print.object), formatC(print.object, digits = digits, format = "f",
                                                                                 zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), print.object))

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## More than two variables ####
    } else {

      # Format contingency coefficients
      print.object <- formatC(print.object, digits = digits, format = "f",
                              zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

      # Lower and/or upper triangular
      switch(tri, "lower" = {

        print.object[upper.tri(print.object)] <- ""

      }, "upper" = {

        print.object[lower.tri(print.object)] <- ""

      })

      # Set diagonal to "
      diag(print.object) <- ""

      # Format
      row.names(print.object) <- paste(" ", row.names(print.object))

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Output ####

    if (isTRUE(is.null(dim(x$result)))) {

      if (isTRUE(x$args$adjust)) {

        cat(" Adjusted Contingency Coefficient\n\n")

      } else {

        cat(" Contingency Coefficient\n\n")

      }

    } else {

      if (isTRUE(x$args$adjust)) {

        cat(" Adjusted Contingency Coefficient Matrix\n\n")

      } else {

        cat(" Contingency Coefficient Matrix\n\n")

      }

    }

    if (isTRUE(is.null(dim(x$result)))) {

      write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE)

    } else {

      print(print.object, quote = FALSE, right = TRUE)

    }

  #_____________________________________________________________________________
  #
  # Correlation Matrix with Statistical Significance Testing -------------------
  }, cor.matrix = {

    if (isTRUE(check)) {

      #......
      # Check input 'print'
      if (isTRUE(any(!print %in% c("all", "cor", "stat", "ddf", "n", "p")))) {

        stop("Character string(s) in the argument 'print' does not match with \"all\", \"cor\",  \"stat\",  \"df\", \"n\", or \"p\".",
             call. = FALSE)

      }

    }

    # R Markdown in progress
    if (isTRUE(getOption("knitr.in.progress"))) {

      x$args$sig <- FALSE

    }

    ####################################################################################
    # Main Function

    #--------------------------------------------------
    # Two variables
    if (isTRUE(ncol(x$data[, grep(".group", colnames(x$data), invert = TRUE)]) == 2L)) {

      # Grouping variable
      if (isTRUE(".group" %in% colnames(x$data))) {

        switch (x$args$method,
                'pearson' = {

                  print.object <- data.frame(Group = c(sort(unique(x$data$.group))[1L], sort(unique(x$data$.group))[2L]),
                                             x = colnames(print.object$cor)[1L],
                                             y = colnames(print.object$cor)[2L],
                                             r = c(print.object$cor[2L, 1L], print.object$cor[1L, 2L]),
                                             n = c(print.object$n[2L, 1L], print.object$n[1L, 2L]),
                                             t = c(print.object$stat[2L, 1L], print.object$stat[1L, 2L]),
                                             df = c(print.object$df[2L, 1L], print.object$df[1L, 2L]),
                                             pval = c(print.object$p[2L, 1L], print.object$p[1L, 2L]))

                }, 'spearman' = {

                  print.object <- data.frame(Group = c(sort(unique(x$data$.group))[1L], sort(unique(x$data$.group))[2L]),
                                             x = colnames(print.object$cor)[1L],
                                             y = colnames(print.object$cor)[2L],
                                             r = c(print.object$cor[2L, 1L], print.object$cor[1L, 2L]),
                                             n = c(print.object$n[2L, 1L], print.object$n[1L, 2L]),
                                             S = c(print.object$stat[2L, 1L], print.object$stat[1L, 2L]),
                                             pval = c(print.object$p[2L, 1L], print.object$p[1L, 2L]))

                }, 'kendall-b' = {

                  print.object <- data.frame(Group = c(sort(unique(x$data$.group))[1L], sort(unique(x$data$.group))[2L]),
                                             x = colnames(print.object$cor)[1L],
                                             y = colnames(print.object$cor)[2L],
                                             tau = c(print.object$cor[2L, 1L], print.object$cor[1L, 2L]),
                                             n = c(print.object$n[2L, 1L], print.object$n[1L, 2L]),
                                             z = c(print.object$stat[2L, 1L], print.object$stat[1L, 2L]),
                                             pval = c(print.object$p[2L, 1L], print.object$p[1L, 2L]))


                }, 'kendall-c' = {

                  print.object <- data.frame(Group = c(sort(unique(x$data$.group))[1L], sort(unique(x$data$.group))[2L]),
                                             x = colnames(print.object$cor)[1L],
                                             y = colnames(print.object$cor)[2L],
                                             tau = c(print.object$cor[2L, 1L], print.object$cor[1L, 2L]),
                                             n = c(print.object$n[2L, 1L], print.object$n[1L, 2L]),
                                             z = c(print.object$stat[2L, 1L], print.object$stat[1L, 2L]),
                                             pval = c(print.object$p[2L, 1L], print.object$p[1L, 2L]))

      # No grouping variable
      })} else {

        switch (x$args$method,
                'pearson' = {

                  print.object <- data.frame(x = colnames(print.object$cor)[1L],
                                             y = colnames(print.object$cor)[2L],
                                             r = print.object$cor[2L, 1L],
                                             n = print.object$n[2L, 1L],
                                             t = print.object$stat[2L, 1L],
                                             df = print.object$df[2L, 1L],
                                             pval = print.object$p[2L, 1L])

                }, 'spearman' = {

                  print.object <- data.frame(x = colnames(print.object$cor)[1L],
                                             y = colnames(print.object$cor)[2L],
                                             r = print.object$cor[2L, 1L],
                                             n = print.object$n[2L, 1L],
                                             S = print.object$stat[2L, 1L],
                                             pval = print.object$p[2L, 1L])

                }, 'kendall-b' = {

                  print.object <- data.frame(x = colnames(print.object$cor)[1L],
                                             y = colnames(print.object$cor)[2L],
                                             tau = print.object$cor[2L, 1L],
                                             z = print.object$stat[2L, 1L],
                                             pval = print.object$p[2L, 1L])


                }, 'kendall-c' = {

                  print.object <- data.frame(x = colnames(print.object$cor)[1L],
                                             y = colnames(print.object$cor)[2L],
                                             tau = print.object$cor[2L, 1L],
                                             n = print.object$n[2L, 1L],
                                             z = print.object$stat[2L, 1L],
                                             pval = print.object$p[2L, 1L])

                })
        }

        #.....................................
        # Grouping variable
        if (isTRUE(".group" %in% colnames(x$data))) {

          # Round
          print.object[, !colnames(print.object) %in% c("Group", "n", "df", "pval")] <- vapply(print.object[, !colnames(print.object) %in% c("Group", "n", "df", "pval")],
                                                                                               formatC, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"), FUN.VALUE = character(2L))

          # Add blank
          print.object[, 1L] <- sapply(format(c("Group", as.character(print.object[, 1L])), justify = "right"), function(y) paste("", y, collapse = ""))[-1L]

        } else {

          # Round
          print.object[, !colnames(print.object) %in% c("n", "df", "pval")] <- vapply(print.object[, !colnames(print.object) %in% c("n", "df", "pval")],
                                                                                      formatC, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"), FUN.VALUE = character(1L))

          # Add blank
          print.object[, 1L] <- paste("", format(as.character(print.object[, 1L]), justify = "right"), collapse = "")

        }

        # Round p-values
        print.object[, "pval"] <- formatC(print.object[, "pval"], digits = p.digits, format = "f",
                                          zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

        #-----------------------------------------
        # Print

        switch(x$args$method, 'pearson' = {

                 cat(" Pearson Product-Moment Correlation Coefficient\n\n")

               }, 'spearman' = {

                 cat(" Spearman's Rank-Order Correlation Coefficient\n\n")

               }, 'kendall-b' = {

                 cat(" Kendall's Tau-b Correlation Coefficient\n\n")

               }, 'kendall-c' = {

                 cat(" Kendall-Stuart's Tau-c Correlation Coefficient\n\n")

               })

        # Print object
        print(print.object, quote = FALSE, right = TRUE, row.names = FALSE)

    #--------------------------------------------------
    # More than two variables
    } else {

      #........................................
      # Round and format

      print.object$cor <- formatC(print.object$cor, digits = digits, format = "f",
                                  zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

      print.object$n <- formatC(print.object$n, zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

      print.object$stat <- formatC(print.object$stat, digits = digits, format = "f",
                                   zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

      if (isTRUE(x$args$method == "pearson")) {

        print.object$df <- formatC(print.object$df, digits = 0L, format = "f")

      }

      print.object$p <- formatC(print.object$p, digits = p.digits, format = "f",
                                zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

      diag(print.object$cor) <- ""
      diag(print.object$n) <- ""
      diag(print.object$stat) <- ""
      diag(print.object$df) <- ""
      diag(print.object$p) <- ""

      #........................................
      # Lower and/or upper triangular

      if (isTRUE(tri == "lower")) {

        print.object$cor[upper.tri(print.object$cor)] <- ""
        print.object$n[upper.tri(print.object$n)] <- ""
        print.object$stat[upper.tri(print.object$stat)] <- ""
        print.object$df[upper.tri(print.object$df)] <- ""
        print.object$p[upper.tri(print.object$p)] <- ""

      }

      if (isTRUE(tri == "upper")) {

        print.object$cor[lower.tri(print.object$cor)] <- ""
        print.object$n[lower.tri(print.object$n)] <- ""
        print.object$stat[lower.tri(print.object$stat)] <- ""
        print.object$df[lower.tri(print.object$df)] <- ""
        print.object$p[lower.tri(print.object$p)] <- ""

      }

      #........................................
      # Row names

      if (isTRUE(!is.null(row.names(print.object$cor)))) {

        row.names(print.object$cor) <- paste0("  ", row.names(print.object$cor))
        row.names(print.object$n) <- paste0("  ", row.names(print.object$n))
        row.names(print.object$stat) <- paste0("  ", row.names(print.object$stat))
        row.names(print.object$df) <- paste0("  ", row.names(print.object$df))
        row.names(print.object$p) <- paste0("  ", row.names(print.object$p))

      }

      print.object$cor <- apply(print.object$cor, 2L, function(y) format(y, justify = "right"))
      print.object$n <- apply(print.object$n, 2L, function(y) format(y, justify = "right"))
      print.object$stat <- apply(print.object$stat, 2L, function(y) format(y, justify = "right"))
      print.object$df <- apply(print.object$df, 2L, function(y) format(y, justify = "right"))
      print.object$p <- apply(print.object$p, 2L, function(y) format(y, justify = "right"))

      #........................................
      # Statistically significant correlation coefficients in boldface

      if (isTRUE(x$args$sig)) {

        numbers <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
        unicode <- c("\U1D7CE", "\U1D7CF", "\U1D7D0", "\U1D7EF", "\U1D7F0", "\U1D7F1", "\U1D7F2", "\U1D7F3", "\U1D7F4", "\U1D7F5")

        print.object$cor[lower.tri(print.object$cor)][which(x$result$p[lower.tri(x$result$p)] <= x$args$alpha)] <- na.omit(misty::chr.gsub(numbers, unicode, print.object$cor[lower.tri(print.object$cor)][which(x$result$p[lower.tri(x$result$p)] <= x$args$alpha)]))
        print.object$cor[upper.tri(print.object$cor)][which(x$result$p[upper.tri(x$result$p)] <= x$args$alpha)] <- misty::chr.gsub(numbers, unicode, print.object$cor[upper.tri(print.object$cor)][which(x$result$p[upper.tri(x$result$p)] <= x$args$alpha)])

      }

      #------------------------------------
      # Print

      #........................
      # Correlation coefficient

      if (isTRUE("cor" %in% print)) {

        switch(x$args$method, "pearson" = {

          cat(" Pearson Product-Moment Correlation Coefficient\n\n")

        }, "spearman" = {

          cat(" Spearman's Rank-Order Correlation Coefficient\n\n")

        }, "kendall-b" = {

          cat(" Kendall's Tau-b Correlation Coefficient\n\n")

        }, "kendall-c" = {

          cat(" Kendall-Stuart's Tau-c Correlation Coefficient\n\n")

        })

        print(print.object$cor, quote = FALSE, right = TRUE, max = 99999L)

      }

      #........................
      # Sample size

      if (isTRUE("n" %in% print & attr(x$data, "missing") & isTRUE(!x$args$na.omit))) {

        if (isTRUE("cor" %in% print)) { cat("\n") }

          cat(" Sample Size\n\n")
          print(print.object$n, quote = FALSE, right = TRUE, max = 99999L)

      }

      #........................
      # Test statistic

      if (isTRUE("stat" %in% print)) {

        if (isTRUE(any(c("cor", "n") %in% print))) { cat("\n") }

        if (isTRUE(x$args$method == "pearson")) {

          cat(" Test Statistic (t value)\n\n")

        } else if (isTRUE(x$args$method == "spearman")) {

          cat(" Test Statistic (S value)\n\n")

        } else {

          cat(" Test Statistic (z value)\n\n")

        }

        print(print.object$stat, quote = FALSE, right = TRUE, max = 99999L)

      }

      #........................
      # Degrees of freedom

      if (isTRUE("df" %in% print & x$args$method == "pearson")) {

        if (isTRUE(any(c("cor", "n", "stat") %in% print))) { cat("\n") }

        cat(" Degrees of Freedom (df) \n\n")

        print(print.object$df, quote = FALSE, right = TRUE, max = 99999L)

      }

      #........................
      # p.values

      if (isTRUE("p" %in% print)) {

        if (x$args$method == "kendall-c") {

          if (isTRUE(any(c("cor", "n", "stat") %in% print))) { cat("\n") }

        } else {

          if (isTRUE(any(c("cor", "n", "stat", "df") %in% print))) { cat("\n") }

        }

        cat(" Significance Value (p-value)\n\n")
        print(print.object$p, quote = FALSE, right = TRUE, max = 99999L)
        cat(paste0("\n  Adjustment for multiple testing: ", x$args$p.adj, "\n"))

      }

      #........................
      # Note

      # Sample size
      cat(paste0("\n Note. n = ", ifelse(isTRUE(x$args$na.omit || !attr(x$data, "missing")), na.omit(unique(c(x$result$n))),
                                         ifelse(length(unique(range(c(x$result$n), na.rm = TRUE))) == 1, unique(range(c(x$result$n), na.rm = TRUE)),
                                                                                                                paste(range(c(x$result$n), na.rm = TRUE), collapse = "-"))),
                                  ifelse(isTRUE(attr(x$data, "missing")),
                                         ifelse(isTRUE(x$args$na.omit), ", Listwise deletion\n", ", Pairwise deletion\n"), ", No missing data\n")))

      # Lower and upper triangular
      if (isTRUE(".group" %in% colnames(x$data))) {

        cat(paste0("       Lower triangular: ", sort(unique(x$data$.group))[1L], ", Upper triangular: ", sort(unique(x$data$.group))[2L]), "\n")

      }


      # Statistical significance
      if (isTRUE(x$args$sig)) {

        cat(paste0("       Statistically significant coefficients at \U03B1 = ", signif(x$args$alpha, digits = 2L), " are boldface\n"))

      }

    }

  #_____________________________________________________________________________
  #
  # Cramer's V -----------------------------------------------------------------
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
                                                    zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")))

    #........................................
    # More than two variables
    } else {

      # Format contingency coefficients
      print.object <- formatC(print.object, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

      # Lower and/or upper triangular
      switch(tri, "lower" = {

        print.object[upper.tri(print.object)] <- ""

      }, "upper" = {

        print.object[lower.tri(print.object)] <- ""

      })

      # Diagonal
      diag(print.object) <- ""

      # Format
      row.names(print.object) <- paste(" ", row.names(print.object))

    }

    #-----------------------------------------------------------------------------------
    # Output

    if (isTRUE(is.null(dim(x$result)))) {

      if (isTRUE(x$args$correct)) {

        cat(" Bias-Corrected Cramer's V\n\n")

      } else {

        cat(" Cramer's V\n\n")

      }

    } else {

      if (isTRUE(x$args$correct)) {

        cat(" Bias-Corrected Cramer's V Matrix\n\n")

      } else {

        cat(" Cramer's V Matrix\n\n")

      }

    }

    if (isTRUE(is.null(dim(x$result)))) {

      write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE)

    } else {

      print(print.object, quote = FALSE, right = TRUE)

    }

  #_____________________________________________________________________________
  #
  # Cross Tabulation -----------------------------------------------------------
  }, crosstab = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Input Check ####

    if (isTRUE(check)) {

      # Check input print
      if (isTRUE(any(!print %in% c("no", "all", "row", "col", "total")))) { stop("Character string(s) in the argument 'print' does not match with \"no\", \"all\", \"row\", \"col\" or \"total\".", call. = FALSE) }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Result table ####

    restab <- print.object$crosstab

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Two-Dimensional Matrix ####

    if (isTRUE(ncol(x$data) == 2L)) {

      #...................
      ### Frequencies and percentages ####

      # No absolute frequencies
      if (!isTRUE(freq)) { restab <- restab[-grep("Freq", restab[, 2L]), ] }

      # No percentages
      if (isTRUE(any(print == "no"))) { restab <- restab[-grep(" %", restab[, 2L]), -2L]

      # Selected percentages
      } else {

        # No row-wise percentages
        if (isTRUE(!"row" %in% print)) { restab <- restab[-grep("Row %", restab[, 2L]), ] }

        # No col-wise percentages
        if (isTRUE(!"col" %in% print)) { restab <- restab[-grep("Col %", restab[, 2L]), ] }

        # No total percentages
        if (isTRUE(!"total" %in% print)) { restab <- restab[-grep("Tot %", restab[, 2L]), ] }

      }

      #...................
      ### Format ####

      #### Frequencies only ####
      if (isTRUE(any(print == "no"))) {

        # Variable names
        restab <- rbind(c("", colnames(x$data)[2L], rep("", times = (ncol(restab) - 2L))),
                        c(colnames(x$data)[1L], colnames(restab)[-c(1L, ncol(restab))], "Total"),
                        restab)

        # Format
        restab[2L, 1L] <- paste0("  ", restab[2L, 1L])
        restab[-c(1L, 2L), 1L] <- paste0("   ", restab[-c(1L, 2L), 1L])

        # Justify
        restab[-2L, 1L] <- format(restab[-2L, 1L], justify = "right")
        restab[, 1L] <- format(restab[, 1L])

        restab[-1L, -1L] <- apply(restab[-1L, -1L], 2L, function(y) format(y, justify = "right"))

        restab[-1L, 2L] <- paste0("  ", restab[-1L, 2L])

      #### Frequencies and Percentages ####
      } else {

        # Variable names
        restab <- rbind(c("", "", colnames(x$data)[2L], rep("", times = (ncol(restab) - 3L))),
                         c(colnames(x$data)[1L], colnames(restab)[-c(1L, ncol(restab))], "Total"),
                         restab)



        # Format percentages
        restab[grep("%", restab[, 2L]), -c(1L:2L)] <- apply(restab[grep("%", restab[, 2L]), -c(1L:2L)], 2L, function(y) paste0(format(formatC(as.numeric(y), digits = digits, format = "f",
                                                                                                                                      zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), justify = "right"), "%"))

        # Justify right and left
        restab[, 1L] <- format(restab[, 1L], justify = "right")

        restab[2L, 1L] <- paste0("  ", sub("^\\s+", "", restab[2L, 1L]), paste0(rep(" ", times = max(nchar(restab[, 1L])) - nchar(colnames(x$data)[1L])), collapse = ""), " ")
        restab[-2L, 1L] <- paste0("   ", restab[-2L, 1L])

        restab[, 2L] <- format(restab[, 2L], justify = "left")

        restab[-1L, -c(1L:2L)] <- apply(restab[-1L, -c(1L:2L)], 2L, format, justify = "right")

        restab[-1L, 3L] <- paste0("  ", restab[-1L, 3L])

      }

      #...................
      ### Output table not split ####
      if (!isTRUE(split) || all(print == "no")) {

        # Remove duplicated row labels
        restab[, 1L] <- ifelse(duplicated(restab[, 1L]), paste(rep(" ", times = unique(nchar(restab[, 1L]))), collapse = ""), restab[, 1L])

        cat(" Cross Tabulation\n\n")

        # Print results
        write.table(restab, col.names = FALSE, row.names = FALSE, quote = FALSE)

      #...................
      ### Output table split ####
      } else {

        cat(" Cross Tabulation\n\n")

        #### Frequencies ####
        if (isTRUE(freq)) {

          # Select absolute frequencies
          restab.abs <- restab[-grep("%", restab[, 2L]), -2L]

          restab.abs[-1L, -1L] <- apply(restab.abs[-1L, -1L], 2L, function(y) formatC(sub("^\\s+", "", y), format = "f"))

          restab.abs[-1L, 2L] <- paste0("  ", restab.abs[-1L, 2L])

          restab.abs[, 1L] <- paste0("  ", restab.abs[, 1L])

          cat("  Frequencies\n")
          write.table(restab.abs, col.names = FALSE, row.names = FALSE, quote = FALSE)

        }

        #### Row-wise percentages ####
        if (isTRUE("row" %in% print)) {

          if (isTRUE(freq)) { cat("\n") }

          # Select row-wise percentage
          restab.row <- rbind(restab[1L:2L, -2L], restab[grep("Row", restab[, 2L]), -2L])

          # Format
          if (length(grep("NA", restab.row[, 2L])) > 0L) {

            restab.row[grep("NA", restab.row[, 2L]), ncol(restab.row)] <- paste(paste(rep(" ", times = unique(nchar(restab.row[grep("NA", restab.row[, 2L]), ncol(restab.row)])) - 3L), collapse = ""), "NA", collapse = "")

          }

          restab.row[, ncol(restab.row)] <- format(restab.row[, ncol(restab.row)], justify = "right")

          restab.row[-1L, -1L] <- apply(restab.row[-1L, -1L], 2L, function(y) formatC(sub("^\\s+", "", y), format = "f"))

          restab.row[-1L, 2L] <- paste0("  ", restab.row[-1L, 2L])

          restab.row[, 1L] <- paste0("  ", restab.row[, 1L])

          cat("  Row-Wise Percentages\n")
          write.table(restab.row, col.names = FALSE, row.names = FALSE, quote = FALSE)

        }

        #### Column-wise percentages ####
        if (isTRUE("col" %in% print)) {

          if (isTRUE(freq) || isTRUE("row" %in% print)) { cat("\n") }

          # Select column-wise percentage
          restab.col <- rbind(restab[1L:2L, -2L], restab[grep("Col", restab[, 2L]), -2L])

          # Format
          if (length(grep("NA", restab.col[3L, ])) > 0L) {

            restab.col[, grep("NA", restab.col[3L, ])][nrow(restab.col)] <- paste(paste(rep(" ", times = unique(nchar(restab.col[, grep("NA", restab.col[3, ])][nrow(restab.col)]) - 3L)), collapse = ""), "NA", collapse = "")

          }

          restab.col[nrow(restab.col), which(apply(restab.col, 2L, function(y) length(grep("NA%", y)) != 0L))] <- "NA%"

          restab.col[-1L, ] <- apply(restab.col[-1L, ], 2L, function(y) formatC(sub("^\\s+", "", y), format = "f"))

          restab.col[, 1L] <- format(restab.col[, 1L])

          restab.col[-1L, 2L] <- paste0("  ", restab.col[-1L, 2L])

          restab.col[, 1L] <- paste0("   ", restab.col[, 1L])

          cat("  Column-Wise Percentages\n")
          write.table(restab.col, col.names = FALSE, row.names = FALSE, quote = FALSE)

        }

        #### Total percentages ####
        if (isTRUE("total" %in% print)) {

          if (isTRUE(freq) || isTRUE("row" %in% print) || isTRUE("col" %in% print)) { cat("\n") }

          # Select total percentage
          restab.total <- rbind(restab[1L:2L, -2L], restab[grep("Tot", restab[, 2L]), -2L])

          # Format
          restab.total[-1L, -1L] <- apply(restab.total[-1L, -1L], 2L, function(y) formatC(sub("^\\s+", "", y), format = "f"))

          restab.total[, ncol(restab.total)] <- format(restab.total[, ncol(restab.total)], justify = "right")

          restab.total[-1L, ] <- apply(restab.total[-1L, ], 2L, format)

          restab.total[-1L, 2L] <- paste0(" ", restab.total[-1L, 2L])

          restab.total[, 1L] <- paste0(" ", restab.total[, 1L])

          cat("  Total Percentages\n")
          write.table(restab.total, col.names = FALSE, row.names = FALSE, quote = FALSE)

        }

      }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Three-Dimensional Matrix ####

    } else if (isTRUE(ncol(x$data) == 3L)) {

      #...................
      ### Frequencies and percentages ####

      # No absolute frequencies
      if (!isTRUE(freq)) { restab <- restab[-grep("Freq", restab[, 3L]), ] }

      # No percentages
      if (isTRUE(any(print == "no"))) { restab <- restab[-grep(" %", restab[, 3L]), -3L]

      # Selected percentages
      } else {

        # No row-wise percentages
        if (isTRUE(!"row" %in% print)) { restab <- restab[-grep("Row %", restab[, 3L]), ] }

        # No col-wise percentages
        if (isTRUE(!"col" %in% print)) { restab <- restab[-grep("Col %", restab[, 3L]), ] }

        # No total percentages
        if (isTRUE(!"total" %in% print)) { restab <- restab[-grep("Tot %", restab[, 3L]), ] }

      }

      #...................
      ### Format ####

      #### Frequencies only ####
      if (isTRUE(any(print == "no"))) {

        ##### Variable names

        restab <- rbind(c("", "", colnames(x$data)[3L], rep("", times = (ncol(restab) - 3L))),
                        c(colnames(x$data)[1L], colnames(x$data)[2L], colnames(restab)[-c(1L, 2L)]),
                        restab)

        # Justify right
        restab[-1L, ] <- apply(restab[-1L, ], 2L, function(y) format(y, justify = "right"))

        ##### First Variable

        # Length of first variable smaller than the maximum length in the column
        if (isTRUE(nchar(colnames(x$data)[1L]) < max(nchar(restab[, 1L])))) {

          restab[2L, 1L] <- paste(sub("^\\s+", " ", restab[2L, 1L]), paste(rep(" ", times = max(nchar(restab[, 1L])) - nchar(colnames(x$data)[1L]) - 1L) , collapse = ""))

        # Length of first variable equal than the maximum length in the column
        } else if (isTRUE(nchar(colnames(x$data)[1L]) == max(nchar(restab[, 1L])))) {

          restab[2L, 1L] <- paste(sub("^\\s+", " ", restab[2L, 1L]), paste(rep(" ", times = max(nchar(restab[, 1L])) - nchar(colnames(x$data)[1L])) , collapse = ""))

        }

        ##### Second Variable

        # Length of second variable smaller than the maximum length in the column
        if (isTRUE(nchar(colnames(x$data)[2L]) < max(nchar(restab[, 2L])))) {

          restab[2L, 2L] <- paste(sub("^\\s+", " ", restab[2L, 2L]), paste(rep(" ", times = max(nchar(restab[, 2L])) - nchar(colnames(x$data)[2L]) - 1L), collapse = ""))

        # Length of second variable equal than the maximum length in the column
        } else if (isTRUE(nchar(colnames(x$data)[2L]) == max(nchar(restab[, 2L])))) {

          restab[2L, 2L] <- paste(sub("^\\s+", " ", restab[2L, 2L]), paste(rep(" ", times = max(nchar(restab[, 2L])) - nchar(colnames(x$data)[2L])), collapse = ""))

        }

        # Format
        restab[, 1L:2L] <- apply(restab[, 1L:2L], 2L, format)

        restab[-2L, 1L] <- paste0(" ", restab[-2L, 1L])
        restab[-2L, 2L] <- paste0(" ", restab[-2L, 2L])
        restab[-1L, 3L] <- paste0(" ", restab[-1L, 3L])

        restab[, 1L:2L] <- apply(restab[, 1L:2L], 2L, format)

        restab[, 1L] <- paste0(" ", restab[, 1L])

      #### Frequencies and Percentages ####
      } else {

        ##### Variable names

        restab <- rbind(c(rep("", times = 3L), colnames(x$data)[3L], rep("", times = (ncol(restab) - 4L))),
                        c(colnames(x$data)[1L], colnames(x$data)[2L], "", colnames(restab)[-c(1L:3L)]),
                        restab)

        # Format percentages
        restab[grep("%", restab[, 3L]), -c(1L:3L)] <- apply(restab[grep("%", restab[, 3L]), -c(1L:3L)], 2L, function(y) paste0(format(formatC(as.numeric(y), digits = digits, format = "f",
                                                                                                                                      zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), justify = "right"), "%"))
        # Format variable names
        restab[2L, 1L] <- format(restab[2L, 1L], justify = "left", width = max(nchar(restab[, 1L]), na.rm = TRUE))
        restab[2L, 2L] <- format(restab[2L, 2L], justify = "left", width = max(nchar(restab[, 2L]), na.rm = TRUE))

        # Format values
        restab[-2L, 1L] <- format(restab[-2L, 1L], justify = "right", width = max(nchar(restab[, 1L]), na.rm = TRUE))
        restab[-2L, 2L] <- format(restab[-2L, 2L], justify = "right", width = max(nchar(restab[, 2L]), na.rm = TRUE))

        # Justify right
        restab[-1L, -c(1L:3L)] <- apply(restab[-1L, -c(1L:3L)], 2L, function(y) format(y, justify = "right"))

        restab[, 3L] <- format(restab[, 3L], justify = "left")

        # First variable
        restab[-2L, 1L] <- paste0("   ", restab[-2L, 1L])
        restab[2L, 1L] <- paste0("  ", restab[2L, 1L], " ")

        # Second variable
        restab[-2L, 2L] <- paste0(" ", restab[-2L, 2L])
        restab[2L, 2L] <- paste0(restab[2L, 2L], " ")

        # Third variable
        restab[-1L, 4L] <- paste0(" ", restab[-1L, 4L])

      }

      #...................
      ### Output ####

      cat(" Cross Tabulation\n\n")

      #...................
      ### Output table not split ####
      if (!isTRUE(split) || all(print == "no")) {

        # Remove duplicated row labels
        restab[-c(1L:2L), 2L] <- unlist(tapply(restab[-c(1L:2L), 2L], restab[-c(1L:2L), 1L], function(y) ifelse(duplicated(y), paste0(rep(" ", times = unique(nchar(restab[, 2L]))), collapse = ""), restab[-c(1L:2L), 2L])))
        restab[, 1L] <- ifelse(duplicated(restab[, 1L]), paste(rep(" ", times = unique(nchar(restab[, 1L]))), collapse = ""), restab[, 1L])

        # Print results
        write.table(restab, col.names = FALSE, row.names = FALSE, quote = FALSE)

      #...................
      ### Output table split ####
      } else {

        if (isTRUE(freq)) {

          #### Frequencies ####
          restab.abs <- restab[-grep("%", restab[, 3L]), -3L]

          restab.abs[-1L, -1L] <- apply(restab.abs[-1L, -1L], 2L, function(y) formatC(sub("^\\s+", "", y), format = "f"))

          restab.abs <- rbind(c(paste(rep(" ", times = max(nchar(restab.abs[, 1L]))), collapse = ""),
                                paste(rep(" ", times = max(nchar(restab.abs[, 2L]))), collapse = ""),
                                colnames(x$data)[3L], rep("", times = ncol(restab.abs) - 3L)),
                              restab.abs[-1L, ])

          restab.abs[-1L, 3L] <- paste0(" ",restab.abs[-1L, 3L])
          restab.abs[1L, 3L] <- paste0(restab.abs[1L, 3L], " ")

          restab.abs[, 1L] <- paste0(" ", restab.abs[, 1L])

          # Remove duplicated row labels
          restab.abs[, 1L] <- ifelse(duplicated(restab.abs[, 1L]), paste(rep(" ", times = unique(nchar(restab.abs[, 1L]))), collapse = ""), restab.abs[, 1L])

          # Output
          cat("  Frequencies\n")
          write.table(restab.abs, col.names = FALSE, row.names = FALSE, quote = FALSE)

        }

        #### Row-wise percentages ####
        if (isTRUE("row" %in% print)) {

          if (isTRUE(freq)) { cat("\n") }

          restab.row <- rbind(restab[1L:2L, -3L], restab[grep("Row", restab[, 3L]), -3L])

          if (length(grep("NA", restab.row[, 3L])) > 0L) {

            restab.row[grep("NA", restab.row[, 3L]), ncol(restab.row)] <- paste(paste(rep(" ", times = unique(nchar(restab.row[grep("NA", restab.row[, 3L]), ncol(restab.row)])) - 3L), collapse = ""), "NA", collapse = "")

          }

          restab.row[, ncol(restab.row)] <- format(restab.row[, ncol(restab.row)], justify = "right")

          restab.row[-1L, -1L] <- apply(restab.row[-1L, -1L], 2L, function(y) formatC(sub("^\\s+", "", y), format = "f"))

          restab.row <- rbind(c(paste(rep(" ", times = max(nchar(restab.row[, 1L]))), collapse = ""),
                                paste(rep(" ", times = max(nchar(restab.row[, 2L]))), collapse = ""), colnames(x$data)[3L], rep("", times = ncol(restab.row) - 3L)),
                              restab.row[-1L, ])

          restab.row[1L, 3L] <- format(restab.row[1L, 3L], justify = "left", width = max(nchar(restab.row[, 3L])))

          restab.row[-1L, 3L] <- paste0(" ",restab.row[-1L, 3L])
          restab.row[1L, 3L] <- paste0(restab.row[1L, 3L], " ")

          restab.row[, 1L] <- paste0(" ", restab.row[, 1L])

          # Remove duplicated row labels
          restab.row[, 1L] <- ifelse(duplicated(restab.row[, 1L]), paste(rep(" ", times = unique(nchar(restab.row[, 1L]))), collapse = ""), restab.row[, 1L])

          # Output
          cat("  Row-Wise Percentages\n")
          write.table(restab.row, col.names = FALSE, row.names = FALSE, quote = FALSE)

        }

        #### Column-wise percentages ####
        if (isTRUE("col" %in% print)) {

          if (isTRUE(freq) || isTRUE("row" %in% print)) { cat("\n") }

          restab.col <- rbind(restab[1L:2L, -3L], restab[grep("Col", restab[, 3L]), -3L])

          restab.col[-1L, -1L] <- apply(restab.col[-1L, -1L], 2L, function(y) formatC(sub("^\\s+", "", y), format = "f"))

          restab.col <- rbind(c(paste0(rep(" ", times = max(nchar(restab.col[, 1L]))), collapse = ""),
                                paste0(rep(" ", times = max(nchar(restab.col[, 2L]))), collapse = ""), colnames(x$data)[3L], rep("", times = ncol(restab.col) - 3L)),
                              restab.col[-1L, ])

          restab.col[-1L, 3L] <- paste0(" ", restab.col[-1L, 3L])
          restab.col[1L, 3L] <- paste0(restab.col[1L, 3L], " ")

          restab.col[, 1L] <- paste0(" ", restab.col[, 1L])

          # Remove duplicated row labels
          restab.col[, 1L] <- ifelse(duplicated(restab.col[, 1L]), paste(rep(" ", times = unique(nchar(restab.col[, 1L]))), collapse = ""), restab.col[, 1L])

          # Output
          cat("  Column-Wise Percentages\n")
          write.table(restab.col, col.names = FALSE, row.names = FALSE, quote = FALSE)

        }

        #### Total percentages ####
        if (isTRUE("total" %in% print)) {

          if (isTRUE(freq) || isTRUE("row" %in% print) || isTRUE("col" %in% print)) { cat("\n") }

          restab.total <- rbind(restab[1L:2L, -3L], restab[grep("Tot", restab[, 3L]), -3L])

          restab.total[, ncol(restab.total)] <- format(restab.total[, ncol(restab.total)], justify = "right")
          restab.total <- apply(restab.total, 2L, format)

          restab.total[-1L, -1L] <- apply(restab.total[-1L, -1L], 2L, function(y) formatC(sub("^\\s+", "", y), format = "f"))

          restab.total <- rbind(c(paste(rep(" ", times = max(nchar(restab.total[, 1L]))), collapse = ""),
                                  paste(rep(" ", times = max(nchar(restab.total[, 2L]))), collapse = ""), colnames(x$data)[3L], rep("", times = ncol(restab.total) - 3L)),
                                restab.total[-1L, ])

          restab.total[-1L, 3L] <- paste0(" ", restab.total[-1L, 3L])
          restab.total[1L, 3L] <- paste0(restab.total[1L, 3L], " ")

          restab.total[, 1L] <- paste0(" ", restab.total[, 1L])

          # Remove duplicated row labels
          restab.total[, 1L] <- ifelse(duplicated(restab.total[, 1L]), paste(rep(" ", times = unique(nchar(restab.total[, 1L]))), collapse = ""), restab.total[, 1L])

          # Output
          cat("  Total Percentages\n")
          write.table(restab.total, col.names = FALSE, row.names = FALSE, quote = FALSE)

        }

      }

    }

  #_____________________________________________________________________________
  #
  # Descriptive Statistics -----------------------------------------------------
  }, descript = {

    if (isTRUE(check)) {

      #......
      # Check input 'print'
      if (isTRUE(!all(print %in%  c("all", "n", "nNA", "pNA", "m", "se.m", "var", "sd", "min", "p25", "med", "p75", "max", "skew",  "range", "iqr", "kurt")))) {

        stop("Character strings in the argument 'print' do not all match with \"all\", \"n\", \"nNA\", \"pNA\", \"m\", \"se.m\", \"var\", \"sd\", \"min\", \"p25\", \"med\", \"p75\", \"max\", \"range\", \"iqr\", \"skew\", or \"kurt\".", call. = FALSE)

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
                                                                                                               zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

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
      print.object[, -1L] <- apply(print.object[, -1L, drop = FALSE], 2L, function(y) format(y, justify = "right"))

      # Add blank space
      print.object[, "variable"] <- c(paste0("  ", print.object[1L, "variable"], collapse = ""), paste0("   ", print.object[-1L, "variable"]))
      print.object[, "variable"] <- format(c(print.object[1L, "variable"], misty::chr.trim(print.object[-1L, "variable"], side = "right")), justify = "left")

      if (isTRUE(ncol(x$data$x) == 1L)) {

        print.object <- print.object[, -1L]

        print.object[, 1L] <- paste0("  ", print.object[, 1L])

      }

      cat(" Descriptive Statistics\n\n")

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
                                                                                    zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

      # Percentages
      print.object[, "pNA"] <- paste0(print.object[, "pNA"], "%")

      # Col names
      print.object <- rbind(c("Group", "Variable", "n", "nNA", "pNA", "M", "SE M", "Var", "SD", "Min", "p25", "Med", "p75", "Max", "Range", "IQR", "Skew", "Kurt"),
                            print.object)

      # Select statistical measures and add variable names
      print.object <- data.frame(print.object[, c("group", "variable")], print.object[, -c(1L, 2L)][, print, drop = FALSE], stringsAsFactors = FALSE)

      # Format
      # Justify left and right
      print.object[, c("group", "variable")] <- format(print.object[, c("group", "variable")], justify = "left")
      print.object[, -c(1L, 2L)] <- format(print.object[, -c(1L, 2L)], justify = "right")

      # Add blank space
      print.object[, "group"] <- c(paste0("  ", print.object[1L, "group"], collapse = ""), paste0("   ", print.object[-1L, "group"]))
      print.object[, "group"] <- format(c(print.object[1L, "group"], misty::chr.trim(print.object[-1L, "group"], side = "right")), justify = "left")

      print.object[, "variable"] <- c(print.object[1L, "variable"], paste0(" ", print.object[-1L, "variable"]))
      print.object[, "variable"] <- format(c(print.object[1L, "variable"], misty::chr.trim(print.object[-1L, "variable"], side = "right")), justify = "left")

      if (isTRUE(ncol(x$data$x) == 1L)) {

        print.object <- print.object[, -2L]

       }

      cat(" Descriptive Statistics\n\n")

      # Print Output
      write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE)

    #----------------------------------------
    # Split, without or with Grouping
    } else if (isTRUE(!is.null(x$data$split))) {

      # Format
      for (i in names(print.object)) {

        # Round
        print.object[[i]][, print.round] <- sapply(print.round, function(y) ifelse(!is.na(print.object[[i]][, y]), formatC(print.object[[i]][, y], digits = digits, format = "f",
                                                                                                                           zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

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
          print.object[[i]][, "variable"] <- c(paste0("   ", print.object[[i]][1L, "variable"], collapse = ""), paste0("    ", print.object[[i]][-1L, "variable"]))
          print.object[[i]][, "variable"] <- format(c(print.object[[i]][1L, "variable"], misty::chr.trim(print.object[[i]][-1L, "variable"], side = "right")), justify = "left")

          if (isTRUE(ncol(x$data$x) == 1L)) {

            print.object[[i]][, "n"] <- paste("", print.object[[i]][, "n"])

          }

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
          print.object[[i]][, "variable"] <- c(paste0("   ", print.object[[i]][1L, "variable"], collapse = ""), paste0("    ", print.object[[i]][-1L, "variable"]))
          print.object[[i]][, "variable"] <- format(c(print.object[[i]][1L, "variable"], misty::chr.trim(print.object[[i]][-1L, "variable"], side = "right")), justify = "left")

          print.object[[i]][, "group"] <- c(print.object[[i]][1L, "group"], paste0(" ", print.object[[i]][-1L, "group"]))
          print.object[[i]][, "group"] <- format(c(print.object[[i]][1L, "group"], misty::chr.trim(print.object[[i]][-1L, "group"], side = "right")), justify = "left")

        }

        # Only one variable
        if (isTRUE(ncol(x$data$x) == 1L)) {

          print.object[[i]] <- print.object[[i]][, -grep("variable", colnames(print.object[[i]]))]

          print.object[[i]][, 1L] <- paste("  ", print.object[[i]][, 1L])

        }

      }

      # Print object
      cat(" Descriptive Statistics\n\n")

      for (i in names(print.object)) {

        cat("  Split Group:", i, "\n")

        write.table(print.object[[i]], quote = FALSE, row.names = FALSE, col.names = FALSE)

        if (i != names(print.object)[length(print.object)]) { cat("\n") }

      }

    }

  #_____________________________________________________________________________
  #
  # Eta Squared ----------------------------------------------------------------
  }, eta.sq = {

    ####################################################################################
    # Data and Arguments

    #-----------------------------------------
    # Number of dependent variables, number of independent variables

    ncol.x <- ncol(x$data$x)
    ncol.group <- ncol(data.frame(x$data$group))

    ####################################################################################
    # Main Function

    #-----------------------------------------
    # One dependent variable, one independent variable

    if (isTRUE(ncol.x == 1L && ncol.group == 1L)) {

      # Print object
      print.object <- cbind("  Estimate  ", formatC(print.object, digits = digits, format = "f",
                                                    zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")))

    #-----------------------------------------
    # More than one dependent variable, more than one independent variable

    } else if (isTRUE(ncol.x > 1L && ncol.group > 1L)) {

      # Variable names and format digits
      print.object <- rbind(c("", "Outcome", rep("", times = ncol(print.object) - 1L)),
                            c("Group", colnames(print.object)),
                            cbind(rownames(print.object),
                                  formatC(print.object, digits = digits, format = "f",
                                          zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))))

      # Format
      print.object[-c(1L, 2L), 1L] <- paste("", print.object[-c(1L, 2L), 1L])
      print.object[, 1L] <- format(print.object[, 1L], justify = "left")

      print.object[-1L, 2L] <- paste("", print.object[-1L, 2L])

      print.object[1L, 2L] <- format(print.object[1L, 2L], justify = "left", width = max(nchar(print.object[, 2L])) )
      print.object[-1L, 2L] <- format(print.object[-1L, 2L], justify = "right")

      print.object[-1L, -1L] <- apply(print.object[-1L, -1L], 2L, function(y) format(y, justify = "right"))

    #-----------------------------------------
    # More than one dependent variable, one independent variable

    } else if (isTRUE(ncol.x > 1 && ncol.group == 1L)) {

      # Variable names and format digis
      print.object <- rbind(colnames(print.object),
                            formatC(print.object, digits = digits, format = "f",
                                    zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")))

      # Format
      print.object <- format(print.object, justify = "right")

    } else if (isTRUE(ncol.x == 1 && ncol.group > 1L)) {

     #-----------------------------------------
     # One dependent variable, more than one independent variable

      # Variable names and format digits
      print.object <- cbind(rownames(print.object),
                            formatC(print.object, digits = digits, format = "f",
                                    zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")))

      # Format
      print.object <- format(print.object, justify = "right")

    }

    ####################################################################################
    # Output

    if (isTRUE(ncol.x == 1L && ncol.group == 1L)) {

      cat(" Eta Squared\n\n")

    } else {

      cat(" Eta Squared Matrix\n\n")

    }

    write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE)

  #_____________________________________________________________________________
  #
  # Frequency Table ----------------------------------------------------------------
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

        print.object <- data.frame(x = c("Value", rep("", nrow(print.object) - 1L), "Missing", "Total"),
                                   val = c(print.object[1L:(which(is.na(print.object$Value)) - 1L), 1L], "Total", "NA", ""),
                                   rbind(print.object[1L:(which(is.na(print.object$Value)) - 1L), -1L],
                                         apply(print.object[1L:(which(is.na(print.object$Value)) - 1L), -1L], 2L, function(y) sum(as.numeric(y), na.rm = TRUE)),
                                         print.object[which(is.na(print.object$Value)), -1L],
                                         c(sum(as.numeric(print.object$Freq), na.rm = TRUE), "100", "")),
                                   stringsAsFactors = FALSE, row.names = NULL, check.names = FALSE)

        # Round digits
        print.object[, c("Perc", "V.Perc")] <- suppressWarnings(apply(print.object[, c("Perc", "V.Perc")], 2L, function(y) paste0(formatC(as.numeric(y), digits = digits, format = "f",
                                                                                                                                          zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), "%")))

        # Remove NA
        print.object[, "V.Perc"] <- gsub("NA%", "  ", print.object[, "V.Perc"])

        # Format
        print.object[, 1L:2L] <- apply(print.object[, 1L:2L], 2L, function(y) format(y, justify = "left"))

        print.object[, -c(1L:2L)] <- apply(print.object[, -(1L:2L)], 2L, function(y) format(y, justify = "right"))

        # Add blank
        print.object[, 1L] <- paste("", print.object[, 1L])

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
                                   val = apply(print.object[, -ncol(print.object)], 1L, sum, na.rm = TRUE),
                                   nNA = print.object[, ncol(print.object)],
                                   total = c(sum(print.object[1L, ], na.rm = TRUE), "100", ""),
                                   stringsAsFactors = FALSE, check.names = FALSE)

        print.object[1L, ] <- as.character(print.object[1L, ])
        print.object[2L, ] <- paste0(formatC(as.numeric(print.object[2L, ]), digits = digits, format = "f",
                                             zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), "%")
        print.object[3L, ] <- paste0(formatC(as.numeric(print.object[3L, ]), digits = digits, format = "f",
                                             zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), "%")

        print.object[3L, ] <- gsub("NA%", "", print.object[3L,  ])

        # Row names
        print.object <- cbind(x = c(" Freq", " Perc", " V.Perc"), print.object)

        # Column names
        colnames(print.object) <- c(" Value", colnames(x$result)[-length(x$result)], "Total", "Missing", "Total")

        # Format
        print.object[, 1L] <- format(print.object[, 1L], justify = "left")
        colnames(print.object)[1L] <- format(c(colnames(print.object)[1L], print.object[, 1L]), justify = "left")[1L]

        print.object[, -1L] <- apply(print.object[, -1L], 2L, function(y) format(y, justify = "right"))

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

        cat(" Frequency Table\n")

        if (isTRUE(x$args$val.col)) { cat("\n") }

      }

      print(print.object, row.names = FALSE, max = 99999L)

    #-----------------------------------------
    # More than one variable
    } else if (isTRUE(ncol(as.data.frame(x$data, stringsAsFactors = FALSE)) > 1L)) {

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
                                            val = c(print.object$freq[1L:(nrow(print.object$freq) - 1L), 1L], "Total", "NA", ""),
                                            rbind(print.object$freq[1L:(nrow(print.object$freq) - 1L), -1L],
                                                  apply(print.object$freq[1L:(nrow(print.object$freq) - 1L), -1L], 2L, function(y) sum(as.numeric(y), na.rm = TRUE)),
                                                  print.object$freq[nrow(print.object$freq), -1L],
                                                  apply(print.object$freq[, -1L], 2L, function(y) sum(as.numeric(y), na.rm = TRUE))),
                                            stringsAsFactors = FALSE, row.names = NULL, check.names = FALSE)

            # Format
            print.object$freq[, 1L:2L] <- apply(print.object$freq[, 1L:2L], 2L, function(y) format(y, justify = "left"))

            print.object$freq[, -c(1L:2L)] <- apply(print.object$freq[, -(1L:2L)], 2L, function(y) format(y, justify = "right"))

            # Add blank
            print.object$freq[, 1L] <- paste("", print.object$freq[, 1L])

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
                                            val = c(print.object$perc[1L:(nrow(print.object$perc) - 1L), 1L], "Total", "NA", ""),
                                            rbind(print.object$perc[1L:(nrow(print.object$perc) - 1L), -1L],
                                                  apply(print.object$perc[1L:(nrow(print.object$perc) - 1L), -1L], 2L, function(y) sum(as.numeric(y), na.rm = TRUE)),
                                                  print.object$perc[nrow(print.object$perc), -1L],
                                                  apply(print.object$perc[, -1L], 2L, function(y) sum(as.numeric(y), na.rm = TRUE))),
                                            stringsAsFactors = FALSE, row.names = NULL, check.names = FALSE)

            # Round digits
            print.object$perc[, -c(1L:2L)] <- apply(print.object$perc[, -c(1L:2L)], 2L, function(y) paste0(formatC(as.numeric(y), digits = digits, format = "f",
                                                                                                                  zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), "%"))

            # Format
            print.object$perc[, 1L:2L] <- apply(print.object$perc[, 1L:2L], 2L, function(y) format(y, justify = "left"))

            print.object$perc[, -c(1L:2L)] <- apply(print.object$perc[, -(1L:2L)], 2L, function(y) format(y, justify = "right"))

            # Add blank
            print.object$perc[, 1L] <- paste("", print.object$perc[, 1L])

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
                                                    apply(print.object$v.perc[, -1L], 2L, function(y) sum(as.numeric(y), na.rm = TRUE))),
                                              stringsAsFactors = FALSE, row.names = NULL, check.names = FALSE)

            # Round digits
            print.object$v.perc[, -c(1L:2L)] <- apply(print.object$v.perc[, -c(1L:2L)], 2L, function(y) paste0(formatC(as.numeric(y), digits = digits, format = "f",
                                                                                                                      zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), "%"))

            # Format
            print.object$v.perc[, 1L:2L] <- apply(print.object$v.perc[, 1L:2L], 2L, function(y) format(y, justify = "left"))

            # Format
            print.object$v.perc[, -c(1L:2L)] <- apply(print.object$v.perc[, -(1L:2L)], 2L, function(y) format(y, justify = "right"))

            # Add blank
            print.object$v.perc[, 1L] <- paste("", print.object$v.perc[, 1L])

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
                                            val = apply(print.object$freq[, 2L:(ncol(print.object$freq) - 1L)], 1L, function(y) sum(as.numeric(y), na.rm = TRUE)),
                                            miss = print.object$freq[, ncol(print.object$freq)],
                                            total = apply(print.object$freq[, 2L:(ncol(print.object$freq))], 1L, function(y) sum(as.numeric(y), na.rm = TRUE)),
                                            check.names = FALSE, stringsAsFactors = FALSE)

            # Add variable names
            colnames(print.object$freq) <- c("", colnames(print.object$freq)[2L:(ncol(print.object$freq) - 3L)], "Total", "Missing", "Total")

            # Format
            print.object$freq[, 1L] <- format(paste("", print.object$freq[, 1L]), justify = "left")

            print.object$freq[, -1L] <- apply(print.object$freq[, -1L], 2L, function(y) format(y, justify = "right"))

            # No missing data
            if (isTRUE(all(!is.na(x$data)))) {

              print.object$freq <- print.object$freq[, -ncol(print.object$freq)]

            }

          }

          #....
          # Percentages
          if (isTRUE(all(print != "no") && "perc" %in% print)) {

            print.object$perc <- data.frame(print.object$perc[, 1L:(ncol(print.object$perc) - 1L)],
                                            val = apply(print.object$perc[, 2L:(ncol(print.object$perc) - 1L)], 1L, function(y) sum(as.numeric(y), na.rm = TRUE)),
                                            miss = print.object$perc[, ncol(print.object$perc)],
                                            total = apply(print.object$perc[, 2L:(ncol(print.object$perc))], 1L, function(y) sum(as.numeric(y), na.rm = TRUE)),
                                            check.names = FALSE, stringsAsFactors = FALSE)

            # Add variable names
            colnames(print.object$perc) <- c("", colnames(print.object$perc)[2L:(ncol(print.object$perc) - 3L)], "Total", "Missing", "Total")

            # Round digits
            print.object$perc[, -1L] <- apply(print.object$perc[, -1L], 2L, function(y) paste0(formatC(as.numeric(y), digits = digits, format = "f",
                                                                                                      zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), "%"))

            # Format
            print.object$perc[, 1L] <- format(paste("", print.object$perc[, 1L]), justify = "left")

            print.object$perc[, -1L] <- apply(print.object$perc[, -1L], 2L, function(y) format(y, justify = "right"))

            # No missing data
            if (all(!is.na(x$data))) {

              print.object$perc <- print.object$perc[, -ncol(print.object$perc)]

            }

          }

          #....
          # Valid percentages
          if (isTRUE(all(print != "no") && "v.perc" %in% print)) {

            print.object$v.perc <- data.frame(print.object$v.perc,
                                              total = apply(print.object$v.perc[, 2L:(ncol(print.object$v.perc))], 1L, function(y) sum(as.numeric(y), na.rm = TRUE)),
                                              check.names = FALSE, stringsAsFactors = FALSE)

            # Add variable names
            colnames(print.object$v.perc) <- c("", colnames(print.object$v.perc)[2L:(ncol(print.object$v.perc) - 1L)], "Total")

            # Round digits
            print.object$v.perc[, -1L] <- apply(print.object$v.perc[, -1L], 2L, function(y) paste0(formatC(as.numeric(y), digits = digits, format = "f",
                                                                                                          zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), "%"))

            # Format
            print.object$v.perc[, 1L] <- format(paste("", print.object$v.perc[, 1L]), justify = "left")

            print.object$v.perc[, -1L] <- apply(print.object$v.perc[, -1L], 2L, function(y) format(y, justify = "right"))

          }

        }

        ####################################################################################
        # Output

        #..................
        # Absolute frequencies
        if (isTRUE(freq)) {

          cat(" Frequencies\n")

          if (isTRUE(x$args$val.col)) { cat("\n") }

          print(print.object$freq, row.names = FALSE, max = 99999L)

        }

        #..................
        # Percentage frequencies
        if (isTRUE(all(print != "no"))) {

          if (isTRUE(freq)) { cat("\n") }

          # Percentages
          if (isTRUE("perc" %in% print)) {

            cat(" Percentages\n")

            if (isTRUE(x$args$val.col)) { cat("\n") }

            print(print.object$perc, row.names = FALSE, max = 99999L)

          }

          if (isTRUE(any(is.na(x$data)))) {

            # Valid percentages
            if (isTRUE("v.perc" %in% print)) {

              if (isTRUE("perc" %in% print)) { cat("\n") }

              cat(" Valid Percentages\n")

              if (isTRUE(x$args$val.col)) { cat("\n") }

              print(print.object$v.perc, quote = FALSE, row.names = FALSE, max = 99999L)

            }

          }

        }

      # split = TRUE
      } else {

        cat(" Frequencies\n\n")

        for (i in names(x$result)) {

          cat(" ", i, "\n")

          temp <- list(call = x$call, type = "freq", data = x$data[, i], args = x$args, result = x$result[[i]])
          class(temp) <- "misty.object"

          print(temp, check = FALSE)

          if (isTRUE(i != names(x$result)[length(names(x$result))])) {

            cat("\n")

          }

        }

      }

    }

  #_____________________________________________________________________________
  #
  # Confidence Intervals for the Indirect Effect -------------------------------
  }, indirect = {

    if (isTRUE(check)) {

      #......
      # Check input 'print'
      if (isTRUE(any(!print %in% c("all", "asymp", "dop", "mc")))) {

        stop("Character string(s) in the argument 'print' does not match with \"all\", \"asymp\", \"dop\", or \"mc\".",
             call. = FALSE)

      }

    }

    if (isTRUE(all(print == "all"))) { print <- c("asymp", "dop", "mc") }

    #....................
    # Round

    print.object$asymp <- sapply(c("est", "se", "low", "upp"), function(y) ifelse(!is.na(print.object$asymp [, y]),
                                                                                  formatC(print.object$asymp[, y], digits = digits, format = "f",
                                                                                          zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))


    print.object$dop <- sapply(c("est", "se", "low", "upp"), function(y) ifelse(!is.na(print.object$dop [, y]),
                                                                                formatC(print.object$dop[, y], digits = digits, format = "f",
                                                                                        zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

    print.object$mc <- sapply(c("est", "se", "low", "upp"), function(y) ifelse(!is.na(print.object$mc [, y]),
                                                                               formatC(print.object$mc[, y], digits = digits, format = "f",
                                                                                       zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))
    #....................
    # Print names

    print.object$asymp <- rbind(c("Est.", "SE", "Low", "Upp"), print.object$asymp)
    print.object$dop <- rbind(c("Est.", "SE", "Low", "Upp"), print.object$dop)
    print.object$mc <- rbind(c("Est.", "SE", "Low", "Upp"), print.object$mc)

    #....................
    # Justify right

    print.object$asymp <- apply(print.object$asymp, 2L, format, justify = "right")
    print.object$dop <- apply(print.object$dop, 2L, format, justify = "right")
    print.object$mc <- apply(print.object$mc, 2L, format, justify = "right")


    #....................
    # Add blank space

    print.object$asymp[, "est"] <- paste0("   ", print.object$asymp[, "est"])
    print.object$dop[, "est"] <- paste0("   ", print.object$dop[, "est"])
    print.object$mc[, "est"] <- paste0("   ", print.object$mc[, "est"])

    #-----------------------------------------------------------------------------------
    # Print

    cat(paste(switch(x$args$alternative,
                     two.sided = " Two-Sided",
                     less = " One-Sided",
                     greater = " One-Sided"),
              paste0(round(x$args$conf.level * 100L, digits = 2L), "%"), "Confidence Interval: Indirect Effect\n\n"))

    #....................
    # Asymptotic Normal Method
    if (isTRUE("asymp" %in% print)) {

      cat("  Asymptotic Normal Method\n")

      write.table(print.object$asymp, quote = FALSE, row.names = FALSE, col.names = FALSE)

      cat("\n   Note.", switch(x$args$se, "sobel" = "Approximate standard error by Sobel (1982)\n",
                               "aroian" = "Exact standard error by Aroian (1947).\n",
                               "goodman" = "Unbiased standard error by Goodman (1960)\n"))

    }

    #....................
    #  Distribution of the Product Method
    if (isTRUE("dop" %in% print)) {

      if (isTRUE("asymp" %in% print)) { cat("\n") }

      cat("  Distribution of the Product Method\n")

      write.table(print.object$dop, quote = FALSE, row.names = FALSE, col.names = FALSE)

    }

    #....................
    #  Monte Carlo Method
    if (isTRUE("mc" %in% print)) {

      if (isTRUE(any(c("asymp", "dop") %in% print))) { cat("\n") }

      # Monte Carlo Method
      cat("  Monte Carlo Method with",  format(x$args$nrep, scientific = FALSE), "repetitions\n")

      write.table(print.object$mc, quote = FALSE, row.names = FALSE, col.names = FALSE)

    }

  #_____________________________________________________________________________
  #
  # Coefficient Alpha ----------------------------------------------------------
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

        print.object$alpha$n <- format(paste(" ", print.object$alpha$n), justify = "right")

        print.object$alpha$items <- format(print.object$alpha$items, justify = "right")

        print.object$alpha$alpha <- formatC(print.object$alpha$alpha, digits = digits, format = "f",
                                            zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

        print.object$alpha$low <- formatC(print.object$alpha$low, digits = digits, format = "f",
                                          zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))
        print.object$alpha$upp <- formatC(print.object$alpha$upp, digits = digits, format = "f",
                                          zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

        print.object$alpha <- rbind(c("n", "Items", "Alpha", "Low", "Upp"), print.object$alpha)

        print.object$alpha <- apply(print.object$alpha, 2L, function(y) format(y, justify = "right"))

        cat(paste0(ifelse(isTRUE(x$args$std), " Standardized ", " Unstandardized "), "Coefficient Alpha with ",
                   x$args$conf.level*100L, "% Confidence Interval\n\n"))

          write.table(print.object$alpha, quote = FALSE, row.names = FALSE, col.names = FALSE)

        } else {

          print.object$alpha$alpha <- formatC(print.object$alpha$alpha, digits = digits, format = "f",
                                              zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

          print.object$alpha <- rbind(c("  Items", "Alpha"), print.object$alpha)

          print.object$alpha <- apply(print.object$alpha, 2L, function(y) format(y, justify = "right"))

        if (!isTRUE(x$args$ordered)) {

          cat(paste0(ifelse(isTRUE(x$args$std), " Standardized ", " Unstandardized "), "Coefficient Alpha\n\n"))

        } else {

          cat(" Ordinal Coefficient Alpha\n\n")

        }

        write.table(print.object$alpha, quote = FALSE, row.names = FALSE, col.names = FALSE)

      }

    }

    #-----------------------------------------
    # Item statistics
    if (isTRUE("item" %in% print && !is.null(print.object$itemstat) && nrow(print.object$itemstat) > 2L)) {

      print.object$item$pNA <- paste0(formatC(print.object$item$pNA, digits = 2L, format = "f",
                                              zero.print = paste0("0.", paste(rep(0L, times = 2L), collapse = ""))), "%")
      print.object$item$m <- formatC(print.object$item$m, digits = 2L, format = "f",
                                     zero.print = paste0("0.", paste(rep(0L, times = 2L), collapse = "")))
      print.object$item$sd <- formatC(print.object$item$sd, digits = 2L, format = "f",
                                      zero.print = paste0("0.", paste(rep(0L, times = 2L), collapse = "")))
      print.object$item$min <- formatC(print.object$item$min, digits = 2L, format = "f",
                                       zero.print = paste0("0.", paste(rep(0L, times = 2L), collapse = "")))
      print.object$item$max <- formatC(print.object$item$max, digits = 2L, format = "f",
                                       zero.print = paste0("0.", paste(rep(0L, times = 2L), collapse = "")))

      print.object$item$it.cor <- formatC(print.object$item$it.cor, digits = digits, format = "f",
                                          zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))
      print.object$item$alpha <- formatC(print.object$item$alpha, digits = digits, format = "f",
                                         zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

      print.object$item <- rbind(c("Variable", "n", "nNA", "pNA", "M", "SD", "Min", "Max", "It.Cor", "Alpha"),
                                 print.object$item)

      # Format
      print.object$item[, 1L] <- format(paste(" ", print.object$item[, 1L]), justify = "left")
      print.object$item[, -1L] <- apply(print.object$item[, -1L], 2L, function(y) format(y, justify = "right"))


      if (isTRUE("alpha" %in% print)) { cat("\n") }

      cat(" Item-Total Correlation and Coefficient Alpha if Item Deleted\n\n")

      write.table(print.object$item, quote = FALSE, row.names = FALSE, col.names = FALSE)

    }

  #_____________________________________________________________________________
  #
  # Confirmatory factor analysis -----------------------------------------------
  }, item.cfa = {

    cat(" Confirmatory Factor Analysis\n")

    #----------------------------------------
    # lavaan summary

    if (isTRUE("summary" %in% x$args$print)) {

      print.summary <- print.object$summary

      #........................................
      # Format

      # Include spaces
      print.summary[1L, 1L] <- paste0(" ", print.summary[1L, 1L])
      print.summary[-1L, 1L] <- paste0("  ", print.summary[-1L, 1L])

      # Justify left
      print.summary[, 1L] <- format(print.summary[, 1L], justify = "left")

      #........................................
      # Print

      # Complete data
      if (isTRUE(print.summary[6L, 2L] == "None")) {

        print.summary <- unname(print.summary[, -3L])

        # No cluster variable
        if (isTRUE(is.null(x$args$cluster))) {

          print(print.summary[-c(8L, 10L), ], col.names = FALSE, row.names = FALSE, quote = FALSE, right = TRUE, max = 99999L)

        # With cluster variable
        } else {

          print(print.summary[-8L, -3L], col.names = FALSE, row.names = FALSE, quote = FALSE, right = TRUE, max = 99999L)

        }

      # Missing data
      } else {

        # No cluster variable
        if (isTRUE(is.null(x$args$cluster))) {

          print(print.summary[-10L, ], col.names = FALSE, row.names = FALSE, quote = FALSE, right = TRUE, max = 99999L)

        # With cluster variable
        } else {

          print(print.summary, col.names = FALSE, row.names = FALSE, quote = FALSE, right = TRUE, max = 99999L)

        }

      }

    }

    #----------------------------------------
    # Covariance coverage

    if (isTRUE("coverage" %in% x$args$print)) {

      cat("\n  Covariance Coverage of Data\n\n")

      print.coverage <- print.object$coverage

      #........................................
      # Lower triangular

      print.coverage[upper.tri(print.coverage)] <- ""

      #........................................
      # Format
      print.coverage <- apply(print.coverage, 2L, function(y) ifelse(!is.na(as.numeric(y)), formatC(as.numeric(y), digits = 2L, format = "f",
                                                                                                    zero.print = ifelse(2L > 0L, paste0("0.", paste(rep(0L, times = 2L), collapse = "")), "0")), ""))

      # Row names
      row.names(print.coverage) <- paste0("   ", colnames(print.coverage))

      #........................................
      # Print

      print(print.coverage, row.names = FALSE, quote = FALSE, right = TRUE, max = 99999L)

    }

    #----------------------------------------
    # Sample Statistics

    if (isTRUE("descript" %in% x$args$print)) {

      #........................................
      # Continuous indicators
      if (isTRUE(is.null(x$args$ordered))) {

        cat("\n  Univariate Sample Statistics\n\n")

        print.itemstat <- print.object$itemstat

        #........................................
        # Format

        # Variables to round
        print.round <- c("pNA", "m", "sd", "min", "max", "skew", "kurt")

        print.itemstat[, print.round] <- sapply(print.round, function(y) ifelse(!is.na(print.itemstat[, y]), formatC(print.itemstat[, y], digits = x$args$digits, format = "f",
                                                                                                                     zero.print = ifelse(x$args$digits > 0L, paste0("0.", paste(rep(0L, times = x$args$digits), collapse = "")), "0")), NA))

        # Percentages
        print.itemstat[, "pNA"] <- paste0(print.itemstat[, "pNA"], "%")

        # Column names
        print.itemstat <- rbind(c("Variable", "n", "nNA", "pNA", "M", "SD", "Min", "Max",  "Skew", "Kurt"), print.itemstat)

        # Justify left and right
        print.itemstat[, 1L] <- format(print.itemstat[, 1L, drop = FALSE], justify = "left")
        print.itemstat[, -1L] <- apply(print.itemstat[, -1L, drop = FALSE], 2L, function(y) format(y, justify = "right"))

        # Add blank space
        print.itemstat[, "variable"] <- c(paste0("   ", print.itemstat[1L, "variable"], collapse = ""), paste0("    ", print.itemstat[-1L, "variable"]))
        print.itemstat[, "variable"] <- format(c(print.itemstat[1L, "variable"], misty::chr.trim(print.itemstat[-1L, "variable"], side = "right")), justify = "left")

        #........................................
        # Print

        write.table(print.itemstat, quote = FALSE, row.names = FALSE, col.names = FALSE)

      #........................................
      # Ordered indicators
      } else if (isTRUE(x$args$ordered)) {

        cat("  Univariate Counts for Ordered Variables\n\n")

        print.itemfreq <- print.object$itemfreq$freq

        # Column names
        print.itemfreq <- rbind(c("Variable", colnames(print.itemfreq)[-1L]), print.itemfreq)

        # Justify left and right
        print.itemfreq[, 1L] <- format(print.itemfreq[, 1L, drop = FALSE], justify = "left")
        print.itemfreq[, -1L] <- apply(print.itemfreq[, -1L, drop = FALSE], 2L, function(y) format(y, justify = "right"))

        # Add blank space
        print.itemfreq[, "Var"] <- c(paste0("   ", print.itemfreq[1L, "Var"], collapse = ""), paste0("    ", print.itemfreq[-1L, "Var"]))
        print.itemfreq[, "Var"] <- format(c(print.itemfreq[1L, "Var"], misty::chr.trim(print.itemfreq[-1L, "Var"], side = "right")), justify = "left")

        #........................................
        # Print

        write.table(print.itemfreq, quote = FALSE, row.names = FALSE, col.names = FALSE)

      #........................................
      # Continuous and ordered indicators
      } else {

        #........................................
        # Sample Statistics

        cat("\n  Univariate Sample Statistics\n\n")

        print.itemstat <- print.object$itemstat

        print.itemstat <- print.itemstat[which(!print.itemstat$variable %in% x$args$ordered),]

        #........................................
        # Format

        # Variables to round
        print.round <- c("pNA", "m", "sd", "min", "max", "skew", "kurt")

        print.itemstat[, print.round] <- sapply(print.round, function(y) ifelse(!is.na(print.itemstat[, y]), formatC(print.itemstat[, y], digits = x$args$digits, format = "f",
                                                                                                                     zero.print = ifelse(x$args$digits > 0L, paste0("0.", paste(rep(0L, times = x$args$digits), collapse = "")), "0")), NA))

        # Percentages
        print.itemstat[, "pNA"] <- paste0(print.itemstat[, "pNA"], "%")

        # Column names
        print.itemstat <- rbind(c("Variable", "n", "nNA", "pNA", "M", "SD", "Min", "Max",  "Skew", "Kurt"), print.itemstat)

        # Justify left and right
        print.itemstat[, 1L] <- format(print.itemstat[, 1L, drop = FALSE], justify = "left")
        print.itemstat[, -1L] <- apply(print.itemstat[, -1L, drop = FALSE], 2L, function(y) format(y, justify = "right"))

        # Add blank space
        print.itemstat[, "variable"] <- c(paste0("   ", print.itemstat[1L, "variable"], collapse = ""), paste0("    ", print.itemstat[-1L, "variable"]))
        print.itemstat[, "variable"] <- format(c(print.itemstat[1L, "variable"], misty::chr.trim(print.itemstat[-1L, "variable"], side = "right")), justify = "left")

        #........................................
        # Print

        write.table(print.itemstat, quote = FALSE, row.names = FALSE, col.names = FALSE)

        #........................................
        # Counts for ordered variables

        cat("\n  Univariate Counts for Ordered Variables\n\n")

        print.itemfreq <- print.object$itemfreq$freq

        print.itemfreq <- print.itemfreq[which(print.itemfreq$Var %in% x$args$ordered), ]

        # Column names
        print.itemfreq <- rbind(c("Variable", colnames(print.itemfreq)[-1L]), print.itemfreq)

        # Justify left and right
        print.itemfreq[, 1L] <- format(print.itemfreq[, 1L, drop = FALSE], justify = "left")
        print.itemfreq[, -1L] <- apply(print.itemfreq[, -1L, drop = FALSE], 2L, function(y) format(y, justify = "right"))

        # Add blank space
        print.itemfreq[, "Var"] <- c(paste0("   ", print.itemfreq[1L, "Var"], collapse = ""), paste0("    ", print.itemfreq[-1L, "Var"]))
        print.itemfreq[, "Var"] <- format(c(print.itemfreq[1L, "Var"], misty::chr.trim(print.itemfreq[-1L, "Var"], side = "right")), justify = "left")

        #........................................
        # Print

        write.table(print.itemfreq, quote = FALSE, row.names = FALSE, col.names = FALSE)

      }

    }

    #----------------------------------------
    # Model fit

    if (isTRUE("fit" %in% x$args$print)) {

      cat("\n  Model Fit Information\n")

      print.fit <- print.object$fit

      #........................................
      # Round

      # digits
      print.fit[-c(1L, 5L, 6L, 17L), -1L] <- lapply(print.fit[-c(1L, 5L, 6L, 17L), -1], function(y) ifelse(!is.na(y), formatC(y, digits = x$args$digits, format = "f",
                                                                                                                              zero.print = ifelse(x$args$digits > 0L, paste0("0.", paste(rep(0L, times = x$args$digits), collapse = "")), "0")), NA))

      # p.digits
      print.fit[c(6L, 17L), -c(1L, 4L)] <- sapply(print.fit[c(6L, 17L), -c(1L, 4L)], function(y) ifelse(!is.na(y), formatC(as.numeric(y), digits = x$args$p.digits, format = "f",
                                                                                                                           zero.print = ifelse(x$args$p.digits > 0L, paste0("0.", paste(rep(0L, times = x$args$p.digits), collapse = "")), "0")), NA))

      if (isTRUE(x$args$estimator %in% c("MLMVS", "PML"))) {

        print.fit[5L, 3L] <- formatC(as.numeric(print.fit[5L, 3L]), digits = x$args$digits, format = "f", zero.print = ifelse(x$args$digits > 0L, paste0("0.", paste(rep(0L, times = x$args$digits), collapse = "")), "0"))

      }

      #........................................
      # Add labels

      print.fit <- rbind(print.fit[1L:9L, ], c("", "Standard", "Ad hoc", "Robust"),
                         print.fit[10L:13L, ], c("", "Standard", "Ad hoc", "Robust"), print.fit[14L:24L, ],
                         make.row.names = FALSE)

      if (isTRUE(x$args$estimator == "PML")) {

        print.fit[15L, 3L] <- ""

      }

      #........................................
      # Replace NA with ""

      print.fit <- unname(misty::na.as(print.fit, ""))

      #........................................
      # Add blank space

      print.fit[, 1L] <- paste0("  ", print.fit[, 1L])

      print.fit[c(4L:7L, 11L, 12L, 16L:19L, 21L, 24L:26L), 1L] <- paste("", unlist(print.fit[c(4L:7L, 11L, 12L, 16L:19L, 21L, 24L:26L), 1L]))

      #........................................
      # Justify left and right

      print.fit[, 1L] <- format(print.fit[, 1L, drop = FALSE], justify = "left")

      print.fit[, -1L] <- apply(print.fit[, -1L, drop = FALSE], 2L, function(y) format(y, justify = "right"))

      #........................................
      # Exclude Chi-square p-value and scaling correction factor
      if (isTRUE(as.numeric(print.fit[5, 2]) == 0)) {

        print.fit <- print.fit[-c(6L, 7L), ]

      } else if (isTRUE(!x$args$estimator %in% c("MLM", "MLMV", "MLMVS", "MLR", "WLSM", "WLSMV", "ULSM", "ULSMV", "DLS", "PML"))) {

        print.fit <- print.fit[-7L, ]

      }

      #........................................
      # Exclude information criteria

      if (isTRUE(!x$args$estimator %in% c("ML", "MLM", "MLMV", "MLMVS", "MLF", "MLR"))) {

        print.fit <- print.fit[-c((nrow(print.fit) - 4L):nrow(print.fit)), ]

      }

      #........................................
      # Print

      if (isTRUE(x$args$estimator %in% c("ML", "MLF", "GLS", "WLS", "DWLS", "ULS"))) {

        print(print.fit[, c(1L, 2L)], row.names = FALSE, quote = FALSE, right = TRUE, max = 99999L)

      } else if (isTRUE(x$args$estimator %in% c("MLMV", "MLMVS", "WLSMV", "ULSMV"))) {

        print(print.fit[, c(1L, 3L)], row.names = FALSE, quote = FALSE, right = TRUE, max = 99999L)

      } else if (isTRUE(x$args$estimator %in% c("MLM", "MLR", "WLSM", "ULSM", "DLS"))) {

        print(print.fit[, c(1L, 3L, 4L)], row.names = FALSE, quote = FALSE, right = TRUE, max = 99999L)

      } else if (isTRUE(x$args$estimator %in% "PML")) {

        # print.fit[c(21L, 24L, 25L, 26L), 3L] <- ""

        print(print.fit[-c(22L:26L), c(1L, 2L, 3L)], row.names = FALSE, quote = FALSE, right = TRUE, max = 99999L)

      }

    }

    #----------------------------------------
    # Parameter estimates

    if (isTRUE("est" %in% x$args$print)) {

      cat("\n  Model Results\n\n")

      print.param <- print.object$param

      #........................................
      # Round

      # digits
      print.param[, -c(1L:4L, 8L)] <- lapply(print.param[, -c(1L:4L, 8L)], function(y) ifelse(!is.na(y), formatC(y, digits = x$args$digits, format = "f",
                                                                                                                 zero.print = ifelse(x$args$digits > 0L, paste0("0.", paste(rep(0L, times = x$args$digits), collapse = "")), "0")), NA))

      # p.digits
      print.param[, "pvalue"] <- formatC(as.numeric(print.param[, "pvalue"]), digits = x$args$p.digits, format = "f",
                                         zero.print = ifelse(x$args$p.digits > 0L, paste0("0.", paste(rep(0L, times = x$args$p.digits), collapse = "")), "0"))

      #........................................
      # Convert NA into ""

      print.param[grep("~", print.param$rhs), c("est", "se", "z", "pvalue", "stdyx")] <- ""

      #........................................
      # Column names
      print.param <- rbind(c("", "", "", "", "Estimate", "Std.Err", "z-value", "P(>|z|)", "StdYX"),
                           print.param)

      #........................................
      # Add blank spaces

      print.param[grep("~", print.param$rhs), "rhs"] <- paste("   ", print.param[grep("~", print.param$rhs), "rhs"])
      print.param[-grep("~", print.param$rhs), "rhs"] <- paste("     ", print.param[-grep("~", print.param$rhs), "rhs"])

      #........................................
      # Justify

      print.param[, "rhs"] <- format(print.param[, "rhs", drop = FALSE], justify = "left")
      print.param[, -c(1L:4L)] <- apply(print.param[, -c(1L:4L), drop = FALSE], 2L, function(y) format(y, justify = "right"))

      #........................................
      # Print

      ###
      # Factor loadings
      cat("   Factor Loadings\n")

      # Heading
      write.table(print.param[1L, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

      print.lv <- print.param[print.param$param == "latent variable", ]

      for (i in unique(print.lv$lhs)) {

        pos.NA <- grep("NA", print.lv[print.lv$lhs == i, "z"])

        if (isTRUE(length(pos.NA) > 0L)) {

          print.lv[print.lv$lhs == i, ][pos.NA, "se"] <- paste(rep("", times = unique(nchar(print.lv[print.lv$lhs == i, ][pos.NA, "se"])) + 1L), collapse = " ")
          print.lv[print.lv$lhs == i, ][pos.NA, "z"] <- paste(rep("", times = unique(nchar(print.lv[print.lv$lhs == i, ][pos.NA, "z"])) + 1L), collapse = " ")
          print.lv[print.lv$lhs == i, ][pos.NA, "pvalue"] <- paste(rep("", times = unique(nchar(print.lv[print.lv$lhs == i, ][pos.NA, "pvalue"])) + 1L), collapse = " ")

        }

        write.table(print.lv[print.lv$lhs == i, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

      }

      ###
      # Latent variable covariances
      if (isTRUE(any(print.param$param %in% "latent variable covariance"))) {

        cat("\n   Latent Variable Covariances\n")

        print.lv.cov <- print.param[print.param$param == "latent variable covariance", ]

        for (i in unique(print.lv.cov$lhs)) {

          write.table(print.lv.cov[print.lv.cov$lhs == i, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

        }

      }

      ###
      # Residual covariances
      if (isTRUE(any(print.param$param %in% "residual covariance"))) {

        cat("\n   Residual Covariances\n")

        print.res.cov <- print.param[print.param$param == "residual covariance", ]

        for (i in unique(print.res.cov$lhs)) {

          write.table(print.res.cov[print.res.cov$lhs == i, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

        }

      }

      ###
      # Latent mean
      if (isTRUE(any(print.param$param %in% "latent mean"))) {

        cat("\n   Latent Means\n")

        print.mean <- print.param[print.param$param == "latent mean", ]

        print.mean[grep("NA", print.mean[, "z"]), c("se", "z", "pvalue", "stdyx")] <- ""

        write.table(print.mean[, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

      }

      ###
      # Latent variance
      if (isTRUE(any(print.param$param %in% "latent variance"))) {

        cat("\n   Latent Variances\n")

        print.var <- print.param[print.param$param == "latent variance", ]

        print.var[grep("NA", print.var[, "z"]), c("se", "z", "pvalue", "stdyx")] <- ""

        write.table(print.var[, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

      }

      ###
      # Intercepts
      if (isTRUE(any(print.param$param %in% "intercept"))) {

        cat("\n   Intercepts\n")

        print.inter <- print.param[print.param$param == "intercept", ]

        print.inter[grep("NA", print.inter[, "z"]), c("se", "z", "pvalue", "stdyx")] <- ""

        write.table(print.inter[, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

      }

      ###
      # Thresholds
      if (isTRUE(any(print.param$param %in% "threshold"))) {

        cat("\n   Thresholds\n")

        print.thres <- print.param[print.param$param == "threshold", ]

        print.thres[grep("NA", print.thres[, "z"]), c("se", "z", "pvalue", "stdyx")] <- ""

        write.table(print.thres[, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

      }

      ###
      # Scales
      if (isTRUE(any(print.param$param %in% "scale"))) {

        cat("\n   Scales\n")

        print.scale <- print.param[print.param$param == "scale", ]

        pos.NA <- grep("NA", print.scale[, "z"])

        if (isTRUE(length(pos.NA) >  0L)) {

          print.scale[pos.NA, "se"] <- paste(rep("", times = unique(nchar(print.scale[pos.NA, "se"])) + 1L), collapse = " ")
          print.scale[pos.NA, "z"] <- paste(rep("", times = unique(nchar(print.scale[pos.NA, "z"])) + 1L), collapse = " ")
          print.scale[pos.NA, "pvalue"] <- paste(rep("", times = unique(nchar(print.scale[pos.NA, "pvalue"])) + 1L), collapse = " ")

        }

        write.table(print.scale[, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

      }

      ###
      # Residual variance
      if (isTRUE(any(print.param$param %in% "residual variance"))) {

        cat("\n   Residual Variances\n")

        print.resid <- print.param[print.param$param == "residual variance", ]

        pos.NA <- grep("NA", print.resid[, "z"])

        if (isTRUE(length(pos.NA) >  0L)) {

        print.resid[pos.NA, "se"] <- paste(rep("", times = unique(nchar(print.resid[pos.NA, "se"])) + 1L), collapse = " ")
        print.resid[pos.NA, "z"] <- paste(rep("", times = unique(nchar(print.resid[pos.NA, "z"])) + 1L), collapse = " ")
        print.resid[pos.NA, "pvalue"] <- paste(rep("", times = unique(nchar(print.resid[pos.NA, "pvalue"])) + 1L), collapse = " ")

        }

        write.table(print.resid[, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

      }

    }

    #----------------------------------------
    # Modification indices

    if (isTRUE("modind" %in% x$args$print)) {

      cat("\n  Modification Indices\n")

      print.modind <- print.object$modind

      #........................................
      # No modification indices
      if (isTRUE(is.null(print.modind))) {

        cat("\n   Modification indices are not available.")

      } else  if (isTRUE(nrow(print.modind) == 0L)) {

        cat(paste0("\n   No modification indices above the minimum value ", round(x$args$min.value, digits = 2L), "."))

      #........................................
      # Modification indices
      } else {

        # Round
        print.modind[, -c(1L:3L)] <- lapply(print.modind[, -c(1L:3L)], function(y) ifelse(!is.na(y), formatC(y, digits = x$args$digits, format = "f",
                                                                                                             zero.print = ifelse(x$args$digits > 0L, paste0("0.", paste(rep(0L, times = x$args$digits), collapse = "")), "0")), NA))

        # Column names
        print.modind <- rbind(c("", "", "", "MI", "EPC", "StdYX EPC"), print.modind)

        # Add blank spaces
        print.modind[, "lhs"] <- paste("   ", print.modind[, "lhs"])

        # Justify
        print.modind[ c("lhs", "op", "rhs")] <- format(print.modind[, c("lhs", "op", "rhs")], justify = "left")
        print.modind[, -c(1L, 2L)] <- apply(print.modind[, -c(1L, 2L), drop = FALSE], 2L, function(y) format(y, justify = "right"))

        ###
        # Factor loadings

        print.modind.load <- print.modind[print.modind$op == "=~", ]

        if (isTRUE(nrow(print.modind.load) > 0L)) {

          cat("\n   Factor Loading\n")

          # Print header
          write.table(print.modind[1L, ], quote = FALSE, row.names = FALSE, col.names = FALSE)

          write.table(print.modind.load, quote = FALSE, row.names = FALSE, col.names = FALSE)

        }

        ###
        # Residual covariances

        print.modind.cov <- print.modind[print.modind$op == "~~", ]

        if (isTRUE(nrow(print.modind.cov) > 0L)) {

          cat("\n   Residual Covariance\n")

          # Print header
          write.table(print.modind[1L, ], quote = FALSE, row.names = FALSE, col.names = FALSE)

          write.table(print.modind.cov, quote = FALSE, row.names = FALSE, col.names = FALSE)

        }

        ###
        # Note
        cat(paste0("\n   Note. Minimum value for printing the index is ", round(x$args$min.value, digits = 2L), ".\n"))

      }

    }

  #_____________________________________________________________________________
  #
  # Within-Group and Between-Group Correlation Matrix --------------------------
  }, multilevel.cor = {

    if (isTRUE(check)) {

      #......
      # Check input 'print'
      if (isTRUE(any(!print %in% c("all", "cor", "se", "stat", "p")))) {

        stop("Character string(s) in the argument 'print' does not match with \"all\", \"cor\", \"se\", \"stat\", or \"p\".",
             call. = FALSE)

      }

    }

    # R Markdown in progress
    if (isTRUE(getOption("knitr.in.progress"))) {

      x$args$sig <- FALSE

    }

    ####################################################################################
    # Main Function

    #-----------------------------------------------------------------------------------
    # Split results
    if (isTRUE(x$args$split)) {

      #........................................
      # Round and format

      # Within
      print.object$with.cor <- formatC(print.object$with.cor, digits = digits, format = "f",
                                       zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

      print.object$with.se <- formatC(print.object$with.se, digits = digits, format = "f",
                                       zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

      print.object$with.stat <- formatC(print.object$with.stat, digits = digits, format = "f",
                                     zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

      print.object$with.p <- formatC(print.object$with.p, digits = p.digits, format = "f",
                                     zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

      # Between
      print.object$betw.cor <- formatC(print.object$betw.cor, digits = digits, format = "f",
                                       zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

      print.object$betw.se <- formatC(print.object$betw.se, digits = digits, format = "f",
                                       zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

      print.object$betw.stat <- formatC(print.object$betw.stat, digits = digits, format = "f",
                                     zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

      print.object$betw.p <- formatC(print.object$betw.p, digits = p.digits, format = "f",
                                     zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

      diag(print.object$with.cor) <- ""
      diag(print.object$with.se) <- ""
      diag(print.object$with.stat) <- ""
      diag(print.object$with.p) <- ""

      diag(print.object$betw.cor) <- ""
      diag(print.object$betw.se) <- ""
      diag(print.object$betw.stat) <- ""
      diag(print.object$betw.p) <- ""

      #........................................
      # Lower and/or upper triangular

      if (isTRUE(tri == "lower")) {

        # Within
        print.object$with.cor[upper.tri(print.object$with.cor)] <- ""
        print.object$with.se[upper.tri(print.object$with.se)] <- ""
        print.object$with.stat[upper.tri(print.object$with.stat)] <- ""
        print.object$with.p[upper.tri(print.object$with.p)] <- ""

        # Between
        print.object$betw.cor[upper.tri(print.object$betw.cor)] <- ""
        print.object$betw.se[upper.tri(print.object$betw.se)] <- ""
        print.object$betw.stat[upper.tri(print.object$betw.stat)] <- ""
        print.object$betw.p[upper.tri(print.object$betw.p)] <- ""

      }

      if (isTRUE(tri == "upper")) {

        # Within
        print.object$with.cor[lower.tri(print.object$with.cor)] <- ""
        print.object$with.se[lower.tri(print.object$with.se)] <- ""
        print.object$with.stat[lower.tri(print.object$with.stat)] <- ""
        print.object$with.p[lower.tri(print.object$with.p)] <- ""

        # Between
        print.object$betw.cor[lower.tri(print.object$betw.cor)] <- ""
        print.object$betw.se[lower.tri(print.object$betw.se)] <- ""
        print.object$betw.stat[lower.tri(print.object$betw.stat)] <- ""
        print.object$betw.p[lower.tri(print.object$betw.p)] <- ""

      }

      #........................................
      # Row names

      # Within
      row.names(print.object$with.cor) <- paste0("   ", row.names(print.object$with.cor))
      row.names(print.object$with.se) <- paste0("   ", row.names(print.object$with.se))
      row.names(print.object$with.stat) <- paste0("   ", row.names(print.object$with.stat))
      row.names(print.object$with.p) <- paste0("   ", row.names(print.object$with.p))

      print.object$with.cor <- apply(print.object$with.cor, 2L, function(y) format(y, justify = "right"))
      print.object$with.se <- apply(print.object$with.se, 2L, function(y) format(y, justify = "right"))
      print.object$with.stat <- apply(print.object$with.stat, 2L, function(y) format(y, justify = "right"))
      print.object$with.p <- apply(print.object$with.p, 2L, function(y) format(y, justify = "right"))

      # Between
      row.names(print.object$betw.cor) <- paste0("   ", row.names(print.object$betw.cor))
      row.names(print.object$betw.se) <- paste0("   ", row.names(print.object$betw.se))
      row.names(print.object$betw.stat) <- paste0("   ", row.names(print.object$betw.stat))
      row.names(print.object$betw.p) <- paste0("   ", row.names(print.object$betw.p))

      print.object$betw.cor <- apply(print.object$betw.cor, 2L, function(y) format(y, justify = "right"))
      print.object$betw.se <- apply(print.object$betw.se, 2L, function(y) format(y, justify = "right"))
      print.object$betw.stat <- apply(print.object$betw.stat, 2L, function(y) format(y, justify = "right"))
      print.object$betw.p <- apply(print.object$betw.p, 2L, function(y) format(y, justify = "right"))

      #........................................
      # Statistically significant correlation coefficients in boldface

      if (isTRUE(x$args$sig)) {

        numbers <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
        unicode <- c("\U1D7CE", "\U1D7CF", "\U1D7D0", "\U1D7EF", "\U1D7F0", "\U1D7F1", "\U1D7F2", "\U1D7F3", "\U1D7F4", "\U1D7F5")

        print.object$with.cor[lower.tri(print.object$with.cor)][which(x$result$with.p[lower.tri(x$result$with.p)] <= x$args$alpha)] <- na.omit(misty::chr.gsub(numbers, unicode, print.object$with.cor[lower.tri(print.object$with.cor)][which(x$result$with.p[lower.tri(x$result$with.p)] <= x$args$alpha)]))
        print.object$with.cor[upper.tri(print.object$with.cor)][which(x$result$with.p[upper.tri(x$result$with.p)] <= x$args$alpha)] <- na.omit(misty::chr.gsub(numbers, unicode, print.object$with.cor[upper.tri(print.object$with.cor)][which(x$result$with.p[upper.tri(x$result$with.p)] <= x$args$alpha)]))

        print.object$betw.cor[lower.tri(print.object$betw.cor)][which(x$result$betw.p[lower.tri(x$result$betw.p)] <= x$args$alpha)] <- na.omit(misty::chr.gsub(numbers, unicode, print.object$betw.cor[lower.tri(print.object$betw.cor)][which(x$result$betw.p[lower.tri(x$result$betw.p)] <= x$args$alpha)]))
        print.object$betw.cor[upper.tri(print.object$betw.cor)][which(x$result$betw.p[upper.tri(x$result$betw.p)] <= x$args$alpha)] <- na.omit(misty::chr.gsub(numbers, unicode, print.object$betw.cor[upper.tri(print.object$betw.cor)][which(x$result$betw.p[upper.tri(x$result$betw.p)] <= x$args$alpha)]))

      }

    #-----------------------------------------------------------------------------------
    # Combined results
    } else {

      #........................................
      # Round and format

      print.object$wb.cor <- formatC(print.object$wb.cor, digits = digits, format = "f",
                                     zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

      print.object$wb.se <- formatC(print.object$wb.se, digits = digits, format = "f",
                                    zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

      print.object$wb.stat <- formatC(print.object$wb.stat, digits = digits, format = "f",
                                   zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

      print.object$wb.p <- formatC(print.object$wb.p, digits = p.digits, format = "f",
                                   zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

      diag(print.object$wb.cor) <- ""
      diag(print.object$wb.se) <- ""
      diag(print.object$wb.stat) <- ""
      diag(print.object$wb.p) <- ""

      # Missing entries
      print.object$wb.cor <- gsub("NA", "  ", print.object$wb.cor)
      print.object$wb.se <- gsub("NA", "  ", print.object$wb.se)
      print.object$wb.stat <- gsub("NA", "  ", print.object$wb.stat)
      print.object$wb.p <- gsub("NA", "  ", print.object$wb.p)

      #........................................
      # Row names

      row.names(print.object$wb.cor) <- paste0("   ", row.names(print.object$wb.cor))
      row.names(print.object$wb.se) <- paste0("   ", row.names(print.object$wb.se))
      row.names(print.object$wb.stat) <- paste0("   ", row.names(print.object$wb.stat))
      row.names(print.object$wb.p) <- paste0("   ", row.names(print.object$wb.p))

      print.object$wb.cor <- apply(print.object$wb.cor, 2L, function(y) format(y, justify = "right"))
      print.object$wb.se <- apply(print.object$wb.se, 2L, function(y) format(y, justify = "right"))
      print.object$wb.stat <- apply(print.object$wb.stat, 2L, function(y) format(y, justify = "right"))
      print.object$wb.p <- apply(print.object$wb.p, 2L, function(y) format(y, justify = "right"))

      #........................................
      # Statistically significant correlation coefficients in boldface

      if (isTRUE(x$args$sig)) {

        numbers <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
        unicode <- c("\U1D7CE", "\U1D7CF", "\U1D7D0", "\U1D7EF", "\U1D7F0", "\U1D7F1", "\U1D7F2", "\U1D7F3", "\U1D7F4", "\U1D7F5")

        print.object$wb.cor[lower.tri(print.object$wb.cor)][which(x$result$wb.p[lower.tri(x$result$wb.p)] <= x$args$alpha)] <- na.omit(misty::chr.gsub(numbers, unicode, print.object$wb.cor[lower.tri(print.object$wb.cor)][which(x$result$wb.p[lower.tri(x$result$wb.p)] <= x$args$alpha)]))
        print.object$wb.cor[upper.tri(print.object$wb.cor)][which(x$result$wb.p[upper.tri(x$result$wb.p)] <= x$args$alpha)] <- na.omit(misty::chr.gsub(numbers, unicode, print.object$wb.cor[upper.tri(print.object$wb.cor)][which(x$result$wb.p[upper.tri(x$result$wb.p)] <= x$args$alpha)]))

      }

    }

    #-----------------------------------------------------------------------------------
    # Print

    cat(" Within-Group and Between-Group Correlation Matrix\n")

    #........................
    # lavaan summary

    print.summary <- print.object$summary

    #.......
    # Format

    # Include spaces
    print.summary[1L, 1L] <- paste0(" ", print.summary[1L, 1L])
    print.summary[-1L, 1L] <- paste0("  ", print.summary[-1L, 1L])

    # Justify left
    print.summary[, 1L] <- format(print.summary[, 1L], justify = "left")

    #.......
    # Print

    print(print.summary, col.names = FALSE, row.names = FALSE, quote = FALSE, right = TRUE, max = 99999L)

    cat("\n")

    #--------------------------------------------
    # Split results
    if (isTRUE(x$args$split)) {

      #........................
      # Correlation coefficient
      if (isTRUE("cor" %in% print)) {

        # Within
        cat("  Within-Group\n")

        print(print.object$with.cor, quote = FALSE, right = TRUE, max = 99999L)

        # Between
        cat("\n  Between-Group\n")

        print(print.object$betw.cor, quote = FALSE, right = TRUE, max = 99999L)

      }

      #........................
      # Standard error

      if (isTRUE("se" %in% print)) {

        if (isTRUE("cor" %in% print)) { cat("\n") }

        cat(" Standard error \n\n")

        # Within
        cat("  Within-Group\n")

        print(print.object$with.se, quote = FALSE, right = TRUE, max = 99999L)

        # Between
        cat("\n  Between-Group\n")

        print(print.object$betw.se, quote = FALSE, right = TRUE, max = 99999L)

      }

      #........................
      # Test statistic

      if (isTRUE("stat" %in% print)) {

        if (isTRUE(any(c("cor", "se") %in% print))) { cat("\n") }

        cat(" Test Statistic (z value) \n\n")

        # Within
        cat("  Within-Group\n")

        print(print.object$with.stat, quote = FALSE, right = TRUE, max = 99999L)

        # Between
        cat("\n  Between-Group\n")

        print(print.object$betw.stat, quote = FALSE, right = TRUE, max = 99999L)

      }

      #........................
      # p.values

      if (isTRUE("p" %in% print)) {

        if (isTRUE(any(c("cor", "se", "stat") %in% print))) { cat("\n") }

        cat(" Significance Value (p-value)\n\n")

        # Within
        cat("  Within-Group\n")

        print(print.object$with.p, quote = FALSE, right = TRUE, max = 99999L)

        # Between
        cat("\n  Between-Group\n")

        print(print.object$betw.p, quote = FALSE, right = TRUE, max = 99999L)

        cat(paste0("\n  Adjustment for multiple testing: ", x$args$p.adj, "\n"))

      }

    #--------------------------------------------
    # Combined results
    } else {

      #........................
      # Correlation coefficient
      if (isTRUE("cor" %in% print)) {

        print(print.object$wb.cor, quote = FALSE, right = TRUE, max = 99999L)

      }

      #........................
      # Standard error

      if (isTRUE("se" %in% print)) {

        if (isTRUE("cor" %in% print)) { cat("\n") }

        cat(" Standard Error\n\n")

        print(print.object$wb.se, quote = FALSE, right = TRUE, max = 99999L)

      }

      #........................
      # Test statistic

      if (isTRUE("stat" %in% print)) {

        if (isTRUE(any(c("cor", "se") %in% print))) { cat("\n") }

        cat(" Test Statistic (z value) \n\n")

        print(print.object$wb.stat, quote = FALSE, right = TRUE, max = 99999L)

      }

      #........................
      # p.values

      if (isTRUE("p" %in% print)) {

        if (isTRUE(any(c("cor", "se", "stat") %in% print))) { cat("\n") }

        cat(" Significance Value (p-value)\n\n")

        print(print.object$wb.p, quote = FALSE, right = TRUE, max = 99999L)

        cat(paste0("\n  Adjustment for multiple testing: ", x$args$p.adj, "\n"))

      }

    }

    #--------------------------------------------
    # Note

    # Sample size within and between
    cat(paste0("\n Note. n(within) = ", lavaan::lavInspect(x$mod.fit, what = "nobs"), ", n(between) = ", lavaan::lavInspect(x$mod.fit, what = "nclusters")), "\n")

    # Lower and upper triangular
    if (isTRUE(!x$args$split)) {

      cat(ifelse(isTRUE(x$args$tri.lower), "       Lower triangular: Within-Group, Upper triangular: Between-Group",
                                           "\n       Lower triangular: Between-Group, Upper triangular: Within-Group"), "\n")

    }

    # Statistical significance
    if (isTRUE(x$args$sig)) {

      cat(paste0("       Statistically significant coefficients at \U03B1 = ", signif(x$args$alpha, digits = 2L), " are boldface\n"))

    }

  #_____________________________________________________________________________
  #
  #  Multilevel Descriptive Statistics -----------------------------------------
  }, multilevel.descript = {

    ####################################################################################
    # Main Function

    print.object <- data.frame(cbind(c("No. of cases", "No. of missing values",
                                       "", "No. of clusters", "Average cluster size", "SD cluster size", "Min cluster size", "Max cluster size",
                                       "", "Mean", "Variance Within", "Variance Between", "ICC(1)", "ICC(2)",
                                       "", "Design effect", "Design effect sqrt", "Effective sample size"),
                                     rbind(x$result$no.obs, x$result$no.miss,
                                           "", x$result$no.cluster, x$result$m.cluster.size, x$result$sd.cluster.size, x$result$min.cluster.size, x$result$max.cluster.size,
                                           "", x$result$mean.x, x$result$var.w, x$result$var.b, x$result$icc1, x$result$icc2,
                                           "", x$result$deff, x$result$deff.sqrt, x$result$n.effect)),
                               stringsAsFactors = FALSE)

    #-----------------------------------------
    # One variable
    if (isTRUE(length(x$result$no.obs) == 1L)) {

      # Format
      for (i in c(5L, 6L, 10L, 11L, 12L, 16L, 17L, 18L)) {

        print.object[i, 2L] <- formatC(as.numeric(unlist(print.object[i, 2L])), digits = digits, format = "f",
                                       zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

      }

      print.object[13L, 2L] <- formatC(as.numeric(unlist(print.object[13L, 2L])), digits = icc.digits, format = "f",
                                       zero.print = ifelse(icc.digits > 0L, paste0("0.", paste(rep(0L, times = icc.digits), collapse = "")), "0"))
      print.object[14L, 2L] <- formatC(as.numeric(unlist(print.object[14L, 2L])), digits = icc.digits, format = "f",
                                       zero.print = ifelse(icc.digits > 0L, paste0("0.", paste(rep(0L, times = icc.digits), collapse = "")), "0"))

      print.object[, 1L] <- paste(" ", print.object[, 1L])


      print.object[, 1L] <- format(print.object[, 1L, drop = FALSE])

      print.object[, 1L] <- format(unlist(print.object[, 1L]), justify = "left")
      print.object[, 2L] <- format(as.character(print.object[, 2L]), justify = "right")

    #-----------------------------------------
    # More than one variable
    } else {

      print.object <- rbind(c("", names(x$result$no.obs)), print.object)

      # Format
      for (i in c(6L, 7L, 11L, 12L, 13L, 17L, 18L, 19L)) {

        print.object[i, 2L:ncol(print.object)] <- formatC(as.numeric(unlist(print.object[i, 2L:ncol(print.object)])), digits = digits, format = "f",
                                                          zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

      }

      print.object[14L, 2L:ncol(print.object)] <- formatC(as.numeric(unlist(print.object[14L, 2L:ncol(print.object)])), digits = icc.digits, format = "f",
                                                          zero.print = ifelse(icc.digits > 0L, paste0("0.", paste(rep(0L, times = icc.digits), collapse = "")), "0"))
      print.object[15L, 2L:ncol(print.object)] <- formatC(as.numeric(unlist(print.object[15L, 2L:ncol(print.object)])), digits = icc.digits, format = "f",
                                                          zero.print = ifelse(icc.digits > 0L, paste0("0.", paste(rep(0L, times = icc.digits), collapse = "")), "0"))

      print.object[, 1L] <- paste(" ", print.object[, 1L])


      print.object[, 1L] <- format(print.object[, 1L, drop = FALSE])

      print.object[, 1L] <- format(unlist(print.object[, 1L]), justify = "left")
      print.object[, 2L:ncol(print.object)] <- apply(print.object[, 2L:ncol(print.object)], 2L, function(y) format(as.character(y), justify = "right"))

    }

    ####################################################################################
    # Output

    cat(" Multilevel Descriptive Statistics\n\n")

    write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE)

  #_____________________________________________________________________________
  #
  # CI for the Indirect Effect in a 1-1-1 Multilevel Mediation Model -----------
  }, multilevel.indirect = {

    #....................
    # Round

    print.object$mc <- sapply(c("est", "se", "low", "upp"), function(y) ifelse(!is.na(print.object$mc [, y]),
                                                                               formatC(print.object$mc[, y], digits = digits, format = "f",
                                                                                       zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))
    #....................
    # Print names

    print.object$mc <- rbind(c("Est.", "SE", "Low", "Upp"), print.object$mc)

    #....................
    # Justify right

    print.object$mc <- apply(print.object$mc, 2L, format, justify = "right")


    #....................
    # Add blank space

    print.object$mc[, "est"] <- paste0("   ", print.object$mc[, "est"])

    #-----------------------------------------------------------------------------------
    # Print

    cat(paste(switch(x$args$alternative,
                     two.sided = " Two-Sided",
                     less = " One-Sided",
                     greater = " One-Sided"),
              paste0(round(x$args$conf.level * 100L, digits = 2L), "%"), "Confidence Interval: Indirect Effect\n\n"))

    cat("  Monte Carlo Method with",  format(x$args$nrep, scientific = FALSE), "repetitions\n")

    write.table(print.object$mc, quote = FALSE, row.names = FALSE, col.names = FALSE)

  #_____________________________________________________________________________
  #
  # R-Squared Measures for Multilevel and Linear Mixed Effects Models ----------
  }, multilevel.r2 = {

    ####################################################################################
    # Main Function

    if (isTRUE("RS" %in% x$args$print)) {

      print.object$rs$total <- data.frame(sapply(print.object$rs$total[, !is.na(print.object$rs$total)], formatC, digits = digits, format = "f", simplify = FALSE))
      row.names(print.object$rs$total) <- "   "

      if (isTRUE(ncol(print.object$rs$decomp) != 1L)) {

        print.object$rs$within <- data.frame(sapply(print.object$rs$within, formatC, digits = digits, format = "f", simplify = FALSE))
        row.names(print.object$rs$within) <- "   "

        print.object$rs$between <- data.frame(sapply(print.object$rs$between, formatC, digits = digits, format = "f", simplify = FALSE))
        row.names(print.object$rs$between) <- "   "

      }

    }

    ####################################################################################
    # Output

    cat(" R-Squared Measures for Multilevel and Linear Mixed Effects Models\n\n")

    if (isTRUE("RB" %in% x$args$print)) {

      cat("  Reduction in Residual Variance (Raudenbush and Bryk, 2002)\n\n")

       cat(paste0("    Within-Cluster ", ifelse(isTRUE(getOption("knitr.in.progress")), "R2: ", "R\u00B2: "), formatC(as.numeric(print.object$rb["rb1"]), digits = digits, format = "f")), "\n",
           paste0("  Between-Cluster ", ifelse(isTRUE(getOption("knitr.in.progress")), "R2: ", "R\u00B2: "), formatC(as.numeric(print.object$rb["rb2"]), digits = digits, format = "f")), "\n")

    }

    if (isTRUE("SB" %in% x$args$print)) {

      if (isTRUE("RB" %in% x$args$print)) cat("\n")

      cat("  Reduction in Mean Squared Prediction Error (Snijders and Bosker, 1994)\n\n")

       cat(paste0("             Total ", ifelse(isTRUE(getOption("knitr.in.progress")), "R2: ", "R\u00B2: "), formatC(as.numeric(print.object$sb["sb1"]), digits = digits, format = "f")), "\n",
           paste0("  Between-Cluster ", ifelse(isTRUE(getOption("knitr.in.progress")), "R2: ", "R\u00B2: "), formatC(as.numeric(print.object$sb["sb2"]), digits = digits, format = "f")), "\n")

    }

    if (isTRUE("NS" %in% x$args$print)) {

      if (isTRUE(any(c("RB", "SB") %in% x$args$print))) cat("\n")

      cat("  Variance Partitioning (Nakagawa and Schielzeth, 2013; Johnson, 2014)\n\n")

       cat(paste0("      Marginal ", ifelse(isTRUE(getOption("knitr.in.progress")), "R2: ", "R\u00B2: "), formatC(as.numeric(print.object$ns["marg"]), digits = digits, format = "f")), "\n",
           paste0("  Conditional ", ifelse(isTRUE(getOption("knitr.in.progress")), "R2: ", "R\u00B2: "), formatC(as.numeric(print.object$ns["cond"]), digits = digits, format = "f")), "\n")

    }

    if (isTRUE("RS" %in% x$args$print)) {

      if (isTRUE(any(c("RB", "SB", "NS") %in% x$args$print))) cat("\n")

      cat("  Integrative Framework of R-Squared Measures (Rights and Sterba, 2019)\n\n")

       cat(ifelse(isTRUE(getOption("knitr.in.progress")), "   Total R2\n", "   Total R\u00B2\n"))

        print(print.object$rs$total, quote = FALSE, max = 99999L, right = TRUE)

       # Predictors are cluster-mean-centered
       if (isTRUE(ncol(print.object$rs$decomp) != 1L)) {

         cat(ifelse(isTRUE(getOption("knitr.in.progress")), "\n   Within-Cluster R2\n", "\n   Within-Cluster R\u00B2\n"))

          print(print.object$rs$within, quote = FALSE, max = 99999L, right = TRUE)

         cat(ifelse(isTRUE(getOption("knitr.in.progress")), "\n   Between-Cluster R2\n", "\n   Between-Cluster R\u00B2\n"))

          print(print.object$rs$between, quote = FALSE, max = 99999L, right = TRUE)

      }

    }

  #_____________________________________________________________________________
  #
  # R-Squared Measures for Multilevel and Linear Mixed Effects Models Manual ----
  }, multilevel.r2.manual = {

    ####################################################################################
    # Main Function

    print.object$rs$total <- data.frame(sapply(print.object$rs$total[, !is.na(print.object$rs$total)], formatC, digits = digits, format = "f", simplify = FALSE))
    row.names(print.object$rs$total) <- "   "

    if (isTRUE(ncol(print.object$rs$decomp) != 1L)) {

      print.object$rs$within <- data.frame(sapply(print.object$rs$within, formatC, digits = digits, format = "f", simplify = FALSE))
      row.names(print.object$rs$within) <- "   "

      print.object$rs$between <- data.frame(sapply(print.object$rs$between, formatC, digits = digits, format = "f", simplify = FALSE))
      row.names(print.object$rs$between) <- "   "

    }

    ####################################################################################
    # Output

    cat(" R-Squared Measures for Multilevel and Linear Mixed Effects Models\n\n")

    cat("  Integrative Framework of R-Squared Measures (Rights and Sterba, 2019)\n\n")

    cat(ifelse(isTRUE(getOption("knitr.in.progress")), "   Total R2\n", "   Total R\u00B2\n"))

    print(print.object$rs$total, quote = FALSE, max = 99999L, right = TRUE)

    # Predictors are cluster-mean-centered
    if (isTRUE(ncol(print.object$rs$decomp) != 1L)) {

      cat(ifelse(isTRUE(getOption("knitr.in.progress")), "\n   Within-Cluster R2\n", "\n   Within-Cluster R\u00B2\n"))

      print(print.object$rs$within, quote = FALSE, max = 99999L, right = TRUE)

      cat(ifelse(isTRUE(getOption("knitr.in.progress")), "\n   Between-Cluster R2\n", "\n   Between-Cluster R\u00B2\n"))

      print(print.object$rs$between, quote = FALSE, max = 99999L, right = TRUE)

    }

  #_____________________________________________________________________________
  #
  # Auxiliary variables analysis -----------------------------------------------
  }, na.auxiliary = {

    ####################################################################################
    # Main Function

    #-----------------------------------------
    # Format correlation matrix
    print.object$cor <- apply(print.object$cor, 2L, function(y) formatC(as.numeric(y), digits = digits, format = "f",
                                                                       zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")))

    # Lower and/or upper triangular
    switch(tri, "lower" = {

      print.object$cor[upper.tri(print.object$cor)] <- ""

    }, "upper" = {

      print.object$cor[lower.tri(print.object$cor)] <- ""

    })

    diag(print.object$cor) <- ""

    #-----------------------------------------
    # Format Cohen's d matrix

    print.object$d <- apply(print.object$d, 2L, function(y) formatC(as.numeric(y), digits = digits, format = "f",
                                                                   zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")))
    diag(print.object$d) <- ""

    # Print table
    print.object  <- data.frame(cbind(c("", colnames(print.object$cor), "", colnames(print.object$cor)),
                                      rbind(colnames(print.object$cor), print.object$cor, colnames(print.object$cor), print.object$d)),
                                stringsAsFactors = FALSE)

    # Format
    print.object[, 1L] <- paste0("   ", print.object[, 1L])

    print.object[, 1L] <- format(print.object[, 1L], justify = "left")

    print.object[, -1L] <- apply(print.object[, -1L], 2L, function(y) format(y, justify = "right"))

    ####################################################################################
    # Output

    cat(" Auxiliary Variables\n\n",
        " Variables related to the incomplete variable\n\n",
        "  Pearson product-moment correlation matrix\n")

    write.table(print.object[1L:(nrow(x$result$cor) + 1L), ], quote = FALSE, row.names = FALSE, col.names = FALSE)

    cat("\n")

    cat("  Variables related to the probability of missigness\n\n",
        "  Cohen's d\n")

    write.table(print.object[(nrow(x$result$cor) + 2L):nrow(print.object), ], quote = FALSE, row.names = FALSE, col.names = FALSE)

    cat("\n",
        " Note. Indicator variables are in the rows (0 = miss, 1 = obs)\n")

  #_____________________________________________________________________________
  #
  # Variance-Covariance Coverage -----------------------------------------------
  }, na.coverage = {

    #-----------------------------------------------------------------------------------
    # Main Function

    #........................................
    # Lower and/or upper triangular

    switch(tri, "lower" = {

      print.object[upper.tri(print.object)] <- ""

    }, "upper" = {

      print.object[lower.tri(print.object)] <- ""

    })

    #-----------------------------------------------------------------------------------
    # Main Function

    # Format proportions
    print.object <- apply(print.object, 2L, function(y) ifelse(!is.na(as.numeric(y)), formatC(as.numeric(y), digits = digits, format = "f",
                                                                                              zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), ""))

    # Row names
    row.names(print.object) <- paste0("  ", row.names(x$result))

    #-----------------------------------------------------------------------------------
    # Output

    cat(" Variance-Covariance Coverage\n\n")

    print(print.object,  quote = FALSE, row.names = FALSE, max = 99999L, right = TRUE)

  #_____________________________________________________________________________
  #
  # Descriptive Statistics for Missing Data ------------------------------------
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
                                        zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), justify = "right")
    restab$no[1L:7L] <- format(formatC(as.numeric(restab$no[1L:7L]), digits = 0L, format = "f"), justify = "right")
    restab$no <- format(restab$no, justify = "right")

    restab$perc[restab$perc != ""] <- paste0("(", formatC(as.numeric(restab$perc[restab$perc != ""]), digits = digits, format = "f",
                                                          zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), "%)")

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

      freqtab[, c("pObs", "pNA")] <- apply(freqtab[, c("pObs", "pNA")], 2L, function(y) paste0(formatC(y, digits = digits, format = "f",
                                                                                                      zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), "%"))
      freqtab <- rbind(colnames(freqtab), freqtab)

      freqtab[, -1L] <- apply(freqtab[ -1L], 2L, format, justify = "right")

      freqtab[, 1L] <- paste0("    ", format(freqtab[, 1L], justify = "left"))

      cat("\n")
      write.table(freqtab, quote = FALSE, row.names = FALSE, col.names = FALSE)

    }

  #_____________________________________________________________________________
  #
  # Missing Data Pattern -------------------------------------------------------
  }, na.pattern = {

    #---------------------------------------------------------------------------------
    # Main Function

    # NA
    print.object[, "pattern"][is.na(print.object[, "pattern"])] <- ""
    print.object[, "nNA"][is.na(print.object[, "nNA"])] <- ""

    # Percentages
    print.object[, "perc"] <- paste0(formatC(as.numeric(print.object[, "perc"]), digits = digits, format = "f",
                                             zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), "%")
    print.object[, "pNA"] <- paste0(formatC(as.numeric(print.object[, "pNA"]), digits = digits, format = "f",
                                            zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), "%")
    print.object[nrow(print.object), ncol(print.object)] <- ""

    # Format
    colnames(print.object)[1L] <- " Pattern"


    #---------------------------------------------------------------------------------
    # Output

    cat(" Missing Data Pattern\n\n")

    print(print.object, row.names = FALSE, max = 99999L, right = TRUE)

  #_____________________________________________________________________________
  #
  # Little's MCAR test ---------------------------------------------------------
  }, na.test = {

    #---------------------------------------------------------------------------------
    # Round

    print.object$statistic  <- formatC(print.object$statistic , digits = digits, format = "f",
                                       zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

    print.object$pval <- formatC(print.object$pval, digits = p.digits, format = "f",
                                 zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

    #---------------------------------------------------------------------------------
    # Format

    print.object <- rbind(c("n", "nIncomp", "nPattern", "chi2", "df", "pval"), print.object)

    print.object <- apply(print.object, 2L, format, justify = "right")

    # R Markdown in progress
    if (is.null(getOption("knitr.in.progress"))) {

      print.object[1L, "statistic"] <- paste0(paste0(rep(" ", times = nchar(print.object[1L, "statistic"]) - 2L), collapse = ""), "\u03C7\u00B2", collapes = "")

    }

    print.object[, 1L] <- paste(" ", print.object[, 1L])

    #.....................................
    # Print output

    cat(paste0(" Little's MCAR Test\n\n"))

    cat(paste(print.object[1L, ], collapse = " "), "\n")
    write.table(print.object[-1L, , drop = FALSE], quote = FALSE, row.names = FALSE, col.names = FALSE, na = "")

  #_____________________________________________________________________________
  #
  # Coefficient Omega ----------------------------------------------------------
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
                                          zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

      print.object$omega$low <- formatC(print.object$omega$low, digits = digits, format = "f",
                                        zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))
      print.object$omega$upp <- formatC(print.object$omega$upp, digits = digits, format = "f",
                                        zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

      print.object$omega <- rbind(c("  n", "Items", "Omega", "Low", "Upp"), print.object$omega)

      print.object$omega <- apply(print.object$omega, 2L, function(y) format(y, justify = "right"))

      if (isTRUE(x$args$type == "omega")) {

        cat(paste0(ifelse(isTRUE(x$args$std), " Standardized ", " Unstandardized "), "Coefficient Omega with ",
                   x$args$conf.level*100L, "% Confidence Interval\n\n"))

      } else  if (isTRUE(x$args$type == "hierarch")) {

        cat(paste0(ifelse(isTRUE(x$args$std), " Standardized ", " Unstandardized "), "Hierarchical Omega with ",
                   x$args$conf.level*100L, "% Confidence Interval\n\n"))

      } else if (isTRUE(x$args$type == "categ")) {

        cat(paste0(" Categorical Omega with ", x$args$conf.level*100L, "% Confidence Interval\n\n"))

      }

      write.table(print.object$omega, quote = FALSE, row.names = FALSE, col.names = FALSE)

    }

    #-----------------------------------------
    # Item statistics

    if (isTRUE("item" %in% print)) {

      print.object$item$pNA <- paste0(formatC(print.object$item$pNA, digits = 2L, format = "f",
                                              zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), "%")
      print.object$item$m <- formatC(print.object$item$m, digits = 2L, format = "f",
                                     zero.print = paste0("0.", paste(rep(0L, times = digits), collapse = "")))
      print.object$item$sd <- formatC(print.object$item$sd, digits = 2L, format = "f",
                                      zero.print = paste0("0.", paste(rep(0L, times = digits), collapse = "")))
      print.object$item$min <- formatC(print.object$item$min, digits = 2L, format = "f",
                                       zero.print = paste0("0.", paste(rep(0L, times = digits), collapse = "")))
      print.object$item$max <- formatC(print.object$item$max, digits = 2L, format = "f",
                                       zero.print = paste0("0.", paste(rep(0L, times = digits), collapse = "")))

      print.object$item$std.ld <- formatC(print.object$item$std.ld, digits = digits, format = "f",
                                          zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))
      print.object$item$omega <- formatC(print.object$item$omega, digits = digits, format = "f",
                                         zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

      print.object$item <- rbind(c("Variable", "n", "nNA", "pNA", "M", "SD", "Min", "Max", "Std.Ld", "Omega"),
                                 print.object$item)

      # Format
      print.object$item[, 1L] <- format(paste(" ", print.object$item[, 1L]), justify = "left")
      print.object$item[, -1L] <- apply(print.object$item[, -1L], 2L, function(y) format(y, justify = "right"))

      if (isTRUE("omega" %in% print)) { cat("\n") }

      if (isTRUE(x$args$type == "omega")) {

        cat(" Standardized Factor Loadings and Coefficient Omega if Item Deleted\n\n")

      } else if (isTRUE(x$args$type == "hierarch")) {

        cat(" Standardized Factor Loadings and Hierarchical Omega if Item Deleted\n\n")

      } else if (isTRUE(x$args$type == "categ")) {

        cat(" Standardized Factor Loadings and Categorical Omega if Item Deleted\n\n")

      }

      write.table(print.object$item, quote = FALSE, row.names = FALSE, col.names = FALSE)

    }

  #_____________________________________________________________________________
  #
  # Phi Coefficient ------------------------------------------------------------
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
                                                    zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")))

    #----------------------------------------
    # More than two variables
    } else {

      # Format contingency coefficients
      print.object <- formatC(print.object, digits = digits, format = "f",
                              zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

      # Lower and/or upper triangular
      switch(tri, "lower" = {

        print.object[upper.tri(print.object)] <- ""

      }, "upper" = {

        print.object[lower.tri(print.object)] <- ""

      })

      # Set diagonal to "
      diag(print.object) <- ""

      # Format
      row.names(print.object) <- paste(" ", row.names(print.object))

    }

    ####################################################################################
    # Output

    if (isTRUE(is.null(dim(x$result)))) {

      if (isTRUE(x$args$adjust)) {

        cat(" Adjusted Phi Coefficient\n\n")

      } else {

        cat(" Phi Coefficient\n\n")

      }

    } else {

      if (isTRUE(x$args$adjust)) {

        cat(" Adjusted Phi Coefficient Matrix\n\n")

      } else {

        cat(" Phi Coefficient Matrix\n\n")

      }

    }

    if (isTRUE(is.null(dim(x$result)))) {

      write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE)

    } else {

      print(print.object, quote = FALSE, right = TRUE)

    }

  #_____________________________________________________________________________
  #
  # Polychoric Correlation Matrix ----------------------------------------------------------
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
                            zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

    row.names(print.object) <- paste(" ", row.names(print.object))


    # Empty matrix diagonal
    diag(print.object) <- ""

    #----------------------------------------
    # Lower and/or upper triangular

    switch(tri, "lower" = {

      print.object[upper.tri(print.object)] <- ""

    }, "upper" = {

      print.object[lower.tri(print.object)] <- ""

    })

    #----------------------------------------
    # Row names
    if (isTRUE(!is.null(rownames(print.object)))) {

      row.names(print.object) <- format(row.names(print.object), justify = "left")

    }

    ####################################################################################
    # Output

    cat(" Polychoric Correlation Matrix\n\n")

    print(print.object, quote = FALSE, right = TRUE, max = 99999L)

  #_____________________________________________________________________________
  #
  # Sample Size Determination --------------------------------------------------
  }, size = {

    ####################################################################################
    # Main function

    #----------------------------------------
    # Arithmetic mean
    switch(x$size, mean = {

      cat(" Sample Size Determination:", ifelse(x$args$sample == "one.sample", "One-Sample", "Two-Sample"), "t-Test\n\n")

      #............................
      # R Markdown in progress
      if (isTRUE(getOption("knitr.in.progress"))) {

        # one-sample
        if (isTRUE(x$args$sample == "one.sample")) {

          switch(x$args$alternative,
                 two.sided = {

                   cat("  Null hypothesis        H0: mu =", x$args$delta, "\n",
                       " Alternative hypothesis H1: mu !=", x$args$delta, "\n\n")

                 }, less = {

                   cat("  Null hypothesis        H0: mu >=", x$args$delta, "\n",
                       " Alternative hypothesis H1: mu <", x$args$delta, "\n\n")


                 }, greater = {

                   cat("  Null hypothesis        H0: mu <=", x$args$delta, "\n",
                       " Alternative hypothesis H1: mu >", x$args$delta, "\n\n")

                 })

        # two-sample
        } else {

          switch(x$args$alternative,
                 two.sided = {

                   cat("  Null hypothesis        H0: mu1 = mu2\n",
                       " Alternative hypothesis H1: mu1 != mu2\n\n")

                 }, less = {

                   cat("  Null hypothesis        H0: mu1 >= mu2\n",
                       " Alternative hypothesis H1: mu1 < mu2\n\n")


                 }, greater = {

                   cat("  Null hypothesis        H0: mu1 <= mu2\n",
                       " Alternative hypothesis H1: mu1 > mu2\n\n")

                 })

        }

        ###

        cat("  alpha =", x$args$alpha, " beta =", x$args$beta, " gamma =", x$args$delta, "\n\n")

        if (isTRUE(x$args$sample == "one.sample")) {

          cat("  optimal sample size: n =", ceiling(x$result$n), "\n")

        } else {

          cat("  optimal sample size: n =", ceiling(x$result$n), "in each group \n")

        }

      #............................
      # R Markdown not in progress
      } else {

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

          cat("  optimal sample size: n =", ceiling(x$result$n), "\n")

        } else {

          cat("  optimal sample size: n =", ceiling(x$result$n), "in each group \n")

        }

      }

    #----------------------------------------
    # Proportion
    }, prop = {

      if (isTRUE(x$args$correct)) {

        cat(" Sample Size Determination:", ifelse(x$args$sample == "one.sample", "One-Sample", "Two-Sample"), "Proportion Test with Continuity Correction\n\n")

      } else {

        cat(" Sample Size Determination:", ifelse(x$args$sample == "one.sample", "One-Sample", "Two-Sample"), "Proportion Test without Continuity Correction\n\n")

      }

      ###

      #............................
      # R Markdown in progress
      if (isTRUE(getOption("knitr.in.progress"))) {

        # one-sample
        if (isTRUE(x$args$sample == "one.sample")) {

          switch(x$args$alternative,
                 two.sided = {

                   cat("  Null hypothesis        H0: pi =", x$args$pi, "\n",
                       " Alternative hypothesis H1: pi !=", x$args$pi, "\n\n")

                 }, less = {

                   cat("  Null hypothesis        H0: pi >=", x$args$pi, "\n",
                       " Alternative hypothesis H1: pi <", x$args$pi, "\n\n")


                 }, greater = {

                   cat("  Null hypothesis        H0: pi <=", x$args$pi, "\n",
                       " Alternative hypothesis H1: pi >", x$args$pi, "\n\n")

                 })

        # two-sample
        } else {

          switch(x$args$alternative,
                 two.sided = {

                   cat("  Null hypothesis        H0: pi1 = pi2\n",
                       " Alternative hypothesis H1: pi1 != pi2\n\n")

                 }, less = {

                   cat("  Null hypothesis        H0: pi1 >= pi2\n",
                       " Alternative hypothesis H1: pi1 < pi2\n\n")


                 }, greater = {

                   cat("  Null hypothesis        H0: pi1 <= pi2\n",
                       " Alternative hypothesis H1: pi1 > pi2\n\n")

                 })


        }

        cat("  alpha =", x$args$alpha, " beta =", x$args$beta, " delta =", x$args$delta, "\n\n")

        ###

        if (isTRUE(x$args$sample == "one.sample")) {

          cat("  optimal sample size: n =", ceiling(x$result$n), "\n")

        } else {

          cat("  optimal sample size: n =", ceiling(x$result$n), "in each group \n")

        }

      #............................
      # R Markdown not in progress
      } else {

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

          cat("  optimal sample size: n =", ceiling(x$result$n), "\n")

        } else {

          cat("  optimal sample size: n =", ceiling(x$result$n), "in each group \n")

        }

      }

    #----------------------------------------
    # Correlation coefficient
    }, cor = {

      cat(" Sample Size Determination: Pearson's Product-Moment Correlation Coefficient\n\n")

      #............................
      # R Markdown in progress
      if (isTRUE(getOption("knitr.in.progress"))) {

        switch(x$args$alternative,
               two.sided = {

                 cat("  Null hypothesis        H0: rho =", x$args$rho, "\n",
                     " Alternative hypothesis H1: rho !=", x$args$rho, "\n\n")

               }, less = {

                 cat("  Null hypothesis        H0: rho >=", x$args$rho, "\n",
                     " Alternative hypothesis H1: rho <", x$args$rho, "\n\n")


               }, greater = {

                 cat("  Null hypothesis        H0: rho <=", x$args$rho, "\n",
                     " Alternative hypothesis H1: rho >", x$args$rho, "\n\n")

               })

        ###

        cat("  alpha =", x$args$alpha, " beta =", x$args$beta, " delta =", x$args$delta, "\n\n")


        ###

        cat("  optimal sample size: n =", ceiling(x$result$n), "\n")

      #............................
      # R Markdown not in progress
      } else {

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

        cat("  \u03B1 =", x$args$alpha, " \u03B2 =", x$args$beta, " \u03B4 =", x$args$delta, "\n\n")


        ###

        cat("  optimal sample size: n =", ceiling(x$result$n), "\n")

      }

    })

  #_____________________________________________________________________________
  #
  # Standardized Coefficients --------------------------------------------------
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
    print.object <- print.object$coef

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
    print.object[, -4L] <- apply(print.object[, -4L], 2L, function(y) formatC(y, digits = digits, format = "f",
                                                                              zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")))

    print.object[, 4L] <- formatC(as.numeric(print.object[, 4L]), digits = p.digits, format = "f",
                                  zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

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

      cat("\n  Note. SD of the criterion variable", names(x$result$sd)[1L], "=", round(x$result$sd[1L], digits = digits))

    }

  #_____________________________________________________________________________
  #
  # Levene's Test for Homogeneity of Variance ----------------------------------
  }, test.levene = {

    #---------------------------------------------------------
    # descript object

    # Round
    print.object[["descript"]][, c("m", "sd", "var", "low", "upp")] <- sapply(c("m", "sd", "var", "low", "upp"),
                                                                              function(y) ifelse(!is.na(print.object[["descript"]][, y]),
                                                                                                 formatC(print.object[["descript"]][, y], digits = digits, format = "f",
                                                                                                         zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

    print.object[["descript"]] <- print.object[["descript"]][, -2L]

    # Col names
    print.object[["descript"]] <- rbind(c("Group", "n", "nNA", "M", "SD", "Var", "Low", "Upp"), print.object[["descript"]])

    # Format
    print.object[["descript"]][, 1L] <- format(print.object[["descript"]][, 1L], justify = "left")

    print.object[["descript"]][, -1L] <- apply(print.object[["descript"]][, -1L], 2L, format, justify = "right")

    print.object[["descript"]][1L, 1L] <- paste0(" ", print.object[["descript"]][1L, 1L], collapse = "")
    print.object[["descript"]][-1L, 1L] <- paste0("  ", print.object[["descript"]][-1L, 1L])

    print.object[["descript"]][, 1L] <- format(misty::chr.trim(print.object[["descript"]][, 1L], side = "right"), justify = "left")

    print.object[["descript"]][, 1L] <- paste("", print.object[["descript"]][, 1L])

    #---------------------------------------------------------
    # test object

    #.....................................
    # Round

    print.object[["test"]][, "Sum Sq"] <- formatC(print.object[["test"]][, "Sum Sq"], digits = digits, format = "f",
                                                  zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))
    print.object[["test"]][, "Mean Sq"] <- formatC(print.object[["test"]][, "Mean Sq"], digits = digits, format = "f",
                                                   zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))
    print.object[["test"]][1L, "F value"] <- formatC(print.object[["test"]][1L, "F value"], digits = digits, format = "f",
                                                     zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))
    print.object[["test"]][1L, "Pr(>F)"] <- formatC(print.object[["test"]][1L, "Pr(>F)"], digits = p.digits, format = "f",
                                                    zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

    #.....................................
    # Format

    print.object[["test"]] <- rbind(c("Df", "Sum Sq", "Mean Sq", "F", "pval"), print.object[["test"]])
    print.object[["test"]] <- cbind(c("", "  Group", "  Residuals"), print.object[["test"]])

    print.object[["test"]][3L, c("F value", "Pr(>F)")] <- ""

    print.object[["test"]][, -1L] <- apply(print.object[["test"]][, -1L], 2L, format, justify = "right")
    print.object[["test"]][, 1L] <- format(print.object[["test"]][, 1L], justify = "left")

    #---------------------------------------------------------
    # Print output

    cat(" Levene's Test based on the", switch(x$args$method, median = "Median\n\n",
                                                mean = "Arithmetic Mean\n\n"))

    # Print hypotheses
    if (isTRUE(hypo)) {

      # R Markdown in progress
      if (isTRUE(getOption("knitr.in.progress"))) {

        if (isTRUE(length(unique(x$data[, 2L])) == 2L)) {

          cat("  Null hypothesis        H0: sigma2.1 = sigma2.2\n",
              " Alternative hypothesis H1: sigma2.1 != sigma2.2\n\n")

        } else {

          cat("  Null hypothesis        H0: sigma2.i = sigma2.j for all i and j\n",
              " Alternative hypothesis H1: sigma2.i != sigma2.j for at least one i != j \n\n")
        }

      } else {

        if (isTRUE(length(unique(x$data[, 2L])) == 2L)) {

          cat("  Null hypothesis        H0: \u03C3\u00B2\u2081 = \u03C3\u00B2\u2082\n",
              " Alternative hypothesis H1: \u03C3\u00B2\u2081 \u2260 \u03C3\u00B2\u2082\n\n")

        } else {

          cat("  Null hypothesis        H0: \u03C3\u00B2\u1D62 = \u03C3\u00B2\u2C7C for all i and j\n",
              " Alternative hypothesis H1: \u03C3\u00B2\u1D62 \u2260 \u03C3\u00B2\u2C7C for at least one i \u2260 j \n\n")
        }

      }

    }

    # Print descriptive statistics
    if (isTRUE(descript)) {

      write.table(print.object[["descript"]], quote = FALSE, row.names = FALSE, col.names = FALSE, na = "")

      cat("\n")

    }

    write.table(print.object[["test"]], quote = FALSE, row.names = FALSE, col.names = FALSE, na = "")

  #_____________________________________________________________________________
  #
  # t-test ---------------------------------------------------------------------
  }, test.t = {

    #---------------------------------------------------------
    # One sample t-test
    switch(x$sample, one = {

      #.....................................
      # Round

      print.object[, c("m", "sd", "m.diff", "se", "m.low", "m.upp", "t", "d", "d.low", "d.upp")] <- vapply(print.object[, c("m", "sd", "m.diff", "se", "m.low", "m.upp", "t", "d", "d.low", "d.upp")], formatC,
                                                                                                           digits = digits, format = "f",
                                                                                                           zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"), FUN.VALUE = character(1L))

      print.object[, "pval"] <- formatC(print.object[, "pval"], digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

      #.....................................
      # Format

      print.object <- rbind(c("n", "nNA", "M", "SD", "M.Diff", "SE", "Low", "Upp", "t", "df", "pval", "d", "Low", "Upp"), print.object)

      print.object <- apply(print.object, 2L, format, justify = "right")

      print.object[, 1L] <- paste(" ", print.object[, 1L])

      #.....................................
      # Print output

      cat(paste0(" One-Sample t-Test\n\n"))

      # Print hypotheses
      if (isTRUE(hypo)) {

        # R Markdown in progress
        if (isTRUE(getOption("knitr.in.progress"))) {

          switch(x$args$alternative,
                 two.sided = {

                   cat("  Null hypothesis        H0: mu =", x$args$mu, "\n",
                       " Alternative hypothesis H1: mu !=", x$args$mu, "\n\n")

                 }, less = {

                   cat("  Null hypothesis        H0: mu >=", x$args$mu, "\n",
                       " Alternative hypothesis H1: mu <", x$args$mu, "\n\n")


                 }, greater = {

                   cat("  Null hypothesis        H0: mu <=", x$args$mu, "\n",
                       " Alternative hypothesis H1: mu >", x$args$mu, "\n\n")

                 })

        } else {

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

      }

      # Print Cohen's d
      if (!isTRUE(effsize)) {

        print.object <- print.object[, -which(colnames(print.object) %in% c("d", "d.low", "d.upp"))]

      }

      # Print descriptive statistics
      if (!isTRUE(descript)) {

        print.object <- print.object[, -which(colnames(print.object) %in% c("n", "nNA", "m", "sd", "m.diff", "se", "m.low", "m.upp"))]

        print.object[, 1L] <- paste(" ", print.object[, 1L])

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

      print.object[, c("m", "sd", "m.diff", "se", "m.low", "m.upp", "t", "d", "d.low", "d.upp")] <- vapply(print.object[, c("m", "sd", "m.diff", "se", "m.low", "m.upp", "t", "d", "d.low", "d.upp")], formatC,
                                                                                                           digits = digits, format = "f",
                                                                                                           zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"), FUN.VALUE = character(2L))

      print.object[, "pval"] <- formatC(print.object[, "pval"], digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

      #.....................................
      # Format

      print.object <- rbind(c("Group", "n", "nNA", "M", "SD", "M.Diff", "SE", "Low", "Upp", "t", "df", "pval", "d", "Low", "Upp"), print.object)

      print.object[, -1L] <- apply(print.object[, -1L], 2L, format, justify = "right")

      print.object[-1L, 1L] <- paste0(" ", print.object[-1L, 1L])
      print.object[, 1L] <- format(print.object[, 1L], justify = "left")

      print.object[, 1L] <- paste(" ", print.object[, 1L])

      # NAs
      colnames.NA <- c("m.diff", "se", "m.low", "m.upp", "t", "df", "pval", "d", "d.low", "d.upp")

      print.object[, which(colnames(print.object) %in% colnames.NA)] <- apply(print.object[, which(colnames(print.object) %in% colnames.NA), drop = FALSE], 2L, function(y) gsub("NA", "  ", y))

      #.....................................
      # Print output

      cat(paste0(" Two-Sample t-Test\n\n"))

      # Print hypotheses
      if (isTRUE(hypo)) {

        # R Markdown in progress
        if (isTRUE(getOption("knitr.in.progress"))) {

          switch(x$args$alternative,
                 two.sided = {

                   cat("  Null hypothesis        H0: mu1 = mu2\n",
                       " Alternative hypothesis H1: mu1 != mu2\n\n")

                 }, less = {

                   cat("  Null hypothesis        H0: mu1 <= mu2\n",
                       " Alternative hypothesis H1: mu > mu2\n\n")

                 }, greater = {

                   cat("  Null hypothesis        H0: mu1 >= mu2\n",
                       " Alternative hypothesis H1: mu1 < mu2\n\n")

                 })

        } else {

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

      }

      # Print Cohen's d
      if (!isTRUE(effsize)) {

        print.object <- print.object[, -which(colnames(print.object) %in% c("d", "d.low", "d.upp"))]

      }

      # Print descriptive statistics
      if (!isTRUE(descript)) {

        print.object <- print.object[-2L, -which(colnames(print.object) %in% c("group", "n", "nNA", "m", "sd", "m.diff", "m.low", "m.upp", "se"))]

        print.object[, 1L] <- paste(" ", print.object[, 1L])

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

      print.object[, c("m1", "m2", "m.diff", "sd.diff", "se", "m.low", "m.upp", "t", "d", "d.low", "d.upp")] <- vapply(print.object[, c("m1", "m2", "m.diff", "sd.diff", "se", "m.low", "m.upp", "t", "d", "d.low", "d.upp")], formatC,
                                                                                                                       digits = digits, format = "f",
                                                                                                                       zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"), FUN.VALUE = character(1L))

      print.object[, "pval"] <- formatC(print.object[, "pval"], digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

      #.....................................
      # Format

      print.object <- rbind(c("n", "nNA", "M1", "M2", "M.Diff", "SD.Diff", "SE", "Low", "Upp", "t", "df", "pval", "d", "Low", "Upp"), print.object)

      print.object <- apply(print.object, 2L, format, justify = "right")

      print.object[, 1L] <- paste(" ", print.object[, 1L])

      #.....................................
      # Print output

      cat(paste0(" Paired-Sample t-Test\n\n"))

      # Print hypotheses
      if (isTRUE(hypo)) {

        # R Markdown in progress
        if (isTRUE(getOption("knitr.in.progress"))) {

          switch(x$args$alternative,
                 two.sided = {

                   cat("  Null hypothesis        H0: mu1 = mu2\n",
                       " Alternative hypothesis H1: mu1 != mu2\n\n")

                 }, less = {

                   cat("  Null hypothesis        H0: mu1 <= mu2\n",
                       " Alternative hypothesis H1: mu1 > mu2n\n")

                 }, greater = {

                   cat("  Null hypothesis        H0: mu >= mu2\n",
                       " Alternative hypothesis H1: mu1 < mu2\n\n")

                 })

        } else {

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

      }

      # Print Cohen's d
      if (!isTRUE(effsize)) {

        print.object <- print.object[, -which(colnames(print.object) %in% c("d", "d.low", "d.upp"))]

      }

      # Print descriptive statistics
      if (!isTRUE(descript)) {

        print.object <- print.object[, -which(colnames(print.object) %in% c("n", "nNA", "m1", "m2", "m.diff", "sd.diff", "se", "m.low", "m.upp"))]

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

  #_____________________________________________________________________________
  #
  # Welch Test -----------------------------------------------------------------
  }, test.welch = {

    #---------------------------------------------------------
    # Welch t-Test
    switch(x$sample, two = {

      #.....................................
      # Round

      print.object[, c("m", "sd", "m.diff", "se", "m.low", "m.upp", "t", "df", "d", "d.low", "d.upp")] <- vapply(print.object[, c("m", "sd", "m.diff", "se", "m.low", "m.upp", "t", "df", "d", "d.low", "d.upp")], formatC,
                                                                                                                 digits = digits, format = "f",
                                                                                                                 zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"), FUN.VALUE = character(2L))

      print.object[, "pval"] <- formatC(print.object[, "pval"], digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

      #.....................................
      # Format

      print.object <- rbind(c("Group", "n", "nNA", "M", "SD", "M.Diff", "SE", "Low", "Upp", "t", "df", "pval", "d", "Low", "Upp"), print.object)

      print.object[, -1L] <- apply(print.object[, -1L], 2L, format, justify = "right")

      print.object[-1L, 1L] <- paste0(" ", print.object[-1L, 1L])
      print.object[, 1L] <- format(print.object[, 1L], justify = "left")

      print.object[, 1L] <- paste(" ", print.object[, 1L])

      # NAs
      colnames.NA <- c("m.diff", "se", "m.low", "m.upp", "t", "df", "pval", "d", "d.low", "d.upp")

      print.object[, which(colnames(print.object) %in% colnames.NA)] <- apply(print.object[, which(colnames(print.object) %in% colnames.NA), drop = FALSE], 2L, function(y) gsub("NA", "  ", y))

      #.....................................
      # Print output

      cat(paste0(" Welch's Two-Sample t-Test\n\n"))

      # Print hypotheses
      if (isTRUE(hypo)) {

        # R Markdown in progress
        if (isTRUE(getOption("knitr.in.progress"))) {

          switch(x$args$alternative,
                 two.sided = {

                   cat("  Null hypothesis        H0: mu1 = mu2\n",
                       " Alternative hypothesis H1: mu1 != mu2\n\n")

                 }, less = {

                   cat("  Null hypothesis        H0: mu1 <= mu2\n",
                       " Alternative hypothesis H1: mu1 > mu2\n\n")

                 }, greater = {

                   cat("  Null hypothesis        H0: mu1 >= mu2\n",
                       " Alternative hypothesis H1: mu1 < mu2\n\n")

                 })

        } else {

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

      }

      # Print Cohen's d
      if (!isTRUE(effsize)) {

        print.object <- print.object[, -which(colnames(print.object) %in% c("d", "low", "upp"))]

      }

      # Print descriptive statistics
      if (!isTRUE(descript)) {

        print.object <- print.object[-2L, -which(colnames(print.object) %in% c("group", "n", "nNA", "m", "sd", "m.diff", "m.low", "m.upp", "se"))]

        print.object[, 1L] <- paste(" ", print.object[, 1L])

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
                                                                         zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"), FUN.VALUE = character(nrow(print.object[["descript"]])))

      print.object[["test"]][, c("F", "df2", "eta.sq", "omega.sq")] <- vapply(print.object[["test"]][, c(c("F", "df2", "eta.sq", "omega.sq"))], formatC,
                                                                              digits = digits, format = "f",
                                                                              zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"), FUN.VALUE = character(1L))

      print.object[["test"]][, "pval"] <- formatC(print.object[["test"]][, "pval"], digits = p.digits, format = "f",
                                                  zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

      print.object[["posthoc"]][, c("m.diff", "se", "m.low", "m.upp", "t", "df", "d", "d.low", "d.upp")] <- vapply(print.object[["posthoc"]][, c("m.diff", "se", "m.low", "m.upp", "t", "df", "d", "d.low", "d.upp")], formatC,
                                                                                                                   digits = digits, format = "f",
                                                                                                                   zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"), FUN.VALUE = character(nrow(print.object[["posthoc"]])))

      print.object[["posthoc"]][, "pval"] <- formatC(print.object[["posthoc"]][, "pval"], digits = p.digits, format = "f",
                                                     zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

      #.....................................
      # Format

      print.object[["descript"]] <- rbind(c("Group", "n", "nNA", "M", "SD", "Low", "Upp"), print.object[["descript"]])
      print.object[["test"]] <- rbind(c("F", "df1", "df2", "pval", "et", "om"), print.object[["test"]])
      print.object[["posthoc"]] <- rbind(c("Group1", "Group2", "M.diff", "SE", "Low", "Upp", "t", "df", "pval", "d", "Low", "Upp"), print.object[["posthoc"]])

      print.object[["descript"]][, -1L] <- apply(print.object[["descript"]][, -1L], 2L, format, justify = "right")
      print.object[["descript"]][-1L, 1L] <- paste0(" ", print.object[["descript"]][-1L, 1L])
      print.object[["descript"]][, 1L] <- apply(print.object[["descript"]][, 1L, drop = FALSE], 2L, format, justify = "left")

      print.object[["test"]] <- apply(print.object[["test"]], 2L, format, justify = "right")

      print.object[["posthoc"]][, -c(1L, 2L)] <- apply(print.object[["posthoc"]][, -c(1L, 2L)], 2L, format, justify = "right")
      print.object[["posthoc"]][-1L, 1L] <- paste0(" ", print.object[["posthoc"]][-1L, 1L])
      print.object[["posthoc"]][-1L, 2L] <- paste0(" ", print.object[["posthoc"]][-1L, 2L])
      print.object[["posthoc"]][, c(1L, 2L)] <- apply(print.object[["posthoc"]][, c(1L, 2L)], 2L, format, justify = "left")

      print.object[["test"]][1L, "eta.sq"] <- paste0(paste0(rep(" ", times = nchar(print.object[["test"]][1L, "eta.sq"]) - 2L), collapse = ""), "\u03B7\u00B2", collapes = "")
      print.object[["test"]][1L, "omega.sq"] <- paste0(paste0(rep(" ", times = nchar(print.object[["test"]][1L, "eta.sq"]) - 2L), collapse = ""), "\u03C9\u00B2", collapes = "")

      print.object[["descript"]][, 1L] <- paste(" ", print.object[["descript"]][, 1L])
      print.object[["test"]][, 1L] <- paste(" ", print.object[["test"]][, 1L])
      print.object[["posthoc"]][, 1L] <- paste("  ", print.object[["posthoc"]][, 1L])

      #.....................................
      # Print output

      cat(paste0(" Welch's ANOVA\n\n"))

      ###
      # Print hypotheses
      if (isTRUE(hypo)) {

        # R Markdown in progress
        if (isTRUE(getOption("knitr.in.progress"))) {

          cat("  Null hypothesis        H0: mu.i = mu.j for all i and j\n",
              " Alternative hypothesis H1: mu.i != mu.j for at least one i != j \n\n")

        } else {

          cat("  Null hypothesis        H0: \u03BC\u1D62 = \u03BC\u2C7C for all i and j\n",
              " Alternative hypothesis H1: \u03BC\u1D62 \u2260 \u03BC\u2C7C for at least one i \u2260 j \n\n")

        }

      }

      ###
      # Print descriptive statistics
      if (isTRUE(descript)) {

        write.table(print.object[["descript"]], quote = FALSE, row.names = FALSE, col.names = FALSE, na = "")

        cat("\n")

      }

      ###
      # Print effects size
      if (!isTRUE(effsize)) {

        print.object[["test"]] <- print.object[["test"]][, -which(colnames(print.object[["test"]]) %in% c("eta.sq", "omega.sq"))]

        print.object[["posthoc"]] <- print.object[["posthoc"]][, -which(colnames(print.object[["posthoc"]]) %in% c("d", "d.low", "d.upp"))]

      }

      cat(paste(print.object[["test"]][1L, ], collapse = " "), "\n")
      write.table(print.object[["test"]][-1L, , drop = FALSE], quote = FALSE, row.names = FALSE, col.names = FALSE, na = "")

      ###
      # Print post-hoc test
      if (!isTRUE(posthoc)) {

        cat(paste0("\n  Games-Howell Post Hoc Test for Multiple Comparison\n\n"))

        write.table(print.object[["posthoc"]], quote = FALSE, row.names = FALSE, col.names = FALSE, na = "")

      }

    })

  #_____________________________________________________________________________
  #
  # z-Test ---------------------------------------------------------------------
  }, test.z = {

    #---------------------------------------------------------
    # One sample z-test
    switch(x$sample, one = {

      #.....................................
      # Round

      print.object[, c("m", "sd", "m.diff", "se", "m.low", "m.upp", "z", "d")] <- vapply(print.object[, c("m", "sd", "m.diff", "se", "m.low", "m.upp", "z", "d")], formatC,
                                                                       digits = digits, format = "f",
                                                                       zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"), FUN.VALUE = character(1L))

      print.object[, "pval"] <- formatC(print.object[, "pval"], digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

      #.....................................
      # Format

      print.object <- rbind(c("n", "nNA", "M", "SD", "M.Diff", "SE", "Low", "Upp", "z", "pval", "d"), print.object)

      print.object <- apply(print.object, 2L, format, justify = "right")

      print.object[, 1L] <- paste(" ", print.object[, 1L])

      #.....................................
      # Print output

      # R Markdown in progress
      if (isTRUE(getOption("knitr.in.progress"))) {

        cat(paste0(" One-Sample z-Test with ", "sigma = ", round(x$args$sigma, digits = digits), "\n\n"))

      } else {

        cat(paste0(" One-Sample z-Test with ", "\u03c3 = ", round(x$args$sigma, digits = digits), "\n\n"))

      }

      # Print hypotheses
      if (isTRUE(hypo)) {

        # R Markdown in progress
        if (isTRUE(getOption("knitr.in.progress"))) {

          switch(x$args$alternative,
                 two.sided = {

                   cat("  Null hypothesis        H0: mu =", x$args$mu, "\n",
                       " Alternative hypothesis H1: mu !=", x$args$mu, "\n\n")

                 }, less = {

                   cat("  Null hypothesis        H0: mu >=", x$args$mu, "\n",
                       " Alternative hypothesis H1: mu <", x$args$mu, "\n\n")


                 }, greater = {

                   cat("  Null hypothesis        H0: mu <=", x$args$mu, "\n",
                       " Alternative hypothesis H1: mu >", x$args$mu, "\n\n")

                 })

        } else {

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

      }

      # Print Cohen's d
      if (!isTRUE(effsize)) {

        print.object <- print.object[, -which(colnames(print.object) %in% c("d"))]

      }

      # Print descriptive statistics
      if (!isTRUE(descript)) {

        print.object <- print.object[, -which(colnames(print.object) %in% c("n", "nNA", "m", "sd", "m.diff", "se", "m.low", "m.upp"))]

        print.object[, 1L] <- paste(" ", print.object[, 1L])

      }

      write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE, na = "")

    #---------------------------------------------------------
    # Two sample z-test
    }, two = {

      #.....................................
      # Round

      print.object[, c("m", "sd", "m.diff", "se", "m.low", "m.upp", "z", "d")] <- vapply(print.object[, c("m", "sd", "m.diff", "se", "m.low", "m.upp", "z", "d")], formatC,
                                                                       digits = digits, format = "f",
                                                                       zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"), FUN.VALUE = character(2L))

      print.object[, "pval"] <- formatC(print.object[, "pval"], digits = p.digits, format = "f",
                                        zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

      #.....................................
      # Format

      print.object <- rbind(c("Group", "n", "nNA", "M", "SD", "M.Diff", "SE", "Low", "Upp", "z", "pval", "d"), print.object)

      print.object[, -1L] <- apply(print.object[, -1L], 2L, format, justify = "right")

      print.object[-1L, 1L] <- paste0(" ", print.object[-1L, 1L])
      print.object[, 1L] <- format(print.object[, 1L], justify = "left")

      print.object[, 1L] <- paste(" ", print.object[, 1L])

      # NAs
      print.object[, which(colnames(print.object) %in% c("m.diff", "se", "m.low", "m.upp", "z", "pval", "d"))] <- apply(print.object[, which(colnames(print.object) %in% c("m.diff", "se", "m.low", "m.upp", "z", "pval", "d")), drop = FALSE], 2L, function(y) gsub("NA", "  ", y))

      #.....................................
      # Print output

      # R Markdown in progress
      if (isTRUE(getOption("knitr.in.progress"))) {

        if (isTRUE(length(unique(x$args$sigma)) == 1L)) {

          cat(paste0(" Two-Sample z-Test with ", "sigma1 = sigma2 = ", round(x$args$sigma[1L], digits = digits), "\n\n"))

        } else {

          cat(paste0(" Two-Sample z-Test with ", "sigma1 = ", x$args$sigma[1L], " and ", "sigma2 = ", round(x$args$sigma[2L], digits = digits), "\n\n"))

        }

      } else {

        if (isTRUE(length(unique(x$args$sigma)) == 1L)) {

          cat(paste0(" Two-Sample z-Test with ", "\u03c3\u2081 = \u03c3\u2082 = ", round(x$args$sigma[1L], digits = digits), "\n\n"))

        } else {

          cat(paste0(" Two-Sample z-Test with ", "\u03c3\u2081 = ", x$args$sigma[1L], " and ", "\u03c3\u2082 = ", round(x$args$sigma[2L], digits = digits), "\n\n"))

        }

      }

      # Print hypotheses
      if (isTRUE(hypo)) {

        # R Markdown in progress
        if (isTRUE(getOption("knitr.in.progress"))) {

          switch(x$args$alternative,
                 two.sided = {

                   cat("  Null hypothesis        H0: mu1 = mu2\n",
                       " Alternative hypothesis H1: mu1 != mu2\n\n")

                 }, less = {

                   cat("  Null hypothesis        H0: mu1 <= mu2\n",
                       " Alternative hypothesis H1: mu1 > mu2\n\n")

                 }, greater = {

                   cat("  Null hypothesis        H0: mu1 >= mu2\n",
                       " Alternative hypothesis H1: mu1 < mu2\n\n")

                 })

        } else {

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

      }

      # Print Cohen's d
      if (!isTRUE(effsize)) {

        print.object <- print.object[, -which(colnames(print.object) %in% c("d"))]

      }

      # Print descriptive statistics
      if (!isTRUE(descript)) {

        print.object <- print.object[-2L, -which(colnames(print.object) %in% c("n", "nNA", "m", "sd", "m.diff", "se"))]

        print.object[, 1L] <- paste(" ", print.object[, 1L])

      }

      write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE, na = "")

    #---------------------------------------------------------
    # Paired z-test
    }, paired = {

      #.....................................
      # Round

      print.object[, c("m1", "m2", "m.diff", "sd.diff", "se", "m.low", "m.upp", "z", "d")] <- vapply(print.object[, c("m1", "m2", "m.diff", "sd.diff", "se", "m.low", "m.upp", "z", "d")], formatC,
                                                                                                     digits = digits, format = "f",
                                                                                                     zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"), FUN.VALUE = character(1L))

      print.object[, "pval"] <- formatC(print.object[, "pval"], digits = p.digits, format = "f",
                                        zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

      #.....................................
      # Format

      print.object <- rbind(c("n", "nNA", "M1", "M2", "M.Diff", "SD.Diff", "SE", "Low", "Upp", "z", "pval", "d"), print.object)

      print.object <- apply(print.object, 2L, format, justify = "right")

      print.object[, 1L] <- paste(" ", print.object[, 1L])

      #.....................................
      # Print output

      # R Markdown in progress
      if (isTRUE(getOption("knitr.in.progress"))) {

        cat(paste0(" Paired-Sample z-Test with ", "sigma(diff) = ", round(x$args$sigma, digits = digits), "\n\n"))

      } else {

        cat(paste0(" Paired-Sample z-Test with ", "\u03c3(diff) = ", round(x$args$sigma, digits = digits), "\n\n"))

      }

      # Print hypotheses
      if (isTRUE(hypo)) {

        # R Markdown in progress
        if (isTRUE(getOption("knitr.in.progress"))) {

          switch(x$args$alternative,
                 two.sided = {

                   cat("  Null hypothesis        H0: mu1 = mu2\n",
                       " Alternative hypothesis H1: mu1 != mu2\n\n")

                 }, less = {

                   cat("  Null hypothesis        H0: mu1 <= mu2\n",
                       " Alternative hypothesis H1: mu1 > mu2\n\n")

                 }, greater = {

                   cat("  Null hypothesis        H0: mu1 >= mu2\n",
                       " Alternative hypothesis H1: mu1 < mu2\n\n")

                 })

        } else {

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

      }

      # Print Cohen's d
      if (!isTRUE(effsize)) {

        print.object <- print.object[, -which(colnames(print.object) %in% c("d"))]

      }

      # Print descriptive statistics
      if (!isTRUE(descript)) {

        print.object <- print.object[, -which(colnames(print.object) %in% c("n", "nNA", "m1", "m2", "m.diff", "sd.diff", "se", "m.low", "m.upp"))]

      }

      write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE, na = "")

    })

  })

}
