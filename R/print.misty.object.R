#' Print misty.object object
#'
#' This function prints the \code{misty.object} object
#'
#' @param x           \code{misty.object} object.
#' @param print       a character string or character vector indicating which
#'                    results to to be printed on the console.
#' @param tri         a character string or character vector indicating which
#'                    triangular of the matrix to show on the console, i.e.,
#'                    \code{both} for upper and lower triangular, \code{lower}
#'                    for the lower triangular, and \code{upper} for the upper
#'                    triangular.
#' @param freq        logical: if \code{TRUE}, absolute frequencies will be included
#'                    in the cross tabulation (\code{crosstab()} function).
#' @param hypo        logical: if \code{TRUE}, null and alternative hypothesis are
#'                    shown on the console (\code{\link{test.t}},
#'                    \code{\link{test.welch}}, \code{\link{test.z}} function).
#' @param descript    logical: if \code{TRUE}, descriptive statistics are shown on
#'                    the console (\code{\link{test.t}}, \code{\link{test.welch}},
#'                    \code{\link{test.z}} function).
#' @param epsilon     logical: if \code{TRUE}, box indices of sphericity (epsilon)
#'                    are shown on the console (\code{\link{aov.w}}).
#' @param effsize     logical: if \code{TRUE}, effect size measure(s) is shown on
#'                    the console (\code{\link{test.t}}, \code{\link{test.welch}},
#'                    \code{\link{test.z}} function).
#'                    \code{\link{test.z}} function).
#' @param posthoc     logical: if \code{TRUE},post hoc test for multiple comparison
#'                    is shown on the console (\code{\link{test.welch}}).
#' @param split       logical: if \code{TRUE}, output table is split by variables
#'                    when specifying more than one variable in \code{x}
#'                    (\code{\link{freq}}).
#' @param table       logical: if \code{TRUE}, a frequency table with number of
#'                    observed values (\code{"nObs"}), percent of observed values
#'                    (\code{"pObs"}), number of missing values (\code{"nNA"}),
#'                    and percent of missing values (\code{"pNA"}) is printed for
#'                    each variable on the console (\code{na.descript()} function).
#' @param digits      an integer value indicating the number of decimal places digits
#'                    to be used for displaying results.
#' @param p.digits    an integer indicating the number of decimal places to be used
#'                    for displaying \emph{p}-values.
#' @param icc.digits  an integer indicating the number of decimal places to be used
#'                    for displaying intraclass correlation coefficients
#'                    (\code{multilevel.descript()} and \code{multilevel.icc()}
#'                    function).
#' @param r.digits    an integer value indicating the number of decimal places
#'                    to be used for displaying R-hat values.
#' @param ess.digits  an integer value indicating the number of decimal places
#'                    to be used for displaying effective sample sizes.
#' @param mcse.digits an integer value indicating the number of decimal places
#'                    to be used for displaying monte carlo standard errors.
#'
#' @param sort.var    logical: if \code{TRUE}, output is sorted by variables.
#' @param order       logical: if \code{TRUE}, variables are ordered from left to
#'                    right in increasing order
#'                    of missing values (\code{na.descript()} function).
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#' @param ...         further arguments passed to or from other methods.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
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
                               icc.digits = x$args$icc.digits, r.digits = x$args$r.digits,
                               ess.digits = x$args$ess.digits, mcse.digits = x$args$mcse.digits,
                               sort.var = x$args$sort.var, order = x$args$order, check = TRUE, ...) {

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

  #____________________--_______________________________________________________
  #
  # Blimp Object ---------------------------------------------------------------
  }, blimp = {

    cat("Please use the blimp.print function to print a \"blimp\" object.")

  #___________________________________________________________________________
  #
  # Blimp Summary Measures, Convergence and Efficiency Diagnostics -----------
  }, blimp.bayes = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Round ####

    # digits
    print.round <- c("m", "med", "map", "sd", "mad", "skew", "kurt", "eti.low", "eti.upp", "hdi.low", "hdi.upp")
    print.object[, print.round] <- sapply(print.round, function(y) ifelse(!is.na(print.object[, y]), formatC(print.object[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

    # r.digits
    print.object[, "rhat"] <- ifelse(!is.na(print.object[, "rhat"]), formatC(print.object[, "rhat"], digits = r.digits, format = "f", zero.print = ifelse(r.digits > 0L, paste0("0.", paste(rep(0L, times = r.digits), collapse = "")), "0")), NA)

    # ess.digits
    print.object[, "b.ess"] <- ifelse(!is.na(print.object[, "b.ess"]), formatC(print.object[, "b.ess"], digits = ess.digits, format = "f", zero.print = ifelse(ess.digits > 0L, paste0("0.", paste(rep(0L, times = ess.digits), collapse = "")), "0")), NA)
    print.object[, "t.ess"] <- ifelse(!is.na(print.object[, "t.ess"]), formatC(print.object[, "t.ess"], digits = ess.digits, format = "f", zero.print = ifelse(ess.digits > 0L, paste0("0.", paste(rep(0L, times = ess.digits), collapse = "")), "0")), NA)

    # mcse.digits
    print.object[, "b.mcse"] <- ifelse(!is.na(print.object[, "b.mcse"]), formatC(print.object[, "b.mcse"], digits = mcse.digits, format = "f", zero.print = ifelse(mcse.digits > 0L, paste0("0.", paste(rep(0L, times = mcse.digits), collapse = "")), "0")), NA)
    print.object[, "t.mcse"] <- ifelse(!is.na(print.object[, "t.mcse"]), formatC(print.object[, "t.mcse"], digits = mcse.digits, format = "f", zero.print = ifelse(mcse.digits > 0L, paste0("0.", paste(rep(0L, times = mcse.digits), collapse = "")), "0")), NA)

    # p.digits
    print.object[, "pd"] <- ifelse(!is.na(print.object[, "pd"]), formatC(print.object[, "pd"], digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0")), NA)
    print.object[, "rope"] <- ifelse(!is.na(print.object[, "rope"]), formatC(print.object[, "rope"], digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0")), NA)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##  Row Names ####

    print.object <- rbind(c("Param", "L1", "L2", "L3", "M", "Med", "MAP", "SD", "MAD", "Skew", "Kurt", "ETI.Low", "ETI.Upp", "HDI.Low", "HDI.Upp", "R-hat", "B.ESS", "T.ESS", "B.MCSE", "T.MCSE", "pd", "ROPE"), print.object)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Select Statistical Measures and Add Parameters ####

    # Print statistics
    if (isTRUE("eti" %in% print)) { print <- c(print, c("eti.low", "eti.upp")) }
    if (isTRUE("hdi" %in% print)) { print <- c(print, c("hdi.low", "hdi.upp")) }

    # pd and ROPE
    if (isTRUE(x$args$pd)) { print <- c(print, "pd")}
    if (isTRUE(!is.null(x$args$rope))) { print <- c(print, "rope")}

    # Sort
    print <- intersect(c("m", "med", "map", "sd", "mad", "skew", "kurt", "eti.low", "eti.upp", "hdi.low", "hdi.upp", "rhat", "b.ess", "t.ess", "b.mcse", "t.mcse", "pd", "rope"), print)

    # Select
    print.object <- data.frame(print.object[, c("param", "latent1", "latent2", "latent3")], print.object[, print, drop = FALSE], stringsAsFactors = FALSE, check.names = FALSE)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Format ####

    # Justify left and right
    print.object[, 1L] <- format(print.object[, 1L, drop = FALSE], justify = "left")
    print.object[, -1L] <- apply(print.object[, -1L, drop = FALSE], 2L, function(y) format(y, justify = "right"))

    # Add blank space
    print.object[, "param"] <- c(paste0("  ", print.object[1L, "param"], collapse = ""), paste0("   ", print.object[-1L, "param"]))
    print.object[, "param"] <- format(c(print.object[1L, "param"], misty::chr.trim(print.object[-1L, "param"], side = "right")), justify = "left")

    # Replace NA
    print.object[, -c(1L:4L)] <- apply(print.object[, -c(1L:4L)], 2L, function(y) gsub("NA", "  ", y))

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Print Result Table ####

    # Header
    cat(" Summary Measures, Convergence and Efficiency Diagnostics\n\n")

    # Print table
    write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE)

    # R-hat
    if (isTRUE("rhat" %in% print)) {

      if (isTRUE(x$args$fold)) {

        cat("\n  Note. Maximum of Rank-Normalized (Folded-)Split R-hat")

      } else {

        if (isTRUE(x$args$rank)) {

          if (isTRUE(x$args$split)) {

            cat("\n  Note. Rank-Normalizsed Split R-hat")

          } else {

            cat("\n  Note. Rank-Normalized R-hat")

          }

        } else {

          if (isTRUE(x$args$split)) {

            cat("\n  Note. Traditional Split R-hat")

          } else {

            cat("\n  Note. Traditional R-hat")

          }

        }

      }

    }

    # ROPE
    if (isTRUE(!is.null(x$args$rope))) {

      if (isTRUE("rhat" %in% print)) {

        switch(x$args$alternative,
               two.sided = { cat(paste0("\n        Region of Practical Equivalence (ROPE): [", x$args$rope[1L], ", ", x$args$rope[2L], "]")) },
               less = { cat(paste0("\n        Region of Practical Equivalence (ROPE): [", x$args$rope[1L], ", Inf]")) },
               greater = { cat(paste0("\n        Region of Practical Equivalence (ROPE): [-Inf, ", x$args$rope[2L], "]"))} )

      } else {

        switch(x$args$alternative,
               two.sided = cat(paste0("\n    Note. Region of Practical Equivalence (ROPE): [", x$args$rope[1L], ", ", x$args$rope[2L], "]")),
               less = cat(paste0("\n    Note. Region of Practical Equivalence (ROPE): [", x$args$rope[1L], ", Inf]")),
               greater = cat(paste0("\n    Note. Region of Practical Equivalence (ROPE): [-Inf, ", x$args$rope[2L], "]")))

      }

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

          if (isTRUE(x$ci %in% c("mean.diff.i", "prop.diff.i"))) {

            print.object[[i]][, "between"] <- c(paste0("   ", print.object[[i]][1L, "between"]), paste0("    ", print.object[[i]][-1L, "between"]))
            print.object[[i]][, "between"] <- format(c(print.object[[i]][1L, "between"], misty::chr.trim(print.object[[i]][-1L, "between"], side = "right")), justify = "left")

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
  # Standardized Coefficients --------------------------------------------------
  }, std.coef = {

    if (isTRUE(check)) {

      #......
      # Check input 'print'
      if (isTRUE(!all(print %in% c("all", "stdx", "stdy", "stdyx")))) {

        stop("Character strings in the argument 'print' do not all match with \"all\", \"stdx\", \"stdy\", or \"stdyx\".", call. = FALSE)

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
  # Cohen's d ..----------------------------------------------------------------
  }, cohens.d = {

    #......
    # Variables to round
    print.round <- switch(x$sample,
                          one = c("m", "m.diff", "sd", "d", "se", "low", "upp"),
                          two = c("m", "m.diff", "sd", "d", "se", "low", "upp"),
                          paired = c("m1", "m2", "m.diff", "sd", "d", "se", "low", "upp"))

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
  # Correlation Matrix with Statistical Significance Testing -------------------
  }, cor.matrix = {

    if (isTRUE(check)) {

      #......
      # Check input 'print'
      if (isTRUE(any(!print %in% c("all", "cor", "stat", "df", "n", "p")))) { stop("Character string(s) in the argument 'print' does not match with \"all\", \"cor\",  \"stat\",  \"df\", \"n\", or \"p\".", call. = FALSE) }

    }

    # R Markdown in progress
    if (isTRUE(getOption("knitr.in.progress"))) { x$args$sig <- FALSE }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Two variables ####

    if (isTRUE(ncol(x$data[, grep(".group", colnames(x$data), invert = TRUE)]) == 2L)) {

      #...................
      ### Grouping variable ####

      if (isTRUE(".group" %in% colnames(x$data))) {

        switch (x$args$method,
                'pearson' = {

                  print.object <- data.frame(Group = c(sort(unique(x$data$.group))[1L], sort(unique(x$data$.group))[2L]),
                                             x = colnames(print.object$cor)[1L],
                                             y = colnames(print.object$cor)[2L],
                                             n = c(print.object$n[2L, 1L], print.object$n[1L, 2L]),
                                             r = c(print.object$cor[2L, 1L], print.object$cor[1L, 2L]),
                                             t = c(print.object$stat[2L, 1L], print.object$stat[1L, 2L]),
                                             df = c(print.object$df[2L, 1L], print.object$df[1L, 2L]),
                                             pval = c(print.object$p[2L, 1L], print.object$p[1L, 2L]))

                }, 'spearman' = {

                  print.object <- data.frame(Group = c(sort(unique(x$data$.group))[1L], sort(unique(x$data$.group))[2L]),
                                             x = colnames(print.object$cor)[1L],
                                             y = colnames(print.object$cor)[2L],
                                             n = c(print.object$n[2L, 1L], print.object$n[1L, 2L]),
                                             r = c(print.object$cor[2L, 1L], print.object$cor[1L, 2L]),
                                             S = c(print.object$stat[2L, 1L], print.object$stat[1L, 2L]),
                                             pval = c(print.object$p[2L, 1L], print.object$p[1L, 2L]))

                }, 'kendall-b' = {

                  print.object <- data.frame(Group = c(sort(unique(x$data$.group))[1L], sort(unique(x$data$.group))[2L]),
                                             x = colnames(print.object$cor)[1L],
                                             y = colnames(print.object$cor)[2L],
                                             n = c(print.object$n[2L, 1L], print.object$n[1L, 2L]),
                                             tau = c(print.object$cor[2L, 1L], print.object$cor[1L, 2L]),
                                             z = c(print.object$stat[2L, 1L], print.object$stat[1L, 2L]),
                                             pval = c(print.object$p[2L, 1L], print.object$p[1L, 2L]))


                }, 'kendall-c' = {

                  print.object <- data.frame(Group = c(sort(unique(x$data$.group))[1L], sort(unique(x$data$.group))[2L]),
                                             x = colnames(print.object$cor)[1L],
                                             y = colnames(print.object$cor)[2L],
                                             n = c(print.object$n[2L, 1L], print.object$n[1L, 2L]),
                                             tau = c(print.object$cor[2L, 1L], print.object$cor[1L, 2L]),
                                             z = c(print.object$stat[2L, 1L], print.object$stat[1L, 2L]),
                                             pval = c(print.object$p[2L, 1L], print.object$p[1L, 2L]))

                }, 'tetra' = {

                  print.object <- data.frame(Group = c(sort(unique(x$data$.group))[1L], sort(unique(x$data$.group))[2L]),
                                             x = colnames(print.object$cor)[1L],
                                             y = colnames(print.object$cor)[2L],
                                             n = c(print.object$n[2L, 1L], print.object$n[1L, 2L]),
                                             r = c(print.object$cor[2L, 1L], print.object$cor[1L, 2L]))

                }, 'poly' = {

                  print.object <- data.frame(Group = c(sort(unique(x$data$.group))[1L], sort(unique(x$data$.group))[2L]),
                                             x = colnames(print.object$cor)[1L],
                                             y = colnames(print.object$cor)[2L],
                                             n = c(print.object$n[2L, 1L], print.object$n[1L, 2L]),
                                             r = c(print.object$cor[2L, 1L], print.object$cor[1L, 2L]))
                })

        # Round
        print.object[, !colnames(print.object) %in% c("Group", "n", "df", "pval")] <- vapply(print.object[, !colnames(print.object) %in% c("Group", "n", "df", "pval")], formatC, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"), FUN.VALUE = character(2L))

        # Add blank
        print.object[, 1L] <- sapply(format(c("Group", as.character(print.object[, 1L])), justify = "right"), function(y) paste("", y, collapse = ""))[-1L]

      #...................
      ### No grouping variable ####

      } else {

        switch(x$args$method, 'pearson' = {

                  print.object <- data.frame(x = colnames(print.object$cor)[1L],
                                             y = colnames(print.object$cor)[2L],
                                             n = print.object$n[2L, 1L],
                                             r = print.object$cor[2L, 1L],
                                             t = print.object$stat[2L, 1L],
                                             df = print.object$df[2L, 1L],
                                             pval = print.object$p[2L, 1L])

                }, 'spearman' = {

                  print.object <- data.frame(x = colnames(print.object$cor)[1L],
                                             y = colnames(print.object$cor)[2L],
                                             n = print.object$n[2L, 1L],
                                             r = print.object$cor[2L, 1L],
                                             S = print.object$stat[2L, 1L],
                                             pval = print.object$p[2L, 1L])

                }, 'kendall-b' = {

                  print.object <- data.frame(x = colnames(print.object$cor)[1L],
                                             y = colnames(print.object$cor)[2L],
                                             n = print.object$n[2L, 1L],
                                             tau = print.object$cor[2L, 1L],
                                             z = print.object$stat[2L, 1L],
                                             pval = print.object$p[2L, 1L])


                }, 'kendall-c' = {

                  print.object <- data.frame(x = colnames(print.object$cor)[1L],
                                             y = colnames(print.object$cor)[2L],
                                             n = print.object$n[2L, 1L],
                                             tau = print.object$cor[2L, 1L],
                                             z = print.object$stat[2L, 1L],
                                             pval = print.object$p[2L, 1L])

                }, 'tetra' = {

                  print.object <- data.frame(x = colnames(print.object$cor)[1L],
                                             y = colnames(print.object$cor)[2L],
                                             n = print.object$n[2L, 1L],
                                             r = print.object$cor[2L, 1L])

                }, 'poly' = {

                  print.object <- data.frame(x = colnames(print.object$cor)[1L],
                                             y = colnames(print.object$cor)[2L],
                                             n = print.object$n[2L, 1L],
                                             r = print.object$cor[2L, 1L])
                })

        # Round
        print.object[, !colnames(print.object) %in% c("n", "df", "pval")] <- vapply(print.object[, !colnames(print.object) %in% c("n", "df", "pval")], formatC, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"), FUN.VALUE = character(1L))

        # Add blank
        print.object[, 1L] <- paste("", format(as.character(print.object[, 1L]), justify = "right"), collapse = "")

      }

      # Round p-values
      if (isTRUE(!x$args$method %in% c("tetra", "poly"))) { print.object[, "pval"] <- formatC(print.object[, "pval"], digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0")) }

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

      }, 'tetra' = {

        cat(" Tetrachoric Correlation Coefficient\n\n")

      }, 'poly' = {

        cat(" Polychoric Correlation Coefficient\n\n")

      })

      # Print object
      print(print.object, quote = FALSE, right = TRUE, row.names = FALSE)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## More than two variables ####

    } else {

      #........................................
      # Round and format

      print.object$cor <- formatC(print.object$cor, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

      print.object$n <- formatC(print.object$n, zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

      print.object$stat <- formatC(print.object$stat, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

      if (isTRUE(x$args$method == "pearson")) { print.object$df <- formatC(print.object$df, digits = 0L, format = "f") }

      print.object$p <- formatC(print.object$p, digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

      diag(print.object$cor) <- ""
      diag(print.object$n) <- ""

      if (isTRUE(!x$args$method %in% c("tetra", "poly"))) {

        diag(print.object$stat) <- ""
        diag(print.object$df) <- ""
        diag(print.object$p) <- ""

      }

      #........................................
      # Lower and/or upper triangular

      if (isTRUE(tri == "lower")) {

        print.object$cor[upper.tri(print.object$cor)] <- ""
        print.object$n[upper.tri(print.object$n)] <- ""

        if (isTRUE(!x$args$method %in% c("tetra", "poly"))) {

          print.object$stat[upper.tri(print.object$stat)] <- ""
          print.object$df[upper.tri(print.object$df)] <- ""
          print.object$p[upper.tri(print.object$p)] <- ""

        }

      }

      if (isTRUE(tri == "upper")) {

        print.object$cor[lower.tri(print.object$cor)] <- ""
        print.object$n[lower.tri(print.object$n)] <- ""

        if (isTRUE(!x$args$method %in% c("tetra", "poly"))) {

          print.object$stat[lower.tri(print.object$stat)] <- ""
          print.object$df[lower.tri(print.object$df)] <- ""
          print.object$p[lower.tri(print.object$p)] <- ""

        }

      }

      #........................................
      # Row names

      if (isTRUE(!is.null(row.names(print.object$cor)))) {

        row.names(print.object$cor) <- paste0("  ", row.names(print.object$cor))
        row.names(print.object$n) <- paste0("  ", row.names(print.object$n))

        if (isTRUE(!x$args$method %in% c("tetra", "poly"))) {

          row.names(print.object$stat) <- paste0("  ", row.names(print.object$stat))
          row.names(print.object$df) <- paste0("  ", row.names(print.object$df))
          row.names(print.object$p) <- paste0("  ", row.names(print.object$p))

        }

      }

      print.object$cor <- apply(print.object$cor, 2L, function(y) format(y, justify = "right"))
      print.object$n <- apply(print.object$n, 2L, function(y) format(y, justify = "right"))

      if (isTRUE(!x$args$method %in% c("tetra", "poly"))) {

        print.object$stat <- apply(print.object$stat, 2L, function(y) format(y, justify = "right"))
        print.object$df <- apply(print.object$df, 2L, function(y) format(y, justify = "right"))
        print.object$p <- apply(print.object$p, 2L, function(y) format(y, justify = "right"))

      }

      #........................................
      # Statistically significant correlation coefficients in boldface

      if (isTRUE(x$args$sig)) {

        if (isTRUE(!x$args$method %in% c("tetra", "poly"))) {

          numbers <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
          unicode <- c("\U1D7CE", "\U1D7CF", "\U1D7D0", "\U1D7EF", "\U1D7F0", "\U1D7F1", "\U1D7F2", "\U1D7F3", "\U1D7F4", "\U1D7F5")

          print.object$cor[lower.tri(print.object$cor)][which(x$result$p[lower.tri(x$result$p)] <= x$args$alpha)] <- na.omit(misty::chr.gsub(numbers, unicode, print.object$cor[lower.tri(print.object$cor)][which(x$result$p[lower.tri(x$result$p)] <= x$args$alpha)]))
          print.object$cor[upper.tri(print.object$cor)][which(x$result$p[upper.tri(x$result$p)] <= x$args$alpha)] <- misty::chr.gsub(numbers, unicode, print.object$cor[upper.tri(print.object$cor)][which(x$result$p[upper.tri(x$result$p)] <= x$args$alpha)])

        } else {

          warning("This function does not provide statistical significance testing for tetrachoric or polychoric correlation coefficients.", call. = FALSE)

        }

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

        }, 'tetra' = {

          cat(" Tetrachoric Correlation Coefficient\n\n")

        }, 'poly' = {

          cat(" Polychoric Correlation Coefficient\n\n")

        })

        print(print.object$cor, quote = FALSE, right = TRUE, max = 99999L)

      }

      #........................
      # Sample size

      if (isTRUE("n" %in% print & attr(x$data, "missing") && isTRUE(!x$args$na.omit))) {

        if (isTRUE("cor" %in% print)) { cat("\n") }

        cat(" Sample Size\n\n")
        print(print.object$n, quote = FALSE, right = TRUE, max = 99999L)

      }

      #........................
      # Test statistic

      if (isTRUE("stat" %in% print && !x$args$method %in% c("tetra", "poly"))) {

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

      if (isTRUE("df" %in% print && x$args$method == "pearson")) {

        if (isTRUE(any(c("cor", "n", "stat") %in% print))) { cat("\n") }

        cat(" Degrees of Freedom (df) \n\n")

        print(print.object$df, quote = FALSE, right = TRUE, max = 99999L)

      }

      #........................
      # p.values

      if (isTRUE("p" %in% print && !x$args$method %in% c("tetra", "poly"))) {

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
      if (isTRUE(x$args$sig && !x$args$method %in% c("tetra", "poly"))) {

        cat(paste0("       Statistically significant coefficients at \U03B1 = ", signif(x$args$alpha, digits = 2L), " are boldface\n"))

      }

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

    if (isTRUE(length(print) == 1L && print == "all")) {

      print <- c("n", "nNA", "pNA", "m", "se.m", "var", "sd", "min", "p25", "med", "p75", "max", "range", "iqr", "skew", "kurt")

    }

    #......
    # Variables to round
    print.round <- c("pNA", "m", "se.m", "var", "sd", "min", "p25", "med", "p75", "max", "range", "iqr", "skew", "kurt")

    #----------------------------------------
    # No Grouping, No Split

    if (isTRUE(is.null(x$data$group) && is.null(x$data$split))) {

      #......
      # Round
      print.object[, print.round] <- sapply(print.round, function(y) ifelse(!is.na(print.object[, y]), formatC(print.object[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

      #......
      # Percentages
      print.object[, "pNA"] <- paste0(print.object[, "pNA"], "%")

      #......
      # Row names
      print.object <- rbind(c("Variable", "n", "nNA", "pNA", "M", "SE.M", "Var", "SD", "Min", "p25", "Med", "p75", "Max", "Range", "IQR", "Skew", "Kurt"), print.object)

      #......
      # Select statistical measures and add variable names
      print.object <- data.frame(variable = print.object[, "variable"], print.object[, print, drop = FALSE], stringsAsFactors = FALSE, check.names = FALSE)

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
  # Manual Dominance Analysis ---------------------------------------------------------
  }, dominance.manual = {

    cat(" Dominance Analysis: General Dominance\n\n")

    #...................
    ### General dominance ####

    # Extract result table
    print.gen <- print.object

    #...................
    ### Format ####

    # NA
    print.gen[, "rank"] <- ifelse(is.na(print.gen[, "rank"]), "", print.gen[, "rank"])

    # Round
    print.gen[, "r2"] <- formatC(print.gen[, "r2"], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))
    print.gen[, "perc"] <- formatC(print.gen[, "perc"], digits = digits - 1L, format = "f", zero.print = ifelse(digits - 1L > 0L, paste0("0.", paste(rep(0L, times = digits - 1L), collapse = "")), "0"))

    # Percentages
    print.gen[, "perc"] <- paste0(print.gen[, "perc"], "%")

    # Variable names
    print.gen <- data.frame(Variable = rownames(print.gen), print.gen)

    # Column names
    print.gen <- rbind(c("Variable", "R2", "Perc", "Rank"), print.gen)

    # Add blank space
    print.gen[, 1L] <- c(paste0("   ", print.gen[1L, 1L], collapse = ""), paste0("    ", print.gen[-c(1L, nrow(print.gen)), 1L]),  paste0("   ", print.gen[nrow(print.gen), 1L], collapse = ""))

    # Justify
    print.gen[, 1L] <- format(print.gen[, 1L, drop = FALSE], justify = "left")
    print.gen[, -1L] <- format(print.gen[, -1L, drop = FALSE], justify = "right")

    # R Markdown not in progress
    if (is.null(getOption("knitr.in.progress"))) { print.gen[1L, ] <- sub("2", "\u00B2", print.gen[1L, ]) }

    #...................
    ### Print ####

    write.table(print.gen, quote = FALSE, row.names = FALSE, col.names = FALSE)

    cat(paste0("\n  Note. Outcome variable: ", x$args$out, "\n"))

  #_____________________________________________________________________________
  #
  # Dominance Analysis ---------------------------------------------------------
  }, dominance = {

    cat(" Dominance Analysis\n")

    #...................
    ### General dominance ####

    if (isTRUE("gen" %in% x$args$print)) {

      cat("\n  General Dominance\n\n")

      # Extract result table
      print.gen <- print.object$gen

      #...................
      ### Format ####

      # NA
      print.gen[, "rank"] <- ifelse(is.na(print.gen[, "rank"]), "", print.gen[, "rank"])

      # Round
      print.gen[, "r2"] <- formatC(print.gen[, "r2"], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))
      print.gen[, "perc"] <- formatC(print.gen[, "perc"], digits = digits - 1L, format = "f", zero.print = ifelse(digits - 1L > 0L, paste0("0.", paste(rep(0L, times = digits - 1L), collapse = "")), "0"))

      # Percentages
      print.gen[, "perc"] <- paste0(print.gen[, "perc"], "%")

      # Variable names
      print.gen <- data.frame(Variable = rownames(print.gen), print.gen)

      # Column names
      print.gen <- rbind(c("Variable", "R2", "Perc", "Rank"), print.gen)

      # Add blank space
      print.gen[, 1L] <- c(paste0("   ", print.gen[1L, 1L], collapse = ""), paste0("    ", print.gen[-c(1L, nrow(print.gen)), 1L]),  paste0("   ", print.gen[nrow(print.gen), 1L], collapse = ""))

      # Justify
      print.gen[, 1L] <- format(print.gen[, 1L, drop = FALSE], justify = "left")
      print.gen[, -1L] <- format(print.gen[, -1L, drop = FALSE], justify = "right")

      # R Markdown not in progress
      if (is.null(getOption("knitr.in.progress"))) { print.gen[1L, ] <- sub("2", "\u00B2", print.gen[1L, ]) }

      #...................
      ### Print ####

      write.table(print.gen, quote = FALSE, row.names = FALSE, col.names = FALSE)

    }

    #...................
    ### Conditional dominance ####

    if (isTRUE("cond" %in% x$args$print)) {

      cat("\n  Conditional Dominance\n\n")

      # Extract result table
      print.cond <- print.object$cond

      #...................
      ### Format ####

      # Diagonal
      diag(print.cond) <- ""

      # Variable names
      print.cond <- data.frame(Variable = rownames(print.cond), print.cond)

      # Column names
      print.cond <- rbind(c("Dominates", rownames(print.cond)), print.cond)

      # Add blank space
      print.cond[, 1L] <- c(paste0("   ", print.cond[1L, 1L], collapse = ""), paste0("    ", print.cond[-1L, 1L]))

      # Justify
      print.cond[, 1L] <- format(print.cond[, 1L, drop = FALSE], justify = "left")
      print.cond[, -1L] <- format(print.cond[, -1L, drop = FALSE], justify = "right")

      #...................
      ### Print ####

      write.table(data.frame(label = c("          Dominated")), quote = FALSE, row.names = FALSE, col.names = FALSE)
      write.table(print.cond, quote = FALSE, row.names = FALSE, col.names = FALSE)

    }

    #...................
    ### Complete dominance ####

    if (isTRUE("comp" %in% x$args$print)) {

      cat("\n  Complete Dominance\n\n")

      # Extract result table
      print.comp <- print.object$comp

      #...................
      ### Format ####

      # Diagonal
      diag(print.comp) <- ""

      # Variable names
      print.comp <- data.frame(Variable = rownames(print.comp), print.comp)

      # Column names
      print.comp <- rbind(c("Dominates", rownames(print.comp)), print.comp)

      # Add blank space
      print.comp[, 1L] <- c(paste0("   ", print.comp[1L, 1L], collapse = ""), paste0("    ", print.comp[-1L, 1L]))

      # Justify
      print.comp[, 1L] <- format(print.comp[, 1L, drop = FALSE], justify = "left")
      print.comp[, -1L] <- format(print.comp[, -1L, drop = FALSE], justify = "right")

      #...................
      ### Print ####

      write.table(data.frame(label = c("          Dominated")), quote = FALSE, row.names = FALSE, col.names = FALSE)
      write.table(print.comp, quote = FALSE, row.names = FALSE, col.names = FALSE)

    }

  #_____________________________________________________________________________
  #
  # Effect Sizes for Categorical Variablese ------------------------------------
  }, effsize = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Round ####

    print.object[, colnames(print.object)[!colnames(print.object) %in% c("n", "var")]] <- apply(print.object[, colnames(print.object)[!colnames(print.object) %in% c("n", "var")]], 2L, function(y) formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")))

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Format ####

    if (isTRUE("var" %in% colnames(print.object))) {

      print.object <- rbind(c("Variable", "n", switch(x$args$type, phi = "Phi", cramer = "V", tschuprow = "T", cont = "C", w = "w", fei = "Feo"), "Low", "Upp"), print.object)

      print.object[, 1L] <- format(print.object[, 1L], justify = "left")

    } else {

      print.object <- rbind(c("n", switch(x$args$type, phi = "Phi", cramer = "V", tschuprow = "T", cont = "C", w = "w", fei = "Feo"), "Low", "Upp"), print.object)

    }

    print.object <- apply(print.object, 2L, format, justify = "right")
    print.object[, 1L] <- paste(" ", print.object[, 1L])

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Output ####

    cat(paste0(switch(x$args$type,
              phi = {

                if (isTRUE(x$args$adjust)) { " Adjusted Phi Coefficient: " } else { " Phi Coefficient: " }

              }, cramer = {

                if (isTRUE(x$args$adjust)) { " Bias-Corrected Cramer's V: " } else { " Cramer's V: " }

              }, tschuprow = {

                 if (isTRUE(x$args$adjust)) { " Bias-Corrected Tschuprow's T: " } else { " Tschuprow's T: " }

              }, cont = {

                 if (isTRUE(x$args$adjust)) { " Adjusted Pearson's Contingency Coefficient: " } else { " Pearson's Contingency Coefficient: " }

              }, w = { cat(" Cohen's w: ")
              }, fei = { " Fei: "}),
              switch(x$args$alternative,
                     two.sided = "Two-Sided ",
                     less = "One-Sided ",
                     greater = "One-Sided "),
              paste0(round(x$args$conf.level * 100L, digits = 2L), "% "), "Confidence Interval\n\n"))

    write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE, na = "")

    # Note
    if (isTRUE(x$args$indep && ncol(x$data) > 2L)) { cat(paste0("\n Note. The focal variable is ", colnames(x$data)[1L])) }

  #_____________________________________________________________________________
  #
  # Frequency Table ----------------------------------------------------------------
  }, freq = {

    if (isTRUE(check)) {

      #..................
      # Check input 'print'
      if (isTRUE(any(!print %in% c("no", "all", "perc", "v.perc")))) {

        stop("Character string in the argument 'print' does not match with \"no\", \"all\", \"perc\", or \"v.perc\".", call. = FALSE)

      }

    }

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
      if (isTRUE(!all(print %in% c("all", "alpha", "item")))) { stop("Character strings in the argument 'print' do not all match with \"all\", \"alpha\", or \"item\".", call. = FALSE) }

    }

    #----------------------------------------
    # Arguments

    # Print coefficient alpha and/or item statistic
    if (length(print) == 1L && "all" %in% print) { print <- c("alpha", "item") }

    #-----------------------------------------
    # Alpha
    if (isTRUE("alpha" %in% print)) {

      if (isTRUE(all(c("low", "upp") %in% names(print.object$alpha)))) {

        print.object$alpha$n <- format(paste(" ", print.object$alpha$n), justify = "right")

        print.object$alpha$items <- format(print.object$alpha$items, justify = "right")

        print.object$alpha$alpha <- formatC(print.object$alpha$alpha, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

        print.object$alpha$low <- formatC(print.object$alpha$low, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

        print.object$alpha$upp <- formatC(print.object$alpha$upp, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

        print.object$alpha <- rbind(c("n", "Items", "Alpha", "Low", "Upp"), print.object$alpha)

        print.object$alpha <- apply(print.object$alpha, 2L, function(y) format(y, justify = "right"))

        cat(paste0(ifelse(isTRUE(x$args$std), " Standardized ", " Unstandardized "), "Coefficient Alpha with ", x$args$conf.level*100L, "% Confidence Interval\n\n"))

        write.table(print.object$alpha, quote = FALSE, row.names = FALSE, col.names = FALSE)

      } else {

        print.object$alpha$alpha <- formatC(print.object$alpha$alpha, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

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

      print.object$itemstat$pNA <- paste0(formatC(print.object$itemstat$pNA, digits = 2L, format = "f", zero.print = paste0("0.", paste(rep(0L, times = 2L), collapse = ""))), "%")

      print.object$itemstat$m <- formatC(print.object$itemstat$m, digits = 2L, format = "f", zero.print = paste0("0.", paste(rep(0L, times = 2L), collapse = "")))

      print.object$itemstat$sd <- formatC(print.object$itemstat$sd, digits = 2L, format = "f", zero.print = paste0("0.", paste(rep(0L, times = 2L), collapse = "")))

      print.object$itemstat$min <- formatC(print.object$itemstat$min, digits = 2L, format = "f", zero.print = paste0("0.", paste(rep(0L, times = 2L), collapse = "")))

      print.object$itemstat$max <- formatC(print.object$itemstat$max, digits = 2L, format = "f", zero.print = paste0("0.", paste(rep(0L, times = 2L), collapse = "")))

      print.object$itemstat$it.cor <- formatC(print.object$itemstat$it.cor, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

      print.object$itemstat$alpha <- formatC(print.object$itemstat$alpha, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

      print.object$itemstat <- rbind(c("Variable", "n", "nNA", "pNA", "M", "SD", "Min", "Max", "It.Cor", "Alpha"), print.object$itemstat)

      # Format
      print.object$itemstat[, 1L] <- format(paste(" ", print.object$itemstat[, 1L]), justify = "left")
      print.object$itemstat[, -1L] <- apply(print.object$itemstat[, -1L], 2L, function(y) format(y, justify = "right"))

      if (isTRUE("alpha" %in% print)) { cat("\n") }

      cat(" Item-Total Correlation and Coefficient Alpha if Item Deleted\n\n")

      write.table(print.object$itemstat, quote = FALSE, row.names = FALSE, col.names = FALSE)

    }

  #_____________________________________________________________________________
  #
  # Confirmatory factor analysis -----------------------------------------------
  }, item.cfa = {

    cat(" Confirmatory Factor Analysis\n")

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## lavaan summary ####

    if (isTRUE("summary" %in% x$args$print)) {

      print.summary1 <- print.object$summary[c(1L:13L), ]
      print.summary2 <- print.object$summary[-c(1L:14L), ]

      #...................
      ### Format ####

      # Include spaces
      print.summary1[1L, 1L] <- paste0(" ", print.summary1[1L, 1L])
      print.summary1[-1L, 1L] <- paste0("  ", print.summary1[-1L, 1L])

      print.summary2[1L, 1L] <- paste0(" ", print.summary2[1L, 1L])
      print.summary2[-1L, 1L] <- paste0("  ", print.summary2[-1L, 1L])

      # Justify left
      print.summary1[, 1L] <- format(print.summary1[, 1L], justify = "left")
      print.summary2[, 1L] <- format(print.summary2[, 1L], justify = "left")

      # Justify right
      print.summary1[, 2L] <- format(print.summary1[, 2L], justify = "right")
      print.summary2[, 2L] <- format(print.summary2[, 2L], justify = "right")

      # Add spaces
      print.summary2[1L, 1L] <- paste0(print.summary2[1L, 1L], paste0(rep(" ", times = nchar(paste0(print.summary1[1, c(1L, 2L)], collapse = " ")) - nchar(paste0(print.summary2[1, ], collapse = " "))), collapse = ""), collapse = "")

      # Justify left
      print.summary2[, 1L] <- format(print.summary2[, 1L], justify = "left")

      # No cluster variable
      if (isTRUE(is.null(x$args$cluster))) { print.summary2 <- print.summary2[-grep("Clusters", print.summary2[, 1L]), ] }

      #...................
      ### Print ####

      print(print.summary1, col.names = FALSE, row.names = FALSE, quote = FALSE, right = TRUE, max = 99999L)
      print(print.summary2, col.names = FALSE, row.names = FALSE, quote = FALSE, right = TRUE, max = 99999L)

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Covariance coverage ####

    if (isTRUE("coverage" %in% x$args$print)) {

      cat("\n  Covariance Coverage of Data\n\n")

      print.coverage <- print.object$coverage

      #...................
      ### Lower triangular ####

      print.coverage[upper.tri(print.coverage)] <- ""

      #...................
      ### Format ####
      print.coverage <- apply(print.coverage, 2L, function(y) ifelse(!is.na(as.numeric(y)), formatC(as.numeric(y), digits = p.digits - 1L, format = "f", zero.print = ifelse(2L > 0L, paste0("0.", paste(rep(0L, times = 2L), collapse = "")), "0")), ""))

      # Row names
      row.names(print.coverage) <- paste0("   ", colnames(print.coverage))

      #...................
      ### Print ####

      print(print.coverage, row.names = FALSE, quote = FALSE, right = TRUE, max = 99999L)

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Sample Statistics ####

    if (isTRUE("descript" %in% x$args$print)) {

      ### Continuous indicators ####
      if (isTRUE(is.null(x$args$ordered))) {

        cat("\n  Univariate Sample Statistics\n\n")

        print.itemstat <- print.object$descript

        #...................
        ### Format ####

        # Variables to round
        print.round <- c("m", "sd", "min", "max", "skew", "kurt")

        print.itemstat[, print.round] <- sapply(print.round, function(y) ifelse(!is.na(print.itemstat[, y]), formatC(print.itemstat[, y], digits = x$args$digits, format = "f", zero.print = ifelse(x$args$digits > 0L, paste0("0.", paste(rep(0L, times = x$args$digits), collapse = "")), "0")), NA))
        print.itemstat[, "pNA"] <- formatC(print.itemstat[, "pNA"], digits = digits - 1L, format = "f", zero.print = ifelse(digits - 1L > 0L, paste0("0.", paste(rep(0L, times = digits - 1L), collapse = "")), "0"))

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

        #...................
        ### Print ####

        write.table(print.itemstat, quote = FALSE, row.names = FALSE, col.names = FALSE)

      ### Ordered indicators ####
      } else if (isTRUE(x$args$ordered)) {

        cat("\n  Univariate Counts for Ordered Variables\n\n")

        print.itemfreq <- print.object$itemfreq$freq

        # Column names
        print.itemfreq <- rbind(c("Variable", colnames(print.itemfreq)[-1L]), print.itemfreq)

        # Justify left and right
        print.itemfreq[, 1L] <- format(print.itemfreq[, 1L, drop = FALSE], justify = "left")
        print.itemfreq[, -1L] <- apply(print.itemfreq[, -1L, drop = FALSE], 2L, function(y) format(y, justify = "right"))

        # Add blank space
        print.itemfreq[, "Var"] <- c(paste0("   ", print.itemfreq[1L, "Var"], collapse = ""), paste0("    ", print.itemfreq[-1L, "Var"]))
        print.itemfreq[, "Var"] <- format(c(print.itemfreq[1L, "Var"], misty::chr.trim(print.itemfreq[-1L, "Var"], side = "right")), justify = "left")

        #...................
        ### Print ####

        write.table(print.itemfreq, quote = FALSE, row.names = FALSE, col.names = FALSE)

      ### Continuous and ordered indicators ####
      } else {

        #...................
        ### Sample Statistics ####

        cat("\n  Univariate Sample Statistics\n\n")

        print.itemstat <- print.object$descript

        print.itemstat <- print.itemstat[which(!print.itemstat$variable %in% x$args$ordered),]

        #...................
        ### Format ####

        # Variables to round
        print.round <- c("m", "sd", "min", "max", "skew", "kurt")

        print.itemstat[, print.round] <- sapply(print.round, function(y) ifelse(!is.na(print.itemstat[, y]), formatC(print.itemstat[, y], digits = x$args$digits, format = "f", zero.print = ifelse(x$args$digits > 0L, paste0("0.", paste(rep(0L, times = x$args$digits), collapse = "")), "0")), NA))
        print.itemstat[, "pNA"] <- formatC(print.itemstat[, "pNA"], digits = digits - 1L, format = "f", zero.print = ifelse(digits - 1L > 0L, paste0("0.", paste(rep(0L, times = digits - 1L), collapse = "")), "0"))

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

        #...................
        ### Print ####

        write.table(print.itemstat, quote = FALSE, row.names = FALSE, col.names = FALSE)

        #...................
        ### Counts for ordered variables ####

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

        #...................
        ### Print ####

        write.table(print.itemfreq, quote = FALSE, row.names = FALSE, col.names = FALSE)

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Model fit ####

    if (isTRUE("fit" %in% x$args$print)) {

      cat("\n  Model Fit Information\n")

      print.fit <- print.object$fit

      #...................
      ### Round ####

      # Round fit indices
      print.fit[-which(print.fit[, 1L] %in% c("H0 Value, Specified Model", "Scaling Correction Factor", "H1 Value, Unrestricted Model", "Scaling Correction Factor", "Akaike (AIC)", "Bayesian (BIC)", "Sample-Size Adjusted BIC", "Test statistic", "Degrees of freedom", "P-value", "P-value RMSEA <= 0.05")), -1] <- sapply(print.fit[-which(print.fit[, 1L] %in% c("H0 Value, Specified Model", "Scaling Correction Factor", "H1 Value, Unrestricted Model", "Scaling Correction Factor", "Akaike (AIC)", "Bayesian (BIC)", "Sample-Size Adjusted BIC", "Test statistic", "Degrees of freedom", "P-value", "P-value RMSEA <= 0.05")), -1], function(y) ifelse(!is.na(y), formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

      # Loglikelihood, information criteria, and test statistic
      print.fit[which(print.fit[, 1L] %in% c("H0 Value, Specified Model", "Scaling Correction Factor", "H1 Value, Unrestricted Model", "Scaling Correction Factor", "Akaike (AIC)", "Bayesian (BIC)", "Sample-Size Adjusted BIC", "Test statistic")), -1L] <- sapply(print.fit[which(print.fit[, 1L] %in% c("H0 Value, Specified Model", "Scaling Correction Factor", "H1 Value, Unrestricted Model", "Scaling Correction Factor", "Akaike (AIC)", "Bayesian (BIC)", "Sample-Size Adjusted BIC", "Test statistic")), -1L], function(y) ifelse(!is.na(y), formatC(as.numeric(y), digits = digits - 1L, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

      # P-values
      print.fit[which(print.fit[, 1L] %in% c("P-value", "P-value RMSEA <= 0.05")), -1L] <- sapply(print.fit[which(print.fit[, 1L] %in% c("P-value", "P-value RMSEA <= 0.05")), -1L], function(y) ifelse(!is.na(y), formatC(as.numeric(y), digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0")), NA))

      # MLMVS and PML estimrator
      if (isTRUE(x$args$estimator %in% c("MLMVS", "PML"))) { print.fit[which(print.fit[, 1L] == "Degrees of freedom"), -c(1L:2L)] <- sapply(as.numeric(print.fit[which(print.fit[, 1L] == "Degrees of freedom"), -c(1L:2L)]), function(y) ifelse(!is.na(y), formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA)) }

      #...................
      ### Add labels ####

      if (isTRUE(ncol(print.fit) == 4L)) {

        print.fit <- rbind(if (isTRUE(which(print.fit[, 1L] == "Chi-Square Test of Model Fit") > 1L)) { print.fit[1L:((which(print.fit[, 1L] == "Chi-Square Test of Model Fit")) - 1L), ] }, c("", "Standard", "Scaled", ""),
                           print.fit[which(print.fit[, 1L] == "Chi-Square Test of Model Fit"):((which(print.fit[, 1L] == "Incremental Fit Indices")) - 1L), ], c("", "Standard", "Scaled", "Robust"),
                           print.fit[which(print.fit[, 1L] == "Incremental Fit Indices"):((which(print.fit[, 1L] == "Absolute Fit Indices")) - 1L), ], c("", "Standard", "Scaled", "Robust"),
                           print.fit[which(print.fit[, 1L] == "Absolute Fit Indices"):nrow(print.fit), ],  make.row.names = FALSE)

      } else {

        print.fit <- rbind(if (isTRUE(print.fit[1L, 1L] == "Loglikelihood")) { print.fit[1L:((which(print.fit[, 1L] == "Chi-Square Test of Model Fit")) - 1L), ]  },
                           if (isTRUE(!x$args$estimator %in% c("PML", "MLMVS"))) { c("", "Standard") } else { c("", "Standard", "Scaled") },
                           print.fit[which(print.fit[, 1L] == "Chi-Square Test of Model Fit"):((which(print.fit[, 1L] == "Incremental Fit Indices")) - 1L), ], if (isTRUE(!x$args$estimator %in% c("PML", "MLMVS"))) { c("", "Standard") } else { c("", "Standard", "Scaled") },
                           print.fit[which(print.fit[, 1L] == "Incremental Fit Indices"):((which(print.fit[, 1L] == "Absolute Fit Indices")) - 1L), ],if (isTRUE(!x$args$estimator %in% c("PML", "MLMVS"))) { c("", "Standard") } else { c("", "Standard", "Scaled") },
                           print.fit[which(print.fit[, 1L] == "Absolute Fit Indices"):nrow(print.fit), ], make.row.names = FALSE)

      }

      #...................
      ### Replace NA with "" ####

      print.fit <- unname(misty::na.as(print.fit, na = "", check = FALSE))

      #...................
      ### Add blank space ####

      print.fit[, 1L] <- paste0("  ", print.fit[, 1L])

      labels <- c("H0 Value, Specified Model", "Scaling Correction Factor", "H1 Value, Unrestricted Model", "Scaling Correction Factor",
                  "Akaike (AIC)", "Bayesian (BIC)", "Sample-Size Adjusted BIC", "Test statistic", "Degrees of freedom", "P-value",
                  "Scaling correction factor", "CFI", "TLI", "RMSEA", "90 Percent CI - lower", "90 Percent CI - upper", "P-value RMSEA <= 0.05", "SRMR")

      print.fit[which(misty::chr.trim(print.fit[, 1L]) %in% labels), 1L] <- paste("", unlist(print.fit[which(misty::chr.trim(print.fit[, 1L]) %in% labels), 1L]))

      print.fit[which(misty::chr.trim(print.fit[, 1L]) %in% c("90 Percent CI - lower", "90 Percent CI - upper", "P-value RMSEA <= 0.05")), 1L] <- paste("", unlist(print.fit[which(misty::chr.trim(print.fit[, 1]) %in% c("90 Percent CI - lower", "90 Percent CI - upper", "P-value RMSEA <= 0.05")), 1L]))

      #...................
      ### Justify left and right ####

      print.fit[, 1L] <- format(print.fit[, 1L, drop = FALSE], justify = "left")

      print.fit[, -1L] <- apply(print.fit[, -1L, drop = FALSE], 2L, function(y) format(y, justify = "right"))

      #...................
      ### Print ####

      print(print.fit, row.names = FALSE, quote = FALSE, right = TRUE, max = 99999L)

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Parameter estimates ####

    if (isTRUE("est" %in% x$args$print)) {

      cat("\n  Model Results\n\n")

      print.param <- print.object$param

      #...................
      ### Round ####

      # digits
      print.param[, -c(1L:4L, 8L)] <- lapply(print.param[, -c(1L:4L, 8L)], function(y) ifelse(!is.na(y), formatC(y, digits = x$args$digits, format = "f", zero.print = ifelse(x$args$digits > 0L, paste0("0.", paste(rep(0L, times = x$args$digits), collapse = "")), "0")), NA))

      # p.digits
      print.param[, "pvalue"] <- formatC(as.numeric(print.param[, "pvalue"]), digits = x$args$p.digits, format = "f", zero.print = ifelse(x$args$p.digits > 0L, paste0("0.", paste(rep(0L, times = x$args$p.digits), collapse = "")), "0"))

      #...................
      ### Convert NA into "" ####

      print.param[grep("~", print.param$rhs), c("est", "se", "z", "pvalue", "stdyx")] <- ""

      #...................
      ### Column names ####

      print.param <- rbind(c("", "", "", "", "Estimate", "Std.Err", "z-value", "P(>|z|)", "StdYX"),
                           print.param)

      #...................
      ### Add blank spaces ####

      print.param[grep("~", print.param$rhs), "rhs"] <- paste("   ", print.param[grep("~", print.param$rhs), "rhs"])
      print.param[-grep("~", print.param$rhs), "rhs"] <- paste("     ", print.param[-grep("~", print.param$rhs), "rhs"])

      #...................
      ### Justify ####

      print.param[, "rhs"] <- format(print.param[, "rhs", drop = FALSE], justify = "left")
      print.param[, -c(1L:4L)] <- apply(print.param[, -c(1L:4L), drop = FALSE], 2L, function(y) format(y, justify = "right"))

      #...................
      ### Print ####

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

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Modification indices ####

    if (isTRUE("modind" %in% x$args$print)) {

      cat("\n  Modification Indices\n\n")

      print.modind <- print.object$modind

      #### Modification indices not available
      if (isTRUE(is.null(print.modind))) {

        cat("\n   Modification indices are not available.\n")

        #### Modification available
      } else {

        # Filter modification indices
        print.modind <- print.modind[which(print.modind[, "mi"] >= x$args$mod.minval), ]

        if (isTRUE(nrow(print.modind) == 0L)) {

          cat(paste0("   No modification indices above the minimum value ", round(x$args$mod.minval, digits = 2L), ".\n"))

          #### Modification indices
        } else {

          # Round
          print.modind[, -c(1L:3L)] <- lapply(print.modind[, -c(1L:3L)], function(y) ifelse(!is.na(y), formatC(y, digits = x$args$digits, format = "f", zero.print = ifelse(x$args$digits > 0L, paste0("0.", paste(rep(0L, times = x$args$digits), collapse = "")), "0")), NA))

          # Column names
          print.modind <- rbind(c("", "", "", "MI", "EPC", "StdYX"), print.modind)

          # Add blank spaces
          print.modind[, "lhs"] <- paste("   ", print.modind[, "lhs"])

          # Justify
          print.modind[ c("lhs", "op", "rhs")] <- format(print.modind[, c("lhs", "op", "rhs")], justify = "left")
          print.modind[, -c(1L, 2L)] <- apply(print.modind[, -c(1L, 2L), drop = FALSE], 2L, function(y) format(y, justify = "right"))

          ###
          # Factor loadings

          print.modind.load <- print.modind[print.modind$op == "=~", ]

          if (isTRUE(nrow(print.modind.load) > 0L)) {

            cat("   Factor Loading\n")

            # Print header
            write.table(print.modind[1L, ], quote = FALSE, row.names = FALSE, col.names = FALSE)

            write.table(print.modind.load, quote = FALSE, row.names = FALSE, col.names = FALSE)

            cat("\n")

          }

          # Residual covariances
          print.modind.cov <- print.modind[print.modind$op == "~~", ]

          if (isTRUE(nrow(print.modind.cov) > 0L)) {

            cat("   Residual Covariances\n")

            # Print header
            write.table(print.modind[1L, ], quote = FALSE, row.names = FALSE, col.names = FALSE)

            write.table(print.modind.cov, quote = FALSE, row.names = FALSE, col.names = FALSE)

          }

          # Note
          cat(paste0("\n   Note. Minimum value for printing modification indices is ", round(x$args$mod.minval, digits = 2L), ".\n"))

        }

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Residual Correlation Matrix ####

    if (isTRUE("resid" %in% x$args$print)) {

      # Extract result table
      print.resid <- print.object$resid

      cat("\n  Residual Correlation Matrix\n\n")

      ### Residual correlation matrix not available
      if (isTRUE(is.null(unlist(print.resid)))) {

        cat("   Residual correlation matrix is not available.")

        ### Residual correlation matrix available
      } else {

        #### Numbers and unicode
        numbers <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
        unicode <- c("\U1D7CE", "\U1D7CF", "\U1D7D0", "\U1D7EF", "\U1D7F0", "\U1D7F1", "\U1D7F2", "\U1D7F3", "\U1D7F4", "\U1D7F5")

        #### Round
        print.resid <- apply(print.resid, 2L, function(y) ifelse(!is.na(y), formatC(y, digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0")), NA))

        #### Lower triangular
        print.resid[upper.tri(print.resid)] <- ""

        #### Correlation coefficients in boldface
        if (isTRUE(is.null(getOption("knitr.in.progress")) && x$args$resid.minval != 1L)) { print.resid[lower.tri(print.resid)][which(abs(x$result$resid[lower.tri(x$result$resid)]) > x$args$resid.minval)] <- na.omit(misty::chr.gsub(numbers, unicode, print.resid[lower.tri(print.resid)][which(abs(x$result$resid[lower.tri(x$result$resid)]) > x$args$resid.minval)])) }

        #### Separate residual mean
        print.resid <- rbind(print.resid[1L:(nrow(print.resid) - 1L), ], rep("", times = ncol(print.resid)), Mean = print.resid[nrow(print.resid), ])

        #### Column names
        print.resid <- rbind(colnames(print.resid), print.resid)
        print.resid <- cbind(rownames(print.resid), print.resid)

        #### Add blank spaces
        print.resid[, 1L] <- paste("  ", print.resid[, 1L])

        #### Justify
        print.resid[, 1L] <- format(print.resid[, 1L], justify = "left")
        print.resid[, -1L] <- format(print.resid[, -1L], justify = "right")

        #...................
        ### Print ####
        write.table(print.resid, quote = FALSE, row.names = FALSE, col.names = FALSE)

        # Note
        if (isTRUE(x$args$resid.minval < 1L)) { cat(paste0("\n  Note. Minimum absolute value for highlighting correlation coefficients is ", round(x$args$resid.minval, digits = 2L), "\n")) }

      }

    }

  #_____________________________________________________________________________
  #
  # Between-Group and Longitudinal Measurement Invariance Evaluation -----------
  }, item.invar = {

    if (isTRUE(!x$args$long)) { cat(" Between-Group Measurement Invariance Evaluation\n") } else { cat(" Longitudinal Measurement Invariance Evaluation\n") }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## lavaan summary ####

    if (isTRUE("summary" %in% x$args$print)) {

      print.summary1 <- print.object$summary
      print.summary2 <- print.object$summary[-c(1L:12L), ]

      #...................
      ### Format ####

      # Include spaces
      print.summary1[1L, 1L] <- paste0(" ", print.summary1[1L, 1L])
      print.summary1[-1L, 1L] <- paste0("  ", unlist(print.summary1[-1L, 1L]))

      print.summary2[1L, 1L] <- paste0(" ", print.summary2[1L, 1L])
      print.summary2[-1L, 1L] <- paste0("  ", unlist(print.summary2[-1L, 1L]))

      if (isTRUE(!x$arg$long)) {

        print.summary2[8L:(8L + (length(na.omit(unique(x$data$.group))) - 1L)), 1L] <- paste0(" ", unlist(print.summary2[8L:(8L + (length(unique(na.omit(x$data$.group))) - 1L)), 1L]))

      }

      # Justify left
      print.summary1[, 1L] <- format(print.summary1[, 1L], justify = "left")
      print.summary2[, 1L] <- format(print.summary2[, 1L], justify = "left")

      #...................
      ### Print ####

      print(print.summary1[1L:11L, ], col.names = FALSE, row.names = FALSE, quote = FALSE, right = TRUE, max = 99999L)
      print(print.summary2, col.names = FALSE, row.names = FALSE, quote = FALSE, right = TRUE, max = 99999L)

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Covariance coverage ####

    if (isTRUE("coverage" %in% x$args$print)) {

      cat("\n  Covariance Coverage of Data\n\n")

      print.coverage <- print.object$coverage

      ### Between-group measurement invariance ####
      if (isTRUE(!x$args$long)) {

        #...................
        ### Lower triangular ####
        for (i in seq_along(print.coverage)) { print.coverage[[i]][upper.tri(print.coverage[[i]])] <- "" }

        #...................
        ### Format ####
        for (i in seq_along(print.coverage)) { print.coverage[[i]] <- apply(print.coverage[[i]], 2L, function(y) ifelse(!is.na(as.numeric(y)), formatC(as.numeric(y), digits = p.digits - 1L, format = "f", zero.print = ifelse(p.digits - 1L > 0L, paste0("0.", paste(rep(0L, times = 2L), collapse = "")), "0")), "")) }

        # Row names
        for (i in seq_along(print.coverage)) { row.names(print.coverage[[i]]) <- paste0("    ", colnames(print.coverage[[i]])) }

        #...................
        ### Print ####

        for (i in seq_along(print.coverage)) {

          cat("   Group:", names(print.coverage)[i], "\n")

          print(print.coverage[[i]], row.names = FALSE, quote = FALSE, right = TRUE, max = 99999L)

          if (isTRUE(i != rev(seq_along(print.coverage))[1L])) { cat("\n") }

        }

      ### Longitudinal measurement invariance ####
      } else {

        #...................
        ### Lower triangular ####
        print.coverage[upper.tri(print.coverage)] <- ""

        #...................
        ### Format ####
        print.coverage <- apply(print.coverage, 2L, function(y) ifelse(!is.na(as.numeric(y)), formatC(as.numeric(y), digits = p.digits - 1L, format = "f", zero.print = ifelse(p.digits - 1L > 0L, paste0("0.", paste(rep(0L, times = 2L), collapse = "")), "0")), ""))

        # Row names
        row.names(print.coverage) <- paste0("   ", colnames(print.coverage))

        #...................
        ### Print ####
        print(print.coverage, row.names = FALSE, quote = FALSE, right = TRUE, max = 99999L)

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Sample Statistics ####

    if (isTRUE("descript" %in% x$args$print)) {

      cat("\n  Univariate Sample Statistics\n\n")

      print.itemstat <- print.object$descript

      #...................
      ### Format ####

      # Variables to round
      print.round <- c("m", "sd", "min", "max", "skew", "kurt")

      # Round
      print.itemstat[, print.round] <- sapply(print.round, function(y) ifelse(!is.na(print.itemstat[, y]), formatC(print.itemstat[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))
      print.itemstat[, "pNA"] <- formatC(print.itemstat[, "pNA"], digits = digits - 1L, format = "f", zero.print = ifelse(digits - 1L > 0L, paste0("0.", paste(rep(0L, times = digits - 1L), collapse = "")), "0"))

      # Percentages
      print.itemstat[, "pNA"] <- paste0(print.itemstat[, "pNA"], "%")

      # Column names
      if (isTRUE(!x$args$long)) {

        print.itemstat <- rbind(c("Group", "Variable", "n", "nNA", "pNA", "M", "SD", "Min", "Max",  "Skew", "Kurt"), print.itemstat)

      } else {

        print.itemstat <- rbind(c("Variable", "n", "nNA", "pNA", "M", "SD", "Min", "Max",  "Skew", "Kurt"), print.itemstat)

      }

      # Justify left and right
      print.itemstat[, 1L] <- format(print.itemstat[, 1L, drop = FALSE], justify = "left")
      if (isTRUE(!x$args$long)) { print.itemstat[, 2L] <- format(print.itemstat[, 2L, drop = FALSE], justify = "left") }

      if (isTRUE(!x$args$long)) {

        print.itemstat[, -c(1L:2L)] <- apply(print.itemstat[, -c(1L:2L), drop = FALSE], 2L, function(y) format(y, justify = "right"))

      } else {

        print.itemstat[, -1L] <- apply(print.itemstat[, -1L, drop = FALSE], 2L, function(y) format(y, justify = "right"))

      }

      # Add blank space
      if (isTRUE(!x$args$long)) {

        print.itemstat[, "group"] <- c(paste0("   ", print.itemstat[1L, "group"], collapse = ""), paste0("    ", print.itemstat[-1L, "group"]))
        print.itemstat[, "group"] <- format(c(print.itemstat[1L, "group"], misty::chr.trim(print.itemstat[-1L, "group"], side = "right")), justify = "left")

      } else {

        print.itemstat[, "variable"] <- c(paste0("   ", print.itemstat[1L, "variable"], collapse = ""), paste0("    ", print.itemstat[-1L, "variable"]))
        print.itemstat[, "variable"] <- format(c(print.itemstat[1L, "variable"], misty::chr.trim(print.itemstat[-1L, "variable"], side = "right")), justify = "left")

      }

      if (isTRUE(!x$args$long)) {

        print.itemstat[, "variable"] <- c(paste0("", print.itemstat[1L, "variable"], collapse = ""), paste0(" ", print.itemstat[-1L, "variable"]))
        print.itemstat[, "variable"] <- format(c(print.itemstat[1L, "variable"], misty::chr.trim(print.itemstat[-1L, "variable"], side = "right")), justify = "left")

      }

      #...................
      ### Print ####

      write.table(print.itemstat, quote = FALSE, row.names = FALSE, col.names = FALSE)

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Model fit ####

    if (isTRUE("fit" %in% x$args$print)) {

      if (isTRUE(x$args$invar != "config")) { cat("\n  Model Fit Information and Model Comparison\n") } else { cat("\n  Model Fit Information\n") }

      print.fit <- print.object$fit

      print.fit.stand <- print.fit$stand
      print.fit.scaled <- print.fit$scaled
      print.fit.robust <- print.fit$robust

      #...................
      ### Round

      ##### Fit indices
      pos.round.stand <- which(print.fit.stand[, 1L] %in% c("CFI", "TLI", "RMSEA", "90 Percent CI - lower", "90 Percent CI - upper", "SRMR"))
      pos.round.scaled <- which(print.fit.scaled[, 1L] %in% c("CFI", "TLI", "RMSEA", "90 Percent CI - lower", "90 Percent CI - upper", "SRMR"))

      print.fit.stand[pos.round.stand, -1L] <- sapply(print.fit.stand[pos.round.stand, -1L], function(y) ifelse(!is.na(y), formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))
      if (isTRUE(!is.null(print.fit.scaled))) { print.fit.scaled[pos.round.scaled, -1L] <- sapply(print.fit.scaled[pos.round.scaled, -1L], function(y) ifelse(!is.na(y), formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA)) }
      if (isTRUE(!is.null(print.fit.robust))) { print.fit.robust[pos.round.scaled, -1L] <- sapply(print.fit.robust[pos.round.scaled, -1L], function(y) ifelse(!is.na(y), formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA)) }

      ##### Chi-squared and Information criteria
      if (isTRUE(x$args$estimator != "PML")) {

        pos.noround.stand <- which(print.fit.stand[, 1L] %in% c("", "Chi-Square Test of Model Fit", "Incremental Fit Indices", "Absolute Fit Indices", "Information Criteria", "Degrees of freedom", "P-value", "CFI", "TLI", "RMSEA", "90 Percent CI - lower", "90 Percent CI - upper", "P-value RMSEA <= 0.05", "SRMR"))

      } else {

        pos.noround.stand <- which(print.fit.stand[, 1L] %in% c("", "Chi-Square Test of Model Fit", "Incremental Fit Indices", "Absolute Fit Indices", "Information Criteria", "P-value", "CFI", "TLI", "RMSEA", "90 Percent CI - lower", "90 Percent CI - upper", "P-value RMSEA <= 0.05", "SRMR"))

      }

      pos.noround.scaled <- which(print.fit.scaled[, 1L] %in% c("", "Chi-Square Test of Model Fit", "Incremental Fit Indices", "Absolute Fit Indices", "Information Criteria", "Degrees of freedom", "P-value", "CFI", "TLI", "RMSEA", "90 Percent CI - lower", "90 Percent CI - upper", "P-value RMSEA <= 0.05", "SRMR"))

      print.fit.stand[-pos.noround.stand, -1L] <- sapply(print.fit.stand[-pos.noround.stand, -1L], function(y) ifelse(!is.na(y), formatC(as.numeric(y), digits = digits - 1L, format = "f", zero.print = ifelse(digits - 1L - 1L > 0L, paste0("0.", paste(rep(0L, times = digits - 1L), collapse = "")), "0")), NA))
      if (isTRUE(!is.null(print.fit.scaled))) { print.fit.scaled[-pos.noround.scaled, -1L] <- sapply(print.fit.scaled[-pos.noround.scaled, -1L], function(y) ifelse(!is.na(y), formatC(as.numeric(y), digits = digits - 1L, format = "f", zero.print = ifelse(digits - 1L > 0L, paste0("0.", paste(rep(0L, times = digits - 1L), collapse = "")), "0")), NA)) }
      if (isTRUE(!is.null(print.fit.robust))) { print.fit.robust[-pos.noround.scaled, -1L] <- sapply(print.fit.robust[-pos.noround.scaled, -1L], function(y) ifelse(!is.na(y), formatC(as.numeric(y), digits = digits - 1L, format = "f", zero.print = ifelse(digits - 1L > 0L, paste0("0.", paste(rep(0L, times = digits - 1L), collapse = "")), "0")), NA)) }

      ##### p.digits
      pos.round.stand <- which(print.fit.stand[, 1L] %in% c("P-value", "P-value RMSEA <= 0.05"))
      pos.round.scaled <- which(print.fit.scaled[, 1L] %in% c("P-value", "P-value RMSEA <= 0.05"))

      print.fit.stand[pos.round.stand, -1L] <- sapply(print.fit.stand[pos.round.stand, -1L], function(y) ifelse(!is.na(y), formatC(as.numeric(y), digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0")), NA))
      if (isTRUE(!is.null(print.fit.scaled))) { print.fit.scaled[pos.round.scaled, -1L] <- sapply(print.fit.scaled[pos.round.scaled, -1L], function(y) ifelse(!is.na(y), formatC(as.numeric(y), digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0")), NA)) }
      if (isTRUE(!is.null(print.fit.robust))) { print.fit.robust[pos.round.scaled, -1L] <- sapply(print.fit.robust[pos.round.scaled, -1L], function(y) ifelse(!is.na(y), formatC(as.numeric(y), digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0")), NA)) }

      #...................
      ##### Add labels ####

      print.fit.stand <- rbind(switch(x$args$invar, config = { c("", "Config") }, metric = { c("", "Config", "Metric", "DMetric") }, scalar = { c("", "Config", "Metric", "Scalar", "DMetric", "DScalar") }, strict = { c("", "Config", "Metric", "Scalar", "Strict", "DMetric", "DScalar", "DStrict") }), print.fit.stand, make.row.names = FALSE)
      if (isTRUE(!is.null(print.fit.scaled))) { print.fit.scaled <- rbind(switch(x$args$invar, config = { c("", "Config") }, metric = { c("", "Config", "Metric", "DMetric") }, scalar = { c("", "Config", "Metric", "Scalar", "DMetric", "DScalar") }, strict = { c("", "Config", "Metric", "Scalar", "Strict", "DMetric", "DScalar", "DStrict") }), print.fit.scaled, make.row.names = FALSE) }
      if (isTRUE(!is.null(print.fit.robust))) { print.fit.robust <- rbind(switch(x$args$invar, config = { c("", "Config") }, metric = { c("", "Config", "Metric", "DMetric") }, scalar = { c("", "Config", "Metric", "Scalar", "DMetric", "DScalar") }, strict = { c("", "Config", "Metric", "Scalar", "Strict", "DMetric", "DScalar", "DStrict") }), print.fit.robust, make.row.names = FALSE) }

      #...................
      ##### Replace NA with "" ####

      print.fit.stand <- unname(misty::na.as(print.fit.stand, na = ""))
      if (isTRUE(!is.null(print.fit.scaled))) { print.fit.scaled <- unname(misty::na.as(print.fit.scaled, na = "")) }
      if (isTRUE(!is.null(print.fit.robust))) { print.fit.robust <- unname(misty::na.as(print.fit.robust, na = "")) }

      #...................
      ##### Add blank space ####

      pos.noblank.stand <- which(print.fit.stand[, 1L] %in% c("", "Chi-Square Test of Model Fit", "Incremental Fit Indices", "Absolute Fit Indices", "Information Criteria"))
      pos.noblank.group.stand <- which(print.fit.stand[, 1L] %in% c("", "Chi-Square Test of Model Fit", "Incremental Fit Indices", "Absolute Fit Indices", "Information Criteria", "Test statistic", "Degrees of freedom", "P-value", "CFI", "TLI", "RMSEA", "90 Percent CI - lower", "90 Percent CI - upper", "P-value RMSEA <= 0.05", "SRMR", "Akaike (AIC)", "Bayesian (BIC)", "Sample-Size Adjusted BIC"))
      pos.blank.rmsea.stand <- which(print.fit.stand[, 1L] %in% c("90 Percent CI - lower", "90 Percent CI - upper", "P-value RMSEA <= 0.05"))

      pos.noblank.scaled <- which(print.fit.scaled[, 1L] %in% c("", "Chi-Square Test of Model Fit", "Incremental Fit Indices", "Absolute Fit Indices", "Information Criteria"))
      pos.noblank.group.scaled <- which(print.fit.scaled[, 1L] %in% c("", "Chi-Square Test of Model Fit", "Incremental Fit Indices", "Absolute Fit Indices", "Information Criteria", "Test statistic", "Degrees of freedom", "P-value", "Scaling Correction Factor", "CFI", "TLI", "RMSEA", "90 Percent CI - lower", "90 Percent CI - upper", "P-value RMSEA <= 0.05", "SRMR", "Akaike (AIC)", "Bayesian (BIC)", "Sample-Size Adjusted BIC"))
      pos.blank.rmsea.scaled <- which(print.fit.scaled[, 1L] %in% c("90 Percent CI - lower", "90 Percent CI - upper", "P-value RMSEA <= 0.05"))

      print.fit.stand[, 1L] <- paste0("    ", print.fit.stand[, 1L])
      if (isTRUE(!is.null(print.fit.scaled))) { print.fit.scaled[, 1L] <- paste0("    ", print.fit.scaled[, 1L]) }
      if (isTRUE(!is.null(print.fit.robust))) { print.fit.robust[, 1L] <- paste0("    ", print.fit.robust[, 1L]) }

      print.fit.stand[-pos.noblank.stand, 1L] <- paste("", unlist(print.fit.stand[-pos.noblank.stand, 1L]))
      if (isTRUE(!is.null(print.fit.scaled))) { print.fit.scaled[-pos.noblank.scaled, 1L] <- paste("", unlist(print.fit.scaled[-pos.noblank.scaled, 1L])) }
      if (isTRUE(!is.null(print.fit.robust))) { print.fit.robust[-pos.noblank.scaled, 1L] <- paste("", unlist(print.fit.robust[-pos.noblank.scaled, 1L])) }

      # Groups
      print.fit.stand[-pos.noblank.group.stand, 1L] <- paste("", unlist(print.fit.stand[-pos.noblank.group.stand, 1L]))
      if (isTRUE(!is.null(print.fit.scaled))) { print.fit.scaled[-pos.noblank.group.scaled, 1L] <- paste("", unlist(print.fit.scaled[-pos.noblank.group.scaled, 1L])) }
      if (isTRUE(!is.null(print.fit.robust))) { print.fit.robust[-pos.noblank.group.scaled, 1L] <- paste("", unlist(print.fit.robust[-pos.noblank.group.scaled, 1L])) }

      # RMSEA 95% CI - p-values
      print.fit.stand[pos.blank.rmsea.stand, 1L] <- paste("", unlist(print.fit.stand[pos.blank.rmsea.stand, 1L]))
      if (isTRUE(!is.null(print.fit.scaled))) { print.fit.scaled[pos.blank.rmsea.scaled, 1L] <- paste("", unlist(print.fit.scaled[pos.blank.rmsea.scaled, 1L])) }
      if (isTRUE(!is.null(print.fit.robust))) { print.fit.robust[pos.blank.rmsea.scaled, 1L] <- paste("", unlist(print.fit.robust[pos.blank.rmsea.scaled, 1L])) }

      #...................
      ##### Justify left and right ####

      print.fit.stand[, 1L] <- format(print.fit.stand[, 1L, drop = FALSE], justify = "left")
      if (isTRUE(!is.null(print.fit.scaled))) { print.fit.scaled[, 1L] <- format(print.fit.scaled[, 1L, drop = FALSE], justify = "left") }
      if (isTRUE(!is.null(print.fit.robust))) { print.fit.robust[, 1L] <- format(print.fit.robust[, 1L, drop = FALSE], justify = "left") }

      print.fit.stand[, -1L] <- apply(print.fit.stand[, -1L, drop = FALSE], 2L, function(y) format(y, justify = "right"))
      if (isTRUE(!is.null(print.fit.scaled))) { print.fit.scaled[, -1L] <- apply(print.fit.scaled[, -1L, drop = FALSE], 2L, function(y) format(y, justify = "right")) }
      if (isTRUE(!is.null(print.fit.robust))) { print.fit.robust[, -1L] <- apply(print.fit.robust[, -1L, drop = FALSE], 2L, function(y) format(y, justify = "right")) }

      #...................
      ##### Print ####

      if (isTRUE("standard" %in% x$args$print.fit)) {

        cat("\n   Standard CFI, TLI, and RMSEA\n")

        # R Markdown not in progress
        if (is.null(getOption("knitr.in.progress"))) { print.fit.stand[1L, ] <- sub("D", "\u0394", print.fit.stand[1L, ]) }

        write.table(print.fit.stand, quote = FALSE, row.names = FALSE, col.names = FALSE)

      }

      if (isTRUE("scaled" %in% x$args$print.fit)) {

        cat("\n   Scaled CFI, TLI, and RMSEA\n")

        # R Markdown not in progress
        if (is.null(getOption("knitr.in.progress"))) { print.fit.scaled[1L, ] <- sub("D", "\u0394", print.fit.scaled[1L, ]) }

        write.table(print.fit.scaled, quote = FALSE, row.names = FALSE, col.names = FALSE)

      }

      if (isTRUE("robust" %in% x$args$print.fit)) {

        cat("\n   Robust CFI, TLI, and RMSEA\n")

        # R Markdown not in progress
        if (is.null(getOption("knitr.in.progress"))) { print.fit.robust[1L, ] <- sub("D", "\u0394", print.fit.robust[1L, ]) }

        write.table(print.fit.robust, quote = FALSE, row.names = FALSE, col.names = FALSE)

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Parameter estimates ####

    if (isTRUE("est" %in% x$args$print)) {

      switch(x$args$invar,
             ### Configural invariance ####
             config = {

               cat("\n  Model Results: Configural Invariance Model\n")
               print.param <- print.object$param$config

               ### Metric invariance ####
             }, metric = {

               cat("\n  Model Results: Metric Invariance Model\n")
               print.param <- print.object$param$metric

               ### Scalar invariance ####
             }, scalar = {

               cat("\n  Model Results: Scalar Invariance Model\n")
               print.param <- print.object$param$scalar

               ### Strict invariance ####
             }, strict = {

               cat("\n  Model Results: Strict Invariance Model\n")
               print.param <- print.object$param$strict

             })

      if (isTRUE(x$args$long)) { cat("\n") }

      #### Round ####

      # digits
      print.param[, c("est", "se", "z", "stdyx")] <- lapply(print.param[, c("est", "se", "z", "stdyx")], function(y) ifelse(!is.na(y), formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

      # p.digits
      print.param[, "pvalue"] <- formatC(as.numeric(print.param[, "pvalue"]), digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

      #### Add blank spaces ####
      print.param[apply(print.param[, c("est", "se", "z", "stdyx")], 1L, function(y) all(is.na(y))), "rhs"] <- paste("    ", print.param[apply(print.param[, c("est", "se", "z", "stdyx")], 1L, function(y) all(is.na(y))), "rhs"])
      print.param[apply(print.param[, c("est", "se", "z", "stdyx")], 1L, function(y) any(!is.na(y))), "rhs"] <- paste("      ", print.param[apply(print.param[, c("est", "se", "z", "stdyx")], 1L, function(y) any(!is.na(y))), "rhs"])

      #### Convert NA into "" ####
      print.param[apply(print.param[, c("est", "se", "z", "stdyx")], 1L, function(y) all(is.na(y))), c("est", "se", "z", "pvalue", "stdyx")] <- ""
      print.param[apply(print.param[, c("z", "pvalue")], 1L, function(y) all( is.na(y) | misty::chr.trim(y) == "NA"  ) ), c("z", "pvalue")] <- ""
      print.param[which(is.na(print.param$stdyx)), "stdyx"] <- ""

      #### Column names ####
      print.param <- rbind(c(if (isTRUE(!x$args$long)) { "" }, "", "", "", "", "", "Estimate", "Std.Err", "z-value", "P(>|z|)", "StdYX"), print.param)

      #### Justify ####
      print.param[, "rhs"] <- format(print.param[, "rhs", drop = FALSE], justify = "left")

      if (isTRUE(!x$args$long)) {

        print.param[, -c(1L:5L)] <- apply(print.param[, -c(1L:5L), drop = FALSE], 2L, function(y) format(y, justify = "right"))

      } else {

        print.param[, -c(1L:4L)] <- apply(print.param[, -c(1L:4L), drop = FALSE], 2L, function(y) format(y, justify = "right"))

      }

      #...................
      #### Print ####

      ##### Between-group measurement invariance ####
      if (isTRUE(!x$args$long)) {

        for (i in misty::chr.omit(unique(print.param$group))) {

          cat(paste0("\n   Group: ", i, "\n\n"))

          ##### Factor loadings
          if (isTRUE(any(print.param[print.param$group == i, "param"] %in% "latent variable"))) {

            cat("    Factor Loadings\n")

            # Heading
            write.table(print.param[1L, c("rhs", "label", "est", "se", "z", "pvalue", "stdyx")], quote = FALSE, row.names = FALSE, col.names = FALSE)

            print.lv <- print.param[print.param$group == i & print.param$param == "latent variable", ]

            for (j in unique(print.lv$lhs)) {

              write.table(print.lv[print.lv$lhs == j, 5L:11L], quote = FALSE, row.names = FALSE, col.names = FALSE)

            }

          }

          ##### Latent variable covariances
          if (isTRUE(any(print.param$param %in% "latent variable covariance"))) {

            cat("\n    Latent Variable Covariances\n")

            print.lv.cov <- print.param[print.param$group == i & print.param$param == "latent variable covariance", ]

            for (j in unique(print.lv.cov$lhs)) {

              write.table(print.lv.cov[print.lv.cov$lhs == j, c("rhs", "label", "est", "se", "z", "pvalue", "stdyx")], quote = FALSE, row.names = FALSE, col.names = FALSE)

            }

          }

          ##### Residual covariances
          if (isTRUE(any(print.param$param %in% "residual covariance"))) {

            # Heading latent variables
            if (isTRUE(any(print.param$param %in% "latent variable"))) {

              cat("\n    Residual Covariances\n")

              # Heading no latent variables
            } else {

              cat("\n    Covariances\n")

              write.table(print.param[1L, c("rhs", "label", "est", "se", "z", "pvalue", "stdyx")], quote = FALSE, row.names = FALSE, col.names = FALSE)

            }

            print.res.cov <- print.param[print.param$group == i & print.param$param == "residual covariance", ]

            for (j in unique(print.res.cov$lhs)) {

              write.table(print.res.cov[print.res.cov$lhs == j, c("rhs", "label", "est", "se", "z", "pvalue", "stdyx")], quote = FALSE, row.names = FALSE, col.names = FALSE)

            }

          }

          ###
          # Latent mean
          if (isTRUE(any(print.param$param %in% "latent mean"))) {

            cat("\n    Latent Means\n")

            print.mean <- print.param[print.param$group == i & print.param$param == "latent mean", ]

            print.mean[grep("NA", print.mean[, "z"]), c("se", "z", "pvalue", "stdyx")] <- ""

            write.table(print.mean[, c("rhs", "label", "est", "se", "z", "pvalue", "stdyx")], quote = FALSE, row.names = FALSE, col.names = FALSE)

          }

          ##### Latent variance
          if (isTRUE(any(print.param$param %in% "latent variance"))) {

            cat("\n    Latent Variances\n")

            print.var <- print.param[print.param$group == i & print.param$param == "latent variance", ]

            write.table(print.var[, c("rhs", "label", "est", "se", "z", "pvalue", "stdyx")], quote = FALSE, row.names = FALSE, col.names = FALSE)

          }

          ##### Intercepts
          if (isTRUE(any(print.param$param %in% "intercept"))) {

            cat("\n    Intercepts\n")

            print.inter <- print.param[print.param$group == i & print.param$param == "intercept", ]

            write.table(print.inter[, c("rhs", "label", "est", "se", "z", "pvalue", "stdyx")], quote = FALSE, row.names = FALSE, col.names = FALSE)

          }

          ##### Residual variance
          if (isTRUE(any(print.param$param %in% "residual variance"))) {

            cat("\n    Residual Variances\n")

            print.resid <- print.param[print.param$group == i & print.param$param == "residual variance", ]

            write.table(print.resid[, c("rhs", "label", "est", "se", "z", "pvalue", "stdyx")], quote = FALSE, row.names = FALSE, col.names = FALSE)

          }

        }

        ##### Longitudinal measurement invariance ####
      } else {

        ##### Factor loadings
        if (isTRUE(any(print.param[, "param"] %in% "latent variable"))) {

          cat("   Factor Loadings\n")

          # Heading
          write.table(print.param[1L, c("rhs", "label", "est", "se", "z", "pvalue", "stdyx")], quote = FALSE, row.names = FALSE, col.names = FALSE)

          print.lv <- print.param[print.param$param == "latent variable", ]

          for (i in unique(print.lv$lhs)) {

            write.table(print.lv[print.lv$lhs == i, c("rhs", "label", "est", "se", "z", "pvalue", "stdyx")], quote = FALSE, row.names = FALSE, col.names = FALSE)

          }

        }

        ##### Latent variable covariances
        if (isTRUE(any(print.param$param %in% "latent variable covariance"))) {

          cat("\n   Latent Variable Covariances\n")

          print.lv.cov <- print.param[print.param$param == "latent variable covariance", ]

          for (i in unique(print.lv.cov$lhs)) {

            write.table(print.lv.cov[print.lv.cov$lhs == i, c("rhs", "label", "est", "se", "z", "pvalue", "stdyx")], quote = FALSE, row.names = FALSE, col.names = FALSE)

          }

        }

        ##### Residual covariances
        if (isTRUE(any(print.param$param %in% "residual covariance"))) {

          # Heading latent variables
          if (isTRUE(any(print.param$param %in% "latent variable"))) {

            cat("\n   Residual Covariances\n")

            # Heading no latent variables
          } else {

            cat("\n   Covariances\n")

            write.table(print.param[1L, c("rhs", "label", "est", "se", "z", "pvalue", "stdyx")], quote = FALSE, row.names = FALSE, col.names = FALSE)

          }

          print.res.cov <- print.param[print.param$param == "residual covariance", ]

          for (i in unique(print.res.cov$lhs)) {

            write.table(print.res.cov[print.res.cov$lhs == i, c("rhs", "label", "est", "se", "z", "pvalue", "stdyx")], quote = FALSE, row.names = FALSE, col.names = FALSE)

          }

        }

        ###
        # Latent mean
        if (isTRUE(any(print.param$param %in% "latent mean"))) {

          cat("\n    Latent Means\n")

          print.mean <- print.param[print.param$param == "latent mean", ]

          print.mean[misty::chr.trim(print.mean[, "z"]) == "", c("se", "z", "pvalue", "stdyx")] <- ""

          print.mean <- print.mean[match(unique(print.param[print.param$param == "latent variable", "lhs"]), print.mean$lhs), ]

          write.table(print.mean[, c("rhs", "label", "est", "se", "z", "pvalue", "stdyx")], quote = FALSE, row.names = FALSE, col.names = FALSE)

        }

        ##### Latent variance
        if (isTRUE(any(print.param$param %in% "latent variance"))) {

          cat("\n   Latent Variances\n")

          print.var <- print.param[print.param$param == "latent variance", ]

          write.table(print.var[, c("rhs", "label", "est", "se", "z", "pvalue", "stdyx")], quote = FALSE, row.names = FALSE, col.names = FALSE)

        }


        ##### Intercepts
        if (isTRUE(any(print.param$param %in% "intercept"))) {

          cat("\n   Intercepts\n")

          print.inter <- print.param[print.param$param == "intercept", ]

          write.table(print.inter[,c("rhs", "label", "est", "se", "z", "pvalue", "stdyx")], quote = FALSE, row.names = FALSE, col.names = FALSE)

        }

        ##### Residual variance
        if (isTRUE(any(print.param$param %in% "residual variance"))) {

          cat("\n   Residual Variances\n")

          print.resid <- print.param[print.param$param == "residual variance", ]

          write.table(print.resid[,  c("rhs", "label", "est", "se", "z", "pvalue", "stdyx")], quote = FALSE, row.names = FALSE, col.names = FALSE)

        }

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Modification indices ####

    if (isTRUE("modind" %in% x$args$print)) {

      switch(x$args$invar,
             ### Configural invariance ####
             config = {

               cat("\n  Modification Indices: Configural Invariance Model\n\n")
               print.modind <- print.object$modind$config

               ### Metric invariance ####
             }, metric = {

               cat("\n  Modification Indices: Metric Invariance Model\n\n")
               print.modind <- print.object$modind$metric

               ### Scalar invariance ####
             }, scalar = {

               cat("\n  Modification Indices: Scalar Invariance Model\n\n")
               print.modind <- print.object$modind$scalar

               ### Strict invariance ####
             }, strict = {

               cat("\n  Modification Indices: Strict Invariance Model\n\n")
               print.modind <- print.object$modind$strict

             })

      #### Modification indices not available
      if (isTRUE(is.null(print.modind))) {

        cat("   Modification indices are not available.\n")

        #### Modification available
      } else {

        # Filter modification indices
        print.modind <- print.modind[which(print.modind[, "mi"] >= x$args$mod.minval), ]

        if (isTRUE(nrow(print.modind) == 0L)) {

          cat(paste0("   No modification indices above the minimum value ", round(x$args$mod.minval, digits = 2L), ".\n"))

          #### Modification indices
        } else {

          # Round
          print.modind[, c("mi", "epc", "stdyx")] <- lapply(print.modind[, c("mi", "epc", "stdyx")], function(y) ifelse(!is.na(y), formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

          # Column names
          print.modind <- if (isTRUE(!x$args$long)) { rbind(c("Group", "", "", "", "MI", "EPC", "StdYX"), print.modind) } else { rbind(c("", "", "", "MI", "EPC", "StdYX"), print.modind) }

          # Add blank spaces
          if (isTRUE(!x$args$long)) { print.modind[, 1L] <- paste("   ", print.modind[, 1L]) } else { print.modind[, 1L] <- paste("  ", print.modind[, 1L]) }
          print.modind[-1L, 1L] <- paste("", print.modind[-1L, 1L])

          # Justify
          if (isTRUE(!x$args$long)) {

            print.modind[, c("group", "op", "rhs")] <- format(print.modind[, c("group", "op", "rhs")], justify = "left")
            print.modind[, "lhs"] <- format(print.modind[, "lhs"], justify = "left")
            print.modind[, -c(1L:4L)] <- apply(print.modind[, -c(1L:4L), drop = FALSE], 2L, function(y) format(y, justify = "right"))

          } else {

            print.modind[, c("op", "rhs")] <- format(print.modind[, c("op", "rhs")], justify = "left")
            print.modind[, "lhs"] <- format(print.modind[, "lhs"], justify = "right")
            print.modind[, -c(1L:3L)] <- apply(print.modind[, -c(1L:3L), drop = FALSE], 2L, function(y) format(y, justify = "right"))

          }

          # Factor loadings
          print.modind.load <- print.modind[print.modind$op == "=~", ]

          if (isTRUE(!x$args$long & nrow(print.modind.load) != 0L)) { print.modind.load <- do.call(rbind, tapply(print.modind.load, print.modind.load$group, function(y) rbind(y, rep("", times = 7L)))) }

          if (isTRUE(nrow(print.modind.load) > 0L)) {

            cat("   Factor Loadings\n")

            # Print header
            write.table(print.modind[1L, ], quote = FALSE, row.names = FALSE, col.names = FALSE)
            write.table(print.modind.load, quote = FALSE, row.names = FALSE, col.names = FALSE)

          }

          if (isTRUE(x$args$long)) { cat("\n") }

          # Residual covariances
          print.modind.cov <- print.modind[print.modind$op == "~~", ]

          if (isTRUE(!x$args$long & nrow(print.modind.cov) != 0L)) { print.modind.cov <- do.call(rbind, tapply(print.modind.cov, print.modind.cov$group, function(y) rbind(y, rep("", times = 7L)))) }

          if (isTRUE(nrow(print.modind.cov) > 0L)) {

            cat("   Residual Covariances\n")

            # Print header
            write.table(print.modind[1L, ], quote = FALSE, row.names = FALSE, col.names = FALSE)

            write.table(print.modind.cov, quote = FALSE, row.names = FALSE, col.names = FALSE)

          }

          if (isTRUE(x$args$long)) { cat("\n") }

        }

      }

      #...................
      ### Univariate Score Tests ####

      #### Level of measurement invariance ####
      switch(x$args$invar,
             ### Configural invariance ####
             config = {

               print.score <- print.object$score$config

               ### Metric invariance ####
             }, metric = {

               print.score <- print.object$score$metric

               ### Scalar invariance ####
             }, scalar = {

               print.score <- print.object$score$scalar

               ### Strict invariance ####
             }, strict = {

               print.score <- print.object$score$strict

             })

      #### Remove p-value and df
      print.score <- print.score[, -which(names(print.score) %in% c("df", "pvalue"))]

      #### Score tests not available
      if (isTRUE(is.null(print.score))) {

        cat("   Modification indices for parameter constraints are not available.\n")

        #### Score tests available
      } else {

        cat("\n  Modification Indices for Parameter Constraints\n\n")

        # Filter score tests
        print.score <- print.score[which(print.score[, "mi"] >= x$args$mod.minval), ]

        ##### Round
        print.score[, c("mi", "lhs.epc", "rhs.epc", "lhs.stdyx", "rhs.stdyx")] <- lapply(print.score[, c("mi", "lhs.epc", "rhs.epc", "lhs.stdyx", "rhs.stdyx")], function(y) ifelse(!is.na(y), formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

        if (isTRUE(nrow(print.score) == 0L)) {

          cat(paste0("   No modification indices for parameter constraints above the minimum value ", round(x$args$mod.minval, digits = 2L), ".\n"))

          #### Score test
        } else {

          ##### Group
          if (isTRUE(!x$args$long)) {

            print.score$group <- apply(print.score[, c("group.lhs", "group.rhs")], 1L, paste, collapse = " vs ")
            print.score <- print.score[, c("label", "group", "lhs", "op", "rhs", "mi", "lhs.epc", "rhs.epc", "lhs.stdyx", "rhs.stdyx")]

          }

          ##### Column names
          print.score <- if (isTRUE(!x$args$long)) { rbind(c("Label", "Group", "lhs", "op", "rhs", "MI", "lhs EPC", "rhs EPC", "lhs StdYX", "rhs StdYX"), print.score) } else { rbind(c("Label", "lhs", "op", "rhs", "MI", "lhs EPC", "rhs EPC", "lhs StdYX", "rhs StdYX"), print.score) }

          ##### Add blank spaces
          print.score[, 1L] <- paste("  ", print.score[, 1L])

          if (isTRUE(!x$args$long)) {

            print.score[-1L, c(1L:2L)] <- apply(print.score[-1L, c(1L:2L)], 2L, function(y) paste0(" ", y))

          } else {

            print.score[-1L, 1L] <- paste0(" ", print.score[-1L, 1L])

          }

          ##### Justify
          if (isTRUE(!x$args$long)) {

            print.score[, c("label", "group")] <- format(print.score[, c("label", "group")], justify = "left")
            print.score[, c("lhs", "op", "rhs")] <- format(print.score[, c("lhs", "op", "rhs")], justify = "centre")
            print.score[, -c(1L:5L)] <- apply(print.score[, -c(1L:5L), drop = FALSE], 2L, function(y) format(y, justify = "right"))

          } else {

            print.score[, "label"] <- format(print.score[, "label"], justify = "left")
            print.score[, c("lhs", "op", "rhs")] <- format(print.score[, c("lhs", "op", "rhs")], justify = "centre")
            print.score[, -c(1L:4L)] <- apply(print.score[, -c(1L:4L), drop = FALSE], 2L, function(y) format(y, justify = "right"))

          }

          #...................
          ### Print ####

          write.table(print.score, quote = FALSE, row.names = FALSE, col.names = FALSE)

          # Note
          cat(paste0("\n   Note. Minimum value for printing modification indices is ", round(x$args$mod.minval, digits = 2L), "\n"))

        }

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Residual Correlation Matrix ####

    if (isTRUE("resid" %in% x$args$print)) {

      #...................
      ### Level of measurement invariance ####
      switch(x$args$invar,
             ### Configural invariance ####
             config = {

               cat("\n Residual Correlation Matrix: Configural Invariance Model\n\n")
               print.resid <- print.object$resid$config

               ### Metric invarianc e ####
             }, metric = {

               cat("\n  Residual Correlation Matrix: Metric Invariance Model\n\n")
               print.resid <- print.object$resid$metric

               ### Scalar invariance ####
             }, scalar = {

               cat("\n  Residual Correlation Matrix: Scalar Invariance Model\n\n")
               print.resid <- print.object$resid$scalar

               ### Strict invariance ####
             }, strict = {

               cat("\n  Residual Correlation Matrix: Strict Invariance Model\n\n")
               print.resid <- print.object$resid$strict

             })

      ### Residual correlation matrix not available
      if (isTRUE(is.null(unlist(print.resid)))) {

        cat("   Residual correlation matrix is not available.")

        ### Residual correlation matrix available
      } else {

        #### Numbers and unicode
        numbers <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
        unicode <- c("\U1D7CE", "\U1D7CF", "\U1D7D0", "\U1D7EF", "\U1D7F0", "\U1D7F1", "\U1D7F2", "\U1D7F3", "\U1D7F4", "\U1D7F5")

        #...................
        #### Between-group measurement invariance
        if (isTRUE(!x$args$long)) {

          #### Round
          print.resid <- lapply(print.resid, function(y) ifelse(!is.na(y), formatC(y, digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0")), NA))

          #### Lower triangular
          for (i in seq_along(print.resid)) { print.resid[[i]][upper.tri(print.resid[[i]])] <- "" }

          #### Correlation coefficients in boldface
          if (isTRUE(is.null(getOption("knitr.in.progress")) && x$args$resid.minval != 1L)) { for (i in seq_along(print.resid)) { print.resid[[i]][lower.tri(print.resid[[i]])][which(abs(x$result$resid[[x$args$invar]][[i]][lower.tri(x$result$resid[[x$args$invar]][[i]])]) > x$args$resid.minval)] <- na.omit(misty::chr.gsub(numbers, unicode, print.resid[[i]][lower.tri(print.resid[[i]])][which(abs(x$result$resid[[x$args$invar]][[i]][lower.tri(x$result$resid[[x$args$invar]][[i]])]) > x$args$resid.minval)])) } }

          #### Separate residual mean
          for (i in seq_along(print.resid)) { print.resid[[i]] <- rbind(print.resid[[i]][1L:(nrow(print.resid[[i]]) - 1L), ], rep("", times = ncol(print.resid[[i]])), Mean = print.resid[[i]][nrow(print.resid[[i]]), ]) }

          #### Column names
          print.resid <- lapply(print.resid, function(y) rbind(colnames(y), y))
          print.resid <- lapply(print.resid, function(y) cbind(rownames(y), y))

          #### Add blank spaces
          for (i in seq_along(print.resid)) { print.resid[[i]][, 1L] <- paste("   ", print.resid[[i]][, 1L]) }

          #### Justify
          for (i in seq_along(print.resid)) {

            print.resid[[i]][, 1L] <- format(print.resid[[i]][, 1L], justify = "left")
            print.resid[[i]][, -1L] <- format(print.resid[[i]][, -1L], justify = "right")

          }

          #...................
          ### Print ####
          for (i in seq_along(print.resid)) {

            if (isTRUE(!x$args$long)) { cat("   Group:", names(print.resid)[i], "\n") }

            write.table(print.resid[[i]], quote = FALSE, row.names = FALSE, col.names = FALSE)

            if (isTRUE(i != length(print.resid))) { cat("\n") }

          }

          #...................
          #### Longitudinal measurement invariance
        } else {

          #### Round
          print.resid <- apply(print.resid, 2L, function(y) ifelse(!is.na(y), formatC(y, digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0")), NA))

          #### Lower triangular
          print.resid[upper.tri(print.resid)] <- ""

          #### Correlation coefficients in boldface
          if (isTRUE(is.null(getOption("knitr.in.progress")) && x$args$resid.minval != 1L)) { print.resid[lower.tri(print.resid)][which(abs(x$result$resid[[x$args$invar]][lower.tri(x$result$resid[[x$args$invar]])]) > x$args$resid.minval)] <- na.omit(misty::chr.gsub(numbers, unicode, print.resid[lower.tri(print.resid)][which(abs(x$result$resid[[x$args$invar]][lower.tri(x$result$resid[[x$args$invar]])]) > x$args$resid.minval)])) }

          #### Separate residual mean
          print.resid <- rbind(print.resid[1L:(nrow(print.resid) - 1L), ], rep("", times = ncol(print.resid)), Mean = print.resid[nrow(print.resid), ])

          #### Column names
          print.resid <- rbind(colnames(print.resid), print.resid)
          print.resid <- cbind(rownames(print.resid), print.resid)

          #### Add blank spaces
          print.resid[, 1L] <- paste("  ", print.resid[, 1L])

          #### Justify
          print.resid[, 1L] <- format(print.resid[, 1L], justify = "left")
          print.resid[, -1L] <- format(print.resid[, -1L], justify = "right")

          #...................
          ### Print ####
          write.table(print.resid, quote = FALSE, row.names = FALSE, col.names = FALSE)

        }

        # Note
        if (isTRUE(x$args$resid.minval < 1L)) { cat(paste0("\n  Note. Minimum absolute value for highlighting correlation coefficients is ", round(x$args$resid.minval, digits = 2L), "\n")) }

      }

    }

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

        stop("Character strings in the argument 'print' do not all match with \"all\", \"omega\", or \"item\".", call. = FALSE)

      }

    }

    #----------------------------------------
    # Arguments

    # Print coefficient omega and/or item statistic
    if (isTRUE(length(print) == 1L && "all" %in% print)) { print <- c("omega", "item") }

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

      print.object$itemstat$pNA <- paste0(formatC(print.object$itemstat$pNA, digits = 2L, format = "f",
                                                  zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), "%")
      print.object$itemstat$m <- formatC(print.object$itemstat$m, digits = 2L, format = "f",
                                         zero.print = paste0("0.", paste(rep(0L, times = digits), collapse = "")))
      print.object$itemstat$sd <- formatC(print.object$itemstat$sd, digits = 2L, format = "f",
                                          zero.print = paste0("0.", paste(rep(0L, times = digits), collapse = "")))
      print.object$itemstat$min <- formatC(print.object$itemstat$min, digits = 2L, format = "f",
                                           zero.print = paste0("0.", paste(rep(0L, times = digits), collapse = "")))
      print.object$itemstat$max <- formatC(print.object$itemstat$max, digits = 2L, format = "f",
                                           zero.print = paste0("0.", paste(rep(0L, times = digits), collapse = "")))

      print.object$itemstat$std.ld <- formatC(print.object$itemstat$std.ld, digits = digits, format = "f",
                                              zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))
      print.object$itemstat$omega <- formatC(print.object$itemstat$omega, digits = digits, format = "f",
                                             zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

      print.object$itemstat <- rbind(c("Variable", "n", "nNA", "pNA", "M", "SD", "Min", "Max", "Std.Ld", "Omega"),
                                     print.object$itemstat)

      # Format
      print.object$itemstat[, 1L] <- format(paste(" ", print.object$itemstat[, 1L]), justify = "left")
      print.object$itemstat[, -1L] <- apply(print.object$itemstat[, -1L], 2L, function(y) format(y, justify = "right"))

      if (isTRUE("omega" %in% print)) { cat("\n") }

      if (isTRUE(x$args$type == "omega")) {

        cat(" Standardized Factor Loadings and Coefficient Omega if Item Deleted\n\n")

      } else if (isTRUE(x$args$type == "hierarch")) {

        cat(" Standardized Factor Loadings and Hierarchical Omega if Item Deleted\n\n")

      } else if (isTRUE(x$args$type == "categ")) {

        cat(" Standardized Factor Loadings and Categorical Omega if Item Deleted\n\n")

      }

      write.table(print.object$itemstat, quote = FALSE, row.names = FALSE, col.names = FALSE)

    }

  #_____________________________________________________________________________
  #
  # Mplus Object ---------------------------------------------------------------
  }, mplus = {

      cat("Please use the mplus.print function to print a \"mplus\" object.")

  #_____________________________________________________________________________
  #
  # Mplus Summary Measures, Convergence and Efficiency Diagnostics -------------
  }, mplus.bayes = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Round ####

    # digits
    print.round <- c("m", "med", "map", "sd", "mad", "skew", "kurt", "eti.low", "eti.upp", "hdi.low", "hdi.upp")
    print.object[, print.round] <- sapply(print.round, function(y) ifelse(!is.na(print.object[, y]), formatC(print.object[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

    # r.digits
    print.object[, "rhat"] <- ifelse(!is.na(print.object[, "rhat"]), formatC(print.object[, "rhat"], digits = r.digits, format = "f", zero.print = ifelse(r.digits > 0L, paste0("0.", paste(rep(0L, times = r.digits), collapse = "")), "0")), NA)

    # ess.digits
    print.object[, "b.ess"] <- ifelse(!is.na(print.object[, "b.ess"]), formatC(print.object[, "b.ess"], digits = ess.digits, format = "f", zero.print = ifelse(ess.digits > 0L, paste0("0.", paste(rep(0L, times = ess.digits), collapse = "")), "0")), NA)
    print.object[, "t.ess"] <- ifelse(!is.na(print.object[, "t.ess"]), formatC(print.object[, "t.ess"], digits = ess.digits, format = "f", zero.print = ifelse(ess.digits > 0L, paste0("0.", paste(rep(0L, times = ess.digits), collapse = "")), "0")), NA)

    # mcse.digits
    print.object[, "b.mcse"] <- ifelse(!is.na(print.object[, "b.mcse"]), formatC(print.object[, "b.mcse"], digits = mcse.digits, format = "f", zero.print = ifelse(mcse.digits > 0L, paste0("0.", paste(rep(0L, times = mcse.digits), collapse = "")), "0")), NA)
    print.object[, "t.mcse"] <- ifelse(!is.na(print.object[, "t.mcse"]), formatC(print.object[, "t.mcse"], digits = mcse.digits, format = "f", zero.print = ifelse(mcse.digits > 0L, paste0("0.", paste(rep(0L, times = mcse.digits), collapse = "")), "0")), NA)

    # p.digits
    print.object[, "pd"] <- ifelse(!is.na(print.object[, "pd"]), formatC(print.object[, "pd"], digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0")), NA)
    print.object[, "rope"] <- ifelse(!is.na(print.object[, "rope"]), formatC(print.object[, "rope"], digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0")), NA)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##  Row Names ####

    print.object <- rbind(c("Parameter", "M", "Med", "MAP", "SD", "MAD", "Skew", "Kurt", "ETI.Low", "ETI.Upp", "HDI.Low", "HDI.Upp", "R-hat", "B.ESS", "T.ESS", "B.MCSE", "T.MCSE", "pd", "ROPE"), print.object)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Select Statistical Measures and Add Parameters ####

    # Print statistics
    if (isTRUE("eti" %in% print)) { print <- c(print, c("eti.low", "eti.upp")) }
    if (isTRUE("hdi" %in% print)) { print <- c(print, c("hdi.low", "hdi.upp")) }

    # pd and ROPE
    if (isTRUE(x$args$pd)) { print <- c(print, "pd")}
    if (isTRUE(!is.null(x$args$rope))) { print <- c(print, "rope")}

    # Sort
    print <- intersect(c("m", "med", "map", "sd", "mad", "skew", "kurt", "eti.low", "eti.upp", "hdi.low", "hdi.upp", "rhat", "b.ess", "t.ess", "b.mcse", "t.mcse", "pd", "rope"), print)

    # Select
    print.object <- data.frame(parameter = print.object[, "param"], print.object[, print, drop = FALSE], stringsAsFactors = FALSE, check.names = FALSE)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Format ####

    # Justify left and right
    print.object[, 1L] <- format(print.object[, 1L, drop = FALSE], justify = "left")
    print.object[, -1L] <- apply(print.object[, -1L, drop = FALSE], 2L, function(y) format(y, justify = "right"))

    # Add blank space
    print.object[, "parameter"] <- c(paste0("  ", print.object[1L, "parameter"], collapse = ""), paste0("   ", print.object[-1L, "parameter"]))
    print.object[, "parameter"] <- format(c(print.object[1L, "parameter"], misty::chr.trim(print.object[-1L, "parameter"], side = "right")), justify = "left")

    # Replace NA
    print.object[, -1L] <- apply(print.object[, -1L], 2L, function(y) gsub("NA", "  ", y))


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Print Result Table ####

    # Header
    cat(" Summary Measures, Convergence and Efficiency Diagnostics\n\n")

    # Print table
    write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE)

    # R-hat
    if (isTRUE("rhat" %in% print)) {

      if (isTRUE(x$args$fold)) {

        cat("\n  Note. Maximum of Rank-Normalized (Folded-)Split R-hat")

      } else {

        if (isTRUE(x$args$rank)) {

          if (isTRUE(x$args$split)) {

            cat("\n  Note. Rank-Normalizsed Split R-hat")

          } else {

            cat("\n  Note. Rank-Normalized R-hat")

          }

        } else {

          if (isTRUE(x$args$split)) {

            cat("\n  Note. Traditional Split R-hat")

          } else {

            cat("\n  Note. Traditional R-hat")

          }

        }

      }

    }

    # ROPE
    if (isTRUE(!is.null(x$args$rope))) {

      if (isTRUE("rhat" %in% print)) {

        switch(x$args$alternative,
               two.sided = { cat(paste0("\n        Region of Practical Equivalence (ROPE): [", x$args$rope[1L], ", ", x$args$rope[2L], "]")) },
               less = { cat(paste0("\n        Region of Practical Equivalence (ROPE): [", x$args$rope[1L], ", Inf]")) },
               greater = { cat(paste0("\n        Region of Practical Equivalence (ROPE): [-Inf, ", x$args$rope[2L], "]"))} )

      } else {

        switch(x$args$alternative,
               two.sided = cat(paste0("\n    Note. Region of Practical Equivalence (ROPE): [", x$args$rope[1L], ", ", x$args$rope[2L], "]")),
               less = cat(paste0("\n    Note. Region of Practical Equivalence (ROPE): [", x$args$rope[1L], ", Inf]")),
               greater = cat(paste0("\n    Note. Region of Practical Equivalence (ROPE): [-Inf, ", x$args$rope[2L], "]")))

      }

    }

  #_____________________________________________________________________________
  #
  # Multilevel Confirmatory Factor Analysis ------------------------------------
  }, multilevel.cfa = {

    cat(" Multilevel Confirmatory Factor Analysis\n")

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## lavaan summary ####

    if (isTRUE("summary" %in% x$args$print)) {

      print.summary1 <- print.object$summary[c(1L:14L), ]
      print.summary2 <- print.object$summary[-c(1L:15L), ]

      #...................
      ### Format ####

      # Include spaces
      print.summary1[1L, 1L] <- paste0(" ", print.summary1[1L, 1L])
      print.summary1[-1L, 1L] <- paste0("  ", print.summary1[-1L, 1L])

      print.summary2[1L, 1L] <- paste0(" ", print.summary2[1L, 1L])
      print.summary2[-1L, 1L] <- paste0("  ", print.summary2[-1L, 1L])

      print.summary1[c(12L, 13L), 1L] <- paste0(" ", print.summary1[c(12L, 13L), 1L])

      # Justify left
      print.summary1[, 1L] <- format(print.summary1[, 1L], justify = "left")
      print.summary2[, 1L] <- format(print.summary2[, 1L], justify = "left")

      # Justify right
      print.summary1[, 2L] <- format(print.summary1[, 2L], justify = "right")
      print.summary2[, 2L] <- format(print.summary2[, 2L], justify = "right")

      # Add spaces
      print.summary2[1L, 1L] <- paste0(print.summary2[1L, 1L], paste0(rep(" ", times = nchar(paste0(print.summary1[1, c(1L, 2L)], collapse = " ")) - nchar(paste0(print.summary2[1, ], collapse = " "))), collapse = ""), collapse = "")

      # Justify left
      print.summary2[, 1L] <- format(print.summary2[, 1L], justify = "left")

      #...................
      ### Print ####

      print(print.summary1, col.names = FALSE, row.names = FALSE, quote = FALSE, right = TRUE, max = 99999L)
      print(print.summary2, col.names = FALSE, row.names = FALSE, quote = FALSE, right = TRUE, max = 99999L)

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Covariance coverage ####

    if (isTRUE("coverage" %in% x$args$print)) {

      cat("\n  Covariance Coverage of Data\n\n")

      print.coverage <- print.object$coverage

      #...................
      ### Lower triangular ####

      print.coverage[upper.tri(print.coverage)] <- ""

      #...................
      ### Format ####

      print.coverage <- apply(print.coverage, 2L, function(y) ifelse(!is.na(as.numeric(y)), formatC(as.numeric(y), digits = p.digits - 1L, format = "f", zero.print = ifelse(2L > 0L, paste0("0.", paste(rep(0L, times = 2L), collapse = "")), "0")), ""))

      # Row names
      row.names(print.coverage) <- paste0("   ", colnames(print.coverage))

      #...................
      ### Print ####

      print(print.coverage, row.names = FALSE, quote = FALSE, right = TRUE, max = 99999L)

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Sample Statistics ####

    if (isTRUE("descript" %in% x$args$print)) {

      cat("\n  Univariate Sample Statistics\n\n")

      print.itemstat <- print.object$descript

      #...................
      ### Format ####

      # Variables to round
      print.round <- c("m", "sd", "min", "max", "skew", "kurt", "ICC")

      print.itemstat[, print.round] <- sapply(print.round, function(y) ifelse(!is.na(print.itemstat[, y]), formatC(print.itemstat[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))
      print.itemstat[, "pNA"] <- formatC(print.itemstat[, "pNA"], digits = digits - 1L, format = "f", zero.print = ifelse(digits - 1L > 0L, paste0("0.", paste(rep(0L, times = digits - 1L), collapse = "")), "0"))

      sapply(print.round, function(y) ifelse(!is.na(print.itemstat[, y]), formatC(print.itemstat[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

      # Percentages
      print.itemstat[, "pNA"] <- paste0(print.itemstat[, "pNA"], "%")

      # Column names
      print.itemstat <- rbind(c("Variable", "n", "nNA", "pNA", "M", "SD", "Min", "Max",  "Skew", "Kurt", "ICC(1)"), print.itemstat)

      # Justify left and right
      print.itemstat[, 1L] <- format(print.itemstat[, 1L, drop = FALSE], justify = "left")
      print.itemstat[, -1L] <- apply(print.itemstat[, -1L, drop = FALSE], 2L, function(y) format(y, justify = "right"))

      # Add blank space
      print.itemstat[, "variable"] <- c(paste0("   ", print.itemstat[1L, "variable"], collapse = ""), paste0("    ", print.itemstat[-1L, "variable"]))
      print.itemstat[, "variable"] <- format(c(print.itemstat[1L, "variable"], misty::chr.trim(print.itemstat[-1L, "variable"], side = "right")), justify = "left")

      #...................
      ### Print ####

      write.table(print.itemstat, quote = FALSE, row.names = FALSE, col.names = FALSE)

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Model fit ####

    if (isTRUE("fit" %in% x$args$print)) {

      cat("\n  Model Fit Information\n\n")

      print.fit <- print.object$fit

      #...................
      ### Simultaneous model fit information only ####
      if (isTRUE(nrow(print.fit) <= 30L)) {

        #...................
        ### Round ####

        #### Maximum likelihood
        if (isTRUE(ncol(print.fit) == 2L)) {

          # Fit indices
          print.fit[-c(2L:3L, 11L:13L, 23L), -1L] <- sapply(print.fit[-c(2L:3L, 11L:13L, 23L), -1L], function(y) ifelse(!is.na(y), formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

          # Information criteria
          print.fit[c(2L:3L, 11L), -1L] <- sapply(print.fit[c(2L:3L, 11L), -1L], function(y) ifelse(!is.na(y), formatC(as.numeric(y), digits = digits - 1L, format = "f", zero.print = ifelse(digits - 1L > 0L, paste0("0.", paste(rep(0L, times = digits - 1L), collapse = "")), "0")), NA))

          # p.digits
          print.fit[c(13L, 23L), -1L] <- sapply(print.fit[c(13L, 23L), -1L], function(y) ifelse(!is.na(y), formatC(as.numeric(y), digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0")), NA))

          #### Robust maximum likelihood
        } else {

          # Fit indices
          print.fit[-c(2L:10L, 13L:15L, 26L), -1L] <- sapply(print.fit[-c(2L:10L, 13L:15L, 26L), -1L], function(y) ifelse(!is.na(y), formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

          # Information criteria
          print.fit[c(2L:10L, 13L), -1L] <- sapply(print.fit[c(2L:10L, 13L), -1L], function(y) ifelse(!is.na(y), formatC(as.numeric(y), digits = digits - 1L, format = "f", zero.print = ifelse(digits - 1L > 0L, paste0("0.", paste(rep(0L, times = digits - 1L), collapse = "")), "0")), NA))

          # p.digits
          print.fit[c(15L, 26L), -1L] <- sapply(print.fit[c(15L, 26L), -1L], function(y) ifelse(!is.na(y), formatC(as.numeric(y), digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0")), NA))

        }

        #...................
        ### Add labels ####

        #### Maximum likelihood
        if (isTRUE(ncol(print.fit) == 2L)) {

          print.fit <- rbind(print.fit[1L:9L, ], c("", "Standard"),
                             print.fit[10L:14L, ], c("", "Standard"),
                             print.fit[15L:18L, ], c("", "Standard"),
                             print.fit[19L:27L, ],
                             make.row.names = FALSE)

          #### Robust maximum likelihood
        } else {

          print.fit <- rbind(print.fit[1L:11L, ], c("", "Standard", "Scaled", ""),
                             print.fit[12L:17L, ], c("", "Standard", "Scaled", "Robust"),
                             print.fit[18L:21L, ], c("", "Standard", "Scaled", "Robust"),
                             print.fit[22L:30L, ],
                             make.row.names = FALSE)

        }

        #...................
        ### Replace NA with "" ####

        print.fit <- unname(misty::na.as(print.fit, na = ""))

        #...................
        ### Add blank space ####

        print.fit[, 1L] <- paste0("  ", print.fit[, 1L])

        #### Maximum likelihood
        if (isTRUE(ncol(print.fit) == 2L)) {

          print.fit[c(2L:3L, 6L:8L, 12L:14L, 18L:19L, 23L:30L), 1L] <- paste("", unlist(print.fit[c(2L:3L, 6L:8L, 12L:14L, 18L:19L, 23L:30L), 1L]))

          # Within/Between
          print.fit[c(29L:30L), 1L] <- paste("", unlist(print.fit[c(29L:30L), 1L]))

          # RMSEA 95% CI - p-values
          print.fit[c(24L:26L), 1L] <- paste("", unlist(print.fit[c(24L:26L), 1L]))

          #### Robust maximum likelihood
        } else {

          print.fit[c(2L:5L, 8L:10L, 14L:17L, 21L:22L, 26L:33L), 1L] <- paste("", unlist(print.fit[c(2L:5L, 8L:10L, 14L:17L, 21L:22L, 26L:33L), 1L]))

          # Within/Between
          print.fit[c(32L:33L), 1L] <- paste("", unlist(print.fit[c(32L:33L), 1L]))

          # RMSEA 95% CI - p-values
          print.fit[c(27L:29L), 1L] <- paste("", unlist(print.fit[c(27L:29L), 1L]))

        }

        #...................
        ### Justify left and right ####

        print.fit[, 1L] <- format(print.fit[, 1L, drop = FALSE], justify = "left")

        print.fit[, -1L] <- apply(print.fit[, -1L, drop = FALSE], 2L, function(y) format(y, justify = "right"))

      #...................
      ### Simultaneous and level-specific model fit information  ####
      } else {

        #...................
        ### Round ####

        #### Maximum likelihood
        if (isTRUE(ncol(print.fit) == 2L)) {

          # digits
          print.fit[-c(17L:19L, 41L:43L), -1L] <- sapply(print.fit[-c(17L:19L, 41L:43L), -1], function(y) ifelse(!is.na(y), formatC(y, digits = digits, format = "f",
                                                                                                                                    zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

          # p.digits
          print.fit[c(17L:19L, 41L:43L), -1L] <- sapply(print.fit[c(17L:19L, 41L:43L), -1L], function(y) ifelse(!is.na(y), formatC(as.numeric(y), digits = p.digits, format = "f",
                                                                                                                                   zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0")), NA))
          #### Robust maximum likelihood
        } else {

          # digits
          print.fit[-c(19L:21L, 46L:48L), -1L] <- sapply(print.fit[-c(19L:21L, 46L:48L), -1L], function(y) ifelse(!is.na(y), formatC(y, digits = digits, format = "f",
                                                                                                                                     zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

          # p.digits
          print.fit[c(19L:21L, 46L:48L), -1L] <- sapply(print.fit[c(19L:21L, 46L:48L), -1L], function(y) ifelse(!is.na(y), formatC(as.numeric(y), digits = p.digits, format = "f",
                                                                                                                                   zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0")), NA))

        }

        #...................
        ### Add labels ####

        #### Maximum likelihood
        if (isTRUE(ncol(print.fit) == 2L)) {

          print.fit <- rbind(print.fit[1L:9L, ], c("", "Standard"),
                             print.fit[10L:20L, ], c("", "Standard"),
                             print.fit[21L:29L, ], c("", "Standard"),
                             print.fit[30L:47L, ],
                             make.row.names = FALSE)

          #### Robust maximum likelihood
        } else {

          print.fit <- rbind(print.fit[1L:11L, ], c("", "Standard", "Scaled", ""),
                             print.fit[12L:25L, ], c("", "Standard", "Scaled", "Robust"),
                             print.fit[26L:34L, ], c("", "Standard", "Scaled", "Robust"),
                             print.fit[35L:52L, ],
                             make.row.names = FALSE)

        }

        #...................
        ### Replace NA with "" ####

        print.fit <- unname(misty::na.as(print.fit, na = ""))

        #...................
        ### Add blank space ####

        print.fit[, 1L] <- paste0("  ", print.fit[, 1L])

        #### Maximum likelihood
        if (isTRUE(ncol(print.fit) == 2L)) {

          print.fit[c(2L:3L, 6L:8L, 12L:20L, 24L:30L, 34L:50L), 1L] <- paste("", unlist(print.fit[c(2L:3L, 6L:8L, 12L:20L, 24L:30L, 34L:50L), 1L]))

          # Within/Between
          print.fit[c(13L, 14L, 16L, 17L, 19L, 20L, 25L, 26L, 29L, 30L, 35L, 36L, 39L, 40L, 42L, 43L, 45L, 46L, 49L, 50L), 1L] <- paste("", unlist(print.fit[c(13L, 14L, 16L, 17L, 19L, 20L, 25L, 26L, 29L, 30L, 35L, 36L, 39L, 40L, 42L, 43L, 45L, 46L, 49L, 50L), 1L]))

          # RMSEA 95% CI - p-values
          print.fit[c(38L:46), 1L] <- paste("", unlist(print.fit[c(38L:46), 1L]))

          #### Robust maximum likelihood
        } else {

          print.fit[c(2L:5L, 8L:10L, 14L:25L, 29L:35L, 39L:55L), 1L] <- paste("", unlist(print.fit[c(2L:5L, 8L:10L, 14L:25L, 29L:35L, 39L:55L), 1L]))

          # Within/Between
          print.fit[c(15L, 16L, 18L, 19L, 21L, 22L, 24L, 25L, 30L, 31L, 34L, 35L, 40L, 41L, 44L, 45L, 47L, 48L, 50L, 51L, 54L, 55L), 1L] <- paste("", unlist(print.fit[c(15L, 16L, 18L, 19L, 21L, 22L, 24L, 25L, 30L, 31L, 34L, 35L, 40L, 41L, 44L, 45L, 47L, 48L, 50L, 51L, 54L, 55L), 1L]))

          # RMSEA 95% CI - p-values
          print.fit[c(43L:51L), 1L] <- paste("", unlist(print.fit[c(43L:51L), 1L]))

        }

        #...................
        ### Justify left and right ####

        print.fit[, 1L] <- format(print.fit[, 1L, drop = FALSE], justify = "left")

        print.fit[, -1L] <- apply(print.fit[, -1L, drop = FALSE], 2L, function(y) format(y, justify = "right"))

      }

      #...................
      ### Print ####

      write.table(print.fit, quote = FALSE, row.names = FALSE, col.names = FALSE)

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Parameter estimates ####

    if (isTRUE("est" %in% x$args$print)) {

      #...................
      ### Within Level ####

      cat("\n  Model Results: Within Level\n\n")

      print.param.w <- print.object$param$within

      #### Round ####

      # digits
      print.param.w[, -c(1L:4L, 8L)] <- lapply(print.param.w[, -c(1L:4L, 8L)], function(y) ifelse(!is.na(y), formatC(y, digits = digits, format = "f",
                                                                                                                     zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

      # p.digits
      print.param.w[, "pvalue"] <- formatC(as.numeric(print.param.w[, "pvalue"]), digits = p.digits, format = "f",
                                           zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

      #### Add blank spaces ####
      print.param.w[apply(print.param.w[, c("est", "se", "z", "stdyx")], 1L, function(y) all(is.na(y))), "rhs"] <- paste("   ", print.param.w[apply(print.param.w[, c("est", "se", "z", "stdyx")], 1L, function(y) all(is.na(y))), "rhs"])
      print.param.w[apply(print.param.w[, c("est", "se", "z", "stdyx")], 1L, function(y) any(!is.na(y))), "rhs"] <- paste("     ", print.param.w[apply(print.param.w[, c("est", "se", "z", "stdyx")], 1L, function(y) any(!is.na(y))), "rhs"])

      #### Convert NA into "" ####
      print.param.w[apply(print.param.w[, c("est", "se", "z", "stdyx")], 1L, function(y) all(is.na(y))), c("est", "se", "z", "pvalue", "stdyx")] <- ""
      print.param.w[apply(print.param.w[, c("se", "z")], 1L, function(y) all(is.na(y))), c("se", "z", "pvalue", "stdyx")] <- ""
      print.param.w[which(is.na(print.param.w$stdyx)), "stdyx"] <- ""

      #### Column names ####
      print.param.w <- rbind(c("", "", "", "", "Estimate", "Std.Err", "z-value", "P(>|z|)", "StdYX"), print.param.w)

      #### Justify ####
      print.param.w[, "rhs"] <- format(print.param.w[, "rhs", drop = FALSE], justify = "left")
      print.param.w[, -c(1L:4L)] <- apply(print.param.w[, -c(1L:4L), drop = FALSE], 2L, function(y) format(y, justify = "right"))

      #...................
      #### Print ####

      ##### Factor loadings
      if (isTRUE(any(print.param.w$param %in% "latent variable"))) {

        cat("   Factor Loadings\n")

        # Heading
        write.table(print.param.w[1L, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

        print.lv <- print.param.w[print.param.w$param == "latent variable", ]

        for (i in unique(print.lv$lhs)) {

          pos.NA <- grep("NA", print.lv[print.lv$lhs == i, "z"])

          if (isTRUE(length(pos.NA) > 0L)) {

            print.lv[print.lv$lhs == i, ][pos.NA, "se"] <- paste(rep("", times = unique(nchar(print.lv[print.lv$lhs == i, ][pos.NA, "se"])) + 1L), collapse = " ")
            print.lv[print.lv$lhs == i, ][pos.NA, "z"] <- paste(rep("", times = unique(nchar(print.lv[print.lv$lhs == i, ][pos.NA, "z"])) + 1L), collapse = " ")
            print.lv[print.lv$lhs == i, ][pos.NA, "pvalue"] <- paste(rep("", times = unique(nchar(print.lv[print.lv$lhs == i, ][pos.NA, "pvalue"])) + 1L), collapse = " ")

          }

          write.table(print.lv[print.lv$lhs == i, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

        }

      }

      ##### Latent variable covariances
      if (isTRUE(any(print.param.w$param %in% "latent variable covariance"))) {

        cat("\n   Latent Variable Covariances\n")

        print.lv.cov <- print.param.w[print.param.w$param == "latent variable covariance", ]

        for (i in unique(print.lv.cov$lhs)) {

          write.table(print.lv.cov[print.lv.cov$lhs == i, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

        }

      }

      ##### Residual covariances
      if (isTRUE(any(print.param.w$param %in% "residual covariance"))) {

        # Heading latent variables
        if (isTRUE(any(print.param.w$param %in% "latent variable"))) {

          cat("\n   Residual Covariances\n")

          # Heading no latent variables
        } else {

          cat("   Covariances\n")

          write.table(print.param.w[1L, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

        }

        print.res.cov <- print.param.w[print.param.w$param == "residual covariance", ]

        for (i in unique(print.res.cov$lhs)) {

          write.table(print.res.cov[print.res.cov$lhs == i, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

        }

      }

      ##### Latent variance
      if (isTRUE(any(print.param.w$param %in% "latent variance"))) {

        cat("\n   Latent Variances\n")

        print.var <- print.param.w[print.param.w$param == "latent variance", ]

        print.var[grep("NA", print.var[, "z"]), c("se", "z", "pvalue", "stdyx")] <- ""

        write.table(print.var[, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

      }

      ##### Residual variance
      if (isTRUE(any(print.param.w$param %in% "residual variance"))) {

        # Heading latent variables
        if (isTRUE(any(print.param.w$param %in% "latent variable"))) {

          cat("\n   Residual Variances\n")

        } else {

          cat("\n   Variances\n")

        }

        print.resid <- print.param.w[print.param.w$param == "residual variance", ]

        pos.NA <- grep("NA", print.resid[, "z"])

        if (isTRUE(length(pos.NA) >  0L)) {

          print.resid[pos.NA, "se"] <- paste(rep("", times = unique(nchar(print.resid[pos.NA, "se"])) + 1L), collapse = " ")
          print.resid[pos.NA, "z"] <- paste(rep("", times = unique(nchar(print.resid[pos.NA, "z"])) + 1L), collapse = " ")
          print.resid[pos.NA, "pvalue"] <- paste(rep("", times = unique(nchar(print.resid[pos.NA, "pvalue"])) + 1L), collapse = " ")

        }

        write.table(print.resid[, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

      }

      #...................
      ### Between Level ####

      cat("\n  Model Results: Between Level\n\n")

      print.param.b <- print.object$param$between

      #...................
      #### Round ####

      # digits
      print.param.b[, -c(1L:4L, 8L)] <- lapply(print.param.b[, -c(1L:4L, 8L)], function(y) ifelse(!is.na(y), formatC(y, digits = digits, format = "f",
                                                                                                                     zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

      # p.digits
      print.param.b[, "pvalue"] <- formatC(as.numeric(print.param.b[, "pvalue"]), digits = p.digits, format = "f",
                                           zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

      #...................
      #### Add blank spaces ####
      print.param.b[apply(print.param.b[, c("est", "se", "z", "stdyx")], 1L, function(y) all(is.na(y))), "rhs"] <- paste("   ", print.param.b[apply(print.param.b[, c("est", "se", "z", "stdyx")], 1L, function(y) all(is.na(y))), "rhs"])
      print.param.b[apply(print.param.b[, c("est", "se", "z", "stdyx")], 1L, function(y) any(!is.na(y))), "rhs"] <- paste("     ", print.param.b[apply(print.param.b[, c("est", "se", "z", "stdyx")], 1L, function(y) any(!is.na(y))), "rhs"])

      #...................
      #### Convert NA into "" ####
      print.param.b[apply(print.param.b[, c("est", "se", "z", "stdyx")], 1L, function(y) all(is.na(y))), c("est", "se", "z", "pvalue", "stdyx")] <- ""
      print.param.b[apply(print.param.b[, c("se", "z")], 1L, function(y) all(is.na(y))), c("se", "z", "pvalue", "stdyx")] <- ""
      print.param.b[which(is.na(print.param.b$stdyx)), "stdyx"] <- ""

      #...................
      #### Column names ####
      print.param.b <- rbind(c("", "", "", "", "Estimate", "Std.Err", "z-value", "P(>|z|)", "StdYX"), print.param.b)

      #...................
      #### Justify ####
      print.param.b[, "rhs"] <- format(print.param.b[, "rhs", drop = FALSE], justify = "left")
      print.param.b[, -c(1L:4L)] <- apply(print.param.b[, -c(1L:4L), drop = FALSE], 2L, function(y) format(y, justify = "right"))

      #...................
      #### Print ####

      ##### Factor loadings
      if (isTRUE(any(print.param.b$param %in% "latent variable"))) {

        cat("   Factor Loadings\n")

        # Heading
        write.table(print.param.b[1L, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

        print.lv <- print.param.b[print.param.b$param == "latent variable", ]

        for (i in unique(print.lv$lhs)) {

          pos.NA <- grep("NA", print.lv[print.lv$lhs == i, "z"])

          if (isTRUE(length(pos.NA) > 0L)) {

            print.lv[print.lv$lhs == i, ][pos.NA, "se"] <- paste(rep("", times = unique(nchar(print.lv[print.lv$lhs == i, ][pos.NA, "se"])) + 1L), collapse = " ")
            print.lv[print.lv$lhs == i, ][pos.NA, "z"] <- paste(rep("", times = unique(nchar(print.lv[print.lv$lhs == i, ][pos.NA, "z"])) + 1L), collapse = " ")
            print.lv[print.lv$lhs == i, ][pos.NA, "pvalue"] <- paste(rep("", times = unique(nchar(print.lv[print.lv$lhs == i, ][pos.NA, "pvalue"])) + 1L), collapse = " ")

          }

          write.table(print.lv[print.lv$lhs == i, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

        }

      }

      ##### Latent variable covariances
      if (isTRUE(any(print.param.b$param %in% "latent variable covariance"))) {

        cat("\n   Latent Variable Covariances\n")

        print.lv.cov <- print.param.b[print.param.b$param == "latent variable covariance", ]

        for (i in unique(print.lv.cov$lhs)) {

          write.table(print.lv.cov[print.lv.cov$lhs == i, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

        }

      }

      ##### (Residual) Covariances
      if (isTRUE(any(print.param.b$param %in% "residual covariance"))) {

        # Heading latent variables
        if (isTRUE(any(print.param.b$param %in% "latent variable"))) {

          cat("\n   Residual Covariances\n")

          # Heading no latent variables
        } else {

          cat("   Covariances\n")

          write.table(print.param.b[1L, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

        }

        print.res.cov <- print.param.b[print.param.b$param == "residual covariance", ]

        for (i in unique(print.res.cov$lhs)) {

          write.table(print.res.cov[print.res.cov$lhs == i, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

        }

      }

      ##### Latent mean
      if (isTRUE(any(print.param.b$param %in% "latent mean"))) {

        cat("\n    Latent Means\n")

        print.mean <- print.param.b[print.param.b$param == "latent mean", ]

        print.mean[grep("NA", print.mean[, "z"]), c("se", "z", "pvalue", "stdyx")] <- ""

        write.table(print.mean[, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

      }

      ##### Latent variance
      if (isTRUE(any(print.param.b$param %in% "latent variance"))) {

        cat("\n   Latent Variances\n")

        print.var <- print.param.b[print.param.b$param == "latent variance", ]

        print.var[grep("NA", print.var[, "z"]), c("se", "z", "pvalue", "stdyx")] <- ""

        write.table(print.var[, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

      }

      ##### Intercepts
      if (isTRUE(any(print.param.b$param %in% "intercept"))) {

        cat("\n   Intercepts\n")

        print.inter <- print.param.b[print.param.b$param == "intercept", ]

        print.inter[grep("NA", print.inter[, "z"]), c("se", "z", "pvalue", "stdyx")] <- ""

        write.table(print.inter[, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

      }

      ##### (Residual) Variance
      if (isTRUE(any(print.param.b$param %in% "residual variance"))) {

        # Heading latent variables
        if (isTRUE(any(print.param.b$param %in% "latent variable"))) {

          cat("\n   Residual Variances\n")

        } else {

          cat("\n   Variances\n")

        }

        print.resid <- print.param.b[print.param.b$param == "residual variance", ]

        pos.NA <- grep("NA", print.resid[, "z"])

        if (isTRUE(length(pos.NA) >  0L)) {

          print.resid[pos.NA, "se"] <- paste(rep("", times = unique(nchar(print.resid[pos.NA, "se"])) + 1L), collapse = " ")
          print.resid[pos.NA, "z"] <- paste(rep("", times = unique(nchar(print.resid[pos.NA, "z"])) + 1L), collapse = " ")
          print.resid[pos.NA, "pvalue"] <- paste(rep("", times = unique(nchar(print.resid[pos.NA, "pvalue"])) + 1L), collapse = " ")

        }

        write.table(print.resid[, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Modification indices ####

    if (isTRUE("modind" %in% x$args$print)) {

      #...................
      ### Within Level ####

      print.modind.w <- print.object$modind$within
      print.modind.b <- print.object$modind$between

      #### Modification indices not available
      if (isTRUE(is.null(print.modind.w) && is.null(print.modind.b))) {

        cat("\n   Modification indices are not available.")

        #### Modification available
      } else {

        cat("\n  Modification Indices: Within Level\n")

        # Filter modification indices
        print.modind.w <- print.modind.w[which(print.modind.w[, "mi"] >= x$args$mod.minval), ]

        if (isTRUE(nrow(print.modind.w) == 0L)) {

          cat(paste0("\n   No modification indices above the minimum value ", round(x$args$mod.minval, digits = 2L), " at the Within Level.\n"))

          #### Modification indices
        } else {

          # Round
          print.modind.w[, -c(1L:3L)] <- lapply(print.modind.w[, -c(1L:3L)], function(y) ifelse(!is.na(y), formatC(y, digits = digits, format = "f",
                                                                                                                   zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

          # Column names
          print.modind.w <- rbind(c("", "", "", "MI", "EPC", "StdYX"), print.modind.w)

          # Add blank spaces
          print.modind.w[, "lhs"] <- paste("   ", print.modind.w[, "lhs"])

          # Justify
          print.modind.w[ c("lhs", "op", "rhs")] <- format(print.modind.w[, c("lhs", "op", "rhs")], justify = "left")
          print.modind.w[, -c(1L, 2L)] <- apply(print.modind.w[, -c(1L, 2L), drop = FALSE], 2L, function(y) format(y, justify = "right"))

          # Factor loadings
          print.modind.w.load <- print.modind.w[print.modind.w$op == "=~", ]

          if (isTRUE(nrow(print.modind.w.load) > 0L)) {

            cat("\n   Factor Loadings\n")

            # Print header
            write.table(print.modind.w[1L, ], quote = FALSE, row.names = FALSE, col.names = FALSE)

            write.table(print.modind.w.load, quote = FALSE, row.names = FALSE, col.names = FALSE)

          }

          # Residual covariances
          print.modind.w.cov <- print.modind.w[print.modind.w$op == "~~", ]

          if (isTRUE(nrow(print.modind.w.cov) > 0L)) {

            cat("\n   Residual Covariances\n")

            # Print header
            write.table(print.modind.w[1L, ], quote = FALSE, row.names = FALSE, col.names = FALSE)

            write.table(print.modind.w.cov, quote = FALSE, row.names = FALSE, col.names = FALSE)

          }

        }

        #...................
        ### Between Level ####

        cat("\n  Modification Indices: Between Level\n")

        # Filter modification indices
        print.modind.b <- print.modind.b[which(print.modind.b[, "mi"] >= x$args$mod.minval), ]

        #### No modification indices above the minimum value
        if (isTRUE(nrow(print.modind.b) == 0L)) {

          cat(paste0("\n   No modification indices above the minimum value ", round(x$args$mod.minval, digits = 2L), " at the Between Level.\n"))

          #### Modification indices
        } else {

          # Round
          print.modind.b[, -c(1L:3L)] <- lapply(print.modind.b[, -c(1L:3L)], function(y) ifelse(!is.na(y), formatC(y, digits = digits, format = "f",
                                                                                                                   zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

          # Column names
          print.modind.b <- rbind(c("", "", "", "MI", "EPC", "StdYX"), print.modind.b)

          # Add blank spaces
          print.modind.b[, "lhs"] <- paste("   ", print.modind.b[, "lhs"])

          # Justify
          print.modind.b[ c("lhs", "op", "rhs")] <- format(print.modind.b[, c("lhs", "op", "rhs")], justify = "left")
          print.modind.b[, -c(1L, 2L)] <- apply(print.modind.b[, -c(1L, 2L), drop = FALSE], 2L, function(y) format(y, justify = "right"))

          # Factor loadings
          print.modind.b.load <- print.modind.b[print.modind.b$op == "=~", ]

          if (isTRUE(nrow(print.modind.b.load) > 0L)) {

            cat("\n   Factor Loadings\n")

            # Print header
            write.table(print.modind.b[1L, ], quote = FALSE, row.names = FALSE, col.names = FALSE)

            write.table(print.modind.b.load, quote = FALSE, row.names = FALSE, col.names = FALSE)

          }

          # Residual covariances
          print.modind.b.cov <- print.modind.b[print.modind.b$op == "~~", ]

          if (isTRUE(nrow(print.modind.b.cov) > 0L)) {

            cat("\n   Residual Covariances\n")

            # Print header
            write.table(print.modind.b[1L, ], quote = FALSE, row.names = FALSE, col.names = FALSE)

            write.table(print.modind.b.cov, quote = FALSE, row.names = FALSE, col.names = FALSE)

          }

        }

      }

      #...................
      ### Univariate Score Tests ####

      # Extract result table
      print.score <- print.object$score

      #### Remove p-value and df
      print.score <- print.score[, -which(names(print.score) %in% c("df", "pvalue"))]

      #### Score tests not available
      if (isTRUE(is.null(print.score))) {

        cat("\n  Modification indices for parameter constraints are not available.\n")

        #### Score tests available
      } else {

        cat("\n  Modification Indices for Parameter Constraints\n\n")

        # Filter score tests
        print.score <- print.score[which(print.score[, "mi"] >= x$args$mod.minval), ]

        ##### Round
        print.score[, c("mi", "lhs.epc", "rhs.epc", "lhs.stdyx", "rhs.stdyx")] <- lapply(print.score[, c("mi", "lhs.epc", "rhs.epc", "lhs.stdyx", "rhs.stdyx")], function(y) ifelse(!is.na(y), formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

        if (isTRUE(nrow(print.score) == 0L)) {

          cat(paste0("   No modification indices for parameter constraints above the minimum value ", round(x$args$mod.minval, digits = 2L), ".\n"))

          #### Score test
        } else {

          ##### Column names
          print.score <- rbind(c("Label", "lhs", "op", "rhs", "MI", "lhs EPC", "rhs EPC", "lhs StdYX", "rhs StdYX"), print.score)

          ##### Add blank spaces
          print.score[, 1L] <- paste("  ", print.score[, 1L])

          print.score[-1L, 1L] <- paste0(" ", print.score[-1L, 1L])

          ##### Justify
          print.score[, "label"] <- format(print.score[, "label"], justify = "left")
          print.score[, c("lhs", "op", "rhs")] <- format(print.score[, c("lhs", "op", "rhs")], justify = "centre")
          print.score[, -c(1L:4L)] <- apply(print.score[, -c(1L:4L), drop = FALSE], 2L, function(y) format(y, justify = "right"))

          #...................
          ### Print ####

          write.table(print.score, quote = FALSE, row.names = FALSE, col.names = FALSE)

          # Note
          cat(paste0("\n   Note. Minimum value for printing modification indices is ", round(x$args$mod.minval, digits = 2L), "\n"))

        }

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Residual Correlation Matrix ####

    if (isTRUE("resid" %in% x$args$print)) {

      # Extract result table
      print.resid <- print.object$resid

      cat("\n  Residual Correlation Matrix\n")

      ### Residual correlation matrix not available
      if (isTRUE(is.null(unlist(print.resid)))) {

        cat("   Residual correlation matrix is not available.")

        ### Residual correlation matrix available
      } else {

        #### Numbers and unicode
        numbers <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
        unicode <- c("\U1D7CE", "\U1D7CF", "\U1D7D0", "\U1D7EF", "\U1D7F0", "\U1D7F1", "\U1D7F2", "\U1D7F3", "\U1D7F4", "\U1D7F5")

        #### Round
        print.resid <- lapply(print.resid, function(y) ifelse(!is.na(y), formatC(y, digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0")), NA))

        #### Lower triangular
        for (i in seq_along(print.resid)) { print.resid[[i]][upper.tri(print.resid[[i]])] <- "" }

        #### Correlation coefficients in boldface
        if (isTRUE(is.null(getOption("knitr.in.progress")) && x$args$resid.minval != 1L)) { for (i in seq_along(print.resid)) { print.resid[[i]][lower.tri(print.resid[[i]])][which(abs(x$result$resid[[i]][lower.tri(x$result$resid[[i]])]) > x$args$resid.minval)] <- na.omit(misty::chr.gsub(numbers, unicode, print.resid[[i]][lower.tri(print.resid[[i]])][which(abs(x$result$resid[[i]][lower.tri(x$result$resid[[i]])]) > x$args$resid.minval)])) } }

        #### Separate residual mean
        for (i in seq_along(print.resid)) { print.resid[[i]] <- rbind(print.resid[[i]][1L:(nrow(print.resid[[i]]) - 1L), ], rep("", times = ncol(print.resid[[i]])), Mean = print.resid[[i]][nrow(print.resid[[i]]), ]) }

        #### Column names
        print.resid <- lapply(print.resid, function(y) rbind(colnames(y), y))
        print.resid <- lapply(print.resid, function(y) cbind(rownames(y), y))

        #### Add blank spaces
        for (i in seq_along(print.resid)) { print.resid[[i]][, 1L] <- paste("   ", print.resid[[i]][, 1L]) }

        #### Justify
        for (i in seq_along(print.resid)) {

          print.resid[[i]][, 1L] <- format(print.resid[[i]][, 1L], justify = "left")
          print.resid[[i]][, -1L] <- format(print.resid[[i]][, -1L], justify = "right")

        }

        #...................
        ### Print ####

        cat("\n   Within Level\n")
        write.table(print.resid$within, quote = FALSE, row.names = FALSE, col.names = FALSE)

        cat("\n   Betweem Level\n")
        write.table(print.resid$between, quote = FALSE, row.names = FALSE, col.names = FALSE)

      }

    }

  #_____________________________________________________________________________
  #
  # Within-Group and Between-Group Correlation Matrix --------------------------
  }, multilevel.cor = {

    # Check input 'print'
    if (isTRUE(check)) {  if (isTRUE(any(!print %in% c("all", "cor", "se", "stat", "p")))) { stop("Character string(s) in the argument 'print' does not match with \"all\", \"cor\", \"se\", \"stat\", or \"p\".", call. = FALSE) } }

    # R Markdown in progress
    if (isTRUE(getOption("knitr.in.progress"))) { x$args$sig <- FALSE }

    #............
    ### Split results
    if (isTRUE(x$args$split)) {

      #### Round and format Within
      print.object$with.cor <- formatC(print.object$with.cor, digits = digits, format = "f",
                                       zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

      print.object$with.se <- formatC(print.object$with.se, digits = digits, format = "f",
                                      zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

      print.object$with.stat <- formatC(print.object$with.stat, digits = digits, format = "f",
                                        zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

      print.object$with.p <- formatC(print.object$with.p, digits = p.digits, format = "f",
                                     zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

      #### Round and format Between
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

      #### Lower and/or upper triangular
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

      #### Row names Within
      row.names(print.object$with.cor) <- paste0("   ", row.names(print.object$with.cor))
      row.names(print.object$with.se) <- paste0("   ", row.names(print.object$with.se))
      row.names(print.object$with.stat) <- paste0("   ", row.names(print.object$with.stat))
      row.names(print.object$with.p) <- paste0("   ", row.names(print.object$with.p))

      print.object$with.cor <- apply(print.object$with.cor, 2L, function(y) format(y, justify = "right"))
      print.object$with.se <- apply(print.object$with.se, 2L, function(y) format(y, justify = "right"))
      print.object$with.stat <- apply(print.object$with.stat, 2L, function(y) format(y, justify = "right"))
      print.object$with.p <- apply(print.object$with.p, 2L, function(y) format(y, justify = "right"))

      #### Row names Between
      row.names(print.object$betw.cor) <- paste0("   ", row.names(print.object$betw.cor))
      row.names(print.object$betw.se) <- paste0("   ", row.names(print.object$betw.se))
      row.names(print.object$betw.stat) <- paste0("   ", row.names(print.object$betw.stat))
      row.names(print.object$betw.p) <- paste0("   ", row.names(print.object$betw.p))

      print.object$betw.cor <- apply(print.object$betw.cor, 2L, function(y) format(y, justify = "right"))
      print.object$betw.se <- apply(print.object$betw.se, 2L, function(y) format(y, justify = "right"))
      print.object$betw.stat <- apply(print.object$betw.stat, 2L, function(y) format(y, justify = "right"))
      print.object$betw.p <- apply(print.object$betw.p, 2L, function(y) format(y, justify = "right"))

      #### Statistically significant correlation coefficients in boldface
      if (isTRUE(x$args$sig)) {

        numbers <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
        unicode <- c("\U1D7CE", "\U1D7CF", "\U1D7D0", "\U1D7EF", "\U1D7F0", "\U1D7F1", "\U1D7F2", "\U1D7F3", "\U1D7F4", "\U1D7F5")

        print.object$with.cor[lower.tri(print.object$with.cor)][which(x$result$with.p[lower.tri(x$result$with.p)] <= x$args$alpha)] <- na.omit(misty::chr.gsub(numbers, unicode, print.object$with.cor[lower.tri(print.object$with.cor)][which(x$result$with.p[lower.tri(x$result$with.p)] <= x$args$alpha)]))
        print.object$with.cor[upper.tri(print.object$with.cor)][which(x$result$with.p[upper.tri(x$result$with.p)] <= x$args$alpha)] <- na.omit(misty::chr.gsub(numbers, unicode, print.object$with.cor[upper.tri(print.object$with.cor)][which(x$result$with.p[upper.tri(x$result$with.p)] <= x$args$alpha)]))

        print.object$betw.cor[lower.tri(print.object$betw.cor)][which(x$result$betw.p[lower.tri(x$result$betw.p)] <= x$args$alpha)] <- na.omit(misty::chr.gsub(numbers, unicode, print.object$betw.cor[lower.tri(print.object$betw.cor)][which(x$result$betw.p[lower.tri(x$result$betw.p)] <= x$args$alpha)]))
        print.object$betw.cor[upper.tri(print.object$betw.cor)][which(x$result$betw.p[upper.tri(x$result$betw.p)] <= x$args$alpha)] <- na.omit(misty::chr.gsub(numbers, unicode, print.object$betw.cor[upper.tri(print.object$betw.cor)][which(x$result$betw.p[upper.tri(x$result$betw.p)] <= x$args$alpha)]))

      }

    #............
    ### Combined results
    } else {

      #### Round and format
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

      #### Row names
      row.names(print.object$wb.cor) <- paste0("   ", row.names(print.object$wb.cor))
      row.names(print.object$wb.se) <- paste0("   ", row.names(print.object$wb.se))
      row.names(print.object$wb.stat) <- paste0("   ", row.names(print.object$wb.stat))
      row.names(print.object$wb.p) <- paste0("   ", row.names(print.object$wb.p))

      print.object$wb.cor <- apply(print.object$wb.cor, 2L, function(y) format(y, justify = "right"))
      print.object$wb.se <- apply(print.object$wb.se, 2L, function(y) format(y, justify = "right"))
      print.object$wb.stat <- apply(print.object$wb.stat, 2L, function(y) format(y, justify = "right"))
      print.object$wb.p <- apply(print.object$wb.p, 2L, function(y) format(y, justify = "right"))

      #### Statistically significant correlation coefficients in boldface
      if (isTRUE(x$args$sig)) {

        numbers <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
        unicode <- c("\U1D7CE", "\U1D7CF", "\U1D7D0", "\U1D7EF", "\U1D7F0", "\U1D7F1", "\U1D7F2", "\U1D7F3", "\U1D7F4", "\U1D7F5")

        print.object$wb.cor[lower.tri(print.object$wb.cor)][which(x$result$wb.p[lower.tri(x$result$wb.p)] <= x$args$alpha)] <- na.omit(misty::chr.gsub(numbers, unicode, print.object$wb.cor[lower.tri(print.object$wb.cor)][which(x$result$wb.p[lower.tri(x$result$wb.p)] <= x$args$alpha)]))
        print.object$wb.cor[upper.tri(print.object$wb.cor)][which(x$result$wb.p[upper.tri(x$result$wb.p)] <= x$args$alpha)] <- na.omit(misty::chr.gsub(numbers, unicode, print.object$wb.cor[upper.tri(print.object$wb.cor)][which(x$result$wb.p[upper.tri(x$result$wb.p)] <= x$args$alpha)]))

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Output ####

    cat(" Within-Group and Between-Group Correlation Matrix\n")

    #............
    ### lavaan summary

    print.summary <- print.object$summary

    #............
    ### Format

    # Include spaces
    print.summary[1L, 1L] <- paste0(" ", print.summary[1L, 1L])
    print.summary[-1L, 1L] <- paste0("  ", print.summary[-1L, 1L])

    # Justify left
    print.summary[, 1L] <- format(print.summary[, 1L], justify = "left")

    #............
    ### Print

    print(print.summary, col.names = FALSE, row.names = FALSE, quote = FALSE, right = TRUE, max = 99999L)

    cat("\n")

    #............
    ### Split results
    if (isTRUE(x$args$split)) {

      #### Correlation coefficient
      if (isTRUE("cor" %in% print)) {

        # Within
        cat("  Within-Group\n")

        print(print.object$with.cor, quote = FALSE, right = TRUE, max = 99999L)

        # Between
        cat("\n  Between-Group\n")

        print(print.object$betw.cor, quote = FALSE, right = TRUE, max = 99999L)

      }

      #### Standard error
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

      #### Test statistic
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

      #### p.values
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

    #............
    ### Combined results
    } else {

      #### Correlation coefficient
      if (isTRUE("cor" %in% print)) {

        print(print.object$wb.cor, quote = FALSE, right = TRUE, max = 99999L)

      }

      #### Standard error
      if (isTRUE("se" %in% print)) {

        if (isTRUE("cor" %in% print)) { cat("\n") }

        cat(" Standard Error\n\n")

        print(print.object$wb.se, quote = FALSE, right = TRUE, max = 99999L)

      }

      #### Test statistic
      if (isTRUE("stat" %in% print)) {

        if (isTRUE(any(c("cor", "se") %in% print))) { cat("\n") }

        cat(" Test Statistic (z value) \n\n")

        print(print.object$wb.stat, quote = FALSE, right = TRUE, max = 99999L)

      }

      #### p.values
      if (isTRUE("p" %in% print)) {

        if (isTRUE(any(c("cor", "se", "stat") %in% print))) { cat("\n") }

        cat(" Significance Value (p-value)\n\n")

        print(print.object$wb.p, quote = FALSE, right = TRUE, max = 99999L)

        cat(paste0("\n  Adjustment for multiple testing: ", x$args$p.adj, "\n"))

      }

    }

    #### Note

    # Sample size within and between
    cat(paste0("\n Note. n(within) = ", lavaan::lavInspect(x$model.fit, what = "nobs"), ", n(between) = ", lavaan::lavInspect(x$model.fit, what = "nclusters")), "\n")

    # Lower and upper triangular
    if (isTRUE(!x$args$split)) {

      cat(ifelse(isTRUE(x$args$tri.lower), "       Lower triangular: Within-Group, Upper triangular: Between-Group",
                 "       Lower triangular: Between-Group, Upper triangular: Within-Group"), "\n")

    }

    # Statistical significance
    if (isTRUE(x$args$sig)) {

      cat(paste0("       Statistically significant coefficients at \U03B1 = ", signif(x$args$alpha, digits = 2L), " are boldface\n"))

    }

  #_____________________________________________________________________________
  #
  #  Multilevel Descriptive Statistics -----------------------------------------
  }, multilevel.descript = {

    print.object <- data.frame(c("Level 1", "No. of cases", "No. of missing values", "", "Variance Within", "SD Within", "",
                                 "Level 2", "No. of clusters", "Average cluster size", "SD cluster size", "Min cluster size", "Max cluster size", "", "Mean", "Variance Between", "SD Between", "ICC(1)", "ICC(2)", "",
                                 "Level 3", "No. of clusters", "Average cluster size", "SD cluster size", "Min cluster size", "Max cluster size", "", "Mean", "Variance Between", "SD Between", "ICC(1)", "ICC(2)", "",
                                 "Design effect", "Design effect sqrt", "Effective sample size"),
                               rbind(NA, print.object$no.obs, print.object$no.miss, NA, print.object$var.r, print.object$sd.r, NA,
                                     NA, print.object$no.cluster.l2, print.object$m.cluster.size.l2, print.object$sd.cluster.size.l2, print.object$min.cluster.size.l2, print.object$max.cluster.size.l2, NA, print.object$mean.x, print.object$var.u, print.object$sd.u, print.object$icc1.l2, print.object$icc2.l2, NA,

                                                           NA, print.object$no.cluster.l3, print.object$m.cluster.size.l3, print.object$sd.cluster.size.l3, print.object$min.cluster.size.l3, print.object$max.cluster.size.l3, NA, print.object$mean.x, print.object$var.v, print.object$sd.v, print.object$icc1.l3, print.object$icc2.l3, NA,
                                     print.object$deff, print.object$deff.sqrt, print.object$n.effect), fix.empty.names = FALSE, stringsAsFactors = FALSE)

    #............
    ### Format

    #### Variable names
    print.object <- rbind(c("", names(x$result$no.obs)), print.object)

    #### Round
    for (i in c(6L:7L, 11L:12L, 16L:18L, 24L:25L, 29L:31L, 35L:37L)) { print.object[i, 2L:ncol(print.object)] <- formatC(as.numeric(unlist(print.object[i, 2L:ncol(print.object)])), digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")) }

    for (i in c(19L:20L, 32L:33L)) { print.object[i, 2L:ncol(print.object)] <- formatC(as.numeric(unlist(print.object[i, 2L:ncol(print.object)])), digits = icc.digits, format = "f", zero.print = ifelse(icc.digits > 0L, paste0("0.", paste(rep(0L, times = icc.digits), collapse = "")), "0")) }

    # Blanks
    print.object[, 1L] <- paste(" ", print.object[, 1L])

    print.object[c(3L:8, 10L:21L, 23L:34L), 1L] <- paste("", print.object[c(3L:8, 10L:21L, 23L:34L), 1L])

    print.object[, 1L] <- format(print.object[, 1L, drop = FALSE])

    print.object[, 1L] <- format(unlist(print.object[, 1L]), justify = "left")
    print.object[, -1L] <- sapply(print.object[, -1L, drop = FALSE], function(y) format(as.character(y), justify = "right"))

    #............
    ### NA

    print.object[, -1L] <- sapply(print.object[, -1L], function(y) gsub("NA", "  ", y))

    #............
    ### Number of clusters

    # One cluster
    if (isTRUE(x$no.cluster == "one")) {

      print.object <- print.object[-c(21L:33L), ]

      # All Between variables
      if (isTRUE(all(misty::chr.trim(print.object[19L, -1]) == ""))) {

        print.object <- print.object[c(1L, 9L:10L, 15L, 16L:18L), ]

      }

    # Two clusters
    } else {

      print.object <- print.object[-16L, ]

      # All Between variables
      if (isTRUE(all(misty::chr.trim(print.object[6L, -1]) == ""))) {

        # Only Level 3 Variables
        if (isTRUE(all(misty::chr.trim(print.object[17L, -1]) == ""))) {

          print.object <- print.object[c(1L, 21L:22L, 27L:30L), ]

        # Level 2 Variables
        } else {

          print.object <- print.object[c(1L, 9L:10L, 15L:17L, 20L:36L), ]

        }

      }

    }

    #............
    ### Select rows

    # Variance and/or SD

    # Variance and/or SD
    if (isTRUE(!"var" %in% x$args$print)) { print.object <- print.object[-grep("Variance", print.object[, 1L]), ] }
    if (isTRUE(!"sd" %in% x$args$print)) { print.object <- print.object[-grep("SD", print.object[, 1L]), ] }

    # One variable
    if (isTRUE(ncol(print.object) == 2L)) { print.object <- print.object[-1L, ] }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Output ####

    cat(" Multilevel Descriptive Statistics\n\n")

    write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE)

  #_____________________________________________________________________________
  #
  # Simultaneous and Level-Specific Multilevel Model Fit Information  -----------
  }, multilevel.fit = {

    cat(" Simultaneous and Level-Specific Multilevel Model Fit Information\n")

    if (isTRUE("summary" %in% x$args$print)) {

      print.summary <- print.object$summary

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ## lavaan summary ####

      #...................
      ### Format ####

      # Include spaces
      print.summary[1L, 1L] <- paste0(" ", print.summary[1L, 1L])
      print.summary[-1L, 1L] <- paste0("  ", print.summary[-1L, 1L])

      # Within/Between
      print.summary[c(11L, 12L), 1L] <- paste0(" ", print.summary[c(10L, 11L), 1L])

      # Justify left
      print.summary[, 1L] <- format(print.summary[, 1L], justify = "left")

      #...................
      ### Print ####

      print(print.summary, col.names = FALSE, row.names = FALSE, quote = FALSE, right = TRUE, max = 99999L)

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Model fit ####

    if (isTRUE("fit" %in% x$args$print)) {

      print.fit <- print.object$fit

      #...................
      ### Round ####

      #### Maximum likelihood
      if (isTRUE(ncol(print.fit) == 2L)) {

        # Fit indices
        print.fit[-c(2L:19L, 41L:43L), -1L] <- sapply(print.fit[-c(2L:19L, 41L:43L), -1], function(y) ifelse(!is.na(y), formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

        # Loglikelihoood, information criteria, and test statistic
        print.fit[c(2L:13L), -1L] <- sapply(print.fit[c(2L:13L), -1], function(y) ifelse(!is.na(y), formatC(as.numeric(y), digits = digits - 1L, format = "f", zero.print = ifelse(digits - 1L > 0L, paste0("0.", paste(rep(0L, times = digits - 1L), collapse = "")), "0")), NA))

        # p.digits
        print.fit[c(17L:19L, 41L:43L), -1L] <- sapply(print.fit[c(17L:19L, 41L:43L), -1L], function(y) ifelse(!is.na(y), formatC(as.numeric(y), digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0")), NA))

        #### Robust maximum likelihood
      } else {

        # Fit indices
        print.fit[-c(2L:21L, 46L:48L), -1L] <- sapply(print.fit[-c(2L:21L, 46L:48L), -1L], function(y) ifelse(!is.na(y), formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

        # Loglikelihoood, information criteria, and test statistic
        print.fit[c(2L:15), -1L] <- sapply(print.fit[c(2L:15), -1L], function(y) ifelse(!is.na(y), formatC(as.numeric(y), digits = digits - 1L, format = "f", zero.print = ifelse(digits - 1L > 0L, paste0("0.", paste(rep(0L, times = digits - 1L), collapse = "")), "0")), NA))

        # p.digits
        print.fit[c(19L:21L, 46L:48L), -1L] <- sapply(print.fit[c(19L:21L, 46L:48L), -1L], function(y) ifelse(!is.na(y), formatC(as.numeric(y), digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0")), NA))

      }

      #...................
      ### Add labels ####

      #### Maximum likelihood
      if (isTRUE(ncol(print.fit) == 2L)) {

        print.fit <- rbind(print.fit[1L:9L, ], c("", "Standard"),
                           print.fit[10L:20L, ], c("", "Standard"),
                           print.fit[21L:29L, ], c("", "Standard"),
                           print.fit[30L:47L, ],
                           make.row.names = FALSE)

        #### Robust maximum likelihood
      } else {

        print.fit <- rbind(print.fit[1L:11L, ], c("", "Standard", "Scaled", ""),
                           print.fit[12L:25L, ], c("", "Standard", "Scaled", "Robust"),
                           print.fit[26L:34L, ], c("", "Standard", "Scaled", "Robust"),
                           print.fit[35L:52L, ],
                           make.row.names = FALSE)

      }

      #...................
      ### Replace NA with "" ####

      print.fit <- unname(misty::na.as(print.fit, na = ""))

      #...................
      ### Add blank space ####

      print.fit[, 1L] <- paste0("  ", print.fit[, 1L])

      #### Maximum likelihood
      if (isTRUE(ncol(print.fit) == 2L)) {

        print.fit[c(2L:3L, 6L:8L, 12L:20L, 24L:30L, 34L:50L), 1L] <- paste("", unlist(print.fit[c(2L:3L, 6L:8L, 12L:20L, 24L:30L, 34L:50L), 1L]))

        # Within/Between
        print.fit[c(13L, 14L, 16L, 17L, 19L, 20L, 25L, 26L, 29L, 30L, 35L, 36L, 39L, 40L, 42L, 43L, 45L, 46L, 49L, 50L), 1L] <- paste("", unlist(print.fit[c(13L, 14L, 16L, 17L, 19L, 20L, 25L, 26L, 29L, 30L, 35L, 36L, 39L, 40L, 42L, 43L, 45L, 46L, 49L, 50L), 1L]))

        # RMSEA 95% CI - p-values
        print.fit[c(38L:46), 1L] <- paste("", unlist(print.fit[c(38L:46), 1L]))

        #### Robust maximum likelihood
      } else {

        print.fit[c(2L:5L, 8L:10L, 14L:25L, 29L:35L, 39L:55L), 1L] <- paste("", unlist(print.fit[c(2L:5L, 8L:10L, 14L:25L, 29L:35L, 39L:55L), 1L]))

        # Within/Between
        print.fit[c(15L, 16L, 18L, 19L, 21L, 22L, 24L, 25L, 30L, 31L, 34L, 35L, 40L, 41L, 44L, 45L, 47L, 48L, 50L, 51L, 54L, 55L), 1L] <- paste("", unlist(print.fit[c(15L, 16L, 18L, 19L, 21L, 22L, 24L, 25L, 30L, 31L, 34L, 35L, 40L, 41L, 44L, 45L, 47L, 48L, 50L, 51L, 54L, 55L), 1L]))

        # RMSEA 95% CI - p-values
        print.fit[c(43L:51L), 1L] <- paste("", unlist(print.fit[c(43L:51L), 1L]))

      }

      #...................
      ### Justify left and right ####

      print.fit[, 1L] <- format(print.fit[, 1L, drop = FALSE], justify = "left")

      print.fit[, -1L] <- apply(print.fit[, -1L, drop = FALSE], 2L, function(y) format(y, justify = "right"))

      #...................
      ### Print ####

      cat("\n")

      write.table(print.fit, quote = FALSE, row.names = FALSE, col.names = FALSE)

    }

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

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Output ####

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
  # Cross-Level Measurement Invariance -----------------------------------------
  }, multilevel.invar = {

    cat(" Cross-Level Measurement Invariance\n")

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## lavaan summary ####

    if (isTRUE("summary" %in% x$args$print)) {

      print.summary <- print.object$summary

      #...................
      ### Format ####

      # Include spaces
      print.summary[1L, 1L] <- paste0(" ", print.summary[1L, 1L])
      print.summary[-1L, 1L] <- paste0("  ", print.summary[-1L, 1L])

      print.summary[13L:14L, 1L] <- paste0(" ", print.summary[13L:14L, 1L])

      # Justify left
      print.summary[, 1L] <- format(print.summary[, 1L], justify = "left")

      #...................
      ### Print ####

      print(print.summary, col.names = FALSE, row.names = FALSE, quote = FALSE, right = TRUE, max = 99999L)

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Covariance coverage ####

    if (isTRUE("coverage" %in% x$args$print)) {

      cat("\n  Covariance Coverage of Data\n\n")

      print.coverage <- print.object$coverage

      #...................
      ### Lower triangular ####

      print.coverage[upper.tri(print.coverage)] <- ""

      #...................
      ### Format ####

      print.coverage <- apply(print.coverage, 2L, function(y) ifelse(!is.na(as.numeric(y)), formatC(as.numeric(y), digits = p.digits - 1L, format = "f", zero.print = ifelse(2L > 0L, paste0("0.", paste(rep(0L, times = 2L), collapse = "")), "0")), ""))

      # Row names
      row.names(print.coverage) <- paste0("   ", colnames(print.coverage))

      #...................
      ### Print ####

      print(print.coverage, row.names = FALSE, quote = FALSE, right = TRUE, max = 99999L)

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Sample Statistics ####

    if (isTRUE("descript" %in% x$args$print)) {

      cat("\n  Univariate Sample Statistics\n\n")

      print.itemstat <- print.object$descript

      #...................
      ### Format ####

      # Variables to round
      print.round <- c("m", "sd", "min", "max", "skew", "kurt", "ICC")

      print.itemstat[, print.round] <- sapply(print.round, function(y) ifelse(!is.na(print.itemstat[, y]), formatC(print.itemstat[, y], digits = digits, format = "f",zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))
      print.itemstat[, "pNA"] <- formatC(print.itemstat[, "pNA"], digits = digits - 1L, format = "f", zero.print = ifelse(digits - 1L > 0L, paste0("0.", paste(rep(0L, times = digits - 1L), collapse = "")), "0"))

      # Percentages
      print.itemstat[, "pNA"] <- paste0(print.itemstat[, "pNA"], "%")

      # Column names
      print.itemstat <- rbind(c("Variable", "n", "nNA", "pNA", "M", "SD", "Min", "Max",  "Skew", "Kurt", "ICC(1)"), print.itemstat)

      # Justify left and right
      print.itemstat[, 1L] <- format(print.itemstat[, 1L, drop = FALSE], justify = "left")
      print.itemstat[, -1L] <- apply(print.itemstat[, -1L, drop = FALSE], 2L, function(y) format(y, justify = "right"))

      # Add blank space
      print.itemstat[, "variable"] <- c(paste0("   ", print.itemstat[1L, "variable"], collapse = ""), paste0("    ", print.itemstat[-1L, "variable"]))
      print.itemstat[, "variable"] <- format(c(print.itemstat[1L, "variable"], misty::chr.trim(print.itemstat[-1L, "variable"], side = "right")), justify = "left")

      #...................
      ### Print ####

      write.table(print.itemstat, quote = FALSE, row.names = FALSE, col.names = FALSE)

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Model fit ####

    if (isTRUE("fit" %in% x$args$print)) {

      if (isTRUE(x$args$invar != "config")) { cat("\n  Model Fit Information and Model Comparison\n") } else { cat("\n  Model Fit Information\n") }

      print.fit <- print.object$fit

      print.fit.stand <- print.fit$stand
      print.fit.scaled <- print.fit$scaled
      print.fit.robust <- print.fit$robust

      #...................
      ##### Round

      print.fit.stand[-c(2L:4L, 14L, 21L:23L), -1L] <- sapply(print.fit.stand[-c(2L:4L, 14L, 21L:23L), -1L], function(y) ifelse(!is.na(y), formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))
      if (isTRUE(!is.null(print.fit.scaled))) { print.fit.scaled[-c(2L:4L, 15L, 22L:24L), -1L] <- sapply(print.fit.scaled[-c(2L:4L, 15L, 22L:24L), -1L], function(y) ifelse(!is.na(y), formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA)) }
      if (isTRUE(!is.null(print.fit.robust))) { print.fit.robust[-c(2L:4L, 15L, 22L:24L), -1L] <- sapply(print.fit.robust[-c(2L:4L, 15L, 22L:24L), -1L], function(y) ifelse(!is.na(y), formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA)) }

      # Information criteria
      print.fit.stand[c(2L, 21L:23L), -1L] <- sapply(print.fit.stand[c(2L, 21L:23L), -1L], function(y) ifelse(!is.na(y), formatC(as.numeric(y), digits = digits - 1L, format = "f", zero.print = ifelse(digits - 1L > 0L, paste0("0.", paste(rep(0L, times = digits - 1L), collapse = "")), "0")), NA))
      if (isTRUE(!is.null(print.fit.scaled))) { print.fit.scaled[c(2L, 22L:24L), -1L] <- sapply(print.fit.scaled[c(2L, 22L:24L), -1L], function(y) ifelse(!is.na(y), formatC(as.numeric(y), digits = digits - 1L, format = "f", zero.print = ifelse(digits - 1L > 0L, paste0("0.", paste(rep(0L, times = digits - 1L), collapse = "")), "0")), NA)) }
      if (isTRUE(!is.null(print.fit.robust))) { print.fit.robust[c(2L, 22L:24L), -1L] <- sapply(print.fit.robust[c(2L, 22L:24L), -1L], function(y) ifelse(!is.na(y), formatC(as.numeric(y), digits = digits - 1L, format = "f", zero.print = ifelse(digits - 1L > 0L, paste0("0.", paste(rep(0L, times = digits - 1L), collapse = "")), "0")), NA)) }

      # p.digits
      print.fit.stand[c(4L, 14L), -1L] <- sapply(print.fit.stand[c(4L, 14L), -1L], function(y) ifelse(!is.na(y), formatC(as.numeric(y), digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0")), NA))
      if (isTRUE(!is.null(print.fit.scaled))) { print.fit.scaled[c(4L, 15L), -1L] <- sapply(print.fit.scaled[c(4L, 15L), -1L], function(y) ifelse(!is.na(y), formatC(as.numeric(y), digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0")), NA)) }
      if (isTRUE(!is.null(print.fit.robust))) { print.fit.robust[c(4L, 15L), -1L] <- sapply(print.fit.robust[c(4L, 15L), -1L], function(y) ifelse(!is.na(y), formatC(as.numeric(y), digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0")), NA)) }

      #...................
      ##### Add labels ####

      print.fit.stand <- rbind(switch(x$args$invar, config = { c("", "Config") }, metric = { c("", "Config", "Metric", "DMetric") }, scalar = { c("", "Config", "Metric", "Scalar", "DMetric", "DScalar") }), print.fit.stand[1L:23L, ], make.row.names = FALSE)
      if (isTRUE(!is.null(print.fit.scaled))) { print.fit.scaled <- rbind(switch(x$args$invar, config = { c("", "Config") }, metric = { c("", "Config", "Metric", "DMetric") }, scalar = { c("", "Config", "Metric", "Scalar", "DMetric", "DScalar") }), print.fit.scaled[1L:24L, ], make.row.names = FALSE) }
      if (isTRUE(!is.null(print.fit.robust))) { print.fit.robust <- rbind(switch(x$args$invar, config = { c("", "Config") }, metric = { c("", "Config", "Metric", "DMetric") }, scalar = { c("", "Config", "Metric", "Scalar", "DMetric", "DScalar") }), print.fit.robust[1L:24L, ], make.row.names = FALSE) }

      #...................
      ##### Replace NA with "" ####

      print.fit.stand <- unname(misty::na.as(print.fit.stand, na = ""))
      if (isTRUE(!is.null(print.fit.scaled))) { print.fit.scaled <- unname(misty::na.as(print.fit.scaled, na = "")) }
      if (isTRUE(!is.null(print.fit.robust))) { print.fit.robust <- unname(misty::na.as(print.fit.robust, na = "")) }

      #...................
      ##### Add blank space ####

      print.fit.stand[, 1L] <- paste0("    ", print.fit.stand[, 1L])
      if (isTRUE(!is.null(print.fit.scaled))) { print.fit.scaled[, 1L] <- paste0("    ", print.fit.scaled[, 1L]) }
      if (isTRUE(!is.null(print.fit.robust))) { print.fit.robust[, 1L] <- paste0("    ", print.fit.robust[, 1L]) }

      print.fit.stand[c(3L:5L, 8L:9L, 12L:19L, 22L:24), 1L] <- paste("", unlist(print.fit.stand[c(3L:5L, 8L:9L, 12L:19L, 22L:24), 1L]))
      if (isTRUE(!is.null(print.fit.scaled))) { print.fit.scaled[c(3L:6L, 9L:10L, 13L:20L, 23L:25L), 1L] <- paste("", unlist(print.fit.scaled[c(3L:6L, 9L:10L, 13L:20L, 23L:25L), 1L])) }
      if (isTRUE(!is.null(print.fit.robust))) { print.fit.robust[c(3L:6L, 9L:10L, 13L:20L, 23L:25L), 1L] <- paste("", unlist(print.fit.robust[c(3L:6L, 9L:10L, 13L:20L, 23L:25L), 1L])) }

      # Within/Between
      print.fit.stand[18L:19L, 1L] <- paste("", unlist(print.fit.stand[18L:19L, 1L]))
      if (isTRUE(!is.null(print.fit.scaled))) { print.fit.scaled[19L:20L, 1L] <- paste("", unlist(print.fit.scaled[19L:20L, 1L])) }
      if (isTRUE(!is.null(print.fit.robust))) { print.fit.robust[19L:20L, 1L] <- paste("", unlist(print.fit.robust[19L:20L, 1L])) }

      # RMSEA 95% CI - p-values
      print.fit.stand[13L:15L, 1L] <- paste("", unlist(print.fit.stand[13L:15L, 1L]))
      if (isTRUE(!is.null(print.fit.scaled))) { print.fit.scaled[14L:16L, 1L] <- paste("", unlist(print.fit.scaled[14L:16L, 1L])) }
      if (isTRUE(!is.null(print.fit.robust))) { print.fit.robust[14L:16L, 1L] <- paste("", unlist(print.fit.robust[14L:16L, 1L])) }

      #...................
      ##### Justify left and right ####

      print.fit.stand[, 1L] <- format(print.fit.stand[, 1L, drop = FALSE], justify = "left")
      if (isTRUE(!is.null(print.fit.scaled))) { print.fit.scaled[, 1L] <- format(print.fit.scaled[, 1L, drop = FALSE], justify = "left") }
      if (isTRUE(!is.null(print.fit.robust))) { print.fit.robust[, 1L] <- format(print.fit.robust[, 1L, drop = FALSE], justify = "left") }

      print.fit.stand[, -1L] <- apply(print.fit.stand[, -1L, drop = FALSE], 2L, function(y) format(y, justify = "right"))
      if (isTRUE(!is.null(print.fit.scaled))) { print.fit.scaled[, -1L] <- apply(print.fit.scaled[, -1L, drop = FALSE], 2L, function(y) format(y, justify = "right")) }
      if (isTRUE(!is.null(print.fit.robust))) { print.fit.robust[, -1L] <- apply(print.fit.robust[, -1L, drop = FALSE], 2L, function(y) format(y, justify = "right")) }

      #...................
      ##### Print ####

      if (isTRUE("standard" %in% x$args$print.fit)) {

        cat("\n   Standard CFI, TLI, and RMSEA\n")

        # R Markdown not in progress
        if (is.null(getOption("knitr.in.progress"))) { print.fit.stand[1L, ] <- sub("D", "\u0394", print.fit.stand[1L, ]) }

        write.table(print.fit.stand, quote = FALSE, row.names = FALSE, col.names = FALSE)

      }

      if (isTRUE("scaled" %in% x$args$print.fit)) {

        cat("\n   Scaled CFI, TLI, and RMSEA\n")

        # R Markdown not in progress
        if (is.null(getOption("knitr.in.progress"))) { print.fit.scaled[1L, ] <- sub("D", "\u0394", print.fit.scaled[1L, ]) }

        write.table(print.fit.scaled, quote = FALSE, row.names = FALSE, col.names = FALSE)

      }

      if (isTRUE("robust" %in% x$args$print.fit)) {

        cat("\n   Robust CFI, TLI, and RMSEA\n")

        # R Markdown not in progress
        if (is.null(getOption("knitr.in.progress"))) { print.fit.robust[1L, ] <- sub("D", "\u0394", print.fit.robust[1L, ]) }

        write.table(print.fit.robust, quote = FALSE, row.names = FALSE, col.names = FALSE)

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Parameter estimates ####

    if (isTRUE("est" %in% x$args$print)) {

      switch(x$args$invar,
             ### Configural invariance ####
             config = {

               cat("\n  Model Results: Configural Cross-Level Measurement Invariance\n")

               print.param.w <- print.object$param$config$within
               print.param.b <- print.object$param$config$between

               ### Metric invariance ####
             }, metric = {

               cat("\n  Model Results: Metric Cross-Level Measurement Invariance\n")

               print.param.w <- print.object$param$metric$within
               print.param.b <- print.object$param$metric$between


               ### Scalar invariance ####
             }, scalar = {

               cat("\n  Model Results: Scalar Cross-Level Measurement Invariance\n")

               print.param.w <- print.object$param$scalar$within
               print.param.b <- print.object$param$scalar$between


             })

      cat("\n   Within Level\n\n")

      #...................
      ### Within Level ####

      #### Round ####

      # digits
      print.param.w[, -c(1L:4L, 8L)] <- lapply(print.param.w[, -c(1L:4L, 8L)], function(y) ifelse(!is.na(y), formatC(y, digits = digits, format = "f",
                                                                                                                     zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

      # p.digits
      print.param.w[, "pvalue"] <- formatC(as.numeric(print.param.w[, "pvalue"]), digits = p.digits, format = "f",
                                           zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

      #### Add blank spaces ####
      print.param.w[apply(print.param.w[, c("est", "se", "z", "stdyx")], 1L, function(y) all(is.na(y))), "rhs"] <- paste("    ", print.param.w[apply(print.param.w[, c("est", "se", "z", "stdyx")], 1L, function(y) all(is.na(y))), "rhs"])
      print.param.w[apply(print.param.w[, c("est", "se", "z", "stdyx")], 1L, function(y) any(!is.na(y))), "rhs"] <- paste("      ", print.param.w[apply(print.param.w[, c("est", "se", "z", "stdyx")], 1L, function(y) any(!is.na(y))), "rhs"])

      #### Convert NA into "" ####
      print.param.w[apply(print.param.w[, c("est", "se", "z", "stdyx")], 1L, function(y) all(is.na(y))), c("est", "se", "z", "pvalue", "stdyx")] <- ""
      print.param.w[apply(print.param.w[, c("se", "z")], 1L, function(y) all(is.na(y))), c("se", "z", "pvalue", "stdyx")] <- ""
      print.param.w[which(is.na(print.param.w$stdyx)), "stdyx"] <- ""

      #### Column names ####
      print.param.w <- rbind(c("", "", "", "", "Estimate", "Std.Err", "z-value", "P(>|z|)", "StdYX"), print.param.w)

      #### Justify ####
      print.param.w[, "rhs"] <- format(print.param.w[, "rhs", drop = FALSE], justify = "left")
      print.param.w[, -c(1L:4L)] <- apply(print.param.w[, -c(1L:4L), drop = FALSE], 2L, function(y) format(y, justify = "right"))

      #...................
      #### Print ####

      ##### Factor loadings
      if (isTRUE(any(print.param.w$param %in% "latent variable"))) {

        cat("    Factor Loadings\n")

        # Heading
        write.table(print.param.w[1L, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

        print.lv <- print.param.w[print.param.w$param == "latent variable", ]

        for (i in unique(print.lv$lhs)) {

          pos.NA <- grep("NA", print.lv[print.lv$lhs == i, "z"])

          if (isTRUE(length(pos.NA) > 0L)) {

            print.lv[print.lv$lhs == i, ][pos.NA, "se"] <- paste(rep("", times = unique(nchar(print.lv[print.lv$lhs == i, ][pos.NA, "se"])) + 1L), collapse = " ")
            print.lv[print.lv$lhs == i, ][pos.NA, "z"] <- paste(rep("", times = unique(nchar(print.lv[print.lv$lhs == i, ][pos.NA, "z"])) + 1L), collapse = " ")
            print.lv[print.lv$lhs == i, ][pos.NA, "pvalue"] <- paste(rep("", times = unique(nchar(print.lv[print.lv$lhs == i, ][pos.NA, "pvalue"])) + 1L), collapse = " ")

          }

          write.table(print.lv[print.lv$lhs == i, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

        }

      }

      ##### Latent variable covariances
      if (isTRUE(any(print.param.w$param %in% "latent variable covariance"))) {

        cat("\n    Latent Variable Covariances\n")

        print.lv.cov <- print.param.w[print.param.w$param == "latent variable covariance", ]

        for (i in unique(print.lv.cov$lhs)) {

          write.table(print.lv.cov[print.lv.cov$lhs == i, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

        }

      }

      ##### Residual covariances
      if (isTRUE(any(print.param.w$param %in% "residual covariance"))) {

        # Heading latent variables
        if (isTRUE(any(print.param.w$param %in% "latent variable"))) {

          cat("\n    Residual Covariances\n")

          # Heading no latent variables
        } else {

          cat("\n    Covariances\n")

          write.table(print.param.w[1L, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

        }

        print.res.cov <- print.param.w[print.param.w$param == "residual covariance", ]

        for (i in unique(print.res.cov$lhs)) {

          write.table(print.res.cov[print.res.cov$lhs == i, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

        }

      }

      ##### Latent variance
      if (isTRUE(any(print.param.w$param %in% "latent variance"))) {

        cat("\n    Latent Variances\n")

        print.var <- print.param.w[print.param.w$param == "latent variance", ]

        print.var[grep("NA", print.var[, "z"]), c("se", "z", "pvalue", "stdyx")] <- ""

        write.table(print.var[, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

      }

      ##### Residual variance
      if (isTRUE(any(print.param.w$param %in% "residual variance"))) {

        # Heading latent variables
        if (isTRUE(any(print.param.w$param %in% "latent variable"))) {

          cat("\n    Residual Variances\n")

        } else {

          cat("\n    Variances\n")

        }

        print.resid <- print.param.w[print.param.w$param == "residual variance", ]

        pos.NA <- grep("NA", print.resid[, "z"])

        if (isTRUE(length(pos.NA) >  0L)) {

          print.resid[pos.NA, "se"] <- paste(rep("", times = unique(nchar(print.resid[pos.NA, "se"])) + 1L), collapse = " ")
          print.resid[pos.NA, "z"] <- paste(rep("", times = unique(nchar(print.resid[pos.NA, "z"])) + 1L), collapse = " ")
          print.resid[pos.NA, "pvalue"] <- paste(rep("", times = unique(nchar(print.resid[pos.NA, "pvalue"])) + 1L), collapse = " ")

        }

        write.table(print.resid[, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

      }

      #...................
      ### Between Level ####

      cat("\n   Between Level\n\n")

      #...................
      #### Round ####

      # digits
      print.param.b[, -c(1L:4L, 8L)] <- lapply(print.param.b[, -c(1L:4L, 8L)], function(y) ifelse(!is.na(y), formatC(y, digits = digits, format = "f",
                                                                                                                     zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

      # p.digits
      print.param.b[, "pvalue"] <- formatC(as.numeric(print.param.b[, "pvalue"]), digits = p.digits, format = "f",
                                           zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

      #...................
      #### Add blank spaces ####
      print.param.b[apply(print.param.b[, c("est", "se", "z", "stdyx")], 1L, function(y) all(is.na(y))), "rhs"] <- paste("    ", print.param.b[apply(print.param.b[, c("est", "se", "z", "stdyx")], 1L, function(y) all(is.na(y))), "rhs"])
      print.param.b[apply(print.param.b[, c("est", "se", "z", "stdyx")], 1L, function(y) any(!is.na(y))), "rhs"] <- paste("      ", print.param.b[apply(print.param.b[, c("est", "se", "z", "stdyx")], 1L, function(y) any(!is.na(y))), "rhs"])

      #...................
      #### Convert NA into "" ####
      print.param.b[apply(print.param.b[, c("est", "se", "z", "stdyx")], 1L, function(y) all(is.na(y))), c("est", "se", "z", "pvalue", "stdyx")] <- ""
      print.param.b[apply(print.param.b[, c("se", "z")], 1L, function(y) all(is.na(y))), c("se", "z", "pvalue", "stdyx")] <- ""
      print.param.b[which(is.na(print.param.b$stdyx)), "stdyx"] <- ""

      #...................
      #### Column names ####
      print.param.b <- rbind(c("", "", "", "", "Estimate", "Std.Err", "z-value", "P(>|z|)", "StdYX"), print.param.b)

      #...................
      #### Justify ####
      print.param.b[, "rhs"] <- format(print.param.b[, "rhs", drop = FALSE], justify = "left")
      print.param.b[, -c(1L:4L)] <- apply(print.param.b[, -c(1L:4L), drop = FALSE], 2L, function(y) format(y, justify = "right"))

      #...................
      #### Print ####

      ##### Factor loadings
      if (isTRUE(any(print.param.b$param %in% "latent variable"))) {

        cat("    Factor Loadings\n")

        # Heading
        write.table(print.param.b[1L, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

        print.lv <- print.param.b[print.param.b$param == "latent variable", ]

        for (i in unique(print.lv$lhs)) {

          pos.NA <- grep("NA", print.lv[print.lv$lhs == i, "z"])

          if (isTRUE(length(pos.NA) > 0L)) {

            print.lv[print.lv$lhs == i, ][pos.NA, "se"] <- paste(rep("", times = unique(nchar(print.lv[print.lv$lhs == i, ][pos.NA, "se"])) + 1L), collapse = " ")
            print.lv[print.lv$lhs == i, ][pos.NA, "z"] <- paste(rep("", times = unique(nchar(print.lv[print.lv$lhs == i, ][pos.NA, "z"])) + 1L), collapse = " ")
            print.lv[print.lv$lhs == i, ][pos.NA, "pvalue"] <- paste(rep("", times = unique(nchar(print.lv[print.lv$lhs == i, ][pos.NA, "pvalue"])) + 1L), collapse = " ")

          }

          write.table(print.lv[print.lv$lhs == i, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

        }

      }

      ##### Latent variable covariances
      if (isTRUE(any(print.param.b$param %in% "latent variable covariance"))) {

        cat("\n    Latent Variable Covariances\n")

        print.lv.cov <- print.param.b[print.param.b$param == "latent variable covariance", ]

        for (i in unique(print.lv.cov$lhs)) {

          write.table(print.lv.cov[print.lv.cov$lhs == i, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

        }

      }

      ##### (Residual) Covariances
      if (isTRUE(any(print.param.b$param %in% "residual covariance"))) {

        # Heading latent variables
        if (isTRUE(any(print.param.b$param %in% "latent variable"))) {

          cat("\n    Residual Covariances\n")

          # Heading no latent variables
        } else {

          cat("\n    Covariances\n")

          write.table(print.param.b[1L, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

        }

        print.res.cov <- print.param.b[print.param.b$param == "residual covariance", ]

        for (i in unique(print.res.cov$lhs)) {

          write.table(print.res.cov[print.res.cov$lhs == i, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

        }

      }

      ##### Latent mean
      if (isTRUE(any(print.param.b$param %in% "latent mean"))) {

        cat("\n     Latent Means\n")

        print.mean <- print.param.b[print.param.b$param == "latent mean", ]

        print.mean[grep("NA", print.mean[, "z"]), c("se", "z", "pvalue", "stdyx")] <- ""

        write.table(print.mean[, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

      }

      ##### Latent variance
      if (isTRUE(any(print.param.b$param %in% "latent variance"))) {

        cat("\n    Latent Variances\n")

        print.var <- print.param.b[print.param.b$param == "latent variance", ]

        print.var[grep("NA", print.var[, "z"]), c("se", "z", "pvalue", "stdyx")] <- ""

        write.table(print.var[, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

      }

      ##### Intercepts
      if (isTRUE(any(print.param.b$param %in% "intercept"))) {

        cat("\n    Intercepts\n")

        print.inter <- print.param.b[print.param.b$param == "intercept", ]

        print.inter[grep("NA", print.inter[, "z"]), c("se", "z", "pvalue", "stdyx")] <- ""

        write.table(print.inter[, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

      }

      ##### (Residual) Variance
      if (isTRUE(any(print.param.b$param %in% "residual variance"))) {

        # Heading latent variables
        if (isTRUE(any(print.param.b$param %in% "latent variable"))) {

          cat("\n    Residual Variances\n")

        } else {

          cat("\n    Variances\n")

        }

        print.resid <- print.param.b[print.param.b$param == "residual variance", ]

        pos.NA <- grep("NA", print.resid[, "z"])

        if (isTRUE(length(pos.NA) >  0L)) {

          print.resid[pos.NA, "se"] <- paste(rep("", times = unique(nchar(print.resid[pos.NA, "se"])) + 1L), collapse = " ")
          print.resid[pos.NA, "z"] <- paste(rep("", times = unique(nchar(print.resid[pos.NA, "z"])) + 1L), collapse = " ")
          print.resid[pos.NA, "pvalue"] <- paste(rep("", times = unique(nchar(print.resid[pos.NA, "pvalue"])) + 1L), collapse = " ")

        }

        write.table(print.resid[, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Modification indices ####

    if (isTRUE("modind" %in% x$args$print)) {

      switch(x$args$invar,
             ### Configural invariance ####
             config = {

               cat("\n  Modification Indices: Configural Cross-Level Measurement Invariance\n")

               print.modind.w <- print.object$modind$config$within
               print.modind.b <- print.object$modind$config$between

               ### Metric invariance ####
             }, metric = {

               cat("\n  Modification Indices: Metric Cross-Level Measurement Invariance\n")

               print.modind.w <- print.object$modind$metric$within
               print.modind.b <- print.object$modind$metric$between


               ### Scalar invariance ####
             }, scalar = {

               cat("\n  Modification Indices: Scalar Cross-Level Measurement Invariance\n")

               print.modind.w <- print.object$modind$scalar$within
               print.modind.b <- print.object$modind$scalar$between

             })

      #...................
      ### Within Level ####

      #### Modification indices not available
      if (isTRUE(is.null(print.modind.w) && is.null(print.modind.b))) {

        cat("\n    Modification indices are not available.")

        #### Modification available
      } else {

        cat("\n   Within Level\n")

        if (isTRUE(nrow(print.modind.w) == 0L)) {

          cat(paste0("\n    No modification indices above the minimum value ", round(x$args$mod.minval, digits = 2L), " at the Within Level.\n"))

          #### Modification indices
        } else {

          # Round
          print.modind.w[, -c(1L:3L)] <- lapply(print.modind.w[, -c(1L:3L)], function(y) ifelse(!is.na(y), formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

          # Column names
          print.modind.w <- rbind(c("", "", "", "MI", "EPC", "StdYX"), print.modind.w)

          # Add blank spaces
          print.modind.w[, "lhs"] <- paste("    ", print.modind.w[, "lhs"])

          # Justify
          print.modind.w[, c("lhs", "op", "rhs")] <- format(print.modind.w[, c("lhs", "op", "rhs")], justify = "left")
          print.modind.w[, -c(1L, 2L)] <- apply(print.modind.w[, -c(1L, 2L), drop = FALSE], 2L, function(y) format(y, justify = "right"))

          # Factor loadings
          print.modind.w.load <- print.modind.w[print.modind.w$op == "=~", ]

          if (isTRUE(nrow(print.modind.w.load) > 0L)) {

            cat("\n    Factor Loadings\n")

            # Print header
            write.table(print.modind.w[1L, ], quote = FALSE, row.names = FALSE, col.names = FALSE)

            write.table(print.modind.w.load, quote = FALSE, row.names = FALSE, col.names = FALSE)

          }

          # Residual covariances
          print.modind.w.cov <- print.modind.w[print.modind.w$op == "~~", ]

          if (isTRUE(nrow(print.modind.w.cov) > 0L)) {

            cat("\n    Residual Covariances\n")

            # Print header
            write.table(print.modind.w[1L, ], quote = FALSE, row.names = FALSE, col.names = FALSE)

            write.table(print.modind.w.cov, quote = FALSE, row.names = FALSE, col.names = FALSE)

          }

        }

        #...................
        ### Between Level ####

        cat("\n   Between Level\n")

        #### No modification indices above the minimum value
        if (isTRUE(nrow(print.modind.b) == 0L)) {

          cat(paste0("\n    No modification indices above the minimum value ", round(x$args$mod.minval, digits = 2L), " at the Between Level.\n"))

          #### Modification indices
        } else {

          # Round
          print.modind.b[, -c(1L:3L)] <- lapply(print.modind.b[, -c(1L:3L)], function(y) ifelse(!is.na(y), formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

          # Column names
          print.modind.b <- rbind(c("", "", "", "MI", "EPC", "StdYX"), print.modind.b)

          # Add blank spaces
          print.modind.b[, "lhs"] <- paste("    ", print.modind.b[, "lhs"])

          # Justify
          print.modind.b[ c("lhs", "op", "rhs")] <- format(print.modind.b[, c("lhs", "op", "rhs")], justify = "left")
          print.modind.b[, -c(1L, 2L)] <- apply(print.modind.b[, -c(1L, 2L), drop = FALSE], 2L, function(y) format(y, justify = "right"))

          # Factor loadings
          print.modind.b.load <- print.modind.b[print.modind.b$op == "=~", ]

          if (isTRUE(nrow(print.modind.b.load) > 0L)) {

            cat("\n    Factor Loadings\n")

            # Print header
            write.table(print.modind.b[1L, ], quote = FALSE, row.names = FALSE, col.names = FALSE)

            write.table(print.modind.b.load, quote = FALSE, row.names = FALSE, col.names = FALSE)

          }

          # Residual covariances
          print.modind.b.cov <- print.modind.b[print.modind.b$op == "~~", ]

          if (isTRUE(nrow(print.modind.b.cov) > 0L)) {

            cat("\n    Residual Covariances\n")

            # Print header
            write.table(print.modind.b[1L, ], quote = FALSE, row.names = FALSE, col.names = FALSE)

            write.table(print.modind.b.cov, quote = FALSE, row.names = FALSE, col.names = FALSE)

          }

        }

      }

      #...................
      ### Univariate Score Tests ####

      #### Level of measurement invariance ####
      switch(x$args$invar,
             ### Configural invariance ####
             config = {

               print.score <- print.object$score$config

               ### Metric invariance ####
             }, metric = {

               print.score <- print.object$score$metric

               ### Scalar invariance ####
             }, scalar = {

               print.score <- print.object$score$scalar

             })

      #### Remove p-value and df
      print.score <- print.score[, -which(names(print.score) %in% c("df", "pvalue"))]

      #### Score tests not available
      if (isTRUE(is.null(print.score))) {

        cat("\n  Modification indices for parameter constraints are not available.\n")

        #### Score tests available
      } else {

        cat("\n  Modification Indices for Parameter Constraints\n\n")

        # Filter score tests
        print.score <- print.score[which(print.score[, "mi"] >= x$args$mod.minval), ]

        ##### Round
        print.score[, c("mi", "lhs.epc", "rhs.epc", "lhs.stdyx", "rhs.stdyx")] <- lapply(print.score[, c("mi", "lhs.epc", "rhs.epc", "lhs.stdyx", "rhs.stdyx")], function(y) ifelse(!is.na(y), formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

        if (isTRUE(nrow(print.score) == 0L)) {

          cat(paste0("   No modification indices for parameter constraints above the minimum value ", round(x$args$mod.minval, digits = 2L), ".\n"))

          #### Score test
        } else {

          ##### Column names
          print.score <- rbind(c("Label", "lhs", "op", "rhs", "MI", "lhs EPC", "rhs EPC", "lhs StdYX", "rhs StdYX"), print.score)

          ##### Add blank spaces
          print.score[, 1L] <- paste("  ", print.score[, 1L])

          print.score[-1L, 1L] <- paste0(" ", print.score[-1L, 1L])

          ##### Justify
          print.score[, "label"] <- format(print.score[, "label"], justify = "left")
          print.score[, c("lhs", "op", "rhs")] <- format(print.score[, c("lhs", "op", "rhs")], justify = "centre")
          print.score[, -c(1L:4L)] <- apply(print.score[, -c(1L:4L), drop = FALSE], 2L, function(y) format(y, justify = "right"))

          #...................
          ### Print ####

          write.table(print.score, quote = FALSE, row.names = FALSE, col.names = FALSE)

          # Note
          cat(paste0("\n   Note. Minimum value for printing modification indices is ", round(x$args$mod.minval, digits = 2L), "\n"))

        }

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Residual Correlation Matrix ####

    if (isTRUE("resid" %in% x$args$print)) {

      #...................
      ### Level of measurement invariance ####
      switch(x$args$invar,
             ### Configural invariance ####
             config = {

               cat("\n Residual Correlation Matrix: Configural Invariance Model\n")
               print.resid <- print.object$resid$config

               ### Metric invarianc e ####
             }, metric = {

               cat("\n  Residual Correlation Matrix: Metric Invariance Model\n")
               print.resid <- print.object$resid$metric

               ### Scalar invariance ####
             }, scalar = {

               cat("\n  Residual Correlation Matrix: Scalar Invariance Model\n")
               print.resid <- print.object$resid$scalar

               ### Strict invariance ####
             }, strict = {

               cat("\n  Residual Correlation Matrix: Strict Invariance Model\n")
               print.resid <- print.object$resid$strict

             })

      ### Residual correlation matrix not available
      if (isTRUE(is.null(unlist(print.resid)))) {

        cat("   Residual correlation matrix is not available.")

        ### Residual correlation matrix available
      } else {

        #### Numbers and unicode
        numbers <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
        unicode <- c("\U1D7CE", "\U1D7CF", "\U1D7D0", "\U1D7EF", "\U1D7F0", "\U1D7F1", "\U1D7F2", "\U1D7F3", "\U1D7F4", "\U1D7F5")

        #### Round
        print.resid <- lapply(print.resid, function(y) ifelse(!is.na(y), formatC(y, digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0")), NA))

        #### Lower triangular
        for (i in seq_along(print.resid)) { print.resid[[i]][upper.tri(print.resid[[i]])] <- "" }

        #### Correlation coefficients in boldface
        if (isTRUE(is.null(getOption("knitr.in.progress")) && x$args$resid.minval != 1L)) { for (i in seq_along(print.resid)) { print.resid[[i]][lower.tri(print.resid[[i]])][which(abs(x$result$resid[[x$args$invar]][[i]][lower.tri(x$result$resid[[x$args$invar]][[i]])]) > x$args$resid.minval)] <- na.omit(misty::chr.gsub(numbers, unicode, print.resid[[i]][lower.tri(print.resid[[i]])][which(abs(x$result$resid[[x$args$invar]][[i]][lower.tri(x$result$resid[[x$args$invar]][[i]])]) > x$args$resid.minval)])) } }

        #### Separate residual mean
        for (i in seq_along(print.resid)) { print.resid[[i]] <- rbind(print.resid[[i]][1L:(nrow(print.resid[[i]]) - 1L), ], rep("", times = ncol(print.resid[[i]])), Mean = print.resid[[i]][nrow(print.resid[[i]]), ]) }

        #### Column names
        print.resid <- lapply(print.resid, function(y) rbind(colnames(y), y))
        print.resid <- lapply(print.resid, function(y) cbind(rownames(y), y))

        #### Add blank spaces
        for (i in seq_along(print.resid)) { print.resid[[i]][, 1L] <- paste("   ", print.resid[[i]][, 1L]) }

        #### Justify
        for (i in seq_along(print.resid)) {

          print.resid[[i]][, 1L] <- format(print.resid[[i]][, 1L], justify = "left")
          print.resid[[i]][, -1L] <- format(print.resid[[i]][, -1L], justify = "right")

        }

        #...................
        ### Print ####

        cat("\n   Within Level\n")
        write.table(print.resid$within, quote = FALSE, row.names = FALSE, col.names = FALSE)

        cat("\n   Betweem Level\n")
        write.table(print.resid$between, quote = FALSE, row.names = FALSE, col.names = FALSE)

      }

    }

  #_____________________________________________________________________________
  #
  # Multilevel Composite Reliability ----
  }, multilevel.omega = {

    switch(x$args$const,
           within = cat(" Multilevel Composite Reliability for a Within-Cluster Construct\n"),
           shared = cat(" Multilevel Composite Reliability for a Shared Cluster-Level Construct\n"),
           config = cat(" Multilevel Composite Reliability for a Configural Construct\n"))

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Omega ####
    if (isTRUE("omega" %in% x$args$print)) {

      print.omega <- print.object$omega

      #### Round ####
      print.omega[, -c(1L:2L)] <- lapply(print.omega[, -c(1L:2L)], function(y) ifelse(!is.na(y), formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

      #...................
      ### Greek letters ####

      # R Markdown not in progress
      if (isTRUE(is.null(getOption("knitr.in.progress")))) {

        print.omega[, 1] <- sub("omega.w", "\U03C9\U02B7", print.omega[, 1])
        print.omega[, 1] <- sub("omega.b", "\U03C9\U1D47", print.omega[, 1])
        print.omega[, 1] <- sub("omega.2l", "\U03C9\U00B2\U02E1", print.omega[, 1])

      }

      # Include spaces
      print.omega[, 1L] <- paste0("    ", print.omega[, 1L])

      # Include labels
      print.omega <- rbind(c("   Type", "Items", "Omega", "Low", "Upp"), print.omega)

      # Justify left
      print.omega[, 1L] <- format(print.omega[, 1L], justify = "left")

      # Justify right
      print.omega[, -1L] <- format(print.omega[, -1L], justify = "right")


      # Print
      x$args$conf.level*100L

      cat(paste0("\n  Coefficient Omega with ", x$args$conf.level*100L, "% Confidence Interval\n\n"))

      write.table(print.omega, quote = FALSE, row.names = FALSE, col.names = FALSE)

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Item Statistics ####

    if (isTRUE("item" %in% x$args$print)) {

      print.item <- print.object$item

      #### Round ####

      # Variables to round
      print.round <- switch(x$args$const,
                            within = c("pNA", "m", "sd", "min", "max", "skew", "kurt", "ICC", "wstd.ld"),
                            shared = c("pNA", "m", "sd", "min", "max", "skew", "kurt", "ICC", "bstd.ld"),
                            config = c("pNA", "m", "sd", "min", "max", "skew", "kurt", "ICC", "wstd.ld", "bstd.ld"))

      print.item[, print.round] <- sapply(print.round, function(y) ifelse(!is.na(print.item[, y]), formatC(print.item[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

      #...................
      ### Format ####

      # Percentages
      print.item[, "pNA"] <- paste0(print.item[, "pNA"], "%")

      # Column names
      print.item <- rbind(switch(x$args$const,
                                 within = c("Variable", "n", "nNA", "pNA", "M", "SD", "Min", "Max",  "Skew", "Kurt", "ICC(1)", "WStd.ld"),
                                 shared = c("Variable", "n", "nNA", "pNA", "M", "SD", "Min", "Max",  "Skew", "Kurt", "ICC(1)", "BStd.ld"),
                                 config = c("Variable", "n", "nNA", "pNA", "M", "SD", "Min", "Max",  "Skew", "Kurt", "ICC(1)", "WStd.ld", "BStd.ld")), print.item)

      # Justify left and right
      print.item[, 1L] <- format(print.item[, 1L, drop = FALSE], justify = "left")
      print.item[, -1L] <- apply(print.item[, -1L, drop = FALSE], 2L, function(y) format(y, justify = "right"))

      # Add blank space
      print.item[, "variable"] <- c(paste0("   ", print.item[1L, "variable"], collapse = ""), paste0("    ", print.item[-1L, "variable"]))
      print.item[, "variable"] <- format(c(print.item[1L, "variable"], misty::chr.trim(print.item[-1L, "variable"], side = "right")), justify = "left")

      #...................
      ### Print ####

      switch(x$args$const,
             within = cat("\n  Standardized Factor Loadings at the Within Level\n\n"),
             shared = cat("\n  Standardized Factor Loadings at the Between Level\n\n"),
             config = cat("\n  Standardized Factor Loadings at the Within and Between Level\n\n"))

      write.table(print.item, quote = FALSE, row.names = FALSE, col.names = FALSE)

    }

  #_____________________________________________________________________________
  #
  # R-Squared Measures for Multilevel and Linear Mixed Effects Models Manual ----
  }, multilevel.r2.manual = {

    print.object$total <- data.frame(sapply(print.object$total[, !is.na(print.object$total)], formatC, digits = digits, format = "f", simplify = FALSE))
    row.names(print.object$total) <- "   "

    if (isTRUE(ncol(print.object$decomp) != 1L)) {

      print.object$within <- data.frame(sapply(print.object$within, formatC, digits = digits, format = "f", simplify = FALSE))
      row.names(print.object$within) <- "   "

      print.object$between <- data.frame(sapply(print.object$between, formatC, digits = digits, format = "f", simplify = FALSE))
      row.names(print.object$between) <- "   "

    }

    ####################################################################################
    # Output

    cat(" R-Squared Measures for Multilevel and Linear Mixed Effects Models\n\n")

    cat("  Integrative Framework of R-Squared Measures (Rights and Sterba, 2019)\n\n")

    cat(ifelse(isTRUE(getOption("knitr.in.progress")), "   Total R2\n", "   Total R\u00B2\n"))

    print(print.object$total, quote = FALSE, max = 99999L, right = TRUE)

    # Predictors are cluster-mean-centered
    if (isTRUE(ncol(print.object$decomp) != 1L)) {

      cat(ifelse(isTRUE(getOption("knitr.in.progress")), "\n   Within-Cluster R2\n", "\n   Within-Cluster R\u00B2\n"))

      print(print.object$within, quote = FALSE, max = 99999L, right = TRUE)

      cat(ifelse(isTRUE(getOption("knitr.in.progress")), "\n   Between-Cluster R2\n", "\n   Between-Cluster R\u00B2\n"))

      print(print.object$between, quote = FALSE, max = 99999L, right = TRUE)

    }

  #_____________________________________________________________________________
  #
  # Auxiliary variables analysis -----------------------------------------------
  }, na.auxiliary = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Product-Moment Correlation matrix and Cohen's d Matrix ####

    if (isTRUE(is.null(x$args$model))) {

      #-----------------------------------------
      # Format correlation matrix

      print.object$cor <- apply(print.object$cor, 2L, function(y) formatC(as.numeric(y), digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")))

      # Lower and/or upper triangular
      switch(tri, "lower" = {

        print.object$cor[upper.tri(print.object$cor)] <- ""

      }, "upper" = {

        print.object$cor[lower.tri(print.object$cor)] <- ""

      })

      diag(print.object$cor) <- ""

      #-----------------------------------------
      # Format Cohen's d matrix

      print.object$d <- apply(print.object$d, 2L, function(y) formatC(as.numeric(y), digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")))

      # Print table
      print.object  <- data.frame(cbind(c("", colnames(print.object$cor), "", row.names(x$result$d)),
                                        rbind(colnames(print.object$cor), print.object$cor, colnames(print.object$cor), print.object$d)),
                                  stringsAsFactors = FALSE)

      # NA
      print.object <- sapply(print.object, function(y) gsub("NA", "", y))

      # Format
      print.object[, 1L] <- paste0("    ", print.object[, 1L])

      print.object[, 1L] <- format(print.object[, 1L], justify = "left")

      print.object[, -1L] <- apply(print.object[, -1L], 2L, function(y) format(y, justify = "right"))

      #-----------------------------------------
      # Output

      cat(" Auxiliary Variables Analysis\n\n",
          " Variables Related to the Incomplete Variable\n\n",
          "  Pearson product-moment correlation matrix\n")

      write.table(print.object[1L:(nrow(x$result$cor) + 1L), ], quote = FALSE, row.names = FALSE, col.names = FALSE)

      cat("\n")

      cat("  Variables Related to the Probability of Missigness\n\n",
          "  Cohen's d\n")

      write.table(print.object[(nrow(x$result$cor) + 2L):nrow(print.object), ], quote = FALSE, row.names = FALSE, col.names = FALSE)

      cat("\n", " Note. Indicator variables are in the rows (0 = obs, 1 = miss)\n")

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Semi-Partial Correlation Coefficients ####
    } else {

      #...................
      ### Extract Results ####

      # Standardized Solution
      print.object <- x$model.fit.stand

      # Outcome variable
      outcome <- setdiff(all.vars(as.formula(x$args$model)), attr(terms(as.formula(x$args$model)[-2L]), "term.labels"))

      # Select outcome rows
      print.object <- print.object[print.object$lhs == outcome, ]

      # Indices substantive model
      model.sub <- which(print.object$op == "~")

      # Indices auxiliary model
      model.aux <- which(print.object$op == "~~" & (print.object$lhs != print.object$rhs))

      #...................
      ### Round ####

      # Variables to round
      print.round <- c("est.std", "se", "z", "ci.lower", "ci.upper")

      print.object[, print.round] <- sapply(print.round, function(y) ifelse(!is.na(print.object[, y]), formatC(print.object[, y], digits = digits, format = "f", zero.print = ifelse(x$args$digits > 0L, paste0("0.", paste(rep(0L, times = x$args$digits), collapse = "")), "0")), NA))
      print.object$pvalue <- formatC(print.object$pval, digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

      #...................
      ### Format ####

      print.object <- rbind(c("", "", "", "Estimate", "Std.Err", "z-value", "P(>|z|)", "Low", "Upp"), print.object)

      print.object[, 1L] <- paste("   ", print.object[, 1L])

      print.object <- apply(print.object, 2L, format, justify = "right")

      #...................
      ### Print Output ####

      cat(" Auxiliary Variables\n\n",
          " Variables Related to the Incomplete Variable\n\n",
          "  Substantive model: Standardized slope\n")

      write.table(print.object[c(1L, model.sub + 1L), ], quote = FALSE, row.names = FALSE, col.names = FALSE, na = "")

      cat("\n   Auxiliary model: Semi-partial correlation coefficient\n")

      write.table(print.object[c(1L, model.aux + 1L), ], quote = FALSE, row.names = FALSE, col.names = FALSE, na = "")

    }

  #_____________________________________________________________________________
  #
  # Variance-Covariance Coverage -----------------------------------------------
  }, na.coverage = {

    #........................................
    # Lower and/or upper triangular

    switch(tri, "lower" = {

      print.object[upper.tri(print.object)] <- ""

    }, "upper" = {

      print.object[lower.tri(print.object)] <- ""

    })

    # Format proportions
    print.object <- apply(print.object, 2L, function(y) ifelse(!is.na(as.numeric(y)), formatC(as.numeric(y), digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), ""))

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

    #----------------------------------------
    # Result table

    #...................
    ### Level-1 Variables ####

    # At least one Level-1 variable
    if (isTRUE(any(!is.na(unlist(print.object$L1[-1L]))))) {

    restab.l1 <- data.frame(statistic = c("No. of cases", "No. of complete cases", "No. of incomplete cases", "No. of values",
                                          "No. of observed values", "No. of missing values", "No. of variables",
                                          "Mean", "SD", "Minimum", "Maximum"),
                            no = c(print.object$L1$no.cases.l1, print.object$L1$no.complete.l1, print.object$L1$no.incomplete.l1,
                                   print.object$L1$no.values.l1, print.object$L1$no.observed.values.l1, print.object$L1$no.missing.values.l1,
                                   print.object$L1$no.var.l1, print.object$L1$no.missing.mean.l1, print.object$L1$no.missing.sd.l1,
                                   print.object$L1$no.missing.min.l1, print.object$L1$no.missing.max.l1),
                            perc = c("", print.object$L1$perc.complete.l1, print.object$L1$perc.incomplete.l1, "", print.object$L1$perc.observed.values.l1,
                                     print.object$L1$perc.missing.values.l1, "", print.object$L1$perc.missing.mean.l1, print.object$L1$perc.missing.sd.l1,
                                     print.object$L1$perc.missing.min.l1, print.object$L1$perc.missing.max.l1),
                            stringsAsFactors = FALSE)

    ##### Format
    restab.l1$statistic <- paste0(ifelse(x$no.cluster == "none", "  ",  "   "), restab.l1$statistic)

    restab.l1$statistic[8L:11L] <- paste0("  ", restab.l1$statistic[8L:11L] )
    restab.l1$statistic <- format(restab.l1$statistic, justify = "left", width = max(nchar(restab.l1$statistic)) + 1L)

    restab.l1$no[8L:11L] <- format(formatC(as.numeric(restab.l1$no[8L:11L]), digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), justify = "right")
    restab.l1$no[1L:7L] <- format(formatC(as.numeric(restab.l1$no[1L:7L]), digits = 0L, format = "f"), justify = "right")
    restab.l1$no <- format(restab.l1$no, justify = "right")

    restab.l1$perc[restab.l1$perc != ""] <- paste0("(", formatC(as.numeric(restab.l1$perc[restab.l1$perc != ""]), digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), "%)")

    restab.l1$perc <- format(restab.l1$perc, width = max(nchar(restab.l1$perc)), justify = "right")

    # No Level-1 variable
    } else {

      restab.l1 <- NULL

    }

    #...................
    ### Level-2 Variables ####

    if (isTRUE(x$no.cluster != "none")) {

      # At least one Level-2 variable
      if (isTRUE(any(!is.na(unlist(print.object$L2[-1L]))))) {

        restab.l2 <- data.frame(statistic = c("No. of clusters", "No. of complete cases", "No. of incomplete cases", "No. of values",
                                              "No. of observed values", "No. of missing values", "No. of variables",
                                              "Mean", "SD", "Minimum", "Maximum"),
                                no = c(print.object$L2$no.cluster.l2, print.object$L2$no.complete.l2, print.object$L2$no.incomplete.l2,
                                       print.object$L2$no.values.l2, print.object$L2$no.observed.values.l2, print.object$L2$no.missing.values.l2,
                                       print.object$L2$no.var.l2, print.object$L2$no.missing.mean.l2, print.object$L2$no.missing.sd.l2,
                                       print.object$L2$no.missing.min.l2, print.object$L2$no.missing.max.l2),
                                perc = c("", print.object$L2$perc.complete.l2, print.object$L2$perc.incomplete.l2, "", print.object$L2$perc.observed.values.l2,
                                         print.object$L2$perc.missing.values.l2, "", print.object$L2$perc.missing.mean.l2, print.object$L2$perc.missing.sd.l2,
                                         print.object$L2$perc.missing.min.l2, print.object$L2$perc.missing.max.l2),
                                stringsAsFactors = FALSE)

        ##### Format
        restab.l2$statistic <- paste0("   ", restab.l2$statistic)

        restab.l2$statistic[8L:11L] <- paste0("  ", restab.l2$statistic[8L:11L] )
        restab.l2$statistic <- format(restab.l2$statistic, justify = "left", width = max(nchar(restab.l2$statistic)) + 1L)

        restab.l2$no[8L:11L] <- format(formatC(as.numeric(restab.l2$no[8L:11L]), digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), justify = "right")
        restab.l2$no[1L:7L] <- format(formatC(as.numeric(restab.l2$no[1L:7L]), digits = 0L, format = "f"), justify = "right")
        restab.l2$no <- format(restab.l2$no, justify = "right")

        restab.l2$perc[restab.l2$perc != ""] <- paste0("(", formatC(as.numeric(restab.l2$perc[restab.l2$perc != ""]), digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), "%)")

        restab.l2$perc <- format(restab.l2$perc, width = max(nchar(restab.l2$perc)), justify = "right")

      # No Level-2 variable
      } else {

        restab.l2 <- NULL

      }

    }

    #...................
    ### Level-3 Variables ####

    if (isTRUE(x$no.cluster == "two")) {

      # At least one Level-3 variable
      if (isTRUE(all(!is.na(unlist(print.object$L3))))) {

        restab.l3 <- data.frame(statistic = c("No. of clusters", "No. of complete cases", "No. of incomplete cases", "No. of values",
                                              "No. of observed values", "No. of missing values", "No. of variables",
                                              "Mean", "SD", "Minimum", "Maximum"),
                                no = c(print.object$L3$no.cluster.l3, print.object$L3$no.complete.l3, print.object$L3$no.incomplete.l3,
                                       print.object$L3$no.values.l3, print.object$L3$no.observed.values.l3, print.object$L3$no.missing.values.l3,
                                       print.object$L3$no.var.l3, print.object$L3$no.missing.mean.l3, print.object$L3$no.missing.sd.l3,
                                       print.object$L3$no.missing.min.l3, print.object$L3$no.missing.max.l3),
                                perc = c("", print.object$L3$perc.complete.l3, print.object$L3$perc.incomplete.l3, "", print.object$L3$perc.observed.values.l3,
                                         print.object$L3$perc.missing.values.l3, "", print.object$L3$perc.missing.mean.l3, print.object$L3$perc.missing.sd.l3,
                                         print.object$L3$perc.missing.min.l3, print.object$L3$perc.missing.max.l3),
                                stringsAsFactors = FALSE)

        ##### Format
        restab.l3$statistic <- paste0("   ", restab.l3$statistic)

        restab.l3$statistic[8L:11L] <- paste0("   ", restab.l3$statistic[8L:11L] )
        restab.l3$statistic <- format(restab.l3$statistic, justify = "left", width = max(nchar(restab.l3$statistic)) + 1L)

        restab.l3$no[8L:11L] <- format(formatC(as.numeric(restab.l3$no[8L:11L]), digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), justify = "right")
        restab.l3$no[1L:7L] <- format(formatC(as.numeric(restab.l3$no[1L:7L]), digits = 0L, format = "f"), justify = "right")
        restab.l3$no <- format(restab.l3$no, justify = "right")

        restab.l3$perc[restab.l3$perc != ""] <- paste0("(", formatC(as.numeric(restab.l3$perc[restab.l3$perc != ""]), digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), "%)")

        restab.l3$perc <- format(restab.l3$perc, width = max(nchar(restab.l3$perc)), justify = "right")

      # No Level-3 variable
      } else {

        restab.l3 <- NULL

      }

    }

    #----------------------------------------
    # Output

    cat(" Descriptive Statistics for Missing Data\n\n")

    #...................
    ### Level-1 Variables ####

    if (isTRUE(x$no.cluster != "none")) { cat("  Level 1\n") }

    # At least one Level-1 variable
    if (isTRUE(!is.null(restab.l1))) {

      write.table(restab.l1[1L:3L, ], quote = FALSE, row.names = FALSE, col.names = FALSE)

      cat("\n")
      write.table(restab.l1[4L:6L, ], quote = FALSE, row.names = FALSE, col.names = FALSE)

      cat("\n")
      write.table(restab.l1[7L, ], quote = FALSE, row.names = FALSE, col.names = FALSE)
      if (isTRUE(x$no.cluster != "none")) { cat("   No. of missing values across all variables\n") } else { cat("  No. of missing values across all variables\n") }
      write.table(restab.l1[8L:11L, ], quote = FALSE, row.names = FALSE, col.names = FALSE)

      #----------------------------------------
      # Frequency table
      if (isTRUE(table)) {

        freqtab <- x$result$L1$table.miss

        freqtab[, c("pObs", "pNA")] <- apply(freqtab[, c("pObs", "pNA")], 2L, function(y) paste0(formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), "%"))
        freqtab <- rbind(colnames(freqtab), freqtab)

        freqtab[, -1L] <- apply(freqtab[ -1L], 2L, format, justify = "right")

        freqtab[, 1L] <- paste0(ifelse(x$no.cluster == "none", "    ", "     "), format(freqtab[, 1L], justify = "left"))

        cat("\n")
        write.table(freqtab, quote = FALSE, row.names = FALSE, col.names = FALSE)

      }

    } else {

      cat("   There are no Level-1 variables\n")

    }

    #...................
    ### Level-2 Variables ####

    if (isTRUE(x$no.cluster != "none")) {

      # Cluster variable
      cat("\n  Level 2\n")

      # At least one Level-2 variable
      if (isTRUE(!is.null(restab.l2))) {

        write.table(restab.l2[1L:3L, ], quote = FALSE, row.names = FALSE, col.names = FALSE)

        cat("\n")
        write.table(restab.l2[4L:6L, ], quote = FALSE, row.names = FALSE, col.names = FALSE)

        cat("\n")
        write.table(restab.l2[7L, ], quote = FALSE, row.names = FALSE, col.names = FALSE)
        cat("   No. of missing values across all variables\n")
        write.table(restab.l2[8L:11L, ], quote = FALSE, row.names = FALSE, col.names = FALSE)

        #----------------------------------------
        # Frequency table
        if (isTRUE(table)) {

          freqtab <- x$result$L2$table.miss

          freqtab[, c("pObs", "pNA")] <- apply(freqtab[, c("pObs", "pNA")], 2L, function(y) paste0(formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), "%"))
          freqtab <- rbind(colnames(freqtab), freqtab)

          freqtab[, -1L] <- apply(freqtab[ -1L], 2L, format, justify = "right")

          freqtab[, 1L] <- paste0("     ", format(freqtab[, 1L], justify = "left"))

          cat("\n")
          write.table(freqtab, quote = FALSE, row.names = FALSE, col.names = FALSE)

        }

      } else {

        cat("   There are no Level-2 variables\n")

      }

    }

    #...................
    ### Level-3 Variables ####

    if (isTRUE(x$no.cluster == "two")) {

      # Cluster variable
      cat("\n  Level 3\n")

      # At least one Level-3 variable
      if (isTRUE(!is.null(restab.l3))) {

        write.table(restab.l3[1L:3L, ], quote = FALSE, row.names = FALSE, col.names = FALSE)

        cat("\n")
        write.table(restab.l3[4L:6L, ], quote = FALSE, row.names = FALSE, col.names = FALSE)

        cat("\n")
        write.table(restab.l3[7L, ], quote = FALSE, row.names = FALSE, col.names = FALSE)
        cat("   No. of missing values across all variables\n")
        write.table(restab.l3[8L:11L, ], quote = FALSE, row.names = FALSE, col.names = FALSE)

        #----------------------------------------
        # Frequency table
        if (isTRUE(table)) {

          freqtab <- x$result$L3$table.miss

          freqtab[, c("pObs", "pNA")] <- apply(freqtab[, c("pObs", "pNA")], 2L, function(y) paste0(formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), "%"))
          freqtab <- rbind(colnames(freqtab), freqtab)

          freqtab[, -1L] <- apply(freqtab[ -1L], 2L, format, justify = "right")

          freqtab[, 1L] <- paste0("     ", format(freqtab[, 1L], justify = "left"))

          cat("\n")
          write.table(freqtab, quote = FALSE, row.names = FALSE, col.names = FALSE)

        }

      } else {

        cat("   There are no Level-3 variables")

      }

    }

  #_____________________________________________________________________________
  #
  # Missing Data Pattern -------------------------------------------------------
  }, na.pattern = {

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


    #---------------------------------------------------------------------------
    # Output

    cat(" Missing Data Pattern\n\n")

    print(print.object, row.names = FALSE, max = 99999L, right = TRUE)

  #_____________________________________________________________________________
  #
  # Missing Completely at Random (MCAR) Test -----------------------------------
  }, na.test = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Little's MCAR Test ####

    if (isTRUE("little" %in% x$args$print)) {

      #...................
      ### Round ####

      print.object$little$statistic <- formatC(print.object$little$statistic, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))
      print.object$little$pval <- formatC(print.object$little$pval, digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

      #...................
      ### Format ####

      print.object$little <- rbind(c("n", "nIncomp", "nPattern", "chi2", "df", "pval"), print.object$little)

      print.object$little <- apply(print.object$little, 2L, format, justify = "right")

      # R Markdown in progress
      if (isTRUE(is.null(getOption("knitr.in.progress")))) {

        print.object$little[1L, "statistic"] <- paste0(paste0(rep(" ", times = nchar(print.object$little[1L, "statistic"]) - 2L), collapse = ""), "\u03C7\u00B2", collapes = "")

      }

      print.object$little[, 1L] <- paste(" ", print.object$little[, 1L])

      #...................
      ### Print Output ####

      cat(paste0(" Little's MCAR Test\n\n"))

      cat(paste(print.object$little[1L, ], collapse = " "), "\n")
      write.table(print.object$little[-1L, , drop = FALSE], quote = FALSE, row.names = FALSE, col.names = FALSE, na = "")

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Jamshidian and Jalal's Approach ####

    if (isTRUE("jamjal" %in% x$args$print)) {

      #...................
      ### Round ####

      print.object$hawkins$statistic <- formatC(print.object$hawkins$statistic, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))
      print.object$hawkins$pval <- formatC(print.object$hawkins$pval, digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

      print.object$anderson$statistic <- formatC(print.object$anderson$statistic , digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))
      print.object$anderson$pval <- formatC(print.object$anderson$pval, digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

      #...................
      ### Format ####

      print.object$hawkins <- rbind(c("n", "nIncomp", "nPattern", "chi2", "df", "pval"), print.object$hawkins)
      print.object$hawkins <- apply(print.object$hawkins, 2L, format, justify = "right")

      print.object$anderson <- rbind(c("n", "nIncomp", "nPattern", "T", "pval"), print.object$anderson)
      print.object$anderson <- apply(print.object$anderson, 2L, format, justify = "right")

      # R Markdown in progress
      if (isTRUE(is.null(getOption("knitr.in.progress")))) { print.object$hawkins[1L, "statistic"] <- paste0(paste0(rep(" ", times = nchar(print.object$hawkins[1L, "statistic"]) - 2L), collapse = ""), "\u03C7\u00B2", collapes = "") }

      print.object$hawkins[, 1L] <- paste("   ", print.object$hawkins[, 1L])
      print.object$anderson[, 1L] <- paste("   ", print.object$anderson[, 1L])

      #...................
      ### Print Output ####

      if (isTRUE("little" %in% x$args$print)) { cat("\n") }

      cat(paste0(" Jamshidian and Jalal's Approach for Testing MCAR\n\n"))

      cat(paste0("  Imputation:         ", ifelse(!is.null(x$args$impdat), "Imputed Data Provided", misty::rec(x$args$method, spec = "'npar' = 'Non-Parametric'; 'normal' = 'Parametric Normal'"))), "\n")
      cat(paste0("  Nr. of Imputations: ", x$args$m), "\n")
      cat(paste0("  Pooling:            ", misty::rec(x$args$pool, spec = "'m' = 'Mean'; 'med' = 'Median'; 'min' = 'Minimum'; 'max' = 'Maximum'; 'random' = 'Random'")), "\n\n")

      #### Hawkins Test ####
      cat(paste0("  Hawkins Test\n\n"))

      if (isTRUE(is.null(getOption("knitr.in.progress")))) {

        cat("   H\u2080: Multivariate Normality and Homogeneity of Covariances (MCAR)\n")
        cat("   H\u2081: Multivariate Non-Normality or Heterogeneity of Covariances (MCAR Violation)\n\n")

      } else {

        cat("   H0: Multivariate Normality and Homogeneity of Covariances (MCAR)\n")
        cat("   H1: Multivariate Non-Normality or Heterogeneity of Covariances (MCAR Violation)\n\n")

      }

      cat(paste(print.object$hawkins[1L, ], collapse = " "), "\n")
      write.table(print.object$hawkins[-1L, , drop = FALSE], quote = FALSE, row.names = FALSE, col.names = FALSE, na = "")

      #### Anderson-Darling Non-Parametric Test ####
      if (isTRUE(x$result$hawkins$pval <= x$args$alpha)) {

        cat(paste0("\n  Anderson-Darling Non-Parametric Test\n\n"))

        if (isTRUE(is.null(getOption("knitr.in.progress")))) {

          cat("   H\u2080: Multivariate Non-Normality, but Homogeneity of Covariances (MCAR)\n")
          cat("   H\u2081: Heterogeneity of Covariances (MCAR Violation)\n\n")

        } else {

          cat("   H0: Multivariate Non-Normality, but Homogeneity of Covariances (MCAR)\n")
          cat("   H1: Heterogeneity of Covariances (MCAR Violation)\n\n")

        }

        cat(paste(print.object$anderson[1L, ], collapse = " "), "\n")
        write.table(print.object$anderson[-1L, , drop = FALSE], quote = FALSE, row.names = FALSE, col.names = FALSE, na = "")

      }

    }

  #_____________________________________________________________________________
  #
  # Heteroscedasticity-Consistent Standard Errors ------------------------------
  }, robust.coef = {

    #...................
    ### Extract coefficients ####

    print.coef <- print.object$coef

    #...................
    ### Round ####

    print.coef[, -4L] <- apply(print.coef[, -4L], 2L, function(y) formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")))

    print.coef[, 4L] <- formatC(as.numeric(print.coef[, 4L]), digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

    #...................
    ### Format ####

    print.coef <- apply(print.coef, 2L, function(y) format(y, justify = "right"))

    row.names(print.coef) <- paste("  ", row.names(print.coef))

    #...................
    ### Print ####

    cat(paste0("  Heteroscedasticity-Consistent Standard Errors (", x$args$type, ")\n\n"))

    # Print coefficients
    print(print.coef, row.names = FALSE, quote = FALSE, right = TRUE, max = 99999L)

    #...................
    ### F-test ####

    if (isTRUE(length(class(x$model)) == 1L)) {

      # Extract F-test
      print.F <- print.object$F.test[2L, ]

      # Round
      print.F["F"] <- formatC(print.F["F"], digits = digits - 1L, format = "f", zero.print = ifelse(digits - 1L > 0L, paste0("0.", paste(rep(0L, times = digits - 1L), collapse = "")), "0"))
      print.F["pval"] <- formatC(as.numeric(print.F["pval"]), digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

      # Model summary
      print.summary <- summary(x$model)

      # Negative r-squared
      if (isTRUE(print.summary$adj.r.squared < 0L)) { print.summary$adj.r.squared <- 0L }

      # Round
      print.summary$sigma <- formatC(print.summary$sigma, digits = digits - 1L, format = "f", zero.print = ifelse(digits - 1L > 0L, paste0("0.", paste(rep(0L, times = digits - 1L), collapse = "")), "0"))
      print.summary$r.squared <- formatC(print.summary$r.squared, digits = p.digits - 1L, format = "f", zero.print = ifelse(p.digits - 1L > 0L, paste0("0.", paste(rep(0L, times = p.digits - 1L), collapse = "")), "0"))
      print.summary$adj.r.squared <- formatC(print.summary$adj.r.squared, digits = p.digits - 1L, format = "f", zero.print = ifelse(p.digits - 1L > 0L, paste0("0.", paste(rep(0L, times = p.digits - 1L), collapse = "")), "0"))

      cat(paste0("\n   Residual standard error: ", print.summary$sigma, " on ", x$model$df.residual, " degrees of freedom\n",
                 paste0("   Multiple R-squared: ", print.summary$r.squared, ",", " Adjusted R-squared: ", print.summary$adj.r.squared , "\n",
                        paste0("   Robust F-statistic: ", print.F["F"], " on ",  print.object$F.test[2L, "Df"], " and ", print.object$F.test[2L, "Res.Df"], " df, p-value: ", print.F["pval"]))))

    }

  #_____________________________________________________________________________
  #
  # Result Table for Latent Profile Analysis Estimated in Mplus ----------------
  }, result.lca = {

    cat(" Latent Class Analysis\n\n")

    #...................
    ### Print object ####

    print.object <- print.object$summary

    #...................
    ### Round ####

    tests <- intersect(c("chi.pear", "chi.lrt", "lmr.lrt", "almr.lrt", "blrt", "entropy"), colnames(print.object))

    print.object[, c("LL", "aic", "caic", "bic", "sabic")] <- apply(print.object[, c("LL", "aic", "caic", "bic", "sabic")], 2L, function(y) formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")))
    print.object[, "LL.scale"] <- formatC(print.object[, "LL.scale"], digits = digits + 1L, format = "f", zero.print = ifelse(digits + 1L > 0L, paste0("0.", paste(rep(0L, times = digits + 1L), collapse = "")), "0"))
    print.object[, c(tests, colnames(print.object)[substr(colnames(print.object), 1L, 1L) == "p"])] <- apply(print.object[, c(tests, colnames(print.object)[substr(colnames(print.object), 1L, 1L) == "p"]), drop = FALSE], 2L, function(y) formatC(y, digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0")))

    #...................
    ### Additional folder row ####

    print.temp <- NULL
    for (i in unique(print.object$folder)) {

      print.temp <- rbind(print.temp,
                          setNames(do.call(data.frame, list(i, rep(list(NA), times = ncol(print.object) - 1L))), nm = colnames(print.object)),
                          print.object[print.object$folder == i, ])

    }

    print.object <- print.temp

    #...................
    ### Column names ####

    tests <- misty::rec(tests, spec = "'lmr.lrt' = 'LMR-LRT'; 'almr.lrt' = 'A-LRT'; 'blrt' = 'BLRT'; 'chi.pear' = 'Chi-Pear'; 'chi.lrt' = 'Chi-LRT'; 'entropy' = 'Entropy'")

    print.object <- rbind(c("Folder", "#Prof", "Conv", "#Param", "logLik", "Scale", "LL Rep", "AIC", "CAIC", "BIC", "SABIC",
                            misty::rec(tests, spec = "'lmr.lrt' = 'LMR-LRT'; 'almr.lrt' = 'A-LRT'; 'blrt' = 'BLRT'; 'chi.pear' = 'Chi-Pear'; 'chi.lrt' = 'Chi-LRT'; 'entropy' = 'Entropy'"),
                            colnames(print.object)[substr(colnames(print.object), 1L, 1L) == "p"]),
                          print.object)

    #...................
    ### Remove duplicated rows ####

    print.object[duplicated(print.object[, "folder"]), "folder"] <- "NA"

    #...................
    ### Replace NA with "" ####

    print.object <- apply(print.object, 2L, function(y) gsub("NA", "", y))
    print.object <- misty::na.as(print.object, na = "", check = FALSE)

    #...................
    ### Add blank space ####

    print.object[1L, "folder"] <- paste0("  ", print.object[1L, "folder"])
    print.object[apply(print.object[, -1L], 1L, function(y) all(y == "")), "folder"] <- paste0("   ", print.object[apply(print.object[, -1L], 1L, function(y) all(y == "")), "folder"])

    #...................
    ### Justify left and right ####

    print.object[apply(print.object[, -1L], 1L, function(y) !all(y == "")), ] <- apply(print.object[apply(print.object[, -1L], 1L, function(y) !all(y == "")), ], 2L, format, justify = "right")

    # Add space
    print.object[, 2L] <- paste0("  ", print.object[, 2L])

    #...................
    ### Print ####

    write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE)

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
      if (isTRUE(posthoc)) {

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

      write.table(print.object, quote = TRUE, row.names = FALSE, col.names = FALSE, na = "")

    })

  })

}
