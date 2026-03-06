#' Print misty.object object
#'
#' This function prints an \code{misty.object} object.
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
#'                    observed values (\code{"nOb"}), percent of observed values
#'                    (\code{"pOb"}), number of missing values (\code{"nNA"}),
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
#' @param horiz       logical: if \code{TRUE}, a horizontal line under the table
#'                    header is drawn.
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
                               sort.var = x$args$sort.var, order = x$args$order,
                               horiz = TRUE, check = TRUE, ...) {

  #_____________________________________________________________________________
  #
  # Global binding -------------------------------------------------------------

  group <- model <- NULL

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(args = c("digits", "p.digits", "icc.digits"), envir = environment(), input.check = check)

  # Additional checks
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

    # Check input 'sort.var'
    if (isTRUE(!is.null(sort.var))) { if (isTRUE(!is.logical(sort.var))) { stop("Please specify TRUE or FALSE for the argument 'sort.var'.", call. = FALSE) } }

    # Check input 'order'
    if (isTRUE(!is.null(order))) { if (isTRUE(!is.logical(order))) { stop("Please specify TRUE or FALSE for the argument 'order'.", call. = FALSE) } }

  }

  #_____________________________________________________________________________
  #
  # Print Object -------------item.nonequi--------------------------------------------------

  print.object <- x$result

  #_____________________________________________________________________________
  #
  # Between-Subject Analysis of Variance (ANOVA), aov.b() ----------------------
  switch(x$type, aov.b = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Round ####

    print.object[["descript"]][, c("m", "low", "upp", "sd", "skew", "kurt")] <- vapply(print.object[["descript"]][, c("m", "low", "upp", "sd", "skew", "kurt")], formatC, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"), FUN.VALUE = character(nrow(print.object[["descript"]])))

    print.object[["test"]][, c("sum.sq", "mean.sq", "F", "eta.sq", "omega.sq")] <- vapply(print.object[["test"]][, c("sum.sq", "mean.sq", "F", "eta.sq", "omega.sq")], formatC, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"), FUN.VALUE = character(nrow(print.object[["test"]])))

    print.object[["test"]][, "pval"] <- formatC(print.object[["test"]][, "pval"], digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

    print.object[["posthoc"]][, c("m.diff", "m.low", "m.upp", "d", "d.low", "d.upp")] <- vapply(print.object[["posthoc"]][, c("m.diff", "m.low", "m.upp", "d", "d.low", "d.upp")], formatC, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"), FUN.VALUE = character(nrow(print.object[["posthoc"]])))

    print.object[["posthoc"]][, "pval"] <- formatC(print.object[["posthoc"]][, "pval"], digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Format ####

    print.object[["descript"]] <- rbind(c("Group", "n", "nNA", "M", "Low", "Upp", "SD", "Skew", "Kurt"), print.object[["descript"]])
    print.object[["test"]] <- rbind(c("Source", "Sum Sq", "df", "Mean Sq", "F", "p", "et", "om"), print.object[["test"]])
    print.object[["posthoc"]] <- rbind(c("Group1", "Group2", "M.diff", "Low", "Upp", "p", "d", "Low", "Upp"), print.object[["posthoc"]])

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

    #...................
    ### Statistical Hypotheses ####

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

    #...................
    ### Descriptive Statistics ####

    if (isTRUE(descript)) {

      .write.table(print.object[["descript"]], right = 3L, horiz = horiz)

      cat("\n")

    }

    #...................
    ### Effect Size ####

    if (isTRUE(!effsize)) {

      print.object[["test"]] <- print.object[["test"]][, -which(colnames(print.object[["test"]]) %in% c("eta.sq", "omega.sq"))]

      print.object[["posthoc"]] <- print.object[["posthoc"]][, -which(colnames(print.object[["posthoc"]]) %in% c("d", "d.low", "d.upp"))]

    }

    .write.table(print.object[["test"]], right = 3L, horiz = horiz)

    #...................
    ### Post-Hoc Tests ####

    if (isTRUE(posthoc)) {

      cat(paste0("\n  Tukey HSD Post Hoc Test for Multiple Comparison\n\n"))

      .write.table(print.object[["posthoc"]], left = 2L, right = 4L, horiz = horiz)

    }

  #_____________________________________________________________________________
  #
  # Repeated Measures Analysis of Varianceaov.w() ------------------------------
  }, aov.w = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Round ####

    # Descriptive statistics
    print.object[["descript"]][, c("m", "low", "upp", "sd", "skew", "kurt")] <- vapply(print.object[["descript"]][, c("m", "low", "upp", "sd", "skew", "kurt")], formatC, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"), FUN.VALUE = character(nrow(print.object[["descript"]])))

    # Box Index of Sphericity
    print.object[["epsilon"]][, "epsilon"] <- formatC(print.object[["epsilon"]][, "epsilon"] , digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

    # ANOVA tables
    print.object[["test"]][["none"]][, c("sum.sq", "mean.sq", "F", "eta.sq", "eta.sq.p", "omega.sq", "omega.sq.p")] <- vapply(print.object[["test"]][["none"]][, c("sum.sq", "mean.sq", "F", "eta.sq", "eta.sq.p", "omega.sq", "omega.sq.p")], formatC, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"), FUN.VALUE = character(nrow(print.object[["test"]][["none"]])))

    print.object[["test"]][["none"]][, "df"] <- formatC(print.object[["test"]][["none"]][, "df"], digits = 0L, mode = "integer")

    print.object[["test"]][["none"]][, "p"] <- formatC(print.object[["test"]][["none"]][, "p"], digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

    ##

    print.object[["test"]][["lb"]][, c("sum.sq", "df", "mean.sq", "F", "eta.sq", "eta.sq.p", "omega.sq", "omega.sq.p")] <- vapply(print.object[["test"]][["lb"]][, c("sum.sq", "df", "mean.sq", "F", "eta.sq", "eta.sq.p", "omega.sq", "omega.sq.p")], formatC, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"), FUN.VALUE = character(nrow(print.object[["test"]][["lb"]])))

    print.object[["test"]][["lb"]][, "p"] <- formatC(print.object[["test"]][["lb"]][, "p"], digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

    ##

    print.object[["test"]][["gg"]][, c("sum.sq", "df", "mean.sq", "F", "eta.sq", "eta.sq.p", "omega.sq", "omega.sq.p")] <- vapply(print.object[["test"]][["gg"]][, c("sum.sq", "df", "mean.sq", "F", "eta.sq", "eta.sq.p", "omega.sq", "omega.sq.p")], formatC, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"), FUN.VALUE = character(nrow(print.object[["test"]][["gg"]])))

    print.object[["test"]][["gg"]][, "p"] <- formatC(print.object[["test"]][["gg"]][, "p"], digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))


    ###

    print.object[["test"]][["hf"]][, c("sum.sq", "df", "mean.sq", "F", "eta.sq", "eta.sq.p", "omega.sq", "omega.sq.p")] <- vapply(print.object[["test"]][["hf"]][, c("sum.sq", "df", "mean.sq", "F", "eta.sq", "eta.sq.p", "omega.sq", "omega.sq.p")], formatC, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"), FUN.VALUE = character(nrow(print.object[["test"]][["hf"]])))

    print.object[["test"]][["hf"]][, "p"] <- formatC(print.object[["test"]][["hf"]][, "p"], digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

    # Post hoc tests
    print.object[["posthoc"]][, c("m.diff", "t", "d", "d.low", "d.upp")] <- vapply(print.object[["posthoc"]][, c("m.diff", "t", "d", "d.low", "d.upp")], formatC, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"), FUN.VALUE = character(nrow(print.object[["posthoc"]])))

    print.object[["posthoc"]][, "p"] <- formatC(print.object[["posthoc"]][, "p"], digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Rename ####

    names(print.object[["epsilon"]]) <- c("index", "ep")
    print.object[["epsilon"]][, 1L] <- c("Lower Bound:", "Greenhouse and Geisser (GG):", "Huynh and Feldt (HF):", "Average of GG and HF:")

    names(print.object[["test"]][["none"]]) <- c("Source", "Sum Sq", "df", "Mean Sq", "F", "p", "et", "et.p", "om", "om.p")
    names(print.object[["test"]][["lb"]]) <- c("Source", "Sum Sq", "df", "Mean Sq", "F", "p", "et", "et.p", "om", "om.p")
    names(print.object[["test"]][["gg"]]) <- c("Source", "Sum Sq", "df", "Mean Sq", "F", "p", "et", "et.p", "om", "om.p")
    names(print.object[["test"]][["hf"]]) <- c("Source", "Sum Sq", "df", "Mean Sq", "F", "p", "et", "et.p", "om", "om.p")

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Format ####

    print.object[["descript"]] <- rbind(c("Variable", "n", "nNA", "M", "Low", "Upp", "SD", "Skew", "Kurt"), print.object[["descript"]])
    print.object[["epsilon"]] <- rbind(c("Box Index of Sphericity", "ep"), print.object[["epsilon"]])
    print.object[["test"]] <- lapply(print.object[["test"]], function(y) rbind(c("Source", "Sum Sq", "df", "Mean Sq", "F", "p", "et", "et.p", "om", "om.p"), y))
    print.object[["posthoc"]] <- rbind(c("Variable1", "Variable2", "M.diff", "t", "df", "p", "d", "Low", "Upp"), print.object[["posthoc"]])

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

    #...................
    ### Statistical Hypotheses ####

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

    #...................
    ### Descriptive Statistics ####

    if (isTRUE(descript)) {

      .write.table(print.object[["descript"]], left = 1L, right = 3L, horiz = horiz)

      cat("\n")

    }

    #...................
    ### Box Indices of Sphericity ####

    if (isTRUE(epsilon)) {

      .write.table(print.object[["epsilon"]], left = 1L, right = 3L, horiz = horiz)

      cat("\n")

    }

    #...................
    ### ANOVA Tables ####

    # Sphericity correction: none
    if (isTRUE("none" %in% x$args$print)) {

      cat(paste0("  Sphericity Correction: None\n"))

      .write.table(print.object[["test"]][["none"]], left = 2L, right = 4L, horiz = horiz)

      if (isTRUE(any(x$args$print %in% c("LB", "GG", "HF")) ||  posthoc)) { cat("\n") }

    }

    # Sphericity correction: lower bound
    if (isTRUE("LB" %in% x$args$print)) {

      cat(paste0("  Sphericity Correction: Lower Bound\n"))

      .write.table(print.object[["test"]][["lb"]], left = 2L, right = 4L, horiz = horiz)

      if (isTRUE(any(x$args$print %in% c("GG", "HF")) ||  posthoc)) { cat("\n") }

    }

    # Sphericity correction: Greenhouse-Geisser
    if (isTRUE("GG" %in% x$args$print)) {

      cat(paste0("  Sphericity Correction: Greenhouse-Geisser \n"))

      .write.table(print.object[["test"]][["gg"]], left = 2L, right = 4L, horiz = horiz)

      if (isTRUE(any(x$args$print %in% c("HF")) ||  posthoc)) { cat("\n") }

    }

    # Sphericity correction: Huynh-Feldt
    if (isTRUE("HF" %in% x$args$print)) {

      cat(paste0("  Sphericity Correction: Huynh-Feldt \n"))

      .write.table(print.object[["test"]][["hf"]], left = 2L, right = 4L, horiz = horiz)

      if (isTRUE(posthoc)) { cat("\n") }

    }

    #...................
    ### Post-Hoc Tests ####

    if (isTRUE(posthoc)) {

      cat(paste0("  Paired-Samples t-Tests for Multiple Comparison\n\n"))

      .write.table(print.object[["posthoc"]], left = 2L, right = 4L, horiz = horiz)

      cat(paste0("\n   Adjustment for multiple testing: ", x$args$p.adj))

      cat("\n")

    }

  #_____________________________________________________________________________
  #
  # Blimp Object, blimp() ------------------------------------------------------
  }, blimp = {

    cat("Please use the blimp.print function to print a \"blimp\" object.")

  #_____________________________________________________________________________
  #
  # Blimp Summary Measures, Convergence and Efficiency Diagnostics, blimp.bayes
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
  # Collinearity Diagnostics, check.collin() -----------------------------------
  }, check.collin = {

    # Check input 'print'
    if (isTRUE(check)) { if (isTRUE(any(!print %in% c("all", "vif", "eigen")))) { stop("Character string(s) in the argument 'print' does not match with \"all\", \"vif\", \"eigen\".", call. = FALSE) } }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Arguments ####

    # Print variance inflation factor and/or eigenvalue
    if (isTRUE(length(print) == 1L && "all" %in% print)) { print <- c("vif", "eigen") }

    cat(" Collinearity Diagnostics\n")

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Tolerance, Std. Error Inflation Factor, and Variance Inflation Factor ####

    if (isTRUE("vif" %in% print)) {

      # Rename variables
      colnames(print.object$coef) <- sub("aGSIF", "SIF", colnames(print.object$coef))
      colnames(print.object$coef) <- sub("aGVIF", "VIF", colnames(print.object$coef))

      # Exclude variables "df" and "GVIF"
      if (isTRUE(all(na.omit(print.object$coef$dfVIF == 1L)))) { print.object$coef <- print.object$coef[, which(!colnames(print.object$coef) %in% "dfVIF")] }
      print.object$coef <- print.object$coef[, which(!colnames(print.object$coef) %in% "GVIF")]

      # Round
      if (isTRUE(inherits(x$model, what = "lmerMod"))) {

        print.object$coef[, setdiff(colnames(print.object$coef), "dfVIF")] <- apply(print.object$coef[, setdiff(colnames(print.object$coef), "dfVIF")], 2L, function(y) formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")))

      } else {

        print.object$coef[, setdiff(colnames(print.object$coef), c("df", "dfVIF", "p"))] <- apply(print.object$coef[, setdiff(colnames(print.object$coef), c("df", "dfVIF", "p"))], 2L, function(y) formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")))

        print.object$coef[, "p"] <- formatC(print.object$coef[, "p"], digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

      }

      # Format
      print.object$coef <- cbind(c("", row.names(print.object$coef)), rbind(colnames(print.object$coef), print.object$coef))
      print.object$coef[-1L, 1L] <- paste0("   ", print.object$coef[-1L, 1L])
      print.object$coef[, 1L] <- apply(print.object$coef[, 1L, drop = FALSE], 2L, format, justify = "left")

      print.object$coef <- apply(print.object$coef, 2L, function(y) format(y, justify = "right"))

      # Repace NA with ""
      print.object$coef <- apply(print.object$coef, 2L, function(y) gsub("NA", "", y))

      # Format row names
      row.names(print.object$coef) <- paste0("   ", row.names(print.object$coef))

      # Print object
      cat("\n  Tolerance (Tol), Std. Error Inflation Factor (SIF), and Variance Inflation Factor (VIF)\n\n")

      .write.table(print.object$coef, left = 2L, right = 4L, horiz = horiz)

      # Note for model involving categorical predictors
      if (isTRUE(any(x$result$vif$dfVIF > 1L))) { cat("\n   Note. Generalized SIF/VIF are computed for terms with more than 1 dfVIF\n") }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Eigenvalue, condition index, and variance proportions ####
    if (isTRUE("eigen" %in% print)) {

      # Round
      print.object$eigen[, -1L] <- apply(print.object$eigen[, -1L], 2L, function(y) formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")))

      # Rename variables
      colnames(print.object$eigen) <- sub("dim", "Dim", colnames(print.object$eigen))
      colnames(print.object$eigen) <- sub("eigen", "Eigen", colnames(print.object$eigen))
      colnames(print.object$eigen) <- sub("ci", "CI", colnames(print.object$eigen))

      # Format
      print.object$eigen <- rbind(colnames(print.object$eigen), print.object$eigen)
      print.object$eigen[, 1L] <- paste0("    ", print.object$eigen[, 1L])
      print.object$eigen[, 1L] <- apply(print.object$eigen[, 1L, drop = FALSE], 2L, format, justify = "left")

      print.object$eigen <- apply(print.object$eigen, 2L, function(y) format(y, justify = "right"))

      # Print object
      cat("\n  Eigenvalue (Eigen), Condition Index (CI), and Variance Proportions\n\n")

      .write.table(print.object$eigen, left = 3L, right = 5L, horiz = horiz)

    }

  #_____________________________________________________________________________
  #
  # Confidence Interval for the Correlation Coefficient, ci.cor() --------------
  }, ci.cor = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## No grouping ####

    if (isTRUE(is.null(x$data$group) && is.null(x$data$split))) {

      #...................
      ### Round ####

      if (isTRUE(all(print.object$nNA == 0L))) {

        print.object[, c("skew1", "kurt1", "skew2", "kurt2", "cor", "low", "upp")] <- sapply(c("skew1", "kurt1", "skew2", "kurt2", "cor", "low", "upp"), function(y) ifelse(!is.na(print.object[, y]), formatC(print.object[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

      } else {

        print.object[, c("pNA", "skew1", "kurt1", "skew2", "kurt2", "cor", "low", "upp")] <- sapply(c("pNA", "skew1", "kurt1", "skew2", "kurt2", "cor", "low", "upp"), function(y) ifelse(!is.na(print.object[, y]), formatC(print.object[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

      }

      print.object[, "pNA"] <- paste0(print.object[, "pNA"], "%")

      #...................
      ### Column Names ####

      switch(x$args$method, "pearson" = {

        print.object <- rbind(c("Var1", "Var2", "n", "nNA", "%NA", "Skew1", "Kurt1", "Skew2", "Kurt2", "r", "Low", "Upp"), print.object)

      }, "spearman" = {

        print.object <- rbind(c("Var1", "Var2", "n", "nNA", "%NA", "Skew1", "Kurt1", "Skew2", "Kurt2", "rs", "Low", "Upp"), print.object)

      }, "kendall-b" = {

        print.object <- rbind(c("Var1", "Var2", "n", "nNA", "%NA", "Skew1", "Kurt1", "Skew2", "Kurt2", "Tau-b", "Low", "Upp"), print.object)

      }, "kendall-c" = {

        print.object <- rbind(c("Var1", "Var2", "n", "nNA", "%NA", "Skew1", "Kurt1", "Skew2", "Kurt2", "Tau-c", "Low", "Upp"), print.object)

      })

      #...................
      ### Format ####

      # Remove duplicated values
      print.object[duplicated(print.object$var1) , "var1"] <- ""

      # Justify left and right
      col.format <- which(colnames(print.object) %in% c("var1", "var2"))

      # Justify left and right
      print.object[, col.format] <- format(print.object[, col.format], justify = "left")
      print.object[, -col.format] <- apply(print.object[, -col.format], 2L, format, justify = "right")

      # Add blank space
      print.object[, "var1"] <- c(paste0("  ", print.object[1L, "var1"]), paste0("   ", print.object[-1L, "var1"]))
      print.object[, "var2"] <- c(paste0("", print.object[1L, "var2"]), paste0(" ", print.object[-1L, "var2"]))

      print.object[, "var1"] <- format(c(print.object[1L, "var1"], misty::chr.trim(print.object[-1L, "var1"], side = "right")), justify = "left")
      print.object[, "var2"] <- format(c(print.object[1L, "var2"], misty::chr.trim(print.object[-1L, "var2"], side = "right")), justify = "left")

      # Missing values, NA
      print.object[, c("skew1", "kurt1", "skew2", "kurt2", "cor", "low", "upp")] <- data.frame(apply(print.object[, c("skew1", "kurt1", "skew2", "kurt2", "cor", "low", "upp")], 2L, function(y) gsub("NA", "  ", as.character(y))))

      #...................
      ### Print Output ####

      cat(paste0(switch(x$args$alternative, two.sided = " Two-Sided ", less = " One-Sided ", greater = " One-Sided "),
                 round(x$args$conf.level * 100L, digits = 2L), ifelse(isTRUE(x$args$boot == "none"), "% Confidence Interval: ", "% Bootstrap Confidence Interval: "),
                 switch(x$args$method,
                        "pearson" = "Pearson Product-Moment Correlation Coefficient\n\n",
                        "spearman" = "Spearman's Rank-Order Correlation Coefficient\n\n",
                        "kendall-b" = "Kendall's Tau-b\n\n",
                        "kendall-c" = "Kendall-Stuart's Tau-c\n\n")))

      .write.table(print.object, left = 1L, right = 3L, horiz = horiz)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Grouping ####

    } else if (isTRUE(!is.null(x$data$group) && is.null(x$data$split))) {

      #...................
      ### Round ####

      if (isTRUE(all(print.object$nNA == 0L))) {

        print.object[, c("skew1", "kurt1", "skew2", "kurt2", "cor", "low", "upp")] <- sapply(c("skew1", "kurt1", "skew2", "kurt2", "cor", "low", "upp"), function(y) ifelse(!is.na(print.object[, y]), formatC(print.object[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

      } else {

        print.object[, c("pNA", "skew1", "kurt1", "skew2", "kurt2", "cor", "low", "upp")] <- sapply(c("pNA", "skew1", "kurt1", "skew2", "kurt2", "cor", "low", "upp"), function(y) ifelse(!is.na(print.object[, y]), formatC(print.object[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

      }

      print.object[, "pNA"] <- paste0(print.object[, "pNA"], "%")

      #...................
      ### Column Names ####

      switch(x$args$method, "pearson" = {

        print.object <- rbind(c("Group", "Var1", "Var2", "n", "nNA", "%NA", "Skew1", "Kurt1", "Skew2", "Kurt2", "r", "Low", "Upp"), print.object)

      }, "spearman" = {

        print.object <- rbind(c("Group", "Var1", "Var2", "n", "nNA", "%NA", "Skew1", "Kurt1", "Skew2", "Kurt2", "rs", "Low", "Upp"), print.object)

      }, "kendall-b" = {

        print.object <- rbind(c("Group", "Var1", "Var2", "n", "nNA", "%NA", "Skew1", "Kurt1", "Skew2", "Kurt2", "Tau-b", "Low", "Upp"), print.object)

      }, "kendall-c" = {

        print.object <- rbind(c("Group", "Var1", "Var2", "n", "nNA", "%NA", "Skew1", "Kurt1", "Skew2", "Kurt2", "Tau-c", "Low", "Upp"), print.object)

      })

      #...................
      ### Format ####

      # Remove duplicated values
      print.object[duplicated(paste(print.object$group, print.object$var1)) , "var1"] <- ""
      print.object[duplicated(print.object$group) , "group"] <- ""

      # Justify left and right
      col.format <- which(colnames(print.object) %in% c("group", "var1", "var2"))

      # Justify left and right
      print.object[, col.format] <- format(print.object[, col.format], justify = "left")
      print.object[, -col.format] <- apply(print.object[, -col.format], 2L, format, justify = "right")

      # Add blank space
      print.object[, "group"] <- c(paste0("  ", print.object[1L, "group"]), paste0("   ", print.object[-1L, "group"]))
      print.object[, "var1"] <- c(paste0("", print.object[1L, "var1"]), paste0(" ", print.object[-1L, "var1"]))
      print.object[, "var2"] <- c(paste0("", print.object[1L, "var2"]), paste0(" ", print.object[-1L, "var2"]))

      print.object[, "group"] <- format(c(print.object[1L, "group"], misty::chr.trim(print.object[-1L, "group"], side = "right")), justify = "left")
      print.object[, "var1"] <- format(c(print.object[1L, "var1"], misty::chr.trim(print.object[-1L, "var1"], side = "right")), justify = "left")
      print.object[, "var2"] <- format(c(print.object[1L, "var2"], misty::chr.trim(print.object[-1L, "var2"], side = "right")), justify = "left")

      # Missing values, NA
      print.object[, c("skew1", "kurt1", "skew2", "kurt2", "cor", "low", "upp")] <- data.frame(apply(print.object[, c("skew1", "kurt1", "skew2", "kurt2", "cor", "low", "upp")], 2L, function(y) gsub("NA", "  ", as.character(y))))

      #...................
      ### Print Output ####

      cat(paste0(switch(x$args$alternative, two.sided = " Two-Sided ", less = " One-Sided ", greater = " One-Sided "),
                 round(x$args$conf.level * 100L, digits = 2L), ifelse(isTRUE(x$args$boot == "none"), "% Confidence Interval: ", "% Bootstrap Confidence Interval: "),
                 switch(x$args$method,
                        "pearson" = "Pearson Product-Moment Correlation Coefficient\n\n",
                        "spearman" = "Spearman's Rank-Order Correlation Coefficient\n\n",
                        "kendall-b" = "Kendall's Tau-b\n\n",
                        "kendall-c" = "Kendall-Stuart's Tau-c\n\n")))

      .write.table(print.object, left = 1L, right = 3L, group = TRUE, result = x$result, horiz = horiz)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Split ####

    } else if (isTRUE(!is.null(x$data$split))) {

      # No grouping
      if (isTRUE(is.null(x$data$group))) {

        for (i in names(print.object)) {

          #...................
          ### Round ####

          if (isTRUE(all(print.object[[i]]$nNA == 0L))) {

            print.object[[i]][, c("skew1", "kurt1", "skew2", "kurt2", "cor", "low", "upp")] <- sapply(c("skew1", "kurt1", "skew2", "kurt2", "cor", "low", "upp"), function(y) ifelse(!is.na(print.object[[i]][, y]), formatC(print.object[[i]][, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

          } else {

            print.object[[i]][, c("pNA", "skew1", "kurt1", "skew2", "kurt2", "cor", "low", "upp")] <- sapply(c("pNA", "skew1", "kurt1", "skew2", "kurt2", "cor", "low", "upp"), function(y) ifelse(!is.na(print.object[[i]][, y]), formatC(print.object[[i]][, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

          }

          print.object[[i]][, "pNA"] <- paste0(print.object[[i]][, "pNA"], "%")

          #...................
          ### Column Names ####

          switch(x$args$method, "pearson" = {

            print.object[[i]] <- rbind(c("Var1", "Var2", "n", "nNA", "%NA", "Skew1", "Kurt1", "Skew2", "Kurt2", "r", "Low", "Upp"), print.object[[i]])

          }, "spearman" = {

            print.object[[i]] <- rbind(c("Var1", "Var2", "n", "nNA", "%NA", "Skew1", "Kurt1", "Skew2", "Kurt2", "rs", "Low", "Upp"), print.object[[i]])

          }, "kendall-b" = {

            print.object[[i]] <- rbind(c("Var1", "Var2", "n", "nNA", "%NA", "Skew1", "Kurt1", "Skew2", "Kurt2", "Tau-b", "Low", "Upp"), print.object[[i]])

          }, "kendall-c" = {

            print.object[[i]] <- rbind(c("Var1", "Var2", "n", "nNA", "%NA", "Skew1", "Kurt1", "Skew2", "Kurt2", "Tau-c", "Low", "Upp"), print.object[[i]])

          })

          #...................
          ### Format ####

          # Remove duplicated values
          print.object[[i]][duplicated(print.object[[i]]$var1) , "var1"] <- ""

          # Justify left and right
          col.format <- which(colnames(print.object[[i]]) %in% c("var1", "var2"))

          # Justify left and right
          print.object[[i]][, col.format] <- format(print.object[[i]][, col.format], justify = "left")
          print.object[[i]][, -col.format] <- apply(print.object[[i]][, -col.format], 2L, format, justify = "right")

          # Add blank space
          print.object[[i]][, "var1"] <- c(paste0("   ", print.object[[i]][1L, "var1"]), paste0("    ", print.object[[i]][-1L, "var1"]))
          print.object[[i]][, "var2"] <- c(paste0("", print.object[[i]][1L, "var2"]), paste0(" ", print.object[[i]][-1L, "var2"]))

          print.object[[i]][, "var1"] <- format(c(print.object[[i]][1L, "var1"], misty::chr.trim(print.object[[i]][-1L, "var1"], side = "right")), justify = "left")
          print.object[[i]][, "var2"] <- format(c(print.object[[i]][1L, "var2"], misty::chr.trim(print.object[[i]][-1L, "var2"], side = "right")), justify = "left")

          # Missing values, NA
          print.object[[i]][, c("skew1", "kurt1", "skew2", "kurt2", "cor", "low", "upp")] <- data.frame(apply(print.object[[i]][, c("skew1", "kurt1", "skew2", "kurt2", "cor", "low", "upp")], 2L, function(y) gsub("NA", "  ", as.character(y))))

        }

      # Grouping
      } else {

        for (i in names(print.object)) {

          #...................
          ### Round ####

          if (isTRUE(all(print.object[[i]]$nNA == 0L))) {

            print.object[[i]][, c("skew1", "kurt1", "skew2", "kurt2", "cor", "low", "upp")] <- sapply(c("skew1", "kurt1", "skew2", "kurt2", "cor", "low", "upp"), function(y) ifelse(!is.na(print.object[[i]][, y]), formatC(print.object[[i]][, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

          } else {

            print.object[[i]][, c("pNA", "skew1", "kurt1", "skew2", "kurt2", "cor", "low", "upp")] <- sapply(c("pNA", "skew1", "kurt1", "skew2", "kurt2", "cor", "low", "upp"), function(y) ifelse(!is.na(print.object[[i]][, y]), formatC(print.object[[i]][, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

          }

          print.object[[i]][, "pNA"] <- paste0(print.object[[i]][, "pNA"], "%")

          #...................
          ### Column Names ####

          switch(x$args$method, "pearson" = {

            print.object[[i]] <- rbind(c("Group", "Var1", "Var2", "n", "nNA", "%NA", "Skew1", "Kurt1", "Skew2", "Kurt2", "r", "Low", "Upp"), print.object[[i]])

          }, "spearman" = {

            print.object[[i]] <- rbind(c("Group", "Var1", "Var2", "n", "nNA", "%NA", "Skew1", "Kurt1", "Skew2", "Kurt2", "rs", "Low", "Upp"), print.object[[i]])

          }, "kendall-b" = {

            print.object[[i]] <- rbind(c("Group", "Var1", "Var2", "n", "nNA", "%NA", "Skew1", "Kurt1", "Skew2", "Kurt2", "Tau-b", "Low", "Upp"), print.object[[i]])

          }, "kendall-c" = {

            print.object[[i]] <- rbind(c("Group", "Var1", "Var2", "n", "nNA", "%NA", "Skew1", "Kurt1", "Skew2", "Kurt2", "Tau-c", "Low", "Upp"), print.object[[i]])

          })

          #...................
          ### Format ####

          # Remove duplicated values
          print.object[[i]][duplicated(paste(print.object[[i]]$group, print.object[[i]]$var1)) , "var1"] <- ""
          print.object[[i]][duplicated(print.object[[i]]$group) , "group"] <- ""

          # Justify left and right
          col.format <- which(colnames(print.object[[i]]) %in% c("group", "var1", "var2"))

          # Justify left and right
          print.object[[i]][, col.format] <- format(print.object[[i]][, col.format], justify = "left")
          print.object[[i]][, -col.format] <- apply(print.object[[i]][, -col.format], 2L, format, justify = "right")

          # Add blank space
          print.object[[i]][, "group"] <- c(paste0("   ", print.object[[i]][1L, "group"]), paste0("    ", print.object[[i]][-1L, "group"]))
          print.object[[i]][, "var1"] <- c(paste0("", print.object[[i]][1L, "var1"]), paste0(" ", print.object[[i]][-1L, "var1"]))
          print.object[[i]][, "var2"] <- c(paste0("", print.object[[i]][1L, "var2"]), paste0(" ", print.object[[i]][-1L, "var2"]))

          print.object[[i]][, "group"] <- format(c(print.object[[i]][1L, "group"], misty::chr.trim(print.object[[i]][-1L, "group"], side = "right")), justify = "left")
          print.object[[i]][, "var1"] <- format(c(print.object[[i]][1L, "var1"], misty::chr.trim(print.object[[i]][-1L, "var1"], side = "right")), justify = "left")
          print.object[[i]][, "var2"] <- format(c(print.object[[i]][1L, "var2"], misty::chr.trim(print.object[[i]][-1L, "var2"], side = "right")), justify = "left")

          # Missing values, NA
          print.object[[i]][, c("skew1", "kurt1", "skew2", "kurt2", "cor", "low", "upp")] <- data.frame(apply(print.object[[i]][, c("skew1", "kurt1", "skew2", "kurt2", "cor", "low", "upp")], 2L, function(y) gsub("NA", "  ", as.character(y))))

        }

      }

      #...................
      ### Print Output ####

      cat(paste0(switch(x$args$alternative, two.sided = " Two-Sided ", less = " One-Sided ", greater = " One-Sided "),
                 round(x$args$conf.level * 100L, digits = 2L), ifelse(isTRUE(x$args$boot == "none"), "% Confidence Interval: ", "% Bootstrap Confidence Interval: "),
                 switch(x$args$method,
                        "pearson" = "Pearson Product-Moment Correlation Coefficient\n\n",
                        "spearman" = "Spearman's Rank-Order Correlation Coefficient\n\n",
                        "kendall-b" = "Kendall's Tau-b\n\n",
                        "kendall-c" = "Kendall-Stuart's Tau-c\n\n")))

      for (i in names(print.object)) {

        cat("  Split Group:", i, "\n")

        .write.table(print.object[[i]], left = 2L, right = 4L, group = ifelse(isTRUE("group" %in% colnames(print.object[[i]])), TRUE, FALSE), result = x$result[[i]], horiz = horiz)

        if (isTRUE(i != names(print.object)[length(print.object)])) { cat("\n") }

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Note ####

    #...................
    ### No Bootstrapping ####

    if (isTRUE(x$args$boot == "none")) {

      switch(x$args$method, "pearson" = {

        cat(paste0("\n  Note. ", switch(x$args$adjust,
                                        "none" = "Without non-normality adjustment.\n",
                                        "joint" = "Non-normality adjustment via sample joint moments method.\n",
                                        "approx" = "Non-normality adjustment via approximate distribution method.\n")))

      }, "spearman" = {

        cat(paste0("\n  Note. ", switch(x$args$se,
                                        "fisher" = "Fisher (1921) standard error.\n",
                                        "fieller" = "Fieller et al. (1957) standard error.\n",
                                        "bonett" = "Bonett and Wright (2000) standard error.\n",
                                        "rin" = "Rank-based inverse normal transformation.\n")))

      }, "kendall-b" = {

        cat("\n  Note. Fieller et al. (1957) standard error.\n")

      }, "kendall-b" = {

        cat("\n  Note. Fieller et al. (1957) standard error.\n")

      })

    #...................
    ### Bootstrapping ####

    } else {

      cat(paste0("\n  Note. ",
                 switch(x$args$boot,
                        "norm" = "Bias-corrected normal approximation bootstrap CI, ",
                        "basic" = "Basic bootstrap CI, ",
                        "perc" = "Percentile bootstrap CI, ",
                        "bc" = "Bias-corrected (BC) percentile bootstrap CI, ",
                        "bca" = "Bias-corrected and accelerated (BCa) bootstrap CI, "),
                 x$args$R, " replications."))

    }

  #_____________________________________________________________________________
  #
  # Confidence Interval for the Arithmetic Mean, ci.mean() ---------------------
  }, ci.mean = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## No grouping ####

    if (isTRUE(is.null(x$data$group) && is.null(x$data$split))) {

      #...................
      ### Round ####

      if (isTRUE(all(print.object$nNA == 0L))) {

        print.object[, c("sd", "skew", "kurt", "m", "low", "upp")] <- sapply(c("sd", "skew", "kurt", "m", "low", "upp"), function(y) ifelse(!is.na(print.object[, y]), formatC(print.object[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

      } else {

        print.object[, c("pNA", "sd", "skew", "kurt", "m", "low", "upp")] <- sapply(c("pNA", "sd", "skew", "kurt", "m", "low", "upp"), function(y) ifelse(!is.na(print.object[, y]), formatC(print.object[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

      }

      print.object[, "pNA"] <- paste0(print.object[, "pNA"], "%")

      #...................
      ### Column Names ####

      print.object <- rbind(c("Variable", "n", "nNA", "%NA", "SD", "Skew", "Kurt", "M", "Low", "Upp"), print.object)

      #...................
      ### Format ####

      # Justify left and right
      col.format <- which(colnames(print.object) %in% "variable")

      # Justify left and right
      print.object[, col.format] <- format(print.object[, col.format], justify = "left")
      print.object[, -col.format] <- apply(print.object[, -col.format], 2L, format, justify = "right")

      # Add blank space
      print.object[, "variable"] <- c(paste0("  ", print.object[1L, "variable"]), paste0("   ", print.object[-1L, "variable"]))
      print.object[, "variable"] <- format(c(print.object[1L, "variable"], misty::chr.trim(print.object[-1L, "variable"], side = "right")), justify = "left")

      # Missing values, NA
      print.object[, c("sd", "skew", "kurt", "m", "low", "upp")] <- data.frame(apply(print.object[, c("sd", "skew", "kurt", "m", "low", "upp")], 2L, function(y) gsub("NA", "  ", as.character(y))))

      #...................
      ### One Variable ####

      if (isTRUE(length(unique(x$result$variable)) == 1L)) {

        print.object <- print.object[, -1L]

        # First column "n"
        print.object[, "n"] <- c(paste0("  ", print.object[1L, "n"]), paste0("  ", print.object[-1L, "n"]))
        print.object[, "n"] <- format(c(print.object[1L, "n"], misty::chr.trim(print.object[-1L, "n"], side = "right")), justify = "right")

      }

      #...................
      ### Print Output ####

      cat(paste0(switch(x$args$alternative, two.sided = " Two-Sided ", less = " One-Sided ", greater = " One-Sided "),
                 round(x$args$conf.level * 100L, digits = 2L), ifelse(isTRUE(x$args$boot == "none"), "% Confidence Interval: ", "% Bootstrap Confidence Interval: "),
                 "Arithmetic Mean\n\n"))

      .write.table(print.object, left = 1L, right = 3L, group = FALSE, horiz = horiz)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Grouping ####

    } else if (isTRUE(!is.null(x$data$group) && is.null(x$data$split))) {

      if (isTRUE(all(print.object$nNA == 0L))) {

        print.object[, c("sd", "skew", "kurt", "m", "low", "upp")] <- sapply(c("sd", "skew", "kurt", "m", "low", "upp"), function(y) ifelse(!is.na(print.object[, y]), formatC(print.object[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

      } else {

        print.object[, c("pNA", "sd", "skew", "kurt", "m", "low", "upp")] <- sapply(c("pNA", "sd", "skew", "kurt", "m", "low", "upp"), function(y) ifelse(!is.na(print.object[, y]), formatC(print.object[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

      }

      print.object[, "pNA"] <- paste0(print.object[, "pNA"], "%")

      #...................
      ### Column Names ####

      print.object <- rbind(c("Group", "Variable", "n", "nNA", "%NA", "SD", "Skew", "Kurt", "M", "Low", "Upp"), print.object)

      #...................
      ### Format ####

      # Remove duplicated values
      print.object[duplicated(print.object$group) , "group"] <- ""

      # Justify left and right
      col.format <- which(colnames(print.object) %in% c("group", "variable"))

      # Justify left and right
      print.object[, col.format] <- format(print.object[, col.format], justify = "left")
      print.object[, -col.format] <- apply(print.object[, -col.format], 2L, format, justify = "right")

      # Add blank space
      print.object[, "group"] <- c(paste0("  ", print.object[1L, "group"]), paste0("   ", print.object[-1L, "group"]))
      print.object[, "variable"] <- c(paste0("", print.object[1L, "variable"]), paste0(" ", print.object[-1L, "variable"]))

      print.object[, "group"] <- format(c(print.object[1L, "group"], misty::chr.trim(print.object[-1L, "group"], side = "right")), justify = "left")
      print.object[, "variable"] <- format(c(print.object[1L, "variable"], misty::chr.trim(print.object[-1L, "variable"], side = "right")), justify = "left")

      # Missing values, NA
      print.object[, c("sd", "skew", "kurt", "m", "low", "upp")] <- data.frame(apply(print.object[, c("sd", "skew", "kurt", "m", "low", "upp")], 2L, function(y) gsub("NA", "  ", as.character(y))))

      #...................
      ### One Variable ####

      if (isTRUE(length(unique(x$result$variable)) == 1L)) { print.object <- print.object[, -2L] }

      #...................
      ### Print Output ####

      cat(paste0(switch(x$args$alternative, two.sided = " Two-Sided ", less = " One-Sided ", greater = " One-Sided "),
                 round(x$args$conf.level * 100L, digits = 2L), ifelse(isTRUE(x$args$boot == "none"), "% Confidence Interval: ", "% Bootstrap Confidence Interval: "),
                 "Arithmetic Mean\n\n"))

      .write.table(print.object, left = 1L, right = 3L, group = TRUE, result = x$result, horiz = horiz)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Split ####

    } else if (isTRUE(!is.null(x$data$split))) {

      # No grouping
      if (isTRUE(is.null(x$data$group))) {

        for (i in names(print.object)) {

          #...................
          ### Round ####

          if (isTRUE(all(print.object[[i]]$nNA == 0L))) {

            print.object[[i]][, c("sd", "skew", "kurt", "m", "low", "upp")] <- sapply(c("sd", "skew", "kurt", "m", "low", "upp"), function(y) ifelse(!is.na(print.object[[i]][, y]), formatC(print.object[[i]][, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

          } else {

            print.object[[i]][, c("pNA", "sd", "skew", "kurt", "m", "low", "upp")] <- sapply(c("pNA", "sd", "skew", "kurt", "m", "low", "upp"), function(y) ifelse(!is.na(print.object[[i]][, y]), formatC(print.object[[i]][, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

          }

          print.object[[i]][, "pNA"] <- paste0(print.object[[i]][, "pNA"], "%")

          #...................
          ### Column Names ####

          print.object[[i]] <- rbind(c("Variable", "n", "nNA", "%NA", "SD", "Skew", "Kurt", "M", "Low", "Upp"), print.object[[i]])

          #...................
          ### Format ####

          # Justify left and right
          col.format <- which(colnames(print.object[[i]]) %in% "variable")

          # Justify left and right
          print.object[[i]][, col.format] <- format(print.object[[i]][, col.format], justify = "left")
          print.object[[i]][, -col.format] <- apply(print.object[[i]][, -col.format], 2L, format, justify = "right")

          # Add blank space
          print.object[[i]][, "variable"] <- c(paste0("   ", print.object[[i]][1L, "variable"]), paste0("    ", print.object[[i]][-1L, "variable"]))
          print.object[[i]][, "variable"] <- format(c(print.object[[i]][1L, "variable"], misty::chr.trim(print.object[[i]][-1L, "variable"], side = "right")), justify = "left")

          # Missing values, NA
          print.object[[i]][, c("sd", "skew", "kurt", "m", "low", "upp")] <- data.frame(apply(print.object[[i]][, c("sd", "skew", "kurt", "m", "low", "upp")], 2L, function(y) gsub("NA", "  ", as.character(y))))

          #...................
          ### One Variable ####

          if (isTRUE(length(unique(x$result[[1L]]$variable)) == 1L)) {

            print.object[[i]] <- print.object[[i]][, -1L]

            print.object[[i]][, "n"] <- c(paste0("   ", print.object[[i]][1L, "n"]), paste0("   ", print.object[[i]][-1L, "n"]))
            print.object[[i]][, "n"] <- format(c(print.object[[i]][1L, "n"], misty::chr.trim(print.object[[i]][-1L, "n"], side = "right")), justify = "left")

          }

        }

      # Grouping
      } else {

        for (i in names(print.object)) {

          #...................
          ### Round ####

          if (isTRUE(all(print.object[[i]]$nNA == 0L))) {

            print.object[[i]][, c("sd", "skew", "kurt", "m", "low", "upp")] <- sapply(c("sd", "skew", "kurt", "m", "low", "upp"), function(y) ifelse(!is.na(print.object[[i]][, y]), formatC(print.object[[i]][, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

          } else {

            print.object[[i]][, c("pNA", "sd", "skew", "kurt", "m", "low", "upp")] <- sapply(c("pNA", "sd", "skew", "kurt", "m", "low", "upp"), function(y) ifelse(!is.na(print.object[[i]][, y]), formatC(print.object[[i]][, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

          }

          print.object[[i]][, "pNA"] <- paste0(print.object[[i]][, "pNA"], "%")

          #...................
          ### Column Names ####

          print.object[[i]] <- rbind(c("Group", "Variable", "n", "nNA", "%NA", "SD", "Skew", "Kurt", "M", "Low", "Upp"), print.object[[i]])

          #...................
          ### Format ####

          # Remove duplicated values
          print.object[[i]][duplicated(paste(print.object[[i]]$group, print.object[[i]]$variable)) , "variable"] <- ""
          print.object[[i]][duplicated(print.object[[i]]$group) , "group"] <- ""

          # Justify left and right
          col.format <- which(colnames(print.object[[i]]) %in% c("group", "variable"))

          # Justify left and right
          print.object[[i]][, col.format] <- format(print.object[[i]][, col.format], justify = "left")
          print.object[[i]][, -col.format] <- apply(print.object[[i]][, -col.format], 2L, format, justify = "right")

          # Add blank space
          print.object[[i]][, "group"] <- c(paste0("   ", print.object[[i]][1L, "group"]), paste0("    ", print.object[[i]][-1L, "group"]))
          print.object[[i]][, "variable"] <- c(paste0("", print.object[[i]][1L, "variable"]), paste0(" ", print.object[[i]][-1L, "variable"]))

          print.object[[i]][, "group"] <- format(c(print.object[[i]][1L, "group"], misty::chr.trim(print.object[[i]][-1L, "group"], side = "right")), justify = "left")
          print.object[[i]][, "variable"] <- format(c(print.object[[i]][1L, "variable"], misty::chr.trim(print.object[[i]][-1L, "variable"], side = "right")), justify = "left")

          # Missing values, NA
          print.object[[i]][, c("sd", "skew", "kurt", "m", "low", "upp")] <- data.frame(apply(print.object[[i]][, c("sd", "skew", "kurt", "m", "low", "upp")], 2L, function(y) gsub("NA", "  ", as.character(y))))

          #...................
          ### One Variable ####

          if (isTRUE(length(unique(x$result[[1L]]$variable)) == 1L)) { print.object[[i]] <- print.object[[i]][, -2L] }

        }

      }

      #...................
      ### Print Output ####

      cat(paste0(switch(x$args$alternative, two.sided = " Two-Sided ", less = " One-Sided ", greater = " One-Sided "),
                  round(x$args$conf.level * 100L, digits = 2L), ifelse(isTRUE(x$args$boot == "none"), "% Confidence Interval: ", "% Bootstrap Confidence Interval: "),
                  "Arithmetic Mean\n\n"))

      for (i in names(print.object)) {

        cat("  Split Group:", i, "\n")

        .write.table(print.object[[i]], left = 2L, right = 4L, group = ifelse(isTRUE("group" %in% colnames(print.object[[i]])), TRUE, FALSE), result = x$result[[i]], horiz = horiz)

        if (isTRUE(i != names(print.object)[length(print.object)])) { cat("\n") }

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Note ####

    #...................
    ### Bootstrapping ####

    if (isTRUE(x$args$boot != "none")) {

      cat(paste0("\n  Note. ",
                 switch(x$args$boot,
                        "norm" = "Bias-corrected normal approximation bootstrap CI, ",
                        "basic" = "Basic bootstrap CI, ",
                        "stud" = "Studentized bootstrap CI, ",
                        "perc" = "Percentile bootstrap CI, ",
                        "bc" = "Bias-corrected (BC) percentile bootstrap CI, ",
                        "bca" = "Bias-corrected and accelerated (BCa) bootstrap CI, "),
                 x$args$R, " replications."))

    }

  #_____________________________________________________________________________
  #
  # Confidence Interval for the Median, ci.median() ----------------------------
  }, ci.median = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## No grouping ####

    if (isTRUE(is.null(x$data$group) && is.null(x$data$split))) {

      #...................
      ### Round ####

      if (isTRUE(all(print.object$nNA == 0L))) {

        print.object[, c("sd", "iqr", "skew", "kurt", "med", "low", "upp")] <- sapply(c("sd", "iqr", "skew", "kurt", "med", "low", "upp"), function(y) ifelse(!is.na(print.object[, y]), formatC(print.object[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

      } else {

        print.object[, c("pNA", "sd", "iqr", "skew", "kurt", "med", "low", "upp")] <- sapply(c("pNA", "sd", "iqr", "skew", "kurt", "med", "low", "upp"), function(y) ifelse(!is.na(print.object[, y]), formatC(print.object[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

      }

      print.object[, "pNA"] <- paste0(print.object[, "pNA"], "%")

      #...................
      ### Column Names ####

      print.object <- rbind(c("Variable", "n", "nNA", "%NA", "SD", "IQR", "Skew", "Kurt", "Med", "Low", "Upp"), print.object)

      #...................
      ### Format ####

      # Justify left and right
      col.format <- which(colnames(print.object) %in% "variable")

      # Justify left and right
      print.object[, col.format] <- format(print.object[, col.format], justify = "left")
      print.object[, -col.format] <- apply(print.object[, -col.format], 2L, format, justify = "right")

      # Add blank space
      print.object[, "variable"] <- c(paste0("  ", print.object[1L, "variable"]), paste0("   ", print.object[-1L, "variable"]))
      print.object[, "variable"] <- format(c(print.object[1L, "variable"], misty::chr.trim(print.object[-1L, "variable"], side = "right")), justify = "left")

      # Missing values, NA
      print.object[, c("sd", "iqr", "skew", "kurt", "med", "low", "upp")] <- data.frame(apply(print.object[, c("sd", "iqr", "skew", "kurt", "med", "low", "upp")], 2L, function(y) gsub("NA", "  ", as.character(y))))

      #...................
      ### One Variable ####

      if (isTRUE(length(unique(x$result$variable)) == 1L)) {

        print.object <- print.object[, -1L]

        # First column "n"
        print.object[, "n"] <- c(paste0("  ", print.object[1L, "n"]), paste0("  ", print.object[-1L, "n"]))
        print.object[, "n"] <- format(c(print.object[1L, "n"], misty::chr.trim(print.object[-1L, "n"], side = "right")), justify = "right")

      }

      #...................
      ### Print Output ####

      cat(paste0(switch(x$args$alternative, two.sided = " Two-Sided ", less = " One-Sided ", greater = " One-Sided "),
                 round(x$args$conf.level * 100L, digits = 2L), ifelse(isTRUE(x$args$boot == "none"), "% Confidence Interval: ", "% Bootstrap Confidence Interval: "),
                 "Median\n\n"))

      .write.table(print.object, left = 1L, right = 3L, group = FALSE, horiz = horiz)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Grouping ####

    } else if (isTRUE(!is.null(x$data$group) && is.null(x$data$split))) {

      if (isTRUE(all(print.object$nNA == 0L))) {

        print.object[, c("sd", "iqr", "skew", "kurt", "med", "low", "upp")] <- sapply(c("sd", "iqr", "skew", "kurt", "med", "low", "upp"), function(y) ifelse(!is.na(print.object[, y]), formatC(print.object[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

      } else {

        print.object[, c("pNA", "iqr", "sd", "skew", "kurt", "med", "low", "upp")] <- sapply(c("pNA", "sd", "iqr", "skew", "kurt", "med", "low", "upp"), function(y) ifelse(!is.na(print.object[, y]), formatC(print.object[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

      }

      print.object[, "pNA"] <- paste0(print.object[, "pNA"], "%")

      #...................
      ### Column Names ####

      print.object <- rbind(c("Group", "Variable", "n", "nNA", "%NA", "SD", "IQR", "Skew", "Kurt", "Med", "Low", "Upp"), print.object)

      #...................
      ### Format ####

      # Remove duplicated values
      print.object[duplicated(print.object$group) , "group"] <- ""

      # Justify left and right
      col.format <- which(colnames(print.object) %in% c("group", "variable"))

      # Justify left and right
      print.object[, col.format] <- format(print.object[, col.format], justify = "left")
      print.object[, -col.format] <- apply(print.object[, -col.format], 2L, format, justify = "right")

      # Add blank space
      print.object[, "group"] <- c(paste0("  ", print.object[1L, "group"]), paste0("   ", print.object[-1L, "group"]))
      print.object[, "variable"] <- c(paste0("", print.object[1L, "variable"]), paste0(" ", print.object[-1L, "variable"]))

      print.object[, "group"] <- format(c(print.object[1L, "group"], misty::chr.trim(print.object[-1L, "group"], side = "right")), justify = "left")
      print.object[, "variable"] <- format(c(print.object[1L, "variable"], misty::chr.trim(print.object[-1L, "variable"], side = "right")), justify = "left")

      # Missing values, NA
      print.object[, c("sd", "iqr", "skew", "kurt", "med", "low", "upp")] <- data.frame(apply(print.object[, c("sd", "iqr", "skew", "kurt", "med", "low", "upp")], 2L, function(y) gsub("NA", "  ", as.character(y))))

      #...................
      ### One Variable ####

      if (isTRUE(length(unique(x$result$variable)) == 1L)) { print.object <- print.object[, -2L] }

      #...................
      ### Print Output ####

      cat(paste0(switch(x$args$alternative, two.sided = " Two-Sided ", less = " One-Sided ", greater = " One-Sided "),
                 round(x$args$conf.level * 100L, digits = 2L), ifelse(isTRUE(x$args$boot == "none"), "% Confidence Interval: ", "% Bootstrap Confidence Interval: "),
                 "Median\n\n"))

      .write.table(print.object, left = 1L, right = 3L, group = TRUE, result = x$result, horiz = horiz)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Split ####

    } else if (isTRUE(!is.null(x$data$split))) {

      # No grouping
      if (isTRUE(is.null(x$data$group))) {

        for (i in names(print.object)) {

          #...................
          ### Round ####

          if (isTRUE(all(print.object[[i]]$nNA == 0L))) {

            print.object[[i]][, c("sd", "iqr", "skew", "kurt", "med", "low", "upp")] <- sapply(c("sd", "iqr", "skew", "kurt", "med", "low", "upp"), function(y) ifelse(!is.na(print.object[[i]][, y]), formatC(print.object[[i]][, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

          } else {

            print.object[[i]][, c("pNA", "iqr", "sd", "skew", "kurt", "med", "low", "upp")] <- sapply(c("pNA", "sd", "iqr", "skew", "kurt", "med", "low", "upp"), function(y) ifelse(!is.na(print.object[[i]][, y]), formatC(print.object[[i]][, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

          }

          print.object[[i]][, "pNA"] <- paste0(print.object[[i]][, "pNA"], "%")

          #...................
          ### Column Names ####

          print.object[[i]] <- rbind(c("Variable", "n", "nNA", "%NA", "SD", "IQR", "Skew", "Kurt", "Med", "Low", "Upp"), print.object[[i]])

          #...................
          ### Format ####

          # Justify left and right
          col.format <- which(colnames(print.object[[i]]) %in% "variable")

          # Justify left and right
          print.object[[i]][, col.format] <- format(print.object[[i]][, col.format], justify = "left")
          print.object[[i]][, -col.format] <- apply(print.object[[i]][, -col.format], 2L, format, justify = "right")

          # Add blank space
          print.object[[i]][, "variable"] <- c(paste0("   ", print.object[[i]][1L, "variable"]), paste0("    ", print.object[[i]][-1L, "variable"]))
          print.object[[i]][, "variable"] <- format(c(print.object[[i]][1L, "variable"], misty::chr.trim(print.object[[i]][-1L, "variable"], side = "right")), justify = "left")

          # Missing values, NA
          print.object[[i]][, c("sd", "iqr", "skew", "kurt", "med", "low", "upp")] <- data.frame(apply(print.object[[i]][, c("sd", "iqr", "skew", "kurt", "med", "low", "upp")], 2L, function(y) gsub("NA", "  ", as.character(y))))

          #...................
          ### One Variable ####

          if (isTRUE(length(unique(x$result[[1L]]$variable)) == 1L)) {

            print.object[[i]] <- print.object[[i]][, -1L]

            print.object[[i]][, "n"] <- c(paste0("   ", print.object[[i]][1L, "n"]), paste0("   ", print.object[[i]][-1L, "n"]))
            print.object[[i]][, "n"] <- format(c(print.object[[i]][1L, "n"], misty::chr.trim(print.object[[i]][-1L, "n"], side = "right")), justify = "left")

          }

        }

      # Grouping
      } else {

        for (i in names(print.object)) {

          #...................
          ### Round ####

          if (isTRUE(all(print.object[[i]]$nNA == 0L))) {

            print.object[[i]][, c("sd", "iqr", "skew", "kurt", "med", "low", "upp")] <- sapply(c("sd", "iqr", "skew", "kurt", "med", "low", "upp"), function(y) ifelse(!is.na(print.object[[i]][, y]), formatC(print.object[[i]][, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

          } else {

            print.object[[i]][, c("pNA", "iqr", "sd", "skew", "kurt", "med", "low", "upp")] <- sapply(c("pNA", "sd", "iqr", "skew", "kurt", "med", "low", "upp"), function(y) ifelse(!is.na(print.object[[i]][, y]), formatC(print.object[[i]][, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

          }

          print.object[[i]][, "pNA"] <- paste0(print.object[[i]][, "pNA"], "%")

          #...................
          ### Column Names ####

          print.object[[i]] <- rbind(c("Group", "Variable", "n", "nNA", "%NA", "SD", "IQR", "Skew", "Kurt", "Med", "Low", "Upp"), print.object[[i]])

          #...................
          ### Format ####

          # Remove duplicated values
          print.object[[i]][duplicated(paste(print.object[[i]]$group, print.object[[i]]$variable)) , "variable"] <- ""
          print.object[[i]][duplicated(print.object[[i]]$group) , "group"] <- ""

          # Justify left and right
          col.format <- which(colnames(print.object[[i]]) %in% c("group", "variable"))

          # Justify left and right
          print.object[[i]][, col.format] <- format(print.object[[i]][, col.format], justify = "left")
          print.object[[i]][, -col.format] <- apply(print.object[[i]][, -col.format], 2L, format, justify = "right")

          # Add blank space
          print.object[[i]][, "group"] <- c(paste0("   ", print.object[[i]][1L, "group"]), paste0("    ", print.object[[i]][-1L, "group"]))
          print.object[[i]][, "variable"] <- c(paste0("", print.object[[i]][1L, "variable"]), paste0(" ", print.object[[i]][-1L, "variable"]))

          print.object[[i]][, "group"] <- format(c(print.object[[i]][1L, "group"], misty::chr.trim(print.object[[i]][-1L, "group"], side = "right")), justify = "left")
          print.object[[i]][, "variable"] <- format(c(print.object[[i]][1L, "variable"], misty::chr.trim(print.object[[i]][-1L, "variable"], side = "right")), justify = "left")

          # Missing values, NA
          print.object[[i]][, c("sd", "iqr", "skew", "kurt", "med", "low", "upp")] <- data.frame(apply(print.object[[i]][, c("sd", "iqr", "skew", "kurt", "med", "low", "upp")], 2L, function(y) gsub("NA", "  ", as.character(y))))

          #...................
          ### One Variable ####

          if (isTRUE(length(unique(x$result[[1L]]$variable)) == 1L)) { print.object[[i]] <- print.object[[i]][, -2L] }

        }

      }

      #...................
      ### Print Output ####

      cat(paste0(switch(x$args$alternative, two.sided = " Two-Sided ", less = " One-Sided ", greater = " One-Sided "),
                 round(x$args$conf.level * 100L, digits = 2L), ifelse(isTRUE(x$args$boot == "none"), "% Confidence Interval: ", "% Bootstrap Confidence Interval: "),
                 "Median\n\n"))

      for (i in names(print.object)) {

        cat("  Split Group:", i, "\n")

        .write.table(print.object[[i]], left = 2L, right = 4L, group = ifelse(isTRUE("group" %in% colnames(print.object[[i]])), TRUE, FALSE), result = x$result[[i]], horiz = horiz)

        if (isTRUE(i != names(print.object)[length(print.object)])) { cat("\n") }

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Note ####

    #...................
    ### Bootstrapping ####

    if (isTRUE(x$args$boot != "none")) {

      cat(paste0("\n  Note. ",
                 switch(x$args$boot,
                        "norm" = "Bias-corrected normal approximation bootstrap CI, ",
                        "basic" = "Basic bootstrap CI, ",
                        "stud" = "Studentized bootstrap CI, ",
                        "perc" = "Percentile bootstrap CI, ",
                        "bc" = "Bias-corrected (BC) percentile bootstrap CI, ",
                        "bca" = "Bias-corrected and accelerated (BCa) bootstrap CI, "),
                 x$args$R, " replications."))

    }

  #_____________________________________________________________________________
  #
  # Confidence Interval for the Propotion, ci.prop() ---------------------------
  }, ci.prop = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## No grouping ####

    if (isTRUE(is.null(x$data$group) && is.null(x$data$split))) {

      #...................
      ### Round ####

      if (isTRUE(all(print.object$nNA == 0L))) {

        print.object[, c("prop", "low", "upp")] <- sapply(c("prop", "low", "upp"), function(y) ifelse(!is.na(print.object[, y]), formatC(print.object[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

      } else {

        print.object[, c("prop", "low", "upp")] <- sapply(c("prop", "low", "upp"), function(y) ifelse(!is.na(print.object[, y]), formatC(print.object[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))
        print.object[, "pNA"] <- formatC(print.object[, "pNA"], digits = ifelse(digits - 1L < 0L, 0L, digits - 1L), format = "f", zero.print = ifelse(ifelse(digits - 1L < 0L, 0L, digits - 1L) > 0L, paste0("0.", paste(rep(0, times = ifelse(digits - 1L < 0L, 0L, digits - 1L)), collapse = "")), "0"))

      }

      print.object[, "pNA"] <- paste0(print.object[, "pNA"], "%")

      #...................
      ### Column Names ####

      print.object <- rbind(c("Variable", "n", "nNA", "%NA", "Freq", "Prop", "Low", "Upp"), print.object)

      #...................
      ### Format ####

      # Justify left and right
      col.format <- which(colnames(print.object) %in% "variable")

      # Justify left and right
      print.object[, col.format] <- format(print.object[, col.format], justify = "left")
      print.object[, -col.format] <- apply(print.object[, -col.format], 2L, format, justify = "right")

      # Add blank space
      print.object[, "variable"] <- c(paste0("  ", print.object[1L, "variable"]), paste0("   ", print.object[-1L, "variable"]))
      print.object[, "variable"] <- format(c(print.object[1L, "variable"], misty::chr.trim(print.object[-1L, "variable"], side = "right")), justify = "left")

      # Missing values, NA
      print.object[, c("freq", "prop", "low", "upp")] <- data.frame(apply(print.object[, c("freq", "prop", "low", "upp")], 2L, function(y) gsub("NA", "  ", as.character(y))))

      #...................
      ### One Variable ####

      if (isTRUE(length(unique(x$result$variable)) == 1L)) {

        print.object <- print.object[, -1L]

        # First column "n"
        print.object[, "n"] <- c(paste0("  ", print.object[1L, "n"]), paste0("  ", print.object[-1L, "n"]))
        print.object[, "n"] <- format(c(print.object[1L, "n"], misty::chr.trim(print.object[-1L, "n"], side = "right")), justify = "right")

      }

      #...................
      ### Print Output ####

      cat(paste0(switch(x$args$alternative, two.sided = " Two-Sided ", less = " One-Sided ", greater = " One-Sided "),
                 round(x$args$conf.level * 100L, digits = 2L), ifelse(isTRUE(x$args$boot == "none"), "% Confidence Interval: ", "% Bootstrap Confidence Interval: "),
                 "Proportion\n\n"))

      .write.table(print.object, left = 1L, right = 3L, group = FALSE, horiz = horiz)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Grouping ####

    } else if (isTRUE(!is.null(x$data$group) && is.null(x$data$split))) {

      if (isTRUE(all(print.object$nNA == 0L))) {

        print.object[, c("prop", "low", "upp")] <- sapply(c("prop", "low", "upp"), function(y) ifelse(!is.na(print.object[, y]), formatC(print.object[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

      } else {

        print.object[, c("prop", "low", "upp")] <- sapply(c("prop", "low", "upp"), function(y) ifelse(!is.na(print.object[, y]), formatC(print.object[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))
        print.object[, "pNA"] <- formatC(print.object[, "pNA"], digits = ifelse(digits - 1L < 0L, 0L, digits - 1L), format = "f", zero.print = ifelse(ifelse(digits - 1L < 0L, 0L, digits - 1L) > 0L, paste0("0.", paste(rep(0, times = ifelse(digits - 1L < 0L, 0L, digits - 1L)), collapse = "")), "0"))

      }

      print.object[, "pNA"] <- paste0(print.object[, "pNA"], "%")

      #...................
      ### Column Names ####

      print.object <- rbind(c("Group", "Variable", "n", "nNA", "%NA", "Freq", "Prop", "Low", "Upp"), print.object)

      #...................
      ### Format ####

      # Remove duplicated values
      print.object[duplicated(print.object$group) , "group"] <- ""

      # Justify left and right
      col.format <- which(colnames(print.object) %in% c("group", "variable"))

      # Justify left and right
      print.object[, col.format] <- format(print.object[, col.format], justify = "left")
      print.object[, -col.format] <- apply(print.object[, -col.format], 2L, format, justify = "right")

      # Add blank space
      print.object[, "group"] <- c(paste0("  ", print.object[1L, "group"]), paste0("   ", print.object[-1L, "group"]))
      print.object[, "variable"] <- c(paste0("", print.object[1L, "variable"]), paste0(" ", print.object[-1L, "variable"]))

      print.object[, "group"] <- format(c(print.object[1L, "group"], misty::chr.trim(print.object[-1L, "group"], side = "right")), justify = "left")
      print.object[, "variable"] <- format(c(print.object[1L, "variable"], misty::chr.trim(print.object[-1L, "variable"], side = "right")), justify = "left")

      # Missing values, NA
      print.object[, c("freq", "prop", "low", "upp")] <- data.frame(apply(print.object[, c("freq", "prop", "low", "upp")], 2L, function(y) gsub("NA", "  ", as.character(y))))

      #...................
      ### One Variable ####

      if (isTRUE(length(unique(x$result$variable)) == 1L)) { print.object <- print.object[, -2L] }

      #...................
      ### Print Output ####

      cat(paste0(switch(x$args$alternative, two.sided = " Two-Sided ", less = " One-Sided ", greater = " One-Sided "),
                 round(x$args$conf.level * 100L, digits = 2L), ifelse(isTRUE(x$args$boot == "none"), "% Confidence Interval: ", "% Bootstrap Confidence Interval: "),
                 "Proportion\n\n"))

      .write.table(print.object, left = 1L, right = 3L, group = TRUE, result = x$result, horiz = horiz)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Split ####

    } else if (isTRUE(!is.null(x$data$split))) {

      # No grouping
      if (isTRUE(is.null(x$data$group))) {

        for (i in names(print.object)) {

          #...................
          ### Round ####

          if (isTRUE(all(print.object[[i]]$nNA == 0L))) {

            print.object[[i]][, c("prop", "low", "upp")] <- sapply(c("prop", "low", "upp"), function(y) ifelse(!is.na(print.object[[i]][, y]), formatC(print.object[[i]][, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

          } else {

            print.object[[i]][, c("prop", "low", "upp")] <- sapply(c("prop", "low", "upp"), function(y) ifelse(!is.na(print.object[[i]][, y]), formatC(print.object[[i]][, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))
            print.object[[i]][, "pNA"] <- formatC(print.object[[i]][, "pNA"], digits = ifelse(digits - 1L < 0L, 0L, digits - 1L), format = "f", zero.print = ifelse(ifelse(digits - 1L < 0L, 0L, digits - 1L) > 0L, paste0("0.", paste(rep(0, times = ifelse(digits - 1L < 0L, 0L, digits - 1L)), collapse = "")), "0"))

          }

          print.object[[i]][, "pNA"] <- paste0(print.object[[i]][, "pNA"], "%")

          #...................
          ### Column Names ####

          print.object[[i]] <- rbind(c("Variable", "n", "nNA", "%NA", "Freq", "Prop", "Low", "Upp"), print.object[[i]])

          #...................
          ### Format ####

          # Justify left and right
          col.format <- which(colnames(print.object[[i]]) %in% "variable")

          # Justify left and right
          print.object[[i]][, col.format] <- format(print.object[[i]][, col.format], justify = "left")
          print.object[[i]][, -col.format] <- apply(print.object[[i]][, -col.format], 2L, format, justify = "right")

          # Add blank space
          print.object[[i]][, "variable"] <- c(paste0("   ", print.object[[i]][1L, "variable"]), paste0("    ", print.object[[i]][-1L, "variable"]))
          print.object[[i]][, "variable"] <- format(c(print.object[[i]][1L, "variable"], misty::chr.trim(print.object[[i]][-1L, "variable"], side = "right")), justify = "left")

          # Missing values, NA
          print.object[[i]][, c("freq", "prop", "low", "upp")] <- data.frame(apply(print.object[[i]][, c("freq", "prop", "low", "upp")], 2L, function(y) gsub("NA", "  ", as.character(y))))

          #...................
          ### One Variable ####

          if (isTRUE(length(unique(x$result[[1L]]$variable)) == 1L)) {

            print.object[[i]] <- print.object[[i]][, -1L]

            print.object[[i]][, "n"] <- c(paste0("   ", print.object[[i]][1L, "n"]), paste0("   ", print.object[[i]][-1L, "n"]))
            print.object[[i]][, "n"] <- format(c(print.object[[i]][1L, "n"], misty::chr.trim(print.object[[i]][-1L, "n"], side = "right")), justify = "left")

          }

        }

      # Grouping
      } else {

        for (i in names(print.object)) {

          #...................
          ### Round ####

          if (isTRUE(all(print.object[[i]]$nNA == 0L))) {

            print.object[[i]][, c("prop", "low", "upp")] <- sapply(c("prop", "low", "upp"), function(y) ifelse(!is.na(print.object[[i]][, y]), formatC(print.object[[i]][, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

          } else {

            print.object[[i]][, c("prop", "low", "upp")] <- sapply(c("prop", "low", "upp"), function(y) ifelse(!is.na(print.object[[i]][, y]), formatC(print.object[[i]][, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))
            print.object[[i]][, "pNA"] <- formatC(print.object[[i]][, "pNA"], digits = ifelse(digits - 1L < 0L, 0L, digits - 1L), format = "f", zero.print = ifelse(ifelse(digits - 1L < 0L, 0L, digits - 1L) > 0L, paste0("0.", paste(rep(0, times = ifelse(digits - 1L < 0L, 0L, digits - 1L)), collapse = "")), "0"))

          }

          print.object[[i]][, "pNA"] <- paste0(print.object[[i]][, "pNA"], "%")

          #...................
          ### Column Names ####

          print.object[[i]] <- rbind(c("Group", "Variable", "n", "nNA", "%NA", "Freq", "Prop", "Low", "Upp"), print.object[[i]])

          #...................
          ### Format ####

          # Remove duplicated values
          print.object[[i]][duplicated(paste(print.object[[i]]$group, print.object[[i]]$variable)) , "variable"] <- ""
          print.object[[i]][duplicated(print.object[[i]]$group) , "group"] <- ""

          # Justify left and right
          col.format <- which(colnames(print.object[[i]]) %in% c("group", "variable"))

          # Justify left and right
          print.object[[i]][, col.format] <- format(print.object[[i]][, col.format], justify = "left")
          print.object[[i]][, -col.format] <- apply(print.object[[i]][, -col.format], 2L, format, justify = "right")

          # Add blank space
          print.object[[i]][, "group"] <- c(paste0("   ", print.object[[i]][1L, "group"]), paste0("    ", print.object[[i]][-1L, "group"]))
          print.object[[i]][, "variable"] <- c(paste0("", print.object[[i]][1L, "variable"]), paste0(" ", print.object[[i]][-1L, "variable"]))

          print.object[[i]][, "group"] <- format(c(print.object[[i]][1L, "group"], misty::chr.trim(print.object[[i]][-1L, "group"], side = "right")), justify = "left")
          print.object[[i]][, "variable"] <- format(c(print.object[[i]][1L, "variable"], misty::chr.trim(print.object[[i]][-1L, "variable"], side = "right")), justify = "left")

          # Missing values, NA
          print.object[[i]][, c("prop", "low", "upp")] <- data.frame(apply(print.object[[i]][, c("prop", "low", "upp")], 2L, function(y) gsub("NA", "  ", as.character(y))))

          #...................
          ### One Variable ####

          if (isTRUE(length(unique(x$result[[1L]]$variable)) == 1L)) { print.object[[i]] <- print.object[[i]][, -2L] }

        }

      }

      #...................
      ### Print Output ####

      cat(paste0(switch(x$args$alternative, two.sided = " Two-Sided ", less = " One-Sided ", greater = " One-Sided "),
                 round(x$args$conf.level * 100L, digits = 2L), ifelse(isTRUE(x$args$boot == "none"), "% Confidence Interval: ", "% Bootstrap Confidence Interval: "),
                 "Proportion\n\n"))

      for (i in names(print.object)) {

        cat("  Split Group:", i, "\n")

        .write.table(print.object[[i]], left = 2L, right = 4L, group = ifelse(isTRUE("group" %in% colnames(print.object[[i]])), TRUE, FALSE), result = x$result[[i]], horiz = horiz)

        if (isTRUE(i != names(print.object)[length(print.object)])) { cat("\n") }

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Note ####

    #...................
    ### Bootstrapping ####

    if (isTRUE(x$args$boot != "none")) {

      cat(paste0("\n  Note. ",
                 switch(x$args$boot,
                        "perc" = "Percentile bootstrap CI, ",
                        "bc" = "Bias-corrected (BC) percentile bootstrap CI, ",
                        "bca" = "Bias-corrected and accelerated (BCa) bootstrap CI, "),
                 x$args$R, " replications."))

    }

  #_____________________________________________________________________________
  #
  # Confidence Interval for the Variance, ci.var() -----------------------------
  }, ci.var = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## No grouping ####

    if (isTRUE(is.null(x$data$group) && is.null(x$data$split))) {

      #...................
      ### Round ####

      if (isTRUE(all(print.object$nNA == 0L))) {

        print.object[, c("skew", "kurt", "m", "var", "low", "upp")] <- sapply(c("skew", "kurt", "m", "var", "low", "upp"), function(y) ifelse(!is.na(print.object[, y]), formatC(print.object[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

      } else {

        print.object[, c("pNA", "skew", "kurt", "m", "var", "low", "upp")] <- sapply(c("pNA", "skew", "kurt", "m", "var", "low", "upp"), function(y) ifelse(!is.na(print.object[, y]), formatC(print.object[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

      }

      print.object[, "pNA"] <- paste0(print.object[, "pNA"], "%")

      #...................
      ### Column Names ####

      print.object <- rbind(c("Variable", "n", "nNA", "%NA", "Skew", "Kurt", "M", "Var", "Low", "Upp"), print.object)

      #...................
      ### Format ####

      # Justify left and right
      col.format <- which(colnames(print.object) %in% "variable")

      # Justify left and right
      print.object[, col.format] <- format(print.object[, col.format], justify = "left")
      print.object[, -col.format] <- apply(print.object[, -col.format], 2L, format, justify = "right")

      # Add blank space
      print.object[, "variable"] <- c(paste0("  ", print.object[1L, "variable"]), paste0("   ", print.object[-1L, "variable"]))
      print.object[, "variable"] <- format(c(print.object[1L, "variable"], misty::chr.trim(print.object[-1L, "variable"], side = "right")), justify = "left")

      # Missing values, NA
      print.object[, c("skew", "kurt", "m", "var", "low", "upp")] <- data.frame(apply(print.object[, c("skew", "kurt", "m", "var", "low", "upp")], 2L, function(y) gsub("NA", "  ", as.character(y))))

      #...................
      ### One Variable ####

      if (isTRUE(length(unique(x$result$variable)) == 1L)) {

        print.object <- print.object[, -1L]

        # First column "n"
        print.object[, "n"] <- c(paste0("  ", print.object[1L, "n"]), paste0("  ", print.object[-1L, "n"]))
        print.object[, "n"] <- format(c(print.object[1L, "n"], misty::chr.trim(print.object[-1L, "n"], side = "right")), justify = "right")

      }

      #...................
      ### Print Output ####

      cat(paste0(switch(x$args$alternative, two.sided = " Two-Sided ", less = " One-Sided ", greater = " One-Sided "),
                 round(x$args$conf.level * 100L, digits = 2L), ifelse(isTRUE(x$args$boot == "none"), "% Confidence Interval: ", "% Bootstrap Confidence Interval: "),
                 "Variance\n\n"))

      .write.table(print.object, left = 1L, right = 3L, group = FALSE, horiz = horiz)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Grouping ####

    } else if (isTRUE(!is.null(x$data$group) && is.null(x$data$split))) {

      if (isTRUE(all(print.object$nNA == 0L))) {

        print.object[, c("skew", "kurt", "m", "var", "low", "upp")] <- sapply(c("skew", "kurt", "m", "var", "low", "upp"), function(y) ifelse(!is.na(print.object[, y]), formatC(print.object[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

      } else {

        print.object[, c("pNA", "skew", "kurt", "m", "var", "low", "upp")] <- sapply(c("pNA", "skew", "kurt", "m", "var", "low", "upp"), function(y) ifelse(!is.na(print.object[, y]), formatC(print.object[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

      }

      print.object[, "pNA"] <- paste0(print.object[, "pNA"], "%")

      #...................
      ### Column Names ####

      print.object <- rbind(c("Group", "Variable", "n", "nNA", "%NA", "Skew", "Kurt", "M", "Var", "Low", "Upp"), print.object)

      #...................
      ### Format ####

      # Remove duplicated values
      print.object[duplicated(print.object$group) , "group"] <- ""

      # Justify left and right
      col.format <- which(colnames(print.object) %in% c("group", "variable"))

      # Justify left and right
      print.object[, col.format] <- format(print.object[, col.format], justify = "left")
      print.object[, -col.format] <- apply(print.object[, -col.format], 2L, format, justify = "right")

      # Add blank space
      print.object[, "group"] <- c(paste0("  ", print.object[1L, "group"]), paste0("   ", print.object[-1L, "group"]))
      print.object[, "variable"] <- c(paste0("", print.object[1L, "variable"]), paste0(" ", print.object[-1L, "variable"]))

      print.object[, "group"] <- format(c(print.object[1L, "group"], misty::chr.trim(print.object[-1L, "group"], side = "right")), justify = "left")
      print.object[, "variable"] <- format(c(print.object[1L, "variable"], misty::chr.trim(print.object[-1L, "variable"], side = "right")), justify = "left")

      # Missing values, NA
      print.object[, c("skew", "kurt", "m", "var", "low", "upp")] <- data.frame(apply(print.object[, c("skew", "kurt", "m", "var", "low", "upp")], 2L, function(y) gsub("NA", "  ", as.character(y))))

      #...................
      ### One Variable ####

      if (isTRUE(length(unique(x$result$variable)) == 1L)) { print.object <- print.object[, -2L] }

      #...................
      ### Print Output ####

      cat(paste0(switch(x$args$alternative, two.sided = " Two-Sided ", less = " One-Sided ", greater = " One-Sided "),
                 round(x$args$conf.level * 100L, digits = 2L), ifelse(isTRUE(x$args$boot == "none"), "% Confidence Interval: ", "% Bootstrap Confidence Interval: "),
                 "Variance\n\n"))

      .write.table(print.object, left = 1L, right = 3L, group = TRUE, result = x$result, horiz = horiz)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Split ####

    } else if (isTRUE(!is.null(x$data$split))) {

      # No grouping
      if (isTRUE(is.null(x$data$group))) {

        for (i in names(print.object)) {

          #...................
          ### Round ####

          if (isTRUE(all(print.object[[i]]$nNA == 0L))) {

            print.object[[i]][, c("skew", "kurt", "m", "var", "low", "upp")] <- sapply(c("skew", "kurt", "m", "var", "low", "upp"), function(y) ifelse(!is.na(print.object[[i]][, y]), formatC(print.object[[i]][, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

          } else {

            print.object[[i]][, c("pNA", "skew", "kurt", "m", "var", "low", "upp")] <- sapply(c("pNA", "skew", "kurt", "m", "var", "low", "upp"), function(y) ifelse(!is.na(print.object[[i]][, y]), formatC(print.object[[i]][, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

          }

          print.object[[i]][, "pNA"] <- paste0(print.object[[i]][, "pNA"], "%")

          #...................
          ### Column Names ####

          print.object[[i]] <- rbind(c("Variable", "n", "nNA", "%NA", "Skew", "Kurt", "M", "Var", "Low", "Upp"), print.object[[i]])

          #...................
          ### Format ####

          # Justify left and right
          col.format <- which(colnames(print.object[[i]]) %in% "variable")

          # Justify left and right
          print.object[[i]][, col.format] <- format(print.object[[i]][, col.format], justify = "left")
          print.object[[i]][, -col.format] <- apply(print.object[[i]][, -col.format], 2L, format, justify = "right")

          # Add blank space
          print.object[[i]][, "variable"] <- c(paste0("   ", print.object[[i]][1L, "variable"]), paste0("    ", print.object[[i]][-1L, "variable"]))
          print.object[[i]][, "variable"] <- format(c(print.object[[i]][1L, "variable"], misty::chr.trim(print.object[[i]][-1L, "variable"], side = "right")), justify = "left")

          # Missing values, NA
          print.object[[i]][, c("skew", "kurt", "m", "var", "low", "upp")] <- data.frame(apply(print.object[[i]][, c("skew", "kurt", "m", "var", "low", "upp")], 2L, function(y) gsub("NA", "  ", as.character(y))))

          #...................
          ### One Variable ####

          if (isTRUE(length(unique(x$result[[1L]]$variable)) == 1L)) {

            print.object[[i]] <- print.object[[i]][, -1L]

            print.object[[i]][, "n"] <- c(paste0("   ", print.object[[i]][1L, "n"]), paste0("   ", print.object[[i]][-1L, "n"]))
            print.object[[i]][, "n"] <- format(c(print.object[[i]][1L, "n"], misty::chr.trim(print.object[[i]][-1L, "n"], side = "right")), justify = "left")

          }

        }

      # Grouping
      } else {

        for (i in names(print.object)) {

          #...................
          ### Round ####

          if (isTRUE(all(print.object[[i]]$nNA == 0L))) {

            print.object[[i]][, c("skew", "kurt", "m", "var", "low", "upp")] <- sapply(c("skew", "kurt", "m", "var", "low", "upp"), function(y) ifelse(!is.na(print.object[[i]][, y]), formatC(print.object[[i]][, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

          } else {

            print.object[[i]][, c("pNA", "skew", "kurt", "m", "var", "low", "upp")] <- sapply(c("pNA", "skew", "kurt", "m", "var", "low", "upp"), function(y) ifelse(!is.na(print.object[[i]][, y]), formatC(print.object[[i]][, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

          }

          print.object[[i]][, "pNA"] <- paste0(print.object[[i]][, "pNA"], "%")

          #...................
          ### Column Names ####

          print.object[[i]] <- rbind(c("Group", "Variable", "n", "nNA", "%NA", "Skew", "Kurt", "M", "Var", "Low", "Upp"), print.object[[i]])

          #...................
          ### Format ####

          # Remove duplicated values
          print.object[[i]][duplicated(paste(print.object[[i]]$group, print.object[[i]]$variable)) , "variable"] <- ""
          print.object[[i]][duplicated(print.object[[i]]$group) , "group"] <- ""

          # Justify left and right
          col.format <- which(colnames(print.object[[i]]) %in% c("group", "variable"))

          # Justify left and right
          print.object[[i]][, col.format] <- format(print.object[[i]][, col.format], justify = "left")
          print.object[[i]][, -col.format] <- apply(print.object[[i]][, -col.format], 2L, format, justify = "right")

          # Add blank space
          print.object[[i]][, "group"] <- c(paste0("   ", print.object[[i]][1L, "group"]), paste0("    ", print.object[[i]][-1L, "group"]))
          print.object[[i]][, "variable"] <- c(paste0("", print.object[[i]][1L, "variable"]), paste0(" ", print.object[[i]][-1L, "variable"]))

          print.object[[i]][, "group"] <- format(c(print.object[[i]][1L, "group"], misty::chr.trim(print.object[[i]][-1L, "group"], side = "right")), justify = "left")
          print.object[[i]][, "variable"] <- format(c(print.object[[i]][1L, "variable"], misty::chr.trim(print.object[[i]][-1L, "variable"], side = "right")), justify = "left")

          # Missing values, NA
          print.object[[i]][, c("skew", "kurt", "m", "var", "low", "upp")] <- data.frame(apply(print.object[[i]][, c("skew", "kurt", "m", "var", "low", "upp")], 2L, function(y) gsub("NA", "  ", as.character(y))))

          #...................
          ### One Variable ####

          if (isTRUE(length(unique(x$result[[1L]]$variable)) == 1L)) { print.object[[i]] <- print.object[[i]][, -2L] }

        }

      }

      #...................
      ### Print Output ####

      cat(paste0(switch(x$args$alternative, two.sided = " Two-Sided ", less = " One-Sided ", greater = " One-Sided "),
                 round(x$args$conf.level * 100L, digits = 2L), ifelse(isTRUE(x$args$boot == "none"), "% Confidence Interval: ", "% Bootstrap Confidence Interval: "),
                 "Variance\n\n"))

      for (i in names(print.object)) {

        cat("  Split Group:", i, "\n")

        .write.table(print.object[[i]], left = 2L, right = 4L, group = ifelse(isTRUE("group" %in% colnames(print.object[[i]])), TRUE, FALSE), result = x$result[[i]], horiz = horiz)

        if (isTRUE(i != names(print.object)[length(print.object)])) { cat("\n") }

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Note ####

    #...................
    ### Bootstrapping ####

    if (isTRUE(x$args$boot != "none")) {

      cat(paste0("\n  Note. ",
                 switch(x$args$boot,
                        "perc" = "Percentile bootstrap CI, ",
                        "bc" = "Bias-corrected (BC) percentile bootstrap CI, ",
                        "bca" = "Bias-corrected and accelerated (BCa) bootstrap CI, "),
                 x$args$R, " replications."))

    }

  #_____________________________________________________________________________
  #
  # Confidence Interval for the Standard Deviation, ci.sd() --------------------
  }, ci.sd = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## No grouping ####

    if (isTRUE(is.null(x$data$group) && is.null(x$data$split))) {

      #...................
      ### Round ####

      if (isTRUE(all(print.object$nNA == 0L))) {

        print.object[, c("skew", "kurt", "m", "sd", "low", "upp")] <- sapply(c("skew", "kurt", "m", "sd", "low", "upp"), function(y) ifelse(!is.na(print.object[, y]), formatC(print.object[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

      } else {

        print.object[, c("pNA", "skew", "kurt", "m", "sd", "low", "upp")] <- sapply(c("pNA", "skew", "kurt", "m", "sd", "low", "upp"), function(y) ifelse(!is.na(print.object[, y]), formatC(print.object[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

      }

      print.object[, "pNA"] <- paste0(print.object[, "pNA"], "%")

      #...................
      ### Column Names ####

      print.object <- rbind(c("Variable", "n", "nNA", "%NA", "Skew", "Kurt", "M", "SD", "Low", "Upp"), print.object)

      #...................
      ### Format ####

      # Justify left and right
      col.format <- which(colnames(print.object) %in% "variable")

      # Justify left and right
      print.object[, col.format] <- format(print.object[, col.format], justify = "left")
      print.object[, -col.format] <- apply(print.object[, -col.format], 2L, format, justify = "right")

      # Add blank space
      print.object[, "variable"] <- c(paste0("  ", print.object[1L, "variable"]), paste0("   ", print.object[-1L, "variable"]))
      print.object[, "variable"] <- format(c(print.object[1L, "variable"], misty::chr.trim(print.object[-1L, "variable"], side = "right")), justify = "left")

      # Missing values, NA
      print.object[, c("skew", "kurt", "m", "sd", "low", "upp")] <- data.frame(apply(print.object[, c("skew", "kurt", "m", "sd", "low", "upp")], 2L, function(y) gsub("NA", "  ", as.character(y))))

      #...................
      ### One Variable ####

      if (isTRUE(length(unique(x$result$variable)) == 1L)) {

        print.object <- print.object[, -1L]

        # First column "n"
        print.object[, "n"] <- c(paste0("  ", print.object[1L, "n"]), paste0("  ", print.object[-1L, "n"]))
        print.object[, "n"] <- format(c(print.object[1L, "n"], misty::chr.trim(print.object[-1L, "n"], side = "right")), justify = "right")

      }

      #...................
      ### Print Output ####

      cat(paste0(switch(x$args$alternative, two.sided = " Two-Sided ", less = " One-Sided ", greater = " One-Sided "),
                 round(x$args$conf.level * 100L, digits = 2L), ifelse(isTRUE(x$args$boot == "none"), "% Confidence Interval: ", "% Bootstrap Confidence Interval: "),
                 "Standard Deviation\n\n"))

      .write.table(print.object, left = 1L, right = 3L, group = FALSE, horiz = horiz)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Grouping ####

    } else if (isTRUE(!is.null(x$data$group) && is.null(x$data$split))) {

      if (isTRUE(all(print.object$nNA == 0L))) {

        print.object[, c("skew", "kurt", "m", "sd", "low", "upp")] <- sapply(c("skew", "kurt", "m", "sd", "low", "upp"), function(y) ifelse(!is.na(print.object[, y]), formatC(print.object[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

      } else {

        print.object[, c("pNA", "skew", "kurt", "m", "sd", "low", "upp")] <- sapply(c("pNA", "skew", "kurt", "m", "sd", "low", "upp"), function(y) ifelse(!is.na(print.object[, y]), formatC(print.object[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

      }

      print.object[, "pNA"] <- paste0(print.object[, "pNA"], "%")

      #...................
      ### Column Names ####

      print.object <- rbind(c("Group", "Variable", "n", "nNA", "%NA", "Skew", "Kurt", "M", "SD", "Low", "Upp"), print.object)

      #...................
      ### Format ####

      # Remove duplicated values
      print.object[duplicated(print.object$group) , "group"] <- ""

      # Justify left and right
      col.format <- which(colnames(print.object) %in% c("group", "variable"))

      # Justify left and right
      print.object[, col.format] <- format(print.object[, col.format], justify = "left")
      print.object[, -col.format] <- apply(print.object[, -col.format], 2L, format, justify = "right")

      # Add blank space
      print.object[, "group"] <- c(paste0("  ", print.object[1L, "group"]), paste0("   ", print.object[-1L, "group"]))
      print.object[, "variable"] <- c(paste0("", print.object[1L, "variable"]), paste0(" ", print.object[-1L, "variable"]))

      print.object[, "group"] <- format(c(print.object[1L, "group"], misty::chr.trim(print.object[-1L, "group"], side = "right")), justify = "left")
      print.object[, "variable"] <- format(c(print.object[1L, "variable"], misty::chr.trim(print.object[-1L, "variable"], side = "right")), justify = "left")

      # Missing values, NA
      print.object[, c("skew", "kurt", "m", "sd", "low", "upp")] <- data.frame(apply(print.object[, c("skew", "kurt", "m", "sd", "low", "upp")], 2L, function(y) gsub("NA", "  ", as.character(y))))

      #...................
      ### One Variable ####

      if (isTRUE(length(unique(x$result$variable)) == 1L)) { print.object <- print.object[, -2L] }

      #...................
      ### Print Output ####

      cat(paste0(switch(x$args$alternative, two.sided = " Two-Sided ", less = " One-Sided ", greater = " One-Sided "),
                 round(x$args$conf.level * 100L, digits = 2L), ifelse(isTRUE(x$args$boot == "none"), "% Confidence Interval: ", "% Bootstrap Confidence Interval: "),
                 "Standard Deviation\n\n"))

      .write.table(print.object, left = 1L, right = 3L, group = TRUE, result = x$result, horiz = horiz)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Split ####

    } else if (isTRUE(!is.null(x$data$split))) {

      # No grouping
      if (isTRUE(is.null(x$data$group))) {

        for (i in names(print.object)) {

          #...................
          ### Round ####

          if (isTRUE(all(print.object[[i]]$nNA == 0L))) {

            print.object[[i]][, c("skew", "kurt", "m", "sd", "low", "upp")] <- sapply(c("skew", "kurt", "m", "sd", "low", "upp"), function(y) ifelse(!is.na(print.object[[i]][, y]), formatC(print.object[[i]][, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

          } else {

            print.object[[i]][, c("pNA", "skew", "kurt", "m", "sd", "low", "upp")] <- sapply(c("pNA", "skew", "kurt", "m", "sd", "low", "upp"), function(y) ifelse(!is.na(print.object[[i]][, y]), formatC(print.object[[i]][, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

          }

          print.object[[i]][, "pNA"] <- paste0(print.object[[i]][, "pNA"], "%")

          #...................
          ### Column Names ####

          print.object[[i]] <- rbind(c("Variable", "n", "nNA", "%NA", "Skew", "Kurt", "M", "SD", "Low", "Upp"), print.object[[i]])

          #...................
          ### Format ####

          # Justify left and right
          col.format <- which(colnames(print.object[[i]]) %in% "variable")

          # Justify left and right
          print.object[[i]][, col.format] <- format(print.object[[i]][, col.format], justify = "left")
          print.object[[i]][, -col.format] <- apply(print.object[[i]][, -col.format], 2L, format, justify = "right")

          # Add blank space
          print.object[[i]][, "variable"] <- c(paste0("   ", print.object[[i]][1L, "variable"]), paste0("    ", print.object[[i]][-1L, "variable"]))
          print.object[[i]][, "variable"] <- format(c(print.object[[i]][1L, "variable"], misty::chr.trim(print.object[[i]][-1L, "variable"], side = "right")), justify = "left")

          # Missing values, NA
          print.object[[i]][, c("skew", "kurt", "m", "sd", "low", "upp")] <- data.frame(apply(print.object[[i]][, c("skew", "kurt", "m", "sd", "low", "upp")], 2L, function(y) gsub("NA", "  ", as.character(y))))

          #...................
          ### One Variable ####

          if (isTRUE(length(unique(x$result[[1L]]$variable)) == 1L)) {

            print.object[[i]] <- print.object[[i]][, -1L]

            print.object[[i]][, "n"] <- c(paste0("   ", print.object[[i]][1L, "n"]), paste0("   ", print.object[[i]][-1L, "n"]))
            print.object[[i]][, "n"] <- format(c(print.object[[i]][1L, "n"], misty::chr.trim(print.object[[i]][-1L, "n"], side = "right")), justify = "left")

          }

        }

      # Grouping
      } else {

        for (i in names(print.object)) {

          #...................
          ### Round ####

          if (isTRUE(all(print.object[[i]]$nNA == 0L))) {

            print.object[[i]][, c("skew", "kurt", "m", "sd", "low", "upp")] <- sapply(c("skew", "kurt", "m", "sd", "low", "upp"), function(y) ifelse(!is.na(print.object[[i]][, y]), formatC(print.object[[i]][, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

          } else {

            print.object[[i]][, c("pNA", "skew", "kurt", "m", "sd", "low", "upp")] <- sapply(c("pNA", "skew", "kurt", "m", "sd", "low", "upp"), function(y) ifelse(!is.na(print.object[[i]][, y]), formatC(print.object[[i]][, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

          }

          print.object[[i]][, "pNA"] <- paste0(print.object[[i]][, "pNA"], "%")

          #...................
          ### Column Names ####

          print.object[[i]] <- rbind(c("Group", "Variable", "n", "nNA", "%NA", "Skew", "Kurt", "M", "SD", "Low", "Upp"), print.object[[i]])

          #...................
          ### Format ####

          # Remove duplicated values
          print.object[[i]][duplicated(paste(print.object[[i]]$group, print.object[[i]]$variable)) , "variable"] <- ""
          print.object[[i]][duplicated(print.object[[i]]$group) , "group"] <- ""

          # Justify left and right
          col.format <- which(colnames(print.object[[i]]) %in% c("group", "variable"))

          # Justify left and right
          print.object[[i]][, col.format] <- format(print.object[[i]][, col.format], justify = "left")
          print.object[[i]][, -col.format] <- apply(print.object[[i]][, -col.format], 2L, format, justify = "right")

          # Add blank space
          print.object[[i]][, "group"] <- c(paste0("   ", print.object[[i]][1L, "group"]), paste0("    ", print.object[[i]][-1L, "group"]))
          print.object[[i]][, "variable"] <- c(paste0("", print.object[[i]][1L, "variable"]), paste0(" ", print.object[[i]][-1L, "variable"]))

          print.object[[i]][, "group"] <- format(c(print.object[[i]][1L, "group"], misty::chr.trim(print.object[[i]][-1L, "group"], side = "right")), justify = "left")
          print.object[[i]][, "variable"] <- format(c(print.object[[i]][1L, "variable"], misty::chr.trim(print.object[[i]][-1L, "variable"], side = "right")), justify = "left")

          # Missing values, NA
          print.object[[i]][, c("skew", "kurt", "m", "sd", "low", "upp")] <- data.frame(apply(print.object[[i]][, c("skew", "kurt", "m", "sd", "low", "upp")], 2L, function(y) gsub("NA", "  ", as.character(y))))

          #...................
          ### One Variable ####

          if (isTRUE(length(unique(x$result[[1L]]$variable)) == 1L)) { print.object[[i]] <- print.object[[i]][, -2L] }

        }

      }

      #...................
      ### Print Output ####

      cat(paste0(switch(x$args$alternative, two.sided = " Two-Sided ", less = " One-Sided ", greater = " One-Sided "),
                 round(x$args$conf.level * 100L, digits = 2L), ifelse(isTRUE(x$args$boot == "none"), "% Confidence Interval: ", "% Bootstrap Confidence Interval: "),
                 "Standard Deviation\n\n"))

      for (i in names(print.object)) {

        cat("  Split Group:", i, "\n")

        .write.table(print.object[[i]], left = 2L, right = 4L, group = ifelse(isTRUE("group" %in% colnames(print.object[[i]])), TRUE, FALSE), result = x$result[[i]], horiz = horiz)

        if (isTRUE(i != names(print.object)[length(print.object)])) { cat("\n") }

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Note ####

    #...................
    ### Bootstrapping ####

    if (isTRUE(x$args$boot != "none")) {

      cat(paste0("\n  Note. ",
                 switch(x$args$boot,
                        "perc" = "Percentile bootstrap CI, ",
                        "bc" = "Bias-corrected (BC) percentile bootstrap CI, ",
                        "bca" = "Bias-corrected and accelerated (BCa) bootstrap CI, "),
                 x$args$R, " replications."))

    }

  #_____________________________________________________________________________
  #
  # Confidence intervals, ci() -------------------------------------------------
  }, ci = {

    #......
    # Variables to round
    print.round <- switch(x$ci,
                          mean.w = c("m", "sd", "se", "low", "upp"),
                          mean.diff.o = c("m", "sd", "mu", "m.diff", "low", "upp"),
                          mean.diff.i = c("m", "sd", "m.diff", "low", "upp"),
                          mean.diff.p = c("m1", "sd1", "m2", "sd2", "m.diff", "sd.diff", "low", "upp"),
                          prop.diff.i = c("p", "p.diff", "low", "upp"),
                          prop.diff.p = c("p1", "p2", "p.diff", "low", "upp"))

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## No grouping ####

    if (isTRUE(is.null(x$data$group) && is.null(x$data$split))) {

      #......
      # Print names
      print.names <- switch(x$ci,
                            mean.w = c("Variable", "n", "nNA", "%NA", "M", "SD",  "SE", "Low", "Upp"),
                            mean.diff.o = c("Variable", "n", "nNA", "M", "SD", "Mu", "M.Diff", "Low", "Upp"),
                            mean.diff.i = c("Variable", "Between", "n", "nNA", "M", "SD", "M.Diff", "Low", "Upp"),
                            mean.diff.p = c("Variable", "n", "nNA", "M1", "SD1", "M2", "SD2", "M.Diff", "SD.Diff", "Low", "Upp"),
                            prop.diff.i = c("Variable", "Between", "n", "nNA", "p", "p.Diff", "Low", "Upp"),
                            prop.diff.p = c("Variable", "n", "nNA", "p1", "p2", "p.Diff", "Low", "Upp"))

      #......
      # Round
      print.object[, print.round] <- sapply(print.round, function(y) ifelse(!is.na(print.object[, y]), formatC(print.object[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

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
        if (isTRUE(x$ci %in% c("mean.diff.o", "mean.diff.p", "prop.diff.p"))) {

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
                       mean.w = "Arithmetic Mean\n\n",
                       mean.diff.o = "Difference in Means from a Population Value\n\n",
                       mean.diff.i = "Difference in Means from Independent Samples\n\n",
                       mean.diff.p = "Difference in Means from Paired Samples\n\n",
                       prop.diff.i = "Difference in Proportions from Independent Samples\n\n",
                       prop.diff.p = "Difference in Proportions from Paired Samples\n\n")))

      .write.table(print.object, left = 1L, right = 3L, group = FALSE, horiz = horiz)

      #......
      # Notes

      # Arithmetic mean
      if (isTRUE(x$ci == "mean")) {

        if (isTRUE(!is.null(x$args$sigma))) { cat(paste0("\n  Note. Known population SD: Sigma = ", round(x$args$sigma, digits = 2L), "\n")) }

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
                            mean.diff.i = c("Group", "Variable", "Between", "n", "nNA", "M", "SD", "M.Diff", "Low", "Upp"),
                            mean.diff.p = c("Group", "Variable", "n", "nNA", "M1", "SD1", "M2", "SD2", "M.Diff", "SD.Diff", "Low", "Upp"),
                            prop.diff.i = c("Group", "Variable", "Between", "n", "nNA", "p", "p.Diff", "Low", "Upp"),
                            prop.diff.p = c("Group", "Variable", "n", "nNA", "p1", "p2", "p.Diff", "Low", "Upp"))

      #......
      # Sort by variables
      if (isTRUE(sort.var)) {

        print.object <- print.object[order(print.object[, "variable"]), ]

      }

      #......
      # Round
      print.object[, print.round] <- sapply(print.round, function(y) ifelse(!is.na(print.object[, y]), formatC(print.object[, y], digits = digits, format = "f", zero.print = paste0("0.", paste(rep(0L, times = digits), collapse = ""))), NA))

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
                       mean.diff.i = "Difference in Means from Independent Samples\n\n",
                       mean.diff.p = "Difference in Means from Paired Samples\n\n",
                       prop.diff.i = "Difference in Proportions from Independent Samples\n\n",
                       prop.diff.p = "Difference in Proportions from Paired Samples\n\n")))

      .write.table(print.object, left = 1L, right = 3L, group = TRUE, result = x$result, horiz = horiz)

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
                                mean.diff.i = c("Variable", "Between", "n", "nNA", "M", "SD", "M.Diff", "Low", "Upp"),
                                mean.diff.p = c("Variable", "n", "nNA", "M1", "SD1", "M2", "SD2", "M.Diff", "SD.Diff", "Low", "Upp"),
                                prop.diff.i = c("Variable", "Between", "n", "nNA", "p", "p.Diff", "Low", "Upp"),
                                prop.diff.p = c("Variable", "n", "nNA", "p1", "p2", "p.Diff", "Low", "Upp"))

          #......
          # Round
          print.object[[i]][, print.round] <- sapply(print.round, function(y) ifelse(!is.na(print.object[[i]][, y]), formatC(print.object[[i]][, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

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
                                mean.diff.i = c("Group", "Variable", "Between", "n", "nNA", "M", "SD", "M.Diff", "Low", "Upp"),
                                mean.diff.p = c("Group", "Variable", "n", "nNA", "M1", "SD1", "M2", "SD2", "M.Diff", "SD.Diff", "Low", "Upp"),
                                prop.diff.i = c("Group", "Variable", "Beween", "n", "nNA", "p", "p.Diff", "Low", "Upp"),
                                prop.diff.p = c("Group", "Variable", "n", "nNA", "p1", "p2", "p.Diff", "Low", "Upp"))

          #......
          # Sort by variables
          if (isTRUE(sort.var)) {

            print.object[[i]] <- print.object[[i]][order(print.object[[i]][, "variable"]), ]

          }

          #......
          # Round
          print.object[[i]][, print.round] <- sapply(print.round, function(y) ifelse(!is.na(print.object[[i]][, y]), formatC(print.object[[i]][, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

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
                       mean.diff.i = "Difference in Means from Independent Samples\n\n",
                       mean.diff.p = "Difference in Means from Paired Samples\n\n",
                       prop.diff.i = "Difference in Proportions from Independent Samples\n\n",
                       prop.diff.p = "Difference in Proportions from Paired Samples\n\n")))

      for (i in names(print.object)) {

        cat("  Split Group:", i, "\n")

        .write.table(print.object[[i]], left = 2L, right = 4L, group = ifelse(isTRUE("group" %in% colnames(print.object[[i]])), TRUE, FALSE), result = x$result[[i]], horiz = horiz)

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
  # HC and CR Standard Errors, coeff.robust() ----------------------------------
  }, coeff.robust = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Model Class ####

    # (Generalized) Linear Model
    if (isTRUE(inherits(x$model, what = "lm"))) {

      model.class <- "lm"

    # Multilevel and Linear Mixed-Effects Model
    } else if (all(class(x$model) %in% c("lmerMod", "lmerModLmerTest"))) {

      model.class <- "lmer"

    } else if (all(class(model) %in% "lme")) {

        model.class <- "lme"

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Model Class ####

    switch(model.class,

           #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           ## Linear Regression, lm() ####

           lm = {

             #...................
             ### Heteroscedasticity-Consistent Standard Error ####

             if (isTRUE(x$args$type %in% c("HC0", "HC1", "HC2", "HC3", "HC4", "HC4m", "HC5"))) {

                #...................
                ### Extract coefficients ####

                print.coef <- print.object$coef

                #...................
                ### Round ####

                print.coef[, setdiff(colnames(print.coef), "p")] <- apply(print.coef[, setdiff(colnames(print.coef), "p")], 2L, function(y) formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")))

                print.coef[, "p"] <- formatC(as.numeric(print.coef[, "p"]), digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

                #...................
                ### Column Names ####

                print.coef <- rbind(c("", colnames(print.coef)), data.frame(row.names(print.coef), Estimate = print.coef[, 1L], do.call("cbind", lapply(print.coef[, -1L], function(y) paste0(" ", y)))))

                #...................
                ### Format ####

                # Justify left and right
                print.coef[, 1L] <- format(print.coef[, 1L, drop = FALSE], justify = "left")
                print.coef[, -1L] <- apply(print.coef[, -1L, drop = FALSE], 2L, function(y) format(y, justify = "right"))

                # Add blank space
                print.coef[, 1L] <-  paste0("   ", print.coef[, 1L])

                #...................
                ### Print ####

                cat(paste0("  Heteroscedasticity-Consistent Standard Errors (", x$args$type, ")\n\n"))

                # Print coefficients
                .write.table(print.coef, left = 2L, right = 4L, group = FALSE, horiz = horiz)

                #...................
                ### F-test ####

                if (isTRUE(length(class(x$model)) == 1L)) {

                  # Extract F-test
                  print.F <- print.object$F.test[2L, ]

                  # Round
                  print.F["F"] <- formatC(print.F["F"], digits = digits - 1L, format = "f", zero.print = ifelse(digits - 1L > 0L, paste0("0.", paste(rep(0L, times = digits - 1L), collapse = "")), "0"))
                  print.F["p"] <- formatC(as.numeric(print.F["p"]), digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

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
                                    paste0("   Robust F-statistic: ", print.F["F"], " on ",  print.object$F.test[2L, "df"], " and ", print.object$F.test[2L, "res.df"], " df, p-value: ", print.F["p"]))))

                }

             #...................
             ### Cluster-Robust Standard Error ####

             } else {

               #...................
               ### Extract coefficients ####

               print.coef <- print.object$coef

               #...................
               ### Round ####

               print.coef[, setdiff(colnames(print.coef), "p")] <- apply(print.coef[, setdiff(colnames(print.coef), "p")], 2L, function(y) formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")))

               print.coef[, "p"] <- formatC(as.numeric(print.coef[, "p"]), digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

               #...................
               ### Column Names ####

               print.coef <- rbind(c("", colnames(print.coef)), data.frame(row.names(print.coef), Estimate = print.coef[, 1L], do.call("cbind", lapply(print.coef[, -1L], function(y) paste0(" ", y)))))

               #...................
               ### Format ####

               # Justify left and right
               print.coef[, 1L] <- format(print.coef[, 1L, drop = FALSE], justify = "left")
               print.coef[, -1L] <- apply(print.coef[, -1L, drop = FALSE], 2L, function(y) format(y, justify = "right"))

               # Add blank space
               print.coef[, 1L] <-  paste0("   ", print.coef[, 1L])

               #...................
               ### Print ####

               cat(paste0("  Clust-Robust Standard Errors (", x$args$type, ")\n\n"))

               # Print coefficients
               .write.table(print.coef, left = 2L, right = 4L, group = FALSE, horiz = horiz)

             }

          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          ## Linear Mixed-Effects Model, lmer() ####

          }, lmer = {

            #...................
            ### Extract coefficients ####

            print.coef <- print.object$coef

            #...................
            ### Round ####

            print.coef[, setdiff(colnames(print.coef), "p")] <- apply(print.coef[, setdiff(colnames(print.coef), "p")], 2L, function(y) formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")))

            print.coef[, "p"] <- formatC(as.numeric(print.coef[, "p"]), digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

            #...................
            ### Column Names ####

            print.coef <- rbind(c("", colnames(print.coef)), data.frame(row.names(print.coef), Estimate = print.coef[, 1L], do.call("cbind", lapply(print.coef[, -1L], function(y) paste0(" ", y)))))

            #...................
            ### Format ####

            # Justify left and right
            print.coef[, 1L] <- format(print.coef[, 1L, drop = FALSE], justify = "left")
            print.coef[, -1L] <- apply(print.coef[, -1L, drop = FALSE], 2L, function(y) format(y, justify = "right"))

            # Add blank space
            print.coef[, 1L] <-  paste0("   ", print.coef[, 1L])

            #...................
            ### Print ####

            cat(paste0("  Cluster-Robust Standard Error (", x$args$type, ")\n\n"))

            # Print coefficients
            .write.table(print.coef, left = 2L, right = 4L, group = FALSE, horiz = horiz)

          }, lme ={

            #...................
            ### Extract coefficients ####

            print.coef <- print.object$coef

            #...................
            ### Round ####

            print.coef[, setdiff(colnames(print.coef), "p")] <- apply(print.coef[, setdiff(colnames(print.coef), "p")], 2L, function(y) formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")))

            print.coef[, "p"] <- formatC(as.numeric(print.coef[, "p"]), digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

            #...................
            ### Column Names ####

            print.coef <- rbind(c("", colnames(print.coef)), data.frame(row.names(print.coef), Estimate = print.coef[, 1L], do.call("cbind", lapply(print.coef[, -1L], function(y) paste0(" ", y)))))

            #...................
            ### Format ####

            # Justify left and right
            print.coef[, 1L] <- format(print.coef[, 1L, drop = FALSE], justify = "left")
            print.coef[, -1L] <- apply(print.coef[, -1L, drop = FALSE], 2L, function(y) format(y, justify = "right"))

            # Add blank space
            print.coef[, 1L] <-  paste0("   ", print.coef[, 1L])

            #...................
            ### Print ####

            cat(paste0("  Cluster-Robust Standard Error (", x$args$type, ")\n\n"))

            # Print coefficients
            .write.table(print.coef, left = 2L, right = 4L, group = FALSE, horiz = horiz)

          })

  #_____________________________________________________________________________
  #
  # Standardized Coefficients, cieff.std() -------------------------------------
  }, coeff.std = {

    # Check input 'print'
    if (isTRUE(check)) { if (isTRUE(!all(print %in% c("all", "stdx", "stdy", "stdyx")))) { stop("Character strings in the argument 'print' do not all match with \"all\", \"stdx\", \"stdy\", or \"stdyx\".", call. = FALSE) } }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Arguments ####

    # Print = "all"
    if (isTRUE(length(print) == 1L && "all" %in% print)) { print <- c("stdx", "stdy", "stdyx") }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Exclude Columns ####

    if (isTRUE(!"stdx" %in% print)) { print.object <- print.object[, which(colnames(print.object) != "StdX")] }

    if (isTRUE(!"stdy" %in% print)) { print.object <- print.object[, which(colnames(print.object) != "StdY")] }

    if (isTRUE(!"stdyx" %in% print)) { print.object <- print.object[, which(colnames(print.object) != "StdYX")] }

    if (isTRUE(all(print == "stdy"))) { print.object <- print.object[, which(colnames(print.object) != "SDx")] }

    if (isTRUE(all(print == "stdx"))) { print.object <- print.object[, which(colnames(print.object) != "SDy")] }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Round ####

    #...................
    ### Linear Model, lm() function ####

    if (isTRUE(inherits(x$model, what = "lm"))) {

      print.object[, -4L] <- apply(print.object[, -4L], 2L, function(y) formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")))
      print.object[, 4L] <- formatC(as.numeric(print.object[, 4L]), digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

      # Rename columns
      print.object <- misty::df.rename(print.object, from = c("Std. Error", "t value", "Pr(>|t|)"), to = c("SE", "t", "p"))

    #...................
    ### Linear Mixed-Effects Model, lmer() function ####

    } else if (isTRUE(class(x$model) %in% c("lmerMod", "lmerModLmerTest"))) {

      print.object[, !colnames(print.object) %in% c("Pr(>|t|)", "Level")] <- apply(print.object[, !colnames(print.object) %in% c("Pr(>|t|)", "Level")], 2L, function(y) formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")))

      print.object[, "Level"] <- format(print.object[, "Level"], justify = "right")

      if (isTRUE("Pr(>|t|)" %in% colnames(print.object))) { print.object[, colnames(print.object) == "Pr(>|t|)"] <- formatC(as.numeric(print.object[, colnames(print.object) == "Pr(>|t|)"]), digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0")) }

      # Rename columns
      print.object <- misty::df.rename(print.object, from = c("Std. Error", "t value"), to = c("SE", "t"))

      if (isTRUE("Pr(>|t|)" %in% colnames(print.object))) { print.object <- misty::df.rename(print.object, from = "Pr(>|t|)", to = "p") }

    #...................
    ### Linear Mixed-Effects Model, lme() function ####

    } else if (isTRUE(inherits(x$model, what = "lme"))) {

      print.object[, !colnames(print.object) %in% c("DF", "p-value", "Level")] <- apply(print.object[, !colnames(print.object) %in% c("DF", "p-value", "Level")], 2L, function(y) formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")))

      print.object[, "DF"] <- format(print.object[, "DF"], justify = "right")
      print.object[, "Level"] <- format(print.object[, "Level"], justify = "right")

      if (isTRUE("p-value" %in% colnames(print.object))) { print.object[, colnames(print.object) == "p-value"] <- formatC(as.numeric(print.object[, colnames(print.object) == "p-value"]), digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0")) }

      # Rename columns
      print.object <- misty::df.rename(print.object, from = c("Value", "Std.Error", "DF", "t-value", "p-value"), to = c("Estimate", "SE", "df", "t", "p"))

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Format ####

    #...................
    ### Replace NA with "" ####

    print.object <- as.data.frame(apply(print.object, 2L, function(y) gsub("NA", "", y)))

    #...................
    ### Column Names ####

    print.object <- rbind(c("", colnames(print.object)), data.frame(row.names(print.object), Estimate = print.object[, 1L], do.call("cbind", lapply(print.object[, -1L], function(y) paste0(" ", y)))))

    #...................
    ### Justify Left and Right ####

    print.object[, 1L] <- format(print.object[, 1L, drop = FALSE], justify = "left")
    print.object[, -1L] <- apply(print.object[, -1L, drop = FALSE], 2L, function(y) format(y, justify = "right"))

    #...................
    ### Add Blank Space ####

    print.object[, 1L] <-  paste0("  ", print.object[, 1L])

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Print ####

    cat(" Unstandardized and Standardized Coefficients\n\n")

    # Print object
    .write.table(print.object, left = 1L, right = 3L, group = FALSE, horiz = horiz)

  #_____________________________________________________________________________
  #
  # Cohen's d, cohens.d() ------------------------------------------------------
  }, cohens.d = {

    # Variables to round
    print.round <- switch(x$sample,
                          one = c("m", "m.diff", "sd", "d", "se", "low", "upp"),
                          two = c("m", "m.diff", "sd", "d", "se", "low", "upp"),
                          paired = c("m1", "m2", "m.diff", "sd", "d", "se", "low", "upp"))

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## No grouping ####

    if (isTRUE(is.null(x$data$group) && is.null(x$data$split))) {

      #...................
      ### Round ####

      print.object[, print.round] <- sapply(print.round, function(y) ifelse(!is.na(print.object[, y]), formatC(print.object[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

      #...................
      ### Column Names ####

      print.object <- rbind(switch(x$sample,
                                   one = c("Variable", "n", "nNA", "M", "M.Diff", "SD", "d", "SE", "Low", "Upp"),
                                   two = c("Variable", "Between", "n", "nNA", "M", "M.Diff", "SD", "d", "SE", "Low", "Upp"),
                                   paired = c("Variable", "n", "nNA", "M1", "M2", "M.Diff", "SD", "d", "SE", "Low", "Upp")), print.object)

      #...................
      ### Format ####

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

      #...................
      ### Print Output ####

      cat(paste(switch(x$sample,
                       one = paste0(" Cohen's d: One-Sample Design for \u03BC = ", round(x$args$mu, digits = 2L), " with"),
                       two = " Cohen's d: Two-Sample Design with",
                       paired = " Cohen's d: Paired-Sample Design with"),
                switch(x$args$alternative,
                       two.sided = "Two-Sided",
                       less = "One-Sided",
                       greater = "One-Sided"),
                paste0(round(x$args$conf.level * 100L, digits = 2L), "%"), "CI\n\n"))

      .write.table(print.object, left = 1L, right = 3L, group = FALSE, horiz = horiz)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Grouping ####

    } else if (isTRUE(!is.null(x$data$group) && is.null(x$data$split))) {

      #...................
      ### Sort by Variables ####

      if (isTRUE(sort.var)) { print.object <- print.object[order(print.object[, "variable"]), ] }

      #...................
      ### Round ####

      print.object[, print.round] <- sapply(print.round, function(y) ifelse(!is.na(print.object[, y]), formatC(print.object[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

      #...................
      ### Column Names ####

      print.object <- rbind(switch(x$sample,
                                   one = c("Group", "Variable", "n", "nNA", "M", "M.Diff", "SD", "d", "SE", "Low", "Upp"),
                                   two = c("Group", "Variable", "Between", "n", "nNA", "M", "M.Diff", "SD", "d", "SE", "Low", "Upp"),
                                   paired = c("Group", "Variable", "n", "nNA", "M1", "M2", "M.Diff", "SD", "d", "SE", "Low", "Upp")), print.object)

      #...................
      ### Format ####

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

      #...................
      ### Print Output ####

      cat(paste(switch(x$sample,
                       one = paste0(" Cohen's d: One-Sample Design for \u03BC = ", round(x$args$mu, digits = 2L), " with"),
                       two = " Cohen's d: Two-Sample Design with",
                       paired = " Cohen's d: Paired-Sample Design with"),
                switch(x$args$alternative,
                       two.sided = "Two-Sided",
                       less = "One-Sided",
                       greater = "One-Sided"),
                paste0(round(x$args$conf.level * 100L, digits = 2L), "%"), "CI\n\n"))

      .write.table(print.object, left = 1L, right = 3L, group = TRUE, result = x$result, horiz = horiz)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Split ####

    } else if (isTRUE(!is.null(x$data$split))) {

      #...................
      ### No Grouping ####

      if (isTRUE(is.null(x$data$group))) {

        for (i in names(print.object)) {

          # Round
          print.object[[i]][, print.round] <- sapply(print.round, function(y) ifelse(!is.na(print.object[[i]][, y]), formatC(print.object[[i]][, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

          # Col names
          print.object[[i]] <- rbind(switch(x$sample,
                                            one = c("Variable", "n", "nNA", "M", "M.Diff", "SD", "d", "SE", "Low", "Upp"),
                                            two = c("Variable", "Between", "n", "nNA", "M", "M.Diff", "SD", "d", "SE", "Low", "Upp"),
                                            paired = c("Variable", "n", "nNA", "M1", "M2", "M.Diff", "SD", "d", "SE", "Low", "Upp")), print.object[[i]])

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

      #...................
      ### Grouping ####

      } else {

        for (i in names(print.object)) {

          # Sort by variables
          if (isTRUE(sort.var)) { print.object[[i]] <- print.object[[i]][order(print.object[[i]][, "variable"]), ] }

          #......
          # Round
          print.object[[i]][, print.round] <- sapply(print.round, function(y) ifelse(!is.na(print.object[[i]][, y]), formatC(print.object[[i]][, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

          #......
          # Col names
          print.object[[i]] <- rbind(switch(x$sample,
                                            one = c("Group", "Variable", "n", "nNA", "M", "M.Diff", "SD", "d", "SE", "Low", "Upp"),
                                            two = c("Group", "Variable", "Between", "n", "nNA", "M", "M.Diff", "SD", "d", "SE", "Low", "Upp"),
                                            paired = c("Group", "Variable", "n", "nNA", "M1", "M2", "M.Diff", "SD", "d", "SE", "Low", "Upp")), print.object[[i]])

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

      # Print output
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

        .write.table(print.object[[i]], left = 2L, right = 4L, group = ifelse(isTRUE("group" %in% colnames(print.object[[i]])), TRUE, FALSE), result = x$result[[i]], horiz = horiz)

        if (isTRUE(i != names(print.object)[length(print.object)])) { cat("\n") }

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Notes ####

    #...................
    ### One-Sample Design ####

    if (isTRUE(x$sample == "one")) {

      if (isTRUE(x$args$correct)) { cat("\n  Note. Applying small sample correction factor\n") }

    #...................
    ### Two-Sample Design ####

    } else if (isTRUE(x$sample == "two")) {

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

    #...................
    ### Paired-Sample Design ####

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
  # Correlation Matrix with Statistical Significance Testing, cor.matrix() -----
  }, cor.matrix = {

    # Check input 'print'
    if (isTRUE(check)) { if (isTRUE(any(!print %in% c("all", "cor", "stat", "df", "n", "p")))) { stop("Character string(s) in the argument 'print' does not match with \"all\", \"cor\",  \"stat\",  \"df\", \"n\", or \"p\".", call. = FALSE) } }

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
                                             p = c(print.object$p[2L, 1L], print.object$p[1L, 2L]))

                }, 'spearman' = {

                  print.object <- data.frame(Group = c(sort(unique(x$data$.group))[1L], sort(unique(x$data$.group))[2L]),
                                             x = colnames(print.object$cor)[1L],
                                             y = colnames(print.object$cor)[2L],
                                             n = c(print.object$n[2L, 1L], print.object$n[1L, 2L]),
                                             r = c(print.object$cor[2L, 1L], print.object$cor[1L, 2L]),
                                             S = c(print.object$stat[2L, 1L], print.object$stat[1L, 2L]),
                                             p = c(print.object$p[2L, 1L], print.object$p[1L, 2L]))

                }, 'kendall-b' = {

                  print.object <- data.frame(Group = c(sort(unique(x$data$.group))[1L], sort(unique(x$data$.group))[2L]),
                                             x = colnames(print.object$cor)[1L],
                                             y = colnames(print.object$cor)[2L],
                                             n = c(print.object$n[2L, 1L], print.object$n[1L, 2L]),
                                             tau = c(print.object$cor[2L, 1L], print.object$cor[1L, 2L]),
                                             z = c(print.object$stat[2L, 1L], print.object$stat[1L, 2L]),
                                             p = c(print.object$p[2L, 1L], print.object$p[1L, 2L]))


                }, 'kendall-c' = {

                  print.object <- data.frame(Group = c(sort(unique(x$data$.group))[1L], sort(unique(x$data$.group))[2L]),
                                             x = colnames(print.object$cor)[1L],
                                             y = colnames(print.object$cor)[2L],
                                             n = c(print.object$n[2L, 1L], print.object$n[1L, 2L]),
                                             tau = c(print.object$cor[2L, 1L], print.object$cor[1L, 2L]),
                                             z = c(print.object$stat[2L, 1L], print.object$stat[1L, 2L]),
                                             p = c(print.object$p[2L, 1L], print.object$p[1L, 2L]))

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
        print.object[, !colnames(print.object) %in% c("Group", "n", "df", "p")] <- vapply(print.object[, !colnames(print.object) %in% c("Group", "n", "df", "p")], formatC, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"), FUN.VALUE = character(2L))

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
                                             p = print.object$p[2L, 1L])

                }, 'spearman' = {

                  print.object <- data.frame(x = colnames(print.object$cor)[1L],
                                             y = colnames(print.object$cor)[2L],
                                             n = print.object$n[2L, 1L],
                                             r = print.object$cor[2L, 1L],
                                             S = print.object$stat[2L, 1L],
                                             p = print.object$p[2L, 1L])

                }, 'kendall-b' = {

                  print.object <- data.frame(x = colnames(print.object$cor)[1L],
                                             y = colnames(print.object$cor)[2L],
                                             n = print.object$n[2L, 1L],
                                             tau = print.object$cor[2L, 1L],
                                             z = print.object$stat[2L, 1L],
                                             p = print.object$p[2L, 1L])


                }, 'kendall-c' = {

                  print.object <- data.frame(x = colnames(print.object$cor)[1L],
                                             y = colnames(print.object$cor)[2L],
                                             n = print.object$n[2L, 1L],
                                             tau = print.object$cor[2L, 1L],
                                             z = print.object$stat[2L, 1L],
                                             p = print.object$p[2L, 1L])

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
        print.object[, !colnames(print.object) %in% c("n", "df", "p")] <- vapply(print.object[, !colnames(print.object) %in% c("n", "df", "p")], formatC, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"), FUN.VALUE = character(1L))

      }

      # Round p-values
      if (isTRUE(!x$args$method %in% c("tetra", "poly"))) { print.object[, "p"] <- formatC(print.object[, "p"], digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0")) }

      #...................
      ### Format ####

      # Column names
      print.object <- rbind(c(colnames(print.object)), print.object)

      # Justify left and right
      print.object <- as.data.frame(apply(print.object, 2L, function(y) format(y, justify = "right")))

      # Add blank space
      print.object[, 1] <- paste0("  ", print.object[, 1L])

      #...................
      ### Print Output ####

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
      .write.table(print.object, left = 1L, right = 3L, group = FALSE, horiz = horiz)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## More than two variables ####

    } else {

      #...................
      ### Correlation coefficient ####

      if (isTRUE("cor" %in% print)) {

        #### Round ####

        print.object$cor <- formatC(print.object$cor, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

        #### Lower and/or Upper Triangular ####

        switch(tri, "lower" =  {

          print.object$cor[upper.tri(print.object$cor)] <- ""

        }, "upper" = {

          print.object$cor[lower.tri(print.object$cor)] <- ""

        })

        # Diagonal
        diag(print.object$cor) <- ""

        #### Statistically Significant Correlation Coefficients in Boldface ####

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

        #### Format ####

        # Row names
        if (isTRUE(!is.null(row.names(print.object$cor)))) { row.names(print.object$cor) <- paste0("  ", row.names(print.object$cor)) }

        # Column names
        print.object$cor <- cbind(c("", row.names(print.object$cor)), rbind(c(colnames(print.object$cor)), print.object$cor))

        # Justify left and right
        print.object$cor[, 1L] <- format(print.object$cor[, 1L, drop = FALSE], justify = "left")
        print.object$cor[, -1L] <- apply(print.object$cor[, -1L, drop = FALSE], 2L, function(y) format(y, justify = "right"))

        #### Print Output ####

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

        .write.table(print.object$cor, left = 1L, right = 3L, group = FALSE, horiz = horiz)

      }

      #...................
      ### Sample size ####

      if (isTRUE("n" %in% print & attr(x$data, "missing") && isTRUE(!x$args$na.omit))) {

        #### Round ####

        print.object$n <- formatC(print.object$n, zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

        #### Lower and/or Upper Triangular ####

        switch(tri, "lower" =  {

          print.object$n[upper.tri(print.object$n)] <- ""

        }, "upper" = {

          print.object$n[lower.tri(print.object$n)] <- ""

        })

        # Diagonal
        diag(print.object$n) <- ""

        #### Format ####

        # Row names

        if (isTRUE(!is.null(row.names(print.object$cor)))) { row.names(print.object$n) <- paste0("  ", row.names(print.object$n)) }

        # Column names
        print.object$n <- cbind(c("", row.names(print.object$n)), rbind(c(colnames(print.object$n)), print.object$n))

        # Justify left and right
        print.object$n[, 1L] <- format(print.object$n[, 1L, drop = FALSE], justify = "left")
        print.object$n[, -1L] <- apply(print.object$n[, -1L, drop = FALSE], 2L, function(y) format(y, justify = "right"))

        #### Print Output ####

        if (isTRUE("cor" %in% print)) { cat("\n") }

        cat(" Sample Size\n\n")

        .write.table(print.object$n, left = 1L, right = 3L, group = FALSE, horiz = horiz)

      }

      #...................
      ### Test statistic ####

      if (isTRUE("stat" %in% print && !x$args$method %in% c("tetra", "poly"))) {

        #### Round ####

        print.object$stat <- formatC(print.object$stat, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

        #### Lower and/or Upper Triangular ####

        switch(tri, "lower" =  {

          print.object$stat[upper.tri(print.object$stat)] <- ""

        }, "upper" = {

          print.object$stat[lower.tri(print.object$stat)] <- ""

        })

        # Diagonal
        diag(print.object$stat) <- ""

        #### Format ####

        # Row names
        if (isTRUE(!is.null(row.names(print.object$cor)))) { row.names(print.object$stat) <- paste0("  ", row.names(print.object$stat)) }

        # Column names
        print.object$stat <- cbind(c("", row.names(print.object$stat)), rbind(c(colnames(print.object$stat)), print.object$stat))

        # Justify left and right
        print.object$stat[, 1L] <- format(print.object$stat[, 1L, drop = FALSE], justify = "left")
        print.object$stat[, -1L] <- apply(print.object$stat[, -1L, drop = FALSE], 2L, function(y) format(y, justify = "right"))

        #### Print Output ####

        if (isTRUE(any(c("cor", "n") %in% print))) { cat("\n") }

        if (isTRUE(x$args$method == "pearson")) {

          cat(" Test Statistic (t value)\n\n")

        } else if (isTRUE(x$args$method == "spearman")) {

            cat(" Test Statistic (S value)\n\n")

        } else {

          cat(" Test Statistic (z value)\n\n")

        }

        .write.table(print.object$stat, left = 1L, right = 3L, group = FALSE, horiz = horiz)

      }

      #...................
      ### Degrees of freedom ####

      if (isTRUE("df" %in% print && x$args$method == "pearson")) {

        #### Round ####

        print.object$df <- formatC(print.object$df, digits = 0L, format = "f")

        #### Lower and/or Upper Triangular ####

        switch(tri, "lower" =  {

          print.object$df[upper.tri(print.object$df)] <- ""

        }, "upper" = {

          print.object$df[lower.tri(print.object$df)] <- ""

        })

        # Diagonal
        diag(print.object$df) <- ""

        #### Format ####

        # Row names
        if (isTRUE(!is.null(row.names(print.object$cor)))) { row.names(print.object$df) <- paste0("  ", row.names(print.object$df)) }

        # Column names
        print.object$df <- cbind(c("", row.names(print.object$df)), rbind(c(colnames(print.object$df)), print.object$df))

        # Justify left and right
        print.object$df[, 1L] <- format(print.object$df[, 1L, drop = FALSE], justify = "left")
        print.object$df[, -1L] <- apply(print.object$df[, -1L, drop = FALSE], 2L, function(y) format(y, justify = "right"))

        #### Print Output ####

        if (isTRUE(any(c("cor", "n", "stat") %in% print))) { cat("\n") }

        cat(" Degrees of Freedom (df) \n\n")

        .write.table(print.object$df, left = 1L, right = 3L, group = FALSE, horiz = horiz)

      }

      #...................
      ### p-values ####

      if (isTRUE("p" %in% print && !x$args$method %in% c("tetra", "poly"))) {

        #### Round ####

        print.object$p <- formatC(print.object$p, digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

        #### Lower and/or Upper Triangular ####

        switch(tri, "lower" =  {

          print.object$p[upper.tri(print.object$p)] <- ""

        }, "upper" = {

          print.object$p[lower.tri(print.object$p)] <- ""

        })

        # Diagonal
        diag(print.object$p) <- ""

        #### Format ####

        # Row names
        if (isTRUE(!is.null(row.names(print.object$cor)))) { row.names(print.object$p) <- paste0("  ", row.names(print.object$p)) }

        # Column names
        print.object$p <- cbind(c("", row.names(print.object$p)), rbind(c(colnames(print.object$p)), print.object$p))

        # Justify left and right
        print.object$p[, 1L] <- format(print.object$p[, 1L, drop = FALSE], justify = "left")
        print.object$p[, -1L] <- apply(print.object$p[, -1L, drop = FALSE], 2L, function(y) format(y, justify = "right"))

        #### Print Output ####

        if (x$args$method == "kendall-c") {

          if (isTRUE(any(c("cor", "n", "stat") %in% print))) { cat("\n") }

        } else {

          if (isTRUE(any(c("cor", "n", "stat", "df") %in% print))) { cat("\n") }

        }

        cat(" Significance Value (p-value)\n\n")

        .write.table(print.object$p, left = 1L, right = 3L, group = FALSE, horiz = horiz)

        cat(paste0("\n  Adjustment for multiple testing: ", x$args$p.adj, "\n"))

      }

      #...................
      ### Notes ####

      # Sample size
      cat(paste0("\n Note. n = ", ifelse(isTRUE(x$args$na.omit || !attr(x$data, "missing")), na.omit(unique(c(x$result$n))),
                                         ifelse(length(unique(range(c(x$result$n), na.rm = TRUE))) == 1, unique(range(c(x$result$n), na.rm = TRUE)),
                                                paste(range(c(x$result$n), na.rm = TRUE), collapse = "-"))),
                 ifelse(isTRUE(attr(x$data, "missing")), ifelse(isTRUE(x$args$na.omit), ", Listwise deletion\n", ", Pairwise deletion\n"), ", No missing data\n")))

      # Lower and upper triangular
      if (isTRUE(".group" %in% colnames(x$data))) { cat(paste0("       Lower triangular: ", sort(unique(x$data$.group))[1L], ", Upper triangular: ", sort(unique(x$data$.group))[2L]), "\n") }


      # Statistical significance
      if (isTRUE(x$args$sig && !x$args$method %in% c("tetra", "poly"))) { cat(paste0("       Statistically significant coefficients at \U03B1 = ", signif(x$args$alpha, digits = 2L), " are boldface\n")) }

    }

  #_____________________________________________________________________________
  #
  # Cross Tabulation, crosstab() -----------------------------------------------
  }, crosstab = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Input Check ####

    # Check input 'print'
    if (isTRUE(check)) { if (isTRUE(any(!print %in% c("no", "all", "row", "col", "total")))) { stop("Character string(s) in the argument 'print' does not match with \"no\", \"all\", \"row\", \"col\" or \"total\".", call. = FALSE) } }

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

        .write.table(restab, left = 1L, right = 3L, line = 2L, horiz = horiz)

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

          .write.table(restab.abs, left = 1L, right = 3L, line = 2L, horiz = horiz)

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

          .write.table(restab.row, left = 1L, right = 3L, line = 2L, horiz = horiz)

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

          .write.table(restab.col, left = 1L, right = 3L, line = 2L, horiz = horiz)

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

          .write.table(restab.total, left = 1L, right = 3L, line = 2L, horiz = horiz)

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
        .write.table(restab, left = 1L, right = 3L, line = 2L, horiz = horiz)

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

          .write.table(restab.abs, left = 1L, right = 3L, line = 2L, horiz = horiz)

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

          .write.table(restab.row, left = 1L, right = 3L, line = 2L, horiz = horiz)

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

          .write.table(restab.col, left = 1L, right = 3L, line = 2L, horiz = horiz)

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

          .write.table(restab.total, left = 1L, right = 3L, line = 2L, horiz = horiz)

        }

      }

    }

  #_____________________________________________________________________________
  #
  # Descriptive Statistics, descript() -----------------------------------------
  }, descript = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Input Check ####

    if (isTRUE(check)) {

      # Check input 'print'
      if (isTRUE(!all(print %in% c("all", "n", "nNA", "pNA", "nUQ", "m", "se.m", "var", "sd", "min", "p.min", "p25", "med", "p75", "max", "p.max", "skew",  "range", "iqr", "kurt")))) {

        stop("Character strings in the argument 'print' do not all match with \"all\", \"n\", \"nNA\", \"pNA\", \"nUQ\", \"m\", \"se.m\", \"var\", \"sd\", \"min\", \"p.min\", \"p25\", \"med\", \"p75\", \"max\", \"p.max\", \"range\", \"iqr\", \"skew\", or \"kurt\".", call. = FALSE)

      }

    }

    if (isTRUE(length(print) == 1L && print == "all")) { print <- c("n", "nNA", "pNA", "nUQ", "m", "se.m", "var", "sd", "min", "p.min", "p25", "med", "p75", "max", "p.max", "range", "iqr", "skew", "kurt") }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Variables to Round ####

    if (isTRUE(all(if (isTRUE(is.data.frame(print.object))) { print.object$nNA == 0L } else { do.call("rbind", print.object)$nNA == 0L }))) {

      print.round <- c("m", "se.m", "var", "sd", "min", "p.min", "p25", "med", "p75", "max", "p.max", "range", "iqr", "skew", "kurt")

    } else {

      print.round <- c("pNA", "m", "se.m", "var", "sd", "min", "p.min", "p25", "med", "p75", "max", "p.max", "range", "iqr", "skew", "kurt")

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## No Grouping, No Split ####

    if (isTRUE(is.null(x$data$group) && is.null(x$data$split))) {

      #...................
      ### Round ####

      print.object[, print.round] <- sapply(print.round, function(y) ifelse(!is.na(print.object[, y]), formatC(print.object[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

      #...................
      ### Percentages ####

      print.object[, "pNA"] <- paste0(print.object[, "pNA"], "%")
      print.object[, "p.min"] <- paste0(print.object[, "p.min"], "%")
      print.object[, "p.max"] <- paste0(print.object[, "p.max"], "%")

      #...................
      ### Column Names ####

      print.object <- rbind(c("Variable", "n", "nNA", "%NA", "nUQ", "M", "SE.M", "Var", "SD", "Min", "%Min", "p25", "Med", "p75", "Max", "%Max", "Range", "IQR", "Skew", "Kurt"), print.object)

      #...................
      ### Select Statistical Measures and Add Variable Names ####

      print.object <- data.frame(variable = print.object[, "variable"], print.object[, print, drop = FALSE], stringsAsFactors = FALSE, check.names = FALSE)

      #...................
      ### Format ####

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

      .write.table(print.object, left = 1L, right = 3L, horiz = horiz)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Grouping, No Split ####

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
      print.object[, print.round] <- sapply(print.round, function(y) ifelse(!is.na(print.object[, y]), formatC(print.object[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

      # Percentages
      print.object[, "pNA"] <- paste0(print.object[, "pNA"], "%")
      print.object[, "p.min"] <- paste0(print.object[, "p.min"], "%")
      print.object[, "p.max"] <- paste0(print.object[, "p.max"], "%")

      # Col names
      print.object <- rbind(c("Group", "Variable", "n", "nNA", "%NA", "nUQ", "M", "SE M", "Var", "SD", "Min", "%Min", "p25", "Med", "p75", "Max", "%Max", "Range", "IQR", "Skew", "Kurt"), print.object)

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

      if (isTRUE(ncol(x$data$x) == 1L)) { print.object <- print.object[, -2L] }

      cat(" Descriptive Statistics\n\n")

      # Print Output
      .write.table(print.object, left = 1L, right = 3L, group = ifelse(isTRUE(ncol(x$data$x) == 1L), FALSE, TRUE), result = x$result, horiz = horiz)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Split, without or with Grouping ####

    } else if (isTRUE(!is.null(x$data$split))) {

      # Format
      for (i in names(print.object)) {

        # Round
        print.object[[i]][, print.round] <- sapply(print.round, function(y) ifelse(!is.na(print.object[[i]][, y]), formatC(print.object[[i]][, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

        # Percentages
        print.object[[i]][, "pNA"] <- paste0(print.object[[i]][, "pNA"], "%")
        print.object[[i]][, "p.min"] <- paste0(print.object[[i]][, "p.min"], "%")
        print.object[[i]][, "p.max"] <- paste0(print.object[[i]][, "p.max"], "%")

        #...................
        ### No grouping ####

        if (isTRUE(is.null(x$data$group))) {

          # Col names
          print.object[[i]] <- rbind(c("Variable", "n", "nNA", "%NA", "nUQ", "M", "SE M", "Var", "SD", "Min", "%Min", "p25", "Med", "p75", "Max", "%Max", "Range", "IQR", "Skew", "Kurt"), print.object[[i]])

          # Select statistical measures and add variable names
          print.object[[i]] <- data.frame(variable = print.object[[i]][, "variable"], print.object[[i]][, print, drop = FALSE], stringsAsFactors = FALSE)

          print.object[[i]][, "variable"] <- format(print.object[[i]][, "variable"], justify = "left")
          print.object[[i]][, -1L] <- format(print.object[[i]][, -1L], justify = "right")

          # Add blank space
          print.object[[i]][, "variable"] <- c(paste0("   ", print.object[[i]][1L, "variable"], collapse = ""), paste0("    ", print.object[[i]][-1L, "variable"]))
          print.object[[i]][, "variable"] <- format(c(print.object[[i]][1L, "variable"], misty::chr.trim(print.object[[i]][-1L, "variable"], side = "right")), justify = "left")

          if (isTRUE(ncol(x$data$x) == 1L)) { print.object[[i]][, "n"] <- paste("", print.object[[i]][, "n"]) }

        #...................
        ### Grouping ####

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
          print.object[[i]] <- rbind(c("Group", "Variable", "n", "nNA", "%NA", "nUQ", "M", "SE.M", "Var", "SD", "Min", "%Min", "p25", "Med", "p75", "Max", "%Max", "Range", "IQR", "Skew", "Kurt"), print.object[[i]])

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

        .write.table(print.object[[i]], left = 2L, right = 4L, group = ifelse(isTRUE("group" %in% colnames(print.object[[i]]) && ncol(x$data$x) > 1L), TRUE, FALSE), result = x$result[[i]], horiz = horiz)

        if (isTRUE(i != names(print.object)[length(print.object)])) { cat("\n") }

      }

    }

  #_____________________________________________________________________________
  #
  # Print the First Rows of a Data Frame, df.head() ----------------------------
  }, df.head = {

    # Print data frame
    write.table(print.object$df, quote = FALSE, col.names = FALSE, row.names = FALSE)

    # Print message
    if (isTRUE(!is.null(print.object$row.col))) { cat(print.object$row.col) }

  #_____________________________________________________________________________
  #
  # Print the Last Rows of a Data Frame, df.tail() -----------------------------
  }, df.tail = {

    # Print data frame
    write.table(print.object$df, quote = FALSE, col.names = FALSE, row.names = FALSE)

    # Print message
    if (isTRUE(!is.null(print.object$row.col))) { cat(print.object$row.col) }

  #_____________________________________________________________________________
  #
  # Manual Dominance Analysis, dominance.manual() ------------------------------
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

    .write.table(print.gen, left = 2L, right = 4L, horiz = horiz)

    cat(paste0("\n  Note. Outcome variable: ", x$args$out, "\n"))

  #_____________________________________________________________________________
  #
  # Chi-Bar-Square Difference Test, difftest.chibarsq() ------------------------
  }, difftest.chibarsq = {

    # Print object
    print.object <- print.object$difftest

    # Remove chisq.crit column
    print.object <- print.object[, setdiff(colnames(print.object), "chisq.crit")]

    # Header
    cat(" Chi-Bar-Square Difference Test\n\n")

    # Round
    print.object[, setdiff(colnames(print.object), c("df", "df.diff", "p"))] <- lapply(print.object[, setdiff(colnames(print.object), c("df", "df.diff", "p"))], function(y) formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")))
    print.object[, "p"] <- formatC(print.object[, "p"], digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

    # Column and row names
    print.object <- data.frame(c("", row.names(print.object)), rbind(c("df", "AIC", "BIC", "SABIC", "Chisq", "dChisq", "ddf", "p"), print.object), fix.empty.names = FALSE)

    # Greek letter Delta
    if (is.null(getOption("knitr.in.progress"))) { print.object[1L, ] <- c("", "df", "AIC", "BIC", "SABIC", "Chisq", paste0("\U2206", "Chisq"), paste0("\U2206", "df"),"p") }

    # Justify right
    print.object[, -1L] <- lapply(print.object[, -1L], function(y) format(y, justify = "right"))
    print.object[, 1L] <- format(print.object[, 1L], justify = "left")

    # Add blank space
    print.object[, 1L] <- paste0("  ", print.object[, 1L])

    # Replace NA
    print.object[, -1L] <- apply(print.object[, -1L], 2L, function(y) gsub("NA", "  ", y))

    # Print
    .write.table(print.object, left = 1L, right = 3L, horiz = horiz)

    # Note
    weights <- formatC(x$result$weights, digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0")) |> (\(p) paste0(paste(p[-length(p)], collapse = ", "), " and ", p[length(p)]))()

    cat("\n  Note. Weights used in the test are", weights)

  #_____________________________________________________________________________
  #
  # Dominance Analysis, dominance() --------------------------------------------
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

      .write.table(print.gen, left = 2L, right = 4L, horiz = horiz)

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
      .write.table(print.cond, left = 2L, right = 4L, horiz = horiz)

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
      .write.table(print.comp, left = 2L, right = 4L, horiz = horiz)

    }

  #_____________________________________________________________________________
  #
  # Effect Sizes for Categorical Variables, effsize() --------------------------
  }, effsize = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Round ####

    print.object[, colnames(print.object)[!colnames(print.object) %in% c("n", "var")]] <- vapply(print.object[, colnames(print.object)[!colnames(print.object) %in% c("n", "var")]], formatC, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"), FUN.VALUE = character(nrow(print.object[, colnames(print.object)[!colnames(print.object) %in% c("n", "var")]])))

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Format ####

    if (isTRUE("var" %in% colnames(print.object))) {

      print.object <- rbind(c("Variable", "n", switch(x$args$type, phi = "Phi", cramer = "V", tschuprow = "T", cont = "C", w = "w", fei = "Fei"), "Low", "Upp"), print.object)

      print.object[, 1L] <- format(print.object[, 1L], justify = "left")

    } else {

      print.object <- rbind(c("n", switch(x$args$type, phi = "Phi", cramer = "V", tschuprow = "T", cont = "C", w = "w", fei = "Fei"), "Low", "Upp"), print.object)

    }

    print.object <- as.data.frame(apply(print.object, 2L, format, justify = "right"))
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

    .write.table(print.object, left = 1L, right = 3L, horiz = horiz)

    # Note
    if (isTRUE(x$args$indep && ncol(x$data) > 2L)) { cat(paste0("\n Note. The focal variable is ", colnames(x$data)[1L])) }

  #_____________________________________________________________________________
  #
  # Frequency Table, freq() ----------------------------------------------------
  }, freq = {

    # Check input 'print'
    if (isTRUE(check)) { if (isTRUE(any(!print %in% c("no", "all", "perc", "v.perc")))) { stop("Character string in the argument 'print' does not match with \"no\", \"all\", \"perc\", or \"v.perc\".", call. = FALSE) } }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## One Variable ####

    if (isTRUE(ncol(as.data.frame(x$data)) == 1L || (x$args$split && ncol(x$data) == 1L))) {

      #...................
      ### Values in Rows ####

      if (isTRUE(!x$args$val.col)) {

        print.object <- data.frame(x = c("Value", rep("", nrow(print.object) - 1L), "Missing", "Total"),
                                   val = c(print.object[1L:(which(is.na(print.object$Value)) - 1L), 1L], "Total", "NA", ""),
                                   rbind(print.object[1L:(which(is.na(print.object$Value)) - 1L), -1L],
                                         apply(print.object[1L:(which(is.na(print.object$Value)) - 1L), -1L], 2L, function(y) sum(as.numeric(y), na.rm = TRUE)),
                                         print.object[which(is.na(print.object$Value)), -1L],
                                         c(sum(as.numeric(print.object$Freq), na.rm = TRUE), "100", "")),
                                   stringsAsFactors = FALSE, row.names = NULL, check.names = FALSE)

        # Round digits
        print.object[, c("Perc", "V.Perc")] <- suppressWarnings(apply(print.object[, c("Perc", "V.Perc")], 2L, function(y) paste0(formatC(as.numeric(y), digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), "%")))

        # Remove NA
        print.object[, "V.Perc"] <- gsub("NA%", "  ", print.object[, "V.Perc"])

        # Format
        print.object[, 1L:2L] <- apply(print.object[, 1L:2L], 2L, function(y) format(y, justify = "left"))

        print.object[, -c(1L:2L)] <- apply(print.object[, -(1L:2L)], 2L, function(y) format(y, justify = "right"))

        # Add blank
        print.object[, 1L] <- paste("", print.object[, 1L])

        #### Omit Total Row if there are No Missing Values ####

        if (isTRUE(all(!is.na(x$data)))) {

          # Object without Total column
          print.object <- print.object[-grep("Total", print.object$x),  ]

          # Object without valid percentage
          print.object <- print.object[, -grep("V.Perc", colnames(print.object))]

        }

        #### Omit Missing and Total Row if print = "v.valid" and freq = FALSE ####

        if (isTRUE(length(print) == 1L && print == "v.perc" && isTRUE(!freq))) {

          # Object without Total row
          print.object <- print.object[-grep("Total", print.object$x),  ]

          # Object without Missing row
          print.object <- print.object[-grep("Missing", print.object$x),  ]

        }

        #### Omit Absolute Frequencies if freq = FALSE ####

        if (!isTRUE(freq)) { print.object <- print.object[, -grep("Freq", colnames(print.object))] }

        #### Omit Percentages ####

        # Omit percentages if !"perc" %in% print
        if (isTRUE(!"perc" %in% print || "no" %in% print)) {

          # Object without percentage
          print.object <- print.object[, -which(colnames(print.object) == "Perc")]

        }

        # Omit valid percentages if !"v.perc" %in% print
        if (isTRUE("V.Perc" %in% colnames(print.object))) {

          if (isTRUE(!"v.perc" %in% print || "no" %in% print)) {

            # Object without valid percentage
            print.object <- print.object[, -which(colnames(print.object) == "V.Perc")]

          }

        }

        # Column names
        colnames(print.object)[1L:2L] <- c("", "")

      #...................
      ### Values in Columns ####

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

        #### Omit Percentages ####


        #### Omit Total Row if there are No Missing Values ####

        if (isTRUE(all(!is.na(x$data)))) {

          # Object without Total column
          print.object <- print.object[, -ncol(print.object)]

          # Object without valid percentage
          print.object <- print.object[-grep("V.Perc", print.object[, 1L]), ]

        }

        #### Omit Missing and Total Row if print = "v.valid" and freq = FALSE ####

        if (isTRUE(length(print) == 1L && print == "v.perc" && !isTRUE(freq))) {

          # Object without Total column
          print.object <- print.object[, -ncol(print.object)]

          # Object without Missing column
          print.object <- print.object[, -ncol(print.object)]

        }

        #### Omit Absolute Frequencies if freq = FALSE ####

        if (!isTRUE(freq)) { print.object <- print.object[-grep("Freq", print.object[, 1L]), ] }

        #### Omit Percentages ####

        # Omit percentages if !"perc" %in% print
        if (isTRUE(!"perc" %in% print || "no" %in% print)) {

          # Object without percentage
          print.object <- print.object[-which(row.names(print.object) == "Perc"), ]

        }

        # Omit valid percentages if !"v.perc" %in% print
        if (isTRUE("V.Perc" %in% row.names(print.object))) {

          if (isTRUE(!"v.perc" %in% print || "no" %in% print)) {

            # Object without valid percentage
            print.object <- print.object[-which(row.names(print.object) == "V.Perc"), ]

          }

        }

      }

      #...................
      ### Format ####

      print.object <- rbind(colnames(print.object), print.object)

      # Justify left and right
      print.object <- format(print.object, justify = "right")

      # Add blank space
      print.object[, 1L] <- paste0(" ", print.object[, 1L])

      #...................
      ### Output ####

      if (!isTRUE(split)) {

        cat(" Frequency Table\n")

        if (isTRUE(x$args$val.col)) { cat("\n") }

      }

      .write.table(print.object, left = 1L, right = 3L, horiz = horiz)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## More than One Variable ####

    } else if (isTRUE(ncol(as.data.frame(x$data)) > 1L)) {

      #...................
      ### split = FALSE ####

      if (!isTRUE(x$args$split)) {

        #### Values in Rows ####

        if (!isTRUE(x$args$val.col)) {

          ##### Absolute frequencies
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

          ##### Percentages
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

          ##### Valid Percentages
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

        #### Values in Columns ####
        } else {

          ##### Absolute Frequencies
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

          ##### Percentages
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

          ##### Valid Percentages
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


        #### Output ####

        ##### Absolute Frequencies
        if (isTRUE(freq)) {

          # Format
          print.object$freq <- rbind(colnames(print.object$freq), print.object$freq)

          # Justify left and right
          print.object$freq <- format(print.object$freq, justify = "right")

          # Add blank space
          print.object$freq[, 1L] <- paste0(" ", print.object$freq[, 1L])

          # Print
          cat(" Frequencies\n")

          if (isTRUE(x$args$val.col)) { cat("\n") }

          .write.table(print.object$freq, left = 1L, right = 3L, horiz = horiz)

        }

        ##### Percentage Frequencies
        if (isTRUE(all(print != "no"))) {

          if (isTRUE(freq)) { cat("\n") }

          # Percentages
          if (isTRUE("perc" %in% print)) {

            # Format
            print.object$perc <- rbind(colnames(print.object$perc), print.object$perc)

            # Justify left and right
            print.object$perc <- format(print.object$perc, justify = "right")

            # Add blank space
            print.object$perc[, 1L] <- paste0(" ", print.object$perc[, 1L])

            # Print
            cat(" Percentages\n")

            if (isTRUE(x$args$val.col)) { cat("\n") }

            .write.table(print.object$perc, left = 1L, right = 3L, horiz = horiz)

          }

          if (isTRUE(any(is.na(x$data)))) {

            # Valid percentages
            if (isTRUE("v.perc" %in% print)) {

              if (isTRUE("perc" %in% print)) { cat("\n") }

              # Format
              print.object$v.perc <- rbind(colnames(print.object$v.perc), print.object$v.perc)

              # Justify left and right
              print.object$v.perc <- format(print.object$v.perc, justify = "right")

              # Add blank space
              print.object$v.perc[, 1L] <- paste0(" ", print.object$v.perc[, 1L])

              # Print
              cat(" Valid Percentages\n")

              if (isTRUE(x$args$val.col)) { cat("\n") }

              .write.table(print.object$v.perc, left = 1L, right = 3L, horiz = horiz)

            }

          }

        }

      #...................
      ### split = TRUE ####

      } else {

        cat(" Frequencies\n\n")

        for (i in names(x$result)) {

          cat(" ", i, "\n")

          temp <- list(call = x$call, type = "freq", data = x$data[, i], args = x$args, result = x$result[[i]])
          class(temp) <- "misty.object"

          print(temp, check = FALSE)

          if (isTRUE(i != names(x$result)[length(names(x$result))])) { cat("\n") }

        }

      }

    }

  #_____________________________________________________________________________
  #
  # Confidence Intervals for the Indirect Effect, indirect() -------------------
  }, indirect = {

    # Check input 'print'
    if (isTRUE(check)) { if (isTRUE(any(!print %in% c("all", "asymp", "dop", "mc")))) { stop("Character string(s) in the argument 'print' does not match with \"all\", \"asymp\", \"dop\", or \"mc\".", call. = FALSE) } }

    if (isTRUE(all(print == "all"))) { print <- c("asymp", "dop", "mc") }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Print Header ####

    cat(paste(switch(x$args$alternative, two.sided = " Two-Sided", less = " One-Sided", greater = " One-Sided"),
              paste0(round(x$args$conf.level * 100L, digits = 2L), "%"), "Confidence Interval: Indirect Effect\n\n"))

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Asymptotic Normal Method ####

    if (isTRUE("asymp" %in% print)) {

      # Round
      print.object$asymp <- sapply(c("est", "se", "low", "upp"), function(y) ifelse(!is.na(print.object$asymp [, y]), formatC(print.object$asymp[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

      # Print names
      print.object$asymp <- rbind(c("Est.", "SE", "Low", "Upp"), print.object$asymp)

      # Justify right
      print.object$asymp <- apply(print.object$asymp, 2L, format, justify = "right")

      # Add blank space
      print.object$asymp[, "est"] <- paste0("   ", print.object$asymp[, "est"])

      #...................
      ### Print Output ####

      cat("  Asymptotic Normal Method\n")

      .write.table(print.object$asymp, left = 2L, right = 4L, horiz = horiz)

      cat("\n   Note.", switch(x$args$se, "sobel" = "Approximate standard error by Sobel (1982)\n",
                               "aroian" = "Exact standard error by Aroian (1947).\n",
                               "goodman" = "Unbiased standard error by Goodman (1960)\n"))

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Distribution of the Product Method ####

    if (isTRUE("dop" %in% print)) {

      # Round
      print.object$dop <- sapply(c("est", "se", "low", "upp"), function(y) ifelse(!is.na(print.object$dop [, y]), formatC(print.object$dop[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

      # Print names
      print.object$dop <- rbind(c("Est.", "SE", "Low", "Upp"), print.object$dop)

      # Justify right
      print.object$dop <- apply(print.object$dop, 2L, format, justify = "right")

      # Add blank space
      print.object$dop[, "est"] <- paste0("   ", print.object$dop[, "est"])

      #...................
      ### Print Output ####

      if (isTRUE("asymp" %in% print)) { cat("\n") }

      cat("  Distribution of the Product Method\n")

      .write.table(print.object$dop, left = 2L, right = 4L, horiz = horiz)

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Monte Carlo Method ####

    if (isTRUE("mc" %in% print)) {

      # Round
      print.object$mc <- sapply(c("est", "se", "low", "upp"), function(y) ifelse(!is.na(print.object$mc [, y]), formatC(print.object$mc[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

      # Print names
      print.object$mc <- rbind(c("Est.", "SE", "Low", "Upp"), print.object$mc)

      # Justify right
      print.object$mc <- apply(print.object$mc, 2L, format, justify = "right")

      # Add blank space
      print.object$mc[, "est"] <- paste0("   ", print.object$mc[, "est"])

      if (isTRUE(any(c("asymp", "dop") %in% print))) { cat("\n") }

      #...................
      ### Print Output ####

      # Monte Carlo Method
      cat("  Monte Carlo Method with",  format(x$args$nrep, scientific = FALSE), "repetitions\n")

      .write.table(print.object$mc, left = 2L, right = 4L, horiz = horiz)

    }

  #_____________________________________________________________________________
  #
  # Coefficient Alpha, item.alpha() --------------------------------------------
  }, item.alpha = {

    # Check input 'print'
    if (isTRUE(check)) { if (isTRUE(!all(print %in% c("all", "alpha", "item")))) { stop("Character strings in the argument 'print' do not all match with \"all\", \"alpha\", or \"item\".", call. = FALSE) } }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Arguments ####

    # Print coefficient alpha and/or item statistic
    if (length(print) == 1L && "all" %in% print) { print <- c("alpha", "item") }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Alpha ####

    if (isTRUE("alpha" %in% print)) {

      print.object$alpha$n <- format(paste(" ", print.object$alpha$n), justify = "right")

      print.object$alpha$items <- format(print.object$alpha$items, justify = "right")

      print.object$alpha$alpha <- formatC(print.object$alpha$alpha, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

      print.object$alpha$low <- formatC(print.object$alpha$low, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

      print.object$alpha$upp <- formatC(print.object$alpha$upp, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

      print.object$alpha <- rbind(c("n", "nNA", "Items", "Alpha", "Low", "Upp"), print.object$alpha)

      print.object$alpha <- apply(print.object$alpha, 2L, function(y) format(y, justify = "right"))

      if (isTRUE(x$args$type != "categ")) {

        cat(paste0(ifelse(isTRUE(x$args$std), " Standardized ", " Unstandardized "), "Coefficient Alpha with ", x$args$conf.level*100L, "% Confidence Interval\n\n"))

      } else {

        cat(paste0("Ordinal Coefficient Alpha with ", x$args$conf.level*100L, "% Confidence Interval\n\n"))

      }

      .write.table(print.object$alpha, left = 1L, right = 3L, horiz = horiz)

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Item Statistics ####

    if (isTRUE("item" %in% print)) {

      if (isTRUE(any(print.object$itemstat$pNA != 0L))) { print.object$itemstat$pNA <- paste0(formatC(print.object$itemstat$pNA, digits = 2L, format = "f", zero.print = paste0("0.", paste(rep(0L, times = 2L), collapse = ""))), "%") }

      print.object$itemstat$m <- formatC(print.object$itemstat$m, digits = 2L, format = "f", zero.print = paste0("0.", paste(rep(0L, times = 2L), collapse = "")))

      print.object$itemstat$sd <- formatC(print.object$itemstat$sd, digits = 2L, format = "f", zero.print = paste0("0.", paste(rep(0L, times = 2L), collapse = "")))

      print.object$itemstat$min <- formatC(print.object$itemstat$min, digits = 2L, format = "f", zero.print = paste0("0.", paste(rep(0L, times = 2L), collapse = "")))

      print.object$itemstat$max <- formatC(print.object$itemstat$max, digits = 2L, format = "f", zero.print = paste0("0.", paste(rep(0L, times = 2L), collapse = "")))

      print.object$itemstat$std.ld <- formatC(print.object$itemstat$std.ld, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

      print.object$itemstat$alpha <- formatC(print.object$itemstat$alpha, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

      print.object$itemstat <- rbind(c("Variable", "n", "nNA", "%NA", "M", "SD", "Min", "Max", "Std.Ld", "Alpha"), print.object$itemstat)

      # Format
      print.object$itemstat[, 1L] <- format(paste(" ", print.object$itemstat[, 1L]), justify = "left")
      print.object$itemstat[, -1L] <- apply(print.object$itemstat[, -1L], 2L, function(y) format(y, justify = "right"))

      # Output
      if (isTRUE("alpha" %in% print)) { cat("\n") }

      if (any(!is.na(x$result$itemstat$alpha))) { cat(paste0(" Standardized Factor Loadings and ", ifelse(x$args$type != "categ", "Coefficient Alpha ", "Ordinal Coefficient Alpha "), "if Item Deleted\n\n")) } else { cat(" Standardized Factor Loadings\n\n") }

      if (any(!is.na(x$result$itemstat$alpha))) { .write.table(print.object$itemstat, left = 1L, right = 3L) } else { .write.table(print.object$itemstat[, colnames(print.object$itemstat) != "alpha"], left = 1L, right = 3L, horiz = horiz) }

    }

  #_____________________________________________________________________________
  #
  # Confirmatory factor analysis, item.cfa() -----------------------------------
  }, item.cfa = {

    cat(" Confirmatory Factor Analysis\n")

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## lavaan Summary ####

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
    ## Covariance Coverage ####

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
      print.coverage <- cbind(paste0("   ", colnames(print.coverage)), print.coverage)

      # Column names
      print.coverage <- rbind(colnames(print.coverage), print.coverage)

      # Justify left and right
      print.coverage[, 1L] <- format(print.coverage[, 1L], justify = "left")
      print.coverage[, -1L] <- apply(print.coverage[, -1L], 2L, format, justify = "right")

      #...................
      ### Print ####

      .write.table(print.coverage, left = 2L, right = 4L, horiz = horiz)

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

        .write.table(print.itemstat, left = 2L, right = 4L, horiz = horiz)

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

        .write.table(print.itemfreq, left = 1L, right = 3L, horiz = horiz)

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

        .write.table(print.itemstat, left = 2L, right = 4L, horiz = horiz)

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

        .write.table(print.itemfreq, left = 1L, right = 3L, horiz = horiz)

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

      # MLMVS and PML estimator
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

      print.param <- rbind(c("", "", "", "", "Estimate", "Std.Err", "z-value", "P(>|z|)", "StdYX"), print.param)

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

      # Heading
      write.table(print.param[1L, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

      # Horizontal line
      cat(paste(rep(" ", times = 2), collapse = ""), paste(rep("\u2500", times = sum(nchar(print.param[1L, 4L:9L])) + length(print.param[1L, 4L:9L]) - 4), collapse = ""), "\n")

      # Factor loadings
      cat("   Factor Loadings\n")

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

      #### Latent variable covariances ####
      if (isTRUE(any(print.param$param %in% "latent variable covariance"))) {

        print.lv.cov <- print.param[print.param$param == "latent variable covariance", ]

        if (isTRUE(nrow(print.lv.cov) == 1L)) { cat("\n   Latent Variable Covariances\n") } else { cat("\n   Latent Variable Covariances\n") }

        for (i in unique(print.lv.cov$lhs)) {

          write.table(print.lv.cov[print.lv.cov$lhs == i, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

        }

      }

      #### Residual covariances ####
      if (isTRUE(any(print.param$param %in% "residual covariance"))) {

        print.res.cov <- print.param[print.param$param == "residual covariance", ]

        if (isTRUE(length(unique(print.res.cov$lhs)) == 1L)) { cat("\n   Residual Covariance\n") } else { cat("\n   Residual Covariances\n") }

        for (i in unique(print.res.cov$lhs)) {

          write.table(print.res.cov[print.res.cov$lhs == i, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

        }

      }

      ####  Latent means ####
      if (isTRUE(any(print.param$param %in% "latent mean"))) {

        print.mean <- print.param[print.param$param == "latent mean", ]

        print.mean[grep("NA", print.mean[, "z"]), c("se", "z", "pvalue", "stdyx")] <- ""

        if (isTRUE(nrow(print.mean) == 1L)) { cat("\n   Latent Mean\n") } else { cat("\n   Latent Means\n") }

        write.table(print.mean[, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

      }

      #### Latent variances ####
      if (isTRUE(any(print.param$param %in% "latent variance"))) {

        print.var <- print.param[print.param$param == "latent variance", ]

        print.var[grep("NA", print.var[, "z"]), c("se", "z", "pvalue", "stdyx")] <- ""

        if (isTRUE(nrow(print.var) == 1L)) { cat("\n   Latent Variance\n") } else { cat("\n   Latent Variances\n") }

        write.table(print.var[, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

      }

      #### Intercepts ####
      if (isTRUE(any(print.param$param %in% "intercept"))) {

        cat("\n   Intercepts\n")

        print.inter <- print.param[print.param$param == "intercept", ]

        print.inter[grep("NA", print.inter[, "z"]), c("se", "z", "pvalue", "stdyx")] <- ""

        write.table(print.inter[, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

      }

      #### Thresholds ####
      if (isTRUE(any(print.param$param %in% "threshold"))) {

        cat("\n   Thresholds\n")

        print.thres <- print.param[print.param$param == "threshold", ]

        print.thres[grep("NA", print.thres[, "z"]), c("se", "z", "pvalue", "stdyx")] <- ""

        write.table(print.thres[, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

      }

      #### Scales ####
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

      #### Residual variance ####
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

      #### Modification indices not available ####
      if (isTRUE(is.null(print.modind))) {

        cat("\n   Modification indices are not available.\n")

      #### Modification available ####
      } else {

        # Filter modification indices
        print.modind <- print.modind[which(print.modind[, "mi"] >= x$args$mod.minval), ]

        ##### Modification not above minimum value
        if (isTRUE(nrow(print.modind) == 0L)) {

          cat(paste0("   No modification indices above the minimum value ", round(x$args$mod.minval, digits = 2L), ".\n"))

        ##### Modification above minimum value
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

          # Factor loadings
          print.modind.load <- print.modind[print.modind$op == "=~", ]

          if (isTRUE(nrow(print.modind.load) > 0L)) {

            cat("   Factor Loading\n")

            # Print header
            write.table(print.modind[1L, ], quote = FALSE, row.names = FALSE, col.names = FALSE)

            # Horizontal line
            cat(paste(rep(" ", times = 3), collapse = ""), paste(rep("\u2500", times = sum(nchar(print.modind[1L, ])) + length(print.modind[1L, ]) - 5), collapse = ""), "\n")

            write.table(print.modind.load, quote = FALSE, row.names = FALSE, col.names = FALSE)

            cat("\n")

          }

          # Residual covariances
          print.modind.cov <- print.modind[print.modind$op == "~~", ]

          if (isTRUE(nrow(print.modind.cov) > 0L)) {

            cat("   Residual Covariances\n")

            # Print header
            write.table(print.modind[1L, ], quote = FALSE, row.names = FALSE, col.names = FALSE)

            # Horizontal line
            cat(paste(rep(" ", times = 3), collapse = ""), paste(rep("\u2500", times = sum(nchar(print.modind[1L, ])) + length(print.modind[1L, ]) - 5), collapse = ""), "\n")

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

      #...................
      ### Residual correlation matrix not available ####

      if (isTRUE(is.null(unlist(print.resid)))) {

        cat("   Residual correlation matrix is not available.")

      #...................
      ### Residual correlation matrix available ####

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

        .write.table(print.resid, left = 2L, right = 4L, horiz = horiz)

        # Note
        if (isTRUE(x$args$resid.minval < 1L)) { cat(paste0("\n  Note. Minimum absolute value for highlighting correlation coefficients is ", round(x$args$resid.minval, digits = 2L), "\n")) }

      }

    }
  #_____________________________________________________________________________
  #
  # Between-Group and Longitudinal MI Evaluation, item.invar() -----------------
  }, item.invar = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Check ####

    # Print argument
    .check.input(m.character = list(print = c("all", "summary", "partial", "coverage", "descript", "fit", "est", "modind", "resid")), envir = environment(), input.check = check)

    # Estimated models
    if (isTRUE(!x$args$lavaan.run && any(c("fit", "est", "modind", "resid") %in% x$args$print))) { stop("Measurement models were not estimated given the argument setting lavaan.run = FALSE.", call. = FALSE) }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Header ####

    if (isTRUE(!x$args$long)) { cat(" Between-Group Measurement Invariance Evaluation\n") } else { cat(" Longitudinal Measurement Invariance Evaluation\n") }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## lavaan summary ####

    if (isTRUE("summary" %in% print && !is.null(print.object$summary))) {

      #...................
      ### Remove Column Names ####

      colnames(print.object$summary) <- NULL

      #...................
      ### Extract and Split Output ####

      print.summary1 <- print.object$summary
      print.summary2 <- print.object$summary[-c(1L:13L), ]

      #...................
      ### Remove Parameterization ####

      if (isTRUE(!x$args$ordered)) { print.summary1 <- print.summary1[-12, ] }

      #...................
      ### Format ####

      # Include spaces
      print.summary1[1L, 1L] <- paste0(" ", print.summary1[1L, 1L])
      print.summary1[-1L, 1L] <- paste0("  ", unlist(print.summary1[-1L, 1L]))

      print.summary2[1L, 1L] <- paste0(" ", print.summary2[1L, 1L])
      print.summary2[-1L, 1L] <- paste0("  ", unlist(print.summary2[-1L, 1L]))

      if (isTRUE(!x$arg$long)) {

        print.summary2[(which(print.summary2[, 1L] == "  Number of Observations per Group") + 1L) |> (\(y) y:(y + length(na.omit(unique(x$data$.group))) - 1L))(), 1L] <- paste0(" ", unlist(print.summary2[(which(print.summary2[, 1L] == "  Number of Observations per Group") + 1L) |> (\(y) y:(y + length(na.omit(unique(x$data$.group))) - 1L))(), 1L]))

        if (isTRUE(!is.null(x$args$cluster))) { print.summary2[(which(print.summary2[, 1L] == "  Number of Clusters") + 1L) |> (\(y) y:(y + length(na.omit(unique(x$data$.group))) - 1L))(), 1L] <- paste0(" ", unlist(print.summary2[(which(print.summary2[, 1L] == "  Number of Clusters") + 1L) |> (\(y) y:(y + length(na.omit(unique(x$data$.group))) - 1L))(), 1L])) }

      }

      # Justify left
      print.summary1[, 1L] <- format(print.summary1[, 1L], justify = "left")
      print.summary2[, 1L] <- format(print.summary2[, 1L], justify = "left")

      #...................
      ### Print ####

      print(print.summary1[1L:11L, ], col.names = FALSE, row.names = FALSE, quote = FALSE, right = TRUE, max = 99999L)
      print(print.summary2, col.names = FALSE, row.names = FALSE, quote = FALSE, right = TRUE, max = 99999L)

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Partial Invariance Specification ####

    if (isTRUE("partial" %in% print && !is.null(x$args$partial))) {

      #...................
      ### Extract Output ####

      print.partial <- print.object$partial

      #...................
      ### Continuous Indicators ####

      if (isTRUE(!x$args$ordered)) {

        #### Between-Group with Two Groups or Longitudinal Measurement Invariance ####

        if (isTRUE((!x$args$long && length(unique(na.omit(x$data$.group))) == 2L) || x$args$long)) {

          # Labels
          print.partial$param <- misty::chr.gsub(c("load", "inter", "resid"), replacement = c("Factor Loading", "Intercept", "Residual Variance"), print.partial$param)

          # Add space
          print.partial$param[1L] <- paste0(print.partial$param[1L], paste0(rep(" ", times = (max(c(nchar(print.summary1[, 1L]), nchar(print.summary1[, 2L]))) - 2L) - nchar(print.partial$param[1L])), collapse = ""), collapse = "")

          # Format
          print.partial$param <- paste0("   ", format(print.partial$param, justify = "left"))
          print.partial <- if (isTRUE(nrow(print.partial) == 1L)) { as.data.frame(matrix(gsub("NA", "", print.partial), byrow = TRUE, ncol = ncol(print.partial))) } else { print.partial <- apply(print.partial, 2L, function(y) ifelse(is.na(y), "", y)) }

          # Quotation marks
          print.partial[, -1L] <- apply(print.partial[, -1L, drop = FALSE], 2L, function(y) ifelse(misty::chr.trim(y) != "", paste0("\"", y ,"\""), ""))

        #### Between-Group Measurement Invariance with more than Two Groups ####

        } else if (isTRUE(!x$args$long && length(unique(na.omit(x$data$.group))) > 2L)) {

          # Factor Loadings
          if (isTRUE("load" %in% print.partial$param)) {

            partial.load <- print.partial[print.partial$param == "load", ] |> (\(p) rbind(c("Factor Loading", rep("", times = ncol(p) - 2L)), p[, -1L]))()
            partial.load[-1L, 1L] <- paste0(" ", gsub("g", "Group ", partial.load[-1L, 1L]))

          } else { partial.load <- NULL }

          # Intercepts
          if (isTRUE("inter" %in% print.partial$param)) {

            partial.inter <- print.partial[print.partial$param == "inter", ] |> (\(p) rbind(c("Intercept", rep("", times = ncol(p) - 2L)), p[, -1L]))()
            partial.inter[-1L, 1L] <- paste0(" ", gsub("g", "Group ", partial.inter[-1L, 1L]))

          } else { partial.inter <- NULL }

          # Residual variances
          if (isTRUE("resid" %in% print.partial$param)) {

            partial.resid <- print.partial[print.partial$param == "resid", ] |> (\(p) rbind(c("Residual Variance", rep("", times = ncol(p) - 2L)), p[, -1L]))()
            partial.resid[-1L, 1L] <- paste0(" ", gsub("g", "Group ", partial.resid[-1L, 1L]))

          } else { partial.resid <- NULL }

          # Empty lines
          if (isTRUE(!is.null(partial.load)))  { partial.load <- rbind(partial.load, matrix("", ncol = ncol(partial.load), dimnames = list(NULL, colnames(partial.load)))) }
          if (isTRUE(!is.null(partial.inter))) { partial.inter <- rbind(partial.inter, matrix("", ncol = ncol(partial.inter), dimnames = list(NULL, colnames(partial.inter)))) }
          if (isTRUE(!is.null(partial.resid))) { partial.resid <- rbind(partial.resid, matrix("", ncol = ncol(partial.resid), dimnames = list(NULL, colnames(partial.resid)))) }

          # Combine tables
          print.partial <- as.data.frame(format(rbind(partial.load, partial.inter, partial.resid), justify = "left"))

          # Add space
          print.partial[1L, 1L] <- paste0(print.partial[1L, 1L], paste0(rep(" ", times = (max(c(nchar(print.summary1[, 1L]), nchar(print.summary1[, 2L]))) - 2L) - nchar(print.partial[1L, 1L])), collapse = ""), collapse = "")

          # Format
          print.partial[, 1L] <- paste0("   ", format(print.partial[, 1L], justify = "left"))
          print.partial <- apply(format(print.partial), 2L, function(y) gsub("NA", "", y))

          # Quotation marks
          print.partial[, -1L] <- apply(print.partial[, -1L, drop = FALSE], 2L, function(y) ifelse(misty::chr.trim(y) != "", paste0("\"", y ,"\""), ""))

        }

      #...................
      ### Ordered Categorical Indicators ####

      } else {

        #### Between-Group with Two Groups or Longitudinal Measurement Invariance ####

        if (isTRUE((!x$args$long && length(unique(na.omit(x$data$.group))) == 2L) || x$args$long)) {

          # Thresholds
          if (isTRUE("thres" %in% print.partial$param)) { print.partial <- print.partial[print.partial$param == "thres", ] |> (\(p) rbind(c("Threshold", rep("", times = ncol(p) - 2L)), misty::df.rename(p[, -1L], from = "thres", to = "param")))() |> (\(q) rbind(q, print.partial[-which(print.partial$param %in% "thres"), -2L]))() }

          # Labels
          print.partial$param <- misty::chr.gsub(c("load", "inter", "resid"), replacement = c("Factor Loading", "Intercept", "Residual Variance"), print.partial$param)

          # Add space
          print.partial$param[1L] <- paste0(print.partial$param[1L], paste0(rep(" ", times = (max(c(nchar(print.summary1[, 1L]), nchar(print.summary1[, 2L]))) - 2L) - nchar(print.partial$param[1L])), collapse = ""), collapse = "")

          # Format
          print.partial$param <- paste0("   ", format(print.partial$param, justify = "left"))
          print.partial$param[substr(misty::chr.trim(print.partial$param), 1L, 1L) == "t"] <- paste0(" ", format(print.partial$param[substr(misty::chr.trim(print.partial$param), 1L, 1L) == "t"], justify = "left"))

          print.partial <- if (isTRUE(nrow(print.partial) == 1L)) { as.data.frame(matrix(gsub("NA", "", print.partial), byrow = TRUE, ncol = ncol(print.partial))) } else { print.partial <- apply(print.partial, 2L, function(y) ifelse(is.na(y), "", y)) }

          # Quotation marks
          print.partial[, -1L] <- apply(print.partial[, -1L, drop = FALSE], 2L, function(y) ifelse(misty::chr.trim(y) != "", paste0("\"", y ,"\""), ""))

        #### Between-Group Measurement Invariance with more than Two Groups ####

        } else if (isTRUE(!x$args$long && length(unique(na.omit(x$data$.group))) > 2L)) {

          # Thresholds
          print.thres <- NULL
          if (isTRUE("thres" %in% print.partial$param)) {

            print.partial[print.partial$param == "thres", ] |>
              (\(p) for (i in unique(p$group)) { print.thres <<- rbind(print.thres, cbind(group = c(sub("g", "Group ",i), p[p$group == i, "thres"]), rbind(rep("", times = ncol(p) -  3L), p[p$group == i, setdiff(colnames(p), c("param", "group", "thres")), drop = FALSE]))) })()
            print.thres <- as.data.frame(misty::df.rename(rbind(c("Threshold", rep("", times = (ncol(print.thres) - 1L))), print.thres), from = "group", to = "param"))

            print.thres$param[substr(print.thres$param, 1L, 1L) == "t"] <- paste0(" ", print.thres$param[substr(print.thres$param, 1L, 1L) == "t"])

            print.thres[-1L, 1L] <- paste0(" ", gsub("g", "Group", print.thres[-1L, 1L]))

          }

          # Factor Loadings
          if (isTRUE("load" %in% print.partial$param)) {

            partial.load <- print.partial[print.partial$param == "load", ] |>
              (\(p) if (isTRUE(nrow(p) > 1L)) { rbind(c("Factor Loading", rep("", times = ncol(p) - 2L)), p[, -1L]) } else {

                if (ncol(p) == 3L) { setNames(data.frame(c("Factor Loading", p[, 2L]),  c("", p[, -c(1L, 2L)])), nm = c("group", names(p)[-c(1L, 2L)])) } else {

                  setNames(cbind(c("Factor Loading", p[, 2L]), rbind(rep("", times = ncol(p) - 2L), p[, -c(1L, 2L)])), nm = c("group", names(p)[-c(1L, 2L)]))

                }

              })() |> (\(q) if (isTRUE("thres" %in%  colnames(q))) { q[, setdiff(colnames(q), "thres")] } else { q })() |> (\(r) misty::df.rename(r, from = "group", to = "param"))()
            partial.load[-1L, 1L] <- paste0(" ", gsub("g", "Group ", partial.load[-1L, 1L]))

          } else { partial.load <- NULL }

          # Intercepts
          if (isTRUE("inter" %in% print.partial$param)) {

            partial.inter <- print.partial[print.partial$param == "inter", ] |>
              (\(p) if (isTRUE(nrow(p) > 1L)) { rbind(c("Intercept", rep("", times = ncol(p) - 2L)), p[, -1L]) } else {

                if (ncol(p) == 3L) { setNames(data.frame(c("Intercept", p[, 2L]),  c("", p[, -c(1L, 2L)])), nm = c("group", names(p)[-c(1L, 2L)])) } else {

                  setNames(cbind(c("Intercept", p[, 2L]), rbind(rep("", times = ncol(p) - 2L), p[, -c(1L, 2L)])), nm = c("group", names(p)[-c(1L, 2L)]))

                }

              })() |> (\(q) if (isTRUE("thres" %in%  colnames(q))) { q[, setdiff(colnames(q), "thres")] } else { q })() |> (\(r) misty::df.rename(r, from = "group", to = "param", check = FALSE))()

            partial.inter[-1L, 1L] <- paste0(" ", gsub("g", "Group ", partial.inter[-1L, 1L]))

          } else { partial.inter <- NULL }

          # Residual variances
          if (isTRUE("resid" %in% print.partial$param)) {

            partial.resid <- print.partial[print.partial$param == "resid", ] |>
              (\(p) if (isTRUE(nrow(p) > 1L)) { rbind(c(ifelse(x$args$parameterization == "delta", "Scaling Factor", "Residual Variance"), rep("", times = ncol(p) - 2L)), p[, -1L]) } else {

                if (ncol(p) == 3L) { setNames(data.frame(c(ifelse(x$args$parameterization == "delta", "Scaling Factor", "Residual Variance"), p[, 2L]),  c("", p[, -c(1L, 2L)])), nm = c("group", names(p)[-c(1L, 2L)])) } else {

                  setNames(cbind(c(ifelse(x$args$parameterization == "delta", "Scaling Factor", "Residual Variance"), p[, 2L]), rbind(rep("", times = ncol(p) - 2L), p[, -c(1L, 2L)])), nm = c("group", names(p)[-c(1L, 2L)]))

                }

              })() |> (\(q) if (isTRUE("thres" %in%  colnames(q))) { q[, setdiff(colnames(q), "thres")] } else { q })() |> (\(r) misty::df.rename(r, from = "group", to = "param"))()
            partial.resid[-1L, 1L] <- paste0(" ", gsub("g", "Group ", partial.load[-1L, 1L]))

          } else { partial.resid <- NULL }

          # Empty lines
          if (isTRUE(!is.null(print.thres)))   { print.thres <- rbind(print.thres, matrix("", ncol = ncol(print.thres), dimnames = list(NULL, colnames(print.thres)))) }
          if (isTRUE(!is.null(partial.load)))  { partial.load <- rbind(partial.load, matrix("", ncol = ncol(partial.load), dimnames = list(NULL, colnames(partial.load)))) }
          if (isTRUE(!is.null(partial.inter))) { partial.inter <- rbind(partial.inter, matrix("", ncol = ncol(partial.inter), dimnames = list(NULL, colnames(partial.inter)))) }
          if (isTRUE(!is.null(partial.resid))) { partial.resid <- rbind(partial.resid, matrix("", ncol = ncol(partial.resid), dimnames = list(NULL, colnames(partial.resid)))) }

          # Combine tables
          print.partial <- format(rbind(print.thres, partial.load, partial.inter, partial.resid), justify = "left")

          # Add space
          print.partial[1L, 1L] <- paste0(print.partial[1L, 1L], paste0(rep(" ", times = (max(c(nchar(print.summary1[, 1L]), nchar(print.summary1[, 2L]))) - 2L) - nchar(print.partial[1L, 1L])), collapse = ""), collapse = "")

          # Format
          print.partial[, 1L] <- paste0("   ", format(print.partial[, 1L], justify = "left"))
          print.partial <- as.data.frame(apply(format(print.partial), 2L, function(y) gsub("NA", "", y)))

          # Quotation marks
          print.partial[, -1L] <- apply(print.partial[, -1L, drop = FALSE], 2L, function(y) ifelse(misty::chr.trim(y) != "", paste0("\"", y ,"\""), ""))

        }

      }

      #...................
      ### Print ####

      cat("\n  Partial Measurement Invariance\n\n")

      write.table(print.partial, quote = FALSE, row.names = FALSE, col.names = FALSE)

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Covariance coverage ####

    if (isTRUE("coverage" %in% print && !is.null(print.object$coverage))) {

      cat("\n  Covariance Coverage of Data\n\n")

      print.coverage <- print.object$coverage

      #...................
      ### Between-Group Measurement Invariance ####

      if (isTRUE(!x$args$long)) {

        for (i in seq_along(print.coverage)) {

          #### Format ###

          # Lower Triangular
          print.coverage[[i]][upper.tri(print.coverage[[i]])] <- ""

          # Round
          print.coverage[[i]] <- apply(print.coverage[[i]], 2L, function(y) ifelse(!is.na(as.numeric(y)), formatC(as.numeric(y), digits = p.digits - 1L, format = "f", zero.print = ifelse(p.digits - 1L > 0L, paste0("0.", paste(rep(0L, times = 2L), collapse = "")), "0")), ""))

          # Row Names
          print.coverage[[i]] <- cbind(paste0(colnames(print.coverage[[i]])), print.coverage[[i]])

          # Column Names
          print.coverage[[i]] <- rbind(colnames(print.coverage[[i]]), print.coverage[[i]])

          # Justify Left and Right
          print.coverage[[i]] <- apply(print.coverage[[i]], 2L, format, justify = "right")

          # Add Blank Space
          print.coverage[[i]][, 1L] <- paste0("    ", print.coverage[[i]][, 1L])

          #### Print ###

          cat("   Group:", names(print.coverage)[i], "\n")

          .write.table(print.coverage[[i]], left = 3L, right = 5L, horiz = horiz)

          if (isTRUE(i != rev(seq_along(print.coverage))[1L])) { cat("\n") }

        }

      #...................
      ### Longitudinal Measurement Invariance ####

      } else {

        #### Format ###

        # Lower Triangular
        print.coverage[upper.tri(print.coverage)] <- ""

        # Round
        print.coverage <- apply(print.coverage, 2L, function(y) ifelse(!is.na(as.numeric(y)), formatC(as.numeric(y), digits = p.digits - 1L, format = "f", zero.print = ifelse(p.digits - 1L > 0L, paste0("0.", paste(rep(0L, times = 2L), collapse = "")), "0")), ""))

        # Row Names
        print.coverage <- cbind(paste0(colnames(print.coverage)), print.coverage)

        # Column Names
        print.coverage <- rbind(colnames(print.coverage), print.coverage)

        # Justify Left and Right
        print.coverage <- apply(print.coverage, 2L, format, justify = "right")

        # Add Blank Space
        print.coverage[, 1L] <- paste0("    ", print.coverage[, 1L])

        #### Print ###

        .write.table(print.coverage, left = 3L, right = 5L, horiz = horiz)

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Descriptive Statistics ####

    if (isTRUE("descript" %in% print && !is.null(print.object$descript))) {

      #...................
      ### Continuous Indicators ####

      if (isTRUE(!x$args$ordered)) {

        cat("\n  Univariate Sample Statistics\n\n")

        print.itemstat <- print.object$descript$stat

        #### Format ####

        # Variables to round
        print.round <- c("m", "sd", "min", "max", "skew", "kurt")

        # Round
        print.itemstat[, print.round] <- sapply(print.round, function(y) ifelse(!is.na(print.itemstat[, y]), formatC(print.itemstat[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))
        print.itemstat[, "pNA"] <- formatC(print.itemstat[, "pNA"], digits = digits - 1L, format = "f", zero.print = ifelse(digits - 1L > 0L, paste0("0.", paste(rep(0L, times = digits - 1L), collapse = "")), "0"))

        # Percentages
        print.itemstat[, "pNA"] <- paste0(print.itemstat[, "pNA"], "%")

        # Column names
        if (isTRUE(!x$args$long)) {

          print.itemstat <- rbind(c("Group", "Variable", "n", "nNA", "%NA", "M", "SD", "Min", "Max",  "Skew", "Kurt"), print.itemstat)

        } else {

          print.itemstat <- rbind(c("Variable", "n", "nNA", "%NA", "M", "SD", "Min", "Max",  "Skew", "Kurt"), print.itemstat)

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

        #### Print ####

        .write.table(print.itemstat, left = 2L, right = 4L, horiz = horiz)


      #...................
      ### Ordered Categorical Indicators ####

      } else {

        cat("\n  Frequencies\n\n")

        print.itemstat <- print.object$descript$freq

        #### Between-Group Measurement Invariance ####

        if (isTRUE(!x$args$long)) {

          for (i in seq_along(print.itemstat)) {

            # Column names
            print.itemstat[[i]] <- format(rbind(sub("Var", "Variable", names(print.itemstat[[i]])), print.itemstat[[i]]))

            # Add Blank Space
            print.itemstat[[i]][-1, 1L] <- paste0("     ", print.itemstat[[i]][-1, 1L])
            print.itemstat[[i]][1, 1L]  <- paste0("    ", print.itemstat[[i]][1L, 1L])

            # Justify left and right
            print.itemstat[[i]][, 1L] <- format(print.itemstat[[i]][, 1L], justify = "left")
            print.itemstat[[i]][, -1L] <- apply(print.itemstat[[i]][, -1L], 2L, format, justify = "right")

            # Print
            cat("   Group:", names(print.itemstat)[i], "\n")

            .write.table(print.itemstat[[i]], left = 3L, right = 5L, horiz = horiz)

            if (isTRUE(i != rev(seq_along(print.itemstat))[1L])) { cat("\n") }

          }

        #### Longitudinal Measurement Invariance ####

        } else {

          # Column names
          print.itemstat <- format(rbind(sub("Var", "Variable", names(print.itemstat)), print.itemstat))

          # Add Blank Space
          print.itemstat[-1, 1L] <- paste0("     ", print.itemstat[-1, 1L])
          print.itemstat[1, 1L]  <- paste0("    ", print.itemstat[1L, 1L])

          # Justify left and right
          print.itemstat[, 1L] <- format(print.itemstat[, 1L], justify = "left")
          print.itemstat[, -1L] <- apply(print.itemstat[, -1L], 2L, format, justify = "right")

          # Print
          .write.table(print.itemstat, left = 3L, right = 5L, horiz = horiz)

        }

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Model Fit ####

    if (isTRUE("fit" %in% print && !is.null(print.object$fit))) {

      if (isTRUE(x$args$invar != "config")) { cat("\n  Model Fit Information and Model Comparison\n") } else { cat("\n  Model Fit Information\n") }

      print.fit <- print.object$fit

      print.fit.stand <- print.fit$stand
      print.fit.scaled <- print.fit$scaled
      print.fit.robust <- print.fit$robust

      #...................
      ### Round ####

      #### Fit Indices ####
      pos.round.stand <- which(print.fit.stand[, 1L] %in% c("CFI", "TLI", "RMSEA", "90 Percent CI - lower", "90 Percent CI - upper", "SRMR"))
      pos.round.scaled <- which(print.fit.scaled[, 1L] %in% c("CFI", "TLI", "RMSEA", "90 Percent CI - lower", "90 Percent CI - upper", "SRMR"))

      print.fit.stand[pos.round.stand, -1L] <- sapply(print.fit.stand[pos.round.stand, -1L], function(y) ifelse(!is.na(y), formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))
      if (isTRUE(!is.null(print.fit.scaled))) { print.fit.scaled[pos.round.scaled, -1L] <- sapply(print.fit.scaled[pos.round.scaled, -1L], function(y) ifelse(!is.na(y), formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA)) }
      if (isTRUE(!is.null(print.fit.robust))) { print.fit.robust[pos.round.scaled, -1L] <- sapply(print.fit.robust[pos.round.scaled, -1L], function(y) ifelse(!is.na(y), formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA)) }

      #### Chi-Squared and Information Criteria ####
      if (isTRUE(x$args$estimator != "PML")) {

        pos.noround.stand <- which(print.fit.stand[, 1L] %in% c("", "Chi-Square Test of Model Fit", "Incremental Fit Indices", "Absolute Fit Indices", "Information Criteria", "Degrees of freedom", "P-value", "CFI", "TLI", "RMSEA", "90 Percent CI - lower", "90 Percent CI - upper", "P-value RMSEA <= 0.05", "SRMR"))

      } else {

        pos.noround.stand <- which(print.fit.stand[, 1L] %in% c("", "Chi-Square Test of Model Fit", "Incremental Fit Indices", "Absolute Fit Indices", "Information Criteria", "P-value", "CFI", "TLI", "RMSEA", "90 Percent CI - lower", "90 Percent CI - upper", "P-value RMSEA <= 0.05", "SRMR"))

      }

      pos.noround.scaled <- which(print.fit.scaled[, 1L] %in% c("", "Chi-Square Test of Model Fit", "Incremental Fit Indices", "Absolute Fit Indices", "Information Criteria", "Degrees of freedom", "P-value", "CFI", "TLI", "RMSEA", "90 Percent CI - lower", "90 Percent CI - upper", "P-value RMSEA <= 0.05", "SRMR"))

      print.fit.stand[-pos.noround.stand, -1L] <- sapply(print.fit.stand[-pos.noround.stand, -1L], function(y) ifelse(!is.na(y), formatC(as.numeric(y), digits = digits - 1L, format = "f", zero.print = ifelse(digits - 1L - 1L > 0L, paste0("0.", paste(rep(0L, times = digits - 1L), collapse = "")), "0")), NA))
      if (isTRUE(!is.null(print.fit.scaled))) { print.fit.scaled[-pos.noround.scaled, -1L] <- sapply(print.fit.scaled[-pos.noround.scaled, -1L], function(y) ifelse(!is.na(y), formatC(as.numeric(y), digits = digits - 1L, format = "f", zero.print = ifelse(digits - 1L > 0L, paste0("0.", paste(rep(0L, times = digits - 1L), collapse = "")), "0")), NA)) }
      if (isTRUE(!is.null(print.fit.robust))) { print.fit.robust[-pos.noround.scaled, -1L] <- sapply(print.fit.robust[-pos.noround.scaled, -1L], function(y) ifelse(!is.na(y), formatC(as.numeric(y), digits = digits - 1L, format = "f", zero.print = ifelse(digits - 1L > 0L, paste0("0.", paste(rep(0L, times = digits - 1L), collapse = "")), "0")), NA)) }

      #### p.digits ####
      pos.round.stand <- which(print.fit.stand[, 1L] %in% c("P-value", "P-value RMSEA <= 0.05"))
      pos.round.scaled <- which(print.fit.scaled[, 1L] %in% c("P-value", "P-value RMSEA <= 0.05"))

      print.fit.stand[pos.round.stand, -1L] <- sapply(print.fit.stand[pos.round.stand, -1L], function(y) ifelse(!is.na(y), formatC(as.numeric(y), digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0")), NA))
      if (isTRUE(!is.null(print.fit.scaled))) { print.fit.scaled[pos.round.scaled, -1L] <- sapply(print.fit.scaled[pos.round.scaled, -1L], function(y) ifelse(!is.na(y), formatC(as.numeric(y), digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0")), NA)) }
      if (isTRUE(!is.null(print.fit.robust))) { print.fit.robust[pos.round.scaled, -1L] <- sapply(print.fit.robust[pos.round.scaled, -1L], function(y) ifelse(!is.na(y), formatC(as.numeric(y), digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0")), NA)) }

      #...................
      ### Add Labels ####

      print.fit.stand <- rbind(sub("Label", "", ifelse(substr(colnames(print.fit.stand), 1, 1) != "d", paste0(toupper(substr(colnames(print.fit.stand), 1, 1)), substr(colnames(print.fit.stand), 2, nchar(colnames(print.fit.stand)))), paste0(toupper(substr(colnames(print.fit.stand), 1, 2)), substr(colnames(print.fit.stand), 3, nchar(colnames(print.fit.stand)))))), print.fit.stand)
      if (isTRUE(!is.null(print.fit.scaled))) { print.fit.scaled <- rbind(sub("Label", "", ifelse(substr(colnames(print.fit.scaled), 1, 1) != "d", paste0(toupper(substr(colnames(print.fit.scaled), 1, 1)), substr(colnames(print.fit.scaled), 2, nchar(colnames(print.fit.scaled)))), paste0(toupper(substr(colnames(print.fit.scaled), 1, 2)), substr(colnames(print.fit.scaled), 3, nchar(colnames(print.fit.scaled)))))), print.fit.scaled) }
      if (isTRUE(!is.null(print.fit.robust))) { print.fit.robust <- rbind(sub("Label", "", ifelse(substr(colnames(print.fit.robust), 1, 1) != "d", paste0(toupper(substr(colnames(print.fit.robust), 1, 1)), substr(colnames(print.fit.robust), 2, nchar(colnames(print.fit.robust)))), paste0(toupper(substr(colnames(print.fit.robust), 1, 2)), substr(colnames(print.fit.robust), 3, nchar(colnames(print.fit.robust)))))), print.fit.robust) }

      #...................
      ### Replace NA with "" ####

      print.fit.stand <- unname(misty::na.as(print.fit.stand, na = ""))
      if (isTRUE(!is.null(print.fit.scaled))) { print.fit.scaled <- unname(misty::na.as(print.fit.scaled, na = "")) }
      if (isTRUE(!is.null(print.fit.robust))) { print.fit.robust <- unname(misty::na.as(print.fit.robust, na = "")) }

      #...................
      ### Add Blank Space ####

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
      ### Justify Left and Right ####

      print.fit.stand[, 1L] <- format(print.fit.stand[, 1L, drop = FALSE], justify = "left")
      if (isTRUE(!is.null(print.fit.scaled))) { print.fit.scaled[, 1L] <- format(print.fit.scaled[, 1L, drop = FALSE], justify = "left") }
      if (isTRUE(!is.null(print.fit.robust))) { print.fit.robust[, 1L] <- format(print.fit.robust[, 1L, drop = FALSE], justify = "left") }

      print.fit.stand[, -1L] <- apply(print.fit.stand[, -1L, drop = FALSE], 2L, function(y) format(y, justify = "right"))
      if (isTRUE(!is.null(print.fit.scaled))) { print.fit.scaled[, -1L] <- apply(print.fit.scaled[, -1L, drop = FALSE], 2L, function(y) format(y, justify = "right")) }
      if (isTRUE(!is.null(print.fit.robust))) { print.fit.robust[, -1L] <- apply(print.fit.robust[, -1L, drop = FALSE], 2L, function(y) format(y, justify = "right")) }

      #...................
      ### Print ####

      if (isTRUE("standard" %in% x$args$print.fit)) {

        cat("\n   Standard CFI, TLI, and RMSEA\n")

        # R Markdown not in progress
        if (is.null(getOption("knitr.in.progress"))) { print.fit.stand[1L, ] <- sub("D", "\u0394", print.fit.stand[1L, ]) }

        .write.table(print.fit.stand, left = 3L, right = 5L, horiz = horiz)

      }

      if (isTRUE("scaled" %in% x$args$print.fit)) {

        cat("\n   Scaled CFI, TLI, and RMSEA\n")

        # R Markdown not in progress
        if (is.null(getOption("knitr.in.progress"))) { print.fit.scaled[1L, ] <- sub("D", "\u0394", print.fit.scaled[1L, ]) }

        .write.table(print.fit.scaled, left = 3L, right = 5L, horiz = horiz)

      }

      if (isTRUE("robust" %in% x$args$print.fit)) {

        cat("\n   Robust CFI, TLI, and RMSEA\n")

        # R Markdown not in progress
        if (is.null(getOption("knitr.in.progress"))) { print.fit.robust[1L, ] <- sub("D", "\u0394", print.fit.robust[1L, ]) }

        .write.table(print.fit.robust, left = 3L, right = 5L, horiz = horiz)

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Parameter Estimates ####

    if (isTRUE("est" %in% print && !is.null(print.object$param))) {

      switch(x$args$invar,
             ### Configural invariance ####
             config = {

               cat("\n  Model Results: Configural Invariance Model\n")
               print.param <- print.object$param$config

             ### Threshold invariance ####
             }, thres = {

               cat("\n  Model Results: Threshold Invariance Model\n")
               print.param <- print.object$param$thres

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

      #...................
      ### Round ####

      # digits
      print.param[, c("est", "se", "z", "stdyx")] <- lapply(print.param[, c("est", "se", "z", "stdyx")], function(y) ifelse(!is.na(y), formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

      # p.digits
      print.param[, "pvalue"] <- formatC(as.numeric(print.param[, "pvalue"]), digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

      #...................
      ### Add Blank Spaces ####

      print.param[apply(print.param[, c("est", "se", "z", "stdyx")], 1L, function(y) all(is.na(y))), "rhs"] <- paste("    ", print.param[apply(print.param[, c("est", "se", "z", "stdyx")], 1L, function(y) all(is.na(y))), "rhs"])
      print.param[apply(print.param[, c("est", "se", "z", "stdyx")], 1L, function(y) any(!is.na(y))), "rhs"] <- paste("      ", print.param[apply(print.param[, c("est", "se", "z", "stdyx")], 1L, function(y) any(!is.na(y))), "rhs"])

      #...................
      ### Convert NA into "" ####

      print.param[apply(print.param[, c("est", "se", "z", "stdyx")], 1L, function(y) all(is.na(y))), c("est", "se", "z", "pvalue", "stdyx")] <- ""
      print.param[apply(print.param[, c("z", "pvalue")], 1L, function(y) all( is.na(y) | misty::chr.trim(y) == "NA"  ) ), c("z", "pvalue")] <- ""
      print.param[which(is.na(print.param$stdyx)), "stdyx"] <- ""

      #...................
      ### Column Names ####

      print.param <- rbind(c(if (isTRUE(!x$args$long)) { "" }, "", "", "", "", "", "Estimate", "Std.Err", "z-value", "P(>|z|)", "StdYX"), print.param)

      #...................
      ### Justify Left and Right ####

      print.param[, "rhs"] <- format(print.param[, "rhs", drop = FALSE], justify = "left")

      if (isTRUE(!x$args$long)) {

        print.param[, -c(1L:5L)] <- apply(print.param[, -c(1L:5L), drop = FALSE], 2L, function(y) format(y, justify = "right"))

      } else {

        print.param[, -c(1L:4L)] <- apply(print.param[, -c(1L:4L), drop = FALSE], 2L, function(y) format(y, justify = "right"))

      }

      #...................
      ### Print ####

      #### Between-Group Measurement Invariance ####
      if (isTRUE(!x$args$long)) {

        for (i in misty::chr.omit(unique(print.param$group))) {

          cat(paste0("\n   Group: ", i, "\n\n"))

          ##### Factor Loading
          if (isTRUE(any(print.param[print.param$group == i, "param"] %in% "latent variable"))) {

            print.lv <- print.param[print.param$group == i & print.param$param == "latent variable", ]

            # Heading
            write.table(print.param[1L, c("rhs", "label", "est", "se", "z", "pvalue", "stdyx")], quote = FALSE, row.names = FALSE, col.names = FALSE)

            # Horizontal line
            cat(paste(rep(" ", times = 3L), collapse = ""), paste(rep("\u2500", times = sum(nchar(print.param[1L, c("rhs", "label", "est", "se", "z", "pvalue", "stdyx")])) + length(print.param[1L, c("rhs", "label", "est", "se", "z", "pvalue", "stdyx")]) - 5L), collapse = ""), "\n")

            cat("    Factor Loadings\n")

            for (j in unique(print.lv$lhs)) { write.table(print.lv[print.lv$lhs == j, 5L:11L], quote = FALSE, row.names = FALSE, col.names = FALSE) }

          }

          ##### Latent Variable Covariance
          if (isTRUE(any(print.param$param %in% "latent variable covariance"))) {

            print.lv.cov <- print.param[print.param$group == i & print.param$param == "latent variable covariance", ]

            if (isTRUE(length(unique(print.lv.cov$lhs)) == 1L)) { cat("\n    Latent Variable Covariance\n") } else { cat("\n    Latent Variable Covariances\n") }

            for (j in unique(print.lv.cov$lhs)) { write.table(print.lv.cov[print.lv.cov$lhs == j, c("rhs", "label", "est", "se", "z", "pvalue", "stdyx")], quote = FALSE, row.names = FALSE, col.names = FALSE) }

          }

          ##### Residual Covariance
          if (isTRUE(any(print.param$param %in% "residual covariance"))) {

            print.res.cov <- print.param[print.param$group == i & print.param$param == "residual covariance", ]

            if (isTRUE(length(unique(print.res.cov$lhs)) == 1L)) { cat("\n    Residual Covariance\n") } else { cat("\n    Residual Covariances\n") }

            for (j in unique(print.res.cov$lhs)) { write.table(print.res.cov[print.res.cov$lhs == j, c("rhs", "label", "est", "se", "z", "pvalue", "stdyx")], quote = FALSE, row.names = FALSE, col.names = FALSE) }

          }

          ##### Latent Mean
          if (isTRUE(any(print.param$param %in% "latent mean"))) {

            print.mean <- print.param[print.param$group == i & print.param$param == "latent mean", ]

            print.mean[grep("NA", print.mean[, "z"]), c("se", "z", "pvalue", "stdyx")] <- ""

            if (isTRUE(nrow(print.mean) == 1L)) { cat("\n    Latent Mean\n") } else { cat("\n    Latent Means\n") }

            write.table(print.mean[, c("rhs", "label", "est", "se", "z", "pvalue", "stdyx")], quote = FALSE, row.names = FALSE, col.names = FALSE)

          }

          ##### Latent Variance
          if (isTRUE(any(print.param$param %in% "latent variance"))) {

            print.var <- print.param[print.param$group == i & print.param$param == "latent variance", ]

            if (isTRUE(nrow(print.var) == 1L)) { cat("\n    Latent Variance\n") } else { cat("\n    Latent Variances\n") }

            write.table(print.var[, c("rhs", "label", "est", "se", "z", "pvalue", "stdyx")], quote = FALSE, row.names = FALSE, col.names = FALSE)

          }

          ##### Threshold
          if (isTRUE(any(print.param$param %in% "threshold"))) {

            print.thres <- print.param[print.param$group == i & print.param$param == "threshold", ]

            if (isTRUE(nrow(print.thres) == 1L)) { cat("\n    Threshold\n") } else { cat("\n    Thresholds\n") }

            write.table(print.thres[, c("rhs", "label", "est", "se", "z", "pvalue", "stdyx")], quote = FALSE, row.names = FALSE, col.names = FALSE)

          }

          ##### Intercept
          if (isTRUE(any(print.param$param %in% "intercept"))) {

            print.inter <- print.param[print.param$group == i & print.param$param == "intercept", ]

            if (isTRUE(nrow(print.inter) == 1L)) { cat("\n    Intercept\n") } else { cat("\n    Intercepts\n") }

            write.table(print.inter[, c("rhs", "label", "est", "se", "z", "pvalue", "stdyx")], quote = FALSE, row.names = FALSE, col.names = FALSE)

          }

          ##### Scaling Parameter
          if (isTRUE(any(print.param$param %in% "scale"))) {

            print.scale <- print.param[print.param$group == i & print.param$param == "scale", ]

            if (isTRUE(nrow(print.scale) == 1L)) { cat("\n    Scaling Parameter\n") } else { cat("\n    Scaling Parameters\n") }

            write.table(print.scale[, c("rhs", "label", "est", "se", "z", "pvalue", "stdyx")], quote = FALSE, row.names = FALSE, col.names = FALSE)

          }

          ##### Residual Variance
          if (isTRUE(any(print.param$param %in% "residual variance"))) {

            print.resid <- print.param[print.param$group == i & print.param$param == "residual variance", ]

            if (isTRUE(nrow(print.resid) == 1L)) { cat("\n    Residual Variance\n") } else { cat("\n    Residual Variances\n") }

            write.table(print.resid[, c("rhs", "label", "est", "se", "z", "pvalue", "stdyx")], quote = FALSE, row.names = FALSE, col.names = FALSE)

          }

        }

      #### Longitudinal Measurement Invariance ####
      } else {

        ##### Factor Loading
        if (isTRUE(any(print.param[, "param"] %in% "latent variable"))) {

          # Heading
          write.table(print.param[1L, c("rhs", "label", "est", "se", "z", "pvalue", "stdyx")], quote = FALSE, row.names = FALSE, col.names = FALSE)

          # Horizontal line
          cat(paste(rep(" ", times = 2), collapse = ""), paste(rep("\u2500", times = sum(nchar(print.param[1L, c("rhs", "label", "est", "se", "z", "pvalue", "stdyx")])) + length(print.param[1L, c("rhs", "label", "est", "se", "z", "pvalue", "stdyx")]) - 4), collapse = ""), "\n")

          cat("   Factor Loadings\n")

          print.lv <- print.param[print.param$param == "latent variable", ]

          for (i in unique(print.lv$lhs)) { write.table(print.lv[print.lv$lhs == i, c("rhs", "label", "est", "se", "z", "pvalue", "stdyx")], quote = FALSE, row.names = FALSE, col.names = FALSE) }

        }

        ##### Latent Variable Covariance
        if (isTRUE(any(print.param$param %in% "latent variable covariance"))) {

          print.lv.cov <- print.param[print.param$param == "latent variable covariance", ]

          if (isTRUE(length(unique(print.lv.cov$lhs)) == 1L)) { cat("\n   Latent Variable Covariance\n") } else { cat("\n   Latent Variable Covariances\n") }

          for (i in unique(print.lv.cov$lhs)) { write.table(print.lv.cov[print.lv.cov$lhs == i, c("rhs", "label", "est", "se", "z", "pvalue", "stdyx")], quote = FALSE, row.names = FALSE, col.names = FALSE) }

        }

        ##### Residual Covariance
        if (isTRUE(any(print.param$param %in% "residual covariance"))) {

          print.res.cov <- print.param[print.param$param == "residual covariance", ]

          if (isTRUE(length(unique(print.res.cov$lhs)) == 1L)) { cat("\n   Residual Covariance\n") } else { cat("\n   Residual Covariances\n") }


          for (i in unique(print.res.cov$lhs)) { write.table(print.res.cov[print.res.cov$lhs == i, c("rhs", "label", "est", "se", "z", "pvalue", "stdyx")], quote = FALSE, row.names = FALSE, col.names = FALSE) }

        }

        ##### Latent Mean
        if (isTRUE(any(print.param$param %in% "latent mean"))) {

          print.mean <- print.param[print.param$param == "latent mean", ]

          print.mean[misty::chr.trim(print.mean[, "z"]) == "", c("se", "z", "pvalue", "stdyx")] <- ""

          print.mean <- print.mean[match(unique(print.param[print.param$param == "latent variable", "lhs"]), print.mean$lhs), ]

          if (isTRUE(nrow(print.mean) == 1L)) { cat("\n   Latent Mean\n") } else { cat("\n   Latent Means\n") }

          write.table(print.mean[, c("rhs", "label", "est", "se", "z", "pvalue", "stdyx")], quote = FALSE, row.names = FALSE, col.names = FALSE)

        }

        ##### Latent Variance
        if (isTRUE(any(print.param$param %in% "latent variance"))) {

          print.var <- print.param[print.param$param == "latent variance", ]

          if (isTRUE(nrow(print.var) == 1L)) { cat("\n   Latent Variance\n") } else { cat("\n   Latent Variances\n") }

          write.table(print.var[, c("rhs", "label", "est", "se", "z", "pvalue", "stdyx")], quote = FALSE, row.names = FALSE, col.names = FALSE)

        }

        ##### Threshold
        if (isTRUE(any(print.param$param %in% "threshold"))) {

          print.thres <- print.param[print.param$param == "threshold", ]

          if (isTRUE(nrow(print.thres) == 1L)) { cat("\n   Threshold\n") } else { cat("\n   Thresholds\n") }

          write.table(print.thres[,c("rhs", "label", "est", "se", "z", "pvalue", "stdyx")], quote = FALSE, row.names = FALSE, col.names = FALSE)

        }

        ##### Intercept
        if (isTRUE(any(print.param$param %in% "intercept"))) {

          print.inter <- print.param[print.param$param == "intercept", ]

          if (isTRUE(nrow(print.inter) == 1L)) { cat("\n   Intercept\n") } else { cat("\n   Intercepts\n") }

          write.table(print.inter[,c("rhs", "label", "est", "se", "z", "pvalue", "stdyx")], quote = FALSE, row.names = FALSE, col.names = FALSE)

        }

        ##### Scaling Parameter
        if (isTRUE(any(print.param$param %in% "scale"))) {

          print.scale <- print.param[print.param$param == "scale", ]

          if (isTRUE(nrow(print.scale) == 1L)) { cat("\n   Scaling Parameter\n") } else { cat("\n   Scaling Parameters\n") }

          write.table(print.scale[,  c("rhs", "label", "est", "se", "z", "pvalue", "stdyx")], quote = FALSE, row.names = FALSE, col.names = FALSE)

        }

        ##### Residual Variance
        if (isTRUE(any(print.param$param %in% "residual variance"))) {

          print.resid <- print.param[print.param$param == "residual variance", ]

          if (isTRUE(nrow(print.resid) == 1L)) { cat("\n   Residual Variance\n") } else { cat("\n   Residual Variances\n") }

          write.table(print.resid[,  c("rhs", "label", "est", "se", "z", "pvalue", "stdyx")], quote = FALSE, row.names = FALSE, col.names = FALSE)

        }

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Modification indices ####

    if (isTRUE("modind" %in% print && !is.null(print.object$modind))) {

      switch(x$args$invar,
             ### Configural invariance ####
             config = {

               cat("\n  Modification Indices: Configural Invariance Model\n\n")
               print.modind <- print.object$modind$config

             ### Threshold invariance ####
             }, thres = {

               cat("\n  Modification Indices: Threshold Invariance Model\n\n")
               print.modind <- print.object$modind$thres

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

      #...................
      ### Modification Indices Not Available ####

      if (isTRUE(is.null(print.modind))) {

        cat("   Modification indices are not available.\n")

      #...................
      ### Modification Indices Available ####

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

            # Horizontal line
            cat(paste(rep(" ", times = 3), collapse = ""), paste(rep("\u2500", times = sum(nchar(print.modind[1L, ])) + length(print.modind[1L, ]) - 5), collapse = ""), "\n")

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

            # Horizontal line
            cat(paste(rep(" ", times = 3), collapse = ""), paste(rep("\u2500", times = sum(nchar(print.modind[1L, ])) + length(print.modind[1L, ]) - 5), collapse = ""), "\n")

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

             ### Threshold invariance ####
             }, thres = {

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

            print.score[-1L, c(1L:2L)] <- do.call("cbind", lapply(print.score[-1L, c(1L:2L)], function(y) paste0(" ", y)))

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

          .write.table(print.score, left = 2L, right = 4L, horiz = horiz)

          # Note
          cat(paste0("\n   Note. Minimum value for printing modification indices is ", round(x$args$mod.minval, digits = 2L), "\n"))

        }

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Residual Correlation Matrix ####

    if (isTRUE("resid" %in% print && !is.null(print.object$resid))) {

      #...................
      ### Level of Measurement Invariance ####

      switch(x$args$invar,
             # Configural invariance
             config = {

               cat("\n Residual Correlation Matrix: Configural Invariance Model\n\n")
               print.resid <- print.object$resid$config

             # Threshold invariance
             }, thres = {

               cat("\n  Residual Correlation Matrix: Threshold Invariance Model\n\n")
               print.resid <- print.object$resid$thres

             # Metric invariance
             }, metric = {

               cat("\n  Residual Correlation Matrix: Metric Invariance Model\n\n")
               print.resid <- print.object$resid$metric

             # Scalar invariance
             }, scalar = {

               cat("\n  Residual Correlation Matrix: Scalar Invariance Model\n\n")
               print.resid <- print.object$resid$scalar

             # Strict invariance
             }, strict = {

               cat("\n  Residual Correlation Matrix: Strict Invariance Model\n\n")
               print.resid <- print.object$resid$strict

             })

      #...................
      ### Residual Correlation Matrix Not Available ####

      if (isTRUE(is.null(unlist(print.resid)))) {

        cat("   Residual correlation matrix is not available.")

      #...................
      ### Residual Correlation Matrix Available ####

      } else {

        #### Numbers and Unicode
        numbers <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
        unicode <- c("\U1D7CE", "\U1D7CF", "\U1D7D0", "\U1D7EF", "\U1D7F0", "\U1D7F1", "\U1D7F2", "\U1D7F3", "\U1D7F4", "\U1D7F5")

        #### Between-group measurement invariance
        if (isTRUE(!x$args$long)) {

          # Round
          print.resid <- lapply(print.resid, function(y) ifelse(!is.na(y), formatC(y, digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0")), NA))

          # Correlation coefficients in boldface
          if (isTRUE(is.null(getOption("knitr.in.progress")) && x$args$resid.minval != 1L)) { for (i in seq_along(print.resid)) { print.resid[[i]][lower.tri(print.resid[[i]])][which(abs(x$result$resid[[x$args$invar]][[i]][lower.tri(x$result$resid[[x$args$invar]][[i]])]) > x$args$resid.minval)] <- na.omit(misty::chr.gsub(numbers, unicode, print.resid[[i]][lower.tri(print.resid[[i]])][which(abs(x$result$resid[[x$args$invar]][[i]][lower.tri(x$result$resid[[x$args$invar]][[i]])]) > x$args$resid.minval)])) } }

          ##### Continuous indicators
          if (isTRUE(!x$args$ordered)) {

            for (i in seq_along(print.resid)) {

              # Correlations
              cor.i <- print.resid[[i]][seq_len(ncol(print.resid[[i]])), ]

              # Lower triangular
              cor.i[upper.tri(cor.i)] <- ""

              # Correlations and means
              print.resid[[i]] <- rbind(cor.i, rep("", times = ncol(print.resid[[i]])), Mean = print.resid[[i]][nrow(print.resid[[i]]), ])

            }

          ##### Ordered categorical indicators
          } else {

            for (i in seq_along(print.resid)) {

              # Correlations
              cor.i <- print.resid[[i]][seq_len(ncol(print.resid[[i]])), ]

              # Lower triangular
              cor.i[upper.tri(cor.i)] <- ""

              # Correlations and thresholds
              print.resid[[i]] <- rbind(cor.i, rep("", times = ncol(print.resid[[i]])), matrix(print.resid[[i]][-seq_len(ncol(print.resid[[i]])), ], ncol = ncol(print.resid[[i]]), dimnames = list(sub("t", "Thres", row.names(print.resid[[i]][-seq_len(ncol(print.resid[[i]])), ])), NULL)))

            }

          }

          # Column names
          print.resid <- lapply(print.resid, function(y) rbind(colnames(y), y))
          print.resid <- lapply(print.resid, function(y) cbind(rownames(y), y))

          # Add blank spaces
          for (i in seq_along(print.resid)) { print.resid[[i]][, 1L] <- paste("   ", print.resid[[i]][, 1L]) }

          # Justify
          for (i in seq_along(print.resid)) {

            print.resid[[i]][, 1L] <- format(print.resid[[i]][, 1L], justify = "left")
            print.resid[[i]][, -1L] <- format(print.resid[[i]][, -1L], justify = "right")

          }

          # Print
          for (i in seq_along(print.resid)) {

            if (isTRUE(!x$args$long)) { cat("   Group:", names(print.resid)[i], "\n") }

            .write.table(print.resid[[i]], left = 3L, right = 5L, horiz = horiz)

            if (isTRUE(i != length(print.resid))) { cat("\n") }

          }

        #### Longitudinal measurement invariance
        } else {

          # Round
          print.resid <- apply(print.resid, 2L, function(y) ifelse(!is.na(y), formatC(y, digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0")), NA))

          # Correlation coefficients in boldface
          if (isTRUE(is.null(getOption("knitr.in.progress")) && x$args$resid.minval != 1L)) { print.resid[lower.tri(print.resid)][which(abs(x$result$resid[[x$args$invar]][lower.tri(x$result$resid[[x$args$invar]])]) > x$args$resid.minval)] <- na.omit(misty::chr.gsub(numbers, unicode, print.resid[lower.tri(print.resid)][which(abs(x$result$resid[[x$args$invar]][lower.tri(x$result$resid[[x$args$invar]])]) > x$args$resid.minval)])) }

          ##### Continuous indicators
          if (isTRUE(!x$args$ordered)) {

            # Correlations
            cor.i <- print.resid[seq_len(ncol(print.resid)), ]

            # Lower triangular
            cor.i[upper.tri(cor.i)] <- ""

            # Correlations and means
            print.resid <- rbind(cor.i, rep("", times = ncol(print.resid)), Mean = print.resid[nrow(print.resid), ])

          ##### Ordered categorical indicators
          } else {

            # Correlations
            cor.i <- print.resid[seq_len(ncol(print.resid)), ]

            # Lower triangular
            cor.i[upper.tri(cor.i)] <- ""

            # Correlations and thresholds
            print.resid <- rbind(cor.i, rep("", times = ncol(print.resid)), matrix(print.resid[-seq_len(ncol(print.resid)), ], ncol = ncol(print.resid), dimnames = list(sub("t", "Thres", row.names(print.resid[-seq_len(ncol(print.resid)), ])), NULL)))

          }

          # Column names
          print.resid <- rbind(colnames(print.resid), print.resid)
          print.resid <- cbind(rownames(print.resid), print.resid)

          # Add blank spaces
          print.resid[, 1L] <- paste("  ", print.resid[, 1L])

          # Justify
          print.resid[, 1L] <- format(print.resid[, 1L], justify = "left")
          print.resid[, -1L] <- format(print.resid[, -1L], justify = "right")

          # Print
          .write.table(print.resid, left = 2L, right = 4L, horiz = horiz)

        }

        # Note
        if (isTRUE(x$args$resid.minval < 1L)) { cat(paste0("\n  Note. Minimum absolute value for highlighting residuals is ", round(x$args$resid.minval, digits = 2L), "\n")) }

      }

    }

  #_____________________________________________________________________________
  #
  # Effect Size Measure of Measurement Non-Invariance, item.nonequi() ---------
  }, item.noninvar = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Header ####

    if (isTRUE(!x$args$long)) { cat(" Effect Size Measure for Between-Group Measurement Non-Invariance\n") } else { cat(" Effect Size Measure for Longitudinal Measurement Non-Invariance\n") }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## lavaan summary ####

    if (isTRUE("summary" %in% print)) {

      #--------------------------------------
      ### Remove Column Names ####

      colnames(print.object$summary) <- NULL

      #--------------------------------------
      ### Extract Output ####

      print.summary <- print.object$summary

      #--------------------------------------
      ### Remove Lines ####

      print.summary <- print.summary[!is.na(print.summary[, 2]), ]

      if (isTRUE(all(c("dmacs", "m.diff", "var.diff") %in% names(print.object$noninvar)) && x$args$pooled)) { print.summary <- print.summary[!print.summary[, 1L] %in% c("Reference Group", "Reference Time Point"), ] }

      #--------------------------------------
      ### Format ####

      print.summary[1L, 1L] <- paste0(" ", print.summary[1L, 1L])
      print.summary[-1L, 1L] <- paste0("  ", unlist(print.summary[-1L, 1L]))

      # Justify left
      print.summary[, 1L] <- format(print.summary[, 1L], justify = "left")

      #--------------------------------------
      ### Print ####

      print(print.summary, col.names = FALSE, row.names = FALSE, quote = FALSE, right = TRUE, max = 99999L)

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## dMACS ####

    if (isTRUE("dmacs" %in% print)) {

      #--------------------------------------
      ### Extract Output ####

      print.dmacs <- print.object$noninvar$dmacs

      #--------------------------------------
      ### Two Groups or Time Points ####

      if (isTRUE((is.null(dim(print.dmacs)) || is.data.frame(print.dmacs)) && class(print.dmacs) != "list")) {

        #...................
        #### One Factor ####

        if (isTRUE(!is.data.frame(print.dmacs))) {

          # Round
          print.dmacs <- formatC(print.dmacs, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

          # Format
          print.dmacs <- rbind(names(print.dmacs), print.dmacs)

          # Justify right
          print.dmacs <- format(print.dmacs, justify = "right")

          # Add blank space
          print.dmacs[, 1L] <- paste0("   ", print.dmacs[, 1L])

          ##### Print ####

          ###### Width of the Console sufficient
          if (isTRUE(max(apply(print.dmacs, 1L, function(y) nchar(paste(y, collapse = " ")))) < getOption("width"))) {

            if (isTRUE(!x$args$signed)) { cat("\n  dMACS\n\n") } else { cat("\n  Signed dMACS\n\n") }

            .write.table(print.dmacs, left = 2L, right = 4L, horiz = horiz)

          ###### Width of the Console insufficient
          } else {

            # Extract output
            print.dmacs <- data.frame(names(print.object$noninvar$dmacs), print.object$noninvar$dmacs, fix.empty.names = FALSE)

            # Round
            print.dmacs[, 2L] <- formatC(print.dmacs[, 2L], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

            # Justify right and left
            print.dmacs[, 1L] <- format(print.dmacs[, 1L], justify = "left")
            print.dmacs[, 2L] <- format(print.dmacs[, 2L], justify = "right")

            # Add blank space
            print.dmacs[, 1L] <- paste0("   ", print.dmacs[, 1L])

            # Print
            if (isTRUE(!x$args$signed)) { cat("\n  dMACS\n") } else { cat("\n  Signed dMACS\n") }

            write.table(print.dmacs, quote = FALSE, row.names = FALSE)

          }

        #...................
        #### More than One Factor ####

        } else {

          # Round
          print.dmacs <- sapply(print.dmacs, function(y) sapply(y, function(y) formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))))

          # Format
          print.dmacs <- cbind(c("", row.names(print.object$noninvar$dmacs)), rbind(colnames(print.dmacs), print.dmacs))

          # Justify right and left
          print.dmacs[, -1L] <- apply(print.dmacs[, -1L], 2L, format, justify = "right")
          print.dmacs[, 1L] <- format(print.dmacs[, 1L], justify = "left")

          # Add blank space
          print.dmacs[, 1L] <- paste0("   ", print.dmacs[, 1L])

          # Replace NA
          print.dmacs[, -1L] <- apply(print.dmacs[, -1L], 2L, function(y) gsub("NA", "  ", y))

          ##### Print ####

          if (isTRUE(!x$args$signed)) { cat("\n  dMACS\n\n") } else { cat("\n  Signed dMACS\n\n") }

          .write.table(print.dmacs, left = 2L, right = 4L, horiz = horiz)

        }

      #--------------------------------------
      ### More than Two Groups or Time Points ####

      } else {

        #...................
        #### One Factor ####

        if (isTRUE(all(sapply(print.dmacs, function(y) is.null(dim(y)))))) {

          # Round
          print.dmacs <- lapply(print.dmacs, function(y) formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")))

          # Format
          print.dmacs <- lapply(print.dmacs, function(y) rbind(names(y), y))

          # Justify right
          print.dmacs <- lapply(print.dmacs, function(y) format(y, justify = "right"))

          # Add blank space
          for (i in seq_along(print.dmacs)) { print.dmacs[[i]][, 1L] <- paste0("    ", print.dmacs[[i]][, 1L]) }

          ##### Print ####

          if (isTRUE(!x$args$signed)) { cat("\n  dMACS\n") } else { cat("\n  Signed dMACS\n") }

          ###### Width of the Console sufficient
          if (isTRUE(max(apply(do.call("rbind", print.dmacs), 1L, function(y) nchar(paste(y, collapse = " ")))) < getOption("width"))) {

            for (i in seq_along(print.dmacs)) {

              if (isTRUE(!x$args$long)) { cat(paste0("\n   Reference Group ", x$args$ref, " vs. ", "Focal Group ", names(print.dmacs)[[i]], "\n\n")) } else { cat(paste0("\n   Reference Time Point ", x$args$ref, " vs. ", "Focal Time Point ", names(print.dmacs)[[i]], "\n\n")) }

              .write.table(print.dmacs[[i]], left = 3L, right = 5L, horiz = horiz)

            }

          ###### Width of the Console insufficient
          } else {

            # Extract output
            print.dmacs <- lapply(print.object$noninvar$dmacs, function(y) data.frame(names(y), y, fix.empty.names = FALSE))

            for (i in seq_along(print.dmacs)) {

              # Round
              print.dmacs[[i]][, 2L] <- formatC(print.dmacs[[i]][, 2L], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

              # Justify right and left
              print.dmacs[[i]][, 1L] <- format(print.dmacs[[i]][, 1L], justify = "left")
              print.dmacs[[i]][, 2L] <- format(print.dmacs[[i]][, 2L], justify = "right")

              # Add blank space
              print.dmacs[[i]][, 1L] <- paste0("    ", print.dmacs[[i]][, 1L])

              # Print
              if (isTRUE(!x$args$long)) { cat(paste0("\n   Reference Group ", x$args$ref, " vs. ", "Focal Group ", names(print.dmacs)[[i]], "\n")) } else { cat(paste0("\n   Reference Time Point ", x$args$ref, " vs. ", "Focal Time Point ", names(print.dmacs)[[i]], "\n")) }

              write.table(print.dmacs[[i]], quote = FALSE, row.names = FALSE)

            }

          }

        #...................
        #### More than One Factor ####

        } else {

          # Round
          print.dmacs <- lapply(print.dmacs, function(y) sapply(y, function(z) formatC(z, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))))

          # Format
          print.dmacs <- lapply(print.dmacs, function(y) cbind(c("", row.names(print.object$noninvar$dmacs[[1L]])), rbind(colnames(y), y)))

          # Justify right and left
          for (i in seq_along(print.dmacs)) {

            print.dmacs[[i]][, -1L] <- apply(print.dmacs[[i]][, -1L], 2L, format, justify = "right")
            print.dmacs[[i]][, 1L] <- format(print.dmacs[[i]][, 1L], justify = "left")

            # Add blank space
            print.dmacs[[i]][, 1L] <- paste0("    ", print.dmacs[[i]][, 1L])

            # Replace NA
            print.dmacs[[i]][, -1L] <- apply(print.dmacs[[i]][, -1L], 2L, function(y) gsub("NA", "  ", y))

          }

          ##### Print ####

          if (isTRUE(!x$args$signed)) { cat("\n  dMACS\n") } else { cat("\n  Signed dMACS\n") }

          for (i in seq_along(print.dmacs)) {

            if (isTRUE(!x$args$long)) { cat(paste0("\n   Reference Group ", x$args$ref, " vs. ", "Focal Group ", names(print.dmacs)[i], "\n\n")) } else { cat(paste0("   Reference Time Point ", x$args$ref, " vs. ", "Focal Time Point ", names(print.dmacs)[i], "\n\n")) }

            .write.table(print.dmacs[[i]], left = 3L, right = 5L, horiz = horiz)

          }

        }

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Expected Bias in the Mean and Variance of the Total Score ####

    if (isTRUE("bias" %in% print)) {

      #--------------------------------------
      ### Extract Output ####

      print.bias <- print.object$noninvar[-1L]

      #--------------------------------------
      ### Two Groups or Time Points ####

      if (isTRUE(class(print.bias[[1L]]) != "list")) {

        #...................
        #### One Factor ####

        if (isTRUE(all(sapply(print.bias, length) <= 1L))) {

          # Round
          print.bias <- lapply(print.bias, function(y) formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")))

          # Format
          if (!isTRUE(x$args$ordered)) {

            print.bias <- as.data.frame(rbind(c("DMean", print.bias$m.diff), c("DVar", print.bias$v.diff)))

          } else {

            print.bias <- as.data.frame(cbind("DMean", print.bias$m.diff))

          }

          # Greek letter Delta
          if (is.null(getOption("knitr.in.progress"))) { print.bias[, 1L] <- gsub("D", "\U2206", print.bias[, 1L]) }

          # Justify right and left
          print.bias[, 1L] <- format(print.bias[, 1L], justify = "left")
          print.bias[, 2L] <- format(print.bias[, 2L], justify = "right")

          # Add blank space
          print.bias[, 1L] <- paste0("  ", print.bias[, 1L])

          # Column names
          colnames(print.bias) <- c("", "")

          ##### Print ####

          if (!isTRUE(x$args$ordered)) { cat("\n  Expected Bias in the Mean and Variance of the Total Score\n") } else { cat("\n  Expected Bias in the Mean of the Total Score\n") }

          print(print.bias, colnames = FALSE, row.names = FALSE, quote = FALSE, right = TRUE, max = 99999L)

        #...................
        #### More than One Factor ####

        } else {

          # Round
          print.bias <- lapply(print.bias, function(y) sapply(y, function(z) formatC(z, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))))

          # Format
          if (!isTRUE(x$args$ordered)) {

            print.bias <- as.data.frame(rbind(c("", names(print.bias$m.diff)), c("DMean", print.bias$m.diff), c("DVar", print.bias$v.diff)))

          } else {

            print.bias <- as.data.frame(rbind(c("", names(print.bias$m.diff)), c("DMean", print.bias$m.diff)))

          }

          # Greek letter Delta
          if (is.null(getOption("knitr.in.progress"))) { print.bias[, 1L] <- gsub("D", "\U2206", print.bias[, 1L]) }

          # Justify right and left
          print.bias[, -1L] <- apply(print.bias[, -1L], 2L, format, justify = "right")
          print.bias[, 1L] <- format(print.bias[, 1L], justify = "left")

          # Add blank space
          print.bias[, 1L] <- paste0("   ", print.bias[, 1L])

          ##### Print ####

          if (!isTRUE(x$args$ordered)) { cat("\n  Expected Bias in the Mean and Variance of the Total Score\n\n") } else { cat("\n  Expected Bias in the Mean of the Total Score\n\n") }

          .write.table(print.bias, left = 2L, right = 4L, horiz = horiz)

        }

      #--------------------------------------
      ### More than Two Groups or Time Points ####

      } else {

        #...................
        #### One Factor ####

        if (isTRUE(all(sapply(print.bias, function(y) sapply(y, function(z) length(z))) <= 1L))) {

          # Round
          print.bias.temp <- lapply(print.bias, function(y) lapply(y, function(z) formatC(z, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))))

          # Format
          print.bias <- list()
          if (!isTRUE(x$args$ordered)) {

            for(i in names(print.bias.temp[[1L]])) { print.bias[[i]] <- data.frame(c("DMean", "DVar"), do.call("rbind", lapply(print.bias.temp, function(y) y[i]))) }

          } else {

            for(i in names(print.bias.temp[[1L]])) { print.bias[[i]] <- data.frame("DMean", print.bias.temp$m.diff[i]) }

          }

          # Greek letter Delta
          if (is.null(getOption("knitr.in.progress"))) { for (i in seq_along(print.bias)) { print.bias[[i]][, 1L] <- gsub("D", "\U2206", print.bias[[i]][, 1L]) } }

          for (i in seq_along(print.bias)) {

            # Justify right and left
            print.bias[[i]][, 1L] <- format(print.bias[[i]][, 1L], justify = "left")
            print.bias[[i]][, 2L] <- format(print.bias[[i]][, 2L], justify = "right")

            # Add blank space
            print.bias[[i]][, 1L] <- paste0("   ", print.bias[[i]][, 1L])

            # Column names
            colnames(print.bias[[i]]) <- c("", "")

          }

          ##### Print ####

          if (!isTRUE(x$args$ordered)) { cat("\n  Expected Bias in the Mean and Variance of the Total Score\n") } else { cat("\n  Expected Bias in the Mean of the Total Score\n") }

          for (i in seq_along(print.bias)) {

            if (isTRUE(!x$args$long)) { cat(paste0("\n   Reference Group ", x$args$ref, " vs. ", "Focal Group ", names(print.object$noninvar[[2L]])[[i]], "\n")) } else { cat(paste0("\n   Reference Time Point ", x$args$ref, " vs. ", "Focal Time Point ", names(print.object$noninvar[[2L]])[[i]], "\n")) }

            print(print.bias[[i]], colnames = FALSE, row.names = FALSE, quote = FALSE, right = TRUE, max = 99999L)

          }

        #...................
        #### More than One Factor ####

        } else {

          # Round
          print.bias.temp <- lapply(print.bias, function(y) lapply(y, function(z) sapply(z, function(y) formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")))))

          # Format
          print.bias <- list()
          if (!isTRUE(x$args$ordered)) {

            for (i in names(print.bias.temp[[1L]])) { print.bias[[i]] <- as.data.frame(rbind(c("", names(print.bias.temp[[1L]][[1L]])), cbind(c("DMean", "DVar"), do.call("rbind", lapply(print.bias.temp, function(y) y[[i]]))))) }

          } else {

            for (i in names(print.bias.temp[[1L]])) { print.bias[[i]] <- as.data.frame(rbind(c("", names(print.bias.temp[[1L]][[1L]])), c("DMean", print.bias.temp$m.diff[[i]]))) }

          }

          # Greek letter Delta
          if (is.null(getOption("knitr.in.progress"))) { for (i in seq_along(print.bias)) { print.bias[[i]][, 1L] <- gsub("D", "\U2206", print.bias[[i]][, 1L]) } }

          for (i in seq_along(print.bias)) {

            # Justify right and left
            print.bias[[i]][, 1L] <- format(print.bias[[i]][, 1L], justify = "left")
            print.bias[[i]][, -1L] <- format(print.bias[[i]][, -1L], justify = "right")

            # Add blank space
            print.bias[[i]][, 1L] <- paste0("   ", print.bias[[i]][, 1L])

            # Column names
            colnames(print.bias[[i]]) <- rep("", times = ncol(print.bias[[i]]))

          }

          ##### Print ####

          if (!isTRUE(x$args$ordered)) { cat("\n  Expected Bias in the Mean and Variance of the Total Score\n") } else { cat("\n  Expected Bias in the Mean of the Total Score\n") }

          for (i in seq_along(print.bias)) {

            if (isTRUE(!x$args$long)) { cat(paste0("\n   Reference Group ", x$args$ref, " vs. ", "Focal Group ", names(print.bias)[i], "\n\n")) } else { cat(paste0("\n   Reference Time Point ", x$args$ref, " vs. ", "Focal Time Point ", names(print.bias)[i], "\n\n")) }

            .write.table(print.bias[[i]], left = 2L, right = 4L, horiz = horiz)

          }

        }

      }

    }

  #_____________________________________________________________________________
  #
  # Coefficient Omega, item.omega() --------------------------------------------
  }, item.omega = {

    # Check input 'print'
    if (isTRUE(check)) { if (isTRUE(!all(print %in% c("all", "omega", "item")))) { stop("Character strings in the argument 'print' do not all match with \"all\", \"omega\", or \"item\".", call. = FALSE) } }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Arguments ####

    # Print coefficient omega and/or item statistic
    if (isTRUE(length(print) == 1L && "all" %in% print)) { print <- c("omega", "item") }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Omega ####

    if (isTRUE("omega" %in% print)) {

      #...................
      ### Format ####

      print.object$omega$n <- format(paste("", print.object$omega$n), justify = "right")

      print.object$omega$items <- format(print.object$omega$items, justify = "right")

      print.object$omega$omega <- formatC(print.object$omega$omega, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

      print.object$omega$low <- formatC(print.object$omega$low, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))
      print.object$omega$upp <- formatC(print.object$omega$upp, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

      # Column names
      print.object$omega <- rbind(c("  n", "nNA", "Items", "Omega", "Low", "Upp"), print.object$omega)

      # Justify right
      print.object$omega <- apply(print.object$omega, 2L, function(y) format(y, justify = "right"))

      #...................
      ### Output ####

      if (isTRUE("omega" %in% print)) { cat("\n") }

      switch(x$args$type, "omega" = {

        cat(paste0(ifelse(isTRUE(x$args$std), " Standardized ", " Unstandardized "), "Coefficient Omega with ", x$args$conf.level*100L, "% Confidence Interval\n\n"))

      }, "hierarch" = {

        cat(paste0(ifelse(isTRUE(x$args$std), " Standardized ", " Unstandardized "), "Hierarchical Omega with ", x$args$conf.level*100L, "% Confidence Interval\n\n"))

      }, "categ" = {

        cat(paste0(" Categorical Omega with ", x$args$conf.level*100L, "% Confidence Interval\n\n"))

      })

      # Print output
      .write.table(print.object$omega, left = 1L, right = 3L, horiz = horiz)

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Item Statistics ####

    if (isTRUE("item" %in% print)) {

      #...................
      ### Format ####

      if (isTRUE(any(print.object$itemstat$pNA != 0L))) { print.object$itemstat$pNA <- paste0(formatC(print.object$itemstat$pNA, digits = 2L, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = 2L), collapse = "")), "0")), "%") }

      print.object$itemstat$m <- formatC(print.object$itemstat$m, digits = 2L, format = "f", zero.print = paste0("0.", paste(rep(0L, times = 2L), collapse = "")))
      print.object$itemstat$sd <- formatC(print.object$itemstat$sd, digits = 2L, format = "f", zero.print = paste0("0.", paste(rep(0L, times = 2L), collapse = "")))
      print.object$itemstat$min <- formatC(print.object$itemstat$min, digits = 2L, format = "f", zero.print = paste0("0.", paste(rep(0L, times = 2L), collapse = "")))
      print.object$itemstat$max <- formatC(print.object$itemstat$max, digits = 2L, format = "f", zero.print = paste0("0.", paste(rep(0L, times = 2L), collapse = "")))

      print.object$itemstat$std.ld <- formatC(print.object$itemstat$std.ld, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))
      print.object$itemstat$omega <- formatC(print.object$itemstat$omega, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

      print.object$itemstat <- rbind(c("Variable", "n", "nNA", "%NA", "M", "SD", "Min", "Max", "Std.Ld", "Omega"), print.object$itemstat)

      # Justify left and right
      print.object$itemstat[, 1L] <- format(paste(" ", print.object$itemstat[, 1L]), justify = "left")
      print.object$itemstat[, -1L] <- apply(print.object$itemstat[, -1L], 2L, function(y) format(y, justify = "right"))

      #...................
      ### Output ####

      if (isTRUE("omega" %in% print)) { cat("\n") }

      switch(x$args$type, "omega" = {

        if (any(!is.na(x$result$itemstat$omega))) { cat(" Standardized Factor Loadings and Coefficient Omega if Item Deleted\n\n") } else { cat(" Standardized Factor Loadings\n\n") }

      }, "hierarch" = {

        if (any(!is.na(x$result$itemstat$omega))) { cat(" Standardized Factor Loadings and Hierarchical Omega if Item Deleted\n\n") } else { cat(" Standardized Factor Loadings\n\n") }

      }, "categ" = {

        if (any(!is.na(x$result$itemstat$omega))) { cat(" Standardized Factor Loadings and Categorical Omega if Item Deleted\n\n") } else { cat(" Standardized Factor Loadings\n\n") }

      })

      # Print output
      if (any(!is.na(x$result$itemstat$omega))) { .write.table(print.object$itemstat, left = 1L, right = 3L, horiz = horiz) } else { .write.table(print.object$itemstat[, colnames(print.object$itemstat) != "omega"], left = 1L, right = 3L, horiz = horiz) }

    }

  #_____________________________________________________________________________
  #
  # Multivariate Kurtosis, kurtosis() ------------------------------------------
  }, kurtosis = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Round ####

    print.object[, c("kurt", "z")] <- sapply(c("kurt", "z"), function(y) ifelse(!is.na(print.object[, y]), formatC(print.object[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

    print.object$pval <- formatC(print.object$pval, digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Row Names ####

    print.object <- rbind(c("n", "Var", "Kurt", "z", "p"), print.object)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Format ####

    # Justify right
    print.object <- apply(print.object, 2L, function(y) format(y, justify = "right"))

    # Add blank space
    print.object[, "n"] <- paste0("  ", print.object[, "n"])

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Print Output ####

    if (isTRUE(x$args$center)) { cat(" Multivariate Excess Kurtosis\n\n") } else { cat(" Multivariate Kurtosis\n\n") }

    .write.table(print.object, left = 1L, right = 3L, horiz = horiz)

  #_____________________________________________________________________________
  #
  # Mplus Object, mplus() ------------------------------------------------------
  }, mplus = {

      cat("Please use the mplus.print function to print a \"mplus\" object.")

  #_____________________________________________________________________________
  #
  # Mplus Summary Measures, Convergence and Efficiency Diagnostics, mplus.bayes()
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
    ## Row Names ####

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
    .write.table(print.object, left = 1L, right = 3L, horiz = horiz)

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
  # Result Table for LPA Estimated in Mplus, mplus.lca.summa() -----------------
  }, mplus.lca.summa = {

    cat(" Latent Class Analysis\n\n")

    #...................
    ### Print object ####

    print.object <- print.object$summary

    #...................
    ### Round ####

    print.object[, c("LL", "aic", "caic", "bic", "sabic", "awe", "occmin")] <- apply(print.object[, c("LL", "aic", "caic", "bic", "sabic", "awe", "occmin")], 2L, function(y) formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")))
    print.object[, "LL.scale"] <- formatC(print.object[, "LL.scale"], digits = p.digits - 1L, format = "f", zero.print = ifelse(p.digits - 1L > 0L, paste0("0.", paste(rep(0L, times = p.digits - -1L), collapse = "")), "0"))

    intersect(c("cmp", "chi.pear", "chi.lrt", "lmr.lrt", "almr.lrt", "blrt", "entropy", "avemin", "pmin"), colnames(print.object)) |>
      (\(p) print.object[, p] <<- apply(print.object[, p, drop = FALSE], 2L, function(y) formatC(y, digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))))()

    print.object[, "nmin"] <- format(round(print.object[, "nmin"], digits = 0L))

    #...................
    ### TRUE/FALSE into Yes/NO ####

    print.object$conv  <- sapply(print.object$conv , function(y) ifelse(isTRUE(y), "Yes", "No"))
    print.object$LL.rep <- sapply(print.object$LL.rep, function(y) ifelse(isTRUE(y), "Yes", "No"))

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

    print.object <- rbind(misty::rec(colnames(print.object), spec = "'folder' = 'Folder'; 'nclass' = '#Class'; 'conv' = 'Conv'; 'nparam' = '#Param'; 'LL' = 'logLik'; 'LL.scale' = 'Scale'; 'LL.rep' = 'LLRep'; 'aic' = 'AIC'; 'caic' = 'CAIC'; 'bic' = 'BIC'; 'sabic' = 'SABIC'; 'awe' = 'AWE'; 'cmp' = 'cmP'; 'lmr.lrt' = 'LMR-LRT'; 'almr.lrt' = 'A-LRT'; 'blrt' = 'BLRT'; 'chi.pear' = 'Chi-Pear'; 'chi.lrt' = 'Chi-LRT'; 'entropy' = 'Entropy'; 'avemin' = 'aPPMin'; 'occmin' = 'OCCMin'; 'nmin' = 'nMin'; 'pmin' = 'pMin'"), print.object)

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

    .write.table(print.object, left = 1L, right = 3L, horiz = horiz)

  #_____________________________________________________________________________
  #
  # Multilevel Confirmatory Factor Analysis, multilevel.cfa() ------------------
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
      print.coverage <- cbind(paste0("   ", colnames(print.coverage)), print.coverage)

      # Column names
      print.coverage <- rbind(colnames(print.coverage), print.coverage)

      # Justify left and right
      print.coverage[, 1L] <- format(print.coverage[, 1L], justify = "left")
      print.coverage[, -1L] <- apply(print.coverage[, -1L], 2L, format, justify = "right")

      #...................
      ### Print ####

      .write.table(print.coverage, left = 2L, right = 4L, horiz = horiz)

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Sample Statistics ####

    if (isTRUE("descript" %in% x$args$print)) {

      cat("\n  Univariate Sample Statistics\n\n")

      print.itemstat <- print.object$descript

      #...................
      ### Format ####

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
      print.itemstat <- rbind(c("Variable", "n", "nNA", "%NA", "M", "SD", "Min", "Max",  "Skew", "Kurt", "ICC(1)"), print.itemstat)

      # Justify left and right
      print.itemstat[, 1L] <- format(print.itemstat[, 1L, drop = FALSE], justify = "left")
      print.itemstat[, -1L] <- apply(print.itemstat[, -1L, drop = FALSE], 2L, function(y) format(y, justify = "right"))

      # Add blank space
      print.itemstat[, "variable"] <- c(paste0("   ", print.itemstat[1L, "variable"], collapse = ""), paste0("    ", print.itemstat[-1L, "variable"]))
      print.itemstat[, "variable"] <- format(c(print.itemstat[1L, "variable"], misty::chr.trim(print.itemstat[-1L, "variable"], side = "right")), justify = "left")

      #...................
      ### Print ####

      .write.table(print.itemstat, left = 2L, right = 4L, horiz = horiz)

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
          print.fit[-c(17L:19L, 41L:43L), -1L] <- sapply(print.fit[-c(17L:19L, 41L:43L), -1], function(y) ifelse(!is.na(y), formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

          # p.digits
          print.fit[c(17L:19L, 41L:43L), -1L] <- sapply(print.fit[c(17L:19L, 41L:43L), -1L], function(y) ifelse(!is.na(y), formatC(as.numeric(y), digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0")), NA))

        #### Robust maximum likelihood
        } else {

          # digits
          print.fit[-c(19L:21L, 46L:48L), -1L] <- sapply(print.fit[-c(19L:21L, 46L:48L), -1L], function(y) ifelse(!is.na(y), formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

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

      }

      #...................
      ### Print ####

      write.table(print.fit, quote = FALSE, row.names = FALSE, col.names = FALSE)

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Parameter Estimates ####

    if (isTRUE("est" %in% x$args$print)) {

      #...................
      ### Within Level ####

      cat("\n  Model Results: Within Level\n\n")

      print.param.w <- print.object$param$within

      #### Round ####

      # digits
      print.param.w[, -c(1L:4L, 8L)] <- lapply(print.param.w[, -c(1L:4L, 8L)], function(y) ifelse(!is.na(y), formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

      # p.digits
      print.param.w[, "pvalue"] <- formatC(as.numeric(print.param.w[, "pvalue"]), digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

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

        print.lv <- print.param.w[print.param.w$param == "latent variable", ]

        # Heading
        write.table(print.param.w[1L, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

        # Horizontal line
        cat(paste(rep(" ", times = 2), collapse = ""), paste(rep("\u2500", times = sum(nchar(print.param.w[1L, 4L:9L])) + length(print.param.w[1L, 4L:9L]) - 4), collapse = ""), "\n")

        cat("   Factor Loadings\n")

        for (i in unique(print.lv$lhs)) {

          pos.NA <- grep("NA", print.lv[print.lv$lhs == i, "z"])

          if (isTRUE(length(pos.NA) > 0L)) {

            print.lv[print.lv$lhs == i, ][pos.NA, "se"] <- paste(rep("", times = unique(nchar(print.lv[print.lv$lhs == i, ][pos.NA, "se"])) + 1L), collapse = " ")
            print.lv[print.lv$lhs == i, ][pos.NA, "z"] <- paste(rep("", times = unique(nchar(print.lv[print.lv$lhs == i, ][pos.NA, "z"])) + 1L), collapse = " ")
            print.lv[print.lv$lhs == i, ][pos.NA, "pvalue"] <- paste(rep("", times = unique(nchar(print.lv[print.lv$lhs == i, ][pos.NA, "pvalue"])) + 1L), collapse = " ")

          }

          write.table(print.lv[print.lv$lhs == i, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

        }

      ##### Shared construct
      } else {

        # Heading
        write.table(print.param.w[1L, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

        # Horizontal line
        cat(paste(rep(" ", times = 2), collapse = ""), paste(rep("\u2500", times = sum(nchar(print.param.w[1L, 4L:9L])) + length(print.param.w[1L, 4L:9L]) - 4), collapse = ""), "\n")

      }

      ##### Latent variable covariances
      if (isTRUE(any(print.param.w$param %in% "latent variable covariance"))) {

        print.lv.cov <- print.param.w[print.param.w$param == "latent variable covariance", ]

        if (isTRUE(length(unique(print.lv.cov$lhs)) == 1L)) { cat("\n   Latent Variable Covariance\n") } else { cat("\n   Latent Variable Covariances\n") }

        for (i in unique(print.lv.cov$lhs)) { write.table(print.lv.cov[print.lv.cov$lhs == i, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE) }

      }

      ##### Residual covariances
      if (isTRUE(any(print.param.w$param %in% "residual covariance"))) {

        print.res.cov <- print.param.w[print.param.w$param == "residual covariance", ]

        # Heading latent variables
        if (isTRUE(any(print.param.w$param %in% "latent variable"))) {

          if (isTRUE(length(unique(print.res.cov$lhs)) == 1L)) { cat("\n   Residual Covariance\n") } else { cat("\n   Residual Covariances\n") }

        # Heading no latent variables
        } else {

          if (isTRUE(any(print.param.w$param %in% "latent variable"))) { cat("\n") }

          if (isTRUE(length(unique(print.res.cov$lhs)) == 1L)) { cat("   Covariance\n") } else { cat("   Covariances\n") }

        }

        for (i in unique(print.res.cov$lhs)) { write.table(print.res.cov[print.res.cov$lhs == i, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE) }

      }

      ##### Latent variance
      if (isTRUE(any(print.param.w$param %in% "latent variance"))) {

        print.var <- print.param.w[print.param.w$param == "latent variance", ]

        print.var[grep("NA", print.var[, "z"]), c("se", "z", "pvalue", "stdyx")] <- ""

        if (isTRUE(nrow(print.var) == 1L)) { cat("\n   Latent Variance\n") } else { cat("\n   Latent Variances\n") }

        write.table(print.var[, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

      }

      ##### Residual variance
      if (isTRUE(any(print.param.w$param %in% "residual variance"))) {

        print.resid <- print.param.w[print.param.w$param == "residual variance", ]

        pos.NA <- grep("NA", print.resid[, "z"])


        if (isTRUE(length(pos.NA) >  0L)) {

          print.resid[pos.NA, "se"] <- paste(rep("", times = unique(nchar(print.resid[pos.NA, "se"])) + 1L), collapse = " ")
          print.resid[pos.NA, "z"] <- paste(rep("", times = unique(nchar(print.resid[pos.NA, "z"])) + 1L), collapse = " ")
          print.resid[pos.NA, "pvalue"] <- paste(rep("", times = unique(nchar(print.resid[pos.NA, "pvalue"])) + 1L), collapse = " ")

        }

        # Heading latent variables
        if (isTRUE(any(print.param.w$param %in% "latent variable"))) {

          if (isTRUE(nrow(print.resid) == 1L)) { cat("\n   Residual Variance\n") } else { cat("\n   Residual Variances\n") }

        } else {

          if (isTRUE(nrow(print.resid) == 1L)) { cat("\n   Variance\n") } else { cat("\n   Variances\n") }

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
      print.param.b[, -c(1L:4L, 8L)] <- lapply(print.param.b[, -c(1L:4L, 8L)], function(y) ifelse(!is.na(y), formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

      # p.digits
      print.param.b[, "pvalue"] <- formatC(as.numeric(print.param.b[, "pvalue"]), digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

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

        print.lv <- print.param.b[print.param.b$param == "latent variable", ]

        # Heading
        write.table(print.param.b[1L, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

        # Horizontal line
        cat(paste(rep(" ", times = 2), collapse = ""), paste(rep("\u2500", times = sum(nchar(print.param.b[1L, 4L:9L])) + length(print.param.b[1L, 4L:9L]) - 4), collapse = ""), "\n")

        cat("   Factor Loadings\n")

        for (i in unique(print.lv$lhs)) {

          pos.NA <- grep("NA", print.lv[print.lv$lhs == i, "z"])

          if (isTRUE(length(pos.NA) > 0L)) {

            print.lv[print.lv$lhs == i, ][pos.NA, "se"] <- paste(rep("", times = unique(nchar(print.lv[print.lv$lhs == i, ][pos.NA, "se"])) + 1L), collapse = " ")
            print.lv[print.lv$lhs == i, ][pos.NA, "z"] <- paste(rep("", times = unique(nchar(print.lv[print.lv$lhs == i, ][pos.NA, "z"])) + 1L), collapse = " ")
            print.lv[print.lv$lhs == i, ][pos.NA, "pvalue"] <- paste(rep("", times = unique(nchar(print.lv[print.lv$lhs == i, ][pos.NA, "pvalue"])) + 1L), collapse = " ")

          }

          write.table(print.lv[print.lv$lhs == i, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

        }

      # Within construct
      } else {

        # Heading
        write.table(print.param.b[1L, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

        # Horizontal line
        cat(paste(rep(" ", times = 2), collapse = ""), paste(rep("\u2500", times = sum(nchar(print.param.b[1L, 4L:9L])) + length(print.param.b[1L, 4L:9L]) - 4), collapse = ""), "\n")

      }

      ##### Latent variable covariances
      if (isTRUE(any(print.param.b$param %in% "latent variable covariance"))) {

        print.lv.cov <- print.param.b[print.param.b$param == "latent variable covariance", ]

        if (isTRUE(length(unique(print.lv.cov$lhs)) == 1L)) { cat("\n   Latent Variable Covariance\n") } else { cat("\n   Latent Variable Covariances\n") }

        for (i in unique(print.lv.cov$lhs)) { write.table(print.lv.cov[print.lv.cov$lhs == i, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE) }

      }

      ##### (Residual) Covariances
      if (isTRUE(any(print.param.b$param %in% "residual covariance"))) {

        print.res.cov <- print.param.b[print.param.b$param == "residual covariance", ]

        # Heading latent variables
        if (isTRUE(any(print.param.b$param %in% "latent variable"))) {

          if (isTRUE(length(unique(print.res.cov$lhs)) == 1L)) { cat("\n   Residual Covariance\n") } else { cat("\n   Residual Covariances\n") }

        # Heading no latent variables
        } else {

          if (isTRUE(any(print.param.b$param %in% "latent variable"))) { cat("\n") }

          if (isTRUE(length(unique(print.res.cov$lhs)) == 1L)) { cat("   Covariance\n") } else { cat("   Covariances\n") }

        }

        for (i in unique(print.res.cov$lhs)) { write.table(print.res.cov[print.res.cov$lhs == i, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE) }

      }

      ##### Latent mean
      if (isTRUE(any(print.param.b$param %in% "latent mean"))) {

        print.mean <- print.param.b[print.param.b$param == "latent mean", ]

        print.mean[grep("NA", print.mean[, "z"]), c("se", "z", "pvalue", "stdyx")] <- ""

        if (isTRUE(nrow(print.mean) == 1L)) { cat("\n   Latent Mean\n") } else { cat("\n   Latent Means\n") }

        write.table(print.mean[, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

      }

      ##### Latent variance
      if (isTRUE(any(print.param.b$param %in% "latent variance"))) {

        print.var <- print.param.b[print.param.b$param == "latent variance", ]

        print.var[grep("NA", print.var[, "z"]), c("se", "z", "pvalue", "stdyx")] <- ""

        if (isTRUE(nrow(print.var) == 1L)) { cat("\n   Latent Variance\n") } else { cat("\n   Latent Variances\n") }

        write.table(print.var[, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

      }

      ##### Intercepts
      if (isTRUE(any(print.param.b$param %in% "intercept"))) {

        print.inter <- print.param.b[print.param.b$param == "intercept", ]

        print.inter[grep("NA", print.inter[, "z"]), c("se", "z", "pvalue", "stdyx")] <- ""

        if (isTRUE(nrow(print.inter) == 1L)) { cat("\n   Intercept\n") } else { cat("\n   Intercepts\n") }

        write.table(print.inter[, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

      }

      ##### (Residual) Variance
      if (isTRUE(any(print.param.b$param %in% "residual variance"))) {

        print.resid <- print.param.b[print.param.b$param == "residual variance", ]

        pos.NA <- grep("NA", print.resid[, "z"])

        if (isTRUE(length(pos.NA) >  0L)) {

          print.resid[pos.NA, "se"] <- paste(rep("", times = unique(nchar(print.resid[pos.NA, "se"])) + 1L), collapse = " ")
          print.resid[pos.NA, "z"] <- paste(rep("", times = unique(nchar(print.resid[pos.NA, "z"])) + 1L), collapse = " ")
          print.resid[pos.NA, "pvalue"] <- paste(rep("", times = unique(nchar(print.resid[pos.NA, "pvalue"])) + 1L), collapse = " ")

        }

        # Heading latent variables
        if (isTRUE(any(print.param.b$param %in% "latent variable"))) {

          if (isTRUE(nrow(print.resid) == 1L)) { cat("\n   Residual Variance\n") } else { cat("\n   Residual Variances\n") }

        } else {

          if (isTRUE(nrow(print.resid) == 1L)) { cat("\n   Variance\n") } else { cat("\n   Variances\n") }

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

            # Horizontal line
            cat(paste(rep(" ", times = 3), collapse = ""), paste(rep("\u2500", times = sum(nchar(print.modind.w[1L, ])) + length(print.modind.w[1L, ]) - 5), collapse = ""), "\n")

            write.table(print.modind.w.load, quote = FALSE, row.names = FALSE, col.names = FALSE)

          }

          # Residual covariances
          print.modind.w.cov <- print.modind.w[print.modind.w$op == "~~", ]

          if (isTRUE(nrow(print.modind.w.cov) > 0L)) {

            cat("\n   Residual Covariances\n")

            # Print header
            write.table(print.modind.w[1L, ], quote = FALSE, row.names = FALSE, col.names = FALSE)

            # Horizontal line
            cat(paste(rep(" ", times = 3), collapse = ""), paste(rep("\u2500", times = sum(nchar(print.modind.w[1L, ])) + length(print.modind.w[1L, ]) - 5), collapse = ""), "\n")

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
          print.modind.b[, -c(1L:3L)] <- lapply(print.modind.b[, -c(1L:3L)], function(y) ifelse(!is.na(y), formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

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

            # Horizontal line
            cat(paste(rep(" ", times = 3), collapse = ""), paste(rep("\u2500", times = sum(nchar(print.modind.b[1L, ],)) + length(print.modind.b[1L, ]) - 5), collapse = ""), "\n")

            write.table(print.modind.b.load, quote = FALSE, row.names = FALSE, col.names = FALSE)

          }

          # Residual covariances
          print.modind.b.cov <- print.modind.b[print.modind.b$op == "~~", ]

          if (isTRUE(nrow(print.modind.b.cov) > 0L)) {

            cat("\n   Residual Covariances\n")

            # Print header
            write.table(print.modind.b[1L, ], quote = FALSE, row.names = FALSE, col.names = FALSE)

            # Horizontal line
            cat(paste(rep(" ", times = 3), collapse = ""), paste(rep("\u2500", times = sum(nchar(print.modind.b[1L, ],)) + length(print.modind.b[1L, ]) - 5), collapse = ""), "\n")

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

          .write.table(print.score, left = 2L, right = 4L, horiz = horiz)

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
        print.resid <- lapply(print.resid, function(y) ifelse(!is.na(y), formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

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

        .write.table(print.resid$within, left = 2L, right = 4L, horiz = horiz)

        cat("\n   Between Level\n")

        .write.table(print.resid$between, left = 2L, right = 4L, horiz = horiz)

      }

    }

  #_____________________________________________________________________________
  #
  # Within-Group and Between-Group Correlation Matrix, multilevel.cor() --------
  }, multilevel.cor = {

    # Check input 'print'
    if (isTRUE(check)) { if (isTRUE(any(!print %in% c("all", "cor", "se", "stat", "p")))) { stop("Character string(s) in the argument 'print' does not match with \"all\", \"cor\", \"se\", \"stat\", or \"p\".", call. = FALSE) } }

    # R Markdown in progress
    if (isTRUE(getOption("knitr.in.progress"))) { x$args$sig <- FALSE }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Split Results####

    if (isTRUE(x$args$split)) {

      #...................
      ### Model Estimation with SE ####

      if (isTRUE(x$args$se != "none")) {

        #### Round ####

        # Round and format Within
        print.object$with.cor <- formatC(print.object$with.cor, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))
        print.object$with.se <- formatC(print.object$with.se, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))
        print.object$with.stat <- formatC(print.object$with.stat, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))
        print.object$with.p <- formatC(print.object$with.p, digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

        # Round and format Between
        print.object$betw.cor <- formatC(print.object$betw.cor, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))
        print.object$betw.se <- formatC(print.object$betw.se, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))
        print.object$betw.stat <- formatC(print.object$betw.stat, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))
        print.object$betw.p <- formatC(print.object$betw.p, digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

        diag(print.object$with.cor) <- ""
        diag(print.object$with.se) <- ""
        diag(print.object$with.stat) <- ""
        diag(print.object$with.p) <- ""

        diag(print.object$betw.cor) <- ""
        diag(print.object$betw.se) <- ""
        diag(print.object$betw.stat) <- ""
        diag(print.object$betw.p) <- ""

        #### Lower and/or Upper Triangular ####

        switch(tri, "lower" = {

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

        }, "upper" = {

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

        })

        #### Row Names ####

        # Row names Within
        print.object$with.cor <- cbind(paste0("   ", row.names(print.object$with.cor)), print.object$with.cor)
        print.object$with.se <- cbind(paste0("   ", row.names(print.object$with.se)), print.object$with.se)
        print.object$with.stat <- cbind(paste0("   ", row.names(print.object$with.stat)), print.object$with.stat)
        print.object$with.p <- cbind(paste0("   ", row.names(print.object$with.p)), print.object$with.p)

        # Row names Between
        print.object$betw.cor <- cbind(paste0("   ", row.names(print.object$betw.cor)), print.object$betw.cor)
        print.object$betw.se <- cbind(paste0("   ", row.names(print.object$betw.se)), print.object$betw.se)
        print.object$betw.stat <- cbind(paste0("   ", row.names(print.object$betw.stat)), print.object$betw.stat)
        print.object$betw.p <- cbind(paste0("   ", row.names(print.object$betw.p)), print.object$betw.p)

        #### Column Names ####

        # Column names Within
        print.object$with.cor <- rbind(colnames(print.object$with.cor), print.object$with.cor)
        print.object$with.se <- rbind(colnames(print.object$with.se), print.object$with.se)
        print.object$with.stat <- rbind(colnames(print.object$with.stat), print.object$with.stat)
        print.object$with.p <- rbind(colnames(print.object$with.p), print.object$with.p)

        # Column names Between
        print.object$betw.cor <- rbind(colnames(print.object$betw.cor), print.object$betw.cor)
        print.object$betw.se <- rbind(colnames(print.object$betw.se), print.object$betw.se)
        print.object$betw.stat <- rbind(colnames(print.object$betw.stat), print.object$betw.stat)
        print.object$betw.p <- rbind(colnames(print.object$betw.p), print.object$betw.p)

        #### Justify Left and Right ####

        # Within
        print.object$with.cor[, 1L] <- format(print.object$with.cor[, 1L], justify = "left")
        print.object$with.cor[, -1L] <- apply(print.object$with.cor[, -1L], 2L, format, justify = "right")

        print.object$with.se[, 1L] <- format(print.object$with.se[, 1L], justify = "left")
        print.object$with.se[, -1L] <- apply(print.object$with.se[, -1L], 2L, format, justify = "right")

        print.object$with.stat[, 1L] <- format(print.object$with.stat[, 1L], justify = "left")
        print.object$with.stat[, -1L] <- apply(print.object$with.stat[, -1L], 2L, format, justify = "right")

        print.object$with.p[, 1L] <- format(print.object$with.p[, 1L], justify = "left")
        print.object$with.p[, -1L] <- apply(print.object$with.p[, -1L], 2L, format, justify = "right")

        # Between
        print.object$betw.cor[, 1L] <- format(print.object$betw.cor[, 1L], justify = "left")
        print.object$betw.cor[, -1L] <- apply(print.object$betw.cor[, -1L], 2L, format, justify = "right")

        print.object$betw.se[, 1L] <- format(print.object$betw.se[, 1L], justify = "left")
        print.object$betw.se[, -1L] <- apply(print.object$betw.se[, -1L], 2L, format, justify = "right")

        print.object$betw.stat[, 1L] <- format(print.object$betw.stat[, 1L], justify = "left")
        print.object$betw.stat[, -1L] <- apply(print.object$betw.stat[, -1L], 2L, format, justify = "right")

        print.object$betw.p[, 1L] <- format(print.object$betw.p[, 1L], justify = "left")
        print.object$betw.p[, -1L] <- apply(print.object$betw.p[, -1L], 2L, format, justify = "right")

        #### Statistically Significant Correlation Coefficients in Boldface ####

        if (isTRUE(x$args$sig)) {

          numbers <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
          unicode <- c("\U1D7CE", "\U1D7CF", "\U1D7D0", "\U1D7EF", "\U1D7F0", "\U1D7F1", "\U1D7F2", "\U1D7F3", "\U1D7F4", "\U1D7F5")

          print.object$with.cor[-1, -1L][lower.tri(print.object$with.cor[-1, -1L])][which(x$result$with.p[lower.tri(x$result$with.p)] <= x$args$alpha)] <- na.omit(misty::chr.gsub(numbers, unicode, print.object$with.cor[-1, -1L][lower.tri(print.object$with.cor[-1, -1L])][which(x$result$with.p[lower.tri(x$result$with.p)] <= x$args$alpha)]))
          print.object$with.cor[-1, -1L][upper.tri(print.object$with.cor[-1, -1L])][which(x$result$with.p[upper.tri(x$result$with.p)] <= x$args$alpha)] <- na.omit(misty::chr.gsub(numbers, unicode, print.object$with.cor[-1, -1L][upper.tri(print.object$with.cor[-1, -1L])][which(x$result$with.p[upper.tri(x$result$with.p)] <= x$args$alpha)]))

          print.object$betw.cor[-1, -1L][lower.tri(print.object$betw.cor[-1, -1L])][which(x$result$betw.p[lower.tri(x$result$betw.p)] <= x$args$alpha)] <- na.omit(misty::chr.gsub(numbers, unicode, print.object$betw.cor[-1, -1L][lower.tri(print.object$betw.cor[-1, -1L])][which(x$result$betw.p[lower.tri(x$result$betw.p)] <= x$args$alpha)]))
          print.object$betw.cor[-1, -1L][upper.tri(print.object$betw.cor[-1, -1L])][which(x$result$betw.p[upper.tri(x$result$betw.p)] <= x$args$alpha)] <- na.omit(misty::chr.gsub(numbers, unicode, print.object$betw.cor[-1, -1L][upper.tri(print.object$betw.cor[-1, -1L])][which(x$result$betw.p[upper.tri(x$result$betw.p)] <= x$args$alpha)]))

        }

      #...................
      ### Model Estimation without SE ####

      } else {

        #### Round ####

        # Round and format Within
        print.object$with.cor <- formatC(print.object$with.cor, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

        diag(print.object$with.cor) <- ""

        # Round and format Between
        print.object$betw.cor <- formatC(print.object$betw.cor, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

        diag(print.object$betw.cor) <- ""

        #### Lower and/or Upper Triangular ####

        switch(tri, "lower" = {

          # Within
          print.object$with.cor[upper.tri(print.object$with.cor)] <- ""

          # Between
          print.object$betw.cor[upper.tri(print.object$betw.cor)] <- ""

        }, "upper" = {

          # Within
          print.object$with.cor[lower.tri(print.object$with.cor)] <- ""

          # Between
          print.object$betw.cor[lower.tri(print.object$betw.cor)] <- ""

        })

        #### Row Names ####

        print.object$with.cor <- cbind(paste0("   ", row.names(print.object$with.cor)), print.object$with.cor)
        print.object$betw.cor <- cbind(paste0("   ", row.names(print.object$betw.cor)), print.object$betw.cor)

        #### Column Names ####

        print.object$with.cor <- rbind(colnames(print.object$with.cor), print.object$with.cor)
        print.object$betw.cor <- rbind(colnames(print.object$betw.cor), print.object$betw.cor)

        #### Justify Left and Right ####

        print.object$with.cor[, 1L] <- format(print.object$with.cor[, 1L], justify = "left")
        print.object$with.cor[, -1L] <- apply(print.object$with.cor[, -1L], 2L, format, justify = "right")

        print.object$betw.cor[, 1L] <- format(print.object$betw.cor[, 1L], justify = "left")
        print.object$betw.cor[, -1L] <- apply(print.object$betw.cor[, -1L], 2L, format, justify = "right")

      }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Combined Results####

    } else {

      #...................
      ### Model Estimation with SE ####

      if (isTRUE(x$args$se != "none")) {

        #### Round and Format ####

        print.object$wb.cor <- formatC(print.object$wb.cor, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))
        print.object$wb.se <- formatC(print.object$wb.se, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))
        print.object$wb.stat <- formatC(print.object$wb.stat, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))
        print.object$wb.p <- formatC(print.object$wb.p, digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

        diag(print.object$wb.cor) <- ""
        diag(print.object$wb.se) <- ""
        diag(print.object$wb.stat) <- ""
        diag(print.object$wb.p) <- ""

        #### Missing Entries ####

        print.object$wb.cor <- gsub("NA", "  ", print.object$wb.cor)
        print.object$wb.se <- gsub("NA", "  ", print.object$wb.se)
        print.object$wb.stat <- gsub("NA", "  ", print.object$wb.stat)
        print.object$wb.p <- gsub("NA", "  ", print.object$wb.p)

        #### Row Names ####

        print.object$wb.cor <- cbind(paste0("   ", row.names(print.object$wb.cor)), print.object$wb.cor)
        print.object$wb.se <- cbind(paste0("   ", row.names(print.object$wb.se)), print.object$wb.se)
        print.object$wb.stat <- cbind(paste0("   ", row.names(print.object$wb.stat)), print.object$wb.stat)
        print.object$wb.p <- cbind(paste0("   ", row.names(print.object$wb.p)), print.object$wb.p)

        #### Column Names ####

        print.object$wb.cor <- rbind(colnames(print.object$wb.cor), print.object$wb.cor)
        print.object$wb.se <- rbind(colnames(print.object$wb.se), print.object$wb.se)
        print.object$wb.stat <- rbind(colnames(print.object$wb.stat), print.object$wb.stat)
        print.object$wb.p <- rbind(colnames(print.object$wb.p), print.object$wb.p)

        #### Justify Left and Right ####

        print.object$wb.cor[, 1L] <- format(print.object$wb.cor[, 1L], justify = "left")
        print.object$wb.cor[, -1L] <- apply(print.object$wb.cor[, -1L], 2L, format, justify = "right")

        print.object$wb.se[, 1L] <- format(print.object$wb.se[, 1L], justify = "left")
        print.object$wb.se[, -1L] <- apply(print.object$wb.se[, -1L], 2L, format, justify = "right")

        print.object$wb.stat[, 1L] <- format(print.object$wb.stat[, 1L], justify = "left")
        print.object$wb.stat[, -1L] <- apply(print.object$wb.stat[, -1L], 2L, format, justify = "right")

        print.object$wb.p[, 1L] <- format(print.object$wb.p[, 1L], justify = "left")
        print.object$wb.p[, -1L] <- apply(print.object$wb.p[, -1L], 2L, format, justify = "right")

        #### Statistically Significant Correlation Coefficients in Boldface ####

        if (isTRUE(x$args$sig)) {

          numbers <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
          unicode <- c("\U1D7CE", "\U1D7CF", "\U1D7D0", "\U1D7EF", "\U1D7F0", "\U1D7F1", "\U1D7F2", "\U1D7F3", "\U1D7F4", "\U1D7F5")

          print.object$wb.cor[-1L, -1L][lower.tri(print.object$wb.cor[-1L, -1L])][which(x$result$wb.p[lower.tri(x$result$wb.p)] <= x$args$alpha)] <- na.omit(misty::chr.gsub(numbers, unicode, print.object$wb.cor[-1L, -1L][lower.tri(print.object$wb.cor[-1L, -1L])][which(x$result$wb.p[lower.tri(x$result$wb.p)] <= x$args$alpha)]))
          print.object$wb.cor[-1L, -1L][upper.tri(print.object$wb.cor[-1L, -1L])][which(x$result$wb.p[upper.tri(x$result$wb.p)] <= x$args$alpha)] <- na.omit(misty::chr.gsub(numbers, unicode, print.object$wb.cor[-1L, -1L][upper.tri(print.object$wb.cor[-1L, -1L])][which(x$result$wb.p[upper.tri(x$result$wb.p)] <= x$args$alpha)]))

        }

      #...................
      ### Model Estimation without SE ####

      } else {

        #### Round and Format ####

        print.object$wb.cor <- formatC(print.object$wb.cor, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

        diag(print.object$wb.cor) <- ""

        #### Missing Entries ####

        print.object$wb.cor <- gsub("NA", "  ", print.object$wb.cor)

        #### Row and Column Names ####

        # Row names
        print.object$wb.cor <- cbind(paste0("   ", row.names(print.object$wb.cor)), print.object$wb.cor)

        # Column names
        print.object$wb.cor <- rbind(colnames(print.object$wb.cor), print.object$wb.cor)

        #### Justify Left and Right ####

        print.object$wb.cor[, 1L] <- format(print.object$wb.cor[, 1L], justify = "left")
        print.object$wb.cor[, -1L] <- apply(print.object$wb.cor[, -1L], 2L, format, justify = "right")

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Output ####

    cat(" Within-Group and Between-Group Correlation Matrix\n")

    #...................
    ### lavaan summary ####

    print.summary <- print.object$summary

    #...................
    ### Format ####

    # Include spaces
    print.summary[1L, 1L] <- paste0(" ", print.summary[1L, 1L])
    print.summary[-1L, 1L] <- paste0("  ", print.summary[-1L, 1L])

    # Justify left
    print.summary[, 1L] <- format(print.summary[, 1L], justify = "left")

    #...................
    ### Print ####

    print(print.summary, col.names = FALSE, row.names = FALSE, quote = FALSE, right = TRUE, max = 99999L)

    cat("\n")

    #...................
    ### Split Results ####

    if (isTRUE(x$args$split)) {

      #### Correlation Coefficient ####

      if (isTRUE("cor" %in% print)) {

        # Within
        cat("  Within-Group\n")

        .write.table(print.object$with.cor, left = 2L, right = 4L, horiz = horiz)

        # Between
        cat("\n  Between-Group\n")

        .write.table(print.object$betw.cor, left = 2L, right = 4L, horiz = horiz)

      }

      #### Model estimation with SE ####

      if (isTRUE(x$args$se != "none")) {

        ##### Standard error
        if (isTRUE("se" %in% print)) {

          if (isTRUE("cor" %in% print)) { cat("\n") }

          cat(" Standard error \n\n")

          # Within
          cat("  Within-Group\n")

          .write.table(print.object$with.se, left = 2L, right = 4L, horiz = horiz)

          # Between
          cat("\n  Between-Group\n")

          .write.table(print.object$betw.se, left = 2L, right = 4L, horiz = horiz)

        }

        ##### Test statistic
        if (isTRUE("stat" %in% print)) {

          if (isTRUE(any(c("cor", "se") %in% print))) { cat("\n") }

          cat(" Test Statistic (z value) \n\n")

          # Within
          cat("  Within-Group\n")

          .write.table(print.object$with.stat, left = 2L, right = 4L, horiz = horiz)

          # Between
          cat("\n  Between-Group\n")

          .write.table(print.object$betw.stat, left = 2L, right = 4L, horiz = horiz)

        }

        ##### p.values
        if (isTRUE("p" %in% print)) {

          if (isTRUE(any(c("cor", "se", "stat") %in% print))) { cat("\n") }

          cat(" Significance Value (p-value)\n\n")

          # Within
          cat("  Within-Group\n")

          .write.table(print.object$with.p, left = 2L, right = 4L, horiz = horiz)

          # Between
          cat("\n  Between-Group\n")

          .write.table(print.object$betw.p, left = 2L, right = 4L, horiz = horiz)

          cat(paste0("\n  Adjustment for multiple testing: ", x$args$p.adj, "\n"))

        }

      #### Model estimation without SE ####

      } else if (isTRUE(any(c("se", "stat", "p") %in% print))) {

        if (isTRUE("cor" %in% print)) { cat("\n") }

        cat(" Standard Error, Test Statistic, and Significance Value\n\n")

        cat("  No output available, model was estimated without standard errors.\n")

      }

    #...................
    ### Combined Results ####

    } else {

      #### Correlation coefficient
      if (isTRUE("cor" %in% print)) { .write.table(print.object$wb.cor, left = 2L, right = 4L, horiz = horiz) }

      #### Model estimation with SE ####
      if (isTRUE(x$args$se != "none")) {

        ##### Standard error
        if (isTRUE("se" %in% print)) {

          if (isTRUE("cor" %in% print)) { cat("\n") }

          cat("  Standard Error\n\n")

          .write.table(print.object$wb.se, left = 2L, right = 4L, horiz = horiz)

        }

        ##### Test statistic
        if (isTRUE("stat" %in% print)) {

          if (isTRUE(any(c("cor", "se") %in% print))) { cat("\n") }

          cat("  Test Statistic (z value) \n\n")

          .write.table(print.object$wb.stat, left = 2L, right = 4L, horiz = horiz)

        }

        ##### p.values
        if (isTRUE("p" %in% print)) {

          if (isTRUE(any(c("cor", "se", "stat") %in% print))) { cat("\n") }

          cat("  Significance Value (p-value)\n\n")

          .write.table(print.object$wb.p, left = 2L, right = 4L, horiz = horiz)

          cat(paste0("\n  Adjustment for multiple testing: ", x$args$p.adj, "\n"))

        }

      #### Model estimation without SE ####

      } else if (isTRUE(any(c("se", "stat", "p") %in% print))) {

        if (isTRUE("cor" %in% print)) { cat("\n") }

        cat(" Standard Error, Test Statistic, and Significance Value\n\n")

        cat("  No output available, model was estimated without standard errors.\n")

      }

    }

    #...................
    ### Note ####

    # Sample size within and between
    cat(paste0("\n Note. n(within) = ", lavaan::lavInspect(x$model.fit, what = "nobs"), ", n(between) = ", lavaan::lavInspect(x$model.fit, what = "nclusters")), "\n")

    # Lower and upper triangular
    if (isTRUE(!x$args$split)) { cat(ifelse(isTRUE(x$args$tri.lower), "       Lower triangular: Within-Group, Upper triangular: Between-Group", "       Lower triangular: Between-Group, Upper triangular: Within-Group"), "\n") }

    # Statistical significance
    if (isTRUE(x$args$sig)) { cat(paste0("       Statistically significant coefficients at \U03B1 = ", signif(x$args$alpha, digits = 2L), " are boldface\n")) }

  #_____________________________________________________________________________
  #
  #  Multilevel Descriptive Statistics, multilevel.descript() ------------------
  }, multilevel.descript = {

    print.object <- data.frame(c("Level 1", "No. of cases", "No. of missing values", "", "Variance Within", "SD Within", "",
                                 "Level 2", "No. of clusters", "Average cluster size", "SD cluster size", "Min cluster size", "Max cluster size", "", "Mean", "Variance Between", "SD Between", "ICC(1)", "ICC(2)", "",
                                 "Level 3", "No. of clusters", "Average cluster size", "SD cluster size", "Min cluster size", "Max cluster size", "", "Mean", "Variance Between", "SD Between", "ICC(1)", "ICC(2)", "",
                                 "Design effect", "Design effect sqrt", "Effective sample size"),
                               rbind(NA, print.object$no.obs, print.object$no.miss, NA, print.object$var.r, print.object$sd.r, NA,
                                     NA, print.object$no.cluster.l2, print.object$m.cluster.size.l2, print.object$sd.cluster.size.l2, print.object$min.cluster.size.l2, print.object$max.cluster.size.l2, NA, print.object$mean.x, print.object$var.u, print.object$sd.u, print.object$icc1.l2, print.object$icc2.l2, NA,
                                     NA, print.object$no.cluster.l3, print.object$m.cluster.size.l3, print.object$sd.cluster.size.l3, print.object$min.cluster.size.l3, print.object$max.cluster.size.l3, NA, print.object$mean.x, print.object$var.v, print.object$sd.v, print.object$icc1.l3, print.object$icc2.l3, NA,
                                     print.object$deff, print.object$deff.sqrt, print.object$n.effect), fix.empty.names = FALSE, stringsAsFactors = FALSE)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Format ####

    #...................
    ### Variable Names ####

    print.object <- rbind(c("", names(x$result$no.obs)), print.object)

    #...................
    ### Round ####

    for (i in c(6L:7L, 11L:12L, 16L:18L, 24L:25L, 29L:31L, 35L:37L)) { print.object[i, 2L:ncol(print.object)] <- formatC(as.numeric(unlist(print.object[i, 2L:ncol(print.object)])), digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")) }

    for (i in c(19L:20L, 32L:33L)) { print.object[i, 2L:ncol(print.object)] <- formatC(as.numeric(unlist(print.object[i, 2L:ncol(print.object)])), digits = icc.digits, format = "f", zero.print = ifelse(icc.digits > 0L, paste0("0.", paste(rep(0L, times = icc.digits), collapse = "")), "0")) }

    #...................
    ### Blanks ####

    print.object[, 1L] <- paste(" ", print.object[, 1L])

    print.object[c(3L:8, 10L:21L, 23L:34L), 1L] <- paste("", print.object[c(3L:8, 10L:21L, 23L:34L), 1L])

    print.object[, 1L] <- format(print.object[, 1L, drop = FALSE])

    print.object[, 1L] <- format(unlist(print.object[, 1L]), justify = "left")
    print.object[, -1L] <- sapply(print.object[, -1L, drop = FALSE], function(y) format(as.character(y), justify = "right"))

    #...................
    ### NAs ####

    print.object[, -1L] <- sapply(print.object[, -1L], function(y) gsub("NA", "  ", y))

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Number of Clusters ####

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

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Select Rows ####

    # Variance and/or SD
    if (isTRUE(!"var" %in% x$args$print)) { print.object <- print.object[-grep("Variance", print.object[, 1L]), ] }
    if (isTRUE(!"sd" %in% x$args$print)) { print.object <- print.object[-grep("SD", print.object[, 1L]), ] }

    # One variable
    if (isTRUE(ncol(print.object) == 2L)) { print.object <- print.object[-1L, ] }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Output ####

    cat(" Multilevel Descriptive Statistics\n\n")

    if (isTRUE(ncol(print.object) > 2L)) {

      .write.table(print.object, left = 1L, right = 3L, horiz = horiz)

    } else {

      write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE)

    }

  #_____________________________________________________________________________
  #
  # Simultaneous and LS Multilevel Model Fit Information, multilevel.fit() -----
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
  # CI for the Indirect Effect in a ML Mediation Model, multilevel.indirect() --
  }, multilevel.indirect = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Format ####

    # Round
    print.object$mc <- sapply(c("est", "se", "low", "upp"), function(y) ifelse(!is.na(print.object$mc [, y]), formatC(print.object$mc[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA))

    # Column names
    print.object$mc <- rbind(c("Est.", "SE", "Low", "Upp"), print.object$mc)

    # Justify right
    print.object$mc <- apply(print.object$mc, 2L, format, justify = "right")

    # Add blank space
    print.object$mc[, "est"] <- paste0("   ", print.object$mc[, "est"])

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Output ####

    cat(paste(switch(x$args$alternative, two.sided = " Two-Sided", less = " One-Sided", greater = " One-Sided"),
              paste0(round(x$args$conf.level * 100L, digits = 2L), "%"), "Confidence Interval: Indirect Effect\n\n"))

    cat("  Monte Carlo Method with",  format(x$args$nrep, scientific = FALSE), "repetitions\n")

    .write.table(print.object$mc, left = 1L, right = 3L, horiz = horiz)

  #_____________________________________________________________________________
  #
  # R-Squared Measures for Multilevel and Linear Mixed Effects Models Manual, multilevel.r2.manual()
  }, multilevel.r2.manual = {

    #...................
    ### Round ####

    print.object$total <- data.frame(sapply(print.object$total[, !is.na(print.object$total)], formatC, digits = digits, format = "f", simplify = FALSE))

    #...................
    ### Format ####

    print.object$total <- rbind(colnames(print.object$total), print.object$total)

    # Justify left and right
    print.object$total <- format(print.object$total, justify = "right")

    # Add blank space
    print.object$total[, 1L] <- paste0("    ", print.object$total[, 1L])

    if (isTRUE(ncol(print.object$decomp) != 1L)) {

      #...................
      ### Round ####

      print.object$within <- data.frame(sapply(print.object$within, formatC, digits = digits, format = "f", simplify = FALSE))
      print.object$between <- data.frame(sapply(print.object$between, formatC, digits = digits, format = "f", simplify = FALSE))

      #...................
      ### Format ####

      print.object$within <- rbind(colnames(print.object$within), print.object$within)
      print.object$between <- rbind(colnames(print.object$between), print.object$between)

      # Justify left and right
      print.object$within <- format(print.object$within, justify = "right")
      print.object$between <- format(print.object$between, justify = "right")

      # Add blank space
      print.object$within[, 1L] <- paste0("    ", print.object$within[, 1L])
      print.object$between[, 1L] <- paste0("    ", print.object$between[, 1L])

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Output ####

    cat(" R-Squared Measures for Multilevel and Linear Mixed Effects Models\n\n")

    cat("  Integrative Framework of R-Squared Measures (Rights and Sterba, 2019)\n\n")

    cat(ifelse(isTRUE(getOption("knitr.in.progress")), "   Total R2\n", "   Total R\u00B2\n"))

    .write.table(print.object$total, left = 3L, right = 5L, horiz = horiz)

    # Predictors are cluster-mean-centered
    if (isTRUE(ncol(print.object$decomp) != 1L)) {

      cat(ifelse(isTRUE(getOption("knitr.in.progress")), "\n   Within-Cluster R2\n", "\n   Within-Cluster R\u00B2\n"))

      .write.table(print.object$within, left = 3L, right = 5L, horiz = horiz)

      cat(ifelse(isTRUE(getOption("knitr.in.progress")), "\n   Between-Cluster R2\n", "\n   Between-Cluster R\u00B2\n"))

      .write.table(print.object$between, left = 3L, right = 5L, horiz = horiz)

    }

  #_____________________________________________________________________________
  #
  # R-Squared Measures for Multilevel and Linear Mixed Effects Models, multilevel.r2()
  }, multilevel.r2 = {

    if (isTRUE("RS" %in% x$args$print)) {

      #...................
      ### Round ####

      print.object$rs$total <- data.frame(sapply(print.object$rs$total[, !is.na(print.object$rs$total)], formatC, digits = digits, format = "f", simplify = FALSE))

      #...................
      ### Format ####

      print.object$rs$total <- rbind(colnames(print.object$rs$total), print.object$rs$total)

      # Justify left and right
      print.object$rs$total <- format(print.object$rs$total, justify = "right")

      # Add blank space
      print.object$rs$total[, 1L] <- paste0("    ", print.object$rs$total[, 1L])

      if (isTRUE(ncol(print.object$rs$decomp) != 1L)) {

        #...................
        ### Round ####

        print.object$rs$within <- data.frame(sapply(print.object$rs$within, formatC, digits = digits, format = "f", simplify = FALSE))
        print.object$rs$between <- data.frame(sapply(print.object$rs$between, formatC, digits = digits, format = "f", simplify = FALSE))

        #...................
        ### Format ####

        print.object$rs$within <- rbind(colnames(print.object$rs$within), print.object$rs$within)
        print.object$rs$between <- rbind(colnames(print.object$rs$between), print.object$rs$between)

        # Justify left and right
        print.object$rs$within <- format(print.object$rs$within, justify = "right")
        print.object$rs$between <- format(print.object$rs$between, justify = "right")

        # Add blank space
        print.object$rs$within[, 1L] <- paste0("    ", print.object$rs$within[, 1L])
        print.object$rs$between[, 1L] <- paste0("    ", print.object$rs$between[, 1L])

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

      .write.table(print.object$rs$total, left = 3L, right = 5L, horiz = horiz)

      # Predictors are cluster-mean-centered
      if (isTRUE(ncol(print.object$rs$decomp) != 1L)) {

        cat(ifelse(isTRUE(getOption("knitr.in.progress")), "\n   Within-Cluster R2\n", "\n   Within-Cluster R\u00B2\n"))

        .write.table(print.object$rs$within, left = 3L, right = 5L, horiz = horiz)

        cat(ifelse(isTRUE(getOption("knitr.in.progress")), "\n   Between-Cluster R2\n", "\n   Between-Cluster R\u00B2\n"))

        .write.table(print.object$rs$between, left = 3L, right = 5L, horiz = horiz)

      }

    }

  #_____________________________________________________________________________
  #
  # Cross-Level Measurement Invariance, multilevel.invar() ---------------------
  }, multilevel.invar = {

    cat(" Cross-Level Measurement Invariance\n")

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## lavaan Summary ####

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
    ## Covariance Coverage ####

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
      print.coverage <- cbind(paste0("   ", colnames(print.coverage)), print.coverage)

      # Column names
      print.coverage <- rbind(colnames(print.coverage), print.coverage)

      # Justify left and right
      print.coverage[, 1L] <- format(print.coverage[, 1L], justify = "left")
      print.coverage[, -1L] <- apply(print.coverage[, -1L], 2L, format, justify = "right")

      #...................
      ### Print ####

      .write.table(print.coverage, left = 2L, right = 4L, horiz = horiz)

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
      print.itemstat <- rbind(c("Variable", "n", "nNA", "%NA", "M", "SD", "Min", "Max",  "Skew", "Kurt", "ICC(1)"), print.itemstat)

      # Justify left and right
      print.itemstat[, 1L] <- format(print.itemstat[, 1L, drop = FALSE], justify = "left")
      print.itemstat[, -1L] <- apply(print.itemstat[, -1L, drop = FALSE], 2L, function(y) format(y, justify = "right"))

      # Add blank space
      print.itemstat[, "variable"] <- c(paste0("   ", print.itemstat[1L, "variable"], collapse = ""), paste0("    ", print.itemstat[-1L, "variable"]))
      print.itemstat[, "variable"] <- format(c(print.itemstat[1L, "variable"], misty::chr.trim(print.itemstat[-1L, "variable"], side = "right")), justify = "left")

      #...................
      ### Print ####

      .write.table(print.itemstat, left = 2L, right = 4L, horiz = horiz)

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Model Fit ####

    if (isTRUE("fit" %in% x$args$print)) {

      if (isTRUE(x$args$invar != "config")) { cat("\n  Model Fit Information and Model Comparison\n") } else { cat("\n  Model Fit Information\n") }

      print.fit <- print.object$fit

      print.fit.stand <- print.fit$stand
      print.fit.scaled <- print.fit$scaled
      print.fit.robust <- print.fit$robust

      #...................
      ### Round ####

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
      ### Add Labels ####

      print.fit.stand <- rbind(switch(x$args$invar, config = { c("", "Config") }, metric = { c("", "Config", "Metric", "DMetric") }, scalar = { c("", "Config", "Metric", "Scalar", "DMetric", "DScalar") }), print.fit.stand[1L:23L, ], make.row.names = FALSE)
      if (isTRUE(!is.null(print.fit.scaled))) { print.fit.scaled <- rbind(switch(x$args$invar, config = { c("", "Config") }, metric = { c("", "Config", "Metric", "DMetric") }, scalar = { c("", "Config", "Metric", "Scalar", "DMetric", "DScalar") }), print.fit.scaled[1L:24L, ], make.row.names = FALSE) }
      if (isTRUE(!is.null(print.fit.robust))) { print.fit.robust <- rbind(switch(x$args$invar, config = { c("", "Config") }, metric = { c("", "Config", "Metric", "DMetric") }, scalar = { c("", "Config", "Metric", "Scalar", "DMetric", "DScalar") }), print.fit.robust[1L:24L, ], make.row.names = FALSE) }

      #...................
      ### Replace NA with "" ####

      print.fit.stand <- unname(misty::na.as(print.fit.stand, na = ""))
      if (isTRUE(!is.null(print.fit.scaled))) { print.fit.scaled <- unname(misty::na.as(print.fit.scaled, na = "")) }
      if (isTRUE(!is.null(print.fit.robust))) { print.fit.robust <- unname(misty::na.as(print.fit.robust, na = "")) }

      #...................
      ### Add Blank Space ####

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
      ### Justify Left and Right ####

      print.fit.stand[, 1L] <- format(print.fit.stand[, 1L, drop = FALSE], justify = "left")
      if (isTRUE(!is.null(print.fit.scaled))) { print.fit.scaled[, 1L] <- format(print.fit.scaled[, 1L, drop = FALSE], justify = "left") }
      if (isTRUE(!is.null(print.fit.robust))) { print.fit.robust[, 1L] <- format(print.fit.robust[, 1L, drop = FALSE], justify = "left") }

      print.fit.stand[, -1L] <- apply(print.fit.stand[, -1L, drop = FALSE], 2L, function(y) format(y, justify = "right"))
      if (isTRUE(!is.null(print.fit.scaled))) { print.fit.scaled[, -1L] <- apply(print.fit.scaled[, -1L, drop = FALSE], 2L, function(y) format(y, justify = "right")) }
      if (isTRUE(!is.null(print.fit.robust))) { print.fit.robust[, -1L] <- apply(print.fit.robust[, -1L, drop = FALSE], 2L, function(y) format(y, justify = "right")) }

      #...................
      ### Print ####

      if (isTRUE("standard" %in% x$args$print.fit)) {

        cat("\n   Standard CFI, TLI, and RMSEA\n")

        # R Markdown not in progress
        if (is.null(getOption("knitr.in.progress"))) { print.fit.stand[1L, ] <- sub("D", "\u0394", print.fit.stand[1L, ]) }

        .write.table(print.fit.stand, left = 3L, right = 5L, horiz = horiz)

      }

      if (isTRUE("scaled" %in% x$args$print.fit)) {

        cat("\n   Scaled CFI, TLI, and RMSEA\n")

        # R Markdown not in progress
        if (is.null(getOption("knitr.in.progress"))) { print.fit.scaled[1L, ] <- sub("D", "\u0394", print.fit.scaled[1L, ]) }

        .write.table(print.fit.scaled, left = 3L, right = 5L, horiz = horiz)

      }

      if (isTRUE("robust" %in% x$args$print.fit)) {

        cat("\n   Robust CFI, TLI, and RMSEA\n")

        # R Markdown not in progress
        if (is.null(getOption("knitr.in.progress"))) { print.fit.robust[1L, ] <- sub("D", "\u0394", print.fit.robust[1L, ]) }

        .write.table(print.fit.robust, left = 3L, right = 5L, horiz = horiz)

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Parameter Estimates ####

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
      print.param.w[, -c(1L:4L, 8L)] <- lapply(print.param.w[, -c(1L:4L, 8L)], function(y) ifelse(!is.na(y), formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

      # p.digits
      print.param.w[, "pvalue"] <- formatC(as.numeric(print.param.w[, "pvalue"]), digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

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

        # Heading
        write.table(print.param.w[1L, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

        # Horizontal line
        cat(paste(rep(" ", times = 3), collapse = ""), paste(rep("\u2500", times = sum(nchar(print.param.w[1L, 4L:9L])) + 1), collapse = ""), "\n")

        cat("    Factor Loadings\n")

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

        print.lv.cov <- print.param.w[print.param.w$param == "latent variable covariance", ]

        if (isTRUE(length(unique(print.lv.cov$lhs)) == 1L)) { cat("\n   Latent Variable Covariance\n") } else { cat("\n   Latent Variable Covariances\n") }

        for (i in unique(print.lv.cov$lhs)) {

          write.table(print.lv.cov[print.lv.cov$lhs == i, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

        }

      }

      ##### Residual covariances
      if (isTRUE(any(print.param.w$param %in% "residual covariance"))) {

        print.res.cov <- print.param.w[print.param.w$param == "residual covariance", ]

        # Heading latent variables
        if (isTRUE(any(print.param.w$param %in% "latent variable"))) {

          if (isTRUE(length(unique(print.res.cov$lhs)) == 1L)) { cat("\n   Residual Covariance\n") } else { cat("\n   Residual Covariances\n") }

          # Heading no latent variables
        } else {

          if (isTRUE(length(unique(print.res.cov$lhs)) == 1L)) { cat("\n   Covariance\n") } else { cat("\n   Covariances\n") }

        }

        for (i in unique(print.res.cov$lhs)) { write.table(print.res.cov[print.res.cov$lhs == i, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE) }

      }

      ##### Latent variance
      if (isTRUE(any(print.param.w$param %in% "latent variance"))) {

        print.var <- print.param.w[print.param.w$param == "latent variance", ]

        print.var[grep("NA", print.var[, "z"]), c("se", "z", "pvalue", "stdyx")] <- ""

        if (isTRUE(nrow(print.var) == 1L)) { cat("\n   Latent Variance\n") } else { cat("\n   Latent Variances\n") }

        write.table(print.var[, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

      }

      ##### Residual variance
      if (isTRUE(any(print.param.w$param %in% "residual variance"))) {

        print.resid <- print.param.w[print.param.w$param == "residual variance", ]

        pos.NA <- grep("NA", print.resid[, "z"])

        if (isTRUE(length(pos.NA) >  0L)) {

          print.resid[pos.NA, "se"] <- paste(rep("", times = unique(nchar(print.resid[pos.NA, "se"])) + 1L), collapse = " ")
          print.resid[pos.NA, "z"] <- paste(rep("", times = unique(nchar(print.resid[pos.NA, "z"])) + 1L), collapse = " ")
          print.resid[pos.NA, "pvalue"] <- paste(rep("", times = unique(nchar(print.resid[pos.NA, "pvalue"])) + 1L), collapse = " ")

        }

        # Heading latent variables
        if (isTRUE(nrow(print.resid) == 1L)) { cat("\n   Residual Variance\n") } else { cat("\n   Residual Variances\n") }

        write.table(print.resid[, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

      }

      #...................
      ### Between Level ####

      cat("\n   Between Level\n\n")

      #...................
      #### Round ####

      # digits
      print.param.b[, -c(1L:4L, 8L)] <- lapply(print.param.b[, -c(1L:4L, 8L)], function(y) ifelse(!is.na(y), formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

      # p.digits
      print.param.b[, "pvalue"] <- formatC(as.numeric(print.param.b[, "pvalue"]), digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

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

        # Heading
        write.table(print.param.b[1L, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

        # Horizontal line
        cat(paste(rep(" ", times = 3), collapse = ""), paste(rep("\u2500", times = sum(nchar(print.param.b[1L, 4L:9L])) + 1), collapse = ""), "\n")

        cat("    Factor Loadings\n")

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

        print.lv.cov <- print.param.b[print.param.b$param == "latent variable covariance", ]

        if (isTRUE(length(unique(print.lv.cov$lhs)) == 1L)) { cat("\n   Latent Variable Covariance\n") } else { cat("\n   Latent Variable Covariances\n") }

        for (i in unique(print.lv.cov$lhs)) { write.table(print.lv.cov[print.lv.cov$lhs == i, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE) }

      }

      ##### (Residual) Covariances
      if (isTRUE(any(print.param.b$param %in% "residual covariance"))) {

        print.res.cov <- print.param.b[print.param.b$param == "residual covariance", ]

        # Heading latent variables
        if (isTRUE(any(print.param.b$param %in% "latent variable"))) {

          if (isTRUE(length(unique(print.res.cov$lhs)) == 1L)) { cat("\n   Residual Covariance\n") } else { cat("\n   Residual Covariances\n") }

        # Heading no latent variables
        } else {

          if (isTRUE(any(print.param.b$param %in% "latent variable"))) { cat("\n") }

          if (isTRUE(length(unique(print.res.cov$lhs)) == 1L)) { cat("   Covariance\n") } else { cat("   Covariances\n") }

        }

        for (i in unique(print.res.cov$lhs)) { write.table(print.res.cov[print.res.cov$lhs == i, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE) }

      }

      ##### Latent mean
      if (isTRUE(any(print.param.b$param %in% "latent mean"))) {

        print.mean <- print.param.b[print.param.b$param == "latent mean", ]

        print.mean[grep("NA", print.mean[, "z"]), c("se", "z", "pvalue", "stdyx")] <- ""

        if (isTRUE(nrow(print.mean) == 1L)) { cat("\n   Latent Mean\n") } else { cat("\n   Latent Means\n") }

        write.table(print.mean[, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

      }

      ##### Latent variance
      if (isTRUE(any(print.param.b$param %in% "latent variance"))) {

        print.var <- print.param.b[print.param.b$param == "latent variance", ]

        print.var[grep("NA", print.var[, "z"]), c("se", "z", "pvalue", "stdyx")] <- ""

        if (isTRUE(nrow(print.var) == 1L)) { cat("\n   Latent Variance\n") } else { cat("\n   Latent Variances\n") }

        write.table(print.var[, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

      }

      ##### Intercepts
      if (isTRUE(any(print.param.b$param %in% "intercept"))) {

        print.inter <- print.param.b[print.param.b$param == "intercept", ]

        print.inter[grep("NA", print.inter[, "z"]), c("se", "z", "pvalue", "stdyx")] <- ""

        if (isTRUE(nrow(print.inter) == 1L)) { cat("\n   Intercept\n") } else { cat("\n   Intercepts\n") }

        write.table(print.inter[, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

      }

      ##### (Residual) Variance
      if (isTRUE(any(print.param.b$param %in% "residual variance"))) {

        print.resid <- print.param.b[print.param.b$param == "residual variance", ]

        pos.NA <- grep("NA", print.resid[, "z"])

        if (isTRUE(length(pos.NA) >  0L)) {

          print.resid[pos.NA, "se"] <- paste(rep("", times = unique(nchar(print.resid[pos.NA, "se"])) + 1L), collapse = " ")
          print.resid[pos.NA, "z"] <- paste(rep("", times = unique(nchar(print.resid[pos.NA, "z"])) + 1L), collapse = " ")
          print.resid[pos.NA, "pvalue"] <- paste(rep("", times = unique(nchar(print.resid[pos.NA, "pvalue"])) + 1L), collapse = " ")

        }

        # Heading latent variables
        if (isTRUE(any(print.param.b$param %in% "latent variable"))) {

          if (isTRUE(nrow(print.resid) == 1L)) { cat("\n   Residual Variance\n") } else { cat("\n   Residual Variances\n") }

        } else {

          if (isTRUE(nrow(print.resid) == 1L)) { cat("\n   Variance\n") } else { cat("\n   Variances\n") }

        }

        write.table(print.resid[, 4L:9L], quote = FALSE, row.names = FALSE, col.names = FALSE)

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Modification Indices ####

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

            # Horizontal line
            cat(paste(rep(" ", times = 4L), collapse = ""), paste(rep("\u2500", times = sum(nchar(print.modind.w[1L, ]))), collapse = ""), "\n")

            write.table(print.modind.w.load, quote = FALSE, row.names = FALSE, col.names = FALSE)

          }

          # Residual covariances
          print.modind.w.cov <- print.modind.w[print.modind.w$op == "~~", ]

          if (isTRUE(nrow(print.modind.w.cov) > 0L)) {

            cat("\n    Residual Covariances\n")

            # Print header
            write.table(print.modind.w[1L, ], quote = FALSE, row.names = FALSE, col.names = FALSE)

            # Horizontal line
            cat(paste(rep(" ", times = 4L), collapse = ""), paste(rep("\u2500", times = sum(nchar(print.modind.w[1L, ]))), collapse = ""), "\n")

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

            cat(paste(rep(" ", times = 4L), collapse = ""), paste(rep("\u2500", times = sum(nchar(print.modind.b[1L, ]))), collapse = ""), "\n")

            write.table(print.modind.b.load, quote = FALSE, row.names = FALSE, col.names = FALSE)

          }

          # Residual covariances
          print.modind.b.cov <- print.modind.b[print.modind.b$op == "~~", ]

          if (isTRUE(nrow(print.modind.b.cov) > 0L)) {

            cat("\n    Residual Covariances\n")

            # Print header
            write.table(print.modind.b[1L, ], quote = FALSE, row.names = FALSE, col.names = FALSE)

            # Horizontal line
            cat(paste(rep(" ", times = 4L), collapse = ""), paste(rep("\u2500", times = sum(nchar(print.modind.b[1L, ]))), collapse = ""), "\n")

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

          .write.table(print.score, left = 2L, right = 4L, horiz = horiz)

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
        .write.table(print.resid$within, left = 3L, right = 5L, horiz = horiz)

        cat("\n   Between Level\n")
        .write.table(print.resid$between, left = 3L, right = 5L, horiz = horiz)

      }

    }

  #_____________________________________________________________________________
  #
  # Multilevel Composite Reliability, multilevel.omega() -----------------------
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

      .write.table(print.omega, left = 2L, right = 4L, horiz = horiz)

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Item Statistics ####

    if (isTRUE("item" %in% x$args$print)) {

      print.item <- print.object$item

      #### Round ####

      # Variables to round
      if (isTRUE(all(print.item$nNA == 0L))) {

        print.round <- switch(x$args$const,
                              within = c("m", "sd", "min", "max", "skew", "kurt", "ICC", "wstd.ld"),
                              shared = c("m", "sd", "min", "max", "skew", "kurt", "ICC", "bstd.ld"),
                              config = c("m", "sd", "min", "max", "skew", "kurt", "ICC", "wstd.ld", "bstd.ld"))

      } else {

        print.round <- switch(x$args$const,
                              within = c("pNA", "m", "sd", "min", "max", "skew", "kurt", "ICC", "wstd.ld"),
                              shared = c("pNA", "m", "sd", "min", "max", "skew", "kurt", "ICC", "bstd.ld"),
                              config = c("pNA", "m", "sd", "min", "max", "skew", "kurt", "ICC", "wstd.ld", "bstd.ld"))

      }

      print.item[, print.round] <- sapply(print.round, function(y) ifelse(!is.na(print.item[, y]), formatC(print.item[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

      #...................
      ### Format ####

      # Percentages
      print.item[, "pNA"] <- paste0(print.item[, "pNA"], "%")

      # Column names
      print.item <- rbind(switch(x$args$const,
                                 within = c("Variable", "n", "nNA", "%NA", "M", "SD", "Min", "Max",  "Skew", "Kurt", "ICC(1)", "WStd.ld"),
                                 shared = c("Variable", "n", "nNA", "%NA", "M", "SD", "Min", "Max",  "Skew", "Kurt", "ICC(1)", "BStd.ld"),
                                 config = c("Variable", "n", "nNA", "%NA", "M", "SD", "Min", "Max",  "Skew", "Kurt", "ICC(1)", "WStd.ld", "BStd.ld")), print.item)

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

      .write.table(print.item, left = 2L, right = 4L, horiz = horiz)

    }

  #_____________________________________________________________________________
  #
  # Auxiliary variables analysis, na.auxiliary() -------------------------------
  }, na.auxiliary = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Product-Moment Correlation Matrix, Cramer's V and Cohen's d Matrix ####

    if (isTRUE(is.null(x$args$model))) {

      #...................
      ### Correlation Matrix ####

      print.object$cor <- apply(print.object$cor, 2L, function(y) formatC(as.numeric(y), digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")))

      # Lower and/or upper triangular
      switch(tri, "lower" = {

        print.object$cor[upper.tri(print.object$cor)] <- ""

      }, "upper" = {

        print.object$cor[lower.tri(print.object$cor)] <- ""

      })

      diag(print.object$cor) <- ""

      #...................
      ### Cohen's d and Cramer's V Matrix ####

      print.object$d <- apply(print.object$d, 2L, function(y) formatC(as.numeric(y), digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")))

      # Print table
      print.object <- data.frame(cbind(c("", colnames(print.object$cor), "", row.names(x$result$d)), rbind(colnames(print.object$cor), print.object$cor, colnames(print.object$cor), print.object$d)))

      # NA
      print.object <- sapply(print.object, function(y) gsub("NA", "", y))

      # Format
      print.object[, 1L] <- paste0("    ", print.object[, 1L])

      print.object[, 1L] <- format(print.object[, 1L], justify = "left")

      print.object[, -1L] <- apply(print.object[, -1L], 2L, function(y) format(y, justify = "right"))

      #...................
      ### Output ####

      # Variables Related to the Incomplete Variable
      cat(" Auxiliary Variables Analysis\n\n",

          " Variables Related to the Incomplete Variable\n\n",
          ifelse(is.null(x$args$categ), "  Pearson product-moment correlation\n",
                 # Not all variables are categorical
                 if (isTRUE(!all(colnames(x$data) %in% x$args$categ))) {

                   if (isTRUE(all(sapply(x$data[, x$args$categ, drop = FALSE], misty::uniq.n) == 2L))) {

                     "  Pearson product-moment correlation, Multiple R and Phi coefficient\n"

                   } else if (isTRUE(all(sapply(x$data[, x$args$categ, drop = FALSE], misty::uniq.n) != 2L))) {

                     "  Pearson product-moment correlation and Cramer's V \n"

                   } else {

                     "  Pearson product-moment correlation, Multiple R, Phi coefficient and Cramer's V\n"

                   }

                 # All variables are categorical
                 } else {

                   if (isTRUE(all(sapply(x$data, misty::uniq.n) == 2L))) {

                     "  Phi coefficient\n"

                   } else if (isTRUE(all(sapply(x$data, misty::uniq.n) != 2L))) {

                     "  Cramer's V\n"

                   } else {

                     "  Phi coefficient and Cramer's V\n"

                   }

                 }))

      .write.table(print.object[1L:(nrow(x$result$cor) + 1L), ], left = 3L, right = 5L, horiz = horiz)

      # Variables Related to the Probability of Missingness
      cat("\n  Variables Related to the Probability of Missigness\n\n",
          ifelse(is.null(x$args$categ), "  Cohen's d\n", if (isTRUE(!all(colnames(x$data) %in% x$args$categ))) {

                   if (isTRUE(all(sapply(x$data[, x$args$categ, drop = FALSE], misty::uniq.n) == 2L))) {

                     "  Cohen's d and Phi coefficient \n"

                   } else if (isTRUE(all(sapply(x$data[, x$args$categ, drop = FALSE], misty::uniq.n) != 2L))) {

                     "  Cohen's d and Cramer's V \n"

                   } else {

                     "  Cohen's d, Phi coefficient and Cramer's V\n"

                   }

                 } else {

                   if (isTRUE(all(sapply(x$data, misty::uniq.n) == 2L))) {

                     "  Phi coefficient\n"

                   } else if (isTRUE(all(sapply(x$data, misty::uniq.n) != 2L))) {

                     "  Cramer's V\n"

                   } else {

                     "  Phi coefficient and Cramer's V \n"

                   }

                 }))

      .write.table(print.object[(nrow(x$result$cor) + 2L):nrow(print.object), ], left = 3L, right = 5L, horiz = horiz)

      cat("\n", "  Note. Indicator variables are in the rows (0 = obs, 1 = miss)\n")

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

      print.object <- rbind(c("", "", "", "Estimate", "SE", "z", "p", "Low", "Upp"), print.object)

      print.object[, 1L] <- paste("   ", print.object[, 1L])

      print.object <- apply(print.object, 2L, format, justify = "right")

      #...................
      ### Print Output ####

      cat(" Auxiliary Variables\n\n",
          " Variables Related to the Incomplete Variable\n\n",
          "  Substantive model: Standardized slope\n")

      .write.table(print.object[c(1L, model.sub + 1L), ], left = 3L, right = 5L, horiz = horiz)

      cat("\n   Auxiliary model: Semi-partial correlation coefficient\n")

      .write.table(print.object[c(1L, model.aux + 1L), ], left = 3L, right = 5L, horiz = horiz)

    }

  #_____________________________________________________________________________
  #
  # Variance-Covariance Coverage, na.coverage() --------------------------------
  }, na.coverage = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Format ####

    # Lower and/or Upper Triangular
    switch(tri, "lower" = {

      print.object[upper.tri(print.object)] <- ""

    }, "upper" = {

      print.object[lower.tri(print.object)] <- ""

    })

    # Format proportions
    print.object <- apply(print.object, 2L, function(y) ifelse(!is.na(as.numeric(y)), formatC(as.numeric(y), digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), ""))

    # Row names
    print.object <- cbind(paste0("  ", row.names(x$result)), print.object)

    # Column names
    print.object <- rbind(colnames(print.object), print.object)

    # Justify left and right
    print.object[, 1L] <- format(print.object[, 1L], justify = "left")
    print.object[, -1L] <- apply(print.object[, -1L], 2L, format, justify = "right")

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Output ####

    cat(" Variance-Covariance Coverage\n\n")

    .write.table(print.object, left = 1L, right = 3L, horiz = horiz)

  #_____________________________________________________________________________
  #
  # Descriptive Statistics for Missing Data, na.descript() ---------------------
  }, na.descript = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Result Table ####

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
                                     print.object$L1$perc.missing.min.l1, print.object$L1$perc.missing.max.l1))

    #### Format ####
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
                                         print.object$L2$perc.missing.min.l2, print.object$L2$perc.missing.max.l2))

        #### Format ####
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
                                         print.object$L3$perc.missing.min.l3, print.object$L3$perc.missing.max.l3))

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

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Output ####

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

      #### Frequency Table ####

      if (isTRUE(table)) {

        freqtab <- x$result$L1$table.miss

        freqtab[, c("pOb", "pNA")] <- apply(freqtab[, c("pOb", "pNA")], 2L, function(y) paste0(formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), "%"))

        # Column names
        freqtab <- rbind(c("Var", "nOb", "%Ob", "nNA", "%NA"), freqtab)

        freqtab[, -1L] <- apply(freqtab[ -1L], 2L, format, justify = "right")

        freqtab[, 1L] <- paste0(ifelse(x$no.cluster == "none", "    ", "     "), format(freqtab[, 1L], justify = "left"))

        cat("\n")

        .write.table(freqtab, left = 3L, right = 5L, horiz = horiz)

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

        #### Frequency Table ####

        if (isTRUE(table)) {

          freqtab <- x$result$L2$table.miss

          freqtab[, c("pOb", "pNA")] <- apply(freqtab[, c("pOb", "pNA")], 2L, function(y) paste0(formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), "%"))

          # Column names
          freqtab <- rbind(c("Var", "nOb", "%Ob", "nNA", "%NA"), freqtab)

          freqtab[, -1L] <- apply(freqtab[ -1L], 2L, format, justify = "right")

          freqtab[, 1L] <- paste0("     ", format(freqtab[, 1L], justify = "left"))

          cat("\n")

          .write.table(freqtab, left = 3L, right = 5L, horiz = horiz)

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

        #### Frequency Table ####

        if (isTRUE(table)) {

          freqtab <- x$result$L3$table.miss

          freqtab[, c("pOb", "pNA")] <- apply(freqtab[, c("pOb", "pNA")], 2L, function(y) paste0(formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), "%"))

          # Column names
          freqtab <- rbind(c("Var", "nOb", "%Ob", "nNA", "%NA"), freqtab)

          freqtab[, -1L] <- apply(freqtab[ -1L], 2L, format, justify = "right")

          freqtab[, 1L] <- paste0("     ", format(freqtab[, 1L], justify = "left"))

          cat("\n")

          .write.table(freqtab, left = 3L, right = 5L, horiz = horiz)

        }

      } else {

        cat("   There are no Level-3 variables")

      }

    }

  #_____________________________________________________________________________
  #
  # Missing Data Pattern, na.pattern() -----------------------------------------
  }, na.pattern = {

    # NA
    print.object[, "pattern"][is.na(print.object[, "pattern"])] <- ""
    print.object[, "nNA"][is.na(print.object[, "nNA"])] <- ""

    # Percentages
    print.object[, "perc"] <- paste0(formatC(as.numeric(print.object[, "perc"]), digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), "%")

    print.object[, "pNA"] <- paste0(formatC(as.numeric(print.object[, "pNA"]), digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), "%")

    print.object[nrow(print.object), ncol(print.object)] <- ""

    # Format
    colnames(print.object)[1L] <- "  Pattern"
    colnames(print.object)[3L] <- " Perc"

    # Pattern
    print.object[, 1L] <- paste("  ", print.object[, 1L])

    # Column names
    print.object <- rbind(colnames(print.object), print.object)

    # Justify left and right
    print.object[, 1L] <- format(print.object[, 1L], justify = "left")
    print.object[, -1L] <- format(print.object[, -1L], justify = "right")

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Output ####

    cat(" Missing Data Pattern\n\n")

    (which(misty::chr.trim(as.data.frame(print.object)[, 1L]) == "") - 1L) |>
      (\(p) {

        .write.table(as.data.frame(print.object)[seq_len(p), ], left = 1L, right = 3L, horiz = horiz)

        cat(paste(" ", paste(rep("\u2500", times = sum(nchar(print.object[1L, ])) + length(print.object[1L, ]) - 3L), collapse = "")), "\n")

        write.table(as.data.frame(print.object)[p + 1L, ], quote = FALSE, row.names = FALSE, col.names = FALSE)

      })()

  #_____________________________________________________________________________
  #
  # Missing Completely at Random (MCAR) Test, na.test() ------------------------
  }, na.test = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Little's MCAR Test ####

    if (isTRUE("little" %in% x$args$print)) {

      #...................
      ### Round ####

      print.object$little$statistic <- formatC(print.object$little$statistic, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))
      print.object$little$p <- formatC(print.object$little$p, digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

      #...................
      ### Format ####

      print.object$little <- rbind(c("n", "nIncomp", "nPattern", "chi2", "df", "p"), print.object$little)

      print.object$little <- apply(print.object$little, 2L, format, justify = "right")

      # R Markdown in progress
      if (isTRUE(is.null(getOption("knitr.in.progress")))) {

        print.object$little[1L, "statistic"] <- paste0(paste0(rep(" ", times = nchar(print.object$little[1L, "statistic"]) - 2L), collapse = ""), "\u03C7\u00B2", collapes = "")

      }

      print.object$little[, 1L] <- paste(" ", print.object$little[, 1L])

      #...................
      ### Print Output ####

      cat(paste0(" Little's MCAR Test\n\n"))

      # Print output
      .write.table(print.object$little, left = 1L, right = 3L, horiz = horiz)

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Jamshidian and Jalal's Approach ####

    if (isTRUE("jamjal" %in% x$args$print)) {

      #...................
      ### Round ####

      print.object$hawkins$statistic <- formatC(print.object$hawkins$statistic, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))
      print.object$hawkins$p <- formatC(print.object$hawkins$p, digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

      print.object$anderson$statistic <- formatC(print.object$anderson$statistic , digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))
      print.object$anderson$p <- formatC(print.object$anderson$p, digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

      #...................
      ### Format ####

      print.object$hawkins <- rbind(c("n", "nIncomp", "nPattern", "chi2", "df", "p"), print.object$hawkins)
      print.object$hawkins <- apply(print.object$hawkins, 2L, format, justify = "right")

      print.object$anderson <- rbind(c("n", "nIncomp", "nPattern", "T", "p"), print.object$anderson)
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

      # Print output
      .write.table(print.object$hawkins, left = 3L, right = 5L, horiz = horiz)

      #### Anderson-Darling Non-Parametric Test ####
      if (isTRUE(x$result$hawkins$p <= x$args$alpha)) {

        cat(paste0("\n  Anderson-Darling Non-Parametric Test\n\n"))

        if (isTRUE(is.null(getOption("knitr.in.progress")))) {

          cat("   H\u2080: Multivariate Non-Normality, but Homogeneity of Covariances (MCAR)\n")
          cat("   H\u2081: Heterogeneity of Covariances (MCAR Violation)\n\n")

        } else {

          cat("   H0: Multivariate Non-Normality, but Homogeneity of Covariances (MCAR)\n")
          cat("   H1: Heterogeneity of Covariances (MCAR Violation)\n\n")

        }

        # Print output
        .write.table(print.object$anderson, left = 3L, right = 5L, horiz = horiz)


      }

    }

  #_____________________________________________________________________________
  #
  # Robust Estimation of MLM and LMM, robust.lmer() ----------------------------
  }, robust.lmer = {

    # Two-level model
    model.twolevel <- ifelse(lme4::getME(x$model, name = "n_rtrms") == 1L, TRUE, FALSE)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Title ####

    cat(" Robust Multilevel and Linear Mixed-Effects Model\n\n")

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Call ####

    cat("  Formula: ", print.object$call$formula, "\n", " Data:    ", print.object$call$data, "\n")

    cat("  Method:  ", x$model@method, "\n\n")

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Coefficients ####

    #...................
    ### Random Effects ####

    # Random intercept model
    if (isTRUE(length(misty::chr.omit(colnames(print.object$randeff), omit = c("groups", "name", "var", "sd", "cor"))) == 0L || (all(na.omit(print.object$randeff[, "cor"]) == 1L) && all(na.omit(print.object$randeff[, ncol(print.object$randeff)]) == 1L)))) {

      # Header
      cat("  Random Effects: Variance Components", "\n\n")

      print.object$randeff <- print.object$randeff[, -(grep("cor", colnames(print.object$randeff)):ncol(print.object$randeff))]

      # Round variables
      print.object$randeff[, c("var", "sd")] <- sapply(c("var", "sd"), function(y) ifelse(!is.na(print.object$randeff[, y]), formatC(print.object$randeff[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

      # Columns
      print.object$randeff <- data.frame(rbind(c("Groups", "Name", "Var", "SD"), print.object$randeff), fix.empty.names = FALSE, row.names = NULL)

    # Random intercept and slope model
    } else {

      # Header
      cat("  Random Effects: Variance and Correlation Components", "\n\n")

      # Round variables
      print.object$randeff[, c("var", "sd")] <- sapply(c("var", "sd"), function(y) ifelse(!is.na(print.object$randeff[, y]), formatC(print.object$randeff[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))
      print.object$randeff[, which(!colnames(print.object$randeff) %in% c("groups", "name", "var", "sd"))] <- sapply(which(!colnames(print.object$randeff) %in% c("groups", "name", "var", "sd")), function(y) ifelse(!is.na(print.object$randeff[, y]), formatC(print.object$randeff[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

      # Two-Level Model
      if (isTRUE(model.twolevel)) {

        # Lower triangular
        print.object$randeff.cor <- print.object$randeff[-nrow(print.object$randeff), setdiff(colnames(print.object$randeff), c("groups", "name", "var", "sd"))]
        print.object$randeff.cor[upper.tri(print.object$randeff.cor)] <- NA

        # Diagonal
        diag(print.object$randeff.cor) <- ""

        # Combine variance and correlation components
        print.object$randeff <- cbind(print.object$randeff[, c("groups", "name", "var", "sd")], rbind(print.object$randeff.cor, rep(NA, times = ncol(print.object$randeff.cor))))

        # Columns
        print.object$randeff <- data.frame(rbind(c("Groups", "Name", "Var", "SD", "Cor", setdiff(colnames(print.object$randeff), c("groups", "name", "var", "sd", "cor"))), print.object$randeff), fix.empty.names = FALSE, row.names = NULL)

        # Remove last column
        print.object$randeff <- print.object$randeff[, -ncol(print.object$randeff)]

      } else {

        # Extract correlation components
        print.object$randeff.cor <- which(print.object$randeff$groups %in% names(lme4::getME(x$model, name = "cnms")))[2L] |> (\(y) list(print.object$randeff[1L:(y - 1L), setdiff(colnames(print.object$randeff), c("groups", "name", "var", "sd"))], print.object$randeff[y:(nrow(print.object$randeff) - 1L), setdiff(colnames(print.object$randeff), c("groups", "name", "var", "sd"))]))()

        for (i in seq_along(print.object$randeff.cor)) {


          # Lower triangular
          print.object$randeff.cor[[i]][upper.tri(print.object$randeff.cor[[i]])] <- NA

          # Diagonal
          diag(print.object$randeff.cor[[i]]) <- ""

        }

        # Combine variance and correlation components
        print.object$randeff <- cbind(print.object$randeff[, c("groups", "name", "var", "sd")], rbind(do.call("rbind", print.object$randeff.cor), rep(NA, times = unique(lapply(print.object$randeff.cor, ncol)))))

        # Columns
        print.object$randeff <- data.frame(rbind(c("Groups", "Name", "Var", "SD", "Cor", setdiff(colnames(print.object$randeff), c("groups", "name", "var", "sd", "cor"))), print.object$randeff), fix.empty.names = FALSE, row.names = NULL)

        # Remove last column
        print.object$randeff <- print.object$randeff[, -ncol(print.object$randeff)]

      }

    }

    # Justify right and left
    print.object$randeff[, -c(1L, 2L)] <- format(print.object$randeff[, -c(1L, 2L)], justify = "right")
    print.object$randeff[, c(1L, 2L)] <- format(print.object$randeff[, c(1L, 2L)], justify = "left")

    # Replace NA with ""
    print.object$randeff <- apply(print.object$randeff, 2L, function(y) gsub("NA", "  ", y))

    # Add blank space
    print.object$randeff[, 1L] <- paste0("   ", print.object$randeff[, 1L] , sep = "")

    # Print
    .write.table(print.object$randeff, left = 2L, right = 4L, horiz = horiz)

    #...................
    ### Fixed Effects ####

    cat("\n  Fixed Effects: Unstandardized Coefficients", "\n\n")

    # Coefficient table without dfs and p-values
    if (isTRUE(!"p" %in% colnames(print.object$coef))) {

      # Round variables
      print.object$coef[, c("Estimate", "SE", "t")] <- sapply(colnames(print.object$coef), function(y) ifelse(!is.na(print.object$coef[, y]), formatC(print.object$coef[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

      # Columns
      print.object$coef <- data.frame(row.names(print.object$coef), print.object$coef, fix.empty.names = FALSE, row.names = NULL)

      # Row names
      print.object$coef <- rbind(c("", colnames(print.object$coef)[-1L]), print.object$coef)

    # Coefficient table with dfs and p-values
    } else {

      # Round variables
      print.object$coef[, setdiff(colnames(print.object$coef), "p")] <- sapply(setdiff(colnames(print.object$coef), "p"), function(y) ifelse(!is.na(print.object$coef[, y]), formatC(print.object$coef[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))
      print.object$coef[, "p"] <- formatC(print.object$coef[, "p"], digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

      # Columns
      print.object$coef <- data.frame(row.names(print.object$coef), print.object$coef, fix.empty.names = FALSE, row.names = NULL)

      # Row names
      print.object$coef <- rbind(c("", colnames(misty::df.rename(print.object$coef, from = "p", to = "p"))[-1L]), print.object$coef)

    }

    # Justify right and left
    print.object$coef[, -1L] <- format(print.object$coef[, -1L], justify = "right")
    print.object$coef[, 1L] <- format(print.object$coef[, 1L], justify = "left")

    # Replace NA with ""
    print.object$coef[, -1L] <- apply(print.object$coef[, -1L], 2L, function(y) gsub("NA", "  ", y))

    # Add blank space
    print.object$coef[, 1L] <- paste0("   ", print.object$coef[, 1L] , sep = "")

    # Print
    .write.table(print.object$coe, left = 2L, right = 4L, horiz = horiz)

    #...................
    ### Weights ####

    #### Two-Level Model ####
    if (isTRUE(model.twolevel)) {

      ##### Residuals

      cat("\n  Robustness Weights: Residuals", "\n")

      if (isTRUE(print.object$weights$resid$ew0 != 0)) {

        if (isTRUE(print.object$weights$resid$ew0 >= 2)) {

          cat(paste0("   ", print.object$weights$resid$ew1, " weights are 1, ", print.object$weights$resid$ew0, " weights with M = ", formatC(print.object$weights$resid$pdescript[, "m"], digits = digits, format = "f"), " (SD = ", formatC(print.object$weights$resid$pdescript[, "sd"], digits = digits, format = "f"), ", Min = ", formatC(print.object$weights$resid$pdescript[, "min"], digits = digits, format = "f"), ", and Max = ", formatC(print.object$weights$resid$pdescript[, "max"], digits = digits, format = "f"), ")"))

        } else {

          cat(paste0("   ", print.object$weights$resid$ew1, " weights are 1, remaining", print.object$weights$resid$ew0, " weight is ", formatC(print.object$weights$resid$pdescript, digits = digits, format = "f")))

        }

      } else {

        cat(paste0("   All ", print.object$weights$resid$ew1, " weights are 1"))

      }

      ##### Random Effects

      cat("\n\n  Robustness Weights: Random Effects", "\n")

      if (isTRUE(print.object$weights$ranef$bw0 != 0)) {

        # More than two weights < 1
        if (isTRUE(print.object$weights$ranef$bw0 >= 2)) {

          cat(paste0("   ", print.object$weights$ranef$bw1, " weights are 1, ", print.object$weights$ranef$bw0, " weights with M = ", formatC(print.object$weights$ranef$bdescript[, "m"], digits = digits, format = "f"), " (SD = ", formatC(print.object$weights$ranef$bdescript[, "sd"], digits = digits, format = "f"), ", Min = ", formatC(print.object$weights$ranef$bdescript[, "min"], digits = digits, format = "f"), ", and Max = ", formatC(print.object$weights$ranef$bdescript[, "max"], digits = digits, format = "f"), ")"))

        # One weight < 1
        } else {

          cat(paste0("   ", print.object$weights$ranef$bw1, " weights are 1, remaining ", print.object$weights$ranef$bw0, " weight is ", formatC(print.object$weights$ranef$bdescript, digits = digits, format = "f")))

        }

      } else {

        cat(paste0("   All ", print.object$weights$ranef$bw1, " weights are 1"))

      }

    #### Three-Level Model ####
    } else {

      ##### Residuals

      cat("\n  Robustness Weights: Residuals", "\n")

      if (isTRUE(print.object$weights$resid$ew0 != 0)) {

        # More than two weights < 1
        if (isTRUE(print.object$weights$resid$ew0 >= 2)) {

          cat(paste0("   ", print.object$weights$resid$ew1, " weights are 1, ", print.object$weights$resid$ew0, " weights with M = ", formatC(print.object$weights$resid$pdescript[, "m"], digits = digits, format = "f"), " (SD = ", formatC(print.object$weights$resid$pdescript[, "sd"], digits = digits, format = "f"), ", Min = ", formatC(print.object$weights$resid$pdescript[, "sd"], digits = digits, format = "f"), ", and Max = ", formatC(print.object$weights$resid$pdescript[, "max"], digits = digits, format = "f"), ")"))

        # One weight < 1
        } else {

          cat(paste0("   ", print.object$weights$resid$ew1, " weights are 1, remaining ", print.object$weights$resid$ew0, " weight is ", formatC(print.object$weights$resid$pdescript, digits = digits, format = "f")))

        }

      } else {

        cat(paste0("   All ", print.object$weights$resid$ew1, " weights are 1"))

      }

      ##### Random Effects: Level 1

      cat(paste0("\n\n  Robustness Weights: Random Effects '", names(lme4::getME(x$model, "w_b"))[1L], "'"), "\n")

      if (isTRUE(print.object$weights$ranef1$b1w0 != 0)) {

        # More than two weights < 1
        if (isTRUE(print.object$weights$ranef1$b1w0 >= 2)) {

          cat(paste0("   ", print.object$weights$ranef1$b1w1, " weights are 1, ", print.object$weights$ranef1$b1w0, " weights with M = ", formatC(print.object$weights$ranef1$b1descript[, "m"], digits = digits, format = "f"), " (SD = ", formatC(print.object$weights$ranef1$b1descript[, "sd"], digits = digits, format = "f"), ", Min = ", formatC(print.object$weights$ranef1$b1descript[, "sd"], digits = digits, format = "f"), ", and Max = ", formatC(print.object$weights$ranef1$b1descript[, "max"], digits = digits, format = "f"), ")"))

        # One weight < 1
        } else {

          cat(paste0("   ", print.object$weights$ranef1$b1w1, " weights are 1, remaining ", print.object$weights$ranef1$b1w0, " weight is ", formatC(print.object$weights$ranef1$b1descript, digits = digits, format = "f")))

        }

      } else {

        cat(paste0("   All ", print.object$weights$ranef1$b1w1, " weights are 1"))

      }

      ##### Random Effects: Level 2

      cat(paste0("\n\n  Robustness Weights: Random Effects '", names(lme4::getME(x$model, "w_b"))[2L], "'"), "\n")

      if (isTRUE(print.object$weights$ranef1$b2w0 != 0)) {

        # More than two weights < 1
        if (isTRUE(print.object$weights$ranef2$b2w0 >= 2)) {

          cat(paste0("   ", print.object$weights$ranef2$b2w1, " weights are 1, ", print.object$weights$ranef2$b2w0, " weights with M = ", formatC(print.object$weights$ranef2$b2descript[, "m"], digits = digits, format = "f"), " (SD = ", formatC(print.object$weights$ranef2$b2descript[, "sd"], digits = digits, format = "f"), ", Min = ", formatC(print.object$weights$ranef2$b2descript[, "sd"], digits = digits, format = "f"), ", and Max = ", formatC(print.object$weights$ranef2$b2descript[, "max"], digits = digits, format = "f"), ")"))

        # One weight < 1
        } else {

          cat(paste0("   ", print.object$weights$ranef1$b1w1, " weights are 1, remaining ", print.object$weights$ranef2$b2w0, " weight is ", formatC(print.object$weights$ranef2$b2descript, digits = digits, format = "f")))

        }

      } else {

        cat(paste0("   All ", print.object$weights$ranef2$b2w1, " weights are 1"))

      }

    }

    #...................
    ### Model Convergence ####

    # -1 = not converged, 0 = singular, 1 = model converged
    switch(as.character(print.object$converg),
           "-1" = { cat("\n\n Warning. Model failed to converge.", "\n") },
           "0" = { cat("\n\n Warning. Fitted model is singular, see help page: ?isSingular", "\n")})

  #_____________________________________________________________________________
  #
  # Sample Size Determination, size() ------------------------------------------
  }, size = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Arithmetic Mean ####

    switch(x$size, mean = {

      cat(" Sample Size Determination:", ifelse(x$args$sample == "one.sample", "One-Sample", "Two-Sample"), "t-Test\n\n")

      #...................
      ### R Markdown in progress ####

      if (isTRUE(getOption("knitr.in.progress"))) {

        #### One-Sample ####
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

        #### Two-Sample ####
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

        cat("  alpha =", x$args$alpha, " beta =", x$args$beta, " gamma =", x$args$delta, "\n\n")

        if (isTRUE(x$args$sample == "one.sample")) {

          cat("  optimal sample size: n =", ceiling(x$result$n), "\n")

        } else {

          cat("  optimal sample size: n =", ceiling(x$result$n), "in each group \n")

        }

      #...................
      ### R Markdown not in progress ####

      } else {

        #### One-Sample ####
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

        #### Two-Sample ####
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

        cat("  \u03B1 =", x$args$alpha, " \u03B2 =", x$args$beta, " \u03B4 =", x$args$delta, "\n\n")

        if (isTRUE(x$args$sample == "one.sample")) {

          cat("  optimal sample size: n =", ceiling(x$result$n), "\n")

        } else {

          cat("  optimal sample size: n =", ceiling(x$result$n), "in each group \n")

        }

      }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Proportions ####

    }, prop = {

      if (isTRUE(x$args$correct)) {

        cat(" Sample Size Determination:", ifelse(x$args$sample == "one.sample", "One-Sample", "Two-Sample"), "Proportion Test with Continuity Correction\n\n")

      } else {

        cat(" Sample Size Determination:", ifelse(x$args$sample == "one.sample", "One-Sample", "Two-Sample"), "Proportion Test without Continuity Correction\n\n")

      }

      #...................
      ### R Markdown in progress ####

      if (isTRUE(getOption("knitr.in.progress"))) {

        #### One-Sample ####
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

        #### Two-Sample ####
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

        if (isTRUE(x$args$sample == "one.sample")) {

          cat("  optimal sample size: n =", ceiling(x$result$n), "\n")

        } else {

          cat("  optimal sample size: n =", ceiling(x$result$n), "in each group \n")

        }

      #...................
      ### R Markdown not in progress ####

      } else {

        #### One-Sample ####
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

        #### Two-Sample ####
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

        cat("  \u03B1 =", x$args$alpha, " \u03B2 =", x$args$beta, " \u03B4 =", x$args$delta, "\n\n")

        if (isTRUE(x$args$sample == "one.sample")) {

          cat("  optimal sample size: n =", ceiling(x$result$n), "\n")

        } else {

          cat("  optimal sample size: n =", ceiling(x$result$n), "in each group \n")

        }

      }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Correlation Coefficient ####

    }, cor = {

      cat(" Sample Size Determination: Pearson's Product-Moment Correlation Coefficient\n\n")

      #...................
      ### R Markdown in progress ####

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

        cat("  optimal sample size: n =", ceiling(x$result$n), "\n")

      #...................
      ### R Markdown not in progress ####

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

        cat("  optimal sample size: n =", ceiling(x$result$n), "\n")

      }

    })

  #_____________________________________________________________________________
  #
  # Multivariate Skewness, skewness() ------------------------------------------
  }, skewness = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Round ####

    print.object[, c("skew", "chi2")] <- sapply(c("skew", "chi2"), function(y) ifelse(!is.na(print.object[, y]), formatC(print.object[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

    print.object$pval <- formatC(print.object$pval, digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Row Names ####

    print.object <- rbind(c("n", "Var", "Skew", "Chi2", "df", "p"), print.object)

    # R Markdown in progress
    if (isTRUE(is.null(getOption("knitr.in.progress")))) { print.object[1L, "chi2"] <- "\u03C7\u00B2" }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Format ####

    # Justify right
    print.object <- apply(print.object, 2L, function(y) format(y, justify = "right"))

    # Add blank space
    print.object[, "n"] <- paste0("  ", print.object[, "n"])

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Print Output ####

    cat(" Multivariate Skewness\n\n")

    .write.table(print.object, left = 1L, right = 3L, horiz = horiz)

  #_____________________________________________________________________________
  #
  # Print Summary Output, summa() ----------------------------------------------
  }, summa = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Linear Regression, lm() ####

    if (isTRUE(all(class(x$model) == "lm"))) {

      #--------------------------------------
      ### Title ####

      cat(" Linear Regression\n\n")

      #--------------------------------------
      ### Call ####

      if (isTRUE("call" %in% print && !is.null(print.object$call))) { cat("  Formula: ", print.object$call$formula, "\n", " Data:    ", print.object$call$data, "\n") }

      #--------------------------------------
      ### Descriptive Statistics ####

      if (isTRUE("descript" %in% print && !is.null(print.object$descript))) {

        if (isTRUE("call" %in% print)) { cat("\n") }

        # Header
        cat("  Descriptive Statistics", "\n\n")

        # Round variables
        print.object$descript[, c("m", "sd", "min", "p.min", "max", "p.max", "skew", "kurt")] <- sapply(c("m", "sd", "min", "p.min", "max", "p.max", "skew", "kurt"), function(y) ifelse(!is.na(print.object$descript[, y]), formatC(print.object$descript[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

        # Percentages
        print.object$descript[, "p.min"] <- paste0(print.object$descript[, "p.min"], "%")
        print.object$descript[, "p.max"] <- paste0(print.object$descript[, "p.max"], "%")

        # Row names
        print.object$descript <- rbind(c("Variable", "n", "nUQ", "M", "SD", "Min", "%Min", "Max", "%Max", "Skew", "Kurt"), print.object$descript)

        # Justify left and right
        print.object$descript[, 1L] <- format(print.object$descript[, 1L, drop = FALSE], justify = "left")
        print.object$descript[, -1L] <- apply(print.object$descript[, -1L, drop = FALSE], 2L, function(y) format(y, justify = "right"))

        # Add blank space
        print.object$descript[, "variable"] <- c(paste0("   ", print.object$descript[1L, "variable"], collapse = ""), paste0("    ", print.object$descript[-1L, "variable"]))
        print.object$descript[, "variable"] <- format(c(print.object$descript[1L, "variable"], misty::chr.trim(print.object$descript[-1L, "variable"], side = "right")), justify = "left")

        # Print
        .write.table(print.object$descript, left = 2, right = 4L, horiz = horiz)

      }

      #--------------------------------------
      ### Correlation Matrix ####

      if (isTRUE("cormat" %in% print && !is.null(print.object$cormat))) {

        if (isTRUE(any(c("call", "descript") %in% print))) { cat("\n") }

        # Header
        cat("  Pearson Product-Moment Correlation Coefficient", "\n\n")

        # Round and format
        print.object$cormat <- formatC(print.object$cormat, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

        # Diagonal
        diag(print.object$cormat) <- ""

        # Lower triangular
        print.object$cormat[upper.tri(print.object$cormat)] <- ""

        # Row names
        print.object$cormat <- cbind(paste0("   ", row.names(print.object$cormat)), print.object$cormat)

        # Column names
        print.object$cormat <- rbind(colnames(print.object$cormat), print.object$cormat)

        # Justify right
        print.object$cormat[, 1L] <- format(print.object$cormat[, 1L], justify = "left")
        print.object$cormat[, -1L] <- format(print.object$cormat[, -1L], justify = "right")

        # Print
        .write.table(print.object$cormat, left = 2L, right = 4L, horiz = horiz)

      }

      #--------------------------------------
      ### Model Summary ####

      if (isTRUE("modsum" %in% print && !is.null(print.object$modsum))) {

        if (isTRUE(any(c("call", "descript", "cormat") %in% print))) { cat("\n") }

        # Model with predictors
        if (all(!is.na(print.object$modsum))) {

          # Header
          if (isTRUE(x$args$robust)) {

            cat("  Multiple Correlation, R-Squared, and Robust F-Test", "\n\n")

          } else {

            cat("  Multiple Correlation, R-Squared, and F-Test", "\n\n")

          }

          # Round variables
          print.object$modsum[, c("R", "R2", "R2.adj", "p")] <- sapply(c("R", "R2", "R2.adj", "p"), function(y) ifelse(!is.na(print.object$modsum[, y]), formatC(print.object$modsum[, y], digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0")), NA))
          print.object$modsum[, "F"] <- formatC(print.object$modsum[, "F"], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

          # Row names
          # R Markdown not in progress
          if (isTRUE(is.null(getOption("knitr.in.progress")))) {

            print.object$modsum <- rbind(c("n", "nNA", "R", "R\U00B2", "adjR\U00B2", "df\U2081", "df\U2082", "F", "p"), print.object$modsum)

          # R Markdown in progress
          } else {

            print.object$modsum <- rbind(c("n", "nNA", "R", "R2", "adjR2", "df1", "df2", "F", "p"), print.object$modsum)

          }

        # Null model
        } else {

          # Header
          cat("  Multiple Correlation and R-Squared", "\n\n")

          # Row names
          # R Markdown not in progress
          if (isTRUE(is.null(getOption("knitr.in.progress")))) {

            print.object$modsum <- rbind(c("n", "nNA", "R", "R\U00B2", "adjR\U00B2"), print.object$modsum[, c("n", "nNA", "R", "R2", "R2.adj")])

            # R Markdown in progress
          } else {

            print.object$modsum <- rbind(c("n", "nNA", "R", "R2", "adjR2"), print.object$modsum[, c("n", "nNA", "R", "R2", "R2.adj")])

          }

        }

        # Justify right
        print.object$modsum <- format(print.object$modsum, justify = "right")

        # Add blank space
        print.object$modsum[, "n"] <- paste0("   ", print.object$modsum[, "n"] , sep = "")

        # Print
        .write.table(print.object$modsum, left = 2L, right = 4L, horiz = horiz)

      }

      #--------------------------------------
      ### Coefficients ####

      if (isTRUE("coef" %in% print && !is.null(print.object$coef))) {

        if (isTRUE(any(c("call", "descript", "cormat", "modsum") %in% print))) { cat("\n") }

        # Header
        if (isTRUE(all(!print %in% c("confint", "stdcoef", "vif")))) {

          cat("  Unstandardized Coefficients", "\n\n")

        } else if (isTRUE(all(!print %in% c("stdcoef", "vif")))) {

          cat(paste0("  Unstandardized Coefficients and ", round(x$args$conf.level * 100L, digits = 2L), "% Confidence Interval"), "\n")

        } else if (isTRUE(all(!print %in% c("confint", "vif")))) {

          cat("  Unstandardized and Standardized Coefficients", "\n\n")

        } else if (isTRUE(all(!print %in% c("confint", "stdcoef")))) {

          cat("  Unstandardized Coefficients and Variance Inflation Factor", "\n\n")

        } else if (isTRUE(all(!print %in% "vif"))) {

          cat(paste0("  Unstandardized and Standardized Coefficients and ", round(x$args$conf.level * 100L, digits = 2L), "% CI"), "\n\n")

        } else if (isTRUE(all(!print %in% "stdcoef"))) {

          cat(paste0("  Unstandardized Coefficients, ", round(x$args$conf.level * 100L, digits = 2L), "% CI and VIF"), "\n\n")

        } else if (isTRUE(all(!print %in% "confint"))) {

          cat("  Unstandardized and Standardized Coefficients and VIF", "\n\n")

        } else if (isTRUE(all(c("coef", "confint", "stdcoef", "vif") %in% print))) {

          cat(paste0("  Unstandardized and Standardized Coefficients ", round(x$args$conf.level * 100L, digits = 2L), "% CI and VIF"), "\n\n")

        }

        # Round variables
        print.object$coef[, setdiff(colnames(print.object$coef), c("df", "p"))] <- sapply(setdiff(colnames(print.object$coef), c("df", "p")), function(y) ifelse(!is.na(print.object$coef[, y]), formatC(print.object$coef[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))
        print.object$coef[, "p"] <- formatC(print.object$coef[, "p"], digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

        # Columns and spacing
        print.object$coef <- data.frame(row.names(print.object$coef), Estimate = print.object$coef[, 1L], do.call("cbind", lapply(print.object$coef[, -1L], function(y) paste0(" ", y))), fix.empty.names = FALSE, row.names = NULL)

        # Row names
        print.object$coef <- rbind(c("", colnames(print.object$coef)[-1L]), print.object$coef)

        # Justify right and left
        print.object$coef[, -1L] <- format(print.object$coef[, -1L], justify = "right")
        print.object$coef[, 1L] <- format(print.object$coef[, 1L], justify = "left")

        # Replace NA with " "
        print.object$coef[, -1L] <- apply(print.object$coef[, -1L], 2L, function(y) gsub("NA", "  ", y))

        # Add blank space
        print.object$coef[, 1L] <- paste0("   ", print.object$coef[, 1L] , sep = "")

        # Print
        .write.table(print.object$coef, left = 2L, right = 4L, horiz = horiz)

        if (isTRUE(x$args$robust)) { cat("\n  Note. Heteroscedasticity-Consistent Standard Errors (HC4)") }

      }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Linear Mixed-Effects Model, lmer() ####

    } else if (all(class(x$model) %in% c("lmerMod", "rlmerMod", "lmerModLmerTest"))) {

      # Two-level model
      model.twolevel <- ifelse(lme4::getME(x$model, name = "n_rtrms") == 1L, TRUE, FALSE)

      #--------------------------------------
      ### Title ####

      if (!inherits(x$model, what = "rlmerMod")) {

        cat(" Multilevel and Linear Mixed-Effects Model\n\n")

      } else {

        cat(" Robust Multilevel and Linear Mixed-Effects Model\n\n")

      }

      #--------------------------------------
      ### Call ####

      if (isTRUE("call" %in% print && !is.null(print.object$call))) {

        cat("  Formula: ", print.object$call$formula, "\n", " Data:    ", print.object$call$data, "\n")

        if (isTRUE(inherits(x$model, what = "rlmerMod"))) { cat("  Method:  ",x$model@method, "\n") }

      }

      #--------------------------------------
      ### Descriptive Statistics ####

      if (isTRUE("descript" %in% print && !is.null(print.object$descript))) {

        if (isTRUE("call" %in% print)) { cat("\n") }

        # Header
        cat("  Descriptive Statistics", "\n\n")

        # Round variables
        print.object$descript[, c("m", "sd", "min", "p.min", "max", "p.max", "skew", "kurt")] <- sapply(c("m", "sd", "min", "p.min", "max", "p.max", "skew", "kurt"), function(y) ifelse(!is.na(print.object$descript[, y]), formatC(print.object$descript[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

        # Percentages
        print.object$descript[, "p.min"] <- paste0(print.object$descript[, "p.min"], "%")
        print.object$descript[, "p.max"] <- paste0(print.object$descript[, "p.max"], "%")

        # Two-Level Model
        if (isTRUE(model.twolevel)) {

          # Round ICC(1)
          print.object$descript[, "icc"] <- formatC(print.object$descript[, "icc"], digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

          # Row names
          print.object$descript <- rbind(c("Variable", "n", "nUQ", "M", "SD", "Min", "%Min", "Max", "%Max", "Skew", "Kurt", "ICC(1)"), print.object$descript)

        # Three-Level Model
        } else {

          # Round ICC(1)
          print.object$descript[, c("icc.l2", "icc.l3")] <- sapply(c("icc.l2", "icc.l3"), function(y) ifelse(!is.na(print.object$descript[, y]), formatC(print.object$descript[, y], digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0")), NA))

          # Row names
          print.object$descript <- rbind(c("Variable", "n", "nUQ", "M", "SD", "Min", "%Min", "Max", "%Max", "Skew", "Kurt", "ICC(1)2", "ICC(1)3"), print.object$descript)

        }

        # Justify left and right
        print.object$descript[, 1L] <- format(print.object$descript[, 1L, drop = FALSE], justify = "left")
        print.object$descript[, -1L] <- apply(print.object$descript[, -1L, drop = FALSE], 2L, function(y) format(y, justify = "right"))

        # Replace NA with " "
        print.object$descript <- apply(print.object$descript, 2L, function(y) gsub("NA", "  ", y))

        # Add blank space
        print.object$descript[, "variable"] <- c(paste0("   ", print.object$descript[1L, "variable"], collapse = ""), paste0("    ", print.object$descript[-1L, "variable"]))
        print.object$descript[, "variable"] <- format(c(print.object$descript[1L, "variable"], misty::chr.trim(print.object$descript[-1L, "variable"], side = "right")), justify = "left")

        # Print
        .write.table(print.object$descript, left = 2L, right = 4L, horiz = horiz)

      }

      #--------------------------------------
      ### Correlation Matrix ####

      if (isTRUE("cormat" %in% print && !is.null(print.object$cormat))) {

        if (isTRUE(any(c("call", "descript") %in% print))) { cat("\n") }

        # Header
        cat("  Within-Group and Between-Group Correlation Matrix", "\n\n")

        # Round and format
        print.object$cormat <- formatC(print.object$cormat, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

        # Diagonal
        diag(print.object$cormat) <- ""

        # Row names
        print.object$cormat <- cbind(paste0("   ", row.names(print.object$cormat)), print.object$cormat)

        # Column names
        print.object$cormat <- rbind(colnames(print.object$cormat), print.object$cormat)

        # Justify right
        print.object$cormat[, 1L] <- format(print.object$cormat[, 1L], justify = "left")
        print.object$cormat[, -1L] <- format(print.object$cormat[, -1L], justify = "right")

        # Replace NA with " "
        print.object$cormat <- apply(print.object$cormat, 2L, function(y) gsub("NA", "  ", y))

        # Print
        .write.table(print.object$cormat, left = 2L, right = 4L, horiz = horiz)

      }

      #--------------------------------------
      ### Model Summary ####

      if (isTRUE("modsum" %in% print && !is.null(print.object$modsum))) {

        if (isTRUE(any(c("call", "descript", "cormat") %in% print))) { cat("\n") }

        # Header
        cat("  Model Summary, Marginal and Conditional R-Squared", "\n\n")

        # Round variables
        print.object$modsum[, c("margR2", "condR2")] <- sapply(c("margR2", "condR2"), function(y) ifelse(!is.na(print.object$modsum[, y]), formatC(print.object$modsum[, y], digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0")), NA))
        print.object$modsum[, c("loglik", "deviance")] <- sapply(c("loglik", "deviance"), function(y) ifelse(!is.na(print.object$modsum[, y]), formatC(print.object$modsum[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

        # Row names
        # Model summary with nNA
        if (isTRUE("nNA" %in% colnames(print.object$modsum))) {

          # R Markdown not in progress
          if (isTRUE(is.null(getOption("knitr.in.progress")))) {

            # Two- or Three-Level Model
            if (isTRUE(model.twolevel)) { print.object$modsum <- rbind(c("n", "nNA", "nCL", "nPar", "Method", "logLik", "Deviance", "margR\U00B2", "condR\U00B2"), print.object$modsum) } else { print.object$modsum <- rbind(c("n", "nNA", "nCL2", "nCL3", "nPar", "Method", "logLik", "Deviance", "margR\U00B2", "condR\U00B2"), print.object$modsum) }

          # R Markdown in progress
          } else {

            # Two- or Three-Level Model
            if (isTRUE(model.twolevel)) { print.object$modsum <- rbind(c("n", "nNA", "nCL", "nPar", "Method", "logLik", "Deviance", "margR2", "condR2"), print.object$modsum) } else { print.object$modsum <- rbind(c("n", "nNA", "nCL2", "nCL3", "nPar", "Method", "logLik", "Deviance", "margR2", "condR2"), print.object$modsum) }

          }

        # Model summary without nNA
        } else {

          # R Markdown not in progress
          if (isTRUE(is.null(getOption("knitr.in.progress")))) {

            # Two- or Three-Level Model
            if (isTRUE(model.twolevel)) { print.object$modsum <- rbind(c("n", "nCL", "nPar", "Method", "logLik", "Deviance", "margR\U00B2", "condR\U00B2"), print.object$modsum) } else { print.object$modsum <- rbind(c("n", "nCL2", "nCL3", "nPar", "Method", "logLik", "Deviance", "margR\U00B2", "condR\U00B2"), print.object$modsum) }


          # R Markdown in progress
          } else {

            # Two- or Three-Level Model
            if (isTRUE(model.twolevel)) { print.object$modsum <- rbind(c("n", "nCL", "nPar", "Method", "logLik", "Deviance", "margR2", "condR2"), print.object$modsum) } else { print.object$modsum <- rbind(c("n", "nCL2", "nCL3", "nPar", "Method", "logLik", "Deviance", "margR2", "condR2"), print.object$modsum) }

          }

        }

        # Justify right
        print.object$modsum <- format(print.object$modsum, justify = "right")

        # Add blank space
        print.object$modsum[, "n"] <- paste0("   ", print.object$modsum[, "n"] , sep = "")

        # Print
        .write.table(print.object$modsum, left = 2L, right = 4L, horiz = horiz)

      }

      #--------------------------------------
      ### Random Effects ####

      if (isTRUE("randeff" %in% print && !is.null(print.object$randeff))) {

        if (isTRUE(any(c("call", "descript", "cormat", "modsum") %in% print))) { cat("\n") }

        #...................
        #### Random Intercept Model ####

        if (isTRUE(length(misty::chr.omit(colnames(print.object$randeff), omit = c("groups", "name", "var", "sd", "cor"))) == 0L || (all(na.omit(print.object$randeff[, "cor"]) == 1L) && all(na.omit(print.object$randeff[, ncol(print.object$randeff)]) == 1L)))) {

          # Header
          cat("  Random Effects: Variance Components", "\n\n")

          print.object$randeff <- print.object$randeff[, -(grep("cor", colnames(print.object$randeff)):ncol(print.object$randeff))]

          # Round variables
          print.object$randeff[, c("var", "sd")] <- sapply(c("var", "sd"), function(y) ifelse(!is.na(print.object$randeff[, y]), formatC(print.object$randeff[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

          # Columns
          print.object$randeff <- data.frame(rbind(misty::rec(colnames(print.object$randeff), spec = "'groups' = 'Groups'; 'name' = 'Name'; 'var' = 'Var'; 'sd' = 'SD'; 'cor' = 'Cor'"), data.frame(print.object$randeff[, setdiff(colnames(print.object$randeff), c("var", "sd"))], do.call("cbind", lapply(print.object$randeff[, c("var", "sd")], function(y) paste0(" ", y))))), fix.empty.names = FALSE, row.names = NULL)

        #...................
        #### Random Intercept and Slope Model ####

        } else {

          # Header
          cat("  Random Effects: Variance and Correlation Components", "\n\n")

          # Round variables
          print.object$randeff[, c("var", "sd")] <- sapply(c("var", "sd"), function(y) ifelse(!is.na(print.object$randeff[, y]), formatC(print.object$randeff[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))
          print.object$randeff[, setdiff(colnames(print.object$randeff), c("groups", "name", "var", "sd"))] <- sapply(setdiff(colnames(print.object$randeff), c("groups", "name", "var", "sd")), function(y) ifelse(!is.na(print.object$randeff[, y]), formatC(print.object$randeff[, y], digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0")), NA))

          # Two-Level Model
          if (isTRUE(model.twolevel)) {

            # Lower triangular
            print.object$randeff.cor <- print.object$randeff[-nrow(print.object$randeff), setdiff(colnames(print.object$randeff), c("groups", "name", "var", "sd"))]
            print.object$randeff.cor[upper.tri(print.object$randeff.cor)] <- NA

            # Diagonal
            diag(print.object$randeff.cor) <- ""

            # Combine variance and correlation components
            print.object$randeff <- cbind(print.object$randeff[, c("groups", "name", "var", "sd")], rbind(print.object$randeff.cor, rep(NA, times = ncol(print.object$randeff.cor))))

            # Columns
            print.object$randeff <- data.frame(rbind(misty::rec(colnames(print.object$randeff), spec = "'groups' = 'Groups'; 'name' = 'Name'; 'var' = 'Var'; 'sd' = 'SD'; 'cor' = 'Cor'"), data.frame(print.object$randeff[, c("groups", "name")], do.call("cbind", lapply(print.object$randeff[, setdiff(colnames(print.object$randeff), c("groups", "name"))], function(y) paste0(" ", y))))), fix.empty.names = FALSE, row.names = NULL)

            # Remove last column
            print.object$randeff <- print.object$randeff[, -ncol(print.object$randeff)]

          # Three-Level Model
          } else {

            # Extract correlation components
            print.object$randeff.cor <- which(print.object$randeff$groups %in% names(lme4::getME(x$model, name = "cnms")))[2L] |> (\(y) list(print.object$randeff[1L:(y - 1L), setdiff(colnames(print.object$randeff), c("groups", "name", "var", "sd"))], print.object$randeff[y:(nrow(print.object$randeff) - 1L), setdiff(colnames(print.object$randeff), c("groups", "name", "var", "sd"))]))()

            for (i in seq_along(print.object$randeff.cor)) {

              # Lower triangular
              print.object$randeff.cor[[i]][upper.tri(print.object$randeff.cor[[i]])] <- NA

              # Diagonal
              diag(print.object$randeff.cor[[i]]) <- ""

            }

            # Combine variance and correlation components
            print.object$randeff <- cbind(print.object$randeff[, c("groups", "name", "var", "sd")], rbind(do.call("rbind", print.object$randeff.cor), rep(NA, times = unique(lapply(print.object$randeff.cor, ncol)))))

            # Columns
            print.object$randeff <- data.frame(rbind(misty::rec(colnames(print.object$randeff), spec = "'groups' = 'Groups'; 'name' = 'Name'; 'var' = 'Var'; 'sd' = 'SD'; 'cor' = 'Cor'"), data.frame(print.object$randeff[, c("groups", "name")], do.call("cbind", lapply(print.object$randeff[, setdiff(colnames(print.object$randeff), c("groups", "name"))], function(y) paste0(" ", y))))), fix.empty.names = FALSE, row.names = NULL)

            # Remove last column
            print.object$randeff <- print.object$randeff[, -ncol(print.object$randeff)]

          }

        }

        # Justify right and left
        print.object$randeff[, -c(1L, 2L)] <- format(print.object$randeff[, -c(1L, 2L)], justify = "right")
        print.object$randeff[, c(1L, 2L)] <- format(print.object$randeff[, c(1L, 2L)], justify = "left")

        # Replace NA with ""
        print.object$randeff <- apply(print.object$randeff, 2L, function(y) gsub("NA", "  ", y))

        # Add blank space
        print.object$randeff[, 1L] <- paste0("   ", print.object$randeff[, 1L] , sep = "")

        # Print
        .write.table(print.object$randeff, left = 2L, right = 4L, horiz = horiz)

      }

      #--------------------------------------
      ### Coefficients ####

      if (isTRUE(any(c("coef", "confint", "stdcoef", "vif") %in% print) && !is.null(print.object$coef))) {

        #...................
        #### Fixed Effects ####

        if (isTRUE(any(c("call", "descript", "cormat", "modsum", "randeff") %in% print))) { cat("\n") }

        # Header
        if (isTRUE(all(!print %in% c("confint", "stdcoef", "vif")))) {

          cat("  Fixed Effects: Unstandardized Coefficients", "\n\n")

        } else if (isTRUE(all(!print %in% c("stdcoef", "vif")))) {

          cat(paste0("  Fixed Effects: Unstandardized Coefficients and ", round(x$args$conf.level * 100L, digits = 2L), "% Confidence Interval"), "\n\n")

        } else if (isTRUE(all(!print %in% c("confint", "vif")))) {

          cat("  Fixed Effects: Unstandardized and Standardized Coefficients", "\n\n")

        } else if (isTRUE(all(!print %in% c("confint", "stdcoef")))) {

          cat("  Fixed Effects: Unstandardized Coefficients and Variance Inflation Factor", "\n\n")

        } else if (isTRUE(all(!print %in% "vif"))) {

          cat(paste0("  Fixed Effects: Unstandardized and Standardized Coefficients and ", round(x$args$conf.level * 100L, digits = 2L), "% CI"), "\n\n")

        } else if (isTRUE(all(!print %in% "stdcoef"))) {

          cat(paste0("  Fixed Effects: Unstandardized Coefficients, ", round(x$args$conf.level * 100L, digits = 2L), "% CI and VIF"), "\n\n")

        } else if (isTRUE(all(!print %in% "confint"))) {

          cat("  Fixed Effects: Unstandardized and Standardized Coefficients and VIF", "\n\n")

        } else if (isTRUE(all(c("coef", "confint", "stdcoef", "vif") %in% print))) {

          cat(paste0("  Fixed Effects: Unstandardized and Standardized Coefficients, ", round(x$args$conf.level * 100L, digits = 2L), "% CI and VIF"), "\n\n")

        }

        # Coefficient table without dfs and p-values
        if (isTRUE(!"p" %in% colnames(print.object$coef))) {

          # Round variables
          print.object$coef[, setdiff(colnames(print.object$coef), "Level")] <- sapply(setdiff(colnames(print.object$coef), "Level"), function(y) ifelse(!is.na(print.object$coef[, y]), formatC(print.object$coef[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

          # Columns and spacing
          print.object$coef <- data.frame(row.names(print.object$coef), Estimate = print.object$coef[, 1L], do.call("cbind", lapply(print.object$coef[, -1L], function(y) paste0(" ", y))), fix.empty.names = FALSE, row.names = NULL)

          # Row names
          print.object$coef <- rbind(c("", colnames(print.object$coef)[-1L]), print.object$coef)

        # Coefficient table with dfs and p-values
        } else {

          # Round variables
          print.object$coef[, setdiff(colnames(print.object$coef), c("p", "Level"))] <- sapply(setdiff(colnames(print.object$coef), c("p", "Level")), function(y) ifelse(!is.na(print.object$coef[, y]), formatC(print.object$coef[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))
          print.object$coef[, "p"] <- formatC(print.object$coef[, "p"], digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

          # Columns and spacing
          print.object$coef <- data.frame(row.names(print.object$coef), Estimate = print.object$coef[, 1L], do.call("cbind", lapply(print.object$coef[, -1L], function(y) paste0(" ", y))), fix.empty.names = FALSE, row.names = NULL)

          # Row names
          print.object$coef <- rbind(c("", colnames(misty::df.rename(print.object$coef, from = "p", to = "p"))[-1L]), print.object$coef)

        }

        # Justify right and left
        print.object$coef[, -1L] <- format(print.object$coef[, -1L], justify = "right")
        print.object$coef[, 1L] <- format(print.object$coef[, 1L], justify = "left")

        # Replace NA with ""
        print.object$coef[, -1L] <- apply(print.object$coef[, -1L], 2L, function(y) gsub("NA", "  ", y))

        # Add blank space
        print.object$coef[, 1L] <- paste0("   ", print.object$coef[, 1L] , sep = "")

        # Print
        .write.table(print.object$coef, left = 2L, right = 4L, horiz = horiz)

        if (isTRUE(x$args$robust)) { cat("\n  Note. Cluster-Robust Standard Errors (CR2)") }

      }

      #--------------------------------------
      ### Weights ####

      if (isTRUE(inherits(x$model, what = "rlmerMod"))) {

        #...................
        #### Two-Level Model ####

        if (isTRUE(model.twolevel)) {

          ##### Residuals

          cat("\n  Robustness Weights: Residuals", "\n")

          if (isTRUE(print.object$weights$resid$ew0 != 0)) {

            if (isTRUE(print.object$weights$resid$ew0 >= 2)) {

              cat(paste0("   ", print.object$weights$resid$ew1, " weights are 1, ", print.object$weights$resid$ew0, " weights with M = ", formatC(print.object$weights$resid$pdescript[, "m"], digits = digits, format = "f"), " (SD = ", formatC(print.object$weights$resid$pdescript[, "sd"], digits = digits, format = "f"), ", Min = ", formatC(print.object$weights$resid$pdescript[, "min"], digits = digits, format = "f"), ", and Max = ", formatC(print.object$weights$resid$pdescript[, "max"], digits = digits, format = "f"), ")"))

            } else {

              cat(paste0("   ", print.object$weights$resid$ew1, " weights are 1, remaining", print.object$weights$resid$ew0, " weight is ", formatC(print.object$weights$resid$pdescript, digits = digits, format = "f")))

            }

          } else {

            cat(paste0("   All ", print.object$weights$resid$ew1, " weights are 1"))

          }

          ##### Random Effects

          cat("\n\n  Robustness Weights: Random Effects", "\n")

          if (isTRUE(print.object$weights$ranef$bw0 != 0)) {

            # More than two weights < 1
            if (isTRUE(print.object$weights$ranef$bw0 >= 2)) {

              cat(paste0("   ", print.object$weights$ranef$bw1, " weights are 1, ", print.object$weights$ranef$bw0, " weights with M = ", formatC(print.object$weights$ranef$bdescript[, "m"], digits = digits, format = "f"), " (SD = ", formatC(print.object$weights$ranef$bdescript[, "sd"], digits = digits, format = "f"), ", Min = ", formatC(print.object$weights$ranef$bdescript[, "min"], digits = digits, format = "f"), ", and Max = ", formatC(print.object$weights$ranef$bdescript[, "max"], digits = digits, format = "f"), ")"))

            # One weight < 1
            } else {

              cat(paste0("   ", print.object$weights$ranef$bw1, " weights are 1, remaining ", print.object$weights$ranef$bw0, " weight is ", formatC(print.object$weights$ranef$bdescript, digits = digits, format = "f")))

            }

          } else {

            cat(paste0("   All ", print.object$weights$ranef$bw1, " weights are 1"))

          }

        #...................
        #### Three-Level Model ####

        } else {

          ##### Residuals

          cat("\n  Robustness Weights: Residuals", "\n")

          if (isTRUE(print.object$weights$resid$ew0 != 0)) {

            # More than two weights < 1
            if (isTRUE(print.object$weights$resid$ew0 >= 2)) {

              cat(paste0("   ", print.object$weights$resid$ew1, " weights are 1, ", print.object$weights$resid$ew0, " weights with M = ", formatC(print.object$weights$resid$pdescript[, "m"], digits = digits, format = "f"), " (SD = ", formatC(print.object$weights$resid$pdescript[, "sd"], digits = digits, format = "f"), ", Min = ", formatC(print.object$weights$resid$pdescript[, "sd"], digits = digits, format = "f"), ", and Max = ", formatC(print.object$weights$resid$pdescript[, "max"], digits = digits, format = "f"), ")"))

            # One weight < 1
            } else {

              cat(paste0("   ", print.object$weights$resid$ew1, " weights are 1, remaining ", print.object$weights$resid$ew0, " weight is ", formatC(print.object$weights$resid$pdescript, digits = digits, format = "f")))

            }

          } else {

            cat(paste0("   All ", print.object$weights$resid$ew1, " weights are 1"))

          }

          ##### Random Effects: Level 1

          cat(paste0("\n\n  Robustness Weights: Random Effects '", names(lme4::getME(x$model, "w_b"))[1L], "'"), "\n")

          if (isTRUE(print.object$weights$ranef1$b1w0 != 0)) {

            # More than two weights < 1
            if (isTRUE(print.object$weights$ranef1$b1w0 >= 2)) {

              cat(paste0("   ", print.object$weights$ranef1$b1w1, " weights are 1, ", print.object$weights$ranef1$b1w0, " weights with M = ", formatC(print.object$weights$ranef1$b1descript[, "m"], digits = digits, format = "f"), " (SD = ", formatC(print.object$weights$ranef1$b1descript[, "sd"], digits = digits, format = "f"), ", Min = ", formatC(print.object$weights$ranef1$b1descript[, "sd"], digits = digits, format = "f"), ", and Max = ", formatC(print.object$weights$ranef1$b1descript[, "max"], digits = digits, format = "f"), ")"))

            # One weight < 1
            } else {

              cat(paste0("   ", print.object$weights$ranef1$b1w1, " weights are 1, remaining ", print.object$weights$ranef1$b1w0, " weight is ", formatC(print.object$weights$ranef1$b1descript, digits = digits, format = "f")))

            }

          } else {

            cat(paste0("   All ", print.object$weights$ranef1$b1w1, " weights are 1"))

          }

          ##### Random Effects: Level 2

          cat(paste0("\n\n  Robustness Weights: Random Effects '", names(lme4::getME(x$model, "w_b"))[2L], "'"), "\n")

          if (isTRUE(print.object$weights$ranef1$b2w0 != 0)) {

            # More than two weights < 1
            if (isTRUE(print.object$weights$ranef2$b2w0 >= 2)) {

              cat(paste0("   ", print.object$weights$ranef2$b2w1, " weights are 1, ", print.object$weights$ranef2$b2w0, " weights with M = ", formatC(print.object$weights$ranef2$b2descript[, "m"], digits = digits, format = "f"), " (SD = ", formatC(print.object$weights$ranef2$b2descript[, "sd"], digits = digits, format = "f"), ", Min = ", formatC(print.object$weights$ranef2$b2descript[, "sd"], digits = digits, format = "f"), ", and Max = ", formatC(print.object$weights$ranef2$b2descript[, "max"], digits = digits, format = "f"), ")"))

            # One weight < 1
            } else {

              cat(paste0("   ", print.object$weights$ranef1$b1w1, " weights are 1, remaining ", print.object$weights$ranef2$b2w0, " weight is ", formatC(print.object$weights$ranef2$b2descript, digits = digits, format = "f")))

            }

          } else {

            cat(paste0("   All ", print.object$weights$ranef2$b2w1, " weights are 1"))

          }

        }

      }

      #--------------------------------------
      ### Model Convergence ####

      # -1 = not converged, 0 = singular, 1 = model converged
      switch(as.character(print.object$converg), "-1" = { cat("\n Warning. Model failed to converge.", "\n") }, "0" = { cat("\n Warning. Fitted model is singular, see help page: ?isSingular", "\n")})

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Linear Mixed-Effects Model, lme() ####

    } else if (isTRUE(all(class(x$model) %in% "lme"))) {

      # Two-level model
      model.twolevel <- ifelse(ncol(x$model$groups) == 1L, TRUE, FALSE)

      #--------------------------------------
      ### Title ####

      cat(" Multilevel and Linear Mixed-Effects Model\n\n")

      #--------------------------------------
      ### Call ####

      if (isTRUE("call" %in% print && !is.null(print.object$call))) {

        cat("  Formula: ", print.object$call$formula, "\n", " Data:    ", print.object$call$data, "\n")

        if (isTRUE(inherits(x$model, what = "rlmerMod"))) { cat("  Method:  ", x$model@method, "\n") }

      }

      #--------------------------------------
      ### Descriptive Statistics ####

      if (isTRUE("descript" %in% print && !is.null(print.object$descript))) {

        if (isTRUE("call" %in% print)) { cat("\n") }

        # Header
        cat("  Descriptive Statistics", "\n\n")

        # Round variables
        print.object$descript[, c("m", "sd", "min", "p.min", "max", "p.max", "skew", "kurt")] <- sapply(c("m", "sd", "min", "p.min", "max", "p.max", "skew", "kurt"), function(y) ifelse(!is.na(print.object$descript[, y]), formatC(print.object$descript[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

        # Percentages
        print.object$descript[, "p.min"] <- paste0(print.object$descript[, "p.min"], "%")
        print.object$descript[, "p.max"] <- paste0(print.object$descript[, "p.max"], "%")

        # Two-Level Model
        if (isTRUE(model.twolevel)) {

          # Round ICC(1)
          print.object$descript[, "icc"] <- formatC(print.object$descript[, "icc"], digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

          # Row names
          print.object$descript <- rbind(c("Variable", "n", "nUQ", "M", "SD", "Min", "%Min", "Max", "%Max", "Skew", "Kurt", "ICC(1)"), print.object$descript)

        # Three-Level Model
        } else {

          # Round ICC(1)
          print.object$descript[, c("icc.l2", "icc.l3")] <- sapply(c("icc.l2", "icc.l3"), function(y) ifelse(!is.na(print.object$descript[, y]), formatC(print.object$descript[, y], digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0")), NA))

          # Row names
          print.object$descript <- rbind(c("Variable", "n", "nUQ", "M", "SD", "Min", "%Min", "Max", "%Max", "Skew", "Kurt", "ICC(1)2", "ICC(1)3"), print.object$descript)

        }

        # Justify left and right
        print.object$descript[, 1L] <- format(print.object$descript[, 1L, drop = FALSE], justify = "left")
        print.object$descript[, -1L] <- apply(print.object$descript[, -1L, drop = FALSE], 2L, function(y) format(y, justify = "right"))

        # Replace NA with " "
        print.object$descript <- apply(print.object$descript, 2L, function(y) gsub("NA", "  ", y))

        # Add blank space
        print.object$descript[, "variable"] <- c(paste0("   ", print.object$descript[1L, "variable"], collapse = ""), paste0("    ", print.object$descript[-1L, "variable"]))
        print.object$descript[, "variable"] <- format(c(print.object$descript[1L, "variable"], misty::chr.trim(print.object$descript[-1L, "variable"], side = "right")), justify = "left")

        # Print
        .write.table(print.object$descript, left = 2L, right = 4L, horiz = horiz)

      }

      #--------------------------------------
      ### Correlation Matrix ####

      if (isTRUE("cormat" %in% print && !is.null(print.object$cormat))) {

        if (isTRUE(any(c("call", "descript") %in% print))) { cat("\n") }

        # Header
        cat("  Within-Group and Between-Group Correlation Matrix", "\n\n")

        # Round and format
        print.object$cormat <- formatC(print.object$cormat, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

        # Diagonal
        diag(print.object$cormat) <- ""

        # Row names
        print.object$cormat <- cbind(paste0("   ", row.names(print.object$cormat)), print.object$cormat)

        # Column names
        print.object$cormat <- rbind(colnames(print.object$cormat), print.object$cormat)

        # Justify right
        print.object$cormat[, 1L] <- format(print.object$cormat[, 1L], justify = "left")
        print.object$cormat[, -1L] <- format(print.object$cormat[, -1L], justify = "right")

        # Replace NA with " "
        print.object$cormat <- apply(print.object$cormat, 2L, function(y) gsub("NA", "  ", y))

        # Print
        .write.table(print.object$cormat, left = 2L, right = 4L, horiz = horiz)

      }

      #--------------------------------------
      ### Model Summary ####

      if (isTRUE("modsum" %in% print && !is.null(print.object$modsum))) {

        if (isTRUE(any(c("call", "descript", "cormat") %in% print))) { cat("\n") }

        # Header
        if (isTRUE(all(c("margR2", "condR2") %in% colnames(print.object$modsum)))) {

            cat("  Model Summary, Marginal and Conditional R-Squared", "\n\n")

        } else {

          cat("  Model Summary", "\n\n")

        }

        # Round variables
        if (isTRUE(all(c("margR2", "condR2") %in% colnames(print.object$modsum)))) { print.object$modsum[, c("margR2", "condR2")] <- sapply(c("margR2", "condR2"), function(y) ifelse(!is.na(print.object$modsum[, y]), formatC(print.object$modsum[, y], digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0")), NA)) }
        print.object$modsum[, c("loglik", "deviance")] <- sapply(c("loglik", "deviance"), function(y) ifelse(!is.na(print.object$modsum[, y]), formatC(print.object$modsum[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

        # Row names
        # Model summary with nNA
        if (isTRUE("nNA" %in% colnames(print.object$modsum))) {

          # Two-level model
          if (isTRUE(all(c("margR2", "condR2") %in% colnames(print.object$modsum)))) {

            # R Markdown not in progress
            if (isTRUE(is.null(getOption("knitr.in.progress")))) {

              if (isTRUE(model.twolevel)) { print.object$modsum <- rbind(c("n", "nNA", "nCL", "nPar", "Method", "logLik", "Deviance", "margR\U00B2", "condR\U00B2"), print.object$modsum) } else { print.object$modsum <- rbind(c("n", "nNA", "nCL2", "nCL3", "nPar", "Method", "logLik", "Deviance", "margR\U00B2", "condR\U00B2"), print.object$modsum) }

            # R Markdown in progress
            } else {

              if (isTRUE(model.twolevel)) { print.object$modsum <- rbind(c("n", "nNA", "nCL", "nPar", "Method", "logLik", "Deviance", "margR2", "condR2"), print.object$modsum) } else { print.object$modsum <- rbind(c("n", "nNA", "nCL2", "nCL3", "nPar", "Method", "logLik", "Deviance", "margR2", "condR2"), print.object$modsum) }

            }

          # Three-level model
          } else {

            print.object$modsum <- rbind(c("n", "nNA", "nCL2", "nCL3", "nPar", "Method", "logLik", "Deviance"), print.object$modsum)

          }

        }

        # Justify right
        print.object$modsum <- format(print.object$modsum, justify = "right")

        # Add blank space
        print.object$modsum[, "n"] <- paste0("   ", print.object$modsum[, "n"] , sep = "")

        # Print
        .write.table(print.object$modsum, left = 2L, right = 4L, horiz = horiz)

      }

      #--------------------------------------
      ### Random Effects ####

      if (isTRUE("randeff" %in% print && !is.null(print.object$randeff))) {

        if (isTRUE(any(c("call", "descript", "cormat", "modsum") %in% print))) { cat("\n") }

        #...................
        #### Random Intercept Model ####

        if (isTRUE(length(misty::chr.omit(colnames(print.object$randeff), omit = c("groups", "name", "var", "sd"))) == 0L)) {

          # Header
          cat("  Random Effects: Variance Components", "\n\n")

          # Round variables
          print.object$randeff[, c("var", "sd")] <- sapply(c("var", "sd"), function(y) ifelse(!is.na(print.object$randeff[, y]), formatC(print.object$randeff[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

          # Columns
          print.object$randeff <- data.frame(rbind(misty::rec(colnames(print.object$randeff), spec = "'groups' = 'Groups'; 'name' = 'Name'; 'var' = 'Var'; 'sd' = 'SD'; 'cor' = 'Cor'"), data.frame(print.object$randeff[, c("groups", "name")], do.call("cbind", lapply(print.object$randeff[, c("var", "sd")], function(y) paste0(" ", y))))), fix.empty.names = FALSE, row.names = NULL)

        #...................
        #### Random Intercept and Slope Model ####

        } else {

          # Header
          cat("  Random Effects: Variance and Correlation Components", "\n\n")

          # Round variables
          print.object$randeff[, c("var", "sd")] <- sapply(c("var", "sd"), function(y) ifelse(!is.na(print.object$randeff[, y]), formatC(print.object$randeff[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))
          print.object$randeff[, which(!colnames(print.object$randeff) %in% c("groups", "name", "var", "sd"))] <- sapply(which(!colnames(print.object$randeff) %in% c("groups", "name", "var", "sd")), function(y) ifelse(!is.na(print.object$randeff[, y]), formatC(print.object$randeff[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

          # Columns
          print.object$randeff <- data.frame(rbind(c("Groups", "Name", "Var", "SD", "Cor", setdiff(colnames(print.object$randeff), c("groups", "name", "var", "sd", "cor"))), data.frame(print.object$randeff[, c("groups", "name")], do.call("cbind", lapply(print.object$randeff[, setdiff(colnames(print.object$randeff), c("groups", "name"))], function(y) paste0(" ", y))))), fix.empty.names = FALSE, row.names = NULL)

        }

        # Justify right and left
        print.object$randeff[, -c(1L, 2L)] <- format(print.object$randeff[, -c(1L, 2L)], justify = "right")
        print.object$randeff[, c(1L, 2L)] <- format(print.object$randeff[, c(1L, 2L)], justify = "left")

        # Replace NA with ""
        print.object$randeff <- apply(print.object$randeff, 2L, function(y) gsub("NA", "  ", y))

        # Add blank space
        print.object$randeff[, 1L] <- paste0("   ", print.object$randeff[, 1L] , sep = "")

        # Print
        .write.table(print.object$randeff, left = 2L, right = 4L, horiz = horiz)

      }

      #--------------------------------------
      ### Variance and Correlation Structure ####

      if (isTRUE("varcor" %in% print && !is.null(print.object$varcor))) {

        #...................
        #### Correlation Structure ####

        if (isTRUE(!is.null(print.object$varcor$corstruct))) {

          # Extract correlational structure
          corstruct <- print.object$varcor$corstruct

          if (isTRUE(any(c("call", "descript", "cormat", "modsum", "randeff") %in% print))) { cat("\n") }

          cat(paste0("  Correlation Structure: ", corstruct$class), "\n\n")

          # Round and format
          corstruct$corstruct <- formatC(as.matrix(corstruct$corstruct), digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

          # Diagonal
          diag(corstruct$corstruct) <- ""

          # Upper diagonal
          corstruct$corstruct[upper.tri(corstruct$corstruct)] <- NA

          # Row names
          corstruct$corstruct <- cbind(paste0("   ", row.names(corstruct$corstruct)), corstruct$corstruct)

          # Column names
          corstruct$corstruct <- rbind(colnames(corstruct$corstruct), corstruct$corstruct)

          # Justify right
          corstruct$corstruct[, 1L] <- format(corstruct$corstruct[, 1L], justify = "left")
          corstruct$corstruct[, -1L] <- format(corstruct$corstruct[, -1L], justify = "right")

          # Replace NA with " "
          corstruct$corstruct <- apply(corstruct$corstruct, 2L, function(y) gsub("NA", "  ", y))

          # Print
          .write.table(corstruct$corstruct, left = 2L, right = 4L, horiz = horiz)

        }

        #...................
        #### Variance Function ####

        if (isTRUE(!is.null(print.object$varcor$varstruct))) {

          # Extract variance function
          varstruct <- print.object$varcor$varstruct

          if (isTRUE(any(c("call", "descript", "cormat", "modsum", "randeff") %in% print) || !is.null(print.object$varcor$corstruct))) { cat("\n") }

          cat(paste0("  Variance Function: ", varstruct$class))

          if (isTRUE(!grepl("varFixed", varstruct$class))) { cat("\n\n") } else { cat("\n") }

          if (isTRUE(!is.null(varstruct$varstruct))) {

            if (isTRUE(!grepl("varComb", varstruct$class))) {

              # Round and format
              varstruct$varstruct <- formatC(as.matrix(varstruct$varstruct), digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

              # Format
              varstruct$varstruct <- c(paste0("   ", varstruct$varstruct[1L]), varstruct$varstruct[-1L])

              # Column names
              varstruct$varstruct <- rbind(names(print.object$varcor$varstruct$varstruct), varstruct$varstruct)

              # Justify right
              varstruct$varstruct <- apply(varstruct$varstruct, 2L, format, justify = "right")

              # Print
              .write.table(varstruct$varstruct, left = 2L, right = 4L, horiz = horiz)

            # Combination of variance functions, varComb
            } else {

              # Round and format
              varstruct$varstruct <- lapply(varstruct$varstruct, function(y) formatC(as.matrix(y), digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0")))

              # Format
              varstruct$varstruct <- lapply(varstruct$varstruct, function(y) c(paste0("    ", y[1L]), y[-1L]))

              # Column names
              varstruct$varstruct <- lapply(seq_along(varstruct$varstruct), function(y) rbind(names(print.object$varcor$varstruct$varstruct[[y]]), varstruct$varstruct[[y]]))

              # Justify right
              varstruct$varstruct <- lapply(varstruct$varstruct, function(y) apply(y, 2L, format, justify = "right"))

              # Print
              for (i in seq_along(varstruct$varstruct)) {

                cat(paste0("   ", names(print.object$varcor$varstruct$varstruct)[i]), "\n\n")

                .write.table(varstruct$varstruct[i], left = 3L, right = 5L, horiz = horiz)

                if (isTRUE(i != length(varstruct$varstruct))) { cat("\n") }

              }

            }

          }

        }

      }

      #--------------------------------------
      ### Coefficients ####

      if (isTRUE(any(c("coef", "confint", "stdcoef", "vif") %in% print) && !is.null(print.object$coef))) {

        #...................
        #### Fixed Effects ####

        if (isTRUE(any(c("call", "descript", "cormat", "modsum", "varcor", "randeff") %in% print))) { cat("\n") }

        # Header
        if (isTRUE(all(!print %in% c("confint", "stdcoef", "vif")))) {

          cat("  Fixed Effects: Unstandardized Coefficients", "\n\n")

        } else if (isTRUE(all(!print %in% c("stdcoef", "vif")))) {

          cat(paste0("  Fixed Effects: Unstandardized Coefficients and ", round(x$args$conf.level * 100L, digits = 2L), "% Confidence Interval"), "\n\n")

        } else if (isTRUE(all(!print %in% c("confint", "vif")))) {

          cat("  Fixed Effects: Unstandardized and Standardized Coefficients", "\n\n")

        } else if (isTRUE(all(!print %in% c("confint", "stdcoef")))) {

          cat("  Fixed Effects: Unstandardized Coefficients and Variance Inflation Factor", "\n\n")

        } else if (isTRUE(all(!print %in% "vif"))) {

          cat(paste0("  Fixed Effects: Unstandardized and Standardized Coefficients and ", round(x$args$conf.level * 100L, digits = 2L), "% CI"), "\n\n")

        } else if (isTRUE(all(!print %in% "stdcoef"))) {

          cat(paste0("  Fixed Effects: Unstandardized Coefficients, ", round(x$args$conf.level * 100L, digits = 2L), "% CI and VIF"), "\n\n")

        } else if (isTRUE(all(!print %in% "confint"))) {

          cat("  Fixed Effects: Unstandardized and Standardized Coefficients and VIF", "\n\n")

        } else if (isTRUE(all(c("coef", "confint", "stdcoef", "vif") %in% print))) {

          cat(paste0("  Fixed Effects: Unstandardized and Standardized Coefficients, ", round(x$args$conf.level * 100L, digits = 2L), "% CI and VIF"), "\n\n")

        }

        # Round variables
        print.object$coef[, setdiff(colnames(print.object$coef), c("p", "Level"))] <- sapply(setdiff(colnames(print.object$coef), c("p", "Level")), function(y) ifelse(!is.na(print.object$coef[, y]), formatC(print.object$coef[, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))
        print.object$coef[, "p"] <- formatC(print.object$coef[, "p"], digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

        # Columns and spacing
        print.object$coef <- data.frame(row.names(print.object$coef), Estimate = print.object$coef[, 1L], do.call("cbind", lapply(print.object$coef[, -1L], function(y) paste0(" ", y))), fix.empty.names = FALSE, row.names = NULL)

        # Row names
        print.object$coef <- rbind(c("", colnames(misty::df.rename(print.object$coef, from = "p", to = "p"))[-1L]), print.object$coef)

        # Justify right and left
        print.object$coef[, -1L] <- format(print.object$coef[, -1L], justify = "right")
        print.object$coef[, 1L] <- format(print.object$coef[, 1L], justify = "left")

        # Replace NA with ""
        print.object$coef[, -1L] <- apply(print.object$coef[, -1L], 2L, function(y) gsub("NA", "  ", y))

        # Add blank space
        print.object$coef[, 1L] <- paste0("   ", print.object$coef[, 1L] , sep = "")

        # Print
        .write.table(print.object$coef, left = 2L, right = 4L, horiz = horiz)

        if (isTRUE(x$args$robust)) { cat("\n  Note. Cluster-Robust Standard Errors (CR2)") }

      }

      #--------------------------------------
      ### Model Convergence ####
      #
      # Info not available in the lme object

    }

  #_____________________________________________________________________________
  #
  # Levene's Test for Homogeneity of Variance, test.levene() -------------------
  }, test.levene = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Descript Object ####

    # Round
    print.object[["descript"]][, c("m", "sd", "var", "low", "upp", "skew", "kurt")] <- sapply(c("m", "sd", "var", "low", "upp", "skew", "kurt"), function(y) ifelse(!is.na(print.object[["descript"]][, y]), formatC(print.object[["descript"]][, y], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0")), NA))

    # Col names
    print.object[["descript"]] <- rbind(c("Group", "n", "nNA", "M", "SD", "Var", "Low", "Upp", "Skew", "Kurt"), print.object[["descript"]])

    # Format
    print.object[["descript"]][, 1L] <- format(print.object[["descript"]][, 1L], justify = "left")

    print.object[["descript"]][, -1L] <- apply(print.object[["descript"]][, -1L], 2L, format, justify = "right")

    print.object[["descript"]][1L, 1L] <- paste0(" ", print.object[["descript"]][1L, 1L], collapse = "")
    print.object[["descript"]][-1L, 1L] <- paste0("  ", print.object[["descript"]][-1L, 1L])

    print.object[["descript"]][, 1L] <- format(misty::chr.trim(print.object[["descript"]][, 1L], side = "right"), justify = "left")

    print.object[["descript"]][, 1L] <- paste("", print.object[["descript"]][, 1L])

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Test Object ####

    #...................
    ### Round ####

    print.object[["test"]][, "Sum Sq"] <- formatC(print.object[["test"]][, "Sum Sq"], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))
    print.object[["test"]][, "Mean Sq"] <- formatC(print.object[["test"]][, "Mean Sq"], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))
    print.object[["test"]][1L, "F value"] <- formatC(print.object[["test"]][1L, "F value"], digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))
    print.object[["test"]][1L, "Pr(>F)"] <- formatC(print.object[["test"]][1L, "Pr(>F)"], digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

    #...................
    ### Format ####

    print.object[["test"]] <- rbind(c("Df", "Sum Sq", "Mean Sq", "F", "p"), print.object[["test"]])
    print.object[["test"]] <- cbind(c("", "  Group", "  Residuals"), print.object[["test"]])

    print.object[["test"]][3L, c("F value", "Pr(>F)")] <- ""

    print.object[["test"]][, -1L] <- apply(print.object[["test"]][, -1L], 2L, format, justify = "right")
    print.object[["test"]][, 1L] <- format(print.object[["test"]][, 1L], justify = "left")

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Print Output ####

    cat(" Levene's Test based on the", switch(x$args$method, median = "Median\n\n", mean = "Arithmetic Mean\n\n"))

    #...................
    ### Hypotheses ####

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

      # R Markdown not in progress
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

    #...................
    ### Descriptive Statistics ####

    if (isTRUE(descript)) {

      .write.table(print.object[["descript"]], left = 1L, right = 3L, horiz = horiz)

      cat("\n")

    }

    .write.table(print.object[["test"]], left = 1L, right = 3L, horiz = horiz)

  #_____________________________________________________________________________
  #
  # t-test, test.t() -----------------------------------------------------------
  }, test.t = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## One-Sample t-Test ####

    switch(x$sample, one = {

      #...................
      ### Round ####

      print.object[, c("m", "sd", "m.diff", "se", "m.low", "m.upp", "t", "d", "d.low", "d.upp")] <- vapply(print.object[, c("m", "sd", "m.diff", "se", "m.low", "m.upp", "t", "d", "d.low", "d.upp")], formatC, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"), FUN.VALUE = character(1L))

      print.object[, "p"] <- formatC(print.object[, "p"], digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

      #...................
      ### Format ####

      print.object <- rbind(c("n", "nNA", "M", "SD", "M.Diff", "SE", "Low", "Upp", "t", "df", "p", "d", "Low", "Upp"), print.object)

      print.object <- apply(print.object, 2L, format, justify = "right")

      print.object[, 1L] <- paste(" ", print.object[, 1L])

      #...................
      ### Print Output ####

      cat(paste0(" One-Sample t-Test\n\n"))

      #### Hypotheses ####

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

        # R Markdown not in progress
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

      #### Cohen's d ####

      if (!isTRUE(effsize)) { print.object <- print.object[, -which(colnames(print.object) %in% c("d", "d.low", "d.upp"))] }

      #### Descriptive Statistics ####

      if (!isTRUE(descript)) {

        print.object <- print.object[, -which(colnames(print.object) %in% c("n", "nNA", "m", "sd", "m.diff", "se", "m.low", "m.upp"))]

        print.object[, 1L] <- paste(" ", print.object[, 1L])

      }

      .write.table(print.object, left = 1L, right = 3L, horiz = horiz)

      #### Note ####

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

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Two-Sample t-Test ####

    }, two = {

      #...................
      ### Round ####

      print.object[, c("m", "sd", "m.diff", "se", "m.low", "m.upp", "t", "d", "d.low", "d.upp")] <- apply(print.object[, c("m", "sd", "m.diff", "se", "m.low", "m.upp", "t", "d", "d.low", "d.upp")], 2L, formatC, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"))

      print.object[, "p"] <- formatC(print.object[, "p"], digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

      #...................
      ### Format ####

      print.object <- rbind(c("Group", "n", "nNA", "M", "SD", "M.Diff", "SE", "Low", "Upp", "t", "df", "p", "d", "Low", "Upp"), print.object)

      print.object[, -1L] <- apply(print.object[, -1L], 2L, format, justify = "right")

      print.object[-1L, 1L] <- paste0(" ", print.object[-1L, 1L])
      print.object[, 1L] <- format(print.object[, 1L], justify = "left")

      print.object[, 1L] <- paste(" ", print.object[, 1L])

      # NAs
      colnames.NA <- c("m.diff", "se", "m.low", "m.upp", "t", "df", "p", "d", "d.low", "d.upp")

      print.object[, which(colnames(print.object) %in% colnames.NA)] <- apply(print.object[, which(colnames(print.object) %in% colnames.NA), drop = FALSE], 2L, function(y) gsub("NA", "  ", y))

      #...................
      ### Print Output ####

      cat(paste0(" Two-Sample t-Test\n\n"))

      #### Hypotheses ####

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

        # R Markdown not in progress
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

      #### Cohen's d ####

      if (!isTRUE(effsize)) { print.object <- print.object[, -which(colnames(print.object) %in% c("d", "d.low", "d.upp"))] }

      #### Descriptive Statistics ####

      if (!isTRUE(descript)) {

        print.object <- print.object[-2L, -which(colnames(print.object) %in% c("group", "n", "nNA", "m", "sd", "m.diff", "m.low", "m.upp", "se"))]

        print.object[, 1L] <- paste(" ", print.object[, 1L])

      }

      .write.table(print.object, left = 1L, right = 3L, horiz = horiz)

      #### Note ####

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

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Paired-Sample t-Test ####

    }, paired = {

      #...................
      ### Round ####

      print.object[, c("m1", "m2", "m.diff", "sd.diff", "se", "m.low", "m.upp", "t", "d", "d.low", "d.upp")] <- vapply(print.object[, c("m1", "m2", "m.diff", "sd.diff", "se", "m.low", "m.upp", "t", "d", "d.low", "d.upp")], formatC, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"), FUN.VALUE = character(1L))

      print.object[, "p"] <- formatC(print.object[, "p"], digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

      #...................
      ### Format ####

      print.object <- rbind(c("n", "nNA", "M1", "M2", "M.Diff", "SD.Diff", "SE", "Low", "Upp", "t", "df", "p", "d", "Low", "Upp"), print.object)

      print.object <- apply(print.object, 2L, format, justify = "right")

      print.object[, 1L] <- paste(" ", print.object[, 1L])

      #...................
      ### Print Output ####

      cat(paste0(" Paired-Sample t-Test\n\n"))

      #### Hypotheses ####

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

        # R Markdown not in progress
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

      #### Cohen's d ####

      if (!isTRUE(effsize)) { print.object <- print.object[, -which(colnames(print.object) %in% c("d", "d.low", "d.upp"))] }

      #### Descriptive Statistics ####

      if (!isTRUE(descript)) { print.object <- print.object[, -which(colnames(print.object) %in% c("n", "nNA", "m1", "m2", "m.diff", "sd.diff", "se", "m.low", "m.upp"))] }

      .write.table(print.object, left = 1L, right = 3L, horiz = horiz)

      #### Note ####

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
  # Welch Test, test.welch() ---------------------------------------------------
  }, test.welch = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Welch t-Test ####

    switch(x$sample, two = {

      #...................
      ### Round ####

      print.object[, c("m", "sd", "m.diff", "se", "m.low", "m.upp", "t", "df", "d", "d.low", "d.upp")] <- vapply(print.object[, c("m", "sd", "m.diff", "se", "m.low", "m.upp", "t", "df", "d", "d.low", "d.upp")], formatC, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"), FUN.VALUE = character(2L))

      print.object[, "pval"] <- formatC(print.object[, "pval"], digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

      #...................
      ### Format ####

      print.object <- rbind(c("Group", "n", "nNA", "M", "SD", "M.Diff", "SE", "Low", "Upp", "t", "df", "p", "d", "Low", "Upp"), print.object)

      print.object[, -1L] <- apply(print.object[, -1L], 2L, format, justify = "right")

      print.object[-1L, 1L] <- paste0(" ", print.object[-1L, 1L])
      print.object[, 1L] <- format(print.object[, 1L], justify = "left")

      print.object[, 1L] <- paste(" ", print.object[, 1L])

      # NAs
      colnames.NA <- c("m.diff", "se", "m.low", "m.upp", "t", "df", "pval", "d", "d.low", "d.upp")

      print.object[, which(colnames(print.object) %in% colnames.NA)] <- apply(print.object[, which(colnames(print.object) %in% colnames.NA), drop = FALSE], 2L, function(y) gsub("NA", "  ", y))

      #...................
      ### Print Output ####

      cat(paste0(" Welch's Two-Sample t-Test\n\n"))

      #### Hypotheses ####

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

        # R Markdown not in progress
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

      #### Cohen's d ####

      if (!isTRUE(effsize)) { print.object <- print.object[, -which(colnames(print.object) %in% c("d", "d.low", "d.upp"))] }

      #### Descriptive Statistics ####

      if (!isTRUE(descript)) {

        print.object <- print.object[-2L, -which(colnames(print.object) %in% c("group", "n", "nNA", "m", "sd", "m.diff", "m.low", "m.upp", "se"))]

        print.object[, 1L] <- paste(" ", print.object[, 1L])

      }

      .write.table(print.object, left = 1L, right = 3L, horiz = horiz)

      #### Note ####

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

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Welch ANOVA ####

    }, multiple = {

      #...................
      ### Round ####

      print.object[["descript"]][, c("m", "low", "upp", "sd", "skew", "kurt")] <- vapply(print.object[["descript"]][, c("m", "low", "upp", "sd", "skew", "kurt")], formatC, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"), FUN.VALUE = character(nrow(print.object[["descript"]])))

      print.object[["test"]][, c("F", "df2", "eta.sq", "omega.sq")] <- vapply(print.object[["test"]][, c(c("F", "df2", "eta.sq", "omega.sq"))], formatC, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"), FUN.VALUE = character(1L))

      print.object[["test"]][, "pval"] <- formatC(print.object[["test"]][, "pval"], digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

      print.object[["posthoc"]][, c("m.diff", "se", "m.low", "m.upp", "t", "df", "d", "d.low", "d.upp")] <- vapply(print.object[["posthoc"]][, c("m.diff", "se", "m.low", "m.upp", "t", "df", "d", "d.low", "d.upp")], formatC, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"), FUN.VALUE = character(nrow(print.object[["posthoc"]])))

      print.object[["posthoc"]][, "pval"] <- formatC(print.object[["posthoc"]][, "pval"], digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

      #...................
      ### Format ####

      print.object[["descript"]] <- rbind(c("Group", "n", "nNA", "M", "Low", "Upp", "SD", "Skew", "Kurt"), print.object[["descript"]])
      print.object[["test"]] <- rbind(c("F", "df1", "df2", "p", "et", "om"), print.object[["test"]])
      print.object[["posthoc"]] <- rbind(c("Group1", "Group2", "M.diff", "SE", "Low", "Upp", "t", "df", "p", "d", "Low", "Upp"), print.object[["posthoc"]])

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

      #...................
      ### Print Output ####

      cat(paste0(" Welch's ANOVA\n\n"))

      #### Hypotheses ####

      if (isTRUE(hypo)) {

        # R Markdown in progress
        if (isTRUE(getOption("knitr.in.progress"))) {

          cat("  Null hypothesis        H0: mu.i = mu.j for all i and j\n",
              " Alternative hypothesis H1: mu.i != mu.j for at least one i != j \n\n")

        # R Markdown not in progress
        } else {

          cat("  Null hypothesis        H0: \u03BC\u1D62 = \u03BC\u2C7C for all i and j\n",
              " Alternative hypothesis H1: \u03BC\u1D62 \u2260 \u03BC\u2C7C for at least one i \u2260 j \n\n")

        }

      }

      #### Descriptive Statistics ####

      if (isTRUE(descript)) {

        .write.table(print.object[["descript"]], left = 1L, right = 3L, horiz = horiz)

        cat("\n")

      }

      #### Effect Size ####

      if (!isTRUE(effsize)) {

        print.object[["test"]] <- print.object[["test"]][, -which(colnames(print.object[["test"]]) %in% c("eta.sq", "omega.sq"))]

        print.object[["posthoc"]] <- print.object[["posthoc"]][, -which(colnames(print.object[["posthoc"]]) %in% c("d", "d.low", "d.upp"))]

      }

      .write.table(print.object[["test"]], left = 1L, right = 3L, horiz = horiz)

      #### Post-Hoc Test ####

      if (isTRUE(posthoc)) {

        cat(paste0("\n  Games-Howell Post Hoc Test for Multiple Comparison\n\n"))

        .write.table(print.object[["posthoc"]], left = 2L, right = 4L, horiz = horiz)

      }

    })

  #_____________________________________________________________________________
  #
  # z-Test, test.z() -----------------------------------------------------------
  }, test.z = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## One sample z-Test ####

    switch(x$sample, one = {

      #...................
      ### Round ####

      print.object[, c("m", "sd", "m.diff", "se", "m.low", "m.upp", "z", "d")] <- vapply(print.object[, c("m", "sd", "m.diff", "se", "m.low", "m.upp", "z", "d")], formatC, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"), FUN.VALUE = character(1L))

      print.object[, "p"] <- formatC(print.object[, "p"], digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

      #...................
      ### Format ####

      print.object <- rbind(c("n", "nNA", "M", "SD", "M.Diff", "SE", "Low", "Upp", "z", "p", "d"), print.object)

      print.object <- apply(print.object, 2L, format, justify = "right")

      print.object[, 1L] <- paste(" ", print.object[, 1L])

      #...................
      ### Print Output ####

      # R Markdown in progress
      if (isTRUE(getOption("knitr.in.progress"))) {

        cat(paste0(" One-Sample z-Test with ", "sigma = ", round(x$args$sigma, digits = digits), "\n\n"))

      # R Markdown not in progress
      } else {

        cat(paste0(" One-Sample z-Test with ", "\u03c3 = ", round(x$args$sigma, digits = digits), "\n\n"))

      }

      #### Hypotheses ####

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

        # R Markdown not in progress
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

      #### Cohen's d ####

      if (!isTRUE(effsize)) { print.object <- print.object[, -which(colnames(print.object) %in% c("d"))] }

      #### Descriptive Statistics ####

      if (!isTRUE(descript)) {

        print.object <- print.object[, -which(colnames(print.object) %in% c("n", "nNA", "m", "sd", "m.diff", "se", "m.low", "m.upp"))]

        print.object[, 1L] <- paste(" ", print.object[, 1L])

      }

      .write.table(print.object, left = 1L, right = 3L, horiz = horiz)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Two-Sample z-Test ####

    }, two = {

      #...................
      ### Round ####

      print.object[, c("m", "sd", "m.diff", "se", "m.low", "m.upp", "z", "d")] <- vapply(print.object[, c("m", "sd", "m.diff", "se", "m.low", "m.upp", "z", "d")], formatC, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"), FUN.VALUE = character(2L))

      print.object[, "p"] <- formatC(print.object[, "p"], digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

      #...................
      ### Format ####

      print.object <- rbind(c("Group", "n", "nNA", "M", "SD", "M.Diff", "SE", "Low", "Upp", "z", "p", "d"), print.object)

      print.object[, -1L] <- apply(print.object[, -1L], 2L, format, justify = "right")

      print.object[-1L, 1L] <- paste0(" ", print.object[-1L, 1L])
      print.object[, 1L] <- format(print.object[, 1L], justify = "left")

      print.object[, 1L] <- paste(" ", print.object[, 1L])

      # NAs
      print.object[, which(colnames(print.object) %in% c("m.diff", "se", "m.low", "m.upp", "z", "p", "d"))] <- apply(print.object[, which(colnames(print.object) %in% c("m.diff", "se", "m.low", "m.upp", "z", "p", "d")), drop = FALSE], 2L, function(y) gsub("NA", "  ", y))

      #...................
      ### Print Output ####

      # R Markdown in progress
      if (isTRUE(getOption("knitr.in.progress"))) {

        if (isTRUE(length(unique(x$args$sigma)) == 1L)) {

          cat(paste0(" Two-Sample z-Test with ", "sigma1 = sigma2 = ", round(x$args$sigma[1L], digits = digits), "\n\n"))

        } else {

          cat(paste0(" Two-Sample z-Test with ", "sigma1 = ", x$args$sigma[1L], " and ", "sigma2 = ", round(x$args$sigma[2L], digits = digits), "\n\n"))

        }

      # R Markdown not in progress
      } else {

        if (isTRUE(length(unique(x$args$sigma)) == 1L)) {

          cat(paste0(" Two-Sample z-Test with ", "\u03c3\u2081 = \u03c3\u2082 = ", round(x$args$sigma[1L], digits = digits), "\n\n"))

        } else {

          cat(paste0(" Two-Sample z-Test with ", "\u03c3\u2081 = ", x$args$sigma[1L], " and ", "\u03c3\u2082 = ", round(x$args$sigma[2L], digits = digits), "\n\n"))

        }

      }

      #### Hypotheses ####

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

        # R Markdown not in progress
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

      #### Cohen's d ####

      if (!isTRUE(effsize)) { print.object <- print.object[, -which(colnames(print.object) %in% c("d"))] }

      #### Descriptive Statistics ####

      if (!isTRUE(descript)) {

        print.object <- print.object[-2L, -which(colnames(print.object) %in% c("n", "nNA", "m", "sd", "m.diff", "se"))]

        print.object[, 1L] <- paste(" ", print.object[, 1L])

      }

      .write.table(print.object, left = 1L, right = 3L, horiz = horiz)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Paired-Sample z-Test ####

    }, paired = {

      #...................
      ### Round ####

      print.object[, c("m1", "m2", "m.diff", "sd.diff", "se", "m.low", "m.upp", "z", "d")] <- vapply(print.object[, c("m1", "m2", "m.diff", "sd.diff", "se", "m.low", "m.upp", "z", "d")], formatC, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0L, times = digits), collapse = "")), "0"), FUN.VALUE = character(1L))

      print.object[, "p"] <- formatC(print.object[, "p"], digits = p.digits, format = "f", zero.print = ifelse(p.digits > 0L, paste0("0.", paste(rep(0L, times = p.digits), collapse = "")), "0"))

      #...................
      ### Format ####

      print.object <- rbind(c("n", "nNA", "M1", "M2", "M.Diff", "SD.Diff", "SE", "Low", "Upp", "z", "p", "d"), print.object)

      print.object <- apply(print.object, 2L, format, justify = "right")

      print.object[, 1L] <- paste(" ", print.object[, 1L])

      #...................
      ### Print Output ####

      # R Markdown in progress
      if (isTRUE(getOption("knitr.in.progress"))) {

        cat(paste0(" Paired-Sample z-Test with ", "sigma(diff) = ", round(x$args$sigma, digits = digits), "\n\n"))

      # R Markdown not in progress
      } else {

        cat(paste0(" Paired-Sample z-Test with ", "\u03c3(diff) = ", round(x$args$sigma, digits = digits), "\n\n"))

      }

      #### Hypotheses ####

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

        # R Markdown not in progress
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

      #### Cohen's d ####

      if (!isTRUE(effsize)) { print.object <- print.object[, -which(colnames(print.object) %in% c("d"))] }

      #### Descriptive Statistics####

      if (!isTRUE(descript)) { print.object <- print.object[, -which(colnames(print.object) %in% c("n", "nNA", "m1", "m2", "m.diff", "sd.diff", "se", "m.low", "m.upp"))] }

      .write.table(print.object, left = 1L, right = 3L, horiz = horiz)

    })

  #_____________________________________________________________________________
  #
  # Extract Unique Elements and Count Number of Unique Elements, uniq() --------
  }, uniq = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Dimension of the object != NULL ####

    if (isTRUE(is.list(print.object))) {

      if (isTRUE(x$args$table)) {

        # Convert into data frame
        print.object <- as.data.frame(lapply(print.object, function(z) c(if (isTRUE(is.numeric(z) && !is.factor(z))) { formatC(z, format = "f", digits = ifelse(any(na.omit(abs(z - round(z)) > .Machine$double.eps^0.5)), max(nchar(unlist(lapply(strsplit(sub('0+$', '', as.character(z)), ".", fixed = TRUE), function(w) w[2]))), na.rm = TRUE), 0L)) } else { as.character(z) }, rep(".MA", times = max(sapply(print.object, length)) - length(z)))))

        # Replace NA
        if (isTRUE(any(unlist(print.object) == ".MA"))) { print.object[print.object == ".MA"] <- "" }

      }

    }

    print(print.object)

  })

}

#_______________________________________________________________________________
