#' Between-Subject Analysis of Variance
#'
#' This function performs an one-way between-subject analysis of variance (ANOVA)
#' including Tukey HSD post hoc test for multiple comparison and provides descriptive
#' statistics, effect size measures, and a plot showing error bars for confidence
#' intervals with jittered data points.
#'
#' Note that by default Tukey HSD post hoc test reports Cohen's d based on the
#' weighted standard deviation (i.e., \code{weighted = TRUE}) when requesting an
#' effect size measure  (i.e., \code{effsize = TRUE}) following the recommendation
#' by Delacre et al. (2021).
#'
#' @param formula      a formula of the form \code{y ~ group} where \code{y} is
#'                     a numeric variable giving the data values and \code{group}
#'                     a numeric variable, character variable or factor with more
#'                     than two values or factor levelsgiving the corresponding
#'                     groups.
#' @param data         a matrix or data frame containing the variables in the
#'                     formula \code{formula}.
#' @param posthoc      logical: if \code{TRUE}, Tukey HSD post hoc test for
#'                     multiple comparison is conducted.
#' @param conf.level   a numeric value between 0 and 1 indicating the confidence
#'                     level of the interval.
#' @param hypo         logical: if \code{TRUE}, null and alternative hypothesis
#'                     are shown on the console.
#' @param descript     logical: if \code{TRUE}, descriptive statistics are shown
#'                     on the console.
#' @param effsize      logical: if \code{TRUE}, effect size measures \eqn{\eta^2}
#'                     and \eqn{\omega^2} for the ANOVA and Cohen's d for the post
#'                     hoc tests are shown on the console.
#' @param weighted     logical: if \code{TRUE}, the weighted pooled standard
#'                     deviation is used to compute Cohen's d.
#' @param correct      logical: if \code{TRUE}, correction factor to remove
#'                     positive bias in small samples is used.
#' @param plot         logical: if \code{TRUE}, a plot showing error bars for
#'                     confidence intervals is drawn.
#' @param point.size   a numeric value indicating the \code{size} aesthetic for
#'                     the point representing the mean value.
#' @param error.width  a numeric value indicating the horizontal bar width of
#'                     the error bar.
#' @param xlab         a character string specifying the labels for the x-axis.
#' @param ylab         a character string specifying the labels for the y-axis.
#' @param ylim         a numeric vector of length two specifying limits of the
#'                     limits of the y-axis.
#' @param breaks       a numeric vector specifying the points at which tick-marks
#'                     are drawn at the y-axis.
#' @param jitter       logical: if \code{TRUE} (default), jittered data points
#'                     are drawn.
#' @param jitter.size  a numeric value indicating the \code{size} aesthetic
#'                     for the jittered data points.
#' @param jitter.width a numeric value indicating the amount of vertical and
#'                     horizontal jitter.
#' @param jitter.alpha a numeric value indicating the opacity of the jittered
#'                     data points.
#' @param title        a character string specifying the text for the title for
#'                     the plot.
#' @param subtile      a character string specifying the text for the subtitle for
#'                     the plot.
#' @param digits       an integer value indicating the number of decimal places
#'                     to be used for displaying descriptive statistics and
#'                     confidence interval.
#' @param p.digits     an integer value indicating the number of decimal places
#'                     to be used for displaying the \emph{p}-value.
#' @param as.na        a numeric vector indicating user-defined missing values,
#'                     i.e. these values are converted to \code{NA} before conducting
#'                     the analysis.
#' @param check        logical: if \code{TRUE}, argument specification is checked.
#' @param output       logical: if \code{TRUE}, output is shown on the console.
#' @param ...          further arguments to be passed to or from methods.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{test.t}}, \code{\link{test.z}}, \code{\link{test.levene}},
#' \code{\link{test.welch}}, \code{\link{cohens.d}}, \code{\link{ci.mean.diff}},
#' \code{\link{ci.mean}}
#'
#' @references
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology
#' - Using R and SPSS}. John Wiley & Sons.
#'
#' Delacre, M., Lakens, D., Ley, C., Liu, L., & Leys, C. (2021). Why Hedges' g*s
#' based on the non-pooled standard deviation should be reported with Welch's t-test.
#' https://doi.org/10.31234/osf.io/tu6mp
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries: function call (\code{call}), type of analysis \code{type}, list with
#' the input specified in \code{x} (\code{data}), specification of function arguments
#' (\code{args}), and result table(s) (\code{result}).
#'
#' @export
#'
#' @examples
#' dat <- data.frame(group = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3),
#'                   y = c(3, 1, 4, 2, 5, 3, 2, 3, 6, 6, 3, NA))
#'
#' # Between-subject ANOVA
#' aov.b(y ~ group, data = dat)
#'
#' # Between-subject ANOVA
#' # print effect size measures
#' aov.b(y ~ group, data = dat, effsize = TRUE)
#'
#' # Between-subject ANOVA
#' # do not print hypotheses and descriptive statistics,
#' aov.b(y ~ group, data = dat, descript = FALSE, hypo = FALSE)
#'
#' \dontrun{
#' # Between-subject ANOVA
#' # plot results
#' aov.b(y ~ group, data = dat, plot = TRUE)
#'
#' # Load ggplot2 package
#' library(ggplot2)
#'
#' # Save plot, ggsave() from the ggplot2 package
#' ggsave("Between-Subject_ANOVA.png", dpi = 600, width = 4.5, height = 6)
#'
#' # Between-subject ANOVA
#' # extract plot
#' p <- aov.b(y ~ group, data = dat, output = FALSE)$plot
#' p
#'
#' # Extract data
#' plotdat <- aov.b(y ~ group, data = dat,, output = FALSE)$data
#'
#' # Draw plot in line with the default setting of aov.b()
#' ggplot(plotdat, aes(group, y)) +
#'   geom_point(stat = "summary", fun = "mean", size = 4) +
#'   stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = 0.20) +
#'   scale_x_discrete(name = NULL) +
#'   labs(subtitle = "Two-Sided 95% Confidence Interval") +
#'   theme_bw() + theme(plot.subtitle = element_text(hjust = 0.5))
#' }
aov.b <- function(formula, data, posthoc = TRUE, conf.level = 0.95,
                  hypo = TRUE, descript = TRUE, effsize = FALSE, weighted = FALSE,
                  correct = FALSE, plot = FALSE, point.size = 4, error.width = 0.1,
                  xlab = NULL, ylab = NULL, ylim = NULL, breaks = ggplot2::waiver(),
                  jitter = TRUE, jitter.size = 1.25, jitter.width = 0.05, jitter.alpha = 0.1,
                  title = "",  subtitle = "Confidence Interval",
                  digits = 2, p.digits = 4, as.na = NULL, check = TRUE,
                  output = TRUE, ...) {

  #......
  # Check if input 'formula' is missing
  if (isTRUE(missing(formula))) { stop("Please specify a formula using the argument 'formula'", call. = FALSE) }

  #......
  # Check if input 'data' is missing
  if (isTRUE(missing(data))) { stop("Please specify a matrix or data frame for the argument 'x'.", call. = FALSE) }

  #......
  # Check if input 'data' is NULL
  if (isTRUE(is.null(data))) { stop("Input specified for the argument 'data' is NULL.", call. = FALSE) }

  ##############################################################################
  # Formula

  #.........................................
  # Variables

  var.formula <- all.vars(as.formula(formula))

  # Grouping variable
  group.var <- attr(terms(formula[-2L]), "term.labels")

  # Outcome variable
  y.var <- var.formula[-which(var.formula %in% group.var)]

  if (isTRUE(length(group.var) != 1L)) { stop("The aov.b function is currently limited to one between-subject factor.", call. = FALSE) }

  ##############################################################################
  # Check

  #......
  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  #......
  # ggplot2 package
  if (isTRUE(!nzchar(system.file(package = "ggplot2"))))  { warning("Package \"ggplot2\" is needed for drawing a bar chart, please install the package.", call. = FALSE) }

  if (isTRUE(check)) {

    # Check if variables are in the data
    var.data <- !var.formula %in% colnames(data)
    if (isTRUE(any(var.data))) {

      stop(paste0("Variables specified in the the formula were not found in 'data': ",
                  paste(var.formula[which(var.data)], collapse = ", ")), call. = FALSE)

    }

    # Check if input 'formula' has only one grouping variable
    if (isTRUE(length(group.var) != 1L)) { stop("Please specify a formula with only one grouping variable.", call. = FALSE) }

    # Check if input 'formula' has only one outcome variable
    if (isTRUE(length(y.var) != 1L)) { stop("Please specify a formula with only one outcome variable.", call. = FALSE) }

    #......
    # Check input 'descript'
    if (isTRUE(!is.logical(descript))) { stop("Please specify TRUE or FALSE for the argument 'descript'.", call. = FALSE) }

    #......
    # Check input 'conf.level'
    if (isTRUE(conf.level >= 1L || conf.level <= 0L)) { stop("Please specifiy a numeric value between 0 and 1 for the argument 'conf.level'.", call. = FALSE) }

    #......
    # Check input 'effsize'
    if (isTRUE(!is.logical(effsize))) { stop("Please specify TRUE or FALSE for the argument 'effsize'.", call. = FALSE) }

    #......
    # Check input 'weighted'
    if (isTRUE(!is.logical(weighted))) { stop("Please specify TRUE or FALSE for the argument 'weighted'.", call. = FALSE) }

    #......
    # Check input 'correct'
    if (isTRUE(!is.logical(correct))) { stop("Please specify TRUE or FALSE for the argument 'correct'.", call. = FALSE) }

    #......
    # Check input 'hypo'
    if (isTRUE(!is.logical(hypo))) { stop("Please specify TRUE or FALSE for the argument 'hypo'.", call. = FALSE) }

    #......
    # Check input 'descript'
    if (isTRUE(!is.logical(descript))) { stop("Please specify TRUE or FALSE for the argument 'descript'.", call. = FALSE) }

    # Check input 'plot'
    if (isTRUE(!is.logical(plot))) { stop("Please specify TRUE or FALSE for the argument 'plot'.", call. = FALSE) }

    #......
    # Check input 'jitter'
    if (isTRUE(!is.logical(jitter))) { stop("Please specify TRUE or FALSE for the argument 'jitter'.", call. = FALSE) }

    #......
    # Check input 'digits'
    if (isTRUE(digits %% 1L != 0L || digits < 0L)) { stop("Please specify a positive integer number for the argument 'digits'.", call. = FALSE) }

    #......
    # Check input 'p.digits'
    if (isTRUE(p.digits %% 1L != 0L || p.digits < 0L)) { stop("Please specify a positive integer number for the argument 'p.digits'.", call. = FALSE) }

    #......
    # Check input 'output'
    if (isTRUE(!is.logical(output))) { stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE) }

  }

  #----------------------------------------
  # Convert user-missing values into NA

  if (isTRUE(!is.null(as.na))) {

    # Replace user-specified values with missing values
    data[, y.var] <- misty::as.na(data[, y.var], na = as.na, check = check)

    # Variable with missing values only
    data.miss <- vapply(data[, y.var, drop = FALSE], function(y) all(is.na(y)), FUN.VALUE = logical(1))
    if (any(data.miss)) {

      stop(paste0("After converting user-missing values into NA, following variables are completely missing: ",
                  paste(names(which(data.miss)), collapse = ", ")), call. = FALSE)

    }

  }

  ##############################################################################
  # Data

  # Outcome
  y <- unlist(data[, y.var])

  # Grouping
  group <- factor(unlist(data[, group.var]))

  ##############################################################################
  # Arguments

  # Global variables
  m <- low <- upp <- NULL

  ####################################################################################
  # Main Function

  #....................
  # Descriptive statistics
  ci <- misty::ci.mean(y, group = group, output = FALSE)$result[, -c(2L, 5L)]

  #....................
  # ANOVA table

  # Fit ANOVA model
  aov.res <- aov(y ~ group)
  aov.table <- summary(aov.res)[[1L]]

  ss.m <- aov.table[["Sum Sq"]][1L]
  df.m <- aov.table[["Df"]][1L]

  ms.r <- aov.table[["Mean Sq"]][2L]
  ss.t <- sum(aov.table[["Sum Sq"]])

  #....................
  # Eta squared
  eta.sq <- ss.m / ss.t

  #....................
  # Omega squared
  omega.sq <- (ss.m - df.m*ms.r) / (ss.t + ms.r)
  omega.sq <- ifelse(omega.sq < 0L, 0L, omega.sq)

  #....................
  # ANOVA table
  test <- data.frame(source = c(misty::chr.trim(row.names(aov.table)), "Total"),
                     sum.sq = c(aov.table[, "Sum Sq"], sum(aov.table[, "Sum Sq"])),
                     df = c(aov.table[, "Df"], sum(aov.table[, "Df"])),
                     mean.sq = c(aov.table[, "Mean Sq"], sum(aov.table[, "Mean Sq"])),
                     F = c(aov.table[, "F value"], NA),
                     pval = c(aov.table[, "Pr(>F)"], NA),
                     eta.sq = c(eta.sq, NA, NA),
                     omega.sq = c(omega.sq, NA, NA))

  #....................
  # Post-hoc test

  ###
  # Compute Tukey HSD tests
  result.ph <- stats::TukeyHSD(aov.res, ordered = FALSE)[[1L]]

  ###
  # Extract groups
  labels <- t(sapply(rownames(result.ph), function(x) {

    reg.x <- max(sapply(levels(group), function(y) regexpr(y, x)))

    c(substr(x, 1, reg.x - 2), substr(x, reg.x, nchar(x)))

  }))

  ###
  # Result table
  result.ph <- data.frame(group1 = labels[, 2L], group2 = labels[, 1L], m.diff = result.ph[, "diff"],
                          m.low = result.ph[, "lwr"], m.upp = result.ph[, "upr"], pval = result.ph[, "p adj"],
                          row.names = NULL)

  ###
  # Cohen's
  cohen <- t(sapply(1:nrow(result.ph), function(x) {

    data.temp <- data.frame(group, y)[which(group %in% unlist(result.ph[x, c("group1", "group2")])), ]

    # Drop factor levels
    data.temp[, "group"] <- droplevels(data.temp[, "group"], except = unlist(result.ph[x, c("group1", "group2")]))

    misty::cohens.d(y ~ group, data = data.temp, weighted = weighted, correct = correct, conf.level = conf.level,
                    check = FALSE, output = FALSE)$result[2L, c("d", "low", "upp")]

  }))

  ###
  # Result table
  result.ph <- data.frame(result.ph, d = unlist(cohen[, "d"]), d.low = unlist(cohen[, "low"]), d.upp = unlist(cohen[, "upp"]))

  ###
  # Sort groups

  # Reverse ordered factor levels
  group.rev <- factor(group, levels = rev(levels(group)))

  # Group 1
  result.ph <- result.ph[rev(unlist(sapply(levels(group.rev), function(x) which(result.ph$group1 == x)))), ]

  # Group 2
  for (i in levels(group)) {

    temp.ind <- which(result.ph$group1 == i)

    temp <- result.ph[temp.ind, ]

    result.ph[temp.ind, ] <- temp[rev(unlist(sapply(levels(group.rev), function(x) which(temp$group2 == x)))), ]

  }

  #....................
  # Result object
  result <- list(descript = ci, test = test, posthoc = result.ph)

  #----------------------------------------
  # Plot

  #......................................
  # Plot data

  # Confidence interval
  plot.ci <- misty::ci.mean(data[, y.var], group = data[, group.var], conf.level = conf.level, output = FALSE)$result

  plotdat <-  data.frame(group = group, y = y, row.names = NULL)

  # Plot Subtitle
  if (isTRUE(subtitle == "Confidence Interval")) { subtitle <- paste0("Two-Sided ", round(conf.level * 100L, digits = 2L), "% Confidence Interval") }

  ###
  # Crease ggplot
  p <- ggplot2::ggplot(plotdat, ggplot2::aes(group, y))

  ###
  # Add jittered points
  if (isTRUE(jitter)) { p <- p + ggplot2::geom_jitter(alpha = jitter.alpha, width = jitter.width, size = jitter.size) }

  p <- p + ggplot2::geom_point(data = plot.ci, ggplot2::aes(group, m), stat = "identity", size = point.size) +
         ggplot2::geom_errorbar(data = plot.ci, ggplot2::aes(group, m, ymin = low, ymax = upp), width = error.width) +
         ggplot2::scale_x_discrete(name = xlab) +
         ggplot2::scale_y_continuous(name = ylab, limits = ylim, breaks = breaks) +
         ggplot2::theme_bw() +
         ggplot2::labs(title = title, subtitle = subtitle) +
         ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5),
                        plot.title = ggplot2::element_text(hjust = 0.5))

  #......................................
  # Print plot
  if (isTRUE(plot)) { suppressWarnings(print(p)) }

  ##############################################################################
  # Return object and output

  object <- list(call = match.call(),
                 type = "aov.b",
                 data = data.frame(y, group, stringsAsFactors = FALSE),
                 plot = p,
                 args = list(formula = formula, conf.level = conf.level,
                             hypo = hypo, descript = descript, effsize = effsize,
                             weighted = weighted, correct = correct, plot = plot,
                             point.size = point.size, error.width = error.width,
                             xlab = xlab, ylab = ylab, ylim = ylim, breaks = breaks,
                             jitter = jitter, jitter.size = jitter.size, jitter.alpha = jitter.alpha,
                             jitter.width = jitter.width, title = title, subtitle = subtitle,
                             digits = digits, p.digits = p.digits, as.na = as.na,
                             check = check, output = output),
                 result = result)

  class(object) <- "misty.object"

  ##############################################################################
  # Output

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
