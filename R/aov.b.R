#' Between-Subject Analysis of Variance
#'
#' This function performs an one-way between-subject analysis of variance (ANOVA)
#' including Tukey HSD post hoc test for multiple comparison and provides descriptive
#' statistics, effect size measures, and a plot showing error bars for
#' difference-adjusted confidence intervals with jittered data points.
#'
#' @param formula       a formula of the form \code{y ~ group} where \code{y} is
#'                      a numeric variable giving the data values and \code{group}
#'                      a numeric variable, character variable or factor with more
#'                      than two values or factor levels giving the corresponding
#'                      groups.
#' @param data          a matrix or data frame containing the variables in the
#'                      formula \code{formula}.
#' @param posthoc       logical: if \code{TRUE}, Tukey HSD post hoc test for
#'                      multiple comparison is conducted.
#' @param conf.level    a numeric value between 0 and 1 indicating the confidence
#'                      level of the interval.
#' @param hypo          logical: if \code{TRUE} (default), null and alternative
#'                      hypothesis are shown on the console.
#' @param descript      logical: if \code{TRUE} (default), descriptive statistics
#'                      are shown on the console.
#' @param effsize       logical: if \code{TRUE}, effect size measures \eqn{\eta^2}
#'                      and \eqn{\omega^2} for the ANOVA and Cohen's d for the post
#'                      hoc tests are shown on the console.
#' @param weighted      logical: if \code{TRUE}, the weighted pooled standard
#'                      deviation is used to compute Cohen's d.
#' @param correct       logical: if \code{TRUE}, correction factor to remove
#'                      positive bias in small samples is used.
#' @param plot          logical: if \code{TRUE}, a plot showing error bars for
#'                      confidence intervals is drawn.
#' @param point.size    a numeric value indicating the \code{size} aesthetic for
#'                      the point representing the mean value.
#' @param adjust        logical: if \code{TRUE} (default), difference-adjustment
#'                      for the confidence intervals is applied.
#' @param error.width   a numeric value indicating the horizontal bar width of
#'                      the error bar.
#' @param xlab          a character string specifying the labels for the x-axis.
#' @param ylab          a character string specifying the labels for the y-axis.
#' @param ylim          a numeric vector of length two specifying limits of the
#'                      limits of the y-axis.
#' @param breaks        a numeric vector specifying the points at which tick-marks
#'                      are drawn at the y-axis.
#' @param jitter        logical: if \code{TRUE} (default), jittered data points
#'                      are drawn.
#' @param jitter.size   a numeric value indicating the \code{size} aesthetic
#'                      for the jittered data points.
#' @param jitter.width  a numeric value indicating the amount of horizontal jitter.
#' @param jitter.height a numeric value indicating the amount of vertical jitter.
#' @param jitter.alpha  a numeric value indicating the opacity of the jittered
#'                      data points.
#' @param title         a character string specifying the text for the title for
#'                      the plot.
#' @param subtitle      a character string specifying the text for the subtitle for
#'                      the plot.
#' @param digits        an integer value indicating the number of decimal places
#'                      to be used for displaying descriptive statistics and
#'                      confidence interval.
#' @param p.digits      an integer value indicating the number of decimal places
#'                      to be used for displaying the \emph{p}-value.
#' @param as.na         a numeric vector indicating user-defined missing values,
#'                      i.e. these values are converted to \code{NA} before conducting
#'                      the analysis.
#' @param check         logical: if \code{TRUE} (default), argument specification
#'                      is checked.
#' @param output        logical: if \code{TRUE} (default), output is shown on the
#'                      console.
#' @param ...           further arguments to be passed to or from methods.
#'
#' @details
#' \describe{
#' \item{\strong{Post Hoc Test}}{Tukey HSD post hoc test reports Cohen's d based
#' on the non-weighted standard deviation (i.e., \code{weighted = FALSE}) when
#' requesting an effect size measure  (i.e., \code{effsize = TRUE}) following the
#' recommendation by Delacre et al. (2021).
#' }
#' \item{\strong{Confidence Intervals}}{Cumming and Finch (2005) pointed out that
#' when 95% confidence intervals (CI) for two separately plotted means overlap,
#' it is still possible that the CI for the difference would not include zero.
#' Baguley (2012) proposed to adjust the width of the CIs by the factor of
#' \eqn{\sqrt{2}} to reflect the correct width of the CI for a mean difference:
#'
#' \deqn{\hat{\mu}_j \pm t_{n - 1, 1 - \alpha/2} \frac{\sqrt{2}}{2} \hat{\sigma}^_{{\hat{\mu}}_j}}
#'
#' These difference-adjusted CIs around the individual means can be interpreted
#' as if it were a CI for their difference. Note that the width of these intervals
#' is sensitive to differences in the variance and sample size of each sample,
#' i.e., unequal population variances and unequal \eqn{n} alter the interpretation
#' of difference-adjusted CIs.}
#' }
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{aov.w}}, \code{\link{test.t}}, \code{\link{test.z}},
#' \code{\link{test.levene}}, \code{\link{test.welch}}, \code{\link{cohens.d}},
#' \code{\link{ci.mean.diff}}, \code{\link{ci.mean}}
#'
#' @references
#' Baguley, T. S. (2012a). \emph{Serious stats: A guide to advanced statistics for
#' the behavioral sciences}. Palgrave Macmillan.
#'
#' Cumming, G., and Finch, S. (2005) Inference by eye: Confidence intervals, and
#' how to read pictures of data. \emph{American Psychologist, 60}, 170–80.
#'
#' Delacre, M., Lakens, D., Ley, C., Liu, L., & Leys, C. (2021). Why Hedges' g*s
#' based on the non-pooled standard deviation should be reported with Welch's t-test.
#' https://doi.org/10.31234/osf.io/tu6mp
#'
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology
#' - Using R and SPSS}. John Wiley & Sons.
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{data}}{data frame with variables used in the current analysis}
#' \item{\code{formula}}{formula of the current analysis}
#' \item{\code{plot}}{ggplot2 object for plotting the results}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{result}}{result tables}
#'
#' @export
#'
#' @examples
#' dat <- data.frame(group = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3),
#'                   y = c(3, 1, 4, 2, 5, 3, 2, 3, 6, 6, 3, NA))
#'
#' # Example 1: Between-subject ANOVA
#' aov.b(y ~ group, data = dat)
#'
#' # Example 2: Between-subject ANOVA
#' # print effect size measures
#' aov.b(y ~ group, data = dat, effsize = TRUE)
#'
#' # Example 3: Between-subject ANOVA
#' # do not print hypotheses and descriptive statistics,
#' aov.b(y ~ group, data = dat, descript = FALSE, hypo = FALSE)
#'
#' \dontrun{
#' # Example 4: Write Results into a text file
#' aov.b(y ~ group, data = dat, write = "ANOVA.txt")
#'
#' # Example 5: Between-subject ANOVA
#' # plot results
#' aov.b(y ~ group, data = dat, plot = TRUE)
#'
#' # Load ggplot2 package
#' library(ggplot2)
#'
#' # Example 6: Save plot, ggsave() from the ggplot2 package
#' ggsave("Between-Subject_ANOVA.png", dpi = 600, width = 4.5, height = 6)
#'
#' # Example 7: Between-subject ANOVA
#' # extract plot
#' p <- aov.b(y ~ group, data = dat, output = FALSE)$plot
#' p
#'
#' # Extract data
#' plotdat <- aov.b(y ~ group, data = dat, output = FALSE)$data
#'
#' # Draw plot in line with the default setting of aov.b()
#' ggplot(plotdat, aes(group, y)) +
#'   geom_jitter(alpha = 0.1, width = 0.05, height = 0, size = 1.25) +
#'   geom_point(stat = "summary", fun = "mean", size = 4) +
#'   stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = 0.20) +
#'   scale_x_discrete(name = NULL) +
#'   labs(subtitle = "Two-Sided 95% Confidence Interval") +
#'   theme_bw() + theme(plot.subtitle = element_text(hjust = 0.5))
#' }
aov.b <- function(formula, data, posthoc = FALSE, conf.level = 0.95,
                  hypo = TRUE, descript = TRUE, effsize = FALSE, weighted = FALSE,
                  correct = FALSE, plot = FALSE, point.size = 4, adjust = TRUE,
                  error.width = 0.1, xlab = NULL, ylab = NULL, ylim = NULL,
                  breaks = ggplot2::waiver(), jitter = TRUE, jitter.size = 1.25,
                  jitter.width = 0.05, jitter.height = 0, jitter.alpha = 0.1,
                  title = "", subtitle = "Confidence Interval",
                  digits = 2, p.digits = 4, as.na = NULL, write = NULL,
                  append = TRUE, check = TRUE, output = TRUE, ...) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'formula' is missing
  if (isTRUE(missing(formula))) { stop("Please specify a formula using the argument 'formula'", call. = FALSE) }

  # Check if input 'data' is missing
  if (isTRUE(missing(data))) { stop("Please specify a matrix or data frame for the argument 'data'.", call. = FALSE) }

  # Check if input 'data' is NULL
  if (isTRUE(is.null(data))) { stop("Input specified for the argument 'data' is NULL.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Formula --------------------------------------------------------------------

  # Variables
  var.formula <- all.vars(as.formula(formula))

  # Grouping variable
  group.var <- attr(terms(formula[-2L]), "term.labels")

  # Outcome variable
  y.var <- var.formula[-which(var.formula %in% group.var)]

  if (isTRUE(length(group.var) != 1L)) { stop("The aov.b function is currently limited to one between-subject factor.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # ggplot2 package
    if (isTRUE(plot && !nzchar(system.file(package = "ggplot2"))))  { stop("Package \"ggplot2\" is needed for drawing a bar chart, please install the package.", call. = FALSE) }

    # Check if variables are in the data
    var.data <- !var.formula %in% colnames(data)
    if (isTRUE(any(var.data))) { stop(paste0("Variables specified in the formula were not found in 'data': ", paste(var.formula[which(var.data)], collapse = ", ")), call. = FALSE) }

    # Check if variance in any group is zero
    y.zero.var <- misty::na.as(c(tapply(data[, y.var], data[, group.var], var, na.rm = TRUE)), na = 0L, check = FALSE) == 0L
    if (isTRUE(any(y.zero.var))) {

      if (isTRUE(sum(y.zero.var) == 1L)) { stop(paste0("Variance in group \"", names(which(y.zero.var))), "\" is zero.", call. = FALSE) }

    } else {

      if (isTRUE(sum(y.zero.var) == 1L)) { stop(paste0("Variance in following groups are zero: ", paste0(names(which(y.zero.var)), collapse = ", ")), call. = FALSE) }

    }

    # Check if input 'formula' has only one outcome variable
    if (isTRUE(length(y.var) != 1L)) { stop("Please specify a formula with only one outcome variable.", call. = FALSE) }

    # Check input 'posthoc'
    if (isTRUE(!is.logical(posthoc))) { stop("Please specify TRUE or FALSE for the argument 'posthoc'.", call. = FALSE) }

    # Check input 'conf.level'
    if (isTRUE(conf.level >= 1L || conf.level <= 0L)) { stop("Please specifiy a numeric value between 0 and 1 for the argument 'conf.level'.", call. = FALSE) }

    # Check input 'hypo'
    if (isTRUE(!is.logical(hypo))) { stop("Please specify TRUE or FALSE for the argument 'hypo'.", call. = FALSE) }

    # Check input 'descript'
    if (isTRUE(!is.logical(descript))) { stop("Please specify TRUE or FALSE for the argument 'descript'.", call. = FALSE) }

    # Check input 'effsize'
    if (isTRUE(!is.logical(effsize))) { stop("Please specify TRUE or FALSE for the argument 'effsize'.", call. = FALSE) }

    # Check input 'weighted'
    if (isTRUE(!is.logical(weighted))) { stop("Please specify TRUE or FALSE for the argument 'weighted'.", call. = FALSE) }

    # Check input 'correct'
    if (isTRUE(!is.logical(correct))) { stop("Please specify TRUE or FALSE for the argument 'correct'.", call. = FALSE) }

    # Check input 'plot'
    if (isTRUE(!is.logical(plot))) { stop("Please specify TRUE or FALSE for the argument 'plot'.", call. = FALSE) }

    # Check input 'adjust'
    if (isTRUE(!is.logical(adjust))) { stop("Please specify TRUE or FALSE for the argument 'adjust'.", call. = FALSE) }

    # Check input 'jitter'
    if (isTRUE(!is.logical(jitter))) { stop("Please specify TRUE or FALSE for the argument 'jitter'.", call. = FALSE) }

    # Check input 'digits'
    if (isTRUE(digits %% 1L != 0L || digits < 0L)) { stop("Please specify a positive integer number for the argument 'digits'.", call. = FALSE) }

    # Check input 'p.digits'
    if (isTRUE(p.digits %% 1L != 0L || p.digits < 0L)) { stop("Please specify a positive integer number for the argument 'p.digits'.", call. = FALSE) }

    # Check input 'write'
    if (isTRUE(!is.null(write) && substr(write, nchar(write) - 3L, nchar(write)) != ".txt")) { stop("Please specify a character string with file extenstion '.txt' for the argument 'write'.") }

    # Check input 'append'
    if (isTRUE(!is.logical(append))) { stop("Please specify TRUE or FALSE for the argument 'append'.", call. = FALSE) }

    # Check input 'output'
    if (isTRUE(!is.logical(output))) { stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Data and Variables ---------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) {

    # Replace user-specified values with missing values
    data[, y.var] <- .as.na(data[, y.var], na = as.na)

    # Check if variance in any group is zero
    y.zero.var <- misty::na.as(c(tapply(data[, y.var], data[, group.var], var, na.rm = TRUE)), na = 0, check = FALSE) == 0L
    if (isTRUE(any(y.zero.var))) {

      if (isTRUE(sum(y.zero.var) == 1L)) { stop(paste0("After converting user-missing values into NA, variance in group \"", names(which(y.zero.var))), "\" is zero.", call. = FALSE) }

    } else {

      if (isTRUE(sum(y.zero.var) == 1L)) { stop(paste0("After converting user-missing values into NA, variance in following groups are zero: ", paste0(names(which(y.zero.var)), collapse = ", ")), call. = FALSE) }

    }

  }

  # Outcome
  y <- unlist(data[, y.var])

  # Grouping
  group <- factor(unlist(data[, group.var]))

  # Global variables
  m <- low <- upp <- NULL

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Descriptive statistics ####

  ci <- misty::ci.mean(y, group = group, adjust = adjust, conf.level = conf.level, output = FALSE)$result[, -c(2L, 5L)]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Fit ANOVA model ####

  aov.res <- aov(y ~ group)

  # ANOVA table
  aov.table <- summary(aov.res)[[1L]]

  # Sum of squares model
  ss.m <- aov.table[["Sum Sq"]][1L]
  # Degrees of freedom model
  df.m <- aov.table[["Df"]][1L]

  # Mean sum of squares residuals
  ms.r <- aov.table[["Mean Sq"]][2L]
  # Total sum of squares
  ss.t <- sum(aov.table[["Sum Sq"]])

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Effect Size Measures ####

  #...................
  ### Eta squared ####

  eta.sq <- ss.m / ss.t

  #...................
  ### Omega squared ####

  omega.sq <- ((ss.m - df.m*ms.r) / (ss.t + ms.r)) |>
    (\(y) ifelse(y < 0L, 0L, y))()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## ANOVA table ####

  test <- data.frame(source = c(misty::chr.trim(row.names(aov.table), side = "both"), "Total"),
                     sum.sq = c(aov.table[, "Sum Sq"], sum(aov.table[, "Sum Sq"])),
                     df = c(aov.table[, "Df"], sum(aov.table[, "Df"])),
                     mean.sq = c(aov.table[, "Mean Sq"], sum(aov.table[, "Mean Sq"])),
                     F = c(aov.table[, "F value"], NA),
                     pval = c(aov.table[, "Pr(>F)"], NA),
                     eta.sq = c(eta.sq, NA, NA),
                     omega.sq = c(omega.sq, NA, NA))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Post-hoc test ####

  #...................
  ### Compute Tukey HSD tests ####

  result.ph <- stats::TukeyHSD(aov.res, ordered = FALSE)[[1L]]

  # Extract groups
  labels <- t(combn(unlist(aov.res$xlevels), 2L))

  #...................
  ### Result table ####
  result.ph <- data.frame(group1 = labels[, 1L], group2 = labels[, 2L], m.diff = result.ph[, "diff"],
                          m.low = result.ph[, "lwr"], m.upp = result.ph[, "upr"], pval = result.ph[, "p adj"],
                          row.names = NULL)

  #...................
  ### Cohen's d ####
  cohen <- t(sapply(1:nrow(result.ph), function(x) {

    data.temp <- data.frame(group, y)[which(group %in% unlist(result.ph[x, c("group1", "group2")])), ]

    # Drop factor levels
    data.temp[, "group"] <- droplevels(data.temp[, "group"], except = unlist(result.ph[x, c("group1", "group2")]))

    misty::cohens.d(y ~ group, data = data.temp, weighted = weighted, correct = correct, conf.level = conf.level,
                    check = FALSE, output = FALSE)$result[2L, c("d", "low", "upp")]

  }))

  #...................
  ### Result table ####
  result.ph <- data.frame(result.ph, d = unlist(cohen[, "d"]), d.low = unlist(cohen[, "low"]), d.upp = unlist(cohen[, "upp"]))

  #...................
  ### Sort groups ####

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

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Result object ####

  result <- list(descript = ci, test = test, posthoc = result.ph, aov = aov.res)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Plot ####

  # Plot data
  plotdat <-  data.frame(group = group, y = y, row.names = NULL)

  # Confidence interval
  plot.ci <- misty::ci.mean(data[, y.var], group = data[, group.var], adjust = adjust, conf.level = conf.level, output = FALSE)$result

  # Subtitle
  if (isTRUE(subtitle == "Confidence Interval")) { subtitle <- paste0("Two-Sided ", round(conf.level * 100L, digits = 2L), "% Confidence Interval") }

  #...................
  ### Create ggplot ####
  p <- ggplot2::ggplot(plotdat, ggplot2::aes(group, y))

  #...................
  ### Add jittered points ####
  if (isTRUE(jitter)) { p <- p + ggplot2::geom_jitter(alpha = jitter.alpha, width = jitter.width, height = jitter.height, size = jitter.size) }

  p <- p + ggplot2::geom_point(data = plot.ci, ggplot2::aes(group, m), stat = "identity", size = point.size) +
         ggplot2::geom_errorbar(data = plot.ci, ggplot2::aes(group, m, ymin = low, ymax = upp), width = error.width) +
         ggplot2::scale_x_discrete(name = xlab) +
         ggplot2::scale_y_continuous(name = ylab, limits = ylim, breaks = breaks) +
         ggplot2::theme_bw() +
         ggplot2::labs(title = title, subtitle = subtitle) +
         ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5),
                        plot.title = ggplot2::element_text(hjust = 0.5))

  #...................
  ### Print plot ####
  if (isTRUE(plot)) { suppressWarnings(print(p)) }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "aov.b",
                 data = data.frame(y, group, stringsAsFactors = FALSE),
                 formula = formula,
                 plot = p,
                 args = list(posthoc = posthoc, conf.level = conf.level,
                             hypo = hypo, descript = descript, effsize = effsize,
                             weighted = weighted, correct = correct, plot = plot,
                             point.size = point.size, error.width = error.width,
                             xlab = xlab, ylab = ylab, ylim = ylim, breaks = breaks,
                             jitter = jitter, jitter.size = jitter.size, jitter.width = jitter.width,
                             jitter.height = jitter.height, jitter.alpha = jitter.alpha,
                             title = title, subtitle = subtitle, digits = digits,
                             p.digits = p.digits, as.na = as.na, check = check,
                             write = write, append = append, output = output),
                 result = result)

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Write results --------------------------------------------------------------

  if (isTRUE(!is.null(write))) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Text file ####

    # Send R output to textfile
    sink(file = write, append = ifelse(isTRUE(file.exists(write)), append, FALSE), type = "output", split = FALSE)

    if (isTRUE(append && file.exists(write))) { write("", file = write, append = TRUE) }

    # Print object
    print(object, check = FALSE)

    # Close file connection
    sink()

  }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
