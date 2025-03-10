#' Between-Subject Analysis of Variance
#'
#' This function performs an one-way between-subject analysis of variance (ANOVA)
#' including Tukey HSD post hoc tests for multiple comparison and provides descriptive
#' statistics, effect size measures, and a plot showing bars representing means
#' for each group and error bars for difference-adjusted confidence intervals.
#'
#' @param formula        a formula of the form \code{y ~ group} where \code{y} is
#'                       a numeric variable giving the data values and \code{group}
#'                       a numeric variable, character variable or factor with more
#'                       than two values or factor levels giving the corresponding
#'                       groups.
#' @param data           a matrix or data frame containing the variables in the
#'                       formula \code{formula}.
#' @param posthoc        logical: if \code{TRUE}, Tukey HSD post hoc test for
#'                       multiple comparison is conducted.
#' @param conf.level     a numeric value between 0 and 1 indicating the confidence
#'                       level of the interval.
#' @param hypo           logical: if \code{TRUE} (default), null and alternative
#'                       hypothesis are shown on the console.
#' @param descript       logical: if \code{TRUE} (default), descriptive statistics
#'                       are shown on the console.
#' @param effsize        logical: if \code{TRUE}, effect size measures \eqn{\eta^2}
#'                       and \eqn{\omega^2} for the ANOVA and Cohen's d for the post
#'                       hoc tests are shown on the console.
#' @param weighted       logical: if \code{TRUE}, the weighted pooled standard
#'                       deviation is used to compute Cohen's d.
#' @param correct        logical: if \code{TRUE}, correction factor to remove
#'                       positive bias in small samples is used.
#' @param digits         an integer value indicating the number of decimal places
#'                       to be used for displaying descriptive statistics and
#'                       confidence interval.
#' @param p.digits       an integer value indicating the number of decimal places
#'                       to be used for displaying the \emph{p}-value.
#' @param as.na          a numeric vector indicating user-defined missing values,
#'                       i.e. these values are converted to \code{NA} before conducting
#'                       the analysis.
#' @param plot           logical: if \code{TRUE}, a plot is drawn.
#' @param bar            logical: if \code{TRUE} (default), bars representing means
#'                       for each groups are drawn.
#' @param point          logical: if \code{TRUE}, points representing means for
#'                       each groups are drawn.
#' @param ci             logical: if \code{TRUE} (default), error bars representing
#'                       confidence intervals are drawn.
#' @param jitter         logical: if \code{TRUE}, jittered data points are drawn.
#' @param adjust         logical: if \code{TRUE} (default), difference-adjustment
#'                       for the confidence intervals is applied.
#' @param point.size     a numeric value indicating the \code{size} aesthetic for
#'                       the point representing the mean value.
#' @param errorbar.width a numeric value indicating the horizontal bar width of
#'                       the error bar.
#' @param jitter.size    a numeric value indicating the \code{size} aesthetic
#'                       for the jittered data points.
#' @param jitter.width   a numeric value indicating the amount of horizontal jitter.
#'                       data points.
#' @param jitter.height  a numeric value indicating the amount of vertical jitter.
#' @param jitter.alpha   a numeric value between 0 and 1 for specifying the
#'                       \code{alpha} argument in the \code{geom_histogram}
#'                       function for controlling the opacity of the jittered
#'                       data points.
#' @param xlab           a character string specifying the labels for the x-axis.
#' @param ylab           a character string specifying the labels for the y-axis.
#' @param ylim           a numeric vector of length two specifying limits of the
#'                       limits of the y-axis.
#' @param ybreaks        a numeric vector specifying the points at which tick-marks
#'                       are drawn at the y-axis.
#' @param title          a character string specifying the text for the title of
#'                       the plot.
#' @param subtitle       a character string specifying the text for the subtitle
#'                       of the plot.
#' @param filename       a character string indicating the \code{filename}
#'                       argument including the file extension in the \code{ggsave}
#'                       function. Note that one of \code{".eps"}, \code{".ps"},
#'                       \code{".tex"}, \code{".pdf"} (default),
#'                       \code{".jpeg"}, \code{".tiff"}, \code{".png"},
#'                       \code{".bmp"}, \code{".svg"} or \code{".wmf"} needs
#'                       to be specified as file extension in the \code{filename}
#'                       argument. Note that plots can only be saved when
#'                       \code{plot = TRUE}.
#' @param width          a numeric value indicating the \code{width} argument
#'                       (default is the size of the current graphics device)
#'                       in the \code{ggsave} function.
#' @param height         a numeric value indicating the \code{height} argument
#'                       (default is the size of the current graphics device)
#'                       in the \code{ggsave} function.
#' @param units          a character string indicating the \code{units} argument
#'                       (default is \code{in}) in the \code{ggsave} function.
#' @param dpi            a numeric value indicating the \code{dpi} argument
#'                       (default is \code{600}) in the \code{ggsave} function.
#' @param write          a character string naming a text file with file extension
#'                       \code{".txt"} (e.g., \code{"Output.txt"}) for writing the
#'                       output into a text file.
#' @param append         logical: if \code{TRUE} (default), output will be appended
#'                       to an existing text file with extension \code{.txt} specified
#'                       in \code{write}, if \code{FALSE} existing text file will be
#'                       overwritten.
#' @param check          logical: if \code{TRUE} (default), argument specification
#'                       is checked.
#' @param output         logical: if \code{TRUE} (default), output is shown on the
#'                       console.
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
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{plot}}{ggplot2 object for plotting the results}
#' \item{\code{result}}{result tables}
#'
#' @export
#'
#' @examples
#' # Example 1: Between-subject ANOVA
#' aov.b(mpg ~ gear, data = mtcars)
#'
#' # Example 2: Between-subject ANOVA, print effect size measures
#' aov.b(mpg ~ gear, data = mtcars, effsize = TRUE)
#'
#' # Example 3: Between-subject ANOVA, do not print hypotheses and descriptive statistics,
#' aov.b(mpg ~ gear, data = mtcars, descript = FALSE, hypo = FALSE)
#'
#' # Example 4: Between-subject ANOVA, plot results
#' aov.b(mpg ~ gear, data = mtcars, plot = TRUE)
#'
#' \dontrun{
#' # Example 5: Write Results into a text file
#' aov.b(mpg ~ gear, data = mtcars, write = "ANOVA.txt")
#'
#' # Example 6: Save plot
#' aov.b(mpg ~ gear, data = mtcars, plot = TRUE, filename = "Between-Subject_ANOVA.png",
#'       width = 7, height = 6)
#' }
aov.b <- function(formula, data, posthoc = FALSE, conf.level = 0.95, hypo = TRUE,
                  descript = TRUE, effsize = FALSE, weighted = FALSE, correct = FALSE,
                  digits = 2, p.digits = 3, as.na = NULL, plot = FALSE, bar = TRUE,
                  point = FALSE, ci = TRUE, jitter = FALSE, adjust = TRUE,
                  point.size = 3, errorbar.width = 0.1, jitter.size = 1.25,
                  jitter.width = 0.05, jitter.height = 0, jitter.alpha = 0.1,
                  xlab = NULL, ylab = "y", ylim = NULL, ybreaks = ggplot2::waiver(),
                  title = NULL, subtitle = "Confidence Interval", filename = NULL,
                  width = NA, height = NA, units = c("in", "cm", "mm", "px"), dpi = 600,
                  write = NULL, append = TRUE, check = TRUE, output = TRUE) {

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

  # Check inputs and R package
  .check.input(logical = c("posthoc", "hypo", "descript", "effsize", "weighted", "correct", "plot", "bar", "point", "ci", "jitter", "adjust", "append", "output"),
               numeric = list(point.size = 1L, errorbar.width = 1L, jitter.size = 1L, jitter.alpha = 1L, jitter.width = 1L, jitter.height = 1L),
               character = list(xlab = 1L, ylab = 1L, title = 1L, subtitle = 1L), args = c("digits", "p.digits", "conf.level", "write1"), envir = environment(), input.check = check)

  # Additional checks
  if (isTRUE(check)) {

    # Package ggplot2
    if (isTRUE(check && plot != "none")) { if (isTRUE(!nzchar(system.file(package = "ggplot2")))) { stop("Package \"ggplot2\" is needed to draw a plot, please install the package.", call. = FALSE) } }

    # Check if variables are in the data
    (!var.formula %in% colnames(data)) |> (\(y) if (isTRUE(any(y))) { stop(paste0(ifelse(sum(y) == 1L, "Variable ", "Variables "), "specified in the formula ", ifelse(sum(y) == 1L, "was ", "were "), "not found in 'data': ", paste(var.formula[y], collapse = ", ")), call. = FALSE) })()

    # Check if variance in any group is zero
    (misty::na.as(c(tapply(data[, y.var], data[, group.var], var, na.rm = TRUE)), na = 0L, check = FALSE) == 0L) |>
     (\(y) if (isTRUE(any(y))) {

       if (isTRUE(sum(y) == 1L)) { stop(paste0("Variance in group \"", names(which(y))), "\" is zero.", call. = FALSE) }

     } else {

       if (isTRUE(sum(y) == 1L)) { stop(paste0("Variance in following groups are zero: ", paste0(names(which(y)), collapse = ", ")), call. = FALSE) }

     })()

    # Check if input 'formula' has only one outcome variable
    if (isTRUE(length(y.var) != 1L)) { stop("Please specify a formula with only one outcome variable.", call. = FALSE) }

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
    (misty::na.as(c(tapply(data[, y.var], data[, group.var], var, na.rm = TRUE)), na = 0, check = FALSE) == 0L) |>
      (\(y) if (isTRUE(any(y))) {

        if (isTRUE(sum(y) == 1L)) { stop(paste0("After converting user-missing values into NA, variance in group \"", names(which(y))), "\" is zero.", call. = FALSE) }

      } else {

        if (isTRUE(sum(y) == 1L)) { stop(paste0("After converting user-missing values into NA, variance in following groups are zero: ", paste0(names(which(y)), collapse = ", ")), call. = FALSE) }

      })()

  }

  # Outcome
  y <- unlist(data[, y.var])

  # Grouping
  group <- factor(unlist(data[, group.var]))

  # Global variables
  m <- low <- upp <- NULL

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'units' Argument ####

  # Default setting
  if (isTRUE(all(c("in", "cm", "mm", "px") %in% units))) { units <- "in" }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Descriptive statistics ####

  ci.table <- misty::ci.mean(y, group = group, adjust = adjust, conf.level = conf.level, output = FALSE)$result |> (\(y) data.frame(y[, c("group", "n", "nNA", "m", "low", "upp", "sd", "skew", "kurt")]))()

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

  omega.sq <- ((ss.m - df.m*ms.r) / (ss.t + ms.r)) |> (\(y) ifelse(y < 0L, 0L, y))()

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
  ## Post hoc test ####

  #...................
  ### Compute Tukey HSD tests ####

  result.ph <- stats::TukeyHSD(aov.res, ordered = FALSE)[[1L]]

  # Extract groups
  labels <- t(combn(unlist(aov.res$xlevels), 2L))

  #...................
  ### Result table ####

  result.ph <- data.frame(group1 = labels[, 1L], group2 = labels[, 2L], m.diff = result.ph[, "diff"], m.low = result.ph[, "lwr"], m.upp = result.ph[, "upr"], pval = result.ph[, "p adj"], row.names = NULL)

  #...................
  ### Cohen's d ####

  cohen <- t(sapply(seq_len(nrow(result.ph)), function(x) {

    data.temp <- data.frame(group, y)[which(group %in% unlist(result.ph[x, c("group1", "group2")])), ]

    # Drop factor levels
    data.temp[, "group"] <- droplevels(data.temp[, "group"], except = unlist(result.ph[x, c("group1", "group2")]))

    misty::cohens.d(y ~ group, data = data.temp, weighted = weighted, correct = correct, conf.level = conf.level, check = FALSE, output = FALSE)$result[2L, c("d", "low", "upp")]

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

  result <- list(descript = ci.table, test = test, posthoc = result.ph, aov = aov.res)

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "aov.b",
                 data = data.frame(y, group),
                 formula = formula,
                 args = list(posthoc = posthoc, conf.level = conf.level,
                             hypo = hypo, descript = descript, effsize = effsize,
                             weighted = weighted, correct = correct,
                             digits = digits, p.digits = p.digits, as.na = as.na,
                             plot = plot, bar = bar, point = point, ci = ci, jitter = jitter,
                             adjust = adjust, point.size = point.size, errorbar.width = errorbar.width,
                             jitter.size = jitter.size, jitter.width = jitter.width, jitter.height = jitter.height,
                             jitter.alpha = jitter.alpha, xlab = xlab, ylab = ylab, ylim = ylim, ybreaks = ybreaks,
                             title = title, subtitle = subtitle, filename = filename,
                             width = width, height = height, units = units, dpi = dpi,
                             write = write, append = append, check = check, output = output),
                 plot = NULL, result = result)

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Plot and Save Results ------------------------------------------------------

  if (isTRUE(plot)) { object$plot <- plot(object, filename = filename, width = width, height = height, units = units, dpi = dpi, check = FALSE) |> (\(y) suppressMessages(suppressWarnings(print(y))))() }

  #_____________________________________________________________________________
  #
  # Write Results --------------------------------------------------------------

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

#_______________________________________________________________________________
