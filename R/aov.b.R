#' Between-Subject Analysis of Variance
#'
#' This function performs an one-way between-subject analysis of variance (ANOVA)
#' including Tukey HSD post hoc tests for multiple comparison and provides descriptive
#' statistics, effect size measures, and a plot showing bars representing means
#' for each group and error bars for difference-adjusted confidence intervals.
#'
#' @param formula    a formula of the form \code{y ~ group} where \code{y} is
#'                   a numeric variable giving the data values and \code{group}
#'                   a numeric variable, character variable or factor with more
#'                   than two values or factor levels giving the corresponding
#'                   groups.
#' @param data       a matrix or data frame containing the variables in the
#'                   formula \code{formula}.
#' @param hypo       logical: if \code{TRUE} (default), null and alternative
#'                   hypothesis are shown on the console.
#' @param descript   logical: if \code{TRUE} (default), descriptive statistics
#'                   are shown on the console.
#' @param effsize    logical: if \code{TRUE}, effect size measures \eqn{\eta^2}
#'                   and \eqn{\omega^2} for the ANOVA and Cohen's d for the post
#'                   hoc tests are shown on the console.
#' @param weighted   logical: if \code{TRUE} (default), the weighted pooled standard
#'                   deviation is used to compute Cohen's d.
#' @param correct    logical: if \code{TRUE}, correction factor to remove positive
#'                   bias in small samples for is used to compute Cohen's d.
#' @param posthoc    logical: if \code{TRUE}, Tukey HSD post hoc test for
#'                   multiple comparison is conducted.
#' @param conf.level a numeric value between 0 and 1 indicating the confidence
#'                   level of the interval.
#' @param digits     an integer value indicating the number of decimal places
#'                   to be used for displaying descriptive statistics and
#'                   confidence interval.
#' @param p.digits   an integer value indicating the number of decimal places
#'                   to be used for displaying the \emph{p}-value.
#' @param as.na      a numeric vector indicating user-defined missing values,
#'                   i.e. these values are converted to \code{NA} before conducting
#'                   the analysis.
#' @param plot       logical: if \code{TRUE}, a plot is drawn.
#' @param bar        logical: if \code{TRUE} (default), bars representing means
#'                   for each groups are drawn.
#' @param point      logical: if \code{TRUE}, points representing means for
#'                   each groups are drawn.
#' @param ci         logical: if \code{TRUE} (default), error bars representing
#'                   confidence intervals are drawn.
#' @param jitter     logical: if \code{TRUE}, jittered data points are drawn.
#' @param adjust     logical: if \code{TRUE} (default), difference-adjustment
#'                   for the confidence intervals is applied.
#' @param filename   a character string indicating the \code{filename}
#'                   argument including the file extension in the \code{ggsave}
#'                   function. Note that one of \code{".eps"}, \code{".ps"},
#'                   \code{".tex"}, \code{".pdf"} (default),
#'                   \code{".jpeg"}, \code{".tiff"}, \code{".png"},
#'                   \code{".bmp"}, \code{".svg"} or \code{".wmf"} needs
#'                   to be specified as file extension in the \code{filename}
#'                   argument. Note that plots can only be saved when
#'                   \code{plot = TRUE}.
#' @param width      a numeric value indicating the \code{width} argument
#'                   (default is the size of the current graphics device)
#'                   in the \code{ggsave} function.
#' @param height     a numeric value indicating the \code{height} argument
#'                   (default is the size of the current graphics device)
#'                   in the \code{ggsave} function.
#' @param dpi        a numeric value indicating the \code{dpi} argument
#'                   (default is \code{600}) in the \code{ggsave} function.
#' @param write      a character string naming a text file with file extension
#'                   \code{".txt"} (e.g., \code{"Output.txt"}) for writing the
#'                   output into a text file.
#' @param append     logical: if \code{TRUE} (default), output will be appended
#'                   to an existing text file with extension \code{.txt} specified
#'                   in \code{write}, if \code{FALSE} existing text file will be
#'                   overwritten.
#' @param check      logical: if \code{TRUE} (default), argument specification
#'                   is checked.
#' @param output     logical: if \code{TRUE} (default), output is shown on the
#'                   console.
#'
#' @details
#' \describe{
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
#' of difference-adjusted CIs.
#' }
#' }
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{aov.w}}, \code{\link{test.t}}, \code{\link{test.z}},
#' \code{\link{test.levene}}, \code{\link{aov.b}}, \code{\link{cohens.d}},
#' \code{\link{ci.mean.diff}}, \code{\link{ci.mean}}
#'
#' @references
#' Baguley, T. S. (2012a). \emph{Serious stats: A guide to advanced statistics for
#' the behavioral sciences}. Palgrave Macmillan.
#'
#' Cumming, G., and Finch, S. (2005) Inference by eye: Confidence intervals, and
#' how to read pictures of data. \emph{American Psychologist, 60}, 170–80.
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
#' #————————————————————————————————————————————————————————————————————————————
#' # Between-Subject Analysis of Variance
#'
#' # Example 1a: Between-Subject ANOVA
#' aov.b(hp ~ gear, data = mtcars)
#'
#' # Example 1b: Between-Subject ANOVA
#' # Print descriptive statistics and Tukey HSD post hoc test
#' aov.b(hp ~ gear, data = mtcars, descript = TRUE, posthoc = TRUE)
#'
#' # Example 1c: Between-Subject ANOVA, print eta-squared and omega-squared
#' aov.b(hp ~ gear, data = mtcars, effsize = TRUE)
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Plot
#'
#' # Example 2a: Plot results, default setting
#' aov.b(hp ~ gear, data = mtcars, plot = TRUE)
#'
#' # Example 2b: Plot results
#' # No bars, draw points representing means and jittered data points
#' aov.b(hp ~ gear, data = mtcars, plot = TRUE, bar = FALSE, point = TRUE, jitter = TRUE)
#'
#' # Example 2c: Plot results using the plot() function, use additional arguments
#' # see Details in the help page of the function plot.misty.object
#' object <- aov.b(hp ~ gear, data = mtcars)
#' plot(object, jitter = TRUE, jitter.alpha = 0.4, title = "Between-Subject ANOVA")
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Create Plot Manually
#'
#' # Load ggplot2 package
#' library(ggplot2)
#'
#' # Create misty object
#' object <- aov.b(hp ~ gear, data = mtcars)
#'
#' # Example 3: Plot
#' ggplot(object$result$descript, aes(group, y)) +
#'   geom_bar(aes(group, m), stat = "summary", fun = "mean") +
#'   geom_jitter(data = object$data, aes(group, y), alpha = 0.1, width = 0.05,
#'              height = 0, size = 1.25) +
#'   geom_point(aes(group, m), stat = "identity", size = 3) +
#'   geom_errorbar(aes(group, m, ymin = low, ymax = upp), width = 0.1) +
#'   theme_bw()
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Write Results and Save Plot
#'
#' \dontrun{
#'
#' # Example 4a: Write results into a text file
#' aov.b(hp ~ gear, data = mtcars, write = "ANOVA.txt")
#'
#' # Example 4b: Write results into an Excel file
#' aov.b(hp ~ gear, data = mtcars, write = "ANOVA.xlsx")
#'
#' # Example 4c: Save plot as PNG fine
#' aov.b(hp ~ gear, data = mtcars, plot = TRUE,
#'       filename = "ANOVA.png", width = 6, height = 5)
#' }
aov.b <- function(formula, data, hypo = FALSE, descript = FALSE, effsize = FALSE,
                  weighted = TRUE, correct = FALSE, posthoc = FALSE, conf.level = 0.95,
                  digits = 2, p.digits = 3, as.na = NULL, plot = FALSE, bar = TRUE,
                  point = FALSE, ci = TRUE, jitter = FALSE, adjust = TRUE,
                  filename = NULL, width = NA, height = NA, dpi = 600,
                  write = NULL, append = TRUE, check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'formula' is missing
  if (isTRUE(missing(formula))) { stop("Please specify a formula using the argument 'formula'", call. = FALSE) }

  # Check if input 'data' is missing or NULL
  if (isTRUE(missing(data) ||is.null(data))) { stop("Please specify a matrix or data frame for the argument 'x'.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Formula and Data Frame -----------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Variables ####

  # Extract variables
  var.formula <- all.vars(as.formula(formula))

  # Grouping variable
  group.var <- attr(terms(formula[-2L]), "term.labels")

  # Outcome variable
  y.var <- setdiff(var.formula, group.var)

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  # As data frame
  data <- as.data.frame(data)

  # Check if variables are in the data
  (!var.formula %in% colnames(data)) |> (\(y) if (isTRUE(any(y))) { stop(paste0("Variables specified in the the formula were not found in 'data': ", paste(var.formula[which(y)], collapse = ", ")), call. = FALSE) })()

  # Check if input 'formula' has only one grouping variable
  if (isTRUE(length(group.var) != 1L)) { stop("Please specify a formula with only one grouping variable.", call. = FALSE) }

  # Check if input 'formula' has only one outcome variable
  if (isTRUE(length(y.var) != 1L)) { stop("Please specify a formula with only one outcome variable.", call. = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) { data[, y.var] <- .as.na(data[, y.var], na = as.na) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Listwise Deletion ####

  data <- na.omit(data[, var.formula])

  #_____________________________________________________________________________
  #
  # Input Check ————————————————————————————————————————————————————————————————

  # Check inputs and R package
  .check.input(logical = c("hypo", "descript", "effsize", "weighted", "correct", "posthoc", "plot", "bar", "point", "ci", "jitter", "adjust", "append", "output"),
               args = c("digits", "p.digits", "conf.level", "write2"), envir = environment(), input.check = check)

  #_____________________________________________________________________________
  #
  # Main Function ——————————————————————————————————————————————————————————————

  # Outcome
  y <- unlist(data[, y.var])

  # Grouping
  group <- factor(unlist(data[, group.var]))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Descriptive Statistics ####

  ci.table <- misty::ci.mean(y, group = group, adjust = adjust, conf.level = conf.level, output = FALSE)$result |> (\(y) data.frame(y[, c("group", "n", "nNA", "m", "low", "upp", "sd", "skew", "kurt")]))()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Fit ANOVA Model ####

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

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Effect Size Measures ####

  #——————————————————————————————————————
  ### Eta Squared ####

  eta.sq <- ss.m / ss.t

  #——————————————————————————————————————
  ### Omega Squared ####

  omega.sq <- ((ss.m - df.m*ms.r) / (ss.t + ms.r)) |> (\(y) ifelse(y < 0L, 0L, y))()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## ANOVA Table ####

  test <- data.frame(source = c(misty::chr.trim(row.names(aov.table), side = "both"), "Total"),
                     sum.sq = c(aov.table[, "Sum Sq"], sum(aov.table[, "Sum Sq"])),
                     df = c(aov.table[, "Df"], sum(aov.table[, "Df"])),
                     mean.sq = c(aov.table[, "Mean Sq"], sum(aov.table[, "Mean Sq"])),
                     F = c(aov.table[, "F value"], NA),
                     pval = c(aov.table[, "Pr(>F)"], NA),
                     eta.sq = c(eta.sq, NA, NA),
                     omega.sq = c(omega.sq, NA, NA))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Post Hoc Test ####

  #——————————————————————————————————————
  ### Compute Tukey HSD Tests ####

  result.ph <- stats::TukeyHSD(aov.res, ordered = FALSE)[[1L]]

  # Extract groups
  labels <- t(combn(unlist(aov.res$xlevels), 2L))

  #——————————————————————————————————————
  ### Result Table ####

  result.ph <- data.frame(group1 = labels[, 1L], group2 = labels[, 2L], m.diff = result.ph[, "diff"], m.low = result.ph[, "lwr"], m.upp = result.ph[, "upr"], pval = result.ph[, "p adj"], row.names = NULL)

  #——————————————————————————————————————
  ### Cohen's d ####

  cohen <- t(sapply(seq_len(nrow(result.ph)), function(x) {

    data.temp <- data.frame(group, y)[which(group %in% unlist(result.ph[x, c("group1", "group2")])), ]

    # Drop factor levels
    data.temp[, "group"] <- droplevels(data.temp[, "group"], except = unlist(result.ph[x, c("group1", "group2")]))

    misty::cohens.d(y ~ group, data = data.temp, weighted = weighted, correct = correct, conf.level = conf.level, check = FALSE, output = FALSE)$result[2L, c("d", "low", "upp")]

  }))

  #——————————————————————————————————————
  ### Result Table ####

  result.ph <- data.frame(result.ph, d = unlist(cohen[, "d"]), d.low = unlist(cohen[, "low"]), d.upp = unlist(cohen[, "upp"]))

  #——————————————————————————————————————
  ### Sort Groups ####

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

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "aov.b",
                 data = data.frame(y, group),
                 formula = formula,
                 args = list(hypo = hypo, descript = descript, effsize = effsize, weighted = weighted, correct = correct, posthoc = posthoc, conf.level = conf.level, digits = digits, p.digits = p.digits, as.na = as.na, plot = plot, bar = bar, point = point, ci = ci, jitter = jitter, adjust = adjust, filename = filename, width = width, height = height, dpi = dpi, write = write, append = append, check = check, output = output),
                 plot = NULL,
                 result = list(descript = ci.table, test = test, posthoc = result.ph, aov = aov.res))

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Plot and Save Plot ---------------------------------------------------------

  if (isTRUE(plot)) { object$plot <- plot(object, filename = filename, width = width, height = height, dpi = dpi, check = FALSE) |> (\(y) suppressMessages(suppressWarnings(print(y))))() }

  #_____________________________________________________________________________
  #
  # Write Results --------------------------------------------------------------

  if (isTRUE(!is.null(write))) { .write.result(object = object, write = write, append = append) }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}

#_______________________________________________________________________________
