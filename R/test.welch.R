#' Welch's Test
#'
#' This function performs Welch's two-sample t-test and Welch's ANOVA including
#' Games-Howell post hoc test for multiple comparison and provides descriptive
#' statistics, effect size measures, and a plot showing error bars for
#' difference-adjusted confidence intervals with jittered data points.
#'
#' @param formula        a formula of the form \code{y ~ group} where \code{y} is
#'                       a numeric variable giving the data values and \code{group}
#'                       a numeric variable, character variable or factor with two
#'                       or more than two values or factor levels giving the
#'                       corresponding groups.
#' @param data           a matrix or data frame containing the variables in the
#'                       formula \code{formula}.
#' @param alternative    a character string specifying the alternative hypothesis,
#'                       must be one of \code{"two.sided"} (default), \code{"greater"}
#'                       or \code{"less"}. Note that this argument is only used when
#'                       conducting Welch's two-sample t-test.
#' @param posthoc        logical: if \code{TRUE}, Games-Howell post hoc test for
#'                       multiple comparison is conducted when performing Welch's
#'                       ANOVA.
#' @param conf.level     a numeric value between 0 and 1 indicating the confidence
#'                       level of the interval.
#' @param hypo           logical: if \code{TRUE} (default), null and alternative
#'                       hypothesis are shown on the console.
#' @param descript       logical: if \code{TRUE} (default), descriptive statistics
#'                       are shown on the console.
#' @param effsize        logical: if \code{TRUE}, effect size measure Cohen's d for
#'                       Welch's two-sample t-test (see \code{\link{cohens.d}}),
#'                       \eqn{\eta^2} and \eqn{\omega^2} for Welch's ANOVA and
#'                       Cohen's d for the post hoc tests are shown on the console.
#' @param weighted       logical: if \code{TRUE}, the weighted pooled standard
#'                       deviation is used to compute Cohen's d.
#' @param ref            a numeric value or character string indicating the reference
#'                       group. The standard deviation of the reference group is
#'                       used to standardized the mean difference to compute
#'                       Cohen's d.
#' @param correct        logical: if \code{TRUE}, correction factor to remove
#'                       positive bias in small samples is used.
#' @param plot           logical: if \code{TRUE}, a plot showing error bars for
#'                       confidence intervals is drawn.
#' @param adjust         logical: if \code{TRUE} (default), difference-adjustment
#'                       for the confidence intervals is applied.
#' @param point.size     a numeric value indicating the \code{size} aesthetic for
#'                       the point representing the mean value.
#' @param errorbar.width a numeric value indicating the horizontal bar width of
#'                       the error bar.
#' @param xlab           a character string specifying the labels for the x-axis.
#' @param ylab           a character string specifying the labels for the y-axis.
#' @param ylim           a numeric vector of length two specifying limits of the
#'                       limits of the y-axis.
#' @param breaks         a numeric vector specifying the points at which tick-marks
#'                       are drawn at the y-axis.
#' @param jitter         logical: if \code{TRUE} (default), jittered data points
#'                       are drawn.
#' @param jitter.size    a numeric value indicating the \code{size} aesthetic
#'                       for the jittered data points.
#' @param jitter.width   a numeric value indicating the amount of horizontal jitter.
#' @param jitter.height  a numeric value indicating the amount of vertical jitter.
#' @param jitter.alpha   a numeric value indicating the opacity of the jittered
#'                       data points.
#' @param title          a character string specifying the text for the title for
#'                       the plot.
#' @param subtitle       a character string specifying the text for the subtitle for
#'                       the plot.
#' @param digits         an integer value indicating the number of decimal places
#'                       to be used for displaying descriptive statistics and
#'                       confidence interval.
#' @param p.digits       an integer value indicating the number of decimal places
#'                       to be used for displaying the \emph{p}-value.
#' @param as.na          a numeric vector indicating user-defined missing values,
#'                       i.e. these values are converted to \code{NA} before conducting
#'                       the analysis.
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
#' @param ...            further arguments to be passed to or from methods.
#'
#' @details
#' \describe{
#' \item{\strong{Effect Size Measure}}{By default, Cohen's d based on the non-weighted
#' standard deviation (i.e., \code{weighted = FALSE}) which does not assume homogeneity
#' of variance is computed (see Delacre et al., 2021) when requesting an effect size
#' measure (i.e., \code{effsize = TRUE}). Cohen's d based on the pooled standard
#' deviation assuming equality of variances between groups can be requested by
#' specifying \code{weighted = TRUE}.}
#' }
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{test.t}}, \code{\link{test.z}}, \code{\link{test.levene}},
#' \code{\link{aov.b}}, \code{\link{cohens.d}}, \code{\link{ci.mean.diff}},
#' \code{\link{ci.mean}}
#'
#' @references
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology
#' - Using R and SPSS}. John Wiley & Sons.
#'
#' Delacre, M., Lakens, D., Ley, C., Liu, L., & Leys, C. (2021). Why Hedges' g*s
#' based on the non-pooled standard deviation should be reported with Welch's
#' t-test. https://doi.org/10.31234/osf.io/tu6mp
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{sample}}{type of sample, i.e., one-, two-, or paired sample}
#' \item{\code{formula}}{formula}
#' \item{\code{data}}{data frame with the outcome and grouping variable}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{plot}}{ggplot2 object for plotting the results}
#' \item{\code{result}}{result table}
#'
#' @export
#'
#' @examples
#' #----------------------------------------------------------------------------
#' # Two-Sample Design
#'
#' # Example 1a: Two-sided two-sample Welch-test
#' test.welch(mpg ~ vs, data = mtcars)
#'
#' # Example 1b: One-sided two-sample Welch-test
#' test.welch(mpg ~ vs, data = mtcars, alternative = "greater")
#'
#' # Example 1c: Two-sided two-sample Welch-test
#' # print Cohen's d with weighted pooled SD
#' test.welch(mpg ~ vs, data = mtcars, effsize = TRUE)
#'
#' # Example 1d: Two-sided two-sample Welch-test
#' # plot results
#' test.welch(mpg ~ vs, data = mtcars, plot = TRUE)
#'
#' # Load ggplot2 package
#' library(ggplot2)
#'
#' # Save plot, ggsave() from the ggplot2 package
#' ggsave("Two-sample_Welch-test.png", dpi = 600, width = 4, height = 6)
#'
#' # Example 1e: Two-sided two-sample Welch-test
#' # extract plot
#' p <- test.welch(mpg ~ vs, data = mtcars, output = FALSE)$plot
#' p
#'
#' # Example 1f: Two-sided two-sample Welch-test
#' # Draw plot in line with the default setting of test.welch()
#'
#' # Extract data
#' plotdat <- test.welch(mpg ~ vs, data = mtcars, output = FALSE)$data
#'
#' # Draw plot
#' ggplot(plotdat, aes(factor(group), y)) +
#'   geom_point(stat = "summary", fun = "mean", size = 4) +
#'   stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = 0.20) +
#'   scale_x_discrete(name = NULL) +
#'   labs(subtitle = "Two-Sided 95% Confidence Interval") +
#'   theme_bw() + theme(plot.subtitle = element_text(hjust = 0.5))
#'
#' #----------------------------------------------------------------------------
#' # Multiple-Sample Design
#'
#' # Example 2a: Welch's ANOVA
#' test.welch(mpg ~ gear, data = mtcars)
#'
#' # Example 2b: Welch's ANOVA
#' # print eta-squared and omega-squared
#' test.welch(mpg ~ gear, data = mtcars, effsize = TRUE)
#'
#' # Example 2c: Welch's ANOVA, plot results
#' test.welch(mpg ~ gear, data = mtcars, plot = TRUE)
#'
#' # Load ggplot2 package
#' library(ggplot2)
#'
#' # Save plot, ggsave() from the ggplot2 package
#' ggsave("Multiple-sample_Welch-test.png", dpi = 600, width = 4.5, height = 6)
#'
#' # Example 2e: Welch's ANOVA
#' # extract plot
#' p <- test.welch(mpg ~ gear, data = mtcars, output = FALSE)$plot
#' p
#'
#' # Example 2e: Welch's ANOVA
#' # Draw plot in line with the default setting of test.welch()
#'
#' # Extract data
#' plotdat <- test.welch(mpg ~ gear, data = mtcars, output = FALSE)$data
#'
#' # Draw plot
#' ggplot(plotdat, aes(group, y)) +
#'   geom_point(stat = "summary", fun = "mean", size = 4) +
#'   stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = 0.20) +
#'   scale_x_discrete(name = NULL) +
#'   labs(subtitle = "Two-Sided 95% Confidence Interval") +
#'   theme_bw() + theme(plot.subtitle = element_text(hjust = 0.5))
test.welch <- function(formula, data, alternative = c("two.sided", "less", "greater"),
                       posthoc = FALSE, conf.level = 0.95, hypo = TRUE, descript = TRUE,
                       effsize = FALSE, weighted = FALSE, ref = NULL, correct = FALSE,
                       plot = FALSE, adjust = TRUE, point.size = 4, errorbar.width = 0.1,
                       xlab = NULL, ylab = NULL, ylim = NULL, breaks = ggplot2::waiver(),
                       jitter = TRUE, jitter.size = 1.25, jitter.width = 0.05,
                       jitter.height = 0, jitter.alpha = 0.1,
                       title = "",  subtitle = "Confidence Interval",
                       digits = 2, p.digits = 3, as.na = NULL, write = NULL, append = TRUE,
                       check = TRUE, output = TRUE, ...) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'formula' is missing
  if (isTRUE(missing(formula))) { stop("Please specify a formula using the argument 'formula'", call. = FALSE) }

  # Check if input 'data' is missing
  if (isTRUE(missing(data))) { stop("Please specify a matrix or data frame for the argument 'x'.", call. = FALSE) }

  # Check if input 'data' is NULL
  if (isTRUE(is.null(data))) { stop("Input specified for the argument 'data' is NULL.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Variables ------------------------------------------------------------------

  var.formula <- all.vars(as.formula(formula))

  # Grouping variable
  group.var <- attr(terms(formula[-2L]), "term.labels")

  # Outcome variable
  y.var <- setdiff(var.formula, group.var)

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("hypo", "descript", "effsize", "weighted", "correct", "plot", "jitter", "append", "output"),
               numeric = list(point.size = 1L, errorbar.width = 1L, jitter.size = 1L, jitter.width = 1L, jitter.height = 1L, jitter.alpha = 1L, ylim = 2L),
               character = list(xlab = 1L, ylab = 1L, title = 1L, subtitle = 1L),
               args = c("conf.level", "digits", "p.digits", "write1"),
               package = "ggplot2", envir = environment(), input.check = check)

  # Additional checks
  if (isTRUE(check)) {

    # Check if variables are in the data
    var.data <- !var.formula %in% colnames(data)
    if (isTRUE(any(var.data))) {

      stop(paste0("Variables specified in the the formula were not found in 'data': ", paste(var.formula[which(var.data)], collapse = ", ")), call. = FALSE)

    }

    # Check if input 'formula' has only one grouping variable
    if (isTRUE(length(group.var) != 1L)) { stop("Please specify a formula with only one grouping variable.", call. = FALSE) }

    # Check if input 'formula' has only one outcome variable
    if (isTRUE(length(y.var) != 1L)) { stop("Please specify a formula with only one outcome variable.", call. = FALSE) }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) { data[, y.var] <- .as.na(data[, y.var], na = as.na) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  # Outcome
  y <- unlist(data[, y.var])

  # Grouping
  group <- factor(unlist(data[, group.var]))

  # Sample
  sample <- ifelse(length(levels(group)) == 2L, "two", "multiple")

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------


  # Global variables
  m <- low <- upp <- NULL

  # Alternative hypothesis
  if (isTRUE(all(c("two.sided", "less", "greater") %in% alternative))) { alternative <- "two.sided" }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Two-sample Welch test ####
  if (isTRUE(sample == "two")) {

    # Descriptive statistics
    ci <- misty::df.rename(misty::ci.mean.diff(formula = formula, data = data,  paired = FALSE, adjust = adjust, alternative = alternative, conf.level = conf.level, check = FALSE, output = FALSE)$result,
                           from = c("between", "low", "upp"), to = c("group", "m.low", "m.upp"))

    # Cohen's d
    d <- misty::cohens.d(formula = formula, data = data, paired = FALSE, mu = 0L,
                         weighted = weighted, cor = TRUE, ref = ref, correct = correct,
                         alternative = alternative, conf.level = conf.level,
                         group = NULL, split = NULL, sort.var = FALSE,
                         check = FALSE, output = FALSE)$result

    # Welch's test for two groups
    welch <- t.test(formula = formula, data = data,
                    alternative = switch(alternative,
                                         two.sided = "two.sided",
                                         greater = "less",
                                         less = "greater"),
                    var.equal = FALSE)

    #...................
    ### Result object ####

    result <- data.frame(cbind(ci[, c("group", "n", "nNA", "m", "sd", "m.diff")],
                               se = c(NA, welch$stderr), ci[, c("m.low", "m.upp")],
                               t = c(NA, welch$statistic)*-1L, df = c(NA, welch$parameter), pval = c(NA, welch$p.value),
                               d = d$d, d.low = d$low, d.upp = d$upp),
                         row.names = NULL)

    sample <- "two"

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Welch's test for more than two groups ####
  } else if (isTRUE(sample == "multiple")) {

    #...................
    ### Descriptive statistics ####

    ci <- misty::ci.mean(y, group = group, adjust = adjust, output = FALSE)$result[, c("group", "n", "nNA", "m", "low", "upp", "sd", "skew", "kurt")]

    #...................
    ### ANOVA table ####

    aov.table <- summary(aov(y ~ group))[[1L]]

    ss.m <- aov.table[["Sum Sq"]][1L]
    df.m <- aov.table[["Df"]][1L]

    ms.r <- aov.table[["Mean Sq"]][2L]
    ss.t <- sum(aov.table[["Sum Sq"]])

    #...................
    ### Eta squared ####

    eta.sq <- ss.m / ss.t

    #...................
    ### Omega squared ####

    omega.sq <- (ss.m - df.m*ms.r) / (ss.t + ms.r)
    omega.sq <- ifelse(omega.sq < 0L, 0L, omega.sq)

    #...................
    ### Welch's ANOVA ####

    welch <- oneway.test(formula = formula, data = data, var.equal = FALSE)

    #...................
    ### Post-Hoc test ####

    # Generate all pairwise combinations
    combs <- combn(levels(group), m = 2L)

    # Number of groups
    groups <- length(levels(group))

    # Sample size
    n <- setNames(ci[["n"]], ci[["group"]])

    # Mean and variance
    means <- setNames(ci[["m"]], ci[["group"]])
    vars <- setNames(ci[["sd"]]^2L, ci[["group"]])

    ###
    # Conduct post-hoc test
    result.ph <- lapply(1L:ncol(combs), function(x) {

      # Mean
      temp.m1 <- means[names(means) == combs[1L, x]]
      temp.m2 <- means[names(means) == combs[2L, x]]

      # Variance
      temp.var1 <- vars[names(vars) == combs[1L, x]]
      temp.var2 <- vars[names(vars) == combs[2L, x]]

      # Sample size
      temp.n1 <- n[names(n) == combs[1L, x]]
      temp.n2 <- n[names(n) == combs[2L, x]]

      # Mean difference
      m.diff <- temp.m2 - temp.m1

      # t-values
      t <- abs(temp.m1 - temp.m2) / sqrt((temp.var1/ temp.n1) + (temp.var2 / temp.n2))

      # Degrees of Freedom
      df <- (temp.var1 / temp.n1 + temp.var2 / temp.n2)^2L / ((temp.var1 / temp.n1)^2L / (temp.n1 - 1L) + (temp.var2 / temp.n2)^2L / (temp.n2 - 1L))

      # p-values
      pval <- stats::ptukey(t * sqrt(2L), groups, df, lower.tail = FALSE)

      # Sigma standard error
      se <- sqrt(0.5 * (temp.var1 / temp.n1 + temp.var2 / temp.n2))

      # Lower Confidence Limit
      m.low <- lapply(1L:ncol(combs), function(x) { m.diff - stats::qtukey(p = 0.95, nmeans = groups, df = df) * se })[[1L]]

      # Upper Confidence Limit
      m.upp <- lapply(1L:ncol(combs), function(x) { m.diff + stats::qtukey(p = 0.95, nmeans = groups, df = df) * se })[[1L]]

      ###
      # Cohen's d
      data.temp <- data.frame(group, y)[which(group %in% c(combs[1L, x], combs[2L, x])), ]

      # Drop factor levels
      data.temp[, "group"] <- droplevels(data.temp[, "group"], except = c(combs[1L, x], combs[2L, x]))

      cohen <- misty::cohens.d(y ~ group, data = data.temp, weighted = weighted, correct = correct, conf.level = conf.level, check = FALSE, output = FALSE)$result

      ###
      # Collect results
      result.ph <- list(combs[1L, x], combs[2L, x], m.diff, se, m.low, m.upp, t, df, pval, d = cohen[2L, "d"], cohen[2L, "low"], cohen[2L, "upp"])

    })

    # Result table
    result.ph <- data.frame(matrix(unlist(lapply(result.ph, function(x) { unlist(x) })), nrow = ncol(combs), byrow = TRUE,
                                   dimnames = list(NULL, c("group1", "group2", "m.diff", "se", "m.low", "m.upp", "t", "df", "pval", "d", "d.low", "d.upp"))))

    # Convert to numeric
    result.ph[, c(3L:ncol(result.ph))] <- as.numeric(as.matrix(result.ph[, c(3L:ncol(result.ph))]))

    #...................
    ### Result object ####

    result <- list(descript = ci,
                   test = data.frame(F = welch$statistic,
                                     df1 = welch$parameter["num df"], df2 = welch$parameter["denom df"],
                                     pval = welch$p.value,
                                     eta.sq = eta.sq, omega.sq = omega.sq, row.names = NULL),
                   posthoc = result.ph)

    sample <- "multiple"

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Plot ####

  #...................
  ### Plot data ####

  # Confidence interval
  plot.ci <- misty::ci.mean(data[, y.var], group = data[, group.var], adjust = adjust, conf.level = conf.level, output = FALSE)$result

  plotdat <-  data.frame(group = group, y = y, row.names = NULL)

  # Plot Subtitle
  if (isTRUE(subtitle == "Confidence Interval")) { subtitle <- paste0("Two-Sided ", round(conf.level * 100L, digits = 2L), "% Confidence Interval") }

  # Create ggplot
  p <- ggplot2::ggplot(plotdat, ggplot2::aes(group, y))

  # Add jittered points
  if (isTRUE(jitter)) { p <- p + ggplot2::geom_jitter(alpha = jitter.alpha, width = jitter.width, height = jitter.height, size = jitter.size) }

  p <- p + ggplot2::geom_point(data = plot.ci, ggplot2::aes(group, m), stat = "identity", size = point.size) +
         ggplot2::geom_errorbar(data = plot.ci, ggplot2::aes(group, m, ymin = low, ymax = upp), width = errorbar.width) +
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
                 type = "test.welch",
                 sample = sample,
                 data = data.frame(y, group),
                 formula = formula,
                 args = list(alternative = alternative, posthoc = posthoc,
                             conf.level = conf.level, hypo = hypo, descript = descript,
                             effsize = effsize, weighted = weighted, ref = ref, correct = correct,
                             plot = plot, adjust = adjust, point.size = point.size, errorbar.width = errorbar.width,
                             xlab = xlab, ylab = ylab, ylim = ylim, breaks = breaks,
                             jitter = jitter, jitter.size = jitter.size, jitter.width = jitter.width,
                             jitter.height = jitter.height, jitter.alpha = jitter.alpha,
                             title = title, subtitle = subtitle, digits = digits, p.digits = p.digits,
                             as.na = as.na, check = check, output = output),
                 plot = p, result = result)

  class(object) <- "misty.object"

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
