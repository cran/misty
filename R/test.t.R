#' t-Test
#'
#' This function performs one-sample, two-sample, and paired-sample t-tests and
#' provides descriptive statistics, effect size measure, and a plot showing error
#' bars for (difference-adjusted) confidence intervals with jittered data points.
#'
#' @param x             a numeric vector of data values.
#' @param y             a numeric vector of data values.
#' @param mu            a numeric value indicating the population mean under the
#'                      null hypothesis. Note that the argument \code{mu} is only
#'                      used when computing a one sample t-test.
#' @param paired        logical: if \code{TRUE}, paired-samples t-test is computed.
#' @param alternative   a character string specifying the alternative hypothesis,
#'                      must be one of \code{"two.sided"} (default),
#'                      \code{"greater"} or \code{"less"}.
#' @param hypo          logical: if \code{TRUE}, null and alternative hypothesis
#'                      are shown on the console.
#' @param descript      logical: if \code{TRUE}, descriptive statistics are shown
#'                      on the console.
#' @param effsize       logical: if \code{TRUE}, effect size measure Cohen's d is
#'                      shown on the console, see \code{\link{cohens.d}} function.
#' @param weighted      logical: if \code{TRUE}, the weighted pooled standard
#'                      deviation is used to compute Cohen's d for a two-sample
#'                      design (i.e., \code{paired = FALSE}), while standard deviation
#'                      of the difference scores is used to compute Cohen's d for
#'                      a paired-sample design (i.e., \code{paired = TRUE}).
#' @param cor           logical: if \code{TRUE} (default), \code{paired = TRUE},
#'                      and \code{weighted = FALSE}, Cohen's d for a paired-sample
#'                      design while controlling for the correlation between the
#'                      two sets of measurement is computed. Note that this
#'                      argument is only used in
#'                      a paired-sample design (i.e., \code{paired = TRUE}) when
#'                      specifying \code{weighted = FALSE}.
#' @param ref           character string \code{"x"} or \code{"y"} for specifying
#'                      the reference reference group when using the default
#'                      \code{test.t()} function or a numeric value or character
#'                      string indicating the reference group in a two-sample
#'                      design when using the formula \code{test.t()} function.
#'                      The standard deviation of the reference variable or
#'                      reference group is used to standardized the mean difference
#'                      to compute Cohen's d. Note that this argument is only used
#'                      in a two-sample design (i.e., \code{paired = FALSE}).
#' @param correct       logical: if \code{TRUE}, correction factor to remove
#'                      positive bias in small samples is used.
#' @param conf.level    a numeric value between 0 and 1 indicating the confidence
#'                      level of the interval.
#' @param plot          logical: if \code{TRUE}, a plot showing error bars for
#'                      confidence intervals is drawn.
#' @param point.size    a numeric value indicating the \code{size} aesthetic for
#'                      the point representing the mean value.
#' @param adjust        logical: if \code{TRUE} (default), difference-adjustment
#'                      for the confidence intervals in a two-sample design is
#'                      applied.
#' @param error.width   a numeric value indicating the horizontal bar width of
#'                      the error bar.
#' @param xlab          a character string specifying the labels for the x-axis.
#' @param ylab          a character string specifying the labels for the y-axis.
#' @param ylim          a numeric vector of length two specifying limits of the
#'                      limits of the y-axis.
#' @param breaks        a numeric vector specifying the points at which tick-marks
#'                      are drawn at the y-axis.
#' @param line          logical: if \code{TRUE} (default), a horizontal line
#'                      is drawn at \code{mu} for the one-sample t-test or at
#'                      0 for the paired-sample t-test.
#' @param line.type     an integer value or character string specifying the line
#'                      type for the line representing the population mean under
#'                      the null hypothesis, i.e., 0 = blank, 1 = solid, 2 = dashed,
#'                      3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash.
#' @param line.size     a numeric value indicating the \code{size} aesthetic
#'                      for the line representing the population mean under the
#'                      null hypothesis.
#' @param jitter        logical: if \code{TRUE} (default), jittered data points
#'                      are drawn.
#' @param jitter.size   a numeric value indicating the \code{size} aesthetic
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
#'                      i.e. these values are converted to \code{NA} before
#'                      conducting the analysis.
#' @param check         logical: if \code{TRUE}, argument specification is checked.
#' @param output        logical: if \code{TRUE}, output is shown on the console.
#' @param formula       in case of two sample t-test (i.e., \code{paired = FALSE}),
#'                      a formula of the form \code{y ~ group} where \code{group}
#'                      is a numeric variable, character variable or factor with
#'                      two values or factor levels giving the corresponding
#'                      groups.
#' @param data          a matrix or data frame containing the variables in the
#'                      formula \code{formula}.
#' @param ...           further arguments to be passed to or from methods.
#'
#' @details
#' \describe{
#' \item{\strong{Effect Size Measure}}{By default, Cohen's d based on the non-weighted
#' standard deviation (i.e., \code{weighted = FALSE}) which does not assume homogeneity
#' of variance is computed (see Delacre et al., 2021) when requesting an effect size
#' measure (i.e., \code{effsize = TRUE}). Cohen's d based on the pooled standard
#' deviation assuming equality of variancees between groups can be requested by
#' specifying \code{weighted = TRUE}.}
#' }
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{aov.b}}, \code{\link{aov.w}}, \code{\link{test.welch}}, \code{\link{test.z}},
#' \code{\link{test.levene}}, \code{\link{cohens.d}}, \code{\link{ci.mean.diff}},
#' \code{\link{ci.mean}}
#'
#' @references
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in
#' psychology - Using R and SPSS}. John Wiley & Sons.
#'
#' Delacre, M., Lakens, D., Ley, C., Liu, L., & Leys, C. (2021). Why Hedges' g*s
#' based on the non-pooled standard deviation should be reported with Welch's t-test.
#' https://doi.org/10.31234/osf.io/tu6mp
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#' \tabular{ll}{
#' \code{call} \tab function call \cr
#' \code{type} \tab type of analysis \cr
#' \code{sample} \tab type of sample, i.e., one-, two-, or paired sample \cr
#' \code{formula} \tab formula \cr
#' \code{data} \tab data frame with the outcome and grouping variable \cr
#' \code{plot} \tab ggplot2 object for plotting the results \cr
#' \code{args} \tab specification of function arguments \cr
#' \code{result} \tab list of result table \cr
#' }
#'
#' @export
#'
#' @examples
#' dat1 <- data.frame(group = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2),
#'                    x = c(3, 1, 4, 2, 5, 3, 2, 3, 6, 6, 3, NA))
#'
#' #--------------------------------------
#' # One-Sample Design
#'
#' # Two-sided one-sample t-test
#' # population mean = 3
#' test.t(dat1$x, mu = 3)
#'
#' # One-sided one-sample t-test
#' # population mean = 3, population standard deviation = 1.2
#' test.t(dat1$x, mu = 3, alternative = "greater")
#'
#' # Two-sided one-sample t-test
#' # population mean = 3, convert value 3 to NA
#' test.t(dat1$x, mu = 3, as.na = 3)
#'
#' # Two-sided one-sample t-test
#' # population mean = 3, print Cohen's d
#' test.t(dat1$x, sigma = 1.2, mu = 3, effsize = TRUE)
#'
#' # Two-sided one-sample t-test
#' # population mean = 3, print Cohen's d with small sample correction factor
#' test.t(dat1$x, sigma = 1.2, mu = 3, effsize = TRUE, correct = TRUE)
#'
#' # Two-sided one-sample t-test
#' # population mean = 3,
#' # do not print hypotheses and descriptive statistics
#' test.t(dat1$x, sigma = 1.2, mu = 3, hypo = FALSE, descript = FALSE)
#'
#' # Two-sided one-sample t-test
#' # print descriptive statistics with 3 digits and p-value with 5 digits
#' test.t(dat1$x,  mu = 3, digits = 3, p.digits = 5)
#'
#' \dontrun{
#' # Two-sided one-sample t-test
#' # population mean = 3, plot results
#' test.t(dat1$x, mu = 3, plot = TRUE)
#'
#' # Load ggplot2 package
#' library(ggplot2)
#'
#' # Save plot, ggsave() from the ggplot2 package
#' ggsave("One-sample_t-test.png", dpi = 600, width = 3, height = 6)
#'
#' # Two-sided one-sample t-test
#' # population mean = 3, extract plot
#' p <- test.t(dat1$x, mu = 3, output = FALSE)$plot
#' p
#'
#' # Extract data
#' plotdat <- data.frame(x = test.t(dat1$x, mu = 3, output = FALSE)$data[[1]])
#'
#' # Draw plot in line with the default setting of test.t()
#' ggplot(plotdat, aes(0, x)) +
#'    geom_point(stat = "summary", fun = "mean", size = 4) +
#'    stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = 0.20) +
#'    scale_x_continuous(name = NULL, limits = c(-2, 2)) +
#'    scale_y_continuous(name = NULL) +
#'    geom_hline(yintercept = 3, linetype = 3, size = 0.8) +
#'    labs(subtitle = "Two-Sided 95% Confidence Interval") +
#'    theme_bw() + theme(plot.subtitle = element_text(hjust = 0.5),
#'                       axis.text.x = element_blank(),
#'                       axis.ticks.x = element_blank())
#' }
#' #--------------------------------------
#' # Two-Sample Design
#'
#' # Two-sided two-sample t-test
#' test.t(x ~ group, data = dat1)
#'
#' # One-sided two-sample t-test
#' test.t(x ~ group, data = dat1, alternative = "greater")
#'
#' # Two-sided two-sample t-test
#' # print Cohen's d with weighted pooled SD
#' test.t(x ~ group, data = dat1, effsize = TRUE)
#'
#' # Two-sided two-sample t-test
#' # print Cohen's d with unweighted pooled SD
#' test.t(x ~ group, data = dat1, effsize = TRUE, weighted = FALSE)
#'
#' # Two-sided two-sample t-test
#' # print Cohen's d with weighted pooled SD and
#' # small sample correction factor
#' test.t(x ~ group, data = dat1, effsize = TRUE, correct = TRUE)
#'
#' # Two-sided two-sample t-test
#' # print Cohen's d with SD of the reference group 1
#' test.t(x ~ group, data = dat1, effsize = TRUE,
#'        ref = 1)
#'
#' # Two-sided two-sample t-test
#' # print Cohen's d with weighted pooled SD and
#' # small sample correction factor
#' test.t(x ~ group, data = dat1, effsize = TRUE,
#'        correct = TRUE)
#'
#' # Two-sided two-sample t-test
#' # do not print hypotheses and descriptive statistics,
#' test.t(x ~ group, data = dat1, descript = FALSE, hypo = FALSE)
#'
#' # Two-sided two-sample t-test
#' # print descriptive statistics with 3 digits and p-value with 5 digits
#' test.t(x ~ group, data = dat1, digits = 3, p.digits = 5)
#'
#' \dontrun{
#' # Two-sided two-sample t-test
#' # Plot results
#' test.t(x ~ group, data = dat1, plot = TRUE)
#'
#' # Load ggplot2 package
#' library(ggplot2)
#'
#' # Save plot, ggsave() from the ggplot2 package
#' ggsave("Two-sample_t-test.png", dpi = 600, width = 4, height = 6)
#'
#' # Two-sided two-sample t-test
#' # extract plot
#' p <- test.t(x ~ group, data = dat1, output = FALSE)$plot
#' p
#'
#' # Extract data used to plot results
#' plotdat <- test.t(x ~ group, data = dat1, output = FALSE)$data
#'
#' # Draw plot in line with the default setting of test.t()
#' ggplot(plotdat, aes(factor(group), x)) +
#'    geom_point(stat = "summary", fun = "mean", size = 4) +
#'    stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = 0.20) +
#'    scale_x_discrete(name = NULL) + scale_y_continuous(name = "y") +
#'    labs(title = "", subtitle = "Two-Sided 95% Confidence Interval") +
#'    theme_bw() + theme(plot.subtitle = element_text(hjust = 0.5))
#' }
#'
#' #-----------------
#'
#' group1 <- c(3, 1, 4, 2, 5, 3, 6, 7)
#' group2 <- c(5, 2, 4, 3, 1)
#'
#' # Two-sided two-sample t-test
#' test.t(group1, group2)
#'
#' #--------------------------------------
#' # Paired-Sample Design
#'
#' dat2 <- data.frame(pre = c(1, 3, 2, 5, 7),
#'                    post = c(2, 2, 1, 6, 8))
#'
#' # Two-sided paired-sample t-test
#' test.t(dat2$pre, dat2$post, paired = TRUE)
#'
#' # One-sided paired-sample t-test
#' test.t(dat2$pre, dat2$post, paired = TRUE,
#'        alternative = "greater")
#'
#' # Two-sided paired-sample t-test
#' # convert value 1 to NA
#' test.t(dat2$pre, dat2$post, as.na = 1, paired = TRUE)
#'
#' # Two-sided paired-sample t-test
#' # print Cohen's d based on the standard deviation of the difference scores
#' test.t(dat2$pre, dat2$post, paired = TRUE, effsize = TRUE)
#'
#' # Two-sided paired-sample t-test
#' # print Cohen's d based on the standard deviation of the difference scores
#' # with small sample correction factor
#' test.t(dat2$pre, dat2$post, paired = TRUE, effsize = TRUE,
#'        correct = TRUE)
#'
#' # Two-sided paired-sample t-test
#' # print Cohen's d controlling for the correlation between measures
#' test.t(dat2$pre, dat2$post, paired = TRUE, effsize = TRUE,
#'        weighted = FALSE)
#'
#' # Two-sided paired-sample t-test
#' # print Cohen's d controlling for the correlation between measures
#' # with small sample correction factor
#' test.t(dat2$pre, dat2$post, paired = TRUE, effsize = TRUE,
#'        weighted = FALSE, correct = TRUE)
#'
#' # Two-sided paired-sample t-test
#' # print Cohen's d ignoring the correlation between measures
#' test.t(dat2$pre, dat2$post, paired = TRUE, effsize = TRUE,
#'        weighted = FALSE, cor = FALSE)
#'
#' # Two-sided paired-sample t-test
#' # do not print hypotheses and descriptive statistics
#' test.t(dat2$pre, dat2$post, paired = TRUE, hypo = FALSE, descript = FALSE)
#'
#' # Two-sided paired-sample t-test
#' # population standard deviation of difference score = 1.2
#' # print descriptive statistics with 3 digits and p-value with 5 digits
#' test.t(dat2$pre, dat2$post, paired = TRUE, digits = 3,
#'        p.digits = 5)
#'
#' \dontrun{
#' # Two-sided paired-sample t-test
#' # Plot results
#' test.t(dat2$pre, dat2$post, paired = TRUE, plot = TRUE)
#'
#' # Load ggplot2 package
#' library(ggplot2)
#'
#' # Save plot, ggsave() from the ggplot2 package
#' ggsave("Paired-sample_t-test.png", dpi = 600, width = 3, height = 6)
#'
#' # Two-sided paired-sample t-test
#' # Extract plot
#' p <- test.t(dat2$pre, dat2$post, paired = TRUE, output = FALSE)$plot
#' p
#'
#' # Extract data used to plot results
#' plotdat <- data.frame(test.t(dat2$pre, dat2$post, paired = TRUE, output = FALSE)$data)
#'
#' # Difference score
#' plotdat$diff <- plotdat$y - plotdat$x
#'
#' # Draw plot in line with the default setting of test.t()
#' ggplot(plotdat, aes(0, diff)) +
#'    geom_point(stat = "summary", fun = "mean", size = 4) +
#'    stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = 0.20) +
#'    scale_x_discrete(name = NULL) + scale_y_continuous(name = NULL) +
#'    geom_hline(yintercept = 0, linetype = 3, size = 0.8) +
#'    labs(subtitle = "Two-Sided 95% Confidence Interval") +
#'    theme_bw() + theme(plot.subtitle = element_text(hjust = 0.5),
#'                       axis.text.x = element_blank(),
#'                       axis.ticks.x = element_blank())
#' }
test.t <- function(x, ...) {

  UseMethod("test.t")

}

#_______________________________________________________________________________
#
# Default S3 method ------------------------------------------------------------

test.t.default <- function(x, y = NULL, mu = 0, paired = FALSE,
                           alternative = c("two.sided", "less", "greater"), conf.level = 0.95,
                           hypo = TRUE, descript = TRUE, effsize = FALSE, weighted = FALSE,
                           cor = TRUE, ref = NULL, correct = FALSE,
                           plot = FALSE, point.size = 4, adjust = TRUE, error.width = 0.1,
                           xlab = NULL, ylab = NULL, ylim = NULL, breaks = ggplot2::waiver(),
                           line = TRUE, line.type = 3, line.size = 0.8,
                           jitter = TRUE, jitter.size = 1.25, jitter.width = 0.05,
                           jitter.height = 0, jitter.alpha = 0.1,
                           title = "", subtitle = "Confidence Interval",
                           digits = 2, p.digits = 4, as.na = NULL, check = TRUE,
                           output = TRUE, ...) {

  # Check if input 'x' is missing
  if (isTRUE(missing(x))) { stop("Please specify a numeric vector for the argument 'x'", call. = FALSE) }

  # Check if input 'x' is NULL
  if (isTRUE(is.null(x))) { stop("Input specified for the argument 'x' is NULL.", call. = FALSE) }

  # Check input 'paired'
  if (isTRUE(!is.logical(paired))) { stop("Please specify TRUE or FALSE for the argument 'paired'.", call. = FALSE) }

  # Check if only one variable specified in the input 'x'
  if (ncol(data.frame(x)) != 1L) { stop("More than one variable specified for the argument 'x'.", call. = FALSE) }

  # Convert 'x' into a vector
  x <- unlist(x, use.names = FALSE)

  # Check 'y'
  if (!is.null(y)) {

    # Check if only one variable specified in the input 'y'
    if (ncol(data.frame(y)) != 1) { stop("More than one variable specified for the argument 'y'.",call. = FALSE) }

    # Convert 'y' into a vector
    y <- unlist(y, use.names = FALSE)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) {

    # One sample
    if (isTRUE(is.null(y))) {

      # Replace user-specified values with missing values
      x <- misty::as.na(x, na = as.na, check = check)

      if (isTRUE(all(is.na(x)))) { stop("After converting user-missing values into NA, 'x' is completely missing.", call. = FALSE) }

    # Two or paired sample
    } else {

      # Replace user-specified values with missing values
      x <- misty::as.na(x, na = as.na, check = check)
      y <- misty::as.na(y, na = as.na, check = check)

      if (isTRUE(!is.null(y))) {

        # Variable with missing values only
        xy.miss <- vapply(list(x = x, y = y), function(y) all(is.na(y)), FUN.VALUE = logical(1))
        if (isTRUE(any(xy.miss))) {

          stop(paste0("After converting user-missing values into NA, following ",
                      ifelse(sum(xy.miss) == 1L, "variable is ", "variables are "),
                      "completely missing: ",
                      paste(names(which(xy.miss)), collapse = ", ")), call. = FALSE)

        }

      }

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Paired sample ####

  if (isTRUE(is.null(y) && isTRUE(paired))) {

    # Length of 'x' and 'y'
    if (isTRUE(length(x) != length(y))) { stop("Length of the vector specified in 'x' does not match the length of the vector specified in 'y'.", call. = FALSE) }

    # Listwise deletion
    if (isTRUE(nrow(na.omit(data.frame(x = x, y = y))) < 2L)) { stop("After listwise deletion, the number of pairs of observations is less than two.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # ggplot2 package
    if (isTRUE(!nzchar(system.file(package = "ggplot2")))) { warning("Package \"ggplot2\" is needed for drawing a bar chart, please install the package.", call. = FALSE) }

    # Check input 'mu'
    if (isTRUE(length(mu) > 1L)) { stop("Please specify one numeric value for the argument 'mu'.", call. = FALSE) }

    # Check input 'alternative'
    if (isTRUE(!all(alternative %in%  c("two.sided", "less", "greater")))) { stop("Character string in the argument 'alternative' does not match with \"two.sided\", \"less\", or \"greater\".", call. = FALSE) }

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

    # Check input 'cor'
    if (isTRUE(!is.logical(cor))) { stop("Please specify TRUE or FALSE for the argument 'cor'.", call. = FALSE) }

    # Check input 'ref'
    if (isTRUE(!is.null(ref))) { if (isTRUE(!isTRUE(ref %in% c("x", "y")))) { stop("Please specify \"x\" or \"y\" for the argument 'ref'.", call. = FALSE) } }

    # Check input 'correct'
    if (isTRUE(!is.logical(correct))) { stop("Please specify TRUE or FALSE for the argument 'correct'.", call. = FALSE) }

    # Check input 'plot'
    if (isTRUE(!is.logical(plot))) { stop("Please specify TRUE or FALSE for the argument 'plot'.", call. = FALSE) }

    # Check input 'adjust'
    if (isTRUE(!is.logical(adjust))) { stop("Please specify TRUE or FALSE for the argument 'adjust'.", call. = FALSE) }

    # Check input 'line'
    if (isTRUE(!is.logical(line))) { stop("Please specify TRUE or FALSE for the argument 'line'.", call. = FALSE) }

    # Check input 'jitter'
    if (isTRUE(!is.logical(jitter))) { stop("Please specify TRUE or FALSE for the argument 'jitter'.", call. = FALSE) }

    # Check input 'digits'
    if (isTRUE(digits %% 1L != 0L || digits < 0L)) { stop("Please specify a positive integer number for the argument 'digits'.", call. = FALSE) }

    # Check input 'digits'
    if (isTRUE(p.digits %% 1L != 0L || p.digits < 0L)) { stop("Please specify a positive integer number for the argument 'p.digits'.", call. = FALSE) }

    # Check input 'output'
    if (isTRUE(!is.logical(output))) { stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  # Global variables
  m <- m.low <- m.upp <- group <- low <- upp <- m.diff <- NULL

  #...................
  ### Alternative hypothesis ####

  if (isTRUE(all(c("two.sided", "less", "greater") %in% alternative))) { alternative <- "two.sided" }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## One-sample design ####
  if (isTRUE(is.null(y))) {

    # Confidence intervals
    x.ci <- misty::ci.mean.diff(x = x, y = NULL, alternative = alternative,
                                conf.level = conf.level, check = FALSE, output = FALSE)$result

    # Cohen's d
    d <- misty::cohens.d(x = x, y = NULL, paired = FALSE, mu = mu,
                         weighted = FALSE, cor = TRUE, ref = NULL, correct = correct,
                         alternative = alternative, conf.level = conf.level,
                         group = NULL, split = NULL, sort.var = FALSE,
                         check = FALSE, output = FALSE)$result

    # t-test
    t <- t.test(x = x, y = y, mu = mu, alternative = alternative)

    result <- data.frame(n = x.ci[["n"]], nNA = x.ci[["nNA"]],
                         m = x.ci[["m"]], sd = x.ci[["sd"]],
                         m.diff = x.ci[["m"]] - mu, se = t$stderr,
                         m.low = x.ci[["low"]], m.upp = x.ci[["upp"]],
                         t = t$statistic, df = t$parameter,
                         pval = t$p.value, d = d$d, d.low = d$low, d.upp = d$upp,
                         row.names = NULL, check.names = FALSE)

    sample <- "one"

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Two samples design ####
  } else if (isTRUE(!is.null(y) && !isTRUE(paired))) {

    # Confidence intervals
    x.ci <- misty::df.rename(misty::ci.mean.diff(x = x, y = y, alternative = alternative, conf.level = conf.level, output = FALSE)$result,
                             from = c("between", "low", "upp"), to = c("group", "m.low", "m.upp"))

    # Cohen's d
    d <- misty::cohens.d(x = x, y = y, paired = FALSE, mu = 0L, weighted = weighted,
                         cor = TRUE, ref = ref, correct = correct,
                         alternative = alternative, conf.level = conf.level,
                         group = NULL, split = NULL, sort.var = FALSE,
                         check = FALSE, output = FALSE)$result
    # t-test
    t <- t.test(x = x, y = y, alternative = switch(alternative,
                                                   two.sided = "two.sided",
                                                   greater = "less",
                                                   less = "greater"), var.equal = TRUE)

    result <- data.frame(cbind(x.ci[, c("group", "n", "nNA", "m", "sd", "m.diff")],
                               se = c(NA, t$stderr), x.ci[, c("m.low", "m.upp")],
                               t = c(NA, t$statistic)*-1L, df = c(NA, t$parameter), pval = c(NA, t$p.value),
                               d = d$d, d.low = d$low, d.upp = d$upp),
                         row.names = NULL, check.names = FALSE)

    sample <- "two"

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Paired samples ####
  } else if (isTRUE(!is.null(y) && isTRUE(paired))) {

    # Confidence intervals
    x.ci <- misty::ci.mean.diff(x = x, y = y, paired = TRUE, alternative = alternative,
                                conf.level = conf.level, output = FALSE)$result

    # Cohen's d
    d <- misty::cohens.d(x = x, y = y, paired = TRUE, mu = 0L, weighted = weighted,
                         cor = cor, ref = ref, correct = correct,
                         alternative = alternative, conf.level = conf.level,
                         group = NULL, split = NULL, sort.var = FALSE,
                         check = FALSE, output = FALSE)$result

    # t-test
    t <- t.test(x = x, y = y, alternative = switch(alternative,
                                                   two.sided = "two.sided",
                                                   greater = "less",
                                                   less = "greater"), paired = TRUE)

    result <- data.frame(n = x.ci[["n"]], nNA = x.ci[["nNA"]],
                         m1 = x.ci[["m1"]], m2 = x.ci[["m2"]],
                         m.diff = x.ci[["m.diff"]], sd.diff = x.ci[["sd.diff"]],
                         se = t$stderr, m.low = x.ci[["low"]], m.upp = x.ci[["upp"]],
                         t = t$statistic*-1, df = t$parameter, pval = t$p.value,
                         d = d$d, d.low = d$low, d.upp = d$upp,
                         row.names = NULL, check.names = FALSE)

    sample <- "paired"

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Plot ####

  switch(sample,
         #...................
         ### One-sample ####
         "one" = {

           # Plot data
           plotdat <- data.frame(x = x)

           # Plot Subtitle
           if (isTRUE(subtitle == "Confidence Interval")) { subtitle <- paste0(ifelse(alternative == "two.sided", "Two-Sided ", "One-Sided "),
                                                                               round(conf.level * 100, digits = 2), "% Confidence Interval") }

           # Crease ggplot
           p <- ggplot2::ggplot(plotdat, ggplot2::aes(x = 0L, y = x))

           # Add jittered points
           if (isTRUE(jitter)) { p <- p + ggplot2::geom_jitter(alpha = jitter.alpha, width = jitter.width, height = jitter.height, size = jitter.size) }

           p <-  p + ggplot2::geom_point(data = result, ggplot2::aes(x = 0L, m), size = point.size) +
                   ggplot2::geom_errorbar(data = result, ggplot2::aes(x = 0L, y = m, ymin = m.low, ymax = m.upp), width = error.width) +
                   ggplot2::scale_x_continuous(name = xlab, limits = c(-2L, 2L)) +
                   ggplot2::scale_y_continuous(name = ylab, limits = ylim, breaks = breaks) +
                   ggplot2::labs(title = title, subtitle = subtitle) +
                   ggplot2::theme_bw() + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                                                        plot.subtitle = ggplot2::element_text(hjust = 0.5),
                                                        axis.text.x = ggplot2::element_blank(),
                                                        axis.ticks.x = ggplot2::element_blank())

           # Add horizontal line
           if (isTRUE(line)) { p <- p + ggplot2::geom_hline(yintercept = mu, linetype = line.type, size = line.size) }

          #...................
          ### Two-sample ####
         }, "two" = {

           # Plot data
           plotdat <- data.frame(group = factor(c(rep("x", times = length(x)), rep("y", times = length(y)))), y = c(x, y))

           # Plot Subtitle
           if (isTRUE(subtitle == "Confidence Interval")) { subtitle <- paste0("Two-Sided ", round(conf.level * 100, digits = 2), "% Confidence Interval") }

           # Confidence interval
           plot.ci <- misty::ci.mean(plotdat[, "y"], group = plotdat[, "group"], adjust = adjust,
                                     conf.level = conf.level, output = FALSE)$result

           # Crease ggplot
           p <- ggplot2::ggplot(plotdat, ggplot2::aes(group, y))

           # Add jittered points
           if (isTRUE(jitter)) { p <- p + ggplot2::geom_jitter(alpha = jitter.alpha, width = jitter.width, size = jitter.size) }

           p <- p + ggplot2::geom_point(data = plot.ci, ggplot2::aes(group, m), stat = "identity", size = point.size) +
                 ggplot2::geom_errorbar(data = plot.ci, ggplot2::aes(group, m, ymin = low, ymax = upp), width = error.width) +
                 ggplot2::scale_x_discrete(name = xlab) +
                 ggplot2::scale_y_continuous(name = ylab, limits = ylim, breaks = breaks) +
                 ggplot2::theme_bw() +
                 ggplot2::labs(title = title, subtitle = subtitle) +
                 ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5), plot.title = ggplot2::element_text(hjust = 0.5))

          #...................
          ### Paired-sample ####
          }, "paired" = {

            # Plot data
            plotdat <- data.frame(x = y - x)

            # Plot Subtitle
            if (isTRUE(subtitle == "Confidence Interval")) { subtitle <- paste0(ifelse(alternative == "two.sided", "Two-Sided ", "One-Sided "),
                                                                                round(conf.level * 100L, digits = 2L), "% Confidence Interval") }
            # Crease ggplot
            p <- ggplot2::ggplot(plotdat, ggplot2::aes(x = 0L, y = x))

            # Add jittered points
            if (isTRUE(jitter)) { p <- p + ggplot2::geom_jitter(alpha = jitter.alpha, width = jitter.width, size = jitter.size) }

            p <- p + ggplot2::geom_point(data = result,
                                         ggplot2::aes(x = 0L, y = m.diff), size = point.size) +
                  ggplot2::geom_errorbar(data = result,
                                         ggplot2::aes(x = 0L, y = m.diff, ymin = m.low, ymax = m.upp), width = error.width) +
                  ggplot2::scale_x_continuous(name = xlab, limits = c(-2L, 2L)) +
                  ggplot2::scale_y_continuous(name = ylab, limits = ylim, breaks = breaks) +
                  ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank()) +
                  ggplot2::labs(title = title, subtitle = subtitle) +
                  ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5),
                                 plot.title = ggplot2::element_text(hjust = 0.5))

            # Add horizontal line
            if (isTRUE(line)) { p <- p + ggplot2::geom_hline(yintercept = 0, linetype = line.type, size = line.size) }

          })

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Print plot ####
  if (isTRUE(plot)) {

    suppressWarnings(print(p))

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "test.t",
                 sample = sample,
                 data = list(x = x, y = y),
                 plot = p,
                 args = list(mu = mu, paired = paired, alternative = alternative,
                             conf.level = conf.level, hypo = hypo, descript = descript,
                             effsize = effsize, weighted = weighted, cor = cor, ref = ref,
                             correct = correct, conf.level = conf.level,
                             plot = plot, point.size = point.size, adjust = adjust,
                             error.width = error.width, xlab = xlab, ylab = ylab,
                             ylim = ylim, breaks = breaks, line = line,
                             line.type = line.type, line.size = line.size,
                             jitter = jitter, jitter.size = jitter.size, jitter.width = jitter.width,
                             jitter.height = jitter.height, jitter.alpha = jitter.alpha,
                             title = title, subtitle = subtitle, digits = digits,
                             p.digits = p.digits, as.na = as.na, check = check,
                             output = output),
                 result = result)

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}

#_______________________________________________________________________________
#
# S3 method for class 'formula' ------------------------------------------------

test.t.formula <- function(formula, data, alternative = c("two.sided", "less", "greater"),
                           conf.level = 0.95, hypo = TRUE, descript = TRUE, effsize = FALSE,
                           weighted = FALSE, cor = TRUE, ref = NULL, correct = FALSE,
                           plot = FALSE, point.size = 4, adjust = TRUE, error.width = 0.1,
                           xlab = NULL, ylab = NULL, ylim = NULL, breaks = ggplot2::waiver(),
                           jitter = TRUE, jitter.size = 1.25, jitter.width = 0.05,
                           jitter.height = 0, jitter.alpha = 0.1,
                           title = "", subtitle = "Confidence Interval",
                           digits = 2, p.digits = 4, as.na = NULL, check = TRUE,
                           output = TRUE, ...) {

  # Check if input 'formula' is missing
  if (isTRUE(missing(formula))) { stop("Please specify a formula using the argument 'formula'", call. = FALSE) }

  # Check if input 'data' is missing
  if (isTRUE(missing(data))) { stop("Please specify a matrix or data frame for the argument 'x'.", call. = FALSE) }

  # Check if input 'data' is NULL
  if (isTRUE(is.null(data))) { stop("Input specified for the argument 'data' is NULL.", call. = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Formula ####

  #...................
  ### Variables ####

  var.formula <- all.vars(as.formula(formula))

  # Grouping variable
  group.var <- attr(terms(formula[-2L]), "term.labels")

  # Outcome(s)
  y.vars <- var.formula[-grep(group.var, var.formula)]

  #...................
  ### Check ####

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

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
    if (isTRUE(length(y.vars) != 1L)) { stop("Please specify a formula with only one outcome variable.", call. = FALSE) }

  }

  #...................
  ### Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) {

    # Replace user-specified values with missing values
    data[, y.vars] <- misty::as.na(data[, y.vars], na = as.na, check = check)

    # Variable with missing values only
    data.miss <- vapply(data[, y.vars, drop = FALSE], function(y) all(is.na(y)), FUN.VALUE = logical(1))
    if (any(data.miss)) {

      stop(paste0("After converting user-missing values into NA, following ",
                  ifelse(sum(data.miss) == 1L, "variable is ", "variables are "),
                  "completely missing: ", paste(names(which(data.miss)), collapse = ", ")), call. = FALSE)

    }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  # Global variables
  group <- m <- low <- upp <- NULL

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Alternative hypothesis ####

  if (isTRUE(all(c("two.sided", "less", "greater") %in% alternative))) { alternative <- "two.sided" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Reference group ####

  ref.return <- ref

  # Check if input 'data' is NULL
  if (isTRUE(!is.null(ref))) {

    if (isTRUE(!ref %in% na.omit(unlist(data[, group.var])))) {

      stop("Reference group specified in the argument 'ref' is not represented in the grouping variable.",
           call. = FALSE)

    }

    ifelse(which(unique(sort(na.omit(unlist(data[, group.var])))) %in% ref) == 1, ref <- "x", ref <- "y")

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  # Split data
  data.split <- split(unlist(data[, y.vars]), f = unlist(data[, group.var]))

  # # Default S3 method
  object <- test.t.default(x = data.split[[1L]], y = data.split[[2L]], alternative = alternative,
                           conf.level = conf.level, hypo = hypo, descript = descript, effsize = effsize,
                           weighted = weighted, cor = cor, ref = ref, correct = correct,
                           plot = FALSE, point.size = point.size, adjust = adjust,
                           error.width = error.width, xlab = xlab, ylab = ylab,
                           ylim = ylim, breaks = breaks, jitter = jitter,
                           jitter.size = jitter.size, jitter.width = jitter.width,
                           jitter.height = jitter.height, jitter.alpha = jitter.alpha, title = title,
                           subtitle = subtitle, check = check, output = FALSE)

  object$result[, "group"] <- names(data.split)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Plot ####

  # Label x-axis
  p <- suppressMessages(object$plot + ggplot2::scale_x_discrete(labels = names(data.split)))

  # Print plot
  if (isTRUE(plot)) { suppressWarnings(print(p)) }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "test.t",
                 sample = "two",
                 data = data[, var.formula],
                 formula = formula,
                 plot = p,
                 args = list(alternative = alternative,
                             conf.level = conf.level, hypo = hypo, descript = descript,
                             effsize = effsize, weighted = weighted, cor = cor,
                             ref = ref.return, correct = correct, plot = plot,
                             point.size = point.size, adjust = adjust,
                             error.width = error.width, xlab = xlab, ylab = ylab,
                             ylim = ylim, breaks = breaks, jitter = jitter,
                             jitter.size = jitter.size, jitter.width = jitter.width,
                             jitter.height = jitter.height, jitter.alpha = jitter.alpha,
                             title = title, subtitle = subtitle, digits = digits, p.digits = p.digits,
                             as.na = as.na, check = check, output = output),
                 result = object$result)

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
