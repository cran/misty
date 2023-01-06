#' Repeated Measures Analysis of Variance (Within-Subject ANOVA)
#'
#' This function performs an one-way repeated measures analysis of variance (within
#' subject ANOVA) including paired-samples t-tests for multiple comparison and
#' provides descriptive statistics, effect size measures, and a plot showing error
#' bars for difference-adjusted Cousineau-Morey within-subject confidence intervals
#' with jittered data points including subject-specific lines.
#'
#' @param formula       a formula of the form \code{cbind(time1, time2, time3) ~ 1}
#'                      where \code{time1}, \code{time2}, and \code{time3} are
#'                      numeric variables representing the levels of the within-
#'                      subject factor, i.e., data are specified in wide-format
#'                      (i.e., multivariate person level format).
#' @param data          a matrix or data frame containing the variables in the
#'                      formula \code{formula}.
#' @param print         a character vector indicating which sphericity correction
#'                      to use, i.e., \code{all} for all corrections, \code{none}
#'                      for no correction, \code{LB} for lower bound correction,
#'                      \code{GG} for Greenhouse-Geisser correction, and \code{HF},
#'                      for Huynh-Feldt correction.
#' @param posthoc       logical: if \code{TRUE}, paired-samples t-tests for multiple
#'                      comparison are conducted.
#' @param conf.level    a numeric value between 0 and 1 indicating the confidence
#'                      level of the interval.
#' @param p.adj         a character string indicating an adjustment method for
#'                      multiple testing based on \code{\link{p.adjust}}, i.e.,
#'                      \code{none}, \code{bonferroni}, \code{holm} (default),
#'                      \code{h ochberg}, \code{hommel}, \code{BH}, \code{BY}, or
#'                      \code{fdr}.
#' @param hypo          logical: if \code{TRUE}, null and alternative hypothesis
#'                      are shown on the console.
#' @param descript      logical: if \code{TRUE}, descriptive statistics are shown
#'                      on the console.
#' @param epsilon       logical: if \code{TRUE}, box indices of sphericity (epsilon)
#'                      are shown on the console, i.e., lower bound, Greenhouse
#'                      and Geiser (GG), Huynh and Feldt (HF) and average of GG
#'                      and HF.
#' @param effsize       logical: if \code{TRUE}, effect size measures eta-squared
#'                      (\eqn{\eta^2}), partial eta-squared (\eqn{\eta^2_p}),
#'                      omega-squared (\eqn{\omega^2}), and partial omega-squared
#'                      (\eqn{\omega^2_p}) for the repeated measures ANOVA and
#'                      Cohen's \emph{d} for the post hoc tests are shown on
#'                      the console.
#' @param na.omit       logical: if \code{TRUE}, incomplete cases are removed
#'                      before conducting the analysis (i.e., listwise deletion).
#' @param plot          logical: if \code{TRUE}, a plot showing error bars for
#'                      confidence intervals is drawn.
#' @param point.size    a numeric value indicating the \code{size} aesthetic for
#'                      the point representing the mean value.
#' @param adjust        logical: if \code{TRUE} (default), difference-adjustment
#'                      for the Cousineau-Morey within-subject confidence
#'                      intervals is applied.
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
#' @param line          logical: if \code{TRUE} (default), subject-specific lines
#'                      are drawn.
#' @param jitter.size   a numeric value indicating the \code{size} aesthetic
#'                      for the jittered data points.
#' @param jitter.width  a numeric value indicating the amount of horizontal jitter.
#' @param jitter.height a numeric value indicating the amount of vertical jitter.
#' @param jitter.alpha  a numeric value indicating the opacity of the jittered
#'                      data points.
#' @param title         a character string specifying the text for the title for
#'                      the plot.
#' @param subtitle      a character string specifying the text for the subtitle
#'                      for the plot.
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
#' @param ...           further arguments to be passed to or from methods.
#'
#' @details
#' \describe{
#' \item{\strong{Sphericity}}{The \emph{F}-Test of the repeated measures ANOVA
#' is based on the assumption of sphericity, which is defined as the assumption
#' that the variance of differences between repeated measures are equal in the
#' population. The Mauchly's test is commonly used to test this hypothesis.
#' However, test of assumptions addresses an irrelevant hypothesis because what
#' matters is the degree of violation rather than its presence (Baguley, 2012a).
#' Moreover, the test is not recommended because it lacks statistical power (Abdi,
#' 2010). Instead, the Box index of sphericity (\eqn{\varepsilon}) should be used to
#' assess the degree of violation of the sphericity. assumption The \eqn{\varepsilon}
#' parameter indicates the degree to which the population departs from sphericity
#' with \eqn{\varepsilon = 1} indicating that sphericity holds. As the departure
#' becomes more extreme, \eqn{\varepsilon} approaches its lower bound
#' \eqn{\hat{\varepsilon}_{lb}}:
#'
#' \deqn{\hat{\varepsilon}_{lb} = \frac{1}{J - 1}}
#'
#' where \eqn{J} is the number of levels of the within-subject factor. Box (1954a,
#' 1954b) suggested a measure for sphericity, which applies to a population
#' covariance matrix. Greenhouse and Geisser (1959) proposed an estimate for
#' \eqn{\varepsilon} known as \eqn{\hat{\varepsilon}_{gg}} that can be computed
#' from the sample covariance matrix, whereas Huynh and Feldt (1976) proposed
#' an alternative estimate \eqn{\hat{\varepsilon}_{hf}}. These estimates can
#' be used to correct the effect and error \emph{df} of the \emph{F}-test.
#' Simulation studies showed that \eqn{\hat{\varepsilon}_{gg} \leq \hat{\varepsilon}_{hf}}
#' and that \eqn{\hat{\varepsilon}_{gg}} tends to be conservative underestimating
#' \eqn{\varepsilon}, whereas \eqn{\hat{\varepsilon}_{hf}} tends to be liberal
#' overestimating \eqn{\varepsilon} and occasionally exceeding one. Baguley (2012a)
#' recommended to compute the average of the conservative estimate \eqn{\hat{\varepsilon}_{gg}}
#' and the liberal estimate \eqn{\hat{\varepsilon}_{hf}} to assess the sphericity
#' assumption.
#' By default, the function prints results depending on the average
#' \eqn{\hat{\varepsilon}_{gg}} and \eqn{\hat{\varepsilon}_{hf}}:
#' \itemize{
#'      \item If the average is less than 0.75 results of the \emph{F}-Test based on
#'      Greenhouse-Geiser correction factor (\eqn{\hat{\varepsilon}_{gg}}) is printed.
#'      \item If the average is less greater or equal 0.75, but less than 0.95
#'      results of the \emph{F}-Test based on Huynh-Feldt correction factor
#'      (\eqn{\hat{\varepsilon}_{hf}}) is printed.
#'      \item If the average is greater or equal 0.95 results of the \emph{F}-Test
#'      without any corrections are printed.
#' }
#' }
#' \item{\strong{Missing Data}}{The function uses listwise deletion by default to
#' deal with missing data. However, the function also allows to use all available
#' observations by conducting the repeated measures ANOVA in long data format when
#' specifying \code{na.omit = FALSE}. Note that in the presence of missing data,
#' the \emph{F}-Test without any sphericity corrections may be reliable, but it
#' is not clear whether results based on Greenhouse-Geiser or Huynh-Feldt correction
#' are trustworthy given that pairwise deletion is used for estimating the
#' variance-covariance matrix when computing \eqn{\hat{\varepsilon}_{gg}} and the total
#' number of subjects regardless of missing values (i.e., complete and incomplete
#' cases) are used for computing \eqn{\hat{\varepsilon}_{hf}}.
#' }
#' \item{\strong{Within-Subject Confidence Intervals}}{The function provides a
#' plot showing error bars for difference-adjusted Cousineau-Morey confidence
#' intervals (Baguley, 2012b). The intervals matches that of a CI for a difference,
#' i.e., non-overlapping CIs corresponds to an inferences of no statistically
#' significant difference. The Cousineau-Morey confidence intervals without
#' adjustment can be used by specifying \code{adjust = FALSE}.
#' }
#' }
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{aov.b}}, \code{\link{test.t}}, \code{\link{test.z}},
#' \code{\link{cohens.d}}, \code{\link{ci.mean.diff}}, \code{\link{ci.mean}}
#'
#' @references
#' Abdi, H. (2010). The Greenhouse-Geisser correction. In N. J. Salkind (Ed.)
#' \emph{Encyclopedia of Research Design} (pp. 630-634), Sage.
#' https://dx.doi.org/10.4135/9781412961288
#'
#' Baguley, T. S. (2012a). \emph{Serious stats: A guide to advanced statistics for the
#' behavioral sciences}. Palgrave Macmillan.
#'
#' Baguley, T. (2012b). Calculating and graphing within-subject confidence intervals
#' for ANOVA. \emph{Behavior Research Methods, 44}, 158-175.
#' https://doi.org/10.3758/s13428-011-0123-7
#'
#' Bakerman, R. (2005). Recommended effect size statistics for repeated measures
#' designs. \emph{Behavior Research Methods}, 37, 179-384.
#' https://doi.org/10.3758/BF03192707
#'
#' Box, G. E. P. (1954a) Some Theorems on Quadratic Forms Applied in the Study
#' of Analysis of Variance Problems, I. Effects of Inequality of Variance in
#' the One-way Classification. \emph{Annals of Mathematical Statistics, 25},
#' 290–302.
#'
#' Box, G. E. P. (1954b) Some Theorems on Quadratic Forms Applied in the Study
#' of Analysis of Variance Problems, II. Effects of Inequality of Variance and
#' of Correlation between Errors in the Two-way Classification.
#' \emph{Annals of Mathematical Statistics, 25}, 484–98.
#'
#' Greenhouse, S. W., and Geisser, S. (1959). On methods in the analysis of profile
#' data.\emph{Psychometrika, 24}, 95–112. https://doi.org/10.1007/BF02289823
#'
#' Huynh, H., and Feldt, L. S. (1976). Estimation of the box correction for degrees
#' of freedom from sample data in randomized block and splitplot designs.
#' \emph{Journal of Educational Statistics, 1}, 69–82.
#' https://doi.org/10.2307/1164736
#'
#' Olejnik, S., & Algina, J. (2000). Measures of effect size for comparative studies:
#' Applications, interpretations, and limitations. \emph{Contemporary Educational
#' Psychology, 25}, 241–286. https://doi.org/10.1006/ceps.2000.1040
#'
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology
#' - Using R and SPSS}. John Wiley & Sons.
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries: function call (\code{call}), type of analysis (\code{type}), list with
#' the data (\code{data}) in wide-format (\code{wide}), reshaped data in long-format
#' (\code{long}), and within-subject confidence intervals (\code{ci}), the ggplot2
#' object (\code{plot}), and specification of function arguments (\code{args}),
#' and result tables (\code{result}).
#'
#' @export
#'
#' @examples
#' dat <- data.frame(time1 = c(3, 2, 1, 4, 5, 2, 3, 5, 6, 7),
#'                   time2 = c(4, 3, 6, 5, 8, 6, 7, 3, 4, 5),
#'                   time3 = c(1, 2, 2, 3, 6, 5, 1, 2, 4, 6))
#'
#' # Repeated measures ANOVA
#' aov.w(cbind(time1, time2, time3) ~ 1, data = dat)
#'
#' # Repeated measures ANOVA
#' # print results based on all sphericity corrections
#' aov.w(cbind(time1, time2, time3) ~ 1, data = dat, print = "all")
#'
#' # Repeated measures ANOVA
#' # print effect size measures
#' aov.w(cbind(time1, time2, time3) ~ 1, data = dat, effsize = TRUE)
#'
#' # Repeated measures ANOVA
#' # do not print hypotheses and descriptive statistics,
#' aov.w(cbind(time1, time2, time3) ~ 1, data = dat, descript = FALSE, hypo = FALSE)
#'
#' \dontrun{
#'  # Repeated measures ANOVA
#'  # plot results
#'  aov.w(cbind(time1, time2, time3) ~ 1, data = dat, plot = TRUE)
#'
#'  # Load ggplot2 package
#'  library(ggplot2)
#'
#'  # Save plot, ggsave() from the ggplot2 package
#'  ggsave("Repeated_measures_ANOVA.png", dpi = 600, width = 4.5, height = 4)
#'
#'  # Repeated measures ANOVA
#'  # extract plot
#'  p <- aov.w(cbind(time1, time2, time3) ~ 1, data = dat, output = FALSE)$plot
#'  p
#'
#'  # Extract data
#'  plotdat <- aov.w(cbind(time1, time2, time3) ~ 1, data = dat, output = FALSE)$data
#'
#'  # Draw plot in line with the default setting of aov.w()
#'  ggplot(plotdat$long, aes(time, y, group = 1L)) +
#'    geom_point(aes(time, y, group = id),
#'               alpha = 0.1, position = position_dodge(0.05)) +
#'    geom_line(aes(time, y, group = id),
#'              alpha = 0.1, position = position_dodge(0.05)) +
#'    geom_point(data = plotdat$ci, aes(variable, m), stat = "identity", size = 4) +
#'    stat_summary(aes(time, y), fun = mean, geom = "line") +
#'    geom_errorbar(data = plotdat$ci, aes(variable, m, ymin = low, ymax = upp), width = 0.1) +
#'    theme_bw() + xlab(NULL) +
#'    labs(subtitle = "Two-Sided 95% Confidence Interval") +
#'    theme(plot.subtitle = element_text(hjust = 0.5),
#'          plot.title = element_text(hjust = 0.5))
#' }
aov.w <- function(formula, data, print = c("all", "none", "LB", "GG", "HF"),
                  posthoc = TRUE, conf.level = 0.95,
                  p.adj = c("none", "bonferroni", "holm", "hochberg", "hommel", "BH", "BY", "fdr"),
                  hypo = TRUE, descript = TRUE, epsilon = TRUE, effsize = FALSE, na.omit = TRUE,
                  plot = FALSE, point.size = 4, adjust = TRUE, error.width = 0.1,
                  xlab = NULL, ylab = NULL, ylim = NULL, breaks = ggplot2::waiver(),
                  jitter = TRUE, line = TRUE, jitter.size = 1.25, jitter.width = 0.05,
                  jitter.height = 0, jitter.alpha = 0.1, title = "",
                  subtitle = "Confidence Interval", digits = 2, p.digits = 4,
                  as.na = NULL, check = TRUE, output = TRUE, ...) {

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

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  # ggplot2 package
  if (isTRUE(!nzchar(system.file(package = "ggplot2"))))  { warning("Package \"ggplot2\" is needed for drawing a bar chart, please install the package.", call. = FALSE) }

  if (isTRUE(check)) {

    # Check if variables are in the data
    var.data <- !var.formula %in% colnames(data)
    if (isTRUE(any(var.data))) {

      stop(paste0("Variables specified in the formula were not found in 'data': ",
                  paste(var.formula[which(var.data)], collapse = ", ")), call. = FALSE)

    }

    # Check input 'print'
    if (isTRUE(any(!print %in% c("all", "none", "LB",  "GG", "HF")))) { stop("Character strings in the argument 'print' do not all match with \"all\", \"none\", \"LB\", \"GG\", or \"HF\".", call. = FALSE) }

    # Check input 'posthoc'
    if (isTRUE(!is.logical(posthoc))) { stop("Please specify TRUE or FALSE for the argument 'posthoc'.", call. = FALSE) }

    # Check input 'conf.level'
    if (isTRUE(conf.level >= 1L || conf.level <= 0L)) { stop("Please specifiy a numeric value between 0 and 1 for the argument 'conf.level'.", call. = FALSE) }

    # Check input 'p.adj'
    if (isTRUE(any(!p.adj %in% c("none", "holm", "bonferroni", "hochberg", "hommel", "BH", "BY", "fdr")))) { stop("Character string in the argument 'p.adj' does not match with \"none\", \"bonferroni\", \"holm\", \"hochberg\", \"hommel\", \"BH\", \"BY\", or \"fdr\".", call. = FALSE) }

    # Check input 'hypo'
    if (isTRUE(!is.logical(hypo))) { stop("Please specify TRUE or FALSE for the argument 'hypo'.", call. = FALSE) }

    # Check input 'descript'
    if (isTRUE(!is.logical(descript))) { stop("Please specify TRUE or FALSE for the argument 'descript'.", call. = FALSE) }

    # Check input 'effsize'
    if (isTRUE(!is.logical(effsize))) { stop("Please specify TRUE or FALSE for the argument 'effsize'.", call. = FALSE) }

    # Check input 'na.omit'
    if (isTRUE(!is.logical(na.omit))) { stop("Please specify TRUE or FALSE for the argument 'na.omit'.", call. = FALSE) }

    # Check input 'plot'
    if (isTRUE(!is.logical(plot))) { stop("Please specify TRUE or FALSE for the argument 'plot'.", call. = FALSE) }

    # Check input 'adjust'
    if (isTRUE(!is.logical(adjust))) { stop("Please specify TRUE or FALSE for the argument 'adjust'.", call. = FALSE) }

    # Check input 'jitter'
    if (isTRUE(!is.logical(jitter))) { stop("Please specify TRUE or FALSE for the argument 'jitter'.", call. = FALSE) }

    # Check input 'line'
    if (isTRUE(!is.logical(line))) { stop("Please specify TRUE or FALSE for the argument 'line'.", call. = FALSE) }

    # Check input 'digits'
    if (isTRUE(digits %% 1L != 0L || digits < 0L)) { stop("Please specify a positive integer number for the argument 'digits'.", call. = FALSE) }

    # Check input 'p.digits'
    if (isTRUE(p.digits %% 1L != 0L || p.digits < 0L)) { stop("Please specify a positive integer number for the argument 'p.digits'.", call. = FALSE) }

    # Check input 'output'
    if (isTRUE(!is.logical(output))) { stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Data and Arguments ---------------------------------------------------------

  # Global variables
  id <- low <- m <- upp <- variable <- NULL

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data frame ####

  data <- data.frame(data[, var.formula], stringsAsFactors = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Attach ID variable ####

  data.id <- data.frame(data, id = factor(1L:nrow(data)))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) {

    # Replace user-specified values with missing values
    data.id[, var.formula] <- misty::as.na(data.id[, var.formula], na = as.na, check = check)

    # Dependent variable with missing values only
    if (isTRUE(any(sapply(data.id[, var.formula], function(y) all(is.na(y)))))) { stop("After converting user-missing values into NA, a dependent variables is completely missing.", call. = FALSE) }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Missing data ####

  if (isTRUE(any(is.na(data.id[, var.formula])))) {

    # Listwise deletion
    if (isTRUE(na.omit)) {

      # Listwise deletion
      data.id <- na.omit(data.id)

      warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ",
                     length(attributes(data.id)$na.action)), call. = FALSE)

      # Check if at least 2 cases are available
      if (isTRUE(nrow(data.id) < 2L)) { stop("After listwise deletion, there are not enough cases for conducting the analysis.", call. = FALSE) }

    } else {

      # Remove cases with NA on all variables
      data.id <- data.id[which(apply(data.id[, var.formula], 1L, function(y) !all(is.na(y)))), ]

      if (nrow(data.id) < 2L) { stop("After removing cases with NA on all variables, there are not enough cases for conducting the analysis.", call. = FALSE) }

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Reshape Data Frame ####

  # Wide data frame into long format
  data.l <- reshape(data.id, idvar = "id", varying = list(var.formula),
                    times = factor(var.formula), v.names = "y", direction = "long")

  row.names(data.l) <- NULL

  if (isTRUE(any(is.na(data.l)))) {

    # Listwise deletion
    if (isTRUE(!na.omit)) {

      # Listwise deletion
      data.l <- na.omit(data.l)

      # Check number of observations within each factor level
      check.y <- table(data.l$time)

      # Check if at least 2 cases are available
      if (isTRUE(any(check.y < 2L))) {

        if (isTRUE(length(which(check.y < 2L)) == 1L)) {

          stop(paste0("There are not enough observations in the variable '",  names(which(check.y < 2L)), "' to conduct the analysis."), call. = FALSE)

        } else {

          stop(paste0("There are not enough observations to conduct the analysis in the following variables: ", names(which(check.y < 2L))), call. = FALSE)

        }

      }

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Adjustment method for multiple testing ####

  p.adj <- ifelse(all(c("none", "bonferroni", "holm", "hochberg", "hommel", "BH", "BY", "fdr") %in% p.adj), "holm", p.adj)

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Descriptive statistics ####

  ci <- misty::ci.mean(data.id[, var.formula], conf.level = conf.level, output = FALSE)$result[, -4L]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Fit ANOVA model ####

  aov.res <- aov(y ~ time + Error(id), data = data.l)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Box index of sphericity ####
  #
  # https://crumplab.com/rstatsforpsych/repeated-measures-anova.html

  # Number of within-subject factor levels
  k <- length(var.formula)

  # Number of subjects
  n <- nrow(data.id)

  cov.matrix <- cov(data.id[, var.formula], use = "pairwise.complete.obs")
  dc.matrix <- cov.matrix - (cov.matrix*0L + colMeans(cov.matrix)) - (t(cov.matrix*0L + rowMeans(cov.matrix))) + mean(cov.matrix)

  #...................
  ### Lower bound ####

  lb <- 1L  / (k - 1L)

  #...................
  ### Greenhouse-Geisser ####

  gg <- sum(diag(dc.matrix))^2L / ((nrow(dc.matrix) - 1L) * sum(dc.matrix^2L))

  #...................
  ### Huynh-Feldt ####

  hf <- (n*(k - 1L)*gg - 2L) / ((k - 1L) * (n - 1L - (k - 1L)*gg))

  #...................
  ### Average of Greenhouse-Geisser and Huynh-Feldt ####

  gg.hf <- mean(c(gg, hf), na.rm = TRUE)

  #...................
  ### Epsilon table ####

  epsilon.table <- data.frame(index = c("lower bound", "greenhouse and geisser", "huynh and feldt", "average of gg and hf"),
                              epsilon = c(lb, gg, hf, gg.hf))

  #...................
  ### print argument ####
  #
  # Baguley (2012), p. 633
  # - if gg.hf < 0.75 -> Greenhouse-Geisser
  # - if gg.hf >= 0.75 & gg.hf < 0.95 -> Huynh-Feldt
  # - if gg.hf >= 0.95 -> no correction

  if (isTRUE(all(c("all", "none", "LB",  "GG", "HF") %in% print))) { print <- ifelse(gg.hf < 0.75, "GG", ifelse(gg.hf >= 0.75 && gg.hf < 0.95, "HF", "none")) }

  if (isTRUE(length(print) == 1 && print == "all")) { print <- c("none", "LB", "GG", "HF") }

  if (isTRUE(any(is.na(data.id[, var.formula])) && any(print %in% c("GG", "HF")))) {

    warning("Greenhouse-Geisser and Huynh-Feldt correction might not be reliable due to the presence of missing data.", call. = FALSE)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Extract sum of squares ####

  # ANOVA table subject
  aov.table.id <- summary(aov.res)[["Error: id"]]
  # ANOVA table effect
  aov.table.within <- summary(aov.res)[["Error: Within"]]

  # Sum of squares subjects
  ss.id <- aov.table.id[[1L]][["Sum Sq"]][1L]
  # Mean sum of squares subjects
  ms.id <- aov.table.id[[1L]][["Mean Sq"]][1L]
  # Degrees of freedom subjects
  df.id <- aov.table.id[[1L]][["Df"]][1L]

  # Sum of squares effect
  ss.e.within <- aov.table.within[[1L]]["time", "Sum Sq"]
  # Mean sum of squares effect
  ms.e.within <- aov.table.within[[1L]]["time", "Mean Sq"]
  # Degrees of freedom effect
  df.e.within <- aov.table.within[[1L]]["time", "Df"]

  # Sum of squares residuals
  ss.r.within <- aov.table.within[[1L]]["Residuals", "Sum Sq"]
  # Mean sum of squares residuals
  ms.r.within <- aov.table.within[[1L]]["Residuals", "Mean Sq"]
  # Degrees of freedom residuals
  df.r.within <- aov.table.within[[1L]]["Residuals", "Df"]

  # Total sum of squares
  ss.t <- sum(c(aov.table.within[[1L]][, "Sum Sq"], ss.id))
  # Total mean sum of squares
  ms.t <- sum(c(aov.table.within[[1L]][, "Mean Sq"], ms.id))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Effect Size Measures ####
  #
  # Olejnik and Algina (2000), Table 17

  #...................
  ### Eta squared ####

  eta.sq <- ss.e.within / ss.t

  #...................
  ### Partial Eta squared ####

  eta.sq.p <- ss.e.within / sum(aov.table.within[[1L]][, "Sum Sq"])

  #...................
  ### Omega squared ####

  omega.sq <- (df.e.within*(ms.e.within - ms.r.within)) / (ss.t + ms.id)
  omega.sq <- ifelse(omega.sq < 0L, 0L, omega.sq)

  #...................
  ### Partial Omega squared ####

  omega.sq.p <- (df.e.within*(ms.e.within - ms.r.within)) / (df.e.within*ms.e.within + (nrow(data.l) - df.e.within)*ms.r.within)
  omega.sq.p <- ifelse(omega.sq.p < 0L, 0L, omega.sq.p)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## ANOVA table ####

  #...................
  ### Sphericity Correction: None  ####

  test.none <- data.frame(source = c("Within-Subjects", "Factor", "Residuals", "Between-Subjects", "Total"),
                          sum.sq = c(NA, ss.e.within, ss.r.within, ss.id, ss.t),
                          df = c(NA, df.e.within, df.r.within, df.id, sum(c(df.e.within, df.r.within, df.id))),
                          mean.sq = c(NA, ms.e.within, ms.r.within, ms.id, ms.t),
                          F = c(NA, aov.table.within[[1L]]["time", "F value"], NA, NA, NA),
                          pval = c(NA, aov.table.within[[1L]]["time", "Pr(>F)"], NA, NA, NA),
                          eta.sq = c(NA, eta.sq, NA, NA, NA),
                          eta.sq.p = c(NA, eta.sq.p, NA, NA, NA),
                          omega.sq = c(NA, omega.sq, NA, NA, NA),
                          omega.sq.p = c(NA, omega.sq.p, NA, NA, NA))

  test.lb <- test.gg <- test.hf <- test.none

  #...................
  ### Sphericity Correction: Lower Bound ####

  # Correct degrees of freedom
  test.lb[test.lb$source %in% c("Factor", "Residuals"), "df"] <- test.lb[test.lb$source %in% c("Factor", "Residuals"), "df"] * lb

  # Correct mean sum of squares
  test.lb[test.lb$source %in% c("Factor", "Residuals"), "mean.sq"] <- test.lb[test.lb$source %in% c("Factor", "Residuals"), "sum.sq"] / test.lb[test.lb$source %in% c("Factor", "Residuals"), "df"]

  # Correct p value
  test.lb[test.lb$source == "Factor", "pval"] <- pf(test.lb[test.lb$source == "Factor", "F"], df1 = test.lb[test.lb$source == "Factor", "df"], df2 = test.lb[test.lb$source == "Residuals", "df"], lower.tail = FALSE)

  #...................
  ### Sphericity Correction: Greenhouse-Geisser ####

  # Correct degrees of freedom
  test.gg[test.gg$source %in% c("Factor", "Residuals"), "df"] <- test.gg[test.gg$source %in% c("Factor", "Residuals"), "df"] * gg

  # Correct mean sum of squares
  test.gg[test.gg$source %in% c("Factor", "Residuals"), "mean.sq"] <- test.gg[test.gg$source %in% c("Factor", "Residuals"), "sum.sq"] / test.gg[test.gg$source %in% c("Factor", "Residuals"), "df"]

  # Correct p value
  test.gg[test.gg$source == "Factor", "pval"] <- pf(test.gg[test.gg$source == "Factor", "F"], df1 = test.gg[test.gg$source == "Factor", "df"], df2 = test.gg[test.gg$source == "Residuals", "df"], lower.tail = FALSE)

  #...................
  ### Sphericity Correction: Huynh-Feldt ####

  # Correct degrees of freedom
  test.hf[test.hf$source %in% c("Factor", "Residuals"), "df"] <- test.hf[test.hf$source %in% c("Factor", "Residuals"), "df"] * ifelse(hf > 1L, 1L, hf)

  # Correct mean sum of squares
  test.hf[test.hf$source %in% c("Factor", "Residuals"), "mean.sq"] <- test.hf[test.hf$source %in% c("Factor", "Residuals"), "sum.sq"] / test.hf[test.hf$source %in% c("Factor", "Residuals"), "df"]

  # Correct p value
  test.hf[test.hf$source == "Factor", "pval"] <- pf(test.hf[test.hf$source == "Factor", "F"], df1 = test.hf[test.hf$source == "Factor", "df"], df2 = test.hf[test.hf$source == "Residuals", "df"], lower.tail = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Post-hoc test ####

  var.combn <- combn(var.formula, m = 2L)

  result.ph <- apply(var.combn, 2L, function(y) misty::test.t(data.id[, y[1L]], data.id[, y[2L]], paired = TRUE, conf.level = conf.level, output = FALSE)$result[c("m.diff", "t", "df", "pval", "d", "d.low", "d.upp")])

  result.ph <- data.frame(var1 = t(var.combn)[, 1L], var2 = t(var.combn)[, 2L],
                          eval(parse(text = paste0("rbind(", paste0("result.ph[[", seq_len(length(result.ph)), "]]", collapse = ", "), ")"))), stringsAsFactors = FALSE)

  # Adjust p-values for multiple comparisons
  if (isTRUE(p.adj != "none")) {

    result.ph$pval <- p.adjust(result.ph$pval, method = p.adj)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Result object ####

  result <- list(descript = ci,
                 epsilon = epsilon.table,
                 test = list(none = test.none, lb = test.lb, gg = test.gg, hf = test.hf),
                 posthoc = result.ph,
                 aov = aov.res)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Plot ####

  #...................
  ### Plot data ####

  plotdat <- data.l

  # Confidence intervals
  plotdat.ci <- misty::ci.mean.w(data.id[, var.formula], adjust = adjust,
                                 conf.level = conf.level, na.omit = na.omit, check = FALSE, output = FALSE)$result

  #...................
  ### Create ggplot ####
  p <- ggplot2::ggplot(plotdat, ggplot2::aes(time, y, group = 1L))

  # Subtitle
  if (isTRUE(subtitle == "Confidence Interval")) { subtitle <- paste0("Two-Sided ", round(conf.level * 100L, digits = 2L), "% Confidence Interval") }

  #...................
  ### Add jittered points and individual lines ####
  if (isTRUE(jitter)) {

    if (isTRUE(line)) {

      p <- p + ggplot2::geom_point(data = plotdat, ggplot2::aes(time, y, group = id),
                                   alpha = jitter.alpha, position = ggplot2::position_dodge(jitter.width)) +
        ggplot2::geom_line(data = plotdat, ggplot2::aes(time, y, group = id),
                           alpha = jitter.alpha, position = ggplot2::position_dodge(jitter.width))

    } else {

      p <- p + ggplot2::geom_jitter(alpha = jitter.alpha, width = jitter.width, height = jitter.height, size = jitter.size)

    }

  }

  #...................
  ### Within-subject confidence intervals ####
  p <- p + ggplot2::geom_point(data = plotdat.ci, ggplot2::aes(variable, m), stat = "identity", size = point.size) +
    ggplot2::stat_summary(data = plotdat, ggplot2::aes(time, y),
                          fun = mean, geom = "line") +
    ggplot2::geom_errorbar(data = plotdat.ci, ggplot2::aes(variable, m, ymin = low, ymax = upp), width = error.width) +
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
                 type = "aov.w",
                 data = list(wide = data, long = data.l, ci = plotdat.ci),
                 formula = formula,
                 plot = p,
                 args = list(print = print, posthoc = posthoc, conf.level = conf.level,
                             p.adjust = p.adj, hypo = hypo, descript = descript,
                             epsilon = epsilon, effsize = effsize, na.omit = na.omit,
                             plot = plot, point.size = point.size, adjust = adjust,
                             error.width = error.width, xlab = xlab, ylab = ylab,
                             ylim = ylim, breaks = breaks, jitter = jitter, line = line,
                             jitter.size = jitter.size, jitter.width = jitter.width,
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
