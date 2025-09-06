#' Repeated Measures Analysis of Variance (Within-Subject ANOVA)
#'
#' This function performs an one-way repeated measures analysis of variance (within
#' subject ANOVA) including paired-samples t-tests for multiple comparison and
#' provides descriptive statistics, effect size measures, and a plot showing error
#' bars for difference-adjusted Cousineau-Morey within-subject confidence intervals
#' with jittered data points including subject-specific lines.
#'
#' @param formula        a formula of the form \code{cbind(time1, time2, time3) ~ 1}
#'                       where \code{time1}, \code{time2}, and \code{time3} are
#'                       numeric variables representing the levels of the within-
#'                       subject factor, i.e., data are specified in wide-format
#'                       (i.e., multivariate person level format).
#' @param data           a matrix or data frame containing the variables in the
#'                       formula \code{formula}.
#' @param print          a character vector indicating which sphericity correction
#'                       to use, i.e., \code{all} for all corrections, \code{none}
#'                       for no correction, \code{LB} for lower bound correction,
#'                       \code{GG} for Greenhouse-Geisser correction, and \code{HF},
#'                       for Huynh-Feldt correction.
#' @param posthoc        logical: if \code{TRUE}, paired-samples t-tests for
#'                       multiple comparison are conducted.
#' @param conf.level     a numeric value between 0 and 1 indicating the confidence
#'                       level of the interval.
#' @param p.adj          a character string indicating an adjustment method for
#'                       multiple testing based on \code{\link{p.adjust}}, i.e.,
#'                       \code{none}, \code{bonferroni}, \code{holm} (default),
#'                       \code{h ochberg}, \code{hommel}, \code{BH}, \code{BY}, or
#'                       \code{fdr}.
#' @param hypo           logical: if \code{TRUE} (default), null and alternative
#'                       hypothesis are shown on the console.
#' @param descript       logical: if \code{TRUE} (default), descriptive statistics
#'                       are shown on the console.
#' @param epsilon        logical: if \code{TRUE} (default), box indices of sphericity
#'                       (epsilon) are shown on the console, i.e., lower bound,
#'                       Greenhouse and Geiser (GG), Huynh and Feldt (HF) and average
#'                       of GG and HF.
#' @param effsize        logical: if \code{TRUE}, effect size measures eta-squared
#'                       (\eqn{\eta^2}), partial eta-squared (\eqn{\eta^2_p}),
#'                       omega-squared (\eqn{\omega^2}), and partial omega-squared
#'                       (\eqn{\omega^2_p}) for the repeated measures ANOVA and
#'                       Cohen's \emph{d} for the post hoc tests are shown on
#'                       the console.
#' @param na.omit        logical: if \code{TRUE} (default), incomplete cases are
#'                       removed before conducting the analysis (i.e., listwise
#'                       deletion).
#' @param digits         an integer value indicating the number of decimal places
#'                       to be used for displaying descriptive statistics and
#'                       confidence interval.
#' @param p.digits       an integer value indicating the number of decimal places
#'                       to be used for displaying the \emph{p}-value.
#' @param as.na          a numeric vector indicating user-defined missing values,
#'                       i.e. these values are converted to \code{NA} before
#'                       conducting the analysis.
#' @param plot           logical: if \code{TRUE}, a plot showing error bars for
#'                       confidence intervals is drawn.
#' @param point          logical: if \code{TRUE} (default), points representing
#'                       means for each groups are drawn.
#' @param line           logical: if \code{TRUE} (default), a line connecting means
#'                       of each groups and lines connecting data points are drawn
#'                       when \code{jitter = TRUE}.
#' @param ci             logical: if \code{TRUE} (default), error bars representing
#'                       confidence intervals are drawn.
#' @param jitter         logical: if \code{TRUE}, jittered data points with
#'                       subject-specific lines are drawn.
#' @param adjust         logical: if \code{TRUE} (default), difference-adjustment
#'                       for the Cousineau-Morey within-subject confidence
#'                       intervals is applied.
#' @param point.size     a numeric value indicating the \code{size} aesthetic for
#'                       the point representing the mean value.
#' @param line.width     a numeric value indicating the \code{linewidth} aesthetic
#'                       for the line connecting means of each groups.
#' @param errorbar.width a numeric value indicating the horizontal bar width of
#'                       the error bar.
#' @param jitter.size    a numeric value indicating the \code{size} aesthetic
#'                       for the jittered data points.
#' @param jitter.width   a numeric value indicating the amount of horizontal jitter.
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
#' \item{\strong{Sphericity}}{The \emph{F}-Test of the repeated measures ANOVA
#' is based on the assumption of sphericity, which is defined as the assumption
#' that the variance of differences between repeated measures are equal in the
#' population. The Mauchly's test is commonly used to test this hypothesis.
#' However, test of assumptions addresses an irrelevant hypothesis because what
#' matters is the degree of violation rather than its presence (Baguley, 2012a).
#' Moreover, the test is not recommended because it lacks statistical power (Abdi,
#' 2010). Instead, the Box index of sphericity (\eqn{\varepsilon}) should be used to
#' assess the degree of violation of the sphericity assumption. The \eqn{\varepsilon}
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
#' entries:
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{data}}{list with the data (\code{data}) in wide-format (\code{wide}),
#'                    reshaped data in long-format (\code{long}), and within-subject
#'                    confidence intervals (\code{ci})}
#' \item{\code{formula}}{formula of the current analysis}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{plot}}{ggplot2 object for plotting the results}
#' \item{\code{result}}{list with result tables}
#'
#' @export
#'
#' @examples
#' dat <- data.frame(time1 = c(3, 2, 1, 4, 5, 2, 3, 5, 6, 7),
#'                   time2 = c(4, 3, 6, 5, 8, 6, 7, 3, 4, 5),
#'                   time3 = c(1, 2, 2, 3, 6, 5, 1, 2, 4, 6))
#'
#' # Example 1: Repeated measures ANOVA
#' aov.w(cbind(time1, time2, time3) ~ 1, data = dat)
#'
#' # Example 2: Repeated measures ANOVA, print results of all sphericity corrections
#' aov.w(cbind(time1, time2, time3) ~ 1, data = dat, print = "all")
#'
#' # Example 3: Repeated measures ANOVA, print effect size measures
#' aov.w(cbind(time1, time2, time3) ~ 1, data = dat, effsize = TRUE)
#'
#' # Example 4: Repeated measures ANOVA, do not print hypotheses and descriptive statistics,
#' aov.w(cbind(time1, time2, time3) ~ 1, data = dat, descript = FALSE, hypo = FALSE)
#'
#' # Example 5: Repeated measures ANOVA, plot results
#' aov.w(cbind(time1, time2, time3) ~ 1, data = dat, plot = TRUE)
#'
#' # Example 6: Write Results into a text file
#' aov.w(cbind(time1, time2, time3) ~ 1, data = dat, write = "RM-ANOVA.txt")
#'
#' # Example 7: Save plot
#' aov.w(cbind(time1, time2, time3) ~ 1, data = dat, plot = TRUE,
#'       filename = "Repeated_measures_ANOVA.png", width = 7, height = 6)
aov.w <- function(formula, data, print = c("all", "none", "LB", "GG", "HF"),
                  posthoc = FALSE, conf.level = 0.95,
                  p.adj = c("none", "bonferroni", "holm", "hochberg", "hommel", "BH", "BY", "fdr"),
                  hypo = TRUE, descript = TRUE, epsilon = TRUE, effsize = FALSE, na.omit = TRUE,
                  digits = 2, p.digits = 3, as.na = NULL, plot = FALSE, point = TRUE, line = TRUE,
                  ci = TRUE, jitter = FALSE, adjust = TRUE, point.size = 3, line.width = 0.5,
                  errorbar.width = 0.1, jitter.size = 1.25,jitter.width = 0.05, jitter.alpha = 0.1,
                  xlab = NULL, ylab = "y", ylim = NULL, ybreaks = ggplot2::waiver(), title = NULL,
                  subtitle = "Confidence Interval", filename = NULL, width = NA, height = NA,
                  units = c("in", "cm", "mm", "px"), dpi = 600, write = NULL, append = TRUE,
                  check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'formula' is missing
  if (isTRUE(missing(formula))) { stop("Please specify a formula using the argument 'formula'", call. = FALSE) }

  # Check if input 'data' is missing or NULL
  if (isTRUE(missing(data) || is.null(data))) { stop("Please specify a matrix or data frame for the argument 'data'", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Formula --------------------------------------------------------------------

  # Variables
  var.formula <- all.vars(as.formula(formula))

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs and R package
  .check.input(logical = c("posthoc", "hypo", "descript", "effsize", "na.omit", "plot", "point", "line", "ci", "jitter", "adjust", "append", "output"),
               numeric = list(point.size = 1L, errorbar.width = 1L, jitter.size = 1L, jitter.alpha = 1L, jitter.width = 1L),
               character = list(xlab = 1L, ylab = 1L, title = 1L, subtitle = 1L), m.character = list(print = c("all", "none", "LB",  "GG", "HF")), args = c("digts", "p.digits", "conf.level", "p.adj", "write1"), envir = environment(), input.check = check)

  # Additional checks
  if (isTRUE(check)) {

    # Package ggplot2
    if (isTRUE(check && plot != "none")) { if (isTRUE(!nzchar(system.file(package = "ggplot2")))) { stop("Package \"ggplot2\" is needed to draw a plot, please install the package.", call. = FALSE) } }

    # Check if variables are in the data
    (!var.formula %in% colnames(data)) |> (\(y) if (isTRUE(any(y))) { stop(paste0(ifelse(sum(y) == 1L, "Variable ", "Variables "), "specified in the formula ", ifelse(sum(y) == 1L, "was ", "were "), "not found in 'data': ", paste(var.formula[y], collapse = ", ")), call. = FALSE) })()

  }

  #_____________________________________________________________________________
  #
  # Data and Arguments ---------------------------------------------------------

  # Global variables
  id <- low <- m <- upp <- variable <- NULL

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data frame ####

  data <- data.frame(data[, var.formula])

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Attach ID variable ####

  data.id <- data.frame(data, id = factor(1L:nrow(data)))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) { .as.na(data.id[, var.formula], na = as.na) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Missing data ####

  if (isTRUE(any(is.na(data.id[, var.formula])))) {

    # Listwise deletion
    if (isTRUE(na.omit)) {

      # Listwise deletion
      data.id <- na.omit(data.id)

      warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ", length(attributes(data.id)$na.action)), call. = FALSE)

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
  data.l <- reshape(data.id, idvar = "id", varying = list(var.formula), times = factor(var.formula), v.names = "y", direction = "long")

  row.names(data.l) <- NULL

  if (isTRUE(any(is.na(data.l)))) {

    # Listwise deletion
    if (isTRUE(!na.omit)) {

      # Listwise deletion
      data.l <- na.omit(data.l)

      # Check number of observations within each factor level
      table(data.l$time) |>
        (\(y) # Check if at least 2 cases are available
         if (isTRUE(any(y < 2L))) {

           if (isTRUE(length(which(y < 2L)) == 1L)) {

             stop(paste0("There are not enough observations in the variable '",  names(which(y < 2L)), "' to conduct the analysis."), call. = FALSE)

           } else {

             stop(paste0("There are not enough observations to conduct the analysis in the following variables: ", names(which(y < 2L))), call. = FALSE)

           }

         })()

    }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Adjustment method for multiple testing ####

  p.adj <- ifelse(all(c("none", "bonferroni", "holm", "hochberg", "hommel", "BH", "BY", "fdr") %in% p.adj), "holm", p.adj)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'units' Argument ####

  # Default setting
  if (isTRUE(all(c("in", "cm", "mm", "px") %in% units))) { units <- "in" }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Descriptive statistics ####

  ci.table <- misty::ci.mean(data.id[, var.formula], conf.level = conf.level, output = FALSE)$result[, c("variable", "n", "nNA", "m", "low", "upp", "sd", "skew", "kurt")]

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

  dc.matrix <- cov(data.id[, var.formula], use = "pairwise.complete.obs") |> (\(y) y - (y*0L + colMeans(y)) - (t(y*0L + rowMeans(y))) + mean(y))()

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

  epsilon.table <- data.frame(index = c("lower bound", "greenhouse and geisser", "huynh and feldt", "average of gg and hf"), epsilon = c(lb, gg, hf, gg.hf))

  #...................
  ### print argument ####
  #
  # Baguley (2012), p. 633
  # - if gg.hf < 0.75 -> Greenhouse-Geisser
  # - if gg.hf >= 0.75 & gg.hf < 0.95 -> Huynh-Feldt
  # - if gg.hf >= 0.95 -> no correction

  if (isTRUE(all(c("all", "none", "LB",  "GG", "HF") %in% print))) { print <- ifelse(gg.hf < 0.75, "GG", ifelse(gg.hf >= 0.75 && gg.hf < 0.95, "HF", "none")) }

  if (isTRUE(length(print) == 1L && print == "all")) { print <- c("none", "LB", "GG", "HF") }

  if (isTRUE(any(is.na(data.id[, var.formula])) && any(print %in% c("GG", "HF")))) { warning("Greenhouse-Geisser and Huynh-Feldt correction might not be reliable due to the presence of missing data.", call. = FALSE) }

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

  omega.sq <- (df.e.within*(ms.e.within - ms.r.within)) / (ss.t + ms.id) |>
    (\(y) ifelse(y < 0L, 0L, y))()

  #...................
  ### Partial Omega squared ####

  omega.sq.p <- (df.e.within*(ms.e.within - ms.r.within)) / (df.e.within*ms.e.within + (nrow(data.l) - df.e.within)*ms.r.within) |>
    (\(y) ifelse(y < 0L, 0L, y))()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## ANOVA table ####

  #...................
  ### Sphericity Correction: None  ####

  test.none <- data.frame(source = c("Within-Subjects", "Factor", "Residuals", "Between-Subjects", "Total"),
                          sum.sq = c(NA, ss.e.within, ss.r.within, ss.id, ss.t),
                          df = c(NA, df.e.within, df.r.within, df.id, sum(c(df.e.within, df.r.within, df.id))),
                          mean.sq = c(NA, ms.e.within, ms.r.within, ms.id, ms.t),
                          F = c(NA, aov.table.within[[1L]]["time", "F value"], NA, NA, NA),
                          p = c(NA, aov.table.within[[1L]]["time", "Pr(>F)"], NA, NA, NA),
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
  test.lb[test.lb$source == "Factor", "p"] <- pf(test.lb[test.lb$source == "Factor", "F"], df1 = test.lb[test.lb$source == "Factor", "df"], df2 = test.lb[test.lb$source == "Residuals", "df"], lower.tail = FALSE)

  #...................
  ### Sphericity Correction: Greenhouse-Geisser ####

  # Correct degrees of freedom
  test.gg[test.gg$source %in% c("Factor", "Residuals"), "df"] <- test.gg[test.gg$source %in% c("Factor", "Residuals"), "df"] * gg

  # Correct mean sum of squares
  test.gg[test.gg$source %in% c("Factor", "Residuals"), "mean.sq"] <- test.gg[test.gg$source %in% c("Factor", "Residuals"), "sum.sq"] / test.gg[test.gg$source %in% c("Factor", "Residuals"), "df"]

  # Correct p value
  test.gg[test.gg$source == "Factor", "p"] <- pf(test.gg[test.gg$source == "Factor", "F"], df1 = test.gg[test.gg$source == "Factor", "df"], df2 = test.gg[test.gg$source == "Residuals", "df"], lower.tail = FALSE)

  #...................
  ### Sphericity Correction: Huynh-Feldt ####

  # Correct degrees of freedom
  test.hf[test.hf$source %in% c("Factor", "Residuals"), "df"] <- test.hf[test.hf$source %in% c("Factor", "Residuals"), "df"] * ifelse(hf > 1L, 1L, hf)

  # Correct mean sum of squares
  test.hf[test.hf$source %in% c("Factor", "Residuals"), "mean.sq"] <- test.hf[test.hf$source %in% c("Factor", "Residuals"), "sum.sq"] / test.hf[test.hf$source %in% c("Factor", "Residuals"), "df"]

  # Correct p value
  test.hf[test.hf$source == "Factor", "p"] <- pf(test.hf[test.hf$source == "Factor", "F"], df1 = test.hf[test.hf$source == "Factor", "df"], df2 = test.hf[test.hf$source == "Residuals", "df"], lower.tail = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Post-hoc test ####

  result.ph <- combn(var.formula, m = 2L) |>
    (\(y) apply(y, 2L, function(y) misty::test.t(data.id[, y[1L]], data.id[, y[2L]], paired = TRUE, conf.level = conf.level, output = FALSE)$result[c("m.diff", "t", "df", "p", "d", "d.low", "d.upp")]) |>
       (\(z) data.frame(var1 = t(y)[, 1L], var2 = t(y)[, 2L], eval(parse(text = paste0("rbind(", paste0("z[[", seq_len(length(z)), "]]", collapse = ", "), ")"))), stringsAsFactors = FALSE))())()

  # Adjust p-values for multiple comparisons
  if (isTRUE(p.adj != "none")) { result.ph$p <- p.adjust(result.ph$p, method = p.adj) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Result object ####

  result <- list(descript = ci.table,
                 epsilon = epsilon.table,
                 test = list(none = test.none, lb = test.lb, gg = test.gg, hf = test.hf),
                 posthoc = result.ph,
                 aov = aov.res)

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "aov.w",
                 data = list(wide = data, long = data.l),
                 formula = formula,
                 args = list(print = print, posthoc = posthoc, conf.level = conf.level,
                             p.adjust = p.adj, hypo = hypo, descript = descript,
                             epsilon = epsilon, effsize = effsize, na.omit = na.omit,
                             digits = digits, p.digits = p.digits, as.na = as.na,
                             plot = plot, point = point, line = line, ci = ci,
                             jitter = jitter, adjust = adjust, point.size = point.size,
                             line.width = line.width, errorbar.width = errorbar.width,
                             jitter.size = jitter.size, jitter.width = jitter.width, jitter.alpha = jitter.alpha,
                             xlab = xlab, ylab = ylab, ylim = ylim, ybreaks = ybreaks, title = title,
                             subtitle = subtitle, filename = filename, width = width, height = height,
                             units = units, dpi = dpi, check = check, write = write, append = append, output = output),
                 plot = NULL, result = result)

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Plot and Save Results ------------------------------------------------------

  if (isTRUE(plot)) { object$plot <- plot(object, filename = filename, width = width, height = height, units = units, dpi = dpi, check = FALSE) |> (\(y) suppressMessages(suppressWarnings(print(y))))() }

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

#_______________________________________________________________________________
