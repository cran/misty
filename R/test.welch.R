#' Welch's Test
#'
#' This function performs Welch's two-sample t-test and Welch's ANOVA including
#' Games-Howell post hoc test for multiple comparison and provides descriptive
#' statistics, effect size measures, and a plot showing bars representing means
#' for each group and error bars for difference-adjusted confidence intervals.
#'
#' @param formula     a formula of the form \code{y ~ group} where \code{y} is
#'                    a numeric variable giving the data values and \code{group}
#'                    a numeric variable, character variable or factor with two
#'                    or more than two values or factor levels giving the
#'                    corresponding groups.
#' @param data        a matrix or data frame containing the variables in the
#'                    formula \code{formula}.
#' @param alternative a character string specifying the alternative hypothesis,
#'                    must be one of \code{"two.sided"} (default), \code{"greater"}
#'                    or \code{"less"}. Note that this argument is only used when
#'                    conducting Welch's two-sample t-test.
#' @param hypo        logical: if \code{TRUE} (default), null and alternative
#'                    hypothesis are shown on the console.
#' @param descript    logical: if \code{TRUE}, descriptive statistics are shown
#'                    on the console when conducting Welch's ANOVA.
#' @param effsize     logical: if \code{TRUE}, effect size measure Cohen's d for
#'                    Welch's two-sample t-test (see \code{\link{cohens.d}}),
#'                    \eqn{\eta^2} and \eqn{\omega^2} for Welch's ANOVA and
#'                    Cohen's d for the post hoc tests are shown on the console.
#' @param weighted    logical: if \code{TRUE}, the weighted pooled standard
#'                    deviation is used to compute Cohen's d.
#' @param correct     logical: if \code{TRUE}, correction factor to remove positive
#'                    bias in small samples for is used to compute Cohen's d.
#' @param posthoc     logical: if \code{TRUE}, Games-Howell post hoc test for
#'                    multiple comparison is conducted when performing Welch's
#'                    ANOVA.
#' @param conf.level  a numeric value between 0 and 1 indicating the confidence
#'                    level of the interval.
#' @param p.adj       a character string indicating an adjustment method for multiple
#'                    testing based on \code{\link{p.adjust}}, i.e., \code{none}
#'                    (default),\code{bonferroni}, \code{holm}, \code{hochberg},
#'                    \code{hommel}, \code{BH}, \code{BY}, or \code{fdr}.
#'                    See help page of the \code{\link{p.adjust}} function.
#' @param na.omit     logical: if \code{TRUE}, incomplete cases are removed
#'                    before conducting the analysis (i.e., listwise deletion)
#'                    when specifying more than one outcome variable.
#' @param digits      an integer value indicating the number of decimal places
#'                    to be used for displaying results.
#' @param p.digits    an integer value indicating the number of decimal places
#'                    to be used for displaying the \emph{p}-value.
#' @param as.na       a numeric vector indicating user-defined missing values,
#'                    i.e. these values are converted to \code{NA} before
#'                    conducting the analysis.
#' @param plot        logical: if \code{TRUE}, a plot showing bar plots with
#'                    error bars for confidence intervals is drawn. For
#'                    additional plotting arguments, see \code{Details} in
#'                    the help page of the function \code{plot.misty.object}.
#' @param geom        a character string or character vector indicating which
#'                    plot elements to draw, i.e., \code{"all"} for drawing
#'                    all plot elements, \code{"bar"} for drawing bars representing
#'                    means for each groups, \code{"point"} for drawing points
#'                    representing means for each group, \code{"ci"} for drawing
#'                    error bars representing confidence intervals, and \code{"jitter"}
#'                    for drawing jittered data points.
#' @param adjust      logical: if \code{TRUE} (default), difference-adjustment
#'                    for the confidence intervals is applied.
#' @param filename    a character string indicating the \code{filename}
#'                    argument including the file extension in the \code{ggsave}
#'                    function. Note that one of \code{".eps"}, \code{".ps"},
#'                    \code{".tex"}, \code{".pdf"} (default),
#'                    \code{".jpeg"}, \code{".tiff"}, \code{".png"},
#'                    \code{".bmp"}, \code{".svg"} or \code{".wmf"} needs
#'                    to be specified as file extension in the \code{filename}
#'                    argument. Note that plots can only be saved when
#'                    \code{plot = TRUE}.
#' @param width       a numeric value indicating the \code{width} argument
#'                    (default is the size of the current graphics device)
#'                    in the \code{ggsave} function.
#' @param height      a numeric value indicating the \code{height} argument
#'                    (default is the size of the current graphics device)
#'                    in the \code{ggsave} function.
#' @param dpi         a numeric value indicating the \code{dpi} argument
#'                    (default: \code{600}) in the \code{ggsave} function.
#' @param write       a character string naming a text file with file extension
#'                    \code{".txt"} (e.g., \code{"Output.txt"}) for writing the
#'                    output into a text file.
#' @param append      logical: if \code{TRUE} (default), output will be appended
#'                    to an existing text file with extension \code{.txt} specified
#'                    in \code{write}, if \code{FALSE} existing text file will be
#'                    overwritten.
#' @param check       logical: if \code{TRUE} (default), argument specification
#'                    is checked.
#' @param output      logical: if \code{TRUE} (default), output is shown on the
#'                    console.
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
#' \item{\code{data}}{data frame with the outcome and grouping variable}
#' \item{\code{formula}}{formula}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{plot}}{ggplot2 object for plotting the results}
#' \item{\code{result}}{result table}
#'
#' @export
#'
#' @examples
#' #————————————————————————————————————————————————————————————————————————————
#' # Two-Sample Design
#'
#' # Example 1a: Two-sided two-sample Welch-test
#' test.welch(hp ~ am, data = mtcars)
#'
#' # Example 1b: One-sided two-sample Welch-test
#' test.welch(hp ~ am, data = mtcars, alternative = "less")
#'
#' # Example 1c: Two-sided two-sample Welch-test,
#' # Print descriptive statistics and Cohen's d
#' test.welch(hp ~ am, data = mtcars, descript = TRUE, effsize = TRUE)
#'
#' # Example 1d: Multiple two-sample Welch-test
#' test.welch(cbind(mpg, cyl, disp, hp, drat, wt) ~ vs, data = mtcars)
#'
#' # Example 1e: Multiple two-sample Welch-test, bonferroni correction
#' test.welch(cbind(mpg, cyl, disp, hp, drat, wt) ~ vs, data = mtcars, p.adj = "bonferroni")
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Multiple-Sample Design
#'
#' # Example 2a: Welch's ANOVA
#' test.welch(hp ~ gear, data = mtcars)
#'
#' # Example 2b: Welch's ANOVA,
#' # Print descriptive statistics and Games-Howell post hoc test
#' test.welch(hp ~ gear, data = mtcars, descript = TRUE, posthoc = TRUE)
#'
#' # Example 2c: Welch's ANOVA, print eta-squared and omega-squared
#' test.welch(hp ~ gear, data = mtcars, effsize = TRUE)
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Plot
#'
#' # Example 3a: Plot results, default setting
#' test.welch(hp ~ gear, data = mtcars, plot = TRUE)
#'
#' # Example 3b: Plot results
#' # No bars, draw points representing means and jittered data points
#' test.welch(hp ~ gear, data = mtcars, plot = TRUE, geom = c("point", "jitter"))
#'
#' # Example 3c: Plot results using the plot() function, use additional arguments
#' # see Details in the help page of the function plot.misty.object
#' object <- test.welch(hp ~ gear, data = mtcars)
#' plot(object, geom = c("bar", "ci", "jitter"), jitter.alpha = 0.4, title = "Welch's Test")
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Create Plot Manually
#'
#' # Load ggplot2 package
#' library(ggplot2)
#'
#' # Create misty object
#' object <- test.welch(hp ~ gear, data = mtcars)
#'
#' # Example 4: Plot
#' ggplot(object$result$descript, aes(group, y)) +
#'   geom_bar(aes(group, m), stat = "summary", fun = "mean") +
#'   geom_errorbar(aes(group, m, ymin = low, ymax = upp), width = 0.1) +
#'   theme_bw()
#'
#' \dontrun{
#' #————————————————————————————————————————————————————————————————————————————
#' # Write Results and Save Plot
#'
#' # Example 5a: Write results into a text file
#' test.welch(hp ~ gear, data = mtcars, write = "Welch-Test.txt")
#'
#' # Example 5b: Write results into an Excel file
#' test.welch(hp ~ gear, data = mtcars, write = "Welch-Test.xlsx")
#'
#' # Example 5c: Save plot as PNG fine
#' test.welch(hp ~ gear, data = mtcars, plot = TRUE,
#'            filename = "Welch-Test.png", width = 6, height = 5)
#' }
test.welch <- function(formula, data, alternative = c("two.sided", "less", "greater"),
                       hypo = FALSE, descript = c(FALSE, TRUE), effsize = FALSE, weighted = FALSE,
                       correct = FALSE, posthoc = FALSE, conf.level = 0.95, p.adj = "none",
                       na.omit = FALSE, digits = 2, p.digits = 3, as.na = NULL,
                       plot = FALSE, geom = c("all", "bar", "point", "ci", "jitter"),
                       adjust = TRUE, filename = NULL, width = NA, height = NA,
                       dpi = 600, write = NULL, append = TRUE, check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check ——————————————————————————————————————————————————————————————

  # Check if input 'formula' is missing
  if (isTRUE(missing(formula))) { stop("Please specify a formula using the argument 'formula'", call. = FALSE) }

  # Check if input 'data' is missing or NULL
  if (isTRUE(missing(data) ||is.null(data))) { stop("Please specify a matrix or data frame for the argument 'x'.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Variables ——————————————————————————————————————————————————————————————————

   # Extract variable names
  var.formula <- all.vars(as.formula(formula))

  # Grouping variable
  group.var <- attr(terms(formula[-2L]), "term.labels")

  # Outcome variable
  y.var <- setdiff(var.formula, group.var)

  #_____________________________________________________________________________
  #
  # Input Check ————————————————————————————————————————————————————————————————

  # Check inputs
  .check.input(logical = c("posthoc", "hypo", "effsize", "weighted", "correct", "plot", "adjust", "append", "output"),
               m.character = list(geom = c("all", "bar", "point", "ci", "jitter")), args = c("conf.level", "p.adjust", "digits", "p.digits", "write2"), envir = environment(), input.check = check)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Additional Checks ####

  if (isTRUE(check)) {

    # Check if variables are in the data
    (!var.formula %in% colnames(data)) |> (\(y) if (isTRUE(any(y))) { stop(paste0("Variables specified in the the formula were not found in 'data': ", paste(var.formula[which(y)], collapse = ", ")), call. = FALSE) })()

    # Check if input 'formula' has only one grouping variable
    if (isTRUE(length(group.var) != 1L)) { stop("Please specify a formula with only one grouping variable.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Data ———————————————————————————————————————————————————————————————————————

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data Frame ####

  x <- as.data.frame(data)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert User-Missing Values into NA ####

  if (isTRUE(!is.null(as.na))) { x[, y.var] <- .as.na(x[, y.var], na = as.na) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Listwise Deletion ####

  if (isTRUE(any(is.na(x[, var.formula])) && length(y.var) > 1L && na.omit)) {

    apply(x[, var.formula], 1L, function(y) all(!is.na(y))) |> (\(p) { x <<- x[p, ]

      warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ", sum(!p)), call. = FALSE)

    })()

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Missing Data in the Grouping Variable ####

  if (isTRUE(any(is.na(x[, group.var])))) {

    (!is.na(x[, group.var])) |> (\(p) { x <<- x[p, ]

    warning(paste0("Listwise deletion of missing values in the grouping variable, number of cases removed from the analysis: ", sum(!p)), call. = FALSE)

    })()

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Outcomes, Grouping Variable and Sample ####

  # Outcomes
  y <- unlist(data[, y.var])

  # Grouping variable
  group <- factor(unlist(x[, group.var]))

  # Sample
  sample <- ifelse(length(levels(group)) == 2L, "two", "multiple")

  # Check for one outcome variable for Welch's ANOVA
  if (isTRUE(sample == "multiple" && length(y.var) != 1L)) { stop("Please specify one outcome variable when performing Welch's ANOVA.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Arguments ——————————————————————————————————————————————————————————————————

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'alternative' Argument ####

  if (isTRUE(all(c("two.sided", "less", "greater") %in% alternative))) { alternative <- "two.sided" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'descript' Argument ####

  if (isTRUE(all(c(TRUE, FALSE) %in% descript))) { descript <- ifelse(sample == "two", TRUE, FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'geom' Argument ####

  if (isTRUE(all(c("all", "bar", "point", "ci", "jitter") %in% geom))) {

    geom <- c("bar", "ci")

  } else if (all(geom == "all")) {

    geom <- c("bar", "point", "ci", "jitter")

  }

  #_____________________________________________________________________________
  #
  # Main Function ——————————————————————————————————————————————————————————————

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Two-Sample Welch Test ####

  if (isTRUE(sample == "two")) {

    #—————————————————————————————————————— #
    ### One Outcome Variable ####

    if (isTRUE(length(y.var) == 1L)) {

      # Descriptive statistics
      ci.table <- misty::df.rename(misty::ci.mean.diff(formula = formula, data = x, paired = FALSE, adjust = adjust, alternative = alternative, conf.level = conf.level, check = FALSE, output = FALSE)$result, from = c("between", "low", "upp"), to = c("group", "m.low", "m.upp"))

      # Cohen's d
      d <- misty::cohens.d(formula = formula, data = x, weighted = weighted, correct = correct, alternative = alternative, conf.level = conf.level, check = FALSE, output = FALSE)$result

      # Welch's test for two groups
      welch <- t.test(formula = formula, data = x, alternative = switch(alternative, two.sided = "two.sided", greater = "less", less = "greater"), var.equal = FALSE)

      #—————————————————————————————————————— #
      ### Result Object ####

      restab <- data.frame(cbind(ci.table[, c("group", "n", "nNA", "m", "sd", "m.diff")], se = c(NA, welch$stderr), ci.table[, c("m.low", "m.upp")], t = c(NA, welch$statistic)*-1L, df = c(NA, welch$parameter), pval = c(NA, welch$p.value), d = d$d, d.low = d$low, d.upp = d$upp), row.names = NULL)

    #—————————————————————————————————————— #
    ### More Than One Outcome Variable ####

    } else {

      # Default S3 method
      restab <- data.frame(variable = rep(y.var, each = 2L), do.call("rbind", lapply(y.var, function(y) misty::test.welch(eval(parse(text = paste0(y, " ~ ", group.var))), data = x, alternative = alternative, conf.level = conf.level, weighted = weighted, correct = correct, check = FALSE, output = FALSE)$result)))

      # Multiple-testing adjustment
      if (isTRUE(p.adj != "none")) { restab$pval <- p.adjust(restab$pval, method = p.adj) }

    }

    sample <- "two"

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Welch's Test for More Than Two Groups ####

  } else if (isTRUE(sample == "multiple")) {

    #—————————————————————————————————————— #
    ### Descriptive Statistics ####

    ci.table <- misty::ci.mean(y, group = group, adjust = adjust, output = FALSE)$result[, c("group", "n", "nNA", "m", "low", "upp", "sd", "skew", "kurt")]

    #—————————————————————————————————————— #
    ### ANOVA Table ####

    aov.table <- summary(aov(y ~ group))[[1L]]

    ss.m <- aov.table[["Sum Sq"]][1L]
    df.m <- aov.table[["Df"]][1L]

    ms.r <- aov.table[["Mean Sq"]][2L]
    ss.t <- sum(aov.table[["Sum Sq"]])

    #—————————————————————————————————————— #
    ### Eta Squared ####

    eta.sq <- ss.m / ss.t

    #—————————————————————————————————————— #
    ### Omega Squared ####

    omega.sq <- (ss.m - df.m*ms.r) / (ss.t + ms.r) |> (\(y) ifelse(y < 0L, 0L, y))()

    #—————————————————————————————————————— #
    ### Welch's ANOVA ####

    welch <- oneway.test(y ~ as.factor(group), data = x, var.equal = FALSE)

    #—————————————————————————————————————— #
    ### Post Hoc Test ####

    # Generate all pairwise combinations
    combs <- combn(levels(group), m = 2L)

    # Number of groups
    groups <- length(levels(group))

    # Sample size
    n <- setNames(ci.table[["n"]], ci.table[["group"]])

    # Mean and variance
    means <- setNames(ci.table[["m"]], ci.table[["group"]])
    vars <- setNames(ci.table[["sd"]]^2L, ci.table[["group"]])

    # Post hoc tests
    result.ph <- lapply(seq_len(ncol(combs)), function(x) {

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
      p <- stats::ptukey(t * sqrt(2L), groups, df, lower.tail = FALSE)

      # Sigma standard error
      se <- sqrt(0.5 * (temp.var1 / temp.n1 + temp.var2 / temp.n2))

      # Lower Confidence Limit
      m.low <- lapply(seq_len(ncol(combs)), function(x) { m.diff - stats::qtukey(p = 0.95, nmeans = groups, df = df) * se })[[1L]]

      # Upper Confidence Limit
      m.upp <- lapply(seq_len(ncol(combs)), function(x) { m.diff + stats::qtukey(p = 0.95, nmeans = groups, df = df) * se })[[1L]]

      # Cohen's d
      data.temp <- data.frame(group, y)[which(group %in% c(combs[1L, x], combs[2L, x])), ]

      # Drop factor levels
      data.temp[, "group"] <- droplevels(data.temp[, "group"], except = c(combs[1L, x], combs[2L, x]))

      cohen <- misty::cohens.d(y ~ group, data = data.temp, weighted = weighted, correct = correct, conf.level = conf.level, check = FALSE, output = FALSE)$result

      # Collect results
      result.ph <- list(combs[1L, x], combs[2L, x], m.diff, se, m.low, m.upp, t, df, p, d = cohen[2L, "d"], cohen[2L, "low"], cohen[2L, "upp"])

    })

    # Result table
    result.ph <- data.frame(matrix(unlist(lapply(result.ph, function(y) { unlist(y) })), nrow = ncol(combs), byrow = TRUE, dimnames = list(NULL, c("group1", "group2", "m.diff", "se", "m.low", "m.upp", "t", "df", "pval", "d", "d.low", "d.upp"))))

    # Convert to numeric
    result.ph[, c(3L:ncol(result.ph))] <- as.numeric(as.matrix(result.ph[, c(3L:ncol(result.ph))]))

    #—————————————————————————————————————— #
    ### Result Object ####

    restab <- list(descript = ci.table,
                   test = data.frame(F = welch$statistic, df1 = welch$parameter["num df"], df2 = welch$parameter["denom df"], pval = welch$p.value, eta.sq = eta.sq, omega.sq = omega.sq, row.names = NULL),
                   posthoc = result.ph)

    sample <- "multiple"

  }

  #_____________________________________________________________________________
  #
  # Return Object ——————————————————————————————————————————————————————————————

  object <- list(call = match.call(),
                 type = "test.welch",
                 sample = sample,
                 data = data.frame(y, group = factor(group)),
                 formula = formula,
                 args = list(alternative = alternative, hypo = hypo, descript = descript, effsize = effsize, weighted = weighted, correct = correct, posthoc = posthoc, conf.level = conf.level, p.adj = p.adj, digits = digits, p.digits = p.digits, as.na = as.na, plot = plot, geom = geom, adjust = adjust, filename = filename, width = width, height = height, dpi = dpi, write = write, append = append, check = check, output = output),
                 plot = NULL,
                 result = restab)

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Plot and Save Plot —————————————————————————————————————————————————————————

  if (isTRUE(plot)) { object$plot <- plot(object, filename = filename, width = width, height = height, dpi = dpi, check = FALSE) |> (\(p) suppressMessages(suppressWarnings(print(p))))() }

  #_____________________________________________________________________________
  #
  # Write Results ——————————————————————————————————————————————————————————————

  if (isTRUE(!is.null(write))) { .write.result(object = object, write = write, append = append) }

  #_____________________________________________________________________________
  #
  # Output —————————————————————————————————————————————————————————————————————

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}

#_______________________________________________________________________________
