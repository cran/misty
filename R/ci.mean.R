#' (Bootstrap) Confidence Intervals for Arithmetic Means and Medians
#'
#' The function \code{ci.mean} computes and plots confidence intervals for
#' arithmetic means with known or unknown population standard deviation or
#' population variance and the function \code{ci.median} computes confidence
#' intervals for medians, optionally by a grouping and/or split variable. These
#' functions also supports six types of bootstrap confidence intervals (e.g.,
#' bias-corrected (BC) percentile bootstrap or bias-corrected and accelerated
#' (BCa) bootstrap confidence intervals) and plots the bootstrap samples with
#' histograms and density curves.
#'
#' @param data              a numeric vector or data frame with numeric
#'                          variables, i.e., factors and character variables are
#'                          excluded from \code{data} before conducting the
#'                          analysis.
#' @param ...               an expression indicating the variable names in \code{data}
#'                          e.g., \code{ci.mean(x1, x2, data = dat)}. Note that the
#'                          operators \code{+}, \code{-}, \code{~}, \code{:},
#'                          \code{::}, and \code{!} can also be used to select
#'                          variables, see 'Details' in the \code{\link{df.subset}}
#'                          function.
#' @param sigma             a numeric vector indicating the population standard
#'                          deviation when computing confidence intervals for the
#'                          arithmetic mean with known standard deviation Note
#'                          that either argument \code{sigma} or argument
#'                          \code{sigma2} is specified and it is only possible to
#'                          specify one value for the argument \code{sigma} even
#'                          though multiple variables are specified in \code{data}.
#' @param sigma2            a numeric vector indicating the population variance
#'                          when computing confidence intervals for the arithmetic
#'                          mean with known variance. Note that either argument
#'                          \code{sigma} or argument \code{sigma2} is specified
#'                          and it is only possible to specify one value for the
#'                          argument \code{sigma2} even though multiple variables
#'                          are specified in \code{data}.
#' @param adjust            logical: if \code{TRUE}, difference-adjustment for
#'                          the confidence intervals for the arithmetic mean
#'                          (Baguley, 2012) is applied.
#' @param boot              a character string specifying the type of bootstrap
#'                          confidence intervals (CI), i.e., \code{"none"}
#'                          (default) for not conducting bootstrapping, \code{"norm"}
#'                          for the bias-corrected normal approximation bootstrap CI,
#'                          \code{"basic"} for the basic bootstrap CI, \code{"stud"}
#'                          for the studentized bootstrap CI, \code{"perc"}, for
#'                          the percentile bootstrap CI \code{"bc"} for the bias-corrected
#'                          (BC) percentile bootstrap CI (without acceleration),
#'                          and \code{"bca"} for the bias-corrected and accelerated
#'                          (BCa) bootstrap CI, see 'Details' in the
#'                          \code{\link{ci.cor}} function.
#' @param R                 a numeric value indicating the number of bootstrap
#'                          replicates (default is 1000).
#' @param seed              a numeric value specifying seeds of the pseudo-random
#'                          numbers used in the bootstrap algorithm when conducting
#'                          bootstrapping.
#' @param sample            logical: if \code{TRUE} (default), the univariate
#'                          sample skewness and kurtosis is computed, while the
#'                          population skewness and kurtosis is computed when
#'                          \code{sample = FALSE}.
#' @param alternative       a character string specifying the alternative hypothesis,
#'                          must be one of \code{"two.sided"} (default),
#'                          \code{"greater"} or \code{"less"}.
#' @param conf.level        a numeric value between 0 and 1 indicating the confidence
#'                          level of the interval.
#' @param group             either a character string indicating the variable name
#'                          of the grouping variable in \code{data}, or a vector
#'                          representing the grouping variable. The grouping variable
#'                          is excluded from the data frame specified in \code{data}.
#'                          Note that a grouping variable can only be used when
#'                          computing confidence intervals with unknown population
#'                          standard deviation and population variance.
#' @param split             either a character string indicating the variable name
#'                          of the split variable in \code{data}, or a vector
#'                          representing the split variable. The split variable
#'                          is excluded from the data frame specified in \code{data}.
#'                          Note that a grouping variable can only be used when
#'                          computing confidence intervals with unknown population
#'                          standard deviation and population variance.
#' @param sort.var          logical: if \code{TRUE}, output table is sorted by
#'                          variables when specifying \code{group}.
#' @param na.omit           logical: if \code{TRUE}, incomplete cases are removed
#'                          before conducting the analysis (i.e., listwise deletion)
#'                          when specifying more than one outcome variable.
#' @param digits            an integer value indicating the number of decimal
#'                          places to be used.
#' @param as.na             a numeric vector indicating user-defined missing
#'                          values, i.e. these values are converted to \code{NA}
#'                          before conducting the analysis. Note that \code{as.na()}
#'                          function is only applied to \code{data}, but not to
#'                          \code{group} or \code{split}.
#' @param plot              a character string indicating the type of the plot
#'                          to display, i.e., \code{"none"} (default) for not
#'                          displaying any plots, \code{"ci"} for displaying
#'                          confidence intervals for the arithmetic mean or median,
#'                          \code{"boot"} for displaying bootstrap samples with
#'                          histograms and density curves when the argument
#'                          \code{"boot"} is other than \code{"none"}.
#' @param hist              logical: if \code{TRUE} (default), histograms are
#'                          drawn when plotting bootstrap samples
#'                          (\code{plot = "boot"}).
#' @param density           logical: if \code{TRUE} (default), density curves are
#'                          drawn when plotting bootstrap samples (\code{plot = "boot"}).
#' @param point             logical: if \code{TRUE} (default), vertical lines
#'                          representing the point estimate of the arithmetic
#'                          mean or median are drawn when plotting bootstrap samples
#'                          (\code{plot = "boot"}).
#' @param ci                logical: if \code{TRUE} (default), vertical lines
#'                          representing the bootstrap confidence intervals of
#'                          the arithmetic mean or median are drawn when
#'                          plotting bootstrap samples (\code{plot = "boot"}).
#' @param line              logical: if \code{TRUE}, a horizontal line
#'                          is drawn when \code{plot = "ci"} or a vertical line
#'                          is drawn when \code{plot = "boot"}
#' @param filename          a character string indicating the \code{filename}
#'                          argument including the file extension in the \code{ggsave}
#'                          function. Note that one of \code{".eps"}, \code{".ps"},
#'                          \code{".tex"}, \code{".pdf"} (default),
#'                          \code{".jpeg"}, \code{".tiff"}, \code{".png"},
#'                          \code{".bmp"}, \code{".svg"} or \code{".wmf"} needs
#'                          to be specified as file extension in the \code{file}
#'                          argument. Note that plots can only be saved when
#'                          \code{plot = "ci"} or \code{plot = "boot"}.
#' @param width             a numeric value indicating the \code{width} argument
#'                          (default is the size of the current graphics device)
#'                          in the \code{ggsave} function.
#' @param height            a numeric value indicating the \code{height} argument
#'                          (default is the size of the current graphics device)
#'                          in the \code{ggsave} function.
#' @param dpi               a numeric value indicating the \code{dpi} argument
#'                          (default is \code{600}) in the \code{ggsave} function.
#' @param write             a character string naming a file for writing the output
#'                          into either a text file with file extension \code{".txt"}
#'                          (e.g., \code{"Output.txt"}) or Excel file with file
#'                          extension \code{".xlsx"}  (e.g., \code{"Output.xlsx"}).
#'                          If the file name does not contain any file extension,
#'                          an Excel file will be written.
#' @param append            logical: if \code{TRUE} (default), output will be
#'                          appended to an existing text file with extension
#'                          \code{.txt} specified in \code{write}, if \code{FALSE}
#'                          existing text file will be overwritten.
#' @param check             logical: if \code{TRUE} (default), argument specification
#'                          is checked.
#' @param output            logical: if \code{TRUE} (default), output is shown
#'                          on the console.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @name ci.mean
#'
#' @seealso
#' \code{\link{test.z}}, \code{\link{test.t}}, \code{\link{ci.mean.diff}},
#' \code{\link{ci.cor}}, \code{\link{ci.prop}}, \code{\link{ci.var}},
#' \code{\link{ci.sd}}, \code{\link{descript}}
#'
#' @references
#' Baguley, T. S. (2012). \emph{Serious stats: A guide to advanced statistics for
#' the behavioral sciences}. Palgrave Macmillan.
#'
#' Canty, A., & Ripley, B. (2024). \emph{boot: Bootstrap R (S-Plus) Functions}.
#' R package version 1.3-31.
#'
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology
#' - Using R and SPSS}. John Wiley & Sons.
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#'
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{data}}{list with the input specified in \code{data}, \code{group}, and \code{split}}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{boot}}{data frame with bootstrap replicates of the arithmetic mean of median when bootstrapping was requested}
#' \item{\code{plot}}{ggplot2 object for plotting the results and the data frame used for plotting}
#' \item{\code{result}}{result table}
#'
#' @note
#' Bootstrap confidence intervals are computed using the R package \code{boot}
#' by Angelo Canty and Brain Ripley (2024).
#'
#' @export
#'
#' @examples
#' #————————————————————————————————————————————————————————————————————————————
#' # Confidence Interval (CI) for the Arithmetic Mean
#'
#' # Example 1a: Two-Sided 95% CI
#' ci.mean(mtcars)
#'
#' # Example 1b: Two-Sided 95% Difference-Adjusted CI
#' ci.mean(mtcars, adjust = TRUE)
#'
#' # Example 1c: Two-Sided 95% CI with known population standard deviation
#' ci.mean(mtcars, mpg, sigma = 6)
#'
#' # Alternative specification without using the '...' argument
#' ci.mean(mtcars$mpg, sigma = 6)
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Confidence Interval (CI) for the Median
#'
#' # Example 2a: Two-Sided 95% CI
#' ci.median(mtcars)
#'
#' # Example 2b: One-Sided 99% CI
#' ci.median(mtcars, alternative = "less", conf.level = 0.99)
#'
#' \dontrun{
#' #————————————————————————————————————————————————————————————————————————————
#' # Bootstrap Confidence Interval (CI)
#'
#' # Example 3a: Bias-corrected (BC) percentile bootstrap CI
#' ci.mean(mtcars, boot = "bc")
#'
#' # Example 3b: Bias-corrected and accelerated (BCa) bootstrap CI,
#' # 5000 bootstrap replications, set seed of the pseudo-random number generator
#' ci.mean(mtcars, boot = "bca", R = 5000, seed = 42)
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Grouping and Split Variable
#'
#' # Example 4a: Grouping variable
#' ci.mean(mtcars, mpg, cyl, disp, group = "vs")
#'
#' # Alternative specification without using the '...' argument
#' ci.mean(mtcars[, c("mpg", "cyl", "disp")], group = mtcars$vs)
#'
#' # Example 4b: Split variable
#' ci.mean(mtcars, mpg, cyl, disp, split = "am")
#'
#' # Alternative specification
#' ci.mean(mtcars[, c("mpg", "cyl", "disp")], split = mtcars$am)
#'
#' # Example 4c: Grouping and split variable
#' ci.mean(mtcars, mpg, cyl, disp, group = "vs", split = "am")
#'
#' # Alternative specification
#' ci.mean(mtcars[, c("mpg", "cyl", "disp")], group = mtcars$vs, split = mtcars$am)
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Plot Confidence Intervals
#'
#' # Example 5a: Two-Sided 95% CI
#' ci.mean(mtcars, disp, hp, plot = "ci")
#'
#' # Example 5b: Grouping variable
#' ci.mean(mtcars, disp, hp, group = "vs", plot = "ci")
#'
#' # Example 5c: Split variable
#' ci.mean(mtcars, disp, hp, split = "am", plot = "ci")
#'
#' # Example 5d: Plot results using the plot() function, use additional arguments
#' # see Details in the help page of the function plot.misty.object
#' object <- ci.mean(mtcars, disp, hp, plot = "ci")
#' plot(object, ybreaks = seq(0, 300, by = 50), title = "Confidence Intervals")
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Plot Bootstrap Samples
#'
#' # Example 6a: Two-Sided 95% CI
#' ci.mean(mtcars, disp, hp, boot = "bc", plot = "boot")
#'
#' # Example 6b: Grouping variable
#' ci.mean(mtcars, disp, hp, group = "vs", boot = "bc", plot = "boot")
#'
#' # Example 6c: Split variable
#' ci.mean(mtcars, disp, hp, split = "am", boot = "bc", plot = "boot")
#'
#' # Example 6d: Plot results using the plot() function, use additional arguments
#' # see Details in the help page of the function plot.misty.object
#' object <- ci.mean(mtcars, disp, hp, boot = "bc", plot = "boot")
#' plot(object, fill = "gray30", title = "Bootstrap Samples")
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Write Results and Save Plot
#'
#' # Example 7a: Write results into a text file
#' ci.mean(mtcars, write = "CI_Mean_Text.txt")
#'
#' # Example 7b: Write results into an Excel file
#' ci.mean(mtcars, write = "CI_Mean_Excel.xlsx")
#'
#' # Example 7ce: Save plot as PNG file
#' ci.mean(mtcars, disp, hp, plot = "ci", filename = "CI_Mean.png",
#'         width = 9, height = 6)
#' }
ci.mean <- function(data, ..., sigma = NULL, sigma2 = NULL, adjust = FALSE,
                    boot = c("none", "norm", "basic", "stud", "perc", "bc", "bca"),
                    R = 1000, seed = NULL, sample = TRUE,
                    alternative = c("two.sided", "less", "greater"),
                    conf.level = 0.95, group = NULL, split = NULL, sort.var = FALSE,
                    na.omit = FALSE, digits = 2, as.na = NULL,
                    plot = c("none", "ci", "boot"), hist = TRUE, density = TRUE,
                    point = TRUE, ci = TRUE, line = TRUE, filename = NULL,
                    width = NA, height = NA, dpi = 600, write = NULL, append = TRUE,
                    check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'data' is missing or NULL
  if (isTRUE(missing(data) || is.null(data))) { stop("Please specify a data frame with numeric variables for the argument 'data'", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data using the argument '...' ####

  if (isTRUE(!missing(...))) {

    # Extract data and convert tibble into data frame or vector
    x <- data[, .var.names(data = data, ..., group = group, split = split), drop = FALSE] |> (\(y) if (isTRUE("tbl" %in% substr(class(y), 1L, 3L))) { if (isTRUE(ncol(as.data.frame(y)) == 1L)) { unname(unlist(y)) } else { as.data.frame(y) } } else { y })()

    # Extract grouping variable and convert tibble into a vector
    if (isTRUE(!is.null(group))) { group <- data[, group] |> (\(y) if (isTRUE("tbl" %in% substr(class(y), 1L, 3L))) { unname(unlist(y)) } else { return(y) })() }

    # Extract splitting variable and convert tibble into a vector
    if (isTRUE(!is.null(split))) { split <- data[, split] |> (\(y) if (isTRUE("tbl" %in% substr(class(y), 1L, 3L))) { unname(unlist(y)) } else { return(y) })() }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data without using the argument '...' ####

  } else {

    # Data frame
    x <- as.data.frame(data)

    # Data and cluster
    var.group <- .var.group(data = x, group = group, split = split)

    # Data
    if (isTRUE(!is.null(var.group$data)))  { x <- var.group$data }

    # Grouping variable
    if (isTRUE(!is.null(var.group$group))) { group <- var.group$group }

    # Split variable
    if (isTRUE(!is.null(var.group$split))) { split <- var.group$split }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Numeric Variables ####

  x <- x |> (\(y) !vapply(y, is.numeric, FUN.VALUE = logical(1L)))() |> (\(z) if (isTRUE(any(z))) {

    warning(paste0("Non-numeric variables were excluded from the analysis: ", paste(names(which(z)), collapse = ", ")), call. = FALSE)

    return(x[, -which(z), drop = FALSE])

  } else {

    return(x)

  })()

  if (isTRUE(ncol(x) == 0L)) { stop("No variables left for analysis after excluding non-numeric variables.", call. = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Grouping and Split Variable ####

  # Grouping variable
  if (!is.null(group)) {

    x <- which(sapply(names(x), function(y) identical(group, x[, y]))) |> (\(z) if (isTRUE(length(z) != 0L)) { return(x[, -z]) } else { x })()

    if (isTRUE(ncol(x) == 0L)) { stop("After excluding the grouping variable from the data frame, there are no variables left.") }

  }

  # Split variable
  if (!is.null(split)) {

    x <- which(sapply(names(x), function(y) identical(split, x[, y]))) |> (\(z) if (isTRUE(length(z) != 0L)) { return(x[, -z]) } else { x })()

    if (isTRUE(ncol(x) == 0L)) { stop("After excluding the split variable from the data frame, there are no variables left.") }

  }

  # Grouping and split variable are identical
  if (isTRUE(!is.null(group) && !is.null(split) && identical(group, split))) { stop("Grouping and split variables are identical.", call. = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) { x <- .as.na(x, na = as.na) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Listwise deletion ####

  # Check input 'na.omit'
  .check.input(logical = "na.omit", envir = environment(), input.check = check)

  if (isTRUE(na.omit && any(is.na(x)))) {

    assign("x", na.omit(x)) |> (\(y) warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ", length(attributes(y)$na.action)), call. = FALSE))()

    # Grouping variable
    if (isTRUE(!is.null(group))) { group <- group[-attributes(na.omit(x))$na.action] }

    # Split variable
    if (isTRUE(!is.null(split))) { split <- split[-attributes(na.omit(x))$na.action] }

  }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("adjust", "sample", "sort.var", "na.omit", "point", "ci", "line", "append", "output"),
               numeric = list(seed = 1L, width = 1L, height = 1L, dpi = 1L), s.character = list(boot = c("none", "norm", "basic", "stud", "perc", "bc", "bca"), plot = c("none", "ci", "boot")),
               args = c("R", "alternative", "conf.level", "digits", "write2"), envir = environment(), input.check = check)

  # Additional checks
  if (isTRUE(isTRUE(check))) {

    # Check input 'sigma' and 'sigma2'
    if (isTRUE(!is.null(sigma) && !is.null(sigma2))) { stop("Please specify either argument 'sigma' or argument 'sigma2', but not both.", call. = FALSE) }

    # Check input 'sigma'
    if (isTRUE(!is.null(sigma) && (sigma <= 0L || length(sigma) != 1L))) { stop("Please specify a numeric value grater than 0 for the argument 'sigma'.", call. = FALSE) }

    # Check input 'sigma2'
    if (isTRUE(!is.null(sigma2) && (sigma2 <= 0L || length(sigma2) != 1L))) { stop("Please specify a numeric value grater than 0 for the argument 'sigma2'.", call. = FALSE) }

    if (isTRUE(!"none" %in% boot)) {

      if (isTRUE(!is.null(sigma))) { stop("Confidence intervals with known population standard deviation are not supported when using bootstrapping.", call. = FALSE) }

      if (isTRUE(!is.null(sigma2))) { stop("Confidence intervals with known population variance are not supported when using bootstrapping.", call. = FALSE) }

       if (isTRUE(adjust)) { stop("Difference-adjustment is not supported for bootstrap confidence intervals.", call. = FALSE) }

    }

    # Check input 'group'
    if (isTRUE(!is.null(group))) {

      # Population standard deviation
      if (isTRUE(!is.null(sigma))) { stop("Grouping variable cannot be used for confidence intervals with known population standard deviation.", call. = FALSE) }

      # Population variance
      if (isTRUE(!is.null(sigma2))) { stop("Grouping variable cannot be used for confidence intervals with known population variance.", call. = FALSE) }

      # Input 'group' completely missing
      if (isTRUE(all(is.na(group)))) { stop("The grouping variable specified in 'group' is completely missing.", call. = FALSE) }

      # Only one group in 'group'
      if (isTRUE(length(na.omit(unique(unlist(group)))) == 1L)) { warning("There is only one group represented in the grouping variable specified in 'group'.", call. = FALSE) }

    }

    # Check input 'split'
    if (isTRUE(!is.null(split))) {

      # Population standard deviation
      if (isTRUE(!is.null(sigma))) { stop("Split variable cannot be used for confidence intervals with known population standard deviation.", call. = FALSE) }

      # Population variance
      if (isTRUE(!is.null(sigma2))) { stop("Split variable cannot be used for confidence intervals with known population variance.", call. = FALSE) }

      # Input 'split' completely missing
      if (isTRUE(all(is.na(split)))) { stop("The split variable specified in 'split' is completely missing.", call. = FALSE) }

      # Only one group in 'split'
      if (isTRUE(length(na.omit(unique(split))) == 1L)) { warning("There is only one group represented in the split variable specified in 'split'.", call. = FALSE) }

    }

    # Check input 'plot'
    if (isTRUE(all(plot == "boot") && (all(boot == "none") || all(c("none", "norm", "basic", "stud", "perc", "bc", "bca") %in% boot)))) { stop("Please request bootstrap confidence intervals by specifying the 'boot' argument to plot bootstrap samples.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  # 'sigma' and 'sigma2' Argument
  if (isTRUE(is.null(sigma) && !is.null(sigma2))) { sigma <- sqrt(sigma2) }

  if (isTRUE(!is.null(sigma) && is.null(sigma2))) { sigma2 <- sigma^2 }

  # 'boot' Argument
  boot <- ifelse(all(c("none", "norm", "basic", "stud", "perc", "bc", "bca") %in% boot), "none", boot)

  # 'alternative' Argument
  if (isTRUE(all(c("two.sided", "less", "greater") %in% alternative))) { alternative <- "two.sided" }

  # 'plot' Argument
  plot <- ifelse(all(c("none", "ci", "boot") %in% plot), "none", plot)

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## No Grouping, No Split ####

  if (isTRUE(is.null(group) && is.null(split))) {

    #——————————————————————————————————————
    ### No Bootstrapping ####

    if (isTRUE(boot == "none")) {

      result <- data.frame(variable = colnames(x),
                           n = vapply(x, function(y) length(na.omit(y)), FUN.VALUE = 1L),
                           nNA = vapply(x, function(y) sum(is.na(y)), FUN.VALUE = 1L),
                           pNA = vapply(x, function(y) sum(is.na(y)) / length(y) * 100, FUN.VALUE = double(1L)),
                           # Standard deviation
                           sd = vapply(x, function(y) ifelse(length(na.omit(y)) <= 1L, NA, sd(y, na.rm = TRUE)), FUN.VALUE = double(1L)),
                           # Skewness
                           skew = vapply(x, function(y) ifelse(length(na.omit(y)) <= 3L, NA, suppressWarnings(misty::skewness(y, sample = sample, check = FALSE))), FUN.VALUE = double(1L)),
                           # Kurtosis
                           kurt = vapply(x, function(y) ifelse(length(na.omit(y)) <= 4L, NA, suppressWarnings(misty::kurtosis(y, sample = sample, check = FALSE))), FUN.VALUE = double(1L)),
                           # Arithmetic mean
                           m = vapply(x, function(y) ifelse(length(na.omit(y)) <= 1L, NA, mean(y, na.rm = TRUE)), FUN.VALUE = double(1L)),
                           # Confidence interval for the arithmetic mean
                           low = vapply(x, .m.conf, sigma = sigma, adjust = adjust, alternative = alternative, conf.level = conf.level, side = "low", FUN.VALUE = double(1L)),
                           upp = vapply(x, .m.conf, sigma = sigma, adjust = adjust, alternative = alternative, conf.level = conf.level, side = "upp", FUN.VALUE = double(1L)),
                           row.names = NULL, check.names = FALSE)

    #——————————————————————————————————————
    ### Bootstrapping ####

    } else {

      result.boot <- apply(matrix(seq_len(ncol(x)), nrow = 1L), 2L, function(y) x |>
                             (\(z) suppressWarnings(.ci.boot(data = x[, y], statistic = .boot.func.mean, boot = boot, R = R, alternative = alternative, conf.level = conf.level, seed = seed)))() |>
                             (\(w) list(t = data.frame(variable = colnames(x)[y], m = w$t), result = data.frame(variable = colnames(x)[y], n = w$n, nNA = w$nNA, pNA = w$pNA, sd = w$sd, skew = w$skew, kurt = w$kurt, m = w$t0[1L], low = w$ci[1L], upp = w$ci[2L], row.names = NULL)))())

      boot.sample <- do.call("rbind", lapply(result.boot, function(y) y$t))
      result <- do.call("rbind", lapply(result.boot, function(y) y$result))

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Grouping, No Split ####

  } else if (isTRUE(!is.null(group) && is.null(split))) {

    #——————————————————————————————————————
    ### No Bootstrapping ####

    if (isTRUE(boot == "none")) {

      result <- lapply(split(x, f = group), function(y) misty::ci.mean(y, group = NULL, split = NULL, adjust = adjust, sample = sample, alternative = alternative, conf.level = conf.level, sort.var = sort.var, check = FALSE, output = FALSE)$result) |>
        (\(y) data.frame(group = rep(names(y), each = ncol(x)), eval(parse(text = paste0("rbind(", paste0("y[[", seq_len(length(y)), "]]", collapse = ", "), ")")))) )()

    #——————————————————————————————————————
    ### Bootstrapping ####

    } else {

      result.boot <- lapply(split(x, f = group), function(y) misty::ci.mean(y, group = NULL, split = NULL, sample = sample, seed = seed, boot = boot, R = R, alternative = alternative, conf.level = conf.level, check = FALSE, output = FALSE)) |>
        (\(z) list(boot.sample = data.frame(group = rep(names(z), each = R*unique(unlist(lapply(z, function(q) nrow(q$result))))), do.call("rbind", lapply(z, function(w) w$boot)), row.names = NULL),
                   result = data.frame(group = rep(names(z), each = unique(unlist(lapply(z, function(q) nrow(q$result))))), do.call("rbind", lapply(z, function(w) w$result)), row.names = NULL)))()

      boot.sample <- result.boot$boot.sample
      result <- result.boot$result

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## No Grouping, Split ####

  } else if (isTRUE(is.null(group) && !is.null(split))) {

    #——————————————————————————————————————
    ### No Bootstrapping ####

    if (isTRUE(boot == "none")) {

      result <- lapply(split(data.frame(x), f = split), function(y) misty::ci.mean(y, group = NULL, split = NULL, adjust = adjust, sample = sample, alternative = alternative, conf.level = conf.level, sort.var = sort.var, check = FALSE, output = FALSE)$result)

    #——————————————————————————————————————
    ### Bootstrapping ####

    } else {

      result.boot <- lapply(split(x, f = split), function(y) misty::ci.mean(y, group = NULL, split = NULL, sample = sample, seed = seed, boot = boot, R = R, alternative = alternative, conf.level = conf.level, check = FALSE, output = FALSE)) |>
        (\(z) list(boot.sample = data.frame(split = rep(names(z), each = R*unique(unlist(lapply(z, function(q) nrow(q$result))))), do.call("rbind", lapply(z, function(w) w$boot)), row.names = NULL), result = lapply(z, function(w) w$result)))()

      boot.sample <- result.boot$boot.sample
      result <- result.boot$result

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Grouping, Split ####

  } else if (isTRUE(!is.null(group) && !is.null(split))) {

    #——————————————————————————————————————
    ### No Bootstrapping ####

    if (isTRUE(boot == "none")) {

      result <- lapply(split(data.frame(x, group = group), f = split), function(y) misty::ci.mean(y[, -grep("group", names(y))], group = y$group, split = NULL, adjust = adjust, alternative = alternative, conf.level = conf.level, sort.var = sort.var, check = FALSE, output = FALSE)$result)

    #——————————————————————————————————————
    ### Bootstrapping ####

    } else {

      result.boot <- lapply(split(data.frame(x, group = group), f = split), function(y) misty::ci.mean(y[, -grep("group", names(y))], group = y$group, split = NULL, sample = sample, seed = seed, boot = boot, R = R, alternative = alternative, conf.level = conf.level, check = FALSE, output = FALSE)) |>
        (\(z) list(boot.sample = data.frame(split = rep(names(z), each = R*unique(unlist(lapply(z, function(q) nrow(q$result))))), do.call("rbind", lapply(z, function(w) w$boot)), row.names = NULL), result = lapply(z, function(w) w$result)))()

      boot.sample <- result.boot$boot.sample
      result <- result.boot$result

    }

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "ci.mean",
                 data = list(x = x, group = group, split = split),
                 args = list(sigma = sigma, sigma2 = sigma2, adjust = adjust, boot = boot, R = R, seed = seed, sample = sample, alternative = alternative, conf.level = conf.level, sort.var = sort.var, na.omit = na.omit, digits = digits, as.na = as.na, plot = plot, hist = hist, density = density, point = point, ci = ci, line = line, filename = filename, width = width, height = height, dpi = dpi, write = write, append = append, check = check, output = output),
                 boot = if (isTRUE(boot != "none")) { boot.sample } else { NULL },
                 plot = NULL, result = result)

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Plot and Save Results ------------------------------------------------------

  if (isTRUE(plot != "none")) { object$plot <- plot(object, filename = filename, width = width, height = height, dpi = dpi, check = FALSE) |> (\(y) suppressMessages(suppressWarnings(print(y))))() }

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
#_______________________________________________________________________________

#' @rdname ci.median
ci.median <- function(data, ..., boot = c("none", "norm", "basic", "stud", "perc", "bc", "bca"),
                      R = 1000, seed = NULL, sample = TRUE,
                      alternative = c("two.sided", "less", "greater"),
                      conf.level = 0.95, group = NULL, split = NULL, sort.var = FALSE,
                      na.omit = FALSE, digits = 2, as.na = NULL, plot = c("none", "ci", "boot"),
                      hist = TRUE, density = TRUE, point = TRUE, ci = TRUE, line = TRUE,
                      filename = NULL, width = NA, height = NA, dpi = 600, write = NULL,
                      append = TRUE, check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'data' is missing
  if (isTRUE(missing(data))) { stop("Please specify a numeric vector or data frame for the argument 'data'", call. = FALSE) }

  # Check if input 'data' is NULL
  if (isTRUE(is.null(data))) { stop("Input specified for the argument 'data' is NULL.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data using the argument 'data' ####

  if (isTRUE(!missing(...))) {

    # Extract data and convert tibble into data frame or vector
    x <- data[, .var.names(..., data = data, group = group, split = split), drop = FALSE] |> (\(y) if (isTRUE("tbl" %in% substr(class(y), 1L, 3L))) { if (isTRUE(ncol(as.data.frame(y)) == 1L)) { unname(unlist(y)) } else { as.data.frame(y) } } else { y })()

    # Extract grouping variable and convert tibble into a vector
    if (isTRUE(!is.null(group))) { group <- data[, group] |> (\(y) if (isTRUE("tbl" %in% substr(class(y), 1L, 3L))) { unname(unlist(y)) } else { return(y) })() }

    # Extract splitting variable and convert tibble into a vector
    if (isTRUE(!is.null(split))) { split <- data[, split] |> (\(y) if (isTRUE("tbl" %in% substr(class(y), 1L, 3L))) { unname(unlist(y)) } else { return(y) })() }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data without using the argument 'data' ####

  } else {

    # Data frame
    x <- as.data.frame(data)

    # Data and cluster
    var.group <- .var.group(data = x, group = group, split = split)

    # Data
    if (isTRUE(!is.null(var.group$data)))  { x <- var.group$data }

    # Grouping variable
    if (isTRUE(!is.null(var.group$group))) { group <- var.group$group }

    # Split variable
    if (isTRUE(!is.null(var.group$split))) { split <- var.group$split }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Numeric Variables ####

  x <- x |> (\(y) !vapply(y, is.numeric, FUN.VALUE = logical(1L)))() |> (\(z) if (isTRUE(any(z))) {

    warning(paste0("Non-numeric variables were excluded from the analysis: ", paste(names(which(z)), collapse = ", ")), call. = FALSE)

    return(x[, -which(z), drop = FALSE])

  } else {

    return(x)

  })()

  if (isTRUE(ncol(x) == 0L)) { stop("No variables left for analysis after excluding non-numeric variables.", call. = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Grouping and Split Variable ####

  # Grouping variable
  if (!is.null(group)) {

    x <- which(sapply(names(x), function(y) identical(group, x[, y]))) |> (\(z) if (isTRUE(length(z) != 0L)) { return(x[, -z]) } else { x })()

    if (isTRUE(ncol(x) == 0L)) { stop("After excluding the grouping variable from the data frame, there are no variables left.") }

  }

  # Split variable
  if (!is.null(split)) {

    x <- which(sapply(names(x), function(y) identical(split, x[, y]))) |> (\(z) if (isTRUE(length(z) != 0L)) { return(x[, -z]) } else { x })()

    if (isTRUE(ncol(x) == 0L)) { stop("After excluding the split variable from the data frame, there are no variables left.") }

  }

  # Grouping and split variable are identical
  if (isTRUE(!is.null(group) && !is.null(split) && identical(group, split))) { stop("Grouping and split variables are identical.", call. = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) { x <- .as.na(x, na = as.na) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Listwise deletion ####

  # Check input 'na.omit'
  .check.input(logical = "na.omit", envir = environment(), input.check = check)

  if (isTRUE(na.omit && any(is.na(x)))) {

    assign("x", na.omit(x)) |> (\(y) warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ", length(attributes(y)$na.action)), call. = FALSE))()

    # Grouping variable
    if (isTRUE(!is.null(group))) { group <- group[-attributes(na.omit(x))$na.action] }

    # Split variable
    if (isTRUE(!is.null(split))) { split <- split[-attributes(na.omit(x))$na.action] }

  }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("sample", "sort.var", "na.omit", "point", "ci", "line", "append", "output"),
               numeric = list(seed = 1L, width = 1L, height = 1L, dpi = 1L), s.character = list(boot = c("none", "norm", "basic", "stud", "perc", "bc", "bca"), plot = c("none", "ci", "boot")),
               args = c("R", "alternative", "conf.level", "digits", "write2"), envir = environment(), input.check = check)

  # Additional checks
  if (isTRUE(check)) {

    # Check input 'plot'
    if (isTRUE(all(plot == "boot") && boot == "none")) { stop("Please request bootstrap confidence intervals by specifying the 'boot' argument to plot bootstrap samples.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  # 'boot' Argument
  boot <- ifelse(all(c("none", "norm", "basic", "stud", "perc", "bc", "bca") %in% boot), "none", boot)

  # 'alternative' Argument
  if (isTRUE(all(c("two.sided", "less", "greater") %in% alternative))) { alternative <- "two.sided" }

  # 'plot' Argument
  plot <- ifelse(all(c("none", "ci", "boot") %in% plot), "none", plot)

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## No Grouping, No Split ####

  if (isTRUE(is.null(group) && is.null(split))) {

    #——————————————————————————————————————
    ### No Bootstrapping ####

    if (isTRUE(boot == "none")) {

      result <- data.frame(variable = colnames(x),
                           n = vapply(x, function(y) length(na.omit(y)), FUN.VALUE = 1L),
                           nNA = vapply(x, function(y) sum(is.na(y)), FUN.VALUE = 1L),
                           pNA = vapply(x, function(y) sum(is.na(y)) / length(y) * 100L, FUN.VALUE = double(1)),
                           # Standard deviation
                           sd = vapply(x, function(y) ifelse(length(na.omit(y)) <= 1L, NA, sd(y, na.rm = TRUE)), FUN.VALUE = double(1L)),
                           # Interquartile range
                           iqr = vapply(x, function(y) ifelse(length(na.omit(y)) <= 1L, NA, IQR(y, na.rm = TRUE)), FUN.VALUE = double(1L)),
                           # Skewness
                           skew = vapply(x, function(y) ifelse(length(na.omit(y)) <= 3L, NA, suppressWarnings(misty::skewness(y, sample = sample, check = FALSE))), FUN.VALUE = double(1L)),
                           # Kurtosis
                           kurt = vapply(x, function(y) ifelse(length(na.omit(y)) <= 4L, NA, suppressWarnings(misty::kurtosis(y, sample = sample, check = FALSE))), FUN.VALUE = double(1L)),
                           # Median
                           med = vapply(x, function(y) ifelse(length(na.omit(y)) <= 1L, NA, median(y, na.rm = TRUE)), FUN.VALUE = double(1L)),
                           # Confidence interval for the median
                           low = vapply(x, .med.conf, alternative = alternative, conf.level = conf.level, side = "low", FUN.VALUE = double(1L)),
                           upp = vapply(x, .med.conf, alternative = alternative, conf.level = conf.level, side = "upp", FUN.VALUE = double(1L)),
                           row.names = NULL, check.names = FALSE)

    #——————————————————————————————————————
    ### Bootstrapping ####

    } else {

      result.boot <- apply(matrix(seq_len(ncol(x)), nrow = 1L), 2L, function(y) x |>
                             (\(z) suppressWarnings(.ci.boot(data = x[, y], statistic = .boot.func.median, boot = boot, R = R, alternative = alternative, conf.level = conf.level, seed = seed)))() |>
                             (\(w) list(t = data.frame(variable = colnames(x)[y], med = w$t), result = data.frame(variable = colnames(x)[y], n = w$n, nNA = w$nNA, pNA = w$pNA, sd = w$sd, iqr = w$iqr, skew = w$skew, kurt = w$kurt, med = w$t0[1L], low = w$ci[1L], upp = w$ci[2L], row.names = NULL)))())

      boot.sample <- do.call("rbind", lapply(result.boot, function(y) y$t))
      result <- do.call("rbind", lapply(result.boot, function(y) y$result))

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Grouping, No Split ####

  } else if (isTRUE(!is.null(group) && is.null(split))) {

    #——————————————————————————————————————
    ### No Bootstrapping ####

    if (isTRUE(boot == "none")) {

      result <- lapply(split(x, f = group), function(y) misty::ci.median(y, group = NULL, split = NULL, alternative = alternative, conf.level = conf.level, sort.var = sort.var, check = FALSE, output = FALSE)$result) |>
        (\(y) data.frame(group = rep(names(y), each = ncol(x)), eval(parse(text = paste0("rbind(", paste0("y[[", seq_len(length(y)), "]]", collapse = ", "), ")")))) )()

    #——————————————————————————————————————
    ### Bootstrapping ####

    } else {

      result.boot <- lapply(split(x, f = group), function(y) misty::ci.median(y, group = NULL, split = NULL, sample = sample, seed = seed, boot = boot, R = R, alternative = alternative, conf.level = conf.level, check = FALSE, output = FALSE)) |>
        (\(z) list(boot.sample = data.frame(group = rep(names(z), each = R*unique(unlist(lapply(z, function(q) nrow(q$result))))), do.call("rbind", lapply(z, function(w) w$boot)), row.names = NULL), result = data.frame(group = rep(names(z), each = unique(unlist(lapply(z, function(q) nrow(q$result))))), do.call("rbind", lapply(z, function(w) w$result)), row.names = NULL)))()

      boot.sample <- result.boot$boot.sample
      result <- result.boot$result

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## No Grouping, Split ####

  } else if (isTRUE(is.null(group) && !is.null(split))) {

    #——————————————————————————————————————
    ### No Bootstrapping ####

    if (isTRUE(boot == "none")) {

      result <- lapply(split(data.frame(x), f = split), function(y) misty::ci.median(y, group = NULL, split = NULL, alternative = alternative, conf.level = conf.level, sort.var = sort.var, check = FALSE, output = FALSE)$result)

    #——————————————————————————————————————
    ### Bootstrapping ####

    } else {

      result.boot <- lapply(split(x, f = split), function(y) misty::ci.median(y, group = NULL, split = NULL, sample = sample, seed = seed, boot = boot, R = R, alternative = alternative, conf.level = conf.level, check = FALSE, output = FALSE)) |>
        (\(z) list(boot.sample = data.frame(split = rep(names(z), each = R*unique(unlist(lapply(z, function(q) nrow(q$result))))), do.call("rbind", lapply(z, function(w) w$boot)), row.names = NULL), result = lapply(z, function(w) w$result)))()

      boot.sample <- result.boot$boot.sample
      result <- result.boot$result

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Grouping, Split ####

  } else if (isTRUE(!is.null(group) && !is.null(split))) {

    #——————————————————————————————————————
    ### No Bootstrapping ####

    if (isTRUE(boot == "none")) {

    result <- lapply(split(data.frame(x, group = group), f = split), function(y) misty::ci.median(y[, -grep("group", names(y))], group = y$group, split = NULL, alternative = alternative, conf.level = conf.level, sort.var = sort.var, check = FALSE, output = FALSE)$result)

    #——————————————————————————————————————
    ### Bootstrapping ####

    } else {

      result.boot <- lapply(split(data.frame(x, group = group), f = split), function(y) misty::ci.median(y[, -grep("group", names(y))], group = y$group, split = NULL, sample = sample, seed = seed, boot = boot, R = R, alternative = alternative, conf.level = conf.level, check = FALSE, output = FALSE)) |>
        (\(z) list(boot.sample = data.frame(split = rep(names(z), each = R*unique(unlist(lapply(z, function(q) nrow(q$result))))), do.call("rbind", lapply(z, function(w) w$boot)), row.names = NULL), result = lapply(z, function(w) w$result)))()

      boot.sample <- result.boot$boot.sample
      result <- result.boot$result

    }

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "ci.median",
                 data = list(x = x, group = group, split = split),
                 args = list(boot = boot, R = R, seed = seed, sample = sample, alternative = alternative, conf.level = conf.level, sort.var = sort.var, na.omit = na.omit, digits = digits, as.na = as.na, plot = plot, hist = hist, density = density, point = point, ci = ci, line = line, filename = filename, width = width, height = height, dpi = dpi, write = write, append = append, check = check, output = output),
                 boot = if (isTRUE(boot != "none")) { boot.sample } else { NULL },
                 plot = NULL, result = result)

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Plot and Save Plot ---------------------------------------------------------

  if (isTRUE(plot != "none")) { object$plot <- plot(object, filename = filename, width = width, height = height, dpi = dpi, check = FALSE) |> (\(y) suppressMessages(suppressWarnings(print(y))))() }

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
