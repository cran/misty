#' (Bootstrap) Confidence Intervals for Proportions
#'
#' This function computes and plots confidence intervals for proportions,
#' optionally by a grouping and/or split variable. The function also supports
#' three types of bootstrap confidence intervals (e.g., bias-corrected (BC)
#' percentile bootstrap or bias-corrected and accelerated (BCa) bootstrap
#' confidence intervals) and plots the bootstrap samples with histograms and
#' density curves.
#'
#' @param data        a numeric vector or data frame with numeric variables
#'                    with 0 and 1 values.
#' @param ...         an expression indicating the variable names in \code{data},
#'                    e.g., \code{ci.prop(dat, x1, x2, x3)}. Note that the
#'                    operators \code{+}, \code{-}, \code{~}, \code{:},
#'                    \code{::}, and \code{!} can also be used to select
#'                    variables, see 'Details' in the \code{\link{df.subset}}
#'                    function.
#' @param method      a character string specifying the method for computing
#'                    the confidence interval, must be one of \code{"wald"},
#'                    or \code{"wilson"} (default).
#' @param boot        a character string specifying the type of bootstrap
#'                    confidence intervals (CI), i.e., \code{"none"} (default)
#'                    for not conducting bootstrapping, \code{"perc"}, for
#'                    the percentile bootstrap CI \code{"bc"} (default) for
#'                    the bias-corrected (BC) percentile bootstrap CI (without
#'                    acceleration), and \code{"bca"} for the bias-corrected
#'                    and accelerated (BCa) bootstrap CI, see 'Details' in
#'                    the \code{\link{ci.cor}} function.
#' @param nrep        a numeric value indicating the number of bootstrap
#'                    replicates (default is 1000).
#' @param seed        a numeric value specifying seeds of the pseudo-random
#'                    numbers used in the bootstrap algorithm when conducting
#'                    bootstrapping.
#' @param alternative a character string specifying the alternative hypothesis,
#'                    must be one of \code{"two.sided"} (default),
#'                    \code{"greater"} or \code{"less"}.
#' @param conf.level  a numeric value between 0 and 1 indicating the confidence
#'                    level of the interval.
#' @param group       either a character string indicating the variable name
#'                    of the grouping variable in \code{data}, or a vector
#'                    representing the grouping variable.
#' @param split       either a character string indicating the variable name
#'                    of the split variable in \code{data}, or a vector
#'                    representing the split variable.
#' @param sort.var    logical: if \code{TRUE}, output table is sorted by
#'                    variables when specifying \code{group}.
#' @param na.omit     logical: if \code{TRUE}, incomplete cases are removed
#'                    before conducting the analysis (i.e., listwise deletion)
#'                    when specifying more than one outcome variable.
#' @param digits      an integer value indicating the number of decimal
#'                    places to be used.
#' @param as.na       a numeric vector indicating user-defined missing
#'                    values, i.e. these values are converted to \code{NA}
#'                    before conducting the analysis. Note that \code{as.na()}
#'                    function is only applied to \code{data}, but not to
#'                    \code{group} or \code{split}.
#' @param plot        a character string indicating the type of the plot
#'                    to display, i.e., \code{"none"} (default) for not
#'                    displaying any plots, \code{"ci"} for displaying
#'                    confidence intervals for the proportion, \code{"boot"}
#'                    for displaying bootstrap samples with histograms and
#'                    density curves when the argument \code{"boot"} is
#'                    other than \code{"none"}.
#' @param geom        a character string or character vector indicating which
#'                    plot elements to draw, i.e., \code{"all"} for drawing
#'                    all plot elements, \code{"hist"} for drawing histograms
#'                    when plotting bootstrap samples (\code{plot = "boot"}),
#'                    \code{"density"} for drawing density curves when plotting
#'                    bootstrap samples (\code{plot = "boot"}), \code{"point"}
#'                    for drawing vertical lines representing the point estimate
#'                    of the proportion when plotting bootstrap
#'                    samples (\code{plot = "boot"}), \code{"ci"} for drawing
#'                    vertical lines representing the bootstrap confidence intervals
#'                    of the proportion when plotting bootstrap
#'                    samples (\code{plot = "boot"}), and \code{"line"} for drawing
#'                    a horizontal line when specifying \code{plot = "ci"} or a
#'                    vertical line when specifying \code{plot = "boot"}.
#' @param filename    a character string indicating the \code{filename}
#'                    argument including the file extension in the \code{ggsave}
#'                    function. Note that one of \code{".eps"}, \code{".ps"},
#'                    \code{".tex"}, \code{".pdf"} (default),
#'                    \code{".jpeg"}, \code{".tiff"}, \code{".png"},
#'                    \code{".bmp"}, \code{".svg"} or \code{".wmf"} needs
#'                    to be specified as file extension in the \code{file}
#'                    argument. Note that plots can only be saved when
#'                    \code{plot = "ci"} or \code{plot = "boot"}.
#' @param width       a numeric value indicating the \code{width} argument
#'                    (default is the size of the current graphics device)
#'                    in the \code{ggsave} function.
#' @param height      a numeric value indicating the \code{height} argument
#'                    (default is the size of the current graphics device)
#'                    in the \code{ggsave} function.
#' @param dpi         a numeric value indicating the \code{dpi} argument
#'                    (default is \code{600}) in the \code{ggsave} function.
#' @param write       a character string naming a file for writing the output
#'                    into either a text file with file extension \code{".txt"}
#'                    (e.g., \code{"Output.txt"}) or Excel file with file
#'                    extension \code{".xlsx"}  (e.g., \code{"Output.xlsx"}).
#'                    If the file name does not contain any file extension,
#'                    an Excel file will be written.
#' @param append      logical: if \code{TRUE} (default), output will be
#'                    appended to an existing text file with extension
#'                    \code{.txt} specified in \code{write}, if \code{FALSE}
#'                    existing text file will be overwritten.
#' @param check       logical: if \code{TRUE} (default), argument specification
#'                    is checked.
#' @param output      logical: if \code{TRUE} (default), output is shown on
#'                    the console.
#'
#' @details
#' The Wald confidence interval which is based on the normal approximation to the
#' binomial distribution are computed by specifying \code{method = "wald"}, while
#' the Wilson (1927) confidence interval (aka Wilson score interval) is requested
#' by specifying \code{method = "wilson"}. By default, Wilson confidence interval
#' is computed which have been shown to be reliable in small samples of n = 40 or
#' less, and larger samples of n > 40 (Brown, Cai & DasGupta, 2001), while the
#' Wald confidence intervals is inadequate in small samples and when \emph{p} is
#' near 0 or 1 (Agresti & Coull, 1998).
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{ci.prop}}, \code{\link{ci.prop.diff}}, \code{\link{ci.median}},
#' \code{\link{ci.prop.diff}}, \code{\link{ci.cor}}, \code{\link{ci.var}},
#' \code{\link{ci.sd}}, \code{\link{descript}}
#'
#' @references
#' Agresti, A. & Coull, B.A. (1998). Approximate is better than "exact" for
#' interval estimation of binomial proportions. \emph{American Statistician, 52},
#' 119-126.
#'
#' Brown, L. D., Cai, T. T., & DasGupta, A., (2001). Interval estimation for a
#' binomial proportion. \emph{Statistical Science, 16}, 101-133.
#'
#' Canty, A., & Ripley, B. (2024). \emph{boot: Bootstrap R (S-Plus) Functions}.
#' R package version 1.3-31.
#'
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology
#' - Using R and SPSS}. John Wiley & Sons.
#'
#' Wilson, E. B. (1927). Probable inference, the law of succession, and statistical
#' inference. \emph{Journal of the American Statistical Association, 22}, 209-212.
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#'
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{data}}{list with the input specified in \code{...}, \code{data}, \code{group}, and \code{split}}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{boot}}{data frame with bootstrap replicates of the proportions when bootstrapping was requested}
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
#' # Confidence Interval (CI) for Proportions
#'
#' # Example 1a: Two-Sided 95% CI
#' ci.prop(mtcars, vs, am)
#'
#' # Example 1b: Alternative specification without using the '...' argument
#' ci.prop(mtcars[, c("vs", "am")])
#'
#' # Example 1c: One-Sided 95% CI using Wald method
#' ci.prop(mtcars, vs, am, method = "wald", alternative = "less")

#' #————————————————————————————————————————————————————————————————————————————
#' # Grouping Variable
#'
#' # Example 2a: Grouping variable
#' ci.prop(mtcars, vs, group = "am")
#'
#' # Example 2b: Alternative specification without using the '...' argument
#' ci.prop(mtcars$vs, group = mtcars$am)
#'
#' # Example 2c: Sort result table by variables
#' ci.prop(mtcars, vs, am, group = "gear", sort.var = TRUE)
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Split Variable
#'
#' # Example 3a: Split variable
#' ci.prop(mtcars, vs, split = "am")
#'
#' # Example 3b: Alternative specification without using the '...' argument
#' ci.prop(mtcars$vs, split = mtcars$am)
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Grouping and Split Variable
#'
#' # Example 4a: Grouping and split variable
#' ci.prop(mtcars, vs, group = "am", split = "cyl")
#'
#' # Example 4b: Alternative specification without using the '...' argument
#' ci.prop(mtcars$vs, group = mtcars$am, split = mtcars$cyl)
#'
#' # Example 4c: Sort result tables by variables
#' ci.prop(mtcars, vs, am, group = "gear", split = "cyl", sort.var = TRUE)
#'
#' \dontrun{
#' #————————————————————————————————————————————————————————————————————————————
#' # Bootstrap Confidence Interval (CI)
#'
#' # Example 5a: Bias-corrected (BC) percentile bootstrap CI
#' ci.prop(mtcars, vs, am, boot = "bc")
#'
#' # Example 5b: Bias-corrected and accelerated (BCa) bootstrap CI,
#' # 5000 bootstrap replications, set seed of the pseudo-random number generator
#' ci.prop(mtcars, vs, am, boot = "bca", nrep = 5000, seed = 42)
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Plot Confidence Intervals
#'
#' # Example 6a: Two-Sided 95% CI
#' ci.prop(mtcars, vs, am, plot = "ci")
#'
#' # Example 6b: Grouping variable
#' ci.prop(mtcars, vs, am, group = "am", plot = "ci")
#'
#' # Example 6c: Split variable
#' ci.prop(mtcars, vs, am, split = "am", plot = "ci")
#'
#' # Example 6d: Plot results using the plot() function, use additional arguments
#' # see Details in the help page of the function plot.misty.object
#' object <- ci.prop(mtcars, vs, am, plot = "ci")
#' plot(object, ybreaks = seq(0, 1, by = 0.1), title = "Confidence Intervals")
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Plot Bootstrap Samples
#'
#' # Example 7a: Two-Sided 95% CI
#' ci.prop(mtcars, vs, am, boot = "bc", plot = "boot")
#'
#' # Example 7b: Grouping variable
#' ci.prop(mtcars, vs, am, group = "am", boot = "bc", plot = "boot")
#'
#' # Example 7c: Split variable
#' ci.prop(mtcars, vs, am, split = "am", boot = "bc", plot = "boot")
#'
#' # Example 7d: Plot results using the plot() function, use additional arguments
#' # see Details in the help page of the function plot.misty.object
#' object <- ci.prop(mtcars, vs, am, boot = "bc", plot = "boot")
#' plot(object, fill = "gray30", title = "Bootstrap Samples")
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Write Results and Save Plot
#'
#' # Example 8a: Write results into a text file
#' ci.prop(mtcars, vs, am, write = "CI_Prop.txt")
#'
#' # Example 8b: Write results into an Excel file
#' ci.prop(mtcars, vs, am, write = "CI_Prop.xlsx")
#'
#' # Example 8c: Save plot as PNG file
#' ci.prop(mtcars, vs, am, plot = "ci", filename = "CI_Prop.png",
#'         width = 9, height = 6)
#' }
ci.prop <- function(data, ..., method = c("wald", "wilson"),
                    boot = c("none", "perc", "bc", "bca"), nrep = 1000, seed = NULL,
                    alternative = c("two.sided", "less", "greater"),
                    conf.level = 0.95, group = NULL, split = NULL, sort.var = FALSE,
                    na.omit = FALSE, digits = 3, as.na = NULL, plot = c("none", "ci", "boot"),
                    geom = c("all", "hist", "density", "point", "ci", "line"),
                    filename = NULL, width = NA, height = NA, dpi = 600,
                    write = NULL, append = TRUE, check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check ——————————————————————————————————————————————————————————————

  # Check if input 'data' is missing or NULL
  if (isTRUE(missing(data) || is.null(data))) { stop("Please specify a numeric vector or data frame for the argument 'data'", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data ———————————————————————————————————————————————————————————————————————

  .data(data, ..., group = group, split = split) |> (\(p) {

    # Data
    assign("x", p$data, envir = parent.frame())

    # Grouping variable
    if (isTRUE(!is.null(p$group))) { assign("group", p$group, envir = parent.frame()) }

    # Split variable
    if (isTRUE(!is.null(p$split))) { assign("split", p$split, envir = parent.frame()) }

  })()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Non-Numeric Variables ####

  x <- .exclude.non.numeric(x)

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
  ## Convert User-Missing Values into NA ####

  if (isTRUE(!is.null(as.na))) { x <- .as.na(x, na = as.na) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Listwise Deletion ####

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
  # Input Check ————————————————————————————————————————————————————————————————

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Check Inputs ####

  .check.input(logical = c("sort.var", "na.omit", "append", "output"), numeric = list(seed = 1L, width = 1L, height = 1L, dpi = 1L),
               s.character = list(method = c("wald", "wilson"), boot = c("none", "perc", "bc", "bca"), plot = c("none", "ci", "boot")),
               m.character = list(geom = c("all", "hist", "density", "point", "ci", "line")), args = c("alternative", "conf.level", "nrep", "digits", "write2"), envir = environment(), input.check = check)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Additional Checks ####

  if (isTRUE(check)) {

    #—————————————————————————————————————— #
    ### Check Input 'prop' ####

    if (isTRUE(!all(unlist(x) %in% c(0L, 1L, NA)))) { stop("Please specify a numeric vector or data frame with numeric variables with 0 and 1 values for the argument 'data'.", call. = FALSE) }

    #—————————————————————————————————————— #
    ### Check Input 'group' ####

    # Check input 'group'
    if (isTRUE(!is.null(group))) {

      # Input 'group' completely missing
      if (isTRUE(all(is.na(group)))) { stop("The grouping variable specified in 'group' is completely missing.", call. = FALSE) }

      # Only one group in 'group'
      if (isTRUE(length(na.omit(unique(group))) == 1L)) { warning("There is only one group represented in the grouping variable specified in 'group'.", call. = FALSE) }

    }

    #—————————————————————————————————————— #
    ### Check Input 'split' ####

    if (isTRUE(!is.null(split))) {

      # Input 'split' completely missing
      if (isTRUE(all(is.na(split)))) { stop("The split variable specified in 'split' is completely missing.", call. = FALSE) }

      # Only one group in 'split'
      if (isTRUE(length(na.omit(unique(split))) == 1L)) { warning("There is only one group represented in the split variable specified in 'split'.", call. = FALSE) }

    }

    #—————————————————————————————————————— #
    ### Check Input 'plot' ####

    if (isTRUE(all(plot == "boot") && (all(boot == "none") || all(c("none", "norm", "basic", "stud", "perc", "bc", "bca") %in% boot)))) { stop("Please request bootstrap confidence intervals by specifying the 'boot' argument to plot bootstrap samples.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Arguments ——————————————————————————————————————————————————————————————————

  #—————————————————————————————————————— #
  ### 'method' Argument ####

  if (isTRUE(all(c("wald", "wilson") %in% method))) { method <- "wilson" }

  #—————————————————————————————————————— #
  ### 'boot' Argument ####

  boot <- ifelse(all(c("none", "perc", "bc", "bca") %in% boot), "none", boot)

  #—————————————————————————————————————— #
  ### 'alternative' Argument ####

  if (isTRUE(all(c("two.sided", "less", "greater") %in% alternative))) { alternative <- "two.sided" }

  #—————————————————————————————————————— #
  ### 'plot' Argument ####

  plot <- ifelse(all(c("none", "ci", "boot") %in% plot), "none", plot)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'geom' Argument ####

  if (isTRUE(all(c("all", "bar", "point", "ci", "line", "jitter") %in% geom))) {

    geom <- c("bar", "ci", "line")

  } else if (all(geom == "all")) {

    geom <- c("bar", "point", "ci", "line", "jitter")

  }

  #_____________________________________________________________________________
  #
  # Main Function ——————————————————————————————————————————————————————————————

  boot.sample <- NULL

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## No Grouping, No Split ####

  if (isTRUE(is.null(group) && is.null(split))) {

    #—————————————————————————————————————— #
    ### No Bootstrapping ####

    if (isTRUE(boot == "none")) {

    result <- data.frame(variable = colnames(x),
                         n = vapply(x, function(y) length(na.omit(y)), FUN.VALUE = 1L),
                         nNA = vapply(x, function(y) sum(is.na(y)), FUN.VALUE = 1L),
                         pNA = vapply(x, function(y) sum(is.na(y)) / length(y) * 100L, FUN.VALUE = double(1L)),
                         # Absolute Frequency
                         freq = vapply(x, function(y) sum(y == 1, na.rm = TRUE), FUN.VALUE = 1L),
                         # Proportion
                         prop = vapply(x, function(y) ifelse(length(na.omit(y)) <= 1L, NA, mean(y, na.rm = TRUE)), FUN.VALUE = double(1L)),
                         # Confidence interval for proportions
                         low = vapply(x, .prop.conf, method = method, alternative = alternative, conf.level = conf.level, side = "low", FUN.VALUE = double(1L)),
                         upp = vapply(x, .prop.conf, method = method, alternative = alternative, conf.level = conf.level, side = "upp", FUN.VALUE = double(1L)),
                         row.names = NULL, check.names = FALSE)

    #—————————————————————————————————————— #
    ### Bootstrapping ####

    } else {

      result.boot <- apply(matrix(seq_len(ncol(x)), nrow = 1L), 2L, function(y) x |>
                       (\(z) suppressWarnings(.ci.boot(data = x[, y], statistic = .boot.func.mean, boot = boot, nrep = nrep, alternative = alternative, conf.level = conf.level, seed = seed)))() |>
                       (\(w) list(t = data.frame(variable = colnames(x)[y], prop = w$t), result = data.frame(variable = colnames(x)[y], n = w$n, nNA = w$nNA, pNA = w$pNA, freq= w$freq, prop = w$t0, low = w$ci[1L], upp = w$ci[2L], row.names = NULL)))())

      boot.sample <- do.call("rbind", lapply(result.boot, function(y) y$t))
      result <- do.call("rbind", lapply(result.boot, function(y) y$result))

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Grouping, No Split ####

  } else if (isTRUE(!is.null(group) && is.null(split))) {

    #—————————————————————————————————————— #
    ### No Bootstrapping ####

    if (isTRUE(boot == "none")) {

      result <- lapply(split(x, f = group), function(y) misty::ci.prop(y, group = NULL, split = NULL, method = method, alternative = alternative, conf.level = conf.level, sort.var = sort.var, digits = digits, check = FALSE, output = FALSE)$result) |>
        (\(y) data.frame(group = rep(names(y), each = ncol(x)), eval(parse(text = paste0("rbind(", paste0("y[[", seq_len(length(y)), "]]", collapse = ", "), ")")))))() |> (\(p) if (isTRUE(sort.var)) { data.frame(p[order(p[, "variable"]), ], row.names = NULL) } else { return(p) })()

    #—————————————————————————————————————— #
    ### Bootstrapping ####

    } else {

      result.boot <- lapply(split(x, f = group), function(y) misty::ci.prop(y, group = NULL, split = NULL, seed = seed, boot = boot, nrep = nrep, alternative = alternative, conf.level = conf.level, check = FALSE, output = FALSE)) |>
        (\(z) list(boot.sample = data.frame(group = rep(names(z), each = nrep*unique(unlist(lapply(z, function(q) nrow(q$result))))), do.call("rbind", lapply(z, function(w) w$boot)), row.names = NULL), result = data.frame(group = rep(names(z), each = unique(unlist(lapply(z, function(q) nrow(q$result))))), do.call("rbind", lapply(z, function(w) w$result)), row.names = NULL)))()

      boot.sample <- result.boot$boot.sample
      result <- result.boot$result |> (\(p) if (isTRUE(sort.var)) { data.frame(p[order(p[, "variable"]), ], row.names = NULL) } else { return(p) })()

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## No Grouping, Split ####

  } else if (isTRUE(is.null(group) && !is.null(split))) {

    #—————————————————————————————————————— #
    ### No Bootstrapping ####

    if (isTRUE(boot == "none")) {

      result <- lapply(split(data.frame(x), f = split), function(y) misty::ci.prop(y, group = NULL, split = NULL, method = method, alternative = alternative, conf.level = conf.level, sort.var = sort.var, digits = digits, check = FALSE, output = FALSE)$result)

    #—————————————————————————————————————— #
    ### Bootstrapping ####

    } else {

      result.boot <- lapply(split(x, f = split), function(y) misty::ci.prop(y, group = NULL, split = NULL, seed = seed, boot = boot, nrep = nrep, alternative = alternative, conf.level = conf.level, check = FALSE, output = FALSE)) |>
        (\(z) list(boot.sample = data.frame(split = rep(names(z), each = nrep*unique(unlist(lapply(z, function(q) nrow(q$result))))), do.call("rbind", lapply(z, function(w) w$boot)), row.names = NULL), result = lapply(z, function(w) w$result)))()

      boot.sample <- result.boot$boot.sample
      result <- result.boot$result

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Grouping, Split ####

  } else if (isTRUE(!is.null(group) && !is.null(split))) {

    #—————————————————————————————————————— #
    ### No Bootstrapping ####

    if (isTRUE(boot == "none")) {

    result <- lapply(split(data.frame(x, group = group), f = split), function(y) misty::ci.prop(y[, -grep("group", names(y))], group = y$group, split = NULL, method = method, alternative = alternative, conf.level = conf.level, sort.var = sort.var, digits = digits,  check = FALSE, output = FALSE)$result) |> (\(p) if (isTRUE(sort.var)) { lapply(p, function(y) data.frame(y[order(y[, "variable"]), ], row.names = NULL)) } else { return(p) })()

    #—————————————————————————————————————— #
    ### Bootstrapping ####

    } else {

      result.boot <- lapply(split(data.frame(x, group = group), f = split), function(y) misty::ci.prop(y[, -grep("group", names(y))], group = y$group, split = NULL, seed = seed, boot = boot, nrep = nrep, alternative = alternative, conf.level = conf.level, check = FALSE, output = FALSE)) |>
        (\(z) list(boot.sample = data.frame(split = rep(names(z), each = nrep*unique(unlist(lapply(z, function(q) nrow(q$result))))), do.call("rbind", lapply(z, function(w) w$boot)), row.names = NULL), result = lapply(z, function(w) w$result)))()

      boot.sample <- result.boot$boot.sample
      result <- result.boot$result  |> (\(p) if (isTRUE(sort.var)) { lapply(p, function(y) data.frame(y[order(y[, "variable"]), ], row.names = NULL)) } else { return(p) })()

    }

  }

  #_____________________________________________________________________________
  #
  # Return Object ——————————————————————————————————————————————————————————————

  object <- list(call = match.call(),
                 type = "ci.prop",
                 data = list(x = x, group = group, split = split),
                 args = list(method = method, boot = boot, nrep = nrep, seed = seed, alternative = alternative, conf.level = conf.level, sort.var = sort.var, na.omit = na.omit, digits = digits, as.na = as.na, plot = plot, geom = geom, filename = filename, width = width, height = height, dpi = dpi, write = write, append = append, check = check, output = output),
                 boot = boot.sample,
                 plot = NULL,
                 result = result)

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Plot and Save Plot —————————————————————————————————————————————————————————

  if (isTRUE(plot != "none")) { object$plot <- plot(object, filename = filename, width = width, height = height, dpi = dpi, check = FALSE) |> (\(y) suppressMessages(suppressWarnings(print(y))))() }

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
