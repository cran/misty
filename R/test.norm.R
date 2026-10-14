#' Tests for Univariate and Multivariate Normality
#'
#' This function performs the Kolmogorov-Smirnov-Lilliefors, the Shapiro-Wilk test,
#' the Anderson–Darling test, and the Cramer-von Mises test for testing for testing
#' univariate normality (null hypothesis) against univariate non-normality (alternative
#' hypothesis), and the Henze-Zirkler test, Henze-Wagner test, and the Royston test
#' for testing multivariate normality (null hypothesis) against multivariate non-normality
#' (alternative hypothesis). By default, the function performs the Shapiro-Wilk
#' test for testing univariate normality, and Henze-Zirkler test for testing multivariate
#' normality. Note that the function also tests normality of residuals of a linear
#' model when specifying an object of class \code{"lm"} for the argument \code{data}.
#'
#' @param data         a fitted model of class \code{lm}, a numeric vector, or
#'                     data frame.
#' @param ...          an expression indicating the variable names in \code{data},
#'                     e.g., \code{test.norm(dat, x1, x2)}. Note that the operators
#'                     \code{+}, \code{-}, \code{~}, \code{:}, \code{::}, and \code{!}
#'                     can also be used to select variables, see 'Details' in the
#'                     \code{\link{df.subset}} function.
#' @param print        a character vector indicating which results to show on the
#'                     console, i.e. \code{"all"} for tests for multivariate and
#'                     univariate normality, \code{"multi"} (default) for a test
#'                     for multivariate normality, and \code{"uni"} for tests
#'                     for univariate normality. Note that this argument is only
#'                     used when specifying more than one variable for the analysis.
#' @param method.uni   a character string indicating the method used for testing
#'                     univariate normality, i.e., \code{"lillie"} for the
#'                     Kolmogorov-Smirnov-Lilliefors test (Lilliefors, 1967),
#'                     \code{"sw"} (default) for the Shapiro-Wilk test (Shapiro
#'                     & Wilk, 1965), \code{"ad"} for the Anderson–Darling test
#'                     (Anderson & Darling, 1954), and \code{"cm"} for the
#'                     Cramer-von Mises test (Stephens, 1986).
#' @param method.multi a character string indicating the method used for testing
#'                     multivariate normality, i.e., \code{"hz"} (default) for
#'                     the Henze-Zirkler test (Henze & Zirkler, 1990), \code{"hw"}
#'                     for the Henze-Wagner test (Henze & Wagner, 1997), and
#'                     \code{"royston"} for the Royston test (Royston, 1983).
#' @param p.adj        a character string indicating an adjustment method for multiple
#'                     testing based on \code{\link{p.adjust}}, i.e., \code{none}
#'                     (default), \code{bonferroni}, \code{holm}, \code{hochberg},
#'                     \code{hommel}, \code{BH}, \code{BY}, or \code{fdr}.
#' @param digits       an integer value indicating the number of decimal places to
#'                     be used for displaying results.
#' @param p.digits     an integer value indicating the number of decimal places to
#'                     be used for displaying the \emph{p}-value.
#' @param as.na        a numeric vector indicating user-defined missing values,
#'                     i.e. these values are converted to \code{NA} before
#'                     conducting the analysis.
#' @param plot         logical: if \code{TRUE}, a plot showing univariate or
#'                     multivariate Mahalanobis Q-Q plot is drawn.
#'                     For additional plotting arguments, see \code{Details} in
#'                     the help page of the function \code{plot.misty.object}.
#' @param geom         a character vector indicating which plot to draw, i.e.,
#'                     \code{"all"} for plotting a Q-Q plot and histogram, \code{"qq"}
#'                     for plotting Q-Q plots, and \code{"hist"} for plotting
#'                     histograms. Note that \code{"all"} and \code{c("qq", "hist")}
#'                     are valid only when specifying a fitted model of class \code{lm}
#'                     or a single variable. When specifying \code{"all"} or
#'                     \code{c("qq", "hist")} given multiple variables, a multivariate
#'                     Mahalanobis Q-Q plot is drawn.
#' @param impute       logical: if \code{TRUE}, missing values are imputed when
#'                     plotting multiple variables \code{plot = TRUE} using the
#'                     \code{mice} function in the R package \pkg{mice} using the
#'                     predictive mean matching method. Note that listwise deletion
#'                     is always used for tests for univariate and multivariate
#'                     normality.
#' @param filename     a character string indicating the \code{filename}
#'                     argument including the file extension in the \code{ggsave}
#'                     function. Note that one of \code{".eps"}, \code{".ps"},
#'                     \code{".tex"}, \code{".pdf"} (default),
#'                     \code{".jpeg"}, \code{".tiff"}, \code{".png"},
#'                     \code{".bmp"}, \code{".svg"} or \code{".wmf"} needs
#'                     to be specified as file extension in the \code{file}
#'                     argument. Note that plots can only be saved when
#'                     \code{plot = TRUE}.
#' @param width        a numeric value indicating the \code{width} argument
#'                     (default is the size of the current graphics device)
#'                     for the \code{ggsave} function.
#' @param height       a numeric value indicating the \code{height} argument
#'                     (default is the size of the current graphics device)
#'                     for the \code{ggsave} function.
#' @param dpi          a numeric value indicating the \code{dpi} argument
#'                     (default is \code{600}) for the \code{ggsave} function.
#' @param write         a character string naming a text file with file extension
#'                     \code{".txt"} (e.g., \code{"Output.txt"}) for writing the
#'                     output into a text file.
#' @param append       logical: if \code{TRUE} (default), output will be appended
#'                     to an existing text file with extension \code{.txt} specified
#'                     in \code{write}, if \code{FALSE} existing text file will
#'                     be overwritten.
#' @param check        logical: if \code{TRUE} (default), argument specification
#'                     is checked.
#' @param output       logical: if \code{TRUE} (default), output is shown on the
#'                     console.
#'
#' @author
#' Takuya Yanagida
#'
#' @seealso
#' \code{\link{skewness}}, \code{\link{kurtosis}}, \code{\link{check.resid}},
#' \code{\link{test.linear}}, \code{\link{test.homo}}, \code{\link{test.levene}}
#'
#' @references
#' Anderson, T. W., & Darling, D. A. (1954). A test of goodness of fit.
#' \emph{Journal of the American Statistical Association, 49}(268), 765-769.
#' https://doi.org/10.1080/01621459.1954.10501232
#'
#' Gross J, & Ligges U (2015). \emph{nortest: Tests for Normality}.
#' R package version 1.0-4, https://doi.org/10.32614/CRAN.package.nortest
#'
#' Henze, N., & Zirkler, B. (1990). A class of invariant consistent tests for
#' multivariate normality. \emph{Communications in Statistics—Theory and Methods, 19}(10),
#' 3595-3617. https://doi.org/10.1080/03610929008830400
#'
#' Henze, N., & Wagner, T. (1997). A new approach to the BHEP tests for multivariate
#' normality. \emph{Journal of Multivariate Analysis, 62}(1), 1-23
#'
#' Korkmaz S, Goksuluk D, & Zararsiz G. (2014) MVN: An R Package for Assessing
#' Multivariate Normality. \emph{The R Journal, 6}(2), 151-162.
#'
#' Lilliefors, H. W. (1967). On the Kolmogorov-Smirnov test for normality with mean
#' and variance unknown. \emph{Journal of the American Statistical Association, 62}(318),
#' 399-402. https://doi.org/10.1080/01621459.1967.10482916
#'
#' Royston, J.P. (1983). Some techniques for assessing multivariate normality based
#' on the Shapiro-Wilk W. \emph{Journal of the Royal Statistical Society. Series C
#' (Applied Statistics), 32}, 121-133. https://doi.org/10.2307/2347291
#'
#' Shapiro, S. S., & Wilk, M. B. (1965). An analysis of variance test for normality
#' (complete samples). \emph{Biometrika, 52}(3–4), 591–611. https://doi.org/10.1093/biomet/52.3-4.591
#'
#' Stephens, M. A. (1986). Tests based on EDF statistics. In R. B. D'Agostino &
#' M. A. Stephens (Eds.), \emph{Goodness-of-Fit Techniques} (pp. 97.193). Marcel
#' Dekker.
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{data}}{numeric vector or data frame}
#' \item{\code{plot}}{ggplot2 object for plotting the results}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{result}}{list with result tables, i.e., \code{uni} for
#'                      test(s) for univariate normality and \code{multi}
#'                      for test for multivariate normality}
#'
#' @note
#' This function is based on the \code{hz()}, \code{hw()}, and \code{royston()}
#' function in the \pkg{MVN} package by Selcuk Korkmaz, Dincer Goksuluk and Gokmen
#' Zararsiz, and the \code{ad.test()}, \code{lillie.test()}, and \code{cvm.test()}
#' function in the \pkg{nortest} package by Juergen Gross and Uwe Ligges.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #————————————————————————————————————————————————————————————————————————————
#' # Fitted Model of Class "lm"
#'
#' # Estimate linear model
#' mod <- lm(mpg ~ cyl + disp, data = mtcars)
#'
#' # Example 1a: Shapiro-Wilk test for normality of residuals
#' test.norm(mod)
#'
#' # Example 1b: Lilliefors (Kolomorov-Smirnov) test for normality of residuals
#' test.norm(mod, method.uni = "lillie")
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # One Variable
#'
#' # Example 2: Shapiro-Wilk test for normality
#' test.norm(mtcars, mpg)
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Multiple Variables
#'
#' # Example 3a: Henze-Zirkler test for multivariate normality
#' test.norm(mtcars, hp, drat, wt, qsec)
#'
#' # Example 3b: Henze-Wagner test for multivariate normality
#' test.norm(mtcars, hp, drat, wt, qsec, method.multi = "hw")
#'
#' # Example 3c: Multiple Shapiro-Wilk tests for normality
#' test.norm(mtcars, hp, drat, wt, qsec, print = "uni")
#'
#' # Example 3c: Henze-Wagner test for multivariate normality
#' #             Multiple Shapiro-Wilk tests for normality
#' test.norm(mtcars, hp, drat, wt, qsec, print = "all")
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Plot
#'
#' # Example 4a: Q-Q plot and histogram
#' test.norm(mtcars, mpg, plot = TRUE)
#'
#' # Example 4b: Multivariate Mahalanobis Q-Q plot
#' test.norm(mtcars, hp, drat, wt, qsec, plot = TRUE)
#'
#' # Example 4c: Multiple Q-Q plots
#' test.norm(mtcars, hp, drat, wt, qsec, print = "uni", plot = TRUE)
#'
#' # Example 4d: Multiple histograms
#' test.norm(mtcars, hp, drat, wt, qsec, print = "uni", plot = TRUE, geom = "hist")
#'
#' # Example 4e: Multivariate Mahalanobis Q-Q plot, impute data for plotting
#' test.norm(airquality, plot = TRUE, impute = TRUE)
#'
#' # Example 4f: Plot results using the plot() function, use additional arguments
#' # see Details in the help page of the function plot.misty.object
#' object <-  test.norm(mtcars, mpg)
#' plot(object, point.shape = 2, ribbon.fill = "red", xlim = c(0, 40))
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Write Results and Save Plot
#'
#' # Example 5a: Write results into a text file
#' test.norm(mtcars, hp, drat, wt, qsec, write = "Multinorm.txt")
#'
#' # Example 5b: Write results into an Excel file
#' test.norm(mtcars, hp, drat, wt, qsec, write = "Multinorm.xlsx")
#'
#' # Example 5c: Save plot as PNG fine
#' test.norm(mtcars, mpg, plot = TRUE, filename = "Multinorm.png",
#'           width = 13, height = 6)
#' }
test.norm <- function(data, ..., print = c("all", "multi", "uni"),
                      method.uni = c("lillie", "sw", "ad", "cm"),
                      method.multi = c("hz", "hw", "royston"), p.adj = "none",
                      digits = 2, p.digits = 3, as.na = NULL, plot = FALSE,
                      geom = c("all", "qq", "hist"), impute = FALSE, filename = NULL,
                      width = NA, height = NA, dpi = 600, write = NULL, append = TRUE,
                      check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check ——————————————————————————————————————————————————————————————

  # Check if input 'data' is missing or NULL
  if (isTRUE(missing(data) || is.null(data))) { stop("Please specify an object of class \"lm\", numeric vector, or data frame for the argument 'data'", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data ———————————————————————————————————————————————————————————————————————

  # Object of class 'lm'
  if (isTRUE(inherits(data, what = "lm"))) {

    x <- data$residuals

  # Numeric vector or data frame
  } else {

    .data(data, ..., drop = TRUE) |> (\(p) { assign("x", p$data, envir = parent.frame()) })()

  }

  #_____________________________________________________________________________
  #
  # Input Check ————————————————————————————————————————————————————————————————

  .check.input(logical = c("plot", "impute", "append", "output"), s.character = list(print = c("all", "uni", "multi"), method.uni = c("lillie", "sw", "ad", "cm"), method.multi = c("hz", "hw", "royston")), m.character = list(geom = c("all", "qq", "hist")),
               args = c("p.adjust", "digits", "p.digits", "write2"), envir = environment(), input.check = check)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Exclude Non-Numeric Variables ####

  x <- .exclude.non.numeric(x)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert User-missing Values into NA ####

  if (isTRUE(!is.null(as.na))) { x <- .as.na(x, na = as.na) }

  #_____________________________________________________________________________
  #
  # Arguments ——————————————————————————————————————————————————————————————————

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'print' Argument ####

  if (isTRUE(all(c("all", "multi", "uni") %in% print))) {

    print <- ifelse(isTRUE(is.null(dim(x))), "uni", "multi")

  } else if (isTRUE(all(print == "all"))) {

    print <- c("uni", "multi")

  }

  c("all", "qq", "hist")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'geom' Argument ####

  if (isTRUE(all(c("all", "qq", "hist") %in% geom) || all(print == "all"))) { geom <- c("qq", "hist") }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'method.uni' Argument ####

  if (isTRUE(all(c("lillie", "sw", "ad", "cm") %in% method.uni))) { method.uni <- "sw" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'method.multi' Argument ####

  if (isTRUE(all(c("hz", "hw", "royston") %in% method.multi))) { method.multi <- "hz" }

  #_____________________________________________________________________________
  #
  # Main Function ——————————————————————————————————————————————————————————————

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Univariate Normality ####

  restab.uni <- restab.multi <- NULL
  if (isTRUE(is.null(dim(x)))) {

    restab.uni <- switch(method.uni, "lillie" = {

      .test.lillie(x)

    }, "sw" = {

      if (isTRUE(length(x) < 4L || length(x) > 5000)) { stop("The Shapiro-Wilk test requires a sample size between 4 and 5000.", call. = FALSE) }

      shapiro.test(x) |> (\(p) data.frame(stat = unname(p$statistic), pval = p$p.value))()

    }, "ad" = {

      .test.ad(x)

    }, "cm" = {

      .test.cm(x)

    })

    # Descriptive statistics
    restab.uni <- data.frame(descript(x, output = FALSE)$result[, c("n", "nNA", "skew", "kurt")], restab.uni)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Multivariate Normality #####

  } else {

    #—————————————————————————————————————— #
    ### Test for Multivariate Normality ####

    if (isTRUE("multi" %in% print)) {

      restab.multi <- switch(method.multi, "hz" = {

        .test.hz(x)

      }, "hw" = {

        .test.hw(x)

      }, "royston" = {

        .test.royston(x)

      })

      # Descriptive statistics
      restab.multi <- data.frame(n = sum(complete.cases(x)), nNA = sum(!complete.cases(x)), skew = suppressWarnings(misty::skewness(x, sample = FALSE, output = FALSE)$result$skew), kurt = suppressWarnings(misty::kurtosis(x, sample = FALSE, output = FALSE)$result$kurt), restab.multi)

    }

    #—————————————————————————————————————— #
    ### Tests for Univariate Normality ####

    if (isTRUE("uni" %in% print)) {

      # Variable names
      y.var <- colnames(x)

      # Default S3 method
      restab.uni <- data.frame(variable = y.var, do.call("rbind", lapply(y.var, function(y) misty::test.norm(x[, y], method.uni = method.uni, check = FALSE, output = FALSE)$result$uni)))

      # Multiple-testing adjustment
      if (isTRUE(p.adj != "none")) { restab.uni$pval <- p.adjust(restab.uni$pval, method = p.adj) }

    }

  }

  #_____________________________________________________________________________
  #
  # Return Object ——————————————————————————————————————————————————————————————

  object <- list(call = match.call(),
                 type = "test.norm",
                 data = x,
                 plot = NULL,
                 args = list(print = print, method.uni = method.uni, method.multi = method.multi, p.adj = p.adj, digits = digits, p.digits = p.digits, as.na = as.na, plot = plot, geom = geom, impute = impute, filename = filename, width = width, height = height, dpi = dpi, write = write, append = append, check = check, output = output),
                 result = list(uni = restab.uni, multi = restab.multi))

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  ## Plot and Save Plot ————————————————————————————————————————————————————————

  if (isTRUE(plot)) { object$plot <- plot(object, filename = filename, width = width, height = height, dpi = dpi, check = FALSE) |> (\(p) suppressMessages(suppressWarnings(print(p))))() }

  #_____________________________________________________________________________
  #
  ## Write Results —————————————————————————————————————————————————————————————

  if (isTRUE(!is.null(write))) { .write.result(object = object, write = write, append = append) }

  #_____________________________________________________________________________
  #
  ## Output ————————————————————————————————————————————————————————————————————

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}

#_______________________________________________________________________________
