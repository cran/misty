#' Tests for Linearity in a Linear Model
#'
#' This function performs the regression equation specification error (RESET)
#' test (Remsey, 1969), the Harvey-Collier test (Harvey & Collier, 1977), and
#' the rainbow test for linearity (Utts, 1982). By default, the function performs
#' the RESET test.
#'
#' @param model       a fitted model of class \code{lm}.
#' @param method      a character string indicating the method used for testing
#'                    linearity, i.e., \code{"reset"} (default) for the regression
#'                    equation specification error (RESET) test, \code{"harv"}
#'                    for the Harvey-Collier test, and \code{"rain"} for the
#'                    rainbow test for linearity.
#' @param power       a positive integer vector indicating the powers of the
#'                    fitted values that should be included in the auxiliary
#'                    regression model. By default, the test includes quadratic
#'                    and cubic terms in the model, i.e., \code{c(2, 3)}. Note
#'                    that this argument is only used for the RESET test.
#' @param frac        a numeric value between 0 and 1 indicating the proportion
#'                    of observations used in the central subset model. Note that
#'                    this argument is only used for the rainbow test.
#' @param order       a character string indicated how the observations are ordered,
#'                    i.e., \code{"none"} for observations assumed to be already
#'                    ordered, \code{"all"} for ordering each predictor in the model
#'                    specified in the \code{"model"} argument separately,
#'                    \code{"mahalanobis"} for ordering observations by their
#'                    Mahalanobis distances from the mean regressor. Note that
#'                    this argument is only used for the Harvey-Collier test and
#'                    the rainbow test.
#' @param p.adj       a character string indicating an adjustment method for multiple
#'                    testing based on \code{\link{p.adjust}}, i.e., \code{none}
#'                    (default), \code{bonferroni}, \code{holm}, \code{hochberg},
#'                    \code{hommel}, \code{BH}, \code{BY}, or \code{fdr}.
#' @param digits      an integer value indicating the number of decimal places to
#'                    be used for displaying results.
#' @param p.digits    an integer value indicating the number of decimal places to
#'                    be used for displaying the \emph{p}-value.
#' @param plot        logical: if \code{TRUE}, partial residual plots or
#'                    component-plus-residual plots are drawn. For additional
#'                    plotting arguments, see \code{Details} in the help page of
#'                    the function \code{plot.misty.object}.
#' @param geom        character vector indicating which plot elements to draw,
#'                    i.e., \code{"all"} (default) for drawing all plot elements,
#'                    \code{"none"} for drawing no regression or smoothing line,
#'                    \code{"lm"} for drawing the regression line, \code{"loess"}
#'                    for drawing the LOESS smoothing curve, \code{"ci.lm"} for
#'                    drawing confidence bands around the regression line,
#'                    \code{"ci.loess"} for drawing confidence bands around the
#'                    LOESS smoothing curve (i.e., \code{se = TRUE}).
#' @param filename    a character string indicating the \code{filename}
#'                    argument including the file extension in the \code{ggsave}
#'                    function. Note that one of \code{".eps"}, \code{".ps"},
#'                    \code{".tex"}, \code{".pdf"} (default),
#'                    \code{".jpeg"}, \code{".tiff"}, \code{".png"},
#'                    \code{".bmp"}, \code{".svg"} or \code{".wmf"} needs
#'                    to be specified as file extension in the \code{file}
#'                    argument. Note that plots can only be saved when
#'                    \code{plot = TRUE}.
#' @param width       a numeric value indicating the \code{width} argument
#'                    (default is the size of the current graphics device)
#'                    for the \code{ggsave} function.
#' @param height      a numeric value indicating the \code{height} argument
#'                    (default is the size of the current graphics device)
#'                    for the \code{ggsave} function.
#' @param dpi         a numeric value indicating the \code{dpi} argument
#'                    (default is \code{600}) for the \code{ggsave} function.
#' @param write       a character string naming a text file with file extension
#'                    \code{".txt"} (e.g., \code{"Output.txt"}) for writing the
#'                    output into a text file.
#' @param append      logical: if \code{TRUE} (default), output will be appended
#'                    to an existing text file with extension \code{.txt} specified
#'                    in \code{write}, if \code{FALSE} existing text file will be
#'                    overwritten.
#' @param check       logical: if \code{TRUE} (default), argument specification
#'                    is checked.
#' @param output      logical: if \code{TRUE} (default), output is shown.
#' @param ...         additional plotting arguments from the \code{plot.misty.object()}
#'                    function, see 'Details in the \code{\link{plot.misty.object}}
#'                    function.
#'
#' @author
#' Takuya Yanagida
#'
#' @seealso
#' \code{\link{check.resid}}, \code{\link{test.homo}}, \code{\link{test.norm}},
#' \code{\link{coeff.robust}}
#'
#' @references
#' Harvey, A., & Collier, P. (1977). Testing for functional misspecification in
#' regression analysis. \emph{Journal of Econometrics, 6}(1), 103-119.
#'
#' Ramsey, J. B. (1969). Tests for specification errors in classical linear
#' least-squares regression analysis. \emph{Journal of the Royal Statistical Society: Series B (Methodological), 31}(2),
#' 350-371.
#'
#' Utts, J. M. (1982). The rainbow test for lack of fit in regression.
#' \emph{Communications in Statistics - Theory and Methods, 11}(24), 2801-2815.
#'
#' Zeileis, A., & Hothorn, T. (2002). Diagnostic checking in regression relationships.
#' \emph{R News, 2}(3), 7-10.
#'
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{model}}{model specified in \code{model}}
#' \item{\code{plot}}{ggplot2 object for plotting the results}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{result}}{data frame with results}
#'
#' @note
#' This function is based on the \code{resettest()}, \code{harvtest()}, and
#' \code{raintest()} function in the \pkg{lmtest} package by Achim Zeileis
#' and Torsten Hothorn.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #————————————————————————————————————————————————————————————————————————————
#' # Tests for Linearity in a Linear Model
#'
#' # Estimate linear model
#' mod <- lm(mpg ~ disp + hp + drat, data = mtcars)
#'
#' # Example 1a: RESET test (default)
#' test.linear(mod)
#'
#' # Example 1b: Harvey-Collier test
#' test.linear(mod, method = "harv")
#'
#' # Example 1c: Rainbow test
#' test.linear(mod, method = "rain")
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # 'order' Argument
#'
#' # Example 2a: Harvey-Collier test, order observations by 'hp'
#' test.linear(mod, method = "harv", order = "hp")
#'
#' # Example 2a: Rainbow test, order by Mahalanobis distances
#' test.linear(mod, method = "rain", order = "mahalanobis")
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Plot
#'
#' # Example 3a: Partial residual plots
#' test.linear(mod, plot = TRUE)
#'
#' # Example 3b: Plot with smoothing line only
#' test.linear(mod, plot = TRUE, geom = "loess")
#'
#' # Example 3c: Partial residual plots, additional arguments
#' # see Details in the help page of the function plot.misty.object
#' test.linear(mod, plot = TRUE, point.size = 1, point.shape = 2)
#'
#' # Example 3d: Plot results using the plot() function, additional arguments
#' # see Details in the help page of the function plot.misty.object
#' object <- test.linear(mod)
#' plot(object, point.size = 1, point.shape = 2)
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Write Results
#'
#' # Example 4a: Write results into a text file
#' test.linear(mod, write = "Test_Linear.txt")
#'
#' # Example 4b: Write results into an Excel file
#' test.linear(mod, write = "Test_Linear.xlsx")
#'
#' # Example 4c: Save plot as PNG fine
#' test.linear(mod, plot = TRUE, filename = "Linear.png", width = 15, height = 5)
#' }
test.linear <- function(model, method = c("reset", "harv", "rain"), power = c(2, 3),
                        frac = 0.5, order = c("none", "all", "mahalanobis"),
                        p.adj = "none", digits = 2, p.digits = 3, plot = FALSE,
                        geom = c("all", "none", "lm", "loess", "ci.lm", "ci.loess"),
                        filename = NULL, width = NA, height = NA, dpi = 600,
                        write = NULL, append = TRUE, check = TRUE, output = TRUE, ...) {

  #_____________________________________________________________________________
  #
  # Initial Check ——————————————————————————————————————————————————————————————

  # Check if input 'model' is missing or NULL
  if (isTRUE(missing(model) || is.null(model))) { stop("Please specify a fitted model for the argument 'model'.", call. = FALSE) }

  # Check if input 'model' is not 'lm'
  if (isTRUE(class(model) != "lm")) { stop("Please specify a fitted model object from the \"lm\" function for the argument 'model'.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Input Check ————————————————————————————————————————————————————————————————

  # Check inputs
  .check.input(logical = c("plot", "append", "output"), numeric = list(frac = 1L), s.character = list(method = c("reset", "harv", "rain")), m.character = list(geom = c("all", "none", "lm", "loess", "ci.lm", "ci.loess")),
               args = c("digits", "p.digits", "write2"), envir = environment(), input.check = check)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Additional Checks ####

  if (isTRUE(check)) {

    # 'power' Argument
    if (isTRUE(!all(power == (2L:(length(power) + 1L))))) { stop("Please specify a numeric vector of positive consecutive integers starting from 2 for the argument 'power'.", call. = FALSE) }

    # 'frac' Argument
    if (isTRUE(frac <= 0L || frac >= 1L)) { stop("Please specify a numeric value between 0 and 1 for the argument 'frac'.", call. = FALSE) }

    # 'order' Argument
    if (isTRUE(any(!order %in% c("none", "all", "mahalanobis")))) {

      if (isTRUE(length(order) != 1L)) { stop("Please specify a single variable name for the argument 'order'.", call. = FALSE) }

      if (isTRUE(!order %in% colnames(model.frame(model)))) { stop("Name of the predictor variable specified in the argument 'order' was not found in the model.", call. = FALSE) }

    }

  }

  #_____________________________________________________________________________
  #
  # Arguments ——————————————————————————————————————————————————————————————————

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'method' Argument ####

  if (isTRUE(all(c("reset", "harv", "rain") %in% method))) { method <- "reset" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'order' Argument ####

  if (isTRUE(all(c("none", "all", "mahalanobis") %in% order))) { order <- "all" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'geom' Argument ####

  if (isTRUE(all(c("all", "none", "lm", "loess", "ci.lm", "ci.loess") %in% geom) || all(geom == "all"))) { geom <- c("lm", "loess", "ci.lm", "ci.loess") }

  #_____________________________________________________________________________
  #
  # Main Function ——————————————————————————————————————————————————————————————

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## RESET Test ####

  restab <- switch(method, "reset" = {

    .test.reset(model, power = power)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Harvey-Collier Test ####

    }, "harv" = {

      # Observations assumed to be ordered
      if (isTRUE(order == "none")) {

        data.frame(order = "None", .test.harv(model, order = NULL), row.names = NULL)

      # Test for each predictor
      } else if (isTRUE(order == "all")) {

        do.call("rbind", lapply(attr(model$terms, which = "term.labels"), function(y) data.frame(order = y, .test.harv(model, order = y), row.names = NULL)))

      # Order by predictor
      } else {

        data.frame(order = order, .test.harv(model, order = order), row.names = NULL)

      }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Rainbow Test ####

    }, "rain" = {

      # Observations assumed to be ordered
      if (isTRUE(order == "none")) {

        data.frame(order = "None", .test.rain(model, frac = frac, order = NULL), row.names = NULL)

      # Order each predictor
      } else if (isTRUE(order == "all")) {

        do.call("rbind", lapply(attr(model$terms, which = "term.labels"), function(y) data.frame(order = y, .test.rain(model, frac = frac, order = y), row.names = NULL)))

      # Mahalanobis distances
      } else if (isTRUE(order == "mahalanobis")) {

        data.frame(order = "Mahalanobis", .test.rain(model, frac = frac, order = "mahalanobis"), row.names = NULL)

      # Order by predictor
      } else {

        data.frame(order = order, .test.rain(model, frac = frac, order = order), row.names = NULL)

      }

    })

  # Multiple-testing adjustment
  if (isTRUE(p.adj != "none" && method %in% c("harv", "rain") && order == "all" && length(attr(model$terms, which = "term.labels")) > 1L)) { restab$pval <- p.adjust(restab$pval, method = p.adj) }

  # Descriptive statistics
  restab <- data.frame(n = nrow(model$model), nNA = length(summary(model)$na.action), restab) |> (\(p) if (isTRUE("order" %in% colnames(p))) { p[, c("order", setdiff(colnames(p), "order"))] } else { return(p) })()

  #_____________________________________________________________________________
  #
  # Return Object ——————————————————————————————————————————————————————————————

  object <- list(call = match.call(),
                 type = "test.linear",
                 model = model,
                 plot = NULL,
                 args = list(method = method, power = power, frac = frac, order = order, p.adj = p.adj, digits = digits, p.digits = p.digits, plot = plot, geom = geom, filename = filename, width = width, height = height, dpi = dpi, write = write, append = append, check = check, output = output),
                 result = restab)

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  ## Plot and Save Plot ————————————————————————————————————————————————————————

  if (isTRUE(plot)) { object$plot <- plot(object, filename = filename, width = width, height = height, dpi = dpi, check = FALSE, ...) |> (\(p) suppressMessages(suppressWarnings(print(p))))() }

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
