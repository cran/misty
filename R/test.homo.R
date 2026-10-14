#' Tests for Homoscedasticity in a Linear Model
#'
#' This function performs the Breusch-Pagan test (Breusch & Pagan, 1979) without
#' or with the studentising modification of Koenker (1981), the White's test
#' with squared terms or squared and interaction terms (White, 1980), Evans-King
#' Generalized Least Squares test (Evans & King, 1988), and the Verbyla's Residual
#' Maximum Likelihood test (Verbyla, 1993) for testing homoskedasticity (null
#' hypothesis) against heteroskedasticity (alternative hypothesis) in a linear
#' regression model. By default, the function performs the Koenker's Studentized
#' Breusch-Pagan test.
#'
#' @param model       a fitted model of class \code{lm}.
#' @param method      a character string indicating the method used for testing
#'                    homoscedasticity, i.e., \code{"bp"} for the the Breusch-Pagan
#'                    test, \code{"sbp"} (default) for the Koenker's Studentized
#'                    Breusch-Pagan test, \code{"white1"} for the White's Test
#'                    regressing squared residuals on predictors and their squares,
#'                    \code{"white2"} for the White's Test regressing squared
#'                    residuals on predictors, their squares, and two-way interactions,
#'                    \code{"evansking"} for the Evans-King Generalized Least
#'                    Squares test, and \code{"verbyla"} for the Verbyla's Residual
#'                    Maximum Likelihood test.
#' @param pred        a character string or character vector indicating the
#'                    predictor variables for the auxiliary design matrix that
#'                    are suspected to be related to the residual variance under
#'                    the alternative hypothesis. By default, the design matrix
#'                    of the regression model including all predictor variables
#'                    is used.
#' @param lambda.star a numeric value (default: \code{5}) indicating the degree
#'                    of heteroskedasticity under the alternative hypothesis for
#'                    Evans-King Generalized Least Squares test
#'                    (\code{method = "evansking"}). Evans and  King (1985) suggests
#'                    2.5, 5, 7.5, and 10 as values to consider, and Evans and
#'                    King (1988) finds that 2.5 and 5 perform best empirically.
#' @param digits      an integer value indicating the number of decimal places
#'                    to be used for displaying the test statistic.
#' @param p.digits    an integer value indicating the number of decimal places
#'                    to be used for displaying the \emph{p}-value.
#' @param plot        logical: if \code{TRUE}, a plot for predicted values against
#'                    the studentized residuals is drawn.
#'                    For additional plotting arguments, see \code{Details} in
#'                    the help page of the function \code{plot.misty.object}.
#' @param geom        a character vector indicating which plot elements to draw,
#'                    i.e., \code{"all"} (default) for drawing all plot elements,
#'                    \code{"none"} for drawing no horizontal or smoothing line,
#'                    \code{"hline"} for drawing a horizontal line at \code{0},
#'                    \code{"loess"} for drawing the LOESS smoothing curve, and
#'                    \code{"ci.loess"} for drawing confidence bands around the
#'                    smoothing curve (i.e., \code{se = TRUE}).
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
#' @details
#' Farrar et al. (2025) reviewed 25 different tests for homoscedasticity implemented
#' in the R package \pkg{scedastic} (Farrar, 2025) and presented results of a Monte
#' Carlo simulation demonstrating that the Evans-King GLS test and Verbyla's
#' Residual Maximum Likelihood test outperform commonly used tests for homoskedasticity,
#' such as the Koenker's Studentized Breusch-Pagan test, White’s test, and the
#' Goldfeld-Quandt F test.
#'
#' @author
#' Takuya Yanagida
#'
#' @seealso
#' \code{\link{check.resid}}, \code{\link{test.linear}}, \code{\link{test.norm}},
#' \code{\link{coeff.robust}}
#'
#' @references
#' Breusch, T. S., & Pagan, A.R. (1979) A simple test for heteroscedasticity and
#' Random Coefficient Variation. \emph{Econometrica, 47}, 1287-1294.
#' http://dx.doi.org/10.2307/1911963
#'
#' Evans, M. A., & King, M. L. (1985). A point optimal test for heteroscedastic
#' disturbances. \emph{Journal of Econometrics, 27}(2), 163–178.
#'
#' Evans, M. A., & King, M. L. (1988). A further class of tests for heteroscedasticity.
#' \emph{Journal of Econometrics, 37}(2), 265–276.
#'
#' Farrar, T. J. (2025). \emph{skedastic: Heteroskedasticity Diagnostics for Linear Regression Models}.
#' R Package version 2.0.3. https://github.com/tjfarrar/skedastic
#'
#' Farrar, T., Blignaut, R., Luus, R., & Steel, S. (2025). A review and comparison
#' of methods of testing for heteroskedasticity in the linear regression model.
#' \emph{Journal of applied statistics, 52}(16), 3121-3150.
#' https://doi.org/10.1080/02664763.2025.2575038
#'
#' Koenker, R. (1981). A note on studentizing a test for heteroscedasticity.
#' \emph{Journal of Econometrics, 17}(1), 107–112.
#' https://doi.org/10.1016/0304-4076(81)90062-2.
#'
#' Verbyla, A. P. (1993). Modelling variance heterogeneity: Residual maximum
#' likelihood and diagnostics. \emph{Journal of the Royal Statistical Society: Series B (Methodological), 55}(2),
#' 493–508.
#'
#' White, H. (1980). A heteroskedasticity-consistent covariance matrix estimator
#' and a direct test for heteroskedasticity. \emph{Econometrica, 48}(4), 817–838.
#' https://doi.org/10.2307/1912934
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
#' This function is based on the \code{breusch_pagan()}, \code{white()},
#' \code{evans_king()}, \code{pRQF()} and \code{verbyla()} function in the
#' \pkg{skedastic} package by Thomas Farrar.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #————————————————————————————————————————————————————————————————————————————
#' # Tests for Homoscedasticity in a Linear Model
#'
#' # Estimate linear model
#' mod <- lm(mpg ~ cyl + disp, data = mtcars)
#'
#' # Example 1a: Koenker's Studentized Breusch-Pagan test (default)
#' test.homo(mod)
#'
#' # Example 1b: White's Test regressing squared residuals on predictors and their squares
#' test.homo(mod, method = "white1")
#'
#' # Example 1c: Evans-King Generalized Least Squares test
#' test.homo(mod, method = "evansking")
#'
#' # Example 1d: Verbyla's Residual Maximum Likelihood test
#' test.homo(mod, method = "verbyla")
#'
#' # Example 2: Predictor variable 'disp' used in the auxiliary design matrix
#' test.homo(mod, pred = "disp")
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Plot
#'
#' # Example 3a: Predicted values vs. Studentized residuals
#' test.homo(mod, plot = TRUE)
#'
#' # Example 3b: Plot with horizontal line only
#' test.homo(mod, plot = TRUE, geom = "hline")
#'
#' # Example 3c: redicted values vs. Studentized residuals, additional arguments
#' # see Details in the help page of the function plot.misty.object
#' test.homo(mod, plot = TRUE, point.shape = 2, linetype = "solid", line.col = "blue",
#'           xlim = c(0, 30))
#'
#' # Example 3d: Plot results using the plot() function, use additional arguments
#' # see Details in the help page of the function plot.misty.object
#' object <- test.homo(mod)
#' plot(object, point.shape = 2, linetype = "solid", line.col = "blue", xlim = c(0, 30))
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Write Results
#'
#' # Example 4a: Write results into a text file
#' test.homo(mod, write = "Test_Homo.txt")
#'
#' # Example 4b: Write results into an Excel file
#' test.homo(mod, write = "Test_Homo.xlsx")
#'
#' # Example 4c: Save plot as PNG fine
#' test.homo(mod, plot = TRUE, filename = "Homo.png", width = 7, height = 6)
#' }
test.homo <- function(model, method = c("bp", "sbp", "white1", "white2", "evansking", "verbyla"),
                      pred = attr(terms(model), which = "term.labels"), lambda.star = 5,
                      digits = 2, p.digits = 3, plot = FALSE, geom = c("all", "none", "hline", "loess", "ci.loess"),
                      filename = NULL, width = NA, height = NA, dpi = 600, write = NULL, append = TRUE,
                      check = TRUE, output = TRUE, ...) {

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
  .check.input(logical = c("append", "output"), numeric = list(lambda.star = 1L), s.character = list(method = c("bp", "sbp", "white1", "white2", "evansking", "verbyla")), m.character = list(geom = c("all", "none", "hline", "loess", "ci.loess")),
               args = c("digits", "p.digits", "write2"), envir = environment(), input.check = check)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Additional Checks ####

  # Variable names in 'pred'
  if (isTRUE(check)) { (!pred %in% attr(terms(model), which = "term.labels")) |> (\(p) if (isTRUE(any(p))) { stop(paste0("Variable names specified in the argument 'pred' are not all included in the model specified in 'model': ", paste(pred[p], collapse = ", ")), call. = FALSE) } )() }

  #_____________________________________________________________________________
  #
  # Arguments ——————————————————————————————————————————————————————————————————

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'method' Argument ####

  if (isTRUE(all(c("bp", "sbp", "white1", "white2", "evansking", "verbyla") %in% method))) { method <- "evansking" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'geom' Argument ####

  if (isTRUE(all(c("all", "none", "hline", "loess", "ci.loess") %in% geom) || all(geom == "all"))) { geom <- c("hline", "loess", "ci.loess") }

  #_____________________________________________________________________________
  #
  # Main Function ——————————————————————————————————————————————————————————————

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## (Koenker's Studentized) Breusch-Pagan Test ####

  tryCatch(if (isTRUE(method %in% c("bp", "sbp"))) {

    # Extract model residuals
    e <- resid(model)

    # Extract Design Matrix
    Z <- model.matrix(model) |> (\(p) cbind(p[, "(Intercept)", drop = FALSE], p[, pred, drop = FALSE]))()

    # Degrees of freedom
    df <- ncol(Z) - 1L

    # Sample size
    n <- nrow(Z)

    # Squared residuals
    w.hat <- e^2L - (sum(e^2L) / n)

    #—————————————————————————————————————— #
    ### Breusch-Pagan Test ####

    switch(method, "bp" = {

      # Test statistic
      stat <- sum(lm.fit(as.matrix(Z), w.hat)$fitted.values^2) / (2L * (sum(e^2) / n)^2L)

    #—————————————————————————————————————— #
    ### Koenker's Studentized Breusch-Pagan Test ####

    }, "sbp" = {

      # Test statistic
      stat <- n * sum(lm.fit(Z, w.hat)$fitted.values^2L) / sum(w.hat^2L)

    })

    # Result table
    restab <- data.frame(chisq = stat, df = df, p = pchisq(stat, df = df, lower.tail = FALSE))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## White's Test ####

  } else if (isTRUE(method %in% c("white1", "white2"))) {

    # Extract model residuals
    e <- resid(model)

    # Extract Design Matrix and add quadratic terms
    Z <- model.matrix(model) |> (\(p) cbind(p[, "(Intercept)", drop = FALSE], p[, pred, drop = FALSE], misty::df.rename(p[, pred, drop = FALSE]^2L, from = pred, to = paste0(pred, "2"), check = FALSE)))()

    # Sample size
    n <- nrow(Z)

    # Add interaction terms
    if (isTRUE(method == "white2")) { Z <- cbind(Z, .gen.inter(model.matrix(model)[, pred, drop = FALSE])) }

    # Degrees of freedom
    df <- ncol(Z) - 1L

    # Test statistic
    stat <- (e^2L) |> (\(p) as.double(n * (1L - crossprod(stats::lm.fit(Z, p)$residuals) / (t(p) %*% (diag(n) - 1L / n * (tcrossprod(rep(1L, times = n)))) %*% p))))()

    # Result table
    restab <- data.frame(chisq = stat, df = df, p = pchisq(stat, df = df, lower.tail = FALSE))

  } else {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Evans-King Generalized Least Squares Test ####

    switch(method, "evansking" =  {

      # Extract model residuals
      e <- resid(model)

      # Extract Design Matrix
      X <- model.matrix(model) |> (\(p) cbind(p[, "(Intercept)", drop = FALSE], p[, pred, drop = FALSE]))()

      # Sample size
      n <- nrow(X)

      M <- .funcM(X, n)

      w <- (1L + lambda.star * (((seq_len(n)) - 1L) / (n - 1L))) ^ (1L / 2L)

      R <- diag(1L / w)

      Mstar <- .funcM(sapply(seq_len(n), function(y) X[y, ] / w[y], USE.NAMES = FALSE) |> (\(p) if (isTRUE(is.matrix(p))) { return(t(p)) } else { return(as.matrix(p)) })(), n)

      # Test statistic
      stat <- as.double((t(e) %*% R %*% Mstar %*% R %*% e) / crossprod(e))

      # Result table
      restab <- data.frame(T = stat, param = lambda.star, p = .pRQF(r = stat, A = M %*% R %*% Mstar %*% R %*% M, B = M, algorithm = "imhof", lower.tail = TRUE))

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Verbyla's Residual Maximum Likelihood Test ####

    }, "verbyla" = {

      # Extract Design Matrix
      X <- model.matrix(model)

      # Extract model residuals
      e <- resid(model)

      # Number of columns in the design matrix
      p <- ncol(X)

      # Extract Design Matrix
      Z <- model.matrix(model) |> (\(p) cbind(p[, "(Intercept)", drop = FALSE], p[, pred, drop = FALSE]))()

      # Degrees of freedom
      df <- ncol(Z) - 1L

      # Sample size
      n <- nrow(Z)

      M <- .funcM(X, n)

      term <- t(t(e^2L / (sum(e^2L) / (n - p)) - diag(M)))

      mat.to.invert <- t(Z) %*% (M^2L) %*% Z

      if (isTRUE(det(mat.to.invert) == 0L)) {

        Z <- Z[, -1L, drop = FALSE]
        mat.to.invert <- t(Z) %*% (M ^ 2) %*% Z

      }

      # Test statistic
      stat <- as.double(1L / 2L * t(term) %*% Z %*% solve(mat.to.invert) %*% t(Z) %*% term)

      # Result table
      restab <- data.frame(chisq = stat, df = df, p = pchisq(stat, df = df, lower.tail = FALSE))

    })

  }, error = function(y) {

    stop(paste0(misty::rec(method, spec = "'bp' = 'Breusch-Pagan Test'; 'sbp' = 'Koenker Studentized Breusch-Pagan test'; 'white1' = 'White test'; 'white2' = 'White test'; 'evansking' = 'Evans-King Generalized Least Squares test' ; 'verbyla' = 'Verbyla Residual Maximum Likelihood test'"), " could not be computed."), call. = FALSE)

  })

  # Descriptive statistics
  restab <- data.frame(n = nrow(model$model), nNA = length(summary(model)$na.action), restab)

  #_____________________________________________________________________________
  #
  # Return Object ——————————————————————————————————————————————————————————————

  object <- list(call = match.call(),
                 type = "test.homo",
                 model = model,
                 plot = NULL,
                 args = list(method = method, pred = pred, lambda.star = lambda.star, digits = digits, p.digits = p.digits, plot = plot, geom = geom, filename = filename, width = width, height = height, check = check, output = output),
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
