#' Unstandardized Coefficients with Heteroscedasticity-Consistent Standard Errors
#'
#' This function computes heteroscedasticity-consistent standard errors and
#' significance values for linear models estimated by using the \code{lm()}
#' function and generalized linear models estimated by using the \code{glm()}
#' function. For linear models the heteroscedasticity-robust F-test is computed
#' as well. By default, the function uses the HC4 estimator.
#'
#' @param model    a fitted model of class \code{lm} or \code{glm}.
#' @param type     a character string specifying the estimation type, where
#'                 \code{"H0"} gives White's estimator and \code{"H1"} to
#'                 \code{"H5"} are refinement of this estimator. See help page
#'                 of the \code{vcovHC()} function in the R package \code{sandwich}
#'                 for more details.
#' @param digits   an integer value indicating the number of decimal places
#'                 to be used for displaying results. Note that information
#'                 criteria and chi-square test statistic are printed with
#'                 \code{digits} minus 1 decimal places.
#' @param p.digits an integer value indicating the number of decimal places
#' @param write    a character string naming a file for writing the output into
#'                 either a text file with file extension \code{".txt"} (e.g.,
#'                 \code{"Output.txt"}) or Excel file with file extension
#'                 \code{".xlsx"}  (e.g., \code{"Output.xlsx"}). If the file
#'                 name does not contain any file extension, an Excel file will
#'                 be written.
#' @param append   logical: if \code{TRUE} (default), output will be appended
#'                 to an existing text file with extension \code{.txt} specified
#'                 in \code{write}, if \code{FALSE} existing text file will be
#'                 overwritten.
#' @param check    logical: if \code{TRUE} (default), argument specification
#'                 is checked.
#' @param output   logical: if \code{TRUE} (default), output is shown.
#'
#' @details
#' The family of heteroscedasticity-consistent (HC) standard errors estimator for
#' the model parameters of a regression model is based on an HC covariance matrix
#' of the parameter estimates and does not require the assumption of homoscedasticity.
#' HC estimators approach the correct value with increasing sample size, even in
#' the presence of heteroscedasticity. On the other hand, the OLS standard error
#' estimator is biased and does not converge to the proper value when the assumption
#' of homoscedasticity is violated (Darlington & Hayes, 2017). White (1980) introduced
#' the idea of HC covariance matrix to econometricians and derived the asymptotically
#' justified form of the HC covariance matrix known as HC0 (Long & Ervin, 2000).
#' Simulation studies have shown that the HC0 estimator tends to underestimate the
#' true variance in small to moderately large samples (\eqn{N \keq 250}) and in
#' the presence of leverage observations, which leads to an inflated type I error
#' risk (e.g., Cribari-Neto & Lima, 2014). The alternative estimators HC1 to HC5
#' are asymptotically equivalent to HC0 but include finite-sample corrections,
#' which results in superior small sample properties compared to the HC0 estimator.
#' Long and Ervin (2000) recommended routinely using the HC3 estimator regardless
#' of a heteroscedasticity test. However, the HC3 estimator can be unreliable when
#' the data contains leverage observations. The HC4 estimator, on the other hand,
#' performs well with small samples, in the presence of high leverage observations,
#' and when errors are not normally distributed (Cribari-Neto, 2004). In summary,
#' it appears that the HC4 estimator performs the best in terms of controlling the
#' type I and type II error risk (Rosopa, 2013). As opposed to the findings of
#' Cribari-Neto et al. (2007), the HC5 estimator did not show any substantial
#' advantages over HC4. Both HC5 and HC4 performed similarly across all the simulation
#' conditions considered in the study (Ng & Wilcox, 2009).
#' Note that the \emph{F}-test of significance on the multiple correlation coefficient
#' \eqn{R} also assumes homoscedasticity of the errors. Violations of this assumption
#' can result in a hypothesis test that is either liberal or conservative, depending
#' on the form and severity of the heteroscedasticity.
#' Hayes (2007) argued that using a HC estimator instead of assuming homoscedasticity
#' provides researchers with more confidence in the validity and statistical power
#' of inferential tests in regression analysis. Hence, the HC3 or HC4 estimator
#' should be used routinely when estimating regression models. If a HC estimator
#' is not used as the default method of standard error estimation, researchers are
#' advised to at least double-check the results by using an HC estimator to ensure
#' that conclusions are not compromised by heteroscedasticity. However, the presence
#' of heteroscedasticity suggests that the data is not adequately explained by
#' the statistical model of estimated conditional means. Unless heteroscedasticity
#' is believed to be solely caused by measurement error associated with the predictor
#' variable(s), it should serve as warning to the researcher regarding the adequacy
#' of the estimated model.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{std.coef}}, \code{\link{write.result}}
#'
#' @references
#' Darlington, R. B., & Hayes, A. F. (2017). \emph{Regression analysis and linear
#' models: Concepts, applications, and implementation}. The Guilford Press.
#'
#' Cribari-Neto, F. (2004). Asymptotic inference under heteroskedasticity of unknown
#' form. \emph{Computational Statistics & Data Analysis, 45}, 215-233.
#' https://doi.org/10.1016/S0167-9473(02)00366-3
#'
#' Cribari-Neto, F., & Lima, M. G. (2014). New heteroskedasticity-robust standard
#' errors for the linear regression model. \emph{Brazilian Journal of Probability and Statistics, 28},
#' 83-95.
#'
#' Cribari-Neto, F., Souza, T., & Vasconcellos, K. L. P. (2007). Inference under
#' heteroskedasticity and leveraged data. \emph{Communications in Statistics - Theory and Methods, 36},
#' 1877-1888. https://doi.org/10.1080/03610920601126589
#'
#' Hayes, A.F, & Cai, L. (2007). Using heteroscedasticity-consistent standard error
#' estimators in OLS regression: An introduction and software implementation.
#' \emph{Behavior Research Methods, 39}, 709-722. https://doi.org/10.3758/BF03192961
#'
#' Long, J.S., & Ervin, L.H. (2000). Using heteroscedasticity consistent standard
#' errors in the linear regression model. \emph{The American Statistician, 54},
#' 217-224. https://doi.org/10.1080/00031305.2000.10474549
#'
#' Ng, M., & Wilcoy, R. R. (2009). Level robust methods based on the least squares
#' regression estimator. \emph{Journal of Modern Applied Statistical Methods, 8},
#' 284-395. https://doi.org/10.22237/jmasm/1257033840
#'
#' Rosopa, P. J., Schaffer, M. M., & Schroeder, A. N. (2013). Managing heteroscedasticity
#' in general linear models. \emph{Psychological Methods, 18}(3), 335-351.
#' https://doi.org/10.1037/a0032553
#'
#' White, H. (1980). A heteroskedastic-consistent covariance matrix estimator and
#' a direct test of heteroskedasticity. \emph{Econometrica, 48}, 817-838.
#' https://doi.org/10.2307/1912934
#'
#' Zeileis, A., & Hothorn, T. (2002). Diagnostic checking in regression relationships.
#' \emph{R News, 2}(3), 7–10. http://CRAN.R-project.org/doc/Rnews/
#'
#' Zeileis A, Köll S, & Graham N (2020). Various versatile variances: An
#' object-oriented implementation of clustered covariances in R.
#' \emph{Journal of Statistical Software, 95}(1), 1-36.
#' https://doi.org/10.18637/jss.v095.i01
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{model}}{model specified in \code{model}}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{result}}{list with results, i.e., \code{coef} for the unstandardized
#' regression coefficients with heteroscedasticity-consistent standard errors,
#' \code{F.test} for the heteroscedasticity-robust F-Test, and \code{sandwich}
#' for the sandwich covariance matrix}
#'
#' @note
#' This function is based on the \code{vcovHC} function from the \code{sandwich}
#' package (Zeileis, Köll, & Graham, 2020) and the functions \code{coeftest} and
#' \code{waldtest} from the \code{lmtest} package (Zeileis & Hothorn, 2002).
#'
#' @export
#'
#' @examples
#' #----------------------------------------------------------------------------
#' # Example 1: Linear model
#'
#' mod.lm <- lm(mpg ~ cyl + disp, data = mtcars)
#' robust.coef(mod.lm)
#'
#' #----------------------------------------------------------------------------
#' # Example 2: Generalized linear model
#'
#' mod.glm <- glm(carb ~ cyl + disp, data = mtcars, family = poisson())
#' robust.coef(mod.glm)
#'
#' #----------------------------------------------------------------------------
#' # Write Results
#'
#' # Example 3a: Write results into a text file
#' robust.coef(mod.lm, write = "Robust_Coef.txt", output = FALSE)
#'
#' # Example 3b: Write results into a Excel file
#' robust.coef(mod.lm, write = "Robust_Coef.xlsx", output = FALSE)
robust.coef <- function(model, type = c("HC0", "HC1", "HC2", "HC3", "HC4", "HC4m", "HC5"),
                        digits = 3, p.digits = 3, write = NULL, append = TRUE, check = TRUE,
                        output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'model' is missing
  if (isTRUE(missing(model))) { stop("Input for the argument 'model' is missing.", call. = FALSE) }

  # Check if input 'model' is NULL
  if (isTRUE(is.null(model))) { stop("Input specified for the argument 'model' is NULL.", call. = FALSE) }

  # Check if input 'model' is not 'lm'
  if (isTRUE(!any(class(model) %in% c("lm", "glm")) )) { stop("Please specify an \"lm\" or \"glm\" object for the argument 'model'.", call. = FALSE) }


  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("append", "output"), s.character = list(type = c("HC0", "HC1", "HC2", "HC3", "HC4", "HC4m", "HC5")), args = c("digits", "p.digits", "write2"), envir = environment(), input.check = check)

  #_____________________________________________________________________________
  #
  # Data and Arguments ---------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Estimation Type ####

  #...................
  ### Default setting ####
  if (isTRUE(all(c("HC0", "HC1", "HC2", "HC3", "HC4", "HC4m", "HC5") %in% type))) {

    type <- "HC4"

  } else {

    if (isTRUE(length(type) != 1L)) { stop("Please specify a character string for the argument 'type'", call. = FALSE)}

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  # Sandwich
  sandw <- .sandw(model, type = type)

  # Inference for estimated coefficients
  coef.res <- .coeftest(model, vcov = sandw)

  # Linear model
  F.test <- NULL
  if (isTRUE(length(class(model)) == 1L)) {

    # Waldtest
    F.test <- .waldtest(update(model, formula = ~ 1, data = model$model), model, vcov = sandw)

  }

  #_____________________________________________________________________________
  #
  # Return object --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Return object ####

  object <- list(call = match.call(),
                 type = "robust.coef",
                 model = model,
                 args = list(type = type, digits = digits, p.digits = p.digits,
                             write = write, append = append, check = check, output = output),
                 result = list(coef = coef.res, F.test = F.test, sandwich = sandw))

  class(object) <- "misty.object"

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
