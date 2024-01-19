#' Standardized Coefficients
#'
#' This function computes standardized coefficients for linear models estimated
#' by using the \code{lm()} function.
#'
#' @param model    a fitted model of class \code{"lm"}.
#' @param print    a character vector indicating which results to show, i.e.
#'                 \code{"all"}, for all results, \code{"stdx"} for standardizing
#'                 only the predictor, \code{"stdy"} for for standardizing only
#'                 the criterion, and \code{"stdyx"} for for standardizing both
#'                 the predictor and the criterion. Note that the default setting
#'                 is depending on the level of measurement of the predictors,
#'                 i.e., if all predictors are continuous, the default setting
#'                 is \code{print = "stdyx"}; if all predictors are binary, the
#'                 default setting is \code{print = "stdy"}; if predictors
#'                 are continuous and binary, the default setting is
#'                 \code{print = c("stdy", "stdyx")}.
#' @param digits   an integer value indicating the number of decimal places to
#'                 be used for displaying
#'                 results.
#' @param p.digits an integer value indicating the number of decimal places to be
#'                 used for displaying the \emph{p}-value.
#' @param write    a character string for writing the results into a Excel file
#'                 naming a file with or without file extension '.xlsx', e.g.,
#'                 \code{"Results.xlsx"} or \code{"Results"}.
#' @param write    a character string naming a text file with file extension
#'                 \code{".txt"} (e.g., \code{"Output.txt"}) for writing the
#'                 output into a text file.
#' @param append   logical: if \code{TRUE} (default), output will be appended
#'                 to an existing text file with extension \code{.txt} specified
#'                 in \code{write}, if \code{FALSE} existing text file will be
#'                 overwritten.
#' @param check    logical: if \code{TRUE} (default), argument specification is
#'                 checked.
#' @param output   logical: if \code{TRUE} (default), output is shown on the console.
#'
#' @details
#' The slope \eqn{\beta} can be standardized with respect to only \eqn{x}, only
#' \eqn{y}, or both \eqn{y} and \eqn{x}:
#'
#' \deqn{StdX(\beta_1) = \beta_1 SD(x)}
#'
#' \eqn{StdX(\beta_1)} standardizes with respect to \eqn{x} only and is interpreted
#' as the change in \eqn{y} when \eqn{x} changes one standard deviation referred
#' to as \eqn{SD(x)}.
#'
#' \deqn{StdY(\beta_1) = \frac{\beta_1}{SD(x)}}
#'
#' \eqn{StdY(\beta_1)} standardizes with respect to \eqn{y} only and is interpreted
#' as the change in \eqn{y} standard deviation units, referred to as \eqn{SD(y)},
#' when \eqn{x} changes one unit.
#'
#' \deqn{StdYX(\beta_1) = \beta_1 \frac{SD(x)}{SD(y)}}
#'
#' \eqn{StdYX(\beta_1)} standardizes with respect to both \eqn{y} and \eqn{x} and
#' is interpreted as the change in \eqn{y} standard deviation units when \eqn{x}
#' changes one standard deviation.
#'
#' Note that the \eqn{StdYX(\beta_1)} and the \eqn{StdY(\beta_1)} standardizations
#' are not suitable for the slope of a binary predictor because a one standard
#' deviation change in a binary variable is generally not of interest (Muthen,
#' Muthen, & Asparouhov, 2016).
#'
#' The standardization of the slope \eqn{\beta_3} in a regression model with an
#' interaction term uses the product of standard deviations \eqn{SD(x_1)SD(x_2)}
#' rather than the standard deviation of the product \eqn{SD(x_1 x_2)} for the
#' interaction variable \eqn{x_1}\eqn{x_2} (see Wen, Marsh & Hau, 2010). Likewise,
#' the standardization of the slope \eqn{\beta_3} in a polynomial regression model
#' with a quadratic term uses the product of standard deviations \eqn{SD(x)SD(x)}
#' rather than the standard deviation of the product \eqn{SD(x x)} for the quadratic
#' term \eqn{x^2}.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @references
#' Muthen, B. O., Muthen, L. K., & Asparouhov, T. (2016). \emph{Regression and
#' mediation analysis using Mplus}. Muthen & Muthen.
#'
#' Wen, Z., Marsh, H. W., & Hau, K.-T. (2010). Structural equation models of latent
#' interactions: An appropriate standardized solution and its scale-free properties.
#' \emph{Structural Equation Modeling: A Multidisciplinary Journal, 17}, 1-22.
#' https://doi.org/10.1080/10705510903438872
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#' \tabular{ll}{
#' \code{call} \tab function call \cr
#' \code{type} \tab type of analysis \cr
#' \code{model} \tab model specified in \code{model} \cr
#' \code{args} \tab specification of function arguments \cr
#' \code{result} \tab list of result table \cr
#' }
#'
#' @export
#'
#' @examples
#' dat <- data.frame(x1 = c(3, 2, 4, 9, 5, 3, 6, 4, 5, 6, 3, 5),
#'                   x2 = c(1, 4, 3, 1, 2, 4, 3, 5, 1, 7, 8, 7),
#'                   x3 = c(0, 0, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1),
#'                   y = c(2, 7, 4, 4, 7, 8, 4, 2, 5, 1, 3, 8))
#'
#' #----------------------------------------------------------------------------
#' # Linear model
#'
#' # Example 1: Regression model with continuous predictors
#' mod.lm1 <- lm(y ~ x1 + x2, data = dat)
#' std.coef(mod.lm1)
#'
#' # Example 2: Print all standardized coefficients
#' std.coef(mod.lm1, print = "all")
#'
#' # Example 3: Regression model with dichotomous predictor
#' mod.lm2 <- lm(y ~ x3, data = dat)
#' std.coef(mod.lm2)
#'
#' # Example 4: Regression model with continuous and dichotomous predictors
#' mod.lm3 <- lm(y ~ x1 + x2 + x3, data = dat)
#' std.coef(mod.lm3)
#'
#' # Example 5: Regression model with continuous predictors and an interaction term
#' mod.lm4 <- lm(y ~ x1*x2, data = dat)
#'
#' # Example 6: Regression model with a quadratic term
#' mod.lm5 <- lm(y ~ x1 + I(x1^2), data = dat)
#' std.coef(mod.lm5)
#'
#' #----------------------------------------------------------------------------
#' # Example 7: Write Results into a Excel file
#' \dontrun{
#' mod.lm1 <- lm(y ~ x1 + x2, data = dat)
#'
#' std.coef(mod.lm1, write = "Std_Coef.xlsx", output = FALSE)
#'
#' result <- std.coef(mod.lm1, output = FALSE)
#' write.result(result, "Std_Coef.xlsx")
#' }
std.coef <- function(model, print = c("all", "stdx", "stdy", "stdyx"),
                     digits = 3, p.digits = 4, write = NULL, append = TRUE,
                     check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'model' is missing
  if (isTRUE(missing(model))) { stop("Input for the argument 'model' is missing.", call. = FALSE) }

  # Check if input 'model' is NULL
  if (isTRUE(is.null(model))) { stop("Input specified for the argument 'model' is NULL.", call. = FALSE) }

  # Check if input 'model' is not 'lm'
  if (isTRUE(!all(class(model) %in% "lm"))) { stop("Please specify an \"lm\" object for the argument 'model'.", call. = FALSE) }

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  if (isTRUE(check)) {

    # Check input 'print'
    if (isTRUE(!all(print %in% c("all", "stdx", "stdy", "stdyx")))) { stop("Character strings in the argument 'print' do not all match with \"all\", \"stdx\", \"stdy\", or \"stdyx\".", call. = FALSE) }

    # Check input 'digits'
    if (isTRUE(digits %% 1L != 0L || digits < 0L)) { stop("Specify a positive integer number for the argument 'digits'.", call. = FALSE) }

    # Check input 'p.digits'
    if (isTRUE(p.digits %% 1L != 0L || p.digits < 0L)) { stop("Specify a positive integer number for the argument 'p.digits'.", call. = FALSE) }

    # Check input 'write'
    if (isTRUE(!is.null(write) && substr(write, nchar(write) - 3L, nchar(write)) != ".txt")) { stop("Please specify a character string with file extenstion '.txt' for the argument 'write'.") }

    # Check input 'append'
    if (isTRUE(!is.logical(append))) { stop("Please specify TRUE or FALSE for the argument 'append'.", call. = FALSE) }

    # Check input 'output'
    if (isTRUE(!is.logical(output))) { stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##  Print argument ####

  #...................
  ### Default settings ####

  if (isTRUE(all(c("all", "stdx", "stdy", "stdyx") %in% print))) {

    # All predictors binary, factor, or character
    if (isTRUE(all(sapply(model$model[, -1L, drop = FALSE], function(y) length(unique(y)) == 2L)) ||
               all(sapply(attr(terms(model), "dataClasses")[-1L], function(y) y == "factor")) ||
               all(sapply(attr(terms(model), "dataClasses")[-1L], function(y) y == "character")))) {

      print <- "stdy"

    # At least one predictor numeric
    } else if (isTRUE(any(sapply(model$model[, -1L, drop = FALSE], function(y) length(unique(y)) == 2L)) ||
                      any(attr(terms(model), "dataClasses") != "numeric"))) {

      print <- c("stdy", "stdyx")

    # All predictors continuous
    } else {

      print <- "stdyx"

    }

  }

  #...................
  ### Print = "all" ####

  if (isTRUE(length(print) == 1L && "all" %in% print)) { print <- c("stdx", "stdy", "stdyx") }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  # Predictor variables
  pred.var <- attr(terms(model), "term.labels")

  # Data classes of predictor variables
  pred.var.class <- attr(terms(model), "dataClasses")[-1L]

  # Criterion variable
  crit.var <- names(model$model)[!names(model$model) %in% pred.var]

  # Interaction terms
  pred.var.int <- pred.var[grep(":", pred.var)]

  # Polynomial terms
  pred.var.poly <- pred.var[grep("^", pred.var, fixed = TRUE)]

  # Unstandardized slopes
  coeff <- model$coefficients[which(names(model$coefficients) != "(Intercept)")]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Predictor standard deviations ####

  sd.pred <- setNames(rep(NA, times = length(coeff)), nm = names(coeff))
  for (i in seq_along(coeff)) {

    coeff.temp <- names(coeff)[i]

    # Interaction term
    if (isTRUE(coeff.temp %in% pred.var.int)) {

      sd.pred[i] <- prod(sapply(model$model[unlist(strsplit(coeff.temp, ":"))], function(y) sd(y, na.rm = TRUE)))

    # Polynomial terms
    } else if (isTRUE(coeff.temp %in% pred.var.poly)) {

      sd.pred[i] <- prod(rep(sd(model$model[, sub("I\\(", "", unlist(strsplit(coeff.temp, "^", fixed = TRUE))[1L])], na.rm = TRUE),
                             times = sub(")", "", unlist(strsplit(coeff.temp, "^", fixed = TRUE))[2L])))

    # Numeric predictor
    } else if (isTRUE(coeff.temp %in% pred.var)) {

      if (isTRUE(pred.var.class[coeff.temp] %in%  c("numeric", "nmatrix.1"))) {

        sd.pred[i] <- sd(model$model[, names(coeff)[i]], na.rm = TRUE)

      }

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Outcome standard deviation ####

  sd.crit <- sapply(model$model[, crit.var, drop = FALSE], sd, na.rm = TRUE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Coefficient table ####

  # Model with intercept
  if (isTRUE("(Intercept)" %in% names(model$coefficients))) {

    coefficients <- rbind(cbind(summary(model)$coefficients,
                          SD = c(NA, sd.pred),
                          StdX = c(NA, coeff * sd.pred),
                          StdY = c(NA, coeff / sd.crit),
                          StdYX = c(NA, coeff * (sd.pred / sd.crit))))

  # Model without intercept
  } else {

    coefficients <- rbind(cbind(summary(model)$coefficients,
                          SD = sd.pred,
                          StdX = coeff * sd.pred,
                          StdY = coeff / sd.crit,
                          StdYX = coeff * (sd.pred / sd.crit)))

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "std.coef",
                 model = model,
                 args = list(print = print, digits = digits, p.digits = p.digits,
                             write = write, append = append, check = check, output = output),
                 result = list(coef = coefficients, sd = c(sd.crit, sd.pred)))

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Write Results --------------------------------------------------------------

  if (isTRUE(!is.null(write))) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Text file ####

    # Send R output to textfile
    sink(file = write, append = ifelse(isTRUE(file.exists(write)), append, FALSE), type = "output", split = FALSE)

    if (append && isTRUE(file.exists(write))) { write("", file = write, append = TRUE) }

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
