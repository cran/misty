#' Unstandardized Coefficients with Heteroscedasticity-Consistent Standard Errors
#'
#' This function computes heteroscedasticity-consistent standard errors and
#' significance values for linear models estimated by using the \code{lm()}
#' function and generalized linear models estimated by using the \code{glm()}
#' function. For linear models the heteroscedasticity-robust F-test is computed
#' as well. By default the function uses the HC4 estimator.
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
#'                 \code{"Output.txt"}) or Excel file with file extention
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
#' of homoscedasticity is violated (Dalington & Hayes, 2017). White (1980) introduced
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
#' models: Concepts, applications, and implementation. The Guilford Press.
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
#' dat <- data.frame(x1 = c(3, 2, 4, 9, 5, 3, 6, 4, 5, 6, 3, 5),
#'                   x2 = c(1, 4, 3, 1, 2, 4, 3, 5, 1, 7, 8, 7),
#'                   x3 = c(0, 0, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1),
#'                   y1 = c(2, 7, 4, 4, 7, 8, 4, 2, 5, 1, 3, 8),
#'                   y2 = c(0, 1, 0, 2, 0, 1, 0, 0, 1, 2, 1, 0))
#'
#' #----------------------------------------------------------------------------
#' # Example 1: Linear model
#'
#' mod1 <- lm(y1 ~ x1 + x2 + x3, data = dat)
#' robust.coef(mod1)
#'
#' #----------------------------------------------------------------------------
#' # Example 2: Generalized linear model
#'
#' mod2 <- glm(y2 ~ x1 + x2 + x3, data = dat, family = poisson())
#' robust.coef(mod2)
#'
#' \dontrun{
#' #----------------------------------------------------------------------------
#' # Write Results
#'
#' # Example 3a: Write results into a text file
#' robust.coef(mod1, write = "Robust_Coef.txt", output = FALSE)
#'
#' # Example 3b: Write results into a Excel file
#' robust.coef(mod1, write = "Robust_Coef.xlsx", output = FALSE)
#'
#' result <- robust.coef(mod1, output = FALSE)
#' write.result(result, "Robust_Coef.xlsx")
#' }
robust.coef <- function(model, type = c("HC0", "HC1", "HC2", "HC3", "HC4", "HC4m", "HC5"),
                        digits = 3, p.digits = 4, write = NULL, append = TRUE, check = TRUE,
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

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    ## Check input 'type' ##
    if (isTRUE(!all(type %in% c("HC0", "HC1", "HC2", "HC3", "HC4", "HC4m", "HC5")))) { stop("Character string in the argument 'estimator' does not match with \"HC0\", \"HC1\", \"HC2\", \"HC3\", \"HC4\", \"HC4m\", or \"HC5\".", call. = FALSE) }

    ## Check input 'digits' ##
    if (isTRUE(digits %% 1L != 0L || digits < 0L || digits == 0L)) { stop("Specify a positive integer number for the argument 'digits'.", call. = FALSE) }

    # Check input 'p.digits'
    if (isTRUE(p.digits %% 1L != 0L || p.digits < 0L)) { stop("Specify a positive integer number for the argument 'p.digits'.", call. = FALSE) }

    # Check input 'append'
    if (isTRUE(!is.logical(append))) { stop("Please specify TRUE or FALSE for the argument 'append'.", call. = FALSE) }

    ## Check input 'output' ##
    if (isTRUE(!is.logical(output))) { stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE) }

  }

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
  # Internal Functions ---------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Making Sandwiches with Bread and Meat ####

  sandw <- function(x, type = c("HC0", "HC1", "HC2", "HC3", "HC4", "HC4m", "HC5")) {

    # Hat values
    diaghat <- try(hatvalues(x), silent = TRUE)

    # Specify omega function
    switch(type,
           const = { omega <- function(residuals, diaghat, df) rep(1, length(residuals)) * sum(residuals^2L) / df },
           HC0   = { omega <- function(residuals, diaghat, df) residuals^2L },
           HC1   = { omega <- function(residuals, diaghat, df) residuals^2L * length(residuals)/df },
           HC2   = { omega <- function(residuals, diaghat, df) residuals^2L / (1L - diaghat) },
           HC3   = { omega <- function(residuals, diaghat, df) residuals^2L / (1L - diaghat)^2L },
           HC4   = { omega <- function(residuals, diaghat, df) {
             n <- length(residuals)
             p <- as.integer(round(sum(diaghat),  digits = 0L))
             delta <- pmin(4L, n * diaghat/p)
             residuals^2L / (1L - diaghat)^delta
           }},
           HC4m  = { omega <- function(residuals, diaghat, df) {
             gamma <- c(1.0, 1.5) ## as recommended by Cribari-Neto & Da Silva
             n <- length(residuals)
             p <- as.integer(round(sum(diaghat)))
             delta <- pmin(gamma[1L], n * diaghat/p) + pmin(gamma[2L], n * diaghat/p)
             residuals^2L / (1L - diaghat)^delta
           }},
           HC5   = { omega <- function(residuals, diaghat, df) {
             k <- 0.7 ## as recommended by Cribari-Neto et al.
             n <- length(residuals)
             p <- as.integer(round(sum(diaghat)))
             delta <- pmin(n * diaghat / p, pmax(4L, n * k * max(diaghat) / p))
             residuals^2L / sqrt((1L - diaghat)^delta)
           }})

    if (isTRUE(type %in% c("HC2", "HC3", "HC4", "HC4m", "HC5"))) {

      if (isTRUE(inherits(diaghat, "try-error"))) stop(sprintf("hatvalues() could not be extracted successfully but are needed for %s", type), call. = FALSE)

      id <- which(diaghat > 1L - sqrt(.Machine$double.eps))

      if(length(id) > 0L) {

        id <- if (isTRUE(is.null(rownames(X)))) { as.character(id) } else { rownames(X)[id] }

        if(length(id) > 10L) id <- c(id[1L:10L], "...")

        warning(sprintf("%s covariances become numerically unstable if hat values are close to 1 as for observations %s", type, paste(id, collapse = ", ")), call. = FALSE)

      }

    }

    # Ensure that NAs are omitted
    if(is.list(x) && !is.null(x$na.action)) class(x$na.action) <- "omit"

    # Extract design matrix
    X <- model.matrix(x)
    if (isTRUE(any(alias <- is.na(coef(x))))) X <- X[, !alias, drop = FALSE]

    # Number of observations
    n <- NROW(X)

    # Generalized Linear Model
    if (isTRUE(inherits(x, "glm"))) {

      wres <- as.vector(residuals(x, "working")) * weights(x, "working")
      dispersion <- if (isTRUE(substr(x$family$family, 1L, 17L) %in% c("poisson", "binomial", "Negative Binomial"))) { 1L } else { sum(wres^2L, na.rm = TRUE) / sum(weights(x, "working"), na.rm = TRUE) }

      ef <- wres * X / dispersion

      # Linear Model
    } else {

      # Weights
      wts <- if (isTRUE(is.null(weights(x)))) { 1L } else { weights(x) }
      ef <- as.vector(residuals(x)) * wts * X

    }

    # Meat
    meat <- crossprod(sqrt(omega(rowMeans(ef / X, na.rm = TRUE), diaghat, n - NCOL(X))) * X) / n

    # Bread
    sx <- summary.lm(x)
    bread <- sx$cov.unscaled * as.vector(sum(sx$df[1L:2L]))

    # Sandwich
    sandw <- 1L / n * (bread %*% meat %*% bread)

    return(invisible(sandw))

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Inference for Estimated Coefficients ####

  coeftest <- function(x, vcov = NULL) {

    # Extract coefficients and standard errors
    est <- coef(x)
    se <- sqrt(diag(vcov))

    ## match using names and compute t/z statistics
    if (isTRUE(!is.null(names(est)) && !is.null(names(se)))) {

      if (length(unique(names(est))) == length(names(est)) && length(unique(names(se))) == length(names(se))) {

        anames <- names(est)[names(est) %in% names(se)]
        est <- est[anames]
        se <- se[anames]

      }

    }

    # Test statistic
    stat <- as.vector(est) / se

    df <- try(df.residual(x), silent = TRUE)

    # Generalized Linear Model
    if (isTRUE(inherits(x, "glm"))) {

      pval <- 2L * pnorm(abs(stat), lower.tail = FALSE)
      cnames <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
      mthd <- "z"

      # Linear Model
    } else {

      pval <- 2L * pt(abs(stat), df = df, lower.tail = FALSE)
      cnames <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
      mthd <- "t"

    }

    object <- cbind(est, se, stat, pval)
    colnames(object) <- cnames

    return(invisible(object))

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Wald Test of Nested Models ####

  waldtest <- function(object, ..., vcov = NULL, name = NULL) {

    coef0 <- function(x, ...) { na.omit(coef(x, ...)) }

    nobs0 <- function(x, ...) {

      nobs1 <- nobs
      nobs2 <- function(x, ...) { NROW(residuals(x, ...)) }

      object <- try(nobs1(x, ...), silent = TRUE)

      if (isTRUE(inherits(object, "try-error") | is.null(object))) object <- nobs2(x, ...)

      return(object)

    }

    df.residual0 <- function(x) {

      df <- try(df.residual(x), silent = TRUE)

      if (isTRUE(inherits(df, "try-error") | is.null(df))) { df <- try(nobs0(x) - attr(logLik(x), "df"), silent = TRUE) }
      if (isTRUE(inherits(df, "try-error") | is.null(df))) { df <- try(nobs0(x) - length(as.vector(coef0(x))), silent = TRUE) }
      if (isTRUE(inherits(df, "try-error"))) df <- NULL

      return(df)

    }

    cls <- class(object)[1L]

    # 1. Extracts term labels
    tlab <- function(x) {

      tt <- try(terms(x), silent = TRUE)
      if (isTRUE(inherits(tt, "try-error"))) "" else attr(tt, "term.labels")

    }

    # 2. Extracts model name
    if (isTRUE(is.null(name))) name <- function(x) {

      object <- try(formula(x), silent = TRUE)

      if (isTRUE(inherits(object, "try-error") | is.null(object))) { object <- try(x$call, silent = TRUE) }
      if (isTRUE(inherits(object, "try-error") | is.null(object))) { return(NULL) } else { return(paste(deparse(object), collapse="\n")) }

    }

    # 3. Compute an updated model object
    modelUpdate <- function(fm, update) {

      if (isTRUE(is.numeric(update))) {

        if (isTRUE(any(update < 1L))) {

          warning("For numeric model specifications all values have to be >= 1", call. = FALSE)
          update <- abs(update)[abs(update) > 0L]

        }

        if (isTRUE(any(update > length(tlab(fm))))) {

          warning(paste("More terms specified than existent in the model:", paste(as.character(update[update > length(tlab(fm))]), collapse = ", ")), call. = FALSE)
          update <- update[update <= length(tlab(fm))]

        }

        update <- tlab(fm)[update]

      }

      if (isTRUE(is.character(update))) {

        if (isTRUE(!all(update %in% tlab(fm)))) {

          warning(paste("Terms specified that are not in the model:", paste(dQuote(update[!(update %in% tlab(fm))]), collapse = ", ")), call. = FALSE)
          update <- update[update %in% tlab(fm)]

        }

        if (isTRUE(length(update) < 1L)) { stop("Empty model specification", call. = FALSE)  }
        update <- as.formula(paste(". ~ . -", paste(update, collapse = " - ")))

      }

      if (isTRUE(inherits(update, "formula"))) {

        update <- update(fm, update, evaluate = FALSE)
        update <- eval(update, parent.frame(3))

      }

      if (isTRUE(!inherits(update, cls))) { stop(paste("Original model was of class \"", cls, "\", updated model is of class \"", class(update)[1], "\"", sep = ""), call. = FALSE) }

      return(update)

    }

    # 4. Compare two fitted model objects
    modelCompare <- function(fm, fm.up, vfun = NULL) {

      q <- length(coef0(fm)) - length(coef0(fm.up))

      if (isTRUE(q > 0L)) {

        fm0 <- fm.up
        fm1 <- fm

      } else {

        fm0 <- fm
        fm1 <- fm.up

      }

      k <- length(coef0(fm1))
      n <- nobs0(fm1)

      # Determine omitted variables
      if (isTRUE(!all(tlab(fm0) %in% tlab(fm1)))) { stop("Nesting of models cannot be determined", call. = FALSE) }

      ovar <- which(!(names(coef0(fm1)) %in% names(coef0(fm0))))

      if (isTRUE(abs(q) != length(ovar))) { stop("Nesting of models cannot be determined", call. = FALSE) }

      # Get covariance matrix estimate
      vc <- if (isTRUE(is.null(vfun))) { vcov(fm1) } else if (isTRUE(is.function(vfun))) { vfun(fm1) } else { vfun }

      ## Compute Chisq statistic
      stat <- t(coef0(fm1)[ovar]) %*% solve(vc[ovar,ovar]) %*% coef0(fm1)[ovar]

      return(c(-q, stat))

    }

    # Recursively fit all objects
    objects <- list(object, ...)
    nmodels <- length(objects)

    if (isTRUE(nmodels < 2L)) {

      objects <- c(objects, . ~ 1)
      nmodels <- 2L

    }

    # Remember which models are already fitted
    no.update <- sapply(objects, function(obj) inherits(obj, cls))

    # Updating
    for(i in 2L:nmodels) objects[[i]] <- modelUpdate(objects[[i - 1L]], objects[[i]])

    # Check responses
    getresponse <- function(x) {

      tt <- try(terms(x), silent = TRUE)
      if (isTRUE(inherits(tt, "try-error"))) { "" } else { deparse(tt[[2L]]) }

    }

    responses <- as.character(lapply(objects, getresponse))
    sameresp <- responses == responses[1L]

    if (isTRUE(!all(sameresp))) {

      objects <- objects[sameresp]
      warning("Models with response ", deparse(responses[!sameresp]), " removed because response differs from ", "model 1", call. = FALSE)

    }

    # Check sample sizes
    ns <- sapply(objects, nobs0)
    if (isTRUE(any(ns != ns[1L]))) {

      for(i in 2L:nmodels) {

        if (isTRUE(ns[1L] != ns[i])) {

          if (isTRUE(no.update[i])) { stop("Models were not all fitted to the same size of dataset")

          } else {

            commonobs <- row.names(model.frame(objects[[i]])) %in% row.names(model.frame(objects[[i - 1L]]))
            objects[[i]] <- eval(substitute(update(objects[[i]], subset = commonobs), list(commonobs = commonobs)))

            if (isTRUE(nobs0(objects[[i]]) != ns[1L])) { stop("Models could not be fitted to the same size of dataset", call. = FALSE) }

          }

        }

      }

    }

    #  ANOVA matrix
    object <- matrix(rep(NA, 4L * nmodels), ncol = 4L)

    colnames(object) <- c("Res.Df", "Df", "F", "pval")
    rownames(object) <- 1L:nmodels

    object[, 1L] <- as.numeric(sapply(objects, df.residual0))
    for(i in 2L:nmodels) object[i, 2L:3L] <- modelCompare(objects[[i - 1L]], objects[[i]], vfun = vcov)

    df <- object[, 1L]
    for(i in 2L:nmodels) if (isTRUE(object[i, 2L] < 0L)) { df[i] <- object[i - 1L, 1L] }
    object[, 3L] <- object[, 3L] / abs(object[, 2L])
    object[, 4L] <- pf(object[, 3L], abs(object[, 2L]), df, lower.tail = FALSE)

    variables <- lapply(objects, name)
    if (isTRUE(any(sapply(variables, is.null)))) { variables <- lapply(match.call()[-1L], deparse)[1L:nmodels] }

    return(invisible(object))

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  # Sandwich
  sandw <- sandw(model, type = type)

  # Inference for estimated coefficients
  coef.res <- coeftest(model, vcov = sandw)

  # Linear model
  F.test <- NULL
  if (isTRUE(length(class(model)) == 1L)) {

    # Waldtest
    F.test <- waldtest(update(model, formula = ~ 1), model, vcov = sandw)

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

  if (isTRUE(!is.null(write))) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Text file ####

    if (isTRUE(grepl("\\.txt", write))) {

      # Send R output to textfile
      sink(file = write, append = ifelse(isTRUE(file.exists(write)), append, FALSE), type = "output", split = FALSE)

      if (append && isTRUE(file.exists(write))) { write("", file = write, append = TRUE) }

      # Print object
      print(object, check = FALSE)

      # Close file connection
      sink()

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ## Excel file ####

    } else {

      misty::write.result(object, file = write)

    }

  }
  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
