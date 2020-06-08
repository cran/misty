#' Collinearity Diagnostics
#'
#' This function computes tolerance, standard error inflation factor, variance inflation factor, eigenvalues,
#' condition index, and variance proportions for linear, generalized linear, and mixed-effects models.
#'
#' Collinearity diagnostics can be conducted for objects returned from the \code{lm()} and \code{glm()}
#' function, but also from objects returned from the \code{lmer()} and \code{glmer()} function from
#' the \pkg{lme4} package, \code{lme()} function from the \pkg{nlme} package, and the \code{glmmTMB()}
#' function from the \pkg{glmmTMB} package.
#'
#' The generalized variance inflation factor (Fox & Monette, 1992) is computed for terms with more than
#' 1 df resulting from factors with more than two levels. The generalized VIF (GVIF) is interpretable
#' as the inflation in size of the confidence ellipse or ellipsoid for the coefficients of the term
#' in comparison with what would be obtained for orthogonal data. GVIF is invariant to the coding of
#' the terms in the model. In order to adjust for the dimension of the confidence ellipsoid,
#' GVIF\eqn{^\frac{1}{2df}} is computed. Note that the adjusted GVIF (aGVIF) is actually a generalized
#' standard error inflation factor (GSIF). Thus, the aGIF needs to be squared before applying a common
#' cutoff threshold for the VIF (e.g., VIF > 10). Note that the output of \code{collin.diag()} function
#' reports either the variance inflation factor or the squared generalized variance inflation factor
#' in the column \code{VIF}, while the standard error inflation factor or the adjusted generalized
#' variance inflation factor is reported in the column \code{SIF}.
#'
#' Note that the computation of the VIF and the GVIF is based on the \code{vif()} function in the
#' \pkg{car} package by John Fox, Sanford Weisberg and Brad Price (2020), and the computation
#' of eigenvalues, condition index, and variance proportions is based on the \code{ols_eigen_cindex()}
#' function in the \pkg{olsrr} package by Aravind Hebbali (2020).
#'
#' @param model    a fitted model of class \code{"lm"}, \code{"glm"}, \code{"lmerMod"}, \code{"lmerModLmerTest"},
#'                 \code{"glmerMod"}, \code{"lme"}, or \code{"glmmTMB"}.
#' @param print    a character vector indicating which results to show, i.e. \code{"all"}, for all results,
#'                 \code{"vif"} for tolerance, std. error inflation factor, and variance inflation factor,
#'                 or \code{eigen} for eigenvalue, condition index, and variance proportions.
#' @param digits   an integer value indicating the number of decimal places to be used for displaying
#'                 results.
#' @param p.digits an integer value indicating the number of decimal places to be used for displaying the
#'                 \emph{p}-value.#' @param check  logical: if \code{TRUE}, argument specification is checked.
#' @param output   logical: if \code{TRUE}, output is shown on the console.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @references
#' Fox, J., & Monette, G. (1992). Generalized collinearity diagnostics.
#' \emph{Journal of the Americaln Statistical Association, 87}, 178-183.
#'
#' Fox, J., Weisberg, S., & Price, B. (2020). \emph{car: Companion to Applied Regression}.
#' R package version 3.0-8. https://cran.r-project.org/web/packages/car/
#'
#' Hebbali, A. (2020). \emph{olsrr: Tools for building OLS regression models}.
#' R package version 0.5.3. https://cran.r-project.org/web/packages/olsrr/
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following entries:
#' function call (\code{call}), type of analysis \code{type}, model specified in the
#' \code{model} argument (\code{model}), specification of function arguments (\code{args}),
#' list with results (\code{result}).
#'
#' @export
#'
#' @examples
#' dat <- data.frame(group = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4),
#'                   x1 = c(3, 2, 4, 9, 5, 3, 6, 4, 5, 6, 3, 5),
#'                   x2 = c(1, 4, 3, 1, 2, 4, 3, 5, 1, 7, 8, 7),
#'                   x3 = c(7, 3, 4, 2, 5, 6, 4, 2, 3, 5, 2, 8),
#'                   x4 = c("a", "b", "a", "c", "c", "c", "a", "b", "b", "c", "a", "c"),
#'                   y1 = c(2, 7, 4, 4, 7, 8, 4, 2, 5, 1, 3, 8),
#'                   y2 = c(0, 1, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1),
#'                   stringsAsFactors = TRUE)
#'
#' #----------------------------
#' # Linear model
#'
#' # Estimate linear model with continuous predictors
#' mod.lm1 <- lm(y1 ~ x1 + x2 + x3, data = dat)
#'
#' # Tolerance, std. error, and variance inflation factor
#' collin.diag(mod.lm1)
#'
#' # Tolerance, std. error, and variance inflation factor
#' # Eigenvalue, Condition index, and variance proportions
#' collin.diag(mod.lm1, print = "all")
#'
#' # Estimate model with continuous and categorical predictors
#' mod.lm2 <- lm(y1 ~ x1 + x2 + x3 + x4, data = dat)
#'
#' # Tolerance, generalized std. error, and variance inflation factor
#' collin.diag(mod.lm2)
#'
#' #----------------------------
#' # Generalized linear model
#'
#' # Estimate logistic regression model with continuous predictors
#' mod.glm <- glm(y2 ~ x1 + x2 + x3, data = dat, family = "binomial")
#'
#' # Tolerance, std. error, and variance inflation factor
#' collin.diag(mod.glm)
#'
#' \dontrun{
#' #----------------------------
#' # Linear mixed-effects model
#'
#' # Estimate linear mixed-effets model with continuous predictors using lme4 package
#' mod.lmer <- lme4::lmer(y1 ~ x1 + x2 + x3 + (1|group), data = dat)
#'
#' # Tolerance, std. error, and variance inflation factor
#' collin.diag(mod.lmer)
#'
#' # Estimate linear mixed-effets model with continuous predictors using nlme package
#' mod.lme <- nlme::lme(y1 ~ x1 + x2 + x3, random = ~ 1 | group, data = dat)
#'
#' # Tolerance, std. error, and variance inflation factor
#' collin.diag(mod.lme)
#'
#' # Estimate linear mixed-effets model with continuous predictors using glmmTMB package
#' mod.glmmTMB1 <- glmmTMB::glmmTMB(y1 ~ x1 + x2 + x3 + (1|group), data = dat)
#'
#' # Tolerance, std. error, and variance inflation factor
#' collin.diag(mod.glmmTMB1)
#' }
#'
#' #----------------------------
#' # Generalized linear mixed-effects model
#'
#' # Estimate mixed-effects logistic regression model with continuous predictors using lme4 package
#' mod.glmer <- lme4::glmer(y2 ~ x1 + x2 + x3 + (1|group), data = dat, family = "binomial")
#'
#' # Tolerance, std. error, and variance inflation factor
#' collin.diag(mod.glmer)
#'
#' # Estimate mixed-effects logistic regression model with continuous predictors using glmmTMB package
#' mod.glmmTMB2 <- glmmTMB::glmmTMB(y2 ~ x1 + x2 + x3 + (1|group), data = dat, family = "binomial")
#'
#' # Tolerance, std. error, and variance inflation factor
#' collin.diag(mod.glmmTMB2)
#' }
collin.diag  <- function(model, print = c("all", "vif", "eigen"),
                         digits = 3, p.digits = 3, check = TRUE, output = TRUE) {

  ####################################################################################
  # Input check

  #......
  # Check if input 'model' is missing
  if (missing(model)) {

    stop("Input for the argument 'model' is missing.", call. = FALSE)

  }

  #......
  # Check if input 'model' is NULL
  if (is.null(model)) {

    stop("Input specified for the argument 'model' is NULL.", call. = FALSE)

  }

  #......
  # Check if input 'model' is NULL
  if (!all(class(model) %in% c("lm", "glm", "lmerMod", "lmerModLmerTest", "glmerMod", "lme", "glmmTMB"))) {

    stop("Please specify an \"lm\", \"glm\", \"lmerMod\", \"lmerModLmerTest\", \"glmerMod\", \"lme\", or \"glmmTMB\" object for the argument 'model'.",
         call. = FALSE)

  }

  #......
  # Check if model has more than one predictor variable
  if (length(labels(terms(model))) < 2L) {

    stop("Please specify a model with more than one predictor variable.", call. = FALSE)

  }

  #.............
  # Check input 'check'
  if (!is.logical(check)) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }

  #-----------------------------------------

  if (isTRUE(check)) {

    #......
    # Check input 'print'
    if (!all(print %in% c("all", "vif", "eigen"))) {

      stop("Character strings in the argument 'print' do not all match with \"all\", \"vif\", or \"eigen\".",
           call. = FALSE)

    }

    #......
    # Check input 'digits'
    if (digits %% 1 != 0L || digits < 0L) {

      stop("Specify a positive integer number for the argument 'digits'.", call. = FALSE)

    }

    #......
    # Check input 'p.digits'
    if (p.digits %% 1 != 0L || p.digits < 0L) {

      stop("Specify a positive integer number for the argument 'p.digits'.", call. = FALSE)

    }

    #......
    # Check input 'output'
    if (!is.logical(output)) {

      stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE)

    }

  }

  #----------------------------------------
  # Print variance inflation factor and/or eigenvalues

  if (all(c("all", "vif", "eigen") %in% print)) { print <- "vif" }

  if (length(print) == 1L && "all" %in% print) { print <- c("vif", "eigen") }

  ####################################################################################
  # Main function

  #----------------------------------------------------------------
  # Variance inflation factor

  #..................
  # Class: lm or glm
  if (all(class(model) %in% c("lm", "glm"))) {

    # Regression model with intercept
    if ("(Intercept)" %in% names(coefficients(model))) {

      intercept <- TRUE

      R <- cov2cor(vcov(model)[-1, -1])
      assign <- attr(model.matrix(model, data = model$model), "assign")[-1L]

      # Regression model without intercept
    } else {

      intercept <- FALSE

      R <- cov2cor(vcov(model))
      assign <- attr(model.matrix(model), "assign")

    }

    #..................
    # Class: lmerMod, lmerModLmerTest or glmerMod
  } else if (all(class(model) %in% c("lmerMod", "lmerModLmerTest", "glmerMod", "lme"))) {

    # Regression model with intercept
    if ("(Intercept)" %in% names(lme4::fixef(model))) {

      intercept <- TRUE

      R <- cov2cor(as.matrix(vcov(model)[-1L, -1L]))
      assign <- attr(model.matrix(model, data = model$data), "assign")[-1L]

      # Regression model without intercept
    } else {

      intercept <- FALSE

      R <- cov2cor(vcov(model))
      assign <- attr(model.matrix(model, data = model$data), "assign")

    }

    #..................
    # Class: glmmTMB
  } else if (all(class(model) %in% "glmmTMB")) {

    # Regression model with intercept
    if ("(Intercept)" %in% names(lme4::fixef(model)$cond)) {

      intercept <- TRUE

      R <- cov2cor(vcov(model)$cond[-1L, -1L])
      assign <- attr(model.matrix(model), "assign")[-1L]

      # Regression model without intercept
    } else {

      intercept <- FALSE

      R <- cov2cor(vcov(model)$cond)
      assign <- attr(model.matrix(model), "assign")

    }

  }

  #..................
  # Warning message: Model without intercept
  if (isFALSE(intercept) && "vif" %in% print) {

    warning("Variance inflation factor might not be sensible in models without an intercept.",
            call. = FALSE)

  }

  terms <- labels(terms(model))

  # Determinant of the Matrix
  det.R <- det(R)

  # Result object
  vif <- data.frame(matrix(0, nrow = length(terms), ncol = 5L,
                           dimnames = list(terms,  c("Tol", "df", "GVIF", "aGSIF", "aGVIF"))),
                    stringsAsFactors = FALSE)

  for (i in seq_along(terms)) {

    subs <- which(assign == i)

    # Degrees of freedom
    vif[i, "df"] <- length(subs)

    # Generalized standard error inflation factor
    vif[i, "GVIF"] <- det(as.matrix(R[subs, subs])) * det(as.matrix(R[-subs, -subs])) / det.R

    # Generalized standard error inflation factor made compareable
    vif[i, "aGSIF"] <- vif[i, "GVIF"]^(1L / (2L * length(subs)))

    # Generalized variance inflation factor made compareable
    vif[i, "aGVIF"] <- vif[i, "aGSIF"]^2L

    # Tolerance statistic
    vif[i, "Tol"] <- 1L / vif[i, "aGVIF"]

  }

  #----------------------------------------------------------------
  # Eigenvalue and Condition Index

  if (all(class(model) %in% c("lmerMod", "lmerModLmerTest", "glmerMod", "lme", "glmmTMB"))) {

    z <- scale(as.data.frame(model.matrix(model, data = model$data), stringsAsFactors = FALSE),
               center = FALSE, scale = TRUE)

  } else if (all(class(model) %in% c("lm", "glm"))) {

    z <- scale(as.data.frame(model.matrix(model, data = model$model), stringsAsFactors = FALSE),
               center = FALSE, scale = TRUE)

  }

  # Eigenvalue
  e <- eigen(t(z) %*% z / diag(t(z) %*% z))$values

  # Condition index
  ci <- sqrt(e[1L] / e)

  # Singular value decomposition
  svdx  <- svd(z)

  ph <- t((svdx$v %*% diag(1L / svdx$d))^2L)

  # Regression coefficient variance-decomposition matrix
  vd <- prop.table(ph %*% diag(rowSums(ph, dims = 1L)), margin = 2L)

  eigenvalue <- data.frame(1:length(e), e, ci, vd, stringsAsFactors = FALSE)
  colnames(eigenvalue) <- c("dim", "eigen", "ci", attributes(z)$dimnames[[2L]])

  ####################################################################################
  # Return object

  #----------------------------------------
  # Regression coefficients with VIF

  #....................
  # Regression coefficients
  if (all(class(model) %in% c("lm", "glm", "lmerMod", "lmerModLmerTest", "glmerMod"))) {

    coeff <- summary(model)$coefficients

  } else if (any(class(model) == "lme")) {

    coeff <- summary(model)$tTable

  } else if (any(class(model) == "glmmTMB")) {

    coeff <- summary(model)$coefficients$cond

  }

  #....................
  # Match VIF with coefficients
  vif.list <- sapply(row.names(vif), function(y) which(substr(row.names(coeff), 1L, nchar(y)) == y))

  coeff.vif <- matrix(NA, ncol = 5L, nrow = nrow(coeff),
                      dimnames = list(row.names(coeff), colnames(vif)))
  for (i in seq_along(vif.list)) {

    coeff.vif[vif.list[[i]], ] <- as.matrix(vif[row.names(vif) == names(vif.list)[i], ][rep(1L, times = length(vif.list[[i]])), ])

  }

  coeff <- data.frame(cbind(coeff, coeff.vif), check.names = FALSE, stringsAsFactors = FALSE)

  #----------------------------------------
  # Return object

  object <- list(call = match.call(),
                 type = "collin.diag",
                 model = model,
                 args = list(print = print, digits = digits, p.digits = p.digits,
                             check = check, output = output),
                 result = list(coef = coeff, vif = vif, eigen = eigenvalue))

  class(object) <- "misty.object"

  ####################################################################################
  # Output

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
