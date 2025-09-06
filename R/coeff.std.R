#' Standardized Coefficients for Linear, Multilevel and Mixed-Effects Models
#'
#' This function computes standardized coefficients for linear models estimated
#' by using the \code{lm()} function and for multilevel and linear mixed-effects
#' models estimated by using the \code{lmer()} or \code{lme()} function from the
#' \pkg{lme4} or \pkg{nlme} package.
#'
#' @param model    a fitted model of class \code{"lm"}, \code{"lmerMod"},
#'                 \code{"lmerModLmerTest"} or \code{"lme"}.
#' @param print    a character vector indicating which results to print, i.e.
#'                 \code{"all"}, for all results, \code{"stdx"} for standardizing
#'                 only the predictor, \code{"stdy"} for for standardizing only
#'                 the criterion, and \code{"stdyx"} for for standardizing both
#'                 the predictor and the criterion. Note that the default setting
#'                 is depending on the level of measurement of the predictors,
#'                 i.e., if all predictors are continuous, the default setting
#'                 is \code{print = "stdyx"}; if all predictors are binary, the
#'                 default setting is \code{print = "stdy"}, and if predictors
#'                 are continuous and binary, the default setting is
#'                 \code{print = c("stdy", "stdyx")}.
#' @param digits   an integer value indicating the number of decimal places to
#'                 be used for displaying
#'                 results.
#' @param p.digits an integer value indicating the number of decimal places to be
#'                 used for displaying the \emph{p}-value.
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
#' @param check    logical: if \code{TRUE} (default), argument specification is
#'                 checked.
#' @param output   logical: if \code{TRUE} (default), output is shown on the console.
#'
#' @details
#' \describe{
#' \item{\strong{Linear Regression Model}}{The linear regression model is expressed
#' as follows:
#'
#' \deqn{y_i = \beta_0 + \beta_1x_i + \epsilon_i}
#'
#' where \eqn{y_i} is the outcome variable for individual \eqn{i}, \eqn{\beta_0}
#' is the intercept, \eqn{\beta_1} is the slope (aka regression coefficient),
#' \eqn{x_i} is the predictor for individual \eqn{i}, and \eqn{\epsilon_i} is the
#' residual for individual \eqn{i}.
#'
#' The slope \eqn{\beta_1} estimated by using the \code{lm()} function can be
#' standardized with respect to only \eqn{x}, only \eqn{y}, or both \eqn{y} and
#' \eqn{x}:
#'
#'    \itemize{
#'      \item{\strong{StdX Standardization}}: \eqn{StdX(\beta_1)}
#'      standardizes with respect to \eqn{x} only and is interpreted as expected
#'      difference in \eqn{y} between individuals that differ one standard
#'      deviation referred to as \eqn{SD(x)}:
#'
#'      \deqn{StdX(\beta_1) = \beta_1 SD(x)}
#'
#'      \item{\strong{StdY Standardization}}: \eqn{StdY(\beta_1)}
#'      standardizes with respect to \eqn{y} only and is interpreted as expected
#'      difference in \eqn{y} standard deviation units, referred to as \eqn{SD(y)},
#'      between individuals that differ one unit in \eqn{x}:
#'
#'      \deqn{StdY(\beta_1) = \frac{\beta_1}{SD(x)}}
#'
#'      \item{\strong{StdYX Standardization}}: \eqn{StdYX(\beta_1)}
#'      standardizes with respect to both \eqn{y} and \eqn{x} and is interpreted
#'      as expected difference in \eqn{y} standard deviation units between individuals
#'      that differ one standard deviation in \eqn{x}:
#'
#'      \deqn{StdYX(\beta_1) = \beta_1 \frac{SD(x)}{SD(y)}}
#'    }
#' Note that the \eqn{StdYX(\beta_1)} and the \eqn{StdY(\beta_1)} standardizations
#' are not suitable for the slope of a binary predictor because a one standard
#' deviation change in a binary variable is generally not of interest (Muthen et
#' al, 2016). Accordingly, the function does not provide the \eqn{StdYX(\beta_1)}
#' and the \eqn{StdY(\beta_1)} standardizations whenever a binary vector, factor,
#' or character vector is specified for the predictor variable.}
#'
#' \item{\strong{Moderated Regression Model}}{The moderated regression model is
#' expressed as follows:
#'
#' \deqn{y_i = \beta_0 + \beta_1x_{1i} + \beta_2x_{2i} + \beta_3x_{1i}x_{2i} + \epsilon_i}
#'
#' where \eqn{\beta_3} is the slope for the interaction variable \eqn{x_1x_2}.
#'
#' The slope \eqn{\beta_3} is standardized by using the product of standard
#' deviations \eqn{SD(x_1)SD(x_2)} rather than the standard deviation of the
#' product \eqn{SD(x_1 x_2)} for the interaction variable \eqn{x_1x_2} as
#' discussed in Wen et al. (2010).
#'
#' Note that the function does not use binary variables in the interaction term
#' in standardizing the interaction variable. For example, when standardizing the
#' interaction term \eqn{x1:x2:x3} with \eqn{x2} being binary, the product
#' \eqn{SD(x_1)SD(x_3)} while excluding binary predictor \code{x2} is used to
#' standardize the interaction term.}
#'
#' \item{\strong{Polynomial Regression Model}}{The polynomial regression model
#' is expressed as follows:
#'
#' \deqn{y_i = \beta_0 + \beta_1x_{i} + \beta_2x^2_{i} + \epsilon_i}
#'
#' where \eqn{\beta_2} is the slope for the quadratic term \eqn{x^2}.
#'
#' The slope \eqn{\beta_3} is standardized by using the product of standard
#' deviations \eqn{SD(x)SD(x)} rather than the standard deviation of the product
#' \eqn{SD(x x)} for the quadratic term \eqn{x^2}.}
#'
#' \item{\strong{Multilevel and Mixed-Effects Model}}{The random intercept and
#' slope model in the multiple-equation notation is expressed as follows:
#'
#' \itemize{
#'    \item{Level 1:} \deqn{y_{ij} = \beta_{0j} + \beta_{1j}x_{ij} + r_{ij}}
#'    \item{Level 2:} \deqn{\beta_{0j} = \gamma_{00} + \gamma_{01}z_{j} + u_{0j}}
#'                    \deqn{\beta_{1j} = \gamma_{10} + u_{1j}}
#' }
#'
#' The model expressed in the single-equation notation is as follows:
#'
#' \deqn{y_{ij} = \gamma_{00} + \gamma_{10}x_{ij} + \gamma_{01}z_{j} + u_{0j} + u_{1j}x_{ij} + r_{ij}}
#'
#' where \eqn{y_{ij}} is the outcome variable for individual \eqn{i} in group \eqn{j},
#' \eqn{\gamma_{00}} is the fixed-effect average intercept, \eqn{\gamma_{10}} is the
#' fixed-effect average slope for the Level-1 predictor \eqn{x}, and \eqn{\gamma_{01}}
#' is the fixed-effect slope for the Level-2 predictor \eqn{z}.
#'
#' The slopes \eqn{\gamma_{10}} and \eqn{\gamma_{01}} are standardized according
#' to the within- and between-group or within-and between-person standard deviations,
#' i.e., slopes are standardizes with respect to the \eqn{x} and \eqn{y} standard
#' deviation relevant for the level of the fixed effect of interest. The resulting
#' standardized slopes are called pseudo-standardized coefficients (Hoffman 2015,
#' p. 342). The StdYX Standardization for \eqn{\gamma_{10}} and \eqn{\gamma_{10}}
#' is expressed as follows:
#'
#' \itemize{
#'   \item{Level-1 Predictor:} \deqn{StdYX(\gamma_{10}) = \gamma_{10} \frac{SD(x_{ij})}{SD(y_{ij})}}
#'   \item{Level-2 Predictor:} \deqn{StdYX(\gamma_{01}) = \gamma_{01} \frac{SD(x_{j})}{SD(y_{j})}}
#' }
#'
#' where \eqn{SD(x_{ij})} and \eqn{SD(x_{j})} are the standard deviations of the
#' predictors at each analytic level, \eqn{SD(y_{ij})} is the square root of the
#' Level-1 residual variance \eqn{\sigma^2_{r}} and \eqn{SD(y_{j})} is square root
#' of the Level-2 intercept variance \eqn{\sigma^2_{u_0}} which are estimated in
#' a null model using the \code{lmer} function in the \pkg{lme4} package using
#' the restricted maximum likelihood estimation method.
#'
#' The function uses the square root of the Level-1 residual variance \eqn{\sigma^2_{r}}
#' to standardize the slope of the cross-level interaction though it should be
#' noted that it is unclear whether this is the correct approach to standardize
#' the slope of the cross-level interaction.}
#' }
#' }
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @references
#' Hoffman, L. (2015). \emph{Longitudinal Analysis: Modeling Within-Person Fluctuation
#' and Change}. Routledge.
#'
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
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{data}}{data frame with variables used in the analysis}
#' \item{\code{model}}{model specified in \code{model} }
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{result}}{list with result tables, i.e., \code{coef} for the regression
#'                      table including standardized coefficients and \code{sd}
#'                      for the standard deviation of the outcome and predictor(s)}
#'
#' @export
#'
#' @examples
#' #----------------------------------------------------------------------------
#' # Linear Model
#'
#' # Example 1a: Continuous predictors
#' mod.lm1 <- lm(mpg ~ cyl + disp, data = mtcars)
#' coeff.std(mod.lm1)
#'
#' # Example 1b: Print all standardized coefficients
#' coeff.std(mod.lm1, print = "all")
#'
#' # Example 1c: Binary predictor
#' mod.lm2 <- lm(mpg ~ vs, data = mtcars)
#' coeff.std(mod.lm2)
#'
#' # Example 1d: Continuous and binary predictors
#' mod.lm3 <- lm(mpg ~ disp + vs, data = mtcars)
#' coeff.std(mod.lm3)
#'
#' # Example 1e: Continuous predictors with interaction term
#' mod.lm4 <- lm(mpg ~ cyl*disp, data = mtcars)
#' coeff.std(mod.lm4)
#'
#' # Example 1f: Continuous and binary predictor with interaction term
#' mod.lm5 <- lm(mpg ~ cyl*vs, data = mtcars)
#' coeff.std(mod.lm5)
#'
#' # Example 1g: Continuous predictor with a quadratic term
#' mod.lm6 <- lm(mpg ~ cyl + I(cyl^2), data = mtcars)
#' coeff.std(mod.lm6)
#'
#' #----------------------------------------------------------------------------
#' # Multilevel and Linear Mixed-Effects Model
#'
#' # Load lme4 and nlme package
#' misty::libraries(lme4, nlme)
#'
#' # Load data set "Demo.twolevel" in the lavaan package
#' data("Demo.twolevel", package = "lavaan")
#'
#' # Cluster-mean centering, center() from the misty package
#' Demo.twolevel <- center(Demo.twolevel, x2, type = "CWC", cluster = "cluster")
#'
#' # Grand-mean centering, center() from the misty package
#' Demo.twolevel <- center(Demo.twolevel, w1, type = "CGM", cluster = "cluster")
#'
#' # Estimate models using the lme4 package
#' mod1a <- lmer(y1 ~ x2.c + w1.c + (1 + x2.c | cluster), data = Demo.twolevel, REML = FALSE)
#' mod2a <- lmer(y1 ~ x2.c + w1.c + x2.c:w1.c + (1 + x2.c | cluster), data = Demo.twolevel,REML = FALSE)
#'
#' # Estimate models using the nlme package
#' mod1b <- lme(y1 ~ x2.c + w1.c, random = ~ 1 + x2.c | cluster, data = Demo.twolevel, method = "ML")
#' mod2b <- lme(y1 ~ x2.c + w1.c + x2.c:w1.c, random = ~ 1 + x2.c | cluster, data = Demo.twolevel, method = "ML")
#'
#' # Example 2: Continuous predictors
#' coeff.std(mod1a)
#' coeff.std(mod1b)
#'
#' # Example 2: Continuous predictors with cross-level interaction
#' coeff.std(mod2a)
#' coeff.std(mod2b)
#'
#' #----------------------------------------------------------------------------
#' # Example 3: Write Results into a text or Excel file
#'
#' # Example 3a: Text file
#' coeff.std(mod.lm1, write = "Std_Coef.txt", output = FALSE, check = FALSE)
#'
#' # Example 3b: Excel file
#' coeff.std(mod.lm1, write = "Std_Coef.xlsx", output = FALSE, check = FALSE)
coeff.std <- function(model, print = c("all", "stdx", "stdy", "stdyx"),
                      digits = 2, p.digits = 3, write = NULL, append = TRUE,
                      check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'model' is missing or NULL
  if (isTRUE(missing(model) || is.null(model))) { stop("Please specify a fitted model for the argument 'model'.", call. = FALSE) }

  # Check if input 'model' is not 'lm', "lmerMod", "lmerModLmerTest" or "lme"
  if (isTRUE(all(!class(model) %in% c("lm", "lmerMod", "lmerModLmerTest", "lme")))) { stop("Please specify a fitted model object from the \"lm\", \"lmer\", or \"lme\" function for the argument 'model'.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("append", "output"),
               m.character = list(print = c("all", "stdx", "stdy", "stdyx")),
               args = c("digits", "p.digits", "write2"), envir = environment(), input.check = check)

  if (isTRUE(check)) {

    # Functions factor(), as.factor() or as.character() within the R formula
    (if (isTRUE(class(model) == "lm" || class(model) == "lme")) { as.character(model$call) } else { as.character(attr(model, which = "call")) }) |>
      (\(y) y[grep("~", y, fixed = TRUE, useBytes = TRUE)])() |>
      (\(z) if (isTRUE(any(misty::chr.grepl(c("factor\\(", "as.factor\\(", "as.character\\("), z)))) { stop(paste0("This function cannot deal with the \"", names(which(sapply(c("factor", "as.character"), function(v) grepl(v, z)))), "\" function within the R formula."), call. = FALSE) })()

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Linear Model, lm() function ####

  if (isTRUE(class(model) == "lm")) {

    #...................
    ### Predictor and Criterion Info ####

    # Data
    model.data <- model$model

    # Unstandardized slopes
    coeff <- model$coefficients |> (\(y) c(na.omit(y[which(names(y) != "(Intercept)")])))()

    # Criterion variable
    crit.var <- all.vars(model$call)[1L]

    # Predictor variables
    pred.var <- names(coeff)

    # Data classes of predictor variables
    pred.var.class <- attr(terms(model), which = "dataClasses")[-1L]

    # Code factors and attach to data
    if (isTRUE(any(pred.var.class %in% c("factor", "ordered", "character")))) {

      for (i in names(pred.var.class)[pred.var.class %in% c("factor", "ordered", "character")]) {

        model.data <- data.frame(eval(parse(text = paste0(model$contrasts[[i]], "(levels(if (isTRUE(class(model.data[, i])) != \"factor\") { as.factor(model.data[, i]) } else { model.data[, i]}))")))) |>
          (\(y) setNames(y, nm = pred.var |>
                           (\(a) a[which(substr(a, 1L, nchar(i)) == i & substr(a, nchar(i) + 1L, nchar(a)) %in%

                                           # Unordered factor
                                           if (isTRUE(pred.var.class[i] %in% c("factor", "character"))) {

                                             if (isTRUE(is.factor(model.data[, i]))) { levels(model.data[, i]) } else { levels(as.factor(model.data[, i])) }

                                             # Ordered factor
                                           } else if (isTRUE(pred.var.class[i] == "ordered")) {

                                             c(".L", ".Q", ".C", "^4", paste0("^", seq_len(100L)))[seq_len(ncol(y))]

                                           }

                           )])() |>

                           (\(b) if (isTRUE(any(grepl(":", b)))) { b[-grep(":", b)] } else { b })()))() |>
          (\(z) data.frame(by = if (isTRUE(is.factor(model.data[, i]))) { levels(model.data[, i]) } else { levels(as.factor(model.data[, i])) }, z))() |>
          (\(v) data.frame(model.data[, -grep(i, colnames(model.data)), drop = FALSE], v[match(model.data[, i], v[, "by"]), -grep("by", colnames(v)), drop = FALSE]))()

      }

    }

    # Interaction terms
    pred.var.int <- pred.var[grep(":", pred.var)] |>
      # Check if main effects are included in the model
      (\(y) sapply(y, function(z) { unname(unlist(sapply(y, strsplit, ":"))) |>

          (\(v) if (isTRUE(any(sapply(v, function(w) !w %in% colnames(model.data))))) { stop(paste0("The main effect of the predictors needs to be included in the model when investigating interction effects."), call. = FALSE) } else { return(z) })()

      }))()

    # Polynomial terms
    pred.var.poly <- unname(pred.var[grep("^", pred.var, fixed = TRUE)] |>
      # Check if linear term is included in the model
      (\(y) sapply(y, function(z) {

        sub("I\\(", "", unlist(strsplit(z, "^", fixed = TRUE))[1L]) |> (\(v) if (isTRUE(!v %in% colnames(model.data))) { stop(paste0("The linear term of the predictor needs to be included in the model when investigating polynomial effects."), call. = FALSE) } else { return(z) } )()

      }))())

    #...................
    ### Binary Predictor ####
    #
    # 0 = non-binary, 1 = binary predictor including ordered factors with polynomial contrasts

    pred.binary <- sapply(unique(c(setdiff(pred.var, c(pred.var.int, pred.var.poly)), unname(unlist(sapply(pred.var.int, strsplit, ":"))))), function(y) {

      # Ordered factor
      if (isTRUE(any(pred.var.class == "ordered") && y %in% as.vector(sapply(names(pred.var.class), function(z) paste0(z, c(".L", ".Q", ".C", "^4", paste0("^", seq_len(100L)))))) )) {

        return(1L)

      # Numeric or unordered factor
      } else {

        return(as.numeric(misty::uniq.n(model.data[, y]) == 2L))

      }

    })

    #...................
    ### Predictor standard deviations ####

    sd.pred <- sapply(names(coeff), function(y) {

      # Interaction term
      if (isTRUE(y %in% pred.var.int)) {

        # Product of SDs for metric predictor variables, i.e., SD of binary predictors set to 1
        return(prod(model.data[unlist(strsplit(y, ":"))] |> (\(y) sapply(y, function(z) if (isTRUE(misty::uniq.n(z) > 2L)) { sd(z, na.rm = TRUE) } else { 1L } ) )()))

      # Polynomial terms
      } else if (isTRUE(y %in% pred.var.poly)) {

        return(prod(rep(sd(model.data[, sub("I\\(", "", unlist(strsplit(y, "^", fixed = TRUE))[1L])], na.rm = TRUE), times = sub(")", "", unlist(strsplit(y, "^", fixed = TRUE))[2L]))))

      # Numeric predictor
      } else if (isTRUE(y %in% pred.var)) {

        # If predictor is binary, then SD = NA
        return(if (isTRUE(pred.binary[y] == 0L)) { sd(model.data[, y], na.rm = TRUE) } else { NA })

      }

    })

    #...................
    ### Criterion Standard Deviation ####

    sd.crit <- sapply(model.data[, crit.var, drop = FALSE], sd, na.rm = TRUE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Linear Mixed-Effects Model, lmer() or lme() function from the lme4 or nlme package ####

  } else if (isTRUE(class(model) %in% c("lmerMod", "lmerModLmerTest", "lme"))) {

    #...................
    ### Predictor and criterion info ####

    #### lmer() function from the lme4 package ####
    if (isTRUE(class(model) %in% c("lmerMod", "lmerModLmerTest"))) {

      # Check
      if (isTRUE(lme4::getME(model, name = "n_rtrms") != 1L)) { stop("This function supports only two-level models.", call. = FALSE) }

      # Unstandardized slopes
      coeff <- lme4::fixef(model) |> (\(y) y[which(names(y) != "(Intercept)")])() |> (\(z) if (isTRUE(length(z) == 0L)) { stop("There are no predictors specified in the fitted model.", call. = FALSE) } else { return(z) })()

      # Grouping variable
      group.var <- names(lme4::ngrps(model))

      # Criterion variable
      crit.var <- all.vars(attr(model, which = "call"))[1L]

      # Predictor variables
      pred.var <- names(coeff)

      # Data
      model.data <- cbind(attributes(model)$frame[, crit.var, drop = FALSE], as.data.frame(attributes(model)[["pp"]]$X) |> (\(y) y[, colnames(y) != "(Intercept)", drop = FALSE])(), attributes(model)$frame[, group.var, drop = FALSE])

    #### lme() function from the nlme package ####
    } else if (isTRUE(class(model) %in% "lme")) {

      # Check
      if (isTRUE(ncol(model$groups) != 1L)) { stop("This function can only deal with two-level models.", call. = FALSE)}

      # Data
      model.data <- model$data

      # Unstandardized slopes
      coeff <- nlme::fixef(model) |> (\(y) y[which(names(y) != "(Intercept)")])() |> (\(z) if (isTRUE(length(z) == 0L)) { stop("There are no predictors specified in the fitted model.", call. = FALSE) } else { return(z) })()

      # Grouping variable
      group.var <- names(model$groups)

      # Criterion variable
      crit.var <- all.vars(model$call)[1L]

      # Predictor variables
      pred.var <- names(coeff)

      # Data classes of predictor variables
      pred.var.class <- sapply(model.data[, colnames(model.data) |> (\(z) z[z %in% all.vars(model$call) & z != crit.var & z != group.var])(), drop = FALSE], function(y) class(y)[1L])

      # Code factors and attach to data
      if (isTRUE(any(pred.var.class %in% c("factor", "ordered", "character")))) {

        for (i in names(pred.var.class)[pred.var.class %in% c("factor", "ordered", "character")]) {

          model.data <- data.frame(model$contrasts |> (\(w) if (isTRUE(length(w) != 0L)) { w[[1L]] } else { contrasts(as.factor(model.data[, i])) })() ) |>
          (\(y) setNames(y, nm = pred.var |>
                           (\(a) a[which(substr(a, 1L, nchar(i)) == i & substr(a, nchar(i) + 1L, nchar(a)) %in%

                                           # Unordered factor
                                           if (isTRUE(pred.var.class[i] %in% c("factor", "character"))) {

                                             if (isTRUE(is.factor(model.data[, i]))) { levels(model.data[, i]) } else { levels(as.factor(model.data[, i])) }

                                           # Ordered factor
                                           } else if (isTRUE(pred.var.class[i] == "ordered")) {

                                             c(".L", ".Q", ".C", "^4", paste0("^", seq_len(100L)))[seq_len(ncol(y))]

                                           }

                             )])() |>

                            (\(b) if (isTRUE(any(grepl(":", b)))) { b[-grep(":", b)] } else { b })()))() |>
            (\(z) data.frame(by = if (isTRUE(is.factor(model.data[, i]))) { levels(model.data[, i]) } else { levels(as.factor(model.data[, i])) }, z))() |>
            (\(v) data.frame(model.data[, -grep(i, colnames(model.data)), drop = FALSE], v[match(model.data[, i], v[, "by"]), -grep("by", colnames(v)), drop = FALSE], row.names = NULL))()

        }

      }

      # Data
      model.data <- model.data[, c(crit.var, pred.var |> (\(y) y[misty::chr.grep(c("\\:", "\\^"), y) |> (\(z) if (isTRUE(length(z) == 0L)) { seq_along(y) } else { -z })()])(), group.var)]

    }

    # Interaction terms
    pred.var.int <- pred.var[grep(":", pred.var)] |>
      # Check if main effects are included in the model
      (\(y) sapply(y, function(z) { unname(unlist(sapply(y, strsplit, ":"))) |> (\(v) if (isTRUE(any(sapply(v, function(w) !w %in% colnames(model.data))))) {

        stop(paste0("The main effect of the predictors needs to be included in the model when investigating interction effects."), call. = FALSE) } else { return(z) })()

      }))()

    # Polynomial terms
    pred.var.poly <- unname(pred.var[grep("^", pred.var, fixed = TRUE)] |>
       # Check if linear term is included in the model
       (\(y) sapply(y, function(z) {

         sub("I\\(", "", unlist(strsplit(z, "^", fixed = TRUE))[1L]) |> (\(v) if (isTRUE(!v %in% colnames(model.data))) { stop(paste0("The linear term for the predictor \"", v, "\" needs to be included in the model when investigating polynomial effects."), call. = FALSE) } else { return(z) } )()

       }))())

    #...................
    ### Level of Predictor Variables ####
    #
    # 1 = Level-1, 2 = Level-2, 12 = Cross-Level predictor

    # No interaction terms
    if (isTRUE(length(pred.var.int) == 0L)) {

      pred.level <- sapply(pred.var, function(y) { if (all(round(tapply(as.numeric(model.data[, y]), model.data[, group.var], var, na.rm = TRUE), digits = 7L) == 0L)) { 2L } else { 1L } })

    # Interaction terms
    } else {

      pred.level <- sapply(setdiff(pred.var, pred.var.int), function(y) { if (all(round(tapply(model.data[, y], model.data[, group.var], var, na.rm = TRUE), digits = 7L) == 0L)) { 2L } else { 1L } })

      # Add interaction term
      pred.level <- unlist(lapply(sapply(pred.var.int, function(y) strsplit(y, ":")), function(z) { if (isTRUE(all(pred.level[z] == 1L))) { 1L } else if (all(pred.level[z] == 2L)) { 2L } else { 12L }})) |> (\(w) c(pred.level, w)[names(coeff)])()

    }

    #...................
    ### Binary Predictor ####
    #
    # 0 = non-binary, 1 = binary predictor including ordered factors with polynomial contrasts

    pred.binary <- sapply(unique(c(setdiff(pred.var, c(pred.var.int, pred.var.poly)), unname(unlist(sapply(pred.var.int, strsplit, ":"))))), function(y) {

      # Ordered factor
      if (isTRUE(y %in% as.vector(sapply(names(which(sapply(if (isTRUE(class(model) != "lme")) { attributes(model)$frame } else { model$data[, all.vars(model$call) |> (\(a) a[a %in% colnames(model$data) & !a %in% c(group.var, crit.var)])(), drop = FALSE] }, function(b) class(b)[1L]) == "ordered")), function(c) paste0(c, c(".L", ".Q", ".C", "^4", paste0("^", seq_len(100L)))))))) {

        return(1L)

      } else {

        # Level-1 predictor
        if (isTRUE(pred.level[y] == 1L)) {

          if (isTRUE(all(tapply(model.data[, y], model.data[, group.var], misty::uniq.n) <= 2L))) { return(1L) } else { return(0L) }

        # Level-2 predictor
        } else {

          if (isTRUE(misty::uniq.n(model.data[!duplicated(model.data[, group.var]), y]) <= 2)) { return(1L) } else { return(0L) }

        }

      }

    })

    # Add interaction term
    if (isTRUE(length(pred.var.int) != 0L)) { pred.binary <- c(pred.binary, unlist(lapply(sapply(pred.var.int, function(y) strsplit(y, ":")), function(z) if (all(pred.binary[z] == 1L)) { return(1L) } else { return(0L) }))) |> (\(w) c(pred.binary, w)[names(coeff)])() }

    #...................
    ### Predictor standard deviations ####

    sd.pred <- sapply(names(coeff), function(y) {

      ### Interaction term ###
      if (isTRUE(y %in% pred.var.int)) {

        # Split interaction in predictors
        prod(unlist(strsplit(y, ":")) |>
          # Determine SD of each predictor
          (\(z) sapply(z, function(w) {
                       # Level-1 predictor
                       if (isTRUE(pred.level[w] == 1L)) {

                         # Binary predictor
                         if (isTRUE(all(tapply(model.data[, w], model.data[, group.var], misty::uniq.n) <= 2))) {

                           return(1L)

                         # Metric predictor
                         } else {

                           # No Level-2 variance
                           if (isTRUE(all(round(misty::cluster.scores(model.data[, w], cluster = model.data[, group.var], expand = FALSE), digits = 7) == 0L))) {

                             return(summary(lm(model.data[, w] ~ 1))$sigma)

                           # Level-2 variance
                           } else {

                             return(suppressMessages(suppressWarnings(sigma(eval(parse(text = paste0("lme4::lmer(", w, " ~ 1 + (1|", group.var, "), data = model.data, control = lme4::lmerControl(optimizer = \"bobyqa\"))")))))))

                           }

                         }

                       # Level-2 predictor
                       } else {

                         # Binary predictor
                         if (isTRUE(misty::uniq.n(model.data[!duplicated(model.data[, group.var]), w]) <= 2)) {

                           return(1L)

                         # Metric predictor
                         } else {

                           return(sd(model.data[!duplicated(model.data[, group.var]), w], na.rm = TRUE))

                         }

                       }}))())

      ### Polynomial terms ###
      } else if (isTRUE(y %in% pred.var.poly)) {

        pred.ply.sd <- sub("I\\(", "", unlist(strsplit(y, "^", fixed = TRUE))[1L]) |>
          # Level-1 predictor
          (\(z) if (isTRUE(pred.level[z] == 1L)) {

            # No Level-2 variance
            if (isTRUE(all(round(misty::cluster.scores(model.data[, z], cluster = model.data[, group.var], expand = FALSE), digits = 7) == 0L))) {

              return(summary(lm(model.data[, z] ~ 1))$sigma)

            # Level-2 variance
            } else {

              return(suppressMessages(suppressWarnings(sigma(eval(parse(text = paste0("lme4::lmer(", z, " ~ 1 + (1|", group.var, "), data = model.data, control = lme4::lmerControl(optimizer = \"bobyqa\"))")))))))

            }

          # Level-2 predictor
          } else {

            return(suppressMessages(suppressWarnings(sigma(eval(parse(text = paste0("lme4::lmer(", z, " ~ 1 + (1|", group.var, "), data = model.data, control = lme4::lmerControl(optimizer = \"bobyqa\"))")))))))

          })()

        return(prod(rep(pred.ply.sd, times = sub(")", "", unlist(strsplit(y, "^", fixed = TRUE))[2L]))))

      ### Numeric predictor ###
      } else {

        # Numeric predictor
        if (isTRUE(pred.binary[y] == 0L)) {

          # Level 1 predictor
          if (isTRUE(pred.level[y] == 1L)) {

            # No Level-2 variance
            if (isTRUE(all(round(misty::cluster.scores(model.data[, y], cluster = model.data[, group.var], expand = FALSE), digits = 7L) == 0L))) {

              return(summary(lm(model.data[, y] ~ 1))$sigma)

            # Level-2 variance
            } else {

              return(suppressMessages(suppressWarnings(sigma(eval(parse(text = paste0("lme4::lmer(", y, " ~ 1 + (1|", group.var, "), data = model.data, control = lme4::lmerControl(optimizer = \"bobyqa\"))")))))))

            }

          # Level 2 predictor
          } else {

            # Note that the standardize_parameters() function in the 'parameters' package is not correct
            # since it uses sd(model.data[, y], na.rm = TRUE) which is correct only if the group sizes are equal
            return(sd(model.data[!is.na(model.data[, y]), ] |> (\(z) z[!duplicated(model.data[, group.var]), y])(), na.rm = TRUE))

          }

        # Binary predictor
        } else {

          return(NA)

        }

      }

    })

    #...................
    ### Criterion Standard Deviation ####

    # SD at Level 1 and Level 2
    sd.crit.l1.l2 <- suppressMessages(suppressWarnings(eval(parse(text = paste0("lme4::lmer(", crit.var, " ~ 1 + (1|", group.var, "), data = model.data, control = lme4::lmerControl(optimizer = \"bobyqa\"))"))))) |> (\(y) setNames(rev(as.data.frame(VarCorr(y))[, "sdcor"]), nm = c("L1", "L2")))()

    # Level-specific SD of the criterion variable
    sd.crit <- sapply(names(coeff), function(y) { ifelse(unname(pred.level[y]) == 2L, sd.crit.l1.l2[2L], sd.crit.l1.l2[1L]) })

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Print Argument ####

  #### Default settings, print = c("all", "stdx", "stdy", "stdyx") ####
  if (isTRUE(all(c("all", "stdx", "stdy", "stdyx") %in% print))) {

    # All predictors binary, factor, or character
    if (isTRUE(all(pred.binary == 1L))) {

      print <- "stdy"

    # At least one predictor binary, factor, or character
    } else if (isTRUE(any(pred.binary == 1L))) {

      print <- c("stdy", "stdyx")

    # All predictors continuous
    } else {

      print <- "stdyx"

    }

  #### All results, print = "all" ####
  } else if (isTRUE(length(print) == 1L && "all" %in% print)) {

    print <- c("stdx", "stdy", "stdyx")

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Coefficient Table ####

  ### Linear Model, lm() function ####
  if (isTRUE(class(model) == "lm")) {

    # Model with intercept
    if (isTRUE("(Intercept)" %in% names(model$coefficients))) {

      restab <- cbind(summary(model)$coefficients, SDy = c(NA, rep(sd.crit, times = length(sd.pred))), SDx = c(NA, sd.pred), StdX = c(NA, coeff * sd.pred), StdY = c(NA, coeff / sd.crit), StdYX = c(NA, coeff * (sd.pred / sd.crit)))

    # Model without intercept
    } else {

      restab <- cbind(summary(model)$coefficients, SDy = rep(sd.crit, times = length(sd.pred)), SDx = sd.pred, StdX = coeff * sd.pred, StdY = coeff / sd.crit, StdYX = coeff * (sd.pred / sd.crit))

    }

  ### Linear Mixed-Effects Model, lmer() or lme() function from the lme4 or nlme package ####
  } else if (isTRUE(class(model) %in% c("lmerMod", "lmerModLmerTest", "lme"))) {

    # Model with intercept
    if (isTRUE("(Intercept)" %in% names(lme4::fixef(model)))) {

      restab <- cbind(if (isTRUE(class(model) %in% c("lmerMod", "lmerModLmerTest"))) { summary(model)$coefficients } else { summary(model)$tTable }, Level = c(NA, pred.level), SDy = c(NA, sd.crit), SDx = c(NA, sd.pred), StdX = c(NA, coeff * sd.pred), StdY = c(NA, coeff / sd.crit), StdYX = c(NA, coeff * (sd.pred / sd.crit)))

    # Model without intercept
    } else {

      restab <- cbind(if (isTRUE(class(model) %in% c("lmerMod", "lmerModLmerTest"))) { summary(model)$coefficients } else { summary(model)$tTable }, Level = (pred.level), SDy = sd.crit, SDx = sd.pred, StdX = coeff * sd.pred, StdY = coeff / sd.crit, StdYX = coeff * (sd.pred / sd.crit))

    }

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "coeff.std",
                 data = model.data,
                 model = model,
                 args = list(print = print, digits = digits, p.digits = p.digits, write = write, append = append, check = check, output = output),
                 result = restab)

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
