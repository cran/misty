#' Print Summary Output
#'
#' This function prints a summary of the result object returned by the function
#' \code{"lm"} for estimating linear regression models and for the result object
#' returned by the function \code{"lmer"} from the \pkg{lme4} or \pkg{lmerTest}
#' package, result object returned by the function \code{"lme"} from the \pkg{nlme}
#' package, or by the function \code{"rlmer"} from the \pkg{robustlmm} package
#' to estimate two- or three-level (robust) multilevel and linear mixed-effects
#' models. By default, the function prints the function call, model summary,
#' variance and correlation components, and the regression coefficient table.
#'
#' @param model      a fitted model of class \code{"lm"}, \code{"lmerMod"},
#'                   \code{"rlmerMod"}, \code{"lmerModLmerTest"}, or \code{"lme"}.
#' @param print      a character vector indicating which results to print, i.e.
#'                   \code{"all"}, for all results, \code{"call"} for the function
#'                   call, \code{"descript"} for descriptive statistics, \code{cormat}
#'                   for the Pearson product-moment correlation matrix for models
#'                   estimated by \code{"lm"} (see \code{\link{cor.matrix}} function)
#'                   or within- and between-group correlation matrix for models
#'                   estimated by \code{"lmer"} or \code{"lme"} (see
#'                   \code{\link{multilevel.cor}} function), \code{modsum} for
#'                   the multiple correlation, r-squared, and F-test for models
#'                   estimated by \code{"lm"} or model summary, marginal, and
#'                   conditional R-squared for models estimated by \code{"lmer"}
#'                   or \code{"lme"} (see \code{\link{multilevel.r2}} function),
#'                   \code{"randeff"} for the random effects (variance and
#'                   correlation components) for models estimated by \code{"lmer"}
#'                   or \code{"lme"}, \code{"varcor"} for the variance and
#'                   correlation structure for models estimated by \code{"lme"},
#'                   \code{coef} for the unstandardized coefficients for models
#'                   estimated by \code{"lm"} and fixed effects for models
#'                   estimated by \code{"lmer"} or \code{"lme"}, \code{confint}
#'                   for the confidence interval for unstandardized coefficients,
#'                   \code{stdcoef} for the standardized coefficients (see
#'                   \code{\link{coeff.std} function}), and \code{vif} for the
#'                   variance inflation factor (see \code{\link{check.collin}}
#'                   function). The default setting is
#'                   \code{print = c("call", "modsum", "randeff", "varcor", "coef")}.
#'                   Note that when a fitted model of class \code{"rlmerMod"} is
#'                   specified for the argument \code{model}, the default setting
#'                   is \code{c("call", "randeff", "coef")} and that \code{"descript"},
#'                   \code{"cormat"}, \code{"modsum"}, \code{"confint"}, \code{"stdcoef"},
#'                   and \code{"vif"} are not available for an \code{"rlmerMod"}
#'                   object.
#' @param robust     logical: if \code{TRUE}, heteroscedasticity-consistent standard
#'                   errors, confidence intervals, and heteroscedasticity-robust
#'                   F-test using the HC4 estimator are computed for linear models
#'                   estimated by using the \code{lm()} function or cluster-robust
#'                   standard errors using the CR2 estimator is computed for
#'                   multilevel and linear mixed-effects models estimated by using
#'                   the \code{lmer()} or \code{lme()}  function (see
#'                   \code{\link{coeff.robust}} function).
#' @param ddf        a character string for specifying the method for computing
#'                   the degrees of freedom when using the \pkg{lmerTest} package
#'                   to obtain \emph{p}-values for fixed effects in multilevel
#'                   and linear mixed-effects models, i.e., \code{"Satterthwaite"}
#'                   (default) for Satterthwaite's method, \code{"Kenward-Roger"}
#'                   for the Kenward-Roger's method, and \code{"lme4"} for the
#'                   lme4-summary without degrees of freedom and significance
#'                   values (see Kuznetsova et al., 2017). Note that when a fitted
#'                   model of class \code{"rlmerMod"} is specified for the argument
#'                   \code{model}, Satterthwaite or Kenward-Roger degrees of freedom
#'                   are computed only if the R package \pkg{lmerTest} is loaded.
#' @param method     a character string for specifying the method for computing
#'                   confidence intervals (CI), i.e., \code{"profile"} (default
#'                   for \code{"lmer"} objects) for computing a likelihood profile
#'                   and finding the appropriate cutoffs based on the likelihood
#'                   ratio test, \code{"wald"} (default for \code{"lme"} objects)
#'                   for approximating the CIs based on the estimated local curvature
#'                   of the likelihood surface, and \code{"boot"} for performing
#'                   bootstrapping with CIs computed from the bootstrap distribution
#'                   according to the argument \code{boot}.
#' @param conf.level a numeric value between 0 and 1 indicating the confidence
#'                   level of the interval.
#' @param R          a numeric value indicating the number of bootstrap replicates
#'                   (default is 1000).
#' @param boot       a character string for specifying the type of bootstrap
#'                   confidence intervals (CI), i.e., i.e., \code{"perc"} (default),
#'                   for the percentile bootstrap CI, \code{"basic"} for the basic
#'                   bootstrap CI, and \code{"norm"} for the normal approximation
#'                   bootstrap CI.
#' @param seed       a numeric value specifying seeds of the pseudo-random
#'                   numbers used in the bootstrap algorithm when conducting
#'                   bootstrapping.
#' @param digits     an integer value indicating the number of decimal places
#'                   to be used.
#' @param p.digits   an integer value indicating the number of decimal places
#'                   to be used for displaying multiple R, R-squared and
#'                   \emph{p}-value.
#' @param write      a character string naming a file for writing the output into
#'                   either a text file with file extension \code{".txt"} (e.g.,
#'                   \code{"Output.txt"}) or Excel file with file extension
#'                   \code{".xlsx"}  (e.g., \code{"Output.xlsx"}). If the file
#'                   name does not contain any file extension, an Excel file will
#'                   be written.
#' @param append     logical: if \code{TRUE} (default), output will be appended
#'                   to an existing text file with extension \code{.txt} specified
#'                   in \code{write}, if \code{FALSE} existing text file will be
#'                   overwritten.
#' @param check      logical: if \code{TRUE} (default), argument specification
#'                   is checked.
#' @param output     logical: if \code{TRUE} (default), output is shown on the console.
#'
#' \describe{
#' \item{\strong{Robust Estimation of Multilevel and Linear Mixed-Effects Models}}{
#' The function \code{rlmer} from the \pkg{robustlmm} package does not provide
#' any degrees of freedom or significance values. This function re-estimates the
#' model without using robust estimation to obtain the Satterthwaite or Kenward-Roger
#' degrees of freedom depending on the argument \code{ddf} before computing
#' significance values for the regression coefficients based on parameter estimates
#' and standard error of the robust multilevel mixed-effects (see Sleegers et al.
#' (2021).}
#' }
#'
#' @author
#' Takuya Yanagida
#'
#' @references
#' Kuznetsova, A, Brockhoff, P. B., & Christensen, R. H. B. (2017). lmerTest Package:
#' Tests in linear mixed effects models. \emph{Journal of Statistical Software, 82}
#' 13, 1-26. https://doi.org/10.18637/jss.v082.i13.
#'
#' Sleegers, W. W. A., Proulx, T., & van Beest, I. (2021). Pupillometry and hindsight bias:
#' Physiological arousal predicts compensatory behavior. \emph{Social Psychological
#' and Personality Science, 12}(7), 1146–1154. https://doi.org/10.1177/1948550620966153
#'
#' @seealso
#' \code{\link{descript}}, \code{\link{cor.matrix}}, \code{\link{coeff.std}},
#' \code{\link{coeff.robust}}, \code{\link{check.collin}}
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{model}}{model specified in \code{model}}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{result}}{list with results, i.e., \code{call} for the the function
#' call, \code{descript} for descriptive statistics, \code{cormat} for the
#' correlation matrix, \code{modsum} for the model summary, \code{randeff} for
#' the variance and correlation components, \code{varcor} for the variance and
#' correlation structure, \code{coef} for the model coefficients,
#' \code{weights} for the robustness weights, and \code{converg} for the convergence
#' check, i.e., \code{1} = model converged, \code{0} = model singular, and \code{-1}
#' model not converged.}
#'
#' @export
#'
#' @examples
#' #----------------------------------------------------------------------------
#' # Linear Model
#'
#' # Estimate linear model
#' mod.lm <- lm(mpg ~ cyl + disp, data = mtcars)
#'
#' # Example 1a: Default setting
#' summa(mod.lm)
#'
#' # Example 1b: Heteroscedasticity-consistent standard errors
#' summa(mod.lm, robust = TRUE)
#'
#' # Example 1c: Print all available results
#' summa(mod.lm, print = "all")
#'
#' # Example 1d: Print default results plus standardized coefficient
#' summa(mod.lm, print = c("default", "stdcoef"))
#'
#' \dontrun{
#' #----------------------------------------------------------------------------
#' # Multilevel and Linear Mixed-Effects Model
#'
#' # Load lme4, nlme, and misty package
#' misty::libraries(lme4, nlme, misty)
#'
#' # Load data set "Demo.twolevel" in the lavaan package
#' data("Demo.twolevel", package = "lavaan")
#'
#' #------------------
#' ## Two-Level Data
#'
#' # Cluster-mean centering, center() from the misty package
#' Demo.twolevel <- center(Demo.twolevel, x2, type = "CWC", cluster = "cluster")
#'
#' # Grand-mean centering, center() from the misty package
#' Demo.twolevel <- center(Demo.twolevel, w1, type = "CGM", cluster = "cluster")
#'
#' # Estimate two-level mixed-effects model using the lme4 package
#' mod.lmer2 <- lmer(y1 ~ x2.c + w1.c + x2.c:w1.c + (1 + x2.c | cluster), data = Demo.twolevel)
#'
#' # Estimate two-level mixed-effects model using the nlme package
#' mod.lme2 <- lme(y1 ~ x2.c + w1.c + x2.c:w1.c, random = ~ 1 + x2.c | cluster, data = Demo.twolevel)
#'
#' # Example 2a: Default setting
#' summa(mod.lmer2)
#' summa(mod.lme2)
#'
#' # Example 2b: Print all available results
#' summa(mod.lmer2, print = "all")
#' summa(mod.lme2, print = "all")
#'
#' # Example 2c: Print default results plus standardized coefficient
#' summa(mod.lmer2, print = c("default", "stdcoef"))
#' summa(mod.lme2, print = c("default", "stdcoef"))
#'
#' # Load lmerTest package
#' library(lmerTest)
#'
#' # Re-estimate two-level model using the lme4 and lmerTest package
#' mod.lmer2 <- lmer(y1 ~ x2.c + w1.c + x2.c:w1.c + (1 + x2.c | cluster), data = Demo.twolevel)
#'
#' # Example 2d: Default setting, Satterthwaite's method
#' summa(mod.lmer2)
#'
#' # Example 2e: Kenward-Roger's method
#' summa(mod.lmer2, ddf = "Kenward-Roger")
#'
#' # Example 2f: Cluster-robust standard errors
#' summa(mod.lmer2, robust = TRUE)
#'
#' #------------------
#' ## Robust Estimation using the R package robustlmm
#'
#' # Estimate two-level mixed-effects model
#' mod.lmer2r <- robustlmm::rlmer(y1 ~ x2.c + w1.c + (1 | cluster), data = Demo.twolevel)
#'
#' # Example 2f: Default setting
#' summa(mod.lmer2r)
#'
#' #------------------
#' ## Three-Level Data
#'
#' # Create arbitrary three-level data
#' Demo.threelevel <- data.frame(Demo.twolevel, cluster2 = Demo.twolevel$cluster,
#'                                              cluster3 = rep(1:10, each = 250))
#'
#' # Cluster-mean centering, center() from the misty package
#' Demo.threelevel <- center(Demo.threelevel, x1, type = "CWC", cluster = c("cluster3", "cluster2"))
#'
#' # Cluster-mean centering, center() from the misty package
#' Demo.threelevel <- center(Demo.threelevel, w1, type = "CWC", cluster = c("cluster3", "cluster2"))
#'
#' # Estimate three-level model using the lme4 package
#' mod.lmer3 <- lmer(y1 ~ x1.c + w1.c + (1 | cluster3/cluster2), data = Demo.threelevel)
#'
#' # Estimate three-level model using the nlme package
#' mod.lme3 <- lme(y1 ~ x1.c + w1.c, random = ~ 1 | cluster3/cluster2, data = Demo.threelevel)
#'
#' # Example 3a: Default setting
#' summa(mod.lmer3)
#' summa(mod.lme3)
#'
#' # Example 3b: Print all available results
#' summa(mod.lmer3, print = "all")
#' summa(mod.lme3, print = "all")
#'
#' #------------------
#' ## Robust Estimation using the R package robustlmm
#'
#' # Estimate three-level model using the lme4 package
#' mod.lmer3r <- robustlmm::rlmer(y1 ~ x1.c + w1.c + (1 | cluster3/cluster2), data = Demo.threelevel)
#'
#'# Example 3c: Default setting
#' summa(mod.lmer3r)
#'
#' #----------------------------------------------------------------------------
#' # Write Results
#'
#' # Example 4a: Write Results into a text file
#' summa(mod.lm, print = "all", write = "Linear_Model.txt")
#'
#' # Example 4b: Write Results into a Excel file
#' summa(mod.lm, print = "all", write = "Linear_Model.xlsx")
#' }
summa <- function(model,
                  print = c("all", "default", "call", "descript", "cormat",
                            "modsum", "randeff", "varcor", "coef", "confint", "stdcoef", "vif"),
                  robust = FALSE, ddf = c("Satterthwaite", "Kenward-Roger", "lme4"),
                  method = c("profile", "wald", "boot"), conf.level = 0.95,
                  R = 1000, boot = c("perc", "basic", "norm"), seed = NULL,
                  digits = 2, p.digits = 3, write = NULL, append = TRUE,
                  check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'model' is missing or NULL
  if (isTRUE(missing(model) ||is.null(model))) { stop("Input for the argument 'model' is missing.", call. = FALSE) }

  # Check if input 'model' is not 'lm', 'lmerMod', 'rlmerMod', 'lmerModLmerTest', or 'lme'
  if (isTRUE(!any(class(model) %in% c("lm", "lmerMod", "rlmerMod", "lmerModLmerTest", "lme")))) { stop("Please specify a \"lm\", \"lmerMod\", \"rlmerMod\", \"lmerModLmerTest\", or \"lme\" object for the argument 'model'.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("robust", "append", "output"), numeric = list(seed = 1L),
               s.character = list(ddf = c("Satterthwaite", "Kenward-Roger", "lme4"), method = c("profile", "wald", "boot"), boot = c("perc", "basic", "norm")),
               m.character = list(print = c("all", "default", "call", "descript", "cormat", "modsum", "randeff", "varcor", "coef", "confint", "stdcoef", "vif")),
               args = c("R", "digits", "p.digits", "conf.level", "write2"), envir = environment(), input.check = check)

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model Class ####

  if (isTRUE(all(class(model) == "lm"))) {

    model.class <- "lm"

  } else if (all(class(model) %in% c("lmerMod", "rlmerMod", "lmerModLmerTest"))) {

    model.class <- "lmer"

  } else if (all(class(model) == "lme")) {

    model.class <- "lme"

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Argument 'robust' ####

  if (isTRUE(robust)) {

    switch(model.class, lmer = {

             if (isTRUE(lme4::getME(model, name = "n_rtrms") != 1L)) { stop("Cluster-robust standard errors are supports only for two-level models.", call. = FALSE) }

           }, lme = {

             if (isTRUE(ncol(model$groups) != 1L)) { stop("Cluster-robust standard errors are supports only for two-level models.", call. = FALSE) }

           })

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Argument 'ddf' ####

  ddf <- if (isTRUE(all(c("Satterthwaite", "Kenward-Roger", "lme4") %in% ddf))) {

      "Satterthwaite"

    } else if (isTRUE(ddf == "Kenward-Roger" && model.class == "lme")) {

      stop("Kenward-Roger's method for approximating the dgrees of freedom is not available for a 'lme' object.", call. = FALSE)

    } else {

      ddf

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Argument 'method' ####

  method <- if (isTRUE(all(c("profile", "wald", "boot") %in% method))) {

    switch(model.class, lm = "wald", lmer = "profile", lme = "wald")

  } else if (isTRUE(method %in% c("profile", "boot") && model.class == "lme")) {

    stop("Profile and boostrapping CI are not available for a 'lme' object.", call. = FALSE)

  } else {

    method

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Argument 'boot' ####

  boot <- ifelse(all(c("perc", "basic", "norm") %in% boot), "perc", boot)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Argument 'print' ####

  # All results
  print.all <- c("call", "descript", "cormat", "modsum", "randeff", "varcor", "coef", "confint", "stdcoef", "vif")

  # Default setting
  if (isTRUE(all(c("all", "default", "call", "descript", "cormat", "modsum", "randeff", "varcor", "coef", "confint", "stdcoef", "vif") %in% print))) {

    if (isTRUE(class(model) != "rlmerMod")) {

      print <- c("call", "modsum", "randeff", "varcor", "coef")

    } else {

      print <- c("call", "randeff", "coef")

    }

  # All print commands
  } else if (isTRUE("all" %in% print)) {

    print <- print.all

  # Default setting with additional print commands
  } else if (isTRUE("default" %in% print && length(print > 1L))) {

    print <- print.all[print.all %in% misty::chr.omit(union(c("call", "modsum", "randeff", "varcor", "coef"), print), "default", check = FALSE)]

  # Manual default setting
  } else if (isTRUE(all(print == "default"))) {

    if (isTRUE(class(model) != "rlmerMod")) {

      print <- c("call", "modsum", "randeff", "varcor", "coef")

    } else {

      print <- c("call", "randeff", "coef")

    }

  }

  # Model of class 'lme'
  if (isTRUE(model.class != "lme")) { print <- setdiff(print, "varcor") }

  # Model of class 'rlmerMod'
  if (isTRUE(class(model) == "rlmerMod")) { print <- setdiff(print,  c("descript", "cormat", "modsum", "confint", "stdcoef", "vif")) }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  call <- descript <- cormat <- modsum <- randeff <- varcor <- modcoef <- weights <- converg <- NULL

  switch(model.class,

         #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         ## Linear Regression, lm() ####

         lm = {

           # Model summary
           model.summary <- summary(model)

           # Remove VIF from the 'print' argument when only one predictor
           if (isTRUE("vif" %in% print && length(attr(model.summary$terms, "term.labels")) <= 1L)) {

             print <- misty::chr.omit(print, omit = "vif", check = FALSE)

             if (isTRUE(length(print) == 0L)) { stop("Variance inflation factor can only be computed for models with at least two predictors.", call. = FALSE) }

           }

           # Remove 'cormat' and 'stdcoef' from the 'print' argument when no predictors
           if (isTRUE(any(c("cormat", "stdcoef") %in% print) && length(attr(model.summary$terms, "term.labels")) == 0L)) {

             print <- misty::chr.omit(print, omit = c("cormat", "stdcoef"), check = FALSE)

             if (isTRUE(length(print) == 0L)) { stop("Correlation matrix and standardized coefficients are not available for the null model.", call. = FALSE) }

           }

           # Heteroscedasticity-Consistent Standard Errors and Heteroscedasticity-Robust F-test
           if (isTRUE(robust)) {

             model.robust <- misty::coeff.robust(model, output = FALSE)$result

             # F test statistic and degrees of freedom
             model.summary$fstatistic <- c(model.robust$F.test[2L, "F"], model.robust$F.test[2L, "df"], model.robust$F.test[2L, "res.df"])

             # Coefficients
             model.summary$coefficients <- model.robust$coef

           }

           #--------------------------------------
           ### Call ####

           if (isTRUE("call" %in% print)) { call <- as.character(stats::getCall(model)) |> (\(p) list(formula = p[2L], data = p[3L]))() }

           #--------------------------------------
           ### Descriptive Statistics ####

           if (isTRUE("descript" %in% print)) { descript <- suppressWarnings(misty::descript(model$model, check = FALSE, output = FALSE))$result[, c("variable", "n", "nUQ", "m", "sd", "min", "p.min", "max", "p.max", "skew", "kurt")] }

           #--------------------------------------
           ### Correlation Matrix ####

           if (isTRUE("cormat" %in% print)) { cormat <- suppressWarnings(misty::cor.matrix(model$model, check = FALSE, output = FALSE))$result$cor }

           #--------------------------------------
           ### Model Summary ####

           if (isTRUE("modsum" %in% print)) { modsum <- data.frame(n = nrow(model$model), nNA = length(model.summary$na.action), R = sqrt(model.summary$r.squared), R2 = model.summary$r.squared, R2.adj = ifelse(model.summary$adj.r.squared < 0L, 0L, model.summary$adj.r.squared), df1 = model.summary$fstatistic[2L] |> (\(p) if (isTRUE(!is.null(p))) { p } else { NA })(), df2 = model.summary$fstatistic[3L] |> (\(p) if (isTRUE(!is.null(p))) { p } else { NA })(), F = model.summary$fstatistic[1L] |> (\(p) if (isTRUE(!is.null(p))) { p } else { NA })(), p = model.summary$fstatistic |> (\(p) if (isTRUE(!is.null(p))) { pf(p[1L], p[2L], p[3L], lower.tail = FALSE) } else { NA } )(), row.names = NULL) }

           #--------------------------------------
           ### Coefficients ####

           if (isTRUE(any(c("coef", "confint", "stdcoef", "vif") %in% print))) {

             #...................
             #### Unstandardized Coefficients ####

             modcoef <- misty::df.move(cbind(setNames(data.frame(model.summary$coefficients),  nm = c("Estimate", "SE", "t", "p")), df = model$df.residual), df, after = "SE")

             #...................
             #### Confidence intervals ####

             if (isTRUE("confint" %in% print)) {

               # Regular Standard Errors
               if (isTRUE(!robust)) {

                 modcoef <- setNames(data.frame(modcoef, confint(model, level = conf.level)), nm = c("Estimate", "SE", "df", "t", "p", "Low", "Upp"))

               # HC Standard Errors
               # https://stackoverflow.com/questions/3817182/vcovhc-and-confidence-interval
               } else {

                 modcoef <- cbind(modcoef, setNames(as.data.frame(coef(model) + sqrt(diag(model.robust$sandwich)) %o% qt(c((1L - conf.level) / 2L, 1L - (1L - conf.level) / 2L), model$df.residual)), nm = c("Low", "Upp")))

               }

             }

             #...................
             #### Standardized Coefficients ####

             if (isTRUE("stdcoef" %in% print)) {

               modcoef <- data.frame(modcoef, misty::coeff.std(model, check = FALSE, output = FALSE) |>
                                       (\(p) if (isTRUE(all(p$args$print == "stdyx"))) {

                                         p$result[, setdiff(colnames(p$result), c("Estimate", "Std. Error", "t value", "Pr(>|t|)", "StdX", "StdY"))]

                                       } else if (isTRUE(all(c("stdy", "stdyx") %in% p$args$print))) {

                                         p$result[, setdiff(colnames(p$result), c("Estimate", "Std. Error", "t value", "Pr(>|t|)", "StdX"))]

                                       } else if (isTRUE(all(p$args$print == "stdy"))) {

                                         p$result[, setdiff(colnames(p$result), c("Estimate", "Std. Error", "t value", "Pr(>|t|)", "SDx", "StdX", "StdYX"))]

                                       } else if (isTRUE(all(p$args$print == "stdx"))) {

                                         p$result[, setdiff(colnames(p$result), c("Estimate", "Std. Error", "t value", "Pr(>|t|)", "SDy", "StdY", "StdYX"))]

                                       })())

             }

             #...................
             #### Variance Inflation Factor ####

             if (isTRUE("vif" %in% print)) { modcoef <- data.frame(modcoef, VIF = misty::check.collin(model, check = FALSE, output = FALSE)$result$coef[, "aGVIF"]) }

           }

         #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         ## Linear Mixed-Effects Model, lmer() ####

         }, lmer = {

           #--------------------------------------
           ### Check ####

           # Remove 'vif' from the 'print' argument when only one predictor
           if (isTRUE("vif" %in% print && length(setdiff(colnames(model.frame(model)), names(lme4::getME(model, name = "cnms")))) <= 2L)) {

             print <- misty::chr.omit(print, omit = "vif", check = FALSE)

             if (isTRUE(length(print) == 0L)) { stop("Variance inflation factor can only be computed for models with at least two predictors.", call. = FALSE) }

           }

           # Remove 'cormat' and 'stdcoef' from the 'print' argument when no predictors
           if (isTRUE(any(c("cormat", "stdcoef") %in% print) && length(misty::chr.omit(attr(lme4::getME(model, name = "X"), which = "dimnames")[[2L]], omit = "(Intercept)", check = FALSE)) == 0L)) {

             print <- misty::chr.omit(print, omit = c("cormat", "stdcoef"), check = FALSE)

             if (isTRUE(length(print) == 0L)) { stop("Correlation matrix and standardized coefficients are not available for the null model.", call. = FALSE) }

           }

           #--------------------------------------
           ### Model Summary and Data ####

           # Model summary
           model.summary <- summary(model)

           # Two-level model
           model.twolevel <- ifelse(lme4::getME(model, name = "n_rtrms") == 1L, TRUE, FALSE)

           # Cluster name
           model.cluster <- unique(names(lme4::getME(model, name = "cnms"))) |> (\(p) p[order(nchar(p), decreasing = TRUE)])()

           # Name of the data
           model.data.name <- as.character(stats::getCall(model))[3L]

           # Model Data
           model.data <- model.frame(model)

           # Modify cluster names
           if (isTRUE(!model.twolevel)) { model.data <- misty::df.rename(model.data, from = names(table(unlist(strsplit(model.cluster, ":")))), to = model.cluster) }

           # Data in Workspace to obtain data including NA
           model.data.ws <- tryCatch(if (isTRUE(exists(model.data.name))) {

             eval(parse(text = model.data.name)) |> (\(p) if (isTRUE(nrow(p) >= nobs(model) && all(colnames(model.frame(model)) %in% colnames(p)))) {

               y[, colnames(model.frame(model)), drop = FALSE]

             } else {

               NULL

             })()

           } else { NULL }, error = function(y) { NULL })

           # Outcome and predictor variables
           model.data.yx <- model.data[, setdiff(colnames(model.data), model.cluster), drop = FALSE]

           #--------------------------------------
           ### Level of the Predictors ####

           #...................
           #### Two-Level Model

           if (isTRUE(model.twolevel)) {

             # Level of Variables, 1 = Level-1 and 2 = Level-2 variable
             var.level <- sapply(colnames(model.data.yx), function(y) { if (all(tapply(as.numeric(model.data[, y]), model.data[, model.cluster], var, na.rm = TRUE) < .Machine$double.eps^0.5, na.rm = TRUE)) { 2L } else { 1L } })

           #...................
           #### Three-Level Model

           } else {

             # Level of Variables, 1 = Level-1, 2 = Level-2 variable, and 3 = Level-3 variable
             var.level <- sapply(colnames(model.data.yx), function(y) {

               if (isTRUE(any(na.omit(as.vector(tapply(as.numeric(model.data[, y]), apply(model.data[, model.cluster], 1L, paste, collapse = ""), var, na.rm = TRUE))) > .Machine$double.eps^0.5))) {

                 1L

               # Level 2 Variable
               } else if (isTRUE(all(na.omit(as.vector(tapply(as.numeric(model.data[, y]), apply(model.data[, model.cluster], 1L, paste, collapse = ""), var, na.rm = TRUE))) < .Machine$double.eps^0.5) && any(as.vector(tapply(model.data[, y], model.data[, model.cluster[2L]], var, na.rm = TRUE)) != 0L))) {

                 2L

               # Level 3 Variable
               } else if (isTRUE(all(na.omit(as.vector(tapply(as.numeric(model.data[, y]), apply(model.data[, model.cluster], 1L, paste, collapse = ""), var, na.rm = TRUE))) < .Machine$double.eps^0.5) && all(na.omit(as.vector(tapply(model.data[, y], model.data[, model.cluster[1L]], var, na.rm = TRUE))) < .Machine$double.eps^0.5))) {

                 3L

               }

             })

           }

           # Level-1 variable names
           var.level1 <- names(which(var.level == 1L))

           # Level-2 variable names
           var.level2 <- names(which(var.level == 2L))

           # Level-3 variable names
           var.level3 <- names(which(var.level == 3L))

           # Factors or Character
           var.factor <- names(which(!sapply(model.data.yx, is.numeric)))

           #--------------------------------------
           ### Cluster-Robust Standard Errors ####

           if (isTRUE(robust)) { model.robust <- misty::coeff.robust(model, output = FALSE)$result }

           #--------------------------------------
           ### Model Not Converged or Singular ####

           # -1 = not converged, 0 = singular, 1 = model converged
           converg <- if (isTRUE(!is.null(unlist(model@optinfo$conv$lme4)) && any(grepl("-1", unlist(model@optinfo$conv$lme4))))) { -1L } else if (isTRUE(!is.null(unlist(model@optinfo$conv$lme4)))) { 0L } else { 1L }

           #--------------------------------------
           ### Call ####

           if (isTRUE("call" %in% print)) { call <- as.character(stats::getCall(model)) |> (\(p) list(formula = p[2L], data = p[3L]))() }

           #--------------------------------------
           ### Descriptive Statistics ####

           if (isTRUE("descript" %in% print)) {

             #...................
             #### Two-Level Model

             if (isTRUE(model.twolevel)) {

               # Level-1 variables
               descript.l1 <- data.frame(suppressWarnings(misty::descript(model.data[, setdiff(var.level1, var.factor), drop = FALSE], check = FALSE, output = FALSE))$result[, c("variable", "n", "nUQ", "m", "sd", "min", "p.min", "max", "p.max", "skew", "kurt")], icc = misty::multilevel.icc(model.data[, setdiff(var.level1, var.factor)], cluster = model.data[, model.cluster]), row.names = setdiff(var.level1, var.factor))

               # Level-2 variables
               if (isTRUE(length(var.level2) != 0L)) { descript.l2 <- data.frame(suppressWarnings(misty::descript(model.data[!duplicated(model.data[, model.cluster[1L]]), var.level2, drop = FALSE], check = FALSE, output = FALSE))$result[, c("variable", "n", "nUQ", "m", "sd", "min", "p.min", "max", "p.max", "skew", "kurt")], icc = NA, row.names = var.level2) } else { descript.l2 <- NULL }

               # Level-1 and Level-2 variables
               descript <- data.frame(rbind(descript.l1, descript.l2)[setdiff(colnames(model.data.yx), var.factor), ], row.names = NULL)

             #...................
             #### Three-Level Model

             } else {

               # Level-1 variables
               descript.l1 <- data.frame(suppressWarnings(misty::descript(model.data[, setdiff(var.level1, var.factor), drop = FALSE], check = FALSE, output = FALSE))$result[, c("variable", "n", "nUQ", "m", "sd", "min", "p.min", "max", "p.max", "skew", "kurt")], setNames(as.data.frame(t(misty::multilevel.icc(model.data[, setdiff(var.level1, var.factor)], cluster = model.data[, rev(model.cluster)]))[, model.cluster]), nm = c("icc.l2", "icc.l3")), row.names = setdiff(var.level1, var.factor))

               # Level-2 variables
               if (isTRUE(length(var.level2) != 0L)) { descript.l2 <- data.frame(suppressWarnings(misty::descript(model.data[!duplicated(model.data[, model.cluster[1L]]), var.level2, drop = FALSE], check = FALSE, output = FALSE))$result[, c("variable", "n", "nUQ", "m", "sd", "min", "p.min", "max", "p.max", "skew", "kurt")], icc.l2 = NA, icc.l3 = misty::multilevel.icc(model.data[, setdiff(var.level2, var.factor)], cluster = model.data[, model.cluster[2L]]), row.names = var.level2) } else { descript.l2 <- NULL }

               # Level-3 variables
               if (isTRUE(length(var.level3) != 0L)) { descript.l3 <- data.frame(suppressWarnings(misty::descript(model.data[!duplicated(model.data[, model.cluster[2L]]), var.level3, drop = FALSE], check = FALSE, output = FALSE))$result[, c("variable", "n", "nUQ", "m", "sd", "min", "p.min", "max", "p.max", "skew", "kurt")], icc.l2 = NA, icc.l3 = NA, row.names = var.level3) } else { descript.l3 <- NULL }

               # Level-1, Level-2, and Level-3 variables
               descript <- data.frame(rbind(descript.l1, descript.l2, descript.l3)[setdiff(colnames(model.data.yx), var.factor), ], row.names = NULL)

             }

           }

           #--------------------------------------
           ### Within-Group and Between-Group Correlation Matrix ####

           if (isTRUE("cormat" %in% print)) {

             # Two-Level Model and Variables after excluding factors
             if (isTRUE(model.twolevel && length(setdiff(union(var.level1, var.level2), var.factor)) >= 2L)) {

               tryCatch(cormat <- suppressWarnings(misty::multilevel.cor(model.data, cluster = model.cluster, optim.switch = FALSE, check = FALSE, output = FALSE))$result$wb.cor,

                        error = function(y) {

                          warning("Within- and between-group correlation matrix computation failed.", call. = FALSE)

                          print <<- setdiff(print, "cormat")

                          cormat <<- NULL

                        })

             } else {

               cormat <- NULL

               print <- setdiff(print, "cormat")

               if (isTRUE(length(print) == 0L)) {

                 # Two-Level Model
                 if (isTRUE(model.twolevel)) {

                   stop("After excluding factors, there are no variables left for computing the within- and between-group correlation matrix.", call. = FALSE)

                 # Three-Level Model
                 } else {

                   stop("Within- and between-group correlation matrix is only available for two-level models.", call. = FALSE)

                 }

               }

             }

           }

           #--------------------------------------
           ### Model Summary ####

           if (isTRUE("modsum" %in% print)) {

             #...................
             #### Number of Cases and Missing Values ####

             # Data available in Workspace
             if (isTRUE(!is.null(model.data.ws))) {

               modsum <- data.frame(n = nobs(model), nNA = nrow(model.data.ws) - nobs(model))

             # Data not available in Workspace
             } else {

               modsum <- data.frame(n = nobs(model))

             }

             #...................
             #### Number of Clusters, Log-Likelihood, and R-Square ####

             # Two-Level Model
             if (isTRUE(model.twolevel)) {

               modsum <- data.frame(modsum, nCl = lme4::ngrps(model), npar = attr(unclass(model.summary$logLik), "df"), method = ifelse(all(names(model.summary$AICtab) == "REML"), "REML", "FML"),
                                    loglik = as.numeric(logLik(model)), deviance = -2*as.numeric(logLik(model)),
                                    setNames(misty::multilevel.r2(model, print = "NS", check = FALSE, output = FALSE)$result$ns, nm = c("margR2", "condR2")) , row.names = NULL)

             # Three-Level Model
             } else {

               modsum <- data.frame(modsum, nCl2 = lme4::ngrps(model)[1L], nCl3 = lme4::ngrps(model)[2L],
                                    npar = attr(unclass(model.summary$logLik), "df"), method = ifelse(all(names(model.summary$AICtab) == "REML"), "REML", "FML"),
                                    loglik = as.numeric(logLik(model)), deviance = -2*as.numeric(logLik(model)),
                                    setNames(misty::multilevel.r2(model, print = "NS", check = FALSE, output = FALSE)$result$ns, nm = c("margR2", "condR2")) , row.names = NULL)

             }

           }

           #--------------------------------------
           ### Random Effects ####

           if (isTRUE("randeff" %in% print)) {

             randeff <- data.frame(groups = c(unlist(sapply(names(VarCorr(model)), function(y) c(y, rep(NA, times = nrow(VarCorr(model)[[y]]) - 1L)))), "Residual"),
                                   name = c(unlist(sapply(names(VarCorr(model)), function(y) names(attr(VarCorr(model)[[y]], which = "stddev")))), "NA"),
                                   var = c(unlist(sapply(names(VarCorr(model)), function(y) attr(VarCorr(model)[[y]], which = "stddev"))), attr(VarCorr(model), which = "sc"))^2,
                                   sd = c(unlist(sapply(names(VarCorr(model)), function(y) attr(VarCorr(model)[[y]], which = "stddev"))), attr(VarCorr(model), which = "sc")),
                                   do.call("rbind", unique(unlist(sapply(names(VarCorr(model)), function(y) colnames(attr(VarCorr(model)[[y]], which = "correlation"))))) |>
                                             (\(z) lapply(names(VarCorr(model)), function(w) attr(VarCorr(model)[[w]], which = "correlation") |> (\(q) if (isTRUE(!setequal(colnames(q), z))) { misty::df.rename(setNames(data.frame(q, matrix(NA, ncol = length(setdiff(z, colnames(q))), nrow = nrow(q))), nm = c(colnames(q), setdiff(z, colnames(q)))), from = "(Intercept)", to = "cor") } else { misty::df.rename(q, from = "(Intercept)", to = "cor") })()))()) |>
                                     (\(p) rbind(p, setNames(rep(NA, times = ncol(p)), nm = colnames(p))))(), check.names = FALSE, row.names = NULL)

           }

           #--------------------------------------
           ### Coefficients ####

           if (isTRUE(any(c("coef", "confint", "stdcoef", "vif") %in% print))) {

             #...................
             #### Unstandardized Coefficients ####

             ##### Regular standard errors
             if (isTRUE(!robust)) {

               if (isTRUE(class(model) %in% c("lmerMod", "rlmerMod"))) {

                 modcoef <- setNames(as.data.frame(coef(model.summary)), nm = c("Estimate", "SE", "t"))

               } else if (isTRUE(class(model) == "lmerModLmerTest")) {

                 if (isTRUE(ddf != "lmer")) {

                   modcoef <- setNames(as.data.frame(coef(summary(model, ddf = ddf))), nm = c("Estimate", "SE", "df", "t", "p"))

                 } else {

                   modcoef <- setNames(as.data.frame(coef(summary(model, ddf = ddf))), nm = c("Estimate", "SE", "t"))

                 }

               }

             ##### Cluster-robust standard errors
             } else {

               modcoef <- model.robust$coef

             }

             #--------------------------------------
             ### Degrees of Freedom for the rlmerMod Object ####

             # Compute df and significance values only if the lmerTest package is attached
             if (isTRUE(inherits(model, what = "rlmerMod") && "package:lmerTest" %in% search() && ddf != "lme4")) {

               # Estimate model to obtain dfs
               modcoef$df <- eval(parse(text = paste0("coef(summary(lmerTest::lmer(", as.character(stats::getCall(model))[2L], ", data = model.frame(model)), ddf = \"", ddf, "\"))[, \"df\"]")))

               # Significance values
               modcoef$p <- pt(abs(modcoef$t), modcoef$df, lower.tail = FALSE)*2L

               # Rearrange columns
               modcoef <- modcoef[, c("Estimate", "SE", "df", "t", "p")]

             }

             #--------------------------------------
             ### Robustness Weights for the rlmerMod Object ####

             if (isTRUE(class(model) == "rlmerMod")) {

               # Two-Level Model
               if (isTRUE(model.twolevel)) {

                 weights <- list(resid = lme4::getME(model, "w_e") |> (\(p) list(ew1 = sum(p == 1L), ew0 = sum(p != 1L), pdescript = if (isTRUE(sum(p != 1L) >= 2L)) { misty::descript(p[p != 1L], output = FALSE)$result[, c("m", "sd", "min", "p25", "med", "p75", "max", "range", "iqr")] } else { pdesscript = p[p != 1] }))(),
                                 ranef = lme4::getME(model, "w_b")[[1L]][, 1L] |> (\(p) list(bw1 = sum(p == 1L), bw0 = sum(p != 1L), bdescript = if (isTRUE(sum(p != 1L) >= 2L)) { misty::descript(p[p != 1L], output = FALSE)$result[, c("m", "sd", "min", "p25", "med", "p75", "max", "range", "iqr")] } else { pdesscript = p[p != 1] }))())

               # Three-Level Model
               } else {

                 weights <- list(resid  = lme4::getME(model, "w_e") |> (\(p) list(ew1 = sum(p == 1L), ew0 = sum(p != 1L), pdescript = if (isTRUE(sum(p != 1L) >= 2L)) { misty::descript(p[p != 1L], output = FALSE)$result[, c("m", "sd", "min", "p25", "med", "p75", "max", "range", "iqr")] } else { pdesscript = p[p != 1] }))(),
                                 ranef1 = lme4::getME(model, "w_b")[[1L]][, 1L] |> (\(p) list(b1w1 = sum(p == 1L), b1w0 = sum(p != 1L), b1descript = if (isTRUE(sum(p != 1L) >= 2L)) { misty::descript(p[p != 1L], output = FALSE)$result[, c("m", "sd", "min", "p25", "med", "p75", "max", "range", "iqr")] } else { pdesscript = p[p != 1] }))(),
                                 ranef2 = lme4::getME(model, "w_b")[[2L]][, 1L] |> (\(p) list(b2w1 = sum(p == 1L), b2w0 = sum(p != 1L), b2descript = if (isTRUE(sum(p != 1L) >= 2L)) { misty::descript(p[p != 1L], output = FALSE)$result[, c("m", "sd", "min", "p25", "med", "p75", "max", "range", "iqr")] } else { pdesscript = p[p != 1] }))())

               }

             }

             #...................
             #### Confidence intervals ####

             if (isTRUE("confint" %in% print)) {

               ##### Profile or Bootstrap CI
               if (method != "wald") {

                 if (!isTRUE(converg %in% c(-1L, 0L) && method == "profile")) {

                   # Set seed when using bootstrap CI
                   if (isTRUE(method == "boot" && !is.null(seed))) { set.seed(seed) }

                   modcoef <- cbind(modcoef,
                                    setNames(as.data.frame(tryCatch(suppressMessages(lme4::confint.merMod(model, parm = "beta_", level = conf.level, method = ifelse(method == "wald", "Wald", method), nsim = R, boot.type = boot)),

                                                                       error = function(y) {

                                                                         if (isTRUE(method == "profile")) {

                                                                           warning("Profile confidence interval computation failed, switched to Wald confidence intervals.", call. = FALSE)

                                                                         } else if (isTRUE(method == "boot")) {

                                                                           warning("Bootstrap confidence interval computation failed, switched to Wald confidence intervals.", call. = FALSE)

                                                                         }

                                                                           method <<- "wald"

                                                                           lme4::confint.merMod(model, parm = "beta_", level = conf.level, method = "Wald", nsim = R, boot.type = boot)

                                                                       })), nm = c("Low", "Upp")))

                  } else {

                    print <- setdiff(print, "confint")

                    if (isTRUE(length(print) == 0L)) { stop("Profile confidence intervals are not available when model is singular or not converged.", call. = FALSE) }

                  }

               ##### Wald CI
               } else {

                 # Regular Standard Errors
                 if (isTRUE(!robust)) {

                   modcoef <- data.frame(modcoef, setNames(as.data.frame(suppressMessages(lme4::confint.merMod(model, parm = "beta_", level = conf.level, method = "Wald"))), nm = c("Low", "Upp")))

                 # CR2 Standard Errors
                 # https://stackoverflow.com/questions/3817182/vcovhc-and-confidence-interval
                 } else {

                   modcoef <- data.frame(modcoef, setNames(as.data.frame(coef(model.summary)[, "Estimate"] + sqrt(diag(model.robust$sandwich)) %o% qnorm(c((1L - conf.level) / 2L, 1L - (1L - conf.level) / 2L))), nm = c("Low", "Upp")))

                 }

               }

              }

             #...................
             #### Standardized Coefficients ####

             if (isTRUE("stdcoef" %in% print)) {

               # Two-Level Model
               if (isTRUE(model.twolevel)) {

                 modcoef <- data.frame(modcoef, misty::coeff.std(model, check = FALSE, output = FALSE) |>
                                         (\(p) if (isTRUE(all(p$args$print == "stdyx"))) {

                                           p$result[, setdiff(colnames(p$result), c("Estimate", "Std. Error", "df", "t value", "Pr(>|t|)", "StdX", "StdY"))]

                                         } else if (isTRUE(all(c("stdy", "stdyx") %in% p$args$print))) {

                                           p$result[, setdiff(colnames(p$result), c("Estimate", "Std. Error", "df", "t value", "Pr(>|t|)", "StdX"))]

                                         } else if (isTRUE(all(p$args$print == "stdy"))) {

                                           p$result[, setdiff(colnames(p$result), c("Estimate", "Std. Error", "df", "t value", "Pr(>|t|)", "SDx", "StdX", "StdYX"))]

                                         } else if (isTRUE(all(p$args$print == "stdx"))) {

                                           p$result[, !colnames(p$result) %in% setdiff(colnames(p$result), c("Estimate", "Std. Error", "df", "t value", "Pr(>|t|)", "SDy", "StdY", "StdYX"))]

                                         })())

                # Three-Level Model
                } else {

                  print <- setdiff(print, "stdcoef")

                  if (isTRUE(length(print) == 0L)) { stop("Standardized coefficients are only available for two-level models.", call. = FALSE) }

                }

             }

             #...................
             #### Variance Inflation Factor ####

             if (isTRUE("vif" %in% print)) { modcoef <- data.frame(modcoef, VIF = misty::check.collin(model, check = FALSE, output = FALSE)$result$coef[, "aGVIF"]) }

           }

         #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         ## Linear Mixed-Effects Model, lme() ####

         }, lme = {

           #--------------------------------------
           ### Check ####

           # Remove 'vif' from the 'print' argument when only one predictor
           if (isTRUE("vif" %in% print && length(misty::chr.omit(names(nlme::fixef(model)), omit = "(Intercept)")) <= 1L)) {

             print <- misty::chr.omit(print, omit = "vif", check = FALSE)

             if (isTRUE(length(print) == 0L)) { stop("Variance inflation factor can only be computed for models with at least two predictors.", call. = FALSE) }

           }

           # Remove 'cormat' and 'stdcoef' from the 'print' argument when no predictors
           if (isTRUE(any(c("cormat", "stdcoef") %in% print) && length(misty::chr.omit(names(nlme::fixef(model)), omit = "(Intercept)")) == 0L)) {

             print <- misty::chr.omit(print, omit = c("cormat", "stdcoef"), check = FALSE)

             if (isTRUE(length(print) == 0L)) { stop("Correlation matrix and standardized coefficients are not available for the null model.", call. = FALSE) }

           }

           #--------------------------------------
           ### Model Summary and Data ####

           # Model summary
           model.summary <- summary(model)

           # Two-level model
           model.twolevel <- ifelse(ncol(model$groups) == 1L, TRUE, FALSE)

           # Cluster name
           model.cluster <- names(model$groups) |> (\(p) if (isTRUE(length(p) > 1L)) { names(sort(sapply(model$groups, misty::uniq.n), decreasing = TRUE)) } else { return(p) } )()

           # Name of the data
           model.data.name <- as.character(stats::getCall(model))[3L]

           # Model Data
           model.data <- nlme::getData(model)

           # Outcome and predictor variables
           model.data.yx <- model.data[, all.vars(formula(model)), drop = FALSE]

           #--------------------------------------
           ### Level of the Predictors ####

           #...................
           #### Two-Level Model

           if (isTRUE(model.twolevel)) {

             # Level of Variables, 1 = Level-1 and 2 = Level-2 variable
             var.level <- sapply(colnames(model.data.yx), function(y) { if (all(tapply(as.numeric(model.data[, y]), model.data[, model.cluster], var, na.rm = TRUE) < .Machine$double.eps^0.5, na.rm = TRUE)) { 2L } else { 1L } })

           #...................
           #### Three-Level Model

           } else {

             # Level of Variables, 1 = Level-1, 2 = Level-2 variable, and 3 = Level-3 variable
             var.level <- sapply(colnames(model.data.yx), function(y) {

               if (isTRUE(any(na.omit(as.vector(tapply(as.numeric(model.data[, y]), apply(model.data[, model.cluster], 1L, paste, collapse = ""), var, na.rm = TRUE))) > .Machine$double.eps^0.5))) {

                 1L

               # Level 2 Variable
               } else if (isTRUE(all(na.omit(as.vector(tapply(as.numeric(model.data[, y]), apply(model.data[, model.cluster], 1L, paste, collapse = ""), var, na.rm = TRUE))) < .Machine$double.eps^0.5) && any(as.vector(tapply(model.data[, y], model.data[, model.cluster[2L]], var, na.rm = TRUE)) != 0L))) {

                 2L

               # Level 3 Variable
               } else if (isTRUE(all(na.omit(as.vector(tapply(as.numeric(model.data[, y]), apply(model.data[, model.cluster], 1L, paste, collapse = ""), var, na.rm = TRUE))) < .Machine$double.eps^0.5) && all(na.omit(as.vector(tapply(model.data[, y], model.data[, model.cluster[1L]], var, na.rm = TRUE))) < .Machine$double.eps^0.5))) {

                 3L

               }

             })

           }

           # Level-1 variable names
           var.level1 <- names(which(var.level == 1L))

           # Level-2 variable names
           var.level2 <- names(which(var.level == 2L))

           # Level-3 variable names
           var.level3 <- names(which(var.level == 3L))

           # Factors or Character
           var.factor <- names(which(!sapply(model.data.yx, is.numeric)))

           #--------------------------------------
           ### Cluster-Robust Standard Errors ####

           if (isTRUE(robust)) { model.robust <- misty::coeff.robust(model, output = FALSE)$result }

           #--------------------------------------
           ### Model Not Converged or Singular ####
           #
           # Info not available in the lme object

           #--------------------------------------
           ### Call ####

           if (isTRUE("call" %in% print)) { call <- as.character(stats::getCall(model)) |> (\(p) list(formula = paste0(p[2L], " + (", sub("~", "", p[4L]), ")"), data = p[3L]))() }

           #--------------------------------------
           ### Descriptive Statistics ####

           if (isTRUE("descript" %in% print)) {

             #...................
             #### Two-Level Model

             if (isTRUE(model.twolevel)) {

               # Level-1 variables
               descript.l1 <- data.frame(suppressWarnings(misty::descript(model.data[, setdiff(var.level1, var.factor), drop = FALSE], check = FALSE, output = FALSE))$result[, c("variable", "n", "nUQ", "m", "sd", "min", "p.min", "max", "p.max", "skew", "kurt")], icc = misty::multilevel.icc(model.data[, setdiff(var.level1, var.factor)], cluster = model.data[, model.cluster]), row.names = setdiff(var.level1, var.factor))

               # Level-2 variables
               if (isTRUE(length(var.level2) != 0L)) { descript.l2 <- data.frame(suppressWarnings(misty::descript(model.data[!duplicated(model.data[, model.cluster[1L]]), var.level2, drop = FALSE], check = FALSE, output = FALSE))$result[, c("variable", "n", "nUQ", "m", "sd", "min", "p.min", "max", "p.max", "skew", "kurt")], icc = NA, row.names = var.level2) } else { descript.l2 <- NULL }

               # Level-1 and Level-2 variables
               descript <- data.frame(rbind(descript.l1, descript.l2)[setdiff(colnames(model.data.yx), var.factor), ], row.names = NULL)

             #...................
             #### Three-Level Model

             } else {

               # Level-1 variables
               descript.l1 <- data.frame(suppressWarnings(misty::descript(model.data[, setdiff(var.level1, var.factor), drop = FALSE], check = FALSE, output = FALSE))$result[, c("variable", "n", "nUQ", "m", "sd", "min", "p.min", "max", "p.max", "skew", "kurt")], setNames(as.data.frame(t(misty::multilevel.icc(model.data[, setdiff(var.level1, var.factor)], cluster = model.data[, rev(model.cluster)]))[, model.cluster]), nm = c("icc.l2", "icc.l3")), row.names = setdiff(var.level1, var.factor))

               # Level-2 variables
               if (isTRUE(length(var.level2) != 0L)) { descript.l2 <- data.frame(suppressWarnings(misty::descript(model.data[!duplicated(model.data[, model.cluster[1L]]), var.level2, drop = FALSE], check = FALSE, output = FALSE))$result[, c("variable", "n", "nUQ", "m", "sd", "min", "p.min", "max", "p.max", "skew", "kurt")], icc.l2 = NA, icc.l3 = misty::multilevel.icc(model.data[, setdiff(var.level2, var.factor)], cluster = model.data[, model.cluster[2L]]), row.names = var.level2) } else { descript.l2 <- NULL }

               # Level-3 variables
               if (isTRUE(length(var.level3) != 0L)) { descript.l3 <- data.frame(suppressWarnings(misty::descript(model.data[!duplicated(model.data[, model.cluster[2L]]), var.level3, drop = FALSE], check = FALSE, output = FALSE))$result[, c("variable", "n", "nUQ", "m", "sd", "min", "p.min", "max", "p.max", "skew", "kurt")], icc.l2 = NA, icc.l3 = NA, row.names = var.level3) } else { descript.l3 <- NULL }

               # Level-1, Level-2, and Level-3 variables
               descript <- data.frame(rbind(descript.l1, descript.l2, descript.l3)[setdiff(colnames(model.data.yx), var.factor), ], row.names = NULL)

             }

           }

           #--------------------------------------
           ### Within-Group and Between-Group Correlation Matrix ####

           if (isTRUE("cormat" %in% print)) {

             # Two-Level Model and Variables after excluding factors
             if (isTRUE(model.twolevel && length(setdiff(union(var.level1, var.level2), var.factor)) >= 2L)) {

               tryCatch(cormat <- suppressWarnings(misty::multilevel.cor(model.data[, setdiff(c(all.vars(formula(model)), model.cluster), var.factor)], cluster = model.cluster, optim.switch = FALSE, check = FALSE, output = FALSE))$result$wb.cor,

                        error = function(y) {

                          warning("Within- and between-group correlation matrix computation failed.", call. = FALSE)

                          print <<- setdiff(print, "cormat")

                          cormat <<- NULL

                        })

             } else {

               cormat <- NULL

               print <- setdiff(print, "cormat")

               if (isTRUE(length(print) == 0L)) {

                 # Two-Level Model
                 if (isTRUE(model.twolevel)) {

                   stop("After excluding factors, there are no variables left for computing the within- and between-group correlation matrix.", call. = FALSE)

                 # Three-Level Model
                 } else {

                   stop("Within- and between-group correlation matrix is only available for two-level models.", call. = FALSE)

                 }

               }

             }

           }

           #--------------------------------------
           ### Model Summary ####

           if (isTRUE("modsum" %in% print)) {

             #...................
             #### Number of Cases and Missing Values ####

             modsum <- data.frame(n = nobs(model), nNA = length(model$na.action))

             #...................
             #### Number of Clusters, Log-Likelihood, and R-Square ####

             # Two-Level Model
             if (isTRUE(model.twolevel)) {

               modsum <- data.frame(modsum, nCl = misty::uniq.n(nlme::getGroups(model)),
                                    npar = attributes(logLik(model))$df,
                                    method = ifelse(all(model$method == "REML"), "REML", "FML"),
                                    loglik = model.summary$logLik, deviance = -2*model.summary$logLik,
                                    setNames(misty::multilevel.r2(model, print = "NS", check = FALSE, output = FALSE)$result$ns, nm = c("margR2", "condR2")) , row.names = NULL)

             # Three-Level Model
             } else {

               modsum <- data.frame(modsum, nCl2 = misty::uniq.n(model.data[, model.cluster[1L]]), nCl3 = misty::uniq.n(model.data[, model.cluster[2L]]),
                                    npar = attributes(logLik(model))$df,
                                    method = ifelse(all(model$method == "REML"), "REML", "FML"),
                                    loglik = model.summary$logLik, deviance = -2*model.summary$logLik, row.names = NULL)

             }

           }

           #--------------------------------------
           ### Random Effects ####

           if (isTRUE("randeff" %in% print)) {

             #...................
             #### Two-Level Model

             if (isTRUE(model.twolevel)) {

               randeff <- data.frame(groups = c(unlist(sapply(names(model$groups), function(y) c(y, rep(NA, times = nrow(VarCorr(model)) - 2L)))), "Residual"),
                                     name = c(names(VarCorr(model)[, 2]) |> (\(p) p[-length(p)] )(), "NA"),
                                     var = as.numeric(unclass(VarCorr(model))[, "Variance"]),
                                     sd = as.numeric(unclass(VarCorr(model))[, "StdDev"]),
                                     misty::df.rename(unclass(VarCorr(model)) |> (\(p) p[, which(!colnames(p) %in% c("Variance", "StdDev")), drop = FALSE])(), from = "Corr", to = "cor", check = FALSE) |> (\(q) setNames(as.data.frame(q), nm = colnames(q)))(), row.names = NULL, check.names = FALSE, fix.empty.names = FALSE)

             #...................
             #### Three-Level Model

             } else {

               var.corr <- VarCorr(model)[row.names(VarCorr(model)) %in% c(names(nlme::fixef(model)), "Residual") , ]

               randeff <- data.frame(groups = c(names(model$groups)[1L], rep(NA, times = grep("(Intercept)", row.names(var.corr))[2L] - 2L), names(model$groups)[2L], rep(NA, times = nrow(var.corr) - grep("(Intercept)", row.names(var.corr))[2L] - 1L), "Residual"),
                                     name = sub("Residual", NA, row.names(var.corr)),
                                     var = as.numeric(var.corr[, "Variance"]),
                                     sd = as.numeric(var.corr[, "StdDev"]),
                                     misty::df.rename(var.corr |> (\(p) p[, which(!colnames(p) %in% c("Variance", "StdDev")), drop = FALSE])(), from = "Corr", to = "cor", check = FALSE) |> (\(q) setNames(as.data.frame(q), nm = colnames(q)))(), row.names = NULL, check.names = FALSE, fix.empty.names = FALSE)

             }

           }

           #--------------------------------------
           ### Variance and Correlation Structure ####

           if (isTRUE("varcor" %in% print)) {

             corstruct <- varstruct <- NULL

              # Correlation Structure
              # https://stat.ethz.ch/R-manual/R-devel/library/nlme/html/corClasses.html
              if (isTRUE(!is.null(model$modelStruct$corStruct))) {

                corstruct <- list(class = sub("~", "~ ", as.character(stats::getCall(model)) |> (\(p) p[misty::chr.grep(c("corAR1", "corARMA", "corCAR1", "corCompSymm", "corExp", "corGaus", "corLin", "corRatio", "corSpher", "corSymm"), p)])()),
                                  corstruct = setNames(data.frame(nlme::corMatrix(model$modelStruct$corStruct)[[1L]], row.names = colnames(nlme::getVarCov(model, type = "conditional")[[1L]])), nm = colnames(nlme::getVarCov(model, type = "conditional")[[1L]])))

              }

              # Variance function structure
              # https://stat.ethz.ch/R-manual/R-devel/RHOME/library/nlme/html/varClasses.html
              if (isTRUE(!is.null(model$modelStruct$varStruct))) {

                varstruct <- list(class = sub("~", "~ ", as.character(stats::getCall(model)) |> (\(p) p[misty::chr.grep(c("varExp", "varPower", "varConstPower", "varConstProp", "varIdent", "varFixed", "varComb"), p)])()),
                                  varstruct = utils::capture.output(model$modelStruct$varStruct) |>
                                    (\(p) {

                                      if (isTRUE(attributes(model$modelStruct$varStruct)$class[1L] %in% c("varExp", "varPower", "varConstPower", "varConstProp", "varIdent"))) {

                                        as.data.frame(matrix(as.numeric(misty::chr.omit(unlist(strsplit(misty::chr.trim(p[3L]), " ")), check = FALSE)), nrow = 1L, dimnames = list(NULL, misty::chr.omit(strsplit(misty::chr.trim(p[2L]), " "), check = FALSE))))

                                      } else if (isTRUE(attributes(model$modelStruct$varStruct)$class[1L] == "varComb")) {

                                        temp <- sapply(grep("Variance function structure", p), function(y) {

                                          as.data.frame(matrix(as.numeric(misty::chr.omit(unlist(strsplit(misty::chr.trim(p[y + 2L]), " ")), check = FALSE)), nrow = 1L, dimnames = list(NULL, misty::chr.omit(strsplit(misty::chr.trim(p[y + 1L]), " "), check = FALSE))))

                                        })

                                        names(temp) <- sapply(grep("Variance function structure", p), function(y) { sub("representing", "", p[y]) })

                                        return(temp)

                                      } else if (isTRUE(attributes(model$modelStruct$varStruct)$class[1L] == "varFixed")) {

                                        NULL

                                      }})())

              }

              varcor <- list(varstruct = varstruct, corstruct = corstruct)

           }

           #--------------------------------------
           ### Coefficients ####

           if (isTRUE(any(c("coef", "confint", "stdcoef", "vif") %in% print))) {

             #...................
             #### Unstandardized Coefficients ####

             ##### Regular standard errors
             if (isTRUE(!robust)) {

               modcoef <- setNames(as.data.frame(coef(model.summary)), nm = c("Estimate", "SE", "df", "t", "p"))

             ##### Cluster-robust standard errors
             } else {

               modcoef <- model.robust$coef

             }

             #...................
             #### Confidence intervals ####

             if (isTRUE("confint" %in% print)) {

               # Regular Standard Errors
               if (isTRUE(!robust)) {

                 modcoef <- data.frame(modcoef, setNames(as.data.frame(nlme::intervals(model, level = conf.level, which = "fixed")$fixed[, c("lower", "upper")]), nm = c("Low", "Upp")))

               # CR2 Standard Errors
               # https://stackoverflow.com/questions/3817182/vcovhc-and-confidence-interval
               } else {

                 modcoef <- data.frame(modcoef, setNames(as.data.frame(coef(model.summary)[, "Estimate"] + sqrt(diag(model.robust$sandwich)) %o% qnorm(c((1L - conf.level) / 2L, 1L - (1L - conf.level) / 2L))), nm = c("Low", "Upp")))

               }

             }

             #...................
             #### Standardized Coefficients ####

             if (isTRUE("stdcoef" %in% print)) {

               # Two-Level Model
               if (isTRUE(model.twolevel)) {

                 modcoef <- data.frame(modcoef, misty::coeff.std(model, check = FALSE, output = FALSE) |>
                                         (\(p) if (isTRUE(all(p$args$print == "stdyx"))) {

                                           p$result[, setdiff(colnames(p$result), c("Value", "Std.Error", "DF", "t-value", "p-value", "StdX", "StdY"))]

                                         } else if (isTRUE(all(c("stdy", "stdyx") %in% p$args$print))) {

                                           p$result[, setdiff(colnames(p$result), c("Value", "Std.Error", "DF", "t-value", "p-value", "StdX"))]

                                         } else if (isTRUE(all(p$args$print == "stdy"))) {

                                           p$result[, setdiff(colnames(p$result), c("Value", "Std.Error", "DF", "t-value", "p-value", "SDx", "StdX", "StdYX"))]

                                         } else if (isTRUE(all(p$args$print == "stdx"))) {

                                           p$result[, !colnames(p$result) %in% setdiff(colnames(p$result), c("Value", "Std.Error", "DF", "t-value", "p-value", "SDy", "StdY", "StdYX"))]

                                         })())

               # Three-Level Model
               } else {

                 print <- setdiff(print, "stdcoef")

                 if (isTRUE(length(print) == 0L)) { stop("Standardized coefficients are only available for two-level models.", call. = FALSE) }

               }

             }

             #...................
             #### Variance Inflation Factor ####

             if (isTRUE("vif" %in% print)) { modcoef <- data.frame(modcoef, VIF = misty::check.collin(model, check = FALSE, output = FALSE)$result$coef[, "aGVIF"]) }

        }

      })

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "summa",
                 model = model,
                 args = list(print = print, robust = robust, ddf = ddf, method = method, conf.level = conf.level, R = R, boot = boot, seed = seed, digits = digits, p.digits = p.digits, write = write, append = append, check = check, output = output),
                 result = list(call = call, descript = descript, cormat = cormat, modsum = modsum, randeff = randeff, varcor = varcor, coef = modcoef, weights = weights, converg = converg))

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
