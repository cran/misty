#' Robust Estimation of Multilevel and Linear Mixed-Effects Models
#'
#' This function estimates a multilevel and linear mixed-effects model based on
#' a robust estimation method using the \code{rlmer()} function from the
#' \pkg{robustlmm} package that down-weights observations depending on robustness
#' weights computed by robustification of the scoring equations and an application
#' of the Design Adaptive Scale (DAS) approach.
#'
#' @param model    a fitted model of class \code{"lmerMod"} or \code{"lmerModLmerTest"}.
#' @param method   a character string indicating the method used for estimating
#'                 theta and sigma, i.e., \code{"DAStau"} (default) for using
#'                 numerical quadrature for computing the consistency factors and
#'                 \code{"DASvar"} for computing the consistency factors using a
#'                 direct approximation. Note that \code{"DAStau"} is slower than
#'                 \code{"DASvar"} but yields more accurate results. However,
#'                 complex models with correlated random effects with more than
#'                 one correlation term can only estimated using \code{"DASvar"}.
#'                 See help page of the \code{rlmer()} function in the R package
#'                 \code{robustlmm} for more details.
#' @param setting  a character string indicating the setting for the parameters
#'                 used for computing the robustness weights, i.e., \code{"RSEn"}
#'                 (default)  and \code{"RSEa"} for higher asymptotic efficiency
#'                 which results in lower robustness. See help page of the
#'                 \code{rlmer()} function in the R package \code{robustlmm}
#'                 for more details.
#' @param digits   an integer value indicating the number of decimal places to
#'                 be used.
#' @param p.digits an integer value indicating the number of decimal places to
#'                 be used for displaying \emph{p}-value.
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
#' @param output   logical: if \code{TRUE} (default), output is shown on the console.
#'
#' @details
#' \describe{
#' \item{\strong{Function specification and Function Arguments}}{The function
#' \code{rlmer} from the \pkg{robustlmm} package is specified much like the
#' function \code{lmer} from the \code{lme4 package}, i.e., a formula object
#' and data frame is specified as the first and second argument. However, the
#' \code{robust.lmer} function requires a fitted \code{"lmerMod"} or
#' \code{"lmerModLmerTest"} that is used to re-estimate the model using the
#' robust estimation method. Note that the function \code{rlmer} provides
#' the additional arguments \code{rho.e}, \code{rho.b}, \code{rho.sigma.e},
#' \code{rho.sigma.b}, \code{rel.tol}, \code{max.iter}, \code{verbose},
#' \code{doFit}, \code{init}, and \code{initTheta} that are not supported by
#' the \code{robust.lmer} function. See help page of the \code{rlmer()} function
#' in the R package \code{robustlmm} for more details.}
#' \item{\strong{Statistical Significance Testing}}{
#' The function \code{rlmer} from the \pkg{robustlmm} package does not provide
#' any degrees of freedom or significance values. When specifying a \code{"lmerModLmerTest"}
#' object for the argument \code{model}, the \code{robust.lmer} function uses the
#' Satterthwaite or Kenward-Roger degrees of freedom from the \code{"lmerModLmerTest"}
#' object to compute significance values for the regression coefficients based on
#' parameter estimates and standard error of the robust multilevel mixed-effects
#' (see Sleegers et al. (2021).}
#' }
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{coeff.robust}}, \code{\link{summa}}
#'
#' @references
#' Koller, M. (2016). robustlmm: An R Package for Robust Estimation of Linear
#' Mixed-Effects Models. \emph{Journal of Statistical Software, 75}(6), 1–24.
#' https://doi.org/10.18637/jss.v075.i06
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{model}}{object returned from the \code{rlmer} function}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{result}}{list with results, i.e., \code{call} for the the function
#' call, \code{randeff} for the variance and correlation components, \code{coef}
#' for the model coefficients, \code{weights} for the robustness weights, and
#' and \code{converg} for the convergence check, i.e., \code{1} = model converged,
#' \code{0} = model singular, and \code{-1} model not converged.}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load lme4, lmerTest, and misty package
#' misty::libraries(lme4, lmerTest, misty)
#'
#' # Load data set "Demo.twolevel" in the lavaan package
#' data("Demo.twolevel", package = "lavaan")
#'
#' #----------------------------------------------------------------------------
#' # Multilevel and Linear Mixed-Effects Model
#'
#' # Cluster-mean centering, center() from the misty package
#' Demo.twolevel <- center(Demo.twolevel, x2, type = "CWC", cluster = "cluster")
#'
#' # Grand-mean centering, center() from the misty package
#' Demo.twolevel <- center(Demo.twolevel, w1, type = "CGM", cluster = "cluster")
#'
#' # Estimate two-level mixed-effects model
#' mod.lmer2 <- lmer(y1 ~ x2.c + w1.c + x2.c:w1.c + (1 + x2.c | cluster), data = Demo.twolevel)
#'
#' # Example 1a: Default setting
#' mod.lmer2r <- robust.lmer(mod.lmer2)
#'
#' # Example 1b: Extract robustness weights
#' mod.lmer2r$result$weight$iresid
#' mod.lmer2r$result$weight$iranef
#'
#' #----------------------------------------------------------------------------
#' # Write Results
#'
#' # Example 2a: Write results into a text file
#' robust.lmer(mod.lmer2, write = "Robust_lmer.txt", output = FALSE)
#'
#' # Example 2b: Write results into a Excel file
#' robust.lmer(mod.lmer2, write = "Robust_lmer.xlsx", output = FALSE)
#' }
robust.lmer <- function(model, method = c("DAStau", "DASvar"), setting = c("RSEn", "RSEa"),
                        digits = 2, p.digits = 3, write = NULL, append = TRUE,
                        check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'model' is missing or NULL
  if (isTRUE(missing(model) ||is.null(model))) { stop("Input for the argument 'model' is missing.", call. = FALSE) }

  # Check if input 'model' is not ''lmerMod' or 'lmerModLmerTest'
  if (isTRUE(!any(class(model) %in% c("lmerMod", "lmerModLmerTest")))) { stop("Please specify a \"lmerMod\" or \"lmerModLmerTest\" object for the argument 'model'.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("append"), s.character = list(method = c("DAStau", "DASvar"), setting = c("RSEn", "RSEa")),
               args = c("digits", "p.digits", "write2"), package = "robustlmm", envir = environment(), input.check = check)

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Argument 'method' ####

  method <- ifelse(all(c("DAStau", "DASvar") %in% method), "DAStau", method)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Argument 'setting' ####

  setting <- ifelse(all(c("RSEn", "RSEa") %in% setting), "RSEn", setting)

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Call ####

  call <- as.character(stats::getCall(model)) |> (\(y) list(formula = y[2L], data = y[3L]))()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Robust Estimation ####

  model.r <-  eval(parse(text = paste0("suppressWarnings(suppressMessages(robustlmm::rlmer(", call$formula, ", data = model.frame(model)", ", method = \"", method, "\", setting = \"", setting, "\")))")))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Random Effects ####

  randeff <- data.frame(groups = c(unlist(sapply(names(VarCorr(model.r)), function(y) c(y, rep(NA, times = nrow(VarCorr(model.r)[[y]]) - 1L)))), "Residual"),
                        name = c(unlist(sapply(names(VarCorr(model.r)), function(y) names(attr(VarCorr(model.r)[[y]], which = "stddev")))), "NA"),
                        var = c(unlist(sapply(names(VarCorr(model.r)), function(y) attr(VarCorr(model.r)[[y]], which = "stddev"))), attr(VarCorr(model.r), which = "sc"))^2,
                        sd = c(unlist(sapply(names(VarCorr(model.r)), function(y) attr(VarCorr(model.r)[[y]], which = "stddev"))), attr(VarCorr(model.r), which = "sc")),
                        do.call("rbind", unique(unlist(sapply(names(VarCorr(model.r)), function(y) colnames(attr(VarCorr(model.r)[[y]], which = "correlation"))))) |>
                                  (\(z) lapply(names(VarCorr(model.r)), function(w) attr(VarCorr(model.r)[[w]], which = "correlation") |> (\(q) if (isTRUE(!setequal(colnames(q), z))) { misty::df.rename(setNames(data.frame(q, matrix(NA, ncol = length(setdiff(z, colnames(q))), nrow = nrow(q))), nm = c(colnames(q), setdiff(z, colnames(q)))), from = "(Intercept)", to = "cor") } else { misty::df.rename(q, from = "(Intercept)", to = "cor") })()))()) |>
                          (\(p) rbind(p, setNames(rep(NA, times = ncol(p)), nm = colnames(p))))(), check.names = FALSE, row.names = NULL)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Unstandardized Coefficients ####

  # Coefficients of regular estimation
  modcoef <- as.data.frame(coef(summary(model)))

  # Coefficients of robust estimation
  modcoef.r <- setNames(as.data.frame(coef(summary(model.r))), nm = c("Estimate", "SE", "t"))

  #...................
  ### Degrees of Freedom ####

  if (isTRUE("df" %in% colnames(modcoef))) { modcoef.r <- misty::df.move(cbind(modcoef.r, df = modcoef$df, p = pt(abs(modcoef.r$t), modcoef$df, lower.tail = FALSE)*2L), df, after = "SE") }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Robustness Weights ####

  # Two-Level Model
  if (isTRUE(lme4::getME(model, name = "n_rtrms") == 1L)) {

    weights <- list(iresid = lme4::getME(model.r, "w_e"),
                    iranef = lme4::getME(model.r, "w_b")[[1L]][, 1L] ,
                    resid = lme4::getME(model.r, "w_e") |> (\(p) list(ew1 = sum(p == 1L), ew0 = sum(p != 1L), pdescript = if (isTRUE(sum(p != 1L) >= 2L)) { misty::descript(p[p != 1L], output = FALSE)$result[, c("m", "sd", "min", "p25", "med", "p75", "max", "range", "iqr")] } else { pdesscript = p[p != 1] }))(),
                    ranef = lme4::getME(model.r, "w_b")[[1L]][, 1L] |> (\(p) list(bw1 = sum(p == 1L), bw0 = sum(p != 1L), bdescript = if (isTRUE(sum(p != 1L) >= 2L)) { misty::descript(p[p != 1L], output = FALSE)$result[, c("m", "sd", "min", "p25", "med", "p75", "max", "range", "iqr")] } else { pdesscript = p[p != 1] }))())

  # Three-Level Model
  } else {

    weights <- list(iresid = lme4::getME(model.r, "w_e"),
                    iranef1 = lme4::getME(model.r, "w_b")[[1L]][, 1L],
                    iranef2 = lme4::getME(model.r, "w_b")[[2L]][, 1L],
                    resid  = lme4::getME(model.r, "w_e") |> (\(p) list(ew1 = sum(p == 1L), ew0 = sum(p != 1L), pdescript = if (isTRUE(sum(p != 1L) >= 2L)) { misty::descript(p[p != 1L], output = FALSE)$result[, c("m", "sd", "min", "p25", "med", "p75", "max", "range", "iqr")] } else { pdesscript = p[p != 1] }))(),
                    ranef1 = lme4::getME(model.r, "w_b")[[1L]][, 1L] |> (\(p) list(b1w1 = sum(p == 1L), b1w0 = sum(p != 1L), b1descript = if (isTRUE(sum(p != 1L) >= 2L)) { misty::descript(p[p != 1L], output = FALSE)$result[, c("m", "sd", "min", "p25", "med", "p75", "max", "range", "iqr")] } else { pdesscript = p[p != 1] }))(),
                    ranef2 = lme4::getME(model.r, "w_b")[[2L]][, 1L] |> (\(p) list(b2w1 = sum(p == 1L), b2w0 = sum(p != 1L), b2descript = if (isTRUE(sum(p != 1L) >= 2L)) { misty::descript(p[p != 1L], output = FALSE)$result[, c("m", "sd", "min", "p25", "med", "p75", "max", "range", "iqr")] } else { pdesscript = p[p != 1] }))())

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model Not Converged or Singular ####

  # -1 = not converged, 0 = singular, 1 = model converged
  converg <- if (isTRUE(!is.null(unlist(model@optinfo$conv$lme4)) && any(grepl("-1", unlist(model@optinfo$conv$lme4))))) { -1L } else if (isTRUE(!is.null(unlist(model@optinfo$conv$lme4)))) { 0L } else { 1L }


  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "robust.lmer",
                 model = model.r,
                 args = list(method = method, setting = setting, digits = digits, p.digits = p.digits, write = write, append = append, check = check, output = output),
                 result = list(call = call, randeff = randeff, coef = modcoef.r, weights = weights, converg = converg))

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
