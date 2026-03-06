#' Chi-Bar-Square Difference Test
#'
#' This function performs the chi-bar-square difference test to compare the random
#' intercept cross-lagged panel model (RI-CLPM) and traditional cross-lagged panel
#' model (CLPM) as discussed in Hamaker et al. (2015).
#'
#' @param clpm     an object of class lavaan, i.e., a fitted random intercept
#'                 cross-lagged panel model (RI-CLPM) with the variance and
#'                 covariances of latent intercept factors fixed to zero. Note
#'                 that a RI-CLPM with the variance of all random intercepts
#'                 fixed to zero is statistically equivalent to the traditional
#'                 cross-lagged panel model (CLPM).
#' @param riclpm   an object of class lavaan, i.e., a fitted random intercept
#'                 cross-lagged panel model with variance and covariances of
#'                 latent intercept factors freely estimated.
#' @param alpha    a numeric value indicating the type-I-risk, \eqn{\alpha}.
#' @param digits   an integer value indicating the number of decimal places
#'                 to be used for displaying results.
#' @param p.digits an integer value indicating the number of decimal places
#'                 to be used for displaying the \emph{p}-values.
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
#' @param check    logical: if \code{TRUE} (default), argument specification is checked.
#' @param output   logical: if \code{TRUE} (default), output is shown on the console.
#'
#' @details
#' \describe{
#' \item{\strong{Random Intercept Cross-Lagged Panel Model (RI-CLPM)}}{The RI-CLPM
#' is an extension of the traditional cross-lagged panel model that disentangles the
#' within-person process from stable between-person differences (Hamaker et al., 2015).
#' In a bivariate RI-CLPM, each variable \eqn{x} and \eqn{y} is decomposed into a
#' stable time-invariant trait-like component, captured with random intercept factors
#' denoted by \eqn{\kappa} for variable \eqn{x} and \eqn{\omega} for variable \eqn{y}
#' (see Figure 1 in Hamaker et al., 2015). Note that the CLPM is nested under the
#' RI-CLPM, i.e., the RI-CLPM is statistically equivalent to the CLPM when fixing
#' the variance of all random intercepts and their covariances to zero.}
#'
#' \item{\strong{Chi-Bar-Square Difference Test}}{The \eqn{\bar{\chi^2}} difference
#' test is used to compare the fit of the nested models CLPM and RI-CLPM based on
#' a mixture of chi-square distributions to test the null hypothesis e.g.,
#' \eqn{H_0: Var_{\kappa} = 0, Var_{\omega} = 0, Cov_{\kappa, \omega} = 0}.
#' The chi-bar-square distribution is a weighted sum of different chi-square distributions
#' with varying degrees of freedom resulting from parameters fixed at the boundaries
#' of the parameter space as variances are non-negative values (Stoel et al., 2016).}
#'
#' \item{\strong{Chi-Square Difference Test}}{The regular \eqn{\chi^2} difference
#' test is conservative due to ignoring the mixture distribution, i.e., if it is
#' statistically significant, we are certain that the chi-bar-square difference test
#' will be significant too, while the reverse will not be the case. Accordingly, if
#' researchers find it more important to detect a true CLPM than a true RI-CLPM, it
#' is advised to use the regular chi-square difference test (Sukpan & Kuiper, 2026).
#' It should also be mentioned that estimating a RI-CLPM when the CLPM is the true
#' model may reduce statistical power due to estimating additional parameters, but
#' it does not introduce bias (see Table 4 in Scott, 2021), while estimating a CLPM
#' when the RI-CLPM is the true model introduces bias (see Table 3 in Scott, 2021).}
#' }
#'
#' @author
#' Takuya Yanagida
#'
#' @references
#' Hamaker, E. L., Kuiper, R. M., & Grasman, R. P. (2015). A critique of the
#' cross-lagged panel model. \emph{Psychological Methods, 20}(1), 102-116.
#' https://doi.org/10.1037/a0038889
#'
#' Kuiper R (2026). \emph{ChiBarSq.DiffTest: Chi-bar-square difference test of the
#' RI-CLPM versus the CLPM and more general}. R package version 0.0.0.9000.
#' https://github.com/rebeccakuiper/ChiBarSq.DiffTest
#'
#' Mulder, J. D., & Hamaker, E. L. (2021). Three extensions of the random intercept
#' cross-lagged panel model. \emph{Structural Equation Modeling: A Multidisciplinary Journal, 28}(4),
#' 638-648. https://doi.org/10.1080/10705511.2020.1784738
#'
#' Scott, P. W. (2021). Accounting for time-varying inter-individual differences
#' in trajectories when assessing cross-lagged models. \emph{Structural Equation Modeling, 28}(3),
#' 365-375. https://doi.org/10.1080/10705511.2020.1819815
#'
#' Stoel, R. D., Garre, F. G., Dolan, C., & van den Wittenboer, G. (2006). On
#' the likelihood ratio test in structural equation modeling when parameters
#' are subject to boundary constraints. \emph{Psychological Methods, 11}(4),
#' 439-455. https://doi.org/10.1037/1082-989X.11.4.439
#'
#' Sukpan, C., & Kuiper, R. M. (2026). Selecting the correct RI-CLPM using
#' chi-square-type tests and AIC-type criteria. \emph{Structural Equation Modeling: A Multidisciplinary Journal},
#' 1-14. https://doi.org/10.1080/10705511.2025.2592831
#'
#' @note This function is based on modified copies of the function \code{ChiBarSq.DiffTest}
#' from the \pkg{ChiBarSq.DiffTest} package by Rebecca M. Kuiper.
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#'
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{model}}{data frame including all variables used in the analysis, i.e.,
#'                    indicators for the factor, grouping variable and cluster
#'                    variable}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{model.fit}}{list of fitted lavaan objects specified in the argument
#'                        \code{riclpm} and \code{clpm}}
#' \item{\code{result}}{list with result tables, i.e., \code{difftest} for the
#'                      chi-bar-square difference test and \code{weights} for the
#'                      weights for the mixture of chi-square mixture distribution}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #----------------------------------------------------------------------------
#' # Step-wise Procedure (Sukpan & Kuiper, 2026)
#' #
#' # Note that only the first step is shown in this example:
#' # - CLPM versus RI-CLPM(Kappa)
#' # - CLPM versus RI-CLPM(Omega)
#' #
#' # Model specification based on code provided on the accompanying website of
#' # Mulder and Hamaker (2021)
#'
#' #..................
#' # Model Specification: Cross-Lagged Panel Model (CLPM)
#' # i.e., Var(Kappa) = 0, Var(Omega) = 0, Cov(Kappa, Omega) = 0
#'
#' mod.clpm <- '
#'   # Create between components (random intercepts)
#'   RIx =~ 1*x1 + 1*x2 + 1*x3
#'   RIy =~ 1*y1 + 1*y2 + 1*y3
#'
#'   # Create within-person centered variables
#'   wx1 =~ 1*x1
#'   wx2 =~ 1*x2
#'   wx3 =~ 1*x3
#'   wy1 =~ 1*y1
#'   wy2 =~ 1*y2
#'   wy3 =~ 1*y3
#'
#'   # Estimate lagged effects between within-person centered variables
#'   wx2 + wy2 ~ wx1 + wy1
#'   wx3 + wy3 ~ wx2 + wy2
#'
#'   # Estimate covariance between within-person centered variables at first wave
#'   wx1 ~~ wy1 # Covariance
#'
#'   # Estimate covariances between residuals of within-person centered variables
#'   wx2 ~~ wy2
#'   wx3 ~~ wy3
#'
#'   # Fix variance and covariance of random intercepts to zero, i.e.,
##   # CLPM model specification within the RI-CLPM model specification
#'   RIx ~~ 0*RIx
#'   RIy ~~ 0*RIy
#'   RIx ~~ 0*RIy
#'
#'   # Estimate (residual) variance of within-person centered variables
#'   wx1 ~~ wx1
#'   wy1 ~~ wy1
#'   wx2 ~~ wx2
#'   wy2 ~~ wy2
#'   wx3 ~~ wx3
#'   wy3 ~~ wy3
#' '
#'
#' #..................
#' # Model Specification: Random Intercept Cross-Lagged Panel Model RI-CLPM(Kappa)
#' # i.e., Var(Kappa) > 0, Var(Omega) = 0, Cov(Kappa, Omega) = 0
#'
#' mod.ri.clpm.k <- '
#'   # Create between components (random intercepts)
#'   RIx =~ 1*x1 + 1*x2 + 1*x3
#'   RIy =~ 1*y1 + 1*y2 + 1*y3
#'
#'   # Create within-person centered variables
#'   wx1 =~ 1*x1
#'   wx2 =~ 1*x2
#'   wx3 =~ 1*x3
#'   wy1 =~ 1*y1
#'   wy2 =~ 1*y2
#'   wy3 =~ 1*y3
#'
#'   # Estimate lagged effects between within-person centered variables
#'   wx2 + wy2 ~ wx1 + wy1
#'   wx3 + wy3 ~ wx2 + wy2
#'
#'   # Estimate covariance between within-person centered variables at first wave
#'   wx1 ~~ wy1
#'
#'   # Estimate covariances between residuals of within-person centered variables
#'   wx2 ~~ wy2
#'   wx3 ~~ wy3
#'
#'   # Fix variance of random intercept RIy and covariance with RIx to zero
#'   RIx ~~ RIx
#'   RIy ~~ 0*RIy
#'   RIx ~~ 0*RIy
#'
#'   # Estimate (residual) variance of within-person centered variables
#'   wx1 ~~ wx1
#'   wy1 ~~ wy1
#'   wx2 ~~ wx2
#'   wy2 ~~ wy2
#'   wx3 ~~ wx3
#'   wy3 ~~ wy3
#' '
#'
#' #..................
#' # Model Specification: Random Intercept Cross-Lagged Panel Model RI-CLPM(Omega)
#' # i.e., Var(Kappa) = 0, Var(Omega) > 0, Cov(Kappa, Omega) = 0
#'
#' mod.ri.clpm.o <- '
#'   # Create between components (random intercepts)
#'   RIx =~ 1*x1 + 1*x2 + 1*x3
#'   RIy =~ 1*y1 + 1*y2 + 1*y3
#'
#'   # Create within-person centered variables
#'   wx1 =~ 1*x1
#'   wx2 =~ 1*x2
#'   wx3 =~ 1*x3
#'   wy1 =~ 1*y1
#'   wy2 =~ 1*y2
#'   wy3 =~ 1*y3
#'
#'   # Estimate lagged effects between within-person centered variables
#'   wx2 + wy2 ~ wx1 + wy1
#'   wx3 + wy3 ~ wx2 + wy2
#'
#'   # Estimate covariance between within-person centered variables at first wave
#'   wx1 ~~ wy1 # Covariance
#'
#'   # Estimate covariances between residuals of within-person centered variables
#'   wx2 ~~ wy2
#'   wx3 ~~ wy3
#'
#'   # Fix variance of random intercept RIx and covariance with RIy to zero
#'   RIx ~~ 0*RIx
#'   RIy ~~ RIy
#'   RIx ~~ 0*RIy
#'
#'   # Estimate (residual) variance of within-person centered variables
#'   wx1 ~~ wx1
#'   wy1 ~~ wy1
#'   wx2 ~~ wx2
#'   wy2 ~~ wy2
#'   wx3 ~~ wx3
#'   wy3 ~~ wy3
#' '
#'
#' #..................
#' # Estimate Models
#' #
#' # Note that the example analysis cannot be conduct as the data set 'data'
#' # is not available.
#'
#' # CLPM
#' fit.clpm <- lavaan(mod.clpm, data = data, estimator = "MLR")
#'
#' # RI-CLPM(Kappa)
#' fit.ri.clpm.k <- lavaan(mod.ri.clpm.k, data = data, estimator = "MLR")
#'
#' # RI-CLPM(Omega)
#' fit.ri.clpm.o <- lavaan(mod.ri.clpm.o, data = data, estimator = "MLR")
#'
#' #..................
#' # Chi-Bar-Square Difference Test
#'
#' # CLPM vs. RI-CLPM(Kappa)
#' difftest.chibarsq(fit.clpm, fit.ri.clpm.k)
#'
#' # CLPM vs. RI-CLPM(Omega)
#' difftest.chibarsq(fit.clpm, fit.ri.clpm.o)
#' }
difftest.chibarsq <- function(clpm, riclpm, alpha = 0.05, digits = 2, p.digits = 3,
                              write = NULL, append = TRUE, check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Class 'lavaan'
  if (isTRUE(class(riclpm) != "lavaan")) { stop("Please specify an object of class lavaan for the argument 'riclpm'.", call. = FALSE) }
  if (isTRUE(class(clpm) != "lavaan")) { stop("Please specify an object of class lavaan for the argument 'clpm'.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("append", "output"), args = c("alpha", "digits", "p.digits", "write2"), envir = environment(), input.check = check)

  if (isTRUE(check)) {

    # Convergence
    if (isTRUE(!lavaan::lavInspect(riclpm, what = "converged"))) { stop("The model specified in the argument 'riclpm' did not converge.", call. = FALSE) }
    if (isTRUE(!lavaan::lavInspect(clpm, what = "converged"))) { stop("The model specified in the argument 'clpm' did not converge.", call. = FALSE) }

    # Estimator
    if (isTRUE(lavaan::lavInspect(riclpm, what = "options")$estimator != "ML")) { stop("Please use a maximum likelihood estimator for the model specified in 'riclpm'.", call. = FALSE) }
    if (isTRUE(lavaan::lavInspect(clpm, what = "options")$estimator != "ML")) { stop("Please use a maximum likelihood estimator for the model specified in 'clpm'.", call. = FALSE) }

    # Same (robust) estimator
    if (isTRUE(!identical(lavaan::lavInspect(riclpm, what = "options")$test, lavaan::lavInspect(clpm, what = "options")$test))) { stop("Please use the same (robust) estimator for both models specified in 'riclpm' and 'clpm'.", call. = FALSE) }

    # Degrees of freedom
    if (isTRUE(lavaan::fitmeasures(riclpm)["df"] == lavaan::fitmeasures(clpm)["df"])) { stop("Degrees of freedom for the model specified in 'riclpm' is equal the model specified in 'clpm'", call. = FALSE) }
    if (isTRUE(lavaan::fitmeasures(riclpm)["df"]  > lavaan::fitmeasures(clpm)["df"])) { stop("Degrees of freedom for the model specified in 'riclpm' is larger the model specified in 'clpm'", call. = FALSE) }

    # CLPM
    if (isTRUE(all(lavaan::parameterestimates(clpm)[, "op"] != "=~"))) { stop("Please specify a CLPM for the argument 'clpm' by fixing the variance of random intercepts and their covariances to zero.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Extract Information from Fitted Models ####

  #--------------------------------------
  ### Random Intercepts ####

  ri <- lavaan::parameterestimates(clpm) |> (\(p) p[p$op == "~~", ])() |>
    (\(q) q[which(is.na(q[, "z"]) != is.na(lavaan::parameterestimates(riclpm) |> (\(r) r[r$op == "~~", "z"])())), ])() |>
    (\(s) s[s$lhs == s$rhs, "lhs"])()

  if (isTRUE(length(ri) == 0L)) { stop("Random intercept variance fixed to zero was not found in the model specified in 'clpm'.", call. = FALSE) }

  #--------------------------------------
  ### Number of Random Intercepts ####

  q <- length(ri)

  #--------------------------------------
  ### Covariance Matrix of Random Intercepts ####

  S <- lavaan::vcov(riclpm) |> (\(p) which(rownames(p) %in% sapply(ri, function(y) paste0(y, "~~", y))) |> (\(q) p[q, q])())()

  #--------------------------------------
  ### Chi-Square ####

  chi2.clpm <- lavaan::fitmeasures(clpm)["chisq"]
  chi2.riclpm <- lavaan::fitmeasures(riclpm)["chisq"]

  chi2.clpm.scaled <- lavaan::fitmeasures(clpm)["chisq.scaled"] |> (\(p) if (is.na(p)) { NULL } else { p })()
  chi2.riclpm.scaled <- lavaan::fitmeasures(riclpm)["chisq.scaled"] |> (\(p) if (is.na(p)) { NULL } else { p })()

  #--------------------------------------
  ### Degrees of Freedom ####

  df.clpm <- lavaan::fitmeasures(clpm)["df"]
  df.riclpm <- lavaan::fitmeasures(riclpm)["df"]

  #--------------------------------------
  ### Number of Constrained Variances ####

  if (isTRUE(length(S) != 1L)) {

    k <- dim(S)[1L]

  } else {

    k <- 1L
    S <- as.matrix(S)

  }

  #--------------------------------------
  ### Number of Unconstrained Variances and Unconstrained Covariances ####

  u <- k*(q - k) + k*(k - 1L) / 2L

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Weights, Critical Value, Chi-Square Difference, df Difference, and Significance Value ####

  #--------------------------------------
  ### Weights ####

  weights <- rev(ic.infer::ic.weights(S))

  #--------------------------------------
  ### Critical Value ####

  sol <- nleqslv::nleqslv(5L, .find.c2(weights, k, u, alpha), control = list(btol = 0.001, allowSingular = TRUE))

  if (isTRUE(sol$fvec <= 0.01)) {

    #--------------------------------------
    ### Chi-Square Difference ####

    # Satorra-Bentler scaled chi-square value
    if (isTRUE(!is.null(chi2.riclpm.scaled) & !is.null(chi2.clpm.scaled))) {

      # Scaling correction factor
      scf.clpm <- chi2.clpm / chi2.clpm.scaled
      scf.riclpm <- chi2.riclpm / chi2.riclpm.scaled

      # Chi-square difference
      chi2.diff <- (chi2.clpm.scaled * scf.clpm - chi2.riclpm.scaled * scf.riclpm) / ((df.clpm * scf.clpm - df.riclpm * scf.riclpm) / (df.clpm - df.riclpm))

    # Chi-square value
    } else {

      # Chi-square difference
      chi2.diff <- chi2.clpm - chi2.riclpm

    }

    #--------------------------------------
    ### Difference in Degrees of Freedom ####

    df.diff <- df.clpm - df.riclpm

    #--------------------------------------
    ### Significance Value ####

    p <- 0L
    for (i in seq_len(k + 1L)) { p <- p + weights[i] * (1L - pchisq(chi2.diff, (u + i - 1L)))  }

  } else {

    stop("The critical value for the chi-bar-square difference test cannot be calculated for these models.", call. = FALSE)

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "difftest.chibarsq",
                 args = list(alpha = alpha, write = write, digits = digits, p.digits = p.digits, append = append, check = check, output = output),
                 model.fit = list(ri.clpm = riclpm, clpm = clpm),
                 result = list(difftest = setNames(data.frame(rbind(lavaan::fitmeasures(riclpm)[c("df", "aic", "bic", "bic2", "chisq", "", "", "", "")], c(lavaan::fitmeasures(clpm)[c("df", "aic", "bic", "bic2", "chisq")], chi2.diff, df.diff, sol$x, p)), row.names = c("RI-CLPM", "CLPM")), nm = c("df", "aic", "bic", "sabic", "chisq", "chisq.diff", "df.diff", "chisq.crit", "p")),
                               weights = weights))

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
