#' Simultaneous and Level-Specific Multilevel Model Fit Information
#'
#' This function prints simultaneous and level-specific model fit information
#' extracted from a fitted multilevel model of class \code{"lavaan"} from the
#' \pkg{lavaan} package. Note that level-specific fit indices cannot be computed
#' when the fitted model contains cross-level constraints, e.g., equal factor
#' loadings across levels in line with the metric cross-level measurement invariance
#' assumption.
#'
#' @param model    a fitted multilevel model of class \code{"lavaan"} from the \pkg{lavaan}
#'                 package.
#' @param print    a character string or character vector indicating which results
#'                 to show on the console, i.e. \code{"all"} for all results,
#'                 \code{"summary"} for a summary of the specification of the
#'                 estimation method and missing data handling in lavaan and
#'                 \code{"fit"} for model fit. By default, the function only prints
#'                 simultaneous and level-specific multilevel model fit information.
#' @param digits   an integer value indicating the number of decimal places
#'                 to be used for displaying results. Note that loglikelihood,
#'                 information criteria and chi-square test statistic is
#'                 printed with \code{digits} minus 1 decimal places.
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
#' @param output   logical: if \code{TRUE} (default), output is shown.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{multilevel.cfa}}, \code{\link{multilevel.invar}},
#' \code{\link{multilevel.omega}}, \code{\link{multilevel.cor}},
#' \code{\link{multilevel.descript}}, \code{\link{write.result}}
#'
#' @references
#' Rosseel, Y. (2012). lavaan: An R Package for Structural Equation Modeling.
#' \emph{Journal of Statistical Software, 48}, 1-36. https://doi.org/10.18637/jss.v048.i02
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{model.fit}}{a fitted model of class \code{"lavaan"}}
#' \item{\code{result}}{list with result tables, i.e., \code{summary} for the
#'                      summary of the specification of the estimation method
#'                      and missing data handling in lavaan and \code{fit} for
#'                      the model fit information.}
#'
#' @note
#' The function uses the functions \code{fitmeasures}, \code{lavInspect},
#' \code{lavTech}, and \code{parTable} provided in the R package \pkg{lavaan} by
#' Yves Rosseel (2012).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load data set "Demo.twolevel" in the lavaan package
#' data("Demo.twolevel", package = "lavaan")
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Model Specification and Model Estimation
#'
#' # Load lavaan package
#' library(lavaan)
#'
#' # Model specification
#' model <- 'level: 1
#'               fw =~ y1 + y2 + y3
#'               fw ~ x1 + x2 + x3
#'            level: 2
#'               fb =~ y1 + y2 + y3
#'               fb ~ w1 + w2'
#'
#' # Model estimation with estimator = "ML"
#' fit1 <- sem(model = model, data = Demo.twolevel, cluster = "cluster", estimator = "ML")
#'
#' # Model estimation with estimator = "MLR"
#' fit2 <- sem(model = model, data = Demo.twolevel, cluster = "cluster", estimator = "MLR")
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Simultaneous and Level-Specific Multilevel Model Fit Information
#'
#' # Example 1a: Standard fit indices
#' multilevel.fit(fit1)
#'
#' # Example 1b: Standard, scaled, and robust fit indices
#' multilevel.fit(fit2)
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Write Results
#'
#' # Example 2a: Write results into a text file
#' multilevel.fit(fit1, write = "LS-Fit1.txt")
#'
#' # Example 2b: Write results into an Excel file
#' multilevel.fit(fit2, write = "LS-Fit2.xlsx")
#' }
multilevel.fit <- function(model, print = c("all", "summary", "fit"), digits = 3,
                           p.digits = 3, write = NULL, append = TRUE, check = TRUE,
                           output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'model' is missing
  if (isTRUE(missing(model) || is.null(model) || !inherits(model, "lavaan"))) { stop("Please specify a fitted model of class 'lavaan' for the argument 'model'.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  .check.input(logical = c("append", "output"), m.character = list( print = c("all", "summary", "fit")),
               args = c("digits", "p.digits", "write2"), package = "lavaan", envir = environment(), input.check = check)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Additional Checks

  if (isTRUE(check)) {

    #—————————————————————————————————————— #
    ### Check lavaan Version ####

    if (isTRUE(substr(packageDescription("lavaan")$Version, 3L, 3L) %in% seq_len(6L))) { stop("This function requires at least lavaan version 0.7-2 (published 2026-07-16), please update the package.", call. = FALSE) }

    #—————————————————————————————————————— #
    ### Check if Model is a Multilevel Model ####

    if (isTRUE(!lavaan::lavInspect(model, what = "options")$.multilevel)) { stop("Please specify a fitted multilevel model of class \"lavaan\" in the argument 'model'.", call. = FALSE) }

    #—————————————————————————————————————— #
    ### Check if Model Converged ####

    if (isTRUE(!lavaan::lavInspect(model, what = "converged"))) { stop("Model specified in the argument 'model' did not converge.", call. = FALSE) }

    #—————————————————————————————————————— #
    ### Check if Degrees of Freedoms is 0 ####

    if (isTRUE(suppressWarnings(lavaan::lavInspect(model, what = "fit"))["df"] == 0L)) { stop("The model specified in the argument 'model' is saturated with zero degrees of freedom.", call. = FALSE) }

    #—————————————————————————————————————— #
    ### Check if Model Includes Cross-Level Constraints ####

    # Parameter table
    mod.par <- lavaan::parTable(model)

    # Cross-level equality constraints
    if (isTRUE(any(mod.par$op == "=="))) {

      # L1 Parameters
      l1.par <- mod.par[mod.par$level == 1L, "plabel"]
      # L2 Parameters
      l2.par <- mod.par[mod.par$level == 2L, "plabel"]

      # Cross-level constraints
      cl.const <- apply(mod.par[mod.par$op == "==", ], 1L, function(y) (y["lhs"] %in% l1.par && y["rhs"] %in% l2.par) | (y["lhs"] %in% l2.par && y["rhs"] %in% l1.par))

      if (isTRUE(any(cl.const))) { stop("The model contains cross-level equality constraints, i.e., level-specific fit indices cannot be computed.", call. = FALSE) }

      # Cross-level inequality constraints
    } else if (isTRUE(any(mod.par$op %in% c(">", "<", ">=", "<=")))) {

      # Cross-level constraints
      mod.par.con <- mod.par[mod.par$op %in% c(">", "<", ">=", "<="), ]

      cl.const <- apply(mod.par.con, 1L, function(y) (mod.par[mod.par$label == as.character(y["lhs"]), "plabel"] %in% l1.par && mod.par[mod.par$label == as.character(y["rhs"]), "plabel"] %in% l2.par) ||
                          (mod.par[mod.par$label == as.character(y["rhs"]), "plabel"] %in% l1.par && mod.par[mod.par$label == as.character(y["lhs"]), "plabel"] %in% l2.par))

      if (isTRUE(any(cl.const))) { stop("The model contains cross-level inequality constraints, i.e., level-specific fit indices cannot be computed.", call. = FALSE) }

    }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  if (isTRUE(all(c("all", "summary", "fit") %in% print))) {

    print  <- "fit"

  } else if (isTRUE(all(print == "all"))) {

    print  <- c("summary", "fit")

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Extract Simultaneous and Level-Specific Fit Indices ####

  model.fit.measures <- tryCatch(suppressWarnings(list(simul = lavaan::fitmeasures(model), fit.l1 = lavaan::fitMeasures(model, level = 1L), fit.l2 = lavaan::fitMeasures(model, level = 2L))),
                                                  error = function(y) {

                                                    stop("Partially saturated models could not be estimated, level-specific fit indices are not available.", call. = FALSE)

                                                  })

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## lavaan summary ####

  # Level 1 model parameters
  npar.l1 <- sum(mod.par$level == 1L & mod.par$free != 0L)

  # Level 2 model parameters
  npar.l2 <- sum(mod.par$level == 2L & mod.par$free != 0L)

  # Number of model parameters
  npar <- npar.l1 + npar.l2

  # Number of equality constraints
  npar.eq <- sum(table(misty::chr.omit(mod.par$label)) - 1L)

  #—————————————————————————————————————— #
  ### Number of Model Parameters ####

  # Level 1 model parameters
  npar.l1 <- sum(mod.par$level == 1L & mod.par$free != 0L)

  # Level 2 model parameters
  npar.l2 <- sum(mod.par$level == 2L & mod.par$free != 0L)

  # Number of model parameters
  npar <- npar.l1 + npar.l2

  # Number of equality constraints
  npar.eq <- sum(table(misty::chr.omit(mod.par$label)) - 1L)

  #—————————————————————————————————————— #
  ### Test Statistic and Standard Error ####

  # Test statistic
  test <- rev(lavaan::inspect(model, what = "options")$test)[1L]

  # Standard error
  se <- lavaan::inspect(model, what = "options")$se

  #—————————————————————————————————————— #
  ### Summary Table ####

  lavaan.summary <- NULL
  if (isTRUE("summary" %in% print)) {

    lavaan.summary <- data.frame(# First column
                                 c(paste("lavaan", lavaan::lavInspect(model, what = "version")), "", "Estimator", "Optimization Method", "",
                                   "Test Statistic", "Standard Errors", "Missing Data", "",
                                   "Numer of Model Parameters", "Within", "Between",
                                   "Numer of Equality Constraints", "", "",
                                   "Number of Observations", "Number of Clusters", "Average Cluster Size"),
                                 # Second column
                                 c("", "",
                                   # Estimator
                                   lavaan::lavTech(model, what = "options")$estimator,
                                   # Optimization method
                                   toupper(lavaan::lavTech(model, what = "options")$optim.method), "",
                                   # Test statistic
                                   switch(test,
                                          "none" = "None",
                                          "standard" = "Conventional",
                                          "satorra.bentler" = "Satorra-Bentler",
                                          "scaled.shifted" = "Scale-Shifted",
                                          "yuan.bentler" = "Yuan-Bentler",
                                          "yuan.bentler.mplus" = "Asymptotic Yuan-Bentler"),
                                   # Standard errors
                                   switch(se,
                                          "none" = "None",
                                          "standard" = "Conventional",
                                          "robust.sem" = "Conventional Robust",
                                          "robust.huber.white" = "Huber-White",
                                          "bootstrap" = "Bootstrap"),
                                   # Missing data
                                   ifelse(lavaan::lavInspect(model, what = "nobs") != lavaan::lavInspect(model, what = "norig"), "Listwise",
                                          ifelse(lavaan::lavInspect(model, what = "nobs") == lavaan::lavInspect(model, what = "norig") && any(is.na(lavaan::lavInspect(model, what = "data"))), "FIML", "None")), "",
                                   # Number of model parameters
                                   npar, npar.l1, npar.l2,
                                   # Number of equality constraints
                                   npar.eq, "", "Used",
                                   # Number of observations
                                   lavaan::lavInspect(model, what = "nobs"),
                                   # Number of clusters
                                   lavaan::lavInspect(model, what = "nclusters"),
                                   # Average cluster size
                                   lavaan::lavInspect(model, what = "ncluster.size")),
                                  # Third column
                                  c(rep("", times = 14L), "Total", lavaan::lavInspect(model, what = "norig"), "", ""), fix.empty.names = FALSE)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model Fit ####

  if (isTRUE("fit" %in% print)) {

    model.fit <- data.frame(# Fist column
                            c("Loglikelihood",
                              "H0 Value, Specified Model", "Scaling Correction Factor", "H1 Value, Unrestricted Model", "Scaling Correction Factor", "",
                              "Information Criteria",
                              "Akaike (AIC)", "Bayesian (BIC)", "Sample-Size Adjusted BIC", "",
                              "Chi-Square Test of Model Fit",
                              "Test statistic", "Within", "Between",
                              "Degrees of Freedom", "Within", "Between",
                              "P-value", "Within", "Between",
                              "Scaling Correction Factor", "Within", "Between", "",
                              "Incremental Fit Indices",
                              "CFI", "Within", "Between", "",
                              "TLI", "Within", "Between", "",
                              "Absolute Fit Indices",
                              "RMSEA", "Within", "Between", "",
                              "90 Percent CI - Lower", "Within", "Between",
                              "90 Percent CI - Upper", "Within", "Between",
                              "P-value RMSEA <= 0.05", "Within", "Between", "",
                              "SRMR", "Within", "Between", "",
                              "Coefficient of Determination", "GFI", "Within", "Between", "", "90 Percent CI - Lower", "Within", "Between", "90 Percent CI - Upper", "Within", "Between"),
                            # Second column
                            standard = c(# Loglikelihood
                                         NA, model.fit.measures$simul[c("logl", "scaling.factor.h0", "unrestricted.logl", "scaling.factor.h1")], NA, NA,
                                         # Information Criteria
                                         model.fit.measures$simul[c("aic", "bic", "bic2")], NA, NA,
                                         # Test statistic
                                         model.fit.measures$simul["chisq"], model.fit.measures$fit.l1["chisq"], model.fit.measures$fit.l2["chisq"],
                                         # Degrees of freedom
                                         model.fit.measures$simul["df"], model.fit.measures$fit.l1["df"],  model.fit.measures$fit.l2["df"],
                                         # P-value
                                         model.fit.measures$simul["pvalue"], model.fit.measures$fit.l1["pvalue"],  model.fit.measures$fit.l2["pvalue"],
                                         # Scaling correction factor
                                         model.fit.measures$simul["chisq.scaling.factor"], model.fit.measures$fit.l1["chisq.scaling.factor"],  model.fit.measures$fit.l2["chisq.scaling.factor"], NA, NA,
                                         # CFI
                                         model.fit.measures$simul["cfi"], model.fit.measures$fit.l1["cfi"], model.fit.measures$fit.l2["cfi"], NA,
                                         # TLI
                                         model.fit.measures$simul["tli"], model.fit.measures$fit.l1["tli"], model.fit.measures$fit.l2["tli"], NA, NA,
                                         # RMSEA
                                         model.fit.measures$simul["rmsea"], model.fit.measures$fit.l1["rmsea"], model.fit.measures$fit.l2["rmsea"], NA,
                                         model.fit.measures$simul["rmsea.ci.lower"], model.fit.measures$fit.l1["rmsea.ci.lower"], model.fit.measures$fit.l2["rmsea.ci.lower"],
                                         model.fit.measures$simul["rmsea.ci.upper"], model.fit.measures$fit.l1["rmsea.ci.upper"], model.fit.measures$fit.l2["rmsea.ci.upper"],
                                         model.fit.measures$simul["rmsea.pvalue"], model.fit.measures$fit.l1["rmsea.pvalue"], model.fit.measures$fit.l2["rmsea.pvalue"], NA,
                                         # SRMR
                                         model.fit.measures$simul["srmr"], model.fit.measures$simul["srmr_within"], model.fit.measures$simul["srmr_between"], NA, NA,
                                         # GFI
                                         model.fit.measures$simul["gfi"], model.fit.measures$fit.l1["gfi"], model.fit.measures$fit.l2["gfi"], NA,
                                         model.fit.measures$simul["gfi.ci.lower"], model.fit.measures$fit.l1["gfi.ci.lower"], model.fit.measures$fit.l2["gfi.ci.lower"],
                                         model.fit.measures$simul["gfi.ci.upper"], model.fit.measures$fit.l1["gfi.ci.upper"], model.fit.measures$fit.l2["gfi.ci.upper"]),
                            # Third column
                            scaled = c(# Loglikelihood and Information Criteria
                                       rep(NA, times = 12L),
                                       # Test statistic
                                       model.fit.measures$simul["chisq.scaled"], model.fit.measures$fit.l1["chisq.scaled"], model.fit.measures$fit.l2["chisq.scaled"],
                                       # Degrees of freedom
                                       model.fit.measures$simul["df.scaled"], model.fit.measures$fit.l1["df.scaled"], model.fit.measures$fit.l2["df.scaled"],
                                       # P-value
                                       model.fit.measures$simul["pvalue.scaled"], model.fit.measures$fit.l1["pvalue.scaled"], model.fit.measures$fit.l2["pvalue.scaled"],
                                        # Scaling correction factor
                                       model.fit.measures$simul["chisq.scaling.factor"], model.fit.measures$fit.l1["chisq.scaling.factor"], model.fit.measures$fit.l2["chisq.scaling.factor"], NA, NA,
                                       # CFI
                                       model.fit.measures$simul["cfi.scaled"], model.fit.measures$fit.l1["cfi.scaled"], model.fit.measures$fit.l2["cfi.scaled"], NA,
                                       # TLI
                                       model.fit.measures$simul["tli.scaled"], model.fit.measures$fit.l1["tli.scaled"], model.fit.measures$fit.l2["tli.scaled"], NA, NA,
                                       # RMSEA
                                       model.fit.measures$simul["rmsea.scaled"], model.fit.measures$fit.l1["rmsea.scaled"], model.fit.measures$fit.l2["rmsea.scaled"], NA,
                                       model.fit.measures$simul["rmsea.ci.lower.scaled"], model.fit.measures$fit.l1["rmsea.ci.lower.scaled"], model.fit.measures$fit.l2["rmsea.ci.lower.scaled"],
                                       model.fit.measures$simul["rmsea.ci.upper.scaled"], model.fit.measures$fit.l1["rmsea.ci.upper.scaled"], model.fit.measures$fit.l2["rmsea.ci.upper.scaled"],
                                       model.fit.measures$simul["rmsea.pvalue.scaled"], model.fit.measures$fit.l1["rmsea.pvalue.scaled"], model.fit.measures$fit.l2["rmsea.pvalue.scaled"], NA,
                                       # SRMR
                                       rep(NA, times = 3L), NA, NA,
                                       # GFI
                                       rep(NA, times = 10)),
                            # Fourth column
                            robust = c(# Loglikelihood and Information Criteria
                                       rep(NA, times = 12L),
                                       # Test statistic
                                       rep(NA, times = 3L),
                                       # Degrees of freedom
                                       rep(NA, times = 3L),
                                       # P-value
                                       rep(NA, times = 3L),
                                       # Scaling correction factor
                                       rep(NA, times = 3L), NA, NA,
                                       # CFI
                                       model.fit.measures$simul["cfi.robust"], model.fit.measures$fit.l1["cfi.robust"], model.fit.measures$fit.l2["cfi.robust"], NA,
                                       # TLI
                                       model.fit.measures$simul["tli.robust"], model.fit.measures$fit.l1["tli.robust"], model.fit.measures$fit.l2["tli.robust"], NA, NA,
                                       # RMSEA
                                       model.fit.measures$simul["rmsea.robust"], model.fit.measures$fit.l1["rmsea.robust"], model.fit.measures$fit.l2["rmsea.robust"], NA,
                                       model.fit.measures$simul["rmsea.ci.lower.robust"], model.fit.measures$fit.l1["rmsea.ci.lower.robust"], model.fit.measures$fit.l2["rmsea.ci.lower.robust"],
                                       model.fit.measures$simul["rmsea.ci.upper.robust"], model.fit.measures$fit.l1["rmsea.ci.upper.robust"], model.fit.measures$fit.l2["rmsea.ci.upper.robust"],
                                       model.fit.measures$simul["rmsea.pvalue.robust"], model.fit.measures$fit.l1["rmsea.pvalue.robust"], model.fit.measures$fit.l2["rmsea.pvalue.robust"], NA,
                                       # SRMR
                                       rep(NA, times = 3L), NA, NA,
                                       # GFI
                                       model.fit.measures$simul["gfi.robust"], model.fit.measures$fit.l1["gfi.robust"], model.fit.measures$fit.l2["gfi.robust"], NA,
                                       model.fit.measures$simul["gfi.ci.lower.robust"], model.fit.measures$fit.l1["gfi.ci.lower.robust"], model.fit.measures$fit.l2["gfi.ci.lower.robust"],
                                       model.fit.measures$simul["gfi.ci.upper.robust"], model.fit.measures$fit.l1["gfi.ci.upper.robust"], model.fit.measures$fit.l2["gfi.ci.upper.robust"]), fix.empty.names = FALSE)

    # Empty third and fourth column
    if (isTRUE(all(lavaan::lavInspect(model, what = "options")$test == "standard"))) {

      model.fit <- model.fit[-c(3L, 5L, 22:24L), c(1L, 2L)]

      rownames(model.fit) <- seq_len(nrow(model.fit))

    }

  }

  #_____________________________________________________________________________
  #
  # Return object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "multilevel.fit",
                 args = list(print = print, digits = digits, p.digits = p.digits, write = write, append = append, check = check, output = output),
                 model.fit = model,
                 result = list(summary = lavaan.summary, fit = model.fit))

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
