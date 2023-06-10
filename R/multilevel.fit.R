#' Simultaneous and Level-Specific Multilevel Model Fit Information
#'
#' This function provides simultaneous and level-specific model fit information
#' using the partially saturated model method for multilevel models estimated
#' with the \pkg{lavaan} package. Note that level-specific fit indices cannot
#' be computed when the fitted model contains cross-level constraints, e.g.,
#' equal factor loadings across levels in line with the metric cross-level
#' measurement invariance assumption.
#'
#' @param x        a fitted model of class \code{"lavaan"} from the \pkg{lavaan}
#'                 package.
#' @param print    a character string or character vector indicating which results
#'                 to show on the console, i.e. \code{"all"} for all results,
#'                 \code{"summary"} for a summary of the specification of the
#'                 estimation method and missing data handling in lavaan and
#'                 \code{"fit"} for model fit.
#' @param digits   an integer value indicating the number of decimal places
#'                 to be used for displaying results. Note that loglikelihood,
#'                 information criteria and chi-square test statistic is
#'                 printed with \code{digits} minus 1 decimal places.
#' @param p.digits an integer value indicating the number of decimal places to be
#'                 used for displaying the \emph{p}-value.
#' @param write    a character string for writing the results into a Excel file
#'                 naming a file with or without file extension '.xlsx', e.g.,
#'                 \code{"Results.xlsx"} or \code{"Results"}.
#' @param check    logical: if \code{TRUE}, argument specification is checked.
#' @param output   logical: if \code{TRUE}, output is shown.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{multilevel.cfa}}, \code{\link{multilevel.invar}},
#' \code{\link{multilevel.omega}}, \code{\link{multilevel.cor}},
#' \code{\link{multilevel.descript}}
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
#' \item{\code{x}}{a fitted model of class \code{"lavaan"}}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{model}}{specified models, i.e., \code{mod.l1} for the model at the
#'                     Within level, \code{mod.l1.syntax} for the lavaan syntax
#'                     for the model at the Between level, \code{mod.l2} for the
#'                     model at the Within level, \code{mod.l2.syntax} for the
#'                     lavaan syntax for the model at the Between level,
#'                     \code{mod.l12} for the model at the Within and Between
#'                     level, \code{mod.l12.syntax} for the lavaan syntax for
#'                     the model at the Within and Between level, \code{l1.mod.base}
#'                     for the baseline model at the Within level saturated at
#'                     the Between level, \code{l1.mod.hypo} for the hypothesized
#'                     model at the Within level saturated at the Between level,
#'                     \code{l2.mod.base} for the baseline model at the Between
#'                     level saturated at the Within level, \code{l2.mod.hypo}
#'                     for the hypothesized model at the Between level saturated
#'                     at the Within level}
#' \item{\code{result}}{list with result tables, i.e., \code{summary} for the
#'                      summary of the specification of the estimation method
#'                      and missing data handling in lavaan and \code{fit} for
#'                      the model fit information.}
#'
#' @note
#' The function uses the functions \code{cfa}, \code{fitmeasures}, \code{lavInspect},
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
#' # Model specification
#' model <- 'level: 1
#'               fw =~ y1 + y2 + y3
#'               fw ~ x1 + x2 + x3
#'            level: 2
#'               fb =~ y1 + y2 + y3
#'               fb ~ w1 + w2'
#'
#' #---------------------------
#' # Model estimation with estimator = "ML"
#' fit1 <- lavaan::sem(model = model, data = Demo.twolevel, cluster = "cluster",
#'                     estimator = "ML")
#'
#' # Simultaneous and kevel-specific multilevel model fit information
#' ls.fit1 <- multilevel.fit(fit1)
#'
#' # Write results into an Excel file
#' write.result(ls.fit1, "LS-Fit1.xlsx")
#'
#' #---------------------------
#' # Model estimation with estimator = "MLR"
#' fit2 <- lavaan::sem(model = model, data = Demo.twolevel, cluster = "cluster",
#'                     estimator = "MLR")
#'
#' # Simultaneous and kevel-specific multilevel model fit information
#' # Write results into an Excel file
#' multilevel.fit(fit2, write = "LS-Fit2.xlsx")
#' }
multilevel.fit <- function(x, print = c("all", "summary", "fit"), digits = 3, p.digits = 3,
                           write = NULL, check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'x' is missing
  if (isTRUE(missing(x))) { stop("Please specify a matrix or data frame for the argument 'x'.", call. = FALSE) }

  # Check if input 'x' is NULL
  if (isTRUE(is.null(x))) { stop("Input specified for the argument 'x' is NULL.", call. = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Check if model is a lavaan object ####

  if (isTRUE(class(x) != "lavaan")) { stop("Please specify a fitted multilevel model of class \"lavaan\" in the argument 'x'.", call. = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Check if model is a multilevel model ####

  if (isTRUE(!lavaan::lavInspect(x, what = "options")$.multilevel)) { stop("Please specify a fitted multilevel model of class \"lavaan\" in the argument 'x'.", call. = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Check if model converged ####

  if (isTRUE(!lavaan::lavInspect(x, what = "converged"))) { stop("Model specified in the argument 'x' did not converge.", call. = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Check if degrees of freedoms is 0 ####

  if (isTRUE(suppressWarnings(lavaan::lavInspect(x, what = "fit"))["df"] == 0L)) { stop("The model specified in the argument 'x' is saturated with zero degrees of freedom.", call. = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Check if model includes cross-level constraints ####

  # Parameter table
  mod.par <- lavaan::parTable(x)

  # Cross-level equality constraints
  if (isTRUE(any(mod.par$op == "=="))) {

    # L1 Parameters
    l1.par <- mod.par[mod.par$level == 1L, "plabel"]
    # L2 Parameters
    l2.par <- mod.par[mod.par$level == 2L, "plabel"]

    # Cross-level constraints
    cl.const <- apply(mod.par[mod.par$op == "==", ], 1L, function(y) (y["lhs"] %in% l1.par && y["rhs"] %in% l2.par) | (y["lhs"] %in% l2.par && y["rhs"] %in% l1.par))

    if (any(cl.const)) { stop("The model contains cross-level equality constraints, i.e., level-specific fit indices cannot be computed.", call. = FALSE) }

  # Cross-level inequality constraints
  } else if (isTRUE(any(mod.par$op %in% c(">", "<", ">=", "<=")))) {

    # Cross-level constraints
    mod.par.con <- mod.par[mod.par$op %in% c(">", "<", ">=", "<="), ]

    cl.const <- apply(mod.par.con, 1L, function(y) (mod.par[mod.par$label == as.character(y["lhs"]), "plabel"] %in% l1.par && mod.par[mod.par$label == as.character(y["rhs"]), "plabel"] %in% l2.par) ||
                                                   (mod.par[mod.par$label == as.character(y["rhs"]), "plabel"] %in% l1.par && mod.par[mod.par$label == as.character(y["lhs"]), "plabel"] %in% l2.par))

    if (any(cl.const)) { stop("The model contains cross-level inequality constraints, i.e., level-specific fit indices cannot be computed.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # R package lavaan
    if (isTRUE(!nzchar(system.file(package = "lavaan")))) { stop("Package \"lavaan\" is needed for this function, please install the package.", call. = FALSE) }

    # Check input 'print'
    if (isTRUE(!all(print %in% c("all", "summary", "fit")))) { stop("Character strings in the argument 'print' do not all match with \"summary\", or \"fit\".", call. = FALSE) }

    # Check input 'digits'
    if (isTRUE(digits %% 1L != 0L || digits < 0L)) { stop("Specify a positive integer value for the argument 'digits'.", call. = FALSE) }

    # Check input 'p.digits'
    if (isTRUE(p.digits %% 1L != 0L || p.digits < 0L)) { stop("Specify a positive integer value for the argument 'p.digits'.", call. = FALSE) }

    # Check input 'output'
    if (isTRUE(!is.logical(output))) { stop("Please specify TRUE or FALSE for the argument 'output'", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  if (isTRUE(all(c("all", "summary", "fit") %in% print))) { print  <- c("summary", "fit") }

  if (isTRUE(length(print) == 1L && "all" %in% print)) { print  <- c("summary", "fit") }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Level 1 Model Specification ####

  # Level 1 parameter table
  l1.mod.par <- mod.par[mod.par$level == 1L & mod.par$user == 1L, ]

  #...................
  ### Factor loadings, residuals ####

  l1.mod.par.b1 <- l1.mod.par[l1.mod.par$op != "~1", ]

  # Without 'ustart' and 'label'
  mod.l1.b1 <- l1.mod.par.b1[is.na(l1.mod.par.b1$ustart) & l1.mod.par.b1$label == "", c("lhs", "op", "rhs")]

  # With 'ustart'
  l1.mod.par.b1.ustart <- l1.mod.par.b1[!is.na(l1.mod.par.b1$ustart), ]

  if (nrow(l1.mod.par.b1.ustart) > 0L) { l1.mod.par.b1.ustart$rhs <- paste(l1.mod.par.b1.ustart$ustart, l1.mod.par.b1.ustart$rhs, sep = "*") }

  mod.l1.b1 <- rbind(mod.l1.b1, l1.mod.par.b1.ustart[, c("lhs", "op", "rhs")])

  # With 'label'
  l1.mod.par.b1.label <- l1.mod.par.b1[l1.mod.par.b1$label != "", ]

  if (nrow(l1.mod.par.b1.label) > 0L) { l1.mod.par.b1.label$rhs <- paste(l1.mod.par.b1.label$label, l1.mod.par.b1.label$rhs, sep = "*") }

  mod.l1.b1 <- rbind(mod.l1.b1, l1.mod.par.b1.label[which(!l1.mod.par.b1.label$plabel %in% l1.mod.par.b1.ustart), c("lhs", "op", "rhs")])

  #...................
  ### Intercepts and latent means ####

  l1.mod.par.b2 <- l1.mod.par[l1.mod.par$op == "~1", ]

  # Without 'ustart' and 'label'
  mod.l1.b2 <- l1.mod.par.b2[is.na(l1.mod.par.b2$ustart) & l1.mod.par.b2$label == "", c("lhs", "op", "rhs")]

  # With 'ustart'
  l1.mod.par.b2.ustart <- l1.mod.par.b2[!is.na(l1.mod.par.b2$ustart) & l1.mod.par.b2$label == "", ]

  if (nrow(l1.mod.par.b2.ustart) > 0L) { l1.mod.par.b2.ustart$op <- paste(paste0("~", l1.mod.par.b2.ustart$ustart), "1", sep = "*") }

  mod.l1.b2 <- rbind(mod.l1.b2, l1.mod.par.b2.ustart[, c("lhs", "op", "rhs")])

  # With 'label'
  l1.mod.par.b2.label <- l1.mod.par.b2[l1.mod.par.b2$label != "", ]

  if (nrow(l1.mod.par.b2.label) > 0L) { l1.mod.par.b2.label$op <- paste(paste0("~", l1.mod.par.b2.label$label), "1", sep = "*") }

  mod.l1.b2 <- rbind(mod.l1.b2, l1.mod.par.b2.label[, c("lhs", "op", "rhs")])

  # Inequality constraints
  mod.l1.ineq <- mod.par[mod.par$user == 1L & mod.par$level == 0L, c("lhs", "op", "rhs")]

  if (isTRUE(mod.par[mod.par$label == mod.l1.ineq$lhs, "level"] == 1L)) {

    # Level 1 model
    mod.l1 <- rbind(mod.l1.b1, mod.l1.b2, mod.l1.ineq)

  } else {

    # Level 1 model
    mod.l1 <- rbind(mod.l1.b1, mod.l1.b2)

  }

  # Order
  mod.l1 <- mod.l1[order(as.numeric(row.names(mod.l1))), ]

  # Level 1 model syntax
  mod.l1.syntax <- paste(apply(mod.l1, 1L, paste, collapse = " "), collapse = " \n   ")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Level 2 Model Specification ####

  # Level 2 parameter table
  l2.mod.par <- mod.par[mod.par$level == 2L & mod.par$user == 1L, ]

  #...................
  ### Factor loadings, residuals ####

  l2.mod.par.b1 <- l2.mod.par[l2.mod.par$op != "~1", ]

  # Without 'ustart' and 'label'
  mod.l2.b1 <- l2.mod.par.b1[is.na(l2.mod.par.b1$ustart) & l2.mod.par.b1$label == "", c("lhs", "op", "rhs")]

  # With 'ustart'
  l2.mod.par.b1.ustart <- l2.mod.par.b1[!is.na(l2.mod.par.b1$ustart), ]

  if (nrow(l2.mod.par.b1.ustart) > 0L) { l2.mod.par.b1.ustart$rhs <- paste(l2.mod.par.b1.ustart$ustart, l2.mod.par.b1.ustart$rhs, sep = "*") }

  mod.l2.b1 <- rbind(mod.l2.b1, l2.mod.par.b1.ustart[, c("lhs", "op", "rhs")])

  # With 'label'
  l2.mod.par.b1.label <- l2.mod.par.b1[l2.mod.par.b1$label != "", ]

  if (nrow(l2.mod.par.b1.label) > 0L) { l2.mod.par.b1.label$rhs <- paste(l2.mod.par.b1.label$label, l2.mod.par.b1.label$rhs, sep = "*") }

  mod.l2.b1 <- rbind(mod.l2.b1, l2.mod.par.b1.label[, c("lhs", "op", "rhs")])

  #...................
  ### Intercepts and latent means ####

  l2.mod.par.b2 <- l2.mod.par[l2.mod.par$op == "~1", ]

  # Without 'ustart' and 'label'
  mod.l2.b2 <- l2.mod.par.b2[is.na(l2.mod.par.b2$ustart) & l2.mod.par.b2$label == "", c("lhs", "op", "rhs")]

  # With 'ustart'
  l2.mod.par.b2.ustart <- l2.mod.par.b2[!is.na(l2.mod.par.b2$ustart) & l2.mod.par.b2$label == "", ]

  if (nrow(l2.mod.par.b2.ustart) > 0L) { l2.mod.par.b2.ustart$op <- paste(paste0("~", l2.mod.par.b2.ustart$ustart), "1", sep = "*") }

  mod.l2.b2 <- rbind(mod.l2.b2, l2.mod.par.b2.ustart[, c("lhs", "op", "rhs")])

  # With 'label'
  l2.mod.par.b2.label <- l2.mod.par.b2[l2.mod.par.b2$label != "", ]

  if (nrow(l2.mod.par.b2.label) > 0L) { l2.mod.par.b2.label$op <- paste(paste0("~", l2.mod.par.b2.label$label), "1", sep = "*") }

  mod.l2.b2 <- rbind(mod.l2.b2, l2.mod.par.b2.label[, c("lhs", "op", "rhs")])

  # Inequality constraints
  mod.l2.ineq <- mod.par[mod.par$user == 1L & mod.par$level == 0L, c("lhs", "op", "rhs")]

  if (isTRUE(mod.par[mod.par$label == mod.l2.ineq$lhs, "level"] == 2L)) {

    # Level 2 model
    mod.l2 <- rbind(mod.l2.b1, mod.l2.b2, mod.l2.ineq)

  } else {

    # Level 2 model
    mod.l2 <- rbind(mod.l2.b1, mod.l2.b2)

  }

  # Order
  mod.l2 <- mod.l2[order(as.numeric(row.names(mod.l2))), ]

  # Level 2 model syntax
  mod.l2.syntax <- paste(apply(mod.l2, 1L, paste, collapse = " "), collapse = " \n   ")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Level 1 and 2 Model Specification ####

  # Level 1 and 2 model
  mod.l12 <- rbind(data.frame(mod.l1, level = 1L),
                   data.frame(mod.l2, level = 2L))

  # Level 1 and model syntax
  mod.l12.syntax <- paste("level: 1 \n",
                          mod.l1.syntax,
                          "\nlevel: 2 \n",
                          mod.l2.syntax)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Partially Saturated Model ####

  # Observed variables
  obs.var <- colnames(lavaan::lavInspect(x, what = "data"))

  # Level 1 observed variables
  l1.obs.var <- obs.var[obs.var %in% unique(unlist(mod.par[mod.par$level == 1L, c("lhs", "rhs")]))]

  # Level 2 observed variables
  l2.obs.var <- obs.var[obs.var %in% unique(unlist(mod.par[mod.par$level == 2L, c("lhs", "rhs")]))]

  # Call
  model.call <- lavaan::lavInspect(x, what = "call")

  model.call <- unlist(model.call[!names(model.call) %in% c("", "model", "data", "cluster", "estimator", "optim.method", "missing", "std.lv", "effect.coding", "test", "se")])

  for (i in which(!model.call %in% c("TRUE", "FALSE"))) { model.call[i] <- paste0("\"", model.call[i], "\"") }

  for (i in seq_along(model.call)) { model.call[i] <- paste0(names(model.call)[i], " = ", model.call[i]) }

  lav.options <- lavaan::lavInspect(x, what = "options")

  model.call <- c(estimator = paste0("estimator = ", "\"", ifelse(lav.options$test == "yuan.bentler.mplus", "MLR", "ML"), "\""),
                  optim.method = paste0("optim.method =", "\"", lav.options$optim.method, "\""),
                  missing = paste0("missing =", "\"", lav.options$missing, "\""),
                  std.lv = paste0("std.lv =", "\"", lav.options$std.lv, "\""),
                  effect.coding = paste0("effect.coding = ", ifelse(all(lav.options$effect.coding == ""), FALSE, TRUE)),
                  test = paste0("test =", "\"", ifelse(lav.options$test == "yuan.bentler.mplus", "yuan.bentler", lav.options$test) , "\""),
                  se = paste0("se =", "\"", lav.options$se, "\""), model.call)

  #...................
  ### Within Level ####

  # Baseline model
  l1.mod.base <- paste0(c(" level: 1\n  ", paste0(apply(combn(l1.obs.var, m = 2L), 2L, paste, collapse = " ~~ 0*"), collapse = " \n   "), "\n",
                          "level: 2\n  ", paste0(apply(combn(l2.obs.var, m = 2L), 2L, paste, collapse = " ~~ "), collapse = " \n   ")))

  l1.mod.base.fit <- eval(parse(text = paste0("suppressWarnings(lavaan::cfa(model = l1.mod.base, data = data.frame(lavaan::lavInspect(x, what = \"data\"), cluster = lavaan::lavInspect(x, what = \"cluster.idx\")), cluster = \"cluster\", ",
                          paste(model.call, collapse = ", "), ", check.gradient = FALSE, check.post = FALSE, check.vcov = FALSE))")))

  # Hypothesized model
  l1.mod.hypo <- paste0(c(" level: 1 \n  ", mod.l1.syntax, "\n",
                         "level: 2 \n  ", paste0(apply(combn(l2.obs.var, m = 2L), 2L, paste, collapse = " ~~ "), collapse = " \n   ")))

  l1.mod.hypo.fit <- eval(parse(text = paste0("suppressWarnings(lavaan::cfa(model = l1.mod.hypo, data = data.frame(lavaan::lavInspect(x, what = \"data\"), cluster = lavaan::lavInspect(x, what = \"cluster.idx\")), cluster = \"cluster\", ",
                                              paste(model.call, collapse = ", "), ", check.gradient = FALSE, check.post = FALSE, check.vcov = FALSE))")))

  #...................
  ### Between Level ####

  # Baseline model
  l2.mod.base <- paste0(c(" level: 1 \n  ", paste0(apply(combn(l1.obs.var, m = 2L), 2L, paste, collapse = " ~~ "), collapse = " \n   "), "\n",
                          "level: 2 \n  ", paste0(apply(combn(l2.obs.var, m = 2L), 2L, paste, collapse = " ~~ 0*"), collapse = " \n   ")))

  l2.mod.base.fit <- eval(parse(text = paste0("suppressWarnings(lavaan::cfa(model = l2.mod.base, data = data.frame(lavaan::lavInspect(x, what = \"data\"), cluster = lavaan::lavInspect(x, what = \"cluster.idx\")), cluster = \"cluster\", ",
                                              paste(model.call, collapse = ", "), ", check.gradient = FALSE, check.post = FALSE, check.vcov = FALSE))")))

  # Hypothesized model
  l2.mod.hypo <- paste0(c(" level: 1 \n  ", paste0(apply(combn(l1.obs.var, m = 2L), 2L, paste, collapse = " ~~ "), collapse = " \n   "), "\n",
                         "level: 2 \n  ", mod.l2.syntax))

  l2.mod.hypo.fit <- eval(parse(text = paste0("suppressWarnings(lavaan::cfa(model = l2.mod.hypo, data = data.frame(lavaan::lavInspect(x, what = \"data\"), cluster = lavaan::lavInspect(x, what = \"cluster.idx\")), cluster = \"cluster\", ",
                                              paste(model.call, collapse = ", "), ", check.gradient = FALSE, check.post = FALSE, check.vcov = FALSE))")))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convergence check ####

  if (isTRUE(check)) {

    #...................
    ### Model convergence ####

    if (isTRUE(!lavaan::lavInspect(l1.mod.base.fit, what = "converged"))) { stop("Within Level baseline model did not converge.", call. = FALSE) }
    if (isTRUE(!lavaan::lavInspect(l1.mod.hypo.fit, what = "converged"))) { stop("Within Level hypothesized model did not converge.", call. = FALSE) }
    if (isTRUE(!lavaan::lavInspect(l2.mod.base.fit, what = "converged"))) { stop("Between Level baseline model did not converge.", call. = FALSE) }
    if (isTRUE(!lavaan::lavInspect(l2.mod.hypo.fit, what = "converged"))) { stop("Between Level hypothesized model did not converge.", call. = FALSE) }

    #...................
    ### Standard error ####

    if (isTRUE(any(is.na(unlist(lavaan::lavInspect(l1.mod.base.fit, what = "se")))))) { stop("Standard errors in the Within Level baseline model could not be computed.", call. = FALSE) }
    if (isTRUE(any(is.na(unlist(lavaan::lavInspect(l1.mod.hypo.fit, what = "se")))))) { stop("Standard errors in the Within Level hypothesized model could not be computed.", call. = FALSE) }
    if (isTRUE(any(is.na(unlist(lavaan::lavInspect(l2.mod.base.fit, what = "se")))))) { stop("Standard errors in the Between Level baseline modelcould not be computed.", call. = FALSE) }
    if (isTRUE(any(is.na(unlist(lavaan::lavInspect(l2.mod.hypo.fit, what = "se")))))) { stop("Standard errors in the Between Level hypothesized model could not be computed.", call. = FALSE) }

    #...................
    ### Variance-covariance matrix of the estimated parameters ####

    l1.base.eigvals <- eigen(lavaan::lavInspect(l1.mod.base.fit, what = "vcov"), symmetric = TRUE, only.values = TRUE)$values
    l1.hypo.eigvals <- eigen(lavaan::lavInspect(l1.mod.hypo.fit, what = "vcov"), symmetric = TRUE, only.values = TRUE)$values
    l2.base.eigvals <- eigen(lavaan::lavInspect(l2.mod.base.fit, what = "vcov"), symmetric = TRUE, only.values = TRUE)$values
    l2.hypo.eigvals <- eigen(lavaan::lavInspect(l2.mod.hypo.fit, what = "vcov"), symmetric = TRUE, only.values = TRUE)$values

    # Model contains equality constraints
    l1.base.model.fit.par <- lavaan::parameterTable(l1.mod.base.fit)$op == "=="
    l1.hypo.model.fit.par <- lavaan::parameterTable(l1.mod.hypo.fit)$op == "=="
    l2.base.model.fit.par <- lavaan::parameterTable(l2.mod.base.fit)$op == "=="
    l2.hypo.model.fit.par <- lavaan::parameterTable(l2.mod.hypo.fit)$op == "=="

    if (isTRUE(any(l1.base.model.fit.par))) { l1.base.eigvals <- rev(l1.base.eigvals)[-seq_len(sum(l1.base.model.fit.par))] }
    if (isTRUE(any(l1.hypo.model.fit.par))) { l1.hypo.eigvals <- rev(l1.hypo.eigvals)[-seq_len(sum(l1.hypo.model.fit.par))] }
    if (isTRUE(any(l2.base.model.fit.par))) { l2.base.eigvals <- rev(l2.base.eigvals)[-seq_len(sum(l2.base.model.fit.par))] }
    if (isTRUE(any(l2.hypo.model.fit.par))) { l2.hypo.eigvals <- rev(l2.hypo.eigvals)[-seq_len(sum(l2.hypo.model.fit.par))] }

    if (isTRUE(min(l1.base.eigvals) < .Machine$double.eps^(3L/4L))) { warning("The variance-covariance matrix of the estimated parameters in the Within Level baseline model is not positive definite.", call. = FALSE) }
    if (isTRUE(min(l1.hypo.eigvals) < .Machine$double.eps^(3L/4L))) { warning("The variance-covariance matrix of the estimated parameters in the Within Level hypothesized model is not positive definite.", call. = FALSE) }
    if (isTRUE(min(l2.base.eigvals) < .Machine$double.eps^(3L/4L))) { warning("The variance-covariance matrix of the estimated parameters in the Between Level baseline model is not positive definite.", call. = FALSE) }
    if (isTRUE(min(l2.hypo.eigvals) < .Machine$double.eps^(3L/4L))) { warning("The variance-covariance matrix of the estimated parameters in the Between Level hypothesized model is not positive definite.", call. = FALSE) }

    #...................
    ### Negative variance of observed variables ####

    #### Within Level
    if (isTRUE(any(diag(lavaan::lavInspect(l1.mod.base.fit, what = "theta")$within) < 0L))) { warning("Some estimated variances of the observed variables in the Within Level baseline model at the Within level are negative.", call. = FALSE) } else if (isTRUE(any(eigen(lavaan::lavTech(l1.mod.base.fit, what = "theta")$within, symmetric = TRUE, only.values = TRUE)$values < (-1L * .Machine$double.eps^(3/4))))) { warning("The model-implied variance-covariance matrix of the residuals of the observed variables or the Within Level baseline model is not positive definite.", call. = FALSE) }
    if (isTRUE(any(diag(lavaan::lavInspect(l1.mod.hypo.fit, what = "theta")$within) < 0L))) { warning("Some estimated variances of the observed variables in the Within Level hypothesized model at the Within level are negative.", call. = FALSE) } else if (isTRUE(any(eigen(lavaan::lavTech(l1.mod.hypo.fit, what = "theta")$within, symmetric = TRUE, only.values = TRUE)$values < (-1L * .Machine$double.eps^(3/4))))) { warning("The model-implied variance-covariance matrix of the residuals of the observed variables or the Within Level hypothesized model is not positive definite.", call. = FALSE) }
    if (isTRUE(any(diag(lavaan::lavInspect(l2.mod.base.fit, what = "theta")$within) < 0L))) { warning("Some estimated variances of the observed variables in the Between Level baseline model at the Within level are negative.", call. = FALSE) } else if (isTRUE(any(eigen(lavaan::lavTech(l2.mod.base.fit, what = "theta")$within, symmetric = TRUE, only.values = TRUE)$values < (-1L * .Machine$double.eps^(3/4))))) { warning("The model-implied variance-covariance matrix of the residuals of the observed variables or the Between Level baseline model is not positive definite.", call. = FALSE) }
    if (isTRUE(any(diag(lavaan::lavInspect(l2.mod.hypo.fit, what = "theta")$within) < 0L))) { warning("Some estimated variances of the observed variables in the Between Level hypothesized model at the Within level are negative.", call. = FALSE) } else if (isTRUE(any(eigen(lavaan::lavTech(l2.mod.hypo.fit, what = "theta")$within, symmetric = TRUE, only.values = TRUE)$values < (-1L * .Machine$double.eps^(3/4))))) { warning("The model-implied variance-covariance matrix of the residuals of the observed variables or the Between Level hypothesized model is not positive definite.", call. = FALSE) }

    #### Between Level
    if (isTRUE(any(diag(lavaan::lavInspect(l1.mod.base.fit, what = "theta")[[2L]]) < 0L))) { warning("Some estimated variances of the observed variables in the Within Level baseline model at the Between level are negative.", call. = FALSE) } else if (isTRUE(any(eigen(lavaan::lavTech(l1.mod.base.fit, what = "theta")[[2L]], symmetric = TRUE, only.values = TRUE)$values < (-1L * .Machine$double.eps^(3/4))))) { warning("The model-implied variance-covariance matrix of the residuals of the observed variables Within Level baseline model at the Between level is not positive definite.", call. = FALSE) }
    if (isTRUE(any(diag(lavaan::lavInspect(l1.mod.hypo.fit, what = "theta")[[2L]]) < 0L))) { warning("Some estimated variances of the observed variables in the Within Level hypothesized model at the Between level are negative.", call. = FALSE) } else if (isTRUE(any(eigen(lavaan::lavTech(l1.mod.hypo.fit, what = "theta")[[2L]], symmetric = TRUE, only.values = TRUE)$values < (-1L * .Machine$double.eps^(3/4))))) { warning("The model-implied variance-covariance matrix of the residuals of the observed variables Within Level hypothesized model at the Between level is not positive definite.", call. = FALSE) }
    if (isTRUE(any(diag(lavaan::lavInspect(l2.mod.base.fit, what = "theta")[[2L]]) < 0L))) { warning("Some estimated variances of the observed variables in the Between Level baseline model at the Between level are negative.", call. = FALSE) } else if (isTRUE(any(eigen(lavaan::lavTech(l2.mod.base.fit, what = "theta")[[2L]], symmetric = TRUE, only.values = TRUE)$values < (-1L * .Machine$double.eps^(3/4))))) { warning("The model-implied variance-covariance matrix of the residuals of the observed variables at Between Level baseline model the Between level is not positive definite.", call. = FALSE) }
    if (isTRUE(any(diag(lavaan::lavInspect(l2.mod.hypo.fit, what = "theta")[[2L]]) < 0L))) { warning("Some estimated variances of the observed variables in the Between Level hypothesized model at the Between level are negative.", call. = FALSE) } else if (isTRUE(any(eigen(lavaan::lavTech(l2.mod.hypo.fit, what = "theta")[[2L]], symmetric = TRUE, only.values = TRUE)$values < (-1L * .Machine$double.eps^(3/4))))) { warning("The model-implied variance-covariance matrix of the residuals of the observed variables Between Level hypothesized model at the Between level is not positive definite.", call. = FALSE) }

    #...................
    ### Negative variance of latent variables ####

    #### Within Level
    if (isTRUE(!is.null(lavaan::lavTech(l1.mod.base.fit, what = "cov.lv")$within))) { if (isTRUE(any(diag(lavaan::lavTech(l1.mod.base.fit, what = "cov.lv")$within) < 0L))) { warning("Some estimated variances of the latent variables in the Within Level baseline model at the Within level are negative.", call. = FALSE) }
    # Model-implied variance-covariance matrix of the latent variables
    } else if (any(dim(lavaan::lavTech(l1.mod.base.fit, what = "cov.lv")$within) != 0L)) { if (isTRUE(any(eigen(lavaan::lavTech(l1.mod.base.fit, what = "cov.lv")$within, symmetric = TRUE, only.values = TRUE)$values < (-1L * .Machine$double.eps^(3/4))))) { warning("The model-implied variance-covariance matrix of the latent variables in the Within Level baseline model at the Within level is not positive definite.", call. = FALSE) }}

    if (isTRUE(!is.null(lavaan::lavTech(l1.mod.hypo.fit, what = "cov.lv")$within))) { if (isTRUE(any(diag(lavaan::lavTech(l1.mod.hypo.fit, what = "cov.lv")$within) < 0L))) { warning("Some estimated variances of the latent variables at the Within level are negative.", call. = FALSE) }
    # Model-implied variance-covariance matrix of the latent variables
    } else if (any(dim(lavaan::lavTech(l1.mod.hypo.fit, what = "cov.lv")$within) != 0L)) { if (isTRUE(any(eigen(lavaan::lavTech(l1.mod.hypo.fit, what = "cov.lv")$within, symmetric = TRUE, only.values = TRUE)$values < (-1L * .Machine$double.eps^(3/4))))) { warning("The model-implied variance-covariance matrix of the latent variables in the Within Level hypothesized model at the Within level is not positive definite.", call. = FALSE) }}

    if (isTRUE(!is.null(lavaan::lavTech(l2.mod.base.fit, what = "cov.lv")$within))) { if (isTRUE(any(diag(lavaan::lavTech(l2.mod.base.fit, what = "cov.lv")$within) < 0L))) { warning("Some estimated variances of the latent variables at the Within level are negative.", call. = FALSE) }
    # Model-implied variance-covariance matrix of the latent variables
    } else if (any(dim(lavaan::lavTech(l2.mod.base.fit, what = "cov.lv")$within) != 0L)) { if (isTRUE(any(eigen(lavaan::lavTech(l2.mod.base.fit, what = "cov.lv")$within, symmetric = TRUE, only.values = TRUE)$values < (-1L * .Machine$double.eps^(3/4))))) { warning("The model-implied variance-covariance matrix of the latent variables in the Between Level baseline model at the Within level is not positive definite.", call. = FALSE) }}

    if (isTRUE(!is.null(lavaan::lavTech(l2.mod.hypo.fit, what = "cov.lv")$within))) { if (isTRUE(any(diag(lavaan::lavTech(l2.mod.hypo.fit, what = "cov.lv")$within) < 0L))) { warning("Some estimated variances of the latent variables at the Within level are negative.", call. = FALSE) }
    # Model-implied variance-covariance matrix of the latent variables
    } else if (any(dim(lavaan::lavTech(l2.mod.hypo.fit, what = "cov.lv")$within) != 0L)) { if (isTRUE(any(eigen(lavaan::lavTech(l2.mod.hypo.fit, what = "cov.lv")$within, symmetric = TRUE, only.values = TRUE)$values < (-1L * .Machine$double.eps^(3/4))))) { warning("The model-implied variance-covariance matrix of the latent variables in the Between Level hypothesized model at the Within level is not positive definite.", call. = FALSE) }}

    #### Between Level
    if (isTRUE(!is.null(lavaan::lavTech(l1.mod.base.fit, what = "cov.lv")$cluster))) { if (isTRUE(any(diag(lavaan::lavTech(l1.mod.base.fit, what = "cov.lv")[[2L]]) < 0L))) { warning("Some estimated variances of the latent variables in the Within Level baseline model at the Between level are negative.", call. = FALSE) }
    # Model-implied variance-covariance matrix of the latent variables
    } else if (any(dim(lavaan::lavTech(l1.mod.base.fit, what = "cov.lv")[[2L]]) != 0L)) { if (isTRUE(any(eigen(lavaan::lavTech(l1.mod.base.fit, what = "cov.lv")[[2L]], symmetric = TRUE, only.values = TRUE)$values < (-1L * .Machine$double.eps^(3/4))))) { warning("The model-implied variance-covariance matrix of the latent variables in the Within Level baseline model at the Between level is not positive definite.", call. = FALSE) } }

    if (isTRUE(!is.null(lavaan::lavTech(l1.mod.hypo.fit, what = "cov.lv")$cluster))) { if (isTRUE(any(diag(lavaan::lavTech(l1.mod.hypo.fit, what = "cov.lv")[[2L]]) < 0L))) { warning("Some estimated variances of the latent variables in the Within Level hypothesized model at the Between level are negative.", call. = FALSE) }
      # Model-implied variance-covariance matrix of the latent variables
    } else if (any(dim(lavaan::lavTech(l1.mod.hypo.fit, what = "cov.lv")[[2L]]) != 0L)) { if (isTRUE(any(eigen(lavaan::lavTech(l1.mod.hypo.fit, what = "cov.lv")[[2L]], symmetric = TRUE, only.values = TRUE)$values < (-1L * .Machine$double.eps^(3/4))))) { warning("The model-implied variance-covariance matrix of the latent variables in the Within Level hypothesized model at the Between level is not positive definite.", call. = FALSE) } }

    if (isTRUE(!is.null(lavaan::lavTech(l2.mod.base.fit, what = "cov.lv")$cluster))) { if (isTRUE(any(diag(lavaan::lavTech(l2.mod.base.fit, what = "cov.lv")[[2L]]) < 0L))) { warning("Some estimated variances of the latent variables in the Between Level baseline model at the Between level are negative.", call. = FALSE) }
      # Model-implied variance-covariance matrix of the latent variables
    } else if (any(dim(lavaan::lavTech(l2.mod.base.fit, what = "cov.lv")[[2L]]) != 0L)) { if (isTRUE(any(eigen(lavaan::lavTech(l2.mod.base.fit, what = "cov.lv")[[2L]], symmetric = TRUE, only.values = TRUE)$values < (-1L * .Machine$double.eps^(3/4))))) { warning("The model-implied variance-covariance matrix of the latent variables at the Between level in the Between Level baseline model is not positive definite.", call. = FALSE) } }

    if (isTRUE(!is.null(lavaan::lavTech(l2.mod.hypo.fit, what = "cov.lv")$cluster))) { if (isTRUE(any(diag(lavaan::lavTech(l2.mod.hypo.fit, what = "cov.lv")[[2L]]) < 0L))) { warning("Some estimated variances of the latent variables in the Between Level hypothesized model at the Between level are negative.", call. = FALSE) }
      # Model-implied variance-covariance matrix of the latent variables
    } else if (any(dim(lavaan::lavTech(l2.mod.hypo.fit, what = "cov.lv")[[2L]]) != 0L)) { if (isTRUE(any(eigen(lavaan::lavTech(l2.mod.hypo.fit, what = "cov.lv")[[2L]], symmetric = TRUE, only.values = TRUE)$values < (-1L * .Machine$double.eps^(3/4))))) { warning("The model-implied variance-covariance matrix of the latent variables at the Between level in the Between Level hypothesized model is not positive definite.", call. = FALSE) } }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Chi-Square Values and Fit Indices ####

  mod.hypo.fit.measures <- lavaan::fitmeasures(x)

  l1.mod.base.fit.measures <- lavaan::fitmeasures(l1.mod.base.fit)
  l1.mod.hypo.fit.measures <- lavaan::fitmeasures(l1.mod.hypo.fit)

  l2.mod.base.fit.measures <- lavaan::fitmeasures(l2.mod.base.fit)
  l2.mod.hypo.fit.measures <- lavaan::fitmeasures(l2.mod.hypo.fit)

  #...................
  ### Chi-square and number of parameters ####

  mod.chisq <- mod.hypo.fit.measures[c("chisq", "df", "pvalue", "chisq.scaled", "df.scaled", "pvalue.scaled", "chisq.scaling.factor")]

  l1.chisq <- l1.mod.hypo.fit.measures[c("chisq", "df", "pvalue", "chisq.scaled", "df.scaled", "pvalue.scaled", "chisq.scaling.factor")]
  l2.chisq <- l2.mod.hypo.fit.measures[c("chisq", "df", "pvalue", "chisq.scaled", "df.scaled", "pvalue.scaled", "chisq.scaling.factor")]

  # Number of parameters
  npar <- mod.hypo.fit.measures["npar"]

  # Level 1 free parameter table
  l1.mod.par.free <- mod.par[mod.par$level == 1L & mod.par$free != 0L, ]

  l1.npar <- ifelse(any(l1.mod.par.free$label != ""), nrow(l1.mod.par.free) - (table(l1.mod.par.free[l1.mod.par.free$label != "", "label"]) - 1L), nrow(l1.mod.par.free))

  # Level free parameter table
  l2.mod.par.free <- mod.par[mod.par$level == 2L & mod.par$free != 0L, ]

  l2.npar <- ifelse(any(l2.mod.par.free$label != ""), nrow(l2.mod.par.free) - (table(l2.mod.par.free[l2.mod.par.free$label != "", "label"]) - 1L), nrow(l2.mod.par.free))

  #...................
  ### CFI ####

  cfi <- c(# CFI
           cfi = unname(mod.hypo.fit.measures["cfi"]),
           # Within CFI
           cfi.w = unname(1 - (l1.mod.hypo.fit.measures["chisq"] - l1.mod.hypo.fit.measures["df"]) /
                              (l1.mod.base.fit.measures["chisq"] - l1.mod.base.fit.measures["df"])),
           # Between CFI
           cfi.b = unname(1 - (l2.mod.hypo.fit.measures["chisq"] - l2.mod.hypo.fit.measures["df"]) /
                              (l2.mod.base.fit.measures["chisq"] - l2.mod.base.fit.measures["df"])),
           # Ad Hoc CFI
           cfi.ad = unname(mod.hypo.fit.measures["cfi.scaled"]),
           # Within Ad Hoc CFI
           cfi.ad.w = unname(1 - (l1.mod.hypo.fit.measures["chisq.scaled"] - l1.mod.hypo.fit.measures["df"]) /
                                 (l1.mod.base.fit.measures["chisq.scaled"] - l1.mod.base.fit.measures["df"])),
           # Between Ad Hoc CFI
           cfi.ad.b = unname(1 - (l2.mod.hypo.fit.measures["chisq.scaled"] - l2.mod.hypo.fit.measures["df"]) /
                                 (l2.mod.base.fit.measures["chisq.scaled"] - l2.mod.base.fit.measures["df"])),
           # Robust CFI
           cfi.ro = unname(mod.hypo.fit.measures["cfi.robust"]),
           # Within Robust CFI
           cfi.ro.w = unname(1 - (l1.mod.hypo.fit.measures["chisq"] - l1.mod.hypo.fit.measures["chisq.scaling.factor"] * l1.mod.hypo.fit.measures["df"]) /
                                 (l1.mod.base.fit.measures["chisq"] - l1.mod.base.fit.measures["chisq.scaling.factor"] * l1.mod.base.fit.measures["df"])),
           # Between Robust CFI
           cfi.ro.b = unname(1 - (l2.mod.hypo.fit.measures["chisq"] - l2.mod.hypo.fit.measures["chisq.scaling.factor"] * l2.mod.hypo.fit.measures["df"]) /
                                 (l2.mod.base.fit.measures["chisq"] - l2.mod.base.fit.measures["chisq.scaling.factor"] * l2.mod.base.fit.measures["df"])))

  cfi <- ifelse(cfi < 0L, 0L, cfi)
  cfi <- ifelse(cfi > 1L, 1L, cfi)

  #...................
  ### TLI ####

  # Parsimony ratio
  l1.parsi <- l1.mod.hypo.fit.measures["df"] / l1.mod.base.fit.measures["df"]
  l2.parsi <- l2.mod.hypo.fit.measures["df"] / l2.mod.base.fit.measures["df"]

  tli <- c(# TLI
           tli = unname(mod.hypo.fit.measures["tli"]),
           # TLI Within
           tli.w = unname(1 - ((l1.mod.hypo.fit.measures["chisq"] - l1.mod.hypo.fit.measures["df"]) /
                               (l1.mod.base.fit.measures["chisq"] - l1.mod.base.fit.measures["df"])) / l1.parsi),
           # TLI Between
            tli.b = unname(1 - ((l2.mod.hypo.fit.measures["chisq"] - l2.mod.hypo.fit.measures["df"]) /
                                (l2.mod.base.fit.measures["chisq"] - l2.mod.base.fit.measures["df"])) / l2.parsi),
           # Ad hoc TLI
           tli.ad = unname(mod.hypo.fit.measures["tli.scaled"]),
           # Ad Hoc TLI Within
           tli.ad.w = unname(1 - ((l1.mod.hypo.fit.measures["chisq.scaled"] - l1.mod.hypo.fit.measures["df"]) /
                                  (l1.mod.base.fit.measures["chisq.scaled"] - l1.mod.base.fit.measures["df"])) / l1.parsi),
           # Ad Hoc TLI Between
           tli.ad.b = unname(1 - ((l2.mod.hypo.fit.measures["chisq.scaled"] - l2.mod.hypo.fit.measures["df"]) /
                                  (l2.mod.base.fit.measures["chisq.scaled"] - l2.mod.base.fit.measures["df"])) / l2.parsi),
           # Robust TLI
           tli.ro = unname(mod.hypo.fit.measures["tli.robust"]),
           # Robust TLI Within
           tli.ro.w = unname(1 - ((l1.mod.hypo.fit.measures["chisq"] - l1.mod.hypo.fit.measures["chisq.scaling.factor"] * l1.mod.hypo.fit.measures["df"]) /
                                  (l1.mod.base.fit.measures["chisq"] - l1.mod.base.fit.measures["chisq.scaling.factor"] * l1.mod.base.fit.measures["df"])) / l1.parsi),
           # Robust TLI Between
           tli.ro.b = unname(1 - ((l2.mod.hypo.fit.measures["chisq"] - l2.mod.hypo.fit.measures["chisq.scaling.factor"] * l2.mod.hypo.fit.measures["df"]) /
                                  (l2.mod.base.fit.measures["chisq"] - l2.mod.base.fit.measures["chisq.scaling.factor"] * l2.mod.base.fit.measures["df"])) / l2.parsi))

  #...................
  ### RMSEA ####

  mod.rmsea <- mod.hypo.fit.measures[grep("rmsea", names(l1.mod.hypo.fit.measures))]
  l1.rmsea <- l1.mod.hypo.fit.measures[grep("rmsea", names(l1.mod.hypo.fit.measures))]
  l2.rmsea <- l2.mod.hypo.fit.measures[grep("rmsea", names(l2.mod.hypo.fit.measures))]

  #...................
  ### Check degrees of freedom ####

  # Saturated model at the Within level
  if(isTRUE(l1.mod.hypo.fit.measures["df"] == 0L)) {

    warning("Model specified at the Within level is saturated, i.e. there are no degrees of freedom.", call. = FALSE)

    cfi[grep(".w", names(cfi))] <- 1
    tli[grep(".w", names(tli))] <- 1
    l1.rmsea[c("rmsea.scaled", "rmsea.robust")] <- 0
    l1.rmsea[c("rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "rmsea.pvalue.scaled", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust", "rmsea.pvalue.robust")] <- NA

  # Scaled test statistic not available when estimator = "MLR"
  } else if (isTRUE(lavaan::lavInspect(x, what = "options")$test != "standard" && is.na(l1.chisq["chisq.scaled"]))) {

    warning("(Scaled and robust) CFI, TLI, and RMSEA at the Within level are not available due to estimation problems.", call. = FALSE)

    cfi[grep(".w", names(cfi))] <- NA
    tli[grep(".w", names(tli))] <- NA
    l1.rmsea[c("rmsea.scaled", "rmsea.robust")] <- NA
    l1.rmsea[c("rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "rmsea.pvalue.scaled", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust", "rmsea.pvalue.robust")] <- NA

  }

  # Saturated model at the Between level
  if(isTRUE(l2.mod.hypo.fit.measures["df"] == 0L)) {

    warning("Model specified at the Between level is saturated, i.e. there are no degrees of freedom.", call. = FALSE)

    cfi[grep(".b", names(cfi))] <- 1
    tli[grep(".b", names(tli))] <- 1
    l2.rmsea[c("rmsea.scaled", "rmsea.robust")] <- 0
    l2.rmsea[c("rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "rmsea.pvalue.scaled", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust", "rmsea.pvalue.robust")] <- NA

  # Scaled test statistic not available when estimator = "MLR"
  } else if (isTRUE(is.na(lavaan::lavInspect(x, what = "options")$test != "standard" && l2.chisq["chisq.scaled"]))) {

    warning("(Scaled and robust) CFI, TLI, and RMSEA at the Between level are not available due to estimation problems.", call. = FALSE)

    cfi[grep(".b", names(cfi))] <- NA
    tli[grep(".b", names(tli))] <- NA
    l2.rmsea[c("rmsea.scaled", "rmsea.robust")] <- NA
    l2.rmsea[c("rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "rmsea.pvalue.scaled", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust", "rmsea.pvalue.robust")] <- NA

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## lavaan summary ####

  # Level 1 model parameters
  npar.l1 <- sum(mod.par$level == 1L & mod.par$free != 0L)

  # Level 2 model parameters
  npar.l2 <- sum(mod.par$level == 2L & mod.par$free != 0L)

  # Number of model parameters
  npar <- npar.l1 + npar.l2

  # Number of equality constraints
  npar.eq <- sum(table(misty::chr.omit(mod.par$label)) - 1L)

  # Summary
  lavaan.summary <- data.frame(# First column
                               c(paste("lavaan", lavaan::lavInspect(x, what = "version")), "", "Estimator", "Optimization Method", "",
                                 "Test Statistic", "Standard Errors", "Missing Data", "",
                                 "Numer of Model Parameters", "Within", "Between",
                                 "Numer of Equality Constraints", "", "",
                                 "Number of Observations", "Number of Clusters", "Average Cluster Size"),
                               # Second column
                               unlist(c("", "",
                                        # Estimator
                                        ifelse(lavaan::lavInspect(x, what = "options")$test == "standard", "ML", "MLR"),
                                        # Optimization method
                                        toupper(lavaan::lavTech(x, what = "options")$optim.method), "",
                                        # Test statistic
                                        switch(lavaan::lavTech(x, what = "options")$test,
                                               "standard" = "Conventional",
                                               "satorra.bentler" = "Satorra-Bentler",
                                               "scaled.shifted" = "Scale-Shifted",
                                               "mean.var.adjusted" = "Satterthwaite",
                                               "yuan.bentler.mplus" = "Yuan-Bentler"),
                                        # Standard errors
                                        switch(lavaan::lavTech(x, what = "options")$se,
                                               "standard" = "Conventional",
                                               "robust.sem" = "Conventional Robust",
                                               "robust.huber.white" = "Huber-White",
                                               "robust.cluster" = "Cluster-Robust H-W",
                                               "robust.cluster.sem" = "Cluster-Robust Conven",
                                               "two.stage" = "Two-Stage",
                                               "robust.two.stage" = "Robust Two-Stage"),
                                        # Missing data
                                        ifelse(lavaan::lavInspect(x, what = "nobs") != lavaan::lavInspect(x, what = "norig"), "Listwise",
                                               ifelse(lavaan::lavInspect(x, what = "nobs") == lavaan::lavInspect(x, what = "norig") && any(is.na(lavaan::lavInspect(x, what = "data"))), "FIML", "None")), "",
                                        # Numer of model parameters
                                        npar, npar.l1, npar.l2,
                                        # Numer of equality constraints
                                        npar.eq, "", "Used",
                                        # Number of observations
                                        lavaan::lavInspect(x, what = "nobs"),
                                        # Number of clusters
                                        lavaan::lavInspect(x, what = "nclusters"),
                                        # Average cluster size
                                        lavaan::lavInspect(x, what = "ncluster.size"))),
                               # Third column
                               c(rep("", times = 14L), "Total", lavaan::lavInspect(x, what = "norig"), "", ""),
                               fix.empty.names = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model fit ####

  model.fit.measures <- data.frame(# Fist column
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
                                     "SRMR", "Within", "Between"),
                                   # Second column
                                   standard = c(# Loglikelihood
                                                NA, mod.hypo.fit.measures[c("logl", "scaling.factor.h0", "unrestricted.logl", "scaling.factor.h1")], NA, NA,
                                                # Information Criteria
                                                mod.hypo.fit.measures[c("aic", "bic", "bic2")], NA, NA,
                                                # Test statistic
                                                mod.chisq["chisq"], l1.chisq["chisq"], l2.chisq["chisq"],
                                                # Degrees of freedom
                                                mod.chisq["df"], l1.chisq["df"], l2.chisq["df"],
                                                # P-value
                                                mod.chisq["pvalue"], l1.chisq["pvalue"], l2.chisq["pvalue"],
                                                # Scaling correction factor
                                                mod.chisq["chisq.scaling.factor"], l1.chisq["chisq.scaling.factor"], l2.chisq["chisq.scaling.factor"], NA, NA,
                                                # CFI
                                                cfi[c("cfi", "cfi.w", "cfi.b")], NA,
                                                # TLI
                                                tli[c("tli", "tli.w", "tli.b")], NA, NA,
                                                # RMSEA
                                                mod.rmsea["rmsea"], l1.rmsea["rmsea"], l2.rmsea["rmsea"], NA,
                                                mod.rmsea["rmsea.ci.lower"], l1.rmsea["rmsea.ci.lower"], l2.rmsea["rmsea.ci.lower"],
                                                mod.rmsea["rmsea.ci.upper"], l1.rmsea["rmsea.ci.upper"], l2.rmsea["rmsea.ci.upper"],
                                                mod.rmsea["rmsea.pvalue"], l1.rmsea["rmsea.pvalue"], l2.rmsea["rmsea.pvalue"], NA,
                                                # SRMR
                                                mod.hypo.fit.measures["srmr"], mod.hypo.fit.measures["srmr_within"], mod.hypo.fit.measures["srmr_between"]),
                                   # Third column
                                   scaled = c(# Loglikelihood and Information Criteria
                                              rep(NA, times = 12L),
                                              # Test statistic
                                              mod.chisq["chisq.scaled"], l1.chisq["chisq.scaled"], l2.chisq["chisq.scaled"],
                                              # Degrees of freedom
                                              mod.chisq["df.scaled"], l1.chisq["df.scaled"], l2.chisq["df.scaled"],
                                              # P-value
                                              mod.chisq["pvalue.scaled"], l1.chisq["pvalue.scaled"], l2.chisq["pvalue.scaled"],
                                              # Scaling correction factor
                                              mod.chisq["chisq.scaling.factor"], l1.chisq["chisq.scaling.factor"], l2.chisq["chisq.scaling.factor"], NA, NA,
                                              # CFI
                                              cfi[c("cfi.ad", "cfi.ad.w", "cfi.ad.b")], NA,
                                              # TLI
                                              tli[c("tli.ad", "tli.ad.w", "tli.ad.b")], NA, NA,
                                              # RMSEA
                                              mod.rmsea["rmsea.scaled"], l1.rmsea["rmsea.scaled"], l2.rmsea["rmsea.scaled"], NA,
                                              mod.rmsea["rmsea.ci.lower.scaled"], l1.rmsea["rmsea.ci.lower.scaled"], l2.rmsea["rmsea.ci.lower.scaled"],
                                              mod.rmsea["rmsea.ci.upper.scaled"], l1.rmsea["rmsea.ci.upper.scaled"], l2.rmsea["rmsea.ci.upper.scaled"],
                                              mod.rmsea["rmsea.pvalue.scaled"], l1.rmsea["rmsea.pvalue.scaled"], l2.rmsea["rmsea.pvalue.scaled"], NA,
                                              # SRMR
                                              rep(NA, times = 3L)),
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
                                              cfi[c("cfi.ro", "cfi.ro.w", "cfi.ro.b")], NA,
                                              # TLI
                                              tli[c("tli.ro", "tli.ro.w", "tli.ro.b")], NA, NA,
                                              # RMSEA
                                              mod.rmsea["rmsea.robust"], l1.rmsea["rmsea.robust"], l2.rmsea["rmsea.robust"], NA,
                                              mod.rmsea["rmsea.ci.lower.robust"], l1.rmsea["rmsea.ci.lower.robust"], l2.rmsea["rmsea.ci.lower.robust"],
                                              mod.rmsea["rmsea.ci.upper.robust"], l1.rmsea["rmsea.ci.upper.robust"], l2.rmsea["rmsea.ci.upper.robust"],
                                              mod.rmsea["rmsea.pvalue.robust"], l1.rmsea["rmsea.pvalue.robust"], l2.rmsea["rmsea.pvalue.robust"], NA,
                                              # SRMR
                                              rep(NA, times = 3L)),
                                   fix.empty.names = FALSE)

  if (isTRUE(lavaan::lavInspect(x, what = "options")$test == "standard")) {

    model.fit.measures <- model.fit.measures[-c(3L, 5L, 22:24L), c(1L, 2L)]

    rownames(model.fit.measures) <- seq_len(nrow(model.fit.measures))

  }

  #_____________________________________________________________________________
  #
  # Return object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "multilevel.fit",
                 x = x,
                 args = list(print = print, digits = digits, p.digits = p.digits, check = check, output = output),
                 model = list(mod.l1 = mod.l1, mod.l1.syntax = mod.l1.syntax,
                              mod.l2 = mod.l2, mod.l2.syntax = mod.l2.syntax,
                              mod.l12 = mod.l12, mod.l12.syntax = mod.l12.syntax,
                              l1.mod.base = l1.mod.base, l1.mod.hypo = l1.mod.hypo,
                              l2.mod.base = l2.mod.base, l2.mod.hypo = l2.mod.hypo),
                 model.fit = list(l1.mod.base.fit = l1.mod.base.fit, l1.mod.hypo.fit = l1.mod.hypo.fit,
                                  l2.mod.base.fit = l2.mod.base.fit, l2.mod.hypo.fit = l2.mod.hypo.fit),
                 result = list(summary = lavaan.summary, fit = model.fit.measures))

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Write Results --------------------------------------------------------------

  if (isTRUE(!is.null(write))) { misty::write.result(object, file = write) }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
