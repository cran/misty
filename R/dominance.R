#' Dominance Analysis
#'
#' This function conducts dominance analysis (Budescu, 1993; Azen & Budescu, 2003)
#' for linear models estimated by using the \code{lm()} function to determine the
#' relative importance of predictor variables. By default, the function reports
#' general dominance, but conditional and complete dominance can be requested by
#' specifying the argument \code{print}.
#'
#' @param model  a fitted model of class \code{lm}.
#' @param print  a character string or character vector indicating which results
#'               to show on the console, i.e. \code{"all"} for all results, \code{"gen"}
#'               for general dominance, \code{"cond"} for conditional dominance,
#'               and \code{"comp"} for complete dominance.
#' @param write  a character string for writing the results into a Excel file
#'               naming a file with or without file extension '.xlsx', e.g.,
#'               \code{"Results.xlsx"} or \code{"Results"}.
#' @param digits an integer value indicating the number of decimal places to be
#'               used for displaying results. Note that the percentage relative
#'               importance of predictors are printed with \code{digits} minus 1
#'               decimal places.
#' @param check  logical: if \code{TRUE}, argument specification is checked.
#' @param output logical: if \code{TRUE}, output is shown.
#'
#' @details
#' Dominance analysis (Budescu, 1993; Azen & Budescu, 2003) is used to determine
#' the relative importance of predictor variables in a statistical model by examining
#' the additional contribution of predictors in \emph{R}-squared relative to each
#' other in all of the possible \eqn{2^{(p - 2)]} subset models with \eqn{p} being
#' the number of predictors. Three levels of dominance can be established through
#' pairwise comparison of all predictors in a regression model:
#' \describe{
#' \item{\strong{Complete Dominance}}{A predictor completely dominates another
#' predictor if its additional contribution in \emph{R}-Squared is higher than that
#' of the other predictor across all possible subset models that do not include both
#' predictors. For example, in a regression model with four predictors, \eqn{X_1}
#' completely dominates \eqn{X_2} if the additional contribution in \emph{R}-squared
#' for \eqn{X_1} is higher compared to \eqn{X_2} in (1) the null model without any
#' predictors, (2) the model including \eqn{X_3}, (3) the model including
#' \eqn{X_4}, and (4) the model including both \eqn{X_3} and \eqn{X_4}. Note
#' that complete dominance cannot be established if one predictor's additional
#' contribution is greater than the other's for some, but not all of the subset
#' models. In this case, dominance is undetermined and the result will be \code{NA}}
#' \item{\strong{Conditional Dominance}}{A predictor conditionally dominates another
#' predictor if its average additional contribution in \emph{R}-squared is higher
#' within each model size than that of the other predictor. For example, in a
#' regression model with four predictors, \eqn{X_1} conditionally dominates \eqn{X_2}
#' if the average additional contribution in \emph{R}-squared is higher compared
#' to \eqn{X_2} in (1) the null model without any predictors, (2) the four models
#' including one predictor, (3) the six models including two predictors, and (4)
#' the four models including three predictors.}
#' \item{\strong{General Dominance}}{A predictor generally dominates another predictor
#' if its overall averaged additional contribution in \emph{R}-squared is higher
#' than that of the other predictor. For example, in a regression model with four
#' predictors, \eqn{X_1} generally dominates \eqn{X_2} if the average across the
#' four conditional values (i.e., null model, model with one predictor, model with
#' two predictors, and model with three predictors) is higher than that of \eqn{X_2}.
#' Note that the general dominance measures represent the proportional contribution
#' that each predictor makes to the \emph{R}-squared since their sum across all
#' predictors equals the  \emph{R}-squared of the full model.}
#' The three levels of dominance are related to each other in a hierarchical fashion:
#' Complete dominance implies conditional dominance, which in turn implies general
#' dominance. However, the converse may not hold for more than three predictors.
#' That is, general dominance does not imply conditional dominance, and conditional
#' dominance does not necessarily imply complete dominance.
#' }
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{dominance.manual}}, \code{\link{std.coef}}, \code{\link{write.result}}
#'
#' @references
#' Azen, R., & Budescu, D. V. (2003). The dominance analysis approach for comparing
#' predictors in multiple regression. \emph{Psychological Methods, 8}(2), 129–148.
#'  https://doi.org/10.1037/1082-989X.8.2.129
#'
#' Budescu, D. V. (1993). Dominance analysis: A new approach to the problem of
#' relative importance of predictors in multiple regression. \emph{Psychological Bulletin, 114}(3),
#' 542–551. https://doi.org/10.1037/0033-2909.114.3.542
#'
#' Luchman J (2023). \emph{domir: Tools to support relative importance analysis}. R package
#' version 1.0.1, https://CRAN.R-project.org/package=domir.
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{model}}{model specified in \code{model}}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{result}}{list with results, i.e., \code{gen} for general dominance,
#' \code{cond} for conditional dominance, \code{comp} for complete dominance,
#' and \code{condtsat} for the statistics of the conditional dominance}
#'
#' @note
#' This function is based on the \code{domir} function from the \code{domir}
#' package (Luchman, 2023).
#'
#' @export
#'
#' @examples
#' dat <- data.frame(x1 = c(3, 2, 4, 9, 5, 3, 6, 4, 5, 6, 3, 5),
#'                   x2 = c(1, 4, 3, 1, 2, 4, 3, 5, 1, 7, 8, 7),
#'                   x3 = c(0, 0, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1),
#'                   y  = c(0, 1, 0, 2, 0, 1, 0, 0, 1, 2, 1, 0))
#'
#' #----------------------------
#' # Dominance analysis for a linear model
#'
#' mod <- lm(y ~ x1 + x2 + x3, data = dat)
#' dominance(mod)
#'
#' # Print all results
#' dominance(mod, print = "all")
#'
#' \dontrun{
#' #----------------------------
#' # Write Results into a Excel file
#'
#' mod <- lm(y ~ x1 + x2 + x3, data = dat)
#'
#' dominance(mod, write = "Dominance.xlsx", output = FALSE)
#'
#' result <- dominance(mod, print = "all", output = FALSE)
#' write.result(result, "Dominance.xlsx")
#' }
dominance <- function(model, print = c("all", "gen", "cond", "comp"), digits = 3,
                      write = NULL, check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'model' is missing
  if (isTRUE(missing(model))) { stop("Input for the argument 'model' is missing.", call. = FALSE) }

  # Check if input 'model' is NULL
  if (isTRUE(is.null(model))) { stop("Input specified for the argument 'model' is NULL.", call. = FALSE) }

  # Check if input 'model' is not 'lm'
  if (isTRUE(class(model) != "lm")) { stop("Please specify an \"lm\" object for the argument 'model'.", call. = FALSE) }


  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    ## Check input 'print' ##
    if (isTRUE(!all(print %in% c("all", "gen", "cond", "comp")))) { stop("Character string in the argument 'print' does not match with \"gen\", \"cond\", or \"comp\".", call. = FALSE) }

    ## Check input 'digits' ##
    if (isTRUE(digits %% 1L != 0L || digits < 0L || digits == 0L)) { stop("Specify a positive integer number for the argument 'digits'.", call. = FALSE) }

    ## Check input 'output' ##
    if (isTRUE(!is.logical(output))) { stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Data and Arguments ---------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Print ####

  if (isTRUE(all(c("all", "gen", "cond", "comp") %in% print))) {

    print <- "gen"

  } else {

    if (isTRUE(length(print) != 1L)) {

      stop("Please specify a character string for the argument 'print'", call. = FALSE)

    } else if (isTRUE("all" %in% print)) {

      print <- c("gen", "cond", "comp")

    }

  }

  #_____________________________________________________________________________
  #
  # Internal Functions ---------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Dominance analysis supporting formula-based modeling functions ####

  domin <- function(formula_overall, reg, fitstat, sets = NULL, all = NULL,
                    conditional = TRUE, complete = TRUE, consmodel = NULL, reverse = FALSE, ...) {

    # Input check
    if (isTRUE(!inherits(formula_overall, "formula"))) { stop(paste(formula_overall, "is not a 'formula' class object."), call. = FALSE) }

    if (isTRUE(!is.null(attr(stats::terms(formula_overall), "offset")))) { stop("'offset()' terms not allowed in formula object.", call. = FALSE) }

    if (isTRUE(!is.list(fitstat))) { stop("fitstat is not a list.", call. = FALSE) }

    if (isTRUE(length(sets) > 0L & !is.list(sets))) { stop("sets is not a list.", call. = FALSE) }

    if (isTRUE(is.list(all))) { stop("all is a list.  Please submit it as a vector.", call. = FALSE) }

    if (isTRUE(!attr(stats::terms(formula_overall), "response"))) { stop(paste(deparse(formula_overall), "missing a response."), call. = FALSE) }

    if (isTRUE(any(attr(stats::terms(formula_overall), "order") > 1L))) { warning(paste(deparse(formula_overall), "contains second or higher order terms, fsunction may not handle them correctly."), call. = FALSE) }

    if (isTRUE(length(fitstat) < 2L)) { stop("fitstat requires at least two elements.", call. = FALSE) }

    # Process variable lists
    Indep_Vars <- attr(stats::terms(formula_overall), "term.labels")

    intercept <- as.logical(attr(stats::terms(formula_overall), "intercept"))

    if (isTRUE(length(sets) > 0L)) {

      set_aggregated <- sapply(sets, paste0, collapse = " + ")

      Indep_Vars <- append(Indep_Vars, set_aggregated)

    }

    Dep_Var <- attr(stats::terms(formula_overall), "variables")[[2L]]

    Total_Indep_Vars <- length(Indep_Vars)

    # IV-based exit conditions
    if (isTRUE(Total_Indep_Vars < 2L)) { stop(paste("Total of", Total_Indep_Vars, "independent variables or sets. At least 2 needed for useful dominance analysis."), call. = FALSE) }

    # Create independent variable/set combination list
    Combination_Matrix <- expand.grid(lapply(1:Total_Indep_Vars, function(x) c(FALSE, TRUE)), KEEP.OUT.ATTRS = FALSE)[-1L, ]

    Total_Models_to_Estimate <- 2L**Total_Indep_Vars - 1L

    # Define function to call regression models
    doModel_Fit <- function(Indep_Var_Combin_lgl, Indep_Vars, Dep_Var, reg, fitstat, all = NULL, consmodel = NULL, intercept, ...) {

      Indep_Var_Combination <- Indep_Vars[Indep_Var_Combin_lgl]

      formula_to_use <- stats::reformulate(c(Indep_Var_Combination, all, consmodel), response = Dep_Var, intercept = intercept)

      Model_Result <- list(do.call(reg, list(formula_to_use, ...)))

      if (isTRUE(length(fitstat) > 2L)) { Model_Result <- append(Model_Result, fitstat[3L:length(fitstat)]) }

      Fit_Value <- do.call(fitstat[[1L]], Model_Result)

      return( Fit_Value[[ fitstat[[2L]]]])

    }

    # Constant model adjustments
    Cons_Result <- NULL
    FitStat_Adjustment <- 0L
    if (isTRUE(length(consmodel) > 0L)) {

      FitStat_Adjustment <- Cons_Result <- doModel_Fit(NULL, Indep_Vars, Dep_Var, reg, fitstat, consmodel = consmodel, intercept = intercept, ...)

    }

    # All subsets adjustment
    All_Result <- NULL
    if (isTRUE(length(all) > 0L)) {

      FitStat_Adjustment <- All_Result <- doModel_Fit(NULL, Indep_Vars, Dep_Var, reg, fitstat, all = all, consmodel = consmodel, intercept = intercept, ...)

    }

    # Obtain all subsets regression results
    Ensemble_of_Models <- sapply(1L:nrow(Combination_Matrix), function(x) { doModel_Fit(unlist(Combination_Matrix[x, ]), Indep_Vars, Dep_Var, reg, fitstat, all = all, consmodel = consmodel, intercept = intercept, ...) },
                                 simplify = TRUE, USE.NAMES = FALSE)

    # Conditional dominance statistics
    Conditional_Dominance <- NULL
    if (isTRUE(conditional)) {

      Conditional_Dominance <- matrix(nrow = Total_Indep_Vars, ncol = Total_Indep_Vars)

      Combination_Matrix_Anti <-!Combination_Matrix

      IVs_per_Model <- rowSums(Combination_Matrix)

      Combins_at_Order <- sapply(IVs_per_Model, function(x) choose(Total_Indep_Vars, x), simplify = TRUE, USE.NAMES = FALSE)

      Combins_at_Order_Prev <- sapply(IVs_per_Model, function(x) choose(Total_Indep_Vars - 1L, x), simplify = TRUE, USE.NAMES = FALSE)

      Weighted_Order_Ensemble <- ((Combination_Matrix*(Combins_at_Order - Combins_at_Order_Prev))**-1L)*Ensemble_of_Models

      Weighted_Order_Ensemble <- replace(Weighted_Order_Ensemble, Weighted_Order_Ensemble == Inf, 0L)

      Weighted_Order_Ensemble_Anti <- ((Combination_Matrix_Anti*Combins_at_Order_Prev)**-1L)*Ensemble_of_Models

      Weighted_Order_Ensemble_Anti <- replace(Weighted_Order_Ensemble_Anti, Weighted_Order_Ensemble_Anti == Inf, 0L)

      for (order in seq_len(Total_Indep_Vars)) {

        Conditional_Dominance[, order] <- t(colSums(Weighted_Order_Ensemble[IVs_per_Model == order, ]) - colSums(Weighted_Order_Ensemble_Anti[IVs_per_Model == (order - 1L), ]))

      }

      Conditional_Dominance[, 1L] <- Conditional_Dominance[, 1L] - FitStat_Adjustment

    }

    # Complete dominance statistics
    Complete_Dominance <- NULL
    if (isTRUE(complete)) {

      Complete_Dominance <- matrix(data = NA, nrow = Total_Indep_Vars, ncol = Total_Indep_Vars)

      Complete_Combinations <- utils::combn(seq_len(Total_Indep_Vars), 2L)

      for (pair in 1L:ncol(Complete_Combinations)) {

        Focal_Cols <- Complete_Combinations[, pair]

        NonFocal_Cols <- setdiff(1:Total_Indep_Vars, Focal_Cols)

        Select_2IVs <- cbind(Combination_Matrix, 1L:nrow(Combination_Matrix))[rowSums(Combination_Matrix[, Focal_Cols]) == 1L, ]

        Sorted_2IVs <- Select_2IVs[do.call("order", as.data.frame(Select_2IVs[,c(NonFocal_Cols, Focal_Cols)])), ]

        Compare_2IVs <- cbind(Ensemble_of_Models[Sorted_2IVs[(1L:nrow(Sorted_2IVs) %% 2L) == 0L, ncol(Sorted_2IVs)]], Ensemble_of_Models[Sorted_2IVs[(1L:nrow(Sorted_2IVs) %% 2) == 1L, ncol(Sorted_2IVs)]])

        Complete_Designation <- ifelse(all(Compare_2IVs[, 1L] > Compare_2IVs[, 2L]), FALSE, ifelse(all(Compare_2IVs[, 1L] < Compare_2IVs[, 2L]), TRUE, NA))

        Complete_Dominance[Focal_Cols[[2L]], Focal_Cols[[1L]]] <- Complete_Designation

        Complete_Dominance[Focal_Cols[[1L]], Focal_Cols[[2L]]] <- !Complete_Designation

      }

    }

    if (isTRUE(reverse)) { Complete_Dominance <- !Complete_Dominance }

    # General dominance statistics
    General_Dominance <- rowMeans(Conditional_Dominance)
    if (isTRUE(!conditional)) {

      Combination_Matrix_Anti <-!Combination_Matrix

      IVs_per_Model <- rowSums(Combination_Matrix)

      Combins_at_Order <- sapply(IVs_per_Model, function(x) choose(Total_Indep_Vars, x), simplify = TRUE, USE.NAMES = FALSE)

      Combins_at_Order_Prev <- sapply(IVs_per_Model, function(x) choose(Total_Indep_Vars - 1L, x), simplify = TRUE, USE.NAMES = FALSE)

      Indicator_Weight <- Combination_Matrix*(Combins_at_Order - Combins_at_Order_Prev)

      Indicator_Weight_Anti <- (Combination_Matrix_Anti*Combins_at_Order_Prev)*-1L

      Weight_Matrix <- ((Indicator_Weight + Indicator_Weight_Anti)*Total_Indep_Vars)^-1L

      General_Dominance <- colSums(Ensemble_of_Models*Weight_Matrix)

      General_Dominance <- General_Dominance - FitStat_Adjustment/Total_Indep_Vars

    }

    # Overall fit statistic and ranks
    FitStat <- sum(General_Dominance) + FitStat_Adjustment

    if (isTRUE(!reverse)) { General_Dominance_Ranks <- rank(-General_Dominance) } else { General_Dominance_Ranks <- rank(General_Dominance) }

    # Return values and attributes
    if (isTRUE(length(sets) == 0L)) IV_Labels <- { attr(stats::terms(formula_overall), "term.labels") } else { IV_Labels <- c( attr(stats::terms(formula_overall), "term.labels"), paste0("set", 1:length(sets))) }

    names(General_Dominance) <- IV_Labels
    names(General_Dominance_Ranks) <- IV_Labels
    if (isTRUE(conditional)) { dimnames(Conditional_Dominance) <- list(IV_Labels, paste0("IVs_", seq_along(Indep_Vars))) }

    if (isTRUE(complete)) { dimnames(Complete_Dominance) <- list(paste0("Dmnates_", IV_Labels),  paste0("Dmnated_", IV_Labels)) }

    if (isTRUE(!reverse)) { Standardized <- General_Dominance / (FitStat - ifelse(length(Cons_Result) > 0L, Cons_Result, 0L)) } else { Standardized <- -General_Dominance / -(FitStat - ifelse(length(Cons_Result) > 0L, Cons_Result, 0L)) }

    # Return object
    return_list <- list(General_Dominance = General_Dominance,
                        Standardized = Standardized,
                        Ranks = General_Dominance_Ranks,
                        Conditional_Dominance = Conditional_Dominance,
                        Complete_Dominance = Complete_Dominance,
                        Fit_Statistic_Overall = FitStat,
                        Fit_Statistic_All_Subsets = All_Result - ifelse(is.null(Cons_Result), 0, Cons_Result),
                        Fit_Statistic_Constant_Model = Cons_Result,
                        Call = match.call(),
                        Subset_Details = list(Full_Model = stats::reformulate(c(Indep_Vars, all, consmodel), response = Dep_Var, intercept = intercept),
                                              Formula = attr(stats::terms(formula_overall), "term.labels"),
                                              All = all, Sets = sets, Constant = consmodel))

    class(return_list) <- c("domin", "list")

    return(return_list)

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Dominance analysis ####

  domin.res <- domin(eval(model$call[[2L]]),
                     lm,
                     list(summary, "r.squared"),
                     data = model$model)

  #_____________________________________________________________________________
  #
  # Return object --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## General dominance ####

  gen.res <- data.frame(r2 = domin.res$General_Dominance,
                        perc = domin.res$Standardized * 100,
                        rank = domin.res$Ranks)

  gen.res <- rbind(gen.res, c(sum(gen.res[, "r2"]), sum(gen.res[, "perc"]), NA))
  row.names(gen.res) <- c(names(domin.res$General_Dominance), "Total")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Conditional dominance ####

  # Extract result table
  cond <- domin.res$Conditional_Dominance

  # Determine conditional dominance
  cond.res <- matrix(NA, ncol = ncol(cond), nrow = nrow(cond), dimnames = list(row.names(cond), row.names(cond)))
  for (i in seq_len(nrow(cond))) {

    temp <- matrix(NA, ncol = ncol(cond), nrow = nrow(cond))
    for (j in seq_len(ncol(cond))) {

      temp[-i, j] <- cond[i, j] > cond[-i, j]

    }

    cond.res[, i] <- apply(temp, 1L, all)

  }

  cond.res <- t(cond.res)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Complete dominance ####

  comp.res <- domin.res$Complete_Dominance
  rownames(comp.res) <- colnames(comp.res) <- names(domin.res$General_Dominance)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Return object ####

  object <- list(call = match.call(),
                 type = "dominance",
                 model = model,
                 args = list(print = print, digits = digits, write = write,
                             check = check, output = output),
                 result = list(gen = gen.res, cond = cond.res, comp = comp.res, condstat = cond))

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
