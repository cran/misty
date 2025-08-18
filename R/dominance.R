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
#' @param digits an integer value indicating the number of decimal places to be
#'               used for displaying results. Note that the percentage relative
#'               importance of predictors are printed with \code{digits} minus 1
#'               decimal places.
#' @param write  a character string naming a file for writing the output into
#'               either a text file with file extension \code{".txt"} (e.g.,
#'               \code{"Output.txt"}) or Excel file with file extension
#'               \code{".xlsx"}  (e.g., \code{"Output.xlsx"}). If the file
#'               name does not contain any file extension, an Excel file will
#'               be written.
#' @param append logical: if \code{TRUE} (default), output will be appended
#'               to an existing text file with extension \code{.txt} specified
#'               in \code{write}, if \code{FALSE} existing text file will be
#'               overwritten.
#' @param check  logical: if \code{TRUE} (default), argument specification is checked.
#' @param output logical: if \code{TRUE} (default), output is shown.
#'
#' @details
#' Dominance analysis (Budescu, 1993; Azen & Budescu, 2003) is used to determine
#' the relative importance of predictor variables in a statistical model by examining
#' the additional contribution of predictors in \emph{R}-squared relative to each
#' other in all of the possible \eqn{2^{(p - 2)}} subset models with \eqn{p} being
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
#' \code{\link{dominance.manual}}, \code{\link{coeff.std}}, \code{\link{write.result}}
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
#' #----------------------------------------------------------------------------
#' # Example 1: Dominance analysis for a linear model
#'
#' mod <- lm(mpg ~ cyl + disp + hp, data = mtcars)
#' dominance(mod)
#'
#' # Print all results
#' dominance(mod, print = "all")
#'
#' #----------------------------------------------------------------------------
#' # Example 2: Write Results into a text file
#'
#' dominance(mod, write = "Dominance.txt", output = FALSE)
#'
#' #----------------------------------------------------------------------------
#' # Example 3: Write Results into a Excel file
#'
#' dominance(mod, write = "Dominance.xlsx", output = FALSE)
dominance <- function(model, print = c("all", "gen", "cond", "comp"), digits = 3,
                      write = NULL, append = TRUE, check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'model' is missing
  if (isTRUE(missing(model))) { stop("Input for the argument 'model' is missing.", call. = FALSE) }

  # Check if input 'model' is NULL
  if (isTRUE(is.null(model))) { stop("Input specified for the argument 'model' is NULL.", call. = FALSE) }

  # Check if input 'model' is not 'lm'
  if (isTRUE(!inherits(model, "lm"))) { stop("Please specify an \"lm\" object for the argument 'model'.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("append", "output"), m.character = list(print = c("all", "gen", "cond", "comp")), args = c("digts", "write2"), envir = environment(), input.check = check)

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
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Dominance analysis ####

  domin.res <- .domin(eval(model$call[[2L]]), lm, list(summary, "r.squared"), data = model$model)

  #_____________________________________________________________________________
  #
  # Return object --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## General dominance ####

  gen.res <- data.frame(r2 = domin.res$General_Dominance, perc = domin.res$Standardized * 100, rank = domin.res$Ranks) |>
    (\(y) rbind(y, c(sum(y[, "r2"]), sum(y[, "perc"]), NA)))()

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
                             append = append, check = check, output = output),
                 result = list(gen = gen.res, cond = cond.res, comp = comp.res, condstat = cond))

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
