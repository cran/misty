#' Dominance Analysis, Manually Inputting a Correlation Matrix
#'
#' This function conducts dominance analysis (Budescu, 1993; Azen & Budescu, 2003)
#' based on a (model-implied) correlation matrix of the manifest or latent variables.
#' Note that the function only provides general dominance.
#'
#' @param x      a matrix or data frame with the (model-implied) correlation matrix
#'               of the manifest or latent variables. Note that column names need
#'               to represent the variables names in \code{x}.
#' @param out    a character string representing the outcome variable. By default,
#'               the first row and column represents the outcome variable.
#' @param write  a character string for writing the results into a Excel file
#'               naming a file with or without file extension '.xlsx', e.g.,
#'               \code{"Results.xlsx"} or \code{"Results"}.
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
#' @param check  logical: if \code{TRUE} (default), argument specification
#'               is checked.
#' @param output logical: if \code{TRUE} (default), output is shown.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{dominance}}, \code{\link{coeff.std}}, \code{\link{write.result}}
#'
#' @references
#' Azen, R., & Budescu, D. V. (2003). The dominance analysis approach for comparing
#' predictors in multiple regression. \emph{Psychological Methods, 8}(2), 129–148.
#' https://doi.org/10.1037/1082-989X.8.2.129
#'
#' Bolker, B., Warnes, G., & Lumley, T. (2022). \emph{gtools: Various R Programming Tools}.
#' R package version 3.9.4, https://CRAN.R-project.org/package=gtools
#'
#' Budescu, D. V. (1993). Dominance analysis: A new approach to the problem of
#' relative importance of predictors in multiple regression. \emph{Psychological Bulletin, 114}(3),
#' 542–551. https://doi.org/10.1037/0033-2909.114.3.542
#'
#' Gu, X. (2022). Assessing the relative importance of predictors in latent regression
#' models. \emph{Structural Equation Modeling: A Multidisciplinary Journal, 4}, 569-583.
#' https://doi.org/10.1080/10705511.2021.2025377
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{x}}{correlation matrix specified in \code{x}}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{result}}{results table for the general dominance}
#'
#' @note
#' This function implements the function provided in Appendix 1 of Gu (2022) and
#' copied the function \code{combinations()} from the \code{gtools} package
#' (Bolker, Warnes, & Lumley, 2022).
#'
#' @export
#'
#' @examples
#' #----------------------------------------------------------------------------
#' # Linear model
#'
#' # Example 1a: Dominance analysis, 'mpg' predicted by 'cyl', 'disp', and 'hp'
#' dominance.manual(cor(mtcars[, c("mpg", "cyl", "disp", "hp")]))
#'
#' # Example 1b: Equivalent results using the dominance() function
#' mod <- lm(mpg ~ cyl + disp + hp, data = mtcars)
#' dominance(mod)
#'
#' # Example 1c: Dominance analysis, 'hp' predicted by 'mpg', 'cyl', and 'disp'
#' dominance.manual(cor(mtcars[, c("mpg", "cyl", "disp", "hp")]), out = "hp")
#'
#' # Example 1d: Write Results into a text file
#' dominance.manual(cor(mtcars[, c("mpg", "cyl", "disp", "hp")]),
#'                  write = "Dominance_Manual.txt")
#'
#' #----------------------------------------------------------------------------
#' # Example 2: Structural equation modeling
#'
#' library(lavaan)
#'
#' #.............
#' # Latent variables
#'
#' # Model specification
#' model <- '# Measurement model
#'           ind60 =~ x1 + x2 + x3
#'           dem60 =~ y1 + y2 + y3 + y4
#'           dem65 =~ y5 + y6 + y7 + y8
#'           # regressions
#'           ind60 ~ dem60 + dem65'
#'
#' # Model estimation
#' fit <- sem(model, data = PoliticalDemocracy)
#'
#' # Model-implied correlation matrix of the latent variables
#' fit.cor <- lavInspect(fit, what = "cor.lv")
#'
#' # Dominance analysis
#' dominance.manual(fit.cor)
#'
#' #.............
#' # Example 3: Latent and manifest variables
#'
#' # Model specification, convert manifest to latent variable
#' model <- '# Measurement model
#'           ind60 =~ x1 + x2 + x3
#'           dem60 =~ y1 + y2 + y3 + y4
#'           # Manifest as latent variable
#'           ly5 =~ 1*y5
#'           y5 ~~ 0*y5
#'           # Regressions
#'           ind60 ~ dem60 + ly5'
#'
#' # Model estimation
#' fit <- sem(model, data = PoliticalDemocracy)
#'
#' # Model-implied correlation matrix of the latent variables
#' fit.cor <- lavInspect(fit, what = "cor.lv")
#'
#' # Dominance analysis
#' dominance.manual(fit.cor)
#'
#' #----------------------------------------------------------------------------
#' # Example 4: Multilevel modeling
#'
#' # Model specification
#' model <- 'level: 1
#'             fw =~ y1 + y2 + y3
#'             # Manifest as latent variables
#'             lx1 =~ 1*x1
#'             lx2 =~ 1*x2
#'             lx3 =~ 1*x3
#'             x1 ~~ 0*x1
#'             x2 ~~ 0*x2
#'             x3 ~~ 0*x3
#'             # Regression
#'             fw ~ lx1 + lx2 + lx3
#'           level: 2
#'             fb =~ y1 + y2 + y3
#'             # Manifest as latent variables
#'             lw1 =~ 1*w1
#'             lw2 =~ 1*w2
#'             # Regression
#'             fb ~ lw1 + lw2'
#'
#' # Model estimation
#' fit <- sem(model, data = Demo.twolevel, cluster = "cluster")
#'
#' # Model-implied correlation matrix of the latent variables
#' fit.cor <- lavInspect(fit, what = "cor.lv")
#'
#' # Dominance analysis Within
#' dominance.manual(fit.cor$within)
#'
#' # Dominance analysis Between
#' dominance.manual(fit.cor$cluster)
#'
#' \dontrun{
#' #----------------------------------------------------------------------------
#' # Example 5: Mplus
#' #
#' # In Mplus, the model-implied correlation matrix of the latent variables
#' # can be requested by OUTPUT: TECH4 and imported into R by using the
#' # MplusAuomtation package, for example:
#'
#' library(MplusAutomation)
#'
#' # Read Mplus output
#' output <- readModels()
#'
#' # Extract model-implied correlation matrix of the latent variables
#' fit.cor <- output$tech4$latCorEst
#' }
dominance.manual <- function(x, out = NULL, digits = 3, write = NULL, append = TRUE,
                             check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'x' is missing or NULL
  if (isTRUE(missing(x) || is.null(x))) { stop("Please specify a matrix or data frame for the argument 'x'.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("append", "output"), character = list(out = 1L), args = c("digits", "write2"), envir = environment(), input.check = check)

  # Additional checks
  if (isTRUE(check)) {

    # Symmetric matrix or data frame for the argument 'x'
    if (isTRUE(ncol(x) != nrow(x) || any(abs(x) > 1L) || any(diag(x) != 1L))) { stop("Please specifiy a correlation matrix for the argument 'x'.", call. = FALSE) }

    # Missing values in the argument 'x'
    if (isTRUE(any(is.na(x[lower.tri(x)])) && any(is.na(x[upper.tri(x)])))) { stop("Missing values are not allowed in the correlation matrix specified in 'x'.", call. = FALSE) }

    # Column names 'x'
    if (is.null(colnames(x))) { stop("There are no column names in the correlation matrix specified in 'x'.", call. = FALSE) }

    ## Check input 'out' ##
    if (isTRUE(!is.null(out) & !out %in% colnames(x))) { stop("Variable name specified in the argument 'out' was not found in 'x'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Data and Arguments ---------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Correlation matrix ####

  # Lower triangular missing
  if (all(is.na(x[lower.tri(x)]))) { x[lower.tri(x)] <- t(x)[lower.tri(x)] }

  # Upper triangular missing
  if (all(is.na(x[upper.tri(x)]))) { x[upper.tri(x)] <- t(x)[upper.tri(x)] }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Outcome ####

  if (isTRUE(!is.null(out))) {

    x <- x[c(which(colnames(x) == out), which(colnames(x) != out)), c(which(colnames(x) == out), which(colnames(x) != out))]

  } else {

    out <- colnames(x)[1L]

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Dominance analysis ####

  domin.res <- .DA(x)

  #_____________________________________________________________________________
  #
  # Return object --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## General dominance ####

  gen.res <- data.frame(r2 = domin.res, perc = domin.res / sum(domin.res)  * 100, rank = rank(-domin.res)) |>
    (\(y) rbind(y, c(sum(y[, "r2"]), sum(y[, "perc"]), NA)))()

  row.names(gen.res) <- c(colnames(x)[-1L], "Total")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Return object ####

  object <- list(call = match.call(),
                 type = "dominance.manual",
                 x = x,
                 args = list(out = out, digits = digits, write = write, append = append, check = check, output = output),
                 result = gen.res)

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
