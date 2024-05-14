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
#'               \code{"Output.txt"}) or Excel file with file extention
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
#' \code{\link{dominance}}, \code{\link{std.coef}}, \code{\link{write.result}}
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
#' \dontrun{
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
#' #----------------------------------------------------------------------------
#' # Example 5: Mplus
#' #
#' # In Mplus, the model-impied correlation matrix of the latent variables
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

  # Check if input 'x' is missing
  if (isTRUE(missing(x))) { stop("Input for the argument 'x' is missing.", call. = FALSE) }

  # Check if input 'model' is NULL
  if (isTRUE(is.null(x))) { stop("Input specified for the argument 'x' is NULL.", call. = FALSE) }

  # Matrix or data frame for the argument 'x'
  if (isTRUE(!is.matrix(x) && !is.data.frame(x))) { stop("Please specifiy a matrix or data frame for the argument 'x'.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # Symmetric matrix or data frame for the argument 'x'
    if (isTRUE(ncol(x) != nrow(x) || any(abs(x) > 1L) || any(diag(x) != 1L))) { stop("Please specifiy a correlation matrix for the argument 'x'.", call. = FALSE) }

    # Missing values in the argument 'x'
    if (isTRUE(any(is.na(x[lower.tri(x)])) && any(is.na(x[upper.tri(x)])))) { stop("Missing values are not allowed in the correlation matrix specified in 'x'.", call. = FALSE) }

    # Column names 'x'
    if (is.null(colnames(x))) { stop("There are no column names in the correlation matrix specified in 'x'.", call. = FALSE) }

    ## Check input 'out' ##
    if (isTRUE(!is.null(out) & !out %in% colnames(x))) { stop("Variable name specified in the argument 'out' was not found in 'x'.", call. = FALSE) }

    ## Check input 'digits' ##
    if (isTRUE(digits %% 1L != 0L || digits < 0L || digits == 0L)) { stop("Specify a positive integer number for the argument 'digits'.", call. = FALSE) }

    # Check input 'append'
    if (isTRUE(!is.logical(append))) { stop("Please specify TRUE or FALSE for the argument 'append'.", call. = FALSE) }

    ## Check input 'output' ##
    if (isTRUE(!is.logical(output))) { stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE) }

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

    x <- x[c(which(colnames(x) == out), which(colnames(x) != out)),
           c(which(colnames(x) == out), which(colnames(x) != out))]

  } else {

    out <- colnames(x)[1L]

  }

  #_____________________________________________________________________________
  #
  # Internal Functions ---------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Enumerate the Combinations or Permutation of the ELements of a Vector ####

  # combinations() from the gtools package
  combinations <- function(n, r, v = 1:n, set = TRUE, repeats.allowed = FALSE) {

    if (isTRUE(mode(n) != "numeric" || length(n) != 1L || n < 1L || (n %% 1) != 0L)) { stop("bad value of n") }
    if (isTRUE(mode(r) != "numeric" || length(r) != 1L || r < 1L || (r %% 1) != 0L)) { stop("bad value of r") }

    if (isTRUE(!is.atomic(v) || length(v) < n)) { stop("v is either non-atomic or too short") }

    if (isTRUE((r > n) & !repeats.allowed)) { stop("r > n and repeats.allowed = FALSE", call. = FALSE) }

    if (isTRUE(set)) {

      v <- unique(sort(v))
      if (length(v) < n) stop("Too few different elements", call. = FALSE)

    }

    v0 <- vector(mode(v), 0L)

    ## Inner workhorse
    if (repeats.allowed) {

      sub <- function(n, r, v) {

        if (isTRUE(r == 0L)) { v0 } else if (isTRUE(r == 1L)) { matrix(v, n, 1) } else if (isTRUE(n == 1L)) { matrix(v, 1L, r) } else { rbind(cbind(v[1L], Recall(n, r - 1L, v)), Recall(n - 1L, r, v[-1L])) }

      }

    } else {

      sub <- function(n, r, v) {

        if (isTRUE(r == 0L)) { v0 } else if (isTRUE(r == 1L)) { matrix(v, n, 1) } else if (isTRUE(r == n)) { matrix(v, 1L, n) } else { rbind(cbind(v[1], Recall(n - 1L, r - 1L, v[-1L])), Recall(n - 1L, r, v[-1L])) }

      }

      return(sub(n, r, v[1L:n]))

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Dominance analysis functions ####

  DA <- function(cormat, index = NULL) {

    # Correlation matrix of the predictors
    Px <- cormat[-1L, -1L]

    # Correlation vector
    rx <- cormat[-1L, 1L]

    if (isTRUE(is.null(index))) { index = as.list(1:length(rx)) }

    # Number of predictors or groups of predictors
    J <- length(index)

    # R2 for model with subset xi
    R2 <- function(xi) {

      xi <- unlist(index[xi])

      R2 <- t(rx[xi])%*%solve(Px[xi, xi])%*%rx[xi]

    }

    # Average R2 change for a subset model with k size

    # Possible subset models before adding a predictor
    submodel <- function(k) {

      temp0 <- lapply(1L:J, function(i) combinations(J - 1L, k, (1L:J)[-i]))

      # Possible subset models after adding a predictor
      temp1 <- lapply(1:J, function(i) t(apply(temp0[[i]], 1L, function(x) c(x, i))))

      # R2 before adding a predictor
      R0 <- lapply(temp0, function(y) apply(y, 1L, R2))

      # R2 after adding a predictor
      R1 <- lapply(temp1, function(y) apply(y, 1L, R2))

      # R2 change
      deltaR2 <- mapply(function(x, y) x - y, R1, R0)

      # Average R2 change
      adeltaR2 <- apply(matrix(deltaR2, ncol = J), 2L, mean)

      return(adeltaR2)

    }

    # Different model size k
    R2matrix <- t(sapply(1L:(J - 1L), submodel))

    R2matrix <- rbind(sapply(1L:J, R2), R2matrix)

    # Overall average
    DA <- apply(R2matrix, 2L, mean)

    return(DA)

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Dominance analysis ####

  domin.res <- DA(x)

  #_____________________________________________________________________________
  #
  # Return object --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## General dominance ####

  gen.res <- data.frame(r2 = domin.res,
                        perc = domin.res / sum(domin.res)  * 100,
                        rank = rank(-domin.res))

  gen.res <- rbind(gen.res, c(sum(gen.res[, "r2"]), sum(gen.res[, "perc"]), NA))
  row.names(gen.res) <- c(colnames(x)[-1L], "Total")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Return object ####

  object <- list(call = match.call(),
                 type = "dominance.manual",
                 x = x,
                 args = list(out = out, digits = digits, write = write,
                             append = append, check = check, output = output),
                 result = gen.res)

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Write Results --------------------------------------------------------------


  if (isTRUE(!is.null(write))) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Text file ####

    if (isTRUE(grepl("\\.txt", write))) {

      # Send R output to textfile
      sink(file = write, append = ifelse(isTRUE(file.exists(write)), append, FALSE), type = "output", split = FALSE)

      if (append && isTRUE(file.exists(write))) { write("", file = write, append = TRUE) }

      # Print object
      print(object, check = FALSE)

      # Close file connection
      sink()

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ## Excel file ####

    } else {

      misty::write.result(object, file = write)

    }

  }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
