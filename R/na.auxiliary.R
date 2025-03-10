#' Auxiliary Variables Analysis
#'
#' This function computes (1) Pearson product-moment correlation matrix to identify
#' variables related to the incomplete variable (i.e., correlates of incomplete
#' variables), (2) Cohen's d matrix comparing cases with and without missing values
#' to identify variables related to the probability of missingness(i.e., correlates
#' of missingness), and (3) semi-partial correlations of an outcome variable
#' conditional on the predictor variables of a substantive model with a set of
#' candidate auxiliary variables to identify correlates of an incomplete outcome
#' variable as suggested by Raykov and West (2016).
#'
#' Note that non-numeric variables (i.e., factors, character vectors, and logical
#' vectors) are excluded from to the analysis.
#'
#' @param data      a data frame with incomplete data, where missing
#'                  values are coded as \code{NA}.
#' @param ...       an expression indicating the variable names in \code{data},
#'                  e.g., \code{na.auxiliary(dat, x1, x2, x3)}. Note that the
#'                  operators \code{.}, \code{+}, \code{-}, \code{~}, \code{:},
#'                  \code{::}, and \code{!} can also be used to select variables,
#'                  see 'Details' in the \code{\link{df.subset}} function.
#' @param model     a character string specifying the substantive model predicting
#'                  an continuous outcome variable using a set of predictor variables
#'                  to estimate semi-partial correlations between the outcome
#'                  variable and a set of candidate auxiliary variables. The default
#'                  setting is \code{model = NULL}, i.e., the function computes
#'                  Pearson product-moment correlation matrix and Cohen's d matrix.
#' @param estimator a character string indicating the estimator to be used
#'                  when estimating semi-partial correlation coefficients, i.e.,
#'                  \code{"ML"} for maximum likelihood parameter estimates with
#'                  conventional standard errors or \code{"MLR"} (default) maximum
#'                  likelihood parameter estimates with Huber-White robust standard
#'                  errors.
#' @param missing   a character string indicating how to deal with missing data
#'                  when estimating semi-partial correlation coefficients, i.e.,
#'                  \code{"fiml"} for full information maximum likelihood method,
#'                  \code{two.stage} for two-stage maximum likelihood method,
#'                  \code{robust.two.stage} for robust two-stage maximum likelihood
#'                  method, and \code{doubly-robust} for doubly-robust method (see
#'                  'Details' in the \code{\link{item.cfa}} function). The default
#'                  setting is \code{missing = "fiml"}.
#' @param tri       a character string indicating which triangular of the correlation
#'                  matrix to show on the console, i.e., \code{both} for upper and
#'                  lower triangular, \code{lower} (default) for the lower triangular,
#'                  and \code{upper} for the upper triangular.
#' @param weighted  logical: if \code{TRUE} (default), the weighted pooled standard
#'                  deviation is used.
#' @param correct   logical: if \code{TRUE}, correction factor for Cohen's d to
#'                  remove positive bias in small samples is used.
#' @param digits    integer value indicating the number of decimal places digits
#'                  to be used for displaying correlation coefficients and Cohen's
#'                  d estimates.
#' @param p.digits  an integer value indicating the number of decimal places
#'                  to be used for displaying the \emph{p}-value.
#' @param as.na     a numeric vector indicating user-defined missing values,
#'                  i.e. these values are converted to \code{NA} before conducting
#'                  the analysis.
#' @param write     a character string naming a file for writing the output into
#'                  either a text file with file extension \code{".txt"} (e.g.,
#'                  \code{"Output.txt"}) or Excel file with file extension
#'                  \code{".xlsx"}  (e.g., \code{"Output.xlsx"}). If the file
#'                  name does not contain any file extension, an Excel file will
#'                  be written.
#' @param append    logical: if \code{TRUE} (default), output will be appended
#'                  to an existing text file with extension \code{.txt} specified
#'                  in \code{write}, if \code{FALSE} existing text file will be
#'                  overwritten.
#' @param check     logical: if \code{TRUE} (default), argument specification is
#'                  checked.
#' @param output    logical: if \code{TRUE} (default), output is shown on the
#'                  console.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{as.na}}, \code{\link{na.as}}, \code{\link{na.coverage}},
#' \code{\link{na.descript}}, \code{\link{na.indicator}}, \code{\link{na.pattern}},
#' \code{\link{na.prop}}, \code{\link{na.test}}
#'
#' @references
#' Enders, C. K. (2010). \emph{Applied missing data analysis}. Guilford Press.
#'
#' Graham, J. W. (2009). Missing data analysis: Making it work in the real world.
#' \emph{Annual Review of Psychology, 60}, 549-576.
#' https://doi.org/10.1146/annurev.psych.58.110405.085530
#'
#' Raykov, T., & West, B. T. (2016). On enhancing plausibility of the missing at
#' random assumption in incomplete data analyses via evaluation of response-auxiliary
#' variable correlations. \emph{Structural Equation Modeling, 23}(1), 45–53.
#' https://doi.org/10.1080/10705511.2014.937848
#'
#' van Buuren, S. (2018). \emph{Flexible imputation of missing data} (2nd ed.).
#' Chapman & Hall.
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{data}}{data frame used for the current analysis}
#' \item{\code{model}}{lavaan model syntax for estimating the semi-partial correlations}
#' \item{\code{model.fit}}{fitted lavaan model for estimating the semi-partial correlations}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{result}}{list with result tables}
#'
#' @export
#'
#' @examples
#' # Example 1: Auxiliary variables
#' na.auxiliary(airquality)
#'
#' # Example 2: Semi-partial correlation coefficients
#' na.auxiliary(airquality, model = "Ozone ~ Solar.R + Wind")
#'
#' \dontrun{
#' # Example 3a: Write Results into a text file
#' na.auxiliary(airquality, write = "NA_Auxiliary.txt")
#'
#' # Example 3a: Write Results into an Excel file
#' na.auxiliary(airquality, write = "NA_Auxiliary.xlsx")
#' }
na.auxiliary <- function(data, ..., model = NULL, estimator = c("ML", "MLR"),
                         missing = c("fiml", "two.stage", "robust.two.stage", "doubly.robust"),
                         tri = c("both", "lower", "upper"), weighted = FALSE, correct = FALSE,
                         digits = 2, p.digits = 3, as.na = NULL, write = NULL, append = TRUE,
                         check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'data' is missing
  if (isTRUE(missing(data))) { stop("Please specify a data frame for the argument 'data'", call. = FALSE) }

  # Check if input 'data' is NULL
  if (isTRUE(is.null(data))) { stop("Input specified for the argument 'data' is NULL.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data using the argument 'data' ####

  if (isTRUE(!missing(...))) {

    # Extract data and convert tibble into data frame or vector
    x <- as.data.frame(data[, .var.names(..., data = data), drop = FALSE])

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data without using the argument 'data' ####

  } else {

    # Data frame
    x <- as.data.frame(data)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Exclude factors, character vectors and logical vectors ####

  x.num <- vapply(x, function(y) is.numeric(y), FUN.VALUE = logical(1L))

  if (isTRUE(any(!x.num))) {

    # Select numeric variables
    x <- x[, which(x.num)]

    warning(paste0("Non-numeric variables excluded from the analysis: ", paste(names(x.num)[!x.num], collapse = ", ")), call. = FALSE)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) { x <- .as.na(x, na = as.na) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("weighted", "correct", "append", "output"),
               s.character = list(estimator = c("ML", "MLR"), missing = c("fiml", "two.stage", "robust.two.stage", "doubly.robust"), tri = c("both", "lower", "upper")),
               args = c("digits", "p.digits", "write2"), envir = environment(), input.check = check)

  # Additional checks
  if (isTRUE(check)) {

    # Check input 'model'
    if (isTRUE(!is.null(model))) {

      # One outcome variable
      if (isTRUE(grepl("\n", model))) { stop("Please specify a substantive model with one outcome variable in the argument 'model'.", call. = FALSE) }

    } else {

      # No missing values
      if (isTRUE(all(!is.na(x)))) { stop("There are no missing values (NA) in the data frame specified in 'data'.", call. = FALSE) }

    }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'tri' argument ####

  tri <- ifelse(all(c("both", "lower", "upper") %in% tri), "lower", tri)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'estimator' argument ####

  estimator <- ifelse(all(c("ML", "MLR") %in% estimator), "MLR", estimator)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'missing' argument ####

  missing <- ifelse(all(c("fiml", "two.stage", "robust.two.stage", "doubly.robust") %in% missing), "fiml", missing)

  #_____________________________________________________________________________
  #
  # Main Function Product-Moment Correlation Matrix and Cohen's d Matrix -------

  if (isTRUE(is.null(model))) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Variables related to the incomplete variable ####

    cor.mat <- cor(x, use = "pairwise.complete.obs")

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Variables related to the probability of missingness ####

    # Indicator matrix
    ind <- misty::na.indicator(x, na = 0L, append = FALSE)
    colnames(ind) <- paste0(colnames(ind), "_ind")

    # Pairwise combinations
    x.combn <- combn(ncol(x), m = 2L)

    # Data
    x.ind <- data.frame(x, ind)

    #...................
    ### Cohen's d ####

    result.d.low <- result.d.upp <- numeric(ncol(x.combn))
    for (i in seq_len(ncol(x.combn))) {

      temp <- x.combn[, i]

      if (isTRUE(length(unique(x.ind[, colnames(ind)[temp[1L]]])) == 2L && all(tapply(x.ind[,  names(x)[temp[2L]]], x.ind[colnames(ind)[temp[1L]]], function(y) length(unique(na.omit(y)))) > 0L))) {

        result.d.upp[i] <- eval(parse(text = paste0(".cohens.d.na.auxiliary(", names(x)[temp[2L]], " ~ ", colnames(ind)[temp[1L]], ", data = x.ind, weighted = weighted, correct = correct)")))

      } else {

        result.d.upp[i] <- NA

      }

      if (isTRUE(length(unique(x.ind[, colnames(ind)[temp[2L]]])) == 2L && all(tapply(x.ind[,  names(x)[temp[1L]]], x.ind[colnames(ind)[temp[2L]]], function(y) length(unique(na.omit(y)))) > 0L))) {

        result.d.low[i] <- eval(parse(text = paste0(".cohens.d.na.auxiliary(", names(x)[temp[1L]], " ~ ", colnames(ind)[temp[2L]], ", data = x.ind, weighted = weighted, correct = correct)")))

      } else {

        result.d.low[i] <- NA

      }

    }

    #...................
    ### Cohen's d matrix ####

    d.mat <- matrix(NA, ncol = ncol(x), nrow = ncol(x), dimnames = list(names(x), names(x)))

    d.mat[rev(upper.tri(d.mat))] <- result.d.upp
    d.mat <- t(d.mat)

    d.mat[lower.tri(d.mat)] <- result.d.low

    # Remove empty rows
    d.mat <- d.mat[apply(d.mat, 1L, function(y) any(!is.na(y))), ]

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Return object ####

    object <- list(call = match.call(),
                   type = "na.auxiliary",
                   data = x,
                   model = NULL,
                   model.fit = NULL, model.fit.stand = NULL,
                   args = list(model = model, estimator = estimator, missing = missing, tri = tri, weighted = weighted, correct = correct, digits = digits, p.digits = p.digits, as.na = as.na, write = write, append = append, check = check, output = output),
                   result = list(cor = cor.mat, d = d.mat))

    class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Main Function Semi-Partial Correlations ------------------------------------

  } else {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Variables ####

    # As formula
    formula <- as.formula(model)

    # All variables
    var.formula <- all.vars(formula)

    # Check if model variables are available
    if (!is.null(data)) {

      which(!var.formula %in% colnames(data)) |>
        (\(z) if (isTRUE(length(z) != 0L)) {

          stop(paste0("Variables specified in the argument 'model' were not all found in 'data': ", paste(var.formula[z], collapse = ", ")), call. = FALSE)

        })()

    } else {

      which(!var.formula %in% colnames(x)) |>
        (\(z) if (isTRUE(length(z) != 0L)) {

          stop(paste0("Variables specified in the argument 'model' were not all found in 'data': ", paste(var.formula[z], collapse = ", ")), call. = FALSE)

        })()

    }

    # Predictor variables
    pred.var <- attr(terms(formula[-2L]), "term.labels")

    # Outcome
    y.var <- setdiff(var.formula, pred.var)

    if (isTRUE(length(y.var) != 1L)) { stop("Please specify a substantive model with one outcome variable in the argument 'model'.", call. = FALSE) }

    # All variable including candidate auxiliary variables
    var.all <- unique(c(y.var, pred.var, colnames(x)))

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Data ####

    if (!is.null(data)) { x <- data[, var.all] }

    if (isTRUE(all(!is.na(x[, y.var])))) { stop("There are no missing values (NA) in the outcome variable specified in the substantive model.", call. = FALSE) }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## lavaan ####

    #...................
    ### Model Specification ####

    mod.na <- NULL
    for (i in seq_along(var.all)[-length(var.all)]) {

      # Outcome variable
      if (isTRUE(var.all[i] != y.var)) {

        mod.na <- paste0(mod.na, paste(var.all[i], paste0(var.all[-seq_len(i)], collapse = " + "), sep = " ~~ "), sep = "\n")

      # Not outcome variable
      } else {

        mod.na <- paste0(mod.na, paste(var.all[i], paste0(setdiff(var.all[-seq_len(i)], pred.var), collapse = " + "), sep = " ~~ "), sep = "\n")

      }

    }

    # Paste substantive model
    mod.na <- paste0("# Substantive model\n", model, "\n# Auxiliary model\n", mod.na)

    #...................
    ### Model Estimation ####

    mod.na.fit <- suppressWarnings(lavaan::sem(mod.na, data = x, estimator = estimator, missing = missing, fixed.x = FALSE))

    # Standardized Solution
    mod.na.fit.stand <- lavaan::standardizedSolution(mod.na.fit)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Return object ####

    object <- list(call = match.call(),
                   type = "na.auxiliary",
                   data = x,
                   model = mod.na,
                   model.fit = mod.na.fit, model.fit.stand = mod.na.fit.stand,
                   args = list(model = model, estimator = estimator, missing = missing, tri = tri, weighted = weighted, correct = correct, digits = digits, p.digits = p.digits, as.na = as.na, write = write, append = append, check = check, output = output),
                   result = list(cor = NULL, d = NULL))

    class(object) <- "misty.object"

  }

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
