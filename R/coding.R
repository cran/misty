#' Coding Categorical Variables
#'
#' This function creates \eqn{k - 1} variables for a categorical variable with
#' \eqn{k} distinct levels. The coding system available in this function are
#' dummy coding, simple coding, unweighted effect coding, weighted effect coding,
#' repeated coding, forward Helmert coding, reverse Helmert coding, and orthogonal
#' polynomial coding.
#'
#' @param ...    a numeric vector with integer values, character vector or factor
#'               Alternatively, an expression indicating the variable name in
#'               \code{data}. Note that the function can only deal with one
#'               categorical variable.
#' @param data   a data frame when specifying a variable in the argument \code{...}.
#'               Note that the argument is \code{NULL} when specifying a numeric
#'               vector with integer values, character vector or factor numeric
#'               vector for the argument \code{...}.
#' @param type   a character string indicating the type of coding, i.e.,
#'               \code{dummy} (default) for dummy coding, \code{simple} for
#'               simple coding, \code{effect} for unweighted effect coding,
#'               \code{weffect} for weighted effect coding, \code{repeat}
#'               for repeated coding, \code{fhelm} for forward Helmert coding,
#'               \code{rhelm} for reverse Helmert coding, and \code{poly} for
#'               orthogonal polynomial coding (see 'Details').
#' @param base   a numeric value or character string indicating the baseline
#'               group for dummy and simple coding and the omitted group in
#'               effect coding. By default, the first group or factor level is
#'               selected as baseline or omitted group.
#' @param name   a character string or character vector indicating the names
#'               of the coded variables. By default, variables are named
#'               \code{"dum."}, \code{"sim."}, \code{"eff."}, \code{"weff."},
#'               \code{"rep."}, \code{"fhelm."}, \code{"rhelm."},or \code{"poly."}
#'               depending on the \code{type} of coding with the category used
#'               in the comparison (e.g., \code{"dum.2"} and \code{"dum.3"}).
#'               Variable names can be specified using a character string (e.g.,
#'               \code{name = "dummy_"} leads to \code{dummy_2} and \code{dummy_3})
#'               or a character vector matching the number of coded variables
#'               (e.g. \code{name = c("x1_2", "x1_3")})  which is the number of
#'               unique categories minus one.
#' @param append logical: if \code{TRUE} (default), coded variables are appended
#'               to the data frame specified in the argument \code{data}.
#' @param as.na  a numeric vector indicating user-defined missing values,
#'               i.e. these values are converted to \code{NA} before conducting
#'               the analysis.
#' @param check  logical: if \code{TRUE} (default), argument specification is checked.
#'
#' @details
#' \describe{
#' \item{\strong{Dummy Coding}}{Dummy or treatment coding compares the mean of
#' each level of the categorical variable to the mean of a baseline group. By
#' default, the first group or factor level is selected as baseline group. The
#' intercept in the regression model represents the mean of the baseline group.
#' For example, dummy coding based on a  categorical variable with four groups
#' \code{A}, \code{B}, \code{C}, \code{D} makes following comparisons:
#' \code{B vs A}, \code{C vs A}, and \code{D vs A} with \code{A} being the
#' baseline group.}
#' \item{\strong{Simple Coding}}{Simple coding compares each level of the
#' categorical variable to the mean of a baseline level. By default, the first
#' group or factor level is selected as baseline group. The intercept in the
#' regression model represents the unweighted grand mean, i.e., mean of group
#' means. For example, simple coding based on a  categorical variable with four
#' groups \code{A}, \code{B}, \code{C}, \code{D} makes following comparisons:
#' \code{B vs A}, \code{C vs A}, and \code{D vs A} with \code{A} being the
#' baseline group.}
#' \item{\strong{Unweighted Effect Coding}}{Unweighted effect or sum coding
#' compares the mean of a given level to the unweighed grand mean, i.e., mean of
#' group means. By default, the first group or factor level is selected as
#' omitted group. For example, effect coding based on a  categorical variable
#' with four groups \code{A}, \code{B}, \code{C}, \code{D} makes following
#' comparisons: \code{B vs (A, B, C, D)}, \code{C vs (A, B, C, D)}, and
#' \code{D vs (A, B, C, D)} with \code{A} being the omitted group.}
#' \item{\strong{Weighted Effect Coding}}{Weighted effect or sum coding compares
#' the mean of a given level to the weighed grand mean, i.e., sample mean. By
#' default, the first group or factor level is selected as omitted group. For
#' example, effect coding based on a categorical variable with four groups
#' \code{A}, \code{B}, \code{C}, \code{D} makes following comparisons:
#' \code{B vs (A, B, C, D)}, \code{C vs (A, B, C, D)}, and \code{D vs (A, B, C, D)}
#' with \code{A} being the omitted group.}
#' \item{\strong{Repeated Coding}}{Repeated or difference coding compares the
#' mean of each level of the categorical variable to the mean of the previous
#' adjacent level. For example, repeated coding based on a  categorical variable
#' with four groups \code{A}, \code{B}, \code{C}, \code{D} makes following
#' comparisons: \code{B vs A}, \code{C vs B}, and \code{D vs C}.}
#' \item{\strong{Foward Helmert Coding}}{Forward Helmert coding compares the
#' mean of each level of the categorical variable to the unweighted mean of all
#' subsequent level(s) of the categorical variable. For example, forward Helmert
#' coding based on a  categorical variable with four groups \code{A}, \code{B},
#' \code{C}, \code{D} makes following comparisons: \code{(B, C, D) vs A},
#' \code{(C, D) vs B}, and \code{D vs C}.}
#' \item{\strong{Reverse Helmert Coding}}{Reverse Helmert coding compares the
#' mean of each level of the categorical variable to the unweighted mean of all
#' prior level(s) of the categorical variable. For example, reverse Helmert
#' coding based on a  categorical variable with four groups \code{A}, \code{B},
#' \code{C}, \code{D} makes following comparisons: \code{B vs A}, \code{C vs (A, B)},
#' and \code{D vs (A, B, C)}.}
#' \item{\strong{Orthogonal Polynomial Coding}}{Orthogonal polynomial coding is
#' a form of trend analysis based on polynomials of order \eqn{k - 1}, where
#' \eqn{k} is the number of levels of the categorical variable. This coding
#' scheme assumes an ordered-categorical variable with equally spaced levels.
#' For example, orthogonal polynomial coding based on a categorical variable with
#' four groups \code{A}, \code{B}, \code{C}, \code{D} investigates a linear,
#' quadratic, and cubic trends in the categorical variable.}
#' }
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{rec}}, \code{\link{item.reverse}}
#'
#' @return
#' Returns a data frame with \eqn{k - 1} coded variables or a data frame with the
#' same length or same number of rows as \code{...} containing the coded variables.
#'
#' @note
#' This function uses the \code{contr.treatment} function from the \pkg{stats}
#' package for dummy coding and simple coding, a modified copy of the
#' \code{contr.sum} function from the \pkg{stats} package for effect coding,
#' a modified copy of the \code{contr.wec} function from the \pkg{wec} package
#' for weighted effect coding, a modified copy of the \code{contr.sdif}
#' function from the \pkg{MASS} package for repeated coding, a modified copy
#' of the \code{code_helmert_forward} function from the \pkg{codingMatrices}
#' for forward Helmert coding, a modified copy of the \code{contr_code_helmert}
#' function from the \pkg{faux} package for reverse Helmert coding, and the
#' \code{contr.poly} function from the \pkg{stats} package for orthogonal
#' polynomial coding.
#'
#' @export
#'
#' @examples
#' # Example 1a: Dummy coding for 'gear', baseline group = 3
#' coding(gear, data = mtcars)
#'
#' # Example 1b: Alterantive specification without using the 'data' argument
#' coding(mtcars$gear)
#'
#' # Example 2: Dummy coding for 'gear', baseline group = 4
#' coding(gear, data = mtcars, base = 4)
#'
#' # Example 3: Effect coding for 'gear', omitted group = 3
#' coding(gear, data = mtcars, type = "effect")
#'
#' # Example 3: Effect coding for 'gear', omitted group = 4
#' coding(gear, data = mtcars, type = "effect", base = 4)
#'
#' # Example 4a: Dummy-coded variable names with prefix "gear3."
#' coding(gear, data = mtcars, name = "gear3.")
#'
#' # Example 4b: Dummy-coded variables named "gear_4vs3" and "gear_5vs3"
#' coding(gear, data = mtcars, name = c("gear_4vs3", "gear_5vs3"))
coding <- function(..., data = NULL,
                   type = c("dummy", "simple", "effect", "weffect", "repeat", "fhelm", "rhelm", "poly"),
                   base = NULL, name = c("dum.", "sim.", "eff.", "weff.", "rep.", "fhelm.", "rhelm.", "poly."),
                   append = TRUE, as.na = NULL, check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input '...' is missing
  if (isTRUE(missing(...))) { stop("Please specify the argument '...'.", call. = FALSE) }

  # Check if input '...' is NULL
  if (isTRUE(is.null(substitute(...)))) { stop("Input specified for the argument '...' is NULL.", call. = FALSE) }

  # Check if input 'data' is data frame
  if (isTRUE(!is.null(data) && !is.data.frame(data))) { stop("Please specify a data frame for the argument 'data'.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data using the argument 'data' ####

  if (isTRUE(!is.null(data))) {

    # Variable names
    var.names <- .var.names(..., data = data, check.chr = "a numeric vector with integer values, character vector or factor")

    # Extract variables
    x <- data[, var.names]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data without using the argument 'data' ####

  } else {

    # Extract data
    x <- eval(..., enclos = parent.frame())

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) { x <- .as.na(x, na = as.na) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # Input check 'x'
    if (isTRUE(ncol(as.data.frame(x)) != 1L)) { stop("Please specify one categorical variable for the argument '...'", call. = FALSE) }

    # Input check 'x'
    if (isTRUE(!is.factor(x) && !is.character(x))) { if (isTRUE(any(na.omit(x) %% 1L != 0L))) { stop("Please specify a vector with integer values, a character vector or a factor for the argument 'x'.", call. = FALSE) } }

    # Input check 'type'
    if (isTRUE(all(!type %in% c("dummy", "simple", "effect", "weffect", "repeat", "fhelm", "rhelm", "poly")))) { stop("Character string in the argument 'type' does not match with \"dummy\", \"simple\", \"effect\", \"weffect\", \"repeat\", \"fhelm\", \"rhelm\", or \"poly\".", call. = FALSE) }

    # Input check 'base'
    if (isTRUE(!is.null(base))) { if (isTRUE(!base %in% x)) { stop("The baseline category specified in 'base' was not found in 'x'.", call. = FALSE) } }

    # Input check 'name'
    if (isTRUE(!is.character(name))) { stop("Please specify a character string or character vector for the argument 'name'.", call. = FALSE) }

    # Input check 'names'
    if (isTRUE(!all(c("dum.", "sim.", "eff.", "weff.", "rep.", "fhelm.", "rhelm.", "poly.") %in% name))) {

      if (isTRUE(length(name) > 1L)) { if (isTRUE(length(name) != (length(na.omit(unique(x))) - 1L))) { stop("The length of the vector specified in 'name' does not match with the number of unique values minus one.", call. = FALSE) } }

    }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Type of Coding ####

  if (isTRUE(all(c("dummy", "simple", "effect", "weffect", "repeat", "fhelm", "rhelm", "poly") %in% type))) { type <- "dummy" }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Vector of Factor Levels or Unique Values ####

  # Factor
  if (isTRUE(is.factor(x))) {

    n <- levels(x)

  # Not a factor
  } else {

    n <- sort(na.omit(unique(x)))

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Baseline Group ####

  # User-defined group
  if (isTRUE(!is.null(base))) {

    n.base <- which(n %in% base)

  # Default first group
  } else {

    n.base <- 1L

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Contrast Matrix ####

  switch(type,
         dummy = {

           cont.mat <- contr.treatment(n, base = n.base)

         }, simple = {

           cont.mat <- contr.treatment(n, base = n.base) - 1L / length(n)

         }, effect = {

           cont.mat <- .contr.sum(n, omitted = n.base)

         }, weffect = {

           cont.mat <- .contr.wec(x, omitted = if (isTRUE(is.null(base))) { n[1] } else { base })

         }, "repeat" = {

           cont.mat <- .contr.repeat(n)

         }, fhelm =  {

           cont.mat <- .forward.helmert(n) * -1L

         }, rhelm =  {

           cont.mat <- .reverse.helmert(n)

         }, poly = {

           cont.mat <- contr.poly(n)

         })

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Match with Data ####

  object <- data.frame(cont.mat[match(x, n), ], row.names = NULL)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Variable names ####

  #...................
  ### Default names ####

  if (isTRUE(all(c("dum.", "sim.", "eff.", "weff.", "rep.", "fhelm.", "rhelm.", "poly.") %in% name))) {

    prefix <- switch(type, "dummy" = "dum.", "simple" = "sim.", "effect" = "eff.", "weffect" = "weff.", "repeat" = "rep.", "fhelm" = "fhelm.", "rhelm" = "rhelm.", "poly" = "poly")

  #...................
  ### User-defined prefix ####

  } else if (isTRUE(length(name) == 1L)) {

    prefix <- name

  } else {

    prefix <- NULL

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Assign names ####

  ##### Names with prefix
  if (isTRUE(!is.null(prefix))) {

    colnames(object) <- paste0(prefix, colnames(cont.mat))

  ##### User-defined vector of names
  } else {

    colnames(object) <- name

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Append ####

  if (isTRUE(!is.null(data) && append)) { object <- data.frame(data, object) }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  return(object)

}
