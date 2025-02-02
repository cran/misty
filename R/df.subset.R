#' Subsetting Data Frames
#'
#' This function returns subsets of data frames which meet conditions.
#'
#' @param ...    an expression indicating variables to select from the data frame
#'               specified in \code{data}. See Details for the list of operators
#'               used in this function, i.e., \code{.}, \code{+}, \code{-},
#'               \code{~}, \code{:}, \code{::}, and \code{!}.
#' @param data   a data frame that contains the variables specified in the
#'               argument \code{...}. Note that if \code{data = NULL}, only the
#'               variables specified in \code{...} are returned.
#' @param subset character string with a logical expression indicating rows to
#'               keep, e.g., \code{"x == 1"}, \code{"x1 == 1 & x2 == 3"}, or
#'               \code{"gender == 'female'"}. By default, all rows of the data
#'               frame specified in \code{data} are kept. Note that logical
#'               queries for rows resulting in missing values are not select.
#' @param drop   logical: if \code{TRUE} (default), data frame with a single
#'               column is converted into a vector.
#' @param check  logical: if \code{TRUE} (default), argument specification is
#'               checked.
#'
#' @details
#' The argument \code{...} is used to specify an expression indicating the
#' variables to select from the data frame specified in \code{data}, e.g.,
#' \code{df.subset(x1, x2, x3, data = dat)}. There are seven operators which
#' can be used in the expression \code{...}:
#' \describe{
#' \item{\strong{Dot (\code{.}) Operator}}{The dot operator is used to select
#' all variables from the data frame specified in \code{data}. For example,
#' \code{df.subset(., data = dat)} selects all variables in \code{dat}. Note
#' that this operator is similar to the function \code{everything()} from the
#' \pkg{tidyselect} package.}
#' \item{\strong{Plus (\code{+}) Operator}}{The plus operator is used to select
#' variables matching a prefix from the data frame specified in \code{data}. For
#' example, \code{df.subset(+x, data = dat)} selects all variables with the
#' prefix \code{x}. Note that this operator is equivalent to the function
#' \code{starts_with()} from the \pkg{tidyselect} package.}
#' \item{\strong{Minus (\code{-}) Operator}}{The minus operator is used to select
#' variables matching a suffix from the data frame specified in \code{data}. For
#' example, \code{df.subset(-y, data = dat)} selects all variables with the
#' suffix \code{y}. Note that this operator is equivalent to the function
#' \code{ends_with()} from the \pkg{tidyselect} package.}
#' \item{\strong{Tilde (\code{~}) Operator}}{The tilde operator is used to select
#' variables containing a word from the data frame specified in \code{data}. For
#' example, \code{df.subset(?al, data = dat)} selects all variables with the word
#' \code{al}. Note that this operator is equivalent to the function
#' \code{contains()} from the \pkg{tidyselect} package.}
#' \item{\strong{Colon (\code{:}) operator}}{The colon operator is used to select
#' a range of consecutive variables from the data frame specified in \code{data}.
#' For example, \code{df.subset(x:z, data = dat)} selects all variables from
#' \code{x} to \code{z}. Note that this operator is equivalent to the \code{:}
#' operator from the \code{select} function in the \pkg{dplyr} package.}
#' \item{\strong{Double Colon (\code{::}) Operator}}{The double colon operator
#' is used to select numbered variables from the data frame specified in
#' \code{data}. For example, \code{df.subset(x1::x3, data = dat)} selects the
#' variables \code{x1}, \code{x2}, and \code{x3}. Note that this operator is
#' similar to the function \code{num_range()} from the \pkg{tidyselect}
#' package.}
#' \item{\strong{Exclamation Point (\code{!}) Operator}}{The exclamation point
#' operator is used to drop variables from the data frame specified in \code{data}
#' or for taking the complement of a set of variables. For example,
#' \code{df.subset(., !x, data = dat)} selects all variables but \code{x} in
#' \code{dat}., \code{df.subset(., !~x, data = dat)} selects all variables but
#' variables with the prefix \code{x}, or \code{df.subset(x:z, !x1:x3, data = dat)}
#' selects all variables from \code{x} to \code{z} but excludes all variables
#' from \code{x1} to \code{x3}. Note that this operator is equivalent to the \code{!}
#' operator from the \code{select} function in the \pkg{dplyr} package.}}
#' Note that operators can be combined within the same function call. For example,
#' \code{df.subset(+x, -y, !x2:x4, z, data = dat)} selects all variables with
#' the prefix \code{x} and with the suffix \code{y} but excludes variables from
#' \code{x2} to \code{x4} and select variable \code{z}.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{df.duplicated}}, \code{\link{df.merge}}, \code{\link{df.move}},
#' \code{\link{df.rbind}}, \code{\link{df.rename}}, \code{\link{df.sort}}
#'
#' @references
#' Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) \emph{The New S Language}.
#' Wadsworth & Brooks/Cole.
#'
#' @return
#' Returns a data frame containing the variables and rows selected in the argument
#' \code{...} and rows selected in the argument \code{subset}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #----------------------------------------------------------------------------
#' # Select single variables
#'
#' # Example 1: Select 'Sepal.Length' and 'Petal.Width'
#' df.subset(Sepal.Length, Petal.Width, data = iris)
#'
#' #----------------------------------------------------------------------------
#' # Select all variables using the . operator
#'
#' # Example 2a: Select all variables, select rows with 'Species' equal 'setosa'
#' # Note that single quotation marks ('') are needed to specify 'setosa'
#' df.subset(., data = iris, subset = "Species == 'setosa'")
#'
#' # Example 2b: Select all variables, select rows with 'Petal.Length' smaller 1.2
#' df.subset(., data = iris, subset = "Petal.Length < 1.2")
#'
#' #----------------------------------------------------------------------------
#' # Select variables matching a prefix using the + operator
#'
#' # Example 3: Select variables with prefix 'Petal'
#' df.subset(+Petal, data = iris)
#'
#' #----------------------------------------------------------------------------
#' # Select variables matching a suffix using the - operator
#'
#' # Example 4: Select variables with suffix 'Width'
#' df.subset(-Width, data = iris)
#'
#' #----------------------------------------------------------------------------
#' # Select variables containing a word using the ~ operator
#
#' # Example 5: Select variables containing 'al'
#' df.subset(~al, data = iris)
#'
#' #----------------------------------------------------------------------------
#' # Select consecutive variables using the : operator
#'
#' # Example 6: Select all variables from 'Sepal.Width' to 'Petal.Width'
#' df.subset(Sepal.Width:Petal.Width, data = iris)
#'
#' #----------------------------------------------------------------------------
#' # Select numbered variables using the :: operator
#'
#' # Example 7: Select all variables from 'x1' to 'x3' and 'y1' to 'y3'
#' df.subset(x1::x3, y1::y3, data = anscombe)
#
#' #----------------------------------------------------------------------------
#' # Drop variables using the ! operator
#'
#' # Example 8a: Select all variables but 'Sepal.Width'
#' df.subset(., !Sepal.Width, data = iris)
#'
#' # Example 8b: Select all variables but 'Sepal.Width' to 'Petal.Width'
#' df.subset(., !Sepal.Width:Petal.Width, data = iris)
#'
#' #----------------------------------------------------------------------------
#' # Combine +, -, !, and : operators
#'
#' # Example 9: Select variables with prefix 'x' and suffix '3', but exclude
#' # variables from 'x2' to 'x3'
#' df.subset(+x, -3, !x2:x3, data = anscombe)
#' }
df.subset <- function(..., data, subset = NULL, drop = TRUE, check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input '...' is missing
  if (isTRUE(missing(...))) { stop("Please specify the argument '...'.", call. = FALSE) }

  # Check if input '...' is NULL
  if (isTRUE(is.null(substitute(...)))) { stop("Input specified for the argument '...' is NULL.", call. = FALSE) }

  # Check if input 'data' is missing
  if (isTRUE(missing(data))) { stop("Please specify a numeric vector for the argument 'data'", call. = FALSE) }

  # Check if input 'data' is NULL
  if (isTRUE(is.null(data))) { stop("Input specified for the argument 'data' is NULL.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  .check.input(logical = "drop", character = list(subset = 1L), envir = environment(), input.check = check)

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Extract Variables ####

  # Variable names
  var.names <- .var.names(..., data = data, check.chr = "a data frame")

  # Extract variables
  object <- data[, var.names, drop = FALSE]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Extract rows ####

  if (isTRUE(!is.null(subset))) {

    object <- tryCatch(subset(object, subset = eval(parse(text = subset))),
                       error = function(y) {

                         if (isTRUE(grepl("==|!=|%in%", subset))) {

                           stop("Logical expression in 'subset' is syntactically not correct. Single quotation marks might be missing, see Example 2a.", call. = FALSE)

                         } else {

                           stop("Logical expression in 'subset' is syntactically not correct.", call. = FALSE)

                         }

                       })

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert single column to a vector ####

  if (isTRUE(ncol(object) == 1L && drop)) { object <- unname(unlist(object)) }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  return(object)

}

#_______________________________________________________________________________
