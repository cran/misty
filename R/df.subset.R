#' Subsetting Data Frames
#'
#' This function returns subsets of data frames which meet conditions.
#'
#' @param data   a data frame.
#' @param ...    an expression indicating variables to select from the data frame
#'               specified in \code{data}. See Details for the list of operators
#'               used in this function, i.e., \code{+}, \code{-}, \code{~}, \code{:},
#'               \code{::}, and \code{!}. Note that all variables are selected if
#'               the argument \code{...} is not specified.
#' @param subset a logical expression indicating rows to keep, e.g., \code{var == 1},
#'               \code{var1 == 1 & var2 == 3}, or \code{gender == "female"}. By default,
#'               all rows of the data frame specified in \code{data} are kept. Note
#'               that logical queries for rows resulting in missing values are
#'               not select.
#' @param drop   logical: if \code{TRUE} (default), data frame with a single
#'               column is converted into a vector.
#' @param check  logical: if \code{TRUE} (default), argument specification is
#'               checked.
#'
#' @details
#' The argument \code{...} is used to specify an expression indicating the
#' variables to select and/or remove from the data frame specified in \code{data}.
#' There are six operators which can be used in the expression \code{...}:
#' \describe{
#' \item{\strong{Plus (\code{+}) Operator}}{The plus operator is used to select
#' variables matching a prefix from the data frame specified in \code{data}. For
#' example, \code{df.subset(dat, +x)} selects all variables with the
#' prefix \code{x}. Note that this operator is equivalent to the function
#' \code{starts_with()} from the \pkg{tidyselect} package.}
#' \item{\strong{Minus (\code{-}) Operator}}{The minus operator is used to select
#' variables matching a suffix from the data frame specified in \code{data}. For
#' example, \code{df.subset(dat, -y)} selects all variables with the
#' suffix \code{y}. Note that this operator is equivalent to the function
#' \code{ends_with()} from the \pkg{tidyselect} package.}
#' \item{\strong{Tilde (\code{~}) Operator}}{The tilde operator is used to select
#' variables containing a word from the data frame specified in \code{data}. For
#' example, \code{df.subset(dat, ~al)} selects all variables with the word
#' \code{al}. Note that this operator is equivalent to the function
#' \code{contains()} from the \pkg{tidyselect} package.}
#' \item{\strong{Colon (\code{:}) operator}}{The colon operator is used to select
#' a range of consecutive variables from the data frame specified in \code{data}.
#' For example, \code{df.subset(dat, x:z)} selects all variables from
#' \code{x} to \code{z}. Note that this operator is equivalent to the \code{:}
#' operator from the \code{select} function in the \pkg{dplyr} package.}
#' \item{\strong{Double Colon (\code{::}) Operator}}{The double colon operator
#' is used to select numbered variables from the data frame specified in
#' \code{data}. For example, \code{df.subset(dat, x1::x3)} selects the
#' variables \code{x1}, \code{x2}, and \code{x3}. Note that this operator is
#' similar to the function \code{num_range()} from the \pkg{tidyselect}
#' package.}
#' \item{\strong{Exclamation Point (\code{!}) Operator}}{The exclamation point
#' operator is used to drop variables from the data frame specified in the argument
#' \code{data} or for taking the complement of a set of variables. For example,
#' \code{df.subset(dat, !x)} selects all variables except the variable \code{x},
#' \code{df.subset(dat, !~x)} selects all variables except variables with the
#' prefix \code{x}, or \code{df.subset(dat, x1:x10, !x3:x5)} selects all variables
#' from \code{x1} to \code{x10} but excludes all variables from \code{x3} to
#' \code{x5}. Note that this operator is equivalent to the \code{!} operator from
#' the \code{select} function in the \pkg{dplyr} package.}}
#' Operators can be combined within the same function call. For example,
#' \code{df.subset(dat, +x, -y, !x2:x4, z)} selects all variables with the prefix
#' \code{x} and with the suffix \code{y} but excludes variables from \code{x2} to
#' \code{x4} and select variable \code{z}.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{df.check}}, \code{\link{df.duplicated}}, \code{\link{df.unique}},
#' \code{\link{df.head}}, \code{\link{df.tail}}, \code{\link{df.long}},
#' \code{\link{df.wide}}, \code{\link{df.merge}}, \code{\link{df.move}},
#' \code{\link{df.rbind}}, \code{\link{df.rename}}, \code{\link{df.sort}},
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
#' #----------------------------------------------------------------------------
#' # Select single variables
#'
#' # Example 1: Select 'Sepal.Length' and 'Petal.Width'
#' df.subset(iris, Sepal.Length, Petal.Width)
#'
#' #----------------------------------------------------------------------------
#' # Select rows
#'
#' # Example 2a: Select all variables, select rows with 'Species' equal 'setosa'
#' df.subset(iris, subset = Species == "setosa")
#'
#' # Example 2b: Select all variables, select rows with 'Petal.Length' smaller 1.2
#' df.subset(iris, subset = Petal.Length < 1.2)
#'
#' #----------------------------------------------------------------------------
#' # Select variables matching a prefix using the + operator
#'
#' # Example 3: Select variables with prefix 'Petal'
#' df.subset(iris, +Petal)
#'
#' #----------------------------------------------------------------------------
#' # Select variables matching a suffix using the - operator
#'
#' # Example 4: Select variables with suffix 'Width'
#' df.subset(iris, -Width)
#'
#' #----------------------------------------------------------------------------
#' # Select variables containing a word using the ~ operator
#
#' # Example 5: Select variables containing 'al'
#' df.subset(iris, ~al)
#'
#' #----------------------------------------------------------------------------
#' # Select consecutive variables using the : operator
#'
#' # Example 6: Select all variables from 'Sepal.Width' to 'Petal.Width'
#' df.subset(iris, Sepal.Width:Petal.Width)
#'
#' #----------------------------------------------------------------------------
#' # Select numbered variables using the :: operator
#'
#' # Example 7: Select all variables from 'x1' to 'x3' and 'y1' to 'y3'
#' df.subset(anscombe, x1::x3, y1::y3)
#
#' #----------------------------------------------------------------------------
#' # Drop variables using the ! operator
#'
#' # Example 8a: Select all variables except 'Sepal.Width'
#' df.subset(iris, !Sepal.Width)
#'
#' # Example 8b: Select all variables except variables with prefix 'Petal'
#' df.subset(iris, !+Petal)
#'
#' # Example 8c: Select all variables except variables with suffix 'Width'
#' df.subset(iris, !-Width)
#'
#' # Example 8d: Select all variables except 'Sepal.Width' to 'Petal.Width'
#' df.subset(iris, !Sepal.Width:Petal.Width)
#'
#' #----------------------------------------------------------------------------
#' # Combine +, -, !, and : operators
#'
#' # Example 9: Select variables with prefix 'x' and suffix '3', but exclude
#' # variables from 'x2' to 'x3'
#' df.subset(anscombe, +x, -3, !x2:x3)
df.subset <- function(data, ..., subset = NULL, drop = TRUE, check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'data' is missing
  if (isTRUE(missing(data) ||is.null(data))) { stop("Please specify a data frame for the argument 'data'", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  .check.input(logical = "drop", envir = environment(), input.check = check)

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Extract Variables ####

  if (isTRUE(missing(...))) { object <- data } else { object <- data[, .var.names(data = data, ...), drop = FALSE] }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Extract rows ####

  if (isTRUE(!is.null(substitute(subset)))) { object <- object[which(eval(substitute(subset), envir = object, enclos = parent.frame())), , drop = FALSE] }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert single column to a vector ####

  if (isTRUE(ncol(object) == 1L && drop)) { object <- unname(unlist(object)) }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  return(object)

}

#_______________________________________________________________________________
