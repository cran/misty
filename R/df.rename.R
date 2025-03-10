#' Rename Columns in a Matrix or Variables in a Data Frame
#'
#' This function renames columns in a matrix or variables in a data frame by
#' (1) using \code{old_name = new_name}, by using the functions \code{toupper},
#' \code{tolower}, \code{sub}, and \code{gsub}, or (3) by specifying a character
#' vector indicating the column(s) or variable(s) to be renamed (argument \code{from})
#' and a character vector indicating the corresponding replacement values (argument
#' \code{to}).
#'
#' @param data  a matrix or data frame.
#' @param ...   \code{old_name = new_name} when \code{from = NULL} and \code{to = NULL},
#'              or one of the functions \code{toupper}, \code{tolower}, \code{sub},
#'              and \code{gsub}. Note that a tilde (\code{~}) needs to be specified
#'              before when using a function, e.g., \code{~toupper} or
#'              \code{~gsub("_", ".")}.
#' @param from  a character string or character vector indicating the column(s)
#'              or variable(s) to be renamed.
#' @param to    a character string or character vector indicating the corresponding
#'              replacement values for the column(s) or variable(s) specified in
#'              the argument \code{name}.
#' @param check logical: if \code{TRUE} (default), argument specification is checked.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{df.duplicated}}, \code{\link{df.merge}}, \code{\link{df.move}},
#' \code{\link{df.rbind}}, \code{\link{df.sort}}, \code{\link{df.subset}}
#'
#' @return
#' Returns the matrix or data frame \code{data} with renamed columns or variables.
#'
#' @export
#'
#' @examples
#' #----------------------------------------------------------------------------
#' # Rename using variable names
#'
#' # Example 1a: Rename 'cyl' in 'mtcars' to 'cylinder' using 'old_name = new_name'
#' df.rename(mtcars, cyl = cylinder)
#'
#' # Example 1b: Rename 'cyl' in 'mtcars' to 'cylinder' using 'from' and 'to'
#' df.rename(mtcars, from = "cyl", to = "cylinder")
#'
#' # Example 2a: Rename 'cyl' and 'wt' in 'mtcars' to 'cylinder' and 'weight'
#' # using 'old_name = new_name'
#' df.rename(mtcars, cyl = cylinder, wt = weight)
#'
#' # Example 2b: Rename 'cyl' and 'wt' in 'mtcars' to 'cylinder' and 'weight'
#' # using using 'from' and 'to'
#' df.rename(mtcars, from = c("cyl", "wt"), to = c("cylinder", "weight"))
#'
#' #----------------------------------------------------------------------------
#' # Rename using functions
#'
#' # Example 3: Convert all variable names to lowercase
#' df.rename(iris, ~tolower)
#'
#' # Example 4: Replace all '.' with '_'
#' # Note, the argument fixed is set to TRUE by default.
#' df.rename(iris, ~gsub(".", "_"))
#'
#' # Example 5: Replace all 'S' with 'P'
#' df.rename(iris, ~gsub("S", "P"))
#'
#' # Example 6: Replace all 'S' with 'P', ignore case during matching
#' df.rename(iris, ~gsub("S", "P", ignore.case = TRUE))
df.rename <- function(data, ..., from = NULL, to = NULL, check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'data' is missing
  if (isTRUE(missing(data) || (!is.matrix(data) && !is.data.frame(data)))) { stop("Please specify a matrix or data frame for the argument 'data'.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Variables ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Rename variables using ... ####

  if (isTRUE(is.null(from) && is.null(to))) {

    # Extract argument '...'
    dots <- sapply(substitute(list(...)), as.character) |> (\(y) if (isTRUE(is.list(y))) { y[[2]] } else { misty::chr.omit(y, omit = "list", check = FALSE) })()

    #...................
    ### Function ####

    if (isTRUE(any(dots == "~"))) {

      # Upper case
      if (isTRUE(any(grepl("toupper", dots)))) {

        from <- colnames(data)
        to <- toupper(colnames(data))

      # Lower case
      } else if (isTRUE(any(grepl("tolower", dots)))) {

        from <- colnames(data)
        to <- tolower(colnames(data))

      # Pattern replacement, only first occurrence
      } else if (isTRUE(any(grepl("gsub", dots)))) {

        # Specify argument 'to'
        from <- colnames(data)

        # Extract arguments
        dots <- misty::chr.omit(misty::chr.trim(misty::chr.gsub(c("~", "gsub", "\\(", ")"), rep("", times = 4L), unlist(strsplit(dots, "\"")), check = FALSE), check = FALSE), omit = c("", ","), check = FALSE)

        # Arguments
        args <- misty::chr.grepl(c("ignore.case", "perl", "fixed", "useBytes"), dots, check = FALSE) |> (\(y) if (isTRUE(any(y))) { misty::chr.trim(unlist(strsplit(dots[y], ",")), check = FALSE) } else { NULL } )()

        # Specify argument 'to'
        to <- suppressWarnings(eval(parse(text = paste0("gsub(pattern = dots[1L], replacement = dots[2L], x = colnames(data)",
                                                        grepl("ignore.case", args) |> (\(y) if (isTRUE(any(y))) { paste0(", ", args[y]) } else { NULL })(),
                                                        grepl("perl", args) |> (\(y) if (isTRUE(any(y))) { paste0(", ", args[y]) } else { NULL })(),
                                                        grepl("fixed", args) |> (\(y) if (isTRUE(any(y))) { paste0(", ", args[y]) } else {

                                                          # Default setting fixed = TRUE when ignore.case = FALSE, perl = FALSE, and useBytes = FALSE
                                                          if (isTRUE(grepl("TRUE", args[grepl("ignore.case", args)]) || grepl("TRUE", args[grepl("perl ", args)]) || grepl("TRUE", args[grepl("useBytes ", args)]))) {

                                                            NULL

                                                          } else {

                                                            ", fixed = TRUE"

                                                          }

                                                        })(),
                                                        grepl("useBytes", args) |> (\(y) if (isTRUE(any(y))) {paste0(", ", args[y]) } else { NULL })(), ")"))))

      # Pattern replacement, all occurrences
      } else if (isTRUE(any(grepl("sub", dots)))) {

        # Specify argument 'to'
        from <- colnames(data)

        # Extract arguments
        dots <- misty::chr.omit(misty::chr.trim(misty::chr.gsub(c("~", "sub", "\\(", ")"), rep("", times = 4L), unlist(strsplit(dots, "\"")), check = FALSE), check = FALSE), omit = c("", ","), check = FALSE)

        # Arguments
        args <- misty::chr.grepl(c("ignore.case", "perl", "fixed", "useBytes"), dots, check = FALSE) |> (\(y) if (isTRUE(any(y))) { misty::chr.trim(unlist(strsplit(dots[y], ",")), check = FALSE) } else { NULL } )()

        # Specify argument 'to'
        to <- suppressWarnings(eval(parse(text = paste0("sub(pattern = dots[1L], replacement = dots[2L], x = colnames(data)",
                                                        grepl("ignore.case", args) |> (\(y) if (isTRUE(any(y))) { paste0(", ", args[y]) } else { NULL })(),
                                                        grepl("perl", args) |> (\(y) if (isTRUE(any(y))) { paste0(", ", args[y]) } else { NULL })(),
                                                        grepl("fixed", args) |> (\(y) if (isTRUE(any(y))) { paste0(", ", args[y]) } else {

                                                          # Default setting fixed = TRUE when ignore.case = FALSE, perl = FALSE, and useBytes = FALSE
                                                          if (isTRUE(grepl("TRUE", args[grepl("ignore.case", args)]) || grepl("TRUE", args[grepl("perl ", args)]) || grepl("TRUE", args[grepl("useBytes ", args)]))) {

                                                            NULL

                                                          } else {

                                                            ", fixed = TRUE"

                                                          }

                                                        })(),
                                                        grepl("useBytes", args) |> (\(y) if (isTRUE(any(y))) {paste0(", ", args[y]) } else { NULL })(), ")"))))

      } else {

        stop("Please specify one of the following functions 'toupper', 'tolower', 'sub', or 'toupper'.", call. = FALSE)

      }

    #...................
    ### No function ####

    } else {

      # Specify argument 'from'
      from <- names(dots)

      # Specify argument 'to'
      to <- unname(dots)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Rename variables using 'from' and 'to' ####

  } else {

    # Check if both arguments are specified
    if (isTRUE(is.null(from) || is.null(to))) { stop("Please specify the arguments 'from' and 'to'.", call. = FALSE) }

    # Check if ... or 'from'/'to' are specified
    if (isTRUE(any(sapply(substitute(list(...)), as.character) != "list"))) { stop("Please specify the argument '...' or the arguments 'from' and 'to'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  .check.input(envir = environment(), input.check = check)

  # Additional checks
  if (isTRUE(check)) {

    # Character string or vector for the argument 'from'?
    if (isTRUE(!is.character(from))) { stop("Please specify a character string or character vector for the argument 'from'.", call. = FALSE) }

    # Character string or vector for the argument 'to'?
    if (isTRUE(!is.character(to))) { stop("Please specify a character string or character vector for the argument 'to'.", call. = FALSE) }

    # Vector in argument 'from' matching with the vector in argument 'to'?
    if (isTRUE(length(from) != length(to))) { stop("Length of the vector specified in 'from' does not match with the vector specified in 'to'.", call. = FALSE) }

    # Variables specified in the argument 'from' in 'data'?
    if (isTRUE(any(!from %in% colnames(data)))) { from[which(!from %in% colnames(data))] |> (\(y) stop("Column name(s) specified in 'from' not found in the matrix or data frame: ", paste(y, collapse = ", "), call. = FALSE))() }

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  # Rename variables
  colnames(data)[match(from, colnames(data))] <- to

  # Duplicated columns from
  if (isTRUE(any(duplicated(colnames(data))))) { warning(paste0("Duplicated column names in the matrix or data frame after renaming columns: ", paste(unique(colnames(data)[duplicated(colnames(data))]), collapse = ", ")), call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  return(data)

}

#_______________________________________________________________________________
