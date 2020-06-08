#' Group Scores
#'
#' This function computes group means by default.
#'
#' @param x           a numeric vector.
#' @param group       a integer vector, character vector, or factor representing the grouping structure (i.e., group variable).
#' @param fun         character string indicating the function used to compute group scores, default: \code{"mean"}.
#' @param expand      logical: if \code{TRUE}, vector of group scores is expanded to match the input vector \code{x}.
#' @param as.na       a numeric vector indicating user-defined missing values,
#'                    i.e. these values are converted to \code{NA} before conducting the analysis.
#'                    Note that \code{as.na()} function is only applied to the argument \code{x},
#'                    but not to \code{group}.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{scores}}, \code{\link{multilevel.descript}}, \code{\link{multilevel.icc}}
#'
#' @references
#' Hox, J., Moerbeek, M., & van de Schoot, R. (2018). \emph{Multilevel analysis: Techniques and applications} (3rd. ed.).
#' Routledge.
#'
#' Snijders, T. A. B., & Bosker, R. J. (2012). \emph{Multilevel analysis: An introduction to basic and advanced multilevel
#' modeling} (2nd ed.). Sage Publishers.
#'
#' @return
#' Returns a numeric vector containing group scores with the same length as \code{x} if \code{expand = TRUE}
#' or with the length \code{length(unique(group))} if \code{expand = FALSE}.
#'
#' @export
#'
#' @examples
#' dat.ml <- data.frame(id = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
#'                      group = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
#'                      x = c(4, 2, 5, 6, 3, 4, 1, 3, 4))
#'
#' # Compute group means and expand to match the input x
#' group.scores(dat.ml$x, group = dat.ml$group)
#'
#' # Compute standard deviation for each group and expand to match the input x
#' group.scores(dat.ml$x, group = dat.ml$group, fun = "sd")
#'
#' # Compute group means without expanding the vector
#' group.scores(dat.ml$x, group = dat.ml$group, expand = FALSE)
group.scores <- function(x, group, fun = c("mean", "sum", "median", "var", "sd", "min", "max"),
                         expand = TRUE, as.na = NULL, check = TRUE) {

  ####################################################################################
  # Input Check

  #......
  # Check if input 'x' is missing
  if (missing(x)) {

    stop("Please specify a numeric vector for the argument 'x'.", call. = FALSE)

  }

  #......
  # Check if input 'x' is NULL
  if (is.null(x)) {

    stop("Input specified for the argument 'x' is NULL.", call. = FALSE)

  }

  #......
  # Numeric vector for 'x'?
  if (!is.atomic(x) || !is.numeric(x)) {

    stop("Please specify a numeric vector for the argument 'x'.", call. = FALSE)

  }

  #......
  # Check input 'group'
  if (missing(group)) {

    stop("Please specify a vector representing the grouping structure for the argument 'group'.", call. = FALSE)

  }

  #......
  # Check input 'check'
  if (!is.logical(check)) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }

  #----------------------------------------

  if (isTRUE(check)) {

    #......
    # Vector for 'group'?
    if (!is.atomic(group) && !is.factor(group) && !is.factor(character)) {

      stop("Please specify an integer vector, character vector, or factor for the argument 'group'.",
           call. = FALSE)

    }

    #......
    # Length of vector and group?
    if (length(x) != length(group)) {

      stop("Length of the vector specified in 'x' does not match with the length of the grouping variable specified in 'group'.",
           call. = FALSE)

    }

    #......
    # Check input 'fun'
    if (!all(fun %in%  c("mean", "sum", "median", "var", "sd", "min", "max"))) {

      stop("Character strings in the argument 'fun' dos not match with \"mean\", \"sum\", \"median\", \"var\", \"sd\", \"min\", or \"max\".",
           call. = FALSE)

    }

    #......
    # Check input 'expand'
    if (!is.logical(expand)) {

      stop("Please specify TRUE or FALSE for the argument 'expand'.", call. = FALSE)

    }

  }

  ####################################################################################
  # Data and Arguments

  #----------------------------------------
  # Function for computing cluster scores

  fun <- ifelse(all(c("mean", "sum", "median", "var", "sd", "min", "max") %in% fun), "mean", fun)

  #----------------------------------------
  # Convert user-missing values into NA
  if (!is.null(as.na)) {

    x <- misty::as.na(x, as.na = as.na, check = check)

    #......
    # Missing values only
    if (all(is.na(x))) {

      stop("After converting user-missing values into NA, vector specified in 'x' is completely missing.",
           call. = FALSE)

    }

  }

  ####################################################################################
  # Main Function

  #----------------------------------------
  # Compute group scores

  if (fun != "sum") {

    agg.scores <- suppressWarnings(eval(parse(text = paste0("tapply(x, INDEX = group, FUN = ", fun, ", na.rm = TRUE)"))))

  # fun = "sum"
  } else {

    agg.scores <- tapply(x, group, function(y) ifelse(all(is.na(y)), NA, sum(y, na.rm = TRUE)))

  }

  # Convert NaN and Inf to NA
  if (fun %in% c("min", "max")) {

    agg.scores <- ifelse(is.infinite(agg.scores), NA, agg.scores)

  } else {

    agg.scores <- ifelse(is.nan(agg.scores), NA, agg.scores)

  }

  #----------------------------------------
  # Expand
  if (isTRUE(expand)) {

    object <- as.vector(agg.scores[match(group, names(agg.scores))])

  } else {

    object <- agg.scores

  }

  ####################################################################################
  # Return object

  return(object)

}
