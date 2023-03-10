#' Cluster Scores
#'
#' This function is used to compute group means by default.
#'
#' @param x       a numeric vector for computing cluster scores for a variable,
#'                matrix or data frame for computing cluster scores for more than
#'                one variable.
#' @param cluster a vector representing the nested grouping structure (i.e., group
#'                or cluster variable).
#' @param fun     character string indicating the function used to compute group
#'                scores, default: \code{"mean"}.
#' @param expand  logical: if \code{TRUE}, vector of cluster scores is expanded
#'                to match the input vector \code{x}.
#' @param names   a character string or character vector indicating the names
#'                of the computed variables when specifying more than one variable.
#'                By default, variables are named with the ending \code{".a"}
#'                resulting in e.g. \code{"x1.a"} and \code{"x2.a"}. Variable names
#'                can also be specified using a character vector matching the number
#'                of variables specified in \code{x} (e.g.,
#'                \code{names = c("cluster.x1", "cluster.x2")}).
#' @param as.na   a numeric vector indicating user-defined missing values, i.e.
#'                these values are converted to \code{NA} before conducting the
#'                analysis. Note that \code{as.na()} function is only applied to
#'                the argument \code{x}, but not to \code{cluster}.
#' @param check   logical: if \code{TRUE}, argument specification is checked.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{item.scores}}, \code{\link{multilevel.descript}},
#' \code{\link{multilevel.icc}}
#'
#' @references
#' Hox, J., Moerbeek, M., & van de Schoot, R. (2018). \emph{Multilevel analysis:
#' Techniques and applications} (3rd. ed.). Routledge.
#'
#' Snijders, T. A. B., & Bosker, R. J. (2012). \emph{Multilevel analysis: An
#' introduction to basic and advanced multilevel modeling} (2nd ed.). Sage
#' Publishers.
#'
#' @return
#' Returns a numeric vector or data frame containing cluster scores with the same
#' length or same number of rows as \code{x} if \code{expand = TRUE} or with the
#' length or number of rows as \code{length(unique(cluster))} if \code{expand = FALSE}.
#'
#' @export
#'
#' @examples
#' dat.ml <- data.frame(id = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
#'                      cluster = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
#'                      x1 = c(4, 2, 5, 6, 3, 4, 1, 3, 4),
#'                      x2 = c(2, 5, 3, 1, 2, 7, 4, 5, 3))
#'
#' # Compute cluster means and expand to match the input x
#' cluster.scores(dat.ml$x1, cluster = dat.ml$cluster)
#'
#' # Compute standard deviation for each cluster and expand to match the input x
#' cluster.scores(dat.ml$x1, cluster = dat.ml$cluster, fun = "sd")
#'
#' # Compute cluster means without expanding the vector
#' cluster.scores(dat.ml$x1, cluster = dat.ml$cluster, expand = FALSE)
#'
#' # Compute cluster means and attach to 'dat.ml'
#' dat.ml <- cbind(dat.ml,
#'                 cluster.scores(dat.ml[, c("x1", "x2")], cluster = dat.ml$cluster))
cluster.scores <- function(x, cluster, fun = c("mean", "sum", "median", "var", "sd", "min", "max"),
                           expand = TRUE, names = ".a", as.na = NULL, check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'x' is missing
  if (isTRUE(missing(x))) { stop("Please specify a numeric vector for the argument 'x'.", call. = FALSE) }

  # Check if input 'x' is NULL
  if (isTRUE(is.null(x))) { stop("Input specified for the argument 'x' is NULL.", call. = FALSE) }

  # Convert 'x' into a vector when only one variable specified in 'x'
  if (isTRUE(ncol(data.frame(x)) == 1L)) { x <- unlist(x, use.names = FALSE) }

  #----------------------------------------

  # Check input 'cluster'
  if (isTRUE(missing(cluster))) { stop("Please specify a vector representing the grouping structure for the argument 'cluster'.", call. = FALSE) }

  # Check if input 'cluster' is NULL
  if (isTRUE(is.null(cluster))) { stop("Input specified for the argument 'cluster' is NULL.", call. = FALSE) }

  if (ncol(data.frame(cluster)) != 1L) { stop("More than one cluster variable specified for the argument 'cluster'.", call. = FALSE) }

  if (nrow(data.frame(cluster)) != nrow(data.frame(x))) { stop("Length of the cluster variable specified in the argument 'cluster' does not match with 'x'.", call. = FALSE) }

  # Convert 'cluster' into a vector
  cluster <- unlist(cluster, use.names = FALSE)

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  #----------------------------------------

  if (isTRUE(check)) {

    # Check input 'x'
    if (isTRUE(any(c(vapply(data.frame(x), mode, FUN.VALUE = character(1L)) != "numeric",
                     vapply(data.frame(x), is.factor, FUN.VALUE = logical(1L)))))) {

      if (isTRUE(is.null(dim(x)))) {

        stop("Please specify a numeric vector for the argument 'x'.", call. = FALSE)

      } else {

        stop("Please specify a matrix or data frame with numeric vectors for the argument 'x'.", call. = FALSE)

      }

    }

    # Check input 'fun'
    if (isTRUE(!all(fun %in%  c("mean", "sum", "median", "var", "sd", "min", "max")))) {

      stop("Character strings in the argument 'fun' dos not match with \"mean\", \"sum\", \"median\", \"var\", \"sd\", \"min\", or \"max\".", call. = FALSE)

    }

    # Check input 'expand'
    if (isTRUE(!is.logical(expand))) { stop("Please specify TRUE or FALSE for the argument 'expand'.", call. = FALSE) }

    # Check input 'names'
    if (isTRUE(!is.null(dim(x)))) {

      if (isTRUE(!is.character(names))) { stop("Please specify a character string or vector for the argument 'names'.", call. = FALSE) }

      if (isTRUE(length(names) > 1L && length(names) != ncol(x))) {  stop("The length of the vector specified in 'names' does not match with the number of variable in 'x'.", call. = FALSE) }

    }

  }

  #_____________________________________________________________________________
  #
  # Data and Arguments ---------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert matrix into data frame ####

  if (is.matrix(x)) { x <- data.frame(x) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Function for computing cluster scores ####

  fun <- ifelse(all(c("mean", "sum", "median", "var", "sd", "min", "max") %in% fun), "mean", fun)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) {

    x <- misty::as.na(x, na = as.na, check = check)

    # Missing values only
    if (isTRUE(all(is.na(x)))) {

      stop("After converting user-missing values into NA, vector specified in 'x' is completely missing.", call. = FALSE)

    }

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Single variable ####
  if (isTRUE(is.null(dim(x)))) {

    #...................
    ### Compute cluster scores ####

    if (isTRUE(fun != "sum")) {

      agg.scores <- suppressWarnings(eval(parse(text = paste0("tapply(x, INDEX = cluster, FUN = ", fun, ", na.rm = TRUE)"))))

    # fun = "sum"
    } else {

      agg.scores <- tapply(x, cluster, function(y) ifelse(all(is.na(y)), NA, sum(y, na.rm = TRUE)))

    }

    # Convert NaN and Inf to NA
    if (isTRUE(fun %in% c("min", "max"))) {

      agg.scores <- ifelse(is.infinite(agg.scores), NA, agg.scores)

    } else {

      agg.scores <- ifelse(is.nan(agg.scores), NA, agg.scores)

    }

    #----------------------------------------
    # Expand
    if (isTRUE(expand)) {

      object <- as.vector(agg.scores[match(cluster, names(agg.scores))])

    } else {

      object <- agg.scores

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Multiple variables ####
  } else {

    if (isTRUE(expand)) {

      object <- data.frame(vapply(x, misty::cluster.scores, cluster = cluster, fun = fun,
                                  expand = expand, as.na = as.na, check = FALSE,
                                  FUN.VALUE = double(nrow(x))))

    } else {

      object <- data.frame(vapply(x, misty::cluster.scores, cluster = cluster, fun = fun,
                                  expand = expand, as.na = as.na, check = FALSE,
                                  FUN.VALUE = double(length(unique(cluster)))))

    }

    #...................
    ### Variable names ####

    if (isTRUE(length(names) == 1L)) {

      colnames(object) <- paste0(colnames(object), names)

    } else {

      colnames(object) <- names

    }

  }

  ####################################################################################
  # Return object

  return(object)

}
