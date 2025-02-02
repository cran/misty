#' Cluster Scores
#'
#' This function computes group means by default.
#'
#' @param ...     a numeric vector for computing cluster scores for a variable,
#'                matrix or data frame for computing cluster scores for more than
#'                one variable. Alternatively, an expression indicating the variable
#'                names in \code{data} e.g., \code{ci.mean(x1, x2, data = dat)}.
#'                Note that the operators \code{.}, \code{+}, \code{-}, \code{~},
#'                \code{:}, \code{::}, and \code{!} can also be used to select
#'                variables, see 'Details' in the \code{\link{df.subset}} function.
#' @param data    a data frame when specifying one or more variables in the
#'                argument \code{...}. Note that the argument is \code{NULL}
#'                when specifying a numeric vector, matrix, or data frame for
#'                the argument \code{...}.
#' @param cluster either a character string indicating the variable name of
#'                the cluster variable in \code{...} or \code{data}, or a
#'                vector representing the nested grouping structure (i.e.,
#'                group or cluster variable).
#' @param fun     character string indicating the function used to compute group
#'                scores, default: \code{"mean"}.
#' @param expand  logical: if \code{TRUE} (default), vector of cluster scores is expanded
#'                to match the input vector \code{x}.
#' @param append  logical: if \code{TRUE} (default), cluster scores are appended
#'                to the data frame specified in the argument \code{data}.
#' @param name    a character string or character vector indicating the names
#'                of the computed variables. By default, variables are named with
#'                the ending \code{".a"} resulting in e.g. \code{"x1.a"} and
#'                \code{"x2.a"}. Variable names can also be specified using a
#'                character vector matching the number of variables specified in
#'                \code{x} (e.g., \code{name = c("cluster.x1", "cluster.x2")}).
#' @param as.na   a numeric vector indicating user-defined missing values, i.e.
#'                these values are converted to \code{NA} before conducting the
#'                analysis. Note that \code{as.na()} function is only applied to
#'                the argument \code{x}, but not to \code{cluster}.
#' @param check   logical: if \code{TRUE} (default), argument specification is checked.
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
#' # Load data set "Demo.twolevel" in the lavaan package
#' data("Demo.twolevel", package = "lavaan")
#'
#' # Example 1: Compute cluster means for 'y1' and expand to match the input 'y1'
#' cluster.scores(Demo.twolevel$y1, cluster = Demo.twolevel$cluster)
#'
#' # Alternative specification using the 'data' argument
#' cluster.scores(y1, data = Demo.twolevel, cluster = "cluster")
#'
#' # Example 2: Compute standard deviation for each cluster
#' # and expand to match the input x
#' cluster.scores(Demo.twolevel$y1, cluster = Demo.twolevel$cluster, fun = "sd")
#'
#' # Example 3: Compute cluster means without expanding the vector
#' cluster.scores(Demo.twolevel$y1, cluster = Demo.twolevel$cluster, expand = FALSE)
#'
#' # Example 4: Compute cluster means for 'y1' and 'y2' and append to 'Demo.twolevel'
#' cbind(Demo.twolevel,
#'       cluster.scores(Demo.twolevel[, c("y1", "y2")], cluster = Demo.twolevel$cluster))
#'
#' # Alternative specification using the 'data' argument
#' cluster.scores(y1, y2, data = Demo.twolevel, cluster = "cluster")
cluster.scores <- function(..., data = NULL, cluster,
                           fun = c("mean", "sum", "median", "var", "sd", "min", "max"),
                           expand = TRUE, append = TRUE, name = ".a", as.na = NULL,
                           check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input '...' is missing
  if (isTRUE(missing(...))) { stop("Please specify the argument '...'.", call. = FALSE) }

  # Check if input '...' is NULL
  if (isTRUE(is.null(substitute(...)))) { stop("Input specified for the argument '...' is NULL.", call. = FALSE) }

  # Check input 'cluster'
  if (isTRUE(missing(cluster))) { stop("Please specify a variable name or vector representing the grouping structure for the argument 'cluster'.", call. = FALSE) }

  # Check if input 'cluster' is NULL
  if (isTRUE(is.null(cluster))) { stop("Input specified for the argument 'cluster' is NULL.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data using the argument 'data' ####

  if (isTRUE(!is.null(data))) {

    # Convert tibble into data frame
    if (isTRUE("tbl" %in% substr(class(data), 1L, 3L))) { data <- as.data.frame(data) }

    # Variable names
    var.names <- .var.names(..., data = data, check.chr = "a numeric vector, matrix, or data frame")

    # Extract data
    x <- data[, var.names]

    # Cluster variable
    cluster <- data[, cluster]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data without using the argument 'data' ####

  } else {

    # Extract data
    x <- eval(..., enclos = parent.frame())

    # Convert tibble into data frame
    if (isTRUE("tbl" %in% substr(class(x), 1L, 3L))) { if (isTRUE(ncol(as.data.frame(x)) == 1L)) { x <- unlist(x) } else { x <- as.data.frame(x) } }

    # Data and cluster
    var.group <- .var.group(data = x, cluster = cluster)

    # Data
    if (isTRUE(!is.null(var.group$data))) { x <- var.group$data }

    # Cluster variable
    if (isTRUE(!is.null(var.group$cluster))) { cluster <- var.group$cluster }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Check input 'cluster' ####

  if (isTRUE(nrow(data.frame(cluster)) != nrow(data.frame(x)))) { stop("The length of the vector in 'cluster' does not match with the length of the vector in 'x'.", call. = FALSE) }

  # Check if only one variable specified in the input 'cluster'
  if (isTRUE(ncol(data.frame(cluster)) != 1L)) { stop("More than one variable specified for the argument 'cluster'.", call. = FALSE) }

  # Convert 'cluster' into a vector
  cluster <- unlist(cluster, use.names = FALSE)

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("expand", "append"),
               m.character = list(fun = c("mean", "sum", "median", "var", "sd", "min", "max")), envir = environment(), input.check = check)

  # Additional checks
  if (isTRUE(check)) {

    # Check input 'x'
    if (isTRUE(any(c(vapply(data.frame(x), mode, FUN.VALUE = character(1L)) != "numeric", vapply(data.frame(x), is.factor, FUN.VALUE = logical(1L)))))) {

      if (isTRUE(is.null(dim(x)))) {

        stop("Please specify a numeric vector for the argument 'x'.", call. = FALSE)

      } else {

        stop("Please specify a matrix or data frame with numeric vectors for the argument 'x'.", call. = FALSE)

      }

    }

    # Check input 'name'
    if (isTRUE(!is.null(dim(x)))) {

      if (isTRUE(!is.character(name))) { stop("Please specify a character string or vector for the argument 'name'.", call. = FALSE) }

      if (isTRUE(length(name) > 1L && length(name) != ncol(x))) {  stop("The length of the vector specified in 'name' does not match with the number of variable in 'x'.", call. = FALSE) }

    }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert matrix into data frame ####

  if (is.matrix(x)) { x <- data.frame(x) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Function for computing cluster scores ####

  fun <- ifelse(all(c("mean", "sum", "median", "var", "sd", "min", "max") %in% fun), "mean", fun)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) { x <- .as.na(x, na = as.na) }

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

      object <- data.frame(vapply(x, misty::cluster.scores, cluster = cluster, fun = fun, expand = expand, as.na = as.na, check = FALSE, FUN.VALUE = double(nrow(x))))

    } else {

      object <- data.frame(vapply(x, misty::cluster.scores, cluster = cluster, fun = fun, expand = expand, as.na = as.na, check = FALSE, FUN.VALUE = double(length(unique(cluster)))))

    }

    #...................
    ### Variable names ####

    if (isTRUE(length(name) == 1L)) {

      colnames(object) <- paste0(colnames(object), name)

    } else {

      colnames(object) <- name

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Append ####

  if (isTRUE(!is.null(data) && expand && append)) {

    if (isTRUE(is.null(dim(x)))) {

      #...................
      ### Variable names ####

      if (isTRUE(name == ".a")) {

        object <- setNames(as.data.frame(object), nm = paste0(var.names, ".a"))

      } else {

        object <- setNames(as.data.frame(object), nm = name)

      }

    }

    object <- data.frame(data, object)

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  return(object)

}

#_______________________________________________________________________________
