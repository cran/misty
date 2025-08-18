#' Cluster Scores
#'
#' This function computes group means by default.
#'
#' @param data    a numeric vector for centering a predictor variable, or a
#'                data frame for centering more than one predictor variable.
#' @param ...     an expression indicating the variable names in \code{data} e.g.,
#'                \code{cluster.scores(dat, x1, x2, cluster = "cluster")}. Note
#'                that the operators \code{+}, \code{-}, \code{~}, \code{:},
#'                \code{::}, and \code{!} can also be used to select variables,
#'                see 'Details' in the \code{\link{df.subset}} function.
#' @param cluster a character string indicating the variable name of the cluster
#'                variable in \code{data}, or a vector representing the nested
#'                grouping structure (i.e., group or cluster variable).
#' @param fun     character string indicating the function used to compute group
#'                scores, default: \code{"mean"}.
#' @param expand  logical: if \code{TRUE} (default), vector of cluster scores is
#'                expanded to match the input vector \code{data}.
#' @param append  logical: if \code{TRUE} (default), cluster scores are appended
#'                to the data frame specified in the argument \code{data}.
#' @param name    a character string or character vector indicating the names
#'                of the computed variables. By default, variables are named with
#'                the ending \code{".a"} resulting in e.g. \code{"x1.a"} and
#'                \code{"x2.a"}. Variable names can also be specified using a
#'                character vector matching the number of variables specified in
#'                \code{data} (e.g., \code{name = c("cluster.x1", "cluster.x2")}).
#' @param as.na   a numeric vector indicating user-defined missing values, i.e.
#'                these values are converted to \code{NA} before conducting the
#'                analysis. Note that \code{as.na()} function is only applied to
#'                the argument \code{data}, but not to \code{cluster}.
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
#' length or same number of rows as \code{data} if \code{expand = TRUE} or with the
#' length or number of rows as \code{length(unique(cluster))} if \code{expand = FALSE}.
#'
#' @export
#'
#' @examples
#' # Load data set "Demo.twolevel" in the lavaan package
#' data("Demo.twolevel", package = "lavaan")
#'
#' # Example 1: Compute cluster means for 'y1' and expand to match the input 'y1'
#' cluster.scores(Demo.twolevel, y1, cluster = "cluster", append = FALSE)
#'
#' # Alternative specification without using the '...' argument
#' cluster.scores(Demo.twolevel$y1, cluster = Demo.twolevel$cluster)
#'
#' # Example 2: Compute standard deviation for each cluster
#' # and expand to match the input x
#' cluster.scores(Demo.twolevel, cluster = "cluster", fun = "sd")
#'
#' # Example 3: Compute cluster means without expanding the vector
#' cluster.scores(Demo.twolevel, cluster = "cluster", expand = FALSE)
#'
#' # Example 4: Compute cluster means for 'y1' and 'y2' and append to 'Demo.twolevel'
#' cluster.scores(Demo.twolevel, y1, y2, cluster = "cluster")
#'
#' # Alternative specification without using the '...' argument
#' cbind(Demo.twolevel,
#'       cluster.scores(Demo.twolevel[, c("y1", "y2")], cluster = Demo.twolevel$cluster))
cluster.scores <- function(data, ..., cluster,
                           fun = c("mean", "sum", "median", "var", "sd", "min", "max"),
                           expand = TRUE, append = TRUE, name = ".a", as.na = NULL,
                           check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'data' is missing
  if (isTRUE(missing(data))) { stop("Please specify a numeric vector or data frame for the argument 'data'", call. = FALSE) }

  # Check if input 'data' is NULL
  if (isTRUE(is.null(data))) { stop("Input specified for the argument 'data' is NULL.", call. = FALSE) }

  # Check input 'cluster'
  if (isTRUE(missing(cluster))) { stop("Please specify a variable name or vector representing the grouping structure for the argument 'cluster'.", call. = FALSE) }

  # Check if input 'cluster' is NULL
  if (isTRUE(is.null(cluster))) { stop("Input specified for the argument 'cluster' is NULL.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data using the argument '...' ####

  if (isTRUE(!missing(...))) {

    # Variable names
    var.names <- .var.names(data = data, ..., cluster = cluster)

    # Extract data and convert tibble into data frame or vector
    x <- data[, var.names] |> (\(y) if (isTRUE("tbl" %in% substr(class(y), 1L, 3L))) { if (isTRUE(ncol(as.data.frame(y)) == 1L)) { unname(unlist(y)) } else { as.data.frame(y) } } else { y })()

    # Cluster variable
    cluster <- data[, cluster]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data without using the argument '...' ####

  } else {

    # Convert 'data' as tibble into data frame
    x <- data |> (\(y) if (isTRUE("tbl" %in% substr(class(y), 1L, 3L))) { if (isTRUE(ncol(as.data.frame(y)) == 1L)) { unname(unlist(y)) } else { as.data.frame(y) } } else { y })()

    # Data and cluster
    var.group <- .var.group(data = x, cluster = cluster)

    # Data
    if (isTRUE(!is.null(var.group$data))) { x <- var.group$data }

    # Cluster variable
    if (isTRUE(!is.null(var.group$cluster))) { cluster <- var.group$cluster }

  }

  # Convert 'cluster' as tibble into data frame
  if (isTRUE(!is.null(cluster) && "tbl" %in% substr(class(cluster), 1L, 3L))) { if (isTRUE(ncol(as.data.frame(cluster)) == 1L)) { cluster <- unname(unlist(cluster)) } else { cluster <- as.data.frame(cluster) } }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) { x <- .as.na(x, na = as.na) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  .check.input(logical = c("expand", "append"), m.character = list(fun = c("mean", "sum", "median", "var", "sd", "min", "max")), envir = environment(), input.check = check)

  # Additional checks
  if (isTRUE(check)) {

    # Check input 'data'
    if (isTRUE(any(c(vapply(data.frame(x), mode, FUN.VALUE = character(1L)) != "numeric", vapply(data.frame(x), is.factor, FUN.VALUE = logical(1L)))))) {

      if (isTRUE(is.null(dim(x)))) {

        stop("Please specify a numeric vector for the argument 'data'.", call. = FALSE)

      } else {

        stop("Please specify a data frame with numeric vectors for the argument 'data'.", call. = FALSE)

      }

    }

    # Check if only one variable specified in the input 'cluster'
    if (isTRUE(ncol(data.frame(cluster)) != 1L)) { stop("Please specify one cluster variable for the argument 'cluster'.", call. = FALSE) }

    # Check input length of 'data' and 'cluster'
    if (isTRUE(nrow(data.frame(cluster)) != nrow(data.frame(x)))) { stop("The length of the vector in 'cluster' does not match with the length of the vector in 'data'.", call. = FALSE) }

    # Check input 'name'
    if (isTRUE(!is.null(dim(x)))) {

      if (isTRUE(!is.character(name))) { stop("Please specify a character string or vector for the argument 'name'.", call. = FALSE) }

      if (isTRUE(length(name) > 1L && length(name) != ncol(x))) {  stop("The length of the vector specified in 'name' does not match with the number of variable in 'data'.", call. = FALSE) }

    }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Function for computing cluster scores ####

  fun <- ifelse(all(c("mean", "sum", "median", "var", "sd", "min", "max") %in% fun), "mean", fun)

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

  if (isTRUE(!missing(...) && expand && append)) {

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
