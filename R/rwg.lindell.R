#' Lindell, Brandt and Whitney (1999) r*wg(j) Within-Group Agreement Index for
#' Multi-Item Scales
#'
#' This function computes r*wg(j) within-group agreement index for multi-item
#' scales as described in Lindell, Brandt and Whitney (1999).
#'
#' The r*wg(j) index is calculated by dividing the mean of the item variance by
#' the expected random variance (i.e., null distribution). The default null
#' distribution in most research is the rectangular or uniform distribution
#' calculated with \eqn{\sigma^2_eu = (A^2 - 1) / 12}, where \eqn{A} is the number
#' of discrete response options of the items. However, what constitutes a reasonable
#' standard for random variance is highly debated. Note that the r*wg(j) allows
#' that the mean of the item variances to be larger than the expected random
#' variances, i.e., r*wg(j) values can be negative.
#'
#' Note that the \code{rwg.j.lindell()} function in the \pkg{multilevel} package
#' uses listwise deletion by default, while the \code{rwg.lindell()} function uses
#' all available information to compute the r*wg(j) agreement index by default.
#' In order to obtain equivalent results in the presence of missing values, listwise
#' deletion (\code{na.omit = TRUE}) needs to be applied.
#'
#' Examples for the application of r*wg(j) within-group agreement index for
#' multi-item scales can be found in Bardach et al. (2018), Bardach et al.
#' (2019a), and Bardach et al. (2019b).
#'
#' @param ...     a numeric vector or data frame. Alternatively, an expression
#'                indicating the variable names in \code{data} e.g.,
#'                \code{rwg.lindell(x1, x2, x3, data = dat)}. Note that the
#'                operators \code{.}, \code{+}, \code{-}, \code{~}, \code{:},
#'                \code{::}, and \code{!} can also be used to select variables,
#'                see 'Details' in the \code{\link{df.subset}} function.
#' @param data    a data frame when specifying one or more variables in the
#'                argument \code{...}. Note that the argument is \code{NULL}
#'                when specifying a numeric vector or data frame for the argument
#'                \code{...}.
#' @param cluster either a character string indicating the variable name of
#'                the cluster variable in \code{...} or \code{data}, or a
#'                vector representing the nested grouping structure (i.e.,
#'                group or cluster variable).
#' @param A       a numeric value indicating the number of discrete response
#'                options of the items from which the random variance is computed
#'                based on \eqn{(A^2 - 1) / 12}. Note that either the argument
#'                \code{j} or the argument\code{ranvar} is specified.
#' @param ranvar  a numeric value indicating the random variance to which the
#'                mean of the item variance is divided. Note that either the
#'                argument \code{j} or the argument\code{ranvar} is specified.
#' @param z       logical: if \code{TRUE} (default), Fisher z-transformation based
#'                on the formula \eqn{z = 0.5*log((1 + r) / (1 - r))} is applied
#'                to the vector of r*wg(j) estimates.
#' @param expand  logical: if \code{TRUE} (default), vector of r*wg(j) estimates
#'                is expanded to match the input vector \code{x}.
#' @param na.omit logical: if \code{TRUE}, incomplete cases are removed before
#'                conducting the analysis (i.e., listwise deletion).
#' @param append  logical: if \code{TRUE} (default), a variable with the r*wg(j)
#'                within-group agreement index are appended to the data frame
#'                specified in the argument \code{data}.
#' @param name    a character string indicating the name of the variable appended
#'                to the data frame specified in the argument \code{data} when
#'                \code{append = TRUE}. By default, the variable is named \code{rwg}.
#' @param as.na   a numeric vector indicating user-defined missing values,
#'                i.e. these values are converted to \code{NA} before conducting
#'                the analysis. Note that \code{as.na()} function is only applied
#'                to \code{x}, but not to \code{cluster}.
#' @param check   logical: if \code{TRUE} (default), argument specification is
#'                checked.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{cluster.scores}}
#'
#' @references
#' Bardach, L., Lueftenegger, M., Yanagida, T., & Schober, B. (2019a). Achievement
#' or agreement - Which comes first? Clarifying the temporal ordering of achievement
#' and within-class consensus on classroom goal structures. \emph{Learning and
#' Instruction, 61}, 72-83. https://doi.org/10.1016/j.learninstruc.2019.01.003
#'
#' Bardach, L., Lueftenegger, M., Yanagida, T., Schober, B. & Spiel, C. (2019b).
#' The role of within-class consensus on mastery goal structures in predicting
#' socio-emotional outcomes. \emph{British Journal of Educational Psychology, 89},
#' 239-258. https://doi.org/10.1111/bjep.12237
#'
#' Bardach, L., Yanagida, T., Schober, B. & Lueftenegger, M. (2018). Within-class
#' consensus on classroom goal structures: Relations to achievement and achievement
#' goals in mathematics and language classes. \emph{Learning and Individual
#' Differences, 67}, 78-90. https://doi.org/10.1016/j.lindif.2018.07.002
#'
#' Lindell, M. K., Brandt, C. J., & Whitney, D. J. (1999). A revised index of
#' interrater agreement for multi-item ratings of a single target. \emph{Applied
#' Psychological Measurement}, \emph{23}, 127-135. https://doi.org/10.1177/01466219922031257
#'
#' O'Neill, T. A. (2017). An overview of interrater agreement on Likert scales for
#' researchers and practitioners. \emph{Frontiers in Psychology}, \emph{8}, Article
#' 777. https://doi.org/10.3389/fpsyg.2017.00777
#'
#' @return
#' Returns a numeric vector containing r*wg(j) agreement index for multi-item scales
#' with the same length as \code{group} if \code{expand = TRUE} or a data frame with
#' following entries if \code{expand = FALSE}:
#' \item{\code{cluster}}{cluster identifier}
#' \item{\code{n}}{cluster size}
#' \item{\code{rwg.lindell}}{r*wg(j) estimate for each group}
#' \item{\code{z.rwg.lindell}}{Fisher z-transformed r*wg(j) estimate for each cluster}
#'
#' @export
#'
#' @examples
#' dat <- data.frame(id = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
#'                   cluster = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
#'                   x1 = c(2, 3, 2, 1, 1, 2, 4, 3, 5),
#'                   x2 = c(3, 2, NA, 1, 2, 1, 3, 2, 5),
#'                   x3 = c(3, 1, 1, 2, 3, 3, 5, 5, 4))
#'
#' # Example 1: Compute Fisher z-transformed r*wg(j) for a multi-item scale with A = 5 response options
#' rwg.lindell(dat[, c("x1", "x2", "x3")], cluster = dat$cluster, A = 5)
#'
#' # Alternative specification using the 'data' argument,
#' rwg.lindell(x1:x3, data = dat, cluster = "cluster", A = 5)
#'
#' # Example 2: Compute Fisher z-transformed r*wg(j) for a multi-item scale with a random variance of 2
#' rwg.lindell(dat[, c("x1", "x2", "x3")], cluster = dat$cluster, ranvar = 2)
#'
#' # Example 3: Compute r*wg(j) for a multi-item scale with A = 5 response options
#' rwg.lindell(dat[, c("x1", "x2", "x3")], cluster = dat$cluster, A = 5, z = FALSE)
#'
#' # Example 4: Compute Fisher z-transformed r*wg(j) for a multi-item scale with A = 5 response options,
#' # do not expand the vector
#' rwg.lindell(dat[, c("x1", "x2", "x3")], cluster = dat$cluster, A = 5, expand = FALSE)
rwg.lindell <- function(..., data = NULL, cluster, A = NULL, ranvar = NULL, z = TRUE,
                        expand = TRUE, na.omit = FALSE, append = TRUE, name = "rwg",
                        as.na = NULL, check = TRUE) {

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

  # Check if input 'data' is a data frame
  if (isTRUE(!is.null(data) && !is.data.frame(data))) { stop("Please specify a data frame for the argument 'data'.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data using the argument 'data' ####

  if (isTRUE(!is.null(data))) {

    # Convert tibble into data frame
    if (isTRUE("tbl" %in% substr(class(data), 1L, 3L))) { data <- as.data.frame(data) }

    # Extract data
    x <- data[, .var.names(..., data = data, cluster = cluster, check.chr = "a matrix or data frame")]

    # Cluster variable
    cluster <- data[, cluster]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data without using the argument 'data' ####

  } else {

    # Extract data
    x <- eval(..., enclos = parent.frame())

    # Convert tibble into data frame
    if (isTRUE("tbl" %in% substr(class(x), 1L, 3L))) { if (isTRUE(ncol(as.data.frame(x)) == 1L)) { x <- unlist(x) } else { x <- data.frame(x) } }
    if (isTRUE("tbl" %in% substr(class(cluster), 1L, 3L))) { if (isTRUE(ncol(as.data.frame(cluster)) == 1L)) { cluster <- unlist(cluster) } else { cluster <- data.frame(cluster) } }

    # Data and cluster
    var.group <- .var.group(data = x, cluster = cluster)

    # Data
    if (isTRUE(!is.null(var.group$data))) { x <- var.group$data }

    # Cluster variable
    if (isTRUE(!is.null(var.group$cluster))) { cluster <- var.group$cluster }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) { x <- .as.na(x, na = as.na) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data frame ####

  x <- data.frame(x, cluster = cluster)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Listwise deletion ####

  if (isTRUE(any(is.na(x)) && na.omit)) { assign("x", na.omit(x)) |> (\(y) warning(paste("Listwise deletion of incomplete data, number of cases removed from the analysis:", length(attributes(y)$na.action)), call. = FALSE))() }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("z", "expand"), numeric = list(A = 1L, ranvar = 1L), character = list(name = 1L), envir = environment(), input.check = check)

  # Additional checks
  if (isTRUE(check)) {

    # Check input 'A'
    if (isTRUE(!is.null(A))) {

      # Check input 'x': Number of discrete responses
      if (isTRUE(length(na.omit(unique(unlist(x)))) > A)) { warning("There are more unique values in 'x' than the number of discrete response options specified in 'A'.", call. = FALSE) }

      # Check input 'x': Integer number
      if (isTRUE(A %% 1L != 0L || A < 0L)) { stop("Please specify a positive integer number for the argument 'A'.", call. = FALSE) }

    }

    # Check input 'A' and 'ranvar'
    if (isTRUE((is.null(A) && is.null(ranvar)) || (!is.null(A) && !is.null(ranvar)))) { stop("Please specify the argument 'A' or the argument 'ranvar'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Data and Arguments ---------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Random variance based on A ####

  if (isTRUE(!is.null(A))) { ranvar <- (A^2L - 1L) / 12L }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Listwise deletion ####

  if (isTRUE(any(is.na(x)) && na.omit)) { assign("x", na.omit(x)) |> (\(y) warning(paste("Listwise deletion of incomplete data, number of cases removed from the analysis:", length(attributes(y)$na.action)), call. = FALSE))() }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  x.split <- split(x[, -grep("cluster", names(x))], x$cluster)

  rwg <- misty::as.na(vapply(x.split, function(y) 1L - (mean(vapply(y, var, na.rm = TRUE, FUN.VALUE = double(1L)), na.rm = TRUE) / ranvar), FUN.VALUE = double(1L)), na = NaN, check = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Expand ####

  if (isTRUE(expand)) {

    object <- rwg[match(x$cluster, names(rwg))]

    #......
    # Fisher z-transformation
    if (isTRUE(z)) {

      object <- ifelse(object == 1L | object == -1L, NA, atanh(object))

    }

  } else {

    object <- data.frame(cluster = names(rwg),
                         n = vapply(x.split, function(y) sum(apply(y, 1, function(z) sum(is.na(z)) != length(z))), FUN.VALUE = 1L),
                         rwg.lindell = rwg, z.rwg.lindell = ifelse(rwg == 1L | rwg == -1L, NA, atanh(rwg)))

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Append ####

  if (isTRUE(!is.null(data) && expand && append)) { object <- data.frame(data, setNames(as.data.frame(object), nm = name)) }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  return(object)

}

#_______________________________________________________________________________
