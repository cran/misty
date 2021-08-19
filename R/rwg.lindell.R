#' Lindell, Brandt and Whitney (1999) r*wg(j) Within-Group Agreement Index for
#' Multi-Item Scales
#'
#' This function computes r*wg(j) within-group agreement index for multi-item scales
#' as described in Lindell, Brandt and Whitney (1999).
#'
#' The r*wg(j) index is calculated by dividing the mean of the item variance by
#' the expected random variance (i.e., null distribution). The default null distribution
#' in most research is the rectangular or uniform distribution calculated with
#' \eqn{\sigma^2_eu = (A^2 - 1) / 12}, where \eqn{A} is the number of discrete response
#' options of the items. However, what constitutes a reasonable standard for random
#' variance is highly debated. Note that the r*wg(j) allows that the mean of the
#' item variances to be larger than the expected random variances, i.e., r*wg(j)
#' values can be negative.
#'
#' Note that the \code{rwg.j.lindell()} function in the \pkg{multilevel} package
#' uses listwise deletion by default, while the \code{rwg.lindell()} function uses
#' all available information to compute the r*wg(j) agreement index by default. In
#' order to obtain equivalent results in the presence of missing values, listwise
#' deletion (\code{na.omit = TRUE}) needs to be applied.
#'
#' Examples for the application of r*wg(j) within-group agreement index for multi-item
#' scales can be found in Bardach, Yanagida, Schober and Lueftenegger (2018),
#' Bardach, Lueftenegger, Yanagida, Schober and Spiel (2018), and Bardach, Lueftenegger,
#' Yanagida, Spiel and Schober (2019).
#'
#' @param x           a matrix or data frame with numeric vectors.
#' @param cluster     a vector representing the nested grouping structure (i.e.,
#'                    group or cluster variable).
#' @param A           a numeric value indicating the number of discrete response
#'                    options of the items from which the random variance is computed
#'                    based on \eqn{(A^2 - 1) / 12}. Note that either the argument
#'                    \code{j} or the argument\code{ranvar} is specified.
#' @param ranvar      a numeric value indicating the random variance to which the
#'                    mean of the item variance is divided. Note that either the
#'                    argument \code{j} or the argument\code{ranvar} is specified.
#' @param z           logical: if \code{TRUE}, Fisher z-transformation based on the
#'                    formula \eqn{z = 0.5*log((1 + r) / (1 - r))} is applied to
#'                    the vector of r*wg(j) estimates.
#' @param expand      logical: if \code{TRUE}, vector of r*wg(j) estimates is expanded
#'                    to match the input vector \code{x}.
#' @param na.omit     logical: if \code{TRUE}, incomplete cases are removed before
#'                    conducting the analysis (i.e., listwise deletion).
#' @param as.na       a numeric vector indicating user-defined missing values,
#'                    i.e. these values are converted to \code{NA} before conducting
#'                    the analysis. Note that \code{as.na()} function is only applied to
#'                    \code{x}, but not to \code{cluster}.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{cluster.scores}}
#'
#' @references
#' Bardach, L., Lueftenegger, M., Yanagida, T., & Schober, B. (2019). Achievement
#' or agreement - Which comes first? Clarifying the temporal ordering of achievement
#' and within-class consensus on classroom goal structures. \emph{Learning and Instruction,
#' 61}, 72-83. https://doi.org/10.1016/j.learninstruc.2019.01.003
#'
#' Bardach, L., Lueftenegger, M., Yanagida, T., Schober, B. & Spiel, C. (2019).
#' The role of within-class consensus on mastery goal structures in predicting
#' socio-emotional outcomes. \emph{British Journal of Educational Psychology, 89},
#' 239-258. https://doi.org/10.1111/bjep.12237
#'
#' Bardach, L., Yanagida, T., Schober, B. & Lueftenegger, M. (2018). Within-class
#' consensus on classroom goal structures: Relations to achievement and achievement
#' goals in mathematics and language classes. \emph{Learning and Individual Differences,
#' 67}, 78-90. https://doi.org/10.1016/j.lindif.2018.07.002
#'
#' Lindell, M. K., Brandt, C. J., & Whitney, D. J. (1999). A revised index of interrater
#' agreement for multi-item ratings of a single target. \emph{Applied Psychological
#' Measurement}, \emph{23}, 127-135. https://doi.org/10.1177/01466219922031257
#'
#' O'Neill, T. A. (2017). An overview of interrater agreement on Likert scales for
#' researchers and practitioners. \emph{Frontiers in Psychology}, \emph{8}, Article
#' 777. https://doi.org/10.3389/fpsyg.2017.00777
#'
#' @return
#' Returns a numeric vector containing r*wg(j) agreement index for multi-item scales
#' with the same length as \code{group} if \code{expand = TRUE} or a data frame with
#' following entries if \code{expand = FALSE}:
#'
#' \tabular{ll}{
#'   \code{cluster}          \tab cluster identifier \cr
#'   \code{n}                \tab cluster size \code{x} \cr
#'   \code{rwg.lindell}      \tab r*wg(j) estimate for each group \cr
#'   \code{z.rwg.lindell}    \tab Fisher z-transformed r*wg(j) estimate for each cluster \cr
#' }
#'
#' @export
#'
#' @examples
#' dat <- data.frame(id = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
#'                   cluster = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
#'                   x1 = c(2, 3, 2, 1, 1, 2, 4, 3, 5),
#'                   x2 = c(3, 2, 2, 1, 2, 1, 3, 2, 5),
#'                   x3 = c(3, 1, 1, 2, 3, 3, 5, 5, 4))
#'
#' # Compute Fisher z-transformed r*wg(j) for a multi-item scale with A = 5 response options
#' rwg.lindell(dat[, c("x1", "x2", "x3")], cluster = dat$cluster, A = 5)
#'
#' # Compute Fisher z-transformed r*wg(j) for a multi-item scale with a random variance of 2
#' rwg.lindell(dat[, c("x1", "x2", "x3")], cluster = dat$cluster, ranvar = 2)
#'
#' # Compute r*wg(j) for a multi-item scale with A = 5 response options
#' rwg.lindell(dat[, c("x1", "x2", "x3")], cluster = dat$cluster, A = 5, z = FALSE)
#'
#' # Compute Fisher z-transformed r*wg(j) for a multi-item scale with A = 5 response options,
#' # do not expand the vector
#' rwg.lindell(dat[, c("x1", "x2", "x3")], cluster = dat$cluster, A = 5, expand = FALSE)
rwg.lindell <- function(x, cluster, A = NULL, ranvar = NULL, z = TRUE, expand = TRUE,
                        na.omit = FALSE, as.na = NULL, check = TRUE) {

  ####################################################################################
  # Convert user-missing values into NA

  #----------------------------------------
  # Convert user-missing values into NA
  if (isTRUE(!is.null(as.na))) {

    x <- misty::as.na(x, na = as.na, check = check)

    #......
    # Missing values only
    if (isTRUE(all(is.na(x)))) {

      stop("After converting user-missing values into NA, matrix or data frame specified in 'x' is completely missing.",
           call. = FALSE)

    }

  }

  ####################################################################################
  # Input Check

  #......
  # Check if input 'x' is missing
  if (isTRUE(missing(x))) {

    stop("Please specify a matrix or data frame with numeric vectors for the argument 'x'.", call. = FALSE)

  }

  #......
  # Check if input 'x' is NULL
  if (isTRUE(is.null(x))) {

    stop("Input specified for the argument 'x' is NULL.", call. = FALSE)

  }

  #......
  # Check if only one variable specified in the input 'x'
  if (ncol(data.frame(cluster)) != 1) {

    stop("More than one variable specified for the argument 'x'.",call. = FALSE)

  }

  #......
  # Convert 'cluster' into a vector
  cluster <- unlist(cluster, use.names = FALSE)

  ###

  #......
  # Check input 'check'
  if (isTRUE(!is.logical(check))) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }

  if (isTRUE(check)) {

    #......
    # Check input 'x'
    if (isTRUE(!is.matrix(x) && !is.data.frame(x))) {

      stop("Please specify a matrix or data frame with numeric vectors for the argument 'x'.", call. = FALSE)

    }

    #......
    # Check input 'x'
    if (isTRUE(ncol(x) == 1L)) {

      stop("Please specify a matrix or data frame with more than one column or variable for the argument 'x'.",
           call. = FALSE)

    }

    #......
    # Numeric vector and cluster?
    if (isTRUE(nrow(x) != length(cluster))) {

      stop("Number of rows in the matrix or data frame in 'x' does not match with the length of the vector in 'cluster'.",
           call. = FALSE)

    }

    #......
    # Check input 'A'
    if (isTRUE(!is.null(A))) {

      if (isTRUE(length(na.omit(unique(unlist(x)))) > A)) {

        warning("There are more unique values in 'x' than the number of discrete response options specified in 'A'.",
                call. = FALSE)

      }

      # Check input 'x': Integer number
      if (isTRUE(A %% 1L != 0L || A < 0L)) {

        stop("Please specify a positive integer number for the argument 'A'.", call. = FALSE)

      }

    }

    #......
    # Check input 'A' and 'ranvar'
    if (isTRUE((is.null(A) && is.null(ranvar)) || (!is.null(A) && !is.null(ranvar)))) {

      stop("Please specify the argument 'A' or the argument 'ranvar'.", call. = FALSE)

    }

    #......
    # Check input 'z'
    if (isTRUE(!is.logical(z))) {

      stop("Please specify TRUE or FALSE for the argument 'z'.", call. = FALSE)

    }

    #......
    # Check input 'expand'
    if (isTRUE(!is.logical(expand))) {

      stop("Please specify TRUE or FALSE for the argument 'expand'.", call. = FALSE)

    }

  }

  ####################################################################################
  # Data and Arguments

  df <- data.frame(x, cluster = cluster, stringsAsFactors = FALSE)

  #----------------------------------------
  # Random variance based on A
  if (isTRUE(!is.null(A))) {

    ranvar <- (A^2L - 1L) / 12L

  }

  #----------------------------------------
  # Listwise deletion
  if (isTRUE(na.omit && any(is.na(x)))) {

    df <- na.omit(df)

    warning(paste("Listwise deletion of incomplete data, number of cases removed from the analysis:",
                  length(attributes(na.omit(df))$na.action)), call. = FALSE)

  }

  ####################################################################################
  # Main Function

  df.split <- split(df[, -grep("cluster", names(df))], df$cluster)

  rwg <- misty::as.na(vapply(df.split, function(y) 1L - (mean(vapply(y, var, na.rm = TRUE, FUN.VALUE = double(1L)), na.rm = TRUE) / ranvar), FUN.VALUE = double(1L)),
                      na = NaN, check = FALSE)

  #......
  # Expand
  if (isTRUE(expand)) {

    object <- rwg[match(cluster, names(rwg))]

    #......
    # Fisher z-transformation
    if (isTRUE(z)) {

      object <- ifelse(object == 1L | object == -1L, NA, atanh(object))

    }

  } else {

    object <- data.frame(cluster = names(rwg),
                         n = vapply(df.split, function(y) sum(apply(y, 1, function(z) sum(is.na(z)) != length(z))), FUN.VALUE = 1L),
                         rwg.lindell = rwg, z.rwg.lindell = ifelse(rwg == 1L | rwg == -1L, NA, atanh(rwg)), stringsAsFactors = FALSE)

  }

  ####################################################################################
  # Return object

  return(object)

}

