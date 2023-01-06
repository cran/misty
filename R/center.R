#' Centering at the Grand Mean and Centering Within Cluster
#'
#' This function is used to center predictors at the grand mean (CGM, i.e., grand
#' mean centering) or within cluster (CWC, i.e., group-mean centering).
#'
#' Predictors in a single-level regression can only be centered at the grand mean
#' (CGM) by specifying \code{type = "CGM"} (default) in conjunction with
#' \code{cluster = NULL} (default).
#'
#' Level-1 (L1) predictors in a multilevel regression can be centered at the
#' grand mean (CGM) by specifying \code{type = "CGM"} (default) in conjunction
#' with code{cluster = NULL} (default) or within cluster (CWC) by specifying
#' \code{type = "CWC"} in conjunction with specifying a cluster membership variable
#' using the \code{cluster} argument.
#'
#' Level-2 (L2) predictors in a multilevel regression can only be centered at
#' the grand mean (CGM) by specifying \code{type = "CGM"} (default) in conjunction
#' with specifying a cluster membership variable using the \code{cluster} argument.
#'
#' Note that predictors can be centered on any meaningful value using the argument
#' \code{value}.
#'
#' @param x       a numeric vector.
#' @param type    a character string indicating the type of centering, i.e.,
#'                \code{"CGM"} for centering at the grand mean (i.e., grand mean
#'                centering) or \code{"CWC"} for centering within cluster (i.e.,
#'                group-mean centering).
#' @param cluster a vector representing the nested grouping structure (i.e.,
#'                group or cluster variable) of each unit in \code{x}. Note, this
#'                argument is required for centering at the grand mean (CGM) of
#'                a level-2 predictor or centering within cluster (CWC) of a
#'                level-1 predictor.
#' @param value   a numeric value for centering on a specific user-defined value.
#' @param as.na   a numeric vector indicating user-defined missing values, i.e.
#'                these values are converted to \code{NA} before conducting the
#'                analysis. Note that \code{as.na()} function is only applied to
#'                \code{x} but not to \code{cluster}.
#' @param check   logical: if \code{TRUE}, argument specification is checked.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{dummy.c}}, \code{\link{cluster.scores}}, \code{\link{rec}},
#' \code{\link{item.reverse}}, code{\link{rwg.lindell}}, \code{\link{item.scores}}.
#'
#' @references
#' Chang, C.-N., & Kwok, O.-M. (2022) Partitioning Variance for a Within-Level
#' Predictor in Multilevel Models. \emph{Structural Equation Modeling: A
#' Multidisciplinary Journal}. Advance online publication.
#' https://doi.org/10.1080/10705511.2022.2051175#'
#'
#' Enders, C. K. (2013). Centering predictors and contextual effects. In M. A.
#' Scott, J. S. Simonoff, & B. D. Marx (Eds.), \emph{The Sage handbook of
#' multilevel modeling} (pp. 89-109). Sage. https://dx.doi.org/10.4135/9781446247600
#'
#' Enders, C. K., & Tofighi, D. (2007). Centering predictor variables in
#' cross-sectional multilevel models: A new look at an old issue. \emph{Psychological
#' Methods, 12}, 121-138. https://doi.org/10.1037/1082-989X.12.2.121
#'
#' Rights, J. D., Preacher, K. J., & Cole, D. A. (2020). The danger of conflating
#' level-specific effects of control variables when primary interest lies in
#' level-2 effects. \emph{British Journal of Mathematical & Statistical Psychology,
#' 73}, 194-211. https://doi.org/10.1111/bmsp.12194
#'
#' Yaremych, H. E., Preacher, K. J., & Hedeker, D. (2021). Centering categorical
#' predictors in multilevel models: Best practices and interpretation.
#' \emph{Psychological Methods}. Advance online publication.
#' https://doi.org/10.1037/met0000434
#'
#' @return
#' Returns a numeric vector with the same length as \code{x} containing centered
#' values.
#'
#' @export
#'
#' @examples
#' #--------------------------------------
#' # Predictors in a single-level regression
#' dat.sl <- data.frame(x = c(4, 2, 5, 6, 3, 4, 1, 3, 4),
#'                      y = c(5, 3, 6, 3, 4, 5, 2, 6, 5))
#'
#' # Center predictor at the sample mean
#' center(dat.sl$x)
#'
#' # Center predictor at the value 3
#' center(dat.sl$x, value = 3)
#'
#' #--------------------------------------
#' # Predictors in a multilevel regression
#' dat.ml <- data.frame(id = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
#'                      cluster = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
#'                      x.l1 = c(4, 2, 5, 6, 3, 4, 1, 3, 4),
#'                      x.l2 = c(4, 4, 4, 1, 1, 1, 3, 3, 3),
#'                      y = c(5, 3, 6, 3, 4, 5, 2, 6, 5))
#'
#' # Center level-1 predictor at the grand mean (CGM)
#' center(dat.ml$x.l1)
#'
#' # Center level-1 predictor within cluster (CWC)
#' center(dat.ml$x.l1, type = "CWC", cluster = dat.ml$cluster)
#'
#' # Center level-2 predictor at the grand mean (CGM)
#' center(dat.ml$x.l2, type = "CGM", cluster = dat.ml$cluster)
center <- function(x, type = c("CGM", "CWC"), cluster = NULL, value = NULL,
                   as.na = NULL, check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'x' is missing
  if (isTRUE(missing(x))) { stop("Please specify a numeric vector frame for the argument 'x'.", call. = FALSE) }

  # Check if input 'x' is NULL
  if (isTRUE(is.null(x))) { stop("Input specified for the argument 'x' is NULL.", call. = FALSE) }

  # Check if only one variable specified in the input 'x'
  if (ncol(data.frame(x)) != 1L) { stop("More than one variable specified for the argument 'x'.",call. = FALSE) }

  # Convert 'x' into a vector
  x <- unlist(x, use.names = FALSE)

  #-----------------------------------------

  # Check input 'cluster'
  if (isTRUE(!is.null(cluster))) {

    if (isTRUE(nrow(data.frame(cluster)) != nrow(data.frame(x)))) { stop("The length of the vector in 'cluster' does not match with the length of the vector in 'x'.", call. = FALSE) }

    # Check if only one variable specified in the input 'cluster'
    if (isTRUE(ncol(data.frame(cluster)) != 1L)) { stop("More than one variable specified for the argument 'cluster'.",call. = FALSE) }

    # Convert 'cluster' into a vector
    cluster <- unlist(cluster, use.names = FALSE)

  }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # Check input 'x'
    if (isTRUE(mode(x) != "numeric")) { stop("Please specify a numeric vector for the argument 'x'.", call. = FALSE) }

    # Check input 'type'
    if (isTRUE(all(!type %in% c("CGM", "CWC")))) { stop("Character string in the argument 'type' does not match with \"CGM\" or \"CWC\".", call. = FALSE) }

    # Centering Within Cluster
    if (isTRUE(all(type == "CWC") && is.null(cluster))) { stop("Please specify the argument 'cluster' to apply centering within cluster (CWC).", call. = FALSE) }

    # Check input 'cluster'
    if (isTRUE(!is.null(cluster))) {

      # Group Mean Centering of a Level 1 predictor
      if (isTRUE(all(type == "CWC"))) {

        if (isTRUE(all(na.omit(as.vector(tapply(x, cluster, var, na.rm = TRUE) == 0L))))) {

          stop("Vector in 'x' specified as level-1 predictor does not have any within-cluster variance.", call. = FALSE)

        }

      }

      # Group Mean Centering of a Level 2 predictor
      if (isTRUE(all(type == "CGM"))) {

        if (isTRUE(any(na.omit(as.vector(tapply(x, cluster, var, na.rm = TRUE) != 0L))))) {

          stop("Vector in 'x' specified as level-2 predictor has within-cluster variance.", call. = FALSE)

        }

      }

    }

  }

  #_____________________________________________________________________________
  #
  # Data and Arguments ---------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) {

    # Replace user-specified values with NAs
    x <- misty::as.na(x, na = as.na, check = check)

    # Variable with missing values only
    if (isTRUE(all(is.na(x)))) { stop("After converting user-missing values into NA, variable 'x' is completely missing.", call. = FALSE) }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Type of centering ####

  type <- ifelse (all(c("CGM", "CWC") %in% type), "CGM", type)

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  if (isTRUE(is.null(value))) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Centering at the grand mean (CGM) ####
    if (isTRUE(type == "CGM")) {

      #...................
      ### Single-level or L1 predictor ####
      if (isTRUE(is.null(cluster))) {

        # Mean centering
        object <- as.numeric(scale(x, scale = FALSE))

      #...................
      ### L2 predictor ####
      } else {

        # Mean centering
        object <- x - mean(x[which(!duplicated(cluster))], na.rm = TRUE)

      }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Centering within cluster (CWC) ####
    } else if (isTRUE(type == "CWC")) {

      object <- unname(x - misty::cluster.scores(x, cluster = cluster, fun = "mean", check = check, expand = TRUE))

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Centering on a user-defined value ####
  } else {

    object <- x - value

    warning(paste("Variable centered at the user-defined value", value), call. = FALSE)

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  return(object)

}
