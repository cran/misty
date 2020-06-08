#' Centering at the Grand Mean or Centering Within Cluster
#'
#' This function is used to center predictors at the grand mean (CGM, i.e., grand mean centering) or
#' within cluster (CWC, i.e., group-mean centering).
#'
#' Predictors in a single-level regression can only be centered at the grand mean (CGM) by specifying
#' \code{type = "CGM"} (default) in conjunction with \code{group = NULL} (default).
#'
#' Level-1 (L1) predictors in a multilevel regression can be centered at the grand mean (CGM)
#' by specifying \code{type = "CGM"} (default) in conjunction with \code{group = NULL} (default) or
#' within cluster (CWC) by specifying \code{type = "CWC"} in conjunction with specifying a group
#' membership variable using the \code{group} argument.
#'
#' Level-2 (L2) predictors in a multilevel regression can only be centered at the grand mean (CGM) by
#' specifying \code{type = "CGM"} (default) in conjunction with specifying a group membership
#' variable using the \code{group} argument.
#'
#' Note that predictors can be centered on any meaningful value using the argument \code{value}.
#'
#' @param x           a numeric vector.
#' @param type        a character string indicating the type of centering, i.e.,
#'                    \code{"CGM"} for centering at the grand mean (i.e., grand mean centering) or
#'                    \code{"CWC"} for centering within cluster (i.e., group-mean centering).
#' @param group       a numeric vector, character vector or factor denoting the group membership
#'                    of each unit in \code{x}. Note, this argument is required for centering at
#'                    the grand mean (CGM) of a level-2 predictor or centering within cluster (CWC)
#'                    of a level-1 predictor.
#' @param value       a numeric value for centering on a specific user-defined value.
#' @param as.na       a numeric vector indicating user-defined missing values,
#'                    i.e. these values are converted to \code{NA} before conducting the analysis.
#'                    Note that \code{as.na()} function is only applied to \code{x} but not to \code{group}.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{dummy.c}}, \code{\link{group.scores}}, \code{\link{rec}}, \code{\link{reverse.item}},
#' \code{\link{rwg.lindell}}, \code{\link{scores}}.
#'
#' @references
#' Enders, C. K. (2013). Centering predictors and contextual effects. In M. A. Scott, J. S. Simonoff, &
#' B. D. Marx (Eds.), \emph{The Sage handbook of multilevel modeling} (pp. 89-109). Sage.
#' https://dx.doi.org/10.4135/9781446247600
#'
#' Enders, C. K., & Tofighi, D. (2007). Centering predictor variables in cross-sectional multilevel models:
#' A new look at an old issue. \emph{Psychological Methods, 12}, 121-138. https://doi.org/10.1037/1082-989X.12.2.121
#'
#' @return
#' Returns a numeric vector with the same length as \code{x} containing centered values.
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
#'                      group = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
#'                      x.l1 = c(4, 2, 5, 6, 3, 4, 1, 3, 4),
#'                      x.l2 = c(4, 4, 4, 1, 1, 1, 3, 3, 3),
#'                      y = c(5, 3, 6, 3, 4, 5, 2, 6, 5))
#'
#' # Center level-1 predictor at the grand mean (CGM)
#' center(dat.ml$x.l1)
#'
#' # Center level-1 predictor within cluster (CWC)
#' center(dat.ml$x.l1, type = "CWC", group = dat.ml$group)
#'
#' # Center level-2 predictor at the grand mean (CGM)
#' center(dat.ml$x.l2, type = "CGM", group = dat.ml$group)
center <- function(x, type = c("CGM", "CWC"), group = NULL, value = NULL, as.na = NULL, check = TRUE) {

  ####################################################################################
  # Input Check

  #......
  # Check if input 'x' is missing
  if (missing(x)) {

    stop("Please specify a numeric vector frame for the argument 'x'.", call. = FALSE)

  }

  #......
  # Check if input 'x' is NULL
  if (is.null(x)) {

    stop("Input specified for the argument 'x' is NULL.", call. = FALSE)

  }

  #......
  # Check input 'check'
  if (!is.logical(check)) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }

  #-----------------------------------------

  if (isTRUE(check)) {

    #......
    # Check input 'x'
    if (!is.atomic(x) || !is.numeric(x)) {

      stop("Please specify a numeric vector for the argument 'x'.", call. = FALSE)

    }

    #......
    # Check input 'type'
    if (all(!type %in% c("CGM", "CWC"))) {

      stop("Character string in the argument 'type' does not  match with \"CGM\" or \"CWC\".", call. = FALSE)

    }

    #......
    # Check input 'group'
    if (!is.null(group)) {

      if (length(group) != length(x)) {

        stop("The length of the vector in 'group' does not match with the length of the vector in 'x'.",
             call. = FALSE)

      }

    }

    #......
    # Centering Within Cluster
    if (all(type == "CWC") && is.null(group)) {

      stop("Please specify the argument 'group' to apply centering within cluster (CWC).",
           call. = FALSE)

    }

    #......
    # Grand Mean Centering of a Level 2 predictor
    if (all(type == "CWC") && !is.null(group)) {

      if (all(tapply(x, group, var, na.rm = TRUE) == 0)) {

        stop("Vector in 'x' is specified as level-1 predictor does not have any within-group variance.",
             call. = FALSE)

      }

    }

    #......
    # Grand Mean Centering of a Level 2 predictor
    if (all(type == "CGM") && !is.null(group)) {

      if (!all(tapply(x, group, var, na.rm = TRUE) == 0)) {

        stop("Vector in 'x' specified as level-2 predictor has within-group variance.",
             call. = FALSE)

      }

    }

  }

  ####################################################################################
  # Data and Arguments

  #----------------------------------------
  # Convert user-missing values into NA

  if (!is.null(as.na)) {

    x <- misty::as.na(x, as.na = as.na, check = check)

    # Variable with missing values only
    if (all(is.na(x))) {

      stop("After converting user-missing values into NA, variable 'x' is completely missing.",
           call. = FALSE)

    }

  }

  #----------------------------------------
  # Type of centering

  type <- ifelse (all(c("CGM", "CWC") %in% type), "CGM", type)

  #-----------------------------------------------------------------------------------
  # Main Function

  #----------------------------------------
  # Centering at the grand mean (CGM)

  if (type == "CGM") {

    #.........................
    # Single-level or L1 predictor
    if (is.null(group)) {

      # Mean centering
      if (is.null(value)) {

        object <- as.numeric(scale(x, scale = FALSE))

      # Center on a user-defined value
      } else {

        object <- x - value

      }

    #.........................
    # L2 predictor
    } else {

      # Mean centering
      if (is.null(value)) {

        object <- x - mean(x[which(!duplicated(group))], na.rm = TRUE)

      # Center on a user-defined value
      } else {

        object <- x - value

      }

    }

  #----------------------------------------
  # Centering within cluster (CWC)

  #.........................
  # L1 predictor
  } else {

    object <- unname(x - misty::group.scores(x, group = group, fun = "mean", check = check, expand = TRUE))

  }

  ####################################################################################
  # Return object

  return(object)

}
