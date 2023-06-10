#' Intraclass Correlation Coefficient, ICC(1) and ICC(2)
#'
#' This function computes the intraclass correlation coefficient ICC(1), i.e.,
#' proportion of the total variance explained by the grouping structure, and ICC(2),
#' i.e., reliability of aggregated variables.
#'
#' Note that this function is restricted to two-level models.
#'
#' @param x           a vector, matrix or data frame.
#' @param cluster     either a character string indicating the variable name of
#'                    the cluster variable in 'x' or a vector representing the
#'                    nested grouping structure (i.e., group or cluster variable).
#' @param type        numeric value indicating the type of intraclass correlation
#'                    coefficient, i.e., \code{type = 1} for ICC(1) and \code{type = 2}
#'                    for ICC(2).
#' @param method      a character string indicating the method used to estimate
#'                    intraclass correlation coefficients, i.e., \code{method = "aov"}
#'                    ICC estimated using the \code{aov} function, \code{method = "lme4"}
#'                    (default) ICC estimated using the \code{lmer} function in the
#'                    \pkg{lme4} package, \code{method = "nlme"} ICC estimated using
#'                    the \code{lme} function in the \pkg{nlme} package. Note that
#'                    if the lme4 package is not installed, method = "aov" will be used.
#' @param REML        logical: if \code{TRUE} (default), restricted maximum likelihood
#'                    is used to estimate the null model when using the \code{lmer}
#'                    function in the \pkg{lme4} package or the \code{lme} function
#'                    in the \pkg{nlme} package.
#' @param as.na       a numeric vector indicating user-defined missing values,
#'                    i.e. these values are converted to \code{NA} before conducting
#'                    the analysis. Note that \code{as.na()} function is only applied
#'                    to \code{x} but not to \code{cluster}.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{multilevel.descript}}
#'
#' @references
#' Hox, J., Moerbeek, M., & van de Schoot, R. (2018). \emph{Multilevel analysis:
#' Techniques and applications} (3rd. ed.). Routledge.
#'
#' Snijders, T. A. B., & Bosker, R. J. (2012). \emph{Multilevel analysis: An introduction
#' to basic and advanced multilevel modeling} (2nd ed.). Sage Publishers.
#'
#' @return
#' Returns a numeric vector with intraclass correlation coefficient(s).
#'
#' @export
#'
#' @examples
#' dat <- data.frame(id = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
#'                   cluster = c(1, 1, 1, 1, 2, 2, 3, 3, 3),
#'                   x1 = c(2, 3, 2, 2, 1, 2, 3, 4, 2),
#'                   x2 = c(3, 2, 2, 1, 2, 1, 3, 2, 5),
#'                   x3 = c(2, 1, 2, 2, 3, 3, 5, 2, 4))
#'
#' #---------------------------
#' # Cluster variable specification
#'
#' # Cluster variable 'cluster' in 'x'
#' multilevel.icc(dat[, c("x1", "cluster")], cluster = "cluster")
#'
#' # Cluster variable 'cluster' not in 'x'
#' multilevel.icc(dat$x1, cluster = dat$cluster)
#'
#' #---------------------------
#' # ICC(1) for x1
#' multilevel.icc(dat$x1, cluster = dat$cluster)
#'
#' # ICC(1) for x1, convert value 1 to NA
#' multilevel.icc(dat$x1, cluster = dat$cluster, as.na = 1)
#'
#' # ICC(2) for x1
#' multilevel.icc(dat$x1, cluster = dat$cluster, type = 2)
#'
#' # ICC(1) for x1,
#' # use lmer() function in the lme4 package to estimate ICC
#' multilevel.icc(dat$x1, cluster = dat$cluster, method = "lme4")
#'
#' # ICC(1) for x1, x2, and x3
#' multilevel.icc(dat[, c("x1", "x2", "x3")], cluster = dat$cluster)
multilevel.icc <- function(x, cluster, type = 1, method = c("aov", "lme4", "nlme"),
                           REML = TRUE, as.na = NULL, check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'x' is missing
  if (isTRUE(missing(x))) { stop("Please specify a vector, matrix or data frame for the argument 'x'.", call. = FALSE) }

  # Check if input 'x' is NULL
  if (isTRUE(is.null(x))) { stop("Input specified for the argument 'x' is NULL.", call. = FALSE) }

  # Vector, matrix or data frame for the argument 'x'?
  if (isTRUE(!is.atomic(x) && !is.matrix(x) && !is.data.frame(x))) { stop("Please specify a numeric vector, matrix or data frame with numeric variables for the argument 'x'.", call. = FALSE) }

  # Check input 'cluster'
  if (isTRUE(missing(cluster))) { stop("Please specify a vector representing the grouping structure for the argument 'cluster'.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Cluster variable ####

  # Cluster variable 'cluster' in 'x'
  if (isTRUE(length(cluster) == 1L)) {

    if (isTRUE(!cluster %in% colnames(x))) { stop("Cluster variable specifed in the argument 'cluster' was not found in 'x'.", call. = FALSE) }

    # Index of cluster in 'x'
    cluster.col <- which(colnames(x) == cluster)

    # Replace variable name with cluster variable
    cluster <- x[, cluster.col]

    # Remove cluster variable
    x <- x[, -cluster.col, drop = FALSE]

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) {

    x <- misty::as.na(x, na = as.na, check = check)

    # Variable with missing values only
    x.miss <- vapply(as.data.frame(x), function(y) all(is.na(y)), FUN.VALUE = logical(1L))
    if (isTRUE(any(x.miss))) {

      stop(paste0("After converting user-missing values into NA, following variables are completely missing: ",
                  paste(names(which(x.miss)), collapse = ", ")), call. = FALSE)

    }

  }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # Check input 'cluster'
    if (isTRUE(length(unique(na.omit(cluster))) == 1L)) { stop("There is only one group represented in the cluster variable specified in 'cluster'.", call. = FALSE) }

    # Check input 'cluster'
    if (isTRUE(is.null(dim(x)))) {

      # Numeric vector and cluster?
      if (isTRUE(length(x) != length(cluster))) { stop("Length of the vector 'x' does not match with the length of the cluster variable in 'cluster'.", call. = FALSE) }

    } else {

      # Numeric vector and cluster?
      if (isTRUE(nrow(x) != length(cluster))) { stop("Number of rows in 'x' does not match with the length of the cluster variable 'cluster'.", call. = FALSE) }

    }

    # Variance within cluster
    if (isTRUE(is.null(dim(x)))) {

      if (isTRUE(all(tapply(unlist(x), cluster, function(y) length(na.omit(y))) <= 1L))) {

        stop("Varialbe specified in 'x' does not have any within-cluster variance.", call. = FALSE)

      }

    } else {

      if (isTRUE(any(apply(x, 2, function(y) all(tapply(y, cluster, function(z) length(na.omit(z))) <= 1L))))) {

        stop("There are variables in 'x' without any within-cluster variance.", call. = FALSE)

      }

    }

    # Check input 'x': Zero variance?
    x.check <- vapply(as.data.frame(x), function(y) length(na.omit(unique(y))) == 1L, FUN.VALUE = logical(1L))

    if (isTRUE(any(x.check))) {

      if (isTRUE(length(x.check) > 1L)) {

        warning(paste0("Following variables in the matrix or data frame specified in 'x' have zero variance: ", paste(names(which(x.check)), collapse = ", ")), call. = FALSE)

      } else {

        stop("Vector specified in 'x' has zero variance.", call. = FALSE)

      }

    }

    # Check input 'type'
    if (isTRUE(any(!type %in% c(1L, 2L)))) { stop("Please specify the numeric value 1 or 2 for the argument'type'.", call. = FALSE) }

    # Check input 'method'
    if (isTRUE(any(!method %in% c("aov", "lme4", "nlme")))) { stop("Character string in the argument 'method' does not match with \"aov\", \"lme4\", or \"nlme\".", call. = FALSE) }

    # Check input 'REML'
    if (isTRUE(!is.logical(REML))) { stop("Please specify TRUE or FALSE for the argument 'REML'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  if (isTRUE(length(method) == 1L)) {

    # Package lme4 installed?
    if (isTRUE(method == "lme4")) {

      if (isTRUE(!nzchar(system.file(package = "lme4")))) {

        warning("Package \"lme4\" is needed for method = \"lme4\", method \"aov\" will be used instead.", call. = FALSE )

        method <- "aov"

      }

    }

    # Package nlme installed?
    if (isTRUE(method == "nlme")) {

      if (isTRUE(!nzchar(system.file(package = "nlme")))) {

        warning("Package \"nlme\" is needed for method = \"nlme\", method \"aov\" will be used instead.",
                call. = FALSE )

        method <- "aov"

      }

    }

  } else {

    # Method default option
    if (isTRUE(all(c("aov", "lme4", "nlme") %in% method))) {

      if (isTRUE(nzchar(system.file(package = "lme4")))) {

        method <- "lme4"

      } else {

        method <- "aov"

      }

    }

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## One dependent variable ####

  if (isTRUE(is.null(dim(x)))) {

    #...................
    ### Variable with non-zero variance ####
    if (isTRUE(var(x, na.rm = TRUE) != 0L)) {

      # ICC using aov() function
      if (isTRUE(method == "aov")) {

        # Estimate model
        mod <- aov(x ~ 1 + Error(as.factor(cluster)))

        # Model summary
        mod.summary <- summary(mod)

        # Mean Squared Error Between
        MSQ.B <- unlist(mod.summary[[1]])["Mean Sq"]

        # Mean Squared Error Within
        MSQ.W <- unlist(mod.summary[[2]])["Mean Sq"]

        # ICC(1)
        if (isTRUE(type == 1L)) {

          # Average cluster size
          cluster.size <- mean(tapply(x, cluster, function(y) sum(!is.na(y))))

          # Intraclass correlation coefficient, ICC(1)
          object <- unname((MSQ.B - MSQ.W) / (MSQ.B + ((cluster.size - 1L) * MSQ.W)))

          if (isTRUE(object < 0L)) { object <- 0L }

        # ICC(2)
        } else {

          # Intraclass correlation coefficient, ICC(2)
          object <- unname((MSQ.B - MSQ.W) / MSQ.B)

          if (isTRUE(object < 0L)) { object <- 0L }

        }

      # ICC using lmer() function
      } else if (isTRUE(method == "lme4")) {

        # Estimate model
        mod <- suppressMessages(lme4::lmer(x ~ 1 + (1|cluster), REML = REML,
                                control = lme4::lmerControl(optimizer = "bobyqa")))

        # Variance components
        vartab <- as.data.frame(suppressMessages(lme4::VarCorr(mod)))

        # Between-cluster variance
        var.u <- vartab[vartab$grp == "cluster", "vcov"]

        # Within-cluster variance
        var.r <- vartab[vartab$grp == "Residual", "vcov"]

        # Total variance
        var.total <- var.u + var.r

        # ICC(1)
        if (isTRUE(type == 1L)) {

          # Intraclass correlation coefficient, ICC(1)
          object <- var.u / var.total

        # ICC(2)
        } else {

          # Average cluster size
          cluster.size <- mean(tapply(x, cluster, function(y) sum(!is.na(y))))

          # Intraclass correlation coefficient, ICC(2)
          object <- var.u / (var.u + var.r / cluster.size)

        }

      # ICC using lme() function
      } else if (isTRUE(method == "nlme")) {

        # REML or ML
        ifelse(isTRUE(REML), REML <- "REML", REML <- "ML")

        # Estimate model
        mod <- nlme::lme(x ~ 1, random = ~1 | cluster, na.action = na.omit, method = REML)

        # Variance components
        vartab <- nlme::VarCorr(mod)

        var.u  <- as.numeric(vartab["(Intercept)", "Variance"])

        var.r <- as.numeric(vartab["Residual", "Variance"])

        # Total variance
        var.total <- var.u + var.r

        # ICC(1)
        if (isTRUE(type == 1L)) {

          # Intraclass correlation coefficient, ICC(1)
          object <- var.u / var.total

          # ICC(2)
        } else {

          # Average cluster size
          cluster.size <- mean(tapply(x, cluster, function(y) sum(!is.na(y))))

          # Intraclass correlation coefficient, ICC(2)
          object <- var.u / (var.u + var.r / cluster.size)

        }

      }

    #...................
    ### Variable with non-zero variance ####
    } else {

      object <- NA

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## More than one dependent variable ####
  } else {

    object <- apply(x, 2, function(y) misty::multilevel.icc(y, cluster, type = type, method = method,
                                                            REML = REML, as.na = NULL, check = FALSE))

  }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  return(object)

}
