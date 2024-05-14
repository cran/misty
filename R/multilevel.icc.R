#' Intraclass Correlation Coefficient, ICC(1) and ICC(2)
#'
#' This function computes the intraclass correlation coefficient ICC(1), i.e.,
#' proportion of the total variance explained by the grouping structure, and
#' ICC(2), i.e., reliability of aggregated variables in a two-level and
#' three-level model.
#'
#' @param ...     a numeric vector, matrix, or data frame. Alternatively, an
#'                expression indicating the variable names in \code{data}. Note
#'                that the operators \code{.}, \code{+}, \code{-}, \code{~},
#'                \code{:}, \code{::}, and \code{!} can also be used to select
#'                variables, see 'Details' in the \code{\link{df.subset}}
#'                function.
#' @param data    a data frame when specifying one or more variables in the
#'                argument \code{...}. Note that the argument is \code{NULL}
#'                when specifying a numeric vector, matrix, or data frame for
#'                the argument \code{...}.
#' @param cluster a character string indicating the name of the cluster
#'                variable in \code{...} or \code{data} for two-level data,
#'                a character vector indicating the names of the cluster
#'                variables in \code{...} for three-level data, or a vector
#'                or data frame representing the nested grouping structure
#'                (i.e., group or cluster variables). Alternatively, a
#'                character string or character vector indicating the variable
#'                name(s) of the cluster variable(s) in \code{data}. Note that
#'                the cluster variable at Level 3 come first in a three-level
#'                model, i.e., \code{cluster = c("level3", "level2")}.
#' @param type    a character string indicating the type of intraclass correlation
#'                coefficient, i.e., \code{type = "1a"} (default) for ICC(1) and
#'                \code{type = "2"} for ICC(2) when specifying a two-level model
#'                (i.e., one cluster variable), and \code{type = "1a"} (default)
#'                for ICC(1) representing the propotion of variance at Level 2
#'                and Level 3, \code{type = "1b"} representing an estimate
#'                of the expected correlation between two randomly chosen elements
#'                in the same group, and \code{type = "2"} for ICC(2) when
#'                specifying a three-level model (i.e., two cluster variables).
#'                See 'Details' for the formula used in this function.
#' @param method  a character string indicating the method used to estimate
#'                intraclass correlation coefficients, i.e., \code{method = "aov"}
#'                ICC estimated using the \code{aov} function, \code{method = "lme4"}
#'                (default) ICC estimated using the \code{lmer} function in the
#'                \pkg{lme4} package, \code{method = "nlme"} ICC estimated using
#'                the \code{lme} function in the \pkg{nlme} package. Note that
#'                if the lme4 or nlme package is needed when estimating ICCs in
#'                a three-level model.
#' @param REML    logical: if \code{TRUE} (default), restricted maximum likelihood
#'                is used to estimate the null model when using the \code{lmer}
#'                function in the \pkg{lme4} package or the \code{lme} function
#'                in the \pkg{nlme} package.
#' @param as.na   a numeric vector indicating user-defined missing values,
#'                i.e. these values are converted to \code{NA} before conducting
#'                the analysis. Note that \code{as.na()} function is only applied
#'                to \code{...} but not to \code{cluster}.
#' @param check   logical: if \code{TRUE} (default), argument specification is
#'                checked.
#'
#' @details
#' \describe{
#' \item{\strong{Two-Level Model}}{In a two-level model, the intraclass
#' correlation coefficients are computed in the random intercept-only model:
#'
#' \deqn{Y_{ij} = \gamma_{00} + u_{0j} + r_{ij}}
#'
#' where the variance in \eqn{Y} is decomposed into two independent components:
#' \eqn{\sigma^2_{u_{0}}}, which represents the variance at Level 2, and
#' \eqn{\sigma^2_{r}}, which represents the variance at Level 1 (Hox et al.,
#' 2018). These two variances sum up to the total variance and are referred to
#' as variance components. The intraclass correlation coefficient, ICC(1)
#' \eqn{\rho} requested by \code{type = "1a"} represents the proportion of the
#' total variance explained by the grouping structure and is defined by the equation
#'
#' \deqn{\rho = \frac{\sigma^2_{u_{0}}}{\sigma^2_{u_{0}} + \sigma^2_{r}}}
#'
#' The intraclass correlation coefficient, ICC(2) \eqn{\lambda_j} requested by
#' \code{type = "2"} represents the reliability of aggregated variables and is
#' defined by the equation
#'
#' \deqn{\lambda_j = \frac{\sigma^2_{u_{0}}}{\sigma^2_{u_{0}} + \frac{\sigma^2_{r}}{n_j}} = \frac{n_j\rho}{1 + (n_j - 1)\rho}}
#'
#' where \eqn{n_j} is the average group size (Snijders & Bosker, 2012).
#' }
#'
#' \item{\strong{Three-Level Model}}{In a three-level model, the intraclass
#' correlation coefficients are computed in the random intercept-only model:
#'
#' \deqn{Y_{ijk} = \gamma_{000} + v_{0k} + u_{0jk} + r_{ijk}}
#'
#' where the variance in \eqn{Y} is decomposed into three independent components:
#' \eqn{\sigma^2_{v_{0}}}, which represents the variance at Level 3,
#' \eqn{\sigma^2_{u_{0}}}, which represents the variance at Level 2, and
#' \eqn{\sigma^2_{r}}, which represents the variance at Level 1 (Hox et al.,
#' 2018). There are two ways to compute intraclass correlation coefficients
#' in a three-level model. The first method requested by \code{type = "1a"}
#' represents the proportion of variance at Level 2 and Level 3 and should be
#' used if we are interestd in a decomposition of the variance across levels.
#' The intraclass correlation coefficient, ICC(1) \eqn{\rho_{L2}} at Level 2 is
#' defined as:
#'
#' \deqn{\rho_{L2} = \frac{\sigma^2_{u_{0}}}{\sigma^2_{v_{0}} + \sigma^2_{u_{0}} + \sigma^2_{r}}}
#'
#' The ICC(1) \eqn{\rho_{L3}} at Level 3 is defined as:
#'
#' \deqn{\rho_{L3} = \frac{\sigma^2_{v_{0}}}{\sigma^2_{v_{0}} + \sigma^2_{u_{0}} + \sigma^2_{r}}}
#'
#' The second method requested by \code{type = "1b"} represents the expected
#' correlation between two randomly chosen elements in the same group. The
#' intraclass correlation coefficient, ICC(1) \eqn{\rho_{L2}} at Level 2 is
#' defined as:
#'
#' \deqn{\rho_{L2} = \frac{\sigma^2_{v_{0}} + \sigma^2_{u_{0}}}{\sigma^2_{v_{0}} + \sigma^2_{u_{0}} + \sigma^2_{r}}}
#'
#' The ICC(1) \eqn{\rho_L3} at Level 3 is defined as:
#'
#' \deqn{\rho_{L3} = \frac{\sigma^2_{v_{0}}}{\sigma^2_{v_{0}} + \sigma^2_{u_{0}} + \sigma^2_{r}}}
#'
#' Note that both formula are correct, but express different aspects of the data,
#' which happen to coincide when there are only two levels (Hox et al., 2018).
#'
#' The intraclass correlation coefficients, ICC(2) requested by \code{type = "2"}
#' represent the reliability of aggregated variables at Level 2 and Level 3.
#' The ICC(2) \eqn{\lambda_j} at Level 2 is defined as:
#'
#' \deqn{\lambda_j = \frac{\sigma^2_{u_{0}}}{\sigma^2_{u_{0}} + \frac{\sigma^2_{r}}{n_j}}}
#'
#' The ICC(2) \eqn{\lambda_k} at Level 3 is defined as:
#'
#' \deqn{\lambda_k = \frac{\sigma^2_{v_{0}}}{\frac{{\sigma^2_{v_{0}} + \sigma^2_{u_{0}}}}{n_{j}} + \frac{\sigma^2_{r}}{n_k \cdot n_j}}}
#'
#' where \eqn{n_j} is the average group size at Level 2 and \eqn{n_j} is the average
#' group size at Level 3 (Hox et al., 2018).}
#' }
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{multilevel.cfa}}, \code{\link{multilevel.cor}}, \code{\link{multilevel.descript}}
#'
#' @references
#' Hox, J., Moerbeek, M., & van de Schoot, R. (2018). \emph{Multilevel analysis:
#' Techniques and applications} (3rd. ed.). Routledge.
#'
#' Snijders, T. A. B., & Bosker, R. J. (2012). \emph{Multilevel analysis: An introduction
#' to basic and advanced multilevel modeling} (2nd ed.). Sage Publishers.
#'
#' @return
#' Returns a numeric vector or matrix with intraclass correlation coefficient(s).
#' In a three level model, the label \code{L2} is used for ICCs at Level 2
#' and \code{L3} for ICCs at Level 3.
#'
#' @export
#'
#' @examples
#' # Load data set "Demo.twolevel" in the lavaan package
#' data("Demo.twolevel", package = "lavaan")
#'
#' #----------------------------------------------------------------------------
#' # Two-Level Models
#'
#' #..........
#' # Cluster variable specification
#'
#' # Example 1a: Cluster variable 'cluster' in '...'
#' multilevel.icc(Demo.twolevel[, c("y1", "cluster")], cluster = "cluster")
#'
#' # Example 1b: Cluster variable 'cluster' not in '...'
#' multilevel.icc(Demo.twolevel$y1, cluster = Demo.twolevel$cluster)
#'
#' # Example 1c: Alternative specification using the 'data' argument
#' multilevel.icc(y1, data = Demo.twolevel, cluster = "cluster")
#'
#' #..........
#'
#' # Example 2: ICC(1) for 'y1'
#' multilevel.icc(Demo.twolevel$y1, cluster = Demo.twolevel$cluster)
#'
#' # Example 3: ICC(2)
#' multilevel.icc(Demo.twolevel$y1, cluster = Demo.twolevel$cluster, type = 2)
#'
#' # Example 4: ICC(1)
#' # use lme() function in the lme4 package to estimate ICC
#' multilevel.icc(Demo.twolevel$y1, cluster = Demo.twolevel$cluster, method = "nlme")
#'
#' # Example 5a: ICC(1) for 'y1', 'y2', and 'y3'
#' multilevel.icc(Demo.twolevel[, c("y1", "y2", "y3")], cluster = Demo.twolevel$cluster)
#'
#' # Example 5b: Alternative specification using the 'data' argument
#' multilevel.icc(y1:y3, data = Demo.twolevel, cluster = "cluster")
#'
#' #----------------------------------------------------------------------------
#' # Three-Level Models
#'
#' # Create arbitrary three-level data
#' Demo.threelevel <- data.frame(Demo.twolevel, cluster2 = Demo.twolevel$cluster,
#'                                              cluster3 = rep(1:10, each = 250))
#'
#' #..........
#' # Cluster variable specification
#'
#' # Example 6a: Cluster variables 'cluster' in '...'
#' multilevel.icc(Demo.threelevel[, c("y1", "cluster3", "cluster2")],
#'                cluster = c("cluster3", "cluster2"))
#'
#' # Example 6b: Cluster variables 'cluster' not in '...'
#' multilevel.icc(Demo.threelevel$y1, cluster = Demo.threelevel[, c("cluster3", "cluster2")])
#'
#' # Example 6c: Alternative specification using the 'data' argument
#' multilevel.icc(y1, data = Demo.threelevel, cluster = c("cluster3", "cluster2"))
#'
#' #----------------------------------------------------------------------------
#'
#' # Example 7a: ICC(1), propotion of variance at Level 2 and Level 3
#' multilevel.icc(y1, data = Demo.threelevel, cluster = c("cluster3", "cluster2"))
#'
#' # Example 7b: ICC(1), expected correlation between two randomly chosen elements
#' # in the same group
#' multilevel.icc(y1, data = Demo.threelevel, cluster = c("cluster3", "cluster2"),
#'                type = "1b")
#'
#' # Example 7c: ICC(2)
#' multilevel.icc(y1, data = Demo.threelevel, cluster = c("cluster3", "cluster2"),
#'                type = "2")
multilevel.icc <- function(..., data = NULL, cluster, type = c("1a", "1b", "2"),
                           method = c("aov", "lme4", "nlme"), REML = TRUE,
                           as.na = NULL, check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input '...' is missing
  if (isTRUE(missing(...))) { stop("Please specify the argument '...'.", call. = FALSE) }

  # Check if input '...' is NULL
  if (isTRUE(is.null(substitute(...)))) { stop("Input specified for the argument '...' is NULL.", call. = FALSE) }

  # Check if input 'data' is data frame
  if (isTRUE(!is.null(data) && !is.data.frame(data))) { stop("Please specify a data frame for the argument 'data'.", call. = FALSE) }

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

    # Convert tibble to data frame
    if (isTRUE("tbl" %in% substr(class(data), 1L, 3L))) { data <- data.frame(data) }

    # Variable names
    var.names <- .var.names(..., data = data, cluster = cluster, check.chr = "a matrix or data frame")

    # Extract data
    x <- data[, var.names]

    # Cluster variable
    cluster <- data[, cluster]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data without using the argument 'data' ####

  } else {

    # Extract data
    x <- eval(..., enclos = parent.frame())

    # Convert tibble to data frame
    if (isTRUE("tbl" %in% substr(class(cluster), 1L, 3L))) {

      if (isTRUE(ncol(cluster) == 1L)) {

        cluster <- unlist(cluster)

      } else {

        cluster <- data.frame(cluster)

      }

    }

    # Data and cluster
    var.group <- .var.group(data = x, cluster = cluster)

    # Data
    if (isTRUE(!is.null(var.group$data))) { x <- var.group$data }

    # Cluster variable
    if (isTRUE(!is.null(var.group$cluster))) { cluster <- var.group$cluster }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Cluster variables ####

  if (isTRUE(ncol(as.data.frame(cluster)) == 2L)) {

    l3.cluster <- cluster[, 1L]
    l2.cluster <- cluster[, 2L]

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) { x <- misty::as.na(x, na = as.na, check = check) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # Check input 'type'
    if (isTRUE(any(!type %in% c(c("1a", "1b", "2"))))) { stop("Please specify \"1a\", \"1b\", or \"2\" for the argument'type'.", call. = FALSE) }

    # Check input 'method'
    if (isTRUE(any(!method %in% c("aov", "lme4", "nlme")))) { stop("Character string in the argument 'method' does not match with \"aov\", \"lme4\", or \"nlme\".", call. = FALSE) }

    # Check input 'REML'
    if (isTRUE(!is.logical(REML))) { stop("Please specify TRUE or FALSE for the argument 'REML'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Type default option ####

  if (isTRUE(all(c("1a", "1b", "2") %in% type))) { type <- "1a" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Method default option ####

  if (isTRUE(all(c("aov", "lme4", "nlme") %in% method))) { method <- "lme4" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Check if packages are installed ####

  # Package lme4 installed?
  if (isTRUE(method == "lme4")) { if (isTRUE(!nzchar(system.file(package = "lme4")))) { stop("Package \"lme4\" is needed for method = \"lme4\", please install the package or switch to a different method.", call. = FALSE) } }

  # Package nlme installed?
  if (isTRUE(method == "nlme")) { if (isTRUE(!nzchar(system.file(package = "nlme")))) { stop("Package \"nlme\" is needed for method = \"nlme\", please install the package or switch to a different method.", call. = FALSE) } }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Two cluster variables ####

  if (isTRUE(ncol(as.data.frame(cluster)) == 2L && method == "aov")) { stop("Please specify \"lme4\" or \"nlme\" for the argument 'method' when specifying two cluster variables.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## One dependent variable ####

  if (isTRUE(is.null(dim(x)))) {

    #...................
    ### Variable with non-zero variance ####
    if (isTRUE(var(x, na.rm = TRUE) != 0L)) {

      #-----------------
      ##### One cluster variable
      if (isTRUE(ncol(as.data.frame(cluster)) == 1L)) {

        ###### ICC using aov() function
        if (isTRUE(method == "aov")) {

          # Estimate model
          mod <- aov(x ~ 1 + Error(as.factor(cluster)))

          # Model summary
          mod.summary <- summary(mod)

          # Between-cluster variance
          var.u <- unname(unlist(mod.summary[[1L]])["Mean Sq"])

          # Within-cluster variance
          var.r <- unname(unlist(mod.summary[[2L]])["Mean Sq"])

          # Total variance
          var.total <- var.u + var.r

        ###### Variance components lmer() function
        } else if (isTRUE(method == "lme4")) {

          # Estimate model
          mod <- suppressMessages(lme4::lmer(x ~ 1 + (1|cluster), REML = REML, control = lme4::lmerControl(optimizer = "bobyqa")))

          # Variance components
          vartab <- as.data.frame(suppressMessages(lme4::VarCorr(mod)))

          # Between-cluster variance
          var.u <- vartab[vartab$grp == "cluster", "vcov"]

          # Within-cluster variance
          var.r <- vartab[vartab$grp == "Residual", "vcov"]

          # Total variance
          var.total <- var.u + var.r

        ###### Variance components lme() function
        } else if (isTRUE(method == "nlme")) {

          # REML or ML
          ifelse(isTRUE(REML), REML <- "REML", REML <- "ML")

          # Estimate model
          mod <- suppressMessages(nlme::lme(x ~ 1, random = ~1 | cluster, na.action = na.omit, method = REML))

          # Variance components
          vartab <- nlme::VarCorr(mod)

          # Between-cluster variance
          var.u <- as.numeric(vartab["(Intercept)", "Variance"])

          # Within-cluster variance
          var.r <- as.numeric(vartab["Residual", "Variance"])

          # Total variance
          var.total <- var.u + var.r

        }

        ###### ICC
        if (isTRUE(method %in% c("lme4", "nlme"))) {

          # ICC(1)
          if (isTRUE(type == "1a")) {

            object <- var.u / var.total

          # ICC(2)
          } else if (isTRUE(type == "2")) {

            # Intraclass correlation coefficient, ICC(2)
            object <- var.u / (var.u + var.r / mean(table(cluster)))

          }

          if (isTRUE(object < 0L)) { object <- 0L }

        }

      #-----------------
      ##### Two cluster variables
      } else if (isTRUE(ncol(as.data.frame(cluster)) == 2L)) {

        ###### ICC using lmer() function
        if (isTRUE(method == "lme4")) {

          # Estimate model
          mod <- suppressMessages(lme4::lmer(x ~ 1 + (1|l3.cluster/l2.cluster), REML = REML, control = lme4::lmerControl(optimizer = "bobyqa")))

          # Variance components
          vartab <- as.data.frame(suppressMessages(lme4::VarCorr(mod)))

          # Level 3 Between-cluster variance
          var.v <- vartab[vartab$grp == "l3.cluster", "vcov"]

          # Level 2 Between-cluster variance
          var.u <- vartab[vartab$grp == "l2.cluster:l3.cluster", "vcov"]

          # Level 1 Within-cluster variance
          var.r <- vartab[vartab$grp == "Residual", "vcov"]

          # Total variance
          var.total <- var.v + var.u + var.r

        ###### ICC using lme() function
        } else if (isTRUE(method == "nlme")) {

          # REML or ML
          ifelse(isTRUE(REML), REML <- "REML", REML <- "ML")

          # Estimate model
          mod <- suppressMessages(nlme::lme(x ~ 1, random = ~1 | l3.cluster/l2.cluster, na.action = na.omit, method = REML))

          # Variance components
          vartab <- nlme::VarCorr(mod)

          # Level 3 Between-cluster variance
          var.v <- as.numeric(vartab[2L, "Variance"])

          # Level 2 Between-cluster variance
          var.u <- as.numeric(vartab[4L, "Variance"])

          # Level 1 Within-cluster variance
          var.r <- as.numeric(vartab["Residual", "Variance"])

          # Total variance
          var.total <- var.v + var.u + var.r

        }

        # ICC(1), proportion of variance
        if (isTRUE(type == "1a")) {

          icc.l3 <- var.v / var.total
          icc.l2 <- var.u / var.total

        # ICC(1), estimate of the expected correlation
        } else if (isTRUE(type == "1b")) {

          icc.l3 <- var.v / var.total
          icc.l2 <- var.v + var.u / var.total

        # ICC(2)
        } else if (isTRUE(type == "2")) {

          # Average cluster size
          cluster.size.l2 <- mean(table(l2.cluster))
          cluster.size.l3 <- mean(table(cluster[which(!duplicated(cluster[, 2])), 1L]))

          # Formula 10.25, Hox et al. (2018, p. 185) and Formula 8.8, Raudenbush and Bryk (2002, p. 230)
          icc.l3 <- var.v / (var.v + var.u / cluster.size.l3 + var.r / (cluster.size.l2 * cluster.size.l3))

          # Formula 10.27, Hox et al. (2018, p. 186)
          icc.l2 <- var.u / (var.u + var.r / cluster.size.l2)

        }

        object <- c(L3 = icc.l3, L2 = icc.l2)

      }

    #...................
    ### Variable with non-zero variance ####
    } else {

      #-----------------
      ##### One cluster variable
      if (isTRUE(ncol(as.data.frame(cluster)) == 1L)) {

        object <- NA

      #-----------------
      ##### Two cluster variables
      } else {

        object <- c(L3 = NA, L2 = NA)

      }

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## More than one dependent variable ####
  } else {

    object <- sapply(x, function(y) misty::multilevel.icc(y, data = NULL, cluster = cluster, type = type, method = method,
                                                          REML = REML, as.na = NULL, check = FALSE))

  }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  return(object)

}
