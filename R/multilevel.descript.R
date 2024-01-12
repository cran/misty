#' Multilevel Descriptive Statistics for Two-Level and Three-Level Data
#'
#' This function computes descriptive statistics for two-level and three-level
#' multilevel data, e.g. average cluster size, variance components, intraclass
#' correlation coefficient, design effect, and effective sample size.
#'
#' @param ...         a numeric vector, matrix, or data frame. Alternatively, an
#'                    expression indicating the variable names in \code{data}.
#'                    Note that the operators \code{.}, \code{+}, \code{-},
#'                    \code{~}, \code{:}, \code{::}, and \code{!} can also be
#'                    used to select variables, see 'Details' in the
#'                    \code{\link{df.subset}} function.
#' @data              a data frame when specifying one or more variables in the
#'                    argument \code{...}. Note that the argument is \code{NULL}
#'                    when specifying a numeric vector, matrix, or data frame for
#'                    the argument \code{...}.
#' @param cluster     a character string indicating the name of the cluster
#'                    variable in \code{...} or \code{data} for two-level data,
#'                    a character vector indicating the names of the cluster
#'                    variables in \code{...} for three-level data, or a vector
#'                    or data frame representing the nested grouping structure
#'                    (i.e., group or cluster variables). Alternatively, a
#'                    character string or character vector indicating the variable
#'                    name(s) of the cluster variable(s) in \code{data}. Note that
#'                    the cluster variable at Level 3 come first in a three-level
#'                    model, i.e., \code{cluster = c("level3", "level2")}.
#' @type              a character string indicating the type of intraclass
#'                    correlation coefficient, i.e., \code{type = "1a"} (default)
#'                    for ICC(1) representing the propotion of variance at Level
#'                    2 and Level 3, \code{type = "1b"} representing an estimate
#'                    of the expected correlation between two randomly chosen
#'                    elements in the same group when specifying a three-level
#'                    model (i.e., two cluster variables). See 'Details' in the
#'                    \code{\link{multilevel.icc}} function for the formula used
#'                    in this function.
#' @param method      a character string indicating the method used to estimate
#'                    intraclass correlation coefficients, i.e., \code{"aov"} ICC
#'                    estimated using the \code{aov} function, \code{"lme4"} (default)
#'                    ICC estimated using the \code{lmer} function in the \pkg{lme4}
#'                    package, \code{"nlme"} ICC estimated using the \code{lme} function
#'                    in the \pkg{nlme} package.
#' @param print       a character string or character vector indicating which
#'                    results to show on the console, i.e. \code{"all"} for
#'                    variances and standard deviations, \code{"var"} (default)
#'                    for variances, or \code{"sd"} for standard deviations within
#'                    and between clusters.
#' @param REML        logical: if \code{TRUE} (default), restricted maximum likelihood
#'                    is used to estimate the null model when using the \code{lmer()}
#'                    function in the \pkg{lme4} package or the \code{lme()} function in
#'                    the \pkg{nlme} package.
#' @param digits      an integer value indicating the number of decimal places to
#'                    be used.
#' @param icc.digits  an integer indicating the number of decimal places to be used
#'                    for displaying intraclass correlation coefficients.
#' @param as.na       a numeric vector indicating user-defined missing values,
#'                    i.e. these values are converted to \code{NA} before conducting
#'                    the analysis. Note that \code{as.na()} function is only applied
#'                    to \code{x} but not to \code{cluster}.
#' @param write       a character string naming a file for writing the output into
#'                    either a text file with file extension \code{".txt"} (e.g.,
#'                    \code{"Output.txt"}) or Excel file with file extention
#'                    \code{".xlsx"}  (e.g., \code{"Output.xlsx"}). If the file
#'                    name does not contain any file extension, an Excel file will
#'                    be written.
#' @param append      logical: if \code{TRUE} (default), output will be appended
#'                    to an existing text file with extension \code{.txt} specified
#'                    in \code{write}, if \code{FALSE} existing text file will be
#'                    overwritten.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#' @param output      logical: if \code{TRUE}, output is shown on the console.
#'
#' @details
#' \describe{
#' \item{\strong{Two-Level Model}}{In a two-level model, the intraclass
#' correlation coefficients, design effect, and the effective sample size are
#' computed based on the random intercept-only model:
#'
#'       \deqn{Y_{ij} = \gamma_{00} + u_{0j} + r_{ij}}
#'
#' where the variance in \eqn{Y} is decomposed into two independent components:
#' \eqn{\sigma^2_{u_{0}}}, which represents the variance at Level 2, and
#' \eqn{\sigma^2_{r}}, which represents the variance at Level 1 (Hox et al.,
#' 2018). For the computation of the intraclass correlation coefficients, see
#' 'Details' in the \code{\link{multilevel.icc}} function. The design effect
#' represents the effect of cluster sampling on the variance of parameter
#' estimation and is defined by the equation
#'
#' \deqn{deff = (\frac{SE_{Cluster}}{SE_{Simple}})^2 = 1 + \rho(J - 1)}
#'
#' where \eqn{SE_{Cluster}} is the standard error under cluster sampling,
#' \eqn{SE_{Simple}} is the standard error under simple random sampling,
#' \eqn{\rho} is the intraclass correlation coefficient, ICC(1), and
#' \eqn{J} is the average cluster size. The effective sample size is defined
#' by the equation:
#'
#' \deqn{N_{effective} = \frac{N{total}}{deff}}
#'
#' The effective sample size \eqn{N_{effective}} represents the equivalent total
#' sample size that we should use in estimating the standard error (Snijders &
#' Bosker, 2012).}
#'
#' \item{\strong{Three-Level Model}}{In a three-level model, the intraclass
#' correlation coefficients, design effect, and the effective sample size are
#' computed based on the random intercept-only model:
#'
#' \deqn{Y_{ijk} = \gamma_{000} + v_{0k} + u_{0jk} + r_{ijk}}
#'
#' where the variance in \eqn{Y} is decomposed into three independent components:
#' \eqn{\sigma^2_{v_{0}}}, which represents the variance at Level 3,
#' \eqn{\sigma^2_{u_{0}}}, which represents the variance at Level 2, and
#' \eqn{\sigma^2_{r}}, which represents the variance at Level 1 (Hox et al., 2018).
#' For the computation of the intraclass correlation coefficients, see 'Details'
#' in the \code{\link{multilevel.icc}} function. The design effect represents
#' the effect of cluster sampling on the variance of parameter estimation and is
#' defined by the equation
#'
#' \deqn{deff = (\frac{SE_{Cluster}}{SE_{Simple}})^2 = 1 + \rho_{L2}(J - 1) + \rho_{L3}(JK - 1)}
#'
#' where \eqn{\rho_{L2}} is the ICC(1) at Level 2, \eqn{\rho_{L3}} is the ICC(1)
#' at Level 3, \eqn{J} is the average cluster size at Level 2, and \eqn{K} is
#' the average cluster size at Level 3.}
#' }
#' }
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{multilevel.icc}}, \code{\link{descript}}, \code{\link{write.result}}
#'
#' @references
#' Hox, J., Moerbeek, M., & van de Schoot, R. (2018). \emph{Multilevel analysis:
#' Techniques and applications} (3rd. ed.). Routledge.
#'
#' Raudenbush, S. W., & Bryk, A. (2002). \emph{Hierarchical linear models: Applications
#' and data analysis methods} (2nd ed.). Sage Publications Inc.
#'
#' Snijders, T. A. B., & Bosker, R. J. (2012). \emph{Multilevel analysis: An
#' introduction to basic and advanced multilevel modeling} (2nd ed.). Sage Publishers.
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{data}}{data frame specified in \code{...} including the cluster
#'                    variable(s) specified in \code{cluster}}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{model.fit}}{fitted lavaan object (\code{mod.fit})}
#' \item{\code{result}}{list with result tables, i.e.,
#'                     \code{no.obs} for the number of observations
#'                     \code{no.no.miss} for the number of missing value,
#'                     \code{no.cluster.l2} and \code{no.cluster.l3} for the number of clusters at Level 2 and/or Level 3,
#'                     \code{m.cluster.size.l2} and \code{m.cluster.size.l3} for the average cluster size at Level 2 and/or Level 3,
#'                     \code{sd.cluster.size.l2} and \code{sd.cluster.size.l3} for the standard deviation of the cluster size at Level 2 and/or Level 3,
#'                     \code{min.cluster.size.l2} \code{min.cluster.size.l3} for the minimum cluster size at Level 2 and/or Level 3,
#'                     \code{max.cluster.size.l2} \code{max.cluster.size.l3} for the maximum cluster size at Level 2 and/or Level 3,
#'                     \code{mean.x} for the intercept of the multilevel model,
#'                     \code{var.r} for the variance within clusters,
#'                     \code{var.u} for the variance between Level 2 clusters,
#'                     \code{var.b} for the variance between Level 3 clusters,
#'                     \code{icc1.l2} and \code{icc1.l3} for ICC(1) at Level 2 and/or Level 3,
#'                     \code{icc2.l2} and \code{icc2.l3} for ICC(2) at Level 2 and/or Level 3,
#'                     \code{deff} for the design effect,
#'                     \code{deff.sqrt} for the square root of the design effect,
#'                      \code{n.effect} for the effective sample size}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load data set "Demo.twolevel" in the lavaan package
#' data("Demo.twolevel", package = "lavaan")
#'
#' #----------------------------------------------------------------------------
#' # Two-Level Data
#'
#' #..........
#' # Cluster variable specification
#'
#' # Example 1a: Cluster variable 'cluster'
#' multilevel.descript(Demo.twolevel[, c("y1", "cluster")], cluster = "cluster")
#'
#' # Example 1b: Cluster variable 'cluster' not in '...'
#' multilevel.descript(Demo.twolevel$y1, cluster = Demo.twolevel$cluster)
#'
#' # Example 1c: Alternative specification using the 'data' argument
#' multilevel.descript(y1, data = Demo.twolevel, cluster = "cluster")
#'
#' #---------------------------
#'
#' # Example 2: Multilevel descriptive statistics for 'y1'
#' multilevel.descript(Demo.twolevel$y1, cluster = Demo.twolevel$cluster)
#'
#' # Example 3: Multilevel descriptive statistics, print variance and standard deviation
#' multilevel.descript(Demo.twolevel$y1, cluster = Demo.twolevel$cluster, print = "all")
#'
#' # Example 4: Multilevel descriptive statistics, print ICC with 5 digits
#' multilevel.descript(Demo.twolevel$y1, cluster = Demo.twolevel$cluster, icc.digits = 5)
#'
#' # Example 5: Multilevel descriptive statistics
#' # use lme() function in the nlme package to estimate ICC
#' multilevel.descript(Demo.twolevel$y1, cluster = Demo.twolevel$cluster, method = "nlme")
#'
#' # Example 6a: Multilevel descriptive statistics for 'y1', 'y2', 'y3', 'w1', and 'w2'
#' multilevel.descript(Demo.twolevel[, c("y1", "y2", "y3", "w1", "w2")],
#'                       cluster = Demo.twolevel$cluster)
#'
#' # Example 6b: Alternative specification using the 'data' argument
#' multilevel.descript(y1:y3, w1, w2, data = Demo.twolevel, cluster = "cluster")
#'
#' #----------------------------------------------------------------------------
#' # Three-Level Data
#'
#' # Create arbitrary three-level data
#' Demo.threelevel <- data.frame(Demo.twolevel, cluster2 = Demo.twolevel$cluster,
#'                                              cluster3 = rep(1:10, each = 250))
#'
#' #..........
#' # Cluster variable specification
#'
#' # Example 7a: Cluster variables 'cluster' in '...'
#' multilevel.descript(Demo.threelevel[, c("y1", "cluster3", "cluster2")],
#'                     cluster = c("cluster3", "cluster2"))
#'
#' # Example 7b: Cluster variables 'cluster' not in '...'
#' multilevel.descript(Demo.threelevel$y1, cluster = Demo.threelevel[, c("cluster3", "cluster2")])
#'
#' # Example 7c: Alternative specification using the 'data' argument
#' multilevel.descript(y1, data = Demo.threelevel, cluster = c("cluster3", "cluster2"))
#'
#' #----------------------------------------------------------------------------
#'
#' # Example 8: Multilevel descriptive statistics for 'y1', 'y2', 'y3', 'w1', and 'w2'
#' multilevel.descript(y1:y3, w1, w2, data = Demo.threelevel, cluster = c("cluster3", "cluster2"))
#'
#' #----------------------------------------------------------------------------
#' # Write Results
#'
#' # Example 9a: Write Results into a text file
#' multilevel.descript(Demo.twolevel[, c("y1", "y2", "y3", "w1", "w2")],
#'                     cluster = Demo.twolevel$cluster, write = "Multilevel_Descript.txt")
#'
#' # Example 9b: Write Results into a Excel file
#' multilevel.descript(Demo.twolevel[, c("y1", "y2", "y3", "w1", "w2")],
#'                     cluster = Demo.twolevel$cluster, write = "Multilevel_Descript.xlsx")
#'
#' result <- multilevel.descript(Demo.twolevel[, c("y1", "y2", "y3", "w1", "w2")],
#'                               cluster = Demo.twolevel$cluster, output = FALSE)
#' write.result(result, "Multilevel_Descript.xlsx")
#' }
multilevel.descript <- function(..., data = NULL, cluster, type = c("1a", "1b"),
                                method = c("aov", "lme4", "nlme"), print = c("all", "var", "sd"),
                                REML = TRUE, digits = 2, icc.digits = 3, as.na = NULL,
                                write = NULL, append = TRUE, check = TRUE, output = TRUE) {

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

    # Data and cluster
    var.group <- .var.group(data = x, cluster = cluster)

    # Data
    if (isTRUE(!is.null(var.group$data)))  { x <- var.group$data }

    # Cluster variable
    if (isTRUE(!is.null(var.group$cluster))) { cluster <- var.group$cluster }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Cluster variables ####

  # Two cluser variables
  if (isTRUE(ncol(as.data.frame(cluster)) == 2L)) {

    l3.cluster <- cluster[, 1L]
    l2.cluster <- cluster[, 2L]

    no.clust <- "two"

  # One cluser variables
  } else {

    no.clust <- "one"

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

    # Check input 'print'
    if (isTRUE(!all(print %in% c("all", "var", "sd")))) { stop("Character strings in the argument 'print' do not all match with \"var\" or \"sd\".", call. = FALSE) }

    # Check input 'type'
    if (isTRUE(!all(type %in% c("1a", "1b")))) { stop("Character strings in the argument 'type' do not all match with \"1a\" or \"1b\".", call. = FALSE) }

    # Check input 'method'
    if (isTRUE(any(!method %in% c("aov", "lme4", "nlme")))) { stop("Character string in the argument 'method' does not match with \"aov\", \"lme4\", or \"nlme\".", call. = FALSE) }

    # Check input 'REML'
    if (isTRUE(!is.logical(REML))) { stop("Please specify TRUE or FALSE for the argument 'REML'", call. = FALSE) }

    # Check digits argument
    if (isTRUE(digits %% 1L != 0L || digits < 0L)) { stop("Specify a positive integer value for the argument 'digits'.", call. = FALSE) }

    # Check icc.digits argument
    if (isTRUE(icc.digits %% 1L != 0L || icc.digits < 0L)) { stop("Specify a positive integer value for the argument 'icc.digits'.", call. = FALSE) }

    # Check input 'append'
    if (isTRUE(!is.logical(append))) { stop("Please specify TRUE or FALSE for the argument 'append'.", call. = FALSE) }

    # Check input 'output'
    if (isTRUE(!is.logical(output))) { stop("Please specify TRUE or FALSE for the argument 'output'", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Data and Arguments ---------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Argument type ####

  if (isTRUE(all(c("1a", "1b") %in% type))) { type <- "1a" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Argument method ####

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

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Print ####

  if (isTRUE(all(c("all", "var", "sd") %in% print))) { print <- "var" }

  if (isTRUE(length(print) == 1L && "all" %in% print)) { print <- c("var", "sd") }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## One variable ####

  if (isTRUE(is.null(dim(x)))) {

    #...................
    ### Variable ####

    ##### One cluster variable
    if (isTRUE(no.clust == "one")) {

      # Level 1 Variable
      if (isTRUE(any(na.omit(as.vector(tapply(x, cluster, var, na.rm = TRUE))) != 0L))) {

        vartype <- "L1"

      # Level 2 Variable
      } else {

        vartype = "L2"

      }

    ##### Two cluster variable s
    } else if (isTRUE(no.clust == "two")) {

      # Level 1 Variable
      if (isTRUE(any(na.omit(as.vector(tapply(x, apply(cluster, 1L, paste, collapse = ""), var, na.rm = TRUE))) != 0L))) {

        vartype <- "L1"

      # Level 2 Variable
      } else if (isTRUE(all(na.omit(as.vector(tapply(x, apply(cluster, 1L, paste, collapse = ""), var, na.rm = TRUE))) == 0L) && any(as.vector(tapply(x, cluster[, 1L], var, na.rm = TRUE)) != 0L))) {

        vartype <- "L2"

      # Level 3 Variable
      } else if (isTRUE(all(na.omit(as.vector(tapply(x, apply(cluster, 1L, paste, collapse = ""), var, na.rm = TRUE))) == 0L) && all(na.omit(as.vector(tapply(x, cluster[, 1L], var, na.rm = TRUE))) == 0L))) {

        vartype <- "L3"

      }

    }

    #...................
    ### Descriptive Statistics ####

    #...................
    #### One cluster variable ####

    if (isTRUE(no.clust == "one")) {

      #...................
      #### Variable with non-zero variance
      if (isTRUE(var(x, na.rm = TRUE) != 0L)) {

        #............
        ##### Level 1 Variable
        switch(vartype, L1 = {

          # No. of observations
          no.obs <- length(na.omit(x))

          # No. of missing values
          no.miss <- no.obs - nrow(na.omit(data.frame(x, cluster)))

          # No. of clusters
          no.cluster.l2 <- length(na.omit(unique(cluster)))

          # Average cluster size
          m.cluster.size.l2 <- mean(table(cluster))

          # Standard deviation cluster size
          sd.cluster.size.l2 <- sd(table(cluster))

          # Minimum cluster size
          min.cluster.size.l2 <- min(table(cluster))

          # Maximum cluster size
          max.cluster.size.l2 <- max(table(cluster))

          # Objects not used in a two-level model
          no.cluster.l3 <- m.cluster.size.l3 <- sd.cluster.size.l3 <- min.cluster.size.l3 <- max.cluster.size.l3 <- NA

          ###### ICC using aov() function
          if (isTRUE(method == "aov")) {

            # Estimate model
            mod <- aov(x ~ 1 + Error(as.factor(cluster)))

            # Model summary
            mod.summary <- summary(mod)

            # Mean
            mean.x <- mean(tapply(unlist(x), cluster, mean, na.rm = TRUE))

            # Mean Squared Error Between
            var.u <- unname(unlist(mod.summary[[1L]])["Mean Sq"])

            # Mean Squared Error Within
            var.r <- unname(unlist(mod.summary[[2L]])["Mean Sq"])

            # Total variance
            var.total <- var.u + var.r

          ###### Variance components lmer() function
          } else if (isTRUE(method == "lme4")) {

            # Estimate model
            mod <- suppressMessages(lme4::lmer(x ~ 1 + (1|cluster), REML = REML, control = lme4::lmerControl(optimizer = "bobyqa")))

            # Mean
            mean.x <- unname(lme4::fixef(mod))

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

            # Mean
            mean.x <- unname(lme4::fixef(mod))

            # Variance components
            vartab <- nlme::VarCorr(mod)

            # Between-cluster variance
            var.u <- as.numeric(vartab["(Intercept)", "Variance"])

            # Within-cluster variance
            var.r <- as.numeric(vartab["Residual", "Variance"])

            # Total variance
            var.total <- var.u + var.r

          }

          # Intraclass correlation coefficient
          icc1.l2 <- var.u / var.total
          icc2.l2 <- var.u / (var.u + var.r / mean(table(cluster)))

          if (isTRUE(icc1.l2 < 0L)) { icc1.l2 <- 0L }
          if (isTRUE(icc2.l2 < 0L)) { icc2.l2 <- 0L }

          # Design effect
          deff <- 1L + icc1.l2*(m.cluster.size.l2 - 1L)

          # Square root design effect
          deff.sqrt <- sqrt(deff)

          # Effective sample size
          n.effect <- no.obs / deff

          # Standard deviation of variance components
          sd.u <- sqrt(var.u)
          sd.r <- sqrt(var.r)

          # Objects not used for a Level 1 variable in a two-level model
          icc1.l3 <- icc2.l3 <- var.v <- sd.v <- NA

        #............
        ##### Level 2 Variable
        }, L2 = {

          # Data
          data <- data.frame(x, cluster)

          # Level 2 data
          x <- data[which(!duplicated(cluster)), 1L]

          # Level 3 cluster
          cluster <- data[which(!duplicated(cluster)), 2L]

          #...................
          #### Level 2 Variable

          # No. of clusters
          no.cluster.l2 <- length(x)

          # Objects not used in a two-level model
          no.cluster.l3 <- m.cluster.size.l3 <- sd.cluster.size.l3 <- min.cluster.size.l3 <- max.cluster.size.l3 <- m.cluster.size.l2 <- sd.cluster.size.l2 <- min.cluster.size.l2 <- max.cluster.size.l2 <- no.obs <- no.miss <- NA

          # Mean
          mean.x <- mean(x, na.rm = TRUE)
          # Variance
          var.u <- var(x, na.rm = TRUE)
          # Standard deviation
          sd.u <- sqrt(var.u)

          # Objects not used for a Level 2 variable in a two-level model
          var.v <- sd.v <- var.r <- sd.r <- icc1.l3 <- icc2.l3 <- icc1.l2 <- icc2.l2 <- deff <- deff.sqrt <- n.effect <- NA

        })

      #...................
      #### Variable with zero variance
      } else {

        mean.x <-  deff <- deff.sqrt <- n.effect <- icc1.l3 <- icc2.l3 <- icc1.l2 <- icc2.l2 <- var.v <- sd.v <- var.u <- sd.u <- var.r <- sd.r <- no.cluster.l3 <- m.cluster.size.l3 <- sd.cluster.size.l3 <- min.cluster.size.l3 <- max.cluster.size.l3 <- NA

      }

    #...................
    #### Two cluster variables ####

    } else if (isTRUE(no.clust == "two")) {

      #...................
      #### Variable with non-zero variance
      if (isTRUE(var(x, na.rm = TRUE) != 0L)) {

        #............
        ##### Level 1 Variable
        switch(vartype, L1 = {

          # No. of observations
          no.obs <- length(x)

          # No. of missing values
          no.miss <- no.obs - nrow(na.omit(data.frame(x, cluster)))

          #...................
          #### Level 2

          # No. of clusters
          no.cluster.l2 <- length(na.omit(unique(cluster[, 2L])))

          # Average cluster size
          m.cluster.size.l2 <- mean(table(cluster[, 2L]))

          # Standard deviation cluster size
          sd.cluster.size.l2 <- sd(table(cluster[, 2L]))

          # Minimum cluster size
          min.cluster.size.l2 <- min(table(cluster[, 2L]))

          # Maximum cluster size
          max.cluster.size.l2 <- max(table(cluster[, 2L]))

          #...................
          #### Level 3

          # No. of clusters
          no.cluster.l3 <- length(na.omit(unique(cluster[, 1L])))

          # Average cluster size
          m.cluster.size.l3 <- mean(table(cluster[which(!duplicated(cluster[, 2])), 1L]))

          # Standard deviation cluster size
          sd.cluster.size.l3 <- sd(table(cluster[which(!duplicated(cluster[, 2])), 1L]))

          # Minimum cluster size
          min.cluster.size.l3 <- min(table(cluster[which(!duplicated(cluster[, 2])), 1L]))

          # Maximum cluster size
          max.cluster.size.l3 <- max(table(cluster[which(!duplicated(cluster[, 2])), 1L]))

          ###### ICC using lmer() function
          if (isTRUE(method == "lme4")) {

            # Estimate model
            mod <- suppressMessages(lme4::lmer(x ~ 1 + (1|l3.cluster/l2.cluster), REML = REML, control = lme4::lmerControl(optimizer = "bobyqa")))

            # Mean
            mean.x <- unname(lme4::fixef(mod))

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

            # Mean
            mean.x <- unname(lme4::fixef(mod))

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

            icc1.l3 <- var.v / var.total
            icc1.l2 <- var.u / var.total

            # ICC(1), estimate of the expected correlation
          } else if (isTRUE(type == "1b")) {

            icc1.l3 <- var.v / var.total
            icc1.l2 <- var.v + var.u / var.total

          }

          # ICC(1) Level 2, Formula 10.27, Hox et al. (2018), p. 186
          icc2.l2 <- var.u / (var.u + var.r / m.cluster.size.l2)

          # ICC(1) Level 3, Formula 10.25, Hox et al. (2018), p. 185
          icc2.l3 <- var.v / (var.v + var.u / m.cluster.size.l3 + var.r / (m.cluster.size.l2 * m.cluster.size.l3))

          # Design effect, see: https://www.dougjohnson.in/post/3-stage/
          deff <- 1 + (m.cluster.size.l2 - 1)*(var.u / var.total) + (m.cluster.size.l3*m.cluster.size.l2 - 1)*(var.v / var.total)

          # Square root design effect
          deff.sqrt <- sqrt(deff)

          # Effective sample size
          n.effect <- no.obs / deff

          # Standard deviation of variance components
          sd.v <- sqrt(var.v)
          sd.u <- sqrt(var.u)
          sd.r <- sqrt(var.r)

        #............
        ##### Level 2 Variable
        }, L2 = {

          # Data
          data <- data.frame(x, cluster)

          # Level 2 data
          x <- data[which(!duplicated(cluster[, 2L])), 1L]

          # Level 3 cluster
          cluster <- data[which(!duplicated(cluster[, 2L])), 2L]

          #...................
          #### Level 2

          # No. of clusters
          no.cluster.l2 <- length(x)

          # Average cluster size
          m.cluster.size.l2 <- NA

          # Standard deviation cluster size
          sd.cluster.size.l2 <- NA

          # Minimum cluster size
          min.cluster.size.l2 <- NA

          # Maximum cluster size
          max.cluster.size.l2 <- NA

          #...................
          #### Level 3

          # No. of clusters
          no.cluster.l3 <- length(na.omit(unique(cluster)))

          # Average cluster size
          m.cluster.size.l3 <- mean(table(cluster))

          # Standard deviation cluster size
          sd.cluster.size.l3 <- sd(table(cluster))

          # Minimum cluster size
          min.cluster.size.l3 <- min(table(cluster))

          # Maximum cluster size
          max.cluster.size.l3 <- max(table(cluster))

          # Objects not used for a Level 2 variable
          no.obs <- no.miss <- m.cluster.size.l2 <- sd.cluster.size.l2 <- min.cluster.size.l2 <- max.cluster.size.l2 <- NA

          ###### Variance components lmer() function
          if (isTRUE(method == "lme4")) {

            # Estimate model
            mod <- suppressMessages(lme4::lmer(x ~ 1 + (1|cluster), REML = REML, control = lme4::lmerControl(optimizer = "bobyqa")))

            # Mean
            mean.x <- unname(lme4::fixef(mod))

            # Variance components
            vartab <- as.data.frame(suppressMessages(lme4::VarCorr(mod)))

            # Level 3 Between-cluster variance
            var.v <- vartab[vartab$grp == "cluster", "vcov"]

            # Level 2 Between-cluster variance
            var.u <- vartab[vartab$grp == "Residual", "vcov"]

            # Total variance
            var.total <- var.v + var.u

          ###### Variance components lme() function
          } else if (isTRUE(method == "nlme")) {

            # REML or ML
            ifelse(isTRUE(REML), REML <- "REML", REML <- "ML")

            # Mean
            mean.x <- unname(lme4::fixef(mod))

            # Estimate model
            mod <- suppressMessages(nlme::lme(x ~ 1, random = ~1 | cluster, na.action = na.omit, method = REML))

            # Variance components
            vartab <- nlme::VarCorr(mod)

            # Between-cluster variance
            var.v <- as.numeric(vartab["(Intercept)", "Variance"])

            # Within-cluster variance
            var.u <- as.numeric(vartab["Residual", "Variance"])

            # Total variance
            var.total <- var.v + var.u

          }

          # Intraclass correlation coefficient
          icc1.l3 <- var.v / var.total
          icc2.l3 <- var.v / (var.v + var.u / mean(table(cluster)))

          # Design effect
          deff <- 1L + icc1.l3*(mean(table(cluster)) - 1L)

          # Square root design effect
          deff.sqrt <- sqrt(deff)

          # Effective sample size
          n.effect <- no.cluster.l2 / deff

          # Standard deviation of variance components
          sd.v <- sqrt(var.v)
          sd.u <- sqrt(var.u)

          # Objects not used for a Level 2 variable in a three-level model
          icc1.l2 <- icc2.l2 <- var.r <- sd.r <- NA

        #............
        ##### Level 3 Variable
        }, L3 = {

          # Data
          data <- data.frame(x, cluster)

          # Level 2 data
          x <- data[which(!duplicated(cluster[, 1L])), 1L]

          # Level 3 cluster
          cluster <- data[which(!duplicated(cluster[, 1L])), 2L]

          #...................
          #### Level 3

          # No. of clusters
          no.cluster.l3 <- length(x)

          # Mean
          mean.x <- mean(x, na.rm = TRUE)
          # Variance
          var.v <- var(x, na.rm = TRUE)
          # Standard deviation
          sd.v <- sqrt(var.v)

          # Objects not used for a Level 3 variable in a three-level model
          deff <- deff.sqrt <- n.effect <- icc1.l3 <- icc2.l3 <- icc1.l2 <- icc2.l2 <- var.u <- sd.u <- var.r <- sd.r <- m.cluster.size.l3 <- sd.cluster.size.l3 <- min.cluster.size.l3 <- max.cluster.size.l3 <- no.cluster.l2 <- m.cluster.size.l2 <- sd.cluster.size.l2 <- min.cluster.size.l2 <- max.cluster.size.l2 <- no.obs <- no.miss <-NA

        })

      #...................
      #### Variable with zero variance
      } else {

        mean.x <-  deff <- deff.sqrt <- n.effect <- icc1.l3 <- icc2.l3 <- icc1.l2 <- icc2.l2 <- var.v <- sd.v <- var.u <- sd.u <- var.r <- sd.r <- no.cluster.l3 <- m.cluster.size.l3 <- sd.cluster.size.l3 <- min.cluster.size.l3 <- max.cluster.size.l3 <- NA

      }

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## More than one variable ####
  } else {

    object <- apply(x, 2L, function(y) misty::multilevel.descript(y, data = NULL, cluster = cluster, method = method,
                                                                  REML = REML, digits = digits, icc.digits = icc.digits,
                                                                  as.na = NULL, check = FALSE, output = FALSE))

    # Extract variable names
    outcome.names <- names(object)

    # Extract element names
    object.names <- unique(as.vector(sapply(object, function(y) names(y$result))))

    # Extract and assign result objects
    for (i in object.names) { assign(i, setNames(unlist(sapply(object, function(y) y$result[i])), nm = outcome.names)) }

  }

  #_____________________________________________________________________________
  #
  # Return object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "multilevel.descript",
                 data = data.frame(x = x, cluster = cluster, stringsAsFactors = FALSE),
                 no.cluster = no.clust,
                 args = list(print = print, type = type, method = method, REML = REML,
                             digits = digits, icc.digits = icc.digits, as.na = as.na,
                             write = write, append = append, check = check, output = output),
                 result = list(no.obs = no.obs, no.miss = no.miss,
                               no.cluster.l2 = no.cluster.l2, no.cluster.l3 = no.cluster.l3,
                               m.cluster.size.l2 = m.cluster.size.l2, m.cluster.size.l3 = m.cluster.size.l3,
                               sd.cluster.size.l2 = sd.cluster.size.l2, sd.cluster.size.l3 = sd.cluster.size.l3,
                               min.cluster.size.l2 = min.cluster.size.l2, min.cluster.size.l3 = min.cluster.size.l3,
                               max.cluster.size.l2 = max.cluster.size.l2, max.cluster.size.l3 = max.cluster.size.l3,
                               mean.x = mean.x, var.r = var.r, var.u = var.u, var.v = var.v, sd.r = sd.r, sd.u = sd.u, sd.v = sd.v,
                               icc1.l2 = icc1.l2, icc1.l3 = icc1.l3, icc2.l2 = icc2.l2, icc2.l3 = icc2.l3,
                               deff = deff, deff.sqrt = deff.sqrt, n.effect = n.effect))

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Write Result ---------------------------------------------------------------

  if (isTRUE(!is.null(write))) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Text file ####

    if (isTRUE(grepl("\\.txt", write))) {

      # Send R output to textfile
      sink(file = write, append = ifelse(isTRUE(file.exists(write)), append, FALSE), type = "output", split = FALSE)

      if (append && isTRUE(file.exists(write))) { write("", file = write, append = TRUE) }

      # Print object
      print(object, check = FALSE)

      # Close file connection
      sink()

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Excel file ####

    } else {

      misty::write.result(object, file = write)

    }

  }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
