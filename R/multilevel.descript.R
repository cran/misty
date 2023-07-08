#' Multilevel Descriptive Statistics
#'
#' This function computes descriptive statistics for multilevel data, e.g. average
#' cluster size, variance components, intraclass correlation coefficient, design
#' effect, and effective sample size.
#'
#' Note that this function is restricted to two-level models.
#'
#' @param x           a vector, matrix or data frame.
#' @param cluster     either a character string indicating the variable name of
#'                    the cluster variable in 'x' or a vector representing the
#'                    nested grouping structure (i.e., group or cluster variable).
#' @param print       a character string or character vector indicating which
#'                    results to show on the console, i.e. \code{"all"} for
#'                    variances and standard deviations, \code{"var"} (default)
#'                    for variances, or \code{"sd"} for standard deviations within
#'                    and between clusters.
#' @param method      a character string indicating the method used to estimate
#'                    intraclass correlation coefficients, i.e., \code{"aov"} ICC
#'                    estimated using the \code{aov} function, \code{"lme4"} (default)
#'                    ICC estimated using the \code{lmer} function in the \pkg{lme4}
#'                    package, \code{"nlme"} ICC estimated using the \code{lme} function
#'                    in the \pkg{nlme} package.
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
#' @param write       a character string for writing the results into a Excel file
#'                    naming a file with or without file extension '.xlsx', e.g.,
#'                    \code{"Results.xlsx"} or \code{"Results"}.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#' @param output      logical: if \code{TRUE}, output is shown on the console.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{write.result}}, \code{\link{multilevel.icc}}, \code{\link{descript}}
#'
#' @references
#' Hox, J., Moerbeek, M., & van de Schoot, R. (2018). \emph{Multilevel analysis:
#' Techniques and applications} (3rd. ed.). Routledge.
#'
#' Snijders, T. A. B., & Bosker, R. J. (2012). \emph{Multilevel analysis: An
#' introduction to basic and advanced multilevel modeling} (2nd ed.). Sage Publishers.
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#' \tabular{ll}{
#' \code{call} \tab function call \cr
#' \code{type} \tab type of analysis \cr
#' \code{data} \tab data frame specified in \code{x} including the group variable
#'              specified in \code{cluster} \cr
#' \code{args} \tab specification of function arguments \cr
#' \code{result} \tab list with result tables \cr
#' }
#'
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries: function call (\code{call}), type of analysis \code{type}, matrix or
#' data frame specified in \code{x} (\code{data}), specification of function arguments
#' (\code{args}), and list with results (\code{result}).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load data set "Demo.twolevel" in the lavaan package
#' data("Demo.twolevel", package = "lavaan")
#'
#' #---------------------------
#' # Cluster variable specification
#'
#' # Cluster variable 'cluster' in 'x'
#' multilevel.descript(Demo.twolevel[, c("y1", "cluster")], cluster = "cluster")
#'
#' # Cluster variable 'cluster' not in 'x'
#' multilevel.descript(Demo.twolevel$y1, cluster = Demo.twolevel$cluster)
#'
#' #---------------------------
#'
#' # Multilevel descriptive statistics for y1
#' multilevel.descript(Demo.twolevel$y1, cluster = Demo.twolevel$cluster)
#'
#' # Multilevel descriptive statistics for y1, print variance and standard deviation
#' multilevel.descript(Demo.twolevel$y1, cluster = Demo.twolevel$cluster, print = "all")
#'
#' # Multilevel descriptive statistics for y1, print ICC with 5 digits
#' multilevel.descript(Demo.twolevel$y1, cluster = Demo.twolevel$cluster, icc.digits = 5)
#'
#' # Multilevel descriptive statistics for y1
#' # use lme() function in the nlme package to estimate ICC
#' multilevel.descript(Demo.twolevel$y1, cluster = Demo.twolevel$cluster, method = "nlme")
#'
#' # Multilevel descriptive statistics for y1, y2, y3, w1, and w2
#' multilevel.descript(Demo.twolevel[, c("y1", "y2", "y3", "w1", "w2")],
#'                     cluster = Demo.twolevel$cluster)
#'
#' # Write Results into a Excel file
#' multilevel.descript(Demo.twolevel[, c("y1", "y2", "y3", "w1", "w2")],
#'                     cluster = Demo.twolevel$cluster, write = "Multilevel_Descript.xlsx")
#'
#' result <- multilevel.descript(Demo.twolevel[, c("y1", "y2", "y3", "w1", "w2")],
#'                               cluster = Demo.twolevel$cluster, output = FALSE)
#' write.result(result, "Multilevel_Descript.xlsx")
#' }
multilevel.descript <- function(x, cluster, print = c("all", "var", "sd"),
                                method = c("aov", "lme4", "nlme"),
                                REML = TRUE, digits = 2, icc.digits = 3, as.na = NULL,
                                write = NULL, check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'x' is missing
  if (isTRUE(missing(x))) { stop("Please specify a vector, matrix or data frame for the argument 'x'.", call. = FALSE) }

  # Check if input 'x' is NULL
  if (isTRUE(is.null(x))) { stop("Input specified for the argument 'x' is NULL.", call. = FALSE) }

  # Vector, matrix or data frame for the argument 'x'?
  if (isTRUE(!is.atomic(x) && !is.matrix(x) && !is.data.frame(x))) { stop("Please specify a numeric vector, matrix or data frame with numeric variables for the argument 'x'.", call. = FALSE) }

  # Check if input 'cluster' is missing
  if (isTRUE(missing(cluster))) { stop("Please specify a vector representing the nested grouping structure for the argument 'cluster'.", call. = FALSE) }

  # Check if input 'cluster' is NULL
  if (isTRUE(is.null(cluster))) { stop("Input specified for the argument 'cluster is NULL.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data and Arguments ---------------------------------------------------------

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
  ## As data frame ####

  x <- as.data.frame(x, stringsAsFactors = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) {

    x <- misty::as.na(x, na = as.na, check = check)

    # Variable with missing values only
    x.miss <- vapply(x, function(y) all(is.na(y)), FUN.VALUE = logical(1L))
    if (isTRUE(any(x.miss))) {

      stop(paste0("After converting user-missing values into NA, following variables are completely missing: ", paste(names(which(x.miss)), collapse = ", ")), call. = FALSE)

    }

  }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # Check input 'cluster'
    if (isTRUE(is.null(dim(x)))) {

      # Length of 'x' and 'cluster'
      if (isTRUE(length(x) != length(cluster))) { stop("Length of the vector 'x' does not match with the length of the cluster variable 'cluster'.", call. = FALSE) }

    } else {

      # Length of 'x' and 'cluster'
      if (isTRUE(nrow(x) != length(cluster))) { stop("Number of rows in 'x' does not match with the length of the cluster variable 'cluster'.", call. = FALSE) }

    }

    # Check input 'cluster'
    if (isTRUE(length(unique(na.omit(cluster))) == 1L)) { stop("There is only one group represented in the cluster variable 'cluster'.", call. = FALSE) }

    # Check input 'print'
    if (isTRUE(!all(print %in% c("all", "var", "sd")))) { stop("Character strings in the argument 'print' do not all match with \"var\" or \"sd\".", call. = FALSE) }

    # Check input 'method'
    if (isTRUE(any(!method %in% c("aov", "lme4", "nlme")))) { stop("Character string in the argument 'method' does not match with \"aov\", \"lme4\", or \"nlme\".", call. = FALSE) }

    # Check input 'REML'
    if (isTRUE(!is.logical(REML))) { stop("Please specify TRUE or FALSE for the argument 'REML'", call. = FALSE) }

    # Check digits argument
    if (isTRUE(digits %% 1L != 0L || digits < 0L)) { stop("Specify a positive integer value for the argument 'digits'.", call. = FALSE) }

    # Check icc.digits argument
    if (isTRUE(icc.digits %% 1L != 0L || icc.digits < 0L)) { stop("Specify a positive integer value for the argument 'icc.digits'.", call. = FALSE) }

    # Check input 'output'
    if (isTRUE(!is.logical(output))) { stop("Please specify TRUE or FALSE for the argument 'output'", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Data and Arguments ---------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Argument method ####

  if (isTRUE(all(c("aov", "lme4", "nlme") %in% method))) {

    if (isTRUE(nzchar(system.file(package = "lme4")))) {

      method <- "lme4"

    } else {

      method <- "aov"

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Print ####

  if (isTRUE(all(c("all", "var", "sd") %in% print))) { print <- "var" }

  if (isTRUE(length(print) == 1L && "all" %in% print)) { print <- c("var", "sd") }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  # No. of observations
  no.obs <- vapply(x, function(y) length(na.omit(y)), FUN.VALUE = 1L)

  # No. of missing values
  no.miss <- vapply(x, function(y) sum(is.na(y)), FUN.VALUE = 1L)

  # No. of clusters
  no.cluster <- vapply(x, function(y) length(unique(na.omit(cbind(y, cluster))[, "cluster"])), FUN.VALUE = 1L)

  # Average cluster size
  m.cluster.size <- vapply(x, function(y) mean(table(na.omit(cbind(y, cluster))[, "cluster"])), FUN.VALUE = double(1L))

  # Standard deviation cluster size
  sd.cluster.size <- vapply(x, function(y) sd(table(na.omit(cbind(y, cluster))[, "cluster"])), FUN.VALUE = double(1L))

  # Minimum cluster size
  min.cluster.size <- vapply(x, function(y) min(table(na.omit(cbind(y, cluster))[, "cluster"])), FUN.VALUE = 1L)

  # Maximum cluster size
  max.cluster.size <- vapply(x, function(y) max(table(na.omit(cbind(y, cluster))[, "cluster"])), FUN.VALUE = 1L)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## One dependent variable ####

  if (isTRUE(ncol(x) == 1L)) {

    #............
    ### Within Variable
    if (isTRUE(any(tapply(x, cluster, var) != 0L))) {

      #### Variable with non-zero variance
      if (isTRUE(var(x, na.rm = TRUE) != 0L)) {

        # ICC using aov() function
        switch(method, aov = {

          # Estimate model
          mod <- aov(unlist(x) ~ 1 + Error(as.factor(cluster)))

          # Model summary
          mod.summary <- summary(mod)

          # Mean
          mean.x <- mean(tapply(unlist(x), cluster, mean, na.rm = TRUE))

          # Mean Squared Error Between
          var.b <- unname(unlist(mod.summary[[1L]])["Mean Sq"])
          sd.b <- sqrt(var.b)

          # Mean Squared Error Within
          var.w <- unname(unlist(mod.summary[[2L]])["Mean Sq"])
          sd.w <- sqrt(var.w)

          # ICC(1)
          icc1 <- unname((var.b - var.w) / (var.b + ((m.cluster.size - 1L) * var.w)))

          if (isTRUE(icc1 < 0L)) { icc1 <- 0L }

          # ICC(2)
          icc2 <- unname((var.b - var.w) / var.b)

          if (isTRUE(icc2 < 0L)) { icc2 <- 0L }

        # ICC using lmer() function
        }, lme4 = {

          # Estimate model
          mod <- suppressMessages(lme4::lmer(unlist(x) ~ 1 + (1|cluster), REML = REML, control = lme4::lmerControl(optimizer = "bobyqa")))

          # Mean
          mean.x <- unname(lme4::fixef(mod))

          # Variance components
          vartab <- as.data.frame(suppressMessages(lme4::VarCorr(mod)))

          # Between-cluster variance
          var.b <- vartab[vartab$grp == "cluster", "vcov"]
          # Between-cluster standard deviation
          sd.b <- sqrt(var.b)

          # Within-cluster variance
          var.w <- vartab[vartab$grp == "Residual", "vcov"]
          # Within-cluster standard deviation
          sd.w <- sqrt(var.w)

          # ICC(1)
          icc1 <- var.b / (var.b + var.w)

          # ICC(2)
          icc2 <- unname(var.b / (var.b + var.w / m.cluster.size))

        # ICC using lme() function
        }, nlme = {

          # REML or ML
          ifelse(isTRUE(REML), REML <- "REML", REML <- "ML")

          # Estimate model
          mod <- nlme::lme(x ~ 1, random = ~1 | cluster, data = data.frame(x, cluster = cluster), na.action = na.omit, method = REML)

          # Mean
          mean.x <- unname(lme4::fixef(mod))

          # Variance components
          vartab <- nlme::VarCorr(mod)

          # Between-cluster variance
          var.b <- as.numeric(vartab["(Intercept)", "Variance"])
          # Between-cluster standard deviation
          sd.b <- sqrt(var.b)

          # Within-cluster variance
          var.w <- as.numeric(vartab["Residual", "Variance"])
          # Within-cluster standard deviation
          sd.w <- sqrt(var.w)

          # ICC(1)
          icc1 <- var.b / (var.b + var.w)

          # ICC(2)
          icc2 <- unname(var.b / (var.b + var.w / m.cluster.size))

        })

      #### Variable with zero variance
      } else {

        object <- NA

      }

    #............
    ### Between Variable
    } else {

      # Mean
      mean.x <- mean(x[which(!duplicated(cluster)), ], na.rm = TRUE)
      # Variance
      var.b <- var(x[which(!duplicated(cluster)), ], na.rm = TRUE)
      # Standard deviation
      sd.b <- sd(x[which(!duplicated(cluster)), ], na.rm = TRUE)

      var.w <- sd.w <- icc1 <- icc2 <- deff <- deff.sqrt <- n.effect <- NA

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## More than one dependent variable ####
  } else {

    object <- apply(x, 2L, function(y) misty::multilevel.descript(y, cluster, method = method, REML = REML,
                                                                  digits = digits, icc.digits = icc.digits,
                                                                  as.na = NULL, check = FALSE, output = FALSE))

    # Extract results
    mean.x <- sapply(object, function(y) y$result$mean)
    var.w <- sapply(object, function(y) y$result$var.w)
    sd.w <- sapply(object, function(y) y$result$sd.w)
    var.b <- sapply(object, function(y) y$result$var.b)
    sd.b <- sapply(object, function(y) y$result$sd.b)
    icc1 <- sapply(object, function(y) y$result$icc1)
    icc2 <- sapply(object, function(y) y$result$icc2)

  }

  # Design effect
  deff <- 1L + icc1*(m.cluster.size - 1L)

  deff.sqrt <- sqrt(deff)

  # Effective sample size
  n.effect <- no.obs / deff

  #_____________________________________________________________________________
  #
  # Return object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "multilevel.descript",
                 data = data.frame(x = x, cluster = cluster, stringsAsFactors = FALSE),
                 args = list(print = print, method = method, REML = REML,
                             digits = digits, icc.digits = icc.digits, as.na = as.na,
                             check = check, output = output),
                 result = list(no.obs = no.obs, no.miss = no.miss, no.cluster = no.cluster,
                               m.cluster.size = m.cluster.size, sd.cluster.size = sd.cluster.size,
                               min.cluster.size = min.cluster.size, max.cluster.size = max.cluster.size,
                               mean = mean.x, var.w = var.w, var.b = var.b, sd.w = sd.w, sd.b = sd.b,
                               icc1 = icc1, icc2 = icc2, deff = deff, deff.sqrt = deff.sqrt, n.effect = n.effect))

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Write Result ---------------------------------------------------------------

  if (isTRUE(!is.null(write))) { misty::write.result(object, file = write) }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
