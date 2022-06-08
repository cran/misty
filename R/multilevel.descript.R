#' Multilevel Descriptive Statistics
#'
#' This function computes descriptive statistics for multilevel data, e.g. average
#' cluster size, variance components, intraclass correlation coefficient,
#' design effect, and effective sample size.
#'
#' Note that this function is restricted to two-level models.
#'
#' @param x           a vector, matrix or data frame.
#' @param cluster     a vector representing the nested grouping structure (i.e.,
#'                    group or cluster variable).
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
#' \code{\link{write.result}}, \code{\link{multilevel.icc}}
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
#' entries: function call (\code{call}), type of analysis \code{type}, matrix or
#' data frame specified in \code{x} (\code{data}), specification of function arguments
#' (\code{args}), and list with results (\code{result}).
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
#' # Multilevel descriptive statistics for x1
#' multilevel.descript(dat$x1, cluster = dat$cluster)
#'
#' # Multilevel descriptive statistics for x1, print ICC with 5 digits
#' multilevel.descript(dat$x1, cluster = dat$cluster, icc.digits = 5)
#'
#' # Multilevel descriptive statistics for x1, convert value 1 to NA
#' multilevel.descript(dat$x1, cluster = dat$cluster, as.na = 1)
#'
#' # Multilevel descriptive statistics for x1,
#' # use lme() function in the nlme package to estimate ICC
#' multilevel.descript(dat$x1, cluster = dat$cluster, method = "nlme")
#'
#' Multilevel descriptive statistics for x1, x2, and x3
#' multilevel.descript(dat[, c("x1", "x2", "x3")], cluster = dat$cluster)
#'
#' \dontrun{
#' # Write Results into a Excel file
#'  multilevel.descript(dat[, c("x1", "x2", "x3")], cluster = dat$cluster,
#'                      write = "Multilevel_Descript.xlsx")
#'
#' result <- multilevel.descript(dat[, c("x1", "x2", "x3")], cluster = dat$cluster,
#'                               output = FALSE)
#' write.result(result, "Multilevel_Descript.xlsx")
#' }
multilevel.descript <- function(x, cluster, method = c("aov", "lme4", "nlme"),
                                REML = TRUE, digits = 2, icc.digits = 3, as.na = NULL,
                                write = NULL, check = TRUE, output = TRUE) {

  ####################################################################################
  # Data

  #......
  # Check if input 'x' is missing
  if (isTRUE(missing(x))) { stop("Please specify a vector, matrix or data frame for the argument 'x'.", call. = FALSE) }

  #......
  # Check if input 'x' is NULL
  if (isTRUE(is.null(x))) { stop("Input specified for the argument 'x' is NULL.", call. = FALSE) }

  #......
  # Vector, matrix or data frame for the argument 'x'?
  if (isTRUE(!is.atomic(x) && !is.matrix(x) && !is.data.frame(x))) { stop("Please specify a numeric vector, matrix or data frame with numeric variables for the argument 'x'.", call. = FALSE) }

  #......
  # Check if input 'cluster' is missing
  if (isTRUE(missing(cluster))) { stop("Please specify a vector representing the nested grouping structure for the argument 'cluster'.", call. = FALSE) }

  #......
  # Check if input 'cluster' is NULL
  if (isTRUE(is.null(cluster))) { stop("Input specified for the argument 'cluster is NULL.", call. = FALSE) }

  #......
  # Check if only one variable specified in the input 'cluster'
  if (ncol(data.frame(cluster)) != 1L) { stop("More than one variable specified for the argument 'cluster'.",call. = FALSE) }

  #......
  # Convert 'cluster' into a vector
  cluster <- unlist(cluster, use.names = FALSE)

  #----------------------------------------
  # Data frame

  x <- as.data.frame(x, stringsAsFactors = FALSE)

  #----------------------------------------
  # Convert user-missing values into NA

  if (isTRUE(!is.null(as.na))) {

    x <- misty::as.na(x, na = as.na, check = check)

    # Variable with missing values only
    x.miss <- vapply(x, function(y) all(is.na(y)), FUN.VALUE = logical(1))
    if (isTRUE(any(x.miss))) {

      stop(paste0("After converting user-missing values into NA, following variables are completely missing: ",
                  paste(names(which(x.miss)), collapse = ", ")), call. = FALSE)

    }

  }

  ####################################################################################
  # Input Check

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  #-----------------------------------------

  if (isTRUE(check)) {

    #......
    # Check input 'cluster'
    if (isTRUE(is.null(dim(x)))) {

      # Length of 'x' and 'cluster'
      if (isTRUE(length(x) != length(cluster))) { stop("Length of the vector 'x' does not match with the length of the cluster variable 'cluster'.", call. = FALSE) }

    } else {

      # Length of 'x' and 'cluster'
      if (isTRUE(nrow(x) != length(cluster))) { stop("Number of rows in 'x' does not match with the length of the cluster variable 'cluster'.", call. = FALSE) }

    }

    #......
    # Check input 'cluster'
    if (isTRUE(length(unique(na.omit(cluster))) == 1L)) { stop("There is only one group represented in the cluster variable 'cluster'.", call. = FALSE) }

    #......
    # Check input 'x': Zero variance?
    x.check <- vapply(x, function(y) length(na.omit(unique(y))) == 1L, FUN.VALUE = logical(1))

    if (isTRUE(any(x.check))) {

      if (isTRUE(length(x.check) > 1L)) {

        warning(paste0("Following variables in the matrix or data frame specified in 'x' have zero variance: ",
                       paste(names(which(x.check)), collapse = ", ")), call. = FALSE)

      } else {

        stop("Variable specified in 'x' has zero variance.", call. = FALSE)

      }

    }

    #......
    # Check input 'method'
    if (isTRUE(any(!method %in% c("aov", "lme4", "nlme")))) { stop("Character string in the argument 'method' does not match with \"aov\", \"lme4\", or \"nlme\".", call. = FALSE) }

    #......
    # Check input 'REML'
    if (isTRUE(!is.logical(REML))) { stop("Please specify TRUE or FALSE for the argument 'REML'", call. = FALSE) }

    #......
    # Check digits argument
    if (isTRUE(digits %% 1L != 0L || digits < 0L)) { stop("Specify a positive integer value for the argument 'digits'.", call. = FALSE) }

    #......
    # Check icc.digits argument
    if (isTRUE(icc.digits %% 1L != 0L || icc.digits < 0L)) { stop("Specify a positive integer value for the argument 'icc.digits'.", call. = FALSE) }

    #......
    # Variance within clusters
    if (isTRUE(ncol(x) == 1L)) {

      if (isTRUE(all(tapply(unlist(x), cluster, function(y) length(na.omit(y))) <= 1L))) {

        stop("Variable specified in 'x' does not have any within-cluster variance.", call. = FALSE)

      }

    } else {

      x.check <- vapply(x, function(y) all(tapply(y, cluster, function(z) length(na.omit(z))) <= 1L), FUN.VALUE = logical(1))

      if (isTRUE(any(x.check))) {

        stop(paste0("Following variables specified in 'x' do not have any within-cluster variance: ",
                    paste(names(which(x.check)), collapse = ", ")), call. = FALSE)

      }

    }

    #......
    # Check input 'output'
    if (isTRUE(!is.logical(output))) { stop("Please specify TRUE or FALSE for the argument 'output'", call. = FALSE) }

  }

  ####################################################################################
  # Data and Arguments

  #-----------------------------------------
  # Argument method
  if (isTRUE(all(c("aov", "lme4", "nlme") %in% method))) {

    if (isTRUE(nzchar(system.file(package = "lme4")))) {

      method <- "lme4"

    } else {

      method <- "aov"

    }

  }

  ####################################################################################
  # Main Function

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

  #-----------------------------------------
  # One dependent variable

  if (isTRUE(ncol(x) == 1L)) {

    #............
    # Variable with non-zero variance
    if (isTRUE(var(x, na.rm = TRUE) != 0L)) {

      # ICC using aov() function
      if (isTRUE(method == "aov")) {

        # Estimate model
        mod <- aov(unlist(x) ~ 1 + Error(as.factor(cluster)))

        # Model summary
        mod.summary <- summary(mod)

        # Mean
        mean.x <- mean(tapply(unlist(x), cluster, mean, na.rm = TRUE))

        # Mean Squared Error Between
        var.b <- unname(unlist(mod.summary[[1]])["Mean Sq"])

        # Mean Squared Error Within
        var.w <- unname(unlist(mod.summary[[2]])["Mean Sq"])

        # ICC(1)
        icc1 <- unname((var.b - var.w) / (var.b + ((m.cluster.size - 1L) * var.w)))

        if (isTRUE(icc1 < 0L)) { icc1 <- 0L }

        # ICC(2)
        icc2 <- unname((var.b - var.w) / var.b)

        if (isTRUE(icc2 < 0L)) { icc2 <- 0L }

      # ICC using lmer() function
      } else if (isTRUE(method == "lme4")) {

        # Estimate model
        mod <- suppressMessages(lme4::lmer(unlist(x) ~ 1 + (1|cluster), REML = REML,
                                           control = lme4::lmerControl(optimizer = "bobyqa")))

        # Mean
        mean.x <- unname(lme4::fixef(mod))

        # Variance components
        vartab <- as.data.frame(suppressMessages(lme4::VarCorr(mod)))

        # Between-cluster variance
        var.b <- vartab[vartab$grp == "cluster", "vcov"]

        # Within-cluster variance
        var.w <- vartab[vartab$grp == "Residual", "vcov"]

        # ICC(1)
        icc1 <- var.b / (var.b + var.w)

        # ICC(2)
        icc2 <- var.b / (var.b + var.w / m.cluster.size)

      # ICC using lme() function
      } else if (isTRUE(method == "nlme")) {

        # REML or ML
        ifelse(isTRUE(REML), REML <- "REML", REML <- "ML")

        # Estimate model
        mod <- nlme::lme(x ~ 1, random = ~1 | cluster, data = data.frame(x, cluster = cluster),
                         na.action = na.omit, method = REML)

        # Mean
        mean.x <- unname(lme4::fixef(mod))

        # Variance components
        vartab <- nlme::VarCorr(mod)

        var.b <- as.numeric(vartab["(Intercept)", "Variance"])

        var.w <- as.numeric(vartab["Residual", "Variance"])

        # ICC(1)
        icc1 <- var.b / (var.b + var.w)

        # ICC(2)
        icc2 <- var.b / (var.b + var.w / m.cluster.size)

      }

    #............
    # Variable with non-zero variance
    } else {

      object <- NA

    }

  #-----------------------------------------
  # More than one dependent variable
  } else {

    object <- apply(x, 2, function(y) misty::multilevel.descript(y, cluster, method = method,
                                                                 REML = REML, digits = digits, icc.digits = icc.digits,
                                                                 as.na = NULL, check = FALSE, output = FALSE))

    # Extract results
    mean.x <- sapply(object, function(y) y$result$mean.x)
    var.w <- sapply(object, function(y) y$result$var.w)
    var.b <- sapply(object, function(y) y$result$var.b)
    icc1 <- sapply(object, function(y) y$result$icc1)
    icc2 <- sapply(object, function(y) y$result$icc2)

  }

  # Design effect
  deff <- 1 + icc1*(m.cluster.size - 1L)

  deff.sqrt <- sqrt(deff)

  # Effective sample size
  n.effect <- no.obs / deff

  ####################################################################################
  # Return object

  object <- list(call = match.call(),
                 type = "multilevel.descript",
                 data = data.frame(x = x, cluster = cluster, stringsAsFactors = FALSE),
                 args = list(method = method, REML = REML,
                             digits = digits, icc.digits = icc.digits, as.na = as.na,
                             check = check, output = output),
                 result = list(no.obs = no.obs, no.miss = no.miss, no.cluster = no.cluster,
                               m.cluster.size = m.cluster.size, sd.cluster.size = sd.cluster.size,
                               min.cluster.size = min.cluster.size, max.cluster.size = max.cluster.size,
                               mean.x = mean.x, var.w = var.w, var.b = var.b, icc1 = icc1, icc2 = icc2,
                               deff = deff, deff.sqrt = deff.sqrt, n.effect = n.effect))

  class(object) <- "misty.object"

  ####################################################################################
  # Write results

  if (isTRUE(!is.null(write))) { misty::write.result(object, file = write) }

  ####################################################################################
  # Output

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
