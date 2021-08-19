#' Within-Group and Between-Group Correlation Matrix
#'
#' This function computes the within-group and between-group correlation matrix
#' using the lavaan package and provides standard errors, z test statistics,
#' and significance values (\emph{p}-values) for testing the hypothesis
#' H0: \eqn{\rho} = 0 for all pairs of variables within and between groups.
#'
#' The specification of the within-group and between-group variables is in line
#' with the syntax in Mplus. That is, the \code{within} argument is used to identify
#' the variables in the matrix or data frame specified in \code{x} that are measured
#' on the individual level and modeled only on the within level. They are specified
#' to have no variance in the between part of the model. The \code{between} argument
#' is used to identify the variables in the matrix or data frame specified in \code{x}
#' that are measured on the cluster level and modeled only on the between level.
#' Variables not mentioned in the arguments \code{within} or \code{between}
#' are measured on the individual level and will be modeled on both the within
#' and between level.
#'
#' By default, the function uses robust maximum likelihood (\code{estimator = "MLR"}),
#' i.e., maximum likelihood with Huber-White robust standard errors. When using
#' \code{estimator = "MLR"}, listwise deletion is used for missing data (\code{na.omit = TRUE}).
#' Note that the lavaan version 0.6-9 supports full information maximum likelihood (FIML)
#' in multilevel models for maximum likelihood (\code{estimator = "ML"}), but not
#' for robust maximum likelihood (\code{estimator = "MLR"}). Moreover, FIML cannot
#' be used when a within-group variables has no variance within some clusters. In
#' this cases, listwise deletion is used even though \code{estimator = "ML"} and
#' \code{na.omit = FALSE} was specified. Note that there might be issues in model
#' convergence when using FIML (\code{estimator = "ML"} and \code{na.omit = FALSE}),
#' which might be resolved when switching to listwise deletion (\code{na.omit = TRUE}).
#'
#' lavaan package uses a quasi-Newton optimization method (\code{"nlminb"}) by default.
#' If the optimizer does not converge, model estimation will switch to the Expectation
#' Maximization (EM) algorithm.
#'
#' Statistically significant correlation coefficients are shown in boldface on the
#' console (\code{sig = TRUE}). However, this option is not supported when using
#' R Markdown, i.e., the argument \code{sig} will switch to \code{FALSE}.
#'
#' Adjustment method for multiple testing when specifying the argument \code{p.adj}
#' is applied to the within-group and between-broup correlation matrix separately.
#'
#' @param x           a matrix or data frame.
#' @param cluster     a vector representing the nested grouping structure (i.e., group or
#'                    cluster variable).
#' @param within      a character vector representing variables that are measured on the
#'                    within level and modeled only on the within level. Variables not
#'                    mentioned in \code{within} or \code{between} are measured on
#'                    the within level and will be modeled on both the within and
#'                    between level.
#' @param between     a character vector representing variables that are measured on the
#'                    between level and modeled only on the between level. Variables not
#'                    mentioned in \code{within} or \code{between} are measured on
#'                    the within level and will be modeled on both the within and
#'                    between level.
#' @param estimator   a character string indicating the estimator to be used: \code{"ML"}
#'                    for maximum likelihood and \code{"MLR"} (default) for maximum
#'                    likelihood with Huber-White robust standard errors. Note that
#'                    incomplete cases are removed listwise (i.e., \code{na.omit = TRUE})
#'                    when using \code{"MLR"}, whereas full information maximum likelihood
#'                    (FIML) is used to deal with missing data when using \code{"ML}
#'                    when specifying \code{na.omit = FALSE}.
#' @param na.omit     logical: if \code{TRUE}, incomplete cases are removed before
#'                    conducting the analysis (i.e., listwise deletion); if \code{FALSE}
#'                    (default), full information maximum likelihood (FIML) is used
#'                    when specifying \code{estimator = "ML"}.
#' @param sig         logical: if \code{TRUE} (default), statistically significant
#'                    correlation coefficients are shown in boldface on the console.
#' @param alpha       a numeric value between 0 and 1 indicating the significance
#'                    level at which correlation coefficients are printed boldface
#'                    when \code{sig = TRUE}.
#' @param print       a character string or character vector indicating which results
#'                    to show on the console, i.e. \code{"all"} for all results,
#'                    \code{"cor"} for correlation coefficients, \code{"se"} for
#'                    standard errors, \code{"stat"} for z test statistics, and
#'                    \code{"p"} for \emph{p}-values.
#' @param split       logical: if \code{TRUE}, output table is split in within-group
#'                    and between-group correlation matrix.
#' @param tri         a character string indicating which triangular of the matrix
#'                    to show on the console when \code{split = TRUE}, i.e., \code{both}
#'                    for upper and \code{upper} for the upper triangular.
#' @param tri.lower   logical: if \code{TRUE} (default) and \code{split = FALSE} (default),
#'                    within-group correlations are shown in the lower triangular
#'                    and between-group correlation are shown in the upper triangular.
#' @param p.adj       a character string indicating an adjustment method for multiple
#'                    testing based on \code{\link{p.adjust}}, i.e., \code{none} (default),
#'                    \code{bonferroni}, \code{holm}, \code{hochberg}, \code{hommel},
#'                    \code{BH}, \code{BY}, or \code{fdr}.
#' @param digits      an integer value indicating the number of decimal places to
#'                    be used for displaying correlation coefficients.
#' @param p.digits    an integer value indicating the number of decimal places to
#'                    be used for displaying \emph{p}-values.
#' @param as.na       a numeric vector indicating user-defined missing values, i.e.
#'                    these values are converted to \code{NA} before conducting the
#'                    analysis. Note that \code{as.na()} function is only applied
#'                    to \code{x} but not to \code{cluster}.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#' @param output      logical: if \code{TRUE}, output is shown on the console.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{multilevel.descript}}, \code{\link{multilevel.icc}}, \code{\link{cluster.scores}}
#'
#' @references
#' Hox, J., Moerbeek, M., & van de Schoot, R. (2018). \emph{Multilevel analysis:
#' Techniques and applications} (3rd. ed.). Routledge.
#'
#' Snijders, T. A. B., & Bosker, R. J. (2012). \emph{Multilevel analysis: An introduction
#' to basic and advanced multilevel modeling} (2nd ed.). Sage Publishers.
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries: function call (\code{call}), type of analysis (\code{type}), matrix or
#' data frame specified in \code{x} (\code{data}), specification of function arguments
#' (\code{args}), fitted lavaan object (\code{mod.fit}), and list with results
#' (\code{result}).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load data set "Demo.twolevel" in the lavaan package
#' data("Demo.twolevel", package = "lavaan")
#'
#' #---------------------------
#' # All variables modeled on both the within and between level
#' multilevel.cor(Demo.twolevel[, c("y1", "y2", "y3")],
#'                cluster = Demo.twolevel$cluster)
#'
#' # Split output table in within-group and between-group correlation matrix.
#' multilevel.cor(Demo.twolevel[, c("y1", "y2", "y3")],
#'                cluster = Demo.twolevel$cluster, split = TRUE)
#'
#' # Print correlation coefficients, standard errors, z test statistics,
#' # and p-values
#' multilevel.cor(Demo.twolevel[, c("y1", "y2", "y3")],
#'                cluster = Demo.twolevel$cluster, print = "all")
#'
#' # Print correlation coefficients and p-values
#' # significance values with Bonferroni correction
#' multilevel.cor(Demo.twolevel[, c("y1", "y2", "y3")],
#'                cluster = Demo.twolevel$cluster, print = c("cor", "p"),
#'                p.adj = "bonferroni")
#'
#' #---------------------------
#' # Variables "y1", "y2", and "y2" modeled on both the within and between level
#' # Variables "w1" and "w2" modeled on the cluster level
#' multilevel.cor(Demo.twolevel[, c("y1", "y2", "y3", "w1", "w2")],
#'                cluster = Demo.twolevel$cluster,
#'                between = c("w1", "w2"))
#'
#' #---------------------------
#' # Variables "y1", "y2", and "y2" modeled only on the within level
#' # Variables "w1" and "w2" modeled on the cluster level
#' multilevel.cor(Demo.twolevel[, c("y1", "y2", "y3", "w1", "w2")],
#'                cluster = Demo.twolevel$cluster,
#'                within = c("y1", "y2", "y3"), between = c("w1", "w2"))
#'
#' # Summary of the multilevel model used to compute the within-group
#' # and between-group correlation matrix
#' mod <- multilevel.cor(Demo.twolevel[, c("y1", "y2", "y3")],
#'                       cluster = Demo.twolevel$cluster, output = FALSE)
#' lavaan::summary(mod$mod.fit, standardized = TRUE)
#' }
multilevel.cor <- function(x, cluster, within = NULL, between = NULL, estimator = c("ML", "MLR"),
                           na.omit = TRUE, sig = TRUE, alpha = 0.05,
                           print = c("all", "cor", "se", "stat", "p"), split = FALSE,
                           tri = c("both", "lower", "upper"), tri.lower = TRUE,
                           p.adj = c("none", "bonferroni", "holm", "hochberg", "hommel", "BH", "BY", "fdr"),
                           digits = 2, p.digits = 3, as.na = NULL, check = TRUE, output = TRUE) {

  ####################################################################################
  # R package

  if (isTRUE(!nzchar(system.file(package = "lavaan")))) {

    stop("Package \"lavaan\" is needed for this function, please install the package.", call. = FALSE)

  }

  ####################################################################################
  # Data

  #......
  # Check if input 'x' is missing
  if (isTRUE(missing(x))) {

    stop("Please specify a vector, matrix or data frame for the argument 'x'.", call. = FALSE)

  }

  #......
  # Check if input 'x' is NULL
  if (isTRUE(is.null(x))) {

    stop("Input specified for the argument 'x' is NULL.", call. = FALSE)

  }

  #......
  # Vector, matrix or data frame for the argument 'x'?
  if (isTRUE(!is.matrix(x) && !is.data.frame(x))) {

    stop("Please specify a matrix or data frame with numeric variables for the argument 'x'.",
         call. = FALSE)

  }

  #......
  # Check if input 'cluster' is missing
  if (isTRUE(missing(cluster))) {

    stop("Please specify a vector representing the nested grouping structure for the argument 'cluster'.",
         call. = FALSE)

  }

  #......
  # Check if input 'cluster' is NULL
  if (isTRUE(is.null(cluster))) {

    stop("Input specified for the argument 'cluster is NULL.", call. = FALSE)

  }

  #......
  # Check if only one variable specified in the input 'cluster'
  if (ncol(data.frame(cluster)) != 1) {

    stop("More than one variable specified for the argument 'cluster'.",call. = FALSE)

  }

  #......
  # Convert 'cluster' into a vector
  cluster <- unlist(cluster, use.names = FALSE)

  #----------------------------------------
  # Convert user-missing values into NA

  if (isTRUE(!is.null(as.na))) {

    x <- misty::as.na(x, na = as.na, check = check)

    # Variable with missing values only
    x.miss <- vapply(x, function(y) all(is.na(y)), FUN.VALUE = logical(1))
    if (isTRUE(any(x.miss))) {

      if (length(which(x.miss)) == 1) {

        stop(paste0("After converting user-missing values into NA, following variable is completely missing: ",
                    names(which(x.miss))), call. = FALSE)

      } else {

        stop(paste0("After converting user-missing values into NA, following variables are completely missing: ",
                    paste(names(which(x.miss)), collapse = ", ")), call. = FALSE)

      }

    }

  }

  #----------------------------------------
  # Within- and Between-Group Variables

  #......
  # Within variables in 'x'
  within.miss <- !within %in% colnames(x)
  if (isTRUE(any(within.miss))) {

    if (length(which(within.miss)) == 1) {

      stop(paste0("Variable specified in the argument 'within' was not found in 'x': ",
                  within[which(within.miss)], collapse = ", "), call. = FALSE)

    } else {

      stop(paste0("Variables specified in the argument 'within' were not found in 'x': ",
                  within[which(within.miss)], collapse = ", "), call. = FALSE)

    }

  }

  #......
  # Within variables
  if (isTRUE(is.null(between))) {

    var.with <- colnames(x)

  } else {

    var.with <- colnames(x)[!colnames(x) %in% between]

  }

  #......
  # No within variables
  if (length(var.with) == 0) {

    stop("Please specify at least two within-group variables.", call. = FALSE)

  }

  #-----------------------------------------

  #......
  # Between variables in 'x'
  between.miss <- !between %in% colnames(x)
  if (isTRUE(any(between.miss))) {

    if (length(which(between.miss)) == 1) {

      stop(paste0("Variable specified in the argument 'between' was not found in 'x': ",
                  between[which(between.miss)], collapse = ", "), call. = FALSE)

    } else {

      stop(paste0("Variables specified in the argument 'between' were not found in 'x': ",
                  between[which(between.miss)], collapse = ", "), call. = FALSE)

    }

  }

  #......
  # Variance within clusters
  x.check <- vapply(x[, between, drop = FALSE], function(y) any(tapply(y, cluster, var, na.rm = TRUE) != 0), FUN.VALUE = logical(1))

  if (isTRUE(any(x.check))) {

    if (length(which(x.check)) == 1) {

      warning(paste0("Following between-group variable has variance within clusters: ",
                     names(which(x.check))), call. = FALSE)

    } else {

      warning(paste0("Following between-group variables have variance within clusters: ",
                     paste(names(which(x.check)), collapse = ", ")), call. = FALSE)

    }

  }

  #......
  #  Between variables
  if (isTRUE(is.null(within))) {

    var.betw <- colnames(x)

  } else {

    var.betw <- colnames(x)[!colnames(x) %in% within]

  }

  ###

  #......
  # No between variables
  if (length(var.betw) == 0) {

    stop("Please specify at least two between-group variables.", call. = FALSE)

  }

  #......
  # Variables in 'within' or 'between'
  wb.inter <- intersect(within, between)
  if (isTRUE(length(wb.inter) > 0)) {

    if (length(wb.inter) == 1) {

      stop(paste0("Following variable is specified in both arguments 'within' and 'between': ",
                  wb.inter), call. = FALSE)

    } else {

      stop(paste0("Following variables are specified in both arguments 'within' and 'between': ",
                  paste(wb.inter, collapse = ", ")), call. = FALSE)

    }


  }

  #----------------------------------------
  # Data frame with Grouping Variable

  x <- data.frame(cluster = cluster, x, stringsAsFactors = FALSE)

  #......
  # Missing data

  attr(x, "missing") <- any(is.na(x))

  ####################################################################################
  # Input Check

  #......
  # Check input 'check'
  if (isTRUE(!is.logical(check))) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }

  #-----------------------------------------

  if (isTRUE(check)) {

    #......
    # Check input 'cluster'
    if (isTRUE(nrow(x) != length(cluster))) {

      stop("Number of rows in 'x' does not match with the length of the cluster variable 'cluster'.",
             call. = FALSE)

    }

    #......
    # Check input 'cluster'
    if (isTRUE(length(unique(na.omit(cluster))) == 1L)) {

      stop("There is only one group represented in the clustering variable 'cluster'.", call. = FALSE)

    }

    #......
    # Check input 'x': Zero variance?
    x.check <- vapply(x, function(y) length(na.omit(unique(y))) == 1L, FUN.VALUE = logical(1))

    if (isTRUE(any(x.check))) {

      if (length(which(x.check)) == 1) {

        warning(paste0("Following variable in the matrix or data frame specified in 'x' has zero variance: ",
                       names(which(x.check))), call. = FALSE)

      } else {

        warning(paste0("Following variables in the matrix or data frame specified in 'x' have zero variance: ",
                       paste(names(which(x.check)), collapse = ", ")), call. = FALSE)

      }

    }

    #......
    # Check input 'within'
    x.check <- vapply(x[, var.with, drop = FALSE], function(y) all(tapply(y, cluster, var, na.rm = TRUE) == 0), FUN.VALUE = logical(1))

    if (isTRUE(any(x.check))) {

      if (length(which(x.check)) == 1) {

        stop(paste0("Following within-group variable has zero variance within all clusters: ",
                    names(which(x.check))), call. = FALSE)

      } else {

        stop(paste0("Following within-group variables have zero variance within all clusters: ",
                    paste(names(which(x.check)), collapse = ", ")), call. = FALSE)

      }

    }

    #......
    # Check input 'estimator'
    if (isTRUE(any(!estimator %in% c("ML", "MLR")))) {

      stop("Character string in the argument 'estimator' does not match with \"ML\" or \"MLR\".",
           call. = FALSE)

    }

    #......
    # Check input 'na.omit'
    if (isTRUE(!is.logical(na.omit))) {

      stop("Please specify TRUE or FALSE for the argument 'na.omit'.", call. = FALSE)

    }

    #......
    # Check input 'sig'
    if (isTRUE(!is.logical(sig))) {

      stop("Please specify TRUE or FALSE for the argument 'sig'.", call. = FALSE)

    }

    #......
    # Check input 'alpha'
    if (isTRUE(alpha >= 1L || alpha <= 0L)) {

      stop("Please specify a number between 0 and 1 for the argument 'alpha'.", call. = FALSE)

    }

    #......
    # Check input 'print'
    if (isTRUE(any(!print %in% c("all", "cor", "se", "stat", "p")))) {

      stop("Character string(s) in the argument 'print' does not match with \"all\", \"cor\", \"se\", \"stat\", or \"p\".",
           call. = FALSE)

    }

    #......
    # Check input 'split'
    if (isTRUE(!is.logical(split))) {

      stop("Please specify TRUE or FALSE for the argument 'split'.", call. = FALSE)

    }

    #......
    # Check input 'tri'
    if (isTRUE(any(!tri %in% c("both", "lower", "upper")))) {

      stop("Character string in the argument 'tri' does not match with \"both\", \"lower\", or \"upper\".",
           call. = FALSE)

    }

    #......
    # Check input 'tri.lower'
    if (isTRUE(!is.logical(tri.lower))) {

      stop("Please specify TRUE or FALSE for the argument 'tri.lower'.", call. = FALSE)

    }

    #......
    # Check input 'p.adj'
    if (isTRUE(any(!p.adj %in% c("none", "holm", "bonferroni", "hochberg", "hommel", "BH", "BY", "fdr")))) {

      stop("Character string in the argument 'p.adj' does not match with \"none\", \"bonferroni\", \"holm\", \"hochberg\", \"hommel\", \"BH\", \"BY\", or \"fdr\".",
           call. = FALSE)

    }

    #......
    # Check input 'digits'
    if (isTRUE(digits %% 1L != 0L || digits < 0L)) {

      stop("Please specify a positive integer number for the argument 'digits'.", call. = FALSE)

    }

    #......
    # Check input 'p.digits'
    if (isTRUE(p.digits %% 1L != 0L || p.digits < 0L)) {

      stop("Please specify a positive integer number for the argument 'p.digits'.", call. = FALSE)

    }

    #......
    # Check input 'output'
    if (isTRUE(!is.logical(output))) {

      stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE)

    }

  }

  ####################################################################################
  # Data and Arguments

  #-----------------------------------------
  # Estimator

  estimator <- ifelse(all(c("ML", "MLR") %in% estimator), "MLR", estimator)

  #-----------------------------------------
  # Listwise deletion

  # No variance of level-1 variables within clusters, i.e. switch to listwise deletion
  x.check <- vapply(x[, var.with, drop = FALSE], function(y) any(tapply(y, cluster, var, na.rm = TRUE) == 0), FUN.VALUE = logical(1))
  if (isTRUE(any(x.check) & isTRUE(!na.omit))) {

    if (length(which(x.check)) == 1) {

      warning(paste0("A within-group variable has no variance within some clusters, FIML switched to listwise deletion: "),
              names(which(x.check)), call. = FALSE)

    } else {

      warning(paste0("Some within-group variables have no variance within some clusters, FIML switched to listwise deletion: "),
              paste(names(which(x.check)), collapse = ", "), call. = FALSE)

    }

    na.omit <- TRUE

  }

  # Robust maximum likelihood
  if (isTRUE(estimator == "MLR")) {

    na.omit <- TRUE

  }

  if (isTRUE(na.omit)) {

    x.cluster <- na.omit(x)

  }

  #-----------------------------------------
  # Missing data

  if (isTRUE(na.omit)) {

    # Listwise deletion
    missing <- "listwise"

  } else {

    # Full information maximum likelihood
    missing <- "ML"

  }

  #-----------------------------------------
  # Print correlation, sample size or significance values

  if (isTRUE(all(c("all", "cor", "se", "stat", "p") %in% print))) { print <- "cor" }

  if (isTRUE(length(print) == 1 && "all" %in% print)) { print <- c("cor", "se", "stat", "p") }

  #-----------------------------------------
  # Print triangular

  tri <- ifelse(all(c("both", "lower", "upper") %in% tri), "lower", tri)

  #-----------------------------------------
  # Adjustment method for multiple testing

  p.adj <- ifelse(all(c("none", "bonferroni", "holm", "hochberg", "hommel", "BH", "BY", "fdr") %in% p.adj), "none", p.adj)

  ####################################################################################
  # Main Function

  #-----------------------------------------
  # Model specification


  mod <- paste("level: 1\n ",
               # Within model
               paste(apply(combn(length(var.with), 2), 2, function(y) paste(var.with[y[1]], var.with[y[2]], sep = " ~~ " )), collapse = "\n "),
               "\n level: 2\n ",
               # Between model
               paste(apply(combn(length(var.betw), 2), 2, function(y) paste(var.betw[y[1]], var.betw[y[2]], sep = " ~~ " )), collapse = "\n "))


  #-----------------------------------------
  # Model estimation

  mod.fit <- suppressWarnings(lavaan::cfa(mod, cluster = "cluster", estimator = estimator,
                                          data = x, missing = missing))

  # Convergence
  if (isTRUE(!lavaan::inspect(mod.fit, what = "converged"))) {

    message("Quasi-Newton optimizer did not converge, switched to the the EM algorithm.")

    mod.fit <- suppressWarnings(lavaan::cfa(mod, cluster = "cluster", estimator = estimator,
                                            data = x, missing = missing, optim.method = "em"))

    if (isTRUE(!lavaan::inspect(mod.fit, what = "converged"))) {

      stop("EM algorithm did not converge.")

    }

  }

  #-----------------------------------------
  # Extract Results

  # Standardized solution
  stand <- lavaan::lavMatrixRepresentation(lavaan::standardizedSolution(mod.fit))

  # Visible binding for global variable
  mat <- level <- NULL

  #......................
  # Within-Group Results

  # Theta
  with.stand.theta <- subset(stand[ unlist(subset(lavaan::lavMatrixRepresentation(lavaan::parameterestimates(mod.fit)), level == 1, select = "id")), ], mat == "theta")

  # Parameter estimate, z and significance value matrix
  with.p <- with.stat <- with.se <- with.cor <- matrix(NA, ncol = max(stand[, "col"]), nrow = max(stand[, "row"]))
  for (i in seq_len(nrow(with.stand.theta))) {

    with.cor[with.stand.theta[i, "row"], with.stand.theta[i, "col"]] <- with.stand.theta[i, "est.std"]
    with.se[with.stand.theta[i, "row"], with.stand.theta[i, "col"]] <- with.stand.theta[i, "se"]
    with.stat[with.stand.theta[i, "row"], with.stand.theta[i, "col"]] <- with.stand.theta[i, "z"]
    with.p[with.stand.theta[i, "row"], with.stand.theta[i, "col"]] <- with.stand.theta[i, "pvalue"]

  }

  with.cor[lower.tri(with.cor)] <- t(with.cor)[lower.tri(with.cor)]
  with.se[lower.tri(with.se)] <- t(with.se)[lower.tri(with.se)]
  with.stat[lower.tri(with.stat)] <- t(with.stat)[lower.tri(with.stat)]
  with.p[lower.tri(with.p)] <- t(with.p)[lower.tri(with.p)]

  colnames(with.cor) <- colnames(with.se) <- colnames(with.stat) <- colnames(with.p) <-
  rownames(with.cor) <- rownames(with.se) <- rownames(with.stat) <- rownames(with.p) <- sapply(seq_len(max(stand[, "row"])), function(y) unique(stand[which(y == stand$row), "lhs"]))

  #......................
  # Between-Group Results

  # Standardized solution
  betw.stand.theta <- subset(stand[ unlist(subset(lavaan::lavMatrixRepresentation(lavaan::parameterestimates(mod.fit)), level == 2, select = "id")), ], mat == "theta")

  # Parameter estimate, z and significance value matrix
  betw.p <- betw.stat <- betw.se <- betw.cor <- matrix(NA, ncol = max(stand[, "col"]), nrow = max(stand[, "row"]))
  for (i in seq_len(nrow(betw.stand.theta))) {

    betw.cor[betw.stand.theta[i, "row"], betw.stand.theta[i, "col"]] <- betw.stand.theta[i, "est.std"]
    betw.se[betw.stand.theta[i, "row"], betw.stand.theta[i, "col"]] <- betw.stand.theta[i, "se"]
    betw.stat[betw.stand.theta[i, "row"], betw.stand.theta[i, "col"]] <- betw.stand.theta[i, "z"]
    betw.p[betw.stand.theta[i, "row"], betw.stand.theta[i, "col"]] <- betw.stand.theta[i, "pvalue"]

  }

  betw.cor[lower.tri(betw.cor)] <- t(betw.cor)[lower.tri(betw.cor)]
  betw.se[lower.tri(betw.se)] <- t(betw.se)[lower.tri(betw.se)]
  betw.stat[lower.tri(betw.stat)] <- t(betw.stat)[lower.tri(betw.stat)]
  betw.p[lower.tri(betw.p)] <- t(betw.p)[lower.tri(betw.p)]

  colnames(betw.cor) <- colnames(betw.se) <- colnames(betw.stat) <- colnames(betw.p) <-
  rownames(betw.cor) <- rownames(betw.se) <- rownames(betw.stat) <- rownames(betw.p) <- sapply(seq_len(max(stand[, "row"])), function(y) unique(stand[which(y == stand$row), "lhs"]))

  #-----------------------------------------
  # Combine Within-Group and Between-Group Results

  #......................
  # Within-group correlations in the lower triangular
  if (isTRUE(tri.lower)) {

    # Within-group results
    wb.cor <- with.cor
    wb.se <- with.se
    wb.stat <- with.stat
    wb.p <- with.p

    # Between-group results
    wb.cor[upper.tri(wb.cor)] <- betw.cor[upper.tri(wb.cor)]
    wb.se[upper.tri(wb.se)] <- betw.se[upper.tri(wb.se)]
    wb.stat[upper.tri(wb.stat)] <- betw.stat[upper.tri(wb.stat)]
    wb.p[upper.tri(wb.p)] <- betw.p[upper.tri(wb.p)]

  #......................
  # Within-group correlations in the upper triangular
  } else {

    # Within-group results
    wb.cor <- betw.cor
    wb.se <- betw.se
    wb.stat <- betw.stat
    wb.p <- betw.p

    # Between-group results
    wb.cor[upper.tri(wb.cor)] <- with.cor[upper.tri(wb.cor)]
    wb.se[upper.tri(wb.se)] <- with.se[upper.tri(wb.se)]
    wb.stat[upper.tri(wb.stat)] <- with.stat[upper.tri(wb.stat)]
    wb.p[upper.tri(wb.p)] <- with.p[upper.tri(wb.p)]

  }

  #......................
  # Adjust p-values for multiple comparison

  if (isTRUE(p.adj != "none")) {

    wb.p[lower.tri(wb.p)] <- p.adjust(wb.p[lower.tri(wb.p)], method = p.adj)
    wb.p[upper.tri(wb.p)] <- p.adjust(wb.p[upper.tri(wb.p)], method = p.adj)

  }

  #-----------------------------------------
  # Split Within-Group and Between-Group Results

  # Within-group results
  with.cor <- with.cor[which(apply(with.cor, 1, function(y) !all(is.na(y)))), which(apply(with.cor, 2, function(y) !all(is.na(y))))]
  with.se <- with.se[which(apply(with.se, 1, function(y) !all(is.na(y)))), which(apply(with.se, 2, function(y) !all(is.na(y))))]
  with.stat <- with.stat[which(apply(with.stat, 1, function(y) !all(is.na(y)))), which(apply(with.stat, 2, function(y) !all(is.na(y))))]
  with.p <- with.p[which(apply(with.p, 1, function(y) !all(is.na(y)))), which(apply(with.p, 2, function(y) !all(is.na(y))))]

  # Between-group results
  betw.cor <- betw.cor[which(apply(betw.cor, 1, function(y) !all(is.na(y)))), which(apply(betw.cor, 2, function(y) !all(is.na(y))))]
  betw.se <- betw.se[which(apply(betw.se, 1, function(y) !all(is.na(y)))), which(apply(betw.se, 2, function(y) !all(is.na(y))))]
  betw.stat <- betw.stat[which(apply(betw.stat, 1, function(y) !all(is.na(y)))), which(apply(betw.stat, 2, function(y) !all(is.na(y))))]
  betw.p <- betw.p[which(apply(betw.p, 1, function(y) !all(is.na(y)))), which(apply(betw.p, 2, function(y) !all(is.na(y))))]

  #......................
  # Adjust p-values for multiple comparison

  if (isTRUE(p.adj != "none")) {

    with.p[lower.tri(with.p)] <- p.adjust(with.p[lower.tri(with.p)], method = p.adj)
    with.p[upper.tri(with.p)] <- p.adjust(with.p[upper.tri(with.p)], method = p.adj)

    betw.p[lower.tri(betw.p)] <- p.adjust(betw.p[lower.tri(betw.p)], method = p.adj)
    betw.p[upper.tri(betw.p)] <- p.adjust(betw.p[upper.tri(betw.p)], method = p.adj)

  }

  ####################################################################################
  # Return object

  object <- list(call = match.call(),
                 type = "multilevel.cor",
                 data = x,
                 args = list(within = within, between = between, estimator = estimator,
                             na.omit = na.omit, sig = sig, alpha = alpha, print = print,
                             split = split, tri = tri, tri.lower = tri.lower, p.adj = p.adj,
                             digits = digits, p.digits = p.digits, as.na = as.na,
                             check = check, output = output),
                 mod.fit = mod.fit,
                 result = list(wb.cor = wb.cor, wb.se = wb.se, wb.stat = wb.stat, wb.p = wb.p,
                               with.cor = with.cor, with.se = with.se, with.stat = with.stat, with.p = with.p,
                               betw.cor = betw.cor, betw.se = betw.se, betw.stat = betw.stat, betw.p = betw.p))

  class(object) <- "misty.object"

  ####################################################################################
  # Output

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
