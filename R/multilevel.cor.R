#' Within-Group and Between-Group Correlation Matrix
#'
#' This function computes the within-group and between-group correlation matrix
#' by calling the \code{sem} function in the R package \pkg{lavaan} and provides
#' standard errors, z test statistics, and significance values (\emph{p}-values)
#' for testing the hypothesis H0: \eqn{\rho} = 0 for all pairs of variables within
#' and between groups. By default, the function computes the within-group and
#' between-group correlation matrix without standard errors, z test statistics,
#' and significance value.
#'
#' @param data         a data frame.
#' @param ...          an expression indicating the variable names in \code{data},
#'                     e.g., \code{multilevel.cor(dat, x1, x2, x3)}. Note that
#'                     the operators \code{+}, \code{-}, \code{~},
#'                     \code{:}, \code{::}, and \code{!} can also be used to
#'                     select variables, see 'Details' in the \code{\link{df.subset}}
#'                     function.
#' @param cluster      either a character string indicating the variable name of
#'                     the cluster variable in \code{data} or a vector representing
#'                     the nested grouping structure (i.e., group or cluster variable).
#' @param within       a character vector representing variables that are measured
#'                     at the within level and modeled only at the within level.
#'                     Variables not mentioned in \code{within} or \code{between}
#'                     are measured at the within level and will be modeled on both
#'                     the within and between level.
#' @param between      a character vector representing variables that are measured
#'                     at the between level and modeled only at the between level.
#'                     Variables not mentioned in \code{within} or \code{between}
#'                     are measured at the within level and will be modeled on
#'                     both the within and between level.
#' @param estimator    a character string indicating the estimator to be used, i.e.,
#'                     \code{"ML"} for maximum likelihood with conventional
#'                     standard errors and \code{"MLR"} for maximum likelihood with
#'                     Huber-White robust standard errors. The default setting
#'                     depends on the argument \code{sig}, i.e., \code{"ML"} is
#'                     used when specifying \code{sig = FALSE} (default) and
#'                     \code{"MLR"} is used when specifying \code{sig = TRUE}.
#' @param optim.method a character string indicating the optimizer, i.e.,
#'                     \code{"nlminb"} (default) for the unconstrained and
#'                     bounds-constrained quasi-Newton method optimizer and
#'                     \code{"em"} for the Expectation Maximization (EM) algorithm.
#' @param optim.switch logical: if \code{TRUE} (default), model estimation switches
#'                     to Expectation Maximization (EM) algorithm (\code{"em"})
#'                     if the quasi-Newton optimization (\code{"nlminb"} (default))
#'                     does not converge.
#' @param missing      a character string indicating how to deal with missing
#'                     data, i.e., \code{"listwise"} for listwise deletion or
#'                     \code{"fiml"} (default) for full information maximum
#'                     likelihood (FIML) method. Note that it takes longer to
#'                     estimate models while using FIML and using FIML is
#'                     prone to issues with model convergence, these issues might
#'                     be resolved by switching to listwise deletion.
#' @param sig          logical: if \code{TRUE}, statistically significant
#'                     correlation coefficients are shown in boldface on the
#'                     console. Note that standard errors, z test statistics, and
#'                     significance values not provided in the return object when
#'                     \code{sig = FALSE} (default).
#' @param alpha        a numeric value between 0 and 1 indicating the significance
#'                     level at which correlation coefficients are printed
#'                     boldface when \code{sig = TRUE}.
#' @param print        a character string or character vector indicating which
#'                     results to show on the console, i.e. \code{"all"} for all
#'                     results, \code{"cor"} for correlation coefficients,
#'                     \code{"se"} for standard errors, \code{"stat"} for z test
#'                     statistics, and \code{"p"} for \emph{p}-values.
#' @param split        logical: if \code{TRUE}, output table is split in
#'                     within-group and between-group correlation matrix.
#' @param order        logical: if \code{TRUE}, variables in the output table are
#'                     ordered, so that variables specified in the argument
#'                     \code{between} are shown first.
#' @param tri          a character string indicating which triangular of the
#'                     matrix to show on the console when \code{split = TRUE},
#'                     i.e., \code{both} for upper and \code{upper} for the upper
#'                     triangular.
#' @param tri.lower    logical: if \code{TRUE} (default) and \code{split = FALSE}
#'                     (default), within-group correlations are shown in the lower
#'                     triangular and between-group correlation are shown in the
#'                     upper triangular.
#' @param p.adj        a character string indicating an adjustment method for
#'                     multiple testing based on \code{\link{p.adjust}}, i.e.,
#'                     \code{none} (default), \code{bonferroni}, \code{holm},
#'                     \code{hochberg}, \code{hommel}, \code{BH}, \code{BY}, or
#'                     \code{fdr}.
#' @param digits       an integer value indicating the number of decimal places
#'                     to be used for displaying correlation coefficients.
#' @param p.digits     an integer value indicating the number of decimal places
#'                     to be used for displaying \emph{p}-values.
#' @param as.na        a numeric vector indicating user-defined missing values,
#'                     i.e. these values are converted to \code{NA} before
#'                     conducting the analysis. Note that \code{as.na()} function
#'                     is only applied to \code{data} but not to \code{cluster}.
#' @param write        a character string naming a file for writing the output into
#'                     either a text file with file extension \code{".txt"} (e.g.,
#'                     \code{"Output.txt"}) or Excel file with file extension
#'                     \code{".xlsx"}  (e.g., \code{"Output.xlsx"}). If the file
#'                     name does not contain any file extension, an Excel file will
#'                     be written.
#' @param append       logical: if \code{TRUE} (default), output will be appended
#'                     to an existing text file with extension \code{.txt} specified
#'                     in \code{write}, if \code{FALSE} existing text file will be
#'                     overwritten.
#' @param check        logical: if \code{TRUE} (default), argument specification,
#'                     convergence and model identification is checked.
#' @param output       logical: if \code{TRUE} (default), output is shown on the
#'                     console.
#'
#' @details
#' \describe{
#' \item{\strong{Within-Group and Between-Group Variables}}{The specification of
#' the within-group and between-group variables is in line with the syntax in Mplus.
#' That is, the \code{within} argument is used to identify variables in the data
#' frame specified in \code{data} that are measured at the individual level and
#' modeled only at the within level. They are specified to have no variance in
#' the between part of the model. The \code{between} argument is used to identify
#' the variables in the data frame specified in \code{data} that are measured at
#' the cluster level and modeled only at the between level. Variables not mentioned
#' in the arguments \code{within} or \code{between} are measured at the individual
#' level and will be modeled at both the within and between level.}
#' \item{\strong{Estimation Method and Missing Data Handling}}{The default setting
#' for the argument \code{estimator} is depending on the setting of the argument
#' \code{sig}. If \code{sig = FALSE} (default), maximum likelihood estimation
#' (\code{estimator = "ML"}) is used, while maximum likelihood with Huber-White
#' robust standard errors (\code{estimator = "MLR"}) that are robust against
#' non-normality is used when \code{sig = TRUE}. In the presence of missing data,
#' full information maximum likelihood (FIML) method (\code{missing = "fiml"}) is
#' used by default. Note that FIML method cannot deal with within-group variables
#' that have no variance within some clusters. In this
#' cases, the function will switch to listwise deletion. Using FIML method might
#' result in issues with model convergence, which will be resolved by switching
#' to listwise deletion (\code{missing = "listwise"}).}
#' \item{\strong{Optimizer}}{The lavaan package uses a quasi-Newton optimization
#' method (\code{"nlminb"}) by default. If the optimizer does not converge, model
#' estimation switches to the Expectation Maximization (EM) algorithm (\code{"em"})
#' if the argument \code{optim.switch} is specified as \code{TRUE} (default).}
#' \item{\strong{Statistical Significance}}{Statistically significant correlation
#' coefficients can be shown in boldface on the console by specifying \code{sig = TRUE}.
#' However, this option is not supported when using R Markdown, i.e., the argument
#' \code{sig} will switch to \code{FALSE}.}
#' \item{\strong{Adjustment Method for Multiple Testing }}{Adjustment method for
#' multiple testing when specifying the argument \code{p.adj} is applied to
#' the within-group and between-group correlation matrix separately.}
#' }
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{multilevel.descript}}, \code{\link{multilevel.icc}},
#' \code{\link{multilevel.cfa}}, \code{\link{cluster.scores}},
#' \code{\link{write.result}}
#'
#' @references
#' Hox, J., Moerbeek, M., & van de Schoot, R. (2018). \emph{Multilevel analysis:
#' Techniques and applications} (3rd. ed.). Routledge.
#'
#' Snijders, T. A. B., & Bosker, R. J. (2012). \emph{Multilevel analysis: An
#' introduction to basic and advanced multilevel modeling} (2nd ed.). Sage
#' Publishers.
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{data}}{data frame specified in \code{data} including the group variable
#'                    specified in \code{cluster}}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{model.fit}}{fitted lavaan object (\code{mod.fit})}
#' \item{\code{result}}{list with result tables, i.e., \code{summary} for the
#'                      specification of the estimation method and missing data
#'                      handling in lavaan, \code{wb.cor} for the within- and
#'                      between-group correlations, \code{wb.se} for the standard
#'                      error of the within- and between-group correlations,
#'                      \code{wb.stat} for the test statistic of within- and between-group
#'                      correlations, \code{wb.p} for the significance value of
#'                      the within- and between-group correlations, \code{with.cor}
#'                      for the within-group correlations, \code{with.se} for the
#'                      standard error of the within-group correlations, \code{with.stat}
#'                      for the test statistic of within-group correlations, \code{with.p}
#'                      for the significance value of the within-group correlations,
#'                      \code{betw.cor} for the between-group correlations, \code{betw.se}
#'                      for the standard error of the between-group correlations,
#'                      \code{betw.stat} for the test statistic of between-group
#'                      correlations, \code{betw.p} for the significance value of
#'                      the between-group correlations}
#'
#' @note
#' The function uses the functions \code{sem}, \code{lavInspect},
#' \code{lavMatrixRepresentation}, \code{lavTech}, \code{parameterEstimates},
#' and \code{standardizedsolution} provided in the R package \pkg{lavaan} by
#' Yves Rosseel (2012).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load data set "Demo.twolevel" in the lavaan package
#' data("Demo.twolevel", package = "lavaan")
#'
#' #----------------------------------------------------------------------------
#' # Cluster variable specification
#'
#' # Example 1: Specification using the argument '...'
#' multilevel.cor(Demo.twolevel, y1, y2, y3, cluster = "cluster")
#'
#' # Alternative specification with cluster variable 'cluster' in 'data'
#' multilevel.cor(Demo.twolevel[, c("y1", "y2", "y3", "cluster")], cluster = "cluster")
#'
#' # Alternative specification with cluster variable 'cluster' not in 'data'
#' multilevel.cor(Demo.twolevel[, c("y1", "y2", "y3")], cluster = Demo.twolevel$cluster)
#'
#' #----------------------------------------------------------------------------
#' # Example 2: All variables modeled at both the within and between level
#' # Highlight statistically significant result at alpha = 0.05
#' multilevel.cor(Demo.twolevel, y1, y2, y3, sig = TRUE, cluster = "cluster")
#'
#' # Example 3: Split output table in within-group and between-group correlation matrix.
#' multilevel.cor(Demo.twolevel, y1, y2, y3, cluster = "cluster", split = TRUE)
#'
#' # Example 4: Print correlation coefficients, standard errors, z test statistics,
#' # and p-values
#' multilevel.cor(Demo.twolevel, y1, y2, y3, cluster = "cluster", sig = TRUE, print = "all")
#'
#' # Example 5: Print correlation coefficients and p-values
#' # significance values with Bonferroni correction
#' multilevel.cor(Demo.twolevel, y1, y2, y3, cluster = "cluster", sig = TRUE,
#'                print = c("cor", "p"), p.adj = "bonferroni")
#'
#' #----------------------------------------------------------------------------
#' # Example 6: Variables "y1", "y2", and "y2" modeled at both the within and between level
#' # Variables "w1" and "w2" modeled at the cluster level
#' multilevel.cor(Demo.twolevel, y1, y2, y3, w1, w2, cluster = "cluster",
#'                between = c("w1", "w2"))
#'
#' # Example 7: Show variables specified in the argument 'between' first
#' multilevel.cor(Demo.twolevel, y1, y2, y3, w1, w2, cluster = "cluster",
#'                between = c("w1", "w2"), order = TRUE)
#'
#' #----------------------------------------------------------------------------
#' # Example 8: Variables "y1", "y2", and "y2" modeled only at the within level
#' # Variables "w1" and "w2" modeled at the cluster level
#' multilevel.cor(Demo.twolevel, y1, y2, y3, w1, w2, cluster = "cluster",
#'                within = c("y1", "y2", "y3"), between = c("w1", "w2"))
#'
#' #----------------------------------------------------------------------------
#' # Example 9: lavaan model and summary of the multilevel model used to compute the
#' # within-group and between-group correlation matrix
#'
#' mod <- multilevel.cor(Demo.twolevel, y1, y2, y3, cluster = "cluster", output = FALSE)
#'
#' # lavaan model syntax
#' mod$model
#'
#' # Fitted lavaan object
#' lavaan::summary(mod$model.fit, standardized = TRUE)
#'
#' #----------------------------------------------------------------------------
#' # Write Results
#'
#' # Example 10a: Write Results into a text file
#' multilevel.cor(Demo.twolevel, y1, y2, y3, cluster = "cluster",
#'                write = "Multilevel_Correlation.txt")
#'
#' # Example 10b: Write Results into a Excel file
#' multilevel.cor(Demo.twolevel, y1, y2, y3, cluster = "cluster",
#'                write = "Multilevel_Correlation.xlsx")
#' }
multilevel.cor <- function(data, ..., cluster, within = NULL, between = NULL,
                           estimator = c("ML", "MLR"), optim.method = c("nlminb", "em"),
                           optim.switch = TRUE, missing = c("listwise", "fiml"),
                           sig = FALSE, alpha = 0.05, print = c("all", "cor", "se", "stat", "p"),
                           split = FALSE, order = FALSE, tri = c("both", "lower", "upper"),
                           tri.lower = TRUE, p.adj = c("none", "bonferroni", "holm", "hochberg", "hommel", "BH", "BY", "fdr"),
                           digits = 2, p.digits = 3, as.na = NULL, write = NULL,
                           append = TRUE, check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'data' is missing or NULL
  if (isTRUE(missing(data) || is.null(data))) { stop("Please specify a data frame for the argument 'data'", call. = FALSE) }

  # Check input 'cluster'
  if (isTRUE(missing(cluster) ||is.null(cluster))) { stop("Please specify a variable name or vector representing the grouping structure for the argument 'cluster'.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data using the argument '...' ####

  if (isTRUE(!missing(...))) {

    # Extract data
    x <- as.data.frame(data[, .var.names(data = data, ..., cluster = cluster), drop = FALSE])

    # Cluster variable
    cluster <- data[, cluster]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data without using the argument '...' ####

  } else {

    # Data frame
    x <- as.data.frame(data)

    # Data and cluster
    var.group <- .var.group(data = x, cluster = cluster)

    # Data
    if (isTRUE(!is.null(var.group$data))) { x <- var.group$data }

    # Cluster variable
    if (isTRUE(!is.null(var.group$cluster))) { cluster <- var.group$cluster }

  }

  # Convert 'cluster' as tibble into a vector
  if (!is.null(cluster) && isTRUE("tbl" %in% substr(class(cluster), 1L, 3L))) { cluster <- unname(unlist(cluster)) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Numeric Variables ####

  x <- (!vapply(as.data.frame(x), is.numeric, FUN.VALUE = logical(1L))) |> (\(y) if (isTRUE(any(y))) {

    warning(paste0("Non-numeric variables were excluded from the analysis: ", paste(names(which(y)), collapse = ", ")), call. = FALSE)

    return(as.data.frame(x)[, -which(y), drop = FALSE])

  } else {

    return(x)

  })()

  if (isTRUE(ncol(x) == 0L)) { stop("No variables left for analysis after excluding non-numeric variables.", call. = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) { x <- .as.na(x, na = as.na) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("optim.switch", "sig", "split", "order", "tri.lower", "append", "output"),
               s.character = list(estimator = c("ML", "MLR"), optim.method = c("nlminb", "em"), missing = c("listwise", "fiml"), tri = c("both", "lower", "upper")),
               m.character = list(print = c("all", "cor", "se", "stat", "p")),
               args = c("alpha", "p.adj", "digits", "p.digits", "write2"),
               package = "lavaan", envir = environment(), input.check = check)

  # Within- and Between-Group Variables Check
  if (isTRUE(check)) {

    # Variables in 'within' and 'between'
    intersect(within, between) |> (\(y) if (isTRUE(length(y) > 0L)) { stop(paste0("Following ", ifelse(length(y) == 1L, "variable is ", "variables are "), "specified in both arguments 'within' and 'between': ", paste(y, collapse = ", ")), call. = FALSE) })()

    #...................
    ### Within-Group Variable ####

    # Within variables in 'data'
    (!within %in% colnames(x)) |> (\(y) if (isTRUE(any(y))) { stop(paste0(ifelse(length(which(y)) == 1L, "Variable ", "Variables "), "specified in the argument 'within' ", ifelse(length(which(y)) == 1L, "was ", "were "), "not found in 'data': ", paste(within[which(y)], collapse = ", ")), call. = FALSE) })()

    # Variance within clusters
    vapply(x[, within, drop = FALSE], function(y) all(tapply(y, cluster, var, na.rm = TRUE) < .Machine$double.eps^0.5), FUN.VALUE = logical(1L)) |> (\(y) if (isTRUE(any(y))) { stop(paste0("Following within-group ", ifelse(length(which(y)) == 1L, "variable has ", "variables have "), "no variance within clusters: ", paste(names(which(y)), collapse = ", ")), call. = FALSE) })()

    #...................
    ### Between-Group Variable ####

    # Between variables in 'data'
    (!between %in% colnames(x)) |> (\(y) if (isTRUE(any(y))) { stop(paste0(ifelse(length(which(y)) == 1L, "Variable ", "Variables "), "specified in the argument 'between' ", ifelse(length(which(y)) == 1L, "was ", "were "), "not found in 'data': ", paste(between[which(y)], collapse = ", ")), call. = FALSE) })()

    # Variance within clusters
    vapply(x[, between, drop = FALSE], function(y) any(tapply(y, cluster, var, na.rm = TRUE) != 0L), FUN.VALUE = logical(1L)) |> (\(y) if (isTRUE(any(y))) { stop(paste0("Following between-group ", ifelse(length(which(y)) == 1L, "variable has ", "variables have "), "variance within clusters: ", paste(names(which(y)), collapse = ", ")), call. = FALSE) })()

    #...................
    ### Within-/Between-Group Variable ####

    # Variance between clusters
    vapply(x[, setdiff(colnames(x), c(within, between)), drop = FALSE], function(y) misty::multilevel.icc(y, cluster = cluster) < .Machine$double.eps^0.5, FUN.VALUE = logical(1L)) |> (\(y) if (isTRUE(any(y))) { stop(paste0("Following ", ifelse(length(which(y)) == 1L, "variable has ", "variables have "), "no variance between clusters and should be specified in the 'within' argument: ", paste(names(which(y)), collapse = ", ")), call. = FALSE) })()

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Within-Group and Between Variables ####

  # Within Only and Within/Between Variables
  if (isTRUE(is.null(between))) { var.with <- colnames(x) } else { var.with <- setdiff(colnames(x), between) }

  # At least one within variables
  if (isTRUE(length(var.with) == 0L)) { stop("Please specify at least one within-group variable.", call. = FALSE) }

  # Between Only and Within/Between variables
  if (isTRUE(is.null(within))) { var.betw <- colnames(x) } else { var.betw <- setdiff(colnames(x), within) }

  # At least one between variables
  if (isTRUE(length(var.betw) == 0L)) { stop("Please specify at least one between-group variable.", call. = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data frame with Cluster Variable ####

  x <- data.frame(x[, unique(c(var.with, var.betw))], .cluster = cluster)

  n.total <- nrow(x)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Missing Data on the Cluster Variable ####

  if (isTRUE(any(is.na(cluster)))) {

    warning(paste0("Data contains missing values on the cluster variable, number of cases removed from the analysis: ", sum(is.na(cluster))), call. = FALSE)

    x <- x[!is.na(cluster), ]

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Estimator ####

  # Default setting: "MLR" if sig = TRUE, "ML" if sig = FALSE
  estimator <- if (isTRUE(sig)) { ifelse(all(c("ML", "MLR") %in% estimator), "MLR", estimator) } else { "ML" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Standard Errors ####

  # Default setting: "robust.huber.white" if sig = TRUE, "none" if sig = FALSE
  se <- if (isTRUE(sig)) { ifelse(estimator == "MLR", "robust.huber.white", "standard") } else { "none" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Optimizer ####

  if (isTRUE(all(c("nlminb", "em") %in% optim.method))) { optim.method <- "nlminb" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Missing data ####

  #...................
  ### Missing values present ####
  if (isTRUE(any(is.na(x)))) {

    complete <- FALSE

    if (isTRUE(all(c("listwise", "fiml") %in% missing))) { missing <- "fiml" }

  #...................
  ### No missing values ####
  } else {

    complete <- TRUE
    missing <- "listwise"

  }

  # Cases with missing on all variables
  if (isTRUE(missing == "fiml")) {

    x <- misty::na.prop(x[, -which(colnames(x) %in% c(".cluster", between)), drop = FALSE], append = FALSE) |>
      (\(y) if (any(y == 1L)) {

        warning(paste0("Data contains cases with missing values on all variables measured at the within level, number of cases removed from the analysis: ", sum(y == 1L)), call. = FALSE)

        # Remove cases with missing on all variables
        return(x[which(y < 1L), ])

      } else {

        return(x)

      })()

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Print correlation, sample size or significance values ####

  if (isTRUE(all(c("all", "cor", "se", "stat", "p") %in% print))) { print <- "cor" }

  if (isTRUE(length(print) == 1L && "all" %in% print)) { print <- c("cor", "se", "stat", "p") }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Print triangular ####

  tri <- ifelse(all(c("both", "lower", "upper") %in% tri), "lower", tri)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Adjustment method for multiple testing ####

  p.adj <- ifelse(all(c("none", "bonferroni", "holm", "hochberg", "hommel", "BH", "BY", "fdr") %in% p.adj), "none", p.adj)

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model specification ####

  # At least two within- and two between-group variables
  if (isTRUE(length(var.with) >= 2L && length(var.betw) >= 2L)) {

    mod <- paste(" level: 1\n  ",
                 # Within model
                 paste(apply(combn(length(var.with), 2L), 2L, function(y) paste(var.with[y[1L]], var.with[y[2L]], sep = " ~~ " )), collapse = "\n   "),
                 "\n level: 2\n  ",
                 # Between model
                 paste(apply(combn(length(var.betw), 2L), 2L, function(y) paste(var.betw[y[1L]], var.betw[y[2L]], sep = " ~~ " )), collapse = "\n   "))

  # At least two within-group variable, but only one between-group variable
  } else if (isTRUE(length(var.with) >= 2L && length(var.betw) == 1L)){

    mod <- paste(" level: 1\n  ",
                 # Within model
                 paste(apply(combn(length(var.with), 2L), 2L, function(y) paste(var.with[y[1L]], var.with[y[2L]], sep = " ~~ " )), collapse = "\n   "),
                 "\n level: 2\n  ",
                 # Between model
                 paste(var.betw, var.betw, sep = " ~~ "), collapse = "\n   ")

  # At least two between-group variable, but only one within-group variable
  } else if (isTRUE(length(var.with) == 1L && length(var.betw) >= 2L)){

    mod <- paste(" level: 1\n  ",
                 # Within model
                 paste(var.with, var.with, sep = " ~~ " ), collapse = "\n   ",
                 "\n level: 2\n  ",
                 # Between model
                 paste(apply(combn(length(var.betw), 2L), 2L, function(y) paste(var.betw[y[1L]], var.betw[y[2L]], sep = " ~~ " )), collapse = "\n   "))

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model estimation ####

  model.fit <- tryCatch(suppressWarnings(lavaan::sem(mod, data = x, cluster = ".cluster", estimator = estimator,
                                                     missing = missing, optim.method = optim.method, se = se,
                                                     check.gradient = FALSE, check.post = FALSE, check.vcov = FALSE)),
                        error = function(y) {

                          if (isTRUE(missing == "fiml")) {

                            stop("There was an estimation problem in lavaan, switching to missing = \"listwise\" might solve the problem.", call. = FALSE)

                          } else if (isTRUE(estimator == "MLR")){

                            stop("There was an estimation problem in lavaan, switching to estimator = \"ML\" might solve the problem.", call. = FALSE)

                          } else {

                            stop("There was an estimation problem in lavaan, correlation matrix could not be computed.", call. = FALSE)

                          }})

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convergence check ####

  if (isTRUE(!lavaan::lavInspect(model.fit, what = "converged"))) {

    #...................
    ### Quasi-Newton method optimizer ####

    if (isTRUE(optim.method == "nlminb")) {

      if (isTRUE(optim.switch)) {

        message("Quasi-Newton optimizer did not converge, switched to the EM algorithm.")

        # Model estimation with EM algorithm
        model.fit <- suppressWarnings(lavaan::sem(mod, data = x, cluster = ".cluster", estimator = estimator,
                                                  missing = missing, optim.method = "em",
                                                  se = ifelse(estimator == "MLR", "robust.huber.white", "standard"),
                                                  check.gradient = FALSE, check.post = FALSE, check.vcov = FALSE))

        # Model not converged
        if (isTRUE(!lavaan::lavInspect(model.fit, what = "converged"))) {

          if (isTRUE(missing == "listwise")) {

            stop("EM algorithm did not converge.", call. = FALSE)

          } else {

            stop("EM algorithm did not converge. Switching to missing = \"listwise\" might solve the estimation problem.", call. = FALSE)

          }

        }

      }

    #...................
    ### Expectation Maximization (EM) algorithm ####

    } else if(isTRUE(optim.method == "em")) {

      if (isTRUE(missing == "listwise")) {

        stop("EM algorithm did not converge.", call. = FALSE)

      } else {

        stop("EM algorithm did not converge. Switching to missing = \"listwise\" might solve the estimation problem.", call. = FALSE)

      }

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convergence and model identification checks ####

  if (isTRUE(check)) {

    check.vcov <- check.theta.w <- check.theta.b <- check.cov.lv.w <- check.cov.lv.b <- TRUE

    if (isTRUE(se != "none")) {

      #...................
      ### Standard error ####

      if (isTRUE(any(is.na(unlist(lavaan::lavInspect(model.fit, what = "se")))))) { stop("Standard errors could not be computed.", call. = FALSE) }

      #...................
      ### Variance-covariance matrix of the estimated parameters ####

      eigvals <- eigen(lavaan::lavInspect(model.fit, what = "vcov"), symmetric = TRUE, only.values = TRUE)$values

      # Model contains equality constraints
      model.fit.par <- lavaan::parameterTable(model.fit)$op == "=="

      if (isTRUE(any(model.fit.par))) { eigvals <- rev(eigvals)[-seq_len(sum(model.fit.par))] }

      if (isTRUE(min(eigvals) < .Machine$double.eps^(3L/4L))) {

        warning("The variance-covariance matrix of the estimated parameters is not positive definite. This may be a symptom that the model is not identified.", call. = FALSE)

        check.vcov <- FALSE

      }

    }

    #...................
    ### Negative variance of observed variables ####

    #### Within Level
    if (isTRUE(any(diag(lavaan::lavInspect(model.fit, what = "theta")$within) < 0L))) {

      warning("Some estimated variances of the observed variables at the Within level are negative.", call. = FALSE)

      check.theta.w <- FALSE

    } else if (isTRUE(any(eigen(lavaan::lavTech(model.fit, what = "theta")$within, symmetric = TRUE, only.values = TRUE)$values < (-1L * .Machine$double.eps^(3/4))))) {

      warning("The model-implied variance-covariance matrix of the residuals of the observed variables is not positive definite indicating an absolute residual correlations greater one.", call. = FALSE)

      check.theta.w <- FALSE

    }

    #### Between Level
    if (isTRUE(any(diag(lavaan::lavInspect(model.fit, what = "theta")$.cluster) < 0L))) {

      warning("Some estimated variances of the observed variables at the Between level are negative.", call. = FALSE)

      check.theta.b <- FALSE

    } else if (isTRUE(any(eigen(lavaan::lavTech(model.fit, what = "theta")$.cluster, symmetric = TRUE, only.values = TRUE)$values < (-1L * .Machine$double.eps^(3/4))))) {

      warning("The model-implied variance-covariance matrix of the residuals of the observed variables at the Between level is not positive definite indicating an absolute residual correlations greater one.", call. = FALSE)

      check.theta.b <- FALSE

    }

    #...................
    ### Negative variance of latent variables ####

    #### Within Level
    if (isTRUE(!is.null(lavaan::lavTech(model.fit, what = "cov.lv")$within))) {

      if (isTRUE(any(diag(lavaan::lavTech(model.fit, what = "cov.lv")$within) < 0L))) {

        warning("Some estimated variances of the latent variables at the Within level are negative.", call. = FALSE)

        check.cov.lv.w <- FALSE

      }

    # Model-implied variance-covariance matrix of the latent variables
    } else if (any(dim(lavaan::lavTech(model.fit, what = "cov.lv")$within) != 0L)) {

      if (isTRUE(any(eigen(lavaan::lavTech(model.fit, what = "cov.lv")$within, symmetric = TRUE, only.values = TRUE)$values < (-1L * .Machine$double.eps^(3/4))))) {

        warning("The model-implied variance-covariance matrix of the latent variables at the Within level is not positive definite indicating an absolute correlation greater one.", call. = FALSE)

        check.cov.lv.w <- FALSE

      }

    }

    #### Between Level
    if (isTRUE(!is.null(lavaan::lavTech(model.fit, what = "cov.lv")$cluster))) {

      if (isTRUE(any(diag(lavaan::lavTech(model.fit, what = "cov.lv")$.cluster) < 0L))) {

        warning("Some estimated variances of the latent variables at the Between level are negative.", call. = FALSE)

        check.cov.lv.b <- FALSE

      }

    # Model-implied variance-covariance matrix of the latent variables
    } else if (any(dim(lavaan::lavTech(model.fit, what = "cov.lv")$.cluster) != 0L)) {

      if (isTRUE(any(eigen(lavaan::lavTech(model.fit, what = "cov.lv")$.cluster, symmetric = TRUE, only.values = TRUE)$values < (-1L * .Machine$double.eps^(3/4))))) {

        warning("The model-implied variance-covariance matrix of the latent variables at the Between level is not positive definite indicating an absolute correlation greater one.", call. = FALSE)

        check.cov.lv.b <- FALSE

      }

    }

  } else {

    check.vcov <- check.theta.w <- check.theta.b <- check.cov.lv.w <- check.cov.lv.b <- NULL

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Extract Results ####

  # Standardized solution
  stand <- lavaan::lavMatrixRepresentation(lavaan::standardizedSolution(model.fit))

  # Visible binding for global variable
  mat <- level <- NULL

  #...................
  ### Within-Group Results ####

  # Theta
  with.stand.theta <- subset(stand[ unlist(subset(lavaan::lavMatrixRepresentation(lavaan::parameterestimates(model.fit)), level == 1L, select = "id")), ], mat == "theta")

  # Parameter estimate, z and significance value matrix
  with.p <- with.stat <- with.se <- with.cor <- matrix(NA, ncol = max(stand[, "col"]), nrow = max(stand[, "row"]))

  # Model estimation with SE
  if (isTRUE(se != "none")) {

    for (i in seq_len(nrow(with.stand.theta))) {

      with.cor[with.stand.theta[i, "row"], with.stand.theta[i, "col"]] <- with.stand.theta[i, "est.std"]
      with.se[with.stand.theta[i, "row"], with.stand.theta[i, "col"]] <- with.stand.theta[i, "se"]
      with.stat[with.stand.theta[i, "row"], with.stand.theta[i, "col"]] <- with.stand.theta[i, "z"]
      with.p[with.stand.theta[i, "row"], with.stand.theta[i, "col"]] <- with.stand.theta[i, "pvalue"]

    }

  # Model estimation without SE
  } else {

    for (i in seq_len(nrow(with.stand.theta))) {

      with.cor[with.stand.theta[i, "row"], with.stand.theta[i, "col"]] <- with.stand.theta[i, "est.std"]

    }

  }

  with.cor[lower.tri(with.cor)] <- t(with.cor)[lower.tri(with.cor)]
  with.se[lower.tri(with.se)] <- t(with.se)[lower.tri(with.se)]
  with.stat[lower.tri(with.stat)] <- t(with.stat)[lower.tri(with.stat)]
  with.p[lower.tri(with.p)] <- t(with.p)[lower.tri(with.p)]

  colnames(with.cor) <- colnames(with.se) <- colnames(with.stat) <- colnames(with.p) <-
  rownames(with.cor) <- rownames(with.se) <- rownames(with.stat) <- rownames(with.p) <- sapply(seq_len(max(stand[, "row"])), function(y) unique(stand[which(y == stand$row), "lhs"]))

  #...................
  ### Between-Group Results ####

  # Standardized solution
  betw.stand.theta <- subset(stand[ unlist(subset(lavaan::lavMatrixRepresentation(lavaan::parameterestimates(model.fit)), level == 2L, select = "id")), ], mat == "theta")

  # Parameter estimate, z and significance value matrix
  betw.p <- betw.stat <- betw.se <- betw.cor <- matrix(NA, ncol = max(stand[, "col"]), nrow = max(stand[, "row"]))

  # Model estimation with SE
  if (isTRUE(se != "none")) {

    for (i in seq_len(nrow(betw.stand.theta))) {

      betw.cor[betw.stand.theta[i, "row"], betw.stand.theta[i, "col"]] <- betw.stand.theta[i, "est.std"]
      betw.se[betw.stand.theta[i, "row"], betw.stand.theta[i, "col"]] <- betw.stand.theta[i, "se"]
      betw.stat[betw.stand.theta[i, "row"], betw.stand.theta[i, "col"]] <- betw.stand.theta[i, "z"]
      betw.p[betw.stand.theta[i, "row"], betw.stand.theta[i, "col"]] <- betw.stand.theta[i, "pvalue"]

    }

  # Model estimation without SE
  } else {

    for (i in seq_len(nrow(betw.stand.theta))) {

      betw.cor[betw.stand.theta[i, "row"], betw.stand.theta[i, "col"]] <- betw.stand.theta[i, "est.std"]

    }

  }

  betw.cor[lower.tri(betw.cor)] <- t(betw.cor)[lower.tri(betw.cor)]
  betw.se[lower.tri(betw.se)] <- t(betw.se)[lower.tri(betw.se)]
  betw.stat[lower.tri(betw.stat)] <- t(betw.stat)[lower.tri(betw.stat)]
  betw.p[lower.tri(betw.p)] <- t(betw.p)[lower.tri(betw.p)]

  colnames(betw.cor) <- colnames(betw.se) <- colnames(betw.stat) <- colnames(betw.p) <-
  rownames(betw.cor) <- rownames(betw.se) <- rownames(betw.stat) <- rownames(betw.p) <- sapply(seq_len(max(stand[, "row"])), function(y) unique(stand[which(y == stand$row), "lhs"]))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Order Between Variables ####

  if (isTRUE(order && !is.null(between))) {

    pos.betw <- which(colnames(betw.cor) %in% between)
    pos.with <- which(!colnames(betw.cor) %in% between)

    with.cor <- with.cor[c(pos.betw, pos.with), c(pos.betw, pos.with)]
    with.se <- with.se[c(pos.betw, pos.with), c(pos.betw, pos.with)]
    with.stat <- with.stat[c(pos.betw, pos.with), c(pos.betw, pos.with)]
    with.p <- with.p[c(pos.betw, pos.with), c(pos.betw, pos.with)]

    betw.cor <- betw.cor[c(pos.betw, pos.with), c(pos.betw, pos.with)]
    betw.se <- betw.se[c(pos.betw, pos.with), c(pos.betw, pos.with)]
    betw.stat <- betw.stat[c(pos.betw, pos.with), c(pos.betw, pos.with)]
    betw.p <- betw.p[c(pos.betw, pos.with), c(pos.betw, pos.with)]

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Combine Within-Group and Between-Group Results ####

  #...................
  ### Within-group correlations in the lower triangular ####
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

  #...................
  ### Within-group correlations in the upper triangular ####
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

  #...................
  ### Adjust p-values for multiple comparison ####
  if (isTRUE(p.adj != "none")) {

    wb.p[lower.tri(wb.p)] <- p.adjust(wb.p[lower.tri(wb.p)], method = p.adj)
    wb.p[upper.tri(wb.p)] <- p.adjust(wb.p[upper.tri(wb.p)], method = p.adj)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Split Within-Group and Between-Group Results ####

  # Model estimation with SE
  if (isTRUE(se != "none")) {

    # Within-group results
    with.cor <- with.cor[which(apply(with.cor, 1L, function(y) !all(is.na(y)))), which(apply(with.cor, 2L, function(y) !all(is.na(y))))]
    with.se <- with.se[which(apply(with.se, 1L, function(y) !all(is.na(y)))), which(apply(with.se, 2L, function(y) !all(is.na(y))))]
    with.stat <- with.stat[which(apply(with.stat, 1L, function(y) !all(is.na(y)))), which(apply(with.stat, 2L, function(y) !all(is.na(y))))]
    with.p <- with.p[which(apply(with.p, 1L, function(y) !all(is.na(y)))), which(apply(with.p, 2L, function(y) !all(is.na(y))))]

    # Between-group results
    betw.cor <- betw.cor[which(apply(betw.cor, 1L, function(y) !all(is.na(y)))), which(apply(betw.cor, 2L, function(y) !all(is.na(y))))]
    betw.se <- betw.se[which(apply(betw.se, 1L, function(y) !all(is.na(y)))), which(apply(betw.se, 2L, function(y) !all(is.na(y))))]
    betw.stat <- betw.stat[which(apply(betw.stat, 1L, function(y) !all(is.na(y)))), which(apply(betw.stat, 2L, function(y) !all(is.na(y))))]
    betw.p <- betw.p[which(apply(betw.p, 1L, function(y) !all(is.na(y)))), which(apply(betw.p, 2L, function(y) !all(is.na(y))))]

  # Model estimation without SE
  } else {

    # Within-group results
    with.cor <- with.cor[which(apply(with.cor, 1L, function(y) !all(is.na(y)))), which(apply(with.cor, 2L, function(y) !all(is.na(y))))]

    # Between-group results
    betw.cor <- betw.cor[which(apply(betw.cor, 1L, function(y) !all(is.na(y)))), which(apply(betw.cor, 2L, function(y) !all(is.na(y))))]

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Adjust p-values for multiple comparison ####

  if (isTRUE(p.adj != "none")) {

    with.p[lower.tri(with.p)] <- p.adjust(with.p[lower.tri(with.p)], method = p.adj)
    with.p[upper.tri(with.p)] <- p.adjust(with.p[upper.tri(with.p)], method = p.adj)

    betw.p[lower.tri(betw.p)] <- p.adjust(betw.p[lower.tri(betw.p)], method = p.adj)
    betw.p[upper.tri(betw.p)] <- p.adjust(betw.p[upper.tri(betw.p)], method = p.adj)

  }

  #_____________________________________________________________________________
  #
  # Return object --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## lavaan summary ####

  lavaan.summary <- data.frame(c(paste("lavaan", lavaan::lavInspect(model.fit, what = "version")), "", "Estimator", "Standard Errors", "Missing Data", "", "",
                                 "Number of Observations", "Number of Clusters"),
                               c("", "",
                                 # Estimator
                                 estimator,
                                 # Standard errors
                                 switch(lavaan::lavTech(model.fit, what = "options")$se,
                                        "none" = "None",
                                        "standard" = "Conventional",
                                        "robust.huber.white" = "Huber-White"),
                                 # Missing data
                                 ifelse(isTRUE(complete), "None",
                                        switch(missing,
                                               "listwise" = "Listwise Deletion",
                                               "fiml" = "FIML")), "", "Used",
                                 # Number of observations
                                 lavaan::lavInspect(model.fit, what = "nobs"),
                                 # Number of clusters
                                 lavaan::lavInspect(model.fit, what = "nclusters")),
                               c(rep("", times = 6L), "Total", n.total, ""),
                               fix.empty.names = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Object ####

  object <- list(call = match.call(),
                 type = "multilevel.cor",
                 data = x,
                 args = list(within = within, between = between, estimator = estimator, optim.method = optim.method, se = se, optim.switch = optim.switch, missing = missing, sig = sig, alpha = alpha, print = print,
                             split = split, order = order, tri = tri, tri.lower = tri.lower, p.adj = p.adj, digits = digits, p.digits = p.digits, as.na = as.na, write = write, append = append, check = check, output = output),
                 model = mod,
                 model.fit = model.fit,
                 check = list(vcov = check.vcov, theta.w = check.theta.w, theta.b = check.theta.b,
                              cov.lv.w = check.cov.lv.w, cov.lv.b = check.cov.lv.b),
                 result = list(summary = lavaan.summary,
                               wb.cor = wb.cor, wb.se = wb.se, wb.stat = wb.stat, wb.p = wb.p,
                               with.cor = with.cor, with.se = with.se, with.stat = with.stat, with.p = with.p,
                               betw.cor = betw.cor, betw.se = betw.se, betw.stat = betw.stat, betw.p = betw.p))

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Write Results --------------------------------------------------------------

  if (isTRUE(!is.null(write))) { .write.result(object = object, write = write, append = append) }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}

#_______________________________________________________________________________
