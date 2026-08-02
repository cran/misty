#' Within-Group and Between-Group Correlation Matrix
#'
#' This function computes the within-group and between-group correlation matrix
#' by calling the \code{sem} function in the R package \pkg{lavaan} and provides
#' standard errors, z test statistics, and significance values (\emph{p}-values)
#' for testing the hypothesis H0: \eqn{\rho} = 0 for all pairs of variables within
#' and between groups. By default, the function provides the within-group and
#' between-group correlation matrix. Statistically significant correlations can
#' be highlighted by specifying the argument \code{color}.
#'
#' @param data         a data frame.
#' @param ...          an expression indicating the variable names in \code{data},
#'                     e.g., \code{multilevel.cor(dat, x1, x2, x3)}. Note that
#'                     the operators \code{+}, \code{-}, \code{~}, \code{:},
#'                     \code{::}, and \code{!} can also be used to select variables,
#'                     see 'Details' in the \code{\link{df.subset}} function.
#' @param cluster      either a character string indicating the variable name of
#'                     the cluster variable in \code{data} or a vector representing
#'                     the nested grouping structure (i.e., group or cluster variable).
#' @param estimator    a character string indicating the estimator to be used, i.e.,
#'                     \code{"ML"} for maximum likelihood with conventional
#'                     standard errors and \code{"MLR"} for maximum likelihood with
#'                     Huber-White robust standard errors. The default setting
#'                     depends on the argument \code{color}, i.e., \code{"ML"} is
#'                     used when specifying \code{color = "default"} (default) and
#'                     \code{"MLR"} is used when specifying a color for the
#'                     argument \code{color}.
#' @param constr.var   logical: if \code{TRUE}, inequality constraints are imposed
#'                     for the variance parameters at the between level, i.e.,
#'                     variances are constrained to be greater than 0.
#' @param optim.method a character string indicating the optimizer, i.e., \code{"nlminb"}
#'                     for the unconstrained and bounds-constrained quasi-Newton
#'                     method optimizer and \code{"em"} for the Expectation Maximization
#'                     (EM) algorithm. By default, the optimizer \code{"nlminb"}
#'                     is used with complete data or when using listwise deletion
#'                     for missing data handling, while the optimizer \code{"em"}
#'                     is used when using for full information maximum likelihood
#'                     (FIML) method for missing data handling.
#' @param optim.switch logical: if \code{TRUE} (default), model estimation switches
#'                     to Expectation Maximization (EM) algorithm (\code{"em"})
#'                     if the quasi-Newton optimization (\code{"nlminb"})
#'                     does not converge.
#' @param print        a character string or character vector indicating which
#'                     results to show on the console, i.e. \code{"all"} for all
#'                     results, \code{"summary"} for a summary of the specification
#'                     of the estimation method and missing data handling in lavaan,
#'                     \code{"cor"} (default) for correlation coefficients, \code{"se"}
#'                     for standard errors, \code{"stat"} for z test statistics,
#'                     and \code{"p"} for \emph{p}-values. By default, the function
#'                     only prints the within-group and between-group correlation
#'                     matrix.
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
#' @param missing      a character string indicating how to deal with missing
#'                     data, i.e., \code{"listwise"} for listwise deletion or
#'                     \code{"fiml"} (default) for full information maximum
#'                     likelihood (FIML) method. Note that it takes longer to
#'                     estimate models while using FIML and using FIML is
#'                     prone to issues with model convergence, these issues might
#'                     be resolved by switching to listwise deletion.
#' @param alpha        a numeric value between 0 and 1 indicating the significance
#'                     level at which correlation coefficients are printed
#'                     boldface when specifying the argument \code{color}.
#' @param color        a character string indicating the text color for highlighting
#'                     statistically significant correlation coefficients, i.e.,
#'                     \code{"default"} (default) for the default text color without
#'                     color coding and various text colors for highlighting like
#'                     \code{"red"}, \code{"b.red"}, \code{"green"}, \code{"b.green"},
#'                     \code{"blue"}, or \code{"b.blue"}, see the help page of the
#'                     \code{\link{chr.color}} function. Note that this option is
#'                     not supported when using R Markdown and when writing the output
#'                     into a text file (\code{.txt}).
#' @param style        a character vector indicating the font style for
#'                     statistically significant correlation coefficients, i.e.,
#'                     \code{"regular"} (default) for regular text, \code{"bold"}
#'                     for bold text, and \code{"italic"} for italic text. Note
#'                     that the font style \code{"bold"} and \code{"italic"} can
#'                     be combined, i.e., style = c("bold", "italic") provides a
#'                     bold and italic text. Note that the argument \code{color}
#'                     needs to be specified to change the style of the text, e.g.
#'                     \code{color = "black"} and \code{style = "bold"} to for
#'                     bold text.
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
#' \item{\strong{Within-Group and Between-Group Variables}}{The function automatically
#' identifies (1) variables in the data frame specified in \code{data} that are
#' measured at the individual level and modeled only at the within level and (2)
#' variables in the data frame specified in \code{data} that are measured at the
#' cluster level and modeled only at the between level. The former variables have
#' no variance in the between part of the model (i.e., ICC(1) is 0 e.g. due to
#' centering within clusters), while the latter variables do not have any variance
#' within clusters.}
#' \item{\strong{Estimation Method}}{The default setting for the argument \code{estimator}
#' is depending on the setting of the argument \code{color}. If \code{color = "default"}
#' (default), maximum likelihood estimation (\code{estimator = "ML"}) is used,
#' while maximum likelihood with Huber-White robust standard errors (\code{estimator = "MLR"})
#' that are robust against non-normality is used when when specifying a color for
#' the argument \code{color}.}
#' \item{\strong{Missing Data}}{In the presence of missing data, full information
#' maximum likelihood (FIML) method (\code{missing = "fiml"}) is used by default.
#' Note that FIML method cannot deal with within-group variables that have no
#' variance within some clusters. In this cases, the function will switch to
#' listwise deletion. Using FIML method might result in issues with model convergence,
#' which might be resolved by switching to listwise deletion (\code{missing = "listwise"}).}
#' \item{\strong{Optimizer}}{The lavaan package uses a quasi-Newton optimization
#' method (\code{"nlminb"}) by default. If the optimizer does not converge, model
#' estimation switches to the Expectation Maximization (EM) algorithm (\code{"em"})
#' if the argument \code{optim.switch} is specified as \code{TRUE} (default).}
#' \item{\strong{Statistical Significance}}{Statistically significant correlation
#' coefficients can be shown color coded on the console by specifying the argument
#' \code{color}. However, this option is not supported when using R Markdown.}
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
#' #————————————————————————————————————————————————————————————————————————————
#' # Cluster Variable Specification
#'
#' # Example 1a: Specification using the argument '...'
#' multilevel.cor(Demo.twolevel, y1, y2, y3, cluster = "cluster")
#'
#' # Example 1b: Alternative specification with cluster variable 'cluster' in 'data'
#' multilevel.cor(Demo.twolevel[, c("y1", "y2", "y3", "cluster")], cluster = "cluster")
#'
#' # Example 1c: Alternative specification with cluster variable 'cluster' not in 'data'
#' multilevel.cor(Demo.twolevel[, c("y1", "y2", "y3")], cluster = Demo.twolevel$cluster)
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Arguments 'color', 'style', 'split', 'print', and 'p.adj'
#'
#' # Example 2a: Highlight statistically significant result in bright red
#' multilevel.cor(Demo.twolevel, y1, y2, y3, cluster = "cluster", color = "b.red")
#'
#' # Example 2b: Highlight statistically significant result in boldface
#' multilevel.cor(Demo.twolevel, y1, y2, y3, cluster = "cluster", color = "black",
#'                style = "bold")
#'
#' # Example 3: Split output table in within-group and between-group correlation matrix
#' multilevel.cor(Demo.twolevel, y1, y2, y3, cluster = "cluster", split = TRUE,
#'                color = "green", style = "bold")
#'
#' # Example 4a: Print summary of the lavaan specification and all results
#' multilevel.cor(Demo.twolevel, y1, y2, y3, cluster = "cluster", print = "all")
#'
#' # Example 4b: Print summary of the lavaan specification and correlation coefficients
#' multilevel.cor(Demo.twolevel, y1, y2, y3, cluster = "cluster", print = c("summary", "cor"))
#'
#' # Example 5: Significance values with Bonferroni correction
#' multilevel.cor(Demo.twolevel, y1, y2, y3, cluster = "cluster", print = c("cor", "p"),
#'                p.adj = "bonferroni")
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Variables Measured at the Within and Cluster Level
#'
#' # Example 6a: Variables "y1", "y2", and "y2" modeled at both the within and between level
#' #             Variables "w1" and "w2" modeled at the cluster level
#' multilevel.cor(Demo.twolevel, y1, y2, y3, w1, w2, cluster = "cluster")
#'
#' # Example 6b: Print cluster level variables first
#' multilevel.cor(Demo.twolevel, y1, y2, y3, w1, w2, cluster = "cluster", order = TRUE)
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # lavaan Model and Summary of the Estimated Model
#'
#' # Example 7: lavaan model and summary of the multilevel model used to compute
#' #             the within-group and between-group correlation matrix
#' mod <- multilevel.cor(Demo.twolevel, y1, y2, y3, cluster = "cluster", output = FALSE)
#'
#' # lavaan model syntax
#' mod$model
#'
#' # Fitted lavaan object
#' lavaan::summary(mod$model.fit, standardized = TRUE)
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Write Results
#'
#' # Example 8a: Write Results into a text file
#' multilevel.cor(Demo.twolevel, y1, y2, y3, cluster = "cluster",
#'                write = "Multilevel_Correlation.txt")
#'
#' # Example 8b: Write Results into a Excel file
#' multilevel.cor(Demo.twolevel, y1, y2, y3, cluster = "cluster",
#'                write = "Multilevel_Correlation.xlsx")
#' }
multilevel.cor <- function(data, ..., cluster, estimator = c("ML", "MLR"), constr.var = FALSE,
                           optim.method = c("nlminb", "em"), optim.switch = TRUE,
                           print = c("all", "summary", "cor", "se", "stat", "p"), split = FALSE,
                           order = FALSE, tri = c("both", "lower", "upper"), tri.lower = TRUE,
                           missing = c("listwise", "fiml"), alpha = 0.05,
                           color = "default", style = c("regular", "bold", "italic"),
                           p.adj = c("none", "bonferroni", "holm", "hochberg", "hommel", "BH", "BY", "fdr"),
                           digits = 2, p.digits = 3, as.na = NULL, write = NULL,
                           append = TRUE, check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'data' is missing or NULL
  if (isTRUE(missing(data) || is.null(data))) { stop("Please specify a data frame for the argument 'data'", call. = FALSE) }

  # Check input 'cluster'
  if (isTRUE(missing(cluster) || is.null(cluster))) { stop("Please specify a variable name or vector representing the grouping structure for the argument 'cluster'.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Using the Argument '...' ####

  if (isTRUE(!missing(...))) {

    # Extract data
    x <- as.data.frame(data[, .var.names(data = data, ..., cluster = cluster), drop = FALSE])

    # Extract cluster variable and convert tibble into data frame or vector
    cluster <- data[, cluster] |> (\(y) if (isTRUE("tbl" %in% substr(class(y), 1L, 3L))) { unname(unlist(y)) } else { return(y) })()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Without Using the Argument '...' ####

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

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Numeric Variables ####

  x <- .exclude.non.numeric(x, func = "multilevel.cor")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) { x <- .as.na(x, na = as.na) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Between Variables ####

  between <- names(which(vapply(x, function(y) all(tapply(y, cluster, var, na.rm = TRUE) < .Machine$double.eps^0.5), FUN.VALUE = logical(1L)))) |> (\(p) if (isTRUE(length(p) == 0)) { NULL } else { p })()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Within Variables ####

  within <- names(which(suppressWarnings(misty::multilevel.icc(x[, setdiff(colnames(x), between)], cluster = unlist(cluster))) < .Machine$double.eps^0.5))

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  .check.input(logical = c("optim.switch", "split", "order", "tri.lower", "append", "output"),
               s.character = list(estimator = c("ML", "MLR"), optim.method = c("nlminb", "em"), missing = c("listwise", "fiml"), tri = c("both", "lower", "upper"), style = c("regular", "bold", "italic")),
               m.character = list(print = c("all", "summary", "cor", "se", "stat", "p")),
               args = c("color", "alpha", "p.adj", "digits", "p.digits", "write2"),
               package = "lavaan", envir = environment(), input.check = check)

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Within-Group and Between Variables ####

  # Within Only and Within/Between Variables
  if (isTRUE(is.null(between))) { var.with <- colnames(x) } else { var.with <- setdiff(colnames(x), between) }

  # At least one within variables
  if (isTRUE(length(var.with) == 0L)) { stop("Please specify at least one within-group variable.", call. = FALSE) }

  # Between Only and Within/Between variables
  if (isTRUE(is.null(within))) { var.betw <- colnames(x) } else { var.betw <- setdiff(colnames(x), within) }

  # At least one between variables
  if (isTRUE(length(var.betw) == 0L)) { stop("Please specify at least one between-group variable.", call. = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data frame with Cluster Variable ####

  x <- data.frame(x[, unique(c(var.with, var.betw))], .cluster = cluster)

  n.total <- nrow(x)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Missing Data on the Cluster Variable ####

  if (isTRUE(any(is.na(cluster)))) {

    warning(paste0("Data contains missing values on the cluster variable, number of cases removed from the analysis: ", sum(is.na(cluster))), call. = FALSE)

    x <- x[!is.na(cluster), ]

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'print' Argument ####

  if (isTRUE(all(c("all", "summary", "cor", "se", "stat", "p") %in% print))) {

    print <- "cor"

  } else if (isTRUE("all" %in% print)) {

    print <- c("summary", "cor", "se", "stat", "p")

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'estimator' Argument ####

  # Default setting: "MLR" when color highlighting, "ML" otherwise
  estimator <- if (isTRUE(color != "none" || "stat" %in% print || "se" %in% print || "p" %in% print)) { ifelse(all(c("ML", "MLR") %in% estimator), "MLR", estimator) } else { "ML" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'se' Argument  ####

  # Default setting: "robust.huber.white" when color highlighting, "none" otherwise
  se <- if (isTRUE(color != "none" || "stat" %in% print || "se" %in% print || "p" %in% print)) { ifelse(estimator == "MLR", "robust.huber.white", "standard") } else { "none" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'missing' Argument  ####

  #—————————————————————————————————————— #
  ### Missing Data ####

  if (isTRUE(any(is.na(x)))) {

    complete <- FALSE

    if (isTRUE(all(c("listwise", "fiml") %in% missing))) { missing <- "fiml" }

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

  #—————————————————————————————————————— #
  ### Complete Data ####

  } else {

    complete <- TRUE

    missing <- "listwise"

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'optim.method' Argument  ####

  if (isTRUE(all(c("nlminb", "em") %in% optim.method))) {

    if (isTRUE(missing == "listwise")) {

      optim.method <- "nlminb"

    } else if (isTRUE(missing == "fiml")) {

      optim.method <- "em"

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'tri' Argument ####

  tri <- ifelse(all(c("both", "lower", "upper") %in% tri), "lower", tri)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'style' Argument ####

  if (isTRUE(all(c("regular", "bold", "italic") %in% style))) { style <- "regular" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'p.adj' Argument ####

  p.adj <- ifelse(all(c("none", "bonferroni", "holm", "hochberg", "hommel", "BH", "BY", "fdr") %in% p.adj), "none", p.adj)

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model Specification ####

  #—————————————————————————————————————— #
  ### At Least Two Within- and Two Between-Group Variables ####

  if (isTRUE(length(var.with) >= 2L && length(var.betw) >= 2L)) {

    mod <- paste("  # Within model\n",
                 " level: 1\n  ",
                 paste(apply(combn(length(var.with), 2L), 2L, function(y) paste(var.with[y[1L]], var.with[y[2L]], sep = " ~~ " )), collapse = "\n   "),
                 "\n\n  # Between model\n",
                 " level: 2\n  ",
                 paste(apply(combn(length(var.betw), 2L), 2L, function(y) paste(var.betw[y[1L]], var.betw[y[2L]], sep = " ~~ " )), collapse = "\n   "))

  #—————————————————————————————————————— #
  ### At Least Two Within-Group Variables, but Only One Between-Group Variable ####

  } else if (isTRUE(length(var.with) >= 2L && length(var.betw) == 1L)){

    mod <- paste("  # Within model\n",
                 " level: 1\n  ",
                 paste(apply(combn(length(var.with), 2L), 2L, function(y) paste(var.with[y[1L]], var.with[y[2L]], sep = " ~~ " )), collapse = "\n   "),
                 "\n\n  # Between model\n",
                 " level: 2\n  ",
                 paste(var.betw, var.betw, sep = " ~~ "), collapse = "\n   ")

  #—————————————————————————————————————— #
  ### Only One Within-Group Variable, but at Least Two Between-Group Variables ####

  } else if (isTRUE(length(var.with) == 1L && length(var.betw) >= 2L)) {

    mod <- paste("  # Within model\n",
                 " level: 1\n  ",
                 paste(var.with, var.with, sep = " ~~ " ), collapse = "\n   ",
                 "\n\n  # Between model\n",
                 " level: 2\n  ",
                 # Between model
                 paste(apply(combn(length(var.betw), 2L), 2L, function(y) paste(var.betw[y[1L]], var.betw[y[2L]], sep = " ~~ " )), collapse = "\n   "))

  }

  #—————————————————————————————————————— #
  ### Inequality Constraints for the Variances at Level 2 ####

  if (isTRUE(constr.var)) {

    mod <- paste(mod, "\n  ",
                 "\n   # Variances\n  ",
                 paste(sapply(seq_along(var.betw), function(y) paste0(var.betw[y], " ~~ V", y, "*", var.betw[y])), collapse = "\n   "), "\n  ",
                 "\n   # Inequality constraints\n  ",
                 paste(sapply(seq_along(var.betw), function(y) paste0("V", y, " > 0")), collapse = "\n   "), collapse = "\n")

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model Estimation ####

  model.fit <- tryCatch(suppressWarnings(lavaan::sem(mod, data = x, cluster = ".cluster", estimator = estimator, missing = missing,
                                                     optim.method = optim.method, se = se, test = "none", fit.by.level = FALSE,
                                                     check.gradient = FALSE, check.post = FALSE, check.vcov = FALSE)),
                        error = function(y) {

                          if (isTRUE(missing == "fiml")) {

                            stop("There was an estimation problem in lavaan, switching to missing = \"listwise\" might solve the problem.", call. = FALSE)

                          } else if (isTRUE(estimator == "MLR")){

                            stop("There was an estimation problem in lavaan, switching to estimator = \"ML\" might solve the problem.", call. = FALSE)

                          } else {

                            stop("There was an estimation problem in lavaan, correlation matrix could not be computed.", call. = FALSE)

                          }})

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convergence Check ####

  if (isTRUE(!lavaan::lavInspect(model.fit, what = "converged"))) {

    #—————————————————————————————————————— #
    ### Quasi-Newton Optimizer ####

    if (isTRUE(optim.method == "nlminb")) {

      if (isTRUE(optim.switch)) {

        message("Quasi-Newton optimizer did not converge, switched to the EM algorithm.")

        # Model estimation with EM algorithm
        model.fit <- suppressWarnings(lavaan::sem(mod, data = x, cluster = ".cluster", estimator = estimator,
                                                  missing = missing, optim.method = "em", fit.by.level = FALSE,
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

    #—————————————————————————————————————— #
    ### Expectation Maximization (EM) Algorithm ####

    } else if(isTRUE(optim.method == "em")) {

      if (isTRUE(missing == "listwise")) {

        stop("EM algorithm did not converge.", call. = FALSE)

      } else {

        stop("EM algorithm did not converge. Switching to missing = \"listwise\" might solve the estimation problem.", call. = FALSE)

      }

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convergence and Model Identification Checks ####

  if (isTRUE(check)) {

    check.vcov <- check.theta.w <- check.theta.b <- check.cov.lv.w <- check.cov.lv.b <- TRUE

    if (isTRUE(se != "none")) {

      #—————————————————————————————————————— #
      ### Standard Error ####

      if (isTRUE(any(is.na(unlist(lavaan::lavInspect(model.fit, what = "se")))))) { stop("Standard errors could not be computed.", call. = FALSE) }

      #—————————————————————————————————————— #
      ### Variance-Covariance Matrix of the Estimated Parameters ####

      if (isTRUE(min(eigen(lavaan::lavInspect(model.fit, what = "vcov"), symmetric = TRUE, only.values = TRUE)$values) < .Machine$double.eps^(3L/4L))) {

        warning("The variance-covariance matrix of the estimated parameters is not positive definite. This may be a symptom that the model is not identified.", call. = FALSE)

        check.vcov <- FALSE

      }

    }

    #—————————————————————————————————————— #
    ### Negative Variance of Observed Variables ####

    #···················
    #### Within Level ####

    if (isTRUE(any(diag(lavaan::lavInspect(model.fit, what = "theta")$within) < 0L))) {

      warning("Some estimated variances at the Within level are negative.", call. = FALSE)

      check.theta.w <- FALSE

    } else if (isTRUE(any(eigen(lavaan::lavTech(model.fit, what = "theta")$within, symmetric = TRUE, only.values = TRUE)$values < (-1L * .Machine$double.eps^(3/4))))) {

      warning("The model-implied variance-covariance matrix of the residuals at the Within Level is not positive definite indicating an absolute correlations greater one.", call. = FALSE)

      check.theta.w <- FALSE

    }

    #···················
    #### Between Level ####

    if (isTRUE(any(diag(lavaan::lavInspect(model.fit, what = "theta")$.cluster) < 0L))) {

      warning("Some estimated variances at the Between level are negative, specifying contr.var = TRUE will solve the problem.", call. = FALSE)

      check.theta.b <- FALSE

    } else if (isTRUE(any(eigen(lavaan::lavTech(model.fit, what = "theta")$.cluster, symmetric = TRUE, only.values = TRUE)$values < (-1L * .Machine$double.eps^(3/4))))) {

      warning("The model-implied variance-covariance matrix of the residuals at the Between level is not positive definite indicating an absolute correlations greater one.", call. = FALSE)

      check.theta.b <- FALSE

    }

  } else {

    check.vcov <- check.theta.w <- check.theta.b <- check.cov.lv.w <- check.cov.lv.b <- NULL

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Extract Results ####

  # Standardized solution
  stand <- lavaan::lavMatrixRepresentation(lavaan::standardizedSolution(model.fit))

  # Visible binding for global variable
  mat <- level <- NULL

  #—————————————————————————————————————— #
  ### Within-Group Results ####

  # Theta
  with.stand.theta <- subset(stand[ unlist(subset(lavaan::lavMatrixRepresentation(lavaan::parameterestimates(model.fit)), level == 1L, select = "id")), ], mat == "theta")

  # Parameter estimate, z and significance value matrix
  with.p <- with.stat <- with.se <- with.cor <- matrix(NA, ncol = max(stand[, "col"]), nrow = max(stand[, "row"]))

  #···················
  #### Model Estimation with SE ####

  if (isTRUE(se != "none")) {

    for (i in seq_len(nrow(with.stand.theta))) {

      with.cor[with.stand.theta[i, "row"], with.stand.theta[i, "col"]] <- with.stand.theta[i, "est.std"]
      with.se[with.stand.theta[i, "row"], with.stand.theta[i, "col"]] <- with.stand.theta[i, "se"]
      with.stat[with.stand.theta[i, "row"], with.stand.theta[i, "col"]] <- with.stand.theta[i, "z"]
      with.p[with.stand.theta[i, "row"], with.stand.theta[i, "col"]] <- with.stand.theta[i, "pvalue"]

    }

  #···················
  #### Model Estimation without SE ####

  } else {

    for (i in seq_len(nrow(with.stand.theta))) {

      with.cor[with.stand.theta[i, "row"], with.stand.theta[i, "col"]] <- with.stand.theta[i, "est.std"]

    }

  }

  with.cor[lower.tri(with.cor)] <- t(with.cor)[lower.tri(with.cor)]
  with.se[lower.tri(with.se)] <- t(with.se)[lower.tri(with.se)]
  with.stat[lower.tri(with.stat)] <- t(with.stat)[lower.tri(with.stat)]
  with.p[lower.tri(with.p)] <- t(with.p)[lower.tri(with.p)]

  colnames(with.cor) <- colnames(with.se) <- colnames(with.stat) <- colnames(with.p) <- rownames(with.cor) <- rownames(with.se) <- rownames(with.stat) <- rownames(with.p) <- sapply(seq_len(max(stand[, "row"])), function(y) unique(stand[which(y == stand$row), "lhs"]))

  #—————————————————————————————————————— #
  ### Between-Group Results ####

  # Standardized solution
  betw.stand.theta <- subset(stand[ unlist(subset(lavaan::lavMatrixRepresentation(lavaan::parameterestimates(model.fit)), level == 2L, select = "id")), ], mat == "theta")

  # Parameter estimate, z and significance value matrix
  betw.p <- betw.stat <- betw.se <- betw.cor <- matrix(NA, ncol = max(stand[, "col"]), nrow = max(stand[, "row"]))

  #···················
  #### Model Estimation with SE ####

  if (isTRUE(se != "none")) {

    for (i in seq_len(nrow(betw.stand.theta))) {

      betw.cor[betw.stand.theta[i, "row"], betw.stand.theta[i, "col"]] <- betw.stand.theta[i, "est.std"]
      betw.se[betw.stand.theta[i, "row"], betw.stand.theta[i, "col"]] <- betw.stand.theta[i, "se"]
      betw.stat[betw.stand.theta[i, "row"], betw.stand.theta[i, "col"]] <- betw.stand.theta[i, "z"]
      betw.p[betw.stand.theta[i, "row"], betw.stand.theta[i, "col"]] <- betw.stand.theta[i, "pvalue"]

    }

  #···················
  #### Model Estimation without SE ####

  } else {

    for (i in seq_len(nrow(betw.stand.theta))) {

      betw.cor[betw.stand.theta[i, "row"], betw.stand.theta[i, "col"]] <- betw.stand.theta[i, "est.std"]

    }

  }

  betw.cor[lower.tri(betw.cor)] <- t(betw.cor)[lower.tri(betw.cor)]
  betw.se[lower.tri(betw.se)] <- t(betw.se)[lower.tri(betw.se)]
  betw.stat[lower.tri(betw.stat)] <- t(betw.stat)[lower.tri(betw.stat)]
  betw.p[lower.tri(betw.p)] <- t(betw.p)[lower.tri(betw.p)]

  colnames(betw.cor) <- colnames(betw.se) <- colnames(betw.stat) <- colnames(betw.p) <- rownames(betw.cor) <- rownames(betw.se) <- rownames(betw.stat) <- rownames(betw.p) <- sapply(seq_len(max(stand[, "row"])), function(y) unique(stand[which(y == stand$row), "lhs"]))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Combine Within-Group and Between-Group Results ####

  #—————————————————————————————————————— #
  ### Within-Group Correlations in the Lower Triangular ####

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

  #—————————————————————————————————————— #
  ### Within-Group Correlations in the Upper Triangular ####

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

  #—————————————————————————————————————— #
  ### Adjust p-Values for Multiple Comparison ####

  if (isTRUE(p.adj != "none")) {

    wb.p[lower.tri(wb.p)] <- p.adjust(wb.p[lower.tri(wb.p)], method = p.adj)
    wb.p[upper.tri(wb.p)] <- p.adjust(wb.p[upper.tri(wb.p)], method = p.adj)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Split Within-Group and Between-Group Results ####

  #···················
  #### Model Estimation with SE ####

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

  #···················
  #### Model Estimation without SE ####

  } else {

    # Within-group results
    with.cor <- with.cor[which(apply(with.cor, 1L, function(y) !all(is.na(y)))), which(apply(with.cor, 2L, function(y) !all(is.na(y))))]

    # Between-group results
    betw.cor <- betw.cor[which(apply(betw.cor, 1L, function(y) !all(is.na(y)))), which(apply(betw.cor, 2L, function(y) !all(is.na(y))))]

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Adjust p-Values for Multiple Comparison ####

  if (isTRUE(p.adj != "none")) {

    with.p[lower.tri(with.p)] <- p.adjust(with.p[lower.tri(with.p)], method = p.adj)
    with.p[upper.tri(with.p)] <- p.adjust(with.p[upper.tri(with.p)], method = p.adj)

    betw.p[lower.tri(betw.p)] <- p.adjust(betw.p[lower.tri(betw.p)], method = p.adj)
    betw.p[upper.tri(betw.p)] <- p.adjust(betw.p[upper.tri(betw.p)], method = p.adj)

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## lavaan Summary ####

  lavaan.summary <- data.frame(c(paste("lavaan", lavaan::lavInspect(model.fit, what = "version")), "", "Estimator", "Optimization Method", "", "Standard Errors", "Missing Data", "", "",
                                 "Number of Observations", "Number of Clusters"),
                               c("", "",
                                 # Estimator
                                 estimator,
                                 # Optimization method
                                 toupper(lavaan::lavTech(model.fit, what = "options")$optim.method), "",
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
                               c(rep("", times = 8L), "Total", n.total, ""),
                               fix.empty.names = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Object ####

  object <- list(call = match.call(),
                 type = "multilevel.cor",
                 data = x,
                 args = list(within = within, between = between, estimator = estimator, constr.var = constr.var, optim.method = optim.method, se = se, optim.switch = optim.switch, print = print, split = split, order = order, tri = tri, tri.lower = tri.lower ,missing = missing, alpha = alpha, color = color, style = style, p.adj = p.adj, digits = digits, p.digits = p.digits, as.na = as.na, write = write, append = append, check = check, output = output),
                 model = mod,
                 model.fit = model.fit,
                 check = list(vcov = check.vcov, theta.w = check.theta.w, theta.b = check.theta.b),
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
