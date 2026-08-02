#' Correlation Matrix
#'
#' This function computes a correlation matrix based on Pearson product-moment
#' correlation coefficient, Spearman's rank-order correlation coefficient,
#' Kendall's Tau-b correlation coefficient, Kendall-Stuart's Tau-c correlation
#' coefficient, tetrachoric correlation coefficient, or polychoric correlation
#' coefficient and computes significance values (\emph{p}-values) for testing the
#' two-sided hypothesis H0: \eqn{\rho} = 0 for all pairs of variables. Statistically
#' significant correlations can be highlighted by specifying the argument \code{color}.
#'
#' @param data       a data frame with numeric variables, i.e., factors and character
#'                   variables are excluded from \code{data} before conducting the
#'                   analysis.
#' @param ...        an expression indicating the variable names in \code{data},
#'                   e.g., \code{cor.matrix(dat, x1, x2, x3)}. Note that the
#'                   operators \code{+}, \code{-}, \code{~}, \code{:}, \code{::},
#'                   and \code{!} can also be used to select variables, see 'Details'
#'                   in the \code{\link{df.subset}} function.
#' @param method     a character vector indicating which correlation coefficient
#'                   is to be computed, i.e. \code{"pearson"} for Pearson product-
#'                   moment correlation coefficient (default), \code{"spearman"}
#'                   for Spearman's rank-order correlation coefficient,
#'                   \code{"kendall-b"} for Kendall's Tau-b correlation coefficient,
#'                   \code{"kendall-c"} for Kendall-Stuart's Tau-c correlation
#'                   coefficient, \code{"tetra"} for tetrachoric correlation
#'                   coefficient, and \code{"poly"} for polychoric correlation
#'                   coefficient.
#' @param group      either a character string indicating the variable name of
#'                   the grouping variable in \code{data}, or a vector representing
#'                   the grouping variable. Note that the grouping variable is
#'                   limited to two groups.
#' @param continuity logical: if \code{TRUE} (default), continuity correction is
#'                   used for testing Spearman's rank-order correlation coefficient
#'                   and Kendall's Tau-b correlation.
#' @param ml         logical: if \code{FALSE} (default), a two-step approximation
#'                   is used to compute the tetrachoric and polychoric correlation
#'                   coefficient, while the maximum-likelihood (ML) estimate is
#'                   computed if \code{TRUE}. Note that ML estimation is computationally
#'                   expensive, i.e., takes a lot of time.
#' @param exact      logical: if \code{TRUE} (default), an exact p-value is computed
#'                   for Spearman's rank-order correlation coefficient and Kendall's
#'                   Kendall's Tau-b correlation coefficient. Note that the exact
#'                   p-value is not available in the presence of ties.
#' @param print      a character string or character vector indicating which results
#'                   to show on the console, i.e. \code{"all"} for all results,
#'                   \code{"cor"} for correlation coefficients, \code{"n"} for the
#'                   sample sizes, \code{"stat"} for the test statistic, \code{"df"}
#'                   for the degrees of freedom, and \code{"p"} for \emph{p}-values.
#' @param tri        a character string indicating which triangular of the matrix
#'                   to show on the console, i.e., \code{both} for upper and lower
#'                   triangular, \code{lower} (default) for the lower triangular,
#'                   and \code{upper} for the upper triangular.
#' @param alpha      a numeric value between 0 and 1 indicating the significance
#'                   level at which correlation coefficients are highlighted when
#'                   specifying the argument \code{color}.
#' @param color      a character string indicating the text color for highlighting
#'                   statistically significant correlation coefficients, i.e.,
#'                   \code{"default"} (default) for the default text color without
#'                   color coding and various text colors for highlighting like
#'                   \code{"red"}, \code{"b.red"}, \code{"green"}, \code{"b.green"},
#'                   \code{"blue"}, or \code{"b.blue"}, see the help page of the
#'                   \code{\link{chr.color}} function. Note that this option is
#'                   not supported when using R Markdown and when writing the output
#'                   into a text file (\code{.txt}).
#' @param style      a character vector indicating the font style for
#'                   statistically significant correlation coefficients, i.e.,
#'                   \code{"regular"} (default) for regular text, \code{"bold"}
#'                   for bold text, and \code{"italic"} for italic text. Note
#'                   that the font style \code{"bold"} and \code{"italic"} can
#'                   be combined, i.e., style = c("bold", "italic") provides a
#'                   bold and italic text. Note that the argument \code{color}
#'                   needs to be specified to change the style of the text, e.g.
#'                   \code{color = "black"} and \code{style = "bold"} to for
#'                   bold text.
#' @param p.adj      a character string indicating an adjustment method for multiple
#'                   testing based on \code{\link{p.adjust}}, i.e., \code{none} ,
#'                   \code{bonferroni}, \code{holm} (default), \code{hochberg},
#'                   \code{hommel}, \code{BH}, \code{BY}, or \code{fdr}.
#' @param na.omit    logical: if \code{TRUE}, incomplete cases are removed before
#'                   conducting the analysis (i.e., listwise deletion); if
#'                   \code{FALSE} (default), pairwise deletion is used.
#' @param digits     an integer value indicating the number of decimal places to be
#'                   used for displaying correlation coefficients.
#' @param p.digits   an integer value indicating the number of decimal places to be
#'                   used for displaying \emph{p}-values.
#' @param as.na      a numeric vector indicating user-defined missing values,
#'                   i.e. these values are converted to \code{NA} before conducting
#'                   the analysis.
#' @param write      a character string naming a file for writing the output into
#'                   either a text file with file extension \code{".txt"} (e.g.,
#'                   \code{"Output.txt"}) or Excel file with file extension
#'                   \code{".xlsx"}  (e.g., \code{"Output.xlsx"}). If the file
#'                   name does not contain any file extension, an Excel file will
#'                   be written.
#' @param append     logical: if \code{TRUE} (default), output will be appended
#'                   to an existing text file with extension \code{.txt} specified
#'                   in \code{write}, if \code{FALSE} existing text file will be
#'                   overwritten.
#' @param check      logical: if \code{TRUE} (default), argument specification is
#'                   checked.
#' @param output     logical: if \code{TRUE} (default), output is shown on the
#'                   console.

#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{cohens.d}}, \code{\link{effsize}}, \code{\link{multilevel.cor}},
#' \code{\link{multilevel.icc}}, \code{\link{na.auxiliary}}, \code{\link{size.cor}}
#'
#' @references
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology
#' - Using R and SPSS}. John Wiley & Sons.
#'
#' Revelle, W. (2018) \emph{psych: Procedures for personality and psychological
#' research}. Northwestern University, Evanston, Illinois, USA,
#' https://CRAN.R-project.org/package=psych Version = 1.8.12.
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#'
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{data}}{data frame used for the current analysis}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{result}}{list with result tables, i.e., \code{cor} for the
#'                      correlation matrix, \code{n} for a matrix with the sample
#'                      sizes, \code{stat} for a matrix with the test statistics,
#'                      \code{df} for a matrix with the degrees of freedom, and
#'                      \code{p}-value for the matrix with the significance values
#'                      (\emph{p}-values)}
#'
#' @note
#' This function uses a modified copy of the \code{polychor()} function in the
#' \pkg{polycor} package by John Fox when requesting tetrachoric or polychoric
#' correlation coefficients.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #————————————————————————————————————————————————————————————————————————————
#' # Pearson Product-Moment Correlation Coefficient
#'
#' # Example 1a: Pearson product-moment correlation matrix using pairwise deletion
#' cor.matrix(airquality, Ozone:Wind)
#'
#' # Example 1b:  Alternative specification without using the '...' argument
#' cor.matrix(airquality[, c("Ozone", "Solar.R", "Wind")])
#'
#' # Example 2a: Highlight statistically significant result in bright red
#' cor.matrix(airquality, Ozone, Solar.R, Wind, color = "b.red")
#'
#' # Example 2b: Highlight statistically significant result in boldface
#' cor.matrix(airquality, Ozone, Solar.R, Wind, color = "black", style = "bold")
#'
#' # Example 3a: Print sample size, degrees of freedom, and significance values
#' cor.matrix(airquality, Ozone, Solar.R, Wind, print = "all")
#'
#' # Example 3b: Listwise deletion
#' cor.matrix(airquality, Ozone, Solar.R, Wind, na.omit = TRUE)
#'
#' # Example 3c: Significance values with Bonferroni correction
#' cor.matrix(airquality, Ozone, Solar.R, Wind, print = "all", p.adj = "bonferroni")
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Spearman's Rank-Order Correlation Coefficient and Kendall's Tau
#'
#' # Example 4a: Spearman's rank-order correlation matrix
#' cor.matrix(airquality, Ozone, Solar.R, Wind, method = "spearman")
#'
#' # Example 4b: Kendall's Tau-c
#' cor.matrix(airquality, Ozone, Solar.R, Wind, method = "kendall-c")
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Tetrachoric and Polychoric Correlation Coefficient
#'
#' # Example 5a: Tetrachoric correlation matrix
#' cor.matrix(data.items, +ditem, method = "tetra")
#'
#' # Example 5b: Polychoric correlation matrix
#' cor.matrix(data.items, +pitem, method = "poly")
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Grouping Variable
#'
#' # Example 6a: Results for group 'vs' = "0" and "1" separately
#' cor.matrix(mtcars, mpg:disp, group = "vs")
#'
#' # Example 6b: Alternative specification without using the '...' argument
#' cor.matrix(mtcars[, c("mpg", "cyl", "disp")], group = mtcars$vs)
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Write Results
#'
#' # Example 7a: Write Results into a text file
#' cor.matrix(airquality, Ozone, Solar.R, Wind, print = "all", write = "Correlation.txt")
#'
#' # Example 7b: Write Results into an Excel file
#' cor.matrix(airquality, Ozone, Solar.R, Wind, print = "all", write = "Correlation.xlsx")
#' }
cor.matrix <- function(data, ...,
                       method = c("pearson", "spearman", "kendall-b", "kendall-c", "tetra", "poly"),
                       group = NULL, exact = FALSE, continuity = TRUE, ml = FALSE,
                       print = c("all", "cor", "n", "stat", "df", "p"),
                       tri = c("both", "lower", "upper"), alpha = 0.05,
                       color = "default", style = c("regular", "bold", "italic"),
                       p.adj = c("none", "bonferroni", "holm", "hochberg", "hommel", "BH", "BY", "fdr"),
                       na.omit = FALSE, digits = 2, p.digits = 3, as.na = NULL,
                       write = NULL, append = TRUE, check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'data' is missing or NULL
  if (isTRUE(missing(data) || is.null(data))) { stop("Please specify a data frame for the argument 'data'", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Using the Argument '...' ####

  if (isTRUE(!missing(...))) {

    # Extract data and convert tibble into data frame or vector
    x <- data[, .var.names(data = data, ..., group = group), drop = FALSE] |> (\(y) if (isTRUE("tbl" %in% substr(class(y), 1L, 3L))) { if (isTRUE(ncol(as.data.frame(y)) == 1L)) { unname(unlist(y)) } else { as.data.frame(y) } } else { y })()

    # Grouping variable
    if (isTRUE(!is.null(group))) { group <- data[, group] }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Without Using the Argument '...' ####

  } else {

    # Data frame
    x <- as.data.frame(data)

    # Data and cluster
    var.group <- .var.group(data = x, group = group)

    # Data
    if (isTRUE(!is.null(var.group$data)))  { x <- var.group$data }

    # Grouping variable
    if (isTRUE(!is.null(var.group$group))) { group <- var.group$group }

  }

  # Convert 'group' as tibble into a vector
  if (!is.null(group) && isTRUE("tbl" %in% substr(class(group), 1L, 3L))) { group <- unname(unlist(group)) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Non-Numeric Variables ####

  x <- .exclude.non.numeric(x, func = "cor.matrix")

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("ml", "na.omit", "continuity", "append", "output"),
               s.character = list(method = c("pearson", "spearman", "kendall-b", "kendall-c", "tetra", "poly"), tri = c("both", "lower", "upper"), style = c("regular", "bold", "italic")),
               m.character = list(print = c("all", "cor", "n", "stat", "df", "p")),
               args = c("color", "alpha", "p.adj", "digits", "p.digits"), envir = environment(), input.check = check)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Additional Checks ####

  if (isTRUE(check)) {

    #—————————————————————————————————————— #
    ### 'tetra' and 'poly' Argument ####

    if (isTRUE((all(method == "tetra") || all(method == "poly")))) {

      if (isTRUE(any(x %% 1L != 0L))) { stop("Pleas specify a matrix or data frame with integer vectors when computing tetrachoric or polychoric correlation coefficients.", call. = FALSE) }

      if (isTRUE(method == "tetra" && any(apply(x, 2L, function(y) length(na.omit(unique(y))) != 2L)))) { stop("Please specify a data frame with dichotomous data when computing tetrachoric correlation coefficients.", call. = FALSE) }

    }

    #—————————————————————————————————————— #
    ### 'group' Argument ####

    if (isTRUE(!is.null(group))) {

      # Specified two groups only?
      if (isTRUE(length(na.omit(unique(group))) != 2L)) { stop("Please specify a grouping variable with only two groups for the argument 'group'.", call. = FALSE) }

      # Zero variance in one of the groups
      vapply(split(x, f = group), function(y) apply(y, 2L, function(z) length(na.omit(unique(z))) == 1L), FUN.VALUE = logical(ncol(x))) |> (\(y) if (isTRUE(any(y))) { stop(paste("Following variables have zero variance in at least one of the groups specified in 'group': ", paste(names(which(apply(y, 1L, any))), collapse = ", ")), call. = FALSE) })()

    }

    # Check input 'data' for zero variance
    vapply(x, function(y) length(na.omit(unique(y))) == 1L, FUN.VALUE = logical(1L)) |> (\(y) if (isTRUE(any(y))) { warning(paste0("Following variables have zero variance: ", paste(names(which(y)), collapse = ", ")), call. = FALSE) })()


  }

  #_____________________________________________________________________________
  #
  # Data  ----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert User-missing Values into NA ####

  if (isTRUE(!is.null(as.na))) { x <- .as.na(x, na = as.na) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Missing Data ####

  attr(x, "missing") <- any(is.na(x))

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'na.omit' Argument ####

  if (isTRUE(any(!complete.cases(x, group)))) {

    if (isTRUE(na.omit)) {

      # Without grouping variable
      if (isTRUE(is.null(group))) {

        x <- na.omit(x)

      # With grouping variable
      } else {

        complete.cases(x, group) |>
          (\(p) {

            group <<- group[p]
            x <<- x[p, ]

          })()

      }

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'method' Argument ####

  method <- ifelse(all(c("pearson", "spearman", "kendall-b", "kendall-c", "tetra", "poly") %in% method), "pearson", method)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'exact' Argument ####

  if (isTRUE(exact)) {

    if (isTRUE(any(c("spearman", "kendall-b") %in% method))) {

      if (isTRUE(any(sapply(x, function(y) misty::uniq.n(na.omit(y)) < length(na.omit(y)))))) {

        warning("Exact test cannot be computed in the presence of ties, the argument 'exact' switchted to FALSE.", call. = FALSE)

        exact <- FALSE

      }

    } else if (isTRUE(any(c("pearson", "kendall-c", "tetra", "poly") %in% method))) {

      warning(paste0("Exact test is not available for method = ", method, "."), call. = FALSE)

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'print' Argument ####

  # Print argument
  if (isTRUE(all(c("all", "cor", "n", "stat", "df", "p") %in% print))) { print <- "cor" }

  # Method argument
  if (isTRUE(method %in% c("pearson", "spearman"))) {

    if (isTRUE(length(print) == 1L && "all" %in% print)) { print <- c("cor", "n", "stat", "df", "p") }

  } else if (isTRUE(method %in% c("kendall-b", "kendall-c", "tetra", "poly"))) {

    if (isTRUE(length(print) == 1L && "all" %in% print)) { print <- c("cor", "n", "stat", "p") }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'tri' Argument ####

  if (isTRUE(is.null(group))) {

    tri <- ifelse(all(c("both", "lower", "upper") %in% tri), "lower", tri)

  } else {

    tri <- "both"

  }

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
  ## Pairwise Combination of Columns ####

  comb <- combn(seq_len(ncol(x)), m = 2L)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Result Objects ####

  p.mat <- df.mat <- stat.mat <- n.mat <- cor.mat <- matrix(NA, ncol = ncol(x), nrow = ncol(x), dimnames = list(colnames(x), colnames(x)))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## No Grouping ####

  if (isTRUE(is.null(group))) {

    #—————————————————————————————————————— #
    ### Correlation Matrix ####

    # Pearson product-moment correlation coefficient
    switch(method, "pearson" = {

      cor.test.res <- apply(comb, 2L, function(y) suppressWarnings(.cor.test.pearson(x[, y[1L]], x[, y[2L]])))

    # Spearman's rank-order correlation coefficient
    }, "spearman" = {

      cor.test.res <- apply(comb, 2L, function(y) suppressWarnings(.cor.test.spearman(x[, y[1L]], x[, y[2L]], exact = exact, continuity = continuity)))

    # Kendall's Tau-b correlation coefficient
    }, "kendall-b" = {

      cor.test.res <- apply(comb, 2L, function(y) suppressWarnings(.cor.test.kendall.b(x[, y[1L]], x[, y[2L]], exact = exact, continuity = continuity)))

    # Kendall-Stuart's Tau-c correlation coefficient
    }, "kendall-c" = {

      cor.test.res <- apply(comb, 2L, function(y) suppressWarnings(.cor.test.kendall.c(x[, y[1L]], x[, y[2L]])$result))

    # Tetrachoric correlation coefficient
    }, "tetra" = {

      cor.test.res <- apply(comb, 2L, function(y) suppressWarnings(.cor.test.polychoric(x[, y[1L]], x[, y[2L]], ml = ml, se = ifelse(color != "default" || "stat" %in% print || "p" %in% print || ncol(comb) == 1L, TRUE, FALSE))$result))

    # Polychoric correlation coefficient
    }, "poly" = {

      cor.test.res <- apply(comb, 2L, function(y) suppressWarnings(.cor.test.polychoric(x[, y[1L]], x[, y[2L]], ml = ml, se = ifelse(color != "default" || "stat" %in% print || "p" %in% print || ncol(comb) == 1L, TRUE, FALSE))$result))

    })

    #—————————————————————————————————————— #
    ### Correlation Coefficient ####

    cor.mat[lower.tri(cor.mat)] <- sapply(cor.test.res, function(y) y$cor)
    cor.mat[upper.tri(cor.mat)] <- t(cor.mat)[upper.tri(cor.mat)]

    #—————————————————————————————————————— #
    ### Sample Size ####

    n.mat[lower.tri(n.mat)] <- if (isTRUE(any(is.na(x)) && !na.omit)) { apply(comb, 2L, function(y) nrow(na.omit(cbind(x[, y[1L]], x[, y[2L]])))) } else { nrow(x) }
    n.mat[upper.tri(n.mat)] <- t(n.mat)[upper.tri(n.mat)]

    #—————————————————————————————————————— #
    ### Test Statistic ####

    # Test statistic
    stat.mat[lower.tri(stat.mat)] <- sapply(cor.test.res, function(y) y$stat)
    stat.mat[upper.tri(stat.mat)] <- t(stat.mat)[upper.tri(stat.mat)]

    #—————————————————————————————————————— #
    ### Degrees of Freedom ####

    df.mat[lower.tri(df.mat)] <- sapply(cor.test.res, function(y) y$df)
    df.mat[upper.tri(df.mat)] <- t(df.mat)[upper.tri(df.mat)]

    #—————————————————————————————————————— #
    ### p-Values ####

    p.mat[lower.tri(p.mat)] <- if (isTRUE(p.adj == "none")) { sapply(cor.test.res, function(y) y$pval) } else { p.adjust(sapply(cor.test.res, function(y) y$pval), method = p.adj) }
    p.mat[upper.tri(p.mat)] <- t(p.mat)[upper.tri(p.mat)]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Grouping ####

  } else {

    # At least 3 observations
    if (isTRUE(any(table(group) < 3L))) { stop("Not enough observations for each group specified in 'group' to compute the correlation matrix separately.", call. = FALSE) }

    # Grouping
    x.group <- split(x, f = group)

    # Correlation matrix
    object.g1 <- misty::cor.matrix(x.group[[1L]], method = method, group = NULL, exact = exact, continuity = continuity, ml = ml, na.omit = FALSE, p.adj = p.adj, check = FALSE, output = FALSE)
    object.g2 <- misty::cor.matrix(x.group[[2L]], method = method, group = NULL, exact = exact, continuity = continuity, ml = ml, na.omit = FALSE, p.adj = p.adj, check = FALSE, output = FALSE)

    #—————————————————————————————————————— #
    ### Data Frame ####

    x <- data.frame(.group = group, x)

    #—————————————————————————————————————— #
    ### Missing Data ####

    attr(x, "missing") <- any(is.na(x))

    #—————————————————————————————————————— #
    ### Result Objects ####

    cor.mat <- object.g1$result$cor
    n.mat <- object.g1$result$n
    stat.mat <- object.g1$result$stat
    df.mat <- object.g1$result$df
    p.mat <- object.g1$result$p

    #—————————————————————————————————————— #
    ### Lower Triangular: Group 1; Upper Triangular: Group 2 ####

    if (isTRUE(!is.null(cor.mat))) { cor.mat[upper.tri(cor.mat)] <- object.g2$result$cor[upper.tri(object.g2$result$cor)] }
    if (isTRUE(!is.null(n.mat))) { n.mat[upper.tri(n.mat)] <- object.g2$result$n[upper.tri(object.g2$result$n)] }
    if (isTRUE(!is.null(stat.mat))) { stat.mat[upper.tri(stat.mat)] <- object.g2$result$stat[upper.tri(object.g2$result$stat)] }
    if (isTRUE(!is.null(df.mat))) { df.mat[upper.tri(df.mat)] <- object.g2$result$df[upper.tri(object.g2$result$df)] }
    if (isTRUE(!is.null(p.mat))) { p.mat[upper.tri(p.mat)] <- object.g2$result$p[upper.tri(object.g2$result$p)] }

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "cor.matrix",
                 data = x,
                 args = list(method = method, exact = exact, continuity = continuity, ml = ml, print = print, tri = tri, alpha = alpha, color = color, style = style, p.adj = p.adj, na.omit = na.omit, digits = digits, p.digits = p.digits, as.na = as.na, write = write, append = append, check = check, output = output),
                 result = list(cor = cor.mat, n = n.mat, stat = stat.mat, df = df.mat, p = p.mat) |> (\(p) p[sapply(p, function(y) any(!is.na(y)))])())

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
