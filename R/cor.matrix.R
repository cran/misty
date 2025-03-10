#' Correlation Matrix
#'
#' This function computes a correlation matrix based on Pearson product-moment
#' correlation coefficient, Spearman's rank-order correlation coefficient,
#' Kendall's Tau-b correlation coefficient, Kendall-Stuart's Tau-c correlation
#' coefficient, tetrachoric correlation coefficient, or polychoric correlation
#' coefficient and computes significance values (\emph{p}-values) for testing the
#' hypothesis H0: \eqn{\rho} = 0 for all pairs of variables.
#'
#' Note that unlike the \code{\link[stats:cor.test]{cor.test}} function, this
#' function does not compute an exact \emph{p}-value for Spearman's rank-order
#' correlation coefficient or Kendall's Tau-b correlation coefficient, but uses
#' the asymptotic \emph{t} approximation.
#'
#' Statistically significant correlation coefficients can be shown in boldface
#' on the console when specifying \code{sig = TRUE}. However, this option is not
#' supported when using R Markdown, i.e., the argument \code{sig} will switch to
#' \code{FALSE}.
#'
#' @param data       a data frame with numeric variables, i.e., factors and character
#'                   variables are excluded from \code{data} before conducting the
#'                   analysis.
#' @param ...        an expression indicating the variable names in \code{data},
#'                   e.g., \code{cor.matrix(dat, x1, x2, x3)}. Note that the
#'                   operators \code{.}, \code{+}, \code{-}, \code{~}, \code{:},
#'                   \code{::}, and \code{!} can also be used to select variables,
#'                   see 'Details' in the \code{\link{df.subset}} function.
#' @param method     a character vector indicating which correlation coefficient
#'                   is to be computed, i.e. \code{"pearson"} for Pearson product-
#'                   moment correlation coefficient (default), \code{"spearman"}
#'                   for Spearman's rank-order correlation coefficient,
#'                   \code{"kendall-b"} for Kendall's Tau-b correlation coefficient,
#'                   \code{"kendall-c"} for Kendall-Stuart's Tau-c correlation
#'                   coefficient, \code{"tetra"} for tetrachoric correlation
#'                   coefficient, and \code{"poly"} for polychoric correlation
#'                   coefficient.
#' @param na.omit    logical: if \code{TRUE}, incomplete cases are removed before
#'                   conducting the analysis (i.e., listwise deletion); if
#'                   \code{FALSE} (default), pairwise deletion is used.
#' @param group      either a character string indicating the variable name of
#'                   the grouping variable in \code{data}, or a vector representing
#'                   the grouping variable. Note that the grouping variable is
#'                   limited to two groups.
#' @param sig        logical: if \code{TRUE}, statistically significant correlation
#'                   coefficients are shown in boldface on the console. Note that
#'                   this function does not provide statistical significance
#'                   testing for tetrachoric or polychoric correlation coefficients.
#' @param alpha      a numeric value between 0 and 1 indicating the significance
#'                   level at which correlation coefficients are printed boldface
#'                   when \code{sig = TRUE}.
#' @param print      a character string or character vector indicating which results
#'                   to show on the console, i.e. \code{"all"} for all results,
#'                   \code{"cor"} for correlation coefficients, \code{"n"} for the
#'                   sample sizes, \code{"stat"} for the test statistic, \code{"df"}
#'                   for the degrees of freedom, and \code{"p"} for \emph{p}-values.
#'                   Note that the function does not provide \emph{p}-values for
#'                   tetrachoric or polychoric correlation coefficients.
#' @param tri        a character string indicating which triangular of the matrix
#'                   to show on the console, i.e., \code{both} for upper and lower
#'                   triangular, \code{lower} (default) for the lower triangular,
#'                   and \code{upper} for the upper triangular.
#' @param p.adj      a character string indicating an adjustment method for multiple
#'                   testing based on \code{\link{p.adjust}}, i.e., \code{none} ,
#'                   \code{bonferroni}, \code{holm} (default), \code{hochberg},
#'                   \code{hommel}, \code{BH}, \code{BY}, or \code{fdr}.
#' @param continuity logical: if \code{TRUE} (default), continuity correction is
#'                   used for testing Spearman's rank-order correlation coefficient
#'                   and Kendall's Tau-b correlation.
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
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{cohens.d}}, \code{\link{effsize}}, \code{\link{multilevel.icc}},
#' \code{\link{na.auxiliary}}, \code{\link{size.cor}}, \code{\link{write.result}}
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
#' This function uses the \code{polychoric()} function in the \pkg{psych}
#' package by William Revelle to estimate tetrachoric and polychoric correlation
#' coefficients.
#'
#' @export
#'
#' @examples
#' # Example 1: Pearson product-moment correlation coefficient between 'Ozone' and 'Solar.R
#' cor.matrix(airquality, Ozone, Solar.R)
#'
#' # Alternative specification without using the '...' argument
#' cor.matrix(airquality[, c("Ozone", "Solar.R")])
#'
#' # Example 2: Pearson product-moment correlation matrix using pairwise deletion
#' cor.matrix(airquality, Ozone:Wind)
#'
#' # Alternative specification without using the '...' argument
#' cor.matrix(airquality[, c("Ozone", "Solar.R", "Wind")])
#'
#' # Example 3: Spearman's rank-order correlation matrix
#' cor.matrix(airquality, Ozone, Solar.R, Wind, method = "spearman")
#'
#' # Example 4: Pearson product-moment correlation matrix
#' # highlight statistically significant result at alpha = 0.05
#' cor.matrix(airquality, Ozone, Solar.R, Wind, sig = TRUE)
#'
#' # Example 5: Pearson product-moment correlation matrix
#' # highlight statistically significant result at alpha = 0.05
#' cor.matrix(airquality, Ozone, Solar.R, Wind, sig = TRUE, alpha = 0.10)
#'
#' # Example 6: Pearson product-moment correlation matrix
#' # print sample size and significance values
#' cor.matrix(airquality, Ozone, Solar.R, Wind, print = "all")
#'
#' # Example 7: Pearson product-moment correlation matrix using listwise deletion,
#' # print sample size and significance values
#' cor.matrix(airquality, Ozone, Solar.R, Wind, na.omit = TRUE, print = "all")
#'
#' # Example 8: Pearson product-moment correlation matrix
#' # print sample size and significance values with Bonferroni correction
#' cor.matrix(airquality, Ozone, Solar.R, Wind, na.omit = TRUE, print = "all",
#'            p.adj = "bonferroni")
#'
#' # Example 9: Pearson product-moment correlation matrix for 'mpg', 'cyl', and 'disp'
#' # results for group "0" and "1" separately
#' cor.matrix(mtcars, mpg:disp, group = "vs")
#'
#' # Alternative specification without using the '...' argument
#' cor.matrix(mtcars[, c("mpg", "cyl", "disp")], group = mtcars$vs)
#'
#' \dontrun{
#' # Example 10a: Write Results into a text file
#' cor.matrix(airquality, Ozone, Solar.R, Wind, print = "all", write = "Correlation.txt")
#'
#' # Example 10b: Write Results into a Excel file
#' cor.matrix(airquality, Ozone, Solar.R, Wind, print = "all", write = "Correlation.xlsx")
#' }
cor.matrix <- function(data, ..., method = c("pearson", "spearman", "kendall-b", "kendall-c", "tetra", "poly"),
                       na.omit = FALSE, group = NULL, sig = FALSE, alpha = 0.05,
                       print = c("all", "cor", "n", "stat", "df", "p"),
                       tri = c("both", "lower", "upper"),
                       p.adj = c("none", "bonferroni", "holm", "hochberg", "hommel", "BH", "BY", "fdr"),
                       continuity = TRUE, digits = 2, p.digits = 3, as.na = NULL,
                       write = NULL, append = TRUE, check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'data' is missing
  if (isTRUE(missing(data))) { stop("Please specify a data frame for the argument 'data'", call. = FALSE) }

  # Check if input 'data' is NULL
  if (isTRUE(is.null(data))) { stop("Input specified for the argument 'data' is NULL.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data using the argument 'data' ####

  if (isTRUE(!missing(...))) {

    # Extract data and convert tibble into data frame or vector
    x <- data[, .var.names(..., data = data, group = group), drop = FALSE] |> (\(y) if (isTRUE("tbl" %in% substr(class(y), 1L, 3L))) { if (isTRUE(ncol(as.data.frame(y)) == 1L)) { unname(unlist(y)) } else { as.data.frame(y) } } else { y })()

    # Grouping variable
    if (isTRUE(!is.null(group))) { group <- data[, group] }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data without using the argument 'data' ####

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

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Numeric Variables ####

  x <- x |>
    (\(y) !vapply(y, is.numeric, FUN.VALUE = logical(1L)))() |>
    (\(z) if (isTRUE(any(z))) {

      warning(paste0("Non-numeric variables were excluded from the analysis: ", paste(names(which(z)), collapse = ", ")), call. = FALSE)

      return(x[, -which(z), drop = FALSE])

    } else {

      return(x)

    })()

  if (isTRUE(ncol(x) == 0L)) { stop("No variables left for analysis after excluding non-numeric variables.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical =  c("na.omit", "sig", "continuity", "append", "output"),
               s.character = list(method = c("pearson", "spearman", "kendall-b", "kendall-c", "tetra", "poly"), tri = c("both", "lower", "upper")),
               m.character = list(print = c("all", "cor", "n", "stat", "df", "p")),
               args = c("alpha", "p.adj", "digits", "p.digits"), envir = environment(), input.check = check)

  # Additional checks
  if (isTRUE(check)) {

    # Check input 'data'
    if (isTRUE(any(vapply(x, function(y) !is.numeric(y), FUN.VALUE = logical(1L))))) { stop("Please specify a data frame with numeric vectors.", call. = FALSE) }

    # Tetrachoric or polychoric corelation coefficient
    if (isTRUE((all(method == "tetra") || all(method == "poly")))) {

      if (isTRUE(any(x %% 1L != 0L))) { stop("Pleas specify a matrix or data frame with integer vectors when computing tetrachoric or polychoric correlation coefficients.", call. = FALSE) }

      if (isTRUE(method == "tetra" && any(apply(x, 2L, function(y) length(na.omit(unique(y))) != 2L)))) { stop("Please specify a data frame with dichotomous data when computing tetrachoric correlation coefficients.", call. = FALSE) }

    }

    # Check input 'group'
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
  # Data and Variables ---------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) { x <- .as.na(x, na = as.na) }

  # Missing data
  attr(x, "missing") <- any(is.na(x))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Listwise deletion ####

  if (isTRUE(na.omit)) {

    # Without grouping variable
    if (isTRUE(is.null(group))) {

      x <- na.omit(x)

    # With grouping variable
    } else {

      x.group <- na.omit(data.frame(x, group))

      x <- x.group[, colnames(x)]
      group <- x.group[, "group"]

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Correlation coefficient ####

  method <- ifelse(all(c("pearson", "spearman", "kendall-b", "kendall-c", "tetra", "poly") %in% method), "pearson", method)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Print correlation, sample size or significance values ####

  # Print argument
  if (isTRUE(all(c("all", "cor", "n", "stat", "df", "p") %in% print))) { print <- "cor" }

  # Method argument
  if (isTRUE(method %in% c("pearson", "spearman"))) {

    if (isTRUE(length(print) == 1L && "all" %in% print)) { print <- c("cor", "n", "stat", "df", "p") }

  } else if (isTRUE(method %in% c("kendall-b", "kendall-c"))) {

    if (isTRUE(length(print) == 1L && "all" %in% print)) { print <- c("cor", "n", "stat", "p") }

  } else if (isTRUE(method %in% c("tetra", "poly"))) {

    if (isTRUE(length(print) == 1L && "all" %in% print)) { print <- c("cor", "n") }

  }

  # Check input 'print'
  if (isTRUE(print == "df" & method %in% c("kendall-b", "kendall-c"))) {

    switch(method, "kendall-b" = {

      stop("There is no degrees of freedom (df) for testing the Kendall's Tau-b correlation coefficient.", call. = FALSE)

    }, "kendall-c" = {

      stop("There is no degrees of freedom (df) for testing the Kendall-Stuart's Tau-c correlation coefficient.", call. = FALSE)

    })

  }

  if (isTRUE(any(print %in% c("stat", "df", "p")) && method %in% c("tetra", "poly"))) {

    switch(method, "tetra" = {

      stop("There are no test statistics, degrees of freedom, or p-values for the tetrachoric correlation coefficient.", call. = FALSE)

    }, "poly" = {

      stop("There are no test statistics, degrees of freedom, or p-values for the polychoric correlation coefficient.", call. = FALSE)

    })

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Print triangular ####

  if (isTRUE(is.null(group))) {

    tri <- ifelse(all(c("both", "lower", "upper") %in% tri), "lower", tri)

  } else {

    tri <- "both"

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Adjustment method for multiple testing ####

  p.adj <- ifelse(all(c("none", "bonferroni", "holm", "hochberg", "hommel", "BH", "BY", "fdr") %in% p.adj), "none", p.adj)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Pairwise combination of columns ####

  comb <- combn(seq_len(ncol(x)), m = 2L)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Correlation, sample size, test statistic, df, and p-value matrix ####

  p.mat <- df.mat <- stat.mat <- n.mat <- cor.mat <- matrix(NA, ncol = ncol(x), nrow = ncol(x), dimnames = list(colnames(x), colnames(x)))

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## No grouping ####

  if (isTRUE(is.null(group))) {

    #...................
    ### Correlation matrix ####

    # Product-moment or Spearman correlation coefficient
    switch(method, "pearson" = {

      cor.mat <- suppressWarnings(cor(x, use = "pairwise.complete.obs", method = "pearson"))
      cor.test.res <- apply(comb, 2L, function(y) suppressWarnings(.internal.cor.test.pearson(x[, y[1L]], x[, y[2L]])))

    }, "spearman" = {

      cor.mat <- suppressWarnings(cor(x, use = "pairwise.complete.obs", method = "spearman"))
      cor.test.res <- apply(comb, 2L, function(y) suppressWarnings(.internal.cor.test.spearman(x[, y[1L]], x[, y[2L]], continuity = continuity)))

    }, "kendall-b" = {

      cor.mat <- suppressWarnings(cor(x, use = "pairwise.complete.obs", method = "kendall"))
      cor.test.res <- apply(comb, 2L, function(y) suppressWarnings(.internal.cor.test.kendall.b(x[, y[1L]], x[, y[2L]], continuity = continuity)))

    }, "kendall-c" = {

      cor.test.res <- apply(comb, 2L, function(y) suppressWarnings(.internal.tau.c(x[, y[1L]], x[, y[2L]])$result))

      cor.mat[lower.tri(cor.mat)] <- sapply(cor.test.res, function(y) y$tau.c)
      cor.mat[upper.tri(cor.mat)] <- t(cor.mat)[upper.tri(cor.mat)]

      diag(cor.mat) <- 1L

    }, "tetra" = {

      cor.mat <- .internal.polychoric(as.matrix(x))
      colnames(cor.mat) <- rownames(cor.mat) <- colnames(x)

    }, "poly" = {

      cor.mat <- .internal.polychoric(as.matrix(x))
      colnames(cor.mat) <- rownames(cor.mat) <- colnames(x)

    })

    #...................
    ### Sample size ####

    if (!isTRUE(na.omit)) {

      n <- apply(comb, 2L, function(y) nrow(na.omit(cbind(x[, y[1L]], x[, y[2L]]))))

    } else {

      n <- nrow(na.omit(x))

    }

    n.mat[lower.tri(n.mat)] <- n
    n.mat[upper.tri(n.mat)] <- t(n.mat)[upper.tri(n.mat)]

    #...................
    ### Test statistic, df and p-values ####

    if (isTRUE(!method %in% c("tetra", "poly"))) {

      # Test statistic
      stat <- sapply(cor.test.res, function(y) y$stat)

      # Degrees of freedom
      df <- sapply(cor.test.res, function(y) y$df)

      # p-values
      pval <- sapply(cor.test.res, function(y) y$pval)

      # Adjust p-values for multiple comparison
      if (isTRUE(p.adj != "none")) { pval <- p.adjust(pval, method = p.adj) }

      stat.mat[lower.tri(stat.mat)] <- stat
      stat.mat[upper.tri(stat.mat)] <- t(stat.mat)[upper.tri(stat.mat)]

      df.mat[lower.tri(df.mat)] <- df
      df.mat[upper.tri(df.mat)] <- t(df.mat)[upper.tri(df.mat)]

      p.mat[lower.tri(p.mat)] <- pval
      p.mat[upper.tri(p.mat)] <- t(p.mat)[upper.tri(p.mat)]

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Grouping ####

  } else {

    # At least 3 observations
    if (isTRUE(any(table(group) < 3L))) { stop("There are not enough observations for each group specified in 'group' to compute the correlation matrix separately.", call. = FALSE) }

    # Grouping
    x.group <- split(x, f = group)

    object.g1 <- misty::cor.matrix(x.group[[1L]], method = method, na.omit = FALSE, group = NULL,
                                   digits = digits, continuity = continuity, print = print, tri = tri,
                                   p.adj = p.adj, p.digits = p.digits, check = FALSE, output = FALSE)

    object.g2 <- misty::cor.matrix(x.group[[2L]], method = method, na.omit = FALSE, group = NULL,
                                   digits = digits, continuity = continuity, print = print, tri = tri,
                                   p.adj = p.adj, p.digits = p.digits, check = FALSE, output = FALSE)

    #...................
    ### Data frame, correlation matrix, sample size, and p-values ####

    x <- data.frame(.group = group, x)

    #...................
    ### Missing data ####

    attr(x, "missing") <- any(is.na(x))

    cor.mat <- object.g1$result$cor
    n.mat <- object.g1$result$n
    stat.mat <- object.g1$result$stat
    df.mat <- object.g1$result$df
    p.mat <- object.g1$result$p

    #...................
    ### Lower triangular: Group 1; Upper triangular: Group 2 ####

    cor.mat[upper.tri(cor.mat)] <- object.g2$result$cor[upper.tri(object.g2$result$cor)]
    n.mat[upper.tri(n.mat)] <- object.g2$result$n[upper.tri(object.g2$result$n)]

    if (isTRUE(!method %in% c("tetra", "poly"))) {

      stat.mat[upper.tri(stat.mat)] <- object.g2$result$stat[upper.tri(object.g2$result$stat)]
      df.mat[upper.tri(df.mat)] <- object.g2$result$df[upper.tri(object.g2$result$df)]
      p.mat[upper.tri(p.mat)] <- object.g2$result$p[upper.tri(object.g2$result$p)]

    }

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "cor.matrix",
                 data = x,
                 args = list(method = method, na.omit = na.omit, sig = sig, alpha = alpha, print = print, tri = tri, p.adj = p.adj, continuity = continuity, digits = digits, p.digits = p.digits, as.na = as.na, write = write, append = append, check = check,voutput = output),
                 result = if (isTRUE(!method %in% c("tetra", "poly"))) {

                             list(cor = cor.mat, n = n.mat, stat = stat.mat, df = df.mat,p = p.mat)

                           } else {

                             list(cor = cor.mat, n = n.mat)

                           })

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
