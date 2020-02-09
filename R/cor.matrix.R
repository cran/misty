#' Correlation Matrix with Statistical Significance Testing
#'
#' This function computes a correlation matrix and computes significance values (\emph{p}-values) for testing the hypothesis
#' H0: \eqn{\rho} = 0 for all possible pairs of variables.
#'
#' @param x            a matrix or data frame.
#' @param method       a character vector indicating which correlation coefficient is to be computed, i.e.
#'                     \code{"pearson"} for Pearson product-moment correlation coefficient (default),
#'                     \code{"spearman"} for Spearman's rank-order correlation coefficient, or \code{kendall-b} for
#'                     Kendall's Tau-b correlation coefficient.
#' @param use          a character vector giving a method for computing a correlation matrix in the presence
#'                     of missing values, i.e., \code{"listwise"} for listwise deletion and \code{"pairwise"} for
#'                     pairwise deletion
#' @param group        a numeric vector, factor or character vector as grouping variable to show results for
#'                     each group separately, i.e., upper triangular for one group and lower triangular for
#'                     another group. Note that the grouping variable is limited to two groups.
#' @param print        a character string or character vector indicating which additional results to show,
#'                     i.e. \code{"all"},for all additional results: \code{"n"} for the sample sizes,
#'                     \code{"p"}, and for \emph{p}-values.
#' @param tri          a character string indicating which triangular of the matrix to show on the console,
#'                     i.e., \code{both} for upper and lower triangular, \code{lower} (default) for the
#'                     lower triangular, and \code{upper} for the upper triangular.
#' @param p.adj        a character string indicating an adjustment method for multiple testing based on \code{\link{p.adjust}},
#'                     i.e.,  \code{none} (default), \code{bonferroni}, \code{holm}, \code{hochberg}, \code{hommel},
#'                     \code{BH}, \code{BY}, or \code{fdr}.
#' @param digits       an integer value indicating the number of decimal places to be used for displaying
#'                     correlation coefficients.
#' @param pval.digits  an integer indicating the number of decimal places to be used for displaying \emph{p}-values.
#' @param as.na        a numeric vector indicating user-defined missing values,
#'                     i.e. these values are converted to \code{NA} before conducting the analysis.
#' @param check        logical: if \code{TRUE}, argument specification is checked.
#' @param output       logical: if \code{TRUE}, output is shown on the console.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{alpha.coef}}, \code{\link{cohens.d}}, \code{\link{cont.coef}}, \code{\link{cramers.v}},
#' \code{\link{multilevel.icc}}, \code{\link{phi.coef}}, \code{\link{na.auxiliary}}, \code{\link{size.cor}}.
#'
#' @references
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology - Using R and SPSS}.
#' John Wiley & Sons.
#'
#' @return
#' Returns an object of class \code{cor.matrix}, which is a list with following entries: function call (\code{call}),
#' matrix or data frame specified in \code{x} (\code{data}), specification of function arguments (\code{args}), and
#' list with results (\code{result}).
#'
#' @export
#'
#' @examples
#' dat <- data.frame(group = c("a", "a", "a", "a", "a",
#'                             "b", "b", "b", "b", "b"),
#'                   x = c(5, NA, 6, 4, 6, 7, 9, 5, 8, 7),
#'                   y = c(3, 3, 5, 6, 7, 4, 7, NA, NA, 8),
#'                   z = c(1, 3, 1, NA, 2, 4, 6, 5, 9, 6))
#'
#' # Pearson product-moment correlation coefficient matrix using pairwise deletion
#' cor.matrix(dat[, c("x", "y", "z")])
#'
#' # Spearman's rank-order correlation matrix using pairwise deletion
#' cor.matrix(dat[, c("x", "y", "z")], method = "spearman")
#'
#' # Kendall's Tau-b correlation matrix using pairwise deletion
#' cor.matrix(dat[, c("x", "y", "z")], method = "kendall-b")
#'
#' # Pearson product-moment correlation coefficient matrix using pairwise deletion,
#' # print sample size and significance values
#' cor.matrix(dat[, c("x", "y", "z")], print = "all")
#'
#' # Pearson product-moment correlation coefficient matrix using listwise deletion,
#' # print sample size and significance values
#' cor.matrix(dat[, c("x", "y", "z")], use = "listwise", print = "all")
#'
#' # Pearson product-moment correlation coefficient matrix using listwise deletion,
#' # print sample size and significance values with Bonferroni correction
#' cor.matrix(dat[, c("x", "y", "z")], use = "listwise", print = "all", p.adj = "bonferroni")
#'
#' # Pearson product-moment correlation coefficient matrix using pairwise deletion,
#' # results for group "a" and "b" separately
#' cor.matrix(dat[, c("x", "y", "z")], group = dat$group, print = "all")
cor.matrix <- function(x, method = c("pearson", "spearman", "kendall-b"), use = c("listwise", "pairwise"),
                       group = NULL, print = c("all", "cor", "n", "p"), tri = c("both", "lower", "upper"),
                       p.adj = c("none", "bonferroni", "holm", "hochberg", "hommel", "BH", "BY", "fdr"),
                       digits = 2, pval.digits = 3, as.na = NULL, check = TRUE, output = TRUE) {

  ####################################################################################
  # Data

  #-----------------------------------------
  # Check if input 'x' is missing
  if (missing(x)) {

    stop("Please specify a matrix or data frame for the argument 'x'", call. = FALSE)

  }

  #......
  # Matrix or data frame for the argument 'x'?
  if (!is.matrix(x) && !is.data.frame(x)) {

    stop("Please specifiy a matrix or data frame for the argument 'x'.",
         call. = FALSE)

  }

  #-----------------------------------------
  # As data frame
  x.df <- as.data.frame(x)

  ####################################################################################
  # Input Check

  #......
  # Check input 'check'
  if (isFALSE(isTRUE(check) | isFALSE(check))) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }

  #.........................................

  if (isTRUE(check)) {

    #......
    # Check input 'x'
    if (any(sapply(x, function(y) !is.numeric(y)))) {

      stop("Please specify a matrix or data frame with numeric vectors.", call. = FALSE)

    }

    #......
    # Check input 'method'
    if (any(!method %in% c("pearson", "spearman", "kendall-b"))) {

      warning("Character string in the argument 'method' does not match with \"pearson\", \"spearman\", or \"kendall-b\".",
              call. = FALSE)

    }

    #......
    # Check input 'use'
    if (any(!use %in% c("listwise", "pairwise"))) {

      warning("Character string in the argument 'use' does not match with \"listwise\" or \"pairwise\".",
              call. = FALSE)

    }

    #......
    # Check input 'group'
    if (!is.null(group)) {

      # Specied two groups only?
      if (length(na.omit(unique(group))) != 2) {

        stop("Please specify a grouping variable with only two groups for the argument 'group'.", call. = FALSE)

      }

      # Variance = 0 in one of the groups
      if (any(sapply(split(x, f = group), function(y) apply(y, 2, var, na.rm = TRUE)) == 0)) {

        stop("There is at least one variable with 0 variance in one of the groups specified in the argument 'group'.", call. = FALSE)

      }

    }

    #......
    # Check input 'print'
    if (any(!print %in% c("all", "cor", "n", "p"))) {

      warning("Character string(s) in the argument 'print' does not match with \"all\", \"cor\", \"n\", or \"p\".",
              call. = FALSE)

    }

    #......
    # Check input 'tri'
    if (any(!tri %in% c("both", "lower", "upper"))) {

      warning("Character string in the argument 'tri' does not match with \"both\", \"lower\", or \"upper\".",
              call. = FALSE)

    }

    #......
    # Check input 'p.adj'
    if (any(!p.adj %in% c("none", "holm", "bonferroni", "hochberg", "hommel", "BH", "BY", "fdr"))) {

      warning("Character string in the argument 'p.adj' does not match with \"none\", \"bonferroni\", \"holm\", \"hochberg\", \"hommel\", \"BH\", \"BY\", or \"fdr\".",
              call. = FALSE)

    }

    #......
    # Check input 'digits'
    if (digits %% 1 != 0 | digits < 0) {

      warning("Specify a positive integer number for the argument 'digits'.", call. = FALSE)

    }

    #......
    # Check input 'pval.digits'
    if (pval.digits %% 1 != 0 | digits < 0) {

      warning("Specify a positive integer number for the argument 'pval.digits'.", call. = FALSE)

    }

    #......
    # Check input 'output'
    if (isFALSE(isTRUE(output) | isFALSE(output))) {

      stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE)

    }

  }

  ####################################################################################
  # Data and Arguments

  #-----------------------------------------
  # Convert user-missing values into NA
  if (!is.null(as.na)) {

    x.df <- misty::as.na(x.df, na = as.na, check = check)

    # Variable with missing values only
    x.df.miss <- sapply(x.df, function(y) all(is.na(y)))
    if (any(x.df.miss)) {

      stop(paste0("After converting user-missing values into NA, following variables are completely missing: ",
                  paste(names(which(x.df.miss)), collapse = ", ")), call. = FALSE)

    }

    # Constant variables
    x.df.con <- sapply(x.df, function(y) var(as.numeric(y), na.rm = TRUE) == 0)
    if (any(x.df.con)) {

      stop(paste0("After converting user-missing values into NA, following variables are constant: ",
                  paste(names(which(x.df.con)), collapse = ", ")), call. = FALSE)

    }

  }

  #----------------------------------------
  # Correlation coefficient
  method <- ifelse(all(c("pearson", "kendall-b", "spearman") %in% method), "pearson", method)

  #----------------------------------------
  # Method for handling missing data
  use <- ifelse(all(c("listwise", "pairwise") %in% use) | all(use == "pairwise"), "pairwise.complete.obs", "complete.obs")

  #-----------------------------------------
  # Print correlation, sample size or significance values
  if (all(c("all", "cor", "n", "p") %in% print)) { print <- "cor" }

  if (length(print) == 1 && "all" %in% print) { print <- c("cor", "n", "p") }

  #-----------------------------------------
  # Print triangular
  tri <- ifelse(all(c("both", "lower", "upper") %in% tri), "lower", tri)

  #-----------------------------------------
  # Adjustment method for multiple testing
  p.adj <- ifelse(all(c("none", "bonferroni", "holm", "hochberg", "hommel", "BH", "BY", "fdr") %in% p.adj), "none", p.adj)

  #-----------------------------------------
  # Pairwise combination of columns
  comb <- combn(1:ncol(x), m = 2)

  #-----------------------------------------
  # Sample size matrix
  n.mat <- matrix(NA, ncol = ncol(x), nrow = ncol(x), dimnames = list(colnames(x), colnames(x)))

  #-----------------------------------------
  # p-values matrix
  p.mat <- n.mat

  ####################################################################################
  # Main Function

  #-----------------------------------------
  # No grouping
  if (is.null(group)) {

    #........................................
    # Correlation matrix

    if (method == "kendall-b") {

      cor.mat <- cor(x.df, use = use, method = "kendall")

    } else {

      cor.mat <- cor(x.df, use = use, method = method)

    }

    #........................................
    # Sample size

    if (use == "pairwise.complete.obs") {

      n <- apply(comb, 2, function(y) nrow(na.omit(cbind(x.df[, y[1]], x.df[, y[2]]))))

    } else {

      n <- nrow(na.omit(x.df))

    }

    n.mat[lower.tri(n.mat)] <- n
    n.mat[upper.tri(n.mat)] <- t(n.mat)[upper.tri(n.mat)]

    #........................................
    # p-values

    # Listwise deletion
    if (use == "complete.obs") {

        x.df <- na.omit(x.df)

    }

    if (method == "kendall-b") {

      pval <- apply(comb, 2, function(y) suppressWarnings(cor.test(x.df[, y[1]], x.df[, y[2]], method = "kendall"))$p.value)

    } else {

      pval <- apply(comb, 2, function(y) suppressWarnings(cor.test(x.df[, y[1]], x.df[, y[2]], method = method))$p.value)

    }

    # Adjust p-values for multiple comparison
    if (p.adj != "none") {

      pval <- p.adjust(pval, method = p.adj)

    }

    p.mat[lower.tri(p.mat)] <- pval
    p.mat[upper.tri(p.mat)] <- t(p.mat)[upper.tri(p.mat)]

  #-----------------------------------------
  # Grouping
  } else {

    # Grouping
    x.group <- split(x, f = group)

    # Method for handling missing data
    use <- ifelse(use == "pairwise.complete.obs", "pairwise", "listwise")

    object.g1 <- misty::cor.matrix(x.group[[1]], method = method, use = use, group = NULL,
                                   digits = digits, print = print, tri = tri, p.adj = p.adj, pval.digits = pval.digits,
                                   check = FALSE, output = FALSE)

    object.g2 <- misty::cor.matrix(x.group[[2]], method = method, use = use, group = NULL,
                                   digits = digits, print = print, tri = tri, p.adj = p.adj, pval.digits = pval.digits,
                                   check = FALSE, output = FALSE)

  }

  ####################################################################################
  # Return object

  if (is.null(group)) {

    object <- list(call = match.call(),
                   data = x,
                   args = list(method = method, use = use, group = group, print = print,
                               tri = tri, p.adj = p.adj, digits = digits, pval.digits = pval.digits,
                               as.na = as.na, check = check, output = output),
                   result = list(cor = cor.mat, n = n.mat, p = p.mat))

  } else {

    object <- list(call = match.call(),
                   data = list(group1 = x.group[[1]], group2 = x.group[[2]]),
                   args = list(method = method, use = use, group = group, print = print,
                               tri = tri, p.adj = p.adj, digits = digits, pval.digits = pval.digits,
                               as.na = as.na, check = check, output = output),
                   result = list(group1 = list(cor = object.g1$result$cor, n = object.g1$result$n,
                                               p = object.g1$result$p),
                                 group2 = list(cor = object.g2$result$cor, n = object.g2$result$n,
                                               p = object.g2$result$p)))

  }

  class(object) <- "cor.matrix"

  ####################################################################################
  # Output

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
