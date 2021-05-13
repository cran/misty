#' Correlation Matrix with Statistical Significance Testing
#'
#' This function computes a correlation matrix and computes significance values (\emph{p}-values) for testing the hypothesis
#' H0: \eqn{\rho} = 0 for all possible pairs of variables.
#'
#' @param x            a matrix or data frame.
#' @param method       a character vector indicating which correlation coefficient is to be computed, i.e.
#'                     \code{"pearson"} for Pearson product-moment correlation coefficient (default),
#'                     \code{"spearman"} for Spearman's rank-order correlation coefficient, \code{kendall-b} for
#'                     Kendall's Tau-b correlation coefficient or \code{kendall-c} for Kendall-Stuart's Tau-c
#'                     correlation coefficient.
#' @param na.omit      logical: if \code{TRUE}, incomplete cases are removed before conducting the analysis
#'                     (i.e., listwise deletion); if \code{FALSE} (default), pairwise deletion is used.
#' @param group        a numeric vector, character vector of factor as grouping variable to show results for
#'                     each group separately, i.e., upper triangular for one group and lower triangular for
#'                     another group. Note that the grouping variable is limited to two groups.
#' @param print        a character string or character vector indicating which additional results to show,
#'                     i.e. \code{"all"}, for all additional results: \code{"n"} for the sample sizes, and
#'                     \code{"p"} for \emph{p}-values.
#' @param tri          a character string indicating which triangular of the matrix to show on the console,
#'                     i.e., \code{both} for upper and lower triangular, \code{lower} (default) for the
#'                     lower triangular, and \code{upper} for the upper triangular.
#' @param p.adj        a character string indicating an adjustment method for multiple testing based on \code{\link{p.adjust}},
#'                     i.e.,  \code{none} (default), \code{bonferroni}, \code{holm}, \code{hochberg}, \code{hommel},
#'                     \code{BH}, \code{BY}, or \code{fdr}.
#' @param digits       an integer value indicating the number of decimal places to be used for displaying
#'                     correlation coefficients.
#' @param p.digits     an integer value indicating the number of decimal places to be used for displaying \emph{p}-values.
#' @param as.na        a numeric vector indicating user-defined missing values,
#'                     i.e. these values are converted to \code{NA} before conducting the analysis.
#' @param check        logical: if \code{TRUE}, argument specification is checked.
#' @param output       logical: if \code{TRUE}, output is shown on the console.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{cohens.d}}, \code{\link{cor.cont}}, \code{\link{cor.cramer}},
#' \code{\link{multilevel.icc}}, \code{\link{cor.phi}}, \code{\link{na.auxiliary}}, \code{\link{size.cor}}.
#'
#' @references
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology - Using R and SPSS}.
#' John Wiley & Sons.
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following entries:
#' function call (\code{call}), type of analysis \code{type}, matrix or data frame specified in
#' \code{x} (\code{data}), specification of function arguments (\code{args}), and
#' list with results (\code{result}).
#'
#' @export
#'
#' @examples
#' dat <- data.frame(group = c("a", "a", "a", "a", "a",
#'                             "b", "b", "b", "b", "b"),
#'                   x = c(5, NA, 6, 4, 6, 7, 9, 5, 8, 7),
#'                   y = c(3, 3, 5, 6, 7, 4, 7, NA, NA, 8),
#'                   z = c(1, 3, 1, NA, 2, 4, 6, 5, 9, 6),
#'                   stringsAsFactors = TRUE)
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
#' # Kendall's Tau-c correlation matrix using pairwise deletion
#' cor.matrix(dat[, c("x", "y", "z")], method = "kendall-c")
#'
#' # Pearson product-moment correlation coefficient matrix using pairwise deletion,
#' # print sample size and significance values
#' cor.matrix(dat[, c("x", "y", "z")], print = "all")
#'
#' # Pearson product-moment correlation coefficient matrix using listwise deletion,
#' # print sample size and significance values
#' cor.matrix(dat[, c("x", "y", "z")], na.omit = TRUE, print = "all")
#'
#' # Pearson product-moment correlation coefficient matrix using listwise deletion,
#' # print sample size and significance values with Bonferroni correction
#' cor.matrix(dat[, c("x", "y", "z")], na.omit = TRUE, print = "all", p.adj = "bonferroni")
#'
#' # Pearson product-moment correlation coefficient matrix using pairwise deletion,
#' # results for group "a" and "b" separately
#' cor.matrix(dat[, c("x", "y", "z")], group = dat$group, print = "all")
cor.matrix <- function(x, method = c("pearson", "spearman", "kendall-b", "kendall-c"), na.omit = FALSE,
                       group = NULL, print = c("all", "cor", "n", "p"), tri = c("both", "lower", "upper"),
                       p.adj = c("none", "bonferroni", "holm", "hochberg", "hommel", "BH", "BY", "fdr"),
                       digits = 2, p.digits = 3, as.na = NULL, check = TRUE, output = TRUE) {

  ####################################################################################
  # Data

  #......
  # Check if input 'x' is missing
  if (isTRUE(missing(x))) {

    stop("Please specify a matrix or data frame for the argument 'x'.", call. = FALSE)

  }

  #......
  # Check if input 'x' is NULL
  if (isTRUE(is.null(x))) {

    stop("Input specified for the argument 'x' is NULL.", call. = FALSE)

  }

  #......
  # Matrix or data frame for the argument 'x'?
  if (isTRUE(!is.matrix(x) && !is.data.frame(x))) {

    stop("Please specifiy a matrix or data frame for the argument 'x'.", call. = FALSE)

  }

  #-----------------------------------------
  # As data frame

  # Is 'x' a matrix?
  is.mat <- is.matrix(x) && !is.data.frame(x)

  # Coerce to a data frame
  x <- as.data.frame(x, stringsAsFactors = FALSE)

  ####################################################################################
  # Input Check

  #......
  # Check input 'check'
  if (isTRUE(!is.logical(check))) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }

  #----------------------------------------

  if (isTRUE(check)) {

    #......
    # Check input 'x'
    if (isTRUE(any(vapply(x, function(y) !is.numeric(y), FUN.VALUE = logical(1L))))) {

      stop("Please specify a matrix or data frame with numeric vectors.", call. = FALSE)

    }

    #......
    # Check input 'method'
    if (isTRUE(any(!method %in% c("pearson", "spearman", "kendall-b", "kendall-c")))) {

      stop("Character string in the argument 'method' does not match with \"pearson\", \"spearman\", \"kendall-b\", or \"kendall-c\".",
           call. = FALSE)

    }

    #......
    # Check input 'na.omit'
    if (isTRUE(!is.logical(na.omit))) {

      stop("Please specify TRUE or FALSE for the argument 'na.omit'.", call. = FALSE)

    }

    #......
    # Check input 'group'
    if (isTRUE(!is.null(group))) {

      # Length of 'group' match with 'x'?
      if (isTRUE(length(group) != nrow(x))) {

        if (isTRUE(is.vector(group) && !is.factor(group))) {

          # Matrix
          if (isTRUE(is.mat)) {

            stop("Length of the vector specified in 'group' does not match the number of rows of the matrix specified in 'x'.",
                 call. = FALSE)

          # Data frame
          } else {

            stop("Length of the vector specified in 'group' does not match the number of rows of the data frame specified in 'x'.",
                 call. = FALSE)

          }

        # Factor
        } else {

          # Matrix
          if (isTRUE(is.mat)) {

            stop("Length of the factor specified in 'group' does not match the number of rows of the matrix specified in 'x'.",
                 call. = FALSE)

          # Data frame
          } else {

            stop("Length of the factor specified in 'group' does not match the number of rows of the data frame specified in 'x'.",
                 call. = FALSE)

          }

        }

      }

      # Specified two groups only?
      if (isTRUE(length(na.omit(unique(group))) != 2)) {

        stop("Please specify a grouping variable with only two groups for the argument 'group'.", call. = FALSE)

      }

      # Zero variance in one of the groups
      x.zero.var <- vapply(split(x, f = group), function(y) apply(y, 2, function(z) length(na.omit(unique(z))) == 1L), FUN.VALUE = logical(ncol(x)))

      if (isTRUE(any(x.zero.var))) {

        stop(paste("Following variables specified in 'x' have zero variance in at least one of the groups specified in 'group': ",
                   paste(names(which(apply(x.zero.var, 1, any))), collapse = ", ")), call. = FALSE)

      }

    }

    #......
    # Check input 'print'
    if (isTRUE(any(!print %in% c("all", "cor", "n", "p")))) {

      stop("Character string(s) in the argument 'print' does not match with \"all\", \"cor\", \"n\", or \"p\".",
           call. = FALSE)

    }

    #......
    # Check input 'tri'
    if (isTRUE(any(!tri %in% c("both", "lower", "upper")))) {

      stop("Character string in the argument 'tri' does not match with \"both\", \"lower\", or \"upper\".",
           call. = FALSE)

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

    #......
    # Check input 'x' for zero variance
    x.zero.var <- vapply(x, function(y) length(na.omit(unique(y))) == 1L, FUN.VALUE = logical(1))
    if (isTRUE(any(x.zero.var))) {

      warning(paste0("Following variables in the matrix or data frame specified in 'x' have zero variance: ",
                     paste(names(which(x.zero.var)), collapse = ", ")), call. = FALSE)

    }

  }

  ####################################################################################
  # Data and Arguments

  #-----------------------------------------
  # Convert user-missing values into NA

  if (isTRUE(!is.null(as.na))) {

    x <- misty::as.na(x, na = as.na, check = check)

    # Variable with missing values only
    x.miss <- vapply(x, function(y) all(is.na(y)), FUN.VALUE = logical(1L))
    if (isTRUE(any(x.miss))) {

      stop(paste0("After converting user-missing values into NA, following variables are completely missing: ",
                  paste(names(which(x.miss)), collapse = ", ")), call. = FALSE)

    }

    # Constant variables
    x.con <- vapply(x, function(y) var(as.numeric(y), na.rm = TRUE) == 0L, FUN.VALUE = logical(1))
    if (isTRUE(any(x.con))) {

      stop(paste0("After converting user-missing values into NA, following variables are constant: ",
                  paste(names(which(x.con)), collapse = ", ")), call. = FALSE)

    }

  }

  #----------------------------------------
  # Correlation coefficient

  method <- ifelse(all(c("pearson", "spearman", "kendall-b", "kendall-c") %in% method), "pearson", method)

  #----------------------------------------
  # Internal function: Kendall-Stuart Tau-c

  if (isTRUE(method == "kendall-c")) {

    .internal.tau.c <- function(xx, yy) {

      ####################################################################################
      # Main Function

      # Contingency table
      x.table <- table(xx, yy)

      # Number of rows
      x.nrow <- nrow(x.table)

      # Number of columns
      x.ncol <- ncol(x.table)

      # Sample size
      x.n <- sum(x.table)

      # Minimum of number of rows/columns
      x.m <- min(dim(x.table))

      #-----------------------------------------
      if (isTRUE(x.n > 1L && x.nrow > 1L && x.ncol > 1L)) {

        pi.c <- pi.d <- matrix(0L, nrow = x.nrow, ncol = x.ncol)

        x.col <- col(x.table)
        x.row <- row(x.table)

        for (i in 1L:x.nrow) {

          for (j in 1L:x.ncol) {

            pi.c[i, j] <- sum(x.table[x.row < i & x.col < j]) + sum(x.table[x.row > i & x.col > j])
            pi.d[i, j] <- sum(x.table[x.row < i & x.col > j]) + sum(x.table[x.row > i & x.col < j])

          }

        }

        # Concordant
        x.con <- sum(pi.c * x.table)/2L

        # Discordant
        x.dis <- sum(pi.d * x.table)/2L

        # Kendall-Stuart Tau-c
        tau.c <- (x.m*2L * (x.con - x.dis)) / ((x.n^2L) * (x.m - 1L))

      } else {

        tau.c <- NA

      }

      #-----------------------------------------
      # If n > 2
      if (isTRUE(x.n > 2L)) {

        # Asymptotic standard error
        sigma <- sqrt(4L * x.m^2L / ((x.m - 1L)^2L * x.n^4L) * (sum(x.table * (pi.c - pi.d)^2L) - 4L * (x.con - x.dis)^2L/x.n))

        # Test statistic
        z <- tau.c / sigma

        # Two-tailed p-value
        pval <- pnorm(abs(z), lower.tail = FALSE)*2L

      } else {

        sigma <- NA
        pval <- NA

      }

      ####################################################################################
      # Return object

      object <- list(data = data.frame(xx, yy, stringsAsFactors = FALSE),
                     result = list(tau.c = tau.c,
                                   n = x.n,
                                   sigma = sigma,
                                   z = z,
                                   pval = pval))

      return(object)

    }

  }

  #-----------------------------------------
  # Listwise deletion

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

  #-----------------------------------------
  # Print correlation, sample size or significance values

  if (isTRUE(all(c("all", "cor", "n", "p") %in% print))) { print <- "cor" }

  if (isTRUE(length(print) == 1 && "all" %in% print)) { print <- c("cor", "n", "p") }

  #-----------------------------------------
  # Print triangular

  tri <- ifelse(all(c("both", "lower", "upper") %in% tri), "lower", tri)

  #-----------------------------------------
  # Adjustment method for multiple testing

  p.adj <- ifelse(all(c("none", "bonferroni", "holm", "hochberg", "hommel", "BH", "BY", "fdr") %in% p.adj), "none", p.adj)

  #-----------------------------------------
  # Pairwise combination of columns

  comb <- combn(seq_len(ncol(x)), m = 2L)

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

  if (isTRUE(is.null(group))) {

    #........................................
    # Correlation matrix

    # Product-moment or Spearman correlation coefficient
    if (isTRUE(!method %in% c("kendall-b", "kendall-c"))) {

      cor.mat <- suppressWarnings(cor(x, use = "pairwise.complete.obs", method = method))

    }

    # Kendall Tau-b
    if (isTRUE(method == "kendall-b")) {

      cor.mat <- suppressWarnings(cor(x, use = "pairwise.complete.obs", method = "kendall"))

    }

    # Kendall-Stuart Tau-c
    if (isTRUE(method == "kendall-c")) {

      cor.mat <- matrix(NA, ncol = ncol(x), nrow = ncol(x), dimnames = list(colnames(x), colnames(x)))

      cor.mat[lower.tri(cor.mat)] <- apply(comb, 2, function(y) suppressWarnings(.internal.tau.c(x[, y[1L]], x[, y[2L]])$result$tau.c))
      cor.mat[upper.tri(cor.mat)] <- t(cor.mat)[upper.tri(cor.mat)]

      diag(cor.mat) <- 1

    }

    #........................................
    # Sample size

    if (!isTRUE(na.omit)) {

      n <- apply(comb, 2, function(y) nrow(na.omit(cbind(x[, y[1]], x[, y[2L]]))))

    } else {

      n <- nrow(na.omit(x))

    }

    n.mat[lower.tri(n.mat)] <- n
    n.mat[upper.tri(n.mat)] <- t(n.mat)[upper.tri(n.mat)]

    #........................................
    # p-values

    # Product-moment or Spearman correlation coefficient
    if (isTRUE(!method %in% c("kendall-b", "kendall-c"))) {

      pval <- apply(comb, 2, function(y) suppressWarnings(cor.test(x[, y[1L]], x[, y[2L]], method = method))$p.value)

    }

    # Kendall Tau-b
    if (isTRUE(method == "kendall-b")) {

      pval <- apply(comb, 2, function(y) suppressWarnings(cor.test(x[, y[1L]], x[, y[2L]], method = "kendall"))$p.value)

    }

    # Kendall-Stuart Tau-c
    if (isTRUE(method == "kendall-c")) {

      pval <- apply(comb, 2, function(y) suppressWarnings(.internal.tau.c(x[, y[1L]], x[, y[2L]])$result$pval))

    }

    ###

    # Adjust p-values for multiple comparison
    if (isTRUE(p.adj != "none")) {

      pval <- p.adjust(pval, method = p.adj)

    }

    p.mat[lower.tri(p.mat)] <- pval
    p.mat[upper.tri(p.mat)] <- t(p.mat)[upper.tri(p.mat)]

  #-----------------------------------------
  # Grouping

  } else {

    # At least 3 observations
    if (isTRUE(any(table(group) < 3))) {

      stop("There are not enough observations for each group specified in 'group' to compute the correlation matrix separately.",
           call. = FALSE)

    }

    # Grouping
    x.group <- split(x, f = group)

    object.g1 <- misty::cor.matrix(x.group[[1]], method = method, na.omit = na.omit, group = NULL,
                                   digits = digits, print = print, tri = tri, p.adj = p.adj, p.digits = p.digits,
                                   check = FALSE, output = FALSE)

    object.g2 <- misty::cor.matrix(x.group[[2]], method = method, na.omit = na.omit, group = NULL,
                                   digits = digits, print = print, tri = tri, p.adj = p.adj, p.digits = p.digits,
                                   check = FALSE, output = FALSE)

    #........................................
    # Correlation matrix, Sample size, and p-values

    cor.mat <- object.g1$result$cor

    n.mat <- object.g1$result$n

    p.mat <- object.g1$result$p

    #........................................
    # Lower triangular: Group 1; Upper triangular: Group 2

    cor.mat[upper.tri(cor.mat)] <- object.g2$result$cor[upper.tri(object.g2$result$cor)]
    n.mat[upper.tri(n.mat)] <- object.g2$result$n[upper.tri(object.g2$result$n)]
    p.mat[upper.tri(p.mat)] <- object.g2$result$p[upper.tri(object.g2$result$p)]

  }

  ####################################################################################
  # Return object

  object <- list(call = match.call(),
                 type = "cor.matrix",
                 data = x,
                 args = list(method = method, na.omit = na.omit, group = group, print = print,
                             tri = tri, p.adj = p.adj, digits = digits, p.digits = p.digits,
                             as.na = as.na, check = check, output = output),
                 result = list(cor = cor.mat, n = n.mat, p = p.mat))

  class(object) <- "misty.object"

  ####################################################################################
  # Output

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
