#' Print cor.matrix object
#'
#' This function prints the \code{cor.matrix} object
#'
#' @param x           \code{cor.matrix} object.
#' @param print        a character string or character vector indicating which additional results to show,
#'                     i.e. \code{"all"}, for all additional results: \code{"n"} for the sample sizes,
#'                     \code{"p"}, and for \emph{p}-values.
#' @param tri          a character string indicating which triangular of the matrix to show on the console,
#'                     i.e., \code{both} for upper and lower triangular, \code{lower} for the lower triangular,
#'                     and \code{upper} for the upper triangular.
#' @param digits       an integer value indicating the number of decimal places to be used for displaying
#'                     correlation coefficients.
#' @param pval.digits  an integer indicating the number of decimal places to be used for displaying \emph{p}-values.
#' @param check        logical: if \code{TRUE}, argument specification is checked.
#' @param ...          further arguments passed to or from other methods.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at},
#'
#' @seealso
#' \code{\link{cor.matrix}}
#'
#' @method print cor.matrix
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
#' dat.cor <- cor.matrix(dat[, c("x", "y", "z")], output = FALSE)
#'
#' # Print cor.matrix object with 3 digits for correlation coefficients
#' # and 4 digits for significance values
#' print(dat.cor, print = "all", digits = 3, pval.digits = 4)
print.cor.matrix <- function(x, print = x$args$print, tri = x$args$tri, digits = x$args$digits,
                             pval.digits = x$args$pval.digits, check = TRUE, ...) {

  ####################################################################################
  # Input Check

  if (isTRUE(check)) {

    #......
    # Check input 'print'
    if (any(!print %in% c("all", "cor", "n", "p"))) {

      stop("Character string(s) in the argument 'print' does not match with \"all\", \"cor\", \"n\", or \"p\".",
              call. = FALSE)

    }

    #......
    # Check input 'tri'
    if (any(!tri %in% c("both", "lower", "upper"))) {

      stop("Character string in the argument 'tri' does not match with \"both\", \"lower\", or \"upper\".",
              call. = FALSE)

    }

    #......
    # Check input 'digits'
    if (digits %% 1 != 0 | digits < 0) {

      stop("Specify a positive integer number for the argument 'digits'.", call. = FALSE)

    }

    #......
    # Check input 'pval.digits'
    if (pval.digits %% 1 != 0 | digits < 0) {

      stop("Specify a positive integer number for the argument 'pval.digits'.", call. = FALSE)

    }

  }

  ####################################################################################
  # Arguments

  #......
  # Print correlation, sample size or significance values
  if (all(c("all", "cor", "n", "p") %in% print)) { print <- "cor" }

  if (length(print) == 1 && "all" %in% print) { print <- c("cor", "n", "p") }

  #......
  # Print triangular
  tri <- ifelse(all(c("both", "lower", "upper") %in% tri), "lower", tri)

  ####################################################################################
  # Main Function

  #......
  # Print object
  print.object <- x$result

  #------------------------------------
  # No grouping
  if (is.null(x$args$group)) {

    #........................................
    # Round and format

    print.object$cor <- formatC(print.object$cor, digits = digits, format = "f")
    print.object$p <- formatC(print.object$p, digits = pval.digits, format = "f")
    print.object$n <- formatC(print.object$n)

    diag(print.object$cor) <- ""
    diag(print.object$p) <- ""
    diag(print.object$n) <- ""

    #........................................
    # Lower and/or upper triangular

    if (tri == "lower") {

      print.object$cor[upper.tri(print.object$cor)] <- ""
      print.object$n[upper.tri(print.object$n)] <- ""
      print.object$p[upper.tri(print.object$p)] <- ""

    }

    if (tri == "upper") {

      print.object$cor[lower.tri(print.object$cor)] <- ""
      print.object$n[lower.tri(print.object$n)] <- ""
      print.object$p[lower.tri(print.object$p)] <- ""

    }

    #........................................
    # Row names

    if (!is.null(row.names(print.object$cor))) {

      # Rownames
      row.names(print.object$cor) <- paste0("  ", row.names(print.object$cor))
      row.names(print.object$n) <- paste0("  ", row.names(print.object$n))
      row.names(print.object$p) <- paste0("  ", row.names(print.object$p))

    }

    print.object$cor <- apply(print.object$cor, 2, function(y) format(y, justify = "right"))
    print.object$n <- apply(print.object$n, 2, function(y) format(y, justify = "right"))
    print.object$p <- apply(print.object$p, 2, function(y) format(y, justify = "right"))

  #------------------------------------
  # Grouping
  } else {

    #........................................
    # Round and format

    print.object.g1 <- print.object$group1
    print.object.g2 <- print.object$group2

    x.ncol <- ncol(print.object.g1$cor)

    #....
    # Group 1
    print.object.g1$cor <- formatC(print.object.g1$cor, digits = digits, format = "f")
    print.object.g1$p <- formatC(print.object.g1$p, digits = pval.digits, format = "f")
    print.object.g1$n <- formatC(print.object.g1$n)

    diag(print.object.g1$cor) <- ""
    diag(print.object.g1$p) <- ""
    diag(print.object.g1$n) <- ""

    print.object.g1$cor <- format(print.object.g1$cor, justify = "left")
    print.object.g1$n <- format(print.object.g1$n, justify = "left")
    print.object.g1$p <- format(print.object.g1$p, justify = "left")

    print.object.g1$cor <- vapply(as.data.frame(print.object.g1$cor), function(y) format(y, justify = "right"), FUN.VALUE = character(x.ncol))
    print.object.g1$n <- vapply(as.data.frame(print.object.g1$n), function(y) format(y, justify = "right"), FUN.VALUE = character(x.ncol))
    print.object.g1$p <- vapply(as.data.frame(print.object.g1$p), function(y) format(y, justify = "right"), FUN.VALUE = character(x.ncol))

    #....
    ### Group 2
    print.object.g2$cor <- formatC(print.object.g2$cor, digits = digits, format = "f")
    print.object.g2$p <- formatC(print.object.g2$p, digits = pval.digits, format = "f")
    print.object.g2$n <- formatC(print.object.g2$n)

    diag(print.object.g2$cor) <- ""
    diag(print.object.g2$p) <- ""
    diag(print.object.g2$n) <- ""

    print.object.g2$cor <- format(print.object.g2$cor, justify = "left")
    print.object.g2$n <- format(print.object.g2$n, justify = "left")
    print.object.g2$p <- format(print.object.g2$p, justify = "left")

    print.object.g2$cor <- vapply(as.data.frame(print.object.g2$cor), function(y) format(y, justify = "right"), FUN.VALUE = character(x.ncol))
    print.object.g2$n <- vapply(as.data.frame(print.object.g2$n), function(y) format(y, justify = "right"), FUN.VALUE = character(x.ncol))
    print.object.g2$p <- vapply(as.data.frame(print.object.g2$p), function(y) format(y, justify = "right"), FUN.VALUE = character(x.ncol))

    # Print object
    print.object <- print.object.g1

    #........................................
    # Lower triangular: Group 1; Upper triangular: Group 2

    print.object$cor[upper.tri(print.object$cor)] <- print.object.g2$cor[upper.tri(print.object.g2$cor)]
    print.object$n[upper.tri(print.object$cor)] <- print.object.g2$n[upper.tri(print.object.g2$n)]
    print.object$p[upper.tri(print.object$p)] <- print.object.g2$p[upper.tri(print.object.g2$p)]

    # Row names
    row.names(print.object$cor) <- paste0("  ", row.names(print.object$cor))
    row.names(print.object$n) <- paste0("  ", row.names(print.object$n))
    row.names(print.object$p) <- paste0("  ", row.names(print.object$p))

  }

  #------------------------------------
  # Print

  #........................
  # Correlation coefficient
  if ("cor" %in% print) {

    if (x$args$method == "pearson") {

     cat(" Pearson Product-Moment Correlation Coefficient\n\n")

    }

    if (x$args$method == "spearman") {

      cat(" Spearman's Rank-Order Correlation Coefficient\n\n")

    }

    if (x$args$method == "kendall-b") {

      cat(" Kendall's Tau-b Correlation Coefficient\n\n")

    }

    print(print.object$cor, quote = FALSE, right = TRUE, max = 99999)

  }

  #........................
  # Sample size
  if ("n" %in% print) {

    if ("cor" %in% print) { cat("\n") }

    if (!is.null(x$args$group)) {

      x$args$use <- ifelse(all(c("listwise", "pairwise") %in% x$args$use) | x$args$use == "pairwise", "pairwise.complete.obs", "complete.obs")

    }

    if (x$args$use == "pairwise.complete.obs") {

      cat(" Sample Size Using Pairwise Deletion\n\n")
      print(print.object$n, quote = FALSE, right = TRUE, max = 99999)

    }

    if (x$args$use == "complete.obs") {

      if (is.null(x$args$group)) {

        cat(paste(" Sample Size Using Listwise Deletion\n  n =", nrow(na.omit(x$data)), "\n"))

      } else {

        cat(paste(" Sample Size Using Listwise Deletion\n  n in group 1 =", nrow(na.omit(x$data$group1)),
                                                      "\n  n in group 2 =", nrow(na.omit(x$data$group2)),"\n"))

      }

    }

  }

  #........................
  # p.values
  if ("p" %in% print) {

    if (any(c("cor", "n") %in% print)) { cat("\n") }

      cat(" Significance Value (p-value)\n\n")
      print(print.object$p, quote = FALSE, right = TRUE, max = 99999)
      cat(paste0("\n  Adjustment for multiple testing: ", x$args$p.adj, "\n"))

  }

  #........................
  # Grouping
  if (!is.null(x$args$group)) {

     cat(paste0("\n Note. Lower triangular: Results for group = ", sort(unique(x$args$group))[1],
                "\n       Upper triangular: Results for group = ", sort(unique(x$args$group))[2]), "\n")

  }

}
