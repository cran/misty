#' Print square.matrix object
#'
#' This function prints the \code{square.matrix} object
#'
#' @param x           \code{square.matrix} object.
#' @param tri         a character string or character vector indicating which triangular of the matrix
#'                    to show on the console, i.e., \code{both} for upper and lower triangular,
#'                    \code{lower} for the lower triangular, and \code{upper} for the upper
#'                    triangular.
#' @param digits      an integer value indicating the number of decimal places digits to be used for
#'                    displaying results.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#' @param ...         further arguments passed to or from other methods.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{cont.coef}},\code{\link{cramers.v}}, \code{\link{na.auxiliary}}, \code{\link{na.coverage}},
#' \code{\link{phi.coef}}, \code{\link{poly.cor}}
#'
#' @method print square.matrix
#'
#' @export
#'
#' @examples
#' #------------------------------------
#' # Pearson's Contingency Coefficient
#' dat <- data.frame(x = c(1, 1, 2, 1, 3, 3, 2, 2, 1, 2),
#'                   y = c(3, 2, 3, 1, 2, 4, 1, 2, 3, 4),
#'                   z = c(2, 2, 2, 1, 2, 2, 1, 2, 1, 2), stringsAsFactors = FALSE)
#'
#' # Adjusted Contingency coefficient between x and y
#' dat.cont <- cont.coef(dat[, c("x", "y")], output = FALSE)
#'
#' # Print cont.coef object with 5 digits
#' print(dat.cont, digits = 5)
#'
#' #------------------------------------
#' # Cramer's V
#' dat <- data.frame(x = c(1, 1, 2, 1, 3, 3, 2, 2, 1, 2),
#'                   y = c(1, 2, 2, 1, 3, 4, 1, 2, 3, 1),
#'                   z = c(1, 1, 2, 1, 2, 3, 1, 2, 3, 2),
#'                   stringsAsFactors = FALSE)
#'
#' # Bias-corrected Cramer's V between x and y
#' dat.cramer <- cramers.v(dat[, c("x", "y")], output = FALSE)
#'
#' # Print cramers.v object with 4 digits
#' print(dat.cramer, digits = 4)
#'
#' #------------------------------------
#' # Auxiliary variables analysis
#' dat <- data.frame(x1 = c(1, NA, 2, 5, 3, NA, 5, 2),
#'                   x2 = c(4, 2, 5, 1, 5, 3, 4, 5),
#'                   x3 = c(NA, 3, 2, 4, 5, 6, NA, 2),
#'                   x4 = c(5, 6, 3, NA, NA, 4, 6, NA),
#'                   stringsAsFactors = FALSE)
#'
#' # Auxiliary variables
#' dat.na.auxiliary <- na.auxiliary(dat, output = FALSE)
#'
#' # Print na.auxiliary object with 3 digits
#' print(dat.na.auxiliary, digits = 3)
#'
#' #------------------------------------
#' # Variance-Covariance Coverage
#' dat <- data.frame(x = c(1, NA, NA, 6, 3),
#'                   y = c(7, NA, 8, 9, NA),
#'                   z = c(2, NA, 3, NA, 5),
#'                   stringsAsFactors = FALSE)
#'
#' # Create missing data indicator matrix R
#' na.coverage(dat)
#'
#' # Create missing data indicator matrix R
#' dat.coverage <- na.coverage(dat, output = FALSE)
#'
#' # Print na.coverage object with 5 digits
#' print(dat.coverage, digits = 5)
#'
#' #------------------------------------
#' # Phi Coefficient
#' dat <- data.frame(x1 = c(0, 1, 0, 1, 0, 1, 0, 1, 1, 0),
#'                   x2 = c(0, 1, 0, 0, 1, 1, 1, 1, 1, 1),
#'                   x3 = c(0, 1, 0, 1, 1, 1, 1, 1, 0, 0),
#'                   stringsAsFactors = FALSE)
#'
#' # Ajusted phi coefficient between x1 and x2
#' dat.phi <- phi.coef(dat[, c("x1", "x2")], output = FALSE)
#'
#' # Print phi.coef object with 5 digits
#' print(dat.phi, digits = 5)
#'
#' # Polychoric Correlation Matrix
#' dat <- data.frame(x1 = c(1, 1, 3, 2, 1, 2, 3, 2, 3, 1),
#'                   x2 = c(1, 2, 1, 1, 2, 2, 2, 1, 3, 1),
#'                   x3 = c(1, 3, 2, 3, 3, 1, 3, 2, 1, 2), stringsAsFactors = FALSE)
#'
#' # Polychoric correlation matrix
#' dat.poly <- poly.cor(dat, output = FALSE)
#'
#' # Print poly.cor object with 3 digits
#' print(dat.poly, digits = 3)
print.square.matrix <- function(x, tri = x$args$tri, digits = x$args$digits, check = TRUE, ...) {

  ####################################################################################
  # Input Check

  if (isTRUE(check)) {

    #......
    # Check input 'tri'
    if (any(!tri %in% c("both", "lower", "upper"))) {

      stop("Character string in the argument 'tri' does not match with \"both\", \"lower\", or \"upper\".",
              call. = FALSE)

    }

    #......
    # Check input 'digits'
    if (digits %% 1L != 0L || digits < 0L) {

      stop("Specify a positive integer number for the argument 'digits'", call. = FALSE)

    }

  }

  ####################################################################################
  # Pearson's Contingency Coefficient

  if (x$type == "cont.coef") {

    ####################################################################################
    # Data and Arguments

    # Print object
    print.object <- x$result

    # Print triangular
    tri <- ifelse(all(c("both", "lower", "upper") %in% tri), "lower", tri)

    ####################################################################################
    # Main Function

    #----------------------------------------
    # Two variables
    if (is.null(dim(print.object))) {

      print.object <- cbind("  Estimate: ", ifelse(!is.na(print.object), formatC(print.object, digits = digits, format = "f"), print.object))

    #----------------------------------------
    # More than two variables
    } else {

      # Format contingenccy coefficients
      print.object <- formatC(print.object, digits = digits, format = "f")

      # Lower and/or upper triangular
      if (tri == "lower") {

        print.object[upper.tri(print.object)] <- ""

      }

      if (tri == "upper") {

        print.object[lower.tri(print.object)] <- ""

      }

      # Set diagonal to "
      diag(print.object) <- ""

      # Format
      row.names(print.object) <- paste("", row.names(print.object))

    }

    ####################################################################################
    # Output

    if (is.null(dim(x$result))) {

      if (isTRUE(x$args$adjust)) {

        cat("Adjusted Contingency Coefficient\n\n")

      } else {

        cat("Contingency Coefficient\n\n")

      }

    } else {

      if (isTRUE(x$args$adjust)) {

        cat("Adjusted Contingency Coefficient Matrix\n\n")

      } else {

        cat("Contingency Coefficient Matrix\n\n")

      }

    }

    if (is.null(dim(x$result))) {

      write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE)

    } else {

      print(print.object, quote = FALSE, right = TRUE)

    }

  ####################################################################################
  # Cramer's V
  } else if (x$type == "cramers.v") {

    #-----------------------------------------------------------------------------------
    # Data and Arguments

    # Print object
    print.object <- x$result

    # Print triangular
    tri <- ifelse(all(c("both", "lower", "upper") %in% tri), "lower", tri)

    #-----------------------------------------------------------------------------------
    # Main Function

    #........................................
    # Two variables
    if (is.null(dim(print.object))) {

      print.object <- cbind("  Estimate: ", formatC(print.object, digits = digits, format = "f"))

      #........................................
      # More than two variables
    } else {

      # Format contingenccy coefficients
      print.object <- formatC(print.object, digits = digits, format = "f")

      # Lower and/or upper triangular
      if (tri == "lower") {

        print.object[upper.tri(print.object)] <- ""

      }

      if (tri == "upper") {

        print.object[lower.tri(print.object)] <- ""

      }

      # Diagonal
      diag(print.object) <- ""

      # Format
      row.names(print.object) <- paste("", row.names(print.object))

    }

    #-----------------------------------------------------------------------------------
    # Output

    if (is.null(dim(x$result))) {

      if (isTRUE(x$args$correct)) {

        cat("Bias-Corrected Cramer's V\n\n")

      } else {

        cat("Cramer's V\n\n")

      }

    } else {

      if (isTRUE(x$args$correct)) {

        cat("Bias-Corrected Cramer's V Matrix\n\n")

      } else {

        cat("Cramer's V Matrix\n\n")

      }

    }

    if (is.null(dim(x$result))) {

      write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE)

    } else {

      print(print.object, quote = FALSE, right = TRUE)

    }

  ####################################################################################
  # Auxiliary variable analysis
  } else if (x$type == "na.auxiliary") {

    ####################################################################################
    # Data and Arguments

    # Print object
    print.object <- x$result

    ####################################################################################
    # Main Function

    #-----------------------------------------
    # Format correlation matrix
    print.object$cor <- apply(print.object$cor, 2, function(y) formatC(as.numeric(y), digits = digits, format = "f"))

    # Lower and/or upper triangular
    if (tri == "lower") {

      print.object$cor[upper.tri(print.object$cor)] <- ""

    }

    if (tri == "upper") {

      print.object$cor[lower.tri(print.object$cor)] <- ""

    }

    diag(print.object$cor) <- ""

    #-----------------------------------------
    # Format Cohen's d matrix

    print.object$d <- apply(print.object$d, 2, function(y) formatC(as.numeric(y), digits = digits, format = "f"))
    diag(print.object$d) <- ""

    # Print table
    print.object  <- data.frame(cbind(c("", colnames(print.object$cor), "", colnames(print.object$cor)),
                                      rbind(colnames(print.object$cor), print.object$cor, colnames(print.object$cor), print.object$d)),
                                stringsAsFactors = FALSE)

    # Format
    print.object[, 1L] <- paste0("   ", print.object[, 1L])

    print.object[, 1L] <- format(print.object[, 1L], justify = "left")

    print.object[, -1L] <- apply(print.object[, -1L], 2, function(y) format(y, justify = "right"))

    ####################################################################################
    # Output

    cat(" Auxiliary Variables\n\n",
        " Variables related to the incomplete variable\n\n",
        "  Pearson product-moment correlation matrix\n")

    write.table(print.object[1:(nrow(x$result$cor) + 1L), ], quote = FALSE, row.names = FALSE, col.names = FALSE)

    cat("\n")

    cat("  Variables related to the probability of missigness\n\n",
        "  Cohen's d\n")

    write.table(print.object[(nrow(x$result$cor) + 2L):nrow(print.object), ], quote = FALSE, row.names = FALSE, col.names = FALSE)

    cat("\n",
        " Note. Indicator variables are in the rows (0 = miss, 1 = obs)\n")

  ####################################################################################
  # Variance-Covariance Coverage
  } else if (x$type == "na.coverage") {

    #-----------------------------------------------------------------------------------
    # Main Function

    # Print object
    print.object <- x$result

    #........................................
    # Lower and/or upper triangular

    if (tri == "lower") {

      print.object[upper.tri(print.object)] <- ""

    }

    if (tri == "upper") {

      print.object[lower.tri(print.object)] <- ""

    }

    #-----------------------------------------------------------------------------------
    # Main Function

    # Format proportions
    print.object <- apply(print.object, 2, function(y) ifelse(!is.na(as.numeric(y)), formatC(as.numeric(y), digits = digits, format = "f"), ""))

    # Row names
    row.names(print.object) <- paste0("  ", row.names(x$result))

    #-----------------------------------------------------------------------------------
    # Output

    cat(" Variance-Covariance Coverage\n\n")

    print(print.object,  quote = FALSE, row.names = FALSE, max = 99999L, right = TRUE)

  ####################################################################################
  # Phi Coefficient
  } else if (x$type == "phi.coef") {

    ####################################################################################
    # Data and Arguments

    # Print object
    print.object <- x$result

    # Print triangular
    tri <- ifelse(all(c("both", "lower", "upper") %in% tri), "lower", tri)

    ####################################################################################
    # Main Function

    #----------------------------------------
    # Two variables
    if (is.null(dim(print.object))) {

      print.object <- cbind("  Estimate: ", formatC(print.object, digits = digits, format = "f"))

    #----------------------------------------
    # More than two variables
    } else {

      # Format contingenccy coefficients
      print.object <- formatC(print.object, digits = digits, format = "f")

      # Lower and/or upper triangular
      if (tri == "lower") {

        print.object[upper.tri(print.object)] <- ""

      }

      if (tri == "upper") {

        print.object[lower.tri(print.object)] <- ""

      }

      # Set diagonal to "
      diag(print.object) <- ""

      # Format
      row.names(print.object) <- paste("", row.names(print.object))

    }

    ####################################################################################
    # Output

    if (is.null(dim(x$result))) {

      if (isTRUE(x$args$adjust)) {

        cat("Adjusted Phi Coefficient\n\n")

      } else {

        cat("Phi Coefficient\n\n")

      }

    } else {

      if (isTRUE(x$args$adjust)) {

        cat("Adjusted Phi Coefficient Matrix\n\n")

      } else {

        cat("Phi Coefficient Matrix\n\n")

      }

    }

    if (is.null(dim(x$result))) {

      write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE)

    } else {

      print(print.object, quote = FALSE, right = TRUE)

    }

  ####################################################################################
  # Polychoric Correlation Matrix
  } else if (x$type == "poly.cor") {

    ####################################################################################
    # Data and Arguments

    #----------------------------------------
    # Print object

    print.object <- x$result$cor

    #----------------------------------------
    # Print triangular

    tri <- ifelse(all(c("both", "lower", "upper") %in% tri), "lower", tri)

    ####################################################################################
    # Main Function

    #----------------------------------------
    # Print object

    print.object <- formatC(print.object, digits = digits, format = "f")
    row.names(print.object) <- paste("", row.names(print.object))


    # Empty matrix diagonal
    diag(print.object) <- ""

    #----------------------------------------
    # Lower and/or upper triangular

    if (tri == "lower") {

      print.object[upper.tri(print.object)] <- ""

    }

    if (tri == "upper") {

      print.object[lower.tri(print.object)] <- ""

    }

    #----------------------------------------
    # Row names
    if (!is.null(rownames(print.object))) {

      row.names(print.object) <- format(row.names(print.object), justify = "left")

    }

    ####################################################################################
    # Output

    cat("Polychoric Correlation Matrix\n\n")

    print(print.object, quote = FALSE, right = TRUE, max = 99999L)

  }

}
