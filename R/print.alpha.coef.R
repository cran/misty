#' Print alpha.coef object
#'
#' This function prints the \code{alpha.coef} object
#'
#' @param x          \code{alpha.coef} object.
#' @param print      a character vector indicating which results to show, i.e. \code{"all"}, for all results
#'                   \code{"alpha"} for the coefficienta alpha, and \code{"item"} for item statistics.
#' @param digits     an integer value indicating the number of decimal places to be used for displaying
#'                   coefficient alpha and item-total correlations.
#' @param check      logical: if \code{TRUE}, argument specification is checked.
#' @param ...        further arguments passed to or from other methods.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{alpha.coef}}
#'
#' @references
#' Cronbach, L.J. (1951). Coefficient alpha and the internal strucuture of tests. \emph{Psychometrika, 16}, 297-334.
#'
#' Cronbach, L.J. (2004). My current thoughts on coefficient alpha and successor procedures. \emph{Educational
#' and Psychological Measurement, 64}, 391-418.
#'
#' Feldt, L. S., Woodruff, D. J., & Salih, F. A. (1987). Statistical inference for coefficient alpha.
#' \emph{Applied Psychological Measurement, 11} 93-103.
#'
#' Kelley, K., & Pornprasertmanit, S. (2016). Confidence intervals for population reliability coefficients:
#' Evaluation of methods, recommendations, and software for composite measures. \emph{Psychological Methods, 21}, 69-92.
#'
#' @method print alpha.coef
#'
#' @export
#'
#' @examples
#' dat <- data.frame(item1 = c(5, 2, 3, 4, 1, 2, 4, 2),
#'                   item2 = c(5, 1, 3, 5, 2, 2, 5, 1),
#'                   item3 = c(4, 2, 4, 5, 1, 3, 5, 1),
#'                   item4 = c(5, 1, 2, 5, 2, 3, 4, 2))
#'
#' # Compute unstandardized coefficient alpha and item statistics
#' dat.alpha <- alpha.coef(dat, output = FALSE)
#'
#' # Print alpha.coef object to show unstandardized coefficient alpha and item statistics with 3 digits
#' print(dat.alpha, digits = 3)
#'
#' # Print alpha.coef object to show unstandardized coefficient with 3 digits
#' print(dat.alpha, print = "alpha", digits = 3)
#'
#' # Print alpha.coef object to show item statistics with 3 digits
#' print(dat.alpha, print = "item", digits = 3)
print.alpha.coef <- function(x, print = x$args$print, digits = x$args$digits, check = TRUE, ...) {

  ####################################################################################
  # Input Check

  if (isTRUE(check)) {

    #......
    # Check input 'print'
    if (!all(print %in% c("all", "alpha", "item"))) {

      warning("Character strings in the argument 'print' do not all match with \"all\", \"alpha\", or \"item\".",
              call. = FALSE)

    }

    #......
    # Check input 'digits'
    if (digits %% 1 != 0 | digits < 0) {

      warning("Specify a positive integer number for the argument 'digits'", call. = FALSE)

    }

  }

  ####################################################################################
  # Data and Arguments

  # Print object
  print.object <- x$result

  # Print coefficient alpha and/or item statistic
  if (all(c(c("all", "alpha", "item")) %in% print)) { print <- c("alpha", "item") }

  if (length(print) == 1 && "all" %in% print) { print <- c("alpha", "item") }

  ####################################################################################
  # Main Function

  #-----------------------------------------
  # Alpha
  if ("alpha" %in% print) {

    if (all(c("low", "upp") %in% names(print.object$alpha))) {

      print.object$alpha$alpha <- formatC(print.object$alpha$alpha, digits = digits, format = "f")

      print.object$alpha$low <- formatC(print.object$alpha$low, digits = digits, format = "f")
      print.object$alpha$upp <- formatC(print.object$alpha$upp, digits = digits, format = "f")

      print.object$alpha <- rbind(c(" Items", "Alpha", "Low", "Upp"), print.object$alpha)

      print.object$alpha <- apply(print.object$alpha, 2, function(y) format(y, justify = "right"))

      cat(paste0(ifelse(isTRUE(x$args$std), "Standardized ", "Unstandardized "), "Coefficient Alpha with ",
                 x$args$conf.level*100, "% Confidence Interval\n\n"))

      write.table(print.object$alpha, quote = FALSE, row.names = FALSE, col.names = FALSE)

    } else {

      print.object$alpha$alpha <- formatC(print.object$alpha$alpha, digits = digits, format = "f")

      print.object$alpha <- rbind(c(" Item", "Alpha"), print.object$alpha)

      print.object$alpha <- apply(print.object$alpha, 2, function(y) format(y, justify = "right"))

      cat(paste0(ifelse(isTRUE(x$args$std), "Standardized ", "Unstandardized "), "Coefficient Alpha\n\n"))

      write.table(print.object$alpha, quote = FALSE, row.names = FALSE, col.names = FALSE)

    }

  }

  #-----------------------------------------
  # Item statistics
  if ("item" %in% print && "itemstat" %in% names(print.object) && nrow(x$result$itemstat) > 2) {

    print.object$item$pNA <- paste0(formatC(print.object$item$pNA, digits = 2, format = "f"), "%")
    print.object$item$m <- formatC(print.object$item$m, digits = 2, format = "f")
    print.object$item$sd <- formatC(print.object$item$sd, digits = 2, format = "f")
    print.object$item$min <- formatC(print.object$item$min, digits = 2, format = "f")
    print.object$item$max <- formatC(print.object$item$max, digits = 2, format = "f")

    print.object$item$it.cor <- formatC(print.object$item$it.cor, digits = digits, format = "f")
    print.object$item$alpha <- formatC(print.object$item$alpha, digits = digits, format = "f")

    print.object$item <- rbind(c("Variable", "n", "nNA", "pNA", "M", "SD", "Min", "Max", "It.Cor", "Alpha"),
                               print.object$item)

    # Format
    print.object$item[, 1] <- format(paste("", print.object$item[, 1]), justify = "left")
    print.object$item[, -1] <- apply(print.object$item[, -1], 2, function(y) format(y, justify = "right"))


    if ("alpha" %in% print) { cat("\n") }

    cat("Item-Total Correlation and Coefficient Alpha if Item Deleted\n\n")

    write.table(print.object$item, quote = FALSE, row.names = FALSE, col.names = FALSE)

  }

}
