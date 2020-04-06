#' Print coef object
#'
#' This function prints the \code{coef} object
#'
#' @param x          \code{coef} object.
#' @param print      a character vector indicating which results to show, i.e. \code{"all"}, for all results
#'                   \code{"alpha"} for the coefficient alpha or \code{omega} for the coefficient omega,
#'                   and \code{"item"} for item statistics.
#' @param digits     an integer value indicating the number of decimal places to be used for displaying
#'                   coefficient alpha or coefficient omega and item-total correlations or standardized
#'                   factor loadings.
#' @param check      logical: if \code{TRUE}, argument specification is checked.
#' @param ...        further arguments passed to or from other methods.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{alpha.coef}}, \code{\link{omega.coef}}
#'
#' @method print coef
#'
#' @export
#'
#' @examples
#' dat <- data.frame(item1 = c(5, 2, 3, 4, 1, 2, 4, 2),
#'                   item2 = c(5, 1, 3, 5, 2, 2, 5, 1),
#'                   item3 = c(4, 2, 4, 5, 1, 3, 5, 1),
#'                   item4 = c(5, 1, 2, 5, 2, 3, 4, 2), stringsAsFactors = FALSE)
#'
#' #--------------------------------------
#' # Coefficient Alpha
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
#'
#' #--------------------------------------
#' # Coefficient Omega
#'
#' # Compute unstandardized coefficient omega and item statistics
#' dat.omega <- omega.coef(dat, output = FALSE)
#'
#' # Print omega.coef object to show unstandardized coefficient omega and item statistics with 3 digits
#' print(dat.omega, digits = 3)
#'
#' # Print omega.coef object to show unstandardized coefficient with 3 digits
#' print(dat.omega, print = "omega", digits = 3)
#'
#' # Print omega.coef object to show item statistics with 3 digits
#' print(dat.omega, print = "item", digits = 3)
print.coef <- function(x, print = x$args$print, digits = x$args$digits, check = TRUE, ...) {

  # Print object
  print.object <- x$result

  ####################################################################################
  # Coefficient Alpha
  if (x$type == "alpha") {

    #----------------------------------------
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
      if (digits %% 1L != 0L || digits < 0L) {

        warning("Specify a positive integer number for the argument 'digits'", call. = FALSE)

      }

    }

    #----------------------------------------
    # Arguments

    # Print coefficient alpha and/or item statistic
    if (length(print) == 1L && "all" %in% print) { print <- c("alpha", "item") }

    #-----------------------------------------------------------------------------------
    # Main Function

    #-----------------------------------------
    # Alpha
    if ("alpha" %in% print) {

      if (all(c("low", "upp") %in% names(print.object$alpha))) {

        print.object$alpha$n <- format(paste("", print.object$alpha$n), justify = "right")

        print.object$alpha$items <- format(print.object$alpha$items, justify = "right")

        print.object$alpha$alpha <- formatC(print.object$alpha$alpha, digits = digits, format = "f")

        print.object$alpha$low <- formatC(print.object$alpha$low, digits = digits, format = "f")
        print.object$alpha$upp <- formatC(print.object$alpha$upp, digits = digits, format = "f")

        print.object$alpha <- rbind(c("n", "Items", "Alpha", "Low", "Upp"), print.object$alpha)

        print.object$alpha <- apply(print.object$alpha, 2, function(y) format(y, justify = "right"))

        cat(paste0(ifelse(isTRUE(x$args$std), "Standardized ", "Unstandardized "), "Coefficient Alpha with ",
                   x$args$conf.level*100L, "% Confidence Interval\n\n"))

        write.table(print.object$alpha, quote = FALSE, row.names = FALSE, col.names = FALSE)

      } else {

        print.object$alpha$alpha <- formatC(print.object$alpha$alpha, digits = digits, format = "f")

        print.object$alpha <- rbind(c(" Items", "Alpha"), print.object$alpha)

        print.object$alpha <- apply(print.object$alpha, 2, function(y) format(y, justify = "right"))

        if (isFALSE(x$args$ordered)) {

          cat(paste0(ifelse(isTRUE(x$args$std), "Standardized ", "Unstandardized "), "Coefficient Alpha\n\n"))

        } else {

          cat("Ordinal Coefficient Alpha\n\n")

        }

        write.table(print.object$alpha, quote = FALSE, row.names = FALSE, col.names = FALSE)

      }

    }

    #-----------------------------------------
    # Item statistics
    if ("item" %in% print && !is.null(print.object$itemstat) && nrow(print.object$itemstat) > 2L) {

      print.object$item$pNA <- paste0(formatC(print.object$item$pNA, digits = 2L, format = "f"), "%")
      print.object$item$m <- formatC(print.object$item$m, digits = 2L, format = "f")
      print.object$item$sd <- formatC(print.object$item$sd, digits = 2L, format = "f")
      print.object$item$min <- formatC(print.object$item$min, digits = 2L, format = "f")
      print.object$item$max <- formatC(print.object$item$max, digits = 2L, format = "f")

      print.object$item$it.cor <- formatC(print.object$item$it.cor, digits = digits, format = "f")
      print.object$item$alpha <- formatC(print.object$item$alpha, digits = digits, format = "f")

      print.object$item <- rbind(c("Variable", "n", "nNA", "pNA", "M", "SD", "Min", "Max", "It.Cor", "Alpha"),
                                 print.object$item)

      # Format
      print.object$item[, 1L] <- format(paste("", print.object$item[, 1]), justify = "left")
      print.object$item[, -1L] <- apply(print.object$item[, -1L], 2, function(y) format(y, justify = "right"))


      if ("alpha" %in% print) { cat("\n") }

      cat("Item-Total Correlation and Coefficient Alpha if Item Deleted\n\n")

      write.table(print.object$item, quote = FALSE, row.names = FALSE, col.names = FALSE)

    }

  ####################################################################################
  # Coefficient Omega
  } else if (x$type == "omega") {

    #----------------------------------------
    # Input Check

    if (isTRUE(check)) {

      #......
      # Check input 'print'
      if (!all(print %in% c("all", "omega", "item"))) {

        stop("Character strings in the argument 'print' do not all match with \"all\", \"omega\", or \"item\".",
             call. = FALSE)

      }

      #......
      # Check input 'digits'
      if (digits %% 1L != 0L || digits < 0L) {

        warning("Specify a positive integer number for the argument 'digits'", call. = FALSE)

      }

    }

    #----------------------------------------
    # Arguments

    # Print coefficient omega and/or item statistic
    if (length(print) == 1L && "all" %in% print) { print <- c("omega", "item") }

    #-----------------------------------------------------------------------------------
    # Main Function

    #-----------------------------------------
    # Omega
    if ("omega" %in% print) {

      print.object$omega$n <- format(paste("", print.object$omega$n), justify = "right")

      print.object$omega$items <- format(print.object$omega$items, justify = "right")

      print.object$omega$omega <- formatC(print.object$omega$omega, digits = digits, format = "f")

      print.object$omega$low <- formatC(print.object$omega$low, digits = digits, format = "f")
      print.object$omega$upp <- formatC(print.object$omega$upp, digits = digits, format = "f")

      print.object$omega <- rbind(c(" n", "Items", "Omega", "Low", "Upp"), print.object$omega)

      print.object$omega <- apply(print.object$omega, 2, function(y) format(y, justify = "right"))

      if (x$args$type == "omega") {

        cat(paste0(ifelse(isTRUE(x$args$std), "Standardized ", "Unstandardized "), "Coefficient Omega with ",
                   x$args$conf.level*100L, "% Confidence Interval\n\n"))

      } else  if (x$args$type == "hierarch") {

        cat(paste0(ifelse(isTRUE(x$args$std), "Standardized ", "Unstandardized "), "Hierarchical Omega with ",
                   x$args$conf.level*100L, "% Confidence Interval\n\n"))

      } else if (x$args$type == "categ") {

        cat(paste0("Categorical Omega with ", x$args$conf.level*100L, "% Confidence Interval\n\n"))

      }

      write.table(print.object$omega, quote = FALSE, row.names = FALSE, col.names = FALSE)

    }

    #-----------------------------------------
    # Item statistics

    if ("item" %in% print) {

      print.object$item$pNA <- paste0(formatC(print.object$item$pNA, digits = 2L, format = "f"), "%")
      print.object$item$m <- formatC(print.object$item$m, digits = 2L, format = "f")
      print.object$item$sd <- formatC(print.object$item$sd, digits = 2L, format = "f")
      print.object$item$min <- formatC(print.object$item$min, digits = 2L, format = "f")
      print.object$item$max <- formatC(print.object$item$max, digits = 2L, format = "f")

      print.object$item$std.loa <- formatC(print.object$item$std.loa, digits = digits, format = "f")
      print.object$item$omega <- formatC(print.object$item$omega, digits = digits, format = "f")

      print.object$item <- rbind(c("Variable", "n", "nNA", "pNA", "M", "SD", "Min", "Max", "Std.Loa", "Omega"),
                                 print.object$item)

      # Format
      print.object$item[, 1L] <- format(paste("", print.object$item[, 1L]), justify = "left")
      print.object$item[, -1L] <- apply(print.object$item[, -1L], 2, function(y) format(y, justify = "right"))

      if ("omega" %in% print) { cat("\n") }

      if (x$args$type == "omega") {

        cat("Standardized Factor Loadings and Coefficient Omega if Item Deleted\n\n")

      } else  if (x$args$type == "hierarch") {

        cat("Standardized Factor Loadings and Hierarchical Omega if Item Deleted\n\n")

      } else if (x$args$type == "categ") {

        cat("Standardized Factor Loadings and Categorical Omega if Item Deleted\n\n")

      }

      write.table(print.object$item, quote = FALSE, row.names = FALSE, col.names = FALSE)

    }

  }

}


