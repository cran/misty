#' Write Results of a misty Object into an Excel file
#'
#' This function writes the results of a misty object (\code{misty.object})
#' into an Excel file.
#'
#' Currently the function supports result objects from the function
#' \code{cor.matrix}, \code{crosstab}, \code{descript}, \code{dominance.manual},
#' \code{dominance}, \code{freq}, \code{item.alpha}, \code{item.cfa}, \code{item.invar},
#' \code{item.omega}, \code{result.lca}, \code{multilevel.cfa}, \code{multilevel.cor},
#' \code{multilevel.descript}, \code{multilevel.fit}, \code{multilevel.invar},
#' \code{multilevel.omega}, \code{na.coverage}, \code{na.descript}, \code{na.pattern},
#' \code{robust.coef}, and \code{std.coef}.
#'
#' @param x          misty object (\code{misty.object}) resulting from a misty
#'                   function supported by the \code{write.result} function (see
#'                   'Details').
#' @param file       a character string naming a file with or without file extension
#'                   '.xlsx', e.g., \code{"Results.xlsx"} or \code{"Results"}.
#' @param tri        a character string or character vector indicating which
#'                   triangular of the matrix to show on the console, i.e.,
#'                   \code{both} for upper and lower triangular, \code{lower}
#'                   for the lower triangular, and \code{upper} for the upper
#'                   triangular.
#' @param digits     an integer value indicating the number of decimal places digits
#'                   to be used for displaying results.
#' @param p.digits   an integer indicating the number of decimal places to be used
#'                   for displaying \emph{p}-values.
#' @param icc.digits an integer indicating the number of decimal places to be used
#'                   for displaying intraclass correlation coefficients
#'                   (\code{multilevel.descript()} and \code{multilevel.icc()}
#'                   function).
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{cor.matrix}}, \code{\link{crosstab}}, \code{\link{freq}},
#' \code{\link{item.alpha}}, \code{\link{item.cfa}}, \code{\link{item.invar}},
#' \code{\link{item.omega}}, \code{\link{result.lca}}, \code{\link{multilevel.cfa}},
#' \code{\link{multilevel.cor}}, \code{\link{multilevel.descript}}, \code{\link{multilevel.fit}},
#' \code{\link{multilevel.invar}}, \code{\link{multilevel.omega}}, \code{\link{na.coverage}},
#' \code{\link{na.descript}}, \code{\link{na.pattern}}, \code{\link{robust.coef}}, \code{\link{std.coef}},
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #--------------------------------------
#' # cor.matrix() function
#'
#' result <- cor.matrix(mtcars, print = "all", output = FALSE)
#' write.result(result, "Correlation.xlsx")
#'
#' #--------------------------------------
#' # crosstab() function
#'
#' result <- crosstab(mtcars[, c("carb", "gear")], print = "all", output = FALSE)
#' write.result(result, "Crosstab.xlsx")
#'
#' #--------------------------------------
#' # descript() function
#'
#' result <- descript(mtcars, output = FALSE)
#' write.result(result, "Descript.xlsx")
#'
#' #--------------------------------------
#' # freq() function
#'
#' result <- freq(mtcars, exclude = 99, output = FALSE)
#' write.result(result, "Freq.xlsx")
#'
#' #--------------------------------------
#' # item.alpha() function
#'
#' result <- item.alpha(attitude, output = FALSE)
#' write.result(result, "Alpha.xlsx")
#'
#' #--------------------------------------
#' # item.cfa() function
#'
#' # Load data set "HolzingerSwineford1939" in the lavaan package
#' data("HolzingerSwineford1939", package = "lavaan")
#'
#' result <- item.cfa(HolzingerSwineford1939[, c("x1", "x2", "x3")],
#'                    output = FALSE)
#' write.result(result, "CFA.xlsx")
#'
#' #--------------------------------------
#' # item.invar() function
#'
#' result <- item.invar(HolzingerSwineford1939, model = c("x1", "x2", "x3", "x4"),
#'                      group = "sex", output = FALSE)
#' write.result(result, "Invariance.xlsx")
#'
#' #--------------------------------------
#' # item.omega() function
#'
#' result <- item.omega(attitude, output = FALSE)
#' write.result(result, "Omega.xlsx")
#'
#' #--------------------------------------
#' # multilevel.cor() function
#'
#' # Load data set "Demo.twolevel" in the lavaan package
#' data("Demo.twolevel", package = "lavaan")
#'
#' result <- multilevel.cor(Demo.twolevel[, c("y1", "y2", "y3")],
#'                          cluster = Demo.twolevel$cluster, output = FALSE)
#' write.result(result, "Multilevel_Correlation.xlsx")
#'
#' #--------------------------------------
#' # multilevel.descript() function
#'
#' # Load data set "Demo.twolevel" in the lavaan package
#' data("Demo.twolevel", package = "lavaan")
#'
#' result <- multilevel.descript(Demo.twolevel[, c("y1", "y2", "y3")],
#'                               cluster = Demo.twolevel$cluster, output = FALSE)
#' write.result(result, "Multilevel_Descript.xlsx")
#' }
write.result <- function(x, file = "Results.xlsx", tri = x$args$tri,
                         digits = x$args$digits, p.digits = x$args$p.digits,
                         icc.digits = x$args$icc.digits, check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'x' is missing
  if (isTRUE(missing(x))) { stop("Please specify a misty object for the argument 'x'.", call. = FALSE) }

  # Check if input 'x' is NULL
  if (isTRUE(is.null(x))) { stop("Input specified for the argument 'x' is NULL.", call. = FALSE) }

  # Check if input 'x' is a misty object
  if (isTRUE(class(x) != "misty.object")) { stop("Please specify a misty object for the argument 'x'.", call. = FALSE) }

  # Check if input 'x' is supported by the function
  if (isTRUE(!x$type %in% c("cor.matrix", "crosstab", "descript", "dominance.manual",
                            "dominance", "freq",  "item.alpha", "item.cfa", "item.invar",
                            "item.omega", "result.lca", "multilevel.cfa", "multilevel.cor",
                            "multilevel.descript", "multilevel.fit", "multilevel.invar",
                            "multilevel.omega", "na.coverage", "na.descript", "na.pattern",
                            "robust.coef", "std.coef"))) {

    stop("This type of misty object is not supported by the function.", call. = FALSE)

  }

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  if (isTRUE(check)) {

    # Check input 'digits'
    if (isTRUE(!is.null(digits))) { if (isTRUE(digits %% 1L != 0L || digits < 0L)) { stop("Specify a positive integer number for the argument 'digits'", call. = FALSE) } }

    # Check input 'p.digits'
    if (isTRUE(!is.null(p.digits))) { if (isTRUE(p.digits %% 1L != 0L || p.digits < 0L)) { stop("Specify a positive integer number for the argument 'p.digits'", call. = FALSE) } }

    # Check input 'icc.digits'
    if (isTRUE(!is.null(icc.digits))) { if (isTRUE(icc.digits %% 1L != 0L || icc.digits < 0L)) { stop("Specify a positive integer number for the argument 'icc.digits'", call. = FALSE) } }

  }

  #_____________________________________________________________________________
  #
  # Data and Arguments ---------------------------------------------------------

  # Write object
  write.object <- x$result

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #_____________________________________________________________________________
  #
  # Correlation Matrix, cor.matrix() -------------------------------------------
    switch(x$type, cor.matrix = {

    # Round
    write.object$cor <- round(write.object$cor, digits = digits)
    write.object$stat <- round(write.object$stat, digits = digits)
    write.object$p <- round(write.object$p, digits = p.digits)

    # Diagonal
    diag(write.object$cor) <- NA
    diag(write.object$n) <- NA
    diag(write.object$stat) <- NA
    diag(write.object$df) <- NA
    diag(write.object$p) <- NA

    # Lower and/or upper triangular
    if (isTRUE(!".group" %in% colnames(x$data))) {

      if (isTRUE(tri == "lower")) {

        write.object$cor[upper.tri(write.object$cor)] <- NA
        write.object$n[upper.tri(write.object$n)] <- NA
        write.object$stat[upper.tri(write.object$stat)] <- NA
        write.object$df[upper.tri(write.object$df)] <- NA
        write.object$p[upper.tri(write.object$p)] <- NA

      }

      if (isTRUE(tri == "upper")) {

        write.object$cor[lower.tri(write.object$cor)] <- NA
        write.object$n[lower.tri(write.object$n)] <- NA
        write.object$stat[lower.tri(write.object$stat)] <- NA
        write.object$df[lower.tri(write.object$df)] <- NA
        write.object$p[lower.tri(write.object$p)] <- NA

      }

    }

    # Add variable names in the rows
    write.object <- lapply(write.object, function(y) data.frame(colnames(y), y,
                                                                row.names = NULL, check.rows = FALSE,
                                                                check.names = FALSE, fix.empty.names = FALSE))

    # Add infos
    write.object$Info <- data.frame(c("Correlation coefficient:", "Missing data:", "Adjustment for multiple testing:"),
                                    c(switch(x$args$method, "pearson" = "Pearson Product-Moment",
                                                            "spearman" = "Spearman's Rank-Order",
                                                            "kendall-b" = "Kendall's Tau-b",
                                                            "kendall-c" = "Kendall-Stuart's Tau-c"),
                                      ifelse(isTRUE(attr(x$data, "missing")),
                                             ifelse(isTRUE(x$args$na.omit), "Listwise deletion", "Pairwise deletion"), "No missing data"),
                                      ifelse(x$args$p.adj == "none", "None", x$args$p.adj)),
                                      row.names = NULL, check.rows = FALSE, check.names = FALSE, fix.empty.names = FALSE)

    if (isTRUE(".group" %in% colnames(x$data))) {

      write.object$Info <- rbind(write.object$Info,
                                 c(paste0("Lower triangular: ", sort(unique(x$data$.group))[1L], ", Upper triangular: ", sort(unique(x$data$.group))[2L]), NA))

    }

    names(write.object) <- c("Cor", "n", "Stat", "df", "p", "Info")

    # Print
    if (isTRUE(!"cor" %in% x$args$print)) { write.object$Cor <- NULL }
    if (isTRUE(!"n" %in% x$args$print)) { write.object$n <- NULL }
    if (isTRUE(!"stat" %in% x$args$print)) { write.object$Stat <- NULL }
    if (isTRUE(!"df" %in% x$args$print)) { write.object$df <- NULL }
    if (isTRUE(!"p" %in% x$args$print)) { write.object$p <- NULL }

  #_____________________________________________________________________________
  #
  # Cross Tabulation, crosstab() -----------------------------------------------

  }, crosstab = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Result table ####

    write.object <- x$result$crosstab

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Round ####

    write.object[, !sapply(write.object, is.character)] <- sapply(write.object[, !sapply(write.object, is.character)], round, digits = digits)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Two-Dimensional Matrix ####

    if (isTRUE(ncol(x$data) == 2L)) {

      #...................
      ### Output table not split ####
      if (!isTRUE(x$args$split)) {

        # Remove duplicated row labels
        write.object[, 1L] <- ifelse(duplicated(write.object[, 1L]), NA, write.object[, 1L])

        #### Frequencies only ####
        if (isTRUE(x$args$print == "no")) {

          write.object <- data.frame(write.object[write.object[, 2L] == "Freq" | is.na(write.object[, 2L]) , 1L],
                                     write.object[write.object[, 2L] == "Freq" | is.na(write.object[, 2L]), -c(1L, 2L)],
                                     row.names = NULL, check.rows = FALSE, check.names = FALSE, fix.empty.names = FALSE)

        #### Frequencies and Percentages ####
        } else {

          # No row-wise percentages
          if (isTRUE(!"row" %in% x$args$print)) { write.object <- write.object[-which(write.object[, 2L] == "Row %"), ] }

          # No col-wise percentages
          if (isTRUE(!"col" %in% x$args$print)) { write.object <- write.object[-which(write.object[, 2L] == "Col %"), ] }

          # No total percentages
          if (isTRUE(!"total" %in% x$args$print)) { write.object <- write.object[-which(write.object[, 2L] == "Tot %"), ] }

        }

        # Add variable names
        names(write.object)[1L:2L] <- colnames(x$data)

      #...................
      ### Output table split ####
      } else {

        #### Absolute Frequencies ####
        write.object.abs <- data.frame(write.object[write.object[, 2L] == "Freq" | is.na(write.object[, 2L]), 1L],
                                       write.object[write.object[, 2L] == "Freq" | is.na(write.object[, 2L]), -c(1L, 2L)],
                                       row.names = NULL, check.rows = FALSE,
                                       check.names = FALSE, fix.empty.names = FALSE)

        write.object.abs <- data.frame(c(colnames(x$data)[1L], rep(NA, times = nrow(write.object.abs) - 1L)),
                                       write.object.abs,
                                       row.names = NULL, check.rows = FALSE, check.names = FALSE, fix.empty.names = FALSE)

        names(write.object.abs)[2L] <- colnames(x$data)[2L]

        #### Row-wise percentages ####
        write.object.row <- data.frame(write.object[which(write.object[, 2L] == "Row %"), 1L],
                                       write.object[which(write.object[, 2L] == "Row %"), -c(1L, 2L)],
                                       row.names = NULL, check.rows = FALSE,
                                       check.names = FALSE, fix.empty.names = FALSE)

        write.object.row <- data.frame(c(colnames(x$data)[1L], rep(NA, times = nrow(write.object.row) - 1L)),
                                       write.object.row,
                                       row.names = NULL, check.rows = FALSE, check.names = FALSE, fix.empty.names = FALSE)

        names(write.object.row)[2L] <- colnames(x$data)[2L]

        #### Column-wise percentages ####
        write.object.col <- data.frame(write.object[which(write.object[, 2L] == "Col %"), 1L],
                                       write.object[which(write.object[, 2L] == "Col %"), -c(1L, 2L)],
                                       row.names = NULL, check.rows = FALSE,
                                       check.names = FALSE, fix.empty.names = FALSE)

        write.object.col <- data.frame(c(colnames(x$data)[1L], rep(NA, times = nrow(write.object.col) - 1L)),
                                       write.object.col,
                                       row.names = NULL, check.rows = FALSE, check.names = FALSE, fix.empty.names = FALSE)

        names(write.object.col)[2L] <- colnames(x$data)[2L]

        #### Total percentages ####
        write.object.tot <- data.frame(write.object[write.object[, 2L] == "Tot %", 1L],
                                       write.object[write.object[, 2L] == "Tot %", -c(1L, 2L)],
                                       row.names = NULL, check.rows = FALSE,
                                       check.names = FALSE, fix.empty.names = FALSE)

        write.object.tot <- data.frame(c(colnames(x$data)[1L], rep(NA, times = nrow(write.object.tot) - 1L)),
                                       write.object.tot,
                                       row.names = NULL, check.rows = FALSE, check.names = FALSE, fix.empty.names = FALSE)

        names(write.object.tot)[2L] <- colnames(x$data)[2L]

        #### Prepare list ####
        write.object <- list()

        if (isTRUE(x$args$freq)) { write.object$"Freq" <- write.object.abs }

        if (isTRUE("row" %in% x$args$print)) { write.object$"Row%" <- write.object.row }

        if (isTRUE("col" %in% x$args$print)) { write.object$"Col%" <- write.object.col }

        if (isTRUE("total" %in% x$args$print)) { write.object$"Total%" <- write.object.tot }

      }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Three-Dimensional Matrix ####
    } else if (isTRUE(ncol(x$data) == 3L)) {

      #...................
      ### Output table not split ####
      if (!isTRUE(x$args$split)) {

        # Remove duplicated row labels
        duplic <- apply(write.object[, c(1L:2L)], 1L, paste, collapse = "")

        write.object[, 1L] <- ifelse(duplicated(duplic), NA, write.object[, 1L])
        write.object[, 2L] <- ifelse(duplicated(duplic), NA, write.object[, 2L])

        write.object[, 1L] <- ifelse(duplicated(write.object[, 1L]), NA, write.object[, 1L])

        #### Frequencies only ####
        if (isTRUE(x$args$print == "no")) {

          write.object <- data.frame(write.object[write.object[, 3L] == "Freq" | is.na(write.object[, 3L]), 1L],
                                     write.object[write.object[, 3L] == "Freq" | is.na(write.object[, 3L]), -c(1L, 3L)],
                                     row.names = NULL, check.rows = FALSE, check.names = FALSE, fix.empty.names = FALSE)

          # Add variable names
          write.object <- data.frame(c(colnames(x$data)[1L], rep(NA, times = nrow(write.object) - 1L)),
                                     write.object,
                                     row.names = NULL, check.rows = FALSE, check.names = FALSE, fix.empty.names = FALSE)

          names(write.object)[c(2L, 3L)] <- colnames(x$data)[c(2L, 3L)]

        #### Frequencies and Percentages ####
        } else {

          # No row-wise percentages
          if (isTRUE(!"row" %in% x$args$print)) { write.object <- write.object[-which(write.object[, 3L] == "Row %"), ] }

          # No col-wise percentages
          if (isTRUE(!"col" %in% x$args$print)) { write.object <- write.object[-which(write.object[, 3L] == "Col %"), ] }

          # No total percentages
          if (isTRUE(!"total" %in% x$args$print)) { write.object <- write.object[-which(write.object[, 3L] == "Tot %"), ] }

          # Add variable names
          names(write.object)[c(1L, 2L, 3L)] <- colnames(x$data)

        }

      #...................
      ### Output table split ####
      } else {

        #### Absolute Frequencies ####
        write.object.abs <- data.frame(write.object[write.object[, 3L] == "Freq" | is.na(write.object[, 3L]), 1L],
                                       write.object[write.object[, 3L] == "Freq" | is.na(write.object[, 3L]), -c(1L, 3L)],
                                       row.names = NULL, check.rows = FALSE,
                                       check.names = FALSE, fix.empty.names = FALSE)

        # Remove duplicated row labels
        write.object.abs[, 1L] <- ifelse(duplicated(write.object.abs[, 1L]), NA, write.object.abs[, 1L])

        # Add variable names
        write.object.abs <- data.frame(c(colnames(x$data)[1L], rep(NA, times = nrow(write.object.abs) - 1L)),
                                       write.object.abs,
                                       row.names = NULL, check.rows = FALSE, check.names = FALSE, fix.empty.names = FALSE)

        names(write.object.abs)[c(2L, 3L)] <- colnames(x$data)[c(2L, 3L)]

        #### Row-wise percentages ####
        write.object.row <- data.frame(write.object[which(write.object[, 3L] == "Row %"), 1L],
                                       write.object[which(write.object[, 3L] == "Row %"), -c(1L, 3L)],
                                       row.names = NULL, check.rows = FALSE,
                                       check.names = FALSE, fix.empty.names = FALSE)

        # Remove duplicated row labels
        write.object.row[, 1L] <- ifelse(duplicated(write.object.row[, 1L]), NA, write.object.row[, 1L])

        # Add variable names
        write.object.row <- data.frame(c(colnames(x$data)[1L], rep(NA, times = nrow(write.object.row) - 1L)),
                                       write.object.row,
                                       row.names = NULL, check.rows = FALSE, check.names = FALSE, fix.empty.names = FALSE)

        names(write.object.row)[c(2L, 3L)] <- colnames(x$data)[c(2L, 3L)]


        #### Column-wise percentages ####
        write.object.col <- data.frame(write.object[which(write.object[, 3L] == "Col %"), 1L],
                                       write.object[which(write.object[, 3L] == "Col %"), -c(1L, 3L)],
                                       row.names = NULL, check.rows = FALSE,
                                       check.names = FALSE, fix.empty.names = FALSE)

        # Remove duplicated row labels
        write.object.col[, 1L] <- ifelse(duplicated(write.object.col[, 1L]), NA, write.object.col[, 1L])

        # Add variable names
        write.object.col <- data.frame(c(colnames(x$data)[1L], rep(NA, times = nrow(write.object.col) - 1L)),
                                       write.object.col,
                                       row.names = NULL, check.rows = FALSE, check.names = FALSE, fix.empty.names = FALSE)

        names(write.object.col)[c(2L, 3L)] <- colnames(x$data)[c(2L, 3L)]

        #### Total percentages ####
        write.object.tot <- data.frame(write.object[write.object[, 3L] == "Tot %", 1L],
                                       write.object[write.object[, 3L] == "Tot %", -c(1L, 3L)],
                                       row.names = NULL, check.rows = FALSE,
                                       check.names = FALSE, fix.empty.names = FALSE)

        # Remove duplicated row labels
        write.object.tot[, 1L] <- ifelse(duplicated(write.object.tot[, 1L]), NA, write.object.tot[, 1L])

        # Add variable write.object.tot
        write.object.tot <- data.frame(c(colnames(x$data)[1L], rep(NA, times = nrow(write.object.col) - 1L)),
                                       write.object.tot,
                                       row.names = NULL, check.rows = FALSE, check.names = FALSE, fix.empty.names = FALSE)

        names(write.object.tot)[c(2L, 3L)] <- colnames(x$data)[c(2L, 3L)]

        #### Prepare list ####
        write.object <- list()

        if (isTRUE(x$args$freq)) { write.object$"Freq" <- write.object.abs }

        if (isTRUE("row" %in% x$args$print)) { write.object$"Row%" <- write.object.row }

        if (isTRUE("col" %in% x$args$print)) { write.object$"Col%" <- write.object.col }

        if (isTRUE("total" %in% x$args$print)) { write.object$"Total%" <- write.object.tot }

      }

    }
  #_____________________________________________________________________________
  #
  # Descriptive Statistics, descript() -----------------------------------------

  }, descript = {

    # Variables to round
    write.round <- c("pNA", "m", "se.m", "var", "sd", "min", "p25", "med", "p75", "max", "range", "iqr", "skew", "kurt")

    #...................
    ### No Grouping, No Split ####
    if (isTRUE(is.null(x$data$group) && is.null(x$data$split))) {

      # Round
      write.object[, write.round] <- sapply(write.round, function(y) ifelse(!is.na(write.object[, y]), round(write.object[, y], digits = digits), NA))

      #...............
      # Select statistical measures

      print <- match(x$args$print, names(write.object))

      # Variable names
      names(write.object) <- c("Variable", "n", "nNA", "pNA", "M", "SE.M", "Var", "SD", "Min", "p25", "Med", "p75", "Max", "Range", "IQR", "Skew", "Kurt")

      # One variable
      if (isTRUE(ncol(x$data$x) == 1L)) {

        # Select statistical measures
        write.object <- write.object[, print]

      # More than one variable
      } else {

        # Select statistical measures
        write.object <- write.object[, c(1L, print)]

      }

    #...................
    ### Grouping, No Split ####
    } else if (isTRUE(!is.null(x$data$group) && is.null(x$data$split))) {

      # Round
      write.object[, write.round] <- sapply(write.round, function(y) ifelse(!is.na(write.object[, y]), round(write.object[, y], digits = digits), NA))

      #...............
      # Select statistical measures

      print <- match(x$args$print, names(write.object))

      # Variable names
      names(write.object) <- c("Group", "Variable", "n", "nNA", "pNA", "M", "SE.M", "Var", "SD", "Min", "p25", "Med", "p75", "Max", "Range", "IQR", "Skew", "Kurt")

      # One variable
      if (isTRUE(ncol(x$data$x) == 1L)) {

        # Select statistical measures
        write.object <- write.object[, c(1L, print)]

      # More than one variable
      } else {

        # Select statistical measures
        write.object <- write.object[, c(1L, 2L, print)]

      }

      # Convert to numeric
      write.object$Group <- ifelse(grepl("(^(-|\\+)?((\\.?\\d+)|(\\d+\\.\\d+)|(\\d+\\.?))$)|(^(-|\\+)?((\\.?\\d+)|(\\d+\\.\\d+)|(\\d+\\.?))e(-|\\+)?(\\d+)$)",
                                         x = write.object$Group), as.numeric(write.object$Group), write.object$Group)

    #...................
    ### Split, without or with Grouping ####
    } else if (isTRUE(!is.null(x$data$split))) {

      # Round
      for (i in names(write.object)) {

        write.object[[i]][, write.round] <- sapply(write.round, function(y) ifelse(!is.na(write.object[[i]][, y]), round(write.object[[i]][, y], digits = digits), NA))

      }

      #......
      # No grouping
      if (isTRUE(is.null(x$data$group))) {

        #...............
        # Select statistical measures

        print <- match(x$args$print, names(write.object[[1]]))

        # Variable names
        write.object <- lapply(write.object, function(y) misty::df.rename(y, from = names(y), to = c("Variable", "n", "nNA", "pNA", "M", "SE.M", "Var", "SD", "Min", "p25", "Med", "p75", "Max", "Range", "IQR", "Skew", "Kurt")))

        # One variable
        if (isTRUE(ncol(x$data$x) == 1L)) {

          # Select statistical measures
          write.object <- lapply(write.object, function(y) y[, ])

        # More than one variable
        } else {

          # Select statistical measures
          write.object <- lapply(write.object, function(y) y[, c(1, print)])

        }

      #......
      # Grouping
      } else {

        #...............
        # Select statistical measures

        print <- match(x$args$print, names(write.object[[1]]))

        # Variable names
        write.object <- lapply(write.object, function(y) misty::df.rename(y, from = names(y), to = c("Group", "Variable", "n", "nNA", "pNA", "M", "SE.M", "Var", "SD", "Min", "p25", "Med", "p75", "Max", "Range", "IQR", "Skew", "Kurt")))

        # One variable
        if (isTRUE(ncol(x$data$x) == 1L)) {

          # Select statistical measures
          write.object <- lapply(write.object, function(y) y[, c(1, print)])

        # More than one variable
        } else {

          # Select statistical measures
          write.object <- lapply(write.object, function(y) y[, c(1, 2, print)])

        }

        # Convert to numeric
        write.object <- lapply(write.object, function(y) within(y, assign("Group", ifelse(grepl("(^(-|\\+)?((\\.?\\d+)|(\\d+\\.\\d+)|(\\d+\\.?))$)|(^(-|\\+)?((\\.?\\d+)|(\\d+\\.\\d+)|(\\d+\\.?))e(-|\\+)?(\\d+)$)",
                                                                          x = y$Group), as.numeric(y$Group), y$Group))))

      }

    }

  #_____________________________________________________________________________
  #
  # Dominance Analysis, Manual, dominance.manual() -----------------------------

  }, dominance.manual = {

    # Extract result table
    write.gen <- write.object

    #...................
    ### Round ####

    write.gen[, "r2"] <- round(write.gen[, "r2"], digits = digits)
    write.gen[, "perc"] <- round(write.gen[, "perc"], digits = digits - 1L)

    #...................
    ### Variable names ####

    write.gen <- data.frame(Variable = rownames(write.gen), write.gen)

    #...................
    ### Write object ####

    write.object <- list(general = write.gen)

  #_____________________________________________________________________________
  #
  # Dominance Analysis, dominance() --------------------------------------------

  }, dominance = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## General Dominance ####

    print.gen <- NULL
    if (isTRUE("gen" %in% x$args$print)) {

      # Extract result table
      write.gen <- write.object$gen

      #...................
      ### Round ####

      write.gen[, "r2"] <- round(write.gen[, "r2"], digits = digits)
      write.gen[, "perc"] <- round(write.gen[, "perc"], digits = digits - 1L)

      #...................
      ### Variable names ####

      write.gen <- data.frame(Variable = rownames(write.gen), write.gen)

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Conditional Dominance ####

    write.cond <- NULL
    if (isTRUE("cond" %in% x$args$print)) {

      # Extract result table
      write.cond <- write.object$cond

      #...................
      ### Variable names ####

      write.cond <- data.frame(Variable = rownames(write.cond), write.cond)

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Complete Dominance ####

    write.comp <- NULL
    if (isTRUE("cond" %in% x$args$print)) {

      # Extract result table
      write.comp <- write.object$comp

      #...................
      ### Variable names ####

      write.comp <- data.frame(Variable = rownames(write.comp), write.comp)

    }

    #...................
    ### Write object ####

    write.object <- list(general = write.gen, conditional = write.cond, complete = write.comp)

    write.object <- write.object[unlist(lapply(write.object, function(y) !is.null(y)))]

  #_____________________________________________________________________________
  #
  # Frequency Table, freq() ----------------------------------------------------

  }, freq = {

    #...................
    ### One variable ####
    if (isTRUE(ncol(x$data) == 1L)) {

      #......................
      # Values shown in columns, variables in the rows
      if (isTRUE(x$args$val.col)) {

        # Complete data
        if (isTRUE(all(!is.na(x$data)))) {

          write.object <- data.frame(Value = c("Freq", "Perc"),
                                     write.object[-nrow(write.object), -ncol(write.object)],
                                     Total = rowSums(write.object[-nrow(write.object), -ncol(write.object)]),
                                     Missing = write.object[-nrow(write.object), ncol(write.object)],
                                     fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

        # Missing data
        } else {

          write.object <- data.frame(Value = c("Freq", "Perc", "Valid Perc"),
                                     write.object[, -ncol(write.object)],
                                     Total = rowSums(write.object[, -ncol(write.object)]),
                                     Missing = write.object[, ncol(write.object)],
                                     Total = rowSums(write.object),
                                     fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

        }

      #......................
      # Values shown in rows, variables in the columns
      } else {

        # Complete data
        if (isTRUE(all(!is.na(x$data)))) {

          write.object <- data.frame(c("Value", rep("", times = nrow(write.object) - 2L), "Total", "Missing"),
                                     c(write.object[, "Value"], NA),
                                     Freq = c(write.object[1:nrow(write.object) - 1L, "Freq"],
                                              sum(write.object[1:nrow(write.object) - 1L, "Freq"]),
                                              write.object[nrow(write.object), "Freq"]),
                                     Perc = c(write.object[1:nrow(write.object) - 1L, "Perc"],
                                              sum(write.object[1:nrow(write.object) - 1L, "Perc"]),
                                              write.object[nrow(write.object), "Perc"]),
                                     fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

          colnames(write.object) <- c("", "", "Freq", "Perc")

        # Missing data
        } else {

          write.object <- data.frame(c("Value", rep("", times = nrow(write.object) - 2L), "Total", "Missing", "Total"),
                                     c(write.object[, "Value"], NA, NA),
                                     Freq = c(write.object[1:nrow(write.object) - 1L, "Freq"],
                                              sum(write.object[1:nrow(write.object) - 1L, "Freq"]),
                                              write.object[nrow(write.object), "Freq"],
                                              sum(write.object[, "Freq"])),
                                     Perc = c(write.object[1:nrow(write.object) - 1L, "Perc"],
                                              sum(write.object[1:nrow(write.object) - 1L, "Perc"]),
                                              write.object[nrow(write.object), "Perc"],
                                              sum(write.object[, "Perc"])),
                                     V.Perc = c(write.object[1:nrow(write.object) - 1L, "V.Perc"],
                                                sum(write.object[1:nrow(write.object) - 1L, "V.Perc"]), NA, NA),
                                     fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

          colnames(write.object) <- c("", "", "Freq", "Perc", "Valid Perc")

        }

      }

      # Round digits
      write.object[, !sapply(write.object, is.character)] <- sapply(write.object[, !sapply(write.object, is.character)], round, digits = digits)

    #...................
    ### More than one variable ####
    } else {

      #......................
      # Variables split to multiple Excel sheets
      if (isTRUE(x$args$split)) {

        write.object <- lapply(write.object, function(y) {

          #......................
          # Values shown in columns, variables in the rows
          if (isTRUE(x$args$val.col)) {

            # Complete data
            if (isTRUE(y[1, ncol(y)] == 0)) {

              data.frame(Value = c("Freq", "Perc"),
                         y[-nrow(y), -ncol(y)], Total = rowSums(y[-nrow(y), -ncol(y)]),
                         Missing = y[-nrow(y), ncol(y)],
                         fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

            } else {

              data.frame(Value = c("Freq", "Perc", "Valid Perc"),
                         y[, -ncol(y)],
                         Total = rowSums(y[, -ncol(y)]),
                         Missing = y[, ncol(y)],
                         Total = rowSums(y),
                         fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

            }

          #......................
          # Values shown in rows, variables in the columns
          } else {

            # Complete data
            if (isTRUE(y[nrow(y), "Freq"] == 0L)) {

              data.frame(c("Value", rep("", times = nrow(y) - 2L), "Total", "Missing"),
                         c(y[, "Value"], NA),
                         Freq = c(y[1:nrow(y) - 1L, "Freq"], sum(y[1:nrow(y) - 1L, "Freq"]), y[nrow(y), "Freq"]),
                         Perc = c(y[1:nrow(y) - 1L, "Perc"], sum(y[1:nrow(y) - 1L, "Perc"]), y[nrow(y), "Perc"]),
                         fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

            } else {

              data.frame(c("Value", rep("", times = nrow(y) - 2L), "Total", "Missing", "Total"),
                         c(y[, "Value"], NA, NA),
                         Freq = c(y[1:nrow(y) - 1L, "Freq"], sum(y[1:nrow(y) - 1L, "Freq"]),
                                  y[nrow(y), "Freq"],
                                  sum(y[, "Freq"])),
                         Perc = c(y[1:nrow(y) - 1L, "Perc"], sum(y[1:nrow(y) - 1L, "Perc"]), y[nrow(y), "Perc"], sum(y[, "Perc"])),
                         V.Perc = c(y[1:nrow(y) - 1L, "V.Perc"], sum(y[1:nrow(y) - 1L, "V.Perc"]), NA, NA),
                         fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

            }

          }

        })

      #......................
      # Variables not split to multiple Excel sheets
      } else {

        #......................
        # Values shown in columns, variables in the rows
        if (isTRUE(x$args$val.col)) {

          # Complete data
          if (isTRUE(all(!is.na(x$data)))) {

            write.object$freq <- data.frame(write.object$freq[, "Var"],
                                            write.object$freq[, -c(1, ncol(write.object$freq))],
                                            Total = rowSums(write.object$freq[, -c(1L, ncol(write.object$freq))]),
                                            Missing = write.object$freq[, ncol(write.object$freq)],
                                            fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

            write.object$perc <- data.frame(write.object$perc[, "Var"],
                                            write.object$perc[, -c(1L, ncol(write.object$perc))],
                                            Total = rowSums(write.object$perc[, -c(1L, ncol(write.object$perc))]),
                                            Missing = write.object$perc[, ncol(write.object$perc)],
                                            fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

            write.object$v.perc <- NULL
            names(write.object) <- c("Freq", "Perc")

          # Missing data
          } else {

            write.object$freq <- data.frame(write.object$freq[, "Var"],
                                            write.object$freq[, -c(1L, ncol(write.object$freq))],
                                            Total = rowSums(write.object$freq[, -c(1L, ncol(write.object$freq))]),
                                            Missing = write.object$freq[, ncol(write.object$freq)],
                                            Total = rowSums(write.object$freq[, -1L]),
                                            fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

            write.object$perc <- data.frame(write.object$perc[, "Var"],
                                            write.object$perc[, -c(1L, ncol(write.object$perc))],
                                            Total = rowSums(write.object$perc[, -c(1L, ncol(write.object$perc))]),
                                            Missing = write.object$perc[, ncol(write.object$perc)],
                                            Total = rowSums(write.object$perc[, -1L]),
                                            fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

            write.object$v.perc <- data.frame(write.object$v.perc[, "Var"],
                                              write.object$v.perc[, -c(1L, ncol(write.object$v.perc))],
                                              Total = rowSums(write.object$v.perc[, -c(1L, ncol(write.object$v.perc))]),
                                              Missing = write.object$v.perc[, ncol(write.object$v.perc)],
                                              Total = rowSums(write.object$v.perc[, -1L]),
                                              fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

            names(write.object) <- c("Freq", "Perc", "Valid Perc")

          }

        #......................
        # Values shown in rows, variables in the columns
        } else {

          # Complete data
          if (isTRUE(all(!is.na(x$data)))) {

            write.object$freq <- data.frame(c("Value", rep("", times = nrow(write.object$freq) - 2), "Total", "Missing"),
                                            c(write.object$freq[, "Value"], NA),
                                            rbind(write.object$freq[1:nrow(write.object$freq) - 1, -1],
                                                  colSums(write.object$freq[1:nrow(write.object$freq) - 1, -1]),
                                                  write.object$freq[nrow(write.object$freq), -1]),
                                            fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

            write.object$perc <- data.frame(c("Value", rep("", times = nrow(write.object$perc) - 2), "Total", "Missing"),
                                            c(write.object$perc[, "Value"], NA),
                                            rbind(write.object$perc[1:nrow(write.object$perc) - 1, -1],
                                                  colSums(write.object$perc[1:nrow(write.object$perc) - 1, -1]),
                                                  write.object$perc[nrow(write.object$perc), -1]),
                                            fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

            write.object$v.perc <- NULL
            names(write.object) <- c("Freq", "Perc")

          # Missing data
          } else {

            write.object$freq <- data.frame(c("Value", rep("", times = nrow(write.object$freq) - 2), "Total", "Missing", "Total"),
                                            c(write.object$freq[, "Value"], NA, NA),
                                            rbind(write.object$freq[1:nrow(write.object$freq) - 1, -1],
                                                  colSums(write.object$freq[1:nrow(write.object$freq) - 1, -1]),
                                                  write.object$freq[nrow(write.object$freq), -1], colSums(write.object$freq[, -1])),
                                            fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

            write.object$perc <- data.frame(c("Value", rep("", times = nrow(write.object$perc) - 2), "Total", "Missing", "Total"),
                                            c(write.object$perc[, "Value"], NA, NA),
                                            rbind(write.object$perc[1:nrow(write.object$perc) - 1, -1],
                                                  colSums(write.object$perc[1:nrow(write.object$perc) - 1, -1]),
                                                  write.object$perc[nrow(write.object$perc), -1], colSums(write.object$perc[, -1])),
                                            fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

            write.object$v.perc <- data.frame(c("Value", rep("", times = nrow(write.object$v.perc) - 1), "Total"),
                                              c(write.object$v.perc[, "Value"], NA),
                                              rbind(write.object$v.perc[1:nrow(write.object$v.perc), -1],
                                                    colSums(write.object$v.perc[1:nrow(write.object$v.perc), -1])),
                                              fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

            names(write.object) <- c("Freq", "Perc", "Valid Perc")

          }

        }

      }

      # Round
      for (i in names(write.object)) {

        write.object[[i]][, !sapply(write.object[[i]], is.character)] <- sapply(write.object[[i]][, !sapply(write.object[[i]], is.character)], round, digits = digits)

      }

    }

    # Print
    if (isTRUE(x$args == "no")) {

      write.object$Perc <- NULL
      write.object$`Valid Perc` <- NULL

    } else {

      if (isTRUE(!"perc" %in% x$args$print)) { write.object$Perc <- NULL }
      if (isTRUE(!"v.perc" %in% x$args$print)) { write.object$`Valid Perc` <- NULL }

    }

  #_____________________________________________________________________________
  #
  # Coefficient Alpha and Item Statistics, item.alpha() ------------------------

  }, item.alpha = {

    if (is.null(write.object$itemstat)) {

      write.object <- write.object$alpha
      names(write.object) <- c("Items", "Alpha")

      write.object$Alpha <- round(write.object$Alpha, digits = digits)

    } else {

      names(write.object)  <- c("Alpha", "Itemstat")

      names(write.object$Alpha) <- c("n", "Items", "Alpha", "Low", "Upp")
      names(write.object$Itemstat) <- c("Variable", "n", "nNA", "pNA", "M", "SD", "Min", "Max", "It.Cor", "Alpha")

      write.object$Alpha <- round(write.object$Alpha, digits = digits)
      write.object$Itemstat[, -1L] <- round(write.object$Itemstat[, -1L], digits = digits)

    }

    # Print
    if (isTRUE(!"alpha" %in% x$args$print)) { write.object$Alpha <- NULL }
    if (isTRUE(!"item" %in% x$args$print)) { write.object$Itemstat <- NULL }

  #_____________________________________________________________________________
  #
  # Confirmatory Factor Analysis, item.cfa() -----------------------------------

  }, item.cfa = {

    #...................
    ### lavaan summary ####

    # Column names
    colnames(write.object$summary) <- c(write.object$summary[1, 1], "", "")

    summary <- write.object$summary[-1, ]

    #...................
    ### Covariance coverage ####

    # Round
    write.object$coverage <- sapply(data.frame(write.object$coverage), round, digits = digits)

    # Add variable names in the rows
    coverage <- data.frame(colnames(write.object$coverage), write.object$coverage,
                           row.names = NULL, check.rows = FALSE,
                           check.names = FALSE, fix.empty.names = FALSE)

    #...................
    ### Univariate Sample Statistics ####

    itemstat <- write.object$descript

    # Round
    itemstat[, -1L] <- sapply(itemstat[, -1L], round, digits = digits)

    colnames(itemstat) <- c("Variable", "n", "nNA", "pNA", "M", "SD", "Min", "Max", "Skew", "Kurt")

    #...................
    ### Univariate Counts for Ordered Variables ####

    itemfreq <- write.object$itemfreq$freq

    colnames(itemfreq)[1] <- "Variable"

    #...................
    ### Model fit ####

    fit <- write.object$fit

    # Round
    fit[, -1L] <- sapply(fit[, -1L], round, digits = digits)

    #...................
    ### Parameter estimates ####

    param <- write.object$param[, -c(2L, 3L)]

    # Round
    param[, -c(1L, 2L, 6L)] <- sapply(param[, -c(1L, 2L, 6L)], round, digits = digits)
    param[, 6L] <- sapply(param[, 6L], round, digits = p.digits)

    colnames(param) <- c("Parameter", "Variable", "Estimate", "SE", "z", "pvalue", "StdYX")

    #...................
    ### Modification indices ####

    if (isTRUE(x$args$estimator != "PML")) {

      modind <- write.object$modind

      # Round
      modind[, -c(1L, 2L, 3L)] <- sapply(modind[, -c(1L, 2L, 3L)], round, digits = digits)

      colnames(modind) <- c("lhs", "op", "rhs", "MI", "EPC", "STDYX EPC")

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Residual Correlation Matrix ####

    resid <- NULL

    if (isTRUE("resid" %in% x$args$print && !is.null(write.object$resid))) {

      # Extract result table
      resid <- write.object$resid

      # Row names
      resid <- data.frame(row.names(resid), resid, row.names = NULL, fix.empty.names = FALSE)

      # Round
      resid[, -1L] <- sapply(resid[, -1L], round, digits = p.digits)

    }

    #...................
    ### Write object ####

    write.object <- list(summary = summary, coverage = coverage, itemstat = itemstat,
                         itemfreq = itemfreq, fit = fit, param = param, modind = modind,
                         resid = resid)

    # Print
    if (isTRUE(!"summary" %in% x$args$print)) { write.object$summary <- NULL }
    if (isTRUE(!"coverage" %in% x$args$print)) { write.object$coverage <- NULL }
    if (isTRUE(!"descript" %in% x$args$print)) { write.object$itemstat <- NULL; write.object$itemfreq <- NULL }
    if (isTRUE(!"fit" %in% x$args$print)) { write.object$fit <- NULL }
    if (isTRUE(!"est" %in% x$args$print)) { write.object$param <- NULL }
    if (isTRUE(!"modind" %in% x$args$print)) { write.object$modind <- NULL }
    if (isTRUE(!"resid" %in% x$args$print)) { write.object$resid <- NULL }

  #_____________________________________________________________________________
  #
  # Measurement Invariance Evaluation, item.invar() ----------------------------
  }, item.invar = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## lavaan summary ####

    # Extract result table
    summary <- write.object$summary

    # Column names
    colnames(summary) <- c(summary[1L, 1L], rep("", times = ncol(summary) - 1L))

    # Remove first row
    summary <- summary[-1, ]

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Covariance coverage ####

    coverage <- NULL

    if (isTRUE("coverage" %in% x$args$print)) {

      # Extract result table
      coverage <- write.object$coverage

      # Between-group measurement invariance
      if (isTRUE(!x$args$long)) {

        # Combine data frames and round
        coverage <- data.frame(group = rep(names(coverage), each = nrow(coverage[[1L]])),
                               colnames(coverage[[1L]]),
                               apply(do.call("rbind", coverage), 2L, round, digits = p.digits),
                               row.names = NULL, fix.empty.names = FALSE)

      # Longitudinal measurement invariance
      } else {

        # Combine data frames and round
        coverage <- data.frame(colnames(coverage), coverage,
                               row.names = NULL, fix.empty.names = FALSE)

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Univariate Sample Statistics ####

    itemstat <- NULL

    if (isTRUE("descript" %in% x$args$print)) {

      # Extract result table
      itemstat <- write.object$descript

      # Round
      itemstat[, c("m", "sd", "min", "max", "skew", "kurt")] <- sapply(itemstat[, c("m", "sd", "min", "max", "skew", "kurt")], round, digits = digits)
      itemstat[, "pNA"] <- round(itemstat[, "pNA"], digits = digits - 1L)

      # Column names
      colnames(itemstat) <- c(if (isTRUE(!x$args$long)) { "Group" }, "Variable", "n", "nNA", "pNA", "M", "SD", "Min", "Max", "Skew", "Kurt")

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Model fit ####

    # Extract result table
    fit <- write.object$fit

    # Remove NULL entries
    fit <- fit[!sapply(fit, is.null)]

    #### Standard fit indices
    if (isTRUE(x$args$estimator %in% c("ML", "MLF", "GLS", "WLS", "DWLS", "ULS", "PML"))) {

      # Combine data frames
      fit <- data.frame(c("Standard", rep(NA, times = nrow(fit$stand))),
                        do.call("rbind", lapply(fit, function(y) rbind(NA, y))),
                        row.names = NULL, fix.empty.names = FALSE)

    #### Standard, scaled, and robust fit indices
    } else {

      # Combine data frames
      fit <- data.frame(c("Standard", rep(NA, times = nrow(fit$stand)), "Scaled", rep(NA, times = nrow(fit$scaled)), "Robust", rep(NA, times = nrow(fit$robust))),
                        do.call("rbind", lapply(fit, function(y) rbind(NA, y))),
                        row.names = NULL, fix.empty.names = FALSE)

    }

    # Round
    fit[which(!fit[, 2L] %in% c("P-value", "P-value RMSEA <= 0.05")), c(3L:ncol(fit))] <- sapply(fit[which(!fit[, 2L] %in% c("P-value", "P-value RMSEA <= 0.05")), c(3L:ncol(fit))], round, digits = digits)
    fit[which(fit[, 2L] %in% c("P-value", "P-value RMSEA <= 0.05")), c(3L:ncol(fit))] <- sapply(fit[which(fit[, 2L] %in% c("P-value", "P-value RMSEA <= 0.05")), c(3L:ncol(fit))], round, digits = p.digits)

    # Column names
    switch(x$args$invar,
           config = { colnames(fit) <- c("", "", "Config") },
           metric = { colnames(fit) <- c("", "", "Config", "Metric", "dMetric") },
           scalar = { colnames(fit) <- c("", "", "Config", "Metric", "Scalar", "dMetric", "dScalar") },
           strict = { colnames(fit) <- c("", "", "Config", "Metric", "Scalar", "Stict", "dMetric", "dScalar", "dStrict") })

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Parameter estimates ####

    # Extract result table
    param <- write.object$param

    # Remove NULL entries
    param <- param[!sapply(param, is.null)]

    # Combine data frames
    param <- data.frame(switch(x$args$invar,
                               config = { c("Config", rep(NA, times = nrow(param$config))) },
                               metric = { c("Config", rep(NA, times = nrow(param$config)), "Metric", rep(NA, times = nrow(param$metric))) },
                               scalar = { c("Config", rep(NA, times = nrow(param$config)), "Metric", rep(NA, times = nrow(param$metric)), "Scalar", rep(NA, times = nrow(param$scalar))) },
                               strict = { c("Config", rep(NA, times = nrow(param$config)), "Metric", rep(NA, times = nrow(param$metric)), "Scalar", rep(NA, times = nrow(param$scalar)), "Stict", rep(NA, times = nrow(param$strict))) }),
                        do.call("rbind", lapply(param, function(y) rbind(NA, y))),
                        row.names = NULL, fix.empty.names = FALSE)

    # Round
    param[, c("est", "se", "z", "stdyx")] <- sapply(param[, c("est", "se", "z", "stdyx")], round, digits = digits)
    param[, "pvalue"] <- round(param[, "pvalue"], digits = p.digits)

    # Column names
    colnames(param) <- c("", "Parameter", if (isTRUE(!x$args$long)) { "Group" }, "lhs", "op", "rhs", "label", "Estimate", "SE", "z", "pvalue", "StdYX")

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Modification indices ####

    modind <- NULL

    if (isTRUE("modind" %in% x$args$print && any(!sapply(write.object$modind, is.null)))) {

      # Extract result table
      modind <- write.object$modind

      # Remove NULL entries
      modind <- modind[!sapply(modind, is.null)]

      # Combine data frames
      modind <- data.frame(switch(x$args$invar,
                                  config = {   if (is.null(modind$config)) { NULL } else { c("Config", rep(NA, times = nrow(modind$config))) } },
                                  metric = { c(if (is.null(modind$config)) { NULL } else { c("Config", rep(NA, times = nrow(modind$config))) },
                                               if (is.null(modind$metric)) { NULL } else { c("Metric", rep(NA, times = nrow(modind$metric))) }) },
                                  scalar = { c(if (is.null(modind$config)) { NULL } else { c("Config", rep(NA, times = nrow(modind$config))) },
                                               if (is.null(modind$metric)) { NULL } else { c("Metric", rep(NA, times = nrow(modind$metric))) },
                                               if (is.null(modind$scalar)) { NULL } else { c("Scalar", rep(NA, times = nrow(modind$scalar))) }) },
                                  strict = { c(if (is.null(modind$config)) { NULL } else { c("Config", rep(NA, times = nrow(modind$config))) },
                                               if (is.null(modind$metric)) { NULL } else { c("Metric", rep(NA, times = nrow(modind$metric))) },
                                               if (is.null(modind$scalar)) { NULL } else { c("Scalar", rep(NA, times = nrow(modind$scalar))) },
                                               if (is.null(modind$strict)) { NULL } else { c("strict", rep(NA, times = nrow(modind$strict))) }) }),
                           do.call("rbind", lapply(modind, function(y) rbind(NA, y))),
                           row.names = NULL, fix.empty.names = FALSE)

      # Round
      modind[, c("mi", "epc", "stdyx")] <- sapply(modind[, c("mi", "epc", "stdyx")], round, digits = digits)

      # Column names
      colnames(modind) <- c("", if (isTRUE(!x$args$long)) { "Group" }, "lhs", "op", "rhs", "MI", "EPC", "StdYX")

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Modification Indices for Parameter Constaints ####

    score <- NULL

    if (isTRUE("modind" %in% x$args$print && any(!sapply(write.object$score, is.null)))) {

      # Extract result table
      score <- write.object$score

      # Remove NULL entries
      score <- score[!sapply(score, is.null)]

      # Combine data frames
      score <- data.frame(switch(x$args$invar,
                                  config = {   if (is.null(score$config)) { NULL } else { c("Config", rep(NA, times = nrow(score$config))) } },
                                  metric = { c(if (is.null(score$config)) { NULL } else { c("Config", rep(NA, times = nrow(score$config))) },
                                               if (is.null(score$metric)) { NULL } else { c("Metric", rep(NA, times = nrow(score$metric))) }) },
                                  scalar = { c(if (is.null(score$config)) { NULL } else { c("Config", rep(NA, times = nrow(score$config))) },
                                               if (is.null(score$metric)) { NULL } else { c("Metric", rep(NA, times = nrow(score$metric))) },
                                               if (is.null(score$scalar)) { NULL } else { c("Scalar", rep(NA, times = nrow(score$scalar))) }) },
                                  strict = { c(if (is.null(score$config)) { NULL } else { c("Config", rep(NA, times = nrow(score$config))) },
                                               if (is.null(score$metric)) { NULL } else { c("Metric", rep(NA, times = nrow(score$metric))) },
                                               if (is.null(score$scalar)) { NULL } else { c("Scalar", rep(NA, times = nrow(score$scalar))) },
                                               if (is.null(score$strict)) { NULL } else { c("strict", rep(NA, times = nrow(score$strict))) }) }),
                           do.call("rbind", lapply(score, function(y) rbind(NA, y))),
                           row.names = NULL, fix.empty.names = FALSE)

      # Round
      score[, c("mi", "lhs.epc", "rhs.epc", "lhs.stdyx", "rhs.stdyx")] <- sapply(score[, c("mi", "lhs.epc", "rhs.epc", "lhs.stdyx", "rhs.stdyx")], round, digits = digits)
      score[, "pvalue"] <- round(score[, "pvalue"], digits = p.digits)

      # Column names
      colnames(score) <- c("", "Label", if (isTRUE(!x$args$long)) { c("Group.lhs", "Group.rhs") }, "lhs", "op", "rhs", "MI", "df", "pvalue", "lhs.EPC", "rhs.EPC", "lhs.StdYX", "rhs.StdYX")

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Residual Correlation Matrix ####

    resid <- NULL

    if (isTRUE("resid" %in% x$args$print && any(!sapply(write.object$resid, is.null)))) {

      # Extract result table
      resid <- write.object$resid

      # Remove NULL entries
      resid <- resid[!sapply(resid, is.null)]

      ### Between-group measurement invariance
      if (isTRUE(!x$args$long)) {

        resid <- data.frame(switch(x$args$invar,
                                   config = {   if (is.null(resid$config)) { NULL } else { rep(c("Config", rep(NA, times = nrow(resid$config[[1L]]))), times = length(resid$config)) } },
                                   metric = { c(if (is.null(resid$config)) { NULL } else { rep(c("Config", rep(NA, times = nrow(resid$config[[1L]]))), times = length(resid$config)) },
                                                if (is.null(resid$metric)) { NULL } else { rep(c("Metric", rep(NA, times = nrow(resid$metric[[1L]]))), times = length(resid$metric)) }) },
                                   scalar = { c(if (is.null(resid$config)) { NULL } else { rep(c("Config", rep(NA, times = nrow(resid$config[[1L]]))), times = length(resid$config)) },
                                                if (is.null(resid$metric)) { NULL } else { rep(c("Metric", rep(NA, times = nrow(resid$metric[[1L]]))), times = length(resid$metric)) },
                                                if (is.null(resid$scalar)) { NULL } else { rep(c("Scalar", rep(NA, times = nrow(resid$scalar[[1L]]))), times = length(resid$scalar)) }) },
                                   strict = { c(if (is.null(resid$config)) { NULL } else { rep(c("Config", rep(NA, times = nrow(resid$config[[1L]]))), times = length(resid$config)) },
                                                if (is.null(resid$metric)) { NULL } else { rep(c("Metric", rep(NA, times = nrow(resid$metric[[1L]]))), times = length(resid$metric)) },
                                                if (is.null(resid$scalar)) { NULL } else { rep(c("Scalar", rep(NA, times = nrow(resid$scalar[[1L]]))), times = length(resid$scalar)) },
                                                if (is.null(resid$strict)) { NULL } else { rep(c("strict", rep(NA, times = nrow(resid$strict[[1L]]))), times = length(resid$strict)) }) }),
                            do.call("rbind", lapply(lapply(resid, function(y) do.call("rbind", lapply(y, function(z) rbind(NA, z)))), function(q) data.frame(rep(names(resid[[1L]]), each = nrow(resid[[1L]][[1L]]) + 1L), c("", row.names(resid[[1L]][[1L]])), q, fix.empty.names = FALSE))),
                            row.names = NULL, fix.empty.names = FALSE)

        # Round
        resid[, -c(1L:3L)] <- sapply(resid[, -c(1L:3L)], round, digits = p.digits)

        # Column names
        colnames(resid) <- c("", if (isTRUE(!x$args$long)) { "Group" }, colnames(resid)[-c(1L:2L)])

      ### Longitudinal measurement invariance
      } else {

        resid <- data.frame(switch(x$args$invar,
                                   config = {   if (is.null(resid$config)) { NULL } else { c("Config", rep(NA, times = nrow(resid$config))) } },
                                   metric = { c(if (is.null(resid$config)) { NULL } else { c("Config", rep(NA, times = nrow(resid$config))) },
                                                if (is.null(resid$metric)) { NULL } else { c("Metric", rep(NA, times = nrow(resid$metric))) }) },
                                   scalar = { c(if (is.null(resid$config)) { NULL } else { c("Config", rep(NA, times = nrow(resid$config))) },
                                                if (is.null(resid$metric)) { NULL } else { c("Metric", rep(NA, times = nrow(resid$metric))) },
                                                if (is.null(resid$scalar)) { NULL } else { c("Scalar", rep(NA, times = nrow(resid$scalar))) }) },
                                   strict = { c(if (is.null(resid$config)) { NULL } else { c("Config", rep(NA, times = nrow(resid$config))) },
                                                if (is.null(resid$metric)) { NULL } else { c("Metric", rep(NA, times = nrow(resid$metric))) },
                                                if (is.null(resid$scalar)) { NULL } else { c("Scalar", rep(NA, times = nrow(resid$scalar))) },
                                                if (is.null(resid$strict)) { NULL } else { c("strict", rep(NA, times = nrow(resid$strict))) }) }),
                            data.frame(c(NA, rownames(resid$config)), do.call("rbind", lapply(resid, function(y) rbind(NA, y))),
                                       row.names = NULL, fix.empty.names = FALSE), row.names = NULL, fix.empty.names = FALSE)

        # Round
        resid[, -c(1L:2L)] <- sapply(resid[, -c(1L:2L)], round, digits = p.digits)

      }

    }

    #...................
    ### Write object ####

    write.object <- list(summary = summary, coverage = coverage, itemstat = itemstat,
                         fit = fit, param = param, modind = modind,
                         score = score, resid = resid)

    # Print
    if (isTRUE(!"summary" %in% x$args$print)) { write.object$summary <- NULL }
    if (isTRUE(!"coverage" %in% x$args$print)) { write.object$coverage <- NULL }
    if (isTRUE(!"descript" %in% x$args$print)) { write.object$itemstat <- NULL }
    if (isTRUE(!"fit" %in% x$args$print)) { write.object$fit <- NULL }
    if (isTRUE(!"est" %in% x$args$print)) { write.object$param <- NULL }
    if (isTRUE(!"modind" %in% x$args$print)) { write.object$modind <- NULL; write.object$score <- NULL }
    if (isTRUE(!"resid" %in% x$args$print)) { write.object$resid <- NULL }

  #_____________________________________________________________________________
  #
  # Coefficient Omega, item.omega() --------------------------------------------

  }, item.omega = {

    if (is.null(write.object$itemstat)) {

      write.object <- write.object$omega
      names(write.object) <- c("Items", "Omega")

      write.object$Omega <- round(write.object$Omega, digits = digits)

    } else {

      names(write.object)  <- c("Omega", "Itemstat")

      names(write.object$Omega) <- c("n", "Items", "Omega", "Low", "Upp")
      names(write.object$Itemstat) <- c("Variable", "n", "nNA", "pNA", "M", "SD", "Min", "Max", "Std.Ld", "Omega")

      write.object$Omega <- round(write.object$Omega, digits = digits)
      write.object$Itemstat[, -1L] <- round(write.object$Itemstat[, -1L], digits = digits)

    }

    if (isTRUE(!"omega" %in% x$args$print)) { write.object$Omega <- NULL }
    if (isTRUE(!"item" %in% x$args$print)) { write.object$Itemstat <- NULL }

  #_____________________________________________________________________________
  #
  # Multilevel Confirmatory Factor Analysis, multilevel.cfa() ------------------

  }, multilevel.cfa = {

    ### lavaan summary ####

    # Column names
    colnames(write.object$summary) <- c(write.object$summary[1, 1], "", "")

    summary <- write.object$summary[-1, ]

    #...................
    ### Covariance coverage ####

    # Round
    write.object$coverage <- sapply(data.frame(write.object$coverage), round, digits = digits)

    # Add variable names in the rows
    coverage <- data.frame(colnames(write.object$coverage), write.object$coverage,
                           row.names = NULL, check.rows = FALSE,
                           check.names = FALSE, fix.empty.names = FALSE)

    #...................
    ### Univariate Sample Statistics ####

    itemstat <- write.object$descript

    # Round
    itemstat[, -1L] <- sapply(itemstat[, -1L], round, digits = digits)

    colnames(itemstat) <- c("Variable", "n", "nNA", "pNA", "M", "SD", "Min", "Max", "Skew", "Kurt", "ICC(1)")

    #...................
    ### Model fit ####

    fit <- write.object$fit

    # Round
    fit[, -1L] <- sapply(fit[, -1L], round, digits = digits)

    # Estimator = "ML"
    if (isTRUE(ncol(write.object$fit) == 2L)) {

      colnames(fit) <- c("", "Standard")

    } else {

      colnames(fit) <- c("", "Standard", "Scaled", "Robust")

    }

    #...................
    ### Parameter estimates ####

    param <- rbind(data.frame(Level = "Within", write.object$param$within),
                   data.frame(Level = "Between", write.object$param$between))

    # Round
    param[, -c(1L:5L, 9L)] <- sapply(param[, -c(1L:5L, 9L)], round, digits = digits)
    param[, 9L] <- sapply(param[, 9L], round, digits = p.digits)

    colnames(param) <- c("Parameter", "Variable", "lhs", "op", "rhs", "Estimate", "SE", "z", "pvalue", "StdYX")

    #...................
    ### Modification indices ####

    if (isTRUE(nrow(write.object$modind$within) == 0L)) {

      write.object$modind$within <- data.frame(matrix(NA, ncol = 6L, dimnames = list(NULL, names(write.object$modind$within))))

    }

    if (isTRUE(nrow(write.object$modind$between) == 0L)) {

      write.object$modind$between <- data.frame(matrix(NA, ncol = 6L, dimnames = list(NULL, names(write.object$modind$between))))

    }

    modind <- rbind(data.frame(Level = "Within", write.object$modind$within),
                    data.frame(Level = "Between", write.object$modind$between))

    # Round
    modind[, -c(1L:4L)] <- sapply(modind[, -c(1L:4L)], round, digits = digits)

    colnames(modind) <- c("Level", "lhs", "op", "rhs", "MI", "EPC", "STDYX EPC")

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Modification Indices for Parameter Constaints ####

    score <- NULL

    if (isTRUE("modind" %in% x$args$print && !is.null(write.object$score))) {

      # Extract result table
      score <- write.object$score

      # Round
      score[, c("mi", "lhs.epc", "rhs.epc", "lhs.stdyx", "rhs.stdyx")] <- sapply(score[, c("mi", "lhs.epc", "rhs.epc", "lhs.stdyx", "rhs.stdyx")], round, digits = digits)
      score[, "pvalue"] <- round(score[, "pvalue"], digits = p.digits)

      # Column names
      colnames(score) <- c("Label", "lhs", "op", "rhs", "MI", "df", "pvalue", "lhs.EPC", "rhs.EPC", "lhs.StdYX", "rhs.StdYX")

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Residual Correlation Matrix ####

    resid <- NULL

    if (isTRUE("resid" %in% x$args$print && !is.null(write.object$resid))) {

      # Extract result table
      resid <- write.object$resid

      # Combine Within and Between level
      resid <- data.frame(c("Within", rep("", times = nrow(resid[[1L]])), "Between", rep("", times = nrow(resid[[1L]]))),
                            do.call("rbind", lapply(resid, function(z) rbind(NA, z))), row.names = NULL, fix.empty.names = FALSE)

      # Round
      resid[, -1L] <- sapply(resid[, -1L], round, digits = p.digits)

    }

    #...................
    ### Write object ####

    write.object <- list(summary = summary, coverage = coverage, descript = itemstat,
                         fit = fit, param = param, modind = modind, score = score,
                         resid = resid)

    # Print
    if (isTRUE(!"summary" %in% x$args$print)) { write.object$summary <- NULL }
    if (isTRUE(!"coverage" %in% x$args$print)) { write.object$coverage <- NULL }
    if (isTRUE(!"descript" %in% x$args$print)) { write.object$itemstat <- NULL; write.object$itemfreq <- NULL }
    if (isTRUE(!"fit" %in% x$args$print)) { write.object$fit <- NULL }
    if (isTRUE(!"est" %in% x$args$print)) { write.object$param <- NULL }
    if (isTRUE(!"modind" %in% x$args$print)) { write.object$modind <- NULL; write.object$score <- NULL  }
    if (isTRUE(!"resid" %in% x$args$print)) { write.object$resid <- NULL }

  #_____________________________________________________________________________
  #
  # Within- and Between-Group Correlation Matrix, multilevel.cor() -------------

  }, multilevel.cor = {

    #............
    ### Split results
    if (isTRUE(x$args$split)) {

      #### Round
      write.object$with.cor <- sapply(data.frame(write.object$with.cor), round, digits = digits)
      write.object$with.se <- sapply(data.frame(write.object$with.se), round, digits = digits)
      write.object$with.stat <- sapply(data.frame(write.object$with.stat), round, digits = digits)
      write.object$with.p <- sapply(data.frame(write.object$with.p), round, digits = p.digits)

      write.object$betw.cor <- sapply(data.frame(write.object$betw.cor), round, digits = digits)
      write.object$betw.se <- sapply(data.frame(write.object$betw.se), round, digits = digits)
      write.object$betw.stat <- sapply(data.frame(write.object$betw.stat), round, digits = digits)
      write.object$betw.p <- sapply(data.frame(write.object$betw.p), round, digits = p.digits)

      #### Lower and/or upper triangular
      if (isTRUE(tri == "lower")) {

        write.object$with.cor[upper.tri(write.object$with.cor)] <- NA
        write.object$with.se[upper.tri(write.object$with.se)] <- NA
        write.object$with.stat[upper.tri(write.object$with.stat)] <- NA
        write.object$with.p[upper.tri(write.object$with.p)] <- NA

        write.object$betw.cor[upper.tri(write.object$betw.cor)] <- NA
        write.object$betw.se[upper.tri(write.object$betw.se)] <- NA
        write.object$betw.stat[upper.tri(write.object$betw.stat)] <- NA
        write.object$betw.p[upper.tri(write.object$betw.p)] <- NA

      }

      if (isTRUE(tri == "upper")) {

        write.object$with.cor[lower.tri(write.object$with.cor)] <- NA
        write.object$with.se[lower.tri(write.object$with.se)] <- NA
        write.object$with.stat[lower.tri(write.object$with.stat)] <- NA
        write.object$with.p[lower.tri(write.object$with.p)] <- NA

        write.object$betw.cor[lower.tri(write.object$betw.cor)] <- NA
        write.object$betw.se[lower.tri(write.object$betw.se)] <- NA
        write.object$betw.stat[lower.tri(write.object$betw.stat)] <- NA
        write.object$betw.p[lower.tri(write.object$betw.p)] <- NA

      }

      write.object <- list(summary = write.object$summary,
                           with.cor = write.object$with.cor, with.se = write.object$with.se,
                           with.stat = write.object$with.stat, with.p = write.object$with.p,
                           betw.cor = write.object$betw.cor, betw.se = write.object$betw.se,
                           betw.stat = write.object$betw.stat, betw.p = write.object$betw.p)

      #### Add 'Lower triangular: Within-Group, Upper triangular: Between-Group
      write.object$summary <- data.frame(rbind(write.object$summary,
                                               c(NA, NA, NA),
                                               c("Lower triangular: Within-Group, Upper triangular: Between-Group", NA, NA)),
                                         row.names = NULL, check.rows = FALSE, check.names = FALSE, fix.empty.names = FALSE)

      #### Print
      if (isTRUE(!"cor" %in% x$args$print)) { write.object$with.cor <- NULL; write.object$betw.cor <- NULL }
      if (isTRUE(!"se" %in% x$args$print)) { write.object$with.se <- NULL; write.object$betw.se <- NULL }
      if (isTRUE(!"stat" %in% x$args$print)) { write.object$with.stat <- NULL; write.object$betw.stat <- NULL }
      if (isTRUE(!"p" %in% x$args$print)) { write.object$with.p <- NULL; write.object$betw.p <- NULL }

    #............
    ### Combined results
    } else {

      #### Round
      write.object$wb.cor <- sapply(data.frame(write.object$wb.cor), round, digits = digits)
      write.object$wb.se <- sapply(data.frame(write.object$wb.se), round, digits = digits)
      write.object$wb.stat <- sapply(data.frame(write.object$wb.stat), round, digits = digits)
      write.object$wb.p <- sapply(data.frame(write.object$wb.p), round, digits = p.digits)

      write.object <- list(summary = write.object$summary,
                           cor = write.object$wb.cor, se = write.object$wb.se,
                           stat = write.object$wb.stat, p = write.object$wb.p)

      #### Print
      if (isTRUE(!"cor" %in% x$args$print)) { write.object$cor <- NULL }
      if (isTRUE(!"se" %in% x$args$print)) { write.object$se <- NULL }
      if (isTRUE(!"stat" %in% x$args$print)) { write.object$stat <- NULL }
      if (isTRUE(!"p" %in% x$args$print)) { write.object$p <- NULL }

    }

    #............
    ###  Add variable names in the rows
    write.object[-1L] <- lapply(write.object[-1L], function(y) data.frame(colnames(y), y,
                                                                          row.names = NULL, check.rows = FALSE,
                                                                          check.names = FALSE, fix.empty.names = FALSE))

  #_____________________________________________________________________________
  #
  # Multilevel Descriptive Statistics, multilevel.descript() -------------------

  }, multilevel.descript = {

    # Round
    write.object$m.cluster.size <- round(write.object$m.cluster.size, digits = digits)
    write.object$sd.cluster.size <- round(write.object$sd.cluster.size, digits = digits)
    write.object$mean <- round(write.object$mean, digits = digits)
    write.object$var.w <- round(write.object$var.w, digits = digits)
    write.object$var.b <- round(write.object$var.b, digits = digits)
    write.object$sd.w <- round(write.object$sd.w, digits = digits)
    write.object$sd.b <- round(write.object$sd.b, digits = digits)
    write.object$icc1 <- round(write.object$icc1, digits = icc.digits)
    write.object$icc2 <- round(write.object$icc2, digits = icc.digits)
    write.object$deff <- round(write.object$deff, digits = digits)
    write.object$deff.sqrt <- round(write.object$deff.sqrt, digits = digits)
    write.object$n.effect <- round(write.object$n.effect, digits = digits)

    write.object <- data.frame(cbind(c("No. of cases", "No. of missing values",
                                       "", "No. of clusters", "Average cluster size", "SD cluster size", "Min cluster size", "Max cluster size",
                                       "", "Mean", "Variance Within", "Variance Between", "SD Within", "SD Between", "ICC(1)", "ICC(2)",
                                       "", "Design effect", "Design effect sqrt", "Effective sample size"),
                                     rbind(write.object$no.obs, write.object$no.miss,
                                           "", write.object$no.cluster, write.object$m.cluster.size, write.object$sd.cluster.size, write.object$min.cluster.size, write.object$max.cluster.size,
                                           "", write.object$mean, write.object$var.w, write.object$var.b, write.object$sd.w, write.object$sd.b, write.object$icc1, write.object$icc2,
                                           "", write.object$deff, write.object$deff.sqrt, write.object$n.effect)), stringsAsFactors = FALSE)

    #............
    ### select rows

    if (isTRUE(!"var" %in% x$args$print)) { write.object <- write.object[-c(11L:12L), ] }
    if (isTRUE(!"sd" %in% x$args$print)) { write.object <- write.object[-c(13L:14L), ] }

    # All Between variables
    if (isTRUE(all(is.na(x$result$var.w)))) { write.object <- write.object[-which(misty::chr.trim(write.object[, 1L]) %in% c("Variance Within", "SD Within", "ICC(1)", "ICC(2)", "Design effect", "Design effect sqrt", "Effective sample size")), ] }

    #...................
    ### One variable ####
    if (isTRUE(length(x$result$no.obs) == 1L)) {

      write.object[, -1L] <- as.numeric(write.object[, -1])
      names(write.object)[1L] <- ""

    #...................
    ### More than one variable ####
    } else {

      write.object[, -1L] <- vapply(write.object[, -1L], as.numeric, FUN.VALUE = numeric(nrow(write.object)))

      names(write.object) <- c("", names(x$result$no.obs))

    }

  #_____________________________________________________________________________
  #
  # Simultaneous and Level-Specific Multilevel Model Fit Information, multievel.fit() ----

  }, multilevel.fit = {

    #...................
    ### lavaan summary ####

    # Column names
    colnames(write.object$summary) <- c(write.object$summary[1L, 1L], "", "")

    summary <- write.object$summary[-1L, ]

    #...................
    ### Model fit ####

    fit <- write.object$fit

    # Round
    fit[, -1L] <- round(fit[, -1L], digits = digits)

    # Estimator = "ML"
    if (isTRUE(ncol(fit) == 2L)) {

      colnames(fit) <- c("", "Standard")

    # Estimator = "MLR"
    } else {

      colnames(fit) <- c("", "Standard", "Scaled", "Robust")

    }

    #...................
    ### Write object ####

    write.object <- list(summary = summary, fit = fit)

    # Print
    if (isTRUE(!"summary" %in% x$args$print)) { write.object$summary <- NULL }
    if (isTRUE(!"fit" %in% x$args$print)) { write.object$fit <- NULL }

  #_____________________________________________________________________________
  #
  # Cross-Level Measurement Invariance Evaluation, multievel.invar() ----

  }, multilevel.invar = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## lavaan summary ####

    # Column names
    colnames(write.object$summary) <- c(write.object$summary[1L, 1L], "", "")

    summary <- write.object$summary[-1L, ]

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Covariance coverage ####

    # Round
    write.object$coverage <- sapply(data.frame(write.object$coverage), round, digits = digits)

    # Add variable names in the rows
    coverage <- data.frame(colnames(write.object$coverage), write.object$coverage,
                           row.names = NULL, check.rows = FALSE,
                           check.names = FALSE, fix.empty.names = FALSE)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Univariate Sample Statistics ####

    itemstat <- write.object$descript

    # Round
    itemstat[, -1L] <- sapply(itemstat[, -1L], round, digits = digits)

    colnames(itemstat) <- c("Variable", "n", "nNA", "pNA", "M", "SD", "Min", "Max", "Skew", "Kurt", "ICC(1)")

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Model fit ####

    # Extract result table
    fit <- write.object$fit

    # Remove NULL entries
    fit <- fit[!sapply(fit, is.null)]

    #### Standard fit indices
    if (isTRUE(x$args$estimator %in% c("ML", "MLF", "GLS", "WLS", "DWLS", "ULS", "PML"))) {

      # Combine data frames
      fit <- data.frame(c("Standard", rep(NA, times = nrow(fit$stand))),
                        rbind(NA, fit$stand),
                        row.names = NULL, fix.empty.names = FALSE)

    #### Standard, scaled, and robust fit indices
    } else {

      # Combine data frames
      fit <- data.frame(c("Standard", rep(NA, times = nrow(fit$stand)), "Scaled", rep(NA, times = nrow(fit$scaled)), "Robust", rep(NA, times = nrow(fit$robust))),
                        do.call("rbind", lapply(fit, function(y) rbind(NA, y))),
                        row.names = NULL, fix.empty.names = FALSE)

    }

    # Round
    fit[which(!fit[, 2L] %in% c("P-value", "P-value RMSEA <= 0.05")), c(3L:ncol(fit))] <- sapply(fit[which(!fit[, 2L] %in% c("P-value", "P-value RMSEA <= 0.05")), c(3L:ncol(fit))], round, digits = digits)
    fit[which(fit[, 2L] %in% c("P-value", "P-value RMSEA <= 0.05")), c(3L:ncol(fit))] <- sapply(fit[which(fit[, 2L] %in% c("P-value", "P-value RMSEA <= 0.05")), c(3L:ncol(fit))], round, digits = p.digits)

    # Column names
    switch(x$args$invar,
           config = { colnames(fit) <- c("", "", "Config") },
           metric = { colnames(fit) <- c("", "", "Config", "Metric", "dMetric") },
           scalar = { colnames(fit) <- c("", "", "Config", "Metric", "Scalar", "dMetric", "dScalar") })

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Parameter estimates ####

    # Extract result table
    param <- write.object$param

    # Remove NULL entries
    param <- param[!sapply(param, is.null)]

    # Combine data frames
    param <- lapply(lapply(param, function(y) do.call("rbind", lapply(y, function(z) rbind(NA, z)))), function(q) data.frame(c("Within", rep(NA, times = nrow(q) / 2L - 1L), "Between", rep(NA, times = nrow(q) / 2L - 1L)), q, row.names = NULL, fix.empty.names = FALSE))

    # Combine data frames
    param <- data.frame(switch(x$args$invar,
                               config = { c("Config", rep(NA, times = nrow(param$config))) },
                               metric = { c("Config", rep(NA, times = nrow(param$config)), "Metric", rep(NA, times = nrow(param$metric))) },
                               scalar = { c("Config", rep(NA, times = nrow(param$config)), "Metric", rep(NA, times = nrow(param$metric)), "Scalar", rep(NA, times = nrow(param$scalar))) }),
                        do.call("rbind", lapply(param, function(y) rbind(NA, y))),
                        row.names = NULL, fix.empty.names = FALSE)

    # Round
    param[, c("est", "se", "z", "stdyx")] <- sapply(param[, c("est", "se", "z", "stdyx")], round, digits = digits)
    param[, "pvalue"] <- round(param[, "pvalue"], digits = p.digits)

    # Column names
    colnames(param) <- c("", "Parameter", "lhs", "op", "rhs", "label", "Estimate", "SE", "z", "pvalue", "StdYX")

    #...................
    ### Modification indices ####

    modind <- NULL

    if (isTRUE("modind" %in% x$args$print && any(!sapply(write.object$modind, is.null)))) {

      # Extract result table
      modind <- write.object$modind

      # Remove NULL entries
      modind <- modind[!sapply(modind, is.null)]

      # Combine data frames
      modind <- lapply(lapply(modind, function(y) do.call("rbind", lapply(y, function(z) rbind(NA, z)))), function(q) data.frame(c("Within", rep(NA, times = nrow(q) / 2L - 1L), "Between", rep(NA, times = nrow(q) / 2L - 1L)), q, row.names = NULL, fix.empty.names = FALSE))

      # Combine data frames
      modind <- data.frame(switch(x$args$invar,
                                  config = {   if (is.null(modind$config)) { NULL } else { c("Config", rep(NA, times = nrow(modind$config))) } },
                                  metric = { c(if (is.null(modind$config)) { NULL } else { c("Config", rep(NA, times = nrow(modind$config))) },
                                               if (is.null(modind$metric)) { NULL } else { c("Metric", rep(NA, times = nrow(modind$metric))) }) },
                                  scalar = { c(if (is.null(modind$config)) { NULL } else { c("Config", rep(NA, times = nrow(modind$config))) },
                                               if (is.null(modind$metric)) { NULL } else { c("Metric", rep(NA, times = nrow(modind$metric))) },
                                               if (is.null(modind$scalar)) { NULL } else { c("Scalar", rep(NA, times = nrow(modind$scalar))) }) }),
                           do.call("rbind", lapply(modind, function(y) rbind(NA, y))),
                           row.names = NULL, fix.empty.names = FALSE)

      # Round
      modind[, c("mi", "epc", "stdyx")] <- sapply(modind[, c("mi", "epc", "stdyx")], round, digits = digits)

      # Column names
      colnames(modind) <- c("", "lhs", "op", "rhs", "MI", "EPC", "StdYX")

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Residual Correlation Matrix ####

    resid <- NULL

    if (isTRUE("resid" %in% x$args$print && any(!sapply(write.object$resid, is.null)))) {

      # Extract result table
      resid <- write.object$resid

      # Remove NULL entries
      resid <- resid[!sapply(resid, is.null)]

      # Combine data frames
      resid <- lapply(lapply(resid, function(y) do.call("rbind", lapply(y, function(z) rbind(NA, z)))), function(q) data.frame(c("Within", rep(NA, times = nrow(q) / 2L - 1L), "Between", rep(NA, times = nrow(q) / 2L - 1L)),  c(NA, rownames(resid[[1]]$within)), q, row.names = NULL, fix.empty.names = FALSE))

      resid <- data.frame(switch(x$args$invar,
                                 config = {   if (is.null(resid$config)) { NULL } else { c("Config", rep(NA, times = nrow(resid$config))) } },
                                 metric = { c(if (is.null(resid$config)) { NULL } else { c("Config", rep(NA, times = nrow(resid$config))) },
                                              if (is.null(resid$metric)) { NULL } else { c("Metric", rep(NA, times = nrow(resid$metric))) }) },
                                 scalar = { c(if (is.null(resid$config)) { NULL } else { c("Config", rep(NA, times = nrow(resid$config))) },
                                              if (is.null(resid$metric)) { NULL } else { c("Metric", rep(NA, times = nrow(resid$metric))) },
                                              if (is.null(resid$scalar)) { NULL } else { c("Scalar", rep(NA, times = nrow(resid$scalar))) }) }),
                          data.frame(do.call("rbind", lapply(resid, function(y) rbind(NA, y))),
                                     row.names = NULL, fix.empty.names = FALSE), row.names = NULL, fix.empty.names = FALSE)

      # Round
      resid[, -c(1L:3L)] <- sapply(resid[, -c(1L:3L)], round, digits = p.digits)

    }

    #...................
    ### Write object ####

    write.object <- list(summary = summary, coverage = coverage, descript = itemstat,
                         fit = fit, param = param, modind = modind,
                         resid = resid)

    # Print
    if (isTRUE(!"summary" %in% x$args$print)) { write.object$summary <- NULL }
    if (isTRUE(!"coverage" %in% x$args$print)) { write.object$coverage <- NULL }
    if (isTRUE(!"descript" %in% x$args$print)) { write.object$itemstat <- NULL; write.object$itemfreq <- NULL }
    if (isTRUE(!"fit" %in% x$args$print)) { write.object$fit  <- NULL }
    if (isTRUE(!"est" %in% x$args$print)) { write.object$param <- NULL }
    if (isTRUE(!"modind" %in% x$args$print)) { write.object$modind <- NULL }
    if (isTRUE(!"resid" %in% x$args$print)) { write.object$resid <- NULL }

  #_____________________________________________________________________________
  #
  # Variance-Covariance Coverage, na.coverage() --------------------------------

  }, na.coverage = {

    write.object <- sapply(data.frame(write.object), round, digits = digits)

    # Add variable names in the rows
    write.object <- data.frame(colnames(write.object), write.object,
                               row.names = NULL, check.rows = FALSE,
                               check.names = FALSE, fix.empty.names = FALSE)

  #_____________________________________________________________________________
  #
  # Descriptive Statistics for Missing Data, na.descript() ---------------------

  }, na.descript = {

    # Round
    write.object$perc.complete <- round(write.object$perc.complete, digits = digits)
    write.object$perc.incomplete <- round(write.object$perc.incomplete, digits = digits)
    write.object$perc.observed.values <- round(write.object$perc.observed.values, digits = digits)
    write.object$perc.missing.values <- round(write.object$perc.missing.values, digits = digits)
    write.object$perc.missing.mean <- round(write.object$perc.missing.mean, digits = digits)
    write.object$perc.missing.sd <- round(write.object$perc.missing.sd, digits = digits)
    write.object$perc.missing.min <- round(write.object$perc.missing.min, digits = digits)
    write.object$perc.missing.p25 <- round(write.object$perc.missing.p25, digits = digits)
    write.object$perc.missing.p75 <- round(write.object$perc.missing.p75, digits = digits)
    write.object$perc.missing.max <- round(write.object$perc.missing.max, digits = digits)

    write.object <- data.frame(c("No. of cases", "No. of complete cases", "No. of incomplete cases", NA,
                                 "No. Of values", "No. Of observed values", "No of missing values", NA,
                                 "No. Of variables", "No. Of missing values across all variables",
                                 "   Mean", "   SD", "   Minimum", "   P25", "   P75", "   Maximum"),
                               Freq = c(write.object$no.cases, write.object$no.complete, write.object$no.incomplete, NA,
                                        write.object$no.values, write.object$no.observed.values, write.object$no.missing.values, NA,
                                        write.object$no.var, NA,
                                        write.object$no.missing.mean, write.object$no.missing.sd, write.object$no.missing.min,
                                        write.object$no.missing.p25, write.object$no.missing.p75, write.object$no.missing.max),
                               Perc = c(NA, write.object$perc.complete, write.object$perc.incomplete, NA,
                                        NA, write.object$perc.observed.values, write.object$perc.missing.values, NA,
                                        NA, NA,
                                        write.object$perc.missing.mean, write.object$perc.missing.sd, write.object$perc.missing.min,
                                        write.object$perc.missing.p25, write.object$perc.missing.p75, write.object$perc.missing.max),
                               row.names = NULL, check.rows = FALSE,
                               check.names = FALSE, fix.empty.names = FALSE)

    # Frequency table for each variable
    write.object <- list(Summary = write.object, Table = x$result$table.miss)

  #_____________________________________________________________________________
  #
  # Multilevel Composite Reliability, multilevel.omega() -----------------------

  }, multilevel.omega = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Omega ####

    write.omega <- NULL

    if (isTRUE("omega" %in% x$args$print)) {

      # Extracr result table
      write.omega <- write.object$omega

      #### Round ####
      write.omega[, -c(1L:2L)] <- sapply(write.omega[, -c(1L:2L)], round, digits = digits)

      #### Column names ####
      colnames(write.omega) <- c("Type", "Items", "Omega", "Low", "Upp")

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Item Statistics ####

    write.item <- NULL

    if (isTRUE("item" %in% x$args$print)) {

      # Extracr result table
      write.item <- write.object$item

      #### Round ####

      # Variables to round
      write.round <- switch(x$args$const,
                            within = c("pNA", "m", "sd", "min", "max", "skew", "kurt", "ICC", "wstd.ld"),
                            shared = c("pNA", "m", "sd", "min", "max", "skew", "kurt", "ICC", "bstd.ld"),
                            config = c("pNA", "m", "sd", "min", "max", "skew", "kurt", "ICC", "wstd.ld", "bstd.ld"))

      write.item[, write.round] <- sapply(write.item[, write.round], round, digits = digits)

      #### Column names ####
      colnames(write.item) <- switch(x$args$const,
                                     within = c("Variable", "n", "nNA", "pNA", "M", "SD", "Min", "Max",  "Skew", "Kurt", "ICC(1)", "WStd.ld"),
                                     shared = c("Variable", "n", "nNA", "pNA", "M", "SD", "Min", "Max",  "Skew", "Kurt", "ICC(1)", "BStd.ld"),
                                     config = c("Variable", "n", "nNA", "pNA", "M", "SD", "Min", "Max",  "Skew", "Kurt", "ICC(1)", "WStd.ld", "BStd.ld"))

    }

    #### Write object ####
    write.object <- list(Omega = write.omega, Itemstat = write.item)

  #_____________________________________________________________________________
  #
  # Missing Data Pattern, na.pattern() -----------------------------------------

  }, na.pattern = {

    # Round
    write.object$perc <- round(write.object$perc, digits = digits)
    write.object$pNA <- round(write.object$pNA, digits = digits)

    names(write.object)[c(1, 3)] <- c("Pattern", "Perc")

  #_____________________________________________________________________________
  #
  # Missing Data Pattern, na.pattern() -----------------------------------------
  }, na.pattern = {

    # Round
    write.object$perc <- round(write.object$perc, digits = digits)
    write.object$pNA <- round(write.object$pNA, digits = digits)

    names(write.object)[c(1, 3)] <- c("Pattern", "Perc")

  #_____________________________________________________________________________
  #
  # Result Table for LCA Estimated in Mplus, result.lca() ----------------------
  }, result.lca = {

    #...................
    ### Result tables ####

    write.object.summary <- write.object$summary
    write.object.mean.var <- write.object$mean_var
    write.object.mean <- write.object$mean
    write.object.var <- write.object$var

    #...................
    ### Round ####

    tests <- intersect(c("chi.pear", "chi.lrt", "lmr.lrt", "almr.lrt", "blrt", "entropy"), colnames(write.object.summary))

    write.object.summary[, c("LL", "aic", "caic", "bic", "sabic")] <- round(write.object.summary[, c("LL", "aic", "caic", "bic", "sabic")], digits = digits)
    write.object.summary[, "LL.scale"] <- round(write.object.summary[, "LL.scale"], digits = digits + 1L)
    write.object.summary[, c(tests, colnames(write.object.summary)[substr(colnames(write.object.summary), 1L, 1L) == "p"])] <- round(write.object.summary[, c(tests, colnames(write.object.summary)[substr(colnames(write.object.summary), 1L, 1L) == "p"])], digits = p.digits)

    #...................
    ### Column names ####

    colnames(write.object.summary) <- c("Folder", "#Class", "Conv", "#Param", "logLik", "Scale", "LL Rep", "AIC", "CAIC", "BIC", "SABIC",
                                        misty::rec(tests, "'lmr.lrt' = 'LMR-LRT'; 'almr.lrt' = 'A-LRT'; 'blrt' = 'BLRT'; 'chi.pear' = 'Chi-Pear'; 'chi.lrt' = 'Chi-LRT'; 'entropy' = 'Entropy'"),
                                        colnames(write.object.summary)[substr(colnames(write.object.summary), 1L, 1L) == "p"])

    #...................
    ### TRUE/FALSE into Yes/NO ####

    write.object.summary$Conv <- sapply(write.object.summary$Conv, function(y) ifelse(isTRUE(y), "Yes", "No"))
    write.object.summary$`LL Rep` <- sapply(write.object.summary$`LL Rep`, function(y) ifelse(isTRUE(y), "Yes", "No"))

    #...................
    ### Split results ####

    write.object.summary.split <- split(write.object.summary, f = write.object.summary$Folder)

    #...................
    ### Additional folder row ####

    write.temp <- NULL
    for (i in unique(write.object.summary$Folder)) {

      write.temp <- rbind(write.temp,
                          setNames(do.call(data.frame, list(i, rep(list(NA), times = ncol(write.object.summary) - 1L))), nm = colnames(write.object.summary)),
                          write.object.summary[write.object.summary$Folder == i, ])

    }

    write.object.summary <- write.temp

    # Duplicated folder entries
    write.object.summary[duplicated(write.object.summary$Folder), "Folder"] <- NA

    #...................
    ### Remove empty columns ####

    # write.object.summary <- write.object.summary[, which(apply(write.object.summary[-1L, ], 2L, function(y) !all(is.na(y))))]

    #...................
    ### List element names ####

    names(write.object.summary.split) <- abbreviate(names(write.object.summary.split), minlength = 1L)

    #...................
    ### Mean/Variance tables ####

    if (any(!is.na(write.object.mean.var))) {

      #### Round
      write.object.mean.var$n <- round(write.object.mean.var$n)
      write.object.mean$n <- round(write.object.mean$n)

      write.object.mean$low <- round(write.object.mean$low, digits = 3L)
      write.object.mean$upp <- round(write.object.mean$upp, digits = 3L)

      #### Numeric
      write.object.mean.var$class <- as.numeric(write.object.mean.var$class)
      write.object.mean$class <- as.numeric(write.object.mean$class)

      #### Column names
      colnames(write.object.mean.var) <- c("Folder", "#Class", "Class", "n", "Param", "Ind", "Est.", "SE", "z", "pval")
      colnames(write.object.mean) <- c("Folder", "#Class", "Class", "n", "Param", "Ind", "Est.", "SE", "z", "pval", "Low", "Upp")

      #### Variance table
      if (any(!is.na(write.object.var))) {

        # Round
        write.object.var$n <- round(write.object.var$n)
        # Numeric
        write.object.var$class <- as.numeric(write.object.var$class)
        # Column names
        colnames(write.object.var) <- c("Folder", "#Class", "Class", "n", "Param", "Ind", "Est.", "SE", "z", "pval")

      }

    }

    #...................
    ### Remove result tables ####

    # One subfolder
    if (isTRUE(length(write.object.summary.split) == 1L)) { write.object.summary.split <- NA }

    # Count variables
    if (isTRUE(all(is.na(write.object.var)) & any(!is.na(write.object.mean)))) { write.object.mean.var <- NA }

    #...................
    ### Return object ####

    # Combine result tables
    write.object <- Reduce(append, list(list(Summary = write.object.summary), write.object.summary.split,
                           list(Mean_Var = write.object.mean.var), list(Mean = write.object.mean), list(Var = write.object.var)))

    # Remove NA list elements
    write.object <- write.object[sapply(write.object, function(y) any(!is.na(y)))]

  #_____________________________________________________________________________
  #
  # Heteroscedasticity-Consistent Standard Errors, coef.robust() ---------------
  }, robust.coef = {

    #...................
    ### Coefficient result table ####

    write.coef <- write.object$coef

    # Round
    write.coef[, -4L] <- sapply(write.coef[, -4L], round, digits = digits)
    write.coef[, 4L] <- round(write.coef[, 4L], digits = p.digits)

    # Row names
    write.coef <- data.frame(row.names(write.coef), write.coef,
                             check.names = FALSE, fix.empty.names = FALSE)

    #...................
    ### F-test result table ####

    write.F <- NULL
    if (isTRUE(length(class(x$model)) == 1L)) {

      write.F <- write.object$F.test

      write.F[, 3L] <- sapply(write.F[, 3L], round, digits = digits)
      write.F[, 4L] <- round(write.F[, 4L], digits = p.digits)

    }

    #...................
    ### Sandwich result table ####

    write.sandwich <- write.object$sandwich

    write.sandwich <- round(write.sandwich, digits = digits)

    # row names
    write.sandwich <- data.frame(row.names(write.sandwich), write.sandwich,
                                 check.names = FALSE, fix.empty.names = FALSE)

    #...................
    ### Write object ####

    write.object <- list(coef = write.coef, F.test = write.F, sandwich = write.sandwich)

  #_____________________________________________________________________________
  #
  # Standardized Coefficients, coef.std() --------------------------------------
  }, std.coef = {

    #...................
    ### Coefficient result table ####

    write.coef <- write.object$coef

    # Round
    write.coef[, -4L] <- sapply(write.coef[, -4L], round, digits = digits)
    write.coef[, 4L] <- round(write.coef[, 4L], digits = p.digits)

    # Row names
    write.coef <- data.frame(row.names(write.coef), write.coef,
                             fix.empty.names = FALSE, check.names = FALSE)

    #...................
    ### Standard deviation ####

    write.sd <- data.frame(sd = round(write.object$sd, digits = digits))

    # Row names
    write.sd <- data.frame(row.names(write.sd), write.sd,
                           fix.empty.names = FALSE, check.names = FALSE)

    #...................
    ### Write object ####

    write.object <- list(coef = write.coef, sd = write.sd)

  })

  #_____________________________________________________________________________
  #
  # Write Excel file -----------------------------------------------------------

  misty::write.xlsx(write.object, file = file)

  return(invisible(write.object))

}
