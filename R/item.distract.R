#' Distractor Analysis for Multiple-Choice Items
#'
#' This function conducts distractor analysis for multiple-choice test items,
#' i.e., computes attractor-distractor selection frequency, percentage, and total
#' point-biserial or biserial correlation.
#'
#' @param data        a data frame. Note that missing values should be represented
#'                    with \code{NA} and not with a valid code. The argument
#'                    \code{as.na} can be used to convert values to \code{NA}
#'                    before conducting the analysis.
#' @param ...         an expression indicating the variable names in \code{data}
#'                    e.g., \code{item.distract(dat, x1, x2, x3)}. Note that the
#'                    operators \code{+}, \code{-}, \code{~}, \code{:}, \code{::},
#'                    and \code{!} can also be used to select variables, see
#'                    'Details' in the \code{\link{df.subset}} function.
#' @param key         a numeric or character vector of the answer key with a length
#'                    matching the number of variables specified in the argument
#'                    \code{data} or selected using the argument \code{...}.
#' @param exclude     a character vector indicating items to be excluded from
#'                    the analysis.
#' @param na.keep     logical: if \code{FALSE} (default), missing values are coded
#'                    as \code{0} when scoring items (i.e., item not solved) and
#'                    creating groups (i.e., answer option not selected).
#' @param correct     logical: if \code{TRUE} (default), the corrected distractor-total
#'                    correlation is computed.
#' @param method      a character string indicating which correlation coefficient
#'                    is used for the item-total correlation, i.e., \code{"pbiser"}
#'                    for the point-biserial correlation coefficient when data
#'                    are dichotomous, \code{"biser"} for the biserial correlation
#'                    coefficient.
#' @param ml          logical: if \code{FALSE} (default), a two-step approximation
#'                    is used to compute the biserial correlation coefficient
#'                    (\code{method = "biser"}), while the maximum-likelihood (ML)
#'                    estimate is computed if \code{TRUE}. Note that ML estimation
#'                    is computationally expensive, i.e., takes a lot of time.
#' @param print       a character vector indicating which results to print, i.e.,
#'                    \code{"all"} for all results, \code{"nUQ"} for the number
#'                    of unique elements after omitting missing values, \code{"key"}
#'                    for the answer key, \code{"n"} for the distractor selection
#'                    frequency, \code{"perc"} for the distractor selection percentage,
#'                    and \code{"r"} for the (corrected) distractor-total correlation.
#' @param color       a character string indicating the text color for highlighting
#'                    the attractor frequency, percentage, and attractor-total
#'                    correlation, i.e., \code{"default"} (default) for the default
#'                    text color without color coding and various text colors for
#'                    highlighting like \code{"red"}, \code{"b.red"}, \code{"green"},
#'                    \code{"b.green"}, \code{"blue"}, or \code{"b.blue"}, see
#'                    the help page of the \code{\link{chr.color}} function.
#'                    Note that this option is not supported when using R Markdown
#'                    and when writing the output into a text file (\code{.txt}).
#' @param style       a character vector indicating the font style, i.e.,
#'                    \code{"regular"} (default) for regular text, \code{"bold"}
#'                    for bold text, and \code{"italic"} for italic text. Note
#'                    that the font style \code{"bold"} and \code{"italic"} can
#'                    be combined, i.e., style = c("bold", "italic") provides a
#'                    bold and italic text. Note that the argument \code{color}
#'                    needs to be specified to change the style of the text, e.g.
#'                    \code{color = "black"} and \code{style = "bold"} to for
#'                    bold text.
#' @param na.omit     logical: if \code{TRUE}, incomplete cases are removed
#'                    before conducting the analysis (i.e., listwise deletion).
#' @param digits      an integer value indicating the number of decimal places
#'                    to be used to display descriptive statistics, e.g., mean,
#'                    median, and standard deviation.
#' @param r.digits    an integer value indicating the number of decimal places
#'                    to be used to display the item difficulty, item-total
#'                    correlation, and the coefficient alpha if the item is deleted.
#' @param as.na       a numeric vector indicating user-defined missing values,
#'                    i.e. these values are converted to \code{NA} before conducting
#'                    the analysis.
#' @param write       a character string naming a file for writing the output into
#'                    either a text file with file extension \code{".txt"} (e.g.,
#'                    \code{"Output.txt"}) or Excel file with file extension
#'                    \code{".xlsx"}  (e.g., \code{"Output.xlsx"}). If the file
#'                    name does not contain any file extension, an Excel file will
#'                    be written.
#' @param append      logical: if \code{TRUE} (default), output will be appended
#'                    to an existing text file with extension \code{.txt} specified
#'                    in \code{write}, if \code{FALSE} existing text file will be
#'                     overwritten.
#' @param check       logical: if \code{TRUE} (default), argument specification
#'                    is checked.
#' @param output      logical: if \code{TRUE} (default), output is shown.
#'
#' @author
#' Takuya Yanagida
#'
#' @seealso
#' \code{\link{item.stats}}, \code{\link{item.alpha}}, \code{\link{item.omega}},
#' \code{\link{item.cfa}}, \code{\link{item.reverse}}, \code{\link{item.scores}}
#'
#' @references
#' Fox, J. (2025). \emph{polycor: Polychoric and polyserial correlations}.
#' R package version 0.8-2. https://doi.org/10.32614/CRAN.package.polycor
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#'
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{data}}{data frame including all variables used in the analysis}
#' \item{\code{key}}{list with the answer key and the position of the answer key
#'                   for each item}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{result}}{result table}
#'
#' @note
#' This function uses a modified copy of the \code{polyserial()} function in the
#' \pkg{polycor} package by John Fox when requesting distractor-total biserial
#' correlation.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #————————————————————————————————————————————————————————————————————————————
#' # Multiple-Choice Items
#'
#' # Answer key
#' key <- c("D", "B", "C", "C", "B", "B", "B", "A", "A", "D")
#'
#' # Example 1a: Corrected distractor-total point-biserial correlation
#' item.distract(data.items, +mitem, key = key)
#'
#' # Example 1b: Corrected distractor-total biserial correlation
#' item.distract(data.items, +mitem, key = key, method = "biser")
#'
#' # Example 1c: Keep missing values, i.e., do not convert NA into 0
#' item.distract(data.items, +mitem, key = key, na.keep = TRUE)
#'
#' # Example 1d: Print corrected distractor-total point-biserial correlation only
#' item.distract(data.items, +mitem, key = key, print = "r")
#'
#' # Example 1e: Highlight attractor frequency, percentage, and correlation in bright red
#' item.distract(data.items, +mitem, key = key, na.keep = TRUE, color = "b.red")
#'
#' # Example 1f: Highlight attractor frequency, percentage, and correlation in boldface
#' item.distract(data.items, +mitem, key = key, na.keep = TRUE, color = "black",
#'               style = "bold")
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Write Results
#'
#' # Example 2a: Write Results into a text file
#' item.distract(data.items, +mitem, key = key, write = "Item-Distract.txt", output = FALSE)
#'
#' # Example 2b: Write Results into an Excel file
#' item.distract(data.items, +mitem, key = key, write = "Item-Distracts.xlsx", output = FALSE)
#' }
item.distract <- function(data, ..., key, exclude = NULL, na.keep = FALSE,
                          correct = TRUE, method = c("pbiser", "biser"),
                          ml = FALSE, print = c("all", "nUQ", "key", "n", "perc", "r"),
                          color = "default", style = c("regular", "bold", "italic"),
                          na.omit = FALSE, digits = 1, r.digits = 3, as.na = NULL,
                          write = NULL, append = TRUE, check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Input Check ####

  # Check if input 'data' is missing or NULL
  if (isTRUE(missing(data) || is.null(data))) { stop("Please specify a data frame for the argument 'data'", call. = FALSE) }

  .check.input(logical = c("na.keep", "na.omit", "append", "output"), s.character = list(method = c("pbiser", "biser"), style = c("regular", "bold", "italic")), args = c("color", "digits", "r.digits"), envir = environment(), input.check = check)

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  # Using the Argument '...'
  if (isTRUE(!missing(...))) {

    x <- as.data.frame(data[, .var.names(data = data, ...), drop = FALSE])

  # Without Using the Argument '...'
  } else {

    x <- as.data.frame(data)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Data and Key Check

  if (isTRUE(check)) {

    # Length of the key
    if (isTRUE(ncol(x) != length(key))) { stop("The length of the key specified in the argument 'key' does not match with the number of items.", call. = FALSE) }

    # Response matching answer key
    which(sapply(seq_along(key), function(y) !key[y] %in% unique(x[, y]))) |> (\(p) if (isTRUE(length(p) > 0L)) { warning(paste0(ifelse(length(p) == 1L, "Item ", "Items "), "with no response matching the answer key: ", paste(colnames(x)[p], collapse = ", ")), call. = FALSE) })()

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Exclude Items ####

  (which(!colnames(x) %in% exclude)) |>
    (\(p) {

      x <<- x[, p]
      key <<- key[p]

      if (isTRUE(p < 2L)) { stop("At least two items after excluding items are needed to conduct distractor analysis.", call. = FALSE) }

    })()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Factors ####

  if (isTRUE(any(!sapply(x, is.factor)))) { x <- data.frame(lapply(x, function(y) factor(y, levels = sort(unique(y)), labels = sort(unique(y))))) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Key Position ####

  key.pos <- sapply(seq_len(ncol(x)), function(y) which(levels(x[, y]) %in% key[y]) |> (\(p) ifelse(length(p) == 0L, NA, p))() )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert User-missing Values into NA ####

  if (isTRUE(!is.null(as.na))) { x <- .as.na(x, na = as.na) }

  #_____________________________________________________________________________
  #
  # Arguments ####

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'method' Argument ####

  if (isTRUE(all(c("pbiser", "biser") %in% method))) { method <- "pbiser" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'print' Argument ####

  if (isTRUE(all(c("all", "nUQ", "key", "n", "perc", "r") %in% print))) { print <- c("nUQ", "key", "n", "perc", "r") }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'style' Argument ####

  if (isTRUE(all(c("regular", "bold", "italic") %in% style))) { style <- "regular" }

  #_____________________________________________________________________________
  #
  # Main Function ####

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Listwise Deletion ####

  if (isTRUE(na.omit && any(is.na(x)))) { (x <- na.omit(x)) |> (\(p) warning(paste0("Listwise deletion of incomplete cases, number of cases removed from the analysis: ", length(attributes(p)$na.action)) , call. = FALSE))() }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Distractor Selection Frequency ####

  restab.freq <- NULL
  if (isTRUE("n" %in% print)) { restab.freq <- do.call(misty::df.rbind, lapply(x, function(y) (table(y, useNA = "always"))|> (\(p) as.data.frame(matrix(p, ncol = length(p), dimnames = list(NULL, c(names(p)[-length(p)], "NA")))))())) |> (\(q) setNames(q, nm = paste0("n", colnames(q))))() }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Distractor Selection Percentages ####

  restab.perc <- NULL
  if (isTRUE("perc" %in% print)) { restab.perc <- do.call(misty::df.rbind, lapply(x, function(y) (prop.table(table(y, useNA = "always")) * 100L) |> (\(p) as.data.frame(matrix(p, ncol = length(p), dimnames = list(NULL, c(names(p)[-length(p)], "NA")))))())) |> (\(q) setNames(q, nm = paste0("p", colnames(q))))() }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Score Items ####

  restab.r <- NULL
  if (isTRUE("r" %in% print)) {

    #—————————————————————————————————————— #
    ### Keep NA ####

    if (isTRUE(na.keep)) {

      x.scored <- setNames(as.data.frame(sapply(seq_len(ncol(x)), function(y) ifelse(x[, y] == key[y], 1L, 0L))), nm = colnames(x))

    #—————————————————————————————————————— #
    ### Replace NA with 0 ####

    } else {

      x.scored <- setNames(as.data.frame(sapply(seq_len(ncol(x)), function(y) misty::na.as(ifelse(x[, y] == key[y], 1L, 0L), na = 0L, check = FALSE))), nm = colnames(x))

    }

    #—————————————————————————————————————— #
    ### Mean Score ####

    score.mean <- NULL
    if (isTRUE(!correct)) { score.mean <- rowMeans(x.scored, na.rm = TRUE) }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Distractor Total Correlation ####

    restab.r <- lapply(seq_len(ncol(x)), function(y) sapply(levels(x[, y]), function(z) {

      #—————————————————————————————————————— #
      ### Keep NA ####

      if (isTRUE(na.keep)) {

        # Corrected distractor-total correlation
        if (isTRUE(correct)) {

          .adt.cor(x = ifelse(x[, y] == z, 1L, 0L), y = rowMeans(x.scored[, -y], na.rm = TRUE), method = method, ml = ml)

        # Uncorrected distractor-total correlation
        } else {

          .adt.cor(x = ifelse(x[, y] == z, 1L, 0L), y = score.mean, method = method, ml = ml)

        }

      #—————————————————————————————————————— #
      ### Replace NA with Group 0 ####

      } else {

        # Corrected distractor-total correlation
        if (isTRUE(correct)) {

          .adt.cor(x = misty::na.as(ifelse(x[, y] == z, 1L, 0L), na = 0L, check = FALSE), y = rowMeans(x.scored[, -y], na.rm = TRUE), method = method, ml = ml)

        # Uncorrected distractor-total correlation
        } else {

          .adt.cor(x = misty::na.as(ifelse(x[, y] == z, 1L, 0L), na = 0L, check = FALSE), y = score.mean, method = method, ml = ml)

        }

      }

    })) |> (\(p) do.call(misty::df.rbind, lapply(p, function(y) as.data.frame(matrix(y, ncol = length(y), dimnames = list(NULL, names(y)))))))() |> (\(q) setNames(q, nm = paste0("r", colnames(q))))()


  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Combine Result Tables ####

  restab <- data.frame(item = names(x),
                       n = vapply(x, function(y) sum(!is.na(y)), FUN.VALUE = integer(1L)),
                       nUQ = if (isTRUE("nUQ" %in% print)) { vapply(x, function(y) misty::uniq.n(y), FUN.VALUE = integer(1L)) } else { rep(NA, times = length(names(x))) },
                       key = if (isTRUE("key" %in% print)) { key } else { rep(NA, times = length(names(x)))  },
                       if (isTRUE(!is.null(restab.freq))) { restab.freq } else { rep(NA, times = length(names(x))) },
                       if (isTRUE(!is.null(restab.perc))) { restab.perc } else { rep(NA, times = length(names(x))) },
                       if (isTRUE(!is.null(restab.r))) { restab.r } else { rep(NA, times = length(names(x))) },
                       row.names = NULL) |> (\(p) p[, sapply(p, function(y) any(!is.na(y)))])()

  #_____________________________________________________________________________
  #
  # Return Object ####

  object <- list(call = match.call(),
                 type = "item.distract",
                 data = x,
                 key = list(key = key, keypos = key.pos),
                 args = list(na.keep = na.keep, correct = correct, method = method, ml = ml, print = print, color = color, style = style, na.omit = na.omit, digits = digits, r.digits = r.digits, as.na = as.na, write = write, append = append, check = check, output = output),
                 result = restab)

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Write Results --------------------------------------------------------------

  if (isTRUE(!is.null(write))) { .write.result(object = object, write = write, append = append) }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  if (isTRUE(output)) { print(object, check = FALSE) }

  #_____________________________________________________________________________
  #
  # Return ---------------------------------------------------------------------

  return(invisible(object))

}

#_______________________________________________________________________________
