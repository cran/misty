#' Item Analysis for Dichotomous, Polytomous, and Continuous Items
#'
#' This function conducts item analysis for dichotomous, polytomous, and continuous
#' items by computing corrected item-total correlations with confidence intervals
#' and coefficient alphas if the item is deleted. The default setting for computing
#' the item-total correlations is depending on the type of the data, i.e.,
#' point-biserial correlation for dichotomous data, polyserial correlation for
#' polytomous data, and product-moment correlation for continuous data.
#'
#' @param data        a data frame. Note that dichotomous items must be coded
#'                    with \code{0} and \code{1}, while polytomous should be
#'                    coded either as ordered factors or as integer values.
#'                    However, the function assumes continuous data whenever items
#'                    have more than two distinct values. Polytomous items can
#'                    be specified either by providing a data frame with ordered
#'                    factors or by specifying a data frame with numeric vectors
#'                    with integer values, along with setting the argument
#'                    \code{method} to \code{polyser}.
#' @param ...         an expression indicating the variable names in \code{data}
#'                    e.g., \code{item.stat(dat, x1, x2, x3)}. Note that the
#'                    operators \code{+}, \code{-}, \code{~}, \code{:}, \code{::},
#'                    and \code{!} can also be used to select variables, see
#'                    'Details' in the \code{\link{df.subset}} function.
#' @param exclude     a character vector indicating items to be excluded from
#'                    the analysis.
#' @param correct     logical: if \code{TRUE} (default), the corrected item-total
#'                    correlation is computed.
#' @param method      a character string indicating which correlation coefficient
#'                    is used for the item-total correlation, i.e., \code{"pearson"}
#'                    for the Pearson product-moment correlation coefficient when
#'                    data are continuous, \code{"pbiser"} for the point-biserial
#'                    correlation coefficient when data are dichotomous, \code{"biser"}
#'                    for the biserial correlation coefficient when data are dichotomous,
#'                    and \code{"polyser"} for the polyserial correlation coefficient
#'                    when data are polytomous. By default, the corrected item-total
#'                    point-biserial correlation (\code{"pbiser"}) is computed for
#'                    dichotomous items, corrected item-total polyserial correlation
#'                    (\code{"polyser"}) is computed for polytomous items, and
#'                    the corrected item-total product-moment correlation
#'                    (\code{"pearson"}) is computed for continuous items.
#' @param adjust      a character string specifying the non-normality adjustment
#'                    method (see 'Details' in the \code{\link{ci.cor}} function),
#'                    i.e., \code{"none"} for the Fisher \eqn{z'} confidence interval
#'                    for the Pearson product-moment correlation coefficient without
#'                    non-normality adjustment, \code{"joint"} for the confidence
#'                    interval with non-normality adjustment via sample joint moments,
#'                    and \code{"approx"} (default) for the confidence interval
#'                    with non-normality adjustment via approximate distribution
#'                    by skewness and kurtosis. Note that this argument only applies
#'                    to the Pearson product-moment correlation coefficient, i.e.,
#'                    \code{method = "pearson"}.
#' @param missing     a character string indicating how to deal with missing data
#'                    when computing coefficient alphas if the item is deleted,
#'                    i.e., \code{"listwise"} for listwise deletion, \code{"pairwise"}
#'                    (default) for pairwise deletion, \code{"fiml"} for full
#'                    information maximum likelihood method. Note that the
#'                    argument \code{na.omit} switches to \code{TRUE} when
#'                    specifying \code{missing = "listwise"}.
#' @param alternative a character string specifying the alternative hypothesis
#'                    for the confidence intervals for the item-total correlation,
#'                    i.e., \code{"two.sided"} (default), \code{"greater"} or
#'                    \code{"less"}.
#' @param conf.level  a numeric value between 0 and 1 indicating the confidence
#'                    level of the interval for the item-total correlation.
#' @param na.omit     logical: if \code{TRUE}, incomplete cases are removed
#'                    before conducting the analysis (i.e., listwise deletion).
#'                    By default pairwise deletion is used for computing point
#'                    biserial and product-moment correlation, while listwise
#'                    deletion is used for computing biserial and polyserial
#'                    correlation. Note that the argument \code{missing} switches
#'                    to \code{"listwise"} when specifying
#'                    \code{na.omit = TRUE}.
#' @param digits      an integer value indicating the number of decimal places
#'                    to be used to display item difficulty, mean, median, and
#'                    standard deviation.
#' @param r.digits    an integer value indicating the number of decimal places
#'                    to be used to display item-total correlation, and
#'                    coefficient alpha if the item is deleted.
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
#' @details
#' \describe{
#' \item{\strong{Confidence Interval for the Item-Total Correlation}}{The confidence
#' interval for the point-biserial correlation is based on Bonett (2020), while
#' the confidence interval for the biserial and polyserial correlation is based
#' on the maximum likelihood standard error provided by the function \code{polyserial}
#' from the \pkg{polycor} package (Fox, 2025). The confidence interval for the
#' Pearson product-moment correlation uses the \code{ci.cor} function from the
#' \pkg{misty} package that computes a confidence interval with non-normality
#' adjustment via approximate distribution by skewness and kurtosis by default.}
#' }
#'
#' @author
#' Takuya Yanagida
#'
#' @seealso
#' \code{\link{item.distract}}, \code{\link{item.alpha}}, \code{\link{item.omega}},
#' \code{\link{item.cfa}}, \code{\link{item.reverse}}, \code{\link{item.scores}}
#'
#' @references
#' Bonett D. G. (2020). Point-biserial correlation: Interval estimation, hypothesis
#' testing, meta-analysis, and sample size determination. \emph{The British Journal
#' of Mathematical and Statistical Psychology, 73} Suppl 1, 113–144.
#' https://doi.org/10.1111/bmsp.12189
#'
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
#' \item{\code{dtype}}{type of the data, i.e., \code{"dich"} for dichotomous,
#'                     \code{"poly"} for polytomous, and \code{"cont"} for
#'                     continuous data}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{result}}{result table}
#'
#' @note
#' This function uses a modified copy of the \code{polyserial()} function in the
#' \pkg{polycor} package by John Fox (2025) when requesting item-total biserial
#' correlation.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #————————————————————————————————————————————————————————————————————————————
#' # Dichotomous Data
#'
#' # Example 1a: Corrected item-total point-biserial correlation
#' item.stats(data.items, +ditem)
#'
#' # Example 1b: Corrected item-total biserial correlation
#' item.stats(data.items, +ditem, method = "biser")
#'
#' # Example 1c: Uncorrected item-total point-biserial correlation
#' item.stats(data.items, +ditem, correct = FALSE)
#'
#' # Example 1d: Display item-total correlation and coefficient alpha with 2 digits
#' item.stats(data.items, +ditem, r.digits = 2)
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Polytomous Data
#'
#' # Example 2a: Corrected item-total polyserial correlation
#' item.stats(data.items, pitem1, pitem2r, pitem3r, pitem4::pitem6, method = "polyser")
#'
#' # Example 2b: One-sided confidence intervals
#' item.stats(data.items, +pitem, method = "polyser", alternative = "greater")
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Continuous Data
#'
#' # Load data set "HolzingerSwineford1939" in the lavaan package
#' data("HolzingerSwineford1939", package = "lavaan")
#'
#' # Example 3a: Corrected item-total product-moment correlation
#' item.stats(HolzingerSwineford1939, x1::x9)
#'
#' # Example 3a: Exclude items 'x2' and 'x7'
#' item.stats(HolzingerSwineford1939, x1::x9, exclude = c("x2", "x7"))
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Write Results
#'
#' # Example 4a: Write Results into a text file
#' item.stats(data.items, +ditem, write = "Item-Stats.txt", output = FALSE)
#'
#' # Example 4b: Write Results into an Excel file
#' item.stats(data.items, +ditem, write = "Item-Stats.xlsx", output = FALSE)
#' }
item.stats <- function(data, ..., exclude = NULL, correct = TRUE,
                       method = c("pearson", "pbiser", "biser", "polyser"),
                       adjust = c("none", "joint", "approx"),
                       missing = c("listwise", "pairwise", "fiml"),
                       alternative = c("two.sided", "less", "greater"),
                       conf.level = 0.95, na.omit = FALSE, digits = 2,
                       r.digits = 3, as.na = NULL, write = NULL, append = TRUE,
                       check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Input Check ####

  # Check if input 'data' is missing or NULL
  if (isTRUE(missing(data) || is.null(data))) { stop("Please specify a data frame for the argument 'data'", call. = FALSE) }

  .check.input(logical = c("correct", "na.omit", "append", "output"), s.character = list(method = c("pearson", "pbiser", "biser", "polyser"), adjust = c("none", "joint", "approx"), missing = c("listwise", "pairwise", "fiml")),
               args = c("alternative", "digits", "r.digits", "conf.level"), envir = environment(), input.check = check)

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
  ## Numeric Items ####

  x <- .exclude.non.numeric(x, func = "item.stat", ordered = TRUE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Exclude Items ####

  if (isTRUE(!is.null(exclude))) { x <- x[, which(!colnames(x) %in% exclude)] |> (\(y) if (isTRUE(ncol(y) < 2L)) { stop("At least two items after excluding items are needed to compute item statistics.", call. = FALSE) } else { return(y) })() }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert User-missing Values into NA ####

  if (isTRUE(!is.null(as.na))) { x <- .as.na(x, na = as.na) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data Type ####

  #—————————————————————————————————————— #
  ### Dichotomous ####

  if (all(sapply(x, misty::uniq.n) %in% c(1L, 2L))) {

    sapply(x, function(y) any(!na.omit(y) %in% c(0L, 1L))) |> (\(p) if (isTRUE(any(p))) { stop(paste0("Dichotomous items are not always coded as 0 and 1: ", paste(names(x)[p], collapse = ", "))) } )()

    type <- "dicho"

  #—————————————————————————————————————— #
  ### Polytomous ####

  } else if (any(sapply(x, is.ordered))) {

    # Convert ordered factor into numeric
    if (isTRUE(any(sapply(x, is.ordered)))) { x <- as.data.frame(sapply(x, as.numeric)) }

    type <- "poly"

  #—————————————————————————————————————— #
  ### Continuous ####

  } else {

    type <- "cont"

  }

  #_____________________________________________________________________________
  #
  # Arguments ####

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'method' Argument ####

  if (isTRUE(all(c("pearson", "pbiser", "biser", "polyser") %in% method))) {

    method <- switch(type, "dicho" = "pbiser", "poly" = "polyser", "cont" = "pearson")

  } else {

    # Dichotomous data
    if (isTRUE(type == "dicho" && !method %in% c("pbiser", "biser"))) {

      stop("Pleae specify \"pbiser\" or \"biser\" for the argument 'method' when analyzing dichotomous data.", call. = FALSE)

    # Polyotomous data
    } else if (isTRUE(method == "polyser")) {

      sapply(x, function(y) any(y %% 1L != 0L)) |> (\(p) if (isTRUE(any(p))) { stop(paste0("Polytomous items are not always represented by integer values: ", paste(names(x)[p], collapse = ", "))) } )()

      type <- "poly"

    # Polytomous data
    } else if (isTRUE(type == "poly" && method != "polyser")) {

      stop("Pleae specify \"polyser\" for the argument 'method' when analyzing ordered-categorical data.", call. = FALSE)

    # Continuous data
    } else if (isTRUE(type == "cont" && method %in% c("pbiser", "biser"))) {

      stop("Pleae specify \"pearson\" for the argument 'method' when analyzing continuous data.", call. = FALSE)

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'adjust' Argument ####

  if (isTRUE(method == "pearson")) { adjust <- ifelse(all(c("none", "joint", "approx") %in% adjust), "approx", adjust) } else { adjust <- "none" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'missing' and 'na.omit' Argument ####

  #—————————————————————————————————————— #
  ### 'missing' ####

  if (isTRUE(any(is.na(x)))) {

    if (isTRUE(all(c("listwise", "pairwise", "fiml") %in% missing))) { missing <- "pairwise" }

  } else {

    missing <- "listwise"

  }

  #—————————————————————————————————————— #
  ### Consistency 'missing' and 'na.omit' ####

  if (isTRUE(missing == "listwise")) { na.omit <- TRUE }

  if (isTRUE(na.omit)) { missing <- "listwise" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'alternative' Argument ####

  if (isTRUE(all(c("two.sided", "less", "greater") %in% alternative))) { alternative <- "two.sided" }

  #_____________________________________________________________________________
  #
  # Main Function ####

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data Check ####

  # Exclude items with zero variance
  (sapply(x, var) == 0L) |> (\(p) if (isTRUE(any(p) != 0L)) {

    if (isTRUE(sum(p) == 1L)) { warning(paste0("Item with zero variance was excluded from the analysis: ", colnames(x)[p]), call. = FALSE) } else { warning(paste0("Items with zero variance were excluded from the analysis: ", paste(colnames(x)[p], collapse = ", ")), call. = FALSE) }

    x <<- x[, !p]

  })()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Listwise Deletion ####

  if (isTRUE(any(is.na(x)) && na.omit)) {

    (x <- na.omit(x)) |> (\(p) warning(paste0("Listwise deletion of incomplete cases, number of cases removed from the analysis: ", length(attributes(p)$na.action)) , call. = FALSE))()

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Item Statistics for Dichotomous Items ####

  itemstat <- switch(type, "dicho" = {

    suppressWarnings(misty::item.alpha(x, missing = missing, print = "item", check = FALSE, output = FALSE)$result) |>
      (\(p) {

        data.frame(item = p$itemstat$item, p$itemstat[, c("n", "nNA", "pNA")],
                   # Frequency of 0 and 1 categories
                   setNames(as.data.frame(do.call("rbind", lapply(x, table))), nm = c("n0", "n1")),
                   # Item difficulty and standard deviation
                   p = p$itemstat$m, sd =  sqrt(p$itemstat$m*(1L - p$itemstat$m)),
                   # Corrected item-total correlation, point-biserial or biserial correlation
                   if (isTRUE(correct)) {

                     setNames(do.call("rbind", lapply(seq_len(ncol(x)), function(y) .it.cor(x = x[, y], y = rowMeans(x[, -y, drop = FALSE], na.rm = TRUE), method = method, alternative = alternative, conf.level = conf.level))), nm = c("r", "low", "upp"))

                   # Uncorrected item-total correlation, point-biserial or biserial correlation
                   } else {

                     setNames(rowMeans(x, na.rm = TRUE) |> (\(p) do.call("rbind", lapply(x, function(y) .it.cor(x = y, y = p, method = method, alternative = alternative, conf.level = conf.level))))(), nm = c("r", "low", "upp"))

                   # Coefficient alpha if item deleted
                   }, data.frame(alpha = p$item$alpha, d.alpha = p$item$alpha - p$alpha$alpha), row.names = NULL)

        })()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Item Statistics for Polytomous Items ####

  }, "poly" = {

    suppressWarnings(misty::item.alpha(x, ordered = TRUE, missing = missing, print = "item", check = FALSE, output = FALSE)$result) |>
      (\(p) {

        data.frame(item = p$itemstat$item, p$itemstat[, c("n", "nNA", "pNA")],
                   # Frequency of categories
                   sort(unique(unlist(x))) |> (\(q) setNames(as.data.frame(do.call("rbind", lapply(x, function(y) table(factor(y, levels = q))))), nm = paste0("n", q)))(),
                   # Item difficulty and standard deviation
                   misty::descript(x, output = FALSE)$result[, c("m", "med", "sd", "min", "max")],
                   # Corrected item-total correlation, polyserial correlation
                   if (isTRUE(correct)) {

                     setNames(do.call("rbind", lapply(seq_len(ncol(x)), function(y) .it.cor(x = rowMeans(x[, -y, drop = FALSE], na.rm = TRUE), y = x[, y], method = method, alternative = alternative, conf.level = conf.level))), nm = c("r", "low", "upp"))

                   # Uncorrected item-total correlation, polyserial correlation
                   } else {

                     setNames(rowMeans(x, na.rm = TRUE) |> (\(p) do.call("rbind", lapply(x, function(y) .it.cor(x = p, y = y, method = method, alternative = alternative, conf.level = conf.level))))(), nm = c("r", "low", "upp"))

                   # Ordinal coefficient alpha if item deleted
                   }, data.frame(alpha = p$item$alpha, d.alpha = p$item$alpha - p$alpha$alpha), row.names = NULL)

      })()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Item Statistics for Continuous Items ####

  }, "cont" = {

    suppressWarnings(misty::item.alpha(x, missing = missing, print = "item", check = FALSE, output = FALSE)$result) |>
      (\(p) {

        data.frame(item = p$itemstat$item, p$itemstat[, c("n", "nNA", "pNA", "m", "sd", "min", "max")],
                   # Corrected item-total correlation, product-moment correlation
                   if (isTRUE(correct)) {

                     setNames(do.call("rbind", lapply(seq_len(ncol(x)), function(y) misty::ci.cor(data = data.frame(x[, y], rowMeans(x[, -y, drop = FALSE], na.rm = TRUE)), method = "pearson", adjust = adjust, alternative = alternative, conf.level = conf.level, check = FALSE, output = FALSE)$result[, c("cor", "low", "upp")])), nm = c("r", "low", "upp"))

                   # Uncorrected item-total correlation, product-moment correlation
                   } else {

                     setNames(rowMeans(x, na.rm = TRUE) |> (\(p) do.call("rbind", lapply(seq_len(ncol(x)), function(y) suppressWarnings(misty::ci.cor(data = data.frame(x[, y], p), method = "pearson", adjust = adjust, alternative = alternative, conf.level = conf.level, check = FALSE, output = FALSE))$result[, c("cor", "low", "upp")])))(), nm = c("r", "low", "upp"))

                   # Coefficient alpha if item deleted
                   }, data.frame(alpha = p$item$alpha, d.alpha = p$item$alpha - p$alpha$alpha), row.names = NULL)

      })()

  })

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Plot ####

  # if (isTRUE(plot)) {
  #
  #   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #   ## Dichotomous Items ####
  #
  #   switch(type, "dicho" = {
  #
  #     # MISSING DATA?
  #
  #     # Score groups
  #     breaks <- 3
  #     score.group <- cut(rowSums(x), breaks = breaks)
  #
  #     score.group <- factor(score.group,
  #                           labels = apply(table(score.group, rowSums(x)), 1L, function(y) names(which(y != 0)) |>
  #                                            (\(p) if (isTRUE(length(p) > 1L)) { paste0("[", p[1], ", ", p[length(p)], "]") } else {  paste0("[", p[1], "]") } )()))
  #
  #     prop <- sapply(lapply(x, factor, level = c(0L, 1L)), function(y) tapply(y, INDEX = score.group, function(z) prop.table(table(z))["1"] ))
  #
  #     plotdat <- data.frame(item = rep(colnames(prop), each = breaks),
  #                           sgroup = levels(score.group),
  #                           prop = as.vector(prop))
  #
  #     library(ggplot2)
  #
  #     ggplot(plotdat, aes(sgroup, prop, group = 1)) +
  #       geom_point() +
  #       geom_line() +
  #       facet_wrap(~ item)
  #
  #
  #     ggplot(plotdat, aes(sgroup, prop, group = item, color = item)) +
  #       geom_line() + geom_point()
  #
  #   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #   ## Polytomous Items ####
  #
  #   }, "poly" = {
  #
  #   })
  #
  # }

  #_____________________________________________________________________________
  #
  # Return Object ####

  object <- list(call = match.call(),
                 type = "item.stats",
                 data = x,
                 dtype = type,
                 args = list(exclude = exclude, correct = correct, method = method, adjust = adjust, missing = missing, alternative = alternative, conf.level = conf.level, na.omit = na.omit, digits = digits, r.digits = r.digits, as.na = as.na, write = write, append = append, check = check, output = output),
                 result = itemstat)

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
