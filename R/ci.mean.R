#' Confidence Interval for the Arithmetic Mean and Median
#'
#' The function \code{ci.mean} computes a confidence interval for the arithmetic
#' mean with known or unknown population standard deviation or population variance
#' and the function \code{ci.median} computes the confidence interval for the
#' median for one or more variables, optionally by a grouping and/or split variable.
#'
#' A difference-adjusted confidence interval (Baguley, 2012) for the arithmetic
#' mean can be computed by specifying \code{adjust = TRUE}.
#'
#' @param ...         a numeric vector, matrix or data frame with numeric variables,
#'                    i.e., factors and character variables are excluded from \code{x}
#'                    before conducting the analysis. Alternatively, an expression
#'                    indicating the variable names in \code{data} e.g.,
#'                    \code{ci.mean(x1, x2, data = dat)}. Note that the operators
#'                    \code{.}, \code{+}, \code{-}, \code{~}, \code{:}, \code{::},
#'                    and \code{!} can also be used to select variables, see 'Details'
#'                    in the \code{\link{df.subset}} function.
#' @param data        a data frame when specifying one or more variables in the
#'                    argument \code{...}. Note that the argument is \code{NULL}
#'                    when specifying a numeric vector, matrix or data frame for
#'                    the argument \code{...}.
#' @param sigma       a numeric vector indicating the population standard deviation
#'                    when computing confidence intervals for the arithmetic mean
#'                    with known standard deviation Note that either argument
#'                    \code{sigma} or argument \code{sigma2} is specified and it
#'                    is only possible to specify one value for the argument
#'                    \code{sigma} even though multiple variables are specified
#'                    in \code{x}.
#' @param sigma2      a numeric vector indicating the population variance when
#'                    computing confidence intervals for the arithmetic mean with
#'                    known variance. Note that either argument \code{sigma}
#'                    or argument \code{sigma2} is specified and it is only possible
#'                    to specify one value for the argument \code{sigma2} even
#'                    though multiple variables are specified in \code{x}.
#' @param adjust      logical: if \code{TRUE} (default), difference-adjustment
#'                    for the confidence intervals for the arithmetic mean is
#'                    applied.
#' @param alternative a character string specifying the alternative hypothesis,
#'                    must be one of \code{"two.sided"} (default), \code{"greater"}
#'                    or \code{"less"}.
#' @param conf.level  a numeric value between 0 and 1 indicating the confidence
#'                    level of the interval.
#' @param group       either a character string indicating the variable name of
#'                    the grouping variable in \code{...} or \code{data}, or a
#'                    vector representing the grouping variable. Note that a
#'                    grouping variable can only be used when computing confidence
#'                    intervals with unknown population standard deviation and
#'                    population variance.
#' @param split       either a character string indicating the variable name of
#'                    the split variable in \code{...} or \code{data}, or a vector
#'                    representing the split variable. Note that a grouping
#'                    variable can only be used when computing confidence intervals
#'                    with unknown population standard deviation and population
#'                    variance.
#' @param sort.var    logical: if \code{TRUE}, output table is sorted by variables
#'                    when specifying \code{group}.
#' @param na.omit     logical: if \code{TRUE}, incomplete cases are removed before
#'                    conducting the analysis (i.e., listwise deletion) when
#'                    specifying more than one outcome variable.
#' @param digits      an integer value indicating the number of decimal places to
#'                    be used.
#' @param as.na       a numeric vector indicating user-defined missing values,
#'                    i.e. these values are converted to \code{NA} before conducting
#'                    the analysis. Note that \code{as.na()} function is only
#'                    applied to \code{x}, but not to \code{group} or \code{split}.
#' @param write       a character string naming a text file with file extension
#'                    \code{".txt"} (e.g., \code{"Output.txt"}) for writing the
#'                    output into a text file.
#' @param append      logical: if \code{TRUE} (default), output will be appended
#'                    to an existing text file with extension \code{.txt} specified
#'                    in \code{write}, if \code{FALSE} existing text file will be
#'                    overwritten.
#' @param check       logical: if \code{TRUE} (default), argument specification is checked.
#' @param output      logical: if \code{TRUE} (default), output is shown on the console.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{test.z}}, \code{\link{test.t}}, \code{\link{ci.mean.diff}},
#' \code{\link{ci.prop}}, \code{\link{ci.var}}, \code{\link{ci.sd}},
#' \code{\link{descript}}
#'
#' @references
#' Baguley, T. S. (2012). \emph{Serious stats: A guide to advanced statistics for
#' the behavioral sciences}. Palgrave Macmillan.
#'
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology
#' - Using R and SPSS}. John Wiley & Sons.
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#' \tabular{ll}{
#' \code{call} \tab function call \cr
#' \code{type} \tab type of analysis \cr
#' \code{data} \tab list with the input specified in \code{...}, \code{data}, \code{group}, and \code{split} \cr
#' \code{args} \tab specification of function arguments \cr
#' \code{result} \tab result table \cr
#' }
#'
#' @export
#'
#' @examples
#' # Example 1a: Two-Sided 95% Confidence Interval for the Arithmetic Mean for 'mpg'
#' ci.mean(mtcars$mpg)
#'
#' # Example 1b: Alternative specification using the 'data' argument
#' ci.mean(mpg, data = mtcars)
#'
#' # Example 2: Two-Sided 95% Confidence Interval for the Median
#' ci.median(mtcars$mpg)
#'
#' # Example 3: Two-Sided 95% Difference-Adjusted Confidence Interval
#' ci.mean(mtcars$mpg, adjust = TRUE)
#'
#' # Example 4: Two-Sided 95% Confidence Interval with known standard deviation
#' ci.mean(mtcars$mpg, sigma = 1.2)
#'
#' # Example 5: Two-Sided 95% Confidence Interval with known variance
#' ci.mean(mtcars$mpg, sigma2 = 2.5)
#'
#' # Example 6: One-Sided 95% Confidence Interval
#' ci.mean(mtcars$mpg, alternative = "less")
#'
#' # Example 7: Two-Sided 99% Confidence Interval
#' ci.mean(mtcars$mpg, conf.level = 0.99)
#'
#' # Example 8: Two-Sided 95% Confidence Interval, print results with 3 digits
#' ci.mean(mtcars$mpg, digits = 3)
#'
#' # Example 9a: Two-Sided 95% Confidence Interval for 'mpg', 'cyl', and 'disp',
#' # listwise deletion for missing data
#' ci.mean(mtcars[, c("mpg", "cyl", "disp")], na.omit = TRUE)
#'
#' # Example 9b: Alternative specification using the 'data' argument
#' ci.mean(mpg:disp, data = mtcars, na.omit = TRUE)
#'
#' # Example 10a: Two-Sided 95% Confidence Interval, analysis by 'vs' separately
#' ci.mean(mtcars[, c("mpg", "cyl", "disp")], group = mtcars$vs)
#'
#' # Example 10b: Alternative specification using the 'data' argument
#' ci.mean(mpg:disp, data = mtcars, group = "vs")
#'
#' # Example 11: Two-Sided 95% Confidence Interval, analysis by 'vs' separately,
#' # sort by variables
#' ci.mean(mtcars[, c("mpg", "cyl", "disp")], group = mtcars$vs, sort.var = TRUE)
#'
#' # Example 12: Two-Sided 95% Confidence Interval, split analysis by 'am'
#' ci.mean(mtcars[, c("mpg", "cyl", "disp")], split = mtcars$am)
#'
#' # Example 13a: Two-Sided 95% Confidence Interval for 'mpg', 'cyl', and 'disp'
#' # analysis by 'vs' separately, split analysis by 'am'
#' ci.mean(mtcars[, c("mpg", "cyl", "disp")], group = mtcars$vs, split = mtcars$am)
#'
#' # Example 13b: Alternative specification using the 'data' argument
#' ci.mean(mpg:disp, data = mtcars, group = "vs", split = "am")
#'
#' \dontrun{
#' # Example 14: Write Results into a text file
#' ci.mean(mpg:disp, data = mtcars, group = "vs", split = "am", write = "Means.txt")
#' }
ci.mean <- function(..., data = NULL, sigma = NULL, sigma2 = NULL, adjust = FALSE,
                    alternative = c("two.sided", "less", "greater"), conf.level = 0.95,
                    group = NULL, split = NULL, sort.var = FALSE, na.omit = FALSE,
                    digits = 2, as.na = NULL, write = NULL, append = TRUE,
                    check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input '...' is missing
  if (isTRUE(missing(...))) { stop("Please specify the argument '...'.", call. = FALSE) }

  # Check if input '...' is NULL
  if (isTRUE(is.null(substitute(...)))) { stop("Input specified for the argument '...' is NULL.", call. = FALSE) }

  # Check if input 'data' is data frame
  if (isTRUE(!is.null(data) && !is.data.frame(data))) { stop("Please specify a data frame for the argument 'data'.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data using the argument 'data' ####

  if (isTRUE(!is.null(data))) {

    # Variable names
    var.names <- .var.names(..., data = data, group = group, split = split, check.chr = "a numeric vector, matrix or data frame")

    # Extract variables
    x <- data[, var.names]

    # Grouping variable
    if (isTRUE(!is.null(group))) { group <- data[, group] }

    # Split variable
    if (isTRUE(!is.null(split))) { split <- data[, split] }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data without using the argument 'data' ####

  } else {

    # Extract data
    x <- eval(..., enclos = parent.frame())

    # Data and cluster
    var.group <- .var.group(data = x, group = group, split = split)

    # Data
    if (isTRUE(!is.null(var.group$data)))  { x <- var.group$data }

    # Grouping variable
    if (isTRUE(!is.null(var.group$group))) { group <- var.group$group }

    # Split variable
    if (isTRUE(!is.null(var.group$split))) { group <- var.group$split }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data Frame ####

  x <- as.data.frame(x, stringsAsFactors = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) { x <- .as.na(x, na = as.na) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Numeric Variables ####

  # Non-numeric variables
  non.num <- !vapply(x, is.numeric, FUN.VALUE = logical(1L))

  if (isTRUE(any(non.num))) {

    x <- x[, -which(non.num), drop = FALSE]

    # Variables left
    if (isTRUE(ncol(x) == 0L)) { stop("No variables left for analysis after excluding non-numeric variables.", call. = FALSE) }

    warning(paste0("Non-numeric variables were excluded from the analysis: ", paste(names(which(non.num)), collapse = ", ")), call. = FALSE)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Listwise deletion ####

  if (isTRUE(na.omit && any(is.na(x)))) {

    #...................
    ### No group and split variable ####
    if (isTRUE(is.null(group) && is.null(split))) {

      x <- na.omit(as.data.frame(x, stringsAsFactors = FALSE))

      warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ", length(attributes(x)$na.action)), call. = FALSE)

    }

    #...................
    ### Group variable, no split variable ####
    if (isTRUE(!is.null(group) && is.null(split))) {

      x.group <- na.omit(data.frame(x, group = group, stringsAsFactors = FALSE))

      x <- x.group[, -grep("group", names(x.group)), drop = FALSE]
      group <- x.group$group

      warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ", length(attributes(x.group)$na.action)), call. = FALSE)

    }

    #...................
    ### No group variable, split variable ####
    if (isTRUE(is.null(group) && !is.null(split))) {

      x.split <- na.omit(data.frame(x, split = split, stringsAsFactors = FALSE))

      x <- x.split[, -grep("split", names(x.split)), drop = FALSE]
      split <- x.split$split

      warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ", length(attributes(x.split)$na.action)), call. = FALSE)

    }

    #...................
    ### Group variable, split variable ####
    if (isTRUE(!is.null(group) && !is.null(split))) {

      x.group.split <- na.omit(data.frame(x, group = group, split = split, stringsAsFactors = FALSE))

      x <- x.group.split[,  !names(x.group.split) %in% c("group", "split"), drop = FALSE]
      group <- x.group.split$group
      split <- x.group.split$split

      warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ", length(attributes(x.group.split)$na.action)), call. = FALSE)

    }

    #...................
    ### Variable with missing values only ####
    x.miss <- vapply(x, function(y) all(is.na(y)), FUN.VALUE = logical(1L))
    if (isTRUE(any(x.miss))) {

      stop(paste0("After listwise deletion, following variables are completely missing: ", paste(names(which(x.miss)), collapse = ", ")), call. = FALSE)

    }

  }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(isTRUE(check))) {

    # Check input 'sigma' and 'sigma2'
    if (isTRUE(!is.null(sigma) && !is.null(sigma2))) { stop("Please specify either argument 'sigma' or argument 'sigma2', but not both.", call. = FALSE) }

    # Check input 'sigma'
    if (isTRUE(!is.null(sigma))) {

      # SD smaller or equal 0
      if (isTRUE(any(sigma <= 0L))) { stop("Please specify a numeric value grater than 0 for the argument 'sigma'.", call. = FALSE) }

      # Length of 'sigma'
      if (isTRUE(length(sigma) != 1L)) { stop("Please specify a numeric value for the argument 'sigma'.", call. = FALSE) }

    }

    # Check input 'sigma2'
    if (isTRUE(!is.null(sigma2))) {

      # Variance smaller or equal 0
      if (isTRUE(any(sigma2 <= 0L))) { stop("Please specify a numeric value grater than 0 for the argument 'sigma2'.", call. = FALSE) }

      # Length of 'sigma2'
      if (isTRUE(length(sigma2) != 1L)) { stop("Please specify a numeric value for the argument 'sigma2'.", call. = FALSE) }

    }

    # Check input 'adjust'
    if (isTRUE(!is.logical(adjust))) { stop("Please specify TRUE or FALSE for the argument 'adjust'.", call. = FALSE) }

    # Check input 'alternative'
    if (isTRUE(!all(alternative %in% c("two.sided", "less", "greater")))) {

      stop("Character string in the argument 'alternative' does not match with \"two.sided\", \"less\", or \"greater\".", call. = FALSE)

    }

    # Check input 'conf.level'
    if (isTRUE(conf.level >= 1L || conf.level <= 0L)) { stop("Please specifiy a numeric value between 0 and 1 for the argument 'conf.level'.", call. = FALSE) }

    # Check input 'group'
    if (isTRUE(!is.null(group))) {

      # Population standard deviation
      if (isTRUE(!is.null(sigma))) { stop("Grouping variable cannot be used for confidence intervals with known population standard deviation.", call. = FALSE) }

      # Population variance
      if (isTRUE(!is.null(sigma2))) { stop("Grouping variable cannot be used for confidence intervals with known population variance.", call. = FALSE) }

      # Input 'group' completely missing
      if (isTRUE(all(is.na(group)))) { stop("The grouping variable specified in 'group' is completely missing.", call. = FALSE) }

      # Only one group in 'group'
      if (isTRUE(length(na.omit(unique(group))) == 1L)) { warning("There is only one group represented in the grouping variable specified in 'group'.", call. = FALSE) }

    }

    # Check input 'split'
    if (isTRUE(!is.null(split))) {

      # Population standard deviation
      if (isTRUE(!is.null(sigma))) { stop("Split variable cannot be used for confidence intervals with known population standard deviation.", call. = FALSE) }

      # Population variance
      if (isTRUE(!is.null(sigma2))) { stop("Split variable cannot be used for confidence intervals with known population variance.", call. = FALSE) }

      # Input 'split' completely missing
      if (isTRUE(all(is.na(split)))) { stop("The split variable specified in 'split' is completely missing.", call. = FALSE) }

      # Only one group in 'split'
      if (isTRUE(length(na.omit(unique(split))) == 1L)) { warning("There is only one group represented in the split variable specified in 'split'.", call. = FALSE) }

    }

    # Check input 'sort.var'
    if (isTRUE(!is.logical(sort.var))) { stop("Please specify TRUE or FALSE for the argument 'sort.var'.", call. = FALSE) }

    # Check input 'na.omit'
    if (isTRUE(!is.logical(na.omit))) { stop("Please specify TRUE or FALSE for the argument 'na.omit'.", call. = FALSE) }

    # Check input 'digits'
    if (isTRUE(digits %% 1L != 0L || digits < 0L)) { stop("Please specify a positive integer number for the argument 'digits'.", call. = FALSE) }

    # Check input 'write'
    if (isTRUE(!is.null(write) && substr(write, nchar(write) - 3L, nchar(write)) != ".txt")) { stop("Please specify a character string with file extenstion '.txt' for the argument 'write'.") }

    # Check input 'append'
    if (isTRUE(!is.logical(append))) { stop("Please specify TRUE or FALSE for the argument 'append'.", call. = FALSE) }

    # Check input 'output'
    if (isTRUE(!is.logical(output))) { stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Population standard deviation and variance ####

  if (isTRUE(is.null(sigma) && !is.null(sigma2))) { sigma <- sqrt(sigma2) }

  if (isTRUE(!is.null(sigma) && is.null(sigma2))) { sigma2 <- sigma^2 }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Alternative hypothesis ####

  if (isTRUE(all(c("two.sided", "less", "greater") %in% alternative))) {

    alternative <- "two.sided"

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Confidence interval for the mean ####

  m.conf <- function(x, sigma, adjust, alternative, conf.level, side) {

    # Data
    x <- na.omit(x)

    # Difference-adjustment factor
    adjust.factor <- ifelse(isTRUE(adjust), sqrt(2L) / 2L, 1L)

    # One observation or SD = 0
    if (isTRUE(length(x) <= 1L || sd(x) == 0L)) {

      ci <- c(NA, NA)

    # More than one observation
    } else {

      x.m <- mean(x)

      #...................
      ### Known population standard deviation ####

      if (isTRUE(!is.null(sigma))) {

        crit <- qnorm(switch(alternative,
                             two.sided = 1L - (1L - conf.level) / 2L,
                             less = conf.level,
                             greater = conf.level))

        se <- sigma / sqrt(length(na.omit(x)))

      #...................
      ### Unknown population standard deviation ####
      } else {

        crit <- qt(switch(alternative,
                          two.sided = 1L - (1L - conf.level) / 2L,
                          less = conf.level,
                          greater = conf.level), df = length(x) - 1L)

        se <- sd(x) / sqrt(length(x))

      }

      #...................
      ### Confidence interval ####
      ci <- switch(alternative,
                   two.sided = c(low = x.m - adjust.factor * crit * se,
                                 upp = x.m + adjust.factor * crit * se),
                   less = c(low = -Inf,
                            upp = x.m + adjust.factor * crit * se),
                   greater = c(low = x.m - adjust.factor * crit * se,
                               upp = Inf))

    }

    #...................
    ### Return object ####
    object <- switch(side, both = ci, low = ci[1L], upp = ci[2L])

    return(object)

  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## No Grouping, No Split ####
  if (isTRUE(is.null(group) && is.null(split))) {

    result <- data.frame(variable = colnames(x),
                         n = vapply(x, function(y) length(na.omit(y)), FUN.VALUE = 1L),
                         nNA = vapply(x, function(y) sum(is.na(y)), FUN.VALUE = 1L),
                         pNA = vapply(x, function(y) sum(is.na(y)) / length(y) * 100, FUN.VALUE = double(1)),
                         # Arithmetic mean
                         m = vapply(x, function(y) ifelse(length(na.omit(y)) <= 1L, NA, mean(y, na.rm = TRUE)), FUN.VALUE = double(1L)),
                         # Standard deviation
                         sd = vapply(x, function(y) ifelse(length(na.omit(y)) <= 1L, NA, sd(y, na.rm = TRUE)), FUN.VALUE = double(1L)),
                         # Confidence interval for the arithmetic mean
                         low = vapply(x, m.conf, sigma = sigma, adjust = adjust, alternative = alternative, conf.level = conf.level, side = "low", FUN.VALUE = double(1L)),
                         upp = vapply(x, m.conf, sigma = sigma, adjust = adjust, alternative = alternative, conf.level = conf.level, side = "upp", FUN.VALUE = double(1L)),
                         stringsAsFactors = FALSE, row.names = NULL, check.names = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##  Grouping, No Split ####
  } else if (isTRUE(!is.null(group) && is.null(split))) {

    object.group <- lapply(split(x, f = group), function(y) misty::ci.mean(y, data = NULL, sigma = NULL, sigma2 = NULL, adjust = adjust, alternative = alternative, conf.level = conf.level,
                                                                           group = NULL, split = NULL, sort.var = sort.var, na.omit = FALSE,
                                                                           as.na = NULL, check = FALSE, output = FALSE)$result)

    result <- data.frame(group = rep(names(object.group), each = ncol(x)),
                         eval(parse(text = paste0("rbind(", paste0("object.group[[", seq_len(length(object.group)), "]]", collapse = ", "), ")"))),
                         stringsAsFactors = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## No Grouping, Split ####
  } else if (isTRUE(is.null(group) && !is.null(split))) {

    result <- lapply(split(data.frame(x, stringsAsFactors = FALSE), f = split),
                     function(y) misty::ci.mean(y, data = NULL, sigma = NULL, sigma2 = NULL, adjust = adjust, alternative = alternative, conf.level = conf.level,
                                                group = NULL, split = NULL, sort.var = sort.var, na.omit = FALSE,
                                                as.na = NULL, check = FALSE, output = FALSE)$result)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Grouping, Split ####
  } else if (isTRUE(!is.null(group) && !is.null(split))) {

    result <- lapply(split(data.frame(x, group = group, stringsAsFactors = FALSE), f = split),
                     function(y) misty::ci.mean(y[, -grep("group", names(y))], data = NULL, sigma = NULL, sigma2 = NULL,
                                                adjust = adjust, alternative = alternative, conf.level = conf.level,
                                                group = y$group, split = NULL, sort.var = sort.var,
                                                na.omit = FALSE, as.na = NULL,
                                                check = FALSE, output = FALSE)$result)

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "ci", ci = "mean",
                 data = list(x = x, group = group, split = split),
                 args = list(sigma = sigma, sigma2 = sigma2, adjust = adjust,
                             alternative = alternative, conf.level = conf.level,
                             sort.var = sort.var, na.omit = na.omit, digits = digits,
                             as.na = as.na, write = write, append = append,
                             check = check, output = output),
                 result = result)

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Write results --------------------------------------------------------------

  if (isTRUE(!is.null(write))) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Text file ####

    # Send R output to textfile
    sink(file = write, append = ifelse(isTRUE(file.exists(write)), append, FALSE), type = "output", split = FALSE)

    if (append && isTRUE(file.exists(write))) { write("", file = write, append = TRUE) }

    # Print object
    print(object, check = FALSE)

    # Close file connection
    sink()

  }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}

#_______________________________________________________________________________
#_______________________________________________________________________________

#' @rdname ci.median
ci.median <- function(..., data = NULL, alternative = c("two.sided", "less", "greater"),
                      conf.level = 0.95, group = NULL, split = NULL, sort.var = FALSE,
                      na.omit = FALSE, digits = 2, as.na = NULL, write = NULL, append = TRUE,
                      check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input '...' is missing
  if (isTRUE(missing(...))) { stop("Please specify the argument '...'.", call. = FALSE) }

  # Check if input '...' is NULL
  if (isTRUE(is.null(substitute(...)))) { stop("Input specified for the argument '...' is NULL.", call. = FALSE) }

  # Check if input 'data' is data frame
  if (isTRUE(!is.null(data) && !is.data.frame(data))) { stop("Please specify a data frame for the argument 'data'.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data using the argument 'data' ####

  if (isTRUE(!is.null(data))) {

    # Variable names
    var.names <- .var.names(..., data = data, group = group, split = split, check.chr = "a numeric vector, matrix or data frame")

    # Extract variables
    x <- data[, var.names]

    # Grouping variable
    if (isTRUE(!is.null(group))) { group <- data[, group] }

    # Splitting variable
    if (isTRUE(!is.null(split))) { split <- data[, split] }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data without using the argument 'data' ####

  } else {

    # Extract data
    x <- eval(..., enclos = parent.frame())

    # Data and cluster
    var.group <- .var.group(data = x, group = group, split = split)

    if (isTRUE(!is.null(var.group$data)))  { x <- var.group$data }
    if (isTRUE(!is.null(var.group$group))) { group <- var.group$group }
    if (isTRUE(!is.null(var.group$split))) { group <- var.group$split }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data frame ####

  x <- as.data.frame(x, stringsAsFactors = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) { x <- .as.na(x, na = as.na) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Numeric Variables ####

  non.num <- !vapply(x, is.numeric, FUN.VALUE = logical(1L))

  if (isTRUE(any(non.num))) {

    x <- x[, -which(non.num), drop = FALSE]

    #......
    # Variables left

    if (isTRUE(ncol(x) == 0L)) { stop("No variables left for analysis after excluding non-numeric variables.", call. = FALSE) }

    warning(paste0("Non-numeric variables were excluded from the analysis: ", paste(names(which(non.num)), collapse = ", ")),
            call. = FALSE)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Listwise deletion ####

  if (isTRUE(na.omit && any(is.na(x)))) {

    #......
    # No group and split variable
    if (isTRUE(is.null(group) && is.null(split))) {

      x <- na.omit(as.data.frame(x, stringsAsFactors = FALSE))

      warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ",
                     length(attributes(x)$na.action)), call. = FALSE)

    #......
    # Group variable, no split variable
    } else  if (isTRUE(!is.null(group) && is.null(split))) {

      x.group <- na.omit(data.frame(x, group = group, stringsAsFactors = FALSE))

      x <- x.group[, -grep("group", names(x.group)), drop = FALSE]
      group <- x.group$group

      warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ",
                     length(attributes(x.group)$na.action)), call. = FALSE)

    #......
    # No group variable, split variable
    } else  if (isTRUE(is.null(group) && !is.null(split))) {

      x.split <- na.omit(data.frame(x, split = split, stringsAsFactors = FALSE))

      x <- x.split[, -grep("split", names(x.split)), drop = FALSE]
      split <- x.split$split

      warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ",
                     length(attributes(x.split)$na.action)), call. = FALSE)

    #......
    # Group variable, split variable
    } else if (isTRUE(!is.null(group) && !is.null(split))) {

      x.group.split <- na.omit(data.frame(x, group = group, split = split, stringsAsFactors = FALSE))

      x <- x.group.split[,  !names(x.group.split) %in% c("group", "split"), drop = FALSE]
      group <- x.group.split$group
      split <- x.group.split$split

      warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ",
                     length(attributes(x.group.split)$na.action)), call. = FALSE)

    }

    #......
    # Check variable with missing values only
    x.miss <- vapply(x, function(y) all(is.na(y)), FUN.VALUE = logical(1L))
    if (isTRUE(any(x.miss))) {

      stop(paste0("After listwise deletion, following variables are completely missing: ",
                  paste(names(which(x.miss)), collapse = ", ")), call. = FALSE)

    }

    #......
    # Check group
    if (isTRUE(!is.null(group))) {

      # Input 'group' completely missing
      if (isTRUE(all(is.na(group)))) { stop("The grouping variable specified in 'group' is completely missing.", call. = FALSE) }

      # Only one group in 'group'
      if (isTRUE(length(na.omit(unique(group))) == 1L)) { warning("There is only one group represented in the grouping variable specified in 'group'.", call. = FALSE) }

    }

    #......
    # Check split
    if (isTRUE(!is.null(split))) {

      # Input 'split' completely missing
      if (isTRUE(all(is.na(split)))) { stop("The split variable specified in 'split' is completely missing.", call. = FALSE) }

      # Only one group in 'split'
      if (isTRUE(length(na.omit(unique(split))) == 1L)) { warning("There is only one group represented in the split variable specified in 'split'.", call. = FALSE) }

    }

  }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # Check input 'alternative'
    if (isTRUE(!all(alternative %in%  c("two.sided", "less", "greater")))) {

      stop("Character string in the argument 'alternative' does not match with \"two.sided\", \"less\", or \"greater\".", call. = FALSE)

    }

    # Check input 'conf.level'
    if (isTRUE(conf.level >= 1L || conf.level <= 0L)) { stop("Please specifiy a numeric value between 0 and 1 for the argument 'conf.level'.", call. = FALSE) }

    # Check input 'sort.var'
    if (isTRUE(!is.logical(sort.var))) { stop("Please specify TRUE or FALSE for the argument 'sort.var'.", call. = FALSE) }

    # Check input 'na.omit'
    if (isTRUE(!is.logical(na.omit))) { stop("Please specify TRUE or FALSE for the argument 'na.omit'.", call. = FALSE) }

    # Check input 'digits'
    if (isTRUE(digits %% 1L != 0L || digits < 0L)) { stop("Please specify a positive integer number for the argument 'digits'.", call. = FALSE) }

    # Check input output
    if (isTRUE(!is.logical(output))) { stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Alternative hypothesis ####

  if (isTRUE(all(c("two.sided", "less", "greater") %in% alternative))) {

    alternative <- "two.sided"

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Confidence interval for the median ####

  med.conf <- function(x,  alternative, conf.level, side) {

    #......
    # Data
    x <- na.omit(x)

    n <- length(x)

    #......
    # Number of observations less than 6 observations
    if (isTRUE(n < 6L)) {

      ci <- c(NA, NA)

    #......
    # At least six observations
    } else {

      # Two-sided CI
      switch(alternative, two.sided = {

        k <- qbinom((1L - conf.level)/2L, size = n, prob = 0.5, lower.tail = TRUE)

        ci <- sort(x)[c(k, n - k + 1L)]

      # One-sided CI: less
      }, less = {

        k <- qbinom(1L - 2L * (1L - conf.level), size = n, prob = 0.5, lower.tail = TRUE)

        ci <- c(-Inf, sort(x)[k])

      # One-sided CI: greater
      }, greater = {

        k <- qbinom(1L - 2L * (1L - conf.level), size = n, prob = 0.5, lower.tail = FALSE)

        ci <- c(sort(x)[k], Inf)

      })

    }

    #......
    # Return object

    # Lower or upper limit
    object <- switch(side, both = ci, low = ci[1L], upp = ci[2L])

    return(object)

  }

  #...................
  ### No Grouping, No Split ####

  if (isTRUE(is.null(group) && is.null(split))) {

    result <- data.frame(variable = colnames(x),
                         n = vapply(x, function(y) length(na.omit(y)), FUN.VALUE = 1L),
                         nNA = vapply(x, function(y) sum(is.na(y)), FUN.VALUE = 1L),
                         pNA = vapply(x, function(y) sum(is.na(y)) / length(y) * 100L, FUN.VALUE = double(1)),
                         # Median
                         med = vapply(x, function(y) ifelse(length(na.omit(y)) <= 1L, NA, median(y, na.rm = TRUE)), FUN.VALUE = double(1L)),
                         # Interquartil range
                         iqr = vapply(x, function(y) ifelse(length(na.omit(y)) <= 1L, NA, IQR(y, na.rm = TRUE)), FUN.VALUE = double(1L)),
                         # Confidence interval for the median
                         low = vapply(x, med.conf, alternative = alternative, conf.level = conf.level, side = "low", FUN.VALUE = double(1L)),
                         upp = vapply(x, med.conf, alternative = alternative, conf.level = conf.level, side = "upp", FUN.VALUE = double(1L)),
                         stringsAsFactors = FALSE, row.names = NULL, check.names = FALSE)

  #...................
  ### Grouping, No Split ####

  } else if (isTRUE(!is.null(group) && is.null(split))) {

    object.group <- lapply(split(x, f = group), function(y) misty::ci.median(y, data = NULL, alternative = alternative, conf.level = conf.level,
                                                                             group = NULL, split = NULL, sort.var = sort.var, na.omit = FALSE,
                                                                             as.na = NULL, check = FALSE, output = FALSE)$result)

    result <- data.frame(group = rep(names(object.group), each = ncol(x)),
                         eval(parse(text = paste0("rbind(", paste0("object.group[[", seq_len(length(object.group)), "]]", collapse = ", "), ")"))),
                         stringsAsFactors = FALSE)

  #...................
  ### No Grouping, Split ####

  } else if (isTRUE(is.null(group) && !is.null(split))) {

    result <- lapply(split(data.frame(x, stringsAsFactors = FALSE), f = split),
                     function(y) misty::ci.median(y, data = NULL, alternative = alternative, conf.level = conf.level,
                                                  group = NULL, split = NULL, sort.var = sort.var, na.omit = FALSE,
                                                  as.na = NULL, check = FALSE, output = FALSE)$result)

  #...................
  ### Grouping, Split ####

  } else if (isTRUE(!is.null(group) && !is.null(split))) {

    result <- lapply(split(data.frame(x, group = group, stringsAsFactors = FALSE), f = split),
                     function(y) misty::ci.median(y[, -grep("group", names(y))],
                                                  alternative = alternative, conf.level = conf.level,
                                                  group = y$group, split = NULL, sort.var = sort.var,
                                                  na.omit = FALSE, as.na = NULL,
                                                  check = FALSE, output = FALSE)$result)

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "ci", ci = "median",
                 data = list(x = x, group = group, split = split),
                 args = list(alternative = alternative, conf.level = conf.level,
                             sort.var = sort.var, na.omit = na.omit, digits = digits, as.na = as.na,
                             write = write, append = append, check = check, output = output),
                 result = result)

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Write results --------------------------------------------------------------

  if (isTRUE(!is.null(write))) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Text file ####

    # Send R output to textfile
    sink(file = write, append = ifelse(isTRUE(file.exists(write)), append, FALSE), type = "output", split = FALSE)

    if (isTRUE(append && file.exists(write))) { write("", file = write, append = TRUE) }

    # Print object
    print(object, check = FALSE)

    # Close file connection
    sink()

  }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
