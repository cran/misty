#' Confidence Interval for the Variance and the Standrad Deviation
#'
#' The function \code{ci.var} computes the confidence interval for the variance,
#' and the function \code{ci.sd} computes the confidence interval for the standard
#' deviation for one or more variables, optionally by a grouping and/or split variable.
#'
#' The confidence interval based on the chi-square distribution is computed by
#' specifying \code{method = "chisq"}, while the Bonett (2006) confidence interval
#' is requested by specifying \code{method = "bonett"}. By default, the Bonett
#' confidence interval interval is computed which performs well under moderate
#' departure from normality, while the confidence interval based on the chi-square
#' distribution is highly sensitive to minor violations of the normality assumption
#' and its performance does not improve with increasing sample size. Note that at
#' least four valid observations are needed to compute the Bonett confidence interval.
#'
#' @param ...         a numeric vector, matrix or data frame with numeric variables,
#'                    i.e., factors and character variables are excluded from \code{x}
#'                    before conducting the analysis. Alternatively, an expression
#'                    indicating the variable names in \code{data} e.g.,
#'                    \code{ci.var(x1, x2, x3, data = dat)}. Note that the operators
#'                    \code{.}, \code{+}, \code{-}, \code{~}, \code{:}, \code{::},
#'                    and \code{!} can also be used to select variables, see 'Details'
#'                    in the \code{\link{df.subset}} function.
#' @param data        a data frame when specifying one or more variables in the
#'                    argument \code{...}. Note that the argument is \code{NULL}
#'                    when specifying a numeric vector, matrix or data frame for
#'                    the argument \code{...}.
#' @param method      a character string specifying the method for computing the
#'                    confidence interval, must be one of \code{"chisq"}, or
#'                    \code{"bonett"} (default).
#' @param alternative a character string specifying the alternative hypothesis,
#'                    must be one of \code{"two.sided"} (default), \code{"greater"}
#'                    or \code{"less"}.
#' @param conf.level  a numeric value between 0 and 1 indicating the confidence
#'                    level of the interval.
#' @param group       either a character string indicating the variable name of
#'                    the grouping variable in \code{...} or \code{data}, or a
#'                    vector representing the grouping variable.
#' @param split       either a character string indicating the variable name of
#'                    the split variable in \code{...} or 'data', or a vector
#'                    representing the split variable.
#' @param sort.var    logical: if \code{TRUE}, output table is sorted by variables
#'                    when specifying \code{group}.
#' @param na.omit     logical: if \code{TRUE}, incomplete cases are removed before
#'                    conducting the analysis (i.e., listwise deletion) when
#'                    specifying more than one outcome variable.
#' @param digits      an integer value indicating the number of decimal places to
#'                    be used.
#' @param as.na       a numeric vector indicating user-defined missing values,
#'                    i.e. these values are converted to \code{NA} before conducting
#'                    the analysis.
#'                    Note that \code{as.na()} function is only applied to \code{x},
#'                    but not to \code{group} or \code{split}.
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
#' \code{\link{ci.mean}}, \code{\link{ci.mean.diff}}, \code{\link{ci.median}},
#' \code{\link{ci.prop}}, \code{\link{ci.prop.diff}},  \code{\link{descript}}
#'
#' @references
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology
#' - Using R and SPSS}. John Wiley & Sons.
#'
#' Bonett, D. G. (2006). Approximate confidence interval for standard deviation
#' of nonnormal distributions. \emph{Computational Statistics and Data Analysis,
#' 50}, 775-782. https://doi.org/10.1016/j.csda.2004.10.003
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#' \tabular{ll}{
#' \code{call} \tab function call \cr
#' \code{type} \tab type of analysis \cr
#' \code{data} \tab list with the input specified in \code{...}, \code{data}, \code{group}, and \code{split} \cr
#' \code{args} \tab specification of function arguments  \cr
#' \code{result} \tab result table \cr
#' }
#'
#' @export
#'
#' @examples
#' # Example 1a: Two-Sided 95% CI for the variance for 'mpg'
#' ci.var(mtcars$mpg)
#'
#' # Example 1b: Alternative specification using the 'data' argument
#' ci.var(mpg, data = mtcars)
#'
#' # Example 2a: Two-Sided 95% CI for the standard deviation for 'mpg'
#' ci.sd(mtcars$mpg)
#'
#' # Example 2b: Alternative specification using the 'data' argument
#' ci.sd(mpg, data = mtcars)
#'
#' # Example 3: Two-Sided 95% CI using chi square distribution
#' ci.var(mtcars$mpg, method = "chisq")
#'
#' # Example 4: One-Sided 95% CI
#' ci.var(mtcars$mpg, alternative = "less")
#'
#' # Example 5: Two-Sided 99% CI
#' ci.var(mtcars$mpg, conf.level = 0.99)
#'
#' # Example 6: Two-Sided 95% CI, print results with 3 digits
#' ci.var(mtcars$mpg, digits = 3)
#'
#' # Example 7a: Two-Sided 95% CI for 'mpg', 'disp', and 'hp',
#' # listwise deletion for missing data
#' ci.var(mtcars[, c("mpg", "disp", "hp")])
#'
#' # Example 7b: Alternative specification using the 'data' argument
#' ci.var(mpg:hp, data = mtcars)
#'
#' # Example 8a: Two-Sided 95% CI, analysis by 'vs' separately
#' ci.var(mtcars[, c("mpg", "disp", "hp")], group = mtcars$vs)
#'
#' # Example 8b: Alternative specification using the 'data' argument
#' ci.var(mpg:hp, data = mtcars, group = "vs")
#'
#' # Example 9: Two-Sided 95% CI for, analysis by 'vs' separately, sort by variables
#' ci.var(mtcars[, c("mpg", "disp", "hp")], group = mtcars$vs, sort.var = TRUE)
#'
#' # Example 10: Two-Sided 95% CI, split analysis by 'vs'
#' ci.var(mtcars[, c("mpg", "disp", "hp")], split = mtcars$vs)
#'
#' # Example 11a: Two-Sided 95% CI, analysis by 'vs' separately, split analysis by 'am'
#' ci.var(mtcars[, c("mpg", "disp", "hp")], group = mtcars$vs, split = mtcars$am)
#'
#' # Example 11b: Alternative specification using the 'data' argument
#' ci.var(mpg:hp, data = mtcars, group = "vs", split = "am")
#'
#' \dontrun{
#' # Example 12: Write results into a text file
#' ci.var(mpg:hp, data = mtcars, group = "vs", split = "am", write = "Variance.txt")
#' }
ci.var <- function(..., data = NULL, method = c("chisq", "bonett"),
                   alternative = c("two.sided", "less", "greater"),
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

      warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ",
                     length(attributes(x.split)$na.action)), call. = FALSE)

    }

    #......
    # Group variable, split variable
    if (isTRUE(!is.null(group) && !is.null(split))) {

      x.group.split <- na.omit(data.frame(x, group = group, split = split, stringsAsFactors = FALSE))

      x <- x.group.split[,  !names(x.group.split) %in% c("group", "split"), drop = FALSE]
      group <- x.group.split$group
      split <- x.group.split$split

      warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ",
                     length(attributes(x.group.split)$na.action)), call. = FALSE)

    }

    #...................
    ### Variable with missing values only ####
    x.miss <- vapply(x, function(y) all(is.na(y)), FUN.VALUE = logical(1L))
    if (isTRUE(any(x.miss))) { stop(paste0("After listwise deletion, following variables are completely missing: ", paste(names(which(x.miss)), collapse = ", ")), call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # Check input 'method'
    if (isTRUE(!all(method %in%  c("chisq", "bonett")))) { stop("Character string in the argument 'method' does not match with \"chisq\", or \"bonett\".", call. = FALSE) }

    # Check input 'alternative'
    if (isTRUE(!all(alternative %in%  c("two.sided", "less", "greater")))) { stop("Character string in the argument 'alternative' does not match with \"two.sided\", \"less\", or \"greater\".", call. = FALSE) }

    # Check input 'conf.level'
    if (isTRUE(conf.level >= 1L || conf.level <= 0L)) { stop("Please specifiy a numeric value between 0 and 1 for the argument 'conf.level'.", call. = FALSE) }

    # Check input 'group'
    if (isTRUE(!is.null(group))) {

      # Input 'group' completely missing
      if (isTRUE(all(is.na(group)))) { stop("The grouping variable specified in 'group' is completely missing.", call. = FALSE) }

      # Only one group in 'group'
      if (isTRUE(length(na.omit(unique(group))) == 1L)) { warning("There is only one group represented in the grouping variable specified in 'group'.", call. = FALSE) }

    }

    # Check input 'split'
    if (isTRUE(!is.null(split))) {

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

    # Check input output
    if (isTRUE(!is.logical(output))) { stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Method ####

  if (isTRUE(all(c("chisq", "bonett") %in% method))) { method <- "bonett" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Alternative hypothesis ####

  if (isTRUE(all(c("two.sided", "less", "greater") %in% alternative))) { alternative <- "two.sided" }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Confidence interval for the variance ####

  var.conf <- function(x, method, alternative, conf.level, side) {

    # Data
    x <- na.omit(x)
    x.var <- var(x)

    # Number of observations
    if (isTRUE((length(x) < 2L && method == "chisq") || (length(x) < 4L && method == "bonett"))) {

      ci <- c(NA, NA)

    } else {

      #...................
      ### Chi square method ####
      if (isTRUE(method == "chisq")) {

        df <- length(x) - 1L

        # Two-sided CI
        switch(alternative, two.sided = {

          crit.low <- qchisq((1L - conf.level)/2L, df = df, lower.tail = FALSE)
          crit.upp <- qchisq((1L - conf.level)/2L, df = df, lower.tail = TRUE)

          ci <- c(low = df*x.var / crit.low, upp = df*x.var / crit.upp)

        # One-sided CI: less
        }, less = {

          crit.upp <- qchisq((1L - conf.level), df = df, lower.tail = TRUE)

          ci <- c(low = 0L, upp = df*x.var / crit.upp)

          # One-sided CI: greater
        }, greater = {

          crit.low <- qchisq((1L - conf.level), df = df, lower.tail = FALSE)

          ci <- c(low = df*x.var / crit.low, upp = Inf)

        })

      #...................
      ### Bonett method ####
      } else if (isTRUE(method == "bonett")) {

        n <- length(x)

        z <- switch(alternative,
                    two.sided = qnorm(1L - (1L - conf.level)/2L),
                    less = qnorm(1L - (1L - conf.level)),
                    greater = qnorm(1L - (1L - conf.level)))

        cc <- n/(n - z)

        gam4 <- n * sum((x - mean(x, trim = 1L / (2L * (n - 4L)^0.5)))^4L) / (sum((x - mean(x))^2L))^2L

        se <- cc * sqrt((gam4 - (n - 3L)/n) / (n - 1L))

        ci <- switch(alternative,
                     two.sided = c(low = exp(log(cc * x.var) - z * se), upp = exp(log(cc * x.var) + z * se)),
                     less = c(low = 0L, upp = exp(log(cc * x.var) + z * se)),
                     greater = c(low = exp(log(cc * x.var) - z * se), upp = Inf))

      }

    }

    # Lower or upper limit
    object <- switch(side, both = ci, low = ci[1], upp = ci[2L])

    return(object)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## No Grouping, No Split ####

  if (isTRUE(is.null(group) && is.null(split))) {

    result <- data.frame(variable = colnames(x),
                         n = vapply(x, function(y) length(na.omit(y)), FUN.VALUE = 1L),
                         nNA = vapply(x, function(y) sum(is.na(y)), FUN.VALUE = 1L),
                         pNA = vapply(x, function(y) sum(is.na(y)) / length(y) * 100L, FUN.VALUE = double(1)),
                         # Arithmetic mean
                         m = vapply(x, function(y) ifelse(length(na.omit(y)) <= 1L, NA, mean(y, na.rm = TRUE)), FUN.VALUE = double(1L)),
                         # Variance
                         var = vapply(x, function(y) ifelse(length(na.omit(y)) <= 1L, NA, var(y, na.rm = TRUE)), FUN.VALUE = double(1L)),
                         # Confidence interval for the variance
                         low = vapply(x, var.conf, method = method, alternative = alternative, conf.level = conf.level, side = "low", FUN.VALUE = double(1L)),
                         upp = vapply(x, var.conf, method = method, alternative = alternative, conf.level = conf.level, side = "upp", FUN.VALUE = double(1L)),
                         stringsAsFactors = FALSE, row.names = NULL, check.names = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Grouping, No Split ####

  } else if (isTRUE(!is.null(group) && is.null(split))) {

    object.group <- lapply(split(x, f = group), function(y) misty::ci.var(y, method = method, alternative = alternative, conf.level = conf.level,
                                                                          group = NULL, split = NULL, sort.var = sort.var, na.omit = na.omit,
                                                                          as.na = as.na, check = FALSE, output = FALSE)$result)

    result <- data.frame(group = rep(names(object.group), each = ncol(x)),
                         eval(parse(text = paste0("rbind(", paste0("object.group[[", seq_len(length(object.group)), "]]", collapse = ", "), ")"))),
                         stringsAsFactors = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## No Grouping, Split ####

  } else if (isTRUE(is.null(group) && !is.null(split))) {

      result <- lapply(split(data.frame(x, stringsAsFactors = FALSE), f = split),
                       function(y) misty::ci.var(y, method = method, alternative = alternative, conf.level = conf.level,
                                                 group = NULL, split = NULL, sort.var = sort.var, na.omit = na.omit,
                                                 as.na = as.na, check = FALSE, output = FALSE)$result)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Grouping, Split ####

  } else if (isTRUE(!is.null(group) && !is.null(split))) {

    result <- lapply(split(data.frame(x, group = group, stringsAsFactors = FALSE), f = split),
                       function(y) misty::ci.var(y[, -grep("group", names(y))], method = method,
                                                 alternative = alternative, conf.level = conf.level,
                                                 group = y$group, split = NULL, sort.var = sort.var,
                                                 na.omit = na.omit, as.na = as.na,
                                                 check = FALSE, output = FALSE)$result)

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "ci", ci = "var",
                 data = list(x = x, group = group, split = split),
                 args = list(method = method, alternative = alternative, conf.level = conf.level,
                             sort.var = sort.var, na.omit = na.omit, digits = digits, as.na = as.na,
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

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#' @rdname ci.sd
ci.sd <- function(..., data = NULL, method = c("chisq", "bonett"),
                  alternative = c("two.sided", "less", "greater"),
                  conf.level = 0.95, group = NULL, split = NULL, sort.var = FALSE,
                  na.omit = FALSE, digits = 2, as.na = NULL, write = NULL,
                  append = TRUE, check = TRUE, output = TRUE) {

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

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data without using the argument 'data' ####

  } else {

    # Extract data
    x <- eval(..., enclos = parent.frame())

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
  non.num <- !vapply(x, is.numeric, FUN.VALUE = logical(1))

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

    #......
    # Group variable, split variable
    if (isTRUE(!is.null(group) && !is.null(split))) {

      x.group.split <- na.omit(data.frame(x, group = group, split = split, stringsAsFactors = FALSE))

      x <- x.group.split[,  !names(x.group.split) %in% c("group", "split"), drop = FALSE]
      group <- x.group.split$group
      split <- x.group.split$split

      warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ", length(attributes(x.group.split)$na.action)), call. = FALSE)

    }

    #...................
    ### Variable with missing values only ####
    x.miss <- vapply(x, function(y) all(is.na(y)), FUN.VALUE = logical(1))
    if (isTRUE(any(x.miss))) {

      stop(paste0("After listwise deletion, following variables are completely missing: ", paste(names(which(x.miss)), collapse = ", ")), call. = FALSE)

    }

  }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # Check input 'method'
    if (isTRUE(!all(method %in%  c("chisq", "bonett")))) { stop("Character string in the argument 'method' does not match with \"chisq\", or \"bonett\".", call. = FALSE) }

    # Check input 'alternative'
    if (isTRUE(!all(alternative %in%  c("two.sided", "less", "greater")))) { stop("Character string in the argument 'alternative' does not match with \"two.sided\", \"less\", or \"greater\".", call. = FALSE) }

    # Check input 'conf.level'
    if (isTRUE(conf.level >= 1L || conf.level <= 0L)) { stop("Please specifiy a numeric value between 0 and 1 for the argument 'conf.level'.", call. = FALSE) }

    # Check input 'group'
    if (isTRUE(!is.null(group))) {

      # Input 'group' completely missing
      if (isTRUE(all(is.na(group)))) { stop("The grouping variable specified in 'group' is completely missing.", call. = FALSE) }

      # Only one group in 'group'
      if (isTRUE(length(na.omit(unique(group))) == 1L)) { warning("There is only one group represented in the grouping variable specified in 'group'.", call. = FALSE) }

    }

    # Check input 'split'
    if (isTRUE(!is.null(split))) {

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
  ## Method ####

  if (isTRUE(all(c("chisq", "bonett") %in% method))) { method <- "bonett" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Alternative hypothesis ####

  if (isTRUE(all(c("two.sided", "less", "greater") %in% alternative))) { alternative <- "two.sided" }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Confidence interval for the standard deviation ####

  sd.conf <- function(x, method, alternative, conf.level, side) {

    # Data
    x <- na.omit(x)
    x.var <- var(x)

    # Number of observations
    if (isTRUE((length(x) < 2L && method == "chisq") || (length(x) < 4L && method == "bonett"))) {

      ci <- c(NA, NA)

    } else {

      #...................
      ### Chi square method ####
      if (isTRUE(method == "chisq")) {

        df <- length(x) - 1L

        # Two-sided CI
        switch(alternative, two.sided = {

          crit.low <- qchisq((1L - conf.level)/2L, df = df, lower.tail = FALSE)
          crit.upp <- qchisq((1L - conf.level)/2L, df = df, lower.tail = TRUE)

          ci <- sqrt(c(low = df*x.var / crit.low, upp = df*x.var / crit.upp))

        # One-sided CI: less
        }, less = {

          crit.upp <- qchisq((1L - conf.level), df = df, lower.tail = TRUE)

          ci <- c(low = 0L, upp = sqrt(df*x.var / crit.upp))

        # One-sided CI: greater
        }, greater = {

          crit.low <- qchisq((1L - conf.level), df = df, lower.tail = FALSE)

          ci <- c(low = sqrt(df*x.var / crit.low), upp = Inf)

        })

      #...................
      ### Bonett ####
      } else if (isTRUE(method == "bonett")) {

        n <- length(x)

        z <- switch(alternative,
                    two.sided = qnorm(1L - (1L - conf.level)/2L),
                    less = qnorm(1L - (1L - conf.level)),
                    greater = qnorm(1L - (1L - conf.level)))

        cc <- n/(n - z)

        gam4 <- n * sum((x - mean(x, trim = 1L / (2L * (n - 4L)^0.5)))^4L) / (sum((x - mean(x))^2))^2L

        se <- cc * sqrt((gam4 - (n - 3L)/n) / (n - 1L))

        ci <- switch(alternative,
                     two.sided = sqrt(c(low = exp(log(cc * x.var) - z * se), upp = exp(log(cc * x.var) + z * se))),
                     less = c(low = 0, upp = sqrt(exp(log(cc * x.var) + z * se))),
                     greater = c(low = sqrt(exp(log(cc * x.var) - z * se)), upp = Inf))

      }

    }

    # Lower or upper limit
    object <- switch(side, both = ci, low = ci[1L], upp = ci[2L])

    return(object)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## No Grouping, No Split ####

  if (isTRUE(is.null(group) && is.null(split))) {

    result <- data.frame(variable = colnames(x),
                         n = vapply(x, function(y) length(na.omit(y)), FUN.VALUE = 1L),
                         nNA = vapply(x, function(y) sum(is.na(y)), FUN.VALUE = 1L),
                         pNA = vapply(x, function(y) sum(is.na(y)) / length(y) * 100L, FUN.VALUE = double(1L)),
                         # Arithmetic mean
                         m = vapply(x, function(y) ifelse(length(na.omit(y)) <= 1L, NA, mean(y, na.rm = TRUE)), FUN.VALUE = double(1L)),
                         # Standard deviation
                         sd = vapply(x, function(y) ifelse(length(na.omit(y)) <= 1L, NA, sd(y, na.rm = TRUE)), FUN.VALUE = double(1L)),
                         # Confidence interval for the variance
                         low = vapply(x, sd.conf, method = method, alternative = alternative, conf.level = conf.level, side = "low", FUN.VALUE = double(1L)),
                         upp = vapply(x, sd.conf, method = method, alternative = alternative, conf.level = conf.level, side = "upp", FUN.VALUE = double(1L)),
                         stringsAsFactors = FALSE, row.names = NULL, check.names = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Grouping, No Split ####

  } else if (isTRUE(!is.null(group) && is.null(split))) {

    object.group <- lapply(split(x, f = group), function(y) misty::ci.sd(y, method = method, alternative = alternative, conf.level = conf.level,
                                                                         group = NULL, split = NULL, sort.var = sort.var, na.omit = na.omit,
                                                                         as.na = as.na, check = FALSE, output = FALSE)$result)

    result <- data.frame(group = rep(names(object.group), each = ncol(x)),
                         eval(parse(text = paste0("rbind(", paste0("object.group[[", seq_len(length(object.group)), "]]", collapse = ", "), ")"))),
                         stringsAsFactors = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## No Grouping, Split ####

  } else if (isTRUE(is.null(group) && !is.null(split))) {

    result <- lapply(split(data.frame(x, stringsAsFactors = FALSE), f = split),
                     function(y) misty::ci.sd(y, method = method, alternative = alternative, conf.level = conf.level,
                                              group = NULL, split = NULL, sort.var = sort.var, na.omit = na.omit,
                                              as.na = as.na, check = FALSE, output = FALSE)$result)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Grouping, Split ####

  } else if (isTRUE(!is.null(group) && !is.null(split))) {

    result <- lapply(split(data.frame(x, group = group, stringsAsFactors = FALSE), f = split),
                     function(y) misty::ci.sd(y[, -grep("group", names(y))], method = method,
                                              alternative = alternative, conf.level = conf.level,
                                              group = y$group, split = NULL, sort.var = sort.var,
                                              na.omit = na.omit, as.na = as.na,
                                              check = FALSE, output = FALSE)$result)

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "ci", ci = "sd",
                 data = list(x = x, group = group, split = split),
                 args = list(method = method, alternative = alternative, conf.level = conf.level,
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
