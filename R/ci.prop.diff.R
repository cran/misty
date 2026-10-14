#' Confidence Interval for the Difference in Proportions
#'
#' This function computes confidence intervals for the difference in proportions
#' in a two-sample and paired-sample design for one or more variables, optionally
#' by a grouping and/or split variable.
#'
#' @param data        a data frame. Note that data are specified either by using
#'                    the argument \code{data} or by using the arguments \code{x}
#'                    and \code{y}.
#' @param ...         an expression indicating the variable names in \code{data},
#'                    e.g., \code{ci.mean.diff(dat, x1, x2)}. Note that the operators
#'                    \code{+}, \code{-}, \code{~}, \code{:}, \code{::}, and \code{!}
#'                    can also be used to select variables, see 'Details' in the
#'                    \code{\link{df.subset}} function.
#' @param x           a numeric vector of data values.
#' @param y           a numeric vector of data values.
#' @param formula     a formula of the form \code{y ~ group} for one outcome variable
#'                    or \code{cbind(y1, y2, y3) ~ group} for more than one outcome
#'                    variable where \code{y} is a numeric variable giving the data
#'                    values and \code{group} a numeric variable, character variable
#'                    or factor with two values or factor levels giving the corresponding
#'                    groups.
#' @param method      a character string specifying the method for computing
#'                    the confidence interval,
#'                    must be one of \code{"wald"}, or \code{"newcombe"} (default).
#' @param paired      logical: if \code{TRUE}, confidence interval for the
#'                    difference of proportions in paired samples is computed.
#' @param alternative a character string specifying the alternative hypothesis,
#'                    must be one of \code{"two.sided"} (default), \code{"greater"}
#'                    or \code{"less"}.
#' @param na.omit     logical: if \code{TRUE}, incomplete cases are removed before
#'                    conducting the analysis (i.e., listwise deletion) when
#'                    specifying more than one outcome variable.
#' @param conf.level  a numeric value between 0 and 1 indicating the confidence
#'                    level of the interval.
#' @param group       a numeric vector, character vector or factor as grouping
#'                    variable. Note that a grouping variable can only be used
#'                    when computing confidence intervals with unknown population
#'                    standard deviation and population variance.
#' @param split       a numeric vector, character vector or factor as split variable.
#'                    Note that a split variable can only be used when computing
#'                    confidence intervals with unknown population standard
#'                    deviation and population variance.
#' @param sort.var    logical: if \code{TRUE}, output table is sorted by variables
#'                    when specifying \code{group}.
#' @param digits      an integer value indicating the number of decimal places
#'                    to be used.
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
#' @param check       logical: if \code{TRUE} (default), argument specification
#'                    is checked.
#' @param output      logical: if \code{TRUE} (default), output is shown on the
#'                    console.
#'
#' @details
#' The Wald confidence interval which is based on the normal approximation to the
#' binomial distribution are computed by specifying \code{method = "wald"}, while
#' the Newcombe Hybrid Score interval (Newcombe, 1998a; Newcombe, 1998b) is
#' requested by specifying \code{method = "newcombe"}. By default, Newcombe Hybrid
#' Score interval is computed which have been shown to be reliable in small samples
#' (less than n = 30 in each sample) as well as moderate to larger samples(n > 30
#' in each sample) and with proportions close to 0 or 1, while the Wald confidence
#' intervals does not perform well unless the sample size is large (Fagerland,
#' Lydersen & Laake, 2011).
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{ci.prop}}, \code{\link{ci.mean}}, \code{\link{ci.mean.diff}},
#' \code{\link{ci.median}}, \code{\link{ci.var}}, \code{\link{ci.sd}},
#' \code{\link{descript}}
#'
#' @exportMethod  ci.prop.diff default
#'
#' @exportMethod  ci.prop.diff formula
#'
#' @references
#' Fagerland, M. W., Lydersen S., & Laake, P. (2011) Recommended confidence
#' intervals for two independent binomial proportions. \emph{Statistical Methods
#' in Medical Research, 24}, 224-254.
#'
#' Newcombe, R. G. (1998a). Interval estimation for the difference between
#' independent proportions: Comparison of eleven methods. \emph{Statistics in
#' Medicine, 17}, 873-890.
#'
#' Newcombe, R. G. (1998b). Improved confidence intervals for the difference
#' between binomial proportions based on paired data. \emph{Statistics in Medicine,
#' 17}, 2635-2650.
#'
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology
#' - Using R and SPSS}. John Wiley & Sons.
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#'
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{data}}{list with the input specified in \code{x}, \code{group}, and \code{split}}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{result}}{result table}
#'
#' @export
#'
#' @examples
#' #————————————————————————————————————————————————————————————————————————————
#' # Two-Sample Design
#'
#' # Example 1a: Two-Sided 95% CI for 'vs' by 'am', Newcombes Hybrid Score interval
#' ci.prop.diff(vs ~ am, data = mtcars)
#'
#' # Example 2b: Alternative specification without using the '...' argument
#' ci.mean.diff(x = df.subset(mtcars, vs, subset = am == 0),
#'              y = df.subset(mtcars, vs, subset = am == 1))
#'
#' # Example 1c: Wald CI
#' ci.prop.diff(vs ~ am, data = mtcars, method = "wald")
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Paired-Sample Design
#'
#' # Example 2a: Two-Sided 95% CI for the difference in proportions 'pre' and 'post'
#' # Newcombes Hybrid Score interval
#' ci.prop.diff(mtcars, vs, am, paired = TRUE)
#'
#' # Example 2b:  Alternative specification without using the '...' argument
#' ci.prop.diff(x = mtcars$vs, y = mtcars$am, paired = TRUE)
#'
#' # Example 2b: Wald CI
#' ci.prop.diff(mtcars, vs, am, method = "wald", paired = TRUE)
ci.prop.diff <- function(...) {

  UseMethod("ci.prop.diff")

}

#_______________________________________________________________________________
#
# Default S3 method ————————————————————————————————————————————————————————————

ci.prop.diff.default <- function(data = NULL, ..., x = NULL, y = NULL,
                                 method = c("wald", "newcombe"), paired = FALSE,
                                 alternative = c("two.sided", "less", "greater"),
                                 conf.level = 0.95, group = NULL, split = NULL, sort.var = FALSE,
                                 digits = 2, as.na = NULL, write = NULL, append = TRUE,
                                 check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  ## Initial Check ####

  # Check if input 'data' and 'x' are NULL
  if (isTRUE(is.null(data) && is.null(x))) { stop("Please specify the argument 'data' or the argument 'x'.", call. = FALSE) }

  # Check if input 'data' or 'x' is specified
  if (isTRUE(!is.null(data) && !is.null(x))) { stop("Please specify either the argument 'data' or the argument 'x', but not both.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data ———————————————————————————————————————————————————————————————————————

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Extract Variables ####

  if (isTRUE(!is.null(data))) {

    .data(data, ..., group = group, split = split, drop = TRUE, check.one.two = TRUE) |> (\(p) {

      # Extract 'x'
      assign("x", p$data, envir = parent.frame())

      # Extract 'y'
      if (isTRUE(ncol(as.data.frame(p$data)) > 1L)) {

        assign("y", x[, 2L], envir = parent.frame())
        assign("x", x[, 1L] , envir = parent.frame())

      } else {

        assign("y", NULL, envir = parent.frame())

      }

      # Grouping variable
      if (isTRUE(!is.null(p$group))) { assign("group", p$group, envir = parent.frame()) }

      # Split variable
      if (isTRUE(!is.null(p$split))) { assign("split", p$split, envir = parent.frame()) }

    })()

  }

  #_____________________________________________________________________________
  #
  ## Convert User-Missing Values into NA ####

  if (isTRUE(!is.null(as.na))) {

    # One-sample
    if (isTRUE(is.null(y))) {

      # Replace user-specified values with missing values
      x <- .as.na(x, na = as.na)

      # Two- or paired-sample
    } else {

      # Replace user-specified values with missing values
      x <- .as.na(x, na = as.na)
      y <- .as.na(y, na = as.na)

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Grouping Variable ####

  if (isTRUE(!is.null(group))) {

    if (isTRUE(!paired)) { stop("Please use formula notation for using a grouping variable in independent samples.", call. = FALSE) }

    if (ncol(data.frame(group)) != 1) { stop("More than one grouping variable specified for the argument 'group'.",call. = FALSE) }

    if (isTRUE(paired)) {

      if (nrow(data.frame(group)) != nrow(data.frame(x))) {

        stop("Length of the vector or factor specified in the argument 'group' does not match with 'x'.", call. = FALSE)

      }

    }

    # Convert 'group' into a vector
    group <- unlist(group, use.names = FALSE)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Split Variable ####

  if (isTRUE(!is.null(split))) {

    if (isTRUE(!paired)) { stop("Please use formula notation for using a split variable in independent samples.", call. = FALSE) }

    if (ncol(data.frame(split)) != 1) { stop("More than one split variable specified for the argument 'split'.",call. = FALSE) }

    if (isTRUE(paired)) {

      if (nrow(data.frame(split)) != nrow(data.frame(x))) { stop("Length of the vector or factor specified in the argument 'split' does not match with 'x'.", call. = FALSE) }

    }

    # Convert 'split' into a vector
    split <- unlist(split, use.names = FALSE)

  }

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## List or Data frame ####

  # Independent samples
  if (!isTRUE(paired)) {

    xy <- list(x = x, y = y)

    # Paired samples
  } else {

    xy <- data.frame(x = x, y = y)

  }

  #_____________________________________________________________________________
  #
  # Input Check ————————————————————————————————————————————————————————————————

  .check.input(logical = c("sort.var", "append", "output"), s.character = list(method = c("wald", "newcombe")), args = c("xy", "conf.level", "digits", "write1"), envir = environment(), input.check = check)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Additional Checks ####

  if (isTRUE(check)) {

    #—————————————————————————————————————— #
    ### 'x' and 'y' Arguments ####

    # Check input 'x'
    if (isTRUE(!all(unlist(x) %in% c(0L, 1L, NA)))) { stop("Please specify a numeric vector with 0 and 1 values for the argument 'x'.", call. = FALSE) }

    # Check input 'y'
    if (isTRUE(!all(unlist(x) %in% c(0L, 1L, NA)))) { stop("Please specify a numeric vector with 0 and 1 values for the argument 'y.", call. = FALSE) }

    #—————————————————————————————————————— #
    ### 'group' Argument ####

    if (isTRUE(!is.null(group))) {

      # Vector or factor for the argument 'group'?
      if (isTRUE(!is.vector(group) && !is.factor(group))) { stop("Please specify a vector or factor for the argument 'group'.", call. = FALSE) }

      # Input 'group' completely missing
      if (isTRUE(all(is.na(group)))) { stop("The grouping variable specified in 'group' is completely missing.", call. = FALSE) }

      # Only one group in 'group'
      if (length(na.omit(unique(group))) == 1L) { warning("There is only one group represented in the grouping variable specified in 'group'.", call. = FALSE) }

    }

    #—————————————————————————————————————— #
    ### 'split' Argument ####

    if (isTRUE(!is.null(split))) {

      # Input 'split' completely missing
      if (isTRUE(all(is.na(split)))) { stop("The split variable specified in 'split' is completely missing.", call. = FALSE) }

      # Only one group in 'split'
      if (length(na.omit(unique(split))) == 1L) { warning("There is only one group represented in the split variable specified in 'split'.", call. = FALSE) }

    }

  }

  #_____________________________________________________________________________
  #
  # Arguments ——————————————————————————————————————————————————————————————————

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'method' Argument ####

  if (isTRUE(all(c("wald", "newcombe") %in% method))) { method <- "newcombe" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'alternative' Argument ####

  if (isTRUE(all(c("two.sided", "less", "greater") %in% alternative))) { alternative <- "two.sided" }

  #_____________________________________________________________________________
  #
  # Main Function ——————————————————————————————————————————————————————————————

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## No Grouping, No Split ####

  if (isTRUE(is.null(group) && is.null(split))) {

    #—————————————————————————————————————— #
    ### Two-Samples ####

    if (!isTRUE(paired)) {

      result <- misty::df.rbind(data.frame(variable = "y", between = 1L, n = length(na.omit(xy$x)), nNA = sum(is.na(xy$x)), p = mean(xy$x, na.rm = TRUE)),
                                data.frame(variable = "y", between = 2L, n = length(na.omit(xy$y)), nNA = sum(is.na(xy$y)), p = mean(xy$y, na.rm = TRUE),
                                           p.diff = mean(xy$y, na.rm = TRUE) - mean(xy$x, na.rm = TRUE),
                                           low = .prop.diff.conf(x = xy$x, y = xy$y, method = method, alternative = alternative, paired = FALSE, conf.level = conf.level, side = "low"),
                                           upp = .prop.diff.conf(x = xy$x, y = xy$y, method = method, alternative = alternative, paired = FALSE, conf.level = conf.level, side = "upp"),
                                           row.names = NULL))

    #—————————————————————————————————————— #
    ### Paired-Samples ####

    } else {

      result <- data.frame(variable = "y", n = nrow(na.omit(xy)), nNA = length(attributes(na.omit(xy))$na.action), p1 = mean(xy$x, na.rm = TRUE), p2 = mean(xy$y, na.rm = TRUE), p.diff = mean(xy$y - xy$x, na.rm = TRUE),
                           low = .prop.diff.conf(x = xy$x, y = xy$y, method = method, alternative = alternative, paired = TRUE, conf.level = conf.level, side = "low"),
                           upp = .prop.diff.conf(x = xy$x, y = xy$y, method = method, alternative = alternative, paired = TRUE, conf.level = conf.level, side = "upp"),
                           row.names = NULL)

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Grouping, No Split ####

  } else if (isTRUE(!is.null(group) && is.null(split))) {

    object.group <- lapply(split(xy, f = group),
                           function(y) ci.prop.diff.default(x = y$x, y = y$y, method = method, alternative = alternative, conf.level = conf.level, paired = paired, group = NULL, split = NULL, sort.var = sort.var, na.omit = na.omit, as.na = as.na, check = FALSE, output = FALSE)$result)

    result <- data.frame(group = names(object.group),
                         eval(parse(text = paste0("rbind(", paste0("object.group[[", seq_len(length(object.group)), "]]", collapse = ", "), ")"))))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## No Grouping, Split ####

  } else if (isTRUE(is.null(group) && !is.null(split))) {

    result <- lapply(split(data.frame(xy), f = split),
                     function(y) ci.prop.diff.default(x = y$x, y = y$y, alternative = alternative, method = method, conf.level = conf.level, paired = paired, group = NULL, split = NULL, sort.var = sort.var, na.omit = na.omit, as.na = as.na, check = FALSE, output = FALSE)$result)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Grouping, Split ####

  } else if (isTRUE(!is.null(group) && !is.null(split))) {

    result <- lapply(split(data.frame(xy, .group = group, row.names = NULL), f = split),
                     function(y) ci.prop.diff.default(x = y$x, y = y$y, method = method, alternative = alternative, conf.level = conf.level, paired = paired, group = y$.group, split = NULL, sort.var = sort.var, na.omit = na.omit, as.na = as.na, check = FALSE, output = FALSE)$result)

  }

  #_____________________________________________________________________________
  #
  # Return Object ——————————————————————————————————————————————————————————————

  object <- list(call = match.call(),
                 type = "ci", ci = ifelse(!isTRUE(paired), "prop.diff.i",  "prop.diff.p"),
                 data = list(x = x, y = y, group = group, split = split),
                 args = list(method = method, alternative = alternative, conf.level = conf.level, paired = paired, sort.var = sort.var, na.omit = na.omit, digits = digits, as.na = as.na, check = check, output = output),
                 result = result)

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Write Results ——————————————————————————————————————————————————————————————

  if (isTRUE(!is.null(write))) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Text File ####

    # Send R output to text file
    sink(file = write, append = ifelse(isTRUE(file.exists(write)), append, FALSE), type = "output", split = FALSE)

    if (append && isTRUE(file.exists(write))) { write("", file = write, append = TRUE) }

    # Print object
    print(object, check = FALSE)

    # Close file connection
    sink()

  }

  #_____________________________________________________________________________
  #
  # Output —————————————————————————————————————————————————————————————————————

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}

#_______________________________________________________________________________
#
# S3 method for class 'formula'  ———————————————————————————————————————————————

ci.prop.diff.formula <- function(formula, data, method = c("wald", "newcombe"),
                                 alternative = c("two.sided", "less", "greater"),
                                 conf.level = 0.95, group = NULL, split = NULL,
                                 sort.var = FALSE, na.omit = FALSE, digits = 2,
                                 as.na = NULL, write = NULL, append = TRUE,
                                 check = TRUE, output = TRUE, ...) {

  #_____________________________________________________________________________
  #
  # Initial Check ——————————————————————————————————————————————————————————————

  # Check if input 'formula' is missing
  if (isTRUE(missing(formula))) { stop("Please specify a formula using the argument 'formula'", call. = FALSE) }

  # Check if input 'data' is missing or NULL
  if (isTRUE(missing(data) ||is.null(data))) { stop("Please specify a matrix or data frame for the argument 'x'.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data and Variables —————————————————————————————————————————————————————————

  # Data frame
  data <- as.data.frame(data)

  # Variables
  var.formula <- all.vars(as.formula(formula))

  # Grouping variable
  group.var <- attr(terms(formula[-2L]), "term.labels")

  # Outcome(s)
  y.vars <- var.formula[-grep(group.var, var.formula)]

  # Check if variables are in the data
  (!var.formula %in% colnames(data)) |> (\(p) if (isTRUE(any(p))) { stop(paste0("Variables specified in the the formula were not found in 'data': ", paste(var.formula[which(p)], collapse = ", ")), call. = FALSE)  })()

  # Check if input 'formula' has only one grouping variable
  if (isTRUE(length(group.var) != 1L)) { stop("Please specify a formula with only one grouping variable.", call. = FALSE) }

  #—————————————————————————————————————— #
  ### Grouping Variable ####

  if (isTRUE(!is.null(group))) {

    # Grouping variable specified with variable name
    if (isTRUE(is.character(group) && length(group) == 1L))  {

      if (isTRUE(!group %in% colnames(data))) { stop("Grouping variable specified in the argument 'group' was not found in the 'data.'", call. = FALSE) }

      group <- data[, group]

      # Grouping variable specified as vector
    } else {

      if (nrow(data.frame(group)) != nrow(data)) { stop("Length of the vector or factor specified in the argument 'group' does not match the number of rows in 'data'.", call. = FALSE) }

      # Convert 'group' into a vector
      group <- unlist(group, use.names = FALSE)

    }

  }

  #—————————————————————————————————————— #
  ### Split Variable ####

  if (isTRUE(!is.null(split))) {

    # Split variable specified with variable name
    if (isTRUE(is.character(split) && length(split) == 1L))  {

      if (isTRUE(!split %in% colnames(data))) { stop("Split variable specified in the argument 'split' was not found in the 'data.'", call. = FALSE) }

      split <- data[, split]

      # Split variable specified as vector
    } else {

      if (nrow(data.frame(split)) != nrow(data)) { stop("Length of the vector or factor specified in the argument 'split' does not match the number of rows in 'data'.", call. = FALSE) }

      # Convert 'split' into a vector
      split <- unlist(split, use.names = FALSE)

    }

  }

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert User-Missing Values into NA ####

  if (isTRUE(!is.null(as.na))) {

    # Replace user-specified values with missing values
    data[, y.vars] <- misty::as.na(data[, y.vars], na = as.na, check = check)

    # Variable with missing values only
    vapply(data[, y.vars, drop = FALSE], function(y) all(is.na(y)), FUN.VALUE = logical(1L)) |>
      (\(y) if (isTRUE(any(y))) {

        stop(paste0("After converting user-missing values into NA, following variables are completely missing: ", paste(names(which(y)), collapse = ", ")), call. = FALSE)

      })()


  }

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Listwise Deletion ####

  if (isTRUE(na.omit && any(is.na(data[, var.formula])))) {

    #—————————————————————————————————————— #
    ### No Group and Split Variable ####

    if (isTRUE(is.null(group) && is.null(split))) {

      x <- na.omit(as.data.frame(data[, var.formula]))

      warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ", length(attributes(x)$na.action)), call. = FALSE)

    }

    #—————————————————————————————————————— #
    ### Group Variable, No Split Variable ####

    if (isTRUE(!is.null(group) && is.null(split))) {

      data.group <- na.omit(data.frame(data[, var.formula], group = group))

      data <- data.group[, -grep("group", names(data.group)), drop = FALSE]
      group <- data.group$group

      warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ", length(attributes(data.group)$na.action)), call. = FALSE)

    }

    #—————————————————————————————————————— #
    ### No Group Variable, Split Variable ####

    if (isTRUE(is.null(group) && !is.null(split))) {

      data.split <- na.omit(data.frame(data[, var.formula], split = split))

      data <- data.split[, -grep("split", names(data.split)), drop = FALSE]
      split <- data.split$split

      warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ", length(attributes(data.split)$na.action)), call. = FALSE)

    }

    #—————————————————————————————————————— #
    ### Group Variable, Split Variable ####

    if (isTRUE(!is.null(group) && !is.null(split))) {

      data.group.split <- na.omit(data.frame(data[, var.formula], group = group, split = split))

      data <- data.group.split[,  !names(data.group.split) %in% c("group", "split"), drop = FALSE]
      group <- data.group.split$group
      split <- data.group.split$split

      warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ", length(attributes(data.group.split)$na.action)), call. = FALSE)

    }

    #—————————————————————————————————————— #
    ### Variable with Missing Values Only ####

    vapply(data[, var.formula], function(y) all(is.na(y)), FUN.VALUE = logical(1L)) |>
      (\(y) if (isTRUE(any(y))) {

        stop(paste0("After listwise deletion, following variables are completely missing: ", paste(names(which(y)), collapse = ", ")), call. = FALSE)

      })()

  }

  #_____________________________________________________________________________
  #
  # Arguments ——————————————————————————————————————————————————————————————————

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'method' Argument ####

  if (isTRUE(all(c("wald", "newcombe") %in% method))) { method <- "newcombe" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'alternative' Argument ####

  if (isTRUE(all(c("two.sided", "less", "greater") %in% alternative))) { alternative <- "two.sided" }

  #_____________________________________________________________________________
  #
  # Main Function ——————————————————————————————————————————————————————————————

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## No Grouping, No Split ####

  if (isTRUE(is.null(group) && is.null(split))) {

    result <- data.frame(matrix(NA, ncol = 8L, nrow = length(y.vars)*2L, dimnames = list(NULL, c("variable", "between", "n", "nNA", "p", "p.diff", "low", "upp"))))

    loop.mat <- matrix(seq_len(length(y.vars)*2L), ncol = 2L, byrow = TRUE)

    for (i in seq_along(y.vars)) {

      data.split <- split(data[, y.vars[i]], f = data[, group.var])

      result[loop.mat[i, ], ] <- data.frame(variable = y.vars[i],
                                            ci.prop.diff.default(x = data.split[[1L]], y = data.split[[2L]], method = method,
                                                                 paired = FALSE, alternative = alternative,
                                                                 conf.level = conf.level, group = NULL, split = NULL, sort.var = sort.var,
                                                                 digits = digits, as.na = NULL, check = check, output = FALSE)$result[, -1L])

      result[loop.mat[i, ], "between"] <- names(data.split)

    }

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Grouping, No Split ####

  } else if (isTRUE(!is.null(group) && is.null(split))) {

    object.group <- lapply(split(data[, var.formula], f = group),
                           function(y) misty::ci.prop.diff(formula, data = y, method = method,
                                                           alternative = alternative,
                                                           conf.level = conf.level, group = NULL, split = NULL,
                                                           sort.var = sort.var, na.omit = na.omit,
                                                           as.na = as.na, check = FALSE, output = FALSE)$result)

    result <- data.frame(group = rep(names(object.group), each = length(y.vars)*2L), eval(parse(text = paste0("rbind(", paste0("object.group[[", seq_len(length(object.group)), "]]", collapse = ", "), ")")))) |> (\(p) if (isTRUE(sort.var)) { data.frame(p[order(p[, "variable"]), ], row.names = NULL) } else { return(p) })()

    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## No Grouping, Split ####

  } else if (isTRUE(is.null(group) && !is.null(split))) {

    result <- lapply(split(data.frame(data[, var.formula]), f = split),
                     function(y) misty::ci.prop.diff(formula, data = y, method = method,
                                                     alternative = alternative, conf.level = conf.level,
                                                     group = NULL, split = NULL, sort.var = sort.var, na.omit = na.omit,
                                                     as.na = as.na, check = FALSE, output = FALSE)$result)

    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Grouping, Split ####

  } else if (isTRUE(!is.null(group) && !is.null(split))) {

    result <- lapply(split(data.frame(data[, var.formula], .group = group), f = split),
                     function(y) misty::ci.prop.diff(formula, data = y, method = method,
                                                     alternative = alternative, conf.level = conf.level,
                                                     group = y$.group, split = NULL, sort.var = sort.var, na.omit = na.omit,
                                                     as.na = as.na, check = FALSE, output = FALSE)$result) |> (\(p) if (isTRUE(sort.var)) { lapply(p, function(y) data.frame(y[order(y[, "variable"]), ], row.names = NULL)) } else { return(p) })()

  }

  #_____________________________________________________________________________
  #
  # Return Object ——————————————————————————————————————————————————————————————

  object <- list(call = match.call(),
                 type = "ci", ci = "prop.diff.i",
                 data = list(data = data[, var.formula], group = group, split = split),
                 args = list(formula = formula, method = method, alternative = alternative, conf.level = conf.level, sort.var = sort.var, na.omit = na.omit, digits = digits, as.na = as.na, write = write, append = append, check = check, output = output),
                 result = result)

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Write results ——————————————————————————————————————————————————————————————

  if (isTRUE(!is.null(write))) {

    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Text file ####

    # Send R output to text file
    sink(file = write, append = ifelse(isTRUE(file.exists(write)), append, FALSE), type = "output", split = FALSE)

    if (isTRUE(append && file.exists(write))) { write("", file = write, append = TRUE) }

    # Print object
    print(object, check = FALSE)

    # Close file connection
    sink()

  }

  #_____________________________________________________________________________
  #
  # Output —————————————————————————————————————————————————————————————————————

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}

#_______________________________________________________________________________
