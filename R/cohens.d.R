#' Cohen's d
#'
#' This function computes Cohen's d for one-sample, two-sample (i.e., between-subject
#' design), and paired-sample designs (i.e., within-subject design) for one or more
#' variables, optionally by a grouping and/or split variable. In a two-sample design,
#' the function computes the standardized mean difference by dividing the difference
#' between  means of the two groups of observations by the weighted pooled standard
#' deviation (i.e., Cohen's \eqn{d_s} according to Lakens, 2013) by default. In
#' a paired-sample design, the function computes the standardized mean difference
#' by dividing the mean of the difference scores by the standard deviation of the
#' difference scores (i.e., Cohen's \eqn{d_z} according to Lakens, 2013) by default.
#' Note that by default Cohen's d is computed without applying the correction factor
#' for removing the small sample bias (i.e., Hedges' g).
#'
#' @param x           a numeric vector or data frame.
#' @param y           a numeric vector.
#' @param mu          a numeric value indicating the reference mean.
#' @param paired      logical: if \code{TRUE}, Cohen's d for a paired-sample design
#'                    is computed.
#' @param weighted    logical: if \code{TRUE} (default), the weighted pooled
#'                    standard deviation is used to compute the standardized mean
#'                    difference between two groups of a two-sample design (i.e.,
#'                    \code{paired = FALSE}), while standard deviation of the
#'                    difference scores is used to compute the standardized mean
#'                    difference in a paired-sample design (i.e., \code{paired = TRUE}).
#' @param cor         logical: if \code{TRUE} (default), \code{paired = TRUE},
#'                    and \code{weighted = FALSE}, Cohen's d for a paired-sample
#'                    design while controlling for the correlation between the
#'                    two sets of measurement is computed. Note that this argument
#'                    is only used in a paired-sample design (i.e., \code{paired = TRUE})
#'                    when specifying \code{weighted = FALSE}.
#' @param ref         character string \code{"x"} or \code{"y"} for specifying
#'                    the reference reference group when using the default
#'                    \code{cohens.d()} function or a numeric value or character
#'                    string indicating the reference group in a two-sample design
#'                    when using the formula \code{cohens.d()} function. The standard
#'                    deviation of the reference variable or reference group is
#'                    used to standardized the mean difference. Note that this
#'                    argument is only used in a two-sample design
#'                    (i.e., \code{paired = FALSE}).
#' @param correct     logical: if \code{TRUE}, correction factor to remove positive
#'                    bias in small samples is used.
#' @param alternative a character string specifying the alternative hypothesis,
#'                    must be one of \code{"two.sided"} (default), \code{"greater"}
#'                    or \code{"less"}.
#' @param conf.level  a numeric value between 0 and 1 indicating the confidence
#'                    level of the interval.
#' @param group       a numeric vector, character vector or factor as grouping
#'                    variable.
#' @param split       a numeric vector, character vector or factor as split variable.
#' @param sort.var    logical: if \code{TRUE}, output table is sorted by variables
#'                    when specifying \code{group}.
#' @param digits      an integer value indicating the number of decimal places to
#'                    be used for displaying results.
#' @param as.na       a numeric vector indicating user-defined missing values,
#'                    i.e. these values are converted to \code{NA} before conducting
#'                    the analysis. Note that \code{as.na()} function is only
#'                    applied to \code{y} but not to \code{group} in a two-sample
#'                    design, while \code{as.na()} function is applied to \code{pre}
#'                    and \code{post} in a paired-sample design.
#' @param write       a character string naming a text file with file extension
#'                    \code{".txt"} (e.g., \code{"Output.txt"}) for writing the
#'                    output into a text file.
#' @param append      logical: if \code{TRUE} (default), output will be appended
#'                    to an existing text file with extension \code{.txt} specified
#'                    in \code{write}, if \code{FALSE} existing text file will be
#'                    overwritten.
#' @param check       logical: if \code{TRUE} (default), argument specification is
#'                    checked.
#' @param output      logical: if \code{TRUE} (default), output is shown on the
#'                    console.
#' @param formula     a formula of the form \code{y ~ group} for one outcome variable
#'                    or \code{cbind(y1, y2, y3) ~ group} for more than one outcome
#'                    variable where \code{y} is a numeric variable giving the
#'                    data values and \code{group} a numeric variable, character
#'                    variable or factor with two values or factor levels giving
#'                    the corresponding groups.
#' @param data        a matrix or data frame containing the variables in the formula
#'                    \code{formula}.
#' @param na.omit     logical: if \code{TRUE}, incomplete cases are removed before
#'                    conducting the analysis (i.e., listwise deletion) when
#'                    specifying more than one outcome variable.
#' @param ...         further arguments to be passed to or from methods.
#'
#' @details#'
#' Cohen (1988, p.67) proposed to compute the standardized mean difference in a
#' two-sample design by dividing the mean difference by the unweighted pooled
#' standard deviation (i.e., \code{weighted = FALSE}).
#'
#' Glass et al. (1981, p. 29) suggested to use the standard deviation of the
#' control group (e.g., \code{ref = 0} if the control group is coded with 0) to
#' compute the standardized mean difference in a two-sample design (i.e., Glass's
#' \eqn{\Delta}) since the standard deviation of the control group is unaffected
#' by the treatment and will therefore more closely reflect the population
#' standard deviation.
#'
#' Hedges (1981, p. 110) recommended to weight each group's standard deviation by
#' its sample size resulting in a weighted and pooled standard deviation (i.e.,
#' \code{weighted = TRUE}, default). According to Hedges and Olkin (1985, p. 81),
#' the standardized mean difference based on the weighted and pooled standard
#' deviation has a positive small sample bias, i.e., standardized mean difference
#' is overestimated in small samples (i.e., sample size less than 20 or less than
#' 10 in each group). However, a correction factor can be applied to remove the
#' small sample bias (i.e., \code{correct = TRUE}). Note that the function uses
#' a gamma function for computing the correction factor, while a approximation
#' method is used if computation based on the gamma function fails.
#'
#' Note that the terminology is inconsistent because the standardized mean
#' difference based on the weighted and pooled standard deviation is usually called
#' Cohen's d, but sometimes called Hedges' g. Oftentimes, Cohen's d is called
#' Hedges' d as soon as the small sample correction factor is applied. Cumming
#' and Calin-Jageman (2017, p.171) recommended to avoid the term Hedges' g , but
#' to report which standard deviation was used to standardized the mean difference
#' (e.g., unweighted/weighted pooled standard deviation, or the standard deviation
#' of the control group) and whether a small sample correction factor was applied.
#'
#' As for the terminology according to Lakens (2013), in a two-sample design (i.e.,
#' \code{paired = FALSE}) Cohen's \eqn{d_s} is computed when using \code{weighted = TRUE}
#' (default) and Hedges's \eqn{g_s} is computed when using \code{correct = TRUE}
#' in addition. In a paired-sample design (i.e., \code{paired = TRUE}), Cohen's
#' \eqn{d_z} is computed when using \code{weighted = TRUE, default}, while Cohen's
#' \eqn{d_{rm}} is computed when using \code{weighted = FALSE} and
#' \code{cor = TRUE, default} and Cohen's \eqn{d_{av}} is computed when using
#' \code{weighted = FALSE} and \code{cor = FALSE}. Corresponding Hedges' \eqn{g_z},
#' \eqn{g_{rm}}, and \eqn{g_{av}} are computed when using \code{correct = TRUE}
#' in addition.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{test.t}}, \code{\link{test.z}}, \code{\link{effsize}},
#' \code{\link{cor.matrix}}
#'
#' @references
#' Cohen, J. (1988). \emph{Statistical power analysis for the behavioral sciences}
#' (2nd ed.). Academic Press.
#'
#' Cumming, G., & Calin-Jageman, R. (2017). \emph{Introduction to the new statistics:
#' Estimation, open science, & beyond}. Routledge.
#'
#' Glass. G. V., McGaw, B., & Smith, M. L. (1981). \emph{Meta-analysis in social
#' research}. Sage Publication.
#'
#' Goulet-Pelletier, J.-C., & Cousineau, D. (2018) A review of effect sizes and
#' their confidence intervals, Part I: The Cohen's d family. \emph{The Quantitative
#' Methods for Psychology, 14}, 242-265. https://doi.org/10.20982/tqmp.14.4.p242
#'
#' Hedges, L. V. (1981). Distribution theory for Glass's estimator of effect size
#' and related estimators. \emph{Journal of Educational Statistics, 6}(3), 106-128.
#'
#' Hedges, L. V. & Olkin, I. (1985). \emph{Statistical methods for meta-analysis}.
#' Academic Press.
#'
#' Lakens, D. (2013). Calculating and reporting effect sizes to facilitate cumulative
#' science: A practical primer for t-tests and ANOVAs. \emph{Frontiers in Psychology, 4},
#' 1-12. https://doi.org/10.3389/fpsyg.2013.00863
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#'
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{sample}}{type of sample, i.e., one-, two-, or, paired-sample}
#' \item{\code{data}}{matrix or data frame specified in \code{x}  }
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{result}}{result table}
#'
#' @export
#'
#' @examples
#' #----------------------------------------------------------------------------
#' # One-sample design
#'
#' # Example 1a: Cohen's d.z with two-sided 95% CI
#' # population mean = 3
#' cohens.d(mtcars$mpg, mu = 20)
#'
#' # Example 1b: Cohen's d.z (aka Hedges' g.z) with two-sided 95% CI
#' # population mean = 3, with small sample correction factor
#' cohens.d(mtcars$mpg, mu = 20, correct = TRUE)
#'
#' # Example 1c: Cohen's d.z with two-sided 95% CI
#' # population mean = 3, by 'vs' separately
#' cohens.d(mtcars$mpg, mu = 20, group = mtcars$vs)
#'
#' # Example 1d: Cohen's d.z with two-sided 95% CI
#' # population mean = 20, split analysis by 'vs'
#' cohens.d(mtcars$mpg, mu = 20, split = mtcars$vs)
#'
#' # Example 1e: Cohen's d.z with two-sided 95% CI
#' # population mean = 3, by 'vs' separately, split by 'am'
#' cohens.d(mtcars$mpg, mu = 20, group = mtcars$vs, split = mtcars$am)
#'
#' #----------------------------------------------------------------------------
#' # Two-sample design
#'
#' # Example 2a: Cohen's d.s with two-sided 95% CI
#' # weighted pooled SD
#' cohens.d(mpg ~ vs, data = mtcars)
#'
#' # Example 2b: Cohen's d.s with two-sided 99% CI
#' # weighted pooled SD
#' cohens.d(mpg ~ vs, data = mtcars, conf.level = 0.99)
#'
#' # Example 2c: Cohen's d.s with one-sided 99% CI
#' # weighted pooled SD
#' cohens.d(mpg ~ vs, data = mtcars, alternative = "greater", conf.level = 0.99)
#'
#' # Example 2d: Cohen's d.s for more than one variable with two-sided 95% CI
#' # weighted pooled SD
#' cohens.d(cbind(mpg, disp, hp) ~ vs, data = mtcars)
#'
#' # Example 2e: Cohen's d with two-sided 95% CI
#' # unweighted SD
#' cohens.d(mpg ~ vs, data = mtcars, weighted = FALSE)
#'
#' # Example 2f: Cohen's d.s (aka Hedges' g.s) with two-sided 95% CI
#' # weighted pooled SD, with small sample correction factor
#' cohens.d(mpg ~ vs, data = mtcars, correct = TRUE)
#'
#' # Example 2g: Cohen's d (aka Hedges' g) with two-sided 95% CI
#' # Unweighted SD, with small sample correction factor
#' cohens.d(mpg ~ vs, data = mtcars, weighted = FALSE, correct = TRUE)
#'
#' # Example 2h: Cohen's d (aka Glass's delta) with two-sided 95% CI
#' # SD of reference group 1
#' cohens.d(mpg ~ vs, data = mtcars, ref = 0)
#'
#' # Example 2i: Cohen's d.s with two-sided 95% CI
#' # weighted pooled SD, by 'am' separately
#' cohens.d(mpg ~ vs, data = mtcars, group = mtcars$am)
#'
#' # Example 2j: Cohen's d.s with two-sided 95% CI
#' # weighted pooled SD, split analysis by 'am'
#' cohens.d(mpg ~ vs, data = mtcars, split = mtcars$am)
#'
#' #----------------------------------------------------------------------------
#' # Paired-sample design
#'
#' # Example 3a: Cohen's d.z with two-sided 95% CI
#' # SD of the difference scores
#' cohens.d(mtcars$drat, mtcars$wt, paired = TRUE)
#'
#' # Example 3b: Cohen's d.z with one-sided 99% CI
#' # SD of the difference scores
#' cohens.d(mtcars$drat, mtcars$wt, paired = TRUE, alternative = "greater",
#'          conf.level = 0.99)
#'
#' # Example 3c: Cohen's d.rm with two-sided 95% CI
#' # controlling for the correlation between measures
#' cohens.d(mtcars$drat, mtcars$wt, paired = TRUE, weighted = FALSE)
#'
#' # Example 3d: Cohen's d.av with two-sided 95% CI
#' # without controlling for the correlation between measures
#' cohens.d(mtcars$drat, mtcars$wt, paired = TRUE, weighted = FALSE, cor = FALSE)
#'
#' # Example 3e: Cohen's d.z (aka Hedges' g.z) with two-sided 95% CI
#' # SD of the differnece scores
#' cohens.d(mtcars$drat, mtcars$wt, paired = TRUE, correct = TRUE)
#'
#' # Example 3f: Cohen's d.rm (aka Hedges' g.rm) with two-sided 95% CI
#' # controlling for the correlation between measures
#' cohens.d(mtcars$drat, mtcars$wt, paired = TRUE, weighted = FALSE, correct = TRUE)
#'
#' # Example 3g: Cohen's d.av (aka Hedges' g.av) with two-sided 95% CI
#' # without controlling for the correlation between measures
#' cohens.d(mtcars$drat, mtcars$wt, paired = TRUE, weighted = FALSE, cor = FALSE,
#'          correct = TRUE)
#'
#' # Example 3h: Cohen's d.z with two-sided 95% CI
#' # SD of the difference scores, by 'vs' separately
#' cohens.d(mtcars$drat, mtcars$wt, paired = TRUE, group = mtcars$vs)
#'
#' # Example 3i:  Cohen's d.z with two-sided 95% CI
#' # SD of the difference scores, split analysis by 'vs'
#' cohens.d(mtcars$drat, mtcars$wt, paired = TRUE, split = mtcars$vs)
cohens.d <- function(x, ...) {

  UseMethod("cohens.d")

}

#_______________________________________________________________________________
#
# Default S3 method ------------------------------------------------------------

cohens.d.default <- function(x, y = NULL, mu = 0, paired = FALSE, weighted = TRUE, cor = TRUE,
                             ref = NULL, correct = FALSE, alternative = c("two.sided", "less", "greater"),
                             conf.level = 0.95, group = NULL, split = NULL, sort.var = FALSE,
                             digits = 2, as.na = NULL, write = NULL, append = TRUE,
                             check = TRUE, output = TRUE, ...) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'x' is missing or NULL
  if (isTRUE(missing(x) || is.null(x))) { stop("Please specify a numeric vector for the argument 'x'", call. = FALSE) }

  # Convert 'x' into a vector
  x <- unlist(x, use.names = FALSE)

  if (isTRUE(!is.null(y))) {

    # Check if only one variable specified in the input 'y'
    if (ncol(data.frame(y)) != 1L) { stop("More than one variable specified for the argument 'x'.",call. = FALSE) }

    # Convert 'y' into a vector
    y <- unlist(y, use.names = FALSE)

  }

  # Check input 'paired'
  if (isTRUE(!is.logical(paired))) { stop("Please specify TRUE or FALSE for the argument 'paired'.", call. = FALSE) }


  if (isTRUE(paired)) {

    # Length of 'x' and 'y'
    if (isTRUE(nrow(data.frame(x)) != nrow(data.frame(y)))) { stop("Length of the vector specified in 'x' does not match the length of the vector specified in 'y'.", call. = FALSE) }

  }

  # Check 'group'
  if (isTRUE(!is.null(group))) {

    if (ncol(data.frame(group)) != 1L) { stop("More than one grouping variable specified for the argument 'group'.",call. = FALSE) }

    if (isTRUE(paired)) {

      if (nrow(data.frame(group)) != nrow(data.frame(x))) { stop("Length of the vector or factor specified in the argument 'group' does not match with 'x'.", call. = FALSE) }

    }

    # Convert 'group' into a vector
    group <- unlist(group, use.names = FALSE)

  }

  # Check 'split'
  if (isTRUE(!is.null(split))) {

    if (ncol(data.frame(split)) != 1L) { stop("More than one split variable specified for the argument 'split'.",call. = FALSE) }

    if (isTRUE(paired)) {

      if (nrow(data.frame(split)) != nrow(data.frame(x))) { stop("Length of the vector or factor specified in the argument 'split' does not match with 'x'.", call. = FALSE) }

    }

    # Convert 'split' into a vector
    split <- unlist(split, use.names = FALSE)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## List or Dataframe ####

  #...................
  ### One-sample design ####
  if (is.null(y)) {

    if (isTRUE(is.null(dim(x)))) {

      xy  <- data.frame(x = x)

    } else {

      xy <- x

    }

  #...................
  ### Two-sample design ####
  } else if (!isTRUE(paired)) {

    xy <- list(x = x, y = y)

  #...................
  ### Paired-sample design ####
  } else if (isTRUE(paired)) {

    xy <- data.frame(x = x, y = y)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) { xy <- .as.na(xy, na = as.na) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Listwise deletion ####

  if (isTRUE(paired && nrow(na.omit(xy)) < 2L)) { stop("After listwise deletion, the number of pairs of observations is less than two.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("weighted", "cor", "correct", "sort.var", "append", "output"),
               numeric = list(mu = 1L),
               args = c("alternative", "conf.level", "digits", "write1"), envir = environment(), input.check = check)

  # Additional checks
  if (isTRUE(check)) {

    # Check input 'ref'
    if (isTRUE(!is.null(ref))) { if (isTRUE(!isTRUE(ref %in% c("x", "y")))) { stop("Please specify \"x\" or \"y\" for the argument 'ref'.", call. = FALSE) } }

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

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Type of sample ####

  # One-sample
  if (isTRUE(is.null(y))) {

    sample <- "one"

  # Two-sample
  } else if (!isTRUE(paired)) {

    sample <- "two"

  # Paired-sample
  } else if (isTRUE(paired)) {

    sample <- "paired"

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Alternative hypothesis ####

  if (isTRUE(all(c("two.sided", "less", "greater") %in% alternative))) { alternative <- "two.sided" }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## No Grouping, No Split ####
  if (isTRUE(is.null(group) && is.null(split))) {

    #...................
    ### One-sample ####
    switch(sample, one = {

      # Compute Cohen's d and confidence intervals
      temp <- Reduce(function(xx, yy) rbind(xx, yy, make.row.names = FALSE),
                     lapply(names(xy), function(z) .internal.d.function(eval(parse(text = "xy[, z]")), eval(parse(text = "y")),
                                                                        paired = paired, weighted = weighted,
                                                                        cor = cor, ref = ref, correct = correct, mu = mu,
                                                                        alternative = alternative, conf.level = conf.level)))

      result <- data.frame(variable = colnames(xy),
                           n = sapply(xy, function(y) length(na.omit(y))),
                           nNA = sapply(xy, function(y) length(attributes(na.omit(y))$na.action)),
                           m = sapply(xy, function(y) mean(y, na.rm = TRUE)),
                           m.diff = temp$m.diff,
                           sd = temp$sd,
                           d = temp$d,
                           se = temp$se,
                           low = temp$low,
                           upp = temp$upp,
                           stringsAsFactors = FALSE, row.names = NULL)

    #...................
    ### Two-sample ####
    }, two = {

      # Compute Cohen's d and confidence intervals
      temp <- .internal.d.function(x = x, y = y, paired = paired, weighted = weighted,
                                   cor = cor, ref = ref, correct = correct, mu = mu,
                                   alternative = alternative, conf.level = conf.level)

      result <- misty::df.rbind(data.frame(variable = "y",
                                           between = 1,
                                           n = length(na.omit(xy$x)),
                                           nNA = sum(is.na(xy$x)),
                                           m = mean(xy$x, na.rm = TRUE),
                                           stringsAsFactors = FALSE),
                                data.frame(variable = "y",
                                           n = length(na.omit(xy$y)),
                                           between = 2,
                                           nNA = sum(is.na(xy$y)),
                                           m = mean(xy$y, na.rm = TRUE),
                                           m.diff = temp$m.diff,
                                           sd = temp$sd,
                                           d = temp$d,
                                           se = temp$se,
                                           low = temp$low,
                                           upp = temp$upp,
                                           stringsAsFactors = FALSE, row.names = NULL))

    #...................
    ### Paired-sample ####
    }, paired = {

      # Compute Cohen's d and confidence intervals
      temp <- .internal.d.function(x = x, y = y, paired = paired, weighted = weighted,
                                   cor = cor, ref = ref, correct = correct, mu = mu,
                                   alternative = alternative, conf.level = conf.level)

      result <- data.frame(variable = "y",
                           n = nrow(na.omit(xy)),
                           nNA = length(attributes(na.omit(xy))$na.action),
                           m1 = mean(xy$x, na.rm = TRUE),
                           m2 = mean(xy$y, na.rm = TRUE),
                           m.diff =  temp$m.diff,
                           sd = temp$sd,
                           d = temp$d,
                           se = temp$se,
                           low = temp$low,
                           upp = temp$upp,
                           stringsAsFactors = FALSE, row.names = NULL)

    })

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Grouping, No Split ####
  } else if (isTRUE(!is.null(group) && is.null(split))) {

    #...................
    ### One-sample ####
    if (isTRUE(sample == "one")) {

      object.group <- lapply(split(xy, f = group),
                             function(z) cohens.d.default(eval(parse(text = "z")), y = NULL,
                                                          mu = mu, paired = paired, weighted = weighted,
                                                          cor = cor, ref = ref, correct = correct, alternative = alternative,
                                                          conf.level = conf.level, group = NULL, split = NULL, sort.var = sort.var,
                                                          as.na = as.na, check = FALSE, output = FALSE)$result)

    #...................
    ### Two-sample and Paired-sample ####
    } else {

      object.group <- lapply(split(xy, f = group),
                             function(y) cohens.d.default(x = y$x, y = y$y, mu = mu, paired = paired, weighted = weighted,
                                                          cor = cor, ref = ref, correct = correct, alternative = alternative,
                                                          conf.level = conf.level, group = NULL, split = NULL, sort.var = sort.var,
                                                          as.na = as.na, check = FALSE, output = FALSE)$result)

    }

    result <- data.frame(group = rep(names(object.group), each = ncol(xy)),
                         eval(parse(text = paste0("rbind(", paste0("object.group[[", seq_len(length(object.group)), "]]",
                                                                   collapse = ", "), ")"))))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## No Grouping, Split ####
  } else if (isTRUE(is.null(group) && !is.null(split))) {

    #...................
    ### One-sample ####
    if (isTRUE(sample == "one")) {

      result <- lapply(split(xy, f = split),
                       function(z) cohens.d.default(eval(parse(text = "z")), y = NULL,
                                                    mu = mu, paired = paired, weighted = weighted,
                                                    cor = cor, ref = ref, correct = correct, alternative = alternative,
                                                    conf.level = conf.level, group = NULL, split = NULL, sort.var = sort.var,
                                                    as.na = as.na, check = FALSE, output = FALSE)$result)

    #...................
    ### Two-sample and Paired-sample ####
    } else {

      result <- lapply(split(xy, f = split),
                             function(y) cohens.d.default(x = y$x, y = y$y, mu = mu, paired = paired, weighted = weighted,
                                                          cor = cor, ref = ref, correct = correct, alternative = alternative,
                                                          conf.level = conf.level, group = NULL, split = NULL, sort.var = sort.var,
                                                          as.na = as.na, check = FALSE, output = FALSE)$result)

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Grouping, Split ####
  } else if (isTRUE(!is.null(group) && !is.null(split))) {

    #...................
    ### One-sample ####
    if (isTRUE(sample == "one")) {

      result <- lapply(split(data.frame(xy, .group = group, row.names = NULL), f = split),
                       function(z) cohens.d.default(eval(parse(text = "z[, -grep('.group', names(z))]")), y = NULL,
                                                    mu = mu, paired = paired, weighted = weighted,
                                                    cor = cor, ref = ref, correct = correct, alternative = alternative,
                                                    conf.level = conf.level, group = z$.group, split = NULL, sort.var = sort.var,
                                                    as.na = as.na, check = FALSE, output = FALSE)$result)

    #...................
    ### Two-sample and Paired-sample ####
    } else {

      result <- lapply(split(data.frame(xy, .group = group, row.names = NULL), f = split),
                       function(z) cohens.d.default(x = z$x, y = z$y, mu = mu, paired = paired, weighted = weighted,
                                                    cor = cor, ref = ref, correct = correct, alternative = alternative,
                                                    conf.level = conf.level, group = z$.group, split = NULL, sort.var = sort.var,
                                                    as.na = as.na, check = FALSE, output = FALSE)$result)

    }

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "cohens.d",
                 sample = sample,
                 data = list(x = x, y = y, group = group, split = split),
                 args = list(paired = paired, weighted = weighted, cor = cor,
                             correct = correct, mu = mu, alternative = alternative,
                             conf.level = conf.level, sort.var = sort.var,
                             na.omit = na.omit, digits = digits, as.na = as.na,
                             write = NULL, append = TRUE, check = check, output = output),
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
#
# S3 method for class 'formula' ------------------------------------------------

cohens.d.formula <- function(formula, data, weighted = TRUE, cor = TRUE,
                             ref = NULL, correct = FALSE,
                             alternative = c("two.sided", "less", "greater"),
                             conf.level = 0.95, group = NULL, split = NULL,
                             sort.var = FALSE, na.omit = FALSE, digits = 2,
                             as.na = NULL, write = NULL, append = TRUE,
                             check = TRUE, output = TRUE, ...) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'formula' is missing
  if (isTRUE(missing(formula))) { stop("Please specify a formula using the argument 'formula'", call. = FALSE) }

  # Check if input 'data' is missing or NULL
  if (isTRUE(missing(data) || is.null(data))) { stop("Please specify a matrix or data frame for the argument 'x'.", call. = FALSE) }

  # Check 'group'
  if (isTRUE(!is.null(group))) {

    if (ncol(data.frame(group)) != 1L) { stop("More than one grouping variable specified for the argument 'group'.",call. = FALSE) }

    if (nrow(data.frame(group)) != nrow(data)) { stop("Length of the vector or factor specified in the argument 'group' does not match the number of rows in 'data'.", call. = FALSE) }

    # Convert 'group' into a vector
    group <- unlist(group, use.names = FALSE)

  }

  # Check 'split'
  if (isTRUE(!is.null(split))) {

    if (ncol(data.frame(split)) != 1L) { stop("More than one split variable specified for the argument 'split'.",call. = FALSE) }

    if (nrow(data.frame(split)) != nrow(data)) { stop("Length of the vector or factor specified in the argument 'split' does not match the number of rows in 'data'.", call. = FALSE) }

    # Convert 'split' into a vector
    split <- unlist(split, use.names = FALSE)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## As data frame ####

  data <- as.data.frame(data)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Variables ####

  var.formula <- all.vars(as.formula(formula))

  # Grouping variable
  group.var <- attr(terms(formula[-2L]), "term.labels")

  # Outcome(s)
  y.vars <- var.formula[-grep(group.var, var.formula)]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Check ####

  # Check if variables are in the data
  (!var.formula %in% colnames(data)) |>
    (\(y) if (isTRUE(any(y))) { stop(paste0("Variables specified in the the formula were not found in 'data': ", paste(var.formula[which(y)], collapse = ", ")), call. = FALSE) })()

  # Check if input 'formula' has only one grouping variable
  if (isTRUE(length(group.var) != 1L)) { stop("Please specify a formula with only one grouping variable.", call. = FALSE) }

  # Check if grouping variable has two levels
  if (isTRUE(length(na.omit(unique(data[, group.var]))) != 2L)) { stop("Please specify a grouping variable with exactly two levels.", call. = FALSE) }

  # Check if 'ref' is in the grouping variable
  if (!isTRUE(is.null(ref))) { if (!isTRUE(ref %in% data[, group.var])) { stop("Reference group specified in the argument 'ref' is not represented in the grouping variable.", call. = FALSE) } }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) {

    # Replace user-specified values with missing values
    data[, y.vars] <- misty::as.na(data[, y.vars], na = as.na, check = check)

    # Variable with missing values only
    vapply(data[, y.vars, drop = FALSE], function(y) all(is.na(y)), FUN.VALUE = logical(1L)) |>
      (\(y) if (any(y)) { stop(paste0("After converting user-missing values into NA, following variables are completely missing: ", paste(names(which(y)), collapse = ", ")), call. = FALSE) })()

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Listwise deletion ####

  if (isTRUE(na.omit && any(is.na(data[, var.formula])))) {

    #...................
    ### No group and split variable ####
    if (isTRUE(is.null(group) && is.null(split))) {

      x <- na.omit(as.data.frame(data[, var.formula]))

      warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ", length(attributes(x)$na.action)), call. = FALSE)

    #...................
    ### Group variable, no split variable ####
    } else if (isTRUE(!is.null(group) && is.null(split))) {

      data.group <- na.omit(data.frame(data[, var.formula], group = group))

      data <- data.group[, -grep("group", names(data.group)), drop = FALSE]
      group <- data.group$group

      warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ", length(attributes(data.group)$na.action)), call. = FALSE)

    #...................
    ### No group variable, split variable ####
    } else if (isTRUE(is.null(group) && !is.null(split))) {

      data.split <- na.omit(data.frame(data[, var.formula], split = split))

      data <- data.split[, -grep("split", names(data.split)), drop = FALSE]
      split <- data.split$split

      warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ", length(attributes(data.split)$na.action)), call. = FALSE)

    #...................
    ### Group variable, split variable ####
    } else if (isTRUE(!is.null(group) && !is.null(split))) {

      data.group.split <- na.omit(data.frame(data[, var.formula], group = group, split = split))

      data <- data.group.split[,  !names(data.group.split) %in% c("group", "split"), drop = FALSE]
      group <- data.group.split$group
      split <- data.group.split$split

      warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ", length(attributes(data.group.split)$na.action)), call. = FALSE)

    }

    #...................
    ### Variable with missing values only ####

    data.miss <- vapply(data[, var.formula], function(y) all(is.na(y)), FUN.VALUE = logical(1L))
    if (isTRUE(any(data.miss))) {

      stop(paste0("After listwise deletion, following variables are completely missing: ", paste(names(which(data.miss)), collapse = ", ")), call. = FALSE)

    }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Alternative hypothesis ####

  if (isTRUE(all(c("two.sided", "less", "greater") %in% alternative))) { alternative <- "two.sided" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Reference group ####

  ref.return <- ref

  if (!isTRUE(is.null(ref))) { ifelse(which(unique(sort(na.omit(data[, group.var]))) %in% ref) == 1, ref <- "x", ref <- "y") }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## No Grouping, No Split ####
  if (isTRUE(is.null(group) && is.null(split))) {

    result <- data.frame(matrix(NA, ncol = 11L, nrow = length(y.vars)*2L, dimnames = list(NULL, c("variable", "between", "n", "nNA", "m", "m.diff", "sd", "d", "se", "low", "upp"))))

    loop.mat <- matrix(1:(length(y.vars)*2), ncol = 2L, byrow = TRUE)

    # Loop over outcome variables
    for (i in seq_along(y.vars)) {

      data.split <- split(data[, y.vars[i]], f = data[, group.var])

      # Check if variance in both groups
      var.check <- unlist(lapply(data.split, var)) == 0L
      if (isTRUE(any(var.check))) {

        if (sum(var.check) == 2L) {

          stop(paste0("There is no variance in both groups in the variable ", y.vars[i]), call. = FALSE)


        } else {

          stop(paste0("There is no variance in group '", names(which(var.check)), "' in the variable ", y.vars[i]), call. = FALSE)

        }

      }

      result[loop.mat[i, ], ] <- data.frame(variable = y.vars[i],
                                            cohens.d.default(x = data.split[[1L]],
                                                             y = data.split[[2L]],
                                                             paired = FALSE, weighted = weighted, cor = cor,
                                                             ref = ref, correct = correct, alternative = alternative,
                                                             conf.level = conf.level, group = NULL, split = NULL,
                                                             sort.var = sort.var, digits = digits, as.na = NULL,
                                                             check = check, output = FALSE)$result[, -1L])

      result[loop.mat[i, ], "between"] <- names(data.split)

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Grouping, No Split ####
  } else if (isTRUE(!is.null(group) && is.null(split))) {

    object.group <- lapply(split(data[, var.formula], f = group),
                           function(y) cohens.d.formula(formula, data = y, weighted = weighted, cor = cor,
                                                        ref = ref, correct = correct, alternative = alternative,
                                                        conf.level = conf.level, group = NULL, split = NULL,
                                                        sort.var = sort.var, digits = digits, as.na = NULL,
                                                        check = check, output = FALSE)$result)

    result <- data.frame(group = rep(names(object.group), each = length(y.vars)*2),
                         eval(parse(text = paste0("rbind(", paste0("object.group[[", seq_len(length(object.group)), "]]",
                                                                   collapse = ", "), ")"))))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## No Grouping, Split ####
  } else if (isTRUE(is.null(group) && !is.null(split))) {

    result <- lapply(split(data[, var.formula], f = split),
                     function(y) cohens.d.formula(formula, data = y, weighted = weighted, cor = cor,
                                                  ref = ref, correct = correct, alternative = alternative,
                                                  conf.level = conf.level, group = NULL, split = NULL,
                                                  sort.var = sort.var, digits = digits, as.na = NULL,
                                                  check = check, output = FALSE)$result)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Grouping, Split ####
  } else if (isTRUE(!is.null(group) && !is.null(split))) {

    result <- lapply(split(data.frame(data[, var.formula], .group = group), f = split),
                     function(y) cohens.d.formula(formula, data = y, weighted = weighted, cor = cor,
                                                  ref = ref, correct = correct, alternative = alternative,
                                                  conf.level = conf.level, group = y$.group, split = NULL,
                                                  sort.var = sort.var, digits = digits, as.na = NULL,
                                                  check = check, output = FALSE)$result)

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "cohens.d",
                 sample = "two",
                 data = list(data = data[, var.formula], group = group, split = split),
                 args = list(formula = formula, weighted = weighted, cor = cor,
                             ref = ref.return, correct = correct, alternative = alternative,
                             conf.level = conf.level, sort.var = sort.var,
                             na.omit = na.omit, digits = digits, as.na = as.na,
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

#_______________________________________________________________________________
