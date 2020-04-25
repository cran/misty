#' Cohen's d for Between- and Within-Subject Design
#'
#' This function computes Cohen's d for between- and within-subject designs with confidence intervals.
#' By default, the function computes the standardized mean difference divided by the weighted pooled
#' standard deviation without applying the correction factor for removing the small sample bias.
#'
#' Cohen (1988, p.67) proposed to compute the standardized mean difference by dividing the
#' mean difference by the unweighted pooled standard deviation (i.e., \code{weighted = FALSE}).
#'
#' Glass et al. (1981, p. 29) suggested to use the standard deviation of the control group
#' (e.g., \code{ref = "control"}) to compute the standardized mean difference since the
#' standard deviation of the control group is unaffected by the treatment and will therefore
#' more closely reflect the population standard deviation.
#'
#' Hedges (1981, p. 110) recommended to weight each group's standard deviation by its sample
#' size resulting in a weighted and pooled standard deviation (i.e., \code{weighted = TRUE}).
#' According to Hedges and Olkin (1985, p. 81), the standardized mean difference based
#' on the weighted and pooled standard deviation has a positive small sample bias, i.e.,
#' standardized mean difference is overestimates in small samples (i.e., sample size less
#' than 20 or less than 10 in each group). However, a correction factor can be applied to
#' remove the small sample bias (i.e., \code{correct = TRUE}). Note that a gamma function
#' is used for computing the correction factor when \eqn{n} < 200, while a approximation method
#' is used when \eqn{n} >= 200.
#'
#' Note that the terminology is inconsistent because the standardized mean difference based
#' on the weighted and pooled standard deviation is usually called Cohen's d, but sometimes
#' called Hedges' g. Oftentimes, Cohen's d is called Hedges' d as soon as the correction factor
#' is applied. It is recommended to avoid the term Hedges' g (Cumming & Calin-Jageman, 2017, p. 171),
#' but to report which standard deviation was used to standardized the mean difference
#' (e.g., unweighted/weighted pooled standard deviation, or the standard deviation of the control group)
#' and whether a small sample correction factor was applied.
#'
#' As for the terminology according to Lakens (2013), in between subject design (\code{paired = FALSE})
#' Cohen's \eqn{d_s} is computed when using \code{weighted = TRUE} and Hedges's \eqn{g_s} is computed when
#' using \code{correct = TRUE} in addition. In within-subject designs (\code{paired = TRUE}),
#' Cohen's \eqn{d_rm} is computed when using \code{weighted = TRUE}, while Cohen's \eqn{d_av} is computed
#' when using \code{weighted = FALSE}, and corresponding Hedges' \eqn{g_rm} and Hedges' \eqn{g_av} are computed
#' when using \code{correct = TRUE}.
#'
#' @param formula     in case of a between-subject design (i.e., \code{paired = FALSE}),
#'                    a formula of the form \code{y ~ group} for one outcome variable or
#'                    \code{cbind(y1, y2, y3) ~ group} for more than one outcome variable where
#'                    \code{y} is a numeric variable giving the data values and \code{group}
#'                    a numeric variable, character variable or factor with two values or factor
#'                    levels giving the corresponding group; in case of a within-subject design
#'                    (i.e., \code{paired = TRUE}), a formula of the form \code{post ~ pre} where
#'                    \code{post} and \code{pre} are numeric variables. Note that analysis for more
#'                    than one outcome variable is not permitted in within-subject design.
#' @param data        a matrix or data frame containing the variables in the \code{formula}.
#' @param paired      logical: if \code{TRUE}, Cohen's d for within-subject design is computed.
#' @param weighted    logical: if \code{TRUE} (default), in case of a between-subject design the weighted
#'                    pooled standard deviation is used; in case of a within-subject design the correlation
#'                    between measures is controlled when computing the pooled standard deviation.
#' @param correct     logical: if \code{TRUE}, correction factor to remove positive bias in small
#'                    samples is used. Note that correction factor is only applied when \code{weighted = TRUE}
#'                    and \code{ref = NULL}.
#' @param ref         a numeric value or character string indicating the reference group in a between-subject
#'                    design or a character string indicating the reference variable in a within-subject
#'                    design. The standard deviation of the reference group or reference variable is used
#'                    to standardized the mean difference. If the standard deviation of the control group
#'                    is used (e.g. \code{group = "control"}), the effect size is usually called Glass' delta.
#' @param digits      an integer value indicating the number of decimal places to be used for
#'                    displaying results.
#' @param conf.level  a numeric value between 0 and 1 indicating the confidence level of the interval.
#' @param as.na       a numeric vector indicating user-defined missing values,
#'                    i.e. these values are converted to \code{NA} before conducting the analysis.
#'                    Note that \code{as.na()} function is only applied to \code{y} but not to \code{group}
#'                    in a between-subject design, while \code{as.na()} function is applied to \code{pre} and
#'                    \code{post} in a within-subject design.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#' @param output      logical: if \code{TRUE}, output is shown on the console.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{eta.sq}}, \code{\link{cont.coef}}, \code{\link{cramers.v}},\code{\link{cor.matrix}},
#' \code{\link{na.auxiliary}}
#'
#' @references
#' Cohen, J. (1988). \emph{Statistical power analysis for the behavioral sciences} (2nd ed.).
#' Academic Press.
#'
#' Cumming, G., & Calin-Jageman, R. (2017). \emph{Introduction to the new statistics: Estimation, open science,
#' & beyond}. Routledge.
#'
#' Glass. G. V., McGaw, B., & Smith, M. L. (1981). \emph{Meta-analysis in social research}. Sage Publication.
#'
#' Goulet-Pelletier, J.-C., & Cousineau, D. (2018) A review of effect sizes and their confidence intervals,
#' Part I: The Cohen's d family. \emph{The Quantitative Methods for Psychology, 14}, 242-265.
#' https://doi.org/10.20982/tqmp.14.4.p242
#'
#' Hedges, L. V. (1981). Distribution theory for Glass's estimator of effect size and related estimators.
#' \emph{Journal of Educational Statistics, 6}(3), 106-128.
#'
#' Hedges, L. V. & Olkin, I. (1985). \emph{Statistical methods for meta-analysis}. Academic Press.
#'
#' Lakens, D. (2013). Calculating and reporting effect sizes to facilitate cumulative science:
#' A practical primer for t-tests and ANOVAs. \emph{Frontiers in Psychology, 4}, 1-12.
#' https://doi.org/10.3389/fpsyg.2013.00863
#'
#' @return
#' Returns an object of class \code{cohens.d}, which is a list with following entries: function call (\code{call}),
#' matrix or data frame specified in \code{x} (\code{data}), specification of function arguments (\code{args}), and
#' list with results (\code{result}).
#'
#' @export
#'
#' @examples
#' #--------------------------------------
#' # Between-Subject Design
#' dat.bs <- data.frame(group = c("cont", "cont", "cont", "treat",  "treat"),
#'                      y1 = c(1, 3, 2, 5, 7),
#'                      y2 = c(4, 3, 3, 6, 4),
#'                      y3 = c(7, 5, 7, 3, 2), stringsAsFactors = FALSE)
#'
#' # Standardized mean difference divided by the weighted pooled
#' # standard deviation without small sample correction factor
#' cohens.d(y1 ~ group, data = dat.bs)
#'
#' # Standardized mean difference divided by the unweighted pooled
#' # standard deviation without small sample correction factor
#' cohens.d(y1 ~ group, data = dat.bs, weighted = FALSE)
#'
#' # Standardized mean difference divided by the weighted pooled
#' # standard deviation with small sample correction factor
#' cohens.d(y1 ~ group, data = dat.bs, correct = TRUE)
#'
#' # Standardized mean difference divided by the standard deviation
#' # of the control group without small sample correction factor
#' cohens.d(y1 ~ group, data = dat.bs, ref = "cont")
#'
#' # Cohens's d for for more than one outcome variable
#' cohens.d(cbind(y1, y2, y3) ~ group, data = dat.bs)
#'
#' #--------------------------------------
#' # Within-Subject Design
#' dat.ws <- data.frame(pre = c(1, 3, 2, 5, 7),
#'                      post = c(2, 2, 1, 6, 8), stringsAsFactors = FALSE)
#'
#' # Standardized mean difference divided by the pooled
#' # standard deviation while controlling for the correlation
#' # without small sample correction factor
#' cohens.d(post ~ pre, data = dat.ws, paired = TRUE)
#'
#' # Standardized mean difference divided by the pooled
#' # standard deviation whithout controlling for the correlation
#' # without small sample correction factor
#' cohens.d(post ~ pre, data = dat.ws, paired = TRUE, weighted = FALSE)
#'
#' # Standardized mean difference divided by the pooled
#' # standard deviation while controlling for the correlation
#' # with small sample correction factor
#' cohens.d(post ~ pre, data = dat.ws, paired = TRUE, correct = TRUE)
#'
#' # Standardized mean difference divided by the standard deviation
#' # of the pretest without small sample correction factor
#' cohens.d(post ~ pre, data = dat.ws, paired = TRUE, ref = "pre")
cohens.d <- function(formula, data, paired = FALSE, weighted = TRUE, ref = NULL, correct = FALSE,
                     digits = 2, conf.level = 0.95, as.na = NULL, check = TRUE, output = TRUE) {

  ####################################################################################
  # Data

  #......
  # Check if input 'formula' is missing
  if (missing(formula)) {

    stop("Please specify a formula for the argument 'formula'.", call. = FALSE)

  }

  #......
  # Check if input 'data' is missing
  if (missing(data)) {

    stop("Please specify a matrix or data frame for the argument 'data'.", call. = FALSE)

  }

  #......
  # Check if input 'data' is NULL
  if (is.null(data)) {

    stop("Input specified for the argument 'data' is NULL.", call. = FALSE)

  }

  #-----------------------------------------------------------------------------------
  # Formula

  #.........................................
  # Variables
  var.formula <- all.vars(as.formula(formula))

  #.........................................
  # Between-subject design
  if (!isTRUE(paired)) {

    # Outcome(s)
    y.var <- var.formula[-length(var.formula)]

    # Grouping variable
    group.var <- var.formula[length(var.formula)]

  #.........................................
  # Within-subject design
  } else {

    # Outcome
    y.var <- var.formula

  }

  #.........................................
  # Data
  data <- as.data.frame(data[, var.formula], stringsAsFactors = FALSE)

  #-----------------------------------------------------------------------------------
  # Convert user-missing values into NA

  if (!is.null(as.na)) {

    # Between-subject design
    if (!isTRUE(paired)) {

      data[, -grep(group.var, names(data))] <- misty::as.na(data[, -grep(group.var, names(data))], as.na = as.na, check = FALSE)

    # Within-subject design
    } else {

      data <- misty::as.na(data, as.na = as.na, check = FALSE)

    }

    # Variable with missing values only
    x.miss <- vapply(data, function(y) all(is.na(y)), FUN.VALUE = logical(1L))
    if (any(x.miss)) {

      stop(paste0("After converting user-missing values into NA, following variables are completely missing: ",
                  paste(names(which(x.miss)), collapse = ", ")), call. = FALSE)

    }

    # Zero variance
    x.zero.var <- vapply(data, function(y) length(na.omit(unique(y))) == 1L, FUN.VALUE = logical(1))
    if (any(x.zero.var)) {

      stop(paste0("After converting user-missing values into NA, following variables have zero variance: ",
                  paste(names(which(x.zero.var)), collapse = ", ")), call. = FALSE)

    }

  }

  ####################################################################################
  # Input Check

  # Check input 'check'
  if (!isTRUE(isTRUE(check) || !isTRUE(check))) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }

  #-----------------------------------------

  if (isTRUE(check)) {

    #......
    # Formula variables in 'data'?
    if (!all(var.formula %in% colnames(data))) {

      stop("Variables specified in the argument 'formula' were not found in 'data'.", call. = FALSE)

    }

    #......
    # Check input 'paired'
    if (!isTRUE(isTRUE(paired) || !isTRUE(paired))) {

      stop("Please specify TRUE or FALSE for the argument 'paired'.", call. = FALSE)

    }

    #......
    if (!isTRUE(paired)) {

      if (length(unique(data[, group.var])) != 2L) {

        stop("Grouping variable specified in 'formula' does hot have two values or factor levels.",
             call. = FALSE)

      }

    } else {

      if (ncol(data) > 2L) {

        stop("Analysis for morethan one outcome variable is not permitted in within-subject design.",
             call. = FALSE)

      }

    }

    #......
    # Check input 'weighted'
    if (!isTRUE(isTRUE(weighted) || !isTRUE(weighted))) {

      stop("Please specify TRUE or FALSE for the argument 'weighted'.", call. = FALSE)

    }

    #......
    # Check input 'correct'
    if (!isTRUE(isTRUE(correct) || !isTRUE(correct))) {

      stop("Please specify TRUE or FALSE for the argument 'correct'.", call. = FALSE)

    }

    #......
    # Check input 'correct'
    if ((!isTRUE(weighted) && isTRUE(correct)) || (!is.null(ref) && isTRUE(correct))) {

      stop("Small sample correction factor is only applied when weighted = TRUE and ref = NULL.", call. = FALSE)

    }

    #......
    # Check input 'digits'
    if (digits %% 1L != 0L || digits < 0L) {

      stop("Specify a positive integer number for the argument 'digits'.", call. = FALSE)

    }

    #......
    # Check input 'group'
    if (!is.null(ref)) {

      # Between-subject design
      if (!isTRUE(paired)) {

        if (!ref %in% data[, group.var]) {

           stop("Numeric value or character string specified in 'ref' was not found in the grouping variable specified in 'formula'.",
                call. = FALSE)

        }

      } else {

        if (!ref %in% colnames(data)) {

          stop("Numeric value or character string specified in 'ref' was not found in 'data'.",
               call. = FALSE)

        }


      }

    }

    #......
    # Check input 'conf.level'
    if (conf.level >= 1L || conf.level <= 0L) {

      stop("Please specifiy a numeric value between 0 and 1 for the argument 'conf.level'.",
           call. = FALSE)

    }

    #......
    # Check input 'output'
    if (!isTRUE(isTRUE(output) || !isTRUE(output))) {

      stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE)

    }

  }

  ####################################################################################
  # Main Function

  #----------------------------------------
  # One outcome variable

  if ((!isTRUE(paired) && length(y.var) == 1L) || (isTRUE(paired) && length(y.var) == 2L))  {

    #............................................................
    # Between-subject design
    if (!isTRUE(paired)) {

      #...................
      # Data and Arguments

      # Outcome
      x.dat <- data[, y.var]

      # Grouping
      group.dat <- data[, group.var]

      #...................
      # Descriptives

      res.descript <- Reduce(function(xx, yy) rbind(xx, yy),
                             suppressWarnings(misty::descript(x.dat, split = group.dat, check = FALSE, output = FALSE))$result)

      # Mean difference
      x.diff <- diff(res.descript[, "m"])

      # Sample size by group
      n.group <- res.descript[, "n"]

      #...................
      # Standard deviation

      # Cohens d
      if (is.null(ref)) {

        # Variance by group
        var.group <- res.descript[, "var"]

        # Weighted pooled standard deviation
        if (isTRUE(weighted)) {

          sd.group <- sqrt(((n.group[1L] - 1L)*var.group[1] + (n.group[2L] - 1L)*var.group[2L]) / (sum(n.group) - 2L))

        # Unweigted pooled standard deviation
        } else {

          sd.group <- sum(res.descript["var"]) / 2L

        }

      #...................
      # Standard deviation from a specific reference group
      } else {

        sd.group <- sd(x.dat[which(group.dat == ref)], na.rm = TRUE)

      }

      #........................................
      # Cohen's d estimate

      estimate <- x.diff / sd.group

      #........................................
      # Correction factor

      v <- sum(n.group) - 2L

      # Correction factor based on gamma function
      if (sum(n.group) < 200L) {

        corr.factor <- gamma(0.5*v) / ((sqrt(v / 2)) * gamma(0.5 * (v - 1)))

      # Correction factor based on approximation
      } else {

        corr.factor <- (1L - (3L / (4L * v - 1L)))

      }

      # Bias-corrected Cohen's d
      if (isTRUE(correct) && isTRUE(weighted)) {

       estimate <- estimate*corr.factor

      }

      #........................................
      # Confidence interval

      if (isTRUE(weighted)) {

        # Harmonic mean
        n.harm <- 1 / mean(1 / n.group)

        # True standard error (Hedges, 1981)
        estimate.SE <- sqrt((v / (v - 2L)) * 2L / n.harm * (1L + estimate^2L * n.harm / 2L) - (estimate^2L / corr.factor^2L))

        # Noncentrality parameter
        ncp <- estimate * sqrt(n.harm / 2L)

        # Confidence interval around ncp
        conf.int.ncp  <- c(suppressWarnings(qt((1L - conf.level) / 2L, v, ncp = ncp)),
                           suppressWarnings(qt(1L - (1L - conf.level) / 2L, v, ncp = ncp)))

        # Confidence interval
        conf.int <- conf.int.ncp / (ncp / estimate)

        # Not a number
        conf.int <- ifelse(is.nan(conf.int), NA, conf.int)

      } else {

        # Standard error (Hunter & Schmidt, 2004)
        estimate.SE <- sqrt((sum(n.group) - 1L) / (sum(n.group) - 3L)  * ((4L / sum(n.group)) * (1L + (estimate^2L / 8L))))

        # t quantile
        t.quantile <- -qt((1L - conf.level) / 2L, (sum(n.group) - 2L))

        # Confidence interval
        conf.int <- c(estimate - t.quantile * estimate.SE, estimate + t.quantile * estimate.SE)

      }

    #............................................................
    # Within-subject design
    } else {

      #...................
      # Data and Arguments

      y1 <- data[, y.var[1L]]
      y2 <- data[, y.var[2L]]

      y.r <- cor(y1, y2, use = "complete.obs")

      y.dat <- na.omit(data.frame(y1, y2, stringsAsFactors = FALSE))

      #...................
      # Descriptives

      res.descript <- suppressWarnings(misty::descript(y.dat, check = FALSE, output = FALSE))$result

      # Mean difference
      x.diff <- mean(y2 - y1, na.rm = TRUE)

      # Sample size
      n <- nrow(y.dat)

      #...................
      # Standard deviation

      # Cohens d
      if (is.null(ref)) {

        # Weighted pooled standard deviation
        if (isTRUE(weighted)) {

          sd.group <- sqrt(sum(res.descript$var) - 2L * y.r * prod(res.descript$sd))

        # Unweigted pooled standard deviation
        } else {

          sd.group <- sqrt(mean(res.descript$var))

        }

      # Standard deviation from a specific ref
      } else {

        sd.group <- sd(data[, ref], na.rm = TRUE)

      }

      #........................................
      # Cohen's d estimate

      # Cohen's d.rm
      if (isTRUE(weighted)) {

        estimate <- x.diff / sd.group * sqrt(2L*(1L -y.r))

      # Cohen's d.av
      } else {

        estimate <- x.diff / sd.group

      }

      #........................................
      # Correction factor

      v <- n*2L - 2L

      # Correction factor based on gamma function
      if (n*2L < 200L) {

        corr.factor <- gamma(0.5*v) / ((sqrt(v / 2L)) * gamma(0.5 * (v - 1L)))

      # Correction factor based on approximation
      } else {

        corr.factor <- (1L - (3L / (4L * v - 1L)))

      }

      # Bias-corrected Cohen's d
      if (isTRUE(correct) && isTRUE(weighted)) {

        estimate <- estimate*corr.factor

      }

      #........................................
      # Confidence interval

      # Cohen's d.rm
      if (isTRUE(weighted)) {

        # True standard error (Hedges, 1981)
        estimate.SE <- sqrt((v / (v - 2L)) * (2L*(1L - y.r) / n) * (1L + estimate^2L * (n / (2L*(1L - y.r)))) - (estimate^2L / corr.factor^2L))

        # Noncentrality parameter
        ncp <- estimate * sqrt(n / (2L*(1L - y.r)))

        # Confidence interval around ncp
        conf.int.ncp  <- c(suppressWarnings(qt((1L - conf.level) / 2L, v, ncp = ncp)),
                           suppressWarnings(qt(1L - (1L - conf.level) / 2L, v, ncp = ncp)))

        # Confidence interval
        conf.int <- conf.int.ncp / (ncp / estimate)

      # Cohen's d.av
      } else {

        # Standard error (Algina & Keselman, 2003, p. 539)
        estimate.SE <- sqrt((2L*(sum(res.descript$var) - 2L * cov(y1, y2, use = "complete.obs"))) / (n*sum(res.descript$var)))

        # t quantile
        t.quantile <- -qt((1L - conf.level) / 2L, n - 1L)

        # Confidence interval
        conf.int <-  c(estimate - t.quantile * estimate.SE, estimate + t.quantile * estimate.SE)

      }

    }

  #----------------------------------------
  # More than one outcome variable

  } else {

    result <- lapply(y.var, function(y) cohens.d(paste(eval(parse(text = "y")), "~", eval(parse(text = "group.var"))),
                                                 data = data, paired = FALSE, weighted = weighted, correct = correct, ref = ref,
                                                 conf.level = conf.level, as.na = as.na, check = FALSE, output = FALSE)$result)

    result <- Reduce(function(xx, yy) rbind(xx, yy, make.row.names = FALSE), result)

  }

  ####################################################################################
  # Return object

  #----------------------------------------
  # One outcome variable

  if ((!isTRUE(paired) && length(y.var) == 1L) || (isTRUE(paired) && length(y.var) == 2L)) {

    #.......................................
    # Between-subject design
    if (!isTRUE(paired)) {

      object <- list(call = match.call(),
                     data = data,
                     args = list(formula = formula, paired = paired, weighted = weighted, correct = correct, ref = ref,
                                 digits = digits, conf.level = conf.level, check = check, output = output),
                     result = data.frame(variable = y.var,
                                         n1 = res.descript[1L, "n"], nNA1 = res.descript[1, "nNA"], m1 = res.descript[1, "m"], sd1 = res.descript[1L, "sd"],
                                         n2 = res.descript[2L, "n"], nNA2 = res.descript[2, "nNA"], m2 = res.descript[2, "m"], sd2 = res.descript[2L, "sd"],
                                         m.diff =  x.diff, sd = sd.group, estimate = estimate,
                                         SE = estimate.SE, low = conf.int[1], upp = conf.int[2],
                                         stringsAsFactors = FALSE))

    #.......................................
    # Within-subject design
    } else {

      object <- list(call = match.call(),
                     data = data,
                     args = list(formula = formula, paired = paired, weighted = weighted, correct = correct, ref = ref,
                                 digits = digits, conf.level = conf.level, check = check, output = output),
                     result = data.frame(n = res.descript[1, "n"], nNA = nrow(data) - res.descript[1, "n"],
                                         variable1 = y.var[1], m1 = res.descript[1, "m"], sd1 = res.descript[1, "sd"],
                                         variable2 = y.var[2], m2 = res.descript[2, "m"], sd2 = res.descript[2, "sd"],
                                         m.diff =  x.diff, sd = sd.group, estimate = estimate,
                                         SE = estimate.SE, low = conf.int[1], upp = conf.int[2],
                                         stringsAsFactors = FALSE))

    }

  #----------------------------------------
  # More than one outcome variable

  } else {

    object <- list(call = match.call(),
                   data = data,
                   args = list(formula = formula, paired = paired, weighted = weighted, correct = correct, ref = ref,
                               digits = digits, conf.level = conf.level, check = check, output = output),
                   result = result)

  }

  class(object) <- "cohens.d"

  ####################################################################################
  # Output

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
