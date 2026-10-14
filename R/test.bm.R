#' Brunner-Munzel Test for Stochastic Equality
#'
#' This function performs two-sample and paired-sample (Permuted) Brunner-Munzel
#' test (Brunner & Munzel, 2000) using either a t-distribution with a Satterthwaite-Welch
#' degrees of freedom approximation, an exact permutation test, or a randomization test
#' (Neubert & Brunner, 2007).
#'
#' @param data        a data frame. Note that data are specified either by using
#'                    the argument \code{data} or by using the arguments \code{x}
#'                    and \code{y}.
#' @param ...         an expression indicating the variable names in \code{data},
#'                    e.g., \code{test.bm(dat, x1, x2)}. Note that the operators
#'                    \code{+}, \code{-}, \code{~}, \code{:}, \code{::}, and \code{!}
#'                    can also be used to select variables, see 'Details' in the
#'                    \code{\link{df.subset}} function.
#' @param x           a numeric vector of data values.
#' @param y           a numeric vector of data values.
#' @param formula     in case of two sample Brunner-Munzel test (i.e., \code{paired = FALSE}),
#'                    a formula of the form \code{y ~ group} where \code{group}
#'                    is a numeric variable, character variable or factor with
#'                    two values or factor levels giving the corresponding groups.
#' @param paired      logical: if \code{TRUE}, paired-sample Brunner-Munzel-test
#'                    is computed.
#' @param method      a character string indicating the method used for statistical
#'                    significance testing, i.e., \code{"t"} for a t-distribution
#'                    with Satterthwaite-Welch degrees of freedom approximation,
#'                    \code{"logit"} for a t-distribution with Satterthwaite-Welch
#'                    degrees of freedom approximation and logit transformation
#'                    for range-preserving confidence intervals, and \code{"perm"}
#'                    for a studentized permutation or randomization test that
#'                    is recommended for a better control of the type I error
#'                    rate when sample sizes are small (smaller than 15 per group).
#' @param alternative a character string specifying the alternative hypothesis,
#'                    must be one of \code{"two.sided"} (default),
#'                    \code{"greater"} or \code{"less"}.
#' @param scale       a character string indicating the scale of the reported
#'                    estimated and confidence interval, i.e., \code{"prob"}
#'                    (default) for \eqn{P(X < Y) + 0.5(P(X = Y)} with range
#'                    \eqn{[0, 1]} and a null value of \eqn{0.5}, \code{"diff"}
#'                    for \eqn{P(X < Y) - P(X > Y)}  with range \eqn{[-1, 1]}
#'                    and a null value of \eqn{0}, \code{"logodds"} for
#'                    \eqn{log[P/(1 - P)]} with range \eqn{[-Inf, Inf]} and a
#'                    null value of \eqn{0}, and \code{"odds"} for \eqn{P/(1 - P)}
#'                    with range \eqn{[0, Inf]} and a null value of \eqn{1}.
#'                    Note that the statistical test always operates on the
#'                    probability scale.
#' @param hypo        logical: if \code{TRUE} (default), null and alternative
#'                    hypothesis are shown on the console.
#' @param descript    logical: if \code{TRUE} (default), descriptive statistics
#'                    are shown on the console.
#' @param conf.level  a numeric value between 0 and 1 indicating the confidence
#'                    level of the interval.
#' @param p.adj       a character string indicating an adjustment method for multiple
#'                    testing based on \code{\link{p.adjust}}, i.e., \code{none}
#'                    (default), \code{bonferroni}, \code{holm}, \code{hochberg},
#'                    \code{hommel}, \code{BH}, \code{BY}, or \code{fdr}.
#'                    See help page of the \code{\link{p.adjust}} function.
#' @param nperm       a numeric value (default: 10000) indicating the maximum
#'                    number of permutations used when \code{method = "perm"}.
#'                    If the number of complete permutations, i.e., \code{choose(N, n.x)}
#'                    in the two-sample design and \code{2^n} in the paired-sample
#'                    design, is smaller than or equal \code{nperm}, an exact
#'                    permutation test is conducted, while a randomization test
#'                    based on \code{nperm} samples of unique permutations is
#'                    conducted when the number of complete permutations is
#'                    greater than \code{nperm}.
#' @param seed        a numeric value specifying the seed of the pseudo-random
#'                    numbers used when drawing permutation samples.
#' @param na.omit     logical: if \code{TRUE}, incomplete cases are removed
#'                    before conducting the analysis (i.e., listwise deletion)
#'                    when specifying more than one outcome variable.
#' @param digits      an integer value indicating the number of decimal .places
#'                    to be used for displaying descriptive statistics and
#'                    confidence interval.
#' @param p.digits    an integer value indicating the number of decimal .places
#'                    to be used for displaying the \emph{p}-value.
#' @param as.na       a numeric vector indicating user-defined missing values,
#'                    i.e. these values are converted to \code{NA} before
#'                    conducting the analysis.
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
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{test.t}}, \code{\link{test.welch}}, \code{\link{test.z}}
#'
#' @references
#' Schüürhuis, S., Konietschke, F., & Brunner, E. (2025). A New Approach to the
#' nonparametric Behrens-Fisher problem With compatible confidence iIntervals.
#' \emph{Biometrical Journal, 67}(6), e70096. https://doi.org/10.1002/bimj.70096
#'
#' Karch, J. D. (2021). Psychologists should use Brunner-Munzel’s instead of
#' Mann-Whitney’s U Test as the default nonparametric procedure.
#' \emph{Advances in Methods and Practices in Psychological Science, 4}(2).
#' https://doi.org/10.1177/2515245921999
#'
#' Neubert, K., & Brunner, E. (2007). A studentized permutation test for the
#' non-parametric Behrens-Fisher problem. \emph{Computational Statistics & Data Analysis, 51}(10),
#' 5192–5204. https://doi.org/10.1016/j.csda.2006.05.024
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{sample}}{type of sample, i.e., two- or paired sample}
#' \item{\code{formula}}{formula}
#' \item{\code{data}}{data frame with the outcome and grouping variable}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{result}}{result table}
#'
#' @note
#' This function is based on the \code{brunner_munzel()} and \code{ses_calc()}
#' function in the \pkg{TOSTER} package by Daniel Lakens and Aaron Caldwell.
#'
#' @export
#'
#' @examples
#' #————————————————————————————————————————————————————————————————————————————
#' # Two-Sample Design
#'
#' # Example 1a: Two-sided two-sample Brunner-Munzel test
#' test.bm(mpg ~ vs, data = mtcars)
#'
#' # Example 1b: Alternative specification without using the '...' argument
#' test.bm(x = df.subset(mtcars, mpg, subset = vs == 0),
#'         y = df.subset(mtcars, mpg, subset = vs == 1))
#'
#' # Example 1c: Two-sided two-sample Brunner-Munzel randomization test
#' # Sample of 10000 out of choose(32, 18) = 471435600 permutations
#' test.bm(mpg ~ vs, data = mtcars, method = "perm")
#'
#' # Example 1d: Two-sided two-sample Brunner-Munzel permutation test
#' # Complete permutation of choose(13, 8) = 1287 permutations
#' test.bm(x = c(3, 1, 4, 2, 5, 3, 6, 7), y = c(5, 2, 4, 3, 1), method = "perm")
#'
#' # Example 1e: One-sided two-sample Brunner-Munzel test
#' test.bm(mpg ~ vs, data = mtcars, alternative = "greater")
#'
#' # Example 1f: Multiple two-sample Brunner-Munzel tests
#' test.bm(cbind(mpg, cyl, disp, hp, drat, wt) ~ vs, data = mtcars)
#'
#' # Example 1g: Multiple two-sample Brunner-Munzel tests, bonferroni correction
#' test.bm(cbind(mpg, cyl, disp, hp, drat, wt) ~ vs, data = mtcars, p.adj = "bonferroni")
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Paired-Sample Design
#'
#' # Example 2a: Two-sided paired-sample Brunner-Munzel test
#' test.bm(mtcars, drat, wt, paired = TRUE)
#'
#' # Example 2b: Alternative specification without using the '...' argument
#' test.bm(mtcars[, c("drat", "wt")], paired = TRUE)
#'
#' # Example 2c: Alternative specification without using the '...' argument
#' test.bm(x = mtcars$drat, y = mtcars$wt, paired = TRUE)
#'
#' # Example 2d: Two-sided paired-sample Brunner-Munzel test with logit transformation
#' test.bm(mtcars, drat, wt, paired = TRUE, method = "logit")
#'
#' \dontrun{
#' #————————————————————————————————————————————————————————————————————————————
#' # Write Results
#'
#' # Example 3a: Write results into a text file
#' test.bm(mpg ~ vs, data = mtcars, write = "BM-Test.txt")
#'
#' # Example 3b:  Write results into an Excel file
#' test.bm(mpg ~ vs, data = mtcars, write = "BM-Test.xlsx")
#' }
test.bm <- function(...) {

  UseMethod("test.bm")

}

#_______________________________________________________________________________
#
# Default S3 method ————————————————————————————————————————————————————————————

test.bm.default <- function(data = NULL, ..., x = NULL, y = NULL, paired = FALSE,
                            method = c("t", "logit", "perm"),
                            alternative = c("two.sided", "less", "greater"),
                            scale = c("prob", "diff", "logodds", "odds"),
                            hypo = FALSE, descript = TRUE, conf.level = 0.95,
                            nperm = 10000, seed = 42, digits = 3, p.digits = 3,
                            as.na = NULL, write = NULL, append = TRUE, check = TRUE,
                            output = TRUE) {

  #_____________________________________________________________________________
  #
  ## Initial Check —————————————————————————————————————————————————————————————

  # Check if input 'data' and 'x' are NULL
  if (isTRUE(is.null(data) && is.null(x))) { stop("Please specify the argument 'data' or the arguments 'x' and 'y'.", call. = FALSE) }

  # Check if input 'data' or 'x' is specified
  if (isTRUE(!is.null(data) && !is.null(x))) { stop("Please specify either the argument 'data' or the argumenta 'x' and 'y', but not both.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  ## Data ——————————————————————————————————————————————————————————————————————

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Extract Variables ####

  if (isTRUE(!is.null(data))) {

    .data(data, ..., drop = TRUE, check.two = TRUE) |> (\(p) {

      # Extract 'x'
      assign("x", p$data[, 1L], envir = parent.frame())

      # Extract 'x'
      assign("y", p$data[, 2L], envir = parent.frame())

    })()

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert User-Missing Values into NA ####

  if (isTRUE(!is.null(as.na))) {

    # Replace user-specified values with missing values
    x <- .as.na(x, na = as.na)
    y <- .as.na(y, na = as.na)

  }

  #_____________________________________________________________________________
  #
  ## Input Check ———————————————————————————————————————————————————————————————

  .check.input(logical = c("paired", "hypo", "descript", "append", "output"),
               s.character = list(method = c("t", "logit", "perm"), scale = c("prob", "diff", "logodds", "odds")), args = c("xy", "nperm", "alternative", "conf.level", "digits", "p.digits", "write2"), envir = environment(), input.check = check)

  #_____________________________________________________________________________
  #
  ## Arguments —————————————————————————————————————————————————————————————————

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'method' Argument ####

  if (isTRUE(all(c("t", "logit", "perm") %in% method))) { method <- "t" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'alternative' Argument ####

  if (isTRUE(all(c("two.sided", "less", "greater") %in% alternative))) { alternative <- "two.sided" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'scale' Argument ####

  if (isTRUE(all(c("prob", "diff", "logodds", "odds") %in% scale))) { scale <- "prob" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'alpha' Argument ####

  alpha <- 1L - conf.level

  #_____________________________________________________________________________
  #
  ## Main Function —————————————————————————————————————————————————————————————

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Two-Sample Design ####

  nperm.actual <- NA
  if (isTRUE(isTRUE(!paired))) {

    # Missing values
    na.x <- sum(!is.finite(x))
    na.y <- sum(!is.finite(y))

    # Remove missing data
    xx <- x[is.finite(x)]
    yy <- y[is.finite(y)]

    # Sample size
    n.x <- length(xx)
    n.y <- length(yy)

    N <- sum(c(n.x, n.y))

    # Ranks
    r.xy <- rank(c(xx, yy))

    r.x <- rank(xx)
    r.y <- rank(yy)

    # Probabilities
    p.x <- (1L / n.y*(r.xy[seq_len(n.x)] - r.x))
    p.y <- (1L / n.x*(r.xy[(n.x + 1L):N] - r.y))

    pm.x <- mean(p.x)
    pm.y <- mean(p.y)

    s.x <- (var(p.x) / n.x)
    s.y <- (var(p.y) / n.y)

    #  Pooled variance and standard deviation
    V <- (N*(s.x + s.y)) |> (\(p) if (isTRUE(p == 0L)) {  N / (2L * n.x * n.y) } else { p })()
    sd <- sqrt(V)

    # Test statistic
    stat <- sqrt(N)*((mean(p.y) |> (\(p) if (isTRUE(p == 1L)) { 0.999 } else if (isTRUE(p == 0L)) { 0.001 } else { p })()) - 1L / 2L) / sd

    # Degrees of freedom
    df <- ((s.x + s.y)^2L / (s.x^2L / (n.x - 1L) + s.y^2L / (n.y - 1L))) |> (\(p) if (isTRUE(is.nan(p))) { 1000L } else { p })()

    #—————————————————————————————————————— #
    ### t Distribution with a Welch-Satterthwaite Approximation without Logit Transformation ####

    switch(method, "t" = {

      switch(alternative,
             "two.sided" = {

               p <- min(2L - 2L * pt(stat, df = df), 2L * pt(stat, df = df))

               low <- pm.y - qt(1L - alpha / 2L, df = df) / sqrt(N)*sd
               upp <- pm.y + qt(1L - alpha / 2L, df = df) / sqrt(N)*sd

             }, "less" = {

               p <- pt(stat, df = df)

               low <- 0L
               upp <- pm.y + qt(conf.level, df = df) / sqrt(N)*sd

             }, "greater"  = {

               p <- 1L - pt(stat, df = df)

               low <- pm.y - qt(conf.level, df = df) / sqrt(N)*sqrt(V)
               upp <- 1L

             })

    #—————————————————————————————————————— #
    ### t Distribution with a Welch-Satterthwaite Approximation with Logit Transformation ####

    }, "logit" = {

      # Logit method is unreliable when the estimated relative effect is exactly 0 or 1
      if (isTRUE(pm.y == 0L)) {

        warning("Estimated relative effect of 0 was set to 0.0001 for logit transformation.", call. = FALSE)

        pm.y <- 0.0001

      } else if (isTRUE(pm.y == 1L)) {

        warning("Estimated relative effect of 1 was set to 0.9999 for logit transformation.", call. = FALSE)

        pm.y <- 0.9999

      }

      logit.pm.y <- log(pm.y / (1L - pm.y))
      logit.dev <- 1L / (pm.y * (1L - pm.y))

      vd.logit <- logit.dev^2L * sd^2
      stat <- (logit.pm.y) * sqrt(N / vd.logit)

      switch(alternative, two.sided = {

        p <- min(2L - 2L * pnorm(stat), 2L * pnorm(stat))

        low <- .expit(logit.pm.y - qnorm(1L - alpha / 2L) / sqrt(N)*sqrt(vd.logit))
        upp <- .expit(logit.pm.y + qnorm(1L - alpha / 2L) / sqrt(N)*sqrt(vd.logit))

      }, less = {

        p <- pnorm(stat)

        low <- 0L
        upp <- .expit(logit.pm.y + qnorm(conf.level) / sqrt(N)*sqrt(vd.logit))

      }, greater = {

        p <- 1L - pnorm(stat)

        low <- .expit(logit.pm.y - qnorm(conf.level) / sqrt(N)*sqrt(vd.logit))
        upp <- 1L

      })

    #—————————————————————————————————————— #
    ### Permutation or Randomization Test ####

    }, "perm" = {

      # Permutation indices
      P <- .bm.perm.indices(N, n.x, nperm, seed = seed)

      # Actual number of unique permutations obtained
      nperm.actual <- ncol(P)

      # Permutation or randomization
      exact <- nperm.actual >= choose(N, n.x)

      # Permutations
      Px <- matrix(c(xx, yy)[P], ncol = nperm.actual)
      Tperm <- t(apply(.perm.loop(x = Px[seq_len(n.x), ], y = Px[(n.x + 1L):N, ], n.x = n.x, n.y = n.y, nperm = nperm.actual), 1L, sort))

      # Test statistic
      test.stat <- sqrt(N) * (pm.y - 0.5) / sd

      # Significance value and confidence interval
      switch(alternative, "two.sided" = {

        p <- .bm.compute.perm.pval(sum(abs(Tperm[1L, ]) >= abs(test.stat)), nperm.actual, p.method = "exact") |> (\(p) if (isTRUE(p == 0L)) { .bm.compute.perm.pval(sum(abs(Tperm[1L, ]) >= abs(test.stat)), nperm.actual, p.method = "plusone") } else { p })()

        low = pm.y - sqrt(V / N)*(0.5*(Tperm[1L, max(1L, floor((1L - alpha / 2L)*nperm.actual))] + Tperm[1L, min(nperm.actual, ceiling((1L - alpha / 2L)*nperm.actual))]))
        upp = pm.y - sqrt(V / N)*(0.5*(Tperm[1L, max(1L, floor(alpha / 2L*nperm.actual))] + Tperm[1L, min(nperm.actual, ceiling(alpha / 2L*nperm.actual))]))

      }, "less" = {

        p <- .bm.compute.perm.pval(sum(Tperm[1L, ] <= test.stat), nperm.actual, p.method = "exact") |> (\(p) if (isTRUE(p == 0L)) { .bm.compute.perm.pval(sum(Tperm[1L, ] <= test.stat), nperm.actual, p.method = "plusone") } else { p })()

        low = 0L
        upp = pm.y - sqrt(V / N)*(0.5*(Tperm[1L, max(1L, floor(alpha*nperm.actual))] + Tperm[1L, min(nperm.actual, ceiling(alpha*nperm.actual))]))

      }, "greater" = {

        p <- .bm.compute.perm.pval(sum(Tperm[1L, ] >= test.stat), nperm.actual, p.method = "exact") |> (\(p) if (isTRUE(p == 0L)) { .bm.compute.perm.pval(sum(Tperm[1L, ] >= test.stat), nperm.actual, p.method = "plusone") } else { p })()

        low = pm.y - sqrt(V / N)*(0.5*(Tperm[1L, max(1, floor((1L - alpha)*nperm.actual))] + Tperm[1L, min(nperm.actual, ceiling((1L - alpha)*nperm.actual))]))
        upp = 1L

      })

    })

    # Clamp confidence interval
    low <- ifelse(low < 0L || is.nan(low), 0L, low)
    upp <- ifelse(upp > 1L || is.nan(upp), 1L, upp)

    #—————————————————————————————————————— #
    ### Scale Transformation ####

    if (isTRUE(scale != "prob")) {

      pm.x <- .trans.p(pm.x, scale = scale)
      pm.y <- .trans.p(pm.y, scale = scale)
      low <- .trans.p(low, scale = scale)
      upp <- .trans.p(upp, scale = scale)

    }

    #—————————————————————————————————————— #
    ### Result Table ####

    restab <- data.frame(group = c("x", "y"), n = c(n.x, n.y), nNA = c(na.x, na.y), est = c(pm.x, pm.y), low = c(NA, low), upp = c(NA, upp), t = c(NA, stat), df = c(NA, df), nperm = c(NA, nperm.actual), p = c(NA, p)) |> (\(p) if (isTRUE(method != "perm")) { p[, setdiff(colnames(p), "nperm")] } else { p })()

    sample <- "two"

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Paired-Sample Design  ####

  } else {

    # Listwise deletion
    xy.omit <- na.omit(data.frame(x = x, y = y))

    x <- xy.omit$x
    y <- xy.omit$y

    # Missing data
    na.xy <- length(attributes(xy.omit)$na.action)

    # Sample size
    n <- length(x)

    n.x <- n + 1L

    df <- n - 1L

    all.data <- c(y, x)

    N <- length(all.data)

    xinverse <- c(x, y)

    x1 <- y
    x2 <- x

    rx <- rank(all.data)
    rxinverse <- rank(xinverse)

    BM1 <- 1L / n * (rx[1:n] - rank(x1))
    BM2 <- 1L / n * (rx[(n+1):N] - rank(x2))

    BM3 <- BM1 - BM2

    pm <- mean(BM2)

    m <- mean(BM3)

    V <- (sum(BM3^2L) - n*m ^ 2L) / (n - 1L) |> (\(p) if (isTRUE(p == 0L)) { 1L / n } else { p })()

    sd = sqrt(V / n)

    #—————————————————————————————————————— #
    ### t Distribution with a Welch-Satterthwaite Approximation without Logit Transformation ####

    nperm.actual <- NA
    switch(method, "t" = {

      # Test statistic
      stat <- sqrt(n) * (pm - 0.5) / sqrt(V)

      switch(alternative,
             "two.sided" = {

               p <- 2L*min(pt(stat, df), 1L - pt(stat, df))

               low <- pm - qt(1L - alpha / 2L, df)*sqrt(V / n)
               upp <- pm + qt(1L - alpha / 2L, df)*sqrt(V / n)

             }, "less" = {

               p <- pt(stat, df = df)

               low <- 0L
               upp <- pm + qt(1L - alpha, df)*sqrt(V / n)

             }, "greater"  = {

               p <- 1L - pt(stat, df = df)

               low <- pt(stat, df = df)
               upp <- 1L

             })

    #—————————————————————————————————————— #
    ### t Distribution with a Welch-Satterthwaite Approximation with Logit Transformation ####

    }, "logit" = {

      # Logit method is unreliable when the estimated relative effect is exactly 0 or 1
      if (isTRUE(pm == 0L)) {

        warning("Estimated relative effect of 0 was set to 0.0001 for logit transformation.", call. = FALSE)

        pm <- 0.0001

      } else if (isTRUE(pm == 1L)) {

        warning("Estimated relative effect of 1 was set to 0.9999 for logit transformation.", call. = FALSE)

        pm <- 0.9999

      }

      # Logit transformation
      pm.logit <- log(pm / (1L - pm))
      se.logit <- sqrt(V / n) / (pm * (1L - pm))

      # Test statistic
      stat <- (pm.logit - log(0.5 / (1L - 0.5))) / se.logit

      switch(alternative, two.sided = {

        p <- 2*min(pt(stat, df), 1L - pt(stat, df))

        low <- .expit(pm.logit - qt(1L - alpha / 2L, df) * se.logit)
        upp <- .expit(pm.logit + qt(1L - alpha / 2L, df) * se.logit)

      }, less = {

        p <- pt(stat, df = df)

        low <- 0L
        upp <- .expit(pm.logit - qt(1-alpha, df) * se.logit)

      }, greater = {

        p <- 1L - pt(stat, df = df)

        low <- .expit(pm.logit - qt(1L - alpha, df) * se.logit)
        upp <- 1L

      })

    #—————————————————————————————————————— #
    ### Permutation or Randomization Test ####

    }, "perm" = {

      # Permutation: Enumerate all unique combinations
      if (isTRUE(nperm >= 2^n)) {

        nperm.actual <- 2^n
        p <- NULL
        for (i in seq_len(n)) {

          a <- rep(c(rep(c(i, i + n), nperm.actual / (2L^i)), rep(c(i + n, i), nperm.actual / (2L^i))), 2L^(i - 1L))
          p <- rbind(p, a)

        }

        P <- matrix(p, ncol = nperm.actual)

        xperm <- matrix(all.data[P], nrow = N, ncol = nperm.actual)
        rxperm <- matrix(rx[P], nrow = N, ncol = nperm.actual)

      # Randomization: Sampling without replacement
      } else {

        nperm.actual <- nperm
        P <- matrix(nrow = n, ncol = nperm.actual)

        P2 <- apply(P, 2L, .permu, seed = seed) |> (\(p) rbind(p, p))()

        xperm <- all.data*P2 + xinverse*(1L - P2)
        rxperm <- rx*P2 + rxinverse*(1L - P2)

      }

      xperm1 <- xperm[seq_len(n), ]
      xperm2 <- xperm[n.x:N, ]

      rperm1 <- rxperm[seq_len(n), ]
      rperm2 <- rxperm[n.x:N, ]

      riperm1 <- apply(xperm1, 2L, rank)
      riperm2 <- apply(xperm2, 2L, rank)

      BMperm2 <- 1L / n*(rperm2 - riperm2)
      BMperm3 <- 1L / n*(rperm1 - riperm1) - BMperm2

      pmperm <- colMeans(BMperm2)
      mperm3 <- colMeans(BMperm3)

      vperm3 <- (colSums(BMperm3^2L) - n*mperm3^2L) / (n - 1L)
      vperm3.0 <- (vperm3 == 0L)

      vperm3[vperm3.0] <- 1L / n

      Tperm <- sqrt(n) * (pmperm - 0.5) / sqrt(vperm3)

      # Test statistic
      stat <- sqrt(n) * (pm - 0.5) / sqrt(V)

      # Significance value and confidence interval
      switch(alternative, "two.sided" = {

        p <- .bm.compute.perm.pval(sum(abs(Tperm) >= abs(stat)), nperm.actual, p.method = "exact") |> (\(p) if (isTRUE(p == 0L)) { .bm.compute.perm.pval(sum(abs(Tperm) >= abs(stat)), nperm.actual, p.method = "plusone") } else { p })()

        low <- pm - sort(Tperm)[min(nperm.actual, max(1L, floor((1L - alpha / 2L)*nperm.actual) + 1L))]*sqrt(V / n)
        upp <- pm + sort(Tperm)[min(nperm.actual, max(1L, floor((1L - alpha / 2L)*nperm.actual) + 1L))]*sqrt(V / n)

      }, "less" = {

        p <- .bm.compute.perm.pval(sum(Tperm <= stat), nperm.actual, p.method = "exact") |> (\(p) if (isTRUE(p == 0L)) { .bm.compute.perm.pval(sum(Tperm <= stat), nperm.actual, p.method = "plusone") } else { p })()

        low <- 0L
        upp <- pm + sort(Tperm)[min(nperm.actual, max(1L, floor((1L - alpha)*nperm.actual) + 1L))]*sqrt(V / n)

      }, "greater" = {

        p <- .bm.compute.perm.pval(sum(Tperm >= stat), nperm.actual, p.method = "exact") |> (\(p) if (isTRUE(p == 0L)) { .bm.compute.perm.pval(sum(Tperm >= stat), nperm.actual, p.method = "plusone") } else { p })()

        low <- pm - sort(Tperm)[min(nperm.actual, max(1L, floor((1L - alpha)*nperm.actual) + 1L))]*sqrt(V / n)
        upp <- 1L

      })

    })

    # Clamp confidence interval
    low <- ifelse(low < 0L || is.nan(low), 0L, low)
    upp <- ifelse(upp > 1L || is.nan(upp), 1L, upp)

    #—————————————————————————————————————— #
    ### Scale Transformation ####

    if (isTRUE(scale != "prob")) {

      pm <- .trans.p(pm, scale = scale)
      low <- .trans.p(low, scale = scale)
      upp <- .trans.p(upp, scale = scale)

    }

    #—————————————————————————————————————— #
    ### Result Table ####

    restab <- data.frame(n = n, nNA = na.xy, est = pm, low = low, upp = upp, t = stat, df = df, nperm = nperm.actual, p = p) |> (\(p) if (isTRUE(method != "perm")) { p[, setdiff(colnames(p), "nperm")] } else { p })()

    sample <- "paired"

  }

  #_____________________________________________________________________________
  #
  ## Return Object —————————————————————————————————————————————————————————————

  object <- list(call = match.call(),
                 type = "test.bm",
                 sample = sample,
                 data = list(x = x, y = y),
                 args = list(paired = paired, method = method, nperm = nperm, alternative = alternative, scale = scale, hypo = hypo, descript = descript, conf.level = conf.level, seed = seed, digits = digits, p.digits = p.digits, as.na = as.na, write = write, append = append, check = check, output = output),
                 result = restab)

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  ## Write Results —————————————————————————————————————————————————————————————

  if (isTRUE(!is.null(write))) { .write.result(object = object, write = write, append = append) }

  #_____________________________________________________________________________
  #
  ## Output ————————————————————————————————————————————————————————————————————

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}

#_______________________________________________________________________________
#
# S3 method for class 'formula' ————————————————————————————————————————————————

test.bm.formula <- function(formula, data, method = c("t", "logit", "perm"),
                            alternative = c("two.sided", "less", "greater"),
                            scale = c("prob", "diff", "logodds", "odds"),
                            hypo = FALSE, descript = TRUE, conf.level = 0.95,
                            p.adj = "none", nperm = 10000, seed = 42, na.omit = FALSE,
                            digits = 3, p.digits = 3, as.na = NULL, write = NULL,
                            append = TRUE, check = TRUE, output = TRUE, ...) {

  #_____________________________________________________________________________
  #
  ## Initial Check —————————————————————————————————————————————————————————————

  # Check if input 'formula' is missing
  if (isTRUE(missing(formula))) { stop("Please specify a formula using the argument 'formula'", call. = FALSE) }

  # Check if input 'data' is missing or NULL
  if (isTRUE(missing(data) ||is.null(data))) { stop("Please specify a matrix or data frame for the argument 'x'.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Variables ——————————————————————————————————————————————————————————————————

  # Extract variable names
  var.formula <- all.vars(as.formula(formula))

  # Grouping variable
  group.var <- attr(terms(formula[-2L]), "term.labels")

  # Outcome variable
  y.var <- setdiff(var.formula, group.var)

  #_____________________________________________________________________________
  #
  # Input Check ————————————————————————————————————————————————————————————————

  .check.input(logical = c("hypo", "descript", "append", "output"), s.character = list(method = c("t", "logit", "perm"), scale = c("prob", "diff", "logodds", "odds")), args = c("alternative", "conf.level", "p.adj", "digits", "p.digits", "write2"), envir = environment(), input.check = check)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Additional Checks ####

  if (isTRUE(check)) {

    # Check if variables are in the data
    (!var.formula %in% colnames(data)) |> (\(y) if (isTRUE(any(y))) { stop(paste0("Variables specified in the the formula were not found in 'data': ", paste(var.formula[which(y)], collapse = ", ")), call. = FALSE) })()

    # Check if input 'formula' has only one grouping variable
    if (isTRUE(length(group.var) != 1L)) { stop("Please specify a formula with only one grouping variable.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Data ———————————————————————————————————————————————————————————————————————

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data Frame ####

  x <- as.data.frame(data)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert User-Missing Values into NA ####

  if (isTRUE(!is.null(as.na))) { x[, y.var] <- .as.na(x[, y.var], na = as.na) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Listwise Deletion ####

  if (isTRUE(any(is.na(x[, var.formula])) && length(y.var) > 1L && na.omit)) {

    apply(x[, var.formula], 1L, function(y) all(!is.na(y))) |> (\(p) { x <<- x[p, ]

      warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ", sum(!p)), call. = FALSE)

    })()

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Missing Data in the Grouping Variable ####

  if (isTRUE(any(is.na(x[, group.var])))) {

    (!is.na(x[, group.var])) |> (\(p) { x <<- x[p, ]

    warning(paste0("Listwise deletion of missing values in the grouping variable, number of cases removed from the analysis: ", sum(!p)), call. = FALSE)

    })()

  }

  #_____________________________________________________________________________
  #
  ## Arguments —————————————————————————————————————————————————————————————————

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'method' Argument ####

  if (isTRUE(all(c("t", "logit", "perm") %in% method))) { method <- "t" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'alternative' Argument ####

  if (isTRUE(all(c("two.sided", "less", "greater") %in% alternative))) { alternative <- "two.sided" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'scale' Argument ####

  if (isTRUE(all(c("prob", "diff", "logodds", "odds") %in% scale))) { scale <- "prob" }

  #_____________________________________________________________________________
  #
  ## Main Function —————————————————————————————————————————————————————————————

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## One Outcome Variable ####

  if (isTRUE(length(y.var) == 1L)) {

    # Default S3 method
    restab <- split(unlist(x[, y.var]), f = unlist(x[, group.var])) |> (\(p) test.bm.default(x = p[[1L]], y = p[[2L]], paired = FALSE, method = method, nperm = nperm, alternative = alternative, conf.level = conf.level, scale = scale, seed = seed, check = FALSE, output = FALSE)$result |> (\(q) data.frame(group = names(p), q[, -1L]))())()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## More Than One Outcome Variable ####

  } else {

    # Default S3 method
    restab <- data.frame(variable = rep(y.var, each = 2L), do.call("rbind", lapply(y.var, function(y) misty::test.bm(eval(parse(text = paste0(y, " ~ ", group.var))), data = x, paired = FALSE, method = method, nperm = nperm, alternative = alternative, conf.level = conf.level, scale = scale, seed = seed, check = FALSE, output = FALSE)$result)))

    # Multiple-testing adjustment
    if (isTRUE(p.adj != "none")) { restab$p <- p.adjust(restab$p, method = p.adj) }

  }

  #_____________________________________________________________________________
  #
  ## Return Object —————————————————————————————————————————————————————————————

  object <- list(call = match.call(),
                 type = "test.bm",
                 sample = "two",
                 data = data[, var.formula],
                 formula = formula,
                 args = list(method = method, alternative = alternative, scale = scale, hypo = hypo, descript = descript, conf.level = conf.level, p.adj = p.adj, nperm = nperm, seed = seed, digits = digits, p.digits = p.digits, as.na = as.na, write = write, append = append, check = check, output = output),
                 result = restab)

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  ## Write Results —————————————————————————————————————————————————————————————

  if (isTRUE(!is.null(write))) { .write.result(object = object, write = write, append = append) }

  #_____________________________________________________________________________
  #
  ## Output ————————————————————————————————————————————————————————————————————

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}

#_______________________________________________________________________________
