#' R-Squared Measures for Multilevel and Linear Mixed Effects Models by Rights
#' and Sterba (2019), Manually Inputting Parameter Estimates
#'
#' This function computes R-squared measures by Rights and Sterba (2019) for
#' multilevel and linear mixed effects models by manually inputting parameter
#' estimates.
#'
#' @param data      a matrix or data frame with the level-1 and level-2 predictors
#'                  and outcome variable used in the model.
#' @param within    a character vector with the variable names in \code{data} or
#'                  numeric vector with numbers corresponding to the columns in
#'                  \code{data} of the level-1 predictors used in the model. If
#'                  none used, set to \code{NULL}.
#' @param between   a character vector with the variable names in \code{data} or
#'                  numeric vector with numbers corresponding to the columns in
#'                  \code{data} of the level-2 predictors used in the model. If
#'                  none used, set to \code{NULL}.
#' @param random    a character vector with the variable names in \code{data} or
#'                  numeric vector with numbers corresponding to the columns in
#'                  \code{data} of the level-1 predictors that have random slopes
#'                  in the model. If no random slopes specified, set to \code{NULL}.
#' @param gamma.w   a numeric vector of fixed slope estimates for all level-1
#'                  predictors, to be entered in the order of the predictors
#'                  listed in the argument \code{within}.
#' @param gamma.b   a numeric vector of the intercept and fixed slope estimates
#'                  for all level-2predictors, to be entered in the order of the
#'                  predictors listed in the argument \code{between}. Note that
#'                  the first element is the parameter estimate for the intercept
#'                  if \code{intercept = TRUE}.
#' @param tau       a matrix indicating the random effects covariance matrix, the
#'                  first row/column denotes the intercept variance and covariances
#'                  (if intercept is fixed, set all to 0) and each subsequent
#'                  row/column denotes a given random slope's variance and covariances
#'                  (to be entered in the order listed in the argument \code{random}).
#' @param sigma2    a numeric value indicating the level-1 residual variance.
#' @param intercept logical: if \code{TRUE} (default), the first element in the
#'                  \code{gamma.b} is assumed to be the fixed intercept estimate;
#'                  if set to \code{FALSE}, the first element in the argument
#'                  \code{gamma.b} is assumed to be the first fixed level-2
#'                  predictor slope.
#' @param center    logical: if \code{TRUE} (default), all level-1 predictors are
#'                  assumed to be cluster-mean-centered and the function will
#'                  output all decompositions; if set to \code{FALSE}, function
#'                  will output only the total decomposition.
#' @param digits    an integer value indicating the number of decimal places to be
#'                  used.
#' @param plot      logical: if \code{TRUE}, bar chart showing the decomposition
#'                  of scaled total, within-cluster, and between-cluster outcome
#'                  variance into five (total), three (within-cluster), and two
#'                  (between-cluster) proportions is drawn. Note that the \pkg{ggplot2}
#'                  package is required to draw the bar chart.
#' @param gray      logical: if \code{TRUE}, graphical parameter to draw the bar
#'                  chart in gray scale.
#' @param start     a numeric value between 0 and 1, graphical parameter to specify
#'                  the gray value at the low end of the palette.
#' @param end       a numeric value between 0 and 1, graphical parameter to specify
#'                  the gray value at the high end of the palette.
#' @param color     a character vector, graphical parameter indicating the color
#'                  of bars in the bar chart in the following order: Fixed slopes
#'                  (Within), Fixed slopes (Between), Slope variation (Within),
#'                  Intercept variation (Between), and Residual (Within). By default,
#'                  colors from the colorblind-friendly palettes are used.
#' @param check     logical: if \code{TRUE}, argument specification is checked.
#' @param output    logical: if \code{TRUE}, output is shown on the console.
#'
#' @details
#' A number of R-squared measures for multilevel and linear mixed effects models
#' have been developed in the methodological literature (see Rights & Sterba, 2018).
#' R-squared measures by Rights and Sterba (2019) provide an integrative framework
#' of R-squared measures for multilevel and linear mixed effects models with random
#' intercepts and/or slopes. Their measures are based on partitioning model implied
#' variance from a single fitted model, but they provide a full partitioning of
#' the total outcome variance to one of five specific sources. See the help page
#' of the \code{\link{multilevel.r2}} function for more details.
#'
#' @author
#' Jason D. Rights, Sonya K. Sterba, Jessica K. Flake, and Takuya Yanagida
#'
#' @seealso
#' \code{\link{multilevel.r2}}, \code{\link{multilevel.cor}},
#' \code{\link{multilevel.descript}}, \code{\link{multilevel.icc}},
#' \code{\link{multilevel.indirect}}
#'
#' @references
#' Rights, J. D., & Cole, D. A. (2018). Effect size measures for multilevel models
#' in clinical child and adolescent research: New r-squared methods and recommendations.
#' \emph{Journal of Clinical Child and Adolescent Psychology, 47}, 863-873.
#'  https://doi.org/10.1080/15374416.2018.1528550
#'
#' Rights, J. D., & Sterba, S. K. (2019). Quantifying explained variance in multilevel
#' models: An integrative framework for defining R-squared measures. \emph{Psychological Methods, 24},
#' 309-338. https://doi.org/10.1037/met0000184
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#' \tabular{ll}{
#' \code{call} \tab function call \cr
#' \code{type} \tab type of analysis \cr
#' \code{data} \tab matrix or data frame specified in \code{data} \cr
#' \code{plot} \tab ggplot2 object for plotting the results \cr
#' \code{args} \tab specification of function arguments \cr
#' \code{result} \tab list with result tables \cr
#' }
#'
#' @note
#' This function is calls the \code{r2mlm_manual()} function in the \pkg{r2mlm}
#' package by Mairead Shaw, Jason Rights, Sonya Sterba, and Jessica Flake.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load misty, lme4, nlme, and ggplot2 package
#' library(misty)
#' library(lme4)
#'
#' # Load data set "Demo.twolevel" in the lavaan package
#' data("Demo.twolevel", package = "lavaan")
#'
#' #---------------------------
#'
#' # Cluster mean centering, center() from the misty package
#' Demo.twolevel$x2.c <- center(Demo.twolevel$x2, type = "CWC",
#'                              cluster = Demo.twolevel$cluster)
#'
#' # Compute group means, cluster.scores() from the misty package
#' Demo.twolevel$x2.b <- cluster.scores(Demo.twolevel$x2,
#'                                      cluster = Demo.twolevel$cluster)
#'
#' # Estimate random intercept model using the lme4 package
#' mod1 <- lmer(y1 ~ x2.c + x2.b + w1 + (1| cluster), data = Demo.twolevel,
#'              REML = FALSE, control = lmerControl(optimizer = "bobyqa"))
#'
#' # Estimate random intercept and slope model using the lme4 package
#' mod2 <- lmer(y1 ~ x2.c + x2.b + w1 + (1 + x2.c | cluster), data = Demo.twolevel,
#'              REML = FALSE, control = lmerControl(optimizer = "bobyqa"))
#'
#' #---------------------------
#' # Random intercept model
#'
#' # Fixed slope estimates
#' fixef(mod1)
#'
#' # Random effects variance-covariance matrix
#' as.data.frame(VarCorr(mod1))
#'
#' # R-squared measures according to Rights and Sterba (2019)
#' multilevel.r2.manual(data = Demo.twolevel,
#'                      within = "x2.c", between = c("x2.b", "w1"),
#'                      gamma.w = 0.41127956,
#'                      gamma.b = c(0.01123245, -0.08269374, 0.17688507),
#'                      tau = 0.9297401,
#'                      sigma2 = 1.813245794)
#'
#' #---------------------------
#' # Random intercept and slope model
#'
#' # Fixed slope estimates
#' fixef(mod2)
#'
#' # Random effects variance-covariance matrix
#' as.data.frame(VarCorr(mod2))
#'
#' # R-squared measures according to Rights and Sterba (2019)
#' multilevel.r2.manual(data = Demo.twolevel,
#'                      within = "x2.c", between = c("x2.b", "w1"), random = "x2.c",
#'                      gamma.w = 0.41127956,
#'                      gamma.b = c(0.01123245, -0.08269374, 0.17688507),
#'                      tau = matrix(c(0.931008649, 0.004110479, 0.004110479, 0.017068857), ncol = 2),
#'                      sigma2 = 1.813245794)
#' }
multilevel.r2.manual <- function(data, within = NULL, between = NULL, random = NULL,
                                 gamma.w = NULL, gamma.b = NULL, tau, sigma2,
                                 intercept = TRUE, center = TRUE, digits = 3,
                                 plot = FALSE, gray = FALSE, start = 0.15, end = 0.85,
                                 color = c("#D55E00", "#0072B2", "#CC79A7", "#009E73", "#E69F00"),
                                 check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'data' is missing
  if (isTRUE(missing(data))) { stop("Please specify a matrix or data frame for the argument 'data'.", call. = FALSE) }

  # Check if input 'data' is NULL
  if (isTRUE(is.null(data))) { stop("Input specified for the argument 'data' is NULL.", call. = FALSE) }

  # Check if input 'tau' is missing
  if (isTRUE(missing(tau))) { stop("Please specify a matrix for the argument 'tau'.", call. = FALSE) }

  # Check if input 'tau' is NULL
  if (isTRUE(is.null(tau))) { stop("Input specified for the argument 'tau' is NULL.", call. = FALSE) }

  # Check if input 'sigma2' is missing
  if (isTRUE(missing(sigma2))) { stop("Please specify a numeric value for the argument 'sigma2'.", call. = FALSE) }

  # Check if input 'sigma' is NULL
  if (isTRUE(is.null(sigma2))) { stop("Input specified for the argument 'sigma2' is NULL.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # r2mlm package
    if (isTRUE(!nzchar(system.file(package = "r2mlm")))) { stop("Package \"r2mlm\" is needed for to compute R-squared according to Rights and Sterba (2019), please install the package.", call. = FALSE) }

    #......
    # Check input 'within'

    # Character vector
    if (isTRUE(is.character(within))) {

      # Check if level-1 predictors are in the data
      var.data <- !within %in% colnames(data)
      if (isTRUE(any(var.data))) {

        stop(paste0("Predictors specified in the argument 'within' were not found in 'data': ", paste(within[which(var.data)], collapse = ", ")), call. = FALSE)

      }

    } else {

      if (isTRUE(ncol(data) < max(within))) {

        stop("Colummn numbers specified in the argument 'within' were not found in 'data'", call. = FALSE)

      }

    }

    #......
    # Check input 'between'

    # Character vector
    if (isTRUE(is.character(between))) {

      # Check if level-2 predictors are in the data
      var.data <- !between %in% colnames(data)
      if (isTRUE(any(var.data))) {

        stop(paste0("Predictors specified in the argument 'between' were not found in 'data': ", paste(between[which(var.data)], collapse = ", ")), call. = FALSE)

      }

    } else {

      if (isTRUE(ncol(data) < max(between))) {

        stop("Colummn numbers specified in the argument 'between' were not found in 'data'", call. = FALSE)

      }

    }

    # Check input 'random'
    # Character vector
    if (isTRUE(is.character(between))) {

      # Check if level-1 predictors with random slopes are in 'within'
      var.data <- !random %in% within
      if (isTRUE(any(var.data))) {

        stop(paste0("Predictors specified in the argument 'random' were not found in 'within': ", paste(between[which(var.data)], collapse = ", ")), call. = FALSE)

      }

    } else {

      if (isTRUE(any(!random %in% within))) {

        stop("Colummn numbers specified in the argument 'random' were not all found in 'within'", call. = FALSE)

      }

    }

    # Check input 'gamma.w'
    if (isTRUE(length(gamma.w) != length(within))) { stop("The number of parameter estimates specified in 'gamma.w' does not match with 'within'", call. = FALSE) }

    # Check input 'gamma.b'
    if (isTRUE(intercept)) {

      if (isTRUE(length(gamma.b) != (length(between) + 1))) { stop("The number of parameter estimates specified in 'gamma.b' does not match with the length of 'between' plus one for the intercept.", call. = FALSE) }

    } else {

      if (isTRUE(length(gamma.b) != length(between))) { stop("The number of parameter estimates specified in 'gamma.b' does not match with 'between'.", call. = FALSE) }

    }

    # Check input 'tau'
    if (isTRUE(!is.null(random))) {

      if (isTRUE(intercept)) {

        if (ncol(tau) != (length(random) + 1L)) { stop("The matrix specified in 'tau' does not match the number of level-1 predictors with random slopes plus one for the intercept.", call. = FALSE) }

      } else {

        if (ncol(tau) != (length(random))) { stop("The matrix specified in 'tau' does not match the number of level-1 predictors with random slopes.", call. = FALSE) }

      }

    } else {

      if (isTRUE(intercept)) {

        if (isTRUE(length(tau) != 1L)) { stop("Please specify a numeric value for the random intercept variance.", call. = FALSE) }

      }

    }

    # Check input 'sigma2'
    if (isTRUE(length(sigma2) != 1L || !is.numeric(sigma2))) { stop("Please specify a numeric value for the argument 'sigma2'.", call. = FALSE) }

    # Check input 'intercept'
    if (isTRUE(!is.logical(intercept))) { stop("Please specify TRUE or FALSE for the argument 'intercept'", call. = FALSE) }

    # Check input 'center'
    if (isTRUE(!is.logical(center))) { stop("Please specify TRUE or FALSE for the argument 'center'", call. = FALSE) }

    # Check input 'digits'
    if (isTRUE(digits %% 1L != 0L || digits < 0L)) { stop("Specify a positive integer value for the argument 'digits'.", call. = FALSE) }

    # Check input 'plot'
    if (isTRUE(!is.logical(plot))) { stop("Please specify TRUE or FALSE for the argument 'plot'", call. = FALSE) }

    # ggplot2 package
    if (isTRUE(!nzchar(system.file(package = "ggplot2")))) { warning("Package \"ggplot2\" is needed for drawing a bar chart, please install the package.", call. = FALSE) }

    # Check input 'gray'
    if (isTRUE(!is.logical(gray))) { stop("Please specify TRUE or FALSE for the argument 'gray'", call. = FALSE) }

    # Check input 'start'
    if (isTRUE(start < 0L || start > 1L)) { stop("Please specify a numeric value between 0 and 1 for the argument 'start'", call. = FALSE) }

    # Check input 'end'
    if (isTRUE(end < 0L || end > 1L)) { stop("Please specify a numeric value between 0 and 1 for the argument 'end'", call. = FALSE) }

    # Check input 'output'
    if (isTRUE(!is.logical(output))) { stop("Please specify TRUE or FALSE for the argument 'output'", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Data and Variables ---------------------------------------------------------

  if (isTRUE(is.character(within))) { within_covs <- match(within, colnames(data)) } else { within_covs <- within }

  if (isTRUE(is.character(between))) { between_covs <- match(between, colnames(data)) } else { between_covs <- between }

  if (isTRUE(is.character(random))) { random_covs <- match(random, colnames(data)) } else { random_covs <- random }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## r2mlm package ####

  r2mlm.out <- r2mlm::r2mlm_manual(data = data,
                                   within_covs = within_covs, between_covs = between_covs,
                                   random_covs = random_covs,
                                   gamma_w = gamma.w, gamma_b = gamma.b,
                                   Tau = tau, sigma2 = sigma2,
                                   has_intercept = intercept, clustermeancentered = center,
                                   bargraph = FALSE)

  rs <- suppressWarnings(list(decomp = matrix(apply(r2mlm.out$Decomposition, 2L, as.numeric),
                                              ncol = ncol(r2mlm.out$Decomposition),
                                              dimnames = list(rownames(r2mlm.out$Decompositions),
                                                              colnames(r2mlm.out$Decompositions))),
                              r2 = matrix(apply(r2mlm.out$R2s, 2L, as.numeric), ncol = ncol(r2mlm.out$R2s),
                                          dimnames = list(rownames(r2mlm.out$R2s), colnames(r2mlm.out$R2s)))))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Plot ####

  part <- NULL

  # Predictors are not cluster-mean-centered
  if (isTRUE(ncol(rs$decomp) == 1L)) {

    df <- data.frame(var = factor(rep("Total", times = 4L)),
                     part = factor(c("Fixed Slopes", "Slope Variation", "Intercept Variation", "Residual"),
                                   levels = c("Residual", "Intercept Variation", "Slope Variation", "Fixed Slopes")),
                     y = as.vector(rs$decomp))

  # Predictors are cluster-mean-centered
  } else {

    df <- data.frame(var = factor(rep(c("Total", "Within", "Between"), each = 5L),
                                  levels = c("Total", "Within", "Between")),
                     part = factor(c("Fixed Slopes (Within)", "Fixed Slopes (Between)","Slope Variation (Within)", "Intercept Variation (Between)", "Residual (Within)"),
                                   levels = c("Residual (Within)", "Intercept Variation (Between)", "Slope Variation (Within)", "Fixed Slopes (Between)", "Fixed Slopes (Within)")),
                     y = as.vector(rs$decomp))

  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = var, y = y, fill = part)) +
         ggplot2::geom_bar(stat = "identity") +
         ggplot2::scale_y_continuous(name = "Proportion of Variance",
                                     breaks = seq(0L, 1L, by = 0.1)) +
         ggplot2::theme_bw() +
         ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                        axis.ticks.x = ggplot2::element_blank(),
                        legend.title = ggplot2::element_blank(),
                        legend.position = "bottom",
                        legend.box.margin = ggplot2::margin(-10L, 6L, 6L, 6L)) +
         ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2L, reverse = TRUE))

  # Gray color scales
  if (isTRUE(gray)) {

    p <- p + ggplot2::scale_fill_grey(start = end, end = start)

  } else {

    p <- p + ggplot2::scale_fill_manual(values = rev(color))

  }

  # Print plot
  if (isTRUE(plot)) { suppressWarnings(print(p)) }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "multilevel.r2.manual",
                 data = data,
                 plot = p,
                 args = list(within = within, between = between, random = random,
                             gamma.w = gamma.w, gamma.b = gamma.b, tau = tau,
                             sigma2 = sigma2, intercept = intercept, center = center,
                             digits = digits, plot = plot, gray = gray,
                             start = start, end = end, color = color, check = check,
                             output = output),
                 result = list(decomp = rs$decomp,
                               total = data.frame(f1 = ifelse(ncol(rs$r2) > 1L, rs$r2[row.names(rs$r2) == "f1", "total"], NA),
                                                  f2 = ifelse(ncol(rs$r2) > 1L, rs$r2[row.names(rs$r2) == "f2", "total"], NA),
                                                  f = rs$r2[row.names(rs$r2) == "f", "total"],
                                                  v = rs$r2[row.names(rs$r2) == "v", "total"],
                                                  m = rs$r2[row.names(rs$r2) == "m", "total"],
                                                  fv = rs$r2[row.names(rs$r2) == "fv", "total"],
                                                  fvm = rs$r2[row.names(rs$r2) == "fvm", "total"]),
                               within = data.frame(f1 = ifelse(ncol(rs$r2) > 1L, rs$r2[row.names(rs$r2) == "f1", "within"], NA),
                                                   v = ifelse(ncol(rs$r2) > 1L, rs$r2[row.names(rs$r2) == "v", "within"], NA),
                                                   f1v = ifelse(ncol(rs$r2) > 1L, rs$r2[row.names(rs$r2) == "fv", "within"], NA)),
                               between = data.frame(f2 = ifelse(ncol(rs$r2) > 1L, rs$r2[row.names(rs$r2) == "f2", "between"], NA),
                                                    m  = ifelse(ncol(rs$r2) > 1L, rs$r2[row.names(rs$r2) == "m", "between"], NA))))

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
