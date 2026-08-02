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
#' @param filename  a character string indicating the \code{filename}
#'                  argument including the file extension in the \code{ggsave}
#'                  function. Note that one of \code{".eps"}, \code{".ps"},
#'                  \code{".tex"}, \code{".pdf"} (default),
#'                  \code{".jpeg"}, \code{".tiff"}, \code{".png"},
#'                  \code{".bmp"}, \code{".svg"} or \code{".wmf"} needs
#'                  to be specified as file extension in the \code{file}
#'                  argument. Note that plots can only be saved when
#'                  \code{plot = TRUE}.
#' @param width     a numeric value indicating the \code{width} argument
#'                  (default is the size of the current graphics device)
#'                  in the \code{ggsave} function.
#' @param height    a numeric value indicating the \code{height} argument
#'                  (default is the size of the current graphics device)
#'                  in the \code{ggsave} function.
#' @param units     a character string indicating the \code{units} argument
#'                  (default is \code{in}) in the \code{ggsave} function.
#' @param dpi       a numeric value indicating the \code{dpi} argument
#'                  (default is \code{600}) in the \code{ggsave} function.
#' @param write      a character string naming a text file with file extension
#'                   \code{".txt"} (e.g., \code{"Output.txt"}) for writing the
#'                   output into a text file.
#' @param write      a character string naming a text file with file extension
#'                   \code{".txt"} (e.g., \code{"Output.txt"}) for writing the
#'                   output into a text file.
#' @param append     logical: if \code{TRUE} (default), output will be appended
#'                   to an existing text file with extension \code{.txt} specified
#'                   in \code{write}, if \code{FALSE} existing text file will be
#'                   overwritten.
#' @param check      logical: if \code{TRUE} (default), argument specification is
#'                   checked.
#' @param output     logical: if \code{TRUE}, (default) output is shown on the console.
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
#' This function is based on a copy of the function \code{r2mlm_manual()} in the
#' \pkg{r2mlm} package by Mairead Shaw, Jason Rights, Sonya Sterba, and Jessica
#' Flake.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load misty and lme4 package
#' misty::libraries(misty, lme4)
#'
#' # Load data set "Demo.twolevel" in the lavaan package
#' data("Demo.twolevel", package = "lavaan")
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Estimate Multilevel Models
#'
#' # Cluster mean centering, center() from the misty package
#' Demo.twolevel <- center(Demo.twolevel, x2, type = "CWC", cluster = "cluster")
#'
#' # Compute group means, cluster.scores() from the misty package
#' Demo.twolevel <- cluster.scores(Demo.twolevel, x2, cluster = "cluster", name = "x2.b")
#'
#' # Estimate random intercept model using the lme4 package
#' mod1 <- lmer(y1 ~ x2.c + x2.b + w1 + (1| cluster), data = Demo.twolevel,
#'              REML = FALSE, control = lmerControl(optimizer = "bobyqa"))
#'
#' # Estimate random intercept and slope model using the lme4 package
#' mod2 <- lmer(y1 ~ x2.c + x2.b + w1 + (1 + x2.c | cluster), data = Demo.twolevel,
#'              REML = FALSE, control = lmerControl(optimizer = "bobyqa"))
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # R-Squared Measures According to Rights and Sterba (2019)
#'
#' # Fixed slope estimates
#' fixef(mod1)
#'
#' # Random effects variance-covariance matrix
#' as.data.frame(VarCorr(mod1))
#'
#' # Example 1a: R-squared measures for a random intercept model
#' multilevel.r2.manual(data = Demo.twolevel,
#'                      within = "x2.c", between = c("x2.b", "w1"),
#'                      gamma.w = 0.41127956,
#'                      gamma.b = c(0.01123245, -0.08269374, 0.17688507),
#'                      tau = 0.9297401,
#'                      sigma2 = 1.813245794)
#'
#' #——————————————————————————————————————
#'
#' # Fixed slope estimates
#' fixef(mod2)
#'
#' # Random effects variance-covariance matrix
#' as.data.frame(VarCorr(mod2))
#'
#' # Example 1b: R-squared measures for a random intercept and slope model
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
                                 filename = NULL, width = NA, height = NA,
                                 units = c("in", "cm", "mm", "px"), dpi = 600,
                                 write = NULL, append = TRUE, check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'data' is missing or NULL
  if (isTRUE(missing(data) || is.null(data))) { stop("Please specify a matrix or data frame for the argument 'data'.", call. = FALSE) }

  # Check if input 'tau' is missing or NULL
  if (isTRUE(missing(tau) || is.null(tau))) { stop("Please specify a matrix for the argument 'tau'.", call. = FALSE) }

  # Check if input 'sigma2' is missing or NULL
  if (isTRUE(missing(sigma2) || is.null(sigma2))) { stop("Please specify a numeric value for the argument 'sigma2'.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  .check.input(logical = c("intercept", "center", "plot", "gray", "append", "output"),
               numeric = list(start = 1L, end = 1L),
               args = c("digits", "write1"),
               envir = environment(), input.check = check)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Additional Checks ####

  if (isTRUE(check)) {

    # Package ggplot2
    if (isTRUE(plot)) { if (isTRUE(!nzchar(system.file(package = "ggplot2")))) { stop("Package \"ggplot2\" is needed to draw a plot, please install the package.", call. = FALSE) } }

    #—————————————————————————————————————— #
    ### Check Input 'within' ####

    # Character vector
    if (isTRUE(is.character(within))) {

      # Check if level-1 predictors are in the data
      (!within %in% colnames(data)) |> (\(y) if (isTRUE(any(y))) { stop(paste0("Predictors specified in the argument 'within' were not found in 'data': ", paste(within[which(y)], collapse = ", ")), call. = FALSE) })()

    } else {

      if (isTRUE(ncol(data) < max(within))) { stop("Colummn numbers specified in the argument 'within' were not found in 'data'", call. = FALSE) }

    }

    #—————————————————————————————————————— #
    ### Check Input 'between' ####

    # Character vector
    if (isTRUE(is.character(between))) {

      # Check if level-2 predictors are in the data
      (!between %in% colnames(data)) |> (\(y) if (isTRUE(any(y))) { stop(paste0("Predictors specified in the argument 'between' were not found in 'data': ", paste(between[which(y)], collapse = ", ")), call. = FALSE) })()

    } else {

      if (isTRUE(ncol(data) < max(between))) { stop("Colummn numbers specified in the argument 'between' were not found in 'data'", call. = FALSE) }

    }

    #—————————————————————————————————————— #
    ### Check Input 'random' ####

    # Character vector
    if (isTRUE(is.character(between))) {

      # Check if level-1 predictors with random slopes are in 'within'
      (!random %in% within) |> (\(y) if (isTRUE(any(y))) { stop(paste0("Predictors specified in the argument 'random' were not found in 'within': ", paste(between[which(y)], collapse = ", ")), call. = FALSE) })()

    } else {

      if (isTRUE(any(!random %in% within))) { stop("Colummn numbers specified in the argument 'random' were not all found in 'within'", call. = FALSE) }

    }

    #—————————————————————————————————————— #
    ### Check Input 'gamma.w' ####

    if (isTRUE(length(gamma.w) != length(within))) { stop("The number of parameter estimates specified in 'gamma.w' does not match with 'within'", call. = FALSE) }

    #—————————————————————————————————————— #
    ### Check Input 'gamma.b' ####

    if (isTRUE(intercept)) {

      if (isTRUE(length(gamma.b) != (length(between) + 1))) { stop("The number of parameter estimates specified in 'gamma.b' does not match with the length of 'between' plus one for the intercept.", call. = FALSE) }

    } else {

      if (isTRUE(length(gamma.b) != length(between))) { stop("The number of parameter estimates specified in 'gamma.b' does not match with 'between'.", call. = FALSE) }

    }

    #—————————————————————————————————————— #
    ### Check Input 'tau' ####

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

    # Check input 'start'
    if (isTRUE(start < 0L || start > 1L)) { stop("Please specify a numeric value between 0 and 1 for the argument 'start'", call. = FALSE) }

    # Check input 'end'
    if (isTRUE(end < 0L || end > 1L)) { stop("Please specify a numeric value between 0 and 1 for the argument 'end'", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Data and Variables ---------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'within', 'between', and 'random' Arguments ####

  if (isTRUE(is.character(within))) { within_covs <- match(within, colnames(data)) } else { within_covs <- within }

  if (isTRUE(is.character(between))) { between_covs <- match(between, colnames(data)) } else { between_covs <- between }

  if (isTRUE(is.character(random))) { random_covs <- match(random, colnames(data)) } else { random_covs <- random }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'units' Argument ####

  # Default setting
  if (isTRUE(all(c("in", "cm", "mm", "px") %in% units))) { units <- "in" }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## R-squared Measures ####

  r2mlm.out <- .r2mlm_manual(data = data,
                            within_covs = within_covs, between_covs = between_covs,
                            random_covs = random_covs,
                            gamma_w = gamma.w, gamma_b = gamma.b,
                            Tau = tau, sigma2 = sigma2,
                            has_intercept = intercept, clustermeancentered = center)

  rs <- suppressWarnings(list(decomp = matrix(apply(r2mlm.out$Decomposition, 2L, as.numeric),
                                              ncol = ncol(r2mlm.out$Decomposition),
                                              dimnames = list(rownames(r2mlm.out$Decompositions), colnames(r2mlm.out$Decompositions))),
                              r2 = matrix(apply(r2mlm.out$R2s, 2L, as.numeric), ncol = ncol(r2mlm.out$R2s),
                                          dimnames = list(rownames(r2mlm.out$R2s), colnames(r2mlm.out$R2s)))))

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "multilevel.r2.manual",
                 data = data,
                 args = list(within = within, between = between, random = random,
                             gamma.w = gamma.w, gamma.b = gamma.b, tau = tau,
                             sigma2 = sigma2, intercept = intercept, center = center,
                             digits = digits, plot = plot, gray = gray,
                             start = start, end = end, color = color,
                             width = width, height = height, units = units, dpi = dpi,
                             write = write, append = append, check = check, output = output),
                 plot = NULL,
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
  # Plot and Save Plot ---------------------------------------------------------

  if (isTRUE(plot)) { object$plot <- plot(object, filename = filename, width = width, height = height, units = units, dpi = dpi, check = FALSE) |> (\(y) suppressMessages(suppressWarnings(print(y))))() }

  #_____________________________________________________________________________
  #
  # Write Results --------------------------------------------------------------

  if (isTRUE(!is.null(write))) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
