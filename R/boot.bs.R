#' Bollen-Stine Bootstrapping with Incomplete Data
#'
#' This function performs the model-based Bollen-Stine Bootstrapping with incomplete
#' data of the chi-square statistic. By default, the function performs model-based
#' bootstrapping based on transformation method 2 in Savalei and Yuan (2009).
#'
#' @param object   an object of class lavaan, i.e., a fitted latent variable model
#'                 including mean structures, i.e., \code{meanstructure = TRUE}.
#' @param data     a data frame representing the target raw data set, optional
#'                 argument if the argument \code{object} is not specified. Note
#'                 that the data frame should only include variables that are
#'                 used in the covariance matrix and mean vector specified in
#'                 the arguments \code{sigma} and \code{mu}.
#' @param model    a character string. Optional argument representing the target
#'                 model if the argument \code{object} is not specified.
#' @param sigma    a matrix. Optional argument representing the model-implied
#'                 covariance matrix if the argument \code{object} is not specified.
#' @param mu       a numeric vector. Optional argument representing the model-implied
#'                 mean vector if the argument \code{object} is not specified.
#' @param group    a character vector. Optional argument representing the name of
#'                 the grouping variable in \code{data} if the argument \code{object}
#'                 is not specified.
#' @param chisq    a numeric value. Optional argument representing the model's
#'                 \eqn{\chi^2} test statistic if the argument \code{object} is
#'                 not specified.
#' @param em.cov   a matrix. Optional argument representing the EM or Two-Stage
#'                 ML estimated covariance matrix used to speed up the Transformation
#'                 2 algorithm.
#' @param trans    a character string representing the transformation method in
#'                 Savalei and Yuan (2009). There are three methods presented in
#'                 the article, but only the first two are currently implemented
#'                 in the function, i.e., \code{trans = 1} when there are few
#'                 missing data patterns, each of which has a large size, such as
#'                 in a planned missing data design, or \code{trans = 2} (default)
#'                 when there are more missing data patterns.
#' @param nrep     a numeric value indicating the number of bootstrap replicates
#'                 (default is 500).
#' @param return   a character string indicating which results to return, i.e.,
#'                 \code{"transdat"} for only the transformed data, \code{"bootsamp"}
#'                 for only the bootstrap samples, or \code{"output"} (default)
#'                 for the output table for the Bollen-Stine Bootstrapping of the
#'                 chi-square statistic.
#' @param seed     a numeric value specifying the seed of the pseudo-random numbers
#'                 used when drawing bootstrap samples.
#' @param progress logical: if \code{TRUE} (default), progress bar will be displayed
#'                 while fitting the model to the bootstrap samples. Note that
#'                 a \code{for} loop is used when \code{progress = TRUE}, while
#'                 the \code{sapply} function is used when \code{progress = FALSE}.
#' @param digits   an integer value indicating the number of decimal places
#'                 to be used for displaying the \eqn{\chi^2} test statistic.
#' @param p.digits an integer value indicating the number of decimal places
#'                 to be used for displaying the \emph{p}-values.
#' @param plot     logical: if \code{TRUE}, bootstrap sampling distribution of
#'                 the \eqn{\chi^2} test statistic is plotted with a histogram
#'                 including a density curve.
#' @param filename a character string indicating the \code{filename} argument
#'                 including the file extension in the \code{ggsave} function.
#'                 Note that one of \code{".eps"}, \code{".ps"}, \code{".tex"},
#'                 \code{".pdf"} (default), \code{".jpeg"}, \code{".tiff"},
#'                 \code{".png"}, \code{".bmp"}, \code{".svg"} or \code{".wmf"}
#'                 needs to be specified as file extension in the \code{file}
#' @param width    a numeric value indicating the \code{width} argument (default
#'                 is the size of the current graphics device) in the \code{ggsave}
#'                 function.
#' @param height   a numeric value indicating the \code{height} argument (default
#'                 is the size of the current graphics device) in the \code{ggsave}
#'                 function.
#' @param dpi      a numeric value indicating the \code{dpi} argument
#'                 (default is \code{600}) in the \code{ggsave} function.
#' @param write    a character string naming a file for writing the output into
#'                 either a text file with file extension \code{".txt"} (e.g.,
#'                 \code{"Output.txt"}) or Excel file with file extension
#'                 \code{".xlsx"} (e.g., \code{"Output.xlsx"}). If the file name
#'                 does not contain any file extension, an Excel file will be
#'                 written.
#' @param append   logical: if \code{TRUE} (default), output will be appended to
#'                 an existing text file with extension \code{.txt} specified in
#'                 \code{write}, if \code{FALSE} existing text file will be
#'                 overwritten.
#' @param check    logical: if \code{TRUE} (default), argument specification
#'                 is checked.
#' @param output   logical: if \code{TRUE} (default), output is shown.
#' @param ...      additional arguments in the lavaan::lavaan() function, see
#'                 lavaan::lavOptions().
#'
#' @author
#' Takuya Yanagida
#'
#' @references
#' Bollen, K. A., & Stine, R. A. (1992). Bootstrapping goodness-of-fit measures
#' in structural equation models. \emph{Sociological Methods & Research, 21}(2),
#' 205-229. https://doi.org/10.1177/0049124192021002004
#'
#' Jorgensen, T. D., Pornprasertmanit, S., Schoemann, A. M., & Rosseel, Y. (2026).
#' \emph{semTools: Useful tools for structural equation modeling}. R package version
#' 0.5-8. Retrieved from https://CRAN.R-project.org/package=semTools
#'
#' Savalei, V., & Yuan, K.-H. (2009). On the model-based bootstrap with missing
#' data: Obtaining a p-value for a test of exact fit. \emph{Multivariate Behavioral
#' Research, 44}(6), 741-763. https://doi.org/10.1080/00273170903333590
#'
#' @note This function is based on modified copies of the functions \code{bsBootMiss}
#' from the \pkg{semTools} package by Terrence D. Jorgensen et al. (2026).
#'
#' @return
#' Returns an object of class \code{misty.object} when specifying \code{return = "output"}:
#'
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{object}}{object of class lavaan specified in the argument \code{object}}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{plot}}{ggplot2 object when specifying \code{plot = TRUE}}
#' \item{\code{result}}{result table}
#'
#' When specifying \code{return = "transdat"}, the transformed data and when
#' specifying \code{return = "bootsamp"}, the bootstrap samples are returned.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load lavaan package
#' library(lavaan)
#'
#' # Holzinger and Swineford data set
#' dat <- HolzingerSwineford1939
#'
#' # Introduce missing data
#' dat$x5 <- ifelse(dat$x1 <= quantile(dat$x1, 0.3), NA, dat$x5)
#' dat$x9 <- ifelse(is.na(dat$x5), NA, dat$x9)
#'
#' # Model specification
#' model <- 'visual  =~ x1 + x2 + x3
#'           textual =~ x4 + x5 + x6
#'           speed   =~ x7 + x8 + x9'
#'
#' # Model estimation
#' fit <- sem(model, data = dat, meanstructure = TRUE, std.lv = TRUE,
#'            missing = "fiml", group = "school")
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Bollen-Stine Bootstrapping with Incomplete Data
#'
#' # Example 1: Default setting, transformation method 2, 500 replicates
#' # Plot bootstrap sampling distribution of the test statistic
#' boot.bs(fit, seed = 42, plot = TRUE)
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Transformed Data and Bootstrap Samples
#'
#' # Example 2: Return transformed data only
#' transdat <- boot.bs(fit, return = "transdat")
#'
#' # Example 3: Return bootstrap samples only
#' bootsamp <- boot.bs(fit, return = "bootsamp")
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Plot Bootstrap Sampling Distribution of Chi-Square Test Statistic
#'
#' # Bollen-Stine Bootstrapping
#' object <- boot.bs(fit, seed = 42)
#'
#' # Load ggplot2 package
#' library(ggplot2)
#'
#' # Plot data
#' plotdat <- data.frame(chisq = object$boot.chisq)
#'
#' # Example 3: Plot bootstrap sampling distribution, create plot manually
#' ggplot(plotdat, aes(chisq)) +
#'   geom_histogram(aes(y = after_stat(density)), color = "black", alpha = 0.4, fill = "gray85") +
#'   geom_density(color = "#0072B2") +
#'   geom_vline(aes(xintercept = object$result$chisq, color = "Observed Test Statistic")) +
#'   scale_x_continuous(name = expression(paste(chi^2, " Test Statistic")),
#'                      limits = c(0, max(c(plotdat$chisq, object$result$chisq), na.rm = TRUE))) +
#'   scale_y_continuous(name = "Probability Density, f(x)", expand = expansion(mult = c(0, 0.05))) +
#'   scale_color_manual(values = c("Observed Test Statistic" = "#CC79A7")) +
#'   theme_bw() +
#'   theme(legend.position = "bottom", legend.box.margin = margin(-12, 0, 0, 0),
#'         legend.title = element_blank())
#' }
boot.bs <- function(object = NULL, data = NULL, model = NULL, sigma = NULL, mu = NULL,
                    group = NULL, chisq = NULL, em.cov = NULL, trans = c(1, 2),
                    nrep = 500, return = c("transdat", "bootsamp", "output"), seed = NULL,
                    progress = TRUE, digits = 2, p.digits = 3, plot = FALSE, filename = NULL,
                    width = NA, height = NA, dpi = 600, write = NULL, append = TRUE,
                    check = TRUE, output = TRUE, ...) {

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  .check.input(logical = c("progress", "plot"), numeric = list(seed = 1L), s.character = list(return = c("transdat", "bootsamp", "output")),  args = "nrep", envir = environment(), input.check = check)

  # Check argument 'trans'
  if (isTRUE(check)) { if (isTRUE(!trans %in% c(1L, 2L))) { stop("Please specify 1 or 2 for the argument 'trans'.", call. = FALSE) } }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  # 'trans' Argument
  if (isTRUE(all(c(1L, 2L) %in% trans))) { trans <- 2 }

  # 'return' Argument
  if (isTRUE(all(c("transdat", "bootsamp", "output") %in% return))) { return <- "output" }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data Preparation ####

  #—————————————————————————————————————— #
  ### lavaan Object Specified ####

  if (isTRUE(!is.null(object))) {

    # Check if object is of class lavaan
    if (isTRUE(class(object) != "lavaan")) { stop("Please specify an object of class lavaan for the argument 'object'.", call. = FALSE) }

    # Check if model is not a multilevel model
    if (isTRUE(lavaan::lavInspect(object, what = "options")$.multilevel)) { stop("This function cannot handle multilevel models.", call. = FALSE) }

    # Check if model includes mean structures
    if (isTRUE(!lavaan::lavInspect(object, what = "options")$meanstructure)) { stop("Please specify a fitted lavaan object including a mean structure for the argument 'object', i.e., meanstructure = TRUE.", call. = FALSE) }

    # Number of groups
    ngroups <- lavaan::lavInspect(object, what = "ngroups")

    # Single- or multiple-group model
    if (isTRUE(ngroups == 1L)) {

      data <- list(as.data.frame(lavaan::lavInspect(object, what = "data")))

    } else  {

      data <- lapply(lavaan::lavInspect(object, what = "data"), as.data.frame)

    }

    # Remove rows containing all missing data
    for (i in seq_along(data)) {

      colnames(data[[i]]) <- lavaan::lavNames(object)
      apply(data[[i]], 1L, function(y) all(is.na(y))) |> (\(p) if (any(p)) data[[i]] <<- data[[i]][!p, ])()

    }

    # Extract (scaled) chi-square value
    chisq <- suppressWarnings(unname(lavaan::lavInspect(object, what = "fit")[c("chisq", "chisq.scaled")] |> (\(p) ifelse(is.na(p[2L]), p[1L], p[2L]))()))

    # Extract name of grouping variable
    group <- lavaan::lavInspect(object, what = "group") |> (\(p) if (isTRUE(length(p) == 0L)) { "group" } else { p })()

    # Extract group labels
    group.label <- lavaan::lavInspect(object, what = "group.label") |> (\(p) if (isTRUE(length(p) == 0L)) { 1L } else { p })()

    # Model-implied covariance matrix
    sigma <- lavaan::lavInspect(object, "cov.ov")

    # Model-implied mean vector
    mu <- lavaan::lavInspect(object, "mean.ov")

    # EM or Two-Stage ML estimated covariance matrix
    em.cov <- lavaan::lavInspect(object, what = "sampstat")$cov

    # Convert to lists
    if (isTRUE(ngroups == 1L)) {

      sigma <- list(sigma)
      mu <- list(mu)
      em.cov <- list(em.cov)

    }

  #—————————————————————————————————————— #
  ### lavaan Object Not Specified ####

  } else {

    if (isTRUE(any(c(is.null(data), is.null(sigma), is.null(mu))))) { stop("Please specify the arguments 'data', 'sigma', and 'mu' in case a lavaan fitted object was not supplied.", call. = FALSE) }

    if (isTRUE(is.null(model) && return == "output"))  { stop("Please specify the arguments 'model' in case a lavaan fitted object was not supplied.", call. = FALSE) }

    # Convert to lists
    if (isTRUE(inherits(data, what = "data.frame"))) { data <- list(data) }

    if (isTRUE(!inherits(data, what = "list"))) {

      stop("Please specify a data frame or list for the argument 'data'.", call. = FALSE)

    } else if (isTRUE(!all(sapply(data, is.data.frame)))) {

      stop("Please specify a data frame for every element of the argument 'data'.", call. = FALSE)

    }

    if (isTRUE(inherits(sigma, what = "matrix"))) { sigma <- list(sigma) }

    if (isTRUE(is.numeric(mu))) { mu <- list(mu) }

    if (isTRUE(is.null(em.cov))) {

      em.cov <- vector("list", length(sigma))

    } else {

      if (isTRUE(inherits(em.cov, what = "matrix"))) { em.cov <- list(em.cov) }

      for (i in seq_along(em.cov)) {

        if (isTRUE(!isSymmetric(em.cov[[i]]))) { stop("The argument 'em.cov' in group ", i, " is not symmetric.", call. = FALSE) }

        if (isTRUE(!all(dim(em.cov[[i]]) == dim(sigma[[i]])))) { stop("Unequal dimensions in sigma and em.cov.", call. = FALSE) }

      }

    }

    ## Check the number of groups by the size of the lists.
    if (isTRUE(!all(length(data) == c(length(sigma), length(mu))))) { stop("Unequal number of groups in data, sigma, and mu. For multiple-group models, data must be a list of data frames, not a single data frame with a \"group\" column.", call. = FALSE) }

    ngroups <- length(sigma)

    ## Check if sigma is symmetric and dimensions match data and mu in each group
    for (i in seq_along(data)) {

      if (isTRUE(!isSymmetric(sigma[[i]]))) { stop("sigma in group ", i, " is not symmetric.", call. = FALSE) }

      if (isTRUE(!all(ncol(data[[i]]) == c(nrow(sigma[[i]]), length(mu[[i]]))))) { stop("Unequal dimensions in data, sigma, and mu.", call. = FALSE) }

    }

    ## Check for names of group levels
    if (isTRUE(is.null(group))) { group <- "group" }

    if (isTRUE(!is.character(group))) { stop("The argument 'group' must be a character string.", call. = FALSE) }

    if (isTRUE(is.null(names(data)))) {

      group.label <- paste0("g", seq_along(data))

    } else {

      group.label <- names(data)

    }

  }

  data.trans <- setNames(data, nm = group.label)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data Transformation ####

  for (i in seq_along(group.label)) {

    #—————————————————————————————————————— #
    ### Transformation 1 ####

    if (isTRUE(trans == 1L)) {

      # Missing data patterns
      pattern <- apply(ifelse(is.na(data[[i]]), 1L, 0L), 1L, function(y) paste(y, collapse = ""))

      pattern.uniq <- unique(pattern)
      pattern.rows <- lapply(pattern.uniq, function(y) which(pattern == y))

      # Apply .trans1 for each MD pattern
      data.trans.list <- lapply(pattern.uniq, .trans1, pattern = pattern, dat = data[[i]], sigma = sigma[[i]], mu = mu[[i]])

      for (j in seq_along(pattern.uniq)) { data.trans[[i]][pattern.rows[[j]], ] <- data.trans.list[[j]] }

    #—————————————————————————————————————— #
    ### Transformation 2 ####

    } else if (isTRUE(trans == 2L)) {

      data.trans[[i]] <- .trans2(dat = data[[i]], sigma = sigma[[i]], mu = mu[[i]], em.cov = em.cov[[i]])

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Bootstrap Samples ####

  boot.samples <- NULL
  if (isTRUE(return %in% c("bootsamp", "output"))) {

    # Set seed
    if (isTRUE(!is.null(seed))) { set.seed(seed) }

    # Draw bootstrap samples
    boot.samples <- lapply(seq_len(nrep), function(y) .getBootSample(data.trans, group = group, group.label = group.label))

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Fit Models to Bootstrap Samples ####

  boot.p <- nconv <- df <- NULL
  if (isTRUE(return == "output")) {

    #—————————————————————————————————————— #
    ### lavaan Arguments ####

    # lavaan arguments in ...
    lavaan.args <- list(...)
    lavaan.args$group <- group

    # Fit model to bootstrap samples, save distribution of chi-squared test statistic
    if (isTRUE(!is.null(object))) {

      ## Default setting from the lavaan object
      lavaan.args$slotParTable <- as.list(lavaan::parTable(object))
      lavaan.args$slotModel <- object@Model
      lavaan.args$slotOptions <- lavaan::lavInspect(object, what = "options")

    } else {

      lavaan.args$model <- model
      lavaan.args$missing <- "fiml"
      lavaan.args$meanstructure <- TRUE
      if (isTRUE(!exists("auto.var", where = lavaan.args))) { lavaan.args$auto.var <- TRUE }
      if (isTRUE(!exists("auto.cov.y", where = lavaan.args))) { lavaan.args$auto.cov.y <- TRUE }
      if (isTRUE(!exists("auto.cov.lv.x", where = lavaan.args))) { lavaan.args$auto.cov.lv.x <- TRUE }

    }

    lavaan.args$slotOptions$se <- "none"
    lavaan.args$slotOptions$baseline <- FALSE
    lavaan.args$slotOptions$h1 <- FALSE
    lavaan.args$slotOptions$check.gradient <- FALSE
    lavaan.args$slotOptions$check.lv.names <- FALSE
    lavaan.args$slotOptions$check.post <- FALSE
    lavaan.args$slotOptions$check.start <- FALSE
    lavaan.args$slotOptions$check.vcov <- FALSE

    #—————————————————————————————————————— #
    ### Fit Models ####

    # With progress bar
    if (isTRUE(progress)) {

      progress.bar <- txtProgressBar(min = 1L, max = nrep, initial = 1L, char = "=", width = getOption("width") - 13L, style = 3, file = "")

      boot.chisq <- numeric()
      for (i in seq_len(nrep)) {

        boot.chisq[i] <- suppressWarnings(.fitBootSample(boot.samples[[i]], args = lavaan.args))

        setTxtProgressBar(progress.bar, value = i)

      }

      close(progress.bar)

    # Without progress bar
    } else {

      boot.chisq <- suppressWarnings(unname(sapply(boot.samples, .fitBootSample, args = lavaan.args)))

    }

    # Degrees of freedom
    if (isTRUE(!is.null(object))) {

      df <- suppressWarnings(lavaan::lavInspect(object, what = "fit")["df"])

    } else {

      lavaan.args$data <- boot.samples[[ which(!is.na(boot.chisq))[1L]]]
      df <- suppressWarnings(lavaan::lavInspect(do.call(function(...) { lavaan::lavaan(...) }, lavaan.args), what = "fit")["df"])

    }

    # Number of not converged bootstrap samples
    nconv <- sum(is.na(boot.chisq))

    #—————————————————————————————————————— #
    ### Bootstrapped p-Value ####

    boot.p <- mean(boot.chisq >= chisq, na.rm = TRUE)

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  #—————————————————————————————————————— #
  ### Transformed Data ####

  # Stack groups
  for (i in seq_along(data.trans)) { data.trans[[i]][ , group] <- group.label[i] }

  data.trans <- data.frame(do.call("rbind", data.trans), row.names = NULL)

  # Return transformed data
  if (isTRUE(return == "transdat")) { return(data.trans) }

  #—————————————————————————————————————— #
  ### Bootstrap Samples ####

  # Return bootstrap samples
  if (isTRUE(return == "bootsamp")) { return(boot.samples) }

  #—————————————————————————————————————— #
  ### Bootstrapping Result Table ####

  restab <- data.frame(nrep = nrep - nconv, nNA = nconv, chisq = chisq, df = df, p = pchisq(chisq, df = df, lower.tail = FALSE), boot.p = boot.p, row.names = NULL)

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "boot.bs",
                 object = object,
                 data.trans = data.trans,
                 boot.chisq = boot.chisq,
                 args = list(data = data, model = model, sigma = sigma, mu = mu, group = group, chisq = chisq, em.cov = em.cov, trans = trans, nrep = nrep, return = return, seed = seed, digits = digits, p.digits = p.digits, plot = plot, filename = filename, width = width, height = height, dpi = dpi, write = write, append = append, check = check, output = output),
                 plot = NULL,
                 result = restab)

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Plot and Save Plot ---------------------------------------------------------

  if (isTRUE(plot)) { object$plot <- plot(object, filename = filename, width = width, height = height, dpi = dpi, check = FALSE) |> (\(y) suppressMessages(suppressWarnings(print(y))))() }

  #_____________________________________________________________________________
  #
  # Write Results --------------------------------------------------------------

  if (isTRUE(!is.null(write))) { .write.result(object = object, write = write, append = append) }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}

#_______________________________________________________________________________
