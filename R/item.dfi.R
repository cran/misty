#' Dynamic Fit Index Cutoffs
#'
#' This function computes simulation-based dynamic fit index cutoffs (McNeish &
#' Wolf, 2022, 2023) for evaluating confirmatory factor models based on multivariate
#' normal, multivariate non-normal, likert-type, and categorical data using the
#' the omitted paths approach.
#'
#' @param model       an object of class lavaan, i.e., a fitted CFA measurement
#'                    model, an object of class misty of type \code{item.cfa}, or
#'                    a character string indicating the lavaan model syntax for
#'                    a CFA measurement model.
#' @param data        a data frame. Note that this argument is needed only when
#'                    specifying a character string for the argument \code{model}
#'                    while specifying \code{"nnorm"} or \code{"likert"} for the
#'                    argument \code{type} as the data frame is extracted from
#'                    the fitted model when specifying an object of class
#'                    \code{lavaan} or \code{misty} for the argument \code{model}.
#' @param n           a numeric value indicating the number of observations for
#'                    simulating fit index cutoffs. Note that this argument is
#'                    needed only when specifying a character string for the
#'                    argument \code{model} as the number of observations of the
#'                    fitted model is extracted from the fitted model when specifying
#'                    an object of class \code{lavaan} or \code{misty} for the
#'                    argument \code{model}.
#' @param type        a character string indicating how data are simulated, i.e.,
#'                    \code{"norm"} (default when specifying a character string
#'                    for the argument \code{model}) for assuming multivariate
#'                    normality across all items, \code{"nnorm"} (default when
#'                    specifying an object of class \code{lavaan} or \code{misty}
#'                    for the argument  \code{model}) for assuming multivariate
#'                    non-normality across all items, \code{"likert"} assuming
#'                    discrete likert-type items treated as continuous, or
#'                    \code{"categ"} (default when specifying a categorical CFA
#'                    model for the argument \code{model}) assuming ordered-categorical
#'                    items.
#' @param level       a numeric vector (default: \code{c(0, 1, 2, 3)}) indicating
#'                    the levels of misspecification for which fit index cutoffs
#'                    are simulated. Note that \code{0} represents the true model
#'                    without any misspecification and always needs to be included
#'                    in the argument \code{level}.
#' @param res.cor     a numeric value (default: \code{0.3}) indicating the magnitude
#'                    of the residual correlations between items introduced for
#'                    model misspecification in a one-factor CFA model.
#' @param estimator   a character string indicating the estimator to be used
#'                    for simulating fit index cutoffs (see 'Details' in the help
#'                    page of the \code{item.cfa()} function). Note that this
#'                    argument is needed only when specifying a character string
#'                    for the argument \code{model} as the estimator of the
#'                    fitted model is extracted from the fitted model when specifying
#'                    an object of class \code{lavaan} or \code{misty} for the
#'                    argument \code{model}.
#' @param fit.indices a character string indicating which version of the CFI, TLI,
#'                    and RMSEA to compute for simulating fit index cutoffs, i.e.,
#'                    \code{"standard"} for fit indices without any non-normality
#'                    correction, \code{"scaled"} for population-corrected robust
#'                    fit indices with ad hoc non-normality correction, and
#'                    \code{robust} for sample-corrected robust fit indices.
#' @param specific    a numeric value (default: \code{0.95}) indicating specificity,
#'                    i.e., proportions of correct models identified by the cutoffs.
#' @param sensitiv    a numeric value (default: \code{0.95}) indicating sensitivity,
#'                    i.e., proportions of incorrect models identified by the cutoffs.
#' @param nrep        an integer value (default: \code{500}) indicating the number of
#'                    replications in simulating fit index cutoffs.
#' @param seed        logical: if \code{TRUE} (default), the same seed of the
#'                    pseudo-random numbers for simulating fit index cutoffs are
#'                    used as in the R package \pkg{dynamic} to reproduce results
#'                    provided by the \code{cfaOne}, \code{cfaHB}, \code{nnorOne},
#'                    \code{nnorHB}, \code{likertOne}, \code{likertHB2}, \code{catOne},
#'                    and \code{catHB} from the \pkg{dynamic} package
#' @param progress    logical: if \code{TRUE} (default), progress bar will be
#'                    displayed while fitting the CFA measurement model to the
#'                    simulated samples. Note that a \code{for} loop is used when
#'                    \code{progress = TRUE}, while the \code{sapply} function
#'                    is used when \code{progress = FALSE}.
#' @param print       a character string or character vector indicating the
#'                    output shown on the console, i.e., \code{"all"} for all
#'                    outputs, \code{"summary"} (default) for a summary of the
#'                    specification in lavaan for the simulation, \code{"model"}
#'                    for the lavaan model syntax for the CFA measurement model
#'                    for each misspecification level specified for the simulation,
#'                    \code{"cutoff"} (default) for the simulated fit index cutoffs.
#' @param digits      an integer value (default: \code{3}) indicating the number
#'                    of decimal places to be used for displaying fit indices.
#' @param plot        logical: if \code{TRUE}, distributions of fit indices for
#'                    each level of misspecification is plotted.
#' @param filename    a character string indicating the \code{filename} argument
#'                    including the file extension in the \code{ggsave} function.
#'                    Note that one of \code{".eps"}, \code{".ps"}, \code{".tex"},
#'                    \code{".pdf"} (default), \code{".jpeg"}, \code{".tiff"},
#'                    \code{".png"}, \code{".bmp"}, \code{".svg"} or \code{".wmf"}
#'                    needs to be specified as file extension in the \code{file}
#' @param width       a numeric value indicating the \code{width} argument (default:
#'                    size of the current graphics device) in the \code{ggsave}
#'                    function.
#' @param height      a numeric value indicating the \code{height} argument (default:
#'                    size of the current graphics device) in the \code{ggsave}
#'                    function.
#' @param dpi         a numeric value indicating the \code{dpi} argument
#'                    (default: \code{600}) in the \code{ggsave} function.
#' @param write       a character string naming a file for writing the output
#'                    into either a text file with file extension \code{".txt"}
#'                    (e.g., \code{"Output.txt"}) or Excel file with file extension
#'                    \code{".xlsx"} (e.g., \code{"Output.xlsx"}). If the file
#'                    name does not contain any file extension, an Excel file will
#'                    be written.
#' @param append      logical: if \code{TRUE} (default), output will be appended
#'                    to an existing text file with extension \code{.txt} specified
#'                    in \code{write}, if \code{FALSE} existing text file will be
#'                    overwritten.
#' @param check       logical: if \code{TRUE} (default), argument specification
#'                    is checked.
#' @param output      logical: if \code{TRUE} (default), output is shown.
#'
#' @author
#' Takuya Yanagida
#'
#' @seealso
#' \code{\link{item.cfa}}
#'
#' @references
#' Liu, X., & McNeish, D. (2025). Optimal number of replications for obtaining
#' stable dynamic fit index cutoffs. \emph{Educational and Psychological Measurement, 85}(3),
#' 539–564. https://doi.org/10.1177/00131644241290172
#'
#' McNeish, D. (2023). Dynamic fit index cutoffs for categorical factor analysis
#' with Likert-type, ordinal, or binary responses. \emph{American Psychologist, 78}(9),
#' 1061–1075. https://doi.org/10.1037/amp0001213
#'
#' McNeish, D. & Wolf, M. G. (2022). Dynamic fit cutoffs for one-factor models.
#' \emph{Behavior Research Methods, 55}, 1157-1174. https://doi.org/10.3758/s13428-022-01847-y
#'
#' McNeish, D., & Wolf, M. G. (2023). Dynamic fit index cutoffs for confirmatory
#' factor analysis models. \emph{Psychological Methods, 28}(1), 61-88. https://doi.org/10.1037/met0000425
#'
#' McNeish, D. (2024). Dynamic fit index cutoffs for treating likert items as continuous.
#' \emph{Psychological Methods}. Advance online publication. https://doi.org/10.1037/met0000683
#'
#' Wolf, M. G., & McNeish, D. (2026). dynamic: DFI Cutoffs for Latent Variable Models.
#' R package version 1.1.0. Retrieved from https://github.com/melissagwolf/dynamic
#'
#' @note This function is based on the functions \code{cfaOne}, \code{cfaHB},
#' \code{nnorOne}, \code{nnorHB}, \code{likertOne}, \code{likertHB2},
#' \code{catOne}, and \code{catHB} from the \pkg{dynamic} package by Melissa
#' Gordon Wolf and Daniel McNeish (2026).
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{model}}{object or character string specified in the argument \code{model}}
#' \item{\code{data}}{a data frame extracted from the object specified in the
#'                    argument \code{model} or a data frame specified in the
#'                    argument \code{data}}
#' \item{\code{sim.model}}{a list of character strings indicating the lavaan model
#'                         syntax for the CFA measurement model for each misspecification
#'                         level specified for the simulation}
#' \item{\code{plot}}{ggplot2 object when specifying \code{plot = TRUE}}
#' \item{\code{result}}{list with results, i.e., \code{summary} for the summary
#'                      of the specification in lavaan for the simulation,
#'                      \code{summary.empirical} for the summary of the specification
#'                      in lavaan for the fitted model, \code{fit.sim} for a list
#'                      with data frames for the simulated fit indices, \code{fit.quant}
#'                      for a list with data frames with the quantiles for the
#'                      simulated fit indices, \code{fit.cutoff} for a data frame
#'                      with the simulated fit index cutoffs and the specificity
#'                      and sensitivity for each fit index, and \code{fit.emp}
#'                      for the chi-square value and empirical fit indices of
#'                      the fitted model.}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load lavaan package
#' library(lavaan)
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Object of Class misty
#'
#' #.....................
#' ## Multivariate Normality across all Items
#'
#' # Conduct confirmatory factor analysis: Continuous items
#' mod1a.fit <- item.cfa(HolzingerSwineford1939, x1:x6, estimator = "ML")
#'
#' # Example 1a: Simulate DFI cutoffs, multivariate normality
#' item.dfi(mod1a.fit, type = "norm")
#'
#' #.....................
#' ## Multivariate Non-Normality across all Items
#'
#' # Conduct confirmatory factor analysis: Continuous items
#' mod1b.fit <- item.cfa(HolzingerSwineford1939, x1:x6)
#'
#' # Example 1b: Simulate DFI cutoffs, multivariate non-normality (default)
#' item.dfi(mod1b.fit)
#'
#' #.....................
#' ## Likert-Type Items Treated as Continuous
#'
#' # Conduct confirmatory factor analysis: Likert-type items as continuous
#' mod1c.fit <- item.cfa(round(HolzingerSwineford1939[, c("x4", "x5", "x6", "x7")]))
#'
#' # Example 1c: Simulate DFI cutoffs, Likert-type
#' item.dfi(mod1c.fit, type = "likert")
#'
#' #.....................
#' ## Ordered-Categorical Items
#'
#' # Conduct confirmatory factor analysis: Ordered-categorical items
#' mod1d.fit <- item.cfa(round(HolzingerSwineford1939[, c("x4", "x5", "x6", "x7")]),
#'                       ordered = TRUE)
#'
#' # Example 1d: Simulate DFI cutoffs, ordered-categorical
#' item.dfi(mod1d.fit, nrep = 50)
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Object of Class lavaan
#'
#' # Model specification
#' mod <- 'f =~ x1 + x2 + x3 + x4 + x5 + x6'
#'
#' #.....................
#' ## Multivariate Normality across all Items
#'
#' # Model estimation
#' mod2a.fit <- cfa(mod, data = HolzingerSwineford1939, estimator = "ML")
#'
#' # Example 2a: Simulate DFI cutoffs, multivariate normality
#' mod2a.dfi <- item.dfi(mod2a.fit, type = "norm")
#'
#' #.....................
#' ## Multivariate Non-Normality across all Items
#'
#' # Model estimation
#' mod2b.fit <- cfa(mod, data = HolzingerSwineford1939, estimator = "MLR")
#'
#' # Example 2b: Simulate DFI cutoffs, multivariate non-normality (default)
#' mod2b.fit <- item.dfi(mod2b.fit)
#'
#' #.....................
#' ## Arguments 'print' and 'level'
#'
#' # Model estimation
#' mod2c.fit <- cfa(mod, data = HolzingerSwineford1939, estimator = "MLR")
#'
#' # Example 2c: Simulate DFI cutoffs, print all outputs
#' mod2c.dfi <- item.dfi(mod2c.fit, print = "all")
#'
#' # Example 2c: Print model syntax for each misspecification level
#' print(mod2c.dfi, print = "model")
#'
#' # Example 2d: Print fit index cutoffs with 5 digits
#' print(mod2c.dfi, digits = 5)
#'
#' # Example 2e: Simulate DFI cutoffs, simulate misspecification level 0 only
#' item.dfi(mod2c.fit, level = 0)
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Character String
#'
#' # Model specification
#' mod3 <- 'f =~ 0.42*x1 + 0.21*x2 + 0.20*x3 + 0.85*x4 + 0.85*x5 + 0.84*x6'
#'
#' # Example 3a: Simulate DFI cutoffs, multivariate normality (default)
#' item.dfi(mod3, n = 301, estimator = "ML")
#'
#' # Example 3b: Simulate DFI cutoffs, multivariate non-normality
#' item.dfi(mod3, n = 301, data = HolzingerSwineford1939, estimator = "MLR")
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Plot
#'
#' # Conduct confirmatory factor analysis
#' mod3.fit <- item.cfa(HolzingerSwineford1939, x1:x6)
#'
#' # Example 4: Plot distributions of fit indices for each level of misspecification
#' item.dfi(mod3.fit, plot = TRUE, nrep = 100)
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Write Results and Save Plot
#'
#' # Conduct confirmatory factor analysis
#' mod4.fit <- item.cfa(HolzingerSwineford1939, x1:x6)
#'
#' # Example 4a: Write Results into a text file
#' item.dfi(mod4.fit, write = "CFA_DFI.txt")
#'
#' # Example 4b: Write Results into an Excel file
#' item.dfi(mod4.fit, write = "CFA_DFI.xlsx")
#'
#' # Example 4c: Save Plot of distributions of fit indices
#' item.dfi(mod4.fit, plot = TRUE, filename = "CFA_DFI.png", width = 10, height = 7)
#' }
item.dfi <- function(model, data = NULL, n = NULL,
                     type = c("norm", "nnorm", "likert", "categ"),
                     level = c(0, 1, 2, 3), res.cor = 0.3, estimator = NULL,
                     fit.indices = c("standard", "scaled", "robust"),
                     specific = 0.95, sensitiv = 0.95, nrep = 500, seed = TRUE,
                     progress = TRUE, print = c("all", "summary", "model", "cutoff"),
                     digits = 3, plot = FALSE, filename = NULL, width = NA, height = NA,
                     dpi = 600, write = NULL, append = TRUE, check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'model' is missing or NULL
  if (isTRUE(missing(model) ||is.null(model))) { stop("Please specify a misty object, lavaan object, or character string for the argument 'model'", call. = FALSE) }

  # Check if input 'model' is not a misty object, lavaan object, or character string
  if (isTRUE(all(c(!(inherits(model, what = "misty.object") |> (\(p) ifelse(p, model$type == "item.cfa", FALSE))()), !inherits(model, what = "lavaan"), !(is.character(model) && length(model) == 1L))))) { stop("Please specify a misty object, lavaan object, or a lavaan model syntax for the argument 'model'", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("seed", "progress", "plot", "append", "output"), s.character = list(type = c("norm", "nnorm", "likert", "categ"), fit.indices = c("standard", "scaled", "robust")), m.character = list(print = c("all", "summary", "model", "cutoff")),
               args = c("res.cor", "specific", "sensitiv", "n", "nrep", "digits", "write2"), package = "lavaan", envir = environment(), input.check = check)

  # Additional checks
  if (isTRUE(check)) {

    # Argument 'level'
    if (isTRUE(!all(level %in% c(0L, 1L, 2L, 3L)))) { stop("Please specify 0, 1, 2, or 3 for the argument 'level'.", call. = FALSE) }

    if (isTRUE(!0L %in% level)) { stop("The value 0 needs to be included in the argument 'level'.", call. = FALSE) }

    # Argument 'estimator'
    if (isTRUE(!is.null(estimator) && !estimator %in% c("ML", "MLM", "MLMV", "MLMVS", "MLF", "MLR", "GLS", "WLS", "DWLS", "WLSM", "WLSMV", "ULS", "ULSM", "ULSMV", "DLS", "PML"))) { stop("Please specify \"ML\", \"MLM\", \"MLMV\", \"MLMVS\", \"MLF\", \"MLR\", \"GLS\", \"WLS\", \"DWLS\", \"WLSM\", \"WLSMV\", \"ULS\", \"ULSM\", \"ULSMV\", \"DLS\", or \"PML\" for the argument 'estimator'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Model Syntax ---------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## misty or lavaan Object ####

  if (isTRUE(inherits(model, what = "misty.object") || inherits(model, what = "lavaan"))) {

    # Factor loadings and residual correlations
    model.syntax <- lavaan::standardizedSolution(if (isTRUE(inherits(model, what = "misty.object"))) { model$model.fit } else { model }) |>
      (\(p) p[which(p$lhs != p$rhs & !p$op %in% c("~1", "|")), c("lhs", "op", "rhs", "est.std")])() |>
      (\(q) lapply(split(q, f = paste(q$op, q$lhs)), function(y) { y |>

          (\(r) data.frame(split = factor(paste(r$op, r$lhs), levels = unique(paste(r$lhs, r$op))), r))() |>
          (\(s) paste(names(split(s, f = (s$split))), paste0(round(y$est.std, digits = 4L), "*", y$rhs, collapse = " + ")) )()

      }))() |> (\(t) paste(unlist(t), collapse = "\n"))()

    # Thresholds
    model.syntax.thres <- lavaan::standardizedSolution(if (isTRUE(inherits(model, what = "misty.object"))) { model$model.fit } else { model }) |>
      (\(p) p[which(p$lhs != p$rhs & p$op == "|"), c("lhs", "op", "rhs", "est.std")])() |>
      (\(q) if (isTRUE(nrow(q) == 0L)) { NULL } else { q })()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## lavaan Model Syntax ####

  } else {

    # Standardized factor loading
    if (isTRUE(lavaan::lavaanify(model) |> (\(p) any(is.na(p[p$op == "=~", "ustart"])))())) { stop("Please specify standardized factor loadings in the lavaan model syntax for the CFA measurement model.", call. = FALSE) }

    # Factor loadings and residual correlations
    model.syntax <- lavaan::lavaanify(model, fixed.x = FALSE) |>
                      (\(p) p[p$lhs != p$rhs & p$op != "~1" & p$op != "|", ])() |>
                      (\(q) lapply(split(q, f = paste(q$op, q$lhs)), function(y) { y |>

                          (\(r) data.frame(split = factor(paste(r$op, r$lhs), levels = unique(paste(r$lhs, r$op))), r))() |>
                          (\(s) paste(names(split(s, f = (s$split))), paste0(y$ustart, "*", y$rhs, collapse = " + ")) )()

                      }))() |> (\(t) paste(unlist(t), collapse = "\n"))()

    # Thresholds
    model.syntax.thres <- lavaan::lavaanify(model, fixed.x = FALSE) |> (\(p) setNames(p[grepl("^t", p$rhs), c("lhs", "op", "rhs", "ustart")], nm = c("lhs", "op", "rhs", "est.std")))()

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Check Model and Model Syntax ####

  if (isTRUE(check)) {

    #——————————————————————————————————————
    ### Multilevel ####

    if (isTRUE(inherits(model, what = "misty.object") || inherits(model, what = "lavaan"))) {

      if (isTRUE(lavaan::inspect(if (isTRUE(inherits(model, what = "misty.object"))) { model$model.fit } else { model }, what = "options")$.multilevel)) {

        stop("This function does not support multilevel models.", call. = FALSE)

      }

    } else {

      if (isTRUE(any(grepl("LEVEL:", toupper(model))))) {

        stop("This function does not support multilevel models.", call. = FALSE)

      }

    }

    #——————————————————————————————————————
    ### Regression Paths ####

    if (isTRUE(lavaan::lavaanify(model.syntax) |> (\(p) all(p$op != "=~") || any(p$op == "~"))())) {

      stop("Please specify a CFA measurement model for the argument 'model'.", call. = FALSE)

    }

    #——————————————————————————————————————
    ### Hierarchical Model ####

    if (isTRUE(lavaan::lavaanify(model.syntax) |> (\(p) any(p[p$op == "=~", "rhs"] %in% p[p$op == "=~", "lhs"]))())) {

      stop("This function does not support hierarhical CFA models.", call. = FALSE)

    }

    #——————————————————————————————————————
    ### Bifactor Model ####

    if (isTRUE(lavaan::lavaanify(model.syntax) |> (\(p) misty::uniq.n(p[p$op == "=~", "lhs"]))() > 1L && lavaan::lavaanify(model.syntax) |> (\(p) p[p$op == "=~", ])() |> (\(q) any(unlist(lapply(split(q, f = q$lhs), function(y) all(lavaan::lavaanify(model.syntax) |> (\(p) unique(p[p$op == "=~", "rhs"]))() %in% y$rhs)))))())) {

      stop("This function does not support bifactor CFA models.", call. = FALSE)

    }
    #——————————————————————————————————————
    ### Degrees of Freedom 0 ####

    if (isTRUE(inherits(model, what = "misty.object") || inherits(model, what = "lavaan"))) {

      if (isTRUE(lavaan::fitmeasures(if (isTRUE(inherits(model, what = "misty.object"))) { model$model.fit } else { model })["df"] <= 0L)) {

        stop("Please specify an overidentified model with degrees of freedom greater than 0 for the argument 'model'.", call. = FALSE)

      }

    } else {

      if (isTRUE(lavaan::fitmeasures(lavaan::cfa(.fixed2free(model.syntax), data = misty::sim.lavaan(model.syntax, std = TRUE)))["df"] <= 0L)) {

        stop("Please specify an overidentified model with degrees of freedom greater than 0 for the argument 'model'.", call. = FALSE)

      }

    }

    #——————————————————————————————————————
    ### Standardized Factor Loadings, Latent or Residual Correlations Greater than 1 ####

    lavaan::lavaanify(model.syntax) |>
      (\(p)

        if (isTRUE(any(abs(p[p$op == "=~", "ustart"]) > 1L))) {

          stop("Please specify a model with standardized factor loadings within [-1, 1] for the argument 'model'.", call. = FALSE)

        } else if (isTRUE(any(abs(p[p$op == "~~" & p$lhs != p$rhs & p$lhs %in% unique(p[p$op == "=~", "lhs"]), "ustart"] ) > 1L))) {

          stop("Please specify a model with latent correlations within [-1, 1] for the argument 'model'.", call. = FALSE)

        } else if (isTRUE(any(abs(p[p$op == "~~" & p$lhs != p$rhs & p$lhs %in% unique(p[p$op == "=~", "rhs"]), "ustart"]) > 1L))) {

          stop("Please specify a model with residual correlations within [-1, 1] for the argument 'model'.", call. = FALSE)

      })()

    #——————————————————————————————————————
    ### Items for Misspecification ####

    # One-Factor CFA Model
    if (isTRUE(.n.factors(model.syntax) == 1L)) {

      if (isTRUE(nrow(.items.no.cor(model.syntax)) < 2L)) { stop("At least two items without any residual correlation are needed to introduce misspecification for the simulation.", call. = FALSE) }

    # Multi-Factor CFA Model
    } else {

      if (isTRUE(nrow(na.omit(.items.n.crossload(model.syntax))) < 1L)) {

        stop("At least one item without any residual correlation and cross-loading is needed to introduce misspecification for the simulation.", call. = FALSE)

      }

    }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data ####

  if (isTRUE(inherits(model, what = "misty.object") || inherits(model, what = "lavaan"))) {

    data <- as.data.frame(lavaan::inspect(if (isTRUE(inherits(model, what = "misty.object"))) { model$model.fit } else { model }, what = "data"))

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Type ####

  # Default setting
  if (isTRUE(all(c("norm", "nnorm", "likert", "categ") %in% type))) {

    #——————————————————————————————————————
    ### Data Available ####

    if (isTRUE(!is.null(data))) {

      # Object misty or lavaan
      if (isTRUE(!is.character(model))) {

        if (isTRUE(all(lavaan::standardizedSolution(if (isTRUE(inherits(model, what = "misty.object"))) { model$model.fit } else { model })$op != "|"))) {

          type <- "nnorm"

        } else {

          type <- "categ"

        }

      # Character string
      } else {

        if (isTRUE(!grepl("|", model, fixed = TRUE))) {

          type <- "nnorm"

        } else {

          type <- "categ"

        }

      }

    #——————————————————————————————————————
    ### Data Not Available ####

    } else {

      # Object misty or lavaan
      if (isTRUE(!is.character(model))) {

        if (isTRUE(all(lavaan::lavaanify(if (isTRUE(inherits(model, what = "misty.object"))) { model$model.fit } else { model })$op != "|"))) {

          type <- "norm"

        } else {

          type <- "categ"

        }

      # Character string
      } else {

        if (isTRUE(!grepl("|", model, fixed = TRUE))) {

          type <- "norm"

        } else {

          type <- "categ"

        }

      }

    }

  }

  #——————————————————————————————————————
  ### Check 'data' ####

  # Object misty or lavaan
  if (isTRUE(inherits(model, what = "misty.object") || inherits(model, what = "lavaan"))) {

    # Categorical data
    if (isTRUE(type == "categ" && !lavaan::inspect(if (isTRUE(inherits(model, what = "misty.object"))) { model$model.fit } else { model }, what = "options")$.categorical)) { stop("Please specify a fitted CFA measurement model based on ordered-categorical indicactors for the argument 'model' when using type = \"categ\".", call. = FALSE) }

    # Likert-type data
    if (isTRUE(type == "likert" && any(data %% 1L != 0L))) { stop("Please specify a fitted CFA measurement model based on integer items for the argument 'model' when using type = \"likert\".", call. = FALSE) }

  # Character string
  } else {

    # Manual specification for "nnorm" or "likert
    if (isTRUE(type %in% c("nnorm", "likert") && is.null(data))) { stop("Please specify the argument 'data' when using type = \"nnorm\" or \"likert\".", call. = FALSE) }

    # Variables in data set
    if (isTRUE(!is.null(data))) { lavaan::lavaanify(model.syntax) |> (\(p) unique(p[which(p$op == "=~"), "rhs"]))() |> (\(q) if (isTRUE(any(!q %in% colnames(data)))) { stop(paste0("Variables not found in the data frame specified in the argument 'data': ",  paste(unique(q[!q %in%colnames(data)]), collapse = ", ")), call. = FALSE) })() }

    # Likert-type data
    if (isTRUE(type %in% c("likert", "categ") && any(data %% 1L != 0L))) { stop("Please specify a data frame with integer variables for the argument 'model' when using type = \"likert\".", call. = FALSE) }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Sample Size ####

  if (isTRUE(is.null(n))) {

    if (isTRUE(inherits(model, what = "misty.object") || inherits(model, what = "lavaan"))) {

      n <- lavaan::inspect(if (isTRUE(inherits(model, what = "misty.object"))) { model$model.fit } else { model }, what = "nobs")

    } else {

      stop("Please specify the argument 'n' when using the lavaan model syntax for the argument 'model'.", call. = FALSE)

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Estimator ####

  if (isTRUE(is.null(estimator))) {

    if (isTRUE(inherits(model, what = "misty.object") || inherits(model, what = "lavaan"))) {

      estimator <- lavaan::inspect(if (isTRUE(inherits(model, what = "misty.object"))) { model$model.fit } else { model }, what = "options")$estimator.orig

    } else {

      stop("Please specify the argument 'estimator' when using the lavaan model syntax for the argument 'model'.", call. = FALSE)

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Standard Error ####

  se <- ifelse(isTRUE(startsWith(estimator, "ULS")), "standard", "none")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Fit Indices ####

  # Default setting
  if (isTRUE(all(c("standard", "scaled", "robust") %in% fit.indices))) {

    # Scaled fit indices
    if (isTRUE(endsWith(estimator, "MVS") || endsWith(estimator, "V"))) {

      sim.fit.indices <- c("cfi.scaled", "tli.scaled", "rmsea.scaled", "srmr")
      fit.indices <- "scaled"

    # Robust fit indices
    } else if (isTRUE(endsWith(estimator, "M") || endsWith(estimator, "R"))) {

      sim.fit.indices <- c("cfi.robust", "tli.robust", "rmsea.robust", "srmr")
      fit.indices <- "robust"

    # Standard fit indices
    } else {

      sim.fit.indices <- c("cfi", "tli", "rmsea", "srmr")
      fit.indices <- "standard"

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Print ####

  if (isTRUE(all(c("all", "summary", "model", "cutoff") %in% print))) {

    print  <- c("summary", "cutoff")

  } else if (isTRUE(length(print) == 1L && "all" %in% print)) {

    print <- c("summary", "model", "cutoff")

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model Specification for the True and Misspecified Models ####

  # One-Factor CFA Model
  if (isTRUE(.n.factors(model.syntax) == 1L)) {

    # Model specification
    sim.model <- .misspec.one(model.syntax = model.syntax, res.cor = res.cor) |> (\(p) setNames(c(model.syntax, p), nm = c("Level 0", paste0("Level ", seq_len(length(p))))))() |> (\(q) q[intersect(names(q), paste0("Level ", level))])()

    # Specify seeds
    if (isTRUE(seed)) { seed <- c(326267L, 649364L) } else { seed <- NULL }

  # Multi-Factor CFA Model
  } else {

    # Model specification
    sim.model <- .misspec.multi(model.syntax = model.syntax) |> (\(p) setNames(c(model.syntax, p), nm = c("Level 0", paste0("Level ", seq_len(length(p))))))() |> (\(q) q[intersect(names(q), paste0("Level ", level))])()

    # Specify seeds
    if (isTRUE(seed)) {

      switch(type,

         "norm" = {

        seed <- c(267326L, 269854L)

      }, "nnorm" = {

        seed <- c(326267L, 649364L)

      }, "likert" = {

        seed <- c(326267L, 269854L)

      }, "categ" = {

        seed <- c(267326L, 269854L)

      })

    } else { seed <- NULL }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Simulation ####

  sim.result <- .sim.fit(model.syntax = model.syntax, model.syntax.thres = model.syntax.thres, sim.model = sim.model, type = type, data = data, n = n, estimator = estimator, se = se, sim.fit.indices = sim.fit.indices, nrep = nrep, seed = seed, progress = progress)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Determine Dynamic Fit Index Cutoffs ####

  #——————————————————————————————————————
  ### Determine Quantiles ####

  fit.quant <- sapply(names(sim.result), function(y)

    # Level 0
    if (isTRUE(y == "Level 0")) {

      list(data.frame(sapply(names(sim.result[[y]]), function(z) if (isTRUE(z %in% c("cfi", "tli"))) { quantile(sim.result[[y]][, z], probs =  1L - specific) } else { quantile(sim.result[[y]][, z], probs = specific) }, simplify = FALSE), row.names = paste0(specific * 100, "%")))

    # Level 1, 2, and 3
    } else {

      list(data.frame(sapply(names(sim.result[[y]]), function(z) if (isTRUE(z %in% c("cfi", "tli"))) { quantile(sim.result[[y]][, z], probs = seq(sensitiv, 0L, -0.01)) } else { quantile(sim.result[[y]][, z], probs = seq(1 - sensitiv, 1L, 0.01)) })))

  })

  #——————————————————————————————————————
  ### Determine Cutoff Values for Each Misspecification Level Except Level 0 ####

  fit.cutoff <- list()
  # Level 1, 2, and/or 3 Misspecification Model
  if (isTRUE(length(fit.quant) > 1L)) {

    for (i in setdiff(names(fit.quant), "Level 0")) {

      # Combine Levels
      fit.cutoff[[i]] <- data.frame(setNames(fit.quant[[i]], nm = c("cfi.m", "tli.m", "rmsea.m", "srmr.m")),
                                    setNames(fit.quant[["Level 0"]], nm = c("cfi.t", "tli.t", "rmsea.t", "srmr.t")), power = seq(sensitiv, 0.0, -0.01))

      # Cutoffs based on misspecified models compared to the true model
      fit.cutoff[[i]]$t <- ifelse(fit.cutoff[[i]]$tli.m <= fit.cutoff[[i]]$tli.t, 1, 0)
      fit.cutoff[[i]]$c <- ifelse(fit.cutoff[[i]]$cfi.m <= fit.cutoff[[i]]$cfi.t, 1, 0)
      fit.cutoff[[i]]$r <- ifelse(fit.cutoff[[i]]$rmsea.m >= fit.cutoff[[i]]$rmsea.t, 1, 0)
      fit.cutoff[[i]]$s <- ifelse(fit.cutoff[[i]]$srmr.m >= fit.cutoff[[i]]$srmr.t, 1, 0)

      # Cutoffs for misspecified models
      fit.cutoff[[i]] <- cbind(setNames(subset(fit.cutoff[[i]], subset = (!duplicated(fit.cutoff[[i]][("c")]) | fit.cutoff[[i]][("power")] == 0), select = c("cfi.m", "power", "c")) |> (\(p) p[p$c == 1 | p$power == 0, c("cfi.m", "power")])(), nm = c("cfi", "power.c"))[1L, ],
                               setNames(subset(fit.cutoff[[i]], subset = (!duplicated(fit.cutoff[[i]][("t")]) | fit.cutoff[[i]][("power")] == 0), select = c("tli.m", "power", "t")) |> (\(p) p[p$t == 1 | p$power == 0, c("tli.m", "power")])(), nm = c("tli", "power.t"))[1L, ],
                               setNames(subset(fit.cutoff[[i]], subset = (!duplicated(fit.cutoff[[i]][("r")]) | fit.cutoff[[i]][("power")] == 0), select = c("rmsea.m", "power", "r")) |> (\(p) p[p$r == 1 | p$power == 0, c("rmsea.m", "power")])(), nm = c("rmsea", "power.r"))[1L, ],
                               setNames(subset(fit.cutoff[[i]], subset = (!duplicated(fit.cutoff[[i]][("s")]) | fit.cutoff[[i]][("power")] == 0), select = c("srmr.m", "power", "s")) |> (\(p) p[p$s == 1 | p$power == 0, c("srmr.m", "power")])(), nm = c("srmr", "power.s"))[1L, ])

    }

  # Level 0 Misspecification Model Only
  } else {

    fit.cutoff <- fit.quant

  }

  #——————————————————————————————————————
  ### Combine Levels ####

  # Level 0 Misspecification Model
  fit.cutoff.result <- data.frame(cfi = fit.quant[["Level 0"]][, "cfi"], power.c = specific, tli = fit.quant[["Level 0"]][, "tli"], power.t = specific, rmsea = fit.quant[["Level 0"]][, "rmsea"], power.r = specific, srmr = fit.quant[["Level 0"]][, "srmr"], power.s = specific, row.names = "Level 0")

  # Level 1, 2, and/or 3 Misspecification Model
  for (i in setdiff(names(fit.quant), "Level 0")) {

    switch(i, "Level 1" = {

      fit.cutoff.result <- rbind(fit.cutoff.result, fit.cutoff[["Level 1"]])

    # Two levels
    }, "Level 2" = {

      fit.cutoff.result <- rbind(fit.cutoff.result, fit.cutoff[["Level 2"]])


    # Three levels
    }, "Level 3" = {

      fit.cutoff.result <- rbind(fit.cutoff.result, fit.cutoff[["Level 3"]])

    })

  }

  # Row names
  rownames(fit.cutoff.result) <- names(fit.quant)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Fit Indices, Number of Observations, and Estimator of the Fitted Model ####

  model.fit <- n.emp <- estimator.emp <- fit.indices.emp <- NULL
  if (isTRUE(inherits(model, what = "misty.object") || inherits(model, what = "lavaan"))) {

    #——————————————————————————————————————
    ### Number of Observations ####

    n.emp <- lavaan::inspect(if (isTRUE(inherits(model, what = "misty.object"))) { model$model.fit } else { model }, what = "nobs")

    if (isTRUE(n.emp != n)) { warning("The number of observations between the simulated and fitted model does not align.", call. = FALSE) }

    #——————————————————————————————————————
    ### Estimator ####

    estimator.emp <- lavaan::inspect(if (isTRUE(inherits(model, what = "misty.object"))) { model$model.fit } else { model }, what = "options")$estimator.orig

    if (isTRUE(estimator.emp != estimator)) { warning("The estimator between the simulated and fitted model does not align.", call. = FALSE) }


    #——————————————————————————————————————
    ### Empirical Fit Indices ####

    # Version of the CFI, TLI, and RMSEA
    lavaan::fitmeasures(if (isTRUE(inherits(model, what = "misty.object"))) { model$model.fit } else { model }) |>
      (\(p) if (isTRUE("robust" %in% fit.indices && !is.na(p["cfi.robust"]))) {

        fit.indices.emp <<- "robust"

      } else if (isTRUE("scaled" %in% fit.indices && !is.na(p["cfi.scaled"]))) {

        fit.indices.emp <<- "scaled"

      } else {

        fit.indices.emp <<- "standard"

      })()

    if (isTRUE(fit.indices.emp != fit.indices)) { warning("The version of the CFI, TLI, and RMSEA between the simulated and fitted model does not align.", call. = FALSE) }

    # Extract fit indices
    model.fit <- lavaan::fitmeasures(if (isTRUE(inherits(model, what = "misty.object"))) { model$model.fit } else { model }) |>
      (\(p) c(if (any(c("scaled", "robust") %in% fit.indices) && "chisq.scaled" %in% p) { p[c("chisq.scaled", "df")] } else { p[c("chisq", "df")] },

                if (isTRUE("robust" %in% fit.indices && !is.na(p["cfi.robust"]))) {

                  p[c("cfi.robust", "tli.robust", "rmsea.robust", "srmr")]

                } else if (isTRUE("scaled" %in% fit.indices && !is.na(p["cfi.scaled"]))) {

                  p[c("cfi.scaled", "tli.scaled", "rmsea.scaled", "srmr")]

                } else {

                  p[c("cfi", "tli", "rmsea", "srmr") ]

                }))() |> (\(q) setNames(q, nm = c("chisq", "df", "cfi", "tli", "rmsea", "srmr")))()

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## lavaan summary ####

  #——————————————————————————————————————
  ### Simulation ####

  lavaan.summary <- data.frame(### First column
                               c(paste("lavaan", packageDescription("lavaan")$Version), "", "Data", "Number of Observations", "Estimator", "Fit Indices", "",
                                 "Number of Factors", "Number of Indicators", "Number of Model Parameters", "",
                                 "Number of Replications"),
                               ### Second column
                               c("", "",
                                 # Data
                                 misty::rec(type, spec = "'norm' = 'Multivariate Normal'; 'nnorm' = 'Multivariate Non-Normal'; 'likert' = 'Likert-Type'; 'categ' = 'Ordered-Categorical'"),
                                 # Number of Observations
                                 n,
                                 # Estimator
                                 estimator,
                                 # Fit Indices
                                 misty::rec(fit.indices, spec = "'standard' = 'Standard'; 'scaled' = 'Scaled'; 'robust' = 'Robust'"), "",
                                 # Number of Factors
                                 .n.factors(model.syntax),
                                 # Number of Indicators
                                 if (isTRUE(.n.factors(model.syntax) == 1L)) {

                                   lavaan::lavaanify(model.syntax) |> (\(p) misty::uniq.n(p[p$op == "=~", "rhs"]))()

                                 } else {

                                   lavaan::lavaanify(model.syntax) |>
                                     (\(p) p[p$op == "=~", ])() |>
                                     (\(q) paste(unlist(rev(lapply(split(q, f = q$lh), function(y) misty::uniq.n(y$rhs)))), collapse = " / "))()

                                 },
                                 # Number of Model Parameters
                                 if (isTRUE(inherits(model, what = "misty.object") || inherits(model, what = "lavaan"))) {

                                  lavaan::fitmeasures(if (isTRUE(inherits(model, what = "misty.object"))) { model$model.fit } else { model }, fit.measures = "npar")

                                 } else {

                                   nrow(lavaan::lavaanify(.fixed2free(model.syntax)))

                                 }, "",
                                 # Number of Replications
                                 nrep), fix.empty.names = FALSE)

  #——————————————————————————————————————
  ### Empirical ####

  lavaan.summary.empirical <- NULL
  if (isTRUE(!is.null(fit.indices.emp))) {

    lavaan.summary.empirical <-  data.frame(### First column
                                            c("Number of Observations", "Estimator", "Fit Indices"),
                                            ### Second column
                                            c(# Number of Observations
                                              n.emp,
                                              # Estimator
                                              estimator.emp,
                                              # Fit Indices
                                              misty::rec(fit.indices.emp, spec = "'standard' = 'Standard'; 'scaled' = 'Scaled'; 'robust' = 'Robust'")), fix.empty.names = FALSE)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Return Object ####

  object <- list(call = match.call(),
                 type = "item.dfi",
                 args = list(n = n, type = type, level = level, res.cor = res.cor, estimator = estimator, fit.indices = fit.indices, specific = specific, sensitiv = sensitiv, nrep = nrep, seed = seed, progress = progress, print = print, digits = digits, plot = plot, filename = filename, width = width, height = height, dpi = dpi, write = write, append = append, check = check, output = output),
                 model = model,
                 data = data,
                 sim.model = unlist(strsplit(model.syntax, "\n")) |> (\(p) paste0(c(p[rev(grep("=~", p))], p[rev(grep("=~", p, invert = TRUE))]), collapse = "\n"))() |>
                                (\(q) lapply(sim.model, function(z) unlist(strsplit(z, "\n")) |> (\(r) paste0(c(q, r[!r %in% unlist(strsplit(q, "\n"))]), collapse = "\n"))()))(),
                 plot = NULL,
                 result = list(summary = lavaan.summary, summary.empirical = lavaan.summary.empirical, fit.sim = sim.result, fit.quant = fit.quant, fit.cutoff = fit.cutoff.result, fit.emp = model.fit))

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
