#' Print Mplus Output
#'
#' This function prints the input command sections and the result sections of a
#' Mplus output file (\code{.out}) on the R console. By default, the function
#' prints selected result sections, e.g., short \code{Summary of Analysis}, short
#' \code{Summary of Data}, \code{Model Fit Information}, and \code{Model Results}.
#'
#' @param x          a character string indicating the name of the Mplus output
#'                   file with or without the file extension \code{.out}, e.g.,
#'                   \code{"Mplus_Output.out"} or \code{"Mplus_Output"}.
#'                   Alternatively, a \code{misty.object} of type \code{mplus}
#'                   can be specified, i.e., result object of the \code{mplus.print()},
#'                   \code{mplus()} or \code{mplus.update()} function.
#' @param print      a character vector indicating which section to show, i.e.
#'                   \code{"all"} for input and result sections, \code{"input"}
#'                   for input command section only, and \code{"result"} (default)
#'                   for result sections only.
#' @param input      a character vector specifying Mplus input command sections
#                    included in the output (see 'Details').
#' @param result     a character vector specifying Mplus result sections included
#'                   in the output (see 'Details').
#' @param exclude    a character vector specifying Mplus input command or result
#'                   sections excluded from the output (see 'Details').
#' @param variable   logical: if \code{TRUE}, names of the variables in the data
#'                   set (\code{NAMES} option) specified in the \code{VARIABLE:}
#'                   command section are shown. By default, names of the variables
#'                   in the data set are excluded from the output unless all variables
#'                   are used in the analysis (i.e., no \code{USEVARIABLES} option
#'                   specified in the Mplus input file).
#' @param not.input  logical: if \code{TRUE} (default), character vector indicating
#'                   the input commands not requested are shown on the console.
#' @param not.result logical: if \code{TRUE} (default), character vector indicating
#'                   the result sections not requested are shown on the console.
#' @param write      a character string naming a file for writing the output into
#'                   a text file with file extension \code{".txt"} (e.g.,
#'                   \code{"Output.txt"}).
#' @param append     logical: if \code{TRUE} (default), output will be appended
#'                   to an existing text file with extension \code{.txt} specified
#'                   in \code{write}, if \code{FALSE} existing text file will be
#'                   overwritten.
#' @param check      logical: if \code{TRUE} (default), argument specification is
#'                   checked.
#' @param output     logical: if \code{TRUE} (default), output is shown on the
#'                   console.
#'
#' @details
#' \describe{
#' \item{\strong{Input Command Sections}}{Following input command sections can be
#' selected by using the \code{input} argument or excluded by using the \code{exclude}
#' argument:
#'    \itemize{
#'       \item{\code{"title"}} for the \code{TITLE} command used to provide a title
#'       for the analysis.
#'       \item{\code{"data"}} for the \code{DATA} command used to provide information
#'       about the data set to be analyzed.
#'       \item{\code{"data.imp"}} for the \code{DATA IMPUTATION} command used to
#'       create a set of imputed data sets using multiple imputation methodology.
#'       \item{\code{"data.wl"}} for the \code{DATA WIDETOLONG} command used to
#'       rearrange data from a multivariate wide format to a univariate long format.
#'       \item{\code{"data.lw"}} for the \code{DATA LONGTOWIDE} command used to
#'       rearrange a univariate long format to a multivariate wide format.
#'       \item{\code{"data.tp"}} for the \code{DATA TWOPART} command used to create
#'       a binary and a continuous variable from a continuous variable with a floor
#'       effect for use in two-part modeling.
#'       \item{\code{"data.miss"}} for the \code{DATA MISSING} command used to
#'       create a set of binary variables that are indicators of missing data or
#'       dropout for another set of variables.
#'       \item{\code{"data.surv"}} for the \code{DATA SURVIVAL} command used to
#'       create variables for discrete-time survival modeling.
#'       \item{\code{"data.coh"}} for the \code{DATA COHORT} command used to
#'       rearrange longitudinal data from a format where time points represent
#'       measurement occasions to a format where time points represent age or
#'       another time-related variable,
#'       \item{\code{"variable"}} for the \code{VARIABLE} command used to provide
#'       information about the variables in the data set to be analyzed.
#'       \item{\code{"define"}} for the \code{DEFINE} command used to transform
#'       existing variables and to create new variables.
#'       \item{\code{"analysis"}} for the \code{ANALYSIS} command used to describe
#'       the technical details for the analysis.
#'       \item{\code{"model"}} \code{MODEL} for the  command used to describe the
#'       model to be estimated.
#'       \item{\code{"mod.ind"}} for the \code{MODEL INDIRECT} command used to
#'       request indirect and directed effects and their standard errors.
#'       \item{\code{"mod.test"}} for the \code{MODEL TEST} command used to
#'       test restrictions on the parameters in the \code{MODEL} and \code{MODEL CONSTRAINT}
#'       commands using the Wald chi-square test.
#'       \item{\code{"mod.prior"}} for the \code{MODEL PRIORS} command used with
#'      \code{ESTIMATOR IS BAYES} to specify the prior distribution for each
#'       parameter.
#'       \item{\code{"montecarlo"}} for the \code{MONTECARLO} command used to set
#'       up and carry out a Monte Carlo simulation study.
#'       \item{\code{"mod.pop"}} for the \code{MODEL POPULATION} command used
#'       to provide the population parameter values to be used in data generation
#'       using the options of the \code{MODEL} command.
#'       \item{\code{"mod.cov"}} for the \code{MODEL COVERAGE} used to provide
#'       the population parameter values to be used for computing coverage.
#'       \item{\code{"mod.miss"}}  for the \code{MODEL MISSING} command used to
#'       provide information about the population parameter values for the missing
#'       data model to be used in the generation of data.
#'       \item{\code{"output"}} for the  for the \code{OUTPUT} command used to
#'       request additional output beyond that included as the default.
#'       \item{\code{"savedata"}} for the \code{SAVEDATA} command used to save
#'       the analysis data and/or a variety of model results in an ASCII file for
#'       future use.
#'       \item{\code{"plot"}} for the \code{PLOT} command used to requested graphical
#'       displays of observed data and analysis results.
#'       \item{\code{"message"}} for warning and error messages that have been
#'       generated by the program after the input command sections.
#'    }
#' Note that all input command sections are requested by specifying \code{input = "all"}.
#' The \code{input} argument is also used to select one (e.g., \code{input = "model"})
#' or more than one input command sections (e.g., \code{input = c("analysis", "model")}),
#' or to request input command sections in addition to the default setting (e.g.,
#' \code{input = c("default", "output")}). The \code{exclude} argument is used
#' to exclude input command sections from the output (e.g., exclude = "variable").
#' }
#' \item{\strong{Result Sections}}{Following result sections can be selected by
#' using the \code{result} argument or excluded by using the \code{exclude}
#' argument:
#'    \itemize{
#'       \item{\code{"summary.analysis"}} for the \code{SUMMARY OF ANALYSIS} section..
#'       \item{\code{"summary.analysis.short"}} for a short \code{SUMMARY OF ANALYSIS} section including the number of observations, number of groups, estimator, and optimization algorithm.
#'       \item{\code{"summary.data"}} for the \code{SUMMARY OF DATA} section indicating.
#'       \item{\code{"summary.data.short"}} for a short \code{SUMMARY OF DATA} section including number of clusters, average cluster size, and estimated intraclass correlations.
#'       \item{\code{"prop.count"}} for the \code{UNIVARIATE PROPORTIONS AND COUNTS FOR CATEGORICAL VARIABLES} section.
#'       \item{\code{"summary.censor"}} for the \code{SUMMARY OF CENSORED LIMITS} section.
#'       \item{\code{"prop.zero"}} for the \code{COUNT PROPORTION OF ZERO, MINIMUM AND MAXIMUM VALUES} section.
#'       \item{\code{"crosstab"}} for the \code{CROSSTABS FOR CATEGORICAL VARIABLES} section.
#'       \item{\code{"summary.miss"}} for the \code{SUMMARY OF MISSING DATA PATTERNS} section.
#'       \item{\code{"coverage"}} for the \code{COVARIANCE COVERAGE OF DATA} section.
#'       \item{\code{"basic"}} for the \code{RESULTS FOR BASIC ANALYSIS} section.
#'       \item{\code{"sample.stat"}} for the \code{SAMPLE STATISTICS} section.
#'       \item{\code{"uni.sample.stat"}} for the \code{UNIVARIATE SAMPLE STATISTICS} section.
#'       \item{\code{"random.starts"}} for the \code{RANDOM STARTS RESULTS} section.
#'       \item{\code{"summary.fit"}} for the \code{SUMMARY OF MODEL FIT INFORMATION} section.
#'       \item{\code{"mod.est"}} for the \code{THE MODEL ESTIMATION TERMINATED NORMALLY} message and warning messages from the model estimation.
#'       \item{\code{"fit"}} for the \code{MODEL FIT INFORMATION} section.
#'       \item{\code{"class.count"}} for the \code{FINAL CLASS COUNTS AND PROPORTIONS FOR THE LATENT CLASSES} section.
#'       \item{\code{"ind.means"}} for the \code{LATENT CLASS INDICATOR MEANS AND PROBABILITIES} section.
#'       \item{\code{"trans.prob"}} for the \code{LATENT TRANSITION PROBABILITIES BASED ON THE ESTIMATED MODEL} section.
#'       \item{\code{"classif"}} for the \code{CLASSIFICATION QUALITY} section.
#'       \item{\code{"mod.result"}} for the \code{MODEL RESULTS} and \code{RESULTS FOR EXPLORATORY FACTOR ANALYSIS} section.
#'       \item{\code{"odds.ratio"}} for the \code{LOGISTIC REGRESSION ODDS RATIO RESULTS} section.
#'       \item{\code{"prob.scale"}} for the \code{RESULTS IN PROBABILITY SCALE} section.
#'       \item{\code{"ind.odds.ratio"}} for the \code{LATENT CLASS INDICATOR ODDS RATIOS FOR THE LATENT CLASSES} section.
#'       \item{\code{"alt.param"}} for the \code{ALTERNATIVE PARAMETERIZATIONS FOR THE CATEGORICAL LATENT VARIABLE REGRESSION} section.
#'       \item{\code{"irt.param"}} for the \code{IRT PARAMETERIZATION} section.
#'       \item{\code{"brant.wald"}} for the \code{BRANT WALD TEST FOR PROPORTIONAL ODDS} section.
#'       \item{\code{"std.mod.result"}} for the \code{STANDARDIZED MODEL RESULTS} section.
#'       \item{\code{"rsquare"}} for the \code{R-SQUARE} section.
#'       \item{\code{"total.indirect"}} for the \code{TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT, AND DIRECT EFFECTS} section.
#'       \item{\code{"std.total.indirect"}} for the \code{STANDARDIZED TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT, AND DIRECT EFFECTS} section.
#'       \item{\code{"std.mod.result.cluster"}} for the \code{WITHIN-LEVEL STANDARDIZED MODEL RESULTS FOR CLUSTER} section.
#'       \item{\code{"fs.comparison"}} for the \code{BETWEEN-LEVEL FACTOR SCORE COMPARISONS} section.
#'       \item{\code{"conf.mod.result"}} for the \code{CONFIDENCE INTERVALS OF MODEL RESULTS} section.
#'       \item{\code{"conf.std.conf"}} for the \code{CONFIDENCE INTERVALS OF STANDARDIZED MODEL RESULTS} section.
#'       \item{\code{"conf.total.indirect"}} for the \code{CONFIDENCE INTERVALS OF TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT, AND DIRECT EFFECTS} section.
#'       \item{\code{"conf.odds.ratio"}} for the \code{CONFIDENCE INTERVALS FOR THE LOGISTIC REGRESSION ODDS RATIO RESULTS} section.
#'       \item{\code{"modind"}} for the \code{MODEL MODIFICATION INDICES} section.
#'       \item{\code{"resid"}} for the \code{RESIDUAL OUTPUT} section.
#'       \item{\code{"logrank"}} for the \code{LOGRANK OUTPUT} section.
#'       \item{\code{"tech1"}} for the \code{TECHNICAL 1 OUTPUT} section.
#'       \item{\code{"tech2"}} for the \code{TECHNICAL 2 OUTPUT} section.
#'       \item{\code{"tech3"}} for the \code{TECHNICAL 3 OUTPUT} section.
#'       \item{\code{"h1.tech3"}} for the \code{H1 TECHNICAL 3 OUTPUT} section.
#'       \item{\code{"tech4"}} for the \code{TECHNICAL 4 OUTPUT} section.
#'       \item{\code{"tech5"}} for the \code{TECHNICAL 5 OUTPUT} section.
#'       \item{\code{"tech6"}} for the \code{TECHNICAL 6 OUTPUT} section.
#'       \item{\code{"tech7"}} for the \code{TECHNICAL 7 OUTPUT} section.
#'       \item{\code{"tech8"}} for the \code{TECHNICAL 8 OUTPUT} section.
#'       \item{\code{"tech9"}} for the \code{TECHNICAL 9 OUTPUT} section.
#'       \item{\code{"tech10"}} for the \code{TECHNICAL 10 OUTPUT} section.
#'       \item{\code{"tech11"}} for the \code{TECHNICAL 11 OUTPUT} section.
#'       \item{\code{"tech12"}} for the \code{TECHNICAL 12 OUTPUT} section.
#'       \item{\code{"tech13"}} for the \code{TECHNICAL 13 OUTPUT} section.
#'       \item{\code{"tech14"}} for the \code{TECHNICAL 14 OUTPUT} section.
#'       \item{\code{"tech15"}} for the \code{TECHNICAL 15 OUTPUT} section.
#'       \item{\code{"tech16"}} for the \code{TECHNICAL 16 OUTPUT} section.
#'       \item{\code{"svalues"}} for the \code{MODEL COMMAND WITH FINAL ESTIMATES USED AS STARTING VALUES} section.
#'       \item{\code{"stat.fscores"}} for the \code{SAMPLE STATISTICS FOR ESTIMATED FACTOR SCORES} section.
#'       \item{\code{"summary.fscores"}} for the \code{SUMMARY OF FACTOR SCORES} section.
#'       \item{\code{"pv"}} for the \code{SUMMARIES OF PLAUSIBLE VALUES} section.
#'       \item{\code{"plotinfo"}} for the \code{PLOT INFORMATION} section.
#'       \item{\code{"saveinfo"}} for the \code{SAVEDATA INFORMATION} section.
#'    }
#' Note that all result sections are requested by specifying \code{result = "all"}.
#' The \code{result} argument is also used to select one (e.g., \code{result = "mod.result"})
#' or more than one result sections (e.g., \code{result = c("mod.result", "std.mod.result")}),
#' or to request result sections in addition to the default setting (e.g.,
#' \code{result = c("default", "odds.ratio")}). The \code{exclude} argument is used
#' to exclude result sections from the output (e.g., \code{exclude = "mod.result"}).
#' }
#' }
#' @author
#' Takuya Yanagida
#'
#' @seealso
#' \code{\link{read.mplus}}, \code{\link{write.mplus}}, \code{\link{mplus}},
#' \code{\link{mplus.update}}, \code{\link{mplus.plot}}, \code{\link{mplus.bayes}},
#' \code{\link{mplus.run}}, \code{\link{mplus.lca}}
#'
#' @references
#' Muthen, L. K., & Muthen, B. O. (1998-2017). \emph{Mplus User's Guide} (8th ed.).
#' Muthen & Muthen.
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#'
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{x}}{character string or misty object}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{print}}{print objects}
#' \item{\code{notprint}}{character vectors indicating the input commands and
#'             result sections not requested}
#' \item{\code{result}}{list with input command sections (\code{input}) and result
#'                      sections (\code{input})}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #----------------------------------------------------------------------------
#' # Mplus Example 3.1: Linear Regression
#'
#' # Example 1a: Default setting
#' mplus.print("ex3.1.out")
#'
#' # Example 1b:  Print result section only
#' mplus.print("ex3.1.out", print = "result")
#'
#' # Example 1c: Print MODEL RESULTS only
#' mplus.print("ex3.1.out", print = "result", result = "mod.result")
#'
#' # Example 1d: Print UNIVARIATE SAMPLE STATISTICS in addition to the default setting
#' mplus.print("ex3.1.out", result = c("default", "uni.sample.stat"))
#'
#' # Example 1e: Exclude MODEL RESULTS section
#' mplus.print("ex3.1.out", exclude = "mod.result")
#'
#' # Example 1f: Print all result sections, but exclude MODEL FIT INFORMATION section
#' mplus.print("ex3.1.out", result  = "all", exclude = "fit")
#'
#' Example 1g: Print result section in a different order
#' mplus.print("ex3.1.out", result  = c("mod.result", "fit", "summary.analysis"))
#'
#' #----------------------------------------------------------------------------
#' # misty.object of type 'mplus.print'
#'
#' # Example 2
#' # Create misty.object
#' object <- mplus.print("ex3.1.out", output = FALSE)
#'
#' # Print misty.object
#' mplus.print(object)
#'
#' #----------------------------------------------------------------------------
#' # Write Results
#'
#' # Example 3: Write Results into a text file
#' mplus.print("ex3.1.out", write = "Output_3-1.txt")
#' }
mplus.print <- function(x, print = c("all", "input", "result"),
                        input = c("all", "default", "data", "variable", "define", "analysis", "model",
                                  "montecarlo", "mod.pop", "mod.cov", "mod.miss", "message"),
                        result = c("all", "default", "summary.analysis.short", "summary.data.short",
                                   "random.starts", "summary.fit", "mod.est", "fit", "class.count",
                                   "classif", "mod.result", "total.indirect"),
                        exclude = NULL, variable = FALSE, not.input = TRUE, not.result = TRUE,
                        write = NULL, append = TRUE, check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'x' is missing or NULL
  if (isTRUE(missing(x) ||is.null(x))) { stop("Please specify a 'mplus' object or a character string indicating the name of a Mplus output file for the argument 'x'", call. = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Character string ####

  if (isTRUE(is.character(x))) {

    # Character string
    if (isTRUE(length(x) != 1L)) { stop("Please specify a character string indicating the name of a Mplus output file for the argument 'x'", call. = FALSE) }

    # File extension .out
    x <- ifelse(isTRUE(!grepl(".out", x)), file <- paste0(x, ".out"), x)

    # Check if 'x' exists
    if (isTRUE(!file.exists(x))) { stop(paste0("Unable to read the Mplus output file: ", sQuote(x), " does not exist."), call. = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## misty object ####

  } else if (isTRUE(inherits(x, "misty.object"))) {

    if (isTRUE(x$type != "mplus")) { stop("Please specify a \"mplus\" object for the argument 'x'.", call. = FALSE) }

  } else {

    stop("Please specify a \"mplus\" object or a character string indicating the name of a Mplus output file for the argument 'x'", call. = FALSE)

  }

  #_____________________________________________________________________________
  #
  # Input and Result Arguments -------------------------------------------------

  # All input options
  input.all <- c("title", "data", "data.imp", "data.wl", "data.lw", "data.tp", "data.miss", "data.surv", "data.coh", "variable", "define", "analysis", "model", "mod.ind", "mod.test", "mod.prior", "montecarlo", "mod.pop", "mod.cov", "mod.miss", "inpoutput", "savedata", "plot", "output", "message")

  # All result options
  result.all <- c("summary.analysis", "summary.analysis.short", "summary.data", "summary.data.short", "prop.count", "summary.censor", "prop.zero", "crosstab", "summary.miss", "coverage", "basic", "sample.stat", "uni.sample.stat", "random.starts", "summary.fit", "mod.est", "fit", "class.count", "ind.means", "trans.prob", "classif", "mod.result", "odds.ratio", "prob.scale", "ind.odds.ratio", "alt.param", "irt.param", "brant.wald", "std.mod.result", "rsquare", "total.indirect", "std.total.indirect", "std.mod.result.cluster", "fs.comparison", "conf.mod.result", "conf.std.conf", "conf.total.indirect", "conf.odds.ratio", "modind", "resid", "logrank", "tech1", "tech2", "tech3", "h1.tech3", "tech4", "tech5", "tech6", "tech7", "tech8", "tech9", "tech10", "tech11", "tech12", "tech13", "tech14", "tech15", "tech16", "svalues", "stat.fscores", "summary.fscores", "pv", "plotinfo", "saveinfo")

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("variable", "not.input", "not.result", "append", "output"),
               s.character = list(print = c("all", "input", "result")),
               args = "write1", envir = environment(), input.check = check)

  if (isTRUE(check)) {

    # Check input 'input'
    input[which(!input %in% c("all", "default", input.all))] |>
      (\(y) if (isTRUE(length(y) != 0L)) { stop(paste0(if (isTRUE(length(y) == 1L)) { "Character string " } else { "Character vector " }, "specified in the argument 'input' is not permissible: ", paste(dQuote(y), collapse = ", ")), call. = FALSE) })()

    # Check input 'result'
    result.check <- result[which(!result %in% c("all", "default", result.all))] |>
      (\(y) if (isTRUE(length(y) != 0L)) { stop(paste0(if (isTRUE(length(y) == 1L)) { "Character string " } else { "Character vector " }, "specified in the argument 'result' is not permissible: ", paste(dQuote(y), collapse = ", ")), call. = FALSE) })()

    # Check input 'exclude'
    exclude.check <- exclude[which(!exclude %in% c(input.all, result.all))] |>
      (\(y) if (isTRUE(length(y) != 0L)) { stop(paste0(if (isTRUE(length(y) == 1L)) { "Character string " } else { "Character vector " }, "specified in the argument 'exclude' is not permissible: ", paste(dQuote(y), collapse = ", ")), call. = FALSE) })()

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'print' Argument ####

  # Default
  if (isTRUE(all(c("all", "input", "result") %in% print))) {

    print <- "result"

  # All, input and results
  } else if (isTRUE(any(print == "all"))) {

    print <- c("input", "result")

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'input' Argument ####

  # Default setting
  if (isTRUE(all(c("all", "default", "data", "variable", "define", "analysis", "model", "montecarlo", "mod.pop", "mod.cov", "mod.miss", "message") %in% input))) {

    input <- misty::chr.omit(input, omit = c("all", "default"), check = FALSE)

  # All input commands
  } else if (isTRUE("all" %in% input)) {

    input <- input.all

  # Default setting with additional input commands
  } else if (isTRUE("default" %in% input && length(input > 1L))) {

    input <- input.all[input.all %in% misty::chr.omit(union(c("data", "variable", "define", "analysis", "model", "montecarlo", "mod.pop", "mod.cov", "mod.miss", "message"), input), "default", check = FALSE)]

  # Manual default setting
  } else if (isTRUE(all(input == "default"))) {

    input <- c("data", "variable", "define", "analysis", "model", "montecarlo", "mod.pop", "mod.cov", "mod.miss", "message")

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'result' Argument ####

  result.default <- FALSE

  # Default setting
  if (isTRUE(all(c("all", "default", "summary.analysis.short", "summary.data.short", "random.starts", "summary.fit", "mod.est", "fit", "class.count", "classif", "mod.result", "total.indirect") %in% result))) {

    result <- result[!result %in% c("all", "default")]

    result.default <- TRUE

  # All result sections
  } else if (isTRUE("all" %in% result)) {

    result <- result.all

  # Default setting with additional result sections
  } else if (isTRUE("default" %in% result & length(result > 1L))) {

    result <- result.all[result.all %in% misty::chr.omit(union(c("summary.analysis.short", "summary.data.short", "random.starts", "summary.fit", "mod.est", "fit", "class.count", "classif", "mod.result", "total.indirect"), result), "default", check = FALSE)]

  # Manual default setting
  } else if (isTRUE("default" %in% result & length(result == 1L))) {

    result <- c("summary.analysis.short", "summary.data.short", "random.starts", "summary.fit", "mod.est", "fit", "class.count", "classif", "mod.result", "total.indirect")

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'exclude' Argument ####

  if (isTRUE(!is.null(exclude))) {

    # Exclude input commands
    input <- setdiff(input, exclude)

    # Exclude result sections
    result <- setdiff(result, exclude)

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #----------------------------------------
  # Mplus Output in Text File

  if (isTRUE(!inherits(x, "misty.object"))) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Read Output ####

    out <- suppressWarnings(readLines(x))

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Check if file is a Mplus output ####

    if (isTRUE(all(!misty::chr.grepl(c("Mplus VERSION", "Mplus DEVELOPMENT"), out)))) { stop("Output file specified in the argument 'x' is not a Mplus output file.", call. = FALSE) }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Default Setting for the Result Argument ####

    # Do not print model fit information when df = 0
    if (isTRUE(result.default && any(out == "Chi-Square Test of Model Fit"))) {

      if (isTRUE(as.numeric(misty::chr.trim(sub("Degrees of Freedom", "", out[which(out == "Chi-Square Test of Model Fit") + 3L]))) == 0L)) {

        result <- misty::chr.omit(result, omit = "fit", check = FALSE)

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Exclude Output ####

    #...................
    ### Variable Names ####

    if (isTRUE(!variable && any(misty::chr.grepl(c("USEVARIABLES", "USEVAR", "USEV"), toupper(out))))) {

      if (isTRUE(any(grepl("VARIABLE:", toupper(out))))) {

        variable <- min(grep("VARIABLE:", toupper(out)))

        semicol <- variable:grep(";", out)[which(grep(";", out) >= variable)][1L]

        if (isTRUE(length(semicol) > 1L)) { out <- out[-semicol[-1L]] }

        out <- sub(out[variable], "  VARIABLE:", out)

        # Remove empty rows
        if (isTRUE(misty::chr.trim(out[grep("VARIABLE:", out) + 1L]) == "")) { out <- out[-(grep("VARIABLE:", out) + 1L)] }

        # Remove empty VARIABLE command
        if (isTRUE(grepl(":", out[(grep("VARIABLE:", out) + 1L)]))) { out <- out[-grep("VARIABLE:", out)] }

        # Remove empty VARIABLE command
        if (isTRUE(check)) {

          out <- sub("VARIABLE:", "VARIABLE:    NAMES ARE ...;\n", out)

        } else {

          out <- sub("VARIABLE:", "VARIABLE: NAMES ARE ...;\n", out)

        }

      }

    }

    #...................
    ### Chi-Square Test of Model Fit for the Baseline Model ####

    grep("Chi-Square Test of Model Fit for the Baseline Model", out) |>
      (\(y) if (isTRUE(length(y) >= 1L)) { assign("out", out[-as.vector(sapply(y, function(y) y:(y + 4L)))]) })()

    #...................
    ### Sample-Size Adjusted BIC ####

    grep("\\(n \\+ 2\\) / 24", out) |>
      (\(y) if (isTRUE(length(y) >= 1L)) { assign("out", out[-y]) })()

    #...................
    ### The chi-square value for MLM, MLMV, MLR, ULSMV, WLSM and WLSMV ####

    chisquare <- grep("*   The chi-square value for MLM, MLMV, MLR, ULSMV, WLSM and WLSMV cannot be used", out)
    if (isTRUE(length(chisquare) >= 1L)) {

      out <- out[-as.vector(sapply(chisquare, function(z) z:(z + 3L)))]

      out[grep("Chi-Square Test of Model Fit", out) + 2L] <- sub("\\*", "", out[grep("Chi-Square Test of Model Fit", out) + 2L])

    }

    #...................
    ### for MLR ####

    if (isTRUE(any(grep("            for MLR", out)))) { out <- out[-grep("            for MLR", out)] }

    #...................
    ### MODEL FIT INFORMATION Proportions, Percentiles and Cumulative Distribution Function ####

    if (isTRUE(any(grep("Number of successful computations", out)))) { out <- out[-c(sapply(c(grep("Percentiles", out), grep("Cumulative Distribution Function", out)), function(z) z:(which(out == "")[which(out == "") > z])[1L]))] }

    #...................
    ### QUALITY OF NUMERICAL RESULTS ####

    grep("QUALITY OF NUMERICAL RESULTS", out) |>
      (\(y) if (isTRUE(length(y) != 0L)) { assign("out", out[-as.vector(sapply(y, function(y) y:(y + 3L)))]) })()

    #...................
    ### MAXIMUM LOG-LIKELIHOOD VALUE FOR THE UNRESTRICTED ####

    grep("MAXIMUM LOG-LIKELIHOOD VALUE FOR THE UNRESTRICTED", out) |>
      (\(y) if (isTRUE(length(y) != 0L)) { assign("out", out[-y]) })()

    #...................
    ### Minimum Rotation Function Value ####

    grep("MINIMUM ROTATION FUNCTION VALUE", out) |>
      (\(y) if (isTRUE(length(y) != 0L)) { assign("out", out[-y]) })()

    #...................
    ### Optimum Function Value ####

    grep("Optimum Function Value", out) |>
      (\(y) if (isTRUE(length(y) != 0L)) { assign("out", out[-c(y:(y + 3L))]) })()

    #...................
    ### DIAGRAM INFORMATION ####

    grep("DIAGRAM INFORMATION", out) |>
      (\(y) if (isTRUE(length(y) != 0L)) { assign("out", out[-c(y:(y + 6L))]) })()

    #...................
    ### Mplus Information ####

    out <- unlist(out)[-c(3L, unlist(sapply(c("MUTHEN & MUTHEN", "Beginning Time:", "Ending Time:", "Elapsed Time:", "3463 Stoner Ave.", "Los Angeles, CA  90066", "Tel: (310) 391-9971", "Fax: (310) 391-8971", "Web: www.StatModel.com", "Support: Support@StatModel.com", "Copyright (c)"), function(z) grep(z, unlist(out), fixed = TRUE))))]

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Run Length Encoding, Sections, Warnings, and Internal Function ####

    #...................
    ### Run Length Encoding ####

    # Values of runs of equal values
    run.val <- rle(out)$value

    # TECHNICAL excluded
    if (isTRUE(any(grepl("TECHNICAL", run.val)))) { run.val.tech <- run.val[-(min(grep("TECHNICAL", run.val)):length(run.val))] } else { run.val.tech <- run.val }

    # Extract input instruction in upper case characters
    run.val.upp <- toupper(run.val[1L:(min(misty::chr.grep(c("INPUT READING TERMINATED NORMALLY", "*** WARNING", "*** ERROR", "*** FATAL", "DEMO VERSION MAXIMUM EXCEEDED"), run.val, fixed = TRUE)) - 1L)])

    # Run length 2
    run.length2 <- which(rle(out)$lengths >= 2L)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Extract Input Commands ####

    if (isTRUE(any(run.val == "INPUT INSTRUCTIONS"))) {

      # Input section
      input.section <- c("TITLE:", "DATA:", "DATA IMPUTATION:", "DATA WIDETOLONG:", "DATA LONGTOWIDE:", "DATA TWOPART:", "DATA MISSING:", "DATA SURVIVAL:", "DATA COHORT:", "VARIABLE:", "DEFINE:", "ANALYSIS:", "MODEL:", "MODEL INDIRECT:", "MODEL TEST:", "MODEL PRIORS:", "MONTECARLO:", "MODEL POPULATION:", "MODEL COVERAGE:", "MODEL MISSING:", "OUTPUT:", "SAVEDATA:", "PLOT:", "INPUT READING TERMINATED NORMALLY", " WARNING", " ERROR", " FATAL", "DEMO VERSION MAXIMUM EXCEEDED")

      # Input objects
      version <- title <- data <- data.imp <- data.wl <- data.lw <- data.tp <- data.miss <- data.surv <- data.coh <- inpvariable <- define <- analysis <- model <- mod.ind <- mod.test <- mod.prior <- montecarlo <-  mod.pop <- mod.cov <- mod.miss <- inpoutput <- savedata <- plot <- message <- NULL

      #...................
      ### Mplus Version ####

      mplus <- run.val[1L]

      #...................
      ### TITLE ####

      if (isTRUE(any(grepl("TITLE:", run.val.upp)))) { title <- min(grep("TITLE:", run.val.upp)[1L]):(.section.ind.from.to(input.section, "TITLE:", run = run.val.upp, input = TRUE)) }

      #...................
      ### DATA ####

      if (isTRUE(any(grepl("DATA:", run.val.upp)))) { data <- min(grep("DATA:", run.val.upp)):(.section.ind.from.to(input.section, "DATA:", run = run.val.upp, input = TRUE)) }

      #...................
      ### DATA IMPUTATION ####

      if (isTRUE(any(grepl("DATA IMPUTATION:", run.val.upp)))) { data.imp <- min(grep("DATA IMPUTATION:", run.val.upp)):(.section.ind.from.to(input.section, "DATA IMPUTATION:", run = run.val.upp, input = TRUE)) }

      #...................
      ### DATA WIDETOLONG ####

      if (isTRUE(any(grepl("DATA WIDETOLONG:", run.val.upp)))) { data.wl <- min(grep("DATA WIDETOLONG:", run.val.upp)):(.section.ind.from.to(input.section, "DATA WIDETOLONG:", run = run.val.upp, input = TRUE)) }

      #...................
      ### DATA LONGTOWIDE ####

      if (isTRUE(any(grepl("DATA LONGTOWIDE:", run.val.upp)))) { data.lw <- min(grep("DATA LONGTOWIDE:", run.val.upp)):(.section.ind.from.to(input.section, "DATA LONGTOWIDE:", run = run.val.upp, input = TRUE)) }

      #...................
      ### DATA TWOPART ####

      if (isTRUE(any(grepl("DATA TWOPART:", run.val.upp)))) { data.tp <- min(grep("DATA TWOPART:", run.val.upp)):(.section.ind.from.to(input.section, "DATA TWOPART:", run = run.val.upp, input = TRUE)) }

      #...................
      ### DATA MISSING ####

      if (isTRUE(any(grepl("DATA MISSING:", run.val.upp)))) { data.miss <- min(grep("DATA MISSING:", run.val.upp)):(.section.ind.from.to(input.section, "DATA MISSING:", run = run.val.upp, input = TRUE)) }

      #...................
      ### DATA SURVIVAL ####

      if (isTRUE(any(grepl("DATA SURVIVAL:", run.val.upp)))) { data.surv <- min(grep("DATA SURVIVAL:", run.val.upp)):(.section.ind.from.to(input.section, "DATA SURVIVAL:", run = run.val.upp, input = TRUE)) }

      #...................
      ### DATA COHORT ####

      if (isTRUE(any(grepl("DATA COHORT:", run.val.upp)))) { data.coh <- min(grep("DATA COHORT:", run.val.upp)):(.section.ind.from.to(input.section, "DATA COHORT:", run = run.val.upp, input = TRUE)) }

      #...................
      ### VARIABLE ####

      if (isTRUE(any(grepl("VARIABLE:", run.val.upp)))) { inpvariable <- min(grep("VARIABLE:", run.val.upp)):(.section.ind.from.to(input.section, "VARIABLE:", run = run.val.upp, input = TRUE)) }

      #...................
      ### DEFINE ####

      if (isTRUE(any(grepl("DEFINE:", run.val.upp)))) { define <- min(grep("DEFINE:", run.val.upp)):(.section.ind.from.to(input.section, "DEFINE:", run = run.val.upp, input = TRUE)) }

      #...................
      ### ANALYSIS ####

      if (isTRUE(any(grepl("ANALYSIS:", run.val.upp)))) {

        analysis <- min(grep("ANALYSIS:", run.val.upp)):(.section.ind.from.to(input.section, "ANALYSIS:", run = run.val.upp, input = TRUE)) |>
          # Mplus Demo Version maximum exceeded
          (\(y) if (isTRUE(any(grepl("Mplus VERSION 8.6 DEMO", run.val[y])))) {

            if (isTRUE(any(grepl("ANALYSIS:", run.val.upp[y])))) { NULL } else { min(grep("ANALYSIS:", run.val.upp)):(.section.ind.from.to(input.section, "ANALYSIS:", run = run.val.upp[-(which(run.val.upp == "DEMO VERSION MAXIMUM EXCEEDED"):length(run.val.upp))], input = TRUE)) }

          } else {

            y

          })()

      }

      #...................
      ### MODEL ####

      if (isTRUE(any(grepl("MODEL:", run.val.upp)))) {

        model <- min(grep("MODEL:", run.val.upp)):(.section.ind.from.to(input.section, "MODEL:", run = run.val.upp, input = TRUE)) |>
          # Mplus Demo Version maximum exceeded
          (\(y) if (isTRUE(any(grepl("Mplus VERSION 8.6 DEMO", run.val[model])))) {

            if (isTRUE(any(grepl("MODEL:", run.val.upp[y])))) { NULL } else { min(grep("MODEL:", run.val.upp)):(.section.ind.from.to(input.section, "MODEL:", run = run.val.upp[-(which(run.val.upp == "DEMO VERSION MAXIMUM EXCEEDED"):length(run.val.upp))], input = TRUE)) }

          } else {

            y

          })()

      }

      #...................
      ### MODEL INDIRECT ####

      if (isTRUE(any(grepl("MODEL INDIRECT:", run.val.upp)))) { mod.ind <- min(grep("MODEL INDIRECT:", run.val.upp)):(.section.ind.from.to(input.section, "MODEL INDIRECT:", run = run.val.upp, input = TRUE)) }

      #...................
      ### MODEL TEST ####

      if (isTRUE(any(grepl("MODEL TEST:", run.val.upp)))) { mod.test <- min(grep("MODEL TEST:", run.val.upp)):(.section.ind.from.to(input.section, "MODEL TEST:", run = run.val.upp, input = TRUE)) }

      #...................
      ### MODEL PRIORS ####

      if (isTRUE(any(grepl("MODEL PRIORS:", run.val.upp)))) { mod.prior <- min(grep("MODEL PRIORS:", run.val.upp)):(.section.ind.from.to(input.section, "MODEL PRIORS:", run = run.val.upp, input = TRUE)) }

      #...................
      ### MONTECARLO ####

      if (isTRUE(any(grepl("MONTECARLO:", run.val.upp)))) { montecarlo <- min(grep("MONTECARLO:", run.val.upp)):(.section.ind.from.to(input.section, "MONTECARLO:", run = run.val.upp, input = TRUE)) }

      #...................
      ### MODEL POPULATION ####

      if (isTRUE(any(grepl("MODEL POPULATION:", run.val.upp)))) { mod.pop <- min(grep("MODEL POPULATION:", run.val.upp)):(.section.ind.from.to(input.section, "MODEL POPULATION:", run = run.val.upp, input = TRUE)) }

      #...................
      ### MODEL COVERAGE ####

      if (isTRUE(any(grepl("MODEL COVERAGE:", run.val.upp)))) { mod.cov <- min(grep("MODEL COVERAGE:", run.val.upp)):(.section.ind.from.to(input.section, "MODEL COVERAGE:", run = run.val.upp, input = TRUE)) }

      #...................
      ### MODEL MISSING ####

      if (isTRUE(any(grepl("MODEL MISSING:", run.val.upp)))) { mod.miss <- min(grep("MODEL MISSING:", run.val.upp)):(.section.ind.from.to(input.section, "MODEL MISSING:", run = run.val.upp, input = TRUE)) }

      #...................
      ### OUTPUT ####

      if (isTRUE(any(grepl("OUTPUT:", run.val.upp)))) { inpoutput <- min(grep("OUTPUT:", run.val.upp)):(.section.ind.from.to(input.section, "OUTPUT:", run = run.val.upp, input = TRUE)) }

      #...................
      ### SAVEDATA ####

      if (isTRUE(any(grepl("SAVEDATA:", run.val.upp)))) { savedata <- min(grep("SAVEDATA:", run.val.upp)):(.section.ind.from.to(input.section, "SAVEDATA:", run = run.val.upp, input = TRUE)) }

      #...................
      ### PLOT ####

      if (isTRUE(any(grepl("PLOT:", run.val.upp)))) { plot <- min(grep("PLOT:", run.val.upp)):(.section.ind.from.to(input.section, "PLOT:", run = run.val.upp, input = TRUE)) }

      #...................
      ### Message ####

      temp.inp <- c("INPUT READING TERMINATED NORMALLY", "*** WARNING", "*** ERROR", "*** FATAL", "DEMO VERSION MAXIMUM EXCEEDED")

      if (isTRUE(any(misty::chr.grepl(temp.inp, run.val.tech, fixed = TRUE)))) {

        if (any(grepl("*** FATAL", run.val.tech, fixed = TRUE))) {

          message <- min(misty::chr.grep(temp.inp, run.val.tech, fixed = TRUE)):length(run.val.tech)

        } else {

          message <- min(misty::chr.grep(temp.inp, run.val.tech, fixed = TRUE)):(run.length2[which(run.length2 > max(unlist(sapply(temp.inp, function(z) grep(z, run.val.tech, fixed = TRUE)))))[1L]])

        }

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Extract Output Sections ####

    #...................
    ### Sections ####

    section <- c("SUMMARY OF ANALYSIS", "SUMMARY OF DATA", "SUMMARY OF DATA FOR THE FIRST DATA SET", "SUMMARY OF DATA FOR THE FIRST REPLICATION",
                 "SUMMARY OF CENSORED LIMITS", "COUNT PROPORTION OF ZERO, MINIMUM AND MAXIMUM VALUES", "CROSSTABS FOR CATEGORICAL VARIABLES",
                 "SUMMARY OF MISSING DATA PATTERNS", "SUMMARY OF MISSING DATA PATTERNS FOR THE FIRST DATA SET", "SUMMARY OF MISSING DATA PATTERNS FOR THE FIRST REPLICATION",
                 "COVARIANCE COVERAGE OF DATA", "COVARIANCE COVERAGE OF DATA FOR THE FIRST DATA SET", "COVARIANCE COVERAGE OF DATA FOR THE FIRST REPLICATION",
                 "UNIVARIATE PROPORTIONS AND COUNTS FOR CATEGORICAL VARIABLES", "UNIVARIATE PROPORTIONS AND COUNTS FOR CATEGORICAL VARIABLES FOR THE FIRST REPLICATION", "UNIVARIATE PROPORTIONS AND COUNTS FOR CATEGORICAL VARIABLES FOR THE FIRST DATA SET",
                 "UNIVARIATE PROPORTIONS FOR CATEGORICAL VARIABLES", "UNIVARIATE PROPORTIONS FOR CATEGORICAL VARIABLES FOR THE FIRST REPLICATION", "UNIVARIATE PROPORTIONS FOR CATEGORICAL VARIABLES FOR THE FIRST DATA SET",
                 "THE MODEL ESTIMATION TERMINATED NORMALLY", "MODEL ESTIMATION DID NOT TERMINATE NORMALLY", "     WARNING:  THE SAMPLE COVARIANCE OF THE INDEPENDENT VARIABLES",
                 "     THE STANDARD ERRORS OF THE MODEL PARAMETER ESTIMATES MAY NOT BE", "     THE STANDARD ERRORS OF THE MODEL PARAMETER ESTIMATES COULD NOT BE",
                 "     WARNING:  THE LATENT VARIABLE COVARIANCE MATRIX (PSI) IS NOT POSITIVE", "     ONE OR MORE PARAMETERS WERE FIXED TO AVOID SINGULARITY OF THE",
                 "     IN THE OPTIMIZATION, ONE OR MORE LOGIT THRESHOLDS APPROACHED EXTREME VALUES", "     THE ROBUST CHI-SQUARE COULD NOT BE COMPUTED.",
                 "SAMPLE STATISTICS", "SAMPLE STATISTICS FOR THE FIRST REPLICATION", "SAMPLE STATISTICS FOR THE FIRST DATA SET", "RESULTS FOR BASIC ANALYSIS", "UNIVARIATE SAMPLE STATISTICS",
                 "RANDOM STARTS RESULTS RANKED FROM THE BEST TO THE WORST FIT FUNCTION VALUES", "RANDOM STARTS RESULTS RANKED FROM THE BEST TO THE WORST LOGLIKELIHOOD VALUES",
                 "SUMMARY OF MODEL FIT INFORMATION", "MODEL FIT INFORMATION", "FINAL CLASS COUNTS AND PROPORTIONS FOR THE LATENT CLASSES", "FINAL CLASS COUNTS AND PROPORTIONS FOR THE LATENT CLASS PATTERNS", "TRANSITION PROBABILITIES", "CLASSIFICATION QUALITY", "C-SPECIFIC CLASSIFICATION RESULTS",
                 "LATENT CLASS INDICATOR MEANS AND PROBABILITIES", "MODEL RESULTS", "MODEL RESULTS FOR THE CONFIGURAL MODEL", "MODEL RESULTS FOR THE METRIC MODEL", "MODEL RESULTS FOR THE SCALAR MODEL",
                 "MODEL RESULTS USE THE LATENT CLASS VARIABLE ORDER", "FACTOR SCORE COMPARISON", "TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT, AND DIRECT EFFECTS", "TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT, AND DIRECT EFFECTS FOR LATENT RESPONSE VARIABLES",
                 "TOTAL, INDIRECT, AND DIRECT EFFECTS BASED ON COUNTERFACTUALS (CAUSALLY-DEFINED EFFECTS)", "STANDARDIZED TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT, AND DIRECT EFFECTS",
                 "STANDARDIZED TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT, AND DIRECT EFFECTS FOR LATENT RESPONSE VARIABLES", "STANDARDIZED TOTAL, INDIRECT, AND DIRECT EFFECTS BASED ON COUNTERFACTUALS (CAUSALLY-DEFINED EFFECTS)",
                 "CONFIDENCE INTERVALS OF MODEL RESULTS", "CREDIBILITY INTERVALS OF MODEL RESULTS", "CONFIDENCE INTERVALS OF STANDARDIZED MODEL RESULTS",
                 "CREDIBILITY INTERVALS OF STANDARDIZED MODEL RESULTS", "CONFIDENCE INTERVALS OF TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT, AND DIRECT EFFECTS", "CREDIBILITY INTERVALS OF TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT, AND DIRECT EFFECTS",
                 "CONFIDENCE INTERVALS OF TOTAL, INDIRECT, AND DIRECT EFFECTS BASED ON COUNTERFACTUALS (CAUSALLY-DEFINED EFFECTS)", "CREDIBILITY INTERVALS OF TOTAL, INDIRECT, AND DIRECT EFFECTS BASED ON COUNTERFACTUALS (CAUSALLY-DEFINED EFFECTS)",
                 "CONFIDENCE INTERVALS OF TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT, AND DIRECT EFFECTS FOR LATENT RESPONSE VARIABLES", "CREDIBILITY INTERVALS OF TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT, AND DIRECT EFFECTS FOR LATENT RESPONSE VARIABLES",
                 "CONFIDENCE INTERVALS FOR THE LOGISTIC REGRESSION ODDS RATIO RESULTS", "CREDIBILITY INTERVALS FOR THE LOGISTIC REGRESSION ODDS RATIO RESULTS", "LOGISTIC REGRESSION ODDS RATIO RESULTS", "RESULTS IN PROBABILITY SCALE",
                 "LATENT CLASS INDICATOR ODDS RATIOS FOR THE LATENT CLASSES", "ALTERNATIVE PARAMETERIZATIONS FOR THE CATEGORICAL LATENT VARIABLE REGRESSION", "IRT PARAMETERIZATION", "STANDARDIZED MODEL RESULTS",
                 "WITHIN-LEVEL STANDARDIZED MODEL RESULTS FOR CLUSTER 1", "R-SQUARE", "MODEL MODIFICATION INDICES", "MODEL COMMAND WITH FINAL ESTIMATES USED AS STARTING VALUES", "RESIDUAL OUTPUT", "SAMPLE STATISTICS FOR ESTIMATED FACTOR SCORES",
                 "LOGRANK OUTPUT", "BRANT WALD TEST FOR PROPORTIONAL ODDS", "TECHNICAL OUTPUT", "TECHNICAL 1 OUTPUT", "TECHNICAL 2 OUTPUT", "TECHNICAL 3 OUTPUT", "H1 TECHNICAL 3 OUTPUT", "TECHNICAL 4 OUTPUT", "TECHNICAL 5/6 OUTPUT", "TECHNICAL 7 OUTPUT",
                 "TECHNICAL 8 OUTPUT", "TECHNICAL 9 OUTPUT", "TECHNICAL 10 OUTPUT", "TECHNICAL 11 OUTPUT", "TECHNICAL 12", "TECHNICAL 13 OUTPUT", "TECHNICAL 14 OUTPUT", "TECHNICAL 15 OUTPUT", "TECHNICAL 16 OUTPUT", "SUMMARY OF FACTOR SCORES",
                 "FACTOR DETERMINACIES", "SUMMARIES OF PLAUSIBLE VALUES (N = NUMBER OF OBSERVATIONS * NUMBER OF IMPUTATIONS)", "PLOT INFORMATION", "SAVEDATA INFORMATION")

    # Invariance Testing
    if (any(grepl("Invariance Testing", run.val.tech))) {

      section <- section[-misty::chr.grep(misty::chr.omit(c("CONFIGURAL MODEL", "METRIC MODEL", "SCALAR MODEL"), omit = names(which.min(which(sapply(c("CONFIGURAL MODEL", "METRIC MODEL", "SCALAR MODEL"), function(y) any(grepl(y, run.val.tech))))))), section)]

      if (isTRUE(result.default)) { result <- c(result, "tech9")}

    }

    #...................
    ### Warnings ####

    warn <- c("MODEL ESTIMATION TERMINATED", "MODEL ESTIMATION DID NOT TERMINATE NORMALLY", "DEGREES OF FREEDOM FOR THIS MODEL ARE NEGATIVE", "MODEL CONTAINS A NON-ZERO CORRELATION",
              "     WARNING:  THE SAMPLE COVARIANCE OF THE INDEPENDENT VARIABLES", "STANDARD ERRORS OF THE MODEL PARAMETER", "MLR STANDARD ERRORS COULD NOT BE COMPUTED",
              "ESTIMATED COVARIANCE MATRIX COULD NOT BE INVERTED", "PROBLEMS OCCURRED IN SEVERAL ITERATIONS", "MCONVERGENCE CRITERION OF THE EM ALGORITHM",
              "PROBLEMS OCCURRED IN SEVERAL ITERATIONS", "ESTIMATED BETWEEN COVARIANCE MATRIX IS NOT POSITIVE DEFINITE", "THE LATENT VARIABLE COVARIANCE MATRIX",
              "LOGLIKELIHOOD DECREASED IN THE LAST EM ITERATION", "IN THE OPTIMIZATION,", "ONE OR MORE MULTINOMIAL LOGIT PARAMETERS WERE FIXED",
              "ONE OR MORE PARAMETERS WERE FIXED TO AVOID SINGULARITY", "H1 MODEL ESTIMATION DID NOT CONVERGE", "NO CONVERGENCE", "ROBUST CHI-SQUARE COULD NOT BE COMPUTED",
              "CONVERGENCE CRITERION IS NOT SATISFIED", "CONVERGENCE CRITERION FOR THE LATENT VARIABLE", "THE RESIDUAL COVARIANCE MATRIX",
              "THE MODEL ESTIMATION HAS REACHED A SADDLE POINT", "THE MISSING DATA EM ALGORITHM FOR THE H1 MODEL")

    # Output objects
    summary.analysis <- summary.analysis.short <- summary.data <- summary.data.short <- prop.count <- summary.censor <- prop.zero <- crosstab <- summary.miss <- coverage <- basic <- sample.stat <- uni.sample.stat <- random.starts <- summary.fit <- mod.est <- fit <- class.count <- ind.means <- trans.prob <- classif <- mod.result <- odds.ratio <- prob.scale <- ind.odds.ratio <- alt.param <- irt.param <- brant.wald <- std.mod.result <- rsquare <- total.indirect <- std.total.indirect <- std.mod.result.cluster <- fs.comparison <- conf.mod.result <- conf.std.conf <- conf.total.indirect <- conf.odds.ratio <- modind <- resid <- logrank <- tech1 <- tech2 <- tech3 <- h1.tech3 <- tech4 <- tech5 <- tech6 <- tech7 <- tech8 <- tech9 <- tech10 <- tech11 <- tech12 <- tech13 <- tech14 <- tech15 <- tech16 <- svalues <- stat.fscores <- summary.fscores <- pv <- plotinfo <- saveinfo <- NULL

    #...................
    ### SUMMARY OF ANALYSIS ####

    if (isTRUE(any(run.val.tech == "SUMMARY OF ANALYSIS"))) {

      #### Long SUMMARY OF ANALYSIS
      summary.analysis <- which(run.val.tech == "SUMMARY OF ANALYSIS"):(.section.ind.from.to(section, "SUMMARY OF ANALYSIS", run = run.val.tech))

      #### Short SUMMARY OF ANALYSIS
      summary.analysis.short <- which(run.val.tech == "SUMMARY OF ANALYSIS"):(grep("Number of dependent variables", run.val.tech) - 1L)

      # Estimator
      if (isTRUE(any(grepl("Estimator                                    ", run.val.tech)))) { summary.analysis.short <- c(summary.analysis.short, grep("Estimator                                    ", run.val.tech)) }

      # Optimization algorithm
      if (isTRUE(any(grepl("Optimization algorithm                                    ", run.val.tech)))) { summary.analysis.short <- c(summary.analysis.short, grep("Optimization algorithm                                    ", run.val.tech)) }

      # Value of seed
      if (isTRUE(any(grepl("Value of seed", run.val.tech)))) { summary.analysis.short <- setdiff(summary.analysis.short, grep("Value of seed", run.val.tech)) }

    }

    #...................
    ### SUMMARY OF DATA ####

    temp.section <- c("SUMMARY OF DATA", "SUMMARY OF DATA FOR THE FIRST DATA SET", "SUMMARY OF DATA FOR THE FIRST REPLICATION")

    if (isTRUE(any(sapply(temp.section, function(z) run.val.tech == z)))) {

      #### Long SUMMARY OF DATA
      summary.data <- min(unlist(sapply(temp.section, function(z) which(run.val.tech == z)))):(.section.ind.from.to(section, temp.section, run = run.val.tech))

      #### Short SUMMARY OF DATA
      summary.data.short <- summary.data

      ##### Number of missing data patterns
      if (isTRUE(any(grepl("Number of missing data patterns", run.val.tech)))) {

        if (isTRUE(any(grepl("Number of clusters", run.val.tech)))) {

          if (isTRUE(grep("Number of clusters", run.val.tech) > grep("Number of missing data patterns", run.val.tech))) {

            summary.data.short <- setdiff(summary.data.short, min(grep("Number of missing data patterns", run.val.tech)):(grep("Number of clusters", run.val.tech) - 1L))

          }

        } else if (isTRUE(any(grepl("Estimated Intraclass Correlations", run.val.tech)))) {

          summary.data.short <- setdiff(summary.data.short, min(grep("Number of missing data patterns", run.val.tech)):(grep("Estimated Intraclass Correlations", run.val.tech) - 1L))

        } else {

          summary.data.short <- setdiff(summary.data.short, min(grep("Number of missing data patterns", run.val.tech)):(run.length2[which(run.length2 > min(grep("Number of missing data patterns", run.val.tech)))[1L]]))

        }

      }

      ##### Cluster ID with Size
      if (isTRUE(any(grepl("Cluster ID with Size", run.val.tech)))) {

        if (isTRUE(any(grepl("Estimated Intraclass Correlations for the Y Variables", run.val.tech)))) {

          summary.data.short <- setdiff(summary.data.short, min(grep("Cluster ID with Size", run.val.tech)):(grep("Estimated Intraclass Correlations for the Y Variables", run.val.tech) - 1L))

        } else {

          summary.data.short <- setdiff(summary.data.short, min(grep("Cluster ID with Size", run.val.tech)):(run.length2[which(run.length2 > min(grep("Cluster ID with Size", run.val.tech)))[1L]]))

        }

      } else if (isTRUE(length(grep("Cluster ID with Size", run.val.tech)) > 1L)) {

        summary.data.short <- setdiff(summary.data.short, grep("Cluster ID with Size", run.val.tech)[2L]:(run.length2[which(run.length2 > grep("Cluster ID with Size", run.val.tech)[2L])[1L]]))

      } else if (isTRUE(any(grepl("Cluster information", run.val.tech)) && all(!grepl("Estimated Intraclass Correlations", run.val.tech)))) {

        summary.data.short <- setdiff(summary.data.short, grep("Cluster information", run.val.tech):(run.length2[which(run.length2 > grep("Cluster information", run.val.tech))[1L]]))

      } else if (isTRUE(any(run.val.tech == "SUMMARY OF DATA FOR THE FIRST REPLICATION") && any(run.val.tech == "     Cluster information"))) {

        summary.data.short <- setdiff(summary.data.short, which(run.val.tech == "     Cluster information"):(which(run.val.tech == "     Estimated Intraclass Correlations for the Y Variables") - 1L))

      }

      if (isTRUE(any(grepl("Cluster ID with Size", run.val.tech[summary.data.short])))) { summary.data.short <- setdiff(summary.data.short, grep("Cluster ID with Size", run.val.tech)[1L]:(grep("Cluster information", run.val.tech)[2L] - 1L)) }

      # Estimated Intraclass Correlations for the Y Variables
      if (isTRUE(any(run.val.tech == "     Estimated Intraclass Correlations for the Y Variables"))) { summary.data.short <- setdiff(summary.data.short, c(sapply(which(run.val.tech == "     Estimated Intraclass Correlations for the Y Variables"), function(z) z:(z + 1L)))) }

      # Identical long and short version
      if (isTRUE(identical(summary.data, summary.data.short))) { summary.data.short <- NULL }

      # Empty short version
      if (isTRUE(length(misty::chr.omit(run.val.tech[summary.data.short], check = FALSE)) == 1L)) { summary.data.short <- NULL }

    }

    #...................
    ### UNIVARIATE PROPORTIONS AND COUNTS FOR CATEGORICAL VARIABLES ####

    temp.section <- c("UNIVARIATE PROPORTIONS AND COUNTS FOR CATEGORICAL VARIABLES", "UNIVARIATE PROPORTIONS AND COUNTS FOR CATEGORICAL VARIABLES FOR THE FIRST DATA", "UNIVARIATE PROPORTIONS AND COUNTS FOR CATEGORICAL VARIABLES FOR THE FIRST REPLICATION", "UNIVARIATE PROPORTIONS FOR CATEGORICAL VARIABLES", "UNIVARIATE PROPORTIONS FOR CATEGORICAL VARIABLES FOR THE FIRST DATA", "UNIVARIATE PROPORTIONS FOR CATEGORICAL VARIABLES FOR THE FIRST REPLICATION")

    if (isTRUE(any(sapply(temp.section, function(z) run.val.tech == z)))) { prop.count <- unlist(sapply(temp.section, function(z) which(run.val.tech == z))):(.section.ind.from.to(section, temp.section, run = run.val.tech)) }

    #...................
    ### SUMMARY OF CENSORED LIMITS ####

    if (isTRUE(any(run.val.tech == "SUMMARY OF CENSORED LIMITS"))) { summary.censor <- which(run.val.tech == "SUMMARY OF CENSORED LIMITS"):(.section.ind.from.to(section, "SUMMARY OF CENSORED LIMITS", run = run.val.tech)) }

    #...................
    ### COUNT PROPORTION OF ZERO, MINIMUM AND MAXIMUM VALUES ####

    if (isTRUE(any(run.val.tech == "COUNT PROPORTION OF ZERO, MINIMUM AND MAXIMUM VALUES"))) { prop.zero <- which(run.val.tech == "COUNT PROPORTION OF ZERO, MINIMUM AND MAXIMUM VALUES"):(.section.ind.from.to(section, "COUNT PROPORTION OF ZERO, MINIMUM AND MAXIMUM VALUES", run = run.val.tech)) }

    #...................
    ### CROSSTABS FOR CATEGORICAL VARIABLES ####

    if (isTRUE(any(run.val.tech == "CROSSTABS FOR CATEGORICAL VARIABLES"))) { crosstab <- min(which(run.val.tech == "CROSSTABS FOR CATEGORICAL VARIABLES")):(.section.ind.from.to(section, "CROSSTABS FOR CATEGORICAL VARIABLES", run = run.val.tech)) }

    #...................
    ### SUMMARY OF MISSING DATA PATTERNS ####

    temp.section <- c("SUMMARY OF MISSING DATA PATTERNS", "SUMMARY OF MISSING DATA PATTERNS FOR THE FIRST DATA SET", "SUMMARY OF MISSING DATA PATTERNS FOR THE FIRST REPLICATION")

    if (isTRUE(any(sapply(temp.section, function(z) run.val.tech == z)))) { summary.miss <- min(unlist(sapply(temp.section, function(z) which(run.val.tech == z)))):(.section.ind.from.to(section, temp.section, run = run.val.tech)) }

    #...................
    ### COVARIANCE COVERAGE OF DATA ####

    temp.section <- c("COVARIANCE COVERAGE OF DATA", "COVARIANCE COVERAGE OF DATA FOR THE FIRST DATA SET", "COVARIANCE COVERAGE OF DATA FOR THE FIRST REPLICATION")

    if (isTRUE(any(sapply(temp.section, function(z) run.val.tech == z)))) { coverage <- min(unlist(sapply(temp.section, function(z) which(run.val.tech == z)))):(.section.ind.from.to(section, temp.section, run = run.val.tech)) }

    #...................
    ### RESULTS FOR BASIC ANALYSIS ####

    if (isTRUE(any(run.val.tech == "RESULTS FOR BASIC ANALYSIS"))) { basic <- which(run.val.tech == "RESULTS FOR BASIC ANALYSIS"):(.section.ind.from.to(section, "RESULTS FOR BASIC ANALYSIS", run = run.val.tech)) }

    #...................
    ### SAMPLE STATISTICS ####

    temp.section <- c("SAMPLE STATISTICS", "SAMPLE STATISTICS FOR THE FIRST DATA SET", "SAMPLE STATISTICS FOR THE FIRST REPLICATION")

    if (isTRUE(any(sapply(temp.section, function(z) run.val.tech == z)))) { sample.stat <- min(unlist(sapply(temp.section, function(z) which(run.val.tech == z)))):(.section.ind.from.to(section, temp.section, run = run.val.tech)) }

    #...................
    ### UNIVARIATE SAMPLE STATISTICS ####

    if (isTRUE(any(run.val.tech =="UNIVARIATE SAMPLE STATISTICS"))) {

      if (isTRUE(any(grepl("WARNING:", run.val.tech)) & all(!grepl("RANDOM STARTS RESULTS", run.val.tech)))) {

        uni.sample.stat <- which(run.val.tech == "UNIVARIATE SAMPLE STATISTICS"):(run.length2[which(run.length2 > min(grep("UNIVARIATE SAMPLE STATISTICS", run.val.tech)))[2L]])

      } else {

        uni.sample.stat <- which(run.val.tech == "UNIVARIATE SAMPLE STATISTICS"):(.section.ind.from.to(section, "UNIVARIATE SAMPLE STATISTICS", run = run.val.tech))

      }

    }

    #...................
    ### RANDOM STARTS RESULTS RANKED ####

    temp.section <- c("RANDOM STARTS RESULTS RANKED FROM THE BEST TO THE WORST LOGLIKELIHOOD VALUES", "RANDOM STARTS RESULTS RANKED FROM THE BEST TO THE WORST FIT FUNCTION VALUES")

    if (isTRUE(any(sapply(temp.section, function(z) run.val.tech == z)))) { random.starts <- min(unlist(sapply(temp.section, function(z) which(run.val.tech == z)))):(.section.ind.from.to(section, temp.section, run = run.val.tech)) }

    #...................
    ### SUMMARY OF MODEL FIT INFORMATION ####

    if (isTRUE(any(run.val.tech == "SUMMARY OF MODEL FIT INFORMATION"))) { summary.fit <- which(run.val.tech == "SUMMARY OF MODEL FIT INFORMATION"):(.section.ind.from.to(section, "SUMMARY OF MODEL FIT INFORMATION", run = run.val.tech)) }

    #...................
    ### MODEL ESTIMATION ####

    if (isTRUE(any(c(sapply(warn, grepl, run.val.tech))))) {

      mod.est <- unlist(sapply(warn, function(y) which(grepl(y, run.val.tech)))) |>
        (\(y) min(y):(run.length2[which(run.length2 > max(y))[1L]]))()

    }

    #...................
    ### MODEL FIT INFORMATION ####

    if (isTRUE(any(run.val.tech == "MODEL FIT INFORMATION") && all(!grepl("RESULTS FOR EXPLORATORY FACTOR ANALYSIS", run.val.tech)))) {

      fit <- which(run.val.tech == "MODEL FIT INFORMATION"):(.section.ind.from.to(section, "MODEL FIT INFORMATION", run = run.val.tech))

    }

    #...................
    ### FINAL CLASS COUNTS AND PROPORTIONS ####

    if (isTRUE(any(grepl("FINAL CLASS COUNTS AND PROPORTIONS FOR", run.val.tech)) && all(!grepl("RESULTS FOR EXPLORATORY FACTOR ANALYSIS", run.val.tech)))) {

      if (isTRUE(any(grepl("LATENT CLASS VARIABLE ORDER", run.val.tech)))) {

        class.count <- which(run.val.tech == "MODEL RESULTS USE THE LATENT CLASS VARIABLE ORDER"):(run.length2[which(run.length2 > max(grep("BASED ON THEIR MOST LIKELY LATENT CLASS PATTERN", run.val.tech)))[1L]])

      } else {

        ind.end <- min(unlist(sapply(section, function(z) if (isTRUE(any(run.val.tech == z))) { sapply(which(run.val.tech == z), function(q) if (isTRUE(q > max(grep("FINAL CLASS COUNTS AND PROPORTIONS FOR", run.val.tech)))) { q } else { length(run.val.tech) + 1L }) })))

        class.count <- min(grep("FINAL CLASS COUNTS AND PROPORTIONS FOR", run.val.tech)):(ind.end - 1L)

      }

    }

    #...................
    ### LATENT CLASS INDICATOR MEANS AND PROBABILITIES ####

    if (isTRUE(any(run.val.tech == "LATENT CLASS INDICATOR MEANS AND PROBABILITIES"))) { ind.means <- which(run.val.tech == "LATENT CLASS INDICATOR MEANS AND PROBABILITIES"):(.section.ind.from.to(section, "LATENT CLASS INDICATOR MEANS AND PROBABILITIES", run = run.val.tech)) }

    #...................
    ### TRANSITION PROBABILITIES ####

    if (isTRUE(any(run.val.tech == "LATENT TRANSITION PROBABILITIES BASED ON THE ESTIMATED MODEL"))) {

      trans.prob <- which(run.val.tech == "LATENT TRANSITION PROBABILITIES BASED ON THE ESTIMATED MODEL"):(.section.ind.from.to(section, "LATENT TRANSITION PROBABILITIES BASED ON THE ESTIMATED MODEL", run = run.val.tech))

      # Remove TRANSITION PROBABILITIES from FINAL CLASS COUNTS AND PROPORTIONS
      class.count <- setdiff(class.count, trans.prob)

    }

    #...................
    ### CLASSIFICATION QUALITY ####

    if (isTRUE(any(grepl("CLASSIFICATION", run.val.tech)) && all(!grepl("EXPLORATORY FACTOR ANALYSIS", run.val.tech)))) {

      if (isTRUE(any(grepl("C-SPECIFIC CLASSIFICATION RESULTS", run.val.tech)))) {

        if (isTRUE(any(grepl("CLASSIFICATION QUALITY", run.val.tech)))) {

          classif <- c(which(run.val.tech == "CLASSIFICATION QUALITY"):(run.length2[which(run.length2 > which(run.val.tech == "CLASSIFICATION QUALITY"))[1L]]), which(run.val.tech == "C-SPECIFIC CLASSIFICATION RESULTS"):(run.length2[which(run.length2 > max(grep("Logits for the Classification Probabilities", run.val.tech)))[1L]]))

        } else {

          classif <- which(run.val.tech == "C-SPECIFIC CLASSIFICATION RESULTS"):(run.length2[which(run.length2 > max(grep("Logits for the Classification Probabilities", run.val.tech)))[1L]])

        }

      } else {

        if (isTRUE(any(grepl("Logits for the Classification Probabilities", run.val.tech)))) {

          classif <- which(run.val.tech == "CLASSIFICATION QUALITY"):(run.length2[which(run.length2 > max(grep("Logits for the Classification Probabilities", run.val.tech)))[1L]])

        } else {

          classif <- grep("CLASSIFICATION QUALITY", run.val.tech):(run.length2[which(run.length2 > max(grep("CLASSIFICATION QUALITY", run.val.tech)))[1L]])

        }

      }

      # Remove empty rows
      classif <- setdiff(classif, (which(run.val.tech %in% c("by Latent Class (Column)", "by Latent Class (Row)")) + 3L))

    }

    #...................
    ### MODEL RESULTS and RESULTS FOR EXPLORATORY FACTOR ANALYSIS ####

    if (isTRUE(any(grepl("MODEL RESULTS", run.val.tech)) || any(grepl("RESULTS FOR EXPLORATORY FACTOR ANALYSIS", run.val.tech)))) {

      if (isTRUE(all(!grepl("RESULTS FOR EXPLORATORY FACTOR ANALYSIS", run.val.tech)))) {

        if (isTRUE(all(!grepl("Invariance Testing", run.val.tech)))) {

          mod.result <- which(run.val.tech == "MODEL RESULTS"):(.section.ind.from.to(section, "MODEL RESULTS", run = run.val.tech))

        #### Invariance Testing
        } else {

          # Configural model
          if (any(grepl("CONFIGURAL MODEL", run.val.tech))) {

            mod.result <- which(run.val.tech == "MODEL RESULTS FOR THE CONFIGURAL MODEL"):(.section.ind.from.to(section, "MODEL RESULTS FOR THE CONFIGURAL MODEL", run = run.val.tech))

          # Metric model
          } else if (any(grepl("METRIC MODEL", run.val.tech))) {

            mod.result <- which(run.val.tech == "MODEL RESULTS FOR THE METRIC MODEL"):(.section.ind.from.to(section, "MODEL RESULTS FOR THE METRIC MODEL", run = run.val.tech))

          # Scalar model
          } else {

            mod.result <- which(run.val.tech == "MODEL RESULTS FOR THE SCALAR MODEL"):(.section.ind.from.to(section, "MODEL RESULTS FOR THE SCALAR MODEL", run = run.val.tech))

          }

        }

      #### Exploratory Factor Analysis
      } else {

        if (isTRUE(any(c(grepl("PLOT INFORMATION", run.val.tech), grepl("SAVEDATA INFORMATION", run.val.tech))))) {

          mod.result <- min(grep("RESULTS FOR EXPLORATORY FACTOR ANALYSIS", run.val.tech)):(min(c(grep("TECHNICAL", run.val.tech), grep("PLOT INFORMATION", run.val.tech), grep("SAVEDATA INFORMATION", run.val.tech))) - 1L)

        } else {

          mod.result <- min(grep("RESULTS FOR EXPLORATORY FACTOR ANALYSIS", run.val.tech)):length(run.val.tech)

        }

      }

    }

    #...................
    ### LOGISTIC REGRESSION ODDS RATIO RESULTS ####

    if (isTRUE(any(run.val.tech == "LOGISTIC REGRESSION ODDS RATIO RESULTS"))) { odds.ratio <- which(run.val.tech == "LOGISTIC REGRESSION ODDS RATIO RESULTS"):(.section.ind.from.to(section, "LOGISTIC REGRESSION ODDS RATIO RESULTS", run = run.val.tech)) }

    #...................
    ### RESULTS IN PROBABILITY SCALE ####

    if (isTRUE(any(run.val.tech == "RESULTS IN PROBABILITY SCALE"))) { prob.scale <- which(run.val.tech == "RESULTS IN PROBABILITY SCALE"):(.section.ind.from.to(section, "RESULTS IN PROBABILITY SCALE", run = run.val.tech)) }

    #...................
    ### LATENT CLASS INDICATOR ODDS RATIOS FOR THE LATENT CLASSES ####

    if (isTRUE(any(run.val.tech == "LATENT CLASS INDICATOR ODDS RATIOS FOR THE LATENT CLASSES"))) { ind.odds.ratio <- which(run.val.tech == "LATENT CLASS INDICATOR ODDS RATIOS FOR THE LATENT CLASSES"):(.section.ind.from.to(section, "LATENT CLASS INDICATOR ODDS RATIOS FOR THE LATENT CLASSES", run = run.val.tech)) }

    #...................
    ### ALTERNATIVE PARAMETERIZATIONS FOR THE CATEGORICAL LATENT VARIABLE REGRESSION ####

    if (isTRUE(any(run.val.tech == "ALTERNATIVE PARAMETERIZATIONS FOR THE CATEGORICAL LATENT VARIABLE REGRESSION"))) { alt.param <- min(which(run.val.tech == "ALTERNATIVE PARAMETERIZATIONS FOR THE CATEGORICAL LATENT VARIABLE REGRESSION")):(.section.ind.from.to(section, "ALTERNATIVE PARAMETERIZATIONS FOR THE CATEGORICAL LATENT VARIABLE REGRESSION", run = run.val.tech)) }

    #...................
    ### IRT PARAMETERIZATION ####

    if (isTRUE(any(grepl("IRT PARAMETERIZATION", run.val.tech)))) { irt.param <- which(run.val.tech == "IRT PARAMETERIZATION"):(.section.ind.from.to(section, "IRT PARAMETERIZATION", run = run.val.tech)) }

    #...................
    ### BRANT WALD TEST FOR PROPORTIONAL ODDS ####

    if (isTRUE(any(run.val.tech == "BRANT WALD TEST FOR PROPORTIONAL ODDS"))) { brant.wald <- which(run.val.tech == "BRANT WALD TEST FOR PROPORTIONAL ODDS"):(.section.ind.from.to(section, "BRANT WALD TEST FOR PROPORTIONAL ODDS", run = run.val.tech)) }

    #...................
    ### STANDARDIZED MODEL RESULTS ####

    if (isTRUE(any(run.val.tech == "STANDARDIZED MODEL RESULTS"))) { std.mod.result <- which(run.val.tech == "STANDARDIZED MODEL RESULTS"):(.section.ind.from.to(section, "STANDARDIZED MODEL RESULTS", run = run.val.tech)) }

    #...................
    ### R-SQUARE ####

    if (isTRUE(any(run.val.tech == "R-SQUARE"))) { rsquare <- which(run.val.tech == "R-SQUARE"):(.section.ind.from.to(section, "R-SQUARE", run = run.val.tech)) }

    #...................
    ### TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT, AND DIRECT EFFECTS ####

    temp.section <- c("TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT, AND DIRECT EFFECTS", "TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT, AND DIRECT EFFECTS FOR LATENT RESPONSE VARIABLES", "TOTAL, INDIRECT, AND DIRECT EFFECTS BASED ON COUNTERFACTUALS (CAUSALLY-DEFINED EFFECTS)")

    if (isTRUE(any(sapply(temp.section, function(z) run.val.tech == z)))) { total.indirect <- min(unlist(sapply(temp.section, function(z) which(run.val.tech == z)))):(.section.ind.from.to(section, temp.section, run = run.val.tech)) }

    #...................
    ### STANDARDIZED TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT, AND DIRECT EFFECTS ####

    temp.section <- c("STANDARDIZED TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT, AND DIRECT EFFECTS", "STANDARDIZED TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT, AND DIRECT EFFECTS FOR LATENT RESPONSE VARIABLES", "STANDARDIZED TOTAL, INDIRECT, AND DIRECT EFFECTS BASED ON COUNTERFACTUALS (CAUSALLY-DEFINED EFFECTS)")

    if (isTRUE(any(sapply(temp.section, function(z) run.val.tech == z)))) { std.total.indirect <- min(unlist(sapply(temp.section, function(z) which(run.val.tech == z)))):(.section.ind.from.to(section, temp.section, run = run.val.tech)) }

    #...................
    ### WITHIN-LEVEL STANDARDIZED MODEL RESULTS FOR CLUSTER ####

    if (isTRUE(any(run.val.tech == "WITHIN-LEVEL STANDARDIZED MODEL RESULTS FOR CLUSTER 1"))) { std.mod.result.cluster <- which(run.val.tech == "WITHIN-LEVEL STANDARDIZED MODEL RESULTS FOR CLUSTER 1", run.val.tech):(.section.ind.from.to(section, "WITHIN-LEVEL STANDARDIZED MODEL RESULTS FOR CLUSTER 1", run = run.val.tech)) }

    #...................
    ### BETWEEN-LEVEL FACTOR SCORE COMPARISONS ####

    if (isTRUE(any(run.val.tech == "BETWEEN-LEVEL FACTOR SCORE COMPARISONS"))) { fs.comparison <- which(run.val.tech == "BETWEEN-LEVEL FACTOR SCORE COMPARISONS"):(.section.ind.from.to(section, "BETWEEN-LEVEL FACTOR SCORE COMPARISONS", run = run.val.tech)) }

    #...................
    ### CONFIDENCE INTERVALS OF MODEL RESULTS ####

    if (isTRUE(any(run.val.tech == "CONFIDENCE INTERVALS OF MODEL RESULTS"))) { conf.mod.result <- which(run.val.tech == "CONFIDENCE INTERVALS OF MODEL RESULTS"):(.section.ind.from.to(section, "CONFIDENCE INTERVALS OF MODEL RESULTS", run = run.val.tech)) }

    #...................
    ### CONFIDENCE INTERVALS OF STANDARDIZED MODEL RESULTS ####

    if (isTRUE(any(run.val.tech == "CONFIDENCE INTERVALS OF STANDARDIZED MODEL RESULTS"))) { conf.std.conf <- which(run.val.tech == "CONFIDENCE INTERVALS OF STANDARDIZED MODEL RESULTS"):(.section.ind.from.to(section, "CONFIDENCE INTERVALS OF STANDARDIZED MODEL RESULTS", run = run.val.tech)) }

    #...................
    ### CONFIDENCE INTERVALS OF TOTAL, INDIRECT, AND DIRECT EFFECTS ####

    temp.section <- c("CONFIDENCE INTERVALS OF TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT, AND DIRECT EFFECTS", "CONFIDENCE INTERVALS OF TOTAL, INDIRECT, AND DIRECT EFFECTS BASED ON COUNTERFACTUALS (CAUSALLY-DEFINED EFFECTS)", "CONFIDENCE INTERVALS OF TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT, AND DIRECT EFFECTS FOR LATENT RESPONSE VARIABLES")

    if (isTRUE(any(sapply(temp.section, function(z) run.val.tech == z)))) { conf.total.indirect <- min(unlist(sapply(temp.section, function(z) which(run.val.tech == z)))):(.section.ind.from.to(section, temp.section, run = run.val.tech)) }

    #...................
    ### CONFIDENCE INTERVALS FOR THE LOGISTIC REGRESSION ODDS RATIO RESULTS ####

    if (isTRUE(any(run.val.tech == "CONFIDENCE INTERVALS FOR THE LOGISTIC REGRESSION ODDS RATIO RESULTS"))) { conf.odds.ratio <- c(which(run.val.tech == "CONFIDENCE INTERVALS FOR THE LOGISTIC REGRESSION ODDS RATIO RESULTS"), which(run.val.tech == "CONFIDENCE INTERVALS FOR THE LOGISTIC REGRESSION ODDS RATIO RESULTS") + 1L, min(which(run.val.tech == "                  Lower .5%  Lower 2.5%    Lower 5%    Estimate    Upper 5%  Upper 2.5%   Upper .5%")), (which(run.val.tech == "CONFIDENCE INTERVALS FOR THE LOGISTIC REGRESSION ODDS RATIO RESULTS") + 1L):(.section.ind.from.to(section, "CONFIDENCE INTERVALS FOR THE LOGISTIC REGRESSION ODDS RATIO RESULTS", run = run.val.tech))) }

    #...................
    ### MODEL MODIFICATION INDICES ####

    if (isTRUE(any(run.val.tech == "MODEL MODIFICATION INDICES"))) { modind <- which(run.val.tech == "MODEL MODIFICATION INDICES"):(.section.ind.from.to(section, "MODEL MODIFICATION INDICES", run = run.val.tech)) }

    #...................
    ### RESIDUAL OUTPUT ####

    if (isTRUE(any(run.val.tech == "RESIDUAL OUTPUT"))) { resid <- which(run.val.tech == "RESIDUAL OUTPUT"):(.section.ind.from.to(section, "RESIDUAL OUTPUT", run = run.val.tech)) }

    #...................
    ### LOGRANK OUTPUT ####

    if (isTRUE(any(run.val.tech == "LOGRANK OUTPUT"))) { logrank <- which(run.val.tech == "LOGRANK OUTPUT"):(.section.ind.from.to(section, "LOGRANK OUTPUT", run = run.val.tech)) }

    #...................
    ### TECH RESULTS ####

    if (isTRUE(any(grepl("TECHNICAL", run.val)))) {

      section <- misty::chr.omit(section, omit = c("THE MODEL ESTIMATION TERMINATED NORMALLY", "MODEL ESTIMATION DID NOT TERMINATE NORMALLY", "     WARNING:  THE SAMPLE COVARIANCE OF THE INDEPENDENT VARIABLES", "     THE STANDARD ERRORS OF THE MODEL PARAMETER ESTIMATES MAY NOT BE", "     THE STANDARD ERRORS OF THE MODEL PARAMETER ESTIMATES COULD NOT BE", "     WARNING:  THE LATENT VARIABLE COVARIANCE MATRIX (PSI) IS NOT POSITIVE", "     ONE OR MORE PARAMETERS WERE FIXED TO AVOID SINGULARITY OF THE", "     IN THE OPTIMIZATION, ONE OR MORE LOGIT THRESHOLDS APPROACHED EXTREME VALUES", "     THE ROBUST CHI-SQUARE COULD NOT BE COMPUTED."), check = FALSE)

      #### TECH outputs
      tech <- names(which(sapply(c(paste("TECHNICAL", c(1L:4L, 7L:16L), "OUTPUT"), "TECHNICAL 5/6 OUTPUT", "H1 TECHNICAL 3 OUTPUT"), function(z) any(run.val == z))))

      #### Loop along TECH outputs
      for (i in tech) {

        if (isTRUE(i != "H1 TECHNICAL 3 OUTPUT")) {

          assign(paste0("tech", gsub(".*?([0-9]+).*", "\\1", i)), which(run.val == i):(.section.ind.from.to(section, cur.section = i, run = run.val)))

          # Remove empty TECH output
          if (isTRUE(length(misty::chr.omit(run.val[eval(parse(text = paste0("tech", gsub(".*?([0-9]+).*", "\\1", i))))], check = FALSE)) == 1L)) { assign(paste0("tech", gsub(".*?([0-9]+).*", "\\1", i)), NULL) }

        } else {

          assign("h1.tech3", which(run.val == i):(.section.ind.from.to(section, cur.section = i, run = run.val)))

          # Remove empty H1 TECHNICAL 3 output
          if (isTRUE(length(misty::chr.omit(run.val[h1.tech3], check = FALSE)) == 1L)) { assign("h1.tech3", NULL) }

        }

      }

    }

    #...................
    ### MODEL COMMAND WITH FINAL ESTIMATES USED AS STARTING VALUES ####

    if (isTRUE(any(run.val == "MODEL COMMAND WITH FINAL ESTIMATES USED AS STARTING VALUES"))) { svalues <- which(run.val == "MODEL COMMAND WITH FINAL ESTIMATES USED AS STARTING VALUES"):(.section.ind.from.to(section, "MODEL COMMAND WITH FINAL ESTIMATES USED AS STARTING VALUES", run = run.val)) }

    #...................
    ### SAMPLE STATISTICS FOR ESTIMATED FACTOR SCORES ####

    if (isTRUE(any(run.val == "SAMPLE STATISTICS FOR ESTIMATED FACTOR SCORES"))) { stat.fscores <- which(run.val == "SAMPLE STATISTICS FOR ESTIMATED FACTOR SCORES"):(.section.ind.from.to(section, "SAMPLE STATISTICS FOR ESTIMATED FACTOR SCORES", run = run.val)) }

    #...................
    ### SUMMARY OF FACTOR SCORES ####

    if (isTRUE(any(run.val == "SUMMARY OF FACTOR SCORES"))) { summary.fscores <- which(run.val == "SUMMARY OF FACTOR SCORES"):(.section.ind.from.to(section, "SUMMARY OF FACTOR SCORES", run = run.val)) }

    #...................
    ### SUMMARIES OF PLAUSIBLE VALUES (N = NUMBER OF OBSERVATIONS * NUMBER OF IMPUTATIONS) ####

    if (isTRUE(any(run.val == "SUMMARIES OF PLAUSIBLE VALUES (N = NUMBER OF OBSERVATIONS * NUMBER OF IMPUTATIONS)"))) { pv <- min(which(run.val == "SUMMARIES OF PLAUSIBLE VALUES (N = NUMBER OF OBSERVATIONS * NUMBER OF IMPUTATIONS)")):(.section.ind.from.to(section, "SUMMARIES OF PLAUSIBLE VALUES (N = NUMBER OF OBSERVATIONS * NUMBER OF IMPUTATIONS)", run = run.val)) }

    #...................
    ### PLOT Information ####

    if (isTRUE(any(run.val == "PLOT INFORMATION"))) { plotinfo <- which(run.val == "PLOT INFORMATION"):(.section.ind.from.to(section, "PLOT INFORMATION", run = run.val)) }

    #...................
    ### SAVEDATA INFORMATION ####

    if (isTRUE(any(run.val == "SAVEDATA INFORMATION"))) { saveinfo <- which(run.val == "SAVEDATA INFORMATION"):(.section.ind.from.to(section, "SAVEDATA INFORMATION", run = run.val)) }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Return Object ####

    return.object <- list(# Mplus version
                          mplus = mplus,
                          # Input commands
                          input = list(title = if (!is.null(title)) { run.val[title] } else { NULL },
                                       data = if (!is.null(data)) { run.val[data] } else { NULL },
                                       data.imp = if (!is.null(data.imp)) { run.val[data.imp] } else { NULL },
                                       data.wl = if (!is.null(data.wl)) { run.val[data.wl] } else { NULL },
                                       data.lw = if (!is.null(data.lw)) { run.val[data.lw] } else { NULL },
                                       data.tp = if (!is.null(data.tp)) { run.val[data.tp] } else { NULL },
                                       data.miss = if (!is.null(data.miss)) { run.val[data.miss] } else { NULL },
                                       data.surv = if (!is.null(data.surv)) { run.val[data.surv] } else { NULL },
                                       data.coh = if (!is.null(data.coh)) { run.val[data.coh] } else { NULL },
                                       variable = if (!is.null(inpvariable)) { run.val[inpvariable] } else { NULL },
                                       define = if (!is.null(define)) { run.val[define] } else { NULL },
                                       analysis = if (!is.null(analysis)) { run.val[analysis] } else { NULL },
                                       model = if (!is.null(model)) { run.val[model] } else { NULL },
                                       mod.ind = if (!is.null(mod.ind)) { run.val[mod.ind] } else { NULL },
                                       mod.test = if (!is.null(mod.test)) { run.val[mod.test] } else { NULL },
                                       mod.prior = if (!is.null(mod.prior)) { run.val[mod.prior] } else { NULL },
                                       montecarlo = if (!is.null(montecarlo)) { run.val[montecarlo] } else { NULL },
                                       mod.pop = if (!is.null(mod.pop)) { run.val[mod.pop] } else { NULL },
                                       mod.cov = if (!is.null(mod.cov)) { run.val[mod.cov] } else { NULL },
                                       mod.miss = if (!is.null(mod.miss)) { run.val[mod.miss] } else { NULL },
                                       output = if (!is.null(inpoutput)) { run.val[inpoutput] } else { NULL },
                                       savedata = if (!is.null(savedata)) { run.val[savedata] } else { NULL },
                                       plot = if (!is.null(plot)) { run.val[plot] } else { NULL },
                                       message = if (!is.null(message)) { run.val[message] } else { NULL }),
                          # Output sections
                          result = list(summary.analysis = if (!is.null(summary.analysis)) { run.val[summary.analysis] } else { NULL },
                                        summary.analysis.short = if (!is.null(summary.analysis.short)) { run.val[summary.analysis.short] } else { NULL },
                                        summary.data = if (!is.null(summary.data)) { run.val[summary.data] } else { NULL },
                                        summary.data.short = if (!is.null(summary.data.short)) { run.val[summary.data.short] } else { NULL },
                                        prop.count = if (!is.null(prop.count)) { run.val[prop.count] } else { NULL },
                                        summary.censor = if (!is.null(summary.censor)) { run.val[summary.censor] } else { NULL },
                                        prop.zero = if (!is.null(prop.zero)) { run.val[prop.zero] } else { NULL },
                                        crosstab = if (!is.null(crosstab)) { run.val[crosstab] } else { NULL },
                                        summary.miss = if (!is.null(summary.miss)) { run.val[summary.miss] } else { NULL },
                                        coverage = if (!is.null(coverage)) { run.val[coverage] } else { NULL },
                                        basic = if (!is.null(basic)) { run.val[basic] } else { NULL },
                                        sample.stat = if (!is.null(sample.stat)) { run.val[sample.stat] } else { NULL },
                                        uni.sample.stat = if (!is.null(uni.sample.stat)) { run.val[uni.sample.stat] } else { NULL },
                                        random.starts = if (!is.null(random.starts)) { run.val[random.starts] } else { NULL },
                                        summary.fit = if (!is.null(summary.fit)) { run.val[summary.fit] } else { NULL },
                                        mod.est = if (!is.null(mod.est)) { run.val[mod.est] } else { NULL },
                                        fit = if (!is.null(fit)) { run.val[fit] } else { NULL },
                                        class.count = if (!is.null(class.count)) { run.val[class.count] } else { NULL },
                                        ind.means = if (!is.null(ind.means)) { run.val[ind.means] } else { NULL },
                                        trans.prob = if (!is.null(trans.prob)) { run.val[trans.prob] } else { NULL },
                                        classif = if (!is.null(classif)) { run.val[classif] } else { NULL },
                                        mod.result = if (!is.null(mod.result)) { run.val[mod.result] } else { NULL },
                                        odds.ratio = if (!is.null(odds.ratio)) { run.val[odds.ratio] } else { NULL },
                                        prob.scale = if (!is.null(prob.scale)) { run.val[prob.scale] } else { NULL },
                                        ind.odds.ratio = if (!is.null(ind.odds.ratio)) { run.val[ind.odds.ratio] } else { NULL },
                                        alt.param = if (!is.null(alt.param)) { run.val[alt.param] } else { NULL },
                                        irt.param = if (!is.null(irt.param)) { run.val[irt.param] } else { NULL },
                                        brant.wald = if (!is.null(brant.wald)) { run.val[brant.wald] } else { NULL },
                                        std.mod.result = if (!is.null(std.mod.result)) { run.val[std.mod.result] } else { NULL },
                                        rsquare = if (!is.null(rsquare)) { run.val[rsquare] } else { NULL },
                                        total.indirect = if (!is.null(total.indirect)) { run.val[total.indirect] } else { NULL },
                                        std.total.indirect = if (!is.null(std.total.indirect)) { run.val[std.total.indirect] } else { NULL },
                                        std.mod.result.cluster = if (!is.null(std.mod.result.cluster)) { run.val[std.mod.result.cluster] } else { NULL },
                                        fs.comparison = if (!is.null(fs.comparison)) { run.val[fs.comparison] } else { NULL },
                                        conf.mod.result = if (!is.null(conf.mod.result)) { run.val[conf.mod.result] } else { NULL },
                                        conf.std.conf = if (!is.null(conf.std.conf)) { run.val[conf.std.conf] } else { NULL },
                                        conf.total.indirect = if (!is.null(conf.total.indirect)) { run.val[conf.total.indirect] } else { NULL },
                                        conf.odds.ratio = if (!is.null(conf.odds.ratio)) { run.val[conf.odds.ratio] } else { NULL },
                                        modind = if (!is.null(modind)) { run.val[modind] } else { NULL },
                                        resid = if (!is.null(resid)) { run.val[resid] } else { NULL },
                                        logrank = if (!is.null(logrank)) { run.val[logrank] } else { NULL },
                                        tech1 = if (!is.null(tech1)) { run.val[tech1] } else { NULL },
                                        tech2 = if (!is.null(tech2)) { run.val[tech2] } else { NULL },
                                        tech3 = if (!is.null(tech3)) { run.val[tech3] } else { NULL },
                                        h1.tech3 = if (!is.null(h1.tech3)) { run.val[h1.tech3] } else { NULL },
                                        tech4 = if (!is.null(tech4)) { run.val[tech4] } else { NULL },
                                        tech5 = if (!is.null(tech5)) { run.val[tech5] } else { NULL },
                                        tech6 = if (!is.null(tech6)) { run.val[tech6] } else { NULL },
                                        tech7 = if (!is.null(tech7)) { run.val[tech7] } else { NULL },
                                        tech8 = if (!is.null(tech8)) { run.val[tech8] } else { NULL },
                                        tech9 = if (!is.null(tech9)) { run.val[tech9] } else { NULL },
                                        tech10 = if (!is.null(tech10)) { run.val[tech10] } else { NULL },
                                        tech11 = if (!is.null(tech11)) { run.val[tech11] } else { NULL },
                                        tech12 = if (!is.null(tech12)) { run.val[tech12] } else { NULL },
                                        tech13 = if (!is.null(tech13)) { run.val[tech13] } else { NULL },
                                        tech14 = if (!is.null(tech14)) { run.val[tech14] } else { NULL },
                                        tech15 = if (!is.null(tech15)) { run.val[tech15] } else { NULL },
                                        tech16 = if (!is.null(tech16)) { run.val[tech16] } else { NULL },
                                        svalues = if (!is.null(svalues)) { run.val[svalues] } else { NULL },
                                        stat.fscores = if (!is.null(stat.fscores)) { run.val[stat.fscores] } else { NULL },
                                        summary.fscores = if (!is.null(summary.fscores)) { run.val[summary.fscores] } else { NULL },
                                        pv = if (!is.null(pv)) { run.val[pv] } else { NULL },
                                        plotinfo = if (!is.null(plotinfo)) { run.val[plotinfo] } else { NULL },
                                        saveinfo = if (!is.null(saveinfo)) { run.val[saveinfo] } else { NULL }))

    # Remove "" from input commands
    return.object$input <- sapply(return.object$input, function(y) if (isTRUE(!is.null(y))) {

      if (isTRUE(misty::chr.trim(y[length(y)]) == "")) { y[-rev(seq_len(length(y)))[seq_len(rev(rle(misty::chr.trim(y))$length)[1L])]] } else { y }

    })

    # Remove "" from result sections
    return.object$result <- sapply(return.object$result, function(y) if (isTRUE(!is.null(y))) {

      if (isTRUE(misty::chr.trim(y[length(y)]) == "")) { y[-rev(seq_len(length(y)))[seq_len(rev(rle(misty::chr.trim(y))$length)[1L])]] } else { y }

    })

  #----------------------------------------
  # Mplus Output in misty object

  } else {

    return.object <- x$result

  }

  #_____________________________________________________________________________
  #
  # Print Object ---------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Print Input ####

  # Extract input commands
  input.object <- Filter(Negate(is.null), return.object$input[input])
  if (isTRUE(length(input.object) != 0L)) {

    # Format input commands
    input.object <- lapply(input.object, function(y) c(y, ""))

    # Remove last ""
    input.object[[length(input.object)]] <- input.object[[length(input.object)]][-length(input.object[[length(input.object)]])]

    # Paste input commands and attach header
    input.object <- c("INPUT INSTRUCTIONS\n\n", unname(unlist(sapply(input.object, function(y) paste(y, "\n")))))

  } else {

    input.object <- NULL

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Input Commands Not Requested  ####

  input.not <- setdiff(names(Filter(Negate(is.null), return.object$input)), input)

  #...................
  ### Format  ####

  input.not <- if (isTRUE(length(input.not) != 0L)) { paste0("\"", input.not, "\"") } else { NULL }

  if (isTRUE(!is.null(input.not))) {

    # Less than or equal 5 additional result sections
    if (isTRUE(length(input.not) <= 5L)) {

      input.not.print <- matrix(input.not, nrow = 1L)

    # Divisible by 5
    } else if (isTRUE(length(input.not) %% 5L == 0L)) {

      input.not.print <- apply(matrix(input.not, ncol = 5L, byrow = TRUE), 2L, function(y) format(y))

    # Not divisible by 5
    } else {

      input.not.print <- apply(matrix(c(input.not, rep("", times = (c(10L, 15L)[c(10L, 15L) > length(input.not)][1L] - length(input.not)))), ncol = 5L, byrow = TRUE), 2L, function(y) format(y))

    }

    input.not.print[, 1L] <- paste(" ", input.not.print[, 1L])

  } else {

    input.not.print <- NULL

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Print Result Sections ####

  #...................
  ### Empty summary.analysis or summary.data.short ####

  # If short version is empty, remove from result
  if (isTRUE(is.null(return.object$result$summary.analysis.short) && "summary.analysis.short" %in% result)) { result <- misty::chr.omit(result, omit = "summary.analysis.short", check = FALSE) }
  if (isTRUE(is.null(return.object$result$summary.data.short) && "summary.data.short" %in% result)) { result <- misty::chr.omit(result, omit = "summary.data.short", check = FALSE) }

  # Remove short version if long version is requested
  if (isTRUE(all(c("summary.analysis", "summary.analysis.short") %in% result))) { result <- misty::chr.omit(result, omit = "summary.analysis.short", check = FALSE) }
  if (isTRUE(all(c("summary.data", "summary.data.short")  %in% result))) { result <- misty::chr.omit(result, omit = "summary.data.short", check = FALSE) }

  # Extract result sections
  result.object <- Filter(Negate(is.null), return.object$result[result])
  if (isTRUE(length(result.object) != 0L)) {

    # Format result sections
    result.object <- lapply(result.object, function(y) c(y, ""))

    # Remove last ""
    result.object[[length(result.object)]] <- result.object[[length(result.object)]][-length(result.object[[length(result.object)]])]

    # Paste result sections
    result.object <- unname(unlist(sapply(result.object, function(y) paste(y, "\n"))))

  } else {

    result.object <- NULL

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Result Sections Not Requested ####

  #...................
  ### Extract not requested result sections ####

  result.not <- setdiff(names(Filter(Negate(is.null), return.object$result)), result)

  # Remove short version if long version is requested
  if (isTRUE("summary.analysis" %in% result && !is.null(return.object$result$summary.analysis.short))) { result.not <- misty::chr.omit(result.not, omit = "summary.analysis.short", check = FALSE) }
  if (isTRUE("summary.data" %in% result && !is.null(return.object$result$summary.data.short))) { result.not <- misty::chr.omit(result.not, omit = "summary.data.short", check = FALSE) }

  #...................
  ### Format ####

  result.not <- if (isTRUE(length(result.not) != 0L)) { paste0("\"", result.not, "\"") } else { NULL }

  if (isTRUE(!is.null(result.not))) {

    # Less than or equal 5 additional result sections
    if (isTRUE(length(result.not) <= 5L)) {

      result.not.print <- matrix(result.not, nrow = 1L)

    # Divisible by 5
    } else if (isTRUE(length(result.not) %% 5L == 0L)) {

      result.not.print <- apply(matrix(result.not, ncol = 5L, byrow = TRUE), 2L, function(y) format(y))

    # Not divisible by 5
    } else {

      result.not.print <- apply(matrix(c(result.not, rep("", times = (seq(10L, 65L, by = 5L)[seq(10L, 65L, by = 5L) > length(result.not)][1L] - length(result.not)))), ncol = 5L, byrow = TRUE), 2L, function(y) format(y))

    }

    result.not.print[, 1L] <- paste(" ", result.not.print[, 1L])

  } else {

    result.not.print <- NULL

  }

  #_____________________________________________________________________________
  #
  # Print Input/Output ---------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Print Input Commands and Result Sections ####

  if (isTRUE(output)) {

    #...................
    ### ERROR in Output ####

    if (isTRUE(any(grepl("\\*\\*\\* ERROR", return.object$input$message)) || any(grepl("\\*\\*\\* FATAL ERROR", return.object$input$message)) )) {

      print <- "input"
      not.result <- not.input <- FALSE

    }

    #...................
    ### Print Input Commands ####

    if (isTRUE("input" %in% print)) {

      # Mplus Version
      cat(return.object$mplus, "\n\n")

      # Print input commands
      if (isTRUE(!is.null(input.object))) { cat(input.object) }

      # Print not requested input commands
      if (isTRUE(not.input)) {

        if (isTRUE(!is.null(input.not.print))) {

          if (isTRUE(length(input.not.print) == 1L)) { cat("\n Not Requested Input Command:\n") } else { cat("\n Not Requested Input Commands:\n") }

          write.table(input.not.print, quote = FALSE, row.names = FALSE, col.names = FALSE, na = "")

        } else {

          cat("\n Not Requested Input Commands: None\n")

        }

      }

    }

    #...................
    ### Result Sections ####

    if (isTRUE("result" %in% print)) {

      # Print result sections
      if (isTRUE(!is.null(result.object))) {

        if (isTRUE("input" %in% print)) { cat("\n") }

        cat(result.object)

      }

    }

    # Print not requested result sections
    if (isTRUE(not.result)) {

      if (isTRUE(!is.null(result.not.print))) {

        if (isTRUE(length(result.not.print) == 1L)) { cat("\n Not Requested Result Section:\n") } else { cat("\n Not Requested Result Sections:\n") }

        write.table(result.not.print, quote = FALSE, row.names = FALSE, col.names = FALSE, na = "")

      } else {

        cat("\n Not Requested Result Sections: None\n")

      }

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Write Results into a text file ####
  #
  # Note that write.table() function does not display results on the console even
  # when sink(write, split = TRUE)

  if (isTRUE(!is.null(write))) {

    # Send R Output to a file
    sink(file = write, append = ifelse(isTRUE(file.exists(write)), append, FALSE), type = "output")

    # Append output
    if (isTRUE(append && file.exists(write))) { write("", file = write, append = TRUE) }

    #...................
    ### Print Input Commands ####

    if (isTRUE("input" %in% print)) {

      # Mplus Version
      cat(return.object$mplus, "\n\n")

      # Print input commands
      if (isTRUE(!is.null(input.object))) { cat(input.object) }

      # Print not requested input commands
      if (isTRUE(not.input)) {

        if (isTRUE(!is.null(input.not.print))) {

          if (isTRUE(length(input.not.print) == 1L)) { cat("\n Not Requested Input Command:\n") } else { cat("\n Not Requested Input Commands:\n") }

          write.table(input.not.print, quote = FALSE, row.names = FALSE, col.names = FALSE, na = "")

        } else {

          cat("\n Not Requested Input Commands: None\n")

        }

      }

    }

    #...................
    ### Result Sections ####

    if (isTRUE("result" %in% print)) {

      # Print result sections
      if (isTRUE(!is.null(result.object))) {

        if (isTRUE("input" %in% print)) { cat("\n") }

        cat(result.object)

      }

    }

    # Print not requested result sections
    if (isTRUE(not.result)) {

      if (isTRUE(!is.null(result.not.print))) {

        if (isTRUE(length(result.not.print) == 1L)) { cat("\n Not Requested Result Section:\n") } else { cat("\n Not Requested Result Sections:\n") }

        write.table(result.not.print, quote = FALSE, row.names = FALSE, col.names = FALSE, na = "")

      } else {

        cat("\n Not Requested Result Sections: None\n")

      }

    }

    # Close file connection
    sink()

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "mplus",
                 x = x,
                 args = list(print = print, input = input, result = result,
                             exclude = exclude, variable = variable,
                             not.input = not.input, not.result = not.result,
                             write = write, append = append, check = check,
                             output = output),
                 print = list(input = input.object, result = result.object),
                 notprint = list(input = input.not, result = result.not),
                 result = return.object)

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  return(invisible(object))

}

#_______________________________________________________________________________
