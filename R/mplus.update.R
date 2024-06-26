#' Mplus Input Updating
#'
#' This function updates specific input command sections of a \code{misty.object}
#' of type \code{mplus} to create an updated Mplus input file, run the updated
#' input file by using the \code{mplus.run()} function, and print the updated Mplus
#' output file by using the \code{mplus.print()} function.
#'
#' @param x           \code{misty.object} object of type \code{mplus}.
#' @param update      a character vector containing the updated input command
#'                    sections.
#' @param file        a character string indicating the name of the updated Mplus
#'                    input file with or without the file extension \code{.inp},
#'                    e.g., \code{"Mplus_Input_Update.inp"} or \code{"Mplus_Input_Update"}.
#' @param replace.inp logical: if \code{TRUE} (default), an existing input
#'                    file will be replaced.
#' @param mplus.run   logical: if \code{TRUE}, the input file specified in the
#'                    argument \code{file} containing the input text specified
#'                    in the argument \code{x} is run using the \code{mplus.run}
#'                    function.
#' @param show.out    logical: if \code{TRUE}, estimation output (\code{TECH8})
#'                    is show on the R console. Note that if run within Rgui,
#'                    output will display within R, but if run via Rterm, a
#'                    separate window will appear during estimation.
#' @param replace.out a character string for specifying three settings:
#'                    \code{"always"} (default), which runs all models, regardless
#'                    of whether an output file for the model exists, \code{"never"},
#'                    which does not run any model that has an existing output file,
#'                    and \code{"modified"}, which only runs a model if the
#'                    modified date for the input file is more recent than the
#'                    output file modified date.
#' @param print       a character vector indicating which results to show, i.e.
#'                    \code{"all"} (default) for all results \code{"input"} for
#'                    input command sections, and \code{"result"} for result sections.
#' @param input       a character vector specifiying Mplus input command sections
#'                    included in the output (see 'Details' in the \code{\link{mplus.print}}
#'                    function).
#' @param result      a character vector specifiying Mplus result sections included
#'                    in the output (see 'Details' in the \code{\link{mplus.print}}
#'                    function).
#' @param exclude     a character vector specifiying Mplus input command or result
#'                    sections excluded from the output (see 'Details' in the
#'                    \code{\link{mplus.print}} function).
#' @param variable    logical: if \code{TRUE}, names of the variables in the data
#'                    set (\code{NAMES ARE}) specified in the \code{VARIABLE:}
#'                    command section are shown. By default, names of the variables
#'                    in the data set are excluded from the output unless all variables
#'                    are used in the analysis (i.e., no \code{USEVARIABLES} command
#'                    specified in the Mplus input file).
#' @param not.input   logical: if \code{TRUE} (default), character vector indicating
#'                    the input commands not requested are shown on the console.
#' @param not.result  logical: if \code{TRUE} (default), character vector indicating
#'                    the result sections not requested are shown on the console.
#' @param write       a character string naming a file for writing the output into
#'                    a text file with file extension \code{".txt"} (e.g.,
#'                    \code{"Output.txt"}).
#' @param append      logical: if \code{TRUE} (default), output will be appended
#'                    to an existing text file with extension \code{.txt} specified
#'                    in \code{write}, if \code{FALSE} existing text file will be
#'                    overwritten.
#' @param check       logical: if \code{TRUE} (default), argument specification is
#'                    checked.
#' @param output      logical: if \code{TRUE} (default), output is shown on the
#'                    console by using the function \code{mplus.print}.
#'
#' @details
#'\describe{
#'  \item{\strong{The \code{...} Specification}}{The \code{...} Specification
#' can be used to update specific options in the \code{VARIABLE} and \code{ANALYSIS}
#' section, while keeping all other options in the \code{misty.object} of type
#' \code{mplus} specified in the argument \code{x}. Note that the \code{...}
#' specification is only available for the \code{VARIABLE} and \code{ANALYSIS} section.}
#' \item{\strong{The \code{---;} Specification}}{can be used to remove entire sections
#' (e.g., \code{OUTPUT: ---;}) or options within the \code{VARIABLE:} and \code{ANALYSIS:}
#' section (e.g., \code{ANALYSIS: ESTIMATOR IS ---;}) from the Mplus input.
#' \item{\strong{Comments in the Mplus Input}}{Comments in the Mplus Input can cause
#' problems when following keywords in uppercase, lower case, or mixed upper and lower
#' case letters are involved in the comments of the \code{VARIABLE} or \code{ANALYSIS}
#' section:
#' \itemize{
#'  \item{\code{VARIABLE} section}: \code{"NAMES", "USEOBSERVATIONS", "USEVARIABLES",
#'         "MISSING", "CENSORED", "CATEGORICAL", "NOMINAL", "COUNT", "DSURVIVAL", "GROUPING",
#'         "IDVARIABLE", "FREQWEIGHT", "TSCORES", "AUXILIARY", "CONSTRAINT", "PATTERN",
#'         "STRATIFICATION", "CLUSTER", "WEIGHT", "WTSCALE", "BWEIGHT", "B2WEIGHT",
#'         "B3WEIGHT", "BWTSCALE", "REPWEIGHTS", "SUBPOPULATION", "FINITE", "CLASSES",
#'         "KNOWNCLASS", "TRAINING", "WITHIN", "BETWEEN", "SURVIVAL", "TIMECENSORED",
#'         "LAGGED"}, or \code{"TINTERVAL"}.
#'  \item{\code{ANALYSIS} section}: \code{"TYPE", "ESTIMATOR", "MODEL", "ALIGNMENT",
#'        "DISTRIBUTION", "PARAMETERIZATION", "LINK", "ROTATION", "ROWSTANDARDIZATION",
#'        "PARALLEL", "REPSE", "BASEHAZARD", "CHOLESKY", "ALGORITHM", "INTEGRATION",
#'        "MCSEED", "ADAPTIVE", "INFORMATION", "BOOTSTRAP", "LRTBOOTSTRAP", "STARTS",
#'        "STITERATIONS", "STCONVERGENCE", "STSCALE", "STSEED", "OPTSEED", "K-1STARTS",
#'        "LRTSTARTS", "RSTARTS", "ASTARTS", "H1STARTS", "DIFFTEST", "MULTIPLIER",
#'        "COVERAGE", "ADDFREQUENCY", "ITERATIONS", "SDITERATIONS", "H1ITERATIONS",
#'        "MITERATIONS", "MCITERATIONS", "MUITERATIONS", "RITERATIONS", "AITERATIONS",
#'        "CONVERGENCE", "H1CONVERGENCE", "LOGCRITERION", "RLOGCRITERION", "MCONVERGENCE",
#'        "MCCONVERGENCE", "MUCONVERGENCE", "RCONVERGENCE", "ACONVERGENCE", "MIXC",
#'        "MIXU", "LOGHIGH", "LOGLOW", "UCELLSIZE", "VARIANCE", "SIMPLICITY", "TOLERANCE",
#'        "METRIC", "MATRIX", "POINT", "CHAINS", "BSEED", "STVALUES", "PREDICTOR",
#'        "ALGORITHM", "BCONVERGENCE", "BITERATIONS", "FBITERATIONS", "THIN",
#'        "MDITERATIONS", "KOLMOGOROV", "PRIOR", "INTERACTIVE"}, or \code{"PROCESSORS"}.
#' }
#' Note that it is recommended to remove all comments in the \code{VARIABLE} and
#' \code{ANALYSIS} section when the function crashes.
#'}
#'}
#'
#' @author
#' Takuya Yanagida
#'
#' @seealso
#' \code{\link{read.mplus}}, \code{\link{write.mplus}}, \code{\link{mplus.print}},
#' \code{\link{mplus}}, \code{\link{mplus.run}}, \code{\link{mplus.lca}}
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
#' \item{\code{x}}{a character vector containing the Mplus input text}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{input}}{list with input command sections}
#' \item{\code{write}}{write command sections}
#' \item{\code{result}}{list with input command sections (\code{input}) and
#'                      result sections}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #----------------------------------------------------------------------------
#' # Example 1: Update VARIABLE and MODEL section
#'
#' # Write Mplus Data File
#' write.mplus(ex3_1, file = "ex3_1.dat")
#'
#' # Specify Mplus input
#' input <- '
#' DATA:     FILE IS ex3_1.dat;
#' VARIABLE: NAMES ARE y1 x1 x3;
#' MODEL:    y1 ON x1 x3;
#' OUTPUT:   SAMPSTAT;
#' '
#'
#' # Run Mplus input
#' mod0 <- mplus(input, file = "ex3_1.inp")
#'
#' # Update VARIABLE and MODEL section
#' update1 <- '
#' VARIABLE: ...
#'           USEVARIABLES ARE y1 x1;
#' MODEL:    y1 ON x1;
#' '
#'
#' # Run updated Mplus input
#' mod1 <- mplus.update(mod1, update1, file = "ex3_1_update1.inp")
#'
#' #----------------------------------------------------------------------------
#' # Example 2: Update ANALYSIS section
#'
#' # Update ANALYSIS section
#' update2 <- '
#' ANALYSIS: ESTIMATOR IS MLR;
#' '
#'
#' # Run updated Mplus input
#' mod2 <- mplus.update(mod2, update2, file = "ex3_1_update2.inp")
#'
#' #----------------------------------------------------------------------------
#' # Example 3: Remove OUTPUT section
#'
#' #' Remove OUTPUT section
#' update3 <- '
#' OUTPUT: ---;
#' '
#'
#' # Run updated Mplus input
#' mod3 <- mplus.update(mod3, update3, file = "ex3_1_update3.inp")
#' }
mplus.update <- function(x, update, file = "Mplus_Input_Update.inp", replace.inp = TRUE, mplus.run = TRUE,
                         show.out = FALSE, replace.out = c("always", "never", "modified"),
                         print = c("all", "input", "result"),
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

  # Check if input 'x' is a misty.object
  if (isTRUE(class(x) != "misty.object")) { stop("Please specify a misty.object for the argument 'x'", call. = FALSE) }

  # Check if input 'x' is a misty.object
  if (isTRUE(x$type != "mplus")) { stop("Please specify a misty.object of type 'mplus' for the argument 'x'", call. = FALSE) }

  # Check if input 'update' is not missing
  if (isTRUE(missing(update))) { stop("Please specify a character vector for the argument 'update'", call. = FALSE) }

  # Check if input 'update' is NULL
  if (isTRUE(is.null(update))) { stop("Input specified for the argument 'update' is NULL.", call. = FALSE) }

  # Check if input 'update' is a character string
  if (isTRUE(!is.character(update) || length(update) != 1L)) { stop("Please specify a character string indicating the updated Mplus input text for the argument 'update'", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  # All input options
  input.all <- c("title", "data", "data.imp", "data.wl", "data.lw", "data.tp", "data.miss", "data.surv", "data.coh", "variable", "define", "analysis", "model", "mod.ind", "mod.test", "mod.prior", "montecarlo", "mod.pop", "mod.cov", "mod.miss", "inpoutput", "savedata", "plot", "output", "message")

  # All result options
  result.all <- c("summary.analysis", "summary.analysis.short", "summary.data", "summary.data.short", "prop.count", "summary.censor", "prop.zero", "crosstab", "summary.miss", "coverage", "basic", "sample.stat", "uni.sample.stat", "random.starts", "summary.fit", "mod.est", "fit", "class.count", "ind.means", "trans.prob", "classif", "mod.result", "odds.ratio", "prob.scale", "ind.odds.ratio", "alt.param", "irt.param", "brant.wald", "std.mod.result", "rsquare", "total.indirect", "std.total.indirect", "std.mod.result.cluster", "fs.comparison", "conf.mod.result", "conf.std.conf", "conf.total.indirect", "conf.odds.ratio", "modind", "resid", "logrank", "tech1", "tech2", "tech3", "h1.tech3", "tech4", "tech5", "tech6", "tech7", "tech8", "tech9", "tech10", "tech11", "tech12", "tech13", "tech14", "tech15", "tech16", "svalues", "stat.fscores", "summary.fscores", "pv", "plotinfo", "saveinfo")

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # Check input 'file'
    if (isTRUE(!is.character(file) || length(file) != 1L)) { stop("Please specify a character string for the argument 'file',", call. = FALSE) }

    # Check input 'replace.inp'
    if (isTRUE(!is.logical(replace.inp))) { stop("Please specify TRUE or FALSE for the argument 'replace.inp'.", call. = FALSE) }

    # Check input 'mplus.run'
    if (isTRUE(!is.logical(mplus.run))) { stop("Please specify TRUE or FALSE for the argument 'mplus.run'.", call. = FALSE) }

    # Check input 'replace.out'
    if (isTRUE(!all(replace.out %in% c("always", "never", "modified")))) { stop("Character strings in the argument 'print' do not all match with \"always\", \"never\", or \"modified\".", call. = FALSE) }

    # Check input 'print'
    if (isTRUE(!all(print %in% c("all", "input", "result")))) { stop("Character strings in the argument 'print' do not all match with \"all\", \"input\", or \"result\".", call. = FALSE) }

    # Check input 'input'
    input.check <- input[which(!input %in% c("all", "default", input.all))]
    if (isTRUE(length(input.check) != 0L)) { stop(paste0(if (isTRUE(length(input.check) == 1L)) { "Character string " } else { "Character vector " }, "specified in the argument 'input' is not permissible: ", paste(dQuote(input.check), collapse = ", ")), call. = FALSE) }

    # Check input 'result'
    result.check <- result[which(!result %in% c("all", "default", result.all))]
    if (isTRUE(length(result.check) != 0L)) { stop(paste0(if (isTRUE(length(result.check) == 1L)) { "Character string " } else { "Character vector " }, "specified in the argument 'result' is not permissible: ", paste(dQuote(result.check), collapse = ", ")), call. = FALSE) }

    # Check input 'exclude'
    exclude.check <- exclude[which(!exclude %in% c(input.all, result.all))]
    if (isTRUE(length(exclude.check) != 0L)) { stop(paste0(if (isTRUE(length(exclude.check) == 1L)) { "Character string " } else { "Character vector " }, "specified in the argument 'exclude' is not permissible: ", paste(dQuote(exclude.check), collapse = ", ")), call. = FALSE) }

    # Check input 'variable'
    if (isTRUE(!is.logical(variable))) { stop("Please specify TRUE or FALSE for the argument 'variable'.", call. = FALSE) }

    # Check input 'not.input'
    if (isTRUE(!is.logical(not.input))) { stop("Please specify TRUE or FALSE for the argument 'not.input'.", call. = FALSE) }

    # Check input 'not.result'
    if (isTRUE(!is.logical(not.result))) { stop("Please specify TRUE or FALSE for the argument 'not.result'.", call. = FALSE) }

    # Check input 'write'
    if (isTRUE(!is.null(write) && !is.character(write))) { stop("Please specify a character string for the argument 'write'.", call. = FALSE) }

    # Check input 'append'
    if (isTRUE(!is.logical(append))) { stop("Please specify TRUE or FALSE for the argument 'append'.", call. = FALSE) }

    # Check input 'output'
    if (isTRUE(!is.logical(output))) { stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## file Argument ####

  # File extension .inp
  file <- ifelse(isTRUE(!grepl(".inp", file)), file <- paste0(file, ".inp"), file)

  # .out object
  file.out <- sub(".inp", ".out", file)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## replace.out Argument ####

  if (isTRUE(all(c("always", "never", "modified") %in% replace.out))) {

    replace.out <- "always"

  } else {

    if (isTRUE(length(replace.out) != 1L)) { stop("Please specify a character string for the argument 'replace.out'", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Extract Position of Input Command Sections ####

  section.pos <- c(if (isTRUE(any(grepl("TITLE:", update)))) { as.numeric(gregexec("TITLE:", toupper(update))[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("DATA:", update)))) { as.numeric(gregexec("DATA:", toupper(update))[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("DATA IMPUTATION:", update)))) { as.numeric(gregexec("DATA IMPUTATION:", toupper(update))[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("DATA WIDETOLONG:", update)))) { as.numeric(gregexec("DATA WIDETOLONG:", toupper(update))[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("DATA LONGTOWIDE:", update)))) { as.numeric(gregexec("DATA LONGTOWIDE:", toupper(update))[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("DATA TWOPART:", update)))) { as.numeric(gregexec("DATA TWOPART:", toupper(update))[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("DATA MISSING:", update)))) { as.numeric(gregexec("DATA MISSING:", toupper(update))[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("DATA SURVIVAL:", update)))) { as.numeric(gregexec("DATA SURVIVAL:", toupper(update))[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("DATA COHORT:", update)))) { as.numeric(gregexec("DATA COHORT:", toupper(update))[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("VARIABLE:", update)))) { as.numeric(gregexec("VARIABLE:", toupper(update))[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("DEFINE:", update)))) { as.numeric(gregexec("DEFINE:", toupper(update))[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("ANALYSIS:", update)))) { as.numeric(gregexec("ANALYSIS:", toupper(update))[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("MODEL:", update)))) { as.numeric(gregexec("MODEL:", toupper(update))[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("MODEL INDIRECT:", update)))) { as.numeric(gregexec("MODEL INDIRECT:", toupper(update))[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("MODEL TEST:", update)))) { as.numeric(gregexec("MODEL TEST:", toupper(update))[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("MODEL PRIORS:", update)))) { as.numeric(gregexec("MODEL PRIORS:", toupper(update))[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("MONTECARLO:", update)))) { as.numeric(gregexec("MONTECARLO:", toupper(update))[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("MODEL POPULATION:", update)))) { as.numeric(gregexec("MODEL POPULATION:", toupper(update))[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("MODEL COVERAGE:", update)))) { as.numeric(gregexec("MODEL COVERAGE:", toupper(update))[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("MODEL MISSING:", update)))) { as.numeric(gregexec("MODEL MISSING:", toupper(update))[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("OUTPUT:", update)))) { as.numeric(gregexec("OUTPUT:", toupper(update))[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("SAVEDATA:", update)))) { as.numeric(gregexec("SAVEDATA:", toupper(update))[[1L]]) } else { NULL })

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Extract Input Command Sections ####

  title <- data <- data.imp <- dat.wl <- data.lw <- data.tp <- data.miss <- data.surv <- data.coh <- inpvariable <- define <- analysis <- model <- model.ind <- model.test <- model.prior <- montecarlo <- model.pop <- model.cov <- model.miss <- inpoutput <- savedata <- plot <- NULL

  #...................
  ### TITLE ####

  if (isTRUE(any(grepl("TITLE:", update)))) { title <- .extract.section("TITLE:", update, section.pos) }

  #...................
  ### DATA ####

  if (isTRUE(any(grepl("DATA:", update)))) { data <- .extract.section("DATA:", update, section.pos) }

  #...................
  ### DATA IMPUTATION ####

  if (isTRUE(any(grepl("DATA IMPUTATION:", update)))) { data.imp <- .extract.section("DATA IMPUTATION:", update, section.pos) }

  #...................
  ### DATA WIDETOLONG ####

  if (isTRUE(any(grepl("DATA WIDETOLONG:", update)))) { data.wl <- .extract.section("DATA WIDETOLONG:", update, section.pos) }

  #...................
  ### DATA LONGTOWIDE ####

  if (isTRUE(any(grepl("DATA LONGTOWIDE:", update)))) { data.lw <- .extract.section("DATA LONGTOWIDE:", update, section.pos) }

  #...................
  ### DATA TWOPART ####

  if (isTRUE(any(grepl("DATA TWOPART:", update)))) { data.tp <- .extract.section("DATA TWOPART:", update, section.pos) }

  #...................
  ### DATA MISSING ####

  if (isTRUE(any(grepl("DATA MISSING:", update)))) { data.miss <- .extract.section("DATA MISSING:", update, section.pos) }

  #...................
  ### DATA SURVIVAL ####

  if (isTRUE(any(grepl("DATA SURVIVAL:", update)))) { data.surv <- .extract.section("DATA SURVIVAL:", update, section.pos) }

  #...................
  ### DATA COHORT ####

  if (isTRUE(any(grepl("DATA COHORT:", update)))) { data.coh <- .extract.section("DATA COHORT:", update, section.pos) }

  #...................
  ### VARIABLE ####

  if (isTRUE(any(grepl("VARIABLE:", update)))) { inpvariable <- .extract.section("VARIABLE:", update, section.pos) }

  #...................
  ### DEFINE ####

  if (isTRUE(any(grepl("DEFINE:", update)))) { define <- .extract.section("DEFINE:", update, section.pos) }

  #...................
  ### ANALYSIS ####

  if (isTRUE(any(grepl("ANALYSIS:", update)))) { analysis <- .extract.section("ANALYSIS:", update, section.pos) }

  #...................
  ### MODEL ####

  if (isTRUE(any(grepl("MODEL:", update)))) { model <- .extract.section("MODEL:", update, section.pos) }

  #...................
  ### MODEL INDIRECT ####

  if (isTRUE(any(grepl("MODEL INDIRECT:", update)))) { model.ind <- .extract.section("MODEL INDIRECT:", update, section.pos) }

  #...................
  ### MODEL TEST ####

  if (isTRUE(any(grepl("MODEL TEST:", update)))) { model.test <- .extract.section("MODEL TEST:", update, section.pos) }

  #...................
  ### MODEL PRIORS ####

  if (isTRUE(any(grepl("MODEL PRIORS:", update)))) { model.prior <- .extract.section("MODEL PRIORS:", update, section.pos) }

  #...................
  ### MONTECARLO ####

  if (isTRUE(any(grepl("MONTECARLO:", update)))) { montecarlo <- .extract.section("MONTECARLO:", update, section.pos) }

  #...................
  ### MODEL POPULATION ####

  if (isTRUE(any(grepl("MODEL POPULATION:", update)))) { model.pop <- .extract.section("MODEL POPULATION:", update, section.pos) }

  #...................
  ### MODEL COVERAGE ####

  if (isTRUE(any(grepl("MODEL COVERAGE:", update)))) { model.cov <- .extract.section("MODEL COVERAGE:", update, section.pos) }

  #...................
  ### MODEL MISSING ####

  if (isTRUE(any(grepl("MODEL MISSING:", update)))) { model.miss <- .extract.section("MODEL MISSING:", update, section.pos) }

  #...................
  ### OUTPUT ####

  if (isTRUE(any(grepl("OUTPUT:", update)))) { inpoutput <- .extract.section("OUTPUT:", update, section.pos) }

  #...................
  ### SAVEDATA ####

  if (isTRUE(any(grepl("SAVEDATA:", update)))) { savedata <- .extract.section("SAVEDATA:", update, section.pos) }

  #...................
  ### PLOT ####

  if (isTRUE(any(grepl("PLOT:", update)))) { plot <- .extract.section("PLOT:", update, section.pos) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Updated Input Object ####

  input.object.update <- Filter(Negate(is.null),
                                list(title = title,
                                     data = data, data.imp = data.imp, dat.wl = dat.wl, data.lw = data.lw, data.tp = data.tp, data.miss = data.miss, data.surv = data.surv, data.coh = data.coh,
                                     variable = inpvariable, define = define, analysis = analysis, model = model, model.ind = model.ind, model.test = model.test, model.prior = model.prior,
                                     montecarlo = montecarlo, model.pop = model.pop, model.cov = model.cov, model.miss = model.miss,
                                     output = inpoutput, savedata = savedata, plot = plot))

  # Input object
  input.object <- Filter(Negate(is.null), x$input)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Update Input Object ####

  for (i in names(input.object.update)) {

    #...................
    ### Update section available in input ####

    if (isTRUE(i %in% names(input.object))) {

      #### VARIABLE section ####
      if (isTRUE(i == "variable")) {

        ##### ... Specification ####
        if (isTRUE(grepl("...", input.object.update[[i]], fixed = TRUE))) {

          # VARIABLE subsections
          subsection <- c("NAMES", "USEOBSERVATIONS", "USEVARIABLES", "MISSING", "CENSORED", "CATEGORICAL", "NOMINAL", "COUNT", "DSURVIVAL", "GROUPING", "IDVARIABLE", "FREQWEIGHT", "TSCORES", "AUXILIARY", "CONSTRAINT", "PATTERN", "STRATIFICATION", "CLUSTER", "WEIGHT", "WTSCALE", "BWEIGHT", "B2WEIGHT", "B3WEIGHT", "BWTSCALE", "REPWEIGHTS", "SUBPOPULATION", "FINITE", "CLASSES", "KNOWNCLASS", "TRAINING", "WITHIN", "BETWEEN", "SURVIVAL", "TIMECENSORED", "LAGGED", "TINTERVAL")
          subsection <- subsection[misty::chr.grepl(subsection, toupper(input.object.update[[i]]))]

          ##### Update VARIABLE section
          for (j in subsection) { input.object[[i]] <- .variable.section(j, input.object[[i]], input.object.update[[i]]) }

        ##### No ... Specification ####
        } else {

          input.object[[i]] <- input.object.update[[i]]

        }

      #### ANALYSIS section ####
      } else if (isTRUE(i == "analysis")) {

        ##### ... Specification ####
        if (isTRUE(grepl("...", input.object.update[[i]], fixed = TRUE))) {

          ###### ANALYSIS subsections ####
          subsection <- c("TYPE", "ESTIMATOR", "MODEL", "ALIGNMENT", "DISTRIBUTION", "PARAMETERIZATION", "LINK", "ROTATION", "ROWSTANDARDIZATION", "PARALLEL", "REPSE", "BASEHAZARD", "CHOLESKY", "ALGORITHM", "INTEGRATION", "MCSEED", "ADAPTIVE", "INFORMATION", "BOOTSTRAP", "LRTBOOTSTRAP", "STARTS", "STITERATIONS", "STCONVERGENCE", "STSCALE", "STSEED", "OPTSEED", "K-1STARTS", "LRTSTARTS", "RSTARTS", "ASTARTS", "H1STARTS", "DIFFTEST", "MULTIPLIER", "COVERAGE", "ADDFREQUENCY", "ITERATIONS", "SDITERATIONS", "H1ITERATIONS", "MITERATIONS", "MCITERATIONS", "MUITERATIONS", "RITERATIONS", "AITERATIONS", "CONVERGENCE", "H1CONVERGENCE", "LOGCRITERION", "RLOGCRITERION", "MCONVERGENCE", "MCCONVERGENCE", "MUCONVERGENCE", "RCONVERGENCE", "ACONVERGENCE", "MIXC", "MIXU", "LOGHIGH", "LOGLOW", "UCELLSIZE", "VARIANCE", "SIMPLICITY", "TOLERANCE", "METRIC", "MATRIX", "POINT", "CHAINS", "BSEED", "STVALUES", "PREDICTOR", "ALGORITHM", "BCONVERGENCE", "BITERATIONS", "FBITERATIONS", "THIN", "MDITERATIONS", "KOLMOGOROV", "PRIOR", "INTERACTIVE", "PROCESSORS")
          subsection <- subsection[chr.grepl(subsection, toupper(input.object.update[[i]]))]

          ###### Update ANALYSIS section ####
          for (j in subsection) { input.object[[i]] <- .variable.section(j, input.object[[i]], input.object.update[[i]]) }

        ##### No ... Specification ####
        } else {

          input.object[[i]] <- input.object.update[[i]]

        }

      #...................
      ### No VARIABLE or ANALYSIS section ####

      } else {

        input.object[[i]] <- input.object.update[[i]]

      }

    #...................
    ### Update section not available in input ####
    } else {

      input.object <- within(input.object, assign(i, input.object.update[[i]]))

      # Order subsection
      section <- c("title", "data", "data.imp", "dat.wl", "data.lw", "data.tp", "data.miss", "data.surv", "data.coh", "variable", "define", "analysis", "model", "model.ind", "model.test", "model.prior", "montecarlo", "model.pop", "model.cov", "model.miss", "output", "savedata", "plot")
      input.object <- input.object[section[section %in% names(input.object)]]

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Remove sections using --- ####

    if (isTRUE(grepl("---", input.object[[i]], fixed = TRUE))) {

      # Colon or Semicolon position
      semicol <- misty::chr.grep(c(":", ";"), unlist(strsplit(input.object[[i]], "")))

      # --- positions
      rempos <- as.numeric(unlist(gregexec("---", input.object[[i]])))

      # Removal position
      start.end <- unlist(sapply(rempos, function(y) {

        # Start and end position of the removal option
        return((rev(semicol[which(semicol < y)])[1L] + 1L):(semicol[which(semicol > y)][1L]))

      }))

      # Remove section
      input.object[[i]] <- paste(unlist(strsplit(input.object[[i]], ""))[-start.end], collapse = "")

      #...................
      ### Remove empty section ####

      if (isTRUE(misty::chr.trim(toupper(input.object[[i]])) %in% c("TITLE:", "DATA:", "DATA IMPUTATION", "DATA WIDETOLONG:", "DATA LONGTOWIDE:", "DATA TWOPART:", "DATA MISSING:", "DATA SURVIVAL:", "DATA COHORT:", "VARIABLE:", "DEFINE:", "ANALYSIS:", "MODEL:", "MODEL INDIRECT:", "MODEL TEST:", "MODEL PRIORS:", "MONTECARLO:", "MODEL POPULATION:", "MODEL COVERAGE:", "MODEL MISSING:", "OUTPUT:", "SAVEDATA:"))) { input.object[[i]] <- NULL }

      #...................
      ### Remove new lines before first entry ####

      colon <- as.numeric(unlist(gregexec(":", input.object[[i]])))
      newline <- as.numeric(unlist(gregexec("\n|\t", input.object[[i]])))
      letter <- as.numeric(unlist(gregexec("[A-Za-z]", input.object[[i]])))

      rempos <- newline[newline < letter[letter > colon][1L]]
      if (isTRUE(length(rempos) != 0L)) { input.object[[i]] <- paste(unlist(strsplit(input.object[[i]], ""))[-rempos], collapse = "") }

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Write Object ####

  write.object <- misty::chr.gsub(c("\n\n\n", "\n\n\n\n", "\n\n\n\n\n", "\n\n\n\n\n\n", "\n\n\n\n\n\n\n"), rep("\n\n", times = 5L), paste(input.object, collapse = "\n\n"))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Write Input ####

  # Input file already exists
  if (isTRUE(file.exists(file))) {

    # Replace input file
    if (isTRUE(replace.inp)) {

      writeLines(write.object, file)

    # Do not replace input file
    } else {

      stop(paste0("Input file ", dQuote(file), " already exists, please specify 'replace.inp = TRUE' to replace existing input file."), call. = FALSE)

    }

  # Input file does not exists
  } else {

    writeLines(write.object, file)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Run Mplus ####

  if (isTRUE(mplus.run)) {

    # Existing input file
    if (isTRUE(file.exists(file))) {

      # Do not replace output file
      if (isTRUE(replace.out == "never" && file.exists(file.out))) { stop(paste0("Output file ", dQuote(file.out), " already exists, please specify 'replace.out = \"always\"' to replace existing output file."), call. = FALSE) }

      cat("Running Model:", file, "\n")

      misty::mplus.run(file, recursive = FALSE, Mplus = "Mplus", replace.out = replace.out, message = FALSE, show.out = show.out)

      if (isTRUE(output)) { cat("\n") }

    # Input file does not exist
    } else {

      stop(paste0("Input file ", dQuote(file), " does not exist."), call. = FALSE)

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Mplus Output ####

  if (isTRUE(output)) {

    # Existing output file
    if (isTRUE(file.exists(file.out))) {

      result.object <- misty::mplus.print(file.out, print = print, input = input, result = result,
                                          exclude = exclude, variable = variable, not.input = not.input,
                                          not.result = not.result, write = write, append = append, check = FALSE)

    # Output file does not exist
    } else {

      stop(paste0("Output file ", dQuote(file.out), " does not exist."), call. = FALSE)

      result.object <- NULL

    }

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "mplus",
                 x = x,
                 update = update,
                 args = list(file = file, replace.inp = replace.inp, mplus.run = mplus.run,
                             show.out = show.out, replace.out = replace.out,
                             print = print, input = input, result = result, exclude = exclude,
                             variable = variable, not.input = not.input, not.result = not.result,
                             write = write, append = append, check = check, output = output),
                 input = input.object, write = write.object,
                 result = result.object)

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  return(invisible(object))

}
