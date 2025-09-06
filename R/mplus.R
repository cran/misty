#' Create, Run, and Print Mplus Models
#'
#' This wrapper function creates a Mplus input file, runs the input file by using
#' the \code{mplus.run()} function, and prints the Mplus output file by using the
#' \code{mplus.print()} function.
#'
#' @param x           a character string containing the Mplus input text.
#' @param file        a character string indicating the name of the Mplus input
#'                    file with or without the file extension \code{.inp}, e.g.,
#'                    \code{"Mplus_Input.inp"} or \code{"Mplus_Input"}.
#' @param data        a matrix or data frame from which the variables names for
#'                    the subsection \code{NAMES} are extracted.
#' @param comment     logical: if \code{FALSE} (default), comments (i.e., text
#'                    after the \code{!} symbol) are removed from the input text
#'                    specified in the argument \code{x}.
#' @param replace.inp logical: if \code{TRUE} (default), an existing input
#'                    file will be replaced.
#' @param mplus.run   logical: if \code{TRUE}, the input file specified in the
#'                    argument \code{file} containing the input text specified
#'                    in the argument \code{x} is run using the \code{mplus.run()}
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
#' @param Mplus       a character string for specifying the name or path of the
#'                    Mplus executable to be used for running models. This covers
#'                    situations where Mplus is not in the system's path, or where
#'                    one wants to test different versions of the Mplus program.
#'                    Note that there is no need to specify this argument for most
#'                    users since it has intelligent defaults.
#' @param print       a character vector indicating which results to show, i.e.
#'                    \code{"all"} (default) for all results \code{"input"} for
#'                    input command sections, and \code{"result"} for result sections.
#' @param input       a character vector specifying Mplus input command sections
#'                    included in the output (see 'Details' in the \code{\link{mplus.print}}
#'                    function).
#' @param result      a character vector specifying Mplus result sections included
#'                    in the output (see 'Details' in the \code{\link{mplus.print}}
#'                    function).
#' @param exclude     a character vector specifying Mplus input command or result
#'                    sections excluded from the output (see 'Details' in the
#'                    \code{\link{mplus.print}} function).
#' @param variable    logical: if \code{TRUE}, names of the variables in the data
#'                    set (\code{NAMES ARE}) specified in the \code{VARIABLE:}
#'                    command section are shown. By default, names of the variables
#'                    in the data set are excluded from the output unless all variables
#'                    are used in the analysis (i.e., no \code{USEVARIABLES} option
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
#'                    console by using the function \code{mplus.print()}.
#'
#' @details
#' \describe{
#' \item{\strong{\code{NAMES} Option}}{The \code{NAMES} Option in the \code{VARIABLE}
#' section used to assign names to the variables in the data set can be specified
#' by using the \code{data} argument:
#'    \itemize{
#'        \item{\code{Write Mplus Data File}}: In the first step, the Mplus data
#'          file is written by using the \code{write.mplus()} function, e.g.
#'          \code{write.mplus(ex3_1, file = "ex3_1.dat")}.
#'        \item{\code{Specify Mplus Input}}: In the second step, the Mplus input
#'          is specified as a character string. The \code{NAMES} option is left
#'          out from the Mplus input text, e.g.,
#'        \code{input <- 'DATA:     FILE IS ex3_1.dat;\nMODEL:    y1 ON x1 x3;'}.
#'        \item{\code{Run Mplus Input}}: In the third step, the Mplus input is run
#'          by using the \code{mplus()} function. The argument \code{data}
#'          needs to be specified given that the \code{NAMES} option was left out
#'          from the Mplus input text in the previous step, e.g.,
#           \code{mplus(input, file = "ex3_1.inp", data = ex3_1)}.
#'    }
#' }
#' }
#'
#' @author
#' Takuya Yanagida
#'
#' @seealso
#' \code{\link{read.mplus}}, \code{\link{write.mplus}}, \code{\link{mplus.update}},
#' \code{\link{mplus.print}}, \code{\link{mplus.plot}}, \code{\link{mplus.bayes}},
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
#' \item{\code{x}}{a character vector containing the Mplus input text}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{input}}{list with input command sections}
#' \item{\code{write}}{write command sections}
#' \item{\code{result}}{list with input command sections (\code{input}) and
#'                      result sections (\code{result})}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #----------------------------------------------------------------------------
#' # Example 1: Write data, specify input, and run input
#'
#' # Write Mplus data file
#' write.mplus(ex3_1, file = "ex3_1.dat")
#'
#' # Specify Mplus input, specify NAMES option
#' input1 <- '
#' DATA:     FILE IS ex3_1.dat;
#' VARIABLE: NAMES ARE y1 x1 x3;
#' MODEL:    y1 ON x1 x3;
#' OUTPUT:   SAMPSTAT;
#' '
#'
#' # Run Mplus input
#' mplus(input1, file = "ex3_1.inp")
#'
#' #----------------------------------------------------------------------------
#' # Example 2: Alternative specification using the data argument
#'
#' # Read Mplus data file and set variables names
#' ex3_1 <- setNames(read.table("ex3_1.dat"), nm = c("y1", "x1", "x3"))
#'
#' # Specify Mplus input, leave out the NAMES option
#' input2 <- '
#' DATA:     FILE IS ex3_1.dat;
#' MODEL:    y1 ON x1 x3;
#' OUTPUT:   SAMPSTAT;
#' '
#'
#' # Run Mplus input, specify the data argument
#' mplus(input2, file = "ex3_1.inp", data = ex3_1)
#' }
mplus <- function(x, file = "Mplus_Input.inp", data = NULL, comment = FALSE, replace.inp = TRUE,
                  mplus.run = TRUE, show.out = FALSE, replace.out = c("always", "never", "modified"),
                  Mplus = .detect.mplus(), print = c("all", "input", "result"),
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

  # Check if input 'x' is missing or NULL or character string
  if (isTRUE(missing(x) ||is.null(x) || !is.character(x) || length(x) != 1L)) { stop("Please specify a character string indicating the Mplus input text for the argument 'x'", call. = FALSE) }

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

  # Check inputs
  .check.input(logical = c("comment", "replace.inp", "mplus.run", "variable", "not.input", "not.result", "append", "output"),
               character = list(file = 1L), s.character = list(replace.out = c("always", "never", "modified"), print = c("all", "input", "result")),
               args = "write1", envir = environment(), input.check = check)

  if (isTRUE(check)) {

    # Check input 'x'
    if (isTRUE(grepl("...", x, fixed = TRUE))) {

      # Semicolon included
      unlist(strsplit(x, ""))[as.numeric(gregexec("\\.\\.\\.", x)[[1L]]) + 3L] |>
        (\(z) if (isTRUE(z != ";" || is.na(z))) { stop("Please include the semicolon ; when using the \"...;\" specification.", call. = FALSE)} )()

    }

    # Check input 'data'
    if (isTRUE(!is.null(data))) {

      # Data frame or matrix for
      if (isTRUE(!is.data.frame(data) && !is.matrix(data))) { stop("Please specify a data frame or matrix for the argument 'data'.", call. = FALSE) }

      # NAMES ARE specification
      if (isTRUE(grepl("NAMES ARE|NAMES IS|NAMES =|NAMES  ARE|NAMES  IS|NAMES  =", toupper(x)))) { stop("Please do not specify the subection NAMES in the Mplus input text when using the argument 'data'.", call. = FALSE) }

    }

    # Check input 'input'
    input.check <- input[which(!input %in% c("all", "default", input.all))]
    if (isTRUE(length(input.check) != 0L)) { stop(paste0(if (isTRUE(length(input.check) == 1L)) { "Character string " } else { "Character vector " }, "specified in the argument 'input' is not permissible: ", paste(dQuote(input.check), collapse = ", ")), call. = FALSE) }

    # Check input 'result'
    result[which(!result %in% c("all", "default", c(input.all, result.all)))] |>
      (\(z) if (isTRUE(length(z) != 0L)) { stop(paste0(if (isTRUE(length(z) == 1L)) { "Character string " } else { "Character vector " }, "specified in the argument 'result' is not permissible: ", paste(dQuote(z), collapse = ", ")), call. = FALSE) } )()

    # Check input 'exclude'
    exclude.check <- exclude[which(!exclude %in% result.all)] |>
      (\(z) if (isTRUE(length(z) != 0L)) { stop(paste0(if (isTRUE(length(z) == 1L)) { "Character string " } else { "Character vector " }, "specified in the argument 'exclude' is not permissible: ", paste(dQuote(z), collapse = ", ")), call. = FALSE) })()

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## file Argument ####

  # File extension .inp
  file <- ifelse(isTRUE(!grepl("\\.inp", file)), file <- paste0(file, ".inp"), file)

  # .out object
  file.out <- sub("\\.inp", ".out", file)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## replace.out Argument ####

  if (isTRUE(all(c("always", "never", "modified") %in% replace.out))) { replace.out <- "always" }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Remove Comments ####

  if (isTRUE(!comment)) {

    x <- paste(unlist(lapply(strsplit(unlist(strsplit(x, "\n")), ""), function(y) {

      if (isTRUE(any(y == "!"))) {

        if (which(misty::chr.omit(y, omit = " ", check = FALSE) == "!")[1L] == 1L) {

          y <- NULL

        } else {

          y <- y[seq_along(y) < which(y == "!")[1L]]

        }

      }

      return(if (isTRUE(!is.null(y))) paste(y, collapse = ""))

    })), collapse = "\n")

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Upper Case Characters ####

  x.upp <- toupper(x)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Extract Position of Input Command Sections ####

  section.pos <- c(if (isTRUE(any(grepl("TITLE:", x.upp)))) { as.numeric(gregexec("TITLE:", x.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("DATA:", x.upp)))) {

                     setdiff(as.numeric(gregexec("DATA:", x.upp)[[1L]]), as.numeric(gregexec("SAVEDATA:", x.upp)[[1L]]) + 4L)

                   } else {

                      NULL

                   },
                   if (isTRUE(any(grepl("DATA IMPUTATION:", x.upp)))) { as.numeric(gregexec("DATA IMPUTATION:", x.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("DATA WIDETOLONG:", x.upp)))) { as.numeric(gregexec("DATA WIDETOLONG:", x.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("DATA LONGTOWIDE:", x.upp)))) { as.numeric(gregexec("DATA LONGTOWIDE:", x.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("DATA TWOPART:", x.upp)))) { as.numeric(gregexec("DATA TWOPART:", x.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("DATA MISSING:", x.upp)))) { as.numeric(gregexec("DATA MISSING:", x.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("DATA SURVIVAL:", x.upp)))) { as.numeric(gregexec("DATA SURVIVAL:", x.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("DATA COHORT:", x.upp)))) { as.numeric(gregexec("DATA COHORT:", x.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("VARIABLE:", x.upp)))) { as.numeric(gregexec("VARIABLE:", x.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("DEFINE:", x.upp)))) { as.numeric(gregexec("DEFINE:", x.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("ANALYSIS:", x.upp)))) { as.numeric(gregexec("ANALYSIS:", x.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("MODEL:", x.upp)))) { as.numeric(gregexec("MODEL:", x.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("MODEL INDIRECT:", x.upp)))) { as.numeric(gregexec("MODEL INDIRECT:", x.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("MODEL TEST:", x.upp)))) { as.numeric(gregexec("MODEL TEST:", x.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("MODEL PRIORS:", x.upp)))) { as.numeric(gregexec("MODEL PRIORS:", x.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("MONTECARLO:", x.upp)))) { as.numeric(gregexec("MONTECARLO:", x.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("MODEL POPULATION:", x.upp)))) { as.numeric(gregexec("MODEL POPULATION:", x.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("MODEL COVERAGE:", x.upp)))) { as.numeric(gregexec("MODEL COVERAGE:", x.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("MODEL MISSING:", x.upp)))) { as.numeric(gregexec("MODEL MISSING:", x.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("MODEL CONSTRAINT:", x.upp)))) { as.numeric(gregexec("MODEL CONSTRAINT:", x.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("PLOT:", x.upp)))) { as.numeric(gregexec("PLOT:", x.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("OUTPUT:", x.upp)))) { as.numeric(gregexec("OUTPUT:", x.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("SAVEDATA:", x.upp)))) { as.numeric(gregexec("SAVEDATA:", x.upp)[[1L]]) } else { NULL })

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Extract Input Command Sections ####

  title <- inpdata <- data.imp <- data.wl <- data.lw <- data.tp <- data.miss <- data.surv <- data.coh <- inpvariable <- define <- analysis <- model <- mod.ind <- mod.test <- mod.prior <- montecarlo <- mod.pop <- mod.cov <- mod.miss <- mod.const <- plot <- inpoutput <- savedata <- NULL

  #...................
  ### TITLE ####

  if (isTRUE(any(grepl("TITLE:", x.upp)))) { title <- .extract.section("TITLE:", x, section.pos) }

  #...................
  ### DATA ####

  if (isTRUE(any(grepl("DATA:", x.upp)))) { inpdata <- .extract.section("DATA:", x, section.pos) }

  #...................
  ### DATA IMPUTATION ####

  if (isTRUE(any(grepl("DATA IMPUTATION:", x.upp)))) { data.imp <- .extract.section("DATA IMPUTATION:", x, section.pos) }

  #...................
  ### DATA WIDETOLONG ####

  if (isTRUE(any(grepl("DATA WIDETOLONG:", x.upp)))) { data.wl <- .extract.section("DATA WIDETOLONG:", x, section.pos) }

  #...................
  ### DATA LONGTOWIDE ####

  if (isTRUE(any(grepl("DATA LONGTOWIDE:", x.upp)))) { data.lw <- .extract.section("DATA LONGTOWIDE:", x, section.pos) }

  #...................
  ### DATA TWOPART ####

  if (isTRUE(any(grepl("DATA TWOPART:", x.upp)))) { data.tp <- .extract.section("DATA TWOPART:", x, section.pos) }

  #...................
  ### DATA MISSING ####

  if (isTRUE(any(grepl("DATA MISSING:", x.upp)))) { data.miss <- .extract.section("DATA MISSING:", x, section.pos) }

  #...................
  ### DATA SURVIVAL ####

  if (isTRUE(any(grepl("DATA SURVIVAL:", x.upp)))) { data.surv <- .extract.section("DATA SURVIVAL:", x, section.pos) }

  #...................
  ### DATA COHORT ####

  if (isTRUE(any(grepl("DATA COHORT:", x.upp)))) { data.coh <- .extract.section("DATA COHORT:", x, section.pos) }

  #...................
  ### VARIABLE ####

  if (isTRUE(any(grepl("VARIABLE:", x.upp)))) { inpvariable <- .extract.section("VARIABLE:", x, section.pos) }

  #...................
  ### DEFINE ####

  if (isTRUE(any(grepl("DEFINE:", x.upp)))) { define <- .extract.section("DEFINE:", x, section.pos) }

  #...................
  ### ANALYSIS ####

  if (isTRUE(any(grepl("ANALYSIS:", x.upp)))) { analysis <- .extract.section("ANALYSIS:", x, section.pos) }

  #...................
  ### MODEL ####

  if (isTRUE(any(grepl("MODEL:", x.upp)))) { model <- .extract.section("MODEL:", x, section.pos) }

  #...................
  ### MODEL INDIRECT ####

  if (isTRUE(any(grepl("MODEL INDIRECT:", x.upp)))) { mod.ind <- .extract.section("MODEL INDIRECT:", x, section.pos) }

  #...................
  ### MODEL TEST ####

  if (isTRUE(any(grepl("MODEL TEST:", x.upp)))) { mod.test <- .extract.section("MODEL TEST:", x, section.pos) }

  #...................
  ### MODEL PRIORS ####

  if (isTRUE(any(grepl("MODEL PRIORS:", x.upp)))) { mod.prior <- .extract.section("MODEL PRIORS:", x, section.pos) }

  #...................
  ### MONTECARLO ####

  if (isTRUE(any(grepl("MONTECARLO:", x.upp)))) { montecarlo <- .extract.section("MONTECARLO:", x, section.pos) }

  #...................
  ### MODEL POPULATION ####

  if (isTRUE(any(grepl("MODEL POPULATION:", x.upp)))) { mod.pop <- .extract.section("MODEL POPULATION:", x, section.pos) }

  #...................
  ### MODEL COVERAGE ####

  if (isTRUE(any(grepl("MODEL COVERAGE:", x.upp)))) { mod.cov <- .extract.section("MODEL COVERAGE:", x, section.pos) }

  #...................
  ### MODEL MISSING ####

  if (isTRUE(any(grepl("MODEL MISSING:", x.upp)))) { mod.miss <- .extract.section("MODEL MISSING:", x, section.pos) }

  #...................
  ### MODEL CONSTRAINT ####

  if (isTRUE(any(grepl("MODEL CONSTRAINT:", x.upp)))) { mod.const <- .extract.section("MODEL CONSTRAINT:", x, section.pos) }

  #...................
  ### PLOT ####

  if (isTRUE(any(grepl("PLOT:", x.upp)))) { plot <- .extract.section("PLOT:", x, section.pos) }

  #...................
  ### OUTPUT ####

  if (isTRUE(any(grepl("OUTPUT:", x.upp)))) { inpoutput <- .extract.section("OUTPUT:", x, section.pos) }

  #...................
  ### SAVEDATA ####

  if (isTRUE(any(grepl("SAVEDATA:", x.upp)))) { savedata <- .extract.section("SAVEDATA:", x, section.pos) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Variable Names ####

  if (isTRUE(!is.null(data))) {

    # VARIABLE Section available
    if (isTRUE(!is.null(inpvariable))) {

      colon.section <- inpvariable

    # VARIABLE Section not available
    } else {

      colon.section <- inpdata

    }

    # Colon position
    col.pos <- as.numeric(gregexec(":", toupper(colon.section)))

    # Space between : and input text
    n.space <- diff(c(col.pos, (which(unlist(strsplit(colon.section, "")) != " ") |>
                                  (\(y) y[y > col.pos][1L])()))) - 1L

    # VARIABLE Section not available
    if (isTRUE(is.null(inpvariable))) {

      if (isTRUE(n.space <= 5L)) {

        n.space <- 1L

      } else {

        n.space <- n.space - 4L

      }

    }

    names.are <- names.temp <- names.length <- paste0(paste0(rep(" ", times = n.space), collapse = ""), "        ", collapse = "")

    # Loop across variables names
    for (i in colnames(data)) {

      # Replace "." with "_"
      if (isTRUE(grepl("\\.", i))) { i <- gsub("\\.", "_", i) }

      names.temp <- paste(names.are, i, collapse = " ")
      names.length <- paste(names.length, i, collapse = " ")

      if (isTRUE(nchar(names.length) < 80L)) {

        names.are <- names.temp

      } else {

        names.are <- paste(names.are, "\n", "      ", paste0(rep(" ", times = n.space), collapse = ""), i, collapse = " ")
        names.length <- paste("      ", paste0(rep(" ", times = n.space), collapse = ""), i, collapse = " ")

      }

    }

    # VARIABLE Section available
    if (isTRUE(!is.null(inpvariable))) {

      inpvariable <- paste0(c("VARIABLE:", rep(" ", times = n.space), "NAMES ARE\n",
                            names.are, ";\n\n",
                            rep(" ", times = col.pos), paste(unlist(strsplit(inpvariable, ""))[-c(1L:col.pos)], collapse = "")), collapse = "")

    # VARIABLE Section not available
    } else {

      inpvariable <- paste0(c("VARIABLE:", rep(" ", times = n.space), "NAMES ARE\n",
                              names.are, ";\n"), collapse = "")

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Input Object ####

  input.object <- list(title = title,
                       data = inpdata, data.imp = data.imp, data.wl = data.wl, data.lw = data.lw,
                       data.tp = data.tp, data.miss = data.miss, data.surv = data.surv, data.coh = data.coh,
                       variable = inpvariable, define = define, analysis = analysis,
                       model = model, mod.ind = mod.ind, mod.test = mod.test, mod.prior = mod.prior,
                       montecarlo = montecarlo, mod.pop = mod.pop, mod.cov = mod.cov, mod.miss = mod.miss,
                       mod.const = mod.const, plot = plot, output = inpoutput, savedata = savedata)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Write Object ####

  write.object <- paste(sapply(unlist(Filter(Negate(is.null), input.object)), function(y) {

    if (substr(y, start = nchar(y), stop = nchar(y)) != "\n") {

      return(paste0(y, "\n"))

    } else {

      return(y)

    }

  }), collapse = "\n")

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

      misty::mplus.run(file, recursive = FALSE, Mplus = Mplus, replace.out = replace.out, message = FALSE, show.out = show.out)

      if (isTRUE(output)) { cat("\n") }

    # Input file does not exist
    } else {

      stop(paste0("Input file ", dQuote(file), " does not exist."), call. = FALSE)

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Mplus Output ####

  if (isTRUE(mplus.run && output)) {

    # Existing output file
    if (isTRUE(file.exists(file.out))) {

      result.object <- misty::mplus.print(tolower(file.out), print = print, input = input, result = result,
                                          exclude = exclude, variable = variable, not.input = not.input,
                                          not.result = not.result, write = write, append = append, check = FALSE)

    # Output file does not exist
    } else {

      stop(paste0("Output file ", dQuote(file.out), " does not exist."), call. = FALSE)

      result.object <- NULL

    }

  } else {

    result.object <- NULL

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "mplus",
                 x = x,
                 args = list(file = file, replace.inp = replace.inp, data = data, mplus.run = mplus.run,
                             show.out = show.out, replace.out = replace.out,
                             Mplus = Mplus, print = print, input = input, result = result, exclude = exclude,
                             variable = variable, not.input = not.input, not.result = not.result,
                             write = write, append = append, check = check, output = output),
                 input = input.object, write = write.object,
                 result = result.object$result)

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  return(invisible(object))

}

#_______________________________________________________________________________
