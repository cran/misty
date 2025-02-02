#' Blimp Input Updating
#'
#' This function updates specific input command sections of a \code{misty.object}
#' of type \code{blimp} to create an updated Blimp input file, run the updated
#' input file by using the \code{blimp.run()} function, and print the updated
#' Blimp output file by using the \code{blimp.print()} function.
#'
#' @param x           \code{misty.object} object of type \code{blimp}.
#' @param update      a character vector containing the updated input command
#'                    sections.
#' @param file        a character string indicating the name of the updated Blimp
#'                    input file with or without the file extension \code{.imp},
#'                    e.g., \code{"Blimp_Input_Update.imp"} or
#'                    \code{"Blimp_Input_Update.imp"}.
#' @param comment     logical: if \code{FALSE} (default), comments (i.e., text
#'                    after the \code{#} symbol) are removed from the input text
#'                    specified in the argument \code{x}.
#' @param replace.inp logical: if \code{TRUE} (default), an existing input
#'                    file will be replaced.
#' @param blimp.run   logical: if \code{TRUE}, the input file specified in the
#'                    argument \code{file} containing the input text specified
#'                    in the argument \code{x} is run using the \code{blimp.run()}
#'                    function.
#' @param posterior   logical: if \code{TRUE}, the posterior distribution including
#'                    burn-in and post-burn-in phase for all parameters are saved
#'                    in long format in a file called \code{posterior.*} in the
#'                    folder specified in the argument \code{folder} and \code{.imp}
#'                    file name in the format specified in the argument \code{format}.
#' @param folder      a character string indicating the prefix of the folder for
#'                    saving the posterior distributions. The default setting is
#'                    \code{folder = "Posterior_"}.
#' @param format      a character vector indicating the file format(s) for saving
#'                    the posterior distributions, i.e., \code{"csv"} (default)
#'                    for \code{write.csv()}, \code{"csv2"} for \code{write.csv2()},
#'                    \code{"xlsx"} for \code{write.xlsx()}, \code{"rds"} for
#'                    \code{saveRDS()}, and \code{"RData"} for \code{write()}.
#' @param clear       logical: if \code{TRUE} (default), the console is cleared
#'                    after estimating each model.
#' @param replace.out a character string for specifying three settings:
#'                    \code{"always"} (default), which runs all models, regardless
#'                    of whether an output file for the model exists, \code{"never"},
#'                    which does not run any model that has an existing output file,
#'                    and \code{"modified"}, which only runs a model if the
#'                    modified date for the input file is more recent than the
#'                    output file modified date.
#' @param Blimp       a character string for specifying the name or path of the
#'                    Blimp executable to be used for running models. This covers
#'                    situations where Blimp is not in the system's path, or where
#'                    one wants to test different versions of the Blimp program.
#'                    Note that there is no need to specify this argument for most
#'                    users since it has intelligent defaults.
#' @param result      a character vector specifying Blimp result sections included
#'                    in the output (see 'Details' in the \code{\link{blimp.print}}
#'                    function).
#' @param exclude     a character vector specifying Blimp input command or result
#'                    sections excluded from the output (see 'Details' in the
#'                    \code{\link{blimp.print}} function).
#' @param color       a character vector with two elements indicating the colors
#'                    used for headers (e.g., \code{"ALGORITHMIC OPTIONS SPECIFIED:"}),
#'                    and for the header \code{Outcome Variable:} and
#'                    \code{Missing predictor:} including variables names.
#' @param style       a character vector with two elements indicating the style
#'                    used for headers (e.g., \code{"ALGORITHMIC OPTIONS SPECIFIED:"}),
#'                    and for the header \code{Outcome Variable:} and
#'                    \code{Missing predictor:} including variables names, i.e.,
#'                    \code{regular}, for regular text, \code{bold} for bold text,
#'                    \code{italic}, for italic text, and \code{underline} for
#'                    underline text.
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
#'                    console by using the function \code{blimp.print()}.
#'
#' @details
#' \describe{
#'  \item{\strong{Bimp Input Sections}}{The function is used to update
#'  following Blimp input sections:
#'  \itemize{
#'    \item{\code{DATA}}
#'    \item{\code{VARIABLES}}
#'    \item{\code{CLUSTERID}}
#'    \item{\code{ORDINAL}}
#'    \item{\code{NOMINAL}}
#'    \item{\code{COUNT}}
#'    \item{\code{WEIGHT}}
#'    \item{\code{MISSING}}
#'    \item{\code{LATENT}}
#'    \item{\code{RANDOMEFFECT}}
#'    \item{\code{TRANSFORM}}
#'    \item{\code{BYGROUP}}
#'    \item{\code{FIXED}}
#'    \item{\code{CENTER}}
#'    \item{\code{MODEL}}
#'    \item{\code{SIMPLE}}
#'    \item{\code{PARAMETERS}}
#'    \item{\code{TEST}}
#'    \item{\code{FCS}}
#'    \item{\code{SIMUALTE}}
#'    \item{\code{SEED}}
#'    \item{\code{BURN}}
#'    \item{\code{ITERATIONS}}
#'    \item{\code{CHAINS}}
#'    \item{\code{NIMPS}}
#'    \item{\code{THIN}}
#'    \item{\code{OPTIONS}}
#'    \item{\code{OUTPUT}}
#'    \item{\code{SAVE}}
#'  }
#' }
#' \item{\strong{The \code{---;} Specification}}{The \code{---;} specification
#' is used to remove entire sections (e.g., \code{CENTER: ---;}) from the Blimp
#' input. Note that \code{---;} including the semicolon \code{;} needs to be
#' specified, i.e., \code{---} without the semicolon \code{;} will result in an
#' error message.
#' }
#' }
#'
#' @author
#' Takuya Yanagida
#'
#' @seealso
#' \code{\link{blimp}}, \code{\link{blimp.run}},
#' \code{\link{blimp.print}}, \code{\link{blimp.plot}}, \code{\link{blimp.bayes}}
#'
#' @references
#' Keller, B. T., & Enders, C. K. (2023). \emph{Blimp user’s guide} (Version 3).
#' Retrieved from www.appliedmissingdata.com/blimp
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#'
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{x}}{\code{misty.object} object of type \code{blimp}}
#' \item{\code{update}}{a character vector containing the updated Blimp input command
#'                      sections}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{write}}{updated write command sections}
#' \item{\code{result}}{list with result sections (\code{result})}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #----------------------------------------------------------------------------
#' # Example 1a: Update BURN and ITERATIONS section
#'
#' # Specify Blimp input
#' input <- '
#' DATA: data1.csv;
#' ORDINAL: d;
#' MISSING: 999;
#' FIXED: d;
#' CENTER: x1 x2;
#' MODEL: y ~ x1 x2 d;
#' SEED: 90291;
#' BURN: 1000;
#' ITERATIONS: 10000;
#' '
#'
#' # Run Blimp input
#' mod0 <- blimp(input, file = "Ex4.3.imp", clear = FALSE)
#'
#' # Update sections
#' update1 <- '
#' BURN: 5000;
#' ITERATIONS: 20000;
#' '
#'
#' # Run updated Blimp input
#' mod1 <- blimp.update(mod0, update1, file = "Ex4.3_update1.imp")
#'
#' #----------------------------------------------------------------------------
#' # Example 1b: Remove CENTER section
#'
#' # Remove section
#' update2 <- '
#' CENTER: ---;
#' '
#'
#' # Run updated Blimp input
#' mod2 <- blimp.update(mod1, update2, file = "Ex4.3_update2.imp")
#' }
blimp.update <- function(x, update, file = "Blimp_Input_Update.imp", comment = FALSE,
                         replace.inp = TRUE, blimp.run = TRUE, posterior = FALSE,
                         folder = "Posterior_", format = c("csv", "csv2", "xlsx", "rds", "RData"),
                         clear = TRUE, replace.out = c("always", "never", "modified"),
                         Blimp = .detect.blimp(),
                         result = c("all", "default", "algo.options", "data.info",
                                    "model.info", "warn.mess", "out.model", "gen.param"),
                         exclude = NULL, color = c("none", "blue", "violet"), style = c("bold", "regular"),
                         not.result = TRUE, write = NULL, append = TRUE, check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'x' is a misty.object
  if (isTRUE(!inherits(x, "misty.object"))) { stop("Please specify a misty.object for the argument 'x'", call. = FALSE) }

  # Check if input 'x' is a misty.object
  if (isTRUE(x$type != "blimp")) { stop("Please specify a misty.object of type 'blimp' for the argument 'x'", call. = FALSE) }

  # Check if input 'update' is not missing
  if (isTRUE(missing(update))) { stop("Please specify a character vector for the argument 'update'", call. = FALSE) }

  # Check if input 'update' is NULL
  if (isTRUE(is.null(update))) { stop("Input specified for the argument 'update' is NULL.", call. = FALSE) }

  # Check if input 'update' is a character string
  if (isTRUE(!is.character(update) || length(update) != 1L)) { stop("Please specify a character string indicating the updated Blimp input text for the argument 'update'", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Result Argument ------------------------------------------------------------

  # All result options
  result.all <- c("algo.options", "simdat.summary", "simdat.summary", "order.simdat", "burnin.psr", "mh.accept", "data.info", "var.imp", "model.info", "param.label", "warn.mess", "fit", "cor.resid", "out.model", "pred.model", "gen.param", "order.impdat")

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("comment", "replace.inp", "blimp.run", "posterior", "clear", "not.result", "append", "output"),
               character = list(file = 1L, folder = 1L, write = 1L, Blimp = 1L, style = 2L),
               m.character = list(format = c("csv", "csv2", "xlsx", "rds", "RData"), replace.out = c("always", "never", "modified"), style = c("regular", "bold", "italic", "underline")), envir = environment(), input.check = check)

  # Additional checks
  if (isTRUE(check)) {

    # Check input 'update': ...; specification
    if (isTRUE(grepl("...", update, fixed = TRUE))) {

      unlist(strsplit(update, ""))[as.numeric(gregexec("\\.\\.\\.", update)[[1L]]) + 3L] |>
        (\(z) if (isTRUE(z != ";" || is.na(z))) { stop("Please include the semicolon ; when using the \"...;\" specification.", call. = FALSE)} )()

    }

    # Check input 'update': ---; specification
    if (isTRUE(grepl("---", update))) {

      unlist(strsplit(update, ""))[as.numeric(gregexec("---", update)[[1L]]) + 3L] |>
        (\(z) if (isTRUE(z != ";" || is.na(z))) { stop("Please include the semicolon ; when using the \"---;\" specification.", call. = FALSE)} )()

    }

    # Check input 'result'
    result[which(!result %in% c("all", "default", result.all))] |>
      (\(y) if (isTRUE(length(y) != 0L)) { stop(paste0(if (isTRUE(length(y) == 1L)) { "Character string " } else { "Character vector " }, "specified in the argument 'result' is not permissible: ", paste(dQuote(y), collapse = ", ")), call. = FALSE) })()

    # Check input 'exclude'
    exclude[which(!exclude %in% result.all)] |>
      (\(y) if (isTRUE(length(y) != 0L)) { stop(paste0(if (isTRUE(length(y) == 1L)) { "Character string " } else { "Character vector " }, "specified in the argument 'exclude' is not permissible: ", paste(dQuote(y), collapse = ", ")), call. = FALSE) })()

    # Check input 'color'
    if (isTRUE(!all(color %in% c("none", "black", "red", "green", "yellow", "blue", "violet", "cyan", "white", "gray", "b.red", "b.green", "b.yellow", "b.blue", "b.violet", "b.cyan", "b.white")))) { stop(paste0(if (isTRUE(length(color) == 1L)) { "Character string " } else { "Character vector " }, "specified in the argument 'color' is not permissible."), call. = FALSE) }

    if (isTRUE(!all(c("none", "blue", "violet") %in% color))) { if (isTRUE(length(color) != 2L)) { stop("Please specify a vector with two elements for the argument 'color'.", call. = FALSE) } }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## update Argument ####

  # Upper case characters
  update.upp <- toupper(update)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## file Argument ####

  # File extension .inp
  file <- ifelse(isTRUE(!grepl("\\.imp", file)), file <- paste0(file, ".imp"), file)

  # .out object
  file.out <- sub("\\.imp", ".blimp-out", file)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## replace.out Argument ####

  if (isTRUE(all(c("always", "never", "modified") %in% replace.out))) {

    replace.out <- "always"

  } else {

    if (isTRUE(length(replace.out) != 1L)) { stop("Please specify a character string for the argument 'replace.out'", call. = FALSE) }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## format ####

  if (isTRUE(all(c("csv", "csv2", "xlsx", "rds", "RData") %in% format))) {

    format <- "csv"

  } else {

    if (isTRUE(all(c("csv", "csv2") %in% format))) {

      stop("Please specify either \"csv\" or \"csv2\" for the argument 'format'.", call. = FALSE)

    }

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Remove Comments ####

  if (isTRUE(!comment)) {

    update <- paste(unlist(lapply(strsplit(unlist(strsplit(update, "\n")), ""), function(y) {

      if (isTRUE(any(y == "#"))) {

        if (which(misty::chr.omit(y, omit = " ", check = FALSE) == "#")[1L] == 1L) {

          y <- NULL

        } else {

          y <- y[seq_along(y) < which(y == "#")[1L]]

        }

      }

      return(if (isTRUE(!is.null(y))) paste(y, collapse = ""))

    })), collapse = "\n")

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Extract Position of Update Input Command Sections ####

  section.pos <- c(if (isTRUE(any(grepl("DATA:", x, ignore.case = TRUE)))) { as.numeric(gregexec("DATA:", toupper(x))[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("VARIABLES:", x, ignore.case = TRUE)))) {

                     setdiff(as.numeric(gregexec("VARIABLES:", toupper(x))[[1L]]), as.numeric(gregexec(paste0("\\.", "VARIABLES:"), toupper(x))[[1L]]) + 1L)

                   } else {

                     NULL

                   },
                   if (isTRUE(any(grepl("CLUSTERID:", x, ignore.case = TRUE)))) { as.numeric(gregexec("CLUSTERID:", update.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("ORDINAL:", x, ignore.case = TRUE)))) { as.numeric(gregexec("ORDINAL:", update.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("NOMINAL:", x, ignore.case = TRUE)))) { as.numeric(gregexec("NOMINAL:", update.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("COUNT:", x, ignore.case = TRUE)))) { as.numeric(gregexec("COUNT:", update.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("WEIGHT:", x, ignore.case = TRUE)))) { as.numeric(gregexec("WEIGHT:", update.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("MISSING:", x, ignore.case = TRUE)))) { as.numeric(gregexec("MISSING:", update.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("LATENT:", x, ignore.case = TRUE)))) { as.numeric(gregexec("LATENT:", update.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("RANDOMEFFECT:", x, ignore.case = TRUE)))) { as.numeric(gregexec("RANDOMEFFECT:", update.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("TRANSFORM:", x, ignore.case = TRUE)))) { as.numeric(gregexec("TRANSFORM:", update.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("BYGROUP:", x, ignore.case = TRUE)))) { as.numeric(gregexec("BYGROUP:", update.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("FIXED:", x, ignore.case = TRUE)))) { as.numeric(gregexec("FIXED:", update.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("CENTER:", x, ignore.case = TRUE)))) { as.numeric(gregexec("CENTER:", update.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("MODEL:", x, ignore.case = TRUE)))) {

                     setdiff(as.numeric(gregexec("MODEL:", update.upp)[[1L]]),
                             c(as.numeric(gregexec(paste0("\\.", "MODEL:"), update.upp)[[1L]]) + 1L, as.numeric(gregexec(paste0("\\_", "MODEL:"), update.upp)[[1L]]) + 1L))

                   } else {

                     NULL

                   },
                   if (isTRUE(any(grepl("SIMPLE:", x, ignore.case = TRUE)))) { as.numeric(gregexec("SIMPLE:", update.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("PARAMETERS:", x, ignore.case = TRUE)))) { as.numeric(gregexec("PARAMETERS:", update.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("TEST:", x, ignore.case = TRUE)))) { as.numeric(gregexec("TEST:", update.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("FCS:", x, ignore.case = TRUE)))) { as.numeric(gregexec("FCS:", update.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("SIMULATE:", x, ignore.case = TRUE)))) { as.numeric(gregexec("SIMULATE:", update.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("SEED:", x, ignore.case = TRUE)))) { as.numeric(gregexec("SEED:", update.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("BURN:", x, ignore.case = TRUE)))) { as.numeric(gregexec("BURN:", update.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("ITERATIONS:", x, ignore.case = TRUE)))) { as.numeric(gregexec("ITERATIONS:", update.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("CHAINS:", x, ignore.case = TRUE)))) { as.numeric(gregexec("CHAINS:", update.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("NIMPS:", x, ignore.case = TRUE)))) { as.numeric(gregexec("NIMPS:", update.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("THIN:", x, ignore.case = TRUE)))) { as.numeric(gregexec("THIN:", update.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("OPTIONS:", x, ignore.case = TRUE)))) { as.numeric(gregexec("OPTIONS:", update.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("OUTPUT:", x, ignore.case = TRUE)))) { as.numeric(gregexec("OUTPUT:", update.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("SAVE:", x, ignore.case = TRUE)))) { as.numeric(gregexec("SAVE:", update.upp)[[1L]]) } else { NULL })


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Extract Update Input Command Sections ####

  bdata <- variables <- ordinal <- nominal <- count <- clusterid <- weight <- missing <- latent <- randomeffect <- transform <- bygroup <- fixed <- center <- model <- simple <- parameters <- test <- fcs <- simulate <- seed <- burn <- iterations <- chains <- nimps <- thin <- options <- boutput <- save <- NULL

  #...................
  ### DATA ####

  if (isTRUE(any(grepl("DATA:", update, ignore.case = TRUE)))) { bdata <- .extract.section("DATA:", update, section.pos) }

  #...................
  ### VARIABLES ####

  if (isTRUE(any(grepl("VARIABLES:", update, ignore.case = TRUE)))) { variables <- .extract.section("VARIABLES:", update, section.pos) }

  #...................
  ### ORDINAL ####

  if (isTRUE(any(grepl("ORDINAL:", update, ignore.case = TRUE)))) { ordinal <- .extract.section("ORDINAL:", update, section.pos) }

  #...................
  ### NOMINAL ####

  if (isTRUE(any(grepl("NOMINAL:", update, ignore.case = TRUE)))) { nominal <- .extract.section("NOMINAL:", update, section.pos) }

  #...................
  ### COUNT ####

  if (isTRUE(any(grepl("COUNT:", update, ignore.case = TRUE)))) { count <- .extract.section("COUNT:", update, section.pos) }

  #...................
  ### CLUSTERID ####

  if (isTRUE(any(grepl("CLUSTERID:", update, ignore.case = TRUE)))) { clusterid <- .extract.section("CLUSTERID:", update, section.pos) }

  #...................
  ### WEIGHT ####

  if (isTRUE(any(grepl("WEIGHT:", update, ignore.case = TRUE)))) { weight <- .extract.section("WEIGHT:", update, section.pos) }

  #...................
  ### MISSING ####

  if (isTRUE(any(grepl("MISSING:", update, ignore.case = TRUE)))) { missing <- .extract.section("MISSING:", update, section.pos) }

  #...................
  ### LATENT ####

  if (isTRUE(any(grepl("LATENT:", update, ignore.case = TRUE)))) { latent <- .extract.section("LATENT:", update, section.pos) }

  #...................
  ### RANDOMEFFECT ####

  if (isTRUE(any(grepl("RANDOMEFFECT:", update, ignore.case = TRUE)))) { randomeffect <- .extract.section("RANDOMEFFECT:", update, section.pos) }

  #...................
  ### TRANSFORM ####

  if (isTRUE(any(grepl("TRANSFORM:", update, ignore.case = TRUE)))) { transform <- .extract.section("TRANSFORM:", update, section.pos) }

  #...................
  ### BYGROUP ####

  if (isTRUE(any(grepl("BYGROUP:", update, ignore.case = TRUE)))) { bygroup <- .extract.section("BYGROUP:", update, section.pos) }

  #...................
  ### FIXED ####

  if (isTRUE(any(grepl("FIXED:", update, ignore.case = TRUE)))) { fixed <- .extract.section("FIXED:", update, section.pos) }

  #...................
  ### CENTER ####

  if (isTRUE(any(grepl("CENTER:", update, ignore.case = TRUE)))) { center <- .extract.section("CENTER:", update, section.pos) }

  #...................
  ### MODEL ####

  if (isTRUE(any(grepl("MODEL:", update, ignore.case = TRUE)))) { model <- .extract.section("MODEL:", update, section.pos) }

  #...................
  ### SIMPLE ####

  if (isTRUE(any(grepl("SIMPLE:", update, ignore.case = TRUE)))) { simple <- .extract.section("SIMPLE:", update, section.pos) }

  #...................
  ### PARAMETERS ####

  if (isTRUE(any(grepl("PARAMETERS:", update, ignore.case = TRUE)))) { parameters <- .extract.section("PARAMETERS:", update, section.pos) }

  #...................
  ### TEST ####

  if (isTRUE(any(grepl("TEST:", update, ignore.case = TRUE)))) { test <- .extract.section("TEST:", update, section.pos) }

  #...................
  ### FCS ####

  if (isTRUE(any(grepl("FCS:", update, ignore.case = TRUE)))) { fcs <- .extract.section("FCS:", update, section.pos) }

  #...................
  ### SIMULATE ####

  if (isTRUE(any(grepl("SIMULATE:", update, ignore.case = TRUE)))) { simulate <- .extract.section("SIMULATE:", update, section.pos) }

  #...................
  ### SEED ####

  if (isTRUE(any(grepl("SEED:", update, ignore.case = TRUE)))) { seed <- .extract.section("SEED:", update, section.pos) }

  #...................
  ### BURN ####

  if (isTRUE(any(grepl("BURN:", update, ignore.case = TRUE)))) { burn <- .extract.section("BURN:", update, section.pos) }

  #...................
  ### ITERATIONS ####

  if (isTRUE(any(grepl("ITERATIONS:", update, ignore.case = TRUE)))) { iterations <- .extract.section("ITERATIONS:", update, section.pos) }

  #...................
  ### CHAINS ####

  if (isTRUE(any(grepl("CHAINS:", update, ignore.case = TRUE)))) { chains <- .extract.section("CHAINS:", update, section.pos) }

  #...................
  ### NIMPS ####

  if (isTRUE(any(grepl("NIMPS:", update, ignore.case = TRUE)))) { nimps <- .extract.section("NIMPS:", update, section.pos) }

  #...................
  ### THIN ####

  if (isTRUE(any(grepl("THIN:", update, ignore.case = TRUE)))) { thin <- .extract.section("THIN:", update, section.pos) }

  #...................
  ### OPTIONS ####

  if (isTRUE(any(grepl("OPTIONS:", update, ignore.case = TRUE)))) { options <- .extract.section("OPTIONS:", update, section.pos) }

  #...................
  ### OUTPUT ####

  if (isTRUE(any(grepl("OUTPUT:", update, ignore.case = TRUE)))) { boutput <- .extract.section("OUTPUT:", update, section.pos) }

  #...................
  ### SAVE ####

  if (isTRUE(any(grepl("SAVE:", update, ignore.case = TRUE)))) { save <- .extract.section("SAVE:", update, section.pos) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Update Input Object ####

  input.object.update <- Filter(Negate(is.null),
                                list(data = bdata, variables = variables, clusterid = clusterid,  ordinal = ordinal, nominal = nominal, count = count,
                                     weight = weight, missing = missing, latent = latent, randomeffect = randomeffect, transform = transform,
                                     bygroup = bygroup, fixed = fixed, center = center, model = model, simple = simple, parameters = parameters,
                                     test = test, fcs = fcs, simulate = simulate, seed = seed, burn = burn, iterations = iterations,
                                     chains = chains, nimps = nimps, thin = thin, options = options, output = boutput, save = save))

  # Input object
  input.object <- Filter(Negate(is.null), x$input)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Update Input Section ####

  for (i in names(input.object.update)) {

    #...................
    ### Update section available in input ####

    if (isTRUE(i %in% names(input.object))) {

      input.object[[i]] <- input.object.update[[i]]

    #...................
    ### Update section not available in input ####
    } else {

      # Order sections
      section <- c("data", "variables", "clusterid", "ordinal", "nominal", "count", "weight", "missing", "latent", "randomeffect", "transform", "bygroup", "fixed", "center", "model", "simple", "parameters", "test", "fcs", "simulate", "seed", "burn", "iterations", "chains", "nimps", "thin", "options", "output", "save")

      input.object <- within(input.object, assign(i, input.object.update[[i]])) |>
        (\(z) z[section[section %in% names(z)]])()

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Remove sections using --- ####

    if (isTRUE(grepl("---", input.object[[i]], fixed = TRUE))) {

      input.object[[i]] <- NULL

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Write Object ####

  write.object <- misty::chr.gsub(c("\n\n\n", "\n\n\n\n", "\n\n\n\n\n", "\n\n\n\n\n\n", "\n\n\n\n\n\n\n"), rep("\n\n", times = 5L), paste(input.object, collapse = "\n"))

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
  ## Run Blimp ####

  if (isTRUE(blimp.run)) {

    # Existing input file
    if (isTRUE(file.exists(file))) {

      # Do not replace output file
      if (isTRUE(replace.out == "never" && file.exists(file.out))) { stop(paste0("Output file ", dQuote(file.out), " already exists, please specify 'replace.out = \"always\"' to replace existing output file."), call. = FALSE) }

      cat("Running Model:", file, "\n")

      misty::blimp.run(file, recursive = FALSE, replace.out = replace.out, posterior = posterior,
                       folder = folder, format = format, clear = clear, Blim = Blimp, check = FALSE)

      if (isTRUE(output)) { cat("\n") }

    # Input file does not exist
    } else {

      stop(paste0("Input file ", dQuote(file), " does not exist."), call. = FALSE)

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Blimp Output ####

  if (isTRUE(output)) {

    # Existing output file
    if (isTRUE(file.exists(file.out))) {

      result.object <- misty::blimp.print(file.out, result = result, exclude = exclude, color = color, style = style,
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
                 type = "blimp",
                 x = x, update = update,
                 args = list(file = file, comment = comment, replace.inp = replace.inp, blimp.run = blimp.run,
                             posterior = posterior, folder = folder, format = format, clear = clear,
                             replace.out = replace.out, Blimp = Blimp, result = result, exclude = exclude,
                             color = color, style = style, not.result = not.result, write = write,
                             append = append, check = check, output = output),
                 input = input.object, write = write.object, result = result.object$result)

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  return(invisible(object))

}

#_______________________________________________________________________________
