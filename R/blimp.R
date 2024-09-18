#' Create, Run, and Print Blimp Models
#'
#' This wrapper function creates a Blimp input file, runs the input file by using
#' the \code{blimp.run()} function, and prints the Blimp output file by using the
#' \code{blimp.print()} function.
#'
#' @param x           a character string containing the Blimp input text.
#' @param file        a character string indicating the name of the Blimp input
#'                    file with or without the file extension \code{.imp}, e.g.,
#'                    \code{"Blimp_Input.imp"} or \code{"Blimp_Input.imp"}.
#' @param data        a matrix or data frame from which the variables names for
#'                    the section \code{VARIABLES} are extracted.
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
#'                    \code{"excel"} for \code{write.xlsx()}, \code{"rds"} for
#'                    \code{saveRDS()}, and \code{"workspace"} for \code{write()}.
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
#' @param color      a character vector with two elements indicating the colors
#'                   used for the main headers (e.g., \code{"ALGORITHMIC OPTIONS SPECIFIED:"}),
#'                   and for the headers \code{Outcome Variable:} and
#'                   \code{Missing predictor:}, \code{Complete variable:},
#'                   \code{Latent Variable:}, and \code{Covariance Matrix:}
#'                   including variables names.
#' @param style      a character vector with two elements indicating the style
#'                   used for headers (e.g., \code{"ALGORITHMIC OPTIONS SPECIFIED:"}),
#'                   and for the main headers (e.g., \code{"ALGORITHMIC OPTIONS SPECIFIED:"}),
#'                   and for the headers \code{Outcome Variable:} and
#'                   \code{Missing predictor:}, \code{Complete variable:},
#'                   \code{Latent Variable:}, and \code{Covariance Matrix:}
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
#' \item{\strong{\code{VARIABLES} Section}}{The \code{VARIABLES} section used to
#' assign names to the variables in the data set can be specified by using
#' the \code{data} argument:
#'    \itemize{
#'       \item{\code{Write Blimp Data File}}: In the first step, the Blimp data
#'       file is written by using the \code{write.mplus()} function, e.g.
#'       \code{write.mplus(data1, file = "data1.dat")}.
#'       \item{\code{Specify Blimp Input}}: In the second step, the Blimp input
#'       is specified as a character string. The \code{VARIABLES} option is left
#'       out from the Blimp input text, e.g.,
#'       \code{input <- 'DATA: data1.dat;\nMODEL: y ~ x1@b1 x2@b2 d2;'}.
#'       \item{\code{Run Blimp Input}}: In the third step, the Blimp input is run
#'       by using the \code{blimp()} function. The argument \code{data} needs to
#'       be specified given that the \code{VARIABLES} section was left out from
#'       the Blimp input text in the previous step, e.g.,
#'       \code{blimp(input, file = "Ex4.3.imp", data = data1)}.
#'    }
#'  Note that unlike Mplus, Blimp allows to specify a CSV data file with variable
#'  names in the first row. Hence, it is recommended to export the data from
#'  R using the \code{write.csv()} function to specify the data file in the \coe{DATA}
#'  section of the Blimp input file without specifying the \code{VARIABLES} section.
#' }
#' }
#'
#' @author
#' Takuya Yanagida
#'
#' @seealso
#' \code{\link{blimp.update}}, \code{\link{blimp.run}},
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
#' \item{\code{x}}{a character vector containing the Blimp input text}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{write}}{write command sections}
#' \item{\code{result}}{list with result sections (\code{result})}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #----------------------------------------------------------------------------
#' # Example 1: Write data, specify input without VARIABLES section, and run input
#'
#' # Write Data File
#' # Note that row.names = FALSE needs to be specified
#' write.csv(data1, file = "data1.csv", row.names = FALSE)
#'
#' # Specify Blimp input
#' input1 <- '
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
#' blimp(input1, file = "Ex4.3.imp")
#'
#' #----------------------------------------------------------------------------
#' # Example 2: Write data, specify input with VARIABLES section, and run input
#'
#' # Write Data File
#' write.mplus(data1, file = "data1.dat", input = FALSE)
#'
#' # Specify Blimp input
#' input2 <- '
#' DATA: data1.dat;
#' VARIABLES: id v1 v2 v3 y x1 d x2 v4;
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
#' blimp(input2, file = "Ex4.3.imp")
#'
#' #----------------------------------------------------------------------------
#' # Example 3: Alternative specification using the data argument
#'
#' # Write Data File
#' write.mplus(data1, file = "data1.dat", input = FALSE)
#'
#' # Specify Blimp input
#' input3 <- '
#' DATA: data1.dat;
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
#' blimp(input3, file = "Ex4.3.imp", data = data1)
#' }
blimp <- function(x, file = "Blimp_Input.imp", data = NULL, comment = FALSE,
                  replace.inp = TRUE, blimp.run = TRUE, posterior = FALSE,
                  folder = "Posterior_", format = c("csv", "csv2", "excel", "rds", "workspace"),
                  clear = TRUE, replace.out = c("always", "never", "modified"),
                  Blimp = detect.blimp(),
                  result = c("all", "default", "algo.options", "data.info",
                             "model.info", "warn.mess", "out.model", "gen.param"),
                  exclude = NULL, color = c("none", "blue", "violet"), style = c("bold", "regular"),
                  not.result = TRUE, write = NULL, append = TRUE, check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'x' is not missing
  if (isTRUE(missing(x))) { stop("Please specify a character vector for the argument 'x'", call. = FALSE) }

  # Check if input 'x' is NULL
  if (isTRUE(is.null(x))) { stop("Input specified for the argument 'x' is NULL.", call. = FALSE) }

  # Check if input 'x' is a character string
  if (isTRUE(!is.character(x) || length(x) != 1L)) { stop("Please specify a character string indicating the Blimp input text for the argument 'x'", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Result Argument ------------------------------------------------------------

  # All result options
  result.all <- c("algo.options", "simdat.summary", "simdat.summary", "order.simdat", "burnin.psr", "mh.accept", "data.info", "var.imp", "model.info", "param.label", "warn.mess", "fit", "cor.resid", "out.model", "pred.model", "gen.param", "order.impdat")

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # Check input 'x'
    if (isTRUE(grepl("...", x, fixed = TRUE))) {

      # Semicolon included
      unlist(strsplit(x, ""))[as.numeric(gregexec("\\.\\.\\.", x)[[1L]]) + 3L] |>
        (\(z) if (isTRUE(z != ";" || is.na(z))) { stop("Please include the semicolon ; when using the \"...;\" specification.", call. = FALSE)} )()

    }

    # Check input 'file'
    if (isTRUE(!is.character(file) || length(file) != 1L)) { stop("Please specify a character string for the argument 'file'.", call. = FALSE) }

    # Check input 'data'
    if (isTRUE(!is.null(data))) {

      # Data frame or matrix for
      if (isTRUE(!is.data.frame(data) && !is.matrix(data))) { stop("Please specify a data frame or matrix for the argument 'data'.", call. = FALSE) }

      # VARIABLES specification
      if (isTRUE(grepl("VARIABLES:", toupper(x)))) { stop("Please do not specify the VARIABLE command in the Blimp input text when using the argument 'data'.", call. = FALSE) }

    }

    # Check input 'comment'
    if (isTRUE(!is.logical(comment))) { stop("Please specify TRUE or FALSE for the argument 'comment'.", call. = FALSE) }

    # Check input 'replace.inp'
    if (isTRUE(!is.logical(replace.inp))) { stop("Please specify TRUE or FALSE for the argument 'replace.inp'.", call. = FALSE) }

    # Check input 'blimp.run'
    if (isTRUE(!is.logical(blimp.run))) { stop("Please specify TRUE or FALSE for the argument 'blimp.run'.", call. = FALSE) }

    # Check input 'posterior'
    if (isTRUE(!is.logical(posterior))) { stop("Please specify TRUE or FALSE for the argument 'posterior'.", call. = FALSE) }

    # Check input 'folder'
    if (isTRUE(!is.character(folder))) { stop("Please specify a character string for the argument 'folder'.", call. = FALSE) }

    # Check input 'format'
    if (isTRUE(!all(format %in% c("csv", "csv2", "excel", "rds", "workspace")))) { stop("Character string in the argument 'format' does not match with \"csv\", \"csv2\", \"excel\", \"rds\", or \"workspace\".", call. = FALSE) }

    # Check input 'clear'
    if (isTRUE(!is.logical(clear))) { stop("Please specify TRUE or FALSE for the argument 'clear'.", call. = FALSE) }

    # Check input 'replace.out'
    if (isTRUE(!all(replace.out %in% c("always", "never", "modified")))) { stop("Character strings in the argument 'print' do not all match with \"always\", \"never\", or \"modified\".", call. = FALSE) }

    if (isTRUE(!all(c("always", "never", "modified") %in% replace.out) && length(replace.out) != 1L)) { stop("Please specify a character string for the argument 'replace.out'", call. = FALSE) }

    # Check input 'result'
    result[which(!result %in% c("all", "default", result.all))] |>
      (\(z) if (isTRUE(length(z) != 0L)) { stop(paste0(if (isTRUE(length(z) == 1L)) { "Character string " } else { "Character vector " }, "specified in the argument 'result' is not permissible: ", paste(dQuote(z), collapse = ", ")), call. = FALSE) } )()

    # Check input 'exclude'
    exclude.check <- exclude[which(!exclude %in% result.all)] |>
      (\(z) if (isTRUE(length(z) != 0L)) { stop(paste0(if (isTRUE(length(z) == 1L)) { "Character string " } else { "Character vector " }, "specified in the argument 'exclude' is not permissible: ", paste(dQuote(z), collapse = ", ")), call. = FALSE) })()

    # Check input 'color'
    if (isTRUE(!all(color %in% c("none", "black", "red", "green", "yellow", "blue", "violet", "cyan", "white", "gray", "b.red", "b.green", "b.yellow", "b.blue", "b.violet", "b.cyan", "b.white")))) { stop(paste0(if (isTRUE(length(color) == 1L)) { "Character string " } else { "Character vector " }, "specified in the argument 'color' is not permissible."), call. = FALSE) }

    if (isTRUE(!all(c("none", "blue", "violet") %in% color))) { if (isTRUE(length(color) != 2L)) { stop("Please specify a vector with two elements for the argument 'color'.", call. = FALSE) } }

    # Check input 'style'
    if (isTRUE(!all(style %in% c("regular", "bold", "italic", "underline")))) { stop("Character vector in the argument 'style' does not match with \"regular\", \"bold\", \"italic\", or \"underline\".", call. = FALSE) }

    if (isTRUE(length(style) != 2L)) { stop("Please specify a vector with two elements for the argument 'style'.", call. = FALSE) }

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

  if (isTRUE(all(c("csv", "csv2", "excel", "rds", "workspace") %in% format))) {

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

    x <- paste(unlist(lapply(strsplit(unlist(strsplit(x, "\n")), ""), function(y) {

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
  ## Upper Case Characters ####

  x.upp <- toupper(x)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Extract Position of Input Command Sections ####

  section.pos <- c(if (isTRUE(any(grepl("DATA:", x.upp)))) { as.numeric(gregexec("DATA:", toupper(x))[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("VARIABLES:", x.upp)))) {

                     setdiff(as.numeric(gregexec("VARIABLES:", toupper(x))[[1L]]), as.numeric(gregexec(paste0("\\.", "VARIABLES:"), toupper(x))[[1L]]) + 1L)

                    } else {

                      NULL

                    },
                   if (isTRUE(any(grepl("CLUSTERID:", x.upp)))) { as.numeric(gregexec("CLUSTERID:", x.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("ORDINAL:", x.upp)))) { as.numeric(gregexec("ORDINAL:", x.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("NOMINAL:", x.upp)))) { as.numeric(gregexec("NOMINAL:", x.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("COUNT:", x.upp)))) { as.numeric(gregexec("COUNT:", x.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("WEIGHT:", x.upp)))) { as.numeric(gregexec("WEIGHT:", x.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("MISSING:", x.upp)))) { as.numeric(gregexec("MISSING:", x.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("LATENT:", x.upp)))) { as.numeric(gregexec("LATENT:", x.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("RANDOMEFFECT:", x.upp)))) { as.numeric(gregexec("RANDOMEFFECT:", x.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("TRANSFORM:", x.upp)))) { as.numeric(gregexec("TRANSFORM:", x.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("BYGROUP:", x.upp)))) { as.numeric(gregexec("BYGROUP:", x.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("FIXED:", x.upp)))) { as.numeric(gregexec("FIXED:", x.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("CENTER:", x.upp)))) { as.numeric(gregexec("CENTER:", x.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("MODEL:", x.upp)))) {

                     setdiff(as.numeric(gregexec("MODEL:", x.upp)[[1L]]),
                             c(as.numeric(gregexec(paste0("\\.", "MODEL:"), x.upp)[[1L]]) + 1L, as.numeric(gregexec(paste0("\\_", "MODEL:"), x.upp)[[1L]]) + 1L))

                    } else {

                      NULL

                    },
                   if (isTRUE(any(grepl("SIMPLE:", x.upp)))) { as.numeric(gregexec("SIMPLE:", x.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("PARAMETERS:", x.upp)))) { as.numeric(gregexec("PARAMETERS:", x.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("TEST:", x.upp)))) { as.numeric(gregexec("TEST:", x.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("FCS:", x.upp)))) { as.numeric(gregexec("FCS:", x.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("SIMULATE:", x.upp)))) { as.numeric(gregexec("SIMULATE:", x.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("SEED:", x.upp)))) { as.numeric(gregexec("SEED:", x.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("BURN:", x.upp)))) { as.numeric(gregexec("BURN:", x.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("ITERATIONS:", x.upp)))) { as.numeric(gregexec("ITERATIONS:", x.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("CHAINS:", x.upp)))) { as.numeric(gregexec("CHAINS:", x.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("NIMPS:", x.upp)))) { as.numeric(gregexec("NIMPS:", x.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("THIN:", x.upp)))) { as.numeric(gregexec("THIN:", x.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("OPTIONS:", x.upp)))) { as.numeric(gregexec("OPTIONS:", x.upp)[[1L]]) } else { NULL },
                   if (isTRUE(any(grepl("OUTPUT:", x.upp)))) {

                     setdiff(as.numeric(gregexec("OUTPUT:", toupper(x))[[1L]]), as.numeric(gregexec(paste0("\\.", "OUTPUT:"), toupper(x))[[1L]]) + 1L)

                    } else {

                      NULL

                    },
                   if (isTRUE(any(grepl("SAVE:", x.upp)))) { as.numeric(gregexec("SAVE:", x.upp)[[1L]]) } else { NULL })

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Extract Input Command Sections ####

  bdata <- variables <- ordinal <- nominal <- count <- clusterid <- weight <- missing <- latent <- randomeffect <- transform <- bygroup <- fixed <- center <- model <- simple <- parameters <- test <- fcs <- simulate <- seed <- burn <- iterations <- chains <- nimps <- thin <- options <- boutput <- save <- NULL

  #...................
  ### DATA ####

  if (isTRUE(any(grepl("DATA:", x.upp)))) { bdata <- .extract.section("DATA:", x, section.pos) }

  #...................
  ### VARIABLES ####

  if (isTRUE(any(grepl("VARIABLES:", x.upp)))) { variables <- .extract.section("VARIABLES:", x, section.pos) }

  #...................
  ### ORDINAL ####

  if (isTRUE(any(grepl("ORDINAL:", x.upp)))) { ordinal <- .extract.section("ORDINAL:", x, section.pos) }

  #...................
  ### NOMINAL ####

  if (isTRUE(any(grepl("NOMINAL:", x.upp)))) { nominal <- .extract.section("NOMINAL:", x, section.pos) }

  #...................
  ### COUNT ####

  if (isTRUE(any(grepl("COUNT:", x.upp)))) { count <- .extract.section("COUNT:", x, section.pos) }

  #...................
  ### CLUSTERID ####

  if (isTRUE(any(grepl("CLUSTERID:", x.upp)))) { clusterid <- .extract.section("CLUSTERID:", x, section.pos) }

  #...................
  ### WEIGHT ####

  if (isTRUE(any(grepl("WEIGHT:", x.upp)))) { weight <- .extract.section("WEIGHT:", x, section.pos) }

  #...................
  ### MISSING ####

  if (isTRUE(any(grepl("MISSING:", x.upp)))) { missing <- .extract.section("MISSING:", x, section.pos) }

  #...................
  ### LATENT ####

  if (isTRUE(any(grepl("LATENT:", x.upp)))) { latent <- .extract.section("LATENT:", x, section.pos) }

  #...................
  ### RANDOMEFFECT ####

  if (isTRUE(any(grepl("RANDOMEFFECT:", x.upp)))) { randomeffect <- .extract.section("RANDOMEFFECT:", x, section.pos) }

  #...................
  ### TRANSFORM ####

  if (isTRUE(any(grepl("TRANSFORM:", x.upp)))) { transform <- .extract.section("TRANSFORM:", x, section.pos) }

  #...................
  ### BYGROUP ####

  if (isTRUE(any(grepl("BYGROUP:", x.upp)))) { bygroup <- .extract.section("BYGROUP:", x, section.pos) }

  #...................
  ### FIXED ####

  if (isTRUE(any(grepl("FIXED:", x.upp)))) { fixed <- .extract.section("FIXED:", x, section.pos) }

  #...................
  ### CENTER ####

  if (isTRUE(any(grepl("CENTER:", x.upp)))) { center <- .extract.section("CENTER:", x, section.pos) }

  #...................
  ### MODEL ####

  if (isTRUE(any(grepl("MODEL:", x.upp)))) { model <- .extract.section("MODEL:", x, section.pos) }

  #...................
  ### SIMPLE ####

  if (isTRUE(any(grepl("SIMPLE:", x.upp)))) { simple <- .extract.section("SIMPLE:", x, section.pos) }

  #...................
  ### PARAMETERS ####

  if (isTRUE(any(grepl("PARAMETERS:", x.upp)))) { parameters <- .extract.section("PARAMETERS:", x, section.pos) }

  #...................
  ### TEST ####

  if (isTRUE(any(grepl("TEST:", x.upp)))) { test <- .extract.section("TEST:", x, section.pos) }

  #...................
  ### FCS ####

  if (isTRUE(any(grepl("FCS:", x.upp)))) { fcs <- .extract.section("FCS:", x, section.pos) }

  #...................
  ### SIMULATE ####

  if (isTRUE(any(grepl("SIMULATE:", x.upp)))) { simulate <- .extract.section("SIMULATE:", x, section.pos) }

  #...................
  ### SEED ####

  if (isTRUE(any(grepl("SEED:", x.upp)))) { seed <- .extract.section("SEED:", x, section.pos) }

  #...................
  ### BURN ####

  if (isTRUE(any(grepl("BURN:", x.upp)))) { burn <- .extract.section("BURN:", x, section.pos) }

  #...................
  ### ITERATIONS ####

  if (isTRUE(any(grepl("ITERATIONS:", x.upp)))) { iterations <- .extract.section("ITERATIONS:", x, section.pos) }

  #...................
  ### CHAINS ####

  if (isTRUE(any(grepl("CHAINS:", x.upp)))) { chains <- .extract.section("CHAINS:", x, section.pos) }

  #...................
  ### NIMPS ####

  if (isTRUE(any(grepl("NIMPS:", x.upp)))) { nimps <- .extract.section("NIMPS:", x, section.pos) }

  #...................
  ### THIN ####

  if (isTRUE(any(grepl("THIN:", x.upp)))) { thin <- .extract.section("THIN:", x, section.pos) }

  #...................
  ### OPTIONS ####

  if (isTRUE(any(grepl("OPTIONS:", x.upp)))) { options <- .extract.section("OPTIONS:", x, section.pos) }

  #...................
  ### OUTPUT ####

  if (isTRUE(any(grepl("OUTPUT:", x.upp)))) { boutput <- .extract.section("OUTPUT:", x, section.pos) }

  #...................
  ### SAVE ####

  if (isTRUE(any(grepl("SAVE:", x.upp)))) { save <- .extract.section("SAVE:", x, section.pos) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Variable Names ####

  if (isTRUE(!is.null(data))) {

    # Colon position
    col.pos <- as.numeric(gregexec(":", toupper(bdata)))

    # Space between : and input text
    n.space <- diff(c(col.pos, (which(unlist(strsplit(bdata, "")) != " ") |>
                                  (\(y) y[y > col.pos][1L])()))) - 1L

    if (isTRUE(n.space <= 5L)) {

      n.space <- 1L

    } else {

      n.space <- n.space - 4L

    }

    names.are <- names.temp <- names.length <- paste0(rep(" ", times = n.space), collapse = "")

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

    variables <- paste0(c("VARIABLES:", paste(rep(" ", times = n.space), collapse = ""), misty::chr.trim(names.are), ";"), collapse = "")

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Input Object ####

  input.object <- list(data = bdata, variables = variables, clusterid = clusterid,  ordinal = ordinal, nominal = nominal, count = count,
                       weight = weight, missing = missing, latent = latent, randomeffect = randomeffect, transform = transform,
                       bygroup = bygroup, fixed = fixed, center = center, model = model, simple = simple, parameters = parameters,
                       test = test, fcs = fcs, simulate = simulate, seed = seed, burn = burn, iterations = iterations,
                       chains = chains, nimps = nimps, thin = thin, options = options, output = boutput, save = save)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Write Object ####

  write.object <- paste(unlist(Filter(Negate(is.null), input.object)), collapse = "\n")

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
                       folder = folder, format = format, clear = clear, Blimp = Blimp, check = FALSE)

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
                 x = x,
                 args = list(file = file, data = data, comment = comment, replace.inp = replace.inp,
                             blimp.run = blimp.run, posterior = posterior, folder = folder, format = format,
                             clear = clear, replace.out = replace.out, Blimp = Blimp, result = result,
                             exclude = exclude, color = color, style = style, not.result = not.result,
                             write = write, append = append, check = check, output = output),
                 input = input.object, write = write.object,
                 result = result.object$result)

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  return(invisible(object))

}
