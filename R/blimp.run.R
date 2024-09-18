#' Run Blimp Models
#'
#' This function runs a group of Blimp models (\code{.imp} files) located within
#' a single directory or nested within subdirectories.
#'
#' @param target      a character string indicating the directory containing
#'                    Blimp input files (\code{.imp}) to run, a character string
#'                    indicating a single \code{.imp} file to run, or a character
#'                    vector for multiple \code{.imp} files to run. May be a full
#'                    path, relative path, a file name, or a vector of file names
#'                    within the working directory.
#' @param recursive   logical: if \code{TRUE}, run all models nested in
#'                    subdirectories within a directory. Not relevant if a single
#'                    or multiple \code{.imp} files were specified for the argument
#'                    \code{target}.
#' @param replace.out a character string for specifying three settings, i.e.,
#'                    \code{"always"} (default), which runs all models, regardless
#'                    of whether an output file for the model exists, \code{"never"},
#'                    which does not run any model that has an existing output
#'                    file, and \code{"modified"}, which only runs a model if
#'                    the modified date for the input file is more recent than
#'                    the output file modified date.
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
#' @param clear       logical: if \code{TRUE}, the console is cleared after
#'                    estimating each model.
#' @param Blimp       a character string for specifying the name or path of the
#'                    Blimp executable to be used for running models. This covers
#'                    situations where Blimp is not in the system's path, or where
#'                    one wants to test different versions of the Blimp program.
#'                    Note that there is no need to specify this argument for most
#'                    users since it has intelligent defaults.
#' @param check       logical: if \code{TRUE} (default), argument specification is
#'                    checked.
#'
#' @author
#' Takuya Yanagida
#'
#' @seealso
#' \code{\link{blimp}}, \code{\link{blimp.update}},
#' \code{\link{blimp.print}}, \code{\link{blimp.plot}}, \code{\link{blimp.bayes}}
#'
#' @references
#' Keller, B. T., & Enders, C. K. (2023). \emph{Blimp user’s guide} (Version 3).
#' Retrieved from www.appliedmissingdata.com/blimp
#'
#' Keller B (2024). \emph{rblimp: Integration of Blimp Software into R}. R package
#' version 0.1.31. https://github.com/blimp-stats/rblimp
#'
#' @return
#' None.
#'
#' @note
#' This function is based on the \code{detect_blimp()} and \code{rblimp()} function
#' in the \pkg{rblimp} package by Brian T.Keller (2024).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1: Run Blimp models located within the current working directory
#' blimp.run()
#'
#' # Example 2: Run Blimp models located nested within subdirectories
#' blimp.run(recursive = TRUE)
#'
#' # Example 3: Run Blimp input file
#' blimp.run("Ex4.1a.imp")
#'
#' # Example 4: Run Blimp input files
#' blimp.run(c("Ex4.1a.imp", "Ex4.1b.imp"))
#'
#' # Example 5: Run Blimp models, save posterior distribution in a R workspace
#' blimp.run(posterior = TRUE, format = "RData")
#' }
blimp.run <- function(target = getwd(), recursive = FALSE,
                      replace.out = c("always", "never", "modified"),
                      posterior = FALSE, folder = "Posterior_",
                      format = c("csv", "csv2", "xlsx", "rds", "RData"),
                      clear = FALSE, Blimp = detect.blimp(), check = TRUE) {

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # Check input 'recursive'
    if (isTRUE(!is.logical(recursive))) { stop("Please specify TRUE or FALSE for the argument 'recursive'.", call. = FALSE) }

    # Check input 'replace.out'
    if (isTRUE(!all(replace.out %in% c("always", "never", "modified")))) { stop("Character string in the argument 'replace.out' does not match with \"always\", \"never\", or \"modified\".", call. = FALSE) }

    # Check input 'posterior'
    if (isTRUE(!is.logical(posterior))) { stop("Please specify TRUE or FALSE for the argument 'posterior'.", call. = FALSE) }

    # Check input 'folder'
    if (isTRUE(!is.character(folder))) { stop("Please specify a character string for the argument 'folder'.", call. = FALSE) }

    # Check input 'format'
    if (isTRUE(!all(format %in% c("csv", "csv2", "xlsx", "rds", "RData")))) { stop("Character string in the argument 'format' does not match with \"csv\", \"csv2\", \"xlsx\", \"rds\", or \"RData\".", call. = FALSE) }

    # Check input 'clear'
    if (isTRUE(!is.logical(clear))) { stop("Please specify TRUE or FALSE for the argument 'clear'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## target ####

  #...................
  ### One or multiple target files ####

  if (isTRUE(any(grepl("\\.imp", target)))) {

    # Check if files exist
    if (isTRUE(any(!sapply(target, file.exists)))) {

      target.exists <- !sapply(target, file.exists)

      if (isTRUE(sum(target.exists) == 1L)) {

        warning(paste0("File specified in the argument 'target' does not exist: ", names(which(target.exists))), call. = FALSE)

      } else {

        warning(paste0("Files specified in the argument 'target' do not exist: ", paste(names(which(target.exists)), collapse = ", ")), call. = FALSE)

      }

      target.imp <- names(which(!target.exists))

      if (isTRUE(length(target.imp) == 0L)) { stop("There are no existing \".imp\" tiles specified in the argument 'target'.", call. = FALSE) }

    } else {

      target.imp <- target

    }

  #...................
  ### Files within (sub-)directories ####

  } else {

    target.imp <- list.files(target, pattern = "\\.imp", full.names = TRUE, recursive = recursive)

    if (isTRUE(length(target.imp) == 0L)) { stop("No \".imp\" file found in the folder specified in the argument 'target'.", call. = FALSE) }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## replace.out ####

  # Argument replace.out
  if (isTRUE(all(c("always", "never", "modified") %in% replace.out))) { replace.out <- "always" }

  if (isTRUE(replace.out != "always")) {

    # Output files
    target.out <- sub(".imp", ".blimp-out", target.imp)

    # Never replace
    if (isTRUE(replace.out == "never")) {

      target.out.exist <- sapply(target.out, file.exists)
      if (isTRUE(any(target.out.exist))) { target.imp <- target.imp[-which(target.out.exist)] }

      if (isTRUE(length(target.imp) == 0L)) { stop("There is no input file without an output file to estimate.", call. = FALSE) }

    # Modified date
    } else {

      target.out.exist <- sapply(target.out, function(y) {

        !file.exists(y) || (file.info(y)$mtime < file.info(sub(".blimp-out", ".imp", y))$mtime)

      })

      if (isTRUE(all(!target.out.exist))) { stop("There is no input file without an output file or more recent input than the output file to estimate.", call. = FALSE) }

      target.imp <- target.imp[which(target.out.exist)]

    }

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

  invisible(sapply(target.imp, function(y) {

    if (isTRUE(length(target.imp) != 1L)) { cat("\n", y) }

    .blimp.source(y, Blimp = Blimp, posterior = posterior, folder = folder, format = format, clear = clear)

    }))

}
