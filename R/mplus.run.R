#' Run Mplus Models
#'
#' This function runs a group of Mplus models (\code{.inp} files) located within
#' a single directory or nested within subdirectories.
#'
#' @param target         a character string indicating the directory containing
#'                       Mplus input files (\code{.inp})
#'                       to run or the single \code{.inp} file to be run. May be
#'                       a full path, relative path, or a filename within the
#'                       working directory.
#' @param recursive      logical: if \code{TRUE}, run all models nested in
#'                       subdirectories within directory. Not relevant if target
#'                       is a single file.
#' @param filefilter     a Perl regular expression (PCRE-compatible) specifying
#'                       particular input files to be run within directory. See
#'                       regex or http://www.pcre.org/pcre.txt for details about
#'                       regular expression syntax. Not relevant if target is a
#'                       single file.
#' @param show.out       logical: if \code{TRUE}, estimation output (\code{TECH8})
#'                       is show on the R console. Note that if run within Rgui,
#'                       output will display within R, but if run via Rterm,
#'                       a separate window will appear during estimation.
#' @param replace.out    a character string for specifying three settings:
#'                       \code{"always"} (default), which runs all models, regardless
#'                       of whether an output file for the model exists, \code{"never"},
#'                       which does not run any model that has an existing output file,
#'                       and \code{"modified"}, which only runs a model if the
#'                       modified date for the input file is more recent than the
#'                       output file modified date.
#' @param message        logical: if \code{TRUE}, message \code{Running model:}
#'                       and \code{System command:} is pringted on the console.
#' @param logFile        a character string specifying a file that records the settings
#'                       passed into the function and the models run (or skipped)
#'                       during the run.
#' @param Mplus          a character string for specifying the name or path of the
#'                       Mplus executable to be used for running models. This covers
#'                       situations where Mplus is not in the system's path, or where
#'                       one wants to test different versions of the Mplus program.
#'                       Note that there is no need to specify this argument for most
#'                       users since it has intelligent defaults.
#' @param killOnFail     logical: if \code{TRUE} (default), all processes named
#'                       mplus.exe when \code{mplus.run()} does not terminate
#'                       normally are killed. Windows only.
#' @param local_tmpdir   logical: if \code{TRUE}, the TMPDIR environment variable
#'                       is set to the location of the \code{.inp file} prior to
#'                       execution. This is useful in Monte Carlo studies where
#'                       many instances of Mplus may run in parallel and we wish
#'                       to avoid collisions in temporary files among processes.
#'                       Linux/Mac only.
#'
#' @author
#' Hadley Wickham, Romain Francois, Lionel Henry, and Kirill Müller, and Davis Vaughan.
#'
#' @seealso
#' \code{\link{read.mplus}}, \code{\link{write.mplus}}, \code{\link{mplus.print}},
#' \code{\link{mplus}}, \code{\link{mplus.update}}, \code{\link{mplus.lca}}
#'
#' @references
#' Hallquist, M. N. & Wiley, J. F. (2018). MplusAutomation: An R package for facilitating
#' large-scale latent variable analyses in Mplus. \emph{Structural Equation Modeling:
#' A Multidisciplinary Journal, 25}, 621-638. https://doi.org/10.1080/10705511.2017.1402334.
#'
#' Muthen, L. K., & Muthen, B. O. (1998-2017). \emph{Mplus User's Guide} (8th ed.).
#' Muthen & Muthen.
#'
#' @return
#' None.
#'
#' @note
#' This function is a copy of the \code{runModels()} function in the
#' \pkg{MplusAutomation} package by Michael Hallquist and Joshua Wiley (2018).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1: Run Mplus models located within a single directory
#' run.mplus(Mplus = "C:/Program Files/Mplus/Mplus.exe")
#'
#' # Example 2: Run Mplus models located nested within subdirectories
#' run.mplus(recursive = TRUE,
#'           Mplus = "C:/Program Files/Mplus/Mplus.exe")
#' }
mplus.run <- function(target = getwd(), recursive = FALSE, filefilter = NULL, show.out = FALSE,
                      replace.out = c("always", "never", "modified"), message = TRUE,
                      logFile = NULL, Mplus = "Mplus", killOnFail = TRUE, local_tmpdir = FALSE) {

  #_____________________________________________________________________________
  #
  # Additional Functions -------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Split File and Path into Separate Parts ####

  splitFilePath <- function(abspath) {

    if (isTRUE(!is.character(abspath))) stop("Path not a character string", call. = FALSE)

    if (isTRUE(nchar(abspath) < 1L || is.na(abspath))) stop("Path is missing or of zero length", call. = FALSE)

    # trailing slash screws up file.exists call on Windows: https://bugs.r-project.org/bugzilla/show_bug.cgi?id=14721
    abspath <- sub("(\\\\|/)?$", "", abspath, perl = TRUE)

    components <- strsplit(abspath, split = "[\\/]")[[1L]]
    lcom <- length(components)

    stopifnot(lcom > 0L)

    # the file is the last element in the list. In the case of length == 1, this will extract the only element.
    relFilename <- components[lcom]
    absolute <- FALSE

    if (isTRUE(lcom == 1L)) {

      dirpart <- NA_character_

    } else if (isTRUE(lcom > 1L)) {

      components <- components[-lcom]
      dirpart <- do.call("file.path", as.list(components))

      #if path begins with C:, /, ~/, //, or \\, then treat as absolute
      if (isTRUE(grepl("^([A-Z]{1}:|~/|/|//|\\\\)+.*$", dirpart, perl = TRUE))) absolute <- TRUE

    }

    return(list(directory = dirpart, filename = relFilename, absolute = absolute))

  }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  stopifnot(replace.out %in% c("always", "never", "modified"))

  if (isTRUE(length(target) > 1L)) { stop("Target for run.mplus() must be a single file or single directory.", call. = FALSE) }

  curdir <- getwd()

  if (isTRUE(grepl(".*\\.inp?$", target, perl = TRUE))) {

    directory <- dirname(target)
    filelist <- basename(target)
    if (isTRUE(!is.null(filefilter))) {

      warning("Using run.mplus() with a single .inp target ignores filefilter.", call. = FALSE)

    }

    if (isTRUE(!file.exists(target))) { stop("run.mplus() cannot locate target file: ", target, call. = FALSE) }

    setwd(directory)

    if (isTRUE(file.exists(outtest <- sub("\\.inp?$", ".out", filelist, perl = TRUE)))) { filelist <- c(filelist, outtest) }

  } else {

    directory <- sub("(\\\\|/)?$", "", target, perl = TRUE)

    if (isTRUE(.Platform$OS.type == "windows" && grepl("^[a-zA-Z]:$", directory))) {

      directory <- paste0(directory, "/")

    }

    if (isTRUE(!file.exists(directory))) { stop("run.mplus() cannot change to directory: ", directory, call. = FALSE) }

    setwd(directory)
    filelist <- list.files(recursive = recursive, pattern = filefilter)

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  # Argument replace.out
  if (isTRUE(all(c("always", "never", "modified") %in% replace.out))) { replace.out <- "always" }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  normalComplete <- FALSE

  if (isTRUE(!is.null(logFile))) {

    logTarget <- file(description = logFile, open = "wt", blocking = TRUE)

    writeLines(c(paste0("------Begin Mplus Model Run: ",
                       format(Sys.time(), "%d%b%Y %H:%M:%S"), "------"),
                 paste0("Target directory: ", directory), "Run options:",
                 paste("\tRecursive (run models in subdirectories):",
                       as.character(recursive)), paste("\tShow output on console:", as.character(show.out)),
                 paste("\tReplace existing outfile:", replace.out), "------"), con = logTarget)

    flush(logTarget)

  }

  isLogOpen <- function() {

    if (isTRUE(is.null(logFile))) {

      return(FALSE)
    }

    connections <- data.frame(showConnections(all = FALSE), stringsAsFactors = FALSE)

    if (isTRUE(length(grep(splitFilePath(logFile)$filename, connections$description, ignore.case = TRUE)) > 0L)) {

      return(TRUE)

    } else {

      return(FALSE)

    }

  }

  exitRun <- function() {

    deleteOnKill <- TRUE

    if (isTRUE(normalComplete == FALSE && isLogOpen())) {

      writeLines("Run terminated abnormally", logTarget)
      flush(logTarget)

    }

    if (isTRUE(.Platform$OS.type == "windows" && normalComplete == FALSE && killOnFail == TRUE)) {

      processList <-  plyr::ldply(strsplit(shell("wmic process get caption, processid", intern = TRUE), split = "\\s+", perl = TRUE),

                           function(element) {

                             return(data.frame(procname = element[1L], pid = element[2L], stringsAsFactors = FALSE))

                           })

      if (isTRUE(length(grep("mplus.exe", processList$procname, ignore.case = TRUE)) > 0L)) {

        if (isTRUE(isLogOpen())) {

          writeLines("Killing wayward Mplus processes", logTarget)
          flush(logTarget)

        }

        shell("taskkill /f /im mplus.exe")

        if (isTRUE(deleteOnKill == TRUE)) {

          noExtension <- substr(absFilename, length(absFilename) - 4L, length(absFilename))
          outDelete <- paste(noExtension, ".out", sep = "")
          gphDelete <- paste(noExtension, ".gph", sep = "")

          if (isTRUE(file.exists(outDelete))) {

            unlink(outDelete)

            if (isTRUE(isLogOpen())) {

              writeLines(paste("Deleting unfinished output file:", outDelete), logTarget)

              flush(logTarget)

            }

          }

          if (isTRUE(file.exists(gphDelete))) {

            unlink(gphDelete)

            if (isTRUE(isLogOpen())) {

              writeLines(paste("Deleting unfinished graph file:", gphDelete), logTarget)

              flush(logTarget)

            }

          }

        }

      }

    }

    if (isTRUE(isLogOpen())) {

      close(logTarget)

    }

    setwd(curdir)

  }

  on.exit(exitRun())
  inpfiles <- filelist[grep(".*\\.inp?$", filelist, ignore.case = TRUE)]
  outfiles <- filelist[grep(".*\\.out$", filelist, ignore.case = TRUE)]

  if (isTRUE(length(inpfiles) < 1L)) { stop("No Mplus input files detected in the target directory: ", directory) }

  dropOutExtensions <- sapply(outfiles, function(x) {

    if (isTRUE(nchar(x) >= 4L))

      return(tolower(substr(x, 1L, (nchar(x) - 4L))))

  })

  for (i in seq_len(length(inpfiles))) {

    if (isTRUE(!replace.out == "always")) {

      if (isTRUE(tolower(sub("\\.inp?$", "", inpfiles[i], perl = TRUE)) %in% dropOutExtensions)) {

        if (isTRUE(replace.out == "modified")) {

          inpmtime <- file.info(inpfiles[i])$mtime

          matchPos <- grep(tolower(substr(inpfiles[i], 1, (nchar(inpfiles[i]) - 4L))), dropOutExtensions)

          if (isTRUE(length(matchPos) < 1L)) {

            warning("Could not locate matching outfile", call. = FALSE)

          }

          outmtime <- file.info(outfiles[matchPos[1L]])$mtime

          if (isTRUE(inpmtime <= outmtime)) {

            if (isTRUE(isLogOpen())) {

              writeLines(paste("Skipping model because output file is newer than input file:", inpfiles[i]), logTarget)

              flush(logTarget)

            }

            next

          }

        } else if (isTRUE(replace.out == "never")) {

          if (isTRUE(isLogOpen())) {

            writeLines(paste("Skipping model because output file already exists for:", inpfiles[i]), logTarget)

            flush(logTarget)

          }

          next

        }

      }

    }

    inputSplit <- splitFilePath(inpfiles[i])
    if (isTRUE(is.na(inputSplit$directory))) {

       dirtocd <- directory

    } else {

      dirtocd <- file.path(directory, inputSplit$directory)

    }

    absFilename <- file.path(directory, inpfiles[i])
    if (isTRUE(.Platform$OS.type == "unix" && Mplus == "Mplus")) {

      if (isTRUE(Sys.info()["sysname"] == "Darwin")) {

        Mplus <- "/Applications/Mplus/mplus"

      } else {

        Mplus <- "mplus"

      }

    }

    command <- paste("cd \"", dirtocd, "\" && \"", Mplus, "\" \"", inputSplit$filename, "\"", sep = "")

    if (isTRUE(.Platform$OS.type == "windows")) {

      command <- chartr("/", "\\", command)
      shellcommand <- Sys.getenv("COMSPEC")
      flag <- "/c"
      command <- paste(shellcommand, flag, command)

    } else if (isTRUE(.Platform$OS.type == "unix")) {
    }

    if (isTRUE(isLogOpen())) {

      writeLines(paste("Currently running model:", inputSplit$filename), logTarget)

      flush(logTarget)

    }

    if (isTRUE(message)) {

      cat("\nRunning Model:", inputSplit$filename, "\n")
      cat("System Command:", command, "\n")

    }

    if (isTRUE(.Platform$OS.type == "windows")) {

      system(command, show.output.on.console = show.out, invisible = (!show.out), wait = TRUE)

    } else {

      if (isTRUE(show.out)) {

        stdout.value <- ""

      }  else {

        stdout.value <- NULL

      }

      oldwd <- getwd()
      setwd(dirtocd)
      if (isTRUE(local_tmpdir)) {

        Sys.setenv(TMPDIR = dirtocd)

      }

      exitCode <- system2(Mplus, args = c(shQuote(inputSplit$filename)), stdout = stdout.value, wait = TRUE)

      if (isTRUE(exitCode > 0L)) {

        warning("Mplus returned error code: ", exitCode, ", for model: ", inputSplit$filename, "\n", call. = FALSE)

      }

      setwd(oldwd)

    }

  }

  if (isTRUE(isLogOpen())) {

    writeLines(c("", paste("------End Mplus Model Run: ",
                           format(Sys.time(), "%d%b%Y %H:%M:%S"), "------",
                           sep = "")), logTarget)
    flush(logTarget)

  }

  normalComplete <- TRUE

}
