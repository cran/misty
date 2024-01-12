#' Load and Attach Multiple Packages
#'
#' This function loads and attaches multiple add-on packages at once.
#'
#' @param ...     the names of the packages to be loaded, given as names
#'                (e.g., \code{misty, lavaan, lme4}), or  literal character
#'                strings (e.g., \code{"misty", "lavaan", "lme4"}), or character
#'                vector (e.g., \code{c("misty", "lavaan", "lme4")}).
#' @param install logical: if \code{TRUE}, missing packages and dependencies are
#'                installed.
#' @param quiet   logical: if \code{TRUE} (default), startup messages when loading
#'                package are disabled.
#' @param check   logical: if \code{TRUE} (default), argument specification is checked.
#' @param output  logical: if \code{TRUE} (default), output is shown on the console.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso \code{\link{library}}, \code{\link{require}}
#'
#' @references
#' Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) \emph{The New S Language}.
#' Wadsworth & Brooks/Cole.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1: Load packages using the names of the packages
#' misty::libraries(misty, lme4, lmerTest)
#'
#' # Example 2: Load packages using literal character strings
#' misty::libraries("misty", "lme4", "lmerTest")
#'
#' # Example 3: Load packages using a character vector
#' misty::libraries(c("misty", "lme4", "lmerTest"))
#'
#' # Example 4: Check packages, i.e., TRUE = all depends/imports/suggests installed
#' misty::libraries(misty, lme4, lmerTest, output = FALSE)$result$restab
#'
#' # Example 5: Depends, FALSE = not installed, TRUE = installed
#' misty::libraries(misty, lme4, lmerTest, output = FALSE)$result$depends
#'
#' # Example 6: Imports, FALSE = not installed, TRUE = installed
#' misty::libraries(misty, lme4, lmerTest, output = FALSE)$result$imports
#'
#' # Example 6: Suggests, FALSE = not installed, TRUE = installed
#' misty::libraries(misty, lme4, lmerTest, output = FALSE)$result$suggests
#' }
libraries <- function(..., install = FALSE, quiet = TRUE, check = TRUE,
                      output = TRUE) {

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # Check input 'install'
    if (isTRUE(!is.logical(install))) { stop("Please specify TRUE or FALSE for the argument 'install'.", call. = FALSE) }

    # Check input 'quiet'
    if (isTRUE(!is.logical(quiet))) { stop("Please specify TRUE or FALSE for the argument 'quiet'.", call. = FALSE) }

    # Check input 'output'
    if (isTRUE(!is.logical(output))) { stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Data and Arguments ---------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Extract package names from the argument ... ####

  pkg <- match.call(expand.dots = FALSE)[[2L]]

  # Name or literal character string
  if (isTRUE(length(pkg) != 1L)) {

    pkg <- as.character(pkg)

  # Character vector
  } else {

    pkg <- misty::chr.omit(as.character(pkg[[1L]]), omit = "c", check = FALSE)

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Installed, version, dependencies, imports, suggests, and loaded ####

  #...................
  ### Installed packages ####

  pkg.installed <- installed.packages()

  #...................
  ### Check if packages are installed ####

  installed <- setNames(pkg %in% row.names(pkg.installed), pkg)

  #...................
  ### Check package version ####

  version <- sapply(pkg, function(y) ifelse(y %in% row.names(pkg.installed), pkg.installed[row.names(pkg.installed) == y, "Version"],""))

  #...................
  ### Check dependencies ####

  depends <- setNames(vector(mode = "list", length = length(pkg)), pkg)
  for (i in seq_along(installed)) {

    if (isTRUE(installed[i])) {

      depends[[names(installed[i])]] <- unlist(lapply(lapply(strsplit(gsub("\n", "", pkg.installed[which(row.names(pkg.installed) %in% names(installed[i])), "Depends"]), ","),
                                                              function(y) misty::chr.omit(misty::chr.omit(misty::chr.trim(ifelse(grepl("\\(", y), yes = substr(y, 1L, unlist(gregexpr('\\(', y)) - 1L), y), check = FALSE), "R", check = FALSE), na.omit = TRUE, check = FALSE)),
                                                       function(y) setNames(y %in% row.names(pkg.installed), y)))

     }

  }

  depends.check <- unlist(sapply(depends, function(y) if (isTRUE(is.null(y))) { NA } else { all(y) }))

  #...................
  ### Check imports ####

  imports <- setNames(vector(mode = "list", length = length(pkg)), pkg)
  for (i in seq_along(installed)) {

    if (isTRUE(installed[i])) {

      imports[[names(installed[i])]] <- unlist(lapply(lapply(strsplit(gsub("\n", "", pkg.installed[which(row.names(pkg.installed) %in% names(installed[i])), "Imports"]), ","),
                                                             function(y) misty::chr.omit(misty::chr.omit(misty::chr.trim(ifelse(grepl("\\(", y), yes = substr(y, 1L, unlist(gregexpr('\\(', y)) - 1L), y), check = FALSE), "R", check = FALSE), na.omit = TRUE, check = FALSE)),
                                                      function(y) setNames(y %in% row.names(pkg.installed), y)))

    }

  }

  imports.check <- unlist(sapply(imports, function(y) if (isTRUE(is.null(y))) { NA } else { all(y) }))

  #...................
  ### Check suggests ####

  suggests <- setNames(vector(mode = "list", length = length(pkg)), pkg)
  for (i in seq_along(installed)) {

    if (isTRUE(installed[i])) {

      suggests[[names(installed[i])]] <- unlist(lapply(lapply(strsplit(gsub("\n", "", pkg.installed[which(row.names(pkg.installed) %in% names(installed[i])), "Suggests"]), ","),
                                                             function(y) misty::chr.omit(misty::chr.omit(misty::chr.trim(ifelse(grepl("\\(", y), yes = substr(y, 1L, unlist(gregexpr('\\(', y)) - 1L), y), check = FALSE), "R", check = FALSE), na.omit = TRUE, check = FALSE)),
                                                      function(y) setNames(y %in% row.names(pkg.installed), y)))

    }

  }

  suggests.check <- unlist(sapply(suggests, function(y) if (isTRUE(is.null(y))) { NA } else { all(y) }))

  #...................
  ### Loaded ####

  loaded <- installed & misty::na.as(depends.check[match(pkg, names(depends.check))], na = TRUE, check = FALSE) & misty::na.as(imports.check[match(pkg, names(imports.check))], na = TRUE, check = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Install packages and dependencies ####

    if (isTRUE(any(!c(installed, depends.check, imports.check)) && install)) {

      #...................
      ### Install packages ####

        # Check if available on CRAN
        if (isTRUE(any(!installed))) {

          install.avail <- names(installed) %in% available.packages()

          if (isTRUE(any(!install.avail))) {

            if (isTRUE(sum(!install.avail) == 1L)){

              stop(paste0("R package called '", names(installed)[!install.avail], "' is not available for this version of R."), call. = FALSE)

            } else {

              stop(paste0("Following R packages are not available for this version of R: ", paste(sapply(names(installed)[!install.avail], function(y) paste0("'", y, "'")), collapse = ", ")), call. = FALSE)

            }

          }

          install.packages(names(which(!installed)))

        }

      #...................
      ### Install depends ####

      if (isTRUE(any(!depends.check))) {

        install.packages(unique(unlist(sapply(depends, function(y) names(which(!y))))))

      }

      #...................
      ### Install imports ####

      if (isTRUE(any(!imports.check))) {

        install.packages(unique(unlist(sapply(imports, function(y) names(which(!y))))))

      }

      #...................
      ### Installed packages ####

      pkg.installed <- installed.packages()

      #...................
      ### Check if packages are installed ####

      installed <- setNames(pkg %in% row.names(pkg.installed), pkg)

      #...................
      ### Check package version ####

      version <- ifelse(installed, pkg.installed[which(row.names(pkg.installed) %in% pkg), "Version"], "")

      #...................
      ### Check dependencies ####

      depends <- setNames(vector(mode = "list", length = length(pkg)), pkg)
      for (i in seq_along(installed)) {

        if (isTRUE(installed[i])) {

          depends[[names(installed[i])]] <- unlist(lapply(lapply(strsplit(gsub("\n", "", pkg.installed[which(row.names(pkg.installed) %in% names(installed[i])), "Depends"]), ","),
                                                                 function(y) misty::chr.omit(misty::chr.omit(misty::chr.trim(ifelse(grepl("\\(", y), yes = substr(y, 1L, unlist(gregexpr('\\(', y)) - 1L), y), check = FALSE), "R", check = FALSE), na.omit = TRUE, check = FALSE)),
                                                          function(y) setNames(y %in% row.names(pkg.installed), y)))

        }

      }

      depends.check <- unlist(sapply(depends, function(y) if (isTRUE(is.null(y))) { NA } else { all(y) }))

      #...................
      ### Check imports ####

      imports <- setNames(vector(mode = "list", length = length(pkg)), pkg)
      for (i in seq_along(installed)) {

        if (isTRUE(installed[i])) {


          imports[[names(installed[i])]] <-  unlist(lapply(lapply(strsplit(gsub("\n", "", pkg.installed[which(row.names(pkg.installed) %in% names(installed[i])), "Imports"]), ","),
                                                                  function(y) misty::chr.omit(misty::chr.omit(misty::chr.trim(ifelse(grepl("\\(", y), yes = substr(y, 1L, unlist(gregexpr('\\(', y)) - 1L), y), check = FALSE), "R", check = FALSE), na.omit = TRUE, check = FALSE)),
                                                           function(y) setNames(y %in% row.names(pkg.installed), y)))

        }

      }

      imports.check <- unlist(sapply(imports, function(y) if (isTRUE(is.null(y))) { NA } else { all(y) }))

      #...................
      ### Check suggests ####

      suggests <- setNames(vector(mode = "list", length = length(pkg)), pkg)
      for (i in seq_along(installed)) {

        if (isTRUE(installed[i])) {


          suggests[[names(installed[i])]] <-  unlist(lapply(lapply(strsplit(gsub("\n", "", pkg.installed[which(row.names(pkg.installed) %in% names(installed[i])), "Suggests"]), ","),
                                                                   function(y) misty::chr.omit(misty::chr.omit(misty::chr.trim(ifelse(grepl("\\(", y), yes = substr(y, 1L, unlist(gregexpr('\\(', y)) - 1L), y), check = FALSE), "R", check = FALSE), na.omit = TRUE, check = FALSE)),
                                                            function(y) setNames(y %in% row.names(pkg.installed), y)))

        }

      }

      suggests.check <- unlist(sapply(suggests, function(y) if (isTRUE(is.null(y))) { NA } else { all(y) }))

      #...................
      ### Loaded ####

      loaded <- installed & misty::na.as(depends.check[match(pkg, names(depends.check))], na = TRUE, check = FALSE) & misty::na.as(imports.check[match(pkg, names(imports.check))], na = TRUE, check = FALSE)

  }

  #...................
  ### Output table ####

  restab <- data.frame(package = pkg,
                       installed = unname(installed),
                       version = unname(version[match(pkg, names(version))]),
                       depends = unname(depends.check[match(pkg, names(depends.check))]),
                       imports = unname(imports.check[match(pkg, names(imports.check))]),
                       suggests = unname(suggests.check[match(pkg, names(suggests.check))]),
                       loaded = unname(loaded))

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 pkg = pkg,
                 args = list(install = install, quiet = quiet, check = check, output = output),
                 result = list(installed = installed,
                               version = version,
                               depends = depends[sapply(depends, function(y) length(y) != 0L)],
                               imports = imports[sapply(imports, function(y) length(y) != 0L)],
                               suggests = suggests[sapply(suggests, function(y) length(y) != 0L)],
                               restab = restab))

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Load packages ####

  if (isTRUE(quiet)) {

    invisible(suppressPackageStartupMessages(lapply(names(which(loaded)), library, character.only = TRUE)))

    if (isTRUE(output)) {

      restab$loaded <- ifelse(restab$loaded, "loaded", ifelse(restab$install, "not loaded, dependencies not met", "not loaded, package not installed"))
      restab$package <- paste("", restab$package)
      restab$version <- ifelse(is.na(restab$version), "", restab$version)

      cat(" Load and Attach Packages\n")

      # Print package overview
      print(setNames(restab[, c("package", "version", "loaded")], c("", "", "")), right = FALSE, row.names = FALSE, col.names = TRUE)

    }

  } else {

    if (isTRUE(output)) {

      restab$loaded <- ifelse(restab$loaded, "loaded", ifelse(restab$install, "not loaded, dependencies not met", "not loaded, package not installed"))
      restab$package <- paste("", restab$package)
      restab$version <- ifelse(is.na(restab$version), "", restab$version)

      cat(" Load and Attach Packages\n")

      # Print package overview
      print(setNames(restab[, c("package", "version", "loaded")], c("", "", "")), right = FALSE, row.names = FALSE, col.names = TRUE)

    }

    invisible(lapply(names(which(loaded)), library, character.only = TRUE))

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Return object ####

  return(invisible(object))

}
