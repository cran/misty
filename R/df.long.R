#' Converting Data Frames Between 'Wide' and 'Long' Format
#'
#' The function \code{df.long} converts a data frame from the 'wide' data format
#' (with repeated measurements in separate columns of the same row) to the 'long'
#' data format (with repeated measurements in separate rows), while the function
#' \code{df.wide} converts from the 'long' data format to the 'wide' data format
#' .
#'
#' @param data       a data frame in 'wide' or 'long' format.
#' @param ...        an expression indicating the time-invariant variable names
#'                   in \code{data} that should be kept after converting data to
#'                   the 'long' or 'wide' format. Note that the operators \code{+},
#'                   \code{-}, \code{~}, \code{:}, \code{::}, and \code{!}
#'                   can also be used to select variables, see 'Details' in the
#'                   \code{\link{df.subset}} function. Note that the \code{...}
#'                   is not specified when all variables should be kept in the
#'                   converted data frame.
#' @param var        a character vector (one set of variable names) or a list of
#'                   character vectors (multiple sets of variables names) in
#'                   the wide data format indicating the sets of time-varying
#'                   variables in the wide format that correspond to single
#'                   variables in the long format when using the \code{df.long}
#'                   function. Note that all variables excluded those specified
#'                   in the argument \code{...} are used when \code{var = NULL}
#'                   (default), see Example 7. A character vector indicating the
#'                   variable name(s) in the long format that are being split into
#'                   separate variables when using the \code{df.wide} function.
#' @param var.name   a character vector specifying the variable names in the long
#'                   format that correspond to the sets of time-varying variables
#'                   in the wide data format when using the \code{df.long} function
#'                   or a character vector specifying the prefix of the variable
#'                   names in the wide format that correspond to the time-varying
#'                   variables in the long format.
#' @param time       a character string indicating the data type of the newly
#'                   created variable in the long format when using the \code{df.long}
#'                   function, i.e., \code{"num"} for numeric consecutive integers
#'                   starting from \code{0} (e.g., \code{0, 1, 2, 3} for a set of
#'                   four variables in the wide data format), \code{"chr"} for a
#'                   character vector, \code{"fac"} for a factor, and \code{"ord"}
#'                   for a ordered factor. Note that the variable names of the set
#'                   of variables in the wide data format is used when specifying
#'                   \code{"chr"}, \code{"fac"}, or \code{"ord"} if only one set
#'                   of variables is specified in the \code{"var"} argument.
#'                   Otherwise numeric consecutive integers starting from \code{1}
#'                   as character, factor or ordered factor are used. Or a
#'                   character string indicating the variable name in the long
#'                   data format that differentiates multiple records from the
#'                   same group or individual when using the \code{df.wide} function.
#' @param time.name  a character string indicating the name of the newly created
#'                   variable in the long format when using the \code{df.long}
#'                   function. By default, the variable is named \code{"time"}.
#'                   Note that variable names can also be specified using the
#'                   \code{var} when multiple sets of time-varying variables are
#'                   specified in a list, e.g.,
#'                   \code{var = list(dep = c("ad", "bd"), anx = c("aa", "ba"))}
#'                   (see alternative specification in Example 5).
#' @param idvar      a character string indicating the name of the identification
#'                   variable in the wide data format that is used to sort the
#'                   data after converting a data frame from wide to long format
#'                   when using the \code{df.long} function and specifying
#'                   \code{sort = TRUE}. Note that the function will create an
#'                   identification variable with consecutive integer starting
#'                   from \code{1} if the variable specified in \code{idvar} is
#'                   not found in \code{data}. Or a character string indicating
#'                   the name of the identification variable in the long data
#'                   format when using the \code{df.wide} function.
#' @param sort       logical: if \code{TRUE} (default), data frame in the long
#'                   format is sorted according to the identification variable
#'                   specified in \code{idvar} when using the \code{df.long}
#'                   function.
#' @param decreasing logical: if TRUE, the sort is decreasing when specifying
#'                   \code{sort = TRUE}.
#' @param sep        a character string indicating a separating character in the
#'                   variable names after converting data from the long format
#'                   to the wide format when using the \code{df.wide} function.
#'                   For example, the variable \code{value} in the long format
#'                   will be split into the variables \code{value0}, \code{value1},
#'                   and \code{value2} when specifying \code{sep = ""} (default),
#'                   but will be split into the variables \code{value_0},
#'                   \code{value_1}, and \code{value_2} when specifying \code{sep = "_"}.
#' @param na.rm      logical: if TRUE, rows with \code{NA} values for all variables
#'                   in the long format that correspond to the sets of time-varying
#'                   variables in the wide data format will be removed from the
#'                   data when using the \code{df.long} function.
#' @param check      logical: if TRUE (default), argument specification is checked.
#'
#' @author
#' Takuya Yanagida
#'
#' @name df.long
#'
#' @seealso
#' \code{\link{df.check}}, \code{\link{df.duplicated}}, \code{\link{df.unique}},
#' \code{\link{df.head}}, \code{\link{df.tail}}, \code{\link{df.merge}},
#' \code{\link{df.move}}, \code{\link{df.rbind}}, \code{\link{df.rename}},
#' \code{\link{df.sort}}, \code{\link{df.subset}}
#'
#' @references
#' Barrett, T., Dowle, M., Srinivasan, A., Gorecki, J., Chirico, M., Hocking, T.,
#' & Schwendinger, B. (2025). data.table: Extension of 'data.frame'. R package
#' version 1.17.8. \url{https://CRAN.R-project.org/package=data.table}
#'
#' @return
#' Data frame that is converted to the 'long' or 'wide' format.
#'
#' @note
#' The function \code{df.long} uses the function \code{melt} and the function
#' \code{df.long} uses the function \code{dcast} provided in the R package
#' \pkg{data.table} by Tyson Barrett et al., (2025).
#'
#' @export
#'
#' @examples
#' dat.w <- data.frame(id = c(23, 55, 71),
#'                     gend = c("male", "female", "male"), age = c(22, 19, 26),
#'                     adep = c(3, 6, NA), bdep = c(5, 5, 6), cdep = c(4, NA, 5),
#'                     aanx = c(5, 3, 6), banx = c(NA, 7, 2), canx = c(6, NA, 8))
#'
#' #----------------------------------------------------------------------------
#' # Convert from 'wide' data format to the 'long' data format
#'
#' # Example 1: One set of time-varying variables combined into "dep"
#' df.long(dat.w, var = c("adep", "bdep", "cdep"), var.name = "dep", idvar = "id")
#'
#' # Example 2: Select time-invariant variables 'gend' and 'age'
#' df.long(dat.w, gend, age, var = c("adep", "bdep", "cdep"), var.name = "dep",
#'         idvar = "id")
#'
#' # Example 3: Newly created variable "type" as character vector
#' df.long(dat.w, age, var = c("adep", "bdep", "cdep"), var.name = "dep",
#'         idvar = "id", time = "chr", time.name = "type")
#'
#' # Example 4: User-defined variable "type"
#' df.long(dat.w, age, var = c("adep", "bdep", "cdep"), var.name = "dep",
#'         idvar = "id", time = c("pre", "post", "follow-up"), time.name = "type")
#'
#' # Example 5: Two sets of time-varying variables combined into "dep" and "anx"
#' df.long(dat.w, age,
#'         var = list(c("adep", "bdep", "cdep"), c("aanx", "banx", "canx")),
#'         var.name = c("dep", "anx"), idvar = "id")
#'
#' # Alternative specification using named lists for the argument 'var'
#' df.long(dat.w, age,
#'         var = list(dep = c("adep", "bdep", "cdep"), anx = c("aanx", "banx", "canx")),
#'         idvar = "id")
#'
#' # Example 6: Remove rows with only NA values
#' df.long(dat.w, age, var = list(c("adep", "bdep", "cdep"), c("aanx", "banx", "canx")),
#'         idvar = "id", sort = FALSE, na.rm = TRUE)
#'
#' # Example 7: Convert all variables except "age" and "gend"
#' df.long(dat.w, age, gend, idvar = "id")
#'
#' #----------------------------------------------------------------------------
#' # Convert from 'long' data format to the 'wide' data format
#'
#' dat.l <- df.long(dat.w,
#'                  var = list(c("adep", "bdep", "cdep"), c("aanx", "banx", "canx")),
#'                  var.name = c("dep", "anx"), idvar = "id")
#'
#' # Example 8: Time-varying variables "dep" and "anx" expanded into multiple variables
#' df.wide(dat.l, var = c("dep", "anx"), idvar = "id", time = "time")
#'
#' # Example 9: Select time-invariant variables 'age'
#' df.wide(dat.l, age, var = c("dep", "anx"), idvar = "id", time = "time")
#'
#' # Example 10: Variable name prefix of the  expanded variables "depre" and "anxie"
#' #             with separating character "."
#' df.wide(dat.l, var = c("dep", "anx"), var.name = c("depre", "anxie"),
#'         idvar = "id", time = "time", sep = ".")
df.long <- function(data, ..., var = NULL, var.name = "value",
                    time = c("num", "chr", "fac", "ord"), time.name = "time",
                    idvar = "idvar", sort = TRUE, decreasing = FALSE, na.rm = FALSE, check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'data' is missing
  if (isTRUE(missing(data))) { stop("Please specify a data frame for the argument 'data'", call. = FALSE) }

  # Check if input 'data' is NULL
  if (isTRUE(is.null(data))) { stop("Input specified for the argument 'data' is NULL.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Variables ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Set of Varying Variables ####

  if (isTRUE(is.null(var))) { measure.vars <- setdiff(colnames(data), idvar) } else { measure.vars <- var }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Variables using the argument '...' ####

  if (isTRUE(!missing(...))) {

    id.vars <- c(idvar, .var.names(..., data = data))

    if (isTRUE(is.null(var))) { measure.vars <- setdiff(measure.vars, id.vars) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Variables without using the argument '...' ####

  } else {

    id.vars <- c(idvar, colnames(data))

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Remove Varying Set of Variables ####

  id.vars <- setdiff(id.vars, unlist(measure.vars))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## ID Variable not in Dataframe ####

  if (isTRUE(!idvar %in% colnames(data))) { data <- setNames(data.frame(seq_len(nrow(data)), data), nm = c(idvar, colnames(data))) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("decreasing", "na.rm"), character = list(time.name = 1L), envir = environment(), input.check = check)

  # Additional checks
  if (isTRUE(isTRUE(check))) {

    # Check input 'var'
    if (isTRUE(is.list(var) && misty::uniq.n(unlist(lapply(var, length))) != 1L)) { stop("Character vectors specified in the argument 'var' do not all have the same length.", call. = FALSE) }

    (!unlist(var) %in% colnames(data)) |> (\(y) if (isTRUE(any(y))) { stop(paste0("Variables specified in the argument 'var' were not all found in 'data': ", paste(unique(unlist(var)[y]), collapse = ", ")), call. = FALSE) })()

    # Check input 'var.name'
    if (isTRUE(is.list(var) && length(var.name) != 1L && length(var.name) != length(var))) { stop("Length of the argument 'var.name' does not match the length of the list specified in 'var'.", call. = FALSE) }

    # Check input 'time'
    if (isTRUE(!all(time %in% c("num", "chr", "fac", "ord")) && length(time) != if (isTRUE(!is.list(var))) { length(var) } else { length(var[[1L]]) })) { stop("Length of the vector specified in 'time' does not match the number of variables specified in 'var'", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Argument 'time' ####

  # Default, "num" when argument 'var' is specified, otherwise "chr"
  time <- if (isTRUE(all(c("num", "chr", "fac", "ord") %in% time))) { if (isTRUE(!is.null(var))) { "num" } else { "chr" } } else { time }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Argument 'var' ####

  # One set of variables specified in a list
  if (isTRUE(is.list(var) && length(var) == 1L)) { measure.vars <- unlist(measure.vars) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Argument 'var.name' ####

  if (isTRUE(is.list(measure.vars) && length(var.name) == 1L)) { var.name <- paste0(var.name, seq_len(length(var))) }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  object <- suppressMessages(suppressWarnings(data.table::setDF(data.table::melt(data.table::setDT(data), measure.vars = measure.vars, value.name = var.name,
                                                                                 id.vars = id.vars, variable.name = time.name, variable.factor = FALSE, value.factor = FALSE,
                                                                                 na.rm = FALSE, verbose = FALSE))))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Time Variable ####

  #...................
  ### Numeric, Character, Factor, or Ordered Factor ####

  if (isTRUE(all(time %in% c("num", "chr", "fac", "ord")))) {

    # Numeric
    if (time == "num") {

      object[, time.name] <- rep(seq_len(misty::uniq.n(object[, time.name])) - 1L, each = nrow(data))

    # Factor, or ordered factor (default: character)
    } else {

      switch(time, "fac" = {

        object[, time.name] <- factor(object[, time.name])

      }, "ord" = {

        object[, time.name] <- factor(object[, time.name], ordered = TRUE)

      })

    }

  #...................
  ### User-Defined Values ####

  } else {

    object[, time.name] <- rep(time, each = nrow(data))

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Sort Data Frame According to 'idvar' ####

  if (isTRUE(sort)) { object <- data.frame(object[order(object[, idvar], decreasing = decreasing), , drop = FALSE], row.names = NULL) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Remove NA values ####

  if (isTRUE(na.rm)) { object <- object[which(misty::na.prop(object[, var.name, drop = FALSE]) != 1L), ] }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  return(object)

}

#_______________________________________________________________________________
#_______________________________________________________________________________

#' @rdname df.wide
df.wide <- function(data, ..., var, var.name = var, time = "time", idvar = "idvar", sep = "", check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'data' is missing
  if (isTRUE(missing(data))) { stop("Please specify a data frame for the argument 'data'", call. = FALSE) }

  # Check if input 'data' is NULL
  if (isTRUE(is.null(data))) { stop("Input specified for the argument 'data' is NULL.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Variables ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Variables using the argument 'data' ####

  if (isTRUE(!missing(...))) {

    plus.var <- c(idvar, .var.names(..., data = data))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Variables without using the argument 'data' ####

  } else {

    plus.var <- c(idvar, colnames(data))

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Remove Variables for Casting and Time Variable ####

  plus.var <- setdiff(plus.var, c(var, time))

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(character = list(idvar = 1L, time = 1L, sep = 1L), envir = environment(), input.check = check)

  # Additional checks
  if (isTRUE(isTRUE(check))) {

    # Check input 'var'
    (!var %in% colnames(data)) |> (\(y) if (isTRUE(any(y))) { stop(paste0("Variables specified in the argument 'var' were not all found in 'data': ", paste(var[y], collapse = ", ")), call. = FALSE) })()

    # Check input 'var.name'
    if (isTRUE(length(var.name) != length(var))) { stop("Length of the argument 'var.name' does not match the length of vector specified in 'var'.", call. = FALSE) }

    # Check input 'idvar'
    if (isTRUE(!idvar %in% colnames(data))) { stop(paste0("Variable specified in the argument 'idvar' was not found in 'data': ", idvar), call. = FALSE) }

    # Check input 'time'
    if (isTRUE(!time %in% colnames(data))) { stop(paste0("Variable specified in the argument 'time' was not found in 'data': ", time), call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Argument 'formula' ####

  formula <- paste(paste(plus.var, collapse = " + "), time, sep = " ~ ")

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  object <- suppressMessages(suppressWarnings(data.table::setDF(data.table::dcast(data.table::setDT(data), formula = formula, value.var = var, sep = sep))))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Variable Names ####

  if (isTRUE(!identical(var.name, var))) { for (i in seq_along(var)) { object <- setNames(object, nm = c(plus.var, sub(var[i], var.name[i], setdiff(colnames(object), plus.var)))) } }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  return(object)

}

#_______________________________________________________________________________
