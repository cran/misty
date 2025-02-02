#' Create Lagged Variables
#'
#' This function computes lagged values of variables by a specified number of
#' observations. By default, the function returns lag-1 values of the vector,
#' matrix, or data frame specified in the first argument.
#'
#' @param ...     a vector for computing a lagged values for a variable, matrix
#'                or data frame for computing lagged values for more than one
#'                variable. Note that the subject ID variable (\code{id}),
#'                observation number variable (\code{obs}), day number variable
#'                (\code{day}), and the date and time variable (\code{time}) are
#'                excluded from \code{...} when specifying the argument the
#'                using the names of the variables. Alternatively, an expression
#'                indicating the variable names in \code{data}. Note that the
#'                operators \code{.}, \code{+}, \code{-}, \code{~}, \code{:},
#'                \code{::}, and \code{!} can also be used to select variables,
#'                see 'Details' in the \code{\link{df.subset}} function.
#' @param data    a data frame when specifying one or more variables in the
#'                argument \code{...}. Note that the argument is \code{NULL}
#'                when specifying a vector, matrix, or data frame for the argument
#'                \code{...}.
#' @param id      either a character string indicating the variable name of the
#'                subject ID variable in '...' or a vector representing the
#'                subject IDs, see 'Details'.
#' @param obs     either a character string indicating the variable name of the
#'                observation number variable in '...' or a vector representing
#'                the observations. Note that duplicated values within the same
#'                subject ID are not allowed, see 'Details'.
#' @param day     either a character string indicating the variable name of the
#'                day number variable in '...' or a vector representing the days,
#'                see 'Details'.
#' @param lag     a numeric value specifying the lag, e.g. \code{lag = 1} (default)
#'                returns lag-1 values.
#' @param time    a variable of class \code{POSIXct} or \code{POSIXlt} representing
#'                the date and time of the observation used to compute time
#'                differences between observations.
#' @param units   a character string indicating the units in which the time
#'                difference is represented, i.e., \code{"secs"} for seconds,
#'                \code{"mins"} (default) for minutes, \code{"hours"} for hours,
#'                \code{"days"} for days, and \code{"weeks"} for weeks.
#' @param append  logical: if \code{TRUE} (default), lagged variable(s) are
#'                appended to the data frame specified in the argument \code{data}.
#' @param name    a character string or character vector indicating the names of
#'                the lagged variables. By default, lagged variables are named
#'                with the ending \code{".lag"} resulting in e.g. \code{"x1.lag"}
#'                and \code{"x2.lag"} when specifying two variables. Variable
#'                names can also be specified using a character vector matching
#'                the number of variables specified in \code{...}, e.g.
#'                \code{name = c("lag.x1", "lag.x2")}).
#' @param name.td a character string or character vector indicating the names of
#'                the time difference variables when specifying a date and time
#'                variables for the argument \code{time}. By default, time
#'                difference variables are named with the ending \code{".td"}
#'                resulting in e.g. \code{"x1.td"} and \code{"x2.td"} when
#'                specifying two variables. Variable names can also be specified
#'                using a character vector matching the number of variables
#'                specified in \code{...}, e.g. \code{name = c("td.x1", "td.x2")}).
#' @param as.na   a numeric vector indicating user-defined missing values, i.e.
#'                these values are converted to \code{NA} before conducting the
#'                analysis. Note that \code{as.na()} function is only applied to
#'                the argument \code{x}, but not to \code{cluster}.
#' @param check   logical: if \code{TRUE} (default), argument specification is
#'                checked.
#'
#' @details
#' \describe{
#' The function is used to create lagged versions of the variable(s) specified via
#' the \code{...} argument:
#' \item{\strong{Optional argument \code{id}}}{If the \code{id} argument is not specified
#' \code{i.e., id = NULL}, all observations are assumed to come from the same
#' subject.  If the dataset includes multiple subjects, then this variable needs
#' to be specified so that observations are not lagged across subjects}
#' \item{\strong{Optional argument \code{obs}}}{If the \code{obs} argument is not specified
#' \code{i.e., obs = NULL}, consecutive observations from the same subjects are
#' assumed to be one lag apart.}
#' \item{\strong{Optional argument \code{day}}}{If the \code{day} argument is not specified
#' \code{i.e., day = NULL}, values of the variable to be lagged are allowed to be
#' lagged across days in case there are multiple observation days.}
#' }
#'
#' @author
#' Wolfgang Viechtbauer and Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{center}}, \code{\link{rec}}, \code{\link{coding}}, \code{\link{item.reverse}}.
#'
#' @references
#' Viechtbauer W, Constantin M (2023). \emph{esmpack: Functions that facilitate
#' preparation and management of ESM/EMA data}. R package version 0.1-20.
#'
#' @return
#' Returns a numeric vector or data frame with the same length or same number of
#' rows as \code{...} containing the lagged variable(s).
#'
#' @note
#' This function is a modified copy of the \code{lagvar()} function in the
#' \pkg{esmpack} package by Wolfgang Viechtbauer and Mihail Constantin (2023).
#'
#' @export
#'
#' @examples
#' dat <- data.frame(subject = rep(1:2, each = 6),
#'                    day = rep(1:2, each = 3),
#'                    obs = rep(1:6, times = 2),
#'                    time = as.POSIXct(c("2024-01-01 09:01:00", "2024-01-01 12:05:00",
#'                                        "2024-01-01 15:14:00", "2024-01-02 09:03:00",
#'                                        "2024-01-02 12:21:00", "2024-01-02 15:03:00",
#'                                        "2024-01-01 09:02:00", "2024-01-01 12:09:00",
#'                                        "2024-01-01 15:06:00", "2024-01-02 09:02:00",
#'                                        "2024-01-02 12:15:00", "2024-01-02 15:06:00")),
#'                     pos = c(6, 7, 5, 8, NA, 7, 4, NA, 5, 4, 5, 3),
#'                     neg = c(2, 3, 2, 5, 3, 4, 6, 4, 6, 4, NA, 8))
#'
#' # Example 1a: Lagged variable for 'pos'
#' lagged(dat$pos, id = dat$subject, day = dat$day)
#'
#' # Example 1b: Alternative specification
#' lagged(dat[, c("pos", "subject", "day")], id = "subject", day = "day")
#'
#' # Example 1c: Alternative specification using the 'data' argument
#' lagged(pos, data = dat, id = "subject", day = "day")
#'
#' # Example 2a: Lagged variable for 'pos' and 'neg'
#' lagged(dat[, c("pos", "neg")], id = dat$subject, day = dat$day)
#'
#' # Example 2b: Alternative specification using the 'data' argument
#' lagged(pos, neg, data = dat, id = "subject", day = "day")
#'
#' # Example 3: Lag-2 variables for 'pos' and 'neg'
#' lagged(pos, neg, data = dat, id = "subject", day = "day", lag = 2)
#'
#' # Example 4: Lagged variable and time difference variable
#' lagged(pos, neg, data = dat, id = "subject", day = "day", time = "time")
#'
#' # Example 5: Lagged variables and time difference variables,
#' # name variables
#' lagged(pos, neg, data = dat, id = "subject", day = "day", time = "time",
#'        name = c("p.lag1", "n.lag1"), name.td = c("p.diff", "n.diff"))
#'
#' # Example 6: NA observations excluded from the data frame
#' dat.excl <- dat[!is.na(dat$pos), ]
#'
#' # Number of observation not taken into account, i.e.,
#' # - observation 4 used as lagged value for observation 6 for subject 1
#' # - observation 1 used as lagged value for observation 3 for subject 2
#' lagged(pos, data = dat.excl, id = "subject", day = "day")
#'
#' # Number of observation taken into account by specifying the 'ob' argument
#' lagged(pos, data = dat.excl, id = "subject", day = "day", obs = "obs")
lagged <- function(..., data = NULL, id = NULL, obs = NULL, day = NULL, lag = 1, time = NULL,
                   units = c("secs", "mins", "hours", "days", "weeks"),
                   append = TRUE, name = ".lag", name.td = ".td", as.na = NULL, check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input '...' is missing
  if (isTRUE(missing(...))) { stop("Please specify the argument '...'.", call. = FALSE) }

  # Check if input '...' is NULL
  if (isTRUE(is.null(substitute(...)))) { stop("Input specified for the argument '...' is NULL.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data using the argument 'data' ####

  if (isTRUE(!is.null(data))) {

    # Convert tibble into data frame
    if (isTRUE("tbl" %in% substr(class(data), 1L, 3L))) { data <- as.data.frame(data) }

    # Variable names
    var.names <- .var.names(..., data = data, id = id, obs = obs, day = day, time = time, check.chr = "vector, matrix, or data frame")

    # Extract variables
    x <- data[, var.names]

    # Subject ID variable
    if (isTRUE(!is.null(id))) { id <- data[, id] }

    # Observation number variable
    if (isTRUE(!is.null(obs))) { obs <- data[, obs] }

    # Day number variable
    if (isTRUE(!is.null(day))) { day <- data[, day] }

    # Actual date and time variable
    if (isTRUE(!is.null(time))) { time <- data[, time] }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data without using the argument 'data' ####

  } else {

    # Extract data
    x <- eval(..., enclos = parent.frame())

    # Convert tibble into data frame
    if (isTRUE("tbl" %in% substr(class(x), 1L, 3L))) { if (isTRUE(ncol(as.data.frame(x)) == 1L)) { x <- unlist(x) } else { x <- as.data.frame(x) } }

    # Data, ID and time variables
    var.group <- .var.group(data = x, id = id, obs = obs, day = day, time = time)

    # Data
    if (isTRUE(!is.null(var.group$data)))  { x <- var.group$data }

    # Subject ID variable
    if (isTRUE(!is.null(var.group$id))) { id <- var.group$id }

    # Observation number variable
    if (isTRUE(!is.null(var.group$obs))) { obs <- var.group$obs }

    # Day number variable
    if (isTRUE(!is.null(var.group$day))) { day <- var.group$day }

    # Actual date and time variable
    if (isTRUE(!is.null(var.group$time))) { time <- var.group$time }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) { x <- .as.na(x, na = as.na) }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  # Check input 'type'
  if (isTRUE(all(!units %in% c("secs", "mins", "hours", "days", "weeks")))) { stop("Character string in the argument 'units' does not match with \"secs\", \"mins\", \"hours\", \"days\"  or \"weeks\".", call. = FALSE) }

  units <- ifelse(all(c("secs", "mins", "hours", "days", "weeks") %in% units), "mins", units)

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = "append",
               numeric = list(lag = 1L),
               s.character = list(units = c("secs", "mins", "hours", "days", "weeks")), envir = environment(), input.check = check)

  if (isTRUE(check)) {

    # Check input 'obs'
    if (isTRUE(!is.null(obs) && !is.numeric(obs))) { stop("Please specify a numeric vector for the argument 'obs'.", call. = FALSE) }

    # Check input 'obs' for repeated values
    if (isTRUE(!is.null(obs) && any(sapply(split(obs, id), function(x) length(x) != length(unique(x)))))) { stop("There are repeated values within subject IDs for the argument 'obs'.", call. = FALSE) }

    # Check input 'lag'
    if (isTRUE(!is.numeric(lag) || lag < 1L)) { stop("Please specify a numeric value >= 1 for the argument 'lag'.", call. = FALSE) }

    # Check input 'time'
    if (isTRUE(!is.null(time) && !inherits(time, "POSIXct") && !inherits(time, "POSIXlt"))) { stop("Please specify a POSIXct or POSIXlt class for the argument 'time'.", call. = FALSE) }

    # Check input 'name'
    if (isTRUE(all(name != ".lag"))) { if (isTRUE(length(name) != ncol(as.data.frame(x)))) { stop("Length of the vector specified in 'name' does not match with the number of variables.", call. = FALSE) } }

    # Check input 'name.td'
    if (isTRUE(all(name.td != ".td"))) { if (isTRUE(length(name.td) != ncol(as.data.frame(x)))) { stop("Length of the vector specified in 'name.td' does not match with the number of variables.", call. = FALSE) } }

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Single variable ####
  if (isTRUE(is.null(dim(x)))) {

    #...................
    ### Subject ID, observation number, day, and date variable ####

    # If 'id' is not specified, assume data are from a single subject
    if (isTRUE(is.null(id))) { id <- rep(1L, length(x)) }

    # If 'obs' is not specified, set it to consecutive integers within subjects
    if (isTRUE(is.null(obs))) { obs <- unsplit(lapply(split(id, id), seq_along), id) }

    # If 'day' is not specified, set it to 1 for all observations
    if (isTRUE(is.null(day))) { day <- rep(1L, times = length(x)) }

    # If 'time' is not specified, set it to 1
    # if (isTRUE(is.null(time))) { time <- rep(NA, times = length(x)) }

    # Data frame
    dat <- data.frame(x = x, id = id, obs = obs, day = day, time = if (isTRUE(is.null(time))) { NA } else { time })

    # Split data frame by subject ID
    res <- lapply(split(dat, f = dat$id), function(y) {

      n <- nrow(y)
      x.lag <- rep(NA, times = n)
      t.lag <- rep(NA, times = n)

      if (isTRUE(!is.null(time))) { class(t.lag) <- "difftime"; attr(t.lag, "units") <- units }

      # Loop over observations
      for (i in seq_len(n)) {

        if (isTRUE(y$obs[i] - lag >= 0L))  {

          pos <- which(y$obs == y$obs[i] - lag)

          if (isTRUE(length(pos) > 0L && !is.na(y$x[pos]))) {

            if (isTRUE(y$day[i] == y$day[pos])) {

              x.lag[i] <- y$x[pos]

              if (isTRUE(!is.null(time))) { t.lag[i] <- difftime(y$time[i], y$time[pos], units = units) }

            }

          }

        }

      }

      return(data.frame(x.lag = x.lag, t.lag = t.lag))

    })

    #...................
    ### Reassemble data frame ####
    object <- data.frame(lagged = unsplit(lapply(res, function(x) x$x.lag), f = dat$id), timediff = unsplit(lapply(res, function(x) x$t.lag), f = dat$id))

    if (all(is.na(object$timediff))) { object <- object$lagged }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Multiple variables ####
  } else {

    object <- apply(x, 2L, misty::lagged, id = id, obs = obs, day = day, lag = lag, time = time, units = units, check = FALSE)

    if (isTRUE(is.list(object))) {

      object <- do.call("cbind", object)

      # Order variables
      object <- which(sapply(object, class) == "difftime") |> (\(y) data.frame(object[, -y], object[, y]) )()

    } else {

      object <- as.data.frame(object)

    }

    #...................
    ### Variable names ####

    ##### With timediff variables
    if (isTRUE(any(sapply(object, class) == "difftime"))) {

      if (isTRUE(all(name == ".lag") && all(name.td == ".td"))) {

        object <- setNames(as.data.frame(object), nm = c(paste0(colnames(x), name), paste0(colnames(x), name.td)))

      } else if (isTRUE(all(name != ".lag") && all(name.td == ".td"))) {

        object <- setNames(as.data.frame(object), nm = c(name, paste0(colnames(x), name.td)))

      } else if (isTRUE(all(name == ".lag") && all(name.td != ".td"))) {

        object <- setNames(as.data.frame(object), nm = c(paste0(colnames(x), name), name.td))

      } else if (isTRUE(all(name != ".lag") && all(name.td != ".td"))) {

        object <- setNames(as.data.frame(object), nm = c(name, name.td))

      }

    ##### Without timediff variable
    } else {

      if (isTRUE(name == ".lag")) {

        object <- setNames(as.data.frame(object), nm = paste0(colnames(x), name))

      } else {

        object <- setNames(as.data.frame(object), nm = name)

      }

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Append ####

  if (isTRUE(!is.null(data) && append)) {

    if (isTRUE(is.null(dim(x)))) {

      #...................
      ### Variable names ####

      ##### With timediff variable
      if (isTRUE(any(sapply(object, class) == "difftime"))) {

        if (isTRUE(name == ".lag" && name.td == ".td")) {

          object <- setNames(as.data.frame(object), nm = c(paste0(var.names, name), paste0(var.names, name.td)))

        } else if (isTRUE(name != ".lag" && name.td == ".td")) {

          object <- setNames(as.data.frame(object), nm = c(name, paste0(var.names, name.td)))

        } else if (isTRUE(name == ".lag" && name.td != ".td")) {

          object <- setNames(as.data.frame(object), nm = c(paste0(var.names, name), name.td))

        } else if (isTRUE(name != ".lag" && name.td != ".td")) {

          object <- setNames(as.data.frame(object), nm = c(name, name.td))

        }

      ##### Without timediff variable
      } else {

        if (isTRUE(name == ".lag")) {

          object <- setNames(as.data.frame(object), nm = paste0(var.names, name))

        } else {

          object <- setNames(as.data.frame(object), nm = name)

        }

      }

    }

    object <- data.frame(data, object)

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  return(object)

}

#_______________________________________________________________________________
