#' Descriptive Statistics for Missing Data in Single-Level, Two-Level and Three-Level Data
#'
#' This function computes descriptive statistics for missing data in single-level,
#' two-level, and three-level data, e.g. number (%) of incomplete cases, number
#' (%) of missing values, and summary statistics for the number (%) of missing
#' values across all variables.
#'
#' @param data    a data frame with incomplete data, where missing values are
#'                coded as \code{NA}.
#' @param ...     an expression indicating the variable names in \code{data},
#'                e.g., \code{na.descript(dat, x1, x2, x3)}. Note that the operators
#'                \code{.}, \code{+}, \code{-}, \code{~}, \code{:}, \code{::},
#'                and \code{!} can also be used to select variables, see 'Details'
#'                in the \code{\link{df.subset}} function.
#' @param cluster a character string indicating the name of the cluster
#'                variable in \code{data} for two-level data, a character vector
#'                indicating the names of the cluster variables in \code{data}
#'                for three-level data, or a vector or data frame representing
#'                the nested grouping structure (i.e., group or cluster variables).
#'                Alternatively, a character string or character vector indicating
#'                the variable name(s) of the cluster variable(s) in \code{data}.
#'                Note that the cluster variable at Level 3 come first in a
#'                three-level model, i.e., \code{cluster = c("level3", "level2")}.
#' @param table   logical: if \code{TRUE}, a frequency table with number of
#'                observed values (\code{"nObs"}), percent of observed values
#'                (\code{"pObs"}), number of missing values (\code{"nNA"}),
#'                and percent of missing values (\code{"pNA"}) is printed for
#'                each variable on the console.
#' @param digits  an integer value indicating the number of decimal places to
#'                be used for displaying percentages.
#' @param as.na   a numeric vector indicating user-defined missing values,
#'                i.e. these values are converted to \code{NA} before conducting
#'                the analysis.
#' @param write   a character string naming a file for writing the output into
#'                either a text file with file extension \code{".txt"} (e.g.,
#'                \code{"Output.txt"}) or Excel file with file extension
#'                \code{".xlsx"}  (e.g., \code{"Output.xlsx"}). If the file
#'                name does not contain any file extension, an Excel file will
#'                be written.
#' @param append  logical: if \code{TRUE} (default), output will be appended
#'                to an existing text file with extension \code{.txt} specified
#'                in \code{write}, if \code{FALSE} existing text file will be
#'                overwritten.
#' @param check  logical: if \code{TRUE} (default), argument specification is checked.
#' @param output logical: if \code{TRUE} (default), output is shown on the console.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{as.na}}, \code{\link{na.as}},\code{\link{na.auxiliary}},
#' \code{\link{na.coverage}}, \code{\link{na.indicator}}, \code{\link{na.pattern}},
#' \code{\link{na.prop}}, \code{\link{na.test}}, \code{\link{write.result}}
#'
#' @references
#' Enders, C. K. (2010). \emph{Applied missing data analysis}. Guilford Press.
#'
#' Graham, J. W. (2009). Missing data analysis: Making it work in the real world.
#' \emph{Annual Review of Psychology, 60}, 549-576.
#' https://doi.org/10.1146/annurev.psych.58.110405.085530
#'
#' van Buuren, S. (2018). \emph{Flexible imputation of missing data} (2nd ed.).
#' Chapman & Hall.
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{data}}{data frame used for the current analysis}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{result}}{list with results}
#'
#' @export
#'
#' @examples
#' #----------------------------------------------------------------------------
#' # Single-Level Data
#'
#' # Example 1: Descriptive statistics for missing data
#' na.descript(airquality)
#'
#' # Example 2: Descriptive statistics for missing data, print results with 3 digits
#' na.descript(airquality, digits = 3)
#'
#' # Example 3: Descriptive statistics for missing data with frequency table
#' na.descript(airquality, table = TRUE)
#'
#' #----------------------------------------------------------------------------
#' # Two-Level Data
#'
#' # Load data set "Demo.twolevel" in the lavaan package
#' data("Demo.twolevel", package = "lavaan")
#'
#' # Example 4: Descriptive statistics for missing data
#' na.descript(Demo.twolevel, cluster = "cluster")
#'
#' #----------------------------------------------------------------------------
#' # Three-Level Data
#'
#' # Create arbitrary three-level data
#' Demo.threelevel <- data.frame(Demo.twolevel, cluster2 = Demo.twolevel$cluster,
#'                                              cluster3 = rep(1:10, each = 250))
#'
#' # Example 5: Descriptive statistics for missing data
#' na.descript(Demo.threelevel, cluster = c("cluster3", "cluster2"))
#'
#' \dontrun{
#' #----------------------------------------------------------------------------
#' # Write Results
#'
#' # Example 6a: Write Results into a text file
#' na.descript(airquality, table = TRUE, write = "NA_Descriptives.txt")
#'
#' # Example 6b: Write Results into a Excel file
#' na.descript(airquality, table = TRUE, write = "NA_Descriptives.xlsx")
#' }
na.descript <- function(data, ..., cluster = NULL, table = FALSE, digits = 2,
                        as.na = NULL, write = NULL, append = TRUE, check = TRUE,
                        output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'data' is missing
  if (isTRUE(missing(data))) { stop("Please specify a data frame for the argument 'data'", call. = FALSE) }

  # Check if input 'data' is NULL
  if (isTRUE(is.null(data))) { stop("Input specified for the argument 'data' is NULL.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data using the argument 'data' ####

  if (isTRUE(!missing(...))) {

    # Extract data
    x <- as.data.frame(data[, .var.names(..., data = data, cluster = cluster), drop = FALSE])

    # Cluster variable
    if (isTRUE(!is.null(cluster))) { cluster <- data[, cluster] }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data without using the argument 'data' ####

  } else {

    # Data frame
    x <- as.data.frame(data) |> (\(y) if (isTRUE(ncol(y) == 1L)) { unname(y) } else { y })()

    # Data and cluster
    var.group <- .var.group(data = x, cluster = cluster, drop = FALSE)

    # Data
    if (isTRUE(!is.null(var.group$data)))  { x <- var.group$data }

    # Cluster variable
    if (isTRUE(!is.null(var.group$cluster))) { cluster <- var.group$cluster }


  }

  # Convert 'cluster' as tibble into a vector
  if (!is.null(cluster) && isTRUE("tbl" %in% substr(class(cluster), 1L, 3L))) { cluster <- unname(unlist(cluster)) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) { x <- .as.na(x, na = as.na) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Cluster variables ####

  no.clust <- "none"
  if (isTRUE(!is.null(cluster))) {

    # Two cluster variables
    if (isTRUE(ncol(as.data.frame(cluster)) == 2L)) {

      l3.cluster <- cluster[, 1L]
      l2.cluster <- cluster[, 2L]

      no.clust <- "two"

    # One cluster variables
    } else {

      no.clust <- "one"

    }

  }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("table", "append", "output"), args = c("digits", "write2"), envir = environment(), input.check = check)

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Variable ####

  if (isTRUE(!is.null(cluster))) {

    #...................
    ### One cluster variable ####

    if (isTRUE(no.clust == "one")) {

      # No. of clusters
      no.cluster.l2 <- length(na.omit(unique(cluster)))

      # Level-1 Variable
      L1.var <- names(which(apply(x, 2L, function(y) any(na.omit(as.vector(tapply(y, cluster, var, na.rm = TRUE) != 0L))))))

      # Level-2 Variable
      L2.var <- setdiff(colnames(x), L1.var)

    #...................
    ### Two cluster variables ####

    } else if (isTRUE(no.clust == "two")) {

      # No. of Level-2 clusters
      no.cluster.l2 <- length(na.omit(unique(cluster[, 2L])))

      # No. of Level-3 clusters
      no.cluster.l3 <- length(na.omit(unique(cluster[, 1L])))

      # Level-2 Variable
      L1.var <- names(which(apply(x, 2L, function(y) any(na.omit(as.vector(tapply(y, apply(cluster, 1L, paste, collapse = ""), var, na.rm = TRUE))) != 0L))))

      # Level-2 Variable
      L2.var <- names(which(apply(x, 2L, function(y) all(na.omit(as.vector(tapply(y, apply(cluster, 1L, paste, collapse = ""), var, na.rm = TRUE))) == 0L)) & apply(x, 2L, function(y) any(as.vector(tapply(y, cluster[, 1L], var, na.rm = TRUE)) != 0L))))

      # Level-3 Variable
      L3.var <- setdiff(colnames(x), c(L1.var, L2.var))

    }

  #...................
  ### No cluster variable ####

  } else {

    L1.var <- colnames(x)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Descriptive Statistics for Missing Data ####

  #...................
  ### Level-1 Variables ####

  ##### Level-1 Variables
  if (isTRUE(length(L1.var) != 0L)) {

    ##### No cluster variable
    if (isTRUE(is.null(cluster))) {

      x.L1 <- x

    ##### Cluster variable(s)
    } else {

      x.L1 <- x[, L1.var, drop = FALSE]

    }

    # Number of cases
    no.cases.l1 <- nrow(x.L1)

    # Number of complete cases
    no.complete.l1 <- sum(apply(x.L1, 1L, function(y) all(!is.na(y))))
    perc.complete.l1 <- no.complete.l1 / no.cases.l1 * 100L

    # Number and percentage of imcomplete cases
    no.incomplete.l1 <- sum(apply(x.L1, 1L, function(y) any(is.na(y))))
    perc.incomplete.l1 <- no.incomplete.l1 / no.cases.l1 * 100L

    # Number of values
    no.values.l1 <- length(unlist(x.L1))

    # Number of observed values
    no.observed.values.l1 <- sum(!is.na(unlist(x.L1)))
    perc.observed.values.l1 <- no.observed.values.l1 / no.values.l1 *100L

    # Number and percentage of missing values
    no.missing.values.l1 <- sum(is.na(unlist(x.L1)))
    perc.missing.values.l1 <- no.missing.values.l1 / no.values.l1 * 100L

    # Number of variables
    no.var.l1 <- ncol(x.L1)

    # Number and percentage of observed values for each variable
    no.observed.var.l1 <- vapply(x.L1, function(y) sum(!is.na(y)), FUN.VALUE = 1L)
    perc.observed.var.l1 <- no.observed.var.l1 / no.cases.l1 * 100L

    # Number and percentage of missing values for each variable
    no.missing.var.l1 <- vapply(x.L1, function(y) sum(is.na(y)), FUN.VALUE = 1L)
    perc.missing.var.l1 <- no.missing.var.l1 / no.cases.l1 * 100L

    no.missing.mean.l1 <- mean(no.missing.var.l1)
    perc.missing.mean.l1 <- no.missing.mean.l1 / no.cases.l1 * 100L

    no.missing.sd.l1 <- misty::na.as(sd(no.missing.var.l1), na = 0L, check = FALSE)
    perc.missing.sd.l1 <- no.missing.sd.l1 / no.cases.l1 * 100L

    no.missing.min.l1 <- min(no.missing.var.l1)
    perc.missing.min.l1 <- no.missing.min.l1 / no.cases.l1 * 100L

    no.missing.p25.l1 <- quantile(no.missing.var.l1, probs = 0.25)
    perc.missing.p25.l1 <- no.missing.p25.l1 / no.cases.l1 * 100L

    no.missing.p75.l1 <- quantile(no.missing.var.l1, probs = 0.75)
    perc.missing.p75.l1 <- no.missing.p75.l1 / no.cases.l1 * 100L

    no.missing.max.l1 <- max(no.missing.var.l1)
    perc.missing.max.l1 <- no.missing.max.l1 / no.cases.l1 * 100L

    # Frequency table
    table.missing.l1 <- data.frame(Var = colnames(x.L1),
                                   matrix(c(no.observed.var.l1, perc.observed.var.l1, no.missing.var.l1, perc.missing.var.l1), ncol = 4L,
                                          dimnames = list(NULL, c("nObs", "pObs", "nNA", "pNA"))))

  # No Level-1 Variables
  } else {

    no.complete.l1 <- perc.complete.l1 <- no.incomplete.l1 <- perc.incomplete.l1 <- no.values.l1 <- no.observed.values.l1 <- perc.observed.values.l1 <-
    no.missing.values.l1 <- perc.missing.values.l1 <- no.var.l1 <- no.observed.var.l1 <- perc.observed.var.l1 <- no.missing.var.l1 <- perc.missing.var.l1 <-
    no.missing.mean.l1 <- perc.missing.mean.l1 <- no.missing.sd.l1 <- perc.missing.sd.l1 <- no.missing.min.l1 <- perc.missing.min.l1 <-
    no.missing.p25.l1 <- perc.missing.p25.l1 <- no.missing.p75.l1 <- perc.missing.p75.l1 <- no.missing.max.l1 <- perc.missing.max.l1 <- table.missing.l1 <- NA

  }

  #...................
  ### Level-2 Variables ####

  if (isTRUE(!is.null(cluster))) {

    ##### Level-2 Variables
    if (isTRUE(length(L2.var) != 0L)) {

      # One cluster variable
      if (no.clust == "one") {

        x.L2 <- x[!duplicated(cluster), L2.var, drop = FALSE]

      # Two cluster variables
      } else {

        x.L2 <- x[!duplicated(apply(cluster, 1L, paste, collapse = "")), L2.var, drop = FALSE]

      }

      # Number of complete cases
      no.complete.l2 <- sum(apply(x.L2, 1L, function(y) all(!is.na(y))))
      perc.complete.l2 <- no.complete.l2 / no.cluster.l2 * 100L

      # Number and percentage of imcomplete cases
      no.incomplete.l2 <- sum(apply(x.L2, 1L, function(y) any(is.na(y))))
      perc.incomplete.l2 <- no.incomplete.l2 / no.cluster.l2 * 100L

      # Number of values
      no.values.l2 <- length(unlist(x.L2))

      # Number of observed values
      no.observed.values.l2 <- sum(!is.na(unlist(x.L2)))
      perc.observed.values.l2 <- no.observed.values.l2 / no.values.l2 *100L

      # Number and percentage of missing values
      no.missing.values.l2 <- sum(is.na(unlist(x.L2)))
      perc.missing.values.l2 <- no.missing.values.l2 / no.values.l2 * 100L

      # Number of variables
      no.var.l2 <- ncol(x.L2)

      # Number and percentage of observed values for each variable
      no.observed.var.l2 <- vapply(x.L2, function(y) sum(!is.na(y)), FUN.VALUE = 1L)
      perc.observed.var.l2 <- no.observed.var.l2 / no.cluster.l2 * 100L

      # Number and percentage of missing values for each variable
      no.missing.var.l2 <- vapply(x.L2, function(y) sum(is.na(y)), FUN.VALUE = 1L)
      perc.missing.var.l2 <- no.missing.var.l2 / no.cluster.l2 * 100L

      no.missing.mean.l2 <- mean(no.missing.var.l2)
      perc.missing.mean.l2 <- no.missing.mean.l2 / no.cluster.l2 * 100L

      no.missing.sd.l2 <- misty::na.as(sd(no.missing.var.l2), na = 0L, check = FALSE)
      perc.missing.sd.l2 <- no.missing.sd.l2 / no.cluster.l2 * 100L

      no.missing.min.l2 <- min(no.missing.var.l2)
      perc.missing.min.l2 <- no.missing.min.l2 / no.cluster.l2 * 100L

      no.missing.p25.l2 <- quantile(no.missing.var.l2, probs = 0.25)
      perc.missing.p25.l2 <- no.missing.p25.l2 / no.cluster.l2 * 100L

      no.missing.p75.l2 <- quantile(no.missing.var.l2, probs = 0.75)
      perc.missing.p75.l2 <- no.missing.p75.l2 / no.cluster.l2 * 100L

      no.missing.max.l2 <- max(no.missing.var.l2)
      perc.missing.max.l2 <- no.missing.max.l2 / no.cluster.l2 * 100L

      # Frequency table
      table.missing.l2 <- data.frame(Var = colnames(x.L2),
                                     matrix(c(no.observed.var.l2, perc.observed.var.l2, no.missing.var.l2, perc.missing.var.l2), ncol = 4L,
                                            dimnames = list(NULL, c("nObs", "pObs", "nNA", "pNA"))))

    # No Level-2 Variables
    } else {

      no.complete.l2 <- perc.complete.l2 <- no.incomplete.l2 <- perc.incomplete.l2 <- no.values.l2 <- no.observed.values.l2 <- perc.observed.values.l2 <-
      no.missing.values.l2 <- perc.missing.values.l2 <- no.var.l2 <- no.observed.var.l2 <- perc.observed.var.l2 <- no.missing.var.l2 <- perc.missing.var.l2 <-
      no.missing.mean.l2 <- perc.missing.mean.l2 <- no.missing.sd.l2 <- perc.missing.sd.l2 <- no.missing.min.l2 <- perc.missing.min.l2 <-
      no.missing.p25.l2 <- perc.missing.p25.l2 <- no.missing.p75.l2 <- perc.missing.p75.l2 <- no.missing.max.l2 <- perc.missing.max.l2 <- table.missing.l2 <- NA

    }

  }

  #...................
  ### Level-3 Variables ####

  if (isTRUE(no.clust == "two")) {

    ##### Level-3 Variables
    if (isTRUE(length(L3.var) != 0L)) {

      x.l3 <- x[!duplicated(cluster[, 1L]), L3.var, drop = FALSE]

      # Number of complete cases
      no.complete.l3 <- sum(apply(x.l3, 1L, function(y) all(!is.na(y))))
      perc.complete.l3 <- no.complete.l3 / no.cluster.l3 * 100L

      # Number and percentage of imcomplete cases
      no.incomplete.l3 <- sum(apply(x.l3, 1L, function(y) any(is.na(y))))
      perc.incomplete.l3 <- no.incomplete.l3 / no.cluster.l3 * 100L

      # Number of values
      no.values.l3 <- length(unlist(x.l3))

      # Number of observed values
      no.observed.values.l3 <- sum(!is.na(unlist(x.l3)))
      perc.observed.values.l3 <- no.observed.values.l3 / no.values.l3 *100L

      # Number and percentage of missing values
      no.missing.values.l3 <- sum(is.na(unlist(x.l3)))
      perc.missing.values.l3 <- no.missing.values.l3 / no.values.l3 * 100L

      # Number of variables
      no.var.l3 <- ncol(x.l3)

      # Number and percentage of observed values for each variable
      no.observed.var.l3 <- vapply(x.l3, function(y) sum(!is.na(y)), FUN.VALUE = 1L)
      perc.observed.var.l3 <- no.observed.var.l3 / no.cluster.l3 * 100L

      # Number and percentage of missing values for each variable
      no.missing.var.l3 <- vapply(x.l3, function(y) sum(is.na(y)), FUN.VALUE = 1L)
      perc.missing.var.l3 <- no.missing.var.l3 / no.cluster.l3 * 100L

      no.missing.mean.l3 <- mean(no.missing.var.l3)
      perc.missing.mean.l3 <- no.missing.mean.l3 / no.cluster.l3 * 100L

      no.missing.sd.l3 <- misty::na.as(sd(no.missing.var.l3), na = 0L, check = FALSE)
      perc.missing.sd.l3 <- no.missing.sd.l3 / no.cluster.l3 * 100L

      no.missing.min.l3 <- min(no.missing.var.l3)
      perc.missing.min.l3 <- no.missing.min.l3 / no.cluster.l3 * 100L

      no.missing.p25.l3 <- quantile(no.missing.var.l3, probs = 0.25)
      perc.missing.p25.l3 <- no.missing.p25.l3 / no.cluster.l3 * 100L

      no.missing.p75.l3 <- quantile(no.missing.var.l3, probs = 0.75)
      perc.missing.p75.l3 <- no.missing.p75.l3 / no.cluster.l3 * 100L

      no.missing.max.l3 <- max(no.missing.var.l3)
      perc.missing.max.l3 <- no.missing.max.l3 / no.cluster.l3 * 100L

      # Frequency table
      table.missing.l3 <- data.frame(Var = colnames(x.l3),
                                     matrix(c(no.observed.var.l3, perc.observed.var.l3, no.missing.var.l3, perc.missing.var.l3), ncol = 4L,
                                            dimnames = list(NULL, c("nObs", "pObs", "nNA", "pNA"))))

    # No Level-3 Variables
    } else {

      no.complete.l3 <- perc.complete.l3 <- no.incomplete.l3 <- perc.incomplete.l3 <- no.values.l3 <- no.observed.values.l3 <- perc.observed.values.l3 <-
      no.missing.values.l3 <- perc.missing.values.l3 <- no.var.l3 <- no.observed.var.l3 <- perc.observed.var.l3 <- no.missing.var.l3 <- perc.missing.var.l3 <-
      no.missing.mean.l3 <- perc.missing.mean.l3 <- no.missing.sd.l3 <- perc.missing.sd.l3 <- no.missing.min.l3 <- perc.missing.min.l3 <-
      no.missing.p25.l3 <- perc.missing.p25.l3 <- no.missing.p75.l3 <- perc.missing.p75.l3 <- no.missing.max.l3 <- perc.missing.max.l3 <- table.missing.l3 <- NA

    }

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "na.descript",
                 data = if (is.null(cluster)) { x } else { switch(no.clust, one = { data.frame(x = x, cluster = cluster, stringsAsFactors = FALSE) }, two = data.frame(x = x, cluster3 = cluster[, 1L], cluster2 = cluster[, 2L], stringsAsFactors = FALSE)) },
                 no.cluster = no.clust,
                 args = list(digits = digits, table = table, as.na = as.na, write = write, append = append, check = check, output = output),
                 result = switch(no.clust,
                                 none = list(L1 = list(no.cases.l1 = no.cases.l1, no.complete.l1 = no.complete.l1, perc.complete.l1 = perc.complete.l1,
                                                       no.incomplete.l1 = no.incomplete.l1, perc.incomplete.l1 = perc.incomplete.l1,
                                                       no.values.l1 = no.values.l1, no.observed.values.l1 = no.observed.values.l1,
                                                       perc.observed.values.l1 = perc.observed.values.l1, no.missing.values.l1 = no.missing.values.l1,
                                                       perc.missing.values.l1 = perc.missing.values.l1, no.var.l1 = no.var.l1,
                                                       no.missing.mean.l1 = no.missing.mean.l1, perc.missing.mean.l1 = perc.missing.mean.l1,
                                                       no.missing.sd.l1 = no.missing.sd.l1, perc.missing.sd.l1 = perc.missing.sd.l1,
                                                       no.missing.min.l1 = no.missing.min.l1, perc.missing.min.l1 = perc.missing.min.l1,
                                                       no.missing.p25.l1 = no.missing.p25.l1, perc.missing.p25.l1 = perc.missing.p25.l1,
                                                       no.missing.p75.l1 = no.missing.p75.l1, perc.missing.p75.l1 = perc.missing.p75.l1,
                                                       no.missing.max.l1 = no.missing.max.l1, perc.missing.max.l1 = perc.missing.max.l1,
                                                       table.miss.l1 = table.missing.l1)),
                                 one = list(L1 = list(no.cases.l1 = no.cases.l1, no.complete.l1 = no.complete.l1, perc.complete.l1 = perc.complete.l1,
                                                      no.incomplete.l1 = no.incomplete.l1, perc.incomplete.l1 = perc.incomplete.l1,
                                                      no.values.l1 = no.values.l1, no.observed.values.l1 = no.observed.values.l1,
                                                      perc.observed.values.l1 = perc.observed.values.l1, no.missing.values.l1 = no.missing.values.l1,
                                                      perc.missing.values.l1 = perc.missing.values.l1, no.var.l1 = no.var.l1,
                                                      no.missing.mean.l1 = no.missing.mean.l1, perc.missing.mean.l1 = perc.missing.mean.l1,
                                                      no.missing.sd.l1 = no.missing.sd.l1, perc.missing.sd.l1 = perc.missing.sd.l1,
                                                      no.missing.min.l1 = no.missing.min.l1, perc.missing.min.l1 = perc.missing.min.l1,
                                                      no.missing.p25.l1 = no.missing.p25.l1, perc.missing.p25.l1 = perc.missing.p25.l1,
                                                      no.missing.p75.l1 = no.missing.p75.l1, perc.missing.p75.l1 = perc.missing.p75.l1,
                                                      no.missing.max.l1 = no.missing.max.l1, perc.missing.max.l1 = perc.missing.max.l1,
                                                      table.miss.l1 = table.missing.l1),
                                            L2 = list(no.cluster.l2 = no.cluster.l2, no.complete.l2 = no.complete.l2, perc.complete.l2 = perc.complete.l2,
                                                      no.incomplete.l2 = no.incomplete.l2, perc.incomplete.l2 = perc.incomplete.l2,
                                                      no.values.l2 = no.values.l2, no.observed.values.l2 = no.observed.values.l2,
                                                      perc.observed.values.l2 = perc.observed.values.l2, no.missing.values.l2 = no.missing.values.l2,
                                                      perc.missing.values.l2 = perc.missing.values.l2, no.var.l2 = no.var.l2,
                                                      no.missing.mean.l2 = no.missing.mean.l2, perc.missing.mean.l2 = perc.missing.mean.l2,
                                                      no.missing.sd.l2 = no.missing.sd.l2, perc.missing.sd.l2 = perc.missing.sd.l2,
                                                      no.missing.min.l2 = no.missing.min.l2, perc.missing.min.l2 = perc.missing.min.l2,
                                                      no.missing.p25.l2 = no.missing.p25.l2, perc.missing.p25.l2 = perc.missing.p25.l2,
                                                      no.missing.p75.l2 = no.missing.p75.l2, perc.missing.p75.l2 = perc.missing.p75.l2,
                                                      no.missing.max.l2 = no.missing.max.l2, perc.missing.max.l2 = perc.missing.max.l2,
                                                      table.miss.l2 = table.missing.l2)),
                                 two = list(L1 = list(no.cases.l1 = no.cases.l1, no.complete.l1 = no.complete.l1, perc.complete.l1 = perc.complete.l1,
                                                      no.incomplete.l1 = no.incomplete.l1, perc.incomplete.l1 = perc.incomplete.l1,
                                                      no.values.l1 = no.values.l1, no.observed.values.l1 = no.observed.values.l1,
                                                      perc.observed.values.l1 = perc.observed.values.l1, no.missing.values.l1 = no.missing.values.l1,
                                                      perc.missing.values.l1 = perc.missing.values.l1, no.var.l1 = no.var.l1,
                                                      no.missing.mean.l1 = no.missing.mean.l1, perc.missing.mean.l1 = perc.missing.mean.l1,
                                                      no.missing.sd.l1 = no.missing.sd.l1, perc.missing.sd.l1 = perc.missing.sd.l1,
                                                      no.missing.min.l1 = no.missing.min.l1, perc.missing.min.l1 = perc.missing.min.l1,
                                                      no.missing.p25.l1 = no.missing.p25.l1, perc.missing.p25.l1 = perc.missing.p25.l1,
                                                      no.missing.p75.l1 = no.missing.p75.l1, perc.missing.p75.l1 = perc.missing.p75.l1,
                                                      no.missing.max.l1 = no.missing.max.l1, perc.missing.max.l1 = perc.missing.max.l1,
                                                      table.miss.l1 = table.missing.l1),
                                            L2 = list(no.cluster.l2 = no.cluster.l2, no.complete.l2 = no.complete.l2, perc.complete.l2 = perc.complete.l2,
                                                      no.incomplete.l2 = no.incomplete.l2, perc.incomplete.l2 = perc.incomplete.l2,
                                                      no.values.l2 = no.values.l2, no.observed.values.l2 = no.observed.values.l2,
                                                      perc.observed.values.l2 = perc.observed.values.l2, no.missing.values.l2 = no.missing.values.l2,
                                                      perc.missing.values.l2 = perc.missing.values.l2, no.var.l2 = no.var.l2,
                                                      no.missing.mean.l2 = no.missing.mean.l2, perc.missing.mean.l2 = perc.missing.mean.l2,
                                                      no.missing.sd.l2 = no.missing.sd.l2, perc.missing.sd.l2 = perc.missing.sd.l2,
                                                      no.missing.min.l2 = no.missing.min.l2, perc.missing.min.l2 = perc.missing.min.l2,
                                                      no.missing.p25.l2 = no.missing.p25.l2, perc.missing.p25.l2 = perc.missing.p25.l2,
                                                      no.missing.p75.l2 = no.missing.p75.l2, perc.missing.p75.l2 = perc.missing.p75.l2,
                                                      no.missing.max.l2 = no.missing.max.l2, perc.missing.max.l2 = perc.missing.max.l2,
                                                      table.miss.l2 = table.missing.l2),
                                            L3 = list(no.cluster.l3 = no.cluster.l3, no.complete.l3 = no.complete.l3, perc.complete.l3 = perc.complete.l3,
                                                      no.incomplete.l3 = no.incomplete.l3, perc.incomplete.l3 = perc.incomplete.l3,
                                                      no.values.l3 = no.values.l3, no.observed.values.l3 = no.observed.values.l3,
                                                      perc.observed.values.l3 = perc.observed.values.l3, no.missing.values.l3 = no.missing.values.l3,
                                                      perc.missing.values.l3 = perc.missing.values.l3, no.var.l3 = no.var.l3,
                                                      no.missing.mean.l3 = no.missing.mean.l3, perc.missing.mean.l3 = perc.missing.mean.l3,
                                                      no.missing.sd.l3 = no.missing.sd.l3, perc.missing.sd.l3 = perc.missing.sd.l3,
                                                      no.missing.min.l3 = no.missing.min.l3, perc.missing.min.l3 = perc.missing.min.l3,
                                                      no.missing.p25.l3 = no.missing.p25.l3, perc.missing.p25.l3 = perc.missing.p25.l3,
                                                      no.missing.p75.l3 = no.missing.p75.l3, perc.missing.p75.l3 = perc.missing.p75.l3,
                                                      no.missing.max.l3 = no.missing.max.l3, perc.missing.max.l3 = perc.missing.max.l3,
                                                      table.miss.l3 = table.missing.l3))))

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Write Results --------------------------------------------------------------

  if (isTRUE(!is.null(write))) { .write.result(object = object, write = write, append = append) }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  if (isTRUE(output)) { print(object) }

  return(invisible(object))

}

#_______________________________________________________________________________
