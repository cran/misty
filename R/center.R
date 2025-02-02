#' Centering Predictor Variables in Single-Level and Multilevel Data
#'
#' This function centers predictor variables in single-level data, two-level
#' data, and three-level data at the grand mean (CGM, i.e., grand mean centering)
#' or within cluster (CWC, i.e., group mean centering).
#'
#' @param ...      a numeric vector for centering a predictor variable, or a
#'                 data frame for centering more than one predictor. Alternatively,
#'                 an expression indicating the variable names in \code{data} e.g.,
#'                 \code{center(x1, x2, data = dat)}. Note that the operators
#'                 \code{.}, \code{+}, \code{-}, \code{~}, \code{:}, \code{::},
#'                 and \code{!} can also be used to select variables, see 'Details'
#'                 in the \code{\link{df.subset}} function.
#' @param data     a data frame when specifying one or more predictor variables
#'                 in the argument \code{...}. Note that the argument is \code{NULL}
#'                 when specifying a numeric vector or data frame for the argument
#'                 \code{...}.
#' @param cluster  a character string indicating the name of the cluster
#'                 variable in \code{...} or \code{data} for a two-level model,
#'                 a character vector indicating the names of the cluster
#'                 variables in \code{...} for a three-level model, or a vector
#'                 or data frame representing the nested grouping structure
#'                 (i.e., group or cluster variables). Alternatively, a
#'                 character string or character vector indicating the variable
#'                 name(s) of the cluster variable(s) in \code{data}. Note that
#'                 the cluster variable at Level 3 come first in a three-level
#'                 model, i.e., \code{cluster = c("level3", "level2")}.
#' @param type     a character string indicating the type of centering, i.e.,
#'                 \code{"CGM"} for centering at the grand mean (i.e., grand mean
#'                 centering, default when \code{cluster = NULL}) or \code{"CWC"}
#'                 for centering within cluster (i.e., group mean centering, default
#'                 when specifying the argument \code{cluster}).
#' @param cwc.mean a character string indicating the type of centering of a level-1
#'                 predictor variable in a three-level model, i.e., \code{L2}
#'                 (default) for centering the predictor variable at the level-2
#'                 cluster means, and  \code{L2} for centering the predictor
#'                 variable at the level-3 cluster means.
#' @param value    a numeric value for centering on a specific user-defined value.
#'                 Note that this option is only available when specifying a
#'                 single-level predictor variable, i.e., \code{cluster = NULL}.
#' @param name     a character string or character vector indicating the names of
#'                 the centered predictor variables. By default, centered predictor
#'                 variables are named with the ending \code{".c"} resulting in
#'                 e.g. \code{"x1.c"} and \code{"x2.c"}. Variable names can also
#'                 be specified by using a character vector matching the number
#'                 of variables specified in \code{...} (e.g.,
#'                 \code{name = c("center.x1", "center.x2")}).
#' @param append   logical: if \code{TRUE} (default), centered predictor variable(s)
#'                 are appended to the data frame specified in the argument \code{data}.
#' @param as.na    a numeric vector indicating user-defined missing values, i.e.
#'                 these values are converted to \code{NA} before conducting the
#'                 analysis. Note that \code{as.na()} function is only applied to
#'                 \code{...} but not to \code{cluster}.
#' @param check    logical: if \code{TRUE} (default), argument specification is
#'                 checked.
#'
#' @details
#' \describe{
#' \item{\strong{Single-Level Data}}{\strong{Predictor variables in single-level
#' data} can only be centered at the grand mean (CGM) by specifying
#' \code{type = "CGM"}:
#'
#' \deqn{x_{i} - \bar{x}_{.}}
#'
#' where \eqn{x_{i}} is the predictor value of observation \eqn{i} and
#' \eqn{\bar{x}_{.}} is the average \eqn{x} score. Note that predictor variables
#' can be centered on any meaningful value specifying the argument \code{value},
#' e.g., a predictor variable centered at 5 by applying following formula:
#'
#' \deqn{x_{i} - \bar{x}_{.} + 5}
#'
#' resulting in a mean of the centered predictor variable of 5.
#' }
#' \item{\strong{Two-Level Data}}{\strong{Level-1 (L1) predictor variables} in
#' two-level data can be centered at the grand mean (CGM) by specifying
#' \code{type = "CGM"}:
#'
#' \deqn{x_{ij} - \bar{x}_{..}}
#'
#' where \eqn{x_{ij}} is the predictor value of observation \eqn{i} in L2 cluster
#' \eqn{j} and \eqn{\bar{x}_{..}} is the average \eqn{x} score.
#'
#' L1 predictor variables are centered at the group mean (CWC) by specifying
#' \code{type = "CWC"} (Default):
#'
#' \deqn{x_{ij} - \bar{x}_{.j}}
#'
#' where \eqn{\bar{x_{.j}}} is the average \eqn{x} score in cluster \eqn{j}.
#'
#' \strong{Level-2 (L1) predictor variables} in two-level data can only be
#' centered at the grand mean:
#'
#' \deqn{x_{.j} - \bar{x}_{..}}
#'
#' where \eqn{x_{.j}} is the predictor value of Level 2 cluster \eqn{j} and
#' \eqn{\bar{x}_{..}} is the average Level-2 cluster score. Note that the cluster
#' membership variable needs to be specified when centering a L2 predictor variable
#' in two-level data. Otherwise the average \eqn{x_{ij}} individual score instead
#' of the average \eqn{x_{.j}} cluster score is used to center the predictor
#' variable.
#' }
#' \item{\strong{Three-Level Data}}{\strong{Level-1 (L1) predictor variables} in
#' three-level data can be centered at the grand mean (CGM) by specifying
#' \code{type = "CGM"} (Default):
#'
#' \deqn{x_{ijk} - \bar{x}_{...}}
#'
#' where \eqn{x_{ijk}} is the predictor value of observation \eqn{i} in Level-2
#' cluster \eqn{j} within Level-3 cluster \eqn{k} and \eqn{\bar{x}_{...}} is the
#' average \eqn{x} score.
#'
#' L1 predictor variables are centered within cluster (CWC) by specifying
#' \code{type = "CWC"} (Default). However, L1 predictor variables can be either
#' centered within Level-2 cluster (\code{cwc.mean = "L2"}, Default,
#' see Brincks et al., 2017):
#'
#' \deqn{x_{ijk} - \bar{x}_{.jk}}
#'
#' or within Level-3 cluster (\code{cwc.mean = "L3"}, see Enders, 2013):
#'
#' \deqn{x_{ijk} - \bar{x}_{..k}}
#'
#' where \eqn{\bar{x}_{.jk}} is the average \eqn{x} score in Level-2 cluster
#' \eqn{j} within Level-3 cluster \eqn{k} and \eqn{\bar{x}_{..k}} is the average
#' \eqn{x} score in Level-3 cluster \eqn{k}.
#'
#' \strong{Level-2 (L2) predictor variables} in three-level data can be centered
#' at the grand mean (CGM) by specifying \code{type = "CGM"}:
#'
#' \deqn{x_{.jk} - \bar{x}_{...}}
#'
#' where \eqn{x_{.jk}} is the predictor value of Level-2 cluster \eqn{j} within
#' Level-3 cluster \eqn{k} and \eqn{\bar{x}_{...}} is the average Level-2 cluster
#' score.
#'
#' L2 predictor variables are centered within cluster (CWC) by specifying
#' \code{type = "CWC"} (Default):
#'
#' \deqn{x_{.jk} - \bar{x}_{..k}}
#'
#' where \eqn{\bar{x}_{..k}} is the average \eqn{x} score in Level-3 cluster
#' \eqn{k}.
#'
#' \strong{Level-3 (L3) predictor variables} in three-level data can only be
#' centered at the grand mean:
#'
#' \deqn{x_{..k} - \bar{x}_{...}}
#'
#' where \eqn{x_{..k}} is the predictor value of Level-3 cluster \eqn{k} and
#' \eqn{\bar{x}_{...}} is the average Level-3 cluster score. Note that the cluster
#' membership variable needs to be specified when centering a L3 predictor variable
#' in three-level data.}
#' }
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{coding}}, \code{\link{cluster.scores}}, \code{\link{rec}},
#' \code{\link{item.reverse}}, \code{\link{rwg.lindell}}, \code{\link{item.scores}}.
#'
#' @references
#' Brincks, A. M., Enders, C. K., Llabre, M. M., Bulotsky-Shearer, R. J., Prado, G.,
#' & Feaster, D. J. (2017). Centering predictor variables in three-level contextual
#' models. \emph{Multivariate Behavioral Research, 52}(2), 149–163.
#' https://doi.org/10.1080/00273171.2016.1256753
#'
#' Chang, C.-N., & Kwok, O.-M. (2022) Partitioning Variance for a Within-Level
#' Predictor in Multilevel Models. \emph{Structural Equation Modeling: A
#' Multidisciplinary Journal}. Advance online publication.
#' https://doi.org/10.1080/10705511.2022.2051175#'
#'
#' Enders, C. K. (2013). Centering predictors and cont
#' extual effects. In M. A.
#' Scott, J. S. Simonoff, & B. D. Marx (Eds.), \emph{The Sage handbook of
#' multilevel modeling} (pp. 89-109). Sage. https://dx.doi.org/10.4135/9781446247600
#'
#' Enders, C. K., & Tofighi, D. (2007). Centering predictor variables in
#' cross-sectional multilevel models: A new look at an old issue. \emph{Psychological
#' Methods, 12}, 121-138. https://doi.org/10.1037/1082-989X.12.2.121
#'
#' Rights, J. D., Preacher, K. J., & Cole, D. A. (2020). The danger of conflating
#' level-specific effects of control variables when primary interest lies in
#' level-2 effects. \emph{British Journal of Mathematical & Statistical Psychology,
#' 73}, 194-211. https://doi.org/10.1111/bmsp.12194
#'
#' Yaremych, H. E., Preacher, K. J., & Hedeker, D. (2021). Centering categorical
#' predictors in multilevel models: Best practices and interpretation.
#' \emph{Psychological Methods}. Advance online publication.
#' https://doi.org/10.1037/met0000434
#'
#' @return
#' Returns a numeric vector or data frame with the same length or same number of
#' rows as \code{...} containing the centered variable(s).
#'
#' @export
#'
#' @examples
#' #----------------------------------------------------------------------------
#' # Predictor Variables in Single-Level Data
#'
#' # Example 1: Center predictor 'disp' at the grand mean
#' center(mtcars$disp)
#'
#' # Alternative specification using the 'data' argument
#' center(disp, data = mtcars)
#'
#' # Example 2: Center predictors 'disp' and 'hp' at the grand mean and append to 'mtcars'
#' cbind(mtcars, center(mtcars[, c("disp", "hp")]))
#'
#' # Alternative specification using the 'data' argument
#' center(disp, hp, data = mtcars)
#'
#' # Example 3: Center predictor 'disp' at the value 3
#' center(disp, data = mtcars, value = 3)
#'
#' # Example 4: Center predictors 'disp' and 'hp' and label with the suffix ".v"
#' center(disp, hp, data = mtcars, name = ".v")
#'
#' #----------------------------------------------------------------------------
#' # Predictor Variables in Two-Level Data
#'
#' # Load data set "Demo.twolevel" in the lavaan package
#' data("Demo.twolevel", package = "lavaan")
#'
#' # Example 5: Center L1 predictor 'y1' within cluster
#' center(Demo.twolevel$y1, cluster = Demo.twolevel$cluster)
#'
#' # Alternative specification using the 'data' argument
#' center(y1, data = Demo.twolevel, cluster = "cluster")
#'
#' # Example 6: Center L2 predictor 'w2' at the grand mean
#' center(w1, data = Demo.twolevel, cluster = "cluster")
#'
#' # Example 6: Center L1 predictor 'y1' within cluster and L2 predictor 'w1' at the grand mean
#' center(y1, w1, data = Demo.twolevel, cluster = "cluster")
#'
#' #----------------------------------------------------------------------------
#' # Predictor Variables in Three-Level Data
#'
#' # Create arbitrary three-level data
#' Demo.threelevel <- data.frame(Demo.twolevel, cluster2 = Demo.twolevel$cluster,
#'                                              cluster3 = rep(1:10, each = 250))
#'
#' # Example 7a: Center L1 predictor 'y1' within L2 cluster
#' center(y1, data = Demo.threelevel, cluster = c("cluster3", "cluster2"))
#'
#' # Example 7b: Center L1 predictor 'y1' within L3 cluster
#' center(y1, data = Demo.threelevel, cluster = c("cluster3", "cluster2"), cwc.mean = "L3")
#'
#' # Example 7b: Center L1 predictor 'y1' within L2 cluster and L2 predictor 'w1' within L3 cluster
#' center(y1, w1, data = Demo.threelevel, cluster = c("cluster3", "cluster2"))
center <- function(..., data = NULL, cluster = NULL, type = c("CGM", "CWC"),
                   cwc.mean = c("L2", "L3"), value = NULL,  append = TRUE,
                   name = ".c", as.na = NULL, check = TRUE) {

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
    var.names <- .var.names(..., data = data, cluster = cluster, check.chr = "a matrix or data frame")

    # Extract data
    x <- data[, var.names]

    # Cluster variable
    if (isTRUE(!is.null(cluster))) { cluster <- data[, cluster] }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data without using the argument 'data' ####

  } else {

    # Extract data
    x <- eval(..., enclos = parent.frame())

    # Convert tibble into data frame
    if (isTRUE("tbl" %in% substr(class(cluster), 1L, 3L))) { if (isTRUE(ncol(as.data.frame(cluster)) == 1L)) { cluster <- unlist(cluster) } else { cluster <- as.data.frame(cluster) } }
    if (isTRUE("tbl" %in% substr(class(x), 1L, 3L))) { if (isTRUE(ncol(as.data.frame(x)) == 1L)) { x <- unlist(x) } else { x <- as.data.frame(x) } }

    # Data and cluster
    var.group <- .var.group(data = x, cluster = cluster)

    # Data
    if (isTRUE(!is.null(var.group$data)))  { x <- var.group$data }

    # Cluster variable
    if (isTRUE(!is.null(var.group$cluster))) { cluster <- var.group$cluster }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) { x <- .as.na(x, na = as.na) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Cluster variables ####

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
  .check.input(logical = "append", numeric = list(value = 1L), s.character = list(type = c("CGM", "CWC"), cwc.mean = c("L2", "L3")), envir = environment(), input.check = check)

  # Additional checks
  if (isTRUE(check)) {

    # Check input 'name'
    if (isTRUE(!is.null(dim(x)))) {

      if (isTRUE(!is.character(name))) { stop("Please specify a character string or vector for the argument 'name'.", call. = FALSE) }

      if (isTRUE(length(name) > 1L && length(name) != ncol(as.data.frame(x)))) { stop("The length of the vector specified in 'name' does not match with the number of variable.", call. = FALSE) }

    }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Type of centering ####

  if (isTRUE(all(c("L2", "L3") %in% cwc.mean))) { cwc.mean <- "L2" }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Single variable ####

  if (isTRUE(is.null(dim(x)))) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Variable ####

    if (isTRUE(!is.null(cluster))) {

      ##### One cluster variable
      if (isTRUE(no.clust == "one")) {

        # Level 1 Variable
        if (isTRUE(any(na.omit(as.vector(tapply(x, cluster, var, na.rm = TRUE))) != 0L))) {

          vartype <- "L1"

          # Level 2 Variable
        } else {

          vartype = "L2"

        }

      ##### Two cluster variable s
      } else if (isTRUE(no.clust == "two")) {

        # Level 1 Variable
        if (isTRUE(any(na.omit(as.vector(tapply(x, apply(cluster, 1L, paste, collapse = ""), var, na.rm = TRUE))) != 0L))) {

          vartype <- "L1"

        # Level 2 Variable
        } else if (isTRUE(all(na.omit(as.vector(tapply(x, apply(cluster, 1L, paste, collapse = ""), var, na.rm = TRUE))) == 0L) && any(as.vector(tapply(x, cluster[, 1L], var, na.rm = TRUE)) != 0L))) {

          vartype <- "L2"

        # Level 3 Variable
        } else if (isTRUE(all(na.omit(as.vector(tapply(x, apply(cluster, 1L, paste, collapse = ""), var, na.rm = TRUE))) == 0L) && all(na.omit(as.vector(tapply(x, cluster[, 1L], var, na.rm = TRUE))) == 0L))) {

          vartype <- "L3"

        }

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Type of centering ####

    if (isTRUE(all(c("CGM", "CWC") %in% type))) {

      #...................
      ### Single-level ####

      if (isTRUE(is.null(cluster))) {

        type <- "CGM"

        #...................
        ### Multi ####

      } else {

        ##### One cluster variable
        if (isTRUE(no.clust == "one")) {

          type <- switch(vartype, L1 = "CWC", L2 = "CGM")

          ##### Two cluster variable s
        } else if (isTRUE(no.clust == "two")) {

          type <- switch(vartype, L1 = "CWC", L2 = "CWC", L3 = "CGM")

        }

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Centering ####

    #...................
    ### No cluster variable ####

    if (isTRUE(is.null(cluster))) {

      ##### Mean centering
      if (isTRUE(is.null(value))) {

        object <- as.numeric(scale(x, scale = FALSE))

      ##### Centering on a user-defined value
      } else {

        object <- x - mean(x, na.rm = TRUE) + value

      }

    #...................
    ### One cluster variable ####

    } else if (isTRUE(no.clust == "one")) {

      ##### Centering at the grand mean (CGM)
      if (isTRUE(type == "CGM")) {

        switch(vartype,
               # Level 1 predictor
               L1 = {

                 object <- as.numeric(scale(x, scale = FALSE))

               },
               # Level 2 predictor
               L2 = {

                 object <- x - mean(x[which(!duplicated(cluster))], na.rm = TRUE)

               })

      ##### Centering within cluster (CWC)
      } else if (isTRUE(type == "CWC")) {

        switch(vartype,
               # Level 1 predictor
               L1 = {

                 object <- unname(x - misty::cluster.scores(x, cluster = cluster, fun = "mean", check = FALSE, expand = TRUE))

               },
               # Level 2 predictor
               L2 = {

                 # Note, level 2 predictor can only be centered at the grand mean
                 object <- x - mean(x[which(!duplicated(cluster))], na.rm = TRUE)

               })

      }

    #...................
    ### Two cluster variables ####

    } else if (isTRUE(no.clust == "two")) {

      ##### Centering at the grand mean (CGM)
      if (isTRUE(type == "CGM")) {

        switch(vartype,
               # Level 1 predictor
               L1 = {

                 object <- as.numeric(scale(x, scale = FALSE))

               },
               # Level 2 predictor
               L2 = {

                 object <- x - mean(x[which(!duplicated(apply(cluster, 1L, paste, collapse = "")))], na.rm = TRUE)

               },
               # Level 3 predictor
               L3 = {

                 object <- x - mean(x[which(!duplicated(cluster[, 1L]))], na.rm = TRUE)

               })

      ##### Centering within cluster (CWC)
      } else if (isTRUE(type == "CWC")) {

        switch(vartype,
               # Level 1 predictor
               L1 = {

                switch(cwc.mean,
                       # Deviation from the Leve-2 cluster mean
                       L2 = {

                         object <- unname(x - misty::cluster.scores(x, cluster = apply(cluster, 1L, paste, collapse = ""), fun = "mean", check = FALSE, expand = TRUE))

                       },
                       # Deviation from the Leve-3 cluster mean
                       L3 = {

                         object <- x - misty::cluster.scores(x, cluster = cluster[, 1L])

                       })

               },
               # Level 2 predictor
               L2 = {

                 x.cluster <- data.frame(x, cluster3 = cluster[, 1L], cluster2 = cluster[, 2])

                 x.cluster.u <- misty::cluster.scores(x, data = x.cluster[!duplicated(x.cluster$cluster2), ], cluster = "cluster3")

                 x.cluster.u$object <- x.cluster.u$x - x.cluster.u$x.a

                 x.cluster <- merge(x.cluster, x.cluster.u[, c("object", "cluster2")], by = "cluster2")

                 object <- x.cluster[match(cluster[, 2], x.cluster[, "cluster2"]), "object"]

               },
               # Level 3 predictor
               L3 = {

                 # Note, level 3 predictor can only be centered at the grand mean
                 object <- x - mean(x[which(!duplicated(cluster[, 1L]))], na.rm = TRUE)

               })

      }

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Multiple variables ####

  } else {

    object <- data.frame(vapply(x, misty::center, cluster = cluster, type = type, cwc.mean = cwc.mean, value = value, as.na = as.na, check = FALSE,
                                FUN.VALUE = double(nrow(x))))

    #...................
    ### Variable names ####

    if (isTRUE(length(name) == 1L)) {

      colnames(object) <- paste0(colnames(object), name)

    } else {

      colnames(object) <- name

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Append ####

  if (isTRUE(!is.null(data) && append)) {

    if (isTRUE(is.null(dim(x)))) {

      #...................
      ### Variable names ####

      if (isTRUE(name == ".c")) {

        object <- setNames(as.data.frame(object), nm = paste0(var.names, ".c"))

      } else {

        object <- setNames(as.data.frame(object), nm = name)

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
