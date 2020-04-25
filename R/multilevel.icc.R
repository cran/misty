#' Intraclass Correlation Coefficient, ICC(1) and ICC(2)
#'
#' This function computes the intraclass correlation coefficient ICC(1), i.e., proportion of the total variance explained
#' by the grouping structure, and ICC(2), i.e., reliability of aggregated variables.
#'
#' Note that this function is restricted to two-level models.
#'
#' @param x           a vector, matrix or data frame.
#' @param group       a vector representing the grouping structure (i.e., group variable).
#' @param type        numeric value indicating the type of intraclass correlation coefficient, i.e.,
#'                    \code{type = 1} for ICC(1) and \code{type = 2} for ICC(2).
#' @param method      a character string indicating the method used to estimate intraclass correlation coefficients, i.e.,
#'                    \code{method = "aov"} (default) ICC estimated using the \code{aov} function,
#'                    \code{method = "lme4"} ICC estimated using the \code{lmer} function in the \pkg{lme4} package,
#'                    \code{method = "nlme"} ICC estimated using the \code{lme} function in the \pkg{nlme} package.
#' @param REML        logical: if \code{TRUE}, restricted maximum likelihood is used to estimate the null model when
#'                    using the \code{lmer} function in the \pkg{lme4} package or the \code{lme} function in the \pkg{nlme} package.
#' @param as.na       a numeric vector indicating user-defined missing values,
#'                    i.e. these values are converted to \code{NA} before conducting the analysis.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{multilevel.descript}}
#'
#' @references
#' Hox, J., Moerbeek, M., & van de Schoot, R. (2018). \emph{Multilevel analysis: Techniques and applications} (3rd. ed.).
#' Routledge.
#'
#' Snijders, T. A. B., & Bosker, R. J. (2012). \emph{Multilevel analysis: An introduction to basic and advanced multilevel
#' modeling} (2nd ed.). Sage Publishers.
#'
#' @return
#' Returns a numeric vector with intraclass correlation coefficient(s).
#'
#' @export
#'
#' @examples
#' dat <- data.frame(id = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
#'                   group = c(1, 1, 1, 1, 2, 2, 3, 3, 3),
#'                   x1 = c(2, 3, 2, 2, 1, 2, 3, 4, 2),
#'                   x2 = c(3, 2, 2, 1, 2, 1, 3, 2, 5),
#'                   x3 = c(2, 1, 2, 2, 3, 3, 5, 2, 4), stringsAsFactors = FALSE)
#'
#' # ICC(1) for x1
#' multilevel.icc(dat$x1, group = dat$group)
#'
#' # ICC(1) for x1, convert value 1 to NA
#' multilevel.icc(dat$x1, group = dat$group, as.na = 1)
#'
#' # ICC(2) for x1
#' multilevel.icc(dat$x1, group = dat$group, type = 2)
#'
#' # ICC(1) for x1,
#' # use lmer() function in the lme4 package to estimate ICC
#' multilevel.icc(dat$x1, group = dat$group, method = "lme4")
#'
#' # ICC(1) for x1, x2, and x3
#' multilevel.icc(dat[, c("x1", "x2", "x3")], group = dat$group)
multilevel.icc <- function(x, group, type = 1, method = c("aov", "lme4", "nlme"), REML = TRUE,
                           as.na = NULL, check = TRUE) {

  ####################################################################################
  # Data

  #......
  # Check if input 'x' is missing
  if (missing(x)) {

    stop("Please specify a vector, matrix or data frame for the argument 'x'.", call. = FALSE)

  }

  #......
  # Check if input 'x' is NULL
  if (is.null(x)) {

    stop("Input specified for the argument 'x' is NULL.", call. = FALSE)

  }

  #......
  # Vector, matrix or data frame for the argument 'x'?
  if (!is.atomic(x) && !is.matrix(x) && !is.data.frame(x)) {

    stop("Please specify a numeric vector, matrix or data frame with numeric variables for the argument 'x'.",
         call. = FALSE)

  }

  #......
  # Check input 'group'
  if (missing(group)) {

    stop("Please specify a vector representing the grouping structure for the argument 'group'.", call. = FALSE)

  }

  #----------------------------------------
  # Convert user-missing values into NA

  if (!is.null(as.na)) {

    x <- misty::as.na(x, as.na = as.na, check = check)

    # Variable with missing values only
    x.miss <- vapply(as.data.frame(x), function(y) all(is.na(y)), FUN.VALUE = logical(1))
    if (any(x.miss)) {

      stop(paste0("After converting user-missing values into NA, following variables are completely missing: ",
                  paste(names(which(x.miss)), collapse = ", ")), call. = FALSE)

    }

  }

  ####################################################################################
  # Input Check

  # Check input 'check'
  if (!isTRUE(isTRUE(check) || !isTRUE(check))) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }

  #-----------------------------------------

  if (isTRUE(check)) {

    #......
    # Check input 'group'
    if (length(unique(na.omit(group))) == 1L) {

      stop("There is only one group represented in the grouping variable specified in 'group'.",
           call. = FALSE)

    }

    #......
    # Check input 'group'
    if (is.null(dim(x))) {

      # Numeric vector and group?
      if (length(x) != length(group)) {

        stop("Length of the vector 'x' does not match with the length of the grouping variable in 'group'.",
             call. = FALSE)

      }

    } else {

      # Numeric vector and group?
      if (nrow(x) != length(group)) {

        stop("Number of rows in 'x' does not match with the length of the grouping variable 'group'.",
             call. = FALSE)

      }

    }

    #......
    # Variance within group
    if (is.null(dim(x))) {

      if (all(tapply(unlist(x), group, function(y) length(na.omit(y))) <= 1L)) {

        stop("Varialbe specified in 'x' does not have any within-group variance.", call. = FALSE)

      }

    } else {

      if (any(apply(x, 2, function(y) all(tapply(y, group, function(z) length(na.omit(z))) <= 1L)))) {

        stop("There are variables in 'x' without any within-group variance.", call. = FALSE)

      }

    }

    #......
    # Check input 'x': Zero variance?
    x.check <- vapply(as.data.frame(x), function(y) length(na.omit(unique(y))) == 1L, FUN.VALUE = logical(1))

    if (any(x.check)) {

      if (length(x.check) > 1L) {

        warning(paste0("Following variables in the matrix or data frame specified in 'x' have zero variance: ",
                       paste(names(which(x.check)), collapse = ", ")), call. = FALSE)

      } else {

        stop("Vector specified in 'x' has zero variance.", call. = FALSE)

      }

    }

    #......
    # Check input 'type'
    if (any(!type %in% c(1L, 2L))) {

      stop("Please specify the numeric value 1 or 2 for the argument'type'.", call. = FALSE)

    }

    #......
    # Check input 'method'
    if (any(!method %in% c("aov", "lme4", "nlme"))) {

      stop("Character string in the argument 'method' does not match with \"aov\", \"lme4\", or \"nlme\".",
           call. = FALSE)

    }

    #......
    # Check input 'REML'
    if (!isTRUE(isTRUE(REML) || !isTRUE(REML))) {

      stop("Please specify TRUE or FALSE for the argument 'REML'.", call. = FALSE)

    }

  }

  ####################################################################################
  # Data and Arguments

  if (all(c("aov", "lme4", "nlme") %in% method)) { method <- "aov"}

  # Package lme4 installed?
  if (method == "lme4") {

    if (!requireNamespace("lme4", quietly = TRUE)) {

      warning("Package \"lme4\" is needed for method = \"lme4\", default method \"aov\" will be used instead.",
              call. = FALSE )

      method <- "aov"

    }

  }

  # Package nlme installed?
  if (method == "nlme") {

    if (!requireNamespace("lme4", quietly = TRUE)) {

      warning("Package \"nlme\" is needed for method = \"nlme\", default method \"aov\" will be used instead.",
              call. = FALSE )

      method <- "aov"

    }

  }

  ####################################################################################
  # Main Function

  #-----------------------------------------
  # One dependent variable

  if (is.null(dim(x))) {

    #............
    # Variable with non-zero variance
    if (var(x, na.rm = TRUE) != 0L) {

      # ICC using aov() function
      if (method == "aov") {

        # Estimate model
        mod <- aov(x ~ 1 + Error(as.factor(group)))

        # Model summary
        mod.summary <- summary(mod)

        # Mean Squared Error Between
        MSQ.B <- unlist(mod.summary[[1]])["Mean Sq"]

        # Mean Squared Error Within
        MSQ.W <- unlist(mod.summary[[2]])["Mean Sq"]

        # ICC(1)
        if (type == 1L) {

          # Average group size
          group.size <- mean(tapply(x, group, function(y) sum(!is.na(y))))

          # Intraclass correlation coefficient, ICC(1)
          object <- unname((MSQ.B - MSQ.W) / (MSQ.B + ((group.size - 1L) * MSQ.W)))

          if (object < 0L) { object <- 0L }

        # ICC(2)
        } else {

          # Intraclass correlation coefficient, ICC(2)
          object <- unname((MSQ.B - MSQ.W) / MSQ.B)

          if (object < 0L) { object <- 0L }

        }

      }

      # ICC using lmer() function
      if (method == "lme4") {

        # Estimate model
        mod <- lme4::lmer(x ~ 1 + (1|group), REML = REML,
                          control = lme4::lmerControl(optimizer = "bobyqa"))

        # Variance components
        vartab <- as.data.frame(lme4::VarCorr(mod))

        # Between-group variance
        var.u <- vartab[vartab$grp == "group", "vcov"]

        # Within-group variance
        var.r <- vartab[vartab$grp == "Residual", "vcov"]

        # Total variance
        var.total <- var.u + var.r

        # ICC(1)
        if (type == 1) {

          # Intraclass correlation coefficient, ICC(1)
          object <- var.u / var.total

        # ICC(2)
        } else {

          # Average group size
          group.size <- mean(tapply(x, group, function(y) sum(!is.na(y))))

          # Intraclass correlation coefficient, ICC(2)
          object <- var.u / (var.u + var.r / group.size)

        }

      }

      # ICC using lme() function
      if (method == "nlme") {

        # REML or ML
        ifelse(isTRUE(REML), REML <- "REML", REML <- "ML")

        # Estimate model
        mod <- nlme::lme(x ~ 1, random = ~1 | group, na.action = na.omit, method = REML)

        # Variance components
        vartab <- nlme::VarCorr(mod)

        var.u  <- as.numeric(vartab["(Intercept)", "Variance"])

        var.r <- as.numeric(vartab["Residual", "Variance"])

        # Total variance
        var.total <- var.u + var.r

        # ICC(1)
        if (type == 1) {

          # Intraclass correlation coefficient, ICC(1)
          object <- var.u / var.total

          # ICC(2)
        } else {

          # Average group size
          group.size <- mean(tapply(x, group, function(y) sum(!is.na(y))))

          # Intraclass correlation coefficient, ICC(2)
          object <- var.u / (var.u + var.r / group.size)

        }

      }

    #............
    # Variable with non-zero variance
    } else {

      object <- NA

    }

  #-----------------------------------------
  # More than one dependent variable
  } else {

    object <- apply(x, 2, function(y) misty::multilevel.icc(y, group, type = type, method = method,
                                                            REML = REML, as.na = NULL, check = FALSE))

  }

  ####################################################################################
  # Output

  return(object)

}
