#' Coefficient Omega, Hierarchical Omega, and Categorical Omega
#'
#' This function computes point estimate and confidence interval for the coefficient omega (McDonald, 1978),
#' hierarchical omega (Kelley & Pornprasertmanit, 2016), and categorical omega (Green & Yang, 2009) along with
#' standardized factor loadings and omega if item deleted.
#'
#' Omega is computed by estimating a confirmatory factor analysis model using the \code{cfa()} function
#' in the \pkg{lavaan} package by Yves Rosseel (2019). Maximum likelihood (\code{"ML"}) estimator is used
#' for computing coefficient omega and hierarchical omega, while diagonally weighted least squares estimator
#' (\code{"DWLS"}) is used for computing categorical omega.
#'
#' Note that the computation of the hierarchical and categorical omega is based on the \code{ci.reliability()}
#' function in the \pkg{MBESS} package by Ken Kelley (2019).
#'
#' Approximate confidence intervals are computed using the procedure by Feldt, Woodruff and Salih (1987).
#' Note that there are at least 10 other procedures for computing the confidence interval (see Kelley and
#' Pornprasertmanit, 2016), which are implemented in the \code{ci.reliability()} function in the \pkg{MBESSS}
#' package by Ken Kelley (2019).
#'
#' @param x          a matrix or data frame. Note that at least three items are needed for computing omega.
#' @param resid.cov  a character vector or a list of character vectors for specifying residual covariances
#'                   when computing coefficient omega, e.g. \code{resid.cov = c("x1", "x2")} for specifying
#'                   a residual covariance between items \code{x1} and \code{x2} or
#'                   \code{resid.cov = list(c("x1", "x2"), c("x3", "x4"))} for specifying residual
#'                   covariances between items between items \code{x1} and \code{x2}, and items \code{x3}
#'                   and \code{x4}.
#' @param type       a character string indicating the type of omega to be computed, i.e.,
#'                   \code{omega} (default) for coefficient omega, \code{hierarch} for
#'                    hierarchical omega, and \code{categ} for categorical omega.
#' @param exclude    a character vector indicating items to be excluded from the analysis.
#' @param std        logical: if \code{TRUE}, the standardized coefficient omega is computed.
#' @param na.omit    logical: if \code{TRUE}, incomplete cases are removed before conducting the analysis
#'                   (i.e., listwise deletion); if \code{FALSE}, full information maximum likelihood (FIML)
#'                   is used for computing coefficient omega or hierarchical omega, while pairwise deletion
#'                   is used for computing categorical omega.
#' @param print      a character vector indicating which results to show, i.e. \code{"all"} (default), for all
#'                   results \code{"omega"} for omega, and \code{"item"} for item statistics.
#' @param digits     an integer value indicating the number of decimal places to be used for displaying
#'                   omega and standardized factor loadings.
#' @param conf.level a numeric value between 0 and 1 indicating the confidence level of the interval.
#' @param as.na      a numeric vector indicating user-defined missing values,
#'                   i.e. these values are converted to \code{NA} before conducting the analysis.
#' @param check      logical: if \code{TRUE}, argument specification is checked.
#' @param output     logical: if \code{TRUE}, output is shown.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#'\code{\link{omega.coef}}, \code{\link{reverse.item}}, \code{\link{scores}}
#'
#' @references
#' Feldt, L. S., Woodruff, D. J., & Salih, F. A. (1987). Statistical inference for coefficient alpha.
#' \emph{Applied Psychological Measurement}, 11 93-103.
#'
#' Green, S. B., & Yang, Y. (2009). Reliability of summed item scores using structural equation modeling:
#' An alternative to coefficient alpha. \emph{Psychometrika, 74}, 155-167. https://doi.org/10.1007/s11336-008-9099-3
#'
#' Kelley, K., & Pornprasertmanit, S. (2016). Confidence intervals for population reliability coefficients:
#' Evaluation of methods, recommendations, and software for composite measures. \emph{Psychological Methods, 21}, 69-92.
#' http://dx.doi.org/10.1037/a0040086
#'
#' Ken Kelley (2019). \emph{MBESS: The MBESS R Package}. R package version 4.6.0.
#' https://CRAN.R-project.org/package=MBESS
#'
#' McDonald, R. P. (1978). Generalizability in factorable domains: "Domain validity and generalizability"
#' \emph{Educational and Psychological Measurement, 38}, 75-79.
#'
#' @return
#' Returns an object of class \code{omega.coef}, which is a list with following entries: function call (\code{call}),
#' matrix or data frame specified in \code{x} (\code{data}), specification of function arguments (\code{args}),
#' fitted lavaan object (\code{mod.fit}), and list with results (\code{result}).
#'
#' @export
#'
#' @examples
#' dat <- data.frame(item1 = c(5, 2, 3, 4, 1, 2, 4, 2),
#'                   item2 = c(5, 3, 3, 5, 2, 2, 5, 1),
#'                   item3 = c(4, 2, 4, 5, 1, 3, 5, 1),
#'                   item4 = c(5, 1, 2, 5, 2, 3, 4, 2),
#'                   stringsAsFactors = FALSE)
#'
#' # Compute unstandardized coefficient omega and item statistics
#' omega.coef(dat)
#'
#' # Compute unstandardized coefficient omega with a residual covariance
#' # and item statistics
#' omega.coef(dat, resid.cov = c("item1", "item2"))
#'
#' # Compute unstandardized coefficient omega with residual covariances
#' # and item statistics
#' omega.coef(dat, resid.cov = list(c("item1", "item2"), c("item3", "item4")))
#'
#' # Compute unstandardized hierarchical omega and item statistics
#' omega.coef(dat, type = "hierarch")
#'
#' # Compute categorical omega and item statistics
#' omega.coef(dat, type = "categ")
#'
#' # Compute standardized coefficient omega and item statistics
#' omega.coef(dat, std = TRUE)
#'
#' # Compute unstandardized coefficient omega
#' omega.coef(dat, print = "omega")
#'
#' # Compute item statistics
#' omega.coef(dat, print = "item")
#'
#' # Compute unstandardized coefficient omega and item statistics while excluding item3
#' omega.coef(dat, exclude = "item3")
#'
#' # Summary of the CFA model used to compute coefficient omega
#' lavaan::summary(omega.coef(dat, output = FALSE)$mod.fit,
#'                 fit.measures = TRUE, standardized = TRUE)
omega.coef <- function(x, resid.cov = NULL, type = c("omega", "hierarch", "categ"),
                       exclude = NULL, std = FALSE, na.omit = FALSE,
                       print = c("all", "omega", "item"), digits = 2,
                       conf.level = 0.95, as.na = NULL, check = TRUE, output = TRUE) {

  #-----------------------------------------------------------------------------------
  ####################################################################################
  # Internal functions
  # - .catOmega
  # - .getThreshold
  # - .polycorLavaan
  # - .refit
  # - .p2
  #
  # MBESS: The MBESS R Package
  # https://cran.r-project.org/web/packages/MBESS/index.html


  #-----------------------------------------------------------------------------------
  # .catOmega Function

  .catOmega <- function(dat, check = TRUE) {

    q <- ncol(dat)
    for(i in 1L:q) {

      dat[, i] <- ordered(dat[, i])

    }

    varnames <- paste0("y", 1L:q)
    colnames(dat) <- varnames

    loadingName <- paste0("a", 1L:q)
    errorName <- paste0("b", 1L:q)

    model <- "f1 =~ NA*y1 + "
    loadingLine <- paste(paste0(loadingName, "*", varnames), collapse = " + ")
    factorLine <- "f1 ~~ 1*f1\n"

    model <- paste(model, loadingLine, "\n", factorLine)
    mod.fit <- suppressWarnings(lavaan::cfa(model, data = dat, estimator = "DWLS", se = "none", ordered = TRUE))

    # Model convergence
    if (isTRUE(check)) {

      if (isFALSE(mod.fit@Fit@converged)) {

        warning("CFA model did not converge, results are most likely unreliable.",
                call. = FALSE)

      }

    }

    param <- lavaan::inspect(mod.fit, "coef")
    ly <- param[["lambda"]]
    ps <- param[["psi"]]

    truevar <- ly%*%ps%*%t(ly)

    threshold <- .getThreshold(mod.fit)[[1]]

    denom <- .polycorLavaan(mod.fit, dat)[varnames, varnames]

    invstdvar <- 1L / sqrt(diag(mod.fit@Fit@Sigma.hat[[1L]]))
    polyr <- diag(invstdvar) %*% truevar %*% diag(invstdvar)

    sumnum <- 0L
    addden <- 0L

    for(j in 1L:q) {

      for(jp in 1L:q) {

        sumprobn2 <- 0L
        addprobn2 <- 0L

        t1 <- threshold[[j]]
        t2 <- threshold[[jp]]

        for(c in 1L:length(t1)) {

          for(cp in 1L:length(t2)) {

            sumprobn2 <- sumprobn2 + .p2(t1[c], t2[cp], polyr[j, jp])
            addprobn2 <- addprobn2 + .p2(t1[c], t2[cp], denom[j, jp])

          }

        }

        sumprobn1 <- sum(pnorm(t1))
        sumprobn1p <- sum(pnorm(t2))
        sumnum <- sumnum + (sumprobn2 - sumprobn1 * sumprobn1p)
        addden <- addden + (addprobn2 - sumprobn1 * sumprobn1p)

      }

    }

    omega <- sumnum / addden

    object <- list(mod.fit = mod.fit, omega = omega)

    return(object)

  }

  #-----------------------------------------------------------------------------------
  # .getThreshold Function

  .getThreshold <- function(object) {

    ngroups <- object@Data@ngroups

    coef <- lavaan::inspect(object, "coef")

    result <- NULL
    if(ngroups == 1L) {

      targettaunames <- rownames(coef$tau)

      barpos <- sapply(strsplit(targettaunames, ""), function(x) which(x == "|"))

      varthres <- apply(data.frame(targettaunames, barpos - 1L, stringsAsFactors = FALSE), 1, function(x) substr(x[1], 1, x[2L]))

      result <- list(split(coef$tau, varthres))

    } else {

      result <- list()

      for (g in 1:ngroups) {

        targettaunames <- rownames(coef[[g]]$tau)

        barpos <- sapply(strsplit(targettaunames, ""), function(x) which(x == "|"))

        varthres <- apply(data.frame(targettaunames, barpos - 1L, stringsAsFactors = FALSE), 1, function(x) substr(x[1L], 1, x[2L]))

        result[[g]] <- split(coef[[g]]$tau, varthres)

      }

    }

    return(result)

  }

  #-----------------------------------------------------------------------------------
  # .polycorLavaan Function

  .polycorLavaan <- function(object, data) {

    ngroups <- object@Data@ngroups

    coef <- lavaan::inspect(object, "coef")

    targettaunames <- NULL

    if(ngroups == 1L) {

      targettaunames <- rownames(coef$tau)

    } else {

      targettaunames <- rownames(coef[[1L]]$tau)

    }

    barpos <- sapply(strsplit(targettaunames, ""), function(x) which(x == "|"))

    varnames <- unique(apply(data.frame(targettaunames, barpos - 1L, stringsAsFactors = FALSE), 1, function(x) substr(x[1L], 1, x[2L])))

    script <- ""

    for(i in 2L:length(varnames)) {

      temp <- paste0(varnames[1L:(i - 1L)], collapse = " + ")

      temp <- paste0(varnames[i], "~~", temp, "\n")

      script <- paste(script, temp)

    }

    suppressWarnings(newobject <- .refit(script, data, varnames, object))

    if(ngroups == 1L) {

      return(lavaan::inspect(newobject, "coef")$theta)

    } else {

      return(lapply(lavaan::inspect(newobject, "coef"), "[[", "theta"))

    }

  }

  #-----------------------------------------------------------------------------------
  # .refit Function

  .refit <- function(pt, data, vnames, object) {

    previousCall <- object@call
    args <- as.list(previousCall[-1L])
    args$model <- pt
    args$data <- data
    args$ordered <- vnames
    funcall <- as.character(previousCall[[1L]])
    tempfit <- do.call(eval(parse(text = paste0("lavaan::", funcall[length(funcall)]))), args)

  }

  #-----------------------------------------------------------------------------------
  # .p2 Function

  .p2 <- function(t1, t2, r) {

    mnormt::pmnorm(c(t1, t2), c(0L, 0L), matrix(c(1L, r, r, 1L), 2L, 2L))

  }

  ####################################################################################
  #-----------------------------------------------------------------------------------


  ####################################################################################
  # Input Check

  #......
  # Check if input 'x' is missing
  if (missing(x)) {

    stop("Please specify a matrix or data frame for the argument 'x'.", call. = FALSE)

  }

  #......
  # Package 'lavaan' installed?
  if (!requireNamespace("lavaan", quietly = TRUE)) {

    stop("Package \"lavaan\" is needed for this function to work, please install it.",
         call. = FALSE)

  }

  #......
  # Package 'mnormt' installed?
  if (isTRUE(ordered)) {

    if (!requireNamespace("mnormt", quietly = TRUE)) {

      stop("Package \"mnormt\" is needed for this function to work, please install it.",
           call. = FALSE)

    }

  }

  #......
  # Check input 'check'
  if (isFALSE(isTRUE(check) || isFALSE(check))) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }

  #----------------------------------------

  if (isTRUE(check)) {

    #......
    # Matrix or data frame for the argument 'x'?
    if (!is.matrix(x) && !is.data.frame(x)) {

      stop("Please specify a matrix or a data frame for the argument 'x'.", call. = FALSE)

    }

    #......
    # Check input 'x': One or two item
    if (ncol(x) < 3L) {

      stop("Please specify at least three items to compute coefficient omega", call. = FALSE)

    }

    #......
    # Check input 'x': Zero variance
    if (nrow(x) != ncol(x)) {

      x.check <- vapply(as.data.frame(x, stringsAsFactors = FALSE), function(y) length(na.omit(unique(y))) == 1, FUN.VALUE = logical(1L))

      if (any(x.check)) {

        stop(paste0("Following variables in the matrix or data frame specified in 'x' have zero variance: ",
                     paste(names(which(x.check)), collapse = ", ")), call. = FALSE)

      }

    }

    #......
    # Check input 'resid.cov'
    if (!is.null(resid.cov)) {

      if (isFALSE(ordered)) {

        resid.cov.items <- unique(unlist(resid.cov))
        if (any(!resid.cov.items %in% colnames(x))) {

          stop(paste0("Items specified in the argument 'resid.cov' were not found in 'x': ",
                      paste(resid.cov.items[!resid.cov.items %in% colnames(x)], collapse = ", ")), call. = FALSE)

        }

      } else if (length(type) == 1L && type %in% c("hierarch", "categ")) {

        warning("Residual covariances cannot be specified when computing hierarchical or categorical omega.", call. = FALSE)

      }

    }

    #......
    # Check input 'type'
    if (!all(type %in% c("omega", "hierarch", "categ"))) {

      stop("Character strings in the argument 'type' do not all match with \"omega\", \"hierarch\", or \"categ\".",
           call. = FALSE)

    }

    #......
    # Check input 'exclude'
    check.ex <- !exclude %in% colnames(x)
    if (any(check.ex)) {

      stop(paste0("Items to be excluded from the analysis were not found in 'x': ", paste(exclude[check.ex], collapse = ", ")),
           call. = FALSE)
    }

    #......
    # Check input 'std'
    if (isFALSE(isTRUE(std) || isFALSE(std))) {

      stop("Please specify TRUE or FALSE for the argument 'std'.", call. = FALSE)

    }

    #......
    # Check input 'print'
    if (!all(print %in% c("all", "omega", "item"))) {

      stop("Character strings in the argument 'print' do not all match with \"all\", \"omega\", or \"item\".",
           call. = FALSE)

    }

    #......
    # Check input 'na.omit'
    if (isFALSE(isTRUE(na.omit) || isFALSE(na.omit))) {

      stop("Please specify TRUE or FALSE for the argument 'na.omit'.", call. = FALSE)

    }

    #......
    # Check input 'digits'
    if (digits %% 1L != 0L || digits < 0L) {

      stop("Specify a positive integer number for the argument 'digits'.", call. = FALSE)

    }

    #......
    # Check input 'conf.level'
    if (conf.level >= 1L || conf.level <= 0L) {

      stop("Please specifiy a numeric value between 0 and 1 for the argument 'conf.level'.",
           call. = FALSE)

    }

    #......
    # Check input 'output'
    if (isFALSE(isTRUE(output) || isFALSE(output))) {

        stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE)

    }

  }

  ####################################################################################
  # Data and Arguments

  #----------------------------------------
  # Data frame

  x <- as.data.frame(x, stringsAsFactors = FALSE)

  #----------------------------------------
  # Exclude items (exclude) and specify user-defined NA (as.na)

  if (!is.null(exclude)) {

    x <- x[, which(!colnames(x) %in% exclude)]

    # One or two items left
    if (ncol(x) <= 2L) {

      stop("At least three items after excluding items are needed to compute coefficient omega.",
           call. = FALSE)

    }

  }

  #----------------------------------------
  # Convert user-missing values into NA

  if (!is.null(as.na)) {

    x <- misty::as.na(x, as.na = as.na, check = check)

    # Variable with missing values only
    x.miss <- vapply(x, function(y) all(is.na(y)), FUN.VALUE = logical(1))
    if (any(x.miss)) {

      stop(paste0("After converting user-missing values into NA, following items are completely missing: ",
                    paste(names(which(x.miss)), collapse = ", ")), call. = FALSE)

    }

    # Zero variance
    x.zero.var <- vapply(x, function(y) length(na.omit(unique(y))) == 1L, FUN.VALUE = logical(1))
    if (any(x.zero.var)) {

      stop(paste0("After converting user-missing values into NA, following items have zero variance: ",
                    paste(names(which(x.zero.var)), collapse = ", ")), call. = FALSE)

    }

  }

  #----------------------------------------
  # Type of omega

  if (all(c(c("omega", "hierarch", "categ")) %in% type)) { type <- "omega" }

  #----------------------------------------
  # Method for handling missing data

  # Listwise deletion
  if (isTRUE(na.omit)) {

    x <- na.omit(x)

  }

  #----------------------------------------
  # Residual covariance

  if (!is.null(resid.cov)) {

    if (!is.list(resid.cov)) { resid.cov <- list(resid.cov) }

  }

  #----------------------------------------
  # Standardize

  # Unstandardized data
  x.raw <- x

  if (isTRUE(std) && type != "categ") {

    x <- as.data.frame(scale(x), stringsAsFactors = FALSE)

  }

  #----------------------------------------
  # Print coefficient omega and/or item statistic

  if (all(c(c("all", "omega", "item")) %in% print)) { print <- c("omega", "item") }

  if (length(print) == 1L && "all" %in% print) { print <- c("omega", "item") }

  ####################################################################################
  # Omega Function

  omega.function <- function(y, y.resid.cov = NULL, y.type = type, y.std = std, check = TRUE) {

    # Variable names
    varnames <- colnames(y)

    #----------------------------------------
    # Omega for continuous items
    if (y.type != "categ") {

      #----------------------------------------
      # Mode specification

      #...................
      # Factor model

      mod.factor <- paste("f =~", paste(varnames, collapse = " + "))

      #...................
      # Residual covariance

      if (!is.null(y.resid.cov)) {

        mod.resid.cov <- vapply(y.resid.cov, function(y) paste(y, collapse = " ~~ "), FUN.VALUE = character(1))

        # Paste residual covariances
        mod.factor <- paste(mod.factor, "\n", paste(mod.resid.cov, collapse = " \n "))

      }

      #----------------------------------------
      # Model estimation

      mod.fit <- suppressWarnings(lavaan::cfa(mod.factor, data = y, ordered = FALSE, se = "none",
                                              std.lv = TRUE, estimator = "ML", missing = "fiml"))

      #................
      # Check for convergence and negative degrees of freedom
      if (isTRUE(check)) {

        # Model convergence
        if (isFALSE(mod.fit@Fit@converged)) {

          warning("CFA model did not converge, results are most likely unreliable.",
                  call. = FALSE)

        }

        # Degrees od freedom
        if (isTRUE(mod.fit@Fit@test[[1]]$df < 0L)) {

          warning("CFA model has negative degrees of freedom, results are most likely unreliable.",
                  call. = FALSE)

        }

      }

      #----------------------------------------
      # Parameter estimates

      if (isFALSE(y.std)) {

        #................
        # Unstandardized parameter estimates

        param <- lavaan::parameterestimates(mod.fit)

      } else {

        #................
        # Standardized parameter estimates

        param <- lavaan::standardizedSolution(mod.fit)

        names(param)[grep("est.std", names(param))] <- "est"

      }

      #................
      # Factor loadings

      param.load <- param[which(param$op == "=~"), ]

      #................
      # Residual covariance

      param.rcov <- param[param$op == "~~" & param$lhs != param$rhs, ]

      #................
      # Residuals

      param.resid <- param[param$op == "~~" & param$lhs == param$rhs & param$lhs != "f" & param$rhs != "f", ]

      #----------------------------------------
      # Omega

      # Numerator
      load.sum2 <- sum(param.load$est)^2L

      #................
      # Total omega
      if (y.type != "hierarch")  {

        resid.sum <- sum(param.resid$est)

        #................
        # Residual covariances
        if (!is.null(y.resid.cov)) {

          resid.sum <- resid.sum + 2L*sum(param.rcov$est)

        }

        omega <- load.sum2 / (load.sum2 + resid.sum)

      #................
      # Hierarchical omega
      } else {

        mod.cov <- paste(apply(combn(seq_len(length(varnames)), m = 2L), 2, function(z) paste(varnames[z[1]], "~~", varnames[z[2L]])), collapse = " \n ")

        mod.cov.fit <- suppressWarnings(lavaan::cfa(mod.cov, data = y, ordered = FALSE, se = "none",
                                                    std.lv = TRUE, estimator = "ML", missing = "fiml"))

        if (isFALSE(std)) {

          var.total <- sum(lavaan::inspect(mod.cov.fit, "cov.ov")[varnames, varnames])

        } else {

          var.total <- sum(lavaan::inspect(mod.cov.fit, "cor.ov")[varnames, varnames])

        }

        omega <- load.sum2 / var.total

      }

      #----------------------------------------
      # Return object

      object <- list(mod.fit = mod.fit, omega = omega)

    #----------------------------------------
    # Omega for ordered-categorical items
    } else {

      object <- .catOmega(y, check = TRUE)

    }

    return(object)

  }

  ####################################################################################
  # Main Function

  omega.mod <- omega.function(y = x, y.resid.cov = resid.cov, y.type = type,
                              y.std = std, check = TRUE)

  omega.x <- data.frame(n = unlist(omega.mod$mod.fit@Data@nobs),
                        items = length(omega.mod$mod.fit@Data@ov$name),
                        omega = omega.mod$omega, stringsAsFactors = FALSE)

  #----------------------------------------
  # Confidence interval

  df1 <- omega.x$n - 1L
  df2 <- (omega.x$items - 1) * df1

  omega.low <- 1L - (1L - omega.x$omega) * qf(1L - (1L - conf.level) / 2L, df1, df2)
  omega.upp <- 1L - (1L - omega.x$omega) * qf((1L - conf.level) / 2L, df1, df2)

  omega.x <- data.frame(omega.x, low = omega.low, upp = omega.upp, stringsAsFactors = FALSE)

  #----------------------------------------
  # Standardized factor loading and omega if item deleted

  itemstat <- matrix(rep(NA, times = ncol(x)*2L), ncol = 2L,
                     dimnames = list(NULL, c("std.loa", "omega")))

  # Standardized factor loadings
  lambda.std <- lavaan::inspect(omega.mod$mod.fit, what = "std")$lambda

  for (i in seq_len(ncol(x))) {

    var <- colnames(x)[i]

    #----------------------------------------
    # Omega for continuous items
    if (type != "categ") {

      # Standardized factor loading
      itemstat[i, 1L] <- lambda.std[i]

      # Omega if item deleted
      if (ncol(x) > 3L) {

        # Residual covariance
        if (!is.null(resid.cov)) {

          resid.cov.i <- resid.cov[-which(vapply(resid.cov, function(y) any(y %in% var), FUN.VALUE = logical(1L)))]

          if (length(resid.cov.i) == 0L) { resid.cov.i <- NULL }

        } else {

          resid.cov.i <- NULL

        }

        itemstat[i, 2L] <- omega.function(y = x[, -grep(var, colnames(x))], y.resid.cov = resid.cov.i, y.type = type,
                                         y.std = std, check = FALSE)$omega

      } else {

        itemstat[i, 2L] <- NA

      }

    #----------------------------------------
    # Omega for ordered-categorical items
    } else {

      # Standardized factor loading
      itemstat[i, 1L] <- lambda.std[i]

      # Omega if item deleted
      if (ncol(x) > 3L) {

        itemstat[i, 2L] <- omega.function(y = x[, -grep(var, colnames(x))], y.resid.cov = NULL, y.type = "categ",
                                          y.std = std, check = FALSE)$omega

      } else {

        itemstat[i, 2L] <- NA

      }

    }

  }

  #........................................
  # Descriptive statistics

  itemstat <- data.frame(var = colnames(x),
                         misty::descript(x.raw, output = FALSE)$result[, c("n", "nNA", "pNA", "m", "sd", "min", "max")],
                         itemstat,
                         stringsAsFactors = FALSE)

  ####################################################################################
  # Return object

  #----------------------------------------
  # Return object

  object <- list(call = match.call(),
                 type = "omega",
                 data = x.raw,
                 args = list(resid.cov = resid.cov, type = type, exclude = exclude,
                             std = std, na.omit = na.omit, print = print,
                             digits = digits, conf.level = conf.level, as.na = as.na,
                             check = check, output = output),
                 mod.fit = omega.mod$mod.fit,
                 result = list(omega = omega.x, itemstat = itemstat))

  class(object) <- "coef"

  ####################################################################################
  # Output

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
