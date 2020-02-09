#' Polychoric Correlation Matrix
#'
#' This function computes a polychoric correlation matrix, which is the estimated Pearson product-moment correlation matrix
#' between underlying normally distributed latent variables which generate the ordinal scores.
#'
#' Note that this function is based on the \code{polychoric()} function in the \pkg{psych} by William Revelle.
#'
#' @param x           a matrix or data frame of discrete values.
#' @param smooth      logical: if \code{TRUE} and if the polychoric matrix is not positive definite,
#'                    a simple smoothing algorithm using \code{cor.smooth()} function is applied.
#' @param global      logical: if \code{TRUE}, the global values of the tau parameter is used instead of the local values.
#' @param weight      a vector of length of the number of observations that specifies the weights to apply to each case.
#'                    The \code{NULL} case is equivalent of weights of 1 for all cases.
#' @param correct     a numeric value indicating the correction value to use to correct for continuity in the case
#'                    of zero entry. Note that unlike in the \code{polychoric()} function in the \pkg{psych} the default
#'                    value is 0.
#' @param progress    logical: if \code{TRUE}, the progress bar is shown.
#' @param na.rm       logical: if \code{TRUE}, missing data are deleted.
#' @param delete      logical: if \code{TRUE}, cases with no variance are deleted with a warning before proceeding.
#' @param tri         a character string indicating which triangular of the matrix to show on the console, i.e.,
#'                    \code{both} for upper and lower triangular, \code{lower} (default) for the lower triangular,
#'                    and \code{upper} for the upper triangular.
#' @param as.na       a numeric vector indicating user-defined missing values,
#'                    i.e. these values are converted to \code{NA} before conducting the analysis.
#' @param digits      an integer value indicating the number of decimal places to be used for
#'                    displaying correlation coefficients.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#' @param output      logical: if \code{TRUE}, output is shown on the console.
#'
#' @author
#' William Revelle
#'
#' @references
#' Revelle, W. (2018) \emph{psych: Procedures for personality and psychological research}. Northwestern University, Evanston,
#' Illinois, USA, https://CRAN.R-project.org/package=psych Version = 1.8.12.
#'
#' @return
#' Returns an object of class \code{poly.cor}, which is a list with following entries: function call (\code{call}),
#' matrix or data frame specified in \code{x} (\code{data}), specification of function arguments (\code{args}), and
#' list with results (\code{result}).
#'
#' @export
#'
#' @examples
#' dat <- data.frame(x1 = c(1, 1, 3, 2, 1, 2, 3, 2, 3, 1),
#'                   x2 = c(1, 2, 1, 1, 2, 2, 2, 1, 3, 1),
#'                   x3 = c(1, 3, 2, 3, 3, 1, 3, 2, 1, 2))
#'
#' # Polychoric correlation matrix
#' poly.cor(dat)
poly.cor <- function(x, smooth = TRUE, global = TRUE, weight = NULL, correct = 0, progress = TRUE,
                     na.rm = TRUE, delete = TRUE, tri = c("both", "lower", "upper"), digits = 2,
                     as.na = NULL, check = TRUE, output = TRUE) {

  ####################################################################################
  # Input Check

  #......
  # Package mnormt installed?
  if (!requireNamespace("mnormt", quietly = TRUE)) {

    stop("Package \"mnormt\" is needed for this function to work, please install it.",
         call. = FALSE )

  }

  #......
  # Check if input 'x' is missing
  if (missing(x)) {

    stop("Please specify a matrix or data frame of discrete values for the argument 'x'.", call. = FALSE)

  }

  #......
  # Matrix or data frame for the argument 'x'?
  if (!is.matrix(x) && !is.data.frame(x)) {

    stop("Please specify a matrix or data frame of discrete values for the argument 'x'.",
         call. = FALSE)

  }

  #......
  # Check input 'check'
  if (isFALSE(isTRUE(check) || isFALSE(check))) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }

  #-----------------------------------------

  if (isTRUE(check)) {

    #......
    # Check input 'x': Discrete values
    if (any(apply(x, 2, function(y) any(na.omit(y) %% 1 != 0)))) {

      stop("Please specify a matrix or data frame of discrete values for the argument 'x'.", call. = FALSE)

    }

    #......
    # Check input 'smooth'
    if (isFALSE(isTRUE(smooth) | isFALSE(smooth))) {

      stop("Please specify TRUE or FALSE for the argument 'smooth'.", call. = FALSE)

    }

    #......
    # Check input 'global'
    if (isFALSE(isTRUE(global) | isFALSE(global))) {

      stop("Please specify TRUE or FALSE for the argument 'global'.", call. = FALSE)

    }

    #......
    # Check input 'na.rm'
    if (isFALSE(isTRUE(na.rm) | isFALSE(na.rm))) {

      stop("Please specify TRUE or FALSE for the argument 'na.rm'.", call. = FALSE)

    }

    #......
    # Check input 'progress'
    if (isFALSE(isTRUE(progress) | isFALSE(progress))) {

      stop("Please specify TRUE or FALSE for the argument 'progress'.", call. = FALSE)

    }

    # Check input 'delete'
    if (isFALSE(isTRUE(delete) | isFALSE(delete))) {

      stop("Please specify TRUE or FALSE for the argument 'delete'.", call. = FALSE)

    }

    #......
    # Check input 'tri'
    if (any(!tri %in% c("both", "lower", "upper"))) {

      warning("Character string in the argument 'tri' does not match with \"both\", \"lower\", or \"upper\".",
              call. = FALSE)

    }

    #......
    # Check input 'digits'
    if (digits %% 1 != 0 || digits < 0) {

      warning("Specify a positive integer number for the argument 'digits'.", call. = FALSE)

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
  # Convert user-missing values into NA

  if (!is.null(as.na)) {

    x <- misty::as.na(x, as.na = as.na, check = check)

  }

  #----------------------------------------
  # Print triangular

  tri <- ifelse(all(c("both", "lower", "upper") %in% tri), "lower", tri)

  ####################################################################################
  # Functions

  #----------------------------------------
  cor.smooth <- function (x, eig.tol = 10^-12) {

    eigens <- try(eigen(x), TRUE)

    if (class(eigens) == as.character("try-error")) {

      warning("There is something wrong with the correlation matrix, i.e., cor.smooth() failed to smooth it because some of the eigenvalues are NA.",
              call. = FALSE)

    } else {

      if (min(eigens$values) < .Machine$double.eps) {

        warning("Matrix was not positive definite, smoothing was done.", call. = FALSE)

        eigens$values[eigens$values < eig.tol] <- 100 * eig.tol
        nvar <- dim(x)[1]
        tot <- sum(eigens$values)
        eigens$values <- eigens$values * nvar/tot
        cnames <- colnames(x)
        rnames <- rownames(x)
        x <- eigens$vectors %*% diag(eigens$values) %*% t(eigens$vectors)
        x <- cov2cor(x)
        colnames(x) <- cnames
        rownames(x) <- rnames

      }

    }

    return(x)

  }

  #----------------------------------------
  mcmapply <- function (FUN, ..., MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE,
                        mc.preschedule = TRUE, mc.set.seed = TRUE, mc.silent = FALSE,
                        mc.cores = 1L, mc.cleanup = TRUE, affinity.list = NULL) {

    cores <- as.integer(mc.cores)

    if (cores < 1L) {
      stop("'mc.cores' must be >= 1", call. = FALSE)

    }

    if (cores > 1L) {

      stop("'mc.cores' > 1 is not supported on Windows", call. = FALSE)

    }

    mapply(FUN = FUN, ..., MoreArgs = MoreArgs, SIMPLIFY = SIMPLIFY,
           USE.NAMES = USE.NAMES)

  }

  #----------------------------------------
  tableF <- function(x,y) {

    minx <- min(x,na.rm = TRUE)
    maxx <- max(x,na.rm = TRUE)
    miny <- min(y,na.rm = TRUE)
    maxy <- max(y,na.rm = TRUE)
    maxxy <- (maxx+(minx == 0))*(maxy + (miny == 0))
    dims <- c(maxx + 1 - min(1, minx), maxy + 1 - min(1, minx))
    bin <- x - minx + (y - miny)*(dims[1]) + max(1, minx)
    ans <- matrix(tabulate(bin, maxxy), dims)

    return(ans)
  }

  #----------------------------------------
  tableFast <- function(x, y, minx, maxx, miny, maxy) {

    maxxy <- (maxx + (minx == 0))*(maxy + (minx == 0))
    bin <- x-minx + (y - minx) *maxx + 1
    dims <- c(maxx + 1 - min(1, minx), maxy + 1 - min(1, miny))
    ans <- matrix(tabulate(bin, maxxy), dims)

    return(ans)

  }

  #----------------------------------------
  polyBinBvn <- function(rho, rc, cc) {

    row.cuts <- c(-Inf, rc,Inf)
    col.cuts <- c(-Inf, cc, Inf)
    nr <- length(row.cuts) -1
    nc <- length(col.cuts) -1

    P <- matrix(0, nr, nc)
    R <- matrix(c(1, rho, rho,1), 2, 2)

    for (i in seq_len((nr - 1))) {

      for (j in seq_len((nc - 1))) {

        P[i, j] <- mnormt::sadmvn(lower = c(row.cuts[i], col.cuts[j]),
                                  upper = c(row.cuts[i + 1], col.cuts[j + 1]), mean = rep(0, 2),
                                  varcov = R)
      }

    }

    P[1,nc] <- pnorm(rc[1]) - sum(P[1, seq_len((nc - 1))])
    P[nr,1] <- pnorm(cc[1]) - sum(P[seq_len((nr - 1)), 1])
    if(nr >2) {for (i in (2:(nr - 1))) {P[i, nc] <- pnorm(rc[i]) -pnorm(rc[i - 1])- sum(P[i, seq_len((nc - 1))]) }}
    if(nc >2) {for (j in (2:(nc - 1))) {P[nr, j] <- pnorm(cc[j]) - pnorm(cc[j - 1])-sum(P[seq_len((nr - 1)), j]) }}
    if(nc > 1)  P[nr, nc] <- 1 - pnorm(rc[nr - 1]) - sum(P[nr, seq_len((nc - 1))])
    P

  }

  #----------------------------------------
  polyF <- function(rho,rc,cc,tab) {

    P <- polyBinBvn(rho, rc, cc)
    P[P <=0] <- NA
    lP <- log(P)
    lP[lP == -Inf] <- NA
    lP[lP == Inf] <- NA
    -sum(tab * lP,na.rm=TRUE)  }

  #----------------------------------------
  wtd.table <- function(x, y, weight) {

    tab <- tapply(weight, list(x, y), sum, na.rm = TRUE, simplify = TRUE)
    tab[is.na(tab)] <- 0

    return(tab)

  }


  #----------------------------------------
  polyc <- function(x, y = NULL, taux, tauy, global = TRUE, weight = NULL, correct = correct,
                    gminx, gmaxx, gminy, gmaxy) {

    if(is.null(weight)) {

      tab <- tableFast(x, y, gminx, gmaxx, gminy, gmaxy)

    }  else {

      tab <- wtd.table(x,y,weight)

    }

    fixed <- 0
    tot <- sum(tab)
    if(tot == 0) {

      result <- list(rho = NA, objective = NA, fixed = 1)

      return(result)

    }

    tab <- tab/tot

    if(correct > 0) {

      if(any(tab[] == 0)) {

        fixed <- 1
        tab[tab == 0] <- correct/tot

      }

    }

    if(isTRUE(global)) {

      rho <- optimize(polyF, interval = c(-1, 1), rc = taux, cc = tauy, tab)

    } else {

      if(!is.na(sum(tab)))  {

        zerorows <- apply(tab, 1, function(x) all(x == 0))
        zerocols <- apply(tab, 2, function(x) all(x == 0))
        zr <- sum(zerorows)
        zc <- sum(zerocols)
        tab <- tab[!zerorows, ,drop = FALSE]
        tab <- tab[, !zerocols, drop = FALSE]
        csum <- colSums(tab)
        rsum <- rowSums(tab)

        if(min(dim(tab)) < 2) {

          rho <- list(objective = NA)

        } else {

          cc <-  qnorm(cumsum(csum)[-length(csum)])
          rc <-  qnorm(cumsum(rsum)[-length(rsum)])
          rho <- optimize(polyF, interval = c(-1, 1), rc = rc, cc = cc, tab)

        }

      } else {

        rho <- list(objective = NA, rho= NA)

      }

    }

    if(is.na(rho$objective)) {

      result <- list(rho = NA, objective = NA, fixed = fixed)

    } else {

      result <- list(rho=rho$minimum, objective=rho$objective, fixed=fixed)

    }

    return(result)

  }

  #----------------------------------------
  polydi <- function(p, d, taup, taud, global = TRUE, ML = FALSE, std.err = FALSE, weight = NULL,
                     progress = TRUE, na.rm = TRUE, delete = TRUE, correct = 0.5) {

    myfun <- function(x, i, j, correct, taup, taud, gminx, gmaxx, gminy, gmaxy, np) {

      polyc(x[, i], x[, j], taup[, i], taud[1, (j - np)], global = global, weight = weight,
            correct = correct, gminx = gminx, gmaxx = gmaxx, gminy = gminy, gmaxy = gmaxy)

    }

    matpLower <- function(x, np, nd, taup, taud, gminx, gmaxx, gminy, gmaxy) {

      k <- 1
      il <- vector()
      jl <- vector()
      for(i in seq_len(np)) {

        for (j in seq_len(nd)) {

          il[k] <- i
          jl [k] <- j
          k <- k+1

        }

      }

      poly <- mcmapply(function(i, j) myfun(x, i, j, correct = correct,
                                            taup = taup, taud = taud, gminx = gminx, gmaxx = gmaxx, gminy = gminy, gmaxy = gmaxy, np = np), il, jl + np)

      mat <- matrix(np,nd)
      mat <- as.numeric(poly[1, ])

      return(mat)

    }

    if(!is.null(weight)) {

      if(length(weight) != nrow(x)) {

        stop("Length of the weight vector must match the number of cases.", call. = FALSE)

      }
    }

    cl <- match.call()
    np <- dim(p)[2]
    nd <- dim(d)[2]

    if(is.null(np)) np <- 1
    if(is.null(nd)) nd <- 1

    nsub <- dim(p)[1]
    p <- as.matrix(p)
    d <- as.matrix(d)
    nvalues <- max(p, na.rm = TRUE) - min(p, na.rm = TRUE) + 1
    dmin <- apply(d, 2,function(x) min(x, na.rm = TRUE))
    dmax <- apply(d, 2,function(x) max(x, na.rm = TRUE))
    dvalues <- max(dmax - dmin)

    if (dvalues != 1) stop("You did not supply a dichotomous variable.", call. = FALSE)

    if (nvalues > 8) stop("You have more than 8 categories for your items, polychoric is probably not needed.", call. = FALSE)

    item.var <- apply(p, 2, sd, na.rm = na.rm)
    bad <- which((item.var <= 0) | is.na(item.var))

    if ((length(bad) > 0)  & isTRUE(delete)) {

      for (baddy in seq_len(length(bad))) {

        message( "Item = ",colnames(p)[bad][baddy], " had no variance and was deleted")

      }

      p <- p[, -bad]
      np <- np - length(bad)

    }

    pmin <- apply(p, 2, function(x) min(x, na.rm = TRUE))
    minx <- min(pmin)
    p <- t(t(p) - pmin + 1)

    miny <- min(dmin)
    d <-  t(t(d) - dmin + 1)
    gminx <- gminy <- 1

    pmax <- apply(p,2,function(x)  max(x,na.rm = TRUE))
    gmaxx <- max(pmax)

    if (min(pmax) != max(pmax)) { global <- FALSE
    warning("The items do not have an equal number of response alternatives, setting global to FALSE.", call. = FALSE)}

    gmaxy <- max(apply(d, 2, function(x) max(x, na.rm = TRUE)))
    pfreq <- apply(p, 2, tabulate, nbins = nvalues)
    n.obs <- colSums(pfreq)
    pfreq <- t(t(pfreq)/n.obs)

    taup <- as.matrix(qnorm(apply(pfreq, 2, cumsum))[seq_len(nvalues - 1), ], ncol = ncol(pfreq))

    rownames(taup) <- paste(seq_len(nvalues - 1))
    colnames(taup) <- colnames(p)

    dfreq <- apply(d, 2, tabulate, nbins = 2)
    if (nd < 2) {

      n.obsd <- sum(dfreq)

    } else {

      n.obsd <- colSums(dfreq)

    }

    dfreq <- t(t(dfreq)/n.obsd)
    taud <-  qnorm(apply(dfreq, 2, cumsum))

    mat <- matrix(0, np, nd)
    rownames(mat) <- colnames(p)
    colnames(mat) <- colnames(d)

    x <- cbind(p,d)

    mat <- matpLower(x, np, nd, taup, taud, gminx, gmaxx, gminy, gmaxy)

    mat <- matrix(mat, np, nd, byrow = TRUE)
    rownames(mat) <- colnames(p)
    colnames(mat)  <- colnames(d)

    taud <- t(taud)
    result <- list(rho = mat,tau = taud, n.obs = nsub)

    class(result) <- c("psych","polydi")

    return(result)

  }

  #----------------------------------------
  polytab <- function(tab, correct = TRUE) {

    tot <- sum(tab)
    tab <- tab/tot
    if(correct > 0) tab[tab == 0] <- correct/tot

    csum <- colSums(tab)
    rsum <- rowSums(tab)
    cc <-  qnorm(cumsum(csum[-length(csum)]))
    rc <-  qnorm(cumsum(rsum[-length(rsum)]))
    rho <- optimize(polyF, interval = c(-1, 1), rc = rc, cc = cc, tab)

    result <- list(rho = rho$minimum, objective = rho$objective, tau.row = rc, tau.col = cc)

    return(result)

  }

  #----------------------------------------
  myfun <- function(x, i, j, gminx, gmaxx, gminy, gmaxy) {

    polyc(x[, i], x[, j], tau[, i], tau[, j], global = global, weight = weight, correct = correct,
          gminx = gminx, gmaxx = gmaxx, gminy = gminy, gmaxy = gmaxy)

  }

  #----------------------------------------
  matpLower <- function(x, nvar, gminx, gmaxx, gminy, gmaxy) {

    k <- 1
    il <- vector()
    jl <- vector()
    for(i in 2:nvar) {

      for (j in seq_len(i - 1)) {

      il[k] <- i
      jl [k] <- j
      k <- k + 1

      }

    }

    poly <- mcmapply(function(i, j) myfun(x, i, j, gminx = gminx, gmaxx = gmaxx, gminy = gminy, gmaxy = gmaxy), il, jl)

    mat <- diag(nvar)
    if(length(dim(poly)) == 2) {

      mat[upper.tri(mat)] <- as.numeric(poly[1, ])
      mat <- t(mat) + mat
      fixed <- as.numeric(poly[3, ])
      diag(mat) <- 1
      fixed <- sum(fixed)

      if((fixed > 0) && ( correct > 0)) {

        warning(fixed ," cell(s) adjusted for 0 values using the correction for continuity.", call. = FALSE)

      }

      return(mat)

    } else {

      warning("Something is wrong in polycor.", call. = FALSE)

      return(poly)

        stop("Something was seriously wrong. Please look at the results.", call. = FALSE)

    }

  }

  if(!is.null(weight)) {

    if(length(weight) !=nrow(x)) {

      stop("Length of the weight vector must match the number of cases", call. = FALSE)

    }

  }

  nvar <- dim(x)[2]
  nsub <- dim(x)[1]
  if((prod(dim(x)) == 4) | is.table(x)) {

    result <- polytab(x, correct = correct)

    } else {

      x <- as.matrix(x)
      if(!is.numeric(x)) {

        x <- matrix(as.numeric(x), ncol = nvar)
        message("Non-numeric input converted to numeric.")

      }

      xt <- table(x)
      nvalues <- length(xt)
      maxx <- max(x, na.rm = TRUE)

      if (maxx > nvalues) {

        xtvalues <- as.numeric(names(xt))
        for(i in seq_len(nvalues)) {

          x[x == xtvalues[i]] <- i

        }

      }

    nvalues <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE) + 1

    item.var <- apply(x, 2, sd, na.rm = na.rm)
    bad <- which((item.var <= 0)|is.na(item.var))

    if((length(bad) > 0) & isTRUE(delete)) {

      for (baddy in seq_len(length(bad))) {

        message("Item = ", colnames(x)[bad][baddy], " had no variance and was deleted.")

      }

      x <- x[, -bad]
      nvar <- nvar - length(bad)

    }

    xmin <- apply(x, 2, function(x) min(x, na.rm = TRUE))

    xmin <- min(xmin)
    x <- t(t(x) - xmin + 1)

    gminx <- gminy <- 1
    xmax <- apply(x, 2, function(x) max(x, na.rm = TRUE))
    xmax <- max(xmax)
    gmaxx <- gmaxy <- xmax

    if (min(xmax) != max(xmax)) {

      global <- FALSE
      warning("Items do not have an equal number of response categories, global set to FALSE.", call. = FALSE)

    }

    xfreq <- apply(x, 2, tabulate, nbins = nvalues)
    n.obs <- colSums(xfreq)
    xfreq <- t(t(xfreq) / n.obs)
    tau <- qnorm(apply(xfreq, 2, cumsum))[seq_len(nvalues - 1), ]

    if(!is.matrix(tau)) tau <- matrix(tau, ncol = nvar)

    rownames(tau) <- seq_len(nvalues - 1)
    colnames(tau) <- colnames(x)

    mat <- matrix(0, nvar, nvar)
    colnames(mat) <- rownames(mat) <- colnames(x)

    mat <- matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy)

    if (any(is.na(mat))) {

     message("Some correlations are missing, smoothing turned off.")
     smooth <- FALSE

    }

    if (smooth) {

      mat <- cor.smooth(mat)

    }

    colnames(mat) <- rownames(mat) <- colnames(x)


    object <- list(call = match.call(),
                   data = x,
                   args = list(smooth = smooth, global = global, weight = weight, correct = correct,
                               progress = progress, na.rm = na.rm, delete = delete, digits = digits,
                               tri = tri, as.na = as.na, check = check, output = output),
                   result = list(cor = mat, tau = tau))

  }

  class(object) <- "poly.cor"

  ####################################################################################
  # Output

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
