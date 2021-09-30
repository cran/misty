#' Polychoric Correlation Matrix
#'
#' This function computes a polychoric correlation matrix, which is the estimated
#' Pearson product-moment correlation matrix between underlying normally distributed
#' latent variables which generate the ordinal scores.
#'
#' @param x        a matrix or data frame of discrete values.
#' @param smooth   logical: if \code{TRUE} and if the polychoric matrix is not
#'                 positive definite, a simple smoothing algorithm using \code{cor.smooth()}
#'                 function is applied.
#' @param global   logical: if \code{TRUE}, the global values of the tau parameter
#'                 is used instead of the local values.
#' @param weight   a vector of length of the number of observations that specifies
#'                 the weights to apply to each case. The \code{NULL} case is
#'                 equivalent of weights of 1 for all cases.
#' @param correct  a numeric value indicating the correction value to use to
#'                 correct for continuity in the case of zero entry. Note that
#'                 unlike in the \code{polychoric()} function in the \pkg{psych}
#'                 the default value is 0.
#' @param progress logical: if \code{TRUE}, the progress bar is shown.
#' @param na.rm    logical: if \code{TRUE}, missing data are deleted.
#' @param delete   logical: if \code{TRUE}, cases with no variance are deleted
#'                 with a warning before proceeding.
#' @param tri      a character string indicating which triangular of the matrix
#'                 to show on the console, i.e., \code{both} for upper and lower
#'                 triangular, \code{lower} (default) for the lower triangular,
#'                 and \code{upper} for the upper triangular.
#' @param as.na    a numeric vector indicating user-defined missing values,
#'                 i.e. these values are converted to \code{NA} before conducting
#'                 the analysis.
#' @param digits   an integer value indicating the number of decimal places to
#'                 be used for displaying correlation coefficients.
#' @param check    logical: if \code{TRUE}, argument specification is checked.
#' @param output   logical: if \code{TRUE}, output is shown on the console.
#'
#' @author
#' William Revelle
#'
#' @references
#' Revelle, W. (2018) \emph{psych: Procedures for personality and psychological
#' research}. Northwestern University, Evanston, Illinois, USA,
#' https://CRAN.R-project.org/package=psych Version = 1.8.12.
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries: function call (\code{call}), type of analysis \code{type}, matrix or
#' data frame specified in \code{x} (\code{data}), specification of function arguments
#' (\code{args}), and list with results (\code{result}).
#'
#' @note
#' This function is based on the \code{polychoric()} function in the \pkg{psych}
#' package by William Revelle.
#'
#' @export
#'
#' @examples
#' dat <- data.frame(x1 = c(1, 1, 3, 2, 1, 2, 3, 2, 3, 1),
#'                   x2 = c(1, 2, 1, 1, 2, 2, 2, 1, 3, 1),
#'                   x3 = c(1, 3, 2, 3, 3, 1, 3, 2, 1, 2))
#'
#' # Polychoric correlation matrix
#' cor.poly(dat)
cor.poly <- function(x, smooth = TRUE, global = TRUE, weight = NULL, correct = 0, progress = TRUE,
                     na.rm = TRUE, delete = TRUE, tri = c("both", "lower", "upper"), digits = 2,
                     as.na = NULL, check = TRUE, output = TRUE) {

  ####################################################################################
  # Input Check

  #......
  # Package mnormt installed?
  if (isTRUE(!requireNamespace("mnormt", quietly = TRUE))) {

    stop("Package \"mnormt\" is needed for this function to work, please install it.",
         call. = FALSE )

  }

  #......
  # Check if input 'x' is missing
  if (isTRUE(missing(x))) {

    stop("Please specify a matrix or data frame of discrete values for the argument 'x'.", call. = FALSE)

  }

  #......
  # Check if input 'x' is NULL
  if (isTRUE(is.null(x))) {

    stop("Input specified for the argument 'x' is NULL.", call. = FALSE)

  }

  #......
  # Matrix or data frame for the argument 'x'?
  if (isTRUE(!is.matrix(x) && !is.data.frame(x))) {

    stop("Please specify a matrix or data frame of discrete values for the argument 'x'.",
         call. = FALSE)

  }

  #......
  # Check input 'check'
  if (isTRUE(!is.logical(check))) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }

  #-----------------------------------------

  if (isTRUE(check)) {

    #......
    # Check input 'x': Discrete values
    if (isTRUE(any(apply(x, 2, function(y) any(na.omit(y) %% 1L != 0L))))) {

      stop("Please specify a matrix or data frame of discrete values for the argument 'x'.", call. = FALSE)

    }

    #......
    # Check input 'smooth'
    if (isTRUE(!is.logical(smooth))) {

      stop("Please specify TRUE or FALSE for the argument 'smooth'.", call. = FALSE)

    }

    #......
    # Check input 'global'
    if (isTRUE(!is.logical(global))) {

      stop("Please specify TRUE or FALSE for the argument 'global'.", call. = FALSE)

    }

    #......
    # Check input 'na.rm'
    if (isTRUE(!is.logical(na.rm))) {

      stop("Please specify TRUE or FALSE for the argument 'na.rm'.", call. = FALSE)

    }

    #......
    # Check input 'progress'
    if (isTRUE(!is.logical(progress))) {

      stop("Please specify TRUE or FALSE for the argument 'progress'.", call. = FALSE)

    }

    # Check input 'delete'
    if (isTRUE(!is.logical(delete))) {

      stop("Please specify TRUE or FALSE for the argument 'delete'.", call. = FALSE)

    }

    #......
    # Check input 'tri'
    if (isTRUE(any(!tri %in% c("both", "lower", "upper")))) {

      warning("Character string in the argument 'tri' does not match with \"both\", \"lower\", or \"upper\".",
              call. = FALSE)

    }

    #......
    # Check input 'digits'
    if (isTRUE(digits %% 1L != 0L || digits < 0L)) {

      warning("Specify a positive integer number for the argument 'digits'.", call. = FALSE)

    }

    #......
    # Check input 'output'
    if (isTRUE(!is.logical(output))) {

      stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE)

    }

  }

  ####################################################################################
  # Data and Arguments

  #----------------------------------------
  # Convert user-missing values into NA

  if (isTRUE(!is.null(as.na))) {

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

    if (isTRUE(class(eigens) == as.character("try-error"))) {

      warning("There is something wrong with the correlation matrix, i.e., cor.smooth() failed to smooth it because some of the eigenvalues are NA.",
              call. = FALSE)

    } else {

      if (isTRUE(min(eigens$values) < .Machine$double.eps)) {

        warning("Matrix was not positive definite, smoothing was done.", call. = FALSE)

        eigens$values[eigens$values < eig.tol] <- 100L * eig.tol
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

    if (isTRUE(cores < 1L)) {

      stop("'mc.cores' must be >= 1", call. = FALSE)

    }

    if (isTRUE(cores > 1L)) {

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
    maxxy <- (maxx+(minx == 0L))*(maxy + (miny == 0L))
    dims <- c(maxx + 1L - min(1, minx), maxy + 1L - min(1L, minx))
    bin <- x - minx + (y - miny)*(dims[1]) + max(1L, minx)
    ans <- matrix(tabulate(bin, maxxy), dims)

    return(ans)
  }

  #----------------------------------------
  tableFast <- function(x, y, minx, maxx, miny, maxy) {

    maxxy <- (maxx + (minx == 0L))*(maxy + (minx == 0L))
    bin <- x-minx + (y - minx) *maxx + 1
    dims <- c(maxx + 1L - min(1L, minx), maxy + 1L - min(1L, miny))
    ans <- matrix(tabulate(bin, maxxy), dims)

    return(ans)

  }

  #----------------------------------------
  polyBinBvn <- function(rho, rc, cc) {

    row.cuts <- c(-Inf, rc,Inf)
    col.cuts <- c(-Inf, cc, Inf)
    nr <- length(row.cuts) - 1L
    nc <- length(col.cuts) - 1L

    P <- matrix(0L, nr, nc)
    R <- matrix(c(1L, rho, rho,1), 2L, 2L)

    for (i in seq_len((nr - 1L))) {

      for (j in seq_len((nc - 1L))) {

        P[i, j] <- mnormt::sadmvn(lower = c(row.cuts[i], col.cuts[j]),
                                  upper = c(row.cuts[i + 1L], col.cuts[j + 1L]), mean = rep(0L, 2L),
                                  varcov = R)
      }

    }

    P[1L, nc] <- pnorm(rc[1L]) - sum(P[1L, seq_len((nc - 1L))])
    P[nr, 1L] <- pnorm(cc[1L]) - sum(P[seq_len((nr - 1L)), 1L])
    if (isTRUE(nr >2L)) { for (i in (2L:(nr - 1L))) {P[i, nc] <- pnorm(rc[i]) -pnorm(rc[i - 1L])- sum(P[i, seq_len((nc - 1L))]) }}
    if (isTRUE(nc >2L)) { for (j in (2L:(nc - 1L))) {P[nr, j] <- pnorm(cc[j]) - pnorm(cc[j - 1L])-sum(P[seq_len((nr - 1L)), j]) }}
    if (isTRUE(nc > 1L))  P[nr, nc] <- 1L - pnorm(rc[nr - 1L]) - sum(P[nr, seq_len((nc - 1L))])
    P

  }

  #----------------------------------------
  polyF <- function(rho,rc,cc,tab) {

    P <- polyBinBvn(rho, rc, cc)
    P[P <= 0L] <- NA
    lP <- log(P)
    lP[lP == -Inf] <- NA
    lP[lP == Inf] <- NA
    -sum(tab * lP,na.rm=TRUE)  }

  #----------------------------------------
  wtd.table <- function(x, y, weight) {

    tab <- tapply(weight, list(x, y), sum, na.rm = TRUE, simplify = TRUE)
    tab[is.na(tab)] <- 0L

    return(tab)

  }


  #----------------------------------------
  polyc <- function(x, y = NULL, taux, tauy, global = TRUE, weight = NULL, correct = correct,
                    gminx, gmaxx, gminy, gmaxy) {

    if (is.null(weight)) {

      tab <- tableFast(x, y, gminx, gmaxx, gminy, gmaxy)

    }  else {

      tab <- wtd.table(x,y,weight)

    }

    fixed <- 0L
    tot <- sum(tab)
    if (isTRUE(tot == 0L)) {

      result <- list(rho = NA, objective = NA, fixed = 1L)

      return(result)

    }

    tab <- tab/tot

    if (isTRUE(correct > 0L)) {

      if (isTRUE(any(tab[] == 0L))) {

        fixed <- 1L
        tab[tab == 0L] <- correct/tot

      }

    }

    if (isTRUE(global)) {

      rho <- optimize(polyF, interval = c(-1, 1), rc = taux, cc = tauy, tab)

    } else {

      if (isTRUE(!is.na(sum(tab))))  {

        zerorows <- apply(tab, 1, function(x) all(x == 0))
        zerocols <- apply(tab, 2, function(x) all(x == 0))
        zr <- sum(zerorows)
        zc <- sum(zerocols)
        tab <- tab[!zerorows, , drop = FALSE]
        tab <- tab[, !zerocols, drop = FALSE]
        csum <- colSums(tab)
        rsum <- rowSums(tab)

        if (isTRUE(min(dim(tab)) < 2L)) {

          rho <- list(objective = NA)

        } else {

          cc <-  qnorm(cumsum(csum)[-length(csum)])
          rc <-  qnorm(cumsum(rsum)[-length(rsum)])
          rho <- optimize(polyF, interval = c(-1L, 1L), rc = rc, cc = cc, tab)

        }

      } else {

        rho <- list(objective = NA, rho= NA)

      }

    }

    if (isTRUE(is.na(rho$objective))) {

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

    if (isTRUE(!is.null(weight))) {

      if (isTRUE(length(weight) != nrow(x))) {

        stop("Length of the weight vector must match the number of cases.", call. = FALSE)

      }
    }

    cl <- match.call()
    np <- dim(p)[2L]
    nd <- dim(d)[2L]

    if (isTRUE(is.null(np))) np <- 1L
    if (isTRUE(is.null(nd))) nd <- 1L

    nsub <- dim(p)[1]
    p <- as.matrix(p)
    d <- as.matrix(d)
    nvalues <- max(p, na.rm = TRUE) - min(p, na.rm = TRUE) + 1L
    dmin <- apply(d, 2,function(x) min(x, na.rm = TRUE))
    dmax <- apply(d, 2,function(x) max(x, na.rm = TRUE))
    dvalues <- max(dmax - dmin)

    if (isTRUE(dvalues != 1L)) stop("You did not supply a dichotomous variable.", call. = FALSE)

    if (isTRUE(nvalues > 8L)) stop("You have more than 8 categories for your items, polychoric is probably not needed.", call. = FALSE)

    item.var <- apply(p, 2, sd, na.rm = na.rm)
    bad <- which((item.var <= 0) | is.na(item.var))

    if (isTRUE(length(bad) > 0L && delete)) {

      for (baddy in seq_len(length(bad))) {

        message( "Item = ",colnames(p)[bad][baddy], " had no variance and was deleted")

      }

      p <- p[, -bad]
      np <- np - length(bad)

    }

    pmin <- apply(p, 2, function(x) min(x, na.rm = TRUE))
    minx <- min(pmin)
    p <- t(t(p) - pmin + 1L)

    miny <- min(dmin)
    d <-  t(t(d) - dmin + 1L)
    gminx <- gminy <- 1L

    pmax <- apply(p,2,function(x)  max(x,na.rm = TRUE))
    gmaxx <- max(pmax)

    if (isTRUE(min(pmax) != max(pmax))) { global <- FALSE
    warning("The items do not have an equal number of response alternatives, setting global to FALSE.", call. = FALSE)}

    gmaxy <- max(apply(d, 2, function(x) max(x, na.rm = TRUE)))
    pfreq <- apply(p, 2, tabulate, nbins = nvalues)
    n.obs <- colSums(pfreq)
    pfreq <- t(t(pfreq)/n.obs)

    taup <- as.matrix(qnorm(apply(pfreq, 2, cumsum))[seq_len(nvalues - 1L), ], ncol = ncol(pfreq))

    rownames(taup) <- paste(seq_len(nvalues - 1L))
    colnames(taup) <- colnames(p)

    dfreq <- apply(d, 2, tabulate, nbins = 2L)
    if (isTRUE(nd < 2L)) {

      n.obsd <- sum(dfreq)

    } else {

      n.obsd <- colSums(dfreq)

    }

    dfreq <- t(t(dfreq)/n.obsd)
    taud <-  qnorm(apply(dfreq, 2, cumsum))

    mat <- matrix(0L, np, nd)
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
    if (isTRUE(correct > 0L)) tab[tab == 0L] <- correct/tot

    csum <- colSums(tab)
    rsum <- rowSums(tab)
    cc <-  qnorm(cumsum(csum[-length(csum)]))
    rc <-  qnorm(cumsum(rsum[-length(rsum)]))
    rho <- optimize(polyF, interval = c(-1L, 1L), rc = rc, cc = cc, tab)

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
    for(i in 2L:nvar) {

      for (j in seq_len(i - 1L)) {

      il[k] <- i
      jl[k] <- j
      k <- k + 1

      }

    }

    poly <- mcmapply(function(i, j) myfun(x, i, j, gminx = gminx, gmaxx = gmaxx, gminy = gminy, gmaxy = gmaxy), il, jl)

    mat <- diag(nvar)
    if (isTRUE(length(dim(poly)) == 2L)) {

      mat[upper.tri(mat)] <- as.numeric(poly[1L, ])
      mat <- t(mat) + mat
      fixed <- as.numeric(poly[3L, ])
      diag(mat) <- 1L
      fixed <- sum(fixed)

      if (isTRUE(fixed > 0L && correct > 0L)) {

        warning(fixed ," cell(s) adjusted for 0 values using the correction for continuity.", call. = FALSE)

      }

      return(mat)

    } else {

      warning("Something is wrong in polycor.", call. = FALSE)

      return(poly)

        stop("Something was seriously wrong. Please look at the results.", call. = FALSE)

    }

  }

  if (isTRUE(!is.null(weight))) {

    if (isTRUE(length(weight) !=nrow(x))) {

      stop("Length of the weight vector must match the number of cases", call. = FALSE)

    }

  }

  nvar <- dim(x)[2L]
  nsub <- dim(x)[1L]
  if (isTRUE((prod(dim(x)) == 4L) || is.table(x))) {

    result <- polytab(x, correct = correct)

    } else {

      x <- as.matrix(x)
      if (isTRUE(!is.numeric(x))) {

        x <- matrix(as.numeric(x), ncol = nvar)
        message("Non-numeric input converted to numeric.")

      }

      xt <- table(x)
      nvalues <- length(xt)
      maxx <- max(x, na.rm = TRUE)

      if (isTRUE(maxx > nvalues)) {

        xtvalues <- as.numeric(names(xt))
        for(i in seq_len(nvalues)) {

          x[x == xtvalues[i]] <- i

        }

      }

    nvalues <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE) + 1L

    item.var <- apply(x, 2, sd, na.rm = na.rm)
    bad <- which((item.var <= 0)|is.na(item.var))

    if (isTRUE(length(bad) > 0L && delete)) {

      for (baddy in seq_len(length(bad))) {

        message("Item = ", colnames(x)[bad][baddy], " had no variance and was deleted.")

      }

      x <- x[, -bad]
      nvar <- nvar - length(bad)

    }

    xmin <- apply(x, 2, function(x) min(x, na.rm = TRUE))

    xmin <- min(xmin)
    x <- t(t(x) - xmin + 1L)

    gminx <- gminy <- 1L
    xmax <- apply(x, 2, function(x) max(x, na.rm = TRUE))
    xmax <- max(xmax)
    gmaxx <- gmaxy <- xmax

    if (isTRUE(min(xmax) != max(xmax))) {

      global <- FALSE
      warning("Items do not have an equal number of response categories, global set to FALSE.", call. = FALSE)

    }

    xfreq <- apply(x, 2, tabulate, nbins = nvalues)
    n.obs <- colSums(xfreq)
    xfreq <- t(t(xfreq) / n.obs)
    tau <- qnorm(apply(xfreq, 2, cumsum))[seq_len(nvalues - 1L), ]

    if (isTRUE(!is.matrix(tau))) tau <- matrix(tau, ncol = nvar)

    rownames(tau) <- seq_len(nvalues - 1L)
    colnames(tau) <- colnames(x)

    mat <- matrix(0L, nvar, nvar)
    colnames(mat) <- rownames(mat) <- colnames(x)

    mat <- matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy)

    if (isTRUE(any(is.na(mat)))) {

     message("Some correlations are missing, smoothing turned off.")
     smooth <- FALSE

    }

    if (isTRUE(smooth)) {

      mat <- cor.smooth(mat)

    }

    colnames(mat) <- rownames(mat) <- colnames(x)


    object <- list(call = match.call(),
                   type = "cor.poly",
                   data = x,
                   args = list(smooth = smooth, global = global, weight = weight, correct = correct,
                               progress = progress, na.rm = na.rm, delete = delete, digits = digits,
                               tri = tri, as.na = as.na, check = check, output = output),
                   result = list(cor = mat, tau = tau))

  }

  class(object) <- "misty.object"

  ####################################################################################
  # Output

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
