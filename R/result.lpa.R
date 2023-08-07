#' Summary Result Table for Latent Profile Analysis Estimated in Mplus
#'
#' This function reads all Mplus output files from latent profile analysis in the
#' subfolders to create a summary result table. By default, the function reads
#' output files in all subfolders of the current working directory. Note that all
#' output files need to be based on \code{ANALYSIS: TYPE IS MIXTURE} and each
#' subfolder needs to contain more than one \code{TYPE IS MIXTURE} output file.
#'
#' @param folder       a character string indicating the path of the folder
#'                     containing subfolders with the Mplus output files. By
#'                     default Mplus outputs in the subfolders of the current
#'                     working directory are read.
#' @param sort.n       logical: if \code{TRUE} (default), result table is sorted
#'                     according to the number of profiles within each folder.
#' @param sort.p       logical: if \code{TRUE} (default), class proportions are
#'                     sorted decreasing.
#' @param digits       an integer value indicating the number of decimal places
#'                     to be used for displaying results. Note that the scaling
#'                     correction factor is displayed  with \code{digits} plus 1
#'                     decimal places.
#' @param p.digits     an integer value indicating the number of decimal places
#'                     to be used for displaying \emph{p}-values, entropy value,
#'                     and class proportions.
#' @param write        a character string for writing the results into a Excel
#'                     file naming a file with or without file extension '.xlsx',
#'                     e.g., \code{"Results.xlsx"} or \code{"Results"}.
#' @param check        logical: if \code{TRUE}, argument specification is checked.
#' @param output       logical: if \code{TRUE}, output is shown.
#'
#' @details
#' The result summary table comprises following entries:
#' \itemize{
#'    \item{\code{"Folder"}}: Subfolder from which the group of Mplus outputs files
#'                            were summarized.
#'    \item{\code{"#Prof"}}: Number of profiles (i.e., \code{CLASSES ARE c(#Prof)}).
#'    \item{\code{"Conv"}}: Model converged, \code{TRUE} or \code{FALSE} (i.e.,
#'                          \code{THE MODEL ESTIMATION TERMINATED NORMALLY}.
#'    \item{\code{"#Param"}}: Number of estimated parameters (i.e.,
#'                            \code{Number of Free Parameters}).
#'    \item{\code{"logLik"}}: Log-likelihood of the estimated model (i.e., \code{H0 Value}).
#'    \item{\code{"Scale"}}: Scaling correction factor (i.e.,
#'                           \code{H0 Scaling Correction Factor for}). Provided
#'                           only when \code{ESTIMATOR IS MLR}.
#'    \item{\code{"LL Rep"}}: Best log-likelihood replicated, \code{TRUE} or \code{FALSE}
#'                            (i.e., \code{THE BEST LOGLIKELIHOOD VALUE HAS BEEN REPLICATED}).
#'    \item{\code{"AIC"}}: Akaike information criterion (i.e., \code{Akaike (AIC)}).
#'    \item{\code{"CAIC"}}: Consistent AIC, not reported in the Mplut output, but
#'                          simply \code{BIC + #Param}.
#'    \item{\code{"BIC"}}: Bayesian information criterion (i.e., \code{Bayesian (BIC)}).
#'    \item{\code{"SABIC"}}: Sample-size adjusted BIC (i.e., \code{Sample-Size Adjusted BIC}).
#'    \item{\code{"LMR-LRT"}}: Significance value (\emph{p}-value) of the Vuong-Lo-Mendell-Rubin test
#'                             (i.e., \code{VUONG-LO-MENDELL-RUBIN LIKELIHOOD RATIO TEST}).
#'                             Provided only when \code{OUTPUT: TECH11}.
#'    \item{\code{"A-LRT"}}: Significance value (\emph{p}-value) of the Adjusted Lo-Mendell-Rubin Test
#'                           (i.e., \code{LO-MENDELL-RUBIN ADJUSTED LRT TEST}).
#'                           Provided only when \code{OUTPUT: TECH11}.
#'    \item{\code{"BLRT"}}: Significance value (\emph{p}-value) of the bootstrapped
#'                          likelihood ratio test. Provided only when \code{OUTPUT: TECH14}.
#'    \item{\code{"Entropy"}}: Sample-size adjusted BIC (i.e., \code{Entropy}).
#'    \item{\code{"p1"}}: Class proportion of the first profile based on the estimated
#'                        posterior probabilities (i.e., \code{FINAL CLASS COUNTS AND PROPORTIONS}).
#'    \item{\code{"p2"}}: Class proportion of the second profile based on the estimated
#'                        posterior probabilities (i.e., \code{FINAL CLASS COUNTS AND PROPORTIONS}).
#'  }
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{mplus.lpa}}, \code{\link{run.mplus}}, \code{\link{read.mplus}},
#' \code{\link{write.mplus}}
#'
#' @references
#' Masyn, K. E. (2013). Latent class analysis and finite mixture modeling. In T. D.
#' Little (Ed.), \emph{The Oxford handbook of quantitative methods: Statistical analysis}
#' (pp. 551–611). Oxford University Press.
#'
#' Muthen, L. K., & Muthen, B. O. (1998-2017). \emph{Mplus User's Guide} (8th ed.).
#' Muthen & Muthen.
#'
#' @return
#' Returns an object, which is a list with following entries:
#'
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{output}}{list with all Mplus outputs}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{result}}{table with results of all Mplus outputs}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load data set "HolzingerSwineford1939" in the lavaan package
#' data("HolzingerSwineford1939", package = "lavaan")
#'
#' # Run LPA with k = 1 to k = 6 profiles
#' mplus.lpa(HolzingerSwineford1939, ind = c("x1", "x2", "x3", "x4"),
#'           run.mplus = TRUE)
#'
#' # Read Mplus output files, create result table, and write table
#' result.lpa(write = "LPA.xlsx")
#' }
result.lpa <- function(folder = getwd(), sort.n = TRUE, sort.p = TRUE,
                       digits = 1, p.digits = 3, write = NULL, check = TRUE,
                       output = TRUE) {

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    ## Check input 'sort.p' ##
    if (isTRUE(!is.logical(sort.p))) { stop("Please specify TRUE or FALSE for the argument 'sort.p'.", call. = FALSE) }

    ## Check input 'digits' ##
    if (isTRUE(digits %% 1L != 0L || digits < 0L || digits == 0L)) { stop("Specify a positive integer number for the argument 'digits'.", call. = FALSE) }

    # Check input 'p.digits'
    if (isTRUE(p.digits %% 1L != 0L || p.digits < 0L)) { stop("Specify a positive integer number for the argument 'p.digits'.", call. = FALSE) }

    ## Check input 'output' ##
    if (isTRUE(!is.logical(output))) { stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Read Mplus Output Files ####

  #...................
  ### Subfolders ####

  # Subfolders
  subfolder <- list.dirs(folder, full.names = TRUE, recursive = FALSE)

  # Mplus output files in the subfolders
  subfolder.out <- lapply(sapply(subfolder, list), list.files, pattern = ".out")

  if (isTRUE(length(unlist(subfolder.out)) == 0L)) { stop("No Mplus output files found in the subfolders specified in the argument 'folder'.", call. = FALSE) }

  #...................
  ### Subfolders with Mplus outputs ####

  # Exclude folders without any Mplus outputs
  lpa.folder <- subfolder[which(sapply(subfolder.out, length) != 0L)]

  # Check if only one output in each folder
  lpa.folder.1 <- sapply(lapply(sapply(lpa.folder, list), list.files, pattern = ".out"), length)

  if (isTRUE(any(lpa.folder.1 == 1L))) { stop(paste0("Following subfolders has only one Mplus TYPE IS MIXTURE output file, please remove them from the parent folder: \n", paste0(names(lpa.folder.1[which(lpa.folder.1 == 1)]), collapse = "\n")), call. = FALSE) }

  #...................
  ### Read Mplus outputs ####

  # Read outputs
  lpa.out <- suppressWarnings(lapply(lpa.folder, function(y) sapply(file.path(y, grep(".out", list.files(y), value = TRUE, useBytes = TRUE)), readLines)))

  # Check if all outputs are MIXTURE
  lpa.out.mix <- suppressWarnings(names(which(unlist(lapply(lpa.out, function(y) lapply(y, function(z) !any(grepl("MIXTURE", z, ignore.case = TRUE, useBytes = TRUE))))))))

  if (isTRUE(length(lpa.out.mix) != 0)) { stop(paste0("Following Mplus output files are not TYPE IS MIXTURE, please remove them from subfolders: \n", paste0(lpa.out.mix, collapse = "\n")), call. = FALSE) }

  # Check if any outputs are based on multiple imputation
  lpa.out.imp <- suppressWarnings(names(which(unlist(lapply(lpa.out, function(y) lapply(y, function(z) any(grepl("IMPUTATION", z, ignore.case = TRUE, useBytes = TRUE))))))))

  if (isTRUE(length(lpa.out.imp) != 0)) { stop("This function does not support Mplus outputs for analaysis based on multiply imputed data sets.", call. = FALSE) }

  #...................
  ### Check if models with more than one latent class ####

  if (isTRUE(any(unlist(lapply(lpa.out, function(y) sapply(y, function(y) as.numeric(misty::chr.trim(sub("Number of categorical latent variables", "", grep("Number of categorical latent variables", y, value = TRUE))))))) != 1L))) {

    stop("This function does not support mixture models with more than one latent class.", call. = FALSE)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Extract Results ####

  #...................
  ### Maximum number of profiles ####

  max.profile <- max(unlist(lapply(lpa.out, function(y) sapply(y, function(y) { classes <- unlist(strsplit(grep("CLASSES ", y, value = TRUE, ignore.case = TRUE, useBytes = TRUE), ""))
                                                                                as.numeric(misty::chr.trim(paste(classes[(grep("\\(", classes, useBytes = TRUE) + 1L):(grep("\\)", classes, useBytes = TRUE) - 1L)], collapse = ""))) }))))

  # Data frame for class proportions
  p.profile <- data.frame(matrix(nrow = 0L, ncol = max.profile, dimnames = list(NULL, paste0("p", 1L:max.profile))))

  #...................
  ### Function for extracting results ####

  extract.result <- function(lpa.out.extract) {

    #### Model converged ####
    conv <- any(grepl("THE MODEL ESTIMATION TERMINATED NORMALLY", lpa.out.extract, useBytes = TRUE))

    #### Number of profiles ####
    if (isTRUE(any(grepl("CLASSES ", lpa.out.extract, ignore.case = TRUE, useBytes = TRUE)))) {

      classes <- unlist(strsplit(grep("CLASSES ", lpa.out.extract, value = TRUE, ignore.case = TRUE, useBytes = TRUE), ""))
      nprofile <- as.numeric(misty::chr.trim(paste(classes[(grep("\\(", classes, useBytes = TRUE) + 1L):(grep("\\)", classes, useBytes = TRUE) - 1L)], collapse = "")))

    } else {

      nprofile <- NA

    }

    #### Model converged ####
    if (isTRUE(conv)) {

      p.profile <- misty::df.rbind(p.profile,
                                   data.frame(matrix(as.numeric(sapply(5L:(5L + nprofile - 1L), function(z) {

                                     temp <- unlist(strsplit(lpa.out.extract[grep("BASED ON THE ESTIMATED MODEL", lpa.out.extract, useBytes = TRUE) + z], " "))

                                     temp[which(grepl("\\.", temp, useBytes = TRUE))][2L]

                                   })), ncol = nprofile, dimnames = list(NULL, paste0("p", 1L:nprofile)))))

      p.profile <- data.frame(matrix(as.numeric(sapply(5L:(5L + nprofile - 1L), function(z) {

        temp <- unlist(strsplit(lpa.out.extract[grep("BASED ON THE ESTIMATED MODEL", lpa.out.extract, useBytes = TRUE) + z], " "))

        temp[which(grepl("\\.", temp, useBytes = TRUE))][2L]

      })), ncol = nprofile, dimnames = list(NULL, paste0("p", 1L:nprofile))))

      data.frame(# Number of profiles
                 nprofile = nprofile,
                 # Model converged
                 converge = conv,
                 # Number of parameters
                 nparam = as.numeric(misty::chr.trim(sub("Number of Free Parameters", "", grep("Number of Free Parameters", lpa.out.extract, value = TRUE, useBytes = TRUE)))),
                 # Loglikelihood
                 LL = as.numeric(misty::chr.trim(sub("H0 Value", "", grep("H0 Value  ", lpa.out.extract, value = TRUE, useBytes = TRUE)))),
                 # Loglikelihood Scaling correction factor
                 LL.scale = ifelse(any(grepl("H0 Scaling Correction Factor", lpa.out.extract, useBytes = TRUE)), as.numeric(misty::chr.trim(sub("H0 Scaling Correction Factor", "", grep("H0 Scaling Correction Factor", lpa.out.extract, value = TRUE, useBytes = TRUE)))), NA),
                 # Loglikelihood replicated
                 LL.rep = any(grepl("THE BEST LOGLIKELIHOOD VALUE HAS BEEN REPLICATED", lpa.out.extract, useBytes = TRUE)),
                 # AIC, CAIC, BIC, SABIC
                 aic = as.numeric(misty::chr.trim(sub("Akaike \\(AIC\\)", "", grep("Akaike \\(AIC\\)", lpa.out.extract, value = TRUE, useBytes = TRUE)))),
                 caic = as.numeric(misty::chr.trim(sub("Bayesian \\(BIC\\)", "", grep("Bayesian \\(BIC\\)", lpa.out.extract, value = TRUE, useBytes = TRUE)))) + as.numeric(misty::chr.trim(sub("Number of Free Parameters", "", grep("Number of Free Parameters", lpa.out.extract, value = TRUE, useBytes = TRUE)))),
                 bic = as.numeric(misty::chr.trim(sub("Bayesian \\(BIC\\)", "", grep("Bayesian \\(BIC\\)", lpa.out.extract, value = TRUE, useBytes = TRUE)))),
                 sabic = as.numeric(misty::chr.trim(sub("Sample-Size Adjusted BIC", "", grep("Sample-Size Adjusted BIC", lpa.out.extract, value = TRUE, useBytes = TRUE)))),
                 # LMR-LRT
                 lmr.lrt = ifelse(any(grepl("VUONG-LO-MENDELL-RUBIN LIKELIHOOD RATIO TEST", lpa.out.extract, useBytes = TRUE)), as.numeric(misty::chr.trim(sub("P-Value", "", lpa.out.extract[grep("Standard Deviation", lpa.out.extract, useBytes = TRUE) + 1L]))), NA),
                 # Adjusted LMR-LRT
                 almr.lrt = ifelse(any(grepl("LO-MENDELL-RUBIN ADJUSTED", lpa.out.extract, useBytes = TRUE)), as.numeric(misty::chr.trim(sub("P-Value", "", lpa.out.extract[grep("LO-MENDELL-RUBIN ADJUSTED", lpa.out.extract, useBytes = TRUE) + 3L]))), NA),
                 # Bootstrap LRT
                 blrt = ifelse(any(grepl("PARAMETRIC BOOTSTRAPPED LIKELIHOOD", lpa.out.extract, useBytes = TRUE)), as.numeric(misty::chr.trim(sub("Approximate P-Value", "", lpa.out.extract[grep("Approximate P-Value", lpa.out.extract, useBytes = TRUE)]))), NA),
                 # Entropy
                 entropy = ifelse(any(grepl("Entropy", lpa.out.extract, useBytes = TRUE)), as.numeric(misty::chr.trim(sub("Entropy", "", grep("Entropy", lpa.out.extract, value = TRUE, useBytes = TRUE)))), NA),
                 # Class proportions
                 p.profile)

    #### Model not converged ####
    } else {

      data.frame(nprofile = nprofile, converge = conv, nparam = NA, LL = NA, LL.scale = NA, LL.rep = NA,
                 aic = NA, caic = NA, bic = NA, sabic = NA, lmr.lrt = NA, almr.lrt = NA, blrt = NA,
                 entropy = NA)

    }

  }

  #...................
  ### Extract results ####

  lpa.result <- NULL
  for (i in seq_along(lpa.out)) {

    lpa.result <- misty::df.rbind(lpa.result,
                                  data.frame(folder = sapply(strsplit(lpa.folder, "/"), function(y) rev(y)[1L])[i], do.call(misty::df.rbind, lapply(lpa.out[[i]], extract.result))))

  }

  #...................
  ### Sort table ####

  if (isTRUE(sort.n)) { lpa.result <- lpa.result[order(lpa.result$folder, lpa.result$nprofile), ] }

  #...................
  ### Sort class proportions ####

  if (isTRUE(sort.p)) { lpa.result[, colnames(lpa.result)[substr(colnames(lpa.result), 1L, 1L) == "p"]] <- t(apply(lpa.result[, colnames(lpa.result)[substr(colnames(lpa.result), 1L, 1L) == "p"]], 1L, sort, decreasing = TRUE, na.last = TRUE)) }

  #_____________________________________________________________________________
  #
  # Return object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "result.lpa",
                 output = lpa.out,
                 args = list(folder = folder, subfolder = lpa.folder, sort.n = sort,
                             sort.p = sort.p, digits = digits, p.digits = p.digits,
                             check = check),
                 result = lpa.result)

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Write Results --------------------------------------------------------------

  if (isTRUE(!is.null(write))) { misty::write.result(object, file = write) }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
