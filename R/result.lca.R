#' Summary Result Table and Bar Charts for Latent Class Analysis Estimated in Mplus
#'
#' This function reads all Mplus output files from latent class analysis in
#' subfolders to create a summary result table and bar charts for each latent
#' class solution separately. By default, the function reads output files in all
#' subfolders of the current working directory. Optionally, bar charts for each
#' latent class solution can be requested by setting the argument \code{plot}
#' to \code{TRUE}. Note that subfolders with only one Mplus output file are
#' excluded.
#'
#' @param folder          a character string indicating the path of the folder
#'                        containing subfolders with the Mplus output files. By
#'                        default Mplus outputs in the subfolders of the current
#'                        working directory are read.
#' @param folder          a character vector indicating the name of the subfolders
#'                        to be excluded from the summary result table.
#' @param exclude         a character vector indicating the name of the subfolders
#'                        excluded from the result tables.
#' @param sort.n          logical: if \code{TRUE} (default), result table is sorted
#'                        according to the number of classes within each folder.
#' @param sort.p          logical: if \code{TRUE} (default), class proportions are
#'                        sorted decreasing.
#' @param plot            logical: if \code{TRUE}, bar charts with error bars for
#'                        confidence intervals are saved in the folder
#'                        \code{_Plots} within subfolders. Note that plots are only
#'                        available for LCA with continuous
#'                        or count indicator variables.
#' @param ci              logical: if \code{TRUE} (default), confidence intervals
#'                        are added to the bar charts.
#' @param conf.level      a numeric value between 0 and 1 indicating the confidence
#'                        level of the interval.
#' @param adjust          logical: if \code{TRUE} (default), difference-adjustment
#'                        for the confidence intervals is applied.
#' @param axis.title      a numeric value specifying the size of the axis title.
#' @param axis.text       a numeric value specifying the size of the axis text
#' @param levels          a character string specifying the order of the indicator
#'                        variables shown on the x-axis.
#' @param labels          a character string specifying the labels of the indicator
#'                        variables shown on the x-axis.
#' @param ylim            a numeric vector of length two specifying limits of the
#'                        y-axis.
#' @param ylab            a character string specifying the label of the y-axis.
#' @param breaks          a numeric vector specifying the points at which tick-marks
#'                        are drawn at the y-axis.
#' @param error.width     a numeric vector specifying the width of the error bars.
#'                        By default, the width of the error bars is 0.1 plus
#'                        number of classes divided by 30.
#' @param legend.title    a numeric value specifying the size of the legend title.
#' @param legend.text     a numeric value specifying the size of the legend text.
#' @param legend.key.size a numeric value specifying the size of the legend keys.
#' @param gray            logical: if \code{TRUE}, bar charts are drawn in gray
#'                        scale.
#' @param start           a numeric value between 0 and 1 specifying the gray value
#'                        at the low end of the palette.
#' @param end             a numeric value between 0 and 1 specifying the gray value
#'                        at the high end of the palette.
#' @param dpi             a numeric value specifying the plot resolution when saving
#'                        the bar chart.
#' @param width           a numeric value specifying the width of the plot when
#'                        saving the bar chart. By default, the width is number of
#'                        indicators plus number of classes divided by 2.
#' @param height          a numeric value specifying the height of the plot when
#'                        saving the bar chart.
#' @param digits          an integer value indicating the number of decimal places
#'                        to be used for displaying results. Note that the scaling
#'                        correction factor is displayed  with \code{digits} plus 1
#'                        decimal places.
#' @param p.digits        an integer value indicating the number of decimal places
#'                        to be used for displaying \emph{p}-values, entropy value,
#'                        and class proportions.
#' @param write           a character string for writing the results into an Excel
#'                        file naming a file with or without file extension '.xlsx',
#'                        e.g., \code{"Results.xlsx"} or \code{"Results"}.
#' @param check           logical: if \code{TRUE}, argument specification is checked.
#' @param output          logical: if \code{TRUE}, output is shown.
#'
#' @details
#' The result summary table comprises following entries:
#' \itemize{
#'    \item{\code{"Folder"}}: Subfolder from which the group of Mplus outputs files
#'                            were summarized.
#'    \item{\code{"#Class"}}: Number of classes (i.e., \code{CLASSES ARE c(#Class)}).
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
#'    \item{\code{"CAIC"}}: Consistent AIC, not reported in the Mplus output, but
#'                          simply \code{BIC + #Param}.
#'    \item{\code{"BIC"}}: Bayesian information criterion (i.e., \code{Bayesian (BIC)}).
#'    \item{\code{"Chi-Pear"}}: Pearson chi-square test of model fit (i.e., \code{Pearson Chi-Square}),
#'                              only available when indicators are count or ordered categorical
#'    \item{\code{"Chi-LRT"}}: Likelihood ratio chi-square test of model fit (i.e., \code{Likelihood Ratio Chi-Square}),
#'                             only available when indicators are count or ordered catgeorical.
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
#'    \item{\code{"p1"}}: Class proportion of the first class based on the estimated
#'                        posterior probabilities (i.e., \code{FINAL CLASS COUNTS AND PROPORTIONS}).
#'    \item{\code{"p2"}}: Class proportion of the second class based on the estimated
#'                        posterior probabilities (i.e., \code{FINAL CLASS COUNTS AND PROPORTIONS}).
#'  }
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{mplus.lca}}, \code{\link{run.mplus}}, \code{\link{read.mplus}},
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
#' \item{\code{result}}{list with result tables, i.e., \code{summary} for the
#'                      summary result table, \code{mean_var} for the result
#'                      table with means and variances for each latent class
#'                      separately, \code{mean} for the result table with means
#'                      for each latent class separately, and \code{var} for the
#'                      result table with variances for each latent class separately}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load data set "HolzingerSwineford1939" in the lavaan package
#' data("HolzingerSwineford1939", package = "lavaan")
#'
#' # Run LCA with k = 1 to k = 6 classes
#' mplus.lca(HolzingerSwineford1939, ind = c("x1", "x2", "x3", "x4"),
#'           run.mplus = TRUE)
#'
#' # Read Mplus output files, create result table, write table, and save plots
#' result.lca(write = "LCA.xlsx", plot = TRUE)
#'
#' #------------------------------------------
#' # Draw bar chart manually
#'
#' library(ggplot2)
#'
#' # Collect LCA results
#' lca.result <- result.lca()
#'
#' # Result table with means
#' means <- lca.result$result$mean
#'
#' # Extract results from variance-covariance structure A with 4 latent classes
#' plotdat <- means[means$folder == "A_Invariant-Theta_Diagonal-Sigma" &  means$nclass == 4, ]
#'
#' # Draw bar chart
#' ggplot(plotdat, aes(ind, est, group = class, fill = class)) +
#'   geom_bar(stat = "identity", position = "dodge", color = "black",
#'            linewidth = 0.1) +
#'   geom_errorbar(aes(ymin = low, ymax = upp), width = 0.23,
#'                 linewidth = 0.2, position = position_dodge(0.9)) +
#'   scale_x_discrete("") +
#'   scale_y_continuous("Mean Value", limits = c(0, 9),
#'                      breaks = seq(0, 9, by = 1)) +
#'   labs(fill = "Latent Class") +
#'   guides(fill = guide_legend(nrow = 1L)) +
#'   theme(axis.title = element_text(size = 11),
#'         axis.text = element_text(size = 11),
#'         legend.position = "bottom",
#'         legend.key.size = unit(0.5 , 'cm'),
#'         legend.title = element_text(size = 11),
#'         legend.text = element_text(size = 11),
#'         legend.box.spacing = unit(-9L, "pt"))
#'
#' # Save bar chart
#' ggsave("LCA_4-Class.png", dpi = 600, width = 6, height = 4)
#' }
result.lca <- function(folder = getwd(), exclude = NULL, sort.n = TRUE, sort.p = TRUE,
                       plot = FALSE, ci = TRUE, conf.level = 0.95, adjust = TRUE,
                       axis.title = 7, axis.text = 7, levels = NULL, labels = NULL,
                       ylim = NULL, ylab = "Mean Value", breaks = ggplot2::waiver(),
                       error.width = 0.1, legend.title = 7, legend.text = 7,
                       legend.key.size = 0.4, gray = FALSE, start = 0.15, end = 0.85,
                       dpi = 600, width = "n.ind", height = 4, digits = 1, p.digits = 3,
                       write = NULL, check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    ## Check input 'sort.p' ##
    if (isTRUE(!is.logical(sort.p))) { stop("Please specify TRUE or FALSE for the argument 'sort.p'.", call. = FALSE) }

    # Check input 'plot'
    if (isTRUE(!is.logical(plot))) { stop("Please specify TRUE or FALSE for the argument 'plot'.", call. = FALSE) }

    # Check input 'ci'
    if (isTRUE(!is.logical(ci))) { stop("Please specify TRUE or FALSE for the argument 'ci'.", call. = FALSE) }

    # Check input 'conf.level'
    if (isTRUE(conf.level >= 1L || conf.level <= 0L)) { stop("Please specifiy a numeric value between 0 and 1 for the argument 'conf.level'.", call. = FALSE) }

    # Check input 'adjust'
    if (isTRUE(!is.logical(adjust))) { stop("Please specify TRUE or FALSE for the argument 'adjust'.", call. = FALSE) }

    ## Check input 'digits' ##
    if (isTRUE(digits %% 1L != 0L || digits < 0L || digits == 0L)) { stop("Specify a positive integer number for the argument 'digits'.", call. = FALSE) }

    # Check input 'gray'
    if (isTRUE(!is.logical(gray))) { stop("Please specify TRUE or FALSE for the argument 'gray'.", call. = FALSE) }

    # Check input 'start'
    if (isTRUE(start < 0L || start > 1L)) { stop("Please specify a numeric value between 0 and 1 for the argument 'start'", call. = FALSE) }

    # Check input 'end'
    if (isTRUE(end < 0L || end > 1L)) { stop("Please specify a numeric value between 0 and 1 for the argument 'end'", call. = FALSE) }

    # Check input 'p.digits'
    if (isTRUE(p.digits %% 1L != 0L || p.digits < 0L)) { stop("Specify a positive integer number for the argument 'p.digits'.", call. = FALSE) }

    ## Check input 'output' ##
    if (isTRUE(!is.logical(output))) { stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  ind <- est <- low <- upp <- NULL

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Read Mplus Output Files ####

  #...................
  ### Subfolders ####

  # Subfolders
  subfolder <- list.dirs(folder, full.names = TRUE, recursive = FALSE)

  # Exclude subfolder
  if (isTRUE(!is.null(exclude))) { subfolder <- subfolder[-sapply(exclude, function(y) grep(y, subfolder))] }

  # Mplus output files in the subfolders
  subfolder.out <- lapply(sapply(subfolder, list), list.files, pattern = ".out")

  if (isTRUE(length(unlist(subfolder.out)) == 0L)) { stop("No Mplus output files found in the subfolders specified in the argument 'folder'.", call. = FALSE) }

  #...................
  ### Subfolders with Mplus outputs ####

  # Exclude folders without any Mplus outputs
  lca.folder <- subfolder[which(sapply(subfolder.out, length) > 1L)]

  #...................
  ### Read Mplus outputs ####

  # Read outputs, iconv() removes Non-ASCII characters
  lca.out <- suppressWarnings(lapply(lca.folder, function(y) sapply(file.path(y, grep(".out", list.files(y), value = TRUE, useBytes = TRUE)), function(z) iconv(readLines(z),  sub = ""))))

  # Check if all outputs are MIXTURE
  lca.out.mix <- suppressWarnings(names(which(unlist(lapply(lca.out, function(y) lapply(y, function(z) !any(grepl("MIXTURE", z, ignore.case = TRUE, useBytes = TRUE))))))))

  # Exclude outputs
  if (isTRUE(length(lca.out.mix) != 0)) { lca.out <- suppressWarnings(lapply(lca.folder, function(y) sapply(misty::chr.omit(file.path(y, grep(".out", list.files(y), value = TRUE, useBytes = TRUE)), omit = lca.out.mix, check = FALSE), function(z) iconv(readLines(z), sub = "")))) }

  # Check if any outputs are based on multiple imputation
  lca.out.imp <- suppressWarnings(names(which(unlist(lapply(lca.out, function(y) lapply(y, function(z) any(grepl("IMPUTATION", z, ignore.case = TRUE, useBytes = TRUE))))))))

  if (isTRUE(length(lca.out.imp) != 0)) { stop("This function does not support Mplus outputs for analysis based on multiply imputed data sets.", call. = FALSE) }

  #...................
  ### Check if models with more than one latent class ####

  if (isTRUE(any(unlist(lapply(lca.out, function(y) sapply(y, function(y) as.numeric(misty::chr.trim(sub("Number of categorical latent variables", "", grep("Number of categorical latent variables", y, value = TRUE))))))) != 1L))) {

    stop("This function does not support mixture models with more than one latent class.", call. = FALSE)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Extract Results ####

  #...................
  ### Maximum number of classes ####

  max.class <- max(unlist(lapply(lca.out, function(y) sapply(y, function(y) { classes <- unlist(strsplit(grep("CLASSES ", y, value = TRUE, ignore.case = TRUE, useBytes = TRUE), ""))
                                                                              as.numeric(misty::chr.trim(paste(classes[(grep("\\(", classes, useBytes = TRUE) + 1L):(grep("\\)", classes, useBytes = TRUE) - 1L)], collapse = ""))) }))))
  #...................
  ### Function for extracting results ####

  extract.result <- function(lca.out.extract) {

    #### Model converged ####
    conv <- any(grepl("THE MODEL ESTIMATION TERMINATED NORMALLY", lca.out.extract, useBytes = TRUE))

    #### Number of classes ####
    if (isTRUE(any(grepl("CLASSES ", lca.out.extract, ignore.case = TRUE, useBytes = TRUE)))) {

      classes <- unlist(strsplit(grep("CLASSES ", lca.out.extract, value = TRUE, ignore.case = TRUE, useBytes = TRUE), ""))
      nclass <- as.numeric(misty::chr.trim(paste(classes[(grep("\\(", classes, useBytes = TRUE) + 1L):(grep("\\)", classes, useBytes = TRUE) - 1L)], collapse = "")))

    } else {

      nclass <- NA

    }

    #### Model converged ####
    if (isTRUE(conv)) {

      ##### Model summary ####
      model.summary <- data.frame(# Number of classes
                                  nclass = nclass,
                                  # Model converged
                                  conv = conv,
                                  # Number of parameters
                                  nparam = as.numeric(misty::chr.trim(sub("Number of Free Parameters", "", grep("Number of Free Parameters", lca.out.extract, value = TRUE, useBytes = TRUE)))),
                                  # Loglikelihood
                                  LL = as.numeric(misty::chr.trim(sub("H0 Value", "", grep("H0 Value  ", lca.out.extract, value = TRUE, useBytes = TRUE)))),
                                  # Loglikelihood Scaling correction factor
                                  LL.scale = ifelse(any(grepl("H0 Scaling Correction Factor", lca.out.extract, useBytes = TRUE)), as.numeric(misty::chr.trim(sub("H0 Scaling Correction Factor", "", grep("H0 Scaling Correction Factor", lca.out.extract, value = TRUE, useBytes = TRUE)))), NA),
                                  # Loglikelihood replicated
                                  LL.rep = ifelse(any(grepl("THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED", lca.out.extract, useBytes = TRUE)), FALSE, TRUE),
                                  # AIC, CAIC, BIC, SABIC
                                  aic = as.numeric(misty::chr.trim(sub("Akaike \\(AIC\\)", "", grep("Akaike \\(AIC\\)", lca.out.extract, value = TRUE, useBytes = TRUE)))),
                                  caic = as.numeric(misty::chr.trim(sub("Bayesian \\(BIC\\)", "", grep("Bayesian \\(BIC\\)", lca.out.extract, value = TRUE, useBytes = TRUE)))) + as.numeric(misty::chr.trim(sub("Number of Free Parameters", "", grep("Number of Free Parameters", lca.out.extract, value = TRUE, useBytes = TRUE)))),
                                  bic = as.numeric(misty::chr.trim(sub("Bayesian \\(BIC\\)", "", grep("Bayesian \\(BIC\\)", lca.out.extract, value = TRUE, useBytes = TRUE)))),
                                  sabic = as.numeric(misty::chr.trim(sub("Sample-Size Adjusted BIC", "", grep("Sample-Size Adjusted BIC", lca.out.extract, value = TRUE, useBytes = TRUE)))),
                                  # Pearson Chi-Square
                                  chi.pear = ifelse(any(grepl("Chi-Square Test of Model Fit", lca.out.extract, useBytes = TRUE)), as.numeric(misty::chr.trim(sub("P-Value", "", lca.out.extract[grep("Pearson Chi-Square", lca.out.extract, useBytes = TRUE)[1L] + 4L]))), NA),
                                  # Likelihood Ratio Chi-Square
                                  chi.lrt = ifelse(any(grepl("Chi-Square Test of Model Fit", lca.out.extract, useBytes = TRUE)), as.numeric(misty::chr.trim(sub("P-Value", "", lca.out.extract[grep("Likelihood Ratio Chi-Square", lca.out.extract, useBytes = TRUE)[1L] + 4L]))), NA),
                                  # LMR-LRT
                                  lmr.lrt = ifelse(any(grepl("VUONG-LO-MENDELL-RUBIN LIKELIHOOD RATIO TEST", lca.out.extract, useBytes = TRUE)), as.numeric(misty::chr.trim(sub("P-Value", "", lca.out.extract[grep("Standard Deviation", lca.out.extract, useBytes = TRUE) + 1L]))), NA),
                                  # Adjusted LMR-LRT
                                  almr.lrt = ifelse(any(grepl("LO-MENDELL-RUBIN ADJUSTED", lca.out.extract, useBytes = TRUE)), as.numeric(misty::chr.trim(sub("P-Value", "", lca.out.extract[grep("LO-MENDELL-RUBIN ADJUSTED", lca.out.extract, useBytes = TRUE) + 3L]))), NA),
                                  # Bootstrap LRT
                                  blrt = ifelse(any(grepl("PARAMETRIC BOOTSTRAPPED LIKELIHOOD", lca.out.extract, useBytes = TRUE)), as.numeric(misty::chr.trim(sub("Approximate P-Value", "", lca.out.extract[grep("Approximate P-Value", lca.out.extract, useBytes = TRUE)]))), NA),
                                  # Entropy
                                  entropy = ifelse(any(grepl("Entropy", lca.out.extract, useBytes = TRUE)), as.numeric(misty::chr.trim(sub("Entropy", "", grep("Entropy", lca.out.extract, value = TRUE, useBytes = TRUE)))), NA),
                                  # Class proportions
                                  misty::df.rbind(data.frame(matrix(as.numeric(sapply(5L:(5L + nclass - 1L), function(z) {

                                                    temp <- unlist(strsplit(lca.out.extract[grep("BASED ON THE ESTIMATED MODEL", lca.out.extract, useBytes = TRUE) + z], " "))

                                                    temp[which(grepl("\\.", temp, useBytes = TRUE))][2L]

                                                  })), ncol = nclass, dimnames = list(NULL, paste0("p", 1L:nclass))))))

      ##### Means and variances ####

      if (isTRUE(length(grep("Binary and ordered categorical", lca.out.extract)) == 0L && length(grep("Unordered categorical", lca.out.extract, ignore.case = TRUE)) == 0L)) {

        # Number of indicators
        n.ind <- as.numeric(misty::chr.trim(sub("Number of dependent variables", "", grep("Number of dependent variables", lca.out.extract, value = TRUE, useBytes = TRUE))))

        # Extract means
        out.means <- lapply(data.frame(sapply(if (isTRUE(nclass == 1L)) { grep("Means", lca.out.extract) } else { head(grep("Means", lca.out.extract), n = -1L) }, function(y) misty::chr.trim(misty::chr.omit(strsplit(lca.out.extract[(y + 1L):(y + n.ind)], "  "), omit = "")))),
                            function(z) data.frame(param = "Mean", matrix(z, ncol = 5L, byrow = TRUE, dimnames = list(NULL, c("ind", "est", "se", "z", "pval")))))

        out.means <- do.call(rbind, lapply(seq_along(out.means), function(y) data.frame(class = y, out.means[y][[1L]])))

        # Extract variances
        out.var <- lapply(data.frame(sapply(grep("Variances", lca.out.extract), function(y) misty::chr.trim(misty::chr.omit(strsplit(lca.out.extract[(y + 1L):(y + n.ind)], "  "), omit = "")))),
                          function(z) data.frame(param = "Variance", matrix(z, ncol = 5L, byrow = TRUE, dimnames = list(NULL, c("ind", "est", "se", "z", "pval")))))

        out.var <- do.call(rbind, lapply(seq_along(out.var), function(y) data.frame(class = y, out.var[y][[1L]])))

        # Class count based on the estimated model
        class.count <- matrix(as.numeric(misty::chr.omit(strsplit(lca.out.extract[(grep("BASED ON THE ESTIMATED MODEL", lca.out.extract) + 5L):(grep("BASED ON ESTIMATED POSTERIOR PROBABILITIES", lca.out.extract) - 2L)], " "), "")), ncol = 3, byrow = TRUE)

        # Merge means and variances
        model.descript <- data.frame(nclass = nclass, n = class.count[match(out.means[, "class"], class.count[, 1L]), 2L], rbind(out.means, out.var))

        # Numeric
        model.descript[, c("class", "est", "se", "z", "pval")] <- sapply(model.descript[, c("class", "est", "se", "z", "pval")], as.numeric)

        # Factor
        model.descript$class <- factor(model.descript$class)

        # Indicator names
        model.descript$ind <- sapply(model.descript$ind, function(y) grep(y, unique(misty::chr.trim(gsub(";", "", misty::chr.omit(unlist(strsplit(lca.out.extract[grep("USEVARIABLES", lca.out.extract, ignore.case = TRUE):grep("CLASSES", lca.out.extract, ignore.case = TRUE)[1L]], " ")))))), ignore.case = TRUE, value = TRUE))

        # Sort variables
        model.descript <- model.descript[, c("nclass", "class", "n", "param", "ind", "est", "se", "z", "pval")]

      } else {

        model.descript <- NA

      }

    #### Model not converged ####
    } else {

      model.summary <- data.frame(nclass = nclass, conv = conv, nparam = NA, LL = NA, LL.scale = NA, LL.rep = NA,
                                  aic = NA, caic = NA, bic = NA, sabic = NA, chi.pear = NA, chi.lrt = NA, lmr.lrt = NA, almr.lrt = NA, blrt = NA,
                                  entropy = NA)

      model.descript <- data.frame(matrix(nrow = 0L, ncol = 9L, dimnames = list(NULL, c("nclass", "n", "class", "param", "ind", "est", "se", "z", "pval"))))

    }

    return(list(model.summary = model.summary, model.descript = model.descript))

  }

  #...................
  ### Extract results ####

  #### Model summary ####
  lca.summary <- NULL
  for (i in seq_along(lca.out)) {

    lca.summary <- misty::df.rbind(lca.summary,
                                   data.frame(folder = sapply(strsplit(lca.folder, "/"), function(y) rev(y)[1L])[i], do.call(misty::df.rbind, lapply(lca.out[[i]], function(z) extract.result(z)$model.summary))))

  }

  ##### Sort table ####

  if (isTRUE(sort.n)) { lca.summary <- lca.summary[order(lca.summary$folder, lca.summary$nclass), ] }

  ##### Exclude NA columns ####

  lca.summary <- lca.summary[, sapply(lca.summary, function(y) any(!is.na(y)))]

  ##### Sort class proportions ####

  if (isTRUE(sort.p)) {

    if (isTRUE(sum(substr(colnames(lca.summary), 1L, 1L) == "p") > 1L)) {

      lca.summary[, colnames(lca.summary)[substr(colnames(lca.summary), 1L, 1L) == "p"]] <- t(apply(lca.summary[, colnames(lca.summary)[substr(colnames(lca.summary), 1L, 1L) == "p"]], 1L, sort, decreasing = TRUE, na.last = TRUE))

    }

  }

  #### Means and Variances ####

  lca.mean.var <- NULL
  for (i in seq_along(lca.out)) {

    model.descript <- lapply(lca.out[[i]], function(z) extract.result(z)$model.descript)

    if (any(!is.na(unlist(model.descript)))) {

    lca.mean.var <- misty::df.rbind(lca.mean.var,
                                    data.frame(folder = sapply(strsplit(lca.folder, "/"), function(y) rev(y)[1L])[i], do.call(misty::df.rbind, model.descript)))

    } else {

      lca.mean.var <- NA

    }

  }

  ##### Order results ####
  if (isTRUE(any(!is.na(lca.mean.var)))) {

    lca.mean.var <- data.frame(lca.mean.var[order(lca.mean.var$folder, lca.mean.var$nclass, lca.mean.var$class), ], row.names = NULL)

    ##### Split table ####

    lca.mean <- data.frame(lca.mean.var[which(lca.mean.var$param == "Mean"), ], row.names = NULL)
    lca.var <- data.frame(lca.mean.var[which(lca.mean.var$param == "Variance"), ], row.names = NULL)

    ##### Confidence intervals for means ####

    # Difference-adjustment
    se_z <- if (isTRUE(adjust)) { lca.mean$se * qnorm((1 - conf.level) / 2L, lower.tail = FALSE) } else { lca.mean$se * qnorm((1 - conf.level) / 2L, lower.tail = FALSE) * sqrt(2L) / 2L }

    # Lower and uppt limit
    lca.mean$low <- lca.mean$est - se_z
    lca.mean$upp <- lca.mean$est + se_z

    #...................
    ##### Plots ####

    if (isTRUE(plot)) {

      #...................
      ### Message ####

      if (isTRUE(length(unique(paste0(lca.mean$folder, lca.mean$nclass))) > 10L)) {

        message(paste0("R is making ", length(unique(paste0(lca.mean$folder, lca.mean$nclass))), " plots, this may take a while."))

      }

      #...................
      ### Create Folder ####

      sapply(unique(lca.mean$folder), function(y) suppressWarnings(invisible(dir.create(file.path(y, "_Plots")))))

      #...................
      ### ggplot ####

      ggplot2::theme_set(ggplot2::theme_bw())

      #### Loop across number of latent class solution
      for (i in unique(lca.mean$folder)) {

        # Select data
        temp1 <- lca.mean[lca.mean$folder == i, ]

        #### Loop across number of latent classes
        for (j in unique(temp1$nclass)) {

          # Select data
          temp2 <- temp1[temp1$nclass == j, ]

          # Levels and Labelsl
          temp2$ind <- factor(temp2$ind,
                              levels = if (isTRUE(is.null(levels))) { unique(temp2$ind) } else { levels },
                              labels = if (isTRUE(is.null(levels) && is.null(labels))) {

                                unique(temp2$ind)

                              } else if (isTRUE(is.null(levels) && !is.null(labels))) {

                                labels

                              } else if (isTRUE(!is.null(levels) && is.null(labels))) {

                                levels

                              })

          # Limits
          if (isTRUE(is.null(ylim))) {

            if (isTRUE(ci)) {

              limits.low <- ifelse(min(temp2$low) < 0L, floor(min(temp2$low) / 0.5) * 0.5, 0L)
              limits.upp <- ceiling(max(temp2$upp) / 0.5) * 0.5

            } else {

              limits.low <- ifelse(min(temp2$est) < 0L, floor(min(temp2$est) / 0.5) * 0.5, 0L)
              limits.upp <- ceiling(max(temp2$est) / 0.5) * 0.5

            }

          } else {

            limits.low <- ylim[1L]
            limits.upp <- ylim[2L]

          }

          # Breaks
          if (isTRUE(class(breaks) == "waiver")) {

            if (isTRUE(abs(limits.upp - limits.low) > 5L)) {

              breaks.j <- seq(limits.low, limits.upp, by = 1L)

            } else {

              breaks.j <- seq(limits.low, limits.upp, by = 0.5)

            }

          } else {

            breaks.j <- breaks

          }

          ##### Plot
          p <- ggplot2::ggplot(temp2, ggplot2::aes(ind, est, group = class, fill = class)) +
                ggplot2::geom_bar(stat = "identity", position = "dodge", color = "black", linewidth = 0.1) +
                ggplot2::scale_x_discrete("") +
                ggplot2::scale_y_continuous(ylab,
                                            limits = c(limits.low, limits.upp),
                                            breaks = breaks.j) +
                ggplot2::labs(fill = "Latent Class") +
                ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1L)) +
                ggplot2::theme(axis.title = ggplot2::element_text(size = axis.title),
                               axis.text = ggplot2::element_text(size = axis.text),
                               legend.position = "bottom",
                               legend.key.size = ggplot2::unit(legend.key.size , 'cm'),
                               legend.title = ggplot2::element_text(size = legend.title),
                               legend.text = ggplot2::element_text(size = legend.text),
                               legend.box.spacing = ggplot2::unit(-9L, "pt"))

          ##### Confidence intervals
          if (isTRUE(ci)) { p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = low, ymax = upp), width = error.width + j / 30L, linewidth = 0.2, position = ggplot2::position_dodge(0.9)) }

          ##### Gray color scales
          if (isTRUE(gray)) { p <- p + ggplot2::scale_fill_grey(start = end, end = start) }

          ##### Save plot

          # width argument
          if (isTRUE(width == "n.ind")) { n.ind <- length(unique(temp2$ind)) }

          ggplot2::ggsave(file.path(i, "_Plots", paste0("Bar_Chart_", j, "-Class.png")), dpi = dpi, width = n.ind + j / 2L, height = height)

        }

      }

    }

  } else {

    lca.mean <- lca.var <- NA

  }

  #_____________________________________________________________________________
  #
  # Return object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "result.lca",
                 output = lca.out,
                 args = list(folder = folder, exclude = exclude, sort.n = sort.n, sort.p = sort.p,
                             plot = plot, ci = ci, conf.level = conf.level, adjust = adjust,
                             axis.title = axis.title, axis.text = axis.text, levels = levels, labels = labels,
                             ylim = ylim, ylab = ylab, breaks = breaks, error.width = error.width,
                             legend.title = legend.title, legend.text = legend.text, legend.key.size = legend.key.size,
                             gray = gray, start = start, end = end, dpi = dpi,
                             width = width, height = height, digits = digits, p.digits = p.digits, check = check),
                 result = list(summary = lca.summary, mean_var = lca.mean.var, mean = lca.mean, var = lca.var))

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
