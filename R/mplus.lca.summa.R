#' Summary Result Tables and Grouped Bar Charts for Latent Class Analysis in Mplus
#'
#' This function reads all Mplus output files from latent class analysis in
#' subfolders to create result tables with model summaries (e.g., AIC, CAIC, BIC,
#' SABIC, AWE and cmP), approximate Bayes factors, classification diagnostics
#' (e.g., relative Entropy, AvePP, and OCC), class-specific means and variances
#' or class-specific item response probabilities of the indicator variables, and
#' Cohen's \emph{d}s to quantify class separation between latent class \emph{j}
#' and latent class \emph{k}. By default, the function reads output files in all
#' subfolders of the current working directory or output files in the current
#' working directory and prints a table with model summaries on the console.
#' Bar charts including confidence intervals for each latent class solution can
#' be requested by setting the argument \code{plot} to \code{TRUE}. Note that
#' result tables with Bayes factors, classification diagnostics, class-specific
#' means and variances, class-specific item response probabilities, and Cohen's
#' \emph{d}s will not be printed on the console, but are only available in the
#' exported Excel file when specifying the \code{write} argument (e.g.,
#' \code{write = "Results_LCA.xlsx"}).
#'
#' @param folder          a character string indicating the path of the folder
#'                        containing subfolders with the Mplus output files. By
#'                        default Mplus outputs in the subfolders of the current
#'                        working directory are read. Note that if there are no
#'                        subfolders available, Mplus outputs from the folder
#'                        specified in the argument \code{folder} are extracted.
#' @param exclude         a character vector indicating the name of the subfolder(s)
#'                        excluded from the result tables.
#' @param sort.n          logical: if \code{TRUE} (default), tables with model
#'                        summaries and classification diagnostics are sorted
#'                        according to the number of classes within each folder
#'                        in increasing order.
#' @param sort.p          logical: if \code{TRUE}, model-estimated class counts,
#'                        proportions, average posterior class probabilities,
#'                        and odds of correct classification ratio in the
#'                        classification diagnostics are sorted in increasing order.
#' @param digits          an integer value indicating the number of decimal places
#'                        to be used for displaying LL, AIC, CAIC, BIC, SABIC,
#'                        AWE, OCC, and approximate Bayes factors (aBF).
#' @param p.digits        an integer value indicating the number of decimal places
#'                        to be used for displaying cmP, \emph{p}-values, relative
#'                        entropy values, class proportions, and confidence intervals.
#'                        Note that the scaling correction factor is displayed
#'                        with \code{p.digits} minus 1 decimal places.
#' @param bf.trunc        logical: if \code{TRUE} (default), approximate Bayes
#'                        factors (aBF) greater than 1000 are truncated.
#' @param conf.level      a numeric value between 0 and 1 indicating the confidence
#'                        level of the intervals in the result table with means
#'                        and variances for each latent class separately. Note
#'                        that only \code{0.9}, \code{0.95}, or \code{0.95} are
#'                        allowed when extracting confidence intervals from Mplus
#'                        outputs.
#' @param plot            logical: if \code{TRUE}, bar charts with error bars for
#'                        confidence intervals for LCA with continuous or count
#'                        indicator variables are saved in the folder \code{_Plots}
#'                        within subfolders.
#' @param group.ind       logical: if \code{TRUE} (default), latent class indicators
#'                        are represented by separate bars when saving plots of
#'                        an LCA based on continuous or count indicator variables,
#'                        while latent class indicators are represented on the
#'                        x-axis when saving plots of an LCA based on ordered or
#'                        unordered categorical indicator, if \code{FALSE} latent
#'                        classes are represented by separate bars when saving plots
#'                        of an LCA based on continuous or count indicator variables,
#'                        while latent classes are represented on the x-axis when
#'                        saving plots of an LCA based on ordered or unordered
#'                        categorical indicator.
#' @param ci              logical: if \code{TRUE} (default), confidence intervals
#'                        are added to the bar charts for LCA with continuous or
#'                        count indicator variables.
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
#' @param errorbar.width  a numeric vector specifying the width of the error bars.
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
#' @param width.ind       a numeric value specifying the width of the plot as a
#'                        factor depending on the number of indicator variables.
#'                        By default, the factor is 1.5.
#' @param width.nclass    a numeric value specifying the width of the plot as a
#'                        factor depending on the number of classes.
#'                        By default, the factor is 0.5 when saving plots of an
#'                        LCA based on continuous or count indicator variables,
#'                        while the factor is 1.5 when saving plots of an LCA
#'                        based on ordered or unordered categorical indicator
#'                        variables.
#' @param height.categ    a numeric value specifying the height of the plot as a
#'                        factor depending on the number of response categories.
#'                        By default, the factor is 0.6. Note that this argument
#'                        is used only when saving plots of an LCA based on ordered
#'                        or unordered categorical indicator variables.
#' @param height          a numeric value specifying the height of the plot when
#'                        saving the bar chart. Note that this argument is used
#'                        only when saving plots of an LCA based on continuous
#'                        or count indicator variables.
#' @param write           a character string naming a file for writing the output into
#'                        either a text file with file extension \code{".txt"} (e.g.,
#'                        \code{"Output.txt"}) or Excel file with file extension
#'                        \code{".xlsx"}  (e.g., \code{"Output.xlsx"}). If the file
#'                        name does not contain any file extension, an Excel file will
#'                        be written.
#' @param append          logical: if \code{TRUE} (default), output will be appended
#'                        to an existing text file with extension \code{.txt} specified
#'                        in \code{write}, if \code{FALSE} existing text file will be
#'                        overwritten.
#' @param check           logical: if \code{TRUE} (default), argument specification
#'                        is checked.
#' @param output          logical: if \code{TRUE} (default), output is shown.
#'
#' @details
#' The Excel file exported by the function for reading Mplus output files from
#' latent class analysis with continuous or count indicator variables by
#' specifying the \code{write} argument (e.g., \code{write = "Results_LCA.xlsx"})
#' contains five sheets.
#'
#' \strong{(1) Summary: Model Summaries}
#'
#' \itemize{
#'    \item{\code{"Folder"}}: Subfolder from which the group of Mplus outputs files
#'                            were summarized
#'    \item{\code{"#Class"}}: Number of latent classes, i.e., \code{CLASSES ARE c(#Class)}
#'    \item{\code{"Conv"}}: Model converged, \code{TRUE} or \code{FALSE}, i.e.,
#'                          \code{THE MODEL ESTIMATION TERMINATED NORMALLY}
#'    \item{\code{"#Param"}}: Number of estimated parameters, i.e., \code{Number of Free Parameters}
#'    \item{\code{"logLik"}}: Log-likelihood of the estimated model, i.e., \code{H0 Value}
#'    \item{\code{"Scale"}}: Scaling correction factor, i.e., \code{H0 Scaling Correction Factor for},
#'                           available only when \code{ESTIMATOR IS MLR}
#'    \item{\code{"LLRep"}}: Best log-likelihood replicated, \code{TRUE} or \code{FALSE},
#'                           i.e., \code{THE BEST LOGLIKELIHOOD VALUE HAS BEEN REPLICATED}
#'    \item{\code{"AIC"}}: Akaike information criterion, i.e., \code{Akaike (AIC)}
#'    \item{\code{"CAIC"}}: Consistent AIC, not reported in the Mplus output, but
#'                          simply \code{BIC + #Param}
#'    \item{\code{"BIC"}}: Bayesian information criterion, i.e., \code{Bayesian (BIC)}
#'    \item{\code{"SABIC"}}: Sample-size adjusted BIC, i.e., \code{Sample-Size Adjusted BIC}
#'    \item{\code{"AWE"}}: Approximate weight of evidence criterion (Banfield & Raftery, 1993)
#'    \item{\code{"cmP"}}: Approximate correct model probability (Schwarz, 1978)
#'                         across estimated models in all Mplus output files in
#'                         the subfolders to create result tables
#'    \item{\code{"Chi-Pear"}}: Pearson chi-square test of model fit, i.e., \code{Pearson Chi-Square},
#'                              available only when indicators are count or ordered categorical
#'    \item{\code{"Chi-LRT"}}: Likelihood ratio chi-square test of model fit, i.e., \code{Likelihood Ratio Chi-Square},
#'                             available only when indicators are count or ordered categorical
#'    \item{\code{"LMR-LRT"}}: Significance value (\emph{p}-value) of the Vuong-Lo-Mendell-Rubin test,
#'                             i.e., \code{VUONG-LO-MENDELL-RUBIN LIKELIHOOD RATIO TEST},
#'                             available only when \code{OUTPUT: TECH11}
#'    \item{\code{"A-LRT"}}: Significance value (\emph{p}-value) of the Adjusted Lo-Mendell-Rubin Test,
#'                           i.e., \code{LO-MENDELL-RUBIN ADJUSTED LRT TEST},
#'                           available only when \code{OUTPUT: TECH11}
#'    \item{\code{"BLRT"}}: Significance value (\emph{p}-value) of the bootstrapped
#'                          likelihood ratio test, available only when \code{OUTPUT: TECH14}
#'    \item{\code{"Entropy"}}: Summary of the class probabilities across classes
#'                             and individuals in the sample, i.e., \code{Entropy}
#'    \item{\code{"aPPMin"}}: Minimum average posterior class probability (AvePP)
#'                            for the latent classes
#'    \item{\code{"OCCMin"}}: Minimum odds of correct classification ratio (OCC)
#'    \item{\code{"nMin"}}: Minimum class count for the latent classes based on
#'                          the estimated model
#'    \item{\code{"pMin"}}: Minimum class proportion for the latent classes based
#'                          on the estimated model
#' }
#'
#' \strong{(2) aBF: Approximate Bayes Factors}
#'
#' \itemize{
#'    \item{\code{"A-Folder"}}: Subfolder from which the group of Mplus outputs files
#'                              for Model A were summarized
#'    \item{\code{"A-#Class"}}: Number of latent classes for Model A, i.e., \code{CLASSES ARE c(#Class)}
#'    \item{\code{"A-BIC"}}: Bayesian information criterion for Model A, i.e., \code{Bayesian (BIC)}
#'    \item{\code{"B-Folder"}}: Subfolder from which the group of Mplus outputs files
#'                              for Model B were summarized
#'    \item{\code{"B-#Class"}}: Number of latent classes for Model B, i.e., \code{CLASSES ARE c(#Class)}
#'    \item{\code{"B-BIC"}}: Bayesian information criterion for Model B, i.e., \code{Bayesian (BIC)}
#'    \item{\code{"aBF"}}: Approximate Bayes Factor for pairwise comparison of relative fit
#'                         between Model A and Model B, i.e., ratio of the probability of
#'                         Model A being correct model to Model B being the correct model
#' }
#'
#' \strong{(3) Classif: Classification Diagnostics}
#'
#' \itemize{
#'    \item{\code{"Folder"}}: Subfolder from which the group of Mplus outputs files
#'                            were summarized
#'    \item{\code{"#Class"}}: Number of latent classes, i.e., \code{CLASSES ARE c(#Class)}.
#'    \item{\code{"Conv"}}: Model converged, \code{TRUE} or \code{FALSE}, i.e.,
#'                          \code{THE MODEL ESTIMATION TERMINATED NORMALLY}.
#'    \item{\code{"#Param"}}: Number of estimated parameters, i.e.,
#'                            \code{Number of Free Parameters}
#'    \item{\code{"LLRep"}}: Best log-likelihood replicated, \code{TRUE} or \code{FALSE},
#'                           i.e., \code{THE BEST LOGLIKELIHOOD VALUE HAS BEEN REPLICATED}
#'    \item{\code{"n1"}}: Class count for the first latent class based on the estimated model,
#'                        i.e., \code{FINAL CLASS COUNTS AND PROPORTIONS}
#'    \item{\code{"n2"}}: Class count for the second latent class based on the estimated model,
#'                        i.e., \code{FINAL CLASS COUNTS AND PROPORTIONS}
#'    \item{\code{"p1"}}: Class proportion of the first class based on the estimated
#'                        posterior probabilities, i.e., \code{FINAL CLASS COUNTS AND PROPORTIONS}
#'    \item{\code{"p2"}}: Class proportion of the second class based on the estimated
#'                        posterior probabilities, i.e., \code{FINAL CLASS COUNTS AND PROPORTIONS}
#'    \item{\code{"Entropy"}}: Summary of the class probabilities across classes
#'                             and individuals in the sample, i.e., \code{Entropy}
#'    \item{\code{"aPP1"}}: Average posterior class probability (AvePP) of the
#'                          first latent class for the latent classes
#'    \item{\code{"aPP2"}}: Average posterior class probability (AvePP) of the
#'                          second latent class for the latent classes
#'    \item{\code{"OCC1"}}: Odds of correct classification ratio (OCC) of the
#'                          first latent class
#'    \item{\code{"OCC2"}}: Odds of correct classification ratio (OCC) of the
#'                          second latent class
#' }
#'
#' \strong{(4) Mean_Var: Means and Variances for each Latent Class Separately}
#'
#' \itemize{
#'    \item{\code{"Folder"}}: Subfolder from which the group of Mplus outputs files
#'                            were summarized
#'    \item{\code{"#Class"}}: Number of latent classes, i.e., \code{CLASSES ARE c(#Class)}
#'    \item{\code{"n"}}: Class counts based on the estimated model,
#'                       i.e., \code{FINAL CLASS COUNTS AND PROPORTIONS}
#'    \item{\code{"Param"}}: Parameter, i.e., mean or variance
#'    \item{\code{"Ind"}}: Latent class indicator variable
#'    \item{\code{"Est."}}: Parameter estimate.
#'    \item{\code{"SE"}}: Standard error
#'    \item{\code{"z"}}: Test statistic
#'    \item{\code{"pval"}}: Significance value
#'    \item{\code{"Low"}}: Lower bound of the confidence interval
#'    \item{\code{"Upp"}}: Upper bound of the confidence interval
#' }
#'
#' \strong{(5) d: Cohen's d}
#'
#' \itemize{
#'    \item{\code{"Folder"}}: Subfolder from which the group of Mplus outputs files were summarized
#'    \item{\code{"#Class"}}: Number of latent classes, i.e., \code{CLASSES ARE c(#Class)}
#'    \item{\code{"Ind"}}: Latent class indicator variable
#'    \item{\code{"Class.j"}}: Number of classes for model \emph{j}
#'    \item{\code{"Class.k"}}: Number of classes for model \emph{k}
#'    \item{\code{"n.j"}}: Latent classes \emph{j}
#'    \item{\code{"M.j"}}: Class-specific mean of the indicator for the latent class \emph{j}
#'    \item{\code{"SD.j"}}: Class-specific standard deviation of the indicator for the latent class \emph{j}
#'    \item{\code{"n.k"}}: Latent classes \emph{k}
#'    \item{\code{"M.k"}}: Class-specific mean of the indicator for the latent class \emph{k}
#'    \item{\code{"SD.k"}}: Class-specific standard deviation of the indicator for the latent class \emph{k}
#'    \item{\code{"d"}}: Cohen's d, Standardized mean difference
#' }
#'
#' For more info on fit indices, classification diagnostics, and evaluating class
#' separation see Masyn (2013) and Sorgente et al. (2025).
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{mplus.lca}}, \code{\link{mplus.run}}, \code{\link{read.mplus}},
#' \code{\link{write.mplus}}
#'
#' @references
#' Banfield, J. D., & Raftery, A E. (1993). Model-based Gaussian and non-Gaussian
#' clustering. \emph{Biometrics, 49}, 803-821.
#'
#' Masyn, K. E. (2013). Latent class analysis and finite mixture modeling. In T. D.
#' Little (Ed.), \emph{The Oxford handbook of quantitative methods: Statistical analysis}
#' (pp. 551–611). Oxford University Press.
#'
#' Muthen, L. K., & Muthen, B. O. (1998-2017). \emph{Mplus User's Guide} (8th ed.).
#' Muthen & Muthen.
#'
#' Schwartz, G. (1978). Estimating the dimension of a model. \emph{The Annals of Statistics, 6},
#' 461-464.
#'
#' Sorgente, A., Caliciuri, R., Robba, M., Lanz, M., & Zumbo, B. D. (2025) A systematic
#' review of latent class analysis in psychology: Examining the gap between guidelines
#' and research practice. \emph{Behavior Research Methods, 57}(11), 301.
#' https://doi.org/10.3758/s13428-025-02812-1
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#'
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{output}}{list with all Mplus outputs}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{result}}{list with result tables, i.e., \code{summary} for the
#'                      model summaries, \code{bf} for approximate Bayes factors,
#'                      \code{classif} classification diagnostics,
#'                      \code{mean_var} for class-specific means and variances
#'                      of the indicator variables, \code{prob} for class-specific
#'                      item response probabilities of the indicator variables
#'                      and \code{d} for Cohen's d standardized mean difference
#'                      between latent class \emph{j} and latent class \emph{k}}}
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
#'           mplus.run = TRUE)
#'
#' # Example 1a: Read Mplus output files, create result table, write table, and save plots
#' mplus.lca.summa(write = "Results_LCA.xlsx", plot = TRUE)
#'
#' # Example 1b: Write results into a text file
#' mplus.lca.summa(write = "Results_LCA.txt")
#'
#' #----------------------------------------------------------------------------
#' # Example 2: Draw bar chart manually
#'
#' library(ggplot2)
#'
#' # Collect LCA results
#' lca.result <- mplus.lca.summa()
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
mplus.lca.summa <- function(folder = getwd(), exclude = NULL, sort.n = TRUE, sort.p = FALSE,
                            digits = 0, p.digits = 3, bf.trunc = TRUE, conf.level = 0.95,
                            plot = FALSE, group.ind = TRUE, ci = TRUE,
                            axis.title = 9, axis.text = 9, levels = NULL, labels = NULL,
                            ylim = NULL, ylab = c("Mean Value", "Item Response Probability"),
                            breaks = ggplot2::waiver(), errorbar.width = 0.1,
                            legend.title = 9, legend.text = 9, legend.key.size = 0.5,
                            gray = FALSE, start = 0.15, end = 0.85, dpi = 600,
                            width.ind = NULL, width.nclass = NULL, height.categ = NULL,
                            height = NA, write = NULL, append = TRUE, check = TRUE,
                            output = TRUE) {

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("sort.p", "bf.trunc", "plot", "group.ind", "ci", "gray", "append", "output"),
               numeric = list(axis.title = 1L, axis.text = 1L, errorbar.width = 1L, legend.title = 1L, legend.text = 1L, legend.key.size = 1L, dpi = 1L, width.ind = 1L, width.nclass = 1L,height.categ = 1L, ylim = 2L),
               character = c("conf.level", "write2"), args = c("start", "end"), envir = environment(), input.check = check)

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  occmin <- ind <- est <- low <- upp <- categ <- NULL

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Read Mplus Output Files ####

  #--------------------------------------
  ### Subfolders ####

  # Exclude subfolders

  subfolder <- misty::chr.omit(list.dirs(folder, full.names = TRUE, recursive = FALSE), omit = file.path(folder, "_Plots"), check = FALSE)|>
                 (\(p) if (isTRUE(!is.null(exclude))) { p[-sapply(exclude, function(y) grep(y, p))] } else { return(p) })() |>
                 (\(q) if (length(q) == 0L) { return(folder)} else { return(q) })()

  # Mplus output files in the subfolders
  subfolder.out <- lapply(sapply(subfolder, list), list.files, pattern = ".out") |> (\(p) if (isTRUE(length(unlist(p)) == 0L)) { stop("No Mplus output files found in the subfolders specified in the argument 'folder'.", call. = FALSE) } else { return(p) })()

  #--------------------------------------
  ### Subfolders with Mplus Outputs ####

  # Exclude folders without any Mplus outputs
  lca.folder <- subfolder[which(sapply(subfolder.out, length) > 0L)]

  #--------------------------------------
  ### Read Mplus Outputs ####

  # Read outputs, iconv() removes Non-ASCII characters
  lca.out <- suppressWarnings(lapply(lca.folder, function(y) sapply(file.path(y, grep(".out", list.files(y), value = TRUE, useBytes = TRUE)), function(z) list(iconv(readLines(z),  sub = "")))))

  # Check if all outputs are MIXTURE and exclude outputs
  suppressWarnings(names(which(unlist(lapply(lca.out, function(y) lapply(y, function(z) !any(grepl("MIXTURE", z, ignore.case = TRUE, useBytes = TRUE)))))))) |>
    (\(p) if (isTRUE(length(p) != 0L)) {

      lca.out <<- suppressWarnings(lapply(lca.folder, function(y) sapply(misty::chr.omit(file.path(y, grep(".out", list.files(y), value = TRUE, useBytes = TRUE)), omit = p, check = FALSE), function(z) list(iconv(readLines(z), sub = ""))))) |>
        (\(q) if (is.null(unlist(q))) { stop("There are no Mplus MIXTURE outputs left after excluding outputs with error messages.", call. = FALSE) } else { return(q) })()

    })()

  #--------------------------------------
  ### Output Checks ####

  # Models with more than one latent class
  if (isTRUE(any(unlist(lapply(lca.out, function(y) sapply(y, function(y) as.numeric(misty::chr.trim(sub("Number of categorical latent variables", "", grep("Number of categorical latent variables", y, value = TRUE))))))) != 1L))) { stop("This function does not support mixture models with more than one latent class.", call. = FALSE) }

  # Outputs are based on multiple imputation
  suppressWarnings(names(which(unlist(lapply(lca.out, function(y) lapply(y, function(z) any(grepl("IMPUTATION", z, ignore.case = TRUE, useBytes = TRUE)))))))) |>
    (\(p) if (isTRUE(length(p) != 0L)) { stop("This function does not support Mplus outputs for analysis based on multiply imputed data sets.", call. = FALSE) } )()

  # Outputs are based on Bayesian estimation
  suppressWarnings(names(which(unlist(lapply(lca.out, function(y) lapply(y, function(z) any(grepl("Estimator                                                    BAYES", z, ignore.case = TRUE, useBytes = TRUE)))))))) |>
    (\(p) if (isTRUE(length(p) != 0L)) { stop("This function does not support Mplus outputs for analysis based on Bayesian estimation.", call. = FALSE) } )()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Extract Results ####

  #--------------------------------------
  ### Maximum Number of Classes ####

  max.class <- max(unlist(lapply(lca.out, function(y) sapply(y, function(y) {

    classes <- unlist(strsplit(grep("CLASSES ", y, value = TRUE, ignore.case = TRUE, useBytes = TRUE), ""))

    as.numeric(misty::chr.trim(paste(classes[(grep("\\(", classes, useBytes = TRUE) + 1L):(grep("\\)", classes, useBytes = TRUE) - 1L)], collapse = "")))

  }))))

  #--------------------------------------
  ### Apply .extract.lca.result() Function ####

  lca.summary <- lca.bf <- lca.class <- lca.mean.var <- lca.prob <- lca.d <- NULL

  #...................
  #### Model Summary ####

  for (i in seq_along(lca.out)) { lca.summary <- misty::df.rbind(lca.summary, data.frame(folder = sapply(strsplit(lca.folder, "/"), function(y) rev(y)[1L])[i], do.call(misty::df.rbind, lapply(lca.out[[i]], function(z) .extract.lca.result(z, max.class = max.class, conf.level = conf.level, return = "model.summary")$model.summary)))) }

  ##### Approximate correct model probability (cmP) ####

  lca.summary <- misty::df.move(data.frame(lca.summary, cmp = (-0.5*lca.summary$bic) |> (\(p) exp(p - max(p, na.rm =TRUE)) / sum(exp(na.omit(p) - max(p, na.rm =TRUE))))()), "cmp", after = "awe")

  ##### Sort table according to the number of classes ####

  if (isTRUE(sort.n)) { lca.summary <- lca.summary[order(lca.summary$folder, lca.summary$nclass), ] }

  ##### Exclude NA columns ####

  lca.summary <- lca.summary[, sapply(lca.summary, function(y) any(!is.na(y)))]

  #...................
  #### Approximate Bayes Factor ####

  lca.bf <- lca.summary |> (\(p) p[!is.na(p[, "bic"]), ])() |> (\(q) do.call("rbind", apply(combn(nrow(q), 2L), 2L, function(y) q[y, ] |> (\(r) data.frame(A.folder = r[1L, "folder"], A.nclass = r[1L, "nclass"], A.bic = r[1L, "bic"], B.folder = r[2L, "folder"], B.nclass = r[2L, "nclass"], B.bic = r[2L, "bic"], bf = exp(-0.5*r[1L, "bic"] - -0.5*r[2L, "bic"])))())))()

  #...................
  #### Classification Diagnostics ####

  for (i in seq_along(lca.out)) { lca.class <- misty::df.rbind(lca.class, data.frame(folder = sapply(strsplit(lca.folder, "/"), function(y) rev(y)[1L])[i], do.call(misty::df.rbind, lapply(lca.out[[i]], function(z) .extract.lca.result(z, max.class = max.class, conf.level = conf.level, return = "model.class")$model.class)))) }

  ##### Sort table according to the number of classes ####

  if (isTRUE(sort.n)) { lca.class <- lca.class[order(lca.class$folder, lca.class$nclass), ] }

  ##### Sort class sizes, proportions, AVEs, and OCCs ####

  if (isTRUE(sort.p)) {

    if (isTRUE(sum(substr(colnames(lca.class), 1L, 1L) == "p") > 1L)) {

      n  <- which(substr(colnames(lca.class), 1L, 1L) == "n" & !colnames(lca.class) %in% c("nclass", "nparam"))
      pi <- which(substr(colnames(lca.class), 1L, 1L) == "p")
      a  <- which(substr(colnames(lca.class), 1L, 3L) == "ave")
      o  <- which(substr(colnames(lca.class), 1L, 3L) == "occ")
      for (i in seq_len(nrow(lca.class))) {

        order(unlist(lca.class[i, n]), decreasing = TRUE, na.last = TRUE) |>
          (\(p) {

            lca.class[i, n]  <<- lca.class[i, n][p]
            lca.class[i, pi] <<- lca.class[i, pi][p]
            lca.class[i, a]  <<- lca.class[i, a][p]
            lca.class[i, o]  <<- lca.class[i, o][p]

          })()

      }

    }

  }

  ##### Add OCC to Model summary ####

  lca.summary <- misty::df.move(data.frame(lca.summary, occmin = apply(lca.class[, grep("occ", colnames(lca.class))], 1L, function(y) if (isTRUE(!all(is.na(y)))) { min(y, na.rm = TRUE) } else { NA })), occmin, after = "avemin")

  ##### Remove One-Class Solutions ####

  if (isTRUE(any(lca.class$nclass == 1L))) { lca.class <- lca.class[which(lca.class$nclass != 1L), ] }

  #...................
  #### Means and Variances ####

  misty::chr.trim(unlist(lca.out)) |>
    (\(p) if (isTRUE(!"Binary and ordered categorical (ordinal)" %in% p && !"Unordered categorical (nominal)" %in% p)) {

      for (i in seq_along(lca.out)) {

        lca.mean.var <<- lapply(lca.out[[i]], function(z) .extract.lca.result(z, max.class = max.class, conf.level = conf.level, return = "model.descript")$model.descript) |> (\(q) if (isTRUE(any(!is.na(q)))) { misty::df.rbind(lca.mean.var, data.frame(folder = sapply(strsplit(lca.folder, "/"), function(y) rev(y)[1L])[i], do.call(misty::df.rbind, q))) } else { return(NULL) })()

      } |> (\(r) if (isTRUE(any(!is.na(r)))) { data.frame(r[order(r$folder, r$nclass, r$class), ], row.names = NULL) })()

    })()

  #...................
  #### Probabilities ####

  misty::chr.trim(unlist(lca.out)) |>
    (\(p) if (isTRUE("Binary and ordered categorical (ordinal)" %in% p || "Unordered categorical (nominal)" %in% p)) {

      for (i in seq_along(lca.out)) {

        lca.prob <<- lapply(lca.out[[i]], function(z) .extract.lca.result(z, max.class = max.class, conf.level = conf.level, return = "model.descript")$model.descript) |>
          (\(q) if (isTRUE(any(!is.na(q)))) { data.frame(folder = sapply(strsplit(lca.folder, "/"), function(y) rev(y)[1L])[i], do.call(misty::df.rbind, q)) } else { return(NULL) })()

      } |> (\(r) if (isTRUE(any(!is.na(r)))) { data.frame(r[order(r$folder, r$nclass, r$class), ], row.names = NULL) })()

    })()

  #...................
  #### Cohen's d ####

  if (isTRUE(!is.null(lca.mean.var) && any(lca.mean.var$param == "Variance"))) {

    # Means and variances
    lca.mean <- lca.mean.var[which(lca.mean.var$param == "Mean"), ]
    lca.var <- lca.mean.var[which(lca.mean.var$param == "Variance"), ]

    # Remove one-class solutions
    lca.mean.sepa <- lca.mean[which(lca.mean$nclass != 1L), ] |> (\(p) data.frame(folder_nclass_ind = apply(p[, c("folder", "nclass", "ind")], 1L, paste, collapse = "_"), p))()
    lca.var.sepa  <- lca.var[which(lca.var$nclass != 1L), ] |> (\(p) data.frame(folder_nclass_ind = apply(p[, c("folder", "nclass", "ind")], 1L, paste, collapse = "_"), p))()

    for (i in unique(lca.mean.sepa$folder_nclass_ind)) {

      list(m = lca.mean.sepa[lca.mean.sepa$folder_nclass_ind == i, ], v = lca.var.sepa[lca.var.sepa$folder_nclass_ind == i, ]) |>
        (\(p) {

          lca.d <<- rbind(lca.d, do.call("rbind", apply(combn(unique(p$m$nclass), m = 2L), 2L, function(y) {

            # Cohen's d
            data.frame(folder = unique(p$m$folder), nclass = p$m[y[1L], "nclass"], ind = p$m[y[1L], "ind"], class.j = as.numeric(p$m[y[1L], "class"]), class.k = as.numeric(p$m[y[2L], "class"]),
                       n.j = p$m[y[1L], "n"], m.j = p$m[y[1L], "est"], sd.j = sqrt(p$v[y[1L], "est"]), n.k = p$m[y[2L], "n"], m.k = p$m[y[2L], "est"], sd.k = sqrt(p$v[y[2L], "est"]),
                       d = if (isTRUE(p$m[y[1L], "n"] >= 1L && p$m[y[2L], "n"] >= 1)) { (p$m[p$m$class == y[1L], "est"] - p$m[p$m$class == y[2L], "est"]) / sqrt(((p$m[p$m$class == y[1L], "n"] - 1L)*p$v[p$v$class == y[1L], "est"] + (p$m[p$m$class == y[2L], "n"] - 1L)*p$v[p$v$class == y[2L], "est"]) / (p$m[p$m$class == y[1L], "n"] + p$m[p$m$class == y[2L], "n"] - 2L)) } else { NA })

          })))

        })()

    }

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "mplus.lca.summa",
                 output = lca.out,
                 args = list(folder = folder, exclude = exclude, sort.n = sort.n, sort.p = sort.p, digits = digits, p.digits = p.digits, bf.trunc = bf.trunc, conf.level = conf.level,
                             plot = plot, ci = ci, axis.title = axis.title, axis.text = axis.text, levels = levels, labels = labels, ylim = ylim, ylab = ylab, breaks = breaks, errorbar.width = errorbar.width, legend.title = legend.title, legend.text = legend.text, legend.key.size = legend.key.size, gray = gray, start = start, end = end, dpi = dpi, width.ind = width.ind, width.nclass = width.nclass, height.categ = height.categ, height = height,
                             write = write, append = append, check = check),
                 result = list(summary = lca.summary, bf = lca.bf, classif = lca.class, mean_var = lca.mean.var, prob = lca.prob, d = lca.d))

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Plot Object ----------------------------------------------------------------

  if (isTRUE(plot && (!is.null(lca.mean.var) || !is.null(lca.prob)))) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Continuous or Count Indicators ####

    if (isTRUE(!is.null(lca.mean.var))) {

      #--------------------------------------
      ### Extract Mean ####

      lca.mean <- lca.mean.var[which(lca.mean.var$param == "Mean"), ]

      #--------------------------------------
      ### Arguments ####

      # y Label
      if (isTRUE(all(c("Mean Value", "Item Response Probability") %in% ylab))) { ylab <- "Mean Value" }

      # width
      if (isTRUE(is.null(width.nclass))) { width.ind <- 1.5 }
      if (isTRUE(is.null(width.nclass))) { width.nclass <- 0.5 }

      # Add label 'Class'
      if (isTRUE(group.ind)) { lca.mean$class <- paste0("Class ", lca.mean$class)  }

      # Levels and Labels
      lca.mean$ind <- factor(lca.mean$ind,
                             levels = if (isTRUE(is.null(levels))) { unique(lca.mean$ind) } else { levels },
                             labels = if (isTRUE(is.null(levels) && is.null(labels))) {

                               unique(lca.mean$ind)

                             } else if (isTRUE(is.null(levels) && !is.null(labels))) {

                               labels

                             } else if (isTRUE(!is.null(levels) && is.null(labels))) {

                               levels

                             })

      #--------------------------------------
      ### Messages ####

      if (isTRUE(length(unique(paste0(lca.mean$folder, lca.mean$nclass))) > 10L)) { message(paste0("R is making ", length(unique(paste0(lca.mean$folder, lca.mean$nclass))), " plots in ", length(unique(lca.mean$folder)), ifelse(length(unique(lca.mean$folder)) == 1L, " folder", " folders" ), ", this may take a while.")) }

      #--------------------------------------
      ### Create Folder ####

      if (isTRUE(length(unique(lca.mean$folder)) == 1L)) {

        suppressWarnings(invisible(dir.create(file.path(folder, "_Plots"))))

      } else {

        sapply(unique(lca.mean$folder), function(y) suppressWarnings(invisible(dir.create(file.path(folder, y, "_Plots")))))

      }

      #--------------------------------------
      ### ggplot Theme ####

      ggplot2::theme_set(ggplot2::theme_bw())

      #--------------------------------------
      ### Loop across Folders ####

      for (i in unique(lca.mean$folder)) {

        # Select data
        temp1 <- lca.mean[lca.mean$folder == i, ]

        #### Loop across number of latent classes ####

        for (j in unique(temp1$nclass)) {

          ##### Select data
          temp2 <- temp1[temp1$nclass == j, ]

          ##### Limits
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

          ##### Breaks
          if (isTRUE(inherits(breaks, "waiver"))) {

            if (isTRUE(abs(limits.upp - limits.low) > 5L)) {

              breaks.j <- seq(limits.low, limits.upp, by = 1L)

            } else {

              breaks.j <- seq(limits.low, limits.upp, by = 0.5)

            }

          } else {

            breaks.j <- breaks

          }

          ##### Plot
          p <- ggplot2::ggplot(temp2, if (isTRUE(group.ind)) { ggplot2::aes(class, est, group = ind, fill = ind) } else { ggplot2::aes(ind, est, group = class, fill = class) }) +
                ggplot2::geom_bar(stat = "identity", position = "dodge", color = "black", linewidth = 0.1) +
                ggplot2::scale_x_discrete("") +
                ggplot2::scale_y_continuous(ylab, limits = c(limits.low, limits.upp), breaks = breaks.j) +
                ggplot2::labs(fill = if (isTRUE(!group.ind)) { "Latent Class" } else { "" }) +
                ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1L)) +
                ggplot2::theme(axis.title = ggplot2::element_text(size = axis.title),
                               axis.text = ggplot2::element_text(size = axis.text),
                               legend.position = "bottom",
                               legend.key.size = ggplot2::unit(legend.key.size , 'cm'),
                               legend.title = ggplot2::element_text(size = legend.title),
                               legend.text = ggplot2::element_text(size = legend.text),
                               legend.box.spacing = ggplot2::unit(-9L, "pt"))

          ##### Confidence intervals
          if (isTRUE(ci)) { p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = low, ymax = upp), width = errorbar.width + j / 30L, linewidth = 0.2, position = ggplot2::position_dodge(0.9)) }

          ##### Gray color scales
          if (isTRUE(gray)) { p <- p + ggplot2::scale_fill_grey(start = end, end = start) }

          ##### Argument 'width'
          width.ij <- (length(unique(temp2$ind)) * width.ind) + (unique(temp2$nclass) * width.nclass)

          ##### Save plots in one folder
          if (isTRUE(length(unique(lca.mean$folder)) == 1L)) {

            suppressMessages(ggplot2::ggsave(file.path(folder, "_Plots", paste0("Bar_Chart_", j, "-Class.png")), dpi = dpi, width = width.ij, height = height))

          ##### Save plots in multiple folder
          } else {

            suppressMessages(ggplot2::ggsave(file.path(folder, i, "_Plots", paste0("Bar_Chart_", j, "-Class.png")), dpi = dpi, width = width.ij, height = height))

          }

        }

      }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Ordered or Unordered Categorical Indicators ####

    } else if (isTRUE(!is.null(lca.prob))) {

      #--------------------------------------
      ### Arguments ####

      # y Label
      if (isTRUE(all(c("Mean Value", "Item Response Probability") %in% ylab))) { ylab <- "Item Response Probability" }

      # width and height
      if (isTRUE(is.null(width.nclass))) { width.ind <- 1.5 }
      if (isTRUE(is.null(width.nclass))) { width.nclass <- 1.5 }
      if (isTRUE(is.null(height.categ))) { height.categ <- 0.6 }

      # Category
      lca.prob$categ <- factor(lca.prob$categ, ordered = TRUE, levels = rev(unique(lca.prob$categ)))

      # Add label 'Class'
      lca.prob$class <- paste0("Class ", lca.prob$class)

      # Limits
      if (isTRUE(is.null(ylim))) {

        limits.low <- 0L
        limits.upp <- 1L

      } else {

        limits.low <- ylim[1L]
        limits.upp <- ylim[2L]

      }

      # Breaks
      if (isTRUE(inherits(breaks, "waiver"))) { breaks.j <- seq(0L, 1L, by = 0.20) } else { breaks.j <- breaks }

      # Levels and Labels
      lca.prob$ind <- factor(lca.prob$ind,
                             levels = if (isTRUE(is.null(levels))) { unique(lca.prob$ind) } else { levels },
                             labels = if (isTRUE(is.null(levels) && is.null(labels))) {

                               unique(lca.prob$ind)

                             } else if (isTRUE(is.null(levels) && !is.null(labels))) {

                               labels

                             } else if (isTRUE(!is.null(levels) && is.null(labels))) {

                               levels

                             })

      #--------------------------------------
      ### Messages ####

      if (isTRUE(length(unique(paste0(lca.prob$folder, lca.prob$nclass))) > 10L)) {       message(paste0("R is making ", length(unique(paste0(lca.prob$folder, lca.prob$nclass))), " plots in ", length(unique(lca.prob$folder)), ifelse(length(unique(lca.prob$folder)) == 1L, " folder", " folders" ), ", this may take a while.")) }

      #--------------------------------------
      ### Create Folder ####

      if (isTRUE(length(unique(lca.prob$folder)) == 1L)) {

        suppressWarnings(invisible(dir.create(file.path(folder, "_Plots"))))

      } else {

        sapply(unique(lca.prob$folder), function(y) suppressWarnings(invisible(dir.create(file.path(folder, y, "_Plots")))))

      }

      #--------------------------------------
      ### ggplot Theme ####

      ggplot2::theme_set(ggplot2::theme_bw())

      #--------------------------------------
      ### Loop across Folders ####

      for (i in unique(lca.prob$folder)) {

        # Select data
        temp1 <- lca.prob[lca.prob$folder == i, ]

        #### Loop across number of latent classes ####

        for (j in unique(temp1$nclass)) {

          ##### Select data
          temp2 <- temp1[temp1$nclass == j, ]

          ##### Plot
          p <- ggplot2::ggplot(temp2, if (isTRUE(group.ind)) { ggplot2::aes(ind, est, group = categ, fill = categ) } else { ggplot2::aes(class, est, group = categ, fill = categ) }) +
            ggplot2::geom_bar(stat = "identity", position = "fill", color = "black", linewidth = 0.1) +
            ggplot2::facet_wrap(if (isTRUE(group.ind)) { ~ class } else { ~ ind }) +
            ggplot2::scale_x_discrete("") +
            ggplot2::scale_y_continuous(ylab, limits = c(limits.low, limits.upp), breaks = breaks.j) +
            ggplot2::labs(fill = "Category") +
            ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1L)) +
            ggplot2::theme(axis.title = ggplot2::element_text(size = axis.title),
                           axis.text = ggplot2::element_text(size = axis.text),
                           legend.position = "bottom",
                           legend.key.size = ggplot2::unit(legend.key.size , 'cm'),
                           legend.title = ggplot2::element_text(size = legend.title),
                           legend.text = ggplot2::element_text(size = legend.text),
                           legend.box.spacing = ggplot2::unit(-9L, "pt"))

          ##### Gray color scales
          if (isTRUE(gray)) { p <- p + ggplot2::scale_fill_grey(start = end, end = start) }

          ##### Argument 'width' and 'height'
          if (isTRUE(group.ind)) {

            width.ij <- (length(unique(temp2$ind)) * width.ind) * ggplot2::wrap_dims(unique(temp2$nclass))[2L]
            height.ij <- (max(as.numeric(unique(temp2$categ))) * height.categ ) * ggplot2::wrap_dims((length(unique(temp2$ind))))[1L]

          } else {

            width.ij <- (unique(temp2$nclass) * width.nclass) * ggplot2::wrap_dims((length(unique(temp2$ind))))[2L]
            height.ij <- (max(as.numeric(unique(temp2$categ))) * height.categ ) * ggplot2::wrap_dims((length(unique(temp2$ind))))[1L]

          }

          ##### Save plots in one folder
          if (isTRUE(length(unique(lca.prob$folder)) == 1L)) {

            suppressMessages(ggplot2::ggsave(file.path(folder, "_Plots", paste0("Bar_Chart_", j, "-Class.png")), dpi = dpi, width = width.ij, height = height.ij))

          ##### Save plots in multiple folder
          } else {

            suppressMessages(ggplot2::ggsave(file.path(folder, i, "_Plots", paste0("Bar_Chart_", j, "-Class.png")), dpi = dpi, width = width.ij, height = height.ij))

          }

        }

      }

    }

  }

  #_____________________________________________________________________________
  #
  # Write Results --------------------------------------------------------------

  if (isTRUE(!is.null(write))) { .write.result(object = object, write = write, append = append) }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}

#_______________________________________________________________________________
