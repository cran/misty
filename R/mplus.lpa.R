#' Mplus Model Specification for Latent Profile Analysis
#'
#' This function writes Mplus input files for conducting latent profile analysis
#' (LPA) based on six different variance-covariance structures. By default, the
#' function creates folders in the current working directory for each of the six
#' sets of analysis, writes Mplus input files for conducting LPA with \emph{k} = 1
#' to \emph{k} = 6 profiles into these folders, and writes the matrix or data frame
#' specified in \code{x} into a Mplus data file in the current working directory.
#' Optionally, all models can be estimated by setting the argument \code{run.mplus}
#' to \code{TRUE}.
#'
#' @param x            a matrix or data frame. Note that all variable names must
#'                     be no longer than 8 character.
#' @param ind          a character vector indicating the variables names of the
#'                     latent profile indicators in \code{x}.
#' @param cluster      a character string indicating the cluster variable in the
#'                     matrix or data frame specified in \code{x} representing the
#'                     nested grouping structure for computing cluster-robust
#'                     standard errors. Note that specifying a cluster variables
#'                     does not have any effect on the information criteria, but
#'                     for the Vuong-Lo-Mendell-Rubin likelihood ratio test of model
#'                     fit.
#' @param folder       a character vector with six character strings for specifying
#'                     the names of the six folder representing different
#'                     variance-covariance structures.
#' @param file         a character string naming the Mplus file with or without
#'                     the file extension '.dat', e.g., \code{"Data_LPA.dat"} (default)
#'                     or \code{"Data_LPA"}.
#' @param write        a character string or character vector indicating whether
#'                     to create the six folders specified in the argument \code{folder}
#'                     (\code{"folder"}), to write the matrix or data frame specified
#'                     in \code{x} into a Mplus data file (\code{"data"}), and
#'                     write the Mplus input files into the six folders specified
#'                     in the argument \code{folder}  (\code{"input"}). By default,
#'                     the function creates the folders, writes the Mplus data file,
#'                     and writes the Mplus input files into the folders.
#' @param missing      a numeric value or character string representing missing
#'                     values (\code{NA}) in the Mplus data set. This values or
#'                     character string will be specified in the Mplus input file
#'                     as \code{MISSING IS ALL(missing)}. By default, \code{-99}
#'                     is used to represent missing values.
#' @param classes      an integer value specifying the maximum number of profiles
#'                     for the latent profile analysis. By default, a LPA with a
#'                     maximum of 6 profiles are specified (i.e., \emph{k} = 1
#'                     to \emph{k} = 6).
#' @param estimator    a character string for specifying the \code{ESTIMATOR}
#'                     option in Mplus. By default, the estimator \code{"MLR"} is
#'                     used.
#' @param starts       a vector with two integer values for specifying the
#'                     \code{STARTS} option in Mplus. The first number represents
#'                     the number of random sets of starting values to generate
#'                     in the initial stage and the second number represents the
#'                     optimizations to use in the final stage. By default, 500
#'                     random sets of starting values are generated and 100
#'                     optimizations are carried out in the final stage.
#' @param stiterations an integer value specifying the \code{STITERATIONS} option
#'                     in Mplus. The numeric value represents the maximum number
#'                     of iterations allowed in the initial stage. By default,
#'                     50 iterations are requested.
#' @param lrtbootstrap an integer value for specifying the \code{LRTBOOTSTRAP} option
#'                     in Mplus when reqeuesting a parametric bootstrapped likelihood
#'                     ratio test (i.e., \code{output = "TECH14"}). The value
#'                     represents the number of bootstrap draws to be used in
#'                     estimating the \emph{p}-value of the parametric bootstrapped
#'                     likelihood ratio test. By default, 1000 bootstrap draws are
#'                     requested.
#' @param lrtstarts    a vector with four integer values for specifying the
#'                     \code{LRTSTARTS} option in Mplus when reqeuesting a parametric
#'                     bootstrapped likelihood ratio test (i.e., \code{output = "TECH14"}).
#'                     The values specify the number of starting values to use in
#'                     the initial stage and the number of optimizations to use
#'                     in the final stage for the \code{k - 1} and \code{k} profile
#'                     model when the data generated by bootstrap draws are analyzed.
#'                     By default, 0 random sets of starting values in the initial
#'                     stage and 0 optimizations in the final stage are used for
#'                     the \code{k - 1} profile model and 100 random sets of starting
#'                     values in the initial stage and 50 optimizations in the
#'                     final stage are used for the \code{k} profile model.
#' @param processors   an integer value for specifying the \code{PROCESSORS} option
#'                     in Mplus. The value specifies the number of processors
#'                     and threads to be used for parallel computing to increase
#'                     computational speed. By default, 8 processors and threads
#'                     are used for parallel computing.
#' @param output       a character string or character vector specifying the \code{TECH}
#'                     options in the \code{OUTPUT} section in Mplus, i.e., code{SVALUES}
#'                     to request input statements that contain parameter estimates
#'                     from the analysis, \code{TECH7} to request sample statistics
#'                     for each profile using raw data weighted by the estimated
#'                     posterior probabilities for each profile, \code{TECH8} to
#'                     request the optimization history in estimating the model,
#'                     \code{TECH11} to request the Lo-Mendell-Rubin likelihood
#'                     ratio test of model fit, and \code{TECH14} to
#'                     request a parametric bootstrapped likelihood ratio test.
#'                     By default, \code{SVALUES} and \code{TECH11} are requested.
#'                     Note that \code{TECH11} is only available for the \code{MLR}
#'                     estimator.
#' @param run.mplus    logical: if \code{TRUE}, all models in the folders specified
#'                     in the argument \code{folder} are estimated by using the
#'                     \code{run.mplus} function in the R package \code{misty}.
#' @param Mplus        a character string for specifying the name or path of the
#'                     Mplus executable to be used for running models. This covers
#'                     situations where Mplus is not in the system's path, or where
#'                     one wants to test different versions of the Mplus program.
#'                     Note that there is no need to specify this argument for most
#'                     users since it has intelligent defaults.
#' @param replace      a character string for specifying three settings:
#'                     \code{"always"} (default), which runs all models, regardless
#'                     of whether an output file for the model exists, \code{"never"},
#'                     which does not run any model that has an existing output file,
#'                     and \code{"modifiedDate"}, which only runs a model if the
#'                     modified date for the input file is more recent than the
#'                     output file modified date.
#' @param check        logical: if \code{TRUE}, argument specification is checked.
#'
#' @details
#' Latent profile analysis is a model-based clustering and classification method
#' used to identify qualitatively different classes of observations which are
#' unknown and must be inferred from the data. The within-profile variance-covariance
#' structures represt different assumptions regarding the variance and covariance
#' of the indicator variables both within and between latent profiles. As the best
#' within-profile variance-covariance structure is not known a priori, all of the
#' different structures must be tested to identify the best model (Masyn, 2013).
#' This function specifies six different six different variance-covariance structures
#' labeled A to F (see Table 1 in Patterer et al, 2023):
#' \describe{
#' \item{\strong{Model A}}{The within-profile variance is constrained to be
#' profile-invariant and covariances are constrained to be 0 in all profiles
#' (i.e., equal variances across profiles and no covariances among indicator
#' variables).}
#' \item{\strong{Model B}}{The within-profile variance profile-varying and covariances
#' are constrained to be 0 in all profiles (i.e., unequal variances across profiles
#' and no covariances among indicator variables).}
#' \item{\strong{Model C}}{The within-profile variance is constrained to be
#' profile-invariant and covariances are constrained to be equal in all profiles
#' (i.e., equal variances and covariances across profiles).}
#' \item{\strong{Model D}}{The within-profile variance is constrained to be
#' profile-invariant and covariances are profile-varying (i.e., equal variances
#' across profiles and unequal covariances across profiles).}
#' \item{\strong{Model E}}{The within-profile variances are profile-varying and
#' covariances are constrained to be equal in all profiles (i.e., unequal variances
#' across profiles and equal covariances across profiles).}
#' \item{\strong{Model F}}{The within-class variance and covariances both
#' profile-varying (i.e., unequal variances and covariances across profiles).}
#' }
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{result.lpa}}, \code{\link{run.mplus}}, \code{\link{read.mplus}},
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
#' Patterer, A. S., Yanagida, T., Kühnel, J., & Korunka, C. (2023). Daily receiving
#' and providing of social support at work: Identifying support exchange patterns
#' in hierarchical data. \emph{Journal of Work and Organizational Psychology, 32}(4),
#' 489-505. https://doi.org/10.1080/1359432X.2023.2177537
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following entries:
#'
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{x}}{matrix or data frame specified in the argument x}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{result}}{list with six entries for each of the variance-covariance
#'                      structures and the Mplus inputs based on different number
#'                      of profiles}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load data set "HolzingerSwineford1939" in the lavaan package
#' data("HolzingerSwineford1939", package = "lavaan")
#'
#' # LPA with k = 1 to k = 8 profiles
#' # Input statements that contain parameter estimates
#' # Vuong-Lo-Mendell-Rubin LRT and bootstrapped LRT
#' mplus.lpa(HolzingerSwineford1939, ind = c("x1", "x2", "x3", "x4"),
#'           classes = 8, output = c("SVALUES", "TECH11", "TECH14"))
#' }
mplus.lpa <- function(x, ind = NULL, cluster = NULL,
                      folder = c("A_Invariant-Theta_Diagonal-Sigma",
                                 "B_Varying-Theta_Diagonal-Sigma",
                                 "C_Invariant-Theta_Invariant-Unrestrictred-Sigma",
                                 "D_Invariant-Theta_Varying-Unrestricted-Sigma",
                                 "E_Varying-Theta_Invariant-Unrestricted-Sigma",
                                 "F_Varying-Theta_Varying-Unrestricted-Sigma"),
                      file = "Data_LPA.dat", write = c("all", "folder", "data", "input"),
                      missing = -99, classes = 6, estimator = "MLR", starts = c(500, 100),
                      stiterations = 50, lrtbootstrap = 1000, lrtstarts = c(0, 0, 100, 50),
                      processors = 8, output = c("all", "SVALUES", "TECH7", "TECH8", "TECH11", "TECH14"),
                      run.mplus = FALSE, Mplus = "Mplus", replace = c("always", "never", "modifiedDate"),
                      check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'x' is missing
  if (isTRUE(missing(x))) { stop("Input for the argument 'x' is missing.", call. = FALSE) }

  # Check if input 'x' is NULL
  if (isTRUE(is.null(x))) { stop("Input specified for the argument 'x' is NULL.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # Check variable names in 'x'
    if (isTRUE(any(nchar(colnames(x)) > 8L))) { stop(paste0("Variables names in the matrix or data frame specified in 'x' have more than 8 characters: ", paste0(colnames(x)[which(nchar(colnames(x)) > 8L)], collapse = ", ")), call. = FALSE) }

    # Check 'ind' in 'x'
    if (isTRUE(any(!ind %in% colnames(x)))) { stop(paste0("Variables specified in 'ind' were not all found in the matrix or data frame specified in 'x': ", paste0(ind[which(!ind %in% colnames(x))], collapse = ", ")), call. = FALSE) }

    # Check 'cluster' in 'x'
    if (isTRUE(!cluster %in% colnames(x))) { stop("Cluster variable specified in 'cluster' was not found in the matrix or data frame specified in 'x'.", call. = FALSE) }

    # Check 'folder'
    if (isTRUE(length(folder) != 6L || !is.character(folder))) { stop("Please specify a character vector with six entries for the argument 'folder'.", call. = FALSE) }

    # Check 'file'
    if (isTRUE(length(file) != 1L || !is.character(file))) { stop("Please specify a character string for the argument 'file'.", call. = FALSE) }

    # Check 'write'
    if (isTRUE(!all(write %in% c("all", "folder", "data", "input")))) { stop("Character string in the argument 'write' does not match with \"folder\", \"data\", or \"input\".", call. = FALSE) }

    # Check 'missing'
    if (isTRUE(length(missing) != 1L)) { stop("Please specify a numeric value or character string for the argument 'missing'.", call. = FALSE) }

    # Check 'classes'
    if (isTRUE(length(classes) != 1L || classes %% 1L != 0L)) { stop("Please specify a numeric value for the argument 'classes'.", call. = FALSE) }

    # Check 'starts'
    if (isTRUE(length(starts) != 2L) || any(starts %% 1L != 0L)) { stop("Please specify a vector with two integer values for the argument 'starts'.", call. = FALSE) }

    # Check 'starts'
    if (isTRUE(starts[2L] > starts[1L])) { stop("The second value must be less than or equal the first value in the argument 'starts'.", call. = FALSE) }

    # Check 'stiterations'
    if (isTRUE(length(stiterations) != 1L || stiterations %% 1L != 0L)) { stop("Please specify an integer value for the argument 'stiterations'.", call. = FALSE) }

    # Check 'lrtbootstrap'
    if (isTRUE(length(lrtbootstrap) != 1L || lrtbootstrap %% 1L != 0L)) { stop("Please specify an integer value for the argument 'lrtbootstrap'.", call. = FALSE) }

    # Check 'lrtstarts'
    if (isTRUE(length(lrtstarts) != 4L || any(lrtstarts %% 1L != 0L))) { stop("Please specify a vector with four integer values for the argument 'lrtstarts'.", call. = FALSE) }

    # Check 'lrtstarts'
    if (isTRUE(lrtstarts[2L] > lrtstarts[1L])) { stop("The second value must be less than or equal the first value in the argument 'lrtstarts'.", call. = FALSE) }

    # Check 'lrtstarts'
    if (isTRUE(lrtstarts[4L] > lrtstarts[3L])) { stop("The fourth value must be less than or equal the third value in the argument 'lrtstarts'.", call. = FALSE) }

    # Check 'processors'
    if (isTRUE(length(processors) != 1L || processors %% 1L != 0L)) { stop("Please specify an integer value for the argument 'processors'.", call. = FALSE) }

    ## Check input 'print' ##
    if (isTRUE(!all(output %in% c("all", "SVALUES", "TECH7", "TECH8", "TECH11", "TECH14")))) { stop("Character string in the argument 'output' does not match with \"SVALUES\", \"TECH7\", \"TECH8\", \"TECH11\", or \"TECH14\".", call. = FALSE) }

    ## Check input 'run.mplus' ##
    if (isTRUE(!is.logical(run.mplus))) { stop("Please specify TRUE or FALSE for the argument 'run.mplus '.", call. = FALSE) }

    ## Check input 'replace' ##
    if (isTRUE(!all(replace %in% c("always", "never", "modifiedDate")))) { stop("Character string in the argument 'replace ' does not match with \"always\", \"never\",  or \"modifiedDate\".", call. = FALSE) }

    ## Check input 'check' ##
    if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Data and Arguments ---------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Write ####

  if (isTRUE(all(c("all", "folder", "data", "input") %in% write))) {

    write <- c("folder", "data", "input")

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Output ####

  if (isTRUE(all(c("all", "SVALUES", "TECH7", "TECH8", "TECH11", "TECH14") %in% output))) {

    output <- c("SVALUES", "TECH11")

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## replaceOutfile ####

  if (isTRUE(all(c("always", "never", "modifiedDate") %in% replace))) {

    replace <- "always"

  } else {

    if (isTRUE(length(replace) != 1L)) { stop("Please specify a character string for the argument 'replace'", call. = FALSE) }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Variabla names ####

  # Split variables names in chucks of 8
  var.x <- split(colnames(x), ceiling(seq_along(colnames(x)) / 8L))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Indicator names ####

  # Number of characters
  ind.nchar <- nchar(paste(ind, collapse = " "))

  if (ind.nchar <= 70L) { ind.x <- list(ind)} else { ind.x <- split(ind, ceiling(seq_along(ind) / 7L)) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Indicator covariances ####

  # Up to 8 indicator variables
  if (isTRUE(ind.nchar <= 70L)) {

    ind.cov <- paste0("            ", paste0(c(ind, "WITH"), collapse = " "), "\n",
                      "            ", paste0(ind, collapse = " "), ";")

    # More than 8 indicator variables
  } else {

    ind.cov <- paste0("            ", apply(combn(ind, m = 2L), 2L, paste0, collapse = " WITH "), collapse = ";\n")

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Create folder ####

  if (isTRUE("folder" %in% write)) { suppressWarnings(invisible(lapply(folder, dir.create))) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Write data ####

  if (isTRUE("data" %in% write)) { write.table(x, file = file, quote = FALSE, na = as.character(missing), row.names = FALSE, col.names = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data, Variable, Analysis, and Output ####

  # Data, Variable, and Analysis
  mod.dva <- paste0("DATA:       FILE IS ../",  file, ";\n\n",
                    "VARIABLE:   NAMES ARE\n",
                    "            ",   paste0(unlist(lapply(var.x, function(y) paste0(y, collapse = " "))), collapse = "\n            "), ";\n\n",
                    "            USEVARIABLES ARE\n",
                    "            ",   paste0(unlist(lapply(ind.x, function(y) paste0(y, collapse = " "))), collapse = "\n            "), ";\n\n",
                    "            MISSING IS ALL(", missing, ");\n\n",
                    if (isTRUE(!is.null(cluster))) { paste0("            CLUSTER IS ", cluster, ";\n\n") },
                    "            CLASSES ARE c(_classes_);\n\n",
                    if (isTRUE(!is.null(cluster))) { "ANALYSIS:   TYPE IS MIXTURE COMPLEX;\n" } else { "ANALYSIS:   TYPE IS MIXTURE;\n" },
                    "            ESTIMATOR IS ", estimator, ";\n",
                    "            STARTS ARE ", paste0(starts, collapse = " "), ";\n",
                    "            STITERATIONS ARE ", stiterations, ";\n",
                    if (isTRUE("TECH14" %in% output)) { paste0("            LRTBOOTSTRAP IS ", lrtbootstrap, ";\n",
                                                               "            LRTSTARTS ARE ", paste0(lrtstarts, collapse = " "), ";\n") },
                    "            PROCESSORS ARE ", processors, ";\n\n")

  # Output
  mod.out <- paste0("OUTPUT:     ", paste0(output, collapse = " "), ";")

  # Output for k = 1 profile
  mod.out.1P <- misty::chr.gsub(c("TECH11", "TECH14"), c("", ""), mod.out)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model A ####

  # Paste syntax
  mod.A <- list()
  for (i in seq_len(classes)) {

    mod.A[[i]] <- paste0(# Title
      "TITLE:      Latent Profile Analysis (LPA)\n",
      "            A: Profile-Invariant Theta, Diagonal-Sigma\n\n",
      # Data, Variable, and Analysis
      sub("_classes_", i, mod.dva),
      # Output
      if (isTRUE(i != 1L)) { mod.out } else { mod.out.1P })

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model B ####

  mod.B <- list()
  for (i in seq_len(classes)) {

    # Model specifcation
    model <- paste0("            %c#_classes_%\n",
                    "            ", paste0(unlist(lapply(ind.x, function(y) paste0(y, collapse = " "))), collapse = "\n            "), ";\n\n")

    # Paste syntax
    mod.B[[i]] <- paste0(# Title
      "TITLE:      Latent Profile Analysis (LPA)\n",
      "            B: Profile-Varying Theta, Diagonal-Sigma\n\n",
      # Data, Variable, and Analysis
      sub("_classes_", i, mod.dva),
      # Model
      "MODEL:            \n",
      paste0(sapply(seq_len(i), function(y) sub("_classes_", y, model)), collapse = ""),
      # Output
      if (isTRUE(i != 1L)) { mod.out } else { mod.out.1P })

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model C ####

  mod.C <- list()
  for (i in seq_len(classes)) {

    # Paste syntax
    mod.C[[i]] <- paste0(# Title
      "TITLE:      Latent Profile Analysis (LPA)\n",
      "            C: Profile-Invariant Theta, Profile-Invariant Unrestrited Sigma\n\n",
      # Data, Variable, and Analysis
      sub("_classes_", i, mod.dva),
      # Model
      "MODEL:            \n",
      "            %OVERALL%\n",
      ind.cov, "\n\n",
      # Output
      if (isTRUE(i != 1L)) { mod.out } else { mod.out.1P })

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model D ####

  mod.D <- list()
  for (i in seq_len(classes)) {

    # Model specifcation
    model <- paste0("            %c#_classes_%\n", ind.cov, "\n\n")

    # Paste syntax
    mod.D[[i]] <- paste0(# Title
      "TITLE:      Latent Profile Analysis (LPA)\n",
      "            D: Profile-Invariant Theta, Profile-Varying Unrestructed Sigma\n\n",
      # Data, Variable, and Analysis
      sub("_classes_", i, mod.dva),
      # Model
      "MODEL:            \n",
      paste0(sapply(seq_len(i), function(y) sub("_classes_", y, model)), collapse = ""),
      # Output
      if (isTRUE(i != 1L)) { mod.out } else { mod.out.1P })

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model E ####

  mod.E <- list()
  for (i in seq_len(classes)) {

    # Model specifcation
    model <- paste0("            %c#_classes_%\n",
                    "            ", paste0(unlist(lapply(ind.x, function(y) paste0(y, collapse = " "))), collapse = "\n            "), ";\n\n")

    # Paste syntax
    mod.E[[i]] <- paste0(# Title
      "TITLE:      Latent Profile Analysis (LPA)\n",
      "            E: Profile-Carying Theta, Profile-Invariant Unrestrited Sigma\n\n",
      # Data, Variable, and Analysis
      sub("_classes_", i, mod.dva),
      # Model
      "MODEL:            \n",
      "            %OVERALL%\n",
      ind.cov, "\n\n",
      paste0(sapply(seq_len(i), function(y) sub("_classes_", y, model)), collapse = ""),
      # Output
      if (isTRUE(i != 1L)) { mod.out } else { mod.out.1P })

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Model F ####

  mod.F <- list()
  for (i in seq_len(classes)) {

    # Model specifcation
    model <- paste0("            %c#_classes_%\n",
                    "            ", paste0(unlist(lapply(ind.x, function(y) paste0(y, collapse = " "))), collapse = "\n            "), ";\n\n",
                    ind.cov, "\n\n")

    # Paste syntax
    mod.F[[i]] <- paste0(# Title
      "TITLE:      Latent Profile Analysis (LPA)\n",
      "            F: Profile-Varying Theta, Profile-Varying Unrestricted Sigma\n\n",
      # Data, Variable, and Analysis
      sub("_classes_", i, mod.dva),
      # Model
      "MODEL:            \n",
      paste0(sapply(seq_len(i), function(y) sub("_classes_", y, model)), collapse = ""),
      # Output
      if (isTRUE(i != 1L)) { mod.out } else { mod.out.1P })

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## write Mplus syntax ####

  if (isTRUE("input" %in% write)) {

    # Model A
    suppressWarnings(invisible(sapply(seq_len(classes), function(y) writeLines(mod.A[[y]], paste0(folder[1L], "/A_LPA-", y, ".inp")))))

    # Model B
    suppressWarnings(invisible(sapply(seq_len(classes), function(y) writeLines(mod.B[[y]], paste0(folder[2L], "/B_LPA-", y, ".inp")))))

    # Model C
    suppressWarnings(invisible(sapply(seq_len(classes), function(y) writeLines(mod.C[[y]], paste0(folder[3L], "/C_LPA-", y, ".inp")))))

    # Model D
    suppressWarnings(invisible(sapply(seq_len(classes), function(y) writeLines(mod.D[[y]], paste0(folder[4L], "/D_LPA-", y, ".inp")))))

    # Model E
    suppressWarnings(invisible(sapply(seq_len(classes), function(y) writeLines(mod.E[[y]], paste0(folder[5L], "/E_LPA-", y, ".inp")))))

    # Model F
    suppressWarnings(invisible(sapply(seq_len(classes), function(y) writeLines(mod.F[[y]], paste0(folder[6L], "/F_LPA-", y, ".inp")))))

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Run Mplus ####

  if (isTRUE(run.mplus)) { misty::run.mplus(recursive = TRUE, Mplus = Mplus, replaceOutfile = replace) }

  #_____________________________________________________________________________
  #
  # Return object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "mplus.lpa",
                 x = x,
                 args = list(ind = ind, cluster = cluster, folder = folder, file = file, write = write,
                             missing = missing, classes = classes, estimator = estimator,
                             starts = starts, stiterations = stiterations, lrtbootstra= lrtbootstrap,
                             lrtstarts = lrtstarts, processors = processors, output = output,
                             run.mplus = run.mplus, Mplus = Mplus, replace = replace,
                             check = check),
                 result = list(A = mod.A, B = mod.B, C = mod.C, D = mod.D, E = mod.E, F = mod.F))

  class(object) <- "misty.object"

  return(invisible(object))

}
