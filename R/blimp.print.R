#' Print Blimp Output
#'
#' This function prints the result sections of a Blimp output file (\code{.blimp-out})
#' on the R console. By default, the function prints selected result sections,
#' i.e., \code{Algorithmic Options Specified}, \code{Data Information},
#' \code{Model Information}, \code{Warning Messages}, \code{Outcome Model Estimates},
#' and \code{Generated Parameters}.
#'
#' @param x          a character string indicating the name of the Blimp output
#'                   file with or without the file extension \code{.blimp-out},
#'                   e.g., \code{"Blimp_Output.blimp-out"} or \code{"Blimp_Output"}.
#'                   Alternatively, a \code{misty.object} of type \code{blimp}
#'                   can be specified, i.e., result object of the \code{blimp.print()}
#'                   function.
#' @param result     a character vector specifying Blimp result sections included
#'                   in the output (see 'Details').
#' @param exclude    a character vector specifying Blimp input command or result
#'                   sections excluded from the output (see 'Details').
#' @param color      a character vector with two elements indicating the colors
#'                   used for the main headers (e.g., \code{"ALGORITHMIC OPTIONS SPECIFIED:"}),
#'                   and for the headers \code{Outcome Variable:} and
#'                   \code{Missing predictor:}, \code{Complete variable:},
#'                   \code{Latent Variable:}, and \code{Covariance Matrix:}
#'                   including variables names.
#' @param style      a character vector with two elements indicating the style
#'                   used for headers (e.g., \code{"ALGORITHMIC OPTIONS SPECIFIED:"}),
#'                   and for the main headers (e.g., \code{"ALGORITHMIC OPTIONS SPECIFIED:"}),
#'                   and for the headers \code{Outcome Variable:} and
#'                   \code{Missing predictor:}, \code{Complete variable:},
#'                   \code{Latent Variable:}, and \code{Covariance Matrix:}
#' @param not.result logical: if \code{TRUE} (default), character vector indicating
#'                   the result sections not requested are shown on the console.
#' @param write      a character string naming a file for writing the output into
#'                   a text file with file extension \code{".txt"} (e.g.,
#'                   \code{"Output.txt"}).
#' @param append     logical: if \code{TRUE} (default), output will be appended
#'                   to an existing text file with extension \code{.txt} specified
#'                   in \code{write}, if \code{FALSE} existing text file will be
#'                   overwritten.
#' @param check      logical: if \code{TRUE} (default), argument specification
#'                   is checked.
#' @param output     logical: if \code{TRUE} (default), output is shown on the
#'                   console.
#'
#' @details
#' \describe{
#' \item{\strong{Result Sections}}{Following result sections can be selected by
#' using the \code{result} argument or excluded by using the \code{exclude}
#' argument:
#'    \itemize{
#'       \item{\code{"algo.options"}} for the \code{ALGORITHMIC OPTIONS SPECIFIED} section
#'       \item{\code{"simdat.summary"}} for the \code{SIMULATED DATA SUMMARIES} section
#'       \item{\code{"order.simdat"}} for the \code{VARIABLE ORDER IN SIMULATED DATA} section
#'       \item{\code{"burnin.psr"}} for the \code{BURN-IN POTENTIAL SCALE REDUCTION (PSR) OUTPUT} section
#'       \item{\code{"mh.accept"}} for the \code{METROPOLIS-HASTINGS ACCEPTANCE RATES} section
#'       \item{\code{"data.info"}} for the \code{DATA INFORMATION} section
#'       \item{\code{"var.imp"}} for the \code{VARIABLES IN IMPUTATION MODEL} section
#'       \item{\code{"model.info"}} for the \code{MODEL INFORMATION} section
#'       \item{\code{"param.label"}} for the \code{PARAMETER LABELS} section
#'       \item{\code{"warn.mess"}} for the \code{WARNING MESSAGES} section
#'       \item{\code{"fit"}} for the \code{MODEL FIT} section
#'       \item{\code{"cor.resid"}} for the \code{CORRELATIONS AMONG RESIDUALS} section
#'       \item{\code{"out.model"}} for the \code{OUTCOME MODEL ESTIMATES} section
#'       \item{\code{"pred.model"}} for the \code{PREDICTOR MODEL ESTIMATES} section
#'       \item{\code{"gen.param"}} for the \code{GENERATED PARAMETERS} section
#'       \item{\code{"order.impdat"}} for the \code{VARIABLE ORDER IN IMPUTED DATA} section
#'    }
#' Note that all result sections are requested by specifying \code{result = "all"}.
#' The \code{result} argument is also used to select one (e.g., \code{result = "algo.options"})
#' or more than one result sections (e.g., \code{result = c("algo.options", "fit")}),
#' or to request result sections in addition to the default setting (e.g.,
#' \code{result = c("default", "fit")}). The \code{exclude} argument is used
#' to exclude result sections from the output (e.g., \code{exclude = "algo.options"}).
#' }
#' }
#'
#' @author
#' Takuya Yanagida
#'
#' @seealso
#' \code{\link{blimp}}, \code{\link{blimp.update}}, \code{\link{blimp.run}},
#' \code{\link{blimp.plot}}, \code{\link{blimp.bayes}}
#'
#' @references
#' Keller, B. T., & Enders, C. K. (2023). \emph{Blimp user’s guide} (Version 3).
#' Retrieved from www.appliedmissingdata.com/blimp
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#'
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{x}}{character string or misty object}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{print}}{print objects}
#' \item{\code{notprint}}{character vectors indicating the result sections not requested}
#' \item{\code{result}}{list with Blimp version (\code{blimp}) and result sections (\code{result})}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #----------------------------------------------------------------------------
#' # Blimp Example 4.3: Linear Regression
#'
#' # Example 1a: Default setting
#' blimp.print("Ex4.3.blimp-out")
#'
#' # Example 1c: Print OUTCOME MODEL ESTIMATES only
#' blimp.print("Ex4.3.blimp-out", result = "out.model")
#'
#' # Example 1d: Print MODEL FIT in addition to the default setting
#' blimp.print("Ex4.3.blimp-out", result = c("default", "fit"))
#'
#' # Example 1e: Exclude DATA INFORMATION section
#' blimp.print("Ex4.3.blimp-out", exclude = "data.info")
#'
#' # Example 1f: Print all result sections, but exclude MODEL FIT section
#' blimp.print("Ex4.3.blimp-out", result = "all", exclude = "fit")
#'
#' # Example 1g: Print result section in a different order
#' blimp.print("Ex4.3.blimp-out", result = c("model.info", "fit", "algo.options"))
#'
#' #----------------------------------------------------------------------------
#' # misty.object of type 'blimp.print'
#'
#' # Example 2
#' # Create misty.object
#' object <- blimp.print("Ex4.3.blimp-out", output = FALSE)
#'
#' # Print misty.object
#' blimp.print(object)
#'
#' #----------------------------------------------------------------------------
#' # Write Results
#'
#' # Example 3: Write Results into a text file
#' blimp.print("Ex4.3.blimp-out", write = "Output_4-3.txt")
#' }
blimp.print <- function(x,
                        result = c("all", "default", "algo.options", "data.info",
                                   "model.info", "warn.mess", "error.mess", "out.model", "gen.param"),
                        exclude = NULL, color = c("none", "blue", "green"),
                        style = c("bold", "regular"), not.result = TRUE,
                        write = NULL, append = TRUE, check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'x' is missing or NULL
  if (isTRUE(missing(x) || is.null(x))) { stop("Please specify a character string indicating the name of folder or name of the posterior data file for the argument 'x'", call. = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Character string ####

  if (isTRUE(is.character(x))) {

    # Character string
    if (isTRUE(length(x) != 1L)) { stop("Please specify a character string indicating the name of a Blimp output file for the argument 'x'", call. = FALSE) }

    # File extension .blimp-out
    x <- ifelse(isTRUE(!grepl(".blimp-out", x)), file <- paste0(x, ".blimp-out"), x)

    # Check if 'x' exists
    if (isTRUE(!file.exists(x))) { stop(paste0("Unable to read the Blimp output file: ", sQuote(x), " does not exist."), call. = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## misty object ####

  } else if (isTRUE(inherits(x, "misty.object"))) {

    if (isTRUE(x$type != "blimp")) { stop("Please specify a \"blimp\" object for the argument 'x'.", call. = FALSE) }

  } else {

    stop("Please specify a \"blimp\" object or a character string indicating the name of a Blimp output file for the argument 'x'", call. = FALSE)

  }

  #_____________________________________________________________________________
  #
  # Result Arguments -----------------------------------------------------------

  # All result options
  result.all <- c("algo.options", "simdat.summary", "simdat.summary", "order.simdat", "burnin.psr", "mh.accept", "data.info", "var.imp", "model.info", "param.label", "warn.mess", "error.mess", "fit", "cor.resid", "out.model", "pred.model", "gen.param", "order.impdat")

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs 'not.result', 'append', 'output', and 'write'
  .check.input(logical = c("not.result", "append", "output"), character = list(style = 2L), m.character = list(style = c("regular", "bold", "italic", "underline")), args = "write1", envir = environment(), input.check = check)

  # Additional checks
  if (isTRUE(check)) {

    # Check input 'result'
    result[which(!result %in% c("all", "default", result.all))] |> (\(z) if (isTRUE(length(z) != 0L)) { stop(paste0(if (isTRUE(length(z) == 1L)) { "Character string " } else { "Character vector " }, "specified in the argument 'result' is not permissible: ", paste(dQuote(z), collapse = ", ")), call. = FALSE) })()

    # Check input 'exclude'
    exclude[which(!exclude %in% result.all)] |> (\(z) if (isTRUE(length(z) != 0L)) { stop(paste0(if (isTRUE(length(z) == 1L)) { "Character string " } else { "Character vector " }, "specified in the argument 'exclude' is not permissible: ", paste(dQuote(z), collapse = ", ")), call. = FALSE) })()

    # Check input 'color'
    if (isTRUE(!all(color %in% c("none", "black", "red", "green", "yellow", "blue", "violet", "cyan", "white", "gray", "b.red", "b.green", "b.yellow", "b.blue", "b.violet", "b.cyan", "b.white")))) { stop(paste0(if (isTRUE(length(color) == 1L)) { "Character string " } else { "Character vector " }, "specified in the argument 'color' is not permissible."), call. = FALSE) }

    if (isTRUE(!all(c("none", "blue", "green") %in% color))) { if (isTRUE(length(color) != 2L)) { stop("Please specify a vector with two elements for the argument 'color'.", call. = FALSE) } }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'result' Argument ####

  # Default setting
  if (isTRUE(all(c("all", "default", "algo.options", "data.info", "model.info", "warn.mess", "error.mess", "out.model", "gen.param") %in% result))) {

    result <- result[!result %in% c("all", "default")]

  # All result sections
  } else if (isTRUE("all" %in% result)) {

    result <- result.all

  # Default setting with additional result sections
  } else if (isTRUE("default" %in% result & length(result > 1L))) {

    result <- result.all[result.all %in% misty::chr.omit(union(c("all", "default", "algo.options", "data.info", "model.info", "warn.mess", "error.mess", "out.model", "gen.param"), result), "default", check = FALSE)]

  # Manual default setting
  } else if (isTRUE("default" %in% result & length(result == 1L))) {

    result <- c("all", "default", "algo.options", "data.info", "model.info", "warn.mess", "error.mess", "out.model", "gen.param")

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'exclude' Argument ####

  if (isTRUE(!is.null(exclude))) { result <- setdiff(result, exclude) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'color' Argument ####

  # Default setting
  if (isTRUE(all(c("none", "blue", "green") %in% color))) { color <- c("blue", "green") }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  section <- c("ALGORITHMIC OPTIONS SPECIFIED:", "SIMULATED DATA SUMMARIES:", "VARIABLE ORDER IN SIMULATED DATA:",
               "BURN-IN POTENTIAL SCALE REDUCTION (PSR) OUTPUT:", "METROPOLIS-HASTINGS ACCEPTANCE RATES:",
               "DATA INFORMATION:", "VARIABLES IN IMPUTATION MODEL:", "MODEL INFORMATION:", "PARAMETER LABELS:",
               "WARNING MESSAGES:", "ERROR:", "MODEL FIT:", "CORRELATIONS AMONG RESIDUALS:", "OUTCOME MODEL ESTIMATES:",
               "PREDICTOR MODEL ESTIMATES:", "GENERATED PARAMETERS:", "VARIABLE ORDER IN IMPUTED DATA:")

  #----------------------------------------
  # Blimp Output in Text File
  if (isTRUE(!inherits(x, "misty.object"))) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Read Output ####

    out <- suppressWarnings(readLines(x))

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Check if file is a Blimp output ####

    if (isTRUE(all(!misty::chr.grepl(c("Blimp", "Craig K. Enders"), out)))) { stop("Output file specified in the argument 'x' is not a Blimp output file.", call. = FALSE) }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Exclude Output ####

    #...................
    ### Blimp Information ####

    # Blimp version
    blimp <- misty::chr.trim(out[grep("Blimp", out)[1L] + 1L])

    out <- out[-c(eval(parse(text = paste(which(out == "---------------------------------------------------------------------------")[1L:2L], collapse = ":"))))]

    #...................
    ### Note ####

    out <- c("NOTE: The default prior for regression coefficients", "in categorical models is 'normal( 0.0, 5.0)'", "been assigned a reference group.", "To change reference group use the following command:", "NOMINAL: variable_name(reference_value);", "NOTE: Split chain PSR is being used. This splits each chain's", "iterations to create twice as many chains.", "NOTE: Suppressing printing of 1 chains.", "Use keyword 'tuneinfo' in options to override.", "Summaries based on ", "NOTE: This model is used to estimate the grand mean and latent group means needed ", "to center outcome variables that appear as predictors in other models.", "NOTE: Intercepts are computed by setting all predictors", "not involved in the conditional effect to zero.") |>
      (\(y) if (isTRUE(any(misty::chr.grepl(y, out)))) {

        unlist(out)[-unlist(sapply(y, function(z) grep(z, unlist(out), fixed = TRUE)))]

      } else {

        return(out)

      })()

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Modify Output ####

    # Horizontal line below Outcome and Predictor Model
    if (isTRUE(any(grepl("N_Eff", out)))) {

      n <- sum(unlist(strsplit(unique(out[grep("N_Eff", out) + 1L]), "")) == "-")

      # Remove horizontal line below Outcome and Predictor Model
      out <- grep(paste(rep("-", times = n), collapse = ""), out) |>
        (\(z) z[seq(2L, length(z), by = 2L)])() |>
        (\(w) out[-w])()

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Run Length Encoding, Sections, Warnings, and Internal Function ####

    #...................
    ### Run Length Encoding ####

    # Values of runs of equal values
    run.val <- rle(out)$value

    #...................
    ### Internal Function for Determining Indices ####

    .internal.ind.to <- function(x, cur.section, run = run.val) {

      parse.text <- parse(text = paste0("min(unlist(sapply(x, function(z) if (isTRUE(any(run == z))) {
                            sapply(which(run == z), function(q) if (isTRUE(q > max(which(", paste0(sapply(cur.section, function(w) paste0("run == ", paste0("\"", w, "\""))), collapse = " | "), ")))) { q } else { length(run) + 1L })
                          })))"))

      return(eval(parse.text) - 1L)

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Extract Output Sections ####

    # Input objects
    algo.options <- simdat.summary <- order.simdat <- burnin.psr <- mh.accept <- data.info <- var.imp <- model.info <- param.label <- warn.mess <- error.mess <- fit <- cor.resid <- out.model <- pred.model <- gen.param <- order.impdat <- NULL

    #...................
    ### Multiple Imputation Within Subgroups ####

    if (isTRUE(sum(run.val == "BURN-IN POTENTIAL SCALE REDUCTION (PSR) OUTPUT:") > 1L)) { stop("The Blimp print function does not support outputs from multiple imputation within subgroups.", call. = FALSE) }

    #...................
    ### ALGORITHMIC OPTIONS SPECIFIED ####

    if (isTRUE(any(run.val == "ALGORITHMIC OPTIONS SPECIFIED:"))) { algo.options <- run.val[which(run.val == "ALGORITHMIC OPTIONS SPECIFIED:"):(.internal.ind.to(section, "ALGORITHMIC OPTIONS SPECIFIED:"))] }

    #...................
    ### SIMULATED DATA SUMMARIES ####

    if (isTRUE(any(run.val == "SIMULATED DATA SUMMARIES:"))) { simdat.summary <- run.val[which(run.val == "SIMULATED DATA SUMMARIES:"):(.internal.ind.to(section, "SIMULATED DATA SUMMARIES:"))] }

    #...................
    ### VARIABLE ORDER IN SIMULATED DATA ####

    if (isTRUE(any(run.val == "VARIABLE ORDER IN SIMULATED DATA:"))) { order.simdat <- run.val[which(run.val == "VARIABLE ORDER IN SIMULATED DATA:"):(.internal.ind.to(section, "VARIABLE ORDER IN SIMULATED DATA:"))] }

    #...................
    ### BURN-IN POTENTIAL SCALE REDUCTION (PSR) OUTPUT ####

    if (isTRUE(any(run.val == "BURN-IN POTENTIAL SCALE REDUCTION (PSR) OUTPUT:"))) { burnin.psr <- run.val[which(run.val == "BURN-IN POTENTIAL SCALE REDUCTION (PSR) OUTPUT:"):(.internal.ind.to(section, "BURN-IN POTENTIAL SCALE REDUCTION (PSR) OUTPUT:"))] }

    #...................
    ### METROPOLIS-HASTINGS ACCEPTANCE RATES ####

    if (isTRUE(any(run.val == "METROPOLIS-HASTINGS ACCEPTANCE RATES:"))) { mh.accept <- run.val[which(run.val == "METROPOLIS-HASTINGS ACCEPTANCE RATES:"):(.internal.ind.to(section, "METROPOLIS-HASTINGS ACCEPTANCE RATES:"))] }

    #...................
    ### DATA INFORMATION ####

    if (isTRUE(any(run.val == "DATA INFORMATION:"))) { data.info <- run.val[which(run.val == "DATA INFORMATION:"):(.internal.ind.to(section, "DATA INFORMATION:"))] }

    #...................
    ### VARIABLES IN IMPUTATION MODEL ####

    if (isTRUE(any(run.val == "VARIABLES IN IMPUTATION MODEL:"))) { var.imp <- run.val[which(run.val == "VARIABLES IN IMPUTATION MODEL:"):(.internal.ind.to(section, "VARIABLES IN IMPUTATION MODEL:"))] }

    #...................
    ### MODEL INFORMATION ####

    if (isTRUE(any(run.val == "MODEL INFORMATION:"))) { model.info <- run.val[which(run.val == "MODEL INFORMATION:"):(.internal.ind.to(section, "MODEL INFORMATION:"))] }

    #...................
    ### PARAMETER LABELS ####

    if (isTRUE(any(run.val == "PARAMETER LABELS:"))) { param.label <- run.val[which(run.val == "PARAMETER LABELS:"):(.internal.ind.to(section, "PARAMETER LABELS:"))] }

    #...................
    ### WARNING MESSAGES ####

    if (isTRUE(any(run.val == "WARNING MESSAGES:"))) {

      # Extract section
      warn.mess <- run.val[which(run.val == "WARNING MESSAGES:"):(.internal.ind.to(section, "WARNING MESSAGES:"))]

      # Highest PSR exceeding 1.05
      if (isTRUE(any(run.val == "BURN-IN POTENTIAL SCALE REDUCTION (PSR) OUTPUT:"))) {

        if (isTRUE(suppressWarnings(min(na.omit(as.numeric(unlist(strsplit(burnin.psr, " ")))))) > 1.05)) {

          if (isTRUE(any(grepl("No warning messages", warn.mess)))) {

            warn.mess <- sub("  No warning messages.", "  The highest PSR exceeds 1.05, please increase the number of burn-in iterations and rerun the model.", warn.mess)

          } else {

            warn.mess <- c(warn.mess, "The highest PSR exceeds 1.05, please increase the number of burn-in iterations and rerun the model.")

          }

        }

      }

    }

    #...................
    ### ERROR MESSAGE ####

    if (isTRUE(any(run.val == "ERROR:"))) { error.mess <- run.val[which(run.val == "ERROR:"):(.internal.ind.to(section, "ERROR:"))] }

    #...................
    ### MODEL FIT ####

    if (isTRUE(any(run.val == "MODEL FIT:"))) { fit <- run.val[which(run.val == "MODEL FIT:"):(.internal.ind.to(section, "MODEL FIT:"))] }

    #...................
    ### CORRELATIONS AMONG RESIDUALS ####

    if (isTRUE(any(run.val == "CORRELATIONS AMONG RESIDUALS:"))) { cor.resid <- run.val[which(run.val == "CORRELATIONS AMONG RESIDUALS:"):(.internal.ind.to(section, "CORRELATIONS AMONG RESIDUALS:"))] }

    #...................
    ### OUTCOME MODEL ESTIMATES ####

    if (isTRUE(any(run.val == "OUTCOME MODEL ESTIMATES:"))) {

      # Extract section
      out.model <- run.val[which(run.val == "OUTCOME MODEL ESTIMATES:"):(.internal.ind.to(section, "OUTCOME MODEL ESTIMATES:"))]

      # Paste space
      out.model[-c(which(out.model == "OUTCOME MODEL ESTIMATES:"), grep("block:", out.model))] <- sapply(out.model[-c(which(out.model == "OUTCOME MODEL ESTIMATES:"), grep("block:", out.model))], function(y) paste0("  ", y))
      out.model[-c(which(out.model == "OUTCOME MODEL ESTIMATES:"), misty::chr.grep(c("Outcome Variable:", "Covariance Matrix:", "block"), out.model))] <- sapply(out.model[-c(which(out.model == "OUTCOME MODEL ESTIMATES:"), misty::chr.grep(c("Outcome Variable:", "Covariance Matrix:", "block"), out.model))], function(y) paste0(" ", y))

    }

    #...................
    ### PREDICTOR MODEL ESTIMATES ####

    if (isTRUE(any(run.val == "PREDICTOR MODEL ESTIMATES:"))) {

      # Extract section
      pred.model <- run.val[which(run.val == "PREDICTOR MODEL ESTIMATES:"):(.internal.ind.to(section, "PREDICTOR MODEL ESTIMATES:"))]

      # Paste space
      pred.model[-which(pred.model == "PREDICTOR MODEL ESTIMATES:")] <- sapply(pred.model[-which(pred.model == "PREDICTOR MODEL ESTIMATES:")], function(y) paste0("  ", y))
      pred.model[-c(which(pred.model == "PREDICTOR MODEL ESTIMATES:"), misty::chr.grep(c("predictor:", "Model:"), pred.model))] <- sapply(pred.model[-c(which(pred.model == "PREDICTOR MODEL ESTIMATES:"), misty::chr.grep(c("predictor:", "Model:"), pred.model))], function(y) paste0(" ", y))

    }

    #...................
    ### GENERATED PARAMETERS ####

    if (isTRUE(any(run.val == "GENERATED PARAMETERS:"))) {

      # Extract section
      gen.param <- run.val[which(run.val == "GENERATED PARAMETERS:"):(.internal.ind.to(section, "GENERATED PARAMETERS:"))]

      # Paste space
      gen.param[-which(gen.param == "GENERATED PARAMETERS:")] <- sapply(gen.param[-which(gen.param == "GENERATED PARAMETERS:")], function(y) paste0("  ", y))

    }

    #...................
    ### VARIABLE ORDER IN IMPUTED DATA ####

    if (isTRUE(any(run.val == "VARIABLE ORDER IN IMPUTED DATA:"))) { order.impdat <- run.val[which(run.val == "VARIABLE ORDER IN IMPUTED DATA:"):(.internal.ind.to(section, "VARIABLE ORDER IN IMPUTED DATA:"))] }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Remove Last Space ####

    for (i in c("algo.options", "simdat.summary", "order.simdat", "burnin.psr", "mh.accept", "data.info", "var.imp", "model.info", "param.label", "warn.mess", "error.mess", "fit", "cor.resid", "out.model", "pred.model", "gen.param", "order.impdat")) {

      if (isTRUE(!is.null(eval(parse(text = i))))) {

        repeat {

          eval(parse(text = paste0("if (isTRUE(misty::chr.trim(", i, "[length(", i, ")]) == \"\")) { ", i, " <- ", i,  "[-length(", i, ")] }")))

          eval(parse(text = paste0("if (isTRUE(misty::chr.trim(", i, "[length(", i, ")]) != \"\")) break")))

        }

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Return Object ####

    return.object <- list(# Blimp version
                          blimp = blimp,
                          # Output sections
                          result = list(algo.options = algo.options, simdat.summary = simdat.summary, order.simdat = order.simdat,
                                        burnin.psr = burnin.psr, mh.accept = mh.accept, data.info = data.info, var.imp = var.imp,
                                        model.info = model.info, param.label = param.label, warn.mess = warn.mess, error.mess = error.mess, fit = fit,
                                        cor.resid = cor.resid, out.model = out.model, pred.model = pred.model, gen.param = gen.param, order.impdat = order.impdat))

  #----------------------------------------
  # Blimp Output in misty object
  } else {

    return.object <- x$result

  }

  #_____________________________________________________________________________
  #
  # Print Result Section -------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Extract result sections ####

  result.object <- Filter(Negate(is.null), return.object$result[result])
  if (isTRUE(length(result.object) != 0L)) {

    # Format result sections
    result.object <- lapply(result.object, function(y) c(y, ""))

    # Remove last ""
    result.object[[length(result.object)]] <- result.object[[length(result.object)]][-length(result.object[[length(result.object)]])]

    # Paste result sections
    result.object <- unname(unlist(sapply(result.object, function(y) paste(y, "\n"))))

  } else {

    result.object <- NULL

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Color Output Text ####

  if (isTRUE(any(color != "none") && is.null(getOption("knitr.in.progress")))) {

    # Main Headers
    print.object <- misty::chr.gsub(section[-grep("ERROR:", section)], misty::chr.color(section[-grep("ERROR:", section)], color = color[1L], style = style[1L], check = FALSE), result.object)

    # Sub-Header
    if (isTRUE(any(misty::chr.grepl(c("Outco me Variable:", "Missing predictor:", "Complete variable:", "Latent Variable:", "Covariance Matrix:"), print.object)))) {

      print.object <- misty::chr.grep(c("Outcome Variable:", "Missing predictor:", "Complete variable:", "Latent Variable:", "Covariance Matrix:"), print.object) |>
        (\(z) misty::chr.gsub(print.object[z], chr.color(print.object[z], color = color[2L], style = style[2L]), print.object))()

    }

    # Error Message
    if (isTRUE(!is.null(return.object$result$error.mess))) {

      print.object <- misty::chr.grep("ERROR:", print.object) |> (\(z) misty::chr.gsub(print.object[z], chr.color(print.object[z], color = "red", style = style[1L]), print.object))()

    }

  } else {

    print.object <- result.object

  }

  # Horizontal line
  if (isTRUE(any(grepl("N_Eff", print.object)) && is.null(getOption("knitr.in.progress")))) {

    n <- sum(unlist(strsplit(unique(print.object[grep("N_Eff", print.object) + 1L]), "")) == "-")

    print.object <- gsub(paste0("                                ", paste(rep("-", times = n), collapse = "")),
                         paste0("                                   ", paste(rep("\u23AF", times = n + 10L), collapse = "")), print.object)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Result Sections Not Requested ####

  result.not <- setdiff(names(Filter(Negate(is.null), return.object$result)), result)

  result.not <- if (isTRUE(length(result.not) != 0L)) { paste0("\"", result.not, "\"") } else { NULL }

  if (isTRUE(!is.null(result.not))) {

    # Less than or equal 5 additional result sections
    if (isTRUE(length(result.not) <= 5L)) {

      result.not.print <- matrix(result.not, nrow = 1L)

    # Divisible by 5
    } else if (isTRUE(length(result.not) %% 5L == 0L)) {

      result.not.print <- apply(matrix(result.not, ncol = 5L, byrow = TRUE), 2L, function(y) format(y))

    # Not divisible by 5
    } else {

      result.not.print <- apply(matrix(c(result.not, rep("", times = (seq(10L, 65L, by = 5L)[seq(10L, 65L, by = 5L) > length(result.not)][1L] - length(result.not)))), ncol = 5L, byrow = TRUE), 2L, function(y) format(y))

    }

    result.not.print[, 1L] <- paste(" ", result.not.print[, 1L])

  } else {

    result.not.print <- NULL

  }

  #_____________________________________________________________________________
  #
  # Print Output ---------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Print Result Sections ####

  if (isTRUE(output)) {

    # Print result sections
    if (isTRUE(!is.null(print.object))) {

      if (isTRUE(any(grepl("ALGORITHMIC OPTIONS SPECIFIED:", print.object)))) { cat(rep(" ", times = 17L), "Blimp", return.object$blimp, "\n\n ") }

      cat(print.object)

    }

    # Print not requested result sections
    if (isTRUE(not.result && is.null(return.object$result$error.mess))) {

      if (isTRUE(!is.null(result.not.print))) {

        if (isTRUE(length(result.not.print) == 1L)) { cat("\n Not Requested Result Section:\n") } else { cat("\n Not Requested Result Sections:\n") }

        write.table(result.not.print, quote = FALSE, row.names = FALSE, col.names = FALSE, na = "")

      } else {

        cat("\n Not Requested Result Sections: None\n")

      }

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Write Results into a text file ####
  #
  # Note that write.table() function does not display results on the console even
  # when sink(write, split = TRUE)

  if (isTRUE(!is.null(write))) {

    # Send R Output to a file
    sink(file = write, append = ifelse(isTRUE(file.exists(write)), append, FALSE), type = "output")

    # Append output
    if (isTRUE(append && file.exists(write))) { write("", file = write, append = TRUE) }

    #...................
    ### Result Sections ####

    if (isTRUE(!is.null(result.object))) {

      result.object <- gsub("                                   ------------------------------------------------------------------- \n",
                            "                                     ------------------------------------------------------------------ \n", result.object)

      cat(rep(" ", times = 17L), "Blimp", return.object$blimp, "\n\n")

      cat(result.object)

    }

    # Print not requested result sections
    if (isTRUE(not.result)) {

      if (isTRUE(!is.null(result.not.print))) {

        if (isTRUE(length(result.not.print) == 1L)) { cat("\n Not Requested Result Section:\n") } else { cat("\n Not Requested Result Sections:\n") }

        write.table(result.not.print, quote = FALSE, row.names = FALSE, col.names = FALSE, na = "")

      } else {

        cat("\n Not Requested Result Sections: None\n")

      }

    }

    # Close file connection
    sink()

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "blimp",
                 x = x,
                 args = list(result = result, exclude = exclude, color = color, style = style, not.result = not.result, write = write, append = append, check = check, output = output),
                 print = result.object, notprint = result.not, result = return.object)

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  return(invisible(object))

}

#_______________________________________________________________________________
